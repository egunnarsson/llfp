
#include "Codegen.h"
#include "Log.h"

#include "Module.h"


namespace llfp
{

ImportedModule::~ImportedModule() {}


SourceModule::SourceModule(std::string path_) :
    path { std::move(path_) }
{}

SourceModule::~SourceModule() {}

bool SourceModule::setAST(std::unique_ptr<ast::Module> astModule_)
{
    bool result = true;

    astModule = std::move(astModule_);

    for (auto &function : astModule->functionDeclarations)
    {
        auto insert = functions.insert((std::make_pair(function->name, function.get())));
        if (!insert.second)
        {
            Log(function->location, "function already defined");
            result = false;
        }
    }

    for (auto &publicDecl : astModule->publicDeclarations)
    {
        auto predicate = [&publicDecl](std::unique_ptr<ast::Function> &function) { return publicDecl.name == function->name; };
        auto it = std::find_if(astModule->functionDeclarations.begin(), astModule->functionDeclarations.end(), predicate);
        if (it != astModule->functionDeclarations.end())
        {
            auto &function = *it;
            publicFunctions.insert(std::make_pair(function->name, function.get()));
        }
        else
        {
            Log(publicDecl.location, "function declared as public is not defined ", publicDecl.name);
            result = false;
        }
    }

    for (auto &dataDecl : astModule->dataDeclarations)
    {
        auto insert = dataDeclarations.insert(std::make_pair(dataDecl->name, dataDecl.get()));
        if (!insert.second)
        {
            Log(dataDecl->location, "data already defined");
            result = false;
        }

        // check duplicate fields;
        auto end = dataDecl->fields.end();
        for (auto it = dataDecl->fields.begin(); it != end; ++it)
        {
            auto predicate = [it](const llfp::ast::Field& field) { return it->name == field.name; };
            auto it1 = std::find_if(it + 1, end, predicate);
            if (it1 != end)
            {
                Log(it1->location, "duplicate field \"", it1->name, '"');
                result = false;
            }
        }
    }

    return result;
}

bool SourceModule::addImportedModules(const std::vector<ImportedModule*> &moduleList)
{
    assert(astModule != nullptr);
    bool result = true;

    for (auto &importDecl : astModule->imports)
    {
        auto predicate = [&importDecl](ImportedModule *module) { return module->name() == importDecl.name; };
        auto it = std::find_if(moduleList.begin(), moduleList.end(), predicate);
        if (it != moduleList.end())
        {
            auto module = *it;
            importedModules.insert(std::make_pair(module->name(), module));
        }
        else
        {
            Log(importDecl.location, "unresolved imported module ", importDecl.name);
            result = false;
        }
    }

    for (auto m : moduleList)
    {
        allModules.insert({m->name(), m});
    }

    return result;
}

void SourceModule::createCodeGenerator()
{
    assert(astModule != nullptr);
    codeGenerator = std::make_unique<codegen::CodeGenerator>(this);
}

const std::string& SourceModule::filePath() const
{
    return path;
}

const std::string& SourceModule::name() const
{
    return astModule->name;
}

const ast::Function* SourceModule::getFunction(const std::string &name) const
{
    return find(publicFunctions, name);
}

const ast::DataDeclaration* SourceModule::getType(const std::string &name) const
{
    return find(dataDeclarations, name);
}

// [%@][-a-zA-Z$._][-a-zA-Z$._0-9]*
std::string SourceModule::getMangledName(const ast::Function*function, const std::vector<type::TypePtr> &types) const
{
    assert(!types.empty());
    if (function->exported)
    {
        return getExportedName(function);
    }

    std::string result;
    result += name();
    result += ':';
    result += function->name;
    for (auto type : types)
    {
        if (!type->isConcreteType())
        {
            Log({}, "trying to mangle with abstract type");
            return "";
        }
        result += '$';
        result += type->identifier().str();
    }
    return result;
}

std::string SourceModule::getMangledName(const ast::DataDeclaration *data, const std::vector<type::TypePtr>& types) const
{
    return name() + '_' + data->name;
}

std::string SourceModule::getExportedName(const ast::Function*function) const
{
    return name() + '_' + function->name;
}

ast::Module* SourceModule::getAST()
{
    return astModule.get();
}

llvm::Module* SourceModule::getLLVM()
{
    return codeGenerator->getLLVM();
}

template<class AstNode, class LocalFun, class GlobalFun>
AstNode SourceModule::lookup(
    const GlobalIdentifier& identifier,
    LocalFun localLookup,
    GlobalFun globalLookup,
    llvm::StringLiteral errorMsg) const
{
    if (identifier.moduleName.empty())
    {
        std::tuple_element_t<1, AstNode> ast;
        std::vector<ImportedModule*> modules;

        auto localAst = localLookup(identifier.name);
        if (localAst != nullptr)
        {
            ast = localAst;
            modules.push_back(const_cast<SourceModule*>(this));
        }

        for (auto &itm : importedModules)
        {
            auto importedModule = itm.second;
            auto globalAst = globalLookup(importedModule, identifier.name);
            if (globalAst != nullptr)
            {
                ast = globalAst;
                modules.push_back(importedModule);
            }
        }

        if (modules.size() == 1)
        {
            return { modules.back(), ast };
        }
        else if (!modules.empty())
        {
            Log({}, "reference to ", identifier.str(), " is ambiguous");
            return { nullptr, nullptr };
        }
        else
        {
            Log({}, errorMsg, identifier.str());
            return { nullptr, nullptr };
        }
    }
    else
    {
        if (identifier.moduleName == name())
        {
            auto localAst = localLookup(identifier.name);
            if (localAst != nullptr)
            {
                return { const_cast<SourceModule*>(this), localAst };
            }
        }
        else
        {
            auto itm = importedModules.find(identifier.moduleName);
            if (itm != importedModules.end())
            {
                auto importedModule = itm->second;
                auto globalAst = globalLookup(importedModule, identifier.name);

                if (globalAst != nullptr)
                {
                    return { importedModule, globalAst };
                }
            }
            else
            {
                Log({}, "undefined module ", identifier.moduleName);
                return { nullptr, nullptr };
            }
        }
    }

    Log({}, errorMsg, identifier.str());
    return { nullptr, nullptr };
}

type::FunAst SourceModule::lookupFunction(const GlobalIdentifier& identifier)
{
    return lookup<type::FunAst>(identifier,
        [this](const std::string& id) { return llfp::find(functions, id); },
        [](ImportedModule* module, const std::string& id) { return module->getFunction(id); },
        "undefined function ");
}

type::DataAst SourceModule::lookupType(const GlobalIdentifier& identifier) const
{
    return lookup<type::DataAst>(identifier,
        [this](const std::string& id) { return llfp::find(dataDeclarations, id); },
        [](ImportedModule* module, const std::string& id) { return module->getType(id); },
        "undefined data type ");
}

type::DataAst SourceModule::lookupTypeGlobal(const GlobalIdentifier& identifier) const
{
    assert(!identifier.moduleName.empty());
    assert(!identifier.name.empty());

    auto it = allModules.find(identifier.moduleName);
    if (it != allModules.end())
    {
        auto astType = it->second->getType(identifier.name);
        if (astType != nullptr)
        {
            return { it->second, astType };
        }
    }

    return { nullptr, nullptr };
}

void SourceModule::requireFunctionInstance(FunctionIdentifier function)
{
    pendingGeneration.push_back(function);
}

bool SourceModule::generateExportedFunctions()
{
    bool result = true;
    for (auto &fun : astModule->functionDeclarations)
    {
        if (fun->exported)
        {
            result &= codeGenerator->generateFunction(fun.get());
        }
    }
    return result;
}

// return success or fail, or no more to generate?
bool SourceModule::generateNextFunction()
{
    if (!pendingGeneration.empty())
    {
        auto &function = pendingGeneration.back();
        auto it = functions.find(function.name.str());
        if (it == functions.end())
        {
            Log({}, "undefined function: ", name(), ':', function.name);
            pendingGeneration.pop_back();
            return false;
        }
        auto ast = it->second;

        std::vector<type::TypePtr> types;
        for (auto t : *function.types)
        {
            //TODO: Now we try to find type in this module with its imports
            // but this might be called from another module with its own type...
            // an import in this module should not be required
            types.push_back(codeGenerator->getTypeContext()->getType(t->identifier()));
        }

        if (std::any_of(types.begin(), types.end(), [](auto x) { return x == nullptr; }))
        {
            Log(ast->location, "unknown type in: ", name(), ':', function.name);
            pendingGeneration.pop_back();
            return false;
        }

        bool result = codeGenerator->generateFunction(ast, std::move(types));
        pendingGeneration.pop_back();
        return result;
    }
    return false;
}

} // llfp
