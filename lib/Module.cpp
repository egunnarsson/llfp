
#include "Codegen.h"
#include "Log.h"

#include "Module.h"


namespace llfp
{

ImportedModule::~ImportedModule() {}


SourceModule::SourceModule(Compiler* parent_, std::string path_) :
    parent { parent_ },
    path { std::move(path_) }
{}

SourceModule::~SourceModule() {}

bool SourceModule::setAST(std::unique_ptr<ast::Module> astModule_)
{
    bool result = true;

    astModule = std::move(astModule_);

    for (auto &function : astModule->functions)
    {
        auto insert = functions.insert((std::make_pair(function->name, function.get())));
        if (!insert.second)
        {
            Log(function->location, "function already defined");
            result = false;
        }
    }

    for (auto &publicDecl : astModule->publics)
    {
        auto predicate = [&publicDecl](std::unique_ptr<ast::Function> &function) { return publicDecl.name == function->name; };
        auto it = std::find_if(astModule->functions.begin(), astModule->functions.end(), predicate);
        if (it != astModule->functions.end())
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

    for (auto& classDecl : astModule->classes)
    {
        for (auto& funDecl : classDecl->functions)
        {
            auto it = functionDeclarations.insert(std::make_pair(funDecl->name, std::make_tuple(classDecl.get(), funDecl.get())));
            if (!it.second)
            {
                Log(funDecl->location, "function declaration already defined");
                result = false;
            }
        }
    }

    for (auto &dataDecl : astModule->datas)
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

    return result;
}

/*void SourceModule::createCodeGenerator()
{
    assert(astModule != nullptr);
    codeGenerator = std::make_unique<codegen::CodeGenerator>(this);
}*/

const std::string& SourceModule::filePath() const
{
    return path;
}

const std::string& SourceModule::name() const
{
    return astModule->name;
}

FunAst SourceModule::getFunction(const std::string &name)
{
    auto ast = find(publicFunctions, name);
    return ast != nullptr ? FunAst{this, ast} : FunAst{};
}

FunDeclAst SourceModule::getFunctionDecl(const std::string& name)
{
    ast::Class* c;
    ast::FunctionDeclaration* f;
    std::tie(c, f) = find(functionDeclarations, name, { nullptr, nullptr });
    return f != nullptr ? FunDeclAst{ this, c, f } : FunDeclAst{};
}

DataAst SourceModule::getType(const std::string &name) const
{
    auto ast =  find(dataDeclarations, name);
    return ast != nullptr ? DataAst{ this, ast } : DataAst{};
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

std::string SourceModule::getMangledName(const ast::Data *data, const std::vector<type::TypePtr>& types) const
{
    return name() + '_' + data->name;
}

std::string SourceModule::getExportedName(const ast::Function*function) const
{
    return name() + '_' + function->name;
}

bool SourceModule::fullyQualifiedName(type::Identifier& identifier, const ast::TypeIdentifier& tid) const
{
    // check primitve type
    if (tid.parameters.empty() && tid.identifier.moduleName.empty())
    {
        type::Identifier id{ tid.identifier, {} };
        if (codeGenerator->getTypeContext()->isPrimitive(id)) // change this check, split type and typeContext and put check in type?
        {
            identifier = std::move(id);
            return true;
        }
    }

    auto ast = lookupType(tid.identifier);
    if (ast.importedModule == nullptr || ast.data == nullptr)
    {
        return false;
    }

    identifier.name = { ast.importedModule->name(), ast.data->name };
    assert(identifier.parameters.size() == 0);
    for (auto& param : tid.parameters)
    {
        identifier.parameters.push_back({});
        if (!fullyQualifiedName(identifier.parameters.back(), param))
        {
            return false;
        }
    }
    return true;
}

ast::Module* SourceModule::getAST()
{
    return astModule.get();
}

/*llvm::Module* SourceModule::getLLVM()
{
    return codeGenerator->getLLVM();
}*/

template<class AstNode, class LocalFun, class GlobalFun>
AstNode SourceModule::lookup(
    const GlobalIdentifier& identifier,
    LocalFun localLookup,
    GlobalFun globalLookup,
    llvm::StringLiteral errorMsg) const
{
    if (identifier.moduleName.empty())
    {
        std::vector<AstNode> results;

        auto localAst = localLookup(identifier.name);
        if (!localAst.empty())
        {
            results.push_back(localAst);
        }

        for (auto &itm : importedModules)
        {
            auto importedModule = itm.second;
            auto globalAst = globalLookup(importedModule, identifier.name);
            if (!globalAst.empty())
            {
                results.push_back(globalAst);
            }
        }

        if (results.size() == 1)
        {
            return results.back();
        }
        else if (!results.empty())
        {
            // for functionDecl not finding a result is ok, but this is not, how do we return this to the caller? start with exception?
            Log({}, "reference to ", identifier.str(), " is ambiguous");
            return AstNode{};
        }
        else
        {
            if (!errorMsg.empty()) { Log({}, errorMsg, identifier.str()); }
            return AstNode{};
        }
    }
    else
    {
        if (identifier.moduleName == name())
        {
            auto localAst = localLookup(identifier.name);
            if (!localAst.empty())
            {
                return localAst;
            }
        }
        else
        {
            auto itm = importedModules.find(identifier.moduleName);
            if (itm != importedModules.end())
            {
                auto importedModule = itm->second;
                auto globalAst = globalLookup(importedModule, identifier.name);

                if (!globalAst.empty())
                {
                    return globalAst;
                }
            }
            else
            {
                Log({}, "undefined module ", identifier.moduleName);
                return AstNode{};
            }
        }
    }

    if (!errorMsg.empty()) { Log({}, errorMsg, identifier.str()); }
    return AstNode{};
}

FunDeclAst SourceModule::lookupFunctionDecl(const GlobalIdentifier& identifier)
{
    return lookup<FunDeclAst>(identifier,
        [this](const std::string& id) { return getFunctionDecl(id); },
        [](ImportedModule* module, const std::string& id) { return module->getFunctionDecl(id); },
        "");
}

FunAst SourceModule::lookupFunction(const GlobalIdentifier& identifier)
{
    return lookup<FunAst>(identifier,
        [this](const std::string& id) { return getFunction(id); },
        [](ImportedModule* module, const std::string& id) { return module->getFunction(id); },
        "undefined function ");
}

DataAst SourceModule::lookupType(const GlobalIdentifier& identifier) const
{
    return lookup<DataAst>(identifier,
        [this](const std::string& id) { return getType(id); },
        [](ImportedModule* module, const std::string& id) { return module->getType(id); },
        "undefined data type ");
}

bool SourceModule::generateExportedFunctions()
{
    bool result = true;
    for (auto &fun : astModule->functions)
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
