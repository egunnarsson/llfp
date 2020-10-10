
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
        auto predicate = [&publicDecl](std::unique_ptr<ast::FunctionDeclaration> &function) { return publicDecl.name == function->name; };
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

    return result;
}

bool SourceModule::addImportedModules(const std::vector<ImportedModule*> &moduleList)
{
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

const ast::FunctionDeclaration* SourceModule::getFunction(const std::string &name) const
{
    auto it = publicFunctions.find(name);
    if (it == publicFunctions.end())
    {
        return nullptr;
    }
    return it->second;
}

// [%@][-a-zA-Z$._][-a-zA-Z$._0-9]*
std::string SourceModule::getMangledName(const ast::FunctionDeclaration *function, const std::vector<type::Type*> &types) const
{
    assert(!types.empty());
    if (function->exported)
    {
        return getExportedName(function);
    }

    std::string result;
    result += name();
    result += '.';
    result += function->name;
    result += '$';
    for (auto type : types)
    {
        if (type->isLiteral())
        {
            Log({}, "trying to mangle with literal type");
            return "";
        }
        result += type->name();
        result += '.';
    }
    result.pop_back();
    return result;
}

std::string SourceModule::getExportedName(const ast::FunctionDeclaration *function) const
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

bool SourceModule::lookupFunction(llvm::StringRef moduleIdentifier, llvm::StringRef identifier, ImportedModule*& funModule, const ast::FunctionDeclaration*& ast)
{
    llvm::StringRef moduleName;

    if (moduleIdentifier.empty())
    {
        std::vector<ImportedModule*> modules;

        auto it = functions.find(identifier);
        if (it != functions.end()) // This find, it creates a string copy?
        {
            funModule = this;
            ast = it->second;
            modules.push_back(this);
        }

        for (auto &importedModule : importedModules)
        {
            auto function = importedModule.second->getFunction(identifier);
            if (function != nullptr)
            {
                funModule = importedModule.second;
                ast = function;
                modules.push_back(importedModule.second);
            }
        }

        if (modules.size() == 1)
        {
            return true;
        }
        else if (!modules.empty())
        {
            funModule = nullptr;
            ast = nullptr;
            Log({}, "reference to ", identifier, " is ambiguous");
            return false;
        }
        else
        {
            Log({}, "undefined function ", identifier);
            return false;
        }
    }
    else
    {
        if (moduleIdentifier == name())
        {
            auto it = functions.find(identifier);
            if (it != functions.end())
            {
                funModule = this;
                ast = it->second;
                return true;
            }
        }
        else
        {
            auto itm = importedModules.find(moduleIdentifier);
            if (itm != importedModules.end())
            {
                auto importedModule = itm->second;
                auto astFunction = importedModule->getFunction(identifier);

                if (astFunction != nullptr)
                {
                    ast = astFunction;
                    return true;
                }
            }
            else
            {
                Log(ast->location, "undefined module ", moduleIdentifier);
                return false;
            }
        }
    }

    Log({}, "undefined function ", moduleIdentifier, ':', identifier);
    return false;
}


void SourceModule::requireFunctionInstance(FunctionIdentifier function)
{
    pendingGeneration.push_back(function);
}

bool SourceModule::generateTypes()
{
    return codeGenerator->generateDataDeclarations(astModule->dataDeclarations);
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
        auto it = functions.find(function.name);
        if (it == functions.end())
        {
            Log({}, "undefined function: ", name(), ":", function.name);
            pendingGeneration.pop_back();
            return false;
        }
        auto ast = it->second;

        std::vector<type::Type*> types;
        for (auto t : *function.types)
        {
            //TODO: Now we try to find type in this module with its imports
            // but this might be called from another module with its own type...
            // an import in this module should not be required
            types.push_back(codeGenerator->getTypeContext()->getType(t->name()));
        }

        if (std::any_of(types.begin(), types.end(), [](auto x) { return x == nullptr; }))
        {
            Log(ast->location, "unknown type in: ", name(), ":", function.name);
            pendingGeneration.pop_back();
            return false;
        }

        bool result = codeGenerator->generateFunction(ast, std::move(types));
        pendingGeneration.pop_back();
        return result;
    }
    return false;
}

}