
#include "Module.h"

#include "Codegen.h"
#include "Common/Algorithm.h"
#include "Error.h"
#include "GlobalContext.h"
#include "Log.h"


namespace llfp
{

ImportedModule::~ImportedModule() {}


std::unique_ptr<SourceModule> SourceModule::create(std::unique_ptr<ast::Module> astModule)
{
    std::unique_ptr<SourceModule> sourceModule = std::make_unique<SourceModule>();

    for (auto& function : astModule->functions)
    {
        auto insert = sourceModule->functions.insert((std::make_pair(function->name, function.get())));
        if (!insert.second)
        {
            Log(function->location, "function already defined");
            return nullptr;
        }
    }

    for (auto& publicDecl : astModule->publics)
    {
        auto predicate = [&publicDecl](std::unique_ptr<ast::Function>& function) { return publicDecl.name == function->name; };
        auto it        = std::find_if(astModule->functions.begin(), astModule->functions.end(), predicate);
        if (it != astModule->functions.end())
        {
            auto& function = *it;
            sourceModule->publicFunctions.insert(std::make_pair(function->name, function.get()));
        }
        else
        {
            Log(publicDecl.location, "function declared as public is not defined ", publicDecl.name);
            return nullptr;
        }
    }

    for (auto& classDecl : astModule->classes)
    {
        for (auto& funDecl : classDecl->functions)
        {
            if (sourceModule->functions.find(funDecl->name) != sourceModule->functions.end())
            {
                Log(funDecl->location, "function already defined");
                return nullptr;
            }

            FunDeclAst funAst{ sourceModule.get(), classDecl.get(), funDecl.get() };
            auto       it = sourceModule->functionDeclarations.insert(std::make_pair(funDecl->name, funAst));
            if (!it.second)
            {
                Log(funDecl->location, "function declaration already defined");
                return nullptr;
            }
        }
    }

    for (auto& dataDecl : astModule->datas)
    {
        auto insert = sourceModule->dataDeclarations.insert(std::make_pair(dataDecl->name, dataDecl.get()));
        if (!insert.second)
        {
            Log(dataDecl->location, "data already defined");
            return nullptr;
        }

        // check duplicate fields;
        for (auto& constructor : dataDecl->constructors)
        {
            auto end = constructor.fields.end();
            for (auto it = constructor.fields.begin(); it != end; ++it)
            {
                auto predicate = [it](const llfp::ast::Field& field) { return it->name == field.name; };
                auto result    = std::find_if(it + 1, end, predicate);
                if (result != end)
                {
                    Log(result->location, "duplicate field \"", result->name, '"');
                    return nullptr;
                }
            }
        }
    }

    sourceModule->astModule = std::move(astModule);
    return sourceModule;
}

bool SourceModule::addImportedModules(GlobalContext& globalContext)
{
    assert(astModule != nullptr);
    bool result = true;

    for (auto& importDecl : astModule->imports)
    {
        auto module = globalContext.getModule(importDecl.name);
        if (module != nullptr)
        {
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

const std::string& SourceModule::name() const
{
    return astModule->name;
}

ast::Module* SourceModule::getAST()
{
    return astModule.get();
}

FunAst SourceModule::getFunction(const std::string& name)
{
    auto ast = find(publicFunctions, name);
    return ast != nullptr ? FunAst{ this, ast } : FunAst{ nullptr, nullptr };
}

FunDeclAst SourceModule::getFunctionDecl(const std::string& name)
{
    return find(functionDeclarations, name, { nullptr, nullptr, nullptr });
}

DataAst SourceModule::getType(const std::string& name) const
{
    auto ast = find(dataDeclarations, name);
    return ast != nullptr ? DataAst{ this, ast } : DataAst{ nullptr, nullptr };
}

DataAst SourceModule::getConstructor(const std::string& id) const
{
    for (const auto& [typeName, dataAst] : dataDeclarations)
    {
        if (typeName == id)
        {
            return { this, dataAst };
        }
        for (const auto& constructor : dataAst->constructors)
        {
            if (constructor.name == id)
            {
                return { this, dataAst };
            }
        }
    }
    return { nullptr, nullptr };
}

template<class AstNode, class LocalFun, class GlobalFun>
AstNode SourceModule::lookup(
    const GlobalIdentifier& identifier,
    LocalFun                localLookup,
    GlobalFun               globalLookup,
    llvm::StringLiteral     errorMsg) const
{
    if (identifier.moduleName.empty())
    {
        std::vector<AstNode> results;

        auto localAst = localLookup(identifier.name);
        if (!localAst.empty())
        {
            results.push_back(localAst);
        }

        for (auto& itm : importedModules)
        {
            auto importedModule = itm.second;
            auto globalAst      = globalLookup(importedModule, identifier.name);
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
            throw Error{ std::string{ "reference to " } + identifier.str() + " is ambiguous" };
        }
        else
        {
            if (!errorMsg.empty())
            {
                throw Error{ errorMsg.str() + identifier.str() };
            }
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
                auto globalAst      = globalLookup(importedModule, identifier.name);

                if (!globalAst.empty())
                {
                    return globalAst;
                }
            }
            else
            {
                throw Error{ std::string{ "undefined module " } + identifier.moduleName };
            }
        }
    }

    if (!errorMsg.empty())
    {
        throw Error{ errorMsg.str() + identifier.str() };
    }
    return AstNode{};
}

FunDeclAst SourceModule::lookupFunctionDecl(const GlobalIdentifier& identifier)
{
    return lookup<FunDeclAst>(
        identifier,
        [this](const std::string& id) { return getFunctionDecl(id); },
        [](ImportedModule* module, const std::string& id) { return module->getFunctionDecl(id); },
        "");
}

FunAst SourceModule::lookupFunction(const GlobalIdentifier& identifier)
{
    return lookup<FunAst>(
        identifier,
        [this](const std::string& id) {
            auto ast = find(functions, id);
            return ast != nullptr ? FunAst{ this, ast } : FunAst{};
        },
        [](ImportedModule* module, const std::string& id) { return module->getFunction(id); },
        "");
}

DataAst SourceModule::lookupType(const GlobalIdentifier& identifier) const
{
    return lookup<DataAst>(
        identifier,
        [this](const std::string& id) { return getType(id); },
        [](ImportedModule* module, const std::string& id) { return module->getType(id); },
        "undefined data type ");
}

DataAst SourceModule::lookupConstructor(const GlobalIdentifier& identifier) const
{
    return lookup<DataAst>(
        identifier, [this](const std::string& id) -> DataAst { return getConstructor(id); },
        [](ImportedModule* module, const std::string& id) -> DataAst { return module->getConstructor(id); },
        "undefined constructor ");
}

} // namespace llfp
