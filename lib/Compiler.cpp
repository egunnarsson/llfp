
#include "Log.h"
#include "Module.h"

#include "Compiler.h"

namespace llfp
{

bool Compiler::compile(const std::vector<std::unique_ptr<lex::Input>>& sourceFiles)
{




    for (auto& sourceModule : sourceModules)
    {
        auto ast = sourceModule->getAST();

        for (auto& instance : ast->classInstances)
        {
            type::Identifier id;
            if (sourceModule->fullyQualifiedName(id, instance->typeArgument))
            {
                for (auto& function : instance->functions)
                {
                    auto& map = functionInstances[function->name];
                    auto it = map.insert({ std::move(id), FunAst{ sourceModule.get(), function.get()} });
                    if (!it.second)
                    {
                        auto& typeKey = it.first->first;
                        Log(function->location, "multiple instances of class for '", typeKey.str(), '\'');
                    }
                }
            }
            else
            {
                Log(instance->location, "unknown type in class instance");
                // we should also check class identifier...
            }
        }
    }




}

FunAst Compiler::lookupInstance(const std::string& funIdentifier, const type::Identifier& typeIdentifier)
{
    auto it = functionInstances.find(funIdentifier);
    if (it == functionInstances.end())
    {
        auto it2 = it->second.find(typeIdentifier);
        if (it2 != it->second.end())
        {
            return it2->second;
        }
    }
    return {nullptr, nullptr};
}

DataAst Compiler::lookupTypeGlobal(const GlobalIdentifier& identifier) const
{
    assert(!identifier.moduleName.empty());
    assert(!identifier.name.empty());

    auto it = allModules.find(identifier.moduleName);
    if (it != allModules.end())
    {
        auto astType = it->second->getType(identifier.name);
        if (!astType.empty())
        {
            return astType;
        }
    }

    return { nullptr, nullptr };
}

void Compiler::requireFunctionInstance(FunctionIdentifier function)
{
    pendingGeneration.push_back(function);
}

bool Compiler::generateNextFunction()
{
    if (!pendingGeneration.empty())
    {
        auto functionId = pendingGeneration.back();
        auto ast = functionId.ast.function;

        std::vector<type::TypePtr> types;
        for (auto t : *functionId.types)
        {
            //TODO: Now we try to find type in this module with its imports
            // but this might be called from another module with its own type...
            // an import in this module should not be required
            types.push_back(codeGenerator->getTypeContext()->getType(t->identifier()));
        }

        if (std::any_of(types.begin(), types.end(), [](auto x) { return x == nullptr; }))
        {
            Log(ast->location, "unknown type in: ", functionId.ast.importedModule->name(), ':', ast->name);
            pendingGeneration.pop_back();
            return false;
        }

        bool result = codeGenerator->generateFunction(ast, std::move(types));
        pendingGeneration.pop_back();
        return result;
    }
    return false;
}

} // namespece llfp
