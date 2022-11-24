
#include "Log.h"
#include "Module.h"

#include "GlobalContext.h"

namespace llfp
{

void GlobalContext::addModule(ImportedModule* srcModule)
{
    allModules.insert(std::make_pair(srcModule->name(), srcModule));
}

bool GlobalContext::buildFunctionInstances(SourceModule* sourceModule)
{
    bool result = true;
    auto ast    = sourceModule->getAST();

    for (auto& instance : ast->classInstances)
    {
        // check that instance->classIdentifier is visible from this module

        type::Identifier id;
        if (sourceModule->fullyQualifiedName(id, instance->typeArgument))
        {
            for (auto& function : instance->functions)
            {
                auto predicate = [&function](std::unique_ptr<ast::Function>& f) { return f->name == function->name; };
                if (std::find_if(ast->functions.begin(), ast->functions.end(), predicate) != ast->functions.end())
                {
                    Log(function->location, "function already defined");
                    result = false;
                }
                else
                {
                    auto& map = functionInstances[function->name];
                    auto  it  = map.insert({ std::move(id), FunAst{ sourceModule, function.get() } });
                    if (!it.second)
                    {
                        auto& typeKey = it.first->first;
                        Log(function->location, "multiple instances of class for '", typeKey.str(), '\'');
                        result = false;
                    }
                }
            }
        }
        else
        {
            Log(instance->location, "unknown type in class instance");
            result = false;
            // we should also check class identifier...
        }
    }

    return result;
}

void GlobalContext::addFunctionInstance(const std::string& name, type::Identifier type, FunAst fun)
{
    auto& map = functionInstances[name];
    auto  it  = map.insert({ std::move(type), fun });
    assert(it.second);
}

FunAst GlobalContext::lookupInstance(const std::string& funIdentifier, const type::Identifier& typeIdentifier) const
{
    auto it = functionInstances.find(funIdentifier);
    if (it != functionInstances.end())
    {
        auto it2 = it->second.find(typeIdentifier);
        if (it2 != it->second.end())
        {
            return it2->second;
        }
    }
    return { nullptr, nullptr };
}

DataAst GlobalContext::lookupTypeGlobal(const GlobalIdentifier& identifier) const
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

ImportedModule* GlobalContext::getModule(const std::string& name) const
{
    return allModules.at(name);
}

} // namespace llfp
