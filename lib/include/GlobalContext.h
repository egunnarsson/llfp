#pragma once

#include <unordered_map>
#include <string>

#include "IModule.h"

namespace llfp
{

class SourceModule;
class ImportedModule;

class GlobalContext
{
    std::unordered_map<std::string, ImportedModule*> allModules;
    std::unordered_map<std::string, std::unordered_map<type::Identifier, FunAst>> functionInstances;

public:

    void            addModule(SourceModule *srcModule);
    ImportedModule* getModule(const std::string& name);

    bool            buildFunctionInstances(SourceModule* sourceModule);

    FunAst          lookupInstance(const std::string& funIdentifier, const type::Identifier& typeIdentifier);
    DataAst         lookupTypeGlobal(const GlobalIdentifier& identifier) const;
};

} // namespace llfp
