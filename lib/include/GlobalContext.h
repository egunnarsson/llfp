#pragma once

#include "IModule.h"

#include <string>
#include <unordered_map>

namespace llfp
{

class SourceModule;
class ImportedModule;

class GlobalContext
{
    std::unordered_map<std::string, ImportedModule*>                              allModules;
    std::unordered_map<std::string, std::unordered_map<type::Identifier, FunAst>> functionInstances;

public:

    void            addModule(ImportedModule* srcModule);
    ImportedModule* getModule(const std::string& name) const;

    bool buildFunctionInstances(SourceModule* sourceModule);
    void addFunctionInstance(const std::string& name, type::Identifier type, FunAst fun);

    FunAst  lookupInstance(const std::string& funIdentifier, const type::Identifier& typeIdentifier) const;
    DataAst lookupTypeGlobal(const GlobalIdentifier& identifier) const;
};

} // namespace llfp
