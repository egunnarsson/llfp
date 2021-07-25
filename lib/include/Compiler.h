#pragma once

#include <unordered_map>
#include <string>

#include "IModule.h"
#include "Type.h"

namespace llfp
{

class SourceModule;
class ImportedModule;


struct FunctionIdentifier
{
    FunAst ast;
    std::vector<type::TypePtr>* types;
};


class Compiler
{
    std::vector<std::unique_ptr<llfp::SourceModule>> sourceModules;

    std::unordered_map<std::string, ImportedModule*> allModules;
    std::unordered_map<std::string, std::unordered_map<type::Identifier, FunAst>> functionInstances;

    std::vector<FunctionIdentifier> pendingGeneration; // Driver

public:

    // also return vector of llvmModules
    bool compile(const std::vector<std::unique_ptr<lex::Input>> &sourceFiles);

    FunAst  lookupInstance(const std::string& funIdentifier, const type::Identifier& typeIdentifier);
    DataAst lookupTypeGlobal(const GlobalIdentifier& identifier) const;

    void requireFunctionInstance(FunctionIdentifier function);
    bool generateNextFunction();
};

} // namespece llfp
