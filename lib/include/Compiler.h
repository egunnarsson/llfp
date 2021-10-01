#pragma once

#include <unordered_map>
#include <string>

#pragma warning(push, 0)

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#pragma warning(pop)

#include "Codegen.h"
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

struct Unit
{
    std::unique_ptr<llvm::LLVMContext> llvmContext;
    std::unique_ptr<llvm::Module> llvmModule;
    std::unique_ptr<SourceModule> sourceModule;
    std::unique_ptr<codegen::CodeGenerator> codeGenerator;
};

// rename global context, and have compile function as static?
class Compiler
{
    std::vector<Unit> compileModules;

    std::unordered_map<std::string, ImportedModule*> allModules;
    std::unordered_map<std::string, std::unordered_map<type::Identifier, FunAst>> functionInstances;

    std::vector<FunctionIdentifier> pendingGeneration; // Driver

public:
    
    enum ErrorCode
    {
        NoError = 0,
        CommandLineArgumentError,
        ParseOrLexerError,
        TypeOrCodeGenerationError,
        LLVMError,
        IOError,
    };

    // also return vector of llvmModules
    ErrorCode compile(const std::vector<std::unique_ptr<lex::Input>> &sourceFiles);

    FunAst    lookupInstance(const std::string& funIdentifier, const type::Identifier& typeIdentifier);
    DataAst   lookupTypeGlobal(const GlobalIdentifier& identifier) const;

    void      requireFunctionInstance(FunctionIdentifier function);

    SourceModule* getModule(size_t index);
    llvm::Module* getLlvmModule(size_t index);

private:

    bool      generateNextFunction();
};

} // namespace llfp
