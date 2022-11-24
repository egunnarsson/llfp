#pragma once

#include "Codegen.h"
#include "Lexer.h"
#include "Module.h"

#pragma warning(push, 0)

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#pragma warning(pop)

#include <memory>
#include <vector>

namespace llfp
{

enum class ReturnCode
{
    NoError = 0,
    CommandLineArgumentError,
    ParseOrLexerError,
    TypeOrCodeGenerationError,
    LLVMError,
    IOError,
};

constexpr bool error(ReturnCode c)
{
    return c != ReturnCode::NoError;
}

struct CompiledModule
{
    std::unique_ptr<llvm::LLVMContext>      llvmContext;
    std::unique_ptr<llvm::Module>           llvmModule;
    std::unique_ptr<SourceModule>           sourceModule;
    std::unique_ptr<codegen::CodeGenerator> codeGenerator;
};

std::vector<CompiledModule> compile(const std::vector<std::unique_ptr<lex::Input>>& sourceFiles);

// used for future Language Server
// llfp::TaggedAst parse(std::string source);

} // namespace llfp
