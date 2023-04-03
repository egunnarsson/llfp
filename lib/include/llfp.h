#pragma once

#include "Codegen.h"
#include "Lexer.h"
#include "Module.h"
#include "Source.h"

#pragma warning(push, 0)

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

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
    LinkError,
    LLVMError,
    IOError,
};

constexpr bool error(ReturnCode c)
{
    return c != ReturnCode::NoError;
}

struct CompiledModule
{
    const Source*                           source;
    std::unique_ptr<llvm::LLVMContext>      llvmContext;
    std::unique_ptr<llvm::Module>           llvmModule;
    std::unique_ptr<SourceModule>           sourceModule;
    std::unique_ptr<codegen::CodeGenerator> codeGenerator;
};

struct LinkedModule
{
    std::unique_ptr<llvm::LLVMContext> llvmContext;
    std::unique_ptr<llvm::Module>      llvmModule;
};

std::vector<CompiledModule> compile(const std::vector<Source>& sources);
LinkedModule                link(llvm::StringRef name, const std::vector<CompiledModule>& modules);

// used for future Language Server
// llfp::TaggedAst parse(std::string source);

} // namespace llfp
