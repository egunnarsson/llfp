
// C4244 conversion, possible loss of data
// BitcodeWriter.h includes ScaledNumber.h which does a bunch of conversions of std::pair with numbers
#define _STL_EXTRA_DISABLED_WARNINGS 4244

#include <string>

#pragma warning(push, 0)

#include "llvm/Bitcode/BitcodeWriter.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/Path.h"

#include "llvm/Target/TargetOptions.h"
#include "llvm/Target/TargetMachine.h"

#pragma warning(pop)

#include "Codegen.h"
#include "Lexer.h"
#include "Parser.h"


enum ErrorCodes
{
    NoError = 0,
    ParseOrLexerError,
    TypeOrCodeGenerationError,
    LLVMError,
    IOError,
};

static llvm::cl::opt<std::string>
InputFilename(llvm::cl::Positional, llvm::cl::desc("<input file>"), llvm::cl::init("-"));

static llvm::cl::opt<std::string>
OutputFilename("o", llvm::cl::desc("Output filename"), llvm::cl::value_desc("filename"));

std::unique_ptr<llfp::lex::Input> makeInput()
{
    if (InputFilename == "-")
    {
        return std::make_unique<llfp::lex::StdinInput>();
    }
    else
    {
        return std::make_unique<llfp::lex::FileInput>(InputFilename.c_str());
    }
}

template<class T>
int write(llvm::SmallString<128> &output, llvm::StringRef extention, T writeFun)
{
    llvm::sys::path::replace_extension(output, extention);
    std::error_code ec;
    llvm::raw_fd_ostream os(output, ec);
    if (ec)
    {
        llvm::errs() << ec.message() << "\n";
        return IOError;
    }
    writeFun(os);
    if (os.has_error())
    {
        llvm::errs() << os.error().message() << "\n";
        return IOError;
    }
    return NoError;
}

int writeIR(std::unique_ptr<llvm::Module> &llvmModule, llvm::SmallString<128> &output)
{
    return write(output, ".ll", [&llvmModule](llvm::raw_fd_ostream &os) { os << (*llvmModule); });
}

int writeBitcode(std::unique_ptr<llvm::Module> &llvmModule, llvm::SmallString<128> &output)
{
    return write(output, ".bc", [&llvmModule](llvm::raw_fd_ostream &os) { llvm::WriteBitcodeToFile(*llvmModule, os); });
}

int writeDefFile(std::unique_ptr<llfp::ast::Module> &module, llvm::SmallString<128> &output)
{
    return write(output, ".def",
        [&module](llvm::raw_fd_ostream &os)
        {
            os << "LIBRARY " << module->identifier << "\n";
            os << "EXPORTS\n";
            // TODO: this will include the _DllMainCRTStartup, should it?
            // when we check "exported" functions this will not be a problem
            for (auto &f : module->functionDeclarations)
            {
                os << "    " << f->identifier << "\n";
            }
        });
}

int createDataLayout(llvm::StringRef targetTriple, llvm::DataLayout &dataLayout)
{
    llvm::InitializeNativeTarget();

    std::string targetError;
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, targetError);

    if (target == nullptr)
    {
        llvm::errs() << targetError << "\n";
        return LLVMError;
    }

    auto CPU = "generic";
    auto Features = "";

    llvm::TargetOptions opt;
    auto RM = llvm::Optional<llvm::Reloc::Model>();
    auto targetMachine = target->createTargetMachine(targetTriple, CPU, Features, opt, RM);

    if (targetMachine == nullptr)
    {
        llvm::errs() << "Failed to create target machine\n";
        return LLVMError;
    }

    dataLayout = targetMachine->createDataLayout();

    return NoError;
}

int main(int argc, char *argv[])
{
    llvm::cl::ParseCommandLineOptions(argc, argv, "");

    llvm::SmallString<128> output = OutputFilename.empty() ? InputFilename : OutputFilename;

    auto input = makeInput();

    llfp::lex::Lexer lexer(input.get());
    llfp::parse::Parser parser(&lexer);

    auto module = parser.parse();

    if (module == nullptr)
    {
        llvm::errs() << "Parse error\n";
        return ParseOrLexerError;
    }

    llfp::codegen::CodeGenerator codeGen;
    auto llvmModule = codeGen.generate(module);

    if (llvmModule == nullptr)
    {
        llvm::errs() << "Code generation error\n";
        return TypeOrCodeGenerationError;
    }

    auto targetTriple = llvm::sys::getDefaultTargetTriple();
    llvm::DataLayout dataLayout("");
    int result = createDataLayout(targetTriple, dataLayout);
    if (result) { return result; }

    llvmModule->setTargetTriple(targetTriple);
    llvmModule->setDataLayout(dataLayout);

    if (output == "-")
    {
        // if binary
        // else
        llvm::outs() << (*llvmModule);
    }
    else
    {
        result = writeIR(llvmModule, output);
        if (result) { return result; }
        result = writeBitcode(llvmModule, output);
        if (result) { return result; }
        result = writeDefFile(module, output);
        if (result) { return result; }
    }

    return NoError;
}