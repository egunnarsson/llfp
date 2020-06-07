
#pragma warning(push, 0)

#include <string>

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FormatVariadic.h"

#pragma warning(pop)

#include "Codegen.h"
#include "Lexer.h"
#include "Parser.h"


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

int main(int argc, char *argv[])
{
    llvm::cl::ParseCommandLineOptions(argc, argv, "");

    if (OutputFilename.empty())
    {
        OutputFilename = InputFilename == "-" ? "-" : InputFilename + ".ll";
    }

    auto input = makeInput();

    llfp::lex::Lexer lexer(input.get());
    llfp::parse::Parser parser(&lexer);

    auto module = parser.parse();

    if (module == nullptr)
    {
        return 1;
    }

    llfp::codegen::CodeGenerator codeGen;
    auto llvmModule = codeGen.generate(module);

    if (llvmModule == nullptr)
    {
        return 2;
    }

    if (OutputFilename == "-")
    {
        llvm::outs() << (*llvmModule);
    }
    else
    {
        std::error_code ec;
        llvm::raw_fd_ostream os(OutputFilename, ec);
        if (ec)
        {
            return 3;
        }
        os << (*llvmModule);
    }

    return 0;
}