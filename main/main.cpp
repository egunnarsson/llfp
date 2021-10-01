
#pragma warning(push, 0)
// C4244 conversion, possible loss of data
// BitcodeWriter.h includes ScaledNumber.h which does a bunch of conversions of std::pair with numbers
// C4996 use of function, class member, variable, or typedef that's marked deprecated
#pragma warning(disable : 4244 4996)

#include <string>

#include "llvm/Bitcode/BitcodeWriter.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/Path.h"

#pragma warning(pop)

#include "Compiler.h"
#include "HeaderWriter.h"
#include "Lexer.h"
#include "Module.h"

static llvm::cl::list<std::string>
InputFilenames(llvm::cl::Positional, llvm::cl::desc("<Input files>"), llvm::cl::ZeroOrMore);

static llvm::cl::opt<std::string>
OutputFilename("o", llvm::cl::desc("Output filename"), llvm::cl::value_desc("filename"));

std::unique_ptr<llfp::lex::Input> makeInput(const std::string &inputFilename)
{
    if (inputFilename == "-")
    {
        return std::make_unique<llfp::lex::StdinInput>();
    }
    else
    {
        return std::make_unique<llfp::lex::FileInput>(inputFilename.c_str());
    }
}

template<class T>
llfp::Compiler::ErrorCode write(llvm::SmallString<128> &output, llvm::StringRef extention, T writeFun)
{
    llvm::sys::path::replace_extension(output, extention);
    std::error_code ec;
    llvm::raw_fd_ostream os(output, ec);
    if (ec)
    {
        llvm::errs() << ec.message() << '\n';
        return llfp::Compiler::IOError;
    }
    writeFun(os);
    if (os.has_error())
    {
        llvm::errs() << os.error().message() << '\n';
        return llfp::Compiler::IOError;
    }
    return llfp::Compiler::NoError;
}

llfp::Compiler::ErrorCode writeIR(llvm::Module *llvmModule, llvm::SmallString<128> &output)
{
    return write(output, ".ll", [llvmModule](llvm::raw_fd_ostream &os) { os << (*llvmModule); });
}

llfp::Compiler::ErrorCode writeBitcode(llvm::Module *llvmModule, llvm::SmallString<128> &output)
{
    return write(output, ".bc", [llvmModule](llvm::raw_fd_ostream &os) { llvm::WriteBitcodeToFile(*llvmModule, os); });
}

llfp::Compiler::ErrorCode writeDefFile(llfp::SourceModule *sourceModule, llvm::SmallString<128> &output)
{
    return write(output, ".def",
        [&sourceModule](llvm::raw_fd_ostream &os)
        {
            os << "LIBRARY " << sourceModule->name() << "\n";
            os << "EXPORTS\n";
            for (auto &f : sourceModule->getAST()->functions)
            {
                if (f->exported)
                {
                    os << "    " << sourceModule->getExportedName(f.get()) << "\n";
                }
            }
        });
}

llfp::Compiler::ErrorCode writeHeaderFile(llfp::SourceModule* module, llvm::SmallString<128> &output)
{
    return write(output, ".h", [&module](llvm::raw_fd_ostream &os) {
        llfp::HeaderWriter writer;
        writer.write(os, *module);
    });
}

llfp::Compiler::ErrorCode write(llfp::Compiler& c, size_t index, llvm::SmallString<128> &output)
{
    auto srcModule = c.getModule(index);
    auto llvmModule = c.getLlvmModule(index);

    llfp::Compiler::ErrorCode result = writeIR(llvmModule, output);
    if (result) { return result; }
    result = writeBitcode(llvmModule, output);
    if (result) { return result; }
    result = writeDefFile(srcModule, output);
    if (result) { return result; }
    result = writeHeaderFile(srcModule, output);
    if (result) { return result; }
    return llfp::Compiler::ErrorCode::NoError;
}

int main(int argc, char *argv[])
{
    if (!llvm::cl::ParseCommandLineOptions(argc, argv, "", &llvm::errs()))
    {
        return llfp::Compiler::ErrorCode::CommandLineArgumentError;
    }

    if (InputFilenames.empty())
    {
        InputFilenames.push_back("-");
    }

    if (InputFilenames.size() > 1 && !OutputFilename.empty())
    {
        llvm::errs() << "multiple input files specified while also specifying an output file";
        return llfp::Compiler::ErrorCode::CommandLineArgumentError;
    }

    std::vector<std::unique_ptr<llfp::lex::Input>> inputFiles;
    for (auto &inputFile : InputFilenames)
    {
        inputFiles.push_back(makeInput(inputFile));
    }

    llfp::Compiler c;
    auto result = c.compile(inputFiles);
    if (result != llfp::Compiler::ErrorCode::NoError)
    {
        return result;
    }

    llvm::SmallString<128> output;

    const auto count = inputFiles.size();
    if (count == 1)
    {
        output = OutputFilename.empty() ? InputFilenames.front() : OutputFilename;
        if (output == "-")
        {
            auto llvmModule = c.getLlvmModule(0);
            llvm::outs() << (*llvmModule);
        }
        else
        {
            return write(c, 0, output);
        }
    }
    else
    {
        for (size_t index = 0; index < count; ++index)
        {
            output = InputFilenames[index];
            result = write(c, index, output);
            if (result != 0) { return result; }
        }
    }

    return llfp::Compiler::ErrorCode::NoError;
}
