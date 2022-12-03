
#include "Error.h"
#include "HeaderWriter.h"
#include "Lexer.h"
#include "llfp.h"
#include "Module.h"

#pragma warning(push, 0)
// C4244 conversion, possible loss of data
// BitcodeWriter.h includes ScaledNumber.h which does a bunch of conversions of std::pair with numbers
// C4996 use of function, class member, variable, or typedef that's marked deprecated
#pragma warning(disable : 4244 4996)

#include <llvm/Bitcode/BitcodeWriter.h>

#include <llvm/Support/CommandLine.h>
#include <llvm/Support/FormatVariadic.h>
#include <llvm/Support/Path.h>

#pragma warning(pop)

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>


static llvm::cl::list<std::string> InputFilenames(llvm::cl::Positional, llvm::cl::desc("<Input files>"), llvm::cl::ZeroOrMore);
static llvm::cl::opt<std::string>  OutputFilename("o", llvm::cl::desc("Output filename"), llvm::cl::value_desc("filename"));

llfp::Source readInput(const std::string& inputFilename)
{
    std::ostringstream buffer;
    try
    {
        if (inputFilename == "-")
        {
            std::cin.exceptions(std::ios::badbit | std::ios::failbit);
            buffer << std::cin.rdbuf();
        }
        else
        {
            std::ifstream fileStream;
            fileStream.exceptions(std::ios::badbit | std::ios::failbit);
            fileStream.open(inputFilename, std::ios::in | std::ios::binary);
            buffer << fileStream.rdbuf();
        }
    }
    catch (const std::ifstream::failure& e)
    {
        // e.what() does not seem to return anything useful
        throw llfp::Error(std::string{ std::strerror(errno) } + " (" + inputFilename + ')');
    }
    return { inputFilename, buffer.str() };
}

template<class T>
llfp::ReturnCode write(llvm::SmallString<128>& output, llvm::StringRef extention, T writeFun)
{
    llvm::sys::path::replace_extension(output, extention);
    std::error_code      ec;
    llvm::raw_fd_ostream os(output, ec);
    if (ec)
    {
        llvm::errs() << ec.message() << '\n';
        return llfp::ReturnCode::IOError;
    }
    writeFun(os);
    if (os.has_error())
    {
        llvm::errs() << os.error().message() << '\n';
        return llfp::ReturnCode::IOError;
    }
    return llfp::ReturnCode::NoError;
}

llfp::ReturnCode writeIR(llvm::Module* llvmModule, llvm::SmallString<128>& output)
{
    return write(output, ".ll", [llvmModule](llvm::raw_fd_ostream& os) { os << (*llvmModule); });
}

llfp::ReturnCode writeBitcode(llvm::Module* llvmModule, llvm::SmallString<128>& output)
{
    return write(output, ".bc", [llvmModule](llvm::raw_fd_ostream& os) { llvm::WriteBitcodeToFile(*llvmModule, os); });
}

llfp::ReturnCode writeDefFile(llfp::SourceModule* sourceModule, llvm::SmallString<128>& output)
{
    return write(output, ".def",
                 [&sourceModule](llvm::raw_fd_ostream& os) {
                     os << "LIBRARY " << sourceModule->name() << "\n";
                     os << "EXPORTS\n";
                     for (auto& f : sourceModule->getAST()->functions)
                     {
                         if (f->exported)
                         {
                             os << "    " << sourceModule->getExportedName(f.get()) << "\n";
                         }
                     }
                 });
}

llfp::ReturnCode writeHeaderFile(llfp::SourceModule* module, llvm::SmallString<128>& output)
{
    return write(output, ".h",
                 [&module](llvm::raw_fd_ostream& os) {
                     llfp::HeaderWriter writer;
                     writer.write(os, *module);
                 });
}

llfp::ReturnCode write(llfp::CompiledModule& compiledModule, llvm::SmallString<128>& output)
{
    auto srcModule  = compiledModule.sourceModule.get();
    auto llvmModule = compiledModule.llvmModule.get();

    llfp::ReturnCode result = writeIR(llvmModule, output);
    if (llfp::error(result)) { return result; }

    result = writeBitcode(llvmModule, output);
    if (llfp::error(result)) { return result; }

    result = writeDefFile(srcModule, output);
    if (llfp::error(result)) { return result; }

    return writeHeaderFile(srcModule, output);
}

llfp::ReturnCode llfp_main(int argc, char* argv[])
{
    if (!llvm::cl::ParseCommandLineOptions(argc, argv, "", &llvm::errs()))
    {
        return llfp::ReturnCode::CommandLineArgumentError;
    }

    if (InputFilenames.empty())
    {
        InputFilenames.push_back("-");
    }

    if (InputFilenames.size() > 1 && !OutputFilename.empty())
    {
        llvm::errs() << "multiple input files specified while also specifying an output file\n";
        return llfp::ReturnCode::CommandLineArgumentError;
    }

    llfp::ReturnCode          returnCode = llfp::ReturnCode::NoError;
    std::vector<llfp::Source> inputFiles;
    for (auto& inputFile : InputFilenames)
    {
        try
        {
            inputFiles.push_back(readInput(inputFile));
        }
        catch (const llfp::Error& e)
        {
            llvm::errs() << e.what() << '\n';
            returnCode = llfp::ReturnCode::CommandLineArgumentError;
        }
    }
    if (returnCode != llfp::ReturnCode::NoError)
    {
        return returnCode;
    }

    try
    {
        auto result = llfp::compile(inputFiles);
        auto count  = result.size();

        llvm::SmallString<128> output;
        if (count == 1)
        {
            output = OutputFilename.empty() ? InputFilenames.front() : OutputFilename;
            if (output == "-")
            {
                auto& llvmModule = *result[0].llvmModule;
                llvm::outs() << llvmModule;
            }
            else
            {
                return write(result[0], output);
            }
        }
        else
        {
            for (size_t index = 0; index < count; ++index)
            {
                output    = InputFilenames[index];
                auto code = write(result[index], output);
                if (llfp::error(code)) { return code; }
            }
        }
    }
    catch (const llfp::ReturnCode& error)
    {
        return error;
    }

    return llfp::ReturnCode::NoError;
}

int main(int argc, char* argv[])
{
    return static_cast<int>(llfp_main(argc, argv));
}
