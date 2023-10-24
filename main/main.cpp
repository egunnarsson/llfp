
#include "Error.h"
#include "HeaderWriter.h"
#include "Lexer.h"
#include "llfp.h"
#include "Module.h"
#include "NameMangling.h"
#include "Utils/DotFileWriter.h"

#pragma warning(disable : 4244 4267)
#include <llvm/Bitcode/BitcodeWriter.h>
#pragma warning(default : 4244 4267)
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Path.h>

#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>


static llvm::cl::OptionCategory LLFPOptionCategory("LLFP Options");

static llvm::cl::list<std::string> InputFilenames(
    llvm::cl::Positional,
    llvm::cl::desc("<Input files>"),
    llvm::cl::OneOrMore,
    llvm::cl::cat(LLFPOptionCategory));
static llvm::cl::opt<std::string> OutputFilename(
    "o",
    llvm::cl::desc("Output filename"),
    llvm::cl::value_desc("filename"),
    llvm::cl::cat(LLFPOptionCategory));
static llvm::cl::opt<std::string> OutputDirectory(
    "d",
    llvm::cl::desc("Output directory"),
    llvm::cl::value_desc("directory"),
    llvm::cl::cat(LLFPOptionCategory));
static llvm::cl::opt<bool> GenDotFiles(
    "graph",
    llvm::cl::desc("Generate dot graph file from parsed AST"),
    llvm::cl::cat(LLFPOptionCategory));

namespace
{

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
    catch ([[maybe_unused]] const std::ifstream::failure& e)
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

llfp::ReturnCode writeDefFile(llvm::ArrayRef<llfp::CompiledModule> modules, llvm::SmallString<128>& output)
{
    return write(output, ".def",
                 [&modules, &output](llvm::raw_fd_ostream& os) {
                     os << "LIBRARY " << llvm::sys::path::filename(output) << "\n";
                     os << "EXPORTS\n";
                     for (auto& m : modules)
                     {
                         for (auto& f : m.sourceModule->getAST()->functions)
                         {
                             if (f->exported)
                             {
                                 os << "    " << llfp::getExportedName(*m.sourceModule, f.get()) << "\n";
                             }
                         }
                     }
                 });
}

llfp::ReturnCode writeHeaderFile(llfp::SourceModule& module, llvm::SmallString<128>& output)
{
    return write(output, ".h",
                 [&module](llvm::raw_fd_ostream& os) {
                     llfp::HeaderWriter writer;
                     writer.write(os, module);
                 });
}

llfp::ReturnCode writeDotFile(llfp::SourceModule& module, llvm::SmallString<128>& output)
{
    return write(output, ".dot",
                 [&module](llvm::raw_fd_ostream& os) {
                     llfp::utils::dot::writeDotFile(os, *module.getAST());
                 });
}

llfp::ReturnCode write(llfp::CompiledModule& compiledModule, llvm::SmallString<128>& output)
{
    auto srcModule  = compiledModule.sourceModule.get();
    auto llvmModule = compiledModule.llvmModule.get();

    auto result = writeIR(llvmModule, output);
    if (llfp::error(result))
    {
        return result;
    }

    result = writeBitcode(llvmModule, output);
    if (llfp::error(result))
    {
        return result;
    }

    result = writeDefFile(compiledModule, output);
    if (llfp::error(result))
    {
        return result;
    }

    if (GenDotFiles)
    {
        result = writeDotFile(*compiledModule.sourceModule, output);
        if (llfp::error(result))
        {
            return result;
        }
    }

    return writeHeaderFile(*srcModule, output);
}

llfp::ReturnCode write(llfp::LinkedModule& compiledModule, llvm::SmallString<128>& output)
{
    auto llvmModule = compiledModule.llvmModule.get();

    auto result = writeIR(llvmModule, output);
    if (llfp::error(result))
    {
        return result;
    }

    return writeBitcode(llvmModule, output);
}

void combinePath(llvm::SmallString<128>& out, const std::string& dir, const std::string& name)
{
    out = (std::filesystem::path{ dir } / name).string();
}

llfp::ReturnCode llfp_main(int argc, char* argv[])
{
    llvm::cl::HideUnrelatedOptions(LLFPOptionCategory);
    if (!llvm::cl::ParseCommandLineOptions(argc, argv, "", &llvm::errs()))
    {
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
    if (llfp::error(returnCode))
    {
        return returnCode;
    }

    try
    {
        auto result = llfp::compile(inputFiles);
        auto count  = result.size();

        llvm::SmallString<128> output;

        if (OutputFilename.empty())
        {
            for (size_t index = 0; index < count; ++index)
            {
                combinePath(output, OutputDirectory.getValue(), result[index].sourceModule->name());
                returnCode = write(result[index], output);
                if (llfp::error(returnCode))
                {
                    return returnCode;
                }
            }
        }
        else // link
        {
            combinePath(output, OutputDirectory.getValue(), OutputFilename.getValue());
            auto linkResult = llfp::link(OutputFilename, result);

            returnCode = write(linkResult, output);
            if (llfp::error(returnCode))
            {
                return returnCode;
            }

            returnCode = writeDefFile(result, output);
            if (llfp::error(returnCode))
            {
                return returnCode;
            }

            for (auto& compiledModule : result)
            {
                combinePath(output, OutputDirectory.getValue(), compiledModule.sourceModule->name());
                returnCode = writeHeaderFile(*compiledModule.sourceModule, output);
                if (llfp::error(returnCode))
                {
                    return returnCode;
                }

                if (GenDotFiles)
                {
                    returnCode = writeDotFile(*compiledModule.sourceModule, output);
                    if (llfp::error(returnCode))
                    {
                        return returnCode;
                    }
                }
            }
        }
    }
    catch (const llfp::ReturnCode& error)
    {
        return error;
    }

    return llfp::ReturnCode::NoError;
}

} // namespace

int main(int argc, char* argv[])
{
    return static_cast<int>(llfp_main(argc, argv));
}
