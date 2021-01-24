
#pragma warning(push, 0)
// C4244 conversion, possible loss of data
// BitcodeWriter.h includes ScaledNumber.h which does a bunch of conversions of std::pair with numbers
// C4996 use of function, class member, variable, or typedef that's marked deprecated
#pragma warning(disable : 4244 4996)

#include <string>

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
#include "HeaderWriter.h"
#include "Lexer.h"
#include "Log.h"
#include "Parser.h"


enum ErrorCodes
{
    NoError = 0,
    CommandLineArgumentError,
    ParseOrLexerError,
    TypeOrCodeGenerationError,
    LLVMError,
    IOError,
};

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
int write(llvm::SmallString<128> &output, llvm::StringRef extention, T writeFun)
{
    llvm::sys::path::replace_extension(output, extention);
    std::error_code ec;
    llvm::raw_fd_ostream os(output, ec);
    if (ec)
    {
        llvm::errs() << ec.message() << '\n';
        return IOError;
    }
    writeFun(os);
    if (os.has_error())
    {
        llvm::errs() << os.error().message() << '\n';
        return IOError;
    }
    return NoError;
}

int writeIR(llvm::Module *llvmModule, llvm::SmallString<128> &output)
{
    return write(output, ".ll", [llvmModule](llvm::raw_fd_ostream &os) { os << (*llvmModule); });
}

int writeBitcode(llvm::Module *llvmModule, llvm::SmallString<128> &output)
{
    return write(output, ".bc", [llvmModule](llvm::raw_fd_ostream &os) { llvm::WriteBitcodeToFile(*llvmModule, os); });
}

int writeDefFile(llfp::SourceModule *sourceModule, llvm::SmallString<128> &output)
{
    return write(output, ".def",
        [&sourceModule](llvm::raw_fd_ostream &os)
        {
            os << "LIBRARY " << sourceModule->name() << "\n";
            os << "EXPORTS\n";
            for (auto &f : sourceModule->getAST()->functionDeclarations)
            {
                if (f->exported)
                {
                    os << "    " << sourceModule->getExportedName(f.get()) << "\n";
                }
            }
        });
}

int writeHeaderFile(llfp::SourceModule* module, llvm::SmallString<128> &output)
{
    return write(output, ".h", [&module](llvm::raw_fd_ostream &os) {
        llfp::HeaderWriter writer;
        writer.write(os, *module);
    });
}

int createDataLayout(const std::string& targetTriple, llvm::DataLayout &dataLayout)
{
#if 1
    llvm::InitializeNativeTarget();
#else
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmPrinters();
#endif

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

int write(llfp::SourceModule* module, llvm::SmallString<128> &output)
{
    auto llvmModule = module->getLLVM();

    int result = writeIR(llvmModule, output);
    if (result) { return result; }
    result = writeBitcode(llvmModule, output);
    if (result) { return result; }
    result = writeDefFile(module, output);
    if (result) { return result; }
    result = writeHeaderFile(module, output);
    if (result) { return result; }
    return 0;
}

int main(int argc, char *argv[])
{
    if (!llvm::cl::ParseCommandLineOptions(argc, argv, "", &llvm::errs()))
    {
        return CommandLineArgumentError;
    }

    if (InputFilenames.empty())
    {
        InputFilenames.push_back("-");
    }

    if (InputFilenames.size() > 1 && !OutputFilename.empty())
    {
        llvm::errs() << "multiple input files specified while also specifying an output file";
        return CommandLineArgumentError;
    }

    std::vector<llfp::ImportedModule*> modules; // will later contain standard modules
    std::vector<std::unique_ptr<llfp::SourceModule>> sourceModules;
    for (auto &inputFile : InputFilenames)
    {
        sourceModules.push_back(std::make_unique<llfp::SourceModule>(inputFile));
        auto &sourceModule = sourceModules.back();

        auto input = makeInput(sourceModule->filePath());
        llfp::lex::Lexer lexer(input.get());
        llfp::parse::Parser parser(&lexer);
        auto astModule = parser.parse();
        if (astModule == nullptr)
        {
            return ParseOrLexerError;
        }

        if (!sourceModule->setAST(std::move(astModule)))
        {
            return TypeOrCodeGenerationError;
        }

        modules.push_back(sourceModule.get());
    }

    // resolve imports
    for (auto &sourceModule : sourceModules)
    {
        if (!sourceModule->addImportedModules(modules))
        {
            return TypeOrCodeGenerationError;
        }
    }

    auto targetTriple = llvm::sys::getDefaultTargetTriple(); // 32 bit "i386-pc-windows-msvc"
    llvm::DataLayout dataLayout("");
    int result = createDataLayout(targetTriple, dataLayout);
    if (result) { return result; }

    for (auto &sourceModule : sourceModules)
    {
        sourceModule->createCodeGenerator();

        auto llvmModule = sourceModule->getLLVM();
        llvmModule->setTargetTriple(targetTriple);
        llvmModule->setDataLayout(dataLayout);

        if (!sourceModule->generateExportedFunctions())
        {
            return TypeOrCodeGenerationError;
        }
    }

    bool done = false;
    while (!done)
    {
        done = true;
        for (auto &module : sourceModules)
        {
            if (module->generateNextFunction())
            {
                while (module->generateNextFunction()) {}
                done = false;
            }
        }
    }

    // on error quit?

    llvm::SmallString<128> output;
    if (sourceModules.size() == 1)
    {
        auto module = sourceModules.front().get();
        output = OutputFilename.empty() ? InputFilenames.front() : OutputFilename;
        if (output == "-")
        {
            auto llvmModule = module->getLLVM();
            llvm::outs() << (*llvmModule);
        }
        else
        {
            return write(module, output);
        }
    }
    else
    {
        for (auto &sourceModule : sourceModules)
        {
            output = sourceModule->filePath();
            result = write(sourceModule.get(), output);
            if (result != 0) { return result; }
        }
    }

    return NoError;
}
