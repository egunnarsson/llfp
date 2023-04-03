
#include "llfp.h"

#include "Driver.h"
#include "GlobalContext.h"
#include "MathModule.h"
#include "Module.h"
#include "Parser.h"
#include "ResolveIdentifiers.h"

#include <llvm/MC/TargetRegistry.h>

#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>

#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

#include <llvm/Bitcode/BitcodeReader.h>
#pragma warning(disable : 4244)
#include <llvm/Bitcode/BitcodeWriter.h>
#pragma warning(default : 4244)
#include <llvm/Support/MemoryBuffer.h>

#include <llvm/Linker/Linker.h>


namespace llfp
{

namespace
{

bool createDataLayout(const std::string& targetTriple, llvm::DataLayout& dataLayout)
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
    auto        target = llvm::TargetRegistry::lookupTarget(targetTriple, targetError);

    if (target == nullptr)
    {
        llvm::errs() << targetError << "\n";
        return false;
    }

    auto CPU      = "generic";
    auto Features = "";

    llvm::TargetOptions opt;
    auto                RM            = llvm::Optional<llvm::Reloc::Model>();
    auto                targetMachine = target->createTargetMachine(targetTriple, CPU, Features, opt, RM);

    if (targetMachine == nullptr)
    {
        llvm::errs() << "Failed to create target machine\n";
        return false;
    }

    dataLayout = targetMachine->createDataLayout();

    return true;
}

bool generateExportedFunctions(codegen::CodeGenerator* codeGenerator, SourceModule* sourceModule)
{
    for (auto& function : sourceModule->getAST()->functions)
    {
        if (function->exported)
        {
            if (!codeGenerator->generateFunction(function.get()))
            {
                return false;
            }
        }
    }
    return true;
}

codegen::CodeGenerator* getCodeGenerator(const std::vector<CompiledModule>& result, const ImportedModule* m)
{
    for (auto& cm : result)
    {
        if (cm.sourceModule.get() == m)
        {
            return cm.codeGenerator.get();
        }
    }
    assert(false);
    return nullptr;
}

bool generateNextFunction(std::vector<CompiledModule>& result, FunctionIdentifier functionId)
{
    auto ast           = functionId.ast.function;
    auto m             = functionId.ast.importedModule;
    auto codeGenerator = getCodeGenerator(result, m);

    std::vector<type::TypeInstPtr> types;
    for (auto t : *functionId.types)
    {
        // TODO: Now we try to find type in this module with its imports
        //  but this might be called from another module with its own type...
        //  an import in this module should not be required
        types.push_back(codeGenerator->getTypeContext()->getType(t->identifier()));
    }

    if (std::any_of(types.begin(), types.end(), [](auto x) { return x == nullptr; }))
    {
        Log(ast->location, "unknown type in: ", functionId.ast.importedModule->name(), ':', ast->name);
        return false;
    }

    return codeGenerator->generateFunction(ast, std::move(types));
}

bool generateTypeFunctions(std::vector<CompiledModule>& result, type::TypeInstPtr origType)
{
    auto m             = origType->getModule();
    auto codeGenerator = getCodeGenerator(result, m);
    auto moduleType    = codeGenerator->getTypeContext()->getType(origType->identifier());
    bool releaseOk     = moduleType->isRefType() ? codeGenerator->generateReleaseFunctionBody(moduleType) : true;
    auto deleteOk      = codeGenerator->generateDeleteFunctionBody(moduleType);
    return releaseOk && deleteOk;
}

} // namespace

std::vector<CompiledModule> compile(const std::vector<Source>& sourceFiles)
{
    auto             targetTriple = llvm::sys::getDefaultTargetTriple(); // 32 bit "i386-pc-windows-msvc"
    llvm::DataLayout dataLayout("");
    if (!createDataLayout(targetTriple, dataLayout))
    {
        throw ReturnCode::LLVMError;
    }

    GlobalContext               globalContext;
    std::vector<CompiledModule> result;

    auto lexAndParseModule = [&globalContext, &result, &targetTriple, &dataLayout](const Source& input) {
        llfp::lex::Lexer    lexer(&input);
        llfp::parse::Parser parser(&lexer);
        auto                astModule = parser.parse();
        if (astModule == nullptr)
        {
            throw ReturnCode::ParseOrLexerError;
        }

        auto sourceModule = SourceModule::create(std::move(astModule));
        if (sourceModule == nullptr)
        {
            throw ReturnCode::TypeOrCodeGenerationError;
        }

        globalContext.addModule(sourceModule.get());

        result.push_back(CompiledModule{});
        auto& unit        = result.back();
        unit.llvmContext  = std::make_unique<llvm::LLVMContext>();
        unit.llvmModule   = std::make_unique<llvm::Module>(sourceModule->name(), *unit.llvmContext);
        unit.sourceModule = std::move(sourceModule);
        unit.source       = &input;

        unit.llvmModule->setTargetTriple(targetTriple);
        unit.llvmModule->setDataLayout(dataLayout);
    };

    // Lex & Parse Input
    for (auto& input : sourceFiles)
    {
        lexAndParseModule(input);
    }

    lexAndParseModule(MathModule::getSource());

    // Resolve imports
    for (auto& unit : result)
    {
        if (!unit.sourceModule->addImportedModules(globalContext))
        {
            throw ReturnCode::TypeOrCodeGenerationError;
        }
    }

    // Build functionInstances list (Type Classes)
    for (auto& unit : result)
    {
        if (!globalContext.buildFunctionInstances(unit.sourceModule.get()))
        {
            throw ReturnCode::TypeOrCodeGenerationError;
        }
    }

    // Resolve identifiers
    for (auto& unit : result)
    {
        if (!resolveIdentifiers(*unit.sourceModule))
        {
            throw ReturnCode::TypeOrCodeGenerationError;
        }
    }

    // Generate exported functions
    Driver driver;
    for (auto& unit : result)
    {
        auto sourceModulePtr = unit.sourceModule.get();
        unit.codeGenerator   = std::make_unique<codegen::CodeGenerator>(&driver, &globalContext, sourceModulePtr, unit.llvmContext.get(), unit.llvmModule.get());
        if (!generateExportedFunctions(unit.codeGenerator.get(), sourceModulePtr))
        {
            throw ReturnCode::TypeOrCodeGenerationError;
        }
    }

    // Generate required functions
    while (!driver.empty())
    {
        auto funId = driver.pop();
        if (!generateNextFunction(result, funId))
        {
            throw ReturnCode::TypeOrCodeGenerationError;
        }
    }

    // Generate malloc/delete functions
    while (auto type = driver.popType())
    {
        if (!generateTypeFunctions(result, type))
        {
            throw ReturnCode::TypeOrCodeGenerationError;
        }
    }

    for (auto& unit : result)
    {
        unit.codeGenerator = nullptr;
    }

    return result;
}

LinkedModule link(llvm::StringRef name, const std::vector<CompiledModule>& modules)
{
    LinkedModule result;
    result.llvmContext = std::make_unique<llvm::LLVMContext>();
    result.llvmModule  = std::make_unique<llvm::Module>(name, *result.llvmContext);

    llvm::Linker            linker{ *result.llvmModule };
    llvm::SmallVector<char> buffer;
    for (auto& cm : modules)
    {
        buffer.clear();

        llvm::BitcodeWriter writer{ buffer };
        writer.writeModule(*cm.llvmModule);
        writer.writeSymtab();
        writer.writeStrtab();

        auto memBuff    = llvm::MemoryBuffer::getMemBuffer(llvm::StringRef{ buffer.begin(), buffer.size() }, "", false);
        auto linkModule = llvm::parseBitcodeFile(*memBuff, *result.llvmContext);
        if (auto error = linkModule.takeError())
        {
            llvm::errs() << llvm::toString(std::move(error));
            throw ReturnCode::LinkError;
        }
        if (linker.linkInModule(std::move(linkModule.get())))
        {
            throw ReturnCode::LinkError;
        }
    }

    return result;
}

} // namespace llfp
