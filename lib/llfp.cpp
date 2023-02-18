
#include "llfp.h"

#include "Driver.h"
#include "GlobalContext.h"
#include "MathModule.h"
#include "Parser.h"
#include "ResolveIdentifiers.h"

#pragma warning(push, 0)

#include <llvm/MC/TargetRegistry.h>

#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>

#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

#pragma warning(pop)


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

codegen::CodeGenerator* getCodeGenerator(std::vector<CompiledModule>& result, const ImportedModule* m)
{
    auto predicate = [m](const CompiledModule& u) { return static_cast<ImportedModule*>(u.sourceModule.get()) == m; };
    auto it        = std::find_if(result.begin(), result.end(), predicate);
    if (it == result.end())
    {
        // standard module
        // return functionId.ast.function->functionBody == nullptr;
        return nullptr;
    }
    return it->codeGenerator.get();
}

bool generateNextFunction(std::vector<CompiledModule>& result, FunctionIdentifier functionId)
{
    auto ast           = functionId.ast.function;
    auto m             = functionId.ast.importedModule;
    auto codeGenerator = getCodeGenerator(result, m);

    if (codeGenerator == nullptr)
    {
        // standard module
        return functionId.ast.function->functionBody == nullptr;
    }

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
    if (codeGenerator == nullptr)
    {
        // or standard module? Should be ok to generator memory functions for those types...
        Log({}, "generating memory functions for basic type? ", origType->identifier().str());
        return false;
    }
    auto moduleType = codeGenerator->getTypeContext()->getType(origType->identifier());
    bool releaseOk  = moduleType->isRefType() ? codeGenerator->generateReleaseFunctionBody(moduleType) : true;
    auto deleteOk   = codeGenerator->generateDeleteFunctionBody(moduleType);
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

    MathModule mathModule;
    mathModule.addToGlobalContext(globalContext);

    // Lex & Parse Input
    for (auto& input : sourceFiles)
    {
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

        unit.llvmModule->setTargetTriple(targetTriple);
        unit.llvmModule->setDataLayout(dataLayout);
    }

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

} // namespace llfp
