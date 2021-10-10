
#pragma warning(push, 0)

#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/Host.h"

#include "llvm/Target/TargetOptions.h"
#include "llvm/Target/TargetMachine.h"

#pragma warning(pop)

#include "Codegen.h"
#include "Lexer.h"
#include "Log.h"
#include "Module.h"
#include "Parser.h"

#include "Compiler.h"

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
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, targetError);

    if (target == nullptr)
    {
        llvm::errs() << targetError << "\n";
        return false;
    }

    auto CPU = "generic";
    auto Features = "";

    llvm::TargetOptions opt;
    auto RM = llvm::Optional<llvm::Reloc::Model>();
    auto targetMachine = target->createTargetMachine(targetTriple, CPU, Features, opt, RM);

    if (targetMachine == nullptr)
    {
        llvm::errs() << "Failed to create target machine\n";
        return false;
    }

    dataLayout = targetMachine->createDataLayout();

    return true;
}

bool generateExportedFunctions(codegen::CodeGenerator* codeGenerator, SourceModule *sourceModule)
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

bool buildFunctionInstances(SourceModule *sourceModule, std::unordered_map<std::string, std::unordered_map<type::Identifier, FunAst>> &functionInstances)
{
    bool result = true;
    auto ast = sourceModule->getAST();
    
    for (auto& instance : ast->classInstances)
    {
        // check that instance->classIdentifier is visible from this module

        type::Identifier id;
        if (sourceModule->fullyQualifiedName(id, instance->typeArgument))
        {
            for (auto& function : instance->functions)
            {
                auto predicate = [&function](std::unique_ptr<ast::Function>& f) {return f->name == function->name; };
                if (std::find_if(ast->functions.begin(), ast->functions.end(), predicate) != ast->functions.end())
                {
                    Log(function->location, "function already defined");
                    result = false;
                }
                else
                {
                    auto& map = functionInstances[function->name];
                    auto it = map.insert({ std::move(id), FunAst{ sourceModule, function.get()} });
                    if (!it.second)
                    {
                        auto& typeKey = it.first->first;
                        Log(function->location, "multiple instances of class for '", typeKey.str(), '\'');
                        result = false;
                    }
                }
            }
        }
        else
        {
            Log(instance->location, "unknown type in class instance");
            result = false;
            // we should also check class identifier...
        }
    }

    return result;
}

} // namespace

Compiler::ErrorCode Compiler::compile(const std::vector<std::unique_ptr<lex::Input>> &sourceFiles)
{
    auto targetTriple = llvm::sys::getDefaultTargetTriple(); // 32 bit "i386-pc-windows-msvc"
    llvm::DataLayout dataLayout("");
    if (!createDataLayout(targetTriple, dataLayout))
    {
        return Compiler::LLVMError;
    }

    // Lex & Parse Input
    for (auto &input : sourceFiles)
    {
        llfp::lex::Lexer lexer(input.get());
        llfp::parse::Parser parser(&lexer);
        auto astModule = parser.parse();
        if (astModule == nullptr)
        {
            return ParseOrLexerError;
        }

        auto sourceModule = SourceModule::create(this, std::move(astModule));
        if (sourceModule == nullptr)
        {
            return TypeOrCodeGenerationError;
        }

        auto llvmModule = allModules.insert({sourceModule->name(), sourceModule.get()});

        compileModules.push_back(Unit{});
        auto& unit = compileModules.back();
        unit.llvmContext = std::make_unique<llvm::LLVMContext>();
        unit.llvmModule = std::make_unique<llvm::Module>(sourceModule->name(), *unit.llvmContext);
        unit.sourceModule = std::move(sourceModule);

        unit.llvmModule->setTargetTriple(targetTriple);
        unit.llvmModule->setDataLayout(dataLayout);
    }

    // Resolve imports
    for (auto &unit : compileModules)
    {
        if (!unit.sourceModule->addImportedModules(allModules))
        {
            return TypeOrCodeGenerationError;
        }
    }

    // Build functionInstances list (Type Classes)
    for (auto& unit : compileModules)
    {
        if (!buildFunctionInstances(unit.sourceModule.get(), functionInstances))
        {
            return TypeOrCodeGenerationError;
        }
    }

    // Genereate exported functions
    for (auto& unit : compileModules)
    {
        auto sourceModulePtr = unit.sourceModule.get();
        unit.codeGenerator = std::make_unique<codegen::CodeGenerator>(sourceModulePtr, unit.llvmContext.get(), unit.llvmModule.get());
        if (!generateExportedFunctions(unit.codeGenerator.get(), sourceModulePtr))
        {
            return TypeOrCodeGenerationError;
        }
    }

    // Generate required functions
    while (!pendingGeneration.empty())
    {
        if (!generateNextFunction())
        {
            return TypeOrCodeGenerationError;
        }
    }

    return NoError;
}

FunAst Compiler::lookupInstance(const std::string& funIdentifier, const type::Identifier& typeIdentifier)
{
    auto it = functionInstances.find(funIdentifier);
    if (it != functionInstances.end())
    {
        auto it2 = it->second.find(typeIdentifier);
        if (it2 != it->second.end())
        {
            return it2->second;
        }
    }
    return {nullptr, nullptr};
}

DataAst Compiler::lookupTypeGlobal(const GlobalIdentifier& identifier) const
{
    assert(!identifier.moduleName.empty());
    assert(!identifier.name.empty());

    auto it = allModules.find(identifier.moduleName);
    if (it != allModules.end())
    {
        auto astType = it->second->getType(identifier.name);
        if (!astType.empty())
        {
            return astType;
        }
    }

    return { nullptr, nullptr };
}

void Compiler::requireFunctionInstance(FunctionIdentifier function)
{
    pendingGeneration.push_back(function);
}

SourceModule* Compiler::getModule(size_t index)
{
    return compileModules.at(index).sourceModule.get();
}

llvm::Module* Compiler::getLlvmModule(size_t index)
{
    return compileModules.at(index).llvmModule.get();
}

bool Compiler::generateNextFunction()
{
    assert(!pendingGeneration.empty());
    
    auto functionId = pendingGeneration.back();
    auto ast = functionId.ast.function;
    auto m = functionId.ast.importedModule;

    auto predicate = [m](const Unit &u) { return static_cast<ImportedModule*>(u.sourceModule.get()) == m; };
    auto it = std::find_if(compileModules.begin(), compileModules.end(), predicate);
    if (it == compileModules.end())
    {
        // standard module?
        return false;
    }
    auto codeGenerator = it->codeGenerator.get();

    std::vector<type::TypePtr> types;
    for (auto t : *functionId.types)
    {
        //TODO: Now we try to find type in this module with its imports
        // but this might be called from another module with its own type...
        // an import in this module should not be required
        types.push_back(codeGenerator->getTypeContext()->getType(t->identifier()));
    }

    if (std::any_of(types.begin(), types.end(), [](auto x) { return x == nullptr; }))
    {
        Log(ast->location, "unknown type in: ", functionId.ast.importedModule->name(), ':', ast->name);
        pendingGeneration.pop_back();
        return false;
    }

    bool result = codeGenerator->generateFunction(ast, std::move(types));
    pendingGeneration.pop_back();
    return result;
}

} // namespace llfp
