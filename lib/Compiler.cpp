
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

bool generateExportedFunctions(codegen::CodeGenerator* codeGenerator, SourceModule *sourceModule)
{
    for (auto& function : sourceModule->getAST()->functions)
    {
        if (function->exported)
        {
            if (codeGenerator->generateFunction(function.get()))
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
        type::Identifier id;
        if (sourceModule->fullyQualifiedName(id, instance->typeArgument))
        {
            for (auto& function : instance->functions)
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
        if (sourceModule != nullptr)
        {
            return TypeOrCodeGenerationError;
        }

        allModules.insert({sourceModule->name(), sourceModule.get()});
        compileModules.push_back(Unit{});
        compileModules.back().llvmContext = std::make_unique<llvm::LLVMContext>();
        compileModules.back().sourceModule = std::move(sourceModule);
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
    if (it == functionInstances.end())
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
