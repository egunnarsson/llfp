#pragma once

#include "Ast.h"
#include "GlobalContext.h"
#include "IModule.h"
#include "Type/TypeInstance.h"

#pragma warning(push, 0)

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#pragma warning(pop)

#include <memory>
#include <string>
#include <unordered_map>


namespace llfp
{

// a module generted from source code
class SourceModule : public ImportedModule
{
    std::unique_ptr<ast::Module> astModule;

    std::unordered_map<std::string, ast::Function*>  functions;
    std::unordered_map<std::string, ast::Function*>  publicFunctions;
    std::unordered_map<std::string, FunDeclAst>      functionDeclarations;
    std::unordered_map<std::string, ast::Data*>      dataDeclarations;
    std::unordered_map<std::string, ImportedModule*> importedModules;

public:

    static std::unique_ptr<SourceModule> create(std::unique_ptr<ast::Module> astModule_);
    bool                                 addImportedModules(GlobalContext& globalContext);

    const std::string& name() const override;
    ast::Module*       getAST() override;

    FunAst     getFunction(const std::string& name) override; // lookup public function
    FunDeclAst getFunctionDecl(const std::string& name) override;
    DataAst    getType(const std::string& name) const override;
    DataAst    getConstructor(const std::string& name) const override;

    // lookup local function or global from imported modules
    FunAst     lookupFunction(const GlobalIdentifier& identifier) override;
    FunDeclAst lookupFunctionDecl(const GlobalIdentifier& identifier) override;
    DataAst    lookupType(const GlobalIdentifier& identifier) const override;
    DataAst    lookupConstructor(const GlobalIdentifier& identifier) const override;

private:

    template<class AstNode, class LocalFun, class GlobalFun>
    AstNode lookup(const GlobalIdentifier& identifier, LocalFun localLookup, GlobalFun globalLookup, llvm::StringLiteral errorMsg) const;
};

} // namespace llfp
