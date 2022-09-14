#pragma once

#include <memory>
#include <string>
#include <unordered_map>

#pragma warning(push, 0)

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#pragma warning(pop)

#include "Ast.h"
#include "IModule.h"
#include "GlobalContext.h"
#include "Type/TypeInstance.h"


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
    bool addImportedModules(GlobalContext &globalContext);

    const std::string& name() const override;

    FunAst             getFunction(const std::string &name) override; // lookup public function
    FunDeclAst         getFunctionDecl(const std::string &name) override;
    DataAst            getType(const std::string &name) const override;

    std::string        getMangledName(const ast::Function *function, const std::vector<const type::TypeInstance*> &types) const override;
    std::string        getMangledName(const ast::Data* data) const override;
    std::string        getMangledName(const ast::Data *data, int constructorIndex) const override;
    std::string        getMangledName(const char* internalFunctionName, type::TypeInstPtr type) const override;
    std::string        getExportedName(const ast::Function *function) const override;
    bool               fullyQualifiedName(type::Identifier& identifier, const ast::TypeIdentifier& tid) const override;

    ast::Module*       getAST();

    // lookup local function or global from imported modules
    FunAst     lookupFunction(const GlobalIdentifier& identifier) override;
    FunDeclAst lookupFunctionDecl(const GlobalIdentifier& identifier) override;
    DataAst    lookupType(const GlobalIdentifier& identifier) const override;

private:

    template<class AstNode, class LocalFun, class GlobalFun>
    AstNode lookup(const GlobalIdentifier& identifier, LocalFun localLookup, GlobalFun globalLookup, llvm::StringLiteral errorMsg) const;
};

class StandardModule : public ImportedModule
{
    std::string name_;

public:

    const std::string& name() const override;
    FunAst             getFunction(const std::string &name) override;
    FunDeclAst         getFunctionDecl(const std::string &name) override;

    std::string        getMangledName(const ast::Function* function, const std::vector<const type::TypeInstance*>& types) const override;
    std::string        getMangledName(const ast::Data* data) const override;
    std::string        getMangledName(const ast::Data* data, int constructorIndex) const override;
    std::string        getExportedName(const ast::Function*function) const override;
};

} // namespace llfp
