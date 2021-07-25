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
#include "Type.h"


namespace llfp
{

class Compiler;

namespace codegen
{

class CodeGenerator;

} // codegen

// a module generted from source code
class SourceModule : public ImportedModule
{
    // for global lookup? others to be able to call getParent... to act like context
    Compiler*                               parent;

    std::string                             path;
    std::unique_ptr<ast::Module>            astModule;
    //std::unique_ptr<codegen::CodeGenerator> codeGenerator; // should be its own thing?

    //TODO: move all these lookup stuff to some helper
    std::unordered_map<std::string, ast::Function*>        functions;
    std::unordered_map<std::string, ast::Function*>        publicFunctions;
    std::unordered_map<std::string, std::tuple<ast::Class*, ast::FunctionDeclaration*>> functionDeclarations;
    std::unordered_map<std::string, std::unordered_map<type::Identifier, FunAst>> functionInstances; // we only do global lookups in this, so it shouldnt be in module
    std::unordered_map<std::string, ast::Data*>            dataDeclarations;
    std::unordered_map<std::string, ImportedModule*>       importedModules;

public:

    SourceModule(Compiler* parent_, std::string path_);
    ~SourceModule();

    std::unique_ptr<SourceModule> create(std::string path_, std::unique_ptr<ast::Module> astModule_);

    bool setAST(std::unique_ptr<ast::Module> astModule_);
    bool addImportedModules(const std::vector<ImportedModule*> &moduleList);
    //void createCodeGenerator();

    const std::string&          filePath() const;
    const std::string&          name() const override;

    FunAst     getFunction(const std::string &name) override; // lookup public function
    FunDeclAst getFunctionDecl(const std::string &name) override;
    DataAst    getType(const std::string &name) const override;

    std::string                 getMangledName(const ast::Function *function, const std::vector<type::TypePtr> &types) const override;
    std::string                 getMangledName(const ast::Data *data, const std::vector<type::TypePtr>& types) const override;
    std::string                 getExportedName(const ast::Function *function) const override;
    bool                        fullyQualifiedName(type::Identifier& identifier, const ast::TypeIdentifier& tid) const override;

    Compiler*                   getParent();
    ast::Module*                getAST();
    //llvm::Module*               getLLVM();

    // lookup local function or global from imported modules
    FunAst     lookupFunction(const GlobalIdentifier& identifier) override;
    FunDeclAst lookupFunctionDecl(const GlobalIdentifier& identifier) override;
    DataAst    lookupType(const GlobalIdentifier& identifier) const override;

    // Driver
    bool generateExportedFunctions();
    bool generateNextFunction();

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

    std::string        getMangledName(const ast::Function*function, const std::vector<type::TypePtr> &types) const override;
    std::string        getExportedName(const ast::Function*function) const override;
};

} // llfp
