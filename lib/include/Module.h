#pragma once

#include <memory>
#include <string>
#include <unordered_map>

#pragma warning(push, 0)

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#pragma warning(pop)

#include "Ast.h"
#include "Type.h"


namespace llfp
{

namespace codegen
{

class CodeGenerator;

} // codegen

struct FunctionIdentifier
{
    llvm::StringRef           name;
    std::vector<type::TypePtr>* types;
};

class ImportedModule
{
public:

    virtual ~ImportedModule();

    virtual const std::string&          name() const = 0;
    virtual const ast::Function*        getFunction(const std::string &name) const = 0;
    virtual const ast::DataDeclaration* getType(const std::string &name) const = 0;
    virtual std::string                 getMangledName(const ast::Function *function, const std::vector<type::TypePtr> &types) const = 0;
    virtual std::string                 getMangledName(const ast::DataDeclaration *data, const std::vector<type::TypePtr>& types) const = 0;
    virtual std::string                 getExportedName(const ast::Function *function) const = 0;

    virtual type::DataAst lookupType(const GlobalIdentifier &) const
    {
        return { nullptr, nullptr };
    }

    //TODO: thread safe!
    virtual void requireFunctionInstance(FunctionIdentifier function) = 0;
};

// a module generted from source code
class SourceModule : public ImportedModule
{
    std::string                             path;
    std::unique_ptr<ast::Module>            astModule;
    std::unique_ptr<codegen::CodeGenerator> codeGenerator; // should be its own thing?

    std::unordered_map<std::string, ast::Function*>        functions;
    std::unordered_map<std::string, ast::Function*>        publicFunctions;
    std::unordered_map<std::string, ast::DataDeclaration*> dataDeclarations;
    std::unordered_map<std::string, ImportedModule*>       importedModules;
    std::unordered_map<std::string, ImportedModule*>       allModules;

    std::vector<FunctionIdentifier>         pendingGeneration; // Driver

public:

    SourceModule(std::string path_);
    ~SourceModule();

    bool setAST(std::unique_ptr<ast::Module> astModule_);
    bool addImportedModules(const std::vector<ImportedModule*> &moduleList);
    void createCodeGenerator();

    const std::string&          filePath() const;
    const std::string&          name() const override;
    const ast::Function*        getFunction(const std::string &name) const override; // lookup public function
    const ast::DataDeclaration* getType(const std::string &name) const override;
    std::string                 getMangledName(const ast::Function *function, const std::vector<type::TypePtr> &types) const override;
    std::string                 getMangledName(const ast::DataDeclaration *data, const std::vector<type::TypePtr>& types) const override;
    std::string                 getExportedName(const ast::Function *function) const override;

    ast::Module*                getAST();
    llvm::Module*               getLLVM();

    // lookup local function or global from imported modules
    type::FunAst  lookupFunction(const GlobalIdentifier& identifier);
    type::DataAst lookupType(const GlobalIdentifier& identifier) const override;
    type::DataAst lookupTypeGlobal(const GlobalIdentifier& identifier) const;

    void requireFunctionInstance(FunctionIdentifier function) override;

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

    const std::string&   name() const override;
    const ast::Function* getFunction(const std::string &name) const override;
    std::string          getMangledName(const ast::Function*function, const std::vector<type::TypePtr> &types) const override;
    std::string          getExportedName(const ast::Function*function) const override;

    // only type check
    void requireFunctionInstance(FunctionIdentifier function) override;
};

} // llfp
