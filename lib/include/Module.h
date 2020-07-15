#pragma once

#include <memory>
#include <string>
#include <unordered_map>

#pragma warning(push, 0)

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#pragma warning(pop)

#include "Ast.h"


namespace llfp
{

class ImportedModule
{
public:

    virtual ~ImportedModule();

    virtual const std::string&              name() const = 0;
    virtual const ast::FunctionDeclaration* getFunction(const std::string &name) const = 0;
    virtual std::string                     getFullFunctionName(const ast::FunctionDeclaration *function) const = 0;
};

// a module generted from source code
class SourceModule : public ImportedModule 
{
    llvm::LLVMContext             llvmContext; // one context per module, one module compiled per thread
    std::string                   path;
    std::unique_ptr<ast::Module>  astModule;
    std::unordered_map<std::string, ast::FunctionDeclaration*> functions;
    std::unordered_map<std::string, const ImportedModule*> importedModules;
    std::unique_ptr<llvm::Module> llvmModule;

public:

    SourceModule(std::string path_);
    ~SourceModule();

    bool setAST(std::unique_ptr<ast::Module> module);
    bool addImportedModules(const std::vector<const ImportedModule*> &moduleList);
    void setLLVM(std::unique_ptr<llvm::Module> module);

    llvm::LLVMContext&              context();
    const std::string&              filePath() const;
    const std::string&              name() const override;
    const ast::FunctionDeclaration* getFunction(const std::string &name) const override;
    std::string                     getFullFunctionName(const ast::FunctionDeclaration *function) const override;
    std::string                     getFullFunctionName(llvm::StringRef functionName) const;
    //void                            getType(const std::string &typeName) const;
    const std::unordered_map<std::string, const ImportedModule*>& getImportedModules() const;
    ast::Module*                    getAST();
    llvm::Module*                   getLLVM();
};

class StandardModule : public ImportedModule
{
    std::string name_;

public:

    const std::string&              name() const override;
    const ast::FunctionDeclaration* getFunction(const std::string &name) const override;
    std::string                     getFullFunctionName(const ast::FunctionDeclaration *function) const override;
};

} // llfp
