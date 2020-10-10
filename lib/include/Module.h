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
    std::vector<type::Type*>* types;
};

class ImportedModule
{
public:

    virtual ~ImportedModule();

    virtual const std::string&              name() const = 0;
    virtual const ast::FunctionDeclaration* getFunction(const std::string &name) const = 0;
    virtual std::string                     getMangledName(const ast::FunctionDeclaration *function, const std::vector<type::Type*> &types) const = 0;
    virtual std::string                     getExportedName(const ast::FunctionDeclaration *function) const = 0;
    //virtual Type* getType(std::string name) = 0;

    //TODO: thread safe!
    virtual void requireFunctionInstance(FunctionIdentifier function) = 0;
};

// a module generted from source code
class SourceModule : public ImportedModule
{
    std::string                             path;
    std::unique_ptr<ast::Module>            astModule;
    std::unique_ptr<codegen::CodeGenerator> codeGenerator; // should be its own thing?

    std::unordered_map<std::string, ast::FunctionDeclaration*> functions;
    std::unordered_map<std::string, ast::FunctionDeclaration*> publicFunctions;
    std::unordered_map<std::string, ImportedModule*>           importedModules;

    std::vector<FunctionIdentifier>         pendingGeneration; // Driver

public:

    SourceModule(std::string path_);
    ~SourceModule();

    bool setAST(std::unique_ptr<ast::Module> astModule_);
    bool addImportedModules(const std::vector<ImportedModule*> &moduleList);
    void createCodeGenerator();

    const std::string&              filePath() const;
    const std::string&              name() const override;
    const ast::FunctionDeclaration* getFunction(const std::string &name) const override; // lookup public function
    std::string                     getMangledName(const ast::FunctionDeclaration *function, const std::vector<type::Type*> &types) const override;
    std::string                     getExportedName(const ast::FunctionDeclaration *function) const override;
    //void                            getType(const std::string &typeName) const;

    ast::Module*                    getAST();
    llvm::Module*                   getLLVM();

    // lookup local function or global from imported modules
    bool lookupFunction(llvm::StringRef moduleIdentifier, llvm::StringRef identifier, ImportedModule*& module, const ast::FunctionDeclaration*& ast);

    void requireFunctionInstance(FunctionIdentifier function) override;

    // Driver
    bool generateTypes();
    bool generateExportedFunctions();
    bool generateNextFunction();
};

class StandardModule : public ImportedModule
{
    std::string name_;

public:

    const std::string&              name() const override;
    const ast::FunctionDeclaration* getFunction(const std::string &name) const override;
    std::string                     getMangledName(const ast::FunctionDeclaration *function, const std::vector<type::Type*> &types) const override;
    std::string                     getExportedName(const ast::FunctionDeclaration *function) const override;

    // only type check
    void requireFunctionInstance(FunctionIdentifier function) override;
};

} // llfp
