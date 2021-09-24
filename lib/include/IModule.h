#pragma once

#include <memory>
#include <string>

#include "Ast.h"


namespace llfp
{

class ImportedModule;

namespace type
{

class Type;
typedef std::shared_ptr<Type> TypePtr;

struct Identifier;

}

struct FunAst
{
    ImportedModule* importedModule;
    const ast::Function* function;

    bool empty() const { return importedModule == nullptr && function == nullptr; }
};

struct FunDeclAst
{
    ImportedModule* importedModule;
    const ast::Class* class_;
    const ast::FunctionDeclaration* function;

    bool empty() const { return importedModule == nullptr && class_ == nullptr && function == nullptr; }
};

struct DataAst
{
    const ImportedModule* importedModule;
    const ast::Data* data;

    bool empty() const { return importedModule == nullptr && data == nullptr; }
};

/*struct FunctionIdentifier
{
    llvm::StringRef           name;
    std::vector<type::TypePtr>* types;
};*/

class ImportedModule
{
public:

    virtual ~ImportedModule();

    virtual const std::string& name() const = 0;

    // Get public
    virtual FunAst      getFunction(const std::string &name) = 0;
    virtual FunDeclAst  getFunctionDecl(const std::string &name) = 0;
    virtual DataAst     getType(const std::string &name) const = 0;

    virtual std::string getMangledName(const ast::Function *function, const std::vector<type::TypePtr> &types) const = 0;
    virtual std::string getMangledName(const ast::Data *data, const std::vector<type::TypePtr>& types) const = 0;
    virtual std::string getExportedName(const ast::Function *function) const = 0;
    virtual bool        fullyQualifiedName(type::Identifier& identifier, const ast::TypeIdentifier& tid) const = 0;

    // Lookup global
    virtual FunAst      lookupFunction(const GlobalIdentifier& identifier) = 0;
    virtual FunDeclAst  lookupFunctionDecl(const GlobalIdentifier& identifier) = 0;
    virtual DataAst     lookupType(const GlobalIdentifier &) const = 0;

    //virtual void        requireFunctionInstance(FunctionIdentifier function) = 0;
    //virtual bool        generateFunction(const ast::Function *ast, std::vector<const type::Identifier*> types) = 0; 
};

} // llfp
