#pragma once

#include "Ast.h"

#pragma warning(push, 0)

#include <llvm/ADT/ArrayRef.h>

#pragma warning(pop)

#include <map>
#include <memory>
#include <string>


namespace llfp
{

// Forward Declarations

class ImportedModule;

namespace type
{

class TypeInstance;
struct Identifier;

} // namespace type

// Structs

struct FunAst
{
    ImportedModule*      importedModule;
    const ast::Function* function;

    bool empty() const { return importedModule == nullptr && function == nullptr; }
};

struct FunDeclAst
{
    ImportedModule*                 importedModule;
    const ast::Class*               class_;
    const ast::FunctionDeclaration* function;

    bool empty() const { return importedModule == nullptr && class_ == nullptr && function == nullptr; }
};

struct DataAst
{
    const ImportedModule* importedModule;
    const ast::Data*      data;

    bool empty() const { return importedModule == nullptr && data == nullptr; }
};

// Module Interface

class ImportedModule
{
public:

    virtual ~ImportedModule();

    virtual const std::string& name() const = 0;

    virtual ast::Module* getAST() = 0;

    // Get public
    virtual FunAst     getFunction(const std::string& name)          = 0;
    virtual FunDeclAst getFunctionDecl(const std::string& name)      = 0;
    virtual DataAst    getType(const std::string& name) const        = 0;
    virtual DataAst    getConstructor(const std::string& name) const = 0;

    virtual std::string getMangledName(const ast::Function* function, const llvm::ArrayRef<const type::TypeInstance*> types) const                              = 0;
    virtual std::string getMangledName(const ast::Data* data, const std::map<std::string, type::Identifier>& typeVariables = {}) const                          = 0;
    virtual std::string getMangledName(const ast::Data* data, size_t constructorIndex, const std::map<std::string, type::Identifier>& typeVariables = {}) const = 0;
    virtual std::string getMangledName(const char* internalName, const type::TypeInstance* type) const                                                          = 0;
    virtual std::string getExportedName(const ast::Function* function) const                                                                                    = 0;
    virtual bool        fullyQualifiedName(type::Identifier& identifier, const ast::TypeIdentifier& tid) const                                                  = 0;

    // Lookup global
    virtual FunAst     lookupFunction(const GlobalIdentifier& identifier)          = 0;
    virtual FunDeclAst lookupFunctionDecl(const GlobalIdentifier& identifier)      = 0;
    virtual DataAst    lookupType(const GlobalIdentifier&) const                   = 0;
    virtual DataAst    lookupConstructor(const GlobalIdentifier& identifier) const = 0;
};

} // namespace llfp
