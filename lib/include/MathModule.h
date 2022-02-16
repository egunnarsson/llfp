
#include <memory>
#include <string>
#include <unordered_map>

#include "IModule.h"


namespace llfp
{

class GlobalContext;

class MathModule : public ImportedModule
{
    std::unique_ptr<ast::Module> astModule;

public:

    MathModule();

    void addToGlobalContext(GlobalContext& context);

    virtual const std::string& name() const;

    // Get public
    virtual FunAst      getFunction(const std::string& name);
    virtual FunDeclAst  getFunctionDecl(const std::string& name);
    virtual DataAst     getType(const std::string& name) const;

    virtual std::string getMangledName(const ast::Function* function, const std::vector<const type::TypeInstance*>& types) const;
    virtual std::string getMangledName(const ast::Data* data, const std::vector<const type::TypeInstance*>& types) const;
    virtual std::string getExportedName(const ast::Function* function) const;
    virtual bool        fullyQualifiedName(type::Identifier& identifier, const ast::TypeIdentifier& tid) const;

    // Lookup global
    virtual FunAst      lookupFunction(const GlobalIdentifier& identifier);
    virtual FunDeclAst  lookupFunctionDecl(const GlobalIdentifier& identifier);
    virtual DataAst     lookupType(const GlobalIdentifier&) const;
};

} // namespace llfp
