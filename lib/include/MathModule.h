
#include "IModule.h"

#include "Source.h"

#include <memory>
#include <string>
#include <unordered_map>


namespace llfp
{

class GlobalContext;

class MathModule : public ImportedModule
{
    Source                       source_;
    std::unique_ptr<ast::Module> astModule_;

public:

    MathModule();

    void addToGlobalContext(GlobalContext& context);

    const std::string& name() const override;

    // Get public
    FunAst     getFunction(const std::string& name) override;
    FunDeclAst getFunctionDecl(const std::string& name) override;
    DataAst    getType(const std::string& name) const override;
    DataAst    getConstructor(const std::string& name) const override;

    std::string getMangledName(const ast::Function* function, const llvm::ArrayRef<const type::TypeInstance*> types) const override;
    std::string getMangledName(const ast::Data* data) const override;
    std::string getMangledName(const ast::Data* data, size_t constructorIndex) const override;
    std::string getMangledName(const char* internalFunctionName, const type::TypeInstance* type) const override;
    std::string getExportedName(const ast::Function* function) const override;
    bool        fullyQualifiedName(type::Identifier& identifier, const ast::TypeIdentifier& tid) const override;

    // Lookup global
    FunAst     lookupFunction(const GlobalIdentifier& identifier) override;
    FunDeclAst lookupFunctionDecl(const GlobalIdentifier& identifier) override;
    DataAst    lookupType(const GlobalIdentifier& identifier) const override;
    DataAst    lookupConstructor(const GlobalIdentifier& identifier) const override;
};

} // namespace llfp
