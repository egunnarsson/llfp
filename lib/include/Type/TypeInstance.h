#pragma once

#include <algorithm>
#include <functional>
#include <memory>
#include <string>
#include <vector>

#pragma warning(push, 0)

#include "llvm/ADT/Hashing.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"

#pragma warning(pop)

#include "Ast.h"
#include "Common/GlobalIdentifier.h"
#include "Type/TypeInference.h"


namespace llfp
{
namespace type
{

struct Identifier
{
    GlobalIdentifier        name; // baseName?
    std::vector<Identifier> parameters;

    std::string str() const;

    bool operator ==(const Identifier& id) const
    {
        return name == id.name && parameters == id.parameters;
    }

    bool operator !=(const Identifier& id) const
    {
        return name != id.name || parameters != id.parameters;
    }
};

} // namespace type
} // namespace llfp

namespace std
{
template<> struct hash<llfp::type::Identifier>
{
    std::size_t operator()(llfp::type::Identifier const& id) const noexcept
    {
        std::vector<llvm::hash_code> tmp(id.parameters.size(), 0);
        std::transform(id.parameters.begin(), id.parameters.end(), tmp.begin(),
            [](llfp::type::Identifier const& id) { return hash<llfp::type::Identifier>{}(id); });

        return llvm::hash_combine(
            std::hash<llfp::GlobalIdentifier>{}(id.name),
            llvm::hash_combine_range(tmp.begin(), tmp.end()));
    }
};
} // namespace std

namespace llfp
{

class ImportedModule;

namespace type
{


bool checkBasicType(const Identifier& id, llvm::StringRef name);
bool isPrimitive(const ast::TypeIdentifier& id);
bool isPrimitive(const Identifier& id);


class TypeInstance
{
    //llfp::hm::TypeConstantPtr  hmType; // maybe instead of creating new ones?
    Identifier                 identifier_;
    std::vector<std::string>   typeClasses;

protected:

    TypeInstance(Identifier identifier_, std::vector<std::string> typeClasses);

public:

    static constexpr auto InvalidIndex = (unsigned int)-1;

    TypeInstance(const TypeInstance&) = delete;
    virtual ~TypeInstance() = default;

    // creates a new hm type
    virtual std::shared_ptr<hm::TypeConstant> getType() const;
    const Identifier& identifier() const;

    virtual llvm::Type* llvmType() const = 0;
    virtual bool isStructType() const = 0;
    virtual const TypeInstance* getTypeParameter(int index) const;

    bool isNum() const;
    bool isInteger() const;
    bool isBool() const;
    bool isFloating() const;
    bool isSigned() const;

    virtual unsigned int getFieldIndex(const std::string& fieldIdentifier) const;
    virtual const TypeInstance* getFieldType(unsigned int index) const;
    virtual unsigned int  getFieldCount() const;
};

typedef const TypeInstance* TypeInstPtr;

class TypeInstanceBasic : public TypeInstance
{
    llvm::Type* llvmType_;

public:

    TypeInstanceBasic(llvm::StringLiteral name, llvm::Type* llvmType, std::vector<std::string> typeClasses);
    virtual ~TypeInstanceBasic() = default;

    llvm::Type* llvmType() const override;
    bool isStructType() const override;
};

class TypeInstanceStruct : public TypeInstance
{
    llvm::StructType*                llvmType_;
    const ImportedModule*            module;
    const ast::Data*                 ast;
    std::vector<const TypeInstance*> fields;
    std::vector<const TypeInstance*> parameters;

public:

    TypeInstanceStruct(Identifier identifier, const ImportedModule* module, const ast::Data* ast, llvm::StructType* llvmType, std::vector<std::string> typeClasses);
    virtual ~TypeInstanceStruct() = default;

    std::shared_ptr<hm::TypeConstant> getType() const override;

    llvm::Type* llvmType() const override;
    bool isStructType() const override;

    unsigned int getFieldIndex(const std::string& fieldIdentifier) const override;
    const TypeInstance* getFieldType(unsigned int index) const override;
    unsigned int  getFieldCount() const override;

    void setFields(std::vector<const TypeInstance*> fieldTypes);
};

} // namespace type
} // namespace llfp
