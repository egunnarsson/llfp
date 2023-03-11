#pragma once

#include "Ast.h"
#include "Common/GlobalIdentifier.h"
#include "Type/TypeInference.h"

#pragma warning(push, 0)

#include <llvm/ADT/Hashing.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#pragma warning(pop)

#include <algorithm>
#include <functional>
#include <memory>
#include <string>
#include <vector>


namespace llvm
{

class Module;

} // namespace llvm

namespace llfp::type
{

struct Identifier
{
    GlobalIdentifier        name; // baseName?
    std::vector<Identifier> parameters;

    std::string str() const;

    bool operator==(const Identifier& id) const
    {
        return name == id.name && parameters == id.parameters;
    }

    bool operator!=(const Identifier& id) const
    {
        return name != id.name || parameters != id.parameters;
    }
};

} // namespace llfp::type

template<>
struct std::hash<llfp::type::Identifier>
{
    std::size_t operator()(llfp::type::Identifier const& id) const noexcept
    {
        std::vector<llvm::hash_code> tmp(id.parameters.size(), 0);
        std::transform(id.parameters.begin(), id.parameters.end(), tmp.begin(),
                       [](llfp::type::Identifier const& id) { return std::hash<llfp::type::Identifier>{}(id); });

        return llvm::hash_combine(
            std::hash<llfp::GlobalIdentifier>{}(id.name),
            llvm::hash_combine_range(tmp.begin(), tmp.end()));
    }
};

namespace llfp
{

class ImportedModule;

namespace type
{

class TypeInstance;
typedef const TypeInstance*      TypeInstPtr;
typedef std::vector<TypeInstPtr> FieldList;

struct TypeConstructor
{
    const ast::DataConstructor* ast;
    FieldList                   fields;
    llvm::StructType*           llvmType_;
};

typedef llvm::ArrayRef<const TypeConstructor> ConstructorList;


bool checkBasicType(const Identifier& id, llvm::StringRef name);
bool isPrimitive(const ast::TypeIdentifier& id);
bool isPrimitive(const Identifier& id);


class TypeInstance
{
    // llfp::hm::TypeConstantPtr  hmType; // maybe instead of creating new ones?
    Identifier               identifier_;
    std::vector<std::string> typeClasses;

protected:

    TypeInstance(Identifier identifier_, std::vector<std::string> typeClasses);

public:

    static constexpr auto InvalidIndex = (unsigned int)-1;

    TypeInstance(const TypeInstance&) = delete;
    virtual ~TypeInstance()           = default;

    // creates a new hm type
    std::shared_ptr<hm::TypeConstant>         getType() const;
    virtual std::shared_ptr<hm::TypeConstant> getType(std::map<const TypeInstance*, std::shared_ptr<hm::TypeConstant>>& types) const;
    const Identifier&                         identifier() const;
    virtual const ImportedModule*             getModule() const;

    // Type used to store this type in another aggregate or on the stack
    virtual llvm::Type* llvmType() const = 0;
    // virtual llvm::Type* llvmTypeValue() const = 0; // actual? constructor.llvmType?
    // virtual llvm::Type* llvmTypeField() const = 0; // or Stack
    // virtual llvm::Type* llvmTypeHeap() const = 0;

    virtual bool        isBasicType() const      = 0;
    virtual bool        isRefType() const        = 0;
    virtual bool        containsRefTypes() const = 0; // or bool canValueCopy() const; bool isMemCopiable() const;
    virtual TypeInstPtr getTypeParameter(size_t index) const;

    virtual llvm::TypeSize getSize(const llvm::Module* llvmModule, size_t constructorIndex) const = 0;

    bool isNum() const;
    bool isInteger() const;
    bool isBool() const;
    bool isFloating() const;
    bool isSigned() const;

    virtual ConstructorList getConstructors() const;

    virtual unsigned int     getFieldIndex(const std::string& fieldIdentifier) const;
    virtual unsigned int     getFieldIndex(const std::string& constructor, const std::string& fieldIdentifier) const;
    virtual const FieldList& getFields() const;
    virtual const FieldList& getFields(const std::string& constructor) const;
};


class TypeInstanceBasic : public TypeInstance
{
    llvm::Type* llvmType_;

public:

    TypeInstanceBasic(llvm::StringLiteral name, llvm::Type* llvmType, std::vector<std::string> typeClasses);
    virtual ~TypeInstanceBasic() = default;

    llvm::Type*    llvmType() const override;
    bool           isBasicType() const override;
    bool           isRefType() const override;
    bool           containsRefTypes() const override;
    llvm::TypeSize getSize(const llvm::Module* llvmModule, size_t constructorIndex) const override;
};


class TypeInstanceAggregate : public TypeInstance
{
    TypeConstructor          constructor;
    const ast::Data*         ast;
    const ImportedModule*    module;
    std::vector<TypeInstPtr> parameters;

public:

    TypeInstanceAggregate(Identifier identifier, const ImportedModule* module, const ast::Data* ast, llvm::StructType* llvmType, std::vector<std::string> typeClasses);
    virtual ~TypeInstanceAggregate() = default;

    std::shared_ptr<hm::TypeConstant> getType(std::map<const TypeInstance*, std::shared_ptr<hm::TypeConstant>>& types) const override;
    const ImportedModule*             getModule() const override;

    llvm::Type* llvmType() const override;
    bool        isBasicType() const override;
    bool        isRefType() const override;
    bool        containsRefTypes() const override;
    TypeInstPtr getTypeParameter(size_t index) const override;

    llvm::TypeSize getSize(const llvm::Module* llvmModule, size_t constructorIndex) const override;

    ConstructorList getConstructors() const override;

    unsigned int     getFieldIndex(const std::string& fieldIdentifier) const override;
    unsigned int     getFieldIndex(const std::string& constructor, const std::string& fieldIdentifier) const override;
    const FieldList& getFields() const override;
    const FieldList& getFields(const std::string& constructor) const override;

    void setFields(FieldList fieldTypes);
};


class TypeInstanceVariant : public TypeInstance
{
    llvm::PointerType*           llvmType_; // if using opaque pointers, no need to store type
    const ImportedModule*        module;
    const ast::Data*             ast;
    std::vector<TypeConstructor> constructors;
    std::vector<TypeInstPtr>     parameters;

public:

    static llvm::IntegerType* getEnumType(llvm::LLVMContext& context, const TypeInstance* type);

    TypeInstanceVariant(
        Identifier               identifier,
        llvm::PointerType*       llvmType,
        const ImportedModule*    module,
        const ast::Data*         ast,
        std::vector<std::string> typeClasses);
    virtual ~TypeInstanceVariant() = default;

    std::shared_ptr<hm::TypeConstant> getType(std::map<const TypeInstance*, std::shared_ptr<hm::TypeConstant>>& types) const override;
    const ImportedModule*             getModule() const override;

    llvm::Type* llvmType() const override;
    bool        isBasicType() const override;
    bool        isRefType() const override;
    bool        containsRefTypes() const override;
    TypeInstPtr getTypeParameter(size_t index) const override;

    llvm::TypeSize getSize(const llvm::Module* llvmModule, size_t constructorIndex) const override;

    ConstructorList getConstructors() const override;

    unsigned int     getFieldIndex(const std::string& fieldIdentifier) const override;
    unsigned int     getFieldIndex(const std::string& constructor, const std::string& fieldIdentifier) const override;
    const FieldList& getFields() const override;
    const FieldList& getFields(const std::string& constructor) const override;

    void setConstructors(std::vector<TypeConstructor> constructors);
};

} // namespace type
} // namespace llfp
