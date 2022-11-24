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

namespace std
{

template<>
struct hash<llfp::type::Identifier>
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

class TypeInstance;
typedef const TypeInstance*      TypeInstPtr;
typedef std::vector<TypeInstPtr> FieldList;

struct TypeConstructor
{
    // name?
    // ast?
    FieldList         fields;
    llvm::StructType* llvmType_;
};

typedef std::vector<TypeConstructor> ConstructorList;


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
    virtual std::shared_ptr<hm::TypeConstant> getType() const;
    const Identifier&                         identifier() const;
    virtual const ImportedModule*             getModule() const;

    virtual llvm::Type* llvmType() const         = 0;
    virtual bool        isStructType() const     = 0; // rename basic?
    virtual bool        isRefType() const        = 0;
    virtual bool        containsRefTypes() const = 0; // or bool canValueCopy() const;
    virtual TypeInstPtr getTypeParameter(size_t index) const;

    virtual llvm::TypeSize getSize(const llvm::Module* llvmModule, size_t constructorIndex) const = 0;

    bool isNum() const;
    bool isInteger() const;
    bool isBool() const;
    bool isFloating() const;
    bool isSigned() const;

    // virtual unsigned int constructorCount() const;
    // const std::string& getConstructor(unsigned int index) const;
    virtual const ConstructorList& getConstructors() const;

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
    bool           isStructType() const override;
    bool           isRefType() const override;
    bool           containsRefTypes() const override;
    llvm::TypeSize getSize(const llvm::Module* llvmModule, size_t constructorIndex) const override;
};


class TypeInstanceStruct : public TypeInstance
{
    llvm::StructType*        llvmType_;
    const ImportedModule*    module;
    const ast::Data*         ast;
    FieldList                fields;
    std::vector<TypeInstPtr> parameters;

public:

    TypeInstanceStruct(Identifier identifier, const ImportedModule* module, const ast::Data* ast, llvm::StructType* llvmType, std::vector<std::string> typeClasses);
    virtual ~TypeInstanceStruct() = default;

    std::shared_ptr<hm::TypeConstant> getType() const override;
    const ImportedModule*             getModule() const override;

    llvm::Type* llvmType() const override;
    bool        isStructType() const override;
    bool        isRefType() const override;
    bool        containsRefTypes() const override;
    TypeInstPtr getTypeParameter(size_t index) const override;

    llvm::TypeSize getSize(const llvm::Module* llvmModule, size_t constructorIndex) const override;

    unsigned int     getFieldIndex(const std::string& fieldIdentifier) const override;
    unsigned int     getFieldIndex(const std::string& constructor, const std::string& fieldIdentifier) const override;
    const FieldList& getFields() const override;
    const FieldList& getFields(const std::string& constructor) const override;

    void setFields(FieldList fieldTypes);
};


class TypeInstanceVariant : public TypeInstance
{
    llvm::PointerType*       llvmType_; // if using opaque pointers, no need to store type
    const ImportedModule*    module;
    const ast::Data*         ast;
    ConstructorList          constructors;
    std::vector<TypeInstPtr> parameters;

public:

    TypeInstanceVariant(
        Identifier               identifier,
        llvm::PointerType*       llvmType,
        const ImportedModule*    module,
        const ast::Data*         ast,
        std::vector<std::string> typeClasses);
    virtual ~TypeInstanceVariant() = default;

    std::shared_ptr<hm::TypeConstant> getType() const override;
    const ImportedModule*             getModule() const override;

    llvm::Type* llvmType() const override;
    bool        isStructType() const override;
    bool        isRefType() const override;
    bool        containsRefTypes() const override;
    TypeInstPtr getTypeParameter(size_t index) const override;

    llvm::TypeSize getSize(const llvm::Module* llvmModule, size_t constructorIndex) const override;

    const ConstructorList& getConstructors() const override;

    unsigned int     getFieldIndex(const std::string& fieldIdentifier) const override;
    unsigned int     getFieldIndex(const std::string& constructor, const std::string& fieldIdentifier) const override;
    const FieldList& getFields() const override;
    const FieldList& getFields(const std::string& constructor) const override;

    void setConstructors(ConstructorList constructors);
};

} // namespace type
} // namespace llfp
