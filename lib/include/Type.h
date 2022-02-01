#pragma once

/*
1. compare, X == Y
2. call infers arguments to get function instance?
3. fieldExp infer LHS before generating it
4. letExp if var has no type, infer before generating

is it ok to fix the type in these cases?
2. No. Calling with literals might not fix to the right type
*/

#include <map>
#include <memory>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>

#pragma warning(push, 0)

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"

#pragma warning(pop)

#include "Ast.h"
#include "IModule.h"
#include "Common.h"
#include "Type/Type2.h"


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
class SourceModule;
class GlobalContext;

namespace type
{

// names of built in primitive types
namespace name
{

#define TypeID(id, name) constexpr llvm::StringLiteral id{name};

TypeID(Bool, "bool");

TypeID(I8, "i8");
TypeID(I16,"i16");
TypeID(I32, "i32");
TypeID(I64, "i64");
TypeID(I128, "i128");

TypeID(U8, "u8");
TypeID(U16, "u16");
TypeID(U32, "u32");
TypeID(U64, "u64");
TypeID(U128, "u128");

TypeID(Half, "half");
TypeID(Float, "float");
TypeID(Double, "double");

TypeID(Char, "char");

#undef TypeID

constexpr llvm::StringLiteral AllTypes[] {
    Bool,
    I8, I16, I32, I64, I128,
    U8, U16, U32, U64, U128,
    Half, Float, Double,
    Char
};

} // namespace name

bool isPrimitive(const Identifier &id);

/*
namespace typeclass
{

// Num
// Integer
// Bool
// Signed
// Floating

}

enum TypeClass
{
    Num,
    Fractional,
    Eq,
    Ord,
    Bits
};
*/

class TypeClass
{
    GlobalIdentifier name;
};

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

class TypeContext
{
    llvm::LLVMContext& llvmContext; // to create types
    SourceModule*      sourceModule; // to do global lookups (actually need parent), qualify names for equals function
    GlobalContext*     globalContext;

    std::unordered_map<Identifier, std::unique_ptr<TypeInstance>> types;
    std::unordered_map<const ast::Function*, std::unique_ptr<hm::TypeAnnotation>> annotations;

    TypeInstance* boolType;
    TypeInstance* i64Type;
    TypeInstance* u64Type;
    TypeInstance* doubleType;
    TypeInstance* charType;

public:

    TypeContext(llvm::LLVMContext &llvmContext_, SourceModule *sourceModule_, GlobalContext* globalContext_);

    const hm::TypeAnnotation& getAnnotation(const ImportedModule* module, const ast::Function* ast);

    TypeInstPtr getTypeFromAst(const ast::TypeIdentifier& identifier);
    TypeInstPtr getTypeFromAst(const ast::TypeIdentifier& identifier, const ImportedModule* lookupModule);
    bool equals(TypeInstPtr type, const ast::TypeIdentifier& identifier);
    bool equals(TypeInstPtr type, llvm::StringRef identifier);

    // type check, and fix if literal etc
    bool check(hm::TypeAnnotation& context, TypeInstPtr inst, const hm::TypePtr& t);
    // a bit temporary
    TypeInstPtr constructTypeUsingAnnotationStuff(hm::TypeAnnotation& context, const ast::Exp& exp);

    TypeInstPtr getBool();
    TypeInstPtr getChar();
    TypeInstPtr getI64();
    TypeInstPtr getU64();
    TypeInstPtr getDouble();

    TypeInstPtr getType(const Identifier& identifier);
    //const TypeInstance& getType(const hm::TypePtr& type);

private:

    TypeInstance* addType(llvm::StringLiteral name, llvm::Type* llvmType, std::vector<std::string> typeClasses);
};

} // namespace type
} // namespace llfp
