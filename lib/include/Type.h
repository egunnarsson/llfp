#pragma once

/*
compare, X == Y
call infers arguments to get function instance?
fieldExp infer LHS before generating it
letExp if var has no type, infer before generating

is it ok to fix the type in these cases?
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


namespace llfp
{

class ImportedModule;
class SourceModule;

namespace type
{

struct Identifier
{
    GlobalIdentifier        name;
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

// special internal
TypeID(Any, ""); // "@Any"?
TypeID(IntegerLiteral, "@IntegerLiteral");
TypeID(SignedIntegerLiteral, "@SignedIntegerLiteral");
TypeID(FloatingLiteral, "@FloatingLiteral");

#undef TypeID

} // namespace name
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

class TypeContext;
class Type;

typedef std::shared_ptr<Type> TypePtr;

class Type
{
    Identifier                     identifier_;
    llvm::Type* const              llvmType_; // without this Types could be shared between contexts...
    const bool                     isSigned_;
    std::unordered_set<TypeClass*> typeClasses;

public:

    Type(Identifier identifier, llvm::Type* llvmType, bool isSigned = false);
    Type(Identifier identifier, bool isFloating, bool isSigned = false);
    Type(std::unordered_set<TypeClass*> typeClasses_);
    virtual ~Type();

    const Identifier&       identifier() const;
    const GlobalIdentifier& baseName() const;

    llvm::Type*             llvmType() const;

    bool                    isBool() const;
    bool                    isNum() const;
    bool                    isInteger() const;
    bool                    isFloating() const;
    bool                    isSigned() const;
    bool                    isLiteral() const;

    virtual bool            isConcreteType() const;
    bool                    isBound() const; // baseName() != ""

    static constexpr auto InvalidIndex = (unsigned int)-1;

    virtual unsigned int    getTypeParameterCount() const;
    virtual TypePtr         getTypeParameter(unsigned int index) const;

    virtual bool            isStructType() const;
    virtual unsigned int    getFieldIndex(const std::string &fieldIdentifier) const;
    virtual unsigned int    getFieldCount() const;
    virtual TypePtr         getFieldType(unsigned int index) const;

    const std::unordered_set<TypeClass*>& getTypeClasses() const;

    // only used in unify struct types, remove if we do type visitor
    virtual const ast::Data* getAst() const { return nullptr; }
};

class StructType : public Type
{
    llvm::StructType* const llvmType_; // without this Types could be shared between contexts...
    const ast::Data*        ast;
    std::vector<TypePtr>    fields;
    std::vector<TypePtr>    parameters;

public:

    StructType(Identifier identifier, const ast::Data* ast, llvm::StructType *llvmType);
    StructType(Identifier identifier, const ast::Data* ast, std::vector<TypePtr> fieldTypes);

    bool                 isConcreteType() const override;

    virtual unsigned int getTypeParameterCount() const;
    virtual TypePtr      getTypeParameter(unsigned int index) const;

    bool                 isStructType() const override;
    unsigned int         getFieldIndex(const std::string &fieldIdentifier) const override;
    unsigned int         getFieldCount() const override;
    TypePtr              getFieldType(unsigned int index) const override;

    void                 setFields(std::vector<TypePtr> fieldTypes);

    const ast::Data*     getAst() const override;
};

struct IdentifierHash
{
    size_t operator()(const Identifier &x) const
    {
        auto a = llvm::hash_value(x.name.moduleName);
        auto b = llvm::hash_value(x.name.name);
        std::vector<llvm::hash_code> args(x.parameters.size()); // TODO: do this without allocation
        std::transform(x.parameters.begin(), x.parameters.end(), args.begin(),
            [](const Identifier& x) { return IdentifierHash()(x); });
        auto c = llvm::hash_combine_range(args.begin(), args.end());
        return llvm::hash_combine(a, b, c);
    }
};

class TypeContext
{
    llvm::LLVMContext& llvmContext; // to create types
    SourceModule*      sourceModule; // to do global lookups (actually need parent), qualify names for equals function

    std::unordered_map<Identifier, TypePtr, IdentifierHash> types;

    TypePtr boolType;

    TypePtr i8Type;
    TypePtr i16Type;
    TypePtr i32Type;
    TypePtr i64Type;
    TypePtr i128Type;

    TypePtr u8Type;
    TypePtr u16Type;
    TypePtr u32Type;
    TypePtr u64Type;
    TypePtr u128Type;

    TypePtr halfType;
    TypePtr floatType;
    TypePtr doubleType;

    TypePtr charType;

    // special internal
    TypePtr anyType;
    TypePtr integerLiteralType;
    TypePtr signedIntegerLiteralType;
    TypePtr floatingLiteralType;

public:

    TypeContext(llvm::LLVMContext &llvmContext_, SourceModule *sourceModule_);

    TypePtr getTypeFromAst(const ast::TypeIdentifier& identifier);
    TypePtr getTypeFromAst(const ast::TypeIdentifier& identifier, const ImportedModule* lookupModule);
    bool    equals(const TypePtr &type, const ast::TypeIdentifier& identifier);
    bool    equals(const TypePtr& type, llvm::StringRef identifier);

    TypePtr unify(const TypePtr &a, const TypePtr& b);
    TypePtr fix(const TypePtr& t);
    TypePtr fixify(const TypePtr& a, const TypePtr& b) { return fix(unify(a, b)); }

    TypePtr getBool();
    TypePtr getChar();
    TypePtr getI64();
    TypePtr getU64();
    TypePtr getDouble();
    TypePtr getAnyType();
    TypePtr getIntegerLiteralType();
    TypePtr getSignedIntegerLiteralType();
    TypePtr getFloatingLiteralType();

    TypePtr getType(const Identifier& identifier);
    bool    isPrimitive(const Identifier& identifier);

private:

    TypePtr unifyLiterals(const TypePtr& a, const TypePtr& b);
    TypePtr unifyConcreteAndLiteral(const TypePtr& nonLiteralType, const TypePtr& literalType);
    TypePtr unifyCheckTypeClass(const TypePtr& t, const TypePtr& typeClass);
    TypePtr unifyStructTypes(const TypePtr& a, const TypePtr& b);
};

class TypeScope
{
public:

    virtual TypeContext* getTypeContext() = 0;
    virtual TypePtr      getVariableType(const std::string& variable) = 0;
    virtual TypePtr      getTypeByName(const ast::TypeIdentifier& identifier, const ImportedModule *astModule)
        { return getTypeContext()->getTypeFromAst(identifier, astModule); }
    // a bit out of place but...
    virtual llfp::FunAst       getFunctionAST(const GlobalIdentifier &identifier) = 0;
    virtual llfp::FunDeclAst   getFunctionDeclarationAST(const GlobalIdentifier& identifier) = 0;
    virtual llfp::DataAst      getDataAST(const GlobalIdentifier &identifier) = 0;

protected:

    virtual ~TypeScope() {}
};

/**
Hides variables from the parent. Used when infering return type of calls to untyped functions.
*/
class EmptyTypeScope : public TypeScope
{
    TypeScope *parent;

public:

    EmptyTypeScope(TypeScope *parent_);
    virtual ~EmptyTypeScope() {}

    TypeContext* getTypeContext() override;
    TypePtr      getVariableType(const std::string& variable) override;
    FunAst       getFunctionAST(const GlobalIdentifier& identifier) override;
    FunDeclAst   getFunctionDeclarationAST(const GlobalIdentifier& identifier) override;
    DataAst      getDataAST(const GlobalIdentifier& identifier) override;
};

/**
For parameterized types, provides a scope for the type variables.
*/
class ConstructorTypeScope : public TypeScope
{
    TypeScope* const               parent;
    std::map<std::string, TypePtr> typeMap;

public:

    ConstructorTypeScope(TypeScope *parent_, const ast::Data *ast);
    virtual ~ConstructorTypeScope();

    bool updateType(const ast::TypeIdentifier& identifier, const TypePtr &type);
    TypePtr getTypeVariable(const std::string& typeVariable) const;

    TypeContext* getTypeContext() override;
    TypePtr      getVariableType(const std::string& variable) override;
    TypePtr      getTypeByName(const ast::TypeIdentifier& identifier, const ImportedModule*astModule) override;// { return getTypeContext()->getType(identifier); }
    // a bit out of place but...
    FunAst       getFunctionAST(const GlobalIdentifier& identifier) override;
    FunDeclAst   getFunctionDeclarationAST(const GlobalIdentifier& identifier) override;
    DataAst      getDataAST(const GlobalIdentifier& identifier) override;

};

class TypeInferer : public ast::ExpVisitor, public TypeScope
{
    TypeScope* const               env;
    std::map<std::string, TypePtr> variables;
    TypePtr                        result;

    TypeInferer(TypeScope *env_);
    ~TypeInferer() {}

public:

    static TypePtr infer(ast::Exp &exp, TypeScope *env);

    void visit(ast::LetExp &exp) override;
    void visit(ast::IfExp &exp) override;
    void visit(ast::CaseExp &exp) override;
    void visit(ast::BinaryExp &exp) override;
    void visit(ast::UnaryExp &exp) override;
    void visit(ast::LiteralExp &exp) override;
    void visit(ast::CallExp &exp) override;
    void visit(ast::VariableExp &exp) override;
    void visit(ast::FieldExp &exp) override;
    void visit(ast::ConstructorExp &exp) override;

    TypeContext* getTypeContext() override;
    TypePtr      getVariableType(const std::string& variable) override;
    FunAst       getFunctionAST(const GlobalIdentifier& identifier) override;
    FunDeclAst   getFunctionDeclarationAST(const GlobalIdentifier& identifier) override;
    DataAst      getDataAST(const GlobalIdentifier& identifier) override;
};

} // namespace type
} // namespace llfp

/*namespace std
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
}*/
