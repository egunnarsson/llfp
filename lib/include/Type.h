#pragma once

#include <map>
#include <memory>
#include <unordered_map>

#pragma warning(push, 0)

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"

#pragma warning(pop)

#include "Ast.h"
#include "Common.h"


namespace llfp
{

class SourceModule;
class ImportedModule;

namespace type
{

// names of built in primitive types
namespace name
{

#define TypeID(id, name) static constexpr GlobalIdentifierRef id{ llvm::StringLiteral(""), llvm::StringLiteral(name)};

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
TypeID(Any, "");
TypeID(IntegerLiteral, "@IntegerLiteral");
TypeID(SignedIntegerLiteral, "@SignedIntegerLiteral");
TypeID(FloatingLiteral, "@FloatingLiteral");

#undef TypeID

} // namespace name

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

class TypeContext;

class Type
{
    GlobalIdentifier   identifier_;
    llvm::Type* const  llvmType_; // without this Types could be shared between contexts...
    const bool         isSigned_;

public:

    Type(GlobalIdentifier identifier, llvm::Type* llvmType, bool isSigned = false);
    Type(GlobalIdentifier identifier, bool isFloating, bool isSigned = false);
    virtual ~Type();

    const GlobalIdentifier& identifier() const;

    llvm::Type*             llvmType() const;

    bool                    isNum() const;
    bool                    isInteger() const;
    bool                    isFloating() const;
    bool                    isSigned() const;
    bool                    isLiteral() const;

    virtual bool            isStructType() const;
    virtual Type*           getFieldType(const std::string &fieldIdentifier) const;
    virtual unsigned int    getFieldIndex(const std::string &fieldIdentifier) const;

    Type*                   unify(Type* other, TypeContext* context);

    bool operator ==(GlobalIdentifierRef id) const;
    bool operator !=(GlobalIdentifierRef id) const;
};

class StructType : public Type
{
    struct Field
    {
        unsigned int index;
        Type*        type;
    };

    llvm::StructType* const  llvmType_; // without this Types could be shared between contexts...

    std::unordered_map<std::string, Field> fields;

public:

    StructType(GlobalIdentifier identifier, llvm::StructType* llvmType);

    bool         isStructType() const override;
    Type*        getFieldType(const std::string &fieldIdentifier) const override;
    unsigned int getFieldIndex(const std::string &fieldIdentifier) const override;

    bool setFields(const std::vector<ast::Field> &astFields, std::vector<type::Type*> &fieldTypes);
};

class TypeContext
{
    llvm::LLVMContext& llvmContext;
    SourceModule*      sourceModule;

    std::unordered_map<GlobalIdentifierRef, std::unique_ptr<Type>, GlobalIdentifierRefHash> types;

public:

    TypeContext(llvm::LLVMContext &llvmContext_, SourceModule *sourceModule_);

    StructType* addType(std::unique_ptr<StructType> type);
    Type*       getType(GlobalIdentifierRef identifier);

private:

    Type *      getType(GlobalIdentifierRef identifier, const ImportedModule *module);
};

class TypeScope
{
public:

    virtual TypeContext* getTypeContext() = 0;
    virtual Type* getVariableType(llvm::StringRef variable) = 0;
    virtual Type* getTypeByName(GlobalIdentifierRef identifier) { return getTypeContext()->getType(identifier); }
    // a bit out of place but...
    virtual const ast::FunctionDeclaration* getFunctionAST(GlobalIdentifierRef identifier) = 0;

protected:

    virtual ~TypeScope() {}
};

class EmptyTypeScope : public TypeScope
{
    TypeScope *parent;

public:

    EmptyTypeScope(TypeScope *parent_);
    virtual ~EmptyTypeScope() {}

    TypeContext*                    getTypeContext() override;
    Type*                           getVariableType(llvm::StringRef variable) override;
    const ast::FunctionDeclaration* getFunctionAST(GlobalIdentifierRef identifier) override;
};

class TypeInferer : public ast::ExpVisitor, public TypeScope
{
    TypeScope* const             env;
    std::map<std::string, Type*> variables;
    Type*                        result;

    TypeInferer(TypeScope *env_);
    ~TypeInferer() {}

public:

    static Type* infer(ast::Exp &exp, TypeScope *env);

    void visit(ast::LetExp &exp) override;
    void visit(ast::IfExp &exp) override;
    void visit(ast::CaseExp &exp) override;
    void visit(ast::BinaryExp &exp) override;
    void visit(ast::UnaryExp &exp) override;
    void visit(ast::LiteralExp &exp) override;
    void visit(ast::CallExp &exp) override;
    void visit(ast::VariableExp &exp) override;
    void visit(ast::FieldExp &exp) override;

    TypeContext* getTypeContext() override;
    Type*        getVariableType(llvm::StringRef variable) override;
    const ast::FunctionDeclaration* getFunctionAST(GlobalIdentifierRef identifier) override;
};

} // namespace type
} // namespace llfp
