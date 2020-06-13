#pragma once

#pragma warning(push, 0)

#include <map>
#include <memory>
#include <unordered_map>

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"

#pragma warning(pop)

#include "Ast.h"


namespace llfp
{
namespace type
{

// names of built in primitive types
namespace name
{

static constexpr auto Bool = "bool";

static constexpr auto I8 = "i8";
static constexpr auto I16 = "i16";
static constexpr auto I32 = "i32";
static constexpr auto I64 = "i64";
static constexpr auto I128 = "i128";

static constexpr auto U8 = "u8";
static constexpr auto U16 = "u16";
static constexpr auto U32 = "u32";
static constexpr auto U64 = "u64";
static constexpr auto U128 = "u128";

static constexpr auto Half = "half";
static constexpr auto Float = "float";
static constexpr auto Double = "double";

static constexpr auto Char = "char";

// special internal
static constexpr auto IntegerLiteral = "@IntegerLiteral";
static constexpr auto SignedIntegerLiteral = "@SignedIntegerLiteral";
static constexpr auto FloatingLiteral = "@FloatingLiteral";

} // namespace name

enum TypeClass
{
    Num,
    Fractional,
    Eq,
    Ord,
    Bits
};

class TypeEnvironment;

class Type
{
    const std::string name_;
    llvm::Type * const llvmType_;
    const bool isSigned_;

public:

    Type(std::string name, llvm::Type* llvmType, bool isSigned = false);
    Type(std::string name, bool isFloating, bool isSigned = false);

    const std::string& name() const;

    llvm::Type *llvmType() const;

    bool isNum() const;
    bool isInteger() const;
    bool isFloating() const;
    bool isSigned() const;
    bool isLiteral() const;

    Type* unify(Type* other, TypeEnvironment* context);

    bool operator ==(const std::string &b) const;
    bool operator !=(const std::string &b) const;
};

class TypeContext
{
    std::unordered_map<std::string, std::unique_ptr<Type>> types;

public:

    TypeContext(llvm::LLVMContext &llvmContext);

    Type* getType(llvm::StringRef name);
};

class TypeEnvironment
{
public:

    virtual Type* getTypeByName(llvm::StringRef variable) = 0;
    virtual Type* getVariableType(llvm::StringRef variable) = 0;
    virtual Type* getFunctionReturnType(llvm::StringRef function) = 0;

protected:

    virtual ~TypeEnvironment() {}
};

class TypeInferer : public ast::ExpVisitor, public TypeEnvironment
{
    TypeEnvironment* const       env;
    std::map<std::string, Type*> variables;
    Type*                        result;

    TypeInferer(TypeEnvironment *env_);
    ~TypeInferer() {}

public:

    static Type* infer(ast::Exp &exp, TypeEnvironment *env);

    void visit(ast::LetExp &exp) override;
    void visit(ast::IfExp &exp) override;
    void visit(ast::CaseExp &exp) override;
    void visit(ast::BinaryExp &exp) override;
    void visit(ast::UnaryExp &exp) override;
    void visit(ast::LiteralExp &exp) override;
    void visit(ast::CallExp &exp) override;
    void visit(ast::VariableExp &exp) override;

    Type* getTypeByName(llvm::StringRef name) override;
    Type* getVariableType(llvm::StringRef variable) override;
    Type* getFunctionReturnType(llvm::StringRef function) override;
};

} // namespace type
} // namespace llfp