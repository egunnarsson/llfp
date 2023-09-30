#pragma once

/*
genPrototype(infered, wanted)
    actual = unify(infered, wanted)
// for proto, store new TypeAnnotation with actual?

genLetExp( // in context where infered for fun has been unified with actual?
genCompare // same
call( we want to call getFunction(id, types) // get wanted, and genPrototype(infered, newWanted)
fieldLHS()
constructor? possible typevars?

*/

#include "Ast.h"
#include "IModule.h"

#pragma warning(push, 0)

#include <llvm/ADT/StringRef.h>

#pragma warning(pop)

#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>


namespace llfp::hm
{

typedef int TypeVarId;

class Type;
class SimpleType;
class TypeVar;
class TypeConstant;
class FunctionType;

typedef std::shared_ptr<Type>         TypePtr;
typedef std::shared_ptr<TypeVar>      TypeVarPtr;
typedef std::shared_ptr<TypeConstant> TypeConstantPtr;
typedef std::shared_ptr<SimpleType>   SimpleTypePtr;
typedef std::shared_ptr<FunctionType> FunTypePtr;


struct Substitution
{
    TypeVarId id;
    TypePtr   type;
};


class TypeVisitor
{
public:

    virtual void visit(TypeVar& t)      = 0;
    virtual void visit(TypeConstant& t) = 0;
    virtual void visit(FunctionType& t) = 0;
};


class Type
{
public:

    virtual std::string str() const = 0;

    virtual bool equals(TypeVarId) const { return false; }

    static void  apply(TypePtr& ptr, Substitution s);
    virtual void apply(Substitution s) = 0;

    virtual void accept(TypeVisitor* visitor) = 0;

    virtual TypePtr copy(std::map<std::string, TypePtr>& typeConstants) const = 0;

protected:

    static std::vector<Substitution> unify(TypeVar& a, SimpleType& b, const TypePtr& ptrB);
    static std::vector<Substitution> unify(TypeVar& a, FunctionType& b, const TypePtr& ptrB);
    [[noreturn]] static void         unifyError(const Type& a, const Type& b);
};


class SimpleType : public Type
{
public:

    std::set<std::string>               typeClasses;
    std::map<std::string, TypePtr>      fields;
    std::set<std::string>               constructors;
    std::optional<std::vector<TypePtr>> parameters;
    // assert(!(!fields.empty() && constructors.size() > 1));

    std::vector<Substitution> addConstraints(const SimpleType& other);
    std::string               printConstraints(const std::string& base) const;
    void                      apply(Substitution s) override;

protected:

    void copy(std::map<std::string, TypePtr>& typeConstants, SimpleType* newObj) const;
};


class TypeVar : public SimpleType
{
public:

    TypeVarId id;

    TypeVar(TypeVarId id_);

    std::string str() const override;

    bool equals(TypeVarId t) const override;

    static std::vector<Substitution> unify(TypeVar& a, TypeVar& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(TypeVar& a, TypeConstant& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(TypeVar& a, FunctionType& b, const TypePtr& ptrA, const TypePtr& ptrB);

    void apply(Substitution s) override;

    void accept(TypeVisitor* visitor) override;

    TypePtr copy(std::map<std::string, TypePtr>& typeConstants) const override;
};


class TypeConstant : public SimpleType
{
public:

    std::string id;
    // DataAst astType; // nullptr for basic types

    TypeConstant(std::string id);

    std::string str() const override;

    static std::vector<Substitution> unify(TypeConstant& a, TypeVar& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(TypeConstant& a, TypeConstant& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(TypeConstant& a, FunctionType& b, const TypePtr& ptrA, const TypePtr& ptrB);

    void accept(TypeVisitor* visitor) override;

    TypePtr copy(std::map<std::string, TypePtr>& typeConstants) const override;
};


class FunctionType : public Type // TypeApplication
{
public:

    std::vector<TypePtr> types;

    FunctionType(std::vector<TypePtr> types);

    std::string str() const override;

    static std::vector<Substitution> unify(FunctionType& a, TypeVar& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(FunctionType& a, TypeConstant& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(FunctionType& a, FunctionType& b, const TypePtr& ptrA, const TypePtr& ptrB);

    void apply(Substitution s) override;

    void accept(TypeVisitor* visitor) override;

    TypePtr    copy(std::map<std::string, TypePtr>& typeConstants) const override;
    FunTypePtr copyFun(std::map<std::string, TypePtr>& typeConstants) const;
};


template<class T>
class TypeUnifierT : public TypeVisitor
{
public:

    T&                        self;
    const TypePtr&            a;
    const TypePtr&            b;
    std::vector<Substitution> result;

    TypeUnifierT(T& self_, const TypePtr& a_, const TypePtr& b_)
        : self(self_),
          a(a_),
          b(b_)
    {}

    template<class V>
    void visit_(V& other)
    {
        result = T::unify(self, other, a, b);
    }

    void visit(TypeVar& other) override { visit_(other); }
    void visit(TypeConstant& other) override { visit_(other); }
    void visit(FunctionType& other) override { visit_(other); }
};


class TypeUnifier : public TypeVisitor
{
    const TypePtr&            a;
    const TypePtr&            b;
    std::vector<Substitution> result;

    TypeUnifier(const TypePtr& a_, const TypePtr& b_);

public:

    static std::vector<Substitution> unify(const TypePtr& a, const TypePtr& b);

    template<class T>
    void visit_(T& self)
    {
        TypeUnifierT<T> u{
            self,
            a,
            b,
        };
        b->accept(&u);
        result = std::move(u.result);
    }

    void visit(TypeVar& self) override;
    void visit(TypeConstant& self) override;
    void visit(FunctionType& self) override;
};


class Constraint
{
public:

    SourceLocation location;
    // const char* explanation?
    TypePtr        left;
    TypePtr        right;

    Constraint(SourceLocation location_, TypePtr left_, TypePtr right_);

    std::string               str();
    void                      substitute(Substitution s);
    std::vector<Substitution> solve();
};


class TypeAnnotation
{
    std::map<const ast::Node*, TypePtr> ast;
    // std::map<std::string, TypePtr>      vars; // things required, like abs(float) and abs(int);
    std::map<std::string, TypePtr>      variables;
    std::map<std::string, FunTypePtr>   functions;
    TypeVarId                           nextFreeVariable = 0;

public:

    TypeAnnotation() = default;
    TypeAnnotation(
        std::map<const ast::Node*, TypePtr> ast_,
        std::map<std::string, TypePtr>      vars_,
        std::map<std::string, FunTypePtr>   functions_,
        TypeVarId                           nextFreeVariable_);
    TypeAnnotation(const TypeAnnotation& other);
    TypeAnnotation& operator=(const TypeAnnotation& other);

    TypePtr    get(const ast::Node* n) const;
    TypePtr    getVar(const std::string& id) const;
    FunTypePtr getFun(const std::string& id) const;

    const auto& getTypes() { return ast; }
    const auto& getFunctions() { return functions; }

    void substitute(Substitution sub);

    bool addConstraint(const std::string& var, const TypePtr& type);
    bool addConstraint(const TypePtr& a, const TypeConstantPtr& b);

    void print();
};


class PatternTypeVisitor;

class Annotator : public ast::ExpVisitor
{
    const ImportedModule*                  astModule_ = nullptr;
    std::map<std::string, TypeConstantPtr> typeConstants;
    // std::map<std::string, TypePtr>      vars; // things required, like abs(float) and abs(int);

public:

    Annotator(const ImportedModule* astModule);

    TypeVarId                           current = 0;
    std::map<std::string, TypePtr>      variables;
    std::map<std::string, FunTypePtr>   functions;
    std::map<const ast::Node*, TypePtr> result;
    std::vector<Constraint>             constraints;

    void operator()(const ast::Function& fun);

    void visit(ast::LetExp& exp) override;
    void visit(ast::IfExp& exp) override;
    void visit(ast::CaseExp& exp) override;
    void visit(ast::BinaryExp& exp) override;
    void visit(ast::UnaryExp& exp) override;
    void visit(ast::LiteralExp& exp) override;
    void visit(ast::CallExp& exp) override;
    void visit(ast::VariableExp& exp) override;
    void visit(ast::FieldExp& exp) override;
    void visit(ast::ConstructorExp& exp) override;
    void visit(ast::IntrinsicExp& exp) override;

private:

    TypeVarPtr      makeVar();
    TypeConstantPtr makeConst(llvm::StringRef s);
    TypePtr         makeClass(llvm::StringRef s);
    FunTypePtr      makeFunction(std::vector<TypePtr> types);

    TypePtr typeFromIdentifier(const ast::TypeIdentifier& id, const std::map<std::string, TypeVarPtr>& typeVariables = {});

    TypePtr tv(const std::string& name);
    TypePtr tv(ast::Node& ast);
    template<class T>
    TypePtr tv(const std::unique_ptr<T>& ast)
    {
        return result.at(ast.get());
    }

    void add(Constraint c);

    friend PatternTypeVisitor;
};


class PatternTypeVisitor : public ast::PatternVisitor
{
    // std::map<std::string, > variables;
    Annotator& annotator;

public:

    PatternTypeVisitor(Annotator& annotator_);

    static void visit(Annotator& annotator, ast::Pattern& pattern);

    void visit(ast::BoolPattern& pattern) override;
    void visit(ast::IdentifierPattern& pattern) override;
    void visit(ast::IntegerPattern& pattern) override;
    void visit(ast::FloatPattern& pattern) override;
    void visit(ast::CharPattern& pattern) override;
    void visit(ast::StringPattern& pattern) override;
    void visit(ast::ConstructorPattern& pattern) override;

private:

    void add(ast::Pattern& pattern, TypePtr type);
};


std::string test(const ImportedModule* astModule, ast::Function& fun);

TypeAnnotation inferType(const ImportedModule* astModule, const ast::Function& fun);

} // namespace llfp::hm
