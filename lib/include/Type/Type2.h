#pragma once

#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include "Ast.h"


namespace llfp
{
namespace hm
{

typedef int TypeVarId;

class Type;
class SimpleType;
class TypeVar;
class TypeConstant;
class FunctionType;

typedef std::shared_ptr<Type> TypePtr;


struct Substitution
{
    TypeVarId id;
    TypePtr   type;
};


class TypeVisitor
{
public:

    virtual void visit(TypeVar& t) = 0;
    virtual void visit(TypeConstant& t) = 0;
    virtual void visit(FunctionType& t) = 0;
};


class Type
{
public:

    virtual std::string str() const = 0;

    virtual bool        equals(TypeVarId) const { return false; }

    static void         apply(TypePtr& ptr, Substitution s);
    virtual void        apply(Substitution s) = 0;

    virtual void        accept(TypeVisitor* visitor) = 0;

protected:

    static std::vector<Substitution> unify(TypeVar& a, SimpleType& b, const TypePtr& ptrB);
    static std::vector<Substitution> unify(TypeVar& a, FunctionType& b, const TypePtr& ptrB);
    [[noreturn]] static void         unifyError(const Type& a, const Type& b);
};


class SimpleType : public Type
{
public:

    std::set<std::string>          typeClasses;
    std::map<std::string, TypePtr> fields;
    //std::vector<std::string> constructors;

    std::vector<Substitution> addConstraints(const SimpleType& other);
    std::string               printConstraints(std::string base) const;
    void                      apply(Substitution s) override;
};


class TypeVar : public SimpleType
{
public:

    TypeVarId id;

    TypeVar(TypeVarId id_);

    std::string                      str() const override;

    bool                             equals(TypeVarId t) const override;

    static std::vector<Substitution> unify(TypeVar& a, TypeVar& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(TypeVar& a, TypeConstant& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(TypeVar& a, FunctionType& b, const TypePtr& ptrA, const TypePtr& ptrB);

    void                             apply(Substitution s) override;

    void                             accept(TypeVisitor* visitor) override;
};


class TypeConstant : public SimpleType
{
public:

    std::string id;

    TypeConstant(std::string id);

    std::string                      str() const override;

    static std::vector<Substitution> unify(TypeConstant& a, TypeVar& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(TypeConstant& a, TypeConstant& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(TypeConstant& a, FunctionType& b, const TypePtr& ptrA, const TypePtr& ptrB);

    void                             accept(TypeVisitor* visitor) override;
};


class FunctionType : public Type // TypeApplication
{
public:

    std::vector<TypePtr> types;

    FunctionType(std::vector<TypePtr> types);

    std::string                      str() const override;

    static std::vector<Substitution> unify(FunctionType& a, TypeVar& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(FunctionType& a, TypeConstant& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(FunctionType& a, FunctionType& b, const TypePtr& ptrA, const TypePtr& ptrB);

    void                             apply(Substitution s) override;

    void                             accept(TypeVisitor* visitor) override;
};


template<class T>
class TypeUnifierT : public TypeVisitor
{
public:
    T&                        self;
    const TypePtr&            a;
    const TypePtr&            b;
    std::vector<Substitution> result;

    TypeUnifierT(T& self_, const TypePtr& a_, const TypePtr& b_) :self(self_), a(a_), b(b_) {}

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
        TypeUnifierT<T> u{ self, a, b, };
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
    //const char* explanation?
    TypePtr        left;
    TypePtr        right;

    Constraint(SourceLocation location_, TypePtr left_, TypePtr right_);

    std::string               str();
    void                      substitute(Substitution s);
    std::vector<Substitution> solve();
};


class TypeAnnotation
{
    std::map<ast::Node*, TypePtr>  ast;
    std::map<std::string, TypePtr> vars; // things required, like abs(float) and abs(int);

public:

    TypeAnnotation(std::map<ast::Node*, TypePtr> ast_, std::map<std::string, TypePtr> vars_);

    TypePtr get(ast::Node* n);
    TypePtr get(const std::string& id);

    void    substitute(Substitution sub);
    void    print();
};


class Annotator : public ast::ExpVisitor
{
    TypeVarId current = 0;

public:

    std::map<std::string, TypePtr> vars;
    std::map<ast::Node*, TypePtr>  result;
    std::vector<Constraint>        constraints;

    void operator()(ast::Function& fun);

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

private:

    TypePtr makeVar();
    TypePtr makeConst(std::string s);
    TypePtr makeClass(std::string s);
    TypePtr makeFunction(std::vector<TypePtr> types);

    TypePtr tv(const std::string& name);
    TypePtr tv(ast::Node& ast);
    template<class T>
    TypePtr tv(const std::unique_ptr<T>& ast)
    {
        return result.at(ast.get());
    }

    void    add(Constraint c);
};


std::string test(ast::Function& fun);

TypeAnnotation inferType(ast::Function& fun);

} // namespace hm
} // namespace llfp
