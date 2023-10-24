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
class UnboundTypeVar;
class BoundTypeVar;
class TypeConstant;
class FunctionType;

typedef std::shared_ptr<Type>           TypePtr;
typedef std::shared_ptr<UnboundTypeVar> UnboundTypeVarPtr;
typedef std::shared_ptr<BoundTypeVar>   BoundTypeVarPtr;
typedef std::shared_ptr<TypeConstant>   TypeConstantPtr;
typedef std::shared_ptr<SimpleType>     SimpleTypePtr;
typedef std::shared_ptr<FunctionType>   FunTypePtr;


struct Substitution
{
    TypeVarId id;
    TypePtr   type;
};


class TypeVisitor
{
public:

    virtual void visit(UnboundTypeVar& t) = 0;
    virtual void visit(BoundTypeVar& t)   = 0;
    virtual void visit(TypeConstant& t)   = 0;
    virtual void visit(FunctionType& t)   = 0;
};


class Type
{
public:

    virtual std::string str() const = 0;
    virtual bool        equals(TypeVarId) const { return false; }
    virtual TypePtr     copy(std::map<std::string, TypeConstantPtr>& typeConstants) const = 0;
    virtual void        accept(TypeVisitor* visitor)                              = 0;

    static void apply(TypePtr& ptr, Substitution s);

protected:

    [[noreturn]] static void unifyError(const Type& a, const Type& b);

    virtual void apply(Substitution s) = 0;
};


class SimpleType : public Type
{
public:

    std::set<std::string>          typeClasses;
    std::map<std::string, TypePtr> fields;
    // std::optional<std::vector<TypePtr>> parameters;
    //  assert(!(!fields.empty() && constructors.size() > 1));

    std::vector<Substitution> addConstraints(const SimpleType& other); // rename?
    std::string               printConstraints(const std::string& base) const;

    void copy(std::map<std::string, TypeConstantPtr>& typeConstants, SimpleType* newObj) const;

protected:

    void apply(Substitution s) override;
};

/* Unknown data construct */
class UnboundTypeVar : public SimpleType
{
public:

    TypeVarId             id_;
    std::set<std::string> constructors_; // when can I know about a constructor without having done a ast lookup and found the ast and thus bound the type?

    UnboundTypeVar(TypeVarId id);

    std::string str() const override;

    bool equals(TypeVarId t) const override;

    static std::vector<Substitution> unify(UnboundTypeVar& a, UnboundTypeVar& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(UnboundTypeVar& a, BoundTypeVar& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(UnboundTypeVar& a, TypeConstant& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(UnboundTypeVar& a, FunctionType& b, const TypePtr& ptrA, const TypePtr& ptrB);

    void accept(TypeVisitor* visitor) override;

    TypePtr copy(std::map<std::string, TypeConstantPtr>& typeConstants) const override;

protected:

    void apply(Substitution s) override;
};

/* The data construct is known but at least one parameter is not known */
class BoundTypeVar : public SimpleType
{
public:

    TypeVarId            id_;
    DataAst              ast_;
    std::vector<TypePtr> parameters_;

    BoundTypeVar(DataAst ast, TypeVarId id);

    std::string str() const override;

    bool equals(TypeVarId t) const override;

    static std::vector<Substitution> unify(BoundTypeVar& a, UnboundTypeVar& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(BoundTypeVar& a, BoundTypeVar& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(BoundTypeVar& a, TypeConstant& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(BoundTypeVar& a, FunctionType& b, const TypePtr& ptrA, const TypePtr& ptrB);

    void accept(TypeVisitor* visitor) override;

    TypePtr copy(std::map<std::string, TypeConstantPtr>& typeConstants) const override;

protected:

    void apply(Substitution s) override;
};


/* Data construct is known and all parameters are also constants */
class TypeConstant : public SimpleType
{
public:

    std::string                  id_;
    DataAst                      ast_; // this this and return empty constructor/fields for basic types?
    std::vector<TypeConstantPtr> parameters_;

    TypeConstant(DataAst ast, std::string id);

    std::string str() const override;

    static std::vector<Substitution> unify(TypeConstant& a, UnboundTypeVar& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(TypeConstant& a, BoundTypeVar& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(TypeConstant& a, TypeConstant& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(TypeConstant& a, FunctionType& b, const TypePtr& ptrA, const TypePtr& ptrB);

    void accept(TypeVisitor* visitor) override;

    TypePtr         copy(std::map<std::string, TypeConstantPtr>& typeConstants) const override;
    TypeConstantPtr copyConst(std::map<std::string, TypeConstantPtr>& typeConstants) const;

protected:

    void apply(Substitution s) override;
};


class FunctionType : public Type // TypeApplication
{
public:

    std::vector<TypePtr> types;

    FunctionType(std::vector<TypePtr> types);

    std::string str() const override;

    static std::vector<Substitution> unify(FunctionType& a, UnboundTypeVar& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(FunctionType& a, BoundTypeVar& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(FunctionType& a, TypeConstant& b, const TypePtr& ptrA, const TypePtr& ptrB);
    static std::vector<Substitution> unify(FunctionType& a, FunctionType& b, const TypePtr& ptrA, const TypePtr& ptrB);

    void accept(TypeVisitor* visitor) override;

    TypePtr    copy(std::map<std::string, TypeConstantPtr>& typeConstants) const override;
    FunTypePtr copyFun(std::map<std::string, TypeConstantPtr>& typeConstants) const;

    // protected:

    void apply(Substitution s) override;
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

    void visit(UnboundTypeVar& other) override { visit_(other); }
    void visit(BoundTypeVar& other) override { visit_(other); }
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

    void visit(UnboundTypeVar& self) override;
    void visit(BoundTypeVar& self) override;
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
    TypeAnnotation(TypeAnnotation&& other) = default;
    TypeAnnotation(const TypeAnnotation& other);

    TypeAnnotation& operator=(TypeAnnotation&& other) = default;
    TypeAnnotation& operator=(const TypeAnnotation& other);

    TypePtr    get(const ast::Node* n) const;
    TypePtr    getVar(const std::string& id) const;
    FunTypePtr getFun(const std::string& id) const;

    auto& getTypes() const { return ast; }
    auto& getFunctions() const { return functions; }

    void substitute(Substitution sub);

    void addConstraint(const std::string& funName, const FunTypePtr& type);
    bool addConstraint(const TypePtr& a, const TypeConstantPtr& b);

    void print() const;
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

    UnboundTypeVarPtr makeVar();
    BoundTypeVarPtr   makeVar(DataAst ast);
    TypeConstantPtr   makeConst(llvm::StringRef s);
    TypeConstantPtr   makeConst(const GlobalIdentifier& id);
    TypeConstantPtr   makeConst(const ast::TypeIdentifier& id);
    TypePtr           makeClass(llvm::StringRef s);
    FunTypePtr        makeFunction(std::vector<TypePtr> types);

    TypePtr typeFromIdentifier(const ast::TypeIdentifier& id, const std::map<std::string, TypePtr>& typeVariables = {});

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

TypeAnnotation inferType(const ImportedModule* mod, const ast::Function& fun);
hm::FunTypePtr inferType(const ImportedModule* mod, const ast::Class& class_, const ast::FunctionDeclaration& ast);

} // namespace llfp::hm
