#pragma once

#include "Common/GlobalIdentifier.h"
#include "Common/SourceLocation.h"
#include "Lexer.h"

#include <memory>
#include <string>
#include <vector>


namespace llfp::ast
{

struct TypeIdentifier
{
    // location?

    GlobalIdentifier            identifier;
    std::vector<TypeIdentifier> parameters;

    std::string str() const;
    bool        empty() const;
};

struct LetExp;
struct IfExp;
struct CaseExp;
struct BinaryExp;
struct UnaryExp;
struct LiteralExp;
struct CallExp;
struct VariableExp;
struct FieldExp;
struct ConstructorExp;
struct IntrinsicExp;

class ExpVisitor
{
public:

    virtual void visit(LetExp& exp)         = 0;
    virtual void visit(IfExp& exp)          = 0;
    virtual void visit(CaseExp& exp)        = 0;
    virtual void visit(BinaryExp& exp)      = 0;
    virtual void visit(UnaryExp& exp)       = 0;
    virtual void visit(LiteralExp& exp)     = 0;
    virtual void visit(CallExp& exp)        = 0;
    virtual void visit(VariableExp& exp)    = 0;
    virtual void visit(FieldExp& exp)       = 0;
    virtual void visit(ConstructorExp& exp) = 0;
    virtual void visit(IntrinsicExp& exp)   = 0;

protected:

    virtual ~ExpVisitor() {}
};

template<class T>
class ExpVisitorCallable : public ExpVisitor
{
    T visitor_;

public:

    explicit ExpVisitorCallable(T&& visitor)
        : visitor_{ visitor }
    {}
    virtual ~ExpVisitorCallable() = default;

    void visit(LetExp& exp) override { visitor_(exp); }
    void visit(IfExp& exp) override { visitor_(exp); }
    void visit(CaseExp& exp) override { visitor_(exp); }
    void visit(BinaryExp& exp) override { visitor_(exp); }
    void visit(UnaryExp& exp) override { visitor_(exp); }
    void visit(LiteralExp& exp) override { visitor_(exp); }
    void visit(CallExp& exp) override { visitor_(exp); }
    void visit(VariableExp& exp) override { visitor_(exp); }
    void visit(FieldExp& exp) override { visitor_(exp); }
    void visit(ConstructorExp& exp) override { visitor_(exp); }
    void visit(IntrinsicExp& exp) override { visitor_(exp); }
};

struct BoolPattern;
struct IdentifierPattern;
struct IntegerPattern;
struct FloatPattern;
struct CharPattern;
struct StringPattern;
struct ConstructorPattern;

class PatternVisitor
{
public:

    virtual void visit(BoolPattern& pattern)        = 0;
    virtual void visit(IdentifierPattern& pattern)  = 0;
    virtual void visit(IntegerPattern& pattern)     = 0;
    virtual void visit(FloatPattern& pattern)       = 0;
    virtual void visit(CharPattern& pattern)        = 0;
    virtual void visit(StringPattern& pattern)      = 0;
    virtual void visit(ConstructorPattern& pattern) = 0;

protected:

    virtual ~PatternVisitor() {}
};

struct Node
{
    SourceLocation location;

protected:

    Node(SourceLocation location_);
    ~Node() = default;
};

struct Field final : public Node
{
    TypeIdentifier type;
    std::string    name;

    Field(SourceLocation location_, TypeIdentifier type_, std::string name_);
    virtual ~Field();
};

struct DataConstructor final : public Node
{
    std::string        name;
    std::vector<Field> fields;

    DataConstructor(
        SourceLocation     location_,
        std::string        name_,
        std::vector<Field> fields_);
    virtual ~DataConstructor();
};

struct Data final : public Node
{
    std::string                  name;
    std::vector<std::string>     typeVariables;
    std::vector<DataConstructor> constructors;
    bool                         exported;

    Data(
        SourceLocation               location_,
        std::string                  name_,
        std::vector<std::string>     typeVariables_,
        std::vector<DataConstructor> constructors_,
        bool                         exported_);
    virtual ~Data();
};

struct Parameter final : public Node
{
    TypeIdentifier type;
    std::string    identifier; // or name?

    Parameter(SourceLocation location_, TypeIdentifier type_, std::string identifier_);
    virtual ~Parameter();
};

struct Exp : public Node
{
    virtual ~Exp() = default;

    virtual void accept(ExpVisitor* visitor) = 0;

    template<class T>
    void visit(T&& visitor)
    {
        ExpVisitorCallable callableWrapper{ std::move(visitor) };
        accept(&callableWrapper);
    }

protected:

    Exp(SourceLocation location_);
};
/*
enum FunctionType
{
    Exported,
    Local,
    Instance
};
*/
struct Function final : public Node
{
    std::string                             name;
    TypeIdentifier                          type;
    std::vector<std::unique_ptr<Parameter>> parameters;
    std::unique_ptr<Exp>                    functionBody; // rename body
    bool                                    exported;

    Function(
        SourceLocation                          location_,
        std::string                             name_,
        TypeIdentifier                          type_,
        std::vector<std::unique_ptr<Parameter>> parameters_,
        std::unique_ptr<Exp>                    functionBody_,
        bool                                    exported);
    virtual ~Function();
};

struct FunctionDeclaration final : public Node
{
    std::string                             name;
    TypeIdentifier                          type;
    std::vector<std::unique_ptr<Parameter>> parameters;

    FunctionDeclaration(
        SourceLocation                          location_,
        std::string                             name_,
        TypeIdentifier                          type_,
        std::vector<std::unique_ptr<Parameter>> parameters_);
    virtual ~FunctionDeclaration();
};

struct Class final : public Node
{
    std::string                                       name;
    std::string                                       typeVariable;
    std::vector<std::unique_ptr<FunctionDeclaration>> functions;

    Class(
        SourceLocation                                    location_,
        std::string                                       name_,
        std::string                                       typeVariable_,
        std::vector<std::unique_ptr<FunctionDeclaration>> functions_);
    virtual ~Class();
};

struct ClassInstance final : public Node
{
    GlobalIdentifier                       classIdentifier;
    TypeIdentifier                         typeArgument;
    std::vector<std::unique_ptr<Function>> functions;

    ClassInstance(
        SourceLocation                         location_,
        GlobalIdentifier                       classIdentifier_,
        TypeIdentifier                         typeArgument_,
        std::vector<std::unique_ptr<Function>> functions_);
    virtual ~ClassInstance();
};

struct Public final : public Node
{
    std::string name;

    Public(SourceLocation location_, std::string name_);
    virtual ~Public();
};

struct Import final : public Node
{
    std::string name;

    Import(SourceLocation location_, std::string name_);
    virtual ~Import();
};

struct Module final : public Node
{
    std::string                                 name;
    std::vector<Public>                         publics;
    std::vector<Import>                         imports;
    std::vector<std::unique_ptr<Data>>          datas;
    std::vector<std::unique_ptr<Function>>      functions;
    std::vector<std::unique_ptr<Class>>         classes;
    std::vector<std::unique_ptr<ClassInstance>> classInstances;

    Module(SourceLocation location_, std::string name_);
    virtual ~Module();
};

struct LetExp final : public Exp
{
    std::vector<std::unique_ptr<Function>> letStatments;
    std::unique_ptr<Exp>                   exp;

    LetExp(SourceLocation location_, std::vector<std::unique_ptr<Function>> letStatments_, std::unique_ptr<Exp> exp_);
    virtual ~LetExp();

    void accept(ExpVisitor* visitor) override;
};

struct IfExp final : public Exp
{
    std::unique_ptr<Exp> condition;
    std::unique_ptr<Exp> thenExp;
    std::unique_ptr<Exp> elseExp;

    IfExp(SourceLocation location_, std::unique_ptr<Exp> condition_, std::unique_ptr<Exp> thenExp_, std::unique_ptr<Exp> elseExp_);
    virtual ~IfExp();

    void accept(ExpVisitor* visitor) override;
};

struct Pattern : public Node
{
    Pattern(SourceLocation location_);
    virtual ~Pattern();

    virtual void accept(PatternVisitor* visitor) = 0;
};

struct BoolPattern final : public Pattern
{
    bool value;

    BoolPattern(SourceLocation location_, bool value_);
    virtual ~BoolPattern();

    void accept(PatternVisitor* visitor) override;
};

struct IdentifierPattern final : public Pattern
{
    std::string value;

    IdentifierPattern(SourceLocation location_, std::string value_);
    virtual ~IdentifierPattern();

    void accept(PatternVisitor* visitor) override;
};

struct IntegerPattern final : public Pattern
{
    std::string value;

    IntegerPattern(SourceLocation location_, std::string value_);
    virtual ~IntegerPattern();

    void accept(PatternVisitor* visitor) override;
};

struct FloatPattern final : public Pattern
{
    std::string value;

    FloatPattern(SourceLocation location_, std::string value_);
    virtual ~FloatPattern();

    void accept(PatternVisitor* visitor) override;
};

struct CharPattern final : public Pattern
{
    std::string value;

    CharPattern(SourceLocation location_, std::string value_);
    virtual ~CharPattern();

    void accept(PatternVisitor* visitor) override;
};

struct StringPattern final : public Pattern
{
    std::string value;

    StringPattern(SourceLocation location_, std::string value_);
    virtual ~StringPattern();

    void accept(PatternVisitor* visitor) override;
};

struct NamedArgumentPattern final : public Node
{
    std::string              name;
    std::unique_ptr<Pattern> pattern;

    NamedArgumentPattern(SourceLocation location_, std::string name, std::unique_ptr<Pattern> pattern);
    NamedArgumentPattern(NamedArgumentPattern&&) = default;
    virtual ~NamedArgumentPattern();
};

struct ConstructorPattern final : public Pattern
{
    GlobalIdentifier                  identifier;
    std::vector<NamedArgumentPattern> arguments;

    ConstructorPattern(SourceLocation location_, GlobalIdentifier identifier_, std::vector<NamedArgumentPattern> arguments);
    virtual ~ConstructorPattern();

    void accept(PatternVisitor* visitor) override;
};

struct Clause final
{
    std::unique_ptr<Pattern> pattern;
    std::unique_ptr<Exp>     exp;
};

struct CaseExp final : public Exp
{
    std::unique_ptr<Exp> caseExp;
    std::vector<Clause>  clauses;

    CaseExp(SourceLocation location_, std::unique_ptr<Exp> caseExp_, std::vector<Clause> clauses_);
    virtual ~CaseExp();

    void accept(ExpVisitor* visitor) override;
};

struct BinaryExp final : public Exp
{
    std::string          op;
    std::unique_ptr<Exp> lhs;
    std::unique_ptr<Exp> rhs;

    BinaryExp(SourceLocation location_, std::string op_, std::unique_ptr<Exp> lhs_, std::unique_ptr<Exp> rhs_);
    virtual ~BinaryExp();

    void accept(ExpVisitor* visitor) override;
};

struct UnaryExp final : public Exp
{
    std::string          op;
    std::unique_ptr<Exp> operand;

    UnaryExp(SourceLocation location_, std::string op_, std::unique_ptr<Exp> operand_);
    virtual ~UnaryExp();

    void accept(ExpVisitor* visitor) override;
};

struct LiteralExp final : public Exp
{
    lex::Token  tokenType; // remove this dependency
    std::string value;

    LiteralExp(SourceLocation location_, lex::Token tokenType_, std::string value_);
    virtual ~LiteralExp();

    void accept(ExpVisitor* visitor) override;
};

struct VariableExp final : public Exp
{
    GlobalIdentifier identifier;

    VariableExp(SourceLocation location_, GlobalIdentifier identifier_);
    virtual ~VariableExp();

    void accept(ExpVisitor* visitor) override;
};

struct CallExp final : public Exp
{
    GlobalIdentifier                  identifier;
    std::vector<std::unique_ptr<Exp>> arguments;

    CallExp(SourceLocation location_, GlobalIdentifier identifier_, std::vector<std::unique_ptr<Exp>> args);
    virtual ~CallExp();

    void accept(ExpVisitor* visitor) override;
};

struct FieldExp final : public Exp
{
    std::unique_ptr<Exp> lhs;
    std::string          fieldIdentifier;

    FieldExp(SourceLocation location_, std::unique_ptr<Exp> lhs_, std::string fieldIdentifier_);
    virtual ~FieldExp();

    void accept(ExpVisitor* visitor) override;
};

struct NamedArgument final : public Node
{
    std::string          name;
    std::unique_ptr<Exp> exp;

    NamedArgument(SourceLocation location_, std::string name_, std::unique_ptr<Exp> exp_);
    NamedArgument(NamedArgument&&) = default;
    virtual ~NamedArgument();
};

struct ConstructorExp final : public Exp
{
    GlobalIdentifier           identifier;
    std::vector<NamedArgument> arguments;

    ConstructorExp(SourceLocation location_, GlobalIdentifier identifier_, std::vector<NamedArgument> arguments_);
    virtual ~ConstructorExp();

    void accept(ExpVisitor* visitor) override;
};

struct IntrinsicExp final : public Exp
{
    std::string                       identifier_;
    std::vector<std::unique_ptr<Exp>> arguments_;

    IntrinsicExp(SourceLocation location, std::string identifier, std::vector<std::unique_ptr<Exp>> args);
    virtual ~IntrinsicExp();

    void accept(ExpVisitor* visitor) override;
};

} // namespace llfp::ast
