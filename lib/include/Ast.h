#pragma once

#include <memory>
#include <string>
#include <vector>

#include "Common.h"
#include "Lexer.h"


namespace llfp
{
namespace ast
{

struct TypeIdentifier
{
    // location?

    GlobalIdentifier            identifier;
    std::vector<TypeIdentifier> parameters;

    std::string str() const;
    bool        empty() const;
};

class LetExp;
class IfExp;
class CaseExp;
class BinaryExp;
class UnaryExp;
class LiteralExp;
class CallExp;
class VariableExp;
class FieldExp;
class ConstructorExp;

class ExpVisitor
{
public:

    virtual void visit(LetExp &exp) = 0;
    virtual void visit(IfExp &exp) = 0;
    virtual void visit(CaseExp &exp) = 0;
    virtual void visit(BinaryExp &exp) = 0;
    virtual void visit(UnaryExp &exp) = 0;
    virtual void visit(LiteralExp &exp) = 0;
    virtual void visit(CallExp &exp) = 0;
    virtual void visit(VariableExp &exp) = 0;
    virtual void visit(FieldExp &exp) = 0;
    virtual void visit(ConstructorExp &exp) = 0;

protected: // ?

    virtual ~ExpVisitor() {}
};

class Node
{
public:

    SourceLocation location;

protected:

    Node(SourceLocation location_);
};

class Field : public Node
{
public:

    TypeIdentifier type;
    std::string    name;

    Field(SourceLocation location_, TypeIdentifier type_, std::string name_);
    virtual ~Field();
};

class Data : public Node
{
public:

    std::string              name;
    std::vector<std::string> typeVariables;
    std::vector<Field>       fields;
    bool                     exported;

    Data(
        SourceLocation location_,
        std::string name_,
        std::vector<std::string> typeVariables_,
        std::vector<Field> fields_,
        bool exported_);
    virtual ~Data();
};

class Parameter : public Node
{
public:

    TypeIdentifier type;
    std::string    identifier; // or name?

    Parameter(SourceLocation location_, TypeIdentifier type_, std::string identifier_);
    virtual ~Parameter();
};

class Exp : public Node
{
protected:

    Exp(SourceLocation location_);

public:

    virtual void accept(ExpVisitor *visitor) = 0;

    virtual ~Exp();
};
/*
enum FunctionType
{
    Exported,
    Local,
    Instance
};
*/
class Function : public Node
{
public:

    std::string          name;
    TypeIdentifier       type;
    std::vector<std::unique_ptr<Parameter>> parameters;
    std::unique_ptr<Exp> functionBody; // rename body
    bool                 exported;

    Function(
        SourceLocation location_,
        std::string name_,
        TypeIdentifier type_,
        std::vector<std::unique_ptr<Parameter>> parameters_,
        std::unique_ptr<Exp> functionBody_,
        bool exported);
    virtual ~Function();
};

class FunctionDeclaration : public Node
{
public:

    std::string    name;
    TypeIdentifier type;
    std::vector<std::unique_ptr<Parameter>> parameters;

    FunctionDeclaration(
        SourceLocation location_,
        std::string name_,
        TypeIdentifier type_,
        std::vector<std::unique_ptr<Parameter>> parameters_);
    virtual ~FunctionDeclaration();
};

class Class : public Node
{
public:

    std::string name;
    std::string typeVariable;
    std::vector<std::unique_ptr<FunctionDeclaration>> functions;

    Class(
        SourceLocation location_,
        std::string name_,
        std::string typeVariable_,
        std::vector<std::unique_ptr<FunctionDeclaration>> functions_);
    virtual ~Class();
};

class ClassInstance : public Node
{
public:

    GlobalIdentifier classIdentifier;
    TypeIdentifier   typeArgument;
    std::vector<std::unique_ptr<Function>> functions;

    ClassInstance(
        SourceLocation location_,
        GlobalIdentifier classIdentifier_,
        TypeIdentifier typeArgument_,
        std::vector<std::unique_ptr<Function>> functions_);
    virtual ~ClassInstance();
};

class Public : public Node
{
public:

    std::string name;

    Public(SourceLocation location_, std::string name_);
    virtual ~Public();
};

class Import : public Node
{
public:

    std::string name;

    Import(SourceLocation location_, std::string name_);
    virtual ~Import();
};

class Module : public Node
{
public:

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

class LetExp : public Exp
{
public:

    std::vector<std::unique_ptr<Function>> letStatments;
    std::unique_ptr<Exp> exp;

    LetExp(SourceLocation location_, std::vector<std::unique_ptr<Function>> letStatments_, std::unique_ptr<Exp> exp_);
    virtual ~LetExp();

    void accept(ExpVisitor *visitor) override;
};

class IfExp : public Exp
{
public:

    std::unique_ptr<Exp> condition;
    std::unique_ptr<Exp> thenExp;
    std::unique_ptr<Exp> elseExp;

    IfExp(SourceLocation location_, std::unique_ptr<Exp> condition_, std::unique_ptr<Exp> thenExp_, std::unique_ptr<Exp> elseExp_);
    virtual ~IfExp();

    void accept(ExpVisitor *visitor) override;
};

class CaseExp : public Exp
{
public:

    std::unique_ptr<Exp> caseExp;

    virtual ~CaseExp();

    void accept(ExpVisitor *visitor) override;
};

class BinaryExp : public Exp
{
public:

    std::string          op;
    std::unique_ptr<Exp> lhs;
    std::unique_ptr<Exp> rhs;

    BinaryExp(SourceLocation location_, std::string op_, std::unique_ptr<Exp> lhs_, std::unique_ptr<Exp> rhs_);
    virtual ~BinaryExp();

    void accept(ExpVisitor *visitor) override;
};

class UnaryExp : public Exp
{
public:

    std::string          op;
    std::unique_ptr<Exp> operand;

    UnaryExp(SourceLocation location_, std::string op_, std::unique_ptr<Exp> operand_);
    virtual ~UnaryExp();

    void accept(ExpVisitor *visitor) override;
};

class LiteralExp : public Exp
{
public:

    lex::Token  tokenType; // remove this dependency
    std::string value;

    LiteralExp(SourceLocation location_, lex::Token tokenType_, std::string value_);
    virtual ~LiteralExp();

    void accept(ExpVisitor *visitor) override;
};

class VariableExp : public Exp
{
public:

    GlobalIdentifier identifier;

    VariableExp(SourceLocation location_, GlobalIdentifier identifier_);
    virtual ~VariableExp();

    void accept(ExpVisitor *visitor) override;
};

class CallExp : public Exp
{
public:

    GlobalIdentifier identifier;
    std::vector<std::unique_ptr<Exp>> arguments;

    CallExp(SourceLocation location_, GlobalIdentifier identifier_, std::vector<std::unique_ptr<Exp>> args);
    virtual ~CallExp();

    void accept(ExpVisitor *visitor) override;
};

class FieldExp : public Exp
{
public:

    std::unique_ptr<Exp> lhs;
    std::string          fieldIdentifier;

    FieldExp(SourceLocation location_, std::unique_ptr<Exp> lhs_, std::string fieldIdentifier_);
    virtual ~FieldExp();

    void accept(ExpVisitor *visitor) override;
};

class NamedArgument : public Node
{
public:

    std::string          name;
    std::unique_ptr<Exp> exp;

    NamedArgument(SourceLocation location_, std::string name_, std::unique_ptr<Exp> exp_);
    virtual ~NamedArgument();
};

class ConstructorExp : public Exp
{
public:

    GlobalIdentifier identifier;
    std::vector<std::unique_ptr<NamedArgument>> arguments; // do not need to be ptrs?

    ConstructorExp(SourceLocation location_, GlobalIdentifier identifier_, std::vector<std::unique_ptr<NamedArgument>> arguments_);
    virtual ~ConstructorExp();

    void accept(ExpVisitor *visitor) override;
};

} // ast
} // llfp
