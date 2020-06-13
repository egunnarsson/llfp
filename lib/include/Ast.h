#pragma once

#include <memory>
#include <string>
#include <vector>

#include "Lexer.h"


namespace llfp
{
namespace ast
{

class Module;
class FunctionDeclaration;
class LetExp;
class IfExp;
class CaseExp;
class BinaryExp;
class UnaryExp;
class LiteralExp;
class CallExp;
class VariableExp;

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

protected: // ?

    virtual ~ExpVisitor() {}
};

class Node
{
    //SourceLocation Loc;
};

class DataDeclaration
{};

class InstanceDeclaration
{};

class ClassDeclaration
{};

class Exp : public Node
{
public:

    virtual void accept(ExpVisitor *visitor) = 0;

    virtual ~Exp();
};

class Parameter
{
public:

    std::string typeName;
    std::string identifier;

    Parameter(std::string typeName_, std::string identifier_) :
        typeName{ std::move(typeName_) },
        identifier{ std::move(identifier_) }
    {}
};

class FunctionDeclaration : public Node
{
public:

    std::string          identifier;
    std::string          typeName;
    std::vector<std::unique_ptr<Parameter>> parameters;
    std::unique_ptr<Exp> functionBody;
    //bool                 export;

    FunctionDeclaration(std::string identifier_,
        std::string typeName_,
        std::vector<std::unique_ptr<Parameter>> parameters_,
        std::unique_ptr<Exp> functionBody_);
    virtual ~FunctionDeclaration();
};

class Module : public Node
{
public:

    std::string identifier;
    std::vector<std::unique_ptr<FunctionDeclaration>> functionDeclarations;

    Module(std::string identifier_, std::vector<std::unique_ptr<FunctionDeclaration>> functionDeclarations_);
    virtual ~Module();
};

class LetExp : public Exp
{
public:

    std::vector<std::unique_ptr<FunctionDeclaration>> letStatments;
    std::unique_ptr<Exp> exp;

    LetExp(std::vector<std::unique_ptr<FunctionDeclaration>> letStatments_, std::unique_ptr<Exp> exp_);
    virtual ~LetExp();

    void accept(ExpVisitor *visitor) override;
};

class IfExp : public Exp
{
public:

    std::unique_ptr<Exp> condition;
    std::unique_ptr<Exp> thenExp;
    std::unique_ptr<Exp> elseExp;

    IfExp(std::unique_ptr<Exp> condition_, std::unique_ptr<Exp> thenExp_, std::unique_ptr<Exp> elseExp_);
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

    std::string operand;
    std::unique_ptr<Exp> lhs;
    std::unique_ptr<Exp> rhs;

    BinaryExp(std::string op, std::unique_ptr<Exp> lhs_, std::unique_ptr<Exp> rhs_);
    virtual ~BinaryExp();

    void accept(ExpVisitor *visitor) override;
};

class UnaryExp : public Exp
{
public:

    std::string op;
    std::unique_ptr<Exp> operand;

    UnaryExp(std::string op_, std::unique_ptr<Exp> operand_);
    virtual ~UnaryExp();

    void accept(ExpVisitor *visitor) override;
};

class LiteralExp : public Exp
{
public:

    lex::Token tokenType;
    std::string value;

    LiteralExp(lex::Token tokenType_, std::string value_);
    virtual ~LiteralExp();

    void accept(ExpVisitor *visitor) override;
};

class VariableExp : public Exp
{
public:

    std::string identifier;

    VariableExp(std::string id);
    virtual ~VariableExp();

    void accept(ExpVisitor *visitor) override;
};

class CallExp : public Exp
{
public:

    std::string identifier;
    std::vector<std::unique_ptr<Exp>> arguments;

    CallExp(std::string id, std::vector<std::unique_ptr<Exp>> args);
    virtual ~CallExp();

    void accept(ExpVisitor *visitor) override;
};


} // ast
} // llfp
