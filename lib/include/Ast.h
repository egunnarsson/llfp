#pragma once

#include <memory>
#include <string>
#include <vector>

#include "Lexer.h"
#include "SourceLocation.h"


namespace llfp
{
namespace ast
{

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
public:

    SourceLocation location;

protected:

    Node(SourceLocation location_);
};

class Field : public Node
{
public:

    std::string typeName;
    std::string identifier;

    Field(SourceLocation location_, std::string typeName_, std::string identifier_);
    virtual ~Field();
};

class DataDeclaration : public Node
{
public:

    std::string        name;
    std::vector<Field> fields;
    bool               exported;

    DataDeclaration(SourceLocation location_, std::string name_, std::vector<Field> fields_, bool exported_);
    virtual ~DataDeclaration();
};

class InstanceDeclaration
{};

class ClassDeclaration
{};

class Exp : public Node
{
protected:

    Exp(SourceLocation location_);

public:

    virtual void accept(ExpVisitor *visitor) = 0;

    virtual ~Exp();
};

class Parameter : public Node
{
public:

    std::string typeName;
    std::string identifier;

    Parameter(SourceLocation location_, std::string typeName_, std::string identifier_);
    virtual ~Parameter();
};

class FunctionDeclaration : public Node
{
public:

    std::string          name;
    std::string          typeName;
    std::vector<std::unique_ptr<Parameter>> parameters;
    std::unique_ptr<Exp> functionBody;
    bool                 exported;

    FunctionDeclaration(
        SourceLocation location_,
        std::string name_,
        std::string typeName_,
        std::vector<std::unique_ptr<Parameter>> parameters_,
        std::unique_ptr<Exp> functionBody_,
        bool exported);
    virtual ~FunctionDeclaration();
};

class PublicDeclaration : public Node
{
public:

    std::string name;

    PublicDeclaration(SourceLocation location_, std::string name_);
    virtual ~PublicDeclaration();
};

class ImportDeclaration : public Node
{
public:

    std::string name;

    ImportDeclaration(SourceLocation location_, std::string name_);
    virtual ~ImportDeclaration();
};

class Module : public Node
{
public:

    std::string                    name;
    std::vector<PublicDeclaration> publicDeclarations;
    std::vector<ImportDeclaration> imports;
    std::vector<std::unique_ptr<DataDeclaration>>     dataDeclarations;
    std::vector<std::unique_ptr<FunctionDeclaration>> functionDeclarations;

    Module(SourceLocation location_, std::string name_);
    virtual ~Module();
};

class LetExp : public Exp
{
public:

    std::vector<std::unique_ptr<FunctionDeclaration>> letStatments;
    std::unique_ptr<Exp> exp;

    LetExp(SourceLocation location_, std::vector<std::unique_ptr<FunctionDeclaration>> letStatments_, std::unique_ptr<Exp> exp_);
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

    lex::Token  tokenType;
    std::string value;

    LiteralExp(SourceLocation location_, lex::Token tokenType_, std::string value_);
    virtual ~LiteralExp();

    void accept(ExpVisitor *visitor) override;
};

class VariableExp : public Exp
{
public:

    std::string moduleName;
    std::string name; // identifier

    VariableExp(SourceLocation location_, std::string moduleName, std::string name_);
    virtual ~VariableExp();

    void accept(ExpVisitor *visitor) override;
};

class CallExp : public Exp
{
public:

    std::string moduleName;
    std::string name;
    std::vector<std::unique_ptr<Exp>> arguments;

    CallExp(SourceLocation location_, std::string moduleName_, std::string name_, std::vector<std::unique_ptr<Exp>> args);
    virtual ~CallExp();

    void accept(ExpVisitor *visitor) override;
};


} // ast
} // llfp
