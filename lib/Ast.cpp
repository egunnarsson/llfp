
#include "Ast.h"


namespace llfp
{
namespace ast
{

Node::Node(SourceLocation location_) :
    location{ location_ }
{}


Field::Field(SourceLocation location_, GlobalIdentifier type_, std::string name_) :
    Node(location_),
    type{ std::move(type_) },
    name{ std::move(name_) }
{
}

Field::~Field() {}


DataDeclaration::DataDeclaration(SourceLocation location_, std::string name_, std::vector<Field> fields_, bool exported_) :
    Node(location_),
    name{ std::move(name_) },
    fields{ std::move(fields_) },
    exported{ exported_ }
{
}

DataDeclaration::~DataDeclaration() {}


Exp::Exp(SourceLocation location_) : Node(location_) {}
Exp::~Exp() {}


Parameter::Parameter(SourceLocation location_, GlobalIdentifier type_, std::string identifier_) :
    Node(location_),
    type{ std::move(type_) },
    identifier{ std::move(identifier_) }
{}

Parameter::~Parameter() {}


FunctionDeclaration::FunctionDeclaration(
    SourceLocation location_,
    std::string name_,
    GlobalIdentifier type_,
    std::vector<std::unique_ptr<Parameter>> parameters_,
    std::unique_ptr<Exp> functionBody_,
    bool exported_) :

    Node(location_),
    name{ std::move(name_) },
    type{ std::move(type_) },
    parameters{ std::move(parameters_) },
    functionBody{ std::move(functionBody_) },
    exported{ exported_ }
{}

FunctionDeclaration::~FunctionDeclaration() {}


PublicDeclaration::PublicDeclaration(SourceLocation location_, std::string name_) :
    Node(location_),
    name{ std::move(name_) }
{}

PublicDeclaration::~PublicDeclaration() {}


ImportDeclaration::ImportDeclaration(SourceLocation location_, std::string name_) :
    Node(location_),
    name{ std::move(name_) }
{}

ImportDeclaration::~ImportDeclaration() {}


Module::Module(SourceLocation location_, std::string name_) :
    Node(location_),
    name{ std::move(name_) }
{}

Module::~Module() {}


LetExp::LetExp(SourceLocation location_, std::vector<std::unique_ptr<FunctionDeclaration>> letStatments_, std::unique_ptr<Exp> exp_) :
    Exp(location_),
    letStatments{ std::move(letStatments_) },
    exp{ std::move(exp_) }
{}

LetExp::~LetExp() {}

void LetExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }


IfExp::IfExp(
    SourceLocation location_,
    std::unique_ptr<Exp> condition_,
    std::unique_ptr<Exp> thenExp_,
    std::unique_ptr<Exp> elseExp_) :

    Exp(location_),
    condition{ std::move(condition_) },
    thenExp{ std::move(thenExp_) },
    elseExp{ std::move(elseExp_) }
{}

IfExp::~IfExp() {}

void IfExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }


CaseExp::~CaseExp() {}

void CaseExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }


BinaryExp::BinaryExp(SourceLocation location_, std::string op_, std::unique_ptr<Exp> lhs_, std::unique_ptr<Exp> rhs_) :
    Exp(location_),
    op{ std::move(op_) },
    lhs{ std::move(lhs_) },
    rhs{ std::move(rhs_) }
{}

BinaryExp::~BinaryExp() {}

void BinaryExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }


UnaryExp::UnaryExp(SourceLocation location_, std::string op_, std::unique_ptr<Exp> operand_) :
    Exp(location_),
    op{ std::move(op_) },
    operand{ std::move(operand_) }
{}

UnaryExp::~UnaryExp() {}

void UnaryExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }


LiteralExp::LiteralExp(SourceLocation location_, lex::Token tokenType_, std::string value_) :
    Exp(location_),
    tokenType{ tokenType_ },
    value{ std::move(value_) }
{}

LiteralExp::~LiteralExp() {}

void LiteralExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }


VariableExp::VariableExp(SourceLocation location_, GlobalIdentifier identifier_) :
    Exp(location_),
    identifier{ std::move(identifier_) }
{}

VariableExp::~VariableExp() {}

void VariableExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }


CallExp::CallExp(
    SourceLocation location_,
    GlobalIdentifier identifier_,
    std::vector<std::unique_ptr<Exp>> args) :

    Exp(location_),
    identifier{ std::move(identifier_) },
    arguments{ std::move(args) }
{}

CallExp::~CallExp() {}

void CallExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }


FieldExp::FieldExp(SourceLocation location_, std::unique_ptr<Exp> lhs_, std::string fieldIdentifier_) :
    Exp(location_),
    lhs{ std::move(lhs_) },
    fieldIdentifier{ std::move(fieldIdentifier_) }
{}

FieldExp::~FieldExp() {}

void FieldExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }


NamedArgument::NamedArgument(SourceLocation location_, std::string name_, std::unique_ptr<Exp> exp_) :
    Node(location_),
    name(std::move(name_)),
    exp(std::move(exp_))
{}

NamedArgument::~NamedArgument() {}


ConstructorExp::ConstructorExp(SourceLocation location_, GlobalIdentifier identifier_, std::vector<std::unique_ptr<NamedArgument>> arguments_) :
    Exp(location_),
    identifier(std::move(identifier_)),
    arguments(std::move(arguments_))
{}

ConstructorExp::~ConstructorExp() {}
void ConstructorExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }


} // namespace ast
} // namespace llfp

