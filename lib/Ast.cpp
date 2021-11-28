
#include "Ast.h"


namespace llfp
{
namespace ast
{

std::string TypeIdentifier::str() const
{
    std::string result = identifier.str();

    if (!parameters.empty())
    {
        result += '[';
        for (auto& arg : parameters)
        {
            result += arg.str();
            result += ',';
        }
        result.back() = ']';
    }

    return result;
}

bool TypeIdentifier::empty() const
{
    return identifier.moduleName.empty() && identifier.name.empty() && parameters.empty();
}


Node::Node(SourceLocation location_) :
    location{ location_ }
{}


Field::Field(SourceLocation location_, TypeIdentifier type_, std::string name_) :
    Node(location_),
    type{ std::move(type_) },
    name{ std::move(name_) }
{}

Field::~Field() {}


Data::Data(
    SourceLocation location_,
    std::string name_,
    std::vector<std::string> typeVariables_,
    std::vector<Field> fields_,
    bool exported_) :

    Node(location_),
    name{ std::move(name_) },
    typeVariables{ std::move(typeVariables_) },
    fields{ std::move(fields_) },
    exported{ exported_ }
{}

Data::~Data() {}


Parameter::Parameter(SourceLocation location_, TypeIdentifier type_, std::string identifier_) :
    Node(location_),
    type{ std::move(type_) },
    identifier{ std::move(identifier_) }
{}

Parameter::~Parameter() {}


Exp::Exp(SourceLocation location_) : Node(location_) {}
Exp::~Exp() {}


Function::Function(
    SourceLocation location_,
    std::string name_,
    TypeIdentifier type_,
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

Function::~Function() {}


FunctionDeclaration::FunctionDeclaration(
    SourceLocation location_,
    std::string name_,
    TypeIdentifier type_,
    std::vector<std::unique_ptr<Parameter>> parameters_) :

    Node(location_),
    name(std::move(name_)),
    type(std::move(type_)),
    parameters(std::move(parameters_))
{}

FunctionDeclaration::~FunctionDeclaration() {}


Class::Class(
    SourceLocation location_,
    std::string name_,
    std::string typeVariable_,
    std::vector<std::unique_ptr<FunctionDeclaration>> functions_) :

    Node(location_),
    name(std::move(name_)),
    typeVariable(std::move(typeVariable_)),
    functions(std::move(functions_))
{}

Class::~Class() {}


ClassInstance::ClassInstance(
    SourceLocation location_,
    GlobalIdentifier classIdentifier_,
    TypeIdentifier typeArgument_,
    std::vector<std::unique_ptr<Function>> functions_) :

    Node(location_),
    classIdentifier(std::move(classIdentifier_)),
    typeArgument(std::move(typeArgument_)),
    functions(std::move(functions_))
{}

ClassInstance::~ClassInstance() {}


Public::Public(SourceLocation location_, std::string name_) :
    Node(location_),
    name{ std::move(name_) }
{}

Public::~Public() {}


Import::Import(SourceLocation location_, std::string name_) :
    Node(location_),
    name{ std::move(name_) }
{}

Import::~Import() {}


Module::Module(SourceLocation location_, std::string name_) :
    Node(location_),
    name{ std::move(name_) }
{}

Module::~Module() {}


LetExp::LetExp(SourceLocation location_, std::vector<std::unique_ptr<Function>> letStatments_, std::unique_ptr<Exp> exp_) :
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

