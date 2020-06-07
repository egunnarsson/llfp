
#include "Ast.h"


namespace llfp
{
namespace ast
{

Exp::~Exp() {}

FunctionDeclaration::FunctionDeclaration(std::string identifier_,
    std::string typeName_,
    std::vector<std::unique_ptr<Parameter>> parameters_,
    std::unique_ptr<Exp> functionBody_) :

    identifier{ std::move(identifier_) },
    typeName{ std::move(typeName_) },
    parameters{ std::move(parameters_) },
    functionBody{ std::move(functionBody_) }
{}

FunctionDeclaration::~FunctionDeclaration() {}

Module::Module(std::string identifier_, std::vector<std::unique_ptr<FunctionDeclaration>> functionDeclarations_) :
    identifier{ std::move(identifier_) },
    functionDeclarations{ std::move(functionDeclarations_) }
{}

Module::~Module() {}

LetExp::LetExp(std::vector<std::unique_ptr<FunctionDeclaration>> letStatments_, std::unique_ptr<Exp> exp_) :
    letStatments{ std::move(letStatments_) },
    exp{ std::move(exp_) }
{}

LetExp::~LetExp() {}

void LetExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }

IfExp::IfExp(std::unique_ptr<Exp> condition_, std::unique_ptr<Exp> thenExp_, std::unique_ptr<Exp> elseExp_) :
    condition{ std::move(condition_) },
    thenExp{ std::move(thenExp_) },
    elseExp{ std::move(elseExp_) }
{}

IfExp::~IfExp() {}

void IfExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }

CaseExp::~CaseExp() {}

void CaseExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }

BinaryExp::BinaryExp(std::string op, std::unique_ptr<Exp> lhs_, std::unique_ptr<Exp> rhs_) :
    operand{ std::move(op) },
    lhs{ std::move(lhs_) },
    rhs{ std::move(rhs_) }
{}

BinaryExp::~BinaryExp() {}

void BinaryExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }

UnaryExp::UnaryExp(std::string op_, std::unique_ptr<Exp> operand_) :
    op{ std::move(op_) },
    operand{ std::move(operand_) }
{}

UnaryExp::~UnaryExp() {}

void UnaryExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }

LiteralExp::LiteralExp(lex::Token tokenType_, std::string value_) :
    tokenType{ tokenType_ },
    value{ std::move(value_) }
{}

LiteralExp::~LiteralExp() {}

void LiteralExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }

VariableExp::VariableExp(std::string id) :
    identifier{ std::move(id) }
{}

VariableExp::~VariableExp() {}

void VariableExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }

CallExp::CallExp(std::string id, std::vector<std::unique_ptr<Exp>> args) :
    identifier{ std::move(id) },
    arguments{ std::move(args) }
{}

CallExp::~CallExp() {}

void CallExp::accept(ExpVisitor *visitor) { visitor->visit(*this); }

} // namespace ast
} // namespace llfp

