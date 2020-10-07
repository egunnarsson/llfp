
#include <memory>

#include "AstTinyPrint.h"


std::ostream& operator<<(std::ostream &os, const llfp::ast::Parameter &p) { return os << p.typeName << ' ' << p.identifier; }
std::ostream& operator<<(std::ostream &os, const llfp::ast::PublicDeclaration &p) { return os << p.name; }
std::ostream& operator<<(std::ostream &os, const llfp::ast::ImportDeclaration &i) { return os << i.name; }

class ExpPrint : public llfp::ast::ExpVisitor
{
    std::ostream &os;
    
    constexpr const char* litName(llfp::lex::Token t)
    {
        switch (t) {
            case llfp::lex::tok_integer: return "int";
            case llfp::lex::tok_float:   return "flt";
            case llfp::lex::tok_char:    return "char";
            case llfp::lex::tok_string:  return "str";
            case llfp::lex::tok_bool:    return "bool";
            default:                     return "XXX";
        }
    }
    
public:
    
    ExpPrint(std::ostream &os_) : os{os_} {}
    
    static std::ostream& visit(std::ostream &os, llfp::ast::Exp &e)
    {
        os << '(';
        ExpPrint ep(os);
        e.accept(&ep);
        os << ')';
        return os;
    }
    
    void visit(llfp::ast::LetExp &exp) override { os << "let " << exp.letStatments << " in " << exp.exp; }
    void visit(llfp::ast::IfExp &exp) override { os << "if " << exp.condition << " then " << exp.thenExp << " else " << exp.elseExp; }
    void visit(llfp::ast::CaseExp &exp) override { os << "case"; }
    void visit(llfp::ast::BinaryExp &exp) override { os << exp.op << ' ' << exp.lhs << ' ' << exp.rhs; }
    void visit(llfp::ast::UnaryExp &exp) override { os << exp.op << ' ' << exp.operand; }
    void visit(llfp::ast::LiteralExp &exp) override { os << litName(exp.tokenType) << ' ' << exp.value; }
    void visit(llfp::ast::CallExp &exp) override { os << exp.moduleName << ':' << exp.name << exp.arguments; }
    void visit(llfp::ast::VariableExp &exp) override { os << exp.moduleName << ':' << exp.name; }
};

std::ostream& operator<<(std::ostream &os, const llfp::ast::Field &f)
{
    return os << f.typeName << ' ' <<  f.identifier;
}

std::ostream& operator<<(std::ostream &os, const llfp::ast::DataDeclaration &d)
{
    return os << '{' << (d.exported ? '+' : '-') << d.name << ' ' << d.fields << '}';
}

std::ostream& operator<<(std::ostream &os, llfp::ast::Exp &e)
{
    return ExpPrint::visit(os, e);
}

std::ostream& operator<<(std::ostream &os, const llfp::ast::FunctionDeclaration &f)
{
    return os << '{' << (f.exported ? '+' : '-' ) << ' ' << f.typeName << ' ' << f.name << ' ' << f.parameters << f.functionBody << '}';
}

std::ostream& operator<<(std::ostream &os, const llfp::ast::Module &m)
{
    return os << '{' << m.name << ", " << m.publicDeclarations << ", " << m.imports << ", " << m.functionDeclarations << ", " << m.dataDeclarations << '}';
}
