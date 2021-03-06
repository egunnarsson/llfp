
#include <memory>

#include "AstTinyPrint.h"


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
    void visit(llfp::ast::CaseExp &) override { os << "case"; }
    void visit(llfp::ast::BinaryExp &exp) override { os << exp.op << ' ' << exp.lhs << ' ' << exp.rhs; }
    void visit(llfp::ast::UnaryExp &exp) override { os << exp.op << ' ' << exp.operand; }
    void visit(llfp::ast::LiteralExp &exp) override { os << litName(exp.tokenType) << ' ' << exp.value; }
    void visit(llfp::ast::CallExp &exp) override { os << exp.identifier << exp.arguments; }
    void visit(llfp::ast::VariableExp &exp) override { os << exp.identifier; }
    void visit(llfp::ast::FieldExp &exp) override { os << exp.lhs << '.' << exp.fieldIdentifier; }
    void visit(llfp::ast::ConstructorExp &exp) override { os << exp.identifier << exp.arguments; }
};

std::ostream& operator<<(std::ostream& os, const llfp::GlobalIdentifier& i) { return os << i.str(); }

std::ostream& operator<<(std::ostream& os, const llfp::ast::TypeIdentifier& t)
{
    return os << t.identifier << t.parameters;
}

std::ostream& operator<<(std::ostream& os, const llfp::ast::Field& f) { return os << f.type << ' ' << f.name; }

std::ostream& operator<<(std::ostream& os, const llfp::ast::DataDeclaration& d)
{
    return os << '{' << (d.exported ? '+' : '-') << d.name << ' ' << d.fields << '}';
}

std::ostream& operator<<(std::ostream& os, llfp::ast::Exp& e) { return ExpPrint::visit(os, e); }

std::ostream& operator<<(std::ostream& os, const llfp::ast::Parameter& p) { return os << p.type << ' ' << p.identifier; }

std::ostream& operator<<(std::ostream& os, const llfp::ast::Function& f)
{
    return os << '{' << (f.exported ? '+' : '-') << ' ' << f.type << ' ' << f.name << ' ' << f.parameters << f.functionBody << '}';
}

std::ostream& operator<<(std::ostream& os, const llfp::ast::FunctionDecl& f) { return os << '{' << f.name << f.type << f.parameters << '}'; }

std::ostream& operator<<(std::ostream& os, const llfp::ast::ClassDeclaration& f) { return os << '{' << f.name << f.typeVariable << f.functions << '}'; }

std::ostream& operator<<(std::ostream& os, const llfp::ast::ClassInstance& f)
{
    return os << '{' << f.classIdentifier << f.typeArgument << f.functions << '}';
}

std::ostream& operator<<(std::ostream& os, const llfp::ast::PublicDeclaration& p) { return os << p.name; }

std::ostream& operator<<(std::ostream& os, const llfp::ast::ImportDeclaration& i) { return os << i.name; }

std::ostream& operator<<(std::ostream &os, const llfp::ast::Module &m)
{
    return os << '{' << m.name << ", " << m.publicDeclarations << ", " << m.imports << ", " << m.functionDeclarations << ", " << m.dataDeclarations << '}';
}

std::ostream& operator<<(std::ostream& os, const llfp::ast::NamedArgument& i) { return os << i.name << " = " << i.exp; }
