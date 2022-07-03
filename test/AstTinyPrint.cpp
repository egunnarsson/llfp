
#include <memory>

#include "AstTinyPrint.h"


class ExpPrint : public llfp::ast::ExpVisitor
{
    std::ostream &os;
    
    constexpr const char* litName(llfp::lex::Token t)
    {
        switch (t) {
            case llfp::lex::Token::Integer: return "int";
            case llfp::lex::Token::Float:   return "flt";
            case llfp::lex::Token::Char:    return "char";
            case llfp::lex::Token::String:  return "str";
            case llfp::lex::Token::Bool:    return "bool";
            default:                        return "XXX";
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
    void visit(llfp::ast::CaseExp &exp) override { os << "case " << exp.caseExp << exp.clauses; }
    void visit(llfp::ast::BinaryExp &exp) override { os << exp.op << ' ' << exp.lhs << ' ' << exp.rhs; }
    void visit(llfp::ast::UnaryExp &exp) override { os << exp.op << ' ' << exp.operand; }
    void visit(llfp::ast::LiteralExp &exp) override { os << litName(exp.tokenType) << ' ' << exp.value; }
    void visit(llfp::ast::CallExp &exp) override { os << exp.identifier << exp.arguments; }
    void visit(llfp::ast::VariableExp &exp) override { os << exp.identifier; }
    void visit(llfp::ast::FieldExp &exp) override { os << exp.lhs << '.' << exp.fieldIdentifier; }
    void visit(llfp::ast::ConstructorExp &exp) override { os << exp.identifier << exp.arguments; }
};

class PatternPrint : public llfp::ast::PatternVisitor
{
    std::ostream& os;

public:

    PatternPrint(std::ostream& os_) : os{ os_ } {}

    static std::ostream& visit(std::ostream& os, llfp::ast::Pattern& p)
    {
        PatternPrint ep(os);
        p.accept(&ep);
        return os;
    }

    void visit(llfp::ast::BoolPattern& pat) override { os << (pat.value ? "true" : "false"); }
    void visit(llfp::ast::IdentifierPattern& pat) override { os << pat.value; }
    void visit(llfp::ast::IntegerPattern& pat) override { os << pat.value; }
    void visit(llfp::ast::FloatPattern& pat) override { os << pat.value; }
    void visit(llfp::ast::CharPattern& pat) override { os << '\'' << pat.value << '\''; }
    void visit(llfp::ast::StringPattern& pat) override { os << '"' << pat.value << '"'; }
    void visit(llfp::ast::ConstructorPattern& pat) override { os << pat.identifier << '{' << pat.arguments << '}'; }
};

std::ostream& operator<<(std::ostream& os, const llfp::GlobalIdentifier& i) { return os << i.str(); }

std::ostream& operator<<(std::ostream& os, const llfp::ast::TypeIdentifier& t)
{
    return os << t.identifier << t.parameters;
}

std::ostream& operator<<(std::ostream& os, const llfp::ast::Field& f) { return os << f.type << ' ' << f.name; }

std::ostream& operator<<(std::ostream& os, const llfp::ast::DataConstructor& d)
{
    return os << d.name << d.fields;
}

std::ostream& operator<<(std::ostream& os, const llfp::ast::Data& d)
{
    return os << '{' << (d.exported ? '+' : '-') << d.name << ' ' << d.constructors << '}';
}

std::ostream& operator<<(std::ostream& os, llfp::ast::Exp& e) { return ExpPrint::visit(os, e); }

std::ostream& operator<<(std::ostream& os, const llfp::ast::Parameter& p) { return os << p.type << ' ' << p.identifier; }

std::ostream& operator<<(std::ostream& os, const llfp::ast::Function& f)
{
    return os << '{' << (f.exported ? '+' : '-') << ' ' << f.type << ' ' << f.name << ' ' << f.parameters << f.functionBody << '}';
}

std::ostream& operator<<(std::ostream& os, const llfp::ast::FunctionDeclaration& f) { return os << '{' << f.name << f.type << f.parameters << '}'; }

std::ostream& operator<<(std::ostream& os, const llfp::ast::Class& f) { return os << '{' << f.name << f.typeVariable << f.functions << '}'; }

std::ostream& operator<<(std::ostream& os, const llfp::ast::ClassInstance& f)
{
    return os << '{' << f.classIdentifier << f.typeArgument << f.functions << '}';
}

std::ostream& operator<<(std::ostream& os, const llfp::ast::Public& p) { return os << p.name; }

std::ostream& operator<<(std::ostream& os, const llfp::ast::Import& i) { return os << i.name; }

std::ostream& operator<<(std::ostream &os, const llfp::ast::Module &m)
{
    return os << '{' << m.name << ", " << m.publics << ", " << m.imports << ", " << m.functions << ", " << m.datas << ", " << m.classes << ", "  << m.classInstances << '}';
}

std::ostream& operator<<(std::ostream& os, const llfp::ast::NamedArgument& i) { return os << i.name << " = " << i.exp; }

std::ostream& operator<<(std::ostream& os, llfp::ast::Pattern& p) { return PatternPrint::visit(os, p); }

std::ostream& operator<<(std::ostream& os, const llfp::ast::Clause& c) { return os << c.pattern << " -> " << c.exp; }

std::ostream& operator<<(std::ostream& os, const llfp::ast::NamedArgumentPattern& n) { return os << n.name << " = " << n.pattern; }