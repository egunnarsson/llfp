
#include <memory>
#include <type_traits>

#include "AstEquality.h"

using namespace llfp::ast;

namespace
{

template<class T>
bool EqPtrV(const std::vector<std::unique_ptr<T>> &v1, const std::vector<std::unique_ptr<T>> &v2)
{
    if (v1.size() != v2.size())
    {
        return false;
    }
    
    for (size_t i = 0; i < v1.size(); ++i)
    {
        if (v1[i] == nullptr)
        {
            if (v2[i] != nullptr)
            {
                return false;
            }
        }
        else if (v2[i] == nullptr)
        {
            return false;
        }
        if (!(*v1[i] == *v2[i]))
        {
            return false;
        }
    }
    
    return true;
}

template<class T>
bool EqV(const std::vector<T> &v1, const std::vector<T> &v2)
{
    if (v1.size() != v2.size())
    {
        return false;
    }
    
    for (size_t i = 0; i < v1.size(); ++i)
    {
        if (!(v1[i] == v2[i]))
        {
            return false;
        }
    }
    
    return true;
}

template<class T, class U, bool Same>
struct CompareExp
{
    static bool compare(const T&, const U&)
    {
        return false;
    }
};

template<class T, class U>
struct CompareExp<T, U, true>
{
    static bool compare(const T &exp1, const U &exp2)
    {
        return exp1 == exp2;
    }
};

#define VISIT(type) void visit(type &exp) override { visit_(exp); }

template<class T>
class ExpEqT : public ExpVisitor
{
public:
    T*   exp_;
    bool result = false;
    
    ExpEqT(T *exp):
        exp_(exp)
    {}
    
    template<class U>
    void visit_(U &exp)
    {
        // probably an easier way, enable_if ?
        result = CompareExp<T, U, std::is_same<T, U>::value>::compare(*exp_, exp);
    }
    
    VISIT(LetExp)
    VISIT(IfExp)
    VISIT(CaseExp)
    VISIT(BinaryExp)
    VISIT(UnaryExp)
    VISIT(LiteralExp)
    VISIT(CallExp)
    VISIT(VariableExp)
    VISIT(FieldExp)
    VISIT(ConstructorExp)
};

class ExpEq : public ExpVisitor
{
public:
    Exp* exp_;
    bool result = false;
    
    ExpEq(Exp *exp):
        exp_(exp)
    {}
    
    static bool check(Exp *e1, Exp *e2)
    {
        if (e1 == nullptr)
        {
            return e2 == nullptr;
        }
        if (e2 != nullptr)
        {
            ExpEq eq(e1);
            e2->accept(&eq);
            return eq.result;
        }
        else
        {
            return false;
        }
    }
    
    static bool check(const std::unique_ptr<Exp>& e1, const std::unique_ptr<Exp>& e2)
    {
        return check(e1.get(), e2.get());
    }
    
    template<class T>
    void visit_(T &exp)
    {
        ExpEqT<T> eq(&exp);
        exp_->accept(&eq);
        result = eq.result;
    }
    
    VISIT(LetExp)
    VISIT(IfExp)
    VISIT(CaseExp)
    VISIT(BinaryExp)
    VISIT(UnaryExp)
    VISIT(LiteralExp)
    VISIT(CallExp)
    VISIT(VariableExp)
    VISIT(FieldExp)
    VISIT(ConstructorExp)
};

#undef VISIT

}

namespace llfp::ast
{

bool operator==(const TypeIdentifier& t1, const TypeIdentifier& t2)
{
    return t1.identifier == t2.identifier && EqV(t1.parameters, t2.parameters);
}

bool operator==(Exp &e1, Exp &e2)
{
    return ExpEq::check(&e1, &e2);
}

bool operator==(const LetExp &e1, const LetExp &e2)
{
    return ExpEq::check(e1.exp, e2.exp) &&
           EqPtrV(e1.letStatments, e2.letStatments);
}

bool operator==(const IfExp &e1, const IfExp &e2)
{
    return ExpEq::check(e1.condition, e2.condition) &&
           ExpEq::check(e1.elseExp, e2.elseExp) &&
           ExpEq::check(e1.thenExp, e2.thenExp);
}

bool operator==(const CaseExp &, const CaseExp &)
{
    return false;
}

bool operator==(const BinaryExp &e1, const BinaryExp &e2)
{
    return e1.op == e2.op &&
           ExpEq::check(e1.lhs, e2.lhs) &&
           ExpEq::check(e1.rhs, e2.rhs);
}

bool operator==(const UnaryExp &e1, const UnaryExp &e2)
{
    return e1.op == e2.op &&
           ExpEq::check(e1.operand, e2.operand);
}

bool operator==(const LiteralExp &e1, const LiteralExp &e2)
{
    return e1.tokenType == e2.tokenType &&
           e1.value == e2.value;
}

bool operator==(const CallExp &e1, const CallExp &e2)
{
    return e1.identifier == e2.identifier &&
           EqPtrV(e1.arguments, e2.arguments);
}

bool operator==(const VariableExp &e1, const VariableExp &e2)
{
    return e1.identifier == e2.identifier;
}

bool operator==(const ConstructorExp &e1, const ConstructorExp &e2)
{
    return e1.identifier == e2.identifier && EqPtrV(e1.arguments, e2.arguments);
}

bool operator==(const FieldExp &e1, const FieldExp &e2)
{
    return e1.fieldIdentifier == e2.fieldIdentifier && ExpEq::check(e1.lhs, e2.lhs);
}

bool operator==(const Parameter &p1, const Parameter &p2)
{
    return p1.type == p2.type && p1.identifier == p2.identifier;
}

bool operator==(const FunctionDecl& f1, const FunctionDecl& f2)
{
    return f1.name == f2.name && f1.type == f2.type && EqPtrV(f1.parameters, f2.parameters);
}

bool operator==(const ClassDeclaration& c1, const ClassDeclaration& c2)
{
    return c1.name == c2.name &&
        c1.typeVariable == c2.typeVariable &&
        EqPtrV(c1.functions, c2.functions);
}

bool operator==(const ClassInstance& c1, const ClassInstance& c2)
{
    return c1.classIdentifier == c2.classIdentifier &&
        c1.typeArgument == c2.typeArgument &&
        EqPtrV(c1.functions, c2.functions);
}

bool operator==(const ImportDeclaration &i1, const ImportDeclaration &i2)
{
    return i1.name == i2.name;
}

bool operator==(const PublicDeclaration &p1, const PublicDeclaration &p2)
{
    return p1.name == p2.name;
}

bool operator==(const NamedArgument &n1, const NamedArgument &n2)
{
    return n1.name == n2.name && ExpEq::check(n1.exp, n2.exp);
}

bool operator==(const Field &f1, const Field &f2)
{
    return f1.type == f2.type && f1.name == f2.name;
}

bool operator==(const DataDeclaration &d1, const DataDeclaration &d2)
{
    return d1.name == d2.name && EqV(d1.fields, d2.fields) && d1.exported == d2.exported;
}

bool operator==(const Function &f1, const Function &f2)
{
    return f1.name == f2.name &&
           f1.type == f2.type &&
           EqPtrV(f1.parameters, f2.parameters) &&
           ExpEq::check(f1.functionBody, f2.functionBody) &&
           f1.exported == f2.exported;
}

bool operator==(const Module &m1, const Module &m2)
{
    return m1.name == m2.name &&
           EqV(m1.publicDeclarations, m2.publicDeclarations) &&
           EqV(m1.imports, m2.imports) &&
           EqPtrV(m1.functionDeclarations, m2.functionDeclarations) &&
           EqPtrV(m1.classDeclarations, m2.classDeclarations) &&
           EqPtrV(m1.instanceDeclarations, m2.instanceDeclarations);
}

} // llfp::ast
