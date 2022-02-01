
#include "Error.h"

#include "Type/Type2.h"

#include <iostream> //TODO: remove only for debug


namespace llfp
{
namespace hm
{

void Type::apply(TypePtr& ptr, Substitution s)
{
    if (ptr->equals(s.id))
    {
        ptr = s.type;
    }
    else
    {
        ptr->apply(s);
    }
}

std::vector<Substitution> Type::unify(TypeVar& a, SimpleType& b, const TypePtr& ptrB)
{
    auto subs = b.addConstraints(a);
    subs.push_back({ a.id, ptrB });
    return subs;
}

std::vector<Substitution> Type::unify(TypeVar& a, FunctionType& b, const TypePtr& ptrB)
{
    if (a.fields.empty() && a.typeClasses.empty())
    {
        return { {a.id, ptrB} };
    }
    else
    {
        unifyError(a, b);
    }
}

void Type::unifyError(const Type& a, const Type& b)
{
    throw Error(std::string{ "Failed to unify " } + a.str() + " and " + b.str());
}

// ----------------------------------------------------------------------------

std::vector<Substitution> SimpleType::addConstraints(const SimpleType& other)
{
    std::vector<Substitution> result;

    for (auto& f : other.fields)
    {
        auto it = fields.find(f.first);
        if (it != fields.end())
        {
            auto subs = TypeUnifier::unify(f.second, it->second); // or flip?
            result.insert(std::begin(result), std::begin(subs), std::end(subs));
        }
        else
        {
            fields.insert(f);
        }
    }

    typeClasses.insert(std::begin(other.typeClasses), std::end(other.typeClasses));
    return result;
}

std::string SimpleType::printConstraints(std::string base) const
{
    if (!typeClasses.empty())
    {
        std::string result{};
        for (auto& typeClass : typeClasses)
        {
            result += typeClass;
            result += ',';
        }
        result.back() = ' ';
        result += base;
        return result;
    }
    else
    {
        return base;
    }
}

void SimpleType::apply(Substitution s)
{
    for (auto& f : fields)
    {
        Type::apply(f.second, s);
    }
}

void SimpleType::copy(SimpleType* newObj) const
{
    newObj->typeClasses = typeClasses;
    for (const auto& [name, type] : fields)
    {
        newObj->fields.insert({name, type->copy()});
    }
}

// ----------------------------------------------------------------------------

TypeVar::TypeVar(TypeVarId id_): id(id_)
{}

std::string TypeVar::str() const
{
    return SimpleType::printConstraints(std::to_string(id));
}

bool TypeVar::equals(TypeVarId t) const
{
    return t == id;
}

std::vector<Substitution> TypeVar::unify(TypeVar& a, TypeVar& b, const TypePtr& /*ptrA*/, const TypePtr& ptrB)
{
    return SimpleType::unify(a, b, ptrB);
}

std::vector<Substitution> TypeVar::unify(TypeVar& a, TypeConstant& b, const TypePtr& /*ptrA*/, const TypePtr& ptrB)
{
    return SimpleType::unify(a, b, ptrB);
}

std::vector<Substitution> TypeVar::unify(TypeVar& a, FunctionType& b, const TypePtr& /*ptrA*/, const TypePtr& ptrB)
{
    return Type::unify(a, b, ptrB);
}

void TypeVar::apply(Substitution s)
{
    assert(id != s.id); // implementation detail, maybe weird, but we cant just do id = s.id... right?
    SimpleType::apply(s);
}

void TypeVar::accept(TypeVisitor* visitor)
{
    visitor->visit(*this);
}

TypePtr TypeVar::copy() const
{
    auto newObj = std::make_shared<TypeVar>(id);
    SimpleType::copy(newObj.get());
    return newObj;
}

// ----------------------------------------------------------------------------

TypeConstant::TypeConstant(std::string id_) : id{std::move(id_)}
{}

std::string TypeConstant::str() const
{
    return SimpleType::printConstraints(id);
}

std::vector<Substitution> TypeConstant::unify(TypeConstant& a, TypeVar& b, const TypePtr& ptrA, const TypePtr& /*ptrB*/)
{
    return SimpleType::unify(b, a, ptrA);
}

std::vector<Substitution> TypeConstant::unify(TypeConstant& a, TypeConstant& b, const TypePtr& /*ptrA*/, const TypePtr& /*ptrB*/)
{
    if (a.id == b.id)
    {
        return {};
    }
    else
    {
        unifyError(a, b);
    }
}

std::vector<Substitution> TypeConstant::unify(TypeConstant& a, FunctionType& b, const TypePtr& /*ptrA*/, const TypePtr& /*ptrB*/)
{
    unifyError(a, b);
}

void TypeConstant::accept(TypeVisitor* visitor) { visitor->visit(*this); }


TypePtr TypeConstant::copy() const
{
    auto newObj = std::make_shared<TypeConstant>(id);
    SimpleType::copy(newObj.get());
    return newObj;
}

// ----------------------------------------------------------------------------

FunctionType::FunctionType(std::vector<TypePtr> types_) : types{std::move(types_)}
{}

std::string FunctionType::str() const
{
    assert(!types.empty());

    std::string result{ "(" };
    result += types[0]->str();
    for (size_t i = 1; i < types.size(); ++i)
    {
        result += " -> ";
        result += types[i]->str();
    }
    result += ")";
    return result;
}

std::vector<Substitution> FunctionType::unify(FunctionType& a, TypeVar& b, const TypePtr& ptrA, const TypePtr& /*ptrB*/)
{
    return Type::unify(b, a, ptrA);
}

std::vector<Substitution> FunctionType::unify(FunctionType& a, TypeConstant& b, const TypePtr& /*ptrA*/, const TypePtr& /*ptrB*/)
{
    unifyError(a, b);
}

std::vector<Substitution> FunctionType::unify(FunctionType& a, FunctionType& b, const TypePtr& /*ptrA*/, const TypePtr& /*ptrB*/)
{
    if (a.types.size() == a.types.size())
    {
        std::vector<Substitution> subs;
        for (size_t i = 0; i < a.types.size(); ++i)
        {
            auto subs2 = TypeUnifier::unify(a.types[i], b.types[i]);
            subs.insert(std::end(subs), std::begin(subs2), std::end(subs2));
            // apply subs to tail of list?
            // "compose" the new subsitions?
        }
        return subs;
    }
    else
    {
        unifyError(a, b);
    }
}

void FunctionType::apply(Substitution s)
{
    for (auto& t : types)
    {
        Type::apply(t, s);
    }
}

void FunctionType::accept(TypeVisitor* visitor)
{
    visitor->visit(*this);
}

TypePtr FunctionType::copy() const
{
    std::vector<TypePtr> typesCopy;
    for (const auto& t : types)
    {
        typesCopy.push_back(t->copy());
    }
    return std::make_shared<FunctionType>(std::move(typesCopy));
}


// ----------------------------------------------------------------------------

TypeUnifier::TypeUnifier(const TypePtr& a_, const TypePtr& b_) : a(a_), b(b_)
{}

std::vector<Substitution> TypeUnifier::unify(const TypePtr& a, const TypePtr& b)
{
    TypeUnifier u{ a,b };
    a->accept(&u);
    return std::move(u.result);
}

void TypeUnifier::visit(TypeVar& self) { visit_(self); }
void TypeUnifier::visit(TypeConstant& self) { visit_(self); }
void TypeUnifier::visit(FunctionType& self) { visit_(self); }

// ----------------------------------------------------------------------------

TypeAnnotation::TypeAnnotation(std::map<const ast::Node*, TypePtr> ast_, std::map<std::string, TypePtr> vars_, TypeVarId nextFreeVariable_) :
    ast(std::move(ast_)),
    vars(std::move(vars_)),
    nextFreeVariable{ nextFreeVariable_ }
{}

TypeAnnotation::TypeAnnotation(const TypeAnnotation& other)
{
    nextFreeVariable = other.nextFreeVariable;
    for (const auto& [node, type] : other.ast)
    {
        ast[node] = type->copy();
    }
    for (const auto& [name, type] : other.vars)
    {
        vars[name] = type->copy();
    }
}

TypeAnnotation& TypeAnnotation::operator=(const TypeAnnotation& other)
{
    ast.clear();
    vars.clear();

    nextFreeVariable = other.nextFreeVariable;
    for (const auto& [node, type] : other.ast)
    {
        ast[node] = type->copy();
    }
    for (const auto& [name, type] : other.vars)
    {
        vars[name] = type->copy();
    }

    return *this;
}

TypePtr TypeAnnotation::get(const ast::Node* n) const
{
    return ast.at(n);
}

TypePtr TypeAnnotation::get(const std::string& id) const
{
    return vars.at(id);
}

void TypeAnnotation::substitute(Substitution sub)
{
    for (auto &nodeType : ast)
    {
        Type::apply(nodeType.second, sub);
    }
    for (auto &varType : vars)
    {
        Type::apply(varType.second, sub);
    }
}

void TypeAnnotation::print()
{
    for (auto var : vars)
    {
        std::cout << var.first << " = " << var.second->str() << '\n';
    }
}

namespace {

class TypeIdConverter : public TypeVisitor
{
    std::map<TypeVarId, TypeVarId> conversion;
    TypeVarId&                     nextFreeVariable;

public:

    TypeIdConverter(TypeVarId & nextFreeVariable_) : nextFreeVariable{ nextFreeVariable_ } {}

    virtual void visit(TypeVar& t)
    {
        auto it = conversion.find(t.id);
        if (it != conversion.end())
        {
            t.id = it->second;
        }
        else
        {
            conversion.insert({ t.id, nextFreeVariable });
            t.id = nextFreeVariable++;
        }
    }

    virtual void visit(TypeConstant& typeConstant)
    {
        for (const auto& [n,t] : typeConstant.fields)
        {
            t->accept(this);
        }
    }

    virtual void visit(FunctionType& funType)
    {
        for (const auto& t : funType.types)
        {
            t->accept(this);
        }
    }
};

}

// rename addConstraint?
void TypeAnnotation::add(const std::string& var, const TypePtr& inType)
{
    // moves type ids into this TypeAnnotation
    auto type = inType->copy();
    TypeIdConverter converter{nextFreeVariable};
    type->accept(&converter);

    auto subs = hm::TypeUnifier::unify(get(var), type);
    for (size_t i = 0; i < subs.size(); ++i)
    {
        substitute(subs[i]);
        for (size_t j = i + 1; j < subs.size(); ++j)
        {
            Type::apply(subs[j].type, subs[i]);
        }
    }
}

// ----------------------------------------------------------------------------

Constraint::Constraint(SourceLocation location_, TypePtr left_, TypePtr right_) :
    location(location_),
    left(std::move(left_)),
    right(std::move(right_))
{}

std::string Constraint::str()
{
    return left->str() + " = " + right->str();
}

void Constraint::substitute(Substitution s)
{
    Type::apply(left, s);
    Type::apply(right, s);
}

std::vector<Substitution> Constraint::solve()
{
    return TypeUnifier::unify(left, right);
}

// ----------------------------------------------------------------------------

void Annotator::operator()(const ast::Function& fun)
{
    current     = 0;
    vars        = std::map<std::string, TypePtr>{};
    result      = std::map<const ast::Node*, TypePtr>{};
    constraints = std::vector<Constraint>{};

    // TODO: should also do module:name since that's also valid
    // foo(x,y) = if ... else m:foo(x,y)
    auto& funTV = vars[fun.name] = makeVar();

    fun.functionBody->accept(this);
    auto bodyTV = tv(fun.functionBody);

    std::vector<TypePtr> args;
    args.push_back(bodyTV);
    for (auto& arg : fun.parameters)
    {
        auto it = vars.find(arg->identifier);
        if (it == vars.end())
        {
            auto argTV = arg->type.empty() ? makeVar() : makeConst(arg->type.str()); // Maybe[int], Maybe[bool]... Maybe[a]?
            vars[arg->identifier] = argTV;
            args.push_back(std::move(argTV));
        }
        else
        {
            args.push_back(it->second);
            if (!arg->type.empty())
            {
                add({ arg->location, it->second, makeConst(arg->type.str()) });
            }
        }
    }
    add({ fun.location, funTV, makeFunction(args) });
    if (!fun.type.empty())
    {
        add({ fun.location, bodyTV, makeConst(fun.type.str())});
    }
}

void Annotator::visit(ast::LetExp& exp)
{
    // push scope

    for (auto& let : exp.letStatments)
    {
        let->functionBody->accept(this);

        if (let->parameters.empty())
        {
            auto it = vars.find(let->name);
            if (it == vars.end())
            {
                TypePtr letTV = let->type.empty() ? makeVar() : makeConst(let->type.str());
                vars[let->name] = letTV;
                add({ let->location, letTV, tv(let->functionBody) });
            }
            else
            {
                // previosuly used something global/parameter/outer let with same name
                // now everything are scoped in the outer function
                // TODO: allow local scope in let body
            }
        }
        else // function
        {
            assert(false); //TODO: implement let functions
        }
    }

    exp.exp->accept(this);
    result[&exp] = tv(exp.exp);

    // pop scope
}

void Annotator::visit(ast::IfExp& exp)
{
    exp.condition->accept(this);
    exp.thenExp->accept(this);
    exp.elseExp->accept(this);

    auto &expTV = result[&exp] = makeVar();
    add({ exp.condition->location, (tv(exp.condition)), makeConst("bool") });
    add({ exp.thenExp->location, expTV, (tv(exp.thenExp)) });
    add({ exp.elseExp->location, expTV, (tv(exp.elseExp)) });
}

void Annotator::visit(ast::CaseExp& exp)
{
    assert(false); //TODO: implement
}

void Annotator::visit(ast::BinaryExp& exp)
{
    exp.lhs->accept(this);
    exp.rhs->accept(this);

    TypePtr expTV;
    if (exp.op == "*") { expTV = makeClass("Num"); }
    else if (exp.op == "/") { expTV = makeClass("Num"); }
    else if (exp.op == "%") { expTV = makeClass("Num"); }
    else if (exp.op == "+") { expTV = makeClass("Num"); }
    else if (exp.op == "-") { expTV = makeClass("Num"); }
    else if (exp.op == "<<") { expTV = makeClass("Integer"); }
    else if (exp.op == ">>") { expTV = makeClass("Integer"); }
    else if (exp.op == ">>>") { expTV = makeClass("Integer"); }
    else if (exp.op == ">") { expTV = makeClass("Ord"); }
    else if (exp.op == ">=") { expTV = makeClass("Ord"); }
    else if (exp.op == "<") { expTV = makeClass("Ord"); }
    else if (exp.op == "<=") { expTV = makeClass("Ord"); }
    else if (exp.op == "==") { expTV = makeClass("Eq"); }
    else if (exp.op == "!=") { expTV = makeClass("Eq"); }
    else if (exp.op == "&") { expTV = makeClass("Integer"); }
    else if (exp.op == "|") { expTV = makeClass("Integer"); }
    else if (exp.op == "^") { expTV = makeClass("Integer"); }
    else if (exp.op == "&&") { expTV = makeConst("bool"); }
    else if (exp.op == "||") { expTV = makeConst("bool"); }
    else { assert(false); }

    result[&exp] = expTV;
    add({ exp.lhs->location, expTV, (tv(exp.lhs)) });
    add({ exp.rhs->location, expTV, (tv(exp.rhs)) });
}

void Annotator::visit(ast::UnaryExp& exp)
{
    exp.operand->accept(this);

    TypePtr expTV;
    if (exp.op == "-") { expTV = makeClass("Signed"); }
    else if (exp.op == "!") { expTV = makeConst("bool"); }
    else if (exp.op == "~") { expTV = makeClass("Integer"); }

    result[&exp] = expTV;
    add({ exp.location, expTV, (tv(exp.operand)) });
}

void Annotator::visit(ast::LiteralExp& exp)
{
    TypePtr expTV;
    switch (exp.tokenType)
    {
    case lex::Token::Integer:
        if (exp.value.front() == '-')
            { expTV = makeClass("Signed"); }
        else
            { expTV = makeClass("Num"); }
        break;
    case lex::Token::Float:  expTV = makeClass("Floating"); break;
    case lex::Token::Char:   expTV = makeConst("char"); break;
    case lex::Token::String: expTV = makeConst("string"); break;
    case lex::Token::Bool:   expTV = makeConst("bool"); break;
    default:
        assert(false);
        break;
    }
    result[&exp] = expTV;
}

void Annotator::visit(ast::CallExp& exp)
{
    for (auto& arg : exp.arguments)
    {
        arg->accept(this);
    }

    auto& returnType = result[&exp] = makeVar();

    std::vector<TypePtr> args;
    args.push_back(returnType);
    for (auto& arg : exp.arguments)
    {
        args.push_back(tv(arg));
    }
    auto funType = makeFunction(args);

    //TODO: can do lower_bound and hint
    auto funName = exp.identifier.str();
    auto it = vars.find(funName);
    if (it == vars.end())
    {
        vars[std::move(funName)] = funType;
    }
    else
    {
        add({ exp.location, funType, it->second });
    }
}

void Annotator::visit(ast::VariableExp& exp)
{
    //TODO: can do lower_bound and hint
    auto varName = exp.identifier.str();
    auto it = vars.find(varName);
    if (it == vars.end())
    {
        // external var... treat differently? treat as if it can be anything?
        auto ptr = makeVar();
        result[&exp] = ptr;
        vars[std::move(varName)] = ptr;
    }
    else
    {
        result[&exp] = it->second;
    }
}

void Annotator::visit(ast::FieldExp& exp)
{
    exp.lhs->accept(this);

    auto expTV = makeVar();
    result[&exp] = expTV;

    auto t = std::make_shared<TypeVar>(current++);
    t->fields.insert(std::make_pair(exp.fieldIdentifier, expTV));
    add({ exp.location, tv(exp.lhs), t });
}

void Annotator::visit(ast::ConstructorExp& exp)
{
    for (auto& arg : exp.arguments)
    {
        arg->exp->accept(this);
    }
    result[&exp] = makeVar();

    // is user type?
    // application, treat as functions?
    // no, constructor tells me something about the type constant for this exp
    auto it = vars.find(exp.identifier.name); // ?
}

TypePtr Annotator::makeVar()
{
    return std::make_shared<TypeVar>(current++);
}

TypePtr Annotator::makeConst(std::string s)
{
    auto it = typeConstants.find(s);
    if (it == typeConstants.end())
    {
        auto &constant = typeConstants[s];
        constant = std::make_shared<TypeConstant>(std::move(s));
        return constant;
    }
    else
    {
        return it->second;
    }
}

TypePtr Annotator::makeClass(std::string s)
{
    auto ptr = std::make_shared<TypeVar>(current++);
    ptr->typeClasses.insert(std::move(s));
    return ptr;
}

TypePtr Annotator::makeFunction(std::vector<TypePtr> types)
{
    return std::make_shared<FunctionType>(std::move(types));
}

TypePtr Annotator::tv(const std::string& name)
{
    return vars.at(name);
}

TypePtr Annotator::tv(ast::Node& ast)
{
    return result.at(&ast);
}

void Annotator::add(Constraint c)
{
    constraints.push_back(std::move(c));
}

// ----------------------------------------------------------------------------

std::string test(ast::Function& fun)
{
    Annotator annotator{};
    annotator(fun);

    TypeAnnotation annotation{ std::move(annotator.result), std::move(annotator.vars), annotator.current };
    std::vector<Constraint> constraints{ std::move(annotator.constraints) };

    annotation.print();
    for (auto c : constraints)
    {
        std::cout << c.str() << '\n';
    }

    for (size_t i = 0; i < constraints.size(); ++i)
    {
        try
        {
            auto subs = constraints[i].solve();
            for (auto& sub : subs)
            {
                std::cout << "sub " << sub.id << " = " << sub.type->str() << '\n';

                annotation.substitute(sub);
                for (size_t j = i + 1; j < constraints.size(); ++j)
                {
                    constraints[j].substitute(sub);
                }
            }
        }
        catch (const Error& error)
        {
            std::cout << error.what() << '\n';
            return "";
        }
    }

    std::cout << "---\n";

    annotation.print();
    for (auto c : constraints)
    {
        std::cout << c.str() << '\n';
    }
    return annotation.get(fun.name)->str();
}

TypeAnnotation inferType(const ast::Function& fun)
{
    Annotator annotator{};
    annotator(fun);

    TypeAnnotation annotation{ std::move(annotator.result), std::move(annotator.vars), annotator.current };
    std::vector<Constraint> constraints{ std::move(annotator.constraints) };

    for (size_t i = 0; i < constraints.size(); ++i)
    {
        // try{} to catch and add source location?

        auto subs = constraints[i].solve();
        for (auto& sub : subs)
        {
            annotation.substitute(sub);
            for (size_t j = i + 1; j < constraints.size(); ++j)
            {
                constraints[j].substitute(sub);
            }
        }
    }

    return annotation;
}

} // namespace hm
} // namespace llfp
