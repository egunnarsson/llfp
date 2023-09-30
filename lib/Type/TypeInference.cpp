
#include "Type/TypeInference.h"

#include "Common/Algorithm.h"
#include "Error.h"
#include "String/StringConstants.h"

#pragma warning(push, 0)

#include <llvm/ADT/STLExtras.h>

#pragma warning(pop)

#include <iostream> //TODO: remove only for debug
#include <tuple>


namespace llfp::hm
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
    if (a.fields.empty() && a.typeClasses.empty() && a.constructors.empty() && (!a.parameters.has_value() || a.parameters->empty()))
    {
        return { { a.id, ptrB } };
    }
    else
    {
        unifyError(a, b);
    }
}

void Type::unifyError(const Type& a, const Type& b)
{
    throw Error(std::string{ "Failed to unify types '" } + a.str() + "' and '" + b.str() + '\'');
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

    if (!parameters.has_value())
    {
        parameters = other.parameters;
    }
    else if (other.parameters.has_value())
    {
        assert(parameters->size() == other.parameters->size());
        for (auto param : llvm::zip(*parameters, *other.parameters))
        {
            auto subs = TypeUnifier::unify(std::get<0>(param), std::get<1>(param));
            result.insert(std::begin(result), std::begin(subs), std::end(subs));
        }
    }

    typeClasses.insert(std::begin(other.typeClasses), std::end(other.typeClasses));
    constructors.insert(std::begin(other.constructors), std::end(other.constructors));
    return result;
}

std::string SimpleType::printConstraints(const std::string& base) const
{
    std::string result{};
    if (!typeClasses.empty())
    {
        for (auto& typeClass : typeClasses)
        {
            result += typeClass;
            result += ',';
        }
        result.back() = ' ';
    }
    result += base;
    return result;
}

void SimpleType::apply(Substitution s)
{
    for (auto& [name, type] : fields)
    {
        Type::apply(type, s);
    }
    if (parameters.has_value())
    {
        for (auto& param : *parameters)
        {
            Type::apply(param, s);
        }
    }
}

void SimpleType::copy(std::map<std::string, TypePtr>& typeConstants, SimpleType* newObj) const
{
    newObj->typeClasses = typeClasses;
    for (const auto& [name, type] : fields)
    {
        newObj->fields.insert({ name, type->copy(typeConstants) });
    }
    if (parameters.has_value())
    {
        newObj->parameters.emplace();
        for (const auto& param : *parameters)
        {
            newObj->parameters->push_back(param->copy(typeConstants));
        }
    }
    newObj->constructors = constructors;
}

// ----------------------------------------------------------------------------

TypeVar::TypeVar(TypeVarId id_)
    : id(id_)
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

TypePtr TypeVar::copy(std::map<std::string, TypePtr>& typeConstants) const
{
    auto newObj = std::make_shared<TypeVar>(id);
    SimpleType::copy(typeConstants, newObj.get());
    return newObj;
}

// ----------------------------------------------------------------------------

TypeConstant::TypeConstant(std::string id_)
    : id{ std::move(id_) }
{
    parameters.emplace();
}

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
        // unify params probably, maybe
        assert(a.parameters.has_value());
        assert(b.parameters.has_value());
        return a.addConstraints(b);
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


TypePtr TypeConstant::copy(std::map<std::string, TypePtr>& typeConstants) const
{
    auto it = typeConstants.find(id);
    if (it != typeConstants.end())
    {
        return it->second;
    }
    auto newObj       = std::make_shared<TypeConstant>(id);
    typeConstants[id] = newObj;
    SimpleType::copy(typeConstants, newObj.get());
    return newObj;
}

// ----------------------------------------------------------------------------

FunctionType::FunctionType(std::vector<TypePtr> types_)
    : types{ std::move(types_) }
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
    if (a.types.size() == b.types.size())
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

TypePtr FunctionType::copy(std::map<std::string, TypePtr>& typeConstants) const
{
    return copyFun(typeConstants);
}

FunTypePtr FunctionType::copyFun(std::map<std::string, TypePtr>& typeConstants) const
{
    std::vector<TypePtr> typesCopy;
    for (const auto& t : types)
    {
        typesCopy.push_back(t->copy(typeConstants));
    }
    return std::make_shared<FunctionType>(std::move(typesCopy));
}


// ----------------------------------------------------------------------------

TypeUnifier::TypeUnifier(const TypePtr& a_, const TypePtr& b_)
    : a{ a_ },
      b{ b_ }
{}

std::vector<Substitution> TypeUnifier::unify(const TypePtr& a, const TypePtr& b)
{
    TypeUnifier u{ a, b };
    a->accept(&u);
    return std::move(u.result);
}

void TypeUnifier::visit(TypeVar& self) { visit_(self); }
void TypeUnifier::visit(TypeConstant& self) { visit_(self); }
void TypeUnifier::visit(FunctionType& self) { visit_(self); }

// ----------------------------------------------------------------------------

TypeAnnotation::TypeAnnotation(
    std::map<const ast::Node*, TypePtr> ast_,
    std::map<std::string, TypePtr>      vars_,
    std::map<std::string, FunTypePtr>   functions_,
    TypeVarId                           nextFreeVariable_)
    : ast{ std::move(ast_) },
      variables{ std::move(vars_) },
      functions{ std::move(functions_) },
      nextFreeVariable{ nextFreeVariable_ }
{}

TypeAnnotation::TypeAnnotation(const TypeAnnotation& other)
{
    std::map<std::string, TypePtr> typeConstants;
    nextFreeVariable = other.nextFreeVariable;
    for (const auto& [node, type] : other.ast)
    {
        ast[node] = type->copy(typeConstants);
    }
    for (const auto& [name, type] : other.variables)
    {
        variables[name] = type->copy(typeConstants);
    }
    for (const auto& [name, type] : other.functions)
    {
        functions[name] = type->copyFun(typeConstants);
    }
}

TypeAnnotation& TypeAnnotation::operator=(const TypeAnnotation& other)
{
    ast.clear();
    variables.clear();
    functions.clear();

    std::map<std::string, TypePtr> typeConstants;
    nextFreeVariable = other.nextFreeVariable;
    for (const auto& [node, type] : other.ast)
    {
        ast[node] = type->copy(typeConstants);
    }
    for (const auto& [name, type] : other.variables)
    {
        variables[name] = type->copy(typeConstants);
    }
    for (const auto& [name, type] : other.functions)
    {
        functions[name] = type->copyFun(typeConstants);
    }

    return *this;
}

TypePtr TypeAnnotation::get(const ast::Node* n) const
{
    return ast.at(n);
}

TypePtr TypeAnnotation::getVar(const std::string& id) const
{
    return variables.at(id);
}

FunTypePtr TypeAnnotation::getFun(const std::string& id) const
{
    return functions.at(id);
}

void TypeAnnotation::substitute(Substitution sub)
{
    for (auto& nodeType : ast)
    {
        Type::apply(nodeType.second, sub);
    }
    for (auto& varType : variables)
    {
        Type::apply(varType.second, sub);
    }
    for (auto& funType : functions)
    {
        funType.second->apply(sub);
        // TypePtr ptr = funType.second;
        // Type::apply(ptr, sub);
    }
}

void TypeAnnotation::print()
{
    for (auto& var : variables)
    {
        std::cout << var.first << " = " << var.second->str() << '\n';
    }
    for (auto& fun : functions)
    {
        std::cout << fun.first << " = " << fun.second->str() << '\n';
    }
}

namespace
{

// moves type ids into this a TypeAnnotation with nextFreeVariable
class TypeIdConverter : public TypeVisitor
{
    std::map<TypeVarId, TypeVarId> conversion;
    TypeVarId&                     nextFreeVariable;

public:

    static TypePtr convert(TypeVarId& nextFreeVariable, const TypePtr& inType)
    {
        std::map<std::string, TypePtr> typeConstants;
        auto                           type = inType->copy(typeConstants);
        TypeIdConverter                converter{ nextFreeVariable };
        type->accept(&converter);
        return type;
    }

    TypeIdConverter(TypeVarId& nextFreeVariable_)
        : nextFreeVariable{ nextFreeVariable_ }
    {}

    virtual void visit(TypeVar& t)
    {
        visitSimpleType(t);
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
        visitSimpleType(typeConstant);
    }

    virtual void visit(FunctionType& funType)
    {
        for (const auto& t : funType.types)
        {
            t->accept(this);
        }
    }

private:

    void visitSimpleType(SimpleType& type)
    {
        for (const auto& [n, t] : type.fields)
        {
            t->accept(this);
        }
        if (type.parameters.has_value())
        {
            for (const auto& param : *type.parameters)
            {
                param->accept(this);
            }
        }
    }
};

} // namespace

bool TypeAnnotation::addConstraint(const std::string& funName, const TypePtr& inType)
{
    auto type = TypeIdConverter::convert(nextFreeVariable, inType);

    auto subs = hm::TypeUnifier::unify(getFun(funName), type);
    for (size_t i = 0; i < subs.size(); ++i)
    {
        substitute(subs[i]);
        for (size_t j = i + 1; j < subs.size(); ++j)
        {
            Type::apply(subs[j].type, subs[i]);
        }
    }
    return !subs.empty();
}

bool TypeAnnotation::addConstraint(const TypePtr& typeA, const TypeConstantPtr& typeConstant)
{
    auto typeB = TypeIdConverter::convert(nextFreeVariable, typeConstant);

    auto subs = hm::TypeUnifier::unify(typeA, typeB);
    for (size_t i = 0; i < subs.size(); ++i)
    {
        substitute(subs[i]);
        for (size_t j = i + 1; j < subs.size(); ++j)
        {
            Type::apply(subs[j].type, subs[i]);
        }
    }
    return !subs.empty();
}

// ----------------------------------------------------------------------------

Constraint::Constraint(SourceLocation location_, TypePtr left_, TypePtr right_)
    : location(location_),
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

Annotator::Annotator(const ImportedModule* astModule)
    : astModule_{ astModule }
{}

/* Tests

T1 foo(T2[T3] x) =
    case x of
        ...
    end

// Allow multiple uses of T2
T1 foo(T2[T3] x, T2[T4] y) = ...;

*/

namespace
{

bool containsTypeVariable(const ast::TypeIdentifier& id, const std::map<std::string, TypeVarPtr>& typeVariables)
{
    if (id.identifier.moduleName.empty() && typeVariables.find(id.identifier.name) != typeVariables.end())
    {
        return true;
    }
    for (auto& param : id.parameters)
    {
        if (containsTypeVariable(param, typeVariables))
        {
            return true;
        }
    }
    return false;
}

} // namespace

// Maybe[int], Maybe[bool]... Maybe[a]?
TypePtr Annotator::typeFromIdentifier(const ast::TypeIdentifier& id, const std::map<std::string, TypeVarPtr>& typeVariables)
{
    if (id.identifier.moduleName.empty())
    {
        auto it = typeVariables.find(id.identifier.name);
        if (it != typeVariables.end())
        {
            return it->second;
        }
    }
    SimpleTypePtr typePtr;
    if (containsTypeVariable(id, typeVariables))
    {
        typePtr = makeVar();
    }
    else
    {
        typePtr = makeConst(id.str());
    }
    for (auto& param : id.parameters)
    {
        typePtr->parameters->push_back(typeFromIdentifier(param));
    }
    return typePtr;
}

void Annotator::operator()(const ast::Function& fun)
{
    current = 0;
    variables.clear();
    functions.clear();
    result.clear();
    constraints.clear();

    auto bodyType = fun.type.empty() ? static_cast<TypePtr>(makeVar()) : typeFromIdentifier(fun.type);

    std::vector<TypePtr> args;
    args.push_back(bodyType);
    for (auto& arg : fun.parameters)
    {
        auto argTV          = arg->type.empty() ? makeVar() : typeFromIdentifier(arg->type);
        // TODO: nested type parameters
        auto [it, inserted] = variables.insert({ arg->identifier, argTV });
        if (!inserted)
        {
            throw ErrorLocation(arg->location, std::string{ "duplicate parameter \"" } + arg->identifier + '"');
        }
        args.push_back(std::move(argTV));
    }

    functions.insert({ std::move(astModule_->name() + ':' + fun.name), makeFunction(std::move(args)) });

    // standard module may not have implementation
    if (fun.functionBody != nullptr)
    {
        fun.functionBody->accept(this);
        add({ fun.location, bodyType, result[fun.functionBody.get()] });
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
            result[let.get()] = tv(let->functionBody);

            auto it = variables.find(let->name);
            if (it == variables.end())
            {
                TypePtr letTV        = let->type.empty() ? static_cast<TypePtr>(makeVar()) : typeFromIdentifier(let->type);
                variables[let->name] = letTV;
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
            assert(false); // TODO: implement let functions
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

    auto& expTV = result[&exp] = makeVar();
    add({ exp.condition->location, (tv(exp.condition)), makeConst(id::Bool) });
    add({ exp.thenExp->location, expTV, (tv(exp.thenExp)) });
    add({ exp.elseExp->location, expTV, (tv(exp.elseExp)) });
}


PatternTypeVisitor::PatternTypeVisitor(Annotator& annotator_)
    : annotator{ annotator_ }
{}

void PatternTypeVisitor::visit(Annotator& annotator, ast::Pattern& pattern)
{
    PatternTypeVisitor visitor{ annotator };
    pattern.accept(&visitor);
}

void PatternTypeVisitor::visit(ast::BoolPattern& pattern) { add(pattern, annotator.makeConst(id::Bool)); }
void PatternTypeVisitor::visit(ast::IntegerPattern& pattern) { add(pattern, annotator.makeClass(id::Num)); }
void PatternTypeVisitor::visit(ast::FloatPattern& pattern) { add(pattern, annotator.makeClass(id::Floating)); }
void PatternTypeVisitor::visit(ast::CharPattern& pattern) { add(pattern, annotator.makeConst(id::Char)); }
void PatternTypeVisitor::visit(ast::StringPattern& pattern) { add(pattern, annotator.makeConst(id::String)); }

void PatternTypeVisitor::visit(ast::IdentifierPattern& pattern)
{
    auto var                           = annotator.makeVar();
    annotator.variables[pattern.value] = var;
    add(pattern, std::move(var));
}

void PatternTypeVisitor::visit(ast::ConstructorPattern& pattern)
{
    // Lookup ast!

    auto type = annotator.makeVar();
    type->constructors.insert(pattern.identifier.str());
    add(pattern, type);
    for (auto& arg : pattern.arguments)
    {
        // TODO: add constraints for fields... by index!
        // and make constraint for parameters...
        arg.pattern->accept(this);
    }
}

void PatternTypeVisitor::add(ast::Pattern& pattern, TypePtr type)
{
    annotator.result[&pattern] = std::move(type);
}

void Annotator::visit(ast::CaseExp& exp)
{
    exp.caseExp->accept(this);
    auto  caseExpTV = tv(exp.caseExp);
    auto& expTV = result[&exp] = makeVar();

    for (auto& c : exp.clauses)
    {
        // TODO: push/pop variable scope
        PatternTypeVisitor::visit(*this, *c.pattern);
        add({ c.pattern->location, caseExpTV, result[c.pattern.get()] });

        // TODO: add constraints between clauses

        c.exp->accept(this);
        add({ c.exp->location, expTV, tv(c.exp) });
    }
}

void Annotator::visit(ast::BinaryExp& exp)
{
    exp.lhs->accept(this);
    exp.rhs->accept(this);

    if (exp.op == ">" ||
        exp.op == ">=" ||
        exp.op == "<" ||
        exp.op == "<=")
    {
        result[&exp] = makeConst(id::Bool);
        add({ exp.location, makeClass(id::Ord), tv(exp.lhs) });
        add({ exp.location, makeClass(id::Ord), tv(exp.rhs) });
        add({ exp.location, tv(exp.lhs), tv(exp.rhs) });
    }
    else if (exp.op == "==" || exp.op == "!=")
    {
        result[&exp] = makeConst(id::Bool);
        add({ exp.location, makeClass(id::Eq), tv(exp.lhs) });
        add({ exp.location, makeClass(id::Eq), tv(exp.rhs) });
        add({ exp.location, tv(exp.lhs), tv(exp.rhs) });
    }
    else
    {
        TypePtr expTV;
        if (exp.op == "*") { expTV = makeClass(id::Num); }
        else if (exp.op == "/") { expTV = makeClass(id::Num); }
        else if (exp.op == "%") { expTV = makeClass(id::Num); }
        else if (exp.op == "+") { expTV = makeClass(id::Num); }
        else if (exp.op == "-") { expTV = makeClass(id::Num); }
        else if (exp.op == "<<") { expTV = makeClass(id::Integer); }
        else if (exp.op == ">>") { expTV = makeClass(id::Integer); }
        else if (exp.op == ">>>") { expTV = makeClass(id::Integer); }
        else if (exp.op == "&") { expTV = makeClass(id::Integer); }
        else if (exp.op == "|") { expTV = makeClass(id::Integer); }
        else if (exp.op == "^") { expTV = makeClass(id::Integer); }
        else if (exp.op == "&&") { expTV = makeConst(id::Bool); }
        else if (exp.op == "||") { expTV = makeConst(id::Bool); }
        else { assert(false); }

        result[&exp] = expTV;
        add({ exp.lhs->location, expTV, (tv(exp.lhs)) });
        add({ exp.rhs->location, expTV, (tv(exp.rhs)) });
    }
}

void Annotator::visit(ast::UnaryExp& exp)
{
    exp.operand->accept(this);

    TypePtr expTV;
    if (exp.op == "-") { expTV = makeClass(id::Signed); }
    else if (exp.op == "!") { expTV = makeConst(id::Bool); }
    else if (exp.op == "~") { expTV = makeClass(id::Integer); }

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
        {
            expTV = makeClass(id::Signed);
        }
        else
        {
            expTV = makeClass(id::Num);
        }
        break;
    case lex::Token::Float: expTV = makeClass(id::Floating); break;
    case lex::Token::Char: expTV = makeConst(id::Char); break;
    case lex::Token::String: expTV = makeConst(id::String); break;
    case lex::Token::Bool: expTV = makeConst(id::Bool); break;
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

    // TODO: can do lower_bound and hint
    auto funName = exp.identifier.str();
    auto it      = functions.find(funName);
    if (it == functions.end())
    {
        assert(strchr(funName.c_str(), ':') != nullptr);
        functions[std::move(funName)] = funType;
    }
    else
    {
        add({ exp.location, funType, it->second });
    }
}

void Annotator::visit(ast::VariableExp& exp)
{
    // TODO: can do lower_bound and hint
    auto varName = exp.identifier.str();
    auto it      = variables.find(varName);
    // if it is a variable it should have been added by arguments or let exp
    if (it == variables.end())
    {
        // so this is something external, i.e. a function
        auto ptr     = makeVar();
        result[&exp] = ptr;
        assert(strchr(varName.c_str(), ':') != nullptr);
        functions[std::move(varName)] = makeFunction({ ptr });
    }
    else
    {
        result[&exp] = it->second;
    }
}

void Annotator::visit(ast::FieldExp& exp)
{
    exp.lhs->accept(this);

    auto expTV   = makeVar();
    result[&exp] = expTV;

    auto t = makeVar();
    t->fields.insert(std::make_pair(exp.fieldIdentifier, expTV));
    add({ exp.location, tv(exp.lhs), t });

    // make constraint for parameters...
}

namespace
{

const ast::DataConstructor& lookupConstructor(const ast::Data& astData, ast::ConstructorExp& exp)
{
    if (astData.constructors.size() == 1)
    {
        if (astData.name == exp.identifier.name)
        {
            return astData.constructors.front();
        }
    }
    else
    {
        for (const auto& con : astData.constructors)
        {
            if (con.name == exp.identifier.name)
            {
                return con;
            }
        }
    }
    throw ErrorLocation{ exp.location, std::string{ "unknown constructor: '" } + exp.identifier.str() + '\'' };
}

} // namespace

void Annotator::visit(ast::ConstructorExp& exp)
{
    auto astData = astModule_->lookupConstructor(exp.identifier);
    assert(!astData.empty());
    auto& constructor = lookupConstructor(*astData.data, exp);

    SimpleTypePtr expTV;
    if (astData.data->typeVariables.empty())
    {
        expTV = makeConst(astData.importedModule->name() + ':' + astData.data->name);
    }
    else
    {
        expTV = makeVar();
        expTV->parameters.emplace();
    }

    std::map<std::string, TypeVarPtr> typeVariables;
    for (auto& typeVarName : astData.data->typeVariables)
    {
        auto typeVar               = makeVar();
        typeVariables[typeVarName] = typeVar;
        expTV->parameters->push_back(std::move(typeVar));
    }
    result[&exp] = expTV;

    if (constructor.fields.size() != exp.arguments.size())
    {
        throw ErrorLocation{ exp.location, "incorrect number of arguments" };
    }
    for (const auto& [field, arg] : llvm::zip(constructor.fields, exp.arguments))
    {
        arg.exp->accept(this);
        auto argType   = tv(arg.exp);
        auto fieldType = typeFromIdentifier(field.type, typeVariables); // the name of the constants made are broken...
        add({ arg.location, argType, fieldType });

        expTV->fields.insert(std::make_pair(field.name, argType));
    }

    for (auto& con : astData.data->constructors)
    {
        expTV->constructors.insert(con.name);
        // maybe add fields for other constructors
        /* if (con.name != exp.identifier.name) {
            for (const auto& [field, arg] : llvm::zip(con.fields, exp.arguments))
            {
            }
        } */
    }
}

void Annotator::visit(ast::IntrinsicExp& exp)
{
    std::vector<std::string_view> types = str_split(exp.identifier_, '\'');

    for (auto& argIt : llvm::enumerate(exp.arguments_))
    {
        argIt.value()->accept(this);
        add({ argIt.value()->location, tv(argIt.value()), makeConst(types.at(argIt.index() + 2)) });
    }

    result[&exp] = makeConst(types.at(1));
}

TypeVarPtr Annotator::makeVar()
{
    return std::make_shared<TypeVar>(current++);
}

TypeConstantPtr Annotator::makeConst(llvm::StringRef stringRef)
{
    std::string str{ stringRef.str() };
    auto        it = typeConstants.find(str);
    if (it == typeConstants.end())
    {
        auto& constant = typeConstants[str];
        constant       = std::make_shared<TypeConstant>(std::move(str));
        return constant;
    }
    else
    {
        return it->second;
    }
}

TypePtr Annotator::makeClass(llvm::StringRef stringRef)
{
    auto ptr = makeVar();
    ptr->typeClasses.insert(stringRef.str());
    return ptr;
}

FunTypePtr Annotator::makeFunction(std::vector<TypePtr> types)
{
    return std::make_shared<FunctionType>(std::move(types));
}

TypePtr Annotator::tv(const std::string& name)
{
    auto it = variables.find(name);
    if (it == variables.end())
    {
        return functions.at(name);
    }
    return it->second;
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

std::string test(ImportedModule* astModule, ast::Function& fun)
{
    Annotator annotator{ astModule };
    annotator(fun);

    TypeAnnotation          annotation{ std::move(annotator.result), std::move(annotator.variables), std::move(annotator.functions), annotator.current };
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
    return annotation.getFun(astModule->name() + ':' + fun.name)->str();
}

TypeAnnotation inferType(const ImportedModule* astModule, const ast::Function& fun)
{
    Annotator annotator{ astModule };
    annotator(fun);

    TypeAnnotation          annotation{ std::move(annotator.result), std::move(annotator.variables), std::move(annotator.functions), annotator.current };
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

} // namespace llfp::hm
