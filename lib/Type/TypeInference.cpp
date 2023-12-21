
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

void Type::unifyError(const Type& a, const Type& b)
{
    throw Error(std::string{ "Failed to unify types '" } + a.str() + "' and '" + b.str() + '\'');
}

// ----------------------------------------------------------------------------

std::vector<Substitution> SimpleType::addConstraints(const SimpleType& other)
{
    std::vector<Substitution> result;
    typeClasses.insert(std::begin(other.typeClasses), std::end(other.typeClasses));
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

void SimpleType::copy(std::map<std::string, TypeConstantPtr>& typeConstants, SimpleType* newObj) const
{
    newObj->typeClasses = typeClasses;
}

// ----------------------------------------------------------------------------

UnboundTypeVar::UnboundTypeVar(TypeVarId id)
    : id_{ id }
{
}

std::string UnboundTypeVar::str() const
{
    return SimpleType::printConstraints(std::to_string(id_));
}

bool UnboundTypeVar::equals(TypeVarId t) const
{
    return id_ == t;
}

std::vector<Substitution> UnboundTypeVar::unify(UnboundTypeVar& a, UnboundTypeVar& b, const TypePtr& ptrA, const TypePtr& /*ptrB*/)
{
    a.constructors_.insert(b.constructors_.begin(), b.constructors_.end());

    auto result = a.addConstraints(b);
    result.push_back({ b.id_, ptrA });
    return result;
}

std::vector<Substitution> UnboundTypeVar::unify(UnboundTypeVar& a, BoundTypeVar& b, const TypePtr& /*ptrA*/, const TypePtr& ptrB)
{
    for (auto& constructor : a.constructors_)
    {
        auto it = std::find_if(b.ast_.data->constructors.begin(), b.ast_.data->constructors.begin(),
                               [&constructor](const ast::DataConstructor& astConstructor) { return astConstructor.name == constructor; });
        if (it == b.ast_.data->constructors.end())
        {
            unifyError(a, b); // add info, missing constructor
        }
    }

    auto result = b.addConstraints(a);
    result.push_back({ a.id_, ptrB });
    return result;
}

std::vector<Substitution> UnboundTypeVar::unify(UnboundTypeVar& a, TypeConstant& b, const TypePtr& /*ptrA*/, const TypePtr& ptrB)
{
    for (auto& constructor : a.constructors_)
    {
        if (b.ast_.empty()) // basic type
        {
            unifyError(a, b);
        }
        auto it = std::find_if(b.ast_.data->constructors.begin(), b.ast_.data->constructors.begin(),
                               [&constructor](const ast::DataConstructor& astConstructor) { return astConstructor.name == constructor; });
        if (it == b.ast_.data->constructors.end())
        {
            unifyError(a, b); // add info, missing constructor
        }
    }

    auto result = b.addConstraints(a);
    result.push_back({ a.id_, ptrB });
    return result;
}

std::vector<Substitution> UnboundTypeVar::unify(UnboundTypeVar& a, FunctionType& b, const TypePtr& /*ptrA*/, const TypePtr& ptrB)
{
    if (!a.constructors_.empty() || !a.typeClasses.empty())
    {
        unifyError(a, b);
    }
    return { { a.id_, ptrB } };
}

void UnboundTypeVar::accept(TypeVisitor* visitor)
{
    visitor->visit(*this);
}

TypePtr UnboundTypeVar::copy(std::map<std::string, TypeConstantPtr>& typeConstants) const
{
    auto type           = std::make_shared<UnboundTypeVar>(id_);
    type->typeClasses   = typeClasses;
    type->constructors_ = constructors_;
    return type;
}

// ----------------------------------------------------------------------------

// BoundTypeVar

BoundTypeVar::BoundTypeVar(DataAst ast, TypeVarId id)
    : id_{ id },
      ast_{ ast }
{
}

std::string BoundTypeVar::str() const
{
    std::string base = ast_.importedModule->name() + ':' + ast_.data->name;
    if (!parameters_.empty())
    {
        base += '[';
        for (auto& param : parameters_)
        {
            base += param->str();
            base += ',';
        }
        base.back() = ']';
    }
    return SimpleType::printConstraints(std::move(base));
}

bool BoundTypeVar::equals(TypeVarId t) const
{
    return t == id_;
}

std::vector<Substitution> BoundTypeVar::unify(BoundTypeVar& a, UnboundTypeVar& b, const TypePtr& ptrA, const TypePtr& /*ptrB*/)
{
    for (auto& constructor : b.constructors_)
    {
        auto it = std::find_if(a.ast_.data->constructors.begin(), a.ast_.data->constructors.end(),
                               [&constructor](const ast::DataConstructor& astConstructor) { return astConstructor.name == constructor; });
        if (it == a.ast_.data->constructors.end())
        {
            unifyError(a, b);
        }
    }

    auto result = a.addConstraints(b);
    result.push_back({ b.id_, ptrA });
    return result;
}

std::vector<Substitution> BoundTypeVar::unify(BoundTypeVar& a, BoundTypeVar& b, const TypePtr& ptrA, const TypePtr& /*ptrB*/)
{
    if (a.ast_.data != b.ast_.data)
    {
        unifyError(a, b);
    }

    auto result = a.addConstraints(b);

    assert(a.parameters_.size() == b.parameters_.size());
    for (auto [aParam, bParam] : llvm::zip(a.parameters_, b.parameters_))
    {
        auto paramSubs = TypeUnifier::unify(aParam, bParam);
        result.insert(result.end(), paramSubs.begin(), paramSubs.end());
    }

    result.push_back({ b.id_, ptrA });
    return result;
}

std::vector<Substitution> BoundTypeVar::unify(BoundTypeVar& a, TypeConstant& b, const TypePtr& /*ptrA*/, const TypePtr& ptrB)
{
    if (a.ast_.data != b.ast_.data)
    {
        unifyError(a, b);
    }

    auto result = b.addConstraints(a);

    assert(a.parameters_.size() == b.parameters_.size());
    for (auto [aParam, bParam] : llvm::zip(a.parameters_, b.parameters_))
    {
        auto paramSubs = TypeUnifier::unify(aParam, bParam);
        result.insert(result.end(), paramSubs.begin(), paramSubs.end());
    }

    result.push_back({ a.id_, ptrB });
    return result;
}

std::vector<Substitution> BoundTypeVar::unify(BoundTypeVar& a, FunctionType& b, const TypePtr& /*ptrA*/, const TypePtr& /*ptrB*/)
{
    unifyError(a, b);
}

void BoundTypeVar::apply(Substitution s)
{
    for (auto& param : parameters_)
    {
        Type::apply(param, s);
    }
}

void BoundTypeVar::accept(TypeVisitor* visitor)
{
    visitor->visit(*this);
}

TypePtr BoundTypeVar::copy(std::map<std::string, TypeConstantPtr>& typeConstants) const
{
    auto result         = std::make_shared<BoundTypeVar>(ast_, id_);
    result->typeClasses = typeClasses;

    for (auto& param : parameters_)
    {
        result->parameters_.push_back(param->copy(typeConstants));
    }

    return result;
}

// ----------------------------------------------------------------------------

TypeConstant::TypeConstant(DataAst ast, std::string id)
    : id_{ std::move(id) },
      ast_{ ast }
{
}

std::string TypeConstant::str() const
{
    return SimpleType::printConstraints(id_);
}

std::vector<Substitution> TypeConstant::unify(TypeConstant& a, UnboundTypeVar& b, const TypePtr& ptrA, const TypePtr& /*ptrB*/)
{
    auto result = a.addConstraints(b);

    for (auto& constructor : b.constructors_)
    {
        auto it = std::find_if(a.ast_.data->constructors.begin(), a.ast_.data->constructors.end(),
                               [&constructor](const ast::DataConstructor& astConstructor) { return astConstructor.name == constructor; });
        if (it == a.ast_.data->constructors.end())
        {
            unifyError(a, b);
        }
    }

    result.push_back({ b.id_, ptrA });
    return result;
}

std::vector<Substitution> TypeConstant::unify(TypeConstant& a, BoundTypeVar& b, const TypePtr& ptrA, const TypePtr& /*ptrB*/)
{
    if (a.ast_.data != b.ast_.data)
    {
        unifyError(a, b);
    }

    auto result = a.addConstraints(b);

    assert(a.parameters_.size() == b.parameters_.size());
    for (auto [aParam, bParam] : llvm::zip(a.parameters_, b.parameters_))
    {
        auto resultParam = TypeUnifier::unify(aParam, bParam);
        result.insert(result.begin(), resultParam.begin(), resultParam.end());
    }

    result.push_back({ b.id_, ptrA });
    return result;
}

std::vector<Substitution> TypeConstant::unify(TypeConstant& a, TypeConstant& b, const TypePtr& /*ptrA*/, const TypePtr& /*ptrB*/)
{
    if (&a == &b)
    {
        return {};
    }
    if (a.id_ == b.id_)
    {
        assert(a.parameters_.size() == b.parameters_.size());
        for (auto [paramA, paramB] : llvm::zip(a.parameters_, b.parameters_))
        {
            auto subs = TypeUnifier::unify(paramA, paramB);
            if (!subs.empty())
            {
                unifyError(a, b);
            }
        }
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

TypePtr TypeConstant::copy(std::map<std::string, TypeConstantPtr>& typeConstants) const
{
    return copyConst(typeConstants);
}

TypeConstantPtr TypeConstant::copyConst(std::map<std::string, TypeConstantPtr>& typeConstants) const
{
    auto it = typeConstants.find(id_);
    if (it != typeConstants.end())
    {
        return it->second;
    }
    auto newObj        = std::make_shared<TypeConstant>(ast_, id_);
    typeConstants[id_] = newObj;
    SimpleType::copy(typeConstants, newObj.get());
    for (auto& param : parameters_)
    {
        newObj->parameters_.push_back(param->copyConst(typeConstants)); // :(
    }
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

std::vector<Substitution> FunctionType::unify(FunctionType& a, UnboundTypeVar& b, const TypePtr& ptrA, const TypePtr& /*ptrB*/)
{
    if (b.constructors_.empty() || b.typeClasses.empty())
    {
        return { { b.id_, ptrA } };
    }
    else
    {
        unifyError(a, b);
    }
}

std::vector<Substitution> FunctionType::unify(FunctionType& a, BoundTypeVar& b, const TypePtr& /*ptrA*/, const TypePtr& /*ptrB*/)
{
    unifyError(a, b);
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

TypePtr FunctionType::copy(std::map<std::string, TypeConstantPtr>& typeConstants) const
{
    return copyFun(typeConstants);
}

FunTypePtr FunctionType::copyFun(std::map<std::string, TypeConstantPtr>& typeConstants) const
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

void TypeUnifier::visit(UnboundTypeVar& self) { visit_(self); }
void TypeUnifier::visit(BoundTypeVar& self) { visit_(self); }
void TypeUnifier::visit(TypeConstant& self) { visit_(self); }
void TypeUnifier::visit(FunctionType& self) { visit_(self); }

// ----------------------------------------------------------------------------

TypeAnnotation::TypeAnnotation(
    std::map<const ast::Node*, TypePtr>      ast_,
    std::set<const ast::FieldExp*>           fieldExpressions_,
    std::map<std::string, TypePtr>           vars_,
    std::map<std::string, AnnotatedFunction> functions_,
    TypeVarId                                nextFreeVariable_)
    : ast{ std::move(ast_) },
      fieldExpressions{ std::move(fieldExpressions_) },
      variables{ std::move(vars_) },
      functions{ std::move(functions_) },
      nextFreeVariable{ nextFreeVariable_ }
{}

TypeAnnotation::TypeAnnotation(const TypeAnnotation& other)
{
    std::map<std::string, TypeConstantPtr> typeConstants;
    nextFreeVariable = other.nextFreeVariable;
    fieldExpressions = other.fieldExpressions;
    for (const auto& [node, type] : other.ast)
    {
        ast[node] = type->copy(typeConstants);
    }
    for (const auto& [name, type] : other.variables)
    {
        variables[name] = type->copy(typeConstants);
    }
    for (const auto& [name, fun] : other.functions)
    {
        functions[name] = { fun.typePtr->copyFun(typeConstants), fun.callExp };
    }
}

TypeAnnotation& TypeAnnotation::operator=(const TypeAnnotation& other)
{
    ast.clear();
    variables.clear();
    functions.clear();

    std::map<std::string, TypeConstantPtr> typeConstants;
    nextFreeVariable = other.nextFreeVariable;
    fieldExpressions = other.fieldExpressions;
    for (const auto& [node, type] : other.ast)
    {
        ast[node] = type->copy(typeConstants);
    }
    for (const auto& [name, type] : other.variables)
    {
        variables[name] = type->copy(typeConstants);
    }
    for (const auto& [name, fun] : other.functions)
    {
        functions[name] = { fun.typePtr->copyFun(typeConstants), fun.callExp };
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
    return functions.at(id).typePtr;
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
        funType.second.typePtr->apply(sub);
        // TypePtr ptr = funType.second;
        // Type::apply(ptr, sub);
    }
}

void TypeAnnotation::print() const
{
    for (auto& var : variables)
    {
        std::cout << var.first << " = " << var.second->str() << '\n';
    }
    for (auto& fun : functions)
    {
        std::cout << fun.first << " = " << fun.second.typePtr->str() << '\n';
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
        std::map<std::string, TypeConstantPtr> typeConstants;
        auto                                   type = inType->copy(typeConstants);
        TypeIdConverter                        converter{ nextFreeVariable };
        type->accept(&converter);
        return type;
    }

    static FunTypePtr convert(TypeVarId& nextFreeVariable, const FunTypePtr& funType)
    {
        std::map<std::string, TypeConstantPtr> typeConstants;
        auto                                   type = funType->copyFun(typeConstants);
        TypeIdConverter                        converter{ nextFreeVariable };
        type->accept(&converter);
        return type;
    }

    TypeIdConverter(TypeVarId& nextFreeVariable_)
        : nextFreeVariable{ nextFreeVariable_ }
    {}

    virtual void visit(UnboundTypeVar& t)
    {
        auto it = conversion.find(t.id_);
        if (it != conversion.end())
        {
            t.id_ = it->second;
        }
        else
        {
            conversion.insert({ t.id_, nextFreeVariable });
            t.id_ = nextFreeVariable++;
        }
    }

    virtual void visit(BoundTypeVar& t)
    {
        for (const auto& param : t.parameters_)
        {
            param->accept(this);
        }

        auto it = conversion.find(t.id_);
        if (it != conversion.end())
        {
            t.id_ = it->second;
        }
        else
        {
            conversion.insert({ t.id_, nextFreeVariable });
            t.id_ = nextFreeVariable++;
        }
    }

    virtual void visit(TypeConstant&)
    {
    }

    virtual void visit(FunctionType& funType)
    {
        for (const auto& t : funType.types)
        {
            t->accept(this);
        }
    }
};

} // namespace

void TypeAnnotation::addConstraint(const std::string& funName, const FunTypePtr& inType)
{
    const auto& annotatedFun          = functions.at(funName);
    auto        convertedFunctionType = TypeIdConverter::convert(nextFreeVariable, inType);
    auto        currentFunctionType   = annotatedFun.typePtr;

    std::vector<Constraint> constraints;

    assert(convertedFunctionType->types.size() == currentFunctionType->types.size());
    for (auto [inParam, currentParam] : llvm::zip(convertedFunctionType->types, currentFunctionType->types))
    {
        const auto location = annotatedFun.callExp != nullptr ? annotatedFun.callExp->location : SourceLocation{};
        constraints.push_back({ location, inParam, currentParam });
    }

    for (size_t i = 0; i < constraints.size(); ++i)
    {
        try
        {
            auto subs = constraints[i].solve();
            for (size_t j = 0; j < subs.size(); ++j)
            {
                substitute(subs[j]);
                for (size_t k = i + 1; k < constraints.size(); ++k)
                {
                    constraints[k].substitute(subs[j]);
                }
                for (size_t k = j + 1; k < subs.size(); ++k)
                {
                    // TODO: assert(subs[i].id != subs[j].id);
                    Type::apply(subs[k].type, subs[j]);
                }
            }
        }
        catch (const Error& error)
        {
            if (constraints[i].location.File != nullptr)
            {
                throw ErrorLocation{ constraints[i].location, error.what() };
            }
            else
            {
                throw;
            }
        }
    }
}

bool TypeAnnotation::addConstraint(const TypePtr& typeA, const TypeConstantPtr& typeConstant)
{
    // also do Constraint?

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

bool containsTypeVariable(const ast::TypeIdentifier& id, const std::map<std::string, TypePtr>& typeVariables)
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

const ast::DataConstructor& lookupConstructor(const ast::Data& astData, const GlobalIdentifier& identifier, SourceLocation location)
{
    if (astData.constructors.size() == 1)
    {
        if (astData.name == identifier.name)
        {
            return astData.constructors.front();
        }
    }
    else
    {
        for (const auto& con : astData.constructors)
        {
            if (con.name == identifier.name)
            {
                return con;
            }
        }
    }
    throw ErrorLocation{ location, std::string{ "unknown constructor: '" } + identifier.str() + '\'' };
}

} // namespace

// Maybe[int], Maybe[bool]... Maybe[a]?
TypePtr Annotator::typeFromIdentifier(const ast::TypeIdentifier& id, const std::map<std::string, TypePtr>& typeVariables)
{
    auto it = typeVariables.find(id.str());
    if (it != typeVariables.end())
    {
        return it->second;
    }

    if (id.identifier.moduleName.empty())
    {
        // must be a basic type
        assert(id.parameters.empty());
        return makeConst(id.identifier.name);
    }

    if (id.parameters.empty())
    {
        return makeConst(id.identifier);
    }
    else if (!containsTypeVariable(id, typeVariables))
    {
        return makeConst(id);
    }
    else
    {
        auto dataAst = astModule_->lookupType(id.identifier);
        if (dataAst.empty())
        {
            throw Error("Annotator::typeFromIdentifier astModule_->lookupType");
        }

        auto typePtr = makeVar(dataAst);
        for (auto& param : id.parameters)
        {
            typePtr->parameters_.push_back(typeFromIdentifier(param, typeVariables));
        }

        return typePtr;
    }
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
        auto argTV          = arg->type.empty() ? static_cast<TypePtr>(makeVar()) : typeFromIdentifier(arg->type); // what if arg->type has typeParameters?
        // TODO: nested type parameters
        auto [it, inserted] = variables.insert({ arg->identifier, argTV });
        if (!inserted)
        {
            throw ErrorLocation(arg->location, std::string{ "duplicate parameter \"" } + arg->identifier + '"');
        }
        args.push_back(std::move(argTV));
    }

    functions.insert({ std::move(astModule_->name() + ':' + fun.name), { makeFunction(std::move(args)), nullptr } });

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
    auto astData = annotator.astModule_->lookupConstructor(pattern.identifier);
    assert(!astData.empty());
    auto& constructor = lookupConstructor(*astData.data, pattern.identifier, pattern.location);

    SimpleTypePtr patternType;
    if (astData.data->typeVariables.empty())
    {
        patternType = annotator.makeConst(GlobalIdentifier{ astData.importedModule->name(), astData.data->name });
    }
    else
    {
        auto varTypePtr = annotator.makeVar(astData);
        for (auto& typeVarName : astData.data->typeVariables)
        {
            auto typeVarPtr = annotator.makeVar();
            varTypePtr->parameters_.push_back(std::move(typeVarPtr));
        }
        patternType = std::move(varTypePtr);
    }

    add(pattern, patternType);
    for (auto& enumer : llvm::enumerate(pattern.arguments))
    {
        auto& arg = enumer.value();

        arg.pattern->accept(this);

        auto astField          = constructor.fields.at(enumer.index());
        auto fieldType         = annotator.typeFromIdentifier(astField.type);
        annotator.result[&arg] = annotator.makeVar();

        annotator.constraints.push_back({ arg.location, std::move(fieldType), annotator.tv(arg) });

        // TODO: add constraints for parameters...
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
        functions[std::move(funName)] = { funType, &exp };
    }
    else
    {
        add({ exp.location, funType, it->second.typePtr });
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
        functions[std::move(varName)] = { makeFunction({ ptr }), nullptr };
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
    add({ exp.location, tv(exp.lhs), t });

    fieldExpressions.insert(&exp);

    // make constraint for parameters...
}

void Annotator::visit(ast::ConstructorExp& exp)
{
    auto astData = astModule_->lookupConstructor(exp.identifier);
    assert(!astData.empty());
    auto& constructor = lookupConstructor(*astData.data, exp.identifier, exp.location);
    if (constructor.fields.size() != exp.arguments.size())
    {
        throw ErrorLocation{ exp.location, "incorrect number of arguments" };
    }

    std::map<std::string, TypePtr> typeVariables;

    SimpleTypePtr expTV;
    if (astData.data->typeVariables.empty())
    {
        expTV = makeConst(GlobalIdentifier{ astData.importedModule->name(), astData.data->name });
    }
    else
    {
        auto varTypePtr = makeVar(astData);
        if (!astData.data->typeVariables.empty())
        {
            std::string dataTypeName = astData.importedModule->name() + ':' + astData.data->name + '[';
            for (auto& typeVarName : astData.data->typeVariables)
            {
                auto typeVarPtr            = makeVar();
                typeVariables[typeVarName] = typeVarPtr;
                varTypePtr->parameters_.push_back(std::move(typeVarPtr));

                dataTypeName += typeVarName;
                dataTypeName += ',';
            }
            dataTypeName.back()                    = ']';
            typeVariables[std::move(dataTypeName)] = varTypePtr;
        }

        expTV = std::move(varTypePtr);
    }

    result[&exp] = expTV;

    for (const auto& [field, arg] : llvm::zip(constructor.fields, exp.arguments))
    {
        arg.exp->accept(this);
        auto argType   = tv(arg.exp);
        auto fieldType = typeFromIdentifier(field.type, typeVariables); // the name of the constants made are broken...
        add({ arg.location, argType, fieldType });
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

UnboundTypeVarPtr Annotator::makeVar()
{
    return std::make_shared<UnboundTypeVar>(current++);
}

BoundTypeVarPtr Annotator::makeVar(DataAst ast)
{
    return std::make_shared<BoundTypeVar>(ast, current++);
}

TypeConstantPtr Annotator::makeConst(llvm::StringRef stringRef)
{
    assert(!stringRef.contains(':'));

    std::string str{ stringRef.str() };
    auto        it = typeConstants.find(str);
    if (it == typeConstants.end())
    {
        auto& constant = typeConstants[str];
        constant       = std::make_shared<TypeConstant>(DataAst{}, std::move(str));
        return constant;
    }
    else
    {
        return it->second;
    }
}

TypeConstantPtr Annotator::makeConst(const ast::TypeIdentifier& id)
{
    if (id.identifier.moduleName.empty())
    {
        assert(id.parameters.empty());
        return makeConst(id.identifier.name);
    }
    auto idStr = id.str();
    auto it    = typeConstants.find(idStr);
    if (it == typeConstants.end())
    {
        auto dataAst = astModule_->lookupType(id.identifier);
        assert(!dataAst.empty());
        auto& constant = typeConstants[idStr];
        constant       = std::make_shared<TypeConstant>(dataAst, std::move(idStr));
        for (auto& param : id.parameters)
        {
            constant->parameters_.push_back(makeConst(param));
        }
        return constant;
    }
    else
    {
        return it->second;
    }
}

TypeConstantPtr Annotator::makeConst(const GlobalIdentifier& id)
{
    if (id.moduleName.empty())
    {
        return makeConst(id.name);
    }
    auto idStr = id.str();
    auto it    = typeConstants.find(idStr);
    if (it == typeConstants.end())
    {
        auto dataAst = astModule_->lookupType(id);
        assert(!dataAst.empty());
        assert(dataAst.data->typeVariables.empty()); // should call makeConst(ast::TypeIdentifier) variant

        auto& constant = typeConstants[idStr];
        constant       = std::make_shared<TypeConstant>(dataAst, std::move(idStr));
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
        return functions.at(name).typePtr;
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

std::string test(ImportedModule* mod, ast::Function& fun)
{
    Annotator annotator{ mod };
    annotator(fun);

    TypeAnnotation          annotation{ std::move(annotator.result), std::move(annotator.fieldExpressions), std::move(annotator.variables), std::move(annotator.functions), annotator.current };
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
    return annotation.getFun(mod->name() + ':' + fun.name)->str();
}

TypeAnnotation inferType(const ImportedModule* astModule, const ast::Function& fun)
{
    Annotator annotator{ astModule };
    annotator(fun);

    TypeAnnotation          annotation{ std::move(annotator.result), std::move(annotator.fieldExpressions), std::move(annotator.variables), std::move(annotator.functions), annotator.current };
    std::vector<Constraint> constraints{ std::move(annotator.constraints) }; // should be a set!

    for (size_t i = 0; i < constraints.size(); ++i)
    {
        try
        {
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
        catch (const Error& error)
        {
            throw ErrorLocation{ constraints[i].location, error.what() };
        }
    }

    return annotation;
}

namespace
{

TypePtr addTypeInFunctionDeclaration(std::map<std::string, TypePtr>& typeMap, const ImportedModule* mod, const ast::TypeIdentifier& argTypeId)
{
    std::string argType = argTypeId.str();
    auto        it      = typeMap.find(argType);
    if (it == typeMap.end())
    {
        if (argTypeId.identifier.moduleName.empty())
        {
            auto type = std::make_shared<TypeConstant>(DataAst{}, argTypeId.identifier.name);
            it        = typeMap.insert(std::make_pair(std::move(argType), std::move(type))).first;
        }
        else
        {
            DataAst astdata = mod->lookupType(argTypeId.identifier);
            auto    type    = std::make_shared<BoundTypeVar>(astdata, static_cast<hm::TypeVarId>(typeMap.size()));
            for (auto& param : argTypeId.parameters)
            {
                type->parameters_.push_back(addTypeInFunctionDeclaration(typeMap, mod, param));
            }
            it = typeMap.insert(std::make_pair(std::move(argType), std::move(type))).first;
        }
    }
    return it->second;
}

} // namespace

FunTypePtr inferType(const ImportedModule* mod, const ast::Class& class_, const ast::FunctionDeclaration& ast)
{
    std::map<std::string, TypePtr> typeMap;

    typeMap[class_.typeVariable] = std::make_shared<UnboundTypeVar>(0);

    std::vector<TypePtr> functionTypes;
    functionTypes.push_back(addTypeInFunctionDeclaration(typeMap, mod, ast.type));
    for (auto& arg : ast.parameters)
    {
        functionTypes.push_back(addTypeInFunctionDeclaration(typeMap, mod, arg->type));
    }

    return std::make_shared<FunctionType>(std::move(functionTypes));
}

} // namespace llfp::hm
