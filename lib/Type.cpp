
#include <algorithm>
#include <iterator>
#include <utility>

#pragma warning(push, 0)

#include "llvm/Support/FormatVariadic.h"

#pragma warning(pop)

#include "Log.h"
#include "Module.h"

#include "Type.h"


namespace llfp
{
namespace type
{

namespace
{

bool checkBasicType(const Identifier& id, llvm::StringRef name)
{
    return id.parameters.empty() && id.name.moduleName.empty() && id.name.name == name;
}

} // namespace

std::string Identifier::str() const
{
    std::string result = name.str();

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

Type::Type(Identifier identifier, llvm::Type* llvmType, bool isSigned) :
    identifier_{ std::move(identifier) },
    llvmType_{ llvmType },
    isSigned_{ isSigned }
{
}

Type::Type(Identifier identifier, bool isFloating, bool isSigned) :
    identifier_{ std::move(identifier) },
    llvmType_{ nullptr },
    isSigned_{ isSigned }
    // isFloating for now we check literal type name instead
{
}

Type::Type(std::unordered_set<TypeClass*> typeClasses_):
    llvmType_{nullptr},
    isSigned_{false},
    typeClasses{std::move(typeClasses_)}
{
}

Type::~Type() {}

const Identifier& Type::identifier() const
{
    return identifier_;
}

const GlobalIdentifier& Type::baseName() const
{
    return identifier_.name;
}

llvm::Type* Type::llvmType() const
{
    return llvmType_;
}

bool Type::isBool() const
{
    return checkBasicType(identifier_, name::Bool);
}

bool Type::isNum() const
{
    return isInteger() || isFloating();
}

bool Type::isInteger() const
{
    if (llvmType_ != nullptr)
    {
        return llvmType_->isIntegerTy() && !isBool();
    }
    return checkBasicType(identifier_, name::IntegerLiteral) || checkBasicType(identifier_, name::SignedIntegerLiteral);
}

bool Type::isFloating() const
{
    if (llvmType_ != nullptr)
    {
        return llvmType_->isFloatingPointTy();
    }
    return checkBasicType(identifier_, name::FloatingLiteral);
}

bool Type::isSigned() const
{
    // the llvm type does not tell us if we are signed or not
    return isSigned_;
}

bool Type::isLiteral() const
{
    return !identifier_.name.name.empty() && identifier_.name.name.front() == '@';
}

bool Type::isConcreteType() const
{
    // we could use llvmType_ != nullptr but we might want to move llvmType_ somewhere else
    return !(isLiteral() || checkBasicType(identifier_, name::Any));
}

bool Type::isBound() const
{
    return !identifier_.name.name.empty();
}

unsigned int Type::getTypeParameterCount() const
{
    return 0;
}

TypePtr Type::getTypeParameter(unsigned int index) const
{
    assert(false);
    return nullptr;
}

bool Type::isStructType() const
{
    return false;
}

unsigned int Type::getFieldIndex(const std::string&) const
{
    return InvalidIndex;
}

unsigned int Type::getFieldCount() const
{
    return 0;
}

TypePtr Type::getFieldType(unsigned int) const
{
    assert(false);
    return nullptr;
}

const std::unordered_set<TypeClass*>& Type::getTypeClasses() const
{
    return typeClasses;
}


TypePtr TypeContext::unifyLiterals(const TypePtr& a, const TypePtr& b)
{
    assert(a->isLiteral() || b->isLiteral());
    assert(a != b);

    if (a->isConcreteType())
    {
        return unifyConcreteAndLiteral(a, b);
    }
    else if (b->isConcreteType())
    {
        return unifyConcreteAndLiteral(b, a);
    }

    // TODO: unify to smallest bitsize
    if (a->isFloating() || b->isFloating())
    {
        return getFloatingLiteralType();
    }
    if (a->isSigned() || b->isSigned())
    {
        return getSignedIntegerLiteralType();
    }
    else
    {
        return getIntegerLiteralType();
    }
}

TypePtr TypeContext::unifyConcreteAndLiteral(const TypePtr& concreteType, const TypePtr& literalType)
{
    assert(concreteType->isConcreteType());
    assert(literalType->isLiteral());
    assert(concreteType != literalType);

    if (concreteType->isStructType() ||
        concreteType->isBool())
    {
        return nullptr;
    }

    // all literal types of ok to combine with floats
    if (concreteType->isFloating())
    {
        return concreteType;
    }
    // if the concrete type is not float the literal cant be float
    if (literalType->isFloating())
    {
        return nullptr;
    }
    if (literalType->isSigned() && !concreteType->isSigned())
    {
        return nullptr;
    }

    return concreteType;
}

TypePtr TypeContext::unifyCheckTypeClass(const TypePtr& a, const TypePtr& b)
{
    if (!a->isBound() && !b->isBound())
    {
        auto typeClassUnion = a->getTypeClasses();
        auto& bTypeClasses = b->getTypeClasses();
        typeClassUnion.insert(bTypeClasses.begin(), bTypeClasses.end());
        return std::make_shared<Type>(std::move(typeClassUnion));
    }

    auto check = [](const TypePtr& bound, const TypePtr& typeClass) -> TypePtr
    {
        auto& boundTypeClasses = bound->getTypeClasses();
        auto& checkTypeClasses = typeClass->getTypeClasses();
        if (std::all_of(checkTypeClasses.begin(), checkTypeClasses.end(),
            [&boundTypeClasses](TypeClass* x) {return boundTypeClasses.find(x) != boundTypeClasses.end(); }))
        {
            return bound;
        }
        return nullptr;
    };

    return b->isBound() ? check(b, a) : check(a, b);
}

TypePtr TypeContext::unifyStructTypes(const TypePtr& a, const TypePtr& b)
{
    assert(a->isStructType());
    assert(b->isStructType());

    if (a->baseName() != b->baseName())
    {
        return nullptr;
    }

    assert(a->getFieldCount() == b->getFieldCount());
    assert(a->getTypeParameterCount() == b->getTypeParameterCount());

    std::vector<TypePtr> typeParams;
    for (unsigned int i = 0; i < a->getTypeParameterCount(); i++)
    {
        typeParams.push_back(unify(a->getTypeParameter(i), b->getTypeParameter(i)));
        if (typeParams.back() == nullptr)
        {
            return nullptr;
        }
    }

    // TODO: optimization? check if a or b is concrete and return that one directly?

    std::vector<Identifier> parameters;
    for (auto& param : typeParams)
    {
        parameters.push_back(param->identifier());
    }
    Identifier typeId{ a->baseName(), std::move(parameters) };

    for (auto& param : typeParams)
    {
        if (!param->isConcreteType())
        {
            // TODO: we dont need to unify the fields, we just have to replace fields with the unified type parameters
            std::vector<TypePtr> fields;
            for (unsigned int i = 0; i < a->getFieldCount(); i++)
            {
                fields.push_back(unify(a->getFieldType(i), b->getFieldType(i)));
            }
            return std::make_shared<StructType>(std::move(typeId), a->getAst(), fields);
        }
    }

    // concrete after unifying parameters, we can do lookup on identifier
    return getType(typeId);
}

TypePtr TypeContext::unify(const TypePtr& a, const TypePtr& b)
{
    auto error = [&a, &b]()
    {
        Log({}, "failed to unify types, '", a->identifier().str(), "' with '", b->identifier().str(), '\'');
        return nullptr;
    };

    auto checkError = [&error](TypePtr a) { return a ? a : error(); };

    if (a == nullptr || b == nullptr)
    {
        return nullptr;
    }
    if (a == b)
    {
        return a;
    }

    // a != b
    if (!a->isBound() || !b->isBound())
    {
        return unifyCheckTypeClass(a, b);
    }
    else if (a->isConcreteType() && b->isConcreteType())
    {
        // both are bound and both are concrete but they are not equal
        return error();
    }
    else if (a->isLiteral() || b->isLiteral())
    {
        // both bound, one or two literal
        return unifyLiterals(a, b);
    }
    else // both bound and not literal, one is abstract
    {
        if (a->baseName() == b->baseName())
        {
            // since a != b they can't be primitive types
            return unifyStructTypes(a, b);
        }
        else
        {
            return error();
        }
    }
}

TypePtr TypeContext::fix(const TypePtr& t)
{
    if (t->isConcreteType())
    {
        return t;
    }
    if (t->isLiteral())
    {
        if (t->isFloating())
        {
            return getDouble();
        }
        else if (t->isSigned())
        {
            return getI64();
        }
        else
        {
            return getU64();
        }
    }
    if (t->isBound())
    {
        // TODO: fix fields that are possibly literals
        Log({}, "fix bound type not supported");
    }
    Log({}, "failed to fix type, not concrete or literal");
    // TODO: can unbound be fixed to void? can it be proven it will never be used?
    return nullptr;
}


StructType::StructType(Identifier identifier, const ast::DataDeclaration* ast_, llvm::StructType* llvmType) :
    Type(std::move(identifier), llvmType),
    llvmType_{ llvmType },
    ast{ ast_ }
{
}

StructType::StructType(Identifier identifier, const ast::DataDeclaration* ast_, std::vector<TypePtr> fieldTypes) :
    Type(std::move(identifier), false),
    llvmType_{ nullptr },
    ast{ ast_ }
    //fields{std::move(fieldTypes)}
{
    setFields(std::move(fieldTypes));
}

bool StructType::isConcreteType() const
{
    // TODO, check params instead
    return std::all_of(fields.begin(), fields.end(), [](const TypePtr& field) { return field->isConcreteType(); });
}

unsigned int StructType::getTypeParameterCount() const
{
    return static_cast<unsigned int>(ast->typeVariables.size());
}

TypePtr StructType::getTypeParameter(unsigned int index) const
{
    assert(index < parameters.size());
    return parameters[index];
}

bool StructType::isStructType() const
{
    return true;
}

unsigned int StructType::getFieldIndex(const std::string& fieldIdentifier) const
{
    assert(ast->fields.size() == fields.size());
    const unsigned int size = static_cast<unsigned int>(ast->fields.size());
    for (unsigned int i = 0; i < size; ++i)
    {
        if (ast->fields[i].name == fieldIdentifier) { return i; }
    }
    return InvalidIndex;
}

TypePtr StructType::getFieldType(unsigned int index) const
{
    assert(index < fields.size());
    return fields[index];
}

unsigned int StructType::getFieldCount() const
{
    return static_cast<unsigned int>(fields.size());
}

void StructType::setFields(std::vector<TypePtr> fieldTypes)
{
    assert(fields.size() == 0);
    assert(parameters.size() == 0);
    assert(ast->fields.size() == fieldTypes.size());

    for (auto& typeVar : ast->typeVariables)
    {
        const auto findType = [&typeVar](const ast::TypeIdentifier& typeId, const TypePtr &type) -> TypePtr
        {
            auto findType_impl = [&typeVar](const ast::TypeIdentifier& typeId, const TypePtr& type, auto& find_ref) -> TypePtr
            {
                if (typeId.identifier.moduleName.empty() && typeId.identifier.name == typeVar)
                {
                    assert(typeId.parameters.empty());
                    return type;
                }

                assert(typeId.parameters.size() == type->getTypeParameterCount());
                for (unsigned int i = 0; i < typeId.parameters.size(); ++i)
                {
                    auto result = find_ref(typeId.parameters[i], type->getTypeParameter(i), find_ref);
                    if (result != nullptr) { return result; }
                }

                return nullptr;
            };
            return findType_impl(typeId, type, findType_impl);
        };

        TypePtr result = nullptr;
        for (unsigned int i = 0 ; i < fieldTypes.size(); ++i)
        {
            result = findType(ast->fields[i].type, fieldTypes[i]);
            if (result != nullptr) { break; }
        }
        assert(result != nullptr);
        parameters.push_back(result);
    }

    if (llvmType_ != nullptr)
    {
        std::vector<llvm::Type*> llvmTypes;
        std::transform(fieldTypes.begin(), fieldTypes.end(), std::back_inserter(llvmTypes),
            [](const TypePtr& type) {return type->llvmType(); });

        llvmType_->setBody(llvmTypes);
    }
    fields = std::move(fieldTypes);
}

const ast::DataDeclaration* StructType::getAst() const
{
    return ast;
}

#define ADD_TYPE(id, getF, sign) (*types.insert({ Identifier{{"",id},{}}, std::make_shared<Type>(Identifier{{"",id},{}}, llvm::Type::getF(llvmContext), sign) }).first).second
#define ADD_TYPE_L(id, floating, sign) (*types.insert({ Identifier{{"",id},{}}, std::make_shared<Type>(Identifier{{"",id},{}}, floating, sign) }).first).second

TypeContext::TypeContext(llvm::LLVMContext& llvmContext_, SourceModule* sourceModule_) :
    llvmContext{ llvmContext_ },
    sourceModule{ sourceModule_ }
{
    boolType = ADD_TYPE("bool", getInt1Ty, false);

    i8Type   = ADD_TYPE("i8",   getInt8Ty,   true);
    i16Type  = ADD_TYPE("i16",  getInt16Ty,  true);
    i32Type  = ADD_TYPE("i32",  getInt32Ty,  true);
    i64Type  = ADD_TYPE("i64",  getInt64Ty,  true);
    i128Type = ADD_TYPE("i128", getInt128Ty, true);

    u8Type   = ADD_TYPE("u8",   getInt8Ty,   false);
    u16Type  = ADD_TYPE("u16",  getInt16Ty,  false);
    u32Type  = ADD_TYPE("u32",  getInt32Ty,  false);
    u64Type  = ADD_TYPE("u64",  getInt64Ty,  false);
    u128Type = ADD_TYPE("u128", getInt128Ty, false);

    halfType   = ADD_TYPE("half",   getHalfTy,   true);
    floatType  = ADD_TYPE("float",  getFloatTy,  true);
    doubleType = ADD_TYPE("double", getDoubleTy, true);

    charType = ADD_TYPE("char", getInt8Ty, false);

    anyType                  = ADD_TYPE_L("",                      false, false);
    integerLiteralType       = ADD_TYPE_L("@IntegerLiteral",       false, false);
    signedIntegerLiteralType = ADD_TYPE_L("@SignedIntegerLiteral", false, true);
    floatingLiteralType      = ADD_TYPE_L("@FloatingLiteral",      true,  true);
}

/**
Does a lookup of type based on the imports of the sourceModule of the TypeContext. Fully qualifies the type and makes sure all types
are visible from the sourceModule before calling getType.
*/
TypePtr TypeContext::getTypeFromAst(const ast::TypeIdentifier& identifier)
{
    return getTypeFromAst(identifier, sourceModule);
}

TypePtr TypeContext::getTypeFromAst(const ast::TypeIdentifier& identifier, const ImportedModule* lookupModule)
{
    Identifier id;
    if (fullyQualifiedName(id, identifier, lookupModule))
    {
        return getType(id);
    }
    Log({}, "failed to fully qualify name: ", identifier.str());
    return nullptr;
}

bool TypeContext::fullyQualifiedName(Identifier& identifier, const ast::TypeIdentifier& tid, const ImportedModule* lookupModule)
{
    // check primitve type
    if (tid.parameters.empty() && tid.identifier.moduleName.empty())
    {
        Identifier id{ tid.identifier, {} };
        if (types.find(id) != types.end())
        {
            identifier = std::move(id);
            return true;
        }
    }

    const ImportedModule* astModule;
    const llfp::ast::DataDeclaration* ast;
    std::tie(astModule, ast) = lookupModule->lookupType(tid.identifier);
    if (astModule == nullptr || ast == nullptr)
    {
        return false;
    }

    identifier.name = {astModule->name(), ast->name};
    assert(identifier.parameters.size() == 0);
    for (auto& param : tid.parameters)
    {
        identifier.parameters.push_back({});
        if (!fullyQualifiedName(identifier.parameters.back(), param, lookupModule))
        {
            return false;
        }
    }
    return true;
}

/**
Get or create a type. The identifier is assumed to be fully qualified and of a concrete type.
*/
TypePtr TypeContext::getType(const Identifier& identifier)
{
    {
        auto it = types.find(identifier);
        if (it != types.end())
        {
            return it->second;
        }
    }

    const ImportedModule* astModule;
    const llfp::ast::DataDeclaration* ast;
    std::tie(astModule, ast) = sourceModule->lookupTypeGlobal(identifier.name);
    if (astModule != nullptr && ast != nullptr)
    {
        if (ast->typeVariables.size() != identifier.parameters.size())
        {
            Log({}, "type arity mismatch between ", identifier.str(), " and ", astModule->name(), ':', ast->name, '/', ast->typeVariables.size());
            return nullptr;
        }

        auto llvmType = llvm::StructType::create(llvmContext, "");
        auto typePtr = std::make_shared<StructType>(identifier, ast, llvmType);
        auto it2 = types.insert({ identifier, typePtr });

        if (!it2.second)
        {
            // shouldn't happen, because we do a find before
            Log({}, "duplicate type");
            return nullptr;
        }

        // generate body

        std::map<std::string, Identifier> typeVariables; // I think we need to build this
        for (int i = 0; i < ast->typeVariables.size(); ++i)
        {
            typeVariables[ast->typeVariables[i]] = identifier.parameters[i];
        }

        std::vector<TypePtr> fieldTypes;
        for (auto& field : ast->fields)
        {
            TypePtr fieldType;
            if (field.type.identifier.moduleName.empty())
            {
                auto it = typeVariables.find(field.type.identifier.name);
                if (it != typeVariables.end())
                {
                    fieldType = getType(it->second);
                }
            }
            if (fieldType == nullptr)
            {
                fieldType = getTypeFromAst(field.type, astModule);
            }

            if (fieldType == nullptr)
            {
                Log(field.location, "unknown type: ", field.type.identifier.str());
                return nullptr;
            }
            fieldTypes.push_back(std::move(fieldType));
        }

        llvmType->setName(astModule->getMangledName(ast, fieldTypes));
        typePtr->setFields(std::move(fieldTypes));

        return typePtr;
    }

    Log({}, "unknown data ", identifier.name.str());
    return nullptr;
}

bool TypeContext::equals(const TypePtr& type, const ast::TypeIdentifier& identifier)
{
    if (type == nullptr)
    {
        return false;
    }

    Identifier id;
    if (fullyQualifiedName(id, identifier, sourceModule))
    {
        return type->identifier() == id;
    }
    return false;
}

bool TypeContext::equals(const TypePtr& type, llvm::StringRef identifier)
{
    if (type == nullptr)
    {
        return false;
    }

    return checkBasicType(type->identifier(), identifier);
}

TypePtr TypeContext::getBool() { return boolType; }
TypePtr TypeContext::getChar() { return charType; }
TypePtr TypeContext::getI64() { return i64Type; }
TypePtr TypeContext::getU64() { return u64Type; }
TypePtr TypeContext::getDouble() { return doubleType; }
TypePtr TypeContext::getAnyType() { return anyType; }
TypePtr TypeContext::getIntegerLiteralType() { return integerLiteralType; }
TypePtr TypeContext::getSignedIntegerLiteralType() { return signedIntegerLiteralType; }
TypePtr TypeContext::getFloatingLiteralType() { return floatingLiteralType; }


EmptyTypeScope::EmptyTypeScope(TypeScope* parent_) :
    parent{ parent_ }
{
}

TypeContext* EmptyTypeScope::getTypeContext()
{
    return parent->getTypeContext();
}

TypePtr EmptyTypeScope::getVariableType(const std::string&)
{
    return nullptr;
}

FunAst EmptyTypeScope::getFunctionAST(const GlobalIdentifier& identifier)
{
    return parent->getFunctionAST(identifier);
}

DataAst EmptyTypeScope::getDataAST(const GlobalIdentifier& identifier)
{
    return parent->getDataAST(identifier);
}


TypeInferer::TypeInferer(TypeScope* scope_) :
    env{ scope_ }
{
    assert(env != nullptr);
}

TypePtr TypeInferer::infer(ast::Exp& exp, TypeScope* scope_)
{
    TypeInferer inferer(scope_);
    exp.accept(&inferer);
    return inferer.result;
}

void TypeInferer::visit(ast::LetExp& exp)
{
    for (auto& var : exp.letStatments)
    {
        if (var->parameters.size() != 0)
        {
            Log(exp.location, "let function definitions not implemented");
            return;
        }

        TypePtr varType;
        if (var->type.identifier.name.empty())
        {
            varType = TypeInferer::infer(*var->functionBody, this);
        }
        else
        {
            varType = getTypeContext()->getTypeFromAst(var->type); // FIXME: if we're infering function call body sourceModule is no the correct one
        }
        if (varType == nullptr)
        {
            return;
        }

        if (variables.find(var->name) != variables.end())
        {
            Log(exp.location, var->name, " already defined");
            return;
        }
        else
        {
            variables[var->name] = varType;
        }
    }

    result = TypeInferer::infer(*exp.exp, this);
}

void TypeInferer::visit(ast::IfExp& exp)
{
    result = getTypeContext()->unify(
        TypeInferer::infer(*exp.thenExp, this),
        TypeInferer::infer(*exp.elseExp, this));
}

void TypeInferer::visit(ast::CaseExp& exp)
{
    Log(exp.location, "case not implemented");
}

namespace
{

TypePtr inferMathExp(TypeInferer* inferer, ast::BinaryExp& exp)
{
    auto lhs = TypeInferer::infer(*exp.lhs, inferer);
    auto rhs = TypeInferer::infer(*exp.rhs, inferer);
    auto result = inferer->getTypeContext()->unify(lhs, rhs);
    if (result != nullptr)
    {
        if (result->isNum())
        {
            return result;
        }
        else
        {
            Log(exp.location, "operand of math expression is not a number");
        }
    }
    else
    {
        Log(exp.location, "failed to unify types, '", lhs->identifier().str(), "' and '", rhs->identifier().str(), '\'');
    }
    return nullptr;
}

TypePtr inferBitExp(TypeInferer* inferer, ast::BinaryExp& exp)
{
    auto lhs = TypeInferer::infer(*exp.lhs, inferer);
    auto rhs = TypeInferer::infer(*exp.rhs, inferer);
    auto result = inferer->getTypeContext()->unify(lhs, rhs);
    if (result != nullptr)
    {
        if (result->isInteger())
        {
            return result;
        }
        else
        {
            Log(exp.location, "operand of bitwise expression is not an integer");
        }
    }
    return nullptr;
}

} // namespace

void TypeInferer::visit(ast::BinaryExp& exp)
{
    // math
    if (exp.op == "*") { result = inferMathExp(this, exp); } // Multiplication
    else if (exp.op == "/") { result = inferMathExp(this, exp); } // Division
    else if (exp.op == "%") { result = inferMathExp(this, exp); } // Remainder
    else if (exp.op == "+") { result = inferMathExp(this, exp); } // Addition
    else if (exp.op == "-") { result = inferMathExp(this, exp); } // Subtraction
    // bitwise
    else if (exp.op == "<<") { result = inferBitExp(this, exp); } // Shift
    else if (exp.op == ">>") { result = inferBitExp(this, exp); } // Signed shift
    else if (exp.op == ">>>") { result = inferBitExp(this, exp); } // Logical shift
    // compare
    else if (exp.op == ">") { result = getTypeContext()->getBool(); } // Greater than
    else if (exp.op == ">=") { result = getTypeContext()->getBool(); } // Greater or equal
    else if (exp.op == "<") { result = getTypeContext()->getBool(); } // Less than
    else if (exp.op == "<=") { result = getTypeContext()->getBool(); } // Less or equal
    else if (exp.op == "==") { result = getTypeContext()->getBool(); } // Equal
    else if (exp.op == "!=") { result = getTypeContext()->getBool(); } // Not equal
    // bitwise
    else if (exp.op == "&") { result = inferBitExp(this, exp); }
    else if (exp.op == "|") { result = inferBitExp(this, exp); }
    else if (exp.op == "^") { result = inferBitExp(this, exp); }
    // logical
    else if (exp.op == "&&") { result = getTypeContext()->getBool(); }
    else if (exp.op == "||") { result = getTypeContext()->getBool(); }
    else
    {
        Log(exp.location, "unknown operator: ", exp.op);
    }
}

void TypeInferer::visit(ast::UnaryExp& exp)
{
    assert(exp.op == "-");
    result = TypeInferer::infer(*exp.operand, this);
    // type check number?
    if (getTypeContext()->getIntegerLiteralType() == result) //getTypeContext()->equals(result, name::IntegerLiteral))
    {
        result = getTypeContext()->getSignedIntegerLiteralType();
    }
}

void TypeInferer::visit(ast::LiteralExp& exp)
{
    switch (exp.tokenType)
    {
    case lex::tok_integer:

        if (exp.value.front() == '-')
        {
            result = getTypeContext()->getSignedIntegerLiteralType();
        }
        else
        {
            result = getTypeContext()->getIntegerLiteralType();
        }
        // or signed
        break;

    case lex::tok_float:

        result = getTypeContext()->getFloatingLiteralType();
        break;

    case lex::tok_char:

        result = getTypeContext()->getChar();
        // add type classes?
        break;

    case lex::tok_string:

        Log(exp.location, "string not supported");
        break;

    case lex::tok_bool:

        result = getTypeContext()->getBool();
        break;

    default:

        assert(false);
        break;
    }
}

void TypeInferer::visit(ast::CallExp& exp)
{
    const ast::Function* ast;
    ImportedModule* astModule;
    std::tie(astModule, ast) = env->getFunctionAST(exp.identifier);

    if (ast != nullptr)
    {
        if (ast->type.identifier.name.empty())
        {
            if (exp.arguments.size() != ast->parameters.size())
            {
                Log(exp.location, "parameter count mismatch");
                return;
            }

            EmptyTypeScope scope(this);
            TypeInferer inferer(&scope);
            for (int i = 0; i < exp.arguments.size(); ++i)
            {
                auto type = TypeInferer::infer(*exp.arguments[i].get(), this);
                if (type == nullptr)
                {
                    Log(exp.arguments[i]->location, "failed to infer type of argument");
                    return;
                }
                inferer.variables[ast->parameters[i]->identifier] = std::move(type);
            }

            //FIXME: standard library does not have bodies?
            ast->functionBody->accept(&inferer);
            result = inferer.result;
        }
        else
        {
            result = env->getTypeByName(ast->type, astModule);
        }
    }
}

void TypeInferer::visit(ast::VariableExp& exp)
{
    if (exp.identifier.moduleName.empty())
    {
        result = getVariableType(exp.identifier.name);
        if (result != nullptr)
        {
            return;
        }
    }

    // not a local variable

    const ast::Function* ast;
    ImportedModule* astModule;
    std::tie(astModule, ast) = env->getFunctionAST(exp.identifier);
    if (astModule != nullptr && ast != nullptr)
    {
        if (ast->type.identifier.name.empty())
        {
            if (0 != ast->parameters.size())
            {
                Log(exp.location, "constant does not take arguments");
                return;
            }

            EmptyTypeScope scope(this);
            TypeInferer inferer(&scope);

            //FIXME: standard library does not have bodies?
            ast->functionBody->accept(&inferer);
            result = inferer.result;
        }
        else
        {
            result = env->getTypeByName(ast->type, astModule);
        }
    }
}

void TypeInferer::visit(ast::FieldExp& exp)
{
    auto structType = TypeInferer::infer(*exp.lhs, this);
    if (structType == nullptr)
    {
        Log(exp.location, "failed to infer type of left hand side of field access");
    }
    else if (!structType->isStructType())
    {
        Log(exp.location, "left hand side of field access is not a struct type");
    }
    else
    {
        auto index = structType->getFieldIndex(exp.fieldIdentifier);
        if (index == Type::InvalidIndex)
        {
            Log(exp.location, "unknown field: ", exp.fieldIdentifier);
        }
        else
        {
            result = structType->getFieldType(index);
        }
    }
}


ConstructorTypeScope::ConstructorTypeScope(TypeScope* parent_, const ast::DataDeclaration* ast) :
    parent{ parent_ }
{
    for (auto& typeVar : ast->typeVariables)
    {
        typeMap.insert({ typeVar, getTypeContext()->getAnyType() });
    }
}

ConstructorTypeScope::~ConstructorTypeScope()
{}

TypePtr ConstructorTypeScope::getTypeByName(const ast::TypeIdentifier& typeName, const ImportedModule *importModule)
{
    auto it = typeName.identifier.moduleName.empty() ?
        typeMap.find(typeName.identifier.name) : typeMap.end();
    if (it == typeMap.end())
    {
        if (typeName.parameters.empty())
        {
            return getTypeContext()->getTypeFromAst(typeName, importModule);
        }

        // if we have parameters those might be type variables and we have to recursively call this->getTypeByName

        bool concrete = true;
        std::vector<TypePtr> parameterTypes;
        for (auto& param : typeName.parameters)
        {
            auto type = getTypeByName(param, importModule);
            if (type == nullptr)
            {
                return nullptr;
            }
            concrete = concrete && type->isConcreteType();
            parameterTypes.push_back(type);
        }

        const ImportedModule* astModule;
        const ast::DataDeclaration* ast;
        std::tie(astModule, ast) = importModule->lookupType(typeName.identifier);
        if (astModule == nullptr || ast == nullptr)
        {
            Log({}, "unknown type: ", typeName.identifier.str());
            return nullptr;
        }

        Identifier newId;
        newId.name = { astModule->name(), typeName.identifier.name };
        for (auto& paramType : parameterTypes)
        {
            newId.parameters.push_back(paramType->identifier());
        }

        if (concrete)
        {
            return getTypeContext()->getType(newId);
        }
        else
        {
            // construct new type
            std::vector<TypePtr> fieldTypes;
            for (auto& field : ast->fields)
            {
                fieldTypes.push_back(getTypeByName(field.type, importModule));
                if (fieldTypes.back() == nullptr)
                {
                    Log({}, "unknown type: ", field.type.str());
                    return nullptr;
                }
            }
            return std::make_shared<StructType>(std::move(newId), ast, fieldTypes);
        }
    }
    else
    {
        // found a typeVar, it is not allowed to have parameters
        if (typeName.parameters.empty())
        {
            return it->second;
        }
        else
        {
            Log({}, "type variable can't have parameters");
            return nullptr;
        }
    }
}

bool ConstructorTypeScope::updateType(const ast::TypeIdentifier& typeId, const TypePtr& type)
{
    // recurse down the typeId...
    auto it = typeId.identifier.moduleName.empty() ?
        typeMap.find(typeId.identifier.name) : typeMap.end();
    if (it == typeMap.end())
    {
        assert(type->getFieldCount() == typeId.parameters.size());

        bool result = true;
        unsigned int i = 0;
        for (auto& param : typeId.parameters)
        {
            result = result && updateType(param, type->getFieldType(i)); // check null? no, we have asserted getFieldCount
            ++i;
        }
        return result;
    }
    else
    {
        auto newType = getTypeContext()->unify(it->second, type);
        it->second = newType;
        return newType != nullptr;
    }
}

TypeContext* ConstructorTypeScope::getTypeContext()
{
    return parent->getTypeContext();
}

TypePtr ConstructorTypeScope::getVariableType(const std::string& variable)
{
    return parent->getVariableType(variable);
}

FunAst ConstructorTypeScope::getFunctionAST(const GlobalIdentifier& identifier)
{
    return parent->getFunctionAST(identifier);
}

DataAst ConstructorTypeScope::getDataAST(const GlobalIdentifier& identifier)
{
    return parent->getDataAST(identifier);
}

TypePtr ConstructorTypeScope::getTypeVariable(const std::string& typeVariable) const
{
    auto it = typeMap.find(typeVariable);
    return it == typeMap.end() ? nullptr : it->second;
}

void TypeInferer::visit(ast::ConstructorExp& exp)
{
    const ImportedModule* astModule;
    const ast::DataDeclaration* ast;
    std::tie(astModule, ast) = getDataAST(exp.identifier);
    if (astModule == nullptr || ast == nullptr)
    {
        return;
    }

    ConstructorTypeScope typeScope(this, ast);

    int i = 0;
    for (auto& field : ast->fields)
    {
        // TODO: can do a check if field.type contains typeVars, else continue

        TypePtr fieldType = typeScope.getTypeByName(field.type, astModule); // replace all type vars
        if (fieldType == nullptr)
        {
            return;
        }
        if (!fieldType->isConcreteType())
        {
            auto &argAst = exp.arguments[i];
            TypePtr argumentType = TypeInferer::infer(*argAst->exp, this);
            auto resultingType = getTypeContext()->unify(fieldType, argumentType);

            if (resultingType == nullptr)
            {
                Log(argAst->location, "failed to unify types, '", fieldType->identifier().str(), "' with '", argumentType->identifier().str(), '\'');
                return;
            }

            if (!typeScope.updateType(field.type, resultingType))
            {
                // How could this fail? It should always be possible to just write the type back to the type scope...
                Log(argAst->location, "");
                return;
            }
        }

        i++;
    }

    bool concreteType = std::all_of(ast->typeVariables.begin(), ast->typeVariables.end(),
        [&typeScope](const std::string &typeVar)
        {
            auto type = typeScope.getTypeVariable(typeVar);
            assert(type != nullptr);
            return type->isConcreteType();
        });

    std::vector<Identifier> typeArgs;
    for (auto& typeVar : ast->typeVariables)
    {
        auto type = typeScope.getTypeVariable(typeVar);
        typeArgs.push_back(type->identifier());
    }
    Identifier typeId{ { astModule->name(), ast->name }, std::move(typeArgs) };

    if (concreteType)
    {
        result = getTypeContext()->getType(typeId);
    }
    else // construct abstract type
    {
        std::vector<TypePtr> fieldTypes;
        for (auto& field : ast->fields)
        {
            fieldTypes.push_back(typeScope.getTypeByName(field.type, astModule));
            assert(fieldTypes.back() != nullptr);
        }
        result = std::make_shared<StructType>(std::move(typeId), ast, std::move(fieldTypes));
    }
}

TypeContext* TypeInferer::getTypeContext()
{
    return env->getTypeContext();
}

TypePtr TypeInferer::getVariableType(const std::string& name)
{
    auto it = variables.find(name);
    if (it != variables.end())
    {
        return it->second;
    }
    return env->getVariableType(name);
}

FunAst TypeInferer::getFunctionAST(const GlobalIdentifier& identifier)
{
    return env->getFunctionAST(identifier);
}

DataAst TypeInferer::getDataAST(const GlobalIdentifier& identifier)
{
    return env->getDataAST(identifier);
}

} // namespace Type
} // namespace llfp
