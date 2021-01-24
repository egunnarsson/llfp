
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

Type::Type(GlobalIdentifier identifier, llvm::Type* llvmType, bool isSigned) :
    identifier_{ std::move(identifier) },
    llvmType_{ llvmType },
    isSigned_{ isSigned }
{
}

Type::Type(GlobalIdentifier identifier, bool isFloating, bool isSigned) :
    identifier_{ std::move(identifier) },
    llvmType_{ nullptr },
    isSigned_{ isSigned }
    // isFloating for now we check literal type name instead
{
}

Type::~Type() {}

const GlobalIdentifier& Type::identifier() const
{
    return identifier_;
}

llvm::Type *Type::llvmType() const
{
    return llvmType_;
}

bool Type::isBool() const
{
    return identifier_ == name::Bool;
}

bool Type::isNum() const
{
    return isInteger() || isFloating();
}

bool Type::isInteger() const
{
    if (llvmType_ != nullptr)
    {
        return llvmType_->isIntegerTy() && identifier_ != name::Bool;
    }
    return identifier_ == name::IntegerLiteral || identifier_ == name::SignedIntegerLiteral;
}

bool Type::isFloating() const
{
    if (llvmType_ != nullptr)
    {
        return llvmType_->isFloatingPointTy();
    }
    return identifier_ == name::FloatingLiteral;
}

bool Type::isSigned() const
{
    // the llvm type does not tell us if we are signed or not
    return isSigned_;
}

bool Type::isLiteral() const
{
    return !identifier_.name.empty() && identifier_.name.front() == '@';
}

bool Type::isStructType() const
{
    return false;
}

unsigned int Type::getFieldIndex(const std::string&) const
{
    return InvalidFieldIndex;
}

unsigned int Type::getFieldCount() const
{
    return 0;
}

Type* Type::getFieldType(unsigned int) const
{
    assert(false);
    return nullptr;
}

Type* Type::unify(Type* other, TypeContext* env)
{
    if (this == other)
    {
        return this;
    }
    assert(this->identifier_ != other->identifier_);
    if (other->identifier_ == name::Any)
    {
        return this;
    }
    if (this->identifier_ == name::Any)
    {
        return other;
    }

    if (isLiteral() && other->isLiteral())
    {
        // TODO: unify to smallest bitsize
        if (isFloating() && other->isFloating())
        {
            return env->getType(name::Double);
        }
        if (isSigned() || other->isSigned())
        {
            return env->getType(name::I64);
        }
        else
        {
            return env->getType(name::U64);
        }
    }
    else if (isLiteral() || other->isLiteral())
    {
        Type *nonLiteralType = this;
        Type *literalType = other;
        if (isLiteral())
        {
            std::swap(nonLiteralType, literalType);
        }

        if (isFloating() && other->isFloating())
        {
            return nonLiteralType;
        }

        if (literalType->isSigned())
        {
            if (!nonLiteralType->isSigned())
            {
                Log({}, "failed to unify types: ", identifier_.str(), " with ", other->identifier_.str());
                return nullptr;
            }
        }
        return nonLiteralType;
    }

    // neither is literal and they are not the same
    Log({}, "failed to unify types: ", identifier_.str(), " with ", other->identifier_.str());
    return nullptr;
}


StructType::StructType(GlobalIdentifier identifier, llvm::StructType* llvmType) :
    Type(std::move(identifier), llvmType),
    llvmType_{ llvmType }
{
}

bool StructType::isStructType() const
{
    return true;
}

unsigned int StructType::getFieldIndex(const std::string &fieldIdentifier) const
{
    const unsigned int size = static_cast<unsigned int>(fields.size());
    for (unsigned int i = 0; i < size; ++i)
    {
        if (fields[i].name == fieldIdentifier) { return i; }
    }
    return InvalidFieldIndex;
}

Type* StructType::getFieldType(unsigned int index) const
{
    assert(index < fields.size());
    return fields[index].type;
}

unsigned int StructType::getFieldCount() const
{
    return static_cast<unsigned int>(fields.size());
}

bool StructType::setFields(const std::vector<ast::Field> &astFields, std::vector<type::Type*> &fieldTypes)
{
    assert(fields.size() == 0);
    assert(astFields.size() == fieldTypes.size());

    std::vector<llvm::Type*> llvmTypes;
    for (unsigned int i = 0; i < astFields.size(); ++i)
    {
        if (getFieldIndex(astFields[i].name) != InvalidFieldIndex)
        {
            Log(astFields[i].location, "duplicate field \"", astFields[i].name, '"');
            return false;
        }

        fields.push_back({astFields[i].name , fieldTypes[i] });
        llvmTypes.push_back(fieldTypes[i]->llvmType());
    }

    llvmType_->setBody(llvmTypes);
    return true;
}


#define ADD_TYPE(id, get, sign) types.insert({ id, std::make_unique<Type>(GlobalIdentifier{id.moduleName.str(), id.name.str()}, llvm::Type::get(llvmContext), sign) })
#define ADD_TYPE_L(id, floating, sign) types.insert({ id, std::make_unique<Type>(GlobalIdentifier{id.moduleName.str(), id.name.str()}, floating, sign) })

TypeContext::TypeContext(llvm::LLVMContext &llvmContext_, SourceModule *sourceModule_):
    llvmContext{ llvmContext_ },
    sourceModule{ sourceModule_ }
{
    ADD_TYPE(name::Bool, getInt1Ty, false);

    ADD_TYPE(name::I8, getInt8Ty, true);
    ADD_TYPE(name::I16, getInt16Ty, true);
    ADD_TYPE(name::I32, getInt32Ty, true);
    ADD_TYPE(name::I64, getInt64Ty, true);
    ADD_TYPE(name::I128, getInt128Ty, true);

    ADD_TYPE(name::U8, getInt8Ty, false);
    ADD_TYPE(name::U16, getInt16Ty, false);
    ADD_TYPE(name::U32, getInt32Ty, false);
    ADD_TYPE(name::U64, getInt64Ty, false);
    ADD_TYPE(name::U128, getInt128Ty, false);

    ADD_TYPE(name::Half, getHalfTy, true);
    ADD_TYPE(name::Float, getFloatTy, true);
    ADD_TYPE(name::Double, getDoubleTy, true);

    ADD_TYPE(name::Char, getInt8Ty, false);

    ADD_TYPE_L(name::Any, false, false);
    ADD_TYPE_L(name::IntegerLiteral, false, false);
    ADD_TYPE_L(name::SignedIntegerLiteral, false, true);
    ADD_TYPE_L(name::FloatingLiteral, true, true);
}

StructType* TypeContext::addType(std::unique_ptr<StructType> type)
{
    auto ptr = type.get();
    if (types.insert({ type->identifier(), std::move(type) }).second)
    {
        return ptr;
    }
    return nullptr;
}

Type* TypeContext::getType(GlobalIdentifierRef identifier)
{
    return getType(identifier, sourceModule);
}

Type* TypeContext::getType(GlobalIdentifierRef identifier, const ImportedModule* lookupModule)
{
    // built in types
    {
        auto it = types.find(identifier);
        if (it != types.end())
        {
            return it->second.get();
        }
    }
    const ImportedModule *astModule;
    const llfp::ast::DataDeclaration *ast;
    if (lookupModule->lookupType(identifier, astModule, ast))
    {
        if (identifier.moduleName.empty())
        {
            identifier.moduleName = astModule->name();
        }

        auto it = types.find(identifier);
        if (it != types.end())
        {
            return it->second.get();
        }

        GlobalIdentifier newIdentifier{ astModule->name(), ast->name };
        auto llvmType = llvm::StructType::create(llvmContext, astModule->getMangledName(ast));
        auto typePtr = std::make_unique<llfp::type::StructType>(std::move(newIdentifier), llvmType);
        auto type = typePtr.get();

        auto it2 = types.insert({ typePtr->identifier(), std::move(typePtr) });

        if (!it2.second)
        {
            // shouldn't happen, because we do a find before
            Log({}, "duplicate type");
            return nullptr;
        }

        // generate body

        std::vector<type::Type*> fieldTypes;
        for (auto &field : ast->fields)
        {
            fieldTypes.push_back(getType(field.type, astModule));
            if (fieldTypes.back() == nullptr)
            {
                Log(field.location, "unknown type: ", field.type.moduleName, ':', field.type.name);
                return nullptr;
            }
        }

        if (!type->setFields(ast->fields, fieldTypes))
        {
            return nullptr;
        }

        return type;
    }

    return nullptr;
}

bool TypeContext::equals(Type *type, GlobalIdentifierRef id)
{
    if (type == nullptr)
    {
        return false;
    }
    return type == getType(id);
}

EmptyTypeScope::EmptyTypeScope(TypeScope *parent_) :
    parent{ parent_ }
{
}

TypeContext* EmptyTypeScope::getTypeContext()
{
    return parent->getTypeContext();
}

Type* EmptyTypeScope::getVariableType(const std::string&)
{
    return nullptr;
}

const ast::FunctionDeclaration* EmptyTypeScope::getFunctionAST(GlobalIdentifierRef identifier)
{
    return parent->getFunctionAST(identifier);
}


TypeInferer::TypeInferer(TypeScope *scope_) :
    env{ scope_ }
{
    assert(env != nullptr);
}

Type* TypeInferer::infer(ast::Exp &exp, TypeScope *scope_)
{
    TypeInferer inferer(scope_);
    exp.accept(&inferer);
    return inferer.result;
}

void TypeInferer::visit(ast::LetExp &exp)
{
    for (auto &var : exp.letStatments)
    {
        if (var->parameters.size() != 0)
        {
            Log(exp.location, "let function definitions not implemented");
            return;
        }

        type::Type* varType;
        if (var->type.name.empty())
        {
            varType = TypeInferer::infer(*var->functionBody, this);
        }
        else
        {
            varType = env->getTypeByName(var->type);
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

void TypeInferer::visit(ast::IfExp &exp)
{
    result = TypeInferer::infer(*exp.elseExp, this);
}

void TypeInferer::visit(ast::CaseExp &exp)
{
    Log(exp.location, "case not implemented");
}

namespace
{

Type* inferMathExp(TypeInferer *inferer, ast::BinaryExp &exp)
{
    auto lhs = TypeInferer::infer(*exp.lhs, inferer);
    auto rhs = TypeInferer::infer(*exp.rhs, inferer);
    auto result = lhs->unify(rhs, inferer->getTypeContext());
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
        Log(exp.location, "failed to unify types: ", lhs->identifier().str(), " and ", rhs->identifier().str());
    }
    return nullptr;
}

Type* inferBitExp(TypeInferer *inferer, ast::BinaryExp &exp)
{
    auto lhs = TypeInferer::infer(*exp.lhs, inferer);
    auto rhs = TypeInferer::infer(*exp.rhs, inferer);
    auto result = lhs->unify(rhs, inferer->getTypeContext());
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

void TypeInferer::visit(ast::BinaryExp &exp)
{
    // math
    if (exp.op == "*")      { result = inferMathExp(this, exp); } // Multiplication
    else if (exp.op == "/") { result = inferMathExp(this, exp); } // Division
    else if (exp.op == "%") { result = inferMathExp(this, exp); } // Remainder
    else if (exp.op == "+") { result = inferMathExp(this, exp); } // Addition
    else if (exp.op == "-") { result = inferMathExp(this, exp); } // Subtraction
    // bitwise
    else if (exp.op == "<<")  { result = inferBitExp(this, exp); } // Shift
    else if (exp.op == ">>")  { result = inferBitExp(this, exp); } // Signed shift
    else if (exp.op == ">>>") { result = inferBitExp(this, exp); } // Logical shift
    // compare
    else if (exp.op == ">")  { result = getTypeByName(name::Bool); } // Greater than
    else if (exp.op == ">=") { result = getTypeByName(name::Bool); } // Greater or equal
    else if (exp.op == "<")  { result = getTypeByName(name::Bool); } // Less than
    else if (exp.op == "<=") { result = getTypeByName(name::Bool); } // Less or equal
    else if (exp.op == "==") { result = getTypeByName(name::Bool); } // Equal
    else if (exp.op == "!=") { result = getTypeByName(name::Bool); } // Not equal
    // bitwise
    else if (exp.op == "&") { result = inferBitExp(this, exp); }
    else if (exp.op == "|") { result = inferBitExp(this, exp); }
    else if (exp.op == "^") { result = inferBitExp(this, exp); }
    // logical
    else if (exp.op == "&&") { result = getTypeByName(name::Bool); }
    else if (exp.op == "||") { result = getTypeByName(name::Bool); }
    else
    {
        Log(exp.location, "unknown operator: ", exp.op);
    }
}

void TypeInferer::visit(ast::UnaryExp &exp)
{
    assert(exp.op == "-");
    result = TypeInferer::infer(*exp.operand, this);
    if (getTypeContext()->equals(result, name::IntegerLiteral))
    {
        result = getTypeByName(name::SignedIntegerLiteral);
    }
}

void TypeInferer::visit(ast::LiteralExp &exp)
{
    switch (exp.tokenType)
    {
    case lex::tok_integer:

        if (exp.value.front() == '-')
        {
            result = getTypeByName(name::SignedIntegerLiteral);
        }
        else
        {
            result = getTypeByName(name::IntegerLiteral);
        }
        // or signed
        break;

    case lex::tok_float:

        result = getTypeByName(name::FloatingLiteral);
        break;

    case lex::tok_char:

        result = getTypeByName(name::Char);
        // add type classes?
        break;

    case lex::tok_string:

        Log(exp.location, "string not supported");
        break;

    case lex::tok_bool:

        result = getTypeByName(name::Bool);
        break;

    default:

        assert(false);
        break;
    }
}

void TypeInferer::visit(ast::CallExp &exp)
{
    auto ast = env->getFunctionAST(exp.identifier);
    if (ast != nullptr)
    {
        if (ast->type.name.empty())
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
                inferer.variables[ast->parameters[i]->identifier] = TypeInferer::infer(*exp.arguments[i].get(), this);
            }

            //FIXME: standard library does not have bodies?
            ast->functionBody->accept(&inferer);
            result = inferer.result;
        }
        else
        {
            result = env->getTypeByName(ast->type);
        }
    }
}

void TypeInferer::visit(ast::VariableExp &exp)
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
    auto ast = env->getFunctionAST(exp.identifier);
    if (ast != nullptr)
    {
        if (ast->type.name.empty())
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
            result = env->getTypeByName(ast->type);
        }
    }
}

void TypeInferer::visit(ast::FieldExp &exp)
{
    auto structType = TypeInferer::infer(*exp.lhs, this);
    if (!structType->isStructType())
    {
        Log(exp.location, "");
    }
    else
    {
        auto index = structType->getFieldIndex(exp.fieldIdentifier);
        if (index == Type::InvalidFieldIndex)
        {
            Log(exp.location, "unknown field: ", exp.fieldIdentifier);
        }
        else
        {
            result = structType->getFieldType(index);
        }
    }
}

void TypeInferer::visit(ast::ConstructorExp &exp)
{
    result = getTypeContext()->getType(exp.identifier);
}

TypeContext* TypeInferer::getTypeContext()
{
    return env->getTypeContext();
}

Type* TypeInferer::getVariableType(const std::string& name)
{
    auto it = variables.find(name);
    if (it != variables.end())
    {
        return it->second;
    }
    return env->getVariableType(name);
}

const ast::FunctionDeclaration* TypeInferer::getFunctionAST(GlobalIdentifierRef identifier)
{
    return env->getFunctionAST(identifier);
}

} // namespace Type
} // namespace llfp
