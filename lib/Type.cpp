
#pragma warning(push, 0)

#include "llvm/Support/FormatVariadic.h"

#pragma warning(pop)

#include "Log.h"

#include "Type.h"


namespace llfp
{
namespace type
{

Type::Type(std::string name, llvm::Type* llvmType, bool isSigned) :
    name_{ std::move(name) },
    llvmType_{ llvmType },
    isSigned_{ isSigned }
{
}

Type::Type(std::string name, bool isFloating, bool isSigned) :
    name_{ std::move(name) },
    llvmType_{ nullptr },
    isSigned_{ isSigned }
    // isFloating for now we check literal type name instead
{
}

const std::string& Type::name() const
{
    return name_;
}

llvm::Type *Type::llvmType() const
{
    return llvmType_;
}

bool Type::isNum() const
{
    return isInteger() || isFloating();
}

bool Type::isInteger() const
{
    if (llvmType_ != nullptr)
    {
        return llvmType_->isIntegerTy() && name_ != name::Bool;
    }
    return name_ == name::IntegerLiteral || name_ == name::SignedIntegerLiteral;
}

bool Type::isFloating() const
{
    if (llvmType_ != nullptr)
    {
        return llvmType_->isFloatingPointTy();
    }
    return name_ == name::FloatingLiteral;
}

bool Type::isSigned() const
{
    // the llvm type does not tell us if we are signed or not
    return isSigned_;
}

bool Type::isLiteral() const
{
    return !name_.empty() && name_.front() == '@';
}

Type* Type::unify(Type* other, TypeEnvironment* env)
{
    if (this == other)
    {
        return this;
    }

    Type *floatType, *intType, *uintType;
    if (isLiteral() && other->isLiteral())
    {
        // TODO: unify to smallest bitsize
        floatType = env->getTypeByName(name::Double);
        intType = env->getTypeByName(name::I64);
        uintType = env->getTypeByName(name::U64);
    }
    else
    {
        if (isLiteral())
        {
            floatType = other;
            intType = other;
            uintType = other;
        }
        else
        {
            floatType = this;
            intType = this;
            uintType = this;
        }
    }

    if (isFloating() && other->isFloating())
    {
        return floatType;
    }
    if (isInteger() && other->isInteger())
    {
        if (isSigned())
        {
            if (other->isSigned())
            {
                return intType;
            }
        }
        else
        {
            if (!other->isSigned())
            {
                return uintType;
            }
        }
    }

    return nullptr;
}

bool Type::operator ==(const std::string &s) const { return name_ == s; }
bool Type::operator !=(const std::string &s) const { return name_ != s; }

#define ADD_TYPE(name, get, sign) types.insert({ name, std::make_unique<Type>(name, llvm::Type::get(llvmContext), sign) })
#define ADD_TYPE_L(name, floating, sign) types.insert({ name, std::make_unique<Type>(name, floating, sign) })

TypeContext::TypeContext(llvm::LLVMContext &llvmContext)
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

    ADD_TYPE_L(name::IntegerLiteral, false, false);
    ADD_TYPE_L(name::SignedIntegerLiteral, false, true);
    ADD_TYPE_L(name::FloatingLiteral, true, true);
}

Type* TypeContext::getType(llvm::StringRef name)
{
    auto it = types.find(name);
    if (it == types.end())
    {
        llvm::errs() << "unknown type: " << name;
        return nullptr;
    }
    return it->second.get();
}


TypeInferer::TypeInferer(TypeEnvironment *env_) :
    env{ env_ }
{
    assert(env != nullptr);
}

Type* TypeInferer::infer(ast::Exp &exp, TypeEnvironment *env)
{
    TypeInferer inferer(env);
    exp.accept(&inferer);
    return inferer.result;
}

/*Type* TypeInferer::infer(ast::Exp &exp, TypeInferer *parent)
{
    TypeInferer inferer(parent);
    exp.accept(&inferer);
    return inferer.result;
}*/

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
        if (var->typeName.empty())
        {
            varType = TypeInferer::infer(*var->functionBody, this);
        }
        else
        {
            varType = env->getTypeByName(var->typeName);
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
    auto result = lhs->unify(rhs, inferer);
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
    return nullptr;
}

Type* inferBitExp(TypeInferer *inferer, ast::BinaryExp &exp)
{
    auto lhs = TypeInferer::infer(*exp.lhs, inferer);
    auto rhs = TypeInferer::infer(*exp.rhs, inferer);
    auto result = lhs->unify(rhs, inferer);
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
    if (result != nullptr && *result == name::IntegerLiteral)
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
    // if types not specified

    // infer types of argument expresions and assign to argument names in a new env

    // infer exp of body in the new environment

    result = env->getFunctionReturnType(exp, exp.moduleName, exp.name);
}

void TypeInferer::visit(ast::VariableExp &exp)
{
    if (exp.moduleName.empty())
    {
        result = getVariableType(exp.name);
        if (result != nullptr)
        {
            return;
        }
    }

    // possibly 0 arity function
    result = getFunctionReturnType(exp, exp.moduleName, exp.name);
}

Type* TypeInferer::getTypeByName(llvm::StringRef name)
{
    return env->getTypeByName(name);
}

Type* TypeInferer::getVariableType(llvm::StringRef name)
{
    auto it = variables.find(name);
    if (it != variables.end())
    {
        return it->second;
    }
    return env->getVariableType(name);
}

Type* TypeInferer::getFunctionReturnType(ast::Exp &exp, llvm::StringRef module, llvm::StringRef functionName)
{
    return env->getFunctionReturnType(exp, module, functionName);
}

} // namespace Type
} // namespace llfp
