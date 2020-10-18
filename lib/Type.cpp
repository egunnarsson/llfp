
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

Type::~Type() {}

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


bool Type::isStructType() const
{
    return false;
}

Type* Type::getFieldType(const std::string &fieldIdentifier) const
{
    return nullptr;
}

unsigned int Type::getFieldIndex(const std::string &fieldIdentifier) const
{
    return (unsigned int)-1;
}

Type* Type::unify(Type* other, TypeContext* env)
{
    if (this == other)
    {
        return this;
    }
    assert(this->name_ != other->name_);
    if (other->name_ == name::Any)
    {
        return this;
    }
    if (this->name_ == name::Any)
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
                Log({}, "failed to unify types: ", name_, " with ", other->name_);
                return nullptr;
            }
        }
        return nonLiteralType;
    }

    // neither is literal and they are not the same
    Log({}, "failed to unify types: ", name_, " with ", other->name_);
    return nullptr;
}

bool Type::operator ==(const std::string &s) const { return name_ == s; }
bool Type::operator !=(const std::string &s) const { return name_ != s; }


StructType::StructType(std::string name, llvm::StructType* llvmType) :
    Type(std::move(name), llvmType),
    llvmType_{ llvmType }
{
}

bool StructType::isStructType() const
{
    return true;
}

Type* StructType::getFieldType(const std::string &fieldIdentifier) const
{
    auto it = fields.find(fieldIdentifier);
    if (it != fields.end())
    {
        return it->second.type;
    }
    return nullptr;
}

unsigned int StructType::getFieldIndex(const std::string &fieldIdentifier) const
{
    auto it = fields.find(fieldIdentifier);
    if (it != fields.end())
    {
        return it->second.index;
    }
    return (unsigned int)-1;
}

bool StructType::setFields(std::vector<ast::Field> &astFields, std::vector<type::Type*> &fieldTypes)
{
    assert(fields.size() == 0);
    assert(astFields.size() == fieldTypes.size()); // or if?

    std::vector<llvm::Type*> llvmTypes;
    for (unsigned int i = 0; i < astFields.size(); ++i)
    {
        Field f{ i, fieldTypes[i] };
        auto inserted = fields.insert(std::make_pair(astFields[i].identifier, f)).second;
        if (!inserted)
        {
            Log(astFields[i].location, "duplicate field: ", astFields[i].identifier);
            return false;
        }
        llvmTypes.push_back(fieldTypes[i]->llvmType());
    }

    llvmType_->setBody(llvmTypes);
    return true;
}


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

    ADD_TYPE_L(name::Any, false, false);
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

StructType* TypeContext::addType(std::unique_ptr<StructType> type)
{
    auto ptr = type.get();
    if (types.insert({ type->name(), std::move(type) }).second)
    {
        return ptr;
    }
    return nullptr;
}

EmptyTypeScope::EmptyTypeScope(TypeScope *parent_) :
    parent{ parent_ }
{
}

TypeContext* EmptyTypeScope::getTypeContext()
{
    return parent->getTypeContext();
}

Type* EmptyTypeScope::getVariableType(llvm::StringRef variable)
{
    return parent->getTypeContext()->getType(variable);
}

const ast::FunctionDeclaration* EmptyTypeScope::getFunctionAST(llvm::StringRef module, llvm::StringRef functionName)
{
    return parent->getFunctionAST(module, functionName);
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
        Log(exp.location, "failed to unify types: ", lhs->name(), " and ", rhs->name());
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
    auto ast = env->getFunctionAST(exp.moduleName, exp.name);
    if (ast != nullptr)
    {
        if (ast->typeName.empty())
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
            result = env->getTypeByName(ast->typeName);
        }
    }
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

    // not a local variable
    auto ast = env->getFunctionAST(exp.moduleName, exp.name);
    if (ast != nullptr)
    {
        if (ast->typeName.empty())
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
            result = env->getTypeByName(ast->typeName);
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
        result = structType->getFieldType(exp.fieldIdentifier);
        if (result == nullptr)
        {
            Log(exp.location, "unknown field: ", exp.fieldIdentifier);
        }
    }
}

TypeContext* TypeInferer::getTypeContext()
{
    return env->getTypeContext();
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

const ast::FunctionDeclaration* TypeInferer::getFunctionAST(llvm::StringRef module, llvm::StringRef functionName)
{
    return env->getFunctionAST(module, functionName);
}

} // namespace Type
} // namespace llfp
