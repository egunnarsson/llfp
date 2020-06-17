
#pragma warning(push, 0)

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/Support/FormatVariadic.h"

#pragma warning(pop)

#include "Codegen.h"


namespace llfp
{
namespace codegen
{

const Value ExpCodeGenerator::EmptyValue{ nullptr, nullptr };

CodeGenerator::CodeGenerator() :
    llvmBuilder(llvmContext),
    typeContext(llvmContext)
{
    // fill functions with default functions
}

std::unique_ptr<llvm::Module> CodeGenerator::generate(const std::unique_ptr<ast::Module> &module)
{
    llvmModule = llvm::make_unique<llvm::Module>(module->identifier, llvmContext);

    for (auto& f : module->functionDeclarations)
    {
        // make prototypes so that we can call them before generating code
        // check no duplicated
        if (functions.find(f->identifier) == functions.end())
        {
            std::vector<llvm::Type*> parameterTypes;
            for (auto& p : f->parameters)
            {
                auto type = typeContext.getType(p->typeName);
                if (type == nullptr)
                {
                    llvm::errs() << "unknown type: " << p->typeName;
                    return nullptr;
                }
                parameterTypes.push_back(type->llvmType());
            }
            auto returnType = typeContext.getType(f->typeName)->llvmType();
            if (returnType == nullptr)
            {
                llvm::errs() << "unknown type: " << f->typeName;
                return nullptr;
            }
            llvm::FunctionType *functionType = llvm::FunctionType::get(returnType, parameterTypes, false);

            auto llvmFunction = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, f->identifier, llvmModule.get());
            functions[f->identifier] = { f.get(), llvmFunction };

            size_t i = 0;
            for (auto &p : llvmFunction->args())
            {
                p.setName(f->parameters[i++]->identifier);
            }
        }
        else
        {
            llvm::errs() << "duplicate function definition: " << f->identifier;
            return nullptr;
        }
    }

    for (auto f_ : functions)
    {
        // actually generate code
        auto f = f_.second;

        auto bb = llvm::BasicBlock::Create(llvmContext, "entry", f.llvm);
        llvmBuilder.SetInsertPoint(bb);

        std::map<std::string, Value> namedValues;
        for (size_t i = 0; i < f.ast->parameters.size(); ++i)
        {
            auto& llvmArg = *(f.llvm->arg_begin() + i);
            if (namedValues.find(llvmArg.getName()) != namedValues.end())
            {
                llvm::errs() << "duplicate parameter: " << llvmArg.getName();
                return nullptr;
            }
            auto &typeName = f.ast->parameters[i]->typeName;
            auto type = typeContext.getType(typeName);
            assert(type != nullptr); // already checked in module->functionDeclarations loop
            namedValues[llvmArg.getName()] = { type, &llvmArg };
        }

        ExpCodeGenerator expGenerator(typeContext.getType(f.ast->typeName), this, std::move(namedValues));
        f.ast->functionBody->accept(&expGenerator);
        llvmBuilder.CreateRet(expGenerator.getResult());
    }

    // TODO: Should not be an ifdef for cross compile support
#ifdef _WIN32
    AddDllMain();
#endif

    return std::move(llvmModule);
}

void CodeGenerator::AddDllMain()
{
    std::vector<llvm::Type*> parameterTypes;
    parameterTypes.push_back(llvm::Type::getInt8PtrTy(llvmContext));
    parameterTypes.push_back(llvm::Type::getInt32Ty(llvmContext));
    parameterTypes.push_back(llvm::Type::getInt8PtrTy(llvmContext));
    auto returnType = llvm::Type::getInt32Ty(llvmContext);
    llvm::FunctionType *functionType = llvm::FunctionType::get(returnType, parameterTypes, false);
    auto llvmFunction = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, "_DllMainCRTStartup", llvmModule.get());
    llvmFunction->setCallingConv(llvm::CallingConv::X86_StdCall);
    auto bb = llvm::BasicBlock::Create(llvmContext, "entry", llvmFunction);
    llvmBuilder.SetInsertPoint(bb);
    auto value = llvm::ConstantInt::get(returnType, 1);
    llvmBuilder.CreateRet(value);
}

ExpCodeGenerator::ExpCodeGenerator(type::Type *type_, CodeGenerator *generator_, std::map<std::string, Value> parameters_) :
    parent{ nullptr },
    generator{ generator_ },
    expectedType{ type_ },
    values{ std::move(parameters_) },
    result{ nullptr }
{
    assert(expectedType->llvmType() != nullptr);
}

ExpCodeGenerator::ExpCodeGenerator(type::Type *type_, ExpCodeGenerator *parent_) :
    parent{ parent_ },
    generator{ parent_->generator },
    expectedType{ type_ },
    result{ nullptr }
{
    assert(expectedType->llvmType() != nullptr);
}

llvm::Value* ExpCodeGenerator::generate(ast::Exp &exp, type::Type *type, ExpCodeGenerator *parent)
{
    if (type == nullptr) // or not?
    {
        return nullptr;
    }
    ExpCodeGenerator generator(type, parent);
    exp.accept(&generator);
    return generator.result;
}

void ExpCodeGenerator::visit(ast::LetExp &exp)
{
    for (auto &var : exp.letStatments)
    {
        if (var->parameters.size() != 0)
        {
            llvm::errs() << "let function definitions not implemented";
            return;
        }

        type::Type* varType;
        if (var->typeName.empty())
        {
            varType = type::TypeInferer::infer(*var->functionBody, this);
        }
        else
        {
            varType = typeContext().getType(var->typeName);
        }
        if (varType == nullptr)
        {
            return;
        }

        auto value = ExpCodeGenerator::generate(*var->functionBody, varType, this);

        if (value == nullptr)
        {
            return;
        }
        if (values.find(var->identifier) != values.end())
        {
            llvm::errs() << "already defined";
            return;
        }
        else
        {
            values[var->identifier] = { varType, value };
        }
    }

    result = ExpCodeGenerator::generate(*exp.exp, expectedType, this);
}

void ExpCodeGenerator::visit(ast::IfExp &exp)
{
    auto condV = ExpCodeGenerator::generate(*exp.condition, typeContext().getType(type::name::Bool), this);
    if (condV == nullptr)
    {
        return;
    }

    auto& builder = llvmBuilder();
    auto function = builder.GetInsertBlock()->getParent();

    auto thenBB = llvm::BasicBlock::Create(llvmContext(), "then", function);
    auto elseBB = llvm::BasicBlock::Create(llvmContext(), "else");
    auto mergeBB = llvm::BasicBlock::Create(llvmContext(), "ifcont");

    builder.CreateCondBr(condV, thenBB, elseBB);

    builder.SetInsertPoint(thenBB);

    auto thenV = ExpCodeGenerator::generate(*exp.thenExp, expectedType, this);
    if (thenV == nullptr)
    {
        return;
    }
    builder.CreateBr(mergeBB);
    // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
    thenBB = builder.GetInsertBlock();

    function->getBasicBlockList().push_back(elseBB);
    builder.SetInsertPoint(elseBB);

    auto elseV = ExpCodeGenerator::generate(*exp.elseExp, expectedType, this);
    if (elseV == nullptr)
    {
        return;
    }
    builder.CreateBr(mergeBB);
    elseBB = builder.GetInsertBlock();

    // Emit merge block.
    function->getBasicBlockList().push_back(mergeBB);
    builder.SetInsertPoint(mergeBB);
    auto PN = builder.CreatePHI(expectedType->llvmType(), 2, "iftmp");

    PN->addIncoming(thenV, thenBB);
    PN->addIncoming(elseV, elseBB);
    result = PN;
}

void ExpCodeGenerator::visit(ast::CaseExp &exp)
{
    llvm::errs() << "case not implemented";
}

namespace
{

void typeError(llvm::StringRef expected, llvm::StringRef actual)
{
    llvm::errs() << llvm::formatv("type mismatch, expected '{0}' actual '{1}'", expected, actual);
}

bool checkNum(type::Type *type)
{
    if (!type->isNum())
    {
        typeError(type->name(), "Num"); // TODO: TypeClass name
        return false;
    }
    return true;
}

bool checkInteger(type::Type *type)
{
    if (!type->isInteger())
    {
        typeError(type->name(), "Integer"); // TODO: TypeClass name
        return false;
    }
    return true;
}

bool checkBool(type::Type *type)
{
    if (*type != type::name::Bool)
    {
        typeError(type->name(), "Bool"); // TODO: TypeClass name
        return false;
    }
    return true;
}

} // namespace

void ExpCodeGenerator::visit(ast::BinaryExp &exp)
{
    // math
    if (exp.operand == "*")// Multiplication
    {
        generateBinary(exp, checkNum,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFMul(v1, v2, "fmul"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateMul(v1, v2, "mul", false, false); });
    }
    else if (exp.operand == "/") // Division
    {
        generateBinary(exp, checkNum,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFMul(v1, v2, "fmul"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateSDiv(v1, v2, "sdiv", false); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateUDiv(v1, v2, "udiv", false); });
    }
    else if (exp.operand == "%") // Remainder
    {
        generateBinary(exp, checkNum,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFRem(v1, v2, "frem"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateSRem(v1, v2, "srem"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateURem(v1, v2, "urem"); });
    }
    else if (exp.operand == "+") // Addition
    {
        generateBinary(exp, checkNum,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFAdd(v1, v2, "fadd"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateAdd(v1, v2, "add"); });
    }
    else if (exp.operand == "-") // Subtraction
    {
        generateBinary(exp, checkNum,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFSub(v1, v2, "fsub"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateSub(v1, v2, "sub"); });
    }
    // bitwise
    else if (exp.operand == "<<") // Shift
    {
        // Both arguments to the ‘shl’ instruction must be the same integer or vector of integer type. ‘op2’ is treated as an unsigned value.
        generateBinary(exp, checkInteger,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateShl(v1, v2, "shl", false, false); });
    }
    else if (exp.operand == ">>") // Signed shift
    {
        generateBinary(exp, checkInteger,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateAShr(v1, v2, "ashr", false); });
    }
    else if (exp.operand == ">>>") // Logical shift
    {
        generateBinary(exp, checkInteger,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateLShr(v1, v2, "lshr", false); });
    }
    // compare
    else if (exp.operand == ">")  { generateCompare(llvm::CmpInst::Predicate::ICMP_UGT, exp); } // Greater than
    else if (exp.operand == ">=") { generateCompare(llvm::CmpInst::Predicate::ICMP_UGE, exp); } // Greater or equal
    else if (exp.operand == "<")  { generateCompare(llvm::CmpInst::Predicate::ICMP_ULT, exp); } // Less than
    else if (exp.operand == "<=") { generateCompare(llvm::CmpInst::Predicate::ICMP_ULE, exp); } // Less or equal
    else if (exp.operand == "==") { generateCompare(llvm::CmpInst::Predicate::ICMP_EQ, exp); }  // Equal
    else if (exp.operand == "!=") { generateCompare(llvm::CmpInst::Predicate::ICMP_NE, exp); }  // Not equal
    // bitwise
    else if (exp.operand == "&")
    {
        generateBinary(exp, checkInteger,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateAnd(v1, v2, "and"); });
    }
    else if (exp.operand == "|")
    {
        generateBinary(exp, checkInteger,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateOr(v1, v2, "or"); });
    }
    else if (exp.operand == "^")
    {
        generateBinary(exp, checkInteger,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateXor(v1, v2, "xor"); });
    }
    // logical
    else if (exp.operand == "&&")
    {
        generateBinary(exp, checkBool,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateAnd(v1, v2, "and"); });
    }
    else if (exp.operand == "||")
    {
        generateBinary(exp, checkBool,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateOr(v1, v2, "or"); });
    }
    else
    {
        llvm::errs() << "unknown operator: " << exp.operand;
    }
}

namespace
{

constexpr llvm::CmpInst::Predicate convertToFPredicate(llvm::CmpInst::Predicate predicate)
{
    switch (predicate)
    {
    case llvm::CmpInst::ICMP_EQ:  return llvm::CmpInst::FCMP_OEQ;
    case llvm::CmpInst::ICMP_NE:  return llvm::CmpInst::FCMP_ONE;
    case llvm::CmpInst::ICMP_UGT: return llvm::CmpInst::FCMP_OGT;
    case llvm::CmpInst::ICMP_UGE: return llvm::CmpInst::FCMP_OGE;
    case llvm::CmpInst::ICMP_ULT: return llvm::CmpInst::FCMP_OLT;
    case llvm::CmpInst::ICMP_ULE: return llvm::CmpInst::FCMP_OLE;
    default: return llvm::CmpInst::BAD_FCMP_PREDICATE;
    }
}

constexpr llvm::CmpInst::Predicate convertToSPredicate(llvm::CmpInst::Predicate predicate)
{
    switch (predicate)
    {
    case llvm::CmpInst::ICMP_EQ:  return llvm::CmpInst::ICMP_EQ;
    case llvm::CmpInst::ICMP_NE:  return llvm::CmpInst::ICMP_NE;
    case llvm::CmpInst::ICMP_UGT: return llvm::CmpInst::ICMP_SGT;
    case llvm::CmpInst::ICMP_UGE: return llvm::CmpInst::ICMP_SGE;
    case llvm::CmpInst::ICMP_ULT: return llvm::CmpInst::ICMP_SLT;
    case llvm::CmpInst::ICMP_ULE: return llvm::CmpInst::ICMP_SLE;
    default: return llvm::CmpInst::BAD_ICMP_PREDICATE;
    }
}

} // namespace

void ExpCodeGenerator::generateCompare(llvm::CmpInst::Predicate predicate, ast::BinaryExp &exp)
{
    if (*expectedType != type::name::Bool)
    {
        typeError(expectedType->name(), type::name::Bool);
    }
    else
    {
        auto lhsT = type::TypeInferer::infer(*exp.lhs, this);
        auto rhsT = type::TypeInferer::infer(*exp.rhs, this);

        if (lhsT == nullptr || rhsT == nullptr)
        {
            return;
        }

        auto type = lhsT->unify(rhsT, this);
        if (type != nullptr)
        {
            auto args = generateBinary(type, exp);
            auto arg1 = std::get<0>(args), arg2 = std::get<1>(args);
            if (arg1 == nullptr || arg2 == nullptr)
            {
                return;
            }
            if (type->isFloating())
            {
                result = llvmBuilder().CreateFCmp(convertToFPredicate(predicate), arg1, arg2, "fcmp");
            }
            else if (type->isSigned())
            {
                result = llvmBuilder().CreateICmp(convertToSPredicate(predicate), arg1, arg2, "scmp");
            }
            else
            {
                result = llvmBuilder().CreateICmp(predicate, arg1, arg2, "icmp");
            }
        }
        else
        {
            llvm::errs() << "failed to unify types: " << lhsT->name() << " and " << rhsT->name();
        }
    }
}

void ExpCodeGenerator::visit(ast::UnaryExp &exp)
{
    if (exp.op == "-")
    {
        if (!expectedType->isSigned())
        {
            typeError(expectedType->name(), "Signed"); // TODO: TypeClass name
        }
        else
        {
            auto value = ExpCodeGenerator::generate(*exp.operand, expectedType, this);
            if (value != nullptr)
            {
                if (expectedType->isFloating())
                {
                    result = llvmBuilder().CreateFNeg(value, "fneg");
                }
                else
                {
                    result = llvmBuilder().CreateNeg(value, "neg");
                }
            }
        }
    }
    else if (exp.op == "!")
    {
        if (*expectedType != type::name::Bool)
        {
            typeError(expectedType->name(), type::name::Bool);
        }
        else
        {
            auto value = ExpCodeGenerator::generate(*exp.operand, expectedType, this);
            if (value != nullptr)
            {
                result = llvmBuilder().CreateNot(value, "lnot");
            }
        }
    }
    else if (exp.op == "~")
    {
        if (!expectedType->isInteger())
        {
            typeError(expectedType->name(), "Integer"); // TODO: TypeClass name
        }
        else
        {
            auto value = ExpCodeGenerator::generate(*exp.operand, expectedType, this);
            if (value != nullptr)
            {
                result = llvmBuilder().CreateNot(value, "bnot");
            }
        }
    }
    else
    {
        llvm::errs() << "unknown unary operator: " << exp.op;
    }
}

void ExpCodeGenerator::visit(ast::LiteralExp &exp)
{
    llvm::Type *type = expectedType->llvmType();

    // TODO: test if value fits in expectedType bit size

    switch (exp.tokenType)
    {
    case lex::tok_integer:

        if (expectedType->isFloating())
        {
            result = llvm::ConstantFP::get(type, exp.value);
        }
        else if (expectedType->isInteger())
        {
            result = llvm::ConstantInt::get(llvm::IntegerType::get(generator->llvmContext, type->getIntegerBitWidth()), exp.value, 10);
        }
        else
        {
            typeError(expectedType->name(), "Num"); // TODO: TypeClass name
        }
        break;

    case lex::tok_float:

        if (!expectedType->isFloating())
        {
            typeError(expectedType->name(), "Floating"); // TODO: TypeClass name
        }
        else
        {
            result = llvm::ConstantFP::get(type, exp.value);
        }
        break;

    case lex::tok_char:

        if (*expectedType != type::name::Char)
        {
            typeError(expectedType->name(), type::name::Char);
        }
        else
        {
            assert(exp.value.size() == 1);
            result = llvm::ConstantInt::get(type, exp.value[0]);
        }
        break;

    case lex::tok_string:

        llvm::errs() << "string not supported";
        break;

    case lex::tok_bool:

        if (*expectedType != type::name::Bool)
        {
            typeError(expectedType->name(), type::name::Bool);
        }
        else
        {
            if (exp.value == "true")
            {
                result = llvm::ConstantInt::getTrue(generator->llvmContext);
            }
            else if (exp.value == "false")
            {
                result = llvm::ConstantInt::getFalse(generator->llvmContext);
            }
            else
            {
                // we should not have constructed ast::LiteralExp with tokenType lex::tok_bool
                assert(false);
            }
        }
        break;

    default:

        assert(false);
        break;
    }
}

void ExpCodeGenerator::visit(ast::CallExp &exp)
{
    auto x = functions().find(exp.identifier);
    if (x == functions().end())
    {
        llvm::errs() << "unknown function: " << exp.identifier;
    }
    else
    {
        auto function = x->second;

        if (*expectedType != function.ast->typeName)
        {
            typeError(expectedType->name(), function.ast->typeName);
            return;
        }

        if (function.ast->parameters.size() != exp.arguments.size())
        {
            llvm::errs() << "parameter count mismatch";
            return;
        }

        std::vector<llvm::Value*> arguments;
        for (size_t i = 0; i < exp.arguments.size(); ++i)
        {
            auto parameterType = function.ast->parameters[i]->typeName;
            arguments.push_back(generate(*exp.arguments[i], typeContext().getType(parameterType), this));
            if (arguments.back() == nullptr)
            {
                return;
            }
        }

        result = llvmBuilder().CreateCall(function.llvm, arguments, "call");
    }
}

void ExpCodeGenerator::visit(ast::VariableExp &exp)
{
    auto x = getNamedValue(exp.identifier);
    if (x.value == nullptr)
    {
        auto y = functions().find(exp.identifier);
        if (y != functions().end())
        {
            auto function = y->second;
            if (function.ast->parameters.empty())
            {
                // generate call
                if (*expectedType != function.ast->typeName)
                {
                    typeError(expectedType->name(), function.ast->typeName);
                    return;
                }
                result = llvmBuilder().CreateCall(function.llvm, std::vector<llvm::Value*>{}, "call");
            }
            else
            {
                llvm::errs() << exp.identifier << " is not a zero parameter function";
            }
        }
        else
        {
            llvm::errs() << "unknown identifier: " << exp.identifier;
        }
    }
    else
    {
        // check type
        if (x.type != expectedType)
        {
            typeError(expectedType->name(), x.type->name());
            return;
        }
        result = x.value;
    }
}

const Value& ExpCodeGenerator::getNamedValue(const std::string &name)
{
    auto it = values.find(name);
    if (it == values.end())
    {
        if (parent == nullptr)
        {
            return EmptyValue;
        }
        else
        {
            return parent->getNamedValue(name);
        }
    }
    return it->second;
}

type::Type* ExpCodeGenerator::getTypeByName(llvm::StringRef name)
{
    return typeContext().getType(name);
}

type::Type* ExpCodeGenerator::getVariableType(llvm::StringRef variable)
{
    return getNamedValue(variable).type;
}

type::Type* ExpCodeGenerator::getFunctionReturnType(llvm::StringRef variable)
{
    auto it = functions().find(variable);
    if (it == functions().end())
    {
        llvm::errs() << "unknown function: " << variable;
        return nullptr;
    }
    return typeContext().getType(it->second.ast->typeName);
}

llvm::Value* ExpCodeGenerator::getResult()
{
    return result;
}

} // namespace codegen
} // namespace hpfp
