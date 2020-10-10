
#pragma warning(push, 0)
// C4996 use of function, class member, variable, or typedef that's marked deprecated
#pragma warning(disable : 4996)

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/Support/FormatVariadic.h"

#pragma warning(pop)

#include "Log.h"
#include "Module.h"

#include "Codegen.h"


namespace llfp
{
namespace codegen
{

const Value ExpCodeGenerator::EmptyValue{ nullptr, nullptr };

CodeGenerator::CodeGenerator(SourceModule *sourceModule_) :
    sourceModule{ sourceModule_ },
    llvmContext{},
    llvmBuilder(llvmContext),
    typeContext(llvmContext)
{
    llvmModule = std::make_unique<llvm::Module>(sourceModule->name(), llvmContext);
}

bool CodeGenerator::generateDataDeclarations(std::vector<std::unique_ptr<ast::DataDeclaration>> &dataDeclarations)
{
    std::vector<llvm::StructType*> llvmTypes;

    for (auto &dataDecl : dataDeclarations)
    {
        auto llvmType = llvm::StructType::create(llvmContext, dataDecl->name);
        llvmTypes.push_back(llvmType);
        if (!typeContext.addType(std::make_unique<llfp::type::Type>(dataDecl->name, llvmType, false)))
        {
            return false;
        }
    }

    int i = 0;
    for (auto &dataDecl : dataDeclarations)
    {
        assert(llvmTypes[i]->getName() == dataDecl->name);

        std::vector<llvm::Type*> fieldTypes;
        for (auto &field : dataDecl->fields)
        {
            fieldTypes.push_back(typeContext.getType(field.typeName)->llvmType());
            if (fieldTypes.back() == nullptr)
            {
                Log(field.location, "unkown type:", field.typeName);
                return false;
            }
        }

        llvmTypes[i]->setBody(fieldTypes);
        ++i;
    }
}

bool CodeGenerator::generateFunction(const ast::FunctionDeclaration *ast)
{
    std::vector<type::Type*> types;

    if (ast->typeName.empty())
    {
        Log(ast->location, "generating exported function with unbound return type");
        return false;
    }
    auto retType = typeContext.getType(ast->typeName);
    if (retType == nullptr)
    {
        Log(ast->location, "unknown type:", ast->typeName);
        return false;
    }
    types.push_back(retType);

    for (auto &param : ast->parameters)
    {
        if (param->typeName.empty())
        {
            Log(ast->location, "generating exported function containing unbound paramter types");
            return false;
        }
        auto type = typeContext.getType(param->typeName);
        if (type == nullptr)
        {
            Log(ast->location, "unknown type:", param->typeName);
            return false;
        }
        types.push_back(type);
    }

    auto f = generatePrototype(sourceModule, ast, std::move(types));
    if (f->llvm->empty())
    {
        return generateFunctionBody(f);
    }
    return true;
}

bool CodeGenerator::generateFunction(const ast::FunctionDeclaration *ast, std::vector<type::Type*> types)
{
    auto f = generatePrototype(sourceModule, ast, std::move(types));
    if (f != nullptr && f->llvm->empty())
    {
        return generateFunctionBody(f);
    }
    return f != nullptr;
}

Function* CodeGenerator::generatePrototype(const ImportedModule* module, const ast::FunctionDeclaration *ast, std::vector<type::Type*> types)
{
    if (types.empty())
    {
        Log(ast->location, "no return type specified");
        return nullptr;
    }
    if (types.size() - 1 != ast->parameters.size())
    {
        Log(ast->location, "wrong number of parameters"); // location should be called..., maybe we check at call site also?
        return nullptr;
    }

    // unify types / type check / remove literals (requires the value...)

    types[0] = types[0]->unify(typeContext.getType(ast->typeName), &typeContext);
    for (size_t i = 1; i < types.size(); ++i)
    {
        types[i] = types[i]->unify(typeContext.getType(ast->parameters[i - 1]->typeName), &typeContext);
    }

    if (std::any_of(types.begin(), types.end(), [](auto x) { return x == nullptr; }))
    {
        Log(ast->location, "failed to unify function types");
        return nullptr;
    }

    auto name = module->getMangledName(ast, types);

    auto funIt = functions.find(name);
    if (funIt == functions.end())
    {
        auto returnType = types.front()->llvmType();

        std::vector<llvm::Type*> parameterTypes;
        for (auto typeIt = types.begin() + 1; typeIt != types.end(); ++typeIt)
        {
            parameterTypes.push_back((*typeIt)->llvmType());
        }

        auto functionType = llvm::FunctionType::get(returnType, parameterTypes, false);
        auto llvmFunction = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, llvmModule.get());

        size_t i = 0;
        for (auto &p : llvmFunction->args())
        {
            p.setName(ast->parameters[i++]->identifier);
        }

        Function fun{ ast, llvmFunction, std::move(types) };
        funIt = functions.insert(std::make_pair(std::move(name), std::move(fun))).first;
    }
    return &funIt->second;
}

bool CodeGenerator::generateFunctionBody(Function *function)
{
    auto llvmFunction = function->llvm;
    auto ast = function->ast;
    auto& types = function->types;

    auto bb = llvm::BasicBlock::Create(llvmContext, "entry", llvmFunction);
    llvmBuilder.SetInsertPoint(bb);

    std::map<std::string, Value> namedValues;
    for (size_t i = 0; i < ast->parameters.size(); ++i)
    {
        auto& llvmArg = *(llvmFunction->arg_begin() + i);
        if (namedValues.find(llvmArg.getName()) != namedValues.end())
        {
            Log(ast->location, "duplicate parameter: ", llvmArg.getName());
            return false;
        }

        if (!ast->parameters[i]->typeName.empty())
        {
            if (*types[i + 1] != ast->parameters[i]->typeName)
            {
                Log(ast->location, "type mismatch, expected '", ast->parameters[i]->typeName, "' actual '", types[i + 1]->name(), '\'');
                return false;
            }
        }

        namedValues[llvmArg.getName()] = { types[i + 1], &llvmArg };
    }

    // type check return
    if (!ast->typeName.empty())
    {
        if (*types[0] != ast->typeName)
        {
            Log(ast->location, "type mismatch, expected '", ast->typeName, "' actual '", types[0]->name(), '\'');
            return false;
        }
    }

    ExpCodeGenerator expGenerator(types[0], this, std::move(namedValues));
    ast->functionBody->accept(&expGenerator);
    llvmBuilder.CreateRet(expGenerator.getResult());

    return expGenerator.getResult() != nullptr;
}

// TODO: only once per dll
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

Function* CodeGenerator::getFunction(llvm::StringRef moduleName, llvm::StringRef name, std::vector<type::Type*> types)
{
    ImportedModule *importedModule = nullptr;
    const ast::FunctionDeclaration *function = nullptr;
    if (sourceModule->lookupFunction(moduleName, name, importedModule, function))
    {
        auto proto = generatePrototype(importedModule, function, std::move(types));
        if (proto != nullptr)
        {
            importedModule->requireFunctionInstance({ name, &proto->types });
        }
        return proto;
    }
    Log({}, "unknown function: ", moduleName, ":", name);
    return nullptr;
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
            Log(exp.location, "let function definitions not implemented");
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
        if (values.find(var->name) != values.end())
        {
            Log(exp.location, "already defined");
            return;
        }
        else
        {
            values[var->name] = { varType, value };
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
    Log(exp.location, "case not implemented");
}

namespace
{

void typeError(ast::Exp &exp, llvm::StringRef expected, llvm::StringRef actual)
{
    Log(exp.location, "type mismatch, expected '", expected, "' actual '", actual, '\'');
}

bool checkNum(ast::Exp &exp, type::Type *type)
{
    if (!type->isNum())
    {
        typeError(exp, type->name(), "Num"); // TODO: TypeClass name
        return false;
    }
    return true;
}

bool checkInteger(ast::Exp &exp, type::Type *type)
{
    if (!type->isInteger())
    {
        typeError(exp, type->name(), "Integer"); // TODO: TypeClass name
        return false;
    }
    return true;
}

bool checkBool(ast::Exp &exp, type::Type *type)
{
    if (*type != type::name::Bool)
    {
        typeError(exp, type->name(), "Bool"); // TODO: TypeClass name
        return false;
    }
    return true;
}

} // namespace

void ExpCodeGenerator::visit(ast::BinaryExp &exp)
{
    // math
    if (exp.op == "*")// Multiplication
    {
        generateBinary(exp, checkNum,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFMul(v1, v2, "fmul"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateMul(v1, v2, "mul", false, false); });
    }
    else if (exp.op == "/") // Division
    {
        generateBinary(exp, checkNum,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFMul(v1, v2, "fmul"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateSDiv(v1, v2, "sdiv", false); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateUDiv(v1, v2, "udiv", false); });
    }
    else if (exp.op == "%") // Remainder
    {
        generateBinary(exp, checkNum,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFRem(v1, v2, "frem"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateSRem(v1, v2, "srem"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateURem(v1, v2, "urem"); });
    }
    else if (exp.op == "+") // Addition
    {
        generateBinary(exp, checkNum,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFAdd(v1, v2, "fadd"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateAdd(v1, v2, "add"); });
    }
    else if (exp.op == "-") // Subtraction
    {
        generateBinary(exp, checkNum,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFSub(v1, v2, "fsub"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateSub(v1, v2, "sub"); });
    }
    // bitwise
    else if (exp.op == "<<") // Shift
    {
        // Both arguments to the 'shl' instruction must be the same integer or vector of integer type. 'op2' is treated as an unsigned value.
        generateBinary(exp, checkInteger,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateShl(v1, v2, "shl", false, false); });
    }
    else if (exp.op == ">>") // Signed shift
    {
        generateBinary(exp, checkInteger,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateAShr(v1, v2, "ashr", false); });
    }
    else if (exp.op == ">>>") // Logical shift
    {
        generateBinary(exp, checkInteger,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateLShr(v1, v2, "lshr", false); });
    }
    // compare
    else if (exp.op == ">") { generateCompare(llvm::CmpInst::Predicate::ICMP_UGT, exp); } // Greater than
    else if (exp.op == ">=") { generateCompare(llvm::CmpInst::Predicate::ICMP_UGE, exp); } // Greater or equal
    else if (exp.op == "<") { generateCompare(llvm::CmpInst::Predicate::ICMP_ULT, exp); } // Less than
    else if (exp.op == "<=") { generateCompare(llvm::CmpInst::Predicate::ICMP_ULE, exp); } // Less or equal
    else if (exp.op == "==") { generateCompare(llvm::CmpInst::Predicate::ICMP_EQ, exp); }  // Equal
    else if (exp.op == "!=") { generateCompare(llvm::CmpInst::Predicate::ICMP_NE, exp); }  // Not equal
    // bitwise
    else if (exp.op == "&")
    {
        generateBinary(exp, checkInteger,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateAnd(v1, v2, "and"); });
    }
    else if (exp.op == "|")
    {
        generateBinary(exp, checkInteger,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateOr(v1, v2, "or"); });
    }
    else if (exp.op == "^")
    {
        generateBinary(exp, checkInteger,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateXor(v1, v2, "xor"); });
    }
    // logical
    else if (exp.op == "&&")
    {
        generateBinary(exp, checkBool,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateAnd(v1, v2, "and"); });
    }
    else if (exp.op == "||")
    {
        generateBinary(exp, checkBool,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateOr(v1, v2, "or"); });
    }
    else
    {
        Log(exp.location, "unknown operator: ", exp.op);
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
        typeError(exp, expectedType->name(), type::name::Bool);
    }
    else
    {
        auto lhsT = type::TypeInferer::infer(*exp.lhs, this);
        auto rhsT = type::TypeInferer::infer(*exp.rhs, this);

        if (lhsT == nullptr || rhsT == nullptr)
        {
            return;
        }

        auto type = lhsT->unify(rhsT, getTypeContext());
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
            Log(exp.location, "failed to unify types: ", lhsT->name(), " and ", rhsT->name());
        }
    }
}

void ExpCodeGenerator::visit(ast::UnaryExp &exp)
{
    if (exp.op == "-")
    {
        if (!expectedType->isSigned())
        {
            typeError(exp, expectedType->name(), "Signed"); // TODO: TypeClass name
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
            typeError(exp, expectedType->name(), type::name::Bool);
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
            typeError(exp, expectedType->name(), "Integer"); // TODO: TypeClass name
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
        Log(exp.location, "unknown unary operator: ", exp.op);
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
            result = llvm::ConstantInt::get(llvm::IntegerType::get(llvmContext(), type->getIntegerBitWidth()), exp.value, 10);
        }
        else
        {
            typeError(exp, expectedType->name(), "Num"); // TODO: TypeClass name
        }
        break;

    case lex::tok_float:

        if (!expectedType->isFloating())
        {
            typeError(exp, expectedType->name(), "Floating"); // TODO: TypeClass name
        }
        else
        {
            result = llvm::ConstantFP::get(type, exp.value);
        }
        break;

    case lex::tok_char:

        if (*expectedType != type::name::Char)
        {
            typeError(exp, expectedType->name(), type::name::Char);
        }
        else
        {
            assert(exp.value.size() == 1);
            result = llvm::ConstantInt::get(type, exp.value[0]);
        }
        break;

    case lex::tok_string:

        Log(exp.location, "string not supported");
        break;

    case lex::tok_bool:

        if (*expectedType != type::name::Bool)
        {
            typeError(exp, expectedType->name(), type::name::Bool);
        }
        else
        {
            if (exp.value == "true")
            {
                result = llvm::ConstantInt::getTrue(llvmContext());
            }
            else if (exp.value == "false")
            {
                result = llvm::ConstantInt::getFalse(llvmContext());
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
    std::vector<type::Type*> types;
    types.push_back(expectedType);
    for (auto &arg : exp.arguments)
    {
        auto type = type::TypeInferer::infer(*arg, this);
        if (type == nullptr)
        {
            return;
        }
        else
        {
            types.push_back(type);
        }
    }

    auto function = getFunction(exp.moduleName, exp.name, std::move(types));
    if (function == nullptr)
    {
        Log(exp.location, "error calling: ", exp.moduleName, ":", exp.name);
    }
    else
    {
        std::vector<llvm::Value*> arguments;
        for (size_t i = 0; i < exp.arguments.size(); ++i)
        {
            arguments.push_back(generate(*exp.arguments[i], function->types[i + 1], this));
            if (arguments.back() == nullptr)
            {
                return;
            }
        }

        result = llvmBuilder().CreateCall(function->llvm, arguments, "call");
    }
}

void ExpCodeGenerator::visit(ast::VariableExp &exp)
{
    if (exp.moduleName.empty())
    {
        auto x = getNamedValue(exp.name);
        if (x.value != nullptr)
        {
            // check type
            if (x.type != expectedType)
            {
                typeError(exp, expectedType->name(), x.type->name());
                return;
            }
            result = x.value;
            return;
        }
    }

    std::vector<type::Type*> types;
    types.push_back(expectedType);

    auto y = getFunction(exp.moduleName, exp.name, std::move(types));
    if (y != nullptr)
    {
        auto &function = *y;
        if (function.ast->parameters.empty())
        {
            // generate call
            if (*expectedType != function.ast->typeName)
            {
                typeError(exp, expectedType->name(), function.ast->typeName);
                return;
            }
            result = llvmBuilder().CreateCall(function.llvm, std::vector<llvm::Value*>{}, "call");
        }
        else
        {
            Log(exp.location, exp.name, " is not a zero parameter function");
        }
    }
    else
    {
        Log(exp.location, "unknown identifier: ", exp.name);
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

type::Type* ExpCodeGenerator::getVariableType(llvm::StringRef variable)
{
    return getNamedValue(variable).type;
}

Function* ExpCodeGenerator::getFunction(llvm::StringRef moduleName, llvm::StringRef name, std::vector<type::Type*> types)
{
    // TODO: add local functions
    return generator->getFunction(moduleName, name, std::move(types));
}

const ast::FunctionDeclaration* ExpCodeGenerator::getFunctionAST(llvm::StringRef moduleIdent, llvm::StringRef functionName)
{
    llfp::ImportedModule *module = nullptr;
    const ast::FunctionDeclaration *ast = nullptr;
    generator->sourceModule->lookupFunction(moduleIdent, functionName, module, ast);
    return ast;
}

llvm::Value* ExpCodeGenerator::getResult()
{
    return result;
}

} // namespace codegen
} // namespace hpfp
