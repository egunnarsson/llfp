
#include <array>
#include <stack>

#pragma warning(push, 0)
// C4996 use of function, class member, variable, or typedef that's marked deprecated
#pragma warning(disable : 4996)

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/Support/FormatVariadic.h"

#pragma warning(pop)

#include "GlobalContext.h"
#include "Error.h"
#include "Log.h"
#include "Module.h"
#include "String/StringConstants.h"

#include "Codegen.h"


namespace llfp
{
namespace codegen
{

const Value ExpCodeGenerator::EmptyValue{ nullptr, nullptr };

CodeGenerator::CodeGenerator(
    Driver* driver_,
    GlobalContext *globalContext_,
    SourceModule *sourceModule_,
    llvm::LLVMContext* llvmContext_,
    llvm::Module* llvmModule_) :
    driver{ driver_ },
    globalContext{ globalContext_ },
    sourceModule{ sourceModule_ },
    llvmContext{ llvmContext_ },
    llvmBuilder(*llvmContext),
    llvmModule{ llvmModule_ },
    typeContext(*llvmContext, sourceModule, globalContext_)
{
}

bool CodeGenerator::generateFunction(const ast::Function*ast)
{
    std::vector<type::TypeInstPtr> types;

    if (ast->type.identifier.name.empty())
    {
        Log(ast->location, "generating exported function with unbound return type");
        return false;
    }
    auto retType = typeContext.getTypeFromAst(ast->type);
    if (retType == nullptr)
    {
        Log(ast->location, "unknown type: ", ast->type.identifier.str());
        return false;
    }
    types.push_back(retType);

    for (auto &param : ast->parameters)
    {
        if (param->type.identifier.name.empty())
        {
            Log(ast->location, "generating exported function containing unbound parameter types");
            return false;
        }
        auto type = typeContext.getTypeFromAst(param->type);
        if (type == nullptr)
        {
            Log(param->location, "unknown type \"", param->type.identifier.str(), '"');
            return false;
        }
        types.push_back(type);
    }

    auto f = generatePrototype(sourceModule, ast, std::move(types));
    if (f != nullptr && f->llvm->empty())
    {
        return generateFunctionBody(f);
    }
    return f != nullptr;
}

bool CodeGenerator::generateFunction(const ast::Function*ast, std::vector<const type::TypeInstance*> types)
{
    auto f = generatePrototype(sourceModule, ast, std::move(types));
    if (f != nullptr && f->llvm->empty())
    {
        return generateFunctionBody(f);
    }
    return f != nullptr;
}

Function* CodeGenerator::generatePrototype(const ImportedModule* module, const ast::Function* ast, std::vector<const type::TypeInstance*> types)
{
    if (types.empty())
    {
        Log(ast->location, "no return type specified");
        return nullptr;
    }
    if (types.size() - 1 != ast->parameters.size())
    {
        Log(ast->location, "incorrect number of arguments"); // location should be called..., maybe we check at call site also?
        return nullptr;
    }

    hm::TypeAnnotation typeAnnotation;
    try
    {
        typeAnnotation = typeContext.getAnnotation(module, ast);

        for (const auto& [funName, varTypePtr] : typeAnnotation.getFunctions())
        {
            auto funAst = sourceModule->lookupFunction(GlobalIdentifier::split(funName)); // module? not sourceModule?
            if (funAst.empty())
            {
                continue; // could be typeclass and we dont know the type instance yet
            }
            const auto& funAnnotation = typeContext.getAnnotation(funAst.importedModule, funAst.function);
            const auto funType = funAnnotation.getFun(funName);
            typeAnnotation.add(funName, funType);
        }

        // standard module may not have implementation
        if (ast->functionBody != nullptr)
        {
            typeContext.check(typeAnnotation, types[0], typeAnnotation.get(ast->functionBody.get()));
        }

        size_t i = 1;
        for (auto &arg : ast->parameters)
        {
            typeContext.check(typeAnnotation, types[i++], typeAnnotation.getVar(arg->identifier));
        }
    }
    catch (const ErrorLocation& error)
    {
        Log(error.location(), error.what());
        return nullptr;
    }
    catch (const Error& error)
    {
        Log(ast->location, error.what());
        return nullptr;
    }

    auto name = module->getMangledName(ast, types);

    auto funIt = functions.find(name);
    if (funIt == functions.end())
    {
        llvm::Type* returnType = nullptr;
        std::vector<llvm::Type*> parameterTypes;

        const bool ptrReturn = types.front()->isStructType();

        if (ptrReturn)
        {
            returnType = llvm::Type::getVoidTy(*llvmContext);
            parameterTypes.push_back(llvm::PointerType::get(types.front()->llvmType(), 0));
        }
        else
        {
            returnType = types.front()->llvmType();
        }

        for (auto typeIt = types.begin() + 1; typeIt != types.end(); ++typeIt)
        {
            if ((*typeIt)->isStructType())
            {
                parameterTypes.push_back(llvm::PointerType::get((*typeIt)->llvmType(), 0));
            }
            else
            {
                parameterTypes.push_back((*typeIt)->llvmType());
            }
        }

        auto functionType = llvm::FunctionType::get(returnType, parameterTypes, false);
        auto llvmFunction = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, llvmModule);

        auto argRange = llvmFunction->args();
        if (ptrReturn)
        {
            argRange.begin()->addAttr(llvm::Attribute::NoAlias);
            argRange.begin()->addAttr(llvm::Attribute::get(*llvmContext, llvm::Attribute::Alignment, 8));
            argRange = { argRange.begin() + 1, argRange.end() };
        }

        size_t i = 0;
        for (auto &arg : argRange)
        {
            arg.setName(ast->parameters[i++]->identifier);
        }

        Function fun{ ast, std::move(typeAnnotation), llvmFunction, std::move(types) };
        funIt = functions.insert(std::make_pair(std::move(name), std::move(fun))).first;
    }
    return &funIt->second;
}

bool CodeGenerator::generateFunctionBody(Function *function)
{
    auto llvmFunction = function->llvm;
    auto ast = function->ast;
    auto& types = function->types;

    llvmFunction->addAttribute(0, llvm::Attribute::get(*llvmContext, "frame-pointer", "all"));

    auto bb = llvm::BasicBlock::Create(*llvmContext, "entry", llvmFunction);
    llvmBuilder.SetInsertPoint(bb);

    // if return type is user type it is returned in first argument
    size_t argOffset = types[0]->isStructType() ? 1 : 0;

    std::map<std::string, Value> namedValues;
    for (size_t i = 0; i < ast->parameters.size(); ++i)
    {
        auto &param = ast->parameters[i];
        if (namedValues.find(param->identifier) != namedValues.end())
        {
            Log(param->location, "duplicate parameter \"", param->identifier, '"');
            return false;
        }

        if (!param->type.identifier.name.empty())
        {
            if (!typeContext.equals(types[i + 1], param->type))
            {
                Log(ast->location, "type mismatch, expected '", param->type.identifier.str(), "' actual '", types[i + 1]->identifier().str(), '\'');
                return false;
            }
        }

        llvm::Value* llvmArg = (llvmFunction->arg_begin() + argOffset + i);
        if (types[i + 1]->isStructType())
        {
            llvmArg = llvmBuilder.CreateLoad(types[i + 1]->llvmType(), llvmArg);
        }
        namedValues[param->identifier] = { types[i + 1], llvmArg };
    }

    // type check return
    if (!ast->type.identifier.name.empty())
    {
        if (!typeContext.equals(types[0], ast->type))
        {
            Log(ast->location, "type mismatch, expected '", ast->type.identifier.str(), "' actual '", types[0]->identifier().str(), '\'');
            return false;
        }
    }

    ExpCodeGenerator expGenerator(types[0], this, std::move(namedValues), &function->typeAnnotation);
    ast->functionBody->accept(&expGenerator);

    if (expGenerator.getResult() != nullptr)
    {
        if (types[0]->isStructType())
        {
            auto store = llvmBuilder.CreateStore(expGenerator.getResult(), llvmFunction->arg_begin());
            store->setAlignment(llvm::Align(8));
            llvmBuilder.CreateRetVoid();
        }
        else
        {
            llvmBuilder.CreateRet(expGenerator.getResult());
        }

        return true;
    }
    else
    {
        return false;
    }
}

// TODO: only once per dll
void CodeGenerator::AddDllMain()
{
    std::vector<llvm::Type*> parameterTypes;
    parameterTypes.push_back(llvm::Type::getInt8PtrTy(*llvmContext));
    parameterTypes.push_back(llvm::Type::getInt32Ty(*llvmContext));
    parameterTypes.push_back(llvm::Type::getInt8PtrTy(*llvmContext));
    auto returnType = llvm::Type::getInt32Ty(*llvmContext);
    llvm::FunctionType *functionType = llvm::FunctionType::get(returnType, parameterTypes, false);
    auto llvmFunction = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, "_DllMainCRTStartup", llvmModule);
    llvmFunction->setCallingConv(llvm::CallingConv::X86_StdCall);
    auto bb = llvm::BasicBlock::Create(*llvmContext, "entry", llvmFunction);
    llvmBuilder.SetInsertPoint(bb);
    auto value = llvm::ConstantInt::get(returnType, 1);
    llvmBuilder.CreateRet(value);
}

namespace
{

bool typeVarEqual(const std::string& typeVar, const ast::TypeIdentifier& type)
{
    return type.parameters.empty() &&
        type.identifier.moduleName.empty() &&
        type.identifier.name == typeVar;
}

std::stack<int> findTypeVariableIndex(const std::string &typeVar, const ast::TypeIdentifier& type)
{
    int paramIndex = 0;
    for (auto& param : type.parameters)
    {
        if (typeVarEqual(typeVar, param))
        {
            std::stack<int> typeVarIndex;
            typeVarIndex.push(paramIndex);
            return typeVarIndex;
        }

        {
            auto typeVarIndex = findTypeVariableIndex(typeVar, param);
            if (!typeVarIndex.empty())
            {
                typeVarIndex.push(paramIndex);
                return typeVarIndex;
            }
        }

        ++paramIndex;
    }
    return {};
}

type::Identifier getTypeByIndex(std::stack<int> index, const type::TypeInstance& type)
{
    if (index.empty())
    {
        return type.identifier();
    }
    else
    {
        auto currentIndex = index.top();
        index.pop();
        return getTypeByIndex(std::move(index), *type.getTypeParameter(currentIndex));
    }
}

type::Identifier findInstanceType(const ast::Class* classDecl, const ast::FunctionDeclaration* funDecl, const std::vector<const type::TypeInstance*>& types)
{
    auto& typeVar = classDecl->typeVariable;

    int first = -1;
    auto typeVariableIndex = findTypeVariableIndex(typeVar, funDecl->type);
    if (typeVariableIndex.empty() && !typeVarEqual(typeVar, funDecl->type))
    {
        for (int paramIndex = 0; paramIndex < funDecl->parameters.size(); ++paramIndex)
        {
            auto& paramTypeId = funDecl->parameters[paramIndex]->type;
            typeVariableIndex = findTypeVariableIndex(typeVar, paramTypeId);
            if (!typeVariableIndex.empty() || typeVarEqual(typeVar, paramTypeId))
            {
                first = paramIndex + 1;
                break;
            }
        }
    }
    else
    {
        first = 0;
    }

    if (first == -1)
    {
        return {};
    }
    else
    {
        //TODO: if this type is not concrete we should try and find another index
        return getTypeByIndex(std::move(typeVariableIndex), *types[first]);
    }
}

} // namespace

Function* CodeGenerator::getFunction(const GlobalIdentifier& identifier, std::vector<type::TypeInstPtr> types)
{
    auto ast = sourceModule->lookupFunction(identifier);
    auto astDecl = sourceModule->lookupFunctionDecl(identifier);

    if (!astDecl.empty())
    {
        if (!ast.empty())
        {
            throw Error(std::string{ "reference to \"" } + identifier.str() + "\" is ambiguous");
        }

        type::Identifier instanceType = findInstanceType(astDecl.class_, astDecl.function, types);
        assert(!instanceType.name.name.empty());

        auto instanceAst = globalContext->lookupInstance(identifier.name, instanceType);
        if (instanceAst.empty())
        {
            throw Error(std::string{ "no instance of \"" } + astDecl.importedModule->name() + ':' + astDecl.class_->name + ' ' + instanceType.str() + '"');
        }

        ast = instanceAst;
    }

    if (ast.function == nullptr)
    {
        throw Error(std::string{ "undefined function \"" } + identifier.str() +'"');
    }

    auto proto = generatePrototype(ast.importedModule, ast.function, std::move(types));
    if (proto != nullptr)
    {
        driver->push({ ast, &proto->types });
    }
    return proto;
}

ExpCodeGenerator::ExpCodeGenerator(type::TypeInstPtr type_, CodeGenerator *generator_, std::map<std::string, Value> parameters_, hm::TypeAnnotation* typeAnnotation_) :
    parent{ nullptr },
    generator{ generator_ },
    expectedType{ type_ },
    values{ std::move(parameters_) },
    result{ nullptr },
    typeAnnotation{ typeAnnotation_ }
{
    assert(expectedType->llvmType() != nullptr);
}

ExpCodeGenerator::ExpCodeGenerator(type::TypeInstPtr type_, ExpCodeGenerator *parent_, std::map<std::string, Value> scope_) :
    parent{ parent_ },
    generator{ parent_->generator },
    expectedType{ type_ },
    values{ std::move(scope_) },
    result{ nullptr },
    typeAnnotation{ parent_->typeAnnotation }
{
    assert(expectedType->llvmType() != nullptr);
}

llvm::Value* ExpCodeGenerator::generate(ast::Exp& exp, type::TypeInstPtr type, ExpCodeGenerator* parent, std::map<std::string, Value> scope)
{
    if (type == nullptr) // or not?
    {
        return nullptr;
    }
    ExpCodeGenerator generator(type, parent, std::move(scope));
    exp.accept(&generator);
    return generator.result;
}

Function* ExpCodeGenerator::getFunction(const GlobalIdentifier& identifier, std::vector<type::TypeInstPtr> types)
{
    return generator->getFunction(identifier, std::move(types));
}

type::TypeContext* ExpCodeGenerator::getTypeContext()
{
    return generator->getTypeContext();
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

        type::TypeInstPtr varType = var->type.identifier.name.empty() ?
            getTypeContext()->constructTypeUsingAnnotationStuff(*typeAnnotation, exp) :
            getTypeContext()->getTypeFromAst(var->type);

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
    auto condV = ExpCodeGenerator::generate(*exp.condition, getTypeContext()->getBool(), this);
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
    auto PN = builder.CreatePHI(expectedType->llvmType(), 2, "ifphi");

    PN->addIncoming(thenV, thenBB);
    PN->addIncoming(elseV, elseBB);
    result = PN;
}

struct PatternCodeGeneratorContext
{
    llvm::IRBuilder<>&           irBuilder;
    llvm::BasicBlock*            failBlock;
    type::TypeContext*           typeContext;
    hm::TypeAnnotation*          typeAnnotation;
    std::map<std::string, Value> variables;
};

class PatternCodeGenerator : public ast::PatternVisitor
{
    PatternCodeGeneratorContext& context;
    llvm::Value*                 value;
    llvm::BasicBlock*            nextBlock;

public:

    PatternCodeGenerator(PatternCodeGeneratorContext& context_, llvm::Value* value_, llvm::BasicBlock* nextBlock_):
        context{ context_ },
        value{ value_ },
        nextBlock{ nextBlock_ }
    {}

    void visit(ast::BoolPattern& pattern) override
    {
        auto condition = context.irBuilder.CreateCmp(llvm::CmpInst::ICMP_EQ, value, llvm::ConstantInt::getBool(context.irBuilder.getContext(), pattern.value), "boolPattern");
        context.irBuilder.CreateCondBr(condition, nextBlock, context.failBlock);
    }

    void visit(ast::IdentifierPattern& pattern) override
    {
        auto type = context.typeContext->constructTypeUsingAnnotationStuff(*context.typeAnnotation, pattern);
        context.variables.insert({ pattern.value, {type, value} });
        context.irBuilder.CreateBr(nextBlock);
    }

    void visit(ast::IntegerPattern& pattern) override
    {
        unsigned int numBits = value->getType()->getIntegerBitWidth();
        auto patternValue = llvm::ConstantInt::get(llvm::IntegerType::get(context.irBuilder.getContext(), numBits), pattern.value, 10);
        auto condition = context.irBuilder.CreateCmp(llvm::CmpInst::ICMP_EQ, value, patternValue, "integerPattern");
        context.irBuilder.CreateCondBr(condition, nextBlock, context.failBlock);
    }

    void visit(ast::FloatPattern& pattern) override
    {
        auto patternValue = llvm::ConstantFP::get(value->getType(), pattern.value);
        auto condition = context.irBuilder.CreateFCmp(llvm::CmpInst::FCMP_OEQ, value, patternValue, "floatPattern");
        context.irBuilder.CreateCondBr(condition, nextBlock, context.failBlock);
    }

    void visit(ast::CharPattern& pattern) override
    {
        assert(pattern.value.size() == 1);
        auto patternValue = llvm::ConstantInt::get(value->getType(), static_cast<uint8_t>(pattern.value[0]));
        auto condition = context.irBuilder.CreateCmp(llvm::CmpInst::ICMP_EQ, value, patternValue, "charPattern");
        context.irBuilder.CreateCondBr(condition, nextBlock, context.failBlock);
    }

    void visit(ast::StringPattern& pattern) override
    {
        assert(false);
    }

    void visit(ast::ConstructorPattern& pattern) override
    {
        const auto count = static_cast<unsigned int>(pattern.arguments.size());
        assert(count != 0);

        std::vector<llvm::BasicBlock*> blocks;
        for (unsigned int i = 0; i < count; ++i)
        {
            blocks.push_back(llvm::BasicBlock::Create(context.irBuilder.getContext()));
        }

        auto originalBlock = context.irBuilder.GetInsertBlock();
        auto function = originalBlock->getParent();

        for (unsigned int index = 0; index < count; ++index)
        {
            auto branchBlock = blocks[index];
            function->getBasicBlockList().push_back(branchBlock);
            context.irBuilder.SetInsertPoint(branchBlock);

            auto argValue = context.irBuilder.CreateExtractValue(value, { index });

            auto branchNextBlock = index == blocks.size() - 1 ? nextBlock : blocks[static_cast<size_t>(index) + 1];
            PatternCodeGenerator gen{ context, argValue, branchNextBlock };
            pattern.arguments[index].pattern->accept(&gen);
        }

        // for now constructors only have one instance, i.e. always true condition
        context.irBuilder.SetInsertPoint(originalBlock);
        auto condition = llvm::ConstantInt::getTrue(context.irBuilder.getContext());
        context.irBuilder.CreateCondBr(condition, blocks.front(), context.failBlock);
    }
};

void ExpCodeGenerator::visit(ast::CaseExp &exp)
{
    const auto count = exp.clauses.size();
    assert(count != 0);
    auto& builder = llvmBuilder();
    auto function = builder.GetInsertBlock()->getParent();

    auto caseT = getTypeContext()->constructTypeUsingAnnotationStuff(*typeAnnotation, *exp.caseExp);
    auto caseV = ExpCodeGenerator::generate(*exp.caseExp, caseT, this);

    std::vector<llvm::BasicBlock*> expBlocks;
    std::vector<llvm::BasicBlock*> patternBlocks;
    for (size_t i = 0; i < count; ++i)
    {
        expBlocks.push_back(llvm::BasicBlock::Create(llvmContext(), "caseExp"));
        patternBlocks.push_back(llvm::BasicBlock::Create(llvmContext(), "casePattern"));
    }
    auto mergeBB = llvm::BasicBlock::Create(llvmContext(), "caseCont");

    builder.CreateBr(patternBlocks.front());

    std::vector<std::map<std::string, Value>> variables;
    // simplify with -jump-threading?
    for (size_t i = 0; i < count; ++i)
    {
        auto uglyHack = expBlocks.back(); //TODO: replace with call to abort()
        auto failBlock = i == count - 1 ? uglyHack : patternBlocks[i + 1];
        PatternCodeGeneratorContext context{ builder, failBlock, getTypeContext(), typeAnnotation };
        PatternCodeGenerator gen{ context, caseV, expBlocks[i] };

        function->getBasicBlockList().push_back(patternBlocks[i]);
        builder.SetInsertPoint(patternBlocks[i]);
        exp.clauses[i].pattern->accept(&gen);

        variables.push_back(std::move(context.variables));
    }

    std::vector<llvm::Value*> caseValues;
    for (size_t i = 0; i < count; ++i)
    {
        function->getBasicBlockList().push_back(expBlocks[i]);
        builder.SetInsertPoint(expBlocks[i]);
        auto thenV = ExpCodeGenerator::generate(*exp.clauses[i].exp, expectedType, this, std::move(variables[i]));
        // Codegen of 'Clause' can change the current block, update ClauseBB for the PHI.
        expBlocks[i] = builder.GetInsertBlock();
        caseValues.push_back(thenV);
        builder.CreateBr(mergeBB);
    }

    function->getBasicBlockList().push_back(mergeBB);
    builder.SetInsertPoint(mergeBB);
    auto PN = builder.CreatePHI(expectedType->llvmType(), static_cast<unsigned int>(count), "casephi");
    for (size_t i = 0; i < count; ++i)
    {
        PN->addIncoming(caseValues[i], expBlocks[i]);
    }
    result = PN;
}

namespace
{

void typeError(ast::Exp &exp, type::TypeInstPtr expected, llvm::StringRef actual)
{
    Log(exp.location, "type mismatch, expected '", expected->identifier().str(), "' actual '", actual, '\'');
}

bool checkNum(ast::Exp &exp, type::TypeInstPtr type)
{
    if (!type->isNum())
    {
        typeError(exp, type, id::Num);
        return false;
    }
    return true;
}

bool checkInteger(ast::Exp &exp, type::TypeInstPtr type)
{
    if (!type->isInteger())
    {
        typeError(exp, type, id::Integer);
        return false;
    }
    return true;
}

bool checkBool(ast::Exp &exp, type::TypeInstPtr type)
{
    if (!type->isBool())
    {
        typeError(exp, type, id::Bool);
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
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFDiv(v1, v2, "fdiv"); },
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
        generateBinary(exp, checkInteger, // check Signed?
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
    if (!expectedType->isBool())
    {
        typeError(exp, expectedType, id::Bool);
    }
    else
    {
        try
        {
            auto type = getTypeContext()->constructTypeUsingAnnotationStuff(*typeAnnotation, *exp.lhs);
            getTypeContext()->check(*typeAnnotation, type, typeAnnotation->get(exp.rhs.get()));

            auto [arg1, arg2] = generateBinary(type, exp);
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
        catch (const Error& error)
        {
            Log(exp.location, error.what());
        }
    }
}

void ExpCodeGenerator::visit(ast::UnaryExp &exp)
{
    if (exp.op == "-")
    {
        if (!expectedType->isSigned())
        {
            typeError(exp, expectedType, id::Signed);
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
        if (!expectedType->isBool())
        {
            typeError(exp, expectedType, id::Bool);
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
            typeError(exp, expectedType, id::Integer);
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
    case lex::Token::Integer:

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
            typeError(exp, expectedType, id::Num);
        }
        break;

    case lex::Token::Float:

        if (!expectedType->isFloating())
        {
            typeError(exp, expectedType, id::Floating);
        }
        else
        {
            result = llvm::ConstantFP::get(type, exp.value);
        }
        break;

    case lex::Token::Char:

        if (!getTypeContext()->equals(expectedType, id::Char))
        {
            typeError(exp, expectedType, id::Char);
        }
        else
        {
            assert(exp.value.size() == 1);
            result = llvm::ConstantInt::get(type, static_cast<uint8_t>(exp.value[0]));
        }
        break;

    case lex::Token::String:

        Log(exp.location, "string not supported");
        break;

    case lex::Token::Bool:

        if (!expectedType->isBool())
        {
            typeError(exp, expectedType, id::Bool);
        }
        else
        {
            if (exp.value == id::True)
            {
                result = llvm::ConstantInt::getTrue(llvmContext());
            }
            else if (exp.value == id::False)
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
    try
    {
        std::vector<type::TypeInstPtr> types;
        types.push_back(expectedType);
        for (auto& arg : exp.arguments)
        {
            auto type = getTypeContext()->constructTypeUsingAnnotationStuff(*typeAnnotation, *arg);
            types.push_back(type);
        }

        auto function = getFunction(exp.identifier, std::move(types));
        if (function == nullptr)
        {
            return;
        }
        else
        {
            std::vector<llvm::Value*> arguments;

            // Returning user type it is done on the stack, function will return void
            llvm::Value* retValue = nullptr;
            if (expectedType->isStructType())
            {
                retValue = llvmBuilder().CreateAlloca(expectedType->llvmType());
                arguments.push_back(retValue);
            }

            for (size_t i = 0; i < exp.arguments.size(); ++i)
            {
                auto argValue = generate(*exp.arguments[i], function->types[i + 1], this);

                if (argValue == nullptr)
                {
                    return;
                }

                // if arg is user type, alloca and store
                if (argValue->getType()->isStructTy())
                {
                    auto allocaValue = llvmBuilder().CreateAlloca(argValue->getType());
                    llvmBuilder().CreateStore(argValue, allocaValue);
                    argValue = allocaValue;
                }

                arguments.push_back(argValue);
            }

            // if return user type, result = load(first arg)
            if (expectedType->isStructType())
            {
                llvmBuilder().CreateCall(function->llvm, arguments);
                result = llvmBuilder().CreateLoad(expectedType->llvmType(), retValue);
            }
            else
            {
                result = llvmBuilder().CreateCall(function->llvm, arguments, "call");
            }
        }
    }
    catch (const Error &e)
    {
        Log(exp.location, e.what());
    }
}

void ExpCodeGenerator::visit(ast::VariableExp &exp)
{
    if (exp.identifier.moduleName.empty())
    {
        auto x = getNamedValue(exp.identifier.name);
        if (x.value != nullptr)
        {
            // check type
            if (x.type != expectedType)
            {
                typeError(exp, expectedType, x.type->identifier().str());
                return;
            }
            result = x.value;
            return;
        }
    }

    std::vector<type::TypeInstPtr> types;
    types.push_back(expectedType);

    auto y = getFunction(exp.identifier, std::move(types));
    if (y != nullptr)
    {
        auto &function = *y;
        if (function.ast->parameters.empty())
        {
            // generate call
            if (!getTypeContext()->equals(expectedType, function.ast->type))
            {
                typeError(exp, expectedType, function.ast->type.identifier.str());
                return;
            }
            result = llvmBuilder().CreateCall(function.llvm, std::vector<llvm::Value*>{}, "call");
        }
        else
        {
            Log(exp.location, exp.identifier.str(), " is not a zero parameter function");
        }
    }
    else
    {
        Log(exp.location, "unknown identifier: ", exp.identifier.str());
    }
}

void ExpCodeGenerator::visit(ast::FieldExp &exp)
{
    try
    {
        auto lhsType = getTypeContext()->constructTypeUsingAnnotationStuff(*typeAnnotation, *exp.lhs);

        auto index = lhsType->getFieldIndex(exp.fieldIdentifier);
        if (index == type::TypeInstance::InvalidIndex)
        {
            Log(exp.location, "unknown field: ", exp.fieldIdentifier);
            return;
        }

        // type check
        auto fieldType = lhsType->getFieldType(index);
        if (fieldType != expectedType)
        {
            Log(exp.location, "Failed to unify types ", fieldType->identifier().str(), " and ", expectedType->identifier().str());
            return;
        }
        // I think we need both since check might do subs
        // 22-01-31 except constructTypeUsingAnnotationStuff already checks?
        getTypeContext()->check(*typeAnnotation, expectedType, typeAnnotation->get(&exp));

        auto structValue = ExpCodeGenerator::generate(*exp.lhs, lhsType, this);
        if (structValue == nullptr)
        {
            return;
        }

        result = llvmBuilder().CreateExtractValue(structValue, { index });
    }
    catch (const Error& error)
    {
        Log(exp.location, error.what());
    }
}

void ExpCodeGenerator::visit(ast::ConstructorExp &exp)
{
    try
    {
        getTypeContext()->check(*typeAnnotation, expectedType, typeAnnotation->get(&exp));

        const auto size = expectedType->getFieldCount(exp.identifier.name);
        if (size != exp.arguments.size())
        {
            Log(exp.location, "incorrect number of arguments");
            return;
        }

        bool fail = false;
        llvm::Value* value = llvm::UndefValue::get(expectedType->llvmType());
        for (unsigned int i = 0; i < size; ++i)
        {
            auto& arg = exp.arguments[i];

            // at the moment name is only used to check correctness
            if (!arg.name.empty())
            {
                auto index = expectedType->getFieldIndex(exp.identifier.name, arg.name);
                if (index != i)
                {
                    Log(arg.location, index == type::TypeInstance::InvalidIndex
                        ? "unknown field name"
                        : "incorrect field position");
                    fail = true;
                }
            }

            auto argValue = ExpCodeGenerator::generate(*arg.exp, expectedType->getFieldType(exp.identifier.name, i), this);
            if (argValue != nullptr)
            {
                value = llvmBuilder().CreateInsertValue(value, argValue, { i });
            }
            else
            {
                fail = true;
            }
        }

        if (!fail)
        {
            result = value;
        }

    }
    catch (const Error& error)
    {
        Log(exp.location, error.what());
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

llvm::Value* ExpCodeGenerator::getResult()
{
    return result;
}

llvm::Function* CodeGenerator::getCopyFunction(type::TypeInstPtr type) const
{
    assert(false);
    return nullptr;
}

/*
struct T {
    int type;
    union {
        struct a {int x; int y ;};
        struct b {foo* ptr;};
        struct c {T* ptr2;};
    };
};

T* copy(T* src) {
    T* dst = malloc(sizeof(T));
    dst->type = src->type;
    switch (src->type) {
        case 0: dst->x = src->x; dst->y = src->y; break;
        case 1: dst->ptr = copy(src->ptr); break;
        case 2: dst->ptr2 = copy(src->ptr2); break;
        default: abort();
    }
    return dst;
}
*/
llvm::Function* CodeGenerator::generateCopyFunctionBody(type::TypeInstPtr type)
{
    if (!type->isStructType()) { return nullptr; }

    auto name = type->identifier().str();

    llvm::Type* returnType = type->llvmType();
    std::array<llvm::Type*, 1> parameterTypes = { type->llvmType() };

    auto functionType = llvm::FunctionType::get(returnType, parameterTypes, false);
    auto llvmFunction = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, llvmModule);
    auto srcValue = llvmFunction->arg_begin();

    auto entryBB = llvm::BasicBlock::Create(*llvmContext, "entry", llvmFunction);
    auto retBB = llvm::BasicBlock::Create(*llvmContext, "return", llvmFunction);
    llvmBuilder.SetInsertPoint(entryBB);

    // call malloc
    auto intPtrType = llvmModule->getDataLayout().getIntPtrType(*llvmContext);
    auto returnTypeSize = type->getSize(llvmModule);
    auto sizeValue = llvm::ConstantInt::get(intPtrType, returnTypeSize.getFixedSize());
    auto dstValue = llvm::CallInst::CreateMalloc(llvmBuilder.GetInsertBlock(), intPtrType, returnType/*wrong type?*/, sizeValue, nullptr, nullptr, "new");

    // copy "type" enum
    llvm::Value* typeValue = nullptr;
    {
        std::array<llvm::Value*, 2> idxList =
        {
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(*llvmContext), 0),
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(*llvmContext), 0)
        };

        assert(!type->getConstructors().empty());
        assert(!type->getConstructors().front().fields.empty());

        // just grab first type, doesn't matter, all cases have type enum as first value
        auto& firstConstructor = type->getConstructors().front();
        auto enumValuePtr = llvmBuilder.CreateGEP(firstConstructor.llvmType_, srcValue, idxList, "typePtr");
        typeValue = llvmBuilder.CreateLoad(firstConstructor.fields.front()->llvmType(), enumValuePtr, "type");
    }

    std::vector<llvm::BasicBlock*> constructorBlocks;
    for (auto& constructor : type->getConstructors())
    {
        // TODO: move malloc here to malloc size of variant instead

        auto block = llvm::BasicBlock::Create(*llvmContext, llvm::Twine{ "constructor" } + llvm::Twine{ constructorBlocks.size() }, llvmFunction);
        llvmBuilder.SetInsertPoint(block);

        auto variantType = constructor.llvmType_;

        const size_t fieldCount = constructor.fields.size();
        for (size_t fieldIndex = 0; fieldIndex < fieldCount; ++fieldIndex)
        {
            std::array<llvm::Value*, 2> idxList =
            {
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(*llvmContext), 0),
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(*llvmContext), fieldIndex + 1)
            };
            auto fieldPtr = llvmBuilder.CreateGEP(variantType, srcValue, idxList, "");
            auto dstPtr = llvmBuilder.CreateGEP(variantType, dstValue, idxList, "");

            auto fieldType = constructor.fields[fieldIndex];
            if (fieldType->isStructType())
            {
                auto copyFunction = getCopyFunction(fieldType);
                std::array<llvm::Value*, 1> arguments{ fieldPtr };
                auto fieldDstValue = llvmBuilder.CreateCall(copyFunction, arguments, "");
                llvmBuilder.CreateStore(fieldDstValue, dstPtr);
            }
            else
            {
                auto fieldValue = llvmBuilder.CreateLoad(fieldType->llvmType(), fieldPtr);
                llvmBuilder.CreateStore(fieldValue, dstPtr);
            }
        }

        llvmBuilder.CreateBr(retBB);

        constructorBlocks.push_back(block);
    }

    // switch over constructors
    auto failBB = llvm::BasicBlock::Create(*llvmContext, "invalidVariantType", llvmFunction);
    llvmBuilder.SetInsertPoint(failBB);
    // TODO: call abort?
    llvmBuilder.CreateUnreachable();

    llvmBuilder.SetInsertPoint(entryBB);
    auto numBits = typeValue->getType()->getIntegerBitWidth();
    auto enumIntType = llvm::IntegerType::get(llvmBuilder.getContext(), numBits);
    auto switchInst = llvmBuilder.CreateSwitch(typeValue, failBB, constructorBlocks.size());
    for (size_t index = 0; index < constructorBlocks.size(); ++index)
    {
        switchInst->addCase(llvm::ConstantInt::get(enumIntType, index), constructorBlocks[index]);
    }

    llvmBuilder.SetInsertPoint(retBB);
    llvmBuilder.CreateRet(dstValue);

    return llvmFunction;
}

llvm::Function* CodeGenerator::generateDeleteFunctionBody(type::TypeInstPtr type)
{
    return nullptr;
}

} // namespace codegen
} // namespace hpfp
