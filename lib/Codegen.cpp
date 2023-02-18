
#include "Codegen.h"

#include "Common/Algorithm.h"
#include "Error.h"
#include "GlobalContext.h"
#include "Log.h"
#include "Module.h"
#include "String/StringConstants.h"

#pragma warning(push, 0)
// C4996 use of function, class member, variable, or typedef that's marked deprecated
#pragma warning(disable : 4996)

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/FormatVariadic.h>

#pragma warning(pop)

#include <array>
#include <stack>


namespace llfp::codegen
{

const Value ExpCodeGenerator::EmptyValue{ nullptr, nullptr };

CodeGenerator::CodeGenerator(
    Driver*            driver_,
    GlobalContext*     globalContext_,
    SourceModule*      sourceModule_,
    llvm::LLVMContext* llvmContext_,
    llvm::Module*      llvmModule_)
    : driver{ driver_ },
      globalContext{ globalContext_ },
      sourceModule{ sourceModule_ },
      llvmContext{ llvmContext_ },
      llvmBuilder(*llvmContext),
      llvmModule{ llvmModule_ },
      typeContext(*llvmContext, sourceModule, globalContext_)
{
}

bool CodeGenerator::generateFunction(const ast::Function* ast)
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

    for (auto& param : ast->parameters)
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

bool CodeGenerator::generateFunction(const ast::Function* ast, std::vector<const type::TypeInstance*> types)
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
            const auto  funType       = funAnnotation.getFun(funName);
            typeAnnotation.add(funName, funType);
        }

        // standard module may not have implementation
        if (ast->functionBody != nullptr)
        {
            typeContext.check(typeAnnotation, types[0], typeAnnotation.get(ast->functionBody.get()));
        }

        size_t i = 1;
        for (auto& arg : ast->parameters)
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
        llvm::Type*              returnType = nullptr;
        std::vector<llvm::Type*> parameterTypes;

        const bool ptrReturn = !(types.front()->isBasicType() || types.front()->isRefType());

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
            if ((*typeIt)->isBasicType())
            {
                parameterTypes.push_back((*typeIt)->llvmType());
            }
            else
            {
                parameterTypes.push_back(llvm::PointerType::get((*typeIt)->llvmType(), 0));
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
        for (auto& arg : argRange)
        {
            arg.setName(ast->parameters[i++]->identifier);
        }

        Function fun{ ast, std::move(typeAnnotation), llvmFunction, std::move(types) };
        funIt = functions.insert(std::make_pair(std::move(name), std::move(fun))).first;
    }
    return &funIt->second;
}

bool CodeGenerator::generateFunctionBody(Function* function)
{
    auto  llvmFunction = function->llvm;
    auto  ast          = function->ast;
    auto& types        = function->types;

    llvmFunction->addFnAttr(llvm::Attribute::get(*llvmContext, "frame-pointer", "all"));

    auto bb = llvm::BasicBlock::Create(*llvmContext, "entry", llvmFunction);
    llvmBuilder.SetInsertPoint(bb);

    // if return type is user type it is returned in first argument
    size_t argOffset = types[0]->isBasicType() ? 0 : 1;

    std::map<std::string, Value> namedValues;
    for (size_t i = 0; i < ast->parameters.size(); ++i)
    {
        auto& param = ast->parameters[i];
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

        llvm::Value* llvmArg           = (llvmFunction->arg_begin() + argOffset + i);
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
    expGenerator.generateCleanup();

    if (expGenerator.getResult() != nullptr)
    {
        if (types[0]->isBasicType() || types[0]->isRefType())
        {
            llvmBuilder.CreateRet(expGenerator.getResult());
        }
        else
        {
            auto load  = llvmBuilder.CreateLoad(types[0]->llvmType(), expGenerator.getResult());
            auto store = llvmBuilder.CreateStore(load, llvmFunction->arg_begin());
            store->setAlignment(llvm::Align(8));
            llvmBuilder.CreateRetVoid();
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
    auto                returnType   = llvm::Type::getInt32Ty(*llvmContext);
    llvm::FunctionType* functionType = llvm::FunctionType::get(returnType, parameterTypes, false);
    auto                llvmFunction = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, "_DllMainCRTStartup", llvmModule);
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

std::stack<size_t> findTypeVariableIndex(const std::string& typeVar, const ast::TypeIdentifier& type)
{
    for (auto& paramIt : llvm::enumerate(type.parameters))
    {
        if (typeVarEqual(typeVar, paramIt.value()))
        {
            std::stack<size_t> typeVarIndex;
            typeVarIndex.push(paramIt.index());
            return typeVarIndex;
        }

        {
            auto typeVarIndex = findTypeVariableIndex(typeVar, paramIt.value());
            if (!typeVarIndex.empty())
            {
                typeVarIndex.push(paramIt.index());
                return typeVarIndex;
            }
        }
    }
    return {};
}

type::Identifier getTypeByIndex(std::stack<size_t> index, const type::TypeInstance& type)
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

    constexpr size_t notFound          = static_cast<size_t>(-1);
    size_t           first             = notFound;
    auto             typeVariableIndex = findTypeVariableIndex(typeVar, funDecl->type);
    if (typeVariableIndex.empty() && !typeVarEqual(typeVar, funDecl->type))
    {
        for (auto paramIt : llvm::enumerate(funDecl->parameters))
        {
            auto& paramTypeId = paramIt.value()->type;
            typeVariableIndex = findTypeVariableIndex(typeVar, paramTypeId);
            if (!typeVariableIndex.empty() || typeVarEqual(typeVar, paramTypeId))
            {
                first = paramIt.index() + 1;
                break;
            }
        }
    }
    else
    {
        first = 0;
    }

    if (first == notFound)
    {
        return {};
    }
    else
    {
        // TODO: if this type is not concrete we should try and find another index
        return getTypeByIndex(std::move(typeVariableIndex), *types[first]);
    }
}

} // namespace

Function* CodeGenerator::getFunction(const GlobalIdentifier& identifier, std::vector<type::TypeInstPtr> types)
{
    auto ast     = sourceModule->lookupFunction(identifier);
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
        throw Error(std::string{ "undefined function \"" } + identifier.str() + '"');
    }

    auto proto = generatePrototype(ast.importedModule, ast.function, std::move(types));
    if (proto != nullptr)
    {
        driver->push({ ast, &proto->types });
    }
    return proto;
}

ExpCodeGenerator::ExpCodeGenerator(type::TypeInstPtr type_, CodeGenerator* generator_, std::map<std::string, Value> parameters_, hm::TypeAnnotation* typeAnnotation_)
    : parent{ nullptr },
      generator{ generator_ },
      expectedType{ type_ },
      values{ std::move(parameters_) },
      result{ nullptr },
      typeAnnotation{ typeAnnotation_ }
{
    assert(expectedType->llvmType() != nullptr);
}

ExpCodeGenerator::ExpCodeGenerator(type::TypeInstPtr type_, ExpCodeGenerator* parent_, std::map<std::string, Value> scope_)
    : parent{ parent_ },
      generator{ parent_->generator },
      expectedType{ type_ },
      values{ std::move(scope_) },
      result{ nullptr },
      typeAnnotation{ parent_->typeAnnotation }
{
    assert(expectedType->llvmType() != nullptr);
}

// Dont retain, move all temp to parent scope
llvm::Value* ExpCodeGenerator::generateUnscoped(ast::Exp& exp, type::TypeInstPtr type, ExpCodeGenerator* parent, std::map<std::string, Value> scope)
{
    if (type == nullptr) // or not?
    {
        return nullptr;
    }
    ExpCodeGenerator generator(type, parent, std::move(scope));
    exp.accept(&generator);
    parent->temporaries.insert(parent->temporaries.end(), generator.temporaries.begin(), generator.temporaries.end());
    generator.temporaries.clear();
    return generator.result;
}

// Retain result and add to parent scope, and release all temporaries
llvm::Value* ExpCodeGenerator::generateScoped(ast::Exp& exp, type::TypeInstPtr type, ExpCodeGenerator* parent, std::map<std::string, Value> scope)
{
    if (type == nullptr) // or not?
    {
        return nullptr;
    }
    ExpCodeGenerator generator(type, parent, std::move(scope));
    exp.accept(&generator);

    auto  result  = generator.result;
    auto& builder = generator.llvmBuilder();

    if (result == nullptr)
    {
        generator.temporaries.clear();
        return nullptr;
    }

    if (!std::any_of(generator.temporaries.begin(), generator.temporaries.end(), [result](const Value& v) { return v.llvmValue == result; }))
    {
        if (generator.expectedType->isRefType())
        {
            auto int32Type    = llvm::Type::getInt32Ty(generator.llvmContext());
            auto oneV         = llvm::ConstantInt::get(int32Type, 1);
            auto counterValue = builder.CreateLoad(int32Type, result, "refCount");
            auto newValue     = builder.CreateAdd(counterValue, oneV, "scopeIncCount");
            builder.CreateStore(newValue, result);
        }
    }

    generator.generateCleanup();

    parent->temporaries.push_back({ generator.expectedType, generator.result });
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

void ExpCodeGenerator::visit(ast::LetExp& exp)
{
    for (auto& var : exp.letStatments)
    {
        if (var->parameters.size() != 0)
        {
            Log(exp.location, "let function definitions not implemented");
            return;
        }

        type::TypeInstPtr varType = var->type.identifier.name.empty()
                                        ? getTypeContext()->constructTypeUsingAnnotationStuff(*typeAnnotation, *var)
                                        : getTypeContext()->getTypeFromAst(var->type);

        auto value = ExpCodeGenerator::generateScoped(*var->functionBody, varType, this);

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

    result = ExpCodeGenerator::generateUnscoped(*exp.exp, expectedType, this);
}

void ExpCodeGenerator::visit(ast::IfExp& exp)
{
    auto condV = ExpCodeGenerator::generateScoped(*exp.condition, getTypeContext()->getBool(), this);
    if (condV == nullptr)
    {
        return;
    }

    auto& builder  = llvmBuilder();
    auto  function = builder.GetInsertBlock()->getParent();

    auto thenBB  = llvm::BasicBlock::Create(llvmContext(), "then", function);
    auto elseBB  = llvm::BasicBlock::Create(llvmContext(), "else");
    auto mergeBB = llvm::BasicBlock::Create(llvmContext(), "ifcont");

    builder.CreateCondBr(condV, thenBB, elseBB);

    builder.SetInsertPoint(thenBB);

    auto thenV = ExpCodeGenerator::generateScoped(*exp.thenExp, expectedType, this);
    if (thenV == nullptr)
    {
        return;
    }
    builder.CreateBr(mergeBB);
    // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
    thenBB = builder.GetInsertBlock();

    function->getBasicBlockList().push_back(elseBB);
    builder.SetInsertPoint(elseBB);

    auto elseV = ExpCodeGenerator::generateScoped(*exp.elseExp, expectedType, this);
    if (elseV == nullptr)
    {
        return;
    }
    builder.CreateBr(mergeBB);
    elseBB = builder.GetInsertBlock();

    // Emit merge block.
    function->getBasicBlockList().push_back(mergeBB);
    builder.SetInsertPoint(mergeBB);
    auto type = (expectedType->isBasicType() || expectedType->isRefType()) ? expectedType->llvmType() : expectedType->llvmType()->getPointerTo();
    auto PN   = builder.CreatePHI(type, 2, "ifphi");

    PN->addIncoming(thenV, thenBB);
    PN->addIncoming(elseV, elseBB);

    // need to replace thenV and elseV with PN in temporaries
    llfp::erase_first_of(temporaries, [thenV](const Value& v) { return v.llvmValue == thenV; });
    llfp::erase_first_of(temporaries, [elseV](const Value& v) { return v.llvmValue == elseV; });
    temporaries.push_back({ expectedType, PN });

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
    Value                        value;
    llvm::BasicBlock*            nextBlock;

public:

    PatternCodeGenerator(PatternCodeGeneratorContext& context_, Value value_, llvm::BasicBlock* nextBlock_)
        : context{ context_ },
          value{ value_ },
          nextBlock{ nextBlock_ }
    {}

    void visit(ast::BoolPattern& pattern) override
    {
        auto patternValue = llvm::ConstantInt::getBool(context.irBuilder.getContext(), pattern.value);
        auto condition    = context.irBuilder.CreateCmp(llvm::CmpInst::ICMP_EQ, value.llvmValue, patternValue, "boolPattern");
        context.irBuilder.CreateCondBr(condition, nextBlock, context.failBlock);
    }

    void visit(ast::IdentifierPattern& pattern) override
    {
        context.variables.insert({ pattern.value, value });
        context.irBuilder.CreateBr(nextBlock);
    }

    void visit(ast::IntegerPattern& pattern) override
    {
        unsigned int numBits      = value.type->llvmType()->getIntegerBitWidth();
        auto         patternValue = llvm::ConstantInt::get(llvm::IntegerType::get(context.irBuilder.getContext(), numBits), pattern.value, 10);
        auto         condition    = context.irBuilder.CreateCmp(llvm::CmpInst::ICMP_EQ, value.llvmValue, patternValue, "integerPattern");
        context.irBuilder.CreateCondBr(condition, nextBlock, context.failBlock);
    }

    void visit(ast::FloatPattern& pattern) override
    {
        auto patternValue = llvm::ConstantFP::get(value.type->llvmType(), pattern.value);
        auto condition    = context.irBuilder.CreateFCmp(llvm::CmpInst::FCMP_OEQ, value.llvmValue, patternValue, "floatPattern");
        context.irBuilder.CreateCondBr(condition, nextBlock, context.failBlock);
    }

    void visit(ast::CharPattern& pattern) override
    {
        assert(pattern.value.size() == 1);
        auto patternValue = llvm::ConstantInt::get(value.llvmValue->getType(), static_cast<uint8_t>(pattern.value[0]));
        auto condition    = context.irBuilder.CreateCmp(llvm::CmpInst::ICMP_EQ, value.llvmValue, patternValue, "charPattern");
        context.irBuilder.CreateCondBr(condition, nextBlock, context.failBlock);
    }

    void visit(ast::StringPattern& pattern) override
    {
        assert(false);
    }

    void visit(ast::ConstructorPattern& pattern) override
    {
        auto&      llvmContext  = context.irBuilder.getContext();
        const auto constructors = value.type->getConstructors();
        const bool variant      = constructors.size() > 1;
        const auto count        = pattern.arguments.size();

        std::vector<llvm::BasicBlock*> blocks;
        for (size_t i = 0; i < count; ++i)
        {
            blocks.push_back(llvm::BasicBlock::Create(llvmContext, llvm::Twine{ "patternConArg" } + llvm::Twine{ i }));
        }

        auto originalBlock = context.irBuilder.GetInsertBlock();
        auto function      = originalBlock->getParent();

        const llfp::type::TypeConstructor* constructor      = nullptr;
        uint64_t                           constructorIndex = 0;
        if (variant)
        {
            for (auto& c : constructors)
            {
                if (c.ast->name == pattern.identifier.name)
                {
                    constructor = &c;
                    break;
                }
                ++constructorIndex;
            }
        }
        else
        {
            assert(pattern.identifier.name == value.type->identifier().name.name);
            constructor = &constructors.front();
        }
        assert(constructor != nullptr); // should have been type checked?

        auto              int32Type = llvm::Type::getInt32Ty(llvmContext);
        llvm::StructType* heapType  = nullptr;
        if (variant)
        {
            auto enumType = type::TypeInstanceVariant::getEnumType(llvmContext, value.type);
            heapType      = llvm::StructType::create(llvmContext, { int32Type, enumType, constructor->llvmType_ });
        }
        else
        {
            heapType = constructor->llvmType_;
        }

        auto zeroV = llvm::ConstantInt::get(int32Type, 0);
        auto oneV  = llvm::ConstantInt::get(int32Type, 1);
        auto twoV  = llvm::ConstantInt::get(int32Type, 2);

        for (auto blockIt : llvm::enumerate(blocks))
        {
            function->getBasicBlockList().push_back(blockIt.value());
            context.irBuilder.SetInsertPoint(blockIt.value());

            llvm::Value* argV    = nullptr;
            auto         indexV  = llvm::ConstantInt::get(int32Type, blockIt.index());
            auto         argPtr  = variant
                                       ? context.irBuilder.CreateGEP(heapType, value.llvmValue, { zeroV, twoV, indexV }, "patternGEP")
                                       : context.irBuilder.CreateGEP(heapType, value.llvmValue, { zeroV, indexV }, "patternGEP");
            auto         argType = constructor->fields[blockIt.index()];
            if (argType->isBasicType() || argType->isRefType())
            {
                argV = context.irBuilder.CreateLoad(argType->llvmType(), argPtr, "patternLoad");
            }
            else
            {
                argV = argPtr;
            }

            auto                 branchNextBlock = blockIt.index() == blocks.size() - 1 ? nextBlock : blocks[blockIt.index() + 1];
            PatternCodeGenerator gen{ context, { argType, argV }, branchNextBlock };
            pattern.arguments[blockIt.index()].pattern->accept(&gen);
        }

        context.irBuilder.SetInsertPoint(originalBlock);
        if (variant)
        {
            auto enumType         = type::TypeInstanceVariant::getEnumType(llvmContext, value.type);
            auto enumPtr          = context.irBuilder.CreateGEP(heapType, value.llvmValue, { zeroV, oneV }, "patternConEnumGEP");
            auto enumValue        = context.irBuilder.CreateLoad(enumType, enumPtr, "patternConEnum");
            auto patternEnumValue = llvm::ConstantInt::get(enumType, constructorIndex);
            auto condition        = context.irBuilder.CreateCmp(llvm::CmpInst::ICMP_EQ, enumValue, patternEnumValue, "patternConEnumCmp");
            context.irBuilder.CreateCondBr(condition, blocks.empty() ? nextBlock : blocks.front(), context.failBlock);
        }
        else
        {
            context.irBuilder.CreateBr(blocks.empty() ? nextBlock : blocks.front());
        }
    }
};

void ExpCodeGenerator::visit(ast::CaseExp& exp)
{
    const auto count = exp.clauses.size();
    assert(count != 0);
    auto& builder  = llvmBuilder();
    auto  function = builder.GetInsertBlock()->getParent();

    auto caseT = getTypeContext()->constructTypeUsingAnnotationStuff(*typeAnnotation, *exp.caseExp);
    auto caseV = ExpCodeGenerator::generateScoped(*exp.caseExp, caseT, this);

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
        auto                        uglyHack  = expBlocks.back(); // TODO: replace with call to abort()
        auto                        failBlock = i == count - 1 ? uglyHack : patternBlocks[i + 1];
        PatternCodeGeneratorContext context{ builder, failBlock, getTypeContext(), typeAnnotation };
        PatternCodeGenerator        gen{ context, { caseT, caseV }, expBlocks[i] };

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
        auto clauseV = ExpCodeGenerator::generateScoped(*exp.clauses[i].exp, expectedType, this, std::move(variables[i]));
        // Codegen of 'Clause' can change the current block, update ClauseBB for the PHI.
        expBlocks[i] = builder.GetInsertBlock();
        if (clauseV != nullptr)
        {
            caseValues.push_back(clauseV);
        }
        builder.CreateBr(mergeBB);
    }

    if (caseValues.size() != count)
    {
        // some clause failed
        temporaries.clear();
        return;
    }

    function->getBasicBlockList().push_back(mergeBB);
    builder.SetInsertPoint(mergeBB);
    auto PN = builder.CreatePHI(expectedType->llvmType(), static_cast<unsigned int>(count), "casephi");
    for (size_t i = 0; i < count; ++i)
    {
        PN->addIncoming(caseValues[i], expBlocks[i]);
    }

    // Replace case values with phi value in temporaries
    for (auto clauseV : caseValues)
    {
        llfp::erase_first_of(temporaries, [clauseV](const Value& v) { return v.llvmValue == clauseV; });
    }
    temporaries.push_back({ expectedType, PN });

    result = PN;
}

namespace
{

void typeError(ast::Exp& exp, type::TypeInstPtr expected, llvm::StringRef actual)
{
    Log(exp.location, "type mismatch, expected '", expected->identifier().str(), "' actual '", actual, '\'');
}

bool checkNum(ast::Exp& exp, type::TypeInstPtr type)
{
    if (!type->isNum())
    {
        typeError(exp, type, id::Num);
        return false;
    }
    return true;
}

bool checkInteger(ast::Exp& exp, type::TypeInstPtr type)
{
    if (!type->isInteger())
    {
        typeError(exp, type, id::Integer);
        return false;
    }
    return true;
}

bool checkBool(ast::Exp& exp, type::TypeInstPtr type)
{
    if (!type->isBool())
    {
        typeError(exp, type, id::Bool);
        return false;
    }
    return true;
}

} // namespace

void ExpCodeGenerator::visit(ast::BinaryExp& exp)
{
    // math
    if (exp.op == "*") // Multiplication
    {
        generateBinary(
            exp, checkNum,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFMul(v1, v2, "fmul"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateMul(v1, v2, "mul", false, false); });
    }
    else if (exp.op == "/") // Division
    {
        generateBinary(
            exp, checkNum,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFDiv(v1, v2, "fdiv"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateSDiv(v1, v2, "sdiv", false); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateUDiv(v1, v2, "udiv", false); });
    }
    else if (exp.op == "%") // Remainder
    {
        generateBinary(
            exp, checkNum,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFRem(v1, v2, "frem"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateSRem(v1, v2, "srem"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateURem(v1, v2, "urem"); });
    }
    else if (exp.op == "+") // Addition
    {
        generateBinary(
            exp, checkNum,
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFAdd(v1, v2, "fadd"); },
            [this](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateAdd(v1, v2, "add"); });
    }
    else if (exp.op == "-") // Subtraction
    {
        generateBinary(
            exp, checkNum,
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
    else if (exp.op == ">") { generateCompare(llvm::CmpInst::Predicate::ICMP_UGT, exp); }  // Greater than
    else if (exp.op == ">=") { generateCompare(llvm::CmpInst::Predicate::ICMP_UGE, exp); } // Greater or equal
    else if (exp.op == "<") { generateCompare(llvm::CmpInst::Predicate::ICMP_ULT, exp); }  // Less than
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
    case llvm::CmpInst::ICMP_EQ: return llvm::CmpInst::FCMP_OEQ;
    case llvm::CmpInst::ICMP_NE: return llvm::CmpInst::FCMP_ONE;
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
    case llvm::CmpInst::ICMP_EQ: return llvm::CmpInst::ICMP_EQ;
    case llvm::CmpInst::ICMP_NE: return llvm::CmpInst::ICMP_NE;
    case llvm::CmpInst::ICMP_UGT: return llvm::CmpInst::ICMP_SGT;
    case llvm::CmpInst::ICMP_UGE: return llvm::CmpInst::ICMP_SGE;
    case llvm::CmpInst::ICMP_ULT: return llvm::CmpInst::ICMP_SLT;
    case llvm::CmpInst::ICMP_ULE: return llvm::CmpInst::ICMP_SLE;
    default: return llvm::CmpInst::BAD_ICMP_PREDICATE;
    }
}

} // namespace

void ExpCodeGenerator::generateCompare(llvm::CmpInst::Predicate predicate, ast::BinaryExp& exp)
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

void ExpCodeGenerator::visit(ast::UnaryExp& exp)
{
    if (exp.op == "-")
    {
        if (!expectedType->isSigned())
        {
            typeError(exp, expectedType, id::Signed);
        }
        else
        {
            auto value = ExpCodeGenerator::generateUnscoped(*exp.operand, expectedType, this);
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
            auto value = ExpCodeGenerator::generateUnscoped(*exp.operand, expectedType, this);
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
            auto value = ExpCodeGenerator::generateUnscoped(*exp.operand, expectedType, this);
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

void ExpCodeGenerator::visit(ast::LiteralExp& exp)
{
    llvm::Type* type = expectedType->llvmType();

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

void ExpCodeGenerator::visit(ast::CallExp& exp)
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

            // Returning user type by value on the stack, function will return void
            llvm::Value* retValue = nullptr;
            if (!(expectedType->isBasicType() || expectedType->isRefType()))
            {
                retValue = llvmBuilder().CreateAlloca(expectedType->llvmType(), nullptr, "retVal");
                arguments.push_back(retValue);
            }

            for (size_t i = 0; i < exp.arguments.size(); ++i)
            {
                auto argValue = generateUnscoped(*exp.arguments[i], function->types[i + 1], this);
                if (argValue == nullptr)
                {
                    return;
                }
                arguments.push_back(argValue);
            }

            result = llvmBuilder().CreateCall(function->llvm, arguments);
            if (retValue != nullptr)
            {
                result = retValue;
            }
            temporaries.push_back({ expectedType, result });
        }
    }
    catch (const Error& e)
    {
        Log(exp.location, e.what());
    }
}

void ExpCodeGenerator::visit(ast::VariableExp& exp)
{
    if (exp.identifier.moduleName.empty())
    {
        auto x = getNamedValue(exp.identifier.name);
        if (x.llvmValue != nullptr)
        {
            // check type
            if (x.type != expectedType)
            {
                typeError(exp, expectedType, x.type->identifier().str());
                return;
            }
            result = x.llvmValue;
            return;
        }
    }

    std::vector<type::TypeInstPtr> types;
    types.push_back(expectedType);

    auto y = getFunction(exp.identifier, std::move(types));
    if (y != nullptr)
    {
        auto& function = *y;
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

void ExpCodeGenerator::visit(ast::FieldExp& exp)
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
        auto fieldType = lhsType->getFields()[index];
        if (fieldType != expectedType)
        {
            Log(exp.location, "Failed to unify types ", fieldType->identifier().str(), " and ", expectedType->identifier().str());
            return;
        }
        // I think we need both since check might do subs
        // 22-01-31 except constructTypeUsingAnnotationStuff already checks?
        getTypeContext()->check(*typeAnnotation, expectedType, typeAnnotation->get(&exp));

        auto structValue = ExpCodeGenerator::generateUnscoped(*exp.lhs, lhsType, this);
        if (structValue == nullptr)
        {
            return;
        }

        auto ptrValue = llvmBuilder().CreateGEP(lhsType->llvmType(), structValue, { i32V(0), i32V(index) }, "fieldGep");
        if (expectedType->isBasicType() || expectedType->isRefType())
        {
            result = llvmBuilder().CreateLoad(expectedType->llvmType(), ptrValue, "fieldLoad");
        }
        else
        {
            result = ptrValue;
        }
    }
    catch (const Error& error)
    {
        Log(exp.location, error.what());
    }
}

void ExpCodeGenerator::visit(ast::ConstructorExp& exp)
{
    try
    {
        getTypeContext()->check(*typeAnnotation, expectedType, typeAnnotation->get(&exp));

        const auto size = expectedType->getFields(exp.identifier.name).size();
        if (size != exp.arguments.size())
        {
            Log(exp.location, "incorrect number of arguments");
            return;
        }

        bool               fail      = false;
        llvm::Value*       value     = nullptr;
        llvm::Type*        valueType = nullptr;
        llvm::ConstantInt* enumValue = nullptr;
        if (expectedType->isRefType())
        {
            assert(expectedType->getConstructors().size() > 1); // only variant types for now
            uint64_t constructorIndex = 0;
            for (auto& constructor : expectedType->getConstructors())
            {
                if (constructor.ast->name == exp.identifier.name)
                {
                    // heapType {refCount, variantEnum, valueType}
                    auto int32Type = llvm::Type::getInt32Ty(llvmContext());
                    auto enumType  = type::TypeInstanceVariant::getEnumType(llvmContext(), expectedType);
                    value          = llvm::UndefValue::get(constructor.llvmType_);
                    valueType      = llvm::StructType::create({ int32Type, enumType, constructor.llvmType_ });
                    enumValue      = llvm::ConstantInt::get(enumType, constructorIndex);
                    break;
                }
                ++constructorIndex;
            }
            assert(value != nullptr);
        }
        else
        {
            value     = llvm::UndefValue::get(expectedType->llvmType());
            valueType = expectedType->llvmType();
        }

        for (auto fieldIt : llvm::enumerate(expectedType->getFields(exp.identifier.name)))
        {
            auto& arg = exp.arguments[fieldIt.index()];

            // at the moment name is only used to check correctness
            if (!arg.name.empty())
            {
                auto index = expectedType->getFieldIndex(exp.identifier.name, arg.name);
                if (index != static_cast<unsigned int>(fieldIt.index()))
                {
                    Log(arg.location, index == type::TypeInstance::InvalidIndex
                                          ? "unknown field name"
                                          : "incorrect field position");
                    fail = true;
                }
            }

            // scoped but remove value from temporary since it's owned by the result
            auto argValue = ExpCodeGenerator::generateScoped(*arg.exp, fieldIt.value(), this);
            if (argValue != nullptr)
            {
                assert(temporaries.back().llvmValue == argValue);
                temporaries.pop_back();

                if (!(fieldIt.value()->isBasicType() || fieldIt.value()->isRefType()))
                {
                    argValue = llvmBuilder().CreateLoad(fieldIt.value()->llvmType(), argValue, "conLoadArg");
                }

                auto index = static_cast<unsigned int>(fieldIt.index());
                value      = llvmBuilder().CreateInsertValue(value, argValue, { index }, "conInsertArg");
            }
            else
            {
                fail = true;
            }
        }

        if (!fail)
        {
            llvm::Value* ptrValue = nullptr;
            if (expectedType->isRefType())
            {
                llvm::Function* mallocFun = nullptr;
                auto            mallocVar = llvmModule().getNamedValue("malloc");
                if (mallocVar == nullptr)
                {
                    auto ptrType    = llvm::Type::getInt8PtrTy(llvmContext());
                    auto sizeType   = llvm::Type::getInt64Ty(llvmContext());
                    auto mallocType = llvm::FunctionType::get(ptrType, sizeType, false);
                    mallocFun       = llvm::Function::Create(mallocType, llvm::Function::ExternalLinkage, "malloc", llvmModule());
                    mallocFun->setReturnDoesNotAlias();
                }
                else
                {
                    mallocFun = llvm::dyn_cast<llvm::Function>(mallocVar);
                }

                ptrValue        = llvmBuilder().CreateCall(mallocFun, llvm::ConstantExpr::getSizeOf(valueType), "conMalloc");
                auto zeroV      = i32V(0);
                auto oneV       = i32V(1);
                auto counterPtr = llvmBuilder().CreateGEP(valueType, ptrValue, { zeroV, zeroV }, "conMallocCounterGEP");
                auto enumPtr    = llvmBuilder().CreateGEP(valueType, ptrValue, { zeroV, oneV }, "conMallocEnumGEP");
                auto dataPtr    = llvmBuilder().CreateGEP(valueType, ptrValue, { zeroV, i32V(2) }, "conMallocDataGEP");
                llvmBuilder().CreateStore(oneV, counterPtr);
                llvmBuilder().CreateStore(enumValue, enumPtr);
                llvmBuilder().CreateStore(value, dataPtr);
            }
            else
            {
                ptrValue = llvmBuilder().CreateAlloca(valueType, nullptr, "conAlloca");
                llvmBuilder().CreateStore(value, ptrValue);
            }

            temporaries.push_back({ expectedType, ptrValue });
            result = ptrValue;
        }
    }
    catch (const Error& error)
    {
        Log(exp.location, error.what());
    }
}

const Value& ExpCodeGenerator::getNamedValue(const std::string& name)
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

void ExpCodeGenerator::generateCleanup()
{
    for (auto temporary : temporaries)
    {
        if (temporary.llvmValue != result)
        {
            if (temporary.type->isRefType())
            {
                auto fun = generator->getReleaseFunction(temporary.type);
                llvmBuilder().CreateCall(fun, { temporary.llvmValue });
            }
            else if (!temporary.type->isBasicType())
            {
                auto fun = generator->getDeleteFunction(temporary.type);
                llvmBuilder().CreateCall(fun, { temporary.llvmValue });
            }
        }
    }
    temporaries.clear();
}

llvm::Function* CodeGenerator::getReleaseFunction(type::TypeInstPtr type)
{
    auto it = releaseFunctions.find(type);
    if (it != releaseFunctions.end())
    {
        return it->second;
    }

    driver->push(type);

    auto name = sourceModule->getMangledName("release", type);

    llvm::Type*                returnType     = llvm::Type::getVoidTy(*llvmContext);
    std::array<llvm::Type*, 1> parameterTypes = { llvm::PointerType::get(*llvmContext, 0) };

    auto functionType = llvm::FunctionType::get(returnType, parameterTypes, false);
    auto llvmFunction = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, llvmModule);
    releaseFunctions.insert({ type, llvmFunction });
    return llvmFunction;
}

llvm::Function* CodeGenerator::getDeleteFunction(type::TypeInstPtr type)
{
    auto it = deleteFunctions.find(type);
    if (it != deleteFunctions.end())
    {
        return it->second;
    }

    driver->push(type);

    auto name = sourceModule->getMangledName("delete", type);

    llvm::Type*                returnType     = llvm::Type::getVoidTy(*llvmContext);
    std::array<llvm::Type*, 1> parameterTypes = { llvm::PointerType::get(*llvmContext, 0) };

    auto functionType = llvm::FunctionType::get(returnType, parameterTypes, false);
    auto llvmFunction = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, llvmModule);
    deleteFunctions.insert({ type, llvmFunction });
    return llvmFunction;
}

namespace
{

void generateVariantSwitch(llvm::IRBuilder<>& llvmBuilder, llvm::Value* value, type::TypeInstPtr type, const std::vector<llvm::BasicBlock*>& variantBlocks)
{
    auto& llvmContext  = llvmBuilder.getContext();
    auto  BB           = llvmBuilder.GetInsertBlock();
    auto  llvmFunction = BB->getParent();

    auto failBB = llvm::BasicBlock::Create(llvmContext, "invalidVariantType", llvmFunction);
    llvmBuilder.SetInsertPoint(failBB);
    // TODO: call abort?
    llvmBuilder.CreateUnreachable();

    llvmBuilder.SetInsertPoint(BB);

    auto int32Type = llvm::Type::getInt32Ty(llvmContext);
    auto enumType  = type::TypeInstanceVariant::getEnumType(llvmContext, type);
    auto zeroV     = llvm::ConstantInt::get(int32Type, 0);
    auto oneV      = llvm::ConstantInt::get(int32Type, 1);

    assert(!type->getConstructors().empty());
    assert(!type->getConstructors().front().fields.empty());

    auto headerType          = llvm::StructType::create({ int32Type, enumType });
    auto variantTypeValuePtr = llvmBuilder.CreateGEP(headerType, value, { zeroV, oneV }, "typePtr");
    auto variantTypeValue    = llvmBuilder.CreateLoad(enumType, variantTypeValuePtr, "type");

    auto switchInst = llvmBuilder.CreateSwitch(variantTypeValue, failBB, static_cast<unsigned int>(variantBlocks.size()));
    for (auto it : llvm::enumerate(variantBlocks))
    {
        switchInst->addCase(llvm::ConstantInt::get(enumType, it.index()), it.value());
    }
}

} // namespace

/*

template<class T>
struct RefT {
    int count;
    T value;
};

void release(RefT* ptr) {
    --(ptr->count);
    if (ptr->count == 0) {
        release(&ptr->value);
        free(ptr);
    }
}

struct S2 {
    RefT* v;
}

struct S {
    float x;
    RefT* v;
    S2 s2;
}

void release(S* s) {
    release(s->v);
    release(&s->s2);
}

*/

bool CodeGenerator::generateReleaseFunctionBody(type::TypeInstPtr type)
{
    assert(type->isRefType());
    auto llvmFunction = getReleaseFunction(type);
    if (!llvmFunction->empty())
    {
        return true;
    }
    auto inValue = llvmFunction->arg_begin();

    auto entryBB = llvm::BasicBlock::Create(*llvmContext, "entry", llvmFunction);
    llvmBuilder.SetInsertPoint(entryBB);

    auto int32Ty = llvm::Type::getInt32Ty(*llvmContext);

    auto oneValue     = llvm::ConstantInt::get(int32Ty, 1);
    auto counterValue = llvmBuilder.CreateLoad(int32Ty, inValue);
    auto compareValue = llvmBuilder.CreateICmpSLE(counterValue, oneValue);

    auto deleteBranch = llvm::BasicBlock::Create(*llvmContext, "delete", llvmFunction);
    auto decBranch    = llvm::BasicBlock::Create(*llvmContext, "dec", llvmFunction);

    llvmBuilder.CreateCondBr(compareValue, deleteBranch, decBranch);

    llvmBuilder.SetInsertPoint(deleteBranch);
    auto deleteFunction = getDeleteFunction(type);
    llvmBuilder.CreateCall(deleteFunction, inValue);
    llvmBuilder.CreateRetVoid();

    llvmBuilder.SetInsertPoint(decBranch);
    auto decValue = llvmBuilder.CreateSub(counterValue, oneValue);
    llvmBuilder.CreateStore(decValue, inValue);
    llvmBuilder.CreateRetVoid();

    return true;
}

bool CodeGenerator::generateDeleteFunctionBody(type::TypeInstPtr type)
{
    auto llvmFunction = getDeleteFunction(type);
    if (!llvmFunction->empty())
    {
        return true;
    }
    if (type->getConstructors().size() == 1)
    {
        return generateDeleteFunctionBodyAggregate(llvmFunction, type);
    }
    else
    {
        return generateDeleteFunctionBodyVariant(llvmFunction, type);
    }
}

void CodeGenerator::generateDeleteConstructorBlock(llvm::Value* argValue, const llfp::type::TypeConstructor& constructor)
{
    auto variantType = constructor.llvmType_;

    const size_t fieldCount = constructor.fields.size();
    for (size_t fieldIndex = 0; fieldIndex < fieldCount; ++fieldIndex)
    {
        auto fieldType = constructor.fields[fieldIndex];
        if (fieldType->isRefType() || fieldType->containsRefTypes())
        {
            auto fieldPtr = llvmBuilder.CreateGEP(variantType, argValue, { i32V(0), i32V(fieldIndex) }, "deleteFieldGEP");
            if (fieldType->isRefType())
            {
                // call release
                auto fieldValue      = llvmBuilder.CreateLoad(fieldType->llvmType(), fieldPtr);
                auto releaseFunction = getReleaseFunction(fieldType);
                llvmBuilder.CreateCall(releaseFunction, fieldValue);
            }
            else if (fieldType->containsRefTypes())
            {
                // call delete
                auto deleteFunction = getDeleteFunction(fieldType);
                llvmBuilder.CreateCall(deleteFunction, fieldPtr);
            }
        }
    }
}

bool CodeGenerator::generateDeleteFunctionBodyAggregate(llvm::Function* llvmFunction, type::TypeInstPtr type)
{
    auto argValue = llvmFunction->arg_begin();

    auto block = llvm::BasicBlock::Create(*llvmContext, "entry", llvmFunction);

    llvmBuilder.SetInsertPoint(block);
    auto  c = type->getConstructors();
    auto* f = &c.front();
    generateDeleteConstructorBlock(argValue, *f);

    // call free and return
    if (type->isRefType())
    {
        // llvm::CallInst::CreateFree(argValue, llvmBuilder.GetInsertBlock());
        assert(false);
    }
    llvmBuilder.CreateRetVoid();

    return true;
}

bool CodeGenerator::generateDeleteFunctionBodyVariant(llvm::Function* llvmFunction, type::TypeInstPtr type)
{
    auto argValue = llvmFunction->arg_begin();

    auto entryBB = llvm::BasicBlock::Create(*llvmContext, "entry", llvmFunction);
    auto retBB   = llvm::BasicBlock::Create(*llvmContext, "return", llvmFunction);

    std::vector<llvm::BasicBlock*> constructorBlocks;
    for (auto& constructor : type->getConstructors())
    {
        auto block = llvm::BasicBlock::Create(*llvmContext, llvm::Twine{ "constructor" } + llvm::Twine{ constructorBlocks.size() }, llvmFunction);
        llvmBuilder.SetInsertPoint(block);
        auto int32Type = llvm::Type::getInt32Ty(*llvmContext);
        auto enumType  = type::TypeInstanceVariant::getEnumType(*llvmContext, type);
        auto heapType  = llvm::StructType::create({ int32Type, enumType, constructor.llvmType_ });
        auto valuePtr  = llvmBuilder.CreateGEP(heapType, argValue, { i32V(0), i32V(2) }, "deleteDataGEP");
        generateDeleteConstructorBlock(valuePtr, constructor);
        llvmBuilder.CreateBr(retBB);
        constructorBlocks.push_back(block);
    }

    llvmBuilder.SetInsertPoint(entryBB);
    generateVariantSwitch(llvmBuilder, argValue, type, constructorBlocks);

    // call free and return
    llvmBuilder.SetInsertPoint(retBB);
    if (type->isRefType())
    {
        llvm::Function* freeFun = nullptr;
        auto            freeVar = llvmModule->getNamedValue("free");
        if (freeVar == nullptr)
        {
            auto voidType = llvm::Type::getVoidTy(*llvmContext);
            auto ptrType  = llvm::Type::getInt8PtrTy(*llvmContext);
            auto freeType = llvm::FunctionType::get(voidType, ptrType, false);
            freeFun       = llvm::Function::Create(freeType, llvm::Function::ExternalLinkage, "free", llvmModule);
        }
        else
        {
            freeFun = llvm::dyn_cast<llvm::Function>(freeVar);
        }
        llvmBuilder.CreateCall(freeFun, { argValue });
    }
    llvmBuilder.CreateRetVoid();

    return true;
}

llvm::Value* CodeGenerator::i32V(uint64_t i)
{
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(*llvmContext), i);
}

} // namespace llfp::codegen
