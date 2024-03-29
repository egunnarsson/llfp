
#include "Codegen.h"

#include "Common/Algorithm.h"
#include "Error.h"
#include "GlobalContext.h"
#include "Log.h"
#include "Module.h"
#include "NameMangling.h"
#include "String/StringConstants.h"

#pragma warning(push, 0)
// C4996 use of function, class member, variable, or typedef that's marked deprecated
#pragma warning(disable : 4996)

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>

#pragma warning(pop)

#include <array>
#include <charconv>
#include <stack>


namespace
{

llvm::Argument* getArg(llvm::Function* f, size_t i)
{
    assert(i < f->arg_size() && "getArg() out of range!");
    return f->arg_begin() + i;
}

llvm::SmallString<64> name(const llfp::ast::Node& exp, llvm::StringLiteral name)
{
    llvm::SmallString<64> result;
    std::array<char, 32>  tmp;
    result += name;
    result += '.';
    auto conv = std::to_chars(&tmp.front(), &tmp.back(), exp.location.Line);
    assert(conv.ec == std::errc());
    result.append(&tmp.front(), conv.ptr);
    result += '.';
    conv = std::to_chars(&tmp.front(), &tmp.back(), exp.location.Column);
    assert(conv.ec == std::errc());
    result.append(&tmp.front(), conv.ptr);
    return result;
}

} // namespace

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

namespace
{

// TODO implement, already done in FunTypePtr inferType(...)?
hm::FunTypePtr replaceClassTypeVariableWithTypeVar(const ast::Class* astClass, const hm::FunTypePtr& funType)
{
    std::map<std::string, hm::TypeConstantPtr> typeConstants;
    return funType->copyFun(typeConstants);
}

void injectFunctionTypes(const ast::Function* fun, hm::TypeAnnotation& typeAnnotation, SourceModule& sourceModule, type::TypeContext& typeContext)
{
    for (const auto& [funName, annotatedFun] : typeAnnotation.getFunctions())
    {
        auto           funAst = sourceModule.lookupFunction(GlobalIdentifier::split(funName)); // module? not sourceModule?
        hm::FunTypePtr funType;
        if (funAst.empty())
        {
            auto funDecl = sourceModule.lookupFunctionDecl(GlobalIdentifier::split(funName));
            assert(!funDecl.empty());
            auto& funDeclType = typeContext.getAnnotation(&sourceModule, funDecl.class_, funDecl.function);
            funType           = replaceClassTypeVariableWithTypeVar(funDecl.class_, funDeclType);
        }
        else if (funAst.function != fun)
        {
            const auto& funAnnotation = typeContext.getAnnotation(funAst.importedModule, funAst.function);
            funType                   = funAnnotation.getFun(funName);
        }
        if (funType != nullptr)
        {
            if (annotatedFun.typePtr->types.size() != funType->types.size())
            {
                if (annotatedFun.callExp != nullptr)
                {
                    throw ErrorLocation{ annotatedFun.callExp->location, "incorrect number of arguments" };
                }
                else
                {
                    throw Error{ "incorrect number of arguments" };
                }
            }
            else
            {
                typeAnnotation.addConstraint(funName, funType);
            }
        }
    }
}

void injectFieldTypes(hm::TypeAnnotation& typeAnnotation, type::TypeContext& typeContext)
{
    bool didApplyUpdate = false;
    do {
        didApplyUpdate = false;

        for (auto& fieldExp : typeAnnotation.getfieldExpressions())
        {
            try
            {
                class : public hm::TypeVisitor
                {
                public:

                    bool                 didApplyUpdate  = false;
                    hm::TypeAnnotation*  typeAnnotation_ = nullptr;
                    type::TypeContext*   typeContext_    = nullptr;
                    const ast::FieldExp* fieldExp_       = nullptr;

                    void injectField(const DataAst& ast)
                    {
                        if (ast.data->constructors.size() != 1)
                        {
                            throw Error("injectFields on type with multiple constructor");
                        }

                        auto& constructor = ast.data->constructors.front();
                        for (auto& field : constructor.fields)
                        {
                            if (field.name == fieldExp_->fieldIdentifier)
                            {
                                auto       fieldInstanceType = typeContext_->getTypeFromAst(field.type);
                                const bool newConstraints    = typeAnnotation_->addConstraint(typeAnnotation_->get(fieldExp_), fieldInstanceType->getType());
                                didApplyUpdate               = didApplyUpdate || newConstraints;
                                break;
                            }
                        }
                    }

                    void visit(hm::UnboundTypeVar&) override {}
                    void visit(hm::BoundTypeVar& t) override
                    {
                        if (!t.parameters_.empty())
                        {
                            throw Error("not implemented");
                        }
                        injectField(t.ast_);
                    }
                    void visit(hm::TypeConstant& t) override
                    {
                        if (!t.parameters_.empty())
                        {
                            throw Error("not implemented");
                        }
                        injectField(t.ast_);
                    }
                    void visit(hm::FunctionType&) override {}
                } visitor;
                visitor.typeAnnotation_ = &typeAnnotation;
                visitor.typeContext_    = &typeContext;
                visitor.fieldExp_       = fieldExp;

                auto type = typeAnnotation.get(fieldExp->lhs.get());
                type->accept(&visitor);
                didApplyUpdate = didApplyUpdate || visitor.didApplyUpdate;
            }
            catch (const Error& error)
            {
                throw ErrorLocation{ fieldExp->location, error.what() };
            }
        }
    } while (didApplyUpdate);
}

} // namespace

Function* CodeGenerator::generatePrototype(const ImportedModule* mod, const ast::Function* ast, std::vector<const type::TypeInstance*> types)
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
        typeAnnotation = typeContext.getAnnotation(mod, ast);
        // should I inject when we generate the prototype? Or later?
        injectFunctionTypes(ast, typeAnnotation, *sourceModule, typeContext);
        injectFieldTypes(typeAnnotation, typeContext);

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

    auto name = getMangledName(*mod, ast, types);

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
            assert(!llvmFunction->arg_empty());
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
    size_t argOffset = (types[0]->isBasicType() || types[0]->isRefType()) ? 0 : 1;

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

        llvm::Value* llvmArg           = getArg(llvmFunction, argOffset + i);
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
    ExpCodeGeneratorContext context{ *this, function->typeAnnotation };
    ExpCodeGenerator        expGenerator(types[0], context, std::move(namedValues));
    ast->functionBody->accept(&expGenerator);

    if (expGenerator.getResult() != nullptr)
    {
        expGenerator.generateCleanup(*ast->functionBody);
        if (types[0]->isBasicType() || types[0]->isRefType())
        {
            llvmBuilder.CreateRet(expGenerator.getResult());
        }
        else
        {
            auto load  = llvmBuilder.CreateLoad(types[0]->llvmType(), expGenerator.getResult());
            auto store = llvmBuilder.CreateStore(load, llvmFunction->getArg(0));
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

IntrinsicFunction* CodeGenerator::getIntrinsicFunction(const std::vector<std::string_view>& splitName)
{
    std::string strName = std::string{ splitName.at(0) };
    auto        funIt   = intrinsicFunctions.find(strName);
    if (funIt == intrinsicFunctions.end())
    {
        if (llvmModule->getNamedValue(strName) == nullptr)
        {
            auto retType = getTypeContext()->getType({ { std::string{}, std::string{ splitName.at(1) } }, {} })->llvmType();

            std::vector<type::TypeInstPtr> types;
            std::vector<llvm::Type*>       llvmTypes;
            if (splitName.size() >= 3)
            {
                for (auto it = splitName.begin() + 2; it != splitName.end(); ++it)
                {
                    auto argType = getTypeContext()->getType({ { std::string{}, std::string{ *it } }, {} });
                    types.push_back(argType);
                    llvmTypes.push_back(argType->llvmType());
                }
            }
            auto funType = llvm::FunctionType::get(retType, llvmTypes, false);
            auto fun     = llvm::Function::Create(funType, llvm::Function::ExternalLinkage, strName, *llvmModule);
            funIt        = intrinsicFunctions.insert({ std::move(strName), { fun, std::move(types) } }).first;
        }
        else
        {
            // intrinsic clash with exported function
            throw Error{ "function already defined" };
        }
    }
    return &funIt->second;
}


ExpCodeGenerator::ExpCodeGenerator(type::TypeInstPtr type_, ExpCodeGeneratorContext& context, std::map<std::string, Value> parameters_)
    : parent{ nullptr },
      context_{ context },
      expectedType{ type_ },
      values{ std::move(parameters_) },
      result{ nullptr }
{
    assert(expectedType->llvmType() != nullptr);
}

ExpCodeGenerator::ExpCodeGenerator(type::TypeInstPtr type_, ExpCodeGenerator* parent_, std::map<std::string, Value> scope_)
    : parent{ parent_ },
      context_{ parent_->context_ },
      expectedType{ type_ },
      values{ std::move(scope_) },
      result{ nullptr }
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

    auto result = generator.result;

    if (result == nullptr)
    {
        generator.temporaries.clear();
        return nullptr;
    }

    generator.generateCleanup(exp);

    parent->temporaries.push_back({ generator.expectedType, generator.result });
    return generator.result;
}

Function* ExpCodeGenerator::getFunction(const GlobalIdentifier& identifier, std::vector<type::TypeInstPtr> types)
{
    return context_.generator_.getFunction(identifier, std::move(types));
}

type::TypeContext* ExpCodeGenerator::getTypeContext()
{
    return context_.generator_.getTypeContext();
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
                                        ? getTypeContext()->constructTypeUsingAnnotationStuff(context_.typeAnnotation_, *var)
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
    const auto n = name(exp, "if");

    auto condV = ExpCodeGenerator::generateScoped(*exp.condition, getTypeContext()->getBool(), this);
    if (condV == nullptr)
    {
        return;
    }

    auto& builder  = llvmBuilder();
    auto  function = builder.GetInsertBlock()->getParent();

    auto thenBB  = llvm::BasicBlock::Create(llvmContext(), n + ".then", function);
    auto elseBB  = llvm::BasicBlock::Create(llvmContext(), n + ".else");
    auto mergeBB = llvm::BasicBlock::Create(llvmContext(), n + ".join");

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
    auto PN   = builder.CreatePHI(type, 2, n + ".phi");

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
        auto condition    = context.irBuilder.CreateCmp(llvm::CmpInst::ICMP_EQ, value.llvmValue, patternValue, name(pattern, "boolPattern"));
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
        auto         condition    = context.irBuilder.CreateCmp(llvm::CmpInst::ICMP_EQ, value.llvmValue, patternValue, name(pattern, "intPattern"));
        context.irBuilder.CreateCondBr(condition, nextBlock, context.failBlock);
    }

    void visit(ast::FloatPattern& pattern) override
    {
        auto patternValue = llvm::ConstantFP::get(value.type->llvmType(), pattern.value);
        auto condition    = context.irBuilder.CreateFCmp(llvm::CmpInst::FCMP_OEQ, value.llvmValue, patternValue, name(pattern, "floatPattern"));
        context.irBuilder.CreateCondBr(condition, nextBlock, context.failBlock);
    }

    void visit(ast::CharPattern& pattern) override
    {
        assert(pattern.value.size() == 1);
        auto patternValue = llvm::ConstantInt::get(value.llvmValue->getType(), static_cast<uint8_t>(pattern.value[0]));
        auto condition    = context.irBuilder.CreateCmp(llvm::CmpInst::ICMP_EQ, value.llvmValue, patternValue, name(pattern, "charPattern"));
        context.irBuilder.CreateCondBr(condition, nextBlock, context.failBlock);
    }

    void visit(ast::StringPattern& pattern) override
    {
        assert(false);
    }

    void visit(ast::ConstructorPattern& pattern) override
    {
        const auto n = name(pattern, "conPattern");

        auto&      llvmContext  = context.irBuilder.getContext();
        const auto constructors = value.type->getConstructors();
        const bool variant      = constructors.size() > 1;
        const auto count        = pattern.arguments.size();

        std::vector<llvm::BasicBlock*> blocks;
        for (size_t i = 0; i < count; ++i)
        {
            blocks.push_back(llvm::BasicBlock::Create(llvmContext, name(pattern.arguments[i], "conPatternArg")));
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
                                       ? context.irBuilder.CreateGEP(heapType, value.llvmValue, { zeroV, twoV, indexV }, n + ".ptr")
                                       : context.irBuilder.CreateGEP(heapType, value.llvmValue, { zeroV, indexV }, n + ".ptr");
            auto         argType = constructor->fields[blockIt.index()];
            if (argType->isBasicType() || argType->isRefType())
            {
                argV = context.irBuilder.CreateLoad(argType->llvmType(), argPtr, n + ".load");
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
            auto enumPtr          = context.irBuilder.CreateGEP(heapType, value.llvmValue, { zeroV, oneV }, n + ".enumPtr");
            auto enumValue        = context.irBuilder.CreateLoad(enumType, enumPtr, n + ".enum");
            auto patternEnumValue = llvm::ConstantInt::get(enumType, constructorIndex);
            auto condition        = context.irBuilder.CreateCmp(llvm::CmpInst::ICMP_EQ, enumValue, patternEnumValue, n + ".enumCmp");
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
    const auto n = name(exp, "case");

    const auto count = exp.clauses.size();
    assert(count != 0);
    auto& builder  = llvmBuilder();
    auto  function = builder.GetInsertBlock()->getParent();

    type::TypeInstPtr caseT = nullptr;
    llvm::Value*      caseV = nullptr;
    try
    {
        caseT = getTypeContext()->constructTypeUsingAnnotationStuff(context_.typeAnnotation_, *exp.caseExp);
        caseV = ExpCodeGenerator::generateScoped(*exp.caseExp, caseT, this);
    }
    catch (const Error& e)
    {
        Log(exp.caseExp->location, e.what());
        return;
    }

    std::vector<llvm::BasicBlock*> expBlocks;
    std::vector<llvm::BasicBlock*> patternBlocks;
    for (size_t i = 0; i < count; ++i)
    {
        expBlocks.push_back(llvm::BasicBlock::Create(llvmContext(), name(*exp.clauses[i].exp, "caseExp")));
        patternBlocks.push_back(llvm::BasicBlock::Create(llvmContext(), name(*exp.clauses[i].pattern, "casePattern")));
    }
    auto mergeBB = llvm::BasicBlock::Create(llvmContext(), n + ".join");

    builder.CreateBr(patternBlocks.front());

    std::vector<std::map<std::string, Value>> variables;
    // simplify with -jump-threading?
    for (size_t i = 0; i < count; ++i)
    {
        try
        {
            auto                        uglyHack  = expBlocks.back(); // TODO: replace with call to abort()
            auto                        failBlock = i == count - 1 ? uglyHack : patternBlocks[i + 1];
            PatternCodeGeneratorContext context{ builder, failBlock, getTypeContext(), &context_.typeAnnotation_ };
            PatternCodeGenerator        gen{ context, { caseT, caseV }, expBlocks[i] };

            function->getBasicBlockList().push_back(patternBlocks[i]);
            builder.SetInsertPoint(patternBlocks[i]);
            exp.clauses[i].pattern->accept(&gen);

            variables.push_back(std::move(context.variables));
        }
        catch (const Error& e)
        {
            Log(exp.clauses[i].pattern->location, e.what());
            return;
        }
    }

    std::vector<llvm::Value*> caseValues;
    for (size_t i = 0; i < count; ++i)
    {
        try
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
        catch (const Error& e)
        {
            Log(exp.clauses[i].exp->location, e.what());
            return;
        }
    }

    if (caseValues.size() != count)
    {
        // some clause failed
        temporaries.clear();
        return;
    }

    function->getBasicBlockList().push_back(mergeBB);
    builder.SetInsertPoint(mergeBB);
    auto PN = builder.CreatePHI(expectedType->llvmType(), static_cast<unsigned int>(count), n + ".phi");
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
            [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFMul(v1, v2, name(exp, "fmul")); },
            [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateMul(v1, v2, name(exp, "mul"), false, false); });
    }
    else if (exp.op == "/") // Division
    {
        generateBinary(
            exp, checkNum,
            [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFDiv(v1, v2, name(exp, "fdiv")); },
            [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateSDiv(v1, v2, name(exp, "sdiv"), false); },
            [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateUDiv(v1, v2, name(exp, "udiv"), false); });
    }
    else if (exp.op == "%") // Remainder
    {
        generateBinary(
            exp, checkNum,
            [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFRem(v1, v2, name(exp, "frem")); },
            [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateSRem(v1, v2, name(exp, "srem")); },
            [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateURem(v1, v2, name(exp, "urem")); });
    }
    else if (exp.op == "+") // Addition
    {
        generateBinary(
            exp, checkNum,
            [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFAdd(v1, v2, name(exp, "fadd")); },
            [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateAdd(v1, v2, name(exp, "add")); });
    }
    else if (exp.op == "-") // Subtraction
    {
        generateBinary(
            exp, checkNum,
            [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateFSub(v1, v2, name(exp, "fsub")); },
            [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateSub(v1, v2, name(exp, "sub")); });
    }
    // bitwise
    else if (exp.op == "<<") // Shift
    {
        // Both arguments to the 'shl' instruction must be the same integer or vector of integer type. 'op2' is treated as an unsigned value.
        generateBinary(exp, checkInteger,
                       [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateShl(v1, v2, name(exp, "shl"), false, false); });
    }
    else if (exp.op == ">>") // Signed shift
    {
        generateBinary(exp, checkInteger, // check Signed?
                       [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateAShr(v1, v2, name(exp, "ashr"), false); });
    }
    else if (exp.op == ">>>") // Logical shift
    {
        generateBinary(exp, checkInteger,
                       [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateLShr(v1, v2, name(exp, "lshr"), false); });
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
                       [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateAnd(v1, v2, name(exp, "and")); });
    }
    else if (exp.op == "|")
    {
        generateBinary(exp, checkInteger,
                       [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateOr(v1, v2, name(exp, "or")); });
    }
    else if (exp.op == "^")
    {
        generateBinary(exp, checkInteger,
                       [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateXor(v1, v2, name(exp, "xor")); });
    }
    // logical
    else if (exp.op == "&&")
    {
        generateBinary(exp, checkBool,
                       [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateAnd(v1, v2, name(exp, "and")); });
    }
    else if (exp.op == "||")
    {
        generateBinary(exp, checkBool,
                       [this, &exp](llvm::Value* v1, llvm::Value* v2) { return llvmBuilder().CreateOr(v1, v2, name(exp, "or")); });
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
            auto type = getTypeContext()->constructTypeUsingAnnotationStuff(context_.typeAnnotation_, *exp.lhs);
            getTypeContext()->check(context_.typeAnnotation_, type, context_.typeAnnotation_.get(exp.rhs.get()));

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
                    result = llvmBuilder().CreateFNeg(value, name(exp, "fneg"));
                }
                else
                {
                    result = llvmBuilder().CreateNeg(value, name(exp, "neg"));
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
                result = llvmBuilder().CreateNot(value, name(exp, "lnot"));
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
                result = llvmBuilder().CreateNot(value, name(exp, "bnot"));
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
        const auto n = name(exp, "call");

        std::vector<type::TypeInstPtr> types;
        types.push_back(expectedType);
        for (auto& arg : exp.arguments)
        {
            auto type = getTypeContext()->constructTypeUsingAnnotationStuff(context_.typeAnnotation_, *arg);
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
                retValue = llvmBuilder().CreateAlloca(expectedType->llvmType(), nullptr, n + ".RetVal");
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

            if (retValue != nullptr)
            {
                llvmBuilder().CreateCall(function->llvm, arguments);
                result = retValue;
            }
            else
            {
                result = llvmBuilder().CreateCall(function->llvm, arguments, n);
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
            result = llvmBuilder().CreateCall(function.llvm, std::vector<llvm::Value*>{}, name(exp, "varCall"));
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
    const auto n = name(exp, "field");

    try
    {
        auto lhsType = getTypeContext()->constructTypeUsingAnnotationStuff(context_.typeAnnotation_, *exp.lhs);

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
            Log(exp.location, "Failed to unify types '", fieldType->identifier().str(), "' and '", expectedType->identifier().str() + '\'');
            return;
        }
        // I think we need both since check might do subs
        // 22-01-31 except constructTypeUsingAnnotationStuff already checks?
        getTypeContext()->check(context_.typeAnnotation_, expectedType, context_.typeAnnotation_.get(&exp));

        auto structValue = ExpCodeGenerator::generateUnscoped(*exp.lhs, lhsType, this);
        if (structValue == nullptr)
        {
            return;
        }

        auto ptrValue = llvmBuilder().CreateGEP(lhsType->llvmType(), structValue, { i32V(0), i32V(index) }, n + ".ptr");
        if (expectedType->isBasicType() || expectedType->isRefType())
        {
            result = llvmBuilder().CreateLoad(expectedType->llvmType(), ptrValue, n + ".load");
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
    const auto n = name(exp, "con");

    try
    {
        getTypeContext()->check(context_.typeAnnotation_, expectedType, context_.typeAnnotation_.get(&exp));

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
                    argValue = llvmBuilder().CreateLoad(fieldIt.value()->llvmType(), argValue, n + ".loadArg");
                }

                auto index = static_cast<unsigned int>(fieldIt.index());
                value      = llvmBuilder().CreateInsertValue(value, argValue, { index }, n + ".insertArg");
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

                ptrValue        = llvmBuilder().CreateCall(mallocFun, llvm::ConstantExpr::getSizeOf(valueType), n + ".malloc");
                auto zeroV      = i32V(0);
                auto oneV       = i32V(1);
                auto counterPtr = llvmBuilder().CreateGEP(valueType, ptrValue, { zeroV, zeroV }, n + ".counterPtr");
                auto enumPtr    = llvmBuilder().CreateGEP(valueType, ptrValue, { zeroV, oneV }, n + ".enumPtr");
                auto dataPtr    = llvmBuilder().CreateGEP(valueType, ptrValue, { zeroV, i32V(2) }, n + ".dataPtr");
                llvmBuilder().CreateStore(oneV, counterPtr);
                llvmBuilder().CreateStore(enumValue, enumPtr);
                llvmBuilder().CreateStore(value, dataPtr);
            }
            else
            {
                ptrValue = llvmBuilder().CreateAlloca(valueType, nullptr, n + ".alloca");
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

namespace
{

uint64_t getSafeIntegerSize(llvm::Type* type)
{
    assert(type->isFloatingPointTy());
    switch (type->getTypeID())
    {
    case llvm::Type::HalfTyID: return 11;
    case llvm::Type::FloatTyID: return 24;
    case llvm::Type::DoubleTyID: return 53;
    }
    return 0;
}

llvm::Value* createPromote(llvm::IRBuilder<>& b, type::TypeInstPtr from, type::TypeInstPtr to, llvm::Value* arg)
{
    auto       type     = to->llvmType();
    const auto fromSize = from->llvmType()->getPrimitiveSizeInBits().getFixedSize();
    const auto toSize   = to->llvmType()->getPrimitiveSizeInBits().getFixedSize();
    if (from->isInteger())
    {
        if (to->isFloating())
        {
            if (fromSize < getSafeIntegerSize(to->llvmType()))
            {
                return to->isSigned()
                           ? b.CreateFPToSI(arg, type)
                           : b.CreateFPToUI(arg, type);
            }
        }
        else if (fromSize < toSize)
        {
            if (from->isSigned())
            {
                if (to->isSigned())
                {
                    return b.CreateSExt(arg, type);
                }
            }
            else
            {
                return b.CreateZExt(arg, type);
            }
        }
    }
    else if (to->isFloating() && fromSize < toSize)
    {
        return b.CreateFPExt(arg, type);
    }
    throw Error{ "no instance of \"promote " + from->identifier().str() + " " + to->identifier().str() + '"' };
}

llvm::Value* createConvert(llvm::IRBuilder<>& b, type::TypeInstPtr from, type::TypeInstPtr to, llvm::Value* arg)
{
    auto       type     = to->llvmType();
    const auto fromSize = from->llvmType()->getPrimitiveSizeInBits().getFixedSize();
    const auto toSize   = to->llvmType()->getPrimitiveSizeInBits().getFixedSize();
    if (from->isFloating())
    {
        if (to->isFloating())
        {
            if (toSize < fromSize)
            {
                return b.CreateFPTrunc(arg, type);
            }
        }
        else if (to->isSigned())
        {
            return b.CreateFPToSI(arg, type);
        }
        else
        {
            return b.CreateFPToUI(arg, type);
        }
    }
    else
    {
        if (to->isFloating())
        {
            // special, if 'to' can hold all values of 'from'
            // dont allow 'convert', it should be a 'promote'
            const auto minSize = getSafeIntegerSize(to->llvmType());
            if (fromSize > minSize)
            {
                return from->isSigned()
                           ? b.CreateSIToFP(arg, type)
                           : b.CreateUIToFP(arg, type);
            }
        }
        else
        {
            if (fromSize > toSize)
            {
                return b.CreateTrunc(arg, type);
            }
            else if (fromSize == toSize && from->isSigned() && !to->isSigned())
            {
                return b.CreateBitCast(arg, type);
            }
        }
    }
    throw Error{ "no instance of \"convert " + from->identifier().str() + " " + to->identifier().str() + '"' };
}

llvm::Value* createBitcast(llvm::IRBuilder<>& b, type::TypeInstPtr, type::TypeInstPtr to, llvm::Value* arg)
{
    return b.CreateBitCast(arg, to->llvmType());
}

using IntrinsicCreateFunction =
    llvm::Value* (*)(llvm::IRBuilder<>& b, type::TypeInstPtr from, type::TypeInstPtr to, llvm::Value* arg);

IntrinsicCreateFunction lookupIntrinsicFunction(std::string_view name)
{
    if (name == "promote")
    {
        return createPromote;
    }
    else if (name == "convert")
    {
        return createConvert;
    }
    else if (name == "bitcast")
    {
        return createBitcast;
    }
    return nullptr;
}

void intrinsicError(SourceLocation location, llvm::ArrayRef<std::string_view> split)
{
    assert(!split.empty());
    std::string msg = "no instance of \"";
    msg += split.front();
    if (split.size() >= 2)
    {
        for (const auto& part : split.drop_front())
        {
            msg += ' ';
            msg += part;
        }
    }
    msg += '"';
    throw ErrorLocation{ location, msg };
}

} // namespace

void ExpCodeGenerator::visit(ast::IntrinsicExp& exp)
{
    auto location        = exp.location;
    auto split           = llfp::str_split(exp.identifier_, '\'');
    auto name            = split.at(0);
    auto createIntrinsic = lookupIntrinsicFunction(name);
    try
    {
        if (createIntrinsic != nullptr)
        {
            if (split.size() != 3)
            {
                intrinsicError(location, split);
            }
            if (exp.arguments_.size() != 1)
            {
                throw ErrorLocation{ location, "incorrect number of arguments" };
            }
            auto fromType = getTypeContext()->getType({ { std::string{}, std::string{ split.at(2) } }, {} });
            auto toType   = getTypeContext()->getType({ { std::string{}, std::string{ split.at(1) } }, {} });
            if (!fromType->isNum() || !toType->isNum())
            {
                intrinsicError(location, split);
            }
            if (name == "bitcast" && fromType->llvmType()->getPrimitiveSizeInBits() != toType->llvmType()->getPrimitiveSizeInBits())
            {
                intrinsicError(location, split);
            }
            auto arg = ExpCodeGenerator::generateUnscoped(*exp.arguments_.at(0), fromType, this);
            result   = createIntrinsic(llvmBuilder(), fromType, toType, arg);
        }
        else
        {
            // create c function
            auto fun = context_.generator_.getIntrinsicFunction(split);

            std::vector<llvm::Value*> arguments;
            for (const auto& argIt : llvm::enumerate(exp.arguments_))
            {
                auto argValue = generateUnscoped(*argIt.value(), fun->types[argIt.index()], this);
                if (argValue == nullptr)
                {
                    return;
                }
                arguments.push_back(argValue);
            }

            result = llvmBuilder().CreateCall(fun->llvm, arguments);
        }
    }
    catch (const Error& e)
    {
        throw ErrorLocation{ location, e.what() };
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

void ExpCodeGenerator::generateCleanup(const ast::Exp& exp)
{
    const auto n = name(exp, "cleanup");

    if (expectedType->isRefType())
    {
        auto res = result;
        if (!std::any_of(temporaries.begin(), temporaries.end(), [res](const Value& v) { return v.llvmValue == res; }))
        {
            auto int32Type    = llvm::Type::getInt32Ty(llvmContext());
            auto oneV         = llvm::ConstantInt::get(int32Type, 1);
            auto counterValue = llvmBuilder().CreateLoad(int32Type, result, n + ".refCount");
            auto newValue     = llvmBuilder().CreateAdd(counterValue, oneV, n + ".incCount");
            llvmBuilder().CreateStore(newValue, result);
        }
    }

    for (auto temporary : temporaries)
    {
        if (temporary.llvmValue != result)
        {
            if (temporary.type->isRefType())
            {
                auto fun = context_.generator_.getReleaseFunction(temporary.type);
                llvmBuilder().CreateCall(fun, { temporary.llvmValue });
            }
            else if (!temporary.type->isBasicType())
            {
                auto fun = context_.generator_.getDeleteFunction(temporary.type);
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

    auto name = getMangledName(*sourceModule, "release", type);

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

    auto name = getMangledName(*sourceModule, "delete", type);

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
    auto inValue = llvmFunction->getArg(0);

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
            auto fieldPtr = llvmBuilder.CreateGEP(variantType, argValue, { i32V(0), i32V(fieldIndex) }, "deleteFieldPtr");
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
    auto argValue = llvmFunction->getArg(0);

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
    auto argValue = llvmFunction->getArg(0);

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
        auto valuePtr  = llvmBuilder.CreateGEP(heapType, argValue, { i32V(0), i32V(2) }, "deleteDataPtr");
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
