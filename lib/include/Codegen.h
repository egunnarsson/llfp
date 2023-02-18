#pragma once

#include "Ast.h"
#include "Driver.h"
#include "IModule.h"
#include "Type/TypeContext.h"
#include "Type/TypeInference.h"

#pragma warning(push, 0)

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

#pragma warning(pop)

#include <memory>
#include <string>
#include <unordered_map>


namespace llfp
{

class SourceModule;

namespace codegen
{

class ExpCodeGenerator;

struct Function
{
    const ast::Function*           ast;
    hm::TypeAnnotation             typeAnnotation;
    llvm::Function*                llvm;
    std::vector<type::TypeInstPtr> types;
};

struct Value
{
    type::TypeInstPtr type;
    llvm::Value*      llvmValue;
};

class CodeGenerator
{
    Driver*        driver;
    GlobalContext* globalContext; // lookup (global)
    SourceModule*  sourceModule;  // lookup (local)

    llvm::LLVMContext* llvmContext; // one context per module, one module compiled per thread
    llvm::IRBuilder<>  llvmBuilder;
    llvm::Module*      llvmModule;

    // mangled name as id
    std::unordered_map<std::string, Function>              functions;   // unique_ptr<Function>? value probably not moved because hm::TypeAnnotation
    type::TypeContext                                      typeContext; // move to source module (if it should create types it needs the llvmContext)
    std::unordered_map<type::TypeInstPtr, llvm::Function*> releaseFunctions;
    std::unordered_map<type::TypeInstPtr, llvm::Function*> deleteFunctions;

public:

    CodeGenerator(
        Driver*            driver_,
        GlobalContext*     globalContext_,
        SourceModule*      sourceModule_,
        llvm::LLVMContext* llvmContext_,
        llvm::Module*      llvmModule_);

    bool generateFunction(const ast::Function* ast);
    bool generateFunction(const ast::Function* ast, std::vector<const type::TypeInstance*> types);

    bool generateReleaseFunctionBody(type::TypeInstPtr type);
    bool generateDeleteFunctionBody(type::TypeInstPtr type);

    type::TypeContext* getTypeContext() { return &typeContext; }

private:

    Function* generatePrototype(const ImportedModule* module, const ast::Function* ast, std::vector<const type::TypeInstance*> types);
    bool      generateFunctionBody(Function* function);

    void AddDllMain(); // should be done on dll not on one module

    // lookup, global functions, generate llvmFunction if first external reference
    Function* getFunction(const GlobalIdentifier& identifier, std::vector<const type::TypeInstance*> types);

    llvm::Function* getReleaseFunction(type::TypeInstPtr type);
    llvm::Function* getDeleteFunction(type::TypeInstPtr type);

    bool generateDeleteFunctionBodyAggregate(llvm::Function* function, type::TypeInstPtr type);
    bool generateDeleteFunctionBodyVariant(llvm::Function* function, type::TypeInstPtr type);
    void generateDeleteConstructorBlock(llvm::Value* argValue, const llfp::type::TypeConstructor& constructor);

    llvm::Value* i32V(uint64_t i);

    friend ExpCodeGenerator;
};

class ExpCodeGenerator : public ast::ExpVisitor
{
    static const Value EmptyValue;

    ExpCodeGenerator* const      parent;
    CodeGenerator* const         generator;
    const type::TypeInstance*    expectedType;
    std::map<std::string, Value> values;
    // std::map<string, .> letExpFunctions; localFunction
    llvm::Value*                 result;
    std::vector<Value>           temporaries;
    hm::TypeAnnotation*          typeAnnotation;

public:

    ExpCodeGenerator(type::TypeInstPtr type_, CodeGenerator* generator_, std::map<std::string, Value> parameters_, hm::TypeAnnotation* typeAnnotation_);
    ExpCodeGenerator(type::TypeInstPtr type_, ExpCodeGenerator* parent_, std::map<std::string, Value> scope_);
    virtual ~ExpCodeGenerator() { assert(temporaries.empty()); }

    static llvm::Value* generateUnscoped(ast::Exp& exp, type::TypeInstPtr type, ExpCodeGenerator* parent, std::map<std::string, Value> scope = {});
    static llvm::Value* generateScoped(ast::Exp& exp, type::TypeInstPtr type, ExpCodeGenerator* parent, std::map<std::string, Value> scope = {});

    void generateCleanup();

    // lookup, local functions, global functions,
    Function*          getFunction(const GlobalIdentifier& identifier, std::vector<type::TypeInstPtr> types);
    type::TypeContext* getTypeContext();
    llvm::Value*       getResult();

    void visit(ast::LetExp& exp) override;
    void visit(ast::IfExp& exp) override;
    void visit(ast::CaseExp& exp) override;
    void visit(ast::BinaryExp& exp) override;
    void visit(ast::UnaryExp& exp) override;
    void visit(ast::LiteralExp& exp) override;
    void visit(ast::CallExp& exp) override;
    void visit(ast::VariableExp& exp) override;
    void visit(ast::FieldExp& exp) override;
    void visit(ast::ConstructorExp& exp) override;

private:

    const Value& getNamedValue(const std::string& name);

    auto& llvmContext() { return *generator->llvmContext; }
    auto& llvmBuilder() { return generator->llvmBuilder; }
    auto& llvmModule() { return *generator->llvmModule; }

    llvm::Value* i32V(uint64_t i) { return generator->i32V(i); }

    auto generateBinary(type::TypeInstPtr type, ast::BinaryExp& exp)
    {
        return std::make_tuple(
            ExpCodeGenerator::generateUnscoped(*exp.lhs, type, this),
            ExpCodeGenerator::generateUnscoped(*exp.rhs, type, this));
    }

    typedef bool (*TypeCheckFunction)(ast::Exp& exp, type::TypeInstPtr);

    template<class T>
    void generateBinary(ast::BinaryExp& exp, TypeCheckFunction tcf, T create)
    {
        if (tcf(exp, expectedType))
        {
            auto [arg1, arg2] = generateBinary(expectedType, exp);
            if (arg1 == nullptr || arg2 == nullptr)
            {
                return;
            }
            result = create(arg1, arg2);
        }
    }

    template<class F, class T>
    void generateBinary(ast::BinaryExp& exp, TypeCheckFunction tcf, F createFloat, T createInteger)
    {
        if (tcf(exp, expectedType))
        {
            auto [arg1, arg2] = generateBinary(expectedType, exp);
            if (arg1 == nullptr || arg2 == nullptr)
            {
                return;
            }
            if (expectedType->isFloating())
            {
                result = createFloat(arg1, arg2);
            }
            else
            {
                result = createInteger(arg1, arg2);
            }
        }
    }

    template<class F, class T, class U>
    void generateBinary(ast::BinaryExp& exp, TypeCheckFunction tcf, F createFloat, T createSigned, U createUsigned)
    {
        if (tcf(exp, expectedType))
        {
            auto [arg1, arg2] = generateBinary(expectedType, exp);
            if (arg1 == nullptr || arg2 == nullptr)
            {
                return;
            }
            if (expectedType->isFloating())
            {
                result = createFloat(arg1, arg2);
            }
            else if (expectedType->isSigned())
            {
                result = createSigned(arg1, arg2);
            }
            else
            {
                result = createUsigned(arg1, arg2);
            }
        }
    }

    void generateCompare(llvm::CmpInst::Predicate predicate, ast::BinaryExp& exp);
};

} // namespace codegen
} // namespace llfp
