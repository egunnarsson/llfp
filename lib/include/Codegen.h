#pragma once

#include <memory>
#include <unordered_map>
#include <string>

#pragma warning(push, 0)

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

#pragma warning(pop)

#include "Ast.h"
#include "Type.h"


namespace llfp
{

class ImportedModule;
class SourceModule;

namespace codegen
{

class ExpCodeGenerator;

struct Function
{
    const ast::Function*     ast;
    llvm::Function*          llvm;
    std::vector<type::Type*> types;
};

struct Value
{
    type::Type*  type;
    llvm::Value* value;
};

class CodeGenerator
{
    SourceModule*                 sourceModule;

    llvm::LLVMContext             llvmContext; // one context per module, one module compiled per thread
    llvm::IRBuilder<>             llvmBuilder;
    std::unique_ptr<llvm::Module> llvmModule;

    // mangled name as id
    std::unordered_map<std::string, Function> functions;
    type::TypeContext             typeContext; // move to source module (if it should create types it needs the llvmContext)

public:

    CodeGenerator(SourceModule *sourceModule_);

    bool               generateFunction(const ast::Function*ast);
    bool               generateFunction(const ast::Function*ast, std::vector<type::Type*> types);

    llvm::Module*      getLLVM() { return llvmModule.get(); }
    type::TypeContext* getTypeContext() { return &typeContext; }

private:

    Function*          generatePrototype(const ImportedModule* module, const ast::Function*ast, std::vector<type::Type*> types);
    bool               generateFunctionBody(Function *function);

    void               AddDllMain(); // should be done on dll not on one module

    // lookup, global functions, generate llvmFunction if first external reference
    Function*          getFunction(GlobalIdentifierRef identifier, std::vector<type::Type*> types);

    friend ExpCodeGenerator;
};

class ExpCodeGenerator : public ast::ExpVisitor, public type::TypeScope
{
    static const Value EmptyValue;

    ExpCodeGenerator* const      parent;
    CodeGenerator* const         generator;
    type::Type* const            expectedType;
    std::map<std::string, Value> values;
    //std::map<string, .> letExpFunctions; localFunction
    llvm::Value*                 result;

public:

    ExpCodeGenerator(type::Type *type_, CodeGenerator *generator_, std::map<std::string, Value> parameters_);
    ExpCodeGenerator(type::Type *type_, ExpCodeGenerator *parent_);
    virtual ~ExpCodeGenerator() {}

    static llvm::Value*  generate(ast::Exp &exp, type::Type *type, ExpCodeGenerator *parent);

    // lookup, local functions, global functions,
    Function*            getFunction(GlobalIdentifierRef identifier, std::vector<type::Type*> types);

    type::TypeContext*   getTypeContext() override { return generator->getTypeContext(); }
    type::Type*          getVariableType(const std::string& variable) override;
    const ast::Function* getFunctionAST(GlobalIdentifierRef identifier) override;
    llvm::Value*         getResult();

    void visit(ast::LetExp &exp) override;
    void visit(ast::IfExp &exp) override;
    void visit(ast::CaseExp &exp) override;
    void visit(ast::BinaryExp &exp) override;
    void visit(ast::UnaryExp &exp) override;
    void visit(ast::LiteralExp &exp) override;
    void visit(ast::CallExp &exp) override;
    void visit(ast::VariableExp &exp) override;
    void visit(ast::FieldExp &exp) override;
    void visit(ast::ConstructorExp &exp) override;

private:

    const Value& getNamedValue(const std::string &name);

    auto& llvmContext() { return generator->llvmContext; }
    auto& llvmBuilder() { return generator->llvmBuilder; }
    auto& typeContext() { return generator->typeContext; }

    auto generateBinary(type::Type* type, ast::BinaryExp &exp)
    {
        return std::make_tuple(
            ExpCodeGenerator::generate(*exp.lhs, type, this),
            ExpCodeGenerator::generate(*exp.rhs, type, this));
    }

    typedef bool(*TypeCheckFunction)(ast::Exp &exp, type::Type*);

    template<class T>
    void generateBinary(ast::BinaryExp &exp, TypeCheckFunction tcf, T create)
    {
        if (tcf(exp, expectedType))
        {
            auto args = generateBinary(expectedType, exp);
            auto arg1 = std::get<0>(args), arg2 = std::get<1>(args);
            if (arg1 == nullptr || arg2 == nullptr)
            {
                return;
            }
            result = create(arg1, arg2);
        }
    }

    template<class F, class T>
    void generateBinary(ast::BinaryExp &exp, TypeCheckFunction tcf, F createFloat, T createInteger)
    {
        if (tcf(exp, expectedType))
        {
            auto args = generateBinary(expectedType, exp);
            auto arg1 = std::get<0>(args), arg2 = std::get<1>(args);
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
    void generateBinary(ast::BinaryExp &exp, TypeCheckFunction tcf, F createFloat, T createSigned, U createUsigned)
    {
        if (tcf(exp, expectedType))
        {
            auto args = generateBinary(expectedType, exp);
            auto arg1 = std::get<0>(args), arg2 = std::get<1>(args);
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

    void generateCompare(llvm::CmpInst::Predicate predicate, ast::BinaryExp &exp);
};

} // codegen
} // llfp
