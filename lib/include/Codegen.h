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
namespace codegen
{

class ExpCodeGenerator;

struct Function
{
    ast::FunctionDeclaration *ast;
    llvm::Function *llvm;
};

struct Value
{
    type::Type *type;
    llvm::Value *value;
};

class CodeGenerator
{
    llvm::LLVMContext llvmContext; // one context per module, one module compiled per thread
    llvm::IRBuilder<> llvmBuilder;
    std::unique_ptr<llvm::Module> llvmModule;

    // should be map<pair<string, TypeEnv>, Function> functions; for "generic" functions
    std::unordered_map<std::string, Function> functions;
    type::TypeContext typeContext;

public:

    CodeGenerator();

    std::unique_ptr<llvm::Module> generate(const std::unique_ptr<ast::Module> &module);

private:

    void AddDllMain();

    friend ExpCodeGenerator;
};

class ExpCodeGenerator : public ast::ExpVisitor, public type::TypeEnvironment
{
    static const Value EmptyValue;

    ExpCodeGenerator* const      parent;
    CodeGenerator* const         generator;
    type::Type* const            expectedType;
    std::map<std::string, Value> values;
    llvm::Value*                 result;

public:

    ExpCodeGenerator(type::Type *type_, CodeGenerator *generator_, std::map<std::string, Value> parameters_);
    ExpCodeGenerator(type::Type *type_, ExpCodeGenerator *parent_);
    virtual ~ExpCodeGenerator() {}

    static llvm::Value* generate(ast::Exp &exp, type::Type *type, ExpCodeGenerator *parent);

    type::Type*  getTypeByName(llvm::StringRef variable) override;
    type::Type*  getVariableType(llvm::StringRef variable) override;
    type::Type*  getFunctionReturnType(llvm::StringRef variable) override;
    llvm::Value* getResult();

    void visit(ast::LetExp &exp) override;
    void visit(ast::IfExp &exp) override;
    void visit(ast::CaseExp &exp) override;
    void visit(ast::BinaryExp &exp) override;
    void visit(ast::UnaryExp &exp) override;
    void visit(ast::LiteralExp &exp) override;
    void visit(ast::CallExp &exp) override;
    void visit(ast::VariableExp &exp) override;

private:

    const Value& getNamedValue(const std::string &name);

    auto& llvmContext() { return generator->llvmContext; }
    auto& llvmBuilder() { return generator->llvmBuilder; }
    auto& llvmModule() { return generator->llvmModule; }
    auto& functions() { return generator->functions; }
    auto& typeContext() { return generator->typeContext; }

    auto generateBinary(type::Type* type, ast::BinaryExp &exp)
    {
        return std::make_tuple(
            ExpCodeGenerator::generate(*exp.lhs, type, this),
            ExpCodeGenerator::generate(*exp.rhs, type, this));
    }

    typedef bool(*TypeCheckFunction)(type::Type*);

    template<class T>
    void generateBinary(ast::BinaryExp &exp, TypeCheckFunction tcf, T create)
    {
        if (tcf(expectedType))
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
        if (tcf(expectedType))
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
        if (tcf(expectedType))
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
