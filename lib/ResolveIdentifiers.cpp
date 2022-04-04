
#include <algorithm>
#include <memory>
#include <string>

#include "Ast.h"
#include "Common/Algorithm.h"
#include "Common/SourceLocation.h"
#include "Log.h"
#include "Module.h"
#include "Type/TypeContext.h"

#include "ResolveIdentifiers.h"


namespace llfp
{
namespace
{

struct Context
{
    SourceModule&            srcModule;
    llvm::raw_string_ostream errs;
};


template<class Check>
void resolveType(Context& context, const SourceLocation& location, ast::TypeIdentifier& type, Check checkFun, bool allowEmpty)
{
    if (type.empty() && allowEmpty
        || type::isPrimitive(type)
        || checkFun(type))
    {
        return;
    }

    auto dataAst = context.srcModule.lookupType(type.identifier);
    if (dataAst.empty())
    {
        Log(context.errs, location, "undefined data type \"", type.identifier.str(), '"');
    }
    else if (type.identifier.moduleName.empty())
    {
        type.identifier.moduleName = dataAst.importedModule->name();
    }

    for (auto& param : type.parameters)
    {
        resolveType(context, location, param, checkFun, allowEmpty);
    }
}


class Scope
{
public:
    virtual bool isLocal(const std::string& id) const = 0;
};


// foward declare for circular dependency
void resolveLetExp(Context& srcModule, const ast::LetExp& letExp, const Scope* parentScope);
void resolveClause(Context& srcModule, const ast::Clause& clause, const Scope* parentScope);


class BaseExpVisitor : public ast::ExpVisitor, public Scope
{
protected:

    Context&           context;
    const Scope* const parentScope;

public:

    BaseExpVisitor(Context& context_, const Scope* parentScope_) :
        context{ context_ },
        parentScope{ parentScope_ }
    {}

    void fix(const SourceLocation& location, GlobalIdentifier& id)
    {
        if (id.moduleName.empty() && isLocal(id.name))
        {
            return;
        }

        auto funDeclAst = context.srcModule.lookupFunctionDecl(id);
        auto funAst = context.srcModule.lookupFunction(id);

        if (!funDeclAst.empty())
        {
            if (!funAst.empty())
            {
                Log(context.errs, location, "reference to \"", id.str(), "\" is ambiguous");
            }
            else if (id.moduleName.empty())
            {
                id.moduleName = funDeclAst.importedModule->name();
            }
        }
        else if (!funAst.empty())
        {
            if (id.moduleName.empty())
            {
                id.moduleName = funAst.importedModule->name();
            }
        }
        else
        {
            Log(context.errs, location, "undefined function \"", id.str(), '"');
        }
    }

    void fixConstructor(const SourceLocation &location, GlobalIdentifier& id)
    {
        auto dataAst = context.srcModule.lookupType(id);
        if (dataAst.empty())
        {
            Log(context.errs, location, "undefined data constructor \"", id.str(), '"');
        }
        else if (id.moduleName.empty())
        {
            id.moduleName = dataAst.importedModule->name();
        }
    }

    void visit(ast::LetExp& exp) override
    {
        resolveLetExp(context, exp, this);
    }

    void visit(ast::IfExp& exp) override
    {
        exp.condition->accept(this);
        exp.thenExp->accept(this);
        exp.elseExp->accept(this);
    }

    void visit(ast::CaseExp& exp) override
    {
        exp.caseExp->accept(this);

        for (auto& clause : exp.clauses)
        {
            resolveClause(context, clause, this);
        }
    }

    void visit(ast::BinaryExp& exp) override
    {
        exp.lhs->accept(this);
        exp.rhs->accept(this);
    }

    void visit(ast::UnaryExp& exp) override
    {
        exp.operand->accept(this);
    }

    void visit(ast::LiteralExp&) override {}

    void visit(ast::CallExp& exp) override
    {
        fix(exp.location, exp.identifier);

        for (auto& arg: exp.arguments)
        {
            arg->accept(this);
        }
    }

    void visit(ast::VariableExp& exp) override
    {
        fix(exp.location, exp.identifier);
    }

    void visit(ast::FieldExp& exp) override
    {
        exp.lhs->accept(this);
    }

    void visit(ast::ConstructorExp& exp) override
    {
        fixConstructor(exp.location, exp.identifier);

        for (auto& arg : exp.arguments)
        {
            arg.exp->accept(this);
        }
    }

    bool isLocal(const std::string& id) const override
    {
        return parentScope != nullptr ? parentScope->isLocal(id) : false;
    }
};


class FunctionExpVisitor final : public BaseExpVisitor
{
    const ast::Function& function;

public:

    FunctionExpVisitor(Context& context, const ast::Function& function_, const Scope* parentScope) :
        BaseExpVisitor(context, parentScope),
        function{ function_ }
    {}

    bool isLocal(const std::string& id) const override
    {
        if (std::any_of(function.parameters.begin(), function.parameters.end(),
            [&id](const std::unique_ptr<ast::Parameter>& param) { return param->identifier == id; }))
        {
            return true;
        }
        return BaseExpVisitor::isLocal(id);
    }
};


class LetExpVisitor final : public BaseExpVisitor
{
    const ast::LetExp& letExp;
    size_t             letIndex = 0;

    LetExpVisitor(Context& context, const ast::LetExp& letExp_, const Scope* parentScope) :
        BaseExpVisitor(context, parentScope),
        letExp{ letExp_ }
    {}

public:

    static void resolve(Context& srcModule, const ast::LetExp& letExp, const Scope* parentScope)
    {
        LetExpVisitor letVisitor{ srcModule, letExp, parentScope };

        for (const auto& letStm : letExp.letStatments)
        {
            FunctionExpVisitor funVisitor{ srcModule, *letStm, &letVisitor };
            letStm->functionBody->accept(&funVisitor);
            letVisitor.letIndex++;
        }

        letExp.exp->accept(&letVisitor);
    }

    bool isLocal(const std::string& id) const override
    {
        for (size_t i = 0; i < letIndex; ++i)
        {
            if (letExp.letStatments[i]->name == id)
            {
                return true;
            }
        }
        return BaseExpVisitor::isLocal(id);
    }
};


class PatternVisitor final : public ast::PatternVisitor
{
    Context& context;

public:

    PatternVisitor(Context& context_) : context{ context_ } {}

    void visit(ast::BoolPattern&) override {}
    void visit(ast::IdentifierPattern&) override {}
    void visit(ast::IntegerPattern&) override {}
    void visit(ast::FloatPattern&) override {}
    void visit(ast::CharPattern&) override {}
    void visit(ast::StringPattern&) override {}
    void visit(ast::ConstructorPattern& pattern) override 
    {
        fixConstructor(pattern.location, pattern.identifier);
        for (auto& arg : pattern.arguments)
        {
            arg.pattern->accept(this);
        }
    }

    void fixConstructor(const SourceLocation& location, GlobalIdentifier& id)
    {
        auto dataAst = context.srcModule.lookupType(id);
        if (dataAst.empty())
        {
            Log(context.errs, location, "undefined data constructor \"", id.str(), '"');
        }
        else if (id.moduleName.empty())
        {
            id.moduleName = dataAst.importedModule->name();
        }
    }
};


class ClauseExpVisitor final : public BaseExpVisitor
{
    class IdentifierLookupVisitor final : public ast::PatternVisitor
    {
    public:

        const std::string& identifier;
        bool               found = false;

        IdentifierLookupVisitor(const std::string& identifier_) : identifier{ identifier_ } {}

        void visit(ast::BoolPattern&) override {}
        void visit(ast::IdentifierPattern& exp) override { found = exp.value == identifier; }
        void visit(ast::IntegerPattern&) override {}
        void visit(ast::FloatPattern&) override {}
        void visit(ast::CharPattern&) override {}
        void visit(ast::StringPattern&) override {}
        void visit(ast::ConstructorPattern& exp) override
        {
            for (auto& arg : exp.arguments)
            {
                arg.pattern->accept(this);
                if (found) { break; }
            }
        }
    };

    const ast::Clause& clause;

public:

    ClauseExpVisitor(Context& srcModule_, const ast::Clause& clause_, const Scope* parentScope_):
        BaseExpVisitor(srcModule_, parentScope_),
        clause{ clause_ }
    {}

    static void resolve(Context& srcModule, const ast::Clause& clause, const Scope* parentScope)
    {
        PatternVisitor patternVisitor{ srcModule };
        clause.pattern->accept(&patternVisitor);
        ClauseExpVisitor expVisitor{ srcModule , clause , parentScope };
        clause.exp->accept(&expVisitor);
    }

    virtual bool isLocal(const std::string& id) const
    {
        IdentifierLookupVisitor visitor{ id };
        clause.pattern->accept(&visitor);
        if (visitor.found)
        {
            return true;
        }
        return BaseExpVisitor::isLocal(id);
    }
};


void resolveLetExp(Context& context, const ast::LetExp& letExp, const Scope* parentScope)
{
    LetExpVisitor::resolve(context, letExp, parentScope);
}

void resolveClause(Context& srcModule, const ast::Clause& clause, const Scope* parentScope)
{
    ClauseExpVisitor::resolve(srcModule, clause, parentScope);
}

void resolveFunction(Context& context, ast::Function& function)
{
    const auto checkLocalFun = [](const ast::TypeIdentifier&) { return false; };

    resolveType(context, function.location, function.type, checkLocalFun, true);
    for (const auto& param : function.parameters)
    {
        resolveType(context, param->location, param->type, checkLocalFun, true);
    }

    FunctionExpVisitor visitor{ context, function, nullptr };
    function.functionBody->accept(&visitor);
}

} // namespace

bool resolveIdentifiers(SourceModule& srcModule)
{
    std::string errorStr;
    Context context {
        srcModule,
        llvm::raw_string_ostream{ errorStr }
    };

    auto ast = srcModule.getAST();

    for (const auto& data : ast->datas)
    {
        auto checkLocalFun = [&data](const ast::TypeIdentifier& type)
        {
            if (type.parameters.empty() && type.identifier.moduleName.empty())
            {
                return contains(data->typeVariables, type.identifier.name);
            }
            return false;
        };

        for (auto& field : data->fields)
        {
            resolveType(context, field.location, field.type, checkLocalFun, false);
        }
    }

    for (const auto& function : ast->functions)
    {
        resolveFunction(context, *function);
    }

    for (const auto& class_ : ast->classes)
    {
        auto checkLocalFun = [&class_](ast::TypeIdentifier& type)
        {
            if (type.parameters.empty() && type.identifier.moduleName.empty())
            {
                return class_->typeVariable == type.identifier.name;
            }
            return false;
        };

        for (const auto& function : class_->functions)
        {
            resolveType(context, function->location, function->type, checkLocalFun, false);
            for (const auto& param : function->parameters)
            {
                resolveType(context, param->location, param->type, checkLocalFun, false);
            }
        }
    }

    for (const auto& classInstance : ast->classInstances)
    {
        auto checkLocalFun = [](const ast::TypeIdentifier&) { return false; };

        resolveType(context, classInstance->location, classInstance->typeArgument, checkLocalFun, false);

        for (const auto& function : classInstance->functions)
        {
            resolveFunction(context, *function);
        }
    }

    context.errs.flush();
    if (!errorStr.empty())
    {
        llvm::errs() << errorStr;
        return false;
    }
    return true;
}

} // namespace llfp
