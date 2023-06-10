
#include "Utils/DotFileWriter.h"

#include <llvm/ADT/Twine.h>

namespace llfp::utils::dot
{

namespace
{

using counter_t = unsigned int;

template<typename F>
counter_t nodeLabelF(llvm::raw_ostream& os, counter_t& counter, F label)
{
    const auto id = counter++;
    os << 'n' << id << "[label=\"";
    label(os);
    os << "\"];\n";
    return id;
}

counter_t nodeLabel(llvm::raw_ostream& os, counter_t& counter, llvm::Twine label)
{
    const auto id = counter++;
    os << 'n' << id << "[label=\"" << label << "\"];\n";
    return id;
}

void connectNodes(llvm::raw_ostream& os, counter_t id1, counter_t id2)
{
    os << 'n' << id1 << " -> n" << id2 << ";\n";
}

class DotVisitor
{
public:

    llvm::raw_ostream& os_;
    counter_t&         counter_;

    DotVisitor(llvm::raw_ostream& os, counter_t& counter)
        : os_{ os },
          counter_{ counter }
    {}

    counter_t node(llvm::Twine label)
    {
        return nodeLabel(os_, counter_, label);
    }

    void connect(counter_t id1, counter_t id2)
    {
        return connectNodes(os_, id1, id2);
    }
};

class DotPatternVisitor : public ast::PatternVisitor, public DotVisitor
{
public:

    DotPatternVisitor(llvm::raw_ostream& os, counter_t& counter)
        : DotVisitor(os, counter)
    {}

    virtual void visit(ast::BoolPattern& pattern)
    {
        node(pattern.value ? "true" : "false");
    }

    virtual void visit(ast::IdentifierPattern& pattern)
    {
        node(pattern.value);
    }

    virtual void visit(ast::IntegerPattern& pattern)
    {
        node(pattern.value);
    }

    virtual void visit(ast::FloatPattern& pattern)
    {
        node(pattern.value);
    }

    virtual void visit(ast::CharPattern& pattern)
    {
        node(pattern.value);
    }

    virtual void visit(ast::StringPattern& pattern)
    {
        node(pattern.value);
    }

    virtual void visit(ast::ConstructorPattern& pattern)
    {
        const auto id = node(pattern.identifier.str() + "{}");
        for (auto& arg : pattern.arguments)
        {
            const auto argId = counter_;
            arg.pattern->accept(this);
            connect(id, argId);
        }
    }
};

class DotExpVisitor : public ast::ExpVisitor, public DotVisitor
{
public:

    DotExpVisitor(llvm::raw_ostream& os, counter_t& counter)
        : DotVisitor(os, counter)
    {}

    counter_t loop(const std::unique_ptr<ast::Exp>& exp)
    {
        const auto id = counter_;
        exp->accept(this);
        return id;
    }

    counter_t loop(const std::unique_ptr<ast::Pattern>& pattern)
    {
        const auto        id = counter_;
        DotPatternVisitor visitor{ os_, counter_ };
        pattern->accept(&visitor);
        return id;
    }

    virtual void visit(ast::LetExp& exp)
    {
        const auto id = node("let");

        for (auto& letStm : exp.letStatments)
        {
            const auto varId = node(llvm::Twine{ letStm->name } + " =");
            connect(varId, loop(letStm->functionBody));
            connect(id, varId);
        }

        const auto inId = node("in");
        connect(inId, loop(exp.exp));
        connect(id, inId);
    }

    virtual void visit(ast::IfExp& exp)
    {
        const auto id = node("if");
        connect(id, loop(exp.condition));
        connect(id, loop(exp.thenExp));
        connect(id, loop(exp.elseExp));
    }

    virtual void visit(ast::CaseExp& exp)
    {
        const auto id = node("case");

        connect(id, loop(exp.caseExp));

        for (auto& clause : exp.clauses)
        {
            const auto clauseId = node("clause");
            connect(clauseId, loop(clause.pattern));
            connect(clauseId, loop(clause.exp));
            connect(id, clauseId);
        }
    }

    virtual void visit(ast::BinaryExp& exp)
    {
        const auto id = node(exp.op);
        connect(id, loop(exp.lhs));
        connect(id, loop(exp.rhs));
    }

    virtual void visit(ast::UnaryExp& exp)
    {
        const auto id = node(exp.op);
        connect(id, loop(exp.operand));
    }

    virtual void visit(ast::LiteralExp& exp)
    {
        node(exp.value);
    }

    virtual void visit(ast::CallExp& exp)
    {
        const auto id = node(llvm::Twine{ "call " } + exp.identifier.str());
        for (auto& arg : exp.arguments)
        {
            connect(id, loop(arg));
        }
    }

    virtual void visit(ast::VariableExp& exp)
    {
        node(exp.identifier.str());
    }

    virtual void visit(ast::FieldExp& exp)
    {
        const auto id = node(llvm::Twine{ '.' } + exp.fieldIdentifier);
        connect(id, loop(exp.lhs));
    }

    virtual void visit(ast::ConstructorExp& exp)
    {
        const auto id = node(llvm::Twine{ exp.identifier.str() } + "{}");
        for (auto& arg : exp.arguments)
        {
            connect(id, loop(arg.exp));
        }
    }

    virtual void visit(ast::IntrinsicExp& exp)
    {
        const auto id = node(exp.identifier_);
        for (auto& arg : exp.arguments_)
        {
            connect(id, loop(arg));
        }
    }
};

} // namespace

void writeDotFile(llvm::raw_ostream& os, const llfp::ast::Module& module)
{
    counter_t counter = 0;

    os << "digraph G {\n"

       << "module [label=\"" << module.name << "\"];\n"

       << "publics [shape=\"record\" label=<<B>publics</B>";
    for (auto& p : module.publics)
    {
        os << "|" << p.name;
    }
    os << ">]\n"
          "module -> publics;\n"

          "imports [shape=\"record\" label=<<B>imports</B>";
    for (auto& i : module.imports)
    {
        os << '|' << i.name;
    }
    os << ">]\n"
          "module -> imports;\n"

          "module -> datas\n";
    for (auto& data : module.datas)
    {
        const auto dataId =
            nodeLabelF(os, counter,
                       [&data](auto& os) {
                           os << data->name << '<';
                           for (auto& typeVar : data->typeVariables)
                           {
                               os << typeVar << ',';
                           }
                           os << '>';
                       });

        for (auto& constructor : data->constructors)
        {
            const auto constId = nodeLabel(os, counter, constructor.name);
            connectNodes(os, dataId, constId);
            for (auto& field : constructor.fields)
            {
                const auto fieldId = nodeLabel(os, counter, llvm::Twine{ field.type.identifier.name } + " " + field.name);
                connectNodes(os, constId, fieldId);
            }
        }

        os << "datas -> n" << dataId << ";\n";
    }

    os << "module -> functions;\n";
    for (auto& func : module.functions)
    {
        const auto groupId = counter++;
        const auto funId   = counter;
        os << "subgraph cluster" << groupId << "{\n";
        {
            (void)nodeLabelF(os, counter,
                             [&func](auto& os) {
                                 os << func->type.str() << ' ' << func->name << '(';
                                 for (auto& arg : func->parameters)
                                 {
                                     os << arg->type.str() << ' ' << arg->identifier << ", ";
                                 }
                                 os << ')';
                             });

            const auto    bodyId = counter;
            DotExpVisitor visitor{ os, counter };
            func->functionBody->accept(&visitor);

            connectNodes(os, funId, bodyId);
        }
        os << "}\n"
           << "functions -> n" << funId << ";\n";
    }

    os << "module -> classes;\n";
    for (auto& c : module.classes)
    {
        const auto classId = nodeLabel(os, counter, llvm::Twine{ c->name } + " " + c->typeVariable);
        for (auto& func : c->functions)
        {
            const auto funcId =
                nodeLabelF(os, counter,
                           [&func](auto& os) {
                               os << func->type.str() << " " << func->name << '(';
                               for (auto& arg : func->parameters)
                               {
                                   os << arg->type.str() << ' ' << arg->identifier << ", ";
                               }
                               os << ')';
                           });
            connectNodes(os, classId, funcId);
        }
        os << "classes -> n" << classId << ";\n";
    }

    os << "module -> classInstances;\n";
    for (auto& c : module.classInstances)
    {
        const auto instId = nodeLabel(os, counter, llvm::Twine{ c->classIdentifier.str() } + "<" + c->typeArgument.str() + ">");
        for (auto& func : c->functions)
        {
            const auto groupId = counter++;
            const auto funId   = counter;
            os << "subgraph cluster" << groupId << "{\n";
            {
                (void)nodeLabelF(os, counter,
                                 [&func](auto& os) {
                                     os << func->type.str() << " " << func->name << '(';
                                     for (auto& arg : func->parameters)
                                     {
                                         os << arg->type.str() << ' ' << arg->identifier << ", ";
                                     }
                                     os << ')';
                                 });
                const auto    bodyId = counter;
                DotExpVisitor visitor{ os, counter };
                func->functionBody->accept(&visitor);

                connectNodes(os, funId, bodyId);
            }
            os << "}\n"
               << "n" << instId << " -> n" << funId << ";\n";
        }
        os << "classInstances -> n" << instId << ";\n";
    }

    os << "}\n";
}

} // namespace llfp::utils::dot
