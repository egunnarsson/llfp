
#include "Lexer.h"
#include "Parser.h"

#include "AstEquality.h"
#include "AstTinyPrint.h"

#include "gtest/gtest.h"

#include <memory>
#include <type_traits>


// https://stackoverflow.com/questions/46737054/vectorunique-ptra-using-initialization-list
template<class T>
struct movable_il
{
    mutable T t;
              operator T() const&& { return std::move(t); }
    movable_il(T&& in)
        : t(std::move(in))
    {}
};

template<class T, class A = std::allocator<T>>
std::vector<T, A> vector_from_il(std::initializer_list<movable_il<T>> il)
{
    std::vector<T, A> r(std::make_move_iterator(il.begin()), std::make_move_iterator(il.end()));
    return r;
}

template<class T>
using uptr_il = std::initializer_list<movable_il<std::unique_ptr<T>>>;

// So that we can print the root node properly
struct ModulePtr
{
    std::unique_ptr<llfp::ast::Module> ptr;

    ModulePtr(std::unique_ptr<llfp::ast::Module>&& ptr_)
        : ptr{ std::move(ptr_) }
    {}
    ModulePtr(llfp::SourceLocation location = { 0, 0 }, std::string name = "m")
    {
        ptr = std::make_unique<llfp::ast::Module>(location, std::move(name));
    }

    bool operator==(const ModulePtr& other) const
    {
        if (ptr == nullptr) return other.ptr == nullptr;
        if (other.ptr == nullptr) return false;
        return *ptr == *other.ptr;
    }

    ModulePtr& publics(std::vector<llfp::ast::Public> f)
    {
        ptr->publics = std::move(f);
        return *this;
    }

    ModulePtr& imports(std::vector<llfp::ast::Import> f)
    {
        ptr->imports = std::move(f);
        return *this;
    }

    ModulePtr& datas(uptr_il<llfp::ast::Data> d)
    {
        ptr->datas = vector_from_il(d);
        return *this;
    }

    ModulePtr& functions(uptr_il<llfp::ast::Function> funs)
    {
        ptr->functions = vector_from_il(funs);
        return *this;
    }

    ModulePtr& classes(uptr_il<llfp::ast::Class> c)
    {
        ptr->classes = vector_from_il(c);
        return *this;
    }

    ModulePtr& instances(uptr_il<llfp::ast::ClassInstance> i)
    {
        ptr->classInstances = vector_from_il(i);
        return *this;
    }
};

std::ostream& operator<<(std::ostream& os, const ModulePtr& ptr)
{
    return os << ptr.ptr;
}

auto Parse(const char* string)
{
    auto input  = llfp::Source("string", string);
    auto lexer  = llfp::lex::Lexer(&input);
    auto parser = llfp::parse::Parser(&lexer);
    return ModulePtr(parser.parse());
}

std::string ParseError(const char* string)
{
    testing::internal::CaptureStderr();
    Parse(string);
    const auto msg = testing::internal::GetCapturedStderr();
    if (msg.size() < 2)
    {
        return {};
    }
    const auto pos = msg.find_last_of('\n', msg.size() - 2);
    if (pos == std::string::npos)
    {
        return {};
    }
    return msg.substr(pos + 1);
}

auto TypeId(std::string moduleName, std::string name, std::vector<llfp::ast::TypeIdentifier> parameters)
{
    return llfp::ast::TypeIdentifier{ llfp::GlobalIdentifier{ std::move(moduleName), std::move(name) }, std::move(parameters) };
}

auto DataDecl(llfp::SourceLocation location, bool exported, std::string name, std::vector<llfp::ast::Field> fields)
{
    llfp::ast::DataConstructor constructor{ {}, "", std::move(fields) };
    return std::make_unique<llfp::ast::Data>(location, std::move(name), std::vector<std::string>{}, std::vector<llfp::ast::DataConstructor>{ std::move(constructor) }, exported);
}

auto Function(
    llfp::SourceLocation            location,
    bool                            exported,
    std::string                     name,
    std::string                     typeName,
    uptr_il<llfp::ast::Parameter>   parameters,
    std::unique_ptr<llfp::ast::Exp> functionBody)
{
    return std::make_unique<llfp::ast::Function>(location, std::move(name), llfp::ast::TypeIdentifier{ llfp::GlobalIdentifier{ "", std::move(typeName) }, {} }, vector_from_il(parameters), std::move(functionBody), exported);
}

auto Function(
    llfp::SourceLocation            location,
    bool                            exported,
    std::string                     name,
    llfp::ast::TypeIdentifier       typeName,
    uptr_il<llfp::ast::Parameter>   parameters,
    std::unique_ptr<llfp::ast::Exp> functionBody)
{
    return std::make_unique<llfp::ast::Function>(location, std::move(name), std::move(typeName), vector_from_il(parameters), std::move(functionBody), exported);
}

auto Parameter(llfp::SourceLocation location, std::string typeName, std::string name)
{
    return std::make_unique<llfp::ast::Parameter>(location, TypeId("", std::move(typeName), {}), std::move(name));
}

auto Parameter(llfp::SourceLocation location, llfp::ast::TypeIdentifier typeName, std::string name)
{
    return std::make_unique<llfp::ast::Parameter>(location, std::move(typeName), std::move(name));
}

std::unique_ptr<llfp::ast::Exp> Literal(llfp::SourceLocation location, llfp::lex::Token tokenType, std::string value)
{
    return std::make_unique<llfp::ast::LiteralExp>(location, tokenType, std::move(value));
}

std::unique_ptr<llfp::ast::Exp> Integer(llfp::SourceLocation location, std::string value)
{
    return Literal(location, llfp::lex::Token::Integer, std::move(value));
}

std::unique_ptr<llfp::ast::Exp> Let(llfp::SourceLocation location, uptr_il<llfp::ast::Function> letStatments, std::unique_ptr<llfp::ast::Exp> exp)
{
    return std::make_unique<llfp::ast::LetExp>(location, vector_from_il(letStatments), std::move(exp));
}

std::unique_ptr<llfp::ast::Exp> If(llfp::SourceLocation location, std::unique_ptr<llfp::ast::Exp> condition, std::unique_ptr<llfp::ast::Exp> thenExp, std::unique_ptr<llfp::ast::Exp> elseExp)
{
    return std::make_unique<llfp::ast::IfExp>(location, std::move(condition), std::move(thenExp), std::move(elseExp));
}

std::unique_ptr<llfp::ast::Exp> Case(llfp::SourceLocation location, std::unique_ptr<llfp::ast::Exp> caseExp, std::initializer_list<movable_il<llfp::ast::Clause>> clauses)
{
    return std::make_unique<llfp::ast::CaseExp>(location, std::move(caseExp), vector_from_il(clauses));
}

std::unique_ptr<llfp::ast::Exp> Binary(llfp::SourceLocation location, std::string op, std::unique_ptr<llfp::ast::Exp> lhs, std::unique_ptr<llfp::ast::Exp> rhs)
{
    return std::make_unique<llfp::ast::BinaryExp>(location, std::move(op), std::move(lhs), std::move(rhs));
}

std::unique_ptr<llfp::ast::Exp> Unary(llfp::SourceLocation location, std::string op, std::unique_ptr<llfp::ast::Exp> operand)
{
    return std::make_unique<llfp::ast::UnaryExp>(location, std::move(op), std::move(operand));
}

std::unique_ptr<llfp::ast::Exp> Call(llfp::SourceLocation location, std::string moduleName, std::string name, uptr_il<llfp::ast::Exp> args)
{
    return std::make_unique<llfp::ast::CallExp>(location, llfp::GlobalIdentifier{ std::move(moduleName), std::move(name) }, vector_from_il(args));
}

std::unique_ptr<llfp::ast::Exp> Variable(llfp::SourceLocation location, std::string moduleName, std::string name)
{
    return std::make_unique<llfp::ast::VariableExp>(location, llfp::GlobalIdentifier{ std::move(moduleName), std::move(name) });
}

std::unique_ptr<llfp::ast::Exp> Field(llfp::SourceLocation location, std::unique_ptr<llfp::ast::Exp> lhs, std::string fieldName)
{
    return std::make_unique<llfp::ast::FieldExp>(location, std::move(lhs), std::move(fieldName));
}

std::unique_ptr<llfp::ast::Exp> Constructor(llfp::SourceLocation location, llfp::GlobalIdentifier name, std::initializer_list<movable_il<llfp::ast::NamedArgument>> args)
{
    return std::make_unique<llfp::ast::ConstructorExp>(location, std::move(name), vector_from_il(args));
}

std::unique_ptr<llfp::ast::Exp> Intrinsic(llfp::SourceLocation location, std::string name, uptr_il<llfp::ast::Exp> args)
{
    return std::make_unique<llfp::ast::IntrinsicExp>(location, std::move(name), vector_from_il(args));
}

auto Class(llfp::SourceLocation location, std::string name, std::string typeVar, uptr_il<llfp::ast::FunctionDeclaration> funs)
{
    return std::make_unique<llfp::ast::Class>(location, std::move(name), std::move(typeVar), vector_from_il(funs));
}

auto Instance(llfp::SourceLocation location, llfp::GlobalIdentifier id, llfp::ast::TypeIdentifier typeArg, uptr_il<llfp::ast::Function> funs)
{
    return std::make_unique<llfp::ast::ClassInstance>(location, std::move(id), std::move(typeArg), vector_from_il(funs));
}

auto FunctionDecl(llfp::SourceLocation location, llfp::ast::TypeIdentifier type, std::string name, uptr_il<llfp::ast::Parameter> params)
{
    return std::make_unique<llfp::ast::FunctionDeclaration>(location, std::move(name), std::move(type), vector_from_il(params));
}

auto BoolPattern(llfp::SourceLocation location, bool value)
{
    return std::make_unique<llfp::ast::BoolPattern>(location, value);
}

auto IdentifierPattern(llfp::SourceLocation location, std::string value)
{
    return std::make_unique<llfp::ast::IdentifierPattern>(location, value);
}

auto IntegerPattern(llfp::SourceLocation location, std::string value)
{
    return std::make_unique<llfp::ast::IntegerPattern>(location, value);
}

auto FloatPattern(llfp::SourceLocation location, std::string value)
{
    return std::make_unique<llfp::ast::FloatPattern>(location, value);
}

auto CharPattern(llfp::SourceLocation location, std::string value)
{
    return std::make_unique<llfp::ast::CharPattern>(location, value);
}

auto StringPattern(llfp::SourceLocation location, std::string value)
{
    return std::make_unique<llfp::ast::StringPattern>(location, value);
}

auto ConstructorPattern(llfp::SourceLocation location, llfp::GlobalIdentifier gid, std::initializer_list<movable_il<llfp::ast::NamedArgumentPattern>> args)
{
    return std::make_unique<llfp::ast::ConstructorPattern>(location, std::move(gid), vector_from_il(args));
}

namespace
{
constexpr bool Local    = false;
constexpr bool Exported = true;
} // namespace

/**
Make a module m with a function f with exp.
*/
ModulePtr m_f(std::unique_ptr<llfp::ast::Exp> exp)
{
    return std::move(ModulePtr().functions({ Function({ 0, 0 }, Local, "f", "", {}, std::move(exp)) }));
}

// clang-format off

TEST(ParserTest, EmptyInput)
{
    EXPECT_EQ(ParseError(""), "string(1,1): expected 'module'\n");
}

TEST(ParserTest, Imports)
{
    // no imports
    EXPECT_EQ(Parse("module m;"), ModulePtr({ 0,0 }, "m"));

    // one import
    EXPECT_EQ(Parse("module m;"
                    "import m2;"),
        ModulePtr({ 0,0 }, "m").imports({ {{0,0},"m2"} }));

    // multiple imports
    EXPECT_EQ(Parse("module m;"
                    "import m2;"
                    "import m3;"),
        ModulePtr({ 0,0 }, "m").imports({ {{0,0},"m2"}, {{0,0},"m3"} }));

    // from, select symbols

    // negative
    EXPECT_EQ(ParseError("module m\nf = 1;"),  "string(2,1): expected 'semicolon'\n");
    EXPECT_EQ(ParseError("module m:a;"),       "string(1,9): expected 'semicolon'\n");
    EXPECT_EQ(ParseError("f = 1; module m;"),  "string(1,1): expected 'module'\n");
    EXPECT_EQ(ParseError("module m; import;"), "string(1,17): expected an identifier\n");
}

TEST(ParserTest, PublicDeclarations)
{
    // no decls
    EXPECT_EQ(Parse("module m;"), ModulePtr({ 0,0 }, "m"));
    EXPECT_EQ(Parse("module m();"), ModulePtr({ 0,0 }, "m"));

    // one decls
    EXPECT_EQ(Parse("module m(x);"), ModulePtr({ 0,0 }, "m").publics({ {{0,0}, "x"} }));

    // multiple decls
    EXPECT_EQ(Parse("module m(x,y);"), ModulePtr({ 0,0 }, "m").publics({ {{0,0}, "x"},{{0,0}, "y"} }));

    // negative
    EXPECT_EQ(ParseError("module m(x y);"), "string(1,12): expected 'comma'\n");
}

#define M "module m;\n"

TEST(ParserTest, DataDeclarations)
{
    EXPECT_EQ(Parse(M"data a={};"), ModulePtr().datas({ DataDecl({ 0,0 }, Local, "a", {}) }));
    EXPECT_EQ(Parse(M"data a={t x;};"), ModulePtr().datas({ DataDecl({ 0,0 }, Local, "a", { llfp::ast::Field({ 0,0 }, {{"", "t"}, {}}, "x") }) }));
    EXPECT_EQ(Parse(M"data a={m2:t x;};"), ModulePtr().datas({ DataDecl({ 0,0 }, Local, "a",{ llfp::ast::Field({ 0,0 }, {{ "m2", "t" }, {}}, "x") }) }));
    EXPECT_EQ(Parse(M"data a={t1 x; t2 y;};"),
        ModulePtr().datas({
            DataDecl({ 0,0 }, Local, "a", {
                llfp::ast::Field({ 0,0 }, {{"", "t1"},{}}, "x"),
                llfp::ast::Field({ 0,0 }, {{"", "t2"},{}}, "y") }) }));
    EXPECT_EQ(Parse(M"export data a={};"), ModulePtr().datas({ DataDecl({ 0,0 }, Exported, "a",{}) }));

    // negative
    EXPECT_EQ(ParseError(M"data a={x;};"),      "string(2,10): expected an identifier\n");
    EXPECT_EQ(ParseError(M"data a{t x;};"),     "string(2,7): expected 'equal'\n");
    EXPECT_EQ(ParseError(M"data a={x};"),       "string(2,10): expected an identifier\n");
    EXPECT_EQ(ParseError(M"data a={t x};"),     "string(2,12): expected 'semicolon'\n");
    EXPECT_EQ(ParseError(M"data a={t x; y;};"), "string(2,15): expected an identifier\n");
    EXPECT_EQ(ParseError(M"data a={t x y;};"),  "string(2,13): expected 'semicolon'\n");
    EXPECT_EQ(ParseError(M"data{t x;};"),       "string(2,5): expected an identifier\n");
    EXPECT_EQ(ParseError(M"data a={t x;}"),     "string(2,14): expected 'comma'\n");
}

TEST(ParserTest, DataConstructor)
{
    EXPECT_EQ(Parse(M"f = a{};"), m_f(Constructor({ 0,0 }, { "", "a" }, {})));
    EXPECT_EQ(Parse(M"f = a{1};"), m_f(Constructor({ 0,0 }, { "", "a" }, { llfp::ast::NamedArgument({0,0}, "", Integer({0,0}, "1")) })));
    EXPECT_EQ(Parse(M"f = a{x};"), m_f(Constructor({ 0,0 }, { "", "a" }, { llfp::ast::NamedArgument({0,0}, "", Variable({0,0}, "", "x")) })));
    EXPECT_EQ(Parse(M"f = a{x:y};"), m_f(Constructor({ 0,0 }, { "", "a" }, { llfp::ast::NamedArgument({ 0,0 }, "", Variable({ 0,0 }, "x", "y")) })));
    EXPECT_EQ(Parse(M"f = a{x = y};"), m_f(Constructor({ 0,0 }, { "", "a" }, { llfp::ast::NamedArgument({ 0,0 }, "x", Variable({ 0,0 }, "", "y")) })));
    EXPECT_EQ(Parse(M"f = a{x()};"), m_f(Constructor({ 0,0 }, { "", "a" }, { llfp::ast::NamedArgument({ 0,0 }, "", Call({0,0},"", "x", {})) })));
    EXPECT_EQ(Parse(M"f = a{x + 1};"), m_f(
        Constructor({ 0,0 }, { "", "a" }, { llfp::ast::NamedArgument({ 0,0 }, "",
            Binary({ 0,0 }, "+",
                Variable({ 0,0 }, "", "x"),
                Integer({ 0,0 }, "1"))) })));
    EXPECT_EQ(Parse(M"f = a{1, x};"), m_f(Constructor({ 0,0 }, { "", "a" }, {
        llfp::ast::NamedArgument({ 0,0 }, "", Integer({ 0,0 }, "1")),
        llfp::ast::NamedArgument({ 0,0 }, "", Variable({ 0,0 }, "", "x")) })));
    EXPECT_EQ(Parse(M"f = a:b{1};"), m_f(Constructor({ 0,0 }, { "a", "b" }, { llfp::ast::NamedArgument({ 0,0 }, "", Integer({ 0,0 }, "1")) })));
    EXPECT_EQ(Parse(M"f = a{b{1}};"), m_f(
        Constructor({ 0,0 }, { "", "a" }, { llfp::ast::NamedArgument({ 0,0 }, "",
            Constructor({ 0,0 }, { "", "b" }, { llfp::ast::NamedArgument({ 0,0 }, "",
                Integer({ 0,0 }, "1")) })) })));
    EXPECT_EQ(Parse(M"f = a{foo() * x};"), m_f(
        Constructor({ 0,0 }, { "", "a" }, {
            llfp::ast::NamedArgument({ 0,0 }, "",
                Binary({ 0,0 }, "*",
                    Call({ 0,0 }, "", "foo",{}),
                    Variable({ 0,0 }, "", "x")))})));
    EXPECT_EQ(Parse(M"f = a{x = foo() * y};"), m_f(
        Constructor({ 0,0 }, { "", "a" }, {
            llfp::ast::NamedArgument({ 0,0 }, "x",
                Binary({ 0,0 }, "*",
                    Call({ 0,0 }, "", "foo", {}),
                    Variable({ 0,0 }, "", "y")))})));

    // negative
    EXPECT_EQ(ParseError(M"f = a{x:}"),             "string(2,9): expected an identifier\n");
    EXPECT_EQ(ParseError(M"f = a{x: = 1}"),         "string(2,10): expected an identifier\n");
    EXPECT_EQ(ParseError(M"f = a{x=}"),             "string(2,9): expected an expression\n");
    EXPECT_EQ(ParseError(M"f = a{x + 1 = foo()};"), "string(2,13): expected 'comma'\n");
    EXPECT_EQ(ParseError(M"f = a{x:y = 1};"),       "string(2,7): expected a data member identifier\n");
}

TEST(ParserTest, Functions)
{
    // untyped function
    EXPECT_EQ(Parse(M"f = 1;"), m_f(Integer({0,0}, "1")));

    // no params
    EXPECT_EQ(Parse(M"t f = 1;"),
        ModulePtr().functions({Function({0,0}, Local, "f", "t", {}, Integer({0,0}, "1")) }));
    EXPECT_EQ(Parse(M"m2:t f = 1;"),
        ModulePtr().functions({Function({ 0,0 }, Local, "f", TypeId("m2", "t", {}), {}, Integer({ 0,0 }, "1")) }));

    // one params
    EXPECT_EQ(Parse(M"t f(x) = 1;"),
        ModulePtr().functions({
            Function({0,0}, Local, "f", "t",{Parameter({0,0}, "", "x")},
                Integer({0,0}, "1")) }));

    // multiple params
    EXPECT_EQ(Parse(M"t f(x,y) = 1;"),
        ModulePtr().functions({
            Function({0,0}, Local, "f", "t", {
                Parameter({0,0}, "", "x"),
                Parameter({0,0}, "", "y")},
                Integer({0,0}, "1")) }));

    // typed params
    EXPECT_EQ(Parse(M"t f(t x) = 1;"),
        ModulePtr().functions({
            Function({0,0}, Local, "f", "t", { Parameter({0,0}, "t", "x")},
                Integer({0,0}, "1")) }));
    EXPECT_EQ(Parse(M"t f(t x, t2 y) = 1;"),
        ModulePtr().functions({
            Function({0,0}, Local, "f", "t", {
                Parameter({0,0}, "t", "x"),
                Parameter({0,0}, "t2", "y")},
                Integer({0,0}, "1")) }));
    EXPECT_EQ(Parse(M"t f(m:t x) = 1;"),
        ModulePtr().functions({
            Function({ 0,0 }, Local, "f", "t", { Parameter({ 0,0 }, TypeId("m", "t", {}), "x") },
                Integer({ 0,0 }, "1")) }));

    // parameterized types
    EXPECT_EQ(Parse(M"t[x] f() = 1;"),
        ModulePtr().functions({
            Function({ 0,0 }, Local, "f", TypeId("", "t", { TypeId("", "x", {}) }), {},
            Integer({ 0,0 }, "1")) }));
    EXPECT_EQ(Parse(M"f(x[y] z) = 1;"),
        ModulePtr().functions({
            Function({ 0,0 }, Local, "f", "", { Parameter({0,0}, TypeId("", "x", { TypeId("", "y", {}) }), "z") },
            Integer({ 0,0 }, "1")) }));
    EXPECT_EQ(Parse(M"x[y[z]] f() = 1;"),
        ModulePtr().functions({
            Function({ 0,0 }, Local, "f", TypeId("", "x", { TypeId("", "y", { TypeId("", "z", {}) }) }), {},
            Integer({ 0,0 }, "1")) }));
    EXPECT_EQ(Parse(M"x[y,z] f() = 1;"),
        ModulePtr().functions({
            Function({ 0,0 }, Local, "f", TypeId("", "x", { TypeId("", "y", {}), TypeId("", "z", {}) }), {},
            Integer({ 0,0 }, "1")) }));

    // negative
    EXPECT_EQ(ParseError(M"f = ;"),        "string(2,5): expected an expression\n");
    EXPECT_EQ(ParseError(M"m2: f = 1;"),   "string(2,7): expected an identifier\n");
    EXPECT_EQ(ParseError(M"f(x[]) = 1;"),  "string(2,4): empty type list\n");
    EXPECT_EQ(ParseError(M"f(x[b]) = 1;"), "string(2,7): expected an identifier\n");
    EXPECT_EQ(ParseError(M"a[b]() = 1;"),  "string(2,5): expected an identifier\n");
    EXPECT_EQ(ParseError(M"a[] b() = 1;"), "string(2,2): empty type list\n");
}

TEST(ParserTest, Declarations)
{
    EXPECT_EQ(Parse(
        M"data a={};"
         "f = 1;"
         "data b={};"
         "g = 2;"),
        ModulePtr()
        .functions({ Function({ 0,0 }, Local, "f", "", {}, Integer({ 0,0 }, "1")),
                     Function({ 0,0 }, Local, "g", "", {}, Integer({ 0,0 }, "2")) })
        .datas({ DataDecl({ 0,0 }, Local, "a",{}),
                 DataDecl({ 0,0 }, Local, "b",{}) }));

    // negative
}

TEST(ParserTest, Literals)
{
    // integer
    EXPECT_EQ(Parse(M"f = 1;"), m_f(Integer({0,0}, "1")));
    // float
    EXPECT_EQ(Parse(M"f = 1.0;"), m_f(Literal({0,0}, llfp::lex::Token::Float, "1.0")));
    // char
    EXPECT_EQ(Parse(M"f = \'c\';"), m_f(Literal({0,0}, llfp::lex::Token::Char, "c")));
    // string
    EXPECT_EQ(Parse(M"f = \"ac\";"), m_f(Literal({0,0}, llfp::lex::Token::String, "ac")));
    // bool
    EXPECT_EQ(Parse(M"f = true;"), m_f(Literal({0,0}, llfp::lex::Token::Bool, "true")));
    EXPECT_EQ(Parse(M"f = false;"), m_f(Literal({0,0}, llfp::lex::Token::Bool, "false")));

    // negative
    // invalid string escape
}

TEST(ParserTest, LetExp)
{
    EXPECT_EQ(Parse(M"f = let x = 1; in 2;"),
        m_f(Let({ 0,0 }, {
                Function({0,0}, Local, "x", "", {},
                    Integer({0,0}, "1")) },
                    Integer({ 0,0 }, "2"))));
    EXPECT_EQ(Parse(M"f = let x = 1; y = 2; in 3;"),
        m_f(Let({ 0,0 }, {
                Function({0,0}, Local, "x", "", {},
                    Integer({0,0}, "1")),
                Function({0,0}, Local, "y", "", {},
                    Integer({0,0}, "2")) },
                    Integer({ 0,0 }, "3"))));

    // nested
    EXPECT_EQ(Parse(M"f = let\n"
        "       x = 1;\n"
        "       y = let\n"
        "               a = 2;\n"
        "           in 3;\n"
        "    in let\n"
        "           z = let\n"
        "                   b = 4;\n"
        "               in 5;\n"
        "           w = 6;\n"
        "       in 7;\n"),
        m_f(Let({ 0,0 }, {
                Function({0,0}, Local, "x", "", {}, Integer({0,0}, "1")),
                Function({0,0}, Local, "y", "", {},
                    Let({0,0}, {
                        Function({0,0}, Local, "a", "", {}, Integer({0,0}, "2"))},
                    Integer({0,0}, "3"))) },
                    Let({ 0,0 }, {
                        Function({0,0}, Local, "z", "", {},
                            Let({0,0}, {
                                Function({0,0}, Local, "b", "", {}, Integer({0,0}, "4"))},
                            Integer({0,0}, "5"))),
                        Function({0,0}, Local, "w", "", {}, Integer({0,0}, "6")) },
                        Integer({ 0,0 }, "7")))));

    // negative
    EXPECT_EQ(ParseError(M"f = let x = 1; 2;"),          "string(2,16): expected an identifier\n");
    EXPECT_EQ(ParseError(M"f = let x = 1; y 2;"),        "string(2,18): expected 'equal'\n");
    EXPECT_EQ(ParseError(M"f = let x = 1 y = 2; in 3;"), "string(2,15): expected 'semicolon'\n");
}

TEST(ParserTest, IfExp)
{
    EXPECT_EQ(Parse(M"f = if x then y else z;"),
        m_f(If({ 0,0 },
                Variable({ 0,0 }, "", "x"),
                Variable({ 0,0 }, "", "y"),
                Variable({ 0,0 }, "", "z"))));

    // negative
    EXPECT_EQ(ParseError(M"f = if x,a then y else z;"), "string(2,9): expected 'then'\n");
}

TEST(ParserTest, CaseExp)
{
    EXPECT_EQ(Parse(
        M"f() = case 0 of\n"
        "   true  -> 1,\n"
        "   id    -> 2,\n"
        "   11    -> 3,\n"
        "   1.1   -> 4,\n"
        "   'a'   -> 5,\n"
        "   \"s\" -> 6,\n"
        "   c{9}  -> 7\n"
        "end;\n"),
        m_f(Case({ 0,0 }, Integer({ 0,0 }, "0"),
            {
                llfp::ast::Clause{BoolPattern({0,0},true),       Integer({0,0}, "1")},
                llfp::ast::Clause{IdentifierPattern({0,0},"id"), Integer({0,0}, "2")},
                llfp::ast::Clause{IntegerPattern({0,0}, "11"),   Integer({0,0}, "3")},
                llfp::ast::Clause{FloatPattern({0,0}, "1.1"),    Integer({0,0}, "4")},
                llfp::ast::Clause{CharPattern({0,0}, "a"),       Integer({0,0}, "5")},
                llfp::ast::Clause{StringPattern({0,0}, "s"),     Integer({0,0}, "6")},
                llfp::ast::Clause{ConstructorPattern({0,0}, {"","c"}, { llfp::ast::NamedArgumentPattern({0,0}, "", IntegerPattern({0,0}, "9")) }), Integer({0,0}, "7")}
            })));

    // nested
    EXPECT_EQ(Parse(
        M"f = "
        "   case x of\n"
        "       1 ->\n"
        "           case y of\n"
        "               2 -> 3,\n"
        "               4 -> 5\n"
        "           end,\n"
        "       6 -> 7\n"
        "   end;\n"),
        m_f(Case({ 0,0 }, Variable({ 0,0 }, "", "x"), {
                        llfp::ast::Clause{ IntegerPattern({0,0}, "1"),
                            Case({0,0}, Variable({0,0}, "", "y"), {
                                llfp::ast::Clause{ IntegerPattern({0,0}, "2"),
                                    Integer({0,0}, "3")},
                                llfp::ast::Clause{ IntegerPattern({0,0}, "4"),
                                    Integer({0,0}, "5")}}) },
                        llfp::ast::Clause{ IntegerPattern({0,0}, "6"),
                            Integer({0,0}, "7") }
            })));
}

TEST(ParserTest, BinaryExp)
{
    EXPECT_EQ(Parse(M"f = x + y;"),
        m_f(Binary({ 0,0 }, "+",
            Variable({ 0,0 }, "", "x"),
            Variable({ 0,0 }, "", "y"))));
    EXPECT_EQ(Parse(M"f = x + y + z;"),
        m_f(Binary({ 0,0 }, "+",
            Binary({ 0,0 }, "+",
                Variable({ 0,0 }, "", "x"),
                Variable({ 0,0 }, "", "y")),
            Variable({ 0,0 }, "", "z"))));
    EXPECT_EQ(Parse(M"f = x + y * z;"),
        m_f(Binary({ 0,0 }, "+",
            Variable({ 0,0 }, "", "x"),
            Binary({ 0,0 }, "*",
                Variable({ 0,0 }, "", "y"),
                Variable({ 0,0 }, "", "z")))));
    EXPECT_EQ(Parse(M"f = x * y + z;"),
        m_f(Binary({ 0,0 }, "+",
            Binary({ 0,0 }, "*",
                Variable({ 0,0 }, "", "x"),
                Variable({ 0,0 }, "", "y")),
            Variable({ 0,0 }, "", "z"))));

    EXPECT_EQ(Parse(M"f = x * (y + z);"),
        m_f(Binary({ 0,0 }, "*",
            Variable({ 0,0 }, "", "x"),
            Binary({ 0,0 }, "+",
                Variable({ 0,0 }, "", "y"),
                Variable({ 0,0 }, "", "z")))));
    EXPECT_EQ(Parse(M"f = (x + y) * z;"),
        m_f(Binary({ 0,0 }, "*",
            Binary({ 0,0 }, "+",
                Variable({ 0,0 }, "", "x"),
                Variable({ 0,0 }, "", "y")),
            Variable({ 0,0 }, "", "z"))));

    //"f = a+-b;"; // ? or force a + - b or a+(-b)

    // negative
    EXPECT_EQ(ParseError(M"f = +;"),       "string(2,6): expected an expression\n");
    EXPECT_EQ(ParseError(M"f = 1 + 2 +;"), "string(2,12): expected an expression\n");
    EXPECT_EQ(ParseError(M"f = a + + ;"),  "string(2,11): expected an expression\n");
}

TEST(ParserTest, UnaryExp)
{
    // why did I skip this?
}

TEST(ParserTest, CallExp)
{
    // one param
    EXPECT_EQ(Parse(M"f = f2(x);"),
        m_f(Call({ 0,0 }, "", "f2",
                { Variable({0,0}, "", "x") })));
    // multiple params
    EXPECT_EQ(Parse(M"f = f2(x,y);"),
        m_f(Call({ 0,0 }, "", "f2",
                { Variable({0,0}, "", "x"),
                  Variable({0,0}, "", "y") })));

    // local function

    // imported function
    EXPECT_EQ(Parse(M"f = m2:f1(x);"),
        m_f(Call({ 0,0 }, "m2", "f1",
                { Variable({0,0}, "", "x") })));
    EXPECT_EQ(Parse(M"f = m2:f1(x,y);"),
        m_f(Call({ 0,0 }, "m2", "f1",
                { Variable({0,0}, "", "x"),
                  Variable({0,0}, "", "y") })));

    // exp in params
    EXPECT_EQ(Parse(M"f = f2(x+y, f3(z));"),
        m_f(Call({ 0,0 }, "", "f2",
                { Binary({0,0}, "+",
                            Variable({0,0}, "", "x"),
                            Variable({0,0}, "", "y")),
                 Call({0,0}, "", "f3",
                    {Variable({0,0}, "", "z")}) })));

    // higher order function
    "f = f2()()";

    // negative
    EXPECT_EQ(ParseError(M"f = f2(x y);"), "string(2,10): expected 'comma'\n");
}

TEST(ParserTest, VariableExp)
{
    EXPECT_EQ(Parse(M"f = x;"),   m_f(Variable({0,0}, "", "x")));
    EXPECT_EQ(Parse(M"f = m:x;"), m_f(Variable({0,0}, "m", "x")));

    // negative
    EXPECT_EQ(ParseError(M"f = m:;"),    "string(2,7): expected an identifier\n");
    EXPECT_EQ(ParseError(M"f = m:x:z;"), "string(2,8): expected 'semicolon'\n");
}

TEST(ParserTest, FieldExp)
{
    EXPECT_EQ(Parse(M"f = x.y;"),   m_f(Field({0,0}, Variable({0,0}, "", "x"), "y")));
    EXPECT_EQ(Parse(M"f = x.y.z;"), m_f(Field({0,0}, Field({0,0}, Variable({0,0}, "", "x"), "y"), "z")));
    EXPECT_EQ(Parse(M"f = x:y.z;"), m_f(Field({0,0}, Variable({0,0}, "x", "y"), "z")));
    EXPECT_EQ(Parse(M"f = x().y;"), m_f(Field({0,0}, Call({0,0}, "", "x", {}), "y")));

    // negative
    EXPECT_EQ(ParseError(M"f = x.);"), "string(2,7): expected a field identifier\n");
}

TEST(ParserTest, IntrinsicExp)
{
    EXPECT_EQ(Parse(M"f = @a'b'c(x);"), m_f(Intrinsic({0,0}, "a'b'c", { Variable({0,0}, "", "x") })));

    EXPECT_EQ(ParseError(M"f = @a'b'c;"), "string(2,11): expected 'parenthesis'\n");
}

TEST(ParserTest, ClassDeclaration)
{
    EXPECT_EQ(Parse(M"class a b {t f();}"),
        ModulePtr().classes({ Class({0,0}, "a", "b", { FunctionDecl({0,0}, {{"", "t"}, {}}, "f", {}) }) }));

    // negative
    EXPECT_EQ(ParseError(M"class{}"),              "string(2,6): expected an identifier\n");
    EXPECT_EQ(ParseError(M"class a b {}"),         "string(2,12): expected a function declaration\n");
    EXPECT_EQ(ParseError(M"class c:a b {t f();}"), "string(2,8): expected an identifier\n");
    EXPECT_EQ(ParseError(M"class a c:b {t f();}"), "string(2,10): expected 'brace'\n");
    EXPECT_EQ(ParseError(M"class a {t f();}"),     "string(2,9): expected an identifier\n");
    EXPECT_EQ(ParseError(M"class a b {t c:f();}"), "string(2,15): expected 'parenthesis'\n");
}

TEST(ParserTest, InstanceDeclaration)
{
    EXPECT_EQ(Parse(M"instance a b {t f() = 1;}"),
        ModulePtr().instances({
            Instance({0,0},{"","a"},{{"","b"},{}},{
                Function({ 0,0 }, Local, "f", llfp::ast::TypeIdentifier{ {"","t"},{} }, {}, Integer({ 0,0 }, "1")),
            })
        }));

    EXPECT_EQ(Parse(M"instance m:a n:b {x:t f() = 1;}"),
        ModulePtr().instances({
            Instance({0,0},{"m","a"},{{"n","b"},{}},{
                Function({ 0,0 }, Local, "f", llfp::ast::TypeIdentifier{ {"x","t"},{} }, {}, Integer({ 0,0 }, "1")),
            })
        }));

    // negative
    EXPECT_EQ(ParseError(M"instance{}"),                  "string(2,9): expected an identifier\n");
    EXPECT_EQ(ParseError(M"instance a b {t f();}"),       "string(2,20): expected 'equal'\n");
    EXPECT_EQ(ParseError(M"instance a b {f() = 1;}"),     "string(2,16): expected an identifier\n");
    EXPECT_EQ(ParseError(M"instance a {t f() = 1;}"),     "string(2,12): expected an identifier\n");
    EXPECT_EQ(ParseError(M"instance a b {t x:f() = 1;}"), "string(2,18): expected 'equal'\n");
}

TEST(ParserTest, Comments)
{
}

// clang-format on
