
#include "Parser.h"
#include "Lexer.h"

#include "AstEquality.h"
#include "AstTinyPrint.h"

#include "gtest/gtest.h"

#include <memory>
#include <type_traits>


using namespace llfp::ast;

// https://stackoverflow.com/questions/46737054/vectorunique-ptra-using-initialization-list
template<class T>
struct movable_il
{
    mutable T t;
    operator T() const&& { return std::move(t); }
    movable_il(T&& in) : t(std::move(in)) {}
};

template <class T, class A = std::allocator<T>>
std::vector<T, A> vector_from_il(std::initializer_list<movable_il<T>> il)
{
    std::vector<T, A> r(std::make_move_iterator(il.begin()), std::make_move_iterator(il.end()));
    return r;
}

// So that we can print the root node properly
struct ModulePtr
{
    std::unique_ptr<Module> ptr;

    ModulePtr(std::unique_ptr<Module>&& ptr_) : ptr{ std::move(ptr_) } {}
    ModulePtr(llfp::SourceLocation location = { 0,0 }, std::string name = "m")
    {
        ptr = std::make_unique<Module>(location, std::move(name));
    }

    bool operator==(const ModulePtr& other) const
    {
        if (ptr == nullptr) return other.ptr == nullptr;
        if (other.ptr == nullptr) return false;
        return *ptr == *other.ptr;
    }

    ModulePtr& publics(std::vector<PublicDeclaration> f)
    {
        ptr->publicDeclarations = std::move(f);
        return *this;
    }

    ModulePtr& imports(std::vector<ImportDeclaration> f)
    {
        ptr->imports = std::move(f);
        return *this;
    }

    ModulePtr& datas(std::initializer_list<movable_il<std::unique_ptr<DataDeclaration>>> d)
    {
        ptr->dataDeclarations = vector_from_il(d);
        return *this;
    }

    ModulePtr& functions(std::initializer_list<movable_il<std::unique_ptr<Function>>> funs)
    {
        ptr->functionDeclarations = vector_from_il(funs);
        return *this;
    }

    ModulePtr& classes(std::initializer_list<movable_il<std::unique_ptr<ClassDeclaration>>> c)
    {
        ptr->classDeclarations = vector_from_il(c);
        return *this;
    }

    ModulePtr& instances(std::initializer_list<movable_il<std::unique_ptr<ClassInstance>>> i)
    {
        ptr->instanceDeclarations = vector_from_il(i);
        return *this;
    }
};

std::ostream& operator<<(std::ostream& os, const ModulePtr& ptr)
{
    return os << ptr.ptr;
}

auto Parse(const char* string)
{
    auto input = llfp::lex::StringInput(string);
    auto lexer = llfp::lex::Lexer(&input);
    auto parser = llfp::parse::Parser(&lexer);
    return ModulePtr(parser.parse());
}

std::string ParseError(const char* string)
{
    testing::internal::CaptureStderr();
    Parse(string);
    return testing::internal::GetCapturedStderr();
}

auto MakeTypeId(std::string moduleName, std::string name, std::vector< llfp::ast::TypeIdentifier> parameters)
{
    return llfp::ast::TypeIdentifier{ llfp::GlobalIdentifier {std::move(moduleName), std::move(name) }, std::move(parameters) };
}

auto MakeDataDecl(llfp::SourceLocation location, bool exported, std::string name, std::vector<Field> fields)
{
    return std::make_unique<DataDeclaration>(location, std::move(name), std::vector<std::string>{}, std::move(fields), exported);
}

auto MakeFunction(
    llfp::SourceLocation location,
    bool exported,
    std::string name,
    std::string typeName,
    std::initializer_list<movable_il<std::unique_ptr<Parameter>>> parameters,
    std::unique_ptr<Exp> functionBody)
{
    return std::make_unique<Function>(location, std::move(name), llfp::ast::TypeIdentifier{ llfp::GlobalIdentifier{ "",  std::move(typeName) }, {} }, vector_from_il(parameters), std::move(functionBody), exported);
}

auto MakeFunction(
    llfp::SourceLocation location,
    bool exported,
    std::string name,
    llfp::ast::TypeIdentifier typeName,
    std::initializer_list<movable_il<std::unique_ptr<Parameter>>> parameters,
    std::unique_ptr<Exp> functionBody)
{
    return std::make_unique<Function>(location, std::move(name), std::move(typeName), vector_from_il(parameters), std::move(functionBody), exported);
}

auto MakeParameter(llfp::SourceLocation location, std::string typeName, std::string name)
{
    return std::make_unique<Parameter>(location, MakeTypeId("", std::move(typeName), {}), std::move(name));
}

auto MakeParameter(llfp::SourceLocation location, llfp::ast::TypeIdentifier typeName, std::string name)
{
    return std::make_unique<Parameter>(location, std::move(typeName), std::move(name));
}

std::unique_ptr<Exp> MakeLiteral(llfp::SourceLocation location, llfp::lex::Token tokenType, std::string value)
{
    return std::make_unique<LiteralExp>(location, tokenType, std::move(value));
}

std::unique_ptr<Exp> MakeInteger(llfp::SourceLocation location, std::string value)
{
    return MakeLiteral(location, llfp::lex::Token::Integer, std::move(value));
}

std::unique_ptr<Exp> MakeLet(
    llfp::SourceLocation location,
    std::initializer_list<movable_il<std::unique_ptr<Function>>> letStatments,
    std::unique_ptr<Exp> exp)
{
    return std::make_unique<LetExp>(location, vector_from_il(letStatments), std::move(exp));
}

std::unique_ptr<Exp> MakeIf(llfp::SourceLocation location, std::unique_ptr<Exp> condition, std::unique_ptr<Exp> thenExp, std::unique_ptr<Exp> elseExp)
{
    return std::make_unique<IfExp>(location, std::move(condition), std::move(thenExp), std::move(elseExp));
}

std::unique_ptr<Exp> MakeCase()
{
    return nullptr; //std::make_unique<CaseExp>();
}

std::unique_ptr<Exp> MakeBinary(llfp::SourceLocation location, std::string op, std::unique_ptr<Exp> lhs, std::unique_ptr<Exp> rhs)
{
    return std::make_unique<BinaryExp>(location, std::move(op), std::move(lhs), std::move(rhs));
}

std::unique_ptr<Exp> MakeUnary(llfp::SourceLocation location, std::string op, std::unique_ptr<Exp> operand)
{
    return std::make_unique<UnaryExp>(location, std::move(op), std::move(operand));
}

std::unique_ptr<Exp> MakeCall(
    llfp::SourceLocation location,
    std::string moduleName,
    std::string name,
    std::initializer_list<movable_il<std::unique_ptr<Exp>>> args)
{
    return std::make_unique<CallExp>(location, llfp::GlobalIdentifier{ std::move(moduleName), std::move(name) }, vector_from_il(args));
}

std::unique_ptr<Exp> MakeVariable(llfp::SourceLocation location, std::string moduleName, std::string name)
{
    return std::make_unique<VariableExp>(location, llfp::GlobalIdentifier{ std::move(moduleName), std::move(name) });
}

std::unique_ptr<Exp> MakeField(llfp::SourceLocation location, std::unique_ptr<Exp> lhs, std::string fieldName)
{
    return std::make_unique<FieldExp>(location, std::move(lhs), std::move(fieldName));
}

std::unique_ptr<Exp> MakeConstructor(llfp::SourceLocation location, llfp::GlobalIdentifier name, std::initializer_list<movable_il<std::unique_ptr<NamedArgument>>> args)
{
    return std::make_unique<ConstructorExp>(location, std::move(name), vector_from_il(args));
}

std::unique_ptr<NamedArgument> MakeNamedArg(llfp::SourceLocation location, std::string name, std::unique_ptr<Exp> exp)
{
    return std::make_unique<NamedArgument>(location, std::move(name), std::move(exp));
}

std::unique_ptr<ClassDeclaration> MakeClass(llfp::SourceLocation location, std::string name, std::string typeVar, std::initializer_list<movable_il<std::unique_ptr<FunctionDecl>>> funs)
{
    return std::make_unique<ClassDeclaration>(location, std::move(name), std::move(typeVar), vector_from_il(funs));
}

std::unique_ptr<ClassInstance> MakeInstance(llfp::SourceLocation location, llfp::GlobalIdentifier id, TypeIdentifier typeArg, std::initializer_list<movable_il<std::unique_ptr<Function>>> funs)
{
    return std::make_unique<ClassInstance>(location, std::move(id), std::move(typeArg), vector_from_il(funs));
}

std::unique_ptr<FunctionDecl> MakeFunctionDecl(llfp::SourceLocation location, TypeIdentifier type, std::string name, std::initializer_list<movable_il<std::unique_ptr<Parameter>>> params)
{
    return std::make_unique<FunctionDecl>(location, std::move(name), std::move(type), vector_from_il(params));
}


namespace {
constexpr bool Local = false;
constexpr bool Exported = true;
}

/**
Make a module m with a function f with exp.
*/
ModulePtr MakeMF(std::unique_ptr<Exp> exp)
{
    return std::move(ModulePtr().functions({ MakeFunction({0,0}, Local, "f","", {}, std::move(exp)) }));
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
    // change to data a = {};
    // to allow data a = a1{} | a2{}; in the future

    EXPECT_EQ(Parse(M"data a{}"), ModulePtr().datas({ MakeDataDecl({ 0,0 }, Local, "a", {}) }));
    EXPECT_EQ(Parse(M"data a{t x;}"), ModulePtr().datas({ MakeDataDecl({ 0,0 }, Local, "a", { Field({ 0,0 }, {"", "t"}, "x") }) }));
    EXPECT_EQ(Parse(M"data a{m2:t x;}"), ModulePtr().datas({ MakeDataDecl({ 0,0 }, Local, "a",{ Field({ 0,0 }, { "m2", "t" }, "x") }) }));
    EXPECT_EQ(Parse(M"data a{t1 x; t2 y;}"),
        ModulePtr().datas({
            MakeDataDecl({ 0,0 }, Local, "a", {
                Field({ 0,0 }, {"", "t1"}, "x"),
                Field({ 0,0 }, {"", "t2"}, "y") }) }));
    EXPECT_EQ(Parse(M"export data a{}"), ModulePtr().datas({ MakeDataDecl({ 0,0 }, Exported, "a",{}) }));

    // negative
    EXPECT_EQ(ParseError(M"data a{x;}"),      "string(2,9): expected an identifier\n");
    EXPECT_EQ(ParseError(M"data a{x}"),       "string(2,9): expected an identifier\n");
    EXPECT_EQ(ParseError(M"data a{t x}"),     "string(2,11): expected 'semicolon'\n");
    EXPECT_EQ(ParseError(M"data a{t x; y;}"), "string(2,14): expected an identifier\n");
    EXPECT_EQ(ParseError(M"data a{t x y;}"),  "string(2,12): expected 'semicolon'\n");
    EXPECT_EQ(ParseError(M"data{t x;}"),      "string(2,5): expected an identifier\n");
}

TEST(ParserTest, DataConstructor)
{
    EXPECT_EQ(Parse(M"f = a{};"), MakeMF(MakeConstructor({ 0,0 }, { "", "a" }, {})));
    EXPECT_EQ(Parse(M"f = a{1};"), MakeMF(MakeConstructor({ 0,0 }, { "", "a" }, { MakeNamedArg({0,0}, "", MakeInteger({0,0}, "1")) })));
    EXPECT_EQ(Parse(M"f = a{x};"), MakeMF(MakeConstructor({ 0,0 }, { "", "a" }, { MakeNamedArg({0,0}, "", MakeVariable({0,0}, "", "x")) })));
    EXPECT_EQ(Parse(M"f = a{x:y};"), MakeMF(MakeConstructor({ 0,0 }, { "", "a" }, { MakeNamedArg({ 0,0 }, "", MakeVariable({ 0,0 }, "x", "y")) })));
    EXPECT_EQ(Parse(M"f = a{x = y};"), MakeMF(MakeConstructor({ 0,0 }, { "", "a" }, { MakeNamedArg({ 0,0 }, "x", MakeVariable({ 0,0 }, "", "y")) })));
    EXPECT_EQ(Parse(M"f = a{x()};"), MakeMF(MakeConstructor({ 0,0 }, { "", "a" }, { MakeNamedArg({ 0,0 }, "", MakeCall({0,0},"", "x", {})) })));
    EXPECT_EQ(Parse(M"f = a{x + 1};"), MakeMF(
        MakeConstructor({ 0,0 }, { "", "a" }, { MakeNamedArg({ 0,0 }, "",
            MakeBinary({ 0,0 }, "+",
                MakeVariable({ 0,0 }, "", "x"),
                MakeInteger({ 0,0 }, "1"))) })));
    EXPECT_EQ(Parse(M"f = a{1, x};"), MakeMF(MakeConstructor({ 0,0 }, { "", "a" }, {
        MakeNamedArg({ 0,0 }, "", MakeInteger({ 0,0 }, "1")),
        MakeNamedArg({ 0,0 }, "", MakeVariable({ 0,0 }, "", "x")) })));
    EXPECT_EQ(Parse(M"f = a:b{1};"), MakeMF(MakeConstructor({ 0,0 }, { "a", "b" }, { MakeNamedArg({ 0,0 }, "",  MakeInteger({ 0,0 }, "1")) })));
    EXPECT_EQ(Parse(M"f = a{b{1}};"), MakeMF(
        MakeConstructor({ 0,0 }, { "", "a" }, { MakeNamedArg({ 0,0 }, "",
            MakeConstructor({ 0,0 }, { "", "b" }, { MakeNamedArg({ 0,0 }, "",
                MakeInteger({ 0,0 }, "1")) })) })));

    // negative
    EXPECT_EQ(ParseError(M"f = a{x:}"),     "string(2,9): expected an identifier\n");
    EXPECT_EQ(ParseError(M"f = a{x: = 1}"), "string(2,10): expected an identifier\n");
    EXPECT_EQ(ParseError(M"f = a{x=}"),     "string(2,9): expected an expression\n");
}

TEST(ParserTest, Functions)
{
    // untyped function
    EXPECT_EQ(Parse(M"f = 1;"),
        ModulePtr().functions({MakeFunction({0,0}, Local, "f", "", {}, MakeInteger({0,0}, "1")) }));

    // no params
    EXPECT_EQ(Parse(M"t f = 1;"),
        ModulePtr().functions({MakeFunction({0,0}, Local, "f", "t", {}, MakeInteger({0,0}, "1")) }));
    EXPECT_EQ(Parse(M"m2:t f = 1;"),
        ModulePtr().functions({MakeFunction({ 0,0 }, Local, "f", MakeTypeId("m2", "t", {}), {}, MakeInteger({ 0,0 }, "1")) }));

    // one params
    EXPECT_EQ(Parse(M"t f(x) = 1;"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "t",{MakeParameter({0,0}, "", "x")},
                MakeInteger({0,0}, "1")) }));

    // multiple params
    EXPECT_EQ(Parse(M"t f(x,y) = 1;"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "t", {
                MakeParameter({0,0}, "", "x"),
                MakeParameter({0,0}, "", "y")},
                MakeInteger({0,0}, "1")) }));

    // typed params
    EXPECT_EQ(Parse(M"t f(t x) = 1;"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "t", { MakeParameter({0,0}, "t", "x")},
                MakeInteger({0,0}, "1")) }));
    EXPECT_EQ(Parse(M"t f(t x, t2 y) = 1;"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "t", {
                MakeParameter({0,0}, "t", "x"),
                MakeParameter({0,0}, "t2", "y")},
                MakeInteger({0,0}, "1")) }));
    EXPECT_EQ(Parse(M"t f(m:t x) = 1;"),
        ModulePtr().functions({
            MakeFunction({ 0,0 }, Local, "f", "t", { MakeParameter({ 0,0 }, MakeTypeId("m", "t", {}), "x") },
                MakeInteger({ 0,0 }, "1")) }));

    // parameterized types
    EXPECT_EQ(Parse(M"t[x] f() = 1;"),
        ModulePtr().functions({
            MakeFunction({ 0,0 }, Local, "f", MakeTypeId("", "t", { MakeTypeId("", "x", {}) }), {},
            MakeInteger({ 0,0 }, "1")) }));
    EXPECT_EQ(Parse(M"f(x[y] z) = 1;"),
        ModulePtr().functions({
            MakeFunction({ 0,0 }, Local, "f", "", { MakeParameter({0,0}, MakeTypeId("", "x", { MakeTypeId("", "y", {}) }), "z") },
            MakeInteger({ 0,0 }, "1")) }));
    EXPECT_EQ(Parse(M"x[y[z]] f() = 1;"),
        ModulePtr().functions({
            MakeFunction({ 0,0 }, Local, "f", MakeTypeId("", "x", { MakeTypeId("", "y", { MakeTypeId("", "z", {}) }) }), {},
            MakeInteger({ 0,0 }, "1")) }));
    EXPECT_EQ(Parse(M"x[y,z] f() = 1;"),
        ModulePtr().functions({
            MakeFunction({ 0,0 }, Local, "f", MakeTypeId("", "x", { MakeTypeId("", "y", {}), MakeTypeId("", "z", {}) }), {},
            MakeInteger({ 0,0 }, "1")) }));

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
        M"data a{}"
         "f = 1;"
         "data b{}"
         "g = 2;"),
        ModulePtr()
        .functions({ MakeFunction({ 0,0 }, Local, "f", "", {}, MakeInteger({ 0,0 }, "1")),
                     MakeFunction({ 0,0 }, Local, "g", "", {}, MakeInteger({ 0,0 }, "2")) })
        .datas({ MakeDataDecl({ 0,0 }, Local, "a",{}),
                 MakeDataDecl({ 0,0 }, Local, "b",{}) }));

    // negative
}

TEST(ParserTest, Literals)
{
    // integer
    EXPECT_EQ(Parse(M"f = 1;"),
        ModulePtr().functions({ MakeFunction({0,0}, Local, "f", "", {}, MakeInteger({0,0}, "1")) }));
    // float
    EXPECT_EQ(Parse(M"f = 1.0;"),
        ModulePtr().functions({ MakeFunction({0,0}, Local, "f", "", {}, MakeLiteral({0,0}, llfp::lex::Token::Float, "1.0")) }));
    // char
    EXPECT_EQ(Parse(M"f = \'c\';"),
        ModulePtr().functions({ MakeFunction({0,0}, Local, "f", "", {}, MakeLiteral({0,0}, llfp::lex::Token::Char, "c")) }));
    // string
    EXPECT_EQ(Parse(M"f = \"ac\";"),
        ModulePtr().functions({ MakeFunction({0,0}, Local, "f", "", {}, MakeLiteral({0,0}, llfp::lex::Token::String, "ac")) }));
    // bool
    EXPECT_EQ(Parse(M"f = true;"),
        ModulePtr().functions({ MakeFunction({0,0}, Local, "f", "", {}, MakeLiteral({0,0}, llfp::lex::Token::Bool, "true")) }));
    EXPECT_EQ(Parse(M"f = false;"),
        ModulePtr().functions({ MakeFunction({0,0}, Local, "f", "", {}, MakeLiteral({0,0}, llfp::lex::Token::Bool, "false")) }));

    // negative
    // invalid string escape
}

TEST(ParserTest, LetExp)
{
    EXPECT_EQ(Parse(M"f = let x = 1; in 2;"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "", {},
                MakeLet({0,0}, {
                    MakeFunction({0,0}, Local, "x", "", {},
                        MakeInteger({0,0}, "1"))},
                    MakeInteger({0,0}, "2"))) }));
    EXPECT_EQ(Parse(M"f = let x = 1; y = 2; in 3;"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "", {},
                MakeLet({0,0}, {
                    MakeFunction({0,0}, Local, "x", "", {},
                        MakeInteger({0,0}, "1")),
                    MakeFunction({0,0}, Local, "y", "", {},
                        MakeInteger({0,0}, "2"))},
                    MakeInteger({0,0}, "3"))) }));

    // negative
    EXPECT_EQ(ParseError(M"f = let x = 1; 2;"),          "string(2,16): expected an identifier\n");
    EXPECT_EQ(ParseError(M"f = let x = 1; y 2;"),        "string(2,18): expected 'equal'\n");
    EXPECT_EQ(ParseError(M"f = let x = 1 y = 2; in 3;"), "string(2,15): expected 'semicolon'\n");
}

TEST(ParserTest, IfExp)
{
    EXPECT_EQ(Parse(M"f = if x then y else z;"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "", {},
                              MakeIf({0,0},
                                     MakeVariable({0,0}, "", "x"),
                                     MakeVariable({0,0}, "", "y"),
                                     MakeVariable({0,0}, "", "z"))) }));

    // negative
    EXPECT_EQ(ParseError(M"f = if x,a then y else z;"), "string(2,9): expected 'then'\n");
}

TEST(ParserTest, CaseExp)
{
}

TEST(ParserTest, BinaryExp)
{
    EXPECT_EQ(Parse(M"f = x + y;"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "", {},
                              MakeBinary({0,0}, "+",
                                         MakeVariable({0,0}, "", "x"),
                                         MakeVariable({0,0}, "", "y"))) }));
    EXPECT_EQ(Parse(M"f = x + y + z;"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "", {},
                              MakeBinary({0,0}, "+",
                                         MakeBinary({0,0}, "+",
                                                    MakeVariable({0,0}, "", "x"),
                                                    MakeVariable({0,0}, "", "y")),
                                         MakeVariable({0,0}, "", "z"))) }));
    EXPECT_EQ(Parse(M"f = x + y * z;"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "", {},
                              MakeBinary({0,0}, "+",
                                         MakeVariable({0,0}, "", "x"),
                                         MakeBinary({0,0}, "*",
                                                    MakeVariable({0,0}, "", "y"),
                                                    MakeVariable({0,0}, "", "z")))) }));
    EXPECT_EQ(Parse(M"f = x * y + z;"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "", {},
                              MakeBinary({0,0}, "+",
                                         MakeBinary({0,0}, "*",
                                                    MakeVariable({0,0}, "", "x"),
                                                    MakeVariable({0,0}, "", "y")),
                                         MakeVariable({0,0}, "", "z"))) }));

    EXPECT_EQ(Parse(M"f = x * (y + z);"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "", {},
                              MakeBinary({0,0}, "*",
                                         MakeVariable({0,0}, "", "x"),
                                         MakeBinary({0,0}, "+",
                                                    MakeVariable({0,0}, "", "y"),
                                                    MakeVariable({0,0}, "", "z")))) }));
    EXPECT_EQ(Parse(M"f = (x + y) * z;"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "", {},
                              MakeBinary({0,0}, "*",
                                         MakeBinary({0,0}, "+",
                                                    MakeVariable({0,0}, "", "x"),
                                                    MakeVariable({0,0}, "", "y")),
                                         MakeVariable({0,0}, "", "z"))) }));

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
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "", {},
                              MakeCall({0,0}, "", "f2",
                                       {MakeVariable({0,0}, "", "x")})) }));
    // multiple params
    EXPECT_EQ(Parse(M"f = f2(x,y);"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "", {},
                              MakeCall({0,0}, "", "f2",
                                       {MakeVariable({0,0}, "", "x"),
                                        MakeVariable({0,0}, "", "y")})) }));

    // local function

    // imported function
    EXPECT_EQ(Parse(M"f = m2:f1(x);"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "", {},
                              MakeCall({0,0}, "m2", "f1",
                                       {MakeVariable({0,0}, "", "x")})) }));
    EXPECT_EQ(Parse(M"f = m2:f1(x,y);"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "", {},
                              MakeCall({0,0}, "m2", "f1",
                                       {MakeVariable({0,0}, "", "x"),
                                        MakeVariable({0,0}, "", "y")})) }));

    // exp in params
    EXPECT_EQ(Parse(M"f = f2(x+y, f3(z));"),
        ModulePtr().functions({
            MakeFunction({0,0}, Local, "f", "", {},
                              MakeCall({0,0}, "", "f2",
                                       {MakeBinary({0,0}, "+",
                                                   MakeVariable({0,0}, "", "x"),
                                                   MakeVariable({0,0}, "", "y")),
                                        MakeCall({0,0}, "", "f3",
                                           {MakeVariable({0,0}, "", "z")})})) }));

    // higher order function
    "f = f2()()";

    // negative
    EXPECT_EQ(ParseError(M"f = f2(x y);"), "string(2,10): expected 'comma'\n");
}

TEST(ParserTest, VariableExp)
{
    EXPECT_EQ(Parse(M"f = x;"),
        ModulePtr().functions({ MakeFunction({0,0}, Local, "f", "", {}, MakeVariable({0,0}, "", "x")) }));
    EXPECT_EQ(Parse(M"f = m:x;"),
        ModulePtr().functions({ MakeFunction({0,0}, Local, "f", "", {}, MakeVariable({0,0}, "m", "x")) }));

    // negative
    EXPECT_EQ(ParseError(M"f = m:;"),    "string(2,7): expected an identifier\n");
    EXPECT_EQ(ParseError(M"f = m:x:z;"), "string(2,8): expected 'semicolon'\n");
}

TEST(ParserTest, FieldExp)
{
    EXPECT_EQ(Parse(M"f = x.y;"),
        ModulePtr().functions({ MakeFunction({0,0}, Local, "f", "", {}, MakeField({0,0}, MakeVariable({0,0}, "", "x"), "y")) }));
    EXPECT_EQ(Parse(M"f = x.y.z;"),
        ModulePtr().functions({ MakeFunction({0,0}, Local, "f", "", {}, MakeField({0,0}, MakeField({0,0}, MakeVariable({0,0}, "", "x"), "y"), "z")) }));
    EXPECT_EQ(Parse(M"f = x:y.z;"),
        ModulePtr().functions({ MakeFunction({0,0}, Local, "f", "", {}, MakeField({0,0}, MakeVariable({0,0}, "x", "y"), "z")) }));
    EXPECT_EQ(Parse(M"f = x().y;"),
        ModulePtr().functions({ MakeFunction({ 0,0 }, Local, "f", "", {}, MakeField({ 0,0 }, MakeCall({0,0}, "", "x", {}), "y")) }));

    // negative
    EXPECT_EQ(ParseError(M"f = x.);"), "string(2,7): expected a field identifier\n");
}

TEST(ParserTest, ClassDeclaration)
{
    EXPECT_EQ(Parse(M"class a b {t f();}"),
        ModulePtr().classes({ MakeClass({0,0}, "a", "b", { MakeFunctionDecl({0,0}, {{"", "t"}, {}}, "f", {}) }) }));

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
            MakeInstance({0,0},{"","a"},{{"","b"},{}},{
                MakeFunction({ 0,0 }, Local, "f",  TypeIdentifier{ {"","t"},{} }, {}, MakeInteger({ 0,0 }, "1")),
            })
        }));

    EXPECT_EQ(Parse(M"instance m:a n:b {x:t f() = 1;}"),
        ModulePtr().instances({
            MakeInstance({0,0},{"m","a"},{{"n","b"},{}},{
                MakeFunction({ 0,0 }, Local, "f",  TypeIdentifier{ {"x","t"},{} }, {}, MakeInteger({ 0,0 }, "1")),
            })
        }));

    // negative
    EXPECT_EQ(ParseError(M"instance{}"),                  "string(2,9): expected an identifier\n");
    EXPECT_EQ(ParseError(M"instance a b {t f();}"),       "string(2,20): expected 'equal'\n");
    EXPECT_EQ(ParseError(M"instance a b {f() = 1;}"),     "string(2,16): expected an identifier\n");
    EXPECT_EQ(ParseError(M"instance a {t f() = 1;}"),     "string(2,12): expected an identifier\n");
    EXPECT_EQ(ParseError(M"instance a b {t x:f() = 1;}"), "string(2,18): expected 'parenthesis'\n");
}

TEST(ParserTest, Comments)
{
}
