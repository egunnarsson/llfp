
#include "Parser.h"
#include "Lexer.h"

#include "AstEquality.h"
#include "AstTinyPrint.h"

#include "gtest/gtest.h"

#include <memory>
#include <type_traits>


using namespace llfp::ast;

// So that we can print the root node properly
struct ModulePtr
{
    std::unique_ptr<Module> ptr;
    ModulePtr(std::unique_ptr<Module> &&ptr_) : ptr(std::move(ptr_)) {}
    bool operator==(const ModulePtr &other) const
    {
        if (ptr == nullptr) return other.ptr == nullptr;
        if (other.ptr == nullptr) return false;
        return *ptr == *other.ptr;
    }
};

std::ostream& operator<<(std::ostream &os, const ModulePtr &ptr)
{
    return os << ptr.ptr;
}

// https://stackoverflow.com/questions/46737054/vectorunique-ptra-using-initialization-list
template<class T>
struct movable_il
{
    mutable T t;
    operator T() const&& { return std::move(t); }
    movable_il(T&& in) : t(std::move(in)) {}
};

template <class T, class A = std::allocator<T>>
std::vector<T,A> vector_from_il(std::initializer_list<movable_il<T>> il)
{
    std::vector<T,A> r(std::make_move_iterator(il.begin()), std::make_move_iterator(il.end()));
    return r;
}

auto Parse(const char *string)
{
    auto input = llfp::lex::StringInput(string);
    auto lexer = llfp::lex::Lexer(&input);
    auto parser = llfp::parse::Parser(&lexer);
    return ModulePtr(parser.parse());
}

std::string ParseError(const char *string)
{
    testing::internal::CaptureStderr();
    Parse(string);
    return testing::internal::GetCapturedStderr();
}

auto MakeTypeId(std::string moduleName, std::string name, std::vector< llfp::ast::TypeIdentifier> parameters)
{
    return llfp::ast::TypeIdentifier{ llfp::GlobalIdentifier {std::move(moduleName), std::move(name) }, std::move(parameters) };
}

ModulePtr MakeModule(llfp::SourceLocation sourceLocation,
                          std::string name,
                          std::vector<PublicDeclaration> publicDeclarations,
                          std::vector<ImportDeclaration> imports,
                          std::initializer_list<movable_il<std::unique_ptr<Function>>> functionDeclarations,
                          std::initializer_list<movable_il<std::unique_ptr<DataDeclaration>>> dataDeclarations)
{
    auto module = std::make_unique<Module>(sourceLocation, std::move(name));
    module->publicDeclarations = std::move(publicDeclarations);
    module->imports = std::move(imports);
    module->functionDeclarations = vector_from_il(functionDeclarations);
    module->dataDeclarations = vector_from_il(dataDeclarations);
    return module;
}

auto MakeDataDecl(llfp::SourceLocation location, std::string name, std::vector<Field> fields, bool exported)
{
    return std::make_unique<DataDeclaration>(location, std::move(name), std::vector<std::string>{}, std::move(fields), exported);
}

auto MakeFunctionDecl(llfp::SourceLocation location,
                      std::string name,
                      std::string typeName,
                      std::initializer_list<movable_il<std::unique_ptr<Parameter>>> parameters,
                      std::unique_ptr<Exp> functionBody,
                      bool exported)
{
    return std::make_unique<Function>(location, std::move(name), llfp::ast::TypeIdentifier{ llfp::GlobalIdentifier{ "",  std::move(typeName) }, {} }, vector_from_il(parameters), std::move(functionBody), exported);
}

auto MakeFunctionDecl(llfp::SourceLocation location,
                      std::string name,
                      llfp::ast::TypeIdentifier typeName,
                      std::initializer_list<movable_il<std::unique_ptr<Parameter>>> parameters,
                      std::unique_ptr<Exp> functionBody,
                      bool exported)
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

std::unique_ptr<Exp> MakeLet(llfp::SourceLocation location,
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

std::unique_ptr<Exp> MakeCall(llfp::SourceLocation location,
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

/**
Make a module m with a function f with exp.
*/
ModulePtr MakeMF(std::unique_ptr<Exp> exp)
{
    return MakeModule({ 0,0 }, "m", {}, {}, { MakeFunctionDecl({0,0}, "f","", {}, std::move(exp), false) }, {});
}

TEST(ParserTest, Imports)
{
    // no imports
    EXPECT_EQ(Parse("module m;"), MakeModule({ 0,0 }, "m", {}, {}, {}, {}));
    
    // one import
    EXPECT_EQ(Parse("module m;"
                    "import m2;"),
              MakeModule({0,0}, "m", {}, {ImportDeclaration({0,0}, "m2")}, {}, {}));
    
    // multiple imports
    EXPECT_EQ(Parse("module m;"
                    "import m2;"
                    "import m3;"),
              MakeModule({0,0}, "m", {}, {ImportDeclaration({0,0}, "m2"), ImportDeclaration({0,0}, "m3")}, {}, {}));
    
    // from, select symbols

    // negative
    EXPECT_EQ(ParseError("module m\nf = 1;"), "string(2,1): expected 'semicolon'\n");
    EXPECT_EQ(ParseError("f = 1; module m;"), "string(1,1): expected 'module'\n");
    EXPECT_EQ(ParseError("module m; import;"), "string(1,17): expected an identifier\n");
}

TEST(ParserTest, PublicDeclarations)
{
    // no decls
    EXPECT_EQ(Parse("module m;"), MakeModule({0,0}, "m", {}, {}, {}, {}));
    EXPECT_EQ(Parse("module m();"), MakeModule({0,0}, "m", {}, {}, {}, {}));
    
    // one decls
    EXPECT_EQ(Parse("module m(x);"), MakeModule({0,0}, "m", {PublicDeclaration({0,0}, "x")}, {}, {}, {}));
    
    // multiple decls
    EXPECT_EQ(Parse("module m(x,y);"), MakeModule({0,0}, "m", {PublicDeclaration({0,0}, "x"), PublicDeclaration({0,0}, "y")}, {}, {}, {}));

    // negative
    EXPECT_EQ(ParseError("module m(x y);"), "string(1,12): expected 'comma'\n");
}

#define M "module m;\n"

TEST(ParserTest, DataDeclarations)
{
    // change to data a = {};
    // to allow data a = a1{} | a2{}; in the future

    EXPECT_EQ(Parse(M"data a{}"), MakeModule({ 0,0 }, "m", {}, {}, {}, { MakeDataDecl({0,0}, "a", {}, false) }));
    EXPECT_EQ(Parse(M"data a{t x;}"), MakeModule({ 0,0 }, "m", {}, {}, {}, { MakeDataDecl({ 0,0 }, "a", { Field({ 0,0 }, {"", "t"}, "x") }, false) }));
    EXPECT_EQ(Parse(M"data a{m2:t x;}"), MakeModule({ 0,0 }, "m", {}, {}, {}, { MakeDataDecl({ 0,0 }, "a",{ Field({ 0,0 },{ "m2", "t" }, "x") }, false) }));
    EXPECT_EQ(Parse(M"data a{t1 x; t2 y;}"), MakeModule({ 0,0 }, "m", {}, {}, {}, {
        MakeDataDecl({ 0,0 }, "a", {
            Field({ 0,0 }, {"", "t1"}, "x"),
            Field({ 0,0 }, { "", "t2"}, "y") }, false) }));
    EXPECT_EQ(Parse(M"export data a{}"), MakeModule({ 0,0 }, "m", {}, {}, {}, { MakeDataDecl({ 0,0 }, "a",{}, true) }));

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
    EXPECT_EQ(Parse(M"f = a{};"),MakeMF(MakeConstructor({ 0,0 }, { "", "a" }, {} )));
    EXPECT_EQ(Parse(M"f = a{1};"), MakeMF(MakeConstructor({ 0,0 }, { "", "a" }, { MakeNamedArg({0,0}, "", MakeLiteral({0,0}, llfp::lex::tok_integer, "1")) } )));
    EXPECT_EQ(Parse(M"f = a{x};"), MakeMF(MakeConstructor({ 0,0 }, { "", "a" }, { MakeNamedArg({0,0}, "", MakeVariable({0,0}, "", "x")) })));
    EXPECT_EQ(Parse(M"f = a{x:y};"), MakeMF(MakeConstructor({ 0,0 }, { "", "a" }, { MakeNamedArg({ 0,0 }, "", MakeVariable({ 0,0 }, "x", "y")) })));
    EXPECT_EQ(Parse(M"f = a{x = y};"), MakeMF(MakeConstructor({ 0,0 }, { "", "a" }, { MakeNamedArg({ 0,0 }, "x", MakeVariable({ 0,0 }, "", "y")) })));
    EXPECT_EQ(Parse(M"f = a{x()};"), MakeMF(MakeConstructor({ 0,0 }, { "", "a" }, { MakeNamedArg({ 0,0 }, "", MakeCall({0,0},"", "x", {})) })));
    EXPECT_EQ(Parse(M"f = a{x + 1};"), MakeMF(
        MakeConstructor({ 0,0 }, { "", "a" }, { MakeNamedArg({ 0,0 }, "",
            MakeBinary({ 0,0 },"+",
                MakeVariable({ 0,0 }, "", "x"),
                MakeLiteral({ 0,0 }, llfp::lex::tok_integer, "1"))) })));
    EXPECT_EQ(Parse(M"f = a{1, x};"), MakeMF(MakeConstructor({ 0,0 }, { "", "a" }, {
        MakeNamedArg({ 0,0 }, "", MakeLiteral({ 0,0 }, llfp::lex::tok_integer, "1")),
        MakeNamedArg({ 0,0 }, "", MakeVariable({ 0,0 }, "", "x")) } )));
    EXPECT_EQ(Parse(M"f = a:b{1};"), MakeMF(MakeConstructor({ 0,0 }, { "a", "b" }, { MakeNamedArg({ 0,0 }, "",  MakeLiteral({ 0,0 }, llfp::lex::tok_integer, "1")) } )));
    EXPECT_EQ(Parse(M"f = a{b{1}};"), MakeMF(
        MakeConstructor({ 0,0 }, { "", "a" }, { MakeNamedArg({ 0,0 }, "",
            MakeConstructor({ 0,0 }, { "", "b" }, { MakeNamedArg({ 0,0 }, "",
                MakeLiteral({ 0,0 }, llfp::lex::tok_integer, "1")) })) } )));

    // negative
    EXPECT_EQ(ParseError(M"f = a{x:}"),     "string(2,9): expected an identifier\n");
    EXPECT_EQ(ParseError(M"f = a{x: = 1}"), "string(2,10): expected an identifier\n");
    EXPECT_EQ(ParseError(M"f = a{x=}"),     "string(2,9): expected an expression\n");
}

TEST(ParserTest, Functions)
{
    // untyped function
    EXPECT_EQ(Parse(M"f = 1;"),
        MakeModule({0,0}, "m", {}, {}, {
            MakeFunctionDecl({0,0}, "f", "", {}, MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false)}, {}));
    
    // no params
    EXPECT_EQ(Parse(M"t f = 1;"),
        MakeModule({0,0}, "m", {}, {}, {
            MakeFunctionDecl({0,0}, "f", "t", {}, MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false)}, {}));
    EXPECT_EQ(Parse(M"m2:t f = 1;"),
        MakeModule({ 0,0 }, "m", {}, {}, {
            MakeFunctionDecl({ 0,0 }, "f", MakeTypeId("m2", "t", {}), {}, MakeLiteral({ 0,0 }, llfp::lex::tok_integer, "1"), false) }, {}));

    // one params
    EXPECT_EQ(Parse(M"t f(x) = 1;"),
        MakeModule({0,0}, "m", {}, {}, {
            MakeFunctionDecl({0,0}, "f", "t",
                {MakeParameter({0,0}, "", "x")},
                MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false)}, {}));
    
    // multiple params
    EXPECT_EQ(Parse(M"t f(x,y) = 1;"),
        MakeModule({0,0}, "m", {}, {}, {
            MakeFunctionDecl({0,0}, "f", "t", 
                {MakeParameter({0,0}, "", "x"), MakeParameter({0,0}, "", "y")},
                MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false)}, {}));
    
    // typed params
    EXPECT_EQ(Parse(M"t f(t x) = 1;"),
        MakeModule({0,0}, "m", {}, {}, {
            MakeFunctionDecl({0,0}, "f", "t",
                {MakeParameter({0,0}, "t", "x")},
                MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false)}, {}));
    EXPECT_EQ(Parse(M"t f(t x, t2 y) = 1;"),
        MakeModule({0,0}, "m", {}, {}, {
            MakeFunctionDecl({0,0}, "f", "t",
                {MakeParameter({0,0}, "t", "x"), MakeParameter({0,0}, "t2", "y")},
                MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false)}, {}));
    EXPECT_EQ(Parse(M"t f(m:t x) = 1;"),
        MakeModule({ 0,0 }, "m", {}, {}, {
            MakeFunctionDecl({ 0,0 }, "f", "t",
                { MakeParameter({ 0,0 }, MakeTypeId("m", "t", {}), "x") },
                MakeLiteral({ 0,0 }, llfp::lex::tok_integer, "1"), false) }, {}));

    // parameterized types
    EXPECT_EQ(Parse(M"t[x] f() = 1;"),
        MakeModule({ 0,0 }, "m", {}, {}, {
            MakeFunctionDecl({ 0,0 }, "f", MakeTypeId("", "t", { MakeTypeId("", "x", {}) }), {},
            MakeLiteral({ 0,0 }, llfp::lex::tok_integer, "1"), false) }, {}));
    EXPECT_EQ(Parse(M"f(x[y] z) = 1;"),
        MakeModule({ 0,0 }, "m", {}, {}, {
            MakeFunctionDecl({ 0,0 }, "f", "", { MakeParameter({0,0}, MakeTypeId("", "x", { MakeTypeId("", "y", {}) }), "z") },
            MakeLiteral({ 0,0 }, llfp::lex::tok_integer, "1"), false) }, {}));
    EXPECT_EQ(Parse(M"x[y[z]] f() = 1;"),
        MakeModule({ 0,0 }, "m", {}, {}, {
            MakeFunctionDecl({ 0,0 }, "f", MakeTypeId("", "x", { MakeTypeId("", "y", { MakeTypeId("", "z", {}) }) }), {},
            MakeLiteral({ 0,0 }, llfp::lex::tok_integer, "1"), false) }, {}));
    EXPECT_EQ(Parse(M"x[y,z] f() = 1;"),
        MakeModule({ 0,0 }, "m", {}, {}, {
            MakeFunctionDecl({ 0,0 }, "f", MakeTypeId("", "x", { MakeTypeId("", "y", {}), MakeTypeId("", "z", {}) }), {},
            MakeLiteral({ 0,0 }, llfp::lex::tok_integer, "1"), false) }, {}));

    // negative
    EXPECT_EQ(ParseError(M"f = ;"), "string(2,5): expected an expression\n");
    EXPECT_EQ(ParseError(M"m2: f = 1;"), "string(2,7): expected an identifier\n");
    EXPECT_EQ(ParseError(M"f(x[]) = 1;"), "string(2,4): empty type list\n");
    EXPECT_EQ(ParseError(M"f(x[b]) = 1;"), "string(2,7): expected an identifier\n");
    EXPECT_EQ(ParseError(M"a[b]() = 1;"), "string(2,5): expected an identifier\n");
    EXPECT_EQ(ParseError(M"a[] b() = 1;"), "string(2,2): empty type list\n");
}

TEST(ParserTest, Declarations)
{
    EXPECT_EQ(Parse(
        M"data a{}"
         "f = 1;"
         "data b{}"
         "g = 2;"),
        MakeModule({ 0,0 }, "m", {}, {},
            { MakeFunctionDecl({0,0},"f", "", {}, MakeLiteral({ 0,0 }, llfp::lex::tok_integer, "1"), false),
            MakeFunctionDecl({ 0,0 },"g", "", {}, MakeLiteral({ 0,0 }, llfp::lex::tok_integer, "2"), false) },
            { MakeDataDecl({ 0,0 }, "a",{}, false),
            MakeDataDecl({ 0,0 }, "b",{}, false) }));

    // negative
}

TEST(ParserTest, Literals)
{
    // integer
    EXPECT_EQ(Parse(M"f = 1;"),
        MakeModule({0,0}, "m", {}, {}, {MakeFunctionDecl({0,0}, "f", "", {}, MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false)}, {}));
    // float
    EXPECT_EQ(Parse(M"f = 1.0;"),
        MakeModule({0,0}, "m", {}, {}, {MakeFunctionDecl({0,0}, "f", "", {}, MakeLiteral({0,0}, llfp::lex::tok_float, "1.0"), false)}, {}));
    // char
    EXPECT_EQ(Parse(M"f = \'c\';"),
        MakeModule({0,0}, "m", {}, {}, {MakeFunctionDecl({0,0}, "f", "", {}, MakeLiteral({0,0}, llfp::lex::tok_char, "c"), false)}, {}));
    // string
    EXPECT_EQ(Parse(M"f = \"ac\";"),
        MakeModule({0,0}, "m", {}, {}, {MakeFunctionDecl({0,0}, "f", "", {}, MakeLiteral({0,0}, llfp::lex::tok_string, "ac"), false)}, {}));
    // bool
    EXPECT_EQ(Parse(M"f = true;"),
        MakeModule({0,0}, "m", {}, {}, {MakeFunctionDecl({0,0}, "f", "", {}, MakeLiteral({0,0}, llfp::lex::tok_bool, "true"), false)}, {}));
    EXPECT_EQ(Parse(M"f = false;"),
        MakeModule({0,0}, "m", {}, {}, {MakeFunctionDecl({0,0}, "f", "", {}, MakeLiteral({0,0}, llfp::lex::tok_bool, "false"), false)}, {}));

    // negative
    // invalid string escape
}

TEST(ParserTest, LetExp)
{
    EXPECT_EQ(Parse(M"f = let x = 1; in 2;"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeLet({0,0},
                                                   {MakeFunctionDecl({0,0}, "x", "", {},
                                                                     MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false)},
                                                   MakeLiteral({0,0}, llfp::lex::tok_integer, "2")), false)}, {}));
    EXPECT_EQ(Parse(M"f = let x = 1; y = 2; in 3;"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeLet({0,0},
                                                   {MakeFunctionDecl({0,0}, "x", "", {},
                                                                     MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false),
                                                    MakeFunctionDecl({0,0}, "y", "", {},
                                                                     MakeLiteral({0,0}, llfp::lex::tok_integer, "2"), false)},
                                                   MakeLiteral({0,0}, llfp::lex::tok_integer, "3")), false)}, {}));

    // negative
    EXPECT_EQ(ParseError(M"f = let x = 1; 2;"),          "string(2,16): expected an identifier\n");
    EXPECT_EQ(ParseError(M"f = let x = 1; y 2;"),        "string(2,18): expected 'equal'\n");
    EXPECT_EQ(ParseError(M"f = let x = 1 y = 2; in 3;"), "string(2,15): expected 'semicolon'\n");
}

TEST(ParserTest, IfExp)
{
    EXPECT_EQ(Parse(M"f = if x then y else z;"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeIf({0,0},
                                                  MakeVariable({0,0}, "", "x"),
                                                  MakeVariable({0,0}, "", "y"),
                                                  MakeVariable({0,0}, "", "z")), false)}, {}));

    // negative
    EXPECT_EQ(ParseError(M"f = if x,a then y else z;"), "string(2,9): expected 'then'\n");
}

TEST(ParserTest, CaseExp)
{
}

TEST(ParserTest, BinaryExp)
{
    EXPECT_EQ(Parse(M"f = x + y;"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeBinary({0,0}, "+",
                                                      MakeVariable({0,0}, "", "x"),
                                                      MakeVariable({0,0}, "", "y")), false)}, {}));
    EXPECT_EQ(Parse(M"f = x + y + z;"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeBinary({0,0}, "+",
                                                      MakeBinary({0,0}, "+",
                                                                 MakeVariable({0,0}, "", "x"),
                                                                 MakeVariable({0,0}, "", "y")),
                                                      MakeVariable({0,0}, "", "z")), false)}, {}));
    EXPECT_EQ(Parse(M"f = x + y * z;"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeBinary({0,0}, "+",
                                                      MakeVariable({0,0}, "", "x"),
                                                      MakeBinary({0,0}, "*",
                                                                 MakeVariable({0,0}, "", "y"),
                                                                 MakeVariable({0,0}, "", "z"))), false)}, {}));
    EXPECT_EQ(Parse(M"f = x * y + z;"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeBinary({0,0}, "+",
                                                      MakeBinary({0,0}, "*",
                                                                 MakeVariable({0,0}, "", "x"),
                                                                 MakeVariable({0,0}, "", "y")),
                                                      MakeVariable({0,0}, "", "z")), false)}, {}));
    
    EXPECT_EQ(Parse(M"f = x * (y + z);"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeBinary({0,0}, "*",
                                                      MakeVariable({0,0}, "", "x"),
                                                      MakeBinary({0,0}, "+",
                                                                 MakeVariable({0,0}, "", "y"),
                                                                 MakeVariable({0,0}, "", "z"))), false)}, {}));
    EXPECT_EQ(Parse(M"f = (x + y) * z;"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeBinary({0,0}, "*",
                                                      MakeBinary({0,0}, "+",
                                                                 MakeVariable({0,0}, "", "x"),
                                                                 MakeVariable({0,0}, "", "y")),
                                                      MakeVariable({0,0}, "", "z")), false)}, {}));

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
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeCall({0,0}, "", "f2",
                                                    {MakeVariable({0,0}, "", "x")}), false)}, {}));
    // multiple params
    EXPECT_EQ(Parse(M"f = f2(x,y);"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeCall({0,0}, "", "f2",
                                                    {MakeVariable({0,0}, "", "x"),
                                                     MakeVariable({0,0}, "", "y")}), false)}, {}));
    
    // local function
    
    // imported function
    EXPECT_EQ(Parse(M"f = m2:f1(x);"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeCall({0,0}, "m2", "f1",
                                                    {MakeVariable({0,0}, "", "x")}), false)}, {}));
    EXPECT_EQ(Parse(M"f = m2:f1(x,y);"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeCall({0,0}, "m2", "f1",
                                                    {MakeVariable({0,0}, "", "x"),
                                                     MakeVariable({0,0}, "", "y")}), false)}, {}));
    
    // exp in params
    EXPECT_EQ(Parse(M"f = f2(x+y, f3(z));"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeCall({0,0}, "", "f2",
                                                    {MakeBinary({0,0}, "+",
                                                                MakeVariable({0,0}, "", "x"),
                                                                MakeVariable({0,0}, "", "y")),
                                                     MakeCall({0,0}, "", "f3",
                                                        {MakeVariable({0,0}, "", "z")})}), false)}, {}));

    // higher order function
    "f = f2()()";

    // negative
    EXPECT_EQ(ParseError(M"f = f2(x y);"), "string(2,10): expected 'comma'\n");
}

TEST(ParserTest, VariableExp)
{
    EXPECT_EQ(Parse(M"f = x;"),
              MakeModule({0,0}, "m", {}, {}, {MakeFunctionDecl({0,0}, "f", "", {}, MakeVariable({0,0}, "", "x"), false)}, {}));
    EXPECT_EQ(Parse(M"f = m:x;"),
              MakeModule({0,0}, "m", {}, {}, {MakeFunctionDecl({0,0}, "f", "", {}, MakeVariable({0,0}, "m", "x"), false)}, {}));

    // negative
    EXPECT_EQ(ParseError(M"f = m:;"), "string(2,7): expected an identifier\n");
    EXPECT_EQ(ParseError(M"f = m:x:z;"), "string(2,8): expected 'semicolon'\n");
}

TEST(ParserTest, FieldExp)
{
    EXPECT_EQ(Parse(M"f = x.y;"),
        MakeModule({ 0,0 }, "m", {}, {}, { MakeFunctionDecl({0,0}, "f", "", {}, MakeField({0,0}, MakeVariable({0,0}, "", "x"), "y"), false) }, {}));
    EXPECT_EQ(Parse(M"f = x.y.z;"),
        MakeModule({ 0,0 }, "m", {}, {}, { MakeFunctionDecl({0,0}, "f", "", {}, MakeField({0,0}, MakeField({0,0}, MakeVariable({0,0}, "", "x"), "y"), "z"), false) }, {}));
    EXPECT_EQ(Parse(M"f = x:y.z;"),
        MakeModule({ 0,0 }, "m", {}, {}, { MakeFunctionDecl({0,0}, "f", "", {}, MakeField({0,0}, MakeVariable({0,0}, "x", "y"), "z"), false) }, {}));
    EXPECT_EQ(Parse(M"f = x().y;"),
        MakeModule({ 0,0 }, "m", {}, {}, { MakeFunctionDecl({ 0,0 }, "f", "", {}, MakeField({ 0,0 }, MakeCall({0,0}, "", "x", {}), "y"), false) }, {}));

    // negative
    EXPECT_EQ(ParseError(M"f = x.);"), "string(2,7): expected a field identifier\n");
}

TEST(ParserTest, Comments)
{
}
