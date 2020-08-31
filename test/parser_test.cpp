
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

ModulePtr MakeModule(llfp::SourceLocation sourceLocation,
                          std::string name,
                          std::vector<PublicDeclaration> publicDeclarations,
                          std::vector<ImportDeclaration> imports,
                          std::initializer_list<movable_il<std::unique_ptr<FunctionDeclaration>>> functionDeclarations)
{
    auto module = std::make_unique<llfp::ast::Module>(sourceLocation, std::move(name));
    module->publicDeclarations = std::move(publicDeclarations);
    module->imports = std::move(imports);
    module->functionDeclarations = vector_from_il(functionDeclarations);
    return module;
}

auto MakeFunctionDecl(llfp::SourceLocation location,
                      std::string name,
                      std::string typeName,
                      std::initializer_list<movable_il<std::unique_ptr<Parameter>>> parameters,
                      std::unique_ptr<Exp> functionBody,
                      bool exported)
{
    return std::make_unique<llfp::ast::FunctionDeclaration>(location, name, typeName, vector_from_il(parameters), std::move(functionBody), exported);
}

auto MakeParameter(llfp::SourceLocation location, std::string typeName, std::string name)
{
    return std::make_unique<Parameter>(location, std::move(typeName), std::move(name));
}

std::unique_ptr<Exp> MakeLiteral(llfp::SourceLocation location, llfp::lex::Token tokenType, std::string value)
{
    return std::make_unique<LiteralExp>(location, tokenType, std::move(value));
}

std::unique_ptr<Exp> MakeLet(llfp::SourceLocation location,
                             std::initializer_list<movable_il<std::unique_ptr<FunctionDeclaration>>> letStatments,
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
    return std::make_unique<CallExp>(location, std::move(moduleName), std::move(name), vector_from_il(args));
}

std::unique_ptr<Exp> MakeVariable(llfp::SourceLocation location, std::string moduleName, std::string name)
{
    return std::make_unique<VariableExp>(location, std::move(moduleName), std::move(name));
}

TEST(ParserTest, Imports)
{
    // no imports
    EXPECT_EQ(Parse("module m;"), MakeModule({0,0}, "m", {}, {}, {}));
    
    // one import
    EXPECT_EQ(Parse("module m;"
                    "import m2;"),
              MakeModule({0,0}, "m", {}, {ImportDeclaration({0,0}, "m2")}, {}));
    
    // multiple imports
    EXPECT_EQ(Parse("module m;"
                    "import m2;"
                    "import m3;"),
              MakeModule({0,0}, "m", {}, {ImportDeclaration({0,0}, "m2"), ImportDeclaration({0,0}, "m3")}, {}));
    
    // from, select symbols
}

TEST(ParserTest, PublicDeclarations)
{
    // no decls
    EXPECT_EQ(Parse("module m;"), MakeModule({0,0}, "m", {}, {}, {}));
    EXPECT_EQ(Parse("module m();"), MakeModule({0,0}, "m", {}, {}, {}));
    
    // one decls
    EXPECT_EQ(Parse("module m(x);"), MakeModule({0,0}, "m", {PublicDeclaration({0,0}, "x")}, {}, {}));
    
    // multiple decls
    EXPECT_EQ(Parse("module m(x,y);"), MakeModule({0,0}, "m", {PublicDeclaration({0,0}, "x"), PublicDeclaration({0,0}, "y")}, {}, {}));
}

#define M "module m;\n"

TEST(ParserTest, Functions)
{
    // untyped function
    EXPECT_EQ(Parse(M"f = 1;"),
        MakeModule({0,0}, "m", {}, {}, {
            MakeFunctionDecl({0,0}, "f", "", {}, MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false)}));
    
    // no params
    EXPECT_EQ(Parse(M"t f = 1;"),
        MakeModule({0,0}, "m", {}, {}, {
            MakeFunctionDecl({0,0}, "f", "t", {}, MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false)}));

    // one params
    EXPECT_EQ(Parse(M"t f(x) = 1;"),
        MakeModule({0,0}, "m", {}, {}, {
            MakeFunctionDecl({0,0}, "f", "t",
                {MakeParameter({0,0}, "", "x")},
                MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false)}));
    
    // multiple params
    EXPECT_EQ(Parse(M"t f(x,y) = 1;"),
        MakeModule({0,0}, "m", {}, {}, {
            MakeFunctionDecl({0,0}, "f", "t", 
                {MakeParameter({0,0}, "", "x"), MakeParameter({0,0}, "", "y")},
                MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false)}));
    
    // typed params
    EXPECT_EQ(Parse(M"t f(t x) = 1;"),
        MakeModule({0,0}, "m", {}, {}, {
            MakeFunctionDecl({0,0}, "f", "t",
                {MakeParameter({0,0}, "t", "x")},
                MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false)}));
    EXPECT_EQ(Parse(M"t f(t x, t2 y) = 1;"),
        MakeModule({0,0}, "m", {}, {}, {
            MakeFunctionDecl({0,0}, "f", "t",
                {MakeParameter({0,0}, "t", "x"), MakeParameter({0,0}, "t2", "y")},
                MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false)}));
}

TEST(ParserTest, Literals)
{
    // integer
    EXPECT_EQ(Parse(M"f = 1;"),
        MakeModule({0,0}, "m", {}, {}, {MakeFunctionDecl({0,0}, "f", "", {}, MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false)}));
    // float
    EXPECT_EQ(Parse(M"f = 1.0;"),
        MakeModule({0,0}, "m", {}, {}, {MakeFunctionDecl({0,0}, "f", "", {}, MakeLiteral({0,0}, llfp::lex::tok_float, "1.0"), false)}));
    // char
    EXPECT_EQ(Parse(M"f = \'c\';"),
        MakeModule({0,0}, "m", {}, {}, {MakeFunctionDecl({0,0}, "f", "", {}, MakeLiteral({0,0}, llfp::lex::tok_char, "c"), false)}));
    // string
    EXPECT_EQ(Parse(M"f = \"ac\";"),
        MakeModule({0,0}, "m", {}, {}, {MakeFunctionDecl({0,0}, "f", "", {}, MakeLiteral({0,0}, llfp::lex::tok_string, "ac"), false)}));
    // bool
    EXPECT_EQ(Parse(M"f = true;"),
        MakeModule({0,0}, "m", {}, {}, {MakeFunctionDecl({0,0}, "f", "", {}, MakeLiteral({0,0}, llfp::lex::tok_bool, "true"), false)}));
    EXPECT_EQ(Parse(M"f = false;"),
        MakeModule({0,0}, "m", {}, {}, {MakeFunctionDecl({0,0}, "f", "", {}, MakeLiteral({0,0}, llfp::lex::tok_bool, "false"), false)}));
}

TEST(ParserTest, LetExp)
{
    EXPECT_EQ(Parse(M"f = let x = 1; in 2;"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeLet({0,0},
                                                   {MakeFunctionDecl({0,0}, "x", "", {},
                                                                     MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false)},
                                                   MakeLiteral({0,0}, llfp::lex::tok_integer, "2")), false)}));
    EXPECT_EQ(Parse(M"f = let x = 1; y = 2; in 3;"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeLet({0,0},
                                                   {MakeFunctionDecl({0,0}, "x", "", {},
                                                                     MakeLiteral({0,0}, llfp::lex::tok_integer, "1"), false),
                                                    MakeFunctionDecl({0,0}, "y", "", {},
                                                                     MakeLiteral({0,0}, llfp::lex::tok_integer, "2"), false)},
                                                   MakeLiteral({0,0}, llfp::lex::tok_integer, "3")), false)}));
}

TEST(ParserTest, IfExp)
{
    EXPECT_EQ(Parse(M"f = if x then y else z;"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeIf({0,0},
                                                  MakeVariable({0,0}, "", "x"),
                                                  MakeVariable({0,0}, "", "y"),
                                                  MakeVariable({0,0}, "", "z")), false)}));
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
                                                      MakeVariable({0,0}, "", "y")), false)}));
    EXPECT_EQ(Parse(M"f = x + y + z;"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeBinary({0,0}, "+",
                                                      MakeBinary({0,0}, "+",
                                                                 MakeVariable({0,0}, "", "x"),
                                                                 MakeVariable({0,0}, "", "y")),
                                                      MakeVariable({0,0}, "", "z")), false)}));
    EXPECT_EQ(Parse(M"f = x + y * z;"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeBinary({0,0}, "+",
                                                      MakeVariable({0,0}, "", "x"),
                                                      MakeBinary({0,0}, "*",
                                                                 MakeVariable({0,0}, "", "y"),
                                                                 MakeVariable({0,0}, "", "z"))), false)}));
    EXPECT_EQ(Parse(M"f = x * y + z;"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeBinary({0,0}, "+",
                                                      MakeBinary({0,0}, "*",
                                                                 MakeVariable({0,0}, "", "x"),
                                                                 MakeVariable({0,0}, "", "y")),
                                                      MakeVariable({0,0}, "", "z")), false)}));
    
    EXPECT_EQ(Parse(M"f = x * (y + z);"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeBinary({0,0}, "*",
                                                      MakeVariable({0,0}, "", "x"),
                                                      MakeBinary({0,0}, "+",
                                                                 MakeVariable({0,0}, "", "y"),
                                                                 MakeVariable({0,0}, "", "z"))), false)}));
    EXPECT_EQ(Parse(M"f = (x + y) * z;"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeBinary({0,0}, "*",
                                                      MakeBinary({0,0}, "+",
                                                                 MakeVariable({0,0}, "", "x"),
                                                                 MakeVariable({0,0}, "", "y")),
                                                      MakeVariable({0,0}, "", "z")), false)}));
}

TEST(ParserTest, UnaryExp)
{
}

TEST(ParserTest, CallExp)
{
    // one param
    EXPECT_EQ(Parse(M"f = f2(x);"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeCall({0,0}, "", "f2",
                                                    {MakeVariable({0,0}, "", "x")}), false)}));
    // multiple params
    EXPECT_EQ(Parse(M"f = f2(x,y);"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeCall({0,0}, "", "f2",
                                                    {MakeVariable({0,0}, "", "x"),
                                                     MakeVariable({0,0}, "", "y")}), false)}));
    
    // local function
    
    // imported function
    EXPECT_EQ(Parse(M"f = m2:f1(x);"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeCall({0,0}, "m2", "f1",
                                                    {MakeVariable({0,0}, "", "x")}), false)}));
    EXPECT_EQ(Parse(M"f = m2:f1(x,y);"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeCall({0,0}, "m2", "f1",
                                                    {MakeVariable({0,0}, "", "x"),
                                                     MakeVariable({0,0}, "", "y")}), false)}));
    
    // exp in params
    EXPECT_EQ(Parse(M"f = f2(x+y, f3(z));"),
              MakeModule({0,0}, "m", {}, {},
                         {MakeFunctionDecl({0,0}, "f", "", {},
                                           MakeCall({0,0}, "", "f2",
                                                    {MakeBinary({0,0}, "+",
                                                                MakeVariable({0,0}, "", "x"),
                                                                MakeVariable({0,0}, "", "y")),
                                                     MakeCall({0,0}, "", "f3",
                                                        {MakeVariable({0,0}, "", "z")})}), false)}));
}

TEST(ParserTest, VariableExp)
{
    EXPECT_EQ(Parse(M"f = x;"),
              MakeModule({0,0}, "m", {}, {}, {MakeFunctionDecl({0,0}, "f", "", {}, MakeVariable({0,0}, "", "x"), false)}));
    EXPECT_EQ(Parse(M"f = m:x;"),
              MakeModule({0,0}, "m", {}, {}, {MakeFunctionDecl({0,0}, "f", "", {}, MakeVariable({0,0}, "m", "x"), false)}));
}

TEST(ParserTest, Comments)
{
}
