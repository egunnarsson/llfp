
#include "llfp.h"
#include "Codegen.h"
#include "Lexer.h"
#include "Module.h"
#include "Parser.h"

#include "gtest/gtest.h"

std::vector<llfp::CompiledModule> compile(const char *string)
{
    std::vector<std::unique_ptr<llfp::lex::Input>> input;
    input.push_back(std::make_unique<llfp::lex::StringInput>(string));
    return llfp::compile(input);
}

std::string compileError(const char *string)
{
    testing::internal::CaptureStderr();
    EXPECT_THROW(compile(string), llfp::ReturnCode);
    return testing::internal::GetCapturedStderr();
}

template<size_t N>
std::vector<llfp::CompiledModule> compile(std::array<const char*, N> source)
{
    std::vector<std::unique_ptr<llfp::lex::Input>> inputs;
    for (size_t i = 0; i < N; i++)
    {
        inputs.push_back(std::make_unique<llfp::lex::StringInput>(source[i]));
    }

    return llfp::compile(inputs);
}

bool empty(llvm::Function *f)
{
    if (f->getInstructionCount() == 0) { return true; }
    std::string str;
    llvm::raw_string_ostream ss(str);
    ss << *f->getBasicBlockList().begin()->getInstList().begin();
    return str.find("ret void") != std::string::npos;
}

#define M "module m;\n"

TEST(CodegenTest, Functions)
{
    auto result = compile(M"i32 x = 1; export i32 y = x;");
    ASSERT_NE(result[0].llvmModule, nullptr);

    auto m = result[0].llvmModule.get();

    EXPECT_EQ(m->getName(), "m");
    EXPECT_NE(m->getFunction("m:x$i32"), nullptr);
    EXPECT_NE(m->getFunction("m_y"), nullptr);

    // negative
    EXPECT_EQ(compileError(M"export i32 f() = x();"),           "string(2,18): undefined function \"x\"\n");
    EXPECT_EQ(compileError(M"export i32 f(i32 x, i32 x) = 1;"), "string(2,21): duplicate parameter \"x\"\n");
    EXPECT_EQ(compileError(M"f = 1;\nf = 2;"),                  "string(3,1): function already defined\n");
}

TEST(CodegenTest, DataDeclaration)
{
    // test valid data with field and types
    auto result = compile(M"data d { i32 x; float y; }\nexport i32 f(d x) = 1;");
    ASSERT_NE(result[0].llvmModule, nullptr);

    auto llvm = result[0].llvmModule.get();
    auto func = llvm->getFunction("m_f");
    auto type = llvm::StructType::getTypeByName(llvm->getContext(), "m_d");

    // test return of custom type
    // "export m:d foo()"

    ASSERT_NE(func, nullptr);
    EXPECT_FALSE(empty(func));
    ASSERT_NE(type, nullptr);
    EXPECT_EQ(type->getNumElements(), 2u);
    EXPECT_TRUE(type->elements()[0]->isIntegerTy(32));
    EXPECT_TRUE(type->elements()[1]->isFloatTy());

    // negative
    EXPECT_EQ(compileError(M"data foo { i32 x; i32 x; }\nexport i32 f(m:foo x) = 1;"), "string(2,19): duplicate field \"x\"\n");
    EXPECT_EQ(compileError(M"data x{}\ndata x{}\nexport i32 f(m:x y) = 1;"),           "string(3,1): data already defined\n");
}

TEST(CodegenTest, DataConstructor)
{
    {
        auto result = compile(M"data d{i32 x; i32 y;}\nexport d f(i32 z) = d{z,z};");
        ASSERT_NE(result[0].llvmModule, nullptr);

        auto llvm = result[0].llvmModule.get();
        auto func = llvm->getFunction("m_f");
        auto type = llvm::StructType::getTypeByName(llvm->getContext(), "m_d");

        ASSERT_NE(func, nullptr);
        EXPECT_FALSE(empty(func));
        ASSERT_NE(type, nullptr);
    }
    {
        auto result = compile(
           M"data d[a]   {a x; a y;}\n"
            "data d2[a,b]{a x; b y;}\n"
            "export i32 f1() = f2().x;\nf2() = d{1,1};\n"
            "export i32 f3() = d2{1,true}.x;");
        ASSERT_NE(result[0].llvmModule, nullptr);

        auto llvm = result[0].llvmModule.get();
        auto func_f1 = llvm->getFunction("m_f1");
        ASSERT_NE(func_f1, nullptr);
        EXPECT_FALSE(empty(func_f1));
        auto func_f3 = llvm->getFunction("m_f3");
        ASSERT_NE(func_f3, nullptr);
        EXPECT_FALSE(empty(func_f3));
    }

    // negative
    // unkown type, how do I trigger that code? type checks will fail before that always...
    //EXPECT_EQ(compileError(M"export d f(i32 x) = d{1,2};"), "string(3,1): \n");
    EXPECT_EQ(compileError(M"data d{i32 x; i32 y;}\nexport d f(i32 z) = d{z,z,z};"),   "string(3,21): incorrect number of arguments\n");
    EXPECT_EQ(compileError(M"data d{i32 x; i32 y;}\nexport d f(i32 z) = d{x=z,j=z};"), "string(3,27): unknown field name\n");
    EXPECT_EQ(compileError(M"data d{i32 x; i32 y;}\nexport d f(i32 z) = d{y=z,y=z};"), "string(3,23): incorrect field position\n");
    EXPECT_EQ(compileError(M"data d[a]{a x; a y;}\nexport i32 f1() = d{1,true}.x;"),   "string(3,23): failed to unify types, '@IntegerLiteral' with 'bool'\n");
}

TEST(CodegenTest, Modules)
{
    // call function
    auto result = compile<2>({
        "module m(foo); i32 foo = 1;",
        "module n; import m; export i32 bar = foo();" });
    ASSERT_NE(result[0].llvmModule, nullptr);
    ASSERT_NE(result[1].llvmModule, nullptr);

    EXPECT_EQ(result[0].llvmModule->getName(), "m");
    EXPECT_EQ(result[1].llvmModule->getName(), "n");
    EXPECT_NE(result[0].llvmModule->getFunction("m:foo$i32"), nullptr);
    EXPECT_NE(result[1].llvmModule->getFunction("n_bar"), nullptr);
    EXPECT_NE(result[1].llvmModule->getFunction("m:foo$i32"), nullptr);

    // import type

    // import type 2nd level reference
    // module a; data d {};
    // moudle b(foo); import a; i32 foo(a:d x) = 1; a:d bar = ...;
    // module c; import b; export i32 baz = foo(bar());

    // negative
    // import private function not allowed
    // duplicate module name
    // ambiguous function
    // ambiguous type
}

TEST(CodegenTest, TypeClass)
{
    // have a class, and an instance, and call the instance
    auto result = compile(
        M"class C a {a f(a);}\n"
        "instance C i32 {i32 f(i32 x) = 1;}\n"
        "export i32 foo() = f(2);");
    ASSERT_NE(result[0].llvmModule, nullptr);
    auto llvm = result[0].llvmModule.get();
    auto func = llvm->getFunction("m:f$i32$i32");
    ASSERT_NE(func, nullptr);
    EXPECT_FALSE(empty(func));

    // Nested type variable
    auto result2 = compile(
        M"data D[a] {a x;}\n"
        "class C a {i32 f(D[a]);}\n"
        "instance C bool {i32 f(D[bool] d) = 1;}\n"
        "export i32 foo() = f(D{true});");
    ASSERT_NE(result2[0].llvmModule, nullptr);
    auto llvm2 = result2[0].llvmModule.get();
    auto func2 = llvm2->getFunction("m:f$i32$m:D[bool]");
    ASSERT_NE(func2, nullptr);
    EXPECT_FALSE(empty(func2));

    // TODO: a bit weird with the locations of these errors
    EXPECT_EQ(compileError(
        M"class C a {a f(a);}\n"
        "instance C i32{i32 f(i32 x) = 1;}\n"
        "i32 f(i32 x) = 2;\n"
        "export i32 ff(i32 x) = f(x);"), "string(2,12): function already defined\n");
    EXPECT_EQ(compileError(
        M"class C a {a f(a);}\n"
        "instance C i32{i32 f(i32 x) = 1;}\n"
        "export i32 f(i32 x) = 2;\n"), "string(2,12): function already defined\n");
    EXPECT_EQ(compileError(
        M"class C a {i32 f(a);}\n"
        "instance C i32 {i32 f(i32 x) = 1;}\n"
        "export i32 foo() = f(true);"), "string(4,20): no instance of \"m:C bool\"\n");
}

TEST(CodegenTest, TypeCheck)
{

    //negative
    // call with wrong type
    // assign with wrong return type, let i32 x = funcReturnBool()
}

/*

T[A] {A,A} ... T{true, 1} should fail
T[A,B] {A,B}
T[A] {A, int32}
T[A] {A, Maybe[A]}

data T[A] = { A[int32] x; }; // error type variable cant have parameters

*/

// recursion
/*
data foo
{
    bar x;
}
data bar
{
    foo x;
}
*/
