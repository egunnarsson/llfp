
#include "Codegen.h"

#include "Module.h"
#include "Parser.h"

#include "gtest/gtest.h"

std::unique_ptr<llfp::SourceModule> compile1(const char *string)
{
    auto input = llfp::lex::StringInput(string);
    auto lexer = llfp::lex::Lexer(&input);
    auto parser = llfp::parse::Parser(&lexer);

    auto module = parser.parse();

    std::unique_ptr<llfp::SourceModule> srcModule = std::make_unique<llfp::SourceModule>("string");
    if (module) { srcModule->setAST(std::move(module)); }

    return srcModule;
}

std::unique_ptr<llfp::SourceModule> compile(const char *string)
{
    auto srcModule = compile1(string);

    if (srcModule->getAST() != nullptr)
    {
        srcModule->createCodeGenerator();
        srcModule->generateExportedFunctions();
        while (srcModule->generateNextFunction()) {}
    }

    return srcModule;
}

std::string compileError(const char *string)
{
    testing::internal::CaptureStderr();
    compile(string);
    return testing::internal::GetCapturedStderr();
}

template<size_t N>
std::array<std::unique_ptr<llfp::SourceModule>, N> compile(std::array<const char*, N> source)
{
    std::array<std::unique_ptr<llfp::SourceModule>, N> modules;
    std::vector<llfp::ImportedModule*> modules2;

    for (size_t i = 0; i < N; i++)
    {
        modules[i] = compile1(source[i]);
        modules2.push_back(modules[i].get());
    }

    for (auto &sourceModule : modules)
    {
        if (sourceModule->getAST() == nullptr) { return modules ; }
        sourceModule->addImportedModules(modules2);
    }

    for (auto &sourceModule : modules)
    {
        sourceModule->createCodeGenerator();
        sourceModule->generateExportedFunctions();
    }

    bool done = false;
    while (!done)
    {
        done = true;
        for (auto &sourceModule : modules)
        {
            if (sourceModule->generateNextFunction())
            {
                while (sourceModule->generateNextFunction()) {}
                done = false;
            }
        }
    }

    return modules;
}

#define M "module m;\n"

TEST(CodegenTest, Functions)
{
    auto llfpModule = compile(M"i32 x = 1; export i32 y = x;");
    ASSERT_NE(llfpModule->getAST(), nullptr);

    auto m = llfpModule->getLLVM();

    EXPECT_EQ(m->getName(), "m");
    EXPECT_NE(m->getFunction("m.x$.i32"), nullptr);
    EXPECT_NE(m->getFunction("m_y"), nullptr);

    // negative
    EXPECT_EQ(compileError(M"export i32 f(i32 x, i32 x) = 1;"), "string(2,21): duplicate parameter \"x\"\n");
    EXPECT_EQ(compileError(M"f = 1;\nf = 2;"), "string(3,1): function already defined\n");
}

TEST(CodegenTest, DataDeclaration)
{
    // test valid data with field and types
    auto llfpModule = compile(M"data d { i32 x; float y; }\nexport i32 f(d x) = 1;");
    ASSERT_NE(llfpModule->getAST(), nullptr);

    auto llvm = llfpModule->getLLVM();
    auto func = llvm->getFunction("m_f");
    auto type = llvm->getTypeByName("m_d");

    // test return of custom type
    // "export m:d foo()"

    ASSERT_NE(func, nullptr);
    EXPECT_NE(func->getInstructionCount(), 0);
    ASSERT_NE(type, nullptr);
    EXPECT_EQ(type->getNumElements(), 2);
    EXPECT_TRUE(type->elements()[0]->isIntegerTy(32));
    EXPECT_TRUE(type->elements()[1]->isFloatTy());

    // negative
    EXPECT_EQ(compileError(M"data foo { i32 x; i32 x; }\nexport i32 f(m:foo x) = 1;"), "string(2,19): duplicate field \"x\"\nstring(3,14): unknown type \"m:foo\"\n");
    EXPECT_EQ(compileError(M"data x{}\ndata x{}\nexport i32 f(m:x y) = 1;"), "string(3,1): data already defined\n");
}

TEST(CodegenTest, DataConstructor)
{
    auto llfpModule = compile(M"data d{i32 x; i32 y;}\nexport d f(i32 z) = d{z,z};");
    ASSERT_NE(llfpModule->getAST(), nullptr);

    auto llvm = llfpModule->getLLVM();
    auto func = llvm->getFunction("m_f");
    auto type = llvm->getTypeByName("m_d");

    ASSERT_NE(func, nullptr);
    EXPECT_NE(func->getInstructionCount(), 0);
    ASSERT_NE(type, nullptr);

    // what happens if we fail compile first arg, but 2nd is valid?

    // negative
    // unkown type, how do I trigger that code? type checks will fail before that always...
    //EXPECT_EQ(compileError(M"export d f(i32 x) = d{1,2};"), "string(3,1): \n");
    EXPECT_EQ(compileError(M"data d{i32 x; i32 y;}\nexport d f(i32 z) = d{z,z,z};"),   "string(3,21): incorrect number of arguments\n");
    EXPECT_EQ(compileError(M"data d{i32 x; i32 y;}\nexport d f(i32 z) = d{x=z,j=z};"), "string(3,27): unknown field name\n");
    EXPECT_EQ(compileError(M"data d{i32 x; i32 y;}\nexport d f(i32 z) = d{y=z,x=z};"), "string(3,23): incorrect field position\n");
}

TEST(CodegenTest, Modules)
{
    // call function
    auto modules = compile<2>({
        "module m(foo); i32 foo = 1;",
        "module n; import m; export i32 bar = foo();" });

    EXPECT_EQ(modules[0]->getLLVM()->getName(), "m");
    EXPECT_EQ(modules[1]->getLLVM()->getName(), "n");
    EXPECT_NE(modules[0]->getLLVM()->getFunction("m.foo$.i32"), nullptr);
    EXPECT_NE(modules[1]->getLLVM()->getFunction("n_bar"), nullptr);
    EXPECT_NE(modules[1]->getLLVM()->getFunction("m.foo$.i32"), nullptr);

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

TEST(CodegenTest, TypeCheck)
{

    //negative
    // call with wrong type
    // assign with wrong return type, let i32 x = funcReturnBool()
}

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
