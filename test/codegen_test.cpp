
#include "Codegen.h"
#include "Lexer.h"
#include "llfp.h"
#include "Module.h"
#include "Parser.h"

#include "JIT.h"
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>

#include "gtest/gtest.h"

std::vector<llfp::CompiledModule> compile(const char* string)
{
    std::vector<llfp::Source> input = { llfp::Source{ "string", string } };
    return llfp::compile(input);
}

std::string compileError(const char* string, bool print = false)
{
    testing::internal::CaptureStderr();
    EXPECT_THROW(compile(string), llfp::ReturnCode);
    const auto msg = testing::internal::GetCapturedStderr();
    if (print)
    {
        std::cout << "\n------------\n"
                  << msg << "\n------------\n";
    }
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

template<size_t N>
std::vector<llfp::CompiledModule> compile(std::array<const char*, N> source)
{
    std::vector<llfp::Source> inputs;
    for (size_t i = 0; i < N; i++)
    {
        inputs.push_back({ "string", source[i] });
    }

    return llfp::compile(inputs);
}

bool empty(llvm::Function* f)
{
    if (f->getInstructionCount() == 0) { return true; }
    std::string              str;
    llvm::raw_string_ostream ss(str);
    ss << *f->getBasicBlockList().begin()->getInstList().begin();
    return str.find("ret void") != std::string::npos;
}

template<class Result, class... Types>
Result call(const std::unique_ptr<llfp::JIT>& jit, llvm::StringRef name, Types... args)
{
    llvm::ExitOnError check;
    auto              Sym = check(jit->lookup(name));
    auto*             FP  = (Result(*)(Types...))(intptr_t)Sym.getAddress();
    return FP(args...);
}


#define M "module m;\n"

TEST(CodegenTest, Functions)
{
    try
    {
        auto result = compile(M "i32 x = 1; export i32 y = x;");
        auto m      = result.at(0).llvmModule.get();
        ASSERT_NE(m, nullptr);
        EXPECT_EQ(m->getName(), "m");
        EXPECT_NE(m->getFunction("m:x$i32"), nullptr);
        EXPECT_NE(m->getFunction("m_y"), nullptr);
    }
    catch (...)
    {
        ADD_FAILURE();
    }

    // negative
    // clang-format off
    EXPECT_EQ(compileError(M "export i32 f() = x();"),                     "string(2,18): undefined function \"x\"\n");
    EXPECT_EQ(compileError(M "export i32 f(i32 x, i32 x) = 1;"),           "string(2,21): duplicate parameter \"x\"\n");
    EXPECT_EQ(compileError(M "f = 1;\nf = 2;"),                            "string(3,1): function already defined\n");
    EXPECT_EQ(compileError(M "export i32 f = x(1,2);\ni32 x(i32 a) = a;"), "string(2,16): incorrect number of arguments\n");
    // clang-format on
}

TEST(CodegenTest, DataDeclaration)
{
    try
    {
        // test valid data with field and types
        auto result = compile(M "data d = { i32 x; float y; };\nexport i32 f(d x) = 1;");

        auto llvm = result.at(0).llvmModule.get();
        ASSERT_NE(llvm, nullptr);
        auto func = llvm->getFunction("m_f");
        auto type = llvm::StructType::getTypeByName(llvm->getContext(), "m_d");

        // test return of custom type
        // "export m:d foo()"

        EXPECT_TRUE(func != nullptr && !empty(func));
        if (type != nullptr)
        {
            EXPECT_EQ(type->getNumElements(), 2u);
            EXPECT_TRUE(type->elements()[0]->isIntegerTy(32));
            EXPECT_TRUE(type->elements()[1]->isFloatTy());
        }
        else
        {
            ADD_FAILURE();
        }
    }
    catch (...)
    {
        ADD_FAILURE();
    }

    // negative
    // clang-format off
    EXPECT_EQ(compileError(M "data foo = { i32 x; i32 x; };\nexport i32 f(m:foo x) = 1;"), "string(2,21): duplicate field \"x\"\n");
    EXPECT_EQ(compileError(M "data x = {};\ndata x = {};\nexport i32 f(m:x y) = 1;"),      "string(3,1): data already defined\n");
    EXPECT_EQ(compileError(M "export i32 f(m:x y) = 1;"),                                  "string(2,14): undefined data type m:x\n");
    EXPECT_EQ(compileError(M "export i32 f(foo y) = 1;"),                                  "string(2,14): undefined data type foo\n");
    // clang-format on
}

TEST(CodegenTest, DataConstructor)
{
    try
    {
        auto result = compile(M "data d = {i32 x; i32 y;};\nexport d f(i32 z) = d{z,z};");

        auto llvm = result.at(0).llvmModule.get();
        ASSERT_NE(llvm, nullptr);
        auto func = llvm->getFunction("m_f");
        auto type = llvm::StructType::getTypeByName(llvm->getContext(), "m_d");

        EXPECT_TRUE(func != nullptr && !empty(func));
        EXPECT_NE(type, nullptr);
    }
    catch (...)
    {
        ADD_FAILURE();
    }

    try
    {
        auto result = compile(
            M "data d[a]    = {a x; a y;};\n"
              "data d2[a,b] = {a x; b y;};\n"
              "export i32 f1() = f2().x;\nf2() = d{1,1};\n"
              "export i32 f3() = d2{1,true}.x;");

        auto llvm = result.at(0).llvmModule.get();
        ASSERT_NE(llvm, nullptr);
        auto func_f1 = llvm->getFunction("m_f1");
        EXPECT_TRUE(func_f1 != nullptr && !empty(func_f1));
        auto func_f3 = llvm->getFunction("m_f3");
        EXPECT_TRUE(func_f3 != nullptr && !empty(func_f3));
    }
    catch (...)
    {
        ADD_FAILURE();
    }

    // negative
    // clang-format off
    // unknown type, how do I trigger that code? type checks will fail before that always...
    // EXPECT_EQ(compileError(M"export d f(i32 x) = d{1,2};"), "string(3,1): \n");
    EXPECT_EQ(compileError(M "data d = {i32 x; i32 y;};\nexport d f(i32 z) = d{z,z,z};"),   "string(3,21): incorrect number of arguments\n");
    EXPECT_EQ(compileError(M "data d = {i32 x; i32 y;};\nexport d f(i32 z) = d{x=z,j=z};"), "string(3,27): unknown field name\n");
    EXPECT_EQ(compileError(M "data d = {i32 x; i32 y;};\nexport d f(i32 z) = d{y=z,y=z};"), "string(3,23): incorrect field position\n");
    // Dont know about this one. Problem is that we now succeed in unify 'Num a' and 'bool' into 'Num bool'
    EXPECT_EQ(compileError(M "data d[a] = {a x; a y;};\nexport i32 f1() = d{1,true}.x;"),   "string(3,23): failed to unify types, '@IntegerLiteral' with 'bool'\n");
    // clang-format on
}

struct D
{
    int64_t x;
    int64_t y;
    int64_t z;
    int64_t w;
    int64_t k;
};

// https://www.agner.org/optimize/calling_conventions.pdf
TEST(CodegenTest, DataArguments)
{
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();

    llvm::ExitOnError          check;
    std::unique_ptr<llfp::JIT> jit = check(llfp::JIT::Create());

    auto m = compile(
        M "data d = {i64 x; i64 y; i64 z; i64 w; i64 k;};\n"
          "i64 foo(d x) = x.x + 1;\n"
          "export i64 bar(d x) = foo(x);\n"
          "export d baz(d x) = d{x.x + x.y + x.z + x.w + x.k, 1, 2, 3, 4};\n");
    ASSERT_NE(m.at(0).llvmModule, nullptr);
    m.at(0).llvmModule->setDataLayout(jit->getDataLayout());

    auto RT  = jit->getMainJITDylib().createResourceTracker();
    auto TSM = llvm::orc::ThreadSafeModule(std::move(m.at(0).llvmModule), std::move(m.at(0).llvmContext));
    check(jit->addModule(std::move(TSM), RT));

    auto  barSym = check(jit->lookup("m_bar"));
    auto* barFP  = (int64_t(*)(const D*))(intptr_t)barSym.getAddress();
    D     arg{ 1, 2, 3, 4, 5 };
    auto  barResult = barFP(&arg);
    EXPECT_EQ(barResult, 2);

    auto  bazSym = check(jit->lookup("m_baz"));
    auto* bazFP  = (void (*)(D*, const D*))(intptr_t)bazSym.getAddress();
    D     bazResult{};
    bazFP(&bazResult, &arg);
    EXPECT_EQ(bazResult.x, 1 + 2 + 3 + 4 + 5);
    EXPECT_EQ(bazResult.y, 1);
    EXPECT_EQ(bazResult.z, 2);
    EXPECT_EQ(bazResult.w, 3);
    EXPECT_EQ(bazResult.k, 4);
}

TEST(CodegenTest, Arithmetic)
{
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();

    llvm::ExitOnError          check;
    std::unique_ptr<llfp::JIT> jit = check(llfp::JIT::Create());

    {
        auto m = compile(
            M "calc(x,y,z,w,k)      = x + y - z * w / k;\n"
              "export float calcf() = calc(1.0, 2.0, 3.0, 4.0, 5.0);\n"
              "export i32 calci()   = calc(1, 2, 3, -4, 5);\n"
              "export u32 calcu()   = calc(1, 2, 3, 4, 5);\n"
              "export float frem(float x, float y) = x % y;\n"
              "export i32 irem(i32 x, i32 y)       = x % y;\n"
              "export u32 urem(u32 x, u32 y)       = x % y;\n"
              "export u8 shl (u8 x, u8 y)          = x << y;\n"
              "export u8 ashr(u8 x, u8 y)          = x >> y;\n"
              "export u8 lshr(u8 x, u8 y)          = x >>> y;\n"
              "export bool gt(float x, float y)    = x > y;\n"
              "export bool ge(i32 x, i32 y)        = x >= y;\n"
              "export bool lt(u32 x, u32 y)        = x < y;\n"
              "export bool le(float x, float y)    = x <= y;\n"
              "export bool eq(i32 x, i32 y)        = x == y;\n"
              "export bool ne(u32 x, u32 y)        = x != y;\n"
              "export u32 band(u32 x, u32 y)       = x & y;\n"
              "export u32 bor (u32 x, u32 y)       = x | y;\n"
              "export u32 bxor(u32 x, u32 y)       = x ^ y;\n"
              "export bool and(bool x, bool y)     = x && y;\n"
              "export bool or (bool x, bool y)     = x || y;\n");

        ASSERT_NE(m.at(0).llvmModule, nullptr);
        for (auto& name : {
                 "m_calci", "m_calcu", "m_calcf",
                 "m_frem", "m_irem", "m_urem",
                 "m_shl", "m_ashr", "m_lshr",
                 "m_gt", "m_ge", "m_lt", "m_le", "m_eq", "m_ne",
                 "m_band", "m_bor", "m_bxor",
                 "m_and", "m_or" })
        {
            ASSERT_NE(m.at(0).llvmModule->getFunction(name), nullptr);
        }

        m.at(0).llvmModule->setDataLayout(jit->getDataLayout());
        auto RT  = jit->getMainJITDylib().createResourceTracker();
        auto TSM = llvm::orc::ThreadSafeModule(std::move(m.at(0).llvmModule), std::move(m.at(0).llvmContext));
        check(jit->addModule(std::move(TSM), RT));

        // add sub mul div
        EXPECT_EQ(call<int32_t>(jit, "m_calci"), 1 + 2 - 3 * -4 / 5);
        EXPECT_EQ(call<uint32_t>(jit, "m_calcu"), (uint32_t)(1 + 2 - 3 * 4 / 5));
        EXPECT_EQ(call<float>(jit, "m_calcf"), 1.0f + 2.0f - 3.0f * 4.0f / 5.0f);

        // remainder
        EXPECT_EQ((call<float, float, float>(jit, "m_frem", 5.1f, 3.0f)), 2.1f);
        EXPECT_EQ((call<int32_t, int32_t, int32_t>(jit, "m_irem", -5, 3)), -2);
        EXPECT_EQ((call<uint32_t, uint32_t, uint32_t>(jit, "m_urem", 5, 3)), (uint32_t)2);

        // shift
        EXPECT_EQ((call<uint8_t, uint8_t, uint8_t>(jit, "m_shl", 0b1, 2)), (uint8_t)0b100);
        EXPECT_EQ((call<uint8_t, uint8_t, uint8_t>(jit, "m_ashr", 0b01000001, 1)), (uint8_t)0b00100000);
        EXPECT_EQ((call<uint8_t, uint8_t, uint8_t>(jit, "m_ashr", 0b10000001, 1)), (uint8_t)0b11000000);
        EXPECT_EQ((call<uint8_t, uint8_t, uint8_t>(jit, "m_lshr", 0b10000001, 1)), (uint8_t)0b01000000);

        // comparison
        EXPECT_EQ((call<bool, float, float>(jit, "m_gt", -1.0f, 1.0f)), false);
        EXPECT_EQ((call<bool, int32_t, int32_t>(jit, "m_ge", 2, 1)), true);
        EXPECT_EQ((call<bool, uint32_t, uint32_t>(jit, "m_lt", 1, 4)), true);
        EXPECT_EQ((call<bool, float, float>(jit, "m_le", 0.3f, 0.3f)), true);
        EXPECT_EQ((call<bool, int32_t, int32_t>(jit, "m_eq", -1, 1)), false);
        EXPECT_EQ((call<bool, uint32_t, uint32_t>(jit, "m_ne", 1, 1)), false);

        // bitwise and/or/xor
        EXPECT_EQ((call<uint32_t, uint32_t, uint32_t>(jit, "m_band", 0b1100, 0b1010)), (uint32_t)0b1000);
        EXPECT_EQ((call<uint32_t, uint32_t, uint32_t>(jit, "m_bor", 0b1100, 0b1010)), (uint32_t)0b1110);
        EXPECT_EQ((call<uint32_t, uint32_t, uint32_t>(jit, "m_bxor", 0b1100, 0b1010)), (uint32_t)0b0110);

        // logical and/or
        EXPECT_EQ((call<bool, bool, bool>(jit, "m_and", false, false)), false);
        EXPECT_EQ((call<bool, bool, bool>(jit, "m_and", true, false)), false);
        EXPECT_EQ((call<bool, bool, bool>(jit, "m_and", false, true)), false);
        EXPECT_EQ((call<bool, bool, bool>(jit, "m_and", true, true)), true);
        EXPECT_EQ((call<bool, bool, bool>(jit, "m_or", false, false)), false);
        EXPECT_EQ((call<bool, bool, bool>(jit, "m_or", true, false)), true);
        EXPECT_EQ((call<bool, bool, bool>(jit, "m_or", false, true)), true);
        EXPECT_EQ((call<bool, bool, bool>(jit, "m_or", true, true)), true);
    }

    // unary - ! ~
}

TEST(CodegenTest, Logic)
{
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();

    llvm::ExitOnError          check;
    std::unique_ptr<llfp::JIT> jit = check(llfp::JIT::Create());

    // if

    // let

    // case
    {
        auto m = compile(
            M "data a = {bool x; bool y;};\n"
              "data b = {a v1; bool v2; i32 v3; float v4; char v5;};\n"
              "export i32 foo(i32 x, i32 y) = case x of\n"
              "    1 -> 0,\n"
              "    2 -> y,\n"
              "    a -> a + 2\n"
              "end;\n"
              "export i32 bar(bool A, i32 B, float C, char D) =\n"
              "case b{a{true, false}, A, B, C, D} of\n"
              "    b{E, true, G, H,   I}   -> 1,\n"
              "    b{E, F,    1, H,   I}   -> 2,\n"
              "    b{E, F,    G, 1.0, I}   -> 3,\n"
              "    b{E, F,    G, H,   'a'} -> 4,\n"
              "    X -> 5\n"
              "end;\n");

        ASSERT_NE(m.at(0).llvmModule, nullptr);

        m.at(0).llvmModule->setDataLayout(jit->getDataLayout());
        auto RT  = jit->getMainJITDylib().createResourceTracker();
        auto TSM = llvm::orc::ThreadSafeModule(std::move(m.at(0).llvmModule), std::move(m.at(0).llvmContext));
        check(jit->addModule(std::move(TSM), RT));

        EXPECT_EQ((call<int32_t, int32_t, int32_t>(jit, "m_foo", 1, 1)), 0);
        EXPECT_EQ((call<int32_t, int32_t, int32_t>(jit, "m_foo", 2, 1)), 1);
        EXPECT_EQ((call<int32_t, int32_t, int32_t>(jit, "m_foo", 3, 1)), 5);

        EXPECT_EQ((call<int32_t, bool, int32_t, float, char>(jit, "m_bar", true, 0, 0.0f, '\0')), 1);
        EXPECT_EQ((call<int32_t, bool, int32_t, float, char>(jit, "m_bar", false, 1, 0.0f, '\0')), 2);
        EXPECT_EQ((call<int32_t, bool, int32_t, float, char>(jit, "m_bar", false, 0, 1.0f, '\0')), 3);
        EXPECT_EQ((call<int32_t, bool, int32_t, float, char>(jit, "m_bar", false, 0, 0.0f, 'a')), 4);
        EXPECT_EQ((call<int32_t, bool, int32_t, float, char>(jit, "m_bar", false, 1, 0.0f, 'a')), 2);
        EXPECT_EQ((call<int32_t, bool, int32_t, float, char>(jit, "m_bar", false, 0, 0.0f, '\0')), 5);
    }

    // field (part of data?)
}

TEST(CodegenTest, Modules)
{
    // call function
    auto result = compile<2>({ "module m(foo); i32 foo = 1;",
                               "module n; import m; export i32 bar = foo();" });
    ASSERT_NE(result.at(0).llvmModule, nullptr);
    ASSERT_NE(result.at(1).llvmModule, nullptr);

    EXPECT_EQ(result.at(0).llvmModule->getName(), "m");
    EXPECT_EQ(result.at(1).llvmModule->getName(), "n");
    EXPECT_NE(result.at(0).llvmModule->getFunction("m:foo$i32"), nullptr);
    EXPECT_NE(result.at(1).llvmModule->getFunction("n_bar"), nullptr);
    EXPECT_NE(result.at(1).llvmModule->getFunction("m:foo$i32"), nullptr);

    // import type

    // import type 2nd level reference
    // module a; data d = {};
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
    try
    {
        // have a class, and an instance, and call the instance
        auto result = compile(
            M "class C a {a f(a);}\n"
              "instance C i32 {i32 f(i32 x) = 1;}\n"
              "export i32 foo() = f(2);"); // We manage to find the instance type because the return type... and when we have the instance everything is fully qualified
        ASSERT_NE(result.at(0).llvmModule, nullptr);
        auto llvm = result.at(0).llvmModule.get();
        auto func = llvm->getFunction("m:f$i32$i32");
        EXPECT_TRUE(func != nullptr && !empty(func));
    }
    catch (...)
    {
        ADD_FAILURE();
    }

    try
    {
        // Nested type variable
        auto result2 = compile(
            M "data D[a] = {a x;};\n"
              "class C a {i32 f(D[a]);}\n"
              "instance C bool {i32 f(D[bool] d) = 1;}\n"
              "export i32 foo() = f(D{true});");
        ASSERT_NE(result2.at(0).llvmModule, nullptr);
        auto llvm2 = result2.at(0).llvmModule.get();
        auto func2 = llvm2->getFunction("m:f$i32$m:D[bool]");
        EXPECT_TRUE(func2 != nullptr && !empty(func2));
    }
    catch (...)
    {
        ADD_FAILURE();
    }

    // TODO: a bit weird with the locations of these errors
    EXPECT_EQ(compileError(
                  M "class C a {a f(a);}\n"
                    "instance C i32{i32 f(i32 x) = 1;}\n"
                    "i32 f(i32 x) = 2;\n"
                    "export i32 ff(i32 x) = f(x);"),
              "string(2,12): function already defined\n");
    EXPECT_EQ(compileError(
                  M "class C a {a f(a);}\n"
                    "instance C i32{i32 f(i32 x) = 1;}\n"
                    "export i32 f(i32 x) = 2;\n"),
              "string(2,12): function already defined\n");
    EXPECT_EQ(compileError(
                  M "class C a {i32 f(a);}\n"
                    "instance C i32 {i32 f(i32 x) = 1;}\n"
                    "export i32 foo() = f(true);"),
              "string(4,20): no instance of \"m:C bool\"\n");
}

TEST(CodegenTest, ArgumentTypeCheck)
{
    // Had an issue with getType() on different type instance types
    EXPECT_NO_THROW({
        auto result = compile(M R"x(
            data d[a]= d1{a x;}, d2{};
            export i32 f(d[i32] arg) = case arg of d1{x} -> x, d2{} -> 0 end;
        )x");
        auto m      = result.at(0).llvmModule.get();
        ASSERT_NE(m, nullptr);
        EXPECT_NE(m->getFunction("m_f"), nullptr);
    });
}

TEST(CodegenTest, Scope)
{
    EXPECT_EQ(compileError(
                  M "export i32 foo(i32 x) =\n"
                    "   if (x < 0) then\n"
                    "       let y = x + 1;\n"
                    "       in y\n"
                    "   else\n"
                    "       y + 1;"),
              "string(7,8): undefined function \"y\"\n");
}

TEST(CodegenTest, MathModule)
{
    auto m = compile(
        M "import math;\n"
          "export float foo(float x, float y) = sin(x) + cos(y);\n");
    auto linked = llfp::link("test", m);

    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();

    llvm::ExitOnError          check;
    std::unique_ptr<llfp::JIT> jit = check(llfp::JIT::Create());

    linked.llvmModule->setDataLayout(jit->getDataLayout());
    auto RT  = jit->getMainJITDylib().createResourceTracker();
    auto TSM = llvm::orc::ThreadSafeModule(std::move(linked.llvmModule), std::move(linked.llvmContext));
    check(jit->addModule(std::move(TSM), RT));

    double result = call<float, float, float>(jit, "m_foo", 1, 1); // 0.8414709848 + 0.54030230586 = 1.38177329066
    EXPECT_NEAR(result, 1.3817732, 0.0000001);
}

TEST(CodegenTest, List)
{
    EXPECT_NO_THROW({
        auto result = compile(M R"x(
            data List[a]= Elem{a x; List[a] next;}, Empty{};
            export List[i32] f() = Elem{1,Empty{}};
        )x");
        auto m      = result.at(0).llvmModule.get();
        ASSERT_NE(m, nullptr);
        EXPECT_NE(m->getFunction("m_f"), nullptr);
    });

    // Alternating, indirect recursion
    EXPECT_NO_THROW({
        auto result = compile(M R"x(
            data List1[a]= Elem1{a x; List2[a] next;}, Empty1{};
            data List2[a]= Elem2{a x; List1[a] next;}, Empty2{};
            export List1[i32] f() = Elem1{1,Empty2{}};
        )x");
        auto m      = result.at(0).llvmModule.get();
        ASSERT_NE(m, nullptr);
        EXPECT_NE(m->getFunction("m_f"), nullptr);
    });
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
data foo =
{
    bar x;
};
data bar =
{
    foo x;
};
*/
