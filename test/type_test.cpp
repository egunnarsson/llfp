
#include "Error.h"
#include "Module.h"
#include "Parser.h"
#include "ResolveIdentifiers.h"
#include "Type/TypeInference.h"

#include "gtest/gtest.h"

namespace
{

auto Parse(const char* string)
{
    auto input  = llfp::Source("string", string);
    auto lexer  = llfp::lex::Lexer(&input);
    auto parser = llfp::parse::Parser(&lexer);
    auto astPtr = parser.parse();
    if (!astPtr) { throw 0; }
    auto modulePtr = llfp::SourceModule::create(std::move(astPtr));
    if (!modulePtr) { throw 0; }
    llfp::resolveIdentifiers(*modulePtr);
    return modulePtr;
}

llfp::hm::FunTypePtr Infer(llfp::SourceModule& srcModule, const char* funName)
{
    auto fun = srcModule.getFunction(funName);
    if (fun.function != nullptr)
    {
        auto annotation = llfp::hm::inferType(&srcModule, *fun.function);
        return annotation.getFun(std::string{ "m:" } + funName);
    }
    return nullptr;
}

llfp::hm::TypeAnnotation InferAnno(llfp::SourceModule& srcModule, const char* funName)
{
    auto fun = srcModule.getFunction(funName);
    if (fun.function != nullptr)
    {
        return llfp::hm::inferType(&srcModule, *fun.function);
    }
    return {};
}

std::string ParseError(const char* string)
{
    testing::internal::CaptureStderr();
    Parse(string);
    return testing::internal::GetCapturedStderr();
}

} // namespace

// auto m = Parse("module m;f(b,x,y) = if b then x + y else x - y;");
// auto m = Parse("module m;f(b,x,y) = if b then ~x else -y;");
// auto m = Parse("module m;f(b,x,y) = if b then foo(~x) else -y;");
// auto m = Parse("module m;f(b,x,y) = if b then foo(~x) else (-y)+x;");
// auto m = Parse("module m;f(a,b,x,y) = if b || a then foo(~x) else (-y)+x;");
// auto m = Parse("module m;int f(bool a, int b, int x, int y) = if b || a then foo(~x) else (-y)+x;");
// auto m = Parse("module m;f(x,y) = let int z = x + y; in x + y + z;");

TEST(TypeTest, CaseExp)
{
    auto modulePtr = Parse("module m(f); f(b,x,y) = case b of true -> x, false -> y end;");
    auto funType   = Infer(*modulePtr, "f");

    ASSERT_NE(funType, nullptr);

    auto typeName = funType->types.at(0)->str();
    EXPECT_EQ(typeName, funType->types.at(2)->str());
    EXPECT_EQ(typeName, funType->types.at(3)->str());

    EXPECT_EQ(funType->types.at(1)->str(), "bool");
}

TEST(TypeTest, IndirectTypeConstraints)
{
    {
        auto modulePtr     = Parse(R"x(
            module m(foo,bar);
            data d[a] = {a x; a y;};
            foo(z, w) = d{z,w};
            bar() = foo('a', true);
        )x");
        ASSERT_FALSE(modulePtr == nullptr);
        auto barAnnotation = InferAnno(*modulePtr, "bar");
        auto fooType       = Infer(*modulePtr, "foo");
        EXPECT_THROW(
            {
                try
                {
                    barAnnotation.addConstraint("m:foo", fooType);
                }
                catch (const llfp::Error& error)
                {
                    const std::string msg = error.what();
                    EXPECT_NE(msg.find("Failed to unify types"), std::string::npos);
                    EXPECT_NE(msg.find("'bool'"), std::string::npos);
                    EXPECT_NE(msg.find("'char'"), std::string::npos);
                    throw;
                }
            },
            llfp::Error);
    }
    {
        auto modulePtr     = Parse(R"x(
            module m(foo,bar);
            data wrap[a] = {a v;};
            data d[a] = {a x; wrap[a] y;};
            foo(z,w) = d{z,w};
            bar() = foo('a', wrap{true});
        )x");
        ASSERT_FALSE(modulePtr == nullptr);
        auto barAnnotation = InferAnno(*modulePtr, "bar");
        auto fooType       = Infer(*modulePtr, "foo");
        EXPECT_THROW(
            {
                try
                {
                    barAnnotation.addConstraint("m:foo", fooType);
                }
                catch (const llfp::Error& error)
                {
                    const std::string msg = error.what();
                    EXPECT_NE(msg.find("Failed to unify types"), std::string::npos);
                    EXPECT_NE(msg.find("'bool'"), std::string::npos);
                    EXPECT_NE(msg.find("'char'"), std::string::npos);
                    throw;
                }
            },
            llfp::Error);
    }
}
