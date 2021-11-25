
#include "Parser.h"
#include "Type/Type2.h"

#include "gtest/gtest.h"

auto Parse(const char* string)
{
    auto input = llfp::lex::StringInput(string);
    auto lexer = llfp::lex::Lexer(&input);
    auto parser = llfp::parse::Parser(&lexer);
    return parser.parse();
}

std::string ParseError(const char* string)
{
    testing::internal::CaptureStderr();
    Parse(string);
    return testing::internal::GetCapturedStderr();
}

TEST(TypeTest, First)
{
    //auto m = Parse("module m;f(b,x,y) = if b then x + y else x - y;");
    //auto m = Parse("module m;f(b,x,y) = if b then ~x else -y;");
    //auto m = Parse("module m;f(b,x,y) = if b then foo(~x) else -y;");
    //auto m = Parse("module m;f(b,x,y) = if b then foo(~x) else (-y)+x;");
    auto m = Parse("module m;f(a,b,x,y) = if b || a then foo(~x) else (-y)+x;");
    auto str = llfp::hm::test(*m->functions.front());
    std::cout << str << '\n';
}
