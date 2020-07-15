
#pragma warning(push, 0)

#include <stdio.h>

#include "llvm/Support/FormatVariadic.h"

#pragma warning(pop)

#include "Codegen.h"
#include "Lexer.h"
#include "Parser.h"


void runParser(const char *string)
{
    auto input = llfp::lex::StringInput(string);
    auto lexer = llfp::lex::Lexer(&input);
    auto parser = llfp::parse::Parser(&lexer);

    auto module = parser.parse();

    // pretty print
    getchar();
}

int main()
{
    runParser(
    "module meh;\n"
    "i32 i = 1;\n"
    "i32 foo(i32 x) = 2;\n"
    "i32 bar(i32 y, i32 z) = let i32 baz = 1; in x() + x + y + baz;\n"
    "i32 meh(bool b, i32 x) = if b then 1 + x else 2;\n"
    "i32 x = meh(true, 1);\n"
    "i32 not_test(bool b, i32 i1, i32 i2) = if !b then ~i1 else i1 + i2;\n"
    "float module_test = math:cos(1.0);");
}
