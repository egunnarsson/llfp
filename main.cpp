
#pragma warning(push, 0)

#include <stdio.h>

#include "llvm/Support/FormatVariadic.h"

#pragma warning(pop)

#include "Codegen.h"
#include "Lexer.h"
#include "Parser.h"


void runLexer(const char *stringInput)
{
    auto input = llfp::lex::StringInput(stringInput);
    auto lexer = llfp::lex::Lexer(&input);
    while (true)
    {
        auto token = lexer.getToken();
        auto location = lexer.getLocation();
        auto string = lexer.getString();
        //printf("%5d  %5d %15s \"%s\"\n", location.Line, location.Column, lexer.tokenName(token), string.c_str());
        llvm::outs() << llvm::formatv("{0,5}  {1,5} {2,15} \"{3,}\"\n", location.Line, location.Column, lexer.tokenName(token), string.c_str());

        if (token == llfp::lex::tok_eof || token == llfp::lex::tok_error)
        {
            break;
        }

        lexer.nextToken();
    }
    puts("----------------------------------------\n");
}

void runLexer(std::initializer_list<const char*> list)
{
    for (auto e = list.begin(); e < list.end(); ++e)
    {
        runLexer(*e);
    }
}

void runParser(const char *string)
{
    runLexer(string);

    auto input = llfp::lex::StringInput(string);
    auto lexer = llfp::lex::Lexer(&input);
    auto parser = llfp::parse::Parser(&lexer);

    auto module = parser.parse();

    llfp::codegen::CodeGenerator gen;
    gen.generate(module);
}

int main()
{
    runLexer({
        "'",
        "''",
        "'\\k'",
        "'\\",
        "'a 1",
        "1111 <= \"\" 'a' data else #commnet\n1.1",
        "()"
    });

    runParser(
    "module meh\n"
    "i32 i = 1;\n"
    "i32 foo(i32 x) = 2;\n"
    "i32 bar(i32 y, i32 z) = let i32 baz = 1; in x() + x + y + baz;\n"
    "i32 meh(bool b, i32 x) = if b then 1 + x else 2;\n"
    "i32 x = meh(true, 1);");

    getchar();
}