
#pragma warning(push, 0)

#include <stdio.h>

#include "llvm/Support/FormatVariadic.h"

#pragma warning(pop)

#include "Lexer.h"


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

int main()
{
    runLexer({
        "'",
        "''",
        "'\\k'",
        "'\\",
        "'a 1",
        "1111 <= \"\" 'a' data else #commnet\n1.1",
        "()",
        " a_ a_b a' a'a a'' a''*b_"
    });

    getchar();
}