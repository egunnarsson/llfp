#pragma once

#pragma warning(push, 0)

#include <memory>

#include "llvm/Support/raw_ostream.h"

#pragma warning(pop)

#include "Ast.h"
#include "Lexer.h"


namespace llfp
{
namespace parse
{

class Parser
{
    lex::Lexer *lexer;

public:

    Parser(lex::Lexer *lexer_):
        lexer{ lexer_ }
    {}

    std::unique_ptr<ast::Module> parse();

private:

    template<class T>
    std::unique_ptr<T> Error(const char *msg)
    {
        llvm::errs() << msg;
        __debugbreak();
        return nullptr;
    }
    bool expect(lex::Token token);

    bool parseDeclaration(std::unique_ptr<ast::Module> &module);

    std::unique_ptr<ast::FunctionDeclaration> parseFunction();

    std::unique_ptr<ast::Exp> parseLiteralExp();
    std::unique_ptr<ast::Exp> parseParenthesizedExp();
    std::unique_ptr<ast::Exp> parseIdentifierExp();
    std::unique_ptr<ast::Exp> parseIfExp();
    std::unique_ptr<ast::Exp> parseLetExp();
    std::unique_ptr<ast::Exp> parsePrimaryExp(); // What is this? more like Term
    std::unique_ptr<ast::Exp> parseUnaryExp();
    std::unique_ptr<ast::Exp> parseBinaryExp(int exprPrec, std::unique_ptr<ast::Exp> LHS);
    std::unique_ptr<ast::Exp> parseExp();
};

} // namespace parse
} // namespace llfp
