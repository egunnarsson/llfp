#pragma once

#include <memory>

#pragma warning(push, 0)

#include "llvm/Support/raw_ostream.h"

#pragma warning(pop)

#include "Ast.h"
#include "Lexer.h"
#include "Log.h"


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
    std::unique_ptr<T>        error(const char *msg);
    bool                      expect(lex::Token token);

    template<llfp::lex::Token StartToken = llfp::lex::tok_open_parenthesis,
             llfp::lex::Token EndToken = llfp::lex::tok_close_parenthesis,
             class F>
    bool                      parseList(F parseElement);
    bool                      parseDeclaration(const std::unique_ptr<ast::Module> &module);

    std::unique_ptr<ast::DataDeclaration>  parseData(bool exported);
    std::unique_ptr<ast::Function>         parseFunction(bool exported);
    std::unique_ptr<ast::ClassDeclaration> parseClass();
    std::unique_ptr<ast::ClassInstance>    parseInstance();
    std::unique_ptr<ast::FunctionDecl>     parseFunctionDefinition();

    std::unique_ptr<ast::Exp> parseLiteralExp();
    std::unique_ptr<ast::Exp> parseParenthesizedExp();
    std::unique_ptr<ast::Exp> parseIdentifierExp();
    std::unique_ptr<ast::Exp> parseCallExp(SourceLocation location, GlobalIdentifier identifier);
    std::unique_ptr<ast::Exp> parseIfExp();
    std::unique_ptr<ast::Exp> parseLetExp();
    std::unique_ptr<ast::Exp> parsePrimaryExp(); // What is this? more like Term
    std::unique_ptr<ast::Exp> parseUnaryExp();
    std::unique_ptr<ast::Exp> parseBinaryExp(int exprPrec, std::unique_ptr<ast::Exp> LHS);
    std::unique_ptr<ast::Exp> parseExp();

    std::unique_ptr<ast::NamedArgument> parseNamedArgument();

    GlobalIdentifier          parseGlobalIdentifier();
};

} // namespace parse
} // namespace llfp
