#pragma once

#include <memory>
#include <optional>

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

    enum class FunctionType
    {
        Internal,
        Exported,
        Instance
    };

    template<class T>
    std::unique_ptr<T>        error(const char *msg);
    bool                      expect(lex::Token token);

    template<llfp::lex::Token StartToken = llfp::lex::Token::Open_parenthesis,
             llfp::lex::Token EndToken = llfp::lex::Token::Close_parenthesis,
             class F>
    bool                      parseList(F parseElement);
    bool                      parseDeclaration(const std::unique_ptr<ast::Module> &module);

    std::unique_ptr<ast::Data>                parseData(bool exported);
    std::unique_ptr<ast::Function>            parseFunction(FunctionType funType);
    std::unique_ptr<ast::Class>               parseClass();
    std::unique_ptr<ast::ClassInstance>       parseInstance();
    std::unique_ptr<ast::FunctionDeclaration> parseFunctionDeclaration();

    std::unique_ptr<ast::Exp> parseLiteralExp();
    std::unique_ptr<ast::Exp> parseParenthesizedExp();
    std::unique_ptr<ast::Exp> parseIdentifierExp();
    std::unique_ptr<ast::Exp> parseCallExp(SourceLocation location, GlobalIdentifier identifier);
    std::unique_ptr<ast::Exp> parseIfExp();
    std::unique_ptr<ast::Exp> parseLetExp();
    std::unique_ptr<ast::Exp> parseCaseExp();
    std::unique_ptr<ast::Exp> parsePrimaryExp(); // What is this? more like Term
    std::unique_ptr<ast::Exp> parseUnaryExp();
    std::unique_ptr<ast::Exp> parseBinaryExp(int exprPrec, std::unique_ptr<ast::Exp> LHS);
    std::unique_ptr<ast::Exp> parseExp();

    std::optional<ast::NamedArgument>         parseNamedArgument();

    std::unique_ptr<ast::Pattern>             parsePattern();
    std::optional<ast::NamedArgumentPattern>  parseNamedArgumentPattern();
    std::unique_ptr<ast::ConstructorPattern>  parseConstructorPattern(GlobalIdentifier gid);

    bool                      parseGlobalIdentifier(GlobalIdentifier&);
    bool                      parseType(ast::TypeIdentifier&);
};

} // namespace parse
} // namespace llfp
