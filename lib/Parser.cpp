
#include <memory>
#include <string>
#include <utility>

#pragma warning(push, 0)

#include "llvm/Support/FormatVariadic.h"

#pragma warning(pop)

#include "Ast.h"
#include "Lexer.h"
#include "Log.h"

#include "Parser.h"


namespace
{

// TODO: This is going to be unmaintainable, depending on how many operators we will have and so on...
int precedence(const std::string &op)
{
    constexpr int invalid = -1;
    const auto size = op.size();

    if (size == 1)
    {
        switch (op[0])
        {
        case '.': return 20;
        case '*': return 19;
        case '/': return 18;
        case '%': return 17;
        case '+': return 16;
        case '-': return 15;
        case '<': return 9;
        case '>': return 11;
        case '&': return 5;
        case '|': return 4;
        case '^': return 3;
        }
    }
    else if (size == 2)
    {
        switch (op[0])
        {
        case '<':
            if (op[1] == '<') { return 14; }
            else if (op[1] == '=') { return 8; }
            else { return invalid; }
        case '>':
            if (op[1] == '>') { return 13; }
            else if (op[1] == '=') { return 10; }
            else { return invalid; }
        case '=': return op[1] == '=' ? 7 : invalid;
        case '!': return op[1] == '=' ? 6 : invalid;
        case '&': return op[1] == '&' ? 2 : invalid;
        case '|': return op[1] == '|' ? 1 : invalid;
        }
    }
    else if (size == 3)
    {
        return op[0] == '>' && op[1] == '>' && op[2] == '>' ? 12 : invalid;
    }

    return invalid;
}

} // namespace

namespace llfp
{
namespace parse
{

bool Parser::expect(lex::Token token)
{
    if (lexer->getToken() != token)
    {
        // should not be token name? but token char
        Log(lexer->getLocation(), "expected '", lex::Lexer::tokenName(token), '\'');
        return false;
    }
    lexer->nextToken(); // eat token
    return true;
}

std::unique_ptr<ast::Module> Parser::parse()
{
    auto location = lexer->getLocation();

    if (!expect(lex::tok_module)) { return nullptr; }

    if (lexer->getToken() != lex::tok_identifier)
    {
        return Error<ast::Module>("expected an identifier");
    }
    std::string name = lexer->getString();
    lexer->nextToken();

    auto module = std::make_unique<ast::Module>(location, name);

    if (lexer->getToken() == lex::tok_open_parenthesis)
    {
        lexer->nextToken();
        if (lexer->getToken() == lex::tok_close_parenthesis)
        {
            lexer->nextToken(); // eat ')'
        }
        else
        {
            while (true)
            {
                if (lexer->getToken() != lex::tok_identifier)
                {
                    return Error<ast::Module>("expected an identifier");
                }
                module->publicDeclarations.push_back(ast::PublicDeclaration(lexer->getLocation(), lexer->getString()));
                
                lexer->nextToken();
                if (lexer->getToken() == lex::tok_close_parenthesis)
                {
                    lexer->nextToken(); // eat ')'
                    break;
                }
                else
                {
                    if (!expect(lex::tok_comma)) { return nullptr; }
                }
            }
        }
    }

    if (!expect(lex::tok_semicolon)) { return nullptr; }

    while (lexer->getToken() == lex::tok_import)
    {
        auto importLocation = lexer->getLocation();
        lexer->nextToken();
        if (lexer->getToken() != lex::tok_identifier)
        {
            return Error<ast::Module>("expected an identifier");
        }
        module->imports.push_back(ast::ImportDeclaration(importLocation, lexer->getString()));
        lexer->nextToken();
        if (!expect(lex::tok_semicolon)) { return nullptr; }
    }

    while (lexer->getToken() != lex::tok_eof)
    {
        if (!parseDeclaration(module))
        {
            return nullptr;
        }
    }

    return std::move(module);
}

bool Parser::parseDeclaration(const std::unique_ptr<ast::Module> &module)
{
    bool exported = false;
    if (lexer->getToken() == lex::tok_export)
    {
        exported = true;
        lexer->nextToken();
    }
    switch (lexer->getToken())
    {
    case lex::tok_data:
        if (auto data = parseData(exported))
        {
            module->dataDeclarations.push_back(std::move(data));
            return true;
        }
        break;

    case lex::tok_class: Log(lexer->getLocation(), "class declaration not implemented"); break;
    case lex::tok_instance: Log(lexer->getLocation(), "instance declaration not implemented"); break;
    case lex::tok_identifier:
        if (auto function = parseFunction(exported))
        {
            module->functionDeclarations.push_back(std::move(function));
            return true;
        }
        break;

    default: Log(lexer->getLocation(), "unexpected token ", lex::Lexer::tokenName(lexer->getToken())); break;
    }
    return false;
}

std::unique_ptr<ast::DataDeclaration> Parser::parseData(bool exported)
{
    auto location = lexer->getLocation();

    if (!expect(lex::tok_data)) { return nullptr; }

    if (lexer->getToken() != lex::tok_identifier)
    {
        return Error<ast::DataDeclaration>("expected an identifier");
    }
    std::string name = lexer->getString();
    lexer->nextToken();

    if (!expect(lex::tok_open_brace)) { return nullptr; }

    std::vector<ast::Field> fields;
    while (lexer->getToken() != lex::tok_close_brace)
    {
        auto fieldLocation = lexer->getLocation();

        GlobalIdentifier fieldType = parseGlobalIdentifier();
        if (fieldType.name.empty())
        {
            return Error<ast::DataDeclaration>("expected an identifier");
        }

        if (lexer->getToken() != lex::tok_identifier)
        {
            return Error<ast::DataDeclaration>("expected an identifier");
        }
        std::string fieldName = lexer->getString();
        lexer->nextToken();

        if (!expect(lex::tok_semicolon)) { return nullptr; }

        fields.push_back({ fieldLocation, std::move(fieldType), std::move(fieldName) });
    }
    lexer->nextToken(); // eat }

    return std::make_unique<ast::DataDeclaration>(location, std::move(name), std::move(fields), exported);
}

std::unique_ptr<ast::FunctionDeclaration> Parser::parseFunction(bool exported)
{
    auto location = lexer->getLocation();

    GlobalIdentifier type = parseGlobalIdentifier();
    if (type.name.empty())
    {
        return Error<ast::FunctionDeclaration>("expected an identifier");
    }
    std::string identifier;

    if (lexer->getToken() != lex::tok_identifier)
    {
        if (type.moduleName.empty())
        {
            identifier = std::move(type.name);
            type.name = "";
        }
        else
        {
            // there must be an identifier since only type can have ':'
            return Error<ast::FunctionDeclaration>("expected an identifier");
        }
    }
    else
    {
        identifier = lexer->getString();
        lexer->nextToken();
    }

    std::vector<std::unique_ptr<ast::Parameter>> parameters;
    if (lexer->getToken() == lex::tok_open_parenthesis)
    {
        do
        {
            lexer->nextToken(); // eat '(' and ','
            auto paramLocation = lexer->getLocation();
            GlobalIdentifier argTypeName = parseGlobalIdentifier();
            if (argTypeName.name.empty()) { return Error<ast::FunctionDeclaration>("expected an identifier"); }

            std::string argIdentifier;
            if (lexer->getToken() != lex::tok_identifier)
            {
                if (argTypeName.moduleName.empty())
                {
                    argIdentifier = std::move(argTypeName.name);
                    argTypeName.name = "";
                }
                else
                {
                    // there must be an identifier since only type can have ':'
                    return Error<ast::FunctionDeclaration>("expected an identifier");
                }
            }
            else
            {
                argIdentifier = lexer->getString();
                lexer->nextToken();
            }

            parameters.push_back(std::make_unique<ast::Parameter>(paramLocation, std::move(argTypeName), std::move(argIdentifier)));

        } while (lexer->getToken() == lex::tok_comma);

        if (!expect(lex::tok_close_parenthesis)) { return nullptr; }
    }

    if (!expect(lex::tok_equal)) { return nullptr; }

    auto body = parseExp();
    if (!body)
    {
        return nullptr;
    }

    if (!expect(lex::tok_semicolon)) { return nullptr; }

    return std::make_unique<ast::FunctionDeclaration>(
        location,
        std::move(identifier),
        std::move(type),
        std::move(parameters),
        std::move(body),
        exported);
}

std::unique_ptr<ast::Exp> Parser::parseLiteralExp()
{
    auto location = lexer->getLocation();
    lex::Token token = lexer->getToken();
    std::string value = lexer->getString();
    lexer->nextToken();
    return std::make_unique<ast::LiteralExp>(location, token, std::move(value));
}

std::unique_ptr<ast::Exp> Parser::parseParenthesizedExp()
{
    lexer->nextToken(); // eat (.

    auto exp = parseExp();
    if (!exp)
    {
        return nullptr;
    }

    if (lexer->getToken() != lex::tok_close_parenthesis)
    {
        return nullptr;
    }
    lexer->nextToken(); // eat ).
    return exp;
}

// identifier and call
std::unique_ptr<ast::Exp> Parser::parseIdentifierExp()
{
    auto location = lexer->getLocation();

    GlobalIdentifier identifier = parseGlobalIdentifier();
    if (identifier.name.empty())
    {
        return Error<ast::Exp>("expected an identifier");
    }

    if (lexer->getToken() != lex::tok_open_parenthesis)
    {
        return std::make_unique<ast::VariableExp>(location, std::move(identifier));
    }

    // call
    lexer->nextToken(); // eat '('
    std::vector<std::unique_ptr<ast::Exp>> args;
    while (true)
    {
        if (lexer->getToken() == lex::tok_close_parenthesis)
        {
            break;
        }
        if (auto arg = parseExp())
        {
            args.push_back(std::move(arg));
        }
        else
        {
            return nullptr;
        }
        // expected ')' or ','
        if (lexer->getToken() != lex::tok_close_parenthesis && !expect(lex::tok_comma))
        {
            return nullptr;
        }
    }
    lexer->nextToken(); // eat ')'

    return std::make_unique<ast::CallExp>(location, std::move(identifier), std::move(args));
}

std::unique_ptr<ast::Exp> Parser::parseIfExp()
{
    auto location = lexer->getLocation();

    lexer->nextToken(); // eat if

    auto cond = parseExp();
    if (!cond)
    {
        return nullptr;
    }

    if (!expect(lex::tok_then)) { return nullptr; }

    auto thenExp = parseExp();
    if (!thenExp)
    {
        return nullptr;
    }

    if (!expect(lex::tok_else)) { return nullptr; }

    auto elseExp = parseExp();
    if (!elseExp)
    {
        return nullptr;
    }

    return std::make_unique<ast::IfExp>(location, std::move(cond), std::move(thenExp), std::move(elseExp));
}

std::unique_ptr<ast::Exp> Parser::parseLetExp()
{
    auto location = lexer->getLocation();

    lexer->nextToken(); // eat let

    std::vector<std::unique_ptr<ast::FunctionDeclaration>> declarations;
    while (true)
    {
        auto declaration = parseFunction(false);
        if (!declaration)
        {
            return nullptr;
        }
        declarations.push_back(std::move(declaration));

        if (lexer->getToken() == lex::tok_in)
        {
            lexer->nextToken(); // eat in
            break;
        }
    }

    auto exp = parseExp();
    if (!exp)
    {
        return nullptr;
    }

    return std::make_unique<ast::LetExp>(location, std::move(declarations), std::move(exp));
}

std::unique_ptr<ast::Exp> Parser::parsePrimaryExp()
{
    switch (lexer->getToken())
    {
    case lex::tok_identifier: return parseIdentifierExp();
    case lex::tok_integer:
    case lex::tok_float:
    case lex::tok_char:
    case lex::tok_string:
    case lex::tok_bool:       return parseLiteralExp();
    case lex::tok_if:         return parseIfExp();
    case lex::tok_let:        return parseLetExp();
    case lex::tok_open_parenthesis: return parseParenthesizedExp();
    default: return Error<ast::Exp>("expected an expression");
    }
}

std::unique_ptr<ast::Exp> Parser::parseUnaryExp()
{
    auto location = lexer->getLocation();

    if (lexer->getToken() != lex::tok_operator)
    {
        return parsePrimaryExp();
    }

    std::string op = lexer->getString();
    lexer->nextToken();
    if (auto operand = parseUnaryExp())
    {
        return std::make_unique<ast::UnaryExp>(location, std::move(op), std::move(operand));
    }
    return nullptr;
}

std::unique_ptr<ast::Exp> Parser::parseBinaryExp(int exprPrec, std::unique_ptr<ast::Exp> LHS)
{
    while (true)
    {
        int tokPrec = precedence(lexer->getString());

        if (tokPrec < exprPrec)
        {
            return LHS;
        }

        auto location = lexer->getLocation();
        std::string op = lexer->getString();
        lexer->nextToken();

        if (op == ".")
        {
            if (lexer->getToken() != lex::tok_identifier)
            {
                return Error<ast::Exp>("expected a field identifier");
            }

            std::string fieldIdentifier = lexer->getString();
            lexer->nextToken();

            LHS = std::make_unique<ast::FieldExp>(location, std::move(LHS), std::move(fieldIdentifier));
        }
        else
        {
            auto RHS = parseUnaryExp();
            if (!RHS)
            {
                return nullptr;
            }

            int nextPrec = precedence(lexer->getString());
            if (tokPrec < nextPrec)
            {
                RHS = parseBinaryExp(tokPrec + 1, std::move(RHS));
                if (!RHS)
                {
                    return nullptr;
                }
            }

            LHS = std::make_unique<ast::BinaryExp>(location, op, std::move(LHS), std::move(RHS));
        }
    }
}

std::unique_ptr<ast::Exp> Parser::parseExp()
{
    auto LHS = parseUnaryExp();
    if (!LHS)
    {
        return nullptr;
    }

    return parseBinaryExp(0, std::move(LHS));
}


GlobalIdentifier Parser::parseGlobalIdentifier()
{
    GlobalIdentifier identifier;

    if (lexer->getToken() != lex::tok_identifier) { return {}; }
    identifier.name = lexer->getString();

    if (lexer->nextToken() == lex::tok_colon)
    {
        if (lexer->nextToken() != lex::tok_identifier) { return {}; }
        identifier.moduleName = std::move(identifier.name);
        identifier.name = lexer->getString();
        lexer->nextToken();
    }

    return identifier;
}

} // namespace parse
} // namespace hpfp
