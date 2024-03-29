
#include "Parser.h"

#include "Ast.h"
#include "Error.h"
#include "Lexer.h"
#include "Log.h"
#include "String/StringConstants.h"

#pragma warning(push, 0)

#include <llvm/Support/FormatVariadic.h>

#pragma warning(pop)

#include <memory>
#include <string>
#include <utility>


namespace
{

// TODO: This is going to be unmaintainable, depending on how many operators we will have and so on...
int precedence(const std::string& op)
{
    constexpr int invalid = -1;
    const auto    size    = op.size();

    // uint32_t hash = op[0] | op[1] << 8 | op[2] << 16;

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

namespace llfp::parse
{

template<class T>
std::unique_ptr<T> Parser::error(const char* msg)
{
    Log(lexer->getLocation(), msg);
    return nullptr;
}

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

template<llfp::lex::Token StartToken, llfp::lex::Token EndToken, class F>
bool Parser::parseList(F parseElement)
{
    assert(lexer->getToken() == StartToken);
    lexer->nextToken(); // eat '('

    if (lexer->getToken() != EndToken)
    {
        while (true)
        {
            if (!parseElement()) { return false; }

            if (lexer->getToken() == EndToken) { break; }
            else if (!expect(lex::Token::Comma)) { return false; }
        }
    }

    lexer->nextToken(); // eat ')'
    return true;
}

std::unique_ptr<ast::Module> Parser::parse()
{
    auto location = lexer->getLocation();

    if (!expect(lex::Token::Module)) { return nullptr; }

    if (lexer->getToken() != lex::Token::Identifier)
    {
        return error<ast::Module>("expected an identifier");
    }
    std::string name = lexer->getString();
    lexer->nextToken();

    auto module = std::make_unique<ast::Module>(location, name);

    if (lexer->getToken() == lex::Token::Open_parenthesis)
    {
        auto parseElement = [this, &module]() {
            if (lexer->getToken() == lex::Token::Identifier)
            {
                module->publics.push_back(ast::Public(lexer->getLocation(), lexer->getString()));
                lexer->nextToken();
                return true;
            }

            Log(lexer->getLocation(), "expected an identifier");
            return false;
        };

        if (!parseList(parseElement)) { return nullptr; }
    }

    if (!expect(lex::Token::Semicolon)) { return nullptr; }

    while (lexer->getToken() == lex::Token::Import)
    {
        auto importLocation = lexer->getLocation();
        lexer->nextToken();
        if (lexer->getToken() != lex::Token::Identifier)
        {
            return error<ast::Module>("expected an identifier");
        }
        module->imports.push_back(ast::Import(importLocation, lexer->getString()));
        lexer->nextToken();
        if (!expect(lex::Token::Semicolon)) { return nullptr; }
    }

    while (lexer->getToken() != lex::Token::Eof)
    {
        if (!parseDeclaration(module))
        {
            return nullptr;
        }
    }

    return std::move(module);
}

bool Parser::parseDeclaration(const std::unique_ptr<ast::Module>& module)
{
    bool exported = false;
    if (lexer->getToken() == lex::Token::Export)
    {
        exported = true;
        lexer->nextToken();
    }
    switch (lexer->getToken())
    {
    case lex::Token::Data:
        if (auto data = parseData(exported))
        {
            module->datas.push_back(std::move(data));
            return true;
        }
        break;

    case lex::Token::Class:
        if (exported)
        {
            Log(lexer->getLocation(), "export class");
        }
        else if (auto typeClass = parseClass())
        {
            module->classes.push_back(std::move(typeClass));
            return true;
        }
        break;

    case lex::Token::Instance:
        if (exported)
        {
            Log(lexer->getLocation(), "export instance");
        }
        else if (auto instance = parseInstance())
        {
            module->classInstances.push_back(std::move(instance));
            return true;
        }
        break;

    case lex::Token::Identifier:
        if (auto function = parseFunction(exported ? FunctionType::Exported : FunctionType::Internal))
        {
            module->functions.push_back(std::move(function));
            return true;
        }
        break;

    default: Log(lexer->getLocation(), "unexpected token ", lex::Lexer::tokenName(lexer->getToken())); break;
    }
    return false;
}

// [ <id> ] "{" { <tid> <id> ";" } "}"
std::optional<ast::DataConstructor> Parser::parseDataConstructor()
{
    auto constructorLocation = lexer->getLocation();

    std::string name;
    if (lexer->getToken() == lex::Token::Identifier)
    {
        name = lexer->getString();
        lexer->nextToken();
    }

    if (!expect(lex::Token::Open_brace)) { return std::nullopt; }

    std::vector<ast::Field> fields;
    while (lexer->getToken() != lex::Token::Close_brace)
    {
        auto fieldLocation = lexer->getLocation();

        ast::TypeIdentifier fieldType;
        if (!parseType(fieldType)) { return std::nullopt; }
        if (fieldType.identifier.name.empty())
        {
            Log(lexer->getLocation(), "expected an identifier");
            return std::nullopt;
        }

        if (lexer->getToken() != lex::Token::Identifier)
        {
            Log(lexer->getLocation(), "expected an identifier");
            return std::nullopt;
        }
        std::string fieldName = lexer->getString();
        lexer->nextToken();

        if (!expect(lex::Token::Semicolon)) { return std::nullopt; }

        fields.push_back({ fieldLocation, std::move(fieldType), std::move(fieldName) });
    }
    lexer->nextToken(); // eat "}"

    return ast::DataConstructor{ constructorLocation, std::move(name), std::move(fields) };
}

// "data" <id> [ "[" { <id> "," } <id> "]" ] "=" <constructor> { "," <constructor> } ";"
std::unique_ptr<ast::Data> Parser::parseData(bool exported)
{
    auto location = lexer->getLocation();

    if (!expect(lex::Token::Data)) { return nullptr; }

    if (lexer->getToken() != lex::Token::Identifier)
    {
        return error<ast::Data>("expected an identifier");
    }
    std::string name = lexer->getString();
    lexer->nextToken();

    std::vector<std::string> typeVariables;
    if (lexer->getToken() == lex::Token::Open_bracket)
    {
        auto parseVariable = [this, &typeVariables]() {
            if (lexer->getToken() != lex::Token::Identifier) { return false; }
            typeVariables.push_back(lexer->getString());
            lexer->nextToken();
            return true;
        };

        if (!parseList<lex::Token::Open_bracket, lex::Token::Close_bracket>(parseVariable)) { return nullptr; }
    }

    std::vector<ast::DataConstructor> constructors;
    auto                              parseConstructor = [this, &constructors]() {
        if (auto constructor = parseDataConstructor())
        {
            constructors.push_back(std::move(constructor.value()));
            return true;
        }
        else
        {
            return false;
        }
    };

    if (lexer->getToken() != lex::Token::Equal)
    {
        return error<ast::Data>("expected 'equal'");
    }

    if (!parseList<lex::Token::Equal, lex::Token::Semicolon>(parseConstructor)) { return nullptr; }

    return std::make_unique<ast::Data>(location, std::move(name), std::move(typeVariables), std::move(constructors), exported);
}

// [ <tid> ] <id> [ "(" [ { <arg> "," } <arg> ] ")" ] "=" <exp> ";"
std::unique_ptr<ast::Function> Parser::parseFunction(FunctionType funType)
{
    auto location = lexer->getLocation();

    ast::TypeIdentifier type;
    if (!parseType(type)) { return nullptr; }
    if (type.identifier.name.empty())
    {
        return error<ast::Function>("expected an identifier"); // TODO: bad error when "t[] f() = 1;"
    }

    std::string identifier;
    if (lexer->getToken() != lex::Token::Identifier)
    {
        if (funType != FunctionType::Instance &&
            type.identifier.moduleName.empty() &&
            type.parameters.empty())
        {
            identifier = std::move(type.identifier.name);
            type.identifier.name.clear();
        }
        else
        {
            // there must be an identifier since only type can have ':' or [ <tid> ]
            return error<ast::Function>("expected an identifier");
        }
    }
    else
    {
        identifier = lexer->getString();
        lexer->nextToken();
    }

    std::vector<std::unique_ptr<ast::Parameter>> parameters;
    if (lexer->getToken() == lex::Token::Open_parenthesis)
    {
        auto parseElement = [this, &parameters]() {
            auto paramLocation = lexer->getLocation();

            ast::TypeIdentifier argTypeName;
            if (!parseType(argTypeName)) { return false; }
            if (argTypeName.identifier.name.empty())
            {
                Log(lexer->getLocation(), "expected an identifier");
                return false;
            }
            std::string argIdentifier;

            if (lexer->getToken() != lex::Token::Identifier)
            {
                if (argTypeName.identifier.moduleName.empty() &&
                    argTypeName.parameters.empty())
                {
                    argIdentifier = std::move(argTypeName.identifier.name);
                    argTypeName.identifier.name.clear();
                }
                else
                {
                    // there must be an identifier since only type can have ':' or [ <tid> ]
                    Log(lexer->getLocation(), "expected an identifier");
                    return false;
                }
            }
            else
            {
                argIdentifier = lexer->getString();
                lexer->nextToken();
            }

            parameters.push_back(std::make_unique<ast::Parameter>(paramLocation, std::move(argTypeName), std::move(argIdentifier)));
            return true;
        };

        if (!parseList(parseElement)) { return nullptr; }
    }

    if (!expect(lex::Token::Equal)) { return nullptr; }

    auto body = parseExp();
    if (!body)
    {
        return nullptr;
    }

    if (!expect(lex::Token::Semicolon)) { return nullptr; }

    const bool exported = funType == FunctionType::Exported;
    return std::make_unique<ast::Function>(
        location,
        std::move(identifier),
        std::move(type),
        std::move(parameters),
        std::move(body),
        exported);
}

// "class" <id> <id> "{" <funDecl> [ <funDecl> ] "}"
std::unique_ptr<ast::Class> Parser::parseClass()
{
    auto location = lexer->getLocation();

    if (!expect(lex::Token::Class)) { return nullptr; }

    if (lexer->getToken() != lex::Token::Identifier)
    {
        return error<ast::Class>("expected an identifier");
    }
    std::string identifier = lexer->getString();
    lexer->nextToken();

    if (lexer->getToken() != lex::Token::Identifier)
    {
        return error<ast::Class>("expected an identifier");
    }
    std::string typeVar = lexer->getString();
    lexer->nextToken();

    if (!expect(lex::Token::Open_brace)) { return nullptr; }

    std::vector<std::unique_ptr<ast::FunctionDeclaration>> functions;
    while (true)
    {
        auto fun = parseFunctionDeclaration();
        if (fun == nullptr) { return nullptr; }
        functions.push_back(std::move(fun));

        if (lexer->getToken() == lex::Token::Close_brace)
        {
            break;
        }
    }
    lexer->nextToken(); // eat '}'

    return std::make_unique<ast::Class>(location, std::move(identifier), std::move(typeVar), std::move(functions));
}

//"instance" <gid> <tid> "{" { <fun> } "}"
std::unique_ptr<ast::ClassInstance> Parser::parseInstance()
{
    auto location = lexer->getLocation();

    if (!expect(lex::Token::Instance)) { return nullptr; }

    GlobalIdentifier identifier;
    if (!parseGlobalIdentifier(identifier)) { return nullptr; }
    if (identifier.name.empty())
    {
        return error<ast::ClassInstance>("expected an identifier");
    }

    ast::TypeIdentifier typeArg;
    if (!parseType(typeArg)) { return nullptr; }
    if (typeArg.identifier.name.empty())
    {
        return error<ast::ClassInstance>("expected an identifier");
    }

    if (!expect(lex::Token::Open_brace)) { return nullptr; }

    std::vector<std::unique_ptr<ast::Function>> functions;
    while (true)
    {
        auto fun = parseFunction(FunctionType::Instance);
        if (fun == nullptr) { return nullptr; }
        functions.push_back(std::move(fun));

        if (lexer->getToken() == lex::Token::Close_brace)
        {
            break;
        }
    }
    lexer->nextToken(); // eat '}'

    return std::make_unique<ast::ClassInstance>(location, std::move(identifier), std::move(typeArg), std::move(functions));
}

std::unique_ptr<ast::FunctionDeclaration> Parser::parseFunctionDeclaration()
{
    auto location = lexer->getLocation();

    ast::TypeIdentifier returnType;
    if (!parseType(returnType)) { return nullptr; }
    if (returnType.identifier.name.empty())
    {
        return error<ast::FunctionDeclaration>("expected a function declaration");
    }

    if (lexer->getToken() != lex::Token::Identifier)
    {
        return error<ast::FunctionDeclaration>("expected an identifier");
    }
    std::string identifier = lexer->getString();
    lexer->nextToken();

    std::vector<std::unique_ptr<ast::Parameter>> args;
    if (lexer->getToken() == lex::Token::Open_parenthesis)
    {
        auto parseArg = [this, &args]() {
            auto                paramLocation = lexer->getLocation();
            ast::TypeIdentifier argTypeName;
            if (!parseType(argTypeName)) { return false; }
            if (argTypeName.identifier.name.empty())
            {
                Log(lexer->getLocation(), "expected an identifier");
                return false;
            }

            std::string argIdentifier;
            if (lexer->getToken() == lex::Token::Identifier)
            {
                argIdentifier = lexer->getString();
                lexer->nextToken();
            }

            args.push_back(std::make_unique<ast::Parameter>(paramLocation, std::move(argTypeName), std::move(argIdentifier)));
            return true;
        };
        if (!parseList(parseArg)) { return nullptr; }
    }
    else
    {
        return error<ast::FunctionDeclaration>("expected 'parenthesis'");
    }

    if (!expect(lex::Token::Semicolon)) { return nullptr; }

    return std::make_unique<ast::FunctionDeclaration>(location, std::move(identifier), std::move(returnType), std::move(args));
}

std::unique_ptr<ast::Exp> Parser::parseLiteralExp()
{
    auto        location = lexer->getLocation();
    lex::Token  token    = lexer->getToken();
    std::string value    = lexer->getString();
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

    if (lexer->getToken() != lex::Token::Close_parenthesis)
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

    GlobalIdentifier identifier;
    if (!parseGlobalIdentifier(identifier)) { return nullptr; }
    if (identifier.name.empty())
    {
        return error<ast::Exp>("expected an identifier");
    }

    if (lexer->getToken() == lex::Token::Identifier)
    {
        // Foo int32 int32
    }

    if (lexer->getToken() == lex::Token::Open_parenthesis || lexer->getToken() == lex::Token::Open_brace)
    {
        return parseCallExp(location, std::move(identifier));
    }
    else
    {
        return std::make_unique<ast::VariableExp>(location, std::move(identifier));
    }
}

std::unique_ptr<ast::Exp> Parser::parseCallExp(SourceLocation location, GlobalIdentifier identifier)
{
    assert(lexer->getToken() == lex::Token::Open_parenthesis || lexer->getToken() == lex::Token::Open_brace);

    if (lexer->getToken() == lex::Token::Open_parenthesis)
    {
        // call
        std::vector<std::unique_ptr<ast::Exp>> args;
        auto                                   parseElement = [this, &args]() {
            if (auto arg = parseExp())
            {
                args.push_back(std::move(arg));
                return true;
            }
            else
            {
                return false;
            }
        };

        if (parseList(parseElement))
        {
            return std::make_unique<ast::CallExp>(location, std::move(identifier), std::move(args));
        }
    }
    else if (lexer->getToken() == lex::Token::Open_brace)
    {
        // constructor
        std::vector<ast::NamedArgument> args;
        auto                            parseElement = [this, &args]() {
            if (auto arg = parseNamedArgument())
            {
                args.push_back(std::move(arg.value()));
                return true;
            }
            else
            {
                return false;
            }
        };

        if (parseList<lex::Token::Open_brace, lex::Token::Close_brace>(parseElement))
        {
            return std::make_unique<ast::ConstructorExp>(location, std::move(identifier), std::move(args));
        }
    }

    return nullptr;
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

    if (!expect(lex::Token::Then)) { return nullptr; }

    auto thenExp = parseExp();
    if (!thenExp)
    {
        return nullptr;
    }

    if (!expect(lex::Token::Else)) { return nullptr; }

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

    std::vector<std::unique_ptr<ast::Function>> declarations;
    while (true)
    {
        auto declaration = parseFunction(FunctionType::Internal);
        if (!declaration)
        {
            return nullptr;
        }
        declarations.push_back(std::move(declaration));

        if (lexer->getToken() == lex::Token::In)
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

// <gid> "{" [ [<id> "=" ] <pattern> { "," [ <id> "="] <pattern> } ] "}"
std::unique_ptr<ast::ConstructorPattern> Parser::parseConstructorPattern(GlobalIdentifier gid)
{
    auto                                   location = lexer->getLocation();
    std::vector<ast::NamedArgumentPattern> arguments;

    auto parseElement = [this, &arguments]() {
        if (auto arg = parseNamedArgumentPattern())
        {
            arguments.push_back(std::move(arg.value()));
            return true;
        }
        else
        {
            return false;
        }
    };

    if (parseList<lex::Token::Open_brace, lex::Token::Close_brace>(parseElement))
    {
        return std::make_unique<ast::ConstructorPattern>(location, std::move(gid), std::move(arguments));
    }
    else
    {
        return nullptr;
    }
}

// [ <id> "="] <pattern>
std::optional<ast::NamedArgumentPattern> Parser::parseNamedArgumentPattern()
{
    auto location = lexer->getLocation();

    std::string                   name;
    std::unique_ptr<ast::Pattern> arg;
    if (lexer->getToken() == lex::Token::Identifier)
    {
        GlobalIdentifier ident;
        if (!parseGlobalIdentifier(ident)) { return std::nullopt; }

        assert(!ident.name.empty());

        const auto token = lexer->getToken();
        if (ident.moduleName.empty() && token == lex::Token::Equal)
        {
            // " = pattern"
            lexer->nextToken(); // eat '='
            name = std::move(ident.name);
            arg  = parsePattern();
        }
        else if (token == lex::Token::Open_brace)
        {
            // "{ ... "
            arg = parseConstructorPattern(std::move(ident));
        }
        else if (ident.moduleName.empty())
        {
            // "X, ..."
            arg = std::make_unique<ast::IdentifierPattern>(location, std::move(ident.name));
        }
        else
        {
            // error "m:i, ..."
            Log(location, "expected constructor (global id used in pattern)");
            return std::nullopt;
        }
    }
    else
    {
        arg = parsePattern();
    }

    if (!arg) { return std::nullopt; }
    return std::make_optional<ast::NamedArgumentPattern>(location, std::move(name), std::move(arg));
}

// <bool> | <string> | <char> | <float> | <int> | <id> | <constructor_pattern>
std::unique_ptr<ast::Pattern> Parser::parsePattern()
{
    auto       location = lexer->getLocation();
    const auto token    = lexer->getToken();

    if (token == lex::Token::Identifier)
    {
        GlobalIdentifier ident;
        if (!parseGlobalIdentifier(ident)) { return nullptr; }

        if (lexer->getToken() == lex::Token::Open_brace)
        {
            return parseConstructorPattern(std::move(ident));
        }
        else if (ident.moduleName.empty())
        {
            return std::make_unique<ast::IdentifierPattern>(location, std::move(ident.name));
        }
        else
        {
            Log(location, "expected constructor (global id used in pattern)");
            return nullptr;
        }
    }
    else
    {
        auto string = lexer->getString();
        lexer->nextToken();

        switch (token)
        {
        case lex::Token::Identifier: assert(false); return nullptr;
        case lex::Token::Integer: return std::make_unique<ast::IntegerPattern>(location, std::move(string));
        case lex::Token::Float: return std::make_unique<ast::FloatPattern>(location, std::move(string));
        case lex::Token::Char: return std::make_unique<ast::CharPattern>(location, std::move(string));
        case lex::Token::String: return std::make_unique<ast::StringPattern>(location, std::move(string));
        case lex::Token::Bool:
            if (string == id::True)
            {
                return std::make_unique<ast::BoolPattern>(location, true);
            }
            else
            {
                assert(string == id::False);
                return std::make_unique<ast::BoolPattern>(location, false);
            }
        default: return error<ast::Pattern>("expected a pattern");
        }
    }
}

// "case" <exp> "of" <pattern> "->" <exp> { "," <pattern> "->" <exp> } "end"
std::unique_ptr<ast::Exp> Parser::parseCaseExp()
{
    auto location = lexer->getLocation();

    lexer->nextToken(); // eat case

    auto caseExp = parseExp();

    if (!expect(lex::Token::Of)) { return nullptr; }

    std::vector<ast::Clause> clauses;
    while (true)
    {
        auto pattern = parsePattern();
        if (!pattern) { return nullptr; }

        if (lexer->getToken() != lex::Token::Operator ||
            lexer->getString() != "->")
        {
            return error<ast::Exp>("expected '->'");
        }

        lexer->nextToken(); // eat "->"

        auto exp = parseExp();
        if (!exp) { return nullptr; }

        clauses.push_back({ std::move(pattern), std::move(exp) });

        if (lexer->getToken() == lex::Token::Comma)
        {
            lexer->nextToken(); // eat ","
        }
        else
        {
            break;
        }
    }

    if (!expect(lex::Token::End)) { return nullptr; }

    return std::make_unique<ast::CaseExp>(location, std::move(caseExp), std::move(clauses));
}

std::unique_ptr<ast::Exp> Parser::parsePrimaryExp()
{
    switch (lexer->getToken())
    {
    case lex::Token::Identifier: return parseIdentifierExp();
    case lex::Token::Integer:
    case lex::Token::Float:
    case lex::Token::Char:
    case lex::Token::String:
    case lex::Token::Bool: return parseLiteralExp();
    case lex::Token::If: return parseIfExp();
    case lex::Token::Let: return parseLetExp();
    case lex::Token::Case: return parseCaseExp();
    case lex::Token::Open_parenthesis: return parseParenthesizedExp();
    case lex::Token::Intrinsic: return parseIntrinsicExp();
    default: return error<ast::Exp>("expected an expression");
    }
}

std::unique_ptr<ast::Exp> Parser::parseUnaryExp()
{
    auto location = lexer->getLocation();

    if (lexer->getToken() != lex::Token::Operator)
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

        auto        location = lexer->getLocation();
        std::string op       = lexer->getString();
        lexer->nextToken();

        if (op == ".")
        {
            if (lexer->getToken() != lex::Token::Identifier)
            {
                return error<ast::Exp>("expected a field identifier");
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

// <intrinsic> "(" [ <exp> { "," <exp> } ] ")"
std::unique_ptr<ast::Exp> Parser::parseIntrinsicExp()
{
    assert(lexer->getToken() == lex::Token::Intrinsic);

    const auto location = lexer->getLocation();
    auto       name     = lexer->getString();

    std::vector<std::unique_ptr<ast::Exp>> args;
    if (lexer->nextToken() == lex::Token::Open_parenthesis)
    {
        auto parseElement = [this, &args]() {
            if (auto arg = parseExp())
            {
                args.push_back(std::move(arg));
                return true;
            }
            else
            {
                return false;
            }
        };

        if (!parseList(parseElement))
        {
            return nullptr;
        }
    }
    else
    {
        return error<ast::Exp>("expected 'parenthesis'");
    }

    return std::make_unique<ast::IntrinsicExp>(location, std::move(name), std::move(args));
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

std::optional<ast::NamedArgument> Parser::parseNamedArgument()
{
    auto location = lexer->getLocation();

    auto        exp = parseExp();
    std::string name;

    if (!exp)
    {
        return std::nullopt;
    }

    if (lexer->getToken() == lex::Token::Equal)
    {
        ast::VariableExp* variable = nullptr;
        exp->visit([&variable](auto& typedExp) -> void {
            if constexpr (std::is_same_v<decltype(typedExp), ast::VariableExp&>)
            {
                variable = &typedExp;
            }
        });

        if (variable == nullptr)
        {
            // throw
            Log(lexer->getLocation(), "expected 'comma'");
            return std::nullopt;
        }
        else if (!variable->identifier.moduleName.empty())
        {
            // throw
            Log(location, "expected a data member identifier");
            return std::nullopt;
        }
        else
        {
            name = variable->identifier.name;
            lexer->nextToken(); // eat '='
            exp = parseExp();

            if (!exp)
            {
                return std::nullopt;
            }
        }
    }

    return std::make_optional<ast::NamedArgument>(location, std::move(name), std::move(exp));
}

// <id> [ ":" <id> ]
bool Parser::parseGlobalIdentifier(GlobalIdentifier& identifier)
{
    if (lexer->getToken() == lex::Token::Identifier)
    {
        identifier.name = lexer->getString();

        if (lexer->nextToken() == lex::Token::Colon)
        {
            if (lexer->nextToken() != lex::Token::Identifier)
            {
                Log(lexer->getLocation(), "expected an identifier");
                return false;
            }
            identifier.moduleName = std::move(identifier.name);
            identifier.name       = lexer->getString();
            lexer->nextToken();
        }
    }

    return true;
}

// <gid> [ "[" { <tid> ","} <tid> "]" ]
bool Parser::parseType(ast::TypeIdentifier& type)
{
    if (lexer->getToken() == lex::Token::Identifier)
    {
        if (!parseGlobalIdentifier(type.identifier)) { return false; }

        if (type.identifier.name.empty())
        {
            Log(lexer->getLocation(), "expected an identifier");
            return false;
        }

        if (lexer->getToken() == lex::Token::Open_bracket) // for now we use []
        {
            auto location = lexer->getLocation();

            auto parseTypeParameter = [this, &type]() {
                ast::TypeIdentifier param;
                if (!parseType(param)) { return false; }
                if (param.identifier.name.empty())
                {
                    Log(lexer->getLocation(), "expected an identifier");
                    return false;
                }
                type.parameters.push_back(std::move(param));
                return true;
            };

            if (!parseList<lex::Token::Open_bracket, lex::Token::Close_bracket>(parseTypeParameter))
            {
                return false;
            }
            if (type.parameters.empty())
            {
                Log(location, "empty type list");
                return false;
            }
        }
    }

    return true;
}

} // namespace llfp::parse
