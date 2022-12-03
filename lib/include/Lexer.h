#pragma once

#include "Common/SourceLocation.h"
#include "Source.h"

#pragma warning(push, 0)

#include <llvm/Support/raw_ostream.h>

#pragma warning(pop)

#include <string>

namespace llfp::lex
{

enum class Token
{
    Invalid = -3,
    Error   = -2,
    Eof     = -1,

    // tok_function, /* "function" */
    Module,
    Import,
    Export,
    Data,     /* "data"     */
    Class,    /* "class"    */
    Instance, /* "instance" */

    Identifier,
    Integer,
    Float,
    Char, // unicode support?
    String,
    Bool,

    If,
    Then,
    Else,
    Let,
    In,
    Case,
    Of,
    End,

    Operator,
    Colon,
    Semicolon,
    Open_parenthesis,
    Close_parenthesis,
    Open_bracket,
    Close_bracket,
    Open_brace,
    Close_brace,
    Comma,
    Equal,

    Comment,
};

class Lexer
{
    const Source*  source;
    SourceIterator iterator;
    int            lastChar;

    Token          currentToken;
    SourceLocation tokenLocation;
    std::string    tokenString;

    bool skipComments = true;

public:

    Lexer(const Source* source);

    // should be llvm::StringLiteral, except testing uses testing::Message << operator
    static constexpr const char* tokenName(Token token)
    {
        switch (token)
        {
        case Token::Error: return "error";
        case Token::Eof: return "eof";
        case Token::Module: return "module";
        case Token::Import: return "import";
        case Token::Export: return "export";
        case Token::Data: return "data";
        case Token::Class: return "class";
        case Token::Instance: return "instance";
        case Token::Identifier: return "identifier";
        case Token::Integer: return "integer";
        case Token::Float: return "float";
        case Token::Char: return "char";
        case Token::String: return "string";
        case Token::Bool: return "boolean";
        case Token::If: return "if";
        case Token::Then: return "then";
        case Token::Else: return "else";
        case Token::Let: return "let";
        case Token::In: return "in";
        case Token::Case: return "case";
        case Token::Of: return "of";
        case Token::End: return "end";
        case Token::Operator: return "operator";
        case Token::Colon: return "colon";
        case Token::Semicolon: return "semicolon";
        case Token::Open_parenthesis: return "parenthesis";
        case Token::Close_parenthesis: return "parenthesis";
        case Token::Open_bracket: return "bracket";
        case Token::Close_bracket: return "bracket";
        case Token::Open_brace: return "brace";
        case Token::Close_brace: return "brace";
        case Token::Comma: return "comma";
        case Token::Equal: return "equal";
        case Token::Comment: return "comment";
        default: return nullptr;
        }
    }

    Token              nextToken(); /*bool skipComment = true*/ /* should return void? */
    Token              getToken() const;
    const std::string& getString() const;
    SourceLocation     getLocation() const;

private:

    // TODO use iterator instead
    int getChar();

    Token parseToken();
    Token error(const char* msg);

    Token parseIdentifier();
    Token parseNumber();
    Token parseChar();
    Token parseString();
    Token parseOperator(bool continuation = false);
    Token parseComment();
};

} // namespace llfp::lex
