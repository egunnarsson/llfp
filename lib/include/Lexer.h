#pragma once

#include <stdio.h>
#include <string>

#pragma warning(push, 0)

#include <llvm/Support/raw_ostream.h>

#pragma warning(pop)

#include "Common.h"


namespace llfp
{
namespace lex
{

enum class Token
{
    Invalid = -3,
    Error = -2,
    Eof = -1,

    //tok_function, /* "function" */
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

class Input
{
    SourceLocation location;

public:

    Input(llvm::StringRef inputFile) :
        location{ 1, 0, inputFile }
    {}
    virtual ~Input() {}

    int            getChar();
    // mark() ? to mark start of token
    SourceLocation getLocation() const { return location; };

protected:

    // "internal"
    virtual int getCharInt() = 0;
};

class StringInput : public Input
{
    const char *input;

public:

    StringInput(const char *_input) :
        Input("string"),
        input(_input)
    {
    }
    virtual ~StringInput() {}

protected:

    int getCharInt() override
    {
        if (*input == 0)
        {
            return EOF;
        }
        else
        {
            return (unsigned char)*(input++);
        }
    }
};

class FileInput : public Input
{
    FILE *file;

public:

    FileInput(const char* fileName_);
    virtual ~FileInput();

protected:

    int getCharInt() override;
};

class StdinInput : public Input
{
public:

    StdinInput() : Input("stdin") {}
    virtual ~StdinInput() {}

protected:

    int getCharInt() override;
};

class Lexer
{
    Input*         input;
    Token          currentToken;
    SourceLocation tokenLocation;
    std::string    tokenString;
    int            lastChar;
    bool           skipComments = true;

public:

    Lexer(Input *input);

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

    Token parseToken();
    Token error(const char *msg);

    Token parseIdentifier();
    Token parseNumber();
    Token parseChar();
    Token parseString();
    Token parseOperator();
    Token parseComment();
};

} // namespace lex
} // namespece llfp
