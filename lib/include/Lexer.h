#pragma once

#include <stdio.h>
#include <string>

#pragma warning(push, 0)

#include <llvm/Support/raw_ostream.h>

#pragma warning(pop)

#include "SourceLocation.h"


namespace llfp
{
namespace lex
{

enum Token
{
    tok_invalid = -3,
    tok_error = -2,
    tok_eof = -1,

    //tok_function, /* "function" */
    tok_module,
    tok_import,
    tok_export,
    tok_data,     /* "data"     */
    tok_class,    /* "class"    */
    tok_instance, /* "instance" */

    tok_identifier,
    tok_integer,
    tok_float,
    tok_char, // unicode support?
    tok_string,
    tok_bool,

    tok_if,
    tok_then,
    tok_else,
    tok_let,
    tok_in,

    tok_operator,
    tok_colon,
    tok_semicolon,
    tok_open_parenthesis,
    tok_close_parenthesis,
    tok_comma,
    tok_equal,

    tok_comment,
};

class Input
{
    SourceLocation location;

public:

    Input(llvm::StringRef inputFile) :
        location{ 0, 0, inputFile }
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
            return *(input++);
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
    Input*      input;
    Token       currentToken;
    std::string tokenString;
    int         lastChar;
    bool        skipComments = true;

public:

    Lexer(Input *input);

    // should be string_view
    static constexpr const char* tokenName(Token token)
    {
        switch (token)
        {
        case tok_error: return "error";
        case tok_eof: return "eof";
        case tok_module: return "module";
        case tok_import: return "import";
        case tok_export: return "export";
        case tok_data: return "data";
        case tok_class: return "class";
        case tok_instance: return "instance";
        case tok_identifier: return "identifier";
        case tok_integer: return "integer";
        case tok_float: return "float";
        case tok_char: return "char";
        case tok_string: return "string";
        case tok_bool: return "boolean";
        case tok_if: return "if";
        case tok_then: return "then";
        case tok_else: return "else";
        case tok_let: return "let";
        case tok_in: return "in";
        case tok_operator: return "operator";
        case tok_colon: return "colon";
        case tok_semicolon: return "semicolon";
        case tok_open_parenthesis: return "parenthesis";
        case tok_close_parenthesis: return "parenthesis";
        case tok_comma: return "comma";
        case tok_equal: return "equal";
        case tok_comment: return "comment";
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
};

} // namespace lex
} // namespece llfp
