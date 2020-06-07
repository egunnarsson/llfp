
// https://rosettacode.org/wiki/Compiler/lexical_analyzer

#include "Lexer.h"


namespace llfp
{
namespace lex
{

int Input::getChar()
{
    int _C = getCharInt();
    if (_C != '\n')
    {
        ++location.Column;
    }
    else
    {
        location.Column = 0;
        ++location.Line;
    }
    return _C;
}

int StdinInput::getCharInt()
{
    return getchar();
}

FileInput::FileInput(const char*filename)
{
    file = fopen(filename, "r");
}

FileInput::~FileInput()
{
    fclose(file);
}

int FileInput::getCharInt()
{
    return fgetc(file);
}

Lexer::Lexer(Input *input) :
    input(input)
{
    lastChar = input->getChar();
    nextToken();
}

namespace
{

/*int convertEscapedChar(int _C)
{
    switch (_C)
    {
    case '"': return '"';
    case '\'': return '\'';
    case 'n': return '\n';
    case 't': return '\t';
    case '0': return '\0';
    default: return EOF; // if I do this will result in error unexpected end of file which is the wrong error
    }
}*/

constexpr bool isoperator(int _C)
{
    switch (_C)
    {
    case '!': return true;
    case '%': return true;
    case '&': return true;
    case '*': return true;
    case '+': return true;
    case '-': return true;
    case '.': return true;
    case '/': return true;
    case '<': return true;
    case '=': return true;
    case '>': return true;
    case '?': return true;
    case '[': return true;
    case ']': return true;
    case '^': return true;
    case '|': return true;
    case '~': return true;
    default:return false;
    }
}

// bad name, things that break identifiers and operators
constexpr Token punctuation(int _C)
{
    switch (_C)
    {
    case '(': return tok_open_parenthesis;
    case ')': return tok_close_parenthesis;
    case ',': return tok_comma;
    case ';': return tok_semicolon;
    default: return tok_error;
    }
}

bool isidentifier(int _C)
{
    return isalnum(_C) || _C == '_' || _C == '\'';
}

} // namespace

Token Lexer::nextToken()
{
    return currentToken = parseToken();
}

Token Lexer::getToken() const
{
    return currentToken;
}

const std::string& Lexer::getString() const
{
    return tokenString;
}

SourceLocation Lexer::getLocation() const
{
    return input->getLocation();
}

// lots of EOF in middle errors, like string not ending etc
Token Lexer::parseToken()
{
    // Skip any whitespace.
    while (isspace(lastChar))
        lastChar = input->getChar();

    if (isalpha(lastChar)) // identifier: [a-zA-Z][a-zA-Z0-9]*
    {
        // identifier: letter (letter | digit | '_' | '\'')*
        tokenString = static_cast<char>(lastChar);

        while (isidentifier(lastChar = input->getChar()))
            tokenString += static_cast<char>(lastChar);

        // change to switch or map
        if (tokenString == "module")
            return tok_module;
        if (tokenString == "data")
            return tok_data;
        if (tokenString == "class")
            return tok_class;
        if (tokenString == "instance")
            return tok_instance;
        if (tokenString == "if")
            return tok_if;
        if (tokenString == "then")
            return tok_then;
        if (tokenString == "else")
            return tok_else;
        if (tokenString == "let")
            return tok_let;
        if (tokenString == "in")
            return tok_in;
        if (tokenString == "true" || tokenString == "false")
            return tok_bool;
        return tok_identifier;
    }

    // -1? -1.0? or treat it as unary op, problem is type check unsigned, or fix in parsing
    // hex, oct, bin
    if (isdigit(lastChar) || lastChar == '.') // Number: digit+ | digit* '.' digit+ ('e' '-'? digit+)?
    {
        tokenString.clear();

        while (isdigit(lastChar))
        {
            tokenString += static_cast<char>(lastChar);
            lastChar = input->getChar();
        }

        if (lastChar != '.') // integer
        {
            return isalpha(lastChar) ? error("invalid number") : tok_integer;
        }
        else // float
        {
            tokenString += '.';
            lastChar = input->getChar();
            while (isdigit(lastChar))
            {
                tokenString += static_cast<char>(lastChar);
                lastChar = input->getChar();
            }
            if (lastChar == 'e')
            {

            }
            return isalpha(lastChar) ? error("invalid number") : tok_float;
        }
    }

    if (lastChar == '\'') // Char: '\'' ((char - ["'\\"]) | ('\\' ["'\\nt"])) '\''
    {
        tokenString.clear();

        lastChar = input->getChar();
        if (lastChar == '\\')
        {
            lastChar = input->getChar();
            switch (lastChar)
            {
            case '\'': tokenString = '\''; break;
            case '\\': tokenString = '\\'; break;
            case 'n': tokenString = '\n'; break;
            case 't': tokenString = '\t'; break;
            default: return error("invalid escape character");
            }
            lastChar = input->getChar();
            return tok_char;
        }
        else
        {
            tokenString = static_cast<char>(lastChar);
            lastChar = input->getChar();
        }
        if (lastChar != '\'')
        {
            return error("unclosed character");
        }

        lastChar = input->getChar();
        return tok_char;
    }

    if (lastChar == '"') // String: '"' ((char - ["\"\\"]) | ('\\' ["\"\\nt"]))* '"'
    {
        tokenString.clear();

        lastChar = input->getChar();
        while (lastChar != '"')
        {
            if (lastChar == '\\')
            {
                lastChar = input->getChar();
                switch (lastChar)
                {
                case '"': tokenString += '"'; break;
                case '\\': tokenString = '\\'; break;
                case 'n': tokenString = '\n'; break;
                case 't': tokenString = '\t'; break;
                default: return error("invalid escape character");
                }
            }
            else if (lastChar == EOF)
            {
                return error("unclosed string at end of file");
            }
            else
            {
                tokenString += static_cast<char>(lastChar);
            }
            lastChar = input->getChar();
        }
        lastChar = input->getChar();
        return tok_string;
    }

    // operators
    if (isoperator(lastChar))
    {
        tokenString = static_cast<char>(lastChar);

        while (isoperator(lastChar = input->getChar()))
        {
            tokenString += static_cast<char>(lastChar);
        }

        if (tokenString == "=")
        {
            return tok_equal;
        }

        return tok_operator;
    }

    auto ptoken = punctuation(lastChar);
    if (ptoken != tok_error)
    {
        tokenString = static_cast<char>(lastChar);

        lastChar = input->getChar();

        return ptoken;
    }

    if (lastChar == '#') // Comments: '#'[.-'\n']*'\n' | #{.*}#
    {
        tokenString.clear();

        lastChar = input->getChar();

        // multiline could be any char you choose?
        // if this how do you single line?
        //int commentChar = lastChar;

        if (lastChar == '{') // multiline comment
        {
            lastChar = input->getChar();
            char exitChar = '}';
            while (true)
            {
                if (lastChar == EOF)
                {
                    return error("unclosed comment at end of file");
                }
                if (lastChar == exitChar)
                {
                    if (exitChar == '}')
                    {
                        exitChar = '#';
                    }
                    else
                    {
                        break;
                    }
                }
                else
                {
                    exitChar = '}';
                }
                lastChar = input->getChar();
            }
        }
        else // single line
        {
            while (lastChar != '\n' && lastChar != EOF)
            {
                lastChar = input->getChar();
            }
        }

        if (skipComments)
        {
            return parseToken();
        }
        else
        {
            return tok_comment;
        }
    }

    // list?

    if (lastChar == EOF)
    {
        tokenString.clear();
        return tok_eof;
    }

    return error("unknown token");
}

Token Lexer::error(const char *msg)
{
    tokenString = msg;
    return currentToken = tok_error;
}

} // namespace lex
} // namespece hpfp
