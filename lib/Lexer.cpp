
// https://rosettacode.org/wiki/Compiler/lexical_analyzer

#include "Lexer.h"

#include "Error.h"
#include "String/StringConstants.h"


namespace llfp::lex
{

Lexer::Lexer(const Source* source_)
    : source{ source_ },
      iterator{ source->begin() },
      currentToken(Token::Invalid)
{
    lastChar = getChar();
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
    case '^': return true;
    case '|': return true;
    case '~': return true;
    default: return false;
    }
}

// bad name, things that break identifiers and operators
constexpr Token punctuation(int _C)
{
    switch (_C)
    {
    case '(': return Token::Open_parenthesis;
    case ')': return Token::Close_parenthesis;
    case ',': return Token::Comma;
    case ':': return Token::Colon;
    case ';': return Token::Semicolon;
    case '[': return Token::Open_bracket;
    case ']': return Token::Close_bracket;
    case '{': return Token::Open_brace;
    case '}': return Token::Close_brace;
    default: return Token::Error;
    }
}

bool isidentifier(int _C)
{
    return isalnum(_C) || _C == '_' || _C == '\'';
}

} // namespace

Token Lexer::nextToken()
{
    if (currentToken != Token::Eof)
    {
        currentToken = parseToken();
    }
    return currentToken;
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
    return tokenLocation;
}

int Lexer::getChar()
{
    int result = EOF;
    if (iterator != source->end())
    {
        result = static_cast<unsigned char>(*iterator);
        ++iterator;
    }
    return result;
}

Token Lexer::parseToken()
{
    assert(currentToken != Token::Eof);

    if (currentToken == Token::Error)
    {
        return currentToken;
    }

    // Skip any whitespace.
    while (isspace(lastChar))
    {
        lastChar = getChar();
    }

    tokenLocation = iterator.location();

    if (isalpha(lastChar)) // identifier: letter (letter | digit | '_' | '\'')*
    {
        return parseIdentifier();
    }

    // -1? -1.0? or treat it as unary op, problem is type check unsigned, or fix in parsing
    // hex, oct, bin
    if (isdigit(lastChar) || lastChar == '.') // Number: '.'? digit+ | digit* '.' digit+ ('e' '-'? digit+)?
    {
        return parseNumber();
    }

    if (lastChar == '\'') // Char: '\'' ((char - ["'\\"]) | ('\\' ["'\\nt"])) '\''
    {
        return parseChar();
    }

    if (lastChar == '"') // String: '"' ((char - ["\"\\"]) | ('\\' ["\"\\nt"]))* '"'
    {
        return parseString();
    }

    // operators
    if (isoperator(lastChar))
    {
        return parseOperator();
    }

    auto ptoken = punctuation(lastChar);
    if (ptoken != Token::Error)
    {
        tokenString = static_cast<char>(lastChar);

        lastChar = getChar();

        return ptoken;
    }

    if (lastChar == '#') // Comments: '#'[.-'\n']*'\n' | #{.*}#
    {
        return parseComment();
    }

    // list?

    if (lastChar == EOF)
    {
        tokenString.clear();
        ++tokenLocation.Column;
        return Token::Eof;
    }

    return error("unknown token");
}

Token Lexer::error(const char* msg)
{
    tokenString         = msg;
    return currentToken = Token::Error;
}

// identifier: letter (letter | digit | '_' | '\'')*
Token Lexer::parseIdentifier()
{
    tokenString = static_cast<char>(lastChar);

    while (isidentifier(lastChar = getChar()))
        tokenString += static_cast<char>(lastChar);

    // TODO: change to switch or map
    if (tokenString == id::Module)
        return Token::Module;
    if (tokenString == id::Import)
        return Token::Import;
    if (tokenString == id::Export)
        return Token::Export;
    if (tokenString == id::Data)
        return Token::Data;
    if (tokenString == id::Class)
        return Token::Class;
    if (tokenString == id::Instance)
        return Token::Instance;
    if (tokenString == id::If)
        return Token::If;
    if (tokenString == id::Then)
        return Token::Then;
    if (tokenString == id::Else)
        return Token::Else;
    if (tokenString == id::Let)
        return Token::Let;
    if (tokenString == id::In)
        return Token::In;
    if (tokenString == id::Case)
        return Token::Case;
    if (tokenString == id::Of)
        return Token::Of;
    if (tokenString == id::End)
        return Token::End;
    if (tokenString == id::True || tokenString == id::False)
        return Token::Bool;

    return Token::Identifier;
}

// Number: '.'? digit+ | digit* '.' digit+ ('e' '-'? digit+)?
Token Lexer::parseNumber()
{
    tokenString.clear();

    while (isdigit(lastChar))
    {
        tokenString += static_cast<char>(lastChar);
        lastChar = getChar();
    }

    if (lastChar != '.') // integer
    {
        return isalpha(lastChar) ? error("invalid number") : Token::Integer;
    }
    else // float
    {
        tokenString += '.';
        lastChar = getChar();
        while (isdigit(lastChar))
        {
            tokenString += static_cast<char>(lastChar);
            lastChar = getChar();
        }
        if (lastChar == 'e')
        {
        }
        if (tokenString == ".")
        {
            if (isoperator(lastChar))
            {
                return parseOperator(true);
            }
            return Token::Operator;
        }
        else
        {
            return isalpha(lastChar) ? error("invalid number") : Token::Float;
        }
    }
}

// Char: '\'' ((char - ["'\\"]) | ('\\' ["'\\nt"])) '\''
Token Lexer::parseChar()
{
    tokenString.clear();

    lastChar = getChar();
    if (lastChar == '\\')
    {
        lastChar = getChar();
        switch (lastChar)
        {
        case '\'': tokenString = '\''; break;
        case '\\': tokenString = '\\'; break;
        case 'n': tokenString = '\n'; break;
        case 't': tokenString = '\t'; break;
        default: return error("invalid escape character");
        }
        lastChar = getChar();
    }
    else if (lastChar == '\'')
    {
        return error("empty character");
    }
    else
    {
        tokenString = static_cast<char>(lastChar);
        lastChar    = getChar();
    }

    if (lastChar != '\'')
    {
        return error("unclosed character");
    }

    lastChar = getChar();
    return Token::Char;
}

// String: '"' ((char - ["\"\\"]) | ('\\' ["\"\\nt"]))* '"'
Token Lexer::parseString()
{
    tokenString.clear();

    lastChar = getChar();
    while (lastChar != '"')
    {
        if (lastChar == '\\')
        {
            lastChar = getChar();
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
        lastChar = getChar();
    }

    lastChar = getChar();
    return Token::String;
}

Token Lexer::parseOperator(bool continuation)
{
    if (!continuation)
    {
        tokenString.clear();
    }

    tokenString += static_cast<char>(lastChar);

    while (isoperator(lastChar = getChar()))
    {
        tokenString += static_cast<char>(lastChar);
    }

    if (tokenString == "=")
    {
        return Token::Equal;
    }

    return Token::Operator;
}

// Comments: '#'[.-'\n']*'\n' | #{.*}#
Token Lexer::parseComment()
{
    tokenString.clear();

    lastChar = getChar();

    // multiline could be any char you choose?
    // if this how do you single line?
    // int commentChar = lastChar;

    if (lastChar == '{') // multiline comment
    {
        lastChar      = getChar();
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
            lastChar = getChar();
        }
    }
    else // single line
    {
        while (lastChar != '\n' && lastChar != EOF)
        {
            lastChar = getChar();
        }
    }

    if (skipComments)
    {
        return parseToken();
    }
    else
    {
        return Token::Comment;
    }
}

} // namespace llfp::lex
