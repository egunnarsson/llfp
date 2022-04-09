
// https://rosettacode.org/wiki/Compiler/lexical_analyzer

#include "Error.h"
#include "String/StringConstants.h"

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

FileInput::FileInput(const char* fileName) :
    Input(fileName)
{
    file = fopen(fileName, "r");
    if (file == nullptr)
    {
        throw llfp::Error(std::string{ std::strerror(errno) } + " (" + fileName + ')' );
    }
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
    input(input),
    currentToken(Token::Invalid)
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
    return tokenLocation;
}

Token Lexer::parseToken()
{
    if (currentToken == Token::Error)
    {
        return currentToken;
    }
    
    // Skip any whitespace.
    while (isspace(lastChar))
    {
        lastChar = input->getChar();
    }

    tokenLocation = input->getLocation();

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

        lastChar = input->getChar();

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
        return Token::Eof;
    }

    return error("unknown token");
}

Token Lexer::error(const char *msg)
{
    tokenString = msg;
    return currentToken = Token::Error;
}

// identifier: letter (letter | digit | '_' | '\'')*
Token Lexer::parseIdentifier()
{
    tokenString = static_cast<char>(lastChar);

    while (isidentifier(lastChar = input->getChar()))
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
        lastChar = input->getChar();
    }

    if (lastChar != '.') // integer
    {
        return isalpha(lastChar) ? error("invalid number") : Token::Integer;
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
        if (tokenString == ".")
        {
            return Token::Operator; // should continue parsing operator... even though there are no operators starting with '.'
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
    }
    else if (lastChar == '\'')
    {
        return error("empty character");
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
    return Token::Char;
}

// String: '"' ((char - ["\"\\"]) | ('\\' ["\"\\nt"]))* '"'
Token Lexer::parseString()
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
    return Token::String;
}

Token Lexer::parseOperator()
{
    tokenString = static_cast<char>(lastChar);

    while (isoperator(lastChar = input->getChar()))
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
        return Token::Comment;
    }
}

} // namespace lex
} // namespece hpfp
