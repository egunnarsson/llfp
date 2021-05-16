
#include "Lexer.h"

#include "gtest/gtest.h"

// Uses Lexer::tokenString() to print tokens, if there are errors in said function disable this
#define PRETTY_PRINT_TOKEN 1


using namespace llfp::lex;

struct TestToken
{
    Token type;
    const char *string;
};

testing::Message& operator<<(testing::Message& msg, TestToken token)
{
#if PRETTY_PRINT_TOKEN
    return msg << Lexer::tokenName(token.type) << '(' << token.string << ')';
#else
    return msg << token.type << '(' << token.string << ')';
#endif
}

/*std::ostream& operator<<(std::ostream& os, std::initializer_list<TestToken> tokens)
{
    
}*/

void Print(testing::Message &msg, std::initializer_list<TestToken> tokens, int count)
{
    for (auto token : tokens) {
        if (count == 0) { return; }
        --count;
        msg << token << ", ";
    }
}

testing::AssertionResult Parse(const char*, const char*, const char *string, std::initializer_list<TestToken> tokens)
{
    if (tokens.size() == 0)
    {
        return testing::AssertionFailure() << "empty tokens list";
    }
    
    StringInput input(string);
    Lexer lexer(&input);
    int ok = 0;
    for (auto token : tokens)
    {
        auto lt = lexer.getToken();
        auto &ls = lexer.getString();
        if (lt != token.type || ls != token.string)
        {
            TestToken parsedToken = { lt, ls.c_str() };
            
            testing::Message msg;
            msg << " Parsing: " << string << "\n";
            msg << "  Actual: "; Print(msg, tokens, ok); msg << parsedToken << '\n';
            msg << "Expected: "; Print(msg, tokens, ok); msg << token;
            
            return testing::AssertionFailure() << msg;
        }
        ++ok;
        lexer.nextToken();
    }
    
    // we got everything we expected, make sure the Lexer does not parse more tokens
    // and that any error is kept
    
    auto lt = lexer.getToken();
    if ((tokens.end() - 1)->type == Token::Error)
    {
        if (lt != Token::Error)
        {
            return testing::AssertionFailure() << "2nd call to getToken after error";
        }
        if (lexer.getString() != (tokens.end() - 1)->string)
        {
            return testing::AssertionFailure() << "getString changed after error";
        }
    }
    else if (lt != Token::Eof)
    {
        auto ls = lexer.getString();
        TestToken parsedToken = { lt, ls.c_str() };
        
        testing::Message msg;
        msg << " Parsing:" << string << "\n";
        msg << "  Actual:" << parsedToken << "\n";
        msg << "Expected: " << Lexer::tokenName(Token::Eof) << "()";
        
        return testing::AssertionFailure() << msg;
    }
    return testing::AssertionSuccess();
}

testing::AssertionResult Parse1(const char*, const char*, const char*, const char *string, llfp::lex::Token token, const char *lexString)
{
    return Parse("", "", string, {{token, lexString}});
}

#define PARSE_TOKEN(input, token, string) EXPECT_PRED_FORMAT3(Parse1, input, token, string);
#define PARSE_TOKENS(input, sequence) EXPECT_PRED_FORMAT2(Parse, input, sequence);

TEST(LexerTest, StringInput)
{
    //EXPECT_TRUE(false) << "this";
}

TEST(LexerTest, Tokens)
{
    PARSE_TOKEN("module", Token::Module, "module");
    
    PARSE_TOKEN("import", Token::Import, "import");
    PARSE_TOKEN("export", Token::Export, "export");
    PARSE_TOKEN("data", Token::Data, "data");
    PARSE_TOKEN("true", Token::Bool, "true");
    PARSE_TOKEN("false", Token::Bool, "false");
    PARSE_TOKEN("if", Token::If, "if");
    PARSE_TOKEN("then", Token::Then, "then");
    PARSE_TOKEN("else", Token::Else, "else");
    PARSE_TOKEN("let", Token::Let, "let");
    PARSE_TOKEN("in", Token::In, "in");
    
    PARSE_TOKEN("=", Token::Equal, "=");
    
    PARSE_TOKEN("(", Token::Open_parenthesis, "(");
    PARSE_TOKEN(")", Token::Close_parenthesis, ")");
    PARSE_TOKEN("[", Token::Open_bracket, "[");
    PARSE_TOKEN("]", Token::Close_bracket, "]");
    PARSE_TOKEN("{", Token::Open_brace, "{");
    PARSE_TOKEN("}", Token::Close_brace, "}");
    PARSE_TOKEN(",", Token::Comma, ",");
    PARSE_TOKEN(":", Token::Colon, ":");
    PARSE_TOKEN(";", Token::Semicolon, ";");
    
    PARSE_TOKEN("ö", Token::Error, "unknown token");
}

TEST(LexerTest, Whitespace)
{
    std::initializer_list<TestToken> tokens = { {Token::Eof, ""} };
    PARSE_TOKENS("", tokens);
    PARSE_TOKENS(" ", tokens);
    PARSE_TOKENS("\t", tokens);
    PARSE_TOKENS("\n", tokens);
    PARSE_TOKENS("\r\n", tokens);
    PARSE_TOKENS("  ", tokens);
    
    tokens = { {Token::Identifier, "a"}, {Token::Identifier, "b"} };
    PARSE_TOKENS("a b", tokens);
    tokens = { {Token::Identifier, "a"} };
    PARSE_TOKENS(" a", tokens);
    PARSE_TOKENS("a ", tokens);
}

TEST(LexerTest, Operators)
{
    PARSE_TOKEN("+", Token::Operator, "+");
    PARSE_TOKEN("-", Token::Operator, "-");
    PARSE_TOKEN("*", Token::Operator, "*");
    PARSE_TOKEN("/", Token::Operator, "/");
    PARSE_TOKEN("%", Token::Operator, "%");
    
    // special...
    //PARSE_TOKEN("=", Token::Operator, "=");
    
    PARSE_TOKEN("==", Token::Operator, "==");
    PARSE_TOKEN("!=", Token::Operator, "!=");
    PARSE_TOKEN("<", Token::Operator, "<");
    PARSE_TOKEN(">", Token::Operator, ">");
    PARSE_TOKEN("<=", Token::Operator, "<=");
    PARSE_TOKEN(">=", Token::Operator, ">=");

    PARSE_TOKEN("<<", Token::Operator, "<<");
    PARSE_TOKEN(">>", Token::Operator, ">>");
    PARSE_TOKEN(">>>", Token::Operator, ">>>");
    
    PARSE_TOKEN("&", Token::Operator, "&");
    PARSE_TOKEN("|", Token::Operator, "|");
    PARSE_TOKEN("^", Token::Operator, "^");
    
    PARSE_TOKEN("&&", Token::Operator, "&&");
    PARSE_TOKEN("||", Token::Operator, "||");
    
    PARSE_TOKEN("!", Token::Operator, "!");
    PARSE_TOKEN("~", Token::Operator, "~");
    
    PARSE_TOKEN(".", Token::Operator, ".");

    // not valid operator but should be parsed as an operator...
    PARSE_TOKEN(".+", Token::Operator, ".+");

    std::initializer_list<TestToken> tokens;

    tokens = { { Token::Identifier, "a" }, { Token::Operator, ".",  }, { Token::Identifier, "b" } };
    PARSE_TOKENS("a.b", tokens);
    //tokens = { { Token::identifier, "a" },{ Token::float, ".2" } };
    //PARSE_TOKENS("a.2", tokens);
    tokens = { {Token::Operator, "!"}, {Token::Open_parenthesis, "("}, {Token::Operator, "+"} };
    PARSE_TOKENS("!(+", tokens);
    tokens = { {Token::Operator, "-"}, {Token::Close_parenthesis, ")"}, {Token::Operator, "/"} };
    PARSE_TOKENS("-)/", tokens);
    tokens = { {Token::Operator, "&"}, {Token::Comma, ","}, {Token::Operator, "^"} };
    PARSE_TOKENS("&,^", tokens);
    tokens = { {Token::Operator, "|"}, {Token::Colon, ":"}, {Token::Operator, "+-"} };
    PARSE_TOKENS("|:+-", tokens);
    tokens = { {Token::Operator, "!!"}, {Token::Semicolon, ";"}, {Token::Operator, "+"} };
    PARSE_TOKENS("!!;+", tokens);
    
    tokens = { {Token::Operator, "~"}, {Token::Identifier, "a"}, {Token::Operator, "*"} };
    PARSE_TOKENS("~a*", tokens);
    
    // "["
}

TEST(LexerTest, Identifiers)
{
    PARSE_TOKEN("a", Token::Identifier, "a");
    PARSE_TOKEN("zz", Token::Identifier, "zz");
    PARSE_TOKEN("A1", Token::Identifier, "A1");
    PARSE_TOKEN("Z'", Token::Identifier, "Z'");
    PARSE_TOKEN("y'a'", Token::Identifier, "y'a'");
    PARSE_TOKEN("Z'b", Token::Identifier, "Z'b");
    PARSE_TOKEN("a_", Token::Identifier, "a_");
    //PARSE_TOKEN("_a", Token::identifier, "_a"); decide what to do with this
    PARSE_TOKEN("a_b", Token::Identifier, "a_b");
}

//TEST(LexerTest, Integers)
//TEST(LexerTest, Floats)

TEST(LexerTest, Numbers)
{
    PARSE_TOKEN("1", Token::Integer, "1");
    PARSE_TOKEN("23", Token::Integer, "23");
    PARSE_TOKEN("4.5", Token::Float, "4.5");
    PARSE_TOKEN("67.89", Token::Float, "67.89");
    PARSE_TOKEN(".0", Token::Float, ".0");
    PARSE_TOKEN(".12", Token::Float, ".12");
    
    std::initializer_list<TestToken> tokens;
    
    tokens = {{Token::Operator, "-"}, {Token::Integer, "0"}};
    PARSE_TOKENS("-0", tokens);
    
    PARSE_TOKEN("3a", Token::Error, "invalid number");
    PARSE_TOKEN("4.56a", Token::Error, "invalid number");
    
    // "1.2,3"
}

//TEST(LexerTest, Chars)
//TEST(LexerTest, Strings)

TEST(LexerTest, StringsAndChars)
{
    PARSE_TOKEN("'a'", Token::Char, "a");
    PARSE_TOKEN("'\\n'", Token::Char, "\n");
    PARSE_TOKEN("'\\t'", Token::Char, "\t");
    PARSE_TOKEN("'\\''", Token::Char, "'");
    PARSE_TOKEN("'\\\\'", Token::Char, "\\");
    
    PARSE_TOKEN("\"\"", Token::String, "");
    PARSE_TOKEN("\"aa\"", Token::String, "aa");
    PARSE_TOKEN("\"aa\\\"aa\"", Token::String, "aa\"aa");
    
    // invalid
    PARSE_TOKEN("''", Token::Error, "empty character");
    PARSE_TOKEN("'", Token::Error, "unclosed character");
    PARSE_TOKEN("'''", Token::Error, "empty character");
    PARSE_TOKEN("'aa", Token::Error, "unclosed character");
    PARSE_TOKEN("'\\a'", Token::Error, "invalid escape character");
    PARSE_TOKEN("'\\na'", Token::Error, "unclosed character");
    
    PARSE_TOKEN("\"", Token::Error, "unclosed string at end of file");
    PARSE_TOKEN("\"a", Token::Error, "unclosed string at end of file");
    PARSE_TOKEN("\"\\a\"", Token::Error, "invalid escape character");
}

TEST(LexerTest, Comments)
{
    
}

TEST(LexerTest, GetLocation)
{
    
}
