
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
    if ((tokens.end() - 1)->type == tok_error)
    {
        if (lt != tok_error)
        {
            return testing::AssertionFailure() << "2nd call to getToken after error";
        }
        if (lexer.getString() != (tokens.end() - 1)->string)
        {
            return testing::AssertionFailure() << "getString changed after error";
        }
    }
    else if (lt != tok_eof)
    {
        auto ls = lexer.getString();
        TestToken parsedToken = { lt, ls.c_str() };
        
        testing::Message msg;
        msg << " Parsing:" << string << "\n";
        msg << "  Actual:" << parsedToken << "\n";
        msg << "Expected: " << Lexer::tokenName(tok_eof) << "()";
        
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
    PARSE_TOKEN("module", tok_module, "module");
    
    PARSE_TOKEN("import", tok_import, "import");
    PARSE_TOKEN("export", tok_export, "export");
    PARSE_TOKEN("data", tok_data, "data");
    PARSE_TOKEN("true", tok_bool, "true");
    PARSE_TOKEN("false", tok_bool, "false");
    PARSE_TOKEN("if", tok_if, "if");
    PARSE_TOKEN("then", tok_then, "then");
    PARSE_TOKEN("else", tok_else, "else");
    PARSE_TOKEN("let", tok_let, "let");
    PARSE_TOKEN("in", tok_in, "in");
    
    PARSE_TOKEN("=", tok_equal, "=");
    
    PARSE_TOKEN("(", tok_open_parenthesis, "(");
    PARSE_TOKEN(")", tok_close_parenthesis, ")");
    PARSE_TOKEN("[", tok_open_bracket, "[");
    PARSE_TOKEN("]", tok_close_bracket, "]");
    PARSE_TOKEN("{", tok_open_brace, "{");
    PARSE_TOKEN("}", tok_close_brace, "}");
    PARSE_TOKEN(",", tok_comma, ",");
    PARSE_TOKEN(":", tok_colon, ":");
    PARSE_TOKEN(";", tok_semicolon, ";");
    
    PARSE_TOKEN("ö", tok_error, "unknown token");
}

TEST(LexerTest, Whitespace)
{
    std::initializer_list<TestToken> tokens = { {tok_eof, ""} };
    PARSE_TOKENS("", tokens);
    PARSE_TOKENS(" ", tokens);
    PARSE_TOKENS("\t", tokens);
    PARSE_TOKENS("\n", tokens);
    PARSE_TOKENS("\r\n", tokens);
    PARSE_TOKENS("  ", tokens);
    
    tokens = { {tok_identifier, "a"}, {tok_identifier, "b"} };
    PARSE_TOKENS("a b", tokens);
    tokens = { {tok_identifier, "a"} };
    PARSE_TOKENS(" a", tokens);
    PARSE_TOKENS("a ", tokens);
}

TEST(LexerTest, Operators)
{
    PARSE_TOKEN("+", tok_operator, "+");
    PARSE_TOKEN("-", tok_operator, "-");
    PARSE_TOKEN("*", tok_operator, "*");
    PARSE_TOKEN("/", tok_operator, "/");
    PARSE_TOKEN("%", tok_operator, "%");
    
    // special...
    //PARSE_TOKEN("=", tok_operator, "=");
    
    PARSE_TOKEN("==", tok_operator, "==");
    PARSE_TOKEN("!=", tok_operator, "!=");
    PARSE_TOKEN("<", tok_operator, "<");
    PARSE_TOKEN(">", tok_operator, ">");
    PARSE_TOKEN("<=", tok_operator, "<=");
    PARSE_TOKEN(">=", tok_operator, ">=");

    PARSE_TOKEN("<<", tok_operator, "<<");
    PARSE_TOKEN(">>", tok_operator, ">>");
    PARSE_TOKEN(">>>", tok_operator, ">>>");
    
    PARSE_TOKEN("&", tok_operator, "&");
    PARSE_TOKEN("|", tok_operator, "|");
    PARSE_TOKEN("^", tok_operator, "^");
    
    PARSE_TOKEN("&&", tok_operator, "&&");
    PARSE_TOKEN("||", tok_operator, "||");
    
    PARSE_TOKEN("!", tok_operator, "!");
    PARSE_TOKEN("~", tok_operator, "~");
    
    PARSE_TOKEN(".", tok_operator, ".");

    // not valid operator but should be parsed as an operator...
    PARSE_TOKEN(".+", tok_operator, ".+");

    std::initializer_list<TestToken> tokens;

    tokens = { { tok_identifier, "a" }, { tok_operator, ".",  }, { tok_identifier, "b" } };
    PARSE_TOKENS("a.b", tokens);
    //tokens = { { tok_identifier, "a" },{ tok_float, ".2" } };
    //PARSE_TOKENS("a.2", tokens);
    tokens = { {tok_operator, "!"}, {tok_open_parenthesis, "("}, {tok_operator, "+"} };
    PARSE_TOKENS("!(+", tokens);
    tokens = { {tok_operator, "-"}, {tok_close_parenthesis, ")"}, {tok_operator, "/"} };
    PARSE_TOKENS("-)/", tokens);
    tokens = { {tok_operator, "&"}, {tok_comma, ","}, {tok_operator, "^"} };
    PARSE_TOKENS("&,^", tokens);
    tokens = { {tok_operator, "|"}, {tok_colon, ":"}, {tok_operator, "+-"} };
    PARSE_TOKENS("|:+-", tokens);
    tokens = { {tok_operator, "!!"}, {tok_semicolon, ";"}, {tok_operator, "+"} };
    PARSE_TOKENS("!!;+", tokens);
    
    tokens = { {tok_operator, "~"}, {tok_identifier, "a"}, {tok_operator, "*"} };
    PARSE_TOKENS("~a*", tokens);
    
    // "["
}

TEST(LexerTest, Identifiers)
{
    PARSE_TOKEN("a", tok_identifier, "a");
    PARSE_TOKEN("zz", tok_identifier, "zz");
    PARSE_TOKEN("A1", tok_identifier, "A1");
    PARSE_TOKEN("Z'", tok_identifier, "Z'");
    PARSE_TOKEN("y'a'", tok_identifier, "y'a'");
    PARSE_TOKEN("Z'b", tok_identifier, "Z'b");
    PARSE_TOKEN("a_", tok_identifier, "a_");
    //PARSE_TOKEN("_a", tok_identifier, "_a"); decide what to do with this
    PARSE_TOKEN("a_b", tok_identifier, "a_b");
}

//TEST(LexerTest, Integers)
//TEST(LexerTest, Floats)

TEST(LexerTest, Numbers)
{
    PARSE_TOKEN("1", tok_integer, "1");
    PARSE_TOKEN("23", tok_integer, "23");
    PARSE_TOKEN("4.5", tok_float, "4.5");
    PARSE_TOKEN("67.89", tok_float, "67.89");
    PARSE_TOKEN(".0", tok_float, ".0");
    PARSE_TOKEN(".12", tok_float, ".12");
    
    std::initializer_list<TestToken> tokens;
    
    tokens = {{tok_operator, "-"}, {tok_integer, "0"}};
    PARSE_TOKENS("-0", tokens);
    
    PARSE_TOKEN("3a", tok_error, "invalid number");
    PARSE_TOKEN("4.56a", tok_error, "invalid number");
    
    // "1.2,3"
}

//TEST(LexerTest, Chars)
//TEST(LexerTest, Strings)

TEST(LexerTest, StringsAndChars)
{
    PARSE_TOKEN("'a'", tok_char, "a");
    PARSE_TOKEN("'\\n'", tok_char, "\n");
    PARSE_TOKEN("'\\t'", tok_char, "\t");
    PARSE_TOKEN("'\\''", tok_char, "'");
    PARSE_TOKEN("'\\\\'", tok_char, "\\");
    
    PARSE_TOKEN("\"\"", tok_string, "");
    PARSE_TOKEN("\"aa\"", tok_string, "aa");
    PARSE_TOKEN("\"aa\\\"aa\"", tok_string, "aa\"aa");
    
    // invalid
    PARSE_TOKEN("''", tok_error, "empty character");
    PARSE_TOKEN("'", tok_error, "unclosed character");
    PARSE_TOKEN("'''", tok_error, "empty character");
    PARSE_TOKEN("'aa", tok_error, "unclosed character");
    PARSE_TOKEN("'\\a'", tok_error, "invalid escape character");
    PARSE_TOKEN("'\\na'", tok_error, "unclosed character");
    
    PARSE_TOKEN("\"", tok_error, "unclosed string at end of file");
    PARSE_TOKEN("\"a", tok_error, "unclosed string at end of file");
    PARSE_TOKEN("\"\\a\"", tok_error, "invalid escape character");
}

TEST(LexerTest, Comments)
{
    
}

TEST(LexerTest, GetLocation)
{
    
}
