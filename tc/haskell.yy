/** Lexical analyser for Haskell */

%{
#include <stdlib.h>
%}

DIGIT [0-9]

INTEGER {DIGIT}+

FLOAT {DIGIT}+"."{DIGIT}*

VARIABLE_IDENTIFIER [[:lower:]][[:alnum:]]*

CONSTANT_IDENTIFIER [[:upper:]][[:alnum:]]*

VARIABLE_TYPE_OPERATOR "::"

%%

"type"|"newtype"|"data" {
    printf("Keyword %s\n", yytext);
}

{VARIABLE_IDENTIFIER} {
    printf("Variable: %s\n", yytext);
}

{CONSTANT_IDENTIFIER} {
    printf("Constant: %s\n", yytext);
}

deriving {
    printf("Deriving statement\n");
}

"(" {
    printf("Opening paranthesis\n");
}

")" {
    printf("Closing paranthesis\n");
}

"[" {
    printf("Opening bracket\n");
}

"]" {
    printf("Closing bracket\n");
}

"," {
    printf("Comma\n");
}

{INTEGER} {
    printf("Integer: %lld\n", strtoll(yytext, NULL, 10));
}

{FLOAT} {
    printf("Double: %lf\n", strtod(yytext, NULL));
}

["="|"+"|"-"|"*"|"^"|"/"|"|"|"."|"<"|">"]+ {
    printf("Operator %s\n", yytext);
}

=> {
    printf("Typeclass operator\n");
}

-> {
    printf("Return type operator\n");
}

{VARIABLE_TYPE_OPERATOR} {
    printf(":: operator\n");
}

[[:space:]] {
    /* Ignore whitespace */
}

"--".+ {
    printf("Comment\n");
}

"{-"(\n|.)+"-}" {
    printf("Multiline comment\n");
}

%%

int main(int argc, char** argv)
{
    if (argc >= 2)
        yyin = fopen(argv[1], "r");
    else
        yyin = stdin;

    yylex();

    return 0;
}
