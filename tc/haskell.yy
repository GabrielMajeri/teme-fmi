/** Lexical analyser for Haskell */

%{
#include <stdlib.h>
%}

DIGIT [0-9]

INTEGER {DIGIT}+

FLOAT {DIGIT}+"."{DIGIT}*

VARIABLE_IDENTIFIER [[:lower:]][[:alnum:]]*

CONSTANT_IDENTIFIER [[:upper:]][[:alnum:]]*
TYPE_IDENTIFIER {CONSTANT_IDENTIFIER}

TYPE_VARIABLE {VARIABLE_IDENTIFIER}
TYPE_VARIABLES ({TYPE_VARIABLE}{WHITESPACE}*)*

TYPE_NAME {VARIABLE_IDENTIFIER}|{TYPE_IDENTIFIER}|"("{WHITESPACE}*{TYPE_IDENTIFIER}{TYPE_VARIABLES}")"

TYPE_CONSTRUCTOR {CONSTANT_IDENTIFIER}({WHITESPACE}*{TYPE_NAME})*
TYPE_CONSTRUCTORS {TYPE_CONSTRUCTOR}({WHITESPACE}*"|"{WHITESPACE}*{TYPE_CONSTRUCTOR})*

WHITESPACE [[:space:]]*

VARIABLE_TYPE_OPERATOR "::"

EXPRESSION .*

%%

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

"," {
    printf("Comma\n");
}

{INTEGER} {
    printf("Integer: %lld\n", strtoll(yytext, NULL, 10));
}

{FLOAT} {
    printf("Double: %lf\n", strtod(yytext, NULL));
}

"type"|"newtype"|"data" {
    printf("Keyword %s\n", yytext);
}

"="|"+"|"-"|"*"|"/"|"|" {
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

/* "="{WHITESPACE}{TYPE_IDENTIFIER} */
