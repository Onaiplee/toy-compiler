%{
#include <stdio.h>
%}

/* regular definitions */
ID      {letter}({letter}|{digit}|[_])*
INT     {digit}+
STR     \"[^\n"]*\"
WS      [ \t\n]+
SYM     [+-=<>.,:;\(\)\[\]]|"<>"|">="|"<="|".."|"*"|":="
digit   [0-9]
letter  [a-z]
CMT     \{[^\{\}]*\}

%%

and|begin|forward|div|do|else|end|for|function|if|array|mod|not|of|or|procedure|program|record|then|to|type|var|while   {
        printf("Keywords: %s\n", yytext);
        }
{SYM}   {
        printf("Predefined syms: %s\n", yytext);
        }
{WS}    {}

{INT}   {
        printf("integers literals: %s\n", yytext);
        }
{STR}   {
        printf("string literals: %s\n", yytext);
        }
{ID}    {
        printf("identifier: %s\n", yytext);
        }
{CMT}   {}


%%

main(int argc, char **argv)
{
        ++argv, --argc;
        if (argc > 0)
                yyin = fopen(argv[0], "r");
        else
                yyin = stdin;
        yylex();
        if (yyin != stdin)
                fclose(yyin);
}
