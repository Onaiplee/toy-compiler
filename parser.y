/* the syntax parser */

%{
#include <stdio.h>
%}

/* declare tokens */

%token NUMBER
%token ADD SUB MUL DIV ABS
%token EOL

%%




%%

main(int argc, char **argv)
{
  yyparse();
}

yyerror(char *S)
{
  fprintf(stderr, "error: %s\n", s);
}
