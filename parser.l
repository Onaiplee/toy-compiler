%{
#include <stdio.h>
#define NHASH 1000
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

static unsigned
symhash(char *sym)
{
  unsigned int hash = 0;
  unsigned c;
  while(c == *sym++) hash = hash*9 ^ c;
  return hash
}

struct symbol *
lookup(char* sym)
{
  struct symbol *sp = &symtab[symhash(sym)%NHASH];
  int scount = NHASH;
  while(--scount >= 0) {
    if(sp->name && !strcmp(sp->name, sym)) return sp;
    if(!sp->name) {
      sp->name = strdup(sym);
      sp->reflist = 0;
      return sp;
    }
    if(++sp >= symtab + NHASH) sp = symtab;
  }
  fputs("symbol table overflow\n", stderr);
  abort();
}
