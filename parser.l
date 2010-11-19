%{
#include <stdio.h>
#include <stdlib.h>
#include "parser.tab.h"
#define NHASH 1000
//void yyerror(char *);
%}

/* regular definitions */
ID      {letter}({letter}|{digit}|[_])*
INT     {digit}+
STR     \"[^\n"]*\"
WS      [ \t\n]+
SYM     [+-=<>.,:;\(\)\[\]]|"*"
digit   [0-9]
letter  [a-z]
CMT     \{[^\{\}]*\}

%%

and       {printf("AND\n"); return (AND);}
begin     {printf("BEGIN\n"); return (begin);}
forward   {printf("FORWARD\n"); return (FORWARD);}
div       {printf("DIV\n"); return (DIV);}
do        {printf("DO\n"); return (DO);}
else      {printf("ELSE\n"); return (ELSE);}
end       {printf("END\n"); return (END);}
for       {printf("FOR\n"); return (FOR);}
function  {printf("FUNCTION\n"); return (FUNCTION);}
if        {printf("IF\n"); return (IF);}
array     {printf("ARRAY\n"); return (ARRAY);}
mod       {printf("MOD\n"); return (MOD);}
not       {printf("NOT\n"); return (NOT);}
of        {printf("OF\n"); return (OF);}
or        {printf("OR\n"); return (OR);}
procedure {printf("PROCEDURE\n"); return (PROCEDURE);}
program   {printf("PROGRAM\n"); return (PROGRAM);}
record    {printf("RECORD\n"); return (RECORD);}
then      {printf("THEN\n"); return (THEN);}
to        {printf("TO\n"); return (TO);}
type      {printf("TYPE\n"); return (TYPE);}
var       {printf("VAR\n"); return (VAR);}
while     {printf("WHILE\n"); return (WHILE);}

"<>"      {printf("NE\n"); return (NE);}
"<="      {printf("LT\n"); return (LT);}
">="      {printf("GT\n"); return (GT);}
".."      {printf("RG\n"); return (RG);}
":="      {printf("CE\n"); return (CE);}


{WS}      {                    }
{INT}     {printf("INT: %s\n", yytext);  return (INTEGER);}
{STR}     {printf("STR: %s\n", yytext);  return (STRING);}
{ID}      {printf("ID: %s\n", yytext);   return (ID);}
{SYM}     {printf("SYM: %s\n", yytext);  return *yytext;}
{CMT}     {                    }


%%

int yywrap(void) {
    return 1;
}

//main(int argc, char **argv)
//{
//        ++argv, --argc;
//        if (argc > 0)
//                yyin = fopen(argv[0], "r");
//        else
//                yyin = stdin;
//        yylex();
//        if (yyin != stdin)
//                fclose(yyin);
//}

//static unsigned
//symhash(char *sym)
//{
//  unsigned int hash = 0;
//  unsigned c;
//  while(c == *sym++) hash = hash*9 ^ c;
//  return hash
//}
//
//struct symbol *
//lookup(char* sym)
//{
//  struct symbol *sp = &symtab[symhash(sym)%NHASH];
//  int scount = NHASH;
//  while(--scount >= 0) {
//    if(sp->name && !strcmp(sp->name, sym)) return sp;
//    if(!sp->name) {
//      sp->name = strdup(sym);
//      sp->reflist = 0;
//      return sp;
//    }
//    if(++sp >= symtab + NHASH) sp = symtab;
//  }
//  fputs("symbol table overflow\n", stderr);
//  abort();
//}
