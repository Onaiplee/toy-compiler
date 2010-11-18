%{
#include <stdio.h>
#include <stdlib.h>
#include "y.tab.h"
#define NHASH 1000
void yyerror(char *);
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

and       {return (AND);       }
begin     {return (BEGIN);     }
forward   {return (FORWARD);   }
div       {return (DIV);       }
do        {return (DO);        }
else      {return (ELSE);      } 
end       {return (END);       }
for       {return (FOR);       }
function  {return (FUNCTION);  }
if        {return (IF);        }
array     {return (ARRAY);     }
mod       {return (MOD);       }
not       {return (NOT);       }
of        {return (OF);        }
or        {return (OR);        }
procedure {return (PROCEDURE); }
program   {return (PROGRAM);   }
record    {return (RECORD);    }
then      {return (THEN);      }
to        {return (TO);        }
type      {return (TYPE);      }
var       {return (VAR);       }
while     {return (WHILE);     }

"<>"      {return (NE);        }
"<="      {return (LT);        }
">="      {return (GT);        }
".."      {return (RG);        }
":="      {return (ASN);       }


{SYM}     {return *yytext;     }
{WS}      {                    }
{INT}     {return (INTEGER);   }
{STR}     {return (STRING);       }
{ID}      {return (ID);        }
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
