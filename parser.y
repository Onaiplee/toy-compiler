/* the syntax parser */

%{

#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include "parser.tab.h"
#include "parser.h"
#define YYDEBUG 1
#define YYLINENO
int yylex(void);

%}

/* declare tokens */

%token <iValue> INTEGER 
%token STRING
%token <iIndex> ID
%token LT GT NE RG CE
%token AND begin FORWARD DIV DO ELSE END FOR FUNCTION IF ARRAY MOD
%token PROCEDURE OR OF NOT PROGRAM RECORD THEN TO TYPE VAR WHILE

%union {
    int iIndex;
    int iValue;
};


%start Program
%%

Program: PROGRAM ID ';' TypeDefinitions VariableDeclarations                                      {};

                 SubprogramDeclarations CompoundStatement '.'                                     {};


TypeDefinitions: 
               /* empty */                                                                        {}; 
               | TYPE TypeDefinition ';' TypeDefinition_X                                         {};
               ;

TypeDefinition_X: 
                /* empty */                                                                       {};
                | TypeDefinition_X TypeDefinition ';'                                             {};
                ;


VariableDeclarations:
                    /* empty */                                                                   {};
                    | VAR VariableDeclaration ';' VariableDeclarations_X                          {};

VariableDeclarations_X:
                     /* empty */                                                                  {};
                     | VariableDeclarations_X VariableDeclaration ';'                             {};
                     ;


SubprogramDeclarations:
                      /* empty */                                                                 {};
                      | SubprogramDeclarations ProFunDeclarationGroup ';'                         {};
                      ;


ProFunDeclarationGroup: ProcedureDeclaration                                                      {};
                      | FunctionDeclaration                                                       {};
                      ;

TypeDefinition: ID '=' Type                                                                       {};


VariableDeclaration: IdentifierList ':' Type                                                      {};

ProcedureDeclaration: PROCEDURE ID '(' FormalParameterList ')' ';' BlockforwardGroup              {};
FunctionDeclaration: FUNCTION ID '(' FormalParameterList ')' ':' ResultType ';' BlockforwardGroup {};

BlockforwardGroup: Block | FORWARD                                                                {};


FormalParameterList:
                   /* empty */                                                                    {};
                   | FormalParameterList_E                                                        {};
                   ;

FormalParameterList_E: IdentifierList ':' Type IdlistType_X                                       {};

IdlistType_X:
            /* empty */                                                                           {};
            | IdlistType_X ';' IdentifierList ':' Type                                            {};
            ;


Block: VariableDeclarations CompoundStatement                                                     {};


CompoundStatement: begin StatementSequence END                                                    {};
StatementSequence: Statement StatementSequence_X                                                  {};

StatementSequence_X:
                   /* empty */                                                                    {};
                   | StatementSequence_X ';' Statement                                            {};
                   ;


Statement: OpenStatement                                                                          {};
         | MatchedStatement                                                                       {};
         ;



SimpleStatement:
               /* empty */                                                                        {};
               | Assignment_Statement                                                             {};
               | ProcedureStatement                                                               {};
               ;


Assignment_Statement: Variable CE Expression                                                      {};

ProcedureStatement: ID '(' ActualParameterList ')'                                                {};


MatchedStatement: IF Expression THEN MatchedStatement ELSE MatchedStatement                       { $$ = opr(IF, 3, $2, $4, $6); };
                | CompoundStatement                                                               { $$ = $1; };
                | SimpleStatement                                                                 { $$ = $1; };
                | WHILE Expression DO MatchedStatement                                            { $$ = opr(WHILE, 2, $2, $4); };
                | FOR ID CE Expression TO Expression DO MatchedStatement                          { $$ = opr(FOR, 4, $2, $4, $6, $8); };
                ;

OpenStatement: IF Expression THEN Statement                                                       { $$ = opr(IF, 2, $2, $4); };
             | IF Expression THEN MatchedStatement ELSE OpenStatement                             { $$ = opr(IF, 3, $2, $4, $6); };
             | WHILE Expression DO OpenStatement                                                  { $$ = opr(WHILE, 2, $2, $4); };
             | FOR ID CE Expression TO Expression DO OpenStatement                                { $$ = opr(FOR, 4, $2, $4, $6, $8); };
             ;

Type: ID                                                                                          { $$ = $1; };
    | ARRAY '[' Constant RG Constant ']' OF Type                                                  { $$ = opr(ARRAY, 3, $3, $5, $8); };
    | RECORD Field_List END                                                                       { $$ = $2; };
    ;

ResultType: ID                                                                                    { $$ = $1; };

Field_List:
          /* empty */                                                                             {};
          | IdentifierList ':' Type IdlistType_X                                                  { $$ = opr();};
          ;


Constant: INTEGER                                                                                 {};
        | Sign INTEGER                                                                            {};
        ;

Expression: Simple_Expression                                                                     { $$ = $1; };
          | Simple_Expression Relational_Op Simple_Expression                                     { $$ = opr($2, 2, $1, $3); };
          ;

Relational_Op: '<'                                                                                { $$ = $1; }; 
             | LT                                                                                 { $$ = $1; }; 
             | '>'                                                                                { $$ = $1; }; 
             | GT                                                                                 { $$ = $1; };
             | NE                                                                                 { $$ = $1; };
             | '='                                                                                { $$ = $1; };
             ;

Simple_Expression: Term AddopTerm_X                                                               { $$ = opr( ,2, $1, $2); };
                 | Sign Term AddopTerm_X                                                          { $$ = opr($2, 2, $1, $3); };
                 ;

AddopTerm_X:
           /* empty */                                                                            {};
           | AddopTerm_X AddOp Term                                                               { $$ = opr($2, 2, $1, $3); };
           ;

AddOp: '+'                                                                                        { $$ = $1; };
     | '-'                                                                                        { $$ = $1; };
     | OR                                                                                         { $$ = $1; };
     ;

Term: Factor MulOpFactor_X                                                                        {};

MulOpFactor_X:
             /* empty */                                                                          {};
             | MulOpFactor_X MulOp Factor                                                         { $$ = opr($2, 2, $1, $3); };
             ;

MulOp: '*'                                                                                        { $$ = $1; }; 
     | DIV                                                                                        { $$ = $1; }; 
     | MOD                                                                                        { $$ = $1; };
     | AND                                                                                        { $$ = $1; };
     ;

Factor: INTEGER                                                                                   {};
      | STRING                                                                                    {};
      | Variable                                                                                  {};
      | Function_Reference                                                                        {};
      | NOT Factor                                                                                {};
      | '(' Expression ')'                                                                        { $$ = $2; };
      ;

Function_Reference: ID '(' ActualParameterList ')'                                                {};

Variable: ID ComponentSelection                                                                   {};

ComponentSelection:
                  /* empty */                                                                     {};
                  | '.' ID ComponentSelection                                                     {};
                  | '[' Expression ']' ComponentSelection                                         {};
                  ;

ActualParameterList:
                   /* empty */                                                                    {};
                   | Expression Expression_X                                                      {};
                   ;
Expression_X:
            /* empty */                                                                           {};
            | Expression_X ',' Expression                                                         {};
            ;

IdentifierList: ID Id_X                                                                           {};

Id_X:
    /* empty */                                                                                   {};
    | Id_X ',' ID                                                                                 {};
    ;

Sign: '+'                                                                                         {};
    | '-'                                                                                         {};
    ;

%%

#define SIZEOF_NODETYPE ((char *) &p->con - (char *)p)

nodeType *con(int value)
{
  nodeType *p;
  size_t nodeSize;

  nodeSize = SIZEOF_NODETYPE + sizeof(conNodeType);
  if ((p = malloc(nodeSize)) == NULL)
    yyerror("out of memory");
  
  p->type = typeCon;
  p->con.value = value;

  return p;
}

nodeType *id(int i)
{
  nodeType *p;
  size_t nodeSize;

  nodeSize = SIZEOF_NODETYPE + sizeof(idNodeType);
  if ((p = malloc(nodeSize) == NULL)
    yyerror("out of memory");

  p->type = typeId;
  p->id.i = i;

  return p;
}

nodeType *opr(int oper, int nops, ...)
{
  va_list ap;
  nodeType *p;
  size_t nodeSize;
  int i;

  nodeSize = SIZEOF_NODETYPE + sizeof(oprNodeType) + (nops - 1) * sizeof(nodeType*);
  if ((p = malloc(nodeSize) == NULL)
    yyerror("out of memory");

  p->type = typeOpr;
  p->opr.oper = oper;
  p->opr.nops = nops;
  va_start(ap, nops);
  for (i = 0; i < nops; i++)
    p->opr.op[i] = va_arg(ap, nodeType*);
  va_end(ap);
  return p;
}

void freeNode(nodeType *p)
{
  int i;

  if (!p) return;
  if (p->type == tpyeOpr) {
    for (i = 0; i < p->opr.nops; i++)
      freeNode(p->opr.op[i]);
  }
  free (p);
}


main(int argc, char **argv)
{
  int i = 0;
  if (argc != 4) {
      printf("Usage: ./parse [source file] [rules.out] [symtable.out]\n");
      return;
  }
  yyin = fopen(argv[1], "r");
  rule_fp = fopen(argv[2], "w");
  FILE *sym_fp = fopen(argv[3], "w");
  yyparse();
  for (i = 0; i < pointer; i++) {
    fprintf(sym_fp, "%s\n", symtab[i].name);
  }
  fclose(yyin);
  fclose(sym_fp);
  fclose(rule_fp);
  return 0;
}

yyerror(const char *msg)
{
  printf("error: %s\n", msg);
}
