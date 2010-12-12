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
    nodeType *nPtr;
};

%type <nPtr> TypeDefinitions VariableDeclarations SubprogramDeclarations CompoundStatement TypeDefinition TypeDefinition_X TypeDefinition
%type <nPtr> VariableDeclarations_X ProFunDeclarationGroup


%start Program
%%

Program: PROGRAM ID ';' TypeDefinitions VariableDeclarations                                      

                 SubprogramDeclarations CompoundStatement '.'                                     { $$ = opr(ProgramNode, 5, id($2), $4, $5, $6, $7); };


TypeDefinitions: 
               /* empty */                                                                        
               | TYPE TypeDefinition ';' TypeDefinition_X                                         { $$ = opr(TypeDefinitionsNode, 2, $2, $4); };
               ;

TypeDefinition_X: 
                /* empty */                                                                       
                | TypeDefinition_X TypeDefinition ';'                                             { $$ = opr(TypeDefinition_XNode, 2, $1, $2); };
                ;


VariableDeclarations:
                    /* empty */                                                                   
                    | VAR VariableDeclaration ';' VariableDeclarations_X                          { $$ = opr(VariableDeclarationsNode, 2, $2, $4); };

VariableDeclarations_X:
                     /* empty */                                                                  
                     | VariableDeclarations_X VariableDeclaration ';'                             { $$ = opr(VariableDeclarations_XNode, 2, $1, $2); };
                     ;


SubprogramDeclarations:
                      /* empty */                                                                 
                      | SubprogramDeclarations ProFunDeclarationGroup ';'                         { $$ = opr(SubprogramDeclarationsNode, 2, $1, $2); };
                      ;


ProFunDeclarationGroup: ProcedureDeclaration                                                      { $$ = $1; };
                      | FunctionDeclaration                                                       { $$ = $1; };
                      ;

TypeDefinition: ID '=' Type                                                                       { $$ = opr(TypeDefinition, 2, id($1), $3); };


VariableDeclaration: IdentifierList ':' Type                                                      { $$ = opr(VariableDeclaration, 2, $1, $3); };

ProcedureDeclaration: PROCEDURE ID '(' FormalParameterList ')' ';' BlockforwardGroup              { $$ = opr(ProcedureDeclaration, 3, id($2), $4, $7); };
FunctionDeclaration: FUNCTION ID '(' FormalParameterList ')' ':' ResultType ';' BlockforwardGroup { $$ = opr(FunctionDeclaration, 4, id($2), $4, $7, $9); };

BlockforwardGroup: Block                                                                          { $$ = $1; }
                 | FORWARD
                 ;


FormalParameterList:
                   /* empty */                                                                    
                   | FormalParameterList_E                                                        { $$ = $1; };
                   ;

FormalParameterList_E: IdentifierList ':' Type IdlistType_X                                       { $$ = opr(FormalParameterList_E, 3, $1, $3, $4); };

IdlistType_X:
            /* empty */                                                                           
            | IdlistType_X ';' IdentifierList ':' Type                                            { $$ = opr(IdlistType_X, 3, $1, $3, $5); };
            ;


Block: VariableDeclarations CompoundStatement                                                     { $$ = opr(Block, 2, $1, $2); };


CompoundStatement: begin StatementSequence END                                                    { $$ = $2; };
StatementSequence: Statement StatementSequence_X                                                  { $$ = opr(StatementSequence, 2, $1, $2); };

StatementSequence_X:
                   /* empty */                                                                    
                   | StatementSequence_X ';' Statement                                            { $$ = opr(StatementSequence_X, 2, $1, $3); };
                   ;


Statement: OpenStatement                                                                          { $$ = $1; };
         | MatchedStatement                                                                       { $$ = $1; };
         ;



SimpleStatement:
               /* empty */                                                                        
               | Assignment_Statement                                                             { $$ = $1; };
               | ProcedureStatement                                                               { $$ = $1; };
               ;


Assignment_Statement: Variable CE Expression                                                      { $$ = opr(CE, 2, $1, $3); };

ProcedureStatement: ID '(' ActualParameterList ')'                                                { $$ = opr(ProceedureStatementNode, 2, id($1), $3); };


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

Type: ID                                                                                          { $$ = id($1); };
    | ARRAY '[' Constant RG Constant ']' OF Type                                                  { $$ = opr(ARRAY, 3, $3, $5, $8); };
    | RECORD Field_List END                                                                       { $$ = $2; };
    ;

ResultType: ID                                                                                    { $$ = $1; };

Field_List:
          /* empty */                                                                             
          | IdentifierList ':' Type IdlistType_X                                                  { $$ = opr(Field_List, 3, $1, $3, $4); };
          ;


Constant: INTEGER                                                                                 { $$ = con($1); };
        | Sign INTEGER                                                                            { $$ = opr(Constant, 2, $1, $2); };
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

Simple_Expression: Term AddopTerm_X                                                               { $$ = opr(Simple_Expression ,2, $1, $2); };
                 | Sign Term AddopTerm_X                                                          { $$ = opr(Simple_Expression, 3, $1, $2, $3); };
                 ;

AddopTerm_X:
           /* empty */                                                                            
           | AddopTerm_X AddOp Term                                                               { $$ = opr($2, 2, $1, $3); };
           ;

AddOp: '+'                                                                                        { $$ = $1; };
     | '-'                                                                                        { $$ = $1; };
     | OR                                                                                         { $$ = $1; };
     ;

Term: Factor MulOpFactor_X                                                                        { $$ = opr(Term, $1, $2); };

MulOpFactor_X:
             /* empty */                                                                          
             | MulOpFactor_X MulOp Factor                                                         { $$ = opr($2, 2, $1, $3); };
             ;

MulOp: '*'                                                                                        { $$ = $1; }; 
     | DIV                                                                                        { $$ = $1; }; 
     | MOD                                                                                        { $$ = $1; };
     | AND                                                                                        { $$ = $1; };
     ;

Factor: INTEGER                                                                                   { $$ = id($1); };
      | STRING                                                                                    { $$ = $1; };
      | Variable                                                                                  { $$ = $1; };
      | Function_Reference                                                                        { $$ = $1; };
      | NOT Factor                                                                                { $$ = opr(NOT, 1, $2); };
      | '(' Expression ')'                                                                        { $$ = $2; };
      ;

Function_Reference: ID '(' ActualParameterList ')'                                                { $$ = opr(Function_Reference, 2, id($1), $3); };

Variable: ID ComponentSelection                                                                   { $$ = opr(Variable, 2, id($1), $2); };

ComponentSelection:
                  /* empty */                                                                     
                  | '.' ID ComponentSelection                                                     { $$ = opr(ComponentSelection, 2, $2, $3); };
                  | '[' Expression ']' ComponentSelection                                         { $$ = opr(ComponentSelection, 2, $2, $4); };
                  ;

ActualParameterList:
                   /* empty */                                                                    
                   | Expression Expression_X                                                      { $$ = opr(ActualParameterList, 2, $1, $3); };
                   ;
Expression_X:
            /* empty */                                                                           
            | Expression_X ',' Expression                                                         { $$ = opr(Expression_X, 2, $1, $3); };
            ;

IdentifierList: ID Id_X                                                                           { $$ = opr(IdentifierList, 2, id($1), $2); };

Id_X:
    /* empty */                                                                                   
    | Id_X ',' ID                                                                                 { $$ = opr(Id_X, 2, $1, id($3)); };
    ;

Sign: '+'                                                                                         { $$ = $1; };
    | '-'                                                                                         { $$ = $1; };
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
