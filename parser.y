/* the syntax parser */

%{

#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include "parser.tab.h"
#include "parser.h"
#define YYDEBUG 1
#define YYLINENO

nodeType *opr(int oper, int nops, ...);
nodeType *id(int i);
nodeType *con(int value);
nodeType *newnode(constructEnum nodetype, int nops, ...);
void freeNode(nodeType *p);
int ex(nodeType *p);
int yylex(void);

%}



%union {
    int iValue;
    int iIndex;
    struct nodeTypeTag * nPtr;
};

%token <iValue> INTEGER 
/* %token <sTring> STRING */
%token <iIndex> ID
%token STRING
%token LT GT NE RG CE
%token AND begin FORWARD DIV DO ELSE END FOR FUNCTION IF ARRAY MOD
%token PROCEDURE OR OF NOT PROGRAM RECORD THEN TO TYPE VAR WHILE
%token MINUS

%type <nPtr> Program TypeDefinitions TypeDefinition_X VariableDeclarations
             VariableDeclarations_X SubprogramDeclarations TypeDefinition VariableDeclaration 
             ProcedureDeclaration FunctionDeclaration FormalParameterList
             IdlistType_X Block CompoundStatement StatementSequence 
             StatementSequence_X Statement SimpleStatement Assignment_Statement
             MatchedStatement Type Field_List Constant 
             Expression Simple_Expression Term Factor 
             Variable ComponentSelection ActualParameterList Expression_X 
             IdentifierList Id_X
             ProcedureStatement ProFunDeclarationGroup
             BlockforwardGroup 
             OpenStatement ResultType 
             AddopTerm_X MulOpFactor_X Function_Reference 



%start Program
%%

Program: PROGRAM ID ';' TypeDefinitions VariableDeclarations                                      

                 SubprogramDeclarations CompoundStatement '.'                                     { $$ = newnode(Program, 5, id($2), $4, $5, $6, $7); };


TypeDefinitions: 
               /* empty */                                                                        
               | TYPE TypeDefinition ';' TypeDefinition_X                                         { $$ = newnode(TypeDefinitions, 2, $2, $4); };
               ;

TypeDefinition_X: 
                /* empty */                                                                       
                | TypeDefinition_X TypeDefinition ';'                                             { $$ = newnode(TypeDefinition_X, 2, $1, $2); };
                ;


VariableDeclarations:
                    /* empty */                                                                   
                    | VAR VariableDeclaration ';' VariableDeclarations_X                          { $$ = newnode(VariableDeclarations, 2, $2, $4); };

VariableDeclarations_X:
                     /* empty */                                                                  
                     | VariableDeclarations_X VariableDeclaration ';'                             { $$ = newnode(VariableDeclarations_X, 2, $1, $2); };
                     ;


SubprogramDeclarations:
                      /* empty */                                                                 
                      | SubprogramDeclarations ProFunDeclarationGroup ';'                         { $$ = newnode(SubprogramDeclarations, 2, $1, $2); };
                      ;


ProFunDeclarationGroup: ProcedureDeclaration                                                      { $$ = newnode(ProFunDeclarationGroup, 1, $1); };
                      | FunctionDeclaration                                                       { $$ = newnode(ProFunDeclarationGroup, 1, $1); };
                      ;

TypeDefinition: ID '=' Type                                                                       { $$ = newnode(TypeDefinition, 2, id($1), $3); };


VariableDeclaration: IdentifierList ':' Type                                                      { $$ = newnode(VariableDeclaration, 2, $1, $3); };

ProcedureDeclaration: PROCEDURE ID '(' FormalParameterList ')' ';' BlockforwardGroup              { $$ = newnode(ProcedureDeclaration, 3, id($2), $4, $7); };
FunctionDeclaration: FUNCTION ID '(' FormalParameterList ')' ':' ResultType ';' BlockforwardGroup { $$ = newnode(FunctionDeclaration, 4, id($2), $4, $7, $9); };

BlockforwardGroup: Block                                                                          { $$ = newnode(BlockforwardGroup, 1, $1); };
                 | FORWARD                                                                        {};
                 ;


FormalParameterList:
                   /* empty */                                                                    
                   | IdentifierList ':' Type IdlistType_X                                         { $$ = newnode(FormalParameterList, 3, $1, $3, $4); };
                   ;

IdlistType_X:
            /* empty */                                                                           
            | IdlistType_X ';' IdentifierList ':' Type                                            { $$ = newnode(IdlistType_X, 3, $1, $3, $5); };
            ;


Block: VariableDeclarations CompoundStatement                                                     { $$ = newnode(Block, 2, $1, $2); };


CompoundStatement: begin StatementSequence END                                                    { $$ = newnode(CompoundStatement, 1, $2); };
StatementSequence: Statement StatementSequence_X                                                  { $$ = newnode(StatementSequence, 2, $1, $2); };

StatementSequence_X:
                   /* empty */                                                                    
                   | StatementSequence_X ';' Statement                                            { $$ = newnode(StatementSequence_X, 2, $1, $3); };
                   ;


Statement: OpenStatement                                                                          { $$ = newnode(Statement, 1, $1); };
         | MatchedStatement                                                                       { $$ = newnode(Statement, 1, $1); };
         ;



SimpleStatement:
               /* empty */                                                                        
               | Assignment_Statement                                                             { $$ = newnode(SimpleStatement, 1, $1); };
               | ProcedureStatement                                                               { $$ = newnode(SimpleStatement, 1, $1); };
               ;


Assignment_Statement: Variable CE Expression                                                      { $$ = newnode(Assignment_Statement, 2, $1, $3); };

ProcedureStatement: ID '(' ActualParameterList ')'                                                { $$ = newnode(ProceedureStatement, 2, id($1), $3); };


MatchedStatement: IF Expression THEN MatchedStatement ELSE MatchedStatement                       { $$ = opr(IF, 3, $2, $4, $6); };
                | CompoundStatement                                                               { $$ = newnode(MatchedStatement, 1, $1); };
                | SimpleStatement                                                                 { $$ = newnode(MatchedStatement, 1, $1); };
                | WHILE Expression DO MatchedStatement                                            { $$ = opr(WHILE, 2, $2, $4); };
                | FOR ID CE Expression TO Expression DO MatchedStatement                          { $$ = opr(FOR, 4, id($2), $4, $6, $8); };
                ;

OpenStatement: IF Expression THEN Statement                                                       { $$ = opr(IF, 2, $2, $4); };
             | IF Expression THEN MatchedStatement ELSE OpenStatement                             { $$ = opr(IF, 3, $2, $4, $6); };
             | WHILE Expression DO OpenStatement                                                  { $$ = opr(WHILE, 2, $2, $4); };
             | FOR ID CE Expression TO Expression DO OpenStatement                                { $$ = opr(FOR, 4, id($2), $4, $6, $8); };
             ;

Type: ID                                                                                          { $$ = id($1); };
    | ARRAY '[' Constant RG Constant ']' OF Type                                                  { $$ = opr(ARRAY, 3, $3, $5, $8); };
    | RECORD Field_List END                                                                       { $$ = newnode(Type, 1, $2); };
    ;

ResultType: ID                                                                                    { $$ = id($1); };

Field_List:
          /* empty */                                                                             
          | IdentifierList ':' Type IdlistType_X                                                  { $$ = newnode(Field_List, 3, $1, $3, $4); };
          ;


Constant: INTEGER                                                                                 { $$ = con($1); };
        | '+' INTEGER                                                                             { $$ = con($2); };
        | '-' INTEGER                                                                             { $$ = opr(MINUS, 1, con($2)); };
        ;

Expression: Simple_Expression                                                                     { $$ = newnode(Expression, 1, $1); };
          | Simple_Expression '<' Simple_Expression                                               { $$ = opr('<', 2, $1, $3); };
          | Simple_Expression LT Simple_Expression                                                { $$ = opr(LT , 2, $1, $3); };
          | Simple_Expression '>' Simple_Expression                                               { $$ = opr('>', 2, $1, $3); };
          | Simple_Expression GT Simple_Expression                                                { $$ = opr(GT , 2, $1, $3); };
          | Simple_Expression NE Simple_Expression                                                { $$ = opr(NE , 2, $1, $3); };
          | Simple_Expression '=' Simple_Expression                                               { $$ = opr('=', 2, $1, $3); };
          ;

/*Relational_Op: '<'                                                                                { $$ = $1; }; 
             | LT                                                                                 { $$ = $1; }; 
             | '>'                                                                                { $$ = $1; }; 
             | GT                                                                                 { $$ = $1; };
             | NE                                                                                 { $$ = $1; };
             | '='                                                                                { $$ = $1; };
             ; */

Simple_Expression: Term AddopTerm_X                                                               { $$ = newnode(Simple_Expression ,2, $1, $2); };
                 | '+' Term AddopTerm_X                                                           { $$ = newnode(Simple_Expression, 2, $2, $3); };
                 | '-' Term AddopTerm_X                                                           { $$ = opr(MINUS, 2, $2, $3); };
                 ;

AddopTerm_X:
           /* empty */                                                                            
           | AddopTerm_X '+' Term                                                               { $$ = opr('+', 2, $1, $3); };
           | AddopTerm_X '-' Term                                                               { $$ = opr('-', 2, $1, $3); };
           | AddopTerm_X OR Term                                                                { $$ = opr(OR , 2, $1, $3); };
           ;

/* AddOp: '+'                                                                                        { $$ = $1; };
     | '-'                                                                                        { $$ = $1; };
     | OR                                                                                         { $$ = $1; };
     ; */

Term: Factor MulOpFactor_X                                                                        { $$ = newnode(Term, 2, $1, $2); };

MulOpFactor_X:
             /* empty */                                                                          
             | MulOpFactor_X '*' Factor                                                         { $$ = opr('*', 2, $1, $3); };
             | MulOpFactor_X DIV Factor                                                         { $$ = opr(DIV, 2, $1, $3); };
             | MulOpFactor_X MOD Factor                                                         { $$ = opr(MOD, 2, $1, $3); };
             | MulOpFactor_X AND Factor                                                         { $$ = opr(AND, 2, $1, $3); };
             ;

/* MulOp: '*'                                                                                        { $$ = $1; }; 
     | DIV                                                                                        { $$ = $1; }; 
     | MOD                                                                                        { $$ = $1; };
     | AND                                                                                        { $$ = $1; };
     ; */

Factor: INTEGER                                                                                   { $$ = con($1); };
      | STRING                                                                                    {};
      | Variable                                                                                  { $$ = newnode(Factor, 1, $1); };
      | Function_Reference                                                                        { $$ = newnode(Factor, 1, $1); };
      | NOT Factor                                                                                { $$ = opr(NOT, 1, $2); };
      | '(' Expression ')'                                                                        { $$ = newnode(Factor, 1, $2); };
      ;

Function_Reference: ID '(' ActualParameterList ')'                                                { $$ = newnode(Function_Reference, 2, id($1), $3); };

Variable: ID ComponentSelection                                                                   { $$ = newnode(Variable, 2, id($1), $2); };

ComponentSelection:
                  /* empty */                                                                     
                  | '.' ID ComponentSelection                                                     { $$ = newnode(ComponentSelection, 2, id($2), $3); };
                  | '[' Expression ']' ComponentSelection                                         { $$ = newnode(ComponentSelection, 2, $2, $4); };
                  ;

ActualParameterList:
                   /* empty */                                                                    
                   | Expression Expression_X                                                      { $$ = newnode(ActualParameterList, 2, $1, $2); };
                   ;
Expression_X:
            /* empty */                                                                           
            | Expression_X ',' Expression                                                         { $$ = newnode(Expression_X, 2, $1, $3); };
            ;

IdentifierList: ID Id_X                                                                           { $$ = newnode(IdentifierList, 2, id($1), $2); };

Id_X:
    /* empty */                                                                                   
    | Id_X ',' ID                                                                                 { $$ = newnode(Id_X, 2, $1, id($3)); };
    ;

/* Sign: '+'                                                                                         { $$ = $1; };
    | '-'                                                                                         { $$ = $1; };
    ; */

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
  if ((p = malloc(nodeSize)) == NULL)
    yyerror("out of memory");

  p->type = typeId;
  p->id.i = i;

  return p;
}

/* nodeType *str(ystring ss)
{
  nodeType *p;
  size_t nodeSize;

  if (ss.length > STRING_LIMIT)
    yyerror("the string is beyond limitation!");

  nodeSize = SIZEOF_NODETYPE + sizeof(strNodeType);
  if ((p = malloc(nodeSize) == NULL)
    yyerror("out of memory");

  p->type = typeStr;
  strncpy(p->str.text, ss.text, ss.length);
  return *p;
} */

nodeType *opr(int oper, int nops, ...)
{
  va_list ap;
  nodeType *p;
  size_t nodeSize;
  int i;

  nodeSize = SIZEOF_NODETYPE + sizeof(oprNodeType) + (nops - 1) * sizeof(nodeType*);
  if ((p = malloc(nodeSize)) == NULL)
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

nodeType *newnode(constructEnum nodetype, int nops, ...)
{
  va_list ap;
  nodeType *p;
  size_t nodeSize;
  int i;
  
  nodeSize = SIZEOF_NODETYPE + sizeof(interNodeType) + (nops - 1) * sizeof(nodeType *);
  if ((p = malloc(nodeSize)) == NULL)
    yyerror("out of memory");

  p->type = typeConstruct;
  p->node.type = nodetype;
  p->node.nops = nops;
  va_start(ap, nops);
  for (i = 0; i < nops; i++)
    p->node.node[i] = va_arg(ap, nodeType*);
  va_end(ap);
  return p;
}
  
void freeNode(nodeType *p)
{
  int i;

  if (!p) return;
  if (p->type == typeOpr) {
    for (i = 0; i < p->opr.nops; i++)
      freeNode(p->opr.op[i]);
  }
  free (p);
}


int
main(int argc, char **argv)
{
  yyin = fopen(argv[1], "r");
  yyparse();
  fclose(yyin);
  return 0;
}

yyerror(const char *msg)
{
  printf("error: %s\n", msg);
}
