/* the syntax parser */

%{

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include "parser.tab.h"
#include "parser.h"
#define YYDEBUG 1
#define YYLINENO

nodeType *opr(int oper, int nops, ...);
nodeType *id(int i);
nodeType *con(int value);
nodeType *newnode(constructEnum nodetype, int nops, ...);
void freeNode(nodeType *p);
void doTask(nodeType *p, int indent);
void i2name(constructEnum i, int indent);
void chkVar(nodeType *p, int scope);
void indent(int indent);
int setVar(nodeType *p, int scope);
int i2type(nodeEnum i);
int scopeTraveler(int scope, char *name);
void doCheck(nodeType *p, int scope);
int yylex(void);
symbolEntry symTab[NENTRY];
int symPointer = 0;

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

%type <nPtr> Program TypeDefinitions TypeDefinitionSequence VariableDeclarations
             VariableDeclarations_X SubprogramDeclarations TypeDefinition VariableDeclaration 
             ProcedureDeclaration FunctionDeclaration FormalParameterList
             Block CompoundStatement StatementSequence 
             Statement SimpleStatement Assignment_Statement
             MatchedStatement Type Field_List Constant 
             Expression Simple_Expression Term Factor 
             Variable ComponentSelection ActualParameterList 
             IdentifierList 
             ProcedureStatement ProFunDeclarationGroup
             OpenStatement ResultType 
             Function_Reference 



%start Program
%%
Program: PROGRAM ID ';' TypeDefinitions VariableDeclarations                                      

                 SubprogramDeclarations CompoundStatement '.'                                     { $$ = newnode(Program, 5, id($2), $4, $5, $6, $7); doTask($$, 0); doCheck($$, -1); };


TypeDefinitions: TYPE TypeDefinitionSequence                                                      { $$ = newnode(TypeDefinitions, 1, $2); };

TypeDefinitionSequence: TypeDefinition ';'                                                        { $$ = $1; };
                      | TypeDefinitionSequence TypeDefinition ';'                                 { $$ = newnode(TypeDefinitionSequence, 2, $1, $2); };
                      ;


VariableDeclarations:
                    /* empty */                                                                   { $$ = NULL; };
                    | VAR VariableDeclaration ';' VariableDeclarations_X                          { $$ = newnode(VariableDeclarations, 2, $2, $4); };

VariableDeclarations_X:
                     /* empty */                                                                  { $$ = NULL; };
                     | VariableDeclarations_X VariableDeclaration ';'                             { $$ = newnode(VariableDeclarations_X, 2, $1, $2); };
                     ;


SubprogramDeclarations:
                      /* empty */                                                                 { $$ = NULL; };
                      | SubprogramDeclarations ProFunDeclarationGroup ';'                         { $$ = newnode(SubprogramDeclarations, 2, $1, $2); };
                      ;


ProFunDeclarationGroup: ProcedureDeclaration                                                      { $$ = newnode(ProFunDeclarationGroup, 1, $1); };
                      | FunctionDeclaration                                                       { $$ = newnode(ProFunDeclarationGroup, 1, $1); };
                      ;

TypeDefinition: ID '=' Type                                                                       { $$ = newnode(TypeDefinition, 2, id($1), $3); };


VariableDeclaration: IdentifierList ':' Type                                                      { $$ = newnode(VariableDeclaration, 2, $1, $3); };

ProcedureDeclaration: PROCEDURE ID '(' FormalParameterList ')' ';' Block                          { $$ = newnode(ProcedureDeclaration, 3, id($2), $4, $7); };
                    | PROCEDURE ID '(' FormalParameterList ')' ';' FORWARD                        { $$ = newnode(ProcedureDeclaration, 2, id($2), $4); };
                    ;

FunctionDeclaration: FUNCTION ID '(' FormalParameterList ')' ':' ResultType ';' Block             { $$ = newnode(FunctionDeclaration, 4, id($2), $4, $7, $9); };
                   | FUNCTION ID '(' FormalParameterList ')' ':' ResultType ';' FORWARD           { $$ = newnode(FunctionDeclaration, 3, id($2), $4, $7); };



FormalParameterList:
                   /* empty */                                                                    { $$ = NULL; };
                   | IdentifierList ':' Type                                                      { $$ = newnode(FormalParameterList, 2, $1, $3); };
                   | IdentifierList ':' Type ';' FormalParameterList                              { $$ = newnode(FormalParameterList, 3, $1, $3, $5); };
                   ;

/* IdlistType_X:
             empty                                                                            { $$ = NULL; };
            | IdlistType_X ';' IdentifierList ':' Type                                            { $$ = newnode(IdlistType_X, 3, $1, $3, $5); };
            ; */


Block: VariableDeclarations CompoundStatement                                                     { $$ = newnode(Block, 2, $1, $2); };


CompoundStatement: begin StatementSequence END                                                    { $$ = newnode(CompoundStatement, 1, $2); };

/* StatementSequence: Statement StatementSequence_X                                                  { $$ = newnode(StatementSequence, 2, $1, $2); }; */

StatementSequence: Statement                                                                      { $$ = newnode(StatementSequence, 1, $1); };
                 | Statement ';' StatementSequence                                                { $$ = newnode(StatementSequence, 2, $1, $3); };
                 ;

/* StatementSequence_X:
                   * empty                                                                     { $$ = NULL; };
                   | StatementSequence_X ';' Statement                                            { $$ = newnode(StatementSequence_X, 2, $1, $3); };
                   ; */


Statement: OpenStatement                                                                          { $$ = newnode(Statement, 1, $1); };
         | MatchedStatement                                                                       { $$ = newnode(Statement, 1, $1); };
         ;



SimpleStatement:
               /* empty */                                                                        { $$ = NULL; };
               | Assignment_Statement                                                             { $$ = newnode(SimpleStatement, 1, $1); };
               | ProcedureStatement                                                               { $$ = newnode(SimpleStatement, 1, $1); };
               ;


Assignment_Statement: Variable CE Expression                                                      { $$ = newnode(Assignment_Statement, 2, $1, $3); };

ProcedureStatement: ID '(' ActualParameterList ')'                                                { $$ = newnode(ProcedureStatement, 2, id($1), $3); };


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
          /* empty */                                                                             { $$ = NULL; };
          | IdentifierList ':' Type /*IdlistType_X*/                                              { $$ = newnode(Field_List, 2, $1, $3); };
          | IdentifierList ':' Type ';' Field_List                                                { $$ = newnode(Field_List, 3, $1, $3, $5); };
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

/*Relational_Op: '<'                                                                              { $$ = $1; }; 
             | LT                                                                                 { $$ = $1; }; 
             | '>'                                                                                { $$ = $1; }; 
             | GT                                                                                 { $$ = $1; };
             | NE                                                                                 { $$ = $1; };
             | '='                                                                                { $$ = $1; };
             ; */

Simple_Expression: Term                                                                                     { $$ = newnode(Simple_Expression, 1, $1); };
                 | Term '+' Simple_Expression                                                               { $$ = opr('+', 2, $1, $3); };
                 | Term '-' Simple_Expression                                                               { $$ = opr('-', 2, $1, $3); };
                 | Term OR  Simple_Expression                                                               { $$ = opr(OR , 2, $1, $3); };
                 | '+' Term '+' Simple_Expression                                                           { $$ = opr('+', 2, $2, $4); };
                 | '+' Term '-' Simple_Expression                                                           { $$ = opr('-', 2, $2, $4); };
                 | '+' Term OR  Simple_Expression                                                           { $$ = opr(OR,  2, $2, $4); };
                 | '-' Term '+' Simple_Expression                                                           { $$ = opr(MINUS, 1, opr('+', 2, $2, $4)); };
                 | '-' Term '-' Simple_Expression                                                           { $$ = opr(MINUS, 1, opr('-', 2, $2, $4)); };
                 | '-' Term OR  Simple_Expression                                                           { $$ = opr(MINUS, 1, opr(OR,  2, $2, $4)); };
                 ; 

/* Simple_Expression: Term                                                                                     { $$ = newnode(Simple_Expression, 1, $1); };
                 | Simple_Expression '+' Term                                                               { $$ = opr('+', 2, $1, $3); };
                 | Simple_Expression '-' Term                                                               { $$ = opr('-', 2, $1, $3); };
                 | Simple_Expression OR  Term                                                               { $$ = opr(OR , 2, $1, $3); };
                 | '+' Simple_Expression '+' Term                                                           { $$ = opr('+', 2, $2, $4); };
                 | '+' Simple_Expression '-' Term                                                           { $$ = opr('-', 2, $2, $4); };
                 | '+' Simple_Expression OR  Term                                                           { $$ = opr(OR,  2, $2, $4); };
                 | '-' Simple_Expression '+' Term                                                           { $$ = opr(MINUS, 1, opr('+', 2, $2, $4)); };
                 | '-' Simple_Expression '-' Term                                                           { $$ = opr(MINUS, 1, opr('-', 2, $2, $4)); };
                 | '-' Simple_Expression OR  Term                                                           { $$ = opr(MINUS, 1, opr(OR,  2, $2, $4)); };
                 ; */



/* AddOp: '+'                                                                                     { $$ = $1; };
     | '-'                                                                                        { $$ = $1; };
     | OR                                                                                         { $$ = $1; };
     ; */

Term: Factor                                                                                                { $$ = newnode(Term, 1, $1); };
    | Factor '*' Term                                                                                       { $$ = opr('*', 2, $1, $3); };
    | Factor DIV Term                                                                                       { $$ = opr(DIV, 2, $1, $3); };
    | Factor MOD Term                                                                                       { $$ = opr(MOD, 2, $1, $3); };
    | Factor AND Term                                                                                       { $$ = opr(AND, 2, $1, $3); };
    ;

/* MulOpFactor_X:
              empty                                                                           { $$ = NULL; };
             | MulOpFactor_X '*' Factor                                                           { $$ = opr('*', 2, $1, $3); };
             | MulOpFactor_X DIV Factor                                                           { $$ = opr(DIV, 2, $1, $3); };
             | MulOpFactor_X MOD Factor                                                           { $$ = opr(MOD, 2, $1, $3); };
             | MulOpFactor_X AND Factor                                                           { $$ = opr(AND, 2, $1, $3); };
             ; */

/* MulOp: '*'                                                                                        { $$ = $1; }; 
     | DIV                                                                                        { $$ = $1; }; 
     | MOD                                                                                        { $$ = $1; };
     | AND                                                                                        { $$ = $1; };
     ; */

Factor: INTEGER                                                                                   { $$ = con($1); };
      | STRING                                                                                    { $$ = NULL; };
      | Variable                                                                                  { $$ = newnode(Factor, 1, $1); };
      | Function_Reference                                                                        { $$ = newnode(Factor, 1, $1); };
      | NOT Factor                                                                                { $$ = opr(NOT, 1, $2); };
      | '(' Expression ')'                                                                        { $$ = newnode(Factor, 1, $2); };
      ;

Function_Reference: ID '(' ActualParameterList ')'                                                { $$ = newnode(Function_Reference, 2, id($1), $3); };

Variable: ID ComponentSelection                                                                   { $$ = newnode(Variable, 2, id($1), $2); };

ComponentSelection:
                  /* empty */                                                                     { $$ = NULL; }; 
                  | '.' ID ComponentSelection                                                     { $$ = newnode(ComponentSelection, 2, id($2), $3); };
                  | '[' Expression ']' ComponentSelection                                         { $$ = newnode(ComponentSelection, 2, $2, $4); };
                  ;

/* ActualParameterList:
                    empty                                                                     { $$ = NULL; };
                   | Expression Expression_X                                                      { $$ = newnode(ActualParameterList, 2, $1, $2); };
                   ; */

ActualParameterList:
                   /* empty */                                                                    { $$ = NULL; };
                   | Expression                                                                   { $$ = newnode(ActualParameterList, 1, $1); };
                   | Expression ',' ActualParameterList                                           { $$ = newnode(ActualParameterList, 2, $1, $3); };
                   ;
/* Expression_X:
             empty                                                                            { $$ = NULL; };
            | Expression_X ',' Expression                                                         { $$ = newnode(Expression_X, 2, $1, $3); };
            ; */

IdentifierList: ID                                                                                { $$ = newnode(IdentifierList, 1, id($1)); };
              | ID ',' IdentifierList                                                             { $$ = newnode(IdentifierList, 2, id($1), $3); };

/* Id_X:
     empty                                                                                    { $$ = NULL; }
    | Id_X ',' ID                                                                                 { $$ = newnode(Id_X, 2, $1, id($3)); };
    ; */

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
  else if (p->type == typeConstruct) {
    for (i = 0; i < p->node.nops; i++)
      freeNode(p->node.node[i]);
  }    

  free (p);
}

void
doCheck(nodeType *p, int scope)
{
  int i;
  int current_scope;
  nodeEnum type;
  constructEnum node_type;
  if (!p) return;
  if (p->type == typeConstruct) {
    node_type = p->node.type;
    if (node_type == VariableDeclaration) {
      setVar(p->node.node[0], scope);
      return;
    }
    else if (node_type == FormalParameterList) {
      setVar(p->node.node[0], scope);
      if (p->node.nops == 3)
        doCheck(p->node.node[2], scope);
      return;
    }
    else if (node_type == Assignment_Statement) {
      chkVar(p->node.node[0], scope);
      return;
    }

    else if (node_type == Program) {
      current_scope = setVar(p->node.node[0], scope);
      for (i = 1; i < p->node.nops; i++)
        doCheck(p->node.node[i], current_scope);
    }
    else if (node_type == ProcedureDeclaration || node_type == FunctionDeclaration) {
      current_scope = setVar2(p->node.node[0], scope);
      for (i = 1; i < p->node.nops; i++)
        doCheck(p->node.node[i], current_scope);
    }
    else {
      for (i = 0; i < p->node.nops; i++)
        doCheck(p->node.node[i], scope);
    }
  }
  else {
    return;
  }
}
int
setVar2(nodeType *p, int scope)
{
  int i;
  int index;
  if (!p) return;
  if (p->type == typeId) {
    for(i = 0; i < symPointer; i++) 
      if (!strcmp(symTab[i].lexeme, idTab[p->id.i].name) && symTab[i].scope == scope) {  /* the var has been declared */
        return -1;
      }
    strcpy(symTab[symPointer].lexeme, idTab[p->id.i].name);
    symTab[symPointer].scope = scope;
    index = symPointer++;
    return index;
  }
  else {
    //i2type(p->type);
    assert(p->node.type == IdentifierList);
    if (p->node.type == IdentifierList) {
      setVar(p->node.node[0], scope);
      setVar(p->node.node[1], scope);
    }
  }
  return -2;
}

void chkVar(nodeType *p, int scope)
{
  int i;
  int result;
  assert(p->node.type == Variable);
  result = scopeTraveler(scope, idTab[p->node.node[0]->id.i].name);
  if (result == -1)
    printf("Semantic Error: %s must be declared first!\n", idTab[p->node.node[0]->id.i].name);
  return;
}

int scopeTraveler(int scope, char *name)
{
  int i;
  if (scope == -1)
    return scope;
  for (i = 0; i < symPointer; i++) {
    if (symTab[i].scope == scope && !strcmp(symTab[i].lexeme, name))
      return i;
  }
  if (i == symPointer)
    return scopeTraveler(symTab[scope].scope, name);
  assert(0);
}

int
setVar(nodeType *p, int scope)
{
  int i;
  int index;
  if (!p) return;
  if (p->type == typeId) {
    for(i = 0; i < symPointer; i++) 
      if (!strcmp(symTab[i].lexeme, idTab[p->id.i].name) && symTab[i].scope == scope) {  /* the var has been declared */
        printf("Semantic Error: %s is multi-declared!\n", idTab[p->id.i].name);
        return -1;
      }
    strcpy(symTab[symPointer].lexeme, idTab[p->id.i].name);
    symTab[symPointer].scope = scope;
    index = symPointer++;
    return index;
  }
  else {
    //i2type(p->type);
    assert(p->node.type == IdentifierList);
    if (p->node.type == IdentifierList) {
      setVar(p->node.node[0], scope);
      setVar(p->node.node[1], scope);
    }
  }
  return -2;
}
      
i2type(nodeEnum i)
{
  switch(i) {
    case typeCon: printf("typeCon\n"); break;
    case typeId : printf("typeId \n"); break;
    case typeOpr: printf("typeOpr\n"); break;
    case typeConstruct: printf("typeConstruct\n"); break;
    default: break;
  }
}   
void
doTask(nodeType *p, int ind)
{
  int i;
  int operand;
  if (!p) return;
  if (p->type == typeOpr) {
    operand = p->opr.oper;
    switch(operand) {
      case IF:      indent(ind); printf("IF\n"); break;
      case WHILE:   indent(ind); printf("WHILE\n"); break;
      case FOR:     indent(ind); printf("FOR\n"); break;
      case ARRAY:   indent(ind); printf("ARRAY\n"); break;
      case MINUS:   indent(ind); printf("MINUS\n"); break;
      case LT:      indent(ind); printf("<=\n"); break;
      case GT:      indent(ind); printf(">=\n"); break;
      case NE:      indent(ind); printf("<>\n"); break;
      case OR:      indent(ind); printf("OR\n"); break;
      case DIV:     indent(ind); printf("DIV\n"); break;
      case MOD:     indent(ind); printf("MOD\n"); break;
      case AND:     indent(ind); printf("AND\n"); break;
      default:      indent(ind); printf("%c\n", operand); break;
    }
    for (i = 0; i < p->opr.nops; i++)
      doTask(p->opr.op[i], ind+1);
  }
  else if (p->type == typeConstruct) {
    i2name(p->node.type, ind);
    for (i = 0; i < p->node.nops; i++)
      doTask(p->node.node[i], ind+1);
  }
  else if (p->type == typeId) {
    indent(ind);
    printf("<id, %d>\n", p->id.i);
    return;
  }
  else if (p->type == typeCon) {
    indent(ind);
    printf("<INT, %d>\n", p->con.value);
    return;
  }
}

void
i2name(constructEnum i, int ind)
{
  switch(i) {
    case Program:                       indent(ind); printf("Program\n"); break;
    case TypeDefinitions:               indent(ind); printf("TypeDefinitions\n"); break;
    case TypeDefinitionSequence:        indent(ind); printf("TypeDefinitionSequence\n"); break;
    case VariableDeclarations:          indent(ind); printf("VariableDeclarations\n"); break;
    case VariableDeclarations_X:        indent(ind); printf("VariableDeclarations_X\n"); break;
    case TypeDefinition:                indent(ind); printf("TypeDefinition\n"); break;
    case VariableDeclaration:           indent(ind); printf("VariableDeclaration\n"); break;
    case ProcedureDeclaration:          indent(ind); printf("ProcedureDeclaration\n"); break;
    case FunctionDeclaration:           indent(ind); printf("FunctionDeclaration\n"); break;
    case FormalParameterList:           indent(ind); printf("FormalParameterList\n"); break;
    case IdlistType_X:                  indent(ind); printf("IdlistType_X\n"); break;
    case BlockforwardGroup:             indent(ind); printf("BlockforwardGroup\n"); break;
    case Block:                         indent(ind); printf("Block\n"); break;
    case CompoundStatement:             indent(ind); printf("CompoundStatement\n"); break;
    case StatementSequence:             indent(ind); printf("StatementSequence\n"); break;
    case StatementSequence_X:           indent(ind); printf("StatementSequence_X\n"); break;
    case Statement:                     indent(ind); printf("Statement\n"); break;
    case SimpleStatement:               indent(ind); printf("SimpleStatement\n"); break;
    case Assignment_Statement:          indent(ind); printf("Assignment_Statement\n"); break;
    case ProcedureStatement:            indent(ind); printf("ProcedureStatement\n"); break;
    case MatchedStatement:              indent(ind); printf("MatchedStatement\n"); break;
    case Type:                          indent(ind); printf("Type\n"); break;
    case Field_List:                    indent(ind); printf("Field_List\n"); break;
    case Constant:                      indent(ind); printf("Constant\n"); break;
    case Expression:                    indent(ind); printf("Expression\n"); break;
    case Simple_Expression:             indent(ind); printf("Simple_Expression\n"); break;
    case Term:                          indent(ind); printf("Term\n"); break;
    case Factor:                        indent(ind); printf("Factor\n"); break;
    case Variable:                      indent(ind); printf("Variable\n"); break;
    case ComponentSelection:            indent(ind); printf("ComponentSelection\n"); break;
    case ActualParameterList:           indent(ind); printf("ActualParameterList\n"); break;
    case Expression_X:                  indent(ind); printf("Expression_X\n"); break;
    case IdentifierList:                indent(ind); printf("IdentifierList\n"); break;
    case Id_X:                          indent(ind); printf("Id_X\n"); break;
    case SubprogramDeclarations:        indent(ind); printf("SubprogramDeclarations\n"); break;
    case ProFunDeclarationGroup:        indent(ind); printf("ProFunDeclarationGroup\n"); break;
    case Function_Reference:            indent(ind); printf("Function_Reference\n"); break;
  }

}

void
indent(int indent)
{
  int i;
  for(i = 0; i < indent; i++) {
    printf("  ");
  }
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
