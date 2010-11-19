/* the syntax parser */

%{
#include <stdio.h>
#include <ctype.h>
int yylex(void);
%}

/* declare tokens */

%token INTEGER STRING
%token ID
%token LT GT NE RG CE
%token AND BEGIN FORWARD DIV DO ELSE END FOR FUNCTION IF ARRAY MOD
%token PROCEDURE OR OF NOT PROGRAM RECORD THEN TO TYPE VAR WHILE

//%union {
//    int iValue;
//    char sIndex;
//    nodeType *nPtr;
//};


%%

Program: PROGRAM ID ';' TypeDefinitions_X VariableDeclarations_X

                 SubprogramDeclarations_X CompoundStatement '.' {};

TypeDefinitions_X:
                 /* empty */
                 | TypeDefinitions {}
                 ;
VariableDeclarations_X:
                      /* empty */
                      | VariableDeclarations {}
                      ;

SubprogramDeclarations_X:
                        /* empty */
                        | SubprogramDeclarations{}
                        ;


TypeDefinitions: TYPE TypeDefinition ';' TypeDefinition_X {};
TypeDefinition_X:
                /* empty */
                | TypeDefinition_X TypeDefinition_E {}
                ;

TypeDefinition_E: TypeDefinition ';' {};


VariableDeclarations: VAR VariableDeclaration ';' VariableDeclaration_X {};
VariableDeclaration_X:
    /* empty */
    | VariableDeclaration_X VariableDeclaration_E {}
    ;

VariableDeclaration_E: VariableDeclaration ';' {};


SubprogramDeclarations: SubprogramDeclarations_X {};
SubprogramDeclarations_X:
    /* empty */
    | SubprogramDeclarations_X SubprogramDeclarations_E {}
    ;

SubprogramDeclarations_E: ProFunDeclarationGroup ';' {};
ProFunDeclarationGroup: ProcedureDeclaration 
    | FunctionDeclaration {}
    ;

TypeDefinition: ID '=' Type {};


VariableDeclaration: IdentifierList ':' Type {};

ProcedureDeclaration: PROCEDURE ID '(' FormalParameterList ')' ';' BlockforwardGroup {};
BlockforwardGroup: Block | FORWARD {};


FunctionDeclaration: FUNCTION ID '(' FormalParameterList ')' ':' ResultType ';' BlockforwardGroup {};
BlockforwardGroup: Block | FORWARD {};


FormalParameterList: FormalParameterList_X {};
FormalParameterList_X:
    /* empty */
    | FormalParameterList_E {}
    ;

FormalParameterList_E: IdentifierList ':' Type IdlistType_X {};
IdlistType_X:
    /* empty */
    | IdlistType_X IdlistType_E {}
    ;

IdlistType_E: ';' IdentifierList ':' Type {};

Block: VariableDeclarations_X CompoundStatement {};
VariableDeclarations_X:
    /* empty */
    | VariableDeclarations {}
    ;

CompoundStatement: BEGIN StatementSequence END {};
StatementSequence: Statement StatementSequence_X {};
StatementSequence_X:
    /* empty */
    | StatementSequence_X StatementSequence_E {}
    ;
StatementSequence_E: ';' Statement {};

Statement: SimpleStatement | StructuredStatement {};


SimpleStatement:
    /* empty */
    | SimpleStatement_E {}
    ;
SimpleStatement_E: Assignment_Statement | ProcedureStatement {};
Assignment_Statement: Variable CE Expression {};

ProcedureStatement: ID '(' ActualParameterList ')' {};

StructuredStatement: MatchedStatement
                   | OpenStatement
                   ;

MatchedStatement: IF Expression THEN MatchedStatement ELSE MatchedStatement {}
                | CompoundStatement {}
                | WHILE Expression DO MatchedStatement {}
                | FOR ID CE Expression TO Expression DO MatchedStatement {}
                ;
OpenStatement: IF Expression THEN Statement
             | IF Expression THEN MatchedStatement ELSE OpenStatement
             | WHILE Expression DO OpenStatement
             | FOR ID CE Expression TO Expression DO OpenStatement
             ;

Type: ID
    | ARRAY '[' Constant RG Constant ']' OF Type {}
    | RECORD Field_List END {}
    ;

ResultType: ID {};
Field_List:
    /* empty */
    | Field_List_E {}
    ;

Field_List_E: IdentifierList ':' Type IdlistType_X {};

Constant: INTEGER {}
    | Sign INTEGER {}
    ;
Expression: Simple_Expression {}
    | Simple_Expression Relational_Op Simple_Expression {}
    ;

Relational_Op: '<' | LT | '>' | GT | NE | '=' {}
Simple_Expression: Term AddopTerm_X {}
    | Sign Term AddopTerm_X {}
    ;

AddopTerm_X:
/* empty */
| AddopTerm_X AddopTerm_E {}
;

AddopTerm_E: AddOp Term {};
AddOp: '+' | '-' | OR {};

Term: Factor MulOpFactor_X {};
MulOpFactor_X:
/* empty */
    | MulOpFactor_X: MulOpFactor_E {}
    ;

MulOpFactor_E: MulOp Factor {};
MulOp: '*' | DIV | MOD | AND {};
Factor: INTEGER | STRING | Variable | Function_Reference | NOT Factor | '(' Expression ')' {};
Function_Reference: ID '(' ActualParameterList ')' {};
Variable: ID  ComponentSelection {};
ComponentSelection:
/* empty */
    | ComponentSelectionGroup {}
    ;
ComponentSelectionGroup: '.' ID ComponentSelection {}
    | '[' Expression ']' ComponentSelection {}
    ;

ActualParameterList:
/* empty */
    | Expression Expression_X {}
    ;
Expression_X:
/* empty */
    | Expression_X Expression_E {}
    ;
Expression_E: ',' Expression {};
IdentifierList: ID Id_X {};
Id_X:
/* empty */
    | Id_X Id_E
    ;

Id_E: ',' ID {};
Sign: '+' | '-' {};

%%

main(int argc, char **argv)
{
  if (argc <= 1) {
      printf("Usage: parse [source file] [rules.out] [symtable.out]\n");
  }
  
  yyparse();
  return 0;
}

yyerror(char *s)
{
  fprintf(stderr, "error: %s\n", s);
}
