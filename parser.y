/* the syntax parser */

%{
#include <stdio.h>
#include <ctype.h>
int yylex(void);
%}

/* declare tokens */

%token INTEGER STRING
%token ID
%token AND BEGIN FORWARD DIV DO ELSE END FOR FUNCTION IF ARRAY MOD
%token PROCEDURE OR OF NOT PROGRAM RECORD THEN TO TYPE VAR WHILE 

//%union {
//    int iValue;
//    char sIndex;
//    nodeType *nPtr;
//};


%%

Program : PROGRAM ID ';' [TypeDefinitions] [VariableDeclarations]

                 [SubprogramDeclarations] CompoundStatement .

TypeDefinitions : TYPE TypeDefinition ';' {TypeDefinition ';' }

VariableDeclarations : VAR VariableDeclaration ';' {VariableDeclaration ';' }

SubprogramDeclarations : {(ProcedureDeclaration | FunctionDeclaration) ';' }

TypeDefinition : ID '=' Type

VariableDeclaration : IdentifierList ':' Type

ProcedureDeclaration : PROCEDURE ID '(' FormalParameterList ')' ';' ( Block | FORWARD )

FunctionDeclaration : FUNCTION ID '(' FormalParameterList ')' ':' ResultType ';' ( Block | FORWARD )

FormalParameterList : [IdentifierList : Type { ';' IdentifierList : Type } ]

Block : [VariableDeclarations] CompoundStatement

CompoundStatement : BEGIN StatementSequence END

StatementSequence : Statement { ';' Statement }

Statement : SimpleStatement | StructuredStatement

SimpleStatement : [ (AssignmentStatement | ProcedureStatement) ]

AssignmentStatement : Variable ASN Expression

ProcedureStatement : ID '(' ActualParameterList ')'

StructuredStatement : CompoundStatement

           | IF Expression THEN Statement [ ELSE Statement ]

           | WHILE Expression DO Statement

           | FOR ID ASN Expression TO Expression DO Statement

Type : ID | ARRAY '[' constant RG constant ']' OF Type | RECORD FieldList END

ResultType : ID

Fieldlist : [ IdentifierList : Type { ';' IdentifierList : Type } ]

Constant : [ sign ] INTEGER

Expression : SimpleExpression [ RelationalOp SimpleExpression ]

RelationalOp : '<' | LE | '>' | GT | NE | '='

SimpleExpression : [ sign ] Term { AddOp Term }

AddOp : '+' | '-' | OR

Term : Factor { MulOp Factor }

MulOp : '*' | DIV | MOD | AND

Factor : INTEGER | STRING | Variable | FunctionReference | NOT Factor | '(' Expression ')'

FunctionReference : ID '(' ActualParameterList ')'

Variable : ID ComponentSelection

ComponentSelection : [ ( '.' ID ComponentSelection | '[' expression ']' ComponentSelection ) ]

ActualParameterList : [ Expression { ',' Expression } ]

IdentifierList : ID { ',' ID }

Sign : '+' | '-'




%%

main(int argc, char **argv)
{
  yyparse();
}

yyerror(char *S)
{
  fprintf(stderr, "error: %s\n", s);
}
