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
extern FILE *yyin;
extern FILE *yyout;
extern struct symbol symtab[NENTRY];
extern int pointer;
FILE *rule_fp = NULL;
%}

/* declare tokens */

%token INTEGER STRING
%token <iIndex> ID
%token LT GT NE RG CE
%token AND begin FORWARD DIV DO ELSE END FOR FUNCTION IF ARRAY MOD
%token PROCEDURE OR OF NOT PROGRAM RECORD THEN TO TYPE VAR WHILE

%union {
    int iIndex;
};


%start Program
%%

Program: PROGRAM ID ';' TypeDefinitions VariableDeclarations

                 SubprogramDeclarations CompoundStatement '.' {fprintf(rule_fp, "Program\n");};


TypeDefinitions: 
               /* empty */
               | TYPE TypeDefinition ';' TypeDefinition_X {fprintf(rule_fp, "TypeDefinitions\n");};
               ;

TypeDefinition_X: 
                /* empty */
                | TypeDefinition_X TypeDefinition ';' {}
                ;


VariableDeclarations:
                    /* empty */
                    | VAR VariableDeclaration ';' VariableDeclarations_X {fprintf(rule_fp, "VariableDeclarations\n");};

VariableDeclarations_X:
                     /* empty */
                     | VariableDeclarations_X VariableDeclaration ';'  {}
                     ;


SubprogramDeclarations:
                      /* empty */
                      | SubprogramDeclarations ProFunDeclarationGroup ';' {fprintf(rule_fp, "SubprogramDeclarations\n");}
                      ;


ProFunDeclarationGroup: ProcedureDeclaration {}
                      | FunctionDeclaration {}
                      ;

TypeDefinition: ID '=' Type {fprintf(rule_fp, "TypeDefinition\n");};


VariableDeclaration: IdentifierList ':' Type {fprintf(rule_fp, "VariableDeclaration\n");};

ProcedureDeclaration: PROCEDURE ID '(' FormalParameterList ')' ';' BlockforwardGroup {fprintf(rule_fp, "ProcedureDeclaration\n");};
FunctionDeclaration: FUNCTION ID '(' FormalParameterList ')' ':' ResultType ';' BlockforwardGroup {fprintf(rule_fp, "FunctionDeclaration\n");};

BlockforwardGroup: Block | FORWARD {};


FormalParameterList:
                   /* empty */
                   | FormalParameterList_E {fprintf(rule_fp, "FormalParameterList\n");}
                   ;

FormalParameterList_E: IdentifierList ':' Type IdlistType_X {};

IdlistType_X:
            /* empty */
            | IdlistType_X ';' IdentifierList ':' Type {}
            ;


Block: VariableDeclarations CompoundStatement {fprintf(rule_fp, "Block\n");};


CompoundStatement: begin StatementSequence END {fprintf(rule_fp, "CompoundStatement\n");};
StatementSequence: Statement StatementSequence_X {fprintf(rule_fp, "StatementSequence\n");};
StatementSequence_X:
                   /* empty */
                   | StatementSequence_X ';' Statement {}
                   ;


Statement: SimpleStatement {fprintf(rule_fp, "Statement\n");}
         | StructuredStatement {fprintf(rule_fp, "Statement\n");}
         ;



SimpleStatement:
               /* empty */
               | Assignment_Statement {fprintf(rule_fp, "SimpleStatement\n");}
               | ProcedureStatement {fprintf(rule_fp, "Simplestatement\n");}
               ;


Assignment_Statement: Variable CE Expression {fprintf(rule_fp, "Assignment_Statement\n");};

ProcedureStatement: ID '(' ActualParameterList ')' {fprintf(rule_fp, "ProcedureStatement\n");};

StructuredStatement: MatchedStatement {fprintf(rule_fp, "StructuredStatement\n");}
                   | OpenStatement {fprintf(rule_fp, "StructuredStatement\n");}
                   ;

MatchedStatement: IF Expression THEN MatchedStatement ELSE MatchedStatement {fprintf(rule_fp, "MatchedStatement\n");}
                | CompoundStatement {fprintf(rule_fp, "MatchedStatement\n");}
                | WHILE Expression DO MatchedStatement {fprintf(rule_fp, "MatchedStatement\n");}
                | FOR ID CE Expression TO Expression DO MatchedStatement {fprintf(rule_fp, "MatchedStatement\n");}
                ;

OpenStatement: IF Expression THEN StructuredStatement {fprintf(rule_fp, "OpenStatement\n");}
             | IF Expression THEN MatchedStatement ELSE OpenStatement {fprintf(rule_fp, "OpenStatement\n");}
             | WHILE Expression DO OpenStatement {fprintf(rule_fp, "OpenStatement\n");}
             | FOR ID CE Expression TO Expression DO OpenStatement {fprintf(rule_fp, "OpenStatement\n");}
             ;

Type: ID {fprintf(rule_fp, "Type\n");}
    | ARRAY '[' Constant RG Constant ']' OF Type {fprintf(rule_fp, "Type\n");}
    | RECORD Field_List END {fprintf(rule_fp, "Type\n");}
    ;

ResultType: ID {fprintf(rule_fp, "ResultType\n");};
Field_List:
    /* empty */
    | IdentifierList ':' Type IdlistType_X {fprintf(rule_fp, "Field_List\n");}
    ;


Constant: INTEGER {fprintf(rule_fp, "Constant\n");}
    | Sign INTEGER {fprintf(rule_fp, "Constant\n");}
    ;
Expression: Simple_Expression {fprintf(rule_fp, "Expression\n");}
    | Simple_Expression Relational_Op Simple_Expression {fprintf(rule_fp, "Expression\n");}
    ;

Relational_Op: '<' | LT | '>' | GT | NE | '=' {fprintf(rule_fp, "Relational_Op\n");}
Simple_Expression: Term AddopTerm_X {fprintf(rule_fp, "Simple_Expression\n");}
    | Sign Term AddopTerm_X {fprintf(rule_fp, "Simple_Expression\n");}
    ;

AddopTerm_X:
           /* empty */
           | AddopTerm_X AddOp Term {}
           ;

AddOp: '+' | '-' | OR {fprintf(rule_fp, "AddOp\n");};

Term: Factor MulOpFactor_X {fprintf(rule_fp, "Term\n");};

MulOpFactor_X:
             /* empty */
             | MulOpFactor_X MulOp Factor {}
             ;

MulOp: '*' | DIV | MOD | AND {fprintf(rule_fp, "MulOp\n");};
Factor:
        INTEGER {fprintf(rule_fp, "Factor\n");}
      | STRING {fprintf(rule_fp, "Factor\n");}
      | Variable {fprintf(rule_fp, "Factor\n");}
      | Function_Reference {fprintf(rule_fp, "Factor\n");}
      | NOT Factor {fprintf(rule_fp, "Factor\n");}
      | '(' Expression ')' {fprintf(rule_fp, "Factor\n");}
      ;

Function_Reference: ID '(' ActualParameterList ')' {fprintf(rule_fp, "Function_Reference\n");};
Variable: ID ComponentSelection {fprintf(rule_fp, "Variable\n");};
ComponentSelection:
                  /* empty */
                  | '.' ID ComponentSelection {fprintf(rule_fp, "ComponentSelection\n");}
                  | '[' Expression ']' ComponentSelection {fprintf(rule_fp, "ComponentSelection\n");}
                  ;

ActualParameterList:
                   /* empty */
                   | Expression Expression_X {fprintf(rule_fp, "ActualParameterList\n");}
                   ;
Expression_X:
            /* empty */
            | Expression_X ',' Expression  {}
            ;

IdentifierList: ID Id_X {fprintf(rule_fp, "IdentifierList\n");};
Id_X:
    /* empty */
    | Id_X ',' ID {}
    ;

Sign: 
      '+' {fprintf(rule_fp, "Sign\n");}
    | '-' {fprintf(rule_fp, "Sign\n");}
    ;

%%

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
