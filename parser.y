/* the syntax parser */

%{
#include <stdio.h>
#include <ctype.h>
#include "parser.tab.h"
#define YYDEBUG 1
#define YYLINENO
int yylex(void);
%}

/* declare tokens */

%token INTEGER STRING
%token ID
%token LT GT NE RG CE
%token AND begin FORWARD DIV DO ELSE END FOR FUNCTION IF ARRAY MOD
%token PROCEDURE OR OF NOT PROGRAM RECORD THEN TO TYPE VAR WHILE

//%union {
//    int iValue;
//    char sIndex;
//    nodeType *nPtr;
//};


%start Program
%%

Program: PROGRAM ID ';' TypeDefinitions VariableDeclarations

                 SubprogramDeclarations CompoundStatement '.' {printf("Program!\n")};

//TypeDefinitions_X:
                 /* empty */
//                 | TypeDefinitions {printf("TypeDefinitions_X\n");}
//                 ;

/*SubprogramDeclarations_X: */
                        /* empty */
                        /* | SubprogramDeclarations {printf("SubprogramDeclarations_X\n");} */
                        /*; */


TypeDefinitions: 
               /* empty */
               | TYPE TypeDefinition ';' TypeDefinition_X {printf("TypeDefinitions\n");};
               ;

TypeDefinition_X: 
                /* empty */
                | TypeDefinition_X TypeDefinition ';' {printf("TypeDefinition_X\n");}
                ;

//TypeDefinition_E: TypeDefinition ';' {printf("TypeDefinition_E\n");};


VariableDeclarations:
                    /* empty */
                    | VAR VariableDeclaration ';' VariableDeclarations_X {printf("VariableDeclarations\n");};

VariableDeclarations_X:
                     /* empty */
                     | VariableDeclarations_X VariableDeclaration ';'  {printf("VariableDeclaration_X\n");}
                     ;


/* SubprogramDeclarations: SubprogramDeclarations_Y {printf("SubprogramDeclarations\n");}; */

/* SubprogramDeclarations: SubprogramDeclarations_Y_L {printf("SubprogramDeclarations\n");}; */
SubprogramDeclarations:
                      /* empty */
                      | SubprogramDeclarations ProFunDeclarationGroup ';' {printf("SubprogramDeclarations\n");}
                      ;

/*SubprogramDeclarations_E: ProFunDeclarationGroup ';' {printf("SubprogramDeclarations_E\n");}; */

ProFunDeclarationGroup: ProcedureDeclaration {printf("ProFunDeclarationGroup\n");}
                      | FunctionDeclaration {printf("ProFunDeclarationGroup\n");}
                      ;

TypeDefinition: ID '=' Type {printf("TypeDefinition\n");};


VariableDeclaration: IdentifierList ':' Type {printf("VariableDeclaration\n");};

ProcedureDeclaration: PROCEDURE ID '(' FormalParameterList ')' ';' BlockforwardGroup {printf("ProcedureDeclaration\n");};
FunctionDeclaration: FUNCTION ID '(' FormalParameterList ')' ':' ResultType ';' BlockforwardGroup {printf("FunctionDeclaration\n");};

BlockforwardGroup: Block | FORWARD {printf("BlockforwardGroup\n");};


FormalParameterList:
    /* empty */
    | FormalParameterList_E {printf("FormalParameterList\n");}
    ;

FormalParameterList_E: IdentifierList ':' Type IdlistType_X {printf("FormalParameterList_E\n");};

IdlistType_X:
            /* empty */
            | IdlistType_X ';' IdentifierList ':' Type {printf("IdlistType_X_L\n");}
            ;

//IdlistType_E: ';' IdentifierList ':' Type {printf("IdlistType_E\n");};

Block: VariableDeclarations CompoundStatement {printf("Block\n");};

//VariableDeclarations_X:
    /* empty */
//    | VariableDeclarations {printf("VariableDeclarations_X\n");}
//    ;

CompoundStatement: begin StatementSequence END {printf("CompoundStatement\n");};
StatementSequence: Statement StatementSequence_X {printf("StatementSequence\n");};
//StatementSequence_X: StatementSequence_X_L {printf("StatementSequence_X\n");};
StatementSequence_X:
                   /* empty */
                   | StatementSequence_X ';' Statement {printf("StatementSequence_X\n");}
                   ;

//StatementSequence_E: ';' Statement {printf("StatementSequence_E\n");};

Statement: SimpleStatement | StructuredStatement {printf("Statement\n");};


SimpleStatement:
    /* empty */
    | Assignment_Statement {printf("SimpleStatement\n");}
    | ProcedureStatement {printf("Simplestatement\n");}
    ;

//SimpleStatement_E: Assignment_Statement | ProcedureStatement {printf("SimpleStatement_E\n");};

Assignment_Statement: Variable CE Expression {printf("Assignment_Statement\n");};

ProcedureStatement: ID '(' ActualParameterList ')' {printf("ProcedureStatement\n");};

StructuredStatement: MatchedStatement {printf("StructuredStatement\n");}
                   | OpenStatement {printf("StructuredStatement\n");}
                   ;

MatchedStatement: IF Expression THEN MatchedStatement ELSE MatchedStatement {printf("MatchedStatement\n");}
                | CompoundStatement {printf("MatchedStatement\n");}
                | WHILE Expression DO MatchedStatement {printf("MatchedStatement\n");}
                | FOR ID CE Expression TO Expression DO MatchedStatement {printf("MatchedStatement\n");}
                ;

OpenStatement: IF Expression THEN StructuredStatement {printf("OpenStatement\n");}
             | IF Expression THEN MatchedStatement ELSE OpenStatement {printf("OpenStatement\n");}
             | WHILE Expression DO OpenStatement {printf("OpenStatement\n");}
             | FOR ID CE Expression TO Expression DO OpenStatement {printf("OpenStatement\n");}
             ;

Type: ID
    | ARRAY '[' Constant RG Constant ']' OF Type {printf("Type\n");}
    | RECORD Field_List END {printf("Type\n");}
    ;

ResultType: ID {printf("ResultType\n");};
Field_List:
    /* empty */
    | IdentifierList ':' Type IdlistType_X {printf("Field_List\n");}
    ;

//Field_List_E: IdentifierList ':' Type IdlistType_X {printf("Field_List_E\n");};

Constant: INTEGER {printf("Constant\n");}
    | Sign INTEGER {printf("Constant\n");}
    ;
Expression: Simple_Expression {printf("Expression\n");}
    | Simple_Expression Relational_Op Simple_Expression {printf("Expression\n");}
    ;

Relational_Op: '<' | LT | '>' | GT | NE | '=' {printf("Relational_Op\n");}
Simple_Expression: Term AddopTerm_X {printf("Simple_Expression\n");}
    | Sign Term AddopTerm_X {printf("Simple_Expression\n");}
    ;

//AddopTerm_X: AddopTerm_X_L {printf("AddopTerm_X\n");};
AddopTerm_X:
           /* empty */
           | AddopTerm_X AddOp Term {printf("AddopTerm_X_L\n");}
           ;

//AddopTerm_E: AddOp Term {printf("AddopTerm_E\n");};
AddOp: '+' | '-' | OR {printf("AddOp\n");};

Term: Factor MulOpFactor_X {printf("Term\n");};

//MulOpFactor_X: MulOpFactor_X_L {printf("MulOpFactor_X\n");};
MulOpFactor_X:
             /* empty */
             | MulOpFactor_X MulOp Factor {printf("MulOpFactor_X_L\n");}
             ;

//MulOpFactor_E: MulOp Factor {printf("MulOpFactor_E\n");};
MulOp: '*' | DIV | MOD | AND {printf("MulOp\n");};
Factor: INTEGER | STRING | Variable | Function_Reference | NOT Factor | '(' Expression ')' {printf("Factor\n");};
Function_Reference: ID '(' ActualParameterList ')' {printf("Function_Reference\n");};
Variable: ID ComponentSelection {printf("Variable\n");};
ComponentSelection:
                  /* empty */
                  | '.' ID ComponentSelection {printf("ComponentSelectionGroup\n");}
                  | '[' Expression ']' ComponentSelection {printf("ComponentSelectionGroup\n");}
                  ;

ActualParameterList:
                   /* empty */
                   | Expression Expression_X {printf("ActualParameterList\n");}
                   ;
//Expression_X: Expression_X_L {printf("Expression_X\n");};
Expression_X:
            /* empty */
            | Expression_X ',' Expression  {printf("Expression_X_L\n");}
            ;

//Expression_E: ',' Expression {printf("Expression_E\n");};
IdentifierList: ID Id_X {printf("IdentifierList\n");};
//Id_X: Id_X_L {printf("Id_X\n");};
Id_X:
    /* empty */
    | Id_X ',' ID {printf("Id_X\n");}
    ;

//Id_E: ',' ID {printf("Id_E\n");};
Sign: '+' | '-' {printf("Sign\n");};

%%

main(int argc, char **argv)
{
  if (argc <= 1) {
      printf("Usage: parse [source file] [rules.out] [symtable.out]\n");
  }
  //++argv, --argc;
  //if (argc > 0)
  //  yyin = fopen(argv[0], "r");
  //else
  //  yyin = stdin;
  yyparse();
  return 0;
}

yyerror(const char *msg)
{
  printf("error: %s\n", msg);
}
