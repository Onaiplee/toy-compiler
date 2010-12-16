/* parser.h */
#define NENTRY 1000
// #define STRING_LIMIT 10

/* typedef struct {
  char *text;
  int length;
} ystring; */

typedef struct {
  char name[20];
} idEntry;

typedef struct {
  char lexeme[20];
  int scope;
} symbolEntry;

typedef enum { typeCon, typeId, typeOpr, typeConstruct } nodeEnum;
typedef enum { Program, TypeDefinitions, TypeDefinition_X, VariableDeclarations,
               VariableDeclarations_X, TypeDefinition, VariableDeclaration, 
               ProcedureDeclaration, FunctionDeclaration, FormalParameterList, 
               IdlistType_X, BlockforwardGroup, Block, CompoundStatement, StatementSequence, 
               StatementSequence_X, Statement, SimpleStatement, Assignment_Statement,
               ProcedureStatement, MatchedStatement, Type, Field_List, Constant, 
               Expression, Simple_Expression, Term, Factor, 
               Variable, ComponentSelection, ActualParameterList, Expression_X, 
               IdentifierList, Id_X, SubprogramDeclarations, ProFunDeclarationGroup,
               Function_Reference, TypeDefinitionSequence, VariableDeclarationSequence
} constructEnum;

typedef struct {
  int value;
} conNodeType;

typedef struct {
  int i;
} idNodeType;

/* typedef struct {
  char text[STRING_LIMIT];
} strNodeType; */

typedef struct {
  int oper;
  int nops;
  struct nodeTypeTag *op[1];
} oprNodeType;

typedef struct {
  constructEnum type;
  int nops;
  struct nodeTypeTag *node[1];
} interNodeType;

typedef struct nodeTypeTag {
  nodeEnum type;
  union {
    conNodeType con;
    idNodeType id;
    /* strNodeType str; */
    oprNodeType opr;
    interNodeType node;
  };
} nodeType;

extern FILE *yyin;
extern FILE *yyout;
extern idEntry idTab[NENTRY];
extern int pointer;
extern int symPointer;
