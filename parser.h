/* parser.h */
#define NENTRY 100
// #define STRING_LIMIT 10

/* typedef struct {
  char *text;
  int length;
} ystring; */

struct symbol {
  char name[10];
  int scope;
};

typedef enum { typeCon, typeId, typeOpr, typeConstruct } nodeEnum;
typedef enum { Program, TypeDefinitions, TypeDefinition_X, VariableDeclarations,
               VariableDeclarations_X, TypeDefinition, VariableDeclaration, 
               ProcedureDeclaration, FunctionDeclaration, FormalParameterList, 
               IdlistType_X, BlockforwardGroup, Block, CompoundStatement, StatementSequence, 
               StatementSequence_X, Statement, SimpleStatement, Assignment_Statement,
               ProceedureStatement, MatchedStatement, Type, Field_List, Constant, 
               Expression, Simple_Expression, Term, Factor, 
               Variable, ComponentSelection, ActualParameterList, Expression_X, 
               IdentifierList, Id_X, SubprogramDeclarations, ProFunDeclarationGroup,
               Function_Reference
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
extern struct symbol symtab[NENTRY];
extern int pointer;
