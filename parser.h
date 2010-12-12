/* parser.h */
#define NENTRY 100

struct symbol {
  char name[10];
  int scope;
};

typedef enum { typeCon, typeId, typeOpr, typeConstruct } nodeEnum;
typedef enum { Program, TypeDefinitions, TypeDefinition_X, VariableDeclarations,
               VariableDeclarations_X, TypeDefinition, VariableDeclaration, 
               ProcedureDeclaration, FunctionDeclaration, 



} constructEnum;

typedef struct {
  int value;
} conNodeType;

typedef struct {
  int i;
} idNodeType;

typedef struct {
  int oper;
  int nops;
  struct nodeTypeTag *op[1]
} oprNodeType;

typedef struct {
  constructEnum type;
  int nops;
  struct nodeTypeTag *node[1]
} interNodeType;

typedef struct nodeTypeTag {
  nodeEnum type;
  union {
    conNodeType con;
    idNodeType id;
    oprNodeType opr;
    interNodeType node;
  };
} nodeType;

extern FILE *yyin;
extern FILE *yyout;
extern struct symbol symtab[NENTRY];
extern int pointer;
