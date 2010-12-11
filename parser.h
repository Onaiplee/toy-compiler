/* parser.h */
#define NENTRY 100

struct symbol {
  char name[10];
  int scope;
};

typedef enum { typeCon, typeId, typeOpr } nodeEnum;

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

typedef struct nodeTypeTag {
  nodeEnum type;
  union {
    conNodeType con;
    idNodeType id;
    oprNodeType opr;
  };
} nodeType;

extern FILE *yyin;
extern FILE *yyout;
extern struct symbol symtab[NENTRY];
extern int pointer;
