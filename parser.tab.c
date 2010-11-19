/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     INTEGER = 258,
     STRING = 259,
     ID = 260,
     LT = 261,
     GT = 262,
     NE = 263,
     RG = 264,
     CE = 265,
     AND = 266,
     begin = 267,
     FORWARD = 268,
     DIV = 269,
     DO = 270,
     ELSE = 271,
     END = 272,
     FOR = 273,
     FUNCTION = 274,
     IF = 275,
     ARRAY = 276,
     MOD = 277,
     PROCEDURE = 278,
     OR = 279,
     OF = 280,
     NOT = 281,
     PROGRAM = 282,
     RECORD = 283,
     THEN = 284,
     TO = 285,
     TYPE = 286,
     VAR = 287,
     WHILE = 288
   };
#endif
/* Tokens.  */
#define INTEGER 258
#define STRING 259
#define ID 260
#define LT 261
#define GT 262
#define NE 263
#define RG 264
#define CE 265
#define AND 266
#define begin 267
#define FORWARD 268
#define DIV 269
#define DO 270
#define ELSE 271
#define END 272
#define FOR 273
#define FUNCTION 274
#define IF 275
#define ARRAY 276
#define MOD 277
#define PROCEDURE 278
#define OR 279
#define OF 280
#define NOT 281
#define PROGRAM 282
#define RECORD 283
#define THEN 284
#define TO 285
#define TYPE 286
#define VAR 287
#define WHILE 288




/* Copy the first part of user declarations.  */
#line 3 "parser.y"

#include <stdio.h>
#include <ctype.h>
#include "parser.tab.h"
#define YYDEBUG 1
#define YYLINENO
int yylex(void);


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 181 "parser.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   160

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  48
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  48
/* YYNRULES -- Number of rules.  */
#define YYNRULES  97
/* YYNRULES -- Number of states.  */
#define YYNSTATES  183

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   288

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      38,    39,    46,    44,    47,    45,    35,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    37,    34,
      42,    36,    43,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    40,     2,    41,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,    12,    13,    18,    19,    23,    24,    29,
      30,    34,    35,    39,    41,    43,    47,    51,    59,    69,
      71,    73,    74,    76,    81,    82,    88,    91,    95,    98,
      99,   103,   105,   107,   108,   110,   112,   116,   121,   123,
     125,   132,   134,   139,   148,   153,   160,   165,   174,   176,
     185,   189,   191,   192,   197,   199,   202,   204,   208,   210,
     212,   214,   216,   218,   220,   223,   227,   228,   232,   234,
     236,   238,   241,   242,   246,   248,   250,   252,   254,   256,
     258,   260,   262,   265,   269,   274,   277,   278,   282,   287,
     288,   291,   292,   296,   299,   300,   304,   306
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      49,     0,    -1,    27,     5,    34,    50,    52,    54,    65,
      35,    -1,    -1,    31,    56,    34,    51,    -1,    -1,    51,
      56,    34,    -1,    -1,    32,    57,    34,    53,    -1,    -1,
      53,    57,    34,    -1,    -1,    54,    55,    34,    -1,    58,
      -1,    59,    -1,     5,    36,    75,    -1,    93,    37,    75,
      -1,    23,     5,    38,    61,    39,    34,    60,    -1,    19,
       5,    38,    61,    39,    37,    76,    34,    60,    -1,    64,
      -1,    13,    -1,    -1,    62,    -1,    93,    37,    75,    63,
      -1,    -1,    63,    34,    93,    37,    75,    -1,    52,    65,
      -1,    12,    66,    17,    -1,    68,    67,    -1,    -1,    67,
      34,    68,    -1,    69,    -1,    72,    -1,    -1,    70,    -1,
      71,    -1,    89,    10,    79,    -1,     5,    38,    91,    39,
      -1,    73,    -1,    74,    -1,    20,    79,    29,    73,    16,
      73,    -1,    65,    -1,    33,    79,    15,    73,    -1,    18,
       5,    10,    79,    30,    79,    15,    73,    -1,    20,    79,
      29,    72,    -1,    20,    79,    29,    73,    16,    74,    -1,
      33,    79,    15,    74,    -1,    18,     5,    10,    79,    30,
      79,    15,    74,    -1,     5,    -1,    21,    40,    78,     9,
      78,    41,    25,    75,    -1,    28,    77,    17,    -1,     5,
      -1,    -1,    93,    37,    75,    63,    -1,     3,    -1,    95,
       3,    -1,    81,    -1,    81,    80,    81,    -1,    42,    -1,
       6,    -1,    43,    -1,     7,    -1,     8,    -1,    36,    -1,
      84,    82,    -1,    95,    84,    82,    -1,    -1,    82,    83,
      84,    -1,    44,    -1,    45,    -1,    24,    -1,    87,    85,
      -1,    -1,    85,    86,    87,    -1,    46,    -1,    14,    -1,
      22,    -1,    11,    -1,     3,    -1,     4,    -1,    89,    -1,
      88,    -1,    26,    87,    -1,    38,    79,    39,    -1,     5,
      38,    91,    39,    -1,     5,    90,    -1,    -1,    35,     5,
      90,    -1,    40,    79,    41,    90,    -1,    -1,    79,    92,
      -1,    -1,    92,    47,    79,    -1,     5,    94,    -1,    -1,
      94,    47,     5,    -1,    44,    -1,    45,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    30,    30,    45,    47,    50,    52,    58,    60,    62,
      64,    71,    73,    78,    79,    82,    85,    87,    88,    90,
      90,    93,    95,    98,   100,   102,   107,   114,   115,   117,
     119,   124,   124,   127,   129,   130,   135,   137,   139,   140,
     143,   144,   145,   146,   149,   150,   151,   152,   155,   156,
     157,   160,   161,   163,   168,   169,   171,   172,   175,   175,
     175,   175,   175,   175,   176,   177,   181,   183,   187,   187,
     187,   189,   192,   194,   198,   198,   198,   198,   199,   199,
     199,   199,   199,   199,   200,   201,   202,   204,   205,   208,
     210,   213,   215,   219,   221,   223,   227,   227
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "INTEGER", "STRING", "ID", "LT", "GT",
  "NE", "RG", "CE", "AND", "begin", "FORWARD", "DIV", "DO", "ELSE", "END",
  "FOR", "FUNCTION", "IF", "ARRAY", "MOD", "PROCEDURE", "OR", "OF", "NOT",
  "PROGRAM", "RECORD", "THEN", "TO", "TYPE", "VAR", "WHILE", "';'", "'.'",
  "'='", "':'", "'('", "')'", "'['", "']'", "'<'", "'>'", "'+'", "'-'",
  "'*'", "','", "$accept", "Program", "TypeDefinitions",
  "TypeDefinition_X", "VariableDeclarations", "VariableDeclarations_X",
  "SubprogramDeclarations", "ProFunDeclarationGroup", "TypeDefinition",
  "VariableDeclaration", "ProcedureDeclaration", "FunctionDeclaration",
  "BlockforwardGroup", "FormalParameterList", "FormalParameterList_E",
  "IdlistType_X", "Block", "CompoundStatement", "StatementSequence",
  "StatementSequence_X", "Statement", "SimpleStatement",
  "Assignment_Statement", "ProcedureStatement", "StructuredStatement",
  "MatchedStatement", "OpenStatement", "Type", "ResultType", "Field_List",
  "Constant", "Expression", "Relational_Op", "Simple_Expression",
  "AddopTerm_X", "AddOp", "Term", "MulOpFactor_X", "MulOp", "Factor",
  "Function_Reference", "Variable", "ComponentSelection",
  "ActualParameterList", "Expression_X", "IdentifierList", "Id_X", "Sign", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,    59,    46,    61,    58,    40,    41,
      91,    93,    60,    62,    43,    45,    42,    44
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    48,    49,    50,    50,    51,    51,    52,    52,    53,
      53,    54,    54,    55,    55,    56,    57,    58,    59,    60,
      60,    61,    61,    62,    63,    63,    64,    65,    66,    67,
      67,    68,    68,    69,    69,    69,    70,    71,    72,    72,
      73,    73,    73,    73,    74,    74,    74,    74,    75,    75,
      75,    76,    77,    77,    78,    78,    79,    79,    80,    80,
      80,    80,    80,    80,    81,    81,    82,    82,    83,    83,
      83,    84,    85,    85,    86,    86,    86,    86,    87,    87,
      87,    87,    87,    87,    88,    89,    90,    90,    90,    91,
      91,    92,    92,    93,    94,    94,    95,    95
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     8,     0,     4,     0,     3,     0,     4,     0,
       3,     0,     3,     1,     1,     3,     3,     7,     9,     1,
       1,     0,     1,     4,     0,     5,     2,     3,     2,     0,
       3,     1,     1,     0,     1,     1,     3,     4,     1,     1,
       6,     1,     4,     8,     4,     6,     4,     8,     1,     8,
       3,     1,     0,     4,     1,     2,     1,     3,     1,     1,
       1,     1,     1,     1,     2,     3,     0,     3,     1,     1,
       1,     2,     0,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     3,     4,     2,     0,     3,     4,     0,
       2,     0,     3,     2,     0,     3,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     1,     3,     0,     7,     0,     0,
       0,    11,     0,     5,    94,     0,     0,     0,    48,     0,
      52,    15,     4,    93,     9,     0,    33,     0,     0,     0,
      13,    14,     0,     0,     0,     0,     0,     0,     8,    16,
      86,     0,     0,     0,    41,     0,    29,    31,    34,    35,
      32,    38,    39,     0,     0,     0,    12,     2,    54,    96,
      97,     0,     0,    50,     0,     6,    95,     0,     0,    89,
       0,    85,     0,    78,    79,    86,     0,     0,     0,    56,
      66,    72,    81,    80,     0,     0,    27,    28,     0,    21,
      21,     0,    55,    24,    10,    86,    91,     0,     0,     0,
      89,    82,     0,     0,    59,    61,    62,    63,    58,    60,
       0,    64,    71,    66,     0,    33,    36,     0,    22,     0,
       0,     0,    53,    87,    90,    37,    86,     0,     0,    83,
      44,    38,    57,    70,    68,    69,     0,    77,    75,    76,
      74,     0,    65,    42,    46,    30,     0,     0,     0,     0,
       0,     0,    88,     0,    84,     0,    67,    73,     0,    24,
       7,     0,     0,    92,     0,    40,    45,    51,     0,    23,
      20,     0,    17,    19,    49,     0,     0,     7,    26,    25,
      43,    47,    18
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,     7,    22,   171,    38,    17,    29,     9,    15,
      30,    31,   172,   117,   118,   122,   173,    44,    45,    87,
      46,    47,    48,    49,    50,    51,    52,    21,   168,    34,
      61,    96,   110,    79,   111,   136,    80,   112,   141,    81,
      82,    83,    71,    97,   124,    16,    23,    84
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -108
static const yytype_int8 yypact[] =
{
      -4,    20,    33,     1,  -108,     5,    32,     7,    15,    10,
      57,  -108,    38,  -108,  -108,    42,    55,    58,  -108,    35,
      57,  -108,    32,    50,  -108,    38,    62,    85,    91,    64,
    -108,  -108,    65,    11,    82,    66,    67,    97,    57,  -108,
      49,    99,     8,     8,  -108,    88,  -108,  -108,  -108,  -108,
    -108,  -108,  -108,    96,    69,    70,  -108,  -108,  -108,  -108,
    -108,   100,   107,  -108,    38,  -108,  -108,    77,   108,     8,
       8,  -108,   105,  -108,  -108,    53,    16,     8,    87,    21,
    -108,  -108,  -108,  -108,    16,   102,  -108,    84,     8,    57,
      57,    11,  -108,  -108,  -108,    43,  -108,    80,    79,     8,
       8,  -108,    83,    12,  -108,  -108,  -108,  -108,  -108,  -108,
       8,    41,     4,  -108,    12,    62,  -108,    86,  -108,    89,
      90,    92,    93,  -108,    74,  -108,    43,    94,    95,  -108,
    -108,   112,  -108,  -108,  -108,  -108,    16,  -108,  -108,  -108,
    -108,    16,    41,  -108,  -108,  -108,    98,    38,   104,   106,
      57,     8,  -108,     8,  -108,    12,  -108,  -108,   125,  -108,
       9,    38,   103,  -108,   121,  -108,  -108,  -108,   109,    93,
    -108,   127,  -108,  -108,  -108,    38,    12,     9,  -108,  -108,
    -108,  -108,  -108
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -108,  -108,  -108,  -108,   134,  -108,  -108,  -108,   120,   110,
    -108,  -108,   -33,    56,  -108,   -14,  -108,   -17,  -108,  -108,
      34,  -108,  -108,  -108,    44,   -97,  -107,   -24,  -108,  -108,
      59,   -39,  -108,    45,    39,  -108,   -75,  -108,  -108,   -68,
    -108,   -21,   -79,    60,  -108,   -18,  -108,   -23
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      32,    39,    35,    78,    85,    53,   131,   144,   101,   113,
      62,    73,    74,    75,    58,   137,   123,   143,   138,    73,
      74,    75,   170,     1,    26,     3,   139,   104,   105,   106,
      41,    98,    42,     4,    76,     5,     6,     8,   102,    10,
      93,    10,    76,    18,    13,    43,    77,   152,   166,   116,
     140,    12,    59,    60,    77,    59,    60,   107,   165,    19,
     127,   156,    14,   108,   109,   133,    20,    40,    62,   181,
      26,   119,   119,   157,    26,    33,    24,    27,    68,   180,
      41,    28,    42,    70,    68,   134,   135,    69,    68,    70,
      54,   100,    25,    70,    53,    43,    55,    37,    56,    63,
      57,    65,    66,    64,    72,    86,    88,    89,    90,    91,
      92,    94,   163,    95,   164,    99,   103,   114,   115,   125,
     126,   151,   129,   159,   153,   146,   147,   150,   155,   148,
     167,   161,   162,   149,   154,   158,   176,   174,   160,    26,
     175,    11,    36,   177,   182,   169,   120,   130,    67,   145,
     121,   179,   142,     0,   178,   132,     0,     0,     0,     0,
     128
};

static const yytype_int16 yycheck[] =
{
      17,    25,    20,    42,    43,    26,   103,   114,    76,    84,
      33,     3,     4,     5,     3,    11,    95,   114,    14,     3,
       4,     5,    13,    27,    12,     5,    22,     6,     7,     8,
      18,    70,    20,     0,    26,    34,    31,     5,    77,    32,
      64,    32,    26,     5,    34,    33,    38,   126,   155,    88,
      46,    36,    44,    45,    38,    44,    45,    36,   155,    21,
      99,   136,     5,    42,    43,    24,    28,     5,    91,   176,
      12,    89,    90,   141,    12,    40,    34,    19,    35,   176,
      18,    23,    20,    40,    35,    44,    45,    38,    35,    40,
       5,    38,    37,    40,   115,    33,     5,    47,    34,    17,
      35,    34,     5,    37,     5,    17,    10,    38,    38,     9,
       3,    34,   151,     5,   153,    10,    29,    15,    34,    39,
      41,    47,    39,   147,    30,    39,    37,    34,    16,    39,
       5,    25,   150,    41,    39,    37,    15,   161,    34,    12,
      37,     7,    22,    34,   177,   159,    90,   103,    38,   115,
      91,   175,   113,    -1,   171,   110,    -1,    -1,    -1,    -1,
     100
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    27,    49,     5,     0,    34,    31,    50,     5,    56,
      32,    52,    36,    34,     5,    57,    93,    54,     5,    21,
      28,    75,    51,    94,    34,    37,    12,    19,    23,    55,
      58,    59,    65,    40,    77,    93,    56,    47,    53,    75,
       5,    18,    20,    33,    65,    66,    68,    69,    70,    71,
      72,    73,    74,    89,     5,     5,    34,    35,     3,    44,
      45,    78,    95,    17,    37,    34,     5,    57,    35,    38,
      40,    90,     5,     3,     4,     5,    26,    38,    79,    81,
      84,    87,    88,    89,    95,    79,    17,    67,    10,    38,
      38,     9,     3,    75,    34,     5,    79,    91,    79,    10,
      38,    87,    79,    29,     6,     7,     8,    36,    42,    43,
      80,    82,    85,    84,    15,    34,    79,    61,    62,    93,
      61,    78,    63,    90,    92,    39,    41,    79,    91,    39,
      72,    73,    81,    24,    44,    45,    83,    11,    14,    22,
      46,    86,    82,    73,    74,    68,    39,    37,    39,    41,
      34,    47,    90,    30,    39,    16,    84,    87,    37,    75,
      34,    25,    93,    79,    79,    73,    74,     5,    76,    63,
      13,    52,    60,    64,    75,    37,    15,    34,    65,    75,
      73,    74,    60
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 32 "parser.y"
    {printf("Program!\n");}
    break;

  case 4:
#line 47 "parser.y"
    {printf("TypeDefinitions\n");;}
    break;

  case 6:
#line 52 "parser.y"
    {printf("TypeDefinition_X\n");;}
    break;

  case 8:
#line 60 "parser.y"
    {printf("VariableDeclarations\n");;}
    break;

  case 10:
#line 64 "parser.y"
    {printf("VariableDeclaration_X\n");;}
    break;

  case 12:
#line 73 "parser.y"
    {printf("SubprogramDeclarations\n");;}
    break;

  case 13:
#line 78 "parser.y"
    {printf("ProFunDeclarationGroup\n");;}
    break;

  case 14:
#line 79 "parser.y"
    {printf("ProFunDeclarationGroup\n");;}
    break;

  case 15:
#line 82 "parser.y"
    {printf("TypeDefinition\n");;}
    break;

  case 16:
#line 85 "parser.y"
    {printf("VariableDeclaration\n");;}
    break;

  case 17:
#line 87 "parser.y"
    {printf("ProcedureDeclaration\n");;}
    break;

  case 18:
#line 88 "parser.y"
    {printf("FunctionDeclaration\n");;}
    break;

  case 20:
#line 90 "parser.y"
    {printf("BlockforwardGroup\n");;}
    break;

  case 22:
#line 95 "parser.y"
    {printf("FormalParameterList\n");;}
    break;

  case 23:
#line 98 "parser.y"
    {printf("FormalParameterList_E\n");;}
    break;

  case 25:
#line 102 "parser.y"
    {printf("IdlistType_X_L\n");;}
    break;

  case 26:
#line 107 "parser.y"
    {printf("Block\n");;}
    break;

  case 27:
#line 114 "parser.y"
    {printf("CompoundStatement\n");;}
    break;

  case 28:
#line 115 "parser.y"
    {printf("StatementSequence\n");;}
    break;

  case 30:
#line 119 "parser.y"
    {printf("StatementSequence_X\n");;}
    break;

  case 32:
#line 124 "parser.y"
    {printf("Statement\n");;}
    break;

  case 34:
#line 129 "parser.y"
    {printf("SimpleStatement\n");;}
    break;

  case 35:
#line 130 "parser.y"
    {printf("Simplestatement\n");;}
    break;

  case 36:
#line 135 "parser.y"
    {printf("Assignment_Statement\n");;}
    break;

  case 37:
#line 137 "parser.y"
    {printf("ProcedureStatement\n");;}
    break;

  case 38:
#line 139 "parser.y"
    {printf("StructuredStatement\n");;}
    break;

  case 39:
#line 140 "parser.y"
    {printf("StructuredStatement\n");;}
    break;

  case 40:
#line 143 "parser.y"
    {printf("MatchedStatement\n");;}
    break;

  case 41:
#line 144 "parser.y"
    {printf("MatchedStatement\n");;}
    break;

  case 42:
#line 145 "parser.y"
    {printf("MatchedStatement\n");;}
    break;

  case 43:
#line 146 "parser.y"
    {printf("MatchedStatement\n");;}
    break;

  case 44:
#line 149 "parser.y"
    {printf("OpenStatement\n");;}
    break;

  case 45:
#line 150 "parser.y"
    {printf("OpenStatement\n");;}
    break;

  case 46:
#line 151 "parser.y"
    {printf("OpenStatement\n");;}
    break;

  case 47:
#line 152 "parser.y"
    {printf("OpenStatement\n");;}
    break;

  case 49:
#line 156 "parser.y"
    {printf("Type\n");;}
    break;

  case 50:
#line 157 "parser.y"
    {printf("Type\n");;}
    break;

  case 51:
#line 160 "parser.y"
    {printf("ResultType\n");;}
    break;

  case 53:
#line 163 "parser.y"
    {printf("Field_List\n");;}
    break;

  case 54:
#line 168 "parser.y"
    {printf("Constant\n");;}
    break;

  case 55:
#line 169 "parser.y"
    {printf("Constant\n");;}
    break;

  case 56:
#line 171 "parser.y"
    {printf("Expression\n");;}
    break;

  case 57:
#line 172 "parser.y"
    {printf("Expression\n");;}
    break;

  case 63:
#line 175 "parser.y"
    {printf("Relational_Op\n");;}
    break;

  case 64:
#line 176 "parser.y"
    {printf("Simple_Expression\n");;}
    break;

  case 65:
#line 177 "parser.y"
    {printf("Simple_Expression\n");;}
    break;

  case 67:
#line 183 "parser.y"
    {printf("AddopTerm_X_L\n");;}
    break;

  case 70:
#line 187 "parser.y"
    {printf("AddOp\n");;}
    break;

  case 71:
#line 189 "parser.y"
    {printf("Term\n");;}
    break;

  case 73:
#line 194 "parser.y"
    {printf("MulOpFactor_X_L\n");;}
    break;

  case 77:
#line 198 "parser.y"
    {printf("MulOp\n");;}
    break;

  case 83:
#line 199 "parser.y"
    {printf("Factor\n");;}
    break;

  case 84:
#line 200 "parser.y"
    {printf("Function_Reference\n");;}
    break;

  case 85:
#line 201 "parser.y"
    {printf("Variable\n");;}
    break;

  case 87:
#line 204 "parser.y"
    {printf("ComponentSelectionGroup\n");;}
    break;

  case 88:
#line 205 "parser.y"
    {printf("ComponentSelectionGroup\n");;}
    break;

  case 90:
#line 210 "parser.y"
    {printf("ActualParameterList\n");;}
    break;

  case 92:
#line 215 "parser.y"
    {printf("Expression_X_L\n");;}
    break;

  case 93:
#line 219 "parser.y"
    {printf("IdentifierList\n");;}
    break;

  case 95:
#line 223 "parser.y"
    {printf("Id_X\n");;}
    break;

  case 97:
#line 227 "parser.y"
    {printf("Sign\n");;}
    break;


/* Line 1267 of yacc.c.  */
#line 1848 "parser.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 229 "parser.y"


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

