/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

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




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

