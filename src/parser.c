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
     EXPR = 258,
     CTXT = 259,
     SCRIPT = 260,
     CASEXP = 261,
     OF = 262,
     DATA = 263,
     TYPE = 264,
     IF = 265,
     THEN = 266,
     ELSE = 267,
     WHERE = 268,
     LET = 269,
     IN = 270,
     INFIXN = 271,
     INFIXL = 272,
     INFIXR = 273,
     PRIMITIVE = 274,
     TNEWTYPE = 275,
     DEFAULT = 276,
     DERIVING = 277,
     DO = 278,
     TCLASS = 279,
     TINSTANCE = 280,
     MDO = 281,
     REPEAT = 282,
     ALL = 283,
     NUMLIT = 284,
     CHARLIT = 285,
     STRINGLIT = 286,
     VAROP = 287,
     VARID = 288,
     CONOP = 289,
     CONID = 290,
     QVAROP = 291,
     QVARID = 292,
     QCONOP = 293,
     QCONID = 294,
     RECSELID = 295,
     IPVARID = 296,
     COCO = 297,
     UPTO = 298,
     FROM = 299,
     ARROW = 300,
     IMPLIES = 301,
     TMODULE = 302,
     IMPORT = 303,
     HIDING = 304,
     QUALIFIED = 305,
     ASMOD = 306,
     NEEDPRIMS = 307,
     FOREIGN = 308
   };
#endif
/* Tokens.  */
#define EXPR 258
#define CTXT 259
#define SCRIPT 260
#define CASEXP 261
#define OF 262
#define DATA 263
#define TYPE 264
#define IF 265
#define THEN 266
#define ELSE 267
#define WHERE 268
#define LET 269
#define IN 270
#define INFIXN 271
#define INFIXL 272
#define INFIXR 273
#define PRIMITIVE 274
#define TNEWTYPE 275
#define DEFAULT 276
#define DERIVING 277
#define DO 278
#define TCLASS 279
#define TINSTANCE 280
#define MDO 281
#define REPEAT 282
#define ALL 283
#define NUMLIT 284
#define CHARLIT 285
#define STRINGLIT 286
#define VAROP 287
#define VARID 288
#define CONOP 289
#define CONID 290
#define QVAROP 291
#define QVARID 292
#define QCONOP 293
#define QCONID 294
#define RECSELID 295
#define IPVARID 296
#define COCO 297
#define UPTO 298
#define FROM 299
#define ARROW 300
#define IMPLIES 301
#define TMODULE 302
#define IMPORT 303
#define HIDING 304
#define QUALIFIED 305
#define ASMOD 306
#define NEEDPRIMS 307
#define FOREIGN 308




/* Copy the first part of user declarations.  */
#line 17 "parser.y"

#ifndef lint
#define lint
#endif
#define defTycon(n,l,lhs,rhs,w)	 tyconDefn(intOf(l),lhs,rhs,w); sp-=n
#define sigdecl(l,vs,t)		 ap(SIGDECL,triple(l,vs,t))
#define fixdecl(l,ops,a,p)	 ap(FIXDECL,\
				    triple(l,ops,mkInt(mkSyntax(a,intOf(p)))))
#define grded(gs)		 ap(GUARDED,gs)
#define bang(t)			 ap(BANG,t)
#define only(t)			 ap(ONLY,t)
#define letrec(bs,e)		 (nonNull(bs) ? ap(LETREC,pair(bs,e)) : e)
#define qualify(ps,t)		 (nonNull(ps) ? ap(QUAL,pair(ps,t)) : t)
#define exportSelf()		 singleton(ap(MODULEENT,mkCon(module(currentModule).text)))
#define yyerror(s)		 /* errors handled elsewhere */
#define YYSTYPE			 Cell

#ifdef YYBISON
# if !defined(__GNUC__) || __GNUC__ <= 1
static void __yy_memcpy Args((char*,char*, unsigned int));
# endif
#endif

#ifdef _MANAGED
static void yymemcpy (char *yyto, const char *yyfrom, size_t yycount);
#endif

static Cell   local gcShadow	 Args((Int,Cell));
static Void   local syntaxError	 Args((String));
static String local unexpected	 Args((Void));
static Cell   local checkPrec	 Args((Cell));
static Cell   local buildTuple	 Args((List));
static List   local checkCtxt	 Args((List));
static Cell   local checkPred	 Args((Cell));
static Pair   local checkDo	 Args((List));
static Cell   local checkTyLhs	 Args((Cell));
static Cell   local checkConstr	 Args((Cell));

#if MUDO
static Pair   local checkMDo	 Args((List));
#endif

#if !TREX
static Void   local noTREX	 Args((String));
#endif
#if !IPARAM
static Void   local noIP	 Args((String));
#endif
#if !MUDO
static Void   local noMDo	 Args((String));
#endif

/* For the purposes of reasonably portable garbage collection, it is
 * necessary to simulate the YACC stack on the Hugs stack to keep
 * track of all intermediate constructs.  The lexical analyser
 * pushes a token onto the stack for each token that is found, with
 * these elements being removed as reduce actions are performed,
 * taking account of look-ahead tokens as described by gcShadow()
 * below.
 *
 * Of the non-terminals used below, only start, topDecl & begin
 * do not leave any values on the Hugs stack.  The same is true for the
 * terminals EXPR and SCRIPT.  At the end of a successful parse, there
 * should only be one element left on the stack, containing the result
 * of the parse.
 */

#define gc0(e)			gcShadow(0,e)
#define gc1(e)			gcShadow(1,e)
#define gc2(e)			gcShadow(2,e)
#define gc3(e)			gcShadow(3,e)
#define gc4(e)			gcShadow(4,e)
#define gc5(e)			gcShadow(5,e)
#define gc6(e)			gcShadow(6,e)
#define gc7(e)			gcShadow(7,e)



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
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
#line 290 "y.tab.c"

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
# if YYENABLE_NLS
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
#define YYFINAL  60
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   4043

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  73
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  165
/* YYNRULES -- Number of rules.  */
#define YYNRULES  501
/* YYNRULES -- Number of states.  */
#define YYNSTATES  891

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   308

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    52,     2,     2,     2,     2,     2,     2,
      53,    56,     2,    72,    55,    48,    61,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    58,
       2,    42,     2,     2,    44,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    57,    46,    59,     2,    71,    60,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    69,    47,    70,    50,     2,     2,     2,
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
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    43,    45,    49,
      51,    54,    62,    63,    64,    65,    66,    67,    68
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     7,    10,    13,    15,    20,    25,    33,
      39,    42,    43,    45,    47,    49,    50,    53,    55,    58,
      63,    64,    67,    71,    75,    80,    84,    86,    88,    90,
      95,   100,   103,   104,   106,   108,   111,   115,   117,   119,
     121,   125,   128,   130,   131,   135,   141,   148,   153,   156,
     157,   162,   166,   167,   169,   171,   174,   178,   180,   182,
     184,   189,   194,   195,   197,   199,   202,   206,   208,   210,
     212,   215,   219,   223,   225,   227,   232,   239,   242,   248,
     256,   259,   264,   267,   273,   281,   284,   287,   290,   293,
     295,   297,   301,   303,   307,   309,   313,   315,   320,   322,
     326,   328,   333,   337,   341,   345,   347,   349,   354,   358,
     360,   364,   367,   370,   374,   377,   381,   384,   386,   388,
     390,   394,   396,   400,   404,   409,   410,   413,   418,   419,
     421,   425,   427,   432,   436,   438,   440,   443,   445,   453,
     460,   469,   477,   485,   490,   494,   499,   502,   505,   508,
     512,   514,   518,   520,   521,   523,   527,   529,   530,   533,
     537,   539,   543,   545,   546,   549,   554,   556,   560,   562,
     566,   570,   574,   576,   581,   583,   587,   593,   596,   598,
     602,   604,   607,   609,   613,   617,   619,   623,   625,   629,
     633,   637,   641,   645,   649,   653,   655,   657,   659,   661,
     665,   669,   673,   675,   677,   679,   682,   684,   687,   689,
     691,   693,   695,   698,   702,   706,   710,   714,   718,   722,
     726,   732,   736,   739,   741,   745,   749,   753,   757,   761,
     765,   769,   771,   775,   779,   782,   786,   789,   793,   796,
     800,   804,   806,   807,   811,   813,   817,   819,   823,   827,
     828,   831,   834,   837,   839,   842,   847,   850,   852,   854,
     856,   860,   864,   868,   872,   876,   881,   886,   891,   894,
     897,   900,   902,   905,   907,   910,   912,   917,   918,   921,
     922,   925,   929,   933,   934,   937,   940,   943,   947,   950,
     952,   954,   956,   960,   962,   966,   968,   970,   972,   974,
     976,   978,   980,   983,   986,   990,   995,   999,  1004,  1008,
    1013,  1017,  1022,  1024,  1026,  1028,  1030,  1033,  1036,  1038,
    1040,  1042,  1046,  1048,  1053,  1055,  1057,  1059,  1063,  1067,
    1071,  1075,  1078,  1082,  1088,  1092,  1096,  1100,  1102,  1103,
    1105,  1109,  1111,  1115,  1117,  1121,  1123,  1127,  1129,  1131,
    1135,  1137,  1139,  1141,  1143,  1145,  1147,  1149,  1154,  1158,
    1161,  1166,  1170,  1175,  1179,  1182,  1187,  1191,  1198,  1203,
    1208,  1210,  1215,  1220,  1225,  1229,  1232,  1236,  1239,  1242,
    1244,  1247,  1249,  1251,  1255,  1258,  1260,  1262,  1264,  1269,
    1274,  1276,  1278,  1280,  1282,  1286,  1290,  1294,  1300,  1302,
    1306,  1311,  1316,  1321,  1325,  1329,  1333,  1335,  1339,  1341,
    1344,  1348,  1351,  1353,  1357,  1359,  1362,  1364,  1367,  1369,
    1374,  1376,  1379,  1383,  1386,  1388,  1392,  1395,  1397,  1398,
    1400,  1404,  1406,  1408,  1412,  1414,  1416,  1419,  1423,  1428,
    1431,  1437,  1441,  1444,  1448,  1450,  1454,  1456,  1459,  1461,
    1464,  1467,  1471,  1474,  1476,  1478,  1480,  1482,  1484,  1486,
    1488,  1490,  1494,  1498,  1502,  1506,  1510,  1512,  1516,  1518,
    1520,  1524,  1526,  1530,  1532,  1534,  1536,  1538,  1540,  1542,
    1544,  1546,  1548,  1552,  1554,  1556,  1558,  1560,  1562,  1566,
    1568,  1570,  1574,  1576,  1580,  1582,  1584,  1586,  1588,  1590,
    1591,  1593
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      74,     0,    -1,     3,   185,   161,    -1,     4,   130,    -1,
       5,    75,    -1,     1,    -1,    76,   236,    79,   237,    -1,
      76,    69,    79,    70,    -1,    62,    77,    80,    13,    69,
      79,   237,    -1,    62,    77,    80,    13,     1,    -1,    62,
       1,    -1,    -1,   221,    -1,   221,    -1,    31,    -1,    -1,
      58,    79,    -1,    96,    -1,    86,    87,    -1,    86,    58,
      87,    96,    -1,    -1,    53,    56,    -1,    53,    55,    56,
      -1,    53,    81,    56,    -1,    53,    81,    55,    56,    -1,
      81,    55,    82,    -1,    82,    -1,   223,    -1,   225,    -1,
     221,    53,    45,    56,    -1,   221,    53,    83,    56,    -1,
      62,    78,    -1,    -1,    55,    -1,    84,    -1,    84,    55,
      -1,    84,    55,    85,    -1,    85,    -1,   223,    -1,   225,
      -1,    86,    58,    88,    -1,    86,    58,    -1,    88,    -1,
      -1,    63,    78,    89,    -1,    63,    78,    66,    78,    89,
      -1,    63,    65,    78,    66,    78,    89,    -1,    63,    65,
      78,    89,    -1,    63,     1,    -1,    -1,    64,    53,    90,
      56,    -1,    53,    90,    56,    -1,    -1,    55,    -1,    91,
      -1,    91,    55,    -1,    91,    55,    92,    -1,    92,    -1,
     222,    -1,    35,    -1,    35,    53,    45,    56,    -1,    35,
      53,    93,    56,    -1,    -1,    55,    -1,    94,    -1,    94,
      55,    -1,    94,    55,    95,    -1,    95,    -1,   222,    -1,
     224,    -1,    96,    58,    -1,    96,    58,    97,    -1,    96,
      58,   152,    -1,    97,    -1,   152,    -1,     9,    98,    42,
     134,    -1,     9,    98,    42,   134,    15,    99,    -1,     9,
       1,    -1,     8,   138,    42,   101,   110,    -1,     8,   130,
      54,    98,    42,   101,   110,    -1,     8,   138,    -1,     8,
     130,    54,    98,    -1,     8,     1,    -1,    20,   138,    42,
     107,   110,    -1,    20,   130,    54,    98,    42,   107,   110,
      -1,    20,     1,    -1,    67,    29,    -1,    67,     1,    -1,
      98,   220,    -1,    35,    -1,     1,    -1,    99,    55,   100,
      -1,   100,    -1,   222,    43,   123,    -1,   222,    -1,   101,
      47,   102,    -1,   102,    -1,    28,   128,    61,   103,    -1,
     104,    -1,   130,    54,   104,    -1,   104,    -1,    52,   136,
     232,   106,    -1,   137,   232,   106,    -1,   138,   232,   106,
      -1,   127,   232,   106,    -1,   138,    -1,   105,    -1,   224,
      69,   108,    70,    -1,   224,    69,    70,    -1,     1,    -1,
     138,    52,   139,    -1,   138,   127,    -1,   105,   139,    -1,
     105,    52,   139,    -1,   105,   127,    -1,    53,    34,    56,
      -1,    52,   136,    -1,   136,    -1,   127,    -1,   102,    -1,
     108,    55,   109,    -1,   109,    -1,   148,    43,   126,    -1,
     148,    43,   134,    -1,   148,    43,    52,   134,    -1,    -1,
      22,   221,    -1,    22,    53,   111,    56,    -1,    -1,   112,
      -1,   112,    55,   221,    -1,   221,    -1,    19,   113,    43,
     123,    -1,   113,    55,   114,    -1,   114,    -1,     1,    -1,
     222,    31,    -1,   222,    -1,    68,    63,   222,    31,   222,
      43,   123,    -1,    68,    63,   222,   222,    43,   123,    -1,
      68,    63,   222,   222,    31,   222,    43,   123,    -1,    68,
      63,   222,   222,   222,    43,   123,    -1,    68,   222,   222,
      31,   222,    43,   123,    -1,    24,   115,   119,   160,    -1,
      25,   116,   160,    -1,    21,    53,   117,    56,    -1,    24,
       1,    -1,    25,     1,    -1,    21,     1,    -1,   130,    54,
     138,    -1,   138,    -1,   130,    54,   138,    -1,   138,    -1,
      -1,   118,    -1,   118,    55,   134,    -1,   134,    -1,    -1,
      47,   120,    -1,   120,    55,   121,    -1,   121,    -1,   122,
      51,   122,    -1,     1,    -1,    -1,   122,   220,    -1,    28,
     128,    61,   124,    -1,   124,    -1,   130,    54,   125,    -1,
     125,    -1,   127,    51,   125,    -1,   137,    51,   125,    -1,
     138,    51,   125,    -1,   136,    -1,    28,   128,    61,   129,
      -1,   127,    -1,    53,   126,    56,    -1,    53,   131,    54,
     134,    56,    -1,   128,   220,    -1,   220,    -1,   130,    54,
     134,    -1,   134,    -1,    53,    56,    -1,   138,    -1,    53,
     138,    56,    -1,    53,   141,    56,    -1,   132,    -1,    53,
     133,    56,    -1,   132,    -1,    53,   133,    56,    -1,   220,
      46,   220,    -1,    41,    43,   134,    -1,   141,    55,   132,
      -1,   133,    55,   138,    -1,   133,    55,   132,    -1,   138,
      55,   132,    -1,   132,    -1,   135,    -1,   138,    -1,   137,
      -1,   127,    51,   134,    -1,   137,    51,   134,    -1,   138,
      51,   134,    -1,     1,    -1,   137,    -1,   138,    -1,   137,
     139,    -1,   140,    -1,   138,   139,    -1,   221,    -1,   140,
      -1,   221,    -1,   220,    -1,    53,    56,    -1,    53,    51,
      56,    -1,    53,   135,    56,    -1,    53,   138,    56,    -1,
      53,   219,    56,    -1,    53,   141,    56,    -1,    53,   142,
      56,    -1,    53,   143,    56,    -1,    53,   143,    47,   134,
      56,    -1,    57,   134,    59,    -1,    57,    59,    -1,    71,
      -1,   141,    55,   138,    -1,   138,    55,   138,    -1,   135,
      55,   134,    -1,   138,    55,   135,    -1,   141,    55,   135,
      -1,   142,    55,   134,    -1,   143,    55,   144,    -1,   144,
      -1,   220,    43,   134,    -1,    16,   146,   147,    -1,    16,
       1,    -1,    17,   146,   147,    -1,    17,     1,    -1,    18,
     146,   147,    -1,    18,     1,    -1,   148,    43,   123,    -1,
     148,    43,     1,    -1,    29,    -1,    -1,   147,    55,   234,
      -1,   234,    -1,   148,    55,   222,    -1,   222,    -1,    69,
     150,   237,    -1,    69,   151,   237,    -1,    -1,   150,    58,
      -1,   151,    58,    -1,   150,   152,    -1,   145,    -1,   153,
     156,    -1,   153,    43,   134,   156,    -1,   169,   156,    -1,
     154,    -1,   155,    -1,   168,    -1,   174,   226,   169,    -1,
     172,   226,   169,    -1,    29,   226,   169,    -1,   222,   228,
     169,    -1,   222,    72,   170,    -1,    53,   154,    56,   176,
      -1,    53,   155,    56,   176,    -1,    53,   168,    56,   176,
      -1,   222,   176,    -1,   155,   176,    -1,   157,   160,    -1,
       1,    -1,    42,   185,    -1,   158,    -1,   158,   159,    -1,
     159,    -1,    47,   187,    42,   185,    -1,    -1,    13,   149,
      -1,    -1,    13,   162,    -1,    69,   163,   237,    -1,    69,
     164,   237,    -1,    -1,   163,    58,    -1,   164,    58,    -1,
     163,   165,    -1,    41,    42,   185,    -1,    41,     1,    -1,
     152,    -1,   168,    -1,   167,    -1,   169,    43,   134,    -1,
     169,    -1,   222,    72,    29,    -1,   222,    -1,    29,    -1,
     171,    -1,   222,    -1,   171,    -1,   174,    -1,   172,    -1,
      48,   173,    -1,    48,     1,    -1,   222,   233,   173,    -1,
     222,   233,    48,   173,    -1,    29,   233,   173,    -1,    29,
     233,    48,   173,    -1,   174,   233,   173,    -1,   174,   233,
      48,   173,    -1,   172,   233,   173,    -1,   172,   233,    48,
     173,    -1,   175,    -1,   176,    -1,   175,    -1,   177,    -1,
     175,   176,    -1,   218,   176,    -1,    29,    -1,   222,    -1,
     177,    -1,   222,    44,   176,    -1,   218,    -1,   225,    69,
     180,    70,    -1,    30,    -1,    31,    -1,    71,    -1,    53,
     167,    56,    -1,    53,   168,    56,    -1,    53,   178,    56,
      -1,    57,   179,    59,    -1,    50,   176,    -1,    53,   183,
      56,    -1,    53,   183,    47,   166,    56,    -1,   178,    55,
     166,    -1,   166,    55,   166,    -1,   179,    55,   166,    -1,
     166,    -1,    -1,   181,    -1,   181,    55,   182,    -1,   182,
      -1,   223,    42,   166,    -1,   222,    -1,   183,    55,   184,
      -1,   184,    -1,   220,    42,   166,    -1,   186,    -1,     1,
      -1,   188,    43,   129,    -1,   187,    -1,   188,    -1,   189,
      -1,   190,    -1,   192,    -1,   191,    -1,   193,    -1,   190,
     235,    48,   192,    -1,   190,   235,   192,    -1,    48,   192,
      -1,   192,   235,    48,   192,    -1,   192,   235,   192,    -1,
     190,   235,    48,   193,    -1,   190,   235,   193,    -1,    48,
     193,    -1,   192,   235,    48,   193,    -1,   192,   235,   193,
      -1,     6,   185,     7,    69,   202,   237,    -1,    23,    69,
     208,   237,    -1,    26,    69,   208,   237,    -1,   197,    -1,
      46,   196,    51,   185,    -1,    14,   162,    15,   185,    -1,
      10,   185,   194,   195,    -1,    58,    11,   185,    -1,    11,
     185,    -1,    58,    12,   185,    -1,    12,   185,    -1,   196,
     176,    -1,   176,    -1,   197,   198,    -1,   198,    -1,   223,
      -1,   223,    44,   198,    -1,    50,   198,    -1,    41,    -1,
      71,    -1,   218,    -1,   225,    69,   211,    70,    -1,   198,
      69,   211,    70,    -1,    29,    -1,    30,    -1,    31,    -1,
      27,    -1,    53,   185,    56,    -1,    53,   199,    56,    -1,
      53,   200,    56,    -1,    53,   200,    47,   185,    56,    -1,
      40,    -1,    57,   214,    59,    -1,    53,   192,   235,    56,
      -1,    53,   231,   187,    56,    -1,    53,   233,   187,    56,
      -1,   199,    55,   185,    -1,   185,    55,   185,    -1,   200,
      55,   201,    -1,   201,    -1,   220,    42,   185,    -1,   203,
      -1,    58,   202,    -1,   203,    58,   204,    -1,   203,    58,
      -1,   204,    -1,   166,   205,   160,    -1,   206,    -1,    51,
     185,    -1,     1,    -1,   206,   207,    -1,   207,    -1,    47,
     187,    51,   185,    -1,   209,    -1,    58,   208,    -1,   209,
      58,   210,    -1,   209,    58,    -1,   210,    -1,   186,    49,
     185,    -1,    14,   162,    -1,   186,    -1,    -1,   212,    -1,
     212,    55,   213,    -1,   213,    -1,   222,    -1,   223,    42,
     185,    -1,   185,    -1,   199,    -1,   185,   215,    -1,   185,
      45,   185,    -1,   185,    55,   185,    45,    -1,   185,    45,
      -1,   185,    55,   185,    45,   185,    -1,   215,    47,   216,
      -1,    47,   216,    -1,   216,    55,   217,    -1,   217,    -1,
     185,    49,   185,    -1,   185,    -1,    14,   162,    -1,   225,
      -1,    53,    56,    -1,    57,    59,    -1,    53,   219,    56,
      -1,   219,    55,    -1,    55,    -1,    33,    -1,    64,    -1,
      65,    -1,    66,    -1,    39,    -1,    35,    -1,   220,    -1,
      53,    32,    56,    -1,    53,    72,    56,    -1,    53,    48,
      56,    -1,    53,    52,    56,    -1,    53,    61,    56,    -1,
      37,    -1,    53,    36,    56,    -1,   222,    -1,    35,    -1,
      53,    34,    56,    -1,    39,    -1,    53,    38,    56,    -1,
     224,    -1,    72,    -1,    48,    -1,   229,    -1,    72,    -1,
     229,    -1,    48,    -1,   229,    -1,    32,    -1,    60,   220,
      60,    -1,    52,    -1,    61,    -1,    48,    -1,   231,    -1,
      36,    -1,    60,    37,    60,    -1,   227,    -1,    34,    -1,
      60,    35,    60,    -1,    38,    -1,    60,    39,    60,    -1,
     232,    -1,   226,    -1,   232,    -1,   230,    -1,   233,    -1,
      -1,    70,    -1,     1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   120,   120,   121,   122,   123,   136,   140,   144,   146,
     148,   154,   157,   159,   160,   168,   169,   170,   171,   172,
     177,   178,   179,   180,   181,   183,   184,   189,   190,   191,
     192,   193,   195,   196,   197,   198,   200,   201,   203,   204,
     209,   210,   211,   213,   224,   226,   229,   232,   235,   237,
     238,   239,   241,   242,   243,   244,   246,   247,   249,   250,
     251,   252,   254,   255,   256,   257,   259,   260,   262,   263,
     268,   269,   270,   271,   272,   277,   278,   281,   282,   285,
     289,   291,   294,   295,   298,   302,   303,   309,   311,   312,
     313,   315,   316,   318,   320,   322,   323,   325,   327,   329,
     330,   332,   333,   334,   335,   336,   337,   338,   339,   340,
     342,   343,   344,   345,   346,   347,   349,   350,   351,   353,
     355,   356,   358,   359,   360,   362,   363,   364,   366,   367,
     369,   370,   375,   377,   378,   379,   381,   382,   387,   389,
     391,   393,   395,   401,   402,   403,   404,   405,   406,   408,
     409,   411,   412,   414,   415,   417,   418,   420,   421,   424,
     425,   427,   428,   430,   431,   436,   438,   440,   441,   443,
     444,   445,   446,   448,   450,   452,   453,   455,   456,   458,
     459,   461,   462,   463,   464,   465,   466,   468,   469,   471,
     478,   486,   487,   488,   489,   490,   493,   494,   496,   497,
     498,   499,   500,   502,   503,   505,   506,   508,   509,   511,
     512,   514,   515,   516,   517,   518,   519,   520,   521,   522,
     529,   536,   537,   538,   541,   542,   544,   545,   546,   547,
     550,   551,   553,   560,   561,   562,   563,   564,   565,   566,
     567,   569,   570,   572,   573,   575,   576,   578,   579,   581,
     582,   583,   585,   587,   588,   589,   592,   594,   595,   596,
     598,   599,   600,   601,   602,   604,   605,   606,   607,   608,
     610,   611,   613,   614,   616,   617,   619,   621,   622,   627,
     628,   631,   632,   635,   636,   637,   640,   642,   649,   650,
     655,   656,   658,   659,   661,   663,   664,   665,   667,   668,
     670,   671,   673,   674,   675,   676,   677,   678,   679,   680,
     681,   682,   684,   685,   687,   688,   690,   691,   693,   694,
     695,   697,   698,   699,   700,   701,   702,   703,   704,   705,
     706,   707,   709,   716,   719,   720,   722,   723,   725,   726,
     728,   729,   731,   732,   735,   736,   738,   750,   751,   753,
     754,   756,   757,   759,   760,   762,   763,   765,   766,   767,
     768,   770,   772,   773,   774,   775,   777,   779,   780,   781,
     788,   790,   793,   794,   799,   800,   802,   803,   806,   807,
     809,   810,   812,   813,   814,   815,   816,   817,   818,   819,
     821,   822,   823,   824,   825,   826,   828,   835,   836,   838,
     839,   840,   841,   843,   844,   847,   848,   850,   859,   860,
     862,   863,   864,   866,   868,   869,   870,   872,   873,   875,
     878,   879,   881,   882,   883,   886,   887,   889,   891,   892,
     894,   895,   897,   898,   903,   904,   905,   921,   922,   923,
     924,   927,   928,   930,   931,   933,   934,   935,   940,   941,
     942,   943,   945,   946,   948,   949,   950,   951,   953,   954,
     956,   957,   958,   959,   960,   961,   963,   964,   965,   967,
     968,   970,   971,   972,   974,   975,   976,   978,   979,   981,
     982,   984,   985,   986,   987,   989,   990,   992,   993,   994,
     997,   998,  1000,  1001,  1002,  1004,  1005,  1007,  1008,  1013,
    1016,  1017
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "EXPR", "CTXT", "SCRIPT", "CASEXP", "OF",
  "DATA", "TYPE", "IF", "THEN", "ELSE", "WHERE", "LET", "IN", "INFIXN",
  "INFIXL", "INFIXR", "PRIMITIVE", "TNEWTYPE", "DEFAULT", "DERIVING", "DO",
  "TCLASS", "TINSTANCE", "MDO", "REPEAT", "ALL", "NUMLIT", "CHARLIT",
  "STRINGLIT", "VAROP", "VARID", "CONOP", "CONID", "QVAROP", "QVARID",
  "QCONOP", "QCONID", "RECSELID", "IPVARID", "'='", "COCO", "'@'", "UPTO",
  "'\\\\'", "'|'", "'-'", "FROM", "'~'", "ARROW", "'!'", "'('", "IMPLIES",
  "','", "')'", "'['", "';'", "']'", "'`'", "'.'", "TMODULE", "IMPORT",
  "HIDING", "QUALIFIED", "ASMOD", "NEEDPRIMS", "FOREIGN", "'{'", "'}'",
  "'_'", "'+'", "$accept", "start", "topModule", "startMain", "modname",
  "modid", "modBody", "expspec", "exports", "export", "qnames", "qnames1",
  "qname", "impDecls", "chase", "impDecl", "impspec", "imports",
  "imports1", "import", "names", "names1", "name", "topDecls", "topDecl",
  "tyLhs", "invars", "invar", "constrs", "pconstr", "qconstr", "constr",
  "btype3", "bbtype", "nconstr", "fieldspecs", "fieldspec", "deriving",
  "derivs0", "derivs", "prims", "prim", "crule", "irule", "dtypes",
  "dtypes1", "fds", "fds1", "fd", "varids0", "topType", "topType0",
  "topType1", "polyType", "bpolyType", "varids", "sigType", "context",
  "lcontext", "lacks", "lacks1", "type", "type1", "btype", "btype1",
  "btype2", "atype", "atype1", "btypes2", "typeTuple", "tfields", "tfield",
  "gendecl", "optDigit", "ops", "vars", "decls", "decls0", "decls1",
  "decl", "funlhs", "funlhs0", "funlhs1", "rhs", "rhs1", "gdrhs", "gddef",
  "wherePart", "lwherePart", "ldecls", "ldecls0", "ldecls1", "ldecl",
  "pat", "pat_npk", "npk", "pat0", "pat0_INT", "pat0_vI", "infixPat",
  "pat10", "pat10_vI", "fpat", "apat", "apat_vI", "pats2", "pats1",
  "patbinds", "patbinds1", "patbind", "patfields", "patfield", "exp",
  "exp_err", "exp0", "exp0a", "exp0b", "infixExpa", "infixExpb", "exp10a",
  "exp10b", "then_exp", "else_exp", "pats", "appExp", "aexp", "exps2",
  "vfields", "vfield", "alts", "alts1", "alt", "altRhs", "guardAlts",
  "guardAlt", "stmts", "stmts1", "stmt", "fbinds", "fbinds1", "fbind",
  "list", "zipquals", "quals", "qual", "gcon", "tupCommas", "varid",
  "qconid", "var", "qvar", "con", "qcon", "varop", "varop_mi", "varop_pl",
  "varop_mipl", "qvarop", "qvarop_mi", "conop", "qconop", "op", "qop",
  "begin", "end", 0
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
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,    61,   297,    64,   298,    92,   124,    45,   299,
     126,   300,    33,    40,   301,    44,    41,    91,    59,    93,
      96,    46,   302,   303,   304,   305,   306,   307,   308,   123,
     125,    95,    43
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    73,    74,    74,    74,    74,    75,    75,    75,    75,
      75,    76,    77,    78,    78,    79,    79,    79,    79,    79,
      80,    80,    80,    80,    80,    81,    81,    82,    82,    82,
      82,    82,    83,    83,    83,    83,    84,    84,    85,    85,
      86,    86,    86,    87,    88,    88,    88,    88,    88,    89,
      89,    89,    90,    90,    90,    90,    91,    91,    92,    92,
      92,    92,    93,    93,    93,    93,    94,    94,    95,    95,
      96,    96,    96,    96,    96,    97,    97,    97,    97,    97,
      97,    97,    97,    97,    97,    97,    97,    97,    98,    98,
      98,    99,    99,   100,   100,   101,   101,   102,   102,   103,
     103,   104,   104,   104,   104,   104,   104,   104,   104,   104,
     105,   105,   105,   105,   105,   105,   106,   106,   106,   107,
     108,   108,   109,   109,   109,   110,   110,   110,   111,   111,
     112,   112,    97,   113,   113,   113,   114,   114,    97,    97,
      97,    97,    97,    97,    97,    97,    97,    97,    97,   115,
     115,   116,   116,   117,   117,   118,   118,   119,   119,   120,
     120,   121,   121,   122,   122,   123,   123,   124,   124,   125,
     125,   125,   125,   126,   126,   127,   127,   128,   128,   129,
     129,   130,   130,   130,   130,   130,   130,   131,   131,   132,
     132,   133,   133,   133,   133,   133,   134,   134,   135,   135,
     135,   135,   135,   136,   136,   137,   137,   138,   138,   139,
     139,   140,   140,   140,   140,   140,   140,   140,   140,   140,
     140,   140,   140,   140,   141,   141,   142,   142,   142,   142,
     143,   143,   144,   145,   145,   145,   145,   145,   145,   145,
     145,   146,   146,   147,   147,   148,   148,   149,   149,   150,
     150,   150,   151,   152,   152,   152,   152,   153,   153,   153,
     154,   154,   154,   154,   154,   155,   155,   155,   155,   155,
     156,   156,   157,   157,   158,   158,   159,   160,   160,   161,
     161,   162,   162,   163,   163,   163,   164,   165,   165,   165,
     166,   166,   167,   167,   168,   169,   169,   169,   170,   170,
     171,   171,   172,   172,   172,   172,   172,   172,   172,   172,
     172,   172,   173,   173,   174,   174,   175,   175,   176,   176,
     176,   177,   177,   177,   177,   177,   177,   177,   177,   177,
     177,   177,   177,   177,   178,   178,   179,   179,   180,   180,
     181,   181,   182,   182,   183,   183,   184,   185,   185,   186,
     186,   187,   187,   188,   188,   189,   189,   190,   190,   190,
     190,   190,   191,   191,   191,   191,   191,   192,   192,   192,
     192,   193,   193,   193,   194,   194,   195,   195,   196,   196,
     197,   197,   198,   198,   198,   198,   198,   198,   198,   198,
     198,   198,   198,   198,   198,   198,   198,   198,   198,   198,
     198,   198,   198,   199,   199,   200,   200,   201,   202,   202,
     203,   203,   203,   204,   205,   205,   205,   206,   206,   207,
     208,   208,   209,   209,   209,   210,   210,   210,   211,   211,
     212,   212,   213,   213,   214,   214,   214,   214,   214,   214,
     214,   215,   215,   216,   216,   217,   217,   217,   218,   218,
     218,   218,   219,   219,   220,   220,   220,   220,   221,   221,
     222,   222,   222,   222,   222,   222,   223,   223,   223,   224,
     224,   225,   225,   225,   226,   226,   226,   227,   227,   228,
     228,   229,   229,   229,   229,   230,   230,   231,   231,   231,
     232,   232,   233,   233,   233,   234,   234,   235,   235,   236,
     237,   237
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     3,     2,     2,     1,     4,     4,     7,     5,
       2,     0,     1,     1,     1,     0,     2,     1,     2,     4,
       0,     2,     3,     3,     4,     3,     1,     1,     1,     4,
       4,     2,     0,     1,     1,     2,     3,     1,     1,     1,
       3,     2,     1,     0,     3,     5,     6,     4,     2,     0,
       4,     3,     0,     1,     1,     2,     3,     1,     1,     1,
       4,     4,     0,     1,     1,     2,     3,     1,     1,     1,
       2,     3,     3,     1,     1,     4,     6,     2,     5,     7,
       2,     4,     2,     5,     7,     2,     2,     2,     2,     1,
       1,     3,     1,     3,     1,     3,     1,     4,     1,     3,
       1,     4,     3,     3,     3,     1,     1,     4,     3,     1,
       3,     2,     2,     3,     2,     3,     2,     1,     1,     1,
       3,     1,     3,     3,     4,     0,     2,     4,     0,     1,
       3,     1,     4,     3,     1,     1,     2,     1,     7,     6,
       8,     7,     7,     4,     3,     4,     2,     2,     2,     3,
       1,     3,     1,     0,     1,     3,     1,     0,     2,     3,
       1,     3,     1,     0,     2,     4,     1,     3,     1,     3,
       3,     3,     1,     4,     1,     3,     5,     2,     1,     3,
       1,     2,     1,     3,     3,     1,     3,     1,     3,     3,
       3,     3,     3,     3,     3,     1,     1,     1,     1,     3,
       3,     3,     1,     1,     1,     2,     1,     2,     1,     1,
       1,     1,     2,     3,     3,     3,     3,     3,     3,     3,
       5,     3,     2,     1,     3,     3,     3,     3,     3,     3,
       3,     1,     3,     3,     2,     3,     2,     3,     2,     3,
       3,     1,     0,     3,     1,     3,     1,     3,     3,     0,
       2,     2,     2,     1,     2,     4,     2,     1,     1,     1,
       3,     3,     3,     3,     3,     4,     4,     4,     2,     2,
       2,     1,     2,     1,     2,     1,     4,     0,     2,     0,
       2,     3,     3,     0,     2,     2,     2,     3,     2,     1,
       1,     1,     3,     1,     3,     1,     1,     1,     1,     1,
       1,     1,     2,     2,     3,     4,     3,     4,     3,     4,
       3,     4,     1,     1,     1,     1,     2,     2,     1,     1,
       1,     3,     1,     4,     1,     1,     1,     3,     3,     3,
       3,     2,     3,     5,     3,     3,     3,     1,     0,     1,
       3,     1,     3,     1,     3,     1,     3,     1,     1,     3,
       1,     1,     1,     1,     1,     1,     1,     4,     3,     2,
       4,     3,     4,     3,     2,     4,     3,     6,     4,     4,
       1,     4,     4,     4,     3,     2,     3,     2,     2,     1,
       2,     1,     1,     3,     2,     1,     1,     1,     4,     4,
       1,     1,     1,     1,     3,     3,     3,     5,     1,     3,
       4,     4,     4,     3,     3,     3,     1,     3,     1,     2,
       3,     2,     1,     3,     1,     2,     1,     2,     1,     4,
       1,     2,     3,     2,     1,     3,     2,     1,     0,     1,
       3,     1,     1,     3,     1,     1,     2,     3,     4,     2,
       5,     3,     2,     3,     1,     3,     1,     2,     1,     2,
       2,     3,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     3,     3,     3,     3,     1,     3,     1,     1,
       3,     1,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     1,     1,     1,     1,     1,     3,     1,
       1,     3,     1,     3,     1,     1,     1,     1,     1,     0,
       1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       0,     5,     0,     0,    11,     0,   348,     0,     0,     0,
       0,     0,   393,   390,   391,   392,   454,   469,   466,   471,
     398,   385,     0,     0,     0,     0,     0,   455,   456,   457,
     386,   279,   347,   350,   351,   352,   353,   355,   354,   356,
     370,   381,   387,   460,   468,   382,   473,   448,   459,   458,
       0,     0,     3,   185,   182,     0,   208,     0,     4,   499,
       1,     0,     0,   283,     0,     0,     0,   318,   324,   325,
       0,     0,     0,   326,   379,   320,     0,   322,   319,   448,
     359,   364,   384,   481,   490,   487,   492,     0,   483,   453,
     449,     0,   484,   477,     0,   354,     0,     0,   406,     0,
     460,   489,   478,     0,   494,     0,   450,   434,   435,     0,
       0,     2,     0,   481,   490,   487,   492,   485,   483,   484,
     477,   497,   486,   498,     0,     0,   380,   428,     0,   428,
       0,   181,   195,     0,     0,     0,     0,     0,   223,   207,
     209,   211,   210,     0,    10,    20,    12,    15,    15,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   427,     0,
     420,   424,     0,   331,   296,     0,     0,     0,     0,     0,
       0,     0,     0,   291,   290,   293,   297,   301,   300,   314,
     315,     0,     0,   345,   322,   460,   295,     0,   337,   291,
     290,     0,     0,   378,     0,   338,   461,   470,   467,   472,
     463,   464,     0,     0,     0,     0,   465,   462,     0,   394,
       0,     0,   395,     0,     0,   396,   452,   451,     0,     0,
     351,     0,     0,     0,     0,   436,   399,   280,   202,     0,
       0,   349,     0,   180,   196,   198,   197,   206,   211,     0,
     358,   363,     0,   361,   366,     0,     0,   429,   431,   432,
       0,   383,     0,     0,   190,   197,     0,   186,     0,   183,
       0,   184,     0,   212,     0,     0,     0,     0,     0,   231,
       0,   211,   222,     0,   189,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   296,     0,    15,
       0,     0,     0,     0,    43,    42,    17,    73,   253,     0,
      74,     0,   257,   258,   259,     0,   301,   300,   295,     0,
       0,   375,     0,     0,     0,   373,   501,     0,   284,   500,
     289,   286,   281,   285,   282,   372,   426,   421,     0,   368,
     423,   369,     0,     0,   303,   302,   312,   313,     0,   327,
     328,     0,     0,     0,   316,     0,   329,     0,     0,   332,
     317,     0,     0,     0,     0,   330,   371,   321,     0,   339,
     341,   343,     0,   491,   488,   493,   482,   404,   400,   403,
       0,   405,     0,   407,   401,   402,   437,     0,   446,   442,
     444,   404,     0,     0,     0,   212,     0,   174,     0,   195,
       0,     0,   211,     0,     0,     0,   205,     0,   357,   362,
     360,   365,     0,     0,   389,     0,     0,   388,   187,   193,
     192,   194,   225,   191,   224,   213,     0,   214,     0,   215,
       0,   217,     0,   218,     0,     0,   219,   216,     0,   221,
     459,   471,     0,     0,    21,     0,     0,    26,     0,    27,
      28,     0,    82,     0,    80,    90,    89,     0,   234,   241,
       0,   236,     0,   238,     0,   135,     0,     0,   134,   137,
      85,     0,   182,   148,     0,   146,   157,     0,   150,   147,
     277,     0,   152,   475,     0,   474,     0,   476,     0,     0,
     290,   295,    16,    48,    14,     0,    49,    13,    87,    86,
       0,     0,     7,    43,    18,    70,     0,     0,   271,     0,
       0,     0,   254,   277,   273,   275,   269,   256,     0,     0,
     479,     0,   268,     0,   480,     6,     0,     0,     0,   408,
     412,   374,   377,     0,   288,     0,   425,   422,     0,   306,
     335,   292,     0,   310,     0,   308,   334,     0,   344,     0,
     346,   294,     0,   304,   336,   323,     0,     0,   397,   447,
       0,     0,     0,   441,     0,   178,     0,     0,     0,   175,
       0,     0,   215,     0,   217,   199,   179,   200,   201,   430,
     433,   226,   227,   225,   228,   224,   229,     0,   230,     0,
     232,    22,    31,     0,    23,    32,     9,    15,     0,     0,
       0,    88,     0,   233,   495,   496,   244,   235,   237,     0,
       0,   136,     0,     0,     0,   154,   156,     0,   277,     0,
       0,   144,     0,   262,   295,     0,     0,   328,    49,    52,
       0,     0,    44,     0,     0,     0,    40,    71,    72,   240,
       0,   239,   166,   168,     0,     0,   172,   203,   204,   245,
     272,     0,     0,   270,   274,   261,   260,   294,   264,   299,
     298,   263,   409,   416,     0,     0,   277,   414,   418,   367,
     411,   376,   287,   307,   311,   309,   333,   305,   340,   342,
     445,   443,   440,     0,   177,   188,     0,   220,    24,    25,
       0,    33,     0,    34,    37,    38,    39,     0,    90,    81,
     109,     0,     0,     0,   125,    96,    98,   106,     0,     0,
     105,     0,    75,     0,   132,   133,     0,   119,   125,   145,
       0,   162,   158,   160,     0,   143,   149,   249,   278,   151,
     265,   266,   267,     0,    47,    59,    53,     0,    54,    57,
      58,    52,    49,     0,     0,     0,    19,     0,     0,     0,
       0,     0,   255,     0,     0,   415,   413,   417,   410,   173,
     176,    29,    30,    35,     8,     0,     0,     0,   203,   204,
       0,     0,     0,    78,     0,   114,   112,     0,     0,     0,
       0,   111,     0,     0,     0,   243,     0,    83,   155,     0,
     163,   164,     0,     0,    49,    62,    51,    55,     0,    45,
       0,     0,     0,     0,     0,     0,   169,   204,   167,   170,
     171,   276,     0,    36,   125,     0,     0,   115,   128,   126,
      95,   113,     0,   104,   118,   117,   102,   110,   103,   108,
       0,   121,     0,   246,    76,    92,    94,   125,   159,   161,
     250,   252,   247,   251,   248,    46,     0,     0,    63,     0,
      64,    67,    68,    69,    56,    50,     0,     0,   139,     0,
       0,   165,   419,    79,     0,    97,   100,     0,   105,   101,
       0,   129,   131,   116,     0,   107,     0,     0,     0,    84,
      60,    61,    65,   138,     0,   141,   142,     0,   127,     0,
     120,     0,   122,   123,    91,    93,    66,   140,    99,   130,
     124
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     5,    58,    59,   145,   486,   293,   276,   436,   437,
     682,   683,   684,   294,   494,   295,   622,   727,   728,   729,
     839,   840,   841,   296,   297,   447,   824,   825,   694,   695,
     855,   696,   697,   813,   708,   820,   821,   763,   860,   861,
     457,   458,   466,   470,   604,   605,   608,   712,   713,   714,
     631,   632,   633,   386,   230,   554,   231,   635,   388,    53,
     133,   233,   234,   636,   235,   255,   139,   237,   266,   267,
     268,   269,   298,   450,   593,   299,   718,   782,   783,   300,
     301,   302,   303,   502,   503,   504,   505,   611,   111,    64,
     153,   154,   321,   517,   189,   190,   175,   648,   176,   177,
     335,   178,   179,   337,   180,   181,   191,   358,   359,   360,
     182,   183,   378,    32,    33,    34,    35,    36,    37,    38,
      39,   152,   315,    76,    40,    41,    96,    97,    98,   518,
     519,   520,   656,   657,   658,   159,   160,   161,   246,   247,
     248,   109,   225,   379,   380,    42,   270,    43,    56,    44,
      45,    46,    79,   594,   101,   513,   477,   121,   122,   104,
     353,   596,   124,   148,   322
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -763
static const yytype_int16 yypact[] =
{
     497,  -763,  2326,   585,    34,   102,  -763,  2326,  2326,   105,
     115,   133,  -763,  -763,  -763,  -763,  -763,  -763,  -763,  -763,
    -763,  -763,  3657,  2885,  3048,  1109,  1971,  -763,  -763,  -763,
    -763,   158,  -763,  -763,   178,  -763,   780,  -763,   780,  -763,
    3048,   140,  -763,  -763,  -763,   193,  -763,   171,  -763,  -763,
     208,  1392,  -763,  -763,  3916,   225,  -763,    58,  -763,   221,
    -763,   302,    67,  -763,   241,  2557,  2557,  -763,  -763,  -763,
    3657,  3135,  3267,  -763,  -763,  -763,  3345,  -763,   270,   248,
    -763,  -763,   140,   273,   276,   283,   297,  2652,   321,  -763,
    -763,  1574,   349,   362,   312,   780,   329,   419,  -763,   380,
     307,  -763,  -763,  2701,  -763,  2701,  -763,   379,   303,   361,
     105,  -763,  1746,  -763,  -763,  -763,  -763,  -763,  -763,  -763,
    -763,  -763,  -763,  -763,  2747,  2793,   140,   333,  3048,   333,
    2511,  -763,  -763,   472,  3741,   483,  1381,  1033,  -763,  -763,
    -763,  -763,  -763,   565,  -763,   377,  -763,  2944,  2944,   385,
    2326,   445,    87,  1320,    62,  2326,   105,  2557,   413,    25,
     409,  -763,    25,  -763,   138,   273,   276,   297,  1905,   321,
     349,   362,   422,   429,   438,   479,  -763,   138,   138,  3657,
    -763,   491,   434,  -763,  3657,   487,   284,  1594,  -763,  -763,
    -763,   134,  2326,  -763,  3657,   333,  -763,  -763,  -763,  -763,
    -763,  -763,   465,   474,   482,   488,  -763,  -763,  2326,  -763,
    2603,  2326,  -763,  2326,   565,  -763,  -763,  -763,  2326,   494,
    -763,   496,  2234,  2372,  2326,   506,  -763,  -763,  -763,  2133,
     505,  -763,   509,  -763,  -763,  1628,  1144,  -763,   225,  2885,
    -763,  -763,  2885,  -763,  -763,   116,   495,   504,  -763,   536,
     542,   140,   518,  2393,  -763,  2088,  1300,  -763,  1300,  -763,
    1300,  -763,   538,  -763,   519,  3660,   546,   556,   489,  -763,
     602,   549,  -763,   540,  -763,  3953,   584,   502,    69,   561,
     672,   759,   454,  1005,    53,  1278,  1789,   906,  3179,  2944,
     204,   166,   920,   534,   548,  -763,   557,  -763,  -763,   149,
    -763,    65,  -763,  3657,  -763,    99,   906,   906,  3091,    25,
    3306,  -763,  2326,  2326,   605,  -763,  -763,    68,  -763,  -763,
    -763,  -763,  -763,  -763,  -763,  -763,   241,  -763,  2326,  -763,
    2839,  -763,   188,  3384,  -763,  -763,  3657,  -763,  3423,  -763,
    -763,  2511,  3462,  3501,  -763,  3423,  -763,  3423,   565,  -763,
    -763,  3423,   590,  3540,  3423,  -763,  -763,  -763,   553,   570,
    -763,   536,   594,  -763,  -763,  -763,  -763,  -763,  -763,  -763,
     589,  -763,   307,  -763,  -763,  -763,  -763,   105,   604,   600,
    -763,   642,  2372,   565,  2393,   637,   636,   505,   644,   645,
    3694,   606,    36,  2511,  2511,  2511,  -763,  2511,  -763,  -763,
    -763,  -763,   283,   646,  -763,   333,  2326,  -763,  -763,  -763,
    3916,  -763,  3916,  -763,  3916,  -763,  2511,  -763,  2511,  -763,
    2511,  -763,  2511,  -763,  2511,   565,  -763,  -763,  2511,  -763,
     203,   647,   899,   649,  -763,   411,   616,  -763,   655,  -763,
    -763,    57,  -763,   656,  3760,    84,  -763,   418,  -763,  -763,
     909,  -763,   909,  -763,   909,  -763,   311,   250,  -763,   688,
    -763,   658,  3820,  -763,  2453,  -763,   674,   668,  3839,  -763,
     712,   689,  3839,  -763,  1389,  -763,  3423,  -763,   693,  3579,
     695,  3223,  -763,  -763,  -763,   411,   249,  -763,  -763,  -763,
     676,   676,  -763,    60,  -763,  3005,   961,   676,  -763,  2326,
    2511,  2701,  -763,   712,   711,  -763,  -763,  -763,  3423,  3423,
    -763,  3618,  -763,  3423,  -763,  -763,  3306,   100,    25,   699,
    -763,  -763,  -763,  2326,  -763,  2326,  -763,  -763,  3657,  -763,
    -763,  -763,  3657,  -763,  3657,  -763,  -763,   706,  -763,   487,
    -763,  -763,  3657,  -763,  -763,  -763,   333,  3423,  -763,   241,
    2326,  2372,  2280,   600,   516,  -763,   626,  3713,   629,  -763,
    2511,  2472,   714,  2472,   716,  -763,  -763,  -763,  -763,  -763,
    -763,  -763,  -763,  2088,  -763,  2088,  -763,   707,  -763,   549,
    -763,  -763,  -763,  3977,  -763,  2181,  -763,  2944,   164,  2017,
    2511,  -763,   336,   717,  -763,  -763,  -763,   717,   717,   575,
     676,  -763,   164,  2017,   715,   719,  -763,   363,   712,   226,
     708,  -763,   226,  -763,    43,  3657,  3657,  3657,   340,  1636,
     722,   411,  -763,   662,   747,  3005,  -763,  -763,  -763,  -763,
     565,  -763,  -763,  -763,   731,   729,  -763,  3855,  3776,  -763,
    -763,    99,   742,  -763,  -763,  -763,  -763,   138,  -763,  -763,
      43,  -763,  -763,  -763,  2701,  2326,   712,   740,  -763,  -763,
    3423,  -763,  -763,  -763,  -763,  -763,  -763,  -763,  -763,  -763,
    -763,  -763,  -763,  1746,  -763,  -763,   739,  -763,  -763,  -763,
     741,  -763,   744,   743,  -763,  -763,  -763,    25,  -763,   681,
    -763,   565,  3916,  1677,   160,  -763,  -763,  1475,    30,  3804,
    1323,   732,   793,   909,  -763,  -763,   833,  -763,   788,  -763,
    2511,  -763,   758,  -763,   614,  -763,  3916,  -763,  -763,  3916,
    -763,  -763,  -763,   411,  -763,   762,  -763,   769,   772,  -763,
    -763,  1636,   293,   676,   611,   676,   557,   784,  3932,  3932,
    3932,  3932,  -763,  2326,   778,  -763,  -763,  -763,  -763,  -763,
    -763,  -763,  -763,  1483,  -763,  2017,   943,    30,  3916,  3916,
     774,   210,  2017,  -763,  3916,  -763,  -763,   798,  3876,  3876,
    3916,  -763,  3876,   211,   676,  -763,  2017,  -763,  -763,   363,
    -763,  -763,   852,    91,   293,  1998,  -763,  1043,   779,  -763,
     791,   676,   575,   794,   796,  3897,  -763,  3913,  -763,  -763,
    -763,  -763,  2326,  -763,   160,  1460,  3876,   773,   226,  -763,
    -763,  -763,  3916,  -763,  -763,  -763,  -763,  -763,  -763,  -763,
      21,  -763,   257,  -763,   799,  -763,   815,   788,  -763,   565,
    -763,  -763,  -763,  -763,  -763,  -763,   803,   444,  -763,   805,
     809,  -763,  -763,  -763,  -763,  -763,   575,   824,  -763,   575,
     575,  -763,  -763,  -763,  1844,  -763,  -763,   818,  1205,  -763,
     817,   821,  -763,  -763,   676,  -763,  2432,   676,   575,  -763,
    -763,  -763,  1181,  -763,   575,  -763,  -763,  2469,  -763,   226,
    -763,  2511,  -763,  -763,  -763,  -763,  -763,  -763,  -763,  -763,
    -763
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -763,  -763,  -763,  -763,  -763,  -420,  -139,  -763,  -763,   301,
    -763,  -763,   136,  -763,   393,   397,  -597,   163,  -763,   114,
    -763,  -763,    35,   281,   416,  -516,  -763,    41,   157,  -547,
    -763,  -762,  -763,  -552,   137,  -763,    50,  -688,  -763,  -763,
    -763,   315,  -763,  -763,  -763,  -763,  -763,  -763,   141,   139,
    -558,   126,    83,    63,   983,  -570,   251,     1,  -763,   -50,
     550,  -127,  -124,  -612,  1375,   187,  -185,   710,   -49,  -763,
    -763,   507,  -763,   229,   -44,  -728,  -763,  -763,  -763,  -145,
    -763,   648,   651,  -294,  -763,  -763,   426,  -465,  -763,  -104,
    -763,  -763,  -763,   170,   -66,   -64,  -134,  -763,   437,  -130,
    -180,  -119,    -2,    52,  1614,  -763,  -763,  -763,  -763,   404,
    -763,   620,  1080,   -19,   -78,   -63,  -763,  -763,  -763,    94,
      88,  -763,  -763,  -763,  -763,     9,   926,  -763,   750,   458,
    -763,   317,  -763,  -763,   323,   -13,  -763,   657,   859,  -763,
     598,  -763,  -763,   608,   459,  1578,    -9,    -3,   838,   709,
     -97,  -373,  1464,   -59,  -763,  -763,    -1,  -763,   970,  -379,
      19,   308,    29,  -763,  -123
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -471
static const yytype_int16 yytable[] =
{
      55,   132,   135,   254,    52,   173,   227,   174,   320,   309,
     273,   507,   264,   305,   305,   582,    99,   306,   306,   305,
     777,   724,   100,   306,   102,   219,   316,   221,   307,   307,
     250,   324,   250,    82,   307,   102,   329,   102,   643,   331,
     220,   704,   220,   856,   105,   822,   158,   158,    55,   126,
     396,   141,   326,   162,   463,   123,   707,   123,   586,   144,
     737,   -41,    99,   316,   114,   618,   498,   125,   185,   524,
     445,   595,   689,   595,    74,   595,   864,   114,   150,   428,
     757,   116,   143,   304,   304,   -77,   706,   194,   205,   304,
     767,   865,   316,    48,   102,   319,    57,    49,   362,   313,
     498,   653,    60,   332,   446,   264,   464,   499,   500,   238,
     525,    81,   501,   232,   123,   888,   853,    80,   -41,    95,
     323,   756,   163,   290,   210,   151,   587,   141,   193,   264,
     -41,   141,   319,   271,   141,   789,   822,   251,   158,   869,
     274,   499,   -77,   715,   327,   314,   501,   654,   165,   833,
     482,   655,   402,   529,   -77,   305,   815,   815,   306,   306,
     815,   319,   533,   535,   403,   688,   336,   488,   169,   307,
     307,   110,   114,   543,    63,    81,   116,   170,   439,   389,
     391,    80,   761,   333,    65,   336,   515,   835,   171,   354,
      54,   746,   496,   355,   815,   489,   342,   343,   332,   446,
     863,   732,    66,   408,   497,   483,   409,   762,   411,   127,
     413,   372,   241,   244,   531,   810,   701,   816,   240,   243,
     818,   112,   173,   202,   480,   304,   392,   204,   476,   707,
     701,   344,   141,   141,   848,   484,   350,   128,   134,    48,
     129,   172,   188,    49,    16,    48,   357,   508,   509,    49,
     392,   130,   141,    55,   859,    55,   155,    55,  -469,  -469,
     264,    48,   141,   808,   456,    49,   565,   566,   567,   485,
     568,   143,  -469,   549,    55,    27,    28,    29,   443,    99,
      55,   819,    55,    55,   461,   185,   467,   471,   873,   571,
     147,   875,   876,   599,   572,   576,   574,   577,   244,   236,
     866,   580,   619,   784,   243,   600,   333,   514,   250,   149,
     885,   158,   497,   620,   194,   621,   887,   195,   114,   768,
     769,   772,   116,   265,   595,   342,   343,   399,   194,   196,
     401,   336,   197,   398,   389,   558,   400,   606,   141,   198,
     336,   336,   613,   165,   332,   539,   619,   742,   663,   218,
     628,   336,   664,   199,   665,   506,   352,   620,   211,   403,
     512,   305,   667,   169,   711,   306,    16,   208,   209,    16,
      18,   202,   170,   641,   645,   646,   307,   201,   806,   651,
     555,   392,   701,   171,   211,   212,   245,   141,   344,   701,
     141,   141,   141,   619,   141,   659,  -163,    27,    28,    29,
      27,    28,    29,   701,   620,   206,   723,   141,   597,   141,
     598,   141,   843,   141,  -163,   141,   390,   141,   207,   141,
     226,   141,   579,   642,   222,   141,   223,  -163,  -163,  -163,
     275,   304,   701,   676,   224,   216,   217,   572,   220,   574,
     265,   141,   484,   410,   591,   412,    48,   414,   687,   362,
      49,    16,   396,   305,   310,   455,   312,   306,   172,   141,
     590,   141,   328,   702,   444,   141,   213,   330,   307,   141,
     462,   205,   468,   472,   214,   215,   165,   338,   166,   772,
     514,   347,    27,    28,    29,   339,   439,    16,   685,   348,
     349,   305,   403,   238,   340,   306,   169,   141,     1,   843,
       2,     3,     4,   442,   701,   170,   307,   456,   530,   452,
     454,   411,   766,   413,   396,   536,   171,   537,    27,    28,
      29,   540,   341,   304,   544,   363,   336,   256,   257,   351,
     336,   506,   336,   512,   364,    16,   424,    48,   260,   261,
     336,    49,   365,    50,   425,   426,   345,   346,   366,    16,
     374,   674,   375,   382,   141,    51,   393,   141,   238,   405,
     238,   304,   448,   394,   754,   404,    27,    28,    29,   264,
     141,   557,   141,   396,   416,   417,   744,   673,  -468,   811,
      27,    28,    29,   778,   406,   817,   141,   141,   407,   205,
     449,   220,   428,  -242,   415,  -242,   238,   441,    16,   429,
     141,   420,   421,   630,   492,   573,   493,   575,    16,  -242,
      48,   422,   423,  -242,    49,   495,    50,   523,    16,   541,
      48,  -242,  -242,   545,    49,   546,    50,   555,   229,    27,
      28,    29,   137,  -242,   141,   141,   547,   831,    51,    27,
      28,    29,   791,   408,    16,   548,   138,    16,   305,    27,
      28,    29,   306,   550,   792,   551,   685,   216,   427,   832,
     834,   563,   564,   307,   456,   780,   333,   720,   721,   722,
     238,   583,   584,   451,   232,    27,    28,    29,    27,    28,
      29,   256,   675,   638,   563,   421,   591,   552,   555,   141,
     392,  -181,   559,   733,   141,    16,   141,   141,   560,  -187,
    -458,   449,   200,   591,  -242,   581,  -242,   141,   585,    16,
     588,   781,   602,   141,    16,   456,   141,   669,   304,   601,
    -242,   607,   609,   755,  -242,   610,    27,    28,    29,   456,
     264,    78,  -242,  -242,   674,   141,   141,   141,   141,   883,
      27,    28,    29,   612,  -242,    27,    28,    29,   573,   615,
     575,   617,   141,   674,   890,   141,   141,   660,   501,   141,
     453,   141,   666,   677,   140,   141,   141,   141,  -183,   141,
    -184,   709,   703,   141,   710,   731,   700,   717,   735,    78,
     186,   186,   738,   739,   743,    78,   638,   654,   449,   238,
     700,  -242,   238,  -242,   141,   750,   716,   751,   753,   719,
     752,   773,   238,   141,   389,   391,   857,  -242,   774,   141,
     761,  -242,   113,   779,   114,   785,   115,    16,   116,  -242,
    -242,   796,   798,   799,   800,   786,   781,   787,   117,   802,
     807,  -242,   118,   202,   846,   845,   249,   849,   249,   850,
      91,   119,  -470,   238,   140,   795,   238,   238,    27,    28,
      29,   392,   120,   316,   867,   141,   308,   308,   868,   870,
     236,   871,   308,   141,   872,   238,    16,   874,   279,   280,
     281,   238,   877,   878,   141,   776,   879,    78,   141,   759,
     265,   287,    68,    69,   679,    16,   625,    17,    78,   803,
     626,    19,   142,    78,   788,   146,    78,    27,    28,    29,
     187,   844,    70,    78,   361,   288,   736,   886,   884,    72,
     830,   627,   804,   827,   880,   705,    27,    28,    29,   829,
     828,   851,   319,    73,   749,   797,   797,   797,   797,   882,
     644,   165,   578,   166,   556,   402,   478,   167,   113,   479,
     114,   113,   700,   114,   116,   140,   140,   403,   649,   700,
     668,   169,   108,    16,   473,   759,   759,   473,   118,   759,
     170,   118,   629,   700,   371,   140,   474,   119,   538,   592,
     119,   171,   142,   456,   652,   140,    16,   748,   475,   638,
     747,   475,   638,   490,    27,    28,    29,   527,   252,   630,
     553,   459,   858,   759,    16,   103,    48,   481,   308,   759,
      49,   491,    50,   569,   805,     0,   460,    27,    28,    29,
     671,   775,    78,     0,   229,     0,     0,    78,   137,   186,
       0,     0,     0,     0,     0,    27,    28,    29,     0,     0,
       0,     0,   138,   638,   228,     0,   638,   638,    16,     0,
      48,   390,    78,     0,    49,    78,    50,   186,     0,     0,
       0,    78,    78,     0,   186,   638,   186,     0,    51,     0,
     186,   638,    78,   186,   700,     0,    16,     0,    48,    27,
      28,    29,    49,   142,   142,     0,    16,     0,   725,     0,
       0,     0,    31,     0,     0,     0,   253,    61,    62,     0,
     137,     0,   272,   142,     0,     0,   456,    27,    28,    29,
     140,     0,     0,   142,   138,    94,   107,    27,    28,    29,
       6,     0,     0,   438,   249,     7,     0,     0,     0,     8,
     140,     0,   140,     9,   140,     0,     0,     0,   487,     0,
       0,     0,    10,     0,     0,    11,    12,     0,    13,    14,
      15,    83,    16,    84,    17,    85,    18,    86,    19,    20,
      21,     0,     0,     0,   140,    22,     0,    87,     0,    24,
       0,    88,    25,     0,    89,    90,    26,     0,     0,    91,
      92,     0,   140,    27,    28,    29,     0,    16,   140,    48,
      30,    93,   140,    49,     0,   614,     0,     0,    78,     0,
      78,     0,     0,     0,     0,   397,     0,   136,  -182,   623,
     624,   137,     0,     0,   308,     0,   639,     0,    27,    28,
      29,     0,   387,     0,    16,   138,    17,   614,   614,     0,
     650,     0,   614,     0,     0,   186,     0,     0,   142,     0,
     311,     0,     0,     0,   837,   325,   387,    78,    16,   114,
      48,    78,     0,    78,    49,    27,    28,    29,   142,     0,
     142,    78,   142,     0,     0,   361,   186,   770,   253,  -182,
       0,     0,   137,     0,     0,   767,     0,   140,     0,    27,
      28,    29,   356,   487,     0,     0,   138,     0,     0,   465,
       0,     0,   142,   140,     0,   140,     0,     0,   367,     0,
       0,   369,     0,   370,     0,     0,   308,     0,   373,     0,
     142,     0,   376,     0,   381,     0,   142,     0,     0,   459,
     142,    16,     0,    48,     0,     0,     0,    49,     0,    50,
       0,   316,     0,   487,    78,    78,    78,     0,   730,     0,
       0,    51,   734,    16,   308,    48,   279,   280,   281,    49,
       0,    50,    27,    28,    29,     0,     0,   140,   140,   287,
      68,    69,     0,    16,     0,    17,    16,   114,    48,    19,
       0,   317,    49,     0,    27,    28,    29,   387,   187,   186,
      70,     0,     0,   288,     0,   770,   253,    72,   318,     0,
     137,     0,   228,   767,    27,    28,    29,    27,    28,    29,
     319,    73,   521,   522,   138,   142,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   140,   526,   140,
     140,   142,     0,   142,    16,     0,    48,     0,     0,     0,
      49,   438,    16,     0,   202,    16,   140,    48,   204,   140,
       0,    49,   262,    50,   253,     0,    89,   263,   137,     0,
     730,     0,   790,   793,   794,    27,    28,    29,   131,     0,
       0,     0,   138,    27,    28,    29,    27,    28,    29,   487,
       0,   690,     0,     0,     0,     0,    47,     0,   140,   140,
       0,    47,    47,     0,   140,   142,   142,     0,     0,   634,
     140,     0,   823,   826,     0,     0,   570,    47,    47,    47,
      47,   308,     0,    16,   842,   430,   730,     0,     0,    49,
     847,    50,     0,     0,    47,     0,     0,   140,    16,     0,
      48,     0,   692,   854,    49,     0,    16,   137,    17,     0,
      18,     0,    19,     0,    27,    28,    29,   764,   253,    47,
      47,   138,   137,     0,     0,   142,   432,   142,   142,    27,
      28,    29,     0,     0,     0,     0,   138,    27,    28,    29,
       0,    47,     0,     0,   142,     0,     0,   142,     0,     0,
       0,   487,     0,     0,     0,     0,     0,    47,   140,    47,
       0,     0,   698,   823,     0,     0,   826,     0,     0,   640,
       0,   842,   634,     0,     0,     0,   698,     0,    47,    47,
       0,     0,    47,     0,     0,   334,   142,   142,     0,   809,
      77,     0,   142,   661,     0,   662,     0,    16,   142,   202,
       0,   203,     0,   204,    47,     0,     0,     0,     0,    47,
       0,    47,     0,    67,    68,    69,     0,    16,     0,    17,
     670,     0,   672,    19,     0,   142,    75,     0,    27,    28,
      29,     0,     0,     0,    70,     0,   862,    71,    77,   184,
     184,    72,     0,     0,    77,     0,    47,     0,    27,    28,
      29,    16,     0,    48,     0,    73,     0,    49,     0,    16,
       0,   725,    47,     0,    47,    47,   387,    47,   228,   395,
     765,   136,    47,   771,    75,   137,    47,    47,    47,   456,
      75,   726,    27,    28,    29,     0,   142,     0,     0,   138,
      27,    28,    29,    47,     0,   383,    47,     0,     0,     0,
      16,   760,    48,     0,     0,     0,    49,   889,    50,     0,
       0,   634,   634,   634,   634,   184,   184,     0,   262,     0,
     384,   184,    89,   263,   137,   745,     0,     0,   698,   440,
       0,    27,    28,    29,     0,   698,   184,   228,   138,     0,
       0,   814,   814,     0,     0,   814,     0,    77,     0,   698,
       0,     0,    77,     0,     0,   184,     0,     0,     0,     0,
       0,     0,    77,     0,     0,   634,    47,    47,   634,    16,
       0,    48,    75,     0,     0,    49,     0,    50,   698,   814,
     469,     0,    47,    75,    47,     0,     0,     0,    75,   229,
       0,    75,     0,   137,     0,     0,     0,     0,    75,     0,
      27,    28,    29,     0,     0,     0,     0,   138,     0,     0,
       0,     0,    16,   801,    48,     0,     0,     0,    49,   634,
      50,     0,   634,   634,     0,     0,     0,   387,     0,     0,
       0,   771,    51,     0,     0,   228,    47,     0,     0,   387,
       0,   634,     0,    27,    28,    29,     0,   634,     0,     0,
     698,     0,     0,     0,     0,     0,   184,   184,     0,     0,
      47,   637,   383,     0,     0,     0,     0,    16,   760,    48,
       0,    77,   852,    49,     0,    50,    77,     0,   184,     0,
       0,     0,     0,     0,     0,   262,     0,   384,     0,    89,
     385,   137,     0,     0,     0,     0,   334,     0,    27,    28,
      29,   184,     0,     0,    77,   138,   184,    75,     0,     0,
     184,   184,    75,   184,     0,   184,     0,     0,     0,   184,
       0,   184,   184,     0,    67,    68,    69,     0,    16,     0,
      17,     0,     0,     0,    19,     0,     0,    75,     0,     0,
      75,     0,     0,     0,     0,    70,    75,    75,    71,     0,
       0,   200,    72,    47,   699,    47,     0,    75,     0,    27,
      28,    29,     6,     0,   637,     0,    73,     7,   699,     0,
       0,     8,     0,     0,     0,     9,     0,    47,     0,    47,
       0,     0,     0,     0,    10,     0,     0,    11,    12,     0,
      13,    14,    15,     0,    16,     0,    17,     0,    18,     0,
      19,    20,    21,     0,    47,    47,    47,    22,   690,    23,
       0,    24,     0,     0,    25,     0,     0,     0,    26,     0,
     106,    16,     0,    17,     0,    27,    28,    29,     0,     0,
       0,     0,    30,   836,     0,   691,     0,   440,     0,   686,
      16,   837,   430,   838,   184,     0,    49,    77,     0,    77,
       0,     0,    27,    28,    29,     0,     0,   758,     0,   692,
     693,     0,     0,   184,   137,     0,     0,     0,     0,     0,
       0,    27,    28,    29,     0,     0,   184,   184,   138,   184,
       0,   184,     0,    75,   184,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   184,     0,     0,     0,
     184,     0,   184,   637,   637,   637,   637,     0,    47,    47,
     184,    16,     0,    48,     0,   184,     0,    49,     0,     0,
     699,     0,     0,     0,   228,     0,     0,   699,     0,   397,
       0,   136,    75,   758,   758,   137,    75,   758,    75,     0,
       0,   699,    27,    28,    29,     0,    75,     0,     0,   138,
       0,   383,     0,     0,     0,   184,    16,   637,    48,     0,
     637,     0,    49,     0,    50,     0,     0,     0,     0,     0,
     699,   758,     0,     0,   262,     0,   384,   758,    89,   385,
     137,     0,     0,    77,    77,    77,     0,    27,    28,    29,
       0,     0,     0,   184,   138,     0,     0,    47,     0,     0,
       0,     0,     0,     0,    16,     0,    17,   686,    18,     0,
      19,   637,     0,     0,   637,   637,   680,     0,     0,    75,
      75,    75,     0,     0,   432,     6,   681,     0,   184,     0,
       7,     0,     0,   637,     8,    27,    28,    29,     9,   637,
       0,     0,   699,     0,     0,     0,     0,    10,     0,     0,
      11,    12,     0,    13,    14,    15,    47,    16,     0,    17,
       0,    18,     0,    19,    20,    21,     0,     0,     0,     0,
      22,     6,    23,     0,    24,     0,     7,    25,     0,     0,
       8,    26,     0,  -439,     9,     0,     0,     0,    27,    28,
      29,     0,     0,    10,     0,    30,    11,    12,     0,    13,
      14,    15,     0,    16,     0,    17,     0,    18,     0,    19,
      20,    21,     0,     0,     0,     0,    22,     6,    23,     0,
      24,     0,     7,    25,     0,     0,     8,    26,     0,  -438,
       9,     0,     0,     0,    27,    28,    29,     0,     0,    10,
       0,    30,    11,    12,     0,    13,    14,    15,     0,    16,
     184,    17,     0,    18,     0,    19,    20,    21,     0,     0,
       0,     0,    22,     6,    23,     0,    24,     0,     7,    25,
       0,     0,     8,    26,     0,     0,   377,     0,     0,     0,
      27,    28,    29,     0,   228,    10,     0,    30,    11,    12,
       0,    13,    14,    15,     0,    16,     0,    17,     0,    18,
       0,    19,    20,    21,     0,     0,     0,     0,    22,     0,
      23,   383,    24,     0,     0,    25,    16,     0,    48,    26,
       0,     0,    49,   228,    50,     0,    27,    28,    29,     0,
       0,     0,     0,    30,   262,     0,   384,     0,    89,   263,
     137,     0,     0,     0,   228,     0,     0,    27,    28,    29,
     383,     0,     0,     0,   138,    16,     0,    48,     0,     0,
     690,    49,     0,   228,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   881,   253,    16,     0,    48,   137,
       0,     0,    49,     0,     0,     0,    27,    28,    29,     0,
       0,     0,    16,   138,   430,    16,   253,    48,    49,  -153,
     137,    49,   228,    50,     0,     0,     0,    27,    28,    29,
       0,   692,   693,     0,   138,   253,   137,     0,     0,   137,
       0,     0,     0,    27,    28,    29,    27,    28,    29,     0,
     138,     0,     0,   138,    16,     0,    48,     0,     0,     0,
      49,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     7,   253,     0,     0,     8,   137,     0,
       0,   156,     0,     0,     0,    27,    28,    29,     0,     0,
      10,     0,   138,    11,    12,     0,    13,    14,    15,     0,
      16,     0,    17,     0,    18,     0,    19,    20,    21,     0,
       0,     0,     0,    22,     0,    23,     0,    24,     0,     7,
      25,     0,     0,     8,    26,   157,     0,     9,     0,     0,
       0,    27,    28,    29,     0,     0,    10,     0,    30,    11,
      12,     0,    13,    14,    15,     0,    16,     0,    17,     0,
      18,     0,    19,    20,    21,     0,     0,     0,     0,    22,
       0,   242,     0,    24,     0,     0,    25,     0,     7,   368,
      26,     0,     8,     0,     0,     0,     9,    27,    28,    29,
       0,     0,     0,     0,    30,    10,     0,     0,    11,    12,
       0,    13,    14,    15,     0,    16,     0,    17,     0,    18,
       0,    19,    20,    21,     0,     0,     0,     0,    22,     0,
       0,     0,    24,     0,     0,    25,     0,     7,   200,    26,
       0,     8,     0,     0,     0,     9,    27,    28,    29,     0,
       0,     0,     0,    30,    10,     0,     0,    11,    12,     0,
      13,    14,    15,     0,    16,     0,    17,     0,    18,     0,
      19,    20,    21,     0,     0,     0,     0,    22,     0,    23,
       0,    24,     0,     7,    25,     0,     0,     8,    26,     0,
       0,     9,     0,     0,     0,    27,    28,    29,     0,     0,
      10,     0,    30,    11,    12,     0,    13,    14,    15,     0,
      16,     0,    17,     0,    18,     0,    19,    20,    21,     0,
       0,     0,     0,    22,     0,   239,     0,    24,     0,     7,
      25,     0,     0,     8,    26,     0,     0,     9,     0,     0,
       0,    27,    28,    29,     0,     0,    10,     0,    30,    11,
      12,     0,    13,    14,    15,     0,    16,     0,    17,     0,
      18,     0,    19,    20,    21,     0,     0,     0,     0,    22,
       0,   242,     0,    24,     0,     7,    25,     0,     0,     8,
      26,     0,     0,   156,     0,     0,     0,    27,    28,    29,
       0,     0,    10,     0,    30,    11,    12,     0,    13,    14,
      15,     0,    16,     0,    17,     0,    18,     0,    19,    20,
      21,     0,     0,     0,     0,    22,     0,    23,     0,    24,
       0,     7,    25,     0,     0,     8,    26,     0,     0,     9,
       0,     0,     0,    27,    28,    29,     0,     0,    10,     0,
      30,    11,    12,     0,    13,    14,    15,     0,    16,     0,
      17,     0,    18,     0,    19,    20,    21,     0,     0,     0,
       0,    22,     0,     0,     0,    24,     0,     0,    25,     0,
       0,     0,    26,     0,     0,     0,     0,     0,     0,    27,
      28,    29,   277,   278,     0,     0,    30,     0,     0,     0,
     279,   280,   281,   282,   283,   284,     0,     0,   285,   286,
       0,     0,     0,   287,    68,    69,     0,    16,     0,    17,
       0,     0,     0,    19,     0,     0,     0,     0,     0,     0,
       0,     0,   187,     0,    70,     0,     0,   288,     0,     0,
       0,    72,   289,     0,     0,     0,     0,   290,    27,    28,
      29,   291,   292,   277,   278,    73,     0,     0,     0,     0,
       0,   279,   280,   281,   282,   283,   284,     0,     0,   285,
     286,     0,     0,     0,   287,    68,    69,     0,    16,     0,
      17,     0,     0,     0,    19,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,    70,     0,     0,   288,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,    27,
      28,    29,   291,   292,     0,    12,    73,    13,    14,    15,
       0,    16,     0,    17,     0,    18,     0,    19,    20,    21,
       0,     0,     0,     0,     0,     0,     0,     0,    24,     0,
       0,    25,     0,     0,     0,    26,     0,     0,     0,     0,
       0,     0,    27,    28,    29,     0,     0,     0,     0,    30,
      67,    68,    69,   113,    16,   114,    17,     0,     0,   116,
      19,     0,     0,     0,  -246,   194,     0,     0,     0,   510,
       0,    70,     0,   118,    71,     0,  -246,     0,    72,     0,
       0,   474,   119,     0,     0,    27,    28,    29,     0,     0,
       0,     0,    73,   511,   164,    68,    69,   165,    16,   166,
      17,     0,     0,   167,    19,     0,     0,     0,     0,     0,
       0,     0,     0,   168,     0,    70,     0,   169,    71,     0,
      89,    90,    72,     0,     0,     0,   170,     0,     0,    27,
      28,    29,     0,     0,     0,     0,    73,   171,   287,    68,
      69,   165,    16,   166,    17,     0,     0,   167,    19,     0,
       0,     0,     0,     0,     0,     0,     0,   168,     0,    70,
       0,   169,   288,     0,    89,    90,    72,     0,     0,     0,
     170,     0,     0,    27,    28,    29,     0,     0,     0,     0,
      73,   171,    67,    68,    69,   113,    16,   114,    17,     0,
       0,   116,    19,     0,     0,     0,     0,   194,     0,     0,
       0,   510,     0,    70,     0,   118,    71,     0,     0,     0,
      72,     0,     0,   474,   119,     0,     0,    27,    28,    29,
       0,     0,     0,     0,    73,   511,   164,    68,    69,     0,
      16,     0,    17,     0,     0,     0,    19,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     0,    70,     0,     0,
      71,     0,     0,     0,    72,     0,   106,     0,     0,     0,
       0,    27,    28,    29,     0,   164,    68,    69,    73,    16,
       0,    17,     0,     0,     0,    19,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,    70,     0,     0,    71,
       0,     0,     0,    72,   516,     0,     0,     0,     0,     0,
      27,    28,    29,     0,    67,    68,    69,    73,    16,     0,
      17,     0,     0,     0,    19,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    70,   192,     0,    71,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,    27,
      28,    29,     0,    67,    68,    69,    73,    16,     0,    17,
       0,     0,     0,    19,     0,     0,     0,     0,     0,     0,
       0,     0,   528,     0,    70,     0,     0,    71,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,    27,    28,
      29,     0,   164,    68,    69,    73,    16,     0,    17,     0,
       0,     0,    19,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,    70,     0,     0,    71,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,    27,    28,    29,
       0,    67,    68,    69,    73,    16,     0,    17,     0,     0,
       0,    19,     0,     0,     0,     0,     0,     0,     0,     0,
     532,     0,    70,     0,     0,    71,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,    27,    28,    29,     0,
      67,    68,    69,    73,    16,     0,    17,     0,     0,     0,
      19,     0,     0,     0,     0,     0,     0,     0,     0,   534,
       0,    70,     0,     0,    71,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,    27,    28,    29,     0,    67,
      68,    69,    73,    16,     0,    17,     0,     0,     0,    19,
       0,     0,     0,     0,     0,     0,     0,     0,   542,     0,
      70,     0,     0,    71,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,    27,    28,    29,     0,    67,    68,
      69,    73,    16,     0,    17,     0,     0,     0,    19,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    70,
       0,     0,    71,     0,     0,   616,    72,     0,     0,     0,
       0,     0,     0,    27,    28,    29,     0,   647,    68,    69,
      73,    16,     0,    17,     0,     0,     0,    19,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,    70,     0,
       0,    71,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,    27,    28,    29,     0,    67,    68,    69,    73,
      16,     0,    17,    16,     0,    48,    19,     0,     0,    49,
       0,     0,     0,     0,     0,     0,     0,    70,     0,     0,
      71,   397,     0,   136,    72,   418,   419,   137,     0,     0,
       0,    27,    28,    29,    27,    28,    29,    16,    73,    48,
       0,   138,     0,    49,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   397,    16,   136,    48,   561,
     562,   137,    49,     0,     0,     0,     0,     0,    27,    28,
      29,     0,     0,     0,   397,   138,   136,     0,   561,   419,
     137,     0,     0,     0,    16,     0,    48,    27,    28,    29,
      49,     0,     0,     0,   138,     0,     0,     0,     0,     0,
       0,     0,     0,    16,   136,    48,   258,   259,   137,    49,
       0,     0,   589,     0,     0,    27,    28,    29,     0,    16,
       0,    48,   138,   136,  -182,    49,     0,   137,     0,     0,
       0,     0,     0,     0,    27,    28,    29,   741,     0,   136,
    -182,   138,     0,   137,     0,     0,     0,    16,   114,    48,
      27,    28,    29,    49,     0,     0,     0,   138,     0,     0,
       0,     0,     0,    16,     0,    48,     0,   136,     0,    49,
       0,   137,   603,     0,   767,     0,     0,     0,    27,    28,
      29,     0,    16,   136,    48,   138,     0,   137,    49,     0,
       0,     0,     0,     0,    27,    28,    29,     0,    16,     0,
      48,   138,   136,  -182,    49,     0,   137,     0,     0,     0,
       0,     0,     0,    27,    28,    29,   740,     0,   136,    16,
     138,    48,   137,     0,     0,    49,     0,     0,     0,    27,
      28,    29,     0,     0,     0,     0,   138,     0,   812,   253,
      16,     0,    48,   137,     0,     0,    49,     0,    50,     0,
      27,    28,    29,     0,     0,     0,    16,   138,    48,    16,
     229,    48,    49,     0,   137,    49,     0,     0,     0,     0,
       0,    27,    28,    29,   741,    16,   136,    48,   138,   136,
     137,    49,     0,   137,     0,     0,     0,    27,    28,    29,
      27,    28,    29,     0,   138,   253,    16,   138,   430,   137,
      18,     0,   431,     0,     0,     0,    27,    28,    29,     0,
       0,     0,     0,   138,     0,     0,   432,     0,   433,   434,
      16,     0,   430,     0,    18,   435,   431,    27,    28,    29,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     432,     0,     0,   678,     0,     0,     0,     0,     0,   435,
       0,    27,    28,    29
};

static const yytype_int16 yycheck[] =
{
       3,    51,    51,   130,     3,    71,   110,    71,   153,   148,
     137,   305,   136,   147,   148,   435,    25,   147,   148,   153,
     708,   618,    25,   153,    25,   103,     1,   105,   147,   148,
     127,   154,   129,    24,   153,    36,   159,    38,   503,   162,
     103,   599,   105,   805,    25,   773,    65,    66,    51,    40,
     235,    54,   156,    66,     1,    36,   603,    38,     1,     1,
     630,     1,    71,     1,    34,   485,     1,    38,    71,     1,
       1,   450,   588,   452,    22,   454,    55,    34,    11,    43,
     692,    38,    46,   147,   148,     1,   602,    44,    91,   153,
      60,    70,     1,    35,    95,    70,    62,    39,   195,    12,
       1,     1,     0,    60,    35,   229,    53,    42,    43,   112,
      42,    23,    47,   112,    95,   877,   804,    23,    58,    25,
      58,   691,    70,    63,    95,    58,    69,   130,    76,   253,
      70,   134,    70,   136,   137,   732,   864,   128,   157,   827,
     143,    42,    58,   608,   157,    58,    47,    47,    32,    58,
     289,    51,    36,   333,    70,   289,   768,   769,   288,   289,
     772,    70,   342,   343,    48,     1,   168,     1,    52,   288,
     289,    13,    34,   353,    69,    87,    38,    61,   275,   229,
     229,    87,    22,   164,    69,   187,   309,   784,    72,    55,
       3,   656,    43,    59,   806,    29,   177,   178,    60,    35,
     812,   621,    69,   253,    55,     1,   256,    47,   258,    69,
     260,   214,   124,   125,   341,   762,   589,   769,   124,   125,
     772,    43,   288,    35,   288,   289,   229,    39,   287,   776,
     603,   179,   235,   236,   792,    31,   184,    44,    51,    35,
      69,    71,    72,    39,    33,    35,   194,   306,   307,    39,
     253,    43,   255,   256,   806,   258,    15,   260,    55,    56,
     384,    35,   265,    53,    53,    39,   393,   394,   395,    65,
     397,    46,    69,   377,   277,    64,    65,    66,   277,   288,
     283,    70,   285,   286,   283,   288,   285,   286,   846,   416,
      69,   849,   850,    43,   418,   422,   420,   424,   210,   112,
      43,   428,    53,   723,   210,    55,   287,   308,   405,     7,
     868,   330,    55,    64,    44,    66,   874,    69,    34,   698,
     699,   700,    38,   136,   703,   306,   307,   239,    44,    56,
     242,   333,    56,   239,   384,   384,   242,   464,   341,    56,
     342,   343,   476,    32,    60,   348,    53,   641,   528,    42,
     495,   353,   532,    56,   534,   303,    72,    64,    55,    48,
     308,   495,   542,    52,     1,   495,    33,    55,    56,    33,
      37,    35,    61,   500,   508,   509,   495,    56,   757,   513,
     383,   384,   755,    72,    55,    56,    53,   390,   336,   762,
     393,   394,   395,    53,   397,   518,    33,    64,    65,    66,
      64,    65,    66,   776,    64,    56,    66,   410,   452,   412,
     454,   414,   785,   416,    51,   418,   229,   420,    56,   422,
      59,   424,   425,   501,    45,   428,    47,    64,    65,    66,
      53,   495,   805,   560,    55,    55,    56,   561,   501,   563,
     253,   444,    31,   256,   447,   258,    35,   260,   587,   546,
      39,    33,   637,   587,    69,     1,    11,   587,   288,   462,
      42,   464,    49,   590,   277,   468,    47,    58,   587,   472,
     283,   474,   285,   286,    55,    56,    32,    55,    34,   858,
     481,    47,    64,    65,    66,    56,   583,    33,   585,    55,
      56,   625,    48,   496,    56,   625,    52,   500,     1,   872,
       3,     4,     5,     1,   877,    61,   625,    53,   338,   280,
     281,   561,   697,   563,   699,   345,    72,   347,    64,    65,
      66,   351,    43,   587,   354,    60,   528,    55,    56,    42,
     532,   479,   534,   481,    60,    33,    47,    35,    55,    56,
     542,    39,    60,    41,    55,    56,    55,    56,    60,    33,
      56,   554,    56,    47,   557,    53,    51,   560,   561,    55,
     563,   625,     1,    54,   687,    70,    64,    65,    66,   693,
     573,   384,   575,   758,    55,    56,   654,    61,    42,   764,
      64,    65,    66,   710,    42,   770,   589,   590,    70,   592,
      29,   654,    43,    32,    56,    34,   599,    13,    33,    59,
     603,    55,    56,    28,    70,   418,    58,   420,    33,    48,
      35,    55,    56,    52,    39,    58,    41,    12,    33,    29,
      35,    60,    61,    70,    39,    55,    41,   630,    53,    64,
      65,    66,    57,    72,   637,   638,    42,   782,    53,    64,
      65,    66,    31,   693,    33,    56,    71,    33,   782,    64,
      65,    66,   782,    49,    43,    55,   753,    55,    56,   782,
     783,    55,    56,   782,    53,    51,   647,   615,   616,   617,
     673,    55,    56,     1,   673,    64,    65,    66,    64,    65,
      66,    55,    56,   496,    55,    56,   689,    45,   691,   692,
     693,    54,    56,    31,   697,    33,   699,   700,    54,    54,
      53,    29,    56,   706,    32,    56,    34,   710,    53,    33,
      54,   714,    54,   716,    33,    53,   719,   547,   782,    31,
      48,    47,    54,    42,    52,    13,    64,    65,    66,    53,
     854,    22,    60,    61,   737,   738,   739,   740,   741,   866,
      64,    65,    66,    54,    72,    64,    65,    66,   561,    56,
     563,    56,   755,   756,   881,   758,   759,    58,    47,   762,
       1,   764,    56,    56,    54,   768,   769,   770,    54,   772,
      54,    56,    55,   776,    55,    53,   589,    69,    31,    70,
      71,    72,    51,    54,    42,    76,   599,    47,    29,   792,
     603,    32,   795,    34,   797,    56,   609,    56,    55,   612,
      56,    69,   805,   806,   854,   854,   805,    48,    15,   812,
      22,    52,    32,    55,    34,    53,    36,    33,    38,    60,
      61,   738,   739,   740,   741,    56,   829,    55,    48,    51,
      56,    72,    52,    35,    43,    56,   127,    43,   129,    43,
      60,    61,    69,   846,   134,    61,   849,   850,    64,    65,
      66,   854,    72,     1,    55,   858,   147,   148,    43,    56,
     673,    56,   153,   866,    55,   868,    33,    43,    16,    17,
      18,   874,    54,    56,   877,    42,    55,   168,   881,   692,
     693,    29,    30,    31,   583,    33,   493,    35,   179,   753,
     493,    39,    54,   184,   731,    57,   187,    64,    65,    66,
      48,   787,    50,   194,   195,    53,   625,   872,   867,    57,
      58,   495,   755,   776,   864,   600,    64,    65,    66,   780,
     779,   795,    70,    71,   673,   738,   739,   740,   741,   866,
     504,    32,   425,    34,   384,    36,   288,    38,    32,   288,
      34,    32,   755,    34,    38,   235,   236,    48,   511,   762,
     546,    52,    26,    33,    48,   768,   769,    48,    52,   772,
      61,    52,     1,   776,   214,   255,    60,    61,   348,    60,
      61,    72,   134,    53,   516,   265,    33,   660,    72,   792,
     657,    72,   795,    63,    64,    65,    66,   330,   129,    28,
     382,   282,   805,   806,    33,    25,    35,   288,   289,   812,
      39,   292,    41,   405,    61,    -1,     1,    64,    65,    66,
     551,   703,   303,    -1,    53,    -1,    -1,   308,    57,   310,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    -1,    -1,
      -1,    -1,    71,   846,     1,    -1,   849,   850,    33,    -1,
      35,   854,   333,    -1,    39,   336,    41,   338,    -1,    -1,
      -1,   342,   343,    -1,   345,   868,   347,    -1,    53,    -1,
     351,   874,   353,   354,   877,    -1,    33,    -1,    35,    64,
      65,    66,    39,   235,   236,    -1,    33,    -1,    35,    -1,
      -1,    -1,     2,    -1,    -1,    -1,    53,     7,     8,    -1,
      57,    -1,    59,   255,    -1,    -1,    53,    64,    65,    66,
     390,    -1,    -1,   265,    71,    25,    26,    64,    65,    66,
       1,    -1,    -1,   275,   405,     6,    -1,    -1,    -1,    10,
     410,    -1,   412,    14,   414,    -1,    -1,    -1,   290,    -1,
      -1,    -1,    23,    -1,    -1,    26,    27,    -1,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    -1,    -1,    -1,   444,    46,    -1,    48,    -1,    50,
      -1,    52,    53,    -1,    55,    56,    57,    -1,    -1,    60,
      61,    -1,   462,    64,    65,    66,    -1,    33,   468,    35,
      71,    72,   472,    39,    -1,   476,    -1,    -1,   479,    -1,
     481,    -1,    -1,    -1,    -1,    51,    -1,    53,    54,   490,
     491,    57,    -1,    -1,   495,    -1,   497,    -1,    64,    65,
      66,    -1,   229,    -1,    33,    71,    35,   508,   509,    -1,
     511,    -1,   513,    -1,    -1,   516,    -1,    -1,   390,    -1,
     150,    -1,    -1,    -1,    53,   155,   253,   528,    33,    34,
      35,   532,    -1,   534,    39,    64,    65,    66,   410,    -1,
     412,   542,   414,    -1,    -1,   546,   547,    52,    53,    54,
      -1,    -1,    57,    -1,    -1,    60,    -1,   557,    -1,    64,
      65,    66,   192,   435,    -1,    -1,    71,    -1,    -1,     1,
      -1,    -1,   444,   573,    -1,   575,    -1,    -1,   208,    -1,
      -1,   211,    -1,   213,    -1,    -1,   587,    -1,   218,    -1,
     462,    -1,   222,    -1,   224,    -1,   468,    -1,    -1,   600,
     472,    33,    -1,    35,    -1,    -1,    -1,    39,    -1,    41,
      -1,     1,    -1,   485,   615,   616,   617,    -1,   619,    -1,
      -1,    53,   623,    33,   625,    35,    16,    17,    18,    39,
      -1,    41,    64,    65,    66,    -1,    -1,   637,   638,    29,
      30,    31,    -1,    33,    -1,    35,    33,    34,    35,    39,
      -1,    41,    39,    -1,    64,    65,    66,   384,    48,   660,
      50,    -1,    -1,    53,    -1,    52,    53,    57,    58,    -1,
      57,    -1,     1,    60,    64,    65,    66,    64,    65,    66,
      70,    71,   312,   313,    71,   557,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   697,   328,   699,
     700,   573,    -1,   575,    33,    -1,    35,    -1,    -1,    -1,
      39,   583,    33,    -1,    35,    33,   716,    35,    39,   719,
      -1,    39,    51,    41,    53,    -1,    55,    56,    57,    -1,
     731,    -1,   733,   734,   735,    64,    65,    66,    56,    -1,
      -1,    -1,    71,    64,    65,    66,    64,    65,    66,   621,
      -1,     1,    -1,    -1,    -1,    -1,     2,    -1,   758,   759,
      -1,     7,     8,    -1,   764,   637,   638,    -1,    -1,   496,
     770,    -1,   773,   774,    -1,    -1,   406,    23,    24,    25,
      26,   782,    -1,    33,   785,    35,   787,    -1,    -1,    39,
     791,    41,    -1,    -1,    40,    -1,    -1,   797,    33,    -1,
      35,    -1,    52,    53,    39,    -1,    33,    57,    35,    -1,
      37,    -1,    39,    -1,    64,    65,    66,    52,    53,    65,
      66,    71,    57,    -1,    -1,   697,    53,   699,   700,    64,
      65,    66,    -1,    -1,    -1,    -1,    71,    64,    65,    66,
      -1,    87,    -1,    -1,   716,    -1,    -1,   719,    -1,    -1,
      -1,   723,    -1,    -1,    -1,    -1,    -1,   103,   858,   105,
      -1,    -1,   589,   864,    -1,    -1,   867,    -1,    -1,   499,
      -1,   872,   599,    -1,    -1,    -1,   603,    -1,   124,   125,
      -1,    -1,   128,    -1,    -1,     1,   758,   759,    -1,   761,
      22,    -1,   764,   523,    -1,   525,    -1,    33,   770,    35,
      -1,    37,    -1,    39,   150,    -1,    -1,    -1,    -1,   155,
      -1,   157,    -1,    29,    30,    31,    -1,    33,    -1,    35,
     550,    -1,   552,    39,    -1,   797,    22,    -1,    64,    65,
      66,    -1,    -1,    -1,    50,    -1,   808,    53,    70,    71,
      72,    57,    -1,    -1,    76,    -1,   192,    -1,    64,    65,
      66,    33,    -1,    35,    -1,    71,    -1,    39,    -1,    33,
      -1,    35,   208,    -1,   210,   211,   693,   213,     1,    51,
     697,    53,   218,   700,    70,    57,   222,   223,   224,    53,
      76,    55,    64,    65,    66,    -1,   858,    -1,    -1,    71,
      64,    65,    66,   239,    -1,    28,   242,    -1,    -1,    -1,
      33,    34,    35,    -1,    -1,    -1,    39,   879,    41,    -1,
      -1,   738,   739,   740,   741,   147,   148,    -1,    51,    -1,
      53,   153,    55,    56,    57,   655,    -1,    -1,   755,   275,
      -1,    64,    65,    66,    -1,   762,   168,     1,    71,    -1,
      -1,   768,   769,    -1,    -1,   772,    -1,   179,    -1,   776,
      -1,    -1,   184,    -1,    -1,   187,    -1,    -1,    -1,    -1,
      -1,    -1,   194,    -1,    -1,   792,   312,   313,   795,    33,
      -1,    35,   168,    -1,    -1,    39,    -1,    41,   805,   806,
       1,    -1,   328,   179,   330,    -1,    -1,    -1,   184,    53,
      -1,   187,    -1,    57,    -1,    -1,    -1,    -1,   194,    -1,
      64,    65,    66,    -1,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    33,   743,    35,    -1,    -1,    -1,    39,   846,
      41,    -1,   849,   850,    -1,    -1,    -1,   854,    -1,    -1,
      -1,   858,    53,    -1,    -1,     1,   382,    -1,    -1,   866,
      -1,   868,    -1,    64,    65,    66,    -1,   874,    -1,    -1,
     877,    -1,    -1,    -1,    -1,    -1,   288,   289,    -1,    -1,
     406,   496,    28,    -1,    -1,    -1,    -1,    33,    34,    35,
      -1,   303,   802,    39,    -1,    41,   308,    -1,   310,    -1,
      -1,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    55,
      56,    57,    -1,    -1,    -1,    -1,     1,    -1,    64,    65,
      66,   333,    -1,    -1,   336,    71,   338,   303,    -1,    -1,
     342,   343,   308,   345,    -1,   347,    -1,    -1,    -1,   351,
      -1,   353,   354,    -1,    29,    30,    31,    -1,    33,    -1,
      35,    -1,    -1,    -1,    39,    -1,    -1,   333,    -1,    -1,
     336,    -1,    -1,    -1,    -1,    50,   342,   343,    53,    -1,
      -1,    56,    57,   499,   589,   501,    -1,   353,    -1,    64,
      65,    66,     1,    -1,   599,    -1,    71,     6,   603,    -1,
      -1,    10,    -1,    -1,    -1,    14,    -1,   523,    -1,   525,
      -1,    -1,    -1,    -1,    23,    -1,    -1,    26,    27,    -1,
      29,    30,    31,    -1,    33,    -1,    35,    -1,    37,    -1,
      39,    40,    41,    -1,   550,   551,   552,    46,     1,    48,
      -1,    50,    -1,    -1,    53,    -1,    -1,    -1,    57,    -1,
      59,    33,    -1,    35,    -1,    64,    65,    66,    -1,    -1,
      -1,    -1,    71,    45,    -1,    28,    -1,   583,    -1,   585,
      33,    53,    35,    55,   476,    -1,    39,   479,    -1,   481,
      -1,    -1,    64,    65,    66,    -1,    -1,   692,    -1,    52,
      53,    -1,    -1,   495,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    64,    65,    66,    -1,    -1,   508,   509,    71,   511,
      -1,   513,    -1,   479,   516,   481,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   528,    -1,    -1,    -1,
     532,    -1,   534,   738,   739,   740,   741,    -1,   654,   655,
     542,    33,    -1,    35,    -1,   547,    -1,    39,    -1,    -1,
     755,    -1,    -1,    -1,     1,    -1,    -1,   762,    -1,    51,
      -1,    53,   528,   768,   769,    57,   532,   772,   534,    -1,
      -1,   776,    64,    65,    66,    -1,   542,    -1,    -1,    71,
      -1,    28,    -1,    -1,    -1,   587,    33,   792,    35,    -1,
     795,    -1,    39,    -1,    41,    -1,    -1,    -1,    -1,    -1,
     805,   806,    -1,    -1,    51,    -1,    53,   812,    55,    56,
      57,    -1,    -1,   615,   616,   617,    -1,    64,    65,    66,
      -1,    -1,    -1,   625,    71,    -1,    -1,   743,    -1,    -1,
      -1,    -1,    -1,    -1,    33,    -1,    35,   753,    37,    -1,
      39,   846,    -1,    -1,   849,   850,    45,    -1,    -1,   615,
     616,   617,    -1,    -1,    53,     1,    55,    -1,   660,    -1,
       6,    -1,    -1,   868,    10,    64,    65,    66,    14,   874,
      -1,    -1,   877,    -1,    -1,    -1,    -1,    23,    -1,    -1,
      26,    27,    -1,    29,    30,    31,   802,    33,    -1,    35,
      -1,    37,    -1,    39,    40,    41,    -1,    -1,    -1,    -1,
      46,     1,    48,    -1,    50,    -1,     6,    53,    -1,    -1,
      10,    57,    -1,    59,    14,    -1,    -1,    -1,    64,    65,
      66,    -1,    -1,    23,    -1,    71,    26,    27,    -1,    29,
      30,    31,    -1,    33,    -1,    35,    -1,    37,    -1,    39,
      40,    41,    -1,    -1,    -1,    -1,    46,     1,    48,    -1,
      50,    -1,     6,    53,    -1,    -1,    10,    57,    -1,    59,
      14,    -1,    -1,    -1,    64,    65,    66,    -1,    -1,    23,
      -1,    71,    26,    27,    -1,    29,    30,    31,    -1,    33,
     782,    35,    -1,    37,    -1,    39,    40,    41,    -1,    -1,
      -1,    -1,    46,     1,    48,    -1,    50,    -1,     6,    53,
      -1,    -1,    10,    57,    -1,    -1,    14,    -1,    -1,    -1,
      64,    65,    66,    -1,     1,    23,    -1,    71,    26,    27,
      -1,    29,    30,    31,    -1,    33,    -1,    35,    -1,    37,
      -1,    39,    40,    41,    -1,    -1,    -1,    -1,    46,    -1,
      48,    28,    50,    -1,    -1,    53,    33,    -1,    35,    57,
      -1,    -1,    39,     1,    41,    -1,    64,    65,    66,    -1,
      -1,    -1,    -1,    71,    51,    -1,    53,    -1,    55,    56,
      57,    -1,    -1,    -1,     1,    -1,    -1,    64,    65,    66,
      28,    -1,    -1,    -1,    71,    33,    -1,    35,    -1,    -1,
       1,    39,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    52,    53,    33,    -1,    35,    57,
      -1,    -1,    39,    -1,    -1,    -1,    64,    65,    66,    -1,
      -1,    -1,    33,    71,    35,    33,    53,    35,    39,    56,
      57,    39,     1,    41,    -1,    -1,    -1,    64,    65,    66,
      -1,    52,    53,    -1,    71,    53,    57,    -1,    -1,    57,
      -1,    -1,    -1,    64,    65,    66,    64,    65,    66,    -1,
      71,    -1,    -1,    71,    33,    -1,    35,    -1,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     6,    53,    -1,    -1,    10,    57,    -1,
      -1,    14,    -1,    -1,    -1,    64,    65,    66,    -1,    -1,
      23,    -1,    71,    26,    27,    -1,    29,    30,    31,    -1,
      33,    -1,    35,    -1,    37,    -1,    39,    40,    41,    -1,
      -1,    -1,    -1,    46,    -1,    48,    -1,    50,    -1,     6,
      53,    -1,    -1,    10,    57,    58,    -1,    14,    -1,    -1,
      -1,    64,    65,    66,    -1,    -1,    23,    -1,    71,    26,
      27,    -1,    29,    30,    31,    -1,    33,    -1,    35,    -1,
      37,    -1,    39,    40,    41,    -1,    -1,    -1,    -1,    46,
      -1,    48,    -1,    50,    -1,    -1,    53,    -1,     6,    56,
      57,    -1,    10,    -1,    -1,    -1,    14,    64,    65,    66,
      -1,    -1,    -1,    -1,    71,    23,    -1,    -1,    26,    27,
      -1,    29,    30,    31,    -1,    33,    -1,    35,    -1,    37,
      -1,    39,    40,    41,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    -1,    50,    -1,    -1,    53,    -1,     6,    56,    57,
      -1,    10,    -1,    -1,    -1,    14,    64,    65,    66,    -1,
      -1,    -1,    -1,    71,    23,    -1,    -1,    26,    27,    -1,
      29,    30,    31,    -1,    33,    -1,    35,    -1,    37,    -1,
      39,    40,    41,    -1,    -1,    -1,    -1,    46,    -1,    48,
      -1,    50,    -1,     6,    53,    -1,    -1,    10,    57,    -1,
      -1,    14,    -1,    -1,    -1,    64,    65,    66,    -1,    -1,
      23,    -1,    71,    26,    27,    -1,    29,    30,    31,    -1,
      33,    -1,    35,    -1,    37,    -1,    39,    40,    41,    -1,
      -1,    -1,    -1,    46,    -1,    48,    -1,    50,    -1,     6,
      53,    -1,    -1,    10,    57,    -1,    -1,    14,    -1,    -1,
      -1,    64,    65,    66,    -1,    -1,    23,    -1,    71,    26,
      27,    -1,    29,    30,    31,    -1,    33,    -1,    35,    -1,
      37,    -1,    39,    40,    41,    -1,    -1,    -1,    -1,    46,
      -1,    48,    -1,    50,    -1,     6,    53,    -1,    -1,    10,
      57,    -1,    -1,    14,    -1,    -1,    -1,    64,    65,    66,
      -1,    -1,    23,    -1,    71,    26,    27,    -1,    29,    30,
      31,    -1,    33,    -1,    35,    -1,    37,    -1,    39,    40,
      41,    -1,    -1,    -1,    -1,    46,    -1,    48,    -1,    50,
      -1,     6,    53,    -1,    -1,    10,    57,    -1,    -1,    14,
      -1,    -1,    -1,    64,    65,    66,    -1,    -1,    23,    -1,
      71,    26,    27,    -1,    29,    30,    31,    -1,    33,    -1,
      35,    -1,    37,    -1,    39,    40,    41,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    -1,    50,    -1,    -1,    53,    -1,
      -1,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    64,
      65,    66,     8,     9,    -1,    -1,    71,    -1,    -1,    -1,
      16,    17,    18,    19,    20,    21,    -1,    -1,    24,    25,
      -1,    -1,    -1,    29,    30,    31,    -1,    33,    -1,    35,
      -1,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    -1,    50,    -1,    -1,    53,    -1,    -1,
      -1,    57,    58,    -1,    -1,    -1,    -1,    63,    64,    65,
      66,    67,    68,     8,     9,    71,    -1,    -1,    -1,    -1,
      -1,    16,    17,    18,    19,    20,    21,    -1,    -1,    24,
      25,    -1,    -1,    -1,    29,    30,    31,    -1,    33,    -1,
      35,    -1,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    -1,    50,    -1,    -1,    53,    -1,
      -1,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    64,
      65,    66,    67,    68,    -1,    27,    71,    29,    30,    31,
      -1,    33,    -1,    35,    -1,    37,    -1,    39,    40,    41,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    -1,
      -1,    53,    -1,    -1,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    64,    65,    66,    -1,    -1,    -1,    -1,    71,
      29,    30,    31,    32,    33,    34,    35,    -1,    -1,    38,
      39,    -1,    -1,    -1,    43,    44,    -1,    -1,    -1,    48,
      -1,    50,    -1,    52,    53,    -1,    55,    -1,    57,    -1,
      -1,    60,    61,    -1,    -1,    64,    65,    66,    -1,    -1,
      -1,    -1,    71,    72,    29,    30,    31,    32,    33,    34,
      35,    -1,    -1,    38,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    -1,    50,    -1,    52,    53,    -1,
      55,    56,    57,    -1,    -1,    -1,    61,    -1,    -1,    64,
      65,    66,    -1,    -1,    -1,    -1,    71,    72,    29,    30,
      31,    32,    33,    34,    35,    -1,    -1,    38,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    50,
      -1,    52,    53,    -1,    55,    56,    57,    -1,    -1,    -1,
      61,    -1,    -1,    64,    65,    66,    -1,    -1,    -1,    -1,
      71,    72,    29,    30,    31,    32,    33,    34,    35,    -1,
      -1,    38,    39,    -1,    -1,    -1,    -1,    44,    -1,    -1,
      -1,    48,    -1,    50,    -1,    52,    53,    -1,    -1,    -1,
      57,    -1,    -1,    60,    61,    -1,    -1,    64,    65,    66,
      -1,    -1,    -1,    -1,    71,    72,    29,    30,    31,    -1,
      33,    -1,    35,    -1,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    -1,    50,    -1,    -1,
      53,    -1,    -1,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    65,    66,    -1,    29,    30,    31,    71,    33,
      -1,    35,    -1,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    -1,    50,    -1,    -1,    53,
      -1,    -1,    -1,    57,    58,    -1,    -1,    -1,    -1,    -1,
      64,    65,    66,    -1,    29,    30,    31,    71,    33,    -1,
      35,    -1,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    50,    51,    -1,    53,    -1,
      -1,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    64,
      65,    66,    -1,    29,    30,    31,    71,    33,    -1,    35,
      -1,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    -1,    50,    -1,    -1,    53,    -1,    -1,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    64,    65,
      66,    -1,    29,    30,    31,    71,    33,    -1,    35,    -1,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    -1,    50,    -1,    -1,    53,    -1,    -1,    -1,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    64,    65,    66,
      -1,    29,    30,    31,    71,    33,    -1,    35,    -1,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    -1,    50,    -1,    -1,    53,    -1,    -1,    -1,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    64,    65,    66,    -1,
      29,    30,    31,    71,    33,    -1,    35,    -1,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      -1,    50,    -1,    -1,    53,    -1,    -1,    -1,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    64,    65,    66,    -1,    29,
      30,    31,    71,    33,    -1,    35,    -1,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,
      50,    -1,    -1,    53,    -1,    -1,    -1,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    64,    65,    66,    -1,    29,    30,
      31,    71,    33,    -1,    35,    -1,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,
      -1,    -1,    53,    -1,    -1,    56,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    64,    65,    66,    -1,    29,    30,    31,
      71,    33,    -1,    35,    -1,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    50,    -1,
      -1,    53,    -1,    -1,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    64,    65,    66,    -1,    29,    30,    31,    71,
      33,    -1,    35,    33,    -1,    35,    39,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    -1,    -1,
      53,    51,    -1,    53,    57,    55,    56,    57,    -1,    -1,
      -1,    64,    65,    66,    64,    65,    66,    33,    71,    35,
      -1,    71,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    51,    33,    53,    35,    55,
      56,    57,    39,    -1,    -1,    -1,    -1,    -1,    64,    65,
      66,    -1,    -1,    -1,    51,    71,    53,    -1,    55,    56,
      57,    -1,    -1,    -1,    33,    -1,    35,    64,    65,    66,
      39,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    33,    53,    35,    55,    56,    57,    39,
      -1,    -1,    42,    -1,    -1,    64,    65,    66,    -1,    33,
      -1,    35,    71,    53,    54,    39,    -1,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    64,    65,    66,    51,    -1,    53,
      54,    71,    -1,    57,    -1,    -1,    -1,    33,    34,    35,
      64,    65,    66,    39,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,    33,    -1,    35,    -1,    53,    -1,    39,
      -1,    57,    42,    -1,    60,    -1,    -1,    -1,    64,    65,
      66,    -1,    33,    53,    35,    71,    -1,    57,    39,    -1,
      -1,    -1,    -1,    -1,    64,    65,    66,    -1,    33,    -1,
      35,    71,    53,    54,    39,    -1,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    64,    65,    66,    51,    -1,    53,    33,
      71,    35,    57,    -1,    -1,    39,    -1,    -1,    -1,    64,
      65,    66,    -1,    -1,    -1,    -1,    71,    -1,    52,    53,
      33,    -1,    35,    57,    -1,    -1,    39,    -1,    41,    -1,
      64,    65,    66,    -1,    -1,    -1,    33,    71,    35,    33,
      53,    35,    39,    -1,    57,    39,    -1,    -1,    -1,    -1,
      -1,    64,    65,    66,    51,    33,    53,    35,    71,    53,
      57,    39,    -1,    57,    -1,    -1,    -1,    64,    65,    66,
      64,    65,    66,    -1,    71,    53,    33,    71,    35,    57,
      37,    -1,    39,    -1,    -1,    -1,    64,    65,    66,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    53,    -1,    55,    56,
      33,    -1,    35,    -1,    37,    62,    39,    64,    65,    66,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      53,    -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    62,
      -1,    64,    65,    66
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,     3,     4,     5,    74,     1,     6,    10,    14,
      23,    26,    27,    29,    30,    31,    33,    35,    37,    39,
      40,    41,    46,    48,    50,    53,    57,    64,    65,    66,
      71,   185,   186,   187,   188,   189,   190,   191,   192,   193,
     197,   198,   218,   220,   222,   223,   224,   225,    35,    39,
      41,    53,   130,   132,   138,   220,   221,    62,    75,    76,
       0,   185,   185,    69,   162,    69,    69,    29,    30,    31,
      50,    53,    57,    71,   176,   177,   196,   218,   222,   225,
     192,   193,   198,    32,    34,    36,    38,    48,    52,    55,
      56,    60,    61,    72,   185,   192,   199,   200,   201,   219,
     220,   227,   229,   231,   232,   233,    59,   185,   199,   214,
      13,   161,    43,    32,    34,    36,    38,    48,    52,    61,
      72,   230,   231,   233,   235,   235,   198,    69,    44,    69,
      43,    56,   132,   133,   138,   141,    53,    57,    71,   139,
     140,   220,   221,    46,     1,    77,   221,    69,   236,     7,
      11,    58,   194,   163,   164,    15,    14,    58,   186,   208,
     209,   210,   208,   176,    29,    32,    34,    38,    48,    52,
      61,    72,   166,   167,   168,   169,   171,   172,   174,   175,
     177,   178,   183,   184,   218,   220,   222,    48,   166,   167,
     168,   179,    51,   176,    44,    69,    56,    56,    56,    56,
      56,    56,    35,    37,    39,   220,    56,    56,    55,    56,
     235,    55,    56,    47,    55,    56,    55,    56,    42,   187,
     188,   187,    45,    47,    55,   215,    59,   162,     1,    53,
     127,   129,   130,   134,   135,   137,   138,   140,   220,    48,
     192,   193,    48,   192,   193,    53,   211,   212,   213,   222,
     223,   198,   211,    53,   134,   138,    55,    56,    55,    56,
      55,    56,    51,    56,   135,   138,   141,   142,   143,   144,
     219,   220,    59,   134,   220,    53,    80,     8,     9,    16,
      17,    18,    19,    20,    21,    24,    25,    29,    53,    58,
      63,    67,    68,    79,    86,    88,    96,    97,   145,   148,
     152,   153,   154,   155,   168,   169,   172,   174,   222,    79,
      69,   185,    11,    12,    58,   195,     1,    41,    58,    70,
     152,   165,   237,    58,   237,   185,   162,   208,    49,   237,
      58,   237,    60,   233,     1,   173,   175,   176,    55,    56,
      56,    43,   233,   233,   176,    55,    56,    47,    55,    56,
     176,    42,    72,   233,    55,    59,   185,   176,   180,   181,
     182,   222,   223,    60,    60,    60,    60,   185,    56,   185,
     185,   201,   220,   185,    56,    56,   185,    14,   185,   216,
     217,   185,    47,    28,    53,    56,   126,   127,   131,   132,
     138,   141,   220,    51,    54,    51,   139,    51,   192,   193,
     192,   193,    36,    48,    70,    55,    42,    70,   132,   132,
     138,   132,   138,   132,   138,    56,    55,    56,    55,    56,
      55,    56,    55,    56,    47,    55,    56,    56,    43,    59,
      35,    39,    53,    55,    56,    62,    81,    82,   221,   223,
     225,    13,     1,   130,   138,     1,    35,    98,     1,    29,
     146,     1,   146,     1,   146,     1,    53,   113,   114,   222,
       1,   130,   138,     1,    53,     1,   115,   130,   138,     1,
     116,   130,   138,    48,    60,    72,   226,   229,   154,   155,
     168,   222,    79,     1,    31,    65,    78,   221,     1,    29,
      63,   222,    70,    58,    87,    58,    43,    55,     1,    42,
      43,    47,   156,   157,   158,   159,   176,   156,   226,   226,
      48,    72,   176,   228,   229,   237,    58,   166,   202,   203,
     204,   185,   185,    12,     1,    42,   185,   210,    48,   173,
     166,   134,    48,   173,    48,   173,   166,   166,   184,   220,
     166,    29,    48,   173,   166,    70,    55,    42,    56,   162,
      49,    55,    45,   216,   128,   220,   133,   138,   141,    56,
      54,    55,    56,    55,    56,   134,   134,   134,   134,   213,
     185,   134,   135,   138,   135,   138,   134,   134,   144,   220,
     134,    56,    78,    55,    56,    53,     1,    69,    54,    42,
      42,   220,    60,   147,   226,   232,   234,   147,   147,    43,
      55,    31,    54,    42,   117,   118,   134,    47,   119,    54,
      13,   160,    54,   169,   222,    56,    56,    56,    78,    53,
      64,    66,    89,   222,   222,    87,    88,    97,   152,     1,
      28,   123,   124,   125,   127,   130,   136,   137,   138,   222,
     185,   134,   187,   160,   159,   169,   169,    29,   170,   171,
     222,   169,   202,     1,    47,    51,   205,   206,   207,   237,
      58,   185,   185,   173,   173,   173,    56,   173,   182,   166,
     185,   217,   185,    61,   220,    56,   134,    56,    56,    82,
      45,    55,    83,    84,    85,   223,   225,    79,     1,    98,
       1,    28,    52,    53,   101,   102,   104,   105,   127,   137,
     138,   224,   134,    55,   123,   114,    98,   102,   107,    56,
      55,     1,   120,   121,   122,   160,   138,    69,   149,   138,
     176,   176,   176,    66,    89,    35,    55,    90,    91,    92,
     222,    53,    78,    31,   222,    31,    96,   128,    51,    54,
      51,    51,   156,    42,   187,   185,   160,   207,   204,   129,
      56,    56,    56,    55,   237,    42,   128,   136,   137,   138,
      34,    22,    47,   110,    52,   127,   139,    60,   232,   232,
      52,   127,   232,    69,    15,   234,    42,   110,   134,    55,
      51,   220,   150,   151,    78,    53,    56,    55,    90,    89,
     222,    31,    43,   222,   222,    61,   125,   138,   125,   125,
     125,   185,    51,    85,   101,    61,   232,    56,    53,   221,
     102,   139,    52,   106,   127,   136,   106,   139,   106,    70,
     108,   109,   148,   222,    99,   100,   222,   107,   121,   122,
      58,   152,   237,    58,   237,    89,    45,    53,    55,    93,
      94,    95,   222,   224,    92,    56,    43,   222,   123,    43,
      43,   124,   185,   110,    53,   103,   104,   130,   138,   106,
     111,   112,   221,   136,    55,    70,    43,    55,    43,   110,
      56,    56,    55,   123,    43,   123,   123,    54,    56,    55,
     109,    52,   126,   134,   100,   123,    95,   123,   104,   221,
     134
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
# if YYLTYPE_IS_TRIVIAL
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
#line 120 "parser.y"
    {inputExpr = letrec((yyvsp[(3) - (3)]),(yyvsp[(2) - (3)])); sp-=2;}
    break;

  case 3:
#line 121 "parser.y"
    {inputContext = (yyvsp[(2) - (2)]);	    sp-=1;}
    break;

  case 4:
#line 122 "parser.y"
    {valDefns  = (yyvsp[(2) - (2)]);	    sp-=1;}
    break;

  case 5:
#line 123 "parser.y"
    {syntaxError("input");}
    break;

  case 6:
#line 136 "parser.y"
    {
					 setExportList(singleton(ap(MODULEENT,mkCon(module(currentModule).text))));
					 (yyval) = gc3((yyvsp[(3) - (4)]));
					}
    break;

  case 7:
#line 140 "parser.y"
    {
					 setExportList(singleton(ap(MODULEENT,mkCon(module(currentModule).text))));
					 (yyval) = gc4((yyvsp[(3) - (4)]));
					}
    break;

  case 8:
#line 145 "parser.y"
    {setExportList((yyvsp[(3) - (7)]));   (yyval) = gc7((yyvsp[(6) - (7)]));}
    break;

  case 9:
#line 147 "parser.y"
    {syntaxError("declaration");}
    break;

  case 10:
#line 148 "parser.y"
    {syntaxError("module definition");}
    break;

  case 11:
#line 154 "parser.y"
    {startModule(conMain); 
					 (yyval) = gc0(NIL);}
    break;

  case 12:
#line 157 "parser.y"
    {startModule(mkCon(mkNestedQual((yyvsp[(1) - (1)])))); (yyval) = gc1(NIL);}
    break;

  case 13:
#line 159 "parser.y"
    {(yyval) = mkCon(mkNestedQual((yyvsp[(1) - (1)])));}
    break;

  case 14:
#line 160 "parser.y"
    { String modName = findPathname(textToStr(textOf((yyvsp[(1) - (1)]))));
					  if (modName) { /* fillin pathname if known */
					      (yyval) = mkStr(findText(modName));
					  } else {
					      (yyval) = (yyvsp[(1) - (1)]);
					  }
					}
    break;

  case 15:
#line 168 "parser.y"
    {(yyval) = gc0(NIL); }
    break;

  case 16:
#line 169 "parser.y"
    {(yyval) = gc2((yyvsp[(2) - (2)]));}
    break;

  case 17:
#line 170 "parser.y"
    {(yyval) = gc1((yyvsp[(1) - (1)]));}
    break;

  case 18:
#line 171 "parser.y"
    {(yyval) = gc2(NIL);}
    break;

  case 19:
#line 172 "parser.y"
    {(yyval) = gc4((yyvsp[(4) - (4)]));}
    break;

  case 20:
#line 177 "parser.y"
    {(yyval) = gc0(exportSelf());}
    break;

  case 21:
#line 178 "parser.y"
    {(yyval) = gc2(NIL);}
    break;

  case 22:
#line 179 "parser.y"
    {(yyval) = gc3(NIL);}
    break;

  case 23:
#line 180 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 24:
#line 181 "parser.y"
    {(yyval) = gc4((yyvsp[(2) - (4)]));}
    break;

  case 25:
#line 183 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 26:
#line 184 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 27:
#line 189 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 28:
#line 190 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 29:
#line 191 "parser.y"
    {(yyval) = gc4(pair((yyvsp[(1) - (4)]),DOTDOT));}
    break;

  case 30:
#line 192 "parser.y"
    {(yyval) = gc4(pair((yyvsp[(1) - (4)]),(yyvsp[(3) - (4)])));}
    break;

  case 31:
#line 193 "parser.y"
    {(yyval) = gc2(ap(MODULEENT,(yyvsp[(2) - (2)])));}
    break;

  case 32:
#line 195 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 33:
#line 196 "parser.y"
    {(yyval) = gc1(NIL);}
    break;

  case 34:
#line 197 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 35:
#line 198 "parser.y"
    {(yyval) = gc2((yyvsp[(1) - (2)]));}
    break;

  case 36:
#line 200 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 37:
#line 201 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 38:
#line 203 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 39:
#line 204 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 40:
#line 209 "parser.y"
    {imps = cons((yyvsp[(3) - (3)]),imps); (yyval)=gc3(NIL);}
    break;

  case 41:
#line 210 "parser.y"
    {(yyval)   = gc2(NIL); }
    break;

  case 42:
#line 211 "parser.y"
    {imps = singleton((yyvsp[(1) - (1)])); (yyval)=gc1(NIL);}
    break;

  case 43:
#line 213 "parser.y"
    {if (chase(imps)) {
					     clearStack();
					     onto(imps);
					     done();
					     closeAnyInput();
					     return 0;
					 }
					 (yyval) = gc0(NIL);
					}
    break;

  case 44:
#line 224 "parser.y"
    {addUnqualImport((yyvsp[(2) - (3)]),NIL,(yyvsp[(3) - (3)]));
					 (yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 45:
#line 227 "parser.y"
    {addUnqualImport((yyvsp[(2) - (5)]),(yyvsp[(4) - (5)]),(yyvsp[(5) - (5)]));
					 (yyval) = gc5((yyvsp[(2) - (5)]));}
    break;

  case 46:
#line 230 "parser.y"
    {addQualImport((yyvsp[(3) - (6)]),(yyvsp[(5) - (6)]),(yyvsp[(6) - (6)]));
					 (yyval) = gc6((yyvsp[(3) - (6)]));}
    break;

  case 47:
#line 233 "parser.y"
    {addQualImport((yyvsp[(3) - (4)]),(yyvsp[(3) - (4)]),(yyvsp[(4) - (4)]));
					 (yyval) = gc4((yyvsp[(3) - (4)]));}
    break;

  case 48:
#line 235 "parser.y"
    {syntaxError("import declaration");}
    break;

  case 49:
#line 237 "parser.y"
    {(yyval) = gc0(DOTDOT);}
    break;

  case 50:
#line 238 "parser.y"
    {(yyval) = gc4(ap(HIDDEN,(yyvsp[(3) - (4)])));}
    break;

  case 51:
#line 239 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 52:
#line 241 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 53:
#line 242 "parser.y"
    {(yyval) = gc1(NIL);}
    break;

  case 54:
#line 243 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 55:
#line 244 "parser.y"
    {(yyval) = gc2((yyvsp[(1) - (2)]));}
    break;

  case 56:
#line 246 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 57:
#line 247 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 58:
#line 249 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 59:
#line 250 "parser.y"
    {(yyval) = gc1(pair((yyvsp[(1) - (1)]),NONE));}
    break;

  case 60:
#line 251 "parser.y"
    {(yyval) = gc4(pair((yyvsp[(1) - (4)]),DOTDOT));}
    break;

  case 61:
#line 252 "parser.y"
    {(yyval) = gc4(pair((yyvsp[(1) - (4)]),(yyvsp[(3) - (4)])));}
    break;

  case 62:
#line 254 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 63:
#line 255 "parser.y"
    {(yyval) = gc1(NIL);}
    break;

  case 64:
#line 256 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 65:
#line 257 "parser.y"
    {(yyval) = gc2((yyvsp[(1) - (2)]));}
    break;

  case 66:
#line 259 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 67:
#line 260 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 68:
#line 262 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 69:
#line 263 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 70:
#line 268 "parser.y"
    {(yyval) = gc2((yyvsp[(1) - (2)]));}
    break;

  case 71:
#line 269 "parser.y"
    {(yyval) = gc2((yyvsp[(1) - (3)]));}
    break;

  case 72:
#line 270 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 73:
#line 271 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 74:
#line 272 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 75:
#line 277 "parser.y"
    {defTycon(4,(yyvsp[(3) - (4)]),(yyvsp[(2) - (4)]),(yyvsp[(4) - (4)]),SYNONYM);}
    break;

  case 76:
#line 279 "parser.y"
    {defTycon(6,(yyvsp[(3) - (6)]),(yyvsp[(2) - (6)]),
						    ap((yyvsp[(4) - (6)]),(yyvsp[(6) - (6)])),RESTRICTSYN);}
    break;

  case 77:
#line 281 "parser.y"
    {syntaxError("type declaration");}
    break;

  case 78:
#line 283 "parser.y"
    {defTycon(5,(yyvsp[(3) - (5)]),checkTyLhs((yyvsp[(2) - (5)])),
						    ap(rev((yyvsp[(4) - (5)])),(yyvsp[(5) - (5)])),DATATYPE);}
    break;

  case 79:
#line 286 "parser.y"
    {defTycon(7,(yyvsp[(5) - (7)]),(yyvsp[(4) - (7)]),
						  ap(qualify((yyvsp[(2) - (7)]),rev((yyvsp[(6) - (7)]))),
						     (yyvsp[(7) - (7)])),DATATYPE);}
    break;

  case 80:
#line 289 "parser.y"
    {defTycon(2,(yyvsp[(1) - (2)]),checkTyLhs((yyvsp[(2) - (2)])),
						    ap(NIL,NIL),DATATYPE);}
    break;

  case 81:
#line 291 "parser.y"
    {defTycon(4,(yyvsp[(1) - (4)]),(yyvsp[(4) - (4)]),
						  ap(qualify((yyvsp[(2) - (4)]),NIL),
						     NIL),DATATYPE);}
    break;

  case 82:
#line 294 "parser.y"
    {syntaxError("data declaration");}
    break;

  case 83:
#line 296 "parser.y"
    {defTycon(5,(yyvsp[(3) - (5)]),checkTyLhs((yyvsp[(2) - (5)])),
						    ap((yyvsp[(4) - (5)]),(yyvsp[(5) - (5)])),NEWTYPE);}
    break;

  case 84:
#line 299 "parser.y"
    {defTycon(7,(yyvsp[(5) - (7)]),(yyvsp[(4) - (7)]),
						  ap(qualify((yyvsp[(2) - (7)]),(yyvsp[(6) - (7)])),
						     (yyvsp[(7) - (7)])),NEWTYPE);}
    break;

  case 85:
#line 302 "parser.y"
    {syntaxError("newtype declaration");}
    break;

  case 86:
#line 303 "parser.y"
    {if (isInt((yyvsp[(2) - (2)]))) {
					     needPrims(intOf((yyvsp[(2) - (2)])), NULL);
					 } else {
					     syntaxError("needprims decl");
					 }
					 sp-=2;}
    break;

  case 87:
#line 309 "parser.y"
    {syntaxError("needprims decl");}
    break;

  case 88:
#line 311 "parser.y"
    {(yyval) = gc2(ap((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 89:
#line 312 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 90:
#line 313 "parser.y"
    {syntaxError("type defn lhs");}
    break;

  case 91:
#line 315 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 92:
#line 316 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 93:
#line 318 "parser.y"
    {(yyval) = gc3(sigdecl((yyvsp[(2) - (3)]),singleton((yyvsp[(1) - (3)])),
									(yyvsp[(3) - (3)])));}
    break;

  case 94:
#line 320 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 95:
#line 322 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 96:
#line 323 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 97:
#line 325 "parser.y"
    {(yyval) = gc4(ap(POLYTYPE,
						     pair(rev((yyvsp[(2) - (4)])),(yyvsp[(4) - (4)]))));}
    break;

  case 98:
#line 327 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 99:
#line 329 "parser.y"
    {(yyval) = gc3(qualify((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 100:
#line 330 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 101:
#line 332 "parser.y"
    {(yyval) = gc4(ap(ap((yyvsp[(3) - (4)]),bang((yyvsp[(2) - (4)]))),(yyvsp[(4) - (4)])));}
    break;

  case 102:
#line 333 "parser.y"
    {(yyval) = gc3(ap(ap((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 103:
#line 334 "parser.y"
    {(yyval) = gc3(ap(ap((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 104:
#line 335 "parser.y"
    {(yyval) = gc3(ap(ap((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 105:
#line 336 "parser.y"
    {(yyval) = checkConstr((yyvsp[(1) - (1)]));}
    break;

  case 106:
#line 337 "parser.y"
    {(yyval) = checkConstr((yyvsp[(1) - (1)]));}
    break;

  case 107:
#line 338 "parser.y"
    {(yyval) = gc4(ap(LABC,pair((yyvsp[(1) - (4)]),rev((yyvsp[(3) - (4)])))));}
    break;

  case 108:
#line 339 "parser.y"
    {(yyval) = gc3(ap(LABC,pair((yyvsp[(1) - (3)]),NIL)));}
    break;

  case 109:
#line 340 "parser.y"
    {syntaxError("data type declaration");}
    break;

  case 110:
#line 342 "parser.y"
    {(yyval) = gc3(ap((yyvsp[(1) - (3)]),bang((yyvsp[(3) - (3)]))));}
    break;

  case 111:
#line 343 "parser.y"
    {(yyval) = gc2(ap((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 112:
#line 344 "parser.y"
    {(yyval) = gc2(ap((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 113:
#line 345 "parser.y"
    {(yyval) = gc3(ap((yyvsp[(1) - (3)]),bang((yyvsp[(3) - (3)]))));}
    break;

  case 114:
#line 346 "parser.y"
    {(yyval) = gc2(ap((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 115:
#line 347 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 116:
#line 349 "parser.y"
    {(yyval) = gc2(bang((yyvsp[(2) - (2)])));}
    break;

  case 117:
#line 350 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 118:
#line 351 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 119:
#line 353 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 120:
#line 355 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 121:
#line 356 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 122:
#line 358 "parser.y"
    {(yyval) = gc3(pair(rev((yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 123:
#line 359 "parser.y"
    {(yyval) = gc3(pair(rev((yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 124:
#line 360 "parser.y"
    {(yyval) = gc4(pair(rev((yyvsp[(1) - (4)])),bang((yyvsp[(4) - (4)]))));}
    break;

  case 125:
#line 362 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 126:
#line 363 "parser.y"
    {(yyval) = gc2(singleton((yyvsp[(2) - (2)])));}
    break;

  case 127:
#line 364 "parser.y"
    {(yyval) = gc4((yyvsp[(3) - (4)]));}
    break;

  case 128:
#line 366 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 129:
#line 367 "parser.y"
    {(yyval) = gc1(rev((yyvsp[(1) - (1)])));}
    break;

  case 130:
#line 369 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 131:
#line 370 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 132:
#line 375 "parser.y"
    {primDefn((yyvsp[(1) - (4)]),(yyvsp[(2) - (4)]),(yyvsp[(4) - (4)])); sp-=4;}
    break;

  case 133:
#line 377 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 134:
#line 378 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 135:
#line 379 "parser.y"
    {syntaxError("primitive defn");}
    break;

  case 136:
#line 381 "parser.y"
    {(yyval) = gc2(pair((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 137:
#line 382 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 138:
#line 388 "parser.y"
    {foreignImport((yyvsp[(1) - (7)]),(yyvsp[(3) - (7)]),NIL,(yyvsp[(4) - (7)]),(yyvsp[(5) - (7)]),(yyvsp[(7) - (7)])); sp-=7;}
    break;

  case 139:
#line 390 "parser.y"
    {foreignImport((yyvsp[(1) - (6)]),(yyvsp[(3) - (6)]),NIL,(yyvsp[(4) - (6)]),(yyvsp[(4) - (6)]),(yyvsp[(6) - (6)])); sp-=6;}
    break;

  case 140:
#line 392 "parser.y"
    {foreignImport((yyvsp[(1) - (8)]),(yyvsp[(3) - (8)]),(yyvsp[(4) - (8)]),(yyvsp[(5) - (8)]),(yyvsp[(6) - (8)]),(yyvsp[(8) - (8)])); sp-=8;}
    break;

  case 141:
#line 394 "parser.y"
    {foreignImport((yyvsp[(1) - (7)]),(yyvsp[(3) - (7)]),(yyvsp[(4) - (7)]),(yyvsp[(5) - (7)]),(yyvsp[(5) - (7)]),(yyvsp[(7) - (7)])); sp-=7;}
    break;

  case 142:
#line 396 "parser.y"
    {foreignExport((yyvsp[(1) - (7)]),(yyvsp[(2) - (7)]),(yyvsp[(3) - (7)]),(yyvsp[(4) - (7)]),(yyvsp[(5) - (7)]),(yyvsp[(7) - (7)])); sp-=7;}
    break;

  case 143:
#line 401 "parser.y"
    {classDefn(intOf((yyvsp[(1) - (4)])),(yyvsp[(2) - (4)]),(yyvsp[(4) - (4)]),(yyvsp[(3) - (4)])); sp-=4;}
    break;

  case 144:
#line 402 "parser.y"
    {instDefn(intOf((yyvsp[(1) - (3)])),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)]));  sp-=3;}
    break;

  case 145:
#line 403 "parser.y"
    {defaultDefn(intOf((yyvsp[(1) - (4)])),(yyvsp[(3) - (4)]));  sp-=4;}
    break;

  case 146:
#line 404 "parser.y"
    {syntaxError("class declaration");}
    break;

  case 147:
#line 405 "parser.y"
    {syntaxError("instance declaration");}
    break;

  case 148:
#line 406 "parser.y"
    {syntaxError("default declaration");}
    break;

  case 149:
#line 408 "parser.y"
    {(yyval) = gc3(pair((yyvsp[(1) - (3)]),checkPred((yyvsp[(3) - (3)]))));}
    break;

  case 150:
#line 409 "parser.y"
    {(yyval) = gc1(pair(NIL,checkPred((yyvsp[(1) - (1)]))));}
    break;

  case 151:
#line 411 "parser.y"
    {(yyval) = gc3(pair((yyvsp[(1) - (3)]),checkPred((yyvsp[(3) - (3)]))));}
    break;

  case 152:
#line 412 "parser.y"
    {(yyval) = gc1(pair(NIL,checkPred((yyvsp[(1) - (1)]))));}
    break;

  case 153:
#line 414 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 154:
#line 415 "parser.y"
    {(yyval) = gc1(rev((yyvsp[(1) - (1)])));}
    break;

  case 155:
#line 417 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 156:
#line 418 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 157:
#line 420 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 158:
#line 421 "parser.y"
    {h98DoesntSupport(row,"dependent parameters");
					 (yyval) = gc2(rev((yyvsp[(2) - (2)])));}
    break;

  case 159:
#line 424 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 160:
#line 425 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 161:
#line 427 "parser.y"
    {(yyval) = gc3(pair(rev((yyvsp[(1) - (3)])),rev((yyvsp[(3) - (3)]))));}
    break;

  case 162:
#line 428 "parser.y"
    {syntaxError("functional dependency");}
    break;

  case 163:
#line 430 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 164:
#line 431 "parser.y"
    {(yyval) = gc2(cons((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 165:
#line 436 "parser.y"
    {(yyval) = gc4(ap(POLYTYPE,
						     pair(rev((yyvsp[(2) - (4)])),(yyvsp[(4) - (4)]))));}
    break;

  case 166:
#line 438 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 167:
#line 440 "parser.y"
    {(yyval) = gc3(qualify((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 168:
#line 441 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 169:
#line 443 "parser.y"
    {(yyval) = gc3(fn((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 170:
#line 444 "parser.y"
    {(yyval) = gc3(fn((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 171:
#line 445 "parser.y"
    {(yyval) = gc3(fn((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 172:
#line 446 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 173:
#line 448 "parser.y"
    {(yyval) = gc4(ap(POLYTYPE,
						     pair(rev((yyvsp[(2) - (4)])),(yyvsp[(4) - (4)]))));}
    break;

  case 174:
#line 450 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 175:
#line 452 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 176:
#line 453 "parser.y"
    {(yyval) = gc5(qualify((yyvsp[(2) - (5)]),(yyvsp[(4) - (5)])));}
    break;

  case 177:
#line 455 "parser.y"
    {(yyval) = gc2(cons((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 178:
#line 456 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 179:
#line 458 "parser.y"
    {(yyval) = gc3(qualify((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 180:
#line 459 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 181:
#line 461 "parser.y"
    {(yyval) = gc2(NIL);}
    break;

  case 182:
#line 462 "parser.y"
    {(yyval) = gc1(singleton(checkPred((yyvsp[(1) - (1)]))));}
    break;

  case 183:
#line 463 "parser.y"
    {(yyval) = gc3(singleton(checkPred((yyvsp[(2) - (3)]))));}
    break;

  case 184:
#line 464 "parser.y"
    {(yyval) = gc3(checkCtxt(rev((yyvsp[(2) - (3)]))));}
    break;

  case 185:
#line 465 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 186:
#line 466 "parser.y"
    {(yyval) = gc3(checkCtxt(rev((yyvsp[(2) - (3)]))));}
    break;

  case 187:
#line 468 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 188:
#line 469 "parser.y"
    {(yyval) = gc3(checkCtxt(rev((yyvsp[(2) - (3)]))));}
    break;

  case 189:
#line 471 "parser.y"
    {
#if TREX
					 (yyval) = gc3(ap(mkExt(textOf((yyvsp[(3) - (3)]))),(yyvsp[(1) - (3)])));
#else
					 noTREX("a type context");
#endif
					}
    break;

  case 190:
#line 478 "parser.y"
    {
#if IPARAM
					 (yyval) = gc3(pair(mkIParam((yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));
#else
					 noIP("a type context");
#endif
					}
    break;

  case 191:
#line 486 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 192:
#line 487 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 193:
#line 488 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 194:
#line 489 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),cons((yyvsp[(1) - (3)]),NIL)));}
    break;

  case 195:
#line 490 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 196:
#line 493 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 197:
#line 494 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 198:
#line 496 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 199:
#line 497 "parser.y"
    {(yyval) = gc3(fn((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 200:
#line 498 "parser.y"
    {(yyval) = gc3(fn((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 201:
#line 499 "parser.y"
    {(yyval) = gc3(fn((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 202:
#line 500 "parser.y"
    {syntaxError("type expression");}
    break;

  case 203:
#line 502 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 204:
#line 503 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 205:
#line 505 "parser.y"
    {(yyval) = gc2(ap((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 206:
#line 506 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 207:
#line 508 "parser.y"
    {(yyval) = gc2(ap((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 208:
#line 509 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 209:
#line 511 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 210:
#line 512 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 211:
#line 514 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 212:
#line 515 "parser.y"
    {(yyval) = gc2(typeUnit);}
    break;

  case 213:
#line 516 "parser.y"
    {(yyval) = gc3(typeArrow);}
    break;

  case 214:
#line 517 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 215:
#line 518 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 216:
#line 519 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 217:
#line 520 "parser.y"
    {(yyval) = gc3(buildTuple((yyvsp[(2) - (3)])));}
    break;

  case 218:
#line 521 "parser.y"
    {(yyval) = gc3(buildTuple((yyvsp[(2) - (3)])));}
    break;

  case 219:
#line 522 "parser.y"
    {
#if TREX
					 (yyval) = gc3(revOnto((yyvsp[(2) - (3)]),typeNoRow));
#else
					 noTREX("a type");
#endif
					}
    break;

  case 220:
#line 529 "parser.y"
    {
#if TREX
					 (yyval) = gc5(revOnto((yyvsp[(2) - (5)]),(yyvsp[(4) - (5)])));
#else
					 noTREX("a type");
#endif
					}
    break;

  case 221:
#line 536 "parser.y"
    {(yyval) = gc3(ap(typeList,(yyvsp[(2) - (3)])));}
    break;

  case 222:
#line 537 "parser.y"
    {(yyval) = gc2(typeList);}
    break;

  case 223:
#line 538 "parser.y"
    {h98DoesntSupport(row,"anonymous type variables");
					 (yyval) = gc1(inventVar());}
    break;

  case 224:
#line 541 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 225:
#line 542 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),cons((yyvsp[(1) - (3)]),NIL)));}
    break;

  case 226:
#line 544 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),cons((yyvsp[(1) - (3)]),NIL)));}
    break;

  case 227:
#line 545 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),cons((yyvsp[(1) - (3)]),NIL)));}
    break;

  case 228:
#line 546 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 229:
#line 547 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 230:
#line 550 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 231:
#line 551 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 232:
#line 553 "parser.y"
    {h98DoesntSupport(row,"extensible records");
					 (yyval) = gc3(ap(mkExt(textOf((yyvsp[(1) - (3)]))),(yyvsp[(3) - (3)])));}
    break;

  case 233:
#line 560 "parser.y"
    {(yyval) = gc3(fixdecl((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]),NON_ASS,(yyvsp[(2) - (3)])));}
    break;

  case 234:
#line 561 "parser.y"
    {syntaxError("fixity decl");}
    break;

  case 235:
#line 562 "parser.y"
    {(yyval) = gc3(fixdecl((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]),LEFT_ASS,(yyvsp[(2) - (3)])));}
    break;

  case 236:
#line 563 "parser.y"
    {syntaxError("fixity decl");}
    break;

  case 237:
#line 564 "parser.y"
    {(yyval) = gc3(fixdecl((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]),RIGHT_ASS,(yyvsp[(2) - (3)])));}
    break;

  case 238:
#line 565 "parser.y"
    {syntaxError("fixity decl");}
    break;

  case 239:
#line 566 "parser.y"
    {(yyval) = gc3(sigdecl((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 240:
#line 567 "parser.y"
    {syntaxError("type signature");}
    break;

  case 241:
#line 569 "parser.y"
    {(yyval) = gc1(checkPrec((yyvsp[(1) - (1)])));}
    break;

  case 242:
#line 570 "parser.y"
    {(yyval) = gc0(mkInt(DEF_PREC));}
    break;

  case 243:
#line 572 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 244:
#line 573 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 245:
#line 575 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 246:
#line 576 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 247:
#line 578 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 248:
#line 579 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 249:
#line 581 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 250:
#line 582 "parser.y"
    {(yyval) = gc2((yyvsp[(1) - (2)]));}
    break;

  case 251:
#line 583 "parser.y"
    {(yyval) = gc2((yyvsp[(1) - (2)]));}
    break;

  case 252:
#line 585 "parser.y"
    {(yyval) = gc2(cons((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 253:
#line 587 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 254:
#line 588 "parser.y"
    {(yyval) = gc2(ap(FUNBIND,pair((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)]))));}
    break;

  case 255:
#line 589 "parser.y"
    {(yyval) = gc4(ap(FUNBIND,
						     pair((yyvsp[(1) - (4)]),ap(RSIGN,
								ap((yyvsp[(4) - (4)]),(yyvsp[(3) - (4)]))))));}
    break;

  case 256:
#line 592 "parser.y"
    {(yyval) = gc2(ap(PATBIND,pair((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)]))));}
    break;

  case 257:
#line 594 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 258:
#line 595 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 259:
#line 596 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 260:
#line 598 "parser.y"
    {(yyval) = gc3(ap2((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 261:
#line 599 "parser.y"
    {(yyval) = gc3(ap2((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 262:
#line 600 "parser.y"
    {(yyval) = gc3(ap2((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 263:
#line 601 "parser.y"
    {(yyval) = gc3(ap2((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 264:
#line 602 "parser.y"
    {(yyval) = gc3(ap2(varPlus,(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 265:
#line 604 "parser.y"
    {(yyval) = gc4(ap((yyvsp[(2) - (4)]),(yyvsp[(4) - (4)])));}
    break;

  case 266:
#line 605 "parser.y"
    {(yyval) = gc4(ap((yyvsp[(2) - (4)]),(yyvsp[(4) - (4)])));}
    break;

  case 267:
#line 606 "parser.y"
    {(yyval) = gc4(ap((yyvsp[(2) - (4)]),(yyvsp[(4) - (4)])));}
    break;

  case 268:
#line 607 "parser.y"
    {(yyval) = gc2(ap((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 269:
#line 608 "parser.y"
    {(yyval) = gc2(ap((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 270:
#line 610 "parser.y"
    {(yyval) = gc2(letrec((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 271:
#line 611 "parser.y"
    {syntaxError("declaration");}
    break;

  case 272:
#line 613 "parser.y"
    {(yyval) = gc2(pair((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 273:
#line 614 "parser.y"
    {(yyval) = gc1(grded(rev((yyvsp[(1) - (1)]))));}
    break;

  case 274:
#line 616 "parser.y"
    {(yyval) = gc2(cons((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 275:
#line 617 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 276:
#line 619 "parser.y"
    {(yyval) = gc4(pair((yyvsp[(3) - (4)]),pair((yyvsp[(2) - (4)]),(yyvsp[(4) - (4)]))));}
    break;

  case 277:
#line 621 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 278:
#line 622 "parser.y"
    {(yyval) = gc2((yyvsp[(2) - (2)]));}
    break;

  case 279:
#line 627 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 280:
#line 628 "parser.y"
    {(yyval) = gc2((yyvsp[(2) - (2)]));}
    break;

  case 281:
#line 631 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 282:
#line 632 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 283:
#line 635 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 284:
#line 636 "parser.y"
    {(yyval) = gc2((yyvsp[(1) - (2)]));}
    break;

  case 285:
#line 637 "parser.y"
    {(yyval) = gc2((yyvsp[(1) - (2)]));}
    break;

  case 286:
#line 640 "parser.y"
    {(yyval) = gc2(cons((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 287:
#line 642 "parser.y"
    {
#if IPARAM
				         (yyval) = gc3(pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));
#else
					 noIP("a binding");
#endif
					}
    break;

  case 288:
#line 649 "parser.y"
    {syntaxError("a binding");}
    break;

  case 289:
#line 650 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 290:
#line 655 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 291:
#line 656 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 292:
#line 658 "parser.y"
    {(yyval) = gc3(ap(ESIGN,pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]))));}
    break;

  case 293:
#line 659 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 294:
#line 661 "parser.y"
    {(yyval) = gc3(ap2(varPlus,(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 295:
#line 663 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 296:
#line 664 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 297:
#line 665 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 298:
#line 667 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 299:
#line 668 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 300:
#line 670 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 301:
#line 671 "parser.y"
    {(yyval) = gc1(ap(INFIX,(yyvsp[(1) - (1)])));}
    break;

  case 302:
#line 673 "parser.y"
    {(yyval) = gc2(ap(NEG,only((yyvsp[(2) - (2)]))));}
    break;

  case 303:
#line 674 "parser.y"
    {syntaxError("pattern");}
    break;

  case 304:
#line 675 "parser.y"
    {(yyval) = gc3(ap(ap((yyvsp[(2) - (3)]),only((yyvsp[(1) - (3)]))),(yyvsp[(3) - (3)])));}
    break;

  case 305:
#line 676 "parser.y"
    {(yyval) = gc4(ap(NEG,ap2((yyvsp[(2) - (4)]),only((yyvsp[(1) - (4)])),(yyvsp[(4) - (4)]))));}
    break;

  case 306:
#line 677 "parser.y"
    {(yyval) = gc3(ap(ap((yyvsp[(2) - (3)]),only((yyvsp[(1) - (3)]))),(yyvsp[(3) - (3)])));}
    break;

  case 307:
#line 678 "parser.y"
    {(yyval) = gc4(ap(NEG,ap2((yyvsp[(2) - (4)]),only((yyvsp[(1) - (4)])),(yyvsp[(4) - (4)]))));}
    break;

  case 308:
#line 679 "parser.y"
    {(yyval) = gc3(ap(ap((yyvsp[(2) - (3)]),only((yyvsp[(1) - (3)]))),(yyvsp[(3) - (3)])));}
    break;

  case 309:
#line 680 "parser.y"
    {(yyval) = gc4(ap(NEG,ap2((yyvsp[(2) - (4)]),only((yyvsp[(1) - (4)])),(yyvsp[(4) - (4)]))));}
    break;

  case 310:
#line 681 "parser.y"
    {(yyval) = gc3(ap(ap((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 311:
#line 682 "parser.y"
    {(yyval) = gc4(ap(NEG,ap(ap((yyvsp[(2) - (4)]),(yyvsp[(1) - (4)])),(yyvsp[(4) - (4)]))));}
    break;

  case 312:
#line 684 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 313:
#line 685 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 314:
#line 687 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 315:
#line 688 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 316:
#line 690 "parser.y"
    {(yyval) = gc2(ap((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 317:
#line 691 "parser.y"
    {(yyval) = gc2(ap((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 318:
#line 693 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 319:
#line 694 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 320:
#line 695 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 321:
#line 697 "parser.y"
    {(yyval) = gc3(ap(ASPAT,pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]))));}
    break;

  case 322:
#line 698 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 323:
#line 699 "parser.y"
    {(yyval) = gc4(ap(CONFLDS,pair((yyvsp[(1) - (4)]),(yyvsp[(3) - (4)]))));}
    break;

  case 324:
#line 700 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 325:
#line 701 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 326:
#line 702 "parser.y"
    {(yyval) = gc1(WILDCARD);}
    break;

  case 327:
#line 703 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 328:
#line 704 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 329:
#line 705 "parser.y"
    {(yyval) = gc3(buildTuple((yyvsp[(2) - (3)])));}
    break;

  case 330:
#line 706 "parser.y"
    {(yyval) = gc3(ap(FINLIST,rev((yyvsp[(2) - (3)]))));}
    break;

  case 331:
#line 707 "parser.y"
    {(yyval) = gc2(ap(LAZYPAT,(yyvsp[(2) - (2)])));}
    break;

  case 332:
#line 709 "parser.y"
    {
#if TREX
					 (yyval) = gc3(revOnto((yyvsp[(2) - (3)]),nameNoRec));
#else
					 (yyval) = gc3(NIL);
#endif
					}
    break;

  case 333:
#line 716 "parser.y"
    {(yyval) = gc5(revOnto((yyvsp[(2) - (5)]),(yyvsp[(4) - (5)])));}
    break;

  case 334:
#line 719 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 335:
#line 720 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),singleton((yyvsp[(1) - (3)]))));}
    break;

  case 336:
#line 722 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 337:
#line 723 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 338:
#line 725 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 339:
#line 726 "parser.y"
    {(yyval) = gc1(rev((yyvsp[(1) - (1)])));}
    break;

  case 340:
#line 728 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 341:
#line 729 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 342:
#line 731 "parser.y"
    {(yyval) = gc3(pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 343:
#line 732 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 344:
#line 735 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 345:
#line 736 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 346:
#line 738 "parser.y"
    {
#if TREX
					 (yyval) = gc3(ap(mkExt(textOf((yyvsp[(1) - (3)]))),(yyvsp[(3) - (3)])));
#else
					 noTREX("a pattern");
#endif
					}
    break;

  case 347:
#line 750 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 348:
#line 751 "parser.y"
    {syntaxError("expression");}
    break;

  case 349:
#line 753 "parser.y"
    {(yyval) = gc3(ap(ESIGN,pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]))));}
    break;

  case 350:
#line 754 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 351:
#line 756 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 352:
#line 757 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 353:
#line 759 "parser.y"
    {(yyval) = gc1(ap(INFIX,(yyvsp[(1) - (1)])));}
    break;

  case 354:
#line 760 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 355:
#line 762 "parser.y"
    {(yyval) = gc1(ap(INFIX,(yyvsp[(1) - (1)])));}
    break;

  case 356:
#line 763 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 357:
#line 765 "parser.y"
    {(yyval) = gc4(ap(NEG,ap(ap((yyvsp[(2) - (4)]),(yyvsp[(1) - (4)])),(yyvsp[(4) - (4)]))));}
    break;

  case 358:
#line 766 "parser.y"
    {(yyval) = gc3(ap(ap((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 359:
#line 767 "parser.y"
    {(yyval) = gc2(ap(NEG,only((yyvsp[(2) - (2)]))));}
    break;

  case 360:
#line 768 "parser.y"
    {(yyval) = gc4(ap(NEG,
						     ap(ap((yyvsp[(2) - (4)]),only((yyvsp[(1) - (4)]))),(yyvsp[(4) - (4)]))));}
    break;

  case 361:
#line 770 "parser.y"
    {(yyval) = gc3(ap(ap((yyvsp[(2) - (3)]),only((yyvsp[(1) - (3)]))),(yyvsp[(3) - (3)])));}
    break;

  case 362:
#line 772 "parser.y"
    {(yyval) = gc4(ap(NEG,ap(ap((yyvsp[(2) - (4)]),(yyvsp[(1) - (4)])),(yyvsp[(4) - (4)]))));}
    break;

  case 363:
#line 773 "parser.y"
    {(yyval) = gc3(ap(ap((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 364:
#line 774 "parser.y"
    {(yyval) = gc2(ap(NEG,only((yyvsp[(2) - (2)]))));}
    break;

  case 365:
#line 775 "parser.y"
    {(yyval) = gc4(ap(NEG,
						     ap(ap((yyvsp[(2) - (4)]),only((yyvsp[(1) - (4)]))),(yyvsp[(4) - (4)]))));}
    break;

  case 366:
#line 777 "parser.y"
    {(yyval) = gc3(ap(ap((yyvsp[(2) - (3)]),only((yyvsp[(1) - (3)]))),(yyvsp[(3) - (3)])));}
    break;

  case 367:
#line 779 "parser.y"
    {(yyval) = gc6(ap(CASE,pair((yyvsp[(2) - (6)]),rev((yyvsp[(5) - (6)])))));}
    break;

  case 368:
#line 780 "parser.y"
    {(yyval) = gc4(ap(DOCOMP,checkDo((yyvsp[(3) - (4)]))));}
    break;

  case 369:
#line 781 "parser.y"
    {
#if MUDO
					 (yyval) = gc4(ap(MDOCOMP, checkMDo((yyvsp[(3) - (4)]))));
#else
					 noMDo("an expression");
#endif
					}
    break;

  case 370:
#line 788 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 371:
#line 790 "parser.y"
    {(yyval) = gc4(ap(LAMBDA,      
						     pair(rev((yyvsp[(2) - (4)])),
							  pair((yyvsp[(3) - (4)]),(yyvsp[(4) - (4)])))));}
    break;

  case 372:
#line 793 "parser.y"
    {(yyval) = gc4(letrec((yyvsp[(2) - (4)]),(yyvsp[(4) - (4)])));}
    break;

  case 373:
#line 794 "parser.y"
    {(yyval) = gc4(ap(COND,triple((yyvsp[(2) - (4)]),(yyvsp[(3) - (4)]),(yyvsp[(4) - (4)]))));}
    break;

  case 374:
#line 799 "parser.y"
    {(yyval) = gc3((yyvsp[(3) - (3)]));}
    break;

  case 375:
#line 800 "parser.y"
    {(yyval) = gc2((yyvsp[(2) - (2)]));}
    break;

  case 376:
#line 802 "parser.y"
    {(yyval) = gc3((yyvsp[(3) - (3)]));}
    break;

  case 377:
#line 803 "parser.y"
    {(yyval) = gc2((yyvsp[(2) - (2)]));}
    break;

  case 378:
#line 806 "parser.y"
    {(yyval) = gc2(cons((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 379:
#line 807 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 380:
#line 809 "parser.y"
    {(yyval) = gc2(ap((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 381:
#line 810 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 382:
#line 812 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 383:
#line 813 "parser.y"
    {(yyval) = gc3(ap(ASPAT,pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]))));}
    break;

  case 384:
#line 814 "parser.y"
    {(yyval) = gc2(ap(LAZYPAT,(yyvsp[(2) - (2)])));}
    break;

  case 385:
#line 815 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 386:
#line 816 "parser.y"
    {(yyval) = gc1(WILDCARD);}
    break;

  case 387:
#line 817 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 388:
#line 818 "parser.y"
    {(yyval) = gc4(ap(CONFLDS,pair((yyvsp[(1) - (4)]),(yyvsp[(3) - (4)]))));}
    break;

  case 389:
#line 819 "parser.y"
    {(yyval) = gc4(ap(UPDFLDS,
						     triple((yyvsp[(1) - (4)]),NIL,(yyvsp[(3) - (4)]))));}
    break;

  case 390:
#line 821 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 391:
#line 822 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 392:
#line 823 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 393:
#line 824 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 394:
#line 825 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 395:
#line 826 "parser.y"
    {(yyval) = gc3(buildTuple((yyvsp[(2) - (3)])));}
    break;

  case 396:
#line 828 "parser.y"
    {
#if TREX
					 (yyval) = gc3(revOnto((yyvsp[(2) - (3)]),nameNoRec));
#else
					 (yyval) = gc3(NIL);
#endif
					}
    break;

  case 397:
#line 835 "parser.y"
    {(yyval) = gc5(revOnto((yyvsp[(2) - (5)]),(yyvsp[(4) - (5)])));}
    break;

  case 398:
#line 836 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 399:
#line 838 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 400:
#line 839 "parser.y"
    {(yyval) = gc4(ap((yyvsp[(3) - (4)]),(yyvsp[(2) - (4)])));}
    break;

  case 401:
#line 840 "parser.y"
    {(yyval) = gc4(ap(ap(nameFlip,(yyvsp[(2) - (4)])),(yyvsp[(3) - (4)])));}
    break;

  case 402:
#line 841 "parser.y"
    {(yyval) = gc4(ap(ap(nameFlip,(yyvsp[(2) - (4)])),(yyvsp[(3) - (4)])));}
    break;

  case 403:
#line 843 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 404:
#line 844 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),cons((yyvsp[(1) - (3)]),NIL)));}
    break;

  case 405:
#line 847 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 406:
#line 848 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 407:
#line 850 "parser.y"
    {
#if TREX
					 (yyval) = gc3(ap(mkExt(textOf((yyvsp[(1) - (3)]))),(yyvsp[(3) - (3)])));
#else
					 noTREX("an expression");
#endif
					}
    break;

  case 408:
#line 859 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 409:
#line 860 "parser.y"
    {(yyval) = gc2((yyvsp[(2) - (2)]));}
    break;

  case 410:
#line 862 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 411:
#line 863 "parser.y"
    {(yyval) = gc2((yyvsp[(1) - (2)]));}
    break;

  case 412:
#line 864 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 413:
#line 866 "parser.y"
    {(yyval) = gc3(pair((yyvsp[(1) - (3)]),letrec((yyvsp[(3) - (3)]),(yyvsp[(2) - (3)]))));}
    break;

  case 414:
#line 868 "parser.y"
    {(yyval) = gc1(grded(rev((yyvsp[(1) - (1)]))));}
    break;

  case 415:
#line 869 "parser.y"
    {(yyval) = gc2(pair((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])));}
    break;

  case 416:
#line 870 "parser.y"
    {syntaxError("case expression");}
    break;

  case 417:
#line 872 "parser.y"
    {(yyval) = gc2(cons((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)])));}
    break;

  case 418:
#line 873 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 419:
#line 875 "parser.y"
    {(yyval) = gc4(pair((yyvsp[(3) - (4)]),pair((yyvsp[(2) - (4)]),(yyvsp[(4) - (4)]))));}
    break;

  case 420:
#line 878 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 421:
#line 879 "parser.y"
    {(yyval) = gc2((yyvsp[(2) - (2)]));}
    break;

  case 422:
#line 881 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 423:
#line 882 "parser.y"
    {(yyval) = gc2((yyvsp[(1) - (2)]));}
    break;

  case 424:
#line 883 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 425:
#line 886 "parser.y"
    {(yyval) = gc3(ap(FROMQUAL,pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]))));}
    break;

  case 426:
#line 887 "parser.y"
    {(yyval) = gc2(ap(QWHERE,(yyvsp[(2) - (2)])));}
    break;

  case 427:
#line 889 "parser.y"
    {(yyval) = gc1(ap(DOQUAL,(yyvsp[(1) - (1)])));}
    break;

  case 428:
#line 891 "parser.y"
    {(yyval) = gc0(NIL);}
    break;

  case 429:
#line 892 "parser.y"
    {(yyval) = gc1(rev((yyvsp[(1) - (1)])));}
    break;

  case 430:
#line 894 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 431:
#line 895 "parser.y"
    {(yyval) = gc1(singleton((yyvsp[(1) - (1)])));}
    break;

  case 432:
#line 897 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 433:
#line 898 "parser.y"
    {(yyval) = gc3(pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])));}
    break;

  case 434:
#line 903 "parser.y"
    {(yyval) = gc1(ap(FINLIST,cons((yyvsp[(1) - (1)]),NIL)));}
    break;

  case 435:
#line 904 "parser.y"
    {(yyval) = gc1(ap(FINLIST,rev((yyvsp[(1) - (1)]))));}
    break;

  case 436:
#line 905 "parser.y"
    {
#if ZIP_COMP
					 if (length((yyvsp[(2) - (2)]))==1) {
					     (yyval) = gc2(ap(COMP,pair((yyvsp[(1) - (2)]),hd((yyvsp[(2) - (2)])))));
					 } else {
					     if (haskell98)
						 syntaxError("list comprehension");
					     (yyval) = gc2(ap(ZCOMP,pair((yyvsp[(1) - (2)]),rev((yyvsp[(2) - (2)])))));
					 }
#else
					 if (length((yyvsp[(2) - (2)]))!=1) {
					     syntaxError("list comprehension");
					 }
					 (yyval) = gc2(ap(COMP,pair((yyvsp[(1) - (2)]),hd((yyvsp[(2) - (2)])))));
#endif
					}
    break;

  case 437:
#line 921 "parser.y"
    {(yyval) = gc3(ap(ap(nameFromTo,(yyvsp[(1) - (3)])),(yyvsp[(3) - (3)])));}
    break;

  case 438:
#line 922 "parser.y"
    {(yyval) = gc4(ap(ap(nameFromThen,(yyvsp[(1) - (4)])),(yyvsp[(3) - (4)])));}
    break;

  case 439:
#line 923 "parser.y"
    {(yyval) = gc2(ap(nameFrom,(yyvsp[(1) - (2)])));}
    break;

  case 440:
#line 924 "parser.y"
    {(yyval) = gc5(ap(ap(ap(nameFromThenTo,
								(yyvsp[(1) - (5)])),(yyvsp[(3) - (5)])),(yyvsp[(5) - (5)])));}
    break;

  case 441:
#line 927 "parser.y"
    {(yyval) = gc3(cons(rev((yyvsp[(3) - (3)])),(yyvsp[(1) - (3)])));}
    break;

  case 442:
#line 928 "parser.y"
    {(yyval) = gc2(cons(rev((yyvsp[(2) - (2)])),NIL));}
    break;

  case 443:
#line 930 "parser.y"
    {(yyval) = gc3(cons((yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])));}
    break;

  case 444:
#line 931 "parser.y"
    {(yyval) = gc1(cons((yyvsp[(1) - (1)]),NIL));}
    break;

  case 445:
#line 933 "parser.y"
    {(yyval) = gc3(ap(FROMQUAL,pair((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]))));}
    break;

  case 446:
#line 934 "parser.y"
    {(yyval) = gc1(ap(BOOLQUAL,(yyvsp[(1) - (1)])));}
    break;

  case 447:
#line 935 "parser.y"
    {(yyval) = gc2(ap(QWHERE,(yyvsp[(2) - (2)])));}
    break;

  case 448:
#line 940 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 449:
#line 941 "parser.y"
    {(yyval) = gc2(nameUnit);}
    break;

  case 450:
#line 942 "parser.y"
    {(yyval) = gc2(nameNil);}
    break;

  case 451:
#line 943 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 452:
#line 945 "parser.y"
    {(yyval) = gc2(mkTuple(tupleOf((yyvsp[(1) - (2)]))+1));}
    break;

  case 453:
#line 946 "parser.y"
    {(yyval) = gc1(mkTuple(2));}
    break;

  case 454:
#line 948 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 455:
#line 949 "parser.y"
    {(yyval) = gc1(varHiding);}
    break;

  case 456:
#line 950 "parser.y"
    {(yyval) = gc1(varQualified);}
    break;

  case 457:
#line 951 "parser.y"
    {(yyval) = gc1(varAsMod);}
    break;

  case 458:
#line 953 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 459:
#line 954 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 460:
#line 956 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 461:
#line 957 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 462:
#line 958 "parser.y"
    {(yyval) = gc3(varPlus);}
    break;

  case 463:
#line 959 "parser.y"
    {(yyval) = gc3(varMinus);}
    break;

  case 464:
#line 960 "parser.y"
    {(yyval) = gc3(varBang);}
    break;

  case 465:
#line 961 "parser.y"
    {(yyval) = gc3(varDot);}
    break;

  case 466:
#line 963 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 467:
#line 964 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 468:
#line 965 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 469:
#line 967 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 470:
#line 968 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 471:
#line 970 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 472:
#line 971 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 473:
#line 972 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 474:
#line 974 "parser.y"
    {(yyval) = gc1(varPlus);}
    break;

  case 475:
#line 975 "parser.y"
    {(yyval) = gc1(varMinus);}
    break;

  case 476:
#line 976 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 477:
#line 978 "parser.y"
    {(yyval) = gc1(varPlus);}
    break;

  case 478:
#line 979 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 479:
#line 981 "parser.y"
    {(yyval) = gc1(varMinus);}
    break;

  case 480:
#line 982 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 481:
#line 984 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 482:
#line 985 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 483:
#line 986 "parser.y"
    {(yyval) = gc1(varBang);}
    break;

  case 484:
#line 987 "parser.y"
    {(yyval) = gc1(varDot);}
    break;

  case 485:
#line 989 "parser.y"
    {(yyval) = gc1(varMinus);}
    break;

  case 486:
#line 990 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 487:
#line 992 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 488:
#line 993 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 489:
#line 994 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 490:
#line 997 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 491:
#line 998 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 492:
#line 1000 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 493:
#line 1001 "parser.y"
    {(yyval) = gc3((yyvsp[(2) - (3)]));}
    break;

  case 494:
#line 1002 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 495:
#line 1004 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 496:
#line 1005 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 497:
#line 1007 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 498:
#line 1008 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 499:
#line 1013 "parser.y"
    {goOffside(startColumn);}
    break;

  case 500:
#line 1016 "parser.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 501:
#line 1017 "parser.y"
    {yyerrok; 
					 if (canUnOffside()) {
					     unOffside();
					     /* insert extra token on stack*/
					     push(NIL);
					     pushed(0) = pushed(1);
					     pushed(1) = mkInt(column);
					 }
					 else
					     syntaxError("declaration");
					}
    break;


/* Line 1267 of yacc.c.  */
#line 5619 "y.tab.c"
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


#line 1032 "parser.y"


static Cell local gcShadow(n,e)		/* keep parsed fragments on stack  */
Int  n;
Cell e; {
    /* If a look ahead token is held then the required stack transformation
     * is:
     *   pushed: n               1     0          1     0
     *           x1  |  ...  |  xn  |  la   ===>  e  |  la
     *                                top()            top()
     *
     * Othwerwise, the transformation is:
     *   pushed: n-1             0        0
     *           x1  |  ...  |  xn  ===>  e
     *                         top()     top()
     */
    if (yychar>=0) {
	pushed(n-1) = top();
	pushed(n)   = e;
    }
    else
	pushed(n-1) = e;
    sp -= (n-1);
    return e;
}

static Void local syntaxError(s)	/* report on syntax error	   */
String s; {
    ERRMSG(row) "Syntax error in %s (unexpected %s)", s, unexpected()
    EEND;
}

static String local unexpected() {     /* find name for unexpected token   */
    static char buffer[100];
    static char *fmt = "%s \"%s\"";
    static char *kwd = "keyword";

    switch (yychar) {
	case 0         : return "end of input";

#define keyword(kw) sprintf(buffer,fmt,kwd,kw); return buffer;
	case INFIXL    : keyword("infixl");
	case INFIXR    : keyword("infixr");
	case INFIXN    : keyword("infix");
	case TINSTANCE : keyword("instance");
	case TCLASS    : keyword("class");
	case PRIMITIVE : keyword("primitive");
	case CASEXP    : keyword("case");
	case OF        : keyword("of");
	case IF        : keyword("if");
	case THEN      : keyword("then");
	case ELSE      : keyword("else");
	case WHERE     : keyword("where");
	case TYPE      : keyword("type");
	case DATA      : keyword("data");
	case TNEWTYPE  : keyword("newtype");
	case LET       : keyword("let");
	case IN        : keyword("in");
	case DERIVING  : keyword("deriving");
	case DEFAULT   : keyword("default");
	case IMPORT    : keyword("import");
	case TMODULE   : keyword("module");
	case ALL       : keyword("forall");
#undef keyword

	case ARROW     : return "`->'";
	case '='       : return "`='";
	case COCO      : return "`::'";
	case '-'       : return "`-'";
	case '!'       : return "`!'";
	case ','       : return "comma";
	case '@'       : return "`@'";
	case '('       : return "`('";
	case ')'       : return "`)'";
	case '{'       : return "`{', possibly due to bad layout";
	case '}'       : return "`}', possibly due to bad layout";
	case '_'       : return "`_'";
	case '|'       : return "`|'";
	case '.'       : return "`.'";
	case ';'       : return "`;', possibly due to bad layout";
	case UPTO      : return "`..'";
	case '['       : return "`['";
	case ']'       : return "`]'";
	case FROM      : return "`<-'";
	case '\\'      : return "backslash (lambda)";
	case '~'       : return "tilde";
	case '`'       : return "backquote";
#if TREX
	case RECSELID  : sprintf(buffer,"selector \"#%s\"",
				 textToStr(extText(snd(yylval))));
			 return buffer;
#endif
#if IPARAM
	case IPVARID   : sprintf(buffer,"implicit parameter \"?%s\"",
				 textToStr(textOf(yylval)));
			 return buffer;
#endif
	case VAROP     :
	case VARID     :
	case CONOP     :
	case CONID     : sprintf(buffer,"symbol \"%s\"",
				 textToStr(textOf(yylval)));
			 return buffer;
	case QVAROP    :
	case QVARID    :
	case QCONOP    : 
	case QCONID    : sprintf(buffer,"symbol \"%s\"",
				 identToStr(yylval));
			 return buffer;
	case HIDING    : return "symbol \"hiding\"";
	case QUALIFIED : return "symbol \"qualified\"";
	case ASMOD     : return "symbol \"as\"";
	case NUMLIT    : return "numeric literal";
	case CHARLIT   : return "character literal";
	case STRINGLIT : return "string literal";
	case IMPLIES   : return "`=>'";
	default        : return "token";
    }
}

static Cell local checkPrec(p)		/* Check for valid precedence value*/
Cell p; {
    if (!isInt(p) || intOf(p)<MIN_PREC || intOf(p)>MAX_PREC) {
	ERRMSG(row) "Precedence value must be an integer in the range [%d..%d]",
		    MIN_PREC, MAX_PREC
	EEND;
    }
    return p;
}

static Cell local buildTuple(tup)	/* build tuple (x1,...,xn) from	   */
List tup; {				/* list [xn,...,x1]		   */
    Int  n = 0;
    Cell t = tup;
    Cell x;

    do {				/*    .                    .	   */
	x      = fst(t);		/*   / \                  / \	   */
	fst(t) = snd(t);		/*  xn  .                .   xn	   */
	snd(t) = x;			/*       .    ===>      .	   */
	x      = t;			/*        .            .	   */
	t      = fun(x);		/*         .          .		   */
	n++;				/*        / \        / \	   */
    } while (nonNull(t));		/*       x1  NIL   (n)  x1	   */
    fst(x) = mkTuple(n);
    return tup;
}

static List local checkCtxt(con)	/* validate context		   */
Type con; {
    mapOver(checkPred, con);
    return con;
}

static Cell local checkPred(c)		/* check that type expr is a valid */
Cell c; {				/* constraint			   */
    Cell cn = getHead(c);
#if TREX
    if (isExt(cn) && argCount==1)
	return c;
#endif
#if IPARAM
    if (isIP(cn))
	return c;
#endif
    if (!isQCon(cn) /*|| argCount==0*/)
	syntaxError("class expression");
    return c;
}

static Pair local checkDo(dqs)		/* convert reversed list of dquals */
List dqs; {				/* to an (expr,quals) pair         */
    if (isNull(dqs) || whatIs(hd(dqs))!=DOQUAL) {
	ERRMSG(row) "Last generator in do {...} must be an expression"
	EEND;
    }
    fst(dqs) = snd(fst(dqs));		/* put expression in fst of pair   */
    snd(dqs) = rev(snd(dqs));		/* & reversed list of quals in snd */
    return dqs;
}

#if MUDO
static Pair local checkMDo(dqs)		/* convert reversed list of dquals */
List dqs; {				/* to an (expr,quals) pair         */
    if (isNull(dqs) || whatIs(hd(dqs))!=DOQUAL) {
	ERRMSG(row) "Last generator in mdo {...} must be an expression"
	EEND;
    }
    fst(dqs) = snd(fst(dqs));		/* put expression in fst of pair   */
    snd(dqs) = rev(snd(dqs));		/* & reversed list of quals in snd */
    return dqs;
}
#endif

static Cell local checkTyLhs(c)		/* check that lhs is of the form   */
Cell c; {				/* T a1 ... a			   */
    Cell tlhs = c;
    while (isAp(tlhs) && whatIs(arg(tlhs))==VARIDCELL) {
	tlhs = fun(tlhs);
    }
    if (whatIs(tlhs)!=CONIDCELL) {
	ERRMSG(row) "Illegal left hand side in data type declaration"
	EEND;
    }
    return c;
}

static Cell local checkConstr(c)	/* check that data constructor has */
Cell c; {				/* an unqualified conid as head    */
    Cell chd = c;
    while (isAp(chd)) {
	chd = fun(chd);
    }
    if (whatIs(chd)==QUALIDENT) {
	ERRMSG(row) "Qualified constructor in data type declaration"
	EEND;
    }
    return c;
}

#if !TREX
static Void local noTREX(where)
String where; {
    ERRMSG(row) "Attempt to use TREX records while parsing %s.\n", where ETHEN
    ERRTEXT     "(TREX is disabled in this build of Hugs)"
    EEND;
}
#endif
#if !IPARAM
static Void local noIP(where)
String where; {
    ERRMSG(row) "Attempt to use Implicit Parameters while parsing %s.\n", where ETHEN
    ERRTEXT     "(Implicit Parameters are disabled in this build of Hugs)"
    EEND;
}
#endif

#if !MUDO
/***
   Due to the way we implement this stuff, this function will actually
   never be called. When MUDO is not defined, the lexer thinks that mdo
   is just another identifier, and hence the MDO token is never returned
   to the parser: consequently the mdo production is never reduced, making 
   this code unreachable. The alternative is to let the lexer to 
   recognize "mdo" all the time, but that's not Haskell compliant. In any 
   case we keep this function here, even if just for documentation purposes.
***/
static Void local noMDo(where)
String where; {
    ERRMSG(row) "Attempt to use MDO while parsing %s.\n", where ETHEN
    ERRTEXT     "(Recursive monadic bindings are disabled in this build of Hugs)"
    EEND;
}
#endif

/*-------------------------------------------------------------------------*/

