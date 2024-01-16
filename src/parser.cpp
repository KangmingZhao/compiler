/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.5.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* "%code top" blocks.  */
#line 1 "src/parser.y"

    #include <iostream>
    #include <assert.h>
    #include "parser.h"
    #include <vector>
    extern Ast ast;

    //ArrDimNode** now_arrdim;
    //int now_arrindex;
    //闂佺懓鐡ㄩ崹鍐测堪閹寸偛顕辨慨姗嗗墯闊憘tmt闂佸憡鑹鹃柊锝咁焽娴兼潙绀夐柣妯夸含閻熸€婻EAK SEMICOLON闂佹眹鍔岀€氼垶鎯侀幋锔芥櫢闁跨噦鎷�?闂佹寧绋戦惉濂稿礉瑜斿畷銉︽償閳ヨ櫕閿梺鍝勭墕椤︻垰锕㈤悮妾抩ckstmt闂佽法鍣﹂幏锟�?婵炴潙鍚嬮悢顒勫箯閿燂拷?闂佽法鍠撻弲顐﹀箖婵犲洤钃熼柟閭︿簽椤忛亶鏌ㄩ悤鍌涘?婵炶揪绲剧划搴ㄥ极閵堝鏄ラ柣鏂挎啞绗戞繛鎴炴尭缁夐潧危閹间礁鎹堕柍铏圭壀ile闂佸憡鍔曢幊姗€宕曢幘顔藉剭闁告洍鏂侀崑鎾诲磼濮橆厾顦繛鎴炴尭閹碱偊宕ｉ悙顒傤浄闁哄稁鍋呴悾閬嶆偠濞戞牕濮€闁规椿浜弫鎾绘晸閿燂拷?
    //闁荤喐娲戝鎺旂不閻愮儤鏅搁柨鐕傛嫹?闂佽壈娅曢弸濠氬箯閿燂拷?闂佽法鍠庨埀顒傚枂閳ь剙鍟蹇涘箚瑜忕涵鈧繛鎴炴尭缁夌兘宕楀Ο鍏煎晳闁告鐛癳ak闂佹寧绋戦惌澶愬箯閿燂拷?闂佽法鍠愰崹婵堢不閻愮儤鏅搁柨鐕傛嫹?閻庡湱枪瀹曨剙顫濋妸鈺佸強闁汇儲鎮渆ak闂佽法鍣﹂幏锟�?
    
    Type  *declType;
    std::vector<Type*> FuncParamsVector;
    int yylex();
    int yyerror( char const * );

    std::vector<BreakStmt*> Break_stack;
    int Break_stack_ok_2_push_back = 0;
    void loop_match_break(StmtNode* loopStmt)
    {
        if(Break_stack.size())
        {
            Break_stack[Break_stack.size() - 1]->match_with_loop(loopStmt);
            Break_stack.pop_back();
        }
    }

    
    std::vector<ContinueStmt*> Continue_stack;
    int Continue_stack_ok_2_push_back = 0;
    void loop_match_continue(StmtNode* loopStmt)
    {
        if(Continue_stack.size())
        {
            Continue_stack[Continue_stack.size() - 1]->match_with_loop(loopStmt);
            Continue_stack.pop_back();
        }
    }

    int arr_dimension_recorder = 0;

    ArrDimNode::ArrDimNode_STATE arrDimNode_state;

#line 112 "src/parser.cpp"




# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef YY_YY_INCLUDE_PARSER_H_INCLUDED
# define YY_YY_INCLUDE_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif
/* "%code requires" blocks.  */
#line 46 "src/parser.y"

    #include "Ast.h"
    #include "SymbolTable.h"
    #include "Type.h"



#line 166 "src/parser.cpp"

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    ID = 258,
    INTEGER = 259,
    FLOATPOINT = 260,
    IF = 261,
    ELSE = 262,
    WHILE = 263,
    INT = 264,
    VOID = 265,
    FLOAT = 266,
    LPAREN = 267,
    RPAREN = 268,
    LBRACE = 269,
    RBRACE = 270,
    LBRACKET = 271,
    RBRACKET = 272,
    SEMICOLON = 273,
    COMMA = 274,
    NOT = 275,
    ADD = 276,
    SUB = 277,
    MUL = 278,
    DIV = 279,
    MOD = 280,
    OR = 281,
    AND = 282,
    LESS = 283,
    GREATER = 284,
    ASSIGN = 285,
    LESSEQUAL = 286,
    GREATEREQUAL = 287,
    EQUAL = 288,
    NOTEQUAL = 289,
    RETURN = 290,
    CONST = 291,
    BREAK = 292,
    CONTINUE = 293,
    THEN = 294
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 77 "src/parser.y"

    int itype;
    float fltype;
    char* strtype;
    StmtNode* stmttype;
    ExprNode* exprtype;
    ArrDimNode * arrdimtype;
    InitNode * inittype;
    ParaNode * paratype;
    Type* type;


#line 230 "src/parser.cpp"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_INCLUDE_PARSER_H_INCLUDED  */



#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))

/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
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
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
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
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  74
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   217

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  40
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  47
/* YYNRULES -- Number of rules.  */
#define YYNRULES  97
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  161

#define YYUNDEFTOK  2
#define YYMAXUTOK   294


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
      35,    36,    37,    38,    39
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   122,   122,   127,   128,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   146,   173,   172,   207,
     213,   212,   223,   232,   235,   245,   244,   256,   269,   281,
     288,   292,   299,   304,   308,   316,   319,   323,   328,   337,
     344,   349,   357,   362,   364,   377,   394,   400,   394,   424,
     428,   432,   445,   461,   463,   477,   491,   501,   503,   517,
     536,   538,   552,   567,   582,   597,   612,   629,   631,   648,
     650,   666,   670,   675,   688,   696,   706,   711,   719,   724,
     730,   736,   745,   747,   756,   758,   763,   771,   776,   777,
     781,   798,   816,   816,   851,   852,   857,   898
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "ID", "INTEGER", "FLOATPOINT", "IF",
  "ELSE", "WHILE", "INT", "VOID", "FLOAT", "LPAREN", "RPAREN", "LBRACE",
  "RBRACE", "LBRACKET", "RBRACKET", "SEMICOLON", "COMMA", "NOT", "ADD",
  "SUB", "MUL", "DIV", "MOD", "OR", "AND", "LESS", "GREATER", "ASSIGN",
  "LESSEQUAL", "GREATEREQUAL", "EQUAL", "NOTEQUAL", "RETURN", "CONST",
  "BREAK", "CONTINUE", "THEN", "$accept", "Stmts", "Stmt", "AssignStmt",
  "BlockStmt", "IfStmt", "ReturnStmt", "DeclStmt", "WhileStmt", "ExprStmt",
  "BreakStmt", "ContinueStmt", "Exp", "AddExp", "MulExp", "Cond", "LOrExp",
  "PrimaryExp", "LVal", "RelExp", "LAndExp", "UnaryExp", "InitVal",
  "FunctCall", "IdDeclLists", "IdDeclList", "ConstDeclLists",
  "ConstDeclList", "VarDeclStmt", "ConstDeclStmt", "EmptyStmt",
  "ArrDimensions", "ArrDimension", "ArrInit", "ArrInitLists",
  "ArrInitList", "FuncDef", "PARAMENT_LISTS", "PARAMENT_LIST", "Type",
  "Program", "$@1", "$@2", "$@3", "$@4", "$@5", "$@6", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294
};
# endif

#define YYPACT_NINF (-71)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-93)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     179,    -3,   -71,   -71,    22,   -71,   -71,   -71,   -71,   191,
      27,   -71,   191,   191,   191,    23,    38,    37,    40,   179,
     -71,   -71,   -71,   -71,   -71,   -71,   -71,   -71,   -71,   -71,
      54,    15,    83,   -71,    49,   -71,   -71,   -71,   -71,   -71,
     -71,    91,    95,    11,    85,   191,    97,    15,    98,    84,
     -71,    71,    86,   -71,   179,   -71,   -71,   -71,   -71,    96,
     113,   -71,   -71,   -71,   -71,   191,   191,   191,   191,   191,
     191,    41,    56,   -71,   -71,   -71,    -2,   -71,   114,    47,
      85,   -71,   107,   191,   -71,   191,   191,   191,   191,   191,
     191,   191,   191,   143,   -71,    93,    70,   -71,    83,    83,
     -71,   -71,   -71,   103,   191,   115,    85,   -71,   126,   -71,
      11,   -71,   -71,   119,   -71,   179,   117,    86,    15,    15,
      15,    15,    15,    15,    71,   -71,   191,   -71,   113,   -71,
     -71,   -71,    11,     8,     9,   -71,   -71,   -71,   124,   179,
     -71,   -71,    -1,   118,   -71,   179,   -71,   -71,   121,   -71,
     136,   121,   -71,    51,   -71,   -71,    58,   -71,   121,   -71,
     -71
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,    16,    36,    37,     0,    25,    71,    72,    73,     0,
      20,    39,     0,     0,     0,     0,     0,     0,     0,     2,
       3,     5,     6,     7,     8,     9,    11,    12,    13,    14,
       0,    32,    57,    49,    34,    53,    38,    84,    85,    15,
      10,     0,     0,    43,     0,     0,     0,    60,     0,    33,
      34,    67,    69,    22,     0,    52,    50,    51,    31,     0,
       0,    27,    28,     4,    29,     0,     0,     0,     0,     0,
       0,    91,     0,    89,     1,    42,     0,    41,     0,     0,
      18,    82,     0,     0,    35,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    30,     0,     0,    95,    58,    59,
      54,    55,    56,     0,     0,     0,     0,    86,     0,    45,
      43,    44,    81,     0,    83,     0,     0,    70,    61,    62,
      64,    63,    65,    66,    68,    21,     0,    87,     0,    19,
      97,    90,    43,    79,    91,    88,    40,    80,    23,     0,
      96,    94,     0,     0,    93,     0,    26,    47,     0,    24,
       0,     0,    74,     0,    77,    48,     0,    78,     0,    75,
      76
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -71,   102,   -17,   -71,   -12,   -71,   -71,   -71,   -71,   -71,
     -71,   -71,   -14,    -5,    26,   -37,   -71,   -71,     0,    48,
      74,    -7,    34,   -71,   -71,    59,   -71,    42,   -71,   -71,
     -71,    60,   -70,   -71,    17,     4,   -71,    39,    62,   -13,
     -71,   -71,   -71,   -71,   -71,   -71,   -71
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    48,    49,    33,    50,    51,
      52,    35,   131,    36,    72,    73,    96,    97,    37,    38,
      39,    80,    81,   144,   153,   154,    40,    76,    77,    41,
      42,    44,    54,    46,   105,   150,   106
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      34,    59,    63,    60,    47,    55,    56,    57,    82,    43,
     114,   109,   147,   -17,     1,     2,     3,   110,   110,    34,
       6,     7,     8,     9,    79,   -92,     1,     2,     3,    75,
      78,    12,    13,    14,    45,     9,    65,    66,   143,   104,
      47,    58,    53,    12,    13,    14,   116,     6,     7,     8,
       1,     2,     3,   -46,    34,    61,   103,   -92,    62,     9,
     100,   101,   102,   114,   112,   113,   157,    12,    13,    14,
     158,   104,    64,   159,   107,   108,    63,   158,    47,    70,
      47,   118,   119,   120,   121,   122,   123,    47,   127,   128,
     130,    98,    99,    34,    71,    74,    75,    78,   138,    86,
      87,    79,    88,    89,    90,    91,    67,    68,    69,    83,
      85,    84,   130,    92,    94,    34,    95,   111,    75,    78,
     115,   129,   146,   126,     1,     2,     3,   132,   149,   134,
     139,   145,   148,     9,   152,   151,   137,   152,   155,    34,
     124,    12,    13,    14,   152,    34,     1,     2,     3,     4,
      10,     5,     6,     7,     8,     9,    93,    10,   125,   117,
     140,    11,   160,    12,    13,    14,   133,   135,   156,     0,
     141,   142,   136,     0,     0,     0,     0,     0,    15,    16,
      17,    18,     1,     2,     3,     4,     0,     5,     6,     7,
       8,     9,     0,    10,     1,     2,     3,    11,     0,    12,
      13,    14,     0,     9,     0,     0,     0,     0,     0,     0,
       0,    12,    13,    14,    15,    16,    17,    18
};

static const yytype_int16 yycheck[] =
{
       0,    15,    19,    16,     9,    12,    13,    14,    45,    12,
      80,    13,    13,    16,     3,     4,     5,    19,    19,    19,
       9,    10,    11,    12,    16,    16,     3,     4,     5,    43,
      43,    20,    21,    22,    12,    12,    21,    22,    30,    30,
      45,    18,    15,    20,    21,    22,    83,     9,    10,    11,
       3,     4,     5,    12,    54,    18,    70,    16,    18,    12,
      67,    68,    69,   133,    17,    79,    15,    20,    21,    22,
      19,    30,    18,    15,    18,    19,    93,    19,    83,    30,
      85,    86,    87,    88,    89,    90,    91,    92,    18,    19,
     104,    65,    66,    93,     3,     0,   110,   110,   115,    28,
      29,    16,    31,    32,    33,    34,    23,    24,    25,    12,
      26,    13,   126,    27,    18,   115,     3,     3,   132,   132,
      13,    18,   139,    30,     3,     4,     5,    12,   145,     3,
      13,     7,    14,    12,   148,    14,    17,   151,   150,   139,
      92,    20,    21,    22,   158,   145,     3,     4,     5,     6,
      14,     8,     9,    10,    11,    12,    54,    14,    15,    85,
     126,    18,   158,    20,    21,    22,   106,   108,   151,    -1,
     128,   132,   110,    -1,    -1,    -1,    -1,    -1,    35,    36,
      37,    38,     3,     4,     5,     6,    -1,     8,     9,    10,
      11,    12,    -1,    14,     3,     4,     5,    18,    -1,    20,
      21,    22,    -1,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    21,    22,    35,    36,    37,    38
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     3,     4,     5,     6,     8,     9,    10,    11,    12,
      14,    18,    20,    21,    22,    35,    36,    37,    38,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    57,    58,    61,    63,    68,    69,    70,
      76,    79,    80,    12,    81,    12,    83,    53,    55,    56,
      58,    59,    60,    15,    82,    61,    61,    61,    18,    52,
      79,    18,    18,    42,    18,    21,    22,    23,    24,    25,
      30,     3,    64,    65,     0,    52,    77,    78,    79,    16,
      71,    72,    55,    12,    13,    26,    28,    29,    31,    32,
      33,    34,    27,    41,    18,     3,    66,    67,    54,    54,
      61,    61,    61,    52,    30,    84,    86,    18,    19,    13,
      19,     3,    17,    52,    72,    13,    55,    60,    53,    53,
      53,    53,    53,    53,    59,    15,    30,    18,    19,    18,
      52,    62,    12,    71,     3,    65,    78,    17,    42,    13,
      62,    67,    77,    30,    73,     7,    42,    13,    14,    42,
      85,    14,    52,    74,    75,    44,    74,    15,    19,    15,
      75
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int8 yyr1[] =
{
       0,    40,    80,    41,    41,    42,    42,    42,    42,    42,
      42,    42,    42,    42,    42,    42,    58,    81,    58,    43,
      82,    44,    44,    45,    45,    83,    48,    50,    51,    49,
      46,    46,    52,    55,    57,    57,    57,    57,    57,    70,
      77,    77,    78,    78,    78,    63,    84,    85,    76,    61,
      61,    61,    61,    54,    54,    54,    54,    53,    53,    53,
      59,    59,    59,    59,    59,    59,    59,    60,    60,    56,
      56,    79,    79,    79,    75,    75,    74,    74,    73,    73,
      72,    72,    71,    71,    47,    47,    68,    69,    64,    64,
      65,    65,    86,    65,    66,    66,    67,    62
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     3,     4,
       0,     4,     2,     5,     7,     0,     6,     2,     2,     2,
       3,     2,     1,     1,     1,     3,     1,     1,     1,     1,
       3,     1,     1,     0,     2,     4,     0,     0,     8,     1,
       2,     2,     2,     1,     3,     3,     3,     1,     3,     3,
       1,     3,     3,     3,     3,     3,     3,     1,     3,     1,
       3,     1,     1,     1,     1,     3,     3,     1,     4,     0,
       3,     2,     1,     2,     1,     1,     3,     4,     3,     1,
       3,     1,     0,     4,     3,     1,     3,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyo, yytype, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[+yyssp[yyi + 1 - yynrhs]],
                       &yyvsp[(yyi + 1) - (yynrhs)]
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

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
#ifndef YYINITDEPTH
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
#   define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
#  else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
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
static char *
yystpcpy (char *yydest, const char *yysrc)
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
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
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
            else
              goto append;

          append:
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

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                yy_state_t *yyssp, int yytoken)
{
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[+*yyssp];
      YYPTRDIFF_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
      yysize = yysize0;
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYPTRDIFF_T yysize1
                    = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen (yyformat) - 2 * yycount) + 1;
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYPTRDIFF_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

/* User initialization code.  */
#line 54 "src/parser.y"
{
    //闁哄鏅滈悷鈺呭闯闁垮浜ゆ俊顖滅帛缁犳帡寮堕埡鍌滄嚂闁瑰嚖鎷�?闂佽法鍠曞Λ鍕垂閸偅鍙忛悗锝庝簻椤曆囨煛閸屾繍娼愮痪顓炵埣閺佸秶浠︽慨鎰枈闂佺粯鐗炲Λ鍕汲閿燂拷
    // std::string getint = "getint";
    // Type* funcType1 = new FunctionType(TypeSystem::intType, {});
    // SymbolEntry* entry1 = new IdentifierSymbolEntry(funcType1, getint, 0);
    // identifiers->install(getint, entry1);
    
    // std::string putint = "putint";
    // Type* funcType2 = new FunctionType(TypeSystem::voidType, {});
    // SymbolEntry* entry2 = new IdentifierSymbolEntry(funcType2, putint, 0);
    // identifiers->install(putint, entry2);

    
    // std::string putch = "putch";
    // std::vector<Type*> vec; 
    // vec.push_back(TypeSystem::intType);
    // Type* funcType3 = new FunctionType(TypeSystem::voidType, vec); 
    // SymbolEntry* entry3 = new IdentifierSymbolEntry(funcType3, putch, 0);
    // identifiers->install(putch, entry3);


}

#line 1374 "src/parser.cpp"

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
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
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
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
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

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
#line 122 "src/parser.y"
            {
        ast.setRoot((yyvsp[0].stmttype));
    }
#line 1566 "src/parser.cpp"
    break;

  case 3:
#line 127 "src/parser.y"
           {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 1572 "src/parser.cpp"
    break;

  case 4:
#line 128 "src/parser.y"
                {
        (yyval.stmttype) = new SeqNode((yyvsp[-1].stmttype), (yyvsp[0].stmttype));
    }
#line 1580 "src/parser.cpp"
    break;

  case 5:
#line 133 "src/parser.y"
                 {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 1586 "src/parser.cpp"
    break;

  case 6:
#line 134 "src/parser.y"
                {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 1592 "src/parser.cpp"
    break;

  case 7:
#line 135 "src/parser.y"
             {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 1598 "src/parser.cpp"
    break;

  case 8:
#line 136 "src/parser.y"
                 {(yyval.stmttype)=(yyvsp[0].stmttype); }
#line 1604 "src/parser.cpp"
    break;

  case 9:
#line 137 "src/parser.y"
               {(yyval.stmttype)=(yyvsp[0].stmttype); }
#line 1610 "src/parser.cpp"
    break;

  case 10:
#line 138 "src/parser.y"
              {(yyval.stmttype)=(yyvsp[0].stmttype); }
#line 1616 "src/parser.cpp"
    break;

  case 11:
#line 139 "src/parser.y"
                { (yyval.stmttype) = (yyvsp[0].stmttype); loop_match_break((yyvsp[0].stmttype)); loop_match_continue((yyvsp[0].stmttype));}
#line 1622 "src/parser.cpp"
    break;

  case 12:
#line 140 "src/parser.y"
               { (yyval.stmttype) = (yyvsp[0].stmttype);}
#line 1628 "src/parser.cpp"
    break;

  case 13:
#line 141 "src/parser.y"
                { (yyval.stmttype) = (yyvsp[0].stmttype); }
#line 1634 "src/parser.cpp"
    break;

  case 14:
#line 142 "src/parser.y"
                   { (yyval.stmttype) = (yyvsp[0].stmttype); }
#line 1640 "src/parser.cpp"
    break;

  case 15:
#line 143 "src/parser.y"
                {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 1646 "src/parser.cpp"
    break;

  case 16:
#line 146 "src/parser.y"
         {
        int state = 0;
        
        SymbolEntry *se;
        se = identifiers->lookup((yyvsp[0].strtype)); //闂侀潻璐熼崝宀勫礄閿熺姴瀚夊璺烘湰閻ｈ京绱掑Δ瀣暠鐟滄媽娉曢幃浼村Ω閳轰焦顏為梺鍦暩閸嬫挸锕㈡担绯曟煢闁斥晛鍟粻鎺楀级閳哄倻鎳侀梺鍙夌獢D闂佽法鍣﹂幏锟�?
        if(se == nullptr) //婵犵鈧啿鈧綊鎮樻径濞炬煢闁斥晛鍟粻锟�
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)(yyvsp[0].strtype));//闂備胶鎳撻悘姘跺箰缁嬭法鏆ら柛鎰ㄦ櫆娴溿倖绻涢崱妤呭弰闁搞倕顑夐弻娑樷枎閹版儼鈧潡鏌涢敐鍛儓闁宠绮欓崺鍕礃閳哄啰鏆㈤梺璇叉唉椤鎯岄鍓т笉闁跨噦鎷�
            state = NOT_DEFINED;
            //delete [](char*)$1;
            //assert(se != nullptr);      //闂佺鍩栫粙鎴﹀吹椤撶喓鈻旈柍褜鍓熼弫鎾绘晸閿燂拷?闂佽法鍣﹂幏锟�?闁荤喎鐨濋崑鎾绘⒑閹稿海鎳曢柟鍑ゆ嫹??


            SymbolEntry *error_se;
            error_se = new IdentifierSymbolEntry(new ERROR_OCUPIER(), (yyvsp[0].strtype), identifiers->getLevel());
            (yyval.exprtype) = new Id(error_se, state);    //缂傚倷鐒﹂悷銊ф崲閺嶎厽鏅搁柨鐕傛嫹?$$闁荤姍鍐惧剮缂佹柨鐡ㄧ粙澶愭偑閻欙綁鎮楀☉娆忓閻炴凹鍋婇幆鍐礋椤旇　鍋撻崘鈺傜秶闁诡垎鍛攨缂傚倷鐒﹂幐楣冨磻閿濆违闁稿本绮嶉弳蹇涙煛婢跺牆鍔ョ紒棰濆弮瀹曟瑩鎼圭憴鍕殸/
            delete [](yyvsp[0].strtype);

        }
        else
        {
            (yyval.exprtype) = new Id(se, state);    //缂傚倷鐒﹂悷銊ф崲閺嶎厽鏅搁柨鐕傛嫹?$$闁荤姍鍐惧剮缂佹柨鐡ㄧ粙澶愭偑閻欙綁鎮楀☉娆忓閻炴凹鍋婇幆鍐礋椤旇　鍋撻崘鈺傜秶闁诡垎鍛攨缂傚倷鐒﹂幐楣冨磻閿濆违闁稿本绮嶉弳蹇涙煛婢跺牆鍔ョ紒棰濆弮瀹曟瑩鎼圭憴鍕殸/
            delete [](yyvsp[0].strtype);
        }
    }
#line 1676 "src/parser.cpp"
    break;

  case 17:
#line 173 "src/parser.y"
    {
        arrDimNode_state = ArrDimNode::ACCESS;
    }
#line 1684 "src/parser.cpp"
    break;

  case 18:
#line 177 "src/parser.y"
    {
        //闂佺懓鐡ㄩ崹鍐测堪閹达附鍎楅柕澶嗘櫃婢规洟鏌℃担鐟邦棆缂侇喖绉归幆鍐礋閼搁潧顏�?闂佽法鍠嶇划娆徫涢崼鏇熸櫢闁跨噦鎷�?闂佽法鍣﹂幏锟�?婵炲濮伴崕鎵礊閺冣偓缁嬪宕崟顏嗩唵闂佺ǹ锕ㄩ崑鎰枔閹达附鏅搁柨鐕傛嫹?
        int state = 0;

        SymbolEntry *se;
        se = identifiers->lookup((yyvsp[-2].strtype)); //闂侀潻璐熼崝宀勫礄閿熺姴瀚夊璺烘湰閻ｈ京绱掑Δ瀣暠鐟滄媽娉曢幃浼村Ω閳轰焦顏為梺鍦暩閸嬫挸锕㈡担绯曟煢闁斥晛鍟粻鎺楀级閳哄倻鎳侀梺鍙夌獢D闂佽法鍣﹂幏锟�?
        if(se == nullptr) //婵犵鈧啿鈧綊鎮樻径濞炬煢闁斥晛鍟粻锟�
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)(yyvsp[-2].strtype));//闂備胶鎳撻悘姘跺箰缁嬭法鏆ら柛鎰ㄦ櫆娴溿倖绻涢崱妤呭弰闁搞倕顑夐弻娑樷枎閹版儼鈧潡鏌涢敐鍛儓闁宠绮欓崺鍕礃閳哄啰鏆㈤梺璇叉唉椤鎯岄鍓т笉闁跨噦鎷�
            state = NOT_DEFINED;
            //delete [](char*)$1;
            //assert(se != nullptr);      //闂佺鍩栫粙鎴﹀吹椤撶喓鈻旈柍褜鍓熼弫鎾绘晸閿燂拷?闂佽法鍣﹂幏锟�?闁荤喎鐨濋崑鎾绘⒑閹稿海鎳曢柟鍑ゆ嫹??

            SymbolEntry *error_se;
            error_se = new IdentifierSymbolEntry(new ERROR_OCUPIER(), (yyvsp[-2].strtype), identifiers->getLevel());
            (yyval.exprtype) = new Id(error_se, state);    //缂傚倷鐒﹂悷銊ф崲閺嶎厽鏅搁柨鐕傛嫹?$$闁荤姍鍐惧剮缂佹柨鐡ㄧ粙澶愭偑閻欙綁鎮楀☉娆忓閻炴凹鍋婇幆鍐礋椤旇　鍋撻崘鈺傜秶闁诡垎鍛攨缂傚倷鐒﹂幐楣冨磻閿濆违闁稿本绮嶉弳蹇涙煛婢跺牆鍔ョ紒棰濆弮瀹曟瑩鎼圭憴鍕殸/
            delete [](yyvsp[-2].strtype);
        }
        else
        {
            (yyval.exprtype) = new Id(se, (yyvsp[0].arrdimtype), state);    //缂傚倷鐒﹂悷銊ф崲閺嶎厽鏅搁柨鐕傛嫹?$$闁荤姍鍐惧剮缂佹柨鐡ㄧ粙澶愭偑閻欙綁鎮楀☉娆忓閻炴凹鍋婇幆鍐礋椤旇　鍋撻崘鈺傜秶闁诡垎鍛攨缂傚倷鐒﹂幐楣冨磻閿濆违闁稿本绮嶉弳蹇涙煛婢跺牆鍔ョ紒棰濆弮瀹曟瑩鎼圭憴鍕殸/
            if(se->get_arr_dimension_recorder() < arr_dimension_recorder)
                fprintf(stderr, "array \"%s\" has max dimension \"%d\" but accessed by \"%d\" \n", (char*)(yyvsp[-2].strtype),se->get_arr_dimension_recorder(), arr_dimension_recorder );//闂佺懓鐏氶幐绋跨暤閸愨晜浜ゆ繛鍡楅叄閸ゅ鎮规担鍛婂仴婵☆偄澧庨幖楣冨川閹殿喚鐣洪梺璺ㄥ櫐閹凤拷?
            arr_dimension_recorder = 0;
            delete [](yyvsp[-2].strtype);
        }
    }
#line 1716 "src/parser.cpp"
    break;

  case 19:
#line 207 "src/parser.y"
                              {
        (yyval.stmttype) = new AssignStmt((yyvsp[-3].exprtype), (yyvsp[-1].exprtype));
    }
#line 1724 "src/parser.cpp"
    break;

  case 20:
#line 213 "src/parser.y"
        {identifiers = new SymbolTable(identifiers); 
        }
#line 1731 "src/parser.cpp"
    break;

  case 21:
#line 216 "src/parser.y"
        {
            (yyval.stmttype) = new CompoundStmt((yyvsp[-1].stmttype));
            SymbolTable *top = identifiers;
            identifiers = identifiers->getPrev();
            delete top;
        }
#line 1742 "src/parser.cpp"
    break;

  case 22:
#line 224 "src/parser.y"
    {
        (yyval.stmttype) = new EmptyStmt();
    }
#line 1750 "src/parser.cpp"
    break;

  case 23:
#line 232 "src/parser.y"
                                            {
        (yyval.stmttype) = new IfStmt((yyvsp[-2].exprtype), (yyvsp[0].stmttype));
    }
#line 1758 "src/parser.cpp"
    break;

  case 24:
#line 235 "src/parser.y"
                                           {
        (yyval.stmttype) = new IfElseStmt((yyvsp[-4].exprtype), (yyvsp[-2].stmttype), (yyvsp[0].stmttype));
    }
#line 1766 "src/parser.cpp"
    break;

  case 25:
#line 245 "src/parser.y"
    {
        Break_stack_ok_2_push_back++;
        Continue_stack_ok_2_push_back++;
    }
#line 1775 "src/parser.cpp"
    break;

  case 26:
#line 250 "src/parser.y"
    {
         (yyval.stmttype) = new WhileStmt((yyvsp[-2].exprtype), (yyvsp[0].stmttype));
    }
#line 1783 "src/parser.cpp"
    break;

  case 27:
#line 257 "src/parser.y"
    {
        BreakStmt* temp = new BreakStmt();
        (yyval.stmttype) = temp;
        if(Break_stack_ok_2_push_back)
        {
            Break_stack.push_back(temp);
            Break_stack_ok_2_push_back--;
        }
    }
#line 1797 "src/parser.cpp"
    break;

  case 28:
#line 270 "src/parser.y"
    {
        ContinueStmt * temp = new ContinueStmt();
        (yyval.stmttype) = temp;
        if(Continue_stack_ok_2_push_back)
        {
            Continue_stack.push_back(temp);
            Continue_stack_ok_2_push_back--;
        }
    }
#line 1811 "src/parser.cpp"
    break;

  case 29:
#line 281 "src/parser.y"
                    {
        
    }
#line 1819 "src/parser.cpp"
    break;

  case 30:
#line 288 "src/parser.y"
                        {
        (yyval.stmttype) = new ReturnStmt((yyvsp[-1].exprtype));
    }
#line 1827 "src/parser.cpp"
    break;

  case 31:
#line 292 "src/parser.y"
                    {
        (yyval.stmttype) = new ReturnStmt();
    }
#line 1835 "src/parser.cpp"
    break;

  case 32:
#line 299 "src/parser.y"
           {(yyval.exprtype) = (yyvsp[0].exprtype);
    }
#line 1842 "src/parser.cpp"
    break;

  case 33:
#line 304 "src/parser.y"
           {(yyval.exprtype) = (yyvsp[0].exprtype);}
#line 1848 "src/parser.cpp"
    break;

  case 34:
#line 308 "src/parser.y"
         {
        (yyval.exprtype) = (yyvsp[0].exprtype);
    }
#line 1856 "src/parser.cpp"
    break;

  case 35:
#line 316 "src/parser.y"
                       {
        (yyval.exprtype)=(yyvsp[-1].exprtype);
    }
#line 1864 "src/parser.cpp"
    break;

  case 36:
#line 319 "src/parser.y"
              {
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, (yyvsp[0].itype));
        (yyval.exprtype) = new Constant(se);
    }
#line 1873 "src/parser.cpp"
    break;

  case 37:
#line 323 "src/parser.y"
                 {
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::floatType, (yyvsp[0].fltype));
        (yyval.exprtype) = new Constant(se);
    }
#line 1882 "src/parser.cpp"
    break;

  case 38:
#line 329 "src/parser.y"
    {
        (yyval.exprtype) = (yyvsp[0].exprtype);
    }
#line 1890 "src/parser.cpp"
    break;

  case 39:
#line 337 "src/parser.y"
              {
        (yyval.stmttype) = new EmptyStmt();
    }
#line 1898 "src/parser.cpp"
    break;

  case 40:
#line 345 "src/parser.y"
    {
        (yyval.paratype) = new ParaNode((yyvsp[-2].paratype),(yyvsp[0].paratype));
    }
#line 1906 "src/parser.cpp"
    break;

  case 41:
#line 350 "src/parser.y"
    {
        (yyval.paratype) = (yyvsp[0].paratype);
    }
#line 1914 "src/parser.cpp"
    break;

  case 42:
#line 358 "src/parser.y"
    {
        (yyval.paratype) = new ParaNode((yyvsp[0].exprtype));
    }
#line 1922 "src/parser.cpp"
    break;

  case 43:
#line 362 "src/parser.y"
           { (yyval.paratype) = nullptr ;}
#line 1928 "src/parser.cpp"
    break;

  case 44:
#line 364 "src/parser.y"
            {
        SymbolEntry *se;
        se = new IdentifierSymbolEntry((yyvsp[-1].type), (yyvsp[0].strtype), identifiers->getLevel());
        identifiers->install((yyvsp[0].strtype), se);
        (yyval.paratype) = new ParaNode(new Id(se));
        delete [](yyvsp[0].strtype);
        FuncParamsVector.push_back((yyvsp[-1].type));
    }
#line 1941 "src/parser.cpp"
    break;

  case 45:
#line 378 "src/parser.y"
    {   
        SymbolEntry *se;
        se = identifiers->lookup((yyvsp[-3].strtype)); 
        if(se == nullptr) //濠电姷顣介埀顒€鍟块埀顒€缍婇幃妯诲緞婵炵偓鐓㈤梺鏂ユ櫅閸燁垳绮婚敓锟�
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)(yyvsp[-3].strtype));//闂備胶鎳撻悘姘跺箰缁嬭法鏆ら柛鎰ㄦ櫆娴溿倖绻涢崱妤呭弰闁搞倕顑夐弻娑樷枎閹版儼鈧潡鏌涢敐鍛儓闁宠绮欓崺鍕礃閳哄啰鏆㈤梺璇叉唉椤鎯岄鍓т笉闁跨噦鎷�
            delete [](char*)(yyvsp[-3].strtype);
            assert(se != nullptr);      //闂佺鍩栫粙鎴﹀吹椤撶喓鈻旈柍褜鍓熼弫鎾绘晸閿燂拷?闂佽法鍣﹂幏锟�?闁荤喎鐨濋崑鎾绘⒑閹稿海鎳曢柟鍑ゆ嫹??
        }
        // SymbolEntry* temp=new TemporarySymbolEntry(se->getType(),SymbolTable::getLabel());
        (yyval.exprtype) = new FunctCall(se, (yyvsp[-1].paratype));
    }
#line 1958 "src/parser.cpp"
    break;

  case 46:
#line 394 "src/parser.y"
           {
        SymbolEntry *se = new IdentifierSymbolEntry(nullptr, (yyvsp[0].strtype), identifiers->getLevel());
        identifiers->install((yyvsp[0].strtype), se);
        identifiers = new SymbolTable(identifiers);
    }
#line 1968 "src/parser.cpp"
    break;

  case 47:
#line 400 "src/parser.y"
    {
        FunctionType *funcType;
        funcType=new FunctionType((yyvsp[-5].type),{});
        FuncParamsVector.swap(funcType->paramsType);

        SymbolEntry *se;
        se = identifiers->lookup((yyvsp[-4].strtype));
        IdentifierSymbolEntry* ss=(IdentifierSymbolEntry*)se;
        ss->setFuncType(((Type*)funcType));
    }
#line 1983 "src/parser.cpp"
    break;

  case 48:
#line 411 "src/parser.y"
    {
        SymbolEntry *se;
        se = identifiers->lookup((yyvsp[-6].strtype));
        assert(se != nullptr);
        (yyval.stmttype) = new FunctionDef(se, (yyvsp[0].stmttype),(yyvsp[-3].paratype));
        SymbolTable *top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
        delete [](yyvsp[-6].strtype);
    }
#line 1998 "src/parser.cpp"
    break;

  case 49:
#line 424 "src/parser.y"
               {
        (yyval.exprtype) = (yyvsp[0].exprtype);
    }
#line 2006 "src/parser.cpp"
    break;

  case 50:
#line 428 "src/parser.y"
                 {
        (yyval.exprtype) = (yyvsp[0].exprtype);
    }
#line 2014 "src/parser.cpp"
    break;

  case 51:
#line 432 "src/parser.y"
                 {
        if((yyvsp[0].exprtype)->get_symbolEntry()->getType()->isInt())
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            (yyval.exprtype) = new UnaryExpr(se, UnaryExpr::SUB, (yyvsp[0].exprtype));
        }
        else if((yyvsp[0].exprtype)->get_symbolEntry()->getType()->isFLOAT())
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            (yyval.exprtype) = new UnaryExpr(se, UnaryExpr::SUB, (yyvsp[0].exprtype));
        }
    }
#line 2031 "src/parser.cpp"
    break;

  case 52:
#line 445 "src/parser.y"
                {
    if((yyvsp[0].exprtype)->get_symbolEntry()->getType()->isInt())
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            (yyval.exprtype) = new UnaryExpr(se, UnaryExpr::NOT, (yyvsp[0].exprtype));
        }
        else if((yyvsp[0].exprtype)->get_symbolEntry()->getType()->isFLOAT())
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            (yyval.exprtype) = new UnaryExpr(se, UnaryExpr::NOT, (yyvsp[0].exprtype));
        }

    }
#line 2049 "src/parser.cpp"
    break;

  case 53:
#line 461 "src/parser.y"
             { (yyval.exprtype) = (yyvsp[0].exprtype);}
#line 2055 "src/parser.cpp"
    break;

  case 54:
#line 463 "src/parser.y"
                        {
        //闁哄鏅滈悷鈺呭闯閻戣棄绠ｉ柟瀛樼矋缁箑霉閻樹警鍤欏┑顔惧枛閺佹捇鏁撻敓锟�?闁荤喐娲戦悞锕€锕㈡担鍦枖闁逞屽墴閺佹捇鏁撻敓锟�?闂佸搫瀚Λ鐎昽at闂備緡鍙忕徊鑲╃不閻愬吀鐒婇弶鍫涘妼閻︾豹loat闂佸搫顦崕鎶芥偩婵犳碍鏅搁柨鐕傛嫹?
        if((yyvsp[0].exprtype)->get_symbolEntry()->getType()->isFLOAT() || (yyvsp[-2].exprtype)->get_symbolEntry()->getType()->isFLOAT())
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::MUL, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::MUL, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
    }
#line 2073 "src/parser.cpp"
    break;

  case 55:
#line 478 "src/parser.y"
    {
        if((yyvsp[0].exprtype)->get_symbolEntry()->getType()->isFLOAT() || (yyvsp[-2].exprtype)->get_symbolEntry()->getType()->isFLOAT())
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::DIV, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::DIV, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
    }
#line 2090 "src/parser.cpp"
    break;

  case 56:
#line 492 "src/parser.y"
    {
        //濠殿喚鎳撻崐椋庣礊閹达箑鏋侀柣妤€鐗婄瑧int闂佹眹鍔岀€氼亞绮幘缁樻櫢闁跨噦鎷�?
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::MOD, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
    }
#line 2100 "src/parser.cpp"
    break;

  case 57:
#line 501 "src/parser.y"
           { (yyval.exprtype) = (yyvsp[0].exprtype); }
#line 2106 "src/parser.cpp"
    break;

  case 58:
#line 504 "src/parser.y"
    {
        if((yyvsp[-2].exprtype)->get_symbolEntry()->getType()->isFLOAT() || (yyvsp[0].exprtype)->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::ADD, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::ADD, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
    }
#line 2123 "src/parser.cpp"
    break;

  case 59:
#line 518 "src/parser.y"
    {
        
        if((yyvsp[-2].exprtype)->get_symbolEntry()->getType()->isFLOAT() || (yyvsp[0].exprtype)->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::SUB, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::SUB, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
    }
#line 2141 "src/parser.cpp"
    break;

  case 60:
#line 536 "src/parser.y"
           {(yyval.exprtype) = (yyvsp[0].exprtype);}
#line 2147 "src/parser.cpp"
    break;

  case 61:
#line 539 "src/parser.y"
    {
        if((yyvsp[-2].exprtype)->get_symbolEntry()->getType()->isFLOAT() || (yyvsp[0].exprtype)->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::LESS, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::LESS, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
    }
#line 2164 "src/parser.cpp"
    break;

  case 62:
#line 553 "src/parser.y"
    {
        
        if((yyvsp[-2].exprtype)->get_symbolEntry()->getType()->isFLOAT() || (yyvsp[0].exprtype)->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::GREATER, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::GREATER, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
    }
#line 2182 "src/parser.cpp"
    break;

  case 63:
#line 568 "src/parser.y"
    {
        
        if((yyvsp[-2].exprtype)->get_symbolEntry()->getType()->isFLOAT() || (yyvsp[0].exprtype)->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::GREATEREQUAL, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::GREATEREQUAL, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
    }
#line 2200 "src/parser.cpp"
    break;

  case 64:
#line 583 "src/parser.y"
    {
        
        if((yyvsp[-2].exprtype)->get_symbolEntry()->getType()->isFLOAT() || (yyvsp[0].exprtype)->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::LESSEQUAL, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::LESSEQUAL, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
    }
#line 2218 "src/parser.cpp"
    break;

  case 65:
#line 598 "src/parser.y"
    {
        
        if((yyvsp[-2].exprtype)->get_symbolEntry()->getType()->isFLOAT() || (yyvsp[0].exprtype)->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::EQUAL, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::EQUAL, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
    }
#line 2236 "src/parser.cpp"
    break;

  case 66:
#line 613 "src/parser.y"
    {
        
        if((yyvsp[-2].exprtype)->get_symbolEntry()->getType()->isFLOAT() || (yyvsp[0].exprtype)->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::NOTEQUAL, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::NOTEQUAL, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
    }
#line 2254 "src/parser.cpp"
    break;

  case 67:
#line 629 "src/parser.y"
           {(yyval.exprtype) = (yyvsp[0].exprtype);}
#line 2260 "src/parser.cpp"
    break;

  case 68:
#line 632 "src/parser.y"
    {
        
        if((yyvsp[-2].exprtype)->get_symbolEntry()->getType()->isFLOAT() || (yyvsp[0].exprtype)->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::AND, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::AND, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
    }
#line 2278 "src/parser.cpp"
    break;

  case 69:
#line 648 "src/parser.y"
            {(yyval.exprtype) = (yyvsp[0].exprtype);}
#line 2284 "src/parser.cpp"
    break;

  case 70:
#line 651 "src/parser.y"
    {
        
        if((yyvsp[-2].exprtype)->get_symbolEntry()->getType()->isFLOAT() || (yyvsp[0].exprtype)->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::OR, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::OR, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
        }
    }
#line 2302 "src/parser.cpp"
    break;

  case 71:
#line 666 "src/parser.y"
          {
        (yyval.type) = TypeSystem::intType;
        declType=TypeSystem::intType;
    }
#line 2311 "src/parser.cpp"
    break;

  case 72:
#line 670 "src/parser.y"
           {
        (yyval.type) = TypeSystem::voidType;
        declType=TypeSystem::voidType;
    }
#line 2320 "src/parser.cpp"
    break;

  case 73:
#line 675 "src/parser.y"
          {
        (yyval.type) = TypeSystem::floatType;
        declType=TypeSystem::floatType;

    }
#line 2330 "src/parser.cpp"
    break;

  case 74:
#line 689 "src/parser.y"
    {
        //闁哄鏅滈悷銈夋煂濠婂牊鏅搁柨鐕傛嫹?闂佸憡甯楀姗€濡甸弮鈧粋宥夊幢椤撶姷顦悗娈垮枓閸嬫挸鈹戦纰卞創缂佺粯锕㈠畷妤呭Ψ閵夈儱绶繛杈剧稻閹告悂鍩€椤掑嫭锛熺紒顭掔節閺佹捇鏁撻敓锟�?

        (yyvsp[0].exprtype)->getSymPtr()->setType(declType);
        (yyval.inittype) = new InitNode((yyvsp[0].exprtype));
    }
#line 2341 "src/parser.cpp"
    break;

  case 75:
#line 697 "src/parser.y"
    {
        //闁哄鏅滈悷銈夋煂濠婂牊鏅搁柨鐕傛嫹?闁荤姴娲ら悺銊ノｉ幋鐐翠氦婵☆垳绮粻鎺楁煠瀹勬澘鏆遍柣顭戝灡缁嬪鍩€椤掑嫭鏅搁柨鐕傛嫹?
        (yyval.inittype) = (yyvsp[-1].inittype);
        (yyval.inittype)->i_m_checkpoint();
    }
#line 2351 "src/parser.cpp"
    break;

  case 76:
#line 707 "src/parser.y"
    {
        (yyval.inittype) = new InitNode((yyvsp[-2].inittype), (yyvsp[0].inittype));
    }
#line 2359 "src/parser.cpp"
    break;

  case 77:
#line 712 "src/parser.y"
    {
        (yyval.inittype) = (yyvsp[0].inittype);
    }
#line 2367 "src/parser.cpp"
    break;

  case 78:
#line 720 "src/parser.y"
    {
        (yyval.inittype) = (yyvsp[-1].inittype);
    }
#line 2375 "src/parser.cpp"
    break;

  case 79:
#line 724 "src/parser.y"
           { (yyval.inittype) = nullptr ;}
#line 2381 "src/parser.cpp"
    break;

  case 80:
#line 731 "src/parser.y"
    {
        arr_dimension_recorder++;
        (yyval.arrdimtype) = new ArrDimNode((yyvsp[-1].exprtype),arrDimNode_state);
    }
#line 2390 "src/parser.cpp"
    break;

  case 81:
#line 737 "src/parser.y"
    {
        (yyval.arrdimtype) = nullptr;
    }
#line 2398 "src/parser.cpp"
    break;

  case 82:
#line 745 "src/parser.y"
                 { (yyval.arrdimtype) = (yyvsp[0].arrdimtype); }
#line 2404 "src/parser.cpp"
    break;

  case 83:
#line 748 "src/parser.y"
    {
        (yyval.arrdimtype) = new ArrDimNode((yyvsp[-1].arrdimtype), (yyvsp[0].arrdimtype));
    }
#line 2412 "src/parser.cpp"
    break;

  case 84:
#line 756 "src/parser.y"
                {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 2418 "src/parser.cpp"
    break;

  case 85:
#line 758 "src/parser.y"
                  {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 2424 "src/parser.cpp"
    break;

  case 86:
#line 763 "src/parser.y"
                              {(yyval.stmttype)=(yyvsp[-1].stmttype);}
#line 2430 "src/parser.cpp"
    break;

  case 87:
#line 771 "src/parser.y"
                                       {(yyval.stmttype)=(yyvsp[-1].stmttype);}
#line 2436 "src/parser.cpp"
    break;

  case 88:
#line 776 "src/parser.y"
                                 {(yyval.stmttype)=new DeclList((yyvsp[-2].stmttype),(yyvsp[0].stmttype));}
#line 2442 "src/parser.cpp"
    break;

  case 89:
#line 777 "src/parser.y"
                 {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 2448 "src/parser.cpp"
    break;

  case 90:
#line 781 "src/parser.y"
                     {
        int state = LEGAL_VAR;
        
        SymbolEntry *check_redefination_se;
        check_redefination_se = identifiers->lookup_in_present_domain((yyvsp[-2].strtype)); //闂侀潻璐熼崝宀勫礄閿熺姴瀚夊璺烘湰閻ｈ京绱掑Δ瀣暠鐟滄媽娉曢幃浼村Ω閳轰焦顏為梺鍦暩閸嬫挸锕㈡担绯曟煢闁斥晛鍟粻鎺楀级閳哄倻鎳侀梺鍙夌獢D闂佽法鍣﹂幏锟�?
        if(check_redefination_se != nullptr) //婵犵鈧啿鈧綊鎮樻径濞炬煢闁斥晛鍟粻锟�
        {
            fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)(yyvsp[-2].strtype));//闂佺懓鐏氶幐绋跨暤閸愨晜浜ゆ繛鍡楅叄閸ゅ鏌涘▎鎰惰€块柛锝嗘そ閺屽苯顓奸崨顖涙瘞闂佽法鍣﹂幏锟�?
            state = REDEFINATION;
        }
        SymbolEntry *se;
        se = new IdentifierSymbolEntry(declType, (yyvsp[-2].strtype), identifiers->getLevel());
        identifiers->install((yyvsp[-2].strtype), se);
        (yyval.stmttype) = new DeclInitStmt(new Id(se, state),(yyvsp[0].exprtype));
        delete [](yyvsp[-2].strtype);
    }
#line 2469 "src/parser.cpp"
    break;

  case 91:
#line 798 "src/parser.y"
      {
        int state = LEGAL_VAR;
        
        SymbolEntry *check_redefination_se;
        check_redefination_se = identifiers->lookup_in_present_domain((yyvsp[0].strtype)); //闂侀潻璐熼崝宀勫礄閿熺姴瀚夊璺烘湰閻ｈ京绱掑Δ瀣暠鐟滄媽娉曢幃浼村Ω閳轰焦顏為梺鍦暩閸嬫挸锕㈡担绯曟煢闁斥晛鍟粻鎺楀级閳哄倻鎳侀梺鍙夌獢D闂佽法鍣﹂幏锟�?
        if(check_redefination_se != nullptr) //婵犵鈧啿鈧綊鎮樻径濞炬煢闁斥晛鍟粻锟�
        {
            fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)(yyvsp[0].strtype));//闂佺懓鐏氶幐绋跨暤閸愨晜浜ゆ繛鍡楅叄閸ゅ鏌涘▎鎰惰€块柛锝嗘そ閺屽苯顓奸崨顖涙瘞闂佽法鍣﹂幏锟�?
            state = REDEFINATION;
        }

        SymbolEntry *se;
        se = new IdentifierSymbolEntry(declType, (yyvsp[0].strtype), identifiers->getLevel());
        identifiers->install((yyvsp[0].strtype), se);
        (yyval.stmttype) = new DeclStmt(new Id(se, state));
        delete [](yyvsp[0].strtype);
    }
#line 2491 "src/parser.cpp"
    break;

  case 92:
#line 816 "src/parser.y"
       { arrDimNode_state = ArrDimNode::INIT; }
#line 2497 "src/parser.cpp"
    break;

  case 93:
#line 816 "src/parser.y"
                                                                       {

            int state = LEGAL_VAR;
        
            SymbolEntry *check_redefination_se;
            check_redefination_se = identifiers->lookup_in_present_domain((yyvsp[-3].strtype)); //闂侀潻璐熼崝宀勫礄閿熺姴瀚夊璺烘湰閻ｈ京绱掑Δ瀣暠鐟滄媽娉曢幃浼村Ω閳轰焦顏為梺鍦暩閸嬫挸锕㈡担绯曟煢闁斥晛鍟粻鎺楀级閳哄倻鎳侀梺鍙夌獢D闂佽法鍣﹂幏锟�?
            if(check_redefination_se != nullptr) //婵犵鈧啿鈧綊鎮樻径濞炬煢闁斥晛鍟粻锟�
            {
                fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)(yyvsp[-3].strtype));//闂佺懓鐏氶幐绋跨暤閸愨晜浜ゆ繛鍡楅叄閸ゅ鏌涘▎鎰惰€块柛锝嗘そ閺屽苯顓奸崨顖涙瘞闂佽法鍣﹂幏锟�?
                state = REDEFINATION;
            }



            SymbolEntry *se;
            if(declType->isInt())
            {
                INT_arrayType * temp = new INT_arrayType();
                se = new IdentifierSymbolEntry(temp, (yyvsp[-3].strtype), identifiers->getLevel());
            }
            else
            {
                FLOAT_arrayType * temp = new FLOAT_arrayType();
                se = new IdentifierSymbolEntry(temp, (yyvsp[-3].strtype), identifiers->getLevel());
            }
            identifiers->install((yyvsp[-3].strtype), se);
            (yyval.stmttype) = new DeclStmt(new Id(se, (yyvsp[-1].arrdimtype), (yyvsp[0].inittype), state, arr_dimension_recorder));
            arr_dimension_recorder = 0;
            delete [](yyvsp[-3].strtype);
        }
#line 2532 "src/parser.cpp"
    break;

  case 94:
#line 851 "src/parser.y"
                                       {(yyval.stmttype)=new ConstDeclList((yyvsp[-2].stmttype),(yyvsp[0].stmttype));}
#line 2538 "src/parser.cpp"
    break;

  case 95:
#line 852 "src/parser.y"
                    {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 2544 "src/parser.cpp"
    break;

  case 96:
#line 857 "src/parser.y"
                      {
        SymbolEntry *se;
        //fprintf(stderr, "%d",identifiers->getLevel());//闂佺懓鐏氶幐绋跨暤閸愨晜浜ゆ繛鍡楅叄閸ゅ鏌涘▎鎰惰€块柛锝嗘そ閺屽苯顓奸崨顖涙瘞闂佽法鍣﹂幏锟�?

        se = new IdentifierSymbolEntry(declType, (yyvsp[-2].strtype), identifiers->getLevel(), (yyvsp[0].exprtype));
        identifiers->install((yyvsp[-2].strtype), se);
        (yyval.stmttype) = new ConstDeclInitStmt(new Id(se),(yyvsp[0].exprtype));
        delete [](yyvsp[-2].strtype);
    }
#line 2558 "src/parser.cpp"
    break;

  case 97:
#line 898 "src/parser.y"
        { (yyval.exprtype)=(yyvsp[0].exprtype);}
#line 2564 "src/parser.cpp"
    break;


#line 2568 "src/parser.cpp"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *, YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
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

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
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
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
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

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


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


#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[+*yyssp], yyvsp);
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
  return yyresult;
}
#line 929 "src/parser.y"


int yyerror(char const* message)
{
    std::cerr<<message<<std::endl;
    return -1;
}
// 
