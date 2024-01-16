/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
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
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

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

#line 113 "src/parser.cpp"




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

#include "parser.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_ID = 3,                         /* ID  */
  YYSYMBOL_INTEGER = 4,                    /* INTEGER  */
  YYSYMBOL_FLOATPOINT = 5,                 /* FLOATPOINT  */
  YYSYMBOL_IF = 6,                         /* IF  */
  YYSYMBOL_ELSE = 7,                       /* ELSE  */
  YYSYMBOL_WHILE = 8,                      /* WHILE  */
  YYSYMBOL_INT = 9,                        /* INT  */
  YYSYMBOL_VOID = 10,                      /* VOID  */
  YYSYMBOL_FLOAT = 11,                     /* FLOAT  */
  YYSYMBOL_LPAREN = 12,                    /* LPAREN  */
  YYSYMBOL_RPAREN = 13,                    /* RPAREN  */
  YYSYMBOL_LBRACE = 14,                    /* LBRACE  */
  YYSYMBOL_RBRACE = 15,                    /* RBRACE  */
  YYSYMBOL_LBRACKET = 16,                  /* LBRACKET  */
  YYSYMBOL_RBRACKET = 17,                  /* RBRACKET  */
  YYSYMBOL_SEMICOLON = 18,                 /* SEMICOLON  */
  YYSYMBOL_COMMA = 19,                     /* COMMA  */
  YYSYMBOL_NOT = 20,                       /* NOT  */
  YYSYMBOL_ADD = 21,                       /* ADD  */
  YYSYMBOL_SUB = 22,                       /* SUB  */
  YYSYMBOL_MUL = 23,                       /* MUL  */
  YYSYMBOL_DIV = 24,                       /* DIV  */
  YYSYMBOL_MOD = 25,                       /* MOD  */
  YYSYMBOL_OR = 26,                        /* OR  */
  YYSYMBOL_AND = 27,                       /* AND  */
  YYSYMBOL_LESS = 28,                      /* LESS  */
  YYSYMBOL_GREATER = 29,                   /* GREATER  */
  YYSYMBOL_ASSIGN = 30,                    /* ASSIGN  */
  YYSYMBOL_LESSEQUAL = 31,                 /* LESSEQUAL  */
  YYSYMBOL_GREATEREQUAL = 32,              /* GREATEREQUAL  */
  YYSYMBOL_EQUAL = 33,                     /* EQUAL  */
  YYSYMBOL_NOTEQUAL = 34,                  /* NOTEQUAL  */
  YYSYMBOL_RETURN = 35,                    /* RETURN  */
  YYSYMBOL_CONST = 36,                     /* CONST  */
  YYSYMBOL_BREAK = 37,                     /* BREAK  */
  YYSYMBOL_CONTINUE = 38,                  /* CONTINUE  */
  YYSYMBOL_THEN = 39,                      /* THEN  */
  YYSYMBOL_YYACCEPT = 40,                  /* $accept  */
  YYSYMBOL_Program = 41,                   /* Program  */
  YYSYMBOL_Stmts = 42,                     /* Stmts  */
  YYSYMBOL_Stmt = 43,                      /* Stmt  */
  YYSYMBOL_LVal = 44,                      /* LVal  */
  YYSYMBOL_45_1 = 45,                      /* $@1  */
  YYSYMBOL_AssignStmt = 46,                /* AssignStmt  */
  YYSYMBOL_BlockStmt = 47,                 /* BlockStmt  */
  YYSYMBOL_48_2 = 48,                      /* $@2  */
  YYSYMBOL_IfStmt = 49,                    /* IfStmt  */
  YYSYMBOL_WhileStmt = 50,                 /* WhileStmt  */
  YYSYMBOL_51_3 = 51,                      /* $@3  */
  YYSYMBOL_BreakStmt = 52,                 /* BreakStmt  */
  YYSYMBOL_ContinueStmt = 53,              /* ContinueStmt  */
  YYSYMBOL_ExprStmt = 54,                  /* ExprStmt  */
  YYSYMBOL_ReturnStmt = 55,                /* ReturnStmt  */
  YYSYMBOL_Exp = 56,                       /* Exp  */
  YYSYMBOL_Cond = 57,                      /* Cond  */
  YYSYMBOL_PrimaryExp = 58,                /* PrimaryExp  */
  YYSYMBOL_EmptyStmt = 59,                 /* EmptyStmt  */
  YYSYMBOL_PARAMENT_LISTS = 60,            /* PARAMENT_LISTS  */
  YYSYMBOL_PARAMENT_LIST = 61,             /* PARAMENT_LIST  */
  YYSYMBOL_FunctCall = 62,                 /* FunctCall  */
  YYSYMBOL_FuncDef = 63,                   /* FuncDef  */
  YYSYMBOL_64_4 = 64,                      /* $@4  */
  YYSYMBOL_65_5 = 65,                      /* $@5  */
  YYSYMBOL_UnaryExp = 66,                  /* UnaryExp  */
  YYSYMBOL_MulExp = 67,                    /* MulExp  */
  YYSYMBOL_AddExp = 68,                    /* AddExp  */
  YYSYMBOL_RelExp = 69,                    /* RelExp  */
  YYSYMBOL_LAndExp = 70,                   /* LAndExp  */
  YYSYMBOL_LOrExp = 71,                    /* LOrExp  */
  YYSYMBOL_Type = 72,                      /* Type  */
  YYSYMBOL_ArrInitList = 73,               /* ArrInitList  */
  YYSYMBOL_ArrInitLists = 74,              /* ArrInitLists  */
  YYSYMBOL_ArrInit = 75,                   /* ArrInit  */
  YYSYMBOL_ArrDimension = 76,              /* ArrDimension  */
  YYSYMBOL_ArrDimensions = 77,             /* ArrDimensions  */
  YYSYMBOL_DeclStmt = 78,                  /* DeclStmt  */
  YYSYMBOL_VarDeclStmt = 79,               /* VarDeclStmt  */
  YYSYMBOL_ConstDeclStmt = 80,             /* ConstDeclStmt  */
  YYSYMBOL_IdDeclLists = 81,               /* IdDeclLists  */
  YYSYMBOL_IdDeclList = 82,                /* IdDeclList  */
  YYSYMBOL_83_6 = 83,                      /* $@6  */
  YYSYMBOL_ConstDeclLists = 84,            /* ConstDeclLists  */
  YYSYMBOL_ConstDeclList = 85,             /* ConstDeclList  */
  YYSYMBOL_InitVal = 86                    /* InitVal  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




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

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
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
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
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

#if !defined yyoverflow

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
#endif /* !defined yyoverflow */

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
#define YYFINAL  63
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

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   294


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

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

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "ID", "INTEGER",
  "FLOATPOINT", "IF", "ELSE", "WHILE", "INT", "VOID", "FLOAT", "LPAREN",
  "RPAREN", "LBRACE", "RBRACE", "LBRACKET", "RBRACKET", "SEMICOLON",
  "COMMA", "NOT", "ADD", "SUB", "MUL", "DIV", "MOD", "OR", "AND", "LESS",
  "GREATER", "ASSIGN", "LESSEQUAL", "GREATEREQUAL", "EQUAL", "NOTEQUAL",
  "RETURN", "CONST", "BREAK", "CONTINUE", "THEN", "$accept", "Program",
  "Stmts", "Stmt", "LVal", "$@1", "AssignStmt", "BlockStmt", "$@2",
  "IfStmt", "WhileStmt", "$@3", "BreakStmt", "ContinueStmt", "ExprStmt",
  "ReturnStmt", "Exp", "Cond", "PrimaryExp", "EmptyStmt", "PARAMENT_LISTS",
  "PARAMENT_LIST", "FunctCall", "FuncDef", "$@4", "$@5", "UnaryExp",
  "MulExp", "AddExp", "RelExp", "LAndExp", "LOrExp", "Type", "ArrInitList",
  "ArrInitLists", "ArrInit", "ArrDimension", "ArrDimensions", "DeclStmt",
  "VarDeclStmt", "ConstDeclStmt", "IdDeclLists", "IdDeclList", "$@6",
  "ConstDeclLists", "ConstDeclList", "InitVal", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

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
     179,    -3,   -71,   -71,    18,   -71,   -71,   -71,   -71,   191,
      25,   -71,   191,   191,   191,    23,    90,    29,    31,    42,
     179,   -71,    38,   -71,   -71,   -71,   -71,   -71,   -71,   -71,
     -71,    53,   -71,   -71,   -71,   -71,   -71,    79,    16,    67,
     -71,   -71,   -71,    12,    60,   191,    66,   -71,    95,    16,
      97,    65,    83,   -71,   179,   -71,   -71,   -71,   -71,    92,
     108,   -71,   -71,   -71,   -71,   191,   -71,   191,   191,   191,
     191,   191,    36,    40,   -71,   -71,    -1,   -71,   110,    52,
     -71,    60,   104,   191,   -71,   191,   191,   191,   191,   191,
     191,   191,   191,   143,   -71,    89,    70,   -71,   114,   -71,
     -71,   -71,    79,    79,   191,   121,    60,   -71,   132,   -71,
      12,   -71,   -71,   119,   -71,   179,   125,    16,    16,    16,
      16,    16,    16,    97,    65,   -71,   191,   -71,   108,   -71,
     -71,   -71,    12,    20,    37,   -71,   -71,   -71,   133,   179,
     -71,   -71,     6,   127,   -71,   179,   -71,   -71,   102,   -71,
     128,   102,   -71,   -71,    -5,   -71,    72,   -71,   102,   -71,
     -71
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,    16,    36,    37,     0,    25,    71,    72,    73,     0,
      20,    39,     0,     0,     0,     0,     0,     0,     0,     0,
       2,     3,    34,     5,     6,     7,    11,    13,    14,    12,
       8,     0,    49,    15,    38,    10,    53,    57,    32,     0,
       9,    84,    85,    43,     0,     0,     0,    34,     0,    60,
      67,    69,    33,    22,     0,    52,    50,    51,    31,     0,
       0,    27,    28,     1,     4,     0,    29,     0,     0,     0,
       0,     0,    91,     0,    89,    42,     0,    41,     0,     0,
      82,    18,     0,     0,    35,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    30,     0,     0,    95,     0,    54,
      55,    56,    58,    59,     0,     0,     0,    86,     0,    45,
      43,    44,    81,     0,    83,     0,     0,    61,    62,    64,
      63,    65,    66,    68,    70,    21,     0,    87,     0,    19,
      97,    90,    43,    79,    91,    88,    40,    80,    23,     0,
      96,    94,     0,     0,    93,     0,    26,    47,     0,    24,
       0,     0,    74,    77,     0,    48,     0,    78,     0,    75,
      76
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -71,   -71,    96,   -18,     0,   -71,   -71,     9,   -71,   -71,
     -71,   -71,   -71,   -71,   -71,   -71,   -14,   -37,   -71,   -71,
      11,    46,   -71,   -71,   -71,   -71,    -7,    24,    -6,    69,
      74,   -71,   -12,     4,    17,   -71,   -70,    61,   -71,   -71,
     -71,   -71,    62,   -71,   -71,    41,    45
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,    19,    20,    21,    47,    44,    23,    24,    54,    25,
      26,    46,    27,    28,    29,    30,    31,    48,    32,    33,
      76,    77,    34,    35,   105,   150,    36,    37,    38,    50,
      51,    52,    39,   153,   154,   144,    80,    81,    40,    41,
      42,    73,    74,   106,    96,    97,   131
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      22,    59,    64,    49,    60,    55,    56,    57,    82,    43,
     157,   114,   109,   -17,   158,     1,     2,     3,   110,   147,
      22,     6,     7,     8,     9,   110,     1,     2,     3,    75,
      45,    78,    12,    13,    14,     9,    79,    70,    71,    49,
      53,    58,    63,    12,    13,    14,   116,    61,   -46,    62,
     143,    98,   -92,   -92,    22,     1,     2,     3,   107,   108,
      99,   100,   101,   114,     9,   113,   104,   104,    65,   112,
      72,    66,    12,    13,    14,    64,    79,    49,    83,   117,
     118,   119,   120,   121,   122,    49,    49,   159,   127,   128,
     130,   158,    91,    22,   102,   103,    75,   138,    78,     6,
       7,     8,    67,    68,    69,     1,     2,     3,    84,    92,
      94,    95,   130,   111,     9,    22,   151,   115,    75,   126,
      78,   146,    12,    13,    14,    85,    86,   149,    87,    88,
      89,    90,   129,   132,   152,   134,   137,   152,   139,    22,
     145,   148,    10,   142,   152,    22,     1,     2,     3,     4,
      93,     5,     6,     7,     8,     9,   136,    10,   125,   155,
     123,    11,   160,    12,    13,    14,   124,   133,   156,   141,
     135,   140,     0,     0,     0,     0,     0,     0,    15,    16,
      17,    18,     1,     2,     3,     4,     0,     5,     6,     7,
       8,     9,     0,    10,     1,     2,     3,    11,     0,    12,
      13,    14,     0,     9,     0,     0,     0,     0,     0,     0,
       0,    12,    13,    14,    15,    16,    17,    18
};

static const yytype_int16 yycheck[] =
{
       0,    15,    20,     9,    16,    12,    13,    14,    45,    12,
      15,    81,    13,    16,    19,     3,     4,     5,    19,    13,
      20,     9,    10,    11,    12,    19,     3,     4,     5,    43,
      12,    43,    20,    21,    22,    12,    16,    21,    22,    45,
      15,    18,     0,    20,    21,    22,    83,    18,    12,    18,
      30,    65,    16,    16,    54,     3,     4,     5,    18,    19,
      67,    68,    69,   133,    12,    79,    30,    30,    30,    17,
       3,    18,    20,    21,    22,    93,    16,    83,    12,    85,
      86,    87,    88,    89,    90,    91,    92,    15,    18,    19,
     104,    19,    27,    93,    70,    71,   110,   115,   110,     9,
      10,    11,    23,    24,    25,     3,     4,     5,    13,    26,
      18,     3,   126,     3,    12,   115,    14,    13,   132,    30,
     132,   139,    20,    21,    22,    28,    29,   145,    31,    32,
      33,    34,    18,    12,   148,     3,    17,   151,    13,   139,
       7,    14,    14,   132,   158,   145,     3,     4,     5,     6,
      54,     8,     9,    10,    11,    12,   110,    14,    15,   150,
      91,    18,   158,    20,    21,    22,    92,   106,   151,   128,
     108,   126,    -1,    -1,    -1,    -1,    -1,    -1,    35,    36,
      37,    38,     3,     4,     5,     6,    -1,     8,     9,    10,
      11,    12,    -1,    14,     3,     4,     5,    18,    -1,    20,
      21,    22,    -1,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    21,    22,    35,    36,    37,    38
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     3,     4,     5,     6,     8,     9,    10,    11,    12,
      14,    18,    20,    21,    22,    35,    36,    37,    38,    41,
      42,    43,    44,    46,    47,    49,    50,    52,    53,    54,
      55,    56,    58,    59,    62,    63,    66,    67,    68,    72,
      78,    79,    80,    12,    45,    12,    51,    44,    57,    68,
      69,    70,    71,    15,    48,    66,    66,    66,    18,    56,
      72,    18,    18,     0,    43,    30,    18,    23,    24,    25,
      21,    22,     3,    81,    82,    56,    60,    61,    72,    16,
      76,    77,    57,    12,    13,    28,    29,    31,    32,    33,
      34,    27,    26,    42,    18,     3,    84,    85,    56,    66,
      66,    66,    67,    67,    30,    64,    83,    18,    19,    13,
      19,     3,    17,    56,    76,    13,    57,    68,    68,    68,
      68,    68,    68,    69,    70,    15,    30,    18,    19,    18,
      56,    86,    12,    77,     3,    82,    61,    17,    43,    13,
      86,    85,    60,    30,    75,     7,    43,    13,    14,    43,
      65,    14,    56,    73,    74,    47,    74,    15,    19,    15,
      73
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    40,    41,    42,    42,    43,    43,    43,    43,    43,
      43,    43,    43,    43,    43,    43,    44,    45,    44,    46,
      48,    47,    47,    49,    49,    51,    50,    52,    53,    54,
      55,    55,    56,    57,    58,    58,    58,    58,    58,    59,
      60,    60,    61,    61,    61,    62,    64,    65,    63,    66,
      66,    66,    66,    67,    67,    67,    67,    68,    68,    68,
      69,    69,    69,    69,    69,    69,    69,    70,    70,    71,
      71,    72,    72,    72,    73,    73,    74,    74,    75,    75,
      76,    76,    77,    77,    78,    78,    79,    80,    81,    81,
      82,    82,    83,    82,    84,    84,    85,    86
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
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


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


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

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


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




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
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
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
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
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
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
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
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






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
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
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

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

#line 1141 "src/parser.cpp"

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
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
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
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
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

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
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
  case 2: /* Program: Stmts  */
#line 122 "src/parser.y"
            {
        ast.setRoot((yyvsp[0].stmttype));
    }
#line 1346 "src/parser.cpp"
    break;

  case 3: /* Stmts: Stmt  */
#line 127 "src/parser.y"
           {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 1352 "src/parser.cpp"
    break;

  case 4: /* Stmts: Stmts Stmt  */
#line 128 "src/parser.y"
                {
        (yyval.stmttype) = new SeqNode((yyvsp[-1].stmttype), (yyvsp[0].stmttype));
    }
#line 1360 "src/parser.cpp"
    break;

  case 5: /* Stmt: AssignStmt  */
#line 133 "src/parser.y"
                 {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 1366 "src/parser.cpp"
    break;

  case 6: /* Stmt: BlockStmt  */
#line 134 "src/parser.y"
                {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 1372 "src/parser.cpp"
    break;

  case 7: /* Stmt: IfStmt  */
#line 135 "src/parser.y"
             {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 1378 "src/parser.cpp"
    break;

  case 8: /* Stmt: ReturnStmt  */
#line 136 "src/parser.y"
                 {(yyval.stmttype)=(yyvsp[0].stmttype); }
#line 1384 "src/parser.cpp"
    break;

  case 9: /* Stmt: DeclStmt  */
#line 137 "src/parser.y"
               {(yyval.stmttype)=(yyvsp[0].stmttype); }
#line 1390 "src/parser.cpp"
    break;

  case 10: /* Stmt: FuncDef  */
#line 138 "src/parser.y"
              {(yyval.stmttype)=(yyvsp[0].stmttype); }
#line 1396 "src/parser.cpp"
    break;

  case 11: /* Stmt: WhileStmt  */
#line 139 "src/parser.y"
                { (yyval.stmttype) = (yyvsp[0].stmttype); loop_match_break((yyvsp[0].stmttype)); loop_match_continue((yyvsp[0].stmttype));}
#line 1402 "src/parser.cpp"
    break;

  case 12: /* Stmt: ExprStmt  */
#line 140 "src/parser.y"
               { (yyval.stmttype) = (yyvsp[0].stmttype);}
#line 1408 "src/parser.cpp"
    break;

  case 13: /* Stmt: BreakStmt  */
#line 141 "src/parser.y"
                { (yyval.stmttype) = (yyvsp[0].stmttype); }
#line 1414 "src/parser.cpp"
    break;

  case 14: /* Stmt: ContinueStmt  */
#line 142 "src/parser.y"
                   { (yyval.stmttype) = (yyvsp[0].stmttype); }
#line 1420 "src/parser.cpp"
    break;

  case 15: /* Stmt: EmptyStmt  */
#line 143 "src/parser.y"
                {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 1426 "src/parser.cpp"
    break;

  case 16: /* LVal: ID  */
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
#line 1456 "src/parser.cpp"
    break;

  case 17: /* $@1: %empty  */
#line 173 "src/parser.y"
    {
        arrDimNode_state = ArrDimNode::ACCESS;
    }
#line 1464 "src/parser.cpp"
    break;

  case 18: /* LVal: ID $@1 ArrDimensions  */
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
#line 1496 "src/parser.cpp"
    break;

  case 19: /* AssignStmt: LVal ASSIGN Exp SEMICOLON  */
#line 207 "src/parser.y"
                              {
        (yyval.stmttype) = new AssignStmt((yyvsp[-3].exprtype), (yyvsp[-1].exprtype));
    }
#line 1504 "src/parser.cpp"
    break;

  case 20: /* $@2: %empty  */
#line 213 "src/parser.y"
        {identifiers = new SymbolTable(identifiers); 
        }
#line 1511 "src/parser.cpp"
    break;

  case 21: /* BlockStmt: LBRACE $@2 Stmts RBRACE  */
#line 216 "src/parser.y"
        {
            (yyval.stmttype) = new CompoundStmt((yyvsp[-1].stmttype));
            SymbolTable *top = identifiers;
            identifiers = identifiers->getPrev();
            delete top;
        }
#line 1522 "src/parser.cpp"
    break;

  case 22: /* BlockStmt: LBRACE RBRACE  */
#line 224 "src/parser.y"
    {
        (yyval.stmttype) = new EmptyStmt();
    }
#line 1530 "src/parser.cpp"
    break;

  case 23: /* IfStmt: IF LPAREN Cond RPAREN Stmt  */
#line 232 "src/parser.y"
                                            {
        (yyval.stmttype) = new IfStmt((yyvsp[-2].exprtype), (yyvsp[0].stmttype));
    }
#line 1538 "src/parser.cpp"
    break;

  case 24: /* IfStmt: IF LPAREN Cond RPAREN Stmt ELSE Stmt  */
#line 235 "src/parser.y"
                                           {
        (yyval.stmttype) = new IfElseStmt((yyvsp[-4].exprtype), (yyvsp[-2].stmttype), (yyvsp[0].stmttype));
    }
#line 1546 "src/parser.cpp"
    break;

  case 25: /* $@3: %empty  */
#line 245 "src/parser.y"
    {
        Break_stack_ok_2_push_back++;
        Continue_stack_ok_2_push_back++;
    }
#line 1555 "src/parser.cpp"
    break;

  case 26: /* WhileStmt: WHILE $@3 LPAREN Cond RPAREN Stmt  */
#line 250 "src/parser.y"
    {
         (yyval.stmttype) = new WhileStmt((yyvsp[-2].exprtype), (yyvsp[0].stmttype));
    }
#line 1563 "src/parser.cpp"
    break;

  case 27: /* BreakStmt: BREAK SEMICOLON  */
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
#line 1577 "src/parser.cpp"
    break;

  case 28: /* ContinueStmt: CONTINUE SEMICOLON  */
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
#line 1591 "src/parser.cpp"
    break;

  case 29: /* ExprStmt: Exp SEMICOLON  */
#line 281 "src/parser.y"
                    {
        
    }
#line 1599 "src/parser.cpp"
    break;

  case 30: /* ReturnStmt: RETURN Exp SEMICOLON  */
#line 288 "src/parser.y"
                        {
        (yyval.stmttype) = new ReturnStmt((yyvsp[-1].exprtype));
    }
#line 1607 "src/parser.cpp"
    break;

  case 31: /* ReturnStmt: RETURN SEMICOLON  */
#line 292 "src/parser.y"
                    {
        (yyval.stmttype) = new ReturnStmt();
    }
#line 1615 "src/parser.cpp"
    break;

  case 32: /* Exp: AddExp  */
#line 299 "src/parser.y"
           {(yyval.exprtype) = (yyvsp[0].exprtype);
    }
#line 1622 "src/parser.cpp"
    break;

  case 33: /* Cond: LOrExp  */
#line 304 "src/parser.y"
           {(yyval.exprtype) = (yyvsp[0].exprtype);}
#line 1628 "src/parser.cpp"
    break;

  case 34: /* PrimaryExp: LVal  */
#line 308 "src/parser.y"
         {
        (yyval.exprtype) = (yyvsp[0].exprtype);
    }
#line 1636 "src/parser.cpp"
    break;

  case 35: /* PrimaryExp: LPAREN Cond RPAREN  */
#line 316 "src/parser.y"
                       {
        (yyval.exprtype)=(yyvsp[-1].exprtype);
    }
#line 1644 "src/parser.cpp"
    break;

  case 36: /* PrimaryExp: INTEGER  */
#line 319 "src/parser.y"
              {
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, (yyvsp[0].itype));
        (yyval.exprtype) = new Constant(se);
    }
#line 1653 "src/parser.cpp"
    break;

  case 37: /* PrimaryExp: FLOATPOINT  */
#line 323 "src/parser.y"
                 {
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::floatType, (yyvsp[0].fltype));
        (yyval.exprtype) = new Constant(se);
    }
#line 1662 "src/parser.cpp"
    break;

  case 38: /* PrimaryExp: FunctCall  */
#line 329 "src/parser.y"
    {
        (yyval.exprtype) = (yyvsp[0].exprtype);
    }
#line 1670 "src/parser.cpp"
    break;

  case 39: /* EmptyStmt: SEMICOLON  */
#line 337 "src/parser.y"
              {
        (yyval.stmttype) = new EmptyStmt();
    }
#line 1678 "src/parser.cpp"
    break;

  case 40: /* PARAMENT_LISTS: PARAMENT_LISTS COMMA PARAMENT_LIST  */
#line 345 "src/parser.y"
    {
        (yyval.paratype) = new ParaNode((yyvsp[-2].paratype),(yyvsp[0].paratype));
    }
#line 1686 "src/parser.cpp"
    break;

  case 41: /* PARAMENT_LISTS: PARAMENT_LIST  */
#line 350 "src/parser.y"
    {
        (yyval.paratype) = (yyvsp[0].paratype);
    }
#line 1694 "src/parser.cpp"
    break;

  case 42: /* PARAMENT_LIST: Exp  */
#line 358 "src/parser.y"
    {
        (yyval.paratype) = new ParaNode((yyvsp[0].exprtype));
    }
#line 1702 "src/parser.cpp"
    break;

  case 43: /* PARAMENT_LIST: %empty  */
#line 362 "src/parser.y"
           { (yyval.paratype) = nullptr ;}
#line 1708 "src/parser.cpp"
    break;

  case 44: /* PARAMENT_LIST: Type ID  */
#line 364 "src/parser.y"
            {
        SymbolEntry *se;
        se = new IdentifierSymbolEntry((yyvsp[-1].type), (yyvsp[0].strtype), identifiers->getLevel());
        identifiers->install((yyvsp[0].strtype), se);
        (yyval.paratype) = new ParaNode(new Id(se));
        delete [](yyvsp[0].strtype);
        FuncParamsVector.push_back((yyvsp[-1].type));
    }
#line 1721 "src/parser.cpp"
    break;

  case 45: /* FunctCall: ID LPAREN PARAMENT_LISTS RPAREN  */
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
#line 1738 "src/parser.cpp"
    break;

  case 46: /* $@4: %empty  */
#line 394 "src/parser.y"
           {
        SymbolEntry *se = new IdentifierSymbolEntry(nullptr, (yyvsp[0].strtype), identifiers->getLevel());
        identifiers->install((yyvsp[0].strtype), se);
        identifiers = new SymbolTable(identifiers);
    }
#line 1748 "src/parser.cpp"
    break;

  case 47: /* $@5: %empty  */
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
#line 1763 "src/parser.cpp"
    break;

  case 48: /* FuncDef: Type ID $@4 LPAREN PARAMENT_LISTS RPAREN $@5 BlockStmt  */
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
#line 1778 "src/parser.cpp"
    break;

  case 49: /* UnaryExp: PrimaryExp  */
#line 424 "src/parser.y"
               {
        (yyval.exprtype) = (yyvsp[0].exprtype);
    }
#line 1786 "src/parser.cpp"
    break;

  case 50: /* UnaryExp: ADD UnaryExp  */
#line 428 "src/parser.y"
                 {
        (yyval.exprtype) = (yyvsp[0].exprtype);
    }
#line 1794 "src/parser.cpp"
    break;

  case 51: /* UnaryExp: SUB UnaryExp  */
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
#line 1811 "src/parser.cpp"
    break;

  case 52: /* UnaryExp: NOT UnaryExp  */
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
#line 1829 "src/parser.cpp"
    break;

  case 53: /* MulExp: UnaryExp  */
#line 461 "src/parser.y"
             { (yyval.exprtype) = (yyvsp[0].exprtype);}
#line 1835 "src/parser.cpp"
    break;

  case 54: /* MulExp: MulExp MUL UnaryExp  */
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
#line 1853 "src/parser.cpp"
    break;

  case 55: /* MulExp: MulExp DIV UnaryExp  */
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
#line 1870 "src/parser.cpp"
    break;

  case 56: /* MulExp: MulExp MOD UnaryExp  */
#line 492 "src/parser.y"
    {
        //濠殿喚鎳撻崐椋庣礊閹达箑鏋侀柣妤€鐗婄瑧int闂佹眹鍔岀€氼亞绮幘缁樻櫢闁跨噦鎷�?
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        (yyval.exprtype) = new BinaryExpr(se, BinaryExpr::MOD, (yyvsp[-2].exprtype), (yyvsp[0].exprtype));
    }
#line 1880 "src/parser.cpp"
    break;

  case 57: /* AddExp: MulExp  */
#line 501 "src/parser.y"
           { (yyval.exprtype) = (yyvsp[0].exprtype); }
#line 1886 "src/parser.cpp"
    break;

  case 58: /* AddExp: AddExp ADD MulExp  */
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
#line 1903 "src/parser.cpp"
    break;

  case 59: /* AddExp: AddExp SUB MulExp  */
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
#line 1921 "src/parser.cpp"
    break;

  case 60: /* RelExp: AddExp  */
#line 536 "src/parser.y"
           {(yyval.exprtype) = (yyvsp[0].exprtype);}
#line 1927 "src/parser.cpp"
    break;

  case 61: /* RelExp: RelExp LESS AddExp  */
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
#line 1944 "src/parser.cpp"
    break;

  case 62: /* RelExp: RelExp GREATER AddExp  */
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
#line 1962 "src/parser.cpp"
    break;

  case 63: /* RelExp: RelExp GREATEREQUAL AddExp  */
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
#line 1980 "src/parser.cpp"
    break;

  case 64: /* RelExp: RelExp LESSEQUAL AddExp  */
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
#line 1998 "src/parser.cpp"
    break;

  case 65: /* RelExp: RelExp EQUAL AddExp  */
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
#line 2016 "src/parser.cpp"
    break;

  case 66: /* RelExp: RelExp NOTEQUAL AddExp  */
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
#line 2034 "src/parser.cpp"
    break;

  case 67: /* LAndExp: RelExp  */
#line 629 "src/parser.y"
           {(yyval.exprtype) = (yyvsp[0].exprtype);}
#line 2040 "src/parser.cpp"
    break;

  case 68: /* LAndExp: LAndExp AND RelExp  */
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
#line 2058 "src/parser.cpp"
    break;

  case 69: /* LOrExp: LAndExp  */
#line 648 "src/parser.y"
            {(yyval.exprtype) = (yyvsp[0].exprtype);}
#line 2064 "src/parser.cpp"
    break;

  case 70: /* LOrExp: LOrExp OR LAndExp  */
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
#line 2082 "src/parser.cpp"
    break;

  case 71: /* Type: INT  */
#line 666 "src/parser.y"
          {
        (yyval.type) = TypeSystem::intType;
        declType=TypeSystem::intType;
    }
#line 2091 "src/parser.cpp"
    break;

  case 72: /* Type: VOID  */
#line 670 "src/parser.y"
           {
        (yyval.type) = TypeSystem::voidType;
        declType=TypeSystem::voidType;
    }
#line 2100 "src/parser.cpp"
    break;

  case 73: /* Type: FLOAT  */
#line 675 "src/parser.y"
          {
        (yyval.type) = TypeSystem::floatType;
        declType=TypeSystem::floatType;

    }
#line 2110 "src/parser.cpp"
    break;

  case 74: /* ArrInitList: Exp  */
#line 689 "src/parser.y"
    {
        //闁哄鏅滈悷銈夋煂濠婂牊鏅搁柨鐕傛嫹?闂佸憡甯楀姗€濡甸弮鈧粋宥夊幢椤撶姷顦悗娈垮枓閸嬫挸鈹戦纰卞創缂佺粯锕㈠畷妤呭Ψ閵夈儱绶繛杈剧稻閹告悂鍩€椤掑嫭锛熺紒顭掔節閺佹捇鏁撻敓锟�?

        (yyvsp[0].exprtype)->getSymPtr()->setType(declType);
        (yyval.inittype) = new InitNode((yyvsp[0].exprtype));
    }
#line 2121 "src/parser.cpp"
    break;

  case 75: /* ArrInitList: LBRACE ArrInitLists RBRACE  */
#line 697 "src/parser.y"
    {
        //闁哄鏅滈悷銈夋煂濠婂牊鏅搁柨鐕傛嫹?闁荤姴娲ら悺銊ノｉ幋鐐翠氦婵☆垳绮粻鎺楁煠瀹勬澘鏆遍柣顭戝灡缁嬪鍩€椤掑嫭鏅搁柨鐕傛嫹?
        (yyval.inittype) = (yyvsp[-1].inittype);
        (yyval.inittype)->i_m_checkpoint();
    }
#line 2131 "src/parser.cpp"
    break;

  case 76: /* ArrInitLists: ArrInitLists COMMA ArrInitList  */
#line 707 "src/parser.y"
    {
        (yyval.inittype) = new InitNode((yyvsp[-2].inittype), (yyvsp[0].inittype));
    }
#line 2139 "src/parser.cpp"
    break;

  case 77: /* ArrInitLists: ArrInitList  */
#line 712 "src/parser.y"
    {
        (yyval.inittype) = (yyvsp[0].inittype);
    }
#line 2147 "src/parser.cpp"
    break;

  case 78: /* ArrInit: ASSIGN LBRACE ArrInitLists RBRACE  */
#line 720 "src/parser.y"
    {
        (yyval.inittype) = (yyvsp[-1].inittype);
    }
#line 2155 "src/parser.cpp"
    break;

  case 79: /* ArrInit: %empty  */
#line 724 "src/parser.y"
           { (yyval.inittype) = nullptr ;}
#line 2161 "src/parser.cpp"
    break;

  case 80: /* ArrDimension: LBRACKET Exp RBRACKET  */
#line 731 "src/parser.y"
    {
        arr_dimension_recorder++;
        (yyval.arrdimtype) = new ArrDimNode((yyvsp[-1].exprtype),arrDimNode_state);
    }
#line 2170 "src/parser.cpp"
    break;

  case 81: /* ArrDimension: LBRACKET RBRACKET  */
#line 737 "src/parser.y"
    {
        (yyval.arrdimtype) = nullptr;
    }
#line 2178 "src/parser.cpp"
    break;

  case 82: /* ArrDimensions: ArrDimension  */
#line 745 "src/parser.y"
                 { (yyval.arrdimtype) = (yyvsp[0].arrdimtype); }
#line 2184 "src/parser.cpp"
    break;

  case 83: /* ArrDimensions: ArrDimensions ArrDimension  */
#line 748 "src/parser.y"
    {
        (yyval.arrdimtype) = new ArrDimNode((yyvsp[-1].arrdimtype), (yyvsp[0].arrdimtype));
    }
#line 2192 "src/parser.cpp"
    break;

  case 84: /* DeclStmt: VarDeclStmt  */
#line 756 "src/parser.y"
                {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 2198 "src/parser.cpp"
    break;

  case 85: /* DeclStmt: ConstDeclStmt  */
#line 758 "src/parser.y"
                  {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 2204 "src/parser.cpp"
    break;

  case 86: /* VarDeclStmt: Type IdDeclLists SEMICOLON  */
#line 763 "src/parser.y"
                              {(yyval.stmttype)=(yyvsp[-1].stmttype);}
#line 2210 "src/parser.cpp"
    break;

  case 87: /* ConstDeclStmt: CONST Type ConstDeclLists SEMICOLON  */
#line 771 "src/parser.y"
                                       {(yyval.stmttype)=(yyvsp[-1].stmttype);}
#line 2216 "src/parser.cpp"
    break;

  case 88: /* IdDeclLists: IdDeclLists COMMA IdDeclList  */
#line 776 "src/parser.y"
                                 {(yyval.stmttype)=new DeclList((yyvsp[-2].stmttype),(yyvsp[0].stmttype));}
#line 2222 "src/parser.cpp"
    break;

  case 89: /* IdDeclLists: IdDeclList  */
#line 777 "src/parser.y"
                 {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 2228 "src/parser.cpp"
    break;

  case 90: /* IdDeclList: ID ASSIGN InitVal  */
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
#line 2249 "src/parser.cpp"
    break;

  case 91: /* IdDeclList: ID  */
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
#line 2271 "src/parser.cpp"
    break;

  case 92: /* $@6: %empty  */
#line 816 "src/parser.y"
       { arrDimNode_state = ArrDimNode::INIT; }
#line 2277 "src/parser.cpp"
    break;

  case 93: /* IdDeclList: ID $@6 ArrDimensions ArrInit  */
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
#line 2312 "src/parser.cpp"
    break;

  case 94: /* ConstDeclLists: ConstDeclLists COMMA ConstDeclList  */
#line 851 "src/parser.y"
                                       {(yyval.stmttype)=new ConstDeclList((yyvsp[-2].stmttype),(yyvsp[0].stmttype));}
#line 2318 "src/parser.cpp"
    break;

  case 95: /* ConstDeclLists: ConstDeclList  */
#line 852 "src/parser.y"
                    {(yyval.stmttype)=(yyvsp[0].stmttype);}
#line 2324 "src/parser.cpp"
    break;

  case 96: /* ConstDeclList: ID ASSIGN InitVal  */
#line 857 "src/parser.y"
                      {
        SymbolEntry *se;
        //fprintf(stderr, "%d",identifiers->getLevel());//闂佺懓鐏氶幐绋跨暤閸愨晜浜ゆ繛鍡楅叄閸ゅ鏌涘▎鎰惰€块柛锝嗘そ閺屽苯顓奸崨顖涙瘞闂佽法鍣﹂幏锟�?

        se = new IdentifierSymbolEntry(declType, (yyvsp[-2].strtype), identifiers->getLevel(), (yyvsp[0].exprtype));
        identifiers->install((yyvsp[-2].strtype), se);
        (yyval.stmttype) = new ConstDeclInitStmt(new Id(se),(yyvsp[0].exprtype));
        delete [](yyvsp[-2].strtype);
    }
#line 2338 "src/parser.cpp"
    break;

  case 97: /* InitVal: Exp  */
#line 898 "src/parser.y"
        { (yyval.exprtype)=(yyvsp[0].exprtype);}
#line 2344 "src/parser.cpp"
    break;


#line 2348 "src/parser.cpp"

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
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

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
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
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
  ++yynerrs;

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

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
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
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
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
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
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
