%code top{
    #include <iostream>
    #include <assert.h>
    #include "parser.h"
    extern Ast ast;

    //ArrDimNode** now_arrdim;
    //int now_arrindex;
    int yylex();
    int yyerror( char const * );
}

%code requires {
    #include "Ast.h"
    #include "SymbolTable.h"
    #include "Type.h"
}

%union {
    int itype;
    char* strtype;
    StmtNode* stmttype;
    ExprNode* exprtype;
    ArrDimNode * arrdimtype;
    InitNode * inittype;
    Type* type;
}

%start Program
%token <strtype> ID 
%token <itype> INTEGER
%token IF ELSE WHILE
%token INT VOID FLOAT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET SEMICOLON COMMA
%token ADD SUB MUL DIV MOD OR AND LESS GREATER ASSIGN INCREMENT DECREMENT LESSEQUAL GREATEREQUAL EQUAL NOTEQUAL
%token RETURN





%nterm <stmttype> Stmts Stmt AssignStmt BlockStmt IfStmt ReturnStmt DeclStmt FuncDef WhileStmt ExprStmt
%nterm <exprtype> Exp AddExp MulExp Cond LOrExp PrimaryExp LVal RelExp LAndExp UnaryExp
%nterm <arrdimtype> ArrDimensions ArrDimension 
%nterm <inittype> ArrInit ArrInitLists ArrInitList
%nterm <type> Type

%precedence THEN
%precedence ELSE
%%
Program
    : Stmts {
        ast.setRoot($1);
    }
    ;
Stmts
    : Stmt {$$=$1;}
    | Stmts Stmt{
        $$ = new SeqNode($1, $2);
    }
    ;
Stmt
    : AssignStmt {$$=$1;}
    | BlockStmt {$$=$1;}
    | IfStmt {$$=$1;}
    | ReturnStmt {$$=$1;}
    | DeclStmt {$$=$1;}
    | FuncDef {$$=$1;}
    | WhileStmt { $$ = $1; }
    | ExprStmt { $$ = $1; }
    ;
LVal
    : ID {
        SymbolEntry *se;
        se = identifiers->lookup($1); //在已有的符号表里找有没有这个ID。
        if(se == nullptr) //如果没有
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);//打印这个变量没有定义
            delete [](char*)$1;
            assert(se != nullptr);      //抛出一个断言错误
        }
        $$ = new Id(se);    //给这里$$赋一个ID子类的表达式结点。用来输出的/
        delete []$1;
    }
    |
    ID ArrDimensions
    {
        //我们知道数组的访问是可以作为左值的。
        
        SymbolEntry *se;
        se = identifiers->lookup($1); //在已有的符号表里找有没有这个ID。
        if(se == nullptr) //如果没有
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);//打印这个变量没有定义
            delete [](char*)$1;
            assert(se != nullptr);      //抛出一个断言错误
        }
        $$ = new Id(se, $2);    //给这里$$赋一个ID子类的表达式结点。用来输出的/
        delete []$1;
    }
    ;
AssignStmt
    :
    LVal ASSIGN Exp SEMICOLON {
        $$ = new AssignStmt($1, $3);
    }
    ;
BlockStmt
    :   LBRACE 
        {identifiers = new SymbolTable(identifiers);} 
        Stmts RBRACE 
        {
            $$ = new CompoundStmt($3);
            SymbolTable *top = identifiers;
            identifiers = identifiers->getPrev();
            delete top;
        }
    ;
IfStmt
    : IF LPAREN Cond RPAREN Stmt %prec THEN {
        $$ = new IfStmt($3, $5);
    }
    | IF LPAREN Cond RPAREN Stmt ELSE Stmt {
        $$ = new IfElseStmt($3, $5, $7);
    }
    ;

WhileStmt
    : WHILE LPAREN Cond RPAREN Stmt 
    {
         $$ = new WhileStmt($3, $5);
    }
    ;
ExprStmt
    : Exp SEMICOLON {
    
    }
    ;

ReturnStmt
    :
    RETURN Exp SEMICOLON{
        $$ = new ReturnStmt($2);
    }
    ;

Exp
    :
    AddExp {$$ = $1;}
    ;
Cond
    :
    LOrExp {$$ = $1;}
    ;
PrimaryExp
    :
    LVal {
        $$ = $1;
    }
    |
    LPAREN Exp RPAREN{
        $$=$2;
    }
    | INTEGER {
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::intType, $1);
        $$ = new Constant(se);
    }
    ;
// 单目运算
UnaryExp
    :
    PrimaryExp {
        $$ = $1;
    }
    | 
    ADD UnaryExp {
        $$ = $2;
    }
    |
    SUB UnaryExp {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new UnaryExpr(se, UnaryExpr::SUB, $2);
    }
    ;
MulExp
    :
    UnaryExp { $$ = $1;}
    |
    MulExp MUL UnaryExp {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MUL, $1, $3);
    }
    |
    MulExp DIV  UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::DIV, $1, $3);
    }
    |
    MulExp MOD UnaryExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MOD, $1, $3);
    }
    ;

AddExp
    :
    Exp INCREMENT
    {
        //SymbolEntry *const_1 = new ConstantSymbolEntry(TypeSystem::intType, 1);
        //ExprNode *const_1_node = new Constant(const_1);

        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::INCREMENT_AFTER, $1);

        //SymbolEntry *se;
        //se = identifiers->lookup($1->get_name()); //在已有的符号表里找有没有这个Lval。
        //$$ = new BinaryExpr(se, BinaryExpr::ADD, $1, const_1_node);
    }
    |
    Exp DECREMENT
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::DECREMENT_AFTER, $1);
    }
    |
    MulExp { $$ = $1; }
    |
    AddExp ADD MulExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::ADD, $1, $3);
    }
    |
    AddExp SUB MulExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::SUB, $1, $3);
    }
    ;


RelExp
    :
    AddExp {$$ = $1;}
    |
    RelExp LESS AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::LESS, $1, $3);
    }
    |
     RelExp GREATER AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::GREATER, $1, $3);
    }
    |
    RelExp GREATEREQUAL AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::GREATEREQUAL, $1, $3);
    }
    |
    RelExp LESSEQUAL AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::LESSEQUAL, $1, $3);
    }
    |
    RelExp EQUAL AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::EQUAL, $1, $3);
    }
    |
    RelExp NOTEQUAL AddExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::NOTEQUAL, $1, $3);
    }
    ;
LAndExp
    :
    RelExp {$$ = $1;}
    |
    LAndExp AND RelExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::AND, $1, $3);
    }
    ;
LOrExp
    :
    LAndExp {$$ = $1;}
    |
    LOrExp OR LAndExp
    {
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::OR, $1, $3);
    }
    ;
Type
    : INT {
        $$ = TypeSystem::intType;
    }
    | VOID {
        $$ = TypeSystem::voidType;
    }
    ;





ArrInitList
    :
    Exp
    {
        //这个是到头了，开始进入具体值了。
        $$ = new InitNode($1);
    }
    |
    LBRACE ArrInitLists RBRACE
    {
        //这个是说明还有至少一层
        $$ = $2;
        $$->i_m_checkpoint();
    }
    ;

ArrInitLists
    :
    ArrInitLists COMMA ArrInitList
    {
        $$ = new InitNode($1, $3);
    }
    |
    ArrInitList
    {
        $$ = $1;
    }
    ;

ArrInit
    :
    ASSIGN LBRACE ArrInitLists RBRACE
    {
        $$ = $3;
    }
    |
    %empty { $$ = nullptr ;}
    ;

    
ArrDimension
    :
    LBRACKET Exp RBRACKET
    {
        $$ = new ArrDimNode($2);
    }
    ;


ArrDimensions
    :
    ArrDimension { $$ = $1; }
    |
    ArrDimensions ArrDimension
    {
        $$ = new ArrDimNode($1, $2);
    }
    ;

DeclStmt
    :
    Type ID SEMICOLON {
        SymbolEntry *se;
        se = new IdentifierSymbolEntry($1, $2, identifiers->getLevel());
        identifiers->install($2, se);
        $$ = new DeclStmt(new Id(se));
        delete []$2;
    }
    |
    Type ID ArrDimensions ArrInit SEMICOLON {
        INT_arrayType * temp = new INT_arrayType();
        SymbolEntry *se;
        se = new IdentifierSymbolEntry(temp, $2, identifiers->getLevel());
        identifiers->install($2, se);
        $$ = new DeclStmt(new Id(se, $3, $4));
        delete []$2;
    }
    ;






FuncDef
    :
    Type ID {
        Type *funcType;
        funcType = new FunctionType($1,{});
        SymbolEntry *se = new IdentifierSymbolEntry(funcType, $2, identifiers->getLevel());
        identifiers->install($2, se);
        identifiers = new SymbolTable(identifiers);
    }
    LPAREN RPAREN

    BlockStmt
    {
        SymbolEntry *se;
        se = identifiers->lookup($2);
        assert(se != nullptr);
        $$ = new FunctionDef(se, $6);
        SymbolTable *top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
        delete []$2;
    }
    ;
%%

int yyerror(char const* message)
{
    std::cerr<<message<<std::endl;
    return -1;
}
// 
