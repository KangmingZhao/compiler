%code top{
    #include <iostream>
    #include <assert.h>
    #include "parser.h"
    #include <vector>
    extern Ast ast;

    //ArrDimNode** now_arrdim;
    //int now_arrindex;
    //我们会在stmt后面加上BREAK SEMICOLON的识别，然后只有在blockstmt中会处理某一个作用域是不是在while内部的。因为其他的表达式
    //要么只能装表达式不能装break，要么自己就是break。
    
    BreakStmt * now_break_stmt = nullptr;
    ContinueStmt * now_continue_stmt = nullptr;



    void deal_continue(bool is_loop)
    {
        if(now_continue_stmt != nullptr)
        {
            if(is_loop)
            {
                now_continue_stmt -> i_m_loop();
                now_continue_stmt -> need_interrupt = 1;
            }
            else
            {
                now_continue_stmt -> i_m_not_loop();
                now_continue_stmt -> need_interrupt = 0;
            }
            now_continue_stmt = nullptr;
        }
    }
    void deal_break(bool is_loop)
    {
        if(now_break_stmt != nullptr)
        {
            if(is_loop)
            {
                now_break_stmt -> i_m_loop();
                now_break_stmt -> need_interrupt = 1;
            }
            else
            {
                now_break_stmt -> i_m_not_loop();
                now_break_stmt -> need_interrupt = 0;
            }
            now_break_stmt = nullptr;
        }
    }

    Type  *declType;
    std::vector<Type*> FuncParamsVector;
    int yylex();
    int yyerror( char const * );


    
    
    
    

}

%code requires {
    #include "Ast.h"
    #include "SymbolTable.h"
    #include "Type.h"

}

%initial-action {
    //这里还有这种初始化方法，太爽啦
    std::string getint = "getint";
    Type* funcType1 = new FunctionType(TypeSystem::intType, {});
    SymbolEntry* entry1 = new IdentifierSymbolEntry(funcType1, getint, 0);
    identifiers->install(getint, entry1);
    
    std::string putint = "putint";
    Type* funcType2 = new FunctionType(TypeSystem::voidType, {});
    SymbolEntry* entry2 = new IdentifierSymbolEntry(funcType2, putint, 0);
    identifiers->install(putint, entry2);

    
    std::string putch = "putch";
    std::vector<Type*> vec; 
    vec.push_back(TypeSystem::intType);
    Type* funcType3 = new FunctionType(TypeSystem::voidType, vec); 
    SymbolEntry* entry3 = new IdentifierSymbolEntry(funcType3, putch, 0);
    identifiers->install(putch, entry3);


}

%union {
    int itype;
    float fltype;
    char* strtype;
    StmtNode* stmttype;
    ExprNode* exprtype;
    ArrDimNode * arrdimtype;
    InitNode * inittype;
    ParaNode * paratype;
    Type* type;

}


/* 这里的INTEGER、FLOATPOINT是具体数据，INT和FLOAT是数据类型 */

%start Program
%token <strtype> ID 
%token <itype> INTEGER
%token <fltype> FLOATPOINT
%token IF ELSE WHILE
%token INT VOID FLOAT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET SEMICOLON COMMA NOT
%token ADD SUB MUL DIV MOD OR AND LESS GREATER ASSIGN INCREMENT DECREMENT LESSEQUAL GREATEREQUAL EQUAL NOTEQUAL
%token RETURN CONST BREAK CONTINUE





%nterm <stmttype> Stmts Stmt AssignStmt BlockStmt IfStmt ReturnStmt DeclStmt  WhileStmt ExprStmt BreakStmt ContinueStmt
%nterm <exprtype> Exp AddExp MulExp Cond LOrExp PrimaryExp LVal RelExp LAndExp UnaryExp InitVal FunctCall 
%nterm <stmttype> IdDeclLists IdDeclList ConstDeclLists ConstDeclList VarDeclStmt ConstDeclStmt 
%nterm <arrdimtype> ArrDimensions ArrDimension 
%nterm <inittype> ArrInit ArrInitLists ArrInitList
%nterm <stmttype> FuncDef 
%nterm <paratype> PARAMENT_LISTS PARAMENT_LIST
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
    | IfStmt {$$=$1; deal_break(0); deal_continue(0); }
    | ReturnStmt {$$=$1; }
    | DeclStmt {$$=$1; }
    | FuncDef {$$=$1; }
    | WhileStmt { $$ = $1;  deal_break(1); deal_continue(1); }
    | ExprStmt { $$ = $1;}
    | BreakStmt { $$ = $1; }
    | ContinueStmt { $$ = $1; }
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
        {identifiers = new SymbolTable(identifiers); 
        } 
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
BreakStmt 
    :
    BREAK SEMICOLON
    {
        BreakStmt* temp = new BreakStmt(0);
        now_break_stmt = temp;
        $$ = temp;
    }

ContinueStmt
    :
    CONTINUE SEMICOLON
    {
        ContinueStmt * temp = new ContinueStmt(0);
        now_continue_stmt = temp;
        $$ = temp;
    }

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
    | FLOATPOINT {
        SymbolEntry *se = new ConstantSymbolEntry(TypeSystem::floatType, $1);
        $$ = new Constant(se);
    }
    |
    FunctCall 
    {
        $$ = $1;
    }
    ;



PARAMENT_LISTS
    :
    PARAMENT_LISTS COMMA PARAMENT_LIST
    {
        $$ = new ParaNode($1,$3);
    }
    |
    PARAMENT_LIST
    {
        $$ = $1;
    }
    ;

PARAMENT_LIST
    :
    Exp
    {
        $$ = new ParaNode($1);
    }
    |
    %empty { $$ = nullptr ;}
    |
    Type ID {
        SymbolEntry *se;
        se = new IdentifierSymbolEntry($1, $2, identifiers->getLevel());
        identifiers->install($2, se);
        $$ = new ParaNode(new Id(se));
        delete []$2;
        FuncParamsVector.push_back($1);
    }
    ;


FunctCall
    :
    ID LPAREN PARAMENT_LISTS RPAREN
    {   
        SymbolEntry *se;
        se = identifiers->lookup($1); 
        if(se == nullptr) //如果没有
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);//打印这个变量没有定义
            delete [](char*)$1;
            assert(se != nullptr);      //抛出一个断言错误
        }
        $$ = new FunctCall(se, $3);
    }
    ;
// 函数定义 Funcdef->
FuncDef
    :
    Type ID{
        SymbolEntry *se = new IdentifierSymbolEntry(nullptr, $2, identifiers->getLevel());
        identifiers->install($2, se);
        identifiers = new SymbolTable(identifiers);
    }
    LPAREN PARAMENT_LISTS RPAREN
    {
        FunctionType *funcType;
        funcType=new FunctionType($1,{});
        FuncParamsVector.swap(funcType->paramsType);

        SymbolEntry *se;
        se = identifiers->lookup($2);
        IdentifierSymbolEntry* ss=(IdentifierSymbolEntry*)se;
        ss->setFuncType(((Type*)funcType));
    }
    BlockStmt
    {
        SymbolEntry *se;
        se = identifiers->lookup($2);
        assert(se != nullptr);
        $$ = new FunctionDef(se, $8,$5);
        SymbolTable *top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
        delete []$2;
    }
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
        if($2->get_symbolEntry()->getType()->isInt())
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            $$ = new UnaryExpr(se, UnaryExpr::SUB, $2);
        }
        else if($2->get_symbolEntry()->getType()->isFLOAT())
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            $$ = new UnaryExpr(se, UnaryExpr::SUB, $2);
        }
    }
    |
    NOT UnaryExp{
    if($2->get_symbolEntry()->getType()->isInt())
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            $$ = new UnaryExpr(se, UnaryExpr::NOT, $2);
        }
        else if($2->get_symbolEntry()->getType()->isFLOAT())
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            $$ = new UnaryExpr(se, UnaryExpr::NOT, $2);
        }

    }
    ;
MulExp
    :
    UnaryExp { $$ = $1;}
    |
    MulExp MUL UnaryExp {
        //这里我们任务只要有一个是float那么就按float来算。
        if($3->get_symbolEntry()->getType()->isFLOAT() || $1->get_symbolEntry()->getType()->isFLOAT())
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::MUL, $1, $3);
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::MUL, $1, $3);
        }
    }
    |
    MulExp DIV  UnaryExp
    {
        if($3->get_symbolEntry()->getType()->isFLOAT() || $1->get_symbolEntry()->getType()->isFLOAT())
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::DIV, $1, $3);
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::DIV, $1, $3);
        }
    }
    |
    MulExp MOD UnaryExp
    {
        //求余数是int的专利
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


        if($1->get_symbolEntry()->getType()->isFLOAT())
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::INCREMENT_AFTER, $1);
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::INCREMENT_AFTER, $1);
        }

        

        //SymbolEntry *se;
        //se = identifiers->lookup($1->get_name()); //在已有的符号表里找有没有这个Lval。
        //$$ = new BinaryExpr(se, BinaryExpr::ADD, $1, const_1_node);
    }
    |
    Exp DECREMENT
    {
        if($1->get_symbolEntry()->getType()->isFLOAT())
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::DECREMENT_AFTER, $1);
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::DECREMENT_AFTER, $1);
        }
    }
    |
    MulExp { $$ = $1; }
    |
    AddExp ADD MulExp
    {
        if($1->get_symbolEntry()->getType()->isFLOAT() || $3->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::ADD, $1, $3);
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::ADD, $1, $3);
        }
    }
    |
    AddExp SUB MulExp
    {
        
        if($1->get_symbolEntry()->getType()->isFLOAT() || $3->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::SUB, $1, $3);
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::SUB, $1, $3);
        }
    }
    ;


RelExp
    :
    AddExp {$$ = $1;}
    |
    RelExp LESS AddExp
    {
        if($1->get_symbolEntry()->getType()->isFLOAT() || $3->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::LESS, $1, $3);
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::LESS, $1, $3);
        }
    }
    |
     RelExp GREATER AddExp
    {
        
        if($1->get_symbolEntry()->getType()->isFLOAT() || $3->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::GREATER, $1, $3);
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::GREATER, $1, $3);
        }
    }
    |
    RelExp GREATEREQUAL AddExp
    {
        
        if($1->get_symbolEntry()->getType()->isFLOAT() || $3->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::GREATEREQUAL, $1, $3);
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::GREATEREQUAL, $1, $3);
        }
    }
    |
    RelExp LESSEQUAL AddExp
    {
        
        if($1->get_symbolEntry()->getType()->isFLOAT() || $3->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::LESSEQUAL, $1, $3);
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::LESSEQUAL, $1, $3);
        }
    }
    |
    RelExp EQUAL AddExp
    {
        
        if($1->get_symbolEntry()->getType()->isFLOAT() || $3->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::EQUAL, $1, $3);
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::EQUAL, $1, $3);
        }
    }
    |
    RelExp NOTEQUAL AddExp
    {
        
        if($1->get_symbolEntry()->getType()->isFLOAT() || $3->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::NOTEQUAL, $1, $3);
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::NOTEQUAL, $1, $3);
        }
    }
    ;
LAndExp
    :
    RelExp {$$ = $1;}
    |
    LAndExp AND RelExp
    {
        
        if($1->get_symbolEntry()->getType()->isFLOAT() || $3->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::AND, $1, $3);
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::AND, $1, $3);
        }
    }
    ;
LOrExp
    :
    LAndExp {$$ = $1;}
    |
    LOrExp OR LAndExp
    {
        
        if($1->get_symbolEntry()->getType()->isFLOAT() || $3->get_symbolEntry()->getType()->isFLOAT() )
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::floatType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::OR, $1, $3);
        }
        else
        {
            SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            $$ = new BinaryExpr(se, BinaryExpr::OR, $1, $3);
        }
    }
    ;
Type
    : INT {
        $$ = TypeSystem::intType;
        declType=TypeSystem::intType;
    }
    | VOID {
        $$ = TypeSystem::voidType;
        declType=TypeSystem::voidType;
    }
    |
    FLOAT {
        $$ = TypeSystem::floatType;
        declType=TypeSystem::floatType;

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

// 常量和变量的声明
DeclStmt
     :
    VarDeclStmt {$$=$1;}
    |
    ConstDeclStmt {$$=$1;}
    ;
// 变量 变量+一堆
VarDeclStmt
    :
    Type IdDeclLists SEMICOLON{$$=$2;}
    |
    Type ID ArrDimensions ArrInit SEMICOLON {
            SymbolEntry *se;
            if($1->isInt())
            {
                INT_arrayType * temp = new INT_arrayType();
                se = new IdentifierSymbolEntry(temp, $2, identifiers->getLevel());
            }
            else
            {
                FLOAT_arrayType * temp = new FLOAT_arrayType();
                se = new IdentifierSymbolEntry(temp, $2, identifiers->getLevel());
            }
            identifiers->install($2, se);
            $$ = new DeclStmt(new Id(se, $3, $4));
            delete []$2;
        }
        ;


// 常量 const +一堆；
ConstDeclStmt
    :
    CONST Type ConstDeclLists SEMICOLON{$$=$3;}
    ;
// 变量
IdDeclLists 
    :
    IdDeclLists COMMA IdDeclList {$$=new DeclList($1,$3);}
    | IdDeclList {$$=$1;}
    ;
IdDeclList
    :
    ID ASSIGN InitVal{
        SymbolEntry *se;
        se = new IdentifierSymbolEntry(declType, $1, identifiers->getLevel());
        identifiers->install($1, se);
        $$ = new DeclInitStmt(new Id(se),$3);
        delete []$1;
    }
    |
    ID{
        SymbolEntry *se;
        se = new IdentifierSymbolEntry(declType, $1, identifiers->getLevel());
        identifiers->install($1, se);
        $$ = new DeclStmt(new Id(se));
        delete []$1;
    }
    ;

// 常量
ConstDeclLists 
    :
    ConstDeclLists COMMA ConstDeclList {$$=new ConstDeclList($1,$3);}
    | ConstDeclList {$$=$1;}
    ;

ConstDeclList
    :
    ID ASSIGN InitVal {
        SymbolEntry *se;
        se = new IdentifierSymbolEntry(declType, $1, identifiers->getLevel());
        identifiers->install($1, se);
        $$ = new ConstDeclInitStmt(new Id(se),$3);
        delete []$1;
    }
    ;

// ----------------------------------
// DeclStmt
//     :
//     Type ID SEMICOLON {
//         SymbolEntry *se;
//         se = new IdentifierSymbolEntry($1, $2, identifiers->getLevel());
//         identifiers->install($2, se);
//         $$ = new DeclStmt(new Id(se));
//         delete []$2;
//     }
//     |
//     Type ID ArrDimensions ArrInit SEMICOLON {
//         SymbolEntry *se;
//         if($1->isInt())
//         {
//             INT_arrayType * temp = new INT_arrayType();
//             se = new IdentifierSymbolEntry(temp, $2, identifiers->getLevel());
//         }
//         else
//         {
//             FLOAT_arrayType * temp = new FLOAT_arrayType();
//             se = new IdentifierSymbolEntry(temp, $2, identifiers->getLevel());
//         }
//         identifiers->install($2, se);
//         $$ = new DeclStmt(new Id(se, $3, $4));
//         delete []$2;
//     }
//     ;
InitVal
    :
    Exp { $$=$1;}
    ;




// FuncDef
//     :
//     Type ID {
//         Type *funcType;
//         funcType = new FunctionType($1,{});
//         SymbolEntry *se = new IdentifierSymbolEntry(funcType, $2, identifiers->getLevel());
//         identifiers->install($2, se);
//         identifiers = new SymbolTable(identifiers);
//     }
//     LPAREN RPAREN
//     {
      
//     }
//     BlockStmt
//     {
//         SymbolEntry *se;
//         se = identifiers->lookup($2);
//         assert(se != nullptr);
//         $$ = new FunctionDef(se, $6);
//         SymbolTable *top = identifiers;
//         identifiers = identifiers->getPrev();
//         delete top;
//         delete []$2;
//     }
//     ;
%%

int yyerror(char const* message)
{
    std::cerr<<message<<std::endl;
    return -1;
}
// 
