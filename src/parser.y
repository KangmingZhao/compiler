%code top{
    #include <iostream>
    #include <assert.h>
    #include "parser.h"
    #include <vector>
    extern Ast ast;

    //ArrDimNode** now_arrdim;
    //int now_arrindex;
    //閹存垳婊戞导姘躬stmt閸氬酣娼伴崝鐘辩瑐BREAK SEMICOLON閻ㄥ嫯鐦戦敓锟�?閿涘瞼鍔ч崥搴″涧閺堝婀猙lockstmt閿燂拷?娴兼熬鎷�?閿熺晫鎮婇弻鎰閿燂拷?娴ｆ粎鏁ら崺鐔告Ц娑撳秵妲搁崷鈺砲ile閸愬懘鍎撮惃鍕┾偓鍌氭礈娑撳搫鍙炬禒鏍畱鐞涖劏鎻敓锟�?
    //鐟曚椒绠為敓锟�?閼虫枻鎷�?閿熷€熴€冩潏鎯х础娑撳秷鍏樼憗鍗媟eak閿涘矉鎷�?閿熸垝绠為敓锟�?瀹稿崬姘ㄩ弰鐥搑eak閿燂拷?
    
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
}

%code requires {
    #include "Ast.h"
    #include "SymbolTable.h"
    #include "Type.h"


}

%initial-action {
    //鏉╂瑩鍣锋潻妯绘箒鏉╂瑱鎷�?閿熻棄鍨垫慨瀣閺傝纭堕敍灞姐亰閻栬棄鏆�
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


/* 鏉╂瑩鍣烽惃鍑TEGER閵嗕笚LOATPOINT閿燂拷?閸忚渹缍嬮弫鐗堝祦閿涘瓥NT閸滃瓗LOAT閿燂拷?閺佺増宓佺猾璇茬€� */

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
%nterm <stmttype> IdDeclLists IdDeclList ConstDeclLists ConstDeclList VarDeclStmt ConstDeclStmt  EmptyStmt
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
    | IfStmt {$$=$1;}
    | ReturnStmt {$$=$1; }
    | DeclStmt {$$=$1; }
    | FuncDef {$$=$1; }
    | WhileStmt { $$ = $1; loop_match_break($1); loop_match_continue($1);}
    | ExprStmt { $$ = $1;}
    | BreakStmt { $$ = $1; }
    | ContinueStmt { $$ = $1; }
    | EmptyStmt {$$=$1;}
    ;
LVal
    : ID {
        int state = 0;
        
        SymbolEntry *se;
        se = identifiers->lookup($1); //閸︺劌鍑￠張澶屾畱缁楋箑褰跨悰銊╁櫡閹电偓婀佸▽鈩冩箒鏉╂瑤閲淚D閿燂拷?
        if(se == nullptr) //婵″倹鐏夊▽鈩冩箒
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);//闁瑰灚鎸稿畵鍐╂交濞嗗酣鍤嬮柛娆愶耿閸ｅ搫鈻介埄鍐╃畳閻庤鐭粻锟�
            state = NOT_DEFINED;
            //delete [](char*)$1;
            //assert(se != nullptr);      //閹舵稑鍤稉鈧敓锟�?閿燂拷?鐟封偓闁挎瑱鎷�??


            SymbolEntry *error_se;
            error_se = new IdentifierSymbolEntry(new ERROR_OCUPIER(), $1, identifiers->getLevel());
            $$ = new Id(error_se, state);    //缂佹瑨绻栭敓锟�?$$鐠у绔存稉鐙狣鐎涙劗琚惃鍕€冩潏鎯х础缂佹挾鍋ｉ妴鍌滄暏閺夈儴绶崙铏规畱/
            delete []$1;

        }
        else
        {
            $$ = new Id(se, state);    //缂佹瑨绻栭敓锟�?$$鐠у绔存稉鐙狣鐎涙劗琚惃鍕€冩潏鎯х础缂佹挾鍋ｉ妴鍌滄暏閺夈儴绶崙铏规畱/
            delete []$1;
        }
    }
    |
    ID
    {
        arrDimNode_state = ArrDimNode::ACCESS;
    }
    ArrDimensions
    {
        //閹存垳婊戦惌銉╀壕閺佹壆绮嶉惃鍕舵嫹?閿熶粙妫堕敓锟�?閿燂拷?娴犮儰缍旀稉鍝勪箯閸婅偐娈戦敓锟�?
        int state = 0;

        SymbolEntry *se;
        se = identifiers->lookup($1); //閸︺劌鍑￠張澶屾畱缁楋箑褰跨悰銊╁櫡閹电偓婀佸▽鈩冩箒鏉╂瑤閲淚D閿燂拷?
        if(se == nullptr) //婵″倹鐏夊▽鈩冩箒
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);//闁瑰灚鎸稿畵鍐╂交濞嗗酣鍤嬮柛娆愶耿閸ｅ搫鈻介埄鍐╃畳閻庤鐭粻锟�
            state = NOT_DEFINED;
            //delete [](char*)$1;
            //assert(se != nullptr);      //閹舵稑鍤稉鈧敓锟�?閿燂拷?鐟封偓闁挎瑱鎷�??

            SymbolEntry *error_se;
            error_se = new IdentifierSymbolEntry(new ERROR_OCUPIER(), $1, identifiers->getLevel());
            $$ = new Id(error_se, state);    //缂佹瑨绻栭敓锟�?$$鐠у绔存稉鐙狣鐎涙劗琚惃鍕€冩潏鎯х础缂佹挾鍋ｉ妴鍌滄暏閺夈儴绶崙铏规畱/
            delete []$1;
        }
        else
        {
            $$ = new Id(se, $3, state);    //缂佹瑨绻栭敓锟�?$$鐠у绔存稉鐙狣鐎涙劗琚惃鍕€冩潏鎯х础缂佹挾鍋ｉ妴鍌滄暏閺夈儴绶崙铏规畱/
            if(se->get_arr_dimension_recorder() < arr_dimension_recorder)
                fprintf(stderr, "array \"%s\" has max dimension \"%d\" but accessed by \"%d\" \n", (char*)$1,se->get_arr_dimension_recorder(), arr_dimension_recorder );//閹垫挸宓冩潻娆庨嚋鐠佸潡妫剁搾鍛扮箖閿燂拷?
            arr_dimension_recorder = 0;
            delete []$1;
        }
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
    |
    LBRACE RBRACE
    {
        $$ = new EmptyStmt();
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
    : WHILE
    {
        Break_stack_ok_2_push_back++;
        Continue_stack_ok_2_push_back++;
    }
    LPAREN Cond RPAREN Stmt 
    {
         $$ = new WhileStmt($4, $6);
    }
    ;
BreakStmt 
    :
    BREAK SEMICOLON
    {
        BreakStmt* temp = new BreakStmt();
        $$ = temp;
        if(Break_stack_ok_2_push_back)
        {
            Break_stack.push_back(temp);
            Break_stack_ok_2_push_back--;
        }
    }

ContinueStmt
    :
    CONTINUE SEMICOLON
    {
        ContinueStmt * temp = new ContinueStmt();
        $$ = temp;
        if(Continue_stack_ok_2_push_back)
        {
            Continue_stack.push_back(temp);
            Continue_stack_ok_2_push_back--;
        }
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
    |
    RETURN SEMICOLON{
        $$ = new ReturnStmt();
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

// 閻犲洤妫楅崺锟�;
EmptyStmt
    :
     SEMICOLON{
        $$ = new EmptyStmt();
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
        if(se == nullptr) //濠碘€冲€归悘澶娾柦閳╁啯绠�
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);//闁瑰灚鎸稿畵鍐╂交濞嗗酣鍤嬮柛娆愶耿閸ｅ搫鈻介埄鍐╃畳閻庤鐭粻锟�
            delete [](char*)$1;
            assert(se != nullptr);      //閹舵稑鍤稉鈧敓锟�?閿燂拷?鐟封偓闁挎瑱鎷�??
        }
        // SymbolEntry* temp=new TemporarySymbolEntry(se->getType(),SymbolTable::getLabel());
        $$ = new FunctCall(se, $3);
    }
    ;
// 闁告垼濮ら弳鐔衡偓瑙勭煯缁狅拷 Funcdef->
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
// 闁告娲滃ú鐗堟交閹邦喚鏆�
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
        //鏉╂瑩鍣烽幋鎴滄粦娴犺濮熼敓锟�?鐟曚焦婀佹稉鈧敓锟�?閺勭棞loat闁絼绠炵亸杈ㄥ瘻float閺夈儳鐣婚敓锟�?
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
        //濮瑰倷缍戦弫鐗堟Цint閻ㄥ嫪绗撻敓锟�?
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
        //se = identifiers->lookup($1->get_name()); //閸︺劌鍑￠張澶屾畱缁楋箑褰跨悰銊╁櫡閹电偓婀佸▽鈩冩箒鏉╂瑤閲淟val閿燂拷?
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
        //鏉╂瑤閲滈敓锟�?閸掓澘銇旀禍鍡礉瀵偓婵绻橀崗銉ュ徔娴ｆ挸鈧棿绨￠敓锟�?

        $1->getSymPtr()->setType(declType);
        $$ = new InitNode($1);
    }
    |
    LBRACE ArrInitLists RBRACE
    {
        //鏉╂瑤閲滈敓锟�?鐠囧瓨妲戞潻妯绘箒閼峰啿鐨稉鈧敓锟�?
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
        arr_dimension_recorder++;
        $$ = new ArrDimNode($2,arrDimNode_state);
    }
    |
    LBRACKET RBRACKET
    {
        $$ = nullptr;
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

// 閻㈩垱鎮傞崳娲椽鐏炶棄缍侀梺鎻掔箳濞堟垶绔熼悧鍫燁潠
DeclStmt
     :
    VarDeclStmt {$$=$1;}
    |
    ConstDeclStmt {$$=$1;}
    ;
// 閸欐﹢鍣� 閸欐﹢鍣�+娑撯偓閿燂拷?
VarDeclStmt
    :
    Type IdDeclLists SEMICOLON{$$=$2;}
    
        ;


// 閻㈩垱鎮傞崳锟� const +濞戞挴鍋撻柛顐㈡４缁憋拷
ConstDeclStmt
    :
    CONST Type ConstDeclLists SEMICOLON{$$=$3;}
    ;
// 闁告瑦锕㈤崳锟�
IdDeclLists 
    :
    IdDeclLists COMMA IdDeclList {$$=new DeclList($1,$3);}
    | IdDeclList {$$=$1;}
    ;
IdDeclList
    :
    ID ASSIGN InitVal{
        int state = LEGAL_VAR;
        
        SymbolEntry *check_redefination_se;
        check_redefination_se = identifiers->lookup_in_present_domain($1); //閸︺劌鍑￠張澶屾畱缁楋箑褰跨悰銊╁櫡閹电偓婀佸▽鈩冩箒鏉╂瑤閲淚D閿燂拷?
        if(check_redefination_se != nullptr) //婵″倹鐏夊▽鈩冩箒
        {
            fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)$1);//閹垫挸宓冩潻娆庨嚋閸欐﹢鍣洪柌宥呯暰閿燂拷?
            state = REDEFINATION;
        }
        SymbolEntry *se;
        se = new IdentifierSymbolEntry(declType, $1, identifiers->getLevel());
        identifiers->install($1, se);
        $$ = new DeclInitStmt(new Id(se, state),$3);
        delete []$1;
    }
    |
    ID{
        int state = LEGAL_VAR;
        
        SymbolEntry *check_redefination_se;
        check_redefination_se = identifiers->lookup_in_present_domain($1); //閸︺劌鍑￠張澶屾畱缁楋箑褰跨悰銊╁櫡閹电偓婀佸▽鈩冩箒鏉╂瑤閲淚D閿燂拷?
        if(check_redefination_se != nullptr) //婵″倹鐏夊▽鈩冩箒
        {
            fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)$1);//閹垫挸宓冩潻娆庨嚋閸欐﹢鍣洪柌宥呯暰閿燂拷?
            state = REDEFINATION;
        }

        SymbolEntry *se;
        se = new IdentifierSymbolEntry(declType, $1, identifiers->getLevel());
        identifiers->install($1, se);
        $$ = new DeclStmt(new Id(se, state));
        delete []$1;
    }
    |
    ID { arrDimNode_state = ArrDimNode::INIT; } ArrDimensions ArrInit  {

            int state = LEGAL_VAR;
        
            SymbolEntry *check_redefination_se;
            check_redefination_se = identifiers->lookup_in_present_domain($1); //閸︺劌鍑￠張澶屾畱缁楋箑褰跨悰銊╁櫡閹电偓婀佸▽鈩冩箒鏉╂瑤閲淚D閿燂拷?
            if(check_redefination_se != nullptr) //婵″倹鐏夊▽鈩冩箒
            {
                fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)$1);//閹垫挸宓冩潻娆庨嚋閸欐﹢鍣洪柌宥呯暰閿燂拷?
                state = REDEFINATION;
            }



            SymbolEntry *se;
            if(declType->isInt())
            {
                INT_arrayType * temp = new INT_arrayType();
                se = new IdentifierSymbolEntry(temp, $1, identifiers->getLevel());
            }
            else
            {
                FLOAT_arrayType * temp = new FLOAT_arrayType();
                se = new IdentifierSymbolEntry(temp, $1, identifiers->getLevel());
            }
            identifiers->install($1, se);
            $$ = new DeclStmt(new Id(se, $3, $4, state, arr_dimension_recorder));
            arr_dimension_recorder = 0;
            delete []$1;
        }
    ;

// 閻㈩垱鎮傞崳锟�
ConstDeclLists 
    :
    ConstDeclLists COMMA ConstDeclList {$$=new ConstDeclList($1,$3);}
    | ConstDeclList {$$=$1;}
    ;

ConstDeclList
    :
    ID ASSIGN InitVal {
        SymbolEntry *se;
        se = new IdentifierSymbolEntry(declType, $1, identifiers->getLevel(), $3);
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
