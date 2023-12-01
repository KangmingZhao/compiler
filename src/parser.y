%code top{
    #include <iostream>
    #include <assert.h>
    #include "parser.h"
    #include <vector>
    extern Ast ast;

    //ArrDimNode** now_arrdim;
    //int now_arrindex;
    //鎴戜滑浼氬湪stmt鍚庨潰鍔犱笂BREAK SEMICOLON鐨勮瘑鍒紝鐒跺悗鍙湁鍦╞lockstmt涓細澶勭悊鏌愪竴涓綔鐢ㄥ煙鏄笉鏄湪while鍐呴儴鐨勩€傚洜涓哄叾浠栫殑琛ㄨ揪寮�
    //瑕佷箞鍙兘瑁呰〃杈惧紡涓嶈兘瑁卋reak锛岃涔堣嚜宸卞氨鏄痓reak銆�
    
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
    //杩欓噷杩樻湁杩欑鍒濆鍖栨柟娉曪紝澶埥鍟�
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


/* 杩欓噷鐨処NTEGER銆丗LOATPOINT鏄叿浣撴暟鎹紝INT鍜孎LOAT鏄暟鎹被鍨� */

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
        se = identifiers->lookup($1); //鍦ㄥ凡鏈夌殑绗﹀彿琛ㄩ噷鎵炬湁娌℃湁杩欎釜ID銆�
        if(se == nullptr) //濡傛灉娌℃湁
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);//鎵撳嵃杩欎釜鍙橀噺娌℃湁瀹氫箟
            state = NOT_DEFINED;
            //delete [](char*)$1;
            //assert(se != nullptr);      //鎶涘嚭涓€涓柇瑷€閿欒


            SymbolEntry *error_se;
            error_se = new IdentifierSymbolEntry(new ERROR_OCUPIER(), $1, identifiers->getLevel());
            $$ = new Id(error_se, state);    //缁欒繖閲�$$璧嬩竴涓狪D瀛愮被鐨勮〃杈惧紡缁撶偣銆傜敤鏉ヨ緭鍑虹殑/
            delete []$1;

        }
        else
        {
            $$ = new Id(se, state);    //缁欒繖閲�$$璧嬩竴涓狪D瀛愮被鐨勮〃杈惧紡缁撶偣銆傜敤鏉ヨ緭鍑虹殑/
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
        //鎴戜滑鐭ラ亾鏁扮粍鐨勮闂槸鍙互浣滀负宸﹀€肩殑銆�
        int state = 0;

        SymbolEntry *se;
        se = identifiers->lookup($1); //鍦ㄥ凡鏈夌殑绗﹀彿琛ㄩ噷鎵炬湁娌℃湁杩欎釜ID銆�
        if(se == nullptr) //濡傛灉娌℃湁
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);//鎵撳嵃杩欎釜鍙橀噺娌℃湁瀹氫箟
            state = NOT_DEFINED;
            //delete [](char*)$1;
            //assert(se != nullptr);      //鎶涘嚭涓€涓柇瑷€閿欒

            SymbolEntry *error_se;
            error_se = new IdentifierSymbolEntry(new ERROR_OCUPIER(), $1, identifiers->getLevel());
            $$ = new Id(error_se, state);    //缁欒繖閲�$$璧嬩竴涓狪D瀛愮被鐨勮〃杈惧紡缁撶偣銆傜敤鏉ヨ緭鍑虹殑/
            delete []$1;
        }
        else
        {
            $$ = new Id(se, $3, state);    //缁欒繖閲�$$璧嬩竴涓狪D瀛愮被鐨勮〃杈惧紡缁撶偣銆傜敤鏉ヨ緭鍑虹殑/
            if(se->get_arr_dimension_recorder() < arr_dimension_recorder)
                fprintf(stderr, "array \"%s\" has max dimension \"%d\" but accessed by \"%d\" \n", (char*)$1,se->get_arr_dimension_recorder(), arr_dimension_recorder );//鎵撳嵃杩欎釜璁块棶瓒呰繃浜�
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

// 璇嗗埆;
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
        if(se == nullptr) //濡傛灉娌℃湁
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);//鎵撳嵃杩欎釜鍙橀噺娌℃湁瀹氫箟
            delete [](char*)$1;
            assert(se != nullptr);      //鎶涘嚭涓€涓柇瑷€閿欒
        }
        $$ = new FunctCall(se, $3);
    }
    ;
// 鍑芥暟瀹氫箟 Funcdef->
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
// 鍗曠洰杩愮畻
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
        //杩欓噷鎴戜滑浠诲姟鍙鏈変竴涓槸float閭ｄ箞灏辨寜float鏉ョ畻銆�
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
        //姹備綑鏁版槸int鐨勪笓鍒�
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
        //se = identifiers->lookup($1->get_name()); //鍦ㄥ凡鏈夌殑绗﹀彿琛ㄩ噷鎵炬湁娌℃湁杩欎釜Lval銆�
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
        //杩欎釜鏄埌澶翠簡锛屽紑濮嬭繘鍏ュ叿浣撳€间簡銆�

        $1->getSymPtr()->setType(declType);
        $$ = new InitNode($1);
    }
    |
    LBRACE ArrInitLists RBRACE
    {
        //杩欎釜鏄鏄庤繕鏈夎嚦灏戜竴灞�
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

// 甯搁噺鍜屽彉閲忕殑澹版槑
DeclStmt
     :
    VarDeclStmt {$$=$1;}
    |
    ConstDeclStmt {$$=$1;}
    ;
// 鍙橀噺 鍙橀噺+涓€鍫�
VarDeclStmt
    :
    Type IdDeclLists SEMICOLON{$$=$2;}
    
        ;


// 甯搁噺 const +涓€鍫嗭紱
ConstDeclStmt
    :
    CONST Type ConstDeclLists SEMICOLON{$$=$3;}
    ;
// 鍙橀噺
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
        check_redefination_se = identifiers->lookup_in_present_domain($1); //鍦ㄥ凡鏈夌殑绗﹀彿琛ㄩ噷鎵炬湁娌℃湁杩欎釜ID銆�
        if(check_redefination_se != nullptr) //濡傛灉娌℃湁
        {
            fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)$1);//鎵撳嵃杩欎釜鍙橀噺閲嶅畾涔�
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
        check_redefination_se = identifiers->lookup_in_present_domain($1); //鍦ㄥ凡鏈夌殑绗﹀彿琛ㄩ噷鎵炬湁娌℃湁杩欎釜ID銆�
        if(check_redefination_se != nullptr) //濡傛灉娌℃湁
        {
            fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)$1);//鎵撳嵃杩欎釜鍙橀噺閲嶅畾涔�
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
            check_redefination_se = identifiers->lookup_in_present_domain($1); //鍦ㄥ凡鏈夌殑绗﹀彿琛ㄩ噷鎵炬湁娌℃湁杩欎釜ID銆�
            if(check_redefination_se != nullptr) //濡傛灉娌℃湁
            {
                fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)$1);//鎵撳嵃杩欎釜鍙橀噺閲嶅畾涔�
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

// 甯搁噺
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
