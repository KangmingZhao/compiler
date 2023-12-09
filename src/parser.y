%code top{
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
}

%code requires {
    #include "Ast.h"
    #include "SymbolTable.h"
    #include "Type.h"


}

%initial-action {
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


/* 闁哄鏅滈悷鈺呭闯閻戣姤鍎嶉柛鎴瘹TEGER闂侀潧妫旂粭姝€OATPOINT闂佽法鍣﹂幏锟�?闂佺ǹ绻楀〒鍦礊鐎ｎ喖鏋侀柣妤€鐗嗙粊锕傛煥濞戞鎽淣T闂佸憡绮岄悺妗砄AT闂佽法鍣﹂幏锟�?闂佽桨鑳舵晶妤€鐣垫担铏瑰暗閻犲洩灏欓埀顒婃嫹 */

%start Program
%token <strtype> ID 
%token <itype> INTEGER
%token <fltype> FLOATPOINT
%token IF ELSE WHILE
%token INT VOID FLOAT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET SEMICOLON COMMA NOT
%token ADD SUB MUL DIV MOD OR AND LESS GREATER ASSIGN  LESSEQUAL GREATEREQUAL EQUAL NOTEQUAL
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
        se = identifiers->lookup($1); //闂侀潻璐熼崝宀勫礄閿熺姴瀚夊璺烘湰閻ｈ京绱掑Δ瀣暠鐟滄媽娉曢幃浼村Ω閳轰焦顏為梺鍦暩閸嬫挸锕㈡担绯曟煢闁斥晛鍟粻鎺楀级閳哄倻鎳侀梺鍙夌獢D闂佽法鍣﹂幏锟�?
        if(se == nullptr) //婵犵鈧啿鈧綊鎮樻径濞炬煢闁斥晛鍟粻锟�
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);//闂備胶鎳撻悘姘跺箰缁嬭法鏆ら柛鎰ㄦ櫆娴溿倖绻涢崱妤呭弰闁搞倕顑夐弻娑樷枎閹版儼鈧潡鏌涢敐鍛儓闁宠绮欓崺鍕礃閳哄啰鏆㈤梺璇叉唉椤鎯岄鍓т笉闁跨噦鎷�
            state = NOT_DEFINED;
            //delete [](char*)$1;
            //assert(se != nullptr);      //闂佺鍩栫粙鎴﹀吹椤撶喓鈻旈柍褜鍓熼弫鎾绘晸閿燂拷?闂佽法鍣﹂幏锟�?闁荤喎鐨濋崑鎾绘⒑閹稿海鎳曢柟鍑ゆ嫹??


            SymbolEntry *error_se;
            error_se = new IdentifierSymbolEntry(new ERROR_OCUPIER(), $1, identifiers->getLevel());
            $$ = new Id(error_se, state);    //缂傚倷鐒﹂悷銊ф崲閺嶎厽鏅搁柨鐕傛嫹?$$闁荤姍鍐惧剮缂佹柨鐡ㄧ粙澶愭偑閻欙綁鎮楀☉娆忓閻炴凹鍋婇幆鍐礋椤旇　鍋撻崘鈺傜秶闁诡垎鍛攨缂傚倷鐒﹂幐楣冨磻閿濆违闁稿本绮嶉弳蹇涙煛婢跺牆鍔ョ紒棰濆弮瀹曟瑩鎼圭憴鍕殸/
            delete []$1;

        }
        else
        {
            $$ = new Id(se, state);    //缂傚倷鐒﹂悷銊ф崲閺嶎厽鏅搁柨鐕傛嫹?$$闁荤姍鍐惧剮缂佹柨鐡ㄧ粙澶愭偑閻欙綁鎮楀☉娆忓閻炴凹鍋婇幆鍐礋椤旇　鍋撻崘鈺傜秶闁诡垎鍛攨缂傚倷鐒﹂幐楣冨磻閿濆违闁稿本绮嶉弳蹇涙煛婢跺牆鍔ョ紒棰濆弮瀹曟瑩鎼圭憴鍕殸/
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
        //闂佺懓鐡ㄩ崹鍐测堪閹达附鍎楅柕澶嗘櫃婢规洟鏌℃担鐟邦棆缂侇喖绉归幆鍐礋閼搁潧顏�?闂佽法鍠嶇划娆徫涢崼鏇熸櫢闁跨噦鎷�?闂佽法鍣﹂幏锟�?婵炲濮伴崕鎵礊閺冣偓缁嬪宕崟顏嗩唵闂佺ǹ锕ㄩ崑鎰枔閹达附鏅搁柨鐕傛嫹?
        int state = 0;

        SymbolEntry *se;
        se = identifiers->lookup($1); //闂侀潻璐熼崝宀勫礄閿熺姴瀚夊璺烘湰閻ｈ京绱掑Δ瀣暠鐟滄媽娉曢幃浼村Ω閳轰焦顏為梺鍦暩閸嬫挸锕㈡担绯曟煢闁斥晛鍟粻鎺楀级閳哄倻鎳侀梺鍙夌獢D闂佽法鍣﹂幏锟�?
        if(se == nullptr) //婵犵鈧啿鈧綊鎮樻径濞炬煢闁斥晛鍟粻锟�
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);//闂備胶鎳撻悘姘跺箰缁嬭法鏆ら柛鎰ㄦ櫆娴溿倖绻涢崱妤呭弰闁搞倕顑夐弻娑樷枎閹版儼鈧潡鏌涢敐鍛儓闁宠绮欓崺鍕礃閳哄啰鏆㈤梺璇叉唉椤鎯岄鍓т笉闁跨噦鎷�
            state = NOT_DEFINED;
            //delete [](char*)$1;
            //assert(se != nullptr);      //闂佺鍩栫粙鎴﹀吹椤撶喓鈻旈柍褜鍓熼弫鎾绘晸閿燂拷?闂佽法鍣﹂幏锟�?闁荤喎鐨濋崑鎾绘⒑閹稿海鎳曢柟鍑ゆ嫹??

            SymbolEntry *error_se;
            error_se = new IdentifierSymbolEntry(new ERROR_OCUPIER(), $1, identifiers->getLevel());
            $$ = new Id(error_se, state);    //缂傚倷鐒﹂悷銊ф崲閺嶎厽鏅搁柨鐕傛嫹?$$闁荤姍鍐惧剮缂佹柨鐡ㄧ粙澶愭偑閻欙綁鎮楀☉娆忓閻炴凹鍋婇幆鍐礋椤旇　鍋撻崘鈺傜秶闁诡垎鍛攨缂傚倷鐒﹂幐楣冨磻閿濆违闁稿本绮嶉弳蹇涙煛婢跺牆鍔ョ紒棰濆弮瀹曟瑩鎼圭憴鍕殸/
            delete []$1;
        }
        else
        {
            $$ = new Id(se, $3, state);    //缂傚倷鐒﹂悷銊ф崲閺嶎厽鏅搁柨鐕傛嫹?$$闁荤姍鍐惧剮缂佹柨鐡ㄧ粙澶愭偑閻欙綁鎮楀☉娆忓閻炴凹鍋婇幆鍐礋椤旇　鍋撻崘鈺傜秶闁诡垎鍛攨缂傚倷鐒﹂幐楣冨磻閿濆违闁稿本绮嶉弳蹇涙煛婢跺牆鍔ョ紒棰濆弮瀹曟瑩鎼圭憴鍕殸/
            if(se->get_arr_dimension_recorder() < arr_dimension_recorder)
                fprintf(stderr, "array \"%s\" has max dimension \"%d\" but accessed by \"%d\" \n", (char*)$1,se->get_arr_dimension_recorder(), arr_dimension_recorder );//闂佺懓鐏氶幐绋跨暤閸愨晜浜ゆ繛鍡楅叄閸ゅ鎮规担鍛婂仴婵☆偄澧庨幖楣冨川閹殿喚鐣洪梺璺ㄥ櫐閹凤拷?
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
    // |
    // LPAREN Exp RPAREN{
    //     $$=$2;
    // }
    |
     LPAREN Cond RPAREN{
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

// 闂佽崵濮村ú銈呂涘Δ鍛槬闁跨噦鎷�;
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
        if(se == nullptr) //濠电姷顣介埀顒€鍟块埀顒€缍婇幃妯诲緞婵炵偓鐓㈤梺鏂ユ櫅閸燁垳绮婚敓锟�
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);//闂備胶鎳撻悘姘跺箰缁嬭法鏆ら柛鎰ㄦ櫆娴溿倖绻涢崱妤呭弰闁搞倕顑夐弻娑樷枎閹版儼鈧潡鏌涢敐鍛儓闁宠绮欓崺鍕礃閳哄啰鏆㈤梺璇叉唉椤鎯岄鍓т笉闁跨噦鎷�
            delete [](char*)$1;
            assert(se != nullptr);      //闂佺鍩栫粙鎴﹀吹椤撶喓鈻旈柍褜鍓熼弫鎾绘晸閿燂拷?闂佽法鍣﹂幏锟�?闁荤喎鐨濋崑鎾绘⒑閹稿海鎳曢柟鍑ゆ嫹??
        }
        // SymbolEntry* temp=new TemporarySymbolEntry(se->getType(),SymbolTable::getLabel());
        $$ = new FunctCall(se, $3);
    }
    ;
// 闂備礁鎲￠崹鍏兼叏閵堝姹查柣鏃囥€€閸嬫挾鎲撮崟顓犲彎缂備胶濯撮幏锟� Funcdef->
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
// 闂備礁鎲￠〃鍡椕哄⿰鍏犳椽鎮ч崼鐔舵唉闂佸綊鍋婇崰姘跺汲閿燂拷
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
        //闁哄鏅滈悷鈺呭闯閻戣棄绠ｉ柟瀛樼矋缁箑霉閻樹警鍤欏┑顔惧枛閺佹捇鏁撻敓锟�?闁荤喐娲戦悞锕€锕㈡担鍦枖闁逞屽墴閺佹捇鏁撻敓锟�?闂佸搫瀚Λ鐎昽at闂備緡鍙忕徊鑲╃不閻愬吀鐒婇弶鍫涘妼閻︾豹loat闂佸搫顦崕鎶芥偩婵犳碍鏅搁柨鐕傛嫹?
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
        //濠殿喚鎳撻崐椋庣礊閹达箑鏋侀柣妤€鐗婄瑧int闂佹眹鍔岀€氼亞绮幘缁樻櫢闁跨噦鎷�?
        SymbolEntry *se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MOD, $1, $3);
    }
    ;

AddExp
    :
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
        //闁哄鏅滈悷銈夋煂濠婂牊鏅搁柨鐕傛嫹?闂佸憡甯楀姗€濡甸弮鈧粋宥夊幢椤撶姷顦悗娈垮枓閸嬫挸鈹戦纰卞創缂佺粯锕㈠畷妤呭Ψ閵夈儱绶繛杈剧稻閹告悂鍩€椤掑嫭锛熺紒顭掔節閺佹捇鏁撻敓锟�?

        $1->getSymPtr()->setType(declType);
        $$ = new InitNode($1);
    }
    |
    LBRACE ArrInitLists RBRACE
    {
        //闁哄鏅滈悷銈夋煂濠婂牊鏅搁柨鐕傛嫹?闁荤姴娲ら悺銊ノｉ幋鐐翠氦婵☆垳绮粻鎺楁煠瀹勬澘鏆遍柣顭戝灡缁嬪鍩€椤掑嫭鏅搁柨鐕傛嫹?
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

// 闂佹眹鍩勯崹閬嶅箖閸岀偛闂繛宸簻濡炰粙鎮橀悙鑸殿棄缂傚秳绶氬娲箵閹烘梻顔婂┑鐐茬墛閸ㄥ墎绮╅悢鍏煎亱闁割偆鍣ュ锟�
DeclStmt
     :
    VarDeclStmt {$$=$1;}
    |
    ConstDeclStmt {$$=$1;}
    ;
// 闂佸憡鐟﹂敃銏ゅ闯閿燂拷 闂佸憡鐟﹂敃銏ゅ闯閿燂拷+婵炴垶鎸撮崑鎾绘煥閻曞倹瀚�?
VarDeclStmt
    :
    Type IdDeclLists SEMICOLON{$$=$2;}
    
        ;


// 闂佹眹鍩勯崹閬嶅箖閸岀偛闂柨鐕傛嫹 const +濠电偞鍨堕幐鎾磻閹剧粯鐓曟い鎰╁灪閿涙梻绱掗幉瀣
ConstDeclStmt
    :
    CONST Type ConstDeclLists SEMICOLON{$$=$3;}
    ;
// 闂備礁鎲￠悷锕傛晝閵忋倕闂柨鐕傛嫹
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
        check_redefination_se = identifiers->lookup_in_present_domain($1); //闂侀潻璐熼崝宀勫礄閿熺姴瀚夊璺烘湰閻ｈ京绱掑Δ瀣暠鐟滄媽娉曢幃浼村Ω閳轰焦顏為梺鍦暩閸嬫挸锕㈡担绯曟煢闁斥晛鍟粻鎺楀级閳哄倻鎳侀梺鍙夌獢D闂佽法鍣﹂幏锟�?
        if(check_redefination_se != nullptr) //婵犵鈧啿鈧綊鎮樻径濞炬煢闁斥晛鍟粻锟�
        {
            fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)$1);//闂佺懓鐏氶幐绋跨暤閸愨晜浜ゆ繛鍡楅叄閸ゅ鏌涘▎鎰惰€块柛锝嗘そ閺屽苯顓奸崨顖涙瘞闂佽法鍣﹂幏锟�?
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
        check_redefination_se = identifiers->lookup_in_present_domain($1); //闂侀潻璐熼崝宀勫礄閿熺姴瀚夊璺烘湰閻ｈ京绱掑Δ瀣暠鐟滄媽娉曢幃浼村Ω閳轰焦顏為梺鍦暩閸嬫挸锕㈡担绯曟煢闁斥晛鍟粻鎺楀级閳哄倻鎳侀梺鍙夌獢D闂佽法鍣﹂幏锟�?
        if(check_redefination_se != nullptr) //婵犵鈧啿鈧綊鎮樻径濞炬煢闁斥晛鍟粻锟�
        {
            fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)$1);//闂佺懓鐏氶幐绋跨暤閸愨晜浜ゆ繛鍡楅叄閸ゅ鏌涘▎鎰惰€块柛锝嗘そ閺屽苯顓奸崨顖涙瘞闂佽法鍣﹂幏锟�?
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
            check_redefination_se = identifiers->lookup_in_present_domain($1); //闂侀潻璐熼崝宀勫礄閿熺姴瀚夊璺烘湰閻ｈ京绱掑Δ瀣暠鐟滄媽娉曢幃浼村Ω閳轰焦顏為梺鍦暩閸嬫挸锕㈡担绯曟煢闁斥晛鍟粻鎺楀级閳哄倻鎳侀梺鍙夌獢D闂佽法鍣﹂幏锟�?
            if(check_redefination_se != nullptr) //婵犵鈧啿鈧綊鎮樻径濞炬煢闁斥晛鍟粻锟�
            {
                fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)$1);//闂佺懓鐏氶幐绋跨暤閸愨晜浜ゆ繛鍡楅叄閸ゅ鏌涘▎鎰惰€块柛锝嗘そ閺屽苯顓奸崨顖涙瘞闂佽法鍣﹂幏锟�?
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

// 闂佹眹鍩勯崹閬嶅箖閸岀偛闂柨鐕傛嫹
ConstDeclLists 
    :
    ConstDeclLists COMMA ConstDeclList {$$=new ConstDeclList($1,$3);}
    | ConstDeclList {$$=$1;}
    ;

ConstDeclList
    :
    ID ASSIGN InitVal {
        SymbolEntry *se;
        //fprintf(stderr, "%d",identifiers->getLevel());//闂佺懓鐏氶幐绋跨暤閸愨晜浜ゆ繛鍡楅叄閸ゅ鏌涘▎鎰惰€块柛锝嗘そ閺屽苯顓奸崨顖涙瘞闂佽法鍣﹂幏锟�?

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
