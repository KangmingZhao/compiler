%code top{
    #include <iostream>
    #include <assert.h>
    #include "parser.h"
    #include <vector>
    extern Ast ast;

    //ArrDimNode** now_arrdim;
    //int now_arrindex;
    //闂備胶鎳撻悺銊╁垂閸愭祴鍫柟瀵稿仜椤曡鲸鎱ㄥ鍡楀闂婎剛鎲榯mt闂備礁鎲￠懝楣冩煀閿濆拋鐒藉ù鍏兼綑缁€澶愭煟濡じ鍚柣鐔糕偓濠籈AK SEMICOLON闂備焦鐪归崝宀€鈧凹鍨堕幆渚€骞嬮敂鑺ユ闂佽法鍣﹂幏锟�?闂備焦瀵х粙鎴︽儔婵傜ǹ绀夌憸鏂跨暦閵夛附鍎熼柍銉ㄦ珪闁款參姊洪崫鍕妞わ富鍨伴敃銏ゆ偖濡炬姪ckstmt闂備浇娉曢崳锕傚箯閿燂拷?濠电偞娼欓崥瀣偄椤掑嫬绠柨鐕傛嫹?闂備浇娉曢崰鎾诲疾椤愶箑绠栧┑鐘叉搐閽冪喖鏌熼柇锔跨敖妞ゅ繘浜堕弻銊╂偆閸屾稑顏�?濠电偠鎻徊鍓у垝鎼淬劌鏋侀柕鍫濐槸閺勩儵鏌ｉ弬鎸庡暈缁楁垶绻涢幋鐐村碍缂佸娼у嵄闁归棿绀侀幑鍫曟煃閾忓湱澹€ile闂備礁鎲￠崝鏇㈠箠濮椻偓瀹曟洟骞橀钘夊壄闂佸憡娲嶉弬渚€宕戦幘璇茬＜婵﹩鍘鹃ˇ顓熺箾閹寸偞灏柟纰卞亰瀹曪綁鎮欓鍌ゆ祫闂佸搫绋侀崑鍛存偩闁秵鍋犳繛鎴炵墪婵偓闂佽妞挎禍顏堝极閹剧粯鏅搁柨鐕傛嫹?
    //闂佽崵鍠愬ú鎴濐渻閹烘梻涓嶉柣鎰劋閺呮悂鏌ㄩ悤鍌涘?闂備浇澹堝▍鏇㈠几婵犳艾绠柨鐕傛嫹?闂備浇娉曢崰搴ㄥ焵椤掑倸鏋傞柍褜鍓欓崯顖氼焽韫囨稑绠氱憸蹇曟兜閳ь剚绻涢幋鐐村碍缂佸鍏樺畷妤€螣閸忕厧鏅抽梺鍛婎殔閻涚櫝ak闂備焦瀵х粙鎴︽儗婢舵劕绠柨鐕傛嫹?闂備浇娉曢崰鎰板垂濠靛牏涓嶉柣鎰劋閺呮悂鏌ㄩ悤鍌涘?闁诲骸婀辨灙鐎规洦鍓欓～婵嬪Ω閳轰礁寮烽梺姹囧劜閹竼ak闂備浇娉曢崳锕傚箯閿燂拷?
    
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
    //闂佸搫顦弲婊堟偡閳哄懎闂梺鍨儐娴溿倖淇婇婊呭笡缂佺姵甯″鍫曞煛閸屾粍鍤傞梺鐟板殩閹凤拷?闂備浇娉曢崰鏇炍涢崟顖氬瀭闁割煈鍋呴崣蹇涙倵閿濆簼绨绘い鏇嗗洦鐓涢柛灞剧箥濞兼劗鐥鐐靛煟闁轰礁绉舵禒锔芥叏閹邦亜鏋堥梻浣虹帛閻楃偛螞閸曨垰姹查柨鐕傛嫹
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


/* 闂佸搫顦弲婊堟偡閳哄懎闂柣鎴ｅГ閸庡秹鏌涢幋顓熺樄TEGER闂備線娼уΛ鏃傜箔濮濃偓OATPOINT闂備浇娉曢崳锕傚箯閿燂拷?闂備胶枪缁绘銆掗崷顓犵閻庯綆鍠栭弸渚€鏌ｅΔ鈧悧鍡欑矈閿曞倹鐓ユ繛鎴烆焽閹芥罚T闂備礁鎲＄划宀勬偤濡楃爠AT闂備浇娉曢崳锕傚箯閿燂拷?闂備浇妗ㄩ懗鑸垫櫠濡も偓閻ｅ灚鎷呴搹鐟版殫闁荤姴娲╃亸娆撳焵椤掑﹥瀚� */

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
        se = identifiers->lookup($1); //闂備線娼荤拹鐔煎礉瀹€鍕闁跨喓濮寸€氬顭跨捄鐑樻拱闁伙綀浜槐鎺懳旂€ｎ剛鏆犻悷婊勫濞夋洟骞冩导鏉懳╅柍杞扮劍椤忕偤姊洪崷顓熸毄闁稿鎸搁敃銏℃媴缁洘鐓㈤梺鏂ユ櫅閸燁垳绮婚幒妤€绾ч柍鍝勫€婚幊渚€姊洪崣澶岀崲D闂備浇娉曢崳锕傚箯閿燂拷?
        if(se == nullptr) //濠电姷顣介埀顒€鍟块埀顒€缍婇幃妯诲緞婵炵偓鐓㈤梺鏂ユ櫅閸燁垳绮婚敓锟�
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);//闂傚倷鑳堕幊鎾绘倶濮樿泛绠扮紒瀣硶閺嗐倝鏌涢幇銊︽珕濞存嚎鍊栫换娑㈠幢濡ゅ懎寮伴梺鎼炲€曢澶愬蓟濞戞ǚ鏋庨柟鐗堝劶閳ь剙娼￠弻娑㈡晲閸涱喗鍎撻梺瀹狀唺缁瑩宕洪崟顖氱闁冲搫鍟伴弳銏ゆ⒑鐠囧弶鍞夋い顐㈩樀閹矂顢欓崜褌绗夐梺璺ㄥ櫐閹凤拷
            state = NOT_DEFINED;
            //delete [](char*)$1;
            //assert(se != nullptr);      //闂備胶顢婇崺鏍矙閹达箑鍚规い鎾跺枔閳绘棃鏌嶈閸撶喖寮幘缁樻櫢闁跨噦鎷�?闂備浇娉曢崳锕傚箯閿燂拷?闂佽崵鍠庨惃婵嬪磻閹剧粯鈷戦柟绋挎捣閹虫洟鏌熼崙銈嗗??


            SymbolEntry *error_se;
            error_se = new IdentifierSymbolEntry(new ERROR_OCUPIER(), $1, identifiers->getLevel());
            $$ = new Id(error_se, state);    //缂傚倸鍊烽悞锕傛偡閵娧勫床闁哄稁鍘介弲鎼佹煥閻曞倹瀚�?$$闂佽崵濮嶉崘鎯у壆缂備焦鏌ㄩ悺銊х矙婢舵劖鍋戦柣娆欑秮閹鈽夊▎蹇擃潕闁荤偞鍑归崑濠囧箚閸愵喖绀嬫い鏃囥€€閸嬫捇宕橀埡鍌滅Ф闂佽鍨庨崨顖ｆ敤缂傚倸鍊烽悞锕傚箰妤ｅ啫纾婚柨婵嗩槸杩濋梺绋挎湰缁秹寮宠箛娑欑厸濠㈣泛鐗嗛崝銉х磼妫版繂寮€规洘鐟╅幖鍦喆閸曨厽娈�/
            delete []$1;

        }
        else
        {
            $$ = new Id(se, state);    //缂傚倸鍊烽悞锕傛偡閵娧勫床闁哄稁鍘介弲鎼佹煥閻曞倹瀚�?$$闂佽崵濮嶉崘鎯у壆缂備焦鏌ㄩ悺銊х矙婢舵劖鍋戦柣娆欑秮閹鈽夊▎蹇擃潕闁荤偞鍑归崑濠囧箚閸愵喖绀嬫い鏃囥€€閸嬫捇宕橀埡鍌滅Ф闂佽鍨庨崨顖ｆ敤缂傚倸鍊烽悞锕傚箰妤ｅ啫纾婚柨婵嗩槸杩濋梺绋挎湰缁秹寮宠箛娑欑厸濠㈣泛鐗嗛崝銉х磼妫版繂寮€规洘鐟╅幖鍦喆閸曨厽娈�/
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
        //闂備胶鎳撻悺銊╁垂閸愭祴鍫柟杈鹃檮閸庢鏌曟径鍡樻珒濠㈣娲熼弻鈩冩媴閻熼偊妫嗙紓渚囧枛缁夊綊骞嗛崘顔肩闁兼悂娼ч锟�?闂備浇娉曢崰宥囧垝濞嗗精娑㈠醇閺囩喐娅㈤梺璺ㄥ櫐閹凤拷?闂備浇娉曢崳锕傚箯閿燂拷?濠电偛顕慨浼村磿閹殿喚绀婇柡鍐ｅ亾缂佸顦靛畷顐﹀礋椤忓棭鍞甸梻浣呵归敃銊╁磻閹邦兘鏋旈柟杈鹃檮閺呮悂鏌ㄩ悤鍌涘?
        int state = 0;

        SymbolEntry *se;
        se = identifiers->lookup($1); //闂備線娼荤拹鐔煎礉瀹€鍕闁跨喓濮寸€氬顭跨捄鐑樻拱闁伙綀浜槐鎺懳旂€ｎ剛鏆犻悷婊勫濞夋洟骞冩导鏉懳╅柍杞扮劍椤忕偤姊洪崷顓熸毄闁稿鎸搁敃銏℃媴缁洘鐓㈤梺鏂ユ櫅閸燁垳绮婚幒妤€绾ч柍鍝勫€婚幊渚€姊洪崣澶岀崲D闂備浇娉曢崳锕傚箯閿燂拷?
        if(se == nullptr) //濠电姷顣介埀顒€鍟块埀顒€缍婇幃妯诲緞婵炵偓鐓㈤梺鏂ユ櫅閸燁垳绮婚敓锟�
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);//闂傚倷鑳堕幊鎾绘倶濮樿泛绠扮紒瀣硶閺嗐倝鏌涢幇銊︽珕濞存嚎鍊栫换娑㈠幢濡ゅ懎寮伴梺鎼炲€曢澶愬蓟濞戞ǚ鏋庨柟鐗堝劶閳ь剙娼￠弻娑㈡晲閸涱喗鍎撻梺瀹狀唺缁瑩宕洪崟顖氱闁冲搫鍟伴弳銏ゆ⒑鐠囧弶鍞夋い顐㈩樀閹矂顢欓崜褌绗夐梺璺ㄥ櫐閹凤拷
            state = NOT_DEFINED;
            //delete [](char*)$1;
            //assert(se != nullptr);      //闂備胶顢婇崺鏍矙閹达箑鍚规い鎾跺枔閳绘棃鏌嶈閸撶喖寮幘缁樻櫢闁跨噦鎷�?闂備浇娉曢崳锕傚箯閿燂拷?闂佽崵鍠庨惃婵嬪磻閹剧粯鈷戦柟绋挎捣閹虫洟鏌熼崙銈嗗??

            SymbolEntry *error_se;
            error_se = new IdentifierSymbolEntry(new ERROR_OCUPIER(), $1, identifiers->getLevel());
            $$ = new Id(error_se, state);    //缂傚倸鍊烽悞锕傛偡閵娧勫床闁哄稁鍘介弲鎼佹煥閻曞倹瀚�?$$闂佽崵濮嶉崘鎯у壆缂備焦鏌ㄩ悺銊х矙婢舵劖鍋戦柣娆欑秮閹鈽夊▎蹇擃潕闁荤偞鍑归崑濠囧箚閸愵喖绀嬫い鏃囥€€閸嬫捇宕橀埡鍌滅Ф闂佽鍨庨崨顖ｆ敤缂傚倸鍊烽悞锕傚箰妤ｅ啫纾婚柨婵嗩槸杩濋梺绋挎湰缁秹寮宠箛娑欑厸濠㈣泛鐗嗛崝銉х磼妫版繂寮€规洘鐟╅幖鍦喆閸曨厽娈�/
            delete []$1;
        }
        else
        {
            $$ = new Id(se, $3, state);    //缂傚倸鍊烽悞锕傛偡閵娧勫床闁哄稁鍘介弲鎼佹煥閻曞倹瀚�?$$闂佽崵濮嶉崘鎯у壆缂備焦鏌ㄩ悺銊х矙婢舵劖鍋戦柣娆欑秮閹鈽夊▎蹇擃潕闁荤偞鍑归崑濠囧箚閸愵喖绀嬫い鏃囥€€閸嬫捇宕橀埡鍌滅Ф闂佽鍨庨崨顖ｆ敤缂傚倸鍊烽悞锕傚箰妤ｅ啫纾婚柨婵嗩槸杩濋梺绋挎湰缁秹寮宠箛娑欑厸濠㈣泛鐗嗛崝銉х磼妫版繂寮€规洘鐟╅幖鍦喆閸曨厽娈�/
            if(se->get_arr_dimension_recorder() < arr_dimension_recorder)
                fprintf(stderr, "array \"%s\" has max dimension \"%d\" but accessed by \"%d\" \n", (char*)$1,se->get_arr_dimension_recorder(), arr_dimension_recorder );//闂備胶鎳撻悘姘跺箰缁嬭法鏆ら柛鎰ㄦ櫆娴溿倖绻涢崱妤呭弰闁搞倕顑夐幃瑙勬媴閸涘﹤浠村┑鈽嗗亜婢у酣骞栨ィ鍐ㄥ窛闁规鍠氶悾娲⒑鐠恒劌娅愰柟鍑ゆ嫹?
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

// 闂備浇宕垫慨鏉懨洪妶鍛傛稑螖閸涱厽妲梺璺ㄥ櫐閹凤拷;
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
        if(se == nullptr) //婵犵數濮烽。浠嬪焵椤掆偓閸熷潡鍩€椤掆偓缂嶅﹪骞冨Ο璇茬窞濠电偟鍋撻悡銏ゆ⒑閺傘儲娅呴柛鐕佸灣缁鏁撻敓锟�
        {
            fprintf(stderr, "identifier \"%s\" is undefined\n", (char*)$1);//闂傚倷鑳堕幊鎾绘倶濮樿泛绠扮紒瀣硶閺嗐倝鏌涢幇銊︽珕濞存嚎鍊栫换娑㈠幢濡ゅ懎寮伴梺鎼炲€曢澶愬蓟濞戞ǚ鏋庨柟鐗堝劶閳ь剙娼￠弻娑㈡晲閸涱喗鍎撻梺瀹狀唺缁瑩宕洪崟顖氱闁冲搫鍟伴弳銏ゆ⒑鐠囧弶鍞夋い顐㈩樀閹矂顢欓崜褌绗夐梺璺ㄥ櫐閹凤拷
            delete [](char*)$1;
            assert(se != nullptr);      //闂備胶顢婇崺鏍矙閹达箑鍚规い鎾跺枔閳绘棃鏌嶈閸撶喖寮幘缁樻櫢闁跨噦鎷�?闂備浇娉曢崳锕傚箯閿燂拷?闂佽崵鍠庨惃婵嬪磻閹剧粯鈷戦柟绋挎捣閹虫洟鏌熼崙銈嗗??
        }
        // SymbolEntry* temp=new TemporarySymbolEntry(se->getType(),SymbolTable::getLabel());
        $$ = new FunctCall(se, $3);
    }
    ;
// 闂傚倷绀侀幉锟犲垂閸忓吋鍙忛柕鍫濐槸濮规煡鏌ｉ弮鍥モ偓鈧柛瀣尵閹叉挳宕熼鐘插綆缂傚倷鑳舵刊鎾箯閿燂拷 Funcdef->
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
// 闂傚倷绀侀幉锟犮€冮崱妞曞搫饪伴崗鐘虫そ閹囧醇閻旇埖鍞夐梻浣哥秺閸嬪﹪宕板璺烘辈闁跨噦鎷�
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
        //闂佸搫顦弲婊堟偡閳哄懎闂柣鎴ｆ缁狅綁鏌熺€涙鐭嬬紒顕嗙畱闇夐柣妯硅閸ゆ瑥鈹戦鎯ф灈闁轰焦鎹囬弫鎾绘晸閿燂拷?闂佽崵鍠愬ú鎴︽倿閿曗偓閿曘垺鎷呴崷顓涙灃闂侀€炲苯澧撮柡浣规崌閺佹捇鏁撻敓锟�?闂備礁鎼€氼喖螞閻庢樈at闂傚倷绶￠崣蹇曞緤閼测晝涓嶉柣鎰悁閻掑﹪寮堕崼娑樺闁伙妇璞筶oat闂備礁鎼ˇ顓㈠磿閹惰姤鍋╁┑鐘崇閺呮悂鏌ㄩ悤鍌涘?
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
        //婵犳鍠氶幊鎾诲磹妞嬪海绀婇柟杈剧畱閺嬩線鏌ｅΔ鈧悧濠勭懅int闂備焦鐪归崝宀€鈧凹浜炵划顓㈠箻缂佹ɑ娅㈤梺璺ㄥ櫐閹凤拷?
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
        //闂佸搫顦弲婊堟偡閵堝鐓傛繝濠傜墛閺呮悂鏌ㄩ悤鍌涘?闂備礁鎲＄敮妤€顭垮鈧俊鐢稿籍閳ь剛绮嬪澶婂耿妞ゆ挾濮烽ˇ顕€鎮楀▓鍨灀闁稿鎸搁埞鎴︻敊绾板崬鍓电紓浣虹帛閿曘垹鐣峰Δ鍛ㄩ柕澶堝劚缁额喗绻涙潏鍓хɑ闁瑰憡鎮傞崺鈧い鎺戝閿涚喓绱掗…鎺旂瘈闁轰焦鎹囬弫鎾绘晸閿燂拷?

        $1->getSymPtr()->setType(declType);
        $$ = new InitNode($1);
    }
    |
    LBRACE ArrInitLists RBRACE
    {
        //闂佸搫顦弲婊堟偡閵堝鐓傛繝濠傜墛閺呮悂鏌ㄩ悤鍌涘?闂佽崵濮村ú銈夋偤閵娿儙锝夊箣閻愮繝姘﹀┑鈽嗗灣缁垳绮婚幒妤佺厾鐎瑰嫭婢橀弳閬嶆煟椤垵鐏＄紒瀣樀閸┾偓妞ゆ帒瀚弲鎼佹煥閻曞倹瀚�?
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

// 闂備焦鐪归崺鍕垂闁秴绠栭柛宀€鍋涢梻顖涚箾瀹割喕绨绘俊鐐扮矙閹﹢鎮欓懜娈挎缂傚倸绉崇欢姘潖濞差亜绠甸柟鐑樻⒒椤斿﹤鈹戦悙鑼闁搞劌澧庣划鈺呮偄閸忕厧浜遍梺鍓插亞閸ｃ儱顭囬敓锟�
DeclStmt
     :
    VarDeclStmt {$$=$1;}
    |
    ConstDeclStmt {$$=$1;}
    ;
// 闂備礁鎲￠悷锕傛晝閵忋倕闂柨鐕傛嫹 闂備礁鎲￠悷锕傛晝閵忋倕闂柨鐕傛嫹+濠电偞鍨堕幐鎾磻閹剧粯鐓ラ柣鏇炲€圭€氾拷?
VarDeclStmt
    :
    Type IdDeclLists SEMICOLON{$$=$2;}
    
        ;


// 闂備焦鐪归崺鍕垂闁秴绠栭柛宀€鍋涢梻顖炴煥閻曞倹瀚� const +婵犵數鍋為崹鍫曞箰閹绢喖纾婚柟鍓х帛閻撴洘銇勯幇鈺佺仾闁挎稒姊荤槐鎺楀箟鐎ｎ偄顏�
ConstDeclStmt
    :
    CONST Type ConstDeclLists SEMICOLON{$$=$3;}
    ;
// 闂傚倷绀侀幉锟犳偡閿曞倹鏅濋柕蹇嬪€曢梻顖炴煥閻曞倹瀚�
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
        check_redefination_se = identifiers->lookup_in_present_domain($1); //闂備線娼荤拹鐔煎礉瀹€鍕闁跨喓濮寸€氬顭跨捄鐑樻拱闁伙綀浜槐鎺懳旂€ｎ剛鏆犻悷婊勫濞夋洟骞冩导鏉懳╅柍杞扮劍椤忕偤姊洪崷顓熸毄闁稿鎸搁敃銏℃媴缁洘鐓㈤梺鏂ユ櫅閸燁垳绮婚幒妤€绾ч柍鍝勫€婚幊渚€姊洪崣澶岀崲D闂備浇娉曢崳锕傚箯閿燂拷?
        if(check_redefination_se != nullptr) //濠电姷顣介埀顒€鍟块埀顒€缍婇幃妯诲緞婵炵偓鐓㈤梺鏂ユ櫅閸燁垳绮婚敓锟�
        {
            fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)$1);//闂備胶鎳撻悘姘跺箰缁嬭法鏆ら柛鎰ㄦ櫆娴溿倖绻涢崱妤呭弰闁搞倕顑夐弻娑樷枎閹版儼鈧潡鏌涢敐鍡樸仢闁哄苯鑻濂稿川椤栨稒鐦為梻浣芥硶閸ｏ箓骞忛敓锟�?
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
        check_redefination_se = identifiers->lookup_in_present_domain($1); //闂備線娼荤拹鐔煎礉瀹€鍕闁跨喓濮寸€氬顭跨捄鐑樻拱闁伙綀浜槐鎺懳旂€ｎ剛鏆犻悷婊勫濞夋洟骞冩导鏉懳╅柍杞扮劍椤忕偤姊洪崷顓熸毄闁稿鎸搁敃銏℃媴缁洘鐓㈤梺鏂ユ櫅閸燁垳绮婚幒妤€绾ч柍鍝勫€婚幊渚€姊洪崣澶岀崲D闂備浇娉曢崳锕傚箯閿燂拷?
        if(check_redefination_se != nullptr) //濠电姷顣介埀顒€鍟块埀顒€缍婇幃妯诲緞婵炵偓鐓㈤梺鏂ユ櫅閸燁垳绮婚敓锟�
        {
            fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)$1);//闂備胶鎳撻悘姘跺箰缁嬭法鏆ら柛鎰ㄦ櫆娴溿倖绻涢崱妤呭弰闁搞倕顑夐弻娑樷枎閹版儼鈧潡鏌涢敐鍡樸仢闁哄苯鑻濂稿川椤栨稒鐦為梻浣芥硶閸ｏ箓骞忛敓锟�?
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
            check_redefination_se = identifiers->lookup_in_present_domain($1); //闂備線娼荤拹鐔煎礉瀹€鍕闁跨喓濮寸€氬顭跨捄鐑樻拱闁伙綀浜槐鎺懳旂€ｎ剛鏆犻悷婊勫濞夋洟骞冩导鏉懳╅柍杞扮劍椤忕偤姊洪崷顓熸毄闁稿鎸搁敃銏℃媴缁洘鐓㈤梺鏂ユ櫅閸燁垳绮婚幒妤€绾ч柍鍝勫€婚幊渚€姊洪崣澶岀崲D闂備浇娉曢崳锕傚箯閿燂拷?
            if(check_redefination_se != nullptr) //濠电姷顣介埀顒€鍟块埀顒€缍婇幃妯诲緞婵炵偓鐓㈤梺鏂ユ櫅閸燁垳绮婚敓锟�
            {
                fprintf(stderr, "identifier \"%s\" is redefined\n", (char*)$1);//闂備胶鎳撻悘姘跺箰缁嬭法鏆ら柛鎰ㄦ櫆娴溿倖绻涢崱妤呭弰闁搞倕顑夐弻娑樷枎閹版儼鈧潡鏌涢敐鍡樸仢闁哄苯鑻濂稿川椤栨稒鐦為梻浣芥硶閸ｏ箓骞忛敓锟�?
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

// 闂備焦鐪归崺鍕垂闁秴绠栭柛宀€鍋涢梻顖炴煥閻曞倹瀚�
ConstDeclLists 
    :
    ConstDeclLists COMMA ConstDeclList {$$=new ConstDeclList($1,$3);}
    | ConstDeclList {$$=$1;}
    ;

ConstDeclList
    :
    ID ASSIGN InitVal {
        SymbolEntry *se;
        //fprintf(stderr, "%d",identifiers->getLevel());//闂備胶鎳撻悘姘跺箰缁嬭法鏆ら柛鎰ㄦ櫆娴溿倖绻涢崱妤呭弰闁搞倕顑夐弻娑樷枎閹版儼鈧潡鏌涢敐鍡樸仢闁哄苯鑻濂稿川椤栨稒鐦為梻浣芥硶閸ｏ箓骞忛敓锟�?

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
