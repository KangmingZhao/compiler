锘�#include "Ast.h"
#include "SymbolTable.h"
#include "Unit.h"
#include "Instruction.h"
#include "IRBuilder.h"
#include <string>
#include <assert.h>
#include<algorithm>
#include "Type.h"

extern FILE *yyout;
int Node::counter = 0;
IRBuilder* Node::builder = nullptr;
bool isreturn=false;
Type *retVal;
std::vector<Type*> paramVector;
std::vector<Operand *> para_operands;// 闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊椤掑鏅悷婊冪箻閸┾偓妞ゆ帊鑳堕埢鎾绘煛閸涱喚绠橀柛鎺撳笒閳诲酣骞樺畷鍥跺敽婵犲痉鏉库偓鎾绘倿閿曗偓閳诲秹寮崼鐔叉嫽闂佺ǹ鏈悷褔宕濆澶嬬厵妞ゆ棁鍋愰崺锝団偓瑙勬礃閸旀洟鍩為幋锕€鐐婇柍鍝勶攻闁裤倝姊洪懡銈呅俊妞煎妿閹峰啴鏁冮崒姘優闂佸搫娲ㄩ崰鍡樼濠婂牊鐓欓柡澶婄仢椤ｆ娊鏌ｉ敐鍫滃惈缂佽鲸甯￠幃顏堝焵椤掑嫬鍨傞柟鎯版閻掑灚銇勯幒鍡椾壕闂佸憡蓱缁挸鐣烽幋锕€绠荤紓浣诡焽閸樻捇鎮峰⿰鍕煉鐎规洘绮岄埢搴ㄥ箻瀹曞洤骞掗梻浣筋潐瀹曟﹢顢氳閺屽宕堕浣哄幐闂佹悶鍎弲娑㈠几閺冨牊鐓曟俊顖氭贡閻瑦鎱ㄦ繝鍐┿仢闁哄苯鎳橀幃娆撴嚑鐠轰警浼冨┑鐘媰鐏炶姤鐏堥梺瀹狀潐閸ㄥ潡宕洪崟顖氱妞ゎ厽鍨靛▓婵堢磽閸屾瑨鍏岀紒顕呭灦閹嫰顢涘杈ㄦ婵犻潧鍊搁幉锟犲磻閸曨偒娓婚悗锝庝簽閸戝湱绱掓潏銊х疄闁哄矉绲鹃幆鏃堝Χ鎼淬垻绉锋繝鐢靛仦缁佹挳寮插┑鍫燁潟闁绘劕鐡ㄦ刊鎾煕濠靛嫬鍔滄繛鍫ョ畺濮婃椽宕烽鐐插闂佹悶鍔庨崰搴″祫闂佸壊鍋嗛崰鎾剁不妤ｅ啯鐓曢柍鈺佸幘椤忓懏鍙忛柨鏇炲€归悡鏇㈡煟閺冣偓瑜板啯绂嶆ィ鍐┾拻闁稿本鑹鹃埀顒佹倐瀹曟劙鎮滈懞銉モ偓鍧楁煥閺囩偛鈧綊宕曢幋鐘冲枑闁绘鐗嗙粭鎺旂磼閳ь剚寰勭仦绋夸壕闁稿繐顦禍楣冩⒑閸涘﹥澶勯柛瀣閹顢橀悩鐢碉紳婵炶揪绲挎灙闁煎摜鎳撻…鑳檪闁瑰嚖鎷�

std::vector<std::vector<Operand*>>para_operands_stack;

LoopManager loop_manager;
BasicBlock* temp_end_bb;
BasicBlock* temp_then_bb;
ExprNode* temp_cond_expr;

ExprNode* bugs_detector;


bool now_is_def_funct = 0;

FunctionDef* temp_funct;


Constant* cal_expr(ExprNode* node2cal)
{
    double cal_result = node2cal->cal_expr_val();
    int cal_result_i = (int)cal_result;
    //std::cout << cal_result_i << std::endl;
    if (node2cal->cal_expr_val() != PRE_CAL_ERROR_MEETING_VAL)
    {
        SymbolEntry* se;
        if (node2cal->getSymPtr()->getType()->isFLOAT())
        {
            se = new ConstantSymbolEntry(TypeSystem::floatType, (int)cal_result);
        }
        else
        {
            se = new ConstantSymbolEntry(TypeSystem::intType, cal_result_i);
        }
        //std::cout << se->toStr() << std::endl;
        return new Constant(se);
    }
    else
    {
        return nullptr;
    }
}

void stmt_genCode(StmtNode* stmtNode_2_gen, BasicBlock* bb_2_entry, Function* func)
{
    BasicBlock* origin_entry = func->setEntry(bb_2_entry);
    stmtNode_2_gen->genCode();
    func->setEntry(origin_entry);
}

bool expr_in_cond = 0;

Node::Node()
{
    seq = counter++;
}


//void Node::backPatch(std::vector<BasicBlock**> &list, BasicBlock*target)
//{
//    for(auto &bb:list)
//        *bb = target;
//}
//
//std::vector<BasicBlock**> Node::merge(std::vector<BasicBlock**> &list1, std::vector<BasicBlock**> &list2)
//{
//    std::vector<BasicBlock**> res(list1);
//    res.insert(res.end(), list2.begin(), list2.end());
//    return res;
//}

void Node::ibackPatch(std::vector<CondBrInstruction*>& list, BasicBlock* target, bool is_true)
{
    for (auto& bb : list)
    {
        if (is_true)
        {
            bb->setTrueBranch(target);
        }
        else
        {
            bb->setFalseBranch(target);
        }
    }
}


std::vector<CondBrInstruction*> Node::merge(std::vector<CondBrInstruction*>& list1, std::vector<CondBrInstruction*>& list2)
{
    std::vector<CondBrInstruction*> res(list1);
    res.insert(res.end(), list2.begin(), list2.end());
    return res;
}




void Ast::genCode(Unit *unit)
{
    IRBuilder *builder = new IRBuilder(unit);
    Node::setIRBuilder(builder);
    root->genCode();
    // 闂傚倸鍊搁崐鎼佸磹閹间礁纾瑰瀣捣閻棗霉閿濆浜ら柤鏉挎健瀵爼宕煎顓熺彅闂佹悶鍔嶇换鍐Φ閸曨垰鍐€妞ゆ劦婢€缁墎绱撴担鎻掍壕婵犮垼鍩栭崝鏍偂濞戙垺鐓曢柕澶堝灪濞呭懘鏌＄€ｎ偄鐏︾紒缁樼洴瀹曘劑顢涘锝嗙€伴梻浣告惈閻寰婇崐鐔轰航闂備焦鍎崇换鎺戔枍閵忋垺顫曢柣鎰劋閳锋垹绱撴担璇＄劷缂佺姵鎹囬弻锝夊冀瑜嬮崑銏⑩偓娈垮枛椤兘寮幇鏉垮窛闁稿本绋掗ˉ鍫ユ煕閳规儳浜炬俊鐐€栫敮鎺斺偓姘煎弮瀹曟垹鈧綆浜栭弨浠嬫煟濡搫绾х紒渚囧亞缁辨挸顓奸崱娆忊拰闂佸搫琚崝鎴﹀箖閵堝纾兼繛鎴烇供娴硷拷
    fprintf(yyout, "declare i32 @getint()\n");
    fprintf(yyout, "declare void @putint(i32)\n");
    fprintf(yyout, "declare i32 @getch()\n");
    fprintf(yyout, "declare void @putch(i32)\n");
    fprintf(yyout, "declare void @putf(i32)\n\n");
}


void ControlFlowGraph::build_BB_link()
{
    std::vector< BasicBlock* >bb_queue;
    BasicBlock* now_bb = entry;
    bb_queue.push_back(now_bb);
    while (bb_queue.size())
    {
        now_bb = bb_queue[0];
        bb_queue.erase(bb_queue.begin());
        for (auto& bb : map_blocks[now_bb])
        {
            LinkBB(now_bb, bb);
            bb_queue.push_back(bb);
        }
    }
}

void FunctionDef::genCode()
{
    Unit *unit = builder->getUnit();
    Function *func = new Function(unit, se);
    BasicBlock *entry = func->getEntry();
    // set the insert point to the entry basicblock of this function.
    builder->setInsertBB(entry);
    builder->set_cfg(entry);

    now_is_def_funct = 1;
    if (paraStmt != nullptr)
    {
        if (paraStmt != nullptr)
            paraStmt->genCode();
    }
    now_is_def_funct = 0;

    stmt->genCode();

    /**
     * Construct control flow graph. You need do set successors and predecessors for each basic block.
     * Todo
    */
    for (auto bb = func->begin(); bb != func->end(); bb++){
    Instruction *last = (*bb)->rbegin();
    Instruction *first = (*bb)->begin();
    while (first != last) {
      if (first->isCond() || first->isUncond()) {
        (*bb)->remove(first);
      }
      first = first->getNext();
    }

    if (last->isCond()){
      BasicBlock *trueBranch = ((CondBrInstruction *)last)->getTrueBranch();
      BasicBlock *falseBranch = ((CondBrInstruction *)last)->getFalseBranch();
      (*bb)->addSucc(trueBranch);
      (*bb)->addSucc(falseBranch);
      trueBranch->addPred(*bb);
      falseBranch->addPred(*bb);
    }
    else if (last->isUncond()){
      BasicBlock *branch = ((UncondBrInstruction *)last)->getBranch();
      (*bb)->addSucc(branch);
      branch->addPred(*bb);
    }
    else if (!last->isRet()){
      if (((FunctionType *)func->getSymPtr()->getType())->getRetType() == TypeSystem::intType)
      {
        new RetInstruction(new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0)), (*bb));
      }
      else if (((FunctionType *)func->getSymPtr()->getType())->getRetType() == TypeSystem::voidType){
        new RetInstruction(nullptr, (*bb));
      }
    }
  }
   
   // builder->build_link();
}

void BinaryExpr::genCode()
{

    BasicBlock *bb = builder->getInsertBB();
    Function *func = bb->getParent();
    if (op < arithmeticEnd)
    {
        //if(expr_in_cond)
        //arithmetic op
        expr1->genCode();
        //std::cout << (expr2->getSymPtr()->getType() == nullptr) << std::endl;
        expr2->genCode();
        Operand* src1 = expr1->getOperand();
        Operand* src2 = expr2->getOperand();
        Operand *n1 = src1, *n2 = src2;
        if (src1->getType() == TypeSystem::boolType)
        {
            SymbolEntry *s = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            n1 = new Operand(s);
            new ZextInstruction(n1, src1, bb);
        }
        if (src2->getType() == TypeSystem::boolType)
        {
            SymbolEntry *s2 = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            n2 = new Operand(s2);
            new ZextInstruction(n2, src2, bb);
        }
        int opcode;
        switch (op)
        {
        case ADD:
            opcode = BinaryInstruction::ADD;
            break;
        case SUB:
            opcode = BinaryInstruction::SUB;
            break;
        case MUL:
            opcode = BinaryInstruction::MUL;
            break;
        case DIV:
            opcode = BinaryInstruction::DIV;
            break;
        case MOD:
            opcode = BinaryInstruction::MOD;
            break;
        default:
            opcode = -1;
            break;
        }

        new BinaryInstruction(opcode, dst, src1, src2, bb);
    }
    else if (op > arithmeticEnd && op < logicEnd)
    {
        //logical op
        BasicBlock* trueBB = new BasicBlock(func);

        expr1->need_2_be_bool = 1;
        expr2->need_2_be_bool = 1;
        //Operand* temp_op;
        switch (op)
        {
        case AND:

            expr1->genCode();
            //LinkBB(builder->getInsertBB(), trueBB);
            builder->add_link(builder->getInsertBB(), trueBB);
            ibackPatch(expr1->itrueList(), trueBB, 1);
            builder->setInsertBB(trueBB);               // set the insert point to the trueBB so that intructions generated by expr2 will be inserted into it.
            expr2->genCode();
            
            i_true_list = expr2->itrueList();
            i_false_list = merge(expr1->ifalseList(), expr2->ifalseList());

            break;

        case OR:
            expr1->genCode();
            //LinkBB(builder->getInsertBB(), trueBB);
            builder->add_link(builder->getInsertBB(), trueBB);
            ibackPatch(expr1->ifalseList(), trueBB, 0);
            builder->setInsertBB(trueBB);               // set the insert point to the trueBB so that intructions generated by expr2 will be inserted into it.
            
            expr2->genCode();
            i_false_list = expr2->ifalseList();
            i_true_list = merge(expr1->itrueList(), expr2->itrueList());
            break;

            break;
        }
    }
    else if (op > logicEnd && op < relationEnd)
    {
        //relation op
        expr1->genCode();
        expr2->genCode();
        Operand* src1 = expr1->getOperand();
        Operand* src2 = expr2->getOperand();
        int opcode;
        switch (op)
        {
        case EQUAL:
            opcode = CmpInstruction::E;
            break;
        case NOTEQUAL:
            opcode = CmpInstruction::NE;
            break;
        case LESS:
            opcode = CmpInstruction::L;
            break;
        case GREATER:
            opcode = CmpInstruction::G;
            break;
        case LESSEQUAL:
            opcode = CmpInstruction::LE;
            break;
        case GREATEREQUAL:
            opcode = CmpInstruction::GE;
            break;
        default:
            opcode = -1;
            break;
        }
        dst->set_bool();
        Operand *n1 = src1, *n2 = src2;
        // bool -> int 闂傚倸鍊搁幊蹇涘礉閺囩姵顫曢柨娑樺鐏忕敻鎮归崶顏勭毢闁逞屽墴閺€鍗烆焽椤忓牜鏁嗗ù锝嚽圭花锟�
        if (src1->getType() == TypeSystem::boolType)
        {
            SymbolEntry *s = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            n1 = new Operand(s);
            new ZextInstruction(n1, src1, bb);
        }
        if (src2->getType() == TypeSystem::boolType)
        {
            SymbolEntry *s2 = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            n2 = new Operand(s2);
            new ZextInstruction(n2, src2, bb);
        }
        new CmpInstruction(opcode, dst, n1, n2, bb);

        BasicBlock* temp_bb = nullptr;// = new BasicBlock(func);
        CondBrInstruction * temp_cb = new CondBrInstruction(temp_bb, temp_bb, dst, builder->getInsertBB());
        //std::cout << "fuck\n";

        i_true_list.emplace_back(temp_cb);
        i_false_list.emplace_back(temp_cb);
        //std::cout << i_true_list.size();
        //std::cout << i_false_list.size();
    }
    else
    {
        //else
    }


    //if (op == AND)
    //{
    //    //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崰姘熼埀顒勬倵濞堝灝鏋熼柟顔煎€搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛鐏炵硶鍋撳畷鍥ㄦ畷闂侀€炲苯澧寸€规洑鍗冲浠嬵敇閻愮數鏆柣鐔哥矋閺屻劑鎮鹃柨瀣檮闁告稑锕ゆ禍婊堟⒑閸涘﹦绠撻悗姘煎墮閳绘捇濡烽埡鍌楁嫽闂佺ǹ鏈悷褔藝閿曞倹鐓欓悹鍥囧懐鐦堥悗瑙勬礀濠€閬嶅箟閹绢喖绀嬫い鎺嗗亾鐎殿喖娼″娲捶椤撯剝顎楅梺鍝ュУ閻楃娀骞冮垾鎰佹建闁逞屽墴瀵鎮㈤崨濠勭Ф婵°倧绲介崯顖烆敁瀹ュ鈷戠紒瀣儥閸庢劙鏌涢弮鈧悷鈺呯嵁閸愩剮鏃堝川椤撶喎绁舵俊鐐€栭幐楣冨磹椤愶箑顫呴幒铏濠婂牊鐓忓鑸电☉椤╊剛绱掗悩鎰佺劷缂佽鲸甯楀蹇涘Ω閿曗偓闂夊秹姊洪悷鏉挎Щ闁硅櫕锚閻ｇ兘顢曢敃鈧粈瀣棯閻楀煫顏呯閸撗€鍋撻崗澶婁壕闂佸憡娲﹂崜娑㈠储闁秵鈷戦梻鍫熻儐瑜版帒纾块柟鎯版閺勩儵鏌ㄥ┑鍡樼闁稿鎸搁埢鎾诲垂椤旂晫浜俊鐐€ら崢楣冨礂濡警鍤曟い鎰剁畱缁犳稒銇勯幘璺轰户缂佹劗鍋炵换婵嬫偨闂堟刀銏ゆ倵濮樺崬鍘寸€规洏鍎靛畷銊р偓娑櫱氶幏缁樼箾鏉堝墽鎮奸柟铏崌椤㈡艾饪伴崨顖滐紲闁荤姴娲﹁ぐ鍐焵椤掆偓濞硷繝鎮伴钘夌窞濠电偟鍋撻～宥夋⒑闂堟稓绠冲┑顔惧厴椤㈡瑩骞掗弮鍌滐紳闂佺ǹ鏈悷褔宕濆鍡愪簻妞ゆ挾鍋為崰妯尖偓瑙勬磸閸ㄤ粙鐛弽銊﹀闁稿繐顦扮€氬ジ姊绘担鍛婂暈缂佸鍨块弫鍐晲閸ヮ煈鍋ㄩ梻渚囧墮缁夌敻鎮￠弴銏＄厽婵☆垵顕ф晶顖涚箾閸喓鐭岄柍褜鍓濋～澶娒哄鈧畷婵嗏枎閹捐泛绁﹂梺鎼炲労閸撴岸宕戠€ｎ喗鐓曟い鎰Т閻忊晜顨ラ悙鏉戝闁哄矉绲鹃幆鏃堝灳閸愯尪绶熼梻浣呵归鍡涘箲閸ヮ剙绠栨俊銈呮噺閺呮煡骞栫划鍏夊亾閹颁焦楠勯梻鍌欐缁鳖喚绮婚幋锔藉亱闁规崘顕х粻鏍ㄤ繆閵堝懎鏆為柛鐘叉閺屾盯寮撮妸銉ヮ潾濡炪倧璐熼崝鎴﹀蓟閿濆顫呴柍杞拌兌娴狀參鏌ｉ姀鈺佺仯闁哥姵鐗犻獮鍐潨閳ь剟鐛幒鎳虫梹鎷呴崫鍕闂傚倷鑳剁划顖炲礉閺囥埄鏁嬫い鎾卞灩閻撴繈鏌熼崜褏甯涢柣鎾冲暟閹茬ǹ饪伴崼婵堫槶濠电偛妫欓幐鎼佸磼閵娾晜鐓涚€广儱鍟俊鍧楁煟濠靛棛鍩ｉ柡宀嬬節瀹曞爼濡烽妷褌鐥梻浣瑰▕閺€閬嶅垂閸ф钃熸繛鎴炃氶弨浠嬫煕閳╁喚娈㈠ù灏栧亾闂傚倷绀侀悿鍥敋瑜旇棟闁汇垻枪閽冪喐绻涢幋鐐茬劰闁稿鎹囬弫宥夊础閻愬弶娈ㄦ繝銏ｎ潐濞茬喎顫忔繝姘＜婵炲棙甯掗崢锟犳煛鐎ｅ吀绨奸柕鍥у楠炲鈹戦崶鑸碉紗婵°倗濮烽崑鐐哄礉濞嗘挾宓侀柡宥冨妿閻熺懓鈹戦悩鎻掝伀闁崇鍊栫换婵嬫偨闂堟稈鏋呭┑鐐板尃閸ヨ埖鏅為梺鍦濠㈡﹢寮告笟鈧弻娑㈠焺閸愵亖濮囬梺缁樻尰濞茬喖骞冨鈧幃娆撳箵閹哄棙瀵栭梻浣芥〃閻掞箓鎮ч悩璇茶摕闁哄洨鍠撶粻楣冩煟閹伴潧澧柣婵囨礋濮婃椽骞庨懞銉︽殸闂佹悶鍔屽鈥愁嚕鐠囨祴妲堟俊顖炴敱閺傗偓闂備胶纭堕崜婵嬨€冭箛鏂款嚤闁稿本鍑瑰〒濠氭煏閸繈顎楀ù婊冨⒔缁辨帡鎮╅搹顐㈢３閻庢鍠涢褔鍩ユ径鎰潊闁绘鏁稿澶愭⒒娓氣偓閳ь剛鍋涢懟顖涙櫠鐎电硶鍋撶憴鍕缂傚秴锕ら悾椋庣矙鐠囩偓妫冨畷姗€濡歌閸嬪﹪姊婚崒娆戭槮闁硅绻濆畷婵嬫晝閸屾氨鐛ラ梺鍝勭▉閸欏繒绱為弽顓熺厱婵炴垶顭囬幗鐘绘煟閹惧磭绠婚柡灞剧洴椤㈡洟鏁愰崶鈺冩毇闂備線娼荤徊钘夛耿鏉堚晜顫曢柟鐑橆殔缁犳稒銇勯幘璺盒涘┑顔兼川缁辨挻鎷呴搹鐟扮缂備浇顕ч悧鎾荤嵁閸愨晝顩烽悗锝庡亽濡啫鈹戦悙娈挎缂傚牅鍗冲畷瑙勭鐎ｎ亞鐣哄┑掳鍊愰崑鎾淬亜椤愶絿绠炴慨濠呭吹閳ь剟娼ч幉锟犲疾閸忚偐绡€缁剧増菤閸嬫捇宕橀懠顒勭崜闂備礁鎲″褰掓偡閵夆晩鏁嬮柨婵嗩槸缁犵粯銇勯弮鍌楁嫛婵炵厧锕娲箮閼恒儲娈伴梺绋款儐閹瑰洭寮婚悢鍓叉Ч閹艰揪绲界粭锛勭磽娴ｈ櫣甯涚紒瀣笒椤洩绠涘☉妯碱槶閻熸粌绉瑰畷鍝勭暆閸曨兘鎷虹紒缁㈠幖閹冲氦顣跨紓鍌欑贰閸犳牕霉閻戣棄绀嗛柟鐑橆殔缁秹鏌涢銈呮瀻闁逞屽墰缁垱绌辨繝鍥舵晬闁绘劕鐡ㄩ悵宕囩磽娴ｉ潧濡兼い顓炲槻椤繐煤椤忓拋妫冨┑鐐村灱妞存悂顢撻崱娑欌拻闁稿本鑹鹃鈺呮倵濮樼厧澧撮柟顔藉劤閻ｏ繝骞嶉鑺ョ暦闂備線鈧偛鑻晶鎾煕閳规儳浜炬俊鐐€栫敮鎺楀窗濮橆剦鐒介柟閭﹀幘缁犻箖鏌涘▎蹇ｆ闁兼澘娼￠弻鏇㈠幢濡も偓閺嗭絿鈧娲忛崝宥咁焽韫囨稑绀堢憸蹇涘煟閵堝棛绡€闁汇垽娼у瓭闂佹寧娲忛崐婵嬪箖瑜斿畷鍗炩槈濞嗗繆鍋撻崸妤佺厱妞ゎ厽鍨垫禍鏍瑰⿰鍕煉闁哄瞼鍠栧畷顐﹀礋椤掑顥ｅ┑鐐茬摠缁秹宕曢幎瑙ｂ偓鏃堝礃椤斿槈褔鏌涢埄鍐剧劷闁宠绋撶槐鎾诲磼濮樻瘷锛勭磼閼搁潧鍝哄┑鈥崇摠缁楃喖鍩€椤掆偓椤曪綁骞橀纰辨綂闂佹枼鏅涢崯顖炴偟閺嶎厽鈷掑〒姘ｅ亾闁逞屽墰閸嬫盯鎳熼娑欐珷闁规鍠氱壕鍏笺亜閺傚灝鈷旈悽顖涚〒缁辨帞绱掑Ο鑲╃暤濡炪値鍋呯换鍫ャ€佸鈧幃娆撳级閸噮鍤欓梻鍌氬€搁崐鐑芥倿閿曞倸绠栭柛顐ｆ礀绾炬寧绻濇繝鍌滃缂佲偓閸愨斂浜滈煫鍥ㄦ尵婢с垻鈧娲樼划宀勫煘閹达附鏅柛鏇ㄥ亗閺夘參姊虹粙鍖℃敾闁搞劌鐏濋悾鐑藉即閵忊€虫濡炪倖甯婄粈浣规償婵犲洦鈷戦柛鎾村絻娴滄繄绱掔拠鑼㈡い顓炴喘瀵粙濡歌椤旀洟鎮楅悷鏉款棌闁哥姵娲滈懞閬嶅礂缁楄桨绨婚梺闈涱槶閸庤櫕鏅跺☉姘辩＜缂備焦顭囧ú瀛橆殽閻愬樊鍎旈柟顔界懅閹瑰嫭绗熼娑辨（婵犵绱曢崑鎴﹀磹瑜忓濠冪鐎ｎ亞顔愬銈嗗姧缁犳垿鎮″鈧弻鐔告綇閸撗呮殸闂佺粯鎼换婵嬪蓟濞戙垹鐒洪柛鎰剁細濞岊亞绱撴担闈涘闁告鍥ｂ偓鏃堝礃椤斿槈褔鏌涢幇鈺佸妞ゎ剙鐗撳娲箰鎼淬垹顦╂繛瀛樼矤娴滄繃绌辨繝鍕ㄥ亾濞戞瑯鐒介柣鐔风秺閺屽秷顧侀柛鎾寸懇椤㈡岸鏁愭径妯绘櫔闂侀€炲苯澧柣锝囧厴楠炴帡骞嬮弮鈧～宥呪攽閳藉棗鐏ｉ柕鍡楊儑濡叉劙鏁撻悩鏂ユ嫽婵炶揪缍€濡嫰宕ヨぐ鎺撶厱閻庯綆鍓欐禒褏绱掗鐣屾噰鐎规洖銈告俊鐑藉Ψ瑜滃Σ鑸电節閻㈤潧鈻堟繛浣冲浂鏁勯柟瀵稿Х缁€濠囨煛瀹ュ骸浜愰柛瀣崌閻涱噣宕归鐓庮潛婵＄偑鍊х紓姘跺础閹惰棄鏄ユ繛鎴欏灩缁狅綁鏌ㄩ弮鍌涙珪闁告﹢浜跺娲箰鎼淬埄姊垮銈嗘肠閸愭儳娈ㄩ梺闈涒康缂傛氨鎹㈤崱娑欑厽闁逛即娼ф晶顖炴煕婵犲啫濮嶉柡宀嬬磿娴狅箓宕滆閸掓稒绻濈喊妯哄⒉闁烩剝娲熻棟闁告瑥顦禍婊堟煥濠靛棛澧曞ù婊冩贡缁辨帗娼忛妸銉﹁癁閻庤娲樼敮鎺楋綖濠靛柊鎺戔枎濞嗘垹袦闂佸搫鏈ú鐔风暦婵傚壊鏁嗛柛灞剧敖濠婂牊鍊甸悷娆忓缁€鍐煥閺囨ê鐏茬€规洘妞介幃娆撴倻濡　鍋撻柨瀣ㄤ簻闁瑰搫妫楁禍鎯ь渻閵堝懐绠抽柛搴涘€楅幑銏犫攽鐎ｎ偄浠洪梻鍌氱墛缁嬫劙宕Δ鍛拺閻熸瑥瀚妵鐔访瑰⿰鍛户婵″弶鍔欓幃娆撳传閸曨厼濮︽俊鐐€栧濠氬磻閹剧粯鐓冮悷娆忓閻忓瓨銇勯姀锛勬噰鐎规洦鍋婂畷鐔煎Ω閵夈倕顥氭繝寰锋澘鈧劙宕戦幘缁樼厓闁芥ê顦藉Ο鈧梺璇″枤閺咁偆鍒掑▎鎾抽敜婵°倐鍋撻柡浣哥秺濮婄粯鎷呴崨濠冨創濠电偛鐪伴崝鎴濈暦娴兼潙绠婚悹鍥ㄥ絻閸嬪秹姊绘笟鍥у缂佸鏁婚崺娑㈠箣閻樼數锛濇繛杈剧到閸樻粌螞閵婏负浜滈煫鍥ㄦ尵婢ф洟鏌ｉ妶搴℃珝闁哄瞼鍠撶划娆撳垂椤曞懎濡烽梻浣告啞閻熴儳鎹㈤幒妤€鐒垫い鎺嗗亾缂佺姴绉瑰畷鏇㈠础閻忕粯妞介幃鈺冩嫚閼碱剨绱遍梻浣告贡閸嬫捇寮告總绋垮嚑闁哄啫鐗婇悡鏇熴亜椤撶喎鐏ラ柡瀣枛閺岋綀绠涢幘璺衡叺濠殿喖锕ㄥ▍锝呪槈閻㈢ǹ妞藉ù锝呮啞閻ゅ倻绱撻崒娆愮グ濡炲枪鐓ら柣鏃囧亹瀹撲線鏌熸潏鍓х暠妞ゎ偄鎳橀弻锝咁潨閸℃ぞ绨奸梺鍝勫€甸崑鎾绘⒒閸屾瑨鍏岀痪顓℃硾椤﹨顦圭€殿喗褰冮オ浼村醇濠靛牞绱抽梻鍌氬€搁悧濠冪瑹濡も偓閵嗘帗绻濆顓犲帾闂佸壊鍋呯换鍐夊⿰鍛＜婵°倓绀佸ù顕€鏌＄仦鍓с€掗柍褜鍓ㄧ紞鍡樼濠靛瑤澶愬閳垛晛浜鹃悷娆忓婢跺嫰鏌涢幘璺烘灈妤犵偛鍟粋鎺斺偓锝庝簻缁愭稒绻濋悽闈涗户闁稿鎸鹃埀顒佷亢濡嫰鍩為幋锔藉€烽柤鎼佹涧濞懷呯磽閸屾氨袦闁稿鎸搁埞鎴︽倷閸欏鐝旂紓浣瑰絻濞尖€愁嚕椤愶富鏁婇悘蹇旂墬椤秹姊洪棃娑㈢崪缂佽鲸娲熷畷銏ゆ焼瀹ュ棌鎷洪梻鍌氱墛閼冲棜銇愰幒鎴狅紵闂備緡鍓欑粔瀵哥不閺嶎厽鐓欓弶鍫ョ畺濡绢噣鏌ｉ幘瀵告噧闁靛棙甯掗～婵嬵敆閸屾瑨妾稿┑鐘愁問閸犳牠宕愰幖浣哥厴闁硅揪闄勯鎰版⒑缁洘娅嗛柣妤佹礋椤㈡岸濡烽埡浣侯槹濡炪倖甯掗崐濠氭儊閸儲鈷戦柛娑橈攻婢跺嫰鏌涢幘瀵告创鐎规洩缍佸畷鐔碱敍濞戞帗瀚介梻浣呵归張顒勬偡瑜旇棟闁挎柨澧界壕鐣屸偓骞垮劚閹锋垵顔忛妷鈺傛嚉闁绘劗鍎ら悡鏇犳喐鎼淬劊鈧啴宕卞☉妯硷紮闂佺鍕垫畷闁绘挶鍎甸弻锟犲炊閳轰椒绮堕梺閫炲苯澧柟鑺ョ矋缁旂喖寮撮悢铏圭槇濠殿喗锕╅崢楣冨储閹剧粯鈷戦悗鍦У閵嗗啰绱掗埀顒佺瑹閳ь剙顕ｉ幎鑺ュ亜闁惧繐婀遍敍婊堟⒑缂佹〒鐟扮暦閻㈠憡鍋╂繛宸簼閻撴盯鎮橀悙鎻掆挃婵炲弶鎸抽弻娑㈠煛娴ｅ壊浼冮悗瑙勬礃閿曘垹鐣峰⿰鍡╂Щ婵犳鍠楀钘夘潖缂佹ɑ濯撮柣鎴灻▓宀勬⒑閸濄儱鏋庢繛纭风節閻涱喗绻濋崶褏鐤€濡炪倖姊婚悺鏂款焽閻斿吋鈷戦柛锔诲幖閸斿鏌熼崫銉у笡闁告帒锕獮姗€顢欓悾灞藉箞婵犵數濞€濞佳兾涘Δ鍜佹晜闁冲搫鍋嗗▓浠嬫煟閹邦剚鈻曢柛搴㈡⒐閹便劍绻濋崘鈹夸虎閻庤娲﹂崑濠傜暦閻旂⒈鏁婄痪鏉垮綁缁辨梻绱撻崒姘偓椋庣矆娓氣偓钘濋梺顒€绉寸粈鍌涗繆椤栨繂浜规俊鏌ユ涧閳规垿鎮欑€涙ê闉嶉梺绯曟櫅閸熸潙鐣烽幋锕€绠荤紓浣诡焽閸樻悂鏌ｈ箛鏇炰粶濠⒀嗘鐓ら悗娑欙供濞堜粙鏌ｉ幇顖涘涧闁兼媽娉曢埀顒冾潐濞测晝鎹㈠┑瀣畺婵炲棗娴氶崯鍛亜閺冨倹娅曠紒澶婄仛缁绘繂顕ラ柨瀣凡闁逞屽墯椤ㄥ棝骞堥妸鈺傚€婚柦妯侯槺閿涙稑鈹戦悙鏉戠仸缁炬澘绉撮悾鐑藉矗婢跺寮垮┑锛勫仩椤曆勭妤ｅ啯鍊垫繛鍫濈仢濞呮﹢鏌涚€ｎ亷韬柣娑卞枛铻ｉ柤娴嬫櫇閿涙粌鈹戦埥鍡楃仯缂侇噮鍨堕、鏃堟偄閸忓皷鎷绘繛杈剧秬婵倗娑甸崼鏇熺厱闁挎繂绻掗悾鍨殽閻愯尙绠婚柡浣规崌閺佹捇鏁撻敓锟�
    //    
    //}
    //else if(op == OR)
    //{
    //    // Todo
    //}
    //else if(op >= LESS && op <= GREATER)
    //{
    //    // Todo
    //}
    //else if(op < arithmeticEnd)
    //{
    //}
}

void Constant::genCode()
{
    // we don't need to generate code.
    //std::cout << dst->getType()->toStr() << " " << dst->getType()->isBool() << std::endl;
    //std::cout << dst->toStr() << std::endl;
    if (dst->getType()->isBool())
    {
        BasicBlock* temp_bb = nullptr;// = new BasicBlock(func);
        CondBrInstruction* temp_cb = new CondBrInstruction(temp_bb, temp_bb, dst, builder->getInsertBB());
        i_true_list.emplace_back(temp_cb);
        i_false_list.emplace_back(temp_cb);
    }
    if (need_2_be_bool)
    {
        SymbolEntry* se_const0 = new ConstantSymbolEntry(TypeSystem::intType, 0);
        ExprNode* temp_const0 = new Constant(se_const0);

        SymbolEntry* addr_se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        Operand* temp_result = new Operand(addr_se);

        new CmpInstruction(CmpInstruction::NE, temp_result, dst, temp_const0->getOperand(), builder->getInsertBB());


        BasicBlock* temp_bb = nullptr;// = new BasicBlock(func);
        CondBrInstruction* temp_cb = new CondBrInstruction(temp_bb, temp_bb, temp_result, builder->getInsertBB());
        //std::cout << "fuck\n";

        i_true_list.emplace_back(temp_cb);
        i_false_list.emplace_back(temp_cb);
    }
}

void Id::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    Operand *addr = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getAddr();
    //std::cout << addr->toStr() << std::endl;
    new LoadInstruction(dst, addr, bb);
    if (need_2_be_bool)
    {
        SymbolEntry* se_const0 = new ConstantSymbolEntry(TypeSystem::intType, 0);
        ExprNode* temp_const0 = new Constant(se_const0);

        SymbolEntry* addr_se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        Operand* temp_result = new Operand(addr_se);

        new CmpInstruction(CmpInstruction::NE, temp_result, dst, temp_const0->getOperand(), builder->getInsertBB());


        BasicBlock* temp_bb = nullptr;// = new BasicBlock(func);
        CondBrInstruction* temp_cb = new CondBrInstruction(temp_bb, temp_bb, temp_result, builder->getInsertBB());
        //std::cout << "fuck\n";

        i_true_list.emplace_back(temp_cb);
        i_false_list.emplace_back(temp_cb);
    }
}

void IfStmt::genCode()
{
    Function* func;
    BasicBlock* then_bb, * end_bb;

    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);
    temp_end_bb = end_bb;
    temp_then_bb = then_bb;


    //std::cout << cond->getSymPtr()->toStr() << std::endl;
    cond->need_2_be_bool = 1;
    //cond->getSymPtr()->setType(TypeSystem::boolType);
    cond->genCode();
    ibackPatch(cond->itrueList(), then_bb, 1);
    ibackPatch(cond->ifalseList(), end_bb, 0);


    //LinkBB(builder->getInsertBB(), end_bb);
    //LinkBB(builder->getInsertBB(), then_bb);

    builder->add_link(builder->getInsertBB(), end_bb);
    builder->add_link(builder->getInsertBB(), then_bb);

    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    builder->setInsertBB(end_bb);

}

void IfElseStmt::genCode()
{
    // Todo
    Function* func;
    BasicBlock* then_bb, * else_bb, * end_bb;

    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    else_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);

    //LinkBB(builder->getInsertBB(), then_bb);
    //LinkBB(builder->getInsertBB(), else_bb);

    //LinkBB(then_bb, end_bb);
    //LinkBB(else_bb, end_bb);

    builder->add_link(builder->getInsertBB(), then_bb);
    builder->add_link(builder->getInsertBB(), else_bb);

    builder->add_link(then_bb, end_bb);
    builder->add_link(else_bb, end_bb);


    cond->need_2_be_bool = 1;
    //cond->getSymPtr()->changeType(TypeSystem::boolType);
    cond->genCode();
    ibackPatch(cond->itrueList(), then_bb, 1);    
    ibackPatch(cond->ifalseList(), else_bb, 0);


    builder->setInsertBB(then_bb);
    stmt_genCode(thenStmt, then_bb, func);
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);


    builder->setInsertBB(else_bb);
    stmt_genCode(elseStmt, then_bb, func);
    //elseStmt->genCode();
    else_bb = builder->getInsertBB();
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閹冣挃闁硅櫕鎹囬垾鏃堝礃椤忎礁浜鹃柨婵嗙凹缁ㄥジ鏌熼惂鍝ョМ闁哄矉缍侀、姗€鎮欓幖顓燁棧闂備線娼уΛ娆戞暜閹烘缍栨繝闈涱儐閺呮煡鏌涘☉鍗炲妞ゃ儲鑹鹃埞鎴炲箠闁稿﹥顨嗛幈銊╂倻閽樺锛涘┑鐐村灍閹崇偤宕堕浣镐缓缂備礁顑嗙€笛囨倵椤掑嫭鍊垫鐐茬仢閸旀碍銇勯敂璇茬仯缂侇喖鐗忛埀顒婄秵閸嬩焦绂嶅⿰鍫熺厵闁告繂瀚倴闂佸憡鏌ㄧ粔鐢稿Φ閸曨垰妫橀柟绋块閺嬬姴鈹戦纭峰姛缂侇噮鍨堕獮蹇涘川椤栨粎鐓撻梺鍦圭€涒晠鎯侀敐澶嬧拻濞达絽鎲＄拹锟犳煃瀹勬壆澧︾€规洘鍨剁换婵嬪炊閳轰胶銈﹀┑鐘垫暩婵挳宕鐐茬？闁绘柨鍚嬮悡銉︾節闂堟稒顥炲┑顖氬悑娣囧﹪顢曢敐鍥╃厑缂備胶绮换鍫ュ箖娴犲顥堟繛纾嬵啇缁插潡鎯€椤忓牆绾ч悹鎭掑壉瑜旈弻鐔碱敋閸℃瑧鐦堥悗娈垮枟閹歌櫕鎱ㄩ埀顒勬煥濞戞ê顏╂鐐村姍濮婄粯鎷呯憴鍕哗闂佺ǹ瀵掗崹璺虹暦閺囩喐缍囬柕濞垮€楃粻姘舵⒑缁嬭法绠抽柍宄扮墕閳绘挻绂掔€ｎ偆鍘介梺褰掑亰閸樹粙宕曞Δ浣虹闁割偆鍣﹂懓鍧楁煛鐏炵晫啸妞ぱ傜窔閺屾盯骞樼€靛摜鐤勯梺鎸庣箘閸嬬偛顕ラ崟顒傜闁圭儤鍨堕幆鍫熴亜椤愶絿鐭掗柛鈹惧亾濡炪倖甯掔€氼剛绮婚弽顓熺叆闁绘柨鎼牎闂佸搫鎳忛幃鍌炲蓟閿濆憘鐔煎垂椤旀寧鏆遍梻浣侯攰婵倕煤閻斿娼栭柧蹇氼潐瀹曞鏌曟繛鍨姕闁诲繋鐒︾换婵嗏枔閸喗鐏嶉梺鐟版啞婵炲﹪鐛崘銊㈡瀻闁归偊鍠氶惁鍫ユ⒑閹肩偛鍔橀柛鏂块叄瀹曘垽骞樼紒妯煎幐婵犮垼娉涢懟顖涚閿曗偓椤潡鎮烽悧鍫＆閻庤娲樼换鍌烆敇閸忕厧绶為悗锝庡墮楠炴姊绘繝搴′簻婵炲眰鍊曢湁闁绘垵澹欓崶顒佹櫆濠殿喗鍔掔花璇差渻閵堝懐绠伴悗姘煎墴瀵娊鏁愰崨顏呮杸闂佺偨鍎辩壕顓㈠春閿濆洠鍋撶憴鍕閻㈩垱甯￠崺銉﹀緞婵炵偓鐎婚梺鐟扮摠缁诲嫮绮欐繝姘拻闁稿本鑹鹃埀顒傚厴閹虫宕滄担绋跨亰濡炪倖鐗滈崑娑氱不鐟欏嫨浜滈柟鎵虫櫅閳ь剚鐗曢悾鐑藉矗婢跺瞼鐦堥梻鍌氱墛缁嬫帡鏁嶅澶嬬厱闁靛牆妫欑粈瀣煙椤旇偐绉虹€规洖鐖兼俊姝岊槹闁稿鍋涢埞鎴﹀焺閸愩劎绁烽梺缁橆殕閹瑰洤顕ｆ繝姘嵆闁挎稑瀚弶鎼佹⒑閸濆嫭宸濋柛濠庡亰婵＄兘鍩￠崒婊冨箞闂備礁鎼崯鐘诲磻閹剧粯鐓欑紒瀣仢閺嗚鲸銇勯銏㈢闁圭厧缍婂畷鐑筋敇閻橀潧骞嗛梻浣藉吹婵灚绂嶆禒瀣閹兼惌鐓堥弫濠傗攽閻樻彃鏆熺紒鈾€鍋撴繝鐢靛仜閻楀棝鎮樺┑瀣€堕梺顒€绉甸悡鏇熶繆椤栨艾鎮戞繛鍛礈缁辨帡顢欑喊鍗炲壈濡炪倖鎸搁妶绋跨暦閵娾晩鏁傞柛娑卞枟濠㈡垿姊绘担绛嬪殭闁告垹鏅槐鐐哄幢濡⒈娲搁梺缁樺姉閸庛倝寮查鍕厽闁哄倸鐏濋幃鎴︽煃闁垮鐏╃紒杈ㄦ尰閹峰懐鎷犻敍鍕Ш闂備焦妞块崢褰掑磹濠靛钃熼柣鏂垮悑閸嬧晠鎮橀悙鏉戝姢闁哄懐銇栭梻鍌氬€搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閹冣挃闁硅櫕鎹囬垾鏃堝礃椤忎礁浜鹃柨婵嗙凹缁ㄥジ鏌熼惂鍝ョМ闁哄矉缍侀、姗€鎮欓幖顓燁棧闂備線娼уΛ娆戞暜閹烘缍栨繝闈涱儐閺呮煡鏌涘☉鍗炲妞ゃ儲鑹鹃埞鎴炲箠闁稿﹥顨嗛幈銊╂倻閽樺锛涘┑鐐村灍閹崇偤宕堕浣镐缓缂備礁顑嗙€笛囨倵椤掑嫭鍊垫鐐茬仢閸旀碍銇勯敂璇茬仯缂侇喖鐗忛埀顒婄秵閸嬩焦绂嶅⿰鍫熺厵闁逛絻娅曞▍鍛存煟韫囷絽鏋涢棁澶嬬節婵炴儳浜鹃梺鍛婎殕婵炲﹤顕ｆ繝姘櫜闁糕剝锚閸斿懘姊洪悙钘夊姎闁哥喐瀵у蹇涘川鐎涙ǚ鎷虹紓浣割儐椤戞瑩宕曢幇鐗堢厵闁荤喓澧楅崰妯尖偓娈垮枦椤曆囧煡婢舵劕顫呴柣妯诲絻缁侇噣姊绘担铏瑰笡闁告梹娲熷畷銉╁焵椤掑倻纾兼い鏃傚亾閺嗩剚鎱ㄦ繝鍐┿仢鐎规洦鍋婂畷鐔碱敆閳ь剙鈻嶅┑瀣拺闁告繂瀚崳钘夆攽椤旇姤灏﹂柨婵堝仩缁犳稑鈽夊▎鎰姃闂佽崵鍠愰悷銉р偓姘煎墴瀹曘垼銇愰幒鎾嫼闂佸憡绋戦敃锝囨闁秵鐓曟俊顖涗航閸嬨垺顨ラ悙鍙夘棦闁诡喗鐟╅幃浠嬫偨绾板闂繝鐢靛仩閹活亞寰婃禒瀣婵犲﹤鐗婇崐鍧楁偣閹帒濡跨痪鎯с偢閺岋絽鈻庣仦鎴掑闂備焦鎮堕崝蹇撯枖濞戭澁缍栭煫鍥ㄦ⒒缁♀偓濠殿喗锕╅崜娑㈡晬濠婂啠鏀芥い鏃€鏋绘笟娑㈡煕韫囨棑鑰跨€规洘鍨块獮妯兼嫚閼碱剦鍟囧┑鐐舵彧缁蹭粙骞楀⿰鍫熸櫖婵炲棙鎸婚埛鎴犵磽娴ｅ顏堟倶閸楃偐鏀芥い鏂挎惈閳ь剚绻傞悾鐑藉箛閺夊潡鏁滃┑掳鍊撻懗鍫曞矗閸℃稒鈷戦柛婵嗗瀹告繈鏌涚€ｎ偆娲撮柛鈹惧亾濡炪倖宸婚崑鎾剁磽瀹ュ拑宸ラ柣锝囧厴瀵挳濮€閻樻鍚呴梻浣虹帛閿氶柛鐔锋健瀵悂骞嬮敂瑙ｆ嫼缂傚倷鐒﹂敃鈺佲枔閺冨牊鐓熸俊銈傚亾婵☆偅绋撻崚鎺楊敇閵忕姷顔婇梺鍝勫暙閸婂鏁嶅⿰鍫熲拺闂侇偆鍋涢懟顖涙櫠椤栨壕鍋撻崹顐ｇ凡閻庢凹鍠楃粋鎺楁晜閸撗呯厯婵犮垹澧庢灙妞ゃ儲宀稿缁樻媴閸涢潧婀遍幑銏ゅ箛閸忣偄娲、娑樷槈濮樺吋閿ゅ┑掳鍊х徊浠嬪疮椤栫偛鐓曢柟杈鹃檮閳锋垶銇勯幇鈺佲偓鏇熺濠婂牊鐓犳繛鑼额嚙閻忥繝鏌￠崨顓犲煟妤犵偞锕㈤、娆撴偩鐏炶棄绠為梻鍌欑閻ゅ洭锝炴径鎰？闁惧浚鍋嗛々鏌ユ煟閹伴潧鍘哥紒璇叉閺岋綁骞囬崗鍝ョ泿闂侀€炲苯澧柣妤佺矒瀵偊顢欓悾宀€鐦堥梺鍛婃处閸橀箖鎮為崗鑲╃闁圭偓娼欓悞褰掓煕鐎ｎ偅灏甸柍褜鍓濋～澶娒哄⿰鍫濇瀬濠电姵鍝庨埀顑跨铻栭柛娑卞幘椤︽澘顪冮妶鍡楃瑨闁稿﹥鎮傞悰顕€宕奸妷锔规嫽婵炴挻鍩冮崑鎾寸箾娴ｅ啿瀚禒姘舵⒒娴ｅ懙鍦偓娑掓櫆缁绘稒绻濋崒婊勬濠电偛妯婃禍婵嬪疾閹绘帩鐔嗛悹杞拌閸庢垿鏌涘鍡曢偗婵﹨娅ｉ崠鏍即閻愭祴鎷ゆ繝鐢靛仜瀵墎鍒掗幘瑙勵棨闂備焦瀵ч弻銊╋綖閺囩喓顩叉繝濠傚娴滄粓鏌熼弶鍨暢闁诡喛鍋愮槐鎺楁偐缁涚鈧潡鏌″畝鈧崰鏍箖閻戣棄绾ф繛鍡楀綖閻ヮ亞绱撻崒娆戝妽妞ゃ劌鐗撳畷浼村冀椤撴壕鍋撴担绯曟婵☆垶鏀遍～宥呪攽椤旂即鎴︽儊閻戣姤鍋勯柛蹇撶毞閹峰姊虹粙鎸庢拱闁煎綊绠栭崺鈧い鎺戝濡垶绻涢崱鎰仼妞ゎ偅绻堥、妤佸緞鐏炶棄楔闂佽崵鍠愮划宥呂涢崘顔惧祦闁告劦鍙庡Ο鍕倵鐟欏嫭绀€鐎规洦鍓熼崺銉﹀緞婵炪垻鍠栭幊鏍煛閸屾壕鍋撻鈧濠氬磼濞嗘劗銈板銈庡亜椤︽壆绮╅悢鐓庡耿婵☆垳绮悵宄扳攽鎺抽崐鏇㈠箠鎼达絽顥氬┑鐘崇閻撴洜鈧厜鍋撻柍褜鍓熷畷鎴︽倷閻戞ê浜楀┑鐐叉閹稿摜鐥閺屾盯顢曢悩鎻掑Б闂佷紮绲块崕銈嗙┍婵犲嫮纾奸柕蹇曞У閻濇繈姊洪崫鍕缂佸缍婂濠氬Ω瑜夐崑鎾绘晲鎼存繄鏁栨繛瀵稿帶閼活垶鍩為幋锔绘晩缁绢厼鍢叉导鎰渻閵堝骸骞栭柛銏＄叀閹箖鏌ㄧ€ｎ剟妾梺鍛婄☉閿曘倖绂嶅⿰鍫熲拺闁告稑锕ョ亸顐︽煠閸愭彃顣崇紒顔碱煼楠炴帡骞橀崜浣烘缂備礁澧芥晶妤呮晪婵犫拃鍡楃毢闁瑰弶鎮傚鍫曞垂椤旇姤顔掗梻浣筋嚃閸犳捇宕曢懠顒傜处闁伙絽鐬奸惌娆撴煠閹颁礁鐏￠柟鑼缁绘繈鎮介棃娑楁勃闂佹悶鍔岄悥濂稿箖閻戣姤鏅濋柛灞炬皑閿涚喖姊虹紒妯荤叆闁告艾顑夐幃陇绠涘☉姘絼闂佹悶鍎滅仦钘夊闂備浇顕х换鍡涘疾濠靛鐒垫い鎺嶇贰閸熷繘鏌涢悩铏磳鐎规洘绻堥獮瀣晝閳ь剛绮婚弽銊ょ箚闁靛牆鎳庨弳鐐烘煕閻樺弶顥㈤柡灞剧洴瀵挳濡搁妷銉骄婵＄偑鍊х拹鐔煎礉閹存繍娼栨繛宸簻缁犲綊鏌ｉ幇顓炵祷妞ゎ剙顦甸弻锝嗘償閿濆洦鍤勯梺鍛婃处閸樻悂寮ㄩ搹顐ょ瘈闁汇垽娼у瓭濠电偠顕滅粻鎾崇暦閹邦厾绡€婵﹩鍘鹃崣鍐ㄢ攽閳藉棗鐏熼悹鈧敃鈧嵄婵炲樊浜濋悡鏇熶繆椤栨粎甯涢柛濠冨姈閹便劍绻濋崨顕呬哗缂備緡鍠楅悷鈺呯嵁閹邦厽鍎熼柕蹇娾偓鎰佷紗缂傚倸鍊搁崐椋庢閿熺姴绐楅柡宥庡幗閸庢鏌涘畝鈧崑鐐哄磻濡眹浜滈柡鍥殔娴滄儳螖閻橀潧浠滄繛宸弮楠炲啳顦圭€规洜鍘ч～婵嬵敇閻戝棔绱叉繝鐢靛Х閺佹悂宕戦悩璇茬妞ゅ繐妫楃欢銈夋煙缁嬫寧鎹ｅù鐘崇娣囧﹪鎮欓鍕ㄥ亾閹达箑纾块弶鍫氭櫆瀹曞弶鎱ㄥ璇蹭壕閻庢鍠涢褔鍩ユ径濠庢僵闁挎繂鎳嶆竟鏇㈡⒑閹稿海绠撶紒銊ㄥ亹閼鸿鲸绺介崨濠勫幈闂侀潧鐗嗛幊蹇涘窗濮椻偓閺屸€崇暆閳ь剟宕伴弽顓炵鐟滅増甯╅弫鍐煏閸繃宸濋柡澶夌矙濮婄粯鎷呯粵瀣異闂佸摜濮甸〃濠囧箖閻戣棄鐓涢柛娑卞亐閸嬫挻鎷呴懖婵囨瀹曘劑顢橀悪鍛簥闂傚倷绶氬褔鎮ч崱妞㈡稑鈹戠€ｎ亝鐎┑鐘诧工鐎氼喚寮ч埀顒勬倵閸忕ǹ宓嗛柡鈧崡鐑嗙唵婵せ鍋撻柛鈹惧亾濡炪倖鍩堥崜娆撳箠鎼达絾顐介柕鍫濇偪瑜版帗鍋愮€瑰壊鍠栭崜浼存⒑濮瑰洤鈧倝宕板Δ鍛﹂柛鏇ㄥ灠閸愨偓濡炪倖鍔﹀鈧紒顔肩埣濮婅櫣绱掑Ο璇查瀺濠电偠灏欓崰鏍ь嚕婵犳碍鍋勯柛娑橈功缁夎埖淇婇妶蹇曞埌闁哥噥鍨堕、鎾愁吋婢跺鎷虹紓鍌欑劍钃遍悘蹇曞缁绘盯宕ｆ径妯煎姺闂佺懓绠嶉崹褰掑煡婢舵劕顫呴柣妯活問閸氬懘姊绘担铏瑰笡闁搞劑娼х叅闁靛牆顦惌妤呮煛閸モ晛鏋戦柛娆忕箻閺岀喖鎮滃Ο鑲╃暤婵炲瓨绮嶇换鍫ュ蓟閵堝牄浜归柟鐑樻⒒閺嗩偊姊洪悙钘夊姷濠碘€虫喘楠炴垿宕熼姣尖晠鏌ㄩ弴妤€浜鹃梺鍝勵儏閸婂灝顫忓ú顏勫窛濠电姴鍟伴ˇ顓㈡⒑鏉炴壆鍔嶉柟顔煎€搁悾宄扳攽閸ヨ埖效闁硅壈鎻徊鍧楁晬濠靛洨绠鹃弶鍫濆⒔鐎佃偐绱掗鐣屾噰鐎规洦鍋勭叅妞ゅ繐鎳庢禒鍝勵渻閵堝棛澧紒瀣灥闇夋い鏇楀亾闁哄瞼鍠栭、娆撴寠婢跺﹤袘闂備礁鐤囬～澶愬垂瑜版帒鐒垫い鎺戯功缁夐潧霉濠婂懎浠辩€规洘绻堥弫鍐焵椤掑嫧鈧棃宕橀鍢壯囨煕閳╁喚娈橀柣鐔村姂閺岋絾鎯旈姀鐘叉瘓闂佸憡鎸荤粙鎾诲礆閹烘挾绡€婵﹩鍓濋幗鏇㈡⒑闂堟侗妾у┑鈥虫喘瀹曘垽鏌嗗鍡忔嫼闂傚倸鐗婃笟妤€螣閸儲鐓曢柣妯虹－婢ь剟鏌￠崨顓犲煟妞ゃ垺鐩幃娆戔偓鐢登圭敮妤呮⒒閸屾瑧顦﹂柣蹇旂箞椤㈡牠宕ㄩ婊呯暥闂佸湱鍎ら〃鍡浰夐崱妤婄唵闁兼悂娼ф慨鍫ユ煟閹惧崬鍔滅紒缁樼箞濡啫鈽夊▎妯伙骏濠电姭鎷冮崱娑樻懙缂備胶绮惄顖氱暦濡ゅ懏鍋傞幖绮规濞兼岸姊绘担鍛婃儓闁哄牜鍓涚划娆撳箻鐠囪尙鍔﹀銈嗗笒閿曪妇绮旈悽鍛婄厱闁规儳顕幊鍥煕閳瑰灝鐏茬€规洖銈告俊鐑芥晜閹冪闂備浇顕х换鎺楀磻濞戞瑦娅犲ù鐘差儏閻撯偓闂佸搫娲㈤崹娲偂濞嗘劑浜滈柡宥冨妿閳洘銇勯妷銉у闁宠鍨块、娆撴偡妫颁礁顥氶梻鍌氬€烽懗鍓佸垝椤栫偞鏅濋柍鍝勬噹绾惧鏌涘☉妯兼憼闁稿﹤鐖奸弻娑㈠焺閸愵亖濮囬梺鎶芥敱鐢帡婀侀梺鎸庣箓閻楀﹪顢旈悩缁樼厪闁糕剝顨愰煬顒勬煛瀹€瀣瘈鐎规洘甯掗埢搴ㄥ箣閻橀潧搴婇梺璇叉唉椤煤濮椻偓瀹曞綊宕稿Δ鍐ㄧウ濠殿喗銇涢崑鎾搭殽閻愬弶澶勯柟宄版嚇閹兘骞嶉鍙帡姊婚崒姘偓椋庢濮橆剦鐒界憸鏃堝箖瑜斿畷鍗烆渻閵忥紕鈽夐摶鏍煕濞戝崬寮柡浣圭矋缁绘繈鎮介棃娴躲垺绻涚拠褏鐣甸柟顕嗙節瀵挳濮€閳ユ枼鍋撻悽鍛婄叆婵犻潧妫濋妤€霉濠婂嫮绠橀柍褜鍓濋～澶娒洪弽顓炲珘妞ゆ帒瀚粻鐐烘煏婵犲繐顩紒鈾€鍋撻梻浣告啞閸斿繘寮崒娑氼浄闁靛繈鍊栭埛鎴︽偣閸ワ絺鍋撻搹顐や邯濠电姭鎷冮崱妞ユ挾绱掗崒娑樻诞闁轰礁鍊垮畷婊嗩槾闁绘挻鎸荤换婵嬪閻樺樊鏆㈤柡瀣典簼閵囧嫰濡烽敃鈧崫娲煛瀹€瀣ɑ闁诡垱妫冮崹楣冨礃閼煎墎绐￠梻鍌欒兌鏋い鎴濇楠炴垿宕堕鈧拑鐔哥箾閹存瑥鐏╅柛妤佸▕閺屾洘绻涢崹顔煎濡ょ姷鍋涢澶婎潖妤﹁￥浜归柟鐑樻尰閺嗙姴鈹戦敍鍕哗婵☆偄瀚伴敐鐐剁疀閹句焦妞介、鏃堝川椤撴繄鏆楅梻浣烘嚀閸氬骞嗗畝鍕獥婵°倕鍟扮粈濠冧繆閵堝懏鍣洪柍閿嬪灩缁辨帞鈧綆浜滈惃锟犳煛閳ь剛绱掑Ο鑲╊啎闂佸憡鐟ラˇ杈ㄦ櫠閻㈠憡鐓欐い鏃囧Г閵囨繃銇勯姀鈽呰€块柟顔哄灲閹煎綊宕烽鐕佹綋闂傚倸鍊烽悞锕傛儑瑜版帒鍨傞柦妯侯樈閻掔晫鎲告惔锝囩焿闁圭儤顨嗛弲婊堟煟閿濆懐鐏辨い鏃€娲熷铏圭矙鐠恒劎顔戦梺绋款儐閸旀瑩鐛繝鍥х倞闁宠鍎虫禍楣冩偡濞嗗繐顏紒鈧崘顔界厱闁哄啠鍋撻柣鐔村劦閹箖鎮块妯规睏闂佸湱鍎ょ换鍐疾閳哄懏鈷戠紓浣姑慨锕傛煕閹炬潙鍝烘い銏＄懇瀵挳濮€閳锯偓閹风粯绻涢幘鏉戠劰闁稿鎸荤换娑欐媴閸愬弶鍣虹€规洘鐓￠弻娑㈩敃閻樻彃濮庣紒鐐劤缂嶅﹪寮婚垾鎰佸悑閹肩补鈧磭顔愰梻浣告啞閼规儳锕㈤柆宥呯劦妞ゆ巻鍋撻柛妯荤矒瀹曟垿骞樼紒妯煎幈闂佸搫娲㈤崝灞炬櫠娴煎瓨鐓曟慨妞诲亾缂佺姵鐗曢～蹇撁洪鍕獩婵犵數濮撮崯顐︽瀹ュ鍋℃繝濠傚缁舵煡鏌涢悢鍛婄稇闁伙絿鍏樻慨鈧柕鍫濇噹缁愭稒绻濋悽闈浶㈤柛鐕佸亜椤曪綁宕稿Δ浣哄幗闂佺粯鏌ㄩ幗婊堝箠閸愵喗鍊垫慨姗嗗墮濡插鏌嶇紒妯诲碍妤楊亙鍗冲畷鐔碱敆閳ь剙顕ｉ幐搴ｇ瘈闁汇垽娼у瓭闂佺懓鍟跨换姗€銆侀弮鍫熷亹闁汇垻鏁搁敍婊堟煛婢跺﹦澧戦柛鏂跨灱缁參骞掑Δ浣瑰殙闂佹寧绻傞ˇ浼存偂閺囩喓绡€闂傚牊绋掗ˉ婊勩亜韫囨挾鎽犵紒缁樼洴瀹曟宕楅悡搴ｏ紦婵犳鍠栭敃锔惧垝椤栫偛绠柛娑欐綑瀹告繂鈹戦悩鍙夊櫧妞ゃ垹鎳樺缁樻媴缁嬫寧姣愰梺鎼炲灪閻擄繝骞冭楠炴ê鐣烽崶銊︻啎闂備胶鎳撻顓㈠磻閻斿搫濮柍褜鍓熷娲箹閻愭彃濡ч梺绯曞墲钃遍柛鐔告そ濮婂宕掑▎鎴犵崲濠电偘鍖犻崱妤婃澓闂傚倷绀侀幖顐﹀箠韫囨洜鐭欓柟瀛樼贩濞差亜鍨傛い鏃囶潐閺傗偓闂備胶绮敃鈺呭窗閺嶎厽鍊堕弶鍫涘妿缁犳儳顭跨捄渚剳婵炴彃鐡ㄩ妵鍕閿涘嫬鈷岄悗瑙勬礀閻栧ジ銆佸Δ鍛劦妞ゆ帒瀚崕妤佷繆閵堝懏鍣洪柣鎾存礋閺屽秹鍩℃担鍛婃闂佹剚鍨卞ú鐔煎蓟瀹ュ鐓ラ悗锝庝簽娴犺偐绱撴担浠嬪摵闁圭ǹ顭烽獮蹇涘川鐎涙ê娈愰梺鏂ユ櫅閸燁偊鎮橀埡鍐＜妞ゆ棁鍋愭晶锕傛煕閳规儳浜炬俊鐐€栫敮鎺斺偓姘煎弮閸╂盯骞嬮悩鐢碉紲闁诲函缍嗘禍婊堟儍閿涘嫧鍋撶憴鍕仩闁稿氦灏欓幑銏犫攽鐎ｎ亞鍔﹀銈嗗笒鐎氼參鎮為懖鈹惧亾楠炲灝鍔氶柟铏姍瀹曟繈鏁冮埀顒勨€旈崘顔嘉ч柛鈩冾殘閻熸劙姊洪悡搴℃毐闁绘牜鍘ч悾鐑筋敍閻愯尪鎽曢梺闈涱檧婵″洭宕㈤悽鍛娾拺闁告稑锕ら悘鐔兼煕婵犲啰澧遍柍褜鍓氶悢顒勫箯閿燂拷
    new UncondBrInstruction(end_bb, else_bb);



    builder->setInsertBB(end_bb);
}

void CompoundStmt::genCode()
{
    // Todo
    if(stmt != nullptr)
        stmt->genCode();
}

void SeqNode::genCode()
{
    // Todo
    if (stmt1 != nullptr)
        stmt1->genCode();
    if (stmt2 != nullptr)
        stmt2->genCode();
}

void DeclStmt::genCode()
{
    IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry *>(id->getSymPtr());
    if(se->isGlobal())
    {
        //std::cout << id->getSymPtr()->toStr() << std::endl;
        //std::cout << "fuck" << std::endl;
        Operand *addr;
        SymbolEntry *addr_se;
        addr_se = new IdentifierSymbolEntry(*se);
        addr_se->setType(new PointerType(se->getType()));
        addr = new Operand(addr_se);
        se->setAddr(addr);
        Instruction *g;
        //initVal->genCode();
        g = new GlobalInstruction(new Operand(id->getSymPtr()), nullptr, se);
        g->output();
     
    }
    else if(se->isLocal())
    {
        //std::cout << "you" << std::endl;
        switch (se->getType()->get_range())
        {
        case 2:
            Function * func = builder->getInsertBB()->getParent();
            BasicBlock* entry = func->getEntry();
            Instruction* alloca;
            Operand* addr;
            SymbolEntry* addr_se;
            Type* type;
            type = new PointerType(se->getType());
            addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
            addr = new Operand(addr_se);
            alloca = new AllocaInstruction(addr, se);                   // allocate space for local id in function stack.
            entry->insertFront(alloca);                                 // allocate instructions should be inserted into the begin of the entry block.
            se->setAddr(addr);
            break;
        }
                                               // set the addr operand in symbol entry so that we can use it in subsequent code generation.
    }
    else
    {
        std::cout << "what the hell are you trying to decl?!" << std::endl;
    }
}

void ReturnStmt::genCode()
{
    //Todo
    BasicBlock* ret_bb = builder->getInsertBB();
    Operand* src=nullptr;
    if(retValue){
        retValue->genCode();
        src=retValue->getOperand();
    }
    new RetInstruction(src,ret_bb);
}

void AssignStmt::genCode()
{

    BasicBlock *bb = builder->getInsertBB();
    Operand *addr = dynamic_cast<IdentifierSymbolEntry*>(lval->getSymPtr())->getAddr();
    expr->genCode();
    Operand* src = expr->getOperand();
    /***
     * We haven't implemented array yet, the lval can only be ID. So we just store the result of the `expr` to the addr of the id.
     * If you want to implement array, you have to caculate the address first and then store the result into it.
     */
    new StoreInstruction(addr, src, bb);
}

void Ast::typeCheck()
{
    if(root != nullptr)
        root->typeCheck();
}

void FunctionDef::typeCheck()
{
    // Todo
    temp_funct = this;
    SymbolEntry *se = this->getSymbolEntry();
    Type *ret = ((FunctionType *)(se->getType()))->getRetType();
    if (stmt == nullptr&&ret != TypeSystem::voidType)
    {   
         fprintf(stderr, "function\'%s\'misses return\n",se->toStr().c_str());
        // 闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閹冣挃闁硅櫕鎹囬垾鏃堝礃椤忎礁浜鹃柨婵嗙凹缁ㄥジ鏌熼惂鍝ョМ闁哄矉缍侀、姗€鎮欓幖顓燁棧闂備線娼уΛ娆戞暜閹烘缍栨繝闈涱儐閺呮煡鏌涘☉鍗炲妞ゃ儲鑹鹃埞鎴炲箠闁稿﹥顨嗛幈銊╂倻閽樺锛涘┑鐐村灍閹崇偤宕堕浣镐缓缂備礁顑嗙€笛囨倵椤掑嫭鈷戦柣鐔告緲閳锋梻绱掗鍛仸鐎规洘鍨块獮鍥偋閸垹骞嶇紓鍌氬€烽悞锕傛晪缂備焦銇嗛崶銊у帗閻熸粍绮撳畷婊堟晝閸屾氨鐓戦梺鍛婂姦閻撳牆顭囬弽顓熺叄闊洦鎸荤拹锟犳煟椤撶喓鎳囬柟顔肩秺楠炰線骞掗幋婵愮€锋俊鐐€栭弻銊ッ洪鐑嗘綎婵炲樊浜滃婵嗏攽閻樺弶鍣规い銉︾箓椤啴濡堕崱妤冧淮濡炪倧瀵岄崹鍫曞箖妤ｅ啯鍊婚柤鎭掑劜濞呮粓姊洪崜鎻掍簴闁搞劌顭峰鍓佺矙鎼存挻鏂€闂佺粯鍔栧娆撴倶閿曞倹鐓熼柣鏃€绻傚ú銈夊磼閵婏负浜滈煫鍥ㄦ尵婢ф盯鏌嶉柨瀣伌闁哄本鐩獮姗€寮堕幋鐘点偡濠电偛鐡ㄧ划宥囨崲閸曨厽顫曢柟鐑樻⒐鐎氭岸鏌ょ喊鍗炲姰闁靛繈鍊曠涵鈧梻渚囧墮缁夌敻鎮￠弴鐔虹闁瑰鍎戦崗顒勬煛閳ь剚绻濆顓犲幈濠电偛妫楃换鎺旂不婵犳碍鐓欐い鏃傚帶濡插鏌嶇拠鍙夊攭缂佺姵鐩獮娆撳礃瑜忕壕濠氭⒒閸屾瑧顦﹂柟璇х節閹兘濮€閵堝懐锛涢梺鐟板⒔缁垶鍩涢幒鎳ㄥ綊鏁愰崨顔兼殘闂佽鍨伴悧鎾诲蓟閿濆鏁囬柣鎴濇妤旂紓鍌欐祰娴滎剟宕戦悙鍝勭闁告稒娼欑粈鍫ユ煙缂佹ê绗氭い鎾村娣囧﹪鎮欓鍕ㄥ亾閺嶎偅鏆滈柟鐑樻煛閸嬫挸顫濋悡搴☆潾缂備緡鍠氱划顖溾偓闈涖偢瀵爼骞嬮悪鈧崯鍥╃磽閸屾瑧璐伴柛鐘冲哺楠炲﹪骞樼€涙ɑ鐝烽梺鍛婄懃椤﹁京绮绘ィ鍐╃厱妞ゆ劑鍊曢弫鐐亜韫囨挾澧曢柣鎾达耿閺岀喐娼忔ィ鍐╊€嶉梺绋款儐閸旀瑩寮婚悢闈╃矗濞达絼璀﹀Σ顕€姊洪崫鍕殗濞存粏娉涢～蹇撁洪鍕啇闂佺粯鍔栬ぐ鍐綖閸ャ劎绠鹃悗娑欘焽閻﹪鏌ｉ弽褋鍋㈤柟顔诲嵆椤㈡岸鍩€椤掑嫮宓侀柛銉墮缁狙囨偣娓氼垳鍘涢柛鐐存尦濮婄粯鎷呴挊澹捇鏌ㄥ顓滀簻闁挎棁妫勯埢鏇燁殽閻愭惌娈滅€殿喗鎸虫慨鈧柍閿亾闁圭柉娅ｇ槐鎾诲磼濞嗘垵濡介柦鍐憾閺屽秹鏌ㄧ€ｎ亝璇為梺鍝勬湰缁嬫垼鐏掗梺缁樏鍫曞闯閽樺妲堥柟鎯х－鏍￠梺閫涚┒閸斿矁鐏掗梺绉嗗嫷娈旈柟鐣屾暬濮婃椽宕妷銉愶綁鏌涢弮鈧悷鈺呭箖閿熺媭鏁冮柨鏇楀亾闁绘劕锕弻鏇熺節韫囨艾鎮呴梺鍦焿椤曆団€旈崘顔嘉ч柛灞剧⊕閻濇棃鎮楀▓鍨灈闁绘牕鍚嬫穱濠囨偨缁嬭法顦板銈嗘煥婢т粙寮堕幖浣光拺閻熸瑥瀚粈鍐╃箾婢跺娲寸捄顖炴煃瑜滈崜鐔奉潖缂佹ɑ濯撮柧蹇曟嚀缁楋繝姊虹憴鍕憙鐎规洜鏁搁崚鎺戔枎閹惧磭顓洪梺鎸庢⒒閺咁偊鎮鹃幎鑺モ拻闁稿本鐟ч崝宥夋倵缁楁稑鎳忓畷鏌ユ煕鐏炵虎鍤ч柟鐑橆殔鍞梺闈涒康缁茬厧效濡ゅ懏鈷戦柛锔诲幖閸斿绱掔拠鎻掓殻鐎殿喗褰冮オ浼村礃閻愵剙鐦滈梻渚€娼ч悧鍡椢涘▎鎴滅剨妞ゅ繐鐗婇悡鏇㈡煏婵犲繘妾柕鍥ㄧ箞閺岀喖顢欓崗澶婁壕闁哄倶鍎查～宥夋⒑闂堟稓绠氶柍褜鍓氶弸濂稿磻閹捐閱囬柡鍥╁枔閸樹粙姊洪柅娑樺祮闁稿锕璺好洪鍛幈闂侀潧鐗嗗Λ娑㈠箠閸愵喗鐓冮悷娆忓閻忓瓨銇勯姀锛勬噰闁诡喒鍓濋幆鏂课熸潪鏉款棜闂備胶顫嬮崟鍨暥闂佽棄鍟伴崰鏍蓟閿濆绫嶉柛灞绢殕鐎氭盯姊烘潪鎵槮闁挎洦浜璇测槈濡攱鏂€闂佺硶鍓濋〃蹇旂闁秵鈷戠紒瀣儥閸庢劙鏌熼崨濠冨€愰柛鈺冨仱楠炲鏁冮埀顒勭嵁閵忥紕绠鹃柛鈩兠慨鍫熴亜閺冣偓濞茬喎顫忕紒妯诲闁告稑锕ㄧ涵鈧梻浣呵圭花娲磹濠靛棛鏆︾憸鐗堝笚閸嬪嫰鏌ら懠顒€鐨戠紓宥勭閻ｉ攱绺介崨濠備簻缂備礁顑呴悘婵囧鎼淬垻绡€鐎典即鏀卞姗€鍩€椤掍礁濮嶇€规洘鍨块獮妯肩礄閻樼數鐣炬俊鐐€栭崝褏寰婃ィ鍐ㄧ厱闁圭儤鎸风换鍡涙煕濞嗗浚妲稿┑顔肩Ч閺屸剝鎷呯憴鍕３闂佽桨鐒﹂幑鍥极閹剧粯鏅搁柨鐕傛嫹?闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閹冣挃闁硅櫕鎹囬垾鏃堝礃椤忎礁浜鹃柨婵嗙凹缁ㄥジ鏌熼惂鍝ョМ闁哄矉缍侀、姗€鎮欓幖顓燁棧闂備線娼уΛ娆戞暜閹烘缍栨繝闈涱儐閺呮煡鏌涘☉鍗炲妞ゃ儲鑹鹃埞鎴炲箠闁稿﹥顨嗛幈銊╂倻閽樺锛涘┑鐐村灍閹崇偤宕堕浣镐缓缂備礁顑嗙€笛囨倵椤掑嫭鍊垫鐐茬仢閸旀碍銇勯敂璇茬仯缂侇喖鐗忛埀顒婄秵閸嬩焦绂嶅⿰鍫熺厵闁告繂瀚倴闂佸憡鏌ㄧ粔鐢稿Φ閸曨垰妫橀柟绋块閺嬬姴鈹戦纭峰姛缂侇噮鍨堕獮蹇涘川椤栨稑鏋傞梺鍛婃处閸嬪倿宕Δ鍛拻濞达絽鎳欓崷顓涘亾濞戞帗娅呮い顏勫暣閹稿﹥寰勭仦鐐啎婵犵數濮撮敃銈夊疮閻樿鍑犻幖娣妽閻撴瑩鎮楀☉娆樼劷缂佺姰鍎叉穱濠囶敃閿濆洨鐓夐梺璇″枛閸㈡煡銈导鏉戝窛妞ゆ挆鍕様闂備焦鐪归崺鍕垂闁秲鈧啯绻濋崶褏鐣洪梺鐐藉劜閺嬬厧危閸喐鍙忔慨妤€鐗忛崚鏉棵瑰⿰鍛壕妞ゃ劊鍎甸幃娆撴嚑椤戣儻妾搁梻浣告啞濮婂綊鎳濋幑鎰簷闂備胶绮灙婵炲懏娲熷畷鎰板箛閻楀牏鍘藉┑鐐村灥瀹曨剙鈻嶅鍥ｅ亾濞堝灝鏋欑紒顔界懇閵嗕礁顫滈埀顒勫箖閵忋倕浼犻柛鏇ㄥ亞閻嫰姊婚崒姘偓椋庣矆娓氣偓楠炲鏁撻悩鑼槷闂佹寧姊婚弲顐︺€呴崣澶岀瘈濠电姴鍊绘晶鏇㈡煕閹寸姴校缂佺粯鐩獮瀣枎韫囨洑鐥梻浣虹帛閹稿鎯勯鐐茬畺婵せ鍋撻柟顔界懇楠炴捇骞掗崱妯虹槺濠电姷鏁搁崑娑橆嚕鐠轰警鐒芥繛鍡樻尭閻撴﹢鏌熸潏楣冩闁哄拋鍓熼弻娑㈠即閵娿儰绨诲Δ鐘靛仜椤戝懘鈥旈崘顔嘉ч柛鎰╁妿娴犻箖姊洪懡銈呮毐闁哄懐濞€閻涱噣宕橀鑲╁姶闂佸憡鍔忛弲娑㈠礉閸涱収娓婚柕鍫濇閳锋劙鏌涢妸锔剧畵妞ゎ偅绻堥弫鍐磼濞戞帗瀚介梻浣呵归張顒勬嚌妤ｅ啫鐒垫い鎺戝濡垹绱掗鑲╁缂佹鍠栭崺鈧い鎺戝瀹撲礁顭块懜闈涘闁哄懏鎮傞弻锝呂熼崫鍕瘣婵炲濯崣鍐潖濞差亝顥堟繛鎴ｉ哺瀛濋梻浣告惈閹冲繒鍒掗幘鎰佸殨闁圭粯宸婚弨浠嬫倵閿濆骸浜為柛姗嗕簼缁绘繈濮€閿濆棛銆愬┑鈽嗗亝閻熴儵鍩㈠澶婂窛妞ゆ挆鍕洬闂備礁鎲＄粙鎴︽偤閵娾晛鐓曢柟杈鹃檮閻撴洘绻涢幋鐑囧叕闁衡偓鐠囧樊鐔嗛悹鍝勬惈椤忣參鏌＄仦鍓ф创濠碘剝鎮傛俊鍫曞川椤撶姰鍋栭梺璇叉唉椤煤濮椻偓閺佸啴濮€閵堝懏妲梺閫炲苯澧柕鍥у楠炴帡骞嬪┑鎰棯闂備焦濞婇弨閬嶅垂閸噮娼栧┑鐘宠壘闁卞洭鏌ｉ弮鈧崕濂稿触閸屾埃鏀介柣鎰綑閻忥箓鏌ｉ褍鏋涢柍璁崇矙椤㈡棃宕奸悢鍝勫箺闂備線鈧稑宓嗘繛浣冲嫭娅犲┑鐘崇閻撴洟鏌ｉ弮鍫熸暠闁哥姵蓱閹便劍绻濋崒銈囧悑閻庤娲樼敮鎺楋綖濠靛鏁勯柦妯侯槷婢规洟姊虹紒妯虹伇濠殿喓鍊濆畷鐢稿礋椤栨稓鍘鹃梺鍛婄缚閸庢煡寮抽埡浼卞綊鎮╃€圭姴顥濋梺瀹狀潐閸ㄥ爼鐛繝鍥ㄧ厱濠电姴鍠氬▓鏇㈡煙娓氬灝濮傞柟顔炬櫕缁瑧鎹勯妸褎姣囬梻鍌欒兌鏋柣顒€銈稿畷鎴濃槈閵忕姴鍤戞繛鎾村焹閸嬫捇鏌＄仦鍓ф创妤犵偛娲Λ鍐ㄢ槈濞嗘垳鎲鹃梻鍌欑閹碱偊骞婅箛鏇炲灊闁规崘顕ч拑鐔兼煥濠靛棭妲哥紒鐘崇洴濮婂宕奸悢琛℃闂佹娊鏀辩敮鎺楀煘閹达附鍊烽悹楦挎〃濮规姊洪崫銉バｉ柣妤佹礋閿濈偠绠涢幘浣规そ椤㈡棃宕熼鈧粊鑸典繆閻愵亜鈧牠骞愭ィ鍐ㄧ；婵炴垶鐟ょ换鍡樼箾瀹割喕绨奸柍閿嬪灴閺岀喖鎳栭埡浣风捕闂侀€炲苯鍔柛娑卞灟缁楀姊洪崷顓炰壕闁告挻宀稿畷鐢稿即閻旂繝绨婚梺鍝勬处椤ㄥ懏绂嶆ィ鍐╁€甸悷娆忓缁€鍐偨椤栨稑绗╅柣蹇撳暣濮婃椽鏌呴悙鑼跺濠⒀屽灣缁辨帡宕掑姣欙綁鏌曢崼顒傜М鐎规洘锕㈤崺锟犲礃閵娿儲杈堟繝鐢靛Х閺佸憡绻涢埀顒佺箾娴ｅ啿鍚樺☉妯锋斀閻庯絽鐏氶弲娑樷攽鎺抽崐鎰板磻閹炬番浜滄い鎾偓鍐插Х濡炪倧绠撴禍鍫曞蓟濞戞鏃€鎷呯化鏇熺亞闁诲孩顔栭崰鏍€﹂悜钘夋瀬闁瑰墽绮崑鎰版煙缂佹ê绗╅柍褜鍓欓悘婵嬪煘閹达附鍊烽柛娆忣樈濡偟绱撴担绛嬪殭婵☆偅绻傞锝夊箹娴ｇǹ绐涘銈嗘尰缁嬫帡寮埀顒佷繆閻愵亜鈧牕顫忔繝姘ラ悗锝庡€犲☉姗嗘僵妞ゆ挾濮烽鏇㈡⒑閸涘﹦鐭婇柛鐔稿缁棃顢欑粵瀣啍闂佺粯鍔樼亸娆戠不婵犳碍鐓欐い鏃囶潐濞呭洭鏌熸搴♀枅闁瑰磭濞€椤㈡牜鎹勯妸褜妲堕梻鍌氬€搁崐鐑芥倿閿曞倸纾块柛鎰梿閻熼偊娼╅悹楦挎閿涙盯姊虹化鏇炲⒉妞ゎ厼娲妴鍛村矗婢跺牅绨婚棅顐㈡处閹稿藟閻樼數妫柛娆嶅劤閻瑩鏌＄仦鍓с€掑ù鐙呯畵瀹曟粏顦抽柛锝庡櫍濮婃椽宕ㄦ繝鍐弳缂備礁顦紞濠傤嚕鐠囨祴妲堥柕蹇曞Х閻嫰姊洪柅鐐茶嫰婢ф挳鏌熼銊ユ处閸嬫劗鈧懓澹婇崰鏍р枔閵娾晜鈷戦梻鍫熶腹濞戙垹绀冩い顓熷灥濞堫厾绱撻崒姘偓鐑芥倿閿曞倸绀冨┑鐘宠壘缁狀垶鏌ㄩ悤鍌涘
    }
    // 闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閹冣挃闁硅櫕鎹囬垾鏃堝礃椤忎礁浜鹃柨婵嗙凹缁ㄥジ鏌熼惂鍝ョМ闁哄矉缍侀、姗€鎮欓幖顓燁棧闂備線娼уΛ娆戞暜閹烘缍栨繝闈涱儐閺呮煡鏌涘☉鍗炲妞ゃ儲鑹鹃埞鎴炲箠闁稿﹥顨嗛幈銊╂倻閽樺锛涘┑鐐村灍閹崇偤宕堕浣镐缓缂備礁顑嗙€笛囨倵椤掑嫭鈷戦柣鐔告緲閳锋梻绱掗鍛仸鐎规洘鍨块獮鍥偋閸垹骞嶇紓鍌氬€烽悞锕傛晪缂備焦銇嗛崶銊у帗閻熸粍绮撳畷婊堟晝閸屾氨鐓戦梺鍛婂姦閻撳牆顭囬弽顓熺叄闊洦鎸荤拹锟犳煟椤撶喓鎳囬柟顔肩秺楠炰線骞掗幋婵愮€锋俊鐐€栭弻銊ッ洪鐑嗘綎婵炲樊浜滃婵嗏攽閻樺弶鍣规い銉︾箓椤啴濡堕崱妤冧淮濡炪倧瀵岄崹鍫曞箖妤ｅ啯鍊婚柤鎭掑劜濞呮粓姊洪崜鎻掍簴闁搞劌顭峰鍓佺矙鎼存挻鏂€闂佺粯鍔栧娆撴倶閿曞倹鐓熼柣鏃€绻傚ú銈夊磼閵婏负浜滈煫鍥ㄦ尵婢ф盯鏌嶉柨瀣伌闁哄本鐩獮姗€寮堕幋鐘点偡濠电偛鐡ㄧ划宥囨崲閸曨厽顫曢柟鐑樻⒐鐎氭岸鏌ょ喊鍗炲姰闁靛繈鍊曠涵鈧梻渚囧墮缁夌敻鎮￠弴鐔虹闁瑰鍎戦崗顒勬煛閳ь剚绻濆顓犲幈濠电偛妫楃换鎺旂不婵犳碍鐓欐い鏃傚帶濡插鏌嶇拠鍙夊攭缂佺姵鐩獮娆撳礃瑜忕壕濠氭⒒閸屾瑧顦﹂柟璇х節閹兘濮€閵堝懐锛涢梺鐟板⒔缁垶鍩涢幒鎳ㄥ綊鏁愰崨顔兼殘闂佽鍨伴悧鎾诲蓟閿濆鏁囬柣鎴濇妤旂紓鍌欐祰娴滎剟宕戦悙鍝勭闁告稒娼欑粈鍫ユ煙缂佹ê绗氭い鎾村娣囧﹪鎮欓鍕ㄥ亾閺嶎偅鏆滈柟鐑樻煛閸嬫挸顫濋悡搴☆潾缂備緡鍠氱划顖溾偓闈涖偢瀵爼骞嬮悪鈧崯鍥╃磽閸屾瑧璐伴柛鐘冲哺楠炲﹪骞樼€涙ɑ鐝烽梺鍛婄懃椤﹁京绮绘ィ鍐╃厱妞ゆ劑鍊曢弫鐐亜韫囨挾澧曢柣鎾达耿閺岀喐娼忔ィ鍐╊€嶉梺绋款儐閸旀瑩寮婚悢闈╃矗濞达絼璀﹀Σ顕€姊洪崫鍕殗濞存粏娉涢～蹇撁洪鍕啇闂佺粯鍔栬ぐ鍐綖閸ャ劎绠鹃悗娑欘焽閻﹪鏌ｉ弽褋鍋㈤柟顔诲嵆椤㈡岸鍩€椤掑嫮宓侀柛銉墮缁狙囨偣娓氼垳鍘涢柛鐐存尦濮婄粯鎷呴挊澹捇鏌ㄥ顓滀簻闁挎棁妫勯埢鏇燁殽閻愭惌娈滅€殿喗鎸虫慨鈧柍閿亾闁圭柉娅ｇ槐鎾诲磼濞嗘垵濡介柦鍐憾閺屽秹鏌ㄧ€ｎ亝璇為梺鍝勬湰缁嬫垼鐏掗梺缁樏鍫曞闯閽樺妲堥柟鎯х－鏁堥梺鍝勭焿缁辨洘绂掗敂鐐珰闁圭粯甯掗～姘舵⒒娴ｈ櫣甯涢柟绋款煼閹兘鍩℃笟鍥ㄧ稁濠电偛妯婃禍鍫曞极閸ヮ剚鐓忓┑鐐戝啯鍣烘繛鍫⑶归埞鎴︽偐椤愶絽顎忛梺鍛婁緱閸欏酣寮抽銏♀拺闂侇偅绋撻埞鎺楁煕閺冣偓閸ㄥ灝顕ｉ锕€绠涢柡澶庢硶椤︺劑姊洪棃娴ゆ盯宕掑⿰鍜佹П濠电姷鏁告慨鐢割敊閺嶎厼绐楅柡宥庡幗閺呮繈鏌曟径鍫濆姶婵炴捁顕ч湁闁绘ê妯婇崕蹇曠磼閻橀潧鈻堥柡灞诲姂閹垽宕崟鎴欏灲瀹曞爼骞橀瑙ｆ嫼闂佸憡绋戦敃锝囨闁秵鐓曢柣妯哄暱閸濇椽鏌涢埞鎯т壕婵＄偑鍊栧濠氬磻閹炬剚鐔嗙憸蹇涘疾椤愨懡锝夊箛閺夎法顔掗柣搴㈢⊕閿氭い搴㈡尵缁辨挻鎷呴崷顓涙濡炪倖鍨甸幊搴ㄥ煝閹炬椿鏁囬柕蹇婃閹疯櫣绱撻崒娆戝妽闁挎洍鏅涢埢鎾诲即閵忥紕鍘甸梺鍛婄☉閿曘儵宕愰幇顓滀簻闁靛繆鈧啿鎽甸梺杞扮劍閸旀瑥鐣烽崡鐐╂瀻闁瑰濮甸～灞解攽閻樻剚鍟忛柛鐘愁殜閺佸啴鍩￠崨顓狅紵闂佹眹鍨绘灙闁绘挻鐩弻娑樷槈閸楃偞鐏撻梺鍛娚戦幑鍥ь潖濞差亜宸濆┑鐘插閻ｇ敻鏌ｆ惔銏犲毈闁告挾鍠栧畷娲焵椤掍降浜滈柟鍝勬娴滈箖姊虹涵鍜佸殝缂佽鲸娲滈崚鎺斺偓锝庝憾閸氬顭跨捄渚剰闁逞屽墰閸忔﹢寮婚敐澶婄闁绘劕鍟畝鎼佺嵁韫囨稒鎯為柛锔诲幘閿涙繃绻涙潏鍓у埌濠㈢懓锕畷鏇㈠箛閻楀牆鈧數绱掑Δ浣衡槈闁哥姵鐩幊鎾诲锤濡や胶鍘靛┑鐐茬墕閻忔繈寮搁妶鍥╂／闁诡垎浣镐划闂佽鍠楅〃濠囧极閹邦厽鍎熼柍銉ㄥ皺閻╁酣姊绘担绛嬪殭缂佺粯鍔欓幃娲Ω閳轰絼锕傛煕閺囥劌鐏犳い顐㈡嚇閺屽秹鍩℃担鍛婄亾濠碘槅鍋勯崯鏉戭潖閻戞ɑ濮滈柟娈垮枛婵′粙姊洪崷顓熷殌婵炲樊鍘奸锝夘敃閿曗偓缁犺崵绱撴担鑲℃垵鈻嶉姀銈嗏拺閻犳亽鍔屽▍鎰版煙閸戙倖瀚�?闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閹冣挃闁硅櫕鎹囬垾鏃堝礃椤忎礁浜鹃柨婵嗙凹缁ㄥジ鏌熼惂鍝ョМ闁哄矉缍侀、姗€鎮欓幖顓燁棧闂備線娼уΛ娆戞暜閹烘缍栨繝闈涱儐閺呮煡鏌涘☉鍗炲妞ゃ儲鑹鹃埞鎴炲箠闁稿﹥顨嗛幈銊╂倻閽樺锛涘┑鐐村灍閹崇偤宕堕浣镐缓缂備礁顑嗙€笛兾ｉ鍕拺闁告稑锕ユ径鍕煕閵婏箑顥嬬紒顔肩墦瀹曟﹢鍩炴径鍝ョ泿闂傚⿴鍋勫ú锕傚箰閹间礁姹查柣鏂挎憸缁犳儳顭跨捄琛″悍闂婎剦鍓氶幈銊︾節閸曨厼绗￠梺鐟板槻閹虫劗鍒掗悽纰樺亾閿濆簼绨兼い鏂垮缁绘繂鈻撻崹顔界亶闂佹寧娲﹂崑鍡欏垝婵犳艾绠荤紓浣股戝▍婊堟⒑缂佹ê濮堟繛鍏肩懅濞嗐垽鎮欏ù瀣杸闂佺粯鍔栧娆戠箔濮橆厺绻嗛柣鎰閻瑦顨ラ悙鎻掓殻濠殿喒鍋撻梺鎸庣☉鐎氭澘顬婇鐣岀瘈闁靛骏绲剧涵楣冩煠濞茶鐏￠柟渚垮妼鐓ゆい蹇撴噽閸樹粙姊虹紒妯荤叆闁硅姤绮撻幆宀€鈧綆鍋傜换鍡樸亜閹扳晛鐏╅柡鍡樼懄閵囧嫰寮撮鍡櫺滃銈冨灪濞茬喖寮崘顔肩劦妞ゆ帒鍊婚惌鍡涙煕閹般劍鏉哄ù婊勭矒閺岋繝宕橀妸銉㈠亾閻㈢ǹ绠柍鈺佸暟缁犻箖鏌ｉ幘鍐插毐闂婎剦鍓氶幈銊︾節閸愨斂浠㈠┑鈽嗗亜閸燁偊鍩ユ径濠庢僵妞ゆ帒顦板В鍥⒒閸屾瑨鍏岄柟铏崌瀹曨垶宕归姘愁潐椤︾増鎯旈垾宕囦喊闂備焦瀵х换鍌炲箠婢跺á锝夊醇閵夛妇鍘棅顐㈡储閸庡磭澹曢崸妤佺厱婵☆垰婀遍惌娆撴煛瀹€鈧崰鏍х暦濡ゅ懏鍤冮柍鍝勫€归鍐⒒娴ｈ鍋犻柛鏂跨焸閳ワ箓鎮滈挊澶嬫К婵犵數濮村ú銈夊础閹惰姤鐓忛煫鍥ュ劤绾惧潡鏌涘▎蹇旑棞闁宠鍨块、娆戞喆閹烘垹绉虹€规洘妞藉浠嬵敇閻旇渹绨甸梻浣告惈濞层垽宕瑰ú顏呭亗婵炲棙鎸婚悡鐔兼煙娴煎瓨娑у褜鍨崇槐鎾愁吋閸℃浼岄梺鍝勮嫰缁夊墎妲愰幒鎳崇喖鎳￠妶搴⑿ч梻鍌欑閹碱偅寰勯崶顒€鐒垫い鎺嗗亾缁剧虎鍙冨鎶藉幢濞戞瑥鈧敻鏌ㄥ┑鍡涱€楀褌鍗抽幏鎴︽焼瀹ュ棌鎷绘繛杈剧悼閻℃棃宕甸崘顔界厱闁靛ǹ鍎遍懜褰掓懚閻愮儤鐓曢柟浼存涧閺嬫棃鏌嶈閸忔稓绮堟笟鈧崺銉﹀緞婵炪垻鍠庨悾锟犲箥椤旂⒈娼介梻鍌氬€风粈渚€骞夐敍鍕煓闁硅揪绠戠粻顖炴煙鐎电ǹ啸缂佲偓婵犲伅褰掓晲閸涱収妫屽┑鐐插悑閻楃娀寮昏缁犳盯骞樼壕瀣攨闂備焦妞块崢浠嬫偡閿旂偓宕叉繛鎴欏灩楠炪垺淇婇姘儓濠殿喓鍨藉娲川婵犲啫鐭梺鍛婎殔閸熷潡锝炶箛娑欐優閻熸瑥瀚崢褰掓⒑閸涘﹣绶遍柛鐘充亢閵囨劘顦规慨濠冩そ瀹曘劍绻濋崘锝嗗闂備浇宕甸崯鍧楀疾閻樼粯鍋樻い鏇楀亾鐎规洘锕㈤、娆撴寠婢跺妫┑锛勫亼閸婃牠鎮ч鐘茬筏濡わ絽鍟悡婵嬫倶閻愭彃鈷旂紒鈾€鍋撴繝鐢靛仜閻楀棝鎮樺┑瀣嚑婵炴垯鍨洪悡銉╂煛閸ユ湹绨绘い鈺婂墴閺屾盯骞掗幘铏癁濡炪們鍨洪惄顖炲箖濞嗘挻鍤戞い鎺戭槸閺佹悂姊婚崒娆戭槮闁硅绱曠划娆撳箳閹搭厾鍔峰┑鐐叉濞存岸宕崨顔剧瘈闂傚牊绋掗ˉ鎴︽煛閳ь剚绂掔€ｎ偆鍘藉┑鈽嗗灥椤曆呭緤婵犳碍鐓冪憸婊堝礈濞戙垹绠犻柟鎹愵嚙閻撴繈鏌熼悙顒€澧繛绗哄姂閺屽秷顧侀柛鎾寸懇閿濈偠绠涢弴鐘碉紲濠碘槅鍨甸褔顢撻幘缁樷拺闁诡垎鍛唺闂佺娅曢幐鍓у垝椤撱垹鐏抽柡鍌樺劜閺傗偓闂備礁鐤囧Λ鍕涘Δ浣侯洸婵犲﹤鎳愮壕鑲╃磽娴ｅ厜妫ㄦい蹇撶墛閸嬧晝鈧懓瀚竟瀣醇椤忓牊鐓曢柟閭﹀枛娴滈箖鏌涘澶嬫锭妞ゎ亜鍟存俊鍫曞幢濡儤娈┑鐘愁問閸ｎ噣宕ｉ崘顭戝殨闁靛ň鏅滈幆鐐淬亜閹扳晛鈧鎯侀崼銉︹拺婵懓娲ら悘鍙夌箾娴ｅ啿鍟伴幗銉モ攽閻樺灚鏆╅柛瀣洴閹洦瀵奸弶鎴狅紮濠电娀娼ч鍛不閺嶃劎绠鹃柛鈩兠慨澶岀磼閳锯偓閸嬫捇姊绘担鍛婂暈闁告枮鍛＝婵ǹ绮炬慨鍐测攽閻樺磭顣查柍閿嬪灴閺屾盯鏁傜拠鎻掔闂佸憡鏌ㄩ澶愬蓟濞戙垹鐓涘ù锝呮惈椤ｆ椽鏌涘Δ鍕惈闁瑰弶鎮傞幃褔宕煎┑鍫㈡嚃闂備胶绮幐濠氭儎椤栫偛钃熸繛鎴炃氬Σ鍫ユ煕濡ゅ啫浠уù鐘櫆缁绘稓鈧數枪椤庢挸鈹戦垾铏窛婵″弶鍔欓獮鎺楀籍閸屾粣绱叉繝纰樻閸ㄤ即宕ョ€ｎ喖绀嗘繛鎴烆焸閺冨牊鍋愰梻鍫熺◥濞岊亪姊洪幐搴㈢８闁搞劌缍婇、姘舵晲婢跺﹪鍞堕梺鍝勬川閸嬬喖顢樺ú顏呪拺缂備焦銆為幋锝冧汗闁告劦鍘搁崥鍌炴⒒閸屾艾鈧悂宕愰幖浣哥９闁绘垼濮ら崵鍕煠閸濄儲鏆╁ù鐘崇娣囧﹪鎮欓鍕ㄥ亾閺嶎厼鍨傞梻鍫熷厷濞戞ǚ鏀介悗锝庡墮缁侊箓姊鸿ぐ鎺戜喊闁告ê缍婂畷鎴﹀川鐎涙鍘遍梺瑙勫閺呮稒淇婇崸妤佺厱闁哄啯鎸鹃悾杈ㄣ亜椤忓嫬鏆ｅ┑鈥崇埣瀹曞崬螖閸愵亝鍣梻鍌欑閹芥粍鎱ㄩ悽鍛婂殞濡わ絽鍟犻埀顒婄畵瀹曞爼顢楅埀顒傜不濞戞瑣浜滈柟鎹愭硾鍟稿銈嗘⒐濞茬喎顫忓ú顏勬嵍妞ゆ挾濮寸粭锟犳煟閵忊晛鐏ｉ柛瀣枔閸掓帗绻濆顓炴闂侀潧鐗嗛崐鍛婄妤ｅ啯鍋℃繛鍡楃箰椤忣亞绱掗埀顒勫礃閳哄啰顔曢梺鑲┾拡閸撴瑩寮告惔銊︽嚉闁绘劗鍎ら悡鏇㈡煙閹佃櫕娅呭┑锟犳敱娣囧﹪骞撻幒鏂款杸闂侀€涚┒閸斿矂锝炲⿰鍫濆耿婵°倐鍋撶紒鐘叉惈椤啴濡堕崱妯锋嫻濠电偠灏欓崰搴綖韫囨洜纾兼俊顖濐嚙椤庢捇姊洪幆褏绠抽柟铏尵缁參鏁撻悩鏂ユ嫼闂侀潻瀵岄崢鎰亹閹烘垹顔戝┑鐘诧工閻楀﹪宕靛澶嬧拺妞ゆ巻鍋撶紒澶嬫尦閺屽宕堕浣哄幐闂佹悶鍎弲娑欑瑜旈弻娑樷攽閸ヨ埖鐣风紓浣虹帛缁嬫捇鍩€椤掑倹鏆╅弸顏嗏偓娑欑箓椤啴濡堕崱妤冧淮濠碘槅鍋呯换鍫ョ嵁閸愵喖鐏抽柡鍌樺劜閻庡姊虹憴鍕姢妞ゆ洦鍙冨畷銏ゆ濞戣鲸瀵岄梺闈涚墕濡稒鏅堕鍛箚妞ゆ劧缍嗗▓婊呪偓瑙勬礃濠㈡﹢锝炲┑鍫熷磯闁绘垶枪缁躲垽姊绘笟鈧褏鎹㈤崼銉ョ９闁哄稁鍘奸悡鏇㈡煙鐎电ǹ啸缁炬崘妫勯湁闁挎繂鐗嗘禍妤呮煙鐎电ǹ小闁绘帒锕ら湁闁绘挸娴烽幗鐘绘煟閹惧瓨绀嬮柡灞炬礃缁绘盯宕归鐓幮戝┑鐐差嚟婵即宕瑰ú顏勎﹂柛鏇ㄥ灱閺佸啴鏌曡箛瀣伄妞ゆ柨锕︾槐鎾存媴娴犲鎽甸柣銏╁灡鐢喖鎮橀幒妤佺厽闁绘ê寮堕幖鎰繆椤栨熬韬柛鈹惧亾濡炪倖甯婇悞锕€鐣风仦鐐弿濠电姴鍟妵婵堚偓瑙勬礃椤ㄥ﹤顫忛懡銈咁棜閻庯綆浜為崝鐢告⒒閸屾瑨鍏岄弸顏堟倵濞戞帗娅嗙紒缁樼⊕缁绘繈宕樿缁犳岸鏌ｆ惔銏⑩姇妞ゎ厼娲ㄥ褔鍩€椤掑嫭鈷戦柟鑲╁仜閸旀﹢鏌涢弬娆炬█妤犵偛绻愮叅妞ゅ繐鎳夐幏濠氭⒑缁嬫寧婀版慨妯稿妽閺呭爼骞囬悧鍫㈠幐闂佸憡渚楅崰姘辩不濡偐纾兼い鏃囧Г椤ュ牏鈧娲樼划宥夊箯閸涘瓨鎯為柣褍鎽滄惔濠囨⒒閸屾瑨鍏岀紒顕呭灡閻忔瑥鈹戦悙鍙夊櫤婵炲弶鐗犻幃鎯х暋閹殿喗娈曢梺鍛婃处閸撴盯宕㈤挊澶嗘斀闁绘劖娼欓悘銉р偓瑙勬处閸撶喎顕ｉ幖浣规櫆闁告挆鍜冪床闂備胶绮敋缁剧虎鍙冮崺銉﹀緞閹邦厾鍘撻梺鑺ッˇ钘壩ｉ幖浣圭厓閻犲洦鐓￠崣鍕殽閻愬弶鍠樻い銏∏归ˇ閬嶆煙鐎电ǹ鍘存慨濠傤煼瀹曟帒顫濋钘変壕鐎瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺呮⒒閸屻倕鐏﹂柡灞诲姂瀵挳鎮欏ù瀣壕闁告縿鍎虫稉宥嗐亜閺嶎偄浠﹂柣鎾卞劦閺岋綁寮撮悙娴嬪亾閸︻厸鍋撳鐐?
    else{
        isreturn=false;
        stmt->typeCheck();
        if(!isreturn && ret != TypeSystem::voidType){//don't have return and not void
            fprintf(stderr, "function  \'%s\'misses return\n",se->toStr().c_str());
        }
        // has return  and wrong return  
        else if(isreturn&&ret!=TypeSystem::voidType)
        {
            if(ret!=retVal)
            {
                //fprintf(stderr, "function \'%s\'has wrong return \n",se->toStr().c_str());
            }
        }

        }

    
    // if (stmt != nullptr)
    //     stmt->typeCheck();
    if(paraStmt != nullptr)
        paraStmt->typeCheck();
}


void BinaryExpr::typeCheck()
{
    // Todo
    /*Type* type1 = expr1->getSymPtr()->getType();
    Type * type2 = expr2->getSymPtr()->getType();
    if (type1 != type2)
    {
        fprintf(stderr, "type %s and %s mismatch \n",
          type1->toStr().c_str(), type2->toStr().c_str());
        if (ERROR_MESSAGE_WRITE_INTO_AST)
        {
            fprintf(yyout, "type %s and %s mismatch \n",
                type1->toStr().c_str(), type2->toStr().c_str());
        }
    }*/

    Type* type1 = expr1->getSymPtr()->getType();
    Type* type2 = expr2->getSymPtr()->getType();

    Type* Prior = type1->getKindValue() > type2->getKindValue() ? type1 : type2;
    Type* later = type1->getKindValue() < type2->getKindValue() ? type1 : type2;


    if (type1->get_range() != type2->get_range())
    {
        fprintf(stderr, "type %s and %s mismatch \n",
            type1->toStr().c_str(), type2->toStr().c_str());
        if (ERROR_MESSAGE_WRITE_INTO_AST)
        {
            fprintf(yyout, "type %s and %s mismatch \n",
                type1->toStr().c_str(), type2->toStr().c_str());
        }
    }
    else
    {
        if (type1->getKindValue() != type2->getKindValue())
        {
            fprintf(stderr, "type %s and %s mismatch , but we convert %s to %s for you \n",
                type1->toStr().c_str(), type2->toStr().c_str(),
                later->toStr().c_str(), Prior->toStr().c_str());
            if (ERROR_MESSAGE_WRITE_INTO_AST)
            {
                fprintf(yyout, "type %s and %s mismatch , but we convert %s to %s for you \n",
                    type1->toStr().c_str(), type2->toStr().c_str(), later->toStr().c_str(), Prior->toStr().c_str());
            }
        }
    }



    if (expr1 != nullptr)
        expr1->typeCheck();
    if (expr2 != nullptr)
        expr2->typeCheck();

}

void Constant::typeCheck()
{
    // Todo
}

void Id::typeCheck()
{
    // Todo


    if (Dimension != nullptr)
        Dimension->typeCheck();
    if (Init != nullptr)
        Init->typeCheck();

}

void IfStmt::typeCheck()
{
    // Todo


    if (cond != nullptr)
        cond->typeCheck();
    if (thenStmt != nullptr)
        thenStmt->typeCheck();

}

void IfElseStmt::typeCheck()
{
    // Todo


    if (cond != nullptr)
        cond->typeCheck();
    if (thenStmt != nullptr)
        thenStmt->typeCheck();
    if (elseStmt != nullptr)
        elseStmt->typeCheck();
}

void CompoundStmt::typeCheck()
{
    // Todo  
    if (stmt != nullptr)
        stmt->typeCheck();
}

void SeqNode::typeCheck()
{
    // Todo
    if (stmt1 != nullptr)
        stmt1->typeCheck();
    if (stmt2 != nullptr)
        stmt2->typeCheck();
}

void DeclStmt::typeCheck()
{
    // Todo
    if (id != nullptr)
        id->typeCheck();
}

void ReturnStmt::typeCheck()
{
    // Todo
    if(retValue)
    {
       isreturn=true;//闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤濠€閬嶅焵椤掑倹鍤€閻庢凹鍙冨畷宕囧鐎ｃ劋姹楅梺鍦劋閸ㄥ綊宕愰悙鐑樺仭婵犲﹤鍟扮粻鑽も偓娈垮枟婵炲﹪寮崘顔肩＜婵炴垶鑹鹃獮鍫熶繆閻愵亜鈧倝宕㈡禒瀣瀭闁割煈鍋嗛々鍙夌節闂堟侗鍎愰柣鎾存礃缁绘盯宕卞Δ鍐唺缂備胶濮撮…鐑藉蓟閿涘嫪娌紒瀣仢閳峰鎮楅崹顐ｇ凡閻庢凹鍣ｉ崺鈧い鎺戯功缁夐潧霉濠婂嫮绠炴い銏＄懇瀹曘劑顢樺☉娆愭澑闂備胶绮崝鏇烆嚕閸洖鐓濋柡鍥╀紳閻熸壋鍫柛鏇ㄥ幘閻撯偓婵犵數鍋炶ぐ鍐倶濠靛鐓橀柟杈鹃檮閸嬫劖銇勯弮鍥т汗濞寸厧娲ら埞鎴︽倷瀹割喗效闂佺儵鏅╅崹鍫曞Υ娓氣偓瀵粙顢橀悙鑼崺婵＄偑鍊栧濠氭惞鎼粹埗褰掑礋椤栨稈鎷绘繛鎾磋壘濞层倖鏅堕鍓х＜濠㈣泛鏈弳顒勬煕閳哄倻娲存鐐差儔閺佸倻鎲撮敐鍡楊伜闂傚倷绀佹竟濠囧磻閸℃稑绐楅柡鍥ュ灩绾惧潡鏌熼柇锕€鏋熼柛鐘冲姍閺岋絽螖閳ь剟鎮ч崱娆戠當闁圭儤姊诲Λ顖炴煟閹伴潧澧い鏇熺矌缁辨帞绱掑Ο鍏煎垱閻庤娲栧畷顒冪亽闁荤姴娲﹂悡锟犳倵濞差亝鈷掑ù锝呮啞閹牓鏌熼崘鑼闁靛洦鍔欏畷姗€顢欓崗澶婁壕闁挎梻鍋撳畷澶愭煏婵犲繒鐣辩痪缁㈠灦濮婅櫣绱掑Ο纰辨殹濠电偛鍚嬮悷锕傚箞閵娿儮妲堟慨妤€妫欑€靛矂姊洪棃娑氬婵☆偅鐟ф禍鎼佹偋閸粎绠氶梺缁樺姌閸╂牠藟婢跺浜滄い鎰╁灮缁犱即鎮￠妶鍡愪簻闊洦鎸搁顐ｃ亜閺傛寧鍠橀柡宀€鍠栧畷妤呮嚃閳哄倹顔冮梻浣虹《閺呮稓鈧凹鍓熼敐鐐剁疀濞戞ê鐎銈嗘椤寮埀顒勬⒒娴ｅ摜鏋冩俊顐㈠铻炴俊銈勮兌椤╁弶绻濇繝鍌滃闁稿﹤鐏氶幈銊ヮ潨閸℃绠虹紓浣藉煐閻擄繝寮婚敐澶嬫櫜闁告侗鍘戒簺婵°倖顨忔禍娆撳础閸愯尙鏆﹂柣鎾崇岸閺€浠嬫煕椤愵偄浜滈柡浣哥埣濮婂宕掑▎鎴М闂佸憡鑹鹃鍡欑矉瀹ュ棛顩烽悗锝庝簽閿涚喎顪冮妶鍡樼５闁稿鎸婚〃銉╂倷閼碱剛顔掗梺鍦帶缂嶅﹤鐣烽悜绛嬫晣闁绘瑥鎳愰梻顖炴⒒閸屾瑨鍏岄弸顏堟煛閸偄澧撮柟顔炬焿椤﹀湱鈧娲樼换鍌烆敇閸忕厧绶為悗锝庝簽瑜板懘姊婚崒娆戣窗闁告挻鐟х划鏃堟偨閸涘﹤浜楅梺缁樻煥閸氬鎮″▎鎾寸厵妞ゆ牕妫楅崯鎶藉春閻愮儤鈷戦柛娑橈攻椤ユ牜绱掗悩铏磳鐎殿喖顭烽崹鎯х暦閸ャ劍鐣烽梻渚€鈧偛鑻晶瀵糕偓瑙勬磻閸楁娊鐛Ο鍏煎珰闁肩⒈鍓﹂崬鐢告⒒娴ｈ櫣甯涢惇澶愭偣閳ь剟鏁冮埀顒€宓勯梺鍛婃寙閸涱垽绱查梻浣虹帛閸旓箓宕滃▎鎴犵焼闁告劏鏅濈粻鎯ь熆鐠轰警鍎愮紒鈧崼鐔剁箚闁告瑥顦慨宥嗩殽閻愭潙绗掗摶鏍р攽閻樻彃鏆炴繛鍛尵缁辨挻鎷呴崫鍕婵犳鍨垫慨銈囩矉瀹ュ鏁嗛柛鏇ㄥ亯琚濋梺璇插嚱缂嶅棝鍩€椤掑骞楁俊顐㈠暣楠炲啴鍩￠崨顕呮濠电偞鍨堕〃鍡涘礋閸愵喗鈷戞慨鐟版搐閻掓椽鏌涢妸銊ゅ惈闁轰緡鍣ｉ幃銏ゅ礂閻撳簶鍋撻崼鏇熺厱闁逛即娼ч弸鐔兼煃闁垮鐏存慨濠冩そ椤㈡洟濡堕崨顒傛崟闂備礁鍚嬪鍧楀垂闁秴鐤鹃柤鎼佹涧椤曢亶鎮楀☉娆樼劷闁告ü绮欏娲濞戣鲸鈻撻梺鍝ュУ濞叉牠婀侀梻浣哥仢椤戝啯绂嶅⿰鍫熺叆闁哄洦顨呮禍鍓х磽閸屾氨孝闁挎洦浜滈锝嗙節濮橆厽娅栭梺鍛婃处娴滄繈宕熼崘顔解拺濞村吋鐟ч崚浼存煟椤愶綆娈欐い锔芥尦閺屽秷顧侀柛鎾寸箞閿濈偞寰勬繛鎺撴そ閺佸啴宕掑鎲嬬幢濠电姷鏁告慨鎾磹婵犳艾姹查柨鏇炲€归悡鐔兼煛閸愩劌鈧敻骞忛敓鐘崇厸濞达絿枪閺嗭綁鏌″畝瀣М鐎殿噮鍣ｅ畷鍫曞Ω閵娿儲鍊曟繝鐢靛О閸ㄥジ锝炴径濞掓椽鎮㈤悡搴ｇ暫閻庣懓瀚竟瀣醇椤忓牊鐓曟繛鎴濆船瀵箖鏌￠崱鈺佸籍闁哄矉缍佹慨鈧柍杞拌兌娴狀厼顪冮妶鍐ㄥ闁挎洏鍊濋幃楣冩倻閽樺顓洪梺鎸庢磵閸嬫挾鐥幆褋鍋㈤柡宀嬬秮楠炲洭顢楁径濠冾啀闂備礁鎲￠敃鈺傜濠靛牊宕叉繝闈涱儏椤懘鏌ㄥ┑鍡樺櫧闁告﹩鍋嗙槐鎾存媴閸撴彃鍓遍梺鎼炲妼濞尖剝淇婇幘顔煎窛闁圭⒈鍘介弲銏ゆ⒑缁嬫寧婀扮紒顔奸叄閺佸秴饪伴崼鐔叉嫼缂傚倷鐒﹂妴鎺楁偡閹殿喗娈鹃梺姹囧灩閹诧繝宕戦埡鍌滅鐎瑰壊鍠曠花濂告煟閹惧娲撮柟顔斤耿閹瑩骞撻幒鍡樺瘱闂備礁鐤囧Λ鍕囬悽绋胯摕鐎广儱娲︾€氭岸鏌涘▎蹇ｆЦ闁衡偓椤撶姷纾藉ù锝呮惈鏍￠梺鐑╂櫓閸ㄨ泛顕ｉ锕€绠涢柡澶庢硶閻涖儵姊洪悡搴☆棌闁告挻鐟︽穱濠囨偩瀹€鈧壕鑲╃磽娴ｈ鐒界紒鐘靛仱閺屾盯鍩￠崒婊勫垱闂佺硶鏂侀崑鎾愁渻閵堝棗绗掗悗姘煎墮椤斿繐鈹戦崶銉ょ盎闂佸搫鍟ú銈堫暱闂備椒绱梽鍕箲閸パ屾綎缂備焦蓱婵挳鏌涘☉姗堟敾闁稿孩鎹囧鍝劽虹拠鎻掝潻闂侀潧鐗忔灙闁伙綁鏀辩€靛ジ寮堕幋婵嗘暏婵＄偑鍊栭幐缁樼珶閺囥垹纾婚柟鎯х摠婵绱掗娑欑妞ゎ偄绉撮埞鎴︻敊缁涘鐣跺┑鈽嗗亝閻熲晛鐣烽幘缁橆棃婵炴垼椴哥€靛矂姊洪棃娑氬婵☆偅顨堢划顓㈠磼閻愬鍘介梺纭呮閸嬬喖鎮鹃崹顐犱簻闁靛骏绱曢幊鍥殽閻愭潙绗掗摶鏍归敐鍛儓妤犵偞顨婂娲偡閺夋寧鍊梺浼欑秵娴滎亜鐣烽幇鏉课ч柛婊€鑳堕崝鐑芥⒑瑜版帒浜伴柛妯哄⒔缁粯銈ｉ崘鈺冨幈濠电偛妫楀ù姘ｅú顏呯厽妞ゆ挾鍠庣粭褔鏌嶈閸撴繈锝炴径濞掓椽骞嬮敃鈧涵鈧梺鍛婂姇濡﹪宕甸弴鐏诲綊鎮╁顔煎壘闂佽桨绀佸ú锔炬崲濠靛⿴妲奸梺绋挎唉濞呮洜绮嬪澶婄畾鐟滃鍩€椤掍礁绗掓い顐ｇ箞閺佹劙宕ㄩ鈧ˉ姘舵⒑鐠囨彃顒㈡い鏃€鐗犲畷鎶筋敋閳ь剙鐣烽幎鑺ユ櫜闁告侗鍨卞▓楣冩⒑缂佹ɑ灏紒銊ョ埣瀵劍绂掔€ｎ偆鍘遍梺鏂ユ櫅閸燁偊鎮炶ぐ鎺撶厱閻庯綆鍋呭畷宀€鈧娲滄灙閾绘牕霉閿濆懎绾ч悗姘偢濮婅櫣鎷犻弻銉偓妤佺節閳ь剚娼忛妸锕€寮块梺姹囧灮鏋潻婵嬫⒑閸涘﹤濮€闁哄懏鐟ч幑銏ゅ幢濞戞瑧鍘介梺褰掑亰閸樼晫绱為幋鐘电＜婵＄儑绠戦幊鎰婵傜ǹ绾ч柛顐ｇ☉婵＄晫鈧娲栭張顒勩€冮妷鈺傚€烽柟纰卞幘閸旈绱撴担浠嬪摵閻㈩垱甯熼悘鎺楁⒒娴ｅ摜浠㈡い鎴濇嚇椤㈡柨煤椤忓應鎷婚梺绋挎湰閻熴劑宕楀畝鍕嚑妞ゅ繐鐗婇悡娑㈡煕椤愵偄浜滃褎鎸抽弻鐔碱敊閹冨箣闂佽桨绀侀崐鍨暦濠婂棭妲鹃梺缁樺浮缁犳牕顫忛搹鍦＜婵妫欓悾鍫曟⒑缂佹﹩娈旂紒缁樺笧閸掓帗绻濆顒傜暰閻熸粍绮撻幃銏ゅ幢濞戞瑥浠柡澶屽仦婵粙顢楅悢鍏肩厱閹兼番鍨规慨澶岀磼鏉堛劌娴柛鈹惧亾濡炪倖甯掗崐鐢稿磻閹剧粯鏅查幖绮光偓鍐茬闂備胶枪椤戝棝骞愭ィ鍐ㄧ疅闁圭虎鍠栫粈瀣亜閹烘垵浜炴俊鍙夋緲閳规垿鎮╅崹顐ｆ瘎闂佺ǹ顑嗛惄顖炲极閸愵噮鏁傞柛鏇㈡涧濞堛劌顪冮妶鍡楀Ё缂佺姵鍨圭划鍫ュ礃閳瑰じ绨婚梺鍝勫暙濞层倖绂嶈ぐ鎺撶叆婵炴垶鐟ч惌濠囨煃鐟欏嫬鐏存い銏＄懇瀵剟濡堕崼姘壕閻犳亽鍔庡Λ顖涖亜閹捐泛鏋戦柛濠冨姉閳ь剝顫夊ú锕傚礈閻旂厧绠栧ù鐘差儏瀹告繂鈹戦悙闈涗壕閻庢俺妫勯埞鎴︽倷閼搁潧娑х紓鍌氱М閸嬫挻绻濆▓鍨灁闁稿﹥顨呴埥澶愭偨缁嬪潡鍞堕梺鍝勬川閸犳捇宕㈤柆宥嗏拺闁圭ǹ娴风粻鎾剁磼缂佹ê娴柟顕€绠栧畷褰掝敃椤愶綆鍟嶉梻浣虹帛閸旀浜稿▎鎴犱笉濠电姵纰嶉悡娑樏归敐鍫綈鐎规洖鐭傞弻鈩冩媴鐟欏嫬纾抽梺杞扮劍閹瑰洭寮幘缁樻櫢闁跨噦鎷�?
        retVal=retValue->getSymPtr()->getType(); //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鎯у⒔閹虫捇鈥旈崘顏佸亾閿濆簼绨奸柟鐧哥秮閺岋綁顢橀悙鎼闂侀潧妫欑敮鎺楋綖濠靛鏅查柛娑卞墮椤ユ艾鈹戞幊閸婃鎱ㄩ悜钘夌；闁绘劗鍎ら崑瀣煟濡崵婀介柍褜鍏涚欢姘嚕閺夋埈娼╅弶鍫氭暕閵忋倖鈷掑ù锝堫潐閸嬬娀鏌涙惔銏°仢鐎规洘绮撻弫鍐磼濮橆厾鈧剟姊洪崨濠傚Е闁哥姵顨婇幃锟犲Ψ閳哄倻鍘搁梺鎼炲労閻撳牆鈻撻弬妫电懓饪伴崼銏㈡毇濠殿喖锕ら幖顐ｆ櫏闂佹悶鍎滈埀顒勫磻閹炬緞鏃堝川椤撶媴绱遍梻浣筋潐瀹曟﹢宕洪弽褏鏆﹂柛娆忣槺缁♀偓闂傚倸鐗婄粙鎺戭啅濠靛牏纾奸柍閿亾闁稿鎹囧缁樻媴閸涢潧缍婂鐢割敆閸屾艾鐏婇悗鍏夊亾闁告洍鏂侀崑鎾诲磼濮ｎ厽妞介、鏃堝川椤忓懎顏归梻鍌欐祰婵倝鏁嬪銈忓瘜閸ㄨ泛顕ｉ幎鑺ュ亜闁稿繗鍋愰崢閬嶆⒑缂佹◤顏嗗椤撶喐娅犻柣銏㈩暯閸嬫挸鈻撻崹顔界亪濡炪値鍘奸崲鏌ヮ敋閿濆绠绘い鏃囨娴滄粓姊洪幆褏绠烘い鏇熺墵瀹曨垱鎯旈妸锔规嫽婵炶揪绲挎灙妞ゃ儱绻橀弻锝夊箻瀹曞洤鈪甸悗瑙勬磻閸楀啿顕ｆ禒瀣╃憸蹇氥亹妤ｅ啯鈷戦柛娑橈攻婢跺嫰鏌涚€Ｑ冧壕闂備浇銆€閸嬫挸霉閻樺樊鍎愰柣鎾跺枛閺岀喖鏌囬敃鈧晶顔剧磼閻欐瑥娲﹂悡鍐磽娴ｈ偂鎴犵矆閳ь剟姊婚崶褜妲圭紒缁樼箖缁绘繈宕掑闂寸磻闂備焦妞块崢鐣屾暜閻愬搫鐒垫い鎺戝枤濞兼劖绻涢崣澶涜€块柡浣稿暣婵偓闁靛牆鎳忓Σ顒勬⒑闁偛鑻晶顖炴煏閸パ冾伃妤犵偞甯″畷鍗烆渻閹屾婵犵绱曢崑鎴﹀磹閵堝棛顩叉繝濠傜墕閻ゎ噣鎮楀☉娅偐鎹㈤崱娑欑厱妞ゆ劧绲剧粈鈧Δ鐘靛亼閸ㄧ儤绌辨繝鍥舵晬婵犲﹤鍟俊鍝勨攽閻愯尙澧遍柛瀣工椤繒绱掑Ο璇差€撻梺鍛婄洴濞佳呯礊婵犲洢鈧線寮介鐐靛€炲銈嗗坊閸嬫挾绱掗幇顓ф疁闁哄备鈧剚鍚嬮幖绮光偓宕囶啇闂備礁鎼鍕嚄閸洖鐒垫い鎺嗗亾缂佺姴绉瑰畷鏇㈡焼瀹ュ懐鐤囨繝銏ｅ煐閸旀洟宕欓悩璇茬婵烇綆鍓欐俊浠嬫煟閹惧鎳囬柡灞剧洴楠炴ê螖閳ь剟寮婚崨顔肩窞闁归偊鍘鹃崢鐢告⒒娴ｅ摜浠㈡い鎴濇婵￠潧鈹戦崶銊ュ伎婵犵數濮撮幊蹇涱敂閻樼數纾兼い鏃傛櫕閹冲懘鏌熼悷鏉款伃濠碘剝鐡曢¨渚€鏌涢悢椋庡ⅵ婵﹦绮幏鍛村川闂堟稓绉烘い銏＄墵瀹曞崬鈽夊Ο纰辨Х婵犵數鍋涘Λ娆撳礉閹烘梹宕查柛鈩冪⊕閻撴洘銇勯幇鍓佺ɑ缂佲偓閳ь剛绱撴担鍝勑ョ紒顕呭灦婵＄敻宕熼姘辩潉闂佺ǹ鏈粙鎺楁偟椤忓牊鈷戦柛娑橆焵閹达箑绠栭柛灞惧嚬濞兼牗绻涘顔荤盎鐎瑰憡绻傞埞鎴︽偐鏉堫偄鍘＄紓浣筋唺缁舵艾顫忓ú顏勭闁绘劖褰冮‖瀣節閳封偓閸曞灚鐣肩紓渚囧枛椤戝鐛幒鎳虫梹鎷呴崫鍕﹂梺璇查缁犲秹宕曢崡鐐嶆稑鈽夐姀鐘电厬闂佺硶鍓濋妵娑㈠绩娴犲鐓熸俊顖濆亹鐢稒绻涢幊宄版处閻撶喐淇婇妶鍌氫壕濠碘槅鍋呴悷褏鍒掔€ｎ喖绠虫俊銈勮兌閸斿爼鎮楅獮鍨姎婵☆偅鐩畷銏ゅ川婵犲嫮顔曢柡澶婄墕婢т粙骞冩總绋垮嚑妞ゅ繐鐗婇悡娑㈡煕閳藉棗骞橀柟顔笺偢閺屽秷顧侀柛鎾寸箞閿濈偞寰勬繛鎺戞惈铻ｉ柛蹇撳悑濞堜即姊洪崷顓炲妺妞ゃ劌鎳橀幃锟犲灳閹颁胶鍞甸柣鐘荤細閵嗏偓闁活亜顑嗙换娑氫焊閺嶃劍鐝曢梺闈涙搐鐎氫即鐛幒妤€骞㈡俊鐐村劤椤ユ艾鈹戦悩鎰佸晱闁哥姵鐩敐鐐村緞閹邦厼浜楅梺鐐藉劜閸撴艾顭囬弽褉鏀介柣妯虹枃婢规鐥幆褍鎮戠紒缁樼洴瀹曞崬螣閾忓湱鎳嗛梻浣告啞閿曨偆妲愰弴鐘愁潟闁规儳顕悷褰掓煕閵夋垵瀚ぐ顖炴⒒娴ｈ鍋犻柛鏂跨У缁旂喖宕卞☉妯肩厬闂佸憡绋戦悺銊╁煕閹达附鐓曟繝闈涙椤忣偊鏌嶇拠宸█闁哄本绋戣灒闁割煈鍠曞Ч妤佺節閵忥綆娼愭繛鍙夌箞婵＄敻骞囬弶璺唺闂佽宕樺▔娑㈢嵁閸儲鈷掑ù锝囨嚀椤曟粍淇婇锛勭獢妞ゃ垺淇洪ˇ褰掓煟濞戝崬娅嶆鐐叉喘閹囧醇閵忕姴绠為梻浣筋嚙閸戠晫绱為崱娑樼；闁告侗鍨悞濠冦亜閹捐泛鏋傚ù婊勭矋閵囧嫰骞囬崜浣瑰仹缂備胶濮烽崑銈夊蓟閻旂⒈鏁婇柣锝呮湰閸ｆ澘顪冮妶鍐ㄧ仾妞ゃ劌锕畷娲焵椤掍降浜滈柟鐑樺灥椤忔挳鏌℃担绋库偓鍧楀蓟閵娾晜鍋嗛柛灞剧☉椤忥拷?
    }


    if (retValue != nullptr)
        retValue->typeCheck();

    if (retValue)
    {

        Type* fuck_temp = retValue->getSymPtr()->getType();
        if (retValue->getSymPtr()->getType()->isFunc()) {
            //std::cout << fuck_temp->toStr();
            fuck_temp = ((FunctionType*)retValue->getSymPtr()->getType())->getRetType();
        }
        if (((FunctionType*)temp_funct->getSymbolEntry()->getType())->getRetType() != fuck_temp)
        {
            fprintf(stderr, "return error, \'%s\'and \'%s\' miss match \n", temp_funct->getSymbolEntry()->getType()->toStr().c_str(), fuck_temp->toStr().c_str());
        }
    }

}

void AssignStmt::typeCheck()
{
    // Todo
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崰姘熼埀顒勬倵濞堝灝鏋熼柟顔煎€搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛鐏炵硶鍋撳畷鍥ㄦ畷闂侀€炲苯澧寸€规洑鍗冲浠嬵敇閻愮數鏆柣鐔哥矋閺屻劑鎮鹃柨瀣檮闁告稑锕ゆ禍婊堟⒑閸涘﹦绠撻悗姘煎墮閳绘捇濡烽埡鍌楁嫽闂佺ǹ鏈悷褔藝閿曞倹鐓欓悹鍥囧懐鐦堥悗瑙勬礀濠€閬嶅箟閹绢喖绀嬫い鎺嗗亾鐎殿喖娼″娲捶椤撯剝顎楅梺鍝ュУ閻楃娀骞冮垾鎰佹建闁逞屽墴瀵鎮㈤崨濠勭Ф婵°倧绲介崯顖烆敁瀹ュ鈷戠紒瀣儥閸庢劙鏌涢弮鈧悷鈺呯嵁閸愩剮鏃堝川椤撶喎绁舵俊鐐€栭幐楣冨磹椤愶箑顫呴幒铏濠婂牊鐓忓鑸电☉椤╊剛绱掗悩鎰佺劷缂佽鲸甯楀蹇涘Ω閿曗偓闂夊秹姊洪悷鏉挎Щ闁硅櫕锚閻ｇ兘顢曢敃鈧粈瀣棯閻楀煫顏呯閸撗€鍋撻崗澶婁壕闂佸憡娲﹂崜娑㈠储闁秵鈷戦梻鍫熻儐瑜版帒纾块柟鎯版閺勩儵鏌ㄥ┑鍡樼闁稿鎸搁埢鎾诲垂椤旂晫浜俊鐐€ら崢楣冨礂濡警鍤曟い鎰剁畱缁犳稒銇勯幘璺轰户缂佹劗鍋炵换婵嬫偨闂堟刀銏ゆ倵濮樺崬鍘寸€规洏鍎靛畷銊р偓娑櫱氶幏缁樼箾鏉堝墽鎮奸柟铏崌椤㈡艾饪伴崨顖滐紲闁荤姴娲﹁ぐ鍐焵椤掆偓濞硷繝鎮伴钘夌窞濠电偟鍋撻～宥夋⒑闂堟稓绠冲┑顔惧厴椤㈡瑩骞掗弮鍌滐紳闂佺ǹ鏈悷褔宕濆鍡愪簻妞ゆ挾鍋為崰妯尖偓瑙勬磸閸ㄤ粙鐛弽銊﹀闁稿繐顦扮€氬ジ姊绘担鍛婂暈缂佸鍨块弫鍐晲閸ヮ煈鍋ㄩ梻渚囧墮缁夌敻鎮￠弴銏＄厪闁割偅绻冮ˉ婊勩亜閵夈儺鍎愮紒缁樼洴楠炲鎮╅搹顐ｇ槗闁诲氦顫夊ú蹇涘礉閹达负鈧礁鈽夊Ο閿嬬€婚梺纭呮彧缁蹭粙宕㈤鐔虹瘈闁汇垽娼у暩闂佽桨绀侀幉锟犲箞閵娾晛绠绘い鏃囨閸擄附绻濋姀锝呯厫闁告梹娲滈惀顏囶樄妤犵偞鐗滈埀顒佺⊕钃遍悘蹇ｅ幖闇夋繝濠傜墢閻ｆ椽鏌″畝瀣瘈鐎规洖鐖奸崺鈩冩媴閸ャ劎娲撮柡灞剧〒閳ь剨缍嗛崑鍛焊椤撶喆浜滄い鎰剁悼缁犵偤鏌熼鐟板⒉闁诡垱妫冮弫宥夊礋椤撶喓绉┑鐘殿暜缁辨洟宕戦幋锕€纾归柟杈剧畱閸ㄥ倿鏌涘┑鍡楃弸婵炴垯鍨圭粈鍐┿亜閺冨洤浜归柣锝嗘そ濮婃椽宕橀崣澶嬪創闂佸摜鍠愭竟鍡樼珶閺囥垹绀傞梻鍌氼嚟缁犳艾顪冮妶鍡欏缂侇喖鐬奸弫顕€宕奸弴鐔蜂缓濡炪倖鐗滈崑鐔兼偩閻戞ɑ鍙忓┑鐘插鐢盯鏌熷畡鐗堝殗闁圭厧缍婇悰顔芥償閹惧厖澹曟繝鐢靛Т濞诧箓鎮￠弴銏＄厵闁诡垎灞芥缂備胶濮烽崑銈夊蓟閺囥垹骞㈡俊銈傚亾闁哄棎鍨荤槐鎺楁偐瀹曞洠妲堥梺瀹犳椤︻垵鐏掔紒缁㈠弮椤ユ捇宕ヨぐ鎺撯拻闁稿本鐟чˇ锕傛煙绾板崬浜扮€规洦鍨堕幃鈺冩嫚閼碱剨绱遍梻浣告贡閸嬫捇鎮鹃鍛洸婵犲﹤鐗婇悡蹇撯攽閻樿尙绠抽柛鎺嶅嵆閺屾盯寮幘鍓佹殼闂佸搫鐬奸崰鏍€佸▎鎾村仼閻忕偞鍎冲▍姘舵⒒娴ｇ儤鍤€缁剧虎鍘界换娑㈠焵椤掍降浜滈柕蹇婃濞堟粎鈧娲樼敮鈩冧繆閸洖宸濇い鏃傛櫕瑜帮綁姊婚崒姘偓鎼佸磹閹间礁纾瑰瀣捣閻棗銆掑锝呬壕濡ょ姷鍋為悧鐘汇€侀弴銏犖ч柛鈩冦仦缁剝淇婇悙顏勨偓鏍礉瑜忕划濠氬箣閻樼數鐒兼繝銏ｅ煐閸旀洜绮荤憴鍕闁挎繂楠告晶顔尖攽閳ョ偨鍋㈤柡宀嬬秮楠炴帡骞嬮悩杈╅┏闂備線娼уú銈団偓姘嵆閻涱噣骞掑Δ鈧粻锝嗐亜閺嶃劏澹樻い顐ゅХ缁辨捇宕掑▎鎰垫▊闂佺厧鍟块悥鐓庣暦濡も偓椤粓鍩€椤掆偓椤曪絿鎷犲ù瀣潔闂侀潧绻掓慨鐢杆夊┑瀣拺闁革富鍘愰崷顓涘亾濞戞帗娅婃鐐茬箲濞煎繘鍩￠崘顏庣闯濠电偠鎻徊鎸庣仚闂侀€炲苯澧柟鑺ョ矌閸掓帡寮崼鐔蜂缓缂佸墽澧楅敋濞存粍鍎抽湁闁挎繂鎳庨弳娆徝瑰⿰鍐Ш闁哄苯绉烽¨渚€鏌涢幘瀛樼殤缂侇喗鐟╅獮鎺戭渻閻戔晛浜鹃柨鏇炲€搁悙濠冦亜閹哄棗浜鹃梺绋胯閸旀垿寮诲☉妯锋婵鐗嗘慨娑氱磽娴ｅ弶顎嗛柛瀣崌濮婄粯绗熼埀顒€顭囪椤ㄣ儴绠涢弴鐐电瓘闂佸憡鎸嗛崟顐㈠箲濠德板€х徊浠嬪疮椤栫偞鍋傞柕澶嗘櫆閻撴盯鏌涢妷锔芥瀯闂侇収鍨辨穱濠勭磼閵忕姵鐝濋梺鍝勭焿缁绘繂鐣烽妸鈺婃晣闁绘ǹ灏欓崢钘夆攽閻樻剚鍟忛柛鐘冲浮瀹曟垿骞樼紒妯锋嫼闂傚倸鐗婄粙鎾存櫠閺囩喍绻嗘い鎰剁悼濞插鈧娲忛崹浠嬪蓟閸℃鍚嬮柛鈩冪懃楠炴姊洪悷鏉挎倯闁诡垰鐭傚畷鐟扮暦閸モ晝鐓嬮梺姹囧灩閹诧繝鎮￠弴銏″€甸柨婵嗙凹缁ㄥジ鏌涢敐鍫燁棄闂囧绻濇繝鍌氼仾闁稿﹥鍔欓弻鐔碱敊閼姐倗鐓撻悗瑙勬磸閸斿秶鎹㈠┑瀣妞ゆ劑鍨婚悷鏌ユ⒒娴ｈ棄鍚归柛鐘冲姉閸掓帒顓奸崶褍鐏婇梺瑙勫劤绾绢參寮抽敂鐣岀瘈濠电姴鍊搁弸銈嗙箾鐏忔牗娅婇柡灞诲€濋獮鏍ㄦ媴鐟欏嫰鏁繝鐢靛仜閻楀繘宕戦妶鍜佹綎闁惧繗顫夌€氭岸鏌嶉妷銉э紞闁逞屽墲瀹曠敻鍩€椤掆偓缁犲秹宕曢柆宥嗗亱闁告劦鍠栫壕濠氭煙閹规劦鍤欓梺瑁ゅ€栨穱濠囧Χ閸曨噮鍞归梺鐟板槻閹碱偊鈥旈崘顔嘉ч柛鈩冾殔濞兼垿姊虹粙娆惧剱闁圭ǹ澧藉Σ鎰板箳閹惧磭绐炴繝鐢靛Т妤犵ǹ鈻撻崜浣虹＝濞撴艾娲ら弸娑欍亜椤撶姴鍘撮柕鍡曠閳藉螣瀹勬壆澧梻浣告啞濞诧箓宕戦崨瀛樺€靛Δ锝呭暞閳锋帒霉閿濆牊顏犻悽顖涚⊕閵囧嫰濡搁妷锕€娈楅梺璇″暙閸パ冭€垮┑鈽嗗灠閻ㄧ兘寮ㄩ崘宸富闁靛牆妫涙晶閬嶆煕鐎ｎ剙浠遍柟顔光偓鏂ユ婵☆垰绻愮紞濠囧极閹版澘鍚归柦妯侯槺缁夎櫣鈧鍠楁繛濠囧极閹邦厼绶為悗锝庡墮楠炴劙姊虹拠鑼闁稿濞€椤㈡俺顦抽柟渚垮姂瀹曟帒鈹冮幆褌澹曞┑鐐茬墕閻忔繂鈻嶅鈧弻娑㈡晲韫囨洖鍩岄梺浼欑悼閸忔ê鐣锋總绋垮嵆婵＄偠顕ф禍楣冩煛瀹ュ骸寮鹃柡浣革躬閺屻倝骞侀幒鎴濆闂傚倸顦粔鍓佹閹惧瓨濯撮柧蹇曟嚀缁楋繝姊洪悜鈺傛珦闁搞劋鍗抽幃楣冩倻閽樺）鈺呮煃閸濆嫸鏀婚柡鍛箞濮婃椽妫冨☉娆戭洶濠电姭鎳囬崑鎾剁磽娴ｄ粙鍝洪柣鐕傚閹广垹鈹戦崱蹇旂亖闂佸壊鐓堥崰妤呮倶閸儲鈷戦柟绋垮绾炬悂鏌涙惔銊ゆ喚闁炽儲妫冨畷姗€顢欓挊澶屾濠电姰鍨煎▔娑㈠嫉椤掆偓閳绘捇骞嗚濞撳鏌曢崼婵囶棞濠殿啫鍛＜闁绘ê鍟块悘鈺冪磼椤旇偐澧㈤柍褜鍓ㄧ紞鍡涘磻娴ｅ湱顩叉繝濠傜墛閻撴瑩姊洪銊х暠闁哄鐩弻锛勨偓锝庝邯椤庢妫佹径瀣瘈濠电姴鍊绘晶銏ゆ煟閿濆棙銇濋柡宀嬬磿娴狅箓宕滆閸掓稑顪冮妶鍐ㄧ仾婵炶尙鍠栧顐﹀箛閺夊灝鑰垮┑鐐叉钃辩悮锝囩磽閸屾艾鈧兘鎳楅崼鏇炵；闁规崘顕х壕璺ㄢ偓瑙勬礀濞层劎绮堟繝鍌楁斀闁绘ê寮堕幖鎰版煢閸愵亜鏋涢柡灞界Ч婵＄兘鏁冮埀顒佹櫠椤栫偞鐓曟慨姗嗗墻閸庢梹顨ラ悙瀵稿⒈闁告帗甯″畷妤佸緞婵犱礁顥氶梻浣藉亹閳峰牓宕滃☉銏犳槬鐎广儱顦伴埛鎺楁煕鐏炲墽鎳嗛柛蹇撶焸瀵悂顢旈崨顐＄盎闂佺懓鐡ㄧ换鍐夐姀鈽嗘闁绘劖娼欐慨宥嗩殽閻愬澧紒妤冨枑缁绘繈宕熼娑欙紙婵犵數濮烽弫鍛婄箾閳ь剚绻涙担鍐叉祩閺佸嫰鏌熼悧鍫熺凡闁哄绶氶弻娑㈠焺閸愵亖濮囬梺缁樻尰濞茬喖骞冨鈧幃娆撳箵閹哄棙瀵栭梻浣芥〃閻掞箓鎮ч悩璇茶摕闁哄洨鍠撶粻楣冩煟閹伴潧澧柣婵囨礋濮婃椽骞庨懞銉︽殸闂佹悶鍔屽鈥愁嚕鐠囨祴妲堟俊顖炴敱閺傗偓闂備胶纭堕崜婵嬨€冭箛鏂款嚤闁稿本鍑瑰〒濠氭煏閸繈顎楀ù婊冨⒔缁辨帡鎮╅搹顐㈢３閻庢鍠涢褔鍩ユ径鎰潊闁绘鏁稿澶愭⒒娓氣偓閳ь剛鍋涢懟顖涙櫠鐎电硶鍋撶憴鍕缂傚秴锕ら悾椋庣矙鐠囩偓妫冨畷姗€濡歌閸嬪﹪姊婚崒娆戭槮闁硅绻濆畷婵嬫晝閸屾氨鐛ラ梺鍝勭▉閸欏繒绱為弽顓熺厱婵炴垶顭囬幗鐘绘煟閹惧磭绠婚柡灞剧洴椤㈡洟鏁愰崶鈺冩毇闂備線娼荤徊钘夛耿鏉堚晜顫曢柟鐑橆殔缁犳稒銇勯幘璺盒涘┑顔兼川缁辨挻鎷呴搹鐟扮缂備浇顕ч悧鎾荤嵁閸愨晝顩烽悗锝庡亽濡啫鈹戦悙娈挎缂傚牅鍗冲畷瑙勭鐎ｎ亞鐣哄┑掳鍊愰崑鎾淬亜椤愶絿绠炴慨濠呭吹閳ь剟娼ч幉锟犲疾閸忚偐绡€缁剧増菤閸嬫捇宕橀懠顒勭崜闂備礁鎲″褰掓偡閵夆晩鏁嬮柨婵嗩槸缁犵粯銇勯弮鍌楁嫛婵炵厧锕娲箮閼恒儲娈伴梺绋款儐閹瑰洭寮婚悢鍓叉Ч閹艰揪绲界粭锛勭磽娴ｈ櫣甯涚紒瀣笒椤洩绠涘☉妯碱槶閻熸粌绉瑰畷鍝勭暆閸曨兘鎷虹紒缁㈠幖閹冲氦顣跨紓鍌欑贰閸犳牕霉閻戣棄绀嗛柟鐑橆殔缁秹鏌涢銈呮瀻闁逞屽墰缁垱绌辨繝鍥舵晬闁绘劕鐡ㄩ悵宕囩磽娴ｉ潧濡兼い顓炲槻椤繐煤椤忓拋妫冨┑鐐村灱妞存悂顢撻崱娑欌拻闁稿本鑹鹃鈺呮倵濮樼厧澧撮柟顔藉劤閻ｏ繝骞嶉鑺ョ暦闂備線鈧偛鑻晶鎾煕閳规儳浜炬俊鐐€栫敮鎺楀窗濮橆剦鐒介柟閭﹀幘缁犻箖鏌涘▎蹇ｆ闁兼澘娼￠弻鏇㈠幢濡も偓閺嗭絿鈧娲忛崝鎴︾嵁瀹ュ鏁婇柡鍕箰閻掑姊婚崒姘偓鎼佸磹閻戣姤鍤勯柛顐ｆ礀閸屻劎鎲搁弮鍫熸櫜闁绘劕鎼崡鎶芥煏韫囨洖啸闁挎稒鐟╁娲濞戞艾顣洪梺璇″枦閸庡濡甸幇鏉跨闁瑰濯Σ瑙勪繆閻愵亜鈧牠宕濋敃鍌氱；闁告侗鍨崑鎾绘濞戞氨鍔┑顔硷攻濡炶棄螞閸愩劉妲堟慨姗嗗墻閺嗩偅绻濈喊妯活潑闁稿鎳愮划娆撳箳濡も偓閻ら箖鏌涢锝嗙闁绘帟鍋愰埀顒€绠嶉崕鍗炍涘☉銏犲偍闂侇剙绉甸悡鐔煎箹閹碱厼鐏ｇ紒澶屾暬閺屾盯骞樼€靛憡鍣紓浣稿€圭敮锟犲蓟閸℃鍚嬮柛鈩冪懃楠炲秵淇婇悙顏勨偓鏍洪弽顬稑鈽夊顒€袣闂侀€炲苯澧紒缁樼箘閸犲﹥寰勫畝鈧敍鐔兼⒑缁嬭法绠查柨鏇樺灩閻ｅ嘲煤椤忓懏娅㈤梺缁樓圭亸娆撴晬濠婂啠鏀介柣妯款嚋瀹搞儵鏌涢悤浣镐簽缂侇喛顕х叅妞ゅ繐鎳夐幏濠氭⒑缁嬫寧婀伴柣鐔濆泚鍥晝閸屾稓鍘电紒鐐緲瀹曨剚绂嶉幍顔瑰亾鐟欏嫭绀冮柛鏃€鐟ラ悾鐑芥倻缁涘鏅ｅ┑鐐村灦鐪夊瑙勬礈缁辨挻鎷呯拠鈩冪暦缂備礁顑嗛崹鐢靛弲闂侀潧鐗嗛ˇ浼村磻椤忓懌浜滈柡鍥殔娴滈箖姊洪崫鍕効缂傚秳绶氶悰顕€宕堕澶嬫櫓闂佺粯鎸告鎼佲€栭崱娑欌拻濞达綀娅ｇ敮娑㈡煙閹间胶鐣虹€规洑鍗冲浠嬵敇閻旇渹绨垫俊鐐€栭崝褏绮婚幋锔藉€峰┑鐘插亞濞撳鎮楅敐搴濋偗妞ゅ孩顨婇弻娑㈠煛娴ｈ棄顏梺瀹狀潐閸ㄥ潡骞冨▎鎾崇煑濠㈣泛锕ラ鐘电磽閸屾瑧顦︽い锔诲灣缁辩偞绻濋崒銈呮婵犵數濮甸懝楣冩倶瀹曞洠鍋撶憴鍕婵炶绠戦埢鎾诲即閵忊檧鎷洪梻渚囧亞閸嬫盯鎳熼娑欐珷閻庣數纭堕崑鎾舵喆閸曨剛锛涢梺鍛婎殕婵炲﹪鎮伴鈧畷鍫曨敆閳ь剛绮堥崒娑栦簻闁规壋鏅涢悘顏堟煕閻樼鑰块柡宀€鍠栭、娆撳Ω閵夛附鎮欓梻浣稿船濞差參寮婚敐澶婃闁割煈鍠椾簺濠电姷顣介崜婵囩箾婵犲偆娼栫紓浣股戞刊鎾煕濠靛棗顏х紓鍌涙崌濮婅櫣绮欓崸妤€寮版繛瀛樼矎濞夋盯鎮鹃悜绛嬫晬婵犲﹤瀚壕顖炴⒑闂堟丹娑㈠磼濠婂懏鍠掗梻鍌氬€烽悞锔锯偓绗涘懏宕查柛宀€鍊涢崶銊ヮ嚤闁哄鍨归崢閬嶆⒑閸︻厼鍔嬮柛銈嗕亢閵囨劙骞掗幘瀛樼彸闂備礁澹婇崑鍛垝閹惰棄绠荤紓浣诡焽閸橆亪姊虹化鏇炲⒉妞ゃ劌鎳樺鎶芥晲婢跺鍘搁悗鍏夊亾閻庯綆鍓涜ⅵ婵°倗濮烽崑鐐垫暜閿熺姴绠栨繛鍡樻尰閸嬨劑鏌涢…鎴濅簼妞ゆ梹鎸搁埞鎴炲箠闁稿﹥顨嗛幈銊╂倻閽樺锛涢梺缁樺姇閹碱偆绮堟径瀣闁糕剝蓱鐏忕數鈧鎸风欢姘跺蓟濞戙垹鐒洪柛鎰╁妼濮ｅ牆鈹戦垾鎻掑珟闁告梹鍨甸～蹇撁洪鍕啇闂佺粯鍔栫粊鎾磻閹捐鍐€妞ゆ挻澹曢崑鎾诲礃椤旂厧绐涢梺鍝勵槹閸ㄥ綊宕㈤幘顔解拺缁绢厼鎳忚ぐ褔姊婚崟顐㈩伃鐎规洘鍨块獮鍥偋閸垹骞嶇紓鍌氬€烽悞锕傛晪缂備焦顨呴崐鑽ゆ閹烘惟闁靛⿵瀵屽Λ鍕⒑鐎圭媭娼愰柛銊ユ健楠炲啫鈻庨幘鏉戔偓缁樻叏濡も偓閻楀繘宕ヨぐ鎺撯拻濞达絽鎲￠崯鐐淬亜閵娿儲顥炵紒缁樼⊕缁绘繈宕掗妶鍛吙濠电姷鏁告慨鐢靛枈瀹ュ鐤鹃柟缁樺俯閻斿棝鎮归崫鍕╁仺闁跨喓濮撮悡娑㈡煕濞戝崬鏋ら柛姗嗕邯濡懘顢曢姀鈥愁槱濠电偛寮堕悧鏇㈡儍椤掆偓閳规垿鎮╅鑲╀紘濡炪値鍋勯ˇ顖炩€栨繝鍥ㄥ仼濠㈣埖鍔栭埛鎺楁煕鐏炴崘澹橀柍褜鍓氶幃鍌氱暦閹版澘绠瑰ù锝呮憸閿涙瑩姊鸿ぐ鎺擄紵缂佲偓娴ｅ搫顥氱憸鐗堝笚閻撴瑩姊婚崒姘煎殶妞わ讣濡囬惀顏堝箚瑜庣拹锟犳煃瑜滈崜娑㈠箠閹惧灈鍋撳鐓庡籍鐎规洖纾竟鏇犫偓锝冨妺濮规姊洪崷顓炲妺闁搞劌缍婂畷鎰版倷閻戞ê浠┑鐘诧工閸犳艾螣閳ь剟鎮楀▓鍨灍闁诡喖鍊搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛鐏炵硶鍋撳畷鍥ㄦ畷闂侀€炲苯澧寸€规洑鍗冲浠嬵敇閻愮數鏆柣鐔哥矋閺屻劑鎮鹃柨瀣檮闁告稑锕ゆ禍婊堟⒑閸涘﹦绠撻悗姘煎墮閳绘捇濡烽埡鍌楁嫽闂佺ǹ鏈悷褔藝閿曞倹鐓欓悹鍥囧懐鐦堥悗瑙勬礀濠€閬嶅箟閹绢喖绀嬫い鎺嗗亾鐎殿喖娼″娲捶椤撯剝顎楅梺鍝ュУ閻楃娀骞冮垾鎰佹建闁逞屽墴瀵鎮㈤崨濠勭Ф婵°倧绲介崯顖烆敁瀹ュ鈷戠紒瀣儥閸庢劙鏌涢弮鈧悷鈺呯嵁閸愩剮鏃堝川椤撶喎绁舵俊鐐€栭幐楣冨磹椤愶箑顫呴幒铏濠婂牊鐓忓鑸电☉椤╊剛绱掗悩鎰佺劷缂佽鲸甯楀蹇涘Ω閿曗偓闂夊秹姊洪悷鏉挎Щ闁硅櫕锚閻ｇ兘顢曢敃鈧粈瀣棯閻楀煫顏呯閸撗€鍋撻崗澶婁壕闂佸憡娲﹂崜娑㈠储闁秵鈷戦梻鍫熻儐瑜版帒纾块柟鎯版閺勩儵鏌ㄥ┑鍡樼闁稿鎸搁埢鎾诲垂椤旂晫浜俊鐐€ら崣鍐绩鏉堛劎鈹嶅┑鐘叉搐缁犵懓霉閿濆懏鎲搁柛妯绘倐濮婅櫣绮欓幐搴㈡嫳闂佹椿鍙庨崰姘舵嚍鏉堛劎顩烽悗锝庡亐閹风粯绻涙潏鍓у埌闁硅櫕鐟ㄩ妵鎰板箳閹存繄褰夋俊鐐€栫敮鎺楀磹婵犳碍鍎楁繛鍡樻尰閻撴瑩寮堕崼鐔峰姢闁伙附绮撻弻鈩冩媴缁嬪簱鍋撻崸妤€钃熼柕濞炬櫆閸嬪棝鏌涚仦鍓р槈妞ゅ骏鎷�
    if (lval->get_symbolEntry()->isConstIdentifer())
    {
        fprintf(stderr, "identifier \"%s\" is const\n", lval->get_name().c_str());
    }


    Type* type1 = lval->getSymPtr()->getType();
    if (type1->isFunc())
    {
        fprintf(stderr, "funct can't be lval \n");
    }
    Type* type2 = expr->getSymPtr()->getType();
    if (type2->isFunc())
    {
        type2 = ((FunctionType*)type2)->returnType;
    }

    if (type1->get_range() != type2->get_range())
    {
        fprintf(stderr, "type %s and %s mismatch \n",
            type1->toStr().c_str(), type2->toStr().c_str());
        if (ERROR_MESSAGE_WRITE_INTO_AST)
        {
            fprintf(yyout, "type %s and %s mismatch \n",
                type1->toStr().c_str(), type2->toStr().c_str());
        }
    }
    else
    {
        if (type1->getKindValue() != type2->getKindValue())
        {
            fprintf(stderr, "type %s and %s mismatch , but we convert %s to %s for you \n",
                type1->toStr().c_str(), type2->toStr().c_str(), type2->toStr().c_str(), type1->toStr().c_str());
            if (ERROR_MESSAGE_WRITE_INTO_AST)
            {
                fprintf(yyout, "type %s and %s mismatch , but we convert %s to %s for you \n",
                    type1->toStr().c_str(), type2->toStr().c_str(), type2->toStr().c_str(), type1->toStr().c_str());
            }
        }
    }


    if (lval != nullptr)
        lval->typeCheck();
    if (expr != nullptr)
        expr->typeCheck();
    //if (type1 != type2)
    //{
    //    fprintf(stderr, "type %s and %s mismatch \n",
    //        type1->toStr().c_str(), type2->toStr().c_str());
    //    if (ERROR_MESSAGE_WRITE_INTO_AST)
    //    {
    //        fprintf(yyout, "type %s and %s mismatch \n",
    //            type1->toStr().c_str(), type2->toStr().c_str());
    //    }
    //}
}





//闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崰姘熼埀顒勬倵濞堝灝鏋熼柟顔煎€搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛鐏炵硶鍋撳畷鍥ㄦ畷闂侀€炲苯澧寸€规洑鍗冲浠嬵敇閻愮數鏆柣鐔哥矋閺屻劑鎮鹃柨瀣檮闁告稑锕ゆ禍婊堟⒑閸涘﹦绠撻悗姘煎墮閳绘捇濡烽埡鍌楁嫽闂佺ǹ鏈悷褔藝閿曞倹鐓欓悹鍥囧懐鐦堥悗瑙勬礀濠€閬嶅箟閹绢喖绀嬫い鎺嗗亾鐎殿喖娼″娲捶椤撯剝顎楅梺鍝ュУ閻楃娀骞冮垾鎰佹建闁逞屽墴瀵鎮㈤崨濠勭Ф婵°倧绲介崯顖烆敁瀹ュ鈷戠紒瀣儥閸庢劙鏌涢弮鈧悷鈺呯嵁閸愩剮鏃堝川椤撶喎绁舵俊鐐€栭幐楣冨磹椤愶箑顫呴幒铏濠婂牊鐓忓鑸电☉椤╊剛绱掗悩鎰佺劷缂佽鲸甯楀蹇涘Ω閿曗偓闂夊秹姊洪悷鏉挎Щ闁硅櫕锚閻ｇ兘顢曢敃鈧粈瀣棯閻楀煫顏呯閸撗€鍋撻崗澶婁壕闂佸憡娲﹂崜娑㈠储闁秵鈷戦梻鍫熻儐瑜版帒纾块柟鎯版閺勩儵鏌ㄥ┑鍡樼闁稿鎸搁埢鎾诲垂椤旂晫浜俊鐐€ら崢楣冨礂濡警鍤曟い鎰剁畱缁犳稒銇勯幘璺轰户缂佹劗鍋炵换婵嬫偨闂堟刀銏ゆ倵濮樺崬鍘寸€规洏鍎靛畷銊р偓娑櫱氶幏缁樼箾鏉堝墽鎮奸柟铏崌椤㈡艾饪伴崨顖滐紲闁荤姴娲﹁ぐ鍐焵椤掆偓濞硷繝鎮伴钘夌窞濠电偟鍋撻～宥夋⒑闂堟稓绠冲┑顔惧厴椤㈡瑩骞掗弮鍌滐紳闂佺ǹ鏈悷褔宕濆鍡愪簻妞ゆ挾鍋為崰妯尖偓瑙勬磸閸ㄤ粙鐛弽銊﹀闁稿繐顦扮€氬ジ姊绘担鍛婂暈缂佸鍨块弫鍐晲閸ヮ煈鍋ㄩ梻渚囧墮缁夌敻鎮￠弴銏＄厽婵☆垵顕ф晶顖涚箾閸喓鐭岄柍褜鍓濋～澶娒哄鈧畷婵嗏枎閹捐泛绁﹂梺鎼炲労閸撴岸宕戠€ｎ喗鐓曟い鎰Т閻忊晜顨ラ悙鏉戝闁哄矉绲鹃幆鏃堝灳閸愯尪绶熼梻浣呵归鍡涘箲閸ヮ剙绠栨俊銈呮噺閺呮煡骞栫划鍏夊亾閹颁焦楠勯梻鍌欐缁鳖喚绮婚幋锔藉亱闁规崘顕х粻鏍ㄤ繆閵堝懎鏆為柛鐘叉閺屾盯寮撮妸銉ヮ潾濡炪倧璐熼崝鎴﹀蓟閿濆顫呴柍杞拌兌娴狀參鏌ｉ姀鈺佺仯闁哥姵鐗犻獮鍐潨閳ь剟鐛幒鎳虫梹鎷呴崫鍕闂傚倷鑳剁划顖炲礉閺囥埄鏁嬫い鎾卞灩閻撴繈鏌熼崜褏甯涢柣鎾冲暟閹茬ǹ饪伴崼婵堫槶濠电偛妫欓幐鎼佸磼閵娾晜鐓涚€广儱鍟俊鍧楁煟濠靛棛鍩ｉ柡宀嬬節瀹曞爼濡烽妷褌鐥梻浣瑰▕閺€閬嶅垂閸ф钃熸繛鎴炃氶弨浠嬫煕閳╁喚娈㈠ù灏栧亾闂傚倷绀侀悿鍥敋瑜旇棟闁汇垻枪閽冪喐绻涢幋鐐茬劰闁稿鎹囬弫宥夊础閻愬弶娈ㄦ繝銏ｎ潐濞茬喎顫忔繝姘＜婵炲棙甯掗崢锟犳煛鐎ｅ吀绨奸柕鍥у楠炲鈹戦崶鑸碉紗婵°倗濮烽崑鐐哄礉濞嗘挾宓侀柡宥冨妿閻熺懓鈹戦悩鎻掝伀闁崇鍊栫换婵嬫偨闂堟稈鏋呭┑鐐板尃閸ヨ埖鏅為梺鍦濠㈡﹢寮告笟鈧弻娑㈠焺閸愵亖濮囬梺缁樻尰濞茬喖骞冨鈧幃娆撳箵閹哄棙瀵栭梻浣芥〃閻掞箓鎮ч悩璇茶摕闁哄洨鍠撶粻楣冩煟閹伴潧澧柣婵囨礋濮婃椽骞庨懞銉︽殸闂佹悶鍔屽鈥愁嚕鐠囨祴妲堟俊顖炴敱閺傗偓闂備胶纭堕崜婵嬨€冭箛鏂款嚤闁稿本鍑瑰〒濠氭煏閸繈顎楀ù婊冨⒔缁辨帡鎮╅搹顐㈢３閻庢鍠涢褔鍩ユ径鎰潊闁绘鏁稿澶愭⒒娓氣偓閳ь剛鍋涢懟顖涙櫠鐎电硶鍋撶憴鍕缂傚秴锕ら悾椋庣矙鐠囩偓妫冨畷姗€濡歌閸嬪﹪姊婚崒娆戭槮闁硅绻濆畷婵嬫晝閸屾氨鐛ラ梺鍝勭▉閸欏繒绱為弽顓熺厱婵炴垶顭囬幗鐘绘煟閹惧磭绠婚柡灞剧洴椤㈡洟鏁愰崶鈺冩毇闂備線娼荤徊钘夛耿鏉堚晜顫曢柟鐑橆殔缁犳稒銇勯幘璺盒涘┑顔兼川缁辨挻鎷呴搹鐟扮缂備浇顕ч悧鎾荤嵁閸愨晝顩烽悗锝庡亽濡啫鈹戦悙娈挎缂傚牅鍗冲畷瑙勭鐎ｎ亞鐣哄┑掳鍊愰崑鎾淬亜椤愶絿绠炴慨濠呭吹閳ь剟娼ч幉锟犲疾閸忚偐绡€缁剧増菤閸嬫捇宕橀懠顒勭崜闂備礁鎲″褰掓偡閵夆晩鏁嬮柨婵嗩槸缁犵粯銇勯弮鍌楁嫛婵炵厧锕娲箮閼恒儲娈伴梺绋款儐閹瑰洭寮婚悢鍓叉Ч閹艰揪绲界粭锛勭磽娴ｈ櫣甯涚紒瀣笒椤洩绠涘☉妯碱槶閻熸粌绉瑰畷鍝勭暆閸曨兘鎷虹紒缁㈠幖閹冲氦顣跨紓鍌欑贰閸犳牕霉閻戣棄绀嗛柟鐑橆殔缁秹鏌涢銈呮瀻闁逞屽墰缁垱绌辨繝鍥舵晬闁绘劕鐡ㄩ悵宕囩磽娴ｉ潧濡兼い顓炲槻椤繐煤椤忓拋妫冨┑鐐村灱妞存悂顢撻崱娑欌拻闁稿本鑹鹃鈺呮倵濮樼厧澧撮柟顔藉劤閻ｏ繝骞嶉鑺ョ暦闂備線鈧偛鑻晶鎾煕閳规儳浜炬俊鐐€栫敮鎺楀窗濮橆剦鐒介柟閭﹀幘缁犻箖鏌涘▎蹇ｆ闁兼澘娼￠弻鏇㈠幢濡も偓閺嗭絿鈧娲忛崝宥咁焽韫囨稑绀堢憸蹇涘煟閵堝棛绡€闁汇垽娼у瓭闂佹寧娲忛崐婵嬪箖瑜斿畷鍗炩槈濞嗗繆鍋撻崸妤佺厱妞ゎ厽鍨垫禍鏍瑰⿰鍕煉闁哄瞼鍠栧畷顐﹀礋椤掑顥ｅ┑鐐茬摠缁秹宕曢幎瑙ｂ偓鏃堝礃椤斿槈褔鏌涢埄鍐剧劷闁宠绋撶槐鎾诲磼濮樻瘷锛勭磼閼搁潧鍝哄┑鈥崇摠缁楃喖鍩€椤掆偓椤曪綁骞橀纰辨綂闂佹枼鏅涢崯顖炴偟閺嶎厽鈷掑〒姘ｅ亾闁逞屽墰閸嬫盯鎳熼娑欐珷闁规鍠氱壕鍏笺亜閺傚灝鈷旈悽顖涚〒缁辨帞绱掑Ο鑲╃暤濡炪値鍋呯换鍫ャ€佸鈧幃娆撳级閸噮鍤欓梻鍌氬€搁崐鐑芥倿閿曞倸绠栭柛顐ｆ礀绾炬寧绻濇繝鍌滃缂佲偓閸愨斂浜滈煫鍥ㄦ尵婢с垻鈧娲樼划宀勫煘閹达附鏅柛鏇ㄥ亗閺夘參姊虹粙鍖℃敾闁搞劌鐏濋悾鐑藉即閵忊€虫濡炪倖甯婄粈浣规償婵犲洦鈷戦柛鎾村絻娴滄繄绱掔拠鑼㈡い顓炴喘瀵粙濡歌椤旀洟鎮楅悷鏉款棌闁哥姵娲滈懞閬嶅礂缁楄桨绨婚梺闈涱槶閸庤櫕鏅跺☉姘辩＜缂備焦顭囧ú瀛橆殽閻愬樊鍎旈柟顔界懅閹瑰嫭绗熼娑辨（婵犵绱曢崑鎴﹀磹瑜忓濠冪鐎ｎ亞顔愬銈嗗姧缁犳垿鎮″鈧弻鐔告綇閸撗呮殸闂佺粯鎼换婵嬪蓟濞戙垹鐒洪柛鎰剁細濞岊亞绱撴担闈涘闁告鍥ｂ偓鏃堝礃椤斿槈褔鏌涢幇鈺佸妞ゎ剙鐗撳娲箰鎼淬垹顦╂繛瀛樼矤娴滄繃绌辨繝鍕ㄥ亾濞戞瑯鐒介柣鐔风秺閺屽秷顧侀柛鎾寸懇椤㈡岸鏁愭径妯绘櫔闂侀€炲苯澧柣锝囧厴閹剝鎯斿Ο缁樻澑闂佽鍑界紞鍡樼濠靛姹叉い蹇撶墛閳锋帡鏌涚仦鍓ф噮妞わ讣绠撻弻鐔哄枈閸楃偘绨婚柣鎾卞€濋弻鏇熺箾閻愵剚鐝旈梺姹囧€ら崳锝夊蓟濞戞粠妲煎銈冨妼閹虫劗鍒掓繝姘兼晬婵炴垶姘ㄩ鏇㈡倵閻熸澘顥忛柛鐘虫礈閼鸿鲸绺介崨濠勫幗闂佽宕樺▔娑㈠几濞戙垺鐓涚€光偓鐎ｎ剙鍩岄柧浼欑秮閺屾稑鈹戦崱妤婁患缂備焦顨忛崣鍐潖濞差亝鍋傞幖绮规濡本绻涚€涙鐭ゅù婊庝簻椤曪絿鎷犲ù瀣潔闂侀潧绻掓慨鐢杆夊┑瀣厽闁绘ê鍘栭懜顏堟煕閺傚潡鍙勭€规洘绻堥、娑㈡倷閺夋垟鍋撻崹顐ょ闁割偅绻勬禒銏ゆ煛鐎ｎ剙鏋涢柡宀€鍠栭、娆撴偂鎼存ê浜鹃柛褎顨嗛崑妯汇亜閺冨牊鏆滈柛瀣崌閺佹劖鎯旈埄鍐憾闂備礁鎼幊蹇曞垝閹捐钃熼柨婵嗩槹閺呮煡鏌涘☉娆愮凡妞ゅ浚鍘艰灃闁绘﹢娼ф禒锕傛煕閺冣偓閻熴儵锝炶箛娑欐優閻熸瑥瀚悵浼存⒑閸︻厾甯涢悽顖涘笒琚欓柟閭﹀枤绾句粙鏌涚仦鐐殤鐎涙繂鈹戦埥鍡椾簼闁荤啙鍛灊閻庯綆浜堕崥瀣熆鐠虹尨韬柛鐐茬埣濮婃椽宕崟顒€绐涢梺鍝ュТ鐎涒晝绮嬪鍛傛棃宕ㄩ瑙勫缂傚倷绀侀鍫濃枖閺囩姷涓嶉柟鎯板Г閻撳啰鈧懓瀚竟鍡樻櫠閺囥垺鐓冪紓浣股戠亸鎵磼閸屾稑绗ч柍褜鍓ㄧ紞鍡涘磻閸℃稑鍌ㄥù鐘差儐閳锋垹鎲搁悧鍫濈瑨濞存粈鍗抽弻娑㈠Ω閵堝懎绁梺璇″灠閸熸挳骞栬ぐ鎺戞嵍妞ゆ挾濯寸槐鏌ユ⒒娴ｈ櫣甯涢柨姘繆椤栨熬韬柟顔瑰墲缁轰粙宕ㄦ繝鍕箰闁诲骸鍘滈崑鎾绘煃瑜滈崜鐔风暦娴兼潙鍐€妞ゆ挻澹曢崑鎾存媴缁洘鐎婚梺鍦亾濞兼瑦绂掓總鍛婂€甸柛蹇擃槸娴滈箖姊洪柅鐐茶嫰婢ф挳鏌熼鐟板⒉鐎垫澘瀚伴獮鍥敇濞戞瑥顏归梻鍌欐祰瀹曠敻宕伴幇顓犵彾闁糕剝绋掗弲顒傗偓骞垮劚椤︿即鎮￠弴銏″€甸柨婵嗗暙婵″ジ鏌嶈閸撴岸鎮уΔ鍐煔閺夊牄鍔庣弧鈧梺鎼炲劘閸斿矂鍩€椤掆偓椤兘寮婚妶澶婄畳闁圭儤鍨垫慨鏇炩攽閻愬弶鍣规俊顐ｇ〒濡叉劙骞樼€涙ê顎撻梺闈╁瘜閸樼ǹ危閸繍娓婚柕鍫濇閻忋儵鎮楀顐㈠祮闁绘侗鍠氶埀顒婄秵閸犳宕愭繝姘厾闁诡厽甯掗崝妤呮煙瀹勯偊鐓兼慨濠呮缁瑩骞愭惔銏″闂備胶鍘х紞濠勭不閺嶎厼鏄ラ柍褜鍓氶妵鍕箳閹存繍浼屽┑鈽嗗亝閸ㄥ湱妲愰幒妤婃晩闁兼祴鏅涢·鈧紓鍌欑劍椤ㄥ牓宕伴弽顓炴槬闁逞屽墯閵囧嫰骞掗幋婵愪紑閻庤鎸风欢姘跺蓟閳ユ剚鍚嬮幖绮光偓鍐差劀闂備浇妗ㄧ粈渚€宕幘顔艰摕闁靛ň鏅涢崡铏繆閵堝倸浜炬繛瀛樼矊婢х晫妲愰幒妤佸€锋い鎺嗗亾闁告柣鍊楃槐鎾愁吋閸滃啳鍚悗娈垮枦椤曆囧煡婢舵劕顫呴柍鍝勫€瑰▍鍥⒒娴ｇ懓顕滅紒璇插€歌灋婵炴垟鎳為崶顒€唯鐟滃繒澹曟總鍛婄厽闁逛即娼ф晶顕€鏌涢弬璇测偓妤冩閹炬剚鍚嬮柛婊冨暢閸氼偊鎮楀▓鍨灈妞ゎ厾鍏樺畷瑙勩偅閸愩劎鐤€婵炶揪绲介幉锟犲磹椤栫偞鈷戠痪顓炴噹娴滃綊鎮跺☉鏍у姦闁糕斁鍋撳銈嗗笒閸燁偊鎯冨ú顏呯厽闁哄稁鍘洪幉鐐叏婵犲嫮甯涢柟宄版嚇瀹曘劑妫冨☉姘毙ㄥ銈冨灪閻楃姴鐣烽崡鐐╂婵☆垳鈷堥崬鍫曟⒒娴ｅ摜绉烘俊顐ユ硶濞嗐垽濡堕崶鈺冾槸闂佸搫绉查崝搴ｅ姬閳ь剟姊婚崒姘卞濞撴碍顨婂畷鏇炩槈閵忥紕鍘告繛杈剧到閹诧繝藟閵忋倖鐓涢悘鐐插⒔閵嗘帒霉閻欏懐鐣电€规洘绮忛ˇ鎶芥煛閸涱厹鍋㈡慨濠冩そ濡啫霉閵夈儳澧︾€殿喗褰冮オ浼村醇濠靛牆骞堥梻浣侯攰閹活亪姊介崟顖涘亗闁哄洨鍠撶弧鈧梻鍌氱墛缁嬫帡藟濠婂嫨浜滈煫鍥风到婢ь垶鏌曢崶褍顏い銏℃礋椤㈡宕掑⿰鍕啌闂傚倷绀侀幖顐﹀嫉椤掑嫭鍎庢い鏍ㄥ嚬閸ゆ洘銇勯弴妤€浜鹃梺绯曟杹閸嬫挸顪冮妶鍡楃瑨閻庢凹鍓熷畷褰掑磼閻愬鍘遍悷婊冮叄閵嗗啴宕煎┑鍫熸婵炴挻鍩冮崑鎾绘煛鐏炲墽娲存鐐叉喘濡啫鈽夊▎鎴滈偗濠碉紕鍋戦崐銈夊磻閸曨垰绠犳慨妞诲亾鐎殿喖顭峰鎾閻樿鏁规繝鐢靛█濞佳兠洪妶鍛瀺闁挎繂娲ㄧ壕钘壝归敐鍥ㄥ殌濠殿喖鐗忕槐鎺斺偓锝庡亜缁椦囨煙楠炲灝鐏╅柍瑙勫灩閳ь剨缍嗛崑鍕濞差亝鈷掗柛灞炬皑婢ф盯鏌涢幒鍡椾壕闂備線娼х换鍫ュ磹閺嶎厼纾归柛顭戝亝閸欏繑鎱ㄥ璇蹭壕濠碘槅鍋夊▔鏇㈡嚍闁秵鍤嶉柕澶堝€楃粻姘舵⒑閸涘﹦鎳冩い锔诲灡閹便劌顓奸崶锝呬壕婵炲牆鐏濋弸鐔兼煙濮濆本鐝柟渚垮姂閸┾偓妞ゆ帒瀚悡鍐⒑濞嗘儳鐏犲ù婊堢畺濮婇缚銇愰幒婵囶棖缂備緡鍣崹鎶藉箲閵忕姭妲堟繛鍡樺姉缁夊爼姊洪崨濠冨瘷闁告劑鍔庨崢鎺楁⒒閸屾瑨鍏屾い銏狅攻閹便劑濡堕崶褍鐏婇柟鑹版彧缁蹭粙宕瑰┑瀣厸闁告劑鍔庢晶娑㈡煕婵犲嫮甯涘ǎ鍥э躬椤㈡盯鎮欑€涙褰哥紓鍌欓檷閸斿矂鈥﹀畡閭︽綎婵炲樊浜濋ˉ鍫熺箾閹达綁鍝烘い搴℃濮婂宕掑顒変患闁诲孩鍑归崜鐔煎箖閹呮殝闁规鍠楀▓鏇㈡⒑闁偛鑻晶鎾煕閳规儳浜炬俊鐐€栫敮鎺楀窗濮橆剦鐒介柟閭﹀幘缁犻箖鏌涘▎蹇ｆ闁兼澘娼￠弻鏇㈠幢濡も偓閺嗭絿鈧娲忛崝宥咁焽韫囨稑绀堢憸蹇涘煟閵堝棛绡€闁汇垽娼у瓭闂佹寧娲忛崐婵嬪箖瑜斿畷鍗炩槈濞嗗繆鍋撻崸妤佺厱妞ゎ厽鍨垫禍鏍瑰⿰鍕煉闁哄瞼鍠栧畷顐﹀礋椤掑顥ｅ┑鐐茬摠缁秹宕曢幎瑙ｂ偓鏃堝礃椤斿槈褔鏌涢埄鍐剧劷闁宠绋撶槐鎾诲磼濮樻瘷锛勭磼閼搁潧鍝哄┑鈥崇摠缁楃喖鍩€椤掆偓椤曪綁骞橀纰辨綂闂佹枼鏅涢崯顖炴偟閺嶎厽鈷掑〒姘ｅ亾闁逞屽墰閸嬫盯鎳熼娑欐珷闁规鍠氱壕鍏笺亜閺傚灝鈷旈悽顖涚〒缁辨帞绱掑Ο鑲╃暤濡炪値鍋呯换鍫ャ€佸鈧幃娆撳级閸噮鍤欓梻鍌氬€搁崐鐑芥倿閿曞倸绠栭柛顐ｆ礀绾炬寧绻濇繝鍌滃缂佲偓閸愨斂浜滈煫鍥ㄦ尵婢с垻鈧娲樼划宀勫煘閹达附鏅柛鏇ㄥ亗閺夘參姊虹粙鍖℃敾闁搞劌鐏濋悾鐑藉即閵忊€虫濡炪倖甯婄粈浣规償婵犲洦鈷戦柛鎾村絻娴滄繄绱掔拠鑼㈡い顓炴喘瀵粙濡歌椤旀洟鎮楅悷鏉款棌闁哥姵娲滈懞閬嶅礂缁楄桨绨婚梺闈涱槶閸庤櫕鏅跺☉姘辩＜缂備焦顭囧ú瀛橆殽閻愬樊鍎旈柟顔界懅閹瑰嫭绗熼娑辨（婵犵绱曢崑鎴﹀磹瑜忓濠冪鐎ｎ亞顔愬銈嗗姧缁犳垿鎮″鈧弻鐔告綇閸撗呮殸闂佺粯鎼换婵嬪蓟濞戙垹鐒洪柛鎰剁細濞岊亞绱撴担闈涘闁告鍥ｂ偓鏃堝礃椤斿槈褔鏌涢幇鈺佸妞ゎ剙鐗撳娲箰鎼淬垹顦╂繛瀛樼矤娴滄繃绌辨繝鍕ㄥ亾濞戞瑯鐒介柣鐔风秺閺屽秷顧侀柛鎾寸懇椤㈡岸鏁愭径妯绘櫔闂侀€炲苯澧柣锝囧厴閹剝鎯斿Ο缁樻澑闂佽鍑界紞鍡樼濠靛姹叉い蹇撶墛閳锋帡鏌涚仦鍓ф噮妞わ讣绠撻弻鐔哄枈閸楃偘绨婚柣鎾卞€濋弻鏇熺箾閻愵剚鐝旈梺姹囧€ら崳锝夊蓟濞戞粠妲煎銈冨妼閹虫劗鍒掓繝姘兼晬婵炴垶姘ㄩ鏇㈡倵閻熸澘顥忛柛鐘虫礈閼鸿鲸绺介崨濠勫幗闂佽宕樺▔娑㈠几濞戙垺鐓涚€光偓鐎ｎ剙鍩岄柧浼欑秮閺屾稑鈹戦崱妤婁患缂備焦顨忛崣鍐潖濞差亝鍋傞幖绮规濡本绻涚€涙鐭ゅù婊庝簻椤曪絿鎷犲ù瀣潔闂侀潧绻掓慨鐢杆夊┑瀣厽闁绘ê鍘栭懜顏堟煕閺傚潡鍙勭€规洘绻堥、娑㈡倷閺夋垟鍋撻崹顐ょ闁割偅绻勬禒銏ゆ煛鐎ｎ剙鏋涢柡宀€鍠栭、娆撴偂鎼存ê浜鹃柛褎顨嗛崑妯汇亜閺冨牊鏆滈柛瀣崌閺佹劖鎯旈埄鍐憾闂備礁鎼幊蹇曞垝閹捐钃熼柨婵嗩槹閺呮煡鏌涘☉娆愮凡妞ゅ浚鍘艰灃闁绘﹢娼ф禒锕傛煕閺冣偓閻熴儵锝炶箛娑欐優閻熸瑥瀚悵浼存⒑閸︻厾甯涢悽顖涘笒琚欓柟閭﹀枤绾句粙鏌涚仦鐐殤鐎涙繂鈹戦埥鍡椾簼闁荤啙鍛灊閻庯綆浜堕崥瀣熆鐠虹尨韬柛鐐茬埣濮婃椽宕崟顒€绐涢梺鍝ュТ鐎涒晝绮嬪鍛傛棃宕ㄩ瑙勫缂傚倷绀侀鍫濃枖閺囩姷涓嶉柟鎯板Г閻撳啰鈧懓瀚竟鍡樻櫠閺囥垺鐓冪紓浣股戠亸鎵磼閸屾稑绗ч柍褜鍓ㄧ紞鍡涘磻閸℃稑鍌ㄥù鐘差儐閳锋垹鎲搁悧鍫濈瑨濞存粈鍗抽弻娑㈠Ω閵堝懎绁梺璇″灠閸熸挳骞栬ぐ鎺戞嵍妞ゆ挾濯寸槐鏌ユ⒒娴ｈ櫣甯涢柨姘繆椤栨熬韬柟顔瑰墲缁轰粙宕ㄦ繝鍕箰闂佽绻掗崑鐔哥仚婵犫拃鍕弨闁哄瞼鍠庨悾锟犳偋閸繃顏ら梻浣烘嚀瀵爼骞愰崘鑼殾闁绘梻鈷堥弫鍐煏閸繄绠冲璺哄濮婂宕掑顑藉亾瀹勬噴褰掑炊瑜滃ù鏍煏婵炵偓娅嗛柛濠傛健閺屻劑寮撮悙娴嬪亾閸濄儳涓嶉柡灞诲劜閻撴洟鏌曟径妯烘灈濠⒀屽櫍閺岋紕鈧絺鏅濈粣鏃堟煛瀹€鈧崰鏍х暦濠婂棭妲鹃柣銏╁灡閻╊垶寮婚敓鐘插窛妞ゆ棁妫勯埀顒佸姍閺岋紕浠︾拠鎻掝潎闂佽鍠撻崐婵嗙暦閹烘垟妲堟慨妤€妫旂槐锟�
/*闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚敐澶婄闁挎繂鎲涢幘缁樼厱闁靛牆鎳庨顓㈡煛鐏炲墽娲存い銏℃礋閺佹劙宕卞▎妯恍氱紓鍌氬€烽懗鑸垫叏闁垮绠鹃柍褜鍓熼弻鈥崇暆閳ь剟宕伴弽褏鏆︽繝濠傛－濡查箖鏌ｉ姀鈺佺仭闁烩晩鍨跺璇测槈濮橈絽浜鹃柨婵嗗暙婵″ジ鏌嶈閸撴氨鎹㈤崼婵愬殨濠电姵鑹鹃崡鎶芥煟閺冨洦顏犳い鏃€娲熷铏圭磼濡搫袝闂佸憡鎸诲畝鎼佸箖閻㈢ǹ鍗抽柕蹇婃閹锋椽姊洪崨濠勭畵閻庢凹鍙冨畷鎶芥惞閸︻厾锛滈柣鐘叉穿鐏忔瑦鏅堕敂閿亾濞堝灝鏋涙い顓炲槻椤曪綁骞橀鑺ユ珫闂佸憡娲﹂崢钘夘熆閹达附鈷戦悹鍥ㄥ絻閸よ京绱撳鍛棡缂佸倸绉瑰浠嬵敇閻愮绱遍梻浣筋潐瀹曟﹢顢氳瀹曪繝骞庨懞銉у帾闂婎偄娲﹀ú鏍х摥闂備焦鎮堕崐鏍垂閸撲焦宕叉繛鎴欏灩缁犲鏌℃径瀣仼闁告柧鍗冲铏圭矙濞嗘儳鍓遍梺鍦嚀濞差厼顕ｆ繝姘伋鐎规洖娲﹀▓鏇㈡⒑闁偛鑻晶瀵糕偓瑙勬磻閸楀啿顕ｆ禒瀣垫晣闁绘ǹ灏欓妶锕傛⒒娴ｄ警鏀伴柟娲讳簽缁骞嬮悩鍏哥瑝濠电偞鍨崹娲煕閹烘鐓曟い鎰╁€曢弸鏃堟煃椤栨稒绀冮柕鍥у瀵剟宕归鍛棯缂傚倷鑳剁划顖滄崲閸曨垰绐楀┑鐘蹭迹閻旇铏圭磼濡偐效闂傚倸鍊烽懗鑸电仚濡炪倖鍨甸幊姗€鐛繝鍌ゆ建闁逞屽墮椤曪綁顢曢敃鈧粈鍐┿亜閺冨倵鎷℃繛鐓庯躬濮婃椽妫冨☉姘暫闂佺娅曢幑鍥嵁濡ゅ懎鍗抽柕蹇婃閹锋椽姊洪崨濠勭畵閻庢凹鍙冨畷鎺楀Ω閳哄倻鍘遍梺闈浨归崕宕囩矓濞差亝鐓涢悘鐐电摂閸庢梻鈧娲栭悥鍏间繆濮濆矈妲鹃梺浼欑畱閻楁挸顫忔繝姘＜婵ê宕·鈧紓鍌欑椤戝棛鏁悙闈涘灊濠电姴娲ら悡娑㈡煕鐏炲墽鎳呯紒鐘宠壘椤啴濡堕崱妤€顫囬梺绋匡攻濞叉绮嬮幒鎴斿牚闁告洍鏅欑花璇差渻閵堝懐绠伴悗姘煎墴瀵娊鏁愰崨顏呮杸闂佺偨鍎辩壕顓㈠春閿濆洠鍋撶憴鍕８闁告梹鍨块妴浣糕槈濡嘲鐗氶柟鑲╄ˉ閸撴繈寮抽鍫熲拻闁稿本鑹鹃埀顒傚厴閹虫宕滄担绋跨亰濡炪倖鐗滈崑鐐哄磻鐎ｎ偆绡€濠电姴鍊绘晶鏇㈡煕鐏炶濡块柟鍙夋倐瀵噣宕奸悢鍛婄彸闂備胶绮崝鏇熸櫠鎼淬劍鍋柍褜鍓欓埞鎴︽倷閺夋垹浠搁梺鑽ゅ櫐缁犳挸鐣烽弴銏╂晜闁割偆鍠撻崢浠嬫⒑缂佹ɑ鐓ラ柟鑺ョ矒楠炲﹪宕橀钘夆偓鍨叏濡厧甯跺褎婢橀…璺ㄦ喆閸曨剛顦板銈冨灪閻╊垶骞冨▎鎴濆灊閻熸瑥瀚褰掓⒒閸屾艾鈧悂宕愰幖浣瑰亱濠电姴娲﹂崵鍕煕閳╁啫鎼稿ù婊冪秺閺岀喖鎮滃Ο铏逛患闂佹寧绋撻崰鏍ь潖濞差亜浼犻柛鏇ㄥ櫘濞煎爼姊虹粙鍨劉闁绘挴鈧磭鏆﹂柟鎵椤ュ牊绻涚壕瀣彧婵☆偄鍟村顐﹀箛閺夊灝绐涘銈嗘尵閸犳劙顢欐径鎰拻濞达絿鎳撻埢鏇㈡煛閳ь剟鏌嗗鍛€梺绉嗗嫷娈旈柦鍐枑缁绘盯骞嬮悙鍐╁哺瀵劍绂掔€ｎ偆鍘遍梺鏂ユ櫅閸犳艾鈻撻姀鐘嗗綊鎮℃惔锝囧姱濠殿喖锕ㄥ▍锝囧垝濞嗘挸閿ゆ俊銈吪堥崑鎾澄旈崘顏嗭紲缂傚倷鐒﹂敋闁诲繑娼欓埞鎴︽晬閸曨剚姣堥悗瑙勬礈閸犳牠銆佸☉銏℃櫜闁糕剝蓱閻濇繈姊婚崒娆掑厡缂侇噮鍨甸幗顐︽偡濠婂嫭绶查柛鐔告尦閹即顢欓柨顖氫壕闁挎繂楠搁弸鐔兼煃闁垮鐏╃紒杈ㄦ尰閹峰懏鎱ㄩ幋锝呬汗婵炲棎鍨介弻鍡楊吋閸℃瑥骞堥梻浣虹帛閿氱痪缁㈠弮婵℃挳骞掗弮鍌滐紲濡炪倖姊归娆撳吹濞嗘挻鐓冮柦妯侯樈濡叉悂鎽堕敐澶嬧拺闁割煈鍣崕娑欑箾閸忓吋鈷愮紒缁樼箞閹粙妫冨☉妤冩崟婵犵妲呴崑鍛淬€冮崼銉ョ闁靛繈鍊曢獮銏＄箾閹寸儐鐒介柨娑欑洴濮婅櫣鎲撮崟顐㈠Б濡炪倖娲﹂崢鎯ь嚕瑜旈崺鈧い鎺嗗亾闁宠鍨块、娆撳棘閵堝嫮杩旈梻浣告啞閿曘垻绮婚弽褏鏆﹂柕寰涙澘浜濋梺鍛婂姀閺備線骞忓ú顏呯厽闁绘ê寮剁粈宀勬煃瑜滈崜娆撳疮鐠恒劎涓嶆繛宸簼閻撶喖鏌曡箛濠冩珔闁诲骏绻濋幃浠嬵敍濡炶浜剧€规洖娲﹀▓鎯р攽閻樼粯娑ф俊顐㈢焸瀹曞ジ顢旈崼鐔哄幈闂佸搫娲㈤崝宀勭嵁濡眹浜滈柡鍐ｅ亾婵炶尙鍠庨～蹇撁洪鍕獩婵犵數濮抽懗鍓佹崲娓氣偓濮婅櫣绮欏▎鎯у壈闁诲孩鐭崡鎶界嵁閸愵煈娼ㄩ柍褜鍓欓悾鐑芥晲閸℃绐為柣搴秵娴滄粍瀵兼惔鈾€鏀介柣妯虹仛閺嗏晛鈹戦鑺ュ唉鐎规洦鍨堕、娑㈡倷閸欏偊闄勭换婵嬫濞戞艾顣甸梺绋款儐閹搁箖骞夐幘顔肩妞ゆ巻鍋撴い锔规櫊濮婅櫣绮欏▎鎯у壈闂佹寧娲忛崐婵嬪箖妤ｅ啯鍊婚柤鎭掑劚娴滄粎绱掗悙顒€顎滃瀛樻倐瀵彃鈹戠€ｎ偀鎷虹紓鍌欑劍钃辨い銉ユ缁绘盯骞樼拠鈩冪秷缂備焦姊婚崰鏍ь嚕閹绢喖顫呴柍閿亾闁归攱妞藉娲川婵犲啫闉嶉悷婊勬緲閸燁垳绮嬪鍛斀闁搞儮鏅濋鏇㈡⒑閸涘﹦鎳冩い锕侀哺閺呭爼宕￠悜鍡欏數闁荤喐鐟ョ€氼厾绮堟径鎰厪闁搞儯鍔屾慨宥嗩殽閻愭潙娴鐐搭焽閹瑰嫰宕崟顓у敹闂傚倷娴囬褎顨ラ崫銉т笉鐎广儱顦壕鍧楀级閸偆鍘涙繛鍫滅矙閺屾稑鈽夊鍫濆濡炪倐鏅滈悡锟犲蓟閻旂厧绠ユい鏃傗拡閺嗩參姊虹紒妯诲鞍婵炶尙鍠栭獮鍐ㄎ旈崨顔芥珳闁硅偐琛ラ埀顒冨皺閸戝綊姊虹拠鑼婵炲瓨宀稿畷銏＄鐎ｎ€箓鏌涢弴銊ョ伇闁轰礁鍟撮弻銊モ攽閸℃ê绐涢梺姹囧€曠€氭澘顫忛搹瑙勫珰闁炽儴娅曢悘宥夋⒑閼姐倕鏆遍柡鍛█婵″瓨鎷呴懖婵囨瀹曘劑顢橀悩鎻捫曞┑锛勫亼閸婃牜鏁幒鏂哄亾濮樺崬鍘寸€殿噮鍋婂畷鎺楁倷鐎电ǹ寮抽梻浣虹帛濞叉牠宕愰崷顓涘亾濮樼偓瀚�

UnaryExpr 
InitNode
ArrDimNode
ParaNode
FunctCall
DeclInitStmt
ConstDeclInitStmt
DeclList
ConstDeclList
WhileStmt
BreakStmt
ContinueStmt
DoNothingStmt*/

void ArrDimNode::typeCheck()
{
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崰姘熼埀顒勬倵濞堝灝鏋熼柟顔煎€搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛鐏炵硶鍋撳畷鍥ㄦ畷闂侀€炲苯澧寸€规洑鍗冲浠嬵敇閻愮數鏆柣鐔哥矋閺屻劑鎮鹃柨瀣檮闁告稑锕ゆ禍婊堟⒑閸涘﹦绠撻悗姘煎墮閳绘捇濡烽埡鍌楁嫽闂佺ǹ鏈悷褔藝閿曞倹鐓欓悹鍥囧懐鐦堥悗瑙勬礀濠€閬嶅箟閹绢喖绀嬫い鎺嗗亾鐎殿喖娼″娲捶椤撯剝顎楅梺鍝ュУ閻楃娀骞冮垾鎰佹建闁逞屽墴瀵鎮㈤崨濠勭Ф婵°倧绲介崯顖烆敁瀹ュ鈷戠紒瀣儥閸庢劙鏌涢弮鈧悷鈺呯嵁閸愩剮鏃堝川椤撶喎绁舵俊鐐€栭幐楣冨磹椤愶箑顫呴幒铏濠婂牊鐓忓鑸电☉椤╊剛绱掗悩鎰佺劷缂佽鲸甯楀蹇涘Ω閿曗偓闂夊秹姊洪悷鏉挎Щ闁硅櫕锚閻ｇ兘顢曢敃鈧粈瀣棯閻楀煫顏呯閸撗€鍋撻崗澶婁壕闂佸憡娲﹂崜娑㈠储闁秵鈷戦梻鍫熻儐瑜版帒纾块柟鎯版閺勩儵鏌ㄥ┑鍡樼闁稿鎸搁埢鎾诲垂椤旂晫浜俊鐐€ら崢楣冨礂濡警鍤曟い鎰剁畱缁犳稒銇勯幘璺轰户缂佹劗鍋炵换婵嬫偨闂堟刀銏ゆ倵濮樺崬鍘寸€规洏鍎靛畷銊р偓娑櫱氶幏缁樼箾鏉堝墽鎮奸柟铏崌椤㈡艾饪伴崨顖滐紲闁荤姴娲﹁ぐ鍐焵椤掆偓濞硷繝鎮伴钘夌窞濠电偟鍋撻～宥夋⒑闂堟稓绠冲┑顔惧厴椤㈡瑩骞掗弮鍌滐紳闂佺ǹ鏈悷褔宕濆鍡愪簻妞ゆ挾鍋為崰妯尖偓瑙勬磸閸ㄤ粙鐛弽銊﹀闁稿繐顦扮€氬ジ姊绘担鍛婂暈缂佸鍨块弫鍐晲閸ヮ煈鍋ㄩ梻渚囧墮缁夌敻鎮￠弴銏＄厽婵☆垵顕ф晶顖涚箾閸喓鐭岄柍褜鍓濋～澶娒哄鈧畷婵嗏枎閹捐泛绁﹂梺鎼炲労閸撴岸宕戠€ｎ喗鐓曟い鎰Т閻忊晜顨ラ悙鏉戝闁哄矉绲鹃幆鏃堝灳閸愯尪绶熼梻浣呵归鍡涘箲閸ヮ剙绠栨俊銈呮噺閺呮煡骞栫划鍏夊亾閹颁焦楠勯梻鍌欐缁鳖喚绮婚幋锔藉亱闁规崘顕х粻鏍ㄤ繆閵堝懎鏆為柛鐘叉閺屾盯寮撮妸銉ヮ潾濡炪倧璐熼崝鎴﹀蓟閿濆顫呴柍杞拌兌娴狀參鏌ｉ姀鈺佺仯闁哥姵鐗犻獮鍐潨閳ь剟鐛幒鎳虫梹鎷呴崫鍕闂傚倷鑳剁划顖炲礉閺囥埄鏁嬫い鎾卞灩閻撴繈鏌熼崜褏甯涢柣鎾冲暟閹茬ǹ饪伴崼婵堫槶濠电偛妫欓幐鎼佸磼閵娾晜鐓涚€广儱鍟俊鍧楁煟濠靛棛鍩ｉ柡宀嬬節瀹曞爼濡烽妷褌鐥梻浣瑰▕閺€閬嶅垂閸ф钃熸繛鎴炃氶弨浠嬫煕閳╁喚娈㈠ù灏栧亾闂傚倷绀侀悿鍥敋瑜旇棟闁汇垻枪閽冪喐绻涢幋鐐茬劰闁稿鎹囬弫宥夊础閻愬弶娈ㄦ繝銏ｎ潐濞茬喎顫忔繝姘＜婵炲棙甯掗崢锟犳煛鐎ｅ吀绨奸柕鍥у楠炲鈹戦崶鑸碉紗婵°倗濮烽崑鐐哄礉濞嗘挾宓侀柡宥冨妿閻熺懓鈹戦悩鎻掝伀闁崇鍊栫换婵嬫偨闂堟稈鏋呭┑鐐板尃閸ヨ埖鏅為梺鍦濠㈡﹢寮告笟鈧弻娑㈠焺閸愮偓鈻嶉悷婊呭鐢寮查弻銉︾厱闁靛绲芥俊浠嬫煟濠垫劒绨绘い顏勫暣婵″爼宕橀妸褌鎮ｉ梻浣侯攰濞呮洟鏁嬪銈庡亜缁绘帞妲愰幒鎳崇喓鎷犲顔瑰亾閹惧绠鹃柟鐐綑閸ゎ剟鏌涢妸鈺€鎲剧€殿噮鍋婇、娑㈡倷鐎电ǹ骞楅梻濠庡亜濞诧箓骞愰崜褏涓嶉柡灞诲劜閻撶喖鏌ｉ弴鐐测偓濠氭偩閻㈢鍋撳▓鍨灕閻忓繑鐟╅獮鎴﹀礋椤掍礁顎撻梺鍝勬川閸犳劙藟閸懇鍋撶憴鍕闁挎洏鍨介妴渚€寮崼婵嗚€垮┑鐐叉閸旀牠顢欓弮鍫熲拻闁稿本鑹鹃埀顒傚厴閹虫宕滄担绋跨亰濡炪倖鐗楅崙鐟邦焽閺嵮€鏀介柣妯虹枃婢规﹢鏌涚仦璇插闁瑰弶鎮傚顕€宕煎┑鍫㈡毇闂備胶绮崝鏇㈠箹椤愶箑鍨傞柛灞剧◤娴滄粓鏌熼柇锕€鏋涚亸蹇曠磽娴ｉ潧濡兼い顓炲槻椤繐煤椤忓嫭宓嶅銈嗘尵閸嬫稒淇婂ú顏呪拺闁硅偐鍋涙俊鍧楁煛娴ｅ壊鐓奸柕鍡曠窔瀵噣宕煎┑鍫濆箰闂備礁鎲℃笟妤呭窗濮樿泛鍌ㄩ柟闂寸劍閻撶喖骞栭幖顓炵仯鐎光偓濞戙垺鐓涢柛娑卞枤缁犳绱掗鍡欑М鐎殿噮鍣ｅ畷濂告偄閸濆嫬绠伴梺璇查閸樻粓宕戦幘缁樼厓鐟滄粓宕滈悢椋庢殾濞村吋娼欓崘鈧銈嗘尵婵绮婇敃鍌涒拺缁绢厼鎳忚ぐ褏绱掗悩鍐茬仼缂侇喛顕ч埥澶愬閳锯偓閹峰姊洪崜鎻掍簽闁哥姵鎹囬崺濠囧即閻旂繝绨婚梺鍝勫€搁悘婵嬵敂椤忓棛纾肩紓浣诡焽缁犳﹢鏌涢悩璇у伐閾伙絾绻涢懠顒傚笡缂傚秴顦靛缁樻媴閸涢潧缍婇、鏍川閺夋垹鐤囧┑顔姐仜閸嬫挾鈧鍣崑濠傜暦濮椻偓椤㈡瑧鍠婃潏銊хП闂傚倷绶氬褔鎮ч崱娑樼疇闁规崘顕х粻娲煛閸ャ儱鐏柍閿嬪灴閺屾稑鈹戦崱妤婁痪闂佺ǹ琚崝鎴﹀蓟濞戙垹惟闁靛／鍛呫劑姊洪崫鍕妞ゆ垵顦悾鐑藉醇閺囩倣鈺冩喐韫囨稒鍎婇柛顐犲劜閳锋帡鏌涚仦鐐殤濠⒀勭〒缁辨帞鈧綆鍋勭粭褏绱掗纭疯含闁糕晪绻濆畷鎺戔枎閹寸姷鏆板┑锛勫亼閸婃牠宕濋幋锕€纾归柡鍥╁枔椤╅攱绻濇繝鍌氼仴濞存粍绮嶉妵鍕疀閹炬剚浠奸梺鍝勬４缁蹭粙鍩為幋锕€鐏崇€规洖娲ら悡鐔兼倵鐟欏嫭纾搁柛鏃€鍨块妴浣糕槈濡嘲鐗氶柟鑲╄ˉ閸撴繂鈻嶈箛娑欌拻闁稿本鑹鹃埀顒傚厴閹虫宕滄担鐟板幑闂佸壊鍋呭ú鏍偪閻愵剛绡€濠电姴鍊绘晶娑㈡煕鐎ｎ亶鍎旈柡宀嬬到铻ｉ柛顭戝枛閹偤鎮楃憴鍕矮缂佽埖宀稿璇测槈閵忊晜鏅濋梺缁樕戣ぐ鍐╂叏鎼淬劍鐓欐い鏍ㄧ矊閺嬫稒鎱ㄦ繝鍛仩闁逞屽墮濠€杈ㄦ叏閻㈢ǹ违闁告劦鍠楅悡鐘绘煕閹邦垰鐨洪柛鈺嬬稻閵囧嫰濮€閿涘嫭鍣紓浣虹帛缁诲牆鐣烽崼鏇炍╅柕蹇娾偓鎰佷紪闂傚倸鍊峰ù鍥х暦閻㈢ǹ绐楅柡宥庡幖绾捐鈹戦悩鍙夋悙闁告艾缍婇弻锝夊棘閸喗鍊梺缁樻尰濞茬喖骞冨Δ鍛櫜閹肩补鈧剚娼婚梻浣告惈椤戝棛绮欓幋锕€鐓橀柟杈剧畱閻忓磭鈧娲栧ú銈夋偂閻斿吋鈷戠紒顖涙礃濞呮梻绱掔紒妯肩畵闁伙絿鍏樻俊鎼佸煛婵犲啯娅撻梺鑽ゅ枑閻熴儳鈧凹鍓熼、鏃堝捶椤撶姷锛濋梺绋挎湰閼归箖鍩€椤掑嫷妫戠紒顔肩墛缁楃喖鍩€椤掑嫨鈧線寮介鐐茶€垮┑鐐村灱妞存悂寮查悙娴嬫斀闁绘劕寮堕ˉ鐐烘煙缁嬫寧鎲稿瑙勬礋瀹曠ǹ螖娴ｅ搫骞嶉梻浣筋潐椤旀牠宕伴弽顓溾偓鍌涚附閸涘﹦鍘介梺瑙勫劤椤曨參骞婇崶顒佺厸鐎光偓鐎ｎ剛袦闂佽鍠撻崹钘夌暦椤愶箑绀嬮柛顭戝亝閻︽棃姊婚崒娆戭槮闁硅姤绮嶉幈銊╂偨閸涘﹥娅囬梺闈涚箞閸婃洟鎷戦悢鍏肩叆婵犻潧妫欓崳铏繆閹绘帞澧﹂柡灞剧☉铻栭柛鎰╁妺濞岊亪姊虹拠鑼婵炲弶绮庡Σ鎰板箻鐎涙ê顎撻梺鍛婄箓鐎氬懘鏁愰崶鈺冿紲闁诲函缍嗛崑鍕倶閹绢喗鐓曢柍鐟扮仢閸旀粎鈧灚婢樼€氼厾鎹㈠☉銏″€锋い鎺戝€甸崑鎾寸節濮橆収妫呭銈嗗姂閸ㄦ椽骞栭幇鐗堢叆闁哄洦锚閳ь剚鐗滅划瀣吋婢跺﹦鐣鹃悷婊冾樀瀵劍绂掔€ｎ偆鍘藉┑鈽嗗灥濞咃綁鏁嶅鍡愪簻闁挎繂妫涢崣鈧梺鍝勭焿缂嶄線鐛Ο鍏煎磯閻炴稈鍓濋悗浼存⒒娴ｅ憡鎯堥柣顒€銈稿畷浼村冀椤撴壕鍋撴担鍓叉建闁逞屽墴楠炲啴濮€閳藉棙效闁硅壈鎻徊楣冨闯椤栫偞鈷掗柛灞剧懅椤︼箓鏌熷ù瀣у亾閹颁焦缍庨梺闈╁瘜閸樺ジ宕瑰┑鍫氬亾楠炲灝鍔氭繛璇х畱閻ｇ兘宕ｆ径宀€顔曢梺鐟扮摠閻熴儵鎮橀埡鍐ｅ亾閻熺増鍟炵紒璇插暣婵＄敻宕熼姘辩杸闂佸疇妗ㄩ懗鑸靛閸曨剛绠鹃悗鐢登圭敮鍫曟煟濡も偓濡繈宕洪悙鍝勭闁挎梻绮弲鈺冪磼缂併垹寮柡鈧柆宥呮瀬闁诡垎鈧弨浠嬫煥濞戞ê顏╁ù鐘櫆娣囧﹪顢曢姀鐙€浼冮梺璇″櫍缁犳牠骞冨⿰鍛┏閻庯綆鍋呭▍鍥⒒娴ｇ懓顕滄繛鎻掔Ч瀹曟垿骞橀崜浣猴紲闂侀€炲苯澧伴柍褜鍓ㄧ紞鍡涘磻娴ｅ湱顩叉繝濠傜墛閻撴瑩姊洪銊х暠闁哄鐩弻锛勨偓锝庝邯椤庢妫佹径瀣瘈濠电姴鍊绘晶銏ゆ煟閿濆棙銇濋柡宀嬬磿娴狅箓宕滆閸掓稑顪冮妶鍐ㄧ仾婵炶尙鍠栧顐﹀箛閺夊灝鑰垮┑鐐叉钃辩悮锝囩磽閸屾艾鈧兘鎳楅崼鏇炵；闁规崘顕х壕璺ㄢ偓瑙勬礀濞层劎绮堟繝鍌楁斀闁绘ê寮堕幖鎰版煢閸愵亜鏋涢柡灞界Ч婵＄兘鏁冮埀顒佹櫠椤栫偞鐓曟慨姗嗗墻閸庢梹顨ラ悙瀵稿⒈闁告帗甯″畷妤佸緞婵犱礁顥氶梻浣藉亹閳峰牓宕滃☉銏犳槬鐎广儱顦伴埛鎺楁煕鐏炲墽鎳嗛柛蹇撶焸瀵悂顢旈崨顐＄盎闂佺懓鐡ㄧ换鍐夐姀鈽嗘闁绘劖娼欐慨宥嗩殽閻愬澧紒妤冨枑缁绘繈宕熼娑欙紙婵犵數濮烽弫鍛婄箾閳ь剚绻涙担鍐叉祩閺佸嫰鏌熼悧鍫熺凡闁哄绶氶弻娑㈠焺閸愵亖濮囬梺缁樻尰濞茬喖骞冨鈧幃娆撳箵閹哄棙瀵栭梻浣芥〃閻掞箓鎮ч悩璇茶摕闁哄洨鍠撶粻楣冩煟閹伴潧澧柣婵囨礋濮婃椽骞庨懞銉︽殸闂佹悶鍔屽鈥愁嚕鐠囨祴妲堟俊顖炴敱閺傗偓闂備胶纭堕崜婵嬨€冭箛鏂款嚤闁稿本鍑瑰〒濠氭煏閸繈顎楀ù婊冨⒔缁辨帡鎮╅搹顐㈢３閻庢鍠涢褔鍩ユ径鎰潊闁绘鏁稿澶愭⒒娓氣偓閳ь剛鍋涢懟顖涙櫠鐎电硶鍋撶憴鍕缂傚秴锕ら悾椋庣矙鐠囩偓妫冨畷姗€濡歌閸嬪﹪姊婚崒娆戭槮闁硅绻濆畷婵嬫晝閸屾氨鐛ラ梺鍝勭▉閸欏繒绱為弽顓熺厱婵炴垶顭囬幗鐘绘煟閹惧磭绠婚柡灞剧洴椤㈡洟鏁愰崶鈺冩毇闂備線娼荤徊钘夛耿鏉堚晜顫曢柟鐑橆殔缁犳稒銇勯幘璺盒涘┑顔兼川缁辨挻鎷呴搹鐟扮缂備浇顕ч悧鎾荤嵁閸愨晝顩烽悗锝庡亽濡啫鈹戦悙娈挎缂傚牅鍗冲畷瑙勭鐎ｎ亞鐣哄┑掳鍊愰崑鎾淬亜椤愶絿绠炴慨濠呭吹閳ь剟娼ч幉锟犲疾閸忚偐绡€缁剧増菤閸嬫捇宕橀懠顒勭崜闂備礁鎲″褰掓偡閵夆晩鏁嬮柨婵嗩槸缁犵粯銇勯弮鍌楁嫛婵炵厧锕娲箮閼恒儲娈伴梺绋款儐閹瑰洭寮婚悢鍓叉Ч閹艰揪绲界粭锛勭磽娴ｈ櫣甯涚紒瀣笒椤洩绠涘☉妯碱槶閻熸粌绉瑰畷鍝勭暆閸曨兘鎷虹紒缁㈠幖閹冲氦顣跨紓鍌欑贰閸犳牕霉閻戣棄绀嗛柟鐑橆殔缁秹鏌涢銈呮瀻闁逞屽墰缁垱绌辨繝鍥舵晬闁绘劕鐡ㄩ悵宕囩磽娴ｉ潧濡兼い顓炲槻椤繐煤椤忓拋妫冨┑鐐村灱妞存悂顢撻崱娑欌拻闁稿本鑹鹃鈺呮倵濮樼厧澧撮柟顔藉劤閻ｏ繝骞嶉鑺ョ暦闂備線鈧偛鑻晶鎾煕閳规儳浜炬俊鐐€栫敮鎺楀窗濮橆剦鐒介柟閭﹀幘缁犻箖鏌涘▎蹇ｆ闁兼澘娼￠弻鏇㈠幢濡も偓閺嗭絿鈧娲忛崝宥咁焽韫囨稑绀堢憸蹇涘煟閵堝棛绡€闁汇垽娼у瓭闂佹寧娲忛崐婵嬪箖瑜斿畷鍗炩槈濞嗗繆鍋撻崸妤佺厱妞ゎ厽鍨垫禍鏍瑰⿰鍕煉闁哄瞼鍠栧畷顐﹀礋椤掑顥ｅ┑鐐茬摠缁秹宕曢幎瑙ｂ偓鏃堝礃椤斿槈褔鏌涢埄鍐剧劷闁宠绋撶槐鎾诲磼濮樻瘷锛勭磼閼搁潧鍝哄┑鈥崇摠缁楃喖鍩€椤掆偓椤曪綁骞橀纰辨綂闂佹枼鏅涢崯顖炴偟閺嶎厽鈷掑〒姘ｅ亾闁逞屽墰閸嬫盯鎳熼娑欐珷闁规鍠氱壕鍏笺亜閺傚灝鈷旈悽顖涚〒缁辨帞绱掑Ο鑲╃暤濡炪値鍋呯换鍫ャ€佸鈧幃娆撳级閸噮鍤欓梻鍌氬€搁崐鐑芥倿閿曞倸绠栭柛顐ｆ礀绾炬寧绻濇繝鍌滃缂佲偓閸愨斂浜滈煫鍥ㄦ尵婢с垻鈧娲樼划宀勫煘閹达附鏅柛鏇ㄥ亗閺夘參姊虹粙鍖℃敾闁搞劌鐏濋悾鐑藉即閵忊€虫濡炪倖甯婄粈浣规償婵犲洦鈷戦柛鎾村絻娴滄繄绱掔拠鑼㈡い顓炴喘瀵粙濡歌椤旀洟鎮楅悷鏉款棌闁哥姵娲滈懞閬嶅礂缁楄桨绨婚梺闈涱槶閸庤櫕鏅跺☉姘辩＜缂備焦顭囧ú瀛橆殽閻愬樊鍎旈柟顔界懅閹瑰嫭绗熼娑辨（婵犵绱曢崑鎴﹀磹瑜忓濠冪鐎ｎ亞顔愬銈嗗姧缁犳垿鎮″鈧弻鐔告綇閸撗呮殸闂佺粯鎼换婵嬪蓟濞戙垹鐒洪柛鎰剁細濞岊亞绱撴担闈涘闁告鍥ｂ偓鏃堝礃椤斿槈褔鏌涢幇鈺佸妞ゎ剙鐗撳娲箰鎼淬垹顦╂繛瀛樼矤娴滄繃绌辨繝鍕ㄥ亾濞戞瑯鐒介柣鐔风秺閺屽秷顧侀柛鎾寸懇椤㈡岸鏁愭径妯绘櫔闂侀€炲苯澧柣锝囧厴閹剝鎯斿Ο缁樻澑闂佽鍑界紞鍡樼濠靛姹叉い蹇撶墛閳锋帡鏌涚仦鍓ф噮妞わ讣绠撻弻鐔哄枈閸楃偘绨婚柣鎾卞€濋弻鏇熺箾閻愵剚鐝旈梺姹囧€ら崳锝夊蓟濞戞粠妲煎銈冨妼閹虫劗鍒掓繝姘兼晬闁绘劕顕崢鍗炩攽閻樼粯娑ф俊顐ｎ殜椤㈡梻鈧稒顭囩粻楣冩煙閻愵剙澧柣鎾炽偢閺岋紕浠﹂悙顒傤槹閻庤娲滈崢褔鍩為幋锕€鐐婇柤鍛婎問濡呯磽閸屾艾鈧兘鎮為敃鍌樷偓鍐幢濞嗘劖娈伴梺璺ㄥ枔婵挳鎷戦悢鍏肩厪濠电偟鍋撳▍鍛存煕濡ゅ嫭鐝柕鍥у缁犳盯骞橀幇浣风礉婵犵數鍋涘Ο濠囧储閸撗勵潟闁规儳顕悷褰掓煕閵夋垵瀚禍鑸电節绾版ɑ顫婇柛瀣瀹曨垶骞橀鍢夈儱霉閿濆懎顥忔繛灏栨櫊閹銈﹂幐搴哗闂佸憡妫戠粻鎴︹€旈崘顔嘉ч柛鈩冾殘閻熴劑鏌ｆ惔銏犲毈闁告挾鍠栭獮蹇涘箣閿旇棄浜滈梺绋跨箺閸嬫劙宕㈤幘缁樷拺閻庡湱濮甸妴鍐磼閳ь剚绗熼埀顒€顕ｉ幎鑺ュ€锋繛鏉戭儐閺傗偓婵＄偑鍊栭悧妤冪矙閹烘柧鐒婇柨鏇炲€归悡娆愩亜閺冨倸甯舵俊鎻掝煼閺屽秶鎷犻崣澶婃敪缂備胶濮甸惄顖炵嵁濞嗘挸绀冪憸搴ㄥ磻閸涱垳纾介柛灞剧懅椤︼箑顭胯缁瑥鐣烽幋锕€绠婚悗闈涙憸椤旀洘绻濋悽闈浶㈡繛灞傚€楃划濠氬冀閵娿倗绠氶梺闈涚墕閸婂憡绂嶆ィ鍐┾拺鐎规洖娲ㄧ敮娑㈡煙閻熺増鎼愰柣锝囨焿閵囨劙骞掑┑鍥ㄦ珦闂備椒绱徊浠嬫嚐椤栫偞瀚冮悗锝庡枟閳锋帡鏌涚仦鍓ф噮妞わ讣绠撻弻鐔烘嫚瑜忕弧鈧Δ鐘靛仦閸旀牠濡堕敐澶婄闁靛ě鍛倞闂傚倷绀佺紞濠囧磻婵犲洤鍌ㄥΔ锝呭暙閻撴鈧箍鍎遍幊澶愬绩娴犲鐓熸俊顖氭惈缁狙囨煙閸忕厧濮嶆慨濠冩そ瀹曪繝鎮欓弶鎴炵亷闂備礁鎼惌澶屾崲濠靛棛鏆﹂柛顐ｆ礀閻撴稑霉閿濆妫戦柟瑙勬礋濮婄粯鎷呯粙鎸庡€┑鐘灪閿曘垽骞冨Ο琛℃斀閻庯綆浜為崢鎾⒑閻熼偊鍤熼柛瀣〒缁鎮╃紒妯锋嫼缂備礁顑呯亸鍛村绩缂佹绠鹃柤纰卞墮閺嬫稓鈧鍣崑濠傜暦濮椻偓椤㈡瑩鎳為妷锔惧礁闂傚倷鐒﹂幃鍫曞磿濞差亝鍋傞柛顐ｇ箥閻掍粙鏌ｉ弬鍨倯闁抽攱鍨块弻鐔虹矙閸噮鍔夌紓浣插亾闁逞屽墴濮婅櫣绮欓崠鈥充紣缂傚倸绉撮敃顏勭暦閸濆嫧妲堥柕蹇曞Х椤斿﹤鈹戞幊閸婃捇鎳楅崼鏇炲偍闂侇剙绉甸悡鐔肩叓閸ャ劍绀€濞寸姵绮岄…鑳槺缂侇喗鐟╅悰顔界節閸パ咁槹濡炪倖鎸炬慨鐑芥晬濠婂嫮绠鹃弶鍫濆⒔閸掍即鏌熺喊鍗炰喊闁靛棗鍊块幊鏍煛閸愵亷绱茬紓鍌氬€烽悞锕傗€﹂崶顒€鐓€闁哄洨濮崑鎾诲垂椤愶絿鍑￠柣搴㈠嚬閸ｏ綁宕洪埀顒併亜閹烘埊鍔熺紒澶屾暬閺屾稓鈧綆浜濋ˉ銏ゆ寠濠靛枹褰掓偂鎼达絾鎲奸梺绋匡功閺佸骞冨畡鎵虫瀻闊洦鎼╂禒鍓х磼閻愵剙鍔ゆい顓犲厴瀵鈽夊Ο婊勬⒐閹峰懐鍖栭弴鐔轰簽闂傚倷鑳剁划顖涚瑹濡ゅ懎绐楅柡宥庡弿缂嶆牗绻濇繝鍌滃闁绘帒鐏氶妵鍕箳閸℃ぞ澹曢梻浣风串缁蹭粙宕查弻銉ｅ亼濞村吋娼欑粈瀣亜閹哄棗浜惧銈呭閹瑰洤顫忓ú顏勭畾鐟滃繒绮婚幎鑺ョ厵闁惧浚鍋呯亸浼存煙娓氬灝濡界紒鍌涘笧閳ь剨缍嗛崑鈧柟宄邦煼濮婅櫣绮欓幐搴㈡嫳闂佽崵鍣︽俊鍥╁垝婵犲洦鍋嬮柛顐犲灮椤旀洘绻濋姀锝嗙【妞ゆ垵妫涚划鍫⑩偓锝庡厴閸嬫挾鎲撮崟顒傤槰婵犵數鍋涢敃顏勵嚕椤愶箑绀冩い鏃囧亹閿涙粌鈹戦鏂よ€跨痪顓熸倐瀹曨剟鎮介崨濞炬嫽婵炶揪缍€婵倗娑甸崼鏇熺厱闁挎繂绻掗悾鍨殽閻愯尙绠婚柡浣规崌閺佹捇鏁撻敓锟�32闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崰姘熼埀顒勬倵濞堝灝鏋熼柟顔煎€搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛鐏炵硶鍋撳畷鍥ㄦ畷闂侀€炲苯澧寸€规洑鍗冲浠嬵敇閻愮數鏆柣鐔哥矋閺屻劑鎮鹃柨瀣檮闁告稑锕ゆ禍婊堟⒑閸涘﹦绠撻悗姘煎墮閳绘捇濡烽埡鍌楁嫽闂佺ǹ鏈悷褔藝閿曞倹鐓欓悹鍥囧懐鐦堥悗瑙勬礀濠€閬嶅箟閹绢喖绀嬫い鎺嗗亾鐎殿喖娼″娲捶椤撯剝顎楅梺鍝ュУ閻楃娀骞冮垾鎰佹建闁逞屽墴瀵鎮㈤崨濠勭Ф婵°倧绲介崯顖烆敁瀹ュ鈷戠紒瀣儥閸庢劙鏌涢弮鈧悷鈺呯嵁閸愩剮鏃堝川椤撶喎绁舵俊鐐€栭幐楣冨磹椤愶箑顫呴幒铏濠婂牊鐓忓鑸电☉椤╊剛绱掗悩鎰佺劷缂佽鲸甯楀蹇涘Ω閿曗偓闂夊秹姊洪悷鏉挎Щ闁硅櫕锚閻ｇ兘顢曢敃鈧粈瀣棯閻楀煫顏呯閸撗€鍋撻崗澶婁壕闂佸憡娲﹂崜娑㈠储闁秵鈷戦梻鍫熻儐瑜版帒纾块柟鎯版閺勩儵鏌ㄥ┑鍡樼闁稿鎸搁埢鎾诲垂椤旂晫浜俊鐐€ら崢楣冨礂濡警鍤曟い鎰剁畱缁犳稒銇勯幘璺轰户缂佹劗鍋炵换婵嬫偨闂堟刀銏ゆ倵濮樺崬鍘寸€规洏鍎靛畷銊р偓娑櫱氶幏缁樼箾鏉堝墽鎮奸柟铏崌椤㈡艾饪伴崨顖滐紲闁荤姴娲﹁ぐ鍐焵椤掆偓濞硷繝鎮伴钘夌窞濠电偟鍋撻～宥夋⒑闂堟稓绠冲┑顔惧厴椤㈡瑩骞掗弮鍌滐紳闂佺ǹ鏈悷褔宕濆鍡愪簻妞ゆ挾鍋為崰妯尖偓瑙勬磸閸ㄤ粙鐛弽銊﹀闁稿繐顦扮€氬ジ姊绘担鍛婂暈缂佸鍨块弫鍐晲閸ヮ煈鍋ㄩ梻渚囧墮缁夌敻鎮￠弴銏＄厽婵☆垵顕ф晶顖涚箾閸喓鐭岄柍褜鍓濋～澶娒哄鈧畷婵嗏枎閹捐泛绁﹂梺鎼炲労閸撴岸宕戠€ｎ喗鐓曟い鎰Т閻忊晜顨ラ悙鏉戝闁哄矉绲鹃幆鏃堝灳閸愯尪绶熼梻浣呵归鍡涘箲閸ヮ剙绠栨俊銈呮噺閺呮煡骞栫划鍏夊亾閹颁焦楠勯梻鍌欐缁鳖喚绮婚幋锔藉亱闁规崘顕х粻鏍ㄤ繆閵堝懎鏆為柛鐘叉閺屾盯寮撮妸銉ヮ潾濡炪倧璐熼崝鎴﹀蓟閿濆顫呴柍杞拌兌娴狀參鏌ｉ姀鈺佺仯闁哥姵鐗犻獮鍐潨閳ь剟鐛幒鎳虫梹鎷呴崫鍕闂傚倷鑳剁划顖炲礉閺囥埄鏁嬫い鎾卞灩閻撴繈鏌熼崜褏甯涢柣鎾冲暟閹茬ǹ饪伴崼婵堫槶濠电偛妫欓幐鎼佸磼閵娾晜鐓涚€广儱鍟俊鍧楁煟濠靛棛鍩ｉ柡宀嬬節瀹曞爼濡烽妷褌鐥梻浣瑰▕閺€閬嶅垂閸ф钃熸繛鎴炃氶弨浠嬫煕閳╁喚娈㈠ù灏栧亾闂傚倷绀侀悿鍥敋瑜旇棟闁汇垻枪閽冪喐绻涢幋鐐茬劰闁稿鎹囬弫宥夊础閻愬弶娈ㄦ繝銏ｎ潐濞茬喎顫忔繝姘＜婵炲棙甯掗崢锟犳煛鐎ｅ吀绨奸柕鍥у楠炲鈹戦崶鑸碉紗婵°倗濮烽崑鐐哄礉濞嗘挾宓侀柡宥冨妿閻熺懓鈹戦悩鎻掝伀闁崇鍊栫换婵嬫偨闂堟稈鏋呭┑鐐板尃閸ヨ埖鏅為梺鍛婁緱閸ㄨ櫕绋夊鍡欑闁瑰鍋愬Λ銊ф喐閻楀牆绗氶柡鍛叀閺屾稑鈽夐崡鐐寸仌缂佺虎鍘搁崑鎾剁磽閸屾艾鈧绮堟笟鈧棟闁汇垺娼屾径鎰闁煎憡顔栧鐔兼⒑鐟欏嫬绀冩い鏇嗗懎顥氶柛蹇氬亹缁犻箖鏌涢埄鍏狀亪鎮為悙顑跨箚鐎瑰壊鍠栭弸娑欍亜椤忓嫬鏆ｅ┑鈥崇埣瀹曞崬螖閸愌勬▕闂傚倷鑳堕、濠傗枍閺囩喓浠氭繝纰樻閸嬪棝宕版惔銊⑩偓鏃堝礃椤斿槈褔鏌涢埄鍐噧妞ゎ値鍛＝濞撴艾娲ら弸娑㈡煠濞茶鐏︽い銏″哺閺佹劖寰勬繝鍕幀濠电姰鍨煎▔娑⑺囨潏鈺冧笉婵ǹ鍩栭埛鎴犵磽娴ｅ顏呮叏閿曗偓闇夋繝濠傚閻帡鏌涢埞鎯т壕婵＄偑鍊栫敮濠囨倿閿斿墽鐭嗛柛顐犲劜閻撴瑦顨ラ悙鑼虎闁诲繐寮堕〃銉╂倷閸欏鏋犻悗娈垮枛婢у酣骞戦崟顖毼╃憸搴ㄦ倵椤曗偓濮婂宕掑锝囨箙闂佺ǹ顑呯€氼剛鍙呴梺鎸庢礀閸婂摜绮绘导瀛樼厵缂備降鍨归弸娑㈡煕閹寸姴啸闁逞屽墮缁犲秹宕曢柆宓ュ洦瀵肩€涙ê浜楅棅顐㈡处缁嬫捇宕ｉ幘缁樼厱闁靛鍨甸懟顖炲礈娴煎瓨鈷戦梺顐ゅ仜閼活垱鏅堕鐐寸厽婵°倕鍟埢鍫燁殽閻愭彃鏆ｉ柡浣规崌閹晠宕橀煫顓犵泿闂傚倷鑳舵灙缂佺粯顨婂畷鎴﹀箛閺夊灝鍋嶆繛瀵稿Т椤戝棝鍩涢幋锔界厱婵犻潧妫楅鈺呮煃瑜滈崜娆忥耿闁秴鐒垫い鎺戝€婚幊妤呮煕閵夋垵鎳愰弸鈧梻鍌欑缂嶅﹤螞閸ф鍊块柨鏇炲€归崑鍌涚箾閸℃ɑ灏伴柣鎾存礋閺岀喖鏌囬敃鈧獮鏍磼閼碱剛甯涢柕鍥у缁犳盯鏁愰崨顓犵潉闂備礁鎼惉濂稿窗閺嶎厹鈧礁鈻庨幘鏉戞闂侀潧鐗嗗ú銊╁储椤栨稐绻嗛柣鎰典簻閳ь剚鍨垮畷褰掓惞閸︻厾鐓撻梺纭呮彧缁犳垿鎮￠垾鎰佺唵閻犲搫銈介敓鐘冲亜闁绘挸楠稿鐑芥椤愩垺澶勬繛鍙夛耿椤㈡捇宕堕浣叉嫽婵炶揪绲块悺鏃堝吹濞嗘劖鍙忔慨妤€鐗忛悾鐢告煥濠靛牆浠︾€垫澘瀚换娑㈠箳閹寸偞鍎撳Δ妤婁簷閸楀啿鐣烽悡搴樻斀闁搞儮鏂侀崑鎾活敆閸曨剛鍘介梺缁樏鍫曞箹閹邦厺绻嗘い鏍ㄧ啲闊剛鈧娲滈弫璇差嚕閹绢喗鍊堕悹鍥皺缁夋椽鏌熼瑙勬珔妞ゆ柨绻橀、娆撴偂鎼粹槅妫熼梻鍌氬€峰ù鍥ь浖閵娧呯焼濞达綀顕氬ú顏嶆晣闁靛繒濮鹃幗鏇㈡⒑閹稿海绠撴い锔诲灣缁鈽夊▎宥勭盎闂佸湱鍋撻〃鍛村疮椤栨埃鏋嶆繛鎴欏灪閳锋帒霉閿濆懏鍟為柟鍙夋倐閺屾稓鈧綆鍓欓弸娑氣偓娈垮櫘閸嬪棝骞忛悩宸晠妞ゆ梻鏅粙浣虹磽閸屾艾鈧兘鎮為敃浣规噷濠电姵顔栭崹閬嶅箰閹惰棄钃熺憸鎴犵不濞戙垹鍗抽柣鏃堟敱閻ｎ剛绱撻崒娆戝妽妞ゃ劍鍔楅幑銏犫攽閸♀晜缍庡┑鐐叉▕娴滄粎绮昏ぐ鎺撶厽闁归偊鍘肩徊缁樸亜椤掆偓閻楁挸顫忓ú顏勫窛濠电姴鎳庨ˉ婵嗩渻閵堝棗鐏ユい锔炬暬閹即顢氶埀顒€鐣锋總绋课ㄩ柕澶婄岸閺呯娀寮诲鍡樺闁规鍠栭閬嶆偡濠婂啰效闁炽儻濡囬幑鍕Ω閿曗偓绾绢垶姊虹紒妯碱暡婵炲吋鐟﹂幈銊モ槈閵忊檧鎷洪梺鍛婄箓鐎氼喖顭囬妶澶嬬厱闁靛绲芥俊鐑芥煙闁垮銇濋柟顔煎槻楗即宕ㄩ褎姣夐梺姹囧焺閸ㄩ亶銆冩繝鍥ф瀬鐎广儱顦伴崑鍕煕韫囨艾浜归柛妯圭矙濮婅櫣娑甸崨顕呬哗闂佺ǹ顑囬崰鏍箖妤ｅ啯鍋ㄩ柛娑橈功閸橀潧顪冮妶鍡樼叆闁告ɑ绮撳畷鎴﹀箻鐠囧弶顥濋梺闈涱檧鐎靛苯顭囬弮鍫熲拻濞撴埃鍋撴繛浣冲嫷娈介煫鍥ㄦ礃椤洟鏌￠崶銉ョ仼闁稿被鍔戝娲敆閳ь剛绮旈悽鍛婂亗闁绘棁鍋愰崣鎾绘煕閵夛絽濡界紒鈧埀顒佺箾鐎电ǹ校閻㈩垳鍋熷Σ鎰板箳濡や礁浜归悗瑙勬礀濞诧箓宕抽弶娆炬富闁靛牆鍟悘顏呬繆椤愩垹鏆欓柣锝囧厴閹剝鎯斿Ο缁樻澑闂備礁澹婇崑鍛崲閸屾繍鏉烘繝鐢靛Х椤ｎ喚妲愰弴銏犵；闁硅揪绠戠壕褰掓煛瀹ュ骸浜濋柡鍡樼矊閳规垿鎮╅崣澶婎槱闂佹悶鍊曠€氫即寮婚妶鍥╃煓閻犳亽鍔嬬划闈涒攽閻愬弶鍣藉┑鐐╁亾濠殿喖锕ュ浠嬨€佸Δ鍛劦妞ゆ帒鍊婚惌鎾绘煟閵忕姵鍟為柛瀣€块弻娑㈠箛閸忓摜鍑归梺鍝ュУ閸旀牜鎹㈠┑鍥╃瘈闁稿本绮岄。铏圭磽娴ｆ彃浜鹃梺鍓插亞閸犳劙宕ｈ箛鏂剧箚妞ゆ牗姘ㄦ禒銏ゆ煟閹炬潙濮嶉柡灞剧洴婵″爼宕ㄩ婊冨П闂備礁鎼張顒勬儎椤栫偟宓佹俊顖欑秿閺冨牆鐒垫い鎺戝缁犵娀鏌熼弶鍨絼闁搞儺鍓﹂弫宥嗙箾閹寸偞鐨戦柣锕€鐗撳鍝勑ч崶褏浼堝┑鐐板尃閸愵亝鎳冩繝鐢靛Х閺佹悂宕戝☉銏″剳濞村吋娼欑壕瑙勭節闂堟侗鍎忕紒鐙€鍨堕弻鐔告綇妤ｅ啯顎嶉梺绋款儏鐎氫即寮诲☉婊呯杸婵﹩鍏涘Ч妤呮⒑閸涘⿵渚涘ù婊勭箘閹广垹鈹戠€ｎ偄浠洪梻鍌氱墛缁嬫劙骞嬮悜鑺モ拺缂佸顑欓崕鎰版煙閸涘﹥鍊愰柛鈹垮灩椤撳吋寰勬繝鍌濃偓鍨攽閻愭潙鐏﹂柣鐔濆洤鍌ㄩ梺顒€绉甸悡鐔煎箹閹碱厼鐏ｇ紒澶愭涧闇夋繝濠傚閻帗銇勯姀鈩冾棃妞ゃ垺锕㈡慨鈧柨娑樺楠炴劙姊虹拠鑼闁稿绋掗弲鑸电鐎Ｑ€鍋撻弽顓熷殐闁冲搫鍟伴敍婊堟⒑缂佹◤顏嗗椤撶伝铏光偓娑欙供濞堜粙鏌ｉ幇顖涘涧闁兼澘娼￠弻鐔兼偂鎼达絿楔濡炪們鍨哄ú鐔煎春濡ゅ懎鐓涘ù锝呭槻椤ユ碍淇婇悙顏勨偓鏍偋濠婂牆纾婚柣鎰惈閻撴洟鏌熸潏楣冩闁抽攱甯￠弻娑㈩敃閿濆浂鏆″┑鐐叉噹濞层劎妲愰幒鏂哄亾閿濆骸寮鹃柛銈嗙懇閺屽秶鎲撮崟顐や紝闂佽鍠楅悷褔骞忛悩渚Щ婵炴潙鍚嬮崝鏇⑩€旈崘顔嘉ч幖绮光偓鑼泿缂傚倷鑳剁划顖炲礉濡ゅ懎鐭楅柛娑卞弾濞撳鏌曢崼婵囧櫧缂佺姳鍗抽幃妤€顫濋梻瀵告晼濠碘€冲级閸旀瑥鐣锋總绋垮嵆闁绘柨寮剁€氬ジ姊绘担鍛婂暈缂佸鍨块弫鍐Ψ閿曗偓缁剁偤鏌涢弴銊ョ仭闁绘挻娲樻穱濠囶敍濠靛棗鎯為梺娲诲幗鐢帡鍩為幋锔绘晩闁荤喖顣︽竟鏇㈡⒒閸屾瑧鍔嶉悗绗涘懐鐭欓柟瀵稿Л閸嬫挸顫濋悡搴㈢彎濡炪們鍨洪悷鈺侇嚕閹绢喖顫呴柍閿亾闁归攱妞藉娲川婵犲嫮鐣甸柣搴㈣壘閸㈡彃宓勯悷婊呭鐢鎮￠弴銏＄厪濠电倯鍐仾婵絽鐗嗛—鍐Χ閸愩劎浠惧┑鈽嗗亜閸熸挳濡存笟鈧鎾閳╁啯鐝抽梻浣告啞濞诧箓宕㈡ィ鍐╁仼濡わ絽鍟埛鎺懨归敐鍛暈闁哥喓鍋ら弻锝夋偄閺夋垵濮﹂柦妯荤箖閵囧嫰寮介妸褉濮囬梺鍛婄懃缁绘﹢寮诲☉銏犖ㄦい鏃傚帶椤亪姊虹紒妯诲碍闁哥喐鎸抽獮鍐ㄧ暋閹佃櫕鐎婚棅顐㈡处閹尖晜绂掓總鍛娾拺閻犲洩灏欑粻鐗堛亜閺囥劌寮柛鈹惧亾濡炪倖甯掗崰姘焽閹邦厾绠鹃柛娆忣槺婢х敻鏌熼鍝勫姕缂佽櫣鏅划娆撳箰鎼粹懣姘舵⒒娴ｈ鐏遍柡鍛洴瀹曨垶骞嶉鐓幮氶梺閫炲苯澧存慨濠冩そ瀹曘劍绻濋崒姣挎洘绻涚€涙鐭嬫い銊ワ工閻ｇ兘骞嬮敃鈧粻铏繆閵堝嫯顔夐柟宄邦煼濮婅櫣绮欓幐搴㈡嫳闂佺厧缍婄粻鏍春閳ь剚銇勯幒鎴濐伌婵☆偅鍨剁换娑㈠幢閹邦剛浠煎銈庡亝缁诲嫰骞戦崟顖涙優闁荤喐婢樿婵犵绱曢崑鎴﹀磹閺嶎偅鏆滈柟鐑橆殔绾惧綊鏌涘☉姗堝姛妞も晠鏀辩换婵囩節閸屾粌顤€闂佺粯鎸炬慨鐢垫崲濞戙垺鍤戝Λ鐗堢箓濞堫參姊虹拠鏌ョ崪缂佺粯绻堝濠氭晸閻樻彃绐涘銈嗘尵婵挳鎮￠悢濂夋富闁靛牆鍟悘顏嗙磼鐎ｎ偄鐏撮柕鍫簼鐎靛ジ寮堕幋婵堢崺婵＄偑鍊栭悧鎾诲磹濡や焦鍎熷┑鐘叉处閳锋帒霉閿濆洨鎽傞柛銈嗙懇閺屾盯骞嬮悩铏彧缂備浇浜崑銈呯暦閸楃倣鐔哄枈鏉堛劑鏁滈梻鍌欒兌缁垶宕硅ぐ鎺戠柧妞ゆ劧绠戦惌妤呮煛閸モ晛鏋傚ù婊勭矒閺岀喖鎮滃Ο铏逛淮闂侀€炲苯澧伴柡浣筋嚙閻ｇ兘骞嬮敃鈧粻濠氭煠閹间焦娑ч柡瀣€垮娲川婵犲啫顦╅梺鍛婃尰閻╊垵妫熼梺闈涱槴閺呮粓鍩涢幋鐘垫／妞ゆ挾鍋為崳鍦偖濮樿京纾藉〒姘搐閺嬫稓绱掓径濠傤暢闁告帗甯￠崺锟犲川椤旈棿绨婚梻浣虹帛閹稿憡顨ョ粙璺ㄦ殼闁糕剝绋掗埛鎴︽⒒閸喓銆掔紒鐘插暱閳规垿顢欓幆褍骞嬮梺璇″枛濞尖€崇暦閻旂⒈鏁囬柣妯碱暜缁卞啿鈹戦悙鑸靛涧缂佸弶瀵ч悘娆戠磽娴ｅ搫鐝￠柛銉ｅ妿閸樹粙姊洪崘鍙夋儓闁挎洏鍊濋幃姗€鎮╅悙鎴掔盎濡炪倖鍔戦崹鑽ょ不閼碱剛纾肩紓浣诡焽缁犳挻銇勯锝囩疄妞ゃ垺顨婂畷鎺楁晜閹呭搸濠电姷鏁告慨鐑藉极閹间礁纾绘繛鎴烆焸濞戞ǚ鏋庨柟鎯х－椤︻喖鈹戞幊閸婃洟骞婃惔鈩冪函闂傚倷绀侀幖顐λ囬婊嗗С闂侇剙绉寸紒鈺伱归悩宸剱闁绘挾鍠栭弻鐔兼焽閿曗偓婢ь垶鏌嶇紒妯荤濞ｅ洤锕幊鐘筹紣濠靛洨绉风紓鍌欒兌缁垶鎯勯鐐茬疇闁绘ɑ妞块弫鍡涙煃瑜滈崜鐔奉嚕閸涘﹥鍎熼柕濠忓閸樹粙姊洪幐搴ｇ畵妞わ缚鍗冲顐︽焼瀹ュ棗鈧敻姊婚崼鐔衡棨闁稿鍨婚埀顒侇問閸犳牠鎮ユ總鍝ュ祦閻庯綆鍠楅崑鎰版煕椤垵浜濈紒棰濆亰濮婂宕掑顑藉亾妞嬪海鐭嗗〒姘ｅ亾妤犵偞鐗犻、鏇氱秴闁搞儺鍓﹂弫宥夋煟閹邦厽缍戦柍褜鍓濋崺鏍崲濠靛顥堟繛鎴濆船閸撲即姊洪崨濠呭缂傚秴锕濠氭偄鐞涒€充壕闁汇垻娅ョ憴鍕浄鐟滃繒妲愰幒妤€鐓㈤柍褜鍓熷畷鎴﹀箻缂佹ǚ鎷绘繛杈剧到濠€鍗烇耿娴犲鐓曢柕濞垮妽椤ュ銇勯鐐寸┛妞わ附濞婇弻锝呂旈埀顒勬煀閿濆拋娼栫紓浣股戞刊鎾煢濡警妲锋慨瑙勵殜濮婅櫣娑甸崨顔惧涧闂佸湱枪椤嘲顕ｉ弻銉ラ唶闁哄洨鍠庢禒濂告煟韫囨洖浠╂俊顐㈠閹ɑ绻濋崘顏嗙槇闂佹眹鍨藉褎鐗庨梻浣告啞閸ㄧ數绱炴繝鍌ゅ殨妞ゆ劑鍩勯崥瀣煕閳╁啰鎳呴柣锕€鐗婄换婵嬫偨闂堟刀銏ゆ煕婵犲倻绉虹€规洝娅曠粋鎺斺偓锝庡亞閸欏棗鈹戦悙鏉戠仸闁荤啙鍥ㄥ剹闁圭儤顨嗛悡娑氣偓鍏夊亾閻庯綆鍓涜ⅲ缂傚倷鑳舵慨鐢告儎椤栨凹鍤曢柟缁㈠枟閸婄兘鏌ｅΔ鈧悧鍡涙偂閺囥垺鈷掗柛灞剧懅椤︼箓鏌熺喊鍗炰喊妤犵偛绻掗埀顒婄秵閸犳牜澹曠拠娴嬫斀闁绘ê寮堕幖鎰磼閻樺磭澧甸柡灞稿墲瀵板嫮鈧綁娼ч崝宀勬⒑閹肩偛鈧牕煤閻旂厧绠栨俊銈呮噺閸嬶繝鏌熷▓鍨灓鐎规洦鍋婂濠氬磼濮橆兘鍋撻幖浣哥９闁归棿绀佺壕褰掓煙闂傚顦︾痪鎯х秺閺岀喖姊荤€靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崰姘熼埀顒勬倵濞堝灝鏋熼柟顔煎€搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛鐏炵硶鍋撳畷鍥ㄦ畷闂侀€炲苯澧寸€规洑鍗冲浠嬵敇閻愮數鏆柣鐔哥矋閺屻劑鎮鹃柨瀣檮闁告稑锕ゆ禍婊堟⒑閸涘﹦绠撻悗姘煎墮閳绘捇濡烽埡鍌楁嫽闂佺ǹ鏈悷褔藝閿曞倹鐓欓悹鍥囧懐鐦堥悗瑙勬礀濠€閬嶅箟閹绢喖绀嬫い鎺嗗亾鐎殿喖娼″娲捶椤撯剝顎楅梺鍝ュУ閻楃娀骞冮垾鎰佹建闁逞屽墴瀵鎮㈤崨濠勭Ф婵°倧绲介崯顖烆敁瀹ュ鈷戠紒瀣儥閸庢劙鏌涢弮鈧悷鈺呯嵁閸愩剮鏃堝川椤撶喎绁舵俊鐐€栭幐楣冨磹椤愶箑顫呴幒铏濠婂牊鐓忓鑸电☉椤╊剛绱掗悩鎰佺劷缂佽鲸甯楀蹇涘Ω閿曗偓闂夊秹姊洪悷鏉挎Щ闁硅櫕锚閻ｇ兘顢曢敃鈧粈瀣棯閻楀煫顏呯閸撗€鍋撻崗澶婁壕闂佸憡娲﹂崜娑㈠储闁秵鈷戦梻鍫熻儐瑜版帒纾块柟鎯版閺勩儵鏌ㄥ┑鍡樼闁稿鎸搁埢鎾诲垂椤旂晫浜俊鐐€ら崢楣冨礂濡警鍤曟い鎰剁畱缁犳稒銇勯幘璺轰户缂佹劗鍋炵换婵嬫偨闂堟刀銏ゆ倵濮樺崬鍘寸€规洏鍎靛畷銊р偓娑櫱氶幏缁樼箾鏉堝墽鎮奸柟铏崌椤㈡艾饪伴崨顖滐紲闁荤姴娲﹁ぐ鍐焵椤掆偓濞硷繝鎮伴钘夌窞濠电偟鍋撻～宥夋⒑闂堟稓绠冲┑顔惧厴椤㈡瑩骞掗弮鍌滐紳闂佺ǹ鏈悷褔宕濆鍡愪簻妞ゆ挾鍋為崰妯尖偓瑙勬磸閸ㄤ粙鐛弽銊﹀闁稿繐顦扮€氬ジ姊绘担鍛婂暈缂佸鍨块弫鍐晲閸ヮ煈鍋ㄩ梻渚囧墮缁夌敻鎮￠弴銏＄厽婵☆垵顕ф晶顖涚箾閸喓鐭岄柍褜鍓濋～澶娒哄鈧畷婵嗏枎閹捐泛绁﹂梺鎼炲労閸撴岸宕戠€ｎ喗鐓曟い鎰Т閻忊晜顨ラ悙鏉戝闁哄矉绲鹃幆鏃堝灳閸愯尪绶熼梻浣呵归鍡涘箲閸ヮ剙绠栨俊銈呮噺閺呮煡骞栫划鍏夊亾閹颁焦楠勯梻鍌欐缁鳖喚绮婚幋锔藉亱闁规崘顕х粻鏍ㄤ繆閵堝懎鏆為柛鐘叉閺屾盯寮撮妸銉ヮ潾濡炪倧璐熼崝鎴﹀蓟閿濆顫呴柍杞拌兌娴狀參鏌ｉ姀鈺佺仯闁哥姵鐗犻獮鍐潨閳ь剟鐛幒鎳虫梹鎷呴崫鍕闂傚倷鑳剁划顖炲礉閺囥埄鏁嬫い鎾卞灩閻撴繈鏌熼崜褏甯涢柣鎾冲暟閹茬ǹ饪伴崼婵堫槶濠电偛妫欓幐鎼佸磼閵娾晜鐓涚€广儱鍟俊鍧楁煟濠靛棛鍩ｉ柡宀嬬節瀹曞爼濡烽妷褌鐥梻浣瑰▕閺€閬嶅垂閸ф钃熸繛鎴炃氶弨浠嬫煕閳╁喚娈㈠ù灏栧亾闂傚倷绀侀悿鍥敋瑜旇棟闁汇垻枪閽冪喐绻涢幋鐐茬劰闁稿鎹囬弫宥夊础閻愬弶娈ㄦ繝銏ｎ潐濞茬喎顫忔繝姘＜婵炲棙甯掗崢锟犳煛鐎ｅ吀绨奸柕鍥у楠炲鈹戦崶鑸碉紗婵°倗濮烽崑鐐哄礉濞嗘挾宓侀柡宥冨妿閻熺懓鈹戦悩鎻掝伀闁崇鍊栫换婵嬫偨闂堟稈鏋呭┑鐐板尃閸ヨ埖鏅為梺鍦濠㈡﹢寮告笟鈧弻娑㈠焺閸愵亖濮囬梺缁樻尰濞茬喖骞冨鈧幃娆撳箵閹哄棙瀵栭梻浣芥〃閻掞箓鎮ч悩璇茶摕闁哄洨鍠撶粻楣冩煟閹伴潧澧柣婵囨礋濮婃椽骞庨懞銉︽殸闂佹悶鍔屽鈥愁嚕鐠囨祴妲堟俊顖炴敱閺傗偓闂備胶纭堕崜婵嬨€冭箛鏂款嚤闁稿本鍑瑰〒濠氭煏閸繈顎楀ù婊冨⒔缁辨帡鎮╅搹顐㈢３閻庢鍠涢褔鍩ユ径鎰潊闁绘鏁稿澶愭⒒娓氣偓閳ь剛鍋涢懟顖涙櫠鐎电硶鍋撶憴鍕缂傚秴锕ら悾椋庣矙鐠囩偓妫冨畷姗€濡歌閸嬪﹪姊婚崒娆戭槮闁硅绻濆畷婵嬫晝閸屾氨鐛ラ梺鍝勭▉閸欏繒绱為弽顓熺厱婵炴垶顭囬幗鐘绘煟閹捐泛鏋戝ǎ鍥э躬椤㈡稑鈹戦崶鏈靛摋濠电偛顕慨鐢电矙閹烘梹宕叉繛鎴欏灩闁卞洭鏌ｉ弮鍥仩闁汇倕瀚埞鎴︻敊绾攱鏁惧┑锛勫仩濡嫰鎮鹃悜绛嬫晝闁挎洍鍋撶紒鈧€ｎ喗鐓曟い顓熷灥閳ь剨缍侀、鏃堝礋閵婏附鏉搁梻浣虹帛閸旀洖螣婵犲伣锝囨喆閸曨厾顔曢梺鍛婄懃椤﹂亶鎯屾繝鍥х闂侇剙绉甸悡娆撴煙濞堝灝鏋涙い锝呫偢閺岋繝宕ㄩ鍓х杽闂佸搫鏈粙鎾寸閿曞倸绀堢憸澶愬几閺冨牊鈷戠憸鐗堝俯濡垹绱掗鍛仯闁瑰箍鍨归埥澶娾枎閹邦喚肖闂備礁鎲￠幐鍡涘礃閵娧冨笓闂傚倸鍊烽懗鍫曘€佹繝鍐╁弿闁靛牆娲ら崹婵嬫煙閻愵剙澧柛銈嗘礋閺岀喖姊荤€靛壊妲紓浣插亾闁告劦鍠楅悡鍐偠濞戞巻鍋撻崗鍛棜缂傚倸鍊峰ù鍥敋瑜嶈灋婵炲棙鎸告闂佸憡娲﹂崹鎵不濞戙垺鐓冮弶鐐村閸忓苯霉閻橀潧甯舵い顏勫暣婵″爼宕ㄩ缁㈡炊闂備礁鎲＄湁缂侇喗鐟╂俊鎾礃椤斿槈銊╂煃閸濆嫬鈧悂鍩€椤掑倸鍘撮柟顔肩秺瀹曞爼宕惰瀹告繈鏌涘▎蹇曅ょ紒杈ㄦ崌瀹曟帒鈻庨幒鎴濆腐濠电姵顔栭崰姘跺极婵犳艾绠氶柛鏇ㄥ灱閺佸秵鎱ㄥΟ鐓庡付婵炲牄鍔戝娲传閸曞灚效闂佹悶鍊х粻鎾愁嚕椤曗偓瀹曠厧鈹戦崼婵喰曞┑锛勫亼閸婃牜鏁繝鍥ㄥ殑闁割偅娲忛埀顒€鎳樺濠氬Ψ閿旀儳骞楅梻渚€鈧稑宓嗘繛浣冲嫭娅犳い鏍仦閻撴洘绻涢崱妤冪缂佺姵褰冭彁闁搞儜宥堝惈濡炪們鍨虹粙鎴︺偑娴兼潙绀冮柕濞垮€楃敮娑欑節閻㈤潧浠滈柣掳鍔庨崚鎺楀箻閸撲椒绗夐梺鍝勭▉閸樿偐绮婚弽銊ょ箚闁靛牆鍊告禍鎯ь渻閵堝骸浜濈紒璇茬墦楠炲啫鈻庨幙鍐╂櫌闂侀€炲苯澧存い銏℃閺佹捇鏁撻敓锟�
    if (!dimension_size->get_symbolEntry()->getType()->isInt())
    {
        fprintf(stderr, "i32 is needed, but %s is given \n",
            dimension_size->get_symbolEntry()->getType()->toStr().c_str());
    }



    if (arr1 != nullptr)
        arr1->typeCheck();
    if (arr2 != nullptr)
        arr2->typeCheck();
    if (dimension_size != nullptr)
    {
        if(dimension_size->cal_expr_val() == PRE_CAL_ERROR_MEETING_VAL)
            fprintf(stderr, "not a const \n");
        dimension_size->typeCheck();
    }
}
void ArrDimNode::genCode()
{

}

void DoNothingStmt::typeCheck()
{

    if (do_nothing_node != nullptr)
        do_nothing_node->typeCheck();
}
void DoNothingStmt::genCode()
{

}

void ContinueStmt::typeCheck()
{
    if (!whether_valid)
        fprintf(stderr, "continue not in loop \n");
}
void ContinueStmt::genCode()
{
    if (whether_valid && !loop_manager.empty())
    {
        BasicBlock* cond_bb;
        cond_bb = loop_manager.get_cond_bb();
        new UncondBrInstruction(cond_bb, builder->getInsertBB());
    }
}

void BreakStmt::typeCheck()
{
    if(!whether_valid)
        fprintf(stderr, "break not in loop \n");
}
void BreakStmt::genCode()
{
    if (whether_valid && !loop_manager.empty())
    {
        BasicBlock* end_bb;
        end_bb = loop_manager.get_end_bb();
        new UncondBrInstruction(end_bb, builder->getInsertBB());
    }
}

void WhileStmt::typeCheck()
{

    if (cond != nullptr)
        cond->typeCheck();

    if (doStmt != nullptr)
        doStmt->typeCheck();
}
void WhileStmt::genCode()
{
    Function* func;
    BasicBlock* loop_cond_bb, * loop_body_bb, * end_bb;
    //BasicBlock* now_bb = builder->getInsertBB();
    //婵犵數濮烽弫鍛婃叏閻戣棄鏋侀柛娑橈攻閸欏繘鏌ｉ幋锝嗩棄闁哄绶氶弻娑樷槈濮楀牊鏁鹃梺鍛婄懃缁绘﹢寮婚敐澶婄闁挎繂妫Λ鍕⒑閸濆嫷鍎庣紒鑸靛哺瀵鈽夊Ο閿嬵潔濠殿喗顨呴悧濠囧极妤ｅ啯鈷戦柛娑橈功閹冲啰绱掔紒妯哄婵犫偓娓氣偓濮婅櫣绮欑捄銊ь唶闂佸憡鑹鹃鍥╂閻愬搫绠ｉ柨鏃傛櫕閸橀亶姊洪棃娴ㄥ綊宕曢柆宥呯劦妞ゆ巻鍋撴い顓犲厴閻涱喗寰勯幇顒傤啇婵炶揪绲块幊鎾寸闁秵鈷戦柛鎾村絻娴滄繄绱掔拠鎻掝伃闁诡喚鍏樺濠氬Ψ閿旇瀚肩紓鍌欑椤戝棝顢栧▎鎾崇？闊洦鏌ｆ禍婊堟煥閺冨倸浜鹃柡鍡╁墰閳ь剚顔栭崰鏍€﹀畡閭﹀殨闁圭虎鍠楅弲顒勬煕閺囥劌骞楁繝銏″灴濮婅櫣鎷犻幓鎺戞瘣缂傚倸绉村Λ娆戠矉瀹ュ鍐€妞ゆ挾鍋熼敍娑㈡⒑鐟欏嫬绀冩い鏇嗗洤纭€闁规儼濮ら悡鐔镐繆椤栨艾鎮戦柡鍡忔櫊閺岋綁骞掗幋娆忎划闂佸搫鐭夌徊楣冨箚閺冨牜鏁嶆繝濠傛啗閿濆鈷戦柛婵勫劚鏍″銈冨妼閿曨亪鐛崘顔芥櫢闁绘ɑ褰冨畵鍡涙⒑缂佹ɑ鈷掔紒缁樼箘閼鸿京鎷犲顔藉瘜闂侀潧鐗嗗Λ娆戜焊閻㈠憡鐓曢柣妯荤叀閸欏嫰鏌熼鏂よ€垮┑锛勫厴婵＄兘宕橀鍛畬濡ゆ浜欓崡鍐茬暦閻旂⒈鏁囬柣妯虹仛濠㈡牠姊婚崒娆戭槮闁圭⒈鍋婇幆澶嬬附缁嬭法鐛ラ梺鍝勭▉閸樺綊鍩€椤戣法顦﹂柍璇查叄楠炴﹢寮堕幋婊勫亝闂備浇顕х换鎺楀磻閻愬搫纾块柛鎰ゴ閺嬪秵鎱ㄥ鍡楀箻缂佲檧鍋撴繝鐢靛仜閻楀棝鎮樺┑瀣嚑婵炴垯鍨洪悡銉╂煛閸ユ湹绨绘い鈺婂墴閺屾盯骞掗幘铏癁濡炪們鍨洪惄顖炲箖濞嗘挻鍤戞い鎺戭槸閺佹悂姊婚崒娆戭槮闁规祴鍓濈粭鐔肺旈崨顓炵€梺鎯ф禋閸嬩焦绂掑鑸电厵闁诡垱婢樿闂佺ǹ锕﹂弫濠氬蓟閵娿儮鏀介柛鈩冪懃閸ゎ剛绱撴担闈涘闁靛牊鎮傚濠氭晲婢跺娅滈梺鍛婃处閸嬪嫮绮婇灏栨斀妞ゆ梻銆嬮崝鐔虹磼椤曞懎鐏︽鐐茬箻瀹曘劑寮堕幋婵堢崺濠电姷鏁告慨鎾磹閻熸壋鏋旀慨妞诲亾婵﹦绮幏鍛驳鐎ｎ偆绉烽梻浣哄劦閸撴繂煤閻旂鈧礁顫濋幇浣光枌闂備焦瀵х换鍕磻濞戙垹鐓橀柟瀵稿Л閸嬫捇鏁愭惔妯轰壕妞ゆ挻绋掑▍濠囨煛鐏炵晫校婵炵⒈浜獮蹇曚沪閽樺鑵愬┑锛勫亼閸婃牠寮婚妸鈹у洭妫冨☉杈ㄧ稁濠电偛妯婃禍婵嬎夐崼鐔虹闁瑰鍋為惃鎴︽煟閵堝懎顏慨濠呮缁瑩宕犻埄鍐╂毎婵犵數鍋犵亸娆撳窗閺嶎厼绠栧ù鍏兼儗閺佸鏌嶈閸撴岸骞堥妸鈺佺劦妞ゆ帒瀚悡銉︾箾閹寸伝顏堫敂椤愶絻浜滈柡鍐ｅ亾婵炶尙鍠庨～蹇撁洪鍛画闂備緡鍙忕粻鎴濃枔閸洘鈷戠紒瀣儥閸庢垿鏌涢悩宕囧⒌闁绘侗鍠楃换婵嬪炊瑜忛ˇ銊ヮ渻閵堝棙鈷掗柡鍜佸亰瀹曘垽鏌嗗鍡忔嫼闁荤喐鐟ョ€氼剛绮堥崘鈹夸簻闁哄洨鍠撴晶顏呫亜椤撶偞鍠樻鐐叉喘椤㈡牠濮€閻樿寮伴悗瑙勬礃閸庡ジ篓閸岀偞顥婃い鎺戭槸婢ь噣鏌嶇憴鍕伌闁糕晛瀚板畷姗€濡搁妷銉愶妇绱撻崒娆掑厡濠殿喚鏁婚幆鍕敍濮樼厧娈ㄩ梺鍦檸閸犳宕愰悜鑺ョ厾缁炬澘宕晶顕€鎮介娑欏磳婵﹤顭峰畷鎺戭潩椤戣棄浜惧瀣捣閻棗銆掑锝呬壕濡ょ姷鍋為〃鍛淬偑娴兼潙閱囨繝闈涱儐閿涙姊绘笟鈧褔藝椤愶箑鐤炬繛鎴炶壘椤ユ岸鎮归崶銊с偞闁衡偓娴犲鐓熸俊顖濐嚙缁插鏌嶈閸撴瑩鈥﹀畡閭﹀殨闁规儼濮ら弲婵嬫煕鐏炲墽銆掗柛娆忔閳规垿鎮╃紒妯婚敪濠碘槅鍋呴悷鈺勬＂闂佺粯顭囬弫鍝ュ婵傚憡鐓冮梺娆惧灠娴滈箖姊虹粙娆惧剱闁圭懓娲ら～蹇曠磼濡顎撻梺鍛婄☉閿曘倝寮抽崼銉︹拺闁告稑锕ラ悘婵嬫煕濞戝崬鐏ｉ柨娑欑箓閳规垿顢欑憴鎺戞贡閸掓帡鎮欓浣稿伎闂佸憡鍔栫粊鎾绩娴犲鐓熸俊顖濇閿涘秵銇勯敐鍡欏弨闁哄苯绉归弻銊р偓锝庝簽娴煎矂姊洪崫鍕拱缂佸鍨块崺鐐哄箣閿曗偓楠炪垺淇婇妶鍜冩缂佽鲸鍨块弻锝夋倷閺夋垵姣堟繛瀛樼矤娴滄粓锝炶箛娑欐優闁革富鍘鹃悡鎾绘⒑缁夊棗瀚峰▓鏃傗偓娈垮枤婢ф鎹㈠☉姘ｅ亾濞戞瑯鐒介柣顓烇躬閺岋綁寮介銏犱粯闂佷紮绲介崲鑼弲濡炪倕绻愮€氼噣宕濋敃鈧—鍐Χ閸℃鐟愰梻鍌氬缁夌數绮嬪鍛牚闁割偆鍠撻崢鐢告⒑閸涘﹦绠撻悗姘煎枦閸婃挳姊洪懡銈呅ｉ柛鏃€顨嗛弲璺何旈崘鈺佸簥濠电偞鍨崹褰掓煁閸ヮ剚鐓熼柡鍥ㄤ亢閸忓矂鏌涢弮鍫缂佽鲸鎸婚幏鍛鐎ｎ亝鎳欓梺姹囧焺閸ㄧ晫鎹㈠┑瀣ュù锝囩《閺嬪酣鏌熼悙顒佺稇婵炲牊鍔欏铏光偓鍦У閵嗗啰绱掔拋鍦瘈鐎殿喖鐖奸獮鏍ㄦ媴閸︻厼骞楅梺纭呭閹活亞妲愰弴鐔侯浄婵炴垶鐟х弧鈧梺闈涚箚濡插懘鎮炴禒瀣厓鐟滄粓宕滃璺虹鐟滅増甯掔粣妤佷繆閵堝懎鏆熼柣顓烆樀閺岀喖鎮滃鍡樼暦闂佸搫鎳忕换鍫ュ蓟濞戞矮娌柣鎰靛墰濞堛倝姊烘导娆戠М缂佺姵鐗犻獮鍐ㄧ暋閹靛啿鐗氶梺鍛婂姦閸犳牕顕ｆ导瀛樷拺閻犲洠鈧櫕鐏嶅銈冨妼濡繃淇婇崼鏇熸櫜濠㈣泛锕﹂ˇ銊╂⒑閸愬弶璐￠柛瀣尵閳ь剚淇哄Λ鍕煘閹达附鍊烽柤鎼佹涧濞懷呯磽閸屾氨袦闁稿鎸搁埞鎴︽倷閸欏鐝旂紓浣瑰絻濞尖€愁嚕椤愶富鏁婇悘蹇旂墬椤秴鈹戦悙鍙夘棞缂佸纾懞杈ㄧ節濮橆厸鎷洪梻鍌氱墛閼冲棜銇愰幒鎴狅紵闂佸搫琚崕閬嶅触瑜版帗鐓曢柕澶樺枛婢ь垶鏌ｉ幘瀵告噭闁靛洤瀚板顒傛嫚閹绘帩鐎抽梺璇插閸戝綊宕板顒夊殨鐟滄棃宕洪埀顒併亜閹烘垵顏柍閿嬪灴閺屾稑鈽夊鍫熸暰婵犮垼娉涚€氼剟婀佸┑鐘诧工閹冲孩绂掓潏鈹惧亾鐟欏嫭绀冪紒顔芥尰娣囧﹪骞栨担鑲濄劍銇勯弮鍥棄闁伙附绮撳缁樼瑹閳ь剙顭囪閹囧幢濡炪垺绋戦埢搴ㄥ箣濠婂懎澧鹃梻浣圭湽閸ㄥ綊骞夐敓鐘冲亗闁哄洢鍨圭粻瑙勭箾閿濆骸澧柣蹇婃櫅閳规垿顢欓懖鈹囨煛瀹€鈧崰鏍€佸▎鎾充紶闁告洦鍘虹槐娆撴煟鎼淬値娼愭繛鍙夌墪閻ｇ兘顢楅崟顐ゅ幒闁瑰吋鐣崝宀€绮诲☉娆嶄簻闁瑰搫绉烽崗宀勬煃瑜滈崜鐔虹不閺嵮屾綎缂備焦顭囩弧鈧柟鑲╄ˉ閳ь剝灏欓惄搴㈢節閻㈤潧浠╂い鏇熺矌缁骞嬮敂鑲╃◤闂婎偄娲﹂崫搴㈢瑜版帗鐓欓柣鎴灻悘鈺呮煟閹炬潙鐏存俊顐㈡嚇椤㈡洟濮€閳ユ剚妲辩紓鍌欑椤戝棛鏁敓鐘叉槬婵炴垯鍨圭粻铏繆閵堝嫯鍏岄柛妯哄船椤啴濡堕崱妤€顫囬梺绋匡攻濞茬喎顕ｇ粙娆惧悑闁搞儮鏅濋敍婊堟⒑閸撴彃浜濇繛鍙夊灴楠炲銈ｉ崘鈺冨幐閻庡厜鍋撻悗锝庡墰琚︽俊銈囧Х閸嬬偟鏁垾鎰佸殨闁圭虎鍠楅崑鍡椕归敐鍫殐婵☆偄瀚板濠氬磼濞嗘埈妲梺姹囧€曞ú顓㈡晲閻愭潙绶為柟閭﹀墮閻庮參姊虹粔鍡楀濞堟梻绱掗悩宕囧⒌闁哄苯绉瑰畷顐﹀礋椤愶絾鐤侀梻浣呵归鍡涘箲閸パ呮殾闁绘垹鐡旈弫鍥煟閹邦厼濮傞柛銊︽そ婵＄敻宕熼姣笺劑鏌ㄩ弬鍨挃闁伙綀鍋愮槐鎾存媴娴犲鎽甸梺鍦归…鐑界嵁韫囨稑宸濋柡澶嬪灣缁卞爼姊洪崨濠冪闁糕晛锕﹀濠勬崉閵婏富娼熼梺鍦劋閺岋繝宕戦幘缁樻櫜閹肩补鈧尙鐩庢繝鐢靛仦閹矂宕伴弽顓炵厴闁硅揪闄勯弲顒勬煕閺囥劌澧慨锝呯墦閹鎲撮崟顒傤槶闂佸摜濮甸悧鐘诲Υ娴ｇ硶妲堟慨妤€妫欓崓闈涱渻閵堝棗鍧婇柛瀣崌閺屸剝鎷呴棃娑掑亾濠靛棭娼栫紓浣股戞刊鎾煕閿旇骞楅柤鍨喘濮婃椽骞栭悙鎻掝瀳濡炪値鍘奸崲鏌ヮ敋閿濆鏁冮柕蹇婃櫆濡椼劑鏌ｈ箛鏇炰户闁稿鎹囧畷鎶芥倻閼恒儮鎷婚梺绋挎湰閼归箖鍩€椤掍焦鍊愮€规洘鍔栭ˇ鐗堟償閿濆洨鍔跺┑鐐存尰閸╁啴宕戦幘鎼闁绘劕顕晶鍨亜閵忊剝绀嬮柡浣稿暣閸┾偓妞ゆ帒鍊搁ˉ姘辨喐閻楀牆绗氶柣鎾跺枛閺屾洝绠涙繝鍐ㄦ畽闂侀潻瀵岄崢濂杆夊顑芥斀闁绘ê纾。鏌ユ煟閹惧鎳囬柡灞剧洴楠炴ê螖閳ь剟骞婃惔锝囩當闁跨喓濮甸埛鎴︽煙閹澘袚闁轰線浜堕幃浠嬵敍閵堝洨鐦堥悗娈垮枦椤曆囧煡婢跺⿴娼╂い鎰剁到婵即姊绘担鍛婂暈闁圭ǹ妫濆畷鐔碱敃閿濆洤钂嬪┑鐘殿暜缁辨洟宕戦幋锕€纾圭憸蹇涘箯鐎ｎ喗鏅查柛銉㈡櫃缁楀姊虹憴鍕妞ゆ泦鍥ㄥ珔闁绘柨鍚嬮悡鐔兼煛閸愩劌鈧悂宕濈€ｎ喗鍊垫慨姗嗗幗閻ㄦ垿鏌熼崣澶屾憙缂佺姵鐩鎾倻閸℃﹩妫ラ梻鍌欒兌椤宕熼崹顐ゆ殾闂備胶绮幐鑽ょ矙閹达箑违闁圭儤鍩堝鈺傘亜閹哄秶鍔嶉柟顔界懇濮婃椽宕崟闈涘壉闂佺儵鏅╅崹鍫曠嵁濡ゅ懎閱囬柡鍥╁仜閸炪劌顪冮妶搴″幋闁逞屽墮绾绢厾绮斿ú顏呯厸濞达絿鎳撴慨宥団偓瑙勬礀閵堟悂骞冮妶鍡樺鐎瑰嫮澧楅幉銏ゆ⒒娴ｈ棄鍚归柛鐘冲姉閸掓帡宕奸妷銉ь攨閻庤娲栧ú銊︾▔瀹ュ鐓涚€规搩鍠栭張顒傜礊鎼淬劍鈷戦柟顖嗗懐顔婇梺纭呮珪閹搁箖鍩€椤掆偓閻忔岸鎮￠敓鐘茶摕闁跨喓濮撮悙濠囨煃鏉炵増顦风紒顔肩埣濮婃椽宕橀幓鎺撔╁銈忕畵濞佳囷綖韫囨拋娲敂閸滀焦顥堟繝鐢靛仦閸ㄥ爼鎮烽妷鈹у洭鍩￠崨顔规嫼闂佺粯鍨熼弲婊冣枍閹剧粯鐓曢柕濞垮€曞畵鍡樸亜閵忊€冲摵闁轰焦鍔栧鍕節閸曢潧鏅ｆ繝鐢靛仩閹活亞寰婃禒瀣婵犲﹤鐗婇崕鎾绘煕閺囥劌鐏￠柍閿嬪灴閺屾盯骞橀弶鎴犵シ婵炲瓨绮庨崑銈夊蓟濞戙垺鍋愰柧蹇ｅ亜绾炬娊鎮楃憴鍕８闁搞劋绮欓獮鍐╃鐎ｎ亜绐涙繝鐢靛Т鐎氼剟鐛Δ鍛拺缁绢厼鎳庨ˉ宥夋煙濞茶绨界€垫澘锕ラ妶锝夊礃閵婏箒绶㈤梻浣稿暱閹碱偊骞婃惔銊ョ闁绘鍋ㄦ禍婊堢叓閸ャ劍灏靛褎娲熼弻娑㈠箛閵娿儰澹曢梻鍌氬€峰ù鍥р枖閺囥垹绐楅柟鎯х摠閸欏繘鏌ｉ姀鈶跺綊鎮￠弴銏＄厵閺夊牓绠栧顕€鏌ｉ幘瀛樼缂佺粯绻堝Λ鍐ㄢ槈濮楀棔鎮ｉ梻浣告啞濮婄懓鐣濋幖浣歌摕闁挎稑瀚ч崑鎾绘晲鎼存繈鍋楅梺鍛婄懃鐎氫即寮婚垾宕囨殕闁逞屽墴瀹曚即骞樼拠鑼崶婵犵數濮村ú銈囨兜閳ь剟姊虹紒妯哄婵犮垺锕㈠鎼佸醇閻旇櫣鐦堥梺姹囧灲濞佳嗏叴闂備胶枪椤戝棝骞愰幖浣肝ラ柟鐑樺焾濞尖晠鏌ㄥ┑鍡樺櫢濠㈣娲樼换婵嗏枔閸喗鐏堥梺鎸庢磸閸庨亶鈥旈崘鈺冾浄閻庯綆鍓欑粊锕傛⒑瑜版帒浜伴柛妯圭矙瀹曟洟鎮㈤崗鑲╁弳濠电娀娼уΛ娆忣啅濠靛鐓涢柛鈩冨姇閳ь剚鐗滈幑銏犫攽鐎ｎ偄浠洪梻鍌氱墛缁嬫劙宕Δ鈧—鍐Χ閸℃顦ㄧ紓渚囧枛濞寸兘宕氶幒鎾剁瘈婵﹩鍓涢鎺楁倵鐟欏嫭绀€婵炶绠戦埢鎾淬偅閸愨斁鎷哄┑顔炬嚀濞层倝鎮橀鈧鎼侇敂閸喓鍘介梺鍦劋閸ㄨ绂掑☉銏＄厪闁搞儜鍐句純濡ょ姷鍋炵敮锟犵嵁鐎ｎ亖鏀介柛鎰╁妺婢规洟姊洪悡搴綗闁稿﹥娲熻棢婵ǹ鍩栭悡銏′繆椤栨瑨顒熸俊顐閹叉悂鎮ч崼婵堢懖闂佸搫鎳忛悡锟犲蓟濞戙垹唯闁挎繂鎳庨‖鍫濃攽閻愯尙澧曢柣顒冨亹濡叉劙骞樼€涙ê顎撻梺鑺ッˇ鍗炐掗姀銏㈢＝濞撴艾娲ゅ▍姗€鏌涢妸銊ョ厫濞ｅ洤锕幃鐣岀矙鐠侯煈妲规俊鐐€栭悧顓犲緤妤ｅ啫鐤鹃柨婵嗩槹閳锋垹绱撴担骞库偓鐐哄箣閿濆啩姹楅梺鍛婂姦閸犳艾鐣垫笟鈧幃妤呮晲鎼粹剝鐏嶉梺绋匡工閻忔氨鎹㈠☉銏犻唶婵犻潧鐗呴搹搴ㄦ⒑閸濆嫷鍎滈梻鍕婵＄敻宕熼姘祮闂佺硶鍓濋〃鍡涘箹閹扮増鐓熼柟缁㈠灙閸嬫捇骞囨担鍝勬暩闂佽崵濮撮幖顐ャ亹閸ф閱囬柕澶堝劦閸炶泛顪冮妶鍛闁绘锕幃锟犲即閻旇櫣顔曢梺鐟扮摠缁诲倿鎳滆ぐ鎺撶厽闊洤锕ゆ晶鑼磼缂佹绠炲┑顔瑰亾闂佺偨鍎辩壕顓熷鐏炶В鏀芥い鏃傘€嬫Λ姘箾閸滃啰绉鐐茬箻閹晝鎷犻懠顒夊斀闂備礁婀遍崕銈夊垂閸洘鍎婇柨婵嗘川绾句粙鏌涚仦鎹愬闁逞屽墮閹芥粎鍒掗弮鍫濈骇闁瑰疇娅曠粙鎴﹀煝鎼淬劌绠ｉ柣妯兼暩閸斿綊鏌ｆ惔锛勭暛闁稿酣浜堕獮濠冨緞閹邦剟鍞跺銈呯箰閻楀﹪鎮￠弴銏＄厸闁搞儯鍎辨俊鐓庮熆瑜滈崰姘跺焵椤掍緡鍟忛柛鐘崇墪椤洭鍨鹃幇浣圭稁濠电偛妯婃禍婵嬎夐崼鐔虹闁瑰鍋為惃鎴炪亜閺傚尅鏀荤紒缁樼箓閳绘捇宕归鐣屽蒋闂備礁鎲￠悷銉╁疮椤愶富鏁嬮柨婵嗩槸鎯熼梺鎸庢⒒閾忓骸霉閸曨垱鐓熼幖鎼灣缁夌敻鏌涚€ｎ亝顥㈢€规洏鍨虹粋鎺斺偓锝庡亐閹锋椽鎮峰⿰鍛暭閻㈩垱甯楃粋宥嗗鐎涙鍘遍梺闈浨归崐鏇炵暤閸℃稒鐓欐い鏍ㄨ壘閺嗭絿鈧娲﹂崑濠傜暦閻斿吋鍋ㄩ梻鍫熺⊕閺嗏晠姊洪懡銈呮瀾缂侇喖鐭傞弻濠囨晲婢跺﹦鐣鹃梺鍛婃处閸ㄥジ寮澶嬬厽闁归偊鍨伴拕濂告煙閼恒儲绀€闁宠鍨块幃娆撳级閹寸姳鎴烽梻浣规偠閸斿繘锝炴径鎰？妞ゆ劧闄勯埛鎺戙€掑锝呬壕濠电偘鍖犻崶銊︽珫闂婎偄娲︾粙鎴犲閸忚偐绡€闂傚牊绋掔粊鈺呮煃瑜滈崜姘躲€冩繝鍌ゆ綎濠电姵鑹剧壕鍏肩箾閸℃ê濮︽俊顖氬€荤槐鎾寸瑹閳ь剛娑甸幖浣哥獥婵ǹ椴搁～鏇㈡煟閹邦剦鍤熼柣鐔风秺閺屽秷顧侀柛鎾寸箓铻為柕蹇嬪€栭埛鎴︽偡濞嗗繐顏╃紒鈧崘鈺冪濠㈣泛顑囧ú瀵糕偓瑙勬礈閹虫捇鍩為幋锕€鐐婇柍鍝勫枤閸熷酣姊绘担铏广€婇柛鎾寸箞閺佸姊虹拠鏌ョ崪缂佺姵鎹囬獮鍐ㄎ旈埀顒勶綖濠靛鏁囬柣鎰级閹虫瑩姊绘担鍛婃儓闁活厼顦辩槐鐐寸瑹閳ь剟鎮伴鈧畷鍫曨敆娓氬洦顥堟繝鐢靛仦閸ㄥ爼鏁冮埡渚囩劷濞寸姴顑嗛埛鎺懨归敐鍥ㄥ殌闁崇粯娲熼弻锟犲焵椤掑嫭鐒肩€广儱鎳愰ˇ顔尖攽閻愬弶顥為柟鍛婃倐閹锋垿鎮㈤崗鑲╁幗闂佸搫鍟悧婊兾涢幋锔界厸闁糕剝顨忛崕鏃堟煛瀹€瀣埌閾绘牠鏌嶈閸撶喖骞冭瀹曞崬顪冪紒妯间簴婵犲痉鏉库偓鏇㈠箠韫囨稑纾归柣銏犳啞閸嬧剝绻涢崱妤冪妞ゅ浚浜炵槐鎺楀焵椤掑嫬绀冩い蹇撴閿涙粌顪冮妶鍡橆梿濠殿喓鍊曢悾宄扮暆閸曨剛鍘藉┑鐐村灥瀹曨剙鈻嶅鍥ｅ亾鐟欏嫭绀€闁绘牕銈搁妴浣肝旈崨顓犲姦濡炪倖甯掔€氼剟宕归崒鐐寸厱閻忕偛澧介埣銈吤瑰⿰鍕煉闁哄瞼鍠栭獮宥夋惞椤愶絿褰呴梻渚€鈧偛鑻崝顖炴煕濞戝崬娅樻繛鑲╁枛濮婂搫煤鐠佸磭鐩庨梺浼欑悼椤洟姊婚崒姘偓鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閹冣挃闁硅櫕鎹囬垾鏃堝礃椤忎礁浜鹃柨婵嗙凹缁ㄧ粯銇勯幒瀣仾闁靛洤瀚伴獮鍥敍濮ｆ寧鎹囬弻鐔哥瑹閸喖顬堝銈庡亝缁挸鐣烽崡鐐嶆棃鍩€椤掑嫬鐓曢柟鐑橆殕閳锋垹绱撴担濮戭亞绮閺岋繝宕担闀愬枈濡ょ姷鍋涢ˇ杈╁垝濞嗗繆鏋庨柟顖嗗嫬鈧垶姊绘担绋款棌闁稿甯掗…鍧楀焵椤掑倻纾介柛鎰ㄦ櫆缁€瀣叏婵犲偆鐓肩€规洘甯掗埢搴ㄥ箣椤撶啘婊堟⒒娴ｅ憡璐￠柍宄扮墦瀹曟垶绻濋崒婊勬濡炪倖鐗滈崕鎰板极閸愵喗鐓ラ柡鍐ㄦ处椤ュ霉濠婂牅鎲炬俊顐㈡嚇椤㈡洟濮€閳ユ剚妲辩紓鍌欑椤戝棝宕归崸妤€绠氶悘鐐跺▏閺冨牆宸濇い鏃€鍎抽獮鎰版⒒娴ｈ鍋犻柛搴灦瀹曟洟鏌嗗鍡椻偓鑸点亜韫囨挾澧涢柣鎾跺枛閺岀喖骞戦幇顓犮€愰梺鍝勵儐閻╊垶寮婚垾宕囨殕閻庯綆鍓涜ⅵ婵°倗濮烽崑娑樏洪鈧偓浣糕枎閹炬潙娈熼梺闈涱檧婵″洤危閹达附鈷掗柛灞剧懅椤︼箓鏌熺喊鍗炰簻閾荤偞淇婇妶鍛櫣闁诲繑濞婇弻銊╁即閻愭祴鍋撹ぐ鎺戠厱闁圭儤顨嗛悡鏇㈡倶閻愭潙绀冨瑙勶耿閺屽秷顧侀柛鎾村哺楠炲啴宕掑杈ㄦ闂佸湱澧楀妯肩不閻熸噴褰掓晲閸涱喗鍠愰梺鎸庣⊕閻燂附绌辨繝鍥ㄥ€风€瑰壊鍠栭崜鏉库攽閻愬樊鍤熼柛蹇旓耿楠炲棗鐣濋崟顐㈣€垮┑锛勫仜閻ㄧ兘宕戦幘璇茬闁规惌鍘介崓鐢告⒑閹稿海绠撻柟鍐茬У缁旂喖寮撮悢鍓佺畾闂侀潧鐗嗗ú銈呮毄闂備胶枪椤戝棝鎯勯婵囶棨闂備焦鐪归崹缁樼仚闂佸搫顑勯懗鍫曞焵椤掆偓缁犲秹宕曢柆宓ュ洭顢涘⿰鍛劶闁诲函缍嗛崑浣圭濠婂牊鐓欓柟浣冩珪濞呭懘鏌ｈ箛锝呮灈闂囧绻濇繛鎯т壕闂佸憡顨嗘繛濠傤嚕婵犳碍鏅查柛鈩兠崝鍛存⒑閻愯棄鍔氶柛鐔稿濞煎繘宕ㄧ€涙ǚ鎷虹紓浣割儐椤戞瑩宕曢幇鐗堢厵闁荤喓澧楅崰妯尖偓娈垮枦椤曆囧煡婢舵劕顫呴柣妯诲絻缁侇噣姊绘笟鈧褎鐏欓梺绋块叄娴滃爼鏁愰悙鍝勭厸闁稿本绮庨鏇㈡煟鎼达絾鏆╅弸顏堟鐎ｎ喗鈷戠紒瀣儥閸庢劙鏌涢弮鈧〃鍛祫闂佸湱澧楀妯肩不婵犳碍鍊垫繛鎴烆仾椤忓牜鏁侀柟鍓х帛閳锋垿鏌涘☉姗堟敾閻忓浚鍘鹃埀顒侇問閸犳盯宕洪弽顓炵柧闁靛濡囩弧鈧梺姹囧灲濞佳勭濠婂牊鐓熼幒鎶藉礉鎼淬劌绀嗛柟鐑橆殕閸嬫劙鎮归崶銊ョ祷闁诲寒鍙冮弻锝夋偐閻戞ǜ鈧啴鎮归埀顒勬晝閳ь剟鈥﹂崶顒€鍐€鐟滃寮ㄦ禒瀣厽婵☆垵娅ｉ弸鍐熆鐟欏嫷鐒介柣銉邯楠炲棜顦查柟顔藉灩缁辨帞绱掑Ο鍏煎垱閻庤娲栧畷顒冪亽闂佹儳绻橀埀顒佺⊕濡﹥绻濋悽闈浶ユい锝勭矙閸┾偓妞ゆ帒鍟惃娲煟椤撶喓鎳勯柟渚垮妽缁绘繈宕掑☉娆忓壍闂佺粯鎸堕崐婵嬪箖鐟欏嫭濯撮柛婵勫劵缁辩偤姊洪悜鈺傛珔婵炴挳顥撳Σ鎰板箻鐠囪尙锛滃┑顔缴戦惁鐑藉閵堝棛鍙嗛梺鍝勬处閿氶柍褜鍓氱换鍫ョ嵁閸愵喖绠氱憸蹇涘汲鐎ｎ喗鐓欏ù锝呭暞閻濐亪鏌￠崒妤€浜鹃梻鍌氬€烽懗鍓佹兜閸洖绀堟繝闈涱儐閸嬶繝鏌ㄩ弴妤€浜鹃梺宕囩帛閹瑰洤顕ｉ幘顔碱潊闁抽敮鍋撻柟閿嬫そ濮婄粯绗熼崶褌绨介梺绋款儐閻╊垶骞婇悢纰辨晬婵炴垶鐟﹂悵鐑芥⒑閸︻叀妾搁柛鐘愁殜閹繝寮撮姀锛勫帗闁荤喐鐟ョ€氼剟宕濋妶鍚ょ懓饪伴崨顓濆婵烇絽娲ら敃顏堝箖濞嗘搩鏁傞柛鏇樺妼娴滈箖鏌″搴′簼闁哄棙绮岄埞鎴︽偐閹绘帩浼€闂佹椿鍘煎Λ妤呭Υ閹烘埈娼╅柨婵嗘噸婢规洟鏌ｆ惔銈庢綈婵炲弶绮撳顐ｇ節濮樸儮鍋撴担鑲濇棃宕ㄩ鐙€妲规俊鐐€栫敮鎺楀磹閹间胶宓侀柛顐犲劜閳锋帒霉閿濆懏鍟為悹鎰剁節閺屾稓鈧綆鍋呯亸顓熴亜椤撶偞鎼愰柍璇查叄楠炴﹢宕楅崨顖滃€為梻鍌欑劍閸庡磭鎹㈤幇鏉垮偍婵犲﹤鐗嗙壕濠氭煙閻愵剚鐏遍柡瀣閺屾盯鈥﹂幋婵囩亶闂佽绻愮粔鐟邦潖閾忓湱纾兼俊顖濐嚙閻ㄦ垿姊洪柅鐐茶嫰婢ь喚鐥紒銏犲籍鐎殿喗濞婇弫鍐焵椤掑嫬鐓橀柟杈剧畱绾惧吋绻濊閹稿摜绱炴繝鍌滄殾闁跨喓濮寸粻顕€鏌ら幁鎺戝姢闁告ü绮欏娲传閸曨偅娈┑鐐额嚋缁犳捇骞婂▎鎴炲枂闁告洦鍓欏鍧楁煟閻斿摜鎳冮悗姘槻閳绘挸饪伴崟顒€寮挎繝鐢靛Т鐎氼喚鏁☉銏＄厵鐎瑰嫮澧楅崵鍥煙閻撳海绉洪柟顔惧厴楠炲秹顢氶崨顓濊繕闂傚倸鍊搁崐椋庣矆娓氣偓楠炲鍨鹃幇浣圭稁閻熸粎澧楃敮妤呭磻鐎ｎ喗鐓熸俊顖氱仢閻ㄦ椽鏌￠崱顓犵暤闁哄矉缍侀幃銏㈢矙濞嗙偓顥撻梻浣姐€€閸嬫挸霉閻樺樊鍎愰柣鎾存礃閵囧嫰骞囬崜浣瑰仹缂備胶濮甸幑鍥蓟閿濆牏鐤€闁瑰灝瀚崥顐⑩攽椤旂》鏀绘俊鐐舵閻ｇ兘鎮滅粵瀣櫍闂佹枼鏅涢崰姘跺焵椤掍緡娈滄慨濠冩そ瀹曨偊濡烽妷鎰剁稻閵囧嫰濡搁妷顖濆惈閻庤娲栭幖顐﹀煡婢舵劕顫呴柣妯活問閸氬懘姊绘担鐟邦嚋婵☆偂绀佽灋闁告洦鍓涢々閿嬨亜閹捐泛浜归柡鈧禒瀣厽闁归偊鍓欑痪褎銇勯妷锔剧煂缂佽鲸甯炵槐鎺懳熼崫鍕垫綒婵＄偑鍊戦崹鍝勎涢崘顔兼槬闁跨喓濮寸粈鍐┿亜韫囨挻锛旂紒杈ㄧ叀濮婄粯鎷呴挊澶夋睏闂佸啿鍢查悧鎾崇暦濠婂啠妲堟慨姗嗗幗濞堟澘鈹戦悙鏉戠仧闁搞劍妞介幃陇绠涘☉娆戝幈闂侀潧顦伴崹鐢稿窗濮橆厼顕遍柛鎰╁壆閺冨牊鍋愰梻鍫熺◥缁泛顪冮妶鍡楃仴闁硅櫕锕㈠畷娲倷閸濆嫮顓洪梺鎸庢磵閸嬫捇鏌ｉ幇顒婂姛闁瑰弶鐡曠粻娑樷槈濞嗗繋绮ч梻浣稿暱閹碱偊宕幍顔碱棜濞寸姴顑嗛悡鏇㈡倶閻愭彃鈷旈柟鍐叉嚇閹綊骞囬鍕ギ闂佸搫鏈惄顖炲箖閳轰胶鏆﹂柛銉戔偓閹蜂即姊绘担绛嬪殐闁哥噥鍋婂畷鐔碱敇閻樺灚顫岄梻鍌欒兌鏋悶姘煎亰瀹曟劙寮撮悙鈺傜亖濡炪倖鍔ч梽鍕煕閹达附鐓曟繛鎴烇公閺€璇差熆鐠哄搫顏柡灞剧洴婵℃悂鏁冮埀顒勊夌€ｎ剛纾兼い鏃€顑欓崵娆撴煃閽樺妲搁柍璇查椤撳ジ宕熼鐘虫櫒闂傚倸鍊峰ù鍥綖婢跺﹦鏆︽俊顖濄€€閺嬫牗绻涢幋鐑嗙劷闁瑰鍎遍埞鎴炲箠闁稿﹥顨嗛幈銊╂倻閽樺锛涢梺缁樺姈娴滀粙鍩€椤掆偓閸燁垳鎹㈠┑瀣＜闁靛牆妫涚粙浣虹磽閸屾艾鈧兘鎮為敃浣规噷濠电姵顔栭崹閬嶅箰閹惰棄钃熸繛鎴炃氬Σ鍫熸叏濡も偓閻楁挾娆㈢€靛摜纾肩紓浣靛灩楠炴劙鏌涚€ｎ偅宕屾慨濠傤煼瀹曟帒鈻庨幒鎴濆腐闂佸搫顑愭禍鐐垫閹烘惟闁挎柨顫曟禒銏ゆ倵鐟欏嫭绀冮柛鏃€鐟ラ锝夊箻椤旇棄浜归梺鍦帛鐢偤寮昏缁绘繈鎮介棃娴躲儲銇勯敐鍫燁棄閸楅亶鏌涘☉娆愮稇闁搞劌鍊搁湁闁稿繐鍚嬬紞鎴︽偡濞嗘瑧鐣甸柟顔筋殜閺佹劖鎯旈埄鍐邯缂傚倷璁查崑鎾绘煕鐏炲墽銆掔紒鈾€鍋撳┑鐘垫暩婵挳宕愰幖浣哥畺闁冲搫鎳忛悡娑㈡煕閹扳晛濡垮褎鐩弻锟犲川椤旀儳寮ㄩ梺鍝勬湰缁嬫垿鍩ユ径鎰闁绘劘灏欓鎴炵節閻㈤潧浠滈柣妤€锕幃锟犲灳閹颁焦缍庨梺鎯х箰濠€杈╁閸忛棿绻嗘い鏍ㄧ箥閸ゆ瑩姊绘径濠勑ф慨濠冩そ閹筹繝濡堕崨顔惧綒缂傚倷鑳舵慨鐢稿箰缂佹鈹嶅┑鐘叉处閸嬨劎绱掔€ｎ厽纭舵い锔芥緲椤啴濡堕崱娆忣潷濠殿喗菧閸旀垿骞嗛崼銉ョ妞ゆ牗绋堥幏铏圭磽娴ｅ壊鍎愰悗绗涘喛鑰块柟娈垮枤绾惧吋銇勯弮鍥т汗閺佸牆螖閻橀潧浠滄い锔诲灥閻忔帡鏌ｉ悙瀵糕枔闁告挻绻堝顒勫焵椤掑嫭鈷掑ù锝堟鐢盯鏌涢弮鈧…鍥ㄧ缁嬪簱鏋庨柟閭﹀幗閻濆嘲鈹戦悙鏉戠仸婵ǜ鍔庢竟鏇㈠礂缁楄桨绨婚梺鍝勫€介敓銉╁磻閵忋倖鐓曟繛鍡楃箰椤忣參鏌＄仦鍓ф创闁糕晝鍋ら獮鍡氼槺濠㈣娲栭埞鎴︽晬閸曨偂鏉梺绋匡攻閻楁粓寮鐣岀瘈闁告洦鍓﹂崑銊╂⒑鐠恒劌鏋斿┑顔芥尦閹潡顢氶埀顒勬偂椤愶箑鐐婇柕濞р偓婵洭姊虹紒妯诲鞍婵炶尙鍠栧濠氭晲閸涘倻鍠栭幖褰掓儌閸濄儳顦繝鐢靛Л閹峰啴宕ㄩ鑺ョ槗闂備礁鎼惉濂稿窗閺嶎厹鈧礁鈻庨幘鍏呯炊闂佸憡娲﹂崑鍕綖濮樿京纾介柛灞剧懄缁佺増绻涢崪鍐М鐎殿喗鐓￠、鏇㈠Χ閸涱厾浜版繝鐢靛仜濡瑩骞愰幖浣稿瀭闁稿本绋忔禍婊堟煛閸愩劎澧涢柡鍡樼矒閺屾稓鈧綆鍋呭畷灞俱亜閵忊槄鑰块柟顔哄灲瀹曟粍鎷呴搹瑙勬瘞闂傚倸鍊风欢姘焽瑜旈幃褔宕卞▎鎰簥闂佸湱鍎ら〃鍛矆閸℃褰掓偂鎼达絾鎲奸梺缁樻尰濞茬喖寮婚敐澶婂嵆婵﹢纭稿Σ顔界箾鐎涙鐭嗛柛妤佸▕瀵鎮㈢喊杈ㄦ櫖濠殿喗锚閸熷潡顢旈銏＄叄缂備焦顭囨晶鐢告煛鐏炵晫啸妞ぱ傜窔閺屾盯骞樼捄鐑樼亪闂佹悶鍔戠粻鏍极閸愵喖纾兼慨妯垮煐閺嗗懏绻濆閿嬫緲閳ь剚鎸剧划鍫熸媴缁洘鐏佸┑鐘绘涧椤戝棝宕愰悽鍛婄厽闁靛繈鍊栧☉褎銇勯敐鍛倯闁靛洤瀚版俊鎼佸Ψ瑜忛澶岀磽娴ｇǹ鈧摜绮旈幘顔肩厴闁硅揪绠戦柋鍥煏韫囧鐏柛鏂跨秺濮婄粯鎷呴崨濠呯闂佺ǹ绨洪崐婵嗙暦闂堟侗娼╂い鎴犲仦濡炰粙銆侀弬娆惧悑闁糕剝蓱閻濇洟姊婚崒娆戭槮闁规祴鍓濈粭鐔肺旀担渚锤濠电娀娼ч悧鍐磻閹炬剚娼╂い鎴炲劤娴狀喖鈹戦纭峰姛缂侇噮鍨堕獮蹇涘川閺夋垳绱堕梺鍛婃处閸嬪倻鍒掗娑楃箚闁绘劦浜滈埀顑惧€濆畷鎴﹀川椤掑吋妞介幃銏☆槹鎼淬垺顔曢梻浣告贡婢ф顭垮鈧畷锟犲箮閼恒儳鍘介梺褰掑亰閸撴稒绂掑☉銏＄厵闁告劖褰冮弳鐐电磼缂佹绠為柟顔荤矙濡啫霉闊彃鍔﹂柡灞稿墲閹峰懐鎲撮崟顐熷亾閹邦喒鍋撶憴鍕闁搞劌娼￠獮鍐ㄢ枎韫囷絽鏅犲銈嗘⒐閸庤櫕鎱ㄩ悜妯肩瘈闁汇垽娼ф禒婊勪繆椤愶絿鎳囨鐐村姍瀹曨偊濡烽妷顔筋棥闁荤喐绮庢晶妤冩暜濡ゅ懏鍋傞柡鍥╁枂娴滄粍銇勯幘璺烘瀻闁诲繈鍎甸弻锛勨偓锝冨妼閳ь剚绻堝濠氭晸閻樻彃鑰垮銈嗘尵婵兘寮抽妶澶嬧拺闁告繂瀚﹢鐗堢箾閸欏绠樼紒顔款嚙閳藉顫濋敐鍛闂佹寧绻傜花鑲╄姳閹绢喗鐓曢悗锝庡亝瀹曞瞼鈧鍠曠划娆撱€佸鈧幃娆撳矗婢跺﹤濮冮梻浣筋嚙妤犳悂宕㈠⿰鍫濈；闁瑰墽绮悡鏇㈡煛閸ャ儱濡煎褌鍗抽弻娑橆潨閳ь剛绮婚弽褜娼栨繛宸簻瀹告繂鈹戦悩鎻掓殭闂傚绉归幃宄扳堪閸曨剛鍑￠梺鍝ュ枑濞兼瑩鎮鹃悜钘夘潊闁靛牆鎳庣粣娑欑節閻㈤潧孝闁哥噥鍋勯锝夊锤濡や胶鍘介梺缁樻煥閹芥粓骞婇崘顔藉€垫慨姗嗗墮濡插鏌嶇紒妯诲碍妤楊亙鍗冲畷鐔碱敆閳ь剙顕ｉ幐搴ｇ瘈闁汇垽娼у瓭闂佺懓鍟跨换姗€銆侀弮鍫熷亹闁汇垻鏁搁敍婊堟煛婢跺﹦澧戦柛鏂跨灱缁參骞掑Δ浣瑰殙闂佹寧绻傞ˇ浼存偂閺囩喓绡€闂傚牊绋掗ˉ婊勩亜韫囨挾鎽犵紒缁樼洴瀹曟宕楅悡搴ｏ紦婵犳鍠栭敃锔惧垝椤栫偛绠柛娑欐綑瀹告繂鈹戦悩鍙夊櫧妞ゃ垹鎳樺缁樻媴缁嬫寧姣愰梺鎼炲灪閻擄繝骞冭楠炴ê鐣烽崶銊︻啎闂備胶鎳撻顓㈠磻閻斿搫濮柍褜鍓熷铏圭矙閹稿孩鎷辩紓渚囧枛缁夋挳鍩㈤幘娣亝闁告劏鏂侀幏娲⒑閸︻厐鐟懊规搴ｄ笉闁瑰墽绻濈换鍡樸亜閹板墎鎮奸柕鍡樺笧閳ь剚顔栭崯顐﹀炊瑜忛崝锕€顪冮妶鍡楃瑨閻庢凹鍙冮崺娑㈠箣閿旂晫鍘卞┑鐐村灦閿曨偄顔忛妷銉㈡斀妞ゆ柣鍔岄幊鎰娴犲鐓曢悘鐐插⒔閳洟鏌ｉ妸锕€鐏撮柡灞诲€栭妶锕傚锤濡も偓閹界數绱撴担铏瑰笡缂佽鍊块敐鐐测攽鐎ｅ灚鏅ｅ┑鐐村灦閻熴儵顢旇ぐ鎺撯拻闁稿本鐟чˇ锕傛煙鐠囇呯？闁瑰箍鍨藉畷鎺戔攦閹傚濠殿喗顭囬崢褍鈻嶅澶嬬厱闁崇懓鐏濋悘顏嗏偓鍨緲鐎氫即鐛崶顒夋晣闁绘ɑ褰冪粻鐐烘⒒娴ｇ瓔鍤欓柛鎴犳櫕缁辩偤宕卞Ο纰辨锤濠电姴锕ら悧婊堝极閸屾稏浜滈煫鍥ㄦ尰椤ョ姷鐥幆褋鍋㈤柡灞剧〒娴狅箓宕滆濡插牆鈹戦悙瀛樺剹闁革綇缍佸濠氬焺閸愨晛顎撶紓浣割儐鐎笛冣枔婵犳碍鈷戠紒顖涙礃閺夊綊鏌涚€ｎ偅灏い顏勫暣婵″爼宕卞Δ鍐噯闂佽瀛╅崙褰掑储閸撗冨灊濠电姵鑹剧粻铏繆閵堝嫯顔夐柟椋庣帛缁绘稒娼忛崜褏袣濠电偛鎷戠徊鍧楀极椤斿皷妲堟繛鍡楃С缁ㄨ顪冮妶鍡樺暗闁稿鍋ゅ畷鏉课熷Ч鍥︾盎闂侀潧绻嗗Σ鍛枔濠婂應鍋撳▓鍨灈妞ゎ參鏀辨穱濠囨倻閼恒儲娅囬梺閫炲苯澧柍缁樻尭閻ｆ繈宕熼鑺ュ缂傚倷绀侀ˇ顖毼涘Δ鍐ｅ亾濮橆剙妲婚柍缁樻崌瀵挳濮€閿涘嫬骞楅梻浣筋潐閸庢娊顢氶銏犵疇闁告洦鍨遍悡蹇涙煕閵夋垵鍠氭导鍐倵濞堝灝娅橀柛锝忕秮瀹曟椽鍩€椤掍降浜滈柟鐑樺灥椤忊晝鐥幆褎鍋ラ柡宀€鍠栭獮鍡涙偋閸偅顥夐梻浣呵圭悮顐﹀礉閹达箑绠栨俊銈呮噺閸嬨劑鏌ｉ姀銏犱化闁逞屽墮閿曨亪寮诲☉姘ｅ亾閿濆骸浜濋悘蹇ｅ弮閺屽秶绱掑Ο璇茬３閻庤娲栫紞濠囥€佸▎鎾村仼閻忕偛鍚嬮崰姘節閻㈤潧啸闁轰焦鎮傚畷鎴濃槈閵忕姾袝闁诲函缍嗛崰妤呭磻閿熺姵鐓冮柛婵嗗閸ｆ椽鏌￠崨顔惧弨闁哄矉缍侀獮鍥敇閻橆喖浠﹂梻浣姐€€閸嬫挸霉閻樺樊鍎愰柣鎾存礋閺屾洘寰勯崼婵嗩瀳闂佺粯鎸婚崝妤冩閹炬剚鍚嬮柛鈩冾殘娴煎苯鈹戦纭锋敾婵＄偠妫勯悾鐑筋敃閿曗偓缁€瀣亜閹炬剚妲哄┑鐐╁亾闂佸搫鏈惄顖炵嵁濮椻偓瀹曟﹢鎳犵捄铏瑰幋濠德板€栭〃蹇旂濞嗘挸纭€闁告劘灏欓弳锕傛煟閺冨倵鎷￠柡浣告喘閺岋絽螣閾忛€涚驳濡炪値鍋勯惌鍌氼潖婵犳艾纾兼繛鍡樺灥婵′粙姊哄ú璁崇盎缂傚秴锕ら悾鐤亹閹烘挸鐧勬繝銏ｅ煐椤洭宕濋悜鑺ュ仭婵犲﹤瀚悘鎾煕閳轰焦鍤囨鐐达耿椤㈡瑧鎲撮敐鍡楊伜婵犵數鍋犻幓顏嗗緤娴犲绠犳繛鍡樺竾娴滃綊鏌熼悜妯肩畺闁哄懐鍋ゅ铏瑰寲閺囩偛鈷夌紓浣割儐閸ㄥ湱鍒掓繝姘骇閹煎瓨鎸婚弬鈧梻浣虹帛閿氶柣銈呮处缁旂喎螣娓氼垱绂嗛梺鍛婄☉閻°劑鍩涢幋锔界厱婵炴垶锕弨璇差熆鐠哄搫顏柡灞剧洴閹晠宕橀幓鎺撴嚈闂備浇顕栭崰鏍ь焽閿熺姴鏄ラ柍褜鍓氶妵鍕箳閹存繍浠鹃梺鎶芥敱閸ㄥ潡寮诲☉銏℃櫆閻犲洦褰冪粻瑙勭箾鐎涙鐭岄柛瀣尵閹广垹鈽夐姀鐘殿槯闂佸吋绁撮弲婊堝闯娴煎瓨鍊甸悷娆忓缁€鍐煕閵娧勬毈妞ゃ垺蓱缁虹晫绮欓崹顔库偓鍨攽閻愬弶顥為柛鈺佺墦瀹曨垱鎯旈妸锔规嫼闁荤姴娲﹁ぐ鍐敆閵忋垻纾奸悗锝庡亜濞搭噣鏌嶉妷顖滅暤鐎规洖鐖奸、妤佹媴缁嬪灝顥楁繝鐢靛Х閺佸憡鎱ㄧ€电硶鍋撳鐓庢灓濠㈣娲熼幃鍓т沪缁嬪じ澹曞Δ鐘靛仜閻忔繈宕濆鑸电厵闂佸灝顑呴悘鎾煟閹垮啫澧存い銏☆殜瀹曟帒螖閳ь剚绂嶉悙顒傜闁割偅绻勬禒銏ゆ煛鐎ｎ偆鈯曢柕鍥у椤㈡﹢鎮╅幓鎺戠婵°倗濮烽崑娑㈠疮鐎涙ɑ鍙忛柍褜鍓熼弻鏇㈠醇濠靛浂妫炴繛瀛樼矋椤ㄥ﹪寮婚悢鍏煎殞闁绘鐗嗗☉褏绱撴担鐟扮祷婵炲皷鈧剚鍤曢柧蹇ｅ亜椤曢亶鎮楀☉娆樼劷闁告ü绮欏铏圭磼濡崵鍙嗛梺纭呭Г缁捇骞嗙仦鍓х瘈闁告洦鍘鹃敍婊勭節閵忥絾纭鹃柡鍫墴瀹曠敻鍩€椤掆偓椤啴濡堕崱妯尖敍缂傚倸绉崇欢姘剁嵁閺嶎兙浜归柟鐑樼箖閺呮繈姊洪幐搴ｇ畵婵炲眰鍔忛崐鎾⒒閸屾艾鈧娆㈠顒夌劷鐟滄棃濡堕鍫熷仺闂傚牊绋撶粵蹇涙⒑閸忚偐銈撮柡鍛仦閹便劑宕樺ù瀣杸闂佺粯锚绾绢參銆傞弻銉︾厓闂佸灝顑呴悘瀛樻叏婵犲偆鐓肩€规洖銈搁幃銏ゅ箒閹哄棗浜鹃柟鐑樺焾閻斿棛鎲告惔鈭舵椽鎮㈤悡搴ゆ憰闂佺粯姊婚崢褏绮堥崘顔界厾缁炬澘宕晶鐗堛亜閺冣偓鐢繝骞冨Δ鍐╁枂闁告洦鍓涢ˇ顓㈡⒑鏉炵増绁版い鏇嗗洦鍋╅柣鎴ｆ闁卞洭鏌￠崶銉ュ闁哄懌鍨虹换娑欐綇閸撗冨煂闂佸湱鈷堥崑濠囨偘椤旂⒈鍚嬮柛鈩冩礈缁犳岸姊虹紒妯哄Е濞存粍绮撻崺鈧い鎺嶈兌婢ь亝銇勯銏㈢闁靛洦鍔欓獮鎺楀箣閻欏懐鍚归梻鍌欐祰椤宕曢悡骞盯宕熼姘憋紲闂侀潧艌閺呮粓宕愰悽鍛婄厽闁靛繈鍨洪弳鈺呮煏閸℃韬柡灞剧〒閳ь剨缍嗛崑鍛焊椤撱垺鐓冪紓浣股戝畷宀€鈧娲栫紞濠囥€佸▎鎾村仼閻忕偛鍚嬮崰姘節閻㈤潧啸闁轰焦鎮傚畷鎴濃槈閵忕姾袝闁诲函缍嗛崰妤呭磻閿熺姵鐓冮柛婵嗗閸ｆ椽鏌￠崨顔惧弨闁哄矉缍侀獮鍥敇閻橆喖浠﹂梻浣姐€€閸嬫挸霉閻樺樊鍎愰柛濠勬暬閺屻劌鈹戦崱娆忓毈缂備降鍔忔慨銈夊Φ閸曨垰顫呴柣妯哄暱濞堟鈹戦纭烽練婵炲拑绲垮Σ鎰板箳閹冲磭鍠栧畷顐﹀礋閸偆绉兼繝鐢靛Х椤ｈ棄危閸涙潙鍨傚ù鐓庣摠閺呮繃銇勮箛鎾跺缂佺姵鐗楁穱濠囧Χ閸涱喖娅ｇ紓浣哄Х缁垶濡甸崟顖氱睄闁搞儱妫欑敮锟犵嵁韫囨拋娲敂閸涱亝瀚奸梻浣虹帛閹稿摜鈧灚甯掗…鍥偄閸忚偐鍘辨繝鐢靛Т閸燁垳绮堢€ｎ兘鍋撳▓鍨灍闁诡喖鍊块悰顔锯偓锝庡枟閸嬫劙鏌ｉ姀銏╂殰缂佸崬寮舵穱濠囨倷椤忓嫧鍋撻弽顬稒绗熼埀顒€鐣烽幋锕€绠涙い鎺戝€归幊鍐╃節绾板纾块柛瀣灴瀹曟劙濡舵径濠傚亶婵犻潧鍊搁幉锟犲磹閼哥數绡€闂傚牊渚楅崕婊呯磽瀹ュ棛澧紒缁樼箖缁绘繈宕掑⿰鍐炬毇闂備胶绮敮鎺楁倶濮樿泛桅闁告洦鍨扮粻鎶芥煕閳╁啨浠﹀瑙勬礋濮婄粯鎷呮搴濊缂備焦褰冨鈥崇暦濡も偓椤粓鍩€椤掑嫬违濞达絿纭跺Σ鍫ユ煏韫囧ň鍋撻弬銉ヤ壕闁绘垼濮ら悡鐘电棯閺夊灝鑸瑰褎鎸抽弻锝呪槈濞嗘劕纾冲┑顔硷功缁垶骞忛崨瀛樺€绘俊顖滃劋閻ｎ剚淇婇悙顏勨偓鏍洪敃鍌氬偍濞寸姴顑呯粻鐐烘煏婵炵偓娅嗘い銉ョ墛缁绘盯骞嬮悙鑼懖闂佽绻嬬欢姘潖閾忚瀚氶柍銉ョ－閳ь剙顭烽弻宥堫檨闁告挾鍠庨锝夊Ω閿旂虎娴勯柣搴秵娴滅偤顢欐繝鍥ㄢ拺閻熸瑥瀚崝鑸典繆椤愩垹顏瑙勬礉閵囨劙骞掗幘璺哄箰闂備焦鎮堕崕顖炲磿闁秵鍊堕柍鍝勫暟绾惧ジ鏌ｅ鈧褔鍩㈤崼鐕佹闁绘劕顕晶閬嶆煕閹烘挸娴柟顔惧厴瀵埖鎯旈幘鏉戠彾闂傚倸鍊风粈渚€骞栭鈶芥稑鈻庨幋婵嗙亰闂佸湱鍋撻崜姘焽閺嶃劎绠剧€瑰壊鍠曠花濂告煕閵堝棗娴柡灞炬礋瀹曠厧鈹戦崶褎顏￠梻浣筋嚙妤犲憡鏅堕悾灞绢潟闁规儳顕悷褰掓煕閵夋垵瀚禍顏堟⒒娴ｈ銇熼柛妯恒偢瀹曟劙宕归鍛缂傚倷鐒﹂…鍥╁姬閳ь剟姊洪幖鐐插姌闁告柨绉撮埢宥呪堪閸曨厾鐦堥梺闈涢獜缁插墽娑垫ィ鍐╃叆闁哄浂浜為悾鐢告煙椤曗偓缁犳牠骞冨⿰鍏剧喖宕烽鐘垫毎闂傚倷绶氬褔鎮ч崱娆愬床闁圭儤顨呯粈鍡椕归悡搴ｆ憼闁绘挻娲熼弻锝夊即濮橀硸妲梺璇查獜缁犳捇寮婚悢纰辨晩闁诡垎鍌涱潟闂備胶顢婂▍鏇㈡偋閻樿鏄ラ柣鎰惈缁犳氨鎲哥仦鍓х彾闁哄洨鍊ｉ弮鍫熷亹闂傚牊绋愮划鍫曟⒑鐠囪尙绠茬紒璇茬墕閻ｅ嘲鈹戦崱鈺傚兊濡炪倖鎸荤换鍕不濮橆剦娓婚柕鍫濇婵呯磼閹绘帞浠㈤悡銈夋倵閻㈢數銆婇柛瀣尵閹叉挳宕熼鍌︾喘闂備焦鎮堕崝瀣垝濞嗗浚鍤曞┑鐘宠壘楠炪垺淇婇妶鍜冨伐闁诡喗鍨垮铏瑰寲閺囩偛鈷夐梺闈涙川閸嬨倝骞冮悾宀€纾兼俊顖濆亹閻﹀牓姊哄Ч鍥х伈婵炰匠鍐懃闂傚倷鐒︾€笛兠鸿箛娑樼９婵犻潧妫涢弳锔姐亜閺嶎偄浠滅紒鐘电帛閵囧嫰寮崶褌姹楅梺缁樻尰閸ㄥ灝顫忓ú顏勫窛濠电姴鍟ˇ鈺呮⒑閹肩偛濮傚ù婊冪埣楠炲啴妾辩紒鐘崇☉閳藉鈻庨幋婵嗙疄闂傚倷鐒﹂弸濂稿疾濞戙垹绐楁慨姗嗗厴閺嬫棃鏌￠崘銊у闁绘挻鐩弻娑㈠焺閸愵亝鍣ф繛瀵稿閸ャ劎鍘撻悷婊勭矒瀹曟粌鈻庨幘鏉戔偓鑸垫叏濮楀棗澧婚柣鏂挎娣囧﹪顢涘杈ㄧ檨闂佺ǹ顑嗛幐鎼侊綖濠靛鍋傞幖娣灮娴滃墎绱撻崒娆戭槮闁稿﹤婀遍弫顕€鏁撻悩鍙夌€梺鍛婂姦閸犳牜澹曢崗鍏煎弿婵☆垰銇橀崥顐︽煛閸℃洖宓嗛柡宀嬬秮閹垽宕妷褏鍘愮紓鍌欑椤︻垶鎮ユ總绋跨疇闁绘劗鍎ら悞鑲┾偓骞垮劚閹叉﹢濡搁埡鍌滃弳闂佸搫鍊搁悘婵嬪煕閺冨牊鐓熼柨鏇楀亾闁绘绻掑Σ鎰板箳濡も偓缁€鍫㈡喐瀹ュ鈧倹绺介崨濠傜彅闁哄鐗勯崝濠冪濠婂嫨浜滈柟鏉跨仛缁舵盯妫呴澶婂⒋闁哄矉绱曟禒锕傚礈瑜庨崚娑欑節绾版ê澧查柟顔煎€规穱濠囧醇閺囩偛绐涘銈嗘尵婵即宕戦弽顓熲拺閻犲洤寮堕崬澶嬨亜椤愩埄妲搁悡銈夌叓閸ャ劎鈽夌痪鎯ф健閺岀喖骞嗛悧鍫闂佺粯甯掗悘姘跺Φ閸曨垰绠抽柟鎼灠婵稓绱撴担鍓叉Ш闁轰礁顭峰璇测槈閵忊€充汗閻庤娲栧ú銈夊煕閸喓绡€闁靛骏缍嗗鎰箾閸欏鐭屾俊鍙夊姍楠炴帡骞婂畷鍥ф灈鐎规洜鍏橀、姗€鎮╅崹顐ｇ槥濠电姷顣槐鏇㈠磻閹达箑纾归柡宥庡幗閸嬪淇婇妶鍌氫壕濡炪値鍋呯换鍫熶繆閹间礁唯闁归偊鍘煎ù顕€鏌熼鐣屾噰闁诡喗绮岃灒闁绘挸楠哥粻鐐烘⒒娴ｇ瓔鍤欓柛鎴犳櫕缁辩偤宕卞☉妯硷紱闂佸憡渚楅崢楣冨汲閿旈敮鍋撻崗澶婁壕闂侀€炲苯澧撮柛鈹惧亾濡炪倖甯掗崰姘焽閹邦厾绠鹃柛娆忣檧閼板潡鏌曢崱妤€鏆ｇ€规洖宕灃濠电姴鍊归鍌炴⒒娴ｅ憡鍟炴繛璇х畵瀹曟粌鈽夊顒€袣闂侀€炲苯澧紒缁樼⊕濞煎繘宕滆閸╁本绻濋姀銏″殌闁挎洏鍊涢悘瀣攽閻樿宸ラ柣妤€锕崺娑㈠箳濡や胶鍘遍柣蹇曞仦瀹曟ɑ绔熷鈧弻宥堫檨闁告挾鍠栬棢闁圭偓鏋奸弸搴ㄦ煏韫囨洖顎屾繛灏栨櫊閹銈﹂幐搴哗闂佸憡妫戠粻鎴︹€旈崘顔嘉ч柛鈩冾殘閻熴劑姊虹粙鍖″伐闁硅绱曠划瀣箳閹存柨鐗氶梺鍓插亞閸犳捇宕㈤幆褉鏀介柣鎰硾閽勫吋銇勯弴鍡楁处閸婂爼鏌ㄩ悢鍝勑ｉ柣鎾寸洴閺屾稑鈽夐崡鐐茬闂佺硶鏅徊楣冨Φ閸曨垰顫呴柍銉ュ暱閹界敻姊虹化鏇熸澒闁稿鎸搁—鍐Χ閸℃鐟ㄩ柣搴㈠嚬閸撶喖骞忛幋锔藉亜閻炴稈鈧厖澹曞Δ鐘靛仜閻忔繈宕濆顓犵閻忕偛鍊告俊鍨熆鐟欏嫭绀嬬€规洘顨婂鑽も偓闈涙憸閻ｇ偓淇婇悙顏勨偓鏍偋濡ゅ啯宕茬€广儱顦崥瑙勭箾閸℃ê鐏╂俊顐灦閺岀喖顢涢崱妤€鏆炴慨锝嗗姍閺屟呯磼濡厧鈷岄梺鍝勭焿缁蹭粙锝炲⿰鍫濈劦妞ゆ巻鍋撴い顓炴穿椤﹀磭绱掗崒娑樼瑨妤楊亙鍗冲畷鐔煎煛閳ь剛鑺辨繝姘拺闁荤喐澹嗛幗鐘电磼鐠囪尙孝閾伙綁鏌嶈閸撶喖骞冨Δ鈧埢鎾诲垂椤旂晫褰梻浣呵归鍛存晝閵堝鍋╃€瑰嫰鍋婂銊╂煃瑜滈崜鐔煎Υ娴ｇ硶鏋庨柟鐐綑濞堟劙姊虹€圭姵銆冪紒鈧担绛嬫澓婵犵數濮烽。顔炬閺囥垹纾婚柟杈剧畱绾惧綊鏌″搴′簼闁哄棙绮撻弻鐔兼倻濮楀棙鐣堕梺姹囧€ら崳锝夊蓟閻斿皝鏋旈柛顭戝枟閻忔挻绻涢幋鐐村碍缂佸鎸冲﹢渚€姊虹紒妯忣亜螣婵犲洤纾块柟鎵閻撳啴姊洪崹顕呭剰婵炴惌鍣ｉ弻锛勪沪閸撗佲偓鎺懨归悪鍛暤鐎规洘绮忛ˇ鎶芥煕閿濆骸骞樼紒杈ㄦ尰閹峰懘鎮块姘腐濠电姭鎷冮崟鍨暭闂佺懓绠嶉崹褰掑煡婢跺ň鏋庨煫鍥ㄦ礈閻涱噣姊虹拠鎻掑毐缂傚秴妫濆畷鎴﹀幢濞存澘娲︾€佃偐鈧稒锚閳ь剛鏁婚幃宄扳枎韫囨搩浠剧紓浣插亾闁告劑鍔夐弨浠嬫煟閹邦垼鍤嬮棅顒夊墰閳ь剚顔栭崳顕€宕戞繝鍌滄殾婵せ鍋撴い銏＄懇閹虫牠鍩＄€ｎ剙绨ユ繝鐢靛У椤旀牠宕板Δ鍛櫇闁靛繈鍨洪崗婊堟煙闂傚顦﹂柤绋跨秺閺岋綁濮€閻樺啿鏆堥梺鎶芥敱鐢帡婀侀梺鎸庣箓閻楀棜顣跨紓浣鸿檸閸樺ジ鎮ユ總绋胯摕婵炴垯鍨洪弲鏌ョ叓閸ャ劍绀堥柡鍡愬€曡灃闁绘﹢娼ф禒婊呯磼缂佹﹫鑰跨€殿噮鍋婇獮妯肩磼濡粯顏熼梻浣芥硶閸ｏ箓骞忛敓锟�
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欏顔藉瘜闂侀潧鐗嗗Λ妤冪箔閹烘鐓ラ柡鍥朵簻椤╊剛绱掗鐣屾噯缂佺姵鐩顕€宕掑⿰鍛潓闂傚倷绶氬褎顨ヨ箛鏇炵筏闁告挆鍕幑闂佺粯鍔﹂崗娆愮濠婂牊鐓欓悗娑欘焽缁犳牠鏌涢悩绛硅€块柡灞炬礋瀹曞崬螖娴ｅ憡鐣绘俊銈囧Х閸嬬偤骞戦崶顒傚祦闁瑰瓨鎯婇弮鍫濈劦妞ゆ帒鍊荤粈濠傗攽閻樺弶鎼愮紒鐘崇墵閺屽秵娼幍顔跨獥闂佸摜鍠庨幊蹇涘Φ閸曨垰绫嶉柛灞捐壘娴犳ɑ绻濋姀銏″殌闁绘绻掑Σ鎰板箻鐎涙ê顎撻梺鍛婄箓鐎氼亝绔熼弴銏♀拺缂佸娼￠妤呮煕閻旈攱鍋ラ柍銉畵瀹曞爼顢楁担瑙勵仧闂備胶绮灙閻忓浚浜獮妤呭即閻愨晜鏂€闂佺粯鍔栧娆撴倶閻斿吋鐓曢柕濞垮劜鐠愶紕绱掗娆惧殭闁宠棄顦埢搴☆吋閸曨厾娉块梻鍌欒兌閹虫捇骞栭銈囩煋闁绘垹鐡旈弫濠傤熆閼搁潧濮堥柣鎾寸洴閹鏁愭惔鈥茬敖闂佺懓鍟垮ú锕傚箞閵婏妇绡€闁告侗鍣禒鈺冪磽娴ｄ粙鍝洪悽顖涘笩閻忔帡姊洪幆褏绠版繝鈧柆宥呯疅妞ゅ繐妫涚壕浠嬫煕鐏炲墽鎳呴悹鎰嵆閺屾盯鎮╅幇浣圭杹闂佽鍠氶崗姗€宕洪埄鍐懝闁搞儜鍕靛悪闂備浇宕垫慨鏉懨洪敂鍓х焾闁哄鍤﹂敐澶婇唶闁靛濡囬崢顏堟椤愩垺澶勬繛鍙夌墪閺嗏晠姊绘担鍦菇闁稿﹥鍔欏畷鎴﹀箻缂佹ǚ鎷婚梺绋挎湰閻熴劑宕楀畝鍕嚑妞ゅ繐妫涚壕鑲╃磽娴ｅ顏呮叏瀹ュ棭娈介柣鎰儗濞堟梹顨滈鍐ㄥ祮鐎规洜鍘ч埞鎴﹀醇椤愶及鐔兼⒑閼姐倕鏋戠紒顔煎閺呰泛螖閸愨晜娈板┑掳鍊曢幊搴ㄦ偂閺囥垺鐓欓弶鍫ョ畺濡绢噣鏌ｉ幘瀛樼缂佺粯绻堝Λ鍐ㄢ槈濮楀棔鎮ｉ梻浣告啞濮婄懓鐣濋幖浣歌摕闁挎稑瀚ч崑鎾绘晲鎼存繈鍋楅梺鍛婄懃鐎氫即寮婚垾宕囨殕闁逞屽墴瀹曚即骞樼拠鑼崶婵犵數濮村ú銈囨兜閳ь剟姊虹紒妯哄婵犮垺锕㈠鎼佸醇濠靛啯鏂€闂佺粯蓱椤旀牠寮抽鐐寸厱闁规儳顕粻鐐搭殽閻愭潙濮嶆鐐查叄閹墽浠﹂悾灞诲亰闂傚倷绀佸﹢閬嶅磿閵堝鈧啴宕ㄩ婊€绗夐梺鎸庣箓濡孩绂嶅⿰鍫熺厵闁硅鍔栫涵楣冩煛鐎ｎ亪鍙勯柡灞剧缁犳盯骞欓崟顔兼儓婵犳鍠栭敃锔惧垝椤栫偛绠柛娑樼摠閸嬶繝姊洪銊х暠婵炲牊绻勭槐鎾诲磼濞嗘劗銈板銈嗘肠閸パ咃紮闂佺鍕垫畼鐎规挷绀侀埞鎴︽偐閹绘帊绨介梺鍛婅壘椤戝寮诲☉銏犵労闁告劦浜濋崳顕€鎮峰⿰鈧崡宕囩不閺嵮屾綎婵炲樊浜滄导鐘绘煕閺囥劌澧柛瀣Ч濮婃椽骞栭悙鎻掝瀴濠碉紕鍋樼划娆忕暦閸濆嫧妲堥柕蹇曞Х椤斿﹪姊洪悷鎵憼闁荤喆鍨藉畷鎴﹀箻鐠囪尙顔婇梺瑙勬儗閸樹粙宕撻悽鍛娾拺闁圭ǹ娴风粻鎾绘煙閸愯尙校缂佹梻鍠栧鎾閳锯偓閹风粯绻涢幘鏉戠劰闁稿鎹囬弻宥堫檨闁告挻鐩畷鎴濃槈閵忊€虫濡炪倖鐗楃粙鎺戔枍閻樿褰掓偂鎼达絾鎲奸梺缁樻尰濞茬喖骞冨Δ鍛櫜閹肩补鈧尙鏁栨繝鐢靛仜瀵爼骞愰幎钘夎摕闁绘棃顥撻弳锕傛煕閵夋垵瀚禍顏呬繆閻愵亜鈧垿宕归搹鍦煓闁硅揪绠戦悡鈥愁熆鐠哄彿鍫ュ几鎼淬劍鐓欓梺顓ㄧ畱閺嬫稑鈹戦悙鍙夋悙闁宠鍨块弫宥夊礋椤愨剝婢€闂備胶枪閿曘儵鎮ч悩璇茬畺鐟滃秹銈导鏉戦唶婵犻潧鐗炵槐閬嶆⒒娴ｈ櫣甯涢柛鏃€顨婂畷鏇㈠Χ婢跺﹦锛涢梺绋跨灱閸嬬偤鎮″☉銏＄厱闁靛绲介崝姘舵煟韫囷絼閭柡宀嬬秮楠炴鈧潧鎲￠崚娑㈡⒑閸濆嫭婀伴柣鈺婂灦閻涱噣宕堕鈧粈鍫澝归敐鍤藉綊鎮￠崒姘ｆ斀闁绘劘灏欓幗鐘电磼椤旇姤灏电紒顔碱煼瀵€燁檨婵炲吋鐗犻弻褑绠涢幘纾嬬缂佺偓鍎抽崥瀣箞閵娿儙鏃堝焵椤掆偓铻炴繛鍡樻尰閸嬧晠鏌涜箛姘汗缂佲檧鍋撻梻浣圭湽閸ㄨ棄顭囪缁傚秷銇愰幒鎾跺幈闁诲函缍嗛崑鍛暦鐏炵偓鍙忓┑鐘插暞閵囨繃淇婇銏犳殭闁宠棄顦埢搴∥熷ú缁橈骏缂傚倸鍊搁崐椋庢媼閺屻儱纾婚柟鎹愮М瑜版帗鏅查柛銉ｅ妽閻濐亝绻涚€涙鐭嬮柣鐔叉櫅椤繒绱掑Ο璇差€撻梺鍛婄☉閿曘倝寮抽崼鐔虹闁规儳顕。鍙夈亜閵娿儻宸ラ柣锝夋敱鐎靛ジ寮堕幋婵嗘暏婵＄偑鍊栭幐楣冨磻閻旂厧绀夐柨鏇楀亾妞ゎ亜鍟存俊鍫曞幢濡ゅ啰鎳嗛梻浣呵归敃銉╁箖閸屾氨鏆﹂柕蹇嬪€曠粻缁樸亜閺冨倹娅曢柛妯绘倐閺岋綀绠涢弴鐐扮捕婵犫拃鍌滅煓闁糕斂鍎插鍕箛椤撶姴甯楅梺鑽ゅ枑閻熴儳鈧凹鍓熼幃妯尖偓鐢电《閸嬫挾鎲撮崟顒傤槬閻庤娲﹂崜婵嬫倶閸愨晝绡€闁汇垽娼у皬闂佺厧鍟挎晶搴㈢珶閺囩姵瀚氭繛鏉戭儐閺傗偓闂備焦瀵х粙鎴犫偓姘煎墯缁傚秵绺介崨濠勫幈婵犵數濮撮崯鐗堟櫠閻㈢鍋撳▓鍨灈妞ゎ參鏀辨穱濠囧箹娴ｅ摜鍘告繛杈剧秬濞咃綁寮搁崘顔解拻濠电姴楠告禍婊勭箾鐠囇呯暠閻撱倖淇婇婵囶仩闁哄棴绠撻弻娑樷枎韫囷絾楔缂備讲妾ч崑鎾绘⒒閸屾艾鈧悂鎮ф繝鍐懝婵°倕鎳庣壕鍧楁煙閹殿喖顣奸柣鎾存礋閹鏁愰崘銊ヮ瀳婵犳鍠栭崐褰掑箞閵婏妇绡€闁稿被鍊楅崥瀣倵鐟欏嫭绀€闁靛牊鎮傞獮鍐閻樺灚娈濋梺瑙勵問閸犳帡宕戦幘娲绘晢闁告洦鍏橀幏娲⒒閸屾氨澧涚紒瀣尵缁鎮欓悜妯煎幈婵犵數濮撮崯鐗堟櫠閻㈠憡鐓欐い鏇炴缁♀偓閻庢鍠楅幐鎶藉箖濞嗘挸鐓涢柛灞剧⊕濠㈡垿姊婚崒娆掑厡缂侇噮鍨堕弫瀣倵濞堝灝鏋涢柣鏍帶閻ｉ鈧湱濮版禍褰掓煙閻戞ɑ灏甸柛妯哄船閳规垿顢欓弬銈勭返闂佸憡鎸诲銊╁箯瑜版帗鏅柛鏇ㄥ幘閿涙粓鏌℃径濠勫闁告柨鑻湁妞ゆ柨顫曟禍婊堟煏韫囥儳纾块柟鍐叉喘閺岀喎鐣烽崶褌妲愭繝纰夌磿閸忔ɑ鎱ㄩ埀顒勬煃閵夛箑澧ù鍏兼礈缁辨捇宕掑顑藉亾閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滄粌霉閺嶎厽鐓忓┑鐐戝啫鏆欓柣蹇擄工閳规垿鎮欓崣澶樻缂備胶绮敮鈥愁嚕閹惰姤鏅濋柛灞剧〒閸橆亝绻濋悽闈浶㈢紒缁樺姍閵嗗懘顢楅崒婊咃紲闂佹娊鏁崑鎾绘煕鐎ｎ偅宕屾慨濠勭帛閹峰懘宕ㄦ繝鍌涙畼婵犵數鍋犻婊呯不閹惧磭鏆﹂柟杈剧畱缁€瀣亜閹捐泛小缂佹顦靛铏圭磼濡搫顫庨梺绋跨昂閸婃繈宕哄☉銏犵婵°倓鑳堕崢閬嶆煟韫囨洖浠滃褌绮欓獮濠囧礋椤撶姷锛滄繛杈剧到閹碱偊銆傛總鍛婄厪闁搞儜鍐句純濡炪們鍨哄ú鐔告叏閳ь剟鏌嶉崫鍕偓鐢稿几閸涘瓨鐓熼幖娣€ゅ鎰箾閸欏顏嗗弲闂佸搫绋侀崢浠嬪磻閳哄懏鐓熼柟杈剧稻椤ュ鏌ｉ幇顒婂姛闁硅尙枪楗即宕楅悙顒傗槈妤楊亙鍗冲畷姗€鎮欓鈧惁婊堟⒒娓氣偓濞佳囨偋閸℃あ娑樷枎閹存粎鍓ㄩ梺姹囧灩閹诧繝鎮￠弴銏″€甸柨婵嗛婢ь喖霉閻樺磭鐭岄柟顕呭枛閳诲酣骞樼€电ǹ骞堥梻浣告惈濞层垽宕濈仦鍓ь洸闁规壆澧楅悡娑㈡煃瑜滈崜姘辩矉閹烘柡鍋撻敐搴′簽闁告瑥妫楅埞鎴︽倷閺夋垹浠搁梺鎸庣閵囧嫯绠涙繝鍐╃彅闂傚洤顦扮换婵囩節閸屾碍娈ч梺绋款儍閸ㄤ粙寮婚敍鍕勃闁告挆鈧慨鍥╃磽娴ｈ櫣甯涚紒璇茬墕閻ｅ嘲顫滈埀顒佷繆閹间礁唯妞ゆ梻鏅粙鍫ユ⒒娴ｇ瓔鍤欓悗娑掓櫊椤㈡瑩寮介鐐电崶濠德板€曢幊蹇涘磻閸岀偞鐓ラ柣鏂挎惈琚ㄩ梺閫炲苯鍘哥紒鑸佃壘椤曪綁顢氶埀顒€鐣烽悡搴樻斀闁糕剝鍩冮崑鎾诲垂椤旇鏂€闂佺粯鍔曞鍫曀夊⿰鍕閻庣數枪閸樻挳鏌熼鐓庢Щ闁宠鍨归埀顒婄秵閸嬧偓闁归绮换娑欐綇閸撗勫仹闂佺儵鍓濋弻銊┾€﹂崶顒€绠涙い鏃傛嚀娴滈箖鎮峰▎蹇擃仾缂佲偓閸愵喗鐓ユ慨妯垮煐閻撶喖鏌ㄥ┑鍡樻悙闁告ê鐡ㄦ穱濠囶敃閵忕姵娈梺瀹犳椤︻垶锝炲┑瀣垫晣闁绘洑绀侀弲顐⑩攽閻樺灚鏆╅柛瀣仱楠炴劖绻濆顒佹К闂佸憡鍔﹂崰鏍婵犳碍鐓欓柛鎾楀懎绗￠梺缁樻尰缁嬫捇鍩€椤掆偓閸樻粓宕戦幘缁樼厓鐟滄粓宕滈悢椋庢殾濞村吋娼欓崘鈧銈嗘尵婵绮婇敃鍌涒拺缁绢厼鎳忚ぐ褏绱掗悩鍐茬仼缂侇喛顕ч埥澶愬閳锯偓閹峰姊洪崜鎻掍簽闁哥姵鎹囬崺濠囧即閻旂繝绨婚梺鍝勫€搁悘婵嬵敂椤愩倗纾奸弶鍫涘妽瀹曞瞼鈧娲樼敮鎺楀煝鎼淬劌绠抽柟瀛樼箓閼垫劖绻濋悽闈浶ｆい鏃€鐗犲畷浼村冀椤撶喎鈧埖鎱ㄥΟ鍧楀摵鐎规洖寮剁换娑㈠箣閻愬灚鍣紓浣叉閸嬫捇姊绘担瑙勫仩闁稿孩绮撳畷鍗炍熺拠鍙夋瘒闂傚倸鍊搁崐鐑芥嚄閸撲礁鍨濇い鏍嚤濞戞瑧鐟归柍褜鍓氭穱濠囧箻椤旇偐鍔﹀銈嗗笒鐎氼參鎮￠弴銏″€甸柨婵嗛娴滄繈鎮樿箛锝呭籍闁哄苯绉归幐濠冨緞濡亶锕傛⒑鐎圭媭鍤欏Δ鐘崇摃閻忔帡姊虹紒妯诲碍濡ょ姴鎲￠〃娆戠磽閸屾艾鈧悂宕愰幖浣哥９闁绘垼濮ら崵鍕煠缁嬭法浠涢柛娆忕箻閺岋綁濮€閳惰泛婀辩划濠氬级鎼存挻鏂€闂佺粯锚閻ゅ洦绔熷Ο鑲╃＜闁绘﹢娼ф禒閬嶆煛鐏炵偓绀冪紒缁樼椤︽煡鎮楀鐓庡⒋鐎殿喗鎮傚畷鎺楁倷缁瀚奸梻浣告啞缁嬫垿鏁冮妷锕€绶為柛鏇ㄥ灡閻撴洘淇婇婊冨付閻㈩垵鍩栭妵鍕即椤忓棛袦婵犵鍓濋幃鍌涗繆閻戣棄顫呴柍鈺佸暞濠㈡垵鈹戦敍鍕杭闁稿﹥鐗曢～蹇旂節濮橆儵銉╂倵閿濆簼鎲鹃柛鐔锋嚇閺屾洘寰勯崱妯荤彆闂佺粯鎸婚悷褔鎯€椤忓牆绾ч柛顭戝枦閸╃偞绻涚€涙鐭嬬紒璇茬墦瀵鏁撻悩鎻掔獩濡炪倖鍔戦崹瑙勬叏閿曗偓閳规垿顢欑涵閿嬫暰濠碉紕鍋樼划娆撴偘椤曗偓瀵粙顢橀悢灏佸亾閻戣姤鐓欑紓浣姑粭鎺楁懚濮樿埖鈷掗柛灞剧懅椤︼箓鏌熺拠褏绡€鐎规洘绻堝鎾偄閸涘﹦褰垮┑鐐差嚟婵挳顢栭崨瀛樺€块柛顭戝亖娴滄粓鏌熼崫鍕ゆい锔肩畵閺屽秶绱掑Ο鑽ゎ槰闂侀€涚┒閸斿秶鎹㈠┑瀣窛妞ゆ洖鎳嶉崫妤呮⒒娴ｅ摜绉烘い銉︽尰缁绘盯鍩€椤掍緡娈介柣鎰摠瀹曞本顨ラ悙鏉戠瑨闁宠鍨垮畷鍗烆渻閳ь剙濮傛慨濠傤煼瀹曟帒鈻庨幋顓熜滈梻浣侯攰椤曟粎鎹㈠┑鍡欐殾鐟滅増甯楅崑銊╂煟閵忋垺鏆╂い锔诲灡缁绘繈鎮介棃娴躲垽鏌涙繝鍌ょ吋鐎殿喗濞婇弫鍌炴偩瀹€鈧惁鍫熺節閻㈤潧孝闁稿﹥鎮傚铏節閸ャ劎鍘搁柣搴秵娴滎亪宕ｉ崟顖涚厸鐎光偓閳ь剟宕伴幘鑸殿潟闁圭儤顨呴～鍛存煟濡搫鎮佺紒顔芥尦濮婄粯鎷呴崨闈涚秺椤㈡牠宕卞☉妯碱唹闂佹悶鍎洪崜娆撳几娓氣偓閺屾盯骞囬棃娑欑亪缂佺偓鍎抽崥瀣箞閵娿儙鐔煎传閸曨喖鐓橀梻渚€鈧偛鑻晶浼存煕鐎ｎ偆娲撮柍銉︽瀹曟﹢顢欓崲澹洦鐓曢柍鈺佸枤濞堟﹢鏌ｉ悢绋垮婵﹨娅ｇ槐鎺懳熷ú顏勬疂缂傚倷鑳舵慨鐢稿垂閸ф鏄ラ柕蹇曞閸氬顭跨捄鐚村伐闁哥偑鍔岄—鍐Χ閸℃ê鏆楅梺鍝ュ枎濞硷繝鐛箛鎾佹椽顢旈崨顏呭闂備礁鎲＄粙鎴︽晝閿曞倸鐓″鑸靛姈閻撱儵鏌￠崶顭嬵亪宕濆⿰鍛亾閸偅绶查悗姘緲閻ｇ兘鎮㈢喊杈ㄦ櫖濠电偞鍨堕懝楣冪嵁閸儲鈷掑ù锝囩摂濞兼劙鏌涙惔銏犫枙妞ゃ垺宀搁、姗€濮€閻樼數鏋冮梻浣规偠閸庮垶宕濇繝鍐洸婵犲﹤鐗婇悡蹇涚叓閸パ嶅伐濠⒀勭叀閹ǹ绠涢敐鍛睄闂佸搫鑻粔褰掑箰婵犲啫绶炴俊顖滃劦閺嬪懎鈹戦悩鎰佸晱闁哥姵甯″畷鎴﹀箻缂佹ǚ鎷洪梺瑙勫劶婵倝寮冲⿰鍫熺厱閻庯綆鍋呭畷宀勬煛瀹€鈧崰鏍蓟閵娧€鍋撻敐搴′簴濞寸姰鍨归埞鎴︻敊绾攱鏁惧┑锛勫仩濡嫰鎮鹃悜钘壩╅柨鏂垮⒔閻﹀牓姊洪崨濠冪８闁告柨绉归敐鐐差吋婢跺鎷绘繛杈剧悼閹虫捇顢氬⿰鍕闁圭粯甯炵粻鑽も偓瑙勬礃濞茬喖鐛惔銊﹀癄濠㈣泛鑻慨锔戒繆閻愵亜鈧牕顔忔繝姘；闁瑰墽绮悡鐔肩叓閸ャ劍鐓ラ柛婵囨そ閺岀喖顢欓幆褍骞嬮梺绯曟櫔缁绘繂鐣烽妸鈺婃晣闁斥晛鍟崯鍝勨攽閻樺灚鏆╅柛瀣仱瀹曞綊骞嗛弶璺ㄧ▕濠电姷顣介崜婵娿亹閸愵喗鍋嬮柟鎹愵嚙閽冪喐绻涢幋鐑嗙劯闁哄啫鐗嗙粈鍐煏婵炲灝鈧牠宕熼崘顔解拻濞达絿鐡旈崵娆戠磼缂佹ê鐏存鐐村灴瀹曟儼顧佹繛鍏肩墬缁绘稑顔忛鑽ゅ嚬闂佺粯甯掗…鐑藉蓟閿濆绠涙い鏍ㄤ亢閺佸潡鏌ｆ惔銏㈩暡婵犮垺锕㈤垾锕傛嚄椤栵絾些婵＄偑鍊ら崢楣冨礂濡警鍤曢悹鍥ㄧゴ濡插牊淇婇娑氱煂闁哥姵鐗曢悾宄拔旈崨顓狀槯闂佸吋绁撮弲娑樷槈瑜斿濠氬磼濞嗘帒鍘￠梺纭呮珪閹稿墽鍒掔拠宸悑濠㈣泛锕ら悗顓㈡偡濠婂懎顣奸悽顖涘浮閹繝鍨鹃幇浣哄數闂佸吋鎮傚褎鎱ㄩ崶銊ｄ簻妞ゆ劑鍨洪崵鍥煛瀹€瀣埌閾伙絾绻涢懠棰濆殭闁哄懘浜跺娲川婵犲懎顥濋梺纭呮珪閿氭い顐㈢箳缁辨帒螣鐠囧樊鈧捇姊洪幆褏绠抽柟铏尵缁參鏁撻悩鏂ユ嫼闂佸憡绋戦敃銉╁煕閹扮増鐓熸俊銈勭劍缁€瀣煃閵夘垳鐣电€规洘鍎奸ˇ顕€鏌＄€ｎ亪鍙勯柡宀€鍠栭獮鎴﹀箛椤撶姴娑ч梺鍝勵槸閻楀棝鎮ч崱娑樼厴闁瑰鍋涚粻鐘绘⒑缁嬪尅鏀绘繛鑼枛閻涱喗绻濋崨顖滄澑濠电偞鍨堕…鍥储闁秵鈷戦柣鐔煎亰閸ょ喖鏌涚€ｎ剙浠辩€规洩缍€缁犳稑鈽夊▎鎴濆箺闂備胶绮弻銊╁箹椤愶负鈧倹绺介崨濠勫幍闂佺ǹ绻掗崢褎鎱ㄩ崶顒佺厓閻熸瑥瀚悘鎾煕閵娾晝鐣洪柟鐓庣秺椤㈡洟锝為鐔糕枙闂傚倸鍊峰ù鍥敋閺嶎厼绐楁俊銈勮兌娴犳氨绱撻崒娆掑厡濠殿喚鏁婚幆鍕敍閻愰潧绁﹀┑鈽嗗灟缁鳖喚妲愰敐鍡欑瘈闂傚牊绋掓径鍕亜閹捐埖娅掗梻鍌氬€搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閹冣挃闁硅櫕鎹囬垾鏃堝礃椤忎礁浜鹃柨婵嗙凹缁ㄥジ鏌熼惂鍝ョМ闁哄矉缍侀、姗€鎮欓幖顓燁棧闂備線娼уΛ娆戞暜閹烘缍栨繝闈涱儐閺呮煡鏌涘☉鍗炲妞ゃ儲鑹鹃埞鎴炲箠闁稿﹥顨嗛幈銊╂倻閽樺锛涢梺缁樺姈娴滀粙鍩€椤掆偓閸熸潙鐣烽妸銉囧酣顢楅埀顒佺閸忚偐绠鹃柡澶嬪灥閹垶绻涢崗鑲╂噰闁哄苯顑夊畷鍫曞Ω瑜忛惁鍫ユ⒒閸屾氨澧涚紒瀣尰閺呭爼濮€閳垛晛浜鹃悷娆忓缁€鈧梺缁樼墱閸樠囷綖韫囨洜纾兼俊顖濐嚙椤庢捇姊洪幆褏绠扮紒鐘茬Ч閸┾偓妞ゆ巻鍋撻柨鏇ㄤ邯瀵鈽夐姀鐘殿啋闁诲酣娼ч幗婊堟偩閼测晝纾藉〒姘搐閺嬬喖鏌ｉ悢鍙夋珚闁绘侗鍠氶埀顒婄秵閸犳宕戦幇顔瑰亾閻熸澘顥忛柛鐘愁殜钘熼柨鐔哄У閳锋帒霉閿濆嫯顒熸繛鍏煎浮閺屾盯寮撮悙鍏哥驳闂佷紮绲块崗妯侯潖閾忓湱纾兼俊顖濐嚙闂夊秴鈹戦悙璺虹毢闁哥姵鐗曢锝嗙節濮橆厼浜滈梺绋跨箺閸嬫劙宕濋悜鑺モ拺闁圭ǹ瀛╃壕鐢告煕鐎ｎ偅灏版い銊ｅ劦閹瑦娼忛幑鎰ㄥ亾閸ф鐓涢悘鐐靛亾缁€瀣偓瑙勬礀閵堟悂骞冮姀銏㈢煓闁割煈鍠楅弫顏堟⒒閸屾瑧顦﹂柟纰卞亰閹本寰勭仦缁㈡綗闂佸搫娲㈤崹褰掓倿閸偁浜滈柟鐑樺煀閸旂喓绱掓径灞炬毈闁哄本绋撻埀顒婄秵娴滄繈宕甸崶顒佸癄闁绘柨鍚嬮悡鏇熴亜閹邦喖孝闁告梹绮撻弻锝夊箻鐎垫悶鈧帡鏌嶈閸撴瑩宕㈠⿰鍫濈；闁瑰墽绮悡鍐喐濠婂牆绀堥柣鏃堫棑閺嗭箑霉閸忓吋缍戠紒鐘崇⊕閵囧嫰骞掗幋顓熜ㄩ梺鍛婃⒒閸忔ê螞閸涙惌鏁冮柕蹇娾偓鎰佹П闂備礁鎼惌澶岀礊娴ｅ摜鏆﹂柟杈剧畱瀹告繈鎮楀☉娆樼劷闁告﹢浜跺铏规兜閸涱厾鍔烽梺鍛婃煥缁夋挳鍩㈠澶婄缂備焦顭囬崢鎾绘偡濠婂嫮鐭掔€规洘绮岄埢搴ㄥ箣閻樼數鍘犻梻浣藉吹閸犳牠宕戞繝鍌ょ劷闁哄秲鍔庣粻鍓р偓鐟板閸犳洜鑺辩拠瑁佸綊鎳栭埡浣叉瀰闂佸搫鏈惄顖氼嚕閹绢喖惟闁靛鍎哄缁樹繆閻愵亜鈧洜鈧稈鏅犻幆鍕敍濮樺吋缍庡┑鐐叉▕娴滄粌顔忓┑鍡忔斀闁绘ɑ褰冮弳娆撴⒒婢跺﹦效婵﹥妞介幃婊堝煛閸屾稓褰熼梻浣规偠閸斿繐鈻斿☉銏″仼鐎瑰嫭鍣村ú顏嶆晜闁告洦鍘兼慨锔戒繆閻愵亜鈧牜鏁繝鍥ㄢ挃鐎广儱妫涢々鍙夌節婵犲倻澧涢柣鎾寸懇閺屾稑鈹戦崱妤婁痪濡炪們鍎茬划宀勬箒濠电姴锕ら悧蹇涙偩閻戞ɑ鍙忓┑鐘插暞閵囨繄鈧娲忛崝宥囨崲濞戞﹩娼╂い鎾跺仧娴犲ジ姊洪崨濠傜瑲閻㈩垽绻濋悰顔嘉熼搹瑙勬闂佹悶鍎崝宥夘敊閺囥垺鈷掑ù锝勮閺€浼存煙濞茶绨界紒顔碱煼楠炲鎮╅崗鍝ョ憹闂備胶鍘ч～鏇㈠磹濡ゅ啫顥氶柦妯侯棦瑜版帗鏅查柛顐亜濞堟劙姊虹紒妯虹瑐缂侇喗鐟╁璇测槈濮楀棙寤洪梺閫炲苯澧紒鍌氱Ф閹瑰嫭绗熼娑氱▉濠德板€х粻鎺楁晸閵夛妇顩茬憸鐗堝笚閻撴瑩鏌ｉ幋鐏活亪鎮橀敐澶嬬厱闁绘ǹ顕滈煬顒佹叏婵犲啯銇濈€规洏鍔嶇换娑㈠箳濠靛懘鍋楁繝纰樷偓鍐差暢缂侇喗鐟ч幑鍕Ω閿旂瓔鍟庡┑鐘垫暩婵炩偓婵炰匠鍥舵晞闁告侗鍘界痪顖涚節閻㈤潧袨闁搞劎鍘ч埢鏂库槈閵忊晜鏅為梺鎼炲労閸撴瑩宕掗妸鈺傜厵闂傚倸顕崝宥夋煃闁垮鐏撮柡灞剧☉閳藉顫滈崼婵嗩潬闂備焦妞块崢鐓幬涘☉姘潟闁圭儤顨呴悞鍨亜閹哄秷鍏岄柣鐔风秺閺屽秷顧侀柛鎾跺枛楠炲啯瀵奸幖顓熸櫓闂佹悶鍨归ˇ鏉课涢崘銊ф殾婵せ鍋撴い銏℃瀹曢亶顢橀悩鑼搸缂傚倸鍊搁崐鎼佸磹閹间礁纾归柣鎴ｅГ閸婂潡鏌ㄩ弴鐐测偓鐢稿焵椤掑﹦鐣电€规洖鐖奸崺锟犲礃瑜忛悷婵嬫⒒娴ｈ櫣甯涢柛鏃€顨堥幑銏犫攽閸喎搴婇梺绯曞墲鑿уù婊勭矒閺岀喖骞嶉搹顐ｇ彅闂佽绻嗛弲鐘诲蓟閿熺姴骞㈡俊顖氬悑閸ｄ即姊洪崫鍕缂佸缍婇獮鎰節閸愩劎绐炲┑鈽嗗灡娴滃爼鏁冮崒娑氬弰婵炴潙鍚嬮悷褔骞冩總鍛婄厓鐟滄粓宕滃☉銏犵疇闁规儳鐏堥崑鎾愁潩椤愩垹绁梺闈涙閸婂潡寮崒鐐村仼鐎光偓閳ь剝銇愭ィ鍐┾拺闁告繂瀚崒銊╂煕閵婏附銇濋柛鈺傜洴楠炲鏁傞悾灞藉箻闂備浇顕栭崢鐣屾暜閹烘绀堟い鎾跺枂娴滄粓鐓崶銊﹀暗濠⒀勭洴閺岋繝宕担绋款潻濡炪値鍙€濞夋洟骞戦崟顖氱煑鐎瑰嫭澹嗘晶鏇㈠础闁秵鐓曟い鎰Т閸旀粓鏌ｉ幘宕囩闁绘搩鍋婂畷鍫曞Ω閿旂虎妲梺鍝ュ枔鏋い顏勫暣婵℃儼绠涘☉娆樷偓宥呪攽閻橆喖鐏柨鏇樺灲閸ㄩ箖寮崼婵堫槰濡炪倖鏌ㄥΣ鍫ｎ樄闁哄本鐩崺鍕礃椤忓懎娅戞俊鐐€ら崐銈夊礃閿濆棙鏉告俊鐐€栧Λ渚€锝炲Δ鈧オ浼村醇濠靛牜妲烽梻浣侯攰閹活亞绮婚幋鐘典笉闁哄稁鐏涢弮鍫熸櫜闁告侗鍘藉▓顓熺箾绾惧浜瑰┑鐐╁亾闂佸搫鏈惄顖氼嚕閺夋嚦鐔煎传閸曠數鈧挳姊绘担鍛婃儓闁稿﹦鏁诲畷鎴﹀箻閺傘儲鏂€闂佹寧绋戠€氼剚绂嶆總鍛婄厱濠电姴鍟版晶顏呫亜椤愩垻绠茬紒缁樼箓椤繈顢栭埞鍨姦闁诡喗顨婇弫鎰償閳╁啰浜紓鍌氬€风拋鏌ュ疾濠靛牊顫曢柟鎯х摠婵潙螖閻橆喖濡界紒顔肩Ф缁顓奸崨顏勭墯闂佸壊鍋嗛崰鎾诲储閹剧粯鈷戦柛娑橈功缁犳捇鎮楀鐓庡⒋鐎殿喗濞婂鎾閿涘嫬骞堥梻浣告贡閸庛倝宕洪崼婵冩灁闁衡偓閹…


    func = builder->getInsertBB()->getParent();
    loop_cond_bb = new BasicBlock(func);
    loop_body_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);

    new UncondBrInstruction(loop_cond_bb, builder->getInsertBB());

    int recorded_loop_bb_in_stack = loop_manager.new_loop_enter(loop_cond_bb, end_bb);


    //LinkBB(builder->getInsertBB(), loop_cond_bb);
    //LinkBB(loop_cond_bb, loop_body_bb);
    //LinkBB(loop_cond_bb, end_bb);


    builder->add_link(builder->getInsertBB(), loop_cond_bb);
    builder->add_link(loop_cond_bb, loop_body_bb);
    builder->add_link(loop_cond_bb, end_bb);


    //the judgement to break is set inside the loopbb
    //thanks to the fucking coding, i have to write english comment now!
    builder->setInsertBB(loop_cond_bb);
    cond->need_2_be_bool = 1;
    //cond->getSymPtr()->changeType(TypeSystem::boolType);
    cond->genCode();
    ibackPatch(cond->itrueList(), loop_body_bb, 1);
    ibackPatch(cond->ifalseList(), end_bb, 0 );


    builder->setInsertBB(loop_body_bb);

    //i don't know whether it's right. but apparently, the domain must be change.

    stmt_genCode(doStmt, loop_body_bb, func);

    loop_body_bb = builder->getInsertBB();
    new UncondBrInstruction(loop_cond_bb, loop_body_bb);



    builder->setInsertBB(end_bb);
    loop_manager.loop_end(recorded_loop_bb_in_stack);

}

void ConstDeclList::typeCheck()
{

    if (decl1 != nullptr)
        decl1->typeCheck();

    if (decl2 != nullptr)
        decl2->typeCheck();
}
void ConstDeclList::genCode()
{
       if (decl1 != nullptr)
        decl1->genCode();

    if (decl2 != nullptr)
        decl2->genCode();
}

void DeclList::typeCheck()
{

    if (decl1 != nullptr)
        decl1->typeCheck();

    if (decl2 != nullptr)
        decl2->typeCheck();
}
void DeclList::genCode()
{
     if (decl1 != nullptr)
        decl1->genCode();

    if (decl2 != nullptr)
        decl2->genCode();
}

void ConstDeclInitStmt::typeCheck()
{

    if (initVal != nullptr)
        initVal->typeCheck();

}
void ConstDeclInitStmt::genCode()
{
    
    Operand* addr;
    SymbolEntry* addr_se;
    addr_se = new IdentifierSymbolEntry(*dynamic_cast<IdentifierSymbolEntry*>(id->getSymPtr()));
    addr_se->setType(new PointerType(id->getSymPtr()->getType()));
    addr = new Operand(addr_se);
    dynamic_cast<IdentifierSymbolEntry*>(id->getSymPtr())->setAddr(addr);
    Instruction* g;
    //initVal->genCode();
    g = new GlobalInstruction(new Operand(id->getSymPtr()), new Operand(initVal->getSymPtr()), id->getSymPtr());
    if (initVal != nullptr)
        initVal->genCode();
    g->output();

}

void DeclInitStmt::typeCheck()
{
    if (initVal != nullptr)
        initVal->typeCheck();
}
void DeclInitStmt::genCode()
{
    //std::cout<<"declInit gencode"<<std::endl;
    IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry *>(id->getSymPtr());
    if(se->isGlobal())
    {
        //std::cout << "fuck" << std::endl;
        Operand *addr;
        SymbolEntry *addr_se;
        addr_se = new IdentifierSymbolEntry(*se);
        addr_se->setType(new PointerType(se->getType()));
        addr = new Operand(addr_se);
        se->setAddr(addr);
        Instruction *g;
        initVal->genCode();
        new StoreInstruction(addr, initVal->getOperand(), builder->getInsertBB());
        g = new GlobalInstruction(new Operand(id->getSymPtr()), initVal->getOperand(), se);
        g->output();
    }
    else if(se->isLocal())
    {
        switch (se->getType()->get_range())
        {
        case 2:
            Function * func = builder->getInsertBB()->getParent();
            BasicBlock* entry = func->getEntry();
            Instruction* alloca;
            Operand* addr;
            SymbolEntry* addr_se;
            Type* type;
            type = new PointerType(se->getType());
            addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
            addr = new Operand(addr_se);
            alloca = new AllocaInstruction(addr, se);                   // allocate space for local id in function stack.
            
            entry->insertFront(alloca);
            initVal->genCode(); 
            new StoreInstruction(addr, initVal->getOperand(), builder->getInsertBB());                                // allocate instructions should be inserted into the begin of the entry block.
            se->setAddr(addr);
            break;
        }
                                               // set the addr operand in symbol entry so that we can use it in subsequent code generation.
    }
    else
    {
        std::cout << "what the hell are you trying to decl?!" << std::endl;
    }
}

void FunctCall::typeCheck()
{
    paramVector.clear();
    if (para_node != nullptr)
        para_node->typeCheck();
  
     int paramR_num=paramVector.size();
     SymbolEntry* s =this->symbolEntry;
    FunctionType* type=(FunctionType*)s->getType();
    int paramF_num=type->paramsType.size();
     if(paramR_num!=paramF_num)
     {
          //printf("Fnum:%d,Rnum:%d\n",paramF_num,paramR_num);
          fprintf(stderr, "function  \'%s\'has wrong  params num!\n",symbolEntry->toStr().c_str());
    }
    std::vector<Type*> paramR = paramVector;
    std::vector<Type*> paramF = type->paramsType;
    bool equal = true;
    if (paramR.size() == paramF.size())
    {
    
    for (size_t i = 0; i < paramR.size(); ++i)
    {
        if (paramR[i] != paramF[i])
        {
            equal = false;
            break;
        }
    }
    }
    if(!equal)
    {
         fprintf(stderr, "function  \'%s\'has wrong params type\n",symbolEntry->toStr().c_str());
    }
    
}
void FunctCall::genCode()
{
    //std::cout<<"functcall_gencode"<<std::endl;
    if(para_node!=nullptr)
    {
        //para_operands.clear();
        std::vector<Operand*> stack_top;
        para_operands_stack.push_back(stack_top);
        para_node->genCode();
    }
    //std::cout<<"paranode_gencode"<<std::endl;
    BasicBlock *bb = builder->getInsertBB();
    // fprintf("ssssssssss%s")
    if (para_node != nullptr) {
        new CallInstruction(dst, symbolEntry, para_operands_stack[para_operands_stack.size() - 1], bb);
        para_operands_stack.pop_back();
    }
    else
    {
        std::vector<Operand*> empty_vec;
        new CallInstruction(dst, symbolEntry, empty_vec, bb);
    }


    if (need_2_be_bool)
    {
        SymbolEntry* se_const0 = new ConstantSymbolEntry(TypeSystem::intType, 0);
        ExprNode* temp_const0 = new Constant(se_const0);

        SymbolEntry* addr_se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        Operand* temp_result = new Operand(addr_se);

        new CmpInstruction(CmpInstruction::NE, temp_result, dst, temp_const0->getOperand(), builder->getInsertBB());


        BasicBlock* temp_bb = nullptr;// = new BasicBlock(func);
        CondBrInstruction* temp_cb = new CondBrInstruction(temp_bb, temp_bb, temp_result, builder->getInsertBB());
        //std::cout << "fuck\n";

        i_true_list.emplace_back(temp_cb);
        i_false_list.emplace_back(temp_cb);
    }
}

void ParaNode::typeCheck()
{
    
    if(!is_link)
    {
        Type *type=para_expr->getSymPtr()->getType();
        paramVector.push_back(type);
    }
    if (para1 != nullptr)
        para1->typeCheck();
    if (para2 != nullptr)
        para2->typeCheck();
    if (para_expr != nullptr)
        para_expr->typeCheck();
}
void ParaNode::genCode()
{

    if(!is_link)
    {
        if (now_is_def_funct)
        {
            Function* func = builder->getInsertBB()->getParent();
            BasicBlock* entry = func->getEntry();
            Instruction* alloca;
            Operand* addr;
            SymbolEntry* addr_se;
            Type* type;
            type = new PointerType(para_expr->getSymPtr()->getType());
            addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
            addr = new Operand(addr_se);
            alloca = new AllocaInstruction(addr, para_expr->getSymPtr());                   // allocate space for local id in function stack.
            entry->insertFront(alloca);                                 // allocate instructions should be inserted into the begin of the entry block.
            dynamic_cast<IdentifierSymbolEntry*>(para_expr->getSymPtr())->setAddr(addr);



            SymbolEntry * temp_src_addr = new TemporarySymbolEntry(para_expr->getSymPtr()->getType(), SymbolTable::getLabel());
            Operand* temp_src = new Operand(temp_src_addr);
            new StoreInstruction(addr, temp_src, entry);

            func->add_para(temp_src);
        }
        else 
        {
            para_operands_stack[para_operands_stack.size() - 1].push_back(para_expr->getOperand());
            para_expr->genCode();
        }
    }
    else 
    {
        if(para1!=nullptr)
        {
            para1->genCode();
        }
        if(para2!=nullptr)
        {
            para2->genCode();
        }
        
    }
    
}

void InitNode::typeCheck()
{
    if (node1 != nullptr)
        node1->typeCheck();
    if (node2 != nullptr)
        node2->typeCheck();
    if (value_here != nullptr)
        value_here->typeCheck();
}
void InitNode::genCode()
{

}

void UnaryExpr::typeCheck()
{
    if (expr != nullptr)
        expr->typeCheck();
}
void UnaryExpr::genCode()
{
    expr->genCode();
    if (op == SUB) 
    {

        Operand* src2 = expr->getOperand();
        Operand* temp = src2;
        int opcode = UnaryInstruction::SUB;
        new UnaryInstruction(opcode, dst, temp, builder->getInsertBB());
    }
    else if( op==NOT)
    {
        // if(闂備焦瀵ч崘濠氬箯閿燂拷2)
        // !(bool)
        if(expr->need_2_be_bool)
        {
             new NotInstruction(this->dst,expr->getOperand(), builder->getInsertBB());
        }
        else
        {
            need_2_be_bool = 1;
            int opcode = CmpInstruction::E;
            Operand *src = expr->getOperand();
            SymbolEntry *se_const0 = new ConstantSymbolEntry(TypeSystem::intType, 0);
            ExprNode *temp_const0 = new Constant(se_const0);
            Operand *src0_const0 = temp_const0->getOperand();
            Operand *src0 = src0_const0;
            SymbolEntry *s = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
            Operand *n = new Operand(s);
            if (dst->getType() != src->getType())
            {
                //std::cout<<"fuck"<<std::endl;
               // std::cout<<dst->toStr()<<std::endl;
                //std::cout<<src->toStr()<<std::endl;
                new ZextInstruction(n,dst,builder->getInsertBB());
            }    
            new CmpInstruction(opcode, this->dst, src, src0, builder->getInsertBB());
        }
    }
    if (need_2_be_bool)
    {
        dst->set_bool();
        BasicBlock* temp_bb = nullptr;// = new BasicBlock(func);
        CondBrInstruction* temp_cb = new CondBrInstruction(temp_bb, temp_bb, dst, builder->getInsertBB());
        i_true_list.emplace_back(temp_cb);
        i_false_list.emplace_back(temp_cb);
    }
    

}



//闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崰姘熼埀顒勬倵濞堝灝鏋熼柟顔煎€搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛鐏炵硶鍋撳畷鍥ㄦ畷闂侀€炲苯澧寸€规洑鍗冲浠嬵敇閻愮數鏆柣鐔哥矋閺屻劑鎮鹃柨瀣檮闁告稑锕ゆ禍婊堟⒑閸涘﹦绠撻悗姘煎墮閳绘捇濡烽埡鍌楁嫽闂佺ǹ鏈悷褔藝閿曞倹鐓欓悹鍥囧懐鐦堥悗瑙勬礀濠€閬嶅箟閹绢喖绀嬫い鎺嗗亾鐎殿喖娼″娲捶椤撯剝顎楅梺鍝ュУ閻楃娀骞冮垾鎰佹建闁逞屽墴瀵鎮㈤崨濠勭Ф婵°倧绲介崯顖烆敁瀹ュ鈷戠紒瀣儥閸庢劙鏌涢弮鈧悷鈺呯嵁閸愩剮鏃堝川椤撶喎绁舵俊鐐€栭幐楣冨磹椤愶箑顫呴幒铏濠婂牊鐓忓鑸电☉椤╊剛绱掗悩鎰佺劷缂佽鲸甯楀蹇涘Ω閿曗偓闂夊秹姊洪悷鏉挎Щ闁硅櫕锚閻ｇ兘顢曢敃鈧粈瀣棯閻楀煫顏呯閸撗€鍋撻崗澶婁壕闂佸憡娲﹂崜娑㈠储闁秵鈷戦梻鍫熻儐瑜版帒纾块柟鎯版閺勩儵鏌ㄥ┑鍡樼闁稿鎸搁埢鎾诲垂椤旂晫浜俊鐐€ら崢楣冨礂濡警鍤曟い鎰剁畱缁犳稒銇勯幘璺轰户缂佹劗鍋炵换婵嬫偨闂堟刀銏ゆ倵濮樺崬鍘寸€规洏鍎靛畷銊р偓娑櫱氶幏缁樼箾鏉堝墽鎮奸柟铏崌椤㈡艾饪伴崨顖滐紲闁荤姴娲﹁ぐ鍐焵椤掆偓濞硷繝鎮伴钘夌窞濠电偟鍋撻～宥夋⒑闂堟稓绠冲┑顔惧厴椤㈡瑩骞掗弮鍌滐紳闂佺ǹ鏈悷褔宕濆鍡愪簻妞ゆ挾鍋為崰妯尖偓瑙勬磸閸ㄤ粙鐛弽銊﹀闁稿繐顦扮€氬ジ姊绘担鍛婂暈缂佸鍨块弫鍐晲閸ヮ煈鍋ㄩ梻渚囧墮缁夌敻鎮￠弴銏＄厽婵☆垵顕ф晶顖涚箾閸喓鐭岄柍褜鍓濋～澶娒哄鈧畷婵嗏枎閹捐泛绁﹂梺鎼炲労閸撴岸宕戠€ｎ喗鐓曟い鎰Т閻忊晜顨ラ悙鏉戝闁哄矉绲鹃幆鏃堝灳閸愯尪绶熼梻浣呵归鍡涘箲閸ヮ剙绠栨俊銈呮噺閺呮煡骞栫划鍏夊亾閹颁焦楠勯梻鍌欐缁鳖喚绮婚幋锔藉亱闁规崘顕х粻鏍ㄤ繆閵堝懎鏆為柛鐘叉閺屾盯寮撮妸銉ヮ潾濡炪倧璐熼崝鎴﹀蓟閿濆顫呴柍杞拌兌娴狀參鏌ｉ姀鈺佺仯闁哥姵鐗犻獮鍐潨閳ь剟鐛幒鎳虫梹鎷呴崫鍕闂傚倷鑳剁划顖炲礉閺囥埄鏁嬫い鎾卞灩閻撴繈鏌熼崜褏甯涢柣鎾冲暟閹茬ǹ饪伴崼婵堫槶濠电偛妫欓幐鎼佸磼閵娾晜鐓涚€广儱鍟俊鍧楁煟濠靛棛鍩ｉ柡宀嬬節瀹曞爼濡烽妷褌鐥梻浣瑰▕閺€閬嶅垂閸ф钃熸繛鎴炃氶弨浠嬫煕閳╁喚娈㈠ù灏栧亾闂傚倷绀侀悿鍥敋瑜旇棟闁汇垻枪閽冪喐绻涢幋鐐茬劰闁稿鎹囬弫宥夊础閻愬弶娈ㄦ繝銏ｎ潐濞茬喎顫忔繝姘＜婵炲棙甯掗崢锟犳煛鐎ｅ吀绨奸柕鍥у楠炲鈹戦崶鑸碉紗婵°倗濮烽崑鐐哄礉濞嗘挾宓侀柡宥冨妿閻熺懓鈹戦悩鎻掝伀闁崇鍊栫换婵嬫偨闂堟稈鏋呭┑鐐板尃閸ヨ埖鏅為梺鍦濠㈡﹢寮告笟鈧弻娑㈠焺閸愵亖濮囬梺缁樻尰濞茬喖骞冨鈧幃娆撳箵閹哄棙瀵栭梻浣芥〃閻掞箓鎮ч悩璇茶摕闁哄洨鍠撶粻楣冩煟閹伴潧澧柣婵囨礋濮婃椽骞庨懞銉︽殸闂佹悶鍔屽鈥愁嚕鐠囨祴妲堟俊顖炴敱閺傗偓闂備胶纭堕崜婵嬨€冭箛鏂款嚤闁稿本鍑瑰〒濠氭煏閸繈顎楀ù婊冨⒔缁辨帡鎮╅搹顐㈢３閻庢鍠涢褔鍩ユ径鎰潊闁绘鏁稿澶愭⒒娓氣偓閳ь剛鍋涢懟顖涙櫠鐎电硶鍋撶憴鍕缂傚秴锕ら悾椋庣矙鐠囩偓妫冨畷姗€濡歌閸嬪﹪姊婚崒娆戭槮闁硅绻濆畷婵嬫晝閸屾氨鐛ラ梺鍝勭▉閸欏繒绱為弽顓熺厱婵炴垶顭囬幗鐘绘煟閹惧磭绠婚柡灞剧洴椤㈡洟鏁愰崶鈺冩毇闂備線娼荤徊钘夛耿鏉堚晜顫曢柟鐑橆殔缁犳稒銇勯幘璺盒涘┑顔兼川缁辨挻鎷呴搹鐟扮缂備浇顕ч悧鎾荤嵁閸愨晝顩烽悗锝庡亽濡啫鈹戦悙娈挎缂傚牅鍗冲畷瑙勭鐎ｎ亞鐣哄┑掳鍊愰崑鎾淬亜椤愶絿绠炴慨濠呭吹閳ь剟娼ч幉锟犲疾閸忚偐绡€缁剧増菤閸嬫捇宕橀懠顒勭崜闂備礁鎲″褰掓偡閵夆晩鏁嬮柨婵嗩槸缁犵粯銇勯弮鍌楁嫛婵炵厧锕娲箮閼恒儲娈伴梺绋款儐閹瑰洭寮婚悢鍓叉Ч閹艰揪绲界粭锛勭磽娴ｈ櫣甯涚紒瀣笒椤洩绠涘☉妯碱槶閻熸粌绉瑰畷鍝勭暆閸曨兘鎷虹紒缁㈠幖閹冲氦顣跨紓鍌欑贰閸犳牕霉閻戣棄绀嗛柟鐑橆殔缁秹鏌涢銈呮瀻闁逞屽墰缁垱绌辨繝鍥舵晬闁绘劕鐡ㄩ悵宕囩磽娴ｉ潧濡兼い顓炲槻椤繐煤椤忓拋妫冨┑鐐村灱妞存悂顢撻崱娑欌拻闁稿本鑹鹃鈺呮倵濮樼厧澧撮柟顔藉劤閻ｏ繝骞嶉鑺ョ暦闂備線鈧偛鑻晶鎾煕閳规儳浜炬俊鐐€栫敮鎺楀窗濮橆剦鐒介柟閭﹀幘缁犻箖鏌涘▎蹇ｆ闁兼澘娼￠弻鏇㈠幢濡も偓閺嗭絿鈧娲忛崝宥咁焽韫囨稑绀堢憸蹇涘煟閵堝棛绡€闁汇垽娼у瓭闂佹寧娲忛崐婵嬪箖瑜斿畷鍗炩槈濞嗗繆鍋撻崸妤佺厱妞ゎ厽鍨垫禍鏍瑰⿰鍕煉闁哄瞼鍠栧畷顐﹀礋椤掑顥ｅ┑鐐茬摠缁秹宕曢幎瑙ｂ偓鏃堝礃椤斿槈褔鏌涢埄鍐剧劷闁宠绋撶槐鎾诲磼濮樻瘷锛勭磼閼搁潧鍝哄┑鈥崇摠缁楃喖鍩€椤掆偓椤曪綁骞橀纰辨綂闂佹枼鏅涢崯顖炴偟閺嶎厽鈷掑〒姘ｅ亾闁逞屽墰閸嬫盯鎳熼娑欐珷闁规鍠氱壕鍏笺亜閺傚灝鈷旈悽顖涚〒缁辨帞绱掑Ο鑲╃暤濡炪値鍋呯换鍫ャ€佸鈧幃娆撳级閸噮鍤欓梻鍌氬€搁崐鐑芥倿閿曞倸绠栭柛顐ｆ礀绾炬寧绻濇繝鍌滃缂佲偓閸愨斂浜滈煫鍥ㄦ尵婢с垻鈧娲樼划宀勫煘閹达附鏅柛鏇ㄥ亗閺夘參姊虹粙鍖℃敾闁搞劌鐏濋悾鐑藉即閵忊€虫濡炪倖甯婄粈浣规償婵犲洦鈷戦柛鎾村絻娴滄繄绱掔拠鑼㈡い顓炴喘瀵粙濡歌椤旀洟鎮楅悷鏉款棌闁哥姵娲滈懞閬嶅礂缁楄桨绨婚梺闈涱槶閸庤櫕鏅跺☉姘辩＜缂備焦顭囧ú瀛橆殽閻愬樊鍎旈柟顔界懅閹瑰嫭绗熼娑辨（婵犵绱曢崑鎴﹀磹瑜忓濠冪鐎ｎ亞顔愬銈嗗姧缁犳垿鎮″鈧弻鐔告綇閸撗呮殸闂佺粯鎼换婵嬪蓟濞戙垹鐒洪柛鎰剁細濞岊亞绱撴担闈涘闁告鍥ｂ偓鏃堝礃椤斿槈褔鏌涢幇鈺佸妞ゎ剙鐗撳娲箰鎼淬垹顦╂繛瀛樼矤娴滄繃绌辨繝鍕ㄥ亾濞戞瑯鐒介柣鐔风秺閺屽秷顧侀柛鎾寸懇椤㈡岸鏁愭径妯绘櫔闂侀€炲苯澧柣锝囧厴閹剝鎯斿Ο缁樻澑闂佽鍑界紞鍡樼濠靛姹叉い蹇撶墛閳锋帡鏌涚仦鍓ф噮妞わ讣绠撻弻鐔哄枈閸楃偘绨婚柣鎾卞€濋弻鏇熺箾閻愵剚鐝旈梺姹囧€ら崳锝夊蓟濞戞粠妲煎銈冨妼閹虫劗鍒掓繝姘兼晬婵炴垶姘ㄩ鏇㈡倵閻熸澘顥忛柛鐘虫礈閼鸿鲸绺介崨濠勫幗闂佽宕樺▔娑㈠几濞戙垺鐓涚€光偓鐎ｎ剙鍩岄柧浼欑秮閺屾稑鈹戦崱妤婁患缂備焦顨忛崣鍐潖濞差亝鍋傞幖绮规濡本绻涚€涙鐭ゅù婊庝簻椤曪絿鎷犲ù瀣潔闂侀潧绻掓慨鐢杆夊┑瀣厽闁绘ê鍘栭懜顏堟煕閺傚潡鍙勭€规洘绻堥、娑㈡倷閺夋垟鍋撻崹顐ょ闁割偅绻勬禒銏ゆ煛鐎ｎ剙鏋涢柡宀€鍠栭、娆撴偂鎼存ê浜鹃柛褎顨嗛崑妯汇亜閺冨牊鏆滈柛瀣崌閺佹劖鎯旈埄鍐憾闂備礁鎼幊蹇曞垝閹捐钃熼柨婵嗩槹閺呮煡鏌涘☉娆愮凡妞ゅ浚鍘艰灃闁绘﹢娼ф禒锕傛煕閺冣偓閻熴儵锝炶箛娑欐優閻熸瑥瀚悵浼存⒑閸︻厾甯涢悽顖涘笒琚欓柟閭﹀枤绾句粙鏌涚仦鐐殤鐎涙繂鈹戦埥鍡椾簼闁荤啙鍛灊閻庯綆浜堕崥瀣熆鐠虹尨韬柛鐐茬埣濮婃椽宕崟顒€绐涢梺鍝ュТ鐎涒晝绮嬪鍛傛棃宕ㄩ瑙勫缂傚倷绀侀鍫濃枖閺囩姷涓嶉柟鎯板Г閻撳啰鈧懓瀚竟鍡樻櫠閺囥垺鐓冪紓浣股戠亸鎵磼閸屾稑绗ч柍褜鍓ㄧ紞鍡涘磻閸℃稑鍌ㄥù鐘差儐閳锋垹鎲搁悧鍫濈瑨濞存粈鍗抽弻娑㈠Ω閵堝懎绁梺璇″灠閸熸挳骞栬ぐ鎺戞嵍妞ゆ挾濯寸槐鏌ユ⒒娴ｈ櫣甯涢柨姘繆椤栨熬韬柟顔瑰墲缁轰粙宕ㄦ繝鍕箰闁诲骸鍘滈崑鎾绘倵閿濆骸澧伴柨娑氬枑缁绘稓鈧數枪椤庢挸鈹戦垾铏枠妤犵偛绻橀幃鈺冩嫚閼碱剦鍞介梻浣告贡閸庛倗鎹㈤崟顖氱劦妞ゆ帒鍊搁崢瀛樻叏婵犲嫮甯涚紒妤冨枛閸┾偓妞ゆ帒瀚悞鍨亜閹烘垵鈧憡绂掑⿰鍫熺厾婵炶尪顕ч悘锟犳煛閸涱厾鍩ｇ€规洘绮忛ˇ瀵哥棯閸撗勬珪闁逞屽墮缁犲秹宕曟潏鈹惧亾濮樼厧娅嶉挊婵嬫煥閺傛娼熷ù婊勭矋閵囧嫰骞樼捄杞版勃缂備礁鏈€笛囧Φ閸曨垱鏅濋柍褜鍓涙竟鏇㈩敇閻樻剚娼熼梺鍦劋椤ㄥ懐鐚惧澶嬬厱閻忕偛澧介。鏌ユ煟鎼淬値鍤欐い顏勫暣婵¤埖鎯旈垾鑼嚬缂傚倷绶￠崰妤呭箲閸パ呮殾婵炲樊浜滈悞鍨亜閹哄秹妾峰ù婊勭矒閺岀喖宕崟顒夋婵炲瓨绮嶉崕鎶解€旈崘顔嘉ч幖绮光偓鑼泿缂傚倷鑳剁划顖炴晝閵忋倗宓侀柡宥庡厵娴滃綊鏌熼悜妯虹仸鐎规挸绉撮—鍐Χ閸℃ê鏆楁繝娈垮枤閸忔ê鐣烽姀銈庢晬闁绘劗琛ラ幏缁樼箾鏉堝墽鍒伴柟璇х節瀹曨垶鎮欓悜妯煎幗闂佺懓鐏濋崯顐㈩嚕椤斿皷鏀介柍銉ョ－閸╋絿鈧娲忛崝鎴︺€佸▎鎾村亗閹肩补妲呭Λ鎰攽閻樺灚鏆╁┑顔惧厴瀵偊宕ㄦ繝鍐ㄥ伎闂佺粯鍨甸～鏇㈠焵椤掆偓閸婂潡骞愭繝鍐彾闁冲搫顑囩粔顔锯偓瑙勬磸閸旀垵顕ｉ崼鏇炵闁绘瑥鎳愰獮銏ゆ⒒閸屾瑧顦﹂柟娴嬧偓瓒佹椽鏁冮崒姘憋紱闂佸湱鍎ら〃鍡涘磹閸啔褰掓偐瀹割喖鍓遍梺缁樻尪閸庣敻寮婚敐澶婃闁圭ǹ瀛╅崕鎾绘煕閻斿憡鍊愰柟顔筋殘閹叉挳宕熼鈧惌顔剧磽娴ｆ彃浜炬繛鎾村焹閸嬫捇鏌嶉妷顖滅暤鐎规洜枪铻栧ù锝呮啞閻ゅ倿姊婚崒姘偓鎼佹偋婵犲嫮鐭欓柟杈捐缂嶆牜鈧厜鍋撻柛鏇ㄥ墰閸橀潧顪冮妶鍡欏缂侇喖鐭傚绋库槈濞嗘劕寮挎繝鐢靛Т閸燁垶濡靛┑鍫氬亾鐟欏嫭绀冮柨鏇樺灩閻ｇ兘鏁愰崱妤冪獮闁诲函缍嗘禍婊勭珶瀹ュ鈷掗柛灞剧懆閸忓矂寮搁鍫熺叆闁哄洦锚閳ь剚绻堥崹楣冩晝閳ь剟鎮鹃敓鐘茬妞ゆ棁濮ら鏇㈡⒒娴ｇ懓顕滅紒璇插€胯棟妞ゆ牜鍋熷畵渚€鏌曡箛銉х？缂佺娀绠栭弻鐔衡偓鐢登规禒褔鏌熼幓鎺撳仴闁哄瞼鍠栭幃鍧楊敍濞戝彉鍝楃紓鍌欐祰妞存悂骞戦崶褏鏆﹂柟鐑樺灍閺嬪酣鏌熺€涙绠撴い顒€顦靛娲寠婢跺﹥娈绘繛瀛樼矊閻栫厧顕ｆ繝姘櫜濠㈣泛锕﹂鎺楁⒑閸涘⿴娈橀柛瀣⊕閹便劑鏁冮崒娑掓嫼闂佸憡绻傜€氼噣鎮橀柆宥嗙厱闁绘ǹ娅曞畷灞绢殽閻愯尙绠婚柟顔规櫆缁楃喖顢涘ù棰濆墰缁辨捇宕掑▎鎺濆敼濠碉紕瀚忛崶褏锛涢梺鐟板⒔缁垶鍩涢幒鎳ㄥ綊鏁愰崶鈺傛啒闂佹悶鍊曢崯鎾蓟濞戙垺鍋愰柟棰佺劍閻や線姊烘潪鎵槮闁哥喎鐡ㄩ幈銊╁焵椤掑嫭鐓忛柛顐ｇ箓閺嗗﹪鏌涚€ｎ偅灏伴柟宄版嚇瀹曟粓宕ｆ径濠傚帪闂備浇宕垫繛鈧紓鍌涜壘閳诲秹鏁愭径濠勵槷闂佹寧娲嶉崑鎾淬亜椤忓嫬鏆ｅ┑鈥崇埣瀹曞崬螖閸愵亝鍣梻浣筋嚙鐎涒晠宕欒ぐ鎺戠闁绘梻鍘х粻顖炴煙鏉堥箖妾柛瀣閺屾稖绠涘顑挎睏闂佸憡眉缁瑥顫忓ú顏勪紶闁告洦鍋€閸嬫捇骞栨担鐟扳偓鑸电節闂堟稓鎳佸璺烘捣閻も偓闂佸搫鍊告晶鐣岀不濮樿埖鈷戦柛锔诲幘鐢盯鎮介婊冧户缂侇喗妫冮幃婊冾潨閸℃﹩鍟嶉梻浣虹帛閸旀洟顢氶鐔告珷闂侇剙绉甸悡鏇㈡煙閻戞ɑ灏繛鎼枤閳ь剝顫夊ú姗€銆冮崱妤婂殫闁告洦鍓涚弧鈧繛杈剧到婢瑰﹤螞濠婂牊鈷掗柛灞剧懅椤︼箓鏌熺拠褏绡€鐎规洘绻傝灒闂傚倸顕崜銊モ攽閻愬弶顥為柟灏栨櫊瀵偊宕堕妸褏顔曢悗鐟板閸犳洜鑺辨總鍛婄厓闂佸灝顑呭ù顕€鏌＄仦鍓с€掑ù鐙呯畵瀹曟粏顦抽柛锝庡幘缁辨挻鎷呴悿顖氬帯婵犫拃鍕垫畷缂佸矁椴哥换婵嬪磼閵堝棗鐦滈梻浣虹帛閹稿憡顨ラ幖渚婄稏闁搞儺鍓氶埛鎴︽偣閸ワ絺鍋撻搹顐や壕闂備胶枪椤戝啴宕愬┑鍡╁殨妞ゆ劧绠戠粈鍐┿亜閺冨洤浜归柨娑欑矊閳规垿顢欓弬銈堚偓璺ㄧ棯椤撶喐鍊愮€规洦鍓濋妵鎰板箳閹绢垱瀚藉┑鐐舵彧缂嶁偓妞ゎ偄顦靛畷鎴︽偐缂佹鍘遍柟鍏肩暘閸ㄥ鎯岀仦鐭綊鎮崨顖滄殼闂佺懓纾崑銈嗕繆閻戠瓔鏁婇柛蹇曞帶娴滃吋绻濋悽闈浶ユい锝庡枤濡叉劙寮撮姀鐘碉紱闂佺鎻粻鎴犲瑜版帗鐓涚€广儱楠告禍婵嬫煛閸℃鐭掗柡宀€鍠栭幃婊兾熼懡銈呭箰闂備胶枪椤戝棝骞愰幖渚婄稏婵犻潧顑嗛崑鍌炲箹缁顫婃繛鑲╃帛缁绘繈鎮介棃娑楁勃閻熸粍婢橀崯顐︹€﹂崶顒€鐏抽柟棰佺劍缂嶅海绱撻崒娆戝妽閽冨崬鈹戦娑欏唉闁哄睙鍡欑杸闁规儳澧庨弳娑㈡⒑闁偛鑻晶顕€鏌ㄩ弴銊ら偗鐎殿喛顕ч埥澶愬閻樻鍟嬮梺璇查叄濞佳囧箺濠婂牊鍋柛鏇ㄥ灡閳锋垿姊婚崼鐔衡姇妞ゃ儲绮撻弻娑㈡偄妞嬪函绱炵紓渚囧枦椤曆囧煡婢舵劕顫呴柣妯诲墯濡喐绻濋悽闈涗粶婵☆偅鐟╁畷鏇㈠礃濞村鐏佹繝闈涘€搁幉锟犲煕閹烘嚚褰掓晲閸パ冨闂佹悶鍊愰崑鎾斥攽閻橆偅濯伴柛娑卞枟閻濐亪姊洪崨濠傜瑲閻㈩垽绻濋妴浣糕槈閵忊€斥偓鐑芥煠绾板崬澧鐐茬墦濮婄粯鎷呴崨濠呯闂佺ǹ绨洪崐婵嗙暦闂堟侗娼╅柣鎰灊濮规姊洪崷顓炲妺闁规瓕顕ч埢宥呪堪閸啿鎷洪梺鍛婃尰瑜板啯绂嶅┑鍫㈢＜妞ゆ棁妫勯埢鏇燁殽閻愯揪鑰跨€规洘绮嶉幏鍛存儌閸濄儳顢呮繝纰夌磿閸嬫垿宕愰弽顓炵濡わ絽鍟壕濠氭煟閺冨倸甯堕柡鍕╁劦閺屻劌鈹戦崱鈺傂ч梺缁樻尰閻╊垶骞冨鈧幃娆撳箵閹哄棙瀵栨繝鐢靛仜瀵爼骞愰幎钘夎摕闁斥晛鍟刊鎾煟閻斿憡绶叉い顐ｅ灩缁辨挻鎷呴崫鍕戯綁鏌ｉ埡濠傜仩妞ゆ洩缍侀獮姗€顢欓挊澶夋睏闂備焦鍎冲ù姘跺磻閹烘鐤悘鐐插⒔缁♀偓閻庡吀鍗抽弨鍗烆熆濮椻偓閸┾偓妞ゆ帊绶″▓鏇㈡煙娓氬灝濮傞柟顔界矒閹稿﹥寰勫畝瀣耿濠电姷鏁搁崑鐐哄垂閸撲礁鏋堢€广儱鎳愰弳锕傛煕濞嗗浚妲归柛娆忕箲娣囧﹪鎮欐０婵嗘婵炲瓨绮撶粻鏍箖濡も偓閳绘捇宕归鐣屽蒋闂備胶枪椤戝懘鏁冮鍫熷仒妞ゆ柨妲堥悢鍏煎殐闁冲搫鍠涚槐鍙夌節绾版ɑ顫婇柛銊ョ－閸掓帡顢涢悜鍡樻櫆闂佺粯鏌ㄩ崥瀣煕閹烘嚚褰掓晲閸粳鎾剁棯椤撶偟鍩ｉ柡灞剧洴閺佹劘绠涢弴鐘虫婵°倗濮烽崑娑氭崲濮椻偓瀵偊骞樼紒妯绘闂佽法鍣﹂幏锟�?


std::string ExprNode::get_name()
{
    return symbolEntry->toStr();
}









void BinaryExpr::output(int level)
{
    /*float temp_store = cal_expr_val();
    if (temp_store != PRE_CAL_ERROR_MEETING_VAL)
    {
        if (getSymPtr()->getType()->isFLOAT())
            fprintf(yyout, "%*c\tExprValue:\t%f\n", level, ' ', temp_store);
        else if (getSymPtr()->getType()->isInt())
            fprintf(yyout, "%*c\tExprValue:\t%d\n", level, ' ', (int)temp_store);
        return;
    }*/
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崰姘熼埀顒勬倵濞堝灝鏋熼柟顔煎€搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛鐏炵硶鍋撳畷鍥ㄦ畷闂侀€炲苯澧寸€规洑鍗冲浠嬵敇閻愮數鏆柣鐔哥矋閺屻劑鎮鹃柨瀣檮闁告稑锕ゆ禍婊堟⒑閸涘﹦绠撻悗姘煎墮閳绘捇濡烽埡鍌楁嫽闂佺ǹ鏈悷褔藝閿曞倹鐓欓悹鍥囧懐鐦堥悗瑙勬礀濠€閬嶅箟閹绢喖绀嬫い鎺嗗亾鐎殿喖娼″娲捶椤撯剝顎楅梺鍝ュУ閻楃娀骞冮垾鎰佹建闁逞屽墴瀵鎮㈤崨濠勭Ф婵°倧绲介崯顖烆敁瀹ュ鈷戠紒瀣儥閸庢劙鏌涢弮鈧悷鈺呯嵁閸愩剮鏃堝川椤撶喎绁舵俊鐐€栭幐楣冨磹椤愶箑顫呴幒铏濠婂牊鐓忓鑸电☉椤╊剛绱掗悩鎰佺劷缂佽鲸甯楀蹇涘Ω閿曗偓闂夊秹姊洪悷鏉挎Щ闁硅櫕锚閻ｇ兘顢曢敃鈧粈瀣棯閻楀煫顏呯閸撗€鍋撻崗澶婁壕闂佸憡娲﹂崜娑㈠储闁秵鈷戦梻鍫熻儐瑜版帒纾块柟鎯版閺勩儵鏌ㄥ┑鍡樼闁稿鎸搁埢鎾诲垂椤旂晫浜俊鐐€ら崢楣冨礂濡警鍤曟い鎰剁畱缁犳稒銇勯幘璺轰户缂佹劗鍋炵换婵嬫偨闂堟刀銏ゆ倵濮樺崬鍘寸€规洏鍎靛畷銊р偓娑櫱氶幏缁樼箾鏉堝墽鎮奸柟铏崌椤㈡艾饪伴崨顖滐紲闁荤姴娲﹁ぐ鍐焵椤掆偓濞硷繝鎮伴钘夌窞濠电偟鍋撻～宥夋⒑闂堟稓绠冲┑顔惧厴椤㈡瑩骞掗弮鍌滐紳闂佺ǹ鏈悷褔宕濆鍡愪簻妞ゆ挾鍋為崰妯尖偓瑙勬磸閸ㄤ粙鐛弽銊﹀闁稿繐顦扮€氬ジ姊绘担鍛婂暈缂佸鍨块弫鍐晲閸ヮ煈鍋ㄩ梻渚囧墮缁夌敻鎮￠弴銏＄厽婵☆垵顕ф晶顖涚箾閸喓鐭岄柍褜鍓濋～澶娒哄鈧畷婵嗏枎閹捐泛绁﹂梺鎼炲労閸撴岸宕戠€ｎ喗鐓曟い鎰Т閻忊晜顨ラ悙鏉戝闁哄矉绲鹃幆鏃堝灳閸愯尪绶熼梻浣呵归鍡涘箲閸ヮ剙绠栨俊銈呮噺閺呮煡骞栫划鍏夊亾閹颁焦楠勯梻鍌欐缁鳖喚绮婚幋锔藉亱闁规崘顕х粻鏍ㄤ繆閵堝懎鏆為柛鐘叉閺屾盯寮撮妸銉ヮ潾濡炪倧璐熼崝鎴﹀蓟閿濆顫呴柍杞拌兌娴狀參鏌ｉ姀鈺佺仯闁哥姵鐗犻獮鍐潨閳ь剟鐛幒鎳虫梹鎷呴崫鍕闂傚倷鑳剁划顖炲礉閺囥埄鏁嬫い鎾卞灩閻撴繈鏌熼崜褏甯涢柣鎾冲暟閹茬ǹ饪伴崼婵堫槶濠电偛妫欓幐鎼佸磼閵娾晜鐓涚€广儱鍟俊鍧楁煟濠靛棛鍩ｉ柡宀嬬節瀹曞爼濡烽妷褌鐥梻浣瑰▕閺€閬嶅垂閸ф钃熸繛鎴炃氶弨浠嬫煕閳╁喚娈㈠ù灏栧亾闂傚倷绀侀悿鍥敋瑜旇棟闁汇垻枪閽冪喐绻涢幋鐐茬劰闁稿鎹囬弫宥夊础閻愬弶娈ㄦ繝銏ｎ潐濞茬喎顫忔繝姘＜婵炲棙甯掗崢锟犳煛鐎ｅ吀绨奸柕鍥у楠炲鈹戦崶鑸碉紗婵°倗濮烽崑鐐哄礉濞嗘挾宓侀柡宥冨妿閻熺懓鈹戦悩鎻掝伀闁崇鍊栫换婵嬫偨闂堟稈鏋呭┑鐐板尃閸ヨ埖鏅為梺鍦濠㈡﹢寮告笟鈧弻娑㈠焺閸愵亖濮囬梺缁樻尰濞茬喖骞冨鈧幃娆撳箵閹哄棙瀵栭梻浣芥〃閻掞箓鎮ч悩璇茶摕闁哄洨鍠撶粻楣冩煟閹伴潧澧柣婵囨礋濮婃椽骞庨懞銉︽殸闂佹悶鍔屽锟犵嵁閸愵厹浜归柟鐑樺灩閸婄偤姊洪幐搴ｇ畵闁稿﹤鎲＄粋宥夊礈瑜夐崑鎾舵喆閸曨剛顦ュ┑鐐额嚋缂嶄礁鐣烽鈶芥棃宕ㄩ瑙勫闂佽崵濮村ú锕併亹閸愵噮鏁嗛柕蹇嬪€栭悡鐔搞亜閹炬鍟悵婵嬫倵鐟欏嫭绌跨紒缁樼箞閻涱喗鎯旈妸锕€娈熼梺闈涱槶閸庢壆鍒掕娣囧﹪鎮欓鍕ㄥ亾閺嵮屾綎濠电姵鑹鹃悡鏇㈡煕椤愶絿绠ラ柣鎺嶇矙閺屾洘绻涢悙顒佺彆闂佺粯鎸搁崯浼村箟缁嬪簱鍫柛顐ｇ箘椤︻參姊虹紒妯烩拻闁告鍡欑婵犵數鍋犻幓顏嗗緤鐠恒劍鍏滈柛顐ｆ礀閸戠娀骞栧ǎ顒€濡介柣鎾寸懇閺岋綁骞囬棃娑橆潽缂傚倸绉甸崹鍓佹崲濞戙垹閱囬柕蹇曞У閻忓牆鈹戦纭锋敾婵＄偠妫勮灋闁告劑鍔夊Σ鍫ュ级閸偆鍘涢柍褜鍓欏Λ婵嬪蓟閿濆棙鍎熼柕濠忛檮闁款參姊洪崫鍕⒈闁告挻绋撻崚鎺斺偓锝庡枛缁犳娊鏌￠崶銉ュ闁宠鏈换婵嗏枔閸喗鐏堥梺鎸庢磸閸婃繈鍨鹃敃鈧悾锟犲箥椤旇姤顔曢梻浣告贡閸庛倝宕归悢鐓庡嚑閹兼番鍔嶉悡娆撴煙绾板崬骞栨鐐搭殘閹噣鏁冮崒娑掓嫽闂佺ǹ鏈悷褔藝閿旂晫绡€闁逞屽墴閺屽棗顓奸崨顖氬箞闂備礁澹婇崑渚€宕曟潏鈺侇棜閻犲洦绁撮弨浠嬫煟濡绲婚柍褜鍓欓…宄邦嚕閹惰姤鏅濋柛灞剧〒閸樼敻姊虹紒姗嗘當闁绘锕﹀▎銏ゅ蓟閵夛腹鎷洪梺鍛婎殘閸嬬偤宕板鈧弻鈥崇暆鐎ｎ剛鐦堥悗瑙勬礀閻栧ジ宕洪埄鍐╁闂佸灝顑呴悡鏇㈡⒒閸屾瑧鍔嶉悗绗涘懐鐭欓柟鐑橆殕閸嬶繝鏌嶆潪鎵窗婵炲吋鐗楃换娑橆啅椤旇崵鐩庢繛纾嬪亹婵炩偓闁哄本鐩鎾Ω閵夈儳顔囬梻浣告惈椤戝懎螞濠靛棭娼栭柧蹇撴贡閻瑦绻涢崱妯哄姢闁告搩鍓熷铏圭矙濞嗘儳鍓遍梺鍦嚀濞层倝锝炶箛娑欐優闁革富鍘鹃悡鎾斥攽閻愬弶顥為柛搴㈠▕瀹曟粓鏌嗗鍡忔嫼闂佽崵鍠撴晶妤呭疮閻愮儤鐓欐い鏃傚帶閳ь剙娼″顐﹀箛閻楀牆鈧攱绻涢弶鎴剱闁哄倵鍋撻梻浣筋嚙閸戠晫寰婃禒瀣€舵慨妯挎硾閻ら箖鏌曡箛瀣偓鏍煕閹寸姷纾奸悗锝庡亜椤曟粍绻濋埀顒勫箥椤斿墽锛滈梺闈浨归崐妤呮儗濞嗗繆鏀介柍鈺佸暞閸婃劙鏌涢埡瀣瘈鐎规洘甯掕灃濞达絽澹婃导鏍⒒閸屾艾鈧绮堟笟鈧獮澶愭晬閸曨剙搴婇梺绋挎湰婢规洟宕戦幘鎰佹僵闁绘挸娴锋禒鎼佹⒑閹稿海绠橀柛瀣ㄥ€濆顐﹀礃椤旇偐锛滃┑鐐村灦閼归箖鐛崼銉︹拻濞达絽鎲￠幆鍫ユ煕閻斿搫鈻堢€规洘鍨块獮妯肩磼濡厧骞堥梻渚€娼ф灙闁稿孩鐓″畷鎴﹀Ψ閳哄倻鍘藉┑鐐村灥瀹曨剙鈻嶅鍥ｅ亾鐟欏嫭绀冮柨鏇樺灲閵嗕礁鈻庨幋婵囩€抽柡澶婄墑閸斿海绮旈柆宥嗏拻闁稿本鐟х粣鏃€绻涙担鍐叉处閸嬪鏌涢埄鍐槈缂佺姷濞€楠炴牗娼忛崜褎鍋ч梺缁樼矌缁垳鎹㈠☉銏犵闁绘劕鐏氶崳顔剧磽娴ｅ弶顎嗛柛瀣崌濮婄粯鎷呴崷顓熻弴闂佹悶鍔忓Λ鍕€﹂崶顏嶆Щ闂佺儵妲呴崣鍐潖閾忚瀚氶柟缁樺俯閸斿姊洪崨濠傜伇妞ゎ偄顦辩划瀣吋婢跺鈧兘鏌涘▎蹇撲喊缂傚秳绶氶悰顕€宕堕鈧痪褔鏌涢…鎴濇灈缂佷緡鍣ｅ缁樼瑹閳ь剙顭囪閹广垽宕卞Δ濠勨偓鍓佹喐閺冨牏宓侀煫鍥ㄦ礈绾惧吋淇婇婵嗕汗妞ゆ梹娲熼弻鈩冨緞婵犲嫬顣烘繝鈷€鍌滅煓鐎规洘纰嶇€佃偐鈧稒顭囬崢鐢告⒑閸忛棿鑸柛搴㈠▕閹苯鐣濋埀顒傛閹烘鏁婇柤鎭掑労濡啴姊虹拠鈥虫灍缂侇喖鐭侀悘鎺楁⒒閸屾浜鹃梺褰掑亰閸犳艾鈻旈崸妤佲拻闁稿本鐟х粣鏃€绻涙担鍐叉瘽閵娾晛鐒垫い鎺戝閳锋帡鏌涢弴銊ヤ簻妞ゅ繆鏅犻弻锝夋晲閸涱厽些闂佷紮绲剧换鍫ュ春閳ь剚銇勯幒鎴濐仼闁绘帒鐏氶妵鍕箳閹存繃鐏撳┑鐐插悑閸旀牜鎹㈠☉銏犵煑濠㈣埖绋栭埀顒€娼￠弻鏇㈠幢濡も偓閺嗭絿鈧娲忛崝宥咁焽韫囨稑绀堢憸蹇涘煟閵堝棛绡€闁汇垽娼у瓭闂佹寧娲忛崐婵嬪箖瑜斿畷鍗炩槈濞嗗繆鍋撻崸妤佺厱妞ゎ厽鍨垫禍鏍瑰⿰鍕煉闁哄瞼鍠栧畷顐﹀礋椤掑顥ｅ┑鐐茬摠缁秹宕曢幎瑙ｂ偓鏃堝礃椤斿槈褔鏌涢埄鍐剧劷闁宠绋撶槐鎾诲磼濮樻瘷锛勭磼閼搁潧鍝哄┑鈥崇摠缁楃喖鍩€椤掆偓椤曪綁骞橀纰辨綂闂佹枼鏅涢崯顖炴偟閺嶎厽鈷掑〒姘ｅ亾闁逞屽墰閸嬫盯鎳熼娑欐珷闁规鍠氱壕鍏笺亜閺傚灝鈷旈悽顖涚〒缁辨帞绱掑Ο鑲╃暤濡炪値鍋呯换鍫ャ€佸鈧幃娆撳级閸噮鍤欓梻鍌氬€搁崐鐑芥倿閿曞倸绠栭柛顐ｆ礀绾炬寧绻濇繝鍌滃缂佲偓閸愨斂浜滈煫鍥ㄦ尵婢с垻鈧娲樼划宀勫煘閹达附鏅柛鏇ㄥ亗閺夘參姊虹粙鍖℃敾闁搞劌鐏濋悾鐑藉即閵忊€虫濡炪倖甯婄粈浣规償婵犲洦鈷戦柛鎾村絻娴滄繄绱掔拠鑼㈡い顓炴喘瀵粙濡歌椤旀洟鎮楅悷鏉款棌闁哥姵娲滈懞閬嶅礂缁楄桨绨婚梺闈涱槶閸庤櫕鏅跺☉姘辩＜缂備焦顭囧ú瀛橆殽閻愬樊鍎旈柟顔界懅閹瑰嫭绗熼娑辨（婵犵绱曢崑鎴﹀磹瑜忓濠冪鐎ｎ亞顔愬銈嗗姧缁犳垿鎮″鈧弻鐔告綇閸撗呮殸闂佺粯鎼换婵嬪蓟濞戙垹鐒洪柛鎰剁細濞岊亞绱撴担闈涘闁告鍥ｂ偓鏃堝礃椤斿槈褔鏌涢幇鈺佸妞ゎ剙鐗撳娲箰鎼淬垹顦╂繛瀛樼矤娴滄繃绌辨繝鍕ㄥ亾濞戞瑯鐒介柣鐔风秺閺屽秷顧侀柛鎾寸懇椤㈡岸鏁愭径妯绘櫔闂侀€炲苯澧柣锝囧厴閹剝鎯斿Ο缁樻澑闂佽鍑界紞鍡樼濠靛姹叉い蹇撶墛閳锋帡鏌涚仦鍓ф噮妞わ讣绠撻弻鐔哄枈閸楃偘绨婚柣鎾卞€濋弻鏇熺箾閻愵剚鐝旈梺姹囧€ら崳锝夊蓟濞戞粠妲煎銈冨妼閹虫劗鍒掓繝姘兼晬婵炴垶姘ㄩ鏇㈡倵閻熸澘顥忛柛鐘虫礈閼鸿鲸绺介崨濠勫幗闂佽宕樺▔娑㈠几濞戙垺鐓涚€光偓鐎ｎ剙鍩岄柧浼欑秮閺屾稑鈹戦崱妤婁患缂備焦顨忛崣鍐潖濞差亝鍋傞幖绮规濡本绻涚€涙鐭ゅù婊庝簻椤曪絿鎷犲ù瀣潔闂侀潧绻掓慨鐢杆夊┑瀣拺闁稿繒鍘ф晶顖炴煕閵娿倕宓嗘い銏℃椤㈡﹢濮€閳锯偓閹锋椽鏌ｆ惔鈩冭础濠殿垼鍙冨畷闈涒枎閹寸姷锛滄繝銏ｆ硾閿曪妇绮斿ú顏呯厽婵炴垵宕▍宥団偓瑙勬礀閻栧ジ銆佸Δ浣瑰闂傗偓閹邦喚绉块梻鍌氬€风粈浣圭珶婵犲洤纾诲〒姘ｅ亾鐎规洜顢婇妵鎰板箳閺傝法銈﹂柣搴＄畭閸庨亶藝娴兼潙鐓曢柟鐑樺殮瑜版帗鏅查柛娑卞枛閺嗗牓姊洪崫銉ユ灁闁稿鍊濆璇测槈閳垛斁鍋撻敃鍌氱婵犻潧鐗呭鎾绘煟鎼淬値娼愭繛鍙夛耿瀹曟繂鈻庤箛锝呮婵炲濮撮鎰板极閸愵喗鐓熼柡鍐ㄦ处閼电懓顭跨憴鍕闁宠鍨块、娆戠驳鐎ｎ剙濮洪梻浣呵归鍡涘箰妤ｅ啫鐒垫い鎺嶈兌閸熸煡鏌熼崙銈嗗


    //typeCheck();
    std::string op_str;
    switch (op)
    {
    case ADD:
        op_str = "add";
        break;
    case SUB:
        op_str = "sub";
        break;
    case MUL:
        op_str = "mul";
        break;
    case DIV:
        op_str = "div";
        break;
    case MOD:
        op_str = "mod";
        break;
    case AND:
        op_str = "and";
        break;
    case OR:
        op_str = "or";
        break;
    case LESS:
        op_str = "less";
        break;
    case INCREMENT_BEFORE:
        op_str = "increment_before";
        break;
    case DECREMENT_AFTER:
        op_str = "decrement_after";
        break;
    case INCREMENT_AFTER:
        op_str = "increment_after";
        break;
    case DECREMENT_BEFORE:
        op_str = "decrement_after";
    case GREATER:
        op_str = "greater";
        break;
    case LESSEQUAL:
        op_str = "less equal";
        break;
    case GREATEREQUAL:
        op_str = "greater equal";
        break;
    case EQUAL:
        op_str = "equal";
        break;
    case NOTEQUAL:
        op_str = "not equal";
        break;
    }
    fprintf(yyout, "%*cBinaryExpr\top: %s\n", level, ' ', op_str.c_str());
    if (is_crement)
    {
        ID->output(level + 4);
    }
    else
    {
        expr1->output(level + 4);
        expr2->output(level + 4);
    }
}
void UnaryExpr::output(int level)
{
    /*float temp_store = cal_expr_val();
    if (temp_store != PRE_CAL_ERROR_MEETING_VAL)
    {
        if (getSymPtr()->getType()->isFLOAT())
            fprintf(yyout, "%*c\tExprValue:\t%f\n", level, ' ', temp_store);
        else if (getSymPtr()->getType()->isInt())
            fprintf(yyout, "%*c\tExprValue:\t%d\n", level, ' ', (int)temp_store);
        return;
    }*/


    std::string op_str;
    switch (op)
    {
    case ADD:
        op_str = "add";
        break;
    case SUB:
        op_str = "sub";
        break;
    case NOT:
        op_str = "NOT";
        break;
    }
    fprintf(yyout, "%*cUnaryExpr\top: %s\n", level, ' ', op_str.c_str());
    expr->output(level + 4);
}
void FunctCall::output(int level)
{
    fprintf(yyout, "%*ccall_funct %s\n", level, ' ', get_symbolEntry()->toStr().c_str());
    if (para_node != nullptr)
    {
        para_node->output(level + 4);
    }
}
void InitNode::output(int level, int dim, int* dim_record)
{
    if (is_checkpoint)
    {
        for (int i = dim + 1; i < 10; i++)
        {
            dim_record[i] = -1;
        }
        dim_record[dim]++;
        if (is_exp)
        {
            value_here->output(level);
            fprintf(yyout, "\t\t\t\tposition in the arr: ");
            for (int i = 0; i < 10 && dim_record[i] != -1; i++)
            {
                fprintf(yyout, " %d ", dim_record[i]);
            }
        }
        else
        {
            node1->output(level, dim + 1, dim_record);
            node2->output(level, dim + 1, dim_record);
        }
    }
    else
    {
        if (is_exp)
        {
            value_here->output(level);
            dim_record[dim]++;
            fprintf(yyout, "\t\t\t\tposition in the arr: ");
            for (int i = 0; i < 10 && dim_record[i] != -1; i++)
            {
                fprintf(yyout, " %d ", dim_record[i]);
            }
            fprintf(yyout, "\n");
        }
        else
        {
            node1->output(level, dim, dim_record);
            node2->output(level, dim, dim_record);
        }
    }
}

void ArrDimNode::output(int level)
{
    if (is_link)
    {
        arr1->output(level);
        arr2->output(level);
    }
    else
    {
        //typeCheck();
        if(node_state == ACCESS)
            fprintf(yyout, "%*c\t\taccessing_pos:\n", level, ' ');
        else
            fprintf(yyout, "%*c\t\tdimension_size:\n", level, ' ');
        if (is_not_val)
            fprintf(yyout, "%*c\t\t%d\n", level, ' ', (int)dimension_size->cal_expr_val());
            //std::cout << dimension_size->cal_expr_val() << std::endl;
        else
        {
            if (node_state == ACCESS)
                dimension_size->output(level + 20);
            else
                //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崰姘熼埀顒勬倵濞堝灝鏋熼柟顔煎€搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛鐏炵硶鍋撳畷鍥ㄦ畷闂侀€炲苯澧寸€规洑鍗冲浠嬵敇閻愮數鏆柣鐔哥矋閺屻劑鎮鹃柨瀣檮闁告稑锕ゆ禍婊堟⒑閸涘﹦绠撻悗姘煎墮閳绘捇濡烽埡鍌楁嫽闂佺ǹ鏈悷褔藝閿曞倹鐓欓悹鍥囧懐鐦堥悗瑙勬礀濠€閬嶅箟閹绢喖绀嬫い鎺嗗亾鐎殿喖娼″娲捶椤撯剝顎楅梺鍝ュУ閻楃娀骞冮垾鎰佹建闁逞屽墴瀵鎮㈤崨濠勭Ф婵°倧绲介崯顖烆敁瀹ュ鈷戠紒瀣儥閸庢劙鏌涢弮鈧悷鈺呯嵁閸愩剮鏃堝川椤撶喎绁舵俊鐐€栭幐楣冨磹椤愶箑顫呴幒铏濠婂牊鐓忓鑸电☉椤╊剛绱掗悩鎰佺劷缂佽鲸甯楀蹇涘Ω閿曗偓闂夊秹姊洪悷鏉挎Щ闁硅櫕锚閻ｇ兘顢曢敃鈧粈瀣棯閻楀煫顏呯閸撗€鍋撻崗澶婁壕闂佸憡娲﹂崜娑㈠储闁秵鈷戦梻鍫熻儐瑜版帒纾块柟鎯版閺勩儵鏌ㄥ┑鍡樼闁稿鎸搁埢鎾诲垂椤旂晫浜俊鐐€ら崢楣冨礂濡警鍤曟い鎰剁畱缁犳稒銇勯幘璺轰户缂佹劗鍋炵换婵嬫偨闂堟刀銏ゆ倵濮樺崬鍘寸€规洏鍎靛畷銊р偓娑櫱氶幏缁樼箾鏉堝墽鎮奸柟铏崌椤㈡艾饪伴崨顖滐紲闁荤姴娲﹁ぐ鍐焵椤掆偓濞硷繝鎮伴钘夌窞濠电偟鍋撻～宥夋⒑闂堟稓绠冲┑顔惧厴椤㈡瑩骞掗弮鍌滐紳闂佺ǹ鏈悷褔宕濆鍡愪簻妞ゆ挾鍋為崰妯尖偓瑙勬磸閸ㄤ粙鐛弽銊﹀闁稿繐顦扮€氬ジ姊绘担鍛婂暈缂佸鍨块弫鍐晲閸ヮ煈鍋ㄩ梻渚囧墮缁夌敻鎮￠弴銏＄厽婵☆垵顕ф晶顖涚箾閸喓鐭岄柍褜鍓濋～澶娒哄鈧畷婵嗏枎閹捐泛绁﹂梺鎼炲労閸撴岸宕戠€ｎ喗鐓曟い鎰Т閻忊晜顨ラ悙鏉戝闁哄矉绲鹃幆鏃堝灳閸愯尪绶熼梻浣呵归鍡涘箲閸ヮ剙绠栨俊銈呮噺閺呮煡骞栫划鍏夊亾閹颁焦楠勯梻鍌欐缁鳖喚绮婚幋锔藉亱闁规崘顕х粻鏍ㄤ繆閵堝懎鏆為柛鐘叉閺屾盯寮撮妸銉ヮ潾濡炪倧璐熼崝鎴﹀蓟閿濆顫呴柍杞拌兌娴狀參鏌ｉ姀鈺佺仯闁哥姵鐗犻獮鍐潨閳ь剟鐛幒鎳虫梹鎷呴崫鍕闂傚倷鑳剁划顖炲礉閺囥埄鏁嬫い鎾卞灩閻撴繈鏌熼崜褏甯涢柣鎾冲暟閹茬ǹ饪伴崼婵堫槶濠电偛妫欓幐鎼佸磼閵娾晜鐓涚€广儱鍟俊鍧楁煟濠靛棛鍩ｉ柡宀嬬節瀹曞爼濡烽妷褌鐥梻浣瑰▕閺€閬嶅垂閸ф钃熸繛鎴炃氶弨浠嬫煕閳╁喚娈㈠ù灏栧亾闂傚倷绀侀悿鍥敋瑜旇棟闁汇垻枪閽冪喐绻涢幋鐐茬劰闁稿鎹囬弫宥夊础閻愬弶娈ㄦ繝銏ｎ潐濞茬喎顫忔繝姘＜婵炲棙甯掗崢锟犳煛鐎ｅ吀绨奸柕鍥у楠炲鈹戦崶鑸碉紗婵°倗濮烽崑鐐哄礉濞嗘挾宓侀柡宥冨妿閻熺懓鈹戦悩鎻掝伀闁崇鍊栫换婵嬫偨闂堟稈鏋呭┑鐐板尃閸ヨ埖鏅為梺绯曞墲缁嬫垹澹曡ぐ鎺撯拺闁割煈鍣崕宥吤瑰⿰鍕煉闁哄瞼鍠栧畷顐﹀礋椤掑顥ｅ┑鐐茬摠缁秹宕曢幎瑙ｂ偓鏃堝礃椤斿槈褔鏌涘☉姗嗗殶鐎规洦浜娲偡閺夋寧顔€闂佺懓鍤栭幏锟�
                fprintf(stderr, "not a const \n");
        }
        //dimension_size->output(level + 20);
    }
}



void ParaNode::output(int level)
{
    if (is_link)
    {
        para1->output(level);
        para2->output(level);
    }
    else
    {
        fprintf(yyout, "%*c\t\tpara expr:\n", level, ' ');
        para_expr->output(level + 20);
    }
}
void DeclInitStmt::output(int level)
{
    fprintf(yyout, "%*cDeclInitStmt\n", level, ' ');
    id->output(level + 4);
    initVal->output(level + 4);
}
void ConstDeclInitStmt::output(int level)
{
    fprintf(yyout, "%*cConstDeclInitStmt\n", level, ' ');
    id->output(level + 4);
    initVal->output(level + 4);
}
void DeclList::output(int level)
{
    fprintf(yyout, "%*cDeclList\n", level, ' ');
    decl1->output(level + 4);
    decl2->output(level + 4);
}
void ConstDeclList::output(int level)
{
    fprintf(yyout, "%*cConstDeclList\n", level, ' ');
    decl1->output(level + 4);
    decl2->output(level + 4);
}
void WhileStmt::output(int level)
{
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崰姘熼埀顒勬倵濞堝灝鏋熼柟顔煎€搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛鐏炵硶鍋撳畷鍥ㄦ畷闂侀€炲苯澧寸€规洑鍗冲浠嬵敇閻愮數鏆柣鐔哥矋閺屻劑鎮鹃柨瀣檮闁告稑锕ゆ禍婊堟⒑閸涘﹦绠撻悗姘煎墮閳绘捇濡烽埡鍌楁嫽闂佺ǹ鏈悷褔藝閿曞倹鐓欓悹鍥囧懐鐦堥悗瑙勬礀濠€閬嶅箟閹绢喖绀嬫い鎺嗗亾鐎殿喖娼″娲捶椤撯剝顎楅梺鍝ュУ閻楃娀骞冮垾鎰佹建闁逞屽墴瀵鎮㈤崨濠勭Ф婵°倧绲介崯顖烆敁瀹ュ鈷戠紒瀣儥閸庢劙鏌涢弮鈧悷鈺呯嵁閸愩剮鏃堝川椤撶喎绁舵俊鐐€栭幐楣冨磹椤愶箑顫呴幒铏濠婂牊鐓忓鑸电☉椤╊剛绱掗悩鎰佺劷缂佽鲸甯楀蹇涘Ω閿曗偓闂夊秹姊洪悷鏉挎Щ闁硅櫕锚閻ｇ兘顢曢敃鈧粈瀣棯閻楀煫顏呯閸撗€鍋撻崗澶婁壕闂佸憡娲﹂崜娑㈠储闁秵鈷戦梻鍫熻儐瑜版帒纾块柟鎯版閺勩儵鏌ㄥ┑鍡樼闁稿鎸搁埢鎾诲垂椤旂晫浜俊鐐€ら崢楣冨礂濡警鍤曟い鎰剁畱缁犳稒銇勯幘璺轰户缂佹劗鍋炵换婵嬫偨闂堟刀銏ゆ倵濮樺崬鍘寸€规洏鍎靛畷銊р偓娑櫱氶幏缁樼箾鏉堝墽鎮奸柟铏崌椤㈡艾饪伴崨顖滐紲闁荤姴娲﹁ぐ鍐焵椤掆偓濞硷繝鎮伴钘夌窞濠电偟鍋撻～宥夋⒑闂堟稓绠冲┑顔惧厴椤㈡瑩骞掗弮鍌滐紳闂佺ǹ鏈悷褔宕濆鍡愪簻妞ゆ挾鍋為崰妯尖偓瑙勬磸閸ㄤ粙鐛弽銊﹀闁稿繐顦扮€氬ジ姊绘担鍛婂暈缂佸鍨块弫鍐晲閸ヮ煈鍋ㄩ梻渚囧墮缁夌敻鎮￠弴銏＄厽婵☆垵顕ф晶顖涚箾閸喓鐭岄柍褜鍓濋～澶娒哄鈧畷婵嗏枎閹捐泛绁﹂梺鎼炲労閸撴岸宕戠€ｎ喗鐓曟い鎰Т閻忊晜顨ラ悙鏉戝闁哄矉绲鹃幆鏃堝灳閸愯尪绶熼梻浣呵归鍡涘箲閸ヮ剙绠栨俊銈呮噺閺呮煡骞栫划鍏夊亾閹颁焦楠勯梻鍌欐缁鳖喚绮婚幋锔藉亱闁规崘顕х粻鏍ㄤ繆閵堝懎鏆為柛鐘叉閺屾盯寮撮妸銉ヮ潾濡炪倧璐熼崝鎴﹀蓟閿濆顫呴柍杞拌兌娴狀參鏌ｉ姀鈺佺仯闁哥姵鐗犻獮鍐潨閳ь剟鐛幒鎳虫梹鎷呴崫鍕闂傚倷鑳剁划顖炲礉閺囥埄鏁嬫い鎾卞灩閻撴繈鏌熼崜褏甯涢柣鎾冲暟閹茬ǹ饪伴崼婵堫槶濠电偛妫欓幐鎼佸磼閵娾晜鐓涚€广儱鍟俊鍧楁煟濠靛棛鍩ｉ柡宀嬬節瀹曞爼濡烽妷褌鐥梻浣瑰▕閺€閬嶅垂閸ф钃熸繛鎴炃氶弨浠嬫煕閳╁喚娈㈠ù灏栧亾闂傚倷绀侀悿鍥敋瑜旇棟闁汇垻枪閽冪喐绻涢幋鐐茬劰闁稿鎹囬弫宥夊础閻愬弶娈ㄦ繝銏ｎ潐濞茬喎顫忔繝姘＜婵炲棙甯掗崢锟犳煛鐎ｅ吀绨奸柕鍥у楠炲鈹戦崶鑸碉紗婵°倗濮烽崑鐐哄礉濞嗘挾宓侀柡宥冨妿閻熺懓鈹戦悩鎻掝伀闁崇鍊栫换婵嬫偨闂堟稈鏋呭┑鐐板尃閸ヨ埖鏅為梺鍦濠㈡﹢寮告笟鈧弻娑㈠焺閸愮偓鈻嶉悷婊呭鐢寮查弻銉︾厱闁靛绲芥俊浠嬫煟濠垫劒绨绘い顏勫暣婵″爼宕橀妸褌鎮ｉ梻浣侯攰濞呮洟鏁嬪銈庡亜缁绘帞妲愰幒鎳崇喖鎳栭埡鍌氭疄闂傚倷绶氶埀顒傚仜閼活垱鏅堕幘顔界厱闁宠鍎虫禍鐐繆閻愵亜鈧牜鏁幒妤€纾归柛娑橈功椤╅攱绻濇繝鍌滃闁绘挾濮电换娑㈡嚑妫版繂娈梺璇查獜缁绘繈寮婚敓鐘插窛妞ゆ挾濮撮悡鐔奉渻閵堝啫鐏柣鐔叉櫊楠炲啴骞嗚閺嗗棝鏌嶈閸撶喖骞嗘笟鈧畷濂稿Ψ閿旇瀚奸梻鍌欑贰閸嬪棝宕戝☉銏″殣妞ゆ牗绋掑▍鐘绘煙缂併垹鏋熼柣鎾寸洴閹﹢鎮欑捄鐩掓挻銇勯妷锔剧煁闁靛洤瀚伴獮姗€鎮欓挊澶夊垝闁诲氦顫夊ú蹇涘礉閹达负鈧礁鈽夐姀鐘栄囨煕閵夋垵妫岄崑鎾诲锤濡や讲鎷婚梺绋挎湰閼归箖鍩€椤掑嫷妫戞繛鍡愬灩椤繈顢楁径灞藉Ш闁荤喐绮嶅Λ鍐嵁閸愵収妯勯悗瑙勬礀瀹曨剟婀侀梺鍝勵槹鐎笛囧窗濡眹浜滈柕蹇ョ磿濞叉挳鏌熼鐣屾噰妤犵偞鎹囬獮鎺楀箣閻戝棗鎮嬫繝鐢靛Х閺佹悂宕戦悙鍝勭濠电姵鑹鹃弰銉╂煕閹伴潧鏋涚痪鎯ь煼閺岀喖宕滆鐢盯鏌ｉ幘瀵搞€掗柍褜鍓欓崢婊堝磻閹剧粯鐓冪憸婊堝礈閻旈鏆﹀ù鍏兼綑閸愨偓濡炪倖鎸炬慨瀵哥矈閿曞倹鈷戠痪顓炴噺瑜把呯磼閻樺啿鐏╃紒顔款嚙閳藉鈻庡鍕泿闂備礁婀遍崕銈夊垂閻㈢ǹ鐒垫い鎺嗗亾闁硅姤绮撳顐︻敋閳ь剙鐣风粙璇炬梹鎷呴崣澶婎伜婵犵數鍋犻幓顏嗗緤娴犲绠犳繛鍡樺竾娴滃綊鏌熼悜妯肩畺闁哄懐鍋ゅ铏瑰寲閺囩偛鈷夌紒鐐緲缁夊綊鐛繝鍕勃閻熸瑱绲鹃弬鈧梻浣虹帛閸ㄩ潧煤閵娾晛绀嗛柣妤€鐗忕粻楣冩倶閻愭彃鈧悂鍩€椤掍胶绠炵€殿噮鍋婂畷姗€顢氶崨顔芥珕闂備礁鎲℃笟妤呭礈濞戞氨涓嶆慨妯垮煐閳锋垿鏌涢幘鏉戠祷濞存粍绻冪换娑㈠矗婢跺苯鈪瑰銈庝簻閸熸挳骞愭繝鍐彾婵犻潧妫涚粔娲煛娴ｇ懓濮嶇€规洜鏁诲畷鎰版偆娴ｅ湱绉风紓鍌欑劍濮婂宕伴幘璇茬劦妞ゆ帒锕︾粔鐢告煕閹惧銆掑瑙勬礃缁傛帞鈧綆鍋嗛崢閬嶆煟韫囨洖浠滃褌绮欓獮濠囧川椤旇桨绨婚梺鎸庢煥閵堢ǹ顬婇鈧弻鏇㈠炊瑜嶉顓㈡煛娴ｇǹ鈧骞忛悩缁樺殤妞ゆ垶瀵ч、姗€姊婚崒娆愮グ妞ゆ洘鐗犲畷浼村箻鐠囪尙锛熼梺鑲┾拡閸撴稓妲愰敃鍌涚厽闁挎繂鎳愰悘閬嶆煟椤撶喓鎳囬柡灞诲€濋獮鍥ㄦ媴閸濄儰鐢婚梻浣侯攰瀵捇宕￠崘宸綎缂備焦蓱婵挳鏌ц箛鏇熷殌缂佹せ鈧枼鏀介柍钘夋娴滄粓鏌涢悢渚婵☆偁鍨藉铏规喆閸曨偆顦ㄥ銈嗘肠閸愬唲鍥ц摕闁靛绠戦埀顒傛暬閺屻劌鈹戦崱娑扁偓妤呮煛鐎ｎ剙鏋涢柡宀嬬秮楠炴鈧數纭跺Σ鍫濐渻閵堝骸寮ㄦ繛澶嬫礋楠炴垿宕熼鍌滄嚌濡炪倖鐗楅懝鐐珶閸喍绻嗛柣鎰典簻閳ь剚鐗曠叅婵☆垳鍘ч崹婵堚偓骞垮劚濞层劑鎯屾径灞稿亾楠炲灝鍔氶柟鍐差槸鍗遍柛顐犲劜閻撳繘鐓崶銉ュ姢缁炬儳娼￠弻娑樜熼崷顓犵厯濠殿喖锕ㄥ▍锝囨閹烘嚦鐔兼嚒閵堝懎姹查梻鍌欒兌椤㈠﹥鎱ㄩ妶鍥у灊闁规崘顕х粻姘扁偓鍏夊亾闁逞屽墴閸┾偓妞ゆ帒锕︾粔闈浢瑰⿰鍕⒌鐎殿喓鍔嶇粋鎺斺偓锝庡亞閸樻捇姊虹€圭姵銆冪紒鈧笟鈧鎶芥倷閻戞鍘辨繝鐢靛Т閸燁偅鎱ㄦ径鎰厓鐟滄粓宕滃┑瀣剁稏濠㈣泛鈯曞ú顏勫唨妞ゆ挾鍠庢禍顖炴⒑缁嬭法绠绘俊顐ユ硶閹广垽宕卞Ο璇插伎濠电娀娼уΛ娑㈡倿閻愵兛绻嗛柟缁樺俯閻撳ジ鏌熼绛嬫當闁崇粯鎹囧畷褰掝敊閻ｅ奔鎲炬繝鐢靛Х閺佹悂宕戝☉銏℃櫇闁靛牆顧€缂嶆牠鐓崶銊︾缁炬儳鍚嬫穱濠囶敍濠垫劕娈銈呭閹瑰洤顫忓ú顏勭畾鐟滃繒绮婚幎鑺ョ厵闁惧浚鍋呯亸鐢电磼椤曞懎寮柛銊╃畺瀵剟濡烽敂鑺ユ緫濠碉紕鍋戦崐鏍ь潖婵犳艾违閻庯綆鍠栫粈澶愭煟閺傚灝鎮戦柣鎾寸懇閺屟嗙疀閿濆懍绨奸悗瑙勬礀閻倿寮诲澶娢ㄩ柨鏃傜摂濡啴鎮楃憴鍕闁挎洏鍨介妴浣糕枎閹炬潙浜楅柟鑹版彧缁叉椽宕戦幘娲绘晣闁绘垵妫欑€靛矂姊洪棃娑氬婵☆偅鐟х划鍫ュ焵椤掑嫭鈷戠痪顓炴噺閻濐亪鏌涢弮鈧悷銉╂偩瀹勬壋鏀介悗锝庡亜閳ь剙顭烽弻銈夊箒閹烘垵濮夐梺褰掓敱濡炶棄顫忓ú顏勬嵍妞ゆ挴鍋撻柣鐔稿櫞瑜版帗鍊婚柦妯侯槺閸欌偓濠电姰鍨奸崺鏍礉閺嶎厼纾归柛顭戝亝閸欏繑淇婇婊冨付閻㈩垰鐖奸弻锛勨偓锝庡亞濞叉挳鏌熼挊澶屽煟闁诡喗鐟ヨ彁妞ゆ垼娉曢惄搴ㄦ⒒娴ｅ憡鎯堢紒瀣╃窔瀹曟垿宕熼鍌ゆ祫闂佹寧娲栭崐褰掓偂閻斿吋鐓欓柟顖嗗苯娈堕梺瀹犳椤︿即濡甸崟顖ｆ晣闁斥晛鍟伴妴濠囨⒑閸︻厽鍤€閻庢凹鍘剧划顓㈡偄绾拌鲸鏅┑顔斤供閸撴瑩藟濠靛鈷掗柛灞剧懆閸忓矂寮搁鍡欑＜闁靛⿵闄勯妵婵堚偓瑙勬礃濞茬喎鐣烽敐鍡楃窞閻忕偛鎲￠弫闈涒攽閻樺灚鏆╁┑顔芥綑鐓ら柕鍫濇川娑撳秹鏌ｉ姀鐘冲暈闁绘挻鐟╅弻锝夊箣閻愬棙鍨块崺娑㈠箳閹宠櫕妫冨顕€宕奸悢鍙夊濠电偠鎻徊鍧楁偤閺冨牆鍚归柛鎰靛枟閻撴洟鏌ｅΟ璇插婵炲牊绮撻弻鏇㈠幢濡も偓閺嗭絿鈧娲忛崝宥咁焽韫囨稑绀堢憸蹇涘煟閵堝棛绡€闁汇垽娼ч埢鍫熺箾娴ｅ啿娴傞弫鍕煙閻楀牊绶查柡瀣╃窔閺屾盯鍩勯崘顏佸闂佺粯鎸诲ú鐔煎箖濮椻偓閹瑩骞撻幒鍡樺瘱闂備浇妗ㄩ悞锕傛偋閻樿钃熼柡鍥╁枔缁犻箖鏌ｉ幇闈涘闁绘繃娲熷娲箮閼恒儲鏆犻梺鎼炲妼濞尖€愁嚕鐠囨祴妲堟俊顖炴敱閺傗偓闂備胶纭堕崜婵嬨€冭箛鏂款嚤闁稿本鍑瑰〒濠氭煏閸繈顎楀ù婊冨⒔缁辨帡鎮╅搹顐㈢３閻庢鍠涢褔鍩ユ径鎰潊闁绘鏁稿澶愭⒒娓氣偓閳ь剛鍋涢懟顖涙櫠鐎电硶鍋撶憴鍕缂傚秴锕ら悾椋庣矙鐠囩偓妫冨畷姗€濡歌閸嬪﹪姊婚崒娆戭槮闁硅绻濆畷婵嬫晝閸屾氨鐛ラ梺鍝勭▉閸欏繒绱為弽顓熺厱婵炴垶顭囬幗鐘绘煟閹惧磭绠婚柡灞剧洴椤㈡洟鏁愰崶鈺冩毇闂備線娼荤徊钘夛耿鏉堚晜顫曢柟鐑橆殔缁犳稒銇勯幘璺盒涘┑顔兼川缁辨挻鎷呴搹鐟扮缂備浇顕ч悧鎾荤嵁閸愨晝顩烽悗锝庡亽濡啫鈹戦悙娈挎缂傚牅鍗冲畷瑙勭鐎ｎ亞鐣哄┑掳鍊愰崑鎾淬亜椤愶絿绠炴慨濠呭吹閳ь剟娼ч幉锟犲疾閸忚偐绡€缁剧増菤閸嬫捇宕橀懠顒勭崜闂備礁鎲″褰掓偡閵夆晩鏁嬮柨婵嗩槸缁犵粯銇勯弮鍌楁嫛婵炵厧锕娲箮閼恒儲娈伴梺绋款儐閹瑰洭寮婚悢鍓叉Ч閹艰揪绲界粭锛勭磽娴ｈ櫣甯涚紒瀣笒椤洩绠涘☉妯碱槶閻熸粌绉瑰畷鍝勭暆閸曨兘鎷虹紒缁㈠幖閹冲氦顣跨紓鍌欑贰閸犳牕霉閻戣棄绀嗛柟鐑橆殔缁秹鏌涢銈呮瀻闁逞屽墰缁垱绌辨繝鍥舵晬闁绘劕鐡ㄩ悵宕囩磽娴ｉ潧濡兼い顓炲槻椤繐煤椤忓拋妫冨┑鐐村灱妞存悂顢撻崱娑欌拻闁稿本鑹鹃鈺呮倵濮樼厧澧撮柟顔藉劤閻ｏ繝骞嶉鑺ョ暦闂備線鈧偛鑻晶鎾煕閳规儳浜炬俊鐐€栫敮鎺楀窗濮橆剦鐒介柟閭﹀幘缁犻箖鏌涘▎蹇ｆ闁兼澘娼￠弻鏇㈠幢濡も偓閺嗭絿鈧娲忛崝宥咁焽韫囨稑绀堢憸蹇涘煟閵堝棛绡€闁汇垽娼у瓭闂佹寧娲忛崐婵嬪箖瑜斿畷鍗炩槈濞嗗繆鍋撻崸妤佺厱妞ゎ厽鍨垫禍鏍瑰⿰鍕煉闁哄瞼鍠栧畷顐﹀礋椤掑顥ｅ┑鐐茬摠缁秹宕曢幎瑙ｂ偓鏃堝礃椤斿槈褔鏌涢埄鍐剧劷闁宠绋撶槐鎾诲磼濮樻瘷锛勭磼閼搁潧鍝哄┑鈥崇摠缁楃喖鍩€椤掆偓椤曪綁骞橀纰辨綂闂佹枼鏅涢崯顖炴偟閺嶎厽鈷掑〒姘ｅ亾闁逞屽墰閸嬫盯鎳熼娑欐珷闁规鍠氱壕鍏笺亜閺傚灝鈷旈悽顖涚〒缁辨帞绱掑Ο鑲╃暤濡炪値鍋呯换鍫ャ€佸鈧幃娆撳煛娴ｅ嘲顥氶梻浣圭湽閸ㄥ綊骞夐敓鐘茬厱闁圭儤顨嗛悡鏇熴亜閹扳晛鈧洟寮搁弮鍫熺厓妞ゆ牗绋掔粈瀣煛瀹€鈧崰鏍х暦閿濆棗绶為悗锝庝簴閸嬫捇骞囬悧鍫㈠幐閻庡厜鍋撻柍褜鍓熷畷浼村冀椤撶偟顔愰悷婊呭鐢晠寮崘顔界厪闁割偅绻冨婵嬫煛婢跺骸袚缂佺粯绻傞埢鎾诲垂椤旂晫浜梻浣瑰濞插繘宕曢柆宥佲偓鏃堝礃椤旇棄鐧勬繝銏犲帨閺呮粓鎯勯姘辨殾婵せ鍋撳┑鈩冩倐婵偓闁宠棄妫欑紞宀勬⒒娴ｅ憡鎯堟い锔诲灦閹囧礋椤栨氨顦梻渚囧墮缁夋潙娲垮┑鐘灱濞夋盯顢栭崒鐐茬闁惧繐婀辩壕钘壝归敐鍛棌闁稿骸绻戠换娑氭兜妞嬪海鐦堝銈冨灪閹告瓕鐏冮梺鍛婂姧缁茶姤绂嶆ィ鍐╁仭婵炲棗绻愰顏勵熆鐠哄搫顏柡灞剧洴楠炴鎹勯悜妯间邯闁诲氦顫夊ú妯侯渻閼恒儲鍙忛柍褜鍓熼弻鏇㈠醇濠靛洤娅у銈嗘⒐鐢繝寮婚敐澶嬪亹闁告瑥顦遍埞娑㈡煟韫囨挾绠查柣鐔叉櫅閻ｇ兘濮€閵堝懐顔愭繛杈剧到閸樻粓骞忓ú顏呪拺闁告稑锕﹂埥澶愭煥閺囶亞鎮兼繛鍡愬灲閺屽棗顓奸崱娆忓箞闂備胶绮敋闁哥喎娼″畷顐︽惞閸忓浜鹃悷娆忓绾炬悂鏌涢妸銊︻棄闁伙絿鏌夐妵鎰板箳濠靛洦娅囬梻浣瑰缁嬫垹鈧凹鍣ｉ幃鐐偅閸愨斁鎷绘繛杈剧秬濞咃絿鏁☉銏＄厱闁哄啠鍋撴繛鍙夌墳閻忔帗绻濋悽闈浶㈡繛灞傚€楁竟鏇㈠锤濡ゅ啫褰勯梺鎼炲劘閸斿秶鈧艾顭烽弻锟犲礃閵娧冾杸闂佺ǹ顑嗛崝娆撳蓟瑜戠粻娑㈡晲閸涱剛鐩庨梻渚€鈧偛鑻晶顖炴煕閺冣偓椤ㄥ﹪宕洪埀顒併亜閹烘垵鏋ゆ繛鍏煎姈缁绘盯骞撻幒鏃傤啋閻庤娲濋～澶岀矉閹烘柡鍋撻敐搴″缂佷緤绠戦埞鎴︻敊婵劒绮堕梺绋款儐閹稿濡甸崟顖氼潊闁挎稑瀚崳浼存⒑鐠団€虫灈濠⒀嗗Г缁傛帡鏁冮崒娑樼檮婵犮垼娉涢惌鍫ュ船閼哥數绡€闁汇垽娼у暩闂佽桨鐒﹂幃鍌氱暦閹达附鍋愰悹鍥皺椤斿苯顪冮妶鍛婵☆偅绋撶划鍫熺節閸ャ劎鍘撻悷婊勭矒瀹曟粌顫濈捄浣曪箓鏌涢弴銊ョ伇闁轰礁鍟撮弻鏇＄疀婵犲啯鐝曠紒妤佸灴濮婄粯鎷呮搴濊缂備浇寮撶划娆忕暦閺屻儱钃熼柕澶涚畱娴犵厧鈹戦悩缁樻锭妞ゆ垵鎳橀幃鐐哄垂椤愮姳绨婚梺鍦劋閸ㄧ敻顢旈埡鍛厽闁圭儤鍨规禒娑㈡煏閸パ冾伃妤犵偞甯掗濂稿炊椤忓棛宕堕梻鍌欑閹芥粓宕版惔锝嗗床婵せ鍋撶€殿喛顕ч埥澶愬閳╁啯鐝抽梺纭呭亹鐞涖儵宕滃┑瀣仧婵犻潧顑嗛崐鐢告偡濞嗗繐顏紒鈧崘顏嗙＜妞ゆ棁濮ゅ畷灞绢殽閻愭彃鏆ｅ┑顔瑰亾闂侀潧鐗嗗Λ宀勫箯瑜版帗鈷戠憸鐗堝笒娴滀即鏌涢悩鍐插摵闁诡喚鍋撻妶锝夊礃閳圭偓瀚藉┑鐐舵彧缁叉寧鐏欏銈冨劚缁绘﹢寮诲☉姘ｅ亾閿濆簼绨奸柛锝呯秺閹繝濡舵径瀣幐閻庡箍鍎辨鎼佺嵁濡皷鍋撶憴鍕矮缂佽埖宀稿濠氬即閿涘嫮鏉告繝鐢靛仦閸庤櫕绂嶆ィ鍐╃厾缁炬澘宕晶浼存煟閿濆鐣烘慨濠勭帛缁楃喖鍩€椤掆偓椤洩顦归柟顔ㄥ洤骞㈡繛鎴烆焽椤斿棝姊洪悷鎵憼缂佽绉撮…鍥冀閵娿倗绠氶梺闈涚墕閹冲酣寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍嵁閹捐绠虫繝闈涙婢瑰姊绘担椋庝覆缂佽弓绮欓幃妯侯潩鐠鸿櫣鐤勯梺闈涒康婵″洨寮ч埀顒勬⒑缁嬭儻顫﹂柛濠冾殜瀹曘儳鈧綆鍠楅崐鐢告偡濞嗗繐顏紒鈧崘顔藉仺妞ゆ牗绋戝ù顕€鏌涢埞鍨仾闁诡垱妫冩俊鎼佸Ψ瑜忛弶鍛婁繆閻愵亜鈧牠寮婚妸鈺佺妞ゆ劧绲块々鐑芥煙閻戞ê鐏嶉柡鈧禒瀣厽婵☆垵娅ｆ禒娑㈡煛閸″繑娅婇柡灞剧〒閳ь剨缍嗛崑鍡椕洪幘顔藉癄婵犻潧顑嗛悡娑橆熆鐠轰警鍎滅紒鎵佹櫆缁绘稑顔忛鑽ゅ嚬闂佺ǹ顑呯€氼剟鈥旈崘顏佸亾閿濆簼绨绘い鎺嬪灪缁绘盯宕奸姀鐘卞缂備胶绮惄顖氼嚕閸洖绠ｆ繝闈涙搐閳ь剦鍨伴埞鎴︽倷閹绘帗鍊梺鑽ゅ暀閸涱厼鐏婇柣鐘叉处缁佹潙危閸儲鐓忛煫鍥ュ劤绾惧潡鏌涘Ο鐑樺暈缂佺粯绻堟慨鈧柨婵嗘閵嗘劙姊洪幐搴㈢┛闂傚嫬瀚划瀣箳濡も偓椤懘鏌曢崼婵囶棤闁告﹢浜跺娲濞戞艾顣哄┑鈽嗗亝椤ㄥ﹪骞冨鈧弫鍌炲礈瑜忛敍婊呯磽閸屾瑧鍔嶆い顓炴川缁鎮欓悜妯煎幍濡炪倖妫佸畷鐢告儗濞嗘劒绻嗘い鎰╁灩閺嗘瑩妫佹径鎰仯濞达絽鎲＄拹锛勭磼婢舵ê娅嶉柡宀嬬磿娴狅妇鎷犲ù瀣壕婵犻潧顑呯粻鏍煟閹达絾顥夌紒鐙欏洤绠归悗娑櫳戠亸浼存倵閸偆澧辩紒杈ㄦ崌瀹曟帒鈻庨幋锝囩崶闂備礁鎽滄慨鐢告偋閻樿鐏抽柨鏇炲€归崐璇测攽椤旇棄濮€闁稿鎸婚幏鍛寲閺囩噥娼旈梻渚€娼х换鍡涘焵椤掍焦鐏遍柛瀣崌閹粓鎳為妷銉㈠亾閻㈠憡鐓熼柕蹇嬪灪椤忋垻鎲搁悧鍫濈鐎规挷鐒︽穱濠囧Χ閸涱喖娅ら梺鎶芥敱鐢帡婀侀梺鎸庣箓鐎氼垶顢楅悢鍏肩厸闁逞屽墯鐎佃偐鈧稒顭囬崢闈涒攽閻愯泛钄兼い鏂匡功閼洪亶鎮剧仦绋夸壕闁割煈鍋呯欢鏌ユ煥閺囥劋绨婚柣锝囧厴楠炲洭鎮ч崼婵呯敖濠电偠鎻紞鈧い顐㈩樀閹線鏁愭径瀣ф嫽闂佺ǹ鏈懝楣冨焵椤掍焦鍊愮€规洘鍔栭ˇ鐗堟償閿濆洨鍔跺┑鐐存尰閸╁啴宕戦幘鎼闁绘劕顕晶鍨亜閵忊剝绀嬮柡浣稿暣閸┾偓妞ゆ帒鍊绘稉宥夋煟濡灝鍚归柛娆愭崌閺屾盯濡烽敐鍛瀷闂佸疇妫勯ˇ杈╂閹烘鍋愮€规洖娲ら埛宀勬⒑閸濆嫭婀扮紒瀣灴閹儳鈹戠€ｎ亞鍔﹀銈嗗笒鐎氼剛澹曠紒妯肩闁瑰瓨鐟ラ悘顏堟倵濮橆剚鍤囬柡宀嬬秮瀵剟宕归钘夆偓顖炴⒑缂佹ɑ灏柛搴ｆ暬瀵鏁愭径濠冾棟闂佸湱枪鐎涒晠宕曢幘缁樺€垫繛鎴炵懅缁犳绱掓潏銊ユ诞闁诡喗鐟╅、妤呭焵椤掑嫬绀夐柕鍫濐槹閻撴洘鎱ㄥ鍡楀⒒闁稿孩鍔楃槐鎺戠暆閸愵喖鎽电紓浣虹帛缁诲牆鐣烽崼鏇熷殝闁割煈鍋呴悵顐⑩攽閻樺灚鏆╁┑鐐╁亾濠电偘鍖犻崨顏勪壕婵ǚ鍋撻柛銉戝拑绱遍梻浣筋潐瀹曟﹢顢氳閹偤宕归鐘辩盎闂佸湱鍎ら崹鐢割敂椤忓牊鐓熼柟鎯х摠缁€瀣煙椤旂瓔娈滈柡浣瑰姈閹棃鍨鹃懠顒€鍤梺璇叉唉椤煤閺嶎厽鍎斿┑鍌溓归悞鍨亜閹哄秶璐伴柛鐔风箻閺屾盯鎮╅搹顐ゎ槶闂佸憡甯楃敮鎺楀煝鎼淬劌绠婚柟鍏哥娴滄儳銆掑锝呬壕閻庢鍠楅幐铏叏閳ь剟鏌嶉妷銉ュ笭缂併劌顭峰缁樻媴閸濆嫪澹曢梺鐓庣秺缁犳牕鐣烽幇鏉跨濞达絺鍋撳璺衡看濞尖晜銇勯幘璺盒ラ柣锕€鐗嗛埞鎴︽倷閼碱剙顣洪梺璇茬箲缁诲牆顕ｉ幖浣瑰亜闁稿繗鍋愰崣鍡涙⒑閸濆嫭澶勬い銊ユ閳诲秵绻濋崟銊ヤ壕閻熸瑥瀚粈鍐磼椤旇偐鐒搁柛鈹垮劜瀵板嫭绻濇惔銏犲厞闂佸搫顦悧鍐疾濠靛鍌ㄩ柣妯虹－缁♀偓闂佹眹鍨藉褎绂掑⿰鍕箚妞ゆ牗鐔弨鑽ょ磼閸屾氨孝闁宠鍨归埀顒婄秵閸嬪嫭绂嶅Δ鍛拺缂備焦蓱閻撱儵鏌熼懞銉х煂缂侇喚绮妶锝夊礃閳哄啫骞嶉梺璇插缁嬫帡鈥﹂崶鈺冧笉闁瑰墽绮悡鐘虫叏濠靛棛鐒炬繛鍏煎姉缁辨帞绱掑Ο鍏煎垱閻庤娲忛崝鎴︺€佸鈧幃鈺佄熺粙搴撳亾濠靛钃熺€广儱鐗滃銊╂⒑閸涘﹥灏扮€光偓閹间降鈧礁鈻庨幘鍐插敤濡炪倖鎸鹃崑鐔兼偘閵夈儮鏀介幒鎶藉磹閺囥垹绠犻幖杈剧悼娑撳秴霉閻撳海鎽犻柣鎿勭秮閻擃偊宕舵搴″煂婵炲瓨绮嶇换鍫ュ蓟濞戞ǚ鏋庨煫鍥ㄦ礈椤旀帡姊洪崫鍕缂佸鍏樼瘬濞撴埃鍋撻柡灞剧洴楠炴鎹勯悜妯间邯闁诲孩顔栭崰娑㈩敋瑜旈、姗€宕楅悡搴ｇ獮婵犵數濮寸€氼剟鐛幇顑芥斀闁绘劘鍩栬ぐ褏绱掗煫顓犵煓妤犵偛顦甸崹楣冨棘閵夛妇浜栭梻浣告贡缁垳鏁埡浣碘偓鎺撶節濮橆厾鍘梺鍓插亝缁诲啴宕抽悾宀€纾奸柣妯哄暱閳绘洟鏌＄仦鍓ф创闁诡喒鏅涢悾鐑藉炊瑜夐崥鍌涗繆閻愵亜鈧倝宕戦崟顓熷床闁圭儤姊归～鏇㈡煙閻戞ê娈鹃柣鏃傚劋鐎氭氨鈧懓澹婇崰鎺楀磻閹捐绠荤紓浣骨氶幏娲⒑閸涘﹦鈽夐柨鏇畵閸┿儲寰勯幇顓犲弳闂佸搫娲﹂敋闁告棑濡囬埀顒冾潐濞叉ê煤閻旇偐宓佹俊顖濆亹绾惧吋淇婇鐐存暠閻庢俺鍋愮槐鎾诲磼濞嗘埈妲銈嗗灥閹虫劖绂嶉幖浣稿窛妞ゆ挆鍐╂珚闂備線娼чˇ顐﹀疾濠婂牊鍋傞柣鏂垮悑閻撴稓鈧箍鍎辨鍛婄濠婂牊鍋ㄦい鏍ㄦ皑閸╋綁鏌″畝瀣М闁轰焦鍔欏畷鎯邦槻妤犵偛顑呰灃闁绘﹢娼ф禒婊勭箾瀹割喖骞栨い鏇秮瀹曞ジ寮撮悙鑼偓顓烆渻閵堝棙鐓ラ柛姘儔瀵疇绠涢幘顖涙杸闂佹寧绋戠€氼剚绂嶆總鍛婄厱濠电姴鍟版晶鐢碘偓瑙勬礃缁诲倽鐏冮梺鍛婁緱閸橀箖鎮伴妷鈺傜厵闁稿繗鍋愰弳姗€鏌涢妸銉吋闁靛棗鎳樺濠氬Ψ閿旀儳骞楅梻渚€鈧稑宓嗘繛浣冲嫭娅犳い鏍仦閻撴洘绻涢崱妤冪缂佺姵鐓￠弻鐔哥瑹閸喖顬堥柧缁樼墵閺屽秷顧侀柛鎾寸懆閻忓鈹戞幊閸婃捇鎮為敃鈧埢宥夋偐缂佹鍘靛┑鐐茬墕閻忔繈寮搁妶鍥╂／闁告挆鍐ㄤ粯濡炪値鍙€濞夋洟骞戦崟顒傜懝妞ゆ牗鑹炬竟澶愭⒒娓氣偓濞佳兠洪敂鐐床闁割偁鍎遍拑鐔兼煃閵夈儳锛嶉柡鍡楁閹鏁愭惔鈥愁潾闂佹寧绋撻崰鎰崲濞戞埃鍋撻悽鐧诲綊顢撻幘鏂ユ斀闁炽儴娅曢崰妯活殽閻愯尙绠婚柟顔规櫇閹风娀鎳犻澶婃杸闂傚倷绀佸﹢閬嶅磿閵堝洦宕查柛鎰典簽缁€濠囨煕閳╁啰鈯曢柍閿嬪浮閺屾稓浠﹂崜褎鍣紓浣疯兌婢ф濡甸崟顖氬嵆闁绘柨鎼埛宀勬倵鐟欏嫭纾婚柛妤佸▕閻涱喖螣閸忕厧鐝伴梺鍦帛鐢偤鍩€椤掑寮慨濠冩そ閹崇偤濡烽崘鍙ラ偗閽樻繈姊洪鈧粔鏉懶ч弻銉︾叆闁哄洨鍋涢埀顒€缍婇幃陇绠涢幘顖涙杸闂佺粯枪鐏忔瑧绮婚悧鍫涗簻闁冲搫锕ゆ晶鎵磼鏉堛劌娴柛鈺嬬畵閸┾偓妞ゆ帒瀚壕濠氭煙閸撗呭笡闁抽攱甯掗湁闁挎繂鎳忛崯鐐烘煕閻斿搫浠︾紒缁樼〒閹风姾顦规俊缁㈠枤缁辨帡宕掑姣櫻囨煙椤旂晫鎳囨俊顐㈠暙閳藉娼忛…鎴烇紖闂傚倸鍊烽懗鍫曪綖鐎ｎ喖绀嬮柛顭戝亞閺嗭箓姊绘担鐟扳枙闁衡偓鏉堚晜鏆滈柨鐔哄Т閽冪喐绻涢幋鐐电叝婵炲矈浜弻娑㈠箻濡も偓鐎氼剙鈻嶅Ο璁崇箚闁绘劦浜滈埀顑懏濯奸柨婵嗘缁诲棝鏌涢妷銏℃珕妞ゎ偅娲熼弻鐔兼倷椤掆偓婢ь垶鏌涘顒夊剶闁哄本绋戦オ浼村礃閵婏附顔戦梻浣告啞閸戝綊宕戞繝鍥ц摕闁挎洍鍋撻摶锝夋煟閹惧啿顒㈤柣蹇撶墦濮婃椽宕崟顓犱紘闂佸摜濮甸悧鐘绘偘椤曗偓楠炲鏁冮埀顒傜不濞戙垺鈷掗柛顐ゅ枔閳洘銇勯弬鍨伀缂佽鲸鎹囧畷鎺戔枎閹烘繂鏁奸梻浣呵归鍡涘箲閸ヮ剙钃熸繛鎴炵煯濞岊亪鏌涢幘妞诲亾濠殿喖娲娲川婵犲嫭鍣х紓浣虹帛閸ㄥ墎绮氭潏銊х瘈闁搞儯鍔岄埀顒€鍢茶灋闁绘鐗忕粻鎾寸箾閸偄骞楃紒缁樼〒閳ь剚绋掗…鍥儗閹烘梻纾奸柣妯虹－婢у灚顨ラ悙鎻掓殭閾绘牠鏌嶈閸撶喖宕洪姀銈呯閻犲洦褰冮悗顓烆渻閵堝棙鈷掗柡鍜佸亯閵囨劙顢涢悙绮规嫽婵炶揪绲介幊娆掋亹閹烘繃鏅為梺鍛婄☉閻°劑寮插┑鍥ヤ簻闊洦鎸炬晶娑㈡煕鐎ｎ倖鎴﹀Φ閸曨垰妫橀柟绋垮瘨濞兼棃姊虹紒妯烩拻闁告鍛焼闁稿瞼鍋為悡鐔兼煙閹规劖鐝柟鐧哥秮閺屻劌鈽夊Ο鑲╊啋濠殿喖锕︾划顖炲箯閸涘瓨鍋￠柡澶婄仢琚樼紓鍌氬€烽懗鍓佸垝椤栫偛绀夐柡鍥ュ灪閺呮繈鏌曡箛瀣偓鏍倿閼测斁鍋撻獮鍨姎婵☆偅顨嗙粋鎺楀础閻愨晜鏂€濡炪倖鏌ㄩ～鏇熺濠婂牊鐓曢悗锝庡亝鐏忎即鏌熸笟鍨缂佺粯绻堝畷鐔碱敇閻愭鍋ч梻鍌欒兌閹虫捇宕查弻銉ョ疇婵せ鍋撴鐐寸墳閵囨劙骞掗幘鍏呯敾闂備浇顫夐鏍闯椤曗偓瀹曟垼顦规慨濠傤煼瀹曟帒鈻庨幒鎴濆腐缂傚倷绶￠崳顕€宕归崼鏇炍ラ柛鎰靛枛楠炪垺淇婇妶鍜冩敾缂傚秴閰ｉ弻锝夋偐閸欏顦╃紓浣风劍閹稿啿顕ｉ崨濠冨劅闁靛⿵濡囬崢顏堟椤愩垺澶勬繛鍙夌墵椤㈡挾浠﹂崜褏顔曢柣蹇撶箲閻楁鈻嶆繝鍥ㄧ厵妞ゆ梻鍋撻崵鈧柣鎾卞€栭妵鍕疀閹炬惌妫ょ紓浣瑰姈椤ㄥ﹤顫忛搹瑙勫厹闁告侗鍘滈幘瀵哥闁肩⒈鍓欓弸搴€亜椤愩垻绠伴悡銈嗐亜韫囨挻濯兼俊顐㈠暙閳规垿鎮欑€靛憡娈梺绋块叄娴滃爼骞冭楠炴﹢骞囨担鍛婎吙闂備礁婀辩划顖氼啅婵犳艾閱囨い蹇撶墛閻撴洘銇勯幇鍓佹偧缂佺姵锕㈤弻銊モ槈濞嗘垶鍒涘┑顔硷攻濡炶棄鐣烽妸锔剧瘈闁告劦鐓堝Σ閬嶆⒒娴ｈ鍋犻柛鏃€鍨剁粋宥夊醇閺囩偟鐣洪梺璺ㄥ枔婵挳宕￠幎鑺ョ厽闁哄啫娲﹂鐘炽亜閺冣偓濞茬喎顫忓ú顏呭仭闂侇叏绠戝▓鍫曟⒑缁嬪灝顒㈤柛銊ョ仢閻ｅ嘲螖閸滀焦鏅㈤梺鍛婃处閸撴盯宕㈤柆宥嗏拺闁告繂瀚弳娆撴煕婵犲啯鍊愮€殿喖鎲￠幆鏃堝Ω閿旂粯鈷栧┑鐘灱閸╂牠宕濋弽顓熷亗闁绘柨鍚嬮悡娆撴煟閹伴潧澧柍缁樻礃閵囧嫭鎯旈姀鈺傜暦闂侀潧娲ょ€氼垳绮诲☉銏℃櫜闁告侗鍘鹃鎴︽⒒娓氣偓閳ь剚绋撻埞鎺楁煕閺傝法肖闁瑰箍鍨归埞鎴犫偓锝庝簽椤︺劑姊哄畷鍥ㄥ殌闁靛洦锕㈤幖瑙勬償閵婏妇鍘介柟鍏肩暘閸娿倕顭囬幇顓犵闁告瑥顦遍惌鎺斺偓瑙勬礃椤ㄥ懘锝炲⿰鍫濈劦妞ゆ帒瀚繚婵炶揪绲芥竟濠傤焽閵娾晜鐓冪憸婊堝礈閻旇偐宓侀柛鎰靛枛椤懘鏌曢崼婵囧櫢缂佸崬鐖煎娲川婵犲啫顦╅梺绋库康缂嶄線銆侀弴銏犵厬闁冲搫鍊归弶鎼佹⒒娴ｈ櫣甯涢柨姘舵煟韫囨柨鍝虹€殿喗濞婇崺锟犲川椤旀儳骞堥梺璇插嚱缂嶅棝宕滃▎鎰浄閺夊牜鐓堝▓浠嬫煟閹邦厽缍戝┑顔碱槺缁辨帡顢欓悾灞惧櫚閻庤娲滄繛鈧俊楣冧憾閺屽秷顧侀柛鎾寸懃椤洭鍨鹃幇浣告闂佸壊鍋呭ú鏍煁閸ャ劍鍙忔慨妤€妫楅獮妤呮煟濠靛洦顥堟慨濠冩そ楠炴牠鎮欓幓鎺濈€寸紓鍌欐祰椤曆囨偋閹炬剚鍤曢悹鍥ф▕閸氬顭跨捄鐚存缂佸崬鐖煎娲川婵犲啫顦╅梺鍛婃尰閻熴儵鍩㈠澶嬫櫜闁搞儮鏅濋敍婵囩箾鏉堝墽鍒伴柟纰卞亜閺嗏晝绱撻崒娆戣窗闁革綆鍣ｅ畷褰掓嚋閸忓摜绠氶梺姹囧灮鏋紒鈧€ｎ偁浜滈柡宥冨妽閻ㄦ垿鏌ｉ銏狀伃婵﹨娅ｇ划娆撳箰鎼淬垺瀚崇紓鍌欑椤戝棛鏁敓鐘茬畺闁汇垹澹婇弫鍌炴煕椤愩倕鏆遍柟閿嬫そ濮婃椽宕ㄦ繝鍕ㄦ闂佹寧娲╃粻鎾荤嵁婵犲洤绀冮柍琛″亾缂佽妫欓妵鍕箛閸撲焦鍋х紓浣哄У閹稿濡甸崟顖ｆ晝闁靛繈鍨婚鎺楁⒑閸︻厼甯剁紓宥咃工閻ｇ兘鎮滅粵瀣櫍闂佺粯鐟ラ幊鎰償婵犲倵鏀介柣妯活問閺嗩垱淇婇幓鎺撳殗鐎规洖缍婂Λ鍐ㄢ槈濮橆厽顔曢梻浣圭湽閸ㄧ粯鐏欓梺娲诲幗椤ㄥ﹪寮诲☉銏犵労闁告劦浜栨慨鍥⒑閹惰姤鏁遍柛銊ョ秺閹偓妞ゅ繐鐗滈弫鍥煟閹邦厽缍戞い搴㈢洴濮婃椽骞栭悙娴嬪亾閺囥埄鏁勯柛銉㈡櫆椤洟鏌熼悜妯烩拻缁炬儳鍚嬫穱濠囶敍濠靛棔姹楅梺姹囧€曠€氼剝鐏冮梺缁橈耿濞佳勭濠婂牊鐓曢柣鏇氱娴滀即鏌ㄥ┑鍫濅粶妤楊亙鍗冲畷鐓庘攽閸繂袝濠碉紕鍋戦崐鏍暜閹烘柡鍋撳銉ュ闁糕斁鍋撳銈嗗灱濞夋洟藝閿斿浜滄い鎰剁悼缁犳﹢鏌熼悷鏉款伃闁圭厧缍婇幃鐑藉级閸熷啯娲栭埞鎴︽晬閸曨偂鏉梺绋匡攻閻楁粓寮鈧獮鎺楀棘閸濆嫪澹曞┑顔筋焽閸樠勬櫠閹绢喗鐓欓梺鍨儏閻忔挳鏌熼鍝勭伈鐎规洘顨婇幊鏍煛娴ｅ摜顔戦梻鍌氬€风粈渚€骞栭锕€纾归柟闂寸劍閸婅埖绻濋棃娑氭噥濠㈣泛娴烽悿鈧梺鍝勫€告晶鐣岀不濮樿埖鈷戦柛锔诲幘鐢盯鎮介婊冧户缂侇喗妫冮幃婊冾潨閸℃﹩鍟嶉梻浣虹帛閸旀洟顢氶鐔告珷闂侇剙绉甸悡鏇㈡煙閻戞ɑ灏繛鎼枤閳ь剝顫夊ú姗€銆冮崱妤婂殫闁告洦鍓涚弧鈧繛杈剧到婢瑰﹤螞濠婂牊鈷掗柛灞剧懅椤︼箓鏌熺拠褏绡€鐎规洘绻傝灒闂傚倸顕崜銊モ攽閻愬弶顥為柟灏栨櫊瀵偊宕堕妸褏顔曢悗鐟板閸犳洜鑺辨總鍛婄厓闂佸灝顑呭ù顕€鏌＄仦鍓с€掑ù鐙呯畵瀹曟粏顦抽柛锝庡幘缁辨挻鎷呴悿顖氬帯婵犫拃鍕垫畷缂佸矁椴哥换婵嬪磼閵堝棗鐦滈梻浣虹帛閹稿憡顨ラ幖渚婄稏闁搞儺鍓氶埛鎴︽偣閸ワ絺鍋撻搹顐や壕闂備胶枪椤戝啴宕愬┑鍡╁殨妞ゆ劧绠戠粈鍐┿亜閺冨洤浜归柨娑欑矊閳规垿顢欓弬銈堚偓璺ㄧ棯椤撶喐鍊愮€规洦鍓濋妵鎰板箳閹绢垱瀚藉┑鐐舵彧缂嶁偓妞ゎ偄顦靛畷鎴︽偐缂佹鍘遍柟鍏肩暘閸ㄥ鎯岀仦鐭綊鎮崨顖滄殼闂佺懓纾崑銈嗕繆閻戠瓔鏁婇柛蹇曞帶娴滃吋绻濋悽闈浶ユい锝庡枤濡叉劙寮撮姀鐘碉紱闂佺鎻粻鎴犲瑜版帗鐓涚€广儱楠告禍婵嬫煛閸℃鐭掗柡宀€鍠栭幃婊兾熼懡銈呭箰闂備胶枪椤戝棝骞愰幖渚婄稏婵犻潧顑嗛崑鍌炲箹缁顫婃繛鑲╃帛缁绘繈鎮介棃娑楁勃閻熸粍婢橀崯顐︹€﹂崶顒€鐏抽柟棰佺劍缂嶅海绱撻崒娆戝妽閽冨崬鈹戦娑欏唉闁哄睙鍡欑杸闁规儳澧庨弳娑㈡⒑闁偛鑻晶顕€鏌ㄩ弴銊ら偗鐎殿喛顕ч埥澶愬閻樻鍟嬮梺璇查叄濞佳囧箺濠婂牊鍋柛鏇ㄥ灡閳锋垿姊婚崼鐔衡姇妞ゃ儲绮撻弻娑㈡偄妞嬪函绱炵紓渚囧枦椤曆囧煡婢舵劕顫呴柣妯诲墯濡喐绻濋悽闈涗粶婵☆偅鐟╁畷鏇㈠礃濞村鐏佹繝闈涘€搁幉锟犲煕閹烘嚚褰掓晲閸パ冨闂佹悶鍊愰崑鎾斥攽閻橆偅濯伴柛娑卞枟閻濐亪姊洪崨濠傜瑲閻㈩垽绻濋妴浣糕槈閵忊€斥偓鐑芥煠绾板崬澧鐐茬墦濮婄粯鎷呴崨濠呯闂佺ǹ绨洪崐婵嗙暦闂堟侗娼╅柣鎰灊濮规姊洪崷顓炲妺闁规瓕顕ч埢宥呪堪閸啿鎷洪梺鍛婃尰瑜板啯绂嶅┑鍫㈢＜妞ゆ棁妫勯埢鏇燁殽閻愯揪鑰跨€规洘绮嶉幏鍛存儌閸濄儳顢呮繝纰夌磿閸嬫垿宕愰弽顓炵濡わ絽鍟壕濠氭煟閺冨倸甯堕柡鍕╁劦閺屻劌鈹戦崱鈺傂ч梺缁樻尰閻╊垶骞冨鈧幃娆撳箵閹哄棙瀵栨繝鐢靛仜瀵爼骞愰幎钘夎摕闁绘梻鈷堥弫濠囨偡濞嗗繐顏柣婵囧哺閹鎲撮崟顒傤槰闂佹寧娲忛崹浠嬫偘椤曗偓楠炴鎷犻懠顒夊敽婵犵數濞€濞佳兠洪妸褏鐭欓柛顐犲劜閳锋帒霉閿濆洨鎽傞柛銈嗙懇閹鈽夐幒鎾寸彋濡炪們鍨洪悷鈺侇嚕娴犲鏁囬柣鎰暩瀹曞爼姊绘担鍛婃儓缂佸绶氬畷銏ゆ嚃閳哄啩绗夐梺缁樻煥閸氬鎮￠妷锔剧闁瑰浼濋鍫晜妞ゅ繐妫岄崑鎾舵喆閸曨剛顦ラ悗娈垮枛婢у酣骞戦姀鐘斀閻庯綆鍋掑Λ鍐ㄢ攽閻愭潙鐏﹂柣鐔讳含濡叉劙鎮欏ù瀣杸闂佺粯鍔樼亸娆徯掗悙鐑樼厸闁告侗鍠氶惌鎺戔攽閿涘嫭鏆€规洜鍠栭、娑橆潩妲屾牕鏁介梻鍌欒兌閹虫捇鎮洪妸褎宕查柍褜鍓涚槐鎺楀Ω閵堝洨鐓撳┑顔硷攻濡炶棄鐣烽锕€唯闁靛鍊栭崺娑㈡⒒娴ｈ姤銆冩繛鏉戞喘椤㈡俺顦归柨婵堝仩缁犳盯寮撮悩纰夌床婵犵妲呴崹鐢割敋瑜旈、鎾诲箻缂佹ǚ鎷洪梺绋跨箻濡潡鎳滈鍫熺厱閹兼番鍨归埢鏇炩攽閳ュ磭鎽犵紒缁樼箞瀹曞爼濡搁姀鈥愁伖闂傚倸鍊搁崐鎼佹偋閸愵喖鐤炬繝闈涚墢閻捇鏌涢幘鑼跺厡缁炬儳銈搁弻锝呂熼幐搴ｅ涧閻庤娲栭惌鍌炲蓟閿濆應妲堥柛妤冨仦閻忔洜绱撴担鍝勑ｇ紒瀣灴閸┿儲寰勬繛鐐€婚梺褰掑亰閸欏孩绂掕濮婂宕掑▎鎰偘濠电偛顦板ú鐔风暦閹惰姤鏅濋柛灞炬皑椤斿棝姊虹捄銊ユ珢闁瑰嚖鎷�
    fprintf(yyout, "%*cWhileStmt\n", level, ' ');
    cond->output(level + 4);
    if (doStmt != nullptr)
    {
        doStmt->output(level + 4);
    }
}
void BreakStmt::output(int level)
{
    if(whether_valid)
        fprintf(yyout, "%*cBreak\n", level, ' ');
    else
    {
        fprintf(stderr, "break not in loop \n");
        fprintf(yyout, "%*cerror,break not in loop\n", level, ' ');
    }
}
void ContinueStmt::output(int level)
{
    if (whether_valid)
        fprintf(yyout, "%*cContinue\n", level, ' ');
    else
    {
        fprintf(stderr, "continue not in loop \n");
        fprintf(yyout, "%*cerror,continue not in loop\n", level, ' ');
    }
}
void EmptyStmt::output(int level) 
{

}
void DoNothingStmt::output(int level)
{
    fprintf(yyout, "%*cWhileStmt\n", level, ' ');
    do_nothing_node->output(level + 4);
}



void Ast::output()
{
    fprintf(yyout, "program\n");
    if(root != nullptr)
        root->output(4);
}

void Constant::output(int level)
{
    //std::string type, value;
    //type = symbolEntry->getType()->toStr();
    //value = symbolEntry->toStr();
    //fprintf(yyout, "%*cIntegerLiteral\tvalue: %s\ttype: %s\n", level, ' ',
    //        value.c_str(), type.c_str());
    std::string type, value;
    type = symbolEntry->getType()->toStr();
    value = symbolEntry->toStr();
    if (this->symbolEntry->getType()->isInt())
    {
        fprintf(yyout, "%*cIntegerLiteral\tvalue: %s\ttype: %s\n", level, ' ',
            value.c_str(), type.c_str());
    }
    else if (this->symbolEntry->getType()->isFLOAT())
    {
        //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崰姘熼埀顒勬倵濞堝灝鏋熼柟顔煎€搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛鐏炵硶鍋撳畷鍥ㄦ畷闂侀€炲苯澧寸€规洑鍗冲鍊燁槾闁哄棴闄勭换婵囩節閸屾冻绱炲┑鈩冪叀缁犳牠寮婚敐澶婎潊闁宠桨鑳舵禒顓㈡⒑閻戔晜娅撻柛銊ㄦ硾椤曪絿鎷犲ù瀣潔濠殿喗锕徊鑺ョ妤ｅ啯鐓ユ繝闈涙閸戝湱绱掗妸銉吋闁哄苯绉烽¨渚€鏌涢幘瀵告噮缂佽京鍋炵换婵嬪礃瑜忕粻姘舵⒑濮瑰洤鐏弸顏嗙磼閳ь剛鈧綆鍋佹禍婊堟煛瀹ュ啫濡介柣銊﹀灴閺岋綁濡堕崒姘闂傚倸鍊搁崐椋庣矆娓氣偓閹潡宕惰濞存牠鏌曟繛褍瀚娑㈡⒑閸濆嫯顫﹂柛搴や含婢规洘绻濆顓犲幍闂佺顫夐崝锕傚吹濞嗘垹纾奸柣妯烘惈閸氬湱绱掓潏銊﹀鞍闁瑰嘲鎳忛幈銊╁箣椤撴繈鍋楅梺绯曟杹閸嬫挸顪冮妶鍡楀潑闁稿鎹囬弻锝堢疀閺冨倻鐤勯梺绯曟櫇閸嬨倝鐛€ｎ喗鏅滈柦妯侯槷濮规姊绘担鍛婂暈闁荤喆鍎佃棟濞村吋娼欑粻鏍ㄧ箾閸℃ê鐏╃痪鎹愭闇夐柨婵嗘噺閹牓鏌ｅ┑鎾剁瘈闁哄本鐩俊鐑芥晲閸涱収鐎烽梻浣告啞閿氱€规洦鍓熼垾鏃堝礃椤斿槈褔鏌涢埄鍐炬當鐞涜偐绱撻崒娆戝妽鐟滄澘鍟…鍥晸閻樺弬銉╂煕瀹€鈧崑娑㈡儗濡も偓椤法鎹勯搹鍦紘濡炪倖姊瑰ú鐔奉潖濞差亜宸濆┑鐘辫兌缁讳線姊洪崨濞掕偐鍒掑▎蹇曟殾闁汇垻枪閻愬﹥銇勯幒宥堝厡闁告ɑ鎮傞幃妤呭礂婢跺﹣澹曢梺璇插嚱缂嶅棝宕滃☉婧惧徍濠电姷鏁告慨鐑姐€傛禒瀣婵犻潧顑呯粻鏍煕瀹€鈧崑娑㈠几娓氣偓閺屾盯骞囬棃娑欑亪闂佺粯甯掗悘姘跺Φ閸曨垰绠抽柟瀛樼箥娴犻箖姊虹粙鍖″伐闁绘牕銈稿濠氭偄閾忓湱锛滈梺闈涚箳婵敻鎮樺澶嬧拺缂備焦蓱鐏忔壆绱掔€ｎ偅宕岄柛鈹惧亾濡炪倖宸婚崑鎾绘煕濡崵鐭掔€规洘鍨块獮妯肩磼濡厧甯楅柣鐔哥矋缁挸鐣峰⿰鍫熷亜濡炲瀛╁▓鐐箾閺夋垵鎮戞繛鍏肩懅缁鈽夐姀锛勫幈闂婎偄娴勭徊鑺ョ濠婂嫮绠剧€瑰壊鍠栧顔筋殽閻愭彃鏆㈡い锕€婀遍埀顒冾潐濞叉牠濡堕幖浣哥畺闁靛繒濮烽弳瀣煛婢跺寒鍤﹂柕鍫濐槹閳锋帡鏌涚仦鍓ф噯闁稿繐鐭傞弻娑氣偓锝冨妼婵倻鈧娲樺ú鐔煎蓟閸℃鍚嬮柛娑卞灡閻掗箖姊洪懡銈呅ｅù婊€绮欏畷婵囨償閵忊懇鏋栭悗骞垮劚椤︿即鎮￠妷锔剧瘈闂傚牊绋掗敍宥嗕繆閹绘帞绉洪柡灞界Ч閺屻劎鈧綆浜為悷銊╂⒒閸パ屾█闁哄被鍔岄埞鎴﹀幢濞嗗浚鏆柣鐐寸瀹€绋款潖濞差亝顥堟繛鎴炵懐濡偤姊洪崫銉ユ瀾闁圭ǹ鍟块锝嗙節濮橆厽娅滄繝銏ｆ硾閿曪箓顢欐繝鍥ㄢ拺缂備焦锚婵鏌℃担瑙勫€愮€殿喗濞婃俊鍫曞川閸屾稒顥堥柟顔规櫊濡啫鈽夊Δ浣稿闂傚倷鐒﹂幃鍫曞礉瀹€鍕€舵繝闈涱儐閸嬧晠鏌ｉ幋锝嗩棄缂佺姵宀稿娲敇閵娾晜顎嶅銈呴濞差厼顫忕紒妯诲閻熸瑥瀚禒鈺呮⒑缁嬪尅鏀婚柣妤佹礈閸欏懎鈹戦濮愪粶闁稿鎸搁埞鎴︽晬閸曨剚姣堥悗瑙勬礈閸犳牠銆佸☉姗嗘僵濡插本鐗曢弫浠嬫⒒閸屾瑨鍏岄柟铏崌閹椽濡歌瀹曡尙鈧箍鍎卞Λ娑€€呭畡鎵虫斀闁稿本纰嶉崯鐐烘煟閹惧瓨绀嬮柡灞炬礃瀵板嫬鈽夐姀鈽嗏偓宥夋煟韫囨捇鐛滈梻鍕楠炲牓濡搁敂鍓х槇闂佸憡渚楅崳顔界閳哄懏鈷戦柛娑橈功婢ь剟鏌ｅΔ鍐ㄐ㈡い鏇秮楠炴﹢顢欓挊澶嗗亾闁垮绠鹃柡澶嬪焾閸庢劙鎮楀鍗炴瀻闁宠鍨块弫宥夊礋椤愨剝婢€闂備胶枪閿曘儵宕归崹顔炬殾婵犻潧妫涢弳鍡涙煃瑜滈崜鐔肩嵁閹达箑顫呴柕鍫濇噽椤撴椽姊虹紒姗堜緵闁稿瀚粋宥嗐偅閸愨斁鎷婚梺鍓插亞閸犳捇鍩ｉ妶澶嬬厱閻庯絻鍔岄悘顏呫亜閵婏絽鍔﹂柟顔界懅閳ь剛鏁告灙闁冲嘲鑻灃闁绘﹢娼ф禒婊勭箾閹绢噮妫戦柛鎺撳笒椤繄鎹勯崫鍕綁缂傚倷娴囧▔娑€€呴悽顪磘ry闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崰姘熼埀顒勬倵濞堝灝鏋熼柟顔煎€搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛鐏炵硶鍋撳畷鍥ㄦ畷闂侀€炲苯澧寸€规洑鍗冲浠嬵敇閻愮數鏆柣鐔哥矋閺屻劑鎮鹃柨瀣檮闁告稑锕ゆ禍婊堟⒑閸涘﹦绠撻悗姘煎墮閳绘捇濡烽埡鍌楁嫽闂佺ǹ鏈悷褔藝閿曞倹鐓欓悹鍥囧懐鐦堥悗瑙勬礀濠€閬嶅箟閹绢喖绀嬫い鎺嗗亾鐎殿喖娼″娲捶椤撯剝顎楅梺鍝ュУ閻楃娀骞冮垾鎰佹建闁逞屽墴瀵鎮㈤崨濠勭Ф婵°倧绲介崯顖烆敁瀹ュ鈷戠紒瀣儥閸庢劙鏌涢弮鈧悷鈺呯嵁閸愩剮鏃堝川椤撶喎绁舵俊鐐€栭幐楣冨磹椤愶箑顫呴幒铏濠婂牊鐓忓鑸电☉椤╊剛绱掗悩鎰佺劷缂佽鲸甯楀蹇涘Ω閿曗偓闂夊秹姊洪悷鏉挎Щ闁硅櫕锚閻ｇ兘顢曢敃鈧粈瀣棯閻楀煫顏呯閸撗€鍋撻崗澶婁壕闂佸憡娲﹂崜娑㈠储闁秵鈷戦梻鍫熻儐瑜版帒纾块柟鎯版閺勩儵鏌ㄥ┑鍡樼闁稿鎸搁埢鎾诲垂椤旂晫浜俊鐐€ら崢楣冨礂濡警鍤曟い鎰剁畱缁犳稒銇勯幘璺轰户缂佹劗鍋炵换婵嬫偨闂堟刀銏ゆ倵濮樺崬鍘寸€规洏鍎靛畷銊р偓娑櫱氶幏缁樼箾鏉堝墽鎮奸柟铏崌椤㈡艾饪伴崨顖滐紲闁荤姴娲﹁ぐ鍐焵椤掆偓濞硷繝鎮伴钘夌窞濠电偟鍋撻～宥夋⒑闂堟稓绠冲┑顔惧厴椤㈡瑩骞掗弮鍌滐紳闂佺ǹ鏈悷褔宕濆鍡愪簻妞ゆ挾鍋為崰妯尖偓瑙勬磸閸ㄤ粙鐛弽銊﹀闁稿繐顦扮€氬ジ姊绘担鍛婂暈缂佸鍨块弫鍐晲閸ヮ煈鍋ㄩ梻渚囧墮缁夌敻鎮￠弴銏＄厽婵☆垵顕ф晶顖涚箾閸喓鐭岄柍褜鍓濋～澶娒哄鈧畷婵嗏枎閹捐泛绁﹂梺鎼炲労閸撴岸宕戠€ｎ喗鐓曟い鎰Т閻忊晜顨ラ悙鏉戝闁哄矉绲鹃幆鏃堝灳閸愯尪绶熼梻浣呵归鍡涘箲閸ヮ剙绠栨俊銈呮噺閺呮煡骞栫划鍏夊亾閹颁焦楠勯梻鍌欐缁鳖喚绮婚幋锔藉亱闁规崘顕х粻鏍ㄤ繆閵堝懎鏆為柛鐘叉閺屾盯寮撮妸銉ヮ潾濡炪倧璐熼崝鎴﹀蓟閿濆顫呴柍杞拌兌娴狀參鏌ｉ姀鈺佺仯闁哥姵鐗犻獮鍐潨閳ь剟鐛幒鎳虫梹鎷呴崫鍕闂傚倷鑳剁划顖炲礉閺囥埄鏁嬫い鎾卞灩閻撴繈鏌熼崜褏甯涢柣鎾冲暟閹茬ǹ饪伴崼婵堫槶濠电偛妫欓幐鎼佸磼閵娾晜鐓涚€广儱鍟俊鍧楁煟濠靛棛鍩ｉ柡宀嬬節瀹曞爼濡烽妷褌鐥梻浣瑰▕閺€閬嶅垂閸ф钃熸繛鎴炃氶弨浠嬫煕閳╁喚娈㈠ù灏栧亾闂傚倷绀侀悿鍥敋瑜旇棟闁汇垻枪閽冪喐绻涢幋鐐茬劰闁稿鎹囬弫宥夊础閻愬弶娈ㄦ繝銏ｎ潐濞茬喎顫忔繝姘＜婵炲棙甯掗崢锟犳煛鐎ｅ吀绨奸柕鍥у楠炲鈹戦崶鑸碉紗婵°倗濮烽崑鐐哄礉濞嗘挾宓侀柡宥冨妿閻熺懓鈹戦悩鎻掝伀闁崇鍊栫换婵嬫偨闂堟稈鏋呭┑鐐板尃閸ヨ埖鏅為梺鍦濠㈡﹢寮告笟鈧弻娑㈠焺閸愵亖濮囬梺缁樻尰濞茬喖骞冨鈧幃娆撳箵閹哄棙瀵栭梻浣芥〃閻掞箓鎮ч悩璇茶摕闁哄洨鍠撶粻楣冩煟閹伴潧澧柣婵囨礋濮婃椽骞庨懞銉︽殸闂佹悶鍔屽鈥愁嚕鐠囨祴妲堟俊顖炴敱閺傗偓闂備胶纭堕崜婵嬨€冭箛鏂款嚤闁稿本鍑瑰〒濠氭煏閸繈顎楀ù婊冨⒔缁辨帡鎮╅搹顐㈢３閻庢鍠涢褔鍩ユ径鎰潊闁绘鏁稿澶愭⒒娓氣偓閳ь剛鍋涢懟顖涙櫠鐎电硶鍋撶憴鍕缂傚秴锕ら悾椋庣矙鐠囩偓妫冨畷姗€濡歌閸嬪﹪姊婚崒娆戭槮闁硅绻濆畷婵嬫晝閸屾氨鐛ラ梺鍝勭▉閸欏繒绱為弽顓熺厱婵炴垶顭囬幗鐘绘煟閹惧磭绠婚柡灞剧洴椤㈡洟鏁愰崶鈺冩毇婵＄偑鍊戦崕鎶藉磻閵堝钃熸繛鎴炵懄閸庣喐銇勯弬娆剧吋闁稿顨呴埞鎴︻敊绾攱鏁惧┑锛勫仒缁瑩鐛繝鍥ㄦ櫢闁绘ê寮閺屾稑鈽夐崡鐐寸亾闂侀€炲苯澧柛搴㈠▕閸╃偤骞嬮敂钘変汗濡炪倖妫侀崑鎰妤ｅ啯鈷戦柟绋垮绾炬悂鏌涢妸銈囩煓闁诡喕鍗抽、姘跺焵椤掆偓閻ｇ兘宕￠悙鈺傤潔濠电偛妫楃换鍡涘磻閹剧粯鍊婚柤鎭掑劤閸樹粙姊洪悷閭﹀殶闁稿﹥鍨甸～婵堟崉閾忚鍞电紓鍌氬€烽悞锕傗€﹂崶鈺冧笉濡わ絽鍟痪褔鏌涢锝囩畵闁抽攱妫冮弻鐔碱敊缁楀搫浠梺鍝勭焿缂嶄線宕洪崟顖氱闁冲搫鍟铏繆閻愵亜鈧倝宕㈤悡骞熸椽顢橀姀銏犵ウ闂佹悶鍎洪崜姘跺磻鐎ｎ喗鐓曟い鎰Т閻忊晜顨ラ悙鑼ⅵ婵﹥妞介幃鐑藉级閹稿骸鈧垶姊烘导娆戠У濞存粠浜滈悾椋庢兜閸涱偂姹楅梺鍦劋閸ㄥ綊宕㈠ú顏呪拺闁告繂瀚婵嬫煕閿濆啫鍔氶柍缁樻尰缁傛帞鈧綆鍋嗛崢浠嬫⒑瑜版帒浜伴柛銊ャ偢瀹曠數鈧綆浜跺鈩冦亜閹板墎绋婚柣銊﹀灩缁辨帗娼忛妸銉﹁癁闂佽鍠掗弲娑㈡偩閻戣棄鐐婇柕濞垮劚閻忥繝姊虹拠鎻掝劉妞ゆ梹鐗犲畷鏉课旈埀顒勨€﹂崶褉鏋庨柟鐑樻煣缁ㄧ兘姊婚崒娆戠獢闁逞屽墰閸嬫盯鎳熼娑欐珷妞ゆ牗绮庣壕濂告煕鐏炵偓鐨戦弫鍫ユ⒑鐠団€虫灍闁荤啿鏅犻妴浣肝旀担鐟邦€撶紓浣割儏閵囨妲愰悢灏佹斀闁绘ɑ鍓氶崯蹇涙煕閻樻剚娈滈柕鍡楀暣瀹曘劑顢橀崶銊р槈閾绘牕霉閿濆牜娼愰柣婵堝厴濮婅櫣鎲撮崟闈涙櫛闂佸摜濮靛銊︾珶閺囥垺鍋傞幖鎼枤閸炵敻鏌ｉ悩鐑樸€冮悹鈧敃鍌氱？闁规壆澧楅悡蹇涙煕閵夛絽濡跨紒鐘靛仱閺岋紕浠︾化鏇炰壕鐎规洖娲﹀▓鏇㈡煟鎼搭垳绉甸柛鎾寸洴閺佸秹鎮㈤崫銉ь啎闁诲海鏁告灙妞ゅ孩鎸搁埞鎴﹀灳閾忣偄鏋犲Δ鐘靛仜缁绘﹢骞冮埡鍐╁珰闁肩⒈鍓﹂崬浠嬫⒒娴ｅ憡璐￠柛搴涘€楅幑銏ゅ礃椤忓棛顦梺绯曞墲缁嬫帡鍩涢幒妤佺厱閻忕偛澧介幊鍕磼娴ｅ搫顣奸柕鍥у瀵挳鎮㈤崫鍕ㄦ嫲闁诲氦顫夊ú鎴﹀础閹剁晫宓佹俊顖氬悑鐎氭岸鏌熺紒妯虹闁逞屽墻閸樼晫鎹㈠☉姘ｅ亾閻㈢櫥鐟扮摥婵＄偑鍊栭崹鐢稿箠閹邦喖鍨濆┑鐘宠壘閹硅埖銇勯幘璺烘瀻闁哄鍨垮铏圭磼濮楀棙鐣堕梺缁橆殔濡稓鍒掗鐑嗘僵闁煎摜鏁搁崢閬嶆煟鎼搭垳鍒板褍娴峰褔鍩€椤掑嫭鈷戦柛婵嗗椤︻剟鏌涢幘瀵哥畵妞ゆ洏鍎靛畷鐔碱敇濞戞ü澹曢梺鎸庣箓缁ㄥジ骞夋ィ鍐╃厱闁靛牆鎷嬮崕鏃€鎱ㄦ繝鍛仩缂侇喗鐟╁畷褰掝敊閻愵儷鐐测攽閻愯尙鎽犵紒顔惧У缁傚秴饪伴崼顒傜◤闂婎偄娲﹀鑽ゅ姬閳ь剙鈹戦鏂や緵闁告挻鐩弫宥呪堪閸愶絾鏂€濡炪倖姊归弸缁樼瑹濞戙垺鐓曢煫鍥ㄦ閼拌法鈧娲橀崹鍧楃嵁濡吋瀚氶柤纰卞墻閸熷洤鈹戦悙鑸靛涧缂佽弓绮欓獮妤€饪伴崼鐔告珫濠电偛妫欓幐濠氬煕閹寸姵鍠愰柣妤€鐗嗙粭姘舵煥濞戞艾鏋涢柡宀嬬秮楠炴帡鎮欓悽鍨闁诲氦顫夊ú姗€宕硅ぐ鎺戠劦妞ゆ帒锕︾粔鐢告煕閻樻剚娈滈柟顕嗙節瀵挳濮€閿涘嫬骞嶅┑鐐差嚟閸樠囨偤閵娿儮妲堢憸鏃堝蓟閿涘嫧鍋撻敐搴′簽婵炲弶鎸抽弻鈥崇暆鐎ｎ剛袦婵犵鈧磭鍩ｇ€规洘鍎奸ˇ鍗烆熆鐟欏嫭绀堢紒杈ㄦ尰閹峰懘鎼归悷鎵偧缂傚倷娴囬褔宕导鏉戠闁靛繒濮弨浠嬫倵閿濆簼绨介柨娑欑矊閳规垿鎮欓弶鎴犱桓闁藉啴浜堕弻锝呪攽閸ャ劌鍞夊┑顔硷功缁垶骞忛崨鏉戝窛闂傚牊绋撶粔鐑樸亜閵忥紕澧电€规洖宕埥澶娢熺喊鍗炴暪闂傚倷绀佺紞濠偽涚捄銊︻偨闁靛牆娲ㄧ粈濠囨煛瀹ュ啫濡跨紒鈾€鍋撻梻鍌氬€搁悧濠冪瑹濡ゅ懎纾挎繛鎴欏灪閻撴稑霉閿濆懏鍟炴い銉ｅ灲閺岋紕浠﹂崜褋鈧帒霉閻欏懐鐣电€规洘绮忛ˇ鎶芥煛閸涱厹鍋㈡慨濠冩そ濡啫霉閵夈儳澧︾€殿喗褰冮オ浼村醇濠靛牆骞堥梻浣侯攰閹活亪姊介崟顖涘亗闁哄洨鍠撶弧鈧梻鍌氱墛缁嬫帡藟濠婂嫨浜滈煫鍥风到婢ь垶鏌曢崶褍顏い銏℃礋椤㈡宕掑⿰鍕啌闂傚倷绀侀幖顐﹀嫉椤掑嫭鍎庢い鏍ㄥ嚬閸ゆ洘銇勯弴妤€浜鹃梺绯曟杹閸嬫挸顪冮妶鍡楃瑨閻庢凹鍓熷畷褰掑磼閻愬鍘遍悷婊冮叄閵嗗啴宕煎┑鍫熸婵炴挻鍩冮崑鎾绘煛鐏炲墽娲存鐐叉喘濡啫鈽夊▎鎴滈偗濠碉紕鍋戦崐銈夊磻閸曨垰绠犳慨妞诲亾鐎殿喖顭峰鎾閻樿鏁规繝鐢靛█濞佳兠洪妶鍛瀺闁挎繂娲ㄧ壕钘壝归敐鍥ㄥ殌濠殿喖鐗忕槐鎺斺偓锝庡亜缁椦囨煙楠炲灝鐏╅柍瑙勫灩閳ь剨缍嗛崑鍕濞差亝鈷掗柛灞炬皑婢ф盯鏌涢幒鍡椾壕闂備線娼х换鍫ュ磹閺嶎厼纾归柛顭戝亝閸欏繑鎱ㄥ璇蹭壕濠碘槅鍋夊▔鏇㈡嚍闁秵鍤嶉柕澶堝€楃粻姘舵⒑閸涘﹦鎳冩い锔诲灡閹便劌顓奸崶锝呬壕婵炲牆鐏濋弸鐔兼煙濮濆本鐝柟渚垮姂閸┾偓妞ゆ帒瀚悡鍐⒑濞嗘儳鐏犲ù婊堢畺濮婇缚銇愰幒婵囶棖缂備緡鍣崹鎶藉箲閵忕姭妲堟繛鍡樺姉缁夊爼姊洪崨濠冨瘷闁告劑鍔庨崢鎺楁⒒閸屾瑨鍏屾い銏狅攻閹便劑濡堕崶褍鐏婇柟鑹版彧缁蹭粙宕瑰┑瀣厸闁告劑鍔庢晶娑㈡煕婵犲嫮甯涘ǎ鍥э躬椤㈡盯鎮欑€涙褰哥紓鍌欓檷閸斿矂鈥﹀畡閭︽綎婵炲樊浜濋ˉ鍫熺箾閹达綁鍝烘い搴℃濮婂宕掑顒変患闁诲孩鍑归崜鐔煎箖閹呮殝闁规鍠楀▓鏇㈡⒑闁偛鑻晶鎾煕閳规儳浜炬俊鐐€栫敮鎺楀窗濮橆剦鐒介柟閭﹀幘缁犻箖鏌涘▎蹇ｆ闁兼澘娼￠弻鏇㈠幢濡も偓閺嗭絿鈧娲忛崝宥咁焽韫囨稑绀堢憸蹇涘煟閵堝棛绡€闁汇垽娼у瓭闂佹寧娲忛崐婵嬪箖瑜斿畷鍗炩槈濞嗗繆鍋撻崸妤佺厱妞ゎ厽鍨垫禍鏍瑰⿰鍕煉闁哄瞼鍠栧畷顐﹀礋椤掑顥ｅ┑鐐茬摠缁秹宕曢幎瑙ｂ偓鏃堝礃椤斿槈褔鏌涢埄鍐剧劷闁宠绋撶槐鎾诲磼濮樻瘷锛勭磼閼搁潧鍝哄┑鈥崇摠缁楃喖鍩€椤掆偓椤曪綁骞橀纰辨綂闂佹枼鏅涢崯顖炴偟閺嶎厽鈷掑〒姘ｅ亾闁逞屽墰閸嬫盯鎳熼娑欐珷闁规鍠氱壕鍏笺亜閺傚灝鈷旈悽顖涚〒缁辨帞绱掑Ο鑲╃暤濡炪値鍋呯换鍫ャ€佸鈧幃娆撳级閸噮鍤欓梻鍌氬€搁崐鐑芥倿閿曞倸绠栭柛顐ｆ礀绾炬寧绻濇繝鍌滃缂佲偓閸愨斂浜滈煫鍥ㄦ尵婢с垻鈧娲樼划宀勫煘閹达附鏅柛鏇ㄥ亗閺夘參姊虹粙鍖℃敾闁搞劌鐏濋悾鐑藉即閵忊€虫濡炪倖甯婄粈浣规償婵犲洦鈷戦柛鎾村絻娴滄繄绱掔拠鑼㈡い顓炴喘瀵粙濡歌椤旀洟鎮楅悷鏉款棌闁哥姵娲滈懞閬嶅礂缁楄桨绨婚梺闈涱槶閸庤櫕鏅跺☉姘辩＜缂備焦顭囧ú瀛橆殽閻愬樊鍎旈柟顔界懅閹瑰嫭绗熼娑辨（婵犵绱曢崑鎴﹀磹瑜忓濠冪鐎ｎ亞顔愬銈嗗姧缁犳垿鎮″鈧弻鐔告綇閸撗呮殸闂佺粯鎼换婵嬪蓟濞戙垹鐒洪柛鎰剁細濞岊亞绱撴担闈涘闁告鍥ｂ偓鏃堝礃椤斿槈褔鏌涢幇鈺佸妞ゎ剙鐗撳娲箰鎼淬垹顦╂繛瀛樼矤娴滄繃绌辨繝鍕ㄥ亾濞戞瑯鐒介柣鐔风秺閺屽秷顧侀柛鎾寸懇椤㈡岸鏁愭径妯绘櫔闂侀€炲苯澧柣锝囧厴閹剝鎯斿Ο缁樻澑闂佽鍑界紞鍡樼濠靛姹叉い蹇撶墛閳锋帡鏌涚仦鍓ф噮妞わ讣绠撻弻鐔哄枈閸楃偘绨婚柣鎾卞€濋弻鏇熺箾閻愵剚鐝旈梺姹囧€ら崳锝夊蓟濞戞粠妲煎銈冨妼閹虫劗鍒掓繝姘兼晬婵炴垶姘ㄩ鏇㈡倵閻熸澘顥忛柛鐘虫礈閼鸿鲸绺介崨濠勫幗闂佽宕樺▔娑㈠几濞戙垺鐓涚€光偓鐎ｎ剙鍩岄柧浼欑秮閺屾稑鈹戦崱妤婁患缂備焦顨忛崣鍐潖濞差亝鍋傞幖绮规濡本绻涚€涙鐭ゅù婊庝簻椤曪絿鎷犲ù瀣潔闂侀潧绻掓慨鐢杆夊┑瀣厽闁绘ê鍘栭懜顏堟煕閺傚潡鍙勭€规洘绻堥、娑㈡倷閺夋垟鍋撻崹顐ょ闁割偅绻勬禒銏ゆ煛鐎ｎ剙鏋涢柡宀€鍠栭、娆撴偂鎼存ê浜鹃柛褎顨嗛崑妯汇亜閺冨牊鏆滈柛瀣崌閺佹劖鎯旈埄鍐憾闂備礁鎼幊蹇曞垝閹捐钃熼柨婵嗩槹閺呮煡鏌涘☉娆愮凡妞ゅ浚鍘艰灃闁绘﹢娼ф禒锕傛煕閺冣偓閻熴儵锝炶箛娑欐優閻熸瑥瀚悵浼存⒑閸︻厾甯涢悽顖涘笒琚欓柟閭﹀枤绾句粙鏌涚仦鐐殤鐎涙繂鈹戦埥鍡椾簼闁荤啙鍛灊閻庯綆浜堕崥瀣熆鐠虹尨韬柛鐐茬埣濮婃椽宕崟顒€绐涢梺鍝ュТ鐎涒晝绮嬪鍛傛棃宕ㄩ瑙勫缂傚倷绀侀鍫濃枖閺囩姷涓嶉柟鎯板Г閻撳啰鈧懓瀚竟鍡樻櫠閺囥垺鐓冪紓浣股戠亸鎵磼閸屾稑绗ч柍褜鍓ㄧ紞鍡涘磻閸℃稑鍌ㄥù鐘差儐閳锋垹鎲搁悧鍫濈瑨濞存粈鍗抽弻娑㈠Ω閵堝懎绁梺璇″灠閸熸挳骞栬ぐ鎺戞嵍妞ゆ挾濯寸槐鏌ユ⒒娴ｈ櫣甯涢柨姘繆椤栨熬韬柟顔瑰墲缁轰粙宕ㄦ繝鍕箰闁诲骸鍘滈崑鎾绘煃瑜滈崜鐔风暦娴兼潙鍐€妞ゆ挻澹曢崑鎾存媴缁洘鐎婚梺鍦亾濞兼瑦绂掓總鍛婂€甸柛蹇擃槸娴滈箖姊洪柅鐐茶嫰婢ф挳鏌熼鐟板⒉鐎垫澘瀚伴獮鍥敇濞戞瑥顏归梻鍌欐祰瀹曠敻宕伴幇顓犵彾闁糕剝绋掗弲顒傗偓骞垮劚椤︿即鎮￠弴銏″€甸柨婵嗗暙婵″ジ鏌嶈閸撴岸鎮уΔ鍐煔閺夊牄鍔庣弧鈧梺鎼炲劘閸斿矂鍩€椤掆偓椤兘寮婚妶澶婄畳闁圭儤鍨垫慨鏇炩攽閻愬弶鍣规俊顐ｇ〒濡叉劙骞樼€涙ê顎撻梺闈╁瘜閸樼ǹ危閸繍娓婚柕鍫濇閻忋儵鎮楀顐㈠祮闁绘侗鍠氶埀顒婄秵閸犳宕愭繝姘厾闁诡厽甯掗崝妤呮煙瀹勯偊鐓兼慨濠呮缁瑩骞愭惔銏″闂備胶鍘х紞濠勭不閺嶎厼鏄ラ柍褜鍓氶妵鍕箳閹存繍浼屽┑鈽嗗亝閸ㄥ湱妲愰幒妤婃晩闁兼祴鏅涢·鈧紓鍌欑劍椤ㄥ牓宕伴弽顓炴槬闁逞屽墯閵囧嫰骞掗幋婵愪紑閻庤鎸风欢姘跺蓟閳ユ剚鍚嬮幖绮光偓鍐差劀闂備浇妗ㄧ粈渚€宕幘顔艰摕闁靛ň鏅涢崡铏繆閵堝倸浜炬繛瀛樼矊婢х晫妲愰幒妤佸€锋い鎺嗗亾闁告柣鍊楃槐鎾愁吋閸滃啳鍚悗娈垮枦椤曆囧煡婢舵劕顫呴柍鍝勫€瑰▍鍥⒒娴ｇ懓顕滅紒璇插€歌灋婵炴垟鎳為崶顒€唯鐟滃繒澹曟總鍛婄厽闁逛即娼ф晶顕€鏌涢弬璇测偓妤冩閹炬剚鍚嬮柛婊冨暢閸氼偊鎮楀▓鍨灈妞ゎ厾鍏樺畷瑙勩偅閸愩劎鐤€婵炶揪绲介幉锟犲磹椤栫偞鈷戠痪顓炴噹娴滃綊鎮跺☉鏍у姦闁糕斁鍋撳銈嗗笒閸燁偊鎯冨ú顏呯厽闁哄稁鍘洪幉鐐叏婵犲嫮甯涢柟宄版嚇瀹曘劑妫冨☉姘毙ㄥ銈冨灪閻楃姴鐣烽崡鐐╂婵☆垳鈷堥崬鍫曟⒒娴ｅ摜绉烘俊顐ユ硶濞嗐垽濡堕崶鈺冾槸闂佸搫绉查崝搴ｅ姬閳ь剟姊婚崒姘卞濞撴碍顨婂畷鏇炩槈閵忥紕鍘告繛杈剧到閹诧繝藟閵忋倖鐓涢悘鐐插⒔閵嗘帒霉閻欏懐鐣电€规洘绮忛ˇ鎶芥煛閸涱厹鍋㈡慨濠冩そ濡啫霉閵夈儳澧︾€殿喗褰冮オ浼村醇濠靛牆骞堥梻浣侯攰閹活亪姊介崟顖涘亗闁哄洨鍠撶弧鈧梻鍌氱墛缁嬫帡藟濠婂嫨浜滈煫鍥风到婢ь垶鏌曢崶褍顏い銏℃礋椤㈡宕掑⿰鍕啌闂傚倷绀侀幖顐﹀嫉椤掑嫭鍎庢い鏍ㄥ嚬閸ゆ洘銇勯弴妤€浜鹃梺绯曟杹閸嬫挸顪冮妶鍡楃瑨閻庢凹鍓熷畷褰掑磼閻愬鍘遍悷婊冮叄閵嗗啴宕煎┑鍫熸婵炴挻鍩冮崑鎾绘煛鐏炲墽娲存鐐叉喘濡啫鈽夊▎鎴滈偗濠碉紕鍋戦崐銈夊磻閸曨垰绠犳慨妞诲亾鐎殿喖顭峰鎾閻樿鏁规繝鐢靛█濞佳兠洪妶鍛瀺闁挎繂娲ㄧ壕钘壝归敐鍥ㄥ殌濠殿喖鐗忕槐鎺斺偓锝庡亜缁椦囨煙楠炲灝鐏╅柍瑙勫灩閳ь剨缍嗛崑鍕濞差亝鈷掗柛灞炬皑婢ф盯鏌涢幒鍡椾壕闂備線娼х换鍫ュ磹閺嶎厼纾归柛顭戝亝閸欏繑鎱ㄥ璇蹭壕濠碘槅鍋夊▔鏇㈡嚍闁秵鍤嶉柕澶堝€楃粻姘舵⒑閸涘﹦鎳冩い锔诲灡閹便劌顓奸崶锝呬壕婵炲牆鐏濋弸鐔兼煙濮濆本鐝柟渚垮姂閸┾偓妞ゆ帒瀚悡鍐⒑濞嗘儳鐏犲ù婊堢畺濮婇缚銇愰幒婵囶棖缂備緡鍣崹鎶藉箲閵忕姭妲堟繛鍡樺姉缁夊爼姊洪崨濠冨瘷闁告劑鍔庨崢鎺楁⒒閸屾瑨鍏屾い銏狅攻閹便劑濡堕崶褍鐏婇柟鑹版彧缁蹭粙宕瑰┑瀣厸闁告劑鍔庢晶娑㈡煕婵犲嫮甯涘ǎ鍥э躬椤㈡盯鎮欑€涙褰哥紓鍌欓檷閸斿矂鈥﹀畡閭︽綎婵炲樊浜濋ˉ鍫熺箾閹达綁鍝烘い搴℃濮婂宕掑顒変患闁诲孩鍑归崜鐔煎箖閹呮殝闁规鍠楀▓鏇㈡⒑闁偛鑻晶鎾煕閳规儳浜炬俊鐐€栫敮鎺楀窗濮橆剦鐒介柟閭﹀幘缁犻箖鏌涘▎蹇ｆ闁兼澘娼￠弻鏇㈠幢濡も偓閺嗭絿鈧娲忛崝宥咁焽韫囨稑绀堢憸蹇涘煟閵堝棛绡€闁汇垽娼у瓭闂佹寧娲忛崐婵嬪箖瑜斿畷鍗炩槈濞嗗繆鍋撻崸妤佺厱妞ゎ厽鍨垫禍鏍瑰⿰鍕煉闁哄瞼鍠栧畷顐﹀礋椤掑顥ｅ┑鐐茬摠缁秹宕曢幎瑙ｂ偓鏃堝礃椤斿槈褔鏌涢埄鍐剧劷闁宠绋撶槐鎾诲磼濮樻瘷锛勭磼閼搁潧鍝哄┑鈥崇摠缁楃喖鍩€椤掆偓椤曪綁骞橀纰辨綂闂佹枼鏅涢崯顖炴偟閺嶎厽鈷掑〒姘ｅ亾闁逞屽墰閸嬫盯鎳熼娑欐珷闁规鍠氱壕鍏笺亜閺傚灝鈷旈悽顖涚〒缁辨帞绱掑Ο鑲╃暤濡炪値鍋呯换鍫ャ€佸鈧幃娆撳级閸噮鍤欓梻鍌氬€搁崐鐑芥倿閿曞倸绠栭柛顐ｆ礀绾炬寧绻濇繝鍌滃缂佲偓閸愨斂浜滈煫鍥ㄦ尵婢с垻鈧娲樼划宀勫煘閹达附鏅柛鏇ㄥ亗閺夘參姊虹粙鍖℃敾闁搞劌鐏濋悾鐑藉即閵忊€虫濡炪倖甯婄粈浣规償婵犲洦鈷戦柛鎾村絻娴滄繄绱掔拠鑼㈡い顓炴喘瀵粙濡歌椤旀洟鎮楅悷鏉款棌闁哥姵娲滈懞閬嶅礂缁楄桨绨婚梺闈涱槶閸庤櫕鏅跺☉姘辩＜缂備焦顭囧ú瀛橆殽閻愬樊鍎旈柟顔界懅閹瑰嫭绗熼娑辨（婵犵绱曢崑鎴﹀磹瑜忓濠冪鐎ｎ亞顔愬銈嗗姧缁犳垿鎮″鈧弻鐔告綇閸撗呮殸闂佺粯鎼换婵嬪蓟濞戙垹鐒洪柛鎰剁細濞岊亞绱撴担闈涘闁告鍥ｂ偓鏃堝礃椤斿槈褔鏌涢幇鈺佸妞ゎ剙鐗撳娲箰鎼淬垹顦╂繛瀛樼矤娴滄繃绌辨繝鍕ㄥ亾濞戞瑯鐒介柣鐔风秺閺屽秷顧侀柛鎾寸懇椤㈡岸鏁愭径妯绘櫔闂侀€炲苯澧柣锝囧厴閹剝鎯斿Ο缁樻澑闂佽鍑界紞鍡樼濠靛姹叉い蹇撶墛閳锋帡鏌涚仦鍓ф噮妞わ讣绠撻弻鐔哄枈閸楃偘绨婚柣鎾卞€濋弻鏇熺箾閻愵剚鐝旂紒鐐劤椤兘寮婚敐澶婃闁割煈鍣Λ灞解攽閻愯尙澧旂紒顔界懇瀵鈽夐姀鐘栥劍銇勯弮鍌氬付妞ゎ偒浜娲箮閼恒儲鏆犻梺鎼炲妼濠€鍗炍ｉ幇鏉跨閻庢稒锚椤庢挻绻濆▓鍨灍闁糕晛鐗婄粋宥夘敂閸繄鐣哄┑顔姐仜閸嬫挸鈹戦敍鍕幋闁轰礁鍊婚幑鍕Ω瑜忛弳鏉库攽閿涘嫬浜奸柛濠冪墵瀹曟繆顦寸紒杈╁仱瀹曞崬顪冪紒妯间簴闂備礁鎼ú銏ゅ垂濞差亝鍋傛繛鎴欏灪閻撴洘绻涢幋婵嗚埞妤犵偞锕㈤弻娑㈠煛鐎ｎ剛蓱濡炪們鍔婇崕鐢稿箖濞嗘劖缍囬柟瑙勫姀閸嬫捇骞愭惔娑楃盎闂侀潧楠忕槐鏇㈠箠閸ヮ剚鐓欐い鏃傚帶閳锋柨霉閻欏懐鐣甸柟绋匡攻瀵板嫬鐣濇繝鍌涙毆婵犵數濮烽弫鍛婃叏閻戣棄鏋侀柟闂寸绾惧鏌ｅΟ娆惧殭缂佺姴鐏氶妵鍕疀閹炬惌妫ょ紓浣插亾濠电姴娲﹂悡鍐喐濠婂牆绀堟慨妯挎硾妗呴梺鍛婃处閸ㄥジ寮崘顔界厪闊洤锕ュ▍鍡欑箔閹达附鈷掑ù锝夘棑娑撹尙绱掗煫顓犵煓鐎规洘鐓″濠氬Ψ閿曗偓娴犵厧鈹戦悩缁樻锭妞ゆ垵鎳橀幃鐐哄垂椤愮姳绨婚梺鍦劋閸ㄧ敻顢旈埡鍛厽闁圭儤鍨规禒娑㈡煏閸パ冾伃妤犵偞甯掗濂稿炊椤忓棛宕堕梻鍌欑閹芥粓宕版惔锝嗗床婵せ鍋撶€殿喛顕ч埥澶愬閳╁啯鐝抽梺纭呭亹鐞涖儵宕滃┑瀣仧婵犻潧顑嗛崐鐢告偡濞嗗繐顏紒鈧崘顏嗙＜妞ゆ棁濮ゅ畷灞绢殽閻愭彃鏆ｅ┑顔瑰亾闂侀潧鐗嗗Λ宀勫箯瑜版帗鈷戠憸鐗堝笒娴滀即鏌涢悩鍐插摵闁诡喚鍋撻妶锝夊礃閳圭偓瀚藉┑鐐舵彧缁叉寧鐏欏銈冨劚缁绘﹢寮诲☉姘ｅ亾閿濆簼绨奸柛锝呯秺閹繝濡舵径瀣幐閻庡箍鍎辨鎼佺嵁濡皷鍋撶憴鍕矮缂佽埖宀稿濠氬即閿涘嫮鏉告繝鐢靛仦閸庤櫕绂嶆ィ鍐╃厾缁炬澘宕晶浼存煟閿濆鐣烘慨濠勭帛缁楃喖鍩€椤掆偓椤洩顦归柟顔ㄥ洤骞㈡繛鎴烆焽椤斿棝姊洪悷鎵憼缂佽绉撮…鍥冀閵娿倗绠氶梺闈涚墕閹冲酣寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍嵁閹捐绠虫繝闈涙婢瑰姊绘担椋庝覆缂傚秳鐒︾粋宥夊醇閺囩偠鎽曢悗骞垮劚濞诧箓宕伴崱娑欑厱闁哄洢鍔屾禍鐐裁归悡搴ｇ劯婵﹨娅ｉ幏鐘诲灳閾忣偆浜堕梻浣藉吹閸熷潡寮插☉銏╂晪闁挎繂顦柋鍥煛閸モ晛浠фい鎾存そ濮婅櫣绱掑Ο鎾诡潐閵囨棃骞栨担纰樺亾娴ｅ壊娼ㄩ柍褜鍓欓～蹇旂節濮橆剛锛滃┑鐐叉閸旀濡堕弶娆炬富闁靛牆楠告禍婊勩亜閿旂偓鏆鐐插暙鐓ゆい蹇撳瀹撳棝姊洪棃娑氱畾闁哄懏绮撳鏌ヮ敂閸喎浠┑鐘诧工鐎氼參藟閸儲鐓曢柕濞炬櫇閻ｈ櫣鈧鍣崳锝呯暦閻撳簶鏀介柛顐亝琚ㄩ梻鍌氬€风粈渚€宕崸妤€绠规い鎰剁畱缁愭鎱ㄥ鍡楀姃鐟滅増甯掔粈瀣亜閺嶃劎銆掗柛妯圭矙閺岀喖宕楅崗鐓庡壒闂佹悶鍔岀壕顓犲垝閺冨牊顥堟繛娣灮閹虫捇銈导鏉戠妞ゆ挾濮烽弶浠嬫⒒娴ｇ瓔鍤冮柛鐘愁殜閵嗗啯绻濋崶銉㈠亾娴ｈ倽鏃堝川椤撶媭妲规俊鐐€栭崹鍏兼叏閵堝洠鍋撳顑惧仮婵﹥妞介幊锟犲Χ閸涱喚鈧箖姊洪懡銈呮瀭闁稿孩濞婇崺鈧い鎺嶇閸ゎ剟鏌涢幘瀵搞€掗柛鎺撳浮瀹曞ジ濡烽妷褜妲版俊鐐€栧濠氬疾椤愶箑鍌ㄩ梺顒€绉甸埛鎴︽煕閹邦剙绾ч柟顖氱墦閺屾稒绻濋崟顓炵闂佸搫鎳庨悥濂稿箖閻ｅ苯鏋堟俊顖濇〃婢规洟鏌ｉ悢鍝ユ噧閻庢凹鍘剧划鍫ュ礃椤旂晫鍘遍梺闈浨归崐妤冪矈閻戣姤鐓熼煫鍥ㄦ煥濞搭噣鏌″畝瀣М闁诡喒鏅滃蹇涘煛婵犲唭婊呯磽娴ｇǹ鈷旈柧蹇撻叄瀹曘垽宕滆閸ㄦ繈鎮归崶褎鈻曟繛灏栨櫊閺屻倝宕妷顔芥瘜闂佸綊鏀卞钘夘潖婵犳艾纾兼慨妯哄船椤も偓缂傚倷绀侀鍡涘箰婵犳艾鐤鹃柛顐ｆ礀閸楁娊鏌曡箛銉х？闁告ǹ椴哥换婵嬫偨闂堟刀銏ゆ煕婵犲倻绉虹€规洦鍨堕幃娆撴偨閻㈢绱查梻浣呵归張顒€顫濋妸锔惧ⅰ闂傚倷绶氬褔鎮ч悙鐢电濞达絽鎽滈弳锔芥叏濡炶浜鹃梺绯曟櫇閸嬨倝骞冨▎鎿冩晢闁逞屽墴椤㈡棁銇愰幒鎾嫽闂佺ǹ鏈悷銊╁礂鐏炰勘浜滈柕蹇曞闊剛鈧娲橀崹鍧楃嵁閸ヮ剙绾ч悹渚厜缁辫鲸绻濋悽闈涗沪闁割煈鍨跺畷纭呫亹閹烘挸鈧潡鏌涢…鎴濅簴濞存粍绮撻弻鐔煎传閸曨厜銉╂煕韫囨捁瀚版い顓″劵椤︽挳鏌℃担鍓茬吋鐎殿喖顭烽幃銏㈠枈鏉堛劍娅撻梻渚€娼ф灙闁稿骸寮跺鍕礋椤栨稓鍘遍柣搴秵閸嬪嫭鎱ㄥ澶嬬厸閻忕偠顕ч埀顒佺墵楠炲牓濡搁埡浣虹杸濡炪倖鍨煎Λ鍕妤ｅ啯鐓涢柛銉㈡櫅鍟搁梺娲诲幖閻楁捇寮婚妸鈺佸嵆闁绘劖绁撮崑鎾广亹閹广倕顦扮换婵嬪礃閿旇法鐩庨梻浣筋潐婢瑰寮插☉銏″€舵い蹇撳閻斿棝鎮归崫鍕儓妞ゅ浚鍋嗙槐鎺撴綇閵娿儳顑傞梺閫炲苯澧剧紓宥呮瀹曟垿宕熼鈧ぐ鎺濇晩闂佹鍨版禍楣冩偡濞嗗繐顏紒鈧崘顔界厱闁哄啠鍋撻柣妤佺矒楠炴垿濮€閻橆偅鏂€闂佺硶妾ч弲婊堝磽闂堟侗娓婚柕鍫濇鐏忛潧鈹戦鑺ュ唉闁绘侗鍠栭～婊堝焵椤掆偓椤繒绱掑Ο璇差€撶紓浣圭☉椤戝懎鈻撻幇鐗堚拺闁告稑饪撮悞浠嬫煛閸滀礁浜扮€殿噮鍋婇、姘跺焵椤掑嫮宓侀柟鐑樺殾閺冨牆鐒垫い鎺戝€婚惌鍡楊熆鐠轰警鍎戠紒鈾€鍋撳┑鐘垫暩婵挳宕愰幖渚囨晜妞ゆ劑鍊楃壕濂告煟濡灝鍚归柣锝嗘そ閺岋紕浠﹂崜褉妲堥梺瀹犳椤﹂潧鐣烽敓鐘冲€烽柟娈垮枤濞夊潡姊婚崒娆愮グ妞ゆ泦鍛床闁归偊鍓﹂崵鏇㈡煕椤愶絾绀€缂佺姷鍋ら弻鏇熺節韫囨搩娲紓浣叉閸嬫捇姊绘担鍛婂暈闁告柨绻樺顒勫磼濞戞凹娴勯梺闈涚箞閸婃牠宕愰悽鍛婂仭婵炲棗绻愰顏勵熆鐠哄搫顏柡灞剧〒閳ь剨缍嗘禍宄邦啅閵夆晜鐓熼柨婵嗘搐閸樻挳鏌熼鍝勭伄闁哥姴锕ュ蹇涘Ω閿旂晫褰囬梻鍌氬€烽懗鍓佸垝椤栨娑欐媴閹肩偐鍋撻幒鏃€鍠嗛柛鏇ㄥ幘瑜伴箖姊洪崫鍕偍闁搞劍妞介崺娑㈠箣閿旂晫鍘遍梺鍦亾濞兼瑧寰婄拠瑁佺懓饪伴崟顓犵暫缂備胶绮惄顖氱暦閿濆棗绶為悘鐐缎掗幏鈥斥攽閻橆喖鐏辨い顐㈩槸鐓ゆ繝闈涱儏妗呴梺鍦濠㈡绮婚幎鑺ョ厵闁硅鍔栫涵鐐殽閻愭潙濮嶉柡宀嬬稻閹棃鏁嶉崟顓熸闂備胶枪濮橈附銇旈幖浣碘偓鍐Ψ閳轰礁绐涙繝鐢靛Т鐎氬嘲煤閹间焦鈷戦悷娆忓椤ュ顭胯椤ㄥ﹪骞冮敓鐘插嵆闁靛繆鈧枼鍋撻悽鍛婄叆婵犻潧妫濋妤€霉濠婂嫮绠為柟顔角圭粻娑㈡晲閸涱厾顔戦梻渚€娼уú銈団偓姘嵆閵嗕線寮撮姀鈩冩珳闂佹悶鍎弲婵嬪级閼恒儰绻嗛柣鎰典簻閳ь剚鍨垮畷鐟懊洪鍌楀亾閸愨晝绡€闁稿本绮嶅▓楣冩⒑閸︻厼鍔嬮柛銊у枛瀵劑鏁冮崒娑氬幗闂佽婢橀崥鈧紒銊嚙閳规垿顢欓悡搴樺亾瑜版帒绠為柕濠忓缁♀偓闂佸憡鍔忛弬鍌涚閵忋倖鍊甸悷娆忓婢跺嫰鏌涚€ｎ亷宸ラ柣锝囧厴閹垻鍠婃潏銊︽珨闂備礁鎲℃笟妤呭窗濞戙垹鐤剧憸鏂款潖婵犳艾纾兼繛鍡樺焾濡差噣姊虹憴鍕偞闁逞屽墯閸撴艾顭囬弽顐ｅ枑闁绘鐗嗙粭鎺楁煕閵娿儱鈧潡寮诲☉婊庢Ъ濡炪們鍔岄悧鍡涙偩閸偆鐟归柍褜鍓熷璇测槈閵忕姷鐤€闂傚倸鐗婄粙鎺楁倶閸繍娓婚柕鍫濋娴滄粓鏌熼搹顐€跨€殿喖顭烽弫鎰板川閸屾粌鏋涢柟鐓庣秺閺屽懎鈽夊鍨涙敽婵犵數濮烽弫鎼佸磻濞戙垺鍋嬪┑鐘插瀹曟煡鏌涢锝囩婵炲吋鐗滅槐鎾存媴鐠囷紕鍔烽梺鑽ゅ枎缂嶅﹪寮诲☉銏犖ㄩ柟瀛樼箓閺嗘洜绱掓潏鈺佹灈婵﹤顭峰畷鎺戭潩椤戣棄浜鹃柛婵勫劗閸嬫挸顫濋悡搴☆潾闂侀€炲苯澧柣顓у櫍瀹曪繝宕樺顔兼濡炪倖鍔戦崺鍕触鐎ｎ喗鐓曢柍鈺佸暟閹冲懘鏌熼搹顐ょ疄婵﹥妞介弻鍛存倷閼艰泛顏繝鈷€灞芥灍闁靛洤瀚伴、鏇㈠閵忋埄鍞虹紓鍌欐祰妞村摜鏁幒鏇犱航闂佽崵濮村ú銈呂熸繝鍥х劦妞ゆ帊鐒﹀畷灞炬叏婵犲啯銇濋柟绛圭節婵″爼宕ㄩ鍙ラ偗闂傚倷鑳舵灙妞ゆ垵鎳橀弫鍐敂閸曨厽娈鹃梺姹囧灮椤ｄ粙宕戦幘缁樻櫜閹肩补鈧剚娼剧紓浣哄亾瀹曟ê螞閸曨垱绠掗梻浣瑰缁诲倸煤閵娾晛绠洪柛宀€鍋為悡鏇㈡煏閸繃顥犻柟鍐叉喘閺岋紕浠﹂崜褉濮囩紓浣虹帛缁诲牆鐣烽幒妤€围闁搞儜鍕偓顖氣攽鎺抽崐妤佹叏閻戣棄纾婚柣鎰惈閸ㄥ倿鏌熺粙鍨劉闁告瑥绻橀弻宥堫檨闁告挾鍠栧濠氬即閵忕娀鍞跺┑鐘绘涧濞层倕鈻嶈椤啴濡堕崱妯洪瀺濠碉紕鍋犲Λ鍕綖韫囨稒鎯為柛锔诲幘閿涙粌鈹戦绛嬫當闁绘鎳愬▎銏ゆ偨閸涘ň鎷虹紓浣割儐椤戞瑩宕曡箛娑欏€垫慨妯煎帶楠炴绱掗纰卞剰妞ゆ挸鍚嬪鍕偓锝庝簴閸嬫捇鏌ㄧ€ｃ劋绨婚梺鐟版惈缁夊爼藝閿旈敮鍋撳▓鍨灈闁诲繑绻堥崺鐐哄箣閿旂粯鏅╃紓浣圭☉椤戝棝鎮剧捄琛℃斀闁宠棄妫楁禍婵囥亜閵娿儲顥㈢€殿喗鐓″畷濂稿即閻愭鍚嬫俊鐐€栭弻銊︽櫠娴犲鏅繝濠傚缁♀偓闂侀潧楠忕徊鍓ф兜閻愵兙浜滈柟瀛樼箖椤ャ垹鈹戦敓鐘虫锭闁宠鍨归埀顒婄秵閸嬪棝宕㈤幖浣瑰€甸柛蹇擃槸娴滈箖鏌ｆ惔顖滅У闁告挻鐩弫宥夋偄閸濄儳顔曢柣搴ｆ暩鏋い搴㈡尭閳规垿鍨鹃搹顐㈡灎濡ょ姷鍋涚换姗€骞冮埡鍐╁珰闁肩⒈鍓﹂崬浠嬫⒒娴ｅ憡璐￠柛搴涘€楅幑銏ゅ礃椤忓棛顦梺绯曞墲缁嬫帡鍩涢幒妤佺厱閻忕偛澧介幊鍕磼娴ｅ搫顣奸柕鍥у瀵挳鎮㈤崫鍕ㄦ嫲闁诲氦顫夊ú鎴﹀础閹剁晫宓佹俊顖氬悑鐎氭岸鏌熺紒妯虹闁逞屽墻閸欏啫顫忓ú顏呭殟闁靛鍠氭禍顏囨闂備緡鍓欑粔鏉懶ч弻銉︾叆闁哄洨鍋涢埀顒€缍婇幃陇绠涢幘顖涙杸闂佺粯枪鐏忔瑧绮婚悧鍫涗簻闁冲搫锕ゆ晶鎵磼鏉堛劌娴柛鈺嬬畵閸┾偓妞ゆ帒瀚壕濠氭煙閸撗呭笡闁抽攱甯掗湁闁挎繂鎳忛崯鐐烘煕閻斿搫浠︾紒缁樼〒閹风姾顦规俊缁㈠枤缁辨帡宕掑姣櫻囨煙椤旂晫鎳囨俊顐㈠暙閳藉娼忛…鎴烇紖闂傚倸鍊烽懗鍫曪綖鐎ｎ喖绀嬮柛顭戝亞閺嗭箓姊绘担鐟扳枙闁衡偓鏉堚晜鏆滈柨鐔哄Т閽冪喐绻涢幋鐐电叝婵炲矈浜弻娑㈠箻濡も偓鐎氼剙鈻嶅Ο璁崇箚闁绘劦浜滈埀顑懏濯奸柨婵嗘缁诲棝鏌涢妷銏℃珕妞ゎ偅娲熼弻鐔兼倷椤掆偓婢ь垶鏌涘顒夊剶闁哄本绋戦オ浼村礃閵婏附顔戦梻浣告啞閸戝綊宕戞繝鍥ц摕闁挎洍鍋撻摶锝夋煟閹惧啿顒㈤柣蹇撶墦濮婃椽宕崟顓犱紘闂佸摜濮甸悧鐘绘偘椤曗偓楠炲鏁冮埀顒傜不濞戙垺鈷掗柛顐ゅ枔閳洘銇勯弬鍨伀缂佽鲸鎹囧畷鎺戔枎閹烘繂鏁奸梻浣呵归鍡涘箲閸ヮ剙钃熸繛鎴炵煯濞岊亪鏌涢幘妞诲亾濠殿喖娲娲川婵犲嫭鍣х紓浣虹帛閸ㄥ墎绮氭潏銊х瘈闁搞儯鍔岄埀顒€鍢茶灋闁绘鐗忕粻鎾寸箾閸偄骞楃紒缁樼〒閳ь剚绋掗…鍥儗閹烘梻纾奸柣妯虹－婢у灚顨ラ悙鎻掓殭閾绘牠鏌嶈閸撶喖宕洪姀銈呯閻犲洦褰冮悗顓烆渻閵堝棙鈷掗柡鍜佸亯閵囨劙顢涢悙绮规嫽婵炶揪绲介幊娆掋亹閹烘繃鏅為梺鍛婄☉閻°劑寮插┑鍥ヤ簻闊洦鎸炬晶娑㈡煕鐎ｎ倖鎴﹀Φ閸曨垰妫橀柟绋垮瘨濞兼棃姊虹紒妯烩拻闁告鍛焼闁稿瞼鍋為悡鐔兼煙閹规劖鐝柟鐧哥悼閹叉悂鎮ч崼鐔峰攭濠殿喖锕︾划顖炲箯閸涙潙宸濆┑鐘插€瑰▓姗€姊绘担钘夊惞闁哥姴妫濆畷鏇熸媴閸愨晩妫ㄩ梻鍌欐祰婵倝鏁嬮梺鎸庣☉椤︻垶鍩ユ径鎰潊闁炽儱鍘栫花濠氭⒒娴ｅ懙鍦崲閹扮増鍎嶉柣鎴ｆ缁狀垳绱撴担璇＄劷缂佺娀绠栭弻鐔兼倷椤掍胶绋囬梺缁樺笒缂嶅﹪寮诲☉姘ｅ亾閿濆骸浜濋悘蹇斿缁辨帞绱掑Ο灏栧闂佺懓鍢查幊妯虹暦濮椻偓瀹曪絾寰勬繝搴⑿熼梻鍌欒兌鏋い鎴濇嚇椤㈡牗寰勯幇顒冩憰濠电偞鍨崹褰掓倿濞差亝鐓曢柟鏉垮缁辨壆绱掗妸鈺傛锭闁宠鍨块幃娆撳级閹寸姳妗撴俊鐐€ら崢濂稿磹閸噮鍤曟い鏇楀亾鐎规洜鍘ч埞鎴﹀炊閵夈倛妾稿┑鐘垫暩閸嬬娀骞撻鍡欑闁逞屽墯閵囧嫰顢曢姀鈺傂﹂梺瀹狀嚙缁夌懓鐣烽崡鐐╂婵炲棗鏈€氳棄鈹戦悙鑸靛涧缂佸弶瀵ч悘娆忣渻閵堝啫鍔氱紒缁橈耿瀵鈽夊锝呬壕闁挎繂绨肩花濂告煕閿濆懐绉洪柟顕嗙節婵″爼宕卞Ο鍝勪紟婵犵绱曢崑鐘活敋瑜庣粋宥嗐偅閸愨斁鎷虹紓渚囧灡濞叉ê鈻嶉崱娑欑厽闁冲搫锕ら悘锕傛煟濞戝崬鏋ら柍褜鍓ㄧ紞鍡涘窗濡ゅ懎鐤炬繝濠傜墛閻撱垺淇婇娆掝劅婵¤尙鍏橀弻娑㈠冀閵娧冣拰濠殿喖锕︾划顖炲箯閸涱垳椹抽悗锝庝簼閻ｄ即姊绘担鍛婂暈妞ゎ厼娲鏌ヮ敃閿曗偓閺嬩焦銇勯弴妤€浜惧Δ鐘靛仦鐢繝鐛鈧幊鐘活敆閸屾粍姣庡┑鐘垫暩婵炩偓婵炰匠鍥х９闁归棿绀佺粻鐔兼煥閻斿搫校闁稿鍊圭换娑㈠箣濞嗗繒浠鹃梺绋匡功閺佸寮婚妸銉㈡斀闁糕剝渚楅埀顒侇殔闇夋繝濠傛噹娴犻亶鏌″畝鈧崰鏍х暦濮椻偓楠炴捇骞掑┑鍛＞闂傚倷绀侀幖顐﹀嫉椤掆偓鐓ら柣鏃堫棑閺嗭箓鏌熼鐐蹭喊婵¤尪顕ч—鍐Χ閸涱喚顩伴梺鍛娒妶鎼佸春閻愬搫绠ｉ柨鏃傜帛閺呪晠姊洪懡銈呮灈妞わ綇闄勭粩鐔煎即閵忊檧鎷婚梺绋挎湰閻熝囧礉瀹ュ鍊电紒妤佺☉閹虫劙鎯屽▎鎾寸厵閻庣數枪琚ラ梺绋款儐閹告悂锝炲┑瀣亗閹兼番鍨昏ぐ搴ㄦ⒒娴ｇ瓔鍤冮柛鐘冲浮瀹曟粌鈹戦崱鈺佹闂佸憡娲﹂崜娑氱不妤ｅ啯鐓忛煫鍥э工婢у弶銇勯妷銉уⅵ闁哄瞼鍠栭、娆戞喆閸曨剛褰嬫繝纰樻閸嬪嫰宕锕€鐓″璺好￠弮鍫濈闁宠桨鑳堕ˇ鏉库攽閻樻鏆滅紒杈ㄦ礋瀹曟垿骞嬮敃鈧壕鍦喐韫囨洖鍨濆┑鐘宠壘缁犲鎮楅棃娑橆棌婵炲弶鎮傚娲礈閹绘帊绨煎┑鐐插级閻楃姴顕ｆ导鏉懳ㄩ柍鍝勫€婚崢鎾绘偡濠婂嫮鐭掔€规洘绮岄～婵堟崉娴ｆ洩绠撻弻娑㈠即閵娿儳浠╃紓浣哄У婵炲﹪寮婚悢鐓庣妞ゆ挾鍋熸禒鈺傜節濞堝灝鏋熺紒瀣尵濡叉劙骞樼€涙ê顎撻梺鍛婃尭瀵埖顨欓梻鍌欐祰濡椼劎绮堟笟鈧垾锕€鐣￠柇锕€娈ㄩ柣鐘叉处缁佹潙危閸喐鍙忔慨妤€鐗忛崚鏉棵瑰⿰鍫㈢暫闁哄被鍔岄埞鎴﹀幢濞嗗繐姣堢紓鍌欓檷閸斿海鍒掗鐐参﹂柛鏇ㄥ枤閻も偓闂佹寧绻傞幊搴ｇ不濞差亝鈷戦梻鍫熺⊕閹兼劙鎮楀鍗烆暭闁逛究鍔戦崺鈧い鎺戝閻撳啴鏌嶆潪鎵槮闁哄鍊栨穱濠囶敃閵忕姵娈婚梺璇″枟椤ㄥ牓骞夐幘顔肩妞ゆ帒鍋嗗Σ顒勬⒒娓氣偓濞佳囨偋韫囨梻浠氭俊鐐€栧ú鈺冪礊娓氣偓閻涱喖顫滈埀顒勩€佸▎鎾村癄濠㈣泛顦遍惄搴ㄦ⒒閸屾瑧顦﹂柟璇х節閹兘濡烽埞褍娲、娑㈡倻閸℃ɑ娅嗛柣鐔哥矌婢ф鏁Δ鍛厱闁圭儤鍤氳ぐ鎺撴櫜闁割偆鍣ュΛ鍕⒑閸涘﹥鐨戦柟铏崌閸╃偤骞嬮敂缁樻櫓闂佸吋绁撮弲娆戝垝閻㈢數纾藉ù锝嚽归埀顒€鎽滅划鏃堟偨閻㈢數鐒块梺鍦劋閺岋繝宕戦幘缁樻櫜閹煎瓨绻勯弫鏍ь渻閵堝繐鐦滈柛锝忕秮瀵鏁愰崼銏㈡澑闂佸搫娲ㄩ崑姗€宕Δ鍛仩婵﹩鍘鹃幊鍥煛瀹€瀣？闁逞屽墾缂嶅棙绂嶉悙瀛樻珡闂傚倷绀侀幖顐︽儔婵傜ǹ绐楅柡宥庡幘瀹撲胶鈧箍鍎遍ˇ浠嬪极婵犲嫮妫柟宄扮焸閸濈儤鎱ㄩ敐鍜佹█婵﹦绮幏鍛存惞閻熸壆顐奸梻浣告贡鏋い顓犲厴楠炲啴鎮滈挊澶屽幐闂佸憡渚楅崣鈧柟鑺ユ礀閳规垿鎮欓弶鎴犱户闂佹悶鍔屽﹢杈╁垝婵犳碍鏅查柛鈩冪懅椤旀洟姊虹化鏇炲⒉妞ゃ劌鐗忕划璇差潩閼哥數鍘遍柣搴秵閸嬪懎鐣峰畝鈧埀顒侇問閸犳洜鍒掑▎鎾扁偓渚€寮撮姀鈥充簻闂佺偓鑹鹃崐鎼佀夊顓犵瘈婵炲牆鐏濋弸鐔搞亜椤撶偞鍠樼€规洘鍨块獮姗€宕瑰☉妯瑰濠殿喗顭囬崑鎾垛偓姘炬嫹?s闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崰姘熼埀顒勬倵濞堝灝鏋熼柟顔煎€搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛瀹€瀣？濞寸媴濡囬崠鏍即閻愭澘顥氶梻浣哥秺椤ｏ妇绮堟笟鈧獮鍐箣閿旂晫鍘甸梺鍏肩ゴ閺呯偠妫㈤梻渚€鈧偛鑻晶顔剧棯缂併垹寮い銏＄懇楠炲洭鎮ч崼姘濠电偠鎻紞鈧い顐㈩樀婵＄敻鎮㈤崗鑲╁幈濠碘槅鍨板﹢閬嶆儗濞嗘挻鐓忛柛鈩冾殔閺嗭絿鈧娲栧畷顒勫煝鎼淬劌绠涙い鎺嗗亾缂併劊鍎茬换婵嬫偨闂堟稐娌梺鎼炲妼閻栧ジ寮崘顔肩厸闁告侗鍘鹃弻鍫ユ⒑閸涘﹣绶遍柛鐘虫尰缁傚秴饪伴崼鐔哄幐闂佸憡鍔戦崝宥夊箹閹扮増鐓曟慨姗嗗墰缁夋椽鏌＄仦鍓ф创妤犵偛顑呴埞鎴﹀醇閳惰￥鍔岄埞鎴︻敊绾兘绶村┑鐐叉嫅缁插潡宕氶幒鎴旀瀻闊洤锕ラ悗濠氭⒑鐠団€崇€婚柛鎰电厛濡﹪姊婚崒姘偓鐑芥嚄閸撲礁鍨濇い鏍亼閳ь剙鍟村畷濂告偄閾忚鍟庨梻渚€鈧偛鑻晶顖涖亜閵婏絽鍔﹂柟顔界懇楠炴捇骞掗幘鎼敳闂備礁澧界划顖炴偋閻樿钃熺€广儱鐗滃銊╂⒑閸涘﹥灏甸柛鐘崇墪閻ｅ嘲煤椤忓嫀銊╂煥閺囨浜剧紓浣稿閸嬫盯鍩為幋锔藉€烽梻鍫熺◥婢规洟姊虹拠鏌ヮ€楅柟纰卞亰閳ユ棃宕橀鍢壯囨煕閳╁厾顏堟儊閻戣姤鈷戠紒瀣硶閻忛亶鏌涚€ｎ偆鈯曠紒鍌涘浮楠炲洭顢樺┑鍫㈢暰闂備胶绮崝锔界濠婂牆鐒垫い鎺嶇閻忓鈧娲栫紞濠傜暦椤愶箑绀嬮幖娣灮濞插鈧娲橀敃銏′繆濮濆矈妲煎┑鐐茬墢閸嬫挾鎹㈠┑瀣棃婵炴垶菤閸嬫挾绱掑Ο闂寸泊闂傚倷绀侀幉锟犮€冮崱妯肩濠电姴娲ら拑鐔兼煥濠靛棭妲告俊顐ｏ耿閺屾盯鈥﹂幋婵囩亾婵°倗濮村ú顓㈠蓟閿濆棙鍎熼柕蹇ョ到椤ユ繈姊洪棃娑氬ⅱ閺嬵亝銇勯銏㈢閻撱倖銇勮箛鎾村櫣婵炲牄鍎靛娲濞戞艾顣洪梺鐟板级閻╊垶寮幇鏉胯Е闁靛牆娲﹂崵鈧Δ妤婁簷閸楁娊宕洪埀顒併亜閹烘垵鈧悂宕瑰┑鍥╃闁糕剝顨堢粻鎶芥煛閸☆參妾紒缁樼箞濡啫鈽夊▎妯伙骏闂備礁鎼Λ妤€螞濠靛钃熸繛鎴欏灩閸楁娊鏌曟繛鍨姍缂併劌顭峰娲箹閻愭祴鍋撻弽顓炲瀭闁秆勵殔閽冪喖鏌ｉ弬鍨倯闁搞倕鍟撮弻宥夊传閸曨偒鈧艾顭跨捄渚剮缂佽妫濋弻鐔虹磼濡櫣鐟查梺鎼炲€曞ú顓㈠蓟閻旂⒈鏁婇柛婵嗗娴煎牓鎮楃憴鍕鐎规洦鍓熼崺銉﹀緞婵炵偓鐎婚棅顐㈡搐濞寸兘寮幆褉鏀介柣妯虹仛閺嗏晠鏌涚€ｎ偆鈽夐摶鐐翠繆閵堝懎鏆炵€规洘鐓￠弻鐔稿閺夋垳鍠婇梺杞扮閸婂綊濡甸崟顖氱疀闁割偅娲橀宥夋⒑缂佹ɑ灏伴柣鐔叉櫊瀵鈽夐姀鐘电杸濡炪倖宸婚崑鎾剁磼閻欐瑥娲﹂悡鏇㈡煏閸繃鍣洪柛鐘筹耿閺岋紕浠︾化鏇炰壕鐎规洖娲﹀▓鏇㈡⒑闁偛鑻晶瀵糕偓瑙勬磻閸楁娊鐛崶顒€绀傛い鎰╁灪閸犳﹢鏌熼銊ユ搐闁卞洭鏌ｉ弬鍨Щ濠碘剝濞婂缁樻媴閸濄儲鐎俊鐐存綑閹芥粓寮鈧崺锟犲川椤撶媭妲舵繝娈垮枟鏋繛澶嬬⊕椤㈠﹪姊绘担鍛婂暈婵炶绠撳畷褰掓焼瀹ュ懐鏌у┑鐘诧工閻楀﹪鎮￠姀鈥茬箚妞ゆ牗鐟ㄩ鐔镐繆閸欏灏﹂柡宀嬬磿閳ь剨缍嗛崑鍡涘箠閸ャ劍鍙忓┑鐘叉噺椤忕娀鏌熸搴♀枅闁搞劑绠栭幖褰掝敃閵堝嫭鍠氭繝纰夌磿閸嬫垿宕愰幋锕€绀夌€光偓閸曨偆鐤囧┑顔姐仜閸嬫捇鏌℃担鐟板闁诡喗鐟ラ湁閻庯綆鍋呴弶鎼佹⒑鐠囨彃鍤辩紒韫矙閹ê顫濈捄铏规煣闂侀潧绻堥崐鏍煕閹寸姷纾奸悗锝庡亜椤曟粍绻濋埀顒勫箥椤斿墽锛滈梺闈浨归崐妤呮儗濞嗗繆鏀介柍鈺佸暞閸婃劙鏌涢埡瀣瘈鐎规洘甯掕灃濞达絽澹婃导鏍⒒閸屾艾鈧绮堟笟鈧獮澶愭晬閸曨剙搴婇梺绋挎湰婢规洟宕戦幘鎰佹僵闁绘挸娴锋禒鎼佹⒑閹稿海绠橀柛瀣ㄥ€濆顐﹀礃椤旇偐锛滃┑鐐村灦閼归箖鐛崼銉︹拻濞达絽鎲￠幆鍫ユ煕閻斿搫鈻堢€规洘鍨块獮妯肩磼濡厧骞堥梻渚€娼ф灙闁稿孩鐓″畷鎴﹀Ψ閳哄倻鍘藉┑鐐村灥瀹曨剙鈻嶅鍥ｅ亾鐟欏嫭绀冮柨鏇樺灲閵嗕礁鈻庨幋婵囩€抽柡澶婄墑閸斿海绮旈柆宥嗏拻闁稿本鐟х粣鏃€绻涙担鍐叉处閸嬪鏌涢埄鍐槈缂佺姷濞€楠炴牗娼忛崜褎鍋ч梺缁樼矌缁垳鎹㈠☉銏犵闁绘劕鐏氶崳顔剧磽娴ｅ弶顎嗛柛瀣崌濮婄粯鎷呴崷顓熻弴闂佹悶鍔忓Λ鍕€﹂崶顏嶆Щ闂佺儵妲呴崣鍐潖閾忚瀚氶柟缁樺俯閸斿姊洪崨濠傜伇妞ゎ偄顦辩划瀣吋婢跺鈧兘鏌涘▎蹇撲喊缂傚秳绶氶悰顕€宕堕鈧痪褔鏌涢…鎴濇灈缂佷緡鍣ｅ缁樼瑹閳ь剙顭囪閹广垽宕卞Δ濠勨偓鍓佹喐閺冨牏宓侀煫鍥ㄦ礈绾惧吋淇婇婵嗕汗妞ゆ梹娲熼弻鈩冨緞婵犲嫬顣烘繝鈷€鍌滅煓鐎规洘纰嶇€佃偐鈧稒顭囬崢鐢告⒑閸忛棿鑸柛搴㈠▕閹苯鐣濋埀顒傛閹烘鏁婇柤鎭掑労濡啴姊虹拠鈥虫灍缂侇喖鐭侀悘鎺楁⒒閸屾浜鹃梺褰掑亰閸犳艾鈻旈崸妤佲拻闁稿本鐟х粣鏃€绻涙担鍐叉瘽閵娾晛鐒垫い鎺戝閳锋帡鏌涢弴銊ヤ簻妞ゅ繆鏅犻弻锝夋晲閸涱厽些闂佷紮绲剧换鍫ュ春閳ь剚銇勯幒鎴濐仼闁绘帒鐏氶妵鍕箳閹存繃鐏撳┑鐐插悑閸旀牜鎹㈠☉銏犵煑濠㈣埖绋栭埀顒€娼￠弻鏇㈠幢濡も偓閺嗭絿鈧娲忛崝宥咁焽韫囨稑绀堢憸蹇涘煟閵堝棛绡€闁汇垽娼у瓭闂佹寧娲忛崐婵嬪箖瑜斿畷鍗炩槈濞嗗繆鍋撻崸妤佺厱妞ゎ厽鍨垫禍鏍瑰⿰鍕煉闁哄瞼鍠栧畷顐﹀礋椤掑顥ｅ┑鐐茬摠缁秹宕曢幎瑙ｂ偓鏃堝礃椤斿槈褔鏌涢埄鍐剧劷闁宠绋撶槐鎾诲磼濮樻瘷锛勭磼閼搁潧鍝哄┑鈥崇摠缁楃喖鍩€椤掆偓椤曪綁骞橀纰辨綂闂佹枼鏅涢崯顖炴偟閺嶎厽鈷掑〒姘ｅ亾闁逞屽墰閸嬫盯鎳熼娑欐珷闁规鍠氱壕鍏笺亜閺冨倵鎷￠柣鎾炽偢閺岀喓绮甸崷顓犵槇婵犵鈧磭鍩ｇ€规洖鐖奸崺锛勨偓锝庡墴濡嘲鈹戦悩鍨毄闁稿濞€楠炴捇顢旈崱妤冪瓘婵炲濮撮鎴犵礊閺嶎厽鐓涚€广儱楠稿楣冩煛娴ｇǹ鈧潡寮诲☉銏犲嵆婵☆垰鎼‖澶愭⒑鐠囪尙绠ｇ紒鑸佃壘椤繘鎼归崷顓狅紲濠碘槅鍨靛畷鐢稿矗閸℃稒鈷戦柛娑橈龚婢规ɑ銇勯幋婵愭█鐎规洖鎼埥澶愬閻樻鍚呴梻浣虹帛椤洭寮幖浣规櫖婵犲﹤鐗婇悡鐔煎箹閹碱厼鐏ｇ紒澶屾暬閺屾稓鈧綆浜滈顓熴亜閵徛ゅ妤楊亙鍗冲畷鐔碱敇閻樺灚顫岄梻鍌欑窔閳ь剛鍋涢懟顖涙櫠鐎涙﹩娈介柣鎰皺鏁堝銈冨灪閻熲晛鐣峰⿰鍡╂缂備礁宕鑸电┍婵犲洦鍊锋い蹇撳閸嬫捇寮介‖顒佺⊕缁楃喖鍩€椤掆偓閻ｇ兘骞嬮敃鈧粻娑欍亜閹捐泛鏋戦柣婵冨墲缁绘繄鍠婃径宀€锛熼梺绋款儐閸ㄥ灝鐣烽幇鏉跨闁挎洍鍋撻柛銊ュ€块弻娑樷攽閸℃寮稿┑鐐村灟閸ㄥ湱绮堥崘鈹夸簻闁哄啫娲ゆ禍褰掓煟閿濆棙銇濇慨濠冩そ濡啫鈽夊杈╂澖闂備胶枪閿曘儱煤濠婂懎鍨濋柛顐犲劚绾惧ジ鏌ｉ幇顒夊殶闁告ɑ鍎抽埞鎴︽倷鐎涙绋囧銈嗗灥濡鍩㈠澶婂窛妞ゆ洖鎳忛鏃堟⒑缂佹ê濮岄悘蹇旂懄閺呫儵姊绘担鍛婃儓闁活厼顦卞濠冦偅閸愩劎鐣鹃梺鍝勫暙閻楀﹪寮查弻銉︾厱闁斥晛鍠氬▓鏇㈡煟閿濆骸澧存慨濠呮閹风娀骞撻幒婵嗗Ψ闂備礁鎲￠崹鐢电礊婵犲倻鏆﹂柕蹇ョ磿闂勫嫮绱掗娑欑５闁稿鎹囬弫鍐磼濮橆剛鈧厼顪冮妶鍡樼厽闁稿孩濞婂畷浼村箛閺夎法顔嗛梺鍛婄⊕濞兼瑩宕￠搹顐犱簻闊洦鎸婚ˉ鐘垫偖閿曞倹鈷掑ù锝呮啞鐠愨剝銇勯幒鏇炵毢闁瑰箍鍨归埞鎴﹀炊閿斿墽鐣鹃梻浣稿閸嬪懎煤閺嶎偄顥氶柛蹇撳悑閸欏繑鎱ㄥΔ鈧Λ妤佺濠婂嫨浜滈柕澶涚畱閸濈儤鎱ㄦ繝鍐┿仢闁圭绻濇俊鍫曞川椤旈敮鍋撴ィ鍐┾拺闁煎鍊曢弸鍌炴煕鎼淬垹鈻曢柛鈹惧亾濡炪倖甯婄粈渚€宕甸鍕厱婵☆垰鎼埛鏃堟煕閹烘挸娴€规洖銈告俊鐑藉Ψ閳轰焦鏆┑鐘垫暩婵挳鎮樺璺虹婵°倕鎳庨懜褰掓煟閵忕姵鍟為柛瀣у墲缁绘繃绻濋崒姘闂佸憡鐟ョ€氫即寮婚弴銏犲耿婵°倐鍋撻柡鍡欏仧缁辨帞绱掑Ο鑲╃杽閻庤娲栭悥濂稿春閳╁啯濯撮柟顓熷笒娴滀即姊婚崒姘偓鎼併偑閹绢喖纾婚柛鏇ㄥ€嬪ú顏呮櫆闁告挆鍛幆闂備胶鎳撻悺銊ф崲瀹ュ鐤柛娑卞枤缁♀偓闂傚倸鐗婄粙鎺楀磼閵娿儍鐟扳堪閸曨厾鐓夐梺鍝勭灱閸犳牠骞冨▎鎿冩晢濠㈣泛顑囬弳銉╂⒒娴ｅ憡鎲搁柛锝冨劦瀹曞湱鎹勯搹瑙勬闂侀€涘嵆閸嬪﹪寮鍡樺弿婵鐗忕粙濠氭煛閸屾浜鹃梻鍌氬€烽懗鍓佸垝椤栫偞鏅濋柍鍝勬噹缁愭鏌″搴″箺闁稿﹤鐖煎鍫曞醇濞戞ê顬嬬紓浣哄У閸庢娊鈥︾捄銊﹀磯濞撴凹鍨伴崜鍐测攽閻愭潙绲婚柣掳鍔戞俊鐢稿礋椤栨凹娼婇梺鎸庣☉鐎氼剟鍩涘畝鍕€甸悷娆忓缁€鈧悗瑙勬处閸撶喖鍨鹃弮鍫濈妞ゅ繐妫涢敍婊冣攽閳藉棗鐏犻柣銊у厴瀹曟垿骞樼搾浣规そ椤㈡棃宕ㄩ鍛伖闂傚倸鍊搁崐鎼佹偋婵犲嫮鐭欓柟鎯у娑撳秹鏌ｉ幇顔煎妺闁绘挻娲栭埞鎴︽偐閹绘帗娈舵繝鈷€鍛毈闁哄本鐩崺锟犲磼濠婂嫬鍨辨俊銈囧Х閸嬫盯顢栨径鎰瀬闁稿瞼鍋為崑鈺呮倶閻愯埖顥夌紒鐘靛仜閳规垿鎮╅崹顐ｆ瘎婵犵數鍋愰崑鎾斥攽閻愭澘灏冮柛蹇曞亾缁嬫垼鐏冮梺鍛婄矆閻掞箓寮插⿰鍫熲拺闁告劕寮堕幆鍫ユ煙閸愯尙绠婚柟顕嗙節瀹曟﹢顢旈崨顓熺€炬繝鐢靛Т閿曘倝鎮ч崱娆戠焼闁割偆鍠撶粻楣冩煕閳╁叐鎴犱焊娴煎瓨鐓曢悗锝庝憾濡偓闂佸搫鐭夌紞渚€鐛鈧幖褰掝敃閿濆懘妫烽梻鍌欑閹碱偄螞鐎靛摜涓嶉柟鎹愵嚙閽冪喖鏌ｉ弬鍨倯闁稿顑夐弻娑㈩敃閵堝懏鐏侀梺鍛婂煀缁绘繂顫忓ú顏勫窛濠电姴鍊搁～鍛存⒑缁嬫鍎愰柟鍝ョ帛缁岃鲸绻濋崶銊モ偓閿嬨亜韫囨挸顏ら柛瀣崌瀵粙顢橀悙娈挎Х婵犲痉鏉库偓鏇㈠箠鎼达絿鐜绘俊銈呮噹缁犺绻涢敐搴″闁诲浚鍠楅妵鍕煛閸屾粌寮ㄩ梺鍝勭焿缂嶄線鐛崶顒侇棃婵炴垶锕╁浠嬫⒒娴ｅ憡鍟為柣鐔村劤閹广垽骞嬮敃鈧粻鐐烘煏婵炲灝鍓婚柡鍐ㄧ墕閻掑灚銇勯幒鍡椾壕闁捐崵鍋炴穱濠囧Χ閸曨厼濡藉┑鐐叉噽婵炩偓闁哄瞼鍠庨埢鎾诲垂椤旂晫浜剧紓浣瑰劤婢т粙宕㈣閳ワ妇鎹勯妸锕€纾繛鎾村嚬閸ㄤ即宕滈柆宥嗏拺闂傚牊绋掓径鍕磼鐠囪尙澧︾€殿喖顭烽崹鎯х暦閸ャ劍鐣烽梺璇插嚱缂嶅棝宕滃☉銏℃櫖闁绘柨鎽滅粻楣冩倵閻㈢櫥褰掝敁閹炬枼鏀介柍銉ㄦ珪閸犳ɑ顨ラ悙鑼闁诡喒鏅濋幏鐘绘嚑椤掑鏂€闂傚倷绀佸﹢閬嶅磿閵堝洦宕查柛鎰典簽缁€濠囨煕閳╁啰鈯曢柍閿嬪浮閺屾稓浠﹂崜褎鍣紓浣疯兌婢ф濡甸崟顖氬嵆闁绘柨鎼埛宀勬倵鐟欏嫭纾婚柛妤佸▕閻涱喖螣閸忕厧鐝伴梺鍦帛鐢偤鍩€椤掑寮慨濠冩そ閹崇偤濡烽崘鍙ラ偗閽樻繈姊洪鈧粔鏉懶ч弻銉︾叆闁哄洨鍋涢埀顒€缍婇幃陇绠涢幘顖涙杸闂佺粯枪鐏忔瑧绮婚悧鍫涗簻闁冲搫锕ゆ晶鎵磼鏉堛劌娴柛鈺嬬畵閸┾偓妞ゆ帒瀚壕濠氭煙閸撗呭笡闁抽攱甯掗湁闁挎繂鎳忛崯鐐烘煕閻斿搫浠︾紒缁樼〒閹风姾顦规俊缁㈠枤缁辨帡宕掑姣櫻囨煙椤旂晫鎳囨俊顐㈠暙閳藉娼忛…鎴烇紖闂傚倸鍊烽懗鍫曪綖鐎ｎ喖绀嬮柛顭戝亞閺嗭箓姊绘担鐟扳枙闁衡偓鏉堚晜鏆滈柨鐔哄Т閽冪喐绻涢幋鐐电叝婵炲矈浜弻娑㈠箻濡も偓鐎氼剙鈻嶅▎鎾粹拻濞达絽鎳欓崷顓熷床闁规壆澧楅崕妤呮煕瀹€鈧崑娑㈡嫅閻斿吋鐓熼柡鍌氱仢閹垿鏌熼婊冧沪闁靛洤瀚伴獮鍥礈娴ｇǹ鐓傜紓浣稿⒔閸嬫挻绻涙繝鍥х畺婵°倕鍟崰鍡涙煕閺囥劌澧版い锔诲弮濮婄儤娼悧鍫€偘濡炪倖娉﹂崶鑸垫櫔闂佹寧绻傚Λ娑€€呴悜鑺ュ€甸柨婵嗘噽娴犳盯鏌￠崪鍐М婵﹨娅ｇ槐鎺懳熼崫鍕垫綌婵＄偑鍊栧褰掓偋閻樿尙鏆︽い鎺戝閸嬨劑鏌涘☉妯戝牓骞忕紒妯肩閺夊牆澧界粔顒€鈹戦鍝勨偓妤€鈽夐悽绋块唶闁哄洨鍠撻崢閬嶆⒑閹稿海绠撶紒缁樺浮閹箖宕归顐ｎ啍闂佺粯鍔樼亸娆戠不婵犳碍鐓涘ù锝堫潐瀹曞矂鏌℃担瑙勫磳闁轰焦鎹囬弫鎾绘晸閿燂拷
        fprintf(yyout, "%*cFLOATLiteral\tvalue: %s\ttype: %s\n", level, ' ',
            value.c_str(), type.c_str());
    }
}

void Id::output(int level)
{
    /*std::string name, type;
    int scope;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    scope = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getScope();
    fprintf(yyout, "%*cId\tname: %s\tscope: %d\ttype: %s\n", level, ' ',
            name.c_str(), scope, type.c_str());*/
    
    std::string name, type;
    int scope;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    scope = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getScope();
    if (ERROR_MESSAGE_WRITE_INTO_AST)
    {
        if (define_state == NOT_DEFINED)
        {
            fprintf(yyout, "%*c\twarning,identifier %s is not defined\n", level, ' ',
                name.c_str());
        }
        else if (define_state == REDEFINATION)
        {
            fprintf(yyout, "%*c\twarning,identifier %s is redefined\n", level, ' ',
                name.c_str());
        }
    }
    fprintf(yyout, "%*cId\tname: %s\tscope: %d\ttype: %s\n", level, ' ',
        name.c_str(), scope, type.c_str());
    if (id_type == INT_ARRAY)
    {
        Dimension->output(level + 4);
        if (Init != nullptr)
        {
            Init->output(level + 4, 0, dim_record);
        }
    }
}

void CompoundStmt::output(int level)
{
    fprintf(yyout, "%*cCompoundStmt\n", level, ' ');
    stmt->output(level + 4);
}

void SeqNode::output(int level)
{
    stmt1->output(level);
    stmt2->output(level);
}

void DeclStmt::output(int level)
{
    fprintf(yyout, "%*cDeclStmt\n", level, ' ');
    id->output(level + 4);
}


void IfStmt::output(int level)
{
    fprintf(yyout, "%*cIfStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
}

void IfElseStmt::output(int level)
{
    fprintf(yyout, "%*cIfElseStmt\n", level, ' ');
    cond->output(level + 4);
 /* thenStmt->output(level + 4);
    elseStmt->output(level + 4);*/

    if (thenStmt != nullptr)
    {
        thenStmt->output(level + 4);
    }
    if (elseStmt != nullptr)
    {
        elseStmt->output(level + 4);
    }
}

void ReturnStmt::output(int level)
{
    fprintf(yyout, "%*cReturnStmt\n", level, ' ');
    if(retValue!=nullptr)
        retValue->output(level + 4);
}

void AssignStmt::output(int level)
{
    typeCheck();
    fprintf(yyout, "%*cAssignStmt\n", level, ' ');
    lval->output(level + 4);

    //std::cout << expr->cal_expr_val() << std::endl;
    expr->output(level + 4);
}

void FunctionDef::output(int level)
{
    //std::string name, type;
    //name = se->toStr();
    //type = se->getType()->toStr();
    //fprintf(yyout, "%*cFunctionDefine function name: %s, type: %s\n", level, ' ', 
    //        name.c_str(), type.c_str());
    //stmt->output(level + 4);

    std::string name, type;
    if (se == nullptr)
    {
        fprintf(stderr, "Oops!闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崰姘熼埀顒勬倵濞堝灝鏋熼柟顔煎€搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛鐏炵硶鍋撳畷鍥ㄦ畷闂侀€炲苯澧寸€规洑鍗冲浠嬵敇閻愮數鏆柣鐔哥矋閺屻劑鎮鹃柨瀣檮闁告稑锕ゆ禍婊堟⒑閸涘﹦绠撻悗姘煎墮閳绘捇濡烽埡鍌楁嫽闂佺ǹ鏈悷褔藝閿曞倹鐓欓悹鍥囧懐鐦堥悗瑙勬礀濠€閬嶅箟閹绢喖绀嬫い鎺嗗亾鐎殿喖娼″娲捶椤撯剝顎楅梺鍝ュУ閻楃娀骞冮垾鎰佹建闁逞屽墴瀵鎮㈤崨濠勭Ф婵°倧绲介崯顖烆敁瀹ュ鈷戠紒瀣儥閸庢劙鏌涢弮鈧悷鈺呯嵁閸愩剮鏃堝川椤撶喎绁舵俊鐐€栭幐楣冨磹椤愶箑顫呴幒铏濠婂牊鐓忓鑸电☉椤╊剛绱掗悩鎰佺劷缂佽鲸甯楀蹇涘Ω閿曗偓闂夊秹姊洪悷鏉挎Щ闁硅櫕锚閻ｇ兘顢曢敃鈧粈瀣棯閻楀煫顏呯閸撗€鍋撻崗澶婁壕闂佸憡娲﹂崜娑㈠储闁秵鈷戦梻鍫熻儐瑜版帒纾块柟鎯版閺勩儵鏌ㄥ┑鍡樼闁稿鎸搁埢鎾诲垂椤旂晫浜俊鐐€ら崢楣冨礂濡警鍤曟い鎰剁畱缁犳稒銇勯幘璺轰户缂佹劗鍋炵换婵嬫偨闂堟刀銏ゆ倵濮樺崬鍘寸€规洏鍎靛畷銊р偓娑櫱氶幏缁樼箾鏉堝墽鎮奸柟铏崌椤㈡艾饪伴崨顖滐紲闁荤姴娲﹁ぐ鍐焵椤掆偓濞硷繝鎮伴钘夌窞濠电偟鍋撻～宥夋⒑闂堟稓绠冲┑顔惧厴椤㈡瑩骞掗弮鍌滐紳闂佺ǹ鏈悷褔宕濆鍡愪簻妞ゆ挾鍋為崰妯尖偓瑙勬磸閸ㄤ粙鐛弽銊﹀闁稿繐顦扮€氬ジ姊绘担鍛婂暈缂佸鍨块弫鍐晲閸ヮ煈鍋ㄩ梻渚囧墮缁夌敻鎮￠弴銏＄厽婵☆垵顕ф晶顖涚箾閸喓鐭岄柍褜鍓濋～澶娒哄鈧畷婵嗏枎閹捐泛绁﹂梺鎼炲労閸撴岸宕戠€ｎ喗鐓曟い鎰Т閻忊晜顨ラ悙鏉戝闁哄矉绲鹃幆鏃堝灳閸愯尪绶熼梻浣呵归鍡涘箲閸ヮ剙绠栨俊銈呮噺閺呮煡骞栫划鍏夊亾閹颁焦楠勯梻鍌欐缁鳖喚绮婚幋锔藉亱闁规崘顕х粻鏍ㄤ繆閵堝懎鏆為柛鐘叉閺屾盯寮撮妸銉ヮ潾濡炪倧璐熼崝鎴﹀蓟閿濆顫呴柍杞拌兌娴狀參鏌ｉ姀鈺佺仯闁哥姵鐗犻獮鍐潨閳ь剟鐛幒鎳虫梹鎷呴崫鍕闂傚倷鑳剁划顖炲礉閺囥埄鏁嬫い鎾卞灩閻撴繈鏌熼崜褏甯涢柣鎾冲暟閹茬ǹ饪伴崼婵堫槶濠电偛妫欓幐鎼佸磼閵娾晜鐓涚€广儱鍟俊鍧楁煟濠靛棛鍩ｉ柡宀嬬節瀹曞爼濡烽妷褌鐥梻浣瑰▕閺€閬嶅垂閸ф钃熸繛鎴炃氶弨浠嬫煕閳╁喚娈㈠ù灏栧亾闂傚倷绀侀悿鍥敋瑜旇棟闁汇垻枪閽冪喐绻涢幋鐐茬劰闁稿鎹囬弫宥夊础閻愬弶娈ㄦ繝銏ｎ潐濞茬喎顫忔繝姘＜婵炲棙甯掗崢锟犳煛鐎ｅ吀绨奸柕鍥у楠炲鈹戦崶鑸碉紗婵°倗濮烽崑鐐哄礉濞嗘挾宓侀柡宥冨妿閻熺懓鈹戦悩鎻掝伀闁崇鍊栫换婵嬫偨闂堟稈鏋呭┑鐐板尃閸ヨ埖鏅為梺鍦濠㈡﹢寮告笟鈧弻娑㈠焺閸愵亖濮囬梺缁樻尰濞茬喖骞冨鈧幃娆撳箵閹哄棙瀵栭梻浣芥〃閻掞箓鎮ч悩璇茶摕闁哄洨鍠撶粻楣冩煟閹伴潧澧柣婵囨礋濮婃椽骞庨懞銉︽殸闂佹悶鍔屽鈥愁嚕鐠囨祴妲堟俊顖炴敱閺傗偓闂備胶纭堕崜婵嬨€冭箛鏂款嚤闁稿本鍑瑰〒濠氭煏閸繈顎楀ù婊冨⒔缁辨帡鎮╅搹顐㈢３閻庢鍠涢褔鍩ユ径鎰潊闁绘鏁稿澶愭⒒娓氣偓閳ь剛鍋涢懟顖涙櫠鐎电硶鍋撶憴鍕缂傚秴锕ら悾椋庣矙鐠囩偓妫冨畷姗€濡歌閸嬪﹪姊婚崒娆戭槮闁硅绻濆畷婵嬫晝閸屾氨鐛ラ梺鍝勭▉閸欏繒绱為弽顓熺厱婵炴垶顭囬幗鐘绘煟閹惧磭绠婚柡灞剧洴椤㈡洟鏁愰崶鈺冩毇闂備線娼荤徊钘夛耿鏉堚晜顫曢柟鐑橆殔缁犳稒銇勯幘璺盒涘┑顔兼川缁辨挻鎷呴搹鐟扮缂備浇顕ч悧鎾荤嵁閸愨晝顩烽悗锝庡亽濡啫鈹戦悙娈挎缂傚牅鍗冲畷瑙勭鐎ｎ亞鐣哄┑掳鍊愰崑鎾淬亜椤愶絿绠炴慨濠呭吹閳ь剟娼ч幉锟犲疾閸忚偐绡€缁剧増菤閸嬫捇宕橀懠顒勭崜闂備礁鎲″褰掓偡閵夆晩鏁嬮柨婵嗩槸缁犵粯銇勯弮鍌楁嫛婵炵厧锕娲箮閼恒儲娈伴梺绋款儐閹瑰洭寮婚悢鍓叉Ч閹艰揪绲界粭锛勭磽娴ｈ櫣甯涚紒瀣笒椤洩绠涘☉妯碱槶閻熸粌绉瑰畷鍝勭暆閸曨兘鎷虹紒缁㈠幖閹冲氦顣跨紓鍌欑贰閸犳牕霉閻戣棄绀嗛柟鐑橆殔缁秹鏌涢銈呮瀻闁逞屽墰缁垱绌辨繝鍥舵晬闁绘劕鐡ㄩ悵宕囩磽娴ｉ潧濡兼い顓炲槻椤繐煤椤忓拋妫冨┑鐐村灱妞存悂顢撻崱娑欌拻闁稿本鑹鹃鈺呮倵濮樼厧澧撮柟顔藉劤閻ｏ繝骞嶉鑺ョ暦闂備線鈧偛鑻晶鎾煕閳规儳浜炬俊鐐€栫敮鎺楀窗濮橆剦鐒介柟閭﹀幘缁犻箖鏌涘▎蹇ｆ闁兼澘娼￠弻鏇㈠幢濡も偓閺嗭絿鈧娲忛崝宥咁焽韫囨稑绀堢憸蹇涘煟閵堝棛绡€闁汇垽娼у瓭闂佹寧娲忛崐婵嬪箖瑜斿畷鍗炩槈濞嗗繆鍋撻崸妤佺厱妞ゎ厽鍨垫禍鏍瑰⿰鍕煉闁哄瞼鍠栧畷顐﹀礋椤掑顥ｅ┑鐐茬摠缁秹宕曢幎瑙ｂ偓鏃堝礃椤斿槈褔鏌涢埄鍐剧劷闁宠绋撶槐鎾诲磼濮樻瘷锛勭磼閼搁潧鍝哄┑鈥崇摠缁楃喖鍩€椤掆偓椤曪綁骞橀纰辨綂闂佹枼鏅涢崯顖炴偟閺嶎厽鈷掑〒姘ｅ亾闁逞屽墰閸嬫盯鎳熼娑欐珷闁规鍠氱壕鍏笺亜閺傚灝鈷旈悽顖涚〒缁辨帞绱掑Ο鑲╃暤濡炪値鍋呯换鍫ャ€佸鈧幃娆撳级閸噮鍤欐繝鐢靛Т閻ュ寮舵惔鎾充壕鐟滄柨顫忔禒瀣妞ゆ挻绋戞禍鍓х磼濡や胶鈽夋繛鏉戝€垮顐﹀磼閻愬鍘藉┑鈽嗗灠閸氬寮抽埡鍐／闁诡垎鍐╁€紓浣虹帛閻╊垶鐛€ｎ喗鍊婚柛鈩冪懃婵櫣绱撻崒娆掑厡濠殿噣顥撻崚鎺楊敍閻愯尙鐤勯梺闈涒康婵″洨寮ч埀顒勬⒑閸涘﹤濮€闁哄倸鍊圭粋鎺楀閵堝棗鈧敻鎮峰▎蹇擃仾缂佲偓閸儲鐓曢煫鍥ㄦ閼板潡鏌曢崱鏇狀槮閾伙綁鎮跺☉鎺戝⒉闁轰胶濞€濮婅櫣鎷犻垾宕団偓濠氭煕鐏炴儳鍤俊顐ｇ矒濮婄粯鎷呯粵瀣闂佸憡鍨归弲顐ゆ閻愬搫骞㈡繛鎴烆焽閻涖儵姊洪崫鍕枆闁稿妫欑粙澶婎吋閸℃劒绨婚梺鍝勭▉閸嬪嫭绂掗敃鍌涚厽闁规儳宕崝锕傛煛瀹€瀣М鐎殿喗鎸抽幃娆徝圭€ｎ亙澹曢梺闈╁瘜閸樻悂宕戦幘鎰佹僵闁绘劦鍓欓锟�");//闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崰姘熼埀顒勬倵濞堝灝鏋熼柟顔煎€搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛鐏炵硶鍋撳畷鍥ㄦ畷闂侀€炲苯澧寸€规洑鍗冲浠嬵敇閻愮數鏆柣鐔哥矋閺屻劑鎮鹃柨瀣檮闁告稑锕ゆ禍婊堟⒑閸涘﹦绠撻悗姘煎墮閳绘捇濡烽埡鍌楁嫽闂佺ǹ鏈悷褔藝閿曞倹鐓欓悹鍥囧懐鐦堥悗瑙勬礀濠€閬嶅箟閹绢喖绀嬫い鎺嗗亾鐎殿喖娼″娲捶椤撯剝顎楅梺鍝ュУ閻楃娀骞冮垾鎰佹建闁逞屽墴瀵鎮㈤崨濠勭Ф婵°倧绲介崯顖烆敁瀹ュ鈷戠紒瀣儥閸庢劙鏌涢弮鈧悷鈺呯嵁閸愩剮鏃堝川椤撶喎绁舵俊鐐€栭幐楣冨磹椤愶箑顫呴幒铏濠婂牊鐓忓鑸电☉椤╊剛绱掗悩鎰佺劷缂佽鲸甯楀蹇涘Ω閿曗偓闂夊秹姊洪悷鏉挎Щ闁硅櫕锚閻ｇ兘顢曢敃鈧粈瀣棯閻楀煫顏呯閸撗€鍋撻崗澶婁壕闂佸憡娲﹂崜娑㈠储闁秵鈷戦梻鍫熺▓鎼撮绱掗悩鍐茬伌闁糕斂鍨藉鎾閿涘嫬骞堥梻浣哥枃椤宕曢搹顐ゎ洸闁绘劦鍏涚换鍡涙煟閹板吀绨婚柍褜鍓氶悧婊堝极椤曗偓楠炴帒螖閳ь剛澹曢崷顓熷枑闁绘鐗嗙粭鎺楁煕濮橆剛绉洪柡灞界Х椤т線鏌涢幘璺烘灈鐎殿喖顭烽弫鎰板幢濡搫濡抽梻渚€娼х换鎺撴叏閺夋嚩鎺楀醇閵夛腹鎷洪梺鍛婄☉閳洟顢旈崼婢囨煙闂傚顦﹂柣蹇斿▕閺屻劑寮撮悙娴嬪亾閹间礁鍨傞柛灞剧◤娴滄粓鏌￠崒婵愬殭闁告柣鍊濋弻娑㈡偐瀹曞洤鈷屽Δ鐘靛仜閿曨亜顕ｆ禒瀣垫晣闁绘劙娼ч獮鎰版⒒娴ｅ憡鍟為柛鏃撻檮缁傚秴饪伴崟顒€寮块梺缁樻尭缁ㄥ爼寮ㄦ禒瀣叆婵炴垶锚椤忊晛霉濠婂啨鍋㈤柡灞剧⊕缁绘繈宕橀埡鍐炬Ф闂備胶鎳撶粻宥夊垂娴犲宓侀柛銉墯閸嬪鏌涢锝囩畱缂傚啯娲栭埞鎴︽偐閸偅姣勬繝娈垮枟濞兼瑧鍙呴梺闈涚墕椤︿即宕戦崒鐐寸厸闁搞儯鍎遍悘顏堟煟閹惧鎳囩€殿喖鐖煎畷濂稿Ψ瑜忛弳顐⑩攽閿涘嫯妾搁柛锝忕秮瀵鈽夊锝呬壕闁挎繂鎳庨。宕囩磼婢舵ê娅嶉柡灞糕偓宕囨殕闁逞屽墴瀹曚即寮介婧惧亾娴ｅ壊娼ㄩ柍褜鍓熼獮鍐閳藉棙效闁硅壈鎻徊楣冨闯椤栨稓绡€闁汇垽娼у暩濡炪倧绲肩划娆撳春閳ь剚銇勯幒鎴濃偓鍛婄濞差亝鐓曢悗锝冨妼閻忔煡鏌＄仦绯曞亾瀹曞洦娈曢柣搴秵閸撴盯鏁嶉悢鍏尖拺闂侇偆鍋涢懟顖涙櫠閸撗呯＝鐎广儱鎳忛ˉ銏⑩偓瑙勬礃閸ㄥ灝鐣烽悢纰辨晬婵炴垶菤閸嬫捇宕奸弴鐔哄幍缂傚倷闄嶉崹褰掑几閻斿吋鐓熼柟鎯ь嚟閵嗘帡鏌嶈閸撴瑩宕㈠⿰鍫濈；闁瑰墽绮崐鐢告煥濠靛棝顎楀褌鍗抽弻銊モ槈濮橆剚鐏堝┑顔硷攻濡炰粙寮婚崨瀛樺€烽柤鑹版硾椤忣厽绻濋埛鈧崘銊㈡寖缂備浇椴搁幐鎼侇敇婵傜ǹ宸濇い鏇炴噺閿涗胶绱撻崒娆戭槮闁稿﹤鎽滅划鏃堟偡閹殿喗娈炬繝闈涘€绘灙闁告濞婇弻锝夊籍閸ャ劏鍚傜紓浣靛妿閺佽顫忕紒妯诲闁告盯娼у﹢閬嶅箲閵忋倕绠虫俊銈傚亾闁绘挻娲熼弻娑㈩敃閿濆洤顩梺鍝勵儐閺屻劑婀侀梺缁樏Ο濠囧磿閹扮増鐓熼柟鎹愭硾閺嬫盯鏌″畝鈧崰鏍蓟閸ヮ剚鏅濋柍褜鍓熼幃姗€鎮╃紒妯煎幗闂侀潧枪閸斿秹顢旈悩鐢电＜閺夊牄鍔屽ù顕€鏌熼瑙勬珔妞ゆ柨绻橀、娆撳箚瑜庨崐顖炴⒒閸屾瑧顦﹂柟纰卞亰閹绺界粙璺ㄧ崶闂佸搫绋侀崢褰掑焵椤戣法顦﹂柍璇查叄楠炴﹢寮堕幋婊勫亝闂佽楠搁崢婊堝磻閹剧粯鐓冪憸婊堝礈閻旂厧绠栭柍鈺佸暙缁剁偤鏌熼柇锕€骞愰柟閿嬫そ濮婃椽鎳￠妶鍛€炬繝銏㈡嚀濡繈寮鍜佸悑濠㈣泛顑囬崢閬嶆煟鎼搭垳绁烽柛鏂跨焸閸┾偓妞ゆ帊鑳舵晶顒傜磼瀹€鍐摵缂佺粯绻堝畷鍫曞Ω閵夈垹浜惧┑鐘崇閻撶娀鏌熼鐔风瑨闁告梹绮嶇换娑㈠川椤撶噥妫ょ紓浣介哺閹稿骞忛崨鏉戞嵍妞ゆ挻绋掗～宥嗕繆閻愵亜鈧牠宕归悽纰樺亾濮橆偄宓嗛柣娑卞枤閳ь剨缍嗛崰妤呭磹婵犳碍鐓犻柟顓熷笒閸旀鏌熷畡閭︾吋婵﹨娅ｇ划娆撳箰鎼淬垺瀚抽梻浣哄帶缂嶅﹦绮婚弽顓炴槬闁逞屽墯閵囧嫰骞掗幋婵愪紝濠碘槅鍋呴崹鍦閹烘鏁婇柤娴嬫櫅椤も偓缂傚倷鐒﹂〃鍫ュ窗閺嶎厼鏄ラ柍褜鍓氶妵鍕箳閹存繍浼€閻庤鎸风欢姘跺蓟閳ユ剚鍚嬮幖绮光偓鍐差劀闂備浇妗ㄧ粈渚€宕幘顔艰摕闁靛ň鏅涢崡铏繆閵堝倸浜炬繛瀛樼矊婢х晫妲愰幒妤佸€锋い鎺嗗亾闁告柣鍊楃槐鎾愁吋閸滃啳鍚悗娈垮枦椤曆囧煡婢舵劕顫呴柍鍝勫€瑰▍鍥⒒娴ｇ懓顕滅紒璇插€歌灋婵炴垟鎳為崶顒€唯鐟滃繒澹曟總鍛婄厽闁逛即娼ф晶顕€鏌涢弬璇测偓妤冩閹炬剚鍚嬮柛婊冨暢閸氼偊鎮楀▓鍨灈妞ゎ厾鍏樺畷瑙勩偅閸愩劎鐤€婵炶揪绲介幉锟犲磹椤栫偞鈷戠痪顓炴噹娴滃綊鎮跺☉鏍у姦闁糕斁鍋撳銈嗗笒閸燁偊鎯冨ú顏呯厽闁哄稁鍘洪幉鐐叏婵犲嫮甯涢柟宄版嚇瀹曘劑妫冨☉姘毙ㄥ銈冨灪閻楃姴鐣烽崡鐐╂婵☆垳鈷堥崬鍫曟⒒娴ｅ摜绉烘俊顐ユ硶濞嗐垽濡堕崶鈺冾槸闂佸搫绉查崝搴ｅ姬閳ь剟姊婚崒姘卞濞撴碍顨婂畷鏇炩槈閵忥紕鍘告繛杈剧到閹诧繝藟閵忋倖鐓涢悘鐐插⒔閵嗘帒霉閻欏懐鐣电€规洘绮忛ˇ鎶芥煛閸涱厹鍋㈡慨濠冩そ濡啫霉閵夈儳澧︾€殿喗褰冮オ浼村醇濠靛牆骞堥梻浣侯攰閹活亪姊介崟顖涘亗闁哄洨鍠撶弧鈧梻鍌氱墛缁嬫帡藟濠婂嫨浜滈煫鍥风到婢ь垶鏌曢崶褍顏い銏℃礋椤㈡宕掑⿰鍕啌闂傚倷绀侀幖顐﹀嫉椤掑嫭鍎庢い鏍ㄥ嚬閸ゆ洘銇勯弴妤€浜鹃梺绯曟杹閸嬫挸顪冮妶鍡楃瑨閻庢凹鍓熷畷褰掑磼閻愬鍘遍悷婊冮叄閵嗗啴宕煎┑鍫熸婵炴挻鍩冮崑鎾绘煛鐏炲墽娲存鐐叉喘濡啫鈽夊▎鎴滈偗濠碉紕鍋戦崐銈夊磻閸曨垰绠犳慨妞诲亾鐎殿喖顭峰鎾閻樿鏁规繝鐢靛█濞佳兠洪妶鍛瀺闁挎繂娲ㄧ壕钘壝归敐鍥ㄥ殌濠殿喖鐗忕槐鎺斺偓锝庡亜缁椦囨煙楠炲灝鐏╅柍瑙勫灩閳ь剨缍嗛崑鍕濞差亝鈷掗柛灞炬皑婢ф盯鏌涢幒鍡椾壕闂備線娼х换鍫ュ磹閺嶎厼纾归柛顭戝亝閸欏繑鎱ㄥ璇蹭壕濠碘槅鍋夊▔鏇㈡嚍闁秵鍤嶉柕澶堝€楃粻姘舵⒑閸涘﹦鎳冩い锔诲灡閹便劌顓奸崶锝呬壕婵炲牆鐏濋弸鐔兼煙濮濆本鐝柟渚垮姂閸┾偓妞ゆ帒瀚悡鍐⒑濞嗘儳鐏犲ù婊堢畺濮婇缚銇愰幒婵囶棖缂備緡鍣崹鎶藉箲閵忕姭妲堟繛鍡樺姉缁夊爼姊洪崨濠冨瘷闁告劑鍔庨崢鎺楁⒒閸屾瑨鍏屾い銏狅攻閹便劑濡堕崶褍鐏婇柟鑹版彧缁蹭粙宕瑰┑瀣厸闁告劑鍔庢晶娑㈡煕婵犲嫮甯涘ǎ鍥э躬椤㈡盯鎮欑€涙褰哥紓鍌欓檷閸斿矂鈥﹀畡閭︽綎婵炲樊浜濋ˉ鍫熺箾閹达綁鍝烘い搴℃濮婂宕掑顒変患闁诲孩鍑归崜鐔煎箖閹呮殝闁规鍠楀▓鏇㈡⒑闁偛鑻晶鎾煕閳规儳浜炬俊鐐€栫敮鎺楀窗濮橆剦鐒介柟閭﹀幘缁犻箖鏌涘▎蹇ｆ闁兼澘娼￠弻鏇㈠幢濡も偓閺嗭絿鈧娲忛崝宥咁焽韫囨稑绀堢憸蹇涘煟閵堝棛绡€闁汇垽娼у瓭闂佹寧娲忛崐婵嬪箖瑜斿畷鍗炩槈濞嗗繆鍋撻崸妤佺厱妞ゎ厽鍨垫禍鏍瑰⿰鍕煉闁哄瞼鍠栧畷顐﹀礋椤掑顥ｅ┑鐐茬摠缁秹宕曢幎瑙ｂ偓鏃堝礃椤斿槈褔鏌涢埄鍐剧劷闁宠绋撶槐鎾诲磼濮樻瘷锛勭磼閼搁潧鍝哄┑鈥崇摠缁楃喖鍩€椤掆偓椤曪綁骞橀纰辨綂闂佹枼鏅涢崯顖炴偟閺嶎厽鈷掑〒姘ｅ亾闁逞屽墰閸嬫盯鎳熼娑欐珷闁规鍠氱壕鍏笺亜閺冨倵鎷￠柣鎾炽偢閺岀喓绮甸崷顓犵槇婵犵鈧磭鍩ｇ€规洖鐖奸崺锛勨偓锝庡墴濡嘲鈹戦悩鍨毄闁稿濞€楠炴捇顢旈崱妤冪瓘婵炲濮撮鎴犵礊閺嶎厽鐓涚€广儱楠稿楣冩煛娴ｇǹ鈧潡寮诲☉銏╂晝闁挎繂娲ㄩ悾鐓庮渻閵堝繐鐦滈柛娆忓暙椤繐煤椤忓懎娈ラ梺闈涚墕閹冲繘鎮橀崼銉︹拺缂佸顑欓崕蹇涙煥閺囨ê鈧繃淇婇幘顔肩闁规儳澧庨惁鍫濃攽椤旀枻渚涢柛姗€绠栧畷銏ゎ敂閸啿鎷洪梺鍛婃尰瑜板啯绂嶅┑鍫㈢＜閻犲洦褰冮弳锝夋寠閻斿摜绡€濠电姴鍊搁弳濠囨煛閳ь剟鎳為妷锝勭盎闂佸搫鍊介褎淇婃總鍛婄厵闁惧浚鍋勬慨澶婎熆鐟欏嫭绀冪紒铏规櫕缁瑧鎹勯妸鈺冨礈闂傚倷绀佹竟濠囧磻閸涱垳鐭欓柟瀵稿仒缁诲棝鏌ｉ敐鍛伇缁炬儳銈搁弻宥堫檨闁告挾鍠庨悾宄邦潨閳ь剟銆侀弮鍫濆耿婵炲棙鐟ф惔濠囨⒒娴ｇ瓔鍤欓柛鎴犳櫕缁辩偤宕卞Ο纰辨锤闁荤姾妗ㄧ紞宥夋偄閻撳海鍔﹀銈嗗笒鐎氼參宕愰崹顐ょ闁割偅绻勬禒銏ゆ煛鐎ｎ偆鈯曢柕鍥у椤㈡﹢濮€閳╁啯娈稿┑鐑囩到濞层倝鏁冮鍫濈畺婵犲﹤鐗婇崵宥夋煏婢舵盯妾柣婵囨礋濮婄粯鎷呯粵瀣異闂佹悶鍔岄柊锝呯暦濮椻偓椤㈡柨顓奸崨顔尖偓顖炴⒒閸屾艾鈧悂宕愬畡鎳婂綊宕堕澶嬫櫔閻熸粌绉瑰﹢渚€姊虹紒妯虹伇婵☆偄瀚划濠氭偐缂佹鍘甸梺璇″瀻閸涱剟鍋楅梻浣呵归鍡涘箰閹间緤缍栨繝闈涱儐閸嬪倿骞栨潏鍓х？妞ゆ梹鍔栨穱濠囨倷椤忓嫧鍋撻妶澶婂偍濠靛倸鎲￠崐鑸电節闂堟冻鍔熷ù婊嗘閳规垿鎮欓懜闈涙锭缂備礁寮堕崕宕囧弲闂佸啿鎼崯鎵矆婵犲倶鈧帒顫濋敐鍛婵°倗濮烽崑娑㈡晝椤忓牆鏋侀柟鍓х帛閸嬫劙鏌涘▎蹇撯偓鎴﹀炊閵娧冨箞闂佽绻掗崑娑欐櫠娴犲鍋傛い鏍ㄥ嚬閻斿棝鎮峰▎蹇擃仾缂佲偓鐎ｎ喗鐓涚€光偓鐎ｎ剛袦闂佽鍠撻崹钘夌暦椤愶箑唯闁挎洍鍋撴繛鍛灴濮婅櫣鎷犻幓鎺濆妷闂佸憡鍨电紞濠傜暦閵忋倖鍊锋い鎺戝亞濞叉悂姊鸿ぐ鎺擄紵缂佽绻濆顐﹀炊椤掍胶鍘藉┑鈽嗗灠閸氬寮抽埡鍛叆婵炴垶鐟ч惌宀€绱掓潏銊﹀磳鐎规洘甯掗埢搴ㄥ箣濠靛棭鐎撮梻鍌欑劍鐎笛兠洪敃鍌氱；濠电姴娲ょ粻鐘绘煙閹规劕鐓愮€规洖顦甸弻鏇熺箾閸喚浠兼繛瀛樼矌閸嬫挾鎹㈠☉銏犻唶婵犻潧鐗呴搹搴ㄦ⒑閸濆嫷鍎愮紒瀣浮婵＄敻宕熼锝嗘櫍闂侀潧绻嗛崜婵嬪汲閻樺磭绠鹃柨婵嗘噺閹兼劙鏌ㄩ弴銊ら偗闁绘侗鍣ｅ浠嬪Ω閿曗偓椤庢捇姊洪崨濠勭細闁稿酣浜堕悰顕€骞囬悧鍫氭嫽婵炶揪绲介幊娆掋亹閹烘垵鐝樺銈嗗笒鐎氼參宕曞Δ浣典簻闁哄啠鍋撶€规洘蓱閹便劑宕樺ù瀣杸闂佺粯锚绾绢參銆傞弻銉︾厓闂佸灝顑呯粭鎺楁婢舵劖鐓ユ繝闈涙閸ｆ椽鏌涢悢鍝勪槐闁哄本鐩幃銏ゅ传閸曨亝鍩涚紓鍌欐祰妞村摜鏁悙鍝勭劦妞ゆ帒锕︾粔鐢告煕鐎ｎ亜顏柟顔斤耿楠炴﹢顢欓悾灞藉箰濠电偠鎻徊鎸庣仚闂佸搫妫寸紞渚€寮婚埄鍐╁缂佸绨遍崑鎾诲锤濡も偓缁犳澘鈹戦悩鎻掓殭缂佸墎鍋炴穱濠囶敍濠婂啫濡哄銈嗘⒐閹瑰洤顫忔繝姘＜婵炲棙鍩堝Σ顕€姊虹憴鍕偞闁逞屽墲缁夘喖煤椤忓懏娅囬梺绋挎湰閼归箖鎮楅銏♀拻濞撴艾娲ゆ晶顔剧磼婢跺灏﹂柟顕嗙節瀵挳濮€閿涘嫬骞嶉梺璇叉捣閺佸憡鐏欓柡宥忕節濮婃椽宕妷銉愶綁鏌涢弮鈧悷鈺侇嚕婵犳碍鏅查柛娑樺€瑰Λ鍐春閳ь剚銇勯幒鎴濐仾闁稿顑夐弻娑㈠焺閸愬墽鍔烽梺宕囨嚀缁夌敻濡甸崟顖氬唨闁靛ě鈧Λ鈺冪磽娴ｇ瓔鍤欐俊顐ｇ懇婵＄敻宕熼姘辩杸闂佸憡鎸风粈渚€宕哄畝鍕拺閻犲洩灏欑粻鏌ユ煙閸涘﹤鈻曠€殿喖顭烽弫宥夊礋椤忓懎濯伴梻浣告啞閹稿棗鐣濋埀顒佸閸モ晝纾介柛灞剧懆閸忓苯鈹戦鐐毈闁诡垰瀚伴、娑橆潩閻撳孩顓挎俊鐐€栧ú宥夊磻閹炬惌娈介柣鎰彧閼拌法鈧娲橀〃鍡楊嚗閸曨剛绡€濞达絽澹婂Λ婊堟⒒娴ｈ棄鍚瑰┑顔肩仛缁傚秵绂掔€ｎ亞顦繝銏ｅ煐閸旀洜澹曟繝姘厵闁绘劦鍓氶悘閬嶆煛鐎ｎ偅顥堥柡灞剧洴閳ワ箓骞嬪┑鍥╀憾闂備胶绮换宥夊闯閿濆拋娼栭柧蹇撴贡閻瑦绻涢崱妯哄姢闁告挷鍗冲娲箰鎼淬垹顦╂繛瀛樼矤娴滎亪鐛崘鈺侇嚤闁圭⒈鍘介弲鈺呮⒑閹肩偛鐏╂い锕備憾瀹曚即寮介鐐电枃闂佸搫绋侀崢濂告偂濞戙垺鐓曢柟閭﹀灡绾爼骞栭弶鎴伐闁宠鍨块幃鈺佲枔閹稿孩鐦滄繝纰樺墲瑜板啴骞婂Ο铏规殾闁荤喐澹嗛弳锕傛煕閵夋垵鍟版禍鐗堜繆閻愵亜鈧牠鎮у⿰鍫濈？闁圭増婢橀弸渚€鏌涘畝鈧崑鐐烘偂閻斿吋鐓涢柛灞炬皑娴犮垽鏌熼钘夌伌闁哄苯绉归弻銊р偓锝庝簽閻熴劑姊婚崶褜妯€闁哄被鍔岄埞鎴﹀幢濞嗗浚鏆柣鐐寸濮樸劎妲愰幒鏃傜＜闁靛繒濮寸粻褰掓⒑缂佹澧柕鍫⑶归锝囨嫚濞村顫嶉梺闈涚箚閸撴繂鈻嶅⿰鍫燁棅妞ゆ劑鍨烘径鍕箾閸欏鐭掔€殿喗濞婃俊鍫曞川閸屾稒顥堥柟顔规櫊濡啫鈽夊Δ鍐╁礋缂傚倸鍊烽懗鍓佸垝椤栫偞鏅濋柕蹇曞閸ゆ洘銇勯幇鍓佺暠缂佺姾宕电槐鎾存媴閼测剝鍨甸埢宥夊閵堝棌鎷洪柣鐘充航閸斿苯鈻嶉幇鐗堢厵闁告垯鍊栫€氾拷?
        assert(se != nullptr);      //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崰姘熼埀顒勬倵濞堝灝鏋熼柟顔煎€搁锝嗙鐎ｅ灚鏅ｉ梺缁樻煥閹碱偊鐛幇鐗堚拻濞达綀顫夐崑鐘绘煕鎼搭喖鐏︾€规洘娲熼弻鍡楊吋閸℃せ鍋撻崼鏇熺厓闁告繂瀚弳鐔虹棯閹佸仮闁哄瞼鍠栭弻鍥晝閳ь剙顕ｉ悙顒傜闁割偆鍠撻惌鎺楁煛瀹€瀣？濞寸媴濡囬崠鏍即閻愭澘顥氶梻浣哥秺椤ｏ妇绮堟笟鈧獮鍐箣閿旂晫鍘甸梺鐓庢贡婢ф銇愯缁辨帡鎳犵捄杞版睏闂佸憡甯楃敮鈥崇暦婵傜ǹ鍗抽柣鏂垮级鐎氳偐绱撻崒姘偓鐑芥倿閿曚焦鎳岄梻浣虹帛閹稿爼宕曢悽绋胯摕闁绘棁顔栭崷顓涘亾閿濆簼绨婚柍宄邦樀濮婃椽宕ㄦ繝鍌滅懆闂佹寧娲︽禍顏堟偘椤旂⒈娼ㄩ柍褜鍓熼妴浣糕枎閹炬潙浠奸柣蹇曞仩濡嫰宕ｆ繝鍥ㄢ拻闁稿本鍩冮惇瀣煕鐎ｎ亜顏╅悡銈夋煥閺囩偛鈧摜绮绘导瀛樼厱闁圭偓顨呴幊鎰版偂鐎ｎ亖鏀介柣鎰级閳绘洖霉濠婂嫮绠為柟顕嗙節瀹曟﹢顢欓悾灞藉箺闂備胶绮缁樼仚濡炪們鍎遍鍥╂閹烘鍋愮€瑰壊鍠氶崥瀣⒑閸濆嫬顦柍褜鍓欑壕顓㈠汲閸℃稒鍊甸柨婵嗛婢ф壆鎮敃鍌涒拻濞达絿鐡旈崵鍐煕閻樿櫕宕岀€规洖缍婇幐濠冨緞鐏炵偓顔曟俊鐐€栭崝褏绮婚幋婵囨殰闂傚倷鐒︾€笛兠哄澶婄；闁规儳顕粻楣冩煕濞戝崬鏋涢柣蹇撶摠閹便劍绻濋崨顕呬哗闂佸綊顥撴繛鈧鐐存崌楠炴帒鈹戦崱妞€劑姊婚崒娆戭槮闁硅绻濆畷褰掓偨閻㈢數顦繝鐢靛Т閸婂寮抽敂鐣岀瘈濠电姴鍊绘晶娑㈡煕鐎ｎ亜顏紒杈ㄥ笒铻栭柛鏇楁杹閸嬫捇鍩€椤掑嫭鐓曢悗锝庡亝鐏忔壆绱掑Δ鍐ㄦ灈闁糕斁鍋撳銈嗗坊閸嬫挻銇勯弴顏嗙М妞ゃ垺宀搁崺鈧い鎺嗗亾闁伙絿鍏橀獮鎺楀箣閺冣偓椤秴鈹戦埥鍡楃仯闁靛棗顑囧Σ鎰版晸閻樻枼鎷绘繛杈剧秬濡嫰宕ヨぐ鎺撶厱閻庯綆鍓欐禒褏绱掗鐣屾噰鐎规洖銈告俊鐑藉Ψ瑜滃Σ鑸电節閻㈤潧鈻堟繛浣冲浂鏁勯柟瀵稿Х缁€濠囨煛瀹ュ骸浜愰柛瀣崌閻涱噣宕归鐓庮潛婵＄偑鍊х紓姘跺础閹惰棄鏄ユ繛鎴欏灩缁狅綁鏌ㄩ弮鍌涙珪闁告﹢浜跺娲箰鎼淬埄姊垮銈嗘肠閸愭儳娈ㄩ梺闈涒康缂傛氨鎹㈤崱娑欑厽闁逛即娼ф晶顖炴煕婵犲啫濮嶉柡宀嬬磿娴狅箓宕滆閸掓稒绻濈喊妯哄⒉闁烩剝娲熻棟闁告瑥顦禍婊堟煥濠靛棛澧曞ù婊冩贡缁辨帗娼忛妸銉﹁癁閻庤娲樼敮鎺楋綖濠靛柊鎺戔枎濞嗘垹袦闂佸搫鏈ú鐔风暦婵傚壊鏁嗛柛灞剧敖濠婂牊鍊甸悷娆忓缁€鍐煥閺囨ê鐏茬€规洘妞介幃娆撴倻濡　鍋撻柨瀣ㄤ簻闁瑰搫妫楁禍鎯ь渻閵堝懐绠抽柛搴涘€楅幑銏犫攽鐎ｎ偄浠洪梻鍌氱墛缁嬫劙宕Δ鍛拺閻熸瑥瀚妵鐔访瑰⿰鍛户婵″弶鍔欓幃娆撳传閸曨厼濮︽俊鐐€栧濠氬磻閹剧粯鐓冮悷娆忓閻忓瓨銇勯姀锛勬噰鐎规洦鍋婂畷鐔煎Ω閵夈倕顥氭繝寰锋澘鈧劙宕戦幘缁樼厓闁芥ê顦藉Ο鈧梺璇″枤閺咁偆鍒掑▎鎾抽敜婵°倐鍋撻柡浣哥秺濮婄粯鎷呴崨濠冨創濠电偛鐪伴崝鎴濈暦娴兼潙绠婚悹鍥ㄥ絻閸嬪秹姊绘笟鍥у缂佸鏁婚崺娑㈠箣閻樼數锛濇繛杈剧到閸樻粌螞閵婏负浜滈煫鍥ㄦ尵婢ф洟鏌ｉ妶搴℃珝闁哄瞼鍠撶划娆撳垂椤曞懎濡烽梻浣告啞閻熴儳鎹㈤幒妤€鐒垫い鎺嗗亾缂佺姴绉瑰畷鏇㈠础閻忕粯妞介幃鈺冩嫚閼碱剨绱遍梻浣告贡閸嬫捇寮告總绋垮嚑闁哄啫鐗婇悡鏇熴亜椤撶喎鐏ラ柡瀣枛閺岋綀绠涢幘璺衡叺濠殿喖锕ㄥ▍锝呪槈閻㈢ǹ妞藉ù锝呮啞閻ゅ倻绱撻崒娆愮グ濡炲枪鐓ら柣鏃囧亹瀹撲線鏌熸潏鍓х暠妞ゎ偄鎳橀弻锝咁潨閸℃ぞ绨奸梺鍝勫€甸崑鎾绘⒒閸屾瑨鍏岀痪顓℃硾椤﹨顦圭€殿喗褰冮オ浼村醇濠靛牞绱抽梻鍌氬€搁悧濠冪瑹濡も偓閵嗘帗绻濆顓犲帾闂佸壊鍋呯换鍐夊⿰鍛＜婵°倓绀佸ù顕€鏌＄仦鍓с€掗柍褜鍓ㄧ紞鍡樼濠靛瑤澶愬閳垛晛浜鹃悷娆忓婢跺嫰鏌涢幘璺烘灈妤犵偛鍟粋鎺斺偓锝庝簻缁愭稒绻濋悽闈涗户闁稿鎸鹃埀顒佷亢濡嫰鍩為幋锔藉€烽柤鎼佹涧濞懷呯磽閸屾氨袦闁稿鎸搁埞鎴︽倷閸欏鐝旂紓浣瑰絻濞尖€愁嚕椤愶富鏁婇悘蹇旂墬椤秹姊洪棃娑㈢崪缂佽鲸娲熷畷銏ゆ焼瀹ュ棌鎷洪梻鍌氱墛閼冲棜銇愰幒鎴狅紵闂備緡鍓欑粔瀵哥不閺嶎厽鐓欓弶鍫ョ畺濡绢噣鏌ｉ幘瀵告噧闁靛棙甯掗～婵嬵敆閸屾瑨妾稿┑鐘愁問閸犳牠宕愰幖浣哥厴闁硅揪闄勯鎰版⒑缁洘娅嗛柣妤佹礋椤㈡岸濡烽埡浣侯槹濡炪倖甯掗崐濠氭儊閸儲鈷戦柛娑橈攻婢跺嫰鏌涢幘瀵告创鐎规洩缍佸畷鐔碱敍濞戞帗瀚介梻浣呵归張顒勬偡瑜旇棟闁挎柨澧界壕鐣屸偓骞垮劚閹锋垵顔忛妷鈺傛嚉闁绘劗鍎ら悡鏇犳喐鎼淬劊鈧啴宕卞☉妯硷紮闂佺鍕垫畷闁绘挶鍎甸弻锟犲炊閳轰椒绮堕梺閫炲苯澧紓宥咃工閻ｇ兘骞嬮敃鈧壕鍏肩箾閹寸偞鐨戞い鏃€娲熷娲偡閹殿喗鎲奸梺鑽ゅ枂閸庣敻骞冨鈧、鏃堝醇閻斿搫骞楅梻浣筋潐閸庢娊鎮洪妸褏鐭嗛悗锝庡枟閻撴瑦銇勯弮鈧弸濠氭嚀閸ф鐓欐い鏃傛櫕閹冲洦顨ラ悙鑼фい銏＄懇閹瑩顢楁担鐑樞掑┑鐘殿暜缁辨洟宕戦幋锕€纾归柡鍥╁У瀹曟煡鏌熼幍顔碱暭闁稿顑夐弻娑㈠焺閸愵亖濮囬梺绋匡工濞硷繝寮诲☉鈶┾偓锕傚箣濠靛懐鎸夊┑鐐茬摠缁秶鍒掗幘璇茶摕闁绘梻鍘ф导鐘绘煕閺囥劌甯ㄩ柕澶嗘櫆閻撴洟鐓崶銊﹀磩閻庢氨澧楅妵鍕敇閳ュ啿濮峰銈忓閹虫捇婀佸┑鐘诧工閸熶即宕洪敐鍥ｅ亾濞堝灝鏋︽い鏇嗗浂鏁囧┑鍌溓归柋鍥ㄣ亜閹板墎绉甸柍褜鍓欏ú顓烆潖缂佹ɑ濯撮柛娑橈工閺嗗牓姊虹憴鍕憙妞ゆ泦鍥ㄥ仼婵犻潧顑呯粻鐟懊归敐鍛喐闁汇倕娲ら埞鎴︽偐缂佹ɑ閿銈嗗灥閹虫劗鍒掗崼銉ラ唶闁靛繆鈧櫕鐎鹃梻濠庡亜濞诧箑煤濠婂牊鍎楅柛鈩冦仜閺€浠嬫煟濡绲婚柍褜鍓涢弫璇差嚕閵婏妇顩烽悗锝庡亞閸樿棄鈹戦埥鍡楃仩闁圭⒈鍋嗛惀顏囶樄闁哄瞼鍠栧畷妤呮偂鎼粹槅娼氶梻浣告惈閻绮婚幘宕囨殾婵せ鍋撴い銏℃瀹曠喖顢楅崒娆撳仐闂傚倸鍊风粈渚€骞栭锕€瀚夋い鎺嗗亾閻撱倖淇婇姘倯鐎规洖寮剁换娑㈠箣濞嗗繒浠鹃梺绋匡躬閺€閬嶅Φ閸曨垰绠崇€广儱鐗嗛崢锟犳偡濠婂嫭绶叉俊顐ｇ箓椤繒绱掑Ο璇差€撻梺鍛婄洴濞佳呯礊婵犲洢鈧線寮介鐐茬獩濡炪倖鐗楅〃鍛閻愵剚鍙忔慨妤€妫楁禍婊呪偓瑙勬尭濡盯鍩€椤掍緡鍟忛柛鐘崇墵閳ワ箓鎮滈挊澶嬬€梺褰掑亰閸樿偐娆㈤悙娴嬫斀闁绘ɑ褰冮鎾煕濮橆剚鍤囨慨濠勭帛閹峰懘鎼归悷鎵偧闂備焦鎮堕崝鎴濐焽瑜戦悘瀣攽閻愭潙鐏熼柛銊ユ贡缁牊寰勯幇顓犲幗闂佺鎻徊鍊燁暱闂備焦濞婇弨閬嶅垂閸ф钃熸繛鎴炃氶弨浠嬫煕閳╁喚娈㈠ù鐘荤畺濮婃椽骞愭惔锝傚濠殿喖锕ㄥ畷闈浳ｉ幇鏉跨婵°倓绀佹禍褰掓⒑閹勭闁稿瀚竟鏇㈠箮閼恒儱鈧灚绻涢崼婵堜虎闁哄绋掗妵鍕敇閻樻彃骞嬮悗瑙勬礃濡炶棄鐣烽悢纰辨晬婵﹢纭稿Σ浼存⒒娓氣偓濞佳嗗櫣闂佸憡渚楅崹鎵矈閿曗偓閳规垿鎮╅崹顐ｆ瘎婵犳鍠氶崗妯侯嚕椤愶箑宸濆┑鐘插暙瀵潡鏌ｆ惔锝嗘毄缁绢厼鐖煎畷鎴﹀箻閹颁焦鍍甸梺缁樻尭妤犳悂鍩涚€ｎ喗鈷戠紒顖涙礃濞呭懘鏌熼搹顐€跨€殿喖顭峰畷鍗炍旀繝鍌涘€梻浣虹《閳ь剙纾粻鏌ユ倵濮樺啿浜圭紒杈ㄦ崌瀹曟帒鈻庨幋顓熜滄俊鐐€栭崹闈浳涘┑瀣ㄢ偓渚€寮借閺嬪酣鏌熼幆褏锛嶉柨娑氬枛濮婅櫣鎲撮崟顐婵犫拃鍕垫疁闁诡喒鈧枼鏋庨柟鎯ь嚟閸樼敻姊婚崒姘偓濠毸夐幇鏉跨鐎光偓閸曨剛鍘介梺鎸庣箓濡瑩濡靛┑瀣厸閻忕偛澧藉ú鎾煕閳哄啫浠辨鐐叉椤﹀崬顭跨憴鍕闁宠鍨块弫宥夊礋椤愨剝婢€闂備胶枪閿曘儵鎮у⿰鍫濇瀬妞ゆ梻鏅弧鈧梺绋款儛閸ㄦ壆绱炴繝鍌滄殾闁割偅娲栨儫閻熸粌绻楅妵鎰板箚瑜夐弨浠嬫煃閽樺顥滈柣蹇嬪劦閺屾稓鈧綆鍋勬慨澶婎熆鐟欏嫭绀嬮柟顔煎⒔娴狅妇绮欐径绋款棜婵°倗濮烽崑娑㈡倶濠靛绀夋慨妯垮煐閻撴瑩姊洪崹顕呭剰闁诲繑鎸抽弻锝夊箼閸愩劌鈷嬪銈冨灪椤ㄥ﹤鐣烽幒鎳虫棃鍩€椤掍胶顩查柣鎰靛墰缁犻箖鏌涢锝囩畼濞寸姰鍨介弻锛勨偓锝庡亞閳洟鏌曢崶褍顏い銏★耿婵偓闁斥晛鍟▓绋库攽閻戝洨鍒版繛灞傚€濋弫鍐敂閸垹绁﹂梺鍝勭Р閸斿秵鍒婇幘顔界厱婵犻潧瀚崝姘殽閻愭潙濮嶆慨濠勭帛閹峰懘宕ㄩ棃娑氱Ш鐎殿喚鏁婚、妤呭礋椤掆偓閸撶懓顪冮妶鍡樷拹鐎规洘蓱閹便劑宕樺ù瀣杸闂佺粯锚绾绢參銆傞弻銉︾厓闂佸灝顑呯粭鎺楁婢舵劖鐓ユ繝闈涙閸ｆ椽鏌涢悢鍝勪槐闁哄本鐩幃銏ゅ传閸曨亝鍩涚紓鍌欐祰妞村摜鏁悙鍝勭劦妞ゆ帒锕︾粔鐢告煕鐎ｎ亜顏柟顔斤耿楠炴﹢顢欓悾灞藉箰濠电偠鎻徊鎸庣仚闂佸搫妫寸紞渚€寮婚埄鍐╁缂佸绨遍崑鎾诲锤濡も偓缁犳澘鈹戦悩鎻掓殭缂佸墎鍋炴穱濠囶敍濠婂啫濡哄銈嗘⒐閹瑰洤顫忔繝姘＜婵炲棙鍩堝Σ顕€姊虹憴鍕偞闁逞屽墲缁夘喖煤椤忓懏娅囬梺绋挎湰閼归箖鎮楅銏♀拻濞撴艾娲ゆ晶顔剧磼婢跺灏﹂柟顕嗙節瀵挳濮€閿涘嫬骞嶉梺鍝勵槸閻楁挾绮婚弽褜鐎舵い鏇楀亾闁哄矉缍侀獮妯兼崉閻戞浜梻浣告惈閻绱炴担鍓插殨妞ゆ帒瀚粻浼村箹缁懓澧叉繛鍫燁焽缁辨捇宕掑▎鎰偘濠电偛顦扮粙鎴︻敋閿濆绀堝ù锝囨嚀鎼村﹤鈹戦悙鏉戠仧闁搞劌缍婇弻瀣炊閵婏箑寮垮┑顔筋殔濡鐛Δ鍛厽婵犻潧娲ら崫鐑樻叏婵犲啯銇濈€规洦鍋婂畷鐔碱敇閻樿京绀冮梻鍌欑閹芥粍鎱ㄩ悽鍛婂亱闁哄洢鍨洪崑鈺冣偓鐟板婢瑰寮告惔銊︾厵闁绘劦鍓涚粣鐐垫喐閺冨牆钃熸繛鎴欏灩閻掓椽鏌涢幇顔间壕妞ゆ捁顫夌换娑氣偓鍨偠閳ь剙顑夐獮蹇涙晸閿燂拷
    }
    name = se->toStr();
    type = se->getType()->toStr();
    fprintf(yyout, "%*cFunctionDefine function name: %s, type: %s\n", level, ' ',
        name.c_str(), type.c_str());
    if (paraStmt != nullptr)
        paraStmt->output(level + 4);
    if (stmt != nullptr)
        stmt->output(level + 4);
}














//merge const exp here
void Ast::mergeConstExp()
{
    root->mergeConstExp();
}




