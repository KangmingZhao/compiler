#include "Ast.h"
#include "SymbolTable.h"
#include "Unit.h"
#include "Instruction.h"
#include "IRBuilder.h"
#include <string>
#include <assert.h>
#include<algorithm>

extern FILE *yyout;
int Node::counter = 0;
IRBuilder* Node::builder = nullptr;
bool isreturn=false;
Type *retVal;
std::vector<Type*> paramVector;
std::vector<Operand *> para_operands;// 闂備浇顕х€涒晝绮欓幒妤佹櫔闂備焦鎮堕崐婵囩鐠轰警鍤曟い鎰剁畱缁狀垳鈧厜鍋撻柍褜鍓欓埢鎾诲醇閺囩喓鍘介梺閫炲苯澧┑鈩冨缁绘盯宕奸悢椋庝紝閻庤娲╃紞浣哥暦閹烘垟妲堟繛鍡樺姦閸熲偓闂傚倷鐒﹂惇褰掑礉瀹€鍕瀭缂佸崬澧�

std::vector<std::vector<Operand*>>para_operands_stack;

LoopManager loop_manager;
BasicBlock* temp_end_bb;
BasicBlock* temp_then_bb;
ExprNode* temp_cond_expr;

ExprNode* bugs_detector;


bool now_is_def_funct = 0;

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
    // 闂傚倷绀佸﹢杈╁垝椤栫偛绀夐柡鍥╁枑濞呯娀鏌ｅΟ铏癸紞闁绘繂鐖奸弻锝呂旈埀顒勬偋婵犲浄缍栭柨鐕傛嫹
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
        new CmpInstruction(opcode, dst, src1, src2, bb);

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
    //    //闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡宀€鍠栭幃褔宕奸悢鍝勫殥闂備礁鎲￠崹瑙勬叏閹绢喖鐓橀柟杈鹃檮閸嬫劙鎮楅崷顓炐㈡い銉︾箘缁辨挻鎷呴崫鍕碘偓宀勬煕閵娿儳鍩ｇ€殿喖顭烽弫鎰緞婵犲嫷鍚呴梻浣瑰缁诲倿骞夊☉銏犵缂備焦顭囬崢鎼佹⒑閸涘﹣绶遍柛鐘愁殜瀹曟劙骞囬悧鍫㈠幐閻庡厜鍋撻悗锝庡墰閻﹀牓鎮楃憴鍕閻㈩垱甯熼悘鎺楁煟閻愬鈻撻柍褜鍓欓崢鏍ㄧ珶閺囥垺鈷戦柛婵嗗閳诲鏌涚€Ｑ冧壕闂備胶枪椤戝棝骞愰幖渚婄稏婵犻潧顑愰弫鍡涙煕鐏炲墽鏁栭柕濞炬櫆閳锋垿鏌涘┑鍡楊伂妞ゎ偓绠撻弻娑㈠籍閳ь剟宕归崸妤冨祦闁告劦鍠栭～鍛存煏閸繃鍣圭紒鎰⊕缁绘繈鎮介棃娴躲垽鎮楀闂寸敖婵″弶鍔欏鎾閳锯偓閹风粯绻涙潏鍓у埌闁硅姤绮庣划鏃堟倻濡晲绨婚梺闈涱檧闂勫嫬鐣峰畝鈧埀顒冾潐濞叉﹢銆冮崨杈剧稏婵犲﹤鐗嗛悞鍨亜閹哄棗浜惧銈嗘穿缂嶄線銆侀弴銏℃櫇闁逞屽墰缁粯绻濆顓炰化闂佹悶鍎滈崘顏堢崜婵＄偑鍊戦崕鑼崲閸繍娼栨繛宸簼椤ュ牊绻涢幋鐐跺妞わ絽鎼埞鎴﹀煡閸℃ぞ绨煎銈冨妼閿曨亪濡存笟鈧顕€宕掑☉妯硷紡闂佽崵鍋炵粙鍫ュ礈濠靛缍栫€广儱顦伴埛鎴︽煕閿旇寮ㄦ俊鐐倐閺屾盯濡歌椤ｈ偐绱掗崒姘毙ｉ柕鍫秮瀹曟﹢鍩為悙顒€顏烘繝鐢靛仩閹活亞绱為埀顒€鈹戦鍝勨偓妤€鈽夐悽绋块唶闁哄洨鍠撻崢閬嶆⒑缂佹◤顏堝疮閹稿孩鍙忕€广儱娲犻崑鎾斥枔閸喗鐏堥梺纭咁嚋缁辨洟骞戦姀鐘斀闁糕檧鏅滈崓闈涱渻閵堝棗绗掗柛瀣瀹曨剟鎮介崨濞炬嫽婵炶揪缍€濞咃絿鏁☉銏＄厱闁靛ǹ鍎遍幃鎴︽煙娓氬灝濮傚┑陇鍩栧鍕節閸曨剛鍙勯梻鍌欑缂嶅﹤螞閸ф鍊块柨鏇炲€哥壕濠氭煃閳轰礁鏆熺紒鐘荤畺閺屾盯鍩勯崘鍓у姺闂佺懓鍟块幊鎰閹烘挻濯撮柛婵嗗楠炲姊洪崫鍕缂佸鍏樼瘬濞撴埃鍋撻柡灞剧洴楠炴﹢宕￠悙宸綆婵°倗濮烽崑娑氭崲濮椻偓瀵偊骞樼紒妯绘闂佽法鍣﹂幏锟�
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
}

void Id::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    Operand *addr = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getAddr();
    //std::cout << addr->toStr() << std::endl;
    new LoadInstruction(dst, addr, bb);
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
    cond->getSymPtr()->setType(TypeSystem::boolType);
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



    cond->getSymPtr()->changeType(TypeSystem::boolType);
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
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾瑰瀣捣閻棗銆掑锝呬壕濡ょ姷鍋涢ˇ鐢稿极瀹ュ绀嬫い鎺嶇劍椤斿洭姊绘担绋挎毐闁圭⒈鍋婇獮濠冩償閿濆拋妫滄繝鐢靛У绾板秹鎮￠弴銏＄厸闁搞儯鍎辨俊鍏碱殽閻愭惌娈橀柍褜鍓濋～澶娒哄鈧幃锟犳晸閻樿尪鎽曢梺璺ㄥ枔婵挳鎮欐繝鍥ㄧ厓閺夌偞澹嗛崝宥夋煟韫囧海顦︽い顏勫暣婵″爼宕掑☉娆戝絾闂備胶绮幖顐ゆ崲濠靛棭鍤曢悹鍥ㄧゴ濡插牓鏌曡箛鏇炐ラ柛鏃€鎸冲铏圭矙鐠恒劎顔婃繝娈垮灱閸樼晫绮嬪鍛斀閻庯綆浜為敍鐔兼⒑缂佹鐭ら柟鐣屾啒e闂傚倸鍊搁崐鎼佸磹閹间礁纾瑰瀣捣閻棗銆掑锝呬壕濡ょ姷鍋涢ˇ鐢稿极瀹ュ绀嬫い鎰╁灮娴滀即鏌ｆ惔鈥冲辅闁稿鎹囬弻娑㈠即閻愬吀绮甸梺鍝勬噳閺呯姴顫忛搹瑙勫厹闁告侗鍘奸幆鐐电磽娴ｅ摜鐒跨紒鐙€鍨跺濠氬磼濮橆兘鍋撻幖浣哥９闁绘垼濮ら崐鍧楁煥閺囩偛鈧綊宕曢幋鐘冲枑闁绘鐗嗙粭鎺旂磼閳ь剚寰勭仦绋夸壕闁稿繐顦禍楣冩⒑闁偛鑻晶鎾煕閳规儳浜炬俊鐐€栫敮濠勭矆娓氣偓瀹曠敻顢楅崟顒傚幈闂侀潧枪閸庢娊宕洪敐澶嬬厪闁糕剝娲滈ˇ锔姐亜椤愶絿绠橀柟鐟板缁楃喖顢涘☉姘毙熼梻鍌氬€风欢姘焽瑜旈幃褔宕卞☉妯肩枃闂佽澹嗘晶妤呭磻閸岀偞鐓欓弶鍫ョ畺濡绢噣鏌嶉柨瀣伌婵﹤顭峰畷濂告偄閸撲胶绠掓繝纰樺墲瑜板啴鎮ユ總绋胯摕婵炴垶鐟﹂崕鐔兼煏韫囧鐏╃€殿喓鍔戦幃妤冩喆閸曨剛顦ㄩ梺鎼炲妽鐎笛勭┍婵犲洦鍊婚柦妯侯槺閸樻悂姊虹粙鎸庢拱缂侇喖鐭傞悰顕€骞囬悧鍫氭嫽婵炶揪绲挎灙妞ゃ儱绻橀弻娑氣偓锝庝簼閸ゅ洭鏌熼搹顐ょ疄闁诡喗鐟╅、妤呭磼閵堝棝鏁滄繝寰锋澘鈧呭緤娴犲鐤い鏍仜閻撴洟鏌熸潏楣冩闁绘挻娲樼换娑㈠箣閻戝洤鍙曟慨鎺濆亝閼归箖鈥︾捄銊﹀枂闁告洜鏁搁崝椋庣磽娴ｄ粙鍝洪悽顖涘浮閹儳鈹戠€ｎ亞鍔﹀銈嗗笒鐎氼剛绮婚弻銉︾厱闁靛鍠栨晶顖炴煃闁垮绗掗棁澶愭煥濠靛棛澧辨繛鍏煎姍閺屾稓鈧綆鍓欓埢鍫ユ煛鐏炲墽鈯曢柟顖涙婵偓闁绘﹩鍋呴悘鍡涙⒒娴ｈ櫣銆婇柡鍌欑窔瀹曟垿骞橀幇浣瑰瘜闂侀潧鐗嗗Λ妤呭锤婵犲洦鐓曢悗锝庡亝瀹曞本鎱ㄦ繝鍐┿仢闁诡喖澧芥禒锔剧矙婢剁ǹ顥氶柣搴″帨閸嬫捇鏌涢弴銊ュ幋闁归攱妞藉娲偂鎼搭喗缍楅梺绋匡攻濞叉牠鎮洪鐔虹瘈闁汇垽娼ф禒锔界箾閸忚偐鎳囩€规洘鍔欏畷褰掝敊閻愵剚顔曢梻渚€娼х换鍫ュ磹閺嶎厼鍚圭€光偓閸曨剛鍘鹃梺鍛婃尰瑜板啴宕㈤崨濠冨弿閻熸瑥瀚峰▓婊勬叏婵犲懏顏犵紒杈ㄥ笒铻ｉ柛婵嗗濞兼捇姊绘担鍛婅础闁冲嘲鐗撳畷鎴炵節閸パ嗘憰闂侀潧艌閺呮稑娲垮┑鐘灱濞夋盯顢栭崶顒€鐭楅柛鈩冪⊕閳锋垹绱撴担鑲℃垹绮堥埀顒勬⒑缁嬪尅宸ユ繝鈧柆宥呯劦妞ゆ帊鑳堕崯鏌ユ煙閸戙倖瀚�
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
    SymbolEntry *se = this->getSymbolEntry();
    Type *ret = ((FunctionType *)(se->getType()))->getRetType();
    if (stmt == nullptr&&ret != TypeSystem::voidType)
    {   
         fprintf(stderr, "function\'%s\'misses return\n",se->toStr().c_str());
        // 闂傚倸鍊搁崐鎼佸磹閹间礁纾瑰瀣捣閻棗銆掑锝呬壕濡ょ姷鍋涢ˇ鐢稿极閹剧粯鍋愰柟缁樺笧閳ь剦鍙冨铏圭矙鐠恒劎顔囬梺鍛婅壘椤戝鐣烽敐鍫㈢杸婵炴垶鐟ч崢顏堟⒑閸撴彃浜濈紒璇茬Т鍗遍柛娑橈攻閸欏繘鎮峰▎蹇擃伀闁告瑢鍋撻梻浣告惈閻绱炴担鍓插殨妞ゆ帒瀚粻浼村箹缁懓澧叉繛鍫熋埞鎴︽偐閹颁礁鏅遍梺鎼炲姂娴滃爼鐛幇鏉跨闁芥ê顦抽幗鏇㈡⒑缂佹ɑ鈷掗柛妯犲懐涓嶆慨妯块哺閸犳劙鐓崶銊︹拻妞も晛寮堕妵鍕疀閹捐泛顤€闂佺粯鎸荤粙鎴︽箒闂佹寧绻傚В銉ㄣ亹閹烘繃鏅滃銈嗘尪閸ㄦ椽鎮″☉銏″€堕柣鎰邦杺閸ゆ瑥鈹戦鐓庘偓鍧楀蓟閻旂⒈鏁婇柛婵嗗閸嬫挸鈹戦崱娆愭闂佸湱鍎ら崹鐔肺ｉ崼鐔稿弿婵°倐鍋撴俊顐ｆ⒒濡叉劙鏁撻敓锟�?闂傚倸鍊搁崐鎼佸磹閹间礁纾瑰瀣捣閻棗銆掑锝呬壕濡ょ姷鍋涢ˇ鐢稿极瀹ュ绀嬫い鎺嶇劍椤斿洭姊绘担瑙勫仩闁稿孩妞藉畷婊冣枎閹存繍妫滈悷婊呭鐢鎮″☉姘ｅ亾楠炲灝鍔氬Δ鐘虫倐閻涱噣寮介鐔哄弮闂佸憡鍔︽禍婊堝几濞戙垺鐓涢悘鐐插⒔濞叉潙鈹戦敍鍕幋妞ゃ垺鐟╅幃鈩冩償椤旀垝姘﹂梻鍌氬€风粈渚€骞夐敓鐘冲仭闁靛鏅滈弲婵嬫煏婢跺棙娅呭鍛存⒑閸涘﹥澶勯柛銊ゅ嵆瀹曟﹢鍩€椤掑嫭鈷戦梻鍫熶腹濞戞矮娌柣鎰靛墻閸熷洭姊婚崒姘偓鐑芥嚄閼哥數浠氬┑掳鍊楁慨鐢稿箖閸岀偛鏋侀柛鎰靛枟閺呮粓鏌ｉ幇闈涘⒒婵炶偐鍠栧娲捶椤撶偛濡洪梺鎼炲€х粻鎾愁嚕椤曗偓瀹曟帒螖閳ь剚绂嶆ィ鍐╃叆婵犻潧妫濋妤€顭胯閸楁娊寮婚敓鐘插耿婵炲棗绻嗛弸鍛攽椤旂》榫氭繛鍜冪秮楠炲繘鎮╃拠鑼紜闂佹儳娴氶崑鍡樼椤忓棛纾介柛灞剧懅閸斿秹鏌涢弮鈧悧鐘差嚕閵娧冨缂佹冻鎷�
    }
    // 闂傚倸鍊搁崐鎼佸磹閹间礁纾瑰瀣捣閻棗銆掑锝呬壕濡ょ姷鍋涢ˇ鐢稿极閹剧粯鍋愰柟缁樺笧閳ь剦鍙冨铏圭矙鐠恒劎顔囬梺鍛婅壘椤戝鐣烽敐鍫㈢杸婵炴垶鐟ч崢顏堟⒑閸撴彃浜濈紒璇茬Т鍗遍柛娑橈攻閸欏繘鎮峰▎蹇擃伀闁告瑢鍋撻梻浣告惈閻绱炴担鍓插殨妞ゆ帒瀚粻浼村箹缁懓澧叉繛鍫熋埞鎴︽偐閹颁礁鏅遍梺鎼炲姂娴滃爼鐛幇鏉跨闁芥ê顦抽幗鏇㈡⒑缂佹ɑ鈷掗柛妯犲懐涓嶉柡灞诲劜閻撴瑩姊洪銊х暠濠⒀呭閹便劑鏁愰崘銊т紙濠殿喖锕ュ浠嬪箠閿熺姴围闁告侗鍠氶埀顒€澧界槐鎾存媴缁涘娈梺缁橆殕閹告悂顢氶敐鍡欑瘈婵﹩鍎甸埡鍛厓闁告繂瀚埀顒€鎲＄粋宥夋倷閻戞ǚ鎷虹紓浣割儐鐎笛冿耿閹殿喚纾奸悗锝庡亜閻忓瓨銇勯姀鈩冾棃闁轰焦鎹囬弫鎾绘晸閿燂拷?闂傚倸鍊搁崐鎼佸磹閹间礁纾瑰瀣捣閻棗銆掑锝呬壕濡ょ姷鍋涢ˇ鐢稿春閸曨垰绀冩い蹇撳暟濞夊﹪姊绘担绛嬪殭婵﹫绠撻敐鐐村緞婵炴帗妞介幃銏＄附婢跺苯濮洪柣鐔哥矋濡啫顕ｉ锕€绀冩い鏃傗拡濞煎﹪姊洪棃娑氬闁瑰啿娴风划锝呪槈閵忥紕鍘介柟鍏肩暘閸ㄥ鍩婇弴銏＄叆婵ǹ鍩栭悡鏇㈡煙閻愵剚缍戦柍缁樻礋閺屸€崇暆鐎ｎ剛蓱闂佽鍨卞Λ鍐€佸☉妯峰牚闁告稑鎷戠紞浣割潖閾忓湱鐭欐繛鍡樺劤閸撲即姊虹涵鍛彧闁告梹鐟╅獮鍐偪椤栵絾鈻屾繝娈垮枛閿曪妇鍒掗鐐茬闁告稒娼欏婵嗏攽閻樻彃鈧懓鈻撳鈧缁樻媴閾忕懓绗″┑鈽嗗亜缁绘ê鐣峰⿰鍐炬僵妞ゆ挾濮弨铏節閻㈤潧孝婵炲眰鍊濋幃娆愮節閸愶缚绨婚梺鍦劋閸╁牆危瑜版帗鐓涢柛娑卞灠娴狅妇绱掔紒妯兼创妤犵偞锕㈠鍫曞箣閻樻彃袪闂傚倷鑳舵灙妞ゆ垵妫濋獮鎰偅閸愩劎鍔﹀銈嗗笒閸犳艾顭囬幇顓犵闁圭粯甯炵粻鑽も偓瑙勬礃濞茬喎螞閸愩劉妲堟慨姗嗗幗椤旀洟姊绘担铏瑰笡闁圭ǹ鎽滈懞閬嶅醇閺囩偟鍝楁繛瀵稿Т椤戝棝鎮″▎鎾寸厵妞ゆ牕妫楅崰姘跺汲閵忋倖鈷戠紓浣股戠亸浼存煕閵娿儳浠㈡い顐㈢箳缁辨帒螣鐠囧樊鈧捇姊洪崗闂磋埅闁稿孩濞婃俊瀛樼節閸ャ劉鎷洪梻鍌氱墐閺呮繄绮旈崜浣虹＜闁绘ǹ娅曞畷宀勬煟濞戝崬鏋︾紒鐘崇☉閳藉鈻庨幇顓т户闂傚倷娴囧▔鏇㈠闯閿曞倸绠柨鐕傛嫹?
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
                fprintf(stderr, "function \'%s\'has wrong return \n",se->toStr().c_str());
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
       isreturn=true;//闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜忛弳锕傛煕椤垵浜濋柛娆忕箻閺屸剝寰勭€ｎ亝顔呭┑鐐叉▕娴滄粌娲垮┑鐘灱濞夋盯顢栭崨顔绢浄闂侇剙绉甸埛鎴︽⒒閸喕鍎愮憸鐗堝笒绾炬寧绻涢崱妯诲鞍闁哄懏褰冮…璺ㄦ崉閻氭潙濮涘銈嗘尰濡炶棄顫忓ú顏勭閹兼番鍩勫鍨攽閳藉棗浜滈悗姘緲閻ｅ嘲饪伴崼婵堫唴闂佸吋浜介崕顕€骞忛搹鍦＝闁稿本鐟ч崝宥夋嫅闁秵鐓熼柟鎯у船閸旓箓鏌″畝鈧崰鎾诲箯閻樹警妲剧紒鐐礃濞夋洜妲愰幒妤婃晩缁炬媽浜崥瀣旈悩闈涗沪閻㈩垱甯熼悘鍐⒑闁偛鑻晶浼存煟閵夘喕娴锋い锕€缍婇弻锛勪沪閻ｅ睗褏鈧娲﹂崑濠傜暦閻旂⒈鏁冮柣鏃囨腹婢规洟鎮峰⿰鍛暭閻㈩垱顨婂畷鎴﹀磼閻愬鍘繝銏ｅ煐缁嬫垿銆呴鍌滅＜闁绘ǹ宕甸悾娲煛鐏炲墽鈽夐柍璇叉唉缁犳盯鏁愰崰鑸妽缁绘繈鍩涢埀顒勫川椤旇姤鐦撻梻浣告惈閻ジ宕伴弽顓溾偓浣糕槈濮楀棙鍍靛銈嗗笂閻掞箓顢欓幒妤佺厽閹兼番鍊ゅ鎰箾閸欏鐭掔€规洑鍗冲浠嬵敇濠ф儳浜惧〒姘ｅ亾鐎殿噮鍣ｅ畷鐓庘攽閸℃埃鍋撻崸妤佲拺閻庡湱濮甸妴鍐╀繆閻愭潙绗х紒杈ㄦ崌楠炴绱掑Ο閿嬪闂備胶枪閺堫剟鎮烽敂鍓х焾闁绘鐗忕粻鎯ь熆鐠轰警鍎愮紒鈧€ｎ兘鍋撶憴鍕濠电偛锕顐﹀礃椤旇偐锛滃┑顔斤耿绾悂宕悽鍛娾拻濞达綀濮ょ涵鍫曟煕閿濆繒鐣垫鐐茬箻閺佹捇鏁撻敓锟�?
        retVal=retValue->getSymPtr()->getType(); //闂傚倸鍊搁崐鎼佸磹妞嬪海鐭嗗〒姘ｅ亾妤犵偞鐗犻、鏇氱秴闁搞儺鍓﹂弫鍐煥閺囨浜鹃梺姹囧€楅崑鎾舵崲濠靛洨绡€闁稿本绮岄。娲⒑閽樺鏆熼柛鐘崇墵瀵寮撮悢铏诡啎闂佸壊鐓堥崰鏍ㄧ珶閸曨偀鏀介柣鎰级閳绘洖霉濠婂嫮鐭掔€规洘锕㈤崺鈧い鎺嗗亾妞ゎ亜鍟存俊鍫曞幢濡儤娈梻浣呵归鍥窗閺嶎厼鏋佺€广儱鎳夐弸搴ㄦ煙閸撗喫夐柟閿嬫そ濮婅櫣绮欑捄銊ь啈闂佺ǹ顑嗛崝娆愪繆閸洖绠绘い鏃傛櫕閸樻悂姊洪崨濠佺繁闁搞劎澧楃粋鎺楁焼瀹ュ棛鍙嗗┑鐘绘涧閻楀棙绂掗柆宥嗙厸濞达絿枪閺嗭絿鈧娲樼敮鎺楀煡婢跺⿴娼╂い鎺嗗亾婵鐓″濠氬磼濞嗘帒鍘＄紓渚囧櫘閸ㄦ壆鍙呴梺鍝勭▉閸樿偐澹曡ぐ鎺撶厱闁挎棁顕ч獮鎰版煕鐎ｎ偅灏い顐ｇ箞閹瑩顢楅埀顒勵敂閿燂拷?
    }


    if (retValue != nullptr)
        retValue->typeCheck();

}

void AssignStmt::typeCheck()
{
    // Todo
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡宀€鍠栭幃褔宕奸悢鍝勫殥闂備礁鎲￠崹瑙勬叏閹绢喖鐓橀柟杈鹃檮閸嬫劙鎮楅崷顓炐㈡い銉︾箘缁辨挻鎷呴崫鍕碘偓宀勬煕閵娿儳鍩ｇ€殿喖顭烽弫鎰緞婵犲嫷鍚呴梻浣瑰缁诲倿骞夊☉銏犵缂備焦顭囬崢鎼佹⒑閸涘﹣绶遍柛銈嗗濞戠敻宕ㄩ娆戣兒婵犵數濮撮惀澶愬级鎼存挸浜鹃柡鍥╁Л閸嬫挸顫濋悡搴＄睄濡炪們鍨哄Λ鍐箖閳哄啰纾兼俊顖滃帶鐢箖姊绘担铏广€婇柛鎾寸箞閵嗗啳绠涘☉妯硷紵闂佸搫鍟犻崑鎾淬亜閵婏絽鍔﹂柟顔界懇閹崇娀顢楅崒銈呮櫔闂傚倷绀侀幖顐ゆ偖椤愶箑绀夐幖娣妼閻掑灚銇勯幒鎴姛缂佷胶鍏橀弻娑㈠棘閹稿孩鎷辩紓浣稿€哥粔鐢碘偓浣冨亹閳ь剚绋掗…鍥储閸涘﹦绡€闁靛骏绲剧涵楣冩煥閺囶亪妾柡鍛埣婵偓闁靛牆妫涢崢閬嶆煟鎼搭垳绉甸柛瀣噽娴滄悂鎮介崨濠勫帗閻熸粍绮撳畷婊冾潩閼搁潧娈為梺璇″瀻瀹ュ牃鍋撻崸妤佲拺妞ゆ巻鍋撶紒澶婎嚟缁顢涘⿰鍕瀾闂佺粯顨呴悧鍡樼┍椤栫偞鐓涘ù锝嚽归弳锝夋煛鐏炶濮傜€殿噮鍣ｅ畷鍫曗€栭鑺ュ鞍缂佺粯鐩畷銊╊敃閵忣澀绱欐繝娈垮枛閿曘儱顪冮挊澶屾殾妞ゆ劧绠戠粈瀣亜閹邦喖鏋庡ù婊堢畺閺屾盯顢曢敐鍡欘槬缂佺偓鍎崇紞濠囧蓟閳ユ剚鍚嬮幖绮光偓鑼埍闂備胶枪椤戝棝骞愰幖渚婄稏婵犻潧顑愰弫鍡涙煕鐏炲墽鏁栭柕濞炬櫆閳锋垿鏌涘┑鍡楊伂妞ゎ偓绠撻弻娑㈠籍閳ь剟宕归崸妤冨祦闁告劦鍠栭～鍛存煏閸繃鍣圭紒鎰⊕缁绘繈鎮介棃娴躲垽鎮楀闂寸敖婵″弶鍔欏鎾閳锯偓閹风粯绻涙潏鍓у埌闁硅姤绮庣划鏃堟倻濡晲绨婚梺闈涱檧闂勫嫬鐣峰畝鈧埀顒冾潐濞叉﹢銆冮崨鎵航闂備胶鍘ч～鏇㈠磹閵堝悿褰掑礋椤愵偅瀵岄梺闈涚墕濡稒鏅堕柆宥嗙厱閻庯綆鍓欐禒閬嶆煙椤曞棛绡€濠碉紕鍏橀崺锟犲磼濠婂啫绠洪梻鍌欑閹碱偊宕愰崷顓涘亾缁楁稑娲ら悿顕€鏌ｉ幇顔煎妺闁绘挾鍠栭悡顐﹀炊妞嬪骸鍩屽┑鐐叉噹缁夊綊寮婚垾宕囨殕闁逞屽墴瀹曚即骞囬崗顐㈡喘閹囧醇閻斿嘲濡抽梻浣哄仺閸庨亶宕捄銊т笉闁挎繂顦伴埛鎺懨归敐鍫澬撻柕鍡楀暣閺岋綁骞掗悙鐢垫殼濡ょ姷鍋為崝娆撶嵁閸℃凹妾ㄩ梺鎼炲€栭〃鍛村煘閹达附鍋愰柟缁樺笂缁ㄨ棄鈹戦悙鐑橈紵闁告濞婂濠氭晲婢跺á鈺呮煏韫囨洖小濠㈣妾畉闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡宀€鍠栭幃褔宕奸悢鍝勫殥闂備礁鎲￠崹瑙勬叏閹绢喖鐓橀柟杈鹃檮閸嬫劙鎮楅崷顓炐㈡い銉︾箘缁辨挻鎷呴崫鍕碘偓宀勬煕閵娿儳鍩ｇ€殿喖顭锋俊鎼佸煛閸屾矮绨介梻浣呵归張顒傜矙閹达富鏁傞柨鐕傛嫹
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





//闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡宀€鍠栭幃褔宕奸悢鍝勫殥闂備礁鎲￠崹瑙勬叏閹绢喖鐓橀柟杈鹃檮閸嬫劙鎮楅崷顓炐㈡い銉︾箘缁辨挻鎷呴崫鍕碘偓宀勬煕閵娿儳鍩ｇ€殿喖顭烽弫鎰緞婵犲嫷鍚呴梻浣瑰缁诲倿骞夊☉銏犵缂備焦顭囬崢鎼佹⒑閸涘﹣绶遍柛鐘愁殜瀹曟劙骞囬悧鍫㈠幐閻庡厜鍋撻悗锝庡墰閻﹀牓鎮楃憴鍕閻㈩垱甯熼悘鎺楁煟閻愬鈻撻柍褜鍓欓崢鏍ㄧ珶閺囥垺鈷戦柛婵嗗閳诲鏌涚€Ｑ冧壕闂備胶枪椤戝棝骞愰幖渚婄稏婵犻潧顑愰弫鍡涙煕鐏炲墽鏁栭柕濞炬櫆閳锋垿鏌涘┑鍡楊伂妞ゎ偓绠撻弻娑㈠籍閳ь剟宕归崸妤冨祦闁告劦鍠栭～鍛存煏閸繃鍣圭紒鎰⊕缁绘繈鎮介棃娴躲垽鎮楀闂寸敖婵″弶鍔欏鎾閳锯偓閹风粯绻涙潏鍓у埌闁硅姤绮庣划鏃堟倻濡晲绨婚梺闈涱檧闂勫嫬鐣峰畝鈧埀顒冾潐濞叉﹢銆冮崨杈剧稏婵犲﹤鐗嗛悞鍨亜閹哄棗浜惧銈嗘穿缂嶄線銆侀弴銏℃櫇闁逞屽墰缁粯绻濆顓炰化闂佹悶鍎滈崘顏堢崜婵＄偑鍊戦崕鑼崲閸繍娼栨繛宸簼椤ュ牊绻涢幋鐐跺妞わ絽鎼埞鎴﹀煡閸℃ぞ绨煎銈冨妼閿曨亪濡存笟鈧顕€宕煎┑瀣暪闂備礁鎼ú銊╁疮閸ф绠繛宸簼閻撶喖鏌ｅΟ鍝勫笭闁煎壊浜弻娑㈠棘鐠恒劎鍔悗娈垮枟閹倿鐛€ｎ喗鏅滈柣锝呰嫰楠炴劙姊虹拠鎻掑毐缂傚秴妫欑粋宥夊醇閺囩喎浜楅梺绋跨箳閳峰牆鈻撴禒瀣厽闁归偊鍨伴惃铏圭磼閻樺樊鐓奸柡宀€鍠栭、娆撴偩瀹€鈧悡澶愭⒑閻熸壆锛嶉柛瀣ㄥ€栨穱濠囨倻閼恒儲娅嗙紓浣圭☉椤戝嫰骞嬮悜鑺モ拻闁稿本鑹鹃埀顒勵棑缁牊绗熼埀顒勭嵁閺嶎収鏁冮柨鏇楀亾缁炬儳缍婇弻娑㈩敃閿濆棛顦ョ紒鐐劤缂嶅﹪寮婚垾鎰佸悑閹肩补鈧尙鐖遍梻浣呵归鍡涘箰閹间緤缍栨繝闈涱儛閺佸棝鏌涚仦鍓ф晼闁靛ň鏅滈埛鎴︽煕濠靛棗顏い顐畵閺屾盯寮埀顒勫垂閸ф宓侀柛鎰靛枛椤懘鏌曢崼婵囧櫣缂佹劖绋掔换婵嬫偨闂堟刀銏ゆ倵濮橀棿绨芥俊鍙夊姍瀵挳濮€閳锯偓閹风粯绻涙潏鍓у埌闁硅姤绮庣划鏃堟倻濡晲绨婚梺闈涱檧闂勫嫬鐣峰畝鈧埀顒冾潐濞叉﹢銆冮崨杈剧稏婵犲﹤鐗嗛悞鍨亜閹哄棗浜惧銈嗘穿缂嶄線銆侀弴銏℃櫇闁逞屽墰缁粯绻濆顓炰化闂佹悶鍎滈崘顏堢崜婵＄偑鍊戦崕鑼崲閸繍娼栨繛宸簼椤ュ牊绻涢幋鐐跺妞わ絽鎼埞鎴﹀煡閸℃ぞ绨煎銈冨妼閿曨亪濡存笟鈧顕€宕煎┑瀣暪闂備礁鎼ú銊╁疮閸ф绠繛宸簼閻撶喖鏌ｅΟ鍝勫笭闁煎壊浜弻娑㈠棘鐠恒劎鍔悗娈垮枟閹倿鐛€ｎ喗鏅滈柣锝呰嫰楠炴劙姊虹拠鎻掑毐缂傚秴妫欑粋宥夊醇閺囩喎浜楅梺绋跨箳閳峰牆鈻撴禒瀣厽闁归偊鍨伴惃铏圭磼閻樺樊鐓奸柡灞稿墲閹峰懐鎲撮崟顐わ紦闂備浇妗ㄩ悞锕傚箲閸ヮ剙鏋侀柟鍓х帛閺呮悂鏌ㄩ悤鍌涘
/*闂傚倸鍊搁崐鎼佸磹閻戣姤鍤勯柛顐ｆ礀缁犵娀鏌熼崜褏甯涢柛瀣ㄥ€濋弻鏇熺箾閻愵剚鐝曢梺鍝勬噺缁诲牓寮婚弴锛勭杸閻庯綆浜栭崑鎾诲即閵忕姴鍤戞繛鎾村焹閸嬫捇鏌″畝鈧崰鏍嵁閹达箑绠涢梻鍫熺⊕椤斿嫭绻濈喊妯活潑闁稿鎳橀弫鍐閵堝懓鎽曢梺鍝勬储閸ㄥ綊鏌嬮崶銊х瘈闂傚牊绋掔粊鈺備繆椤愩倕浠滄い顏勫暣婵″爼宕ㄩ婊庡敹闂備胶绮悧婊堝储瑜旈敐鐐剁疀濞戞瑦鍎柣鐔哥懃鐎氼剛澹曢鐐粹拺闂傚牊鑳嗚ぐ鎺戠？闁哄被鍎查崐闈浳旈敐鍛殲闁抽攱鍨块弻娑樷攽閸℃浼岄梺绋块缁绘垿濡甸崟顖ｆ晣闁绘ɑ褰冮獮瀣⒑缂佹ü绶遍柛鐘崇〒缁鈽夊Ο閿嬵潔濠电偛妫楃换鍡涘磻閹惧绡€婵﹩鍘鹃崢楣冩⒑鐠団€冲箺閻㈩垱甯″畷婵嗏堪閸曨厾顔曢梺鍛婁緱閸嬪嫰鎮橀崣澶嬪弿濠电姴鍟妵婵堚偓瑙勬处閸嬪﹤鐣烽悢纰辨晝闁绘棁娓规竟鏇㈡⒑閸撴彃浜濇繛鍙夌墱婢规洝銇愰幒鎾跺幐閻庡箍鍎辨鍛婄閹扮増鐓曢悗锝庡亝鐏忣厽銇勯锝囩疄闁诡喗鐟╅、妤呭焵椤掑嫷鏁傞柨鐕傛嫹

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
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡宀€鍠栭幃褔宕奸悢鍝勫殥闂備礁鎲￠崹瑙勬叏閹绢喖鐓橀柟杈鹃檮閸嬫劙鎮楅崷顓炐㈡い銉︾箘缁辨挻鎷呴崫鍕碘偓宀勬煕閵娿儳鍩ｇ€殿喖顭烽弫鎰緞婵犲嫷鍚呴梻浣瑰缁诲倿骞夊☉銏犵缂備焦顭囬崢鎼佹⒑閸涘﹣绶遍柛鐘愁殜瀹曟劙骞囬悧鍫㈠幐閻庡厜鍋撻悗锝庡墰閻﹀牓鎮楃憴鍕閻㈩垱甯熼悘鎺楁煟閻愬鈻撻柍褜鍓欓崢鏍ㄧ珶閺囥垺鈷戦柛婵嗗閳诲鏌涚€Ｑ冧壕闂備胶枪椤戝棝骞愰幖渚婄稏婵犻潧顑愰弫鍡椕归敐鍥ㄦ珪妞ゃ垹鎳樺缁樻媴娓氼垱鏁柣蹇撶箲閻燂箓寮茬捄浣曟棃宕橀鍡欐殽闂備胶绮濠氬储瑜旈幃鐐哄垂椤愮姳绨婚梺鍦劋閸ㄧ敻顢旈埡鍛厸閻庯絺鏅濈粣鏃堟煛瀹€鈧崰鏍嵁閹达箑绠涢梻鍫熺⊕椤斿嫭绻濈喊妯活潑闁稿鎳橀弫鍐閵堝懓鎽曢梺鍝勬储閸ㄥ綊鏌嬮崶銊х瘈闂傚牊绋掔粊鈺備繆椤愩倕浠滄い顏勫暣婵″爼宕ㄩ婊庡晥闂備胶纭堕弲婊堟偋閻樿崵宓侀柛顐犲劚鎯熼梺瀹犳〃閼冲爼顢欐繝鍥ㄢ拺闁荤喖鍋婇崵鐔兼煕鐎ｎ剙鏋涘┑鈥崇埣閸╋繝宕ㄩ瑙勫缂傚倷绀侀鍡涱敄濞嗘挸纾块柟鎵閻撴瑩鏌ｉ悢鍝勵暭闁哥姵锕㈤弻锝呪槈閸楃偞鐏曠紓浣哄У缁嬫垿鍩ユ径濞炬瀻闊洤锕ゆ禍鎯р攽閻樺磭顣查柣鎾跺█閺岀喖顢橀悢椋庣懆闂佸憡姊圭划宥囨崲濞戙垹宸濇い鎰╁灮娴煎牆鈹戦纭锋敾婵＄偠妫勯悾鐑筋敃閿曗偓缁€瀣亜閹邦喖鏋庡ù婊堢畺閺屾盯顢曢敐鍡欘槬缂佺偓鍎崇紞濠囧蓟閳ユ剚鍚嬮幖绮光偓鑼埍闂備胶枪椤戝棝骞愰幖渚婄稏婵犻潧顑愰弫鍡涙煕鐏炲墽鏁栭柕濞炬櫆閳锋垿鏌涘┑鍡楊伂妞ゎ偓绠撻弻娑㈠籍閳ь剟宕归崸妤冨祦闁告劦鍠栭～鍛存煏閸繃鍣圭紒鎰⊕缁绘繈鎮介棃娴躲垽鎮楀闂寸敖婵″弶鍔欏鎾閳锯偓閹风粯绻涙潏鍓у埌闁硅姤绮庣划鏃堟倻濡晲绨婚梺闈涱檧闂勫嫬鐣峰畝鈧埀顒冾潐濞叉﹢銆冮崨杈剧稏婵犲﹤鐗嗛悞鍨亜閹哄棗浜惧銈嗘穿缂嶄線銆侀弴銏℃櫇闁逞屽墰缁粯绻濆顓炰化闂佹悶鍎滈崘顏堢崜婵＄偑鍊戦崕鑼崲閸繍娼栨繛宸簼椤ュ牊绻涢幋鐐跺妞わ絽鎼埞鎴﹀煡閸℃ぞ绨煎銈冨妼閿曨亪濡存笟鈧顕€宕煎┑瀣暪闂備礁鎼ú銊╁疮閸ф绠柛婵嗗濞撳鏌曢崼婵囶棞缂佹甯￠弻銊╁即椤忓嫷娲紓渚囧枟濡啴銆佸璺虹劦妞ゆ帒瀚拑鐔兼煃閳轰礁鏆炲┑顖涙尦閹嘲鈻庤箛鎿冧患缂佸墽鍋撴繛濠囧箖濡も偓閳绘捇宕归鐣屽蒋闂備礁鎲￠幐楣冨窗閹邦厾鈹嶅┑鐘叉搐闁卞洭鏌￠崶鈺佷粶闁兼澘鐏濋埞鎴炲箠闁稿﹥娲熼弫鍐晲閸ヮ煈娼熼梺鎸庢礀閸婂綊鍩涢幒妤佺厱閻忕偛澧介幊鍕磼娴ｅ搫顣肩紒缁樼⊕瀵板嫮鈧綆鍋嗛ˇ浼存倵鐟欏嫭纾婚柛妤佸▕閻涱喖顫滈埀顒€鐣峰⿰鍕闁惧繒鎳撴慨浠嬫⒒閸屾瑦绁版繛澶嬫礋瀹曟娊鏁冮崒姘鳖唵闂佽法鍣﹂幏锟�32闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡宀€鍠栭幃褔宕奸悢鍝勫殥闂備礁鎲￠崹瑙勬叏閹绢喖鐓橀柟杈鹃檮閸嬫劙鎮楅崷顓炐㈡い銉︾箘缁辨挻鎷呴崫鍕碘偓宀勬煕閵娿儳鍩ｇ€殿喖顭烽弫鎰緞婵犲嫷鍚呴梻浣瑰缁诲倿骞夊☉銏犵缂備焦顭囬崢鎼佹⒑閸涘﹣绶遍柛鐘愁殜瀹曟劙骞囬悧鍫㈠幐閻庡厜鍋撻悗锝庡墰閻﹀牓鎮楃憴鍕閻㈩垱甯熼悘鎺楁煟閻愬鈻撻柍褜鍓欓崢鏍ㄧ珶閺囥垺鈷戦柛婵嗗閳诲鏌涚€Ｑ冧壕闂備胶枪椤戝棝骞愰幖渚婄稏婵犻潧顑呯粻娑樏归敐鍛础缂佸顥撶槐鎾诲磼濞嗘埈妲銈嗗灥閹虫ǹ妫熷銈嗙墬缁苯顕ｉ崣澶夌箚闁绘劦浜滈埀顒佹礈閹广垽骞囬弶璺ㄧ枃闂婎偄娲﹂幐鎾几瀹ュ鐓忛煫鍥ь儏閳ь剚鐗犻幃锟犳偄閸忚偐鍘介梺鍝勫€圭€笛囧箟閹间焦鐓欓柛鎰絻椤忣參鏌熼鑽ょ煓妞ゃ垺绋戦埥澶娢熼弰蹇曟闂佽姘﹂～澶娒哄⿰鍫濇瀬閻犲洤妯婇崵鏇灻归悩宸剾闁轰礁娲弻锝夊箛椤撗冩櫛闂佸摜鍋熼弫璇差潖缂佹ɑ濯撮柧蹇曟嚀缁楋繝姊洪幖鐐插闁告艾顑夐獮鍫ュΩ閳轰絼褍顭跨捄鐚村姛闁哄睙鍥ㄢ拺鐟滅増甯楅敍鐔兼煟閹虹偟鐣甸柟顔光偓鏂ユ闁靛骏绱曢崢閬嶆煟韫囨洖浠掔紒鐘崇懇椤㈡棃宕煎┑鍫濆Е婵＄偑鍊栫敮濠囨嚄閼稿灚娅犻梺顒€绉甸悡鍐喐濠婂牆绀堟慨妯垮煐閸ゅ矂鏌涢幘妤€鎳愰ˇ顖涚節閻㈤潧孝婵炲眰鍊濋幏鎴︽偄閸忚偐鍘繝鐢靛仧閸嬫挸鈻嶉崱娑欑厱閻庯綆浜滈顓㈡煛鐏炲墽顬肩紒鐘崇洴楠炴﹢鎼归鈶╁亾椤栫偞鈷戦梻鍫氭櫇缁嬭鈹戦悙璇у伐妞ゎ偄绻愮叅妞ゅ繐瀚槐鍫曟⒑閸涘﹥澶勯柛妯荤矒婵偓闁挎稑瀚鏇㈡⒑閸濆嫷妲归柛銊ф暬椤㈡棃顢曢敐鍕畾闂佸憡鐟ラˇ顖涙叏閸ヮ剚鐓冮悷娆忓閻忓鈧娲栭悥濂稿春閳╁啰绡€闁告劦浜楅崑鎺楁⒒閸屾艾鈧娆㈠璺虹劦妞ゆ帒鍊告禒婊堟煠濞茶鐏￠柡鍛埣椤㈡稑饪伴崨顖ょ床闂佽鍑界紞鍡樼閻戠晫鍙曟い鎺戝€甸崑鎾舵喆閸曨剛顦ラ梺纭呭Г缁秶绮╅悢鐓庡嵆闁靛繒濮烽悿鍕磽娴ｇǹ绾ч柣鈺婂灠椤繑绻濆顒傦紲濠电偛妫欓崝妤呭Χ閺夊簱鏀介柣鎰▕濡茶绻涢懠顒€鏋庨柣锝夋敱鐎靛ジ寮堕幋鐘垫澑闂備礁鎼ˇ顖炴倶濠靛鍚归柟瀵稿仧缁♀偓闂佹眹鍨藉褎鐗庨梻浣藉亹閹虫挻鏅堕悾灞藉灊濠电姵鍑归弫宥嗙節婵犲倹鍣介柨娑欑矒閺岋綀绠涢弴鐐版埛闂佸搫鎷嬮崑濠囧箖閺夋鍤曢悗鐟扮獝闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡宀€鍠栭幃褔宕奸悢鍝勫殥闂備礁鎲￠崹瑙勬叏閹绢喖鐓橀柟杈鹃檮閸嬫劙鎮楅崷顓炐㈡い銉︾箘缁辨挻鎷呴崫鍕碘偓宀勬煕閵娿儳鍩ｇ€殿喖顭烽弫鎰緞婵犲嫷鍚呴梻浣瑰缁诲倿骞夊☉銏犵缂備焦顭囬崢鎼佹⒑閸涘﹣绶遍柛鐘愁殜瀹曟劙骞囬悧鍫㈠幐閻庡厜鍋撻悗锝庡墰閻﹀牓鎮楃憴鍕閻㈩垱甯熼悘鎺楁煟閻愬鈻撻柍褜鍓欓崢鏍ㄧ珶閺囥垺鈷戦柛婵嗗閳诲鏌涚€Ｑ冧壕闂備胶枪椤戝棝骞愰幖渚婄稏婵犻潧顑愰弫鍡涙煕鐏炲墽鏁栭柕濞炬櫆閳锋垿鏌涘┑鍡楊伂妞ゎ偓绠撻弻娑㈠籍閳ь剟宕归崸妤冨祦闁告劦鍠栭～鍛存煟濡櫣浠涙繛鍫熺箞濮婃椽宕崟鍨┑鐐差槹濞叉粌危閹扮増鏅濋柛灞剧▓閹风粯绻涙潏鍓у埌闁硅姤绮撳鎼佸礃閳衡偓缁诲棙銇勯幇鈺佺仼妞ゅ浚鍘介〃銉╂倷鐠囇囩反缂備胶濮甸惄顖氼嚕閺夋嚦鏃堝焵椤掍胶顩查柣鎰靛墯閸欏繑淇婇婊冨付濞存粓绠栭幃妤€顫濋悙顒€顏�
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
    //婵犵數濮烽弫鍛婃叏閻戣棄鏋侀柛娑橈攻閸欏繐霉閸忓吋缍戦柛銊ュ€婚幉鎼佹偋閸繄鐟查梺绋款儏椤︾敻寮婚弴锛勭杸閻庯綆浜栭崑鎾诲即閻樼數鐒奸柣搴秵閸犳鎮￠弴鐔翠簻妞ゆ挾鍠庨悘銉ッ瑰⿰鍕疄闁哄瞼鍠栭、娆撴偩鐏炴儳娅氭繝娈垮枛閿曪妇鍒掗鐐茬闁告侗鍨虫す鎶芥倵閿濆簼绨奸柣锝勫嵆濮婄粯鎷呴崨濠傛殘闂佽鐡曢褔锝炲┑瀣╅柍杞拌兌椤斿姊洪棃娑氱疄闁稿﹥娲熼悰顕€濮€閿涘嫮顔曢梺鐟邦嚟閸嬬喖骞婇崘顔界厱闁规崘顕栧鎰庨崶褝韬┑鈩冨劶缁犳盯寮撮悙鑼帓闂傚倷绀侀幗婊堝闯閵夆晛纾诲┑鐘插€婚弳锕傛煥濠靛棙顥犵紒鈾€鍋撻梻浣告啞閸斞呯磽濮樿精濮冲┑鐘崇閳锋垿鏌涘┑鍡楊伀鐞氼亪姊虹粙鍧楀弰濞存粌鐖煎畷娲焵椤掍降浜滈柟鍝勭Ф鐠愮増绻涢崼鐕佺劷缂佽鲸甯￠、娆撴嚃閳轰緡鏉搁柣搴＄仛濠㈡鈧凹鍠楃粋鎺楁晝閸屾氨鐣鹃悷婊冮叄閹銈ｉ崘鈹炬嫽婵炶揪缍€濞咃絿鏁☉娆嶄簻闁靛ǹ鍎查崵鍥煙椤斻劌瀚弧鈧梺鍛婃处娴滅偤宕滈妸銉富闁靛牆妫欑亸鐢告煕鎼淬垹濮嶉柟顔哄灲瀹曞崬鈻庨幇顒佺€鹃梻濠庡亜濞诧箑顫忚ぐ鎺戠煑闊洦绋掗埛鎺楁煕椤愩倕鏋嶇紒鍫曚憾閺岋綁骞掗弴鐕佹闂佸疇顫夐崹鍧楀箖閳哄啠鍋撻崷顓炐㈡い銉︾箘缁辨挻鎷呴崫鍕戯絿绱掔€ｎ偄娴┑锛勬暬瀹曠喖顢涘顒€鏁ら梻浣圭湽閸ㄥ寮灞炬噷闂傚倸鍊风粈渚€骞栭锕€纾归柣銏⑶圭壕濠氭煙閸撗呭笡闁抽攱甯￠弻娑氫沪閸撗勫櫘濡炪倧璁ｇ粻鎾诲蓟閻旂儤瀚氱憸蹇曞閸ф鐓涘〒姘搐閺嬫盯鎽堕弽顬″綊鏁愰崼鐕佷哗閻熸粈鍗崇粻鏍ь潖閾忕懓瀵查柡鍥╁仜閳峰姊洪棃娑欏闁烩晩鍨伴锝夊箹娴ｅ摜鐫勯梺鍓插亞閸犳捇宕㈤悽鍛婄厽閹艰揪绲鹃弳鈺呭几椤忓牆鍐€闁跨喓濮甸埛鎴︽煕濠靛嫬鍔氶柡瀣叄閺岀喓鍠婇崡鐐板枈濠电姭鍋撳〒姘ｅ亾婵﹥妞介獮鎰償閿濆洨鏆ゆ繝鐢靛仩椤曟粎绮婚幘宕囨殾闁汇垻枪閻掑灚銇勯幒鎴濃偓鑽ゅ閽樺褰掓晲閸ャ劌娈岀紓浣藉皺缁垶濡甸崟顖ｆ晣闁绘ɑ褰冮獮瀣倵濞堝灝鏋︽い鏇嗗洤鐓″璺号堥崼顏堟煕濞戝崬鐏℃繝銏″灴濮婅櫣鎷犻幓鎺戞瘣缂傚倸绉村Λ娆戠矉瀹ュ應鍫柛鏇ㄥ墮瀵潡姊洪棃娑氱濠殿噮鍙冨銊︾鐎ｎ偆鍘介梺褰掑亰閸ㄤ即鎯冮崫鍕电唵鐟滃酣鎯勯鐐茶摕婵炴垯鍨规儫闂侀潧锛忛崒婵囶€楅梻鍌欐缁鳖喚绱炴笟鈧棢闁规崘娉涢崹婵囩箾閸℃ê鐏﹂悗姘哺閺岋綁骞囬棃娑橆潽闂佹娊鏀遍崝娆撳蓟閻旈鏆嬮柣妤€鐗嗗▓妤呮倵鐟欏嫭绀冪紒顔芥崌閻涱噣宕堕鈧崹鍌涖亜閹扳晛鈧呮閻當n闂傚倸鍊搁崐鎼佸磹閹间礁纾瑰瀣椤愯姤鎱ㄥ鍡楀⒒闁绘帞鏅幉绋款吋閸澀缃曢梻鍌欑閹碱偊宕锔藉亱闊洦绋戦惌妤呮煛閸ャ儱鐏柣鎾跺枛閺岋綁骞囬鑺ユ瘎闂佸搫妫楅垾锕傛⒒閸屾艾鈧悂宕愰幖浣哥９濡炲娴烽惌鍡椼€掑锝呬壕濡ょ姷鍋涢ˇ鐢稿极瀹ュ绀嬫い鎰╁灮娴滀即鏌ｆ惔鈥冲辅闁稿鎹囬弻娑㈠即閵娿儱顫╂繛瀵稿Ь閸嬫劗妲愰幘瀛樺闁告挸寮舵晥濠电偛鐡ㄧ划鐘诲垂鐠轰警娼栧┑鐘宠壘绾惧吋绻涢崱妯虹仴濠碘€茬矙濮婂搫煤鐠佸磭鐩庣紓鍌氱Т閿曘倝鎮惧畡鎵虫斀闁糕剝鐟﹀▓鏇㈡⒑闁偛鑻晶鎾煕閳规儳浜炬俊鐐€栫敮濠勭矆娓氣偓瀹曠敻顢楅崟顒傚幈闂佸湱鍋撳娆撳传閾忓厜鍋撳▓鍨灍闁绘挴鈧磭鏆﹀┑鍌溓归崡鎶芥煟閺冨洦鑵规慨瑙勵殜濮婄粯鎷呴搹鐟扮闂佸憡姊归悧鐘茬暦閻熸壋鏀介柛顐ｇ箓閻忓﹪姊洪崷顓℃闁哥姵顨婇崺娑㈠箣閿旇棄浠┑鐐叉缁绘劙顢旈埡鍐＜闁肩⒈鍓涚敮娑㈡煏閸パ冾伃妤犵偛顑呴埞鎴﹀炊瑜忛悰鈺冪磽閸屾瑧璐伴柛鐘崇墵閹勭節閸屻倕娈梺鍛婃处閸ㄦ壆绮诲☉娆嶄簻闁规崘娉涘瓭濡ょ姷鍋為〃濠傤潖缂佹ɑ濯撮柛娑橈龚绾偓闂備胶枪缁ㄦ椽宕愰弴鐘插疾婵犵妲呴崹浼村触鐎ｎ喖绀傛繝濠傚悩閺冨牊鏅查柛娑卞幗濞堟煡姊虹紒妯诲鞍闁荤噦绠撴俊鐢稿礋椤栨氨鐫勯梺绋挎湰缁秹骞夊Ο琛℃斀闁绘劘灏欐晶娑氱磼椤斿ジ鍙勯柛鈹惧亾濡炪倖甯婄欢锟犲疮韫囨稒鐓曢柣妯虹－婢х敻鏌熼鍝勫姕缂佽櫣鏅划娆撳垂椤旈敮鍋撻鐔剁箚闁靛牆绻掗崚浼存煕閻曚礁浜伴柟顖氳嫰铻栭柍褜鍓熼垾锕傚锤濡や礁娈濋悗鐢靛椤ㄥ繑绂嶅┑鍫㈢焿闁圭儤鍤氬ú顏嶆晜闁告侗浜濈€氬ジ姊绘担鍝勫付妞ゎ偅娲熷畷鎰板即閻愬灚鎳冩繝鐢靛Х閺佹悂宕戦悙宸劷婵炲棙鎸哥壕褰掓煕椤垵鏋ら柡鍡畵閺屾洝绠涚€ｎ亖鍋撻弽顓炲惞鐎光偓閸曨剛鍘鹃梺鍛婃尰瑜板啴宕㈤崨濠冨弿閻熸瑥瀚峰▓婊勬叏婵犲懏顏犵紒杈ㄥ笒铻ｉ柛婵嗗濞兼捇姊绘担鍛婅础闁冲嘲鐗撳畷鎴炵節閸屾粍娈鹃梺鎸庣箓濡稓寮ч埀顒勬⒑閸涘﹤濮х紓鍌涙皑閼鸿鲸绻濆顓涙嫼闂佸憡绻傜€氼噣鍩㈡径鎰厱閻庯綆浜滈顏嗙磼閸屾稑娴柡浣稿暣閸┾偓妞ゆ帒瀚粻鎺楁⒒娴ｈ櫣甯涙い顓炴喘椤㈡寮婚妷锕€娈岄梺绉嗗嫷娈曢柣鎾寸懄閵囧嫰寮埀顒勫磿閹惰棄鍌ㄩ悗鐢电《閸嬫挸鈻撻崹顔界亶濠电偛鍚嬮悷鈺呮晲閻愭祴鏀介柛銉ㄥ煐缂嶆垿姊洪崫鍕犻柛鎾冲船楗即宕熼鐘垫闂備焦鐪归崹钘夘焽瑜戦埅鎾⒒娴ｅ懙褰掝敄閸℃稑绠板┑鐘宠壘妗呴梺鍛婃处閸犳岸鎮块埀顒勬⒑閸︻厼浜炬繛鍏肩懇瀹曟垵顓奸崱娆戭啎闁诲孩绋掗…鍥儗婵犲嫮纾界€广儱鎷戦煬顒傗偓娈垮枛椤兘骞冮姀銈嗗亗閹艰揪绲块弳浼存煟閻斿摜鐭婄紒澶屾嚀椤曪綁鎼归锝囩Ф闂侀潧饪电徊鑲╄姳婵犳碍鈷戦梻鍫熺〒婢ф洟鏌熼崘鑼鐎规洜鏁诲畷鍫曨敆娴ｅ搫骞嶆俊鐐€栧褰掑磿閹惰棄鍌ㄩ悗鐢电《閸嬫挸鈻撻崹顔界亶濠电偛鍚嬮悷鈺呮晲閻愭祴鏀介悗锝庝簽椤㈠懘姊虹紒妯哄婵☆垰锕幃妤呭箻椤旇В鎷洪梺鐓庮潟閸婃洟寮搁幋鐘电＜妞ゆ棁鍋愬瓭濡炪値鍘奸悘婵嬶綖濠婂牆鐒垫い鎺嗗亾闁伙絿鍏橀、娑㈡倷閼碱剟鐛撻梻浣烘嚀椤曨厽鎱ㄩ棃娑掓灁闁诡垎鈧弨浠嬪箳閹惰棄纾归柡鍥ュ灩缁犵娀骞栫划鐟板⒉闁哄棙绮撳娲嚒閵堝懏鐎剧紓渚囧枛閻倿寮绘繝鍥ㄦ櫜濠㈣泛锕﹂悾鐑樹繆閵堝繒鍒伴柛鐕佸灦閹偤鎳栭埞鎯т壕妤犵偛鐏濋崝姘繆椤愶絿娲寸€规洏鍨介弻鍡楊吋閸℃ぞ鐢绘繝鐢靛Т閿曘倝宕幘顔肩煑闁告洦鍨遍悡蹇涙煕閳╁喚娈旈柡鍡欏仧閳ь剙鐏氬妯尖偓姘煎枟缁傛帡鏁冮崒姘辩暰閻熸粌绉堕懞閬嶅Ψ閿斿墽鐦堢紒鍓у钃辨い顐躬閺屾盯濡搁妶鍛ギ閻庤娲忛崕鎶藉焵椤掑﹦绉甸柛鐘崇墱婢规洜绱掑Ο鍦畾濡炪倖鐗楀銊バ掗悙鐑樼厱閻庯綆鍋呭畷宀€鈧娲橀〃鍡楊嚗閸曨剛绡€濞达絽澹婂Λ婊堟⒒閸屾艾鈧娆㈠璺虹劦妞ゆ帒鍊告禒婊堟煠濞茶鐏￠柡鍛埣椤㈡岸鍩€椤掑嫬钃熺€广儱娲ㄧ壕鍏间繆椤栨繍鍤欑痪鎯у暱铻栭柣姗€娼ф禒婊勩亜閹存繍妯€鐎殿噮鍋婂畷姗€顢欓懖鈺佸Е婵＄偑鍊栫敮鎺楀疮閹殿喚涓嶅ù鐘差儐閳锋帡鏌涚仦鍓ф噭缂佷胶澧楃换娑欏緞鐎ｎ偆顦伴悗瑙勬礃婵炲﹪寮崘顔肩＜婵﹢纭搁崬鐢告⒒娴ｈ姤纭堕柛锝忕畵楠炲繘鏁撻敓锟�
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞妞ゆ帒锕︾粙蹇旂節閵忥絾纭炬い鎴濇喘閵嗗懘骞撻幑妤€缍婇幃鈺侇啅椤旂厧澹堢紓浣哄亾閸庡啿顭囬敓鐘茶摕婵炴垯鍨圭粻娑欍亜閹哄秷顔夐柡瀣懇濮婅櫣绱掑Ο铏圭憪缂備緡鍠楅悷鈺侇嚕鐠囨祴妲堥柕蹇曞Х閻も偓闂備胶枪缁绘劙骞楀⿰鍫濈鐟滅増甯楅埛鎴︽煕濠靛嫬鍔氶柡瀣叄閺岀喓鍠婇崡鐐板枈婵犵鍓濋幐鎶界嵁閹烘绠ｆい鎾跺枎閸忓﹪姊绘担鐟邦嚋缂佽鍊块獮濠傤吋閸℃瑧褰鹃梺鍝勬储閸ㄦ椽鍩涢幒鎴欌偓鎺戭潩閿濆懍澹曢梻渚€鈧偛鑻晶浼存煕鐎ｎ偆娲撮柟宕囧枛椤㈡稑鈽夊▎鎰娇婵＄偑鍊栭悧婊堝磻閻愮儤鍋傞柡鍥ュ灪閻撴盯鏌涢幇鍓佸埌濞存粓绠栧铏圭磼濮楀棙鐣兼繝鐢靛亹閸嬫捇姊洪棃娑欘棛缂佲偓娴ｇ晫浜欓梻浣告啞娓氭宕伴弽顓炵劦妞ゆ帒鍊搁崢鎾煛鐏炲墽鈽夐柍瑙勫灴瀹曞崬螖婵犱胶纾诲┑锛勫亼閸婃牠寮婚妸鈺佺疇闊洦娲樺畷鍙夌節闂堟侗鍎愰柛瀣ㄥ姂濮婂宕奸悢宄扮€梺闈涚箞閸婃牠鎮￠弴鐐╂斀闁绘ê寮堕崳褰掓煕濮橆剚璐＄紒杈ㄥ浮椤㈡瑩鎳為妷銏″缂傚倷娴囧銊у垝濞嗗繒鏆﹂柕濠忓缁♀偓闂佸憡娲﹂崑鍕不閻愮儤鈷掗柛灞剧懄缁佺増銇勯銏╂█鐎规洘娲栭悾鐑藉炊瑜滃Λ婊冾渻閵堝棙灏甸柛瀣仜琚欓柛鏇ㄥ亐閺€浠嬫煥濞戞ê顏╁ù婊冦偢閺屾稒绻濋崘銊ヮ潚閻庢鍠楅悡锟犮€侀弮鍫濆耿婵炲棗娴烽悷婵嬫⒒娴ｇ瓔娼愮€规洘锚閻ｇ兘妫冨☉姘婵炴挻鍩冮崑鎾绘煛瀹€鈧崰鏍嵁閹达箑绠涢梻鍫熺⊕椤斿嫮绱撻崒娆掑厡濠殿喖鐏氱换娑欑節閸パ嗘憰闂佺粯妫侀妴鈧柛瀣尭閳藉鈻庡Ο鐓庡Ш缂傚倷鑳舵慨鐢稿箰婵犳艾绠為柕濞у嫬鏋傞梺鍛婃处閸樼厧顕ｉ妸銉㈡斀妞ゆ梻銆嬪銉╂煙绾板崬浜濋柡鍛劦濮婃椽骞栭悙鎻掑Η闂佸憡鍔曟晶浠嬪焵椤掍礁鈻曟慨濠呮閹风娀骞撻幒婵嗗Ψ闂備礁鎲￠崹鐢电礊婵犲倻鏆︽繝濠傚暊閺嬪酣鏌熺紒銏犵仚闁稿鎹囬弫鍐磼濮樺崬鈧偛顪冮妶鍡楃瑐闁绘帪绠戦埢宥夊冀椤撶啿鎷洪梺鍛婄☉閿曘儲寰勯崟顖涚厱闁规儳顕幊鍥ㄣ亜閵忊剝灏柍钘夘樀婵偓闁抽敮鍋撻柟閿嬫そ濮婃椽宕ㄦ繝鍕ㄦ闂佹寧娲忛崕鎶藉焵椤掆偓閻忔岸鎮烽埡鍛摕婵炴垶菤濡插牓鏌涘Δ鍐ㄤ沪闁诲繑鎹囧娲焻閻愯尪瀚板褍顭烽弻娑樜熼崗鍏肩彧闂佺懓绠嶉崹褰掑煡婢舵劕顫呴柣妯活問閸氬懘姊绘担铏瑰笡闁告梹鐗曢…鍥р枎閹捐櫕杈堝┑掳鍊曢幊蹇涘煕閹寸偞鍙忛柣鐔哄閹兼劕鈹戦垾铏仴闁诡喗枪缁犳盯骞樻０婵囨n闂傚倸鍊搁崐鎼佸磹閹间礁纾瑰瀣捣閻棗銆掑锝呬壕濡ょ姷鍋為〃鍛淬偑娴兼潙閱囨繝闈涚墢瀹曞爼姊绘担铏广€婇柛鎾寸箘缁瑩骞嬮悩宸闂佺鍕垫畷闁绘挻娲熼弻鐔兼焽閿曗偓楠炴﹢鏌曢崼鐔稿唉闁哄矉绱曢埀顒婄秵閸嬪懘藟閸儲鐓涚€光偓閳ь剟宕伴幇顒夌劷闊洦鏌ｉ崑鍛存煕閹般劍娅撻柍褜鍓欑粔鐟邦潖濞差亜浼犻柛鏇ㄥ墮椤庢盯姊洪崨濠冨鞍闁烩晩鍨跺畷娲焵椤掍降浜滈柟鍝勬娴滈箖姊洪崨濞氭垹鍒掗幘宕囨殾闁硅揪绠戠粻鑽ょ磽娴ｈ鐒介柛姗€浜跺楦裤亹閹烘垳鍠婇梺鍛娒妶绋跨暦濠靛围濠㈣泛顑囬崢鎼佹⒑閸涘﹦鐭嗙紒鈧笟鈧悰顔嘉旈崘顏嗭紲缂傚倷鐒﹂…鍥╃不閻愮繝绻嗘い鎰╁灩閺嗘瑩鏌嶉挊澶樻Ц閾伙絿鈧懓瀚伴。锔界珶閺囥垺鈷戦柤鎭掑剭椤忓牊鏅璺号堝Σ鍫ユ煏韫囨洖啸妞わ附婢橀—鍐Χ閸℃瑥顫у┑顔角滈崝鎴﹀箖妤ｅ啫浼犻柛鏇樺妽閺傗偓婵＄偑鍊栧濠氬Υ鐎ｎ喖缁╃紓浣骨滄禍婊堟煃閸濆嫬鏆曢柛鎺戭殔ode


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
    cond->getSymPtr()->changeType(TypeSystem::boolType);
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
    

    IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry *>(id->getSymPtr());
    if(se->isGlobal())
    {
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
        if (initVal != nullptr)
        {
            initVal->genCode();
        }
        g->output();
    }
    else if(se->isLocal())
    {
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
                                               // set the addr operand in symbol entry so that we can use it in subsequent code generation.
    }

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
        //SymbolEntry* se = new ConstantSymbolEntry(TypeSystem::intType, -1);
        //Constant* temp_const = new Constant(se); 
        //Operand* src1=temp_const->getOperand();
        Operand* src2 = expr->getOperand();
        Operand *temp=src2;
        int opcode = UnaryInstruction::SUB;
        // if(src2->getType()->isBool())
        // {
        //     std::cout<<src2->toStr()<<std::endl;
        //     std::cout<<"fuck"<<std::endl;
        //     SymbolEntry *s = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        //     temp = new Operand(s);
        //     new ZextInstruction(temp, src2, builder->getInsertBB());
        // }
        //std::cout<<"fuck22"<<std::endl;
        //new BinaryInstruction(opcode, dst, src1, src2, builder->getInsertBB());
         new UnaryInstruction(opcode,dst,temp,builder->getInsertBB());
    }
    else if( op==NOT)
    {
        // if(锛�2)
        // !(bool)
        
        if(expr->getSymPtr()->getType()->isBool())
        {
             new NotInstruction(this->dst,expr->getOperand(), builder->getInsertBB());
        }
        else
        {
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
    if (dst->getType()->isBool())
    {
        BasicBlock* temp_bb = nullptr;// = new BasicBlock(func);
        CondBrInstruction* temp_cb = new CondBrInstruction(temp_bb, temp_bb, dst, builder->getInsertBB());
        i_true_list.emplace_back(temp_cb);
        i_false_list.emplace_back(temp_cb);
    }
    

}



//闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡宀€鍠栭幃褔宕奸悢鍝勫殥闂備礁鎲￠崹瑙勬叏閹绢喖鐓橀柟杈鹃檮閸嬫劙鎮楅崷顓炐㈡い銉︾箘缁辨挻鎷呴崫鍕碘偓宀勬煕閵娿儳鍩ｇ€殿喖顭烽弫鎰緞婵犲嫷鍚呴梻浣瑰缁诲倿骞夊☉銏犵缂備焦顭囬崢鎼佹⒑閸涘﹣绶遍柛鐘愁殜瀹曟劙骞囬悧鍫㈠幐閻庡厜鍋撻悗锝庡墰閻﹀牓鎮楃憴鍕閻㈩垱甯熼悘鎺楁煟閻愬鈻撻柍褜鍓欓崢鏍ㄧ珶閺囥垺鈷戦柛婵嗗閳诲鏌涚€Ｑ冧壕闂備胶枪椤戝棝骞愰幖渚婄稏婵犻潧顑愰弫鍡涙煕鐏炲墽鏁栭柕濞炬櫆閳锋垿鏌涘┑鍡楊伂妞ゎ偓绠撻弻娑㈠籍閳ь剟宕归崸妤冨祦闁告劦鍠栭～鍛存煏閸繃鍣圭紒鎰⊕缁绘繈鎮介棃娴躲垽鎮楀闂寸敖婵″弶鍔欏鎾閳锯偓閹风粯绻涙潏鍓у埌闁硅姤绮庣划鏃堟倻濡晲绨婚梺闈涱檧闂勫嫬鐣峰畝鈧埀顒冾潐濞叉﹢銆冮崨杈剧稏婵犲﹤鐗嗛悞鍨亜閹哄棗浜惧銈嗘穿缂嶄線銆侀弴銏℃櫇闁逞屽墰缁粯绻濆顓炰化闂佹悶鍎滈崘顏堢崜婵＄偑鍊戦崕鑼崲閸繍娼栨繛宸簼椤ュ牊绻涢幋鐐跺妞わ絽鎼埞鎴﹀煡閸℃ぞ绨煎銈冨妼閿曨亪濡存笟鈧顕€宕煎┑瀣暪闂備礁鎼ú銊╁疮閸ф绠繛宸簼閻撶喖鏌ｅΟ鍝勫笭闁煎壊浜弻娑㈠棘鐠恒劎鍔悗娈垮枟閹倿鐛€ｎ喗鏅滈柣锝呰嫰楠炴劙姊虹拠鎻掑毐缂傚秴妫欑粋宥夊醇閺囩喎浜楅梺绋跨箳閳峰牆鈻撴禒瀣厽闁归偊鍨伴惃铏圭磼閻樺樊鐓奸柡宀嬬磿娴狅妇鎷犻幓鎺濈€烽梻渚€鈧偛鑻晶鍓х磽瀹ュ嫮顦︽い顓炴穿椤﹀磭绱掗弮鍌氭瀻妞ゎ偅绮撻崺鈧い鎺嗗亾妞ゎ偄绻橀幖褰掑捶椤撶姷鍘梻浣告啞閻擃參濡堕崨顔煎壍婵犵數濮烽。钘壩ｉ崨鏉戠；闁告洦鍋掗悞浠嬫煥閻斿搫啸鐎规挷绀侀…鍧楁嚋闂堟稑顫岄柟顖滃枛濮婃椽宕橀崣澶嬪創闂佺ǹ锕ら…宄扮暦閿濆骞㈡繛鎴炵憿閹疯櫣绱撻崒娆戝妽閽冮亶鏌ｉ幘鍗炲姦闁哄瞼鍠撻幏鐘侯槾缂佲檧鍋撻梻浣筋嚃閸犳鏁冮姀銈冣偓浣割潩鐠鸿櫣鍔﹀銈嗗笒鐎氼剟鎷戦悢鍏肩厽闁哄倸鐏濋ˉ蹇斾繆閹绘帞绉烘鐐寸墱閸掓帡宕楁径濠佸闂佸憡鍔樼亸娆擃敇濞差亝鈷掗柛灞剧懄缁佺増淇婂鐓庡闁诡喚鍋ら弫鍐磼濮橀硸鍞归梻浣规偠閸庢粎浠﹂懞銉悪濠碉紕鍋戦崐鏍暜閹烘柡鍋撳鐓庡婵炴彃娼″缁樻媴閾忓箍鈧﹪鏌涢幘瀵哥疄闁靛棗鍟崇粻娑㈠棘濞嗘儳娈奸梻浣烘嚀婢х晫鍒掗鐐茬厱闁圭儤鍤氳ぐ鎺撴櫜闁割偅绮抽幘鍓佹／妞ゆ挾鍋熼崺锝夋煛鐏炲墽娲撮柡浣瑰姌缁犳盯寮撮悩铏啌濠德板€楁慨鐑藉磻濞戞碍宕叉慨妞诲亾鐎殿噮鍋婇獮妯肩磼濡粯顏熼梻浣芥硶閸ｏ箓骞忛敓锟�?


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
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡宀€鍠栭幃褔宕奸悢鍝勫殥闂備礁鎲￠崹瑙勬叏閹绢喖鐓橀柟杈鹃檮閸嬫劙鎮楅崷顓炐㈡い銉︾箘缁辨挻鎷呴崫鍕碘偓宀勬煕閵娿儳鍩ｇ€殿喖顭烽弫鎰緞婵犲嫷鍚呴梻浣瑰缁诲倿骞夊☉銏犵缂備焦顭囬崢鎼佹⒑閸涘﹣绶遍柛鐘愁殜瀹曟劙骞囬悧鍫㈠幐閻庡厜鍋撻悗锝庡墰閻﹀牓鎮楃憴鍕閻㈩垱甯熼悘鎺楁煟閻愬鈻撻柍褜鍓欓崢鏍ㄧ珶閺囥垺鈷戦柛婵嗗閳诲鏌涚€Ｑ冧壕闂備胶枪椤戝棝骞愰幖渚婄稏婵犻潧顑愰弫鍡涙煕鐏炲墽鏁栭柕濞炬櫆閳锋垿鎮归崶锝傚亾瀹曞洣鍝楅梺璇插閻噣宕￠幎鑺ュ仒妞ゆ梻鈷堝銊╂煃瑜滈崜鐔煎Υ娴ｇ硶鏋庨柟鐐綑濞堢喖姊洪棃娑辨閻庨潧鏈粋鎺曨樄婵﹤顭峰畷鎺戔枎閹搭厽袦闂備礁婀遍埛鍫ュ磻婵犲倻鏆﹂柟鐗堟緲闁卞洭鏌￠崶鈺佷户闁挎稒鐟╁娲传閸曨厸鏋嗛梺绋款儌閸嬫捇姊虹粙娆惧剱闁瑰憡鎮傞敐鐐测攽鐎ｅ灚鏅㈤梺绋挎湰缁ㄤ粙濡搁埡鍌楁嫼闂佸憡绻傜€氼垶锝為敃鍌涚厱闁哄啠鍋撻柛銊ユ健閻涱噣宕橀纰辨綂闂侀潧鐗嗛幊搴ｇ玻濞戞瑧绡€闁汇垽娼у瓭闁诲孩鍑规禍鐐参ｉ幇鏉垮嵆闁靛繆妾ч幏缁樼箾鏉堝墽鍒伴柟鑺ョ矌缁棃鎮滃Ο闀愮盎闂侀潧顧€闂勫嫬鐣峰畝鈧埀顒冾潐濞叉﹢銆冮崨杈剧稏婵犲﹤鐗嗛悞鍨亜閹哄棗浜惧銈嗘穿缂嶄線銆侀弴銏℃櫇闁逞屽墰缁粯绻濆顓炰化闂佹悶鍎滈崘顏堢崜婵＄偑鍊戦崕鑼崲閸繍娼栨繛宸簼椤ュ牊绻涢幋鐐跺妞わ絽鎼埞鎴﹀煡閸℃ぞ绨煎銈冨妼閿曨亪濡存笟鈧顕€宕煎┑瀣暪闂備礁鎼ú銊╁疮閸ф绠繛宸簼閻撶喖鏌ｅΟ鍝勫笭闁煎壊浜弻娑㈠棘閼愁垰顏梺瀹狀嚙缁夊綊鐛崶顒佸亱闁割偁鍨归獮宥夋⒒娴ｅ憡鍟為柛顭戝灦瀹曟劙寮介鐔蜂壕婵鍋撶€氾拷


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
                //闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡宀€鍠栭幃褔宕奸悢鍝勫殥闂備礁鎲￠崹瑙勬叏閹绢喖鐓橀柟杈鹃檮閸嬫劙鎮楅崷顓炐㈡い銉︾箘缁辨挻鎷呴崫鍕碘偓宀勬煕閵娿儳鍩ｇ€殿喖顭烽弫鎰緞婵犲嫷鍚呴梻浣瑰缁诲倿骞夊☉銏犵缂備焦顭囬崢鎼佹⒑閸涘﹣绶遍柛鐘愁殜瀹曟劙骞囬悧鍫㈠幐閻庡厜鍋撻悗锝庡墰閻﹀牓鎮楃憴鍕閻㈩垱甯熼悘鎺楁煟閻愬鈻撻柍褜鍓欓崢鏍ㄧ珶閺囥垺鈷戦柛婵嗗閳诲鏌涚€Ｑ冧壕闂備胶枪椤戝棝骞愰幖渚婄稏婵犻潧顑嗛崐閿嬨亜閹哄棗浜惧銈呴獜閹凤拷
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
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡宀€鍠栭幃褔宕奸悢鍝勫殥闂備礁鎲￠崹瑙勬叏閹绢喖鐓橀柟杈鹃檮閸嬫劙鎮楅崷顓炐㈡い銉︾箘缁辨挻鎷呴崫鍕碘偓宀勬煕閵娿儳鍩ｇ€殿喖顭烽弫鎰緞婵犲嫷鍚呴梻浣瑰缁诲倿骞夊☉銏犵缂備焦顭囬崢鎼佹⒑閸涘﹣绶遍柛鐘愁殜瀹曟劙骞囬悧鍫㈠幐閻庡厜鍋撻悗锝庡墰閻﹀牓鎮楃憴鍕閻㈩垱甯熼悘鎺楁煟閻愬鈻撻柍褜鍓欓崢鏍ㄧ珶閺囥垺鈷戦柛婵嗗閳诲鏌涚€Ｑ冧壕闂備胶枪椤戝棝骞愰幖渚婄稏婵犻潧顑愰弫鍡椕归敐鍥ㄦ珪妞ゃ垹鎳樺缁樼瑹閳ь剟鍩€椤掑倸浠滈柤娲诲灡閺呭爼顢欓悾宀€鐦堥梺閫炲苯澧撮柡灞芥椤撳ジ宕ㄩ銈囧耿闂傚倷绶氬褔鎮ч崱娴板洭顢涢悙鎻掑殤婵炴挻鍩冮崑鎾绘煛瀹€鈧崰鏍嵁閹达箑绠涙い鎺戝€哥敮鍧楁⒒娴ｅ懙褰掝敄閸℃碍鎳屽┑鐑囩到濞层倝鏁冮鍛箚闁割偅娲栧钘壝归敐鍤借绔熺€ｎ剛纾介柛灞剧懅椤︼附銇勯敂璇茬仸闁诡喓鍎茬缓鐣岀矙閼愁垱鎲伴梻浣虹帛濡啴藟閹捐姹查悗锝庡枟閻撴稑顭跨捄渚剰妞ゆ洘绮嶇粋宥呪槈閵忥紕鍘介梺缁樻煥閹芥粓鎯岀€ｎ亖鏀介柍鈺佸暞閸婃劗鈧娲栭悥鍏间繆濮濆矈妲婚悗娈垮枟婵炲﹤顫忕紒妯诲闁惧繒鎳撶粭鈥斥攽椤旂》鍔熼柟铏崌閳ユ棃宕橀鐓庣€銈嗘礀閹虫劙宕濋悜鑺モ拺闁圭ǹ娴烽埥澶愭倵濮樼厧鏋涢摶鐐淬亜閺嶎偄浠﹂柣鎾寸☉椤法鎹勬笟顖氬壘闂佺ǹ绨洪崕鍙夌┍婵犲洤绠甸柟鐑樻⒒妤旈梻浣筋嚃閸犳銆冮崨杈剧稏婵犻潧顑愰弫鍡涙煕鐏炲墽鏁栭柕濞炬櫆閳锋垿鏌涘┑鍡楊伂妞ゎ偓绠撻弻娑㈠籍閳ь剟宕归崸妤冨祦闁告劦鍠栭～鍛存煏閸繃鍣圭紒鎰⊕缁绘繈鎮介棃娴躲垽鎮楀闂寸敖婵″弶鍔欏鎾閳锯偓閹风粯绻涙潏鍓у埌闁硅姤绮庣划鏃堟倻濡晲绨婚梺闈涱檧闂勫嫬鐣峰畝鈧埀顒冾潐濞叉﹢銆冮崨杈剧稏婵犲﹤鐗嗛悞鍨亜閹哄棗浜惧銈嗘穿缂嶄線銆侀弴銏℃櫇闁逞屽墰缁粯绻濆顓炩偓鐢告⒒閸喓鈯曢柟鍙夋倐閺岀喖鎼归銈囩杽濠殿喖锕ュ钘壩涢崘銊㈡閺夊牄鍓遍妶澶嬧拺缂佸娼￠妤冣偓瑙勬处閸撶喖宕洪姀銈呯閻犲洩灏欓崢鎼佹⒑閸涘﹣绶遍柛鐘冲哺瀹曘垽宕归锛勭畾闂佺粯鍔曞Ο濠囧磿韫囨拹鏃堟偐閾忣偄鈧劗鈧娲忛崹浠嬬嵁濮椻偓椤㈡瑩鎮剧仦钘夌婵犵數鍋犻幓顏嗙礊閳ь剚銇勯銏╂Ц闁伙絽鍢查悾婵嬪焵椤掑嫬鐒垫い鎺嶇贰閸熷繘鏌涢敐搴℃珝鐎规洘绮撻幃銏ゆ⒐閹邦喚鐣鹃梻浣稿閸嬪懎煤閺嶎厾宓侀柕蹇娾偓鑼畾闂侀潧鐗嗛幏鎴濐潖濡ゅ啰纾奸柟鎻掝儑閻ｅ灚鎱ㄦ繝鍌ょ吋鐎规洘甯掗埢搴ㄥ箛椤斿搫浠掑┑鐘垫暩閸嬫盯藝閺夋５娲偄妞嬪孩娈炬繛鏉戝悑濞兼瑧绮诲☉銏＄厸濠㈣泛锕︽禒銏ゆ煕閵堝棗鐏︾紒缁樼箞閹粙妫冨ù璁圭秮閺屻倝鎮烽弶搴撴寖缂備緡鍠栭…鐑姐€佸☉銏″€烽悗鐢登归獮妤呮⒒娴ｄ警鏀伴柟娲讳簽瀵板﹪宕愰悤浣规瘣闂傚倸鍊风粈渚€骞栭锕€绠犻煫鍥ㄧ☉缁犳牠鏌涘畝鈧崑娑㈡嫅閻斿吋鐓ラ柣鏂挎惈瀛濈紒鐐劤閸氬绌辨繝鍥舵晬婵犻潧瀚ч崑鎾诲焵椤掑嫭鐓曢悗锝庡亝瀹曞瞼鈧鍠曠划娆撱€侀弴銏″亜闁炬艾鍊搁ˉ姘節濞堝灝鏋熼柨鏇楁櫊瀹曟粌鈽夊杈╃厠濠电偛妯婃禍婵嬫偂閻斿摜绠鹃柟瀛樼箓閼歌绻涢崨顓犘ら柍褜鍓氶鏍窗濡ゅ懎绠伴柧蹇ｅ亝閸欏繘鏌嶈閸撶喖寮诲澶婄厸濞达絽鎲″▓鎻掝渻閵堝懐绠抽柛鐘崇墪椤繒绱掑Ο璇差€撻梺鍛婄☉閿曘儵宕曢幘缁樷拺缂佸灏呭銉╂煟閺嶎偄甯堕柣锝囧厴楠炲洭寮堕崹顔兼暏婵＄偑鍊栭崝妤呭窗鎼淬垻顩查柣鎰靛墯閸欏繑鎱ㄥΔ鈧Λ妤佹櫠閹殿喚纾煎璺侯儐閵囨繈鏌″畝瀣？濞寸媴濡囬幏鐘诲箵閹烘繃缍嗛梻鍌欐祰椤曟牠宕伴幘璇插瀭闁芥ê顦遍弳锕傛煏婵犲繐顩紒鈾€鍋撻梻浣告啞濞诧附绂嶉悙闈涱棜闁秆勵殕閳锋帡鏌涚仦鍓ф噮妞わ讣濡囩槐鎾愁吋閸滃啳鍚柦妯煎枛閺岀喖骞戦幇闈涙缂備讲鍋撻柛灞剧⊕閸欏繑淇婇悙棰濆殭濞存粓绠栭幃妤€鈻撻崹顔界彯闂佺ǹ顑呴敃顏堟偘椤斿槈鐔煎礂閻撳海锛忛梻浣瑰劤缁绘劕锕㈤柆宥嗗仱婵せ鍋撴慨濠勭帛閹峰懘鎼归悷鎵偧闂備胶绮〃鍡楊潖鐠囨祴鏋庨柕蹇曞濞尖晠鎮瑰ú顏嗙窗闁硅姤娲熷娲濞淬儱鐗撳鎻掆堪閸繄锛涢梺鍛婃处閸ㄩ亶鎮￠弴鐔虹闁瑰鍎戦崗顒勬煕閺冨倸鏋庨棁澶愭煟閹炬娊顎楁い蹇ｅ幗閹便劍绻濋崘鈹夸虎閻庤娲滈崗姗€銆佸鈧幃銏ゅ传閸曨倣顐⑩攽閻樺灚鏆╅柛瀣仱楠炴劗绮欓崠陇鈧潡寮堕崼姘珔闁搞劍绻堥弻宥夊传閸曨剙娅ｇ紓浣哄Х缁垶濡甸崟顖氱睄闁稿本绋掗悵顏堟⒑閸涘﹦鎳冩俊顐ｇ箓椤繘鎼归崷顓狅紲濠碘槅鍨靛畷鐢稿矗閸℃稒鈷戠痪顓炴媼閸ゅ绱掔紒妯肩疄妤犵偛鍟～婊堝焵椤掑嫨鈧礁顫濈捄铏瑰姦濡炪倖甯掔€氼剟鎷戦悢鍏肩厽闁哄倸鐏濋ˉ蹇斾繆閹绘帞绉烘鐐寸墱閸掓帡宕楁径濠佸闂佸憡鍔樼亸娆擃敇濞差亝鈷掗柛灞剧懄缁佺増淇婂鐓庡闁诡喚鍋ら弫鍐磼濮橀硸鍞归梻浣规偠閸庢粎浠﹂懞銉悪濠碉紕鍋戦崐鏍暜閹烘柡鍋撳鐓庡婵炴彃娼″缁樻媴閾忓箍鈧﹪鏌涢幘瀵哥疄闁靛棗鍟崇粻娑㈠棘濞嗘儳娈奸梻浣烘嚀婢х晫鍒掗鐐茬厱闁圭儤鍤氳ぐ鎺撴櫜闁割偅绮抽幘鍓佹／妞ゆ挾鍋熼崺锝夋煛鐏炲墽娲撮柟顔规櫊瀹曟﹢骞撻幒婵囩稈闂佽瀛╅鏍窗閺嶎厼绠熼柨娑樺瀹曞弶绻濋棃娑欙紞婵炲皷鏅滈妵鍕箳閹存繀绨介梺鍝ュ仧閺佽顫忕紒妯诲闁兼亽鍎抽妴濠囨⒑闂堚晝绉剁紒鐘虫崌閻涱喛绠涘☉娆愭闂佽法鍣﹂幏锟�
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
        //闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡宀€鍠栧畷婊嗩槾閻㈩垱鐩弻娑氣偓锝庝簻閳ь剙缍婃俊鐢稿礋椤栨氨鐤€闂佸疇妗ㄧ拋鏌ュ磻閹捐绠荤紓浣姑埀顒€鐏濋埞鎴︽偐閸欏鎮欑紓浣哄珡閸ャ劎鍘卞銈嗗姧缁插潡銆傞崣澶岀闁告侗鍘介崵鍥煛瀹€瀣М濠碘剝鎮傞弫鍐焵椤掑嫭鍊堕柟缁㈠枟閻撴稓鈧厜鍋撻柍褜鍓熷畷浼村箛閸忣偄娲幃褔宕奸悢宄板Τ闂備胶鍋ㄩ崕閬嶅疮鐠恒劎涓嶉柨婵嗩槹閳锋帒霉閿濆牆袚闁靛棗鍟撮弻锝夊箳閻愮數鏆ゅΔ鐘靛仦閸ㄥ灝鐣烽幆閭︽闂佹悶鍊栧ú姗€濡甸崟顖氱闁瑰瓨绻嶆禒濂告煕閵夛附灏︽慨濠呮閸栨牠寮撮悙娴嬫嫟婵＄偑鍊戦崝宀€鎹㈤幋鐘亾闂堟稏鍋㈡鐐寸墵楠炲海绮氶張绶妉entry闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡宀€鍠栭幃褔宕奸悢鍝勫殥闂備礁鎲￠崹瑙勬叏閹绢喖鐓橀柟杈鹃檮閸嬫劙鎮楅崷顓炐㈡い銉︾箘缁辨挻鎷呴崫鍕碘偓宀勬煕閵娿儳鍩ｇ€殿喖顭烽弫鎰緞婵犲嫷鍚呴梻浣瑰缁诲倿骞夊☉銏犵缂備焦顭囬崢鎼佹⒑閸涘﹣绶遍柛鐘愁殜瀹曟劙骞囬悧鍫㈠幐閻庡厜鍋撻悗锝庡墰閻﹀牓鎮楃憴鍕閻㈩垱甯熼悘鎺楁煟閻愬鈻撻柍褜鍓欓崢鏍ㄧ珶閺囥垺鈷戦柛婵嗗閳诲鏌涚€Ｑ冧壕闂備胶枪椤戝棝骞愰幖渚婄稏婵犻潧顑愰弫鍡涙煕鐏炲墽鏁栭柕濞炬櫆閳锋垿鏌涘┑鍡楊伂妞ゎ偓绠撻弻娑㈠籍閳ь剟宕归崸妤冨祦闁告劦鍠栭～鍛存煏閸繃顥為柛搴邯濮婃椽鎳￠妶鍛€惧┑鐘灪閿曘垽鐛崱娑欏€烽柣鎴炃氶幏缁樼箾鏉堝墽鎮奸柣鈩冩煥椤洭骞囬悧鍫㈠幈闁瑰吋鐣崹褰掑煝閺囩噥娈介柣鎰嚋姒氨绱掗悩宕囨创鐎殿喗娼欒灃闁逞屽墯缁傚秹鎮欓浣稿伎濠殿喗顨呭Λ妤佹櫠閹殿喚纾煎璺侯儐閵囨繈鏌″畝瀣？濞寸媴濡囬幏鐘诲箵閹烘繃缍嗛梻鍌欐祰椤曟牠宕伴幘璇插瀭濞寸姴顑呯粻鍦喐閻楀牆绗氶柛瀣剁節閺屸剝寰勭€ｎ亶鍤嬮梺绋垮瘨娴滎亜顫忔繝姘＜婵﹩鍏橀崑鎾诲箹娴ｅ摜锛欓梺褰掓？閻掞箓宕戠€ｎ喗鐓曢柟浼存涧閺嬫盯鏌ｉ妶鍥т壕缂佺粯绻冪换婵嬪磼濞戞ɑ鐝撮梻浣呵归鍡涘箰閹间緤缍栨繝闈涱儛閺佸棝鏌涚仦鍓ф晼闁靛ň鏅滈埛鎴︽煕濠靛棗顏い顐畵閺屾盯寮埀顒勫垂閸ф宓侀柛鎰靛枛椤懘鏌曢崼婵囧櫣缂佹劖绋掔换婵嬫偨闂堟刀銏ゆ倵濮橀棿绨芥俊鍙夊姍瀵挳濮€閳锯偓閹风粯绻涙潏鍓у埌闁硅姤绮庣划鏃堟倻濡晲绨婚梺闈涱檧闂勫嫬鐣峰畝鈧埀顒冾潐濞叉﹢銆冮崨杈剧稏婵犲﹤鐗嗛悞鍨亜閹哄棗浜惧銈嗘穿缂嶄線銆侀弴銏℃櫇闁逞屽墰缁粯绻濆顓炰化闂佹悶鍎滈崘顏堢崜婵＄偑鍊戦崕鑼崲閸繍娼栨繛宸簼椤ュ牊绻涢幋鐐跺妞わ絽鎼埞鎴﹀煡閸℃ぞ绨煎銈冨妼閿曨亪濡存笟鈧顕€宕煎┑瀣暪闂備礁鎼ú銊╁疮閸ф绠繛宸簼閻撶喖鏌ｅΟ鍝勫笭闁煎壊浜弻娑㈠棘鐠恒劎鍔悗娈垮枟閹倿鐛€ｎ喗鏅滈柣锝呰嫰楠炴劙姊虹拠鎻掑毐缂傚秴妫欑粋宥夊醇閺囩喎浜楅梺绋跨箳閳峰牆鈻撴禒瀣厽闁归偊鍨伴惃铏圭磼閻樺樊鐓奸柡宀€鍠栭、娆撴偩瀹€鈧悡澶愭⒑閻熸壆锛嶉柛瀣ㄥ€栨穱濠囨倻閼恒儲娅嗙紓浣圭☉椤戝嫰骞嬮悜鑺モ拻闁稿本鑹鹃埀顒勵棑缁牊绗熼埀顒勭嵁閺嶎収鏁冮柨鏇楀亾缁炬儳缍婇弻娑㈩敃閿濆棛顦ョ紒鐐劤缂嶅﹪寮婚垾鎰佸悑閹肩补鈧尙鐖遍梻浣呵归鍡涘箰閹间緤缍栨繝闈涱儛閺佸棝鏌涚仦鍓ф晼闁靛ň鏅滈埛鎴︽煕濠靛棗顏い顐畵閺屾盯寮埀顒勫垂閸ф宓侀柛鎰靛枛椤懘鏌曢崼婵囧櫣缂佹劖绋掔换婵嬫偨闂堟刀銏ゆ倵濮橀棿绨芥俊鍙夊姍瀵挳濮€閳锯偓閹风粯绻涙潏鍓у埌闁硅姤绮庣划鏃堟倻濡晲绨婚梺闈涱檧闂勫嫬鐣峰畝鈧埀顒冾潐濞叉﹢銆冮崨杈剧稏婵犲﹤鐗嗛悞鍨亜閹哄棗浜惧銈嗘穿缂嶄線銆侀弴銏℃櫇闁逞屽墰缁粯绻濆顓炰化闂佹悶鍎滈崘顏堢崜婵＄偑鍊戦崕鑼崲閸繍娼栨繛宸簼椤ュ牊绻涢幋鐐跺妞わ絽鎼埞鎴﹀煡閸℃ぞ绨煎銈冨妼閿曨亪濡存笟鈧顕€宕煎┑瀣暪闂備礁鎼ú銊╁磻閻斿壊娈介柛銉墯閳锋垹绱撴担濮戭亪鎮橀妷锔跨箚妞ゆ劧缍嗗▓鏇㈡煛閸涱厾鍩ｆ鐐达耿椤㈡瑩鎮剧仦钘夌婵犵數鍋犻幓顏嗙礊閳ь剚銇勯銏╂Ц闁伙絽鍢查悾婵嬪焵椤掑嫬鐒垫い鎺嶇贰閸熷繘鏌涢敐搴℃珝鐎规洘绮撻幃銏ゆ⒐閹邦喚鐣鹃梻浣稿閸嬪懎煤閺嶎厾宓侀柕蹇娾偓鑼畾闂侀潧鐗嗛幏鎴濐潖濡ゅ啰纾奸柟鎻掝儑閻ｉ亶鏌嶇憴鍕伌闁诡喗鐟╁鍫曞箣閻樿鲸顢橀梻鍌欐祰椤鎮洪弴銏╂晪鐟滄棃鍨鹃弮鍫濈闁冲搫鍠氬ù鍕煟鎼搭垳绉靛ù婊勭箞閹顢楅崟顑芥嫼闁荤喐鐟ョ€氼剛绮堥崘顔界厱闁冲搫鍊绘晶鐢碘偓瑙勬礃閸ㄤ絻鐏掗柣鐘辩绾绢厾绮诲鑸碘拺闂傚牊绋撴晶鏇㈡煕婵犲倹鍟炵紒鍌氱У閵堬綁宕橀埞鐐闂備胶顢婇崑鎰板磻濞戙垹鐒垫い鎺嶈兌婢х敻鏌熼銊ユ处閸嬫劙鏌ゆ慨鎰偓鏇犵矈閿曞倹顥婃い鎰╁灪婢跺嫮绱掔€ｎ偄娴€规洘鍔栫换婵嗩潩椤撶姴骞橀梻浣筋嚃閸樼晫鏁幒妤€绀夋繝濠傚缁犻箖鏌涘☉鍗炰簻闁诲繐寮堕幈銊︾節閸愨斂浠㈤悗瑙勬处閸嬪﹤鐣烽悢纰辨晝闁绘棁娓规竟鏇㈡⒑閸撴彃浜濇繛鍙夌墱婢规洝銇愰幒鎾跺幐閻庡箍鍎辨鍛婄閹扮増鐓曢悗锝庡亝鐏忣厽銇勯锝囩疄妞ゃ垺顨婂畷鎺戔攦閻愵亜濮傛慨濠冩そ瀹曨偊宕熼鍛晧闂備礁鎲″褰掑垂閸ф宓侀柛鎰靛枛椤懘鏌ｅΟ铏逛粵婵炲牊绻堝缁樼瑹閸パ冧紟闂傚倸瀚€氫即寮鍡欑懝闁逞屽墮椤繒绱掑Ο璇差€撻梺鑽ゅ枛閸嬪﹪宕电€ｎ剛纾藉ù锝囩摂閸ゆ瑩鏌涙繝鍌ょ吋妤犵偞鍨挎慨鈧柕鍫濇噽椤撴椽姊虹紒姗堜緵闁稿瀚粋宥夋倷椤掍礁寮垮┑顔筋殔濡鏅堕幍顔剧＜濠㈣泛顑嗛妵婵嬫煛瀹€瀣？濞寸媴濡囬幏鐘诲箵閹烘繃缍嗛梻鍌欐祰椤曟牠宕伴幘璇插瀭闁芥ê顦遍弳锕傛煏婵犲繐顩紒鈾€鍋撻梻浣告啞濞诧附绂嶉悙闈涱棜闁秆勵殕閳锋帡鏌涚仦鍓ф噮妞わ讣濡囩槐鎾愁吋閸滃啳鍚柦妯煎枛閺岀喖骞戦幇闈涙缂備讲鍋撻柛灞剧⊕閸欏繑淇婇悙棰濆殭濞存粓绠栭幃妤€鈻撻崹顔界彯闂佺ǹ顑呴敃顏堟偘椤斿槈鐔煎礂閻撳海褰撮梻浣告啞閻熴儵藝閽樺）娑㈡偄閸忓皷鎷绘繛鎾村焹閸嬫挻绻涙担鍐叉处閸嬪鏌涢埄鍐︿簵婵炴垶顭傞弮鍫濇そ濞达綀顫夊▍鍡涙⒒娴ｇǹ顥忛柛瀣噹鐓ゆ慨妞诲亾闁糕晜鐩獮瀣晜閽樺鍖栭梻浣筋嚃閸樼晫鏁幒鎾茬箚濠靛倸鎲￠悡娑㈡倵閿濆骸浜為柟鍐插閺屾洟宕惰椤忣厼鈹戦鐟颁壕闂備焦瀵х粙鎴犫偓姘煎櫍閹剝寰勬繛銏℃閹晠妫冨☉妤冩崟婵＄偑鍊х粻鎾寸閸洖鏄ラ柍褜鍓氶妵鍕箳閹搭垰濮涚紓浣割槺閺佸寮诲☉姘ｅ亾閿濆簼绨绘い蹇ｅ幗閵囧嫰濮€閳╁喚妫冮悗瑙勬礀閵堢ǹ顕ｉ幘顔藉亜闁告瑥顦褰掓⒒娴ｈ棄鍚瑰┑顔肩仛缁傚秵绂掔€ｎ亞顦柣蹇曞仩琚欓柡瀣Ч閺岀喖骞嶉纰辨毉闂佺粯鎸婚悷鈺呭蓟濞戙垹绠ｆ繛鍡楃箚閸嬫捇寮介鐔蜂壕婵鍋撶€氾拷?s闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡灞糕偓鎰佸悑閹肩补鈧磭顔愰梻浣规た閸樹粙銆冮崱娆愬床婵炴垶锕╅崯鍛亜閺冨洤鍚归柛鎴濈秺濮婅櫣绮欏▎鎯у壉闂佺粯顨堟繛鈧鐐插暙铻栭柍褜鍓熼崺銉﹀緞婵炵偓鐎哄銈嗘寙閸岀偛浠愭俊鐐€戦崹鍝勎涢崘銊ф殾濠靛倻枪閸楁娊鏌ｉ弬鍨骇閻庢艾銈稿娲嚒閵堝憛锝吤瑰⿰鈧划娆忕暦濠靛棛鏆嗛柛鏇ㄥ亜閸ゆ垿鏌熼懖鈺勊夋俊鎻掓嚇閹垽宕卞☉娆忎画濠电偛妫楃换鎰邦敂椤愶附鐓欓柛鎰级閸ゅ洭鏌″畝鈧崰鎰八囬悧鍫熷劅闁宠鲸甯╅崜姘跺Φ閸曨垱鏅滈柦妯侯槴閺嬫瑩姊虹拠鈥虫灍妞ゃ劌鎳橀敐鐐测攽鐎ｅ灚鏅㈤梺绋挎湰缁ㄤ粙濡搁埡鍌楁嫼闂佸憡绻傜€氼垶锝為敃鍌涚厱闁哄啠鍋撻柛銊ユ健閻涱噣宕橀纰辨綂闂侀潧鐗嗛幊搴ｇ玻濞戞瑧绡€闁汇垽娼у瓭闁诲孩鍑规禍鐐参ｉ幇鏉垮嵆闁靛繆妾ч幏缁樼箾鏉堝墽鍒伴柟鑺ョ矌缁棃鎮滃Ο闀愮盎闂侀潧顧€闂勫嫬鐣峰畝鈧埀顒冾潐濞叉﹢銆冮崨杈剧稏婵犲﹤鐗嗛悞鍨亜閹哄棗浜惧銈嗘穿缂嶄線銆侀弴銏℃櫇闁逞屽墰缁鎳￠妶鍥╋紳婵炶揪绲介～鏍敂閸曘劍鐏侀梺缁樻煥閸氬鍩涢幋鐘电＜閻庯綆鍋勯婊勭節閳ь剟骞嶉鍓э紲濡炪倖妫侀崑鎰櫏婵＄偑鍊戦崹鍝勎涢崘顔肩畺闁靛繈鍊曠粈鍌炴煠濞村娅呮鐐茬Ч閺岋絾鎯旈敐鍡楁畬闁诲孩鍑归崢濂糕€﹂崹顔ョ喐鎷呴弴顏嗙М闁圭绻濇俊鍫曞磼濮橆偄顥氶梺鑽ゅ枑閻熴儳鈧凹鍘剧划鍫ュ醇濠垫劗鍞甸悷婊冮叄閵嗗啯绻濋崶鈺佺ウ闂婎偄娲︾粙鎴濐啅濠靛洢浜滈柟鐐殔閸燁垶宕愰姘ｆ斀闁绘﹩鍠栭悘杈ㄧ箾婢跺娲存い銏＄墵瀹曞崬顪冪紒妯尖偓娲⒑闂堟稓绠為柛濠冪墵閸╂盯骞嬮敂鐣屽幈濠电娀娼уΛ妤咁敂閳哄懏鐓冪憸婊堝礈濞嗘挻鍋夐柣鎾虫捣閺嗭箓鏌熼悧鍫熺凡闁告劏鍋撴俊鐐€栭崝妤呭窗鎼淬垻顩查柣鎰靛墯閸欏繑鎱ㄥΔ鈧Λ妤佹櫠閹殿喚纾煎璺侯儐閵囨繈鏌″畝瀣？濞寸媴濡囬幏鐘诲箵閹烘繃缍嗛梻鍌欐祰椤曟牠宕伴幘璇插瀭闁芥ê顦遍弳锕傛煏婵犲繐顩紒鈾€鍋撻梻浣告啞濞诧附绂嶉悙闈涱棜闁秆勵殕閳锋帡鏌涚仦鍓ф噯闁稿繐鐭傞弻鐔兼惞椤愶絽纾抽悗瑙勬礈閺佽顕ｆ禒瀣垫晣闁绘劖顔栭崬鐢告⒒娴ｈ姤纭堕柛锝忕畵楠炲繘鏁撻敓锟�
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
        fprintf(stderr, "Oops!闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡宀€鍠栭幃褔宕奸悢鍝勫殥闂備礁鎲￠崹瑙勬叏閹绢喖鐓橀柟杈鹃檮閸嬫劙鎮楅崷顓炐㈡い銉︾箘缁辨挻鎷呴崫鍕碘偓宀勬煕閵娿儳鍩ｇ€殿喖顭烽弫鎰緞婵犲嫷鍚呴梻浣瑰缁诲倿骞夊☉銏犵缂備焦顭囬崢鎼佹⒑閸涘﹣绶遍柛鐘愁殜瀹曟劙骞囬悧鍫㈠幐閻庡厜鍋撻悗锝庡墰閻﹀牓鎮楃憴鍕閻㈩垱甯熼悘鎺楁煟閻愬鈻撻柍褜鍓欓崢鏍ㄧ珶閺囥垺鈷戦柛婵嗗閳诲鏌涚€Ｑ冧壕闂備胶枪椤戝棝骞愰幖渚婄稏婵犻潧顑愰弫鍡涙煕鐏炲墽鏁栭柕濞炬櫆閳锋垿鏌涘┑鍡楊伂妞ゎ偓绠撻弻娑㈠籍閳ь剟宕归崸妤冨祦闁告劦鍠栭～鍛存煏閸繃鍣圭紒鎰⊕缁绘繈鎮介棃娴躲垽鎮楀闂寸敖婵″弶鍔欏鎾閳锯偓閹风粯绻涙潏鍓у埌闁硅姤绮庣划鏃堟倻濡晲绨婚梺闈涱檧闂勫嫬鐣峰畝鈧埀顒冾潐濞叉﹢銆冮崨杈剧稏婵犲﹤鐗嗛悞鍨亜閹哄棗浜惧銈嗘穿缂嶄線銆侀弴銏℃櫇闁逞屽墰缁粯绻濆顓炰化婵°倧闄勭€笛囶敂閻樼數纾兼繛鎴烆焽缁犵粯鎱ㄦ繝鍐┿仢鐎规洜鍏橀、妯衡攦閹傚闁诲函缍嗛崰鏍不椤栫偞鐓ラ柣鏇炲€圭€氾拷");//闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡宀€鍠栭幃褔宕奸悢鍝勫殥闂備礁鎲￠崹瑙勬叏閹绢喖鐓橀柟杈鹃檮閸嬫劙鎮楅崷顓炐㈡い銉︾箘缁辨挻鎷呴崫鍕碘偓宀勬煕閵婏箑鈻曢柛鈹惧亾濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告ê銈搁垾锕傚垂椤斻儳鍠栭幊鏍煛閸愵亜濮煎┑锛勫亼閸婃洜鎹㈤幇鐗堝亱濠电姴娲ら悡鏇㈡煙鏉堥箖妾柛瀣剁秮閺岀喖顢橀悢椋庣懆婵犵鈧偨鍋㈤柡宀嬬磿閳ь剨缍嗛崑鍕倶閹绢喗鐓涚€光偓鐎ｎ剛锛熼梺閫炲苯澧剧紓宥呮缁傚秹宕奸弴鐔蜂簵闂佺ǹ绻掗埛鍫濃枔娴犲鐓熼柟閭﹀灠閻ㄨ櫣绱掗悩宸吋闁哄瞼鍠栭、娆撴偩瀹€鈧悡澶愭⒑閻熸壆锛嶉柛瀣ㄥ€栨穱濠囨倻閼恒儲娅嗙紓浣圭☉椤戝嫰骞嬮悜鑺モ拻闁稿本鑹鹃埀顒勵棑缁牊绗熼埀顒勭嵁閺嶎収鏁冮柨鏇楀亾缁炬儳缍婇弻娑㈩敃閿濆棛顦ョ紒鐐劤缂嶅﹪寮婚垾鎰佸悑閹肩补鈧尙鐖遍梻浣呵归鍡涘箰閹间緤缍栨繝闈涱儛閺佸棝鏌涚仦鍓ф晼闁靛ň鏅滈埛鎴︽煕濠靛棗顏い顐畵閺屾盯寮埀顒勫垂閸ф宓侀柛鎰靛枛椤懘鏌曢崼婵囧櫣缂佹劖绋掔换婵嬫偨闂堟刀銏ゆ倵濮橀棿绨芥俊鍙夊姍瀵挳濮€閳锯偓閹风粯绻涙潏鍓у埌闁硅姤绮庣划鏃堟倻濡晲绨婚梺闈涱檧闂勫嫬鐣峰畝鈧埀顒冾潐濞叉﹢銆冮崨杈剧稏婵犲﹤鐗嗛悞鍨亜閹哄棗浜惧銈嗘穿缂嶄線銆侀弴銏℃櫇闁逞屽墰缁鎳￠妶鍥╋紳婵炶揪绲介～鏍敂閸繄顦梺鑽ゅ枛閸嬪﹤銆掓繝姘厪闁割偅绻冮ˉ婊冣攽椤斻劌鎳愮壕濂稿级閸稑濡界紒鈧崘顏嗙＜閻庯綆浜跺Ο鈧悗娈垮枛椤攱淇婇悜鑺ユ櫆闁诡垎鍐吋闂備浇顕уù鐑藉箠閹惧灈鍋撳鐓庡箹妞ゎ厼鐏濊灒濞达絾娲╃紞渚€骞婇敓鐘参ч柛灞剧煯婢规洟鏌ｉ悢鍝ユ噧閻庢凹鍘炬禍鎼侇敇閻樼數锛滅紓鍌欑劍閿曨偊鎳撶捄銊㈠亾濞堝灝鏋涙い顓㈡敱娣囧﹪骞栨担鍝ュ幐闂佺ǹ鏈惌顔捐姳婵犳碍鈷掑ù锝呮啞閸熺偟绱掔€ｎ偄鐏撮柡浣稿暣閺佸倿鎸婃径妯烘闂備礁鎲″ú锕傚垂闁秴鐓曢柟瀵稿У閸犳劙鏌ｅΔ鈧悧鍡樼┍椤栨稐绻嗘い鎰剁到閻忔挳鏌＄仦鍓ь灱缂佺姵鐩顕€骞橀崗澶婁壕閻犳亽鍔夐崑鎾斥枔閸喗鐝梺绋款儏閿曨亪鎮伴鍢夌喖鎳栭埡鍐跨床婵犵妲呴崹宕囧垝椤栫儐鏁傞柣鏂垮悑閳锋帒霉閿濆牆袚缁绢厼鐖奸弻娑㈡晲韫囨洜鏆ゅΔ鐘靛仜缁绘﹢寮幘缁樻櫢闁跨噦鎷�?
        assert(se != nullptr);      //闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞鐟滃寮搁弽顓熺厸闁搞儯鍎遍悘鈺冪磼閹邦収娈滈柡灞糕偓鎰佸悑閹艰揪绲肩划鍫曟⒑缂佹ɑ鐓ラ柟娴嬪墲閸掑﹪骞橀鐣屽弰闂婎偄娲﹂幐鍓х矓濞差亝鐓冮柣鐔稿缁犲鏌″畝瀣М闁诡喓鍨藉畷顐﹀Ψ閿曗偓濞呮垿姊虹拠鎻掝劉闁告垵缍婂畷婊堟偄缁楄　鍋撴笟鈧顕€宕掑☉妯硷紡闂佽崵鍋炵粙鍫ュ礈濠靛缍栫€广儱顦伴埛鎴︽煕閿旇寮ㄦ俊鐐倐閺屾盯濡歌椤ｈ偐绱掗崒姘毙ｉ柕鍫秮瀹曟﹢鍩為悙顒€顏烘繝鐢靛仩閹活亞绱為埀顒€鈹戦鍝勨偓妤€鈽夐悽绋块唶闁哄洨鍠撻崢閬嶆⒑缂佹◤顏堝疮閹稿孩鍙忕€广儱娲犻崑鎾斥枔閸喗鐏堥梺纭咁嚋缁辨洟骞戦姀鐘斀闁糕檧鏅滈崓闈涱渻閵堝棗绗掗柛瀣瀹曨剟鎮介崨濞炬嫽婵炶揪缍€濞咃絿鏁☉銏＄厱闁靛ǹ鍎遍幃鎴︽煙娓氬灝濮傚┑陇鍩栧鍕節閸曨剛鍙勯梻鍌欑缂嶅﹤螞閸ф鍊块柨鏇炲€归崑鍌涚箾閹存瑥鐏柣鎾存礋閹﹢鎮欓幍顔炬毌濡炪倕绻愰悧鍡涙倿閸偁浜滈柟鍝勭Х閸忓苯顭胯娴滃爼寮诲鍫闂佸憡鎸诲畝鎼佸箠濠靛绀堢憸蹇曠矆婵犲倵鏀介柣妯哄级閹兼劙鏌﹂崘顏勬灈闁哄本娲樼换娑㈡倷椤掍胶褰嗛梻浣呵归鍛偓姘嵆瀵鏁愭径濠勵吅闂佺粯鍨靛Λ娑㈠礉閻戣姤鈷戦柛婵嗗閿涙梻绱掗悩铏磳鐎殿喖顭烽崺鍕礃閵娧呯嵁濠电姷鏁告慨瀵糕偓姘煎墴閸╁懘鏌嗗鍡忔嫼缂備礁顑嗛娆撳磿閹扮増鐓欓柟闂磋兌閻ｆ椽鏌涢埞鎯т壕婵＄偑鍊栫敮濠偯归崶銊х彾婵☆垵鍋愮壕鐣屸偓骞垮劚閹锋垿鐓鍌楀亾濞堝灝鏋涙い顓㈡敱娣囧﹪骞栨担鍝ュ幐闂佺ǹ鏈惌顔捐姳婵犳碍鈷掑ù锝呮啞閸熺偟绱掔€ｎ偄鐏撮柡浣稿暣閺佸倿鎸婃径妯烘闂備礁鎲″ú锕傚垂闁秴鐓曢柟杈鹃檮閻撶喖鏌ｉ弬鎸庡暈缂佽泛寮剁换娑㈠醇濮橆厽鐝氶梺鍝勭灱閸犳牕鐣峰▎鎾澄ч柛鈩冾殢娴硷拷
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




