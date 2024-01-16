#include "Ast.h"
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
std::vector<Operand *> para_operands;// 闂傚倸鍊搁崐鎼佸磹閹间礁纾圭€瑰嫭鍣磋ぐ鎺戠倞妞ゆ帒顦伴弲顏堟偡濠婂啰绠婚柛鈹惧亾濡炪倖甯婇懗鍫曞煝閹剧粯鐓涢柛娑卞枤缁犳﹢鏌涢幒鎾崇瑨闁宠閰ｉ獮妯虹暦閸ヨ泛鏁藉┑鐘茬棄閺夊簱鍋撻幘缁樺€块柨鏇楀亾闁宠绉瑰顕€宕奸悢鍙夊闂備胶枪閺堫剟鎮疯瀹曟繂顓兼径瀣幍濡炪倖妫侀崑鎰板春閿濆洠鍋撶憴鍕闁告梹娲熼崺鐐哄箣閿曗偓閻愬﹪鏌嶉崫鍕舵敾闂佽￥鍊濆娲嚒閵堝憛顒佷繆濡炵厧濡块柟宄板暣閺佸啴宕掑顒佸劒闂備礁鎼ú銊╁窗閸℃顩叉繝濠傜墛閻撴瑩鏌℃径濠勪虎妞わ絾濞婇弻锝夋晲閸粌鎯堢紓浣介哺鐢繝骞冮鍫濈劦妞ゆ帒瀚崹鍌炴煙閹増顥夐柣鎺戠仛閵囧嫰骞掗崱妞惧闂備礁鎲¤摫缂侇喗鎸搁悾鐑藉箣閿曗偓缁犺崵绱撴担璇＄劷闁告ɑ鎹囬幃宄扳堪閸曨厾鐓夐悗瑙勬礃缁矂鍩㈡惔銊ョ鐎规洖娲ら獮鎺楁⒒娴ｇ瓔娼愮€规洘锕㈤、姘愁樄闁哄苯顑夊畷鍫曨敆娴ｅ搫骞愰梻浣规偠閸庮噣寮插☉銏犲嚑闁哄啫鐗婇悡鏇熶繆椤栨碍璐￠柣顓熺懄閹便劍绻濋崘鈹夸虎闂佸搫鑻幊姗€骞冨▎鎾村殤閻犺桨璀︽导鍐ㄢ攽閻橆偅濯伴悘鐐跺Г閻忓牓姊虹€圭媭娼愰柛銊ユ健瀹曟椽宕熼姘鳖槰濡炪値鍘介崹闈涒枔濠靛牏纾介柛灞剧懆閸忓瞼绱掗鍛仸闁诡垰瀚伴、娑橆潩鏉堛劍顔曞┑鐘绘涧閸婃悂骞夐敓鐘茬；闁告洦鍋掑〒濠氭倵閿濆簼绨介柛鎴濇贡缁辨帗娼忛妸褏鐤勯梺鍝勭焿缁查箖骞嗛弮鍫澪ч幖娣灮缁夐攱绻濋悽闈涗沪缂佷焦鎸冲鎻掆攽閸噥娼熼梺缁樺姇閻°劍鍒婇幘顔界厱婵犻潧瀚崝婊勭箾閸儳鐣烘慨濠冩そ瀹曠兘顢橀悙鎻掝瀱闂備焦鎮堕崝搴ㄥ窗鎼粹€崇カ闂備礁澹婇崑鍡涘窗閹惧墎涓嶅Δ锝呭暞閻撴洟鏌嶉埡浣稿箻妞ゅ繐鎳忛崣蹇涙煥閺囩偛鈧綊鎮￠弴銏＄厽闁哄啠鍋撶憸鏉垮暞缁傚秵銈ｉ崘鈹炬嫽闂佺ǹ鏈懝楣冨焵椤掍焦鍊愮€规洘鍔欓幃婊堟嚍閵夈儮鍋撻崸妤佺叆闁哄洨鍋涢埀顒€缍婂畷鏇㈠箣閻樺啿鏋戦梺缁橆殔閻楀棛绮幒鏃傜＜闁逞屽墯瀵板嫮浠︾粙澶稿闂佺ǹ绻愰ˇ顖涚妤ｅ啯鈷戦柛娑橈攻婢跺嫰鏌涚€ｎ亜顏柟顔筋殜椤㈡﹢鎮╅悽纰夌闯濠电偠鎻徊鎸庣仚闂佺厧鎽滈幊鎾烩€﹂懗顖ｆ闂佺懓鍤栭幏锟�

std::vector<std::vector<Operand*>>para_operands_stack;
class temp_data_bag
{
public:
    Instruction* insn;
    Operand* addr;
    Operand* temp_src;
};
std::vector<std::vector<temp_data_bag>>ParaNodeStack;//这个鬼东西是设置来做那个参数的。


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
    // 闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾剧懓顪冪€ｎ亝鎹ｉ柣顓炴闇夐柨婵嗩槹娴溿倝鏌ら弶鎸庡仴鐎殿喖鐖煎畷鐓庮潩椤撶喓褰呴梻浣规偠閸斿秶鎹㈤崘顔嘉﹂柛鏇ㄥ灠閸愨偓濡炪倖鍔﹀鈧紒顔煎缁辨挻鎷呴幓鎺嶅濠电姰鍨奸崺鏍礉閺嶎厽鍋傛繛鎴欏灪閻撴洟鏌曟径鍫濈仾婵炲懎鎳橀弻锛勨偓锝庡亜閻忥妇绱掔紒妯兼创鐎规洏鍔戦、娑橆潩閿濆棛鈧即姊绘担鍛婃儓闁活厼顦卞濠囧磹閻旇桨鑸梻鍌欑劍閸庡磭鎹㈤幒鎴旀瀺闁靛繈鍨洪～鏇㈡煟閹邦喖鍔嬮柍閿嬪灩缁辨挻鎷呯拠锛勫姺缂備胶濮甸幑鍥蓟閿濆鍐€鐟滃宕戦姀鈶╁亾濞堝灝鏋涙い顓犲厴瀵偊骞囬弶鍨獩闂佺ǹ鏈粙鎺椝夐崼銉︾厱闁宠鍎虫禍鐐繆閻愵亜鈧牜鏁幒鏂哄亾濮樼厧寮€规洘鍨归埀顒婄秵娴滄牠寮ㄦ禒瀣厽婵☆垰鎼痪褏绱掓笟鍥т簽缂佽鲸鎸搁濂稿幢濞嗗繆鎷伴梻浣告惈鐞氼偊宕濋幋锕€绠栭柕鍫濐槸绾惧吋绻涢幋鐑囦緵濞寸》鎷�
    /*fprintf(yyout, "declare i32 @getint()\n");
    fprintf(yyout, "declare void @putint(i32)\n");
    fprintf(yyout, "declare i32 @getch()\n");
    fprintf(yyout, "declare void @putch(i32)\n");
    fprintf(yyout, "declare void @putf(i32)\n\n");*/
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
    ParaNodeStack.push_back(std::vector<temp_data_bag>());
    if (paraStmt != nullptr)
    {
        if (paraStmt != nullptr)
            paraStmt->genCode();
    }

    for (int i = (int)ParaNodeStack[ParaNodeStack.size() - 1].size() - 1; i > 0; i--)
    {
        Operand* addr = ParaNodeStack[ParaNodeStack.size() - 1][i].addr;
        Operand* temp_src = ParaNodeStack[ParaNodeStack.size() - 1][i].temp_src;

        entry->insertFront(ParaNodeStack[ParaNodeStack.size() - 1][i].insn);       // allocate instructions should be inserted into the begin of the entry block.
        new StoreInstruction(addr, temp_src, entry, 1);
        func->add_para(temp_src);
    }


    ParaNodeStack.pop_back();
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
        // bool -> int 闂傚倸鍊搁崐鎼佸箠韫囨稑绀夐柡鍥╁У椤洟鏌ㄥ☉妯侯伀閻忓繒鏁婚幃褰掑炊椤忓嫮姣㈤梺閫炲苯澧撮柡鈧崡鐑嗙劷妞ゅ繐鐗滈弫鍡椕归敐鍤藉湱鑺遍敓锟�
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
    //    //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆撴倻濡桨鐢绘繝鐢靛Т閿曘倝宕板顑╃喖鍩€椤掑嫭鍊垫繛鍫濈仢閺嬬喖鏌熼鐓庘偓鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸閻忕偟纭堕崑鎾崇暦閸ャ劍鐣烽梻渚€鈧偛鑻晶瀵糕偓瑙勬磻閸楀啿顕ｆ禒瀣垫晣闁绘劗鏁搁弳顐︽煟閻斿摜鐭嬮柡灞诲姂閹箖鏌ㄧ€ｎ剟妾梺鍛婄☉閿曘倖绂嶅⿰鍫熲拺闁告稑锕︾粻鎾绘倵濮樼厧澧柍缁樻崌婵＄兘鍩￠崒妤佸闂備胶枪閺堫剟鎮疯钘濋柨鏇炲€归悡娆撴偣閸ュ洤鎳愰惁鍫ユ倵鐟欏嫭绀€婵犫偓闁秴绠熼柟缁㈠枛缁€瀣亜閹哄棗浜鹃悗娈垮枛濞尖€愁潖濞差亜鎹舵い鎾墲椤庢姊洪崫銉バｉ柣妤冨█楠炲啴鍨鹃幇浣瑰缓闂侀€炲苯澧寸€殿喖顭烽幃銏ゅ川婵犲嫮肖濠德板€х徊浠嬪疮椤栫儐鏁佺€广儱顦伴埛鎴犵磼鐎ｎ偒鍎ラ柛搴㈠姍閺屾盯寮埀顒勬偡閳哄懐宓侀柛鎰╁壆閺冨牆宸濇い鎾跺枎缁佽埖淇婇悙顏勨偓鏍箰妤ｅ啫纾规い鎰剁畱椤懘骞掗搹顐ゎ洸婵犲﹤鐗婇悡蹇擃熆閼哥數鈽夋い鈺婂墰缁辨帡鎮╅幇浣哄姺缂備浇椴哥敮妤€顭囪箛娑樜╅柨鏇楀亾闂傚绉瑰娲偡閺夋寧些闂佺娅曢敋闁伙絿鍏橀、鏇㈡晝閳ь剛绮堢€ｎ剛妫柣妤€鐓鍛洸闁告挆鈧崑鎾诲礂婢跺﹣澹曢梻浣告啞濞诧箓宕滃☉銏犲偍闂侇剙绉甸埛鎴︽⒒閸喕鍎愮憸鐗堝笒绾惧潡鏌熼幆鐗堫棄闁哄嫨鍎甸弻銊モ攽閸℃顦遍梺绋款儐閹告悂鍩㈤幘璇插瀭妞ゆ梻鏅禍顏呬繆閻愵亜鈧倝宕㈡ィ鍐ㄧ婵☆垯璀﹂崵鏇熴亜閹板墎鐣辩紒鐘崇⊕閵囧嫰骞樼捄杞版埛缂備焦鍔楅崑鐐垫崲濠靛鍋ㄩ梻鍫熷垁閵忋倖鍊垫慨妯哄船閸樺鈧娲忛崕闈涚暦閵娧€鍋撳☉娅辨岸骞忕紒妯肩閺夊牆澧介幃濂告煙閾忣偅宕屾い銏¤壘楗即宕ㄩ婊愮床闂佽崵濮村ú锕併亹閸愵喖鐒垫い鎺嗗亾婵炵》绻濋幃浼搭敊閽樺绐炴繝鐢靛仧閸嬫捇锝炲澶嬧拺闂傚牊绋撶粻鍐测攽椤旀儳鍘存い銏＄懇楠炴帡寮崒婊愮闯闂備胶枪閺堫剟鎮疯瀹曟繂顓奸崱鎰盎濡炪倖鎸鹃崑鐐哄窗濡皷鍋撶憴鍕８闁搞劋绮欓悰顕€寮介妸锕€顎撻梺绋跨箰椤︽壆鈧艾銈稿缁樻媴閸涘﹤鏆堢紓浣割儐閸ㄥ潡寮崘顔芥櫜闁搞儺鐓堥崑銊╂⒒娓氬洤澧紒澶屾暬閹繝寮撮姀锛勫幗濠碘槅鍨甸褎鏅堕娑氱闁割偆鍠撻惌宀勬煃瑜滈崜婵嬶綖婢跺⊕鍝勵潨閳ь剙鐣峰┑鍡忔瀻闁规崘娉涚粊锕傛⒑閹肩偛鍔撮柛鎾村哺瀹曟垹鈧綆鍠楅悡鏇熴亜閹邦喖孝闁诲繆鏅滈〃銉╂倷閺夋垵顫掗梺鍝勭焿缁查箖骞嗛弮鍫濈伋闁告劘灏欢鐔兼⒒娴ｅ懙褰掝敄閸℃稑绠查柛銉墮缁犳牗淇婇妶鍛櫤闁哄懏鐓￠獮鏍垝閸忓浜鹃柟棰佺劍妤犲嫰姊婚崒娆愵樂缂侀硸鍠氱划濠氬箣閿旇棄浜遍梺瑙勫礃椤曆呯不閺嶃劋绻嗛柕鍫濇噹閺嗙偤鏌涢悩鍙夘棦闁哄本鐩鎾Ω閵夈儺娼炬俊鐐€х拹鐔煎礉閹达箑钃熼柨婵嗩槸椤懘鏌嶆潪鎷屽厡濞寸媭鍙冮弻锝夊閳轰胶浠梺鍝ュУ閻楃娀鐛崘顭戞建闁逞屽墴閻涱噣骞掗幊铏⒐閹峰懘宕崟顐ゎ啈闂傚倸鍊烽懗鍓佸垝椤栫偛绀夐柡鍥ュ焺閺佸銇勯幘鍗炵仼闁绘挻绻堥弻鐔煎礈瑜忕敮娑㈡煟閹惧啿鏆熼柟鑼归オ浼村醇濠靛牜妲舵繝鐢靛仜濡瑩骞愰幖浣哥＜闁靛ň鏅滈悡娑氣偓骞垮劚閸燁偅淇婇崸妤佺厽婵犻潧妫涢崺锝夋煛瀹€瀣瘈鐎规洖鐖兼俊鐑藉Ψ瑜岄惀顏堟⒒娴ｇ懓鈻曢柡鈧柆宥呭瀭闁秆勵殔閽冪喐绻涢幋鐐冩岸寮ㄦ禒瀣厱闁斥晛鍠氬▓銏犆圭亸鏍т壕闂傚倸鍊风粈渚€鎮块崶顒夋晪鐟滄棁妫熼梺姹囧灮鏋柦鍐枑缁绘盯骞嬮悙鑼姲闂佺ǹ顑嗛幑鍥极瀹ュ纭€闁绘劕寮跺▓銊︾節閵忥綆娼愭繛鑼枎椤繑绻濆顒傦紲濠电偛妫欑敮鎺楀储閿熺姵鐓涢悗锝呭悁缁ㄥジ鏌曢崶褍顏鐐差儏閳规垿宕堕懜纰夌礂濠德板€楁慨鐑藉磻閻愬搫绀夋繛鍡樻尵瀹撲線鏌″鍐ㄥ闁荤喓鎳撻埞鎴︽偐閹绘帩浼€闂佸磭顑曢崐鏍崲濠靛鍋ㄩ梻鍫熺▓閺嬪懎鈹戦悙鏉垮皟闁搞儴鍩栭弲鐐烘⒑閸︻厼顣兼繝銏★耿瀵憡绗熼埀顒勫蓟濞戙垹鐒洪柛鎰典簴婵洭姊虹紒妯诲鞍婵炶尙鍠栭獮鍐潨閳ь剟骞冨▎鎾崇闁瑰搫妫欑€垫牠姊绘担鑺ャ€冮柣鎺炵畵閹囨偐鐠囪尪鎽曢梺鍝勬川閸犳挾绮绘ィ鍐╃厽闁逛即娼ф晶顖炴煟濠靛洦绀嬫慨濠冩そ楠炲酣鎳為妷锔芥闂備焦鎮堕崝灞筋焽閳ユ剚鍤曢悹鍥ㄧゴ濡插牊淇婇鐐存暠闁哄倵鍋撻梻鍌欒兌绾爼宕滃┑瀣ㄢ偓鍐疀閺傛鍤ら梺绋挎湰閸戠懓銆掓繝姘厪闁割偅绻堥妤€霉濠婂啫鈷旂紒杈ㄥ浮閹晠鎼归銏紦闁诲孩顔栭崰娑㈩敋瑜旈崺銉﹀緞閹邦剦娼婇梺缁橆焽閺佺ǹ顭囨径鎰拻濞撴埃鍋撻柍褜鍓涢崑娑㈡嚐椤栨稒娅犻悗鐢电《閸嬫挾鎲撮崟顒傤槬缂傚倸绉撮敃銈夋偩妞嬪海鐭欓悹鍥╁亾濡啫鐣峰鈧俊姝岊槹闁稿锕濠氬磼濞嗘埈妲梺纭咁嚋缁绘繂鐣峰┑瀣櫇闁稿本姘ㄩ悰銉╂⒑閸濆嫮鈻夐柛娆忕箳缁辩偤寮介鐔哄幈濠电偞鍨堕…鍥箺閻樼粯鐓熼柟鎯х－缁犲鏌＄仦鍓ф创妞ゃ垺娲熼弫鎰板炊閳哄啯姣囬梻鍌欑窔濞艰崵寰婇挊澶涜€块弶鍫氭櫆椤洟鏌熼悜姗嗘當缂佺姵绋掗妵鍕箻鐠虹洅娑樷攽椤斿吋宸濈紒杈ㄦ尰閹峰懘鎼归悷鎵偧缂傚倷娴囬褔鎮ч幘鑽ゅ祦闁告劏鏅濋々鐑芥倵閿濆骸浜芥俊顐㈠暙閳规垿鎮欏▓鎸庮棑缂傚倸鐗呴崡鍐茬暦鐟欏嫮顩烽悗锝庝簽閻ｅ搫鈹戞幊閸婃劙宕戦幘娣簻妞ゆ劧绲跨粻鐐存叏婵犲懎鍚归柍褜鍓熷褔骞夐敓鐘茬柧闁稿繗鍋愮弧鈧紒鍓у鑿ら柛瀣崌瀹曟﹢鎳犻鍕礈闂傚倷绀侀幉鈥愁潖瑜版帗鍋￠柕澶嗘櫓閺佸鏌ㄥ┑鍡╂Ц缂佺姷绮妵鍕籍閸屾瀚涘┑鐐靛帶閿曨亜顫忓ú顏勭闁兼亽鍎插▓浼存⒑缁嬫鍎愰柟鐟版喘瀵鎮㈤崜鍙壭ч柟鑹版彧缁茬晫绮敍鍕＝濞达綀娅ｇ敮娑氱磼鐎ｎ偄绗掓い顓炴穿缁犳稑鈽夊Ο纰辨Ф闁荤喐绮岀粔鐟扮暦閸濆嫮鏆嗛柛鏇ㄥ厴閹疯櫣绱掔紒銏犲箹闁瑰啿姘﹂。璺ㄧ磽閸屾瑧璐伴柛鐘崇墪闇夐柣鎴ｆ缁€鍡涙煙閻戞﹩娈旂紒顐㈢Ч閺屾盯顢曢妶鍛€婚梺閫炲苯澧扮紒顕呭灡缁岃鲸绻濋崶鑸垫櫖闂佺粯鍔曢悺銊╂偟瀹曞洨纾藉ù锝夋涧婵″吋銇勯鐐叉Щ妞ゎ偄绻愮叅妞ゅ繐鎷嬪Λ鍐ㄢ攽閻愭潙鐏卞瀛樻倐椤㈡捇宕卞☉娆屾嫽闂佺ǹ鏈懝楣冾敂閳哄懏鍊垫慨妯煎帶婢ф挳鏌熼钘夊姢闁伙綇绻濋獮宥夘敊閼恒儳鏆﹂梻鍌欑窔閳ь剛鍋涢懟顖涙櫠閹绢喗鐓曢柍瑙勫劤娴滅偓淇婇悙顏勨偓鏍暜閹烘绐楁慨姗嗗墻閻掍粙鏌熼柇锕€骞樼紒鐘荤畺閺屾稑鈻庤箛锝嗩€嗛梺鍏兼緲濞硷繝寮婚弴銏犲耿婵°倐鍋撻柡鍡悼閳ь剝顫夊ú蹇涘礉瀹ュ拋鐒介煫鍥ㄧ☉缁€鍫㈡喐韫囨稑鐓熼柕鍫濇缁♀偓闂佹眹鍨藉褍鐡梻浣瑰濞插繘宕愬┑瀣畺鐟滄柨鐣烽崡鐐╂婵炲棗绻嗛崑鎾诲锤濡や胶鍘卞銈庡幗閸ㄥ灚绂嶉弽顬＄懓饪伴崟顓犵厜闂佸搫鐬奸崰鏍х暦椤愶箑绀嬫い鎺戭槹椤ワ絽鈹戦悙鑼憼缂侇喖绉瑰畷鏇㈠箮鐟欙絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯鍩勯崘鍓у姺闂佸疇顫夌粙鎾舵閹捐纾兼慨妯荤樂閿涘嫮纾奸柤鎼佹涧閸濆搫鈹戦垾宕囨憼缂佹鍠栭崺鈧い鎺嗗亾妞ゆ洩缍侀獮姗€顢欑喊杈ㄧ秱闂備焦鏋奸弲娑㈠疮椤栫偞鍋熼柡宥庡幗閳锋帒銆掑锝呬壕闂侀€炲苯澧伴柛瀣洴閹崇喖顢涘☉娆愮彿闂佽顔栭崰姘卞閸忕浜滈柡鍌氱仢閳锋棃鎮介娑氥€掔紒杈ㄥ笧缁辨帒螣閼测晝鏆ゆ俊鐐€ら崑鍛崲閸儯鈧礁顫滈埀顒勫箖濞嗘挸绾ч柛顭戝櫘閸ゆ瑩姊婚崒姘偓鎼佸磹閻戣姤鍊块柨鏇炲€哥粻鏍煕椤愶絾绀€缁剧偓瀵х换婵囩節閸屾粌顣虹紓浣插亾闁告劏鏂傛禍婊堢叓閸ャ劍灏靛褋鍨婚埀顒冾潐濞叉鍒掑畝鍕厴闁硅揪闄勯弲顒勬煕閺囥劌浜楅柡澶樺弮濮婅櫣绮欓崠鈩冩暰闂佹悶鍔岄悘婵嬫偩閻戣棄鍗抽柕蹇娾偓铏吇婵＄偑鍊栫敮濠勭矆娴ｈ鍎熷┑鐘叉处閳锋垿鏌涢幘鏉戠祷濞存粍绻勭槐鎺旀嫚閼碱儷銏°亜椤撶偞鍠樼€殿喕绮欐俊姝岊槾妞ゆ梹娲熼幃妤呮偡閺夋妫岄梺鍝ュУ濞叉粓鎳為柆宥呯缂佹妗ㄧ花濠氭⒑闂堟侗妲堕柛搴ゆ珪閺呰泛鈽夊杈╋紲缂傚倷鐒﹂…鍥虹€涙﹩娈介柣鎰▕閸庢棃鏌熼鐣屾噮闁圭懓瀚粭鐔碱敍濞戣鲸锛堝┑鐘殿暜缁辨洟宕戦幋锕€纾圭憸蹇擃嚗婵犲啰顩烽悗锝庝簽椤旀劕顪冮妶鍡楀Ё缂佺姵鍨块幃鈥愁潨閳ь剟寮婚悢鍛婄秶闁告挆鍛闂備胶绮幖顐ゆ崲濠靛钃熸繛鎴欏灩閻掓椽鏌涢幇鍓佺窗婵炲矈浜炵槐鎾存媴闂堟稑顬嬮梺鍛婎焼閸ワ絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯骞囬埡浣割瀳濡炪値鍓欓悧鎾愁潖濞差亜绠伴幖娣灩椤︹晜绻涚€涙鐭ゅù婊勭箖缁岃鲸绻濋崟銊ヤ壕婵炴垶鐟悞浠嬫煟閻旈绉洪柡灞界Х椤т線鏌涢幘瀵告噰妞ゃ垺宀搁弫鎰緞濡粯娅旈梻渚€鈧偛鑻晶顕€鏌ｉ敐鍥у幋妤犵偞甯￠獮瀣籍閳ь剟锝炲鍛斀闁宠棄妫楅悘锝夋煏閸℃鍎戞俊鍙夊姍閺佹捇鎮╅弬銉﹀濠电偠鎻紞鈧俊顐㈠瀹曘儴銇愰幒鎾跺幈闁诲函缍嗛崜娆愮瑜忕槐鎺楊敊閻ｅ本鍣伴悗瑙勬礀閵堝憡淇婇悜钘壩ㄧ憸婊兾ｉ懜鐢电瘈闁汇垽娼ч埢鍫熺箾娴ｅ啿娴傞弫鍕煙鐎电ǹ啸缂佲偓婵犲洦鐓涚€广儱楠告禍鎰版煕鐎ｎ偅宕岄柣娑卞櫍瀹曞綊顢欓悡搴經濠碉紕鍋戦崐褏绱撳璺虹闁规儼妫勯弰銉︾箾閹存瑥鐏╃紒鐙呯秮閺屻劑寮崒娑欑彧闂佸憡锕㈡禍璺侯潖濞差亜绠伴幖娣焺濮婂灝顪冮妶鍡樿偁闁告劖鍎冲▓銊╂⒑闂堟稈搴风紓鍌涙皑閹广垽宕卞☉娆戝幗闂侀€涘嵆濞佳勬櫠椤栫偞鐓曞┑鐘插暙婵秹鏌″畝瀣？濞寸媴绠撳畷婊嗩槼闁告帗绋掔换婵堝枈濡搫鈷夐梺鐑╁墲濞茬喕妫熼梺鍛婄懃椤︻厽绂嶅⿰鍫熺叆婵犻潧妫涙晶鏇灻瑰⿰鍐╄础缂佽鲸甯楀蹇涘Ω閵夛箒鐧侀柣搴ゎ潐濞叉鏁幒妤嬬稏婵犻潧鏌婇幒鎴旀瀻婵炲棙鍨硅ⅵ闂備礁鎼張顒€煤閻旈鏆﹀┑鍌氬閺佸棝鏌涚仦鍓ф晼婵犲﹤鐗婇崐鐢告偡濞嗗繐顏紒鈧崘顔界叆闁哄洦锚閻忚尙鈧娲樺浠嬪箖濞嗘挻鍊绘俊顖濄€€閸嬫捇鏌ㄧ€ｃ劋绨婚梺鐟版惈濡绂嶉幆褜娓婚柕鍫濇噽缁犳娊鏌涙惔娑樷偓妤呭箲閵忕姭鏀介悗锝庡亜娴犳椽姊婚崒姘卞缂佸鍔欏畷顐⑽旈崨顔规嫼闁荤喐鐟ョ€氼參濡甸悢璁跨懓饪伴崨顓濇埛濠碘€冲级閸旀瑩骞冨▎鎾充紶闁告洦鍘兼慨锔戒繆閻愵亜鈧牕顔忔繝姘；闁瑰墽绮悡鍐偡濞嗗繐顏╅柣蹇撶摠閵囧嫰濮€閿涘嫭鍣伴悗瑙勬处閸嬪﹤鐣烽悢鐓幬╅柕澶堝€曢ˉ姘節瀵伴攱婢橀埀顒佸姍瀹曟垿骞樼紒妯煎帗闂佽姤锚椤﹁棄螣閳ь剟姊虹拠鈥虫灓闁哄拋鍋嗛崚鎺戔枎閹炬娊鏁滃┑掳鍊愰崑鎾绘煛娴ｅ摜绉烘慨濠勭帛閹峰懘宕ㄦ繝鍐ㄥ壍婵犵數鍋涢惇浼村礉閹存繄鏆﹀ù鍏兼綑缁犲鎮归崶銊ョ祷闁稿绉瑰缁樼瑹閸パ冾潻缂備礁顦遍弫濠氬春濞戙垹绠ｉ柣妯兼暩閿涙繃绻涙潏鍓у埌闁告ɑ绮岃灋闁靛⿵璐熸禍婊堢叓閸ャ劍灏靛褎娲熼弻锝夊Χ鎼粹剝鐝濋梺鍝勭灱閸犳挾鍒掑▎鎾冲瀭妞ゆ洖鎳庢俊鐑芥⒒娴ｅ憡鍟為柣鐔村劤閹广垽骞掑Δ鈧悞鍨亜閹哄棗浜剧紓浣哄Т缁夌懓鐣烽弴銏犵闁诲繒绮浠嬪箖閳哄啯瀚氶柤纰卞墾缁遍亶姊绘担鍛婅础闁稿鎹囧鍛婄附缁嬪灝鍤戦梺鍝勫暙閻楀﹪鎮￠弴鐔翠簻妞ゆ挾鍠庨悘銉╂煛鐎ｎ剙鏋涢柡宀嬬秬缁犳盯骞樼捄琛″徍婵犳鍠栭敃銊モ枍閿濆應妲堥柣銏⑶瑰钘壝归敐鍛暈闁汇倕鍊荤槐鎾诲磼濞嗘劗銈版俊鐐差嚟鏋悡銈夋煟閺冨洤浜圭€规挷绶氶弻鐔告綇閸撗呮殸濡炪値鍋勯幊姗€寮婚敐鍜佹建闁糕剝銇炵花濂告⒑閸濆嫬鈧敻宕戦幘缁樷拻闁稿本鐟ㄩ崗宀€鐥鈩冪【妞ゎ剙锕ㄩˇ鍦偓娈垮枟瑜板啴銈导鏉戦唶婵犻潧鐗炵槐鎶芥⒒閸屾艾鈧悂鎮ф繝鍐懝婵°倐鍋撻柕鍡樺笚缁绘繂顫濋鐘插妇闂備礁澹婇崑鍛崲閸愵啟澶娾堪閸涱垳锛滃┑掳鍊撶粈浣该归鈧弻锛勪沪閸撗佲偓鎺楁煃瑜滈崜銊х礊閸℃顩叉繝闈涚懁婢舵劕顫呴柍鍨涙櫅娴滈箖鎮峰▎蹇擃仾濠㈣泛瀚伴弻娑㈠箻鐠虹儤鐏堝Δ鐘靛仜閸燁垳绮嬮幒鏂哄亾閿濆簼绨荤紒鎰⊕缁绘繈鎮介棃娑楁埛闂佺ǹ顑嗛幐楣冨焵椤掍椒浜㈡俊顐㈠閸╃偤骞嬮敂钘夆偓鐑芥煠閹间焦娑ф繛鎳峰懐纾介柛灞炬皑琚﹂梺绋款儐閹告悂鍩為幋锔藉€烽柛娆忣槸閻濇梻绱撴担鐟扮祷婵炲皷鈧剚鍤曟い鎰跺瘜閺佸﹪鎮樿箛鏃傚妞ゎ偄绉瑰娲濞戙垻宕紓浣介哺濞茬喎鐣烽姀銈嗙劶鐎广儱妫岄幏娲⒒閸屾氨澧涢柤鍐叉閵囨劙骞掗幋鐙呯吹闂傚倷绶￠崜娆戠矓鐎靛摜涓嶉柡宥庡幗閻撴瑩寮堕崼銉х暫婵＄虎鍣ｉ弻锝夊箻鐎靛憡鍣ч梺闈涙鐢帡锝炲┑瀣垫晢闁稿本鐟ㄥ绋库攽閻樻剚鍟忛柛鐘崇墵瀹曟劙骞栨担鍝ュ幋闂佺鎻梽鍕敁閹扮増鈷戠紒顖涙礃濞呭棝鏌ｅΔ浣圭妞ゃ垺宀告俊鐑藉煛娴ｄ警妲规俊鐐€栫敮鎺楀磹婵犳碍鍎婇柛顐犲劜閳锋垿鏌涘☉姗堟敾濠㈣泛瀚伴弻娑㈠箻鐎靛憡鍒涢悗瑙勬穿缂嶄礁鐣烽悢纰辨晬婵炴垶甯楃€氫粙姊绘担鍛靛綊寮甸鍕仭鐟滄棁妫熼梺鎸庢煥婢х晫澹曢悾灞稿亾楠炲灝鍔氶柟閿嬪灥椤斿繘濡烽埡鍌涘殙闂佺粯鍔楅崕銈夋偂閺囩姵鍠愰幖娣妸閳ь剙鍟村畷鍗炩槈濡》绱梻浣侯潒閸曞灚鐣烽梺缁樻尪閸庣敻寮婚敓鐘茬倞闁宠桨妞掔划鍫曟⒑闁偛鑻晶顕€鏌熼懞銉х煁缂佹梻鍠栧鎾偄閾忓湱妲囨繝娈垮枟閿曗晠宕㈡ィ鍐ㄥ偍闁瑰墽绮埛鎴︽倵閸︻厼校闁靛棗鍟扮槐鎺楀焵椤掍胶鐟归柍褜鍓欓锝夊箮閼恒儱浜滈梺鎯х箰濠€閬嶆晬濠婂牊鈷戠紓浣广€掗悷鎵殾闁汇垹鎲￠崑鈺傜箾瀹割喕绨奸柣鎾寸洴閹﹢鎮欓幓鎺嗘寖濠电偛寮堕幐鎶藉蓟濞戙垹鐓涘ù锝呭娴煎啴鎮楃憴鍕闁挎洏鍨归悾宄扳堪閸♀晜些濠电姵顔栭崰妤€顪冮挊澶樻綎缂備焦蓱婵挳鏌ｉ幋鐏活亜鈻撳畝鍕拺闁告縿鍎遍弸搴㈢箾绾绡€闁绘侗鍠楃换婵嬪炊瑜忛悿鈧俊鐐€栧濠氭偤閺傛鐒介柣鏂垮悑閳锋垿鏌涢敂璇插箹闁告柨顑夐弻鐔煎传閵壯冪闂佸憡甯掗敃顏堢嵁濮椻偓椤㈡瑩鎮剧仦钘夌疄濠电姷鏁告繛鈧繛浣冲吘娑樜旈崪浣规櫆闂佸啿鎼崑鍡椻枔娴犲鐓熼柟閭﹀墯閳绘洟鏌涙惔銏♀拹闁逛究鍔嶇换婵嬪礃閳瑰じ铏庨柣搴ゎ潐濞诧箓宕戞繝鍌滄殾闁绘梻鈷堥弫濠勭棯閺夊灝缍佺紒杈ㄦ⒒缁辨捇宕掑顑藉亾妞嬪海鐭嗗〒姘ｅ亾閽樻繈姊洪鈧粔瀵哥矆閸屾稐绻嗘い鏍ㄧ箓娴滆淇婇弻銉︽锭闁宠鍨块幃娆戔偓娑櫭棄宥夋⒑缁洘娅呴柛鐔告綑閻ｇ兘骞嬮敃鈧粻鑽ょ磽娴ｈ鐒介柛妯绘倐閺岋綀绠涢弴鐐扮捕婵犫拃鍡橆棄閻撱倝鎮楀☉娆欎緵婵炲牅绮欓弻锝夊箛椤栨稑娑ч梺鍏煎濞夋洟鍩€椤掑喚娼愭繛娴嬫櫇閹广垹鈹戠€ｎ亞鐣哄┑鐐叉濞存岸宕崨顔轰簻闁哄啫鍊瑰▍鏇犵磼婢跺﹦浠涚紒缁樼箓椤曘儵鏌ㄧ€ｎ偆鍑￠梺閫炲苯澧い銊ユ楠炲牓濡搁埡鍌氣偓濠氭煢濡警妲洪柨娑欑☉閳规垿鎮欓弶鎴犱桓缂佺偓婢樼粔鎾偩閻戣棄鐭楀璺侯儏瀵灝鈹戦敍鍕哗妞ゆ泦鍕洸濡わ絽鍟崐鍨箾閸繄浠㈡繛鍛耿閺屾稓鈧綆浜烽煬顒勬煟濞戝崬鏋涢摶锝夋煠濞村娅囬柨娑欑矊閳规垿鍩ラ崱妤冧化缂備緡鍣崹鍫曘€侀弮鍫熷亜闁稿繐鐨烽幏缁樼箾鏉堝墽绉┑顔哄€楀☉鐢稿醇閺囩喓鍘遍梺鎸庣箓缁绘帡鎮鹃崹顐闁绘劘灏欑粻濠氭煛娴ｈ宕岄柡浣规崌閺佹捇鏁撻敓锟�
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
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁诡垎鍐ｆ寖闂佺娅曢幑鍥灳閺冨牆绀冩い蹇庣娴滈箖鏌ㄥ┑鍡欏嚬缂併劌銈搁弻鐔兼儌閸濄儳袦闂佸搫鐭夌紞渚€銆佸鈧幃娆撳箹椤撶噥妫ч梻鍌欑窔濞佳兾涘▎鎴炴殰闁圭儤顨愮紞鏍ㄧ節闂堟侗鍎愰柡鍛叀閺屾稑鈽夐崡鐐差潻濡炪們鍎查懝楣冨煘閹寸偛绠犻梺绋匡攻椤ㄥ棝骞堥妸鈺傚€婚柦妯侯槺閿涙稑鈹戦悙鏉戠亶闁瑰磭鍋ゅ畷鍫曨敆娴ｉ晲缂撶紓鍌欑椤戝棛鈧瑳鍥ㄥ€垫い鎺戝閸婂灚顨ラ悙鑼虎闁告梹纰嶉妵鍕晜鐠囪尙浠紓渚囧枛閻楀繘鍩€椤掑﹦绉甸柛瀣╃劍缁傚秴饪伴崼鐔哄幍闂佸憡绻傜€氼喛鍊撮梻浣告啞閺屻劎绮旈悽绋课﹂柛鏇ㄥ灠濡﹢鏌熺粙鍧楊€楅柡瀣Т閳规垿顢欑涵宄板缂備緡鍣崹鍫曠嵁韫囨稑宸濇い鏍ㄧ矌閻撴捇姊洪崷顓€鍦偓娑掓櫊閹線鏁愭径瀣ф嫽婵炶揪绲介幉锛勬嫻閿熺姵鐓冪€瑰嫭澹嗘晶锔锯偓瑙勬礃閸ㄥ墎鎹㈠┑瀣倞闁宠桨鑳堕妶锕€鈹戦悩鍨毄濠殿喗鎸冲畷顖烆敍閻愯尙锛熼梺缁樻煥閸氬鎮￠妷锔剧瘈闂傚牊绋掗ˉ鐐测攽椤栨艾鎮戝ǎ鍥э躬椤㈡洟鏁愰崶鈺冨帒缂傚倷鑳剁划顖滄崲閸儱绠栧ù鐘差儐椤ュ牊绻涚壕瀣靛晣缂佹彃娼￠幆鈧い蹇撶墕缁狙囨偣閹帒澹夌憸鏃堝蓟閻旂⒈鏁嬮柛鈩冪懅閻﹀牓鎮楀▓鍨灍闁规瓕娅曢幈銊╁焵椤掑嫭鐓ユ繛鎴灻鈺傤殽閻愭潙濮嶆慨濠勭帛閹峰懐鎲撮崟顐″摋闂備胶枪鐎垫帡宕圭捄铏规殾闁哄洨鍠愮紞鍥煏婵炲灝鈧绮诲鑸碘拺缂佸娉曠粻鎶芥煃瀹勬壆澧曢柍缁樻尰缁傛帞鈧綆鍋嗛崢浠嬫⒑瑜版帒浜伴柛妯圭矙瀹曟洖螖娴ｈ櫣顔曢梺鍓插亞閸ｏ箓鎳撻崸妤佺厸閻忕偟鏅暩濡炪伇鍌滅獢闁哄本鐩獮妯尖偓闈涙憸閻ゅ嫰姊洪幐搴ｇ畼闁稿鍋涢銉╁礋椤掑倻顔曢梺鍦劋閸ㄥ爼骞嗛崼鐔翠簻妞ゆ劧绲块惌鎺楁煕閳规儳浜炬俊鐐€栫敮鎺斺偓姘煎墰缁寮介鐔哄弳闂佺粯鏌ㄩ幖顐ｇ墡闂備礁鎼幊蹇涘箖閸岀偛钃熼柨婵嗘啒閻旂厧鍨傛い鏃€瀵ч弳閬嶆⒒娴ｄ警鏀板┑顔哄€曠叅闁绘柨顨庡鏍煣韫囨凹娼愮€规洖顦甸弻鏇熺箾閸喖濮曢梺璇茬箣閻掞妇鎹㈠┑鍡忔灁闁割煈鍠楅悘宥夋⒑閻熺増鍟炲┑鐐诧躬閻涱噣宕橀妸銏＄€婚梺褰掑亰閸犳岸鎯侀崼銉︹拺闁硅偐鍋涢崝姗€鏌涢弬鍧楀弰鐎规洏鍨介獮妯肩磼濡厧骞愬┑鐘灱濞夋盯鎳熼娑氼浄闁挎洍鍋撴い顓炴健閹兘鎮ч崼顐ｏ紗闁诲氦顫夊ú妯兼崲閸岀儐鏁囬柛蹇曞帶缁剁偤鎮楅敐搴″妤犵偞顨婂缁樼節鎼粹€茬盎濠电偛鐪伴崐鏇㈡箒闂佺粯鍨垫竟娆撳炊椤掍焦娅嗘繝娈垮枟閸旀帞鑺辩拠宸富闁靛牆鎳愮粻浼存倵濮樼厧澧寸€殿喗濞婇弫鎰板川椤忓懏鏉搁梻浣哄仺閸庤京澹曢銏犳槬闁挎繂娲犻崑鎾舵喆閸曨剛顦ㄩ柣銏╁灡鐢繝宕洪妷锕€绶炲┑鐐靛亾閻庡姊洪悷鎵憼缂佽瀚划娆愮節濮橆厸鎷婚梺绋挎湰閼归箖鍩€椤掑倸鍘撮柟铏殜瀹曟粍鎷呯粙璺ㄤ喊婵＄偑鍊栭悧婊堝磻濞戞氨涓嶉悷娆忓娴滄粓鏌熼幍铏珔闁逞屽墯閻楁洟鎮鹃悜钘夌煑濠㈣泛鐬奸惁鍫ユ⒒閸屾氨澧涚紒瀣浮閺佸秴顓兼径瀣幈闂侀潧鐗嗗Λ娆戠矆鐎ｎ喗鐓欐い鏃囧亹缁夎櫣鈧娲栭悥鍏间繆濮濆矈妲归梺绋款儑閸嬫盯鍩為幋锕€鐒洪柛鎰╁妿缁佺兘姊虹紒姗嗘畷闁圭懓娲ら锝嗙節濮橆剙宓嗛梺鎸庣☉鐎氼噣寮堕幖浣光拺闁告繂瀚婵嬫煕婵犲骸浜板┑锛勫厴閸╋繝宕掑⿰鍐ㄧ疄闂傚倷绀侀幖顐﹀疮閻樿纾婚柟鍓х帛閻撴瑧绱掔€ｎ亞浠㈤柡鍡氶哺閵囧嫰顢曢姀銏㈩啋闂佸湱鍘х紞濠傜暦閻戠瓔鏁囬柣姗€娼ч獮鍡涙⒒娴ｈ棄鍚瑰┑顔肩仛缁傚秵绂掔€ｎ亞顦柟鍏兼儗閻撳牓寮繝鍌楁斀闁绘ɑ褰冮弳鐔虹磼閳锯偓閸嬫挻绻濋悽闈涗粶闁绘妫濋幃妯衡攽鐎ｎ偄鈧爼姊洪鈧粔鐢告偂閺囩喍绻嗘い鏍ㄨ壘閹垶绻涢崨顔界缂佽鲸甯￠、娆戝枈閸楃偛澹堟俊鐐€栭幐鎼佸Χ缁嬭法鏆﹂柕濞炬櫓閺佸倿鏌涘☉鍗炴灍婵犮垺鍨垮缁樻媴缁涘娈梺鍛婂灩閺咁偆妲愰悙鍝勫耿婵☆垳鈷堝ú鎼佹⒑缂佹ê濮夐柛搴涘€濆鏌ヮ敆閸曨剛鍘介梺鍝勫€搁悘婵嬪箖閹达附鐓冮梺鍨儏閻忊晝绱掓潏銊﹀鞍闁瑰嘲鎳愰幏鐘绘晬閸曨偄楔闂傚倷鐒﹀鍧楀储瑜版帒纾规繝闈涱儏閽冪喖鏌ｉ弬鍨倯闁稿鏅犻幃姗€鎮欓弶鎴濆Б闂佸搫鎳愰妵鏍⒒閸屾艾鈧悂宕愰幖浣哥９闁归棿绀佺壕褰掓煙闂傚顦︾痪鎯х秺閺岀喖姊荤€靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁诡垎鍐ｆ寖闂佺娅曢幑鍥灳閺冨牆绀冩い蹇庣娴滈箖鏌ㄥ┑鍡欏嚬缂併劌銈搁弻鐔兼儌閸濄儳袦闂佸搫鐭夌紞渚€銆佸鈧幃娆撳箹椤撶噥妫ч梻鍌欑窔濞佳兾涘▎鎴炴殰闁圭儤顨愮紞鏍ㄧ節闂堟侗鍎愰柡鍛叀閺屾稑鈽夐崡鐐差潻濡炪們鍎查懝楣冨煘閹寸偛绠犻梺绋匡攻椤ㄥ棝骞堥妸鈺傚€婚柦妯侯槺閿涙稑鈹戦悙鏉戠亶闁瑰磭鍋ゅ畷鍫曨敆娴ｉ晲缂撶紓鍌欑椤戝棛鈧瑳鍥ㄥ€垫い鎺戝閸婂灚顨ラ悙鑼虎闁告梹纰嶉妵鍕晜鐠囪尙浠紓渚囧枛閻楀繘鍩€椤掑﹦绉甸柛瀣╃劍缁傚秴饪伴崼鐔哄幍闂侀€涚祷濞呮洖鈻嶉崨瀛樼厽闊浄绲介弸娑㈡婢跺绡€濠电偞鍎虫禍楣冩⒑閸涘⿴娈曞┑鐐诧工椤曪絾绻濆顓熸珳闂佺硶鍓濋敋闁告柨鎳樺娲倷閽樺濮庨梺鍝ュ枑鐎笛冾焽韫囨稑宸濋悗娑櫱氶幏铏圭磽娴ｅ壊鍎愭い鎴炵懇瀹曟洟骞囬悧鍫㈠幍闂佽崵鍠撴晶妤呭窗濡皷鍋撳▓鍨灕妞ゆ泦鍥х叀濠㈣埖鍔曢～鍛存煟濡绲荤紒渚囧櫍濮婄粯鎷呴搹鐟扮闂佸憡姊瑰ú鐔风暦閵夆晛鐒垫い鎺戝€荤壕鍏笺亜閺冨倸浜鹃柡鍡╁墯閹便劍绻濋崘鈹夸虎閻庤娲﹂崑濠傜暦閻旂⒈鏁嗛柍褜鍓欓埢宥呪攽鐎ｎ偀鎷洪梺鍛婄箓鐎氼參宕抽挊澶嗘斀妞ゆ棁濮ょ亸锕傛煥濠靛牆浠╃紒鐘崇☉閳藉鈻庨幇顒€濮冮梻浣藉吹閸犳劙鎮烽妷褉鍋撳鐓庡⒋鐎规洏鍨奸妵鎰板箳閹绢垱瀚奸梻浣告啞缁嬫垿鏁冮敐鍥偨闂侇剙绉甸悡鏇熶繆椤栨稐鑸柛瀣ㄥ灪椤ㄣ儵鎮欓崣澶樻＆闂佽鍠楅悷鈺呭箖娴犲鍋ㄧ痪鏉款槺闂傤垱绻濋悽闈涗哗闁规椿浜炲濠冪鐎ｎ亞顔戝┑鐘诧工閻楀﹪宕愰崸妤佸仯闁诡厽甯掓俊璺ㄧ棯閹佸仮闁哄矉绲介埢搴ｄ沪閹存帒顥氶梻鍌欑劍閹爼宕濊箛鎾灃婵炴埈婢佺紞鏍叓閸ャ劍鈷掔紒鈾€鍋撴繝娈垮枟閿曗晠宕滃☉銏℃櫖婵犲﹤鍟犻弨鑺ャ亜閺冣偓閺嬬粯绗熷☉銏＄厱闊洦妫戦懓璺ㄢ偓瑙勬礃閸ㄥ潡鐛Ο鍏煎珰闁肩⒈鍓﹂崯鍥р攽閻愯埖褰х紒韫矙楠炴饪伴崼鐔告珫濠电偛妫欓幐濠氬煕閹寸姷纾藉ù锝咁潠椤忓牊鍊堕柛妤冨亹閺€鑺ャ亜閺傛寧鎯堥柍褜鍓氱换鍌炴偩閻戣棄绠涢柡澶婃健閺佹粌鈹戞幊閸婃捇鎳楅崼鏇炵煑闁糕剝绋掗埛鎴︽煕濠靛棗顏€瑰憡绻堥弻娑氣偓锝庡亞濞叉挳鏌涢埞鎯т壕婵＄偑鍊栧濠氬磻閹惧墎纾界€广儱鎷戝銉╂煟閿濆洤鍘寸€殿喗鎸虫慨鈧柣妯活問閸氬懘姊绘担铏瑰笡闁挎岸鏌涢悢閿嬪仴鐎殿喗鎮傞獮瀣晜鐟欙絾瀚肩紓鍌氬€烽悞锕傛晝閳轰讲鏋旈柡鍐ㄧ墛閻撶喐淇婇妶鍌氫壕濠碘槅鍋呯粙鎾诲礆閹烘鏁囬柕蹇曞Х椤斿﹪姊洪崫鍕殭闁稿﹤顭烽弫宥呪堪閸啿鎷洪梻渚囧亞閸嬫盯鎳熼娑欐珷妞ゆ牗澹曢崑鎾诲垂椤愶絿鍑￠柣搴㈠嚬閸犳绮嬮幒妤佹櫆闁告挆鍛幆濠电姰鍨规晶搴㈢仚濡炪們鍎插畝绋款潖缂佹ɑ濯撮柛娑㈡涧濠€閬嶅箲閵忋倕绠涢柛蹇ｅ亜濞差參銆佸☉妯锋婵ê鍚嬮柨銈呪攽鎺抽崐褏寰婃禒瀣柈妞ゆ牜鍋涢悡鏇㈡煙鏉堥箖妾柍閿嬪灦閵囧嫰骞囬埡浣插亾閺囩喓顩锋繝濠傜墛閻撶姵绻涢懠棰濆殭闁诲骏绻濋弻锟犲川椤撶姴鐓熷Δ鐘靛仦閿曘垽銆佸▎鎾村仼閻忕偠妫勭粻鐐烘⒒閸屾瑧顦﹂柣銈呮喘閿濈偞寰勯幇顒傦紵闂佹儳娴氶崑鍡涖€呴弻銉︾厽闁逛即娼ч崢鍝ョ磼鐠囧弶顥㈤柡宀嬬秮楠炲洭宕楅崫銉ф晨闂備線鈧偛鑻晶顖炴煟濡や胶鐭掔€殿噮鍋婇、娆撴偩瀹€鈧惁鍫ユ⒑閸涘﹥澶勯柛姗€绠栭幃鐐哄礂閼测晝顔曢梺鍦亾濞兼瑩鎮炶ぐ鎺撶厱閻庯綆鍋呯亸鐢告煃瑜滈崜婵嬶綖婢跺⊕鍝勨堪閸繃鐎繝鐢靛У閸濆酣鍩€椤戣法顦﹂摶鏍煕濞戝崬骞樻い锔芥緲椤啴濡堕崱妤冪懆闂佺ǹ锕ラ幃鍌炴偘椤曗偓瀹曞ジ濡烽敂瑙勫濠电偞鎸婚崺鍐磻閹惧绠惧ù锝呭暱鐎氼喗绂掑鑸碘拻濞达絽鎳欓崷顓涘亾濞戞帗娅嗙紒缁樼⊕缁绘繈宕掑⿰鍕啎婵犵數鍋涘Ο濠冪濠靛鐤鹃柟缁樺俯閻斿棝鎮规潪鎷岊劅闁稿孩鍨块弻娑橆潨閸℃洟鍋楀┑顔硷龚濞咃綁宕犻弽顓炲嵆闁绘劖绁撮幏銈嗙節閻㈤潧浠滅€殿喖澧庨崚鎺楀箻鐟欏嫷妫ㄩ梻鍌欑劍鐎笛囧蓟閵娾晪缍栭柡鍥╁枔椤╁弶绻濇繝鍌氼仴濞存粍绮撻弻鐔煎级閸噮鏆㈤梺璇″枦閸嬫劗妲愰幒妤佸亹缂佹稓顢婇埀顒€娼￠弻鈥崇暆閳ь剟宕伴弽顓炵畺闁绘垼妫勭痪褎绻涢崱妤€缍栭柣銉簽缁辨捇宕掑▎鎴濆濡炪們鍔岄悧鎾崇暦娴兼潙鍐€妞ゆ挻澹曢崑鎾存媴缁洘顫嶅┑鈽嗗灦閺€閬嶏綖瀹ュ應鏀芥い鏃傚嵆閹达附鍎婇柣鎴ｅГ閸嬪嫰鏌涜箛鎾舵癁闁瑰嘲顭峰铏圭矙閹稿孩鎷遍梺鐓庣秺缁犳牠宕洪埀顒併亜閹烘垵顏存俊顐ｅ灦缁绘盯宕遍幇顒備患濡炪値鍋呯换鍫ャ€佸Δ浣哥窞閻忕偠妫勬闂備浇宕甸崰鎰垝瀹ュ憘娑㈠礃椤旀儳绁﹂梺鍛婂姦閸欏骸螣閸曨垱鍊甸悷娆忓缁€鈧悗瑙勬处閸撶喖宕洪妷锕€绶炲┑鐐灮閸犳牠骞婇弽顓炵厸闁稿本澹曢崑鎾活敋閳ь剙顫忔繝姘＜婵炲棙鍔楅妶鏉款渻閵堝骸浜滄い锔藉缁晠鎮㈤悡搴¤€垮┑鈽嗗灣缁垶鎮靛畡鎵虫斀閹烘娊宕愰弴銏犵疇閹艰揪绲介ˉ姘攽閻樺磭顣查柣鎾存礈閳ь剙鍘滈崑鎾绘煃瑜滈崜鐔风暦閹达附鍊烽柣鎴灻禍妤€鈹戦悙鍙夘棡闁圭ǹ鎽滈惀顏囶樄闁哄本鐩、鏇㈡偐閹绘帒袘闂備椒绱徊鍧楀磿閵堝棛鈹嶅┑鐘插绾惧ジ鏌曡箛鏇炐ｉ柣婵囩箞濮婃椽宕崟顔碱伃缂備礁顦紞濠傤嚕婵犳艾惟鐟滃宕戦幘缁樻櫜閹煎瓨绻勯弫鏍ㄧ箾鐎电ǹ甯堕柤娲诲灦閸╃偤骞嬮敂缁樻櫓缂佺虎鍘奸崲鍙夊閹邦剦娓婚柕鍫濋楠炴牠鏌涢姀锛勫弨闁诡噣绠栭弻銊р偓锝庡墴濡绢噣姊洪崨濠勨槈闁挎洏鍊栫粋宥呪堪閸啿鎷洪梺鍛婄☉閿曘儳浜搁锔界厾闁告劖褰冮。宕囩磼椤旂⒈鐓兼鐐村浮楠炴﹢宕滄担鐑橆潓缂傚倷绀佹晶鑺ユ櫠濡ゅ懏鏅┑鐘媰閸℃姣㈤梺鐟板级閹倸顕ｉ崼鏇炲瀭妞ゆ棁濮ら鎺楁⒒娴ｇ瓔鍤冮柛鐘虫崌瀹曟洟鎳犻鍌滃闂佷紮绲介惉濂告儗濞嗘挻鐓犻柟棰佺閻忥繝鏌熼懠顒€顣肩紒缁樼箞閹粙妫冨☉妤佸媰闂備焦鎮堕崝宀勬偉婵傜ǹ绠栭柣鎴ｅГ閺呮繈鏌涚仦鐐殤闁挎稓鍠栧铏圭磼濡崵鍙嗛梺鍛婅壘椤戝骞冮檱缁犳稑鈽夊顒€绲奸梻浣规偠閸庢粎浠﹂挊澶婎棈闂傚倷娴囬褏鎹㈤崱娑樼柧婵犻潧顑呴悞鍨亜閹哄秶璐伴柛鐔风箻閺屾盯鎮╅搹顐㈢３閻庤娲樼换鍫ョ嵁鐎ｎ喗鏅濋柍褜鍓涚划濠氬冀閵娿倗绠氶梺闈涚墕閹冲酣寮抽悙鐑樼厱闁绘ê寮堕ˉ銏ゆ煛鐏炲墽娲寸€殿喗鎸虫俊鎼佸Ψ閵夘喗楠勫┑锛勫亼閸娧呮嫻閻旂厧绀夐柟瀛樼箥濞兼牗绻涘顔荤盎缂佺姴缍婇弻锝夊箛椤撶偟绁峰銈庡墮椤︾敻寮婚敐鍡樺劅闁挎繂娲﹂崵鍕⒑閸涘﹥澶勯柛妯绘倐瀵劑鎼归銈囩槇闂佹眹鍨藉褍鐡繝鐢靛仩椤曟粎绮婚幘宕囨殾闁归偊鍘剧弧鈧┑顔斤供閸橀箖宕ｉ崘銊㈡斀闁宠棄妫楅悘鐔兼偣閳ь剟鏁冮埀顒€宓勫┑鐐叉▕娴滄繈鎮￠弴鐔剁箚妞ゆ牗绮庣敮娑㈡煕婵犲啫濮堥柟渚垮妽缁绘繈宕ㄩ鍛摋缂傚倷绶￠崰妤呮偡閳哄懐宓侀柟閭﹀幗閸庣喖鏌曡箛濞惧亾閹颁椒绱楃紓鍌氬€搁崐鎼佸磹妞嬪孩顐介柨鐔哄Т缁愭鏌″搴″箺闁稿孩顨婇弻娑樼暆閳ь剟宕戦悙鍝勭；婵☆垱鐪规禍婊堟煛閸ヮ煈娈斿ù婊勫劤铻栭柣姗€娼ф禒婊勭箾瀹割喖寮鐐插暢椤﹀湱鈧娲滈崢褔锝炲┑瀣垫晣闁绘垵妫旂槐鍙夌節閻㈤潧啸闁轰焦鎮傚畷鎴︽偐鐠囪尙顔屽銈呯箰濡娆㈤妶澶嬬厵缂佸瀵ч幑锝吤归悩宕囶暡濞ｅ洤锕幃娆擃敂閸曘劌浜鹃柟杈剧畱绾惧潡寮堕崼姘珕鐎规洖寮堕幈銊ヮ渻鐠囪弓澹曢柣搴㈩問閸犳盯顢氳閸┿儲寰勬繝搴㈠兊闂佹寧绻傞幊宥嗙珶閺囥垺鈷戦柟绋挎捣缁犳挾绱掗妸銊ヤ汗闁奸缚椴哥缓浠嬪川婵犲嫬骞堥梻渚€娼ч悧鍡涘箠韫囨稑绐楁慨妞诲亾闁哄备鈧磭鏆嗛柍褜鍓熷畷浼村冀椤撶偟顔愰悷婊呭鐢晠寮崘顔界厪闁割偅绻冨婵嬫煛婢跺鐭欐慨濠勭帛閹峰懐绮电€ｎ亝鐣伴梻浣告憸婵敻銆冩繝鍥х畺闁绘垼妫勯悡娑㈡煕濞戝崬浜愰柛瀣尰閹峰懘鎳栧┑鍥棃鐎规洏鍔戦、姗€鎮崨顖氱哎闂傚倸鍊风欢姘焽瑜旈幃褔宕卞銏＄☉閳规垹鈧綆浜濋悗顒€鈹戦悩璇у伐閻庢凹鍠氬褔鍩€椤掑嫭鍊甸柛蹇暻瑰畵鍡涙煛閳ь剟宕￠悜鍡欏數濠殿喗銇涢崑鎾绘煕閳规儳浜炬俊鐐€栭崺鍫ュ礈濞嗘挸绠犻幖杈剧稻椤愪粙鏌曢崼婵囧仾鐟滅増甯楅崑鎰偓鐟板閸犳牠宕滄导瀛樷拺婵懓娲ら埀顑惧€濆畷鏉课旈崨顓囷箓鏌涢弴銊ョ仩闁告劏鍋撴俊鐐€栭崝锕€顭块埀顒傜磼椤旇偐鍩ｆ慨濠呮缁辨帒螣鐠囨煡鐎烘繝鐢靛仩鐏忔瑩宕伴弽褜鍤曞┑鐘崇閸嬪嫰鏌涘☉姗堝姛缂佸鍩栨穱濠囧Χ韫囨洖鍩岄梺鍝ュ櫏閸ㄥ爼銆侀幘鎰佸悑濠㈣泛顑傞幏铏圭磽閸屾瑧鍔嶉拑閬嶆倶韫囨洖顣肩紒缁樼洴瀹曪絾寰勫Ο鐓庡Ш闂備胶鎳撶粻宥夊垂瑜版帒鐓″鑸靛姇椤懘鏌ｅΟ娲诲晱闁告艾鎳樺缁樻媴閾忕懓绗￠梺鎼炲姂濞佳呭弲闂侀潧鐗嗛ˇ顖炴儗濡ゅ懏鐓涢柛銉㈡櫅閺嬫垿鏌涘▎蹇曠闁哄瞼鍠栭幃婊兾熼懖鈺冩殼濠电偛鐡ㄧ划宥囨崲閸儱钃熼柕鍫濈墑娴滃綊鏌熼悜妯烩拻闁哄棭鍋婂娲倷閽樺濮锋繝纰樷偓铏枠妤犵偞鍨垮畷鐔碱敍濮ｅ皷鏅犻弻銊╁即濡も偓娴滈箖姊洪崫鍕靛剰闁稿﹤鐏濋～蹇撁洪鍕獩婵犵數濮撮崯浼此囬銏♀拺閺夌偞澹嗛崝宥夋煙椤旂厧鈧悂鎮惧畡鎵虫斀闁搞儴鍩栨晥闂佺澹堥幓顏嗗緤閸ф鏅繝闈涙川缁犻箖寮堕崼婵嗏挃閻庝絻鍋愮槐鎺楊敊閻ｅ本鍣伴悗瑙勬处閸嬪嫮鍙呭銈呯箰閹冲孩绂掗崫鍕垫富闁靛牆妫涙晶顒傜磼鐎ｎ偄鐏ラ棁澶嬨亜閺囨浜鹃梺鍝勭灱閸犳牠銆佸▎鎾村癄濠㈣泛锕よ闂傚倷绀侀悿鍥綖婢舵劕鍨傜憸鐗堝笒閻掑灚銇勯幒鎴姛缂佸娼ч湁婵犲﹤鎳庢禒杈┾偓瑙勬礃缁诲牓寮崘顔肩劦妞ゆ帒瀚ч埀顒佹瀹曟﹢顢欓崲澹洦鐓曢柍鈺佸枤濞堟﹢鏌ｉ悢鏉戝闁哄矉绲鹃幆鏃堝閻樺弶鐦撻梻浣告啞閹歌崵绮欓幘璇茬闁圭儤鎸剧弧鈧┑顔斤供閸撴繈骞楅弴銏♀拺闂傚牊渚楀褍鈹戦垾铏枠鐎规洏鍨介弻鍡楊吋閸″繑瀚奸梻鍌氬€搁悧濠冪瑹濡も偓铻ｉ柛顐犲劜閻撴洟鏌ｅΟ铏癸紞濠⒀屽墴閺岋繝宕ㄩ鐘茬厽濡炪們鍨洪惄顖炲箖濞嗘垟鍋撻悽鐧诲湱鏁Δ鍛拻闁稿本鐟чˇ锕傛煟韫囨梻绠炴い銏＄墵瀹曘劑顢涘⿰鍛殽闂備礁婀遍崕銈夈€冮崱娴板宕卞Δ濠勫數闂佸吋鎮傚褎鎱ㄩ崼銉︾厽闁规儳宕崝婊呯磼缂佹绠炴俊顐㈠暙閳藉鈻庡Ο浼欓獜婵犵數濮幏鍐幢濞戞ɑ鎳欑紓鍌欒兌缁垶鎯勯姘辨殾婵°倕鎳忛崑鍌炲箹缁顫婃繛鍏煎哺濮婄粯鎷呴崨濠冨創闂佸搫鐗滈崜娑氬垝濞嗘挸绠婚悹鍥皺閸旓箑顪冮妶鍡楃瑨闁挎洩濡囩划鏃堟偨閸涘﹦鍘遍梺瑙勫劤椤曨參骞婇崶顒佺厱闁崇懓鐏濋悘鑼偓瑙勬礀閵堝憡淇婇悜鑺ユ櫆闁诡垎鍐唶闂傚倷娴囬褏鎹㈤幒妤€纾绘繛鎴炵懄濞呯姴霉閻樺樊鍎忛柣鎾亾闂備礁鎼ú銏ゅ垂濞差亝鍋傛繛鍡樺姂娴滄粓鏌″鍐ㄥ闁愁垱娲橀妵鍕Ψ閵壯冾暫闂佸疇顫夐崹鍧椼€佸▎鎾村仭濡绀侀ˉ姘舵⒒閸屾艾鈧兘鎳楅崜浣稿灊妞ゆ牜鍋為弲婵嬫煃閸濆嫭鍣圭痪鎯ь煼閺屾稑鈽夊Ο鍏兼喖闂佺ǹ锕ら悥濂稿蓟濞戙垹鐒洪柛鎰典簴婵洭姊洪幎鑺ユ暠閻㈩垱甯″﹢渚€姊洪幐搴ｇ畵闁绘锕、鏃堟偐缂佹鍘梺绯曞墲椤ㄦ劙鐓鍕厸鐎光偓鐎ｎ剛鐦堥悗瑙勬礃鐢帡鍩㈡惔銊ョ闁绘﹢娼ф惔濠囨⒑鐠囧弶鍞夋い顐㈩槸鐓ゆ慨妞诲亾鐎规洖缍婂畷绋课旈崘銊с偊婵犳鍠楅妵娑㈠磻閹炬惌娈介柣鎰级婢跺嫰鏌熷畡鐗堝殗闁诡喚鍏橀獮宥夘敊閸欘偅甯″濠氬磼濮橆兘鍋撴搴㈩偨婵﹩鍓﹂悞鐣屾喐閺冨牆绠栫憸鏂跨暦閸楃儐娓婚柕蹇ョ磿閳藉鎽堕弽顓熺厱婵炴垵宕顕€鏌℃担鍦煁缂佺粯绻堥幃浠嬫濞磋翰鍨虹换娑氭嫚瑜忛悾鐢告煙椤曞棛绡€鐎殿喗鎸虫慨鈧柍銉︽灱閸嬫捇鎮介崨濠勫弳濠电娀娼уΛ婵嬵敁濡も偓闇夋繝濠傚缁犳﹢鏌嶈閸撴繈锝炴径濞掓椽寮介鐐茬彉濡炪倖甯掔€氼剛绮婚悙鐑樼厪濠电姴绻愰々顒傜磼閳锯偓閸嬫捇姊绘担鍛婂暈闁告柨绻樺顒勫磼濞戞凹娴勯梺闈涚箞閸婃牠鍩涢幋锔藉仯闁搞儻绲洪崑鎾绘惞椤愩倓閭繝鐢靛Л閹峰啴宕卞銉︽尵缁辨帡宕掑☉妯昏癁闂佽桨绀侀崐鍨暦濠婂棭妲鹃梺缁樻尰閹歌崵鎹㈠┑瀣潊闁绘ê妯婇弳銏ゆ煛鐎ｅ吀绨奸柕鍥у婵＄兘鏁冮埀顒勫传濞差亝鐓涚€光偓鐎ｎ剛蓱闂佽鍨卞Λ鍐垂妤ｅ啫绀冮柤鐓庡缁愶繝姊婚崒娆掑厡閺嬵亝銇勯幋婵囶棦妤犵偞鍨垮畷鍫曨敆閳ь剟鎷戦悢鍝ョ闁瑰瓨鐟ラ悘鈺呮煕濡や礁鈻曢柡灞炬礃缁绘盯宕归鐓庮潛婵°倗濮烽崑娑㈩敄婢跺⿴娼栧Δ锕侊骏娴滃綊鏌熼悜妯诲鞍闁哄棛濮撮埞鎴︽晬閸曨偂鍝楀┑鈽嗗亜鐎氫即鏁愰悙鍓佺杸闁瑰彞鐒﹀浠嬨€侀弮鍫濆窛妞ゆ挻绻勯弳妤呮⒒娴ｇ儤鍤€闁告艾顑夐獮鍡楃暆閸曨偆鐛ュ┑掳鍊曢崯鎵矆婵犲啩绻嗛柕鍫濇噺閸ｆ椽鏌嶉柨瀣仼缂佽鲸甯為埀顒婄秵娴滄粓鎯冮敓鐘崇厸闁逞屽墰缁辨帒螣閼测晩鍟庨梻浣告啞閻熴儵藝鏉堛劍娅犻柣銏犳啞閻撴瑦銇勯弮鍥撻柕鍥ㄧ箖閵囧嫰濮€閳藉懓鈧潡鏌熼鍝勭伈闁圭厧缍婂畷鐑筋敇閻曚焦缍嬮梻鍌氬€搁崐鐑芥倿閿曞倹鍎戠憸鐗堝笒閸ㄥ倿鏌﹀Ο渚▓闁绘帞鏅幉鍛婃償閿濆洨鐒块梺鍦劋椤ㄥ棝寮插⿰鍫熺厽闁挎繂鎳愰悘杈ㄣ亜閺冣偓濞茬喎顫忛搹鍦煓閻犳亽鍔庨鎴︽⒑缁嬫鍎愰柛鏃€鐟╅悰顔界節閸パ呭€為梺瀹狀潐閸庤櫕绂嶆ィ鍐╁仭婵炲棗绻愰顏嗙磼閳ь剟宕橀鐣屽幈闂佸搫鍟犻崑鎾绘煟閻旀潙鍔﹂柟顕€绠栭幃鍧楊敍濡鐫忛梻浣告贡閸庛倗鎹㈤崘顔肩柧闁冲搫鎳忛埛鎴犵磽娴ｅ顏呮叏閿曞倹鐓曢柟鐐綑閸濈儤銇勯姀锛勬噰鐎殿喗鎸虫慨鈧柍閿亾闁归绮换娑㈠箻閺夋垹鍔伴梺绋款儐閹歌崵鎹㈠☉娆愬闁告劕寮堕崳铏光偓瑙勬礃閻擄繝寮诲☉銏╂晝闁绘ɑ褰冩慨搴ｇ磼閻愵剙鍔ょ紓宥咃躬瀵鍨鹃幇浣告倯闁硅偐琛ラ埀顒€纾鎰版⒒娴ｅ憡鍟為柤瑙勫劤閿曘垽鏌嗗鍛姦濡炪倖宸婚崑鎾绘煕濡崵鐭掔€规洘鍨块獮妯肩磼濡厧骞堥梻浣告惈濞层垽宕濈仦鐐珷濞寸厧鐡ㄩ悡鏇熸叏濡炶浜剧紓浣哄У閻楁洟锝炶箛鎾佹椽顢旈崟顐ょ崺濠电姷鏁告慨鎾疮椤愶附顥夌€广儱顦伴崑鈩冪節婵犲倸顏紒鑸电叀閺屾盯鎮㈤崨濠勭▏闂佷紮绲块崗妯绘叏閳ь剟鏌曢崼婵囧櫣缂佹劖绋掔换婵嬫偨闂堟刀銏ゆ煕閻曚礁浜滄い鏇秮瀹曠ǹ螖娴ｅ搫骞楅梻浣虹帛閺屻劑骞楀⿰鍫濈疇闁告劦鍠楅崐鍨叏濮楀棗澧俊鎻掝煼閺屽秶绱掑Ο璇茬濡ゆ浜欓崡鍐茬暦閻旂⒈鏁嗛柍褜鍓欓锝夊箰鎼达絿鐦堥梺姹囧灲濞佳冪摥闂備胶鎳撻崯璺ㄦ崲濮椻偓閵嗕線寮崼鐔蜂汗闂佹眹鍨婚弫鎼佹晬濠婂牊鐓涘璺猴功婢ф垿鏌涢弬璺ㄧ伇缂侇噮鍙冮獮鎺懳旀担鐟版畽闂備焦瀵х换鍌炈囨导瀛樺亗闁哄洨鍠撶弧鈧梻鍌氱墛缁嬫帡藟濠婂嫨浜滈煫鍥ㄦ尵閹界姷绱掔紒妯兼创鐎规洘顨婂畷妤呮偂鎼达綇绱﹀┑鐘愁問閸犳牠鏁冮敂鎯у灊妞ゆ牜鍋涚粻顖炴煕濞戞瑦缍戠€瑰憡绻傞埞鎴︽偐閸欏娅у銈冨灩閹虫ê顫忕紒妯诲缂佸瀵уВ鎰版⒑閹肩偛鐏柣鎿勭節楠炲啳顦规鐐疵悾鐑藉炊閵婏富鍟庨梻鍌欒兌閹虫捇顢氶銏犵；闁绘柨鎼慨顒勬煃瑜滈崜鐔奉潖濞差亜绠归柣鎰絻婵⊙囨⒑缁洖澧查拑閬嶆煕閻斿憡銇濇慨濠傤煼瀹曟帒鈻庨幋鐘靛床婵犵數鍋橀崠鐘诲幢濡ゅ﹥婢撻梻鍌氬€风粈渚€骞栭锕€绠犻煫鍥ㄦ礈閻瑩鏌熺€涙璐╂繛宸簻閸ㄥ倹銇勯弮鍥舵綈闁哄倵鍋撻梻鍌欒兌缁垶鏁冮埡鍛獥闁哄稁鍘介崐鍫曞级閸稑濡跨紒鐘冲劤椤法鎹勬笟顖氬壋濠电偞褰冮悺銊╁Φ閸曨垰顫呴柨娑樺閳峰矂鎮楃憴鍕闁绘牕銈搁妴浣肝旈崨顓犲姦濡炪倖甯掔€氼參宕曞Δ浣风箚闁靛牆鎳忛崳娲煟閹惧瓨绀嬮柡灞界Ч閸┾剝鎷呴崨濠冾啀闂備焦鍓氶崹鍗灻洪悢鐓庤摕鐎广儱顦伴悡銉╂倵閿濆簼绨藉ù鐘哄亹缁辨挻鎷呮禒瀣懙闂佸湱枪椤兘鐛箛娑樺窛閻庢稒锚濞堟劙姊洪弬銉︽珔闁哥噥鍋婇幃姗€鍩￠崘顏嗭紲濡炪倖妫侀崑鎰櫠閿曞倹鐓曢柍瑙勫劤娴滅偓淇婇悙顏勨偓鏍暜閹烘柡鍋撳鐓庡籍闁糕晜鐩獮瀣偐閻㈢绱查梺璇插嚱缂嶅棙绂嶅⿰鍫熷剭闁挎稑瀚ч崑鎾舵喆閸曨剙浠╅梺绋挎唉鐏忔瑩骞戦姀鐘斀閻庯綆浜為崝锕€顪冮妶鍡楃瑨閻庢凹鍙冮幃鐐烘嚃閳规儳浜炬鐐茬仢閸旀岸鏌熼搹顐㈠鐎规洘绻堥弫鍐焵椤掑嫧鈧棃宕橀鍢壯囨煕閳╁喚娈橀柣鐔稿姍濮婃椽鎮℃惔鈩冩瘣闂佺粯鐗滈崢褔鎮鹃悜绛嬫晬闁绘劘灏幗鏇㈡⒑闂堟侗妾у┑鈥虫喘瀹曘垽鎮介崨濞炬嫼闂佸憡绋戦敃銈夋倶閻斿吋鐓曞┑鐘插暟婢ч亶鏌嶈閸撴岸鎮㈤鍕闁跨噦鎷�
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
        //只好把全局变量当一个函数块看待了。
        Unit* unit = builder->getUnit();
        Function* func = new Function(unit, nullptr);
        BasicBlock* entry = func->getEntry();


        //std::cout << id->getSymPtr()->toStr() << std::endl;
        //std::cout << "fuck" << std::endl;
        Operand *addr;
        SymbolEntry *addr_se;
        addr_se = new IdentifierSymbolEntry(*se);
        addr_se->setType(new PointerType(se->getType()));
        addr = new Operand(addr_se);
        se->setAddr(addr);
        //initVal->genCode();

        new GlobalInstruction(new Operand(id->getSymPtr()), nullptr, se, entry);


        //Instruction* g;
        //g = new GlobalInstruction(new Operand(id->getSymPtr()), nullptr, se);
        //g->output();
     
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
        // 闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁诡垎鍐ｆ寖闂佺娅曢幑鍥灳閺冨牆绀冩い蹇庣娴滈箖鏌ㄥ┑鍡欏嚬缂併劌銈搁弻鐔兼儌閸濄儳袦闂佸搫鐭夌紞渚€銆佸鈧幃娆撳箹椤撶噥妫ч梻鍌欑窔濞佳兾涘▎鎴炴殰闁圭儤顨愮紞鏍ㄧ節闂堟侗鍎愰柡鍛叀閺屾稑鈽夐崡鐐差潻濡炪們鍎查懝楣冨煘閹寸偛绠犻梺绋匡攻椤ㄥ棝骞堥妸鈺傚€婚柦妯侯槺閿涙稑鈹戦悙鏉戠亶闁瑰磭鍋ゅ畷鍫曨敆娴ｉ晲缂撶紓鍌欑椤戝棛鈧瑳鍥ㄥ€垫い鎺戝閳锋垿鏌ｉ悢鍛婄凡闁抽攱姊荤槐鎺楊敋閸涱厾浠搁悗瑙勬礃閸ㄥ潡鐛崶顒佸亱闁割偁鍨归獮宥囩磽閸屾艾鈧兘鎮為敃鍌涙櫔缂傚倷鐒﹂妵鍡涘炊閵娧冨笚闁荤喐绮嶇划鎾崇暦濠婂牊鏅濋柛灞炬皑閻撴垿姊洪崨濠傚Е闁绘挸鐗嗛…鍥冀椤撶喓鍙勯棅顐㈡处閹歌崵鎷归敓鐘崇厽妞ゆ挾鍠撻幊鍥煙椤旇偐绉烘鐐扮窔楠炴帡骞嬪┑鎰偓閿嬩繆閻愵亜鈧牠寮婚妸銉冩椽顢橀悜鍡樼稁濠电偛妯婃禍婊冾啅濠靛棌鏀介柣妯哄级閸ｈ銇勯妷锔剧畵妞ゎ亜鍟存俊鍫曞幢濡ゅ啩娣俊鐐€х€靛矂宕归崼鏇炵畺濡わ絽鍟崐濠氭煠閹帒鍔滄繛鍛矒濮婃椽宕滈幓鎺嶇按闂佹悶鍔岄…宄邦嚕閸撲胶鐭欓幖瀛樻尰閺傗偓闂備胶绮崝鏍ь焽濞嗘挻鍊堕柨鏇炲€归悡鐔兼煟閺冣偓缁诲倸煤閵堝纾奸柕濠忚礋娴滄粓鐓崶銊﹀暗濠⒀勭洴閺屽秹鏌ㄧ€ｎ亞浼岄梺鍝勬湰閻╊垶鐛鈧鍫曞箣閻樼偣鍋℃繝鐢靛仜閻°劎鍒掑鍥ㄥ床闁告洦鍘介～鏇㈡煙閻戞ɑ鈷愰悗姘哺閺屻倗鍠婇崡鐐插О闂侀潧绻堥崐鏇犳兜閳ь剟姊绘笟鍥у缂佸鏁婚幃锟犲即閻旇櫣顔曢梺鐟邦嚟閸庢垿宕楅鍕厸闁逞屽墯缁绘繂顫濋鐘插箞婵犵數鍋涘Λ妤冩崲閹烘梻涓嶅┑鐘崇閻撴瑦銇勯弮鍌氬付婵℃彃顭烽弻宥囨嫚閸欏鏀紓浣哄У閻╊垶鐛▎鎾崇鐟滃繒澹曟繝姘拻闁稿本鐟чˇ锕傛煙鐠囇呯瘈闁诡喚鍏樻慨鈧柕鍫濇噽閿涙盯姊洪悷鏉库挃缂侇噮鍨堕崺娑㈠箳閹炽劌缍婇弫鎰板川椤斿吋娈橀梻浣筋嚃閸ㄤ即鎮ч幘璇茶摕闁挎繂顦介弫鍥煟閹存繃顥炲Δ鏃傜磽閸屾瑦绁板ù婊庡墴瀹曟垿鎮欓崫鍕唵闂佸憡绋掑娆戠矆閸儲鐓欑紓浣姑粭姘亜閹炬潙顕滃ǎ鍥э躬閹瑩顢旈崟銊ヤ壕闁哄稁鍋呴弳婊堟煙閻戞ɑ鐓涢柛瀣尭椤繈鎮℃惔鈽嗘骄缂傚倷绶￠崰姘卞垝椤栨壕鍋撻棃娑栧仮鐎殿喖鐖奸獮瀣偑閳ь剟宕崶鈺冪＝闁稿本鐟х拹浼存煕閻樺啿鍝烘鐐诧躬楠炴鈧稒蓱閻濈兘姊洪崨濠勬噧妞わ箒浜划缁樸偅閸愨晝鍘卞銈嗗姂閸婃洟寮悙顑句簻闊洦鎸炬晶鏇㈡煟閹捐揪鑰块柡宀€鍠愬蹇斻偅閸愨晩鈧秹姊虹粙娆惧剱闁告梹鐟╁濠氭偄闂堚晝鐭楁繛杈剧导鐠€锕€危椤曗偓濮婃椽宕崟顒€娈楁繛瀛樼矎濞夋盯锝炶箛鎾佹椽顢旈崟顓у晣闂備胶绮崝鏍亹閸愵噯缍栭柛銉ｅ妿缁犻箖鎮楀☉娆樼劷闁活厼锕弻锝夊冀瑜嬮崑銏ゆ煙椤旇宓嗘い銏″哺閸┾偓妞ゆ帒瀚畵渚€鏌涢妷顔煎缂佺嫏鍥ㄥ仯濞撴凹鍨抽崢娑㈡煕閻愬瓨灏︽慨濠勭帛閹峰懘鎸婃竟顓熸崌閺屻劌顫濋婊€绨婚梺鎸庢濡嫰鍩㈤弴鐕佹闁绘劖鎯屽▓婊呪偓娈垮枟閹歌櫕鎱ㄩ埀顒勬煃闁款垰浜鹃梺鍦焿濞咃絿妲愰幘璇茬＜婵炲棙鍨垫俊浠嬫煢閸愵喕鎲鹃柡灞界Ч閺屻劎鈧綆浜濈拠鐐烘⒑閸濆嫭婀扮紒瀣灱閻忔帡姊虹紒妯忣亜顕ｉ崼鏇為棷闁芥ê顦Σ鍫ユ煙閹咃紞閺嶏繝姊洪柅娑氣敀闁告柨鐭侀悘鎺楁⒑缁夊棗瀚峰▓鏃堟煙閻ｅ本鏆慨濠冩そ瀹曨偊濡烽妷鎰剁秮閺屾盯寮埀顒勬偡閳哄懎绠栭柨鐔哄閺佸啴鏌ㄩ弴妤€浜鹃梺缁樺姇閿曨亪寮婚弴鐔虹瘈闊洦鑹鹃幃鍛存⒑閸︻厽鐒挎い鏇嗗洠鈧棃宕橀鍢壯囨煕鐏炲墽鈯曢柣婵囨閹鈻撻崹顔界亪闂佺粯鐗曢崥瀣┍婵犲洦鍋ㄧ紒瀣硶椤︽澘顪冮妶鍡樼叆濠⒀傜矙瀵爼骞栨担鍏夋嫼闁荤喐鐟ョ€氼剛绮堥崘鈺冪濠㈣泛顑囧ú瀵告崉椤栫偞鐓冪憸婊堝礈閻斿娼栫紓浣股戞刊鎾煣韫囨洘鍤€缂佹绻濆铏规喆閸曨厽鎲欓悗瑙勬礈閺佹悂宕氶幒鎴旀瀻闁规儳纾娲⒑閹稿孩鈷掗柡鍜佸亰閹箖骞庨懞銉㈡嫽闂佺ǹ鏈悷褔宕濆澶嬪€电紒妤佺☉閹冲繐鐣烽弻銉︾厱閻忕偟铏庨崵褔鏌熼悜姗嗘當閸烆垶姊洪棃娑掑悍缂佽尙鍘ф晥婵°倕鎳忛埛鎴︽煕閿旇骞栭柛鏂款儑缁辨帞鎷犻幓鎺撴閻庢鍠楄ぐ鍐偑娴兼潙绀冮柣鎰靛墮閻︽粓姊绘笟鈧褔鎮ч崱妞㈡稑鈻庨幋婊呭墾濡炪倕绻愰悧濠囨偂閺囥垺鐓忓┑鐘茬箻濡绢噣鏌曢崶銊х疄闁哄瞼鍠栭、娆撳礂婢跺﹣澹曢梺鍝勫€堕崕鏌ワ綖瀹ュ鈷戦梻鍫熺〒缁犳岸鏌嶈閸撴岸寮告總绋跨；闁规崘顕ч柋鍥煛閸モ晛鏋旈柛妯圭矙濮婃椽鏌呭☉妯虹ギ闂佺ǹ顑呴敃顏勵嚕鐠哄ソ娲敂閸涱垰骞堥梻渚€娼ч悧鍡椢涘☉銏犵疇闁告劦鍠楅悡鍐偡濞嗗繐顏╅柣蹇撶摠閵囧嫰濮€閿涘嫭鍣伴梺璇″枓閸撴繈骞嗛弬璇剧喐娼弶娆炬闂傚倷鑳堕～瀣礋閸偆鏆ラ梻浣芥閸熶即宕伴弽顓炶摕闁挎繂顦猾宥夋煕鐏炵虎娈曢悗姘洴濮婄儤娼幍顕呮М闂佹寧娲︽禍顏勵嚕鐠囨祴妲堟俊顖炴敱閺傗偓闂備胶纭堕崜婵嬨€冭箛鏃傤浄闂侇剙绉甸埛鎴犵磼鐎ｎ偒鍎ラ柛搴㈠姍閺岀喖宕ㄦ繝鍐ㄢ偓鎰版煕閳哄啫浠辨鐐差儔閺佸啴鍩€椤掑嫮宓侀柕蹇ョ磿缁犻箖鏌涢埄鍏狀亝鎱ㄩ崼鐔翠簻闁哄啠鍋撴繛鑼枎椤繒绱掑Ο璇差€撻梺鍛婄☉閿曘劎娑甸埀顒勬⒒娴ｅ懙鍦姳濞差亜纾规繝闈涙閺嗭妇鎲搁悧鍫濈瑲闁稿瀚伴弻銈夋嚑椤掆偓閻ㄦ垹绱撳鍕槮闁伙綁鏀辩缓浠嬪川婵犲倷绨荤紓鍌欑椤戝懘鎮樺┑鍥ь嚤閹兼番鍨荤弧鈧悗鍏稿嵆閺€鍗烆熆濮椻偓閸┾偓妞ゆ帊绀佹慨宥団偓瑙勬礃閸ㄥ潡鐛Ο鑲╃闁绘鏁搁悾鐐繆閻愵亜鈧牠宕濊瀵板﹥銈ｉ崘銊у幈闂佸湱鍎ら幐椋庢崲閸℃稒鐓曟繛鍡楁禋濡茬ǹ鈹戦鑲┬ч柡灞稿墲閹峰懐鎲撮崟顐わ紦闂備浇妗ㄩ悞锕傚箲閸ヮ剙鏋侀柟鍓х帛閺呮悂鏌ㄩ悤鍌涘?闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁诡垎鍐ｆ寖闂佺娅曢幑鍥灳閺冨牆绀冩い蹇庣娴滈箖鏌ㄥ┑鍡欏嚬缂併劌銈搁弻鐔兼儌閸濄儳袦闂佸搫鐭夌紞渚€銆佸鈧幃娆撳箹椤撶噥妫ч梻鍌欑窔濞佳兾涘▎鎴炴殰闁圭儤顨愮紞鏍ㄧ節闂堟侗鍎愰柡鍛叀閺屾稑鈽夐崡鐐差潻濡炪們鍎查懝楣冨煘閹寸偛绠犻梺绋匡攻椤ㄥ棝骞堥妸鈺傚€婚柦妯侯槺閿涙稑鈹戦悙鏉戠亶闁瑰磭鍋ゅ畷鍫曨敆娴ｉ晲缂撶紓鍌欑椤戝棛鈧瑳鍥ㄥ€垫い鎺戝閸婂灚顨ラ悙鑼虎闁告梹纰嶉妵鍕晜鐠囪尙浠紓渚囧枛閻楀繘鍩€椤掑﹦绉甸柛瀣╃劍缁傚秴饪伴崼鐔哄幍闂佸憡绻傜€氼喛鍊撮梻浣告啞閺屻劎绮旈悽绋课﹂柛鏇ㄥ灠濡﹢鏌熺粙鍧楊€楅柡瀣Т閳规垿顢欑涵宄板缂備緡鍣崹鍫曠嵁韫囨稑宸濇い鏍ㄧ☉閺嬪倿姊洪崨濠冨闁稿鍊垮畷顐⑽旈崨顔规嫽婵炶揪绲介幊娆撳捶椤撴稑浜炬繛鎴炲笚濞呭懏銇勯鍕殻闁圭ǹ锕ュ鍕沪閻愵剦鍟庡┑鐘垫暩婵挳鏁冮妶澶婄柈闁绘ǹ顕ч崙鐘诲箹濞ｎ剙濡介柣鎾寸懇閹鈽夊▎妯煎姺缂備胶濮伴崕鍙夌┍婵犲浂鏁冮柨婵嗘川閻撳姊虹拠鈥虫灈闁搞垺鐓￠妶顏呭閺夋垵绐涘銈嗘寙閸曨剛妲橀梻鍌欑劍閻綊宕洪崟顖氬瀭闂侇剙绉查埀顒€鍟换婵嬪炊瑜忛悾娲⒑閻愯棄鍔滈柡瀣帶鍗遍柛顐ゅ枑閸欏繑鎱ㄥΔ鈧悧蹇涘礆閺夋５鐟扳堪閸涱厺澹曞銈冨妸閸庣敻骞冨▎鎾村殤妞ゆ垼鍎诲鎼佹⒒娴ｅ憡鍟炴慨濠傜秺閹虫繈骞戦幇顔荤胺闂傚倷鑳剁划顖涚仚濠电偛鎳忓ú鐔风暦閹版澘绠涢柣妤€鐗忛崢钘夆攽閻愭潙鐏ョ€规洦鍓欓埢宥咁吋閸ワ絽浜炬繛鍫濈仢閺嬫瑧绱掗鐣屾噰闁靛棔绀侀～婊堝焵椤掑嫬绠栭柕蹇嬪€曟导鐘绘煕閺囥劌浜為柣顐㈠濮婂宕掑顑藉亾妞嬪海鐭嗗〒姘ｅ亾妤犵偛顦甸弫鎾绘偐閼碱剦妲烽梻浣瑰濮婂寮查锔衡偓鍛村矗婢跺瞼鐦堟繝鐢靛Т閸婄粯鏅堕弴銏＄厱闁瑰濮存牎缂備胶绮惄顖炵嵁鐎ｎ亖鏋庨煫鍥ㄦ磻閻ヮ亪姊绘担铏瑰笡闁圭ǹ顭烽幆鍕敍閻愯尙鐣哄┑顔姐仜閸嬫捇鏌熼鐣屾噰妤犵偞鎹囬獮鎺楀幢濡櫣妲烘繝鐢靛Х閺佹悂宕戝☉姗嗗殨閻犺桨璀﹂悞鑺ョ箾閸℃ɑ灏柣鎾达耿閺岀喐娼忔ィ鍐╊€嶉梺鍝勬媼閸撶喖寮诲☉銏犲嵆闁靛ǹ鍎扮花璇参旈悩闈涗粶妞ゆ垵鎳橀垾鏃堝礃椤斿槈褔鏌涢幇鈺佸濞寸娀绠栧娲嚒閵堝懏姣愰梺鍝勬噽婵炩偓闁绘侗鍣ｅ畷姗€顢欓懖鈺佸Ф闂備礁鎲￠崝蹇涘疾濞戙垹绀夐柛娑卞弾濞撳鏌曢崼婵囶棡闁抽攱鍔欓弻娑㈠Ω閿斿墽鐣靛銈庡亝缁诲牓寮崘顔肩＜婵炴垶甯楃€氫粙姊绘担鍛靛綊寮甸鍕殞濡わ絽鍟悞鍨亜閹烘垵顏存俊顐ｅ灩缁辨帡顢欓懖鈺侇杸缂備焦顨堥崰鏍春閳ь剚銇勯幒鎴濐伀鐎规挷绀侀…鍧楁嚋闂堟稑顫嶉梺鍝勬噺閹倿寮婚敐鍛傜喖宕崟顒佺槪濠电偛顕刊顓㈠矗閸愵煈娼栨繛宸簼椤ュ牊绻涢幋锝夊摵鐎涙繈姊绘担鍛婃儓闁瑰啿绻掗崚鎺楀箻閹颁礁娈ㄩ梺鍦帛瀹稿寮ㄦ禒瀣€甸柨婵嗛娴滅偤鏌涘鍡曠凹缂佺粯绻堟慨鈧柨婵嗘閵嗘劕鈹戦埥鍡椾簼闁荤喆鍎甸崺銏狀吋婢跺﹤绐涘銈嗘寙閸曨収娲梻鍌欑閹诧紕绮欓幋锔藉仱闁靛ň鏅涢悡鏇㈡煙鏉堥箖妾柣鎾存礃缁绘盯骞嬮悜鍥у彆闂佽　鍋撻悹鍥ф▕閻斿棝鎮归崫鍕儓妞ゅ浚鍙冮弻锛勪沪閸撗勫垱婵犵鍓濋幃鍌涗繆閸洖宸濇い鎾跺О閸嬫牠姊虹拠鍙夊攭妞ゎ偄顦叅婵せ鍋撻柡浣稿暣婵偓闁靛牆鎳忓Σ顒勬⒑闁偛鑻晶顖炴煏閸パ冾伃妤犵偞甯￠獮瀣攽閹邦亞妫梻鍌欑劍婵炲﹪寮ㄩ柆宥呭瀭闁割煈鍣鏍р攽閻樺疇澹橀梺鍗炴喘閺岋綁寮埀顒勫磿婵傜ǹ瑙﹂柛灞惧焹閺€浠嬫煟閹邦厽缍戦柣蹇ョ畵閺岋綁顢樿閺嬫盯鏌嶇拋宕囩煓妞ゃ垺妫冨畷濂告偄閸濆嫬绠洪梻鍌欑窔閳ь剚绋戝畵鍡樼箾娴ｅ啿瀚▍鐘测攽閻樺磭顣查柣鎾存礋閺岋綁寮崼鐔告殸闂佸摜濮佃摫闁逛究鍔嶇换婵嬪磼閵堝洤鎮戦柣搴ゎ潐濞叉鏁幒妤嬬稏婵犻潧顑愰弫鍕煢濡警妲峰瑙勬礋濮婅櫣绱掑Ο铏逛紘婵犳鍠撻崐婵嗙暦閻㈢ǹ绀嬫い鏍ㄧ〒閸橀箖姊洪崨濠勭細闁稿孩鐓″鎶藉煛娴煎崬缍婇幃鈺冣偓鍦Т椤ユ繈姊虹€圭媭娼愰柛銊ョ埣閻涱喗绻濋崶銊у幈婵犵數濮撮崰姘枔閺囥垺鐓欏〒姘仢婵倿鏌熼鐐珪缂侇喗鐟ч幑鍕Ω瑜庡В鍥⒒閸屾瑨鍏岄弸顏堟煟椤掆偓閵堢ǹ鐣烽幋婵冩闁靛繒濮撮崵鎴炵箾閹炬潙鐒归柛瀣崌閺岋紕浠﹂崜褎鍒涘Δ鐘靛仜濞差厼螞閸愩劉妲堟繛鍡樺灣閹查箖姊婚崒娆戭槮闁圭⒈鍋婇獮濠呯疀閺囩偛鐏婇梺瑙勫礃椤曆囨嫅閻斿吋鐓ユ繝闈涙－濡插摜绱掗悩宕囨创婵﹤顭峰畷濂告偄鐞涒剝顫嶉梻浣瑰▕閺€杈╂暜閹烘鐓橀柟杈鹃檮閸婄兘鎮规ウ鎸庛€冩慨瑙勵殜濮婃椽宕妷銉愶綁鏌ｅΔ浣圭闁挎繄鍋犵粻娑㈠箻娴ｈ銇濇い銏℃瀹曠喖顢楅埀顒傜矈閼稿吀绻嗛柣鎰典簻閳ь剚鐗犻獮鎰偅閸愩劎锛涘┑鐐村灦閻熴倗鎹㈤崱妯肩鐎瑰壊鍠曠花濂告煃闁垮鐏撮柡宀€鍠栭幊鏍煛娴ｉ鎹曢梻渚€鈧偛鑻崝顖炴煕濞戝崬鐏熺紒妤€顦靛娲捶椤撶偘澹曢梺鍛婃尰瀹€绋跨暦閻㈢ǹ鍗抽柣鏃傜節缁ㄥ姊洪崫鍕妞ゃ劌鎳忕粋宥嗐偅閸愨晛鈧敻鎮峰▎蹇擃仾缂佲偓閸愵喗鍋ㄦい鏍ㄧ☉缁椻晠鏌ｈ箛鎾虫殻婵﹥妞介弻鍛存倷閼艰泛顏繝鈷€灞界仯缂佽鲸甯″畷鎺戭潩濮ｆ瑱缍侀弻鏇㈠醇椤掑倻袦閻庤娲橀敃銏ゅ春閿熺姴绀冮柕濞垮劜鏉堝牊绻濋悽闈浶ラ柡浣告啞缁绘盯鍩€椤掍胶绠惧ù锝呭暱閸氭ê鈽夊Ο閿嬫杸闁诲函绲介悘姘跺疾濞戞ǚ鏀介幒鎶藉磹閹版澘纾婚柟鐐暘娴滄粍銇勯幘顖楀亾閸愭彃啸婵＄偑鍊х粻鎾寸閸洖钃熸繛鎴烆焸閺冣偓閹峰懐鍖栭弴鐔轰簽闂佽瀛╅鏍窗閺嶎厸鈧箓鎮滈挊澶嬬€梺鐟板⒔缁垶宕戦幇鐗堢厵缂備焦锚缁椻晠鏌嶈閸撴瑩鎮樺┑瀣厴闁硅揪闄勯崐鐑芥煕濞嗗浚妯堟俊顐ゅ仧缁辨挻鎷呯粵瀣濠碘槅鍋呯换鍌烆敋閿濆绠瑰ù锝嚽圭粣娑橆渻閵堝棙灏扮紒瀣浮瀵剟鍩€椤掍椒绻嗛柣鎰典簻閳ь剚鐗曢～蹇旂節濮橆儵銉╂倵閿濆骸鈧姴鈽夊鍡樺兊濡炪倖鎸炬慨鐑筋敊閺囥垺鈷戦柛娑橈功閻﹪鏌涢悢绋款棆缂侇喗妫冮、娆戠驳鐎ｎ偒鍟嶉梻浣虹帛閸旀浜稿▎鎴犱笉濠电姵纰嶉悡娆愩亜閺冨浂娼愭繛鍛喘閺岀喐顦版惔鈾€鏋呴梺鐟扮－婵炩偓妞ゃ垺鐗滈幑鍕Ω瑜滃Σ鍫曟⒒閸屾艾鈧悂宕愰悜鑺ュ€块柨鏇炲€哥壕鍧楁煕閹邦垼姊块柣鐔煎亰濞尖晠鎮规ウ鎸庮仩闁挎稒鐩铏瑰寲閺囩偛鈷夊銈庡幖濞差參濡撮崨鏉戠煑濠㈣泛鐗呯花濠氭椤愩垺澶勯柟绋款煼钘熼柣妯兼暩濡垶鏌涘▎宥呭姢闁活厽鐟╅弻锛勪沪閸撗佲偓鎺懨归悪鍛暤鐎规洘绮忛ˇ鎶芥煕閿濆骸娅嶆慨濠冩そ瀹曘劍绻濋崘顏勫汲缂傚倷绀侀ˇ顖滅礊婵犲偆鍤曢悹鍥ㄧゴ濡插牓鏌曡箛鏇炐ラ柣顐㈠濮婃椽鏌呴悙鑼跺濠⒀勬尦閺岀喖顢欓妸銉﹀闁稿鍔楅埀顒傛嚀婢瑰﹪宕伴弽褉鏋旈柕濞炬櫆閳锋垿姊婚崼鐔惰吂婵炴垯鍨圭粈鍐┿亜椤撶喎鐏ユ繛鍫幘缁辨捇宕掑顑藉亾閻戣姤鍊块柨鏇炲€哥粈鍐ㄢ攽閻樺疇澹樼紒鐙€鍨堕弻銊╂偆閸屾稑顏�
    }
    // 闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁诡垎鍐ｆ寖闂佺娅曢幑鍥灳閺冨牆绀冩い蹇庣娴滈箖鏌ㄥ┑鍡欏嚬缂併劌銈搁弻鐔兼儌閸濄儳袦闂佸搫鐭夌紞渚€銆佸鈧幃娆撳箹椤撶噥妫ч梻鍌欑窔濞佳兾涘▎鎴炴殰闁圭儤顨愮紞鏍ㄧ節闂堟侗鍎愰柡鍛叀閺屾稑鈽夐崡鐐差潻濡炪們鍎查懝楣冨煘閹寸偛绠犻梺绋匡攻椤ㄥ棝骞堥妸鈺傚€婚柦妯侯槺閿涙稑鈹戦悙鏉戠亶闁瑰磭鍋ゅ畷鍫曨敆娴ｉ晲缂撶紓鍌欑椤戝棛鈧瑳鍥ㄥ€垫い鎺戝閳锋垿鏌ｉ悢鍛婄凡闁抽攱姊荤槐鎺楊敋閸涱厾浠搁悗瑙勬礃閸ㄥ潡鐛崶顒佸亱闁割偁鍨归獮宥囩磽閸屾艾鈧兘鎮為敃鍌涙櫔缂傚倷鐒﹂妵鍡涘炊閵娧冨笚闁荤喐绮嶇划鎾崇暦濠婂牊鏅濋柛灞炬皑閻撴垿姊洪崨濠傚Е闁绘挸鐗嗛…鍥冀椤撶喓鍙勯棅顐㈡处閹歌崵鎷归敓鐘崇厽妞ゆ挾鍠撻幊鍥煙椤旇偐绉烘鐐扮窔楠炴帡骞嬪┑鎰偓閿嬩繆閻愵亜鈧牠寮婚妸銉冩椽顢橀悜鍡樼稁濠电偛妯婃禍婊冾啅濠靛棌鏀介柣妯哄级閸ｈ銇勯妷锔剧畵妞ゎ亜鍟存俊鍫曞幢濡ゅ啩娣俊鐐€х€靛矂宕归崼鏇炵畺濡わ絽鍟崐濠氭煠閹帒鍔滄繛鍛矒濮婃椽宕滈幓鎺嶇按闂佹悶鍔岄…宄邦嚕閸撲胶鐭欓幖瀛樻尰閺傗偓闂備胶绮崝鏍ь焽濞嗘挻鍊堕柨鏇炲€归悡鐔兼煟閺冣偓缁诲倸煤閵堝纾奸柕濠忚礋娴滄粓鐓崶銊﹀暗濠⒀勭洴閺屽秹鏌ㄧ€ｎ亞浼岄梺鍝勬湰閻╊垶鐛鈧鍫曞箣閻樼偣鍋℃繝鐢靛仜閻°劎鍒掑鍥ㄥ床闁告洦鍘介～鏇㈡煙閻戞ɑ鈷愰悗姘哺閺屻倗鍠婇崡鐐插О闂侀潧绻堥崐鏇犳兜閳ь剟姊绘笟鍥у缂佸鏁婚幃锟犲即閻旇櫣顔曢梺鐟邦嚟閸庢垿宕楅鍕厸闁逞屽墯缁绘繂顫濋鐘插箞婵犵數鍋涘Λ妤冩崲閹烘梻涓嶅┑鐘崇閻撴瑦銇勯弮鍌氬付婵℃彃顭烽弻宥囨嫚閸欏鏀紓浣哄У閻╊垶鐛▎鎾崇鐟滃繒澹曟繝姘拻闁稿本鐟чˇ锕傛煙鐠囇呯瘈闁诡喚鍏樻慨鈧柕鍫濇噽閿涙盯姊洪悷鏉库挃缂侇噮鍨堕崺娑㈠箳閹炽劌缍婇弫鎰板川椤斿吋娈橀梻浣筋嚃閸ㄤ即鎮ч幘璇茶摕闁挎繂顦介弫鍥煟閹存繃顥炲Δ鏃傜磽閸屾瑦绁板ù婊庡墴瀹曟垿鎮欓崫鍕唵闂佸憡绋掑娆戠矆閸儲鐓欑紓浣姑粭姘亜閹炬潙顕滃ǎ鍥э躬閹瑩顢旈崟銊ヤ壕闁哄稁鍋呴弳婊堟煙閻戞ɑ鐓涢柛瀣尭椤繈鎮℃惔鈽嗘骄缂傚倷绶￠崰姘卞垝椤栨壕鍋撻棃娑栧仮鐎殿喖鐖奸獮瀣偑閳ь剟宕崶鈺冪＝闁稿本鐟х拹浼存煕閻樺啿鍝烘鐐诧躬楠炴鈧稒蓱閻濈兘姊洪崨濠勬噧妞わ箒浜划缁樸偅閸愨晝鍘卞銈嗗姂閸婃洟寮悙顑句簻闊洦鎸炬晶鏇㈡煟閹捐揪鑰块柡宀€鍠愬蹇斻偅閸愨晩鈧秹姊虹粙娆惧剱闁告梹鐟╁濠氭偄闂堚晝鐭楁繛杈剧导鐠€锕€危椤曗偓濮婃椽宕崟顒€娈楁繛瀛樼矎濞夋盯锝炶箛鎾佹椽顢旈崟顓у晣闂備胶绮崝鏍亹閸愵噯缍栭柛銉ｅ妿缁犻箖鎮楀☉娆樼劷闁活厼锕弻锝夊冀瑜嬮崑銏ゆ煙椤旇宓嗘い銏″哺閸┾偓妞ゆ帒瀚畵渚€鏌涢妷顔煎缂佺嫏鍥ㄥ仯濞撴凹鍨抽崢娑㈡煕閻愬瓨灏︽慨濠勭帛閹峰懘鎸婃竟顓熸崌閺屻劌顫濋婊€绨婚梺鎸庢濡嫰鍩㈤弴鐕佹闁绘劖鎯屽▓婊呪偓娈垮枟閹歌櫕鎱ㄩ埀顒勬煃闁款垰浜鹃梺鍦焿濞咃絿妲愰幘璇茬＜婵炲棙鍨垫俊浠嬫煢閸愵喕鎲鹃柡灞界Ч閺屻劎鈧綆浜濈拠鐐烘⒑閸濆嫭婀扮紒瀣灱閻忔帡姊虹紒妯忣亜顕ｉ崼鏇為棷闁芥ê顦Σ鍫ユ煙閹咃紞閺佸牓姊洪崫鍕効缂佽鲸娲樼粋鎺楁晜閻愵剙鐝伴梺鍦帛鐢帡锝炲鑸碘拻濞达綀娅ｇ敮娑㈡煙缁嬫鐓奸柟顔惧厴閸┾剝绗熼崶銊х▉婵犵數鍋涘Ο濠冪閸洖鏋侀柛銉墯閻撳繐鈹戦悙鎴濆暞閸ｇ儤绻涢崼鈶跺綊鍩為幋锔藉亹妞ゆ劧绲介蹇涙⒑閸涘﹣绶遍柛娆忛叄瀵娊顢橀姀鈾€鎷洪梻渚囧亝缁嬫捇鍩為幒妤佺厱闁哄啠鍋撻柛銊ョ仢椤曪綁顢曢敃鈧粻娑㈡煛婢跺孩纭舵い锔哄姂濮婃椽妫冨ù銈嗙洴瀹曟帒饪伴崪浣剐熸繝鐢靛Х閺佸憡鎱ㄩ悽鍓叉晩闁哄稁鍘肩粣妤呮煛瀹ュ骸骞楅柡鍛箞閺屾洘寰勯崼婵嗗Ф濠电偞鎹侀褔婀侀梺缁樏Ο濠囧磿韫囨洜纾奸柣姗€娼ч埢鍫ユ煛鐏炶濮傞柟顔哄灲瀹曨偊宕熼幋娆忕伈鐎规洖鐖奸獮姗€顢欑憴锝嗗闂備礁鎲＄粙鎴︽晝閿濆洦顐介梺顒€绉甸悡鏇㈡煟濡搫鏆遍柛婵囨そ閺屾盯鍩為幆褌澹曞┑锛勫亼閸婃牕顔忔繝姘；闁圭偓鍓氶悢鍡欐喐韫囨稑鐤炬い鎰ㄦ嚒閿濆绠涢柡澶庢硶椤旀帡鏌ｆ惔銏⑩姇闁挎碍銇勬惔銏″暗缂佽鲸鎸婚幏鍛村捶椤撴稒顫嶆俊鐐€栭崹鐢稿箠鎼淬劌鐓濋柟鐐た閺佸洭鏌曡箛濠冾€嗛柟鐤缁辨捇宕掑▎鎴濆闂佹寧娲嶉弲娑㈠煝閹捐鍗抽柕蹇ョ磿閸樼敻姊洪崨濠勨槈闁挎洏鍎靛畷鎰板箛椤撴粈绨婚梺闈涚箚閳ь剙鍟块幗鐢告⒑鏉炴壆鍔嶉柛鏃€鐟ラ悾鐑藉础閻愨晜鐎婚梺鐟邦嚟婵敻锝炵仦瑙ｆ斀闁绘ɑ鍓氶崯蹇涙煕閻樻剚娈滈柡浣稿暣閸╋繝宕ㄩ鐙呯吹闂備焦鐪归崹缁樼仚闂佺粯鎸婚惄顖炲蓟濞戞ǚ妲堥柛妤冨仦閻忔捇姊洪崨濞氭垿骞戦崶褜娼栨繛宸簻瀹告繂鈹戦悩鎻掝伀闁伙絿鏁婚弻锝嗘償閵忕姴姣堥梺鍛婃尵閸犳牕鐣峰ú顏勭劦妞ゆ帊闄嶆禍婊堟煙閸濆嫭顥滃ù婊堢畺濮婅櫣娑甸崪浣告疂缂備浇椴稿ú婊堝礆閹烘柡鍋撻敐搴濇喚闁告艾顑呴…璺ㄦ崉娓氼垰鍓伴梺閫炲苯澧伴柛蹇旓耿瀵鏁愭径濠勵吅闂佺粯鍔曢崯顖氱暆閹间胶宓侀煫鍥ㄧ⊕閹偤鏌涢敂璇插箻闁挎稒绻冪换娑欐綇閸撗冨煂婵犮垻鎳撻敃顏勭暦閺囥垹绠涢柣妤€鐗嗛埀顒傛暩缁辨帒螖娴ｈ　妲堥梺鍝ュУ閻╊垶骞婇幘璇查敜婵°倓鑳堕崢闈涒攽閻愯尙澧曢柣蹇旂箞瀵悂濡堕崶鈺傦紡闂佽鍨庢担闀愬垝闂備浇顕栭崰妤呫€冩繝鍥ф瀬闁归偊鍘介崕鐔兼煃閵夈劌鐨洪柣鈺侀叄濮婄粯鎷呯粵瀣缂備胶绮崝娆撳箖濞差亜惟闁宠桨绲奸敃鍌涚厱闁哄洢鍔岄悘鐘炽亜椤愩垺鍤囬柡灞界Ч閸┾剝鎷呴崨濠勪壕婵犵妲呴崑鍕疮閺夋埈娼栭柣鎴炆戞慨婊堟煙濞堝灝鏋涘┑鈥茬矙濮婃椽宕烽鐔锋畬濠电偛妯婇崢濂割敋閿濆鏁冮柨鏇楀亾缂佺姾宕电槐鎾存媴閼测剝鍨甸埢宥夊閵堝棌鎷洪柣鐘充航閸斿苯鈻嶉幇鐗堢厵闁告垯鍊栫€氾拷?闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁诡垎鍐ｆ寖闂佺娅曢幑鍥灳閺冨牆绀冩い蹇庣娴滈箖鏌ㄥ┑鍡欏嚬缂併劌銈搁弻鐔兼儌閸濄儳袦闂佸搫鐭夌紞渚€銆佸鈧幃娆撳箹椤撶噥妫ч梻鍌欑窔濞佳兾涘▎鎴炴殰闁圭儤顨愮紞鏍ㄧ節闂堟侗鍎愰柡鍛叀閺屾稑鈽夐崡鐐差潻濡炪們鍎查懝楣冨煘閹寸偛绠犻梺绋匡攻椤ㄥ棝骞堥妸鈺傚€婚柦妯侯槺閿涙稑鈹戦悙鏉戠亶闁瑰磭鍋ゅ畷鍫曨敆娴ｉ晲缂撶紓鍌欑椤戝棛鈧瑳鍏撅綁顢楅崟顑芥嫼闂佸憡绋戦敃銉﹀緞閸曨垱鐓曢柕濠忕畱椤ュ绱掗鑲╁ⅵ鐎规洘锕㈤崺鐐村緞閸濄儳娉块梻鍌氣看閸嬪嫬煤閿曞倸绠伴柟闂寸濮规煡鏌ｉ弬鎸庢喐缂佺姵鍎抽…璺ㄦ崉鐞涒€虫倣闂傚⿴鍓﹂崜姘跺箞閵婏妇绡€闁告洦鍘肩粭锟犳⒑閻熸澘妲婚柟铏姉閸掓帡鎮界喊妯轰壕闁挎繂绨肩花鍏笺亜閺傚灝顏紒缁樼箓閳绘捇宕归鐣屼憾闂備焦瀵уú锕傚磻閸℃瑥鍨濆┑鐘宠壘缁犺崵绱撴担鑲℃垵鈻嶅⿰鍫熲拺缂備焦锚婵牊绻涢崗鑲╂噮婵炲棎鍨介幃娆徝圭€ｎ偅鏉搁梻浣虹帛閸旀牕顭囧▎鎴犵當婵﹩鍘虹换鍡涙煟閹邦厼顥嬮柣顓熺懄椤ㄣ儵鎮欓幓鎺撴婵犳鍠掗崑鎾绘⒑閹稿海鈽夐悗姘緲椤﹪顢欓悾宀€鐦堥梺闈涢獜缁插墽娑垫ィ鍐╃厾婵炶尪顕ч悘锟犳煙娓氬灝濡奸悡銈嗐亜韫囨挻鍣介柛妯圭矙濮婅櫣绱掑Ο鑽ゅ弳闂佺濮ょ划鎾诲箚瀹€鈧埀顒婄秵閸嬪倻鎹㈤崱妯镐簻闁规壋鏅涢悘鈺呮煛閸℃鎳勯柕鍥у瀵挳顢旈崱娅烘粌顪冮妶鍐ㄧ仾婵炶尙鍠栧顐﹀礃椤旇偐鍔﹀銈嗗笒閸婂鎯岄崱娑欑厱闁硅埇鍔嶉弶鍝劽瑰⿰鍕煉闁哄矉绻濆畷姗€濡搁妷銏犱壕闁汇垻枪缁狀垶鏌嶉埡浣告殶缂佺娀绠栭弻锝夊箻閸愭彃姣愰梻濠庡墻閸撴岸骞堥妸锔剧瘈闁告劏鏂傛禒銏犫攽閳藉棗浜滈柛鐕佸亰閸┿儲寰勬繝搴㈠兊濡炪倖甯掗ˇ鏉啃掗崶顒佲拻闁稿本鐟ㄩ崗宀勬煙閾忣偅宕岀€规洦鍨跺畷褰掝敊濮樻剚娼愭い锔惧閹棃鍨惧畷鍥﹀枈闂傚倷鐒︾€笛呮崲閸岀偛绠犲璺好￠敐澶婇唶闁靛濡囬崢顏堟椤愩垺鍌ㄩ柛搴＄－婢规洟宕稿Δ浣哄幈濠碘槅鍨板﹢閬嶆儗濞嗘挻鐓涚€光偓閳ь剟宕伴弽褏鏆︽俊銈呮噺閸ゅ啴鏌嶉崫鍕偓褰掝敊閸愵喗鈷掑ù锝堫潐閸嬬娀鏌涢弬璺ㄧ劯闁炽儻绠撻幃婊堟寠婢跺袣濠电姷鏁告慨鏉懨洪妶澶婄闁规儼濮ら悡蹇涚叓閸ャ儱鍔ょ痪鎯ф健閺屾稑鈻庤箛鏃戞闂佸疇顫夐崹鍧椼€佸▎鎴炲枂闁圭儤鍨圭粔铏光偓瑙勬礃濡炶棄顕ｆ禒瀣垫晣闁绘棁娓圭花鐢告⒒娴ｅ憡鎯堟繛灞傚灲瀹曠懓煤椤忓懎浜楀┑鐐叉閹稿鎮￠悢鍏肩厵濞寸厧鐡ㄥ☉褍顭胯閸ㄥ磭妲愰幘鎰佸悑闁糕剝顨堟导宀勬⒑閸濆嫯瀚扮紒澶婂濡叉劙骞掗幊宕囧枛閹筹繝濡舵惔鈶垦囨⒒閸屾瑧顦﹂柟纰卞亝瀵板嫰宕堕鈧悞鍨亜閹哄棗浜剧紒鍓ц檸閸欏啫顕ｉ幎钘夊耿婵炴垶鐟ラ埀顒傛暬閺屻劌鈹戦崱娑扁偓妤€顭胯閸楁娊骞忛幋锔界劶鐎广儱妫岄幏缁樼箾鏉堝墽鎮奸柣鈩冩瀹曠敻宕橀鐣屽幈闂侀潧枪閸庨亶鎳滆ぐ鎺撴嚉闁绘劗鍎ら悡鏇㈡煙娴煎瓨娑ч柡瀣閺屽秷顧侀柛蹇旂〒缁牊绗熼埀顒勫春閵夛箑绶炲┑鐐灮閸犲酣鎮鹃敓鐘茬妞ゆ梻鈷堝浠嬫⒒閸屾艾鈧绮堟笟鈧獮澶愭晬閸曨厾鐓撻梺纭呮彧缁犳垹绮婚鐐寸厵閻庣數枪鍟哥紓浣插亾濠电姴浼呰ぐ鎺撴櫜闁告侗鍙庡Λ灞解攽閻愭彃鎮戦柣妤冨█瀵槒顦剁紒鐘崇洴楠炴澹曠€ｎ剦鏀ㄩ梻鍌欑劍濡炲潡宕㈡禒瀣仭闁挎梻鍋撳畷鍙夌箾閹存瑥鐏╂鐐灪娣囧﹪顢涘顓熷創婵犳鍠撻崹钘夘潖濞差亜宸濆┑鐘插暙閻噣姊洪崨濠庢當闁哥喎娼￠敐鐐剁疀濞戞瑦鍎柣鐔哥懃鐎氼剟宕㈣ぐ鎺撯拺闁告稑锕ｇ欢閬嶆煕閻樺厖浜㈤柕鍥ㄥ姌椤﹁鎱ㄦ繝鍐┿仢鐎规洏鍔嶇换婵嬪礃閿濆棗顏搁梻鍌欐祰瀹曠敻宕崸妤€鐤鹃柣妯肩帛閸嬫ɑ銇勯弴妤€浜鹃悗瑙勬礃閿曘垽銆佸▎鎾村癄濠㈣泛顦卞Λ顖氣攽閿涘嫬浜奸柛濠冪墵閹囶敇閻樿尙绛忔俊銈忕到閸燁偊鎮″┑瀣€堕柣鎰絻閳锋梻绱掗埦鈧崑鎾寸節閻㈤潧浠滈柣妤€妫濋幃妯衡攽鐎ｎ亜鍤戝┑鐐村灟閸ㄦ椽鎮￠妷鈺傜厸闁搞儲婀圭花缁樸亜閳哄﹤澧撮柡灞剧洴楠炴帡骞橀搹顐ョ檨婵＄偑鍊戦崹娲儎椤栫偛绠栨繛鍡樻尰閸ゆ垶銇勯幒鎴Ц闁轰焦鎮傚濠氬磼濞嗘埈妲梺纭咁嚋缁辨洜鍒掑▎鎾崇闁规惌鍘鹃崝宄扳攽閻愬弶顥滄繛瀛樺哺瀹曨垶宕ㄩ鍓х槇闂傚倸鐗婄粙鎺椝夐幋锔界厸闁逞屽墯缁傛帞鈧綆鍋嗛崢钘夆攽閳藉棗鐏ユい鏇嗗懎绶ゅ┑鐘崇閻撳啰鎲稿⿰鍫濈婵炴垯鍨圭粻鐘绘煙閹规劦鍤欓柣鎾寸箞閺岀喖鎮欓鈧晶顖涚箾缁楀搫濮傞柡灞界Х椤т線鏌涢幘瀵告噰闁挎繄鍋犵粻娑㈠即閻樼绱叉繝纰樻閸ㄧ敻顢氳椤㈡捇骞樼紒妯锋嫼闂佽鍨庨崨顖ｅ敽闂備胶顢婂▍鏇㈠箰閸撗冨灊妞ゆ挶鍨归悘鎶芥煛閸屾ê鍔滈柡鍌楀亾闂傚倷绀侀悿鍥涢崟顐嬫稑螖娴ｄ警娲稿┑鐘诧工閹虫劗澹曢懖鈺冪＝濞达絽鍘滃Λ銊︺亜韫囨挾澧涢柛瀣ф櫇閳ь剛鎳撶€氼厽绔熺€ｎ喖閱囨い蹇撶墛閻撴洟鏌熼柇锕€鏋涘ù婊堢畺閺屾稑顓兼径瀣敪濡炪値浜滈崯瀛樹繆閸洖骞㈡俊顖滃劋濞堫偄鈹戦悩鎰佸晱闁革綆鍣ｅ畷锝夊礃椤垵娈ㄩ梺闈浥堥弲婊堝箚閻愭番浜滈柟鎵虫櫅閳ь剚顨婇幆渚€宕奸妷锔规嫼濠殿喚鎳撳ú銈夋倶閸欏绠惧ù锝呭暱閸熶即骞楅妷銉㈡斀闁绘ê鐏氶弳鈺呮煕鐎ｎ偆娲撮柟顖氭处鐎靛ジ寮堕幋鐙呯串婵犵數濞€濞佳囶敄閸涱垳涓嶉柡宥冨妿缁犻箖鏌涢埄鍏狀亝鎱ㄦ径宀€纾奸柍閿亾闁稿鎹囧缁樻媴閸涘﹤鏆堥梺鍛婃灝閸涱垳锛濆┑顔角圭划鐐叏閸愭祴鏀介柣妯虹－椤ｆ煡鏌嶉柨瀣伌闁哄本鐩弫鍌滄嫚閹绘帞顔愰梻浣告啞閺屻劑顢栨径鎰摕婵炴垯鍨归悡娑樏归敐鍛儓妞わ絾妞介弻娑樜旈崟顏勬儓闂佺懓寮堕幃鍌炲箖瑜斿畷鐓庘攽閸垺鍤冮梻鍌欒兌缁垶骞愭繝姘剮妞ゆ牜鍋涢拑鐔哥箾閹寸們姘ｉ崼銉︾厱婵°倕鍟禒褍霉閻橆偅娅嗙紒缁樼〒閳ь剛鏁告灙妞ゅ孩鎸搁埞鎴﹀灳閾忣偄绐涘┑鈥冲级閸旀瑩鐛幒妤€绫嶉柛灞剧玻缁卞弶绻濈喊妯活潑闁搞劋鍗冲畷銉р偓锝庡枛缁€鍡樼箾閹寸儐鐒搁柡鍐ㄧ墛閸嬫劙姊婚崼鐔衡棩婵炲矈浜娲箰鎼淬垻锛橀梺鎼炲妼缂嶅﹪銆佸鑸垫櫜濠㈣泛锕崬鍫曟⒑閸濆嫭宸濋柛瀣枛椤㈡ê煤椤忓應鎷虹紓鍌欑劍閵嗙偤骞嬮敐鍐ф睏闂佸憡鍔﹂崢鎼佸触閸岀偞鈷掗柛灞捐壘閳ь剚鎮傚畷鎰板箹娴ｅ摜锛欓梺缁樺灱婵倝宕甸崟顖涚厾闁告縿鍎查弳鈺伱归悩宕囶暡濞ｅ洤锕幃娆擃敂閸曘劌浜鹃柡宥庡幖閸ㄥ倿姊婚崼鐔峰幏婵炴垶菤閺€浠嬫倵閿濆骸澧紒渚婄畵濮婇缚銇愰幒鎴滃枈闂佸憡锚缂嶅﹤鐣烽幋锕€宸濋悗娑欘焽閸橀亶姊虹憴鍕棎闁哄懏绋掓穱濠囧锤濡や胶鍘遍梺鍝勫暞閹搁箖鎮炬潏銊ｄ簻妞ゅ繐瀚弳锝呪攽閳ュ磭鍩ｇ€规洖宕灃闁告劦浜濋崳顖炴⒒閸屾瑧顦﹂柟鑺ョ矋閹便劑鎮介崨濠傛疄婵°倧绲介崯鐘诲焵椤掑﹦鐣电€规洖鐖奸、妤呭焵椤掑倻涓嶆繛鎴炵懀娴滄粓鏌熼幑鎰【閸熺ǹ顪冮妶鍡樷拹婵炶尙鍠庨～蹇撁洪鍕祶濡炪倖鎸炬慨瀵哥箔閿熺姵鐓熼柕蹇婃櫅閻忥綁鏌涚€ｎ剙鏋旈柛鎺撳笚缁绘繂顫濋鐐搭吋闂備線娼ч悧鍡涘磹閸涘﹦顩插Δ锝呭暞閸嬧剝绻涢崱妤冪妞ゅ浚浜炵槐鎺楀焵椤掑嫬绀冮柍鍝勫暟椤旀洟姊洪懖鈹炬嫛闁告挻鐟╁鍛婃償閵婏附鍤夐梺缁樺姉閸庛倝鎮￠弴銏＄厵闁逛絻娅曞▍鍛攽閿熺姵鏁卞ǎ鍥э躬楠炴捇骞掗弬娆炬澑闂備線鈧稓鈹掗柛鏂跨焸閿濈偛饪伴崼婵嗚€垮┑掳鍊愰崑鎾剁磼閻樺弶鎯堟い顏勫暣婵″爼宕卞Ο閿嬪婵犵數鍋犵亸娆撳窗鎼搭煉缍栭煫鍥ㄦ礈绾惧吋淇婇婵愬殭妞ゅ孩鎹囧娲箚瑜忕粻鎶芥煙閾忣偅灏电紒顕呭弮閺佹捇鎮╅弬銉﹀闂備線娼荤€靛矂宕㈤幇顖樹汗闁圭儤鍨归鎴濃攽閻樿宸ラ柣妤€锕畷闈涱吋婢跺鎷哄銈嗗坊閸嬫挾绱掓径瀣唉闁哄苯顑夊畷鍫曨敆娴ｅ搫骞愰梻浣规偠閸庮噣寮插☉娆戭浄鐟滄棃寮诲☉妯锋斀闁搞儴鍩栭悾椋庣磽娴ｈ櫣甯涚紒瀣崌閸┾偓妞ゆ帒鍊归弳鈺呭几椤忓棌鍋撳☉娆戠畵妞ゎ亜鍟存俊鍫曞幢濡ゅ啩娣繝纰樻閸嬪懐鎹㈤崼銉у祦闁告劦鍠栭悘鎶芥煛閸屾ê鍔滈柣搴☆煼濮婅櫣鎲撮崟顐㈠Б濡炪倖娲﹂崣鍐ㄧ暦閵忋倖顥堟繛鎴ｉ哺鐎靛矂姊洪棃娑氬婵☆偅绋掗弲鍫曨敂閸涱剛绠氬銈嗗姧缂嶅棗鈻撳⿰鍛亾鐟欏嫭绀冩繝銏★耿閿濈偛鈹戦崼鐔风／闂佺粯鍨舵灙缂佽翰鍨藉缁樼瑹閳ь剙顭囪閹广垽宕奸妷銉э紮闂佸搫绋侀崢濂告偂閺囥垺鐓欓悗鐢登瑰暩缂佺偓宕樺Λ鍕箒闂佹寧绻傞悧鍡樼濡ゅ懏鐓欓悗鐢登瑰皬闂佺粯甯掗敃銈夋箒闂佺粯鎸稿ù鐑藉箺閻樼粯鐓熼柟鎯х摠缁€瀣煛鐏炵偓绀冪紒缁樼洴瀹曞綊顢欓悡骞垵鈹戦悙宸殶濠殿喕鍗冲畷鐟懊洪鍕庯箓鏌涢弴銊ョ伇闁轰礁鍟撮弻鏇＄疀鐎ｎ亞浼勫銈嗘煥閿曪妇妲愰幘瀛樺濞寸姴顑呴幗鐢告煟閵忊晛鐏￠悽顖滃枛閹﹢骞掑Δ浣哄幗闂佺粯锚瀵爼骞栭幇顓濈箚妞ゆ牗鐔煬顒勬煕閳规儳浜炬俊鐐€栫敮濠囨倿閿曗偓閻ｉ浠﹂悙顒€寮挎繝鐢靛Т閸燁垶濡靛┑鍫氬亾鐟欏嫭绀冩い銊ワ工椤繘鎳￠妶鍜佹闁诲函缍嗘禍鐐哄礉閻㈠憡鈷掗柛灞剧懆閸忓矂寮搁鍫熷€垫繛鎴炲笚濞呭棛绱掔紒妯尖姇缂佺粯绻堝畷妯款槾缂佺姵宀搁弻锝嗘償閵忊懇濮囧銈庡幖濞层劌顕ｈ閸┾偓妞ゆ帒瀚埛鎴︽煙閼测晛浠滈柛鏃€锕㈤弻娑㈠棘濞嗙偓鈻堝Δ鐘靛仜缁绘劗鍙呭銈呯箰閹冲骞忔繝姘拺缂佸瀵у﹢鐗堟叏濡ǹ濡介柡鍛埣楠炲洭鎮ч崼銏犲箰闂備礁鎲℃笟妤呭窗濮樿京涓嶆俊顖濆亹绾惧吋銇勯弮鍥撴い銉ョ墢閳ь剝顫夊ú妯煎垝瀹ュ绠柛娑樼摠閹偤鏌ｈ閹芥粍鎯旀繝鍥ㄢ拻闁稿本鐟ㄩ崗宀€绱掗鍛仭闁诲繑鐟ラ埞鎴︽倷閸欏娅ゅ┑鐐插级閻楃娀骞冮幆褏鏆嬮柟娈垮枟濞堟洟姊洪崨濠冨闁告挻鐩畷銏ゆ寠婢跺棙鏂€闂佺粯鍔栧娆撴倶閵壯€鍋撶憴鍕闁告挾鍠庨锝夊箹娴ｈ娅嗛梺鍛婃寙閸滃啰搴婇梻鍌欒兌缁垶鏁嬬紒鍓ц檸閸欏啴宕洪妷锕€绶為柟閭﹀幘閸樻捇姊洪懞銉冾亪藝閽樺）锝夊箹娴ｅ湱鍘撻柣鐘叉处閻擄繝宕ｉ崟顒夋闁绘劕寮堕崰妯汇亜閵忊垙褰捤囬柆宥嗙厵閻庣數枪閸樺瓨鎱ㄦ繝鍌ょ吋鐎规洘甯掗～婵嬵敄閽樺澹曢悗鐟板閸ｇ銇愰幒鎴犲€為悷婊冾樀瀵悂寮介鐔哄幐闂佹悶鍎崕閬嶆倶閳哄懏鈷掗柛灞诲€曢悘锕傛煛鐏炶濮傜€殿喗鎸抽幃娆徝圭€ｎ亙澹曢梺鍛婄缚閸庤櫕绋夊鍡愪簻闁哄稁鍋勬禒锕傛煟閹惧崬鍔﹂柡宀嬬秮瀵挳鎮欏ù瀣壕闁革富鍘搁崑鎾愁潩閻愵剙顏�?
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
       isreturn=true;//闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ゆ繝鈧柆宥呯劦妞ゆ帒鍊归崵鈧柣搴㈠嚬閸欏啫鐣峰畷鍥ь棜閻庯絻鍔嬪Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曟劙鎮欓悜妯轰画濠电姴锕ら崯鎵不閼姐倐鍋撳▓鍨灍濠电偛锕顐﹀礃椤旇偐锛滃┑鐐村灦閼归箖鐛崼鐔剁箚闁绘劦浜滈埀顑惧€濆畷銏＄鐎ｎ亜鐎梺鍓茬厛閸嬪棝銆呴崣澶岀瘈闂傚牊渚楅崕鎰版煟閹惧瓨绀冪紒缁樼洴瀹曞崬螖閸愵亶鍞虹紓鍌欒兌婵挳鈥﹂悜钘夎摕闁挎稑瀚▽顏嗙磼鐎ｎ亞浠㈤柍宄邦樀閹宕归锝囧嚒闁诲孩鍑归崳锝夊春閳ь剚銇勯幒鎴姛缂佸娼ч湁婵犲﹤瀚粻鐐淬亜閵忥紕鎳囩€规洏鍔戦、妯衡槈濞嗘劖婢戦梻鍌欒兌缁垶宕濋弴鐑嗗殨闁割偅娲栭悡婵嬫煛閸モ晙绱抽柣鐔稿閸亪鏌涢弴銊ュ箻闁绘挴鍋撳┑鐘垫暩閸嬬偠銇愰崘顔藉€舵繝闈涱儏閻撴﹢鏌熸潏楣冩闁稿鍔栭妵鍕籍閸パ傛睏婵炲鍘уú銈夊煘閹达附鍊风€瑰壊鍠楁晥闂備胶鍎甸弲鈺呭垂閸洖违濞撴埃鍋撶€殿喕绮欓、姗€鎮欓懠顒傚春濠碉紕鍋戦崐鏍ь潖婵犳碍鎯為幖绮瑰煑瑜版帒绀嬫い鏍ㄧ▓閹风粯绻涢幘纾嬪婵炲眰鍊栭弲鍫曨敊閸撗咃紲婵犮垼娉涢張顒勫汲椤掑嫭鐓曢柍鍝勫€诲ú瀛橆殽閻愬樊鍎旈柡浣稿€婚幉鎾晲閸℃浼滈梻鍌氬€风粈浣圭珶婵犲洤纾婚柛鈩冪☉缁愭鏌￠崶銉ョ仼缁炬儳娼￠弻鐔兼焽閿曗偓閺嬬喖鏌涢悩鍐插闁哄矉绲借灃闁逞屽墴閹囧幢濞嗘垹鐣堕梺鍦劋濮婅螞椤栫偞鐓熼柟浼存涧婢ь喗銇勯弴鐔虹煂缂佽鲸甯炵槐鎺懳熼崗鐓庡灡闁诲氦顫夊ú鏍х暦椤掑啰浜介梺鑽ゅТ濞诧箓鎮￠敓鐘冲€垫繛宸簼閳锋帒霉閿濆懏鍟為柟顖氱墦閺岀喖宕橀懠顒傤啈闂侀潧娲﹂崝娆忕暦濮椻偓椤㈡瑩宕楁径濠佸闂佹寧姊婚崑鎾崇暦婢舵劖鐓忓┑鐘茬箳閻ｈ京鐥紒銏犵仸婵﹨娅ｇ槐鎺懳熺喊杈ㄦ婵犵數鍋涢崥瀣偡閿曞倸绠為柕濞垮劗濡插牊鎱ㄥΔ鈧Λ娆戔偓闈涚焸濮婃椽妫冨☉姘暫濠碘槅鍋呴悷褎绂嶉幖浣瑰亱闁割偅绮庣粻姘舵⒑缂佹ê濮岄柛鈺傜墵钘熷璺侯儍娴滄粍銇勯幇鈺佺伄缂佺姳鍗抽幃锟犲Χ閸℃劒绨婚棅顐㈡处閹告悂顢旈锝冧簻闁哄倹瀵ч崰姗€鏌″畝鈧崰鏍х暦濡ゅ懏鍤冮柍鍝勫€归鍐⒒娴ｈ櫣銆婇柡鍛〒閳ь剚鍑归崜鐔兼晲閻愬墎鐤€婵炴垶锚閻庮厼顪冮妶鍡橆梿妞ゎ偄顦靛顒勫焵椤掑嫭鈷掑ù锝呮憸閺嬪啯淇婇銏狀仼閾荤偞淇婇妶鍕厡妞も晛寮剁换婵囩節閸屾粌顤€闂佺ǹ锕ら悘姘跺箞閵娿儺娼ㄩ柛鈩冾殔缁犺櫣绱撴担钘夌厫闁绘搫绻濆濠氭晲婢跺娅滈梺鍛婁緱閸樻垝绨哄┑掳鍊栭〃蹇旂濞嗘挸纭€闁告劘灏欓弳锕傛煟閹惧磭宀搁柡鈧禒瀣厱妞ゆ劦鍋勬禍婊堟煛娴ｅ摜鍩ｆ慨濠傤煼瀹曟帒鈻庨幋顓熜滈梻浣告啞閼归箖顢栭崱娆戠焿鐎广儱妫涢々鐑芥倵閿濆簼绨介柨娑氬枎椤啴濡堕崱妯硷紩闂佺ǹ顑嗛幐濠氥€冮妷鈺傚€烽柤纰卞墰椤旀帡姊洪崷顓炲付缂傚秴锕ら悾鐑芥倻缁涘鏅ｉ梺缁樼懃閹虫劙姊婚鐐粹拻闁稿本鐟ㄩ崗宀勫几椤忓牊鐓涢柛顐亜婢ф挳鏌熼鐐効妞わ箑婀遍埀顒冾潐濞叉鎹㈤崒鐑嗘晣闁稿繒鍘х欢鐐烘倵閿濆簼绨界憸鏉挎嚇濮婂宕掑▎鎴ｇ獥闂佸憡鎸婚悷褏鍒掗弮鍫熷仺闁告稑锕ゆ禍妤呮⒑缂佹ɑ鐓ラ柛姘儔閹€斥枎閹惧鍘靛銈嗙墪濡宕幎钘夋槬闁绘劗鍎ら埛鎴︽煕濞戞﹫鏀绘い銉︾墱缁辨帡鎮╅搹顐㈢３閻庢鍠栭…鐑藉垂閹呮殾闁搞儯鍔嶉悾鐑芥⒒娓氣偓閳ь剛鍋涢懟顖涙櫠鐎电硶鍋撶憴鍕；闁告濞婇悰顔嘉熼崗鐓庣彴闂佽偐鈷堥崜锕傚船閻㈠憡鈷掑ù锝堟鐢盯鎯囨径鎰仯闁逞屽墴閺佸啴鍩€椤掆偓瀹撳嫰姊洪崨濠冨瘷闁告侗鍨界槐鏌ユ⒒娴ｈ櫣甯涢柛鏃撶畵瀹曟粌鈻庨幋鐘电劶闂佸憡鍔忛弲婵堢不閹岀唵閻犺桨璀﹂崕鎰磼閳ь剟宕奸悢鍓佺畾闂佸憡鐟ラˇ顖涙叏瀹ュ棭娈介柣鎰綑缁楁帡鎽堕弽褉鏀介柣妯诲絻閺嗙偞绻涢崨顔藉暗缂佽鲸鎸婚幏鍛村传閸曨亜顥氬┑鐘愁問閸ㄥ灚鎱ㄩ妶鍥╃焿鐎广儱顦介弫鍡涙煕閺囥劌浜悮婵嬫⒑鐠囨彃鍤辩紓宥呮閸┾偓妞ゆ帒顦獮妤佷繆椤愩垹鏆ｆ鐐插暣閸╋繝宕ㄩ鍛棃婵犵數鍋為崹鍫曘€冮崱娑樼闁告劦鍠楅埛鎴炴叏閻熺増鎼愰柣鎺撴そ閺屾盯濡搁妸銈呮儓闂佽桨绶￠崳锝夊箖閵忋倕绀傞柣鎾崇岸閸嬫捇宕奸弴鐔哄幈闂侀€涘嵆濞佳囧几閻斿吋鐓冮梺鍨儏閻忓瓨鎱ㄦ繝鍐┿仢妞ゃ垺娲熸俊鍫曞川椤掑倹宕熼梻鍌欑閸氬顪冮崸妤€鍨傞梺顒€绉撮悿楣冩煠閹间焦娑фい鏇憾閹鈽夊▎妯煎姺闂佸憡眉缁瑥顫忓ú顏咁棃婵炴垼椴搁埢鎾绘⒑閸濄儱校婵炲弶鐗犲﹢渚€姊绘担鍝ヤ虎妞ゆ垵鍟粋宥呪堪閸喓鍙嗛梺鍝勬处椤ㄥ懏绂嶉崜褏纾介柛灞炬皑瀛濋梺鎸庢处娴滄粓顢氶敐鍡欑瘈婵﹩鍘藉▍鏍⒑閸涘﹥澶勫ù婊勭箞瀹曠喖宕橀瑙ｆ嫼婵炴潙鍚嬮悷褔宕氭导瀛樼厽妞ゆ劧缍嗗▓娆愩亜閿旇姤灏﹂柡灞界Х椤т線鏌涢幘瀵哥疄闁挎繄鍋炲鍕箾閹烘挻銇濋柡浣稿暣瀹曟帒顫濋幉瀣耿婵犵數濮烽弫鍛婃叏閹绢喖纾瑰┑鐘宠壘濮规煡鏌ㄩ弴鐐测偓褰掓偂閻斿吋鐓涢柛鎰╁妼閳ь剛鏁婚獮蹇涙晸閻樺磭鍘告繛杈剧悼鏋柡鍡秮閺屸€崇暆鐎ｎ剛袦閻庢鍣崳锝呯暦閸洖惟闁靛ǹ鍎查崐鏇熺節閻㈤潧袨闁搞劌銈搁敐鐐村緞婵炴帗妞介幃銏ゆ偂鎼达絿鏆柣搴ｆ嚀鐎氼厽绔熺€ｎ喖閱囨い蹇撶墛閻撴洘绻涢幋婵嗚埞鐎殿噣绠栭弻锟犲幢閳轰礁绫嶉梺鍝勭焿缂嶄焦鎱ㄩ埀顒勬煃鏉炴媽鍏屽ù鐙€鍘奸—鍐Χ閸愩劌顬堥梺鎸庢磸閸婃繈骞冩ィ鍐╁€婚柦妯侯槺椤撴椽姊洪幐搴㈢５闁稿鎸鹃惀顏堝箚瑜嬮崑銏ゆ煛瀹€瀣М妤犵偛娲、妤佸緞婵犲喚鍟€闂傚倷绀侀幉锟犳晝閳哄倻顩叉繝闈涚墛瀹曞弶绻濋棃娑卞剰妞ゎ偄鎳橀弻銊モ攽閸℃ê娅ч梺鍛婏供閸嬪棛妲愰幘瀛樺闁告挻褰冮崜閬嶆⒑閹肩偛濡兼繛灏栧墲娣囧﹪骞橀鐓庣獩闂佸湱鈷堥崢浠嬪疾閵忋倖鈷戠紒瀣濠€鎵磼椤斿ジ鍙勯柡浣哥Т楗即宕奸悢鍙夊缂傚倸鍊烽悞锕傚Υ閹烘鍋￠柟娈垮枟濞堥箖姊哄Ч鍥х仼闁硅绻濆畷鎴﹀煛閸屾粎顔曢悗鐟板閸犳洜鑺辨總鍛婄厽闁规儳顕ú鎾煙椤旀枻鑰块柟顔界懇楠炴捇骞掗崱妯虹槺闂傚倷绀侀悿鍥涢崟顖€鍥偨缁嬭儻鎽曢悗骞垮劚濞诧妇鈧碍宀搁弻娑樷枎韫囷絾笑闂佽　鍋撴い鎾跺Х绾捐棄霉閿濆懏鎯堥弽锟犳⒑閻戔晜娅撻柛銊ㄦ硾椤曪綁顢曢敃鈧粻娑㈡煛婢跺孩纭堕柣娑栧劦濮婃椽鎮℃惔鈽嗘闂佸憡鎸婚悷锔界┍婵犲洦鍋╃€光偓閳ь剛澹曢懖鈺冪＝濞达綀顕栭悞鐣岀磼閻橀潧浠遍柡灞剧洴閸╋繝宕掑⿰鍕灡闂備胶纭堕弬渚€宕戦幘鎰佹富闁靛牆妫楃粭鎺楁倵濮樼厧澧い鏂跨箰閳规垿宕堕妷銈囩泿闂備礁鎼崯顐⒚洪妶鍫毐闂傚倷妞掔槐顕€姊介崟顖氱闁搞儜灞剧稁缂傚倷鐒﹁摫濠殿垱鎸抽弻娑樷槈濮楀牊鏁鹃梺绋垮閹瑰洤顫忛崫鍔借櫣鎷犻幓鎺濇交闂備線娼ч悧蹇旂仚闂佷紮缍侀弨杈┾偓闈涖偢瀵爼骞嬪┑鍡樻殢濠碉紕鍋戦崐鏍箰缂佹鐝堕柡鍥ュ灩绾惧鏌熼幆褏鎽犲┑顖氼嚟缁辨帡顢涘☉娆戭槰濡炪値鍋勭粔鎾煘閹达富鏁婄紒娑橆儐閻ｈ泛鈹戦埥鍡椾簼闁荤啿鏅涢悾鐑藉箻缂佹﹩妫冨┑鐐村灱妞村摜鈧潧鐭傚娲濞戞艾顣哄┑鈽嗗亝椤ㄥ牏鍒掗銏犵＜闁绘劕顕崢浠嬫⒑绾懏顏犻柛瀣枛閹箖宕归鐘辩盎闂侀潧楠忕槐鏇㈠箠閸ヮ煈娈介柣鎰綑缁楁帡鎽堕弽顬″綊鏁愰崨顔藉創濡ょ姷鍋為〃濠傤潖濞差亝鍋￠柡澶嬪閸婎垶姊烘导娆戠У濞存粠浜滈悾鐑藉箛閺夎褔鏌涘⿰鈧懗鍫曞礉閻戣姤鈷戠憸鐗堝笒娴滀即鏌涘Ο鍝勨挃缂侇喚绮妶锝夊礃閳哄啫骞堟繝鐢靛仜濡霉濮橆儵锝吤洪鍛幗濡炪倖鎸鹃崰搴ｇ箔瑜旈弻宥堫檨闁告挻绻堥敐鐐村緞婵炴帗妞介獮瀣晝閳ь剛娑甸埀顒勬⒑閸涘﹤濮囨俊顖氾躬瀹曠敻寮撮悘璇茬秺閹晛顔忛鐓庡闂備浇妗ㄧ粈浣该洪敂鐐床婵犻潧饪村Σ濂告⒑缁嬫寧鍞夋繛鍛礈缁顓兼径濠勭暰閻熸粌顑夐崺鈧い鎺嶇缁楁帗銇勯锝囩疄闁轰焦鍔欏畷銊╊敆閳ь剟藟濮樿埖鈷戦悹鍥ㄥ絻椤掋垺銇勯弮鈧悧鐘茬暦閹剁瓔鏁嬮柍褜鍓欓悾鐑藉箮閼恒儲娅滈梺鍛婁緱閸ㄥ崬鈻撴ィ鍐┾拺缂備焦蓱鐏忣厾绱掗妸銉у煟鐎殿喓鍔嶇粋鎺斺偓锝庡亞閸橀亶姊洪弬銉︽珔闁哥噥鍋婇幃鐐躲亹閹烘挾鍘遍柣搴秵閸嬪懎鐣峰畝鈧埀顒冾潐濞叉粍鐏欓柧缁樼墪闇夐柨婵嗘噹缁狙囨倵濮橆兙鍋㈡慨濠呮閹风娀寮婚妷顔瑰亾濡や胶绡€闁逞屽墯濞煎繘濡搁敃鈧鍧楁⒑濮瑰洤鐏弸顏呮交濠靛鈷戦柛娑橈工婵偓闂佸搫鎳忛悷褔骞戦姀銈呭耿婵炴垶鐟ч崢浠嬫⒑瑜版帒浜伴柛妯兼櫕缁辩偤骞嬮悩鐢碉紲濠碉紕鍎戠粻鎴﹀箠閹邦喖顥氬┑鍌溓圭痪褔鏌涢锝団槈濠碉紕鏅埀顒冾潐濞叉牠寮甸鍕┾偓鍐Ψ閳哄倸鈧兘鏌熺喊鍗炲箻闁告棃顥撶槐鎾存媴娴犲鎽甸柣銏╁灡鐢喖鎮橀幒妤佲拻濞达絽鎽滄禒銏°亜閹存繃鍤囨い銏℃煥鐓ゆい蹇撴噳閹峰姊虹粙鎸庢拱闁荤喆鍔戝畷妤€鐣濋崟顐㈠殤濡炪倕绻愰悧濠囨偂濞戙垺鐓曟い鎰靛亜娴滄粌顭胯閹告娊寮婚悢纰辨晩闁诡垎鍐ㄧ闂備浇妗ㄧ粈渚€宕愰崹顔炬殾婵犲﹤妫Σ楣冩⒑缂佹ê娴紒鐘崇墪椤繘鎼归崷顓狅紲濠殿喗顨呭Λ娆撴偩閸洘鈷戠紓浣癸供濞堟梻绱掔紒妯虹闁告帗甯楃换婵嗩潩椤掑倻鏆伴柣鐔哥矋缁捇骞冮姀銈呭耿婵炴垶鐟ユ禒顖炴煛婢跺苯浠﹀┑顖欑矙椤㈡鎮㈤崗鑲╁幈闁瑰吋鐣崹瑙勬叏婢跺瞼纾奸弶鍫涘妼濞搭噣鏌涢埞鎯т壕婵＄偑鍊栫敮鎺楀磹閻㈢ǹ纾婚柟鍓х帛閺呮煡骞栫划鍏夊亾閸愯尙顓洪梻鍌欒兌鏋い鎴濇楠炴劖銈ｉ崘銊х枀闂佸湱铏庨崰鏍矆鐎ｎ偁浜滈柟鐑樺灥娴滅偞淇婇崣澶嬬凡闁宠鍨块幃鈺呭垂椤愶絾鐦庨梻浣呵归鍡涙儎椤栫偛鏋侀柛鎰靛櫘閺佸倿鏌涢弴銏℃锭婵炲牄鍔岄—鍐Χ閸℃衼缂備胶濮甸崹鍦垝閸儱绀冮柍鐟般仒缁ㄥ姊洪崫鍕殭婵炲眰鍊栫粋宥堛亹閹烘挾鍙嗗┑鐐村灦閻熝囨儗婵犲洦鐓冮悷娆忓閻忓瓨銇勯姀锛勬噰鐎殿噮鍓熸俊鍫曞醇濮橆兛澹曢柣鐘充航閸斿骸螞椤栨稏浜滈柟鎹愭硾閺嬫垿鏌涙繝鍐ㄥ闁逞屽墲椤煤閿曞倸绀堥柣鏃傚帶缁犳牕霉閻樺樊鍎忕€瑰憡绻傞埞鎴︽倷闂堟稐澹曢柣搴亢濡嫰鍩為幋锔藉€烽柤鎼佹涧濞懷呯磽閸屾氨袦闁稿鎸荤换婵嗏枔閸喗鐏侀梺绋匡攻椤ㄥ懘鍩ユ径鎰仺缂佸娼￠崬鍫曟⒑閸濆嫭宸濋柛鐘虫崌瀹曘垽鏌嗗鍡忔嫼闂佸湱枪濞撮绮婚幘鍓佺＜缂備焦锚濞搭噣鏌熼鈧粻鏍х暦瑜版帩鏁冩い鎰剁秵閸熷秹姊绘担铏瑰笡闁告梹顭囨禍绋库枎閹寸姳绗夋繝鐢靛У绾板秹鎮″☉妯忓綊鏁愰崼顐ｇ秷閻庤娲栭惌鍌炲蓟閳╁啯濯撮悷娆忓绾炬娊姊烘潪鎵妽闁圭懓娲顐﹀箻缂佹ɑ娅㈤梺璺ㄥ櫐閹凤拷?
        retVal=retValue->getSymPtr()->getType(); //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閹冣挃闁硅櫕鎹囬垾鏃堝礃椤忎礁浜鹃柨婵嗙凹缁ㄥジ鏌熼惂鍝ョМ闁哄矉缍侀、姗€鎮欓幖顓燁棧闂備線娼уΛ娆戞暜閹烘缍栨繝闈涱儐閺呮煡鏌涘☉鍗炲妞ゃ儲鑹鹃埞鎴炲箠闁稿﹥顨嗛幈銊╂倻閽樺锛涢梺缁樺姉閸庛倝宕戠€ｎ喗鐓熸俊顖濆吹濠€浠嬫煃瑜滈崗娑氭濮橆剦鍤曢柡澶嬪焾濞尖晠寮堕崼姘殨闁靛繈鍊栭埛鎺懨归敐鍫綈闁稿濞€閺屾稒鎯旈姀掳浠㈤悗瑙勬礃缁捇寮崘顔肩＜婵﹩鍘鹃埀顒夊墴濮婃椽宕ㄦ繝鍌毿曢梺鍝ュУ椤ㄥ﹪骞冮敓鐘参ㄩ柍鍝勫€婚崢鎼佹⒑閹肩偛鍔撮柣鎾崇墕閳绘捇寮Λ鐢垫嚀楗即宕奸姀銏℃瘒婵犳鍠栭敃銈夊箹椤愶絾娅忛梻浣规偠閸庢粓鍩€椤掑嫬纾婚柟鐐窞閺冨牆宸濇い鎾跺缁遍亶姊绘担绛嬫綈鐎规洘锕㈠畷娲冀瑜忛弳锕傛煕濞嗗浚妲虹紒鈾€鍋撻梻鍌氬€搁悧濠勭矙閹烘埈鍟呮繝闈涚墢绾惧ジ鏌嶉柨顖氫壕闂佺ǹ顑嗛幑鍥ь潖缂佹ɑ濯撮柛娑㈡涧缂嶅﹤顕ｉ悽鍓叉晢闁稿本鑹鹃悘濠囨倵閸忓浜鹃梺鍛婃磵閺備線宕戦幘璇茬＜婵綆鍘藉浠嬨€侀弮鍫濆窛妞ゅ繐鎳庨褰掓⒒閸屾瑦绁板┑顔哄€濋弫瀣渻閵堝繐鐦滈柛銊ㄦ硾椤曪綁骞庨懞銉ヤ簻闂佺ǹ绻楅崑鎰板储闁秵鈷戠紓浣光棨椤忓棗顥氭い鎾跺枑濞呯娀鏌ｉ姀銏╂毌闁稿鎸搁埢鎾诲垂椤旂晫浜俊鐐€ら崢濂稿床閺屻儺鏁嬮柨婵嗩槸缁犵粯銇勯弮鍥棄濞存粍绮撳娲箚瑜忕粻鐑樸亜閺囩喓澧电€规洦鍨遍幆鏃堝Ω閿旇瀚藉┑鐐舵彧缁叉寧鐏欏銈冨劚缁绘﹢寮婚敐澶婄鐎规洖娲ら埅鐢告倵鐟欏嫭纾婚柛妤€鍟块锝嗙鐎ｎ€晝鎲歌箛姘ヤ汗濡わ絽鍟埛鎴︽煕濞戞﹫鏀诲璺哄閺屾稓鈧急鍐у闂傚倷娴囬妴鈧柛瀣尭闇夐柣妯烘▕閸庢劙鏌ｉ幘璺烘灈闁哄瞼鍠栭弻鍥晝閳ь剚鏅堕鍓х＜闁绘瑦鐟ュú锕傛偂閸愵亞纾藉ù锝堝亗閹寸姷鐭嗛柍褜鍓熷濠氬炊瑜滃Σ鍦磼缂佹绠栫紒缁樼箞瀹曟帒顫濋梻瀵哥；闂傚倷鐒﹀鍧楀储閻ｅ本鏆滈柣鎰惈閻掑灚銇勯幒鎴濇灓婵炲吋鍔栫换娑㈠矗婢舵稖鈧潡鏌℃担绋挎殻濠殿喒鍋撻梺闈涚墕閹冲繐危椤掑嫭鈷戦梺顐ゅ仜閼活垱鏅堕鐐寸厪闁搞儜鍐句純濡ょ姷鍋炵敮鈥崇暦閸楃儐娓婚柟顖嗗本顥″┑鐘殿暜缁辨洟宕戦幋锕€纾归柕鍫濇椤╁弶绻濇繝鍌滃闁汇値鍣ｉ幃妤€鈽夊▍顓″亹閹广垽宕卞☉娆戝幈濡炪倖鍔х徊鍓х矆閳ь剙螖閻橀潧浜奸柛銊у劋缁岃鲸绻濋崶鑸垫櫖濠电姴锕ら崯顐ｄ繆閸濆嫧鏀介柣鎰皺婢ч亶鏌涚€ｎ亷宸ユい顐㈢箳缁辨帒螣鐠囧樊鈧捇姊洪崨濠勬创婵炰匠鍛濠电姴娲㈤埀顑跨窔瀵粙顢橀悙闈涒偓鐐差渻閵堝棗鍧婇柛瀣尵缁辨帡骞囬褎鐤侀梺鍝勫閳ь剚鍓氶崥瀣箹缁厜鍋撳畷鍥跺晣闂傚倷绀侀幖顐︻敄閸曨垱鍤勯柛顐ｆ礀閻掑灚銇勯幒鍡椾壕缂備胶濮寸粔鐟扮暦閺囥垺鐒肩€广儱鎳愰悿鍥ㄧ節閵忥絽鐓愰柛鏃€娲熷畷娆撴偐鐠囪尙顔愬┑鐑囩秵閸撴瑦淇婃禒瀣厽闁规儳顕幊鍥煛鐏炲墽娲存鐐疵灃闁逞屽墴瀵宕ㄩ鑲╃獮闂佸綊鍋婇崢楣冨储閻㈠憡鈷掑ù锝呮憸娴犮垺銇勯幋婵囶棦濠碉繝娼ч埞鎴﹀炊閵娿儱浼庡┑鐘垫暩婵挳骞婅箛娑辨晜闁绘鏁哥壕鍏笺亜閺冨倹娅曢柟鍐叉嚇閺岀喖鎮烽弶娆句純婵犵鍓濋悺鏇⒙ㄦ笟鈧弻娑㈡偄妞嬪骸鈪靛┑顔硷功缁垶骞忛崨鏉戝窛闂傚牊绋撶粔鐑樸亜閵忥紕澧电€规洖宕埥澶娢熺喊杈ㄐュ┑鐘垫暩閸嬫稑螞濞嗘挸绀夐柟鐑樻⒐瀹曟煡鏌涢埄鍐姇闁绘挻娲橀妵鍕箛閸撲胶蓱缂備讲鍋撻柍褜鍓涚槐鎾存媴閸濆嫅銉х磼椤曞懎鐏﹀┑锛勬暬瀹曠喖顢涘杈╂綁闂備胶枪閺堫剛绮欓幒妤佸仧妞ゅ繐鐗婇埛鎴︽煕濞戞﹩鐒甸柟杈剧畱缁犳牠鏌涚仦鎯у毈婵炲吋鐗楃换娑橆啅椤旇崵鐩庨悗鐟版啞缁诲倿鍩為幋锔藉亹閺夊牜鍋勯崢锛勭磽娴ｇ瓔鍞虹紒鑸佃壘椤繐煤椤忓嫮顔愰梺缁樺姈瑜板啴鈥栫€ｎ偆绡€闁冲皝鍋撻柛鏇炵仛閻ｈ偐绱撴笟鍥ф灈妞ゆ垵顦甸悰顕€骞掗幊铏⒐閹峰懘宕崟顐嶏箓姊虹拠鏌ヮ€楃紒鐘茬Ч瀹曟洟宕￠悙宥嗙☉閳藉濮€閻樼數鍘梻浣虹《閸撴繈濡靛☉銏犵哗濞寸姴顑嗛悡鐔镐繆椤栨繂浜归悽顖涚⊕缁绘盯骞婂畡鐗堝闁绘挾鍠愭穱濠囧Χ閸屾矮澹曟繝纰樻閸嬪懘鎮疯閸掓帞鈧綆鍠栫粻铏繆閵堝嫯鍏岄柛鏂跨埣閹鐛崹顔煎濠碘槅鍋呴惄顖氱暦閵忋倕宸濆┑鐘插椤旀洟鏌℃径濠勫濠⒀傜矙楠炲啯绺界粙鍨殤濡炪倕绻愰悧濠囨偂濞戙垺鐓曢柍钘夋楠炴﹢鏌熼绗哄仮闁哄苯绉烽¨渚€鏌涢幘瀵哥疄闁挎繄鍋炲鍕箾閹烘垶鎯堥摶锝夋煕韫囨挸鎮戞繛鍫滃嵆濮婃椽宕烽鐐插濡炪們鍔岄幊姗€骞冮敓鐘茬伋闁归鑳堕崬鐢告煟閻樿崵绱伴柕鍡忓亾闂佹椿浜滈鍡欐崲濞戞矮鐒婇柡宥冨妽閻濇洟姊洪棃娑欐悙閻庢矮鍗抽悰顕€骞掑Δ鈧獮銏′繆閻愭潙鍔ゆい銉﹁壘閳规垿鎮╅幇浣告櫛闂佸摜濮甸惄顖炴晲閻愭潙绶為柟閭﹀幖娴滄姊洪悙钘夊姕闁告挻鑹鹃…鍥冀瑜夐弨浠嬫煟濡櫣鏋冨瑙勵焽閻ヮ亪骞嗚閹垹绱掔紒妯兼创鐎规洖宕灒闁惧繐婀遍幊鍡涙⒒娴ｅ憡鍟為柨鏇ㄥ亞濡叉劙寮撮悩鎰佹綗闂佽鍎抽顓㈡偡瑜版帗鐓曢柕澶嬪灥鐎氼喛銇愰鐐粹拻濞达綀顫夐崑鐘绘煕閺傝法校缂佹梻鍠栧畷鍗炩槈濡偐鍘梻浣告啞缁嬫垿鎮洪妸鈺佺厱闁硅揪闄勯悡鏇熺節闂堟稒顥滄い蹇ｅ亰閺屽秶鎷犲顔解枅闂佸搫鏈粙鎴ｇ亽闂佸壊鐓堥崰鏇炐уΔ浣虹瘈闁靛骏缍嗗鎰箾閸欏绠炲┑锛勬暬楠炲洭寮剁捄顭戝敽闂備浇顫夊畷妯衡枖濞戙垻宓侀柛顐犲劜閳锋帒霉閿濆洦鍤€妞ゆ洘绮嶆穱濠囶敃閿涘嫮鐛㈠銈冨灪娣囨椽藝瑜版帗鐓熸繛鎴濆船濞呭秵顨ラ悙鍙夊枠闁诡啫鍥ч唶闁靛繒濮寸粻鐐烘⒒娴ｇ瓔鍤欓柛鎴犳櫕缁辩偤宕卞☉妯硷紱闂佸憡渚楅崹顖炴倿婵犲啨浜滈柟鎹愭硾閺嬪倸霉濠婂嫮鐭嬮柕鍥у楠炲洭宕滄担鐟颁还缂傚倷鑳舵慨鐑藉磻閵堝钃熼柣鏃傗拡閺佸﹪鏌ｉ敐鍛拱闁革絾婢橀—鍐Χ閸愩劎浠惧銈冨妼閿曨亜鐣峰ú顏勭劦妞ゆ帊闄嶆禍婊堟煙閻戞ê鐏ユい蹇旀尦閺屸剝鎷呯粙搴撳亾閸ф钃熼柕濞炬櫆閸嬪棝鏌涚仦鍓р槈妞ゅ骏鎷�?
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
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆撴倻濡桨鐢绘繝鐢靛Т閿曘倝宕板顑╃喖鍩€椤掑嫭鍊垫繛鍫濈仢閺嬬喖鏌熼鐓庘偓鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸閻忕偟纭堕崑鎾崇暦閸ャ劍鐣烽梻渚€鈧偛鑻晶瀵糕偓瑙勬磻閸楀啿顕ｆ禒瀣垫晣闁绘劗鏁搁弳顐︽煟閻斿摜鐭嬮柡灞诲姂閹箖鏌ㄧ€ｎ剟妾梺鍛婄☉閿曘倖绂嶅⿰鍫熲拺闁告稑锕︾粻鎾绘倵濮樼厧澧柍缁樻崌婵＄兘鍩￠崒妤佸闂備胶枪閺堫剟鎮疯钘濋柨鏇炲€归悡娆撴偣閸ュ洤鎳愰惁鍫ユ倵鐟欏嫭绀€婵犫偓闁秴绠熼柟缁㈠枛缁€瀣亜閹哄棗浜鹃悗娈垮枛濞尖€愁潖濞差亜鎹舵い鎾墲椤庢姊洪崫銉バｉ柣妤冨█楠炲啴鍨鹃幇浣瑰缓闂侀€炲苯澧寸€殿喖顭烽幃銏ゅ川婵犲嫮肖濠德板€х徊浠嬪疮椤栫儐鏁佺€广儱顦伴埛鎴犵磼鐎ｎ偒鍎ラ柛搴㈠姍閺屾盯寮埀顒勬偡閳哄懐宓侀柛鎰╁壆閺冨牆宸濇い鎾跺枎缁佽埖淇婇悙顏勨偓鏍箰妤ｅ啫纾规い鎰剁畱椤懘骞掗搹顐ゎ洸婵犲﹤鐗婇悡蹇擃熆閼哥數鈽夋い鈺婂墰缁辨帡鎮╅幇浣哄姺缂備浇椴哥敮妤€顭囪箛娑樜╅柨鏇楀亾闂傚绉瑰娲偡閺夋寧些闂佺娅曢敋闁伙絿鍏橀、鏇㈡晝閳ь剛绮堢€ｎ剛妫柣妤€鐓鍛洸闁告挆鈧崑鎾诲礂婢跺﹣澹曢梻浣告啞濞诧箓宕滃☉銏犲偍闂侇剙绉甸埛鎴︽⒒閸喕鍎愮憸鐗堝笒绾惧潡鏌熼幆鐗堫棄闁哄嫨鍎甸弻銊モ攽閸℃顦遍梺绋款儐閹告悂鍩㈤幘璇插瀭妞ゆ梻鏅禍顏呬繆閻愵亜鈧倝宕㈡ィ鍐ㄧ婵☆垯璀﹂崵鏇熴亜閹板墎鐣辩紒鐘崇⊕閵囧嫰骞樼捄杞版埛缂備焦鍔楅崑鐐垫崲濠靛鍋ㄩ梻鍫熷垁閵忋倖鍊垫慨妯哄船閸樺鈧娲忛崕闈涚暦閵娧€鍋撳☉娅辨岸骞忕紒妯肩閺夊牆澧介幃濂告煙閾忣偅宕屾い銏¤壘楗即宕ㄩ婊愮床闂佽崵濮村ú锕併亹閸愵喖鐒垫い鎺嗗亾婵炵》绻濋幃浼搭敊閽樺绐炴繝鐢靛仧閸嬫捇锝炲澶嬧拺闂傚牊绋撶粻鍐测攽椤旀儳鍘存い銏＄懇楠炴帡寮崒婊愮闯闂備胶枪閺堫剟鎮疯瀹曟繂顓奸崱鎰盎濡炪倖鎸鹃崑鐐哄窗濡皷鍋撶憴鍕８闁搞劋绮欓悰顕€寮介妸锕€顎撻梺绋跨箰椤︽壆鈧艾銈稿缁樻媴閸涘﹤鏆堢紓浣割儐閸ㄥ潡寮崘顔芥櫜闁搞儺鐓堥崑銊╂⒒娓氬洤澧紒澶屾暬閹繝寮撮姀锛勫帾闂佸壊鍋呯换鍐夊⿰鍕╀簻闁靛鍎洪崕鎰磼缂佹娲存鐐差儔閹晠鎼归锝囨闂佽姘﹂～澶娒鸿箛娑樼闁硅揪璐熼埀顑跨閳藉螣闁垮鈧姊虹涵鍛涧缂佽弓绮欏畷銏ゎ敇閻旇櫣鐦堥梺姹囧灲濞佳冩毄闂備浇妗ㄧ粈渚€骞夐敓鐘茬疄闁靛ň鏅涚粻缁樸亜閺冨洦顥夐柛鎿勯檮缁绘繈濮€閿濆懐鍘梺鍛婃⒐濞叉粓鎯€椤忓浂妯勫Δ鐘靛仦閻楁粓鍩€椤掍胶鈯曢拑閬嶆倶韫囷絽骞栭棁澶嬬節婵犲倻澧㈤柣锝嗘そ閺屸€崇暆鐎ｎ剛鐦堥悗瑙勬礀閻栧ジ宕洪埄鍐╁闁搞儯鍔庡ú鎾煛鐏炲墽銆掗柍褜鍓ㄧ紞鍡涘磻閸涱垯鐒婃い鎾跺枂娴滄粍銇勯幇鍓佹偧缂佺姷鍋ら弻鐔碱敊閻熸澘鈷夐梺璇″灡濡啴寮澶婄妞ゆ挾鍠撶粔顔尖攽閻樻鏆滅紒杈ㄦ礋瀹曟垿骞嬮敃鈧壕褰掓煙鏉堝墽鐣遍柛銊ュ€块弻娑樷攽閸℃寮稿┑鐐村灟閸ㄥ湱绮堥崘鈹夸簻闁哄啫娲ゆ禍褰掓煟閿濆棙銇濇慨濠冩そ瀹曟﹢宕ｆ径瀣壍闂備礁鎽滈崰鎰珶閸℃鐝堕柡鍥ュ灩缁€鍌炴⒒閸屾凹鍤熺紒鐘宠壘椤啴濡堕崱娆忣潷缂備緡鍠栭惉濂稿极椤曗偓瀹曞ジ寮撮悢铚傜紦婵＄偑鍊栭悧婊堝磻閻斿吋鍋╅柣鎴炆戦崣蹇撯攽閻樻彃顏悽顖涚洴閺岀喎鐣￠悧鍫濇畻闂佸湱鍘х紞濠囨偘椤旇姤鍎熼柟鎯у帠婢规洘绻濋悽闈浶㈡繛璇х畵閹繝寮撮姀锛勫幍闂佽鍨庣仦鑺ヮ啀缂傚倷鑳舵慨鐑藉磻閵堝钃熼柡鍥ュ灩楠炪垺淇婇妶鍌氫壕闂佸搫妫庨崹鑽ゆ閹烘鍋愮€规洖娲犲Σ鍫ユ⒑鐎圭姵顥夋い锔诲灥閻忔帞绱掔紒銏犲籍妞ゃ儲鎹囧畷銉ㄣ亹閹烘挴鎷婚梺绋挎湰閻熝囁囬敃鍌涚厵缁炬澘宕禍鎵偓瑙勬处閸ㄥ爼骞冮埡鍐╁珰闁肩⒈鍓ㄧ槐閬嶆⒒娴ｅ憡璐￠柛瀣崌閹箖顢旈崨顖ｆ锤濠电姴锕ら悧濠囨偂韫囨挴鏀介柣妯垮皺缁犳娊鏌涢幒宥呭祮闁哄本鐩顕€骞橀崜浣规闂備礁鎼惉濂稿窗閺嶎厹鈧礁鈻庨幘鏉戜患闁诲繒鍋為崕鍐测枍濮樿埖鈷掑ù锝囧劋閸も偓缂佸墽铏庨崢鐣屾崲濞戙垹鐒垫い鎺嶉檷娴滄粓鏌曡箛濠冾潑婵炲牊绮庨埀顒冾潐濞叉鏁埄鍐х箚闁割偅娲栧婵囥亜閺冨倹娅曠憸甯秮濮婂宕掑顑藉亾閹间礁纾归柟闂寸绾剧懓顪冪€ｎ亝鎹ｉ柣顓炴閵嗘帒顫濋敐鍛婵°倗濮烽崑鐐烘偋閻樻眹鈧線寮撮姀鐘栄囨煕閳╁啨浠︾紒顔瑰墲娣囧﹪鎮欓鍕ㄥ亾閺嶎厼绀夌憸蹇曞垝婵犳艾绠ｉ柣妯兼暩閻掑吋绻濋姀锝呯厫闁告梹娲滅划鑽ゆ喆閸曨厾顔曢梺鎸庣箓妤犲憡鏅堕灏栨斀闁炽儳鍋ㄩ崑銏ゆ煛瀹€瀣М妤犵偞甯￠獮瀣偐鏉堚晠鈹忛梻鍌欑窔濞佳兠洪妶鍥ｅ亾濮橆偄宓嗛柣娑卞櫍楠炴帒螖閳ь剛绮婚敐鍡愪簻闁哄秲鍔忔竟妯汇亜椤愩倕啸缂佽鲸鎹囧畷鎺戔枎閹板灚鈻婇梻浣哄帶閸熷潡鎮ラ悡搴ｆ殾婵°倐鍋撴い顐ｇ矒閸┾偓妞ゆ巻鍋撴い鏇悼閹风姴霉鐎ｎ偒娼旈梻渚€娼х换鎺撴叏閻㈡潌澶娾攽鐎ｎ偀鎷洪梺闈╁瘜閸樻劙宕烽娑樹壕婵炴垶甯楀▍濠冾殽閻愯尙绠叉繛鐓庣箻閸╋繝宕橀搴ｉ棷婵犵數鍋犻幓顏嗗緤閹稿海浠氶梻渚€鈧偛鑻晶顕€鏌熼懞銉х煂闁告帗甯″顕€宕奸悢铚傜紦缂備礁澧芥晶妤呮晪婵炲瓨绮嶉崕鎶芥箒闂佹寧绻傞幊搴ㄥ汲濞嗗緷鐟扳堪閸愵亞楔闂佸搫鑻粔鐑铰ㄦ笟鈧弻娑㈠箻鐎涙娈ょ紓渚囧枟閻熲晠鐛幒鎴富闁绘垟鏅涙禍楣冩煥閺囩偛鈧悂鎮欐繝鍐︿簻闁瑰搫妫楁禍楣冩⒑缁嬭儻顫﹂柛鏃€鍨垮璇测槈濡攱顫嶅┑顔筋殔閻楀棙鎱ㄥ☉姘辩＝濞达絽寮堕鍡涙煕鐎ｎ偅宕屾慨濠勭帛缁楃喖鍩€椤掆偓椤洩顦虫い銊ｅ劥缁犳盯寮撮悙鐢电摌闂備礁鎲￠幐鍡涘礋椤愩垹绠叉繝寰锋澘鈧呭緤娴犲鐤い鏍仦閸嬪倿鏌曟径鍡樻珕闁绘挻鐩弻娑㈠Ψ閿旇姤鐎梻渚囧弾閸ㄨ鲸绌辨繝鍕＜闁靛繒濮甸悵婵嬫⒑閸濆嫮鐒跨紒缁樼箓閻ｇ兘濡搁埡濠冩櫍闂佺粯枪鐏忔瑩宕㈤挊澶嗘斀闁绘ɑ鍓氶崯蹇涙煕閻樺啿娴€规洘鍨块獮妯肩磼濡攱瀚奸梻鍌氬€搁悧濠勭矙閹惧瓨娅犻柡鍥╁枍缁诲棙銇勯幇鍓佹偧婵炴彃顕埀顒冾潐濞插繘宕规禒瀣摕闁糕剝顨忛崥瀣煕閳╁啰鎳冩鐐搭殜濮婃椽鎮烽弶鎸庡€梺璇″灠閻倸鐣烽悷鎵殾闁搞儮鏅濋悡瀣⒑濮瑰洤鐏╅柟璇х節閹繝寮撮姀鈥斥偓鐢告煥濠靛棛鍑圭紒銊ャ偢閺屾盯鏁愰崼鐕佹闂傚洤顦扮换婵囩節閸屾凹浠鹃梺绋匡攻閸旀瑩寮婚悢纰辨晩闁煎鍊楅悡鎾绘倵鐟欏嫭纾搁柛鏂跨Ф閹广垹鈹戠€ｎ亞顦板銈嗗姂閸ㄥ鎮烽弻銉︹拻濞达綀妫勯崥褰掓煕閻樺啿濮夐柛鎺撳笒椤撳ジ宕惰閻忓﹪姊虹憴鍕姢缁剧虎鍙冨鎶芥晜閻ｅ瞼鐦堟繝鐢靛Т閸婃悂寮搁妶鍡欑閻忓繑鐗楀▍濠囨煛鐏炶鈧繈鐛弽銊﹀閻熸瑥瀚伴弫顏呯節閻㈤潧浠滈柣妤€绻樺畷鎴﹀Χ閸滀焦缍庨梺鎯х箺椤鈧碍宀搁弻宥夊Ψ閵壯嶇礊闂侀€炲苯澧茬€规洜鏁婚崺鈧い鎺嗗亾缂佺姴绉瑰畷鏇㈡焼瀹ュ棗浜遍梺鍛婂姦閸犳牜澹曟繝姘厵闁硅鍔﹂崵娆撴⒑鐟併倕鈧牗绌辨繝鍥ч柛鏇ㄥ櫘閸炲綊姊洪悷鏉挎Щ闁圭⒈鍋婇垾鏃堝礃椤斿槈褔鏌涢埄鍐炬當婵炲吋鍨垮铏圭矙濞嗘儳鍓遍梺鍦规晶钘壩ｉ幇鏉跨闁规儳纾粣鐐寸節閻㈤潧孝濡ょ姷枪閳绘捇宕滄担铏癸紳婵炴挻鑹惧ú銈夊几濞戞瑣浜滄い鎾跺Т閸樻挳鏌曢崱鏇狀槮闁宠棄顦灒鐎瑰嫭澹嗘晶顖炴⒒娴ｅ憡鍟炴繛璇х畵瀹曟垿宕ㄧ€涙ê鈧潧螖閿濆懎鏆為柍閿嬪笒闇夐柨婵嗙墛椤忕娀鎮介娑氣姇闁靛洤瀚版俊鎼佸Ψ閿曗偓濞堟姊虹拠鈥虫殭闁搞儜鍐偓鍨攽閳藉棗鐏犻柣銊у厴瀵劑宕樺顔藉瘜闂侀潧鐗嗗Λ娑欐櫠闁秵鐓曢悗锝庡墮娴犻亶鏌熼鍏夊亾閺傘儲顫嶅┑鈽嗗灠缁绘劗绱炴繝鍥ф瀬闁圭増婢橀崥褰掓煢濡警妲虹紒澶庢閳ь剚顔栭崰妤佺箾婵犲洤鏋侀柟閭﹀幖缁剁偤鎮楅敐搴″妤犵偞鍔欏铏规嫚閼碱剛顔婇梺绋款儑婵炩偓妞ゃ垺淇洪ˇ鎶芥煙娓氬灝濮傜€规洘甯掗埞鍐箚瑜屾竟鏇炩攽閻愯尙澧曢柣蹇旂箓閳诲秴顫滈埀顒勫蓟濞戙垺鏅查煫鍥ㄦ礀閸╁矂姊烘导娆戞偧闁稿繑锚閻ｉ攱绺界粙鍨祮濠碉紕鍋犻褎绂嶆ィ鍐╃厸鐎广儱楠稿楣冩煛娴ｉ潻韬柡灞诲€濋獮渚€骞掗幋婵嗩潛闂傚倸鍊搁ˇ顖滅矓閸撲焦顫曢柟鎯х摠婵挳鏌ц箛鏇熷殌缂佹绻濆娲倻閳哄倹鐝﹂梺鎼炲妺閸楁娊骞冩ィ鍐╁€婚柦妯猴級閳哄懏鐓冮柛婵嗗閺€濠氭煛閸涱喚绠炴慨濠冩そ濡啫鈽夊▎鎴炊婵犵數濮幊鍥磻閹惧墎纾藉ù锝勭矙閸濇椽鏌ｉ悤鍌氼洭闁瑰箍鍨归埞鎴﹀幢韫囨梻浜栭梻浣稿閻撳牓宕板Δ鍛€堕柛顐犲劜閳锋垿鏌熺粙鍨劉缁剧偓鎮傞弻娑欐償閵娿倖鍠氶梺鐐藉劜濡啫鐣峰鈧、娆撴寠婢跺本顎嶆繝鐢靛О閸ㄧ厧鈻斿☉銏犲珘妞ゆ巻鍋撻柍缁樻崌楠炲棜顧佹繛鎾愁煼閺屾洟宕煎┑鍥舵婵犳鍟崨顖滐紲闂佺粯锚閸熷潡鎮橀埡鍐＜妞ゆ棁鍋愭晶銏ゆ煃瑜滈崜銊х礊閸℃稑纾诲ù锝呮贡椤╁弶绻濇繝鍌滃闁绘挻鐟╁娲敇閵娧呮殸闂佸搫顑嗛惄顖炲蓟閿涘嫧鍋撻敐搴濋偗妞ゅ孩顨婂Λ浣瑰緞鐎ｎ剛鐦堟繝鐢靛Т閸婄粯鏅堕姀銈嗙厽闁挎繂妫欓妵婵嬫煛瀹€瀣？濞寸媴绠撳畷婊嗩槼闁告帗绋戦—鍐Χ閸愩劎浠惧┑鐐跺皺閸犳牕顕ｉ锕€绠涢柡澶婄仢閼板灝鈹戦悙鍙夘棡閽冭京鎮敐鍥╃＝闁稿本鑹鹃埀顒傚厴閹虫宕奸弴鐐碉紱闂佽宕橀褏澹曠捄銊㈠亾鐟欏嫭绀€婵炲眰鍔庣划鍫熺節閸屾鏂€闂佺粯锚瀵爼骞栭幇鐗堢參闁告劦浜滈弸娑㈡煛鐏炵晫效濠碉紕鍏橀弫鍐焵椤掍焦娅犳い鏍仦閻撴洘鎱ㄥ鍡楀⒒闁稿孩姊归〃銉╂倷鐎电ǹ鈷堥梺鍛婂笚鐢€崇暦濡や礁绶炲┑鐘辩椤ユ岸姊绘担钘変汗闁冲嘲鐗撳畷婊冣槈閵忕姵妲悗骞垮劚椤︿即鍩涢幒妤佺厱閻忕偛澧介幊鍡涙煕韫囨挾鐒哥€殿喗鎮傞、鏃堝川椤愶紕鐩庨梻浣烘嚀閻°劎鎹㈤崘顔㈠濮€閳藉棙顔旈梺缁樺姈濞兼瑦鎱ㄥ鍡╂闁绘劕顕晶顒傜磼濡ゅ啫鏋戠紒缁樼箞瀹曠喖顢樺☉娆欑礄濠电姷鏁告慨鐑藉极閸涘﹦绠鹃柍褜鍓氱换娑欐媴閸愬弶绁╅柡浣稿閺岀喖鎮ч崼鐔哄嚒闂佸搫顑勭欢姘跺蓟濞戙垹鐒洪柛鎰典簴婵洭姊虹紒妯诲鞍婵炶尙鍠栭獮鍐潨閳ь剟骞冨▎鎾崇闁瑰搫妫欑€垫牠姊绘担鑺ャ€冮柣鎺炵畵閹囨偐鐠囪尪鎽曢梺鍝勬川閸犳挾绮绘ィ鍐╃厽闁逛即娼ф晶顖炴煟濠靛洦绀嬫慨濠冩そ楠炲酣鎳為妷锔芥闂備焦鎮堕崝灞筋焽閳ユ剚鍤曢悹鍥ㄧゴ濡插牊淇婇鐐存暠闁哄倵鍋撻梻鍌欒兌绾爼宕滃┑瀣ㄢ偓鍐疀閺傛鍤ら梺绋挎湰閸戠懓銆掓繝姘厪闁割偅绻堥妤€霉濠婂啫鈷旂紒杈ㄥ浮閹晠鎼归銏紦闁诲孩顔栭崰娑㈩敋瑜旈崺銉﹀緞閹邦剦娼婇梺缁橆焽閺佺ǹ顭囨径鎰拻濞撴埃鍋撻柍褜鍓涢崑娑㈡嚐椤栨稒娅犻悗鐢电《閸嬫挾鎲撮崟顒傤槬缂傚倸绉撮敃銈夋偩妞嬪海鐭欓悹鍥╁亾濡啫鐣峰鈧俊姝岊槹闁稿锕濠氬磼濞嗘埈妲梺纭咁嚋缁绘繂鐣峰┑瀣櫇闁稿本姘ㄩ悰銉╂⒑閸濆嫮鈻夐柛娆忕箳缁辩偤寮介鐔哄幈濠电偞鍨堕…鍥箺閻樼粯鐓熼柟鎯х－缁犲鏌＄仦鍓ф创妞ゃ垺娲熼弫鎰板炊閳哄啯姣囬梻鍌欑窔濞艰崵寰婇挊澶涜€块弶鍫氭櫆椤洟鏌熼悜姗嗘當缂佺姵绋掗妵鍕箻鐠虹洅娑樷攽椤斿吋宸濈紒杈ㄦ尰閹峰懘鎼归悷鎵偧缂傚倷娴囬褔鎮ч幘鑽ゅ祦闁告劏鏅濋々鐑芥倵閿濆骸浜芥俊顐㈠暙閳规垿鎮欏▓鎸庮棑缂傚倸鐗呴崡鍐茬暦鐟欏嫮顩烽悗锝庝簽閻ｅ搫鈹戞幊閸婃劙宕戦幘娣簻妞ゆ劧绲跨粻鐐存叏婵犲懎鍚归柍褜鍓熷褔骞夐敓鐘茬柧闁稿繗鍋愮弧鈧紒鍓у鑿ら柛瀣崌瀹曟﹢鎳犻鍕礈闂傚倷绀侀幉鈥愁潖瑜版帗鍋￠柕澶嗘櫓閺佸鏌ㄥ┑鍡╂Ц缂佺姷绮妵鍕籍閸屾瀚涘┑鐐靛帶閿曨亜顫忓ú顏勭闁兼亽鍎插▓浼存⒑缁嬫鍎愰柟鐟版喘瀵鎮㈤崜鍙壭ч柟鑹版彧缁茬晫绮敍鍕＝濞达綀娅ｇ敮娑氱磼鐎ｎ偄绗掓い顓炴穿缁犳稑鈽夊Ο纰辨Ф闁荤喐绮岀粔鐟扮暦閸濆嫮鏆嗛柛鏇ㄥ厴閹疯櫣绱掔紒銏犲箹闁瑰啿姘﹂。璺ㄧ磽閸屾瑧璐伴柛鐘崇墪闇夐柣鎴ｆ缁€鍡涙煙閻戞﹩娈旂紒顐㈢Ч閺屾盯顢曢妶鍛€婚梺閫炲苯澧扮紒顕呭灡缁岃鲸绻濋崶鑸垫櫖闂佺粯鍔曢悺銊╂偟瀹曞洨纾藉ù锝夋涧婵″吋銇勯鐐叉Щ妞ゎ偄绻愮叅妞ゅ繐鎷嬪Λ鍐ㄢ攽閻愭潙鐏卞瀛樻倐椤㈡捇宕卞☉娆屾嫽闂佺ǹ鏈懝楣冾敂閳哄懏鍊垫慨妯煎帶婢ф挳鏌熼钘夊姢闁伙綇绻濋獮宥夘敊閼恒儳鏆﹂梻鍌欑窔閳ь剛鍋涢懟顖涙櫠閹绢喗鐓曢柍瑙勫劤娴滅偓淇婇悙顏勨偓鏍暜閹烘绐楁慨姗嗗墻閻掍粙鏌熼柇锕€骞樼紒鐘荤畺閺屾稑鈻庤箛锝嗩€嗛梺鍏兼緲濞硷繝寮婚弴銏犲耿婵°倐鍋撻柡鍡悼閳ь剝顫夊ú蹇涘礉閹达妇宓佺€广儱顦介弫濠囨煛閸曨偆绠伴柣鎺戭煼濮婂宕掑顑藉亾閹间礁纾归柣鎴ｅГ閸ゅ嫰鏌涢锝嗙闁稿被鍔庨幉鎼佸籍閸喐娅滈梺缁樺姇閹碱偊宕￠幎鑺ョ厪闊洦娲栧暩闂佹寧绋掗悷鈺侇潖濞差亝顥堟繛鎴炶壘椤ｆ椽姊虹拠鈥虫灕闁稿骸顭锋俊鐢稿箛閺夎法顔婇梺鐟邦嚟婵厼危鐟欏嫪绻嗛柣鎰典簻閳ь剚鐗犲畷婵嬫晝閸屾氨锛涢梺鍛婁緱閸亪宕戦幘缁橆棃婵炴垶姘ㄩ崝顖氣攽椤旂》鏀绘俊鐐舵铻為柛鎰╁妷濡插牊鎱ㄥ鍡楀⒒闁哄棭鍋呯换婵堝枈濡椿娼戦梺绋款儏閹虫劗鍒掑▎鎾崇婵°倐鍋撻柣銈夌畺閺屾盯顢曢敐鍡欘槰闂佺粯甯熼崑鎰板焵椤掆偓缁犲秹宕曢崡鐐嶆稑鈽夐姀鐘插亶闂備緡鍓欑粔鐢告偂閻旂厧绠归柟纰卞幖閻忥絿绱掓径灞炬毈闁哄本鐩獮妯尖偓闈涙啞閸ｎ厾绱撴担绋库偓鍦暜閿熺姴钃熼柛鈩冾殢閸氬鏌涢埄鍐噧妤犵偛绉垫穱濠囨倷椤忓嫧鍋撻弽顬℃椽寮介‖顒佺☉閳藉顫濋鈧ⅲ闂備線鈧偛鑻晶顖滅磼缂佹绠橀柛鐘诧攻瀵板嫬鐣濋埀顒勬晬閻斿吋鈷戠紒瀣硶缁犳煡鏌ㄩ弴妯虹仼闁伙絽鍢茬叅妞ゅ繐鎳忓▍銏ゆ⒑缂佹〒鍦焊濞嗘挻鏅繝濠傚暊閺€浠嬫煟濡鍤嬬€规悶鍎甸弻娑㈡偆娴ｉ晲绨界紓渚囧枦椤曆呭弲濡炪倕绻愰幊澶愬箯婵犳碍鈷戠紒瀣濠€浼存煟閻旀繂娉氶崶顒佹櫇闁稿本绋撻崢鐢电磼閻愵剚绶茬€规洦鍓氱粋宥夊箥椤旂懓浜鹃悷娆忓缁€鍐煕閺冣偓閻熴儵鎮鹃悜鑺ュ€荤紒娑橆儐閺咃絽鈹戦悙鏉戠仸閻顭跨憴鍕缂佽鲸鎸婚幏鍛嫚閳╁啰鏆︾紓鍌欑椤戝棝宕归悽闈涘疾闂備線娼ч悧鍡浰囨导鏉戠；妞ゅ繐鎳屾禍婊堟煛閸ヮ煈娈斿ù婊堢畺濮婃椽宕崟顓犲姽缂傚倸绉崇欢姘舵偘椤曗偓瀹曞爼顢楁径瀣珦闂備胶绮幐鍛婎殽閹间讲鈧牠宕卞☉娆屾嫽婵炶揪缍€濞咃絿鏁☉銏＄厵闁归棿鑳堕悾铏光偓瑙勬磻閸楀啿顕ｆ禒瀣垫晣闁绘棁娓圭花鍨繆閻愵亜鈧牠宕濊缁骞嬮敂钘夆偓宄扳攽閻樻彃浜炴繛鎾愁煼閹鏁愭惔婵嬪仐濡炪倕瀛╅〃濠囧蓟濞戙垹鐓涘ù锝堟椤忣參姊虹€圭媭娼愰柛銊ユ健楠炲啫鈻庨幘宕囩厬婵犮垼娉涢敃銉╊敂閻樼數纾介柛灞剧懅椤︼附銇勯敂璇茬仯缂佽京鍋炵换婵嬪磼閵堝懏顓垮┑鐘垫暩婵敻鎳濇ィ鍐╁€剁€规洖娲犻崑鎾舵喆閸曨剛顦ュ┑鐐额嚋缁犳垿鍩㈤幘璇插嵆闁靛繆妾ч幏娲⒒娓氬洤浜為柛瀣洴閹崇喖顢涘☉娆愮彿闁诲海鏁哥涵鍫曞磻閹捐埖鍠嗛柛鏇ㄥ墰閿涙盯姊洪崨濠庢畷濠电偛锕幃浼搭敋閳ь剙鐣烽崼鏇ㄦ晢闁逞屽墰缁牓宕掑☉鏍︾盎闂佽澹嬮弲娑㈡倶椤忓牊鐓曢柣妯碱劜閼板潡鏌″畝鈧崰鏍€佸▎鎾澄╅柕澶涢檮閹瑩姊绘担绋胯埞婵炲樊鍙冨濠氭晲婢跺﹥顥濋梺鍓茬厛閸犳ぞ绨烘繝鐢靛Х椤ｄ粙宕滃┑鍥╃濠电姴鍋嗗鏍磽娴ｈ偂鎴炲垔閹绢喗鐓曟繝闈涙椤徰呯磽閸屾稒宕屾慨濠呮缁瑩宕稿Δ鈧鐗堢箾鐎涙鐭庢繛澶嬬洴閹箖鎮滅粵瀣櫖濠电姴锕ょ€氼剛澹曢鐐粹拺闂傚牊涓瑰☉銏犵＜婵犲﹤鎳忛崰鎺楁⒒閸屾艾鈧兘鎮為敂閿亾缁楁稑鎳忓畷鏌ユ煕瀹€鈧崐娑㈠炊閵娿儺鍤ら梺鍝勵槹閸ㄥ綊宕㈤柆宥嗏拺闁革富鍘奸崝瀣煕閵堝棔浜㈤柕鍥ㄥ姍楠炴帡骞樼€涙褰搁梻鍌欑婢瑰﹪宕戦崨顖氬灊闁规儼妫勭粻鑽ょ磽娴ｈ鐒介柛姗嗕邯濮婅櫣鍖栭弴鐐测拤濡炪們鍔岄幊妯侯嚕閹惰姤鏅插璺侯儑閸樻悂鎮楅崗澶婁壕闁诲函缍嗛崜娑溾叺濠德板€楁慨鐑藉磻閻愬灚鏆滈柨鐔哄Т缁犳牗绻涢崱妯诲鞍闁稿鍔戦弻娑⑩€﹂幋婵呯凹濡炪倖姊归幐鎼佸煘閹寸偛绠犻梺绋匡攻椤ㄥ棝骞堥妸鈺傚€婚柦妯侯槺閿涙盯姊虹紒妯哄闁圭⒈鍋嗙划鍫熷緞鐎ｎ剛顔曢梺绯曞墲钃遍悘蹇曟暩閳ь剝顫夐幐椋庢濮樿泛钃熸繛鎴欏灩閻掓椽鏌涢幇鈺佸婵絽鐗嗛埞鎴﹀灳閹绘帒鐝熼梺鍛婃⒐閸ㄧ敻锝炶箛鎾佹椽顢旈崟顓у晣闂備胶绮崝鏍矈閹绢喖纾婚柟鎹愵嚙閸愨偓濡炪倖鎸绘竟鏇㈠磻閹捐绀冩い鏃傚帶缁愭盯姊洪崫鍕垫Ч闁搞劌缍婂畷銏ゅ箻椤旇В鎷虹紒缁㈠幖閹冲繗銇愯濮婂宕熼銏╀純閻庤娲橀崹鍧楃嵁閸ヮ剚鍋嬮柛顐犲灩楠炲秶绱撻崒姘偓鐑芥倿閿曞倹鏅紓鍌欑劍椤ㄥ懘宕愰懡銈嗩潟闁圭儤顨呮儫闂侀潧饪电€靛苯螞閸曨垱鈷戦悗鍦濞兼劙鏌涢妸銉﹀仴妤犵偛鍟埢搴ㄥ箻閺夋垟鍋撶紒妯诲弿婵°倐鍋撻柣妤€绻樺畷銉ㄣ亹閹烘挴鎷绘繛杈剧到閹诧繝宕悙娣簻闁靛ǹ鍎查ˉ鐐电磼缂佹鈯曠紒缁樼箞瀹曟帡濡堕崨顕呭悪婵犵數濮烽弫鍛婃叏閻㈤潧鏋堢€广儱顦悿楣冩煙缂佹ê淇柣鏂挎閹綊宕崟鈺佷缓闂佽法鍠撴慨鎾偂濞戙垺鐓曟繛鎴濆船閺嬨倝鏌涘鍡曢偗婵☆偄鎳橀、鏇㈠閳ユ剚妲辨繝鐢靛仜瀵爼鎮ч弴銏″剭妞ゆ巻鍋撻柍瑙勫灴閹晠顢欓懖鈺€绱樻俊鐐€ら崑鍕囬鐐┾偓鏍ㄧ節閸ャ劌浠兼繝銏ｅ煐閸旀牠鍩涢幒妤佺厱閻忕偞宕樻竟姗€鏌嶈閸撴岸骞冮崒姘辨殾闁圭増婢樼粻鐟懊归敐鍛喐闁挎稒鐟╁楦裤亹閹烘搫绱电紓浣插亾濞达絽鎼ˉ姘辨喐閻楀牆绗氶柣鎾寸懇濮婂宕掑鐓庢濡炪倧璁ｆ俊鍥儉椤忓牆绠氱憸搴ｆ嫻閿熺姵鐓冪憸婊堝礈濞戙垹绠犻柟鎯х亪閸嬫挸顫濋悡搴＄睄閻庤娲栫壕顓熺珶閺囩姭鍋撻敐鍐ㄥ婵顨婂娲捶椤撶偛濡洪梺鎼炲妼缂嶅﹤鐣烽幇鐗堝€烽柣鎴灻禒顓炩攽閻樿宸ラ柛鐘宠壘铻ｉ柍褜鍓熼幃妤€鈻撻崹顔界亶闂佽鍠栭崐鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸閻忕偟纭堕崑鎾崇暦閸ャ劍鐣烽梻渚€鈧偛鑻晶瀵糕偓瑙勬磻閸楀啿顕ｆ禒瀣垫晣闁绘劗鏁搁弳顐︽煟閻斿摜鐭嬮柡灞诲姂閹箖鏌ㄧ€ｎ剟妾梺鍛婄☉閿曘倖绂嶅⿰鍫熲拺闁告稑锕︾粻鎾绘倵濮樼厧澧柍缁樻崌婵＄兘鍩￠崒妤佸闂備胶枪閺堫剟鎮疯钘濋柨鏇炲€归悡娆撴偣閸ュ洤鎳愰惁鍫ユ倵鐟欏嫭绀€婵犫偓闁秴绠熼柟缁㈠枛缁€瀣亜閹哄棗浜鹃悗娈垮枛濞尖€愁潖濞差亜鎹舵い鎾墲椤庢姊洪崫銉バｉ柣妤冨█楠炲啴鍨鹃幇浣瑰缓闂侀€炲苯澧寸€殿喖顭烽幃銏ゅ川婵犲嫮肖濠德板€х徊浠嬪疮椤栫儐鏁佺€广儱顦伴埛鎴犵磼鐎ｎ偒鍎ラ柛搴㈠姍閺屾盯寮埀顒勬偡閳哄懐宓侀柛鎰╁壆閺冨牆宸濇い鎾跺枎缁佽埖淇婇悙顏勨偓鏍箰妤ｅ啫纾规い鎰剁畱椤懘骞掗搹顐ゎ洸婵犲﹤鐗婇悡蹇擃熆閼哥數鈽夋い鈺婂墰缁辨帡鎮╅幇浣哄姺缂備浇椴哥敮妤€顭囪箛娑樜╅柨鏇楀亾闂傚绉瑰娲偡閺夋寧些闂佺娅曢敋闁伙絿鍏橀、鏇㈡晝閳ь剛绮堢€ｎ剛妫柣妤€鐓鍛洸闁告挆鈧崑鎾诲礂婢跺﹣澹曢梻浣告啞濞诧箓宕滃☉銏犲偍闂侇剙绉甸埛鎴︽⒒閸喕鍎愮憸鐗堝笒绾惧潡鏌熼幆鐗堫棄闁哄嫨鍎甸弻銊モ攽閸℃顦遍梺绋款儐閹告悂鍩㈤幘璇插瀭妞ゆ梻鏅禍顏呬繆閻愵亜鈧倝宕ｉ崘顔肩哗閺夊牄鍔庨埞宥呪攽閻樺弶鎼愮紒鐘垫嚀闇夐柨婵嗘噺閹叉悂鏌涘Ο缁樺€愭慨濠呮缁瑩骞愭惔銏″闂備焦妞块崣搴ㄥ窗濮樿埖鍤嶉弶鍫涘妿椤╃兘鎮楅敐搴′簮闁归绮换娑欐綇閸撗冨煂闂佺娅曢悷銊╁Φ閹版澘绠抽柟瀛樼箘瑜板淇婇悙顏勨偓鏍暜閹烘纾瑰┑鐘崇閸庢绻涢崱妯诲鞍闁绘挻鐟╁鍫曞醇閻斿嘲濮㈤梺浼欓檮缁捇寮婚埄鍐╁缂佸绨遍崑鎾诲锤濡も偓閽冪喖鏌曟繛鐐珕闁稿妫濋弻娑氫沪閸撗€妲堝銈呴獜閹凤拷
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





//闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆撴倻濡桨鐢绘繝鐢靛Т閿曘倝宕板顑╃喖鍩€椤掑嫭鍊垫繛鍫濈仢閺嬬喖鏌熼鐓庘偓鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸閻忕偟纭堕崑鎾崇暦閸ャ劍鐣烽梻渚€鈧偛鑻晶瀵糕偓瑙勬磻閸楀啿顕ｆ禒瀣垫晣闁绘劗鏁搁弳顐︽煟閻斿摜鐭嬮柡灞诲姂閹箖鏌ㄧ€ｎ剟妾梺鍛婄☉閿曘倖绂嶅⿰鍫熲拺闁告稑锕︾粻鎾绘倵濮樼厧澧柍缁樻崌婵＄兘鍩￠崒妤佸闂備胶枪閺堫剟鎮疯钘濋柨鏇炲€归悡娆撴偣閸ュ洤鎳愰惁鍫ユ倵鐟欏嫭绀€婵犫偓闁秴绠熼柟缁㈠枛缁€瀣亜閹哄棗浜鹃悗娈垮枛濞尖€愁潖濞差亜鎹舵い鎾墲椤庢姊洪崫銉バｉ柣妤冨█楠炲啴鍨鹃幇浣瑰缓闂侀€炲苯澧寸€殿喖顭烽幃銏ゅ川婵犲嫮肖濠德板€х徊浠嬪疮椤栫儐鏁佺€广儱顦伴埛鎴犵磼鐎ｎ偒鍎ラ柛搴㈠姍閺屾盯寮埀顒勬偡閳哄懐宓侀柛鎰╁壆閺冨牆宸濇い鎾跺枎缁佽埖淇婇悙顏勨偓鏍箰妤ｅ啫纾规い鎰剁畱椤懘骞掗搹顐ゎ洸婵犲﹤鐗婇悡蹇擃熆閼哥數鈽夋い鈺婂墰缁辨帡鎮╅幇浣哄姺缂備浇椴哥敮妤€顭囪箛娑樜╅柨鏇楀亾闂傚绉瑰娲偡閺夋寧些闂佺娅曢敋闁伙絿鍏橀、鏇㈡晝閳ь剛绮堢€ｎ剛妫柣妤€鐓鍛洸闁告挆鈧崑鎾诲礂婢跺﹣澹曢梻浣告啞濞诧箓宕滃☉銏犲偍闂侇剙绉甸埛鎴︽⒒閸喕鍎愮憸鐗堝笒绾惧潡鏌熼幆鐗堫棄闁哄嫨鍎甸弻銊モ攽閸℃顦遍梺绋款儐閹告悂鍩㈤幘璇插瀭妞ゆ梻鏅禍顏呬繆閻愵亜鈧倝宕㈡ィ鍐ㄧ婵☆垯璀﹂崵鏇熴亜閹板墎鐣辩紒鐘崇⊕閵囧嫰骞樼捄杞版埛缂備焦鍔楅崑鐐垫崲濠靛鍋ㄩ梻鍫熷垁閵忋倖鍊垫慨妯哄船閸樺鈧娲忛崕闈涚暦閵娧€鍋撳☉娅辨岸骞忕紒妯肩閺夊牆澧介幃濂告煙閾忣偅宕屾い銏¤壘楗即宕ㄩ婊愮床闂佽崵濮村ú锕併亹閸愵喖鐒垫い鎺嗗亾婵炵》绻濋幃浼搭敊閽樺绐炴繝鐢靛仧閸嬫捇锝炲澶嬧拺闂傚牊绋撶粻鍐测攽椤旀儳鍘存い銏＄懇楠炴帡寮崒婊愮闯闂備胶枪閺堫剟鎮疯瀹曟繂顓奸崱鎰盎濡炪倖鎸鹃崑鐐哄窗濡皷鍋撶憴鍕８闁搞劋绮欓悰顕€寮介妸锕€顎撻梺绋跨箰椤︽壆鈧艾銈稿缁樻媴閸涘﹤鏆堢紓浣割儐閸ㄥ潡寮崘顔芥櫜闁搞儺鐓堥崑銊╂⒒娓氬洤澧紒澶屾暬閹繝寮撮姀锛勫幗濠碘槅鍨甸褎鏅堕娑氱闁割偆鍠撻惌宀勬煃瑜滈崜婵嬶綖婢跺⊕鍝勵潨閳ь剙鐣峰┑鍡忔瀻闁规崘娉涚粊锕傛⒑閹肩偛鍔撮柛鎾村哺瀹曟垹鈧綆鍠楅悡鏇熴亜閹邦喖孝闁诲繆鏅滈〃銉╂倷閺夋垵顫掗梺鍝勭焿缁查箖骞嗛弮鍫濈伋闁告劘灏欢鐔兼⒒娴ｅ懙褰掝敄閸℃稑绠查柛銉墮缁犳牗淇婇妶鍛櫤闁哄懏鐓￠獮鏍垝閸忓浜鹃柟棰佺劍妤犲嫰姊婚崒娆愵樂缂侀硸鍠氱划濠氬箣閿旇棄浜遍梺瑙勫礃椤曆呯不閺嶃劋绻嗛柕鍫濇噹閺嗙偤鏌涢悩鍙夘棦闁哄本鐩鎾Ω閵夈儺娼炬俊鐐€х拹鐔煎礉閹达箑钃熼柨婵嗩槸椤懘鏌嶆潪鎷屽厡濞寸媭鍙冮弻锝夊閳轰胶浠梺鍝ュУ閻楃娀鐛崘顭戞建闁逞屽墴閻涱噣骞掗幊铏⒐閹峰懘宕崟顐ゎ啈闂傚倸鍊烽懗鍓佸垝椤栫偛绀夐柡鍥ュ焺閺佸銇勯幘鍗炵仼闁绘挻绻堥弻鐔煎礈瑜忕敮娑㈡煟閹惧啿鏆熼柟鑼归オ浼村醇濠靛牜妲舵繝鐢靛仜濡瑩骞愰幖浣哥＜闁靛ň鏅滈悡娑氣偓骞垮劚閸燁偅淇婇崸妤佺厽婵犻潧妫涢崺锝夋煛瀹€瀣瘈鐎规洖鐖兼俊鐑藉Ψ瑜岄惀顏堟⒒娴ｇ懓鈻曢柡鈧柆宥呭瀭闁秆勵殔閽冪喐绻涢幋鐐冩岸寮ㄦ禒瀣厱闁斥晛鍠氬▓銏犆圭亸鏍т壕闂傚倸鍊风粈渚€鎮块崶顒夋晪鐟滄棁妫熼梺姹囧灮鏋柦鍐枑缁绘盯骞嬮悙鑼姲闂佺ǹ顑嗛幑鍥极瀹ュ纭€闁绘劕寮跺▓銊︾節閵忥綆娼愭繛鑼枎椤繑绻濆顒傦紲濠电偛妫欑敮鎺楀储閿熺姵鐓涢悗锝呭悁缁ㄥジ鏌曢崶褍顏鐐差儏閳规垿宕堕懜纰夌礂濠德板€楁慨鐑藉磻閻愬搫绀夋繛鍡樻尵瀹撲線鏌″鍐ㄥ闁荤喓鎳撻埞鎴︽偐閹绘帩浼€闂佸磭顑曢崐鏍崲濠靛鍋ㄩ梻鍫熺▓閺嬪懎鈹戦悙鏉垮皟闁搞儴鍩栭弲鐐烘⒑閸︻厼顣兼繝銏★耿瀵憡绗熼埀顒勫蓟濞戙垹鐒洪柛鎰典簴婵洭姊虹紒妯诲鞍婵炶尙鍠栭獮鍐潨閳ь剟骞冨▎鎾崇闁瑰搫妫欑€垫牠姊绘担鑺ャ€冮柣鎺炵畵閹囨偐鐠囪尪鎽曢梺鍝勬川閸犳挾绮绘ィ鍐╃厽闁逛即娼ф晶顖炴煟濠靛洦绀嬫慨濠冩そ楠炲酣鎳為妷锔芥闂備焦鎮堕崝灞筋焽閳ユ剚鍤曢悹鍥ㄧゴ濡插牊淇婇鐐存暠闁哄倵鍋撻梻鍌欒兌绾爼宕滃┑瀣ㄢ偓鍐疀閺傛鍤ら梺绋挎湰閸戠懓銆掓繝姘厪闁割偅绻堥妤€霉濠婂啫鈷旂紒杈ㄥ浮閹晠鎼归銏紦闁诲孩顔栭崰娑㈩敋瑜旈崺銉﹀緞閹邦剦娼婇梺缁橆焽閺佺ǹ顭囨径鎰拻濞撴埃鍋撻柍褜鍓涢崑娑㈡嚐椤栨稒娅犻悗鐢电《閸嬫挾鎲撮崟顒傤槬缂傚倸绉撮敃銈夋偩妞嬪海鐭欓悹鍥╁亾濡啫鐣峰鈧俊姝岊槹闁稿锕濠氬磼濞嗘埈妲梺纭咁嚋缁绘繂鐣峰┑瀣櫇闁稿本姘ㄩ悰銉╂⒑閸濆嫮鈻夐柛娆忕箳缁辩偤寮介鐔哄幈濠电偞鍨堕…鍥箺閻樼粯鐓熼柟鎯х－缁犲鏌＄仦鍓ф创妞ゃ垺娲熼弫鎰板炊閳哄啯姣囬梻鍌欑窔濞艰崵寰婇挊澶涜€块弶鍫氭櫆椤洟鏌熼悜姗嗘當缂佺姵绋掗妵鍕箻鐠虹洅娑樷攽椤斿吋宸濈紒杈ㄦ尰閹峰懘鎼归悷鎵偧缂傚倷娴囬褔鎮ч幘鑽ゅ祦闁告劏鏅濋々鐑芥倵閿濆骸浜芥俊顐㈠暙閳规垿鎮欏▓鎸庮棑缂傚倸鐗呴崡鍐茬暦鐟欏嫮顩烽悗锝庝簽閻ｅ搫鈹戞幊閸婃劙宕戦幘娣簻妞ゆ劧绲跨粻鐐存叏婵犲懎鍚归柍褜鍓熷褔骞夐敓鐘茬柧闁稿繗鍋愮弧鈧紒鍓у鑿ら柛瀣崌瀹曟﹢鎳犻鍕礈闂傚倷绀侀幉鈥愁潖瑜版帗鍋￠柕澶嗘櫓閺佸鏌ㄥ┑鍡╂Ц缂佺姷绮妵鍕籍閸屾瀚涘┑鐐靛帶閿曨亜顫忓ú顏勭闁兼亽鍎插▓浼存⒑缁嬫鍎愰柟鐟版喘瀵鎮㈤崜鍙壭ч柟鑹版彧缁茬晫绮敍鍕＝濞达綀娅ｇ敮娑氱磼鐎ｎ偄绗掓い顓炴穿缁犳稑鈽夊Ο纰辨Ф闁荤喐绮岀粔鐟扮暦閸濆嫮鏆嗛柛鏇ㄥ厴閹疯櫣绱掔紒銏犲箹闁瑰啿姘﹂。璺ㄧ磽閸屾瑧璐伴柛鐘崇墪闇夐柣鎴ｆ缁€鍡涙煙閻戞﹩娈旂紒顐㈢Ч閺屾盯顢曢妶鍛€婚梺閫炲苯澧扮紒顕呭灡缁岃鲸绻濋崶鑸垫櫖闂佺粯鍔曢悺銊╂偟瀹曞洨纾藉ù锝夋涧婵″吋銇勯鐐叉Щ妞ゎ偄绻愮叅妞ゅ繐鎷嬪Λ鍐ㄢ攽閻愭潙鐏卞瀛樻倐椤㈡捇宕卞☉娆屾嫽闂佺ǹ鏈懝楣冾敂閳哄懏鍊垫慨妯煎帶婢ф挳鏌熼钘夊姢闁伙綇绻濋獮宥夘敊閼恒儳鏆﹂梻鍌欑窔閳ь剛鍋涢懟顖涙櫠閹绢喗鐓曢柍瑙勫劤娴滅偓淇婇悙顏勨偓鏍暜閹烘绐楁慨姗嗗墻閻掍粙鏌熼柇锕€骞樼紒鐘荤畺閺屾稑鈻庤箛锝嗩€嗛梺鍏兼緲濞硷繝寮婚弴銏犲耿婵°倐鍋撻柡鍡悼閳ь剝顫夊ú蹇涘礉瀹ュ拋鐒介煫鍥ㄧ☉缁€鍫㈡喐韫囨稑鐓熼柕鍫濇缁♀偓闂佹眹鍨藉褍鐡梻浣瑰濞插繘宕愬┑瀣畺鐟滄柨鐣烽崡鐐╂婵炲棗绻嗛崑鎾诲锤濡や胶鍘卞銈庡幗閸ㄥ灚绂嶉弽顬＄懓饪伴崟顓犵厜闂佸搫鐬奸崰鏍х暦椤愶箑绀嬫い鎺戭槹椤ワ絽鈹戦悙鑼憼缂侇喖绉瑰畷鏇㈠箮鐟欙絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯鍩勯崘鍓у姺闂佸疇顫夌粙鎾舵閹捐纾兼慨妯荤樂閿涘嫮纾奸柤鎼佹涧閸濆搫鈹戦垾宕囨憼缂佹鍠栭崺鈧い鎺嗗亾妞ゆ洩缍侀獮姗€顢欑喊杈ㄧ秱闂備焦鏋奸弲娑㈠疮椤栫偞鍋熼柡宥庡幗閳锋帒銆掑锝呬壕闂侀€炲苯澧伴柛瀣洴閹崇喖顢涘☉娆愮彿闂佽顔栭崰姘卞閸忕浜滈柡鍌氱仢閳锋棃鎮介娑氥€掔紒杈ㄥ笧缁辨帒螣閼测晝鏆ゆ俊鐐€ら崑鍛崲閸儯鈧礁顫滈埀顒勫箖濞嗘挸绾ч柛顭戝櫘閸ゆ瑩姊婚崒姘偓鎼佸磹閻戣姤鍊块柨鏇炲€哥粻鏍煕椤愶絾绀€缁剧偓瀵х换婵囩節閸屾粌顣虹紓浣插亾闁告劏鏂傛禍婊堢叓閸ャ劍灏靛褋鍨婚埀顒冾潐濞叉鍒掑畝鍕厴闁硅揪闄勯弲顒勬煕閺囥劌浜楅柡澶樺弮濮婅櫣绮欓崠鈩冩暰闂佹悶鍔岄悘婵嬫偩閻戣棄鍗抽柕蹇娾偓铏吇婵＄偑鍊栫敮濠勭矆娴ｈ鍎熷┑鐘叉处閳锋垿鏌涢幘鏉戠祷濞存粍绻勭槐鎺旀嫚閼碱儷銏°亜椤撶偞鍠樼€殿喕绮欐俊姝岊槾妞ゆ梹娲熼幃妤呮偡閺夋妫岄梺鍝ュУ濞叉粓鎳為柆宥呯缂佹妗ㄧ花濠氭⒑闂堟侗妲堕柛搴ゆ珪閺呰泛鈽夊杈╋紲缂傚倷鐒﹂…鍥虹€涙﹩娈介柣鎰▕閸庢棃鏌熼鐣屾噮闁圭懓瀚粭鐔碱敍濞戣鲸锛堝┑鐘殿暜缁辨洟宕戦幋锕€纾圭憸蹇擃嚗婵犲啰顩烽悗锝庝簽椤旀劕顪冮妶鍡楀Ё缂佺姵鍨块幃鈥愁潨閳ь剟寮婚悢鍛婄秶闁告挆鍛闂備胶绮幖顐ゆ崲濠靛钃熸繛鎴欏灩閻掓椽鏌涢幇鍓佺窗婵炲矈浜炵槐鎾存媴闂堟稑顬嬮梺鍛婎焼閸ワ絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯骞囬埡浣割瀳濡炪値鍓欓悧鎾愁潖濞差亜绠伴幖娣灩椤︹晜绻涚€涙鐭ゅù婊勭箖缁岃鲸绻濋崟銊ヤ壕婵炴垶鐟悞浠嬫煟閻旈绉洪柡灞界Х椤т線鏌涢幘瀵告噰妞ゃ垺宀搁弫鎰緞濡粯娅旈梻渚€鈧偛鑻晶顕€鏌ｉ敐鍥у幋闁诡喒鍓濋幆鏂课熺紒妯绘緫闂備浇顕ч崙鐣岀礊閸℃顩叉繝闈涱儏濮瑰弶銇勮箛鎾跺闁抽攱甯￠弻娑氫沪閸撗勫櫘濡炪倧璁ｇ粻鎾诲蓟閻斿搫鏋堥柛妤冨仒缁ㄥ鏌ｉ幘鍗炩偓婵嬪蓟閺囩喓绠鹃柣鎰靛墯閻濇棃姊哄Ч鍥р偓銈夊闯閿濆钃熸繛鎴炵矤濡茬厧顪冮妶鍐ㄥ闁硅櫕鍔楅崚鎺撶節濮樺吋鏅┑鐐村灦濮樸劑顢欓弴銏″€甸柣鐔告緲椤ュ繘鏌涢悩铏闁奸缚椴哥缓浠嬪川婵犲嫬骞楅梻浣筋潐瀹曟ê鈻斿☉銏犲嚑婵炴垯鍨洪悡娑氣偓鍏夊亾閻庯綆鍓欓崺宀勬煣娴兼瑧绉柡灞剧☉閳规垿宕卞Δ濠佹偅缂傚倷鐒﹂〃蹇涘矗閸愵煈娼栨繛宸簼閸嬪倿骞栫划瑙勵潑婵☆偄鏈换娑氣偓娑欘焽閻倕霉濠婂簼绨绘い鏇悼閹风姴霉鐎ｎ偒娼旈梻渚€娼х换鎺撴叏閻㈡潌澶娾攽鐎ｎ偆鍘介梺缁樏崢鏍嚋椤忓牊鐓曢柡鍌氭健閸欏嫮鈧娲樼换鍫ャ€佸☉銏″€烽柡澶嬪灍閸嬫捇宕归銈囶啎闂佸壊鍋呯换鍕閵忋倖鐓涢悗锝庡墮閺嬫盯鏌″畝鈧崰鏍€佸▎鎾村亗閹煎瓨锚娴滈箖鏌涜椤ㄥ棝宕戝Ο姹囦簻闁哄啫鐗婇弳婊堟煕鐎ｎ偅宕岄柡浣瑰姈閹棃鍩勯崘顏冩喚闂傚倷绀侀幖顐﹀箠韫囨洖鍨濋柟鎹愵嚙閽冪喖鏌ㄥ┑鍡╂Ч闁哄懏鐓￠弻娑樷槈濞嗘劗鍑″銈呮禋閸樿壈鐏冮梺缁橈耿濞佳勭閿曞倹鐓曢柡鍐ｅ亾闁荤喆鍎甸敐鐐剁疀濞戞瑦鍎柣鐔哥懃鐎氼剟鎮垫导瀛樷拺闁革富鍘剧敮娑㈡偨椤栨稑绗掔悮娆撴煙闁箑鏋ょ痪鍙ョ矙閺屾稓浠﹂悙顒傛閻庢稒绻傞埞鎴﹀煡閸℃ぞ绨奸梺鑽ゅ暀閸涱厼鐏婇柣搴秵娴滃爼宕ョ€ｎ亶鐔嗛悹铏瑰皑闊剟鏌涢悙鑼煟婵﹥妞藉畷顐﹀礋椤掆偓缁愭盯姊洪崫銉バ㈤悗娑掓櫇缁顓奸崨鍌涙瀹曘劑顢欑憴鍕伖缂傚倸鍊风粈渚€顢栭崼婵冩灃闁哄洨濮锋稉宥夋煙閹澘袚闁绘挸鍟伴埀顒傛嚀鐎氼厽绔熼崱妯绘珷闁哄洢鍨洪悡鍐磽娴ｈ偂鎴犱焊閹殿喚纾奸柛灞剧☉缁椦囨煃瑜滈崜銊х礊閸℃稑纾婚柛鈩冪☉閸屻劌霉閻樺樊鍎愰柍閿嬪灩閹叉悂鎮ч崼婵堢懆婵炲瓨绮堥崡鎶藉蓟濞戙垹惟闁靛牆鎳庣粊顕€姊虹拠鈥崇仩闁哥喐鎸抽獮鏍亹閹烘垶宓嶅銈嗘尵婵妲愰弻銉︹拻濞达綀娅ｇ敮娑㈡煥濮橆厺绻嗘い鏍ㄧ啲闊剟鏌熼鐟板⒉缂佽桨绮欏畷銊︾節閸曨偄绠伴梺璇查閸樻粓宕戦幘缁樼厓鐟滄粓宕滈悢椋庢殾濞村吋娼欓崘鈧銈嗘尰婢规洟宕戦幘瀛樺缂侇垱娲橀悗濠氭⒑閸︻厼浜炬繛鍏肩懄缁傛帗绺介崨濠傗偓鐢告煕韫囨搩妲稿ù婊堢畺濮婃椽鏌呴悙鑼跺濠⒀勬尦閺岀喖顢欓悷鏉库拤閻庡灚婢樼€氫即鐛崶顒夋晣婵炴垶鐟ラ褰掓⒒閸屾瑦绁扮€规洜鏁诲畷浼村箛椤撶姷褰鹃梺绯曞墲缁嬫帡寮查鍌楀亾楠炲灝鍔氭い锔垮嵆閹繝寮撮姀鈥斥偓鐢告煥濠靛棗鏆欏┑鈥炽偢閺屽秷顧侀柛鎾村哺閹兾旈崘顏嗙厰闁哄鐗勯崝搴ｅ姬閳ь剟姊洪幖鐐插姌闁告柨鐭傞崺鈧い鎺嗗亾妞ゎ厾鍏樺濠氬Χ婢跺﹦鐣抽梺鍦劋閸ㄥ灚鎱ㄩ弴鐐╂斀闁绘劕寮堕崳瑙勪繆椤愶絿銆掓俊鍙夊姍楠炴鈧稒锚椤庢捇姊洪棃鈺佺槣闁告枪鍗遍柛顐ｇ箥濞撳鏌曢崼婵囶棞闁诲繈鍎甸幃妤€顫濋銏犵ギ闂佺粯渚楅崰姘跺焵椤掑﹦绉甸柛鐘愁殜瀹曟劖绻濆顓犲幘闂佽鍘界敮鎺楀礉濡ゅ懏鐓欑€瑰嫰鍋婇悡鍏兼叏婵犲懏顏犵紒顔界懇楠炴劖鎯旈姀鈥愁伆闂傚倷鑳堕崢褏绱炴繝鍕笉闁哄稁鍘奸弰銉╂煃瑜滈崜姘跺Φ閸曨垰绠抽柟瀛樼箥娴煎苯鈹戦埥鍡椾簼闁搞劌婀卞Σ鎰板箳濡ゅ﹥鏅╅梺鍏肩ゴ閺呮盯路閳ь剛绱撻崒娆戝妽妞ゃ劌鐗撳畷浼村冀椤撶偞妲梺閫炲苯澧柕鍥у楠炴帡骞嬪┑鎰磻闁诲氦顫夐幐椋庢濮樿泛钃熼柍銉﹀墯閸氬骞栫划鍏夊亾閸愬樊鍔€闂傚倷娴囧銊х矆娓氣偓瀹曨垶骞橀鑹版憰闂侀潧艌閺呮盯宕￠搹顐＄箚闁靛牆鍊告禍鐐箾鐎涙鐭婂褏鏅Σ鎰板箳濡や礁鈧攱銇勯幒鍡椾壕闂佸憡鏌ｉ崐妤冩閹炬剚鍚嬮柛婊冨暢閸氼偊鎮楀▓鍨灕妞ゆ泦鍥х叀濠㈣埖鍔曢～鍛存煃閸濆嫬鈧懓鈻嶉崶顒佲拻濞达絿鎳撻婊呯磼鐠囨彃鈧瓕鐏嬪┑鐐村灍閹崇偤宕堕鈧敮閻熸粌绻掓竟鏇熺附閸涘﹦鍘介梺閫涘嵆濞佳勬櫠椤曗偓閺屾盯寮拠娴嬪亾濡ゅ啯顫曢柟鐐墯閸氬鏌涘⿰鍐ㄦ殺闁告凹鍋婇幃妤€鈻撻崹顔界亪濡炪値鍘鹃崗妯虹暦鐟欏嫨鍋呴柛鎰╁妿閻も偓濠电偠鎻徊浠嬪箟閿熺姴纾规い鏍仦閳锋垹鐥鐐村櫣濞存粌缍婇幃璺衡槈閺嵮冨Е闂佺硶鏂侀崑鎾愁渻閵堝棗绗掗柛鐕佸亰閹啫煤椤忓懐鍘介梺鍝勭▉閸樻椽骞夐悙顒佸弿濠电姴瀚敮娑㈡煙瀹勭増鍤囩€规洏鍔戝Λ鍐ㄢ槈濮樻瘷銊ヮ渻閵堝啫鐏柣妤冨Т閻ｇ兘宕￠悙鈺傤潔濠碘槅鍨抽埛鍫ュ船閸洘鈷掑ù锝呮憸缁夌儤淇婇銉︾《婵炲棎鍨芥俊鍫曞炊閳哄喚妲搁梻浣告惈缁夋煡宕濇惔锝呭К闁逞屽墴濮婂宕掑鍗烆杸婵炴挻纰嶉〃濠傜暦閺囩偐妲堥柕蹇ョ磿閸樺憡绻涙潏鍓у埌闁硅绻濊棢闁靛繈鍊栭悡娑㈡倶閻愭彃鈷旈柕鍡樺笒闇夐柣娆忔噽閻ｇ數鈧娲樼划蹇浰囬幎鑺ョ厸闁告侗鍘归崑銏℃叏婵犲啯銇濇俊顐㈠暙闇夐柕澶堝劤婢э妇鈧鍠楄ぐ鍐偑娴兼潙閱囨繝闈涚墕楠炲牓姊绘担渚敯闁规椿浜浠嬪礋椤栨稑浜楅梺鍝勬川閸犳挾寮ч埀顒勬⒒閸屾氨澧涚紒瀣浮钘熸繝濠傚娴滄粓鐓崶椋庡埌濠⒀屽灦閺屾洟宕惰椤忣厽銇勯姀鈩冪妞ゃ垺顨婂畷鎺戔堪閸曨剦鍟岄梻鍌氬€风粈渚€骞栭锕€瀚夋い鎺戝閸庡孩銇勯弽銊ュ毈闁搞倖娲橀妵鍕即濡も偓娴滈箖姊虹化鏇熸澒闁稿鎸搁—鍐Χ閸℃鐟ㄩ柣搴㈠嚬閸撶喎鐣疯ぐ鎺戠＜闁绘劕顕崢閬嶆偡濠婂啴鍙勯柕鍡楀暣瀹曠厧鈹戦崼鐔割啌濠电偞鎸婚崺鍐磻閹剧粯鐓涢悘鐐插⒔濞插瓨顨ラ悙鍙夊枠婵☆偄鍟埥澶娾枎閹存粓鍋楁繝纰夌磿閸嬫垿宕愰妶澶婄；闁告洦鍨扮粻鐘虫叏濡炶浜鹃悗娈垮枛椤嘲顕ｉ幘顔碱潊闁绘ǹ顕ч弫瑙勭節閻㈤潧鈻堟繛浣冲厾娲Χ閸涱亝鐎洪梺鎸庣箓濞层劎澹曢挊澹濆綊鏁愰崶銊ユ畬婵犳鍠栭悧蹇曟閹烘柡鍋撻敐搴′簻缂佹う鍥ㄧ厵妤犵偛鐏濋悘鈺呮煃鐟欏嫬鐏╅柍褜鍓ㄧ紞鍡涘磻閸曨剛顩锋繛宸簼閳锋帡鏌涚仦鐐殤濠⒀勭洴閺屾盯骞掗崱妞惧闂傚倷绶氬褏鎹㈤崼銉ョ９闁哄稁鍘肩壕褰掓煕椤垵浜濋柛娆忕箲閹便劌顪冪拠韫婵犵妲呴崑澶娾枖閺囥垺鍤嶉梺顒€绉甸崵宥夋煏婢跺牆鈧绮诲鑸碘拺闁告稑锕﹂幊鍐┿亜閿旇鐏￠柟渚垮妼椤撳ジ宕堕敐鍛濠电偛鐗嗛悘婵嬪几閻斿吋鐓欐慨婵嗘湰閻濐亪鏌熸笟鍨闁糕斁鍋撳銈嗗笒鐎氼參鎮￠崘顔解拺婵炲棙鍎抽悘鐘裁瑰⿰鍫㈢暫婵﹪缂氶妵鎰板箳濠靛浂妫栫紓鍌欑贰閸ｎ噣宕归幎钘夌闁靛繒濮Σ鍫熺箾閸℃ê濮夌紒澶婄埣濮婃椽宕ㄦ繝鍐ㄧ樂闂佸憡鍔戦崝搴ㄥ储閹烘鈷掗柛灞剧懆閸忓本銇勯姀鐙呮敾闁逛究鍔戞俊鍫曞炊瑜嶉悘濠囨煙閼圭増褰х紒韫矙瀹曠懓鈹戠€ｎ偆鍘搁梺鍛婂姂閸斿孩鏅跺☉銏＄厱濠电姴瀚敮娑樓庨崶褝韬い銏＄洴閹瑧鈧稒顭囪ぐ鍝ョ磽閸屾瑩妾烽柛鏂跨焸閳ワ箑鐣￠柇锔界稁濠电偛妯婃禍婵嬎夐崼鐔虹闁硅揪缍侀崫鐑樸亜鎼粹剝顥㈡慨濠傤煼瀹曟帒顫濋澶夋偅闂佽瀛╅崙褰掑礈閻旂厧绠栭柟顖嗗懏娈濋梺瑙勵問閸犳鈻撻弴銏♀拺闂侇偆鍋涢懟顖涙櫠閹绢喗鐓曢柍瑙勫劤娴滅偓淇婇悙顏勨偓鏍暜閹烘绐楁慨姗嗗墻閻掍粙鏌熼柇锕€骞樼紒鐘荤畺閺屾稑鈻庤箛锝嗩€嗛梺鍏兼緲濞硷繝寮婚弴銏犲耿婵°倐鍋撻柡鍡悼閳ь剝顫夊ú蹇涘礉瀹ュ拋鐒介煫鍥ㄧ☉缁€鍫㈡喐韫囨稑鐓熼柕鍫濇缁♀偓闂佹眹鍨藉褍鐡梻浣瑰濞插繘宕愬┑瀣畺鐟滄柨鐣烽崡鐐╂婵炲棗绻嗛崑鎾诲锤濡や胶鍘卞銈庡幗閸ㄥ灚绂嶉弽顬＄懓饪伴崟顓犵厜闂佸搫鐬奸崰鏍х暦椤愶箑绀嬫い鎺戭槹椤ワ絽鈹戦悙鑼憼缂侇喖绉瑰畷鏇㈠箮鐟欙絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯鍩勯崘鍓у姺闂佸疇顫夌粙鎾舵閹捐纾兼慨妯荤樂閿涘嫮纾奸柤鎼佹涧閸濆搫鈹戦垾宕囨憼缂佹鍠栭崺鈧い鎺嗗亾妞ゆ洩缍侀獮姗€顢欑喊杈ㄧ秱闂備焦鏋奸弲娑㈠疮椤栫偞鍋熼柡宥庡幗閳锋帒銆掑锝呬壕闂侀€炲苯澧伴柛瀣洴閹崇喖顢涘☉娆愮彿闂佽顔栭崰姘卞閸忕浜滈柡鍌氱仢閳锋棃鎮介娑氥€掔紒杈ㄥ笧缁辨帒螣閼测晝鏆ゆ俊鐐€ら崑鍛崲閸儯鈧礁顫滈埀顒勫箖濞嗘挸绾ч柛顭戝櫘閸ゆ瑩姊婚崒姘偓鎼佸磹閻戣姤鍊块柨鏇炲€哥粻鏍煕椤愶絾绀€缁剧偓瀵х换婵囩節閸屾粌顣虹紓浣插亾闁告劏鏂傛禍婊堢叓閸ャ劍灏靛褋鍨婚埀顒冾潐濞叉鍒掑畝鍕厴闁硅揪闄勯弲顒勬煕閺囥劌浜楅柡澶樺弮濮婅櫣绮欓崠鈩冩暰闂佹悶鍔岄悘婵嬫偩閻戣棄鍗抽柕蹇娾偓铏吇婵＄偑鍊栫敮濠勭矆娴ｈ鍎熷┑鐘叉处閳锋垿鏌涢幘鏉戠祷濞存粍绻勭槐鎺旀嫚閼碱儷銏°亜椤撶偞鍠樼€殿喕绮欐俊姝岊槾妞ゆ梹娲熼幃妤呮偡閺夋妫岄梺鍝ュУ濞叉粓鎳為柆宥呯缂佹妗ㄧ花濠氭⒑闂堟侗妲堕柛搴ゆ珪閺呰泛鈽夊杈╋紲缂傚倷鐒﹂…鍥虹€涙﹩娈介柣鎰▕閸庢棃鏌熼鐣屾噮闁圭懓瀚粭鐔碱敍濞戣鲸锛堝┑鐘殿暜缁辨洟宕戦幋锕€纾圭憸蹇擃嚗婵犲啰顩烽悗锝庝簽椤旀劕顪冮妶鍡楀Ё缂佺姵鍨块幃鈥愁潨閳ь剟寮婚悢鍛婄秶闁告挆鍛闂備胶绮幖顐ゆ崲濠靛钃熸繛鎴欏灩閻掓椽鏌涢幇鍓佺窗婵炲矈浜炵槐鎾存媴闂堟稑顬嬮梺鍛婎焼閸ワ絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯骞囬埡浣割瀳濡炪値鍓欓悧鎾愁潖濞差亜绠伴幖娣灩椤︹晜绻涚€涙鐭ゅù婊勭箖缁岃鲸绻濋崟銊ヤ壕婵炴垶鐟悞浠嬫煟閻旈绉洪柡灞界Х椤т線鏌涢幘瀵告噰妞ゃ垺宀搁弫鎰緞濡粯娅旈梻渚€鈧偛鑻晶顕€鏌ｉ敐鍥у幋闁诡喒鍓濋幆鏂课熺紒妯绘緫闂備浇顕ч崙鐣岀礊閸℃顩叉繝闈涱儏濮瑰弶銇勮箛鎾跺闁抽攱甯￠弻娑氫沪閸撗勫櫘濡炪倧璁ｇ粻鎾诲蓟閻斿搫鏋堥柛妤冨仒缁ㄥ鏌ｉ幘鍗炩偓婵嬪蓟閺囩喓绠鹃柣鎰靛墯閻濇棃姊哄Ч鍥р偓銈夊闯閿濆钃熸繛鎴炵矤濡茬厧顪冮妶鍐ㄥ闁硅櫕鍔楅崚鎺撶節濮樺吋鏅┑鐐村灦濮樸劑顢欓弴銏″€甸柣鐔告緲椤ュ繘鏌涢悩铏闁奸缚椴哥缓浠嬪川婵犲嫬骞楅梻浣筋潐瀹曟ê鈻斿☉銏犲嚑婵炴垯鍨洪悡娑氣偓鍏夊亾閻庯綆鍓欓崺宀勬煣娴兼瑧绉柡灞剧☉閳规垿宕卞Δ濠佹偅缂傚倷鐒﹂〃蹇涘矗閸愵煈娼栨繛宸簼閸嬪倿骞栫划瑙勵潑婵☆偄鏈换娑氣偓娑欘焽閻倕霉濠婂簼绨绘い鏇悼閹风姴霉鐎ｎ偒娼旈梻渚€娼х换鎺撴叏閻㈡潌澶娾攽鐎ｎ偆鍘介梺缁樏崢鏍嚋椤忓牊鐓曢柡鍌氭健閸欏嫮鈧娲樼换鍫ャ€佸☉銏″€烽柡澶嬪灍閸嬫捇宕归銈囶啎闂佸壊鍋呯换鍕閵忋倖鐓涢悗锝庡墮閺嬫盯鏌″畝鈧崰鏍€佸▎鎾村亗閹煎瓨锚娴滈箖鏌涜椤ㄥ棝宕戝Ο姹囦簻闁哄啫鐗婇弳婊堟煕鐎ｎ偅宕岄柡浣瑰姈閹棃鍩勯崘顏冩喚闂傚倷绀侀幖顐﹀箠韫囨洖鍨濋柟鎹愵嚙閽冪喖鏌ㄥ┑鍡╂Ч闁哄懏鐓￠弻娑樷槈濞嗘劗鍑″銈呮禋閸樿壈鐏冮梺缁橈耿濞佳勭閿曞倹鐓曢柡鍐ｅ亾闁荤喆鍎甸敐鐐剁疀濞戞瑦鍎柣鐔哥懃鐎氼剟鎮垫导瀛樷拺闁革富鍘剧敮娑㈡偨椤栨稑绗掔悮娆撴煙闁箑鏋ょ痪鍙ョ矙閺屾稓浠﹂悙顒傛閻庢稒绻傞埞鎴﹀煡閸℃ぞ绨奸梺鑽ゅ暀閸涱厼鐏婇柣搴秵娴滃爼宕ョ€ｎ亶鐔嗛悹铏瑰皑闊剟鏌涢悙鑼煟婵﹥妞藉畷顐﹀礋椤掆偓缁愭盯姊洪崫銉バ㈤悗娑掓櫇缁顓奸崨鍌涙瀹曘劑顢欑憴鍕伖缂傚倸鍊风粈渚€顢栭崼婵冩灃闁哄洨濮锋稉宥夋煙閹澘袚闁绘挸鍟伴埀顒傛嚀鐎氼厽绔熼崱妯绘珷闁哄洢鍨洪悡鍐磽娴ｈ偂鎴犱焊閹殿喚纾奸柛灞剧☉缁椦囨煃瑜滈崜銊х礊閸℃稑纾婚柛鈩冪☉閸屻劌霉閻樺樊鍎愰柍閿嬪灩閹叉悂鎮ч崼婵堢懆婵炲瓨绮堥崡鎶藉蓟濞戙垹惟闁靛牆鎳庣粊顕€姊虹拠鈥崇仩闁哥喐鎸抽獮鏍亹閹烘垶宓嶅銈嗘尵婵妲愰弻銉︹拻濞达綀娅ｇ敮娑㈡煥濮橆厺绻嗘い鏍ㄧ啲闊剟鏌熼鐟板⒉缂佽桨绮欏畷銊︾節閸曨偄绠伴梻浣筋嚙缁绘帡宕戦悢鍝ヤ粴濠电姭鎷冮崟顓炲绩闂佸搫鐬奸崰搴ㄦ偩閿熺姵鍋嬮柛顐ｇ箖椤忋倝姊绘担鐑樺殌鐎殿喖鐖奸獮鎰板礃閼碱剚娈鹃梺缁樻⒒閳峰牓寮崘顔界厪闁割偅绻勭粻鍐差熆鐠哄搫顏慨濠傤煼瀹曟帒顫濋钘変壕鐎瑰嫭鍣磋ぐ鎺戠倞鐟滄粌霉閺嶎厽鐓忓┑鐐靛亾濞呭棝鏌涙繝鍌涘仴闁哄被鍔戝鎾倷濞村浜鹃柛婵勫劤娑撳秹鏌＄仦璇插姕闁绘挻娲熼弻鏇熷緞濡儤鐏堟繝鈷€灞芥珝闁哄矉绱曢埀顒婄岛閺呮繄绮ｉ弮鍫熺厸鐎光偓閳ь剟宕伴弽褏鏆︽繝濠傛－濡查箖鏌ｉ姀鈺佺仭闁烩晩鍨跺濠氭晸閻樻彃绐涘銈嗘濡嫰鍩€椤掍礁濮嶉柡宀嬬磿娴狅妇鎷犻幓鎺濇綆闂備浇顕栭崰鎾诲磹濠靛棛鏆﹂柟鐑樺灍濡插牊鎱ㄥΔ鈧Λ鏃傛閿燂拷
/*闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鏁愭径濠勵吅闂佹寧绻傞幉娑㈠箻缂佹鍘遍梺闈涚墕閹冲酣顢旈銏＄厸閻忕偛澧藉ú瀛樸亜閵忊剝绀嬮柡浣瑰姍瀹曞崬鈻庡Ο鎭嶆氨绱撻崒姘偓鐑芥嚄閼稿灚鍙忛梺鍨儑缁犻箖鏌嶈閸撶喖寮婚垾宕囨殕闁逞屽墴瀹曚即寮借閺嗭附绻濇繝鍌涳紞婵℃煡绠栭弻锝夊閳轰胶浠梺鐑╂櫓閸ㄨ泛顕ｇ拠娴嬫婵﹫绲芥禍楣冩煥濠靛棗鏆欏┑鈥炽偢閺屽秷顧侀柛鎾存皑閹广垽宕煎┑鎰婵犵數濮甸懝楣冨础閹惰姤鐓熼柡鍐ㄦ处椤忕姵銇勯弮鈧ú鐔奉潖閾忓湱纾兼俊顖氭惈琚濋梻浣告啞閹歌鐣濋幖浣哥畺闁汇垻枪閸楁娊鏌曡箛濠冾€嗛柟閿嬫そ濮婃椽宕ㄦ繝鍕暤闁诲孩鍑归崣鍐ㄧ暦閹惰姤鎯為柛锔诲幘閿涙粓鏌ｉ悩鍙夌┛閻忓繑鐟﹂弲鍫曟晜闁款垰浜炬繛鍫濈仢閺嬫稒銇勯鐐叉Щ妞ゆ洩缍侀獮姗€顢欓懞銉︾彨闂備礁鎲″ú锕傚储閽樺鐔嗛柟杈鹃檮閳锋垿鎮归崶銊ョ祷闁搞倛浜槐鎾愁吋閸涱噮妫＄紓浣稿€哥粔鐟邦嚕娴犲鏁囬柣鎰暜缁遍亶姊绘担绛嬫綈鐎规洘锕㈤、姘愁樄鐎规洩绻濋獮搴ㄦ嚍閵壯冨妇闂傚⿴鍋勫ú锕€煤閺嵮呮懃闂傚倷鐒﹂幃鍫曞磹閺嶎厼鍨傞柛鎾茬劍瀹曞弶绻涢幋娆忕仼缂佺姴顭烽弻鈩冨緞鐎ｎ亞浠奸梺鍛婃煣閸楀啿顫忛搹鍦煓婵炲棙鍎抽崜閬嶆⒑閸︻厽鍤€婵炲樊鍘奸锝嗙節濮橆剛浼嬮悗瑙勬礀濞诧箑鈻撻弴銏♀拺闂侇偆鍋涢懟顖涙櫠鐎电硶鍋撶憴鍕；闁告鍟块锝嗙鐎ｅ灚鏅ｉ梺缁樓圭亸娆撳Χ閿曞倹鈷掑ù锝勮閺€浼存煙濞茶绨界紒顔碱煼楠炲鎮╅崗鍝ョ憹婵犵數鍋為崹顖炲垂濞差亜鐓曢柟鐑橆殕閻撴洘銇勯幇鈺佲偓鏇㈠几閺冨牊鐓冩い鏍ㄧ⊕缁€鍐煏閸パ冾伃鐎殿噮鍓熷畷褰掝敋閸涱剛妫紓鍌氬€烽懗鍓佸垝椤栨粍宕查柛鏇ㄥ灠缁愭鈹戦悩韫抗闁绘棁顔栭搹鍦＜婵☆垵鍋愭晥闂傚倸鍊搁崐鐑芥嚄閼哥數浠氭俊鐐€栭崹鐢稿箠濮椻偓閻涱喗绻濋崒銈嗗缓闂侀€炲苯澧い鏇秮椤㈡洟鏁冮埀顒傜矆閸愨斂浜滈柡鍐ㄥ€甸幏鈩冪箾閻撳函韬慨濠冩そ濡啫鈽夊顒夋毇闂備胶顢婂▍鏇㈠箲閸ヮ剛宓佹俊銈呮噹閸楁娊鏌曡箛濠冾€嗛柟閿嬫そ濮婃椽宕ㄦ繝鍕暤闁诲孩鍑归崣鍐ㄧ暦閹烘惟闁冲搫鍊婚崢閬嶆⒑闂堟胆褰掑磿瀹曞洨鐭撴繛宸簼閻撴盯鎮橀悙鐢垫憘闁稿孩姊婚埀顒冾潐濞叉牠鎮ラ崗闂寸箚婵繂鐭堝Σ楣冩⒑娴兼瑧鐣遍柣妤佹尭椤繑绻濆顒傦紲濠殿喗锚瀹曨剟路閳ь剛绱撻崒娆戭槮妞ゆ垵妫涢弫顕€鎮欓棃娑樼亰婵犵數濮村ú銈夋偂濞戙垺鐓曢悘鐐插⒔閹冲懐绱掗悩瀹犲妞ゎ亜鍟存俊鍫曞幢濡も偓椤洭姊虹粙鍖℃敾婵炲弶顭囩划瀣箳閹存柨鐗氶梺鍛婃磵閺呮瑧鑺辩拠宸富闁靛牆鎳愮粻浼存倵濮樼厧澧寸€殿喗濞婇弫鎰板川椤忓懏鏉搁梻浣哄仺閸庤京澹曢銏犳槬闁挎繂娲犻崑鎾舵喆閸曨剛锛橀梺鍛婃⒐閸ㄥ潡濡存担绯曟婵☆垰鍢查悧姘舵煙閼测晞藟闁告挻绻堝鎶筋敂閸啿鎷婚梺绋挎湰閼归箖鍩€椤掑倸鍘撮柟铏殜瀹曟粍鎷呯粙璺ㄤ喊婵＄偑鍊栭悧婊堝磻閻愬搫纾婚悗锝庡亞缁♀偓婵犵數濮撮崐缁樻櫠閺囥垺鐓曢悘鐐额嚙婵″潡鏌熼崣澶嬪€愮€殿噮鍣ｅ畷濂告偄閸涘﹦褰搁梻鍌欒兌缁垶宕濋弴鐔告珷閹兼番鍔嶉崑顏堟煃瑜滈崜娆撳煘閹达附鍊烽柡澶嬪灩娴犳悂姊洪懡銈呮珢缂佺姵鎸搁悾鐑藉即閵忊晜鏅滈梺鍓插亞閸犳捇宕㈡禒瀣拺缂備焦蓱閻撱儵鏌熼懞銉х煉妤犵偛锕畷姗€顢欓挊澶嗗亾閸偅鍙忔俊顖滃帶鐢泛顭胯濠㈡﹢鈥︾捄銊﹀枂闁告洦鍓涢ˇ鏉款渻閵堝啫鐏柣鈺婂灦楠炲啫鈻庨幋婵嗙亰闁荤喐鐟ョ€氼剟顢旇ぐ鎺撯拻闁稿本鑹鹃埀顒佹倐瀹曟劙骞栨担鐟颁罕婵犵數濮村ú锕傚吹閸曨垱鐓曢柍鈺佸暙閹肩ǹ霉濠婂啰绉洪柡宀€鍠栭幃婊兾熼搹閫涙偅闂備焦瀵х粙鎾诲窗閺嵮屾綎婵炲樊浜滄导鐘绘煕閺囥劌娅樻繛鐓庣埣濮婅櫣绮欓崹顕呭妷闂佺粯鎸撮埀顒€纾弳锕傛煙閹殿喖顣兼い銉ョ墛缁绘稓澹曠€ｎ偆褰у┑鈽嗗亜閸熸潙顕ｉ锕€绠涢柡澶婄仢缁愭稑顪冮妶鍡樺暗闁哥姵鍔欓、娆愬緞閹邦厸鎷绘繛杈剧悼閹虫捇鍩㈤弴銏＄厸闁逞屽墴閺屽棗顓奸崨顓犫偓顓㈡⒑缁夊棗瀚峰▓鏃堟煢閸愵亜鏋戠紒缁樼洴楠炲鎮欓崘鈺佸摵鐎殿喓鍔嶇粋鎺斺偓锝庡亞閸橀亶姊洪弬銉︽珔闁哥姵鑹鹃埢鎾诲閻樺棗缍婇幃鈩冩償閿濆洤濮辨繝娈垮枛閿曘劌鈻嶉敐鍥у灊婵炲棙鎸搁柨銈嗕繆閵堝惇鍫ュ磻閹炬緞鏃堝礃椤忓棴绱茬紓鍌氬€烽悞锕傛晪闂佽绻戝娆撳煘閹达附鏅柛鏇ㄥ墯濮ｅ牓鎮楃憴鍕闁哥姵鐗犻妴浣糕槈閵忊剝娅滈梺绯曞墲钃遍柣婵囩箞濮婂宕掑▎鎺戝帯缂備緡鍣崹鐢稿箺椤愶附鍋℃繝濠傚缁舵煡鏌涢悢鍛婂唉闁诡喕鍗抽、娆撴煥椤栨矮澹曢梺鎸庣箓妤犳悂寮搁悢鍏肩厓闂佸灝顑呴悘鈺冪磼鏉堛劍灏伴柟宄版噺閹便劑骞嬮敐鍛睏濠电偛妫庨崹浠嬪蓟閸℃鍚嬮柛鈩冪懃楠炲牓姊绘担铏瑰笡闁挎氨鐥紒銏犲籍濠碘剝鎸抽獮鎺楀籍閸屾粣绱叉俊鐐€栧褰掝敄濞嗘挸鍚规繛鍡樻尰閻撳啴鏌﹀Ο渚▓婵″弶鎮傞幗鍫曟晲婢跺鎷洪梺鍓茬厛閸ｎ噣宕曞☉娆戠闁稿繐鍚嬮埛鎰磼缂佹绠為柟顔荤矙濡啫鈽夊Δ鍐╁礋濠电姷顣藉Σ鍛村磻閸涙番鈧啴宕奸妷銉ь唹闂侀潧绻堥崐鏇㈢嵁閵忥紕绠鹃柟瀵稿剱閻掍粙鏌ㄥ☉娆戞创婵﹨娅ｉ幉鎾礋椤愩垹袘婵＄偑鍊栧ú锕傚储閹屽殨鐟滄棃宕洪埀顒併亜閹哄棗浜鹃梺瀹狀潐閸ㄥ潡銆佸▎鎾虫闁靛牆瀚潻鏃堟⒒娴ｅ憡鍟為柨鏇樺灮缁寮借閺嗭箓鏌曞娑欐緲娴滄繈姊洪崨濠傚闁哄倷绶氶獮蹇撁洪鍛幗闂佺粯锚瀵墎绮堝畝鍕厓鐟滄粓宕滃▎鎾崇柈閻犳亽鍔庢稉宥嗙箾瀹割喕绨奸柣鎾跺枛閺屾洝绠涙繝鍐╃彅闂佽楠忕换婵嬪箖娴犲鏁嶆俊鐐额嚙娴滃墽鈧娲栧ú锕€鈻撻幆褉鏀介柣妯肩帛濞懷勪繆椤愩垻鐒哥€规洖銈搁、鏃堝醇閻斿搫骞堥梻浣告惈濞层垽宕濆畝鍕祦婵☆垱鐪规禍婊堟煛閸愶絽浜惧┑鐐跺皺閸犲酣锝炶箛鎾佹椽顢旈崟顐ょ崺濠电姷鏁告慨鎶芥嚄閸撲焦宕插〒姘ｅ亾婵﹨娅ｇ划娆忊枎閹冨闂佽瀛╅惌顕€宕￠幎鐣屽祦闁告劦鐓堝銊╂煃瑜滈崜娆撴偩閻戣姤鏅查柛鈩冾殘缁愮偤鏌ｆ惔顖滅У濞存粍绮嶇€靛吋鎯旈埦鈧弨浠嬫煟濡櫣浠涢柡鍡忔櫅閳规垿顢欓懞銉ュ攭閻庤娲﹂崹鍫曘€佸☉銏″€烽柛娆忓亰闂勫嫮鎹㈠┑瀣棃婵炴垶鑹鹃。鐢告⒑缁嬫鍎愰柟鎼佺畺楠炲骞橀鑲╊槹濡炪倖宸婚崑鎾淬亜閿旇娅婃慨濠呮缁瑥鈻庨幆褍澹堥梻浣瑰濞插繘宕愬┑瀣畺濡わ絽鍟崐濠氭煠閹帒鍔氬ù婊勭矌缁辨帡鎮欓鈧婊冾渻鐎涙ɑ鍊愮€殿喗褰冮埞鎴犫偓锝庡亐閹疯櫣绱撻崒娆戝妽閽冭鲸銇勯妷銉︻棡缂佺粯鐩獮妯兼嫚閳╁啰绉风紓鍌欑劍濮婂宕伴弽褜鍤曢柟缁㈠枛椤懘鏌嶉柨顖氫壕闂佸綊鏀卞钘夘潖濞差亜宸濆┑鐘插暙闂夊秹鎮峰⿰鍕凡闁哥噥鍨崇划瀣吋閸涱亝鏂€闂佹悶鍎弲婵嬵敊閺囥垺鈷戦柛娑橈功閹冲啯銇勯敃渚€鍝洪柡鍛埣瀹曪繝鎮滈崱娆忔暩闂佽崵鍠愰悷銉р偓姘煎幘缁牊寰勯幇顓犲帾闂佹悶鍎崝灞炬叏瀹ュ棭娈介柣鎰綑濞搭喗顨ラ悙鎼劷闁圭懓瀚板畷顐﹀礋椤撗冩暪闂傚倸鍊峰ù鍥敋瑜庨〃銉╁传閵壯傜瑝閻庡箍鍎遍ˇ顖滃閸ф绾ч柛顐亞閸樻稒绻涢崼婊呯煓闁哄本绋戦埥澶婎潨閸繂顫犳俊鐐€愰弲婊堟偂閿熺姴钃熼柣鏃傚帶缁犮儲銇勯弮鍌楁嫛闁哄棭鍙冨铏圭磼濡闉嶅┑鐐跺皺閸犳牠鐛崘銊庢棃宕ㄩ鑺ョ彸闂佺鍋愮悰銉╁焵椤掑啫鐨洪柛鎴濈秺濮婅櫣鎷犻懠顒傤唶濠电偛鐡ㄥ畝绋跨暦閵忥紕顩烽悗锝庘偓顓ㄧ畵閺屾盯寮撮妸銉т紘闂佽桨绀侀崯鎾蓟閵娿儮鏀介柛鈩兠粣娑㈡⒑濮瑰洤鈧洜鈧碍婢橀～蹇涙惞鐟欏嫬鐝伴梺鐐藉劥濞呮洟鎮樺澶嬧拺闁煎鍊曢弳閬嶆煛閸涱垰鈻堝┑鈥崇摠閹峰懘鎳栧┑鍥棃鐎规洏鍔戦、姗€鎮╅幓鎹洖鈹戦敍鍕杭闁稿﹥鐗滈弫顕€骞掗弬鍝勪壕婵ê宕崢瀵糕偓娈垮櫘閸嬪﹤鐣烽幒妤佸€烽悗鐢登瑰鎶芥⒒娴ｈ櫣甯涙繛鍙夌墵瀹曟劙宕烽娑樹壕婵鍋撶€氾拷

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
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆撴倻濡桨鐢绘繝鐢靛Т閿曘倝宕板顑╃喖鍩€椤掑嫭鍊垫繛鍫濈仢閺嬬喖鏌熼鐓庘偓鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸閻忕偟纭堕崑鎾崇暦閸ャ劍鐣烽梻渚€鈧偛鑻晶瀵糕偓瑙勬磻閸楀啿顕ｆ禒瀣垫晣闁绘劗鏁搁弳顐︽煟閻斿摜鐭嬮柡灞诲姂閹箖鏌ㄧ€ｎ剟妾梺鍛婄☉閿曘倖绂嶅⿰鍫熲拺闁告稑锕︾粻鎾绘倵濮樼厧澧柍缁樻崌婵＄兘鍩￠崒妤佸闂備胶枪閺堫剟鎮疯钘濋柨鏇炲€归悡娆撴偣閸ュ洤鎳愰惁鍫ユ倵鐟欏嫭绀€婵犫偓闁秴绠熼柟缁㈠枛缁€瀣亜閹哄棗浜鹃悗娈垮枛濞尖€愁潖濞差亜鎹舵い鎾墲椤庢姊洪崫銉バｉ柣妤冨█楠炲啴鍨鹃幇浣瑰缓闂侀€炲苯澧寸€殿喖顭烽幃銏ゅ川婵犲嫮肖濠德板€х徊浠嬪疮椤栫儐鏁佺€广儱顦伴埛鎴犵磼鐎ｎ偒鍎ラ柛搴㈠姍閺屾盯寮埀顒勬偡閳哄懐宓侀柛鎰╁壆閺冨牆宸濇い鎾跺枎缁佽埖淇婇悙顏勨偓鏍箰妤ｅ啫纾规い鎰剁畱椤懘骞掗搹顐ゎ洸婵犲﹤鐗婇悡蹇擃熆閼哥數鈽夋い鈺婂墰缁辨帡鎮╅幇浣哄姺缂備浇椴哥敮妤€顭囪箛娑樜╅柨鏇楀亾闂傚绉瑰娲偡閺夋寧些闂佺娅曢敋闁伙絿鍏橀、鏇㈡晝閳ь剛绮堢€ｎ剛妫柣妤€鐓鍛洸闁告挆鈧崑鎾诲礂婢跺﹣澹曢梻浣告啞濞诧箓宕滃☉銏犲偍闂侇剙绉甸埛鎴︽⒒閸喕鍎愮憸鐗堝笒绾惧潡鏌熼幆鐗堫棄闁哄嫨鍎甸弻銊モ攽閸℃顦遍梺绋款儐閹告悂鍩㈤幘璇插瀭妞ゆ梻鏅禍顏呬繆閻愵亜鈧倝宕㈡ィ鍐ㄧ婵☆垯璀﹂崵鏇熴亜閹板墎鐣辩紒鐘崇⊕閵囧嫰骞樼捄杞版埛缂備焦鍔楅崑鐐垫崲濠靛鍋ㄩ梻鍫熷垁閵忋倖鍊垫慨妯哄船閸樺鈧娲忛崕闈涚暦閵娧€鍋撳☉娅辨岸骞忕紒妯肩閺夊牆澧介幃濂告煙閾忣偅宕屾い銏¤壘楗即宕ㄩ婊愮床闂佽崵濮村ú锕併亹閸愵喖鐒垫い鎺嗗亾婵炵》绻濋幃浼搭敊閽樺绐炴繝鐢靛仧閸嬫捇锝炲澶嬧拺闂傚牊绋撶粻鍐测攽椤旀儳鍘存い銏＄懇楠炴帡寮崒婊愮闯闂備胶枪閺堫剟鎮疯瀹曟繂顓奸崱鎰盎濡炪倖鎸鹃崑鐐哄窗濡皷鍋撶憴鍕８闁搞劋绮欓悰顕€寮介妸锕€顎撻梺绋跨箰椤︽壆鈧艾銈稿缁樻媴閸涘﹤鏆堢紓浣割儐閸ㄥ潡寮崘顔芥櫜闁搞儺鐓堥崑銊╂⒒娓氬洤澧紒澶屾暬閹繝寮撮姀锛勫幗濠碘槅鍨甸褎鏅堕娑氱闁割偆鍠撻惌宀勬煃瑜滈崜婵嬶綖婢跺⊕鍝勵潨閳ь剙鐣峰┑鍡忔瀻闁规崘娉涚粊锕傛⒑閹肩偛鍔撮柛鎾村哺瀹曟垹鈧綆鍠楅悡鏇熴亜閹邦喖孝闁诲繆鏅滈〃銉╂倷閺夋垵顫掗梺鍝勭焿缁查箖骞嗛弮鍫濈伋闁告劘灏欢鐔兼⒒娴ｅ懙褰掝敄閸℃稑绠查柛銉墮缁犳牗淇婇妶鍛櫤闁哄懏鐓￠獮鏍垝閸忓浜鹃柟棰佺劍妤犲嫰姊婚崒娆愵樂缂侀硸鍠氱划濠氬箣閿旇棄浜遍梺瑙勫礃椤曆呯不閺嶃劋绻嗛柕鍫濇噹閺嗙偤鏌涢悩鍙夘棦闁哄本鐩鎾Ω閵夈儺娼炬俊鐐€х拹鐔煎礉閹达箑钃熼柨婵嗩槸椤懘鏌嶆潪鎷屽厡濞寸媭鍙冮弻锝夊閳轰胶浠梺鍝ュУ閻楃娀鐛崘顭戞建闁逞屽墴閻涱噣骞掗幊铏⒐閹峰懘宕崟顐ゎ啈闂傚倸鍊烽懗鍓佸垝椤栫偛绀夐柡鍥ュ焺閺佸銇勯幘鍗炵仼闁绘挻绻堥弻鐔煎礈瑜忕敮娑㈡煟閹惧啿鏆熼柟鑼归オ浼村醇濠靛牜妲舵繝鐢靛仜濡瑩骞愰幖浣哥＜闁靛ň鏅滈悡娑氣偓骞垮劚閸燁偅淇婇崸妤佺厽婵犻潧妫涢崺锝夋煛瀹€瀣瘈鐎规洖鐖兼俊鐑藉Ψ瑜岄惀顏堟⒒娴ｇ懓鈻曢柡鈧柆宥呭瀭闁秆勵殔閽冪喐绻涢幋鐐冩岸寮ㄦ禒瀣厱闁斥晛鍠氬▓銏犆圭亸鏍т壕闂傚倸鍊风粈渚€鎮块崶顒夋晪鐟滄棁妫熼梺姹囧灮鏋柦鍐枑缁绘盯骞嬮悙鑼姲闂佺ǹ顑嗛幑鍥极瀹ュ纭€闁绘劕寮跺▓銊︾節閵忥綆娼愭繛鑼枎椤繑绻濆顒傦紲濠电偛妫欑敮鎺楀储閿熺姵鐓涢悗锝呭悁缁ㄥジ鏌曢崶褍顏鐐差儏閳规垿宕堕懜纰夌礂濠德板€楁慨鐑藉磻閻愬搫绀夋繛鍡樻尵瀹撲線鏌″鍐ㄥ闁荤喓鎳撻埞鎴︽偐閹绘帩浼€闂佸磭顑曢崐鏍崲濠靛鍋ㄩ梻鍫熺▓閺嬪懎鈹戦悙鏉垮皟闁搞儴鍩栭弲鐐烘⒑閸︻厼顣兼繝銏★耿瀵憡绗熼埀顒勫蓟濞戙垹鐒洪柛鎰亾閳诲秹鎮峰⿰鍛暭閻㈩垱顨婂鏌ュ蓟閵夛妇鍘遍梺闈涱檧缁茶姤淇婃禒瀣厽婵犲灚鍔掔花缁樸亜椤忓嫬鏆ｅ┑鈥崇埣瀹曟﹢濡歌閹綁姊绘担渚敯婵炲懏娲熼弫瀣渻閵堝骸浜滅紒缁樺笧濡叉劙骞掗幊宕囧枔閹风姴顔忛鐟颁壕闁规儳顕粻楣冩煙閻愵剚缍戦柛銈庡墴閺屾盯濡搁埡鈧幉鍓р偓娈垮櫘閸嬪﹪銆佸☉銏″€烽悗鐢登归獮妤呮⒒婵犲骸浜滄繛璇х畵楠炴劙宕滆娑撳秹鏌＄仦璇插姕闁绘挾鍠栭弻锝夊即閻愭祴鍋撴繝姘仼闁汇垻顣介崑鎾斥枔閸喗鐏曢柣蹇撶箲閻熲晠鐛幋锕€绀嬫い鎺嶇椤庢捇姊洪崫鍕窛闁哥姵鍔欒棢闁割偀鎳囬崑鎾舵喆閸曨剛顦ㄩ梺鎸庢磸閸ㄤ粙濡存笟鈧顕€宕煎┑鍡氣偓鍨攽閻愬弶顥為柛鏃€鐗犻、娆撳籍閸啿鎷婚梺绋挎湰閼归箖鍩€椤掑倸鍘撮柟铏殜瀹曟粍鎷呯粙璺ㄤ喊婵＄偑鍊栭悧妤呭礄閻熼偊鐒介柡宓偓閺€浠嬫煟濡櫣鏋冨瑙勶耿閺屾稓浠︾拠鎻掝瀷闂佺懓寮堕幃鍌氼嚕椤曗偓瀹曠厧鈹戦崼銏℃瘒闂傚倷鑳剁划顖炲礉閺囥垹绠规い鎰剁畱閸ㄥ倿鏌涚仦鍓р棨濞存粍绮撻弻鐔兼焽閿曗偓閺嬫稓浜歌箛鏇犵＝濞达綁娼ф俊鍏笺亜椤撶偛妲绘い顐㈢箰鐓ゆい蹇撳瀹撳秴顪冮妶鍡樺暗闁稿绋掓穱濠偯洪鍛嫼闂佺鍋愰崑娑欎繆閸ф鐓涘ù锝呭閻撳ジ鏌曢崱鏇犵獢鐎殿噮鍣ｅ畷鐓庘攽閸繂绠伴梻鍌欑閹测剝绗熷Δ鍛獥婵ǹ娉涢崒銊╂煙闂傚鍔嶉柣鎾跺枛楠炴牠骞栭鐐典化閻庡厜鍋撴繛鎴欏灪閻撴盯鏌涘☉鍗炴灓缂佺姵顭囩槐鎺楊敋閸℃瑧袦閻庢鍣崳锝呯暦婵傚憡鍋勯柛婵嗗缁犱即姊虹拠鏌ヮ€楅柛妯荤矒瀹曟垿骞樼紒妯煎帗閻熸粍绮撳畷婊堟偄妞嬪孩娈炬繛鏉戝悑濞兼瑩宕橀埀顒€顪冮妶鍡樺暗濠殿喖顕划濠囨晝閸屾稈鎷虹紒缁㈠幖閹冲繗銇愯缁辨帡鎮╅崘鑼患缂備緡鍠涢褔鍩ユ径鎰潊闁抽敮鍋撻柟宄邦煼濮婃椽宕滈幓鎺嶇敖闂佸摜濮甸幑鍥春婵犲洤鍗抽柣鏃傜節缁ㄥ姊洪崫鍕偓鎼佹倶濠靛鏁傛い蹇撴绾捐偐绱撴担璇＄劷缂佺姵锕㈤弻娑㈡偐鐠囇冧紣闁句紮绲剧换娑㈡嚑椤掑倸绗＄紓鍌氱Т椤﹂潧顫忕紒妯诲闁告盯娼х紞濠囥€侀弽顓炲窛闁哄鍨归悿鍥р攽椤斿浠滈柛瀣尵閳ь剚顔栭崳顕€宕戞繝鍌滄殾婵せ鍋撴い銏＄懅閸犲﹥娼忛妸褏袩闂傚倸鍊风欢姘焽瑜旈幃褔宕卞☉妯肩枃闂佽宕橀褏绮诲ú顏呯厸闁搞儯鍎遍悘顏堟煃闁垮鐏撮柡灞剧☉閳规垿宕卞Δ濠佺棯闂備胶枪鐞氼偊宕濋幋锕€钃熸繛鎴欏灩鎯熼梺闈涳紡閸涘懌鍔戝娲传閸曨喖顏銈嗗灥椤︻垶鎮鹃悜钘夐唶闁哄洨鍊ｉ埡鍐╁枑闊洦绋掗崕濠囨煕椤愮姴鍔滈柍閿嬪浮閺屾稓浠﹂悙顒傛婵犫拃鍕€掔紒杈ㄥ笧閳ь剨缍嗛崑鍕箔瑜忕槐鎺楊敊绾柉鍚梺绯曟櫔缁绘繂鐣烽幒鎴旀瀻闁瑰濮烽弳鏉库攽閿涘嫬浜奸柛濠冪墵瀹曟繈骞嬮敃鈧壕褰掓煛閸モ晛鏋旀い鈺呮敱缁绘繃绻濋崒姘间淮婵炲瓨绮嶇划宥夊Φ閸曨喚鐤€闁圭偓鍓氭禒濂告⒑閸濆嫭锛旂紒韫矙閸╃偤骞嬮敃鈧悘宕団偓瑙勬礀濞层倝鎮￠悢鍏煎€甸悷娆忓绾炬悂鏌涢弮鈧崹鍧楀Υ娴ｇ硶妲堟俊顖氬槻閻楁岸鏌熼懖鈺勊夐柛鎾寸箓閳诲秷绠涘☉娆屾嫽闂佺ǹ鏈懝楣冨焵椤掑倸鍘撮柟铏殜瀹曟粍鎷呴悷鏉垮箲闂備礁澹婇崑鍛洪弽顓熷仾闁绘劦鍓涚弧鈧繝鐢靛Т閸婄粯鏅跺☉銏＄厱閻庯綆浜堕崕鏃堟煛瀹€瀣埌閾伙綁鏌涢…鎴濇灈闁诡垳鍋ら幃妤冩喆閸曨剙鐭紓浣藉煐瀹€绋款嚕鐠囨祴妲堥柕蹇婃櫆閺呮繈姊虹紒妯曟垼銇愰崘鈺傚弿閹兼番鍔嶉悡娆愩亜閺嶃劎鐭婇柡瀣⊕閹便劍绻濋崨顕呬哗闂侀€炲苯澧繝鈧潏銊﹀弿闁汇垻枪杩濋梺鍛婂姦閸犳鎮￠悩缁樼厱闁归偊鍨伴惃娲煕閳哄绋婚柕鍥у婵偓闁挎稑瀚崳顓犵磽娴ｈ櫣甯涚紒璇茬墕閻ｇ兘宕奸弴鐐嶁晠鏌曡箛濞惧亾閹颁椒绱梻鍌氬€搁崐宄懊归崶褏鏆﹂柣銏⑶圭粣妤呮煛瀹ュ骸骞栫痪鎹愵嚙閳规垿鎮╅崣澶嬫倷闂佸憡鑹剧紞濠囧蓟閿濆妫橀柛顭戝枟閸婎垶姊虹紒妯诲鞍婵炶尙鍠栭獮鍐ㄎ旈崨顔芥珳闁硅偐琛ラ埀顒佸墯濞煎姊绘担鍛婃儓妞ゆ垵妫涚划娆撳箣閿曗偓閻撴﹢鏌熸潏鍓х暠闁诲繐纾埀顒冾潐濞叉牕煤閵堝鍋傞柣鏂垮悑閳锋垹绱掗娑欑婵炲懏姊荤槐鎺旂磼濡偐鐣甸梺浼欑悼閸忔ɑ淇婇幖浣哥厸濠电姴鍟▍鎾绘⒑閼姐倕鏋戦柣鐔村劤閳ь剚鍑归崜鐔笺€侀弮鍫濇嵍妞ゆ挾濮烽敍婵嬫⒑缁嬫寧婀伴柤褰掔畺閸┾偓妞ゆ帒瀚峰Λ鎴犵磼椤旇偐澧涚紒妤冨枛閸┾偓妞ゆ帒瀚ㄩ埀顑跨窔瀵粙顢橀悙鑼垛偓鍨攽閻愭潙鐏卞瀛樻倐瀵煡鎮欏ù瀣杸闂佺粯鍔曞鍫曀夐悙鐑樼厵缂佸瀵ч幉绋款熆鐟欏嫭绀嬬€规洜枪铻栧ù锝呮惈楠炲秹姊绘担绛嬫綈妞ゆ梹鐗犲畷浼村冀椤撴壕鍋撻崒娑氶檮闁告稑锕﹂崢浠嬫⒑鐟欏嫬鍔ゆい鏇ㄥ弮楠炲﹪宕堕浣哄幐閻庡厜鍋撻悗锝庡墰琚﹂梻浣筋嚃閸犳捇宕归挊澶屾殾妞ゆ劧绠戠粈瀣煕椤垵浜濋柣锔芥濮婂宕掑▎鎴М闂佺濮ょ划宥夊箞閵娾晜鍋ㄩ柛娑橈攻濞呭洭姊洪棃娑氱疄闁稿﹥娲熼幏鎴︽偄閸忚偐鍙嗗┑鐘绘涧濡瑩宕抽搹顐＄箚闁圭粯甯炴晶锕傛煛鐏炲墽鈽夐摶鏍煕閹扳晛濡烘繛宀婁邯濮婅櫣鎷犻懠顒傤唹濠电偛寮剁划搴∥ｉ幇鏉跨閻庢稒锚椤庢捇姊洪崨濠勭畵閻庢艾鎳橀弫鎰板炊閳哄喛绱查梺璇插嚱缂嶅棝宕戦崟顖涘€堕柟缁㈠枟閻撴洟鏌嶉悷鎵虎闁告梹绮庨埀顒€鐏氬妯尖偓姘煎幘閹广垹鈽夐姀鈥斥偓閿嬨亜閹烘垵鈧敻宕戦幘瀵哥瘈婵﹩鍙庡Λ鍛渻閵堝棗濮傞柛銊︽そ楠炴牠骞囬悧鍫㈠弳闂佸搫娲﹂敋闁逞屽墯閻楁粎鍒掔€ｎ亶鍚嬪璺猴功閻ｉ箖鎮峰⿰鍐炬█鐎殿喓鍔嶇粋鎺斺偓锝庡亞閸樿棄鈹戦埥鍡楃仴婵炲拑缍侀弫宥咁吋閸℃劒绨婚梺鎸庣箓濡盯宕ｉ埀顒勬⒑閸濆嫮鐒跨紓宥勭窔閻涱喖螣閸忕厧纾柣鐐寸▓閸撴繈鎮楁导瀛樷拻濞达絽鎲￠幆鍫ユ煟椤掆偓閵堢ǹ鐣锋导鏉戝唨妞ゆ挻澹曢崑鎾存媴閸撳弶寤洪梺閫炲苯澧存鐐插暣婵偓闁宠棄妫欐晥闂佺澹堥幓顏嗗緤妤ｅ啫闂い鏍仦閳锋帡鏌涚仦鍓ф噮妞わ讣绠撻弻鐔访圭€Ｑ冧壕闁归鐒︾紞搴ㄦ⒑闂堚晛鐦滈柛妯恒偢瀹曠懓鈹戦崼姘壕妤犵偛鐏濋崝姘箾鐠囇呯暠闁伙絿鍏樺畷锝嗗緞瀹€鈧鏇㈡⒑閻熸壆鎽犻柣鐔村劦閹﹢鍩￠崘锝呬壕闁荤喓澧楅崯鐐电磼鐠囨彃鏆ｅ┑锛勬暬瀹曠喖顢涘杈╂澑闂備礁鐤囧銊╂嚄閼搁潧顕遍柛鏇ㄥ墰缁犻箖鎮楅悽鐧诲湱鏁崼鏇熺厽婵°倐鍋撴俊顐ｇ箞瀹曟椽鎮欓崫鍕吅闂佹寧姊荤划顖炲疾閳哄啰纾肩紓浣靛灩瀵噣鏌￠埀顒勬焼瀹ュ懏鐎梺璇″瀻閳ь剟寮ㄦ禒瀣叆婵炴垶锚椤忊晛霉閻橆偅娅嗗ǎ鍥э躬椤㈡洟濮€閻欌偓娴煎啴姊虹拠鈥虫珝缂佺姵鐗犻獮鍐ㄢ堪閸涱垶鈹忛柣搴秵閸嬪懎鈻嶉崶顒佲拻濞达絿鎳撻婊勭箾閹绘帞效鐎规洘鍨块獮姗€宕滄担鐚寸床闂備線鈧偛鑻晶浼存煃瑜滈崜銊х礊閸℃稑纾诲ù锝呮贡椤╁弶绻濇繝鍌滃闁绘挻鐟╁娲敇閵娧呮殸闂佸搫顑嗛惄顖炲蓟閿涘嫧鍋撻敐搴濋偗妞ゅ孩顨婂Λ浣瑰緞鐎ｎ剛鐦堟繝鐢靛Т閸婄粯鏅堕姀銈嗙厽闁挎繂妫欓妵婵嬫煛瀹€瀣？濞寸媴绠撳畷婊嗩槼闁告帗绋戦—鍐Χ閸愩劎浠惧┑鐐跺皺閸犳牕顕ｉ锕€绠涢柡澶婄仢閼板灝鈹戦悙鍙夘棡閽冭京鎮敐鍥╃＝闁稿本鑹鹃埀顒傚厴閹虫宕奸弴鐐碉紱闂佽宕橀褏澹曠捄銊㈠亾鐟欏嫭绀€婵炲眰鍔庣划鍫熺節閸屾鏂€闂佺粯锚瀵爼骞栭幇鐗堢參闁告劦浜滈弸娑㈡煛鐏炵晫效濠碉紕鍏橀弫鍐焵椤掍焦娅犳い鏍仦閻撴洘鎱ㄥ鍡楀⒒闁稿孩姊归〃銉╂倷鐎电ǹ鈷堥梺鍛婂笚鐢€崇暦濡や礁绶炲┑鐘辩椤ユ岸姊绘担钘変汗闁冲嘲鐗撳畷婊冣槈閵忕姵妲悗骞垮劚椤︿即鍩涢幒妤佺厱閻忕偛澧介幊鍡涙煕韫囨挾鐒哥€殿喗鎮傞、鏃堝川椤愶紕鐩庨梻浣烘嚀閻°劎鎹㈤崘顔㈠濮€閳藉棙顔旈梺缁樺姈濞兼瑦鎱ㄥ鍡╂闁绘劕顕晶顒傜磼濡ゅ啫鏋戠紒缁樼箞瀹曠喖顢樺☉娆欑礄濠电姷鏁告慨鐑藉极閸涘﹦绠鹃柍褜鍓氱换娑欐媴閸愬弶绁╅柡浣稿閺岀喖鎮ч崼鐔哄嚒闂佸搫顑勭欢姘跺蓟濞戙垹鐒洪柛鎰典簴婵洭姊虹紒妯诲鞍婵炶尙鍠栭獮鍐潨閳ь剟骞冨▎鎾崇闁瑰搫妫欑€垫牠姊绘担鑺ャ€冮柣鎺炵畵閹囨偐鐠囪尪鎽曢梺鍝勬川閸犳挾绮绘ィ鍐╃厽闁逛即娼ф晶顖炴煟濠靛洦绀嬫慨濠冩そ楠炲酣鎳為妷锔芥闂備焦鎮堕崝灞筋焽閳ユ剚鍤曢悹鍥ㄧゴ濡插牊淇婇鐐存暠闁哄倵鍋撻梻鍌欒兌绾爼宕滃┑瀣ㄢ偓鍐疀閺傛鍤ら梺绋挎湰閸戠懓銆掓繝姘厪闁割偅绻堥妤€霉濠婂啫鈷旂紒杈ㄥ浮閹晠鎼归銏紦闁诲孩顔栭崰娑㈩敋瑜旈崺銉﹀緞閹邦剦娼婇梺缁橆焽閺佺ǹ顭囨径鎰拻濞撴埃鍋撻柍褜鍓涢崑娑㈡嚐椤栨稒娅犻悗鐢电《閸嬫挾鎲撮崟顒傤槬缂傚倸绉撮敃銈夋偩妞嬪海鐭欓悹鍥╁亾濡啫鐣峰鈧俊姝岊槹闁稿锕濠氬磼濞嗘埈妲梺纭咁嚋缁绘繂鐣峰┑瀣櫇闁稿本姘ㄩ悰銉╂⒑閸濆嫮鈻夐柛娆忕箳缁辩偤寮介鐔哄幈濠电偞鍨堕…鍥箺閻樼粯鐓熼柟鎯х－缁犲鏌＄仦鍓ф创妞ゃ垺娲熼弫鎰板炊閳哄啯姣囬梻鍌欑窔濞艰崵寰婇挊澶涜€块弶鍫氭櫆椤洟鏌熼悜姗嗘當缂佺姵绋掗妵鍕箻鐠虹洅娑樷攽椤斿吋宸濈紒杈ㄦ尰閹峰懘鎼归悷鎵偧缂傚倷娴囬褔鎮ч幘鑽ゅ祦闁告劏鏅濋々鐑芥倵閿濆骸浜芥俊顐㈠暙閳规垿鎮欏▓鎸庮棑缂傚倸鐗呴崡鍐茬暦鐟欏嫮顩烽悗锝庝簽閻ｅ搫鈹戞幊閸婃劙宕戦幘娣簻妞ゆ劧绲跨粻鐐存叏婵犲懎鍚归柍褜鍓熷褔骞夐敓鐘茬柧闁稿繗鍋愮弧鈧紒鍓у鑿ら柛瀣崌瀹曟﹢鎳犻鍕礈闂傚倷绀侀幉鈥愁潖瑜版帗鍋￠柕澶嗘櫓閺佸鏌ㄥ┑鍡╂Ц缂佺姷绮妵鍕籍閸屾瀚涘┑鐐靛帶閿曨亜顫忓ú顏勭闁兼亽鍎插▓浼存⒑缁嬫鍎愰柟鐟版喘瀵鎮㈤崜鍙壭ч柟鑹版彧缁茬晫绮敍鍕＝濞达綀娅ｇ敮娑氱磼鐎ｎ偄绗掓い顓炴穿缁犳稑鈽夊Ο纰辨Ф闁荤喐绮岀粔鐟扮暦閸濆嫮鏆嗛柛鏇ㄥ厴閹疯櫣绱掔紒銏犲箹闁瑰啿姘﹂。璺ㄧ磽閸屾瑧璐伴柛鐘崇墪闇夐柣鎴ｆ缁€鍡涙煙閻戞﹩娈旂紒顐㈢Ч閺屾盯顢曢妶鍛€婚梺閫炲苯澧扮紒顕呭灡缁岃鲸绻濋崶鑸垫櫖闂佺粯鍔曢悺銊╂偟瀹曞洨纾藉ù锝夋涧婵″吋銇勯鐐叉Щ妞ゎ偄绻愮叅妞ゅ繐鎷嬪Λ鍐ㄢ攽閻愭潙鐏卞瀛樻倐椤㈡捇宕卞☉娆屾嫽闂佺ǹ鏈懝楣冾敂閳哄懏鍊垫慨妯煎帶婢ф挳鏌熼钘夊姢闁伙綇绻濋獮宥夘敊閼恒儳鏆﹂梻鍌欑窔閳ь剛鍋涢懟顖涙櫠閹绢喗鐓曢柍瑙勫劤娴滅偓淇婇悙顏勨偓鏍暜閹烘绐楁慨姗嗗墻閻掍粙鏌熼柇锕€骞樼紒鐘荤畺閺屾稑鈻庤箛锝嗩€嗛梺鍏兼緲濞硷繝寮婚弴銏犲耿婵°倐鍋撻柡鍡悼閳ь剝顫夊ú蹇涘礉瀹ュ拋鐒介煫鍥ㄧ☉缁€鍫㈡喐韫囨稑鐓熼柕鍫濇缁♀偓闂佹眹鍨藉褍鐡梻浣瑰濞插繘宕愬┑瀣畺鐟滄柨鐣烽崡鐐╂婵炲棗绻嗛崑鎾诲锤濡や胶鍘卞銈庡幗閸ㄥ灚绂嶉弽顬＄懓饪伴崟顓犵厜闂佸搫鐬奸崰鏍х暦椤愶箑绀嬫い鎺戭槹椤ワ絽鈹戦悙鑼憼缂侇喖绉瑰畷鏇㈠箮鐟欙絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯鍩勯崘鍓у姺闂佸疇顫夌粙鎾舵閹捐纾兼慨妯荤樂閿涘嫮纾奸柤鎼佹涧閸濆搫鈹戦垾宕囨憼缂佹鍠栭崺鈧い鎺嗗亾妞ゆ洩缍侀獮姗€顢欑喊杈ㄧ秱闂備焦鏋奸弲娑㈠疮椤栫偞鍋熼柡宥庡幗閳锋帒銆掑锝呬壕闂侀€炲苯澧伴柛瀣洴閹崇喖顢涘☉娆愮彿闂佽顔栭崰姘卞閸忕浜滈柡鍌氱仢閳锋棃鎮介娑氥€掔紒杈ㄥ笧缁辨帒螣閼测晝鏆ゆ俊鐐€ら崑鍛崲閸儯鈧礁顫滈埀顒勫箖濞嗘挸绾ч柛顭戝櫘閸ゆ瑩姊婚崒姘偓鎼佸磹閻戣姤鍊块柨鏇炲€哥粻鏍煕椤愶絾绀€缁剧偓瀵х换婵囩節閸屾粌顣虹紓浣插亾闁告劏鏂傛禍婊堢叓閸ャ劍灏靛褋鍨婚埀顒冾潐濞叉鍒掑畝鍕厴闁硅揪闄勯弲顒勬煕閺囥劌浜楅柡澶樺弮濮婅櫣绮欓崠鈩冩暰闂佹悶鍔岄悘婵嬫偩閻戣棄鍗抽柕蹇娾偓铏吇婵＄偑鍊栫敮濠勭矆娴ｈ鍎熷┑鐘叉处閳锋垿鏌涢幘鏉戠祷濞存粍绻勭槐鎺旀嫚閼碱儷銏°亜椤撶偞鍠樼€殿喕绮欐俊姝岊槾妞ゆ梹娲熼幃妤呮偡閺夋妫岄梺鍝ュУ濞叉粓鎳為柆宥呯缂佹妗ㄧ花濠氭⒑闂堟侗妲堕柛搴ゆ珪閺呰泛鈽夊杈╋紲缂傚倷鐒﹂…鍥虹€涙﹩娈介柣鎰▕閸庢棃鏌熼鐣屾噮闁圭懓瀚粭鐔碱敍濞戣鲸锛堝┑鐘殿暜缁辨洟宕戦幋锕€纾圭憸蹇擃嚗婵犲啰顩烽悗锝庝簽椤旀劕顪冮妶鍡楀Ё缂佺姵鍨块幃鈥愁潨閳ь剟寮婚悢鍛婄秶闁告挆鍛闂備胶绮幖顐ゆ崲濠靛钃熸繛鎴欏灩閻掓椽鏌涢幇鍓佺窗婵炲矈浜炵槐鎾存媴闂堟稑顬嬮梺鍛婎焼閸ワ絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯骞囬埡浣割瀳濡炪値鍓欓悧鎾愁潖濞差亜绠伴幖娣灩椤︹晜绻涚€涙鐭ゅù婊勭箖缁岃鲸绻濋崟銊ヤ壕婵炴垶鐟悞浠嬫煟閻旈绉洪柡灞界Х椤т線鏌涢幘瀵告噰妞ゃ垺宀搁弫鎰緞濡粯娅旈梻渚€鈧偛鑻晶顕€鏌ｉ敐鍥у幋闁诡喒鍓濋幆鏂课熺紒妯绘緫闂備浇顕ч崙鐣岀礊閸℃顩叉繝闈涱儏濮瑰弶銇勮箛鎾跺闁抽攱甯￠弻娑氫沪閸撗勫櫘濡炪倧璁ｇ粻鎾诲蓟閻斿搫鏋堥柛妤冨仒缁ㄥ鏌ｉ幘鍗炩偓婵嬪蓟閺囩喓绠鹃柣鎰靛墯閻濇棃姊哄Ч鍥р偓銈夊闯閿濆钃熸繛鎴炵矤濡茬厧顪冮妶鍐ㄥ闁硅櫕鍔楅崚鎺撶節濮樺吋鏅梺缁樺姇椤曨參宕㈤崡鐐╂斀闁绘绮☉褎淇婇锝庢疁妞ゃ垺姊婚埀顒佺⊕椤洨绮绘ィ鍐╃厵闁绘劦鍓欐晶顖炴煟閹剧偨鍋㈤柡宀嬬磿娴狅箓鎮欓鍌ゆЧ闁诲氦顫夊ú婊堝储瑜旈崺鐐哄箣閿曗偓閻愬﹪鏌ら崨濠庡晱婵☆値鍛＝闁稿本鑹鹃埀顒傚厴閹偤鏁冮崒妯峰亾閸愵喖骞㈡繛鍡樺姈濞堜即姊虹捄銊ユ灁濠殿喗鎸抽幏鎴︽偄閸忚偐鍘繝鐢靛仧閸嬫挸鈻嶉崨瀛樼厱婵°倕瀚悵顏堟煏閸パ冾伂缂佺姵鐩獮姗€骞囨担椋庣濠电姷鏁搁崑娑樜熸繝鍥у偍闁告挆鍕垫綗闂佽鍎抽顓㈡偡瑜版帗鐓曢柕澶嬪灥鐎氼喗绂嶉懜鐢电瘈缁剧増蓱椤﹪鏌涚€ｎ亜顏€规洦鍨堕獮姗€顢欓崲澶堝劚闇夐柨婵嗘噹椤ュ繑绻涚亸鏍ㄦ珚闁诡喛顫夐妶锕傚箰鎼搭喕鍝楅梻浣告啞濡垹绮婚幋锔光偓鏃堝礃椤斿槈褔鏌涢埄鍐炬畼闁荤喆鍔戦弻锝嗘償閵忕姴姣堥梺鍛婃尵閸犳牠鐛箛娑樼闁挎棁妫勬禍婊堟⒑缁嬭法绠洪柛瀣姍瀹曘垽骞樼紒妯锋嫼闁诲骸婀辨慨鐢稿Υ閸愵亞纾奸柍褜鍓氱粭鐔煎焵椤掆偓椤曪綁骞庨懞銉モ偓閿嬬箾閺夋埈鍎愰柡鍌楀亾濠碉紕鍋戦崐鏍偋濡ゅ啰鐭欓柟鐑樻煣閻掑﹪鏌ㄩ弴鐐测偓褰掓偂濞嗘劑浜滈柡鍐ㄥ€哥敮鑸典繆閹绘帩鐓奸柡灞界Ф閹风娀宕ｆ径濠冩暘缂傚倷鑳舵慨鐢告儎椤栫偟宓佹繛鍡樻尭缁€鍐喐鎼淬劌纾婚柛娑卞灣绾句粙鏌涚仦鍓ф噮妞わ讣绠戦…鑳槺缂侇喗鐟ラ悾鐑藉箣閿曗偓缁犲鎮楅棃娑欐喐妞ゆ梹娲樼换婵嬫偨闂堟刀銏＄箾鐏炲倸鈧鍒掓繝姘唨闁靛ǹ鍊楃粻姘舵⒑闂堟稓澧曢柛濠傛啞缁傚秵銈ｉ崘鈹炬嫼閻庤娲栧ú銊ф暜濞戙垺鐓欓柣鐔哄閹兼劙鏌ｉ敐鍥ㄧ効闁靛洦鍔欓獮鎺戔攽閸ャ劍鐝﹂梻鍌欐缁鳖喚寰婃禒瀣殣妞ゆ牜鍋炵€氬啴鎮楅敐搴℃灍闁抽攱甯￠弻娑氫沪閸撗勫櫘濡炪倧璁ｇ粻鎾诲蓟閻旂儤瀚氱憸蹇曞姬閳ь剙螖閻橀潧浠﹂柛鏃€鐗犳俊鍫曟晲婢跺﹦顦ㄩ梺闈浤涢崨顓犲€為梻鍌氬€风粈浣虹礊婵犲洤纾诲┑鐘叉搐閸屻劌螖閿濆懎鏆欓柣鎾搭焽閳ь剙绠嶉崕閬嶅箠婢舵劕缁╁ù鐘差儐閻撶喐淇婇姘儓缂佺嫏鍥ㄧ厵闁稿繒鍘ф慨宥嗘叏婵犲啯銇濈€规洩绻濋幃娆撳级閹寸偟浜烽梻鍌欑閹碱偊鎯屾径灞惧床婵犻潧妫涢弳锕傛煕椤愶絾绀€闁绘挻绋戦湁闁挎繂顦藉Λ鎴︽煙鐟欏嫭绀嬫慨濠勭帛閹峰懐绮欓幐搴♀偓顖氣攽閻橆喖鐏柨鏇樺灲楠炲啫螣鐞涒剝鏂€闁诲函缍嗘禍鐐哄储閹绢喗鈷戦柣鐔煎亰閸ょ喖鏌涚€ｎ偆銆掔紒顔碱煼閹晝绱掑Ο閿嬪缂傚倷绀侀鍛焊閸涙潙缁╃紓浣诡焽缁犻箖鏌ょ喊鍗炲闁哄绋撻埀顒侇問閸ｎ噣宕戞繝鍌滄殾婵せ鍋撴い銏＄懇閹崇偤濡烽敂鎯х闂傚倸鍊烽悞锕傚箖閸洖纾挎繛宸簼閸嬪倿鏌涢锝囩闁绘帊绮欓弻锝夊棘閸喗鍊梺鎶芥敱閸ㄥ潡寮婚悢铏圭煓闁割煈鍣崝澶岀磽娴ｆ彃浜鹃梺閫炲苯澧存慨濠呮缁瑩宕犻垾鍏呯矗缂傚倸鍊哥粔鎾晝椤忓嫮鏆﹂柛婵嗗濡插牓鏌曡箛鏇炐ユい鏂匡工閳规垶骞婇柛濠冩崌閹虫宕奸弴鐐插亶闂備緡鍓欑粔鐢告偂閻旇偐鍙撻柛銉ｅ妽缁€鈧繛瀵稿У缁矂鈥﹂懗顖ｆШ缂備緡鍠楅悷鈺呮偘椤旂晫绡€闁搞儜鍜佹Ч婵＄偑鍊栭幐鐐叏閻戣姤鏅繝濠傚缁犻箖寮堕崼婵嗏挃闁告帊鍗抽弻鐔哄枈閸楃偘鍠婇梺闈涙閸婂潡骞婇弽顓炵厸闁告劦浜风槐鑼磽閸屾艾鈧兘鎮為敃鍌椻偓锕傚炊椤掆偓閻撯偓闂佸搫娲ㄦ慨顓㈠磻閹捐鍨傛い鎰剁悼閸戯繝鏌ｆ惔銏犲毈闁革綇缍佸畷娲焵椤掍降浜滈柟鐑樺煀閸旂喓绱掓径灞炬毈闁哄本绋撻埀顒婄秵娴滄繈藟閵忋倖瀵犳繝闈涙灩瑜版帗鍋傞幖杈剧稻閹插ジ姊虹粙鍖″姛闁轰礁顭烽獮鍐ㄧ暋閹佃櫕鐎婚棅顐㈡处閹尖晜绂掗崜褏纾奸柣鎰靛墮閸斻倖銇勯鐘插幋鐎殿喛顕ч埥澶娢熷⿰鍕拹闁瑰嘲鎳愰崠鏍即閻旇桨绨介梻鍌氬€烽懗鍓佸垝椤栨稓鐟规俊銈呮噹缁愭鏌″搴″伎缂傚秵鐗楃换婵囩節閸屾粌顣洪梺缁樺笒閻忔岸濡甸崟顖氱闁糕剝銇炴竟鏇㈡⒒娴ｉ涓茬紒韫矙瀹曟煡寮婚妷锝呬杭婵炴潙鍚嬪娆戠矆鐎ｎ偁浜滈柟鍝勬娴滄儳顪冮妶鍛劉闁圭懓娲ら～蹇撁洪鍕暰閻熸粌绻掔划濠氬箮閼恒儳鍘甸梺鎯ф禋閸嬪懐浜告导瀛樼厵濞撴艾鐏濇俊鐣岀磼閸屾稑绗ч柍褜鍓ㄧ紞鍡涘磻閳ь剟鏌熷畡閭︾吋婵﹨娅ｇ划娆撳箰鎼淬垺瀚抽梻浣藉吹閸ｏ附淇婇崶鈺佸灊濠电姴娲﹂崑瀣煕椤愮姴鐏い鏃€娲樼换婵嬪閿濆棛銆愬銈嗗灥濡稓鍒掗崼鈶╁亾閿濆骸鍘撮柛瀣尵閹叉挳宕熼鍌ゆО濠电姷鏁搁崑娑㈡晝椤忓嫷鍤曟い鎰剁畱缁€鍐┿亜閺冨洤浜归柨娑欑矊閳规垿顢欓弬銈堚偓璺ㄧ棯椤撶喐鍊愮€规洦鍓熼幃浠嬪川婵炵偓瀚藉┑鐐舵彧缂嶁偓濠殿喓鍊楀☉鐢稿醇閺囩喓鍘遍梺鎸庣箓缁绘帡鎮鹃崹顐闁绘劘灏欑粻濠氭煛娴ｈ宕岄柡浣规崌閺佹捇鏁撻敓锟�32闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆撴倻濡桨鐢绘繝鐢靛Т閿曘倝宕板顑╃喖鍩€椤掑嫭鍊垫繛鍫濈仢閺嬬喖鏌熼鐓庘偓鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸閻忕偟纭堕崑鎾崇暦閸ャ劍鐣烽梻渚€鈧偛鑻晶瀵糕偓瑙勬磻閸楀啿顕ｆ禒瀣垫晣闁绘劗鏁搁弳顐︽煟閻斿摜鐭嬮柡灞诲姂閹箖鏌ㄧ€ｎ剟妾梺鍛婄☉閿曘倖绂嶅⿰鍫熲拺闁告稑锕︾粻鎾绘倵濮樼厧澧柍缁樻崌婵＄兘鍩￠崒妤佸闂備胶枪閺堫剟鎮疯钘濋柨鏇炲€归悡娆撴偣閸ュ洤鎳愰惁鍫ユ倵鐟欏嫭绀€婵犫偓闁秴绠熼柟缁㈠枛缁€瀣亜閹哄棗浜鹃悗娈垮枛濞尖€愁潖濞差亜鎹舵い鎾墲椤庢姊洪崫銉バｉ柣妤冨█楠炲啴鍨鹃幇浣瑰缓闂侀€炲苯澧寸€殿喖顭烽幃銏ゅ川婵犲嫮肖濠德板€х徊浠嬪疮椤栫儐鏁佺€广儱顦伴埛鎴犵磼鐎ｎ偒鍎ラ柛搴㈠姍閺屾盯寮埀顒勬偡閳哄懐宓侀柛鎰╁壆閺冨牆宸濇い鎾跺枎缁佽埖淇婇悙顏勨偓鏍箰妤ｅ啫纾规い鎰剁畱椤懘骞掗搹顐ゎ洸婵犲﹤鐗婇悡蹇擃熆閼哥數鈽夋い鈺婂墰缁辨帡鎮╅幇浣哄姺缂備浇椴哥敮妤€顭囪箛娑樜╅柨鏇楀亾闂傚绉瑰娲偡閺夋寧些闂佺娅曢敋闁伙絿鍏橀、鏇㈡晝閳ь剛绮堢€ｎ剛妫柣妤€鐓鍛洸闁告挆鈧崑鎾诲礂婢跺﹣澹曢梻浣告啞濞诧箓宕滃☉銏犲偍闂侇剙绉甸埛鎴︽⒒閸喕鍎愮憸鐗堝笒绾惧潡鏌熼幆鐗堫棄闁哄嫨鍎甸弻銊モ攽閸℃顦遍梺绋款儐閹告悂鍩㈤幘璇插瀭妞ゆ梻鏅禍顏呬繆閻愵亜鈧倝宕㈡ィ鍐ㄧ婵☆垯璀﹂崵鏇熴亜閹板墎鐣辩紒鐘崇⊕閵囧嫰骞樼捄杞版埛缂備焦鍔楅崑鐐垫崲濠靛鍋ㄩ梻鍫熷垁閵忋倖鍊垫慨妯哄船閸樺鈧娲忛崕闈涚暦閵娧€鍋撳☉娅辨岸骞忕紒妯肩閺夊牆澧介幃濂告煙閾忣偅宕屾い銏¤壘楗即宕ㄩ婊愮床闂佽崵濮村ú锕併亹閸愵喖鐒垫い鎺嗗亾婵炵》绻濋幃浼搭敊閽樺绐炴繝鐢靛仧閸嬫捇锝炲澶嬧拺闂傚牊绋撶粻鍐测攽椤旀儳鍘存い銏＄懇楠炴帡寮崒婊愮闯闂備胶枪閺堫剟鎮疯瀹曟繂顓奸崱鎰盎濡炪倖鎸鹃崑鐐哄窗濡皷鍋撶憴鍕８闁搞劋绮欓悰顕€寮介妸锕€顎撻梺绋跨箰椤︽壆鈧艾銈稿缁樻媴閸涘﹤鏆堢紓浣割儐閸ㄥ潡寮崘顔芥櫜闁搞儺鐓堥崑銊╂⒒娓氬洤澧紒澶屾暬閹繝寮撮姀锛勫幗濠碘槅鍨甸褎鏅堕娑氱闁割偆鍠撻惌宀勬煃瑜滈崜婵嬶綖婢跺⊕鍝勵潨閳ь剙鐣峰┑鍡忔瀻闁规崘娉涚粊锕傛⒑閹肩偛鍔撮柛鎾村哺瀹曟垹鈧綆鍠楅悡鏇熴亜閹邦喖孝闁诲繆鏅滈〃銉╂倷閺夋垵顫掗梺鍝勭焿缁查箖骞嗛弮鍫濈伋闁告劘灏欢鐔兼⒒娴ｅ懙褰掝敄閸℃稑绠查柛銉墮缁犳牗淇婇妶鍛櫤闁哄懏鐓￠獮鏍垝閸忓浜鹃柟棰佺劍妤犲嫰姊婚崒娆愵樂缂侀硸鍠氱划濠氬箣閿旇棄浜遍梺瑙勫礃椤曆呯不閺嶃劋绻嗛柕鍫濇噹閺嗙偤鏌涢悩鍙夘棦闁哄本鐩鎾Ω閵夈儺娼炬俊鐐€х拹鐔煎礉閹达箑钃熼柨婵嗩槸椤懘鏌嶆潪鎷屽厡濞寸媭鍙冮弻锝夊閳轰胶浠梺鍝ュУ閻楃娀鐛崘顭戞建闁逞屽墴閻涱噣骞掗幊铏⒐閹峰懘宕崟顐ゎ啈闂傚倸鍊烽懗鍓佸垝椤栫偛绀夐柡鍥ュ焺閺佸銇勯幘鍗炵仼闁绘挻绻堥弻鐔煎礈瑜忕敮娑㈡煟閹惧啿鏆熼柟鑼归オ浼村醇濠靛牜妲舵繝鐢靛仜濡瑩骞愰幖浣哥＜闁靛ň鏅滈悡娑氣偓骞垮劚閸燁偅淇婇崸妤佺厽婵犻潧妫涢崺锝夋煛瀹€瀣瘈鐎规洖鐖兼俊鐑藉Ψ瑜岄惀顏堟⒒娴ｇ懓鈻曢柡鈧柆宥呭瀭闁秆勵殔閽冪喐绻涢幋鐐冩岸寮ㄦ禒瀣厱闁斥晛鍠氬▓銏犆圭亸鏍т壕闂傚倸鍊风粈渚€鎮块崶顒夋晪鐟滄棁妫熼梺姹囧灮鏋柦鍐枑缁绘盯骞嬮悙鑼姲闂佺ǹ顑嗛幑鍥极瀹ュ纭€闁绘劕寮跺▓銊︾節閵忥綆娼愭繛鑼枎椤繑绻濆顒傦紲濠电偛妫欑敮鎺楀储閿熺姵鐓涢悗锝呭悁缁ㄥジ鏌曢崶褍顏鐐差儏閳规垿宕堕懜纰夌礂濠德板€楁慨鐑藉磻閻愬搫绀夋繛鍡樻尵瀹撲線鏌″鍐ㄥ闁荤喓鎳撻埞鎴︽偐閹绘帩浼€闂佸磭顑曢崐鏍崲濠靛鍋ㄩ梻鍫熺▓閺嬪懎鈹戦悙鏉垮皟闁搞儴鍩栭弲鐐烘⒑閸涘﹣绶遍柛銊ㄦ珪缁嬪顓奸崱娆戭啎闂佺懓顕崑鎰涢妸褎鍠愰柣妤€鐗嗙粭姘舵煛閸涱喚鍙€闁哄本绋戦埥澶愬础閻愬浠岀紓浣鸿檸閸樻悂宕戦幘鍓佺＝闁稿本鑹鹃埀顒勵棑缁牊绗熼埀顒冩闂佹眹鍨哄灞惧緞閹邦剛顔掗梺鐓庢啞椤旀牕顭囬悢鍏尖拺閻熸瑥瀚粈鍐┿亜閺囧棗鎳庨ˉ姘舵煕韫囨艾浜圭紒鐘荤畺閺屾盯鍩勯崗鐙€浜幃鐐烘倷椤戣法绠氶悗鐟板閸犳牠寮稿☉娆嶄簻妞ゅ繐瀚弳锝呪攽閳ュ磭鍩ｇ€规洖宕灃闁告剬鍕枙闂傚倸鍊烽懗鍫曘€佹繝鍌楁瀺闁哄洨鍠撴禒姘節绾版ɑ顫婇柛瀣瀹曠増鎯旈妸鈶╁亾閺冨牆绀冩い鏂挎瑜旈弻娑㈠焺閸愵亝鍣у銈庡€ら崨顖滐紳婵炴挻鑹惧ú銈夊几濞戙垺鐓犳繛鑼额嚙閻忥附銇勯姀鈥冲摵闁轰焦鍔栧鍕節閸曨厼骞€婵犵數濮伴崹鐓庘枖濞戔懞鍥ㄦ綇閳哄啩绗夊┑顔角归崺鏍煕閹寸姷纾藉ù锝咁潠椤忓懏鍙忛柨鏇楀亾闂囧绻濇繝鍌氼伀闁活厽甯￠弻娑㈠煘閹傚濠碉紕鍋戦崐鏍暜婵犲洦鍊块柨鏂垮⒔閻棝鏌涢鐘插姕闁绘挻鐟﹂〃銉╂倷閼碱兛铏庨梺璇茬箰瀵爼銆冮妷鈺傚€烽柛娆忣槸閺嬬娀鎮楀▓鍨灈濠⒀冮叄楠炴垿宕熼姣尖晝鎲告惔銊﹀€垫い鏇楀亾婵﹤顭峰畷鎺戭潩閿濆洦绠欓梻浣呵归鍛偓姘煎墰閸欏懘姊洪幐搴㈢闁稿﹤鎽滅划缁樺鐎涙鍘电紓鍌欓檷閸ㄥ綊寮稿☉銏＄厱闁瑰濮村暩闂侀€炲苯澧紒鐘茬Ч瀹曟洟鏌嗗畵銉ユ处鐎佃偐鈧稒锚娴滄妫呴銏″缂佸鎹囧畷锝夊箻缂佹鍘遍梺闈涱槹閸ㄧ敻鎳熼鐐茬濞寸厧鐡ㄩ埛鎴︽⒑椤愩倕浠滈柤娲诲灡閺呭爼顢涢悙瀵稿幗濠德板€曢崯顐﹀煝閸噥娈介柣鎰絻閺嗭綁鏌℃担瑙勫磳闁诡喒鏅犲畷姗€鐓鐘垫晨闂傚倸鍊烽懗鑸电仚缂備胶绮〃濠傜暦閹达箑绠涢柡澶婄仢閸嬪秵绻涚€电ǹ孝妞ゆ垵妫濋崺娑㈠箣閿旂晫鍘卞┑鐘绘涧濡顢旈埡鍛厓鐟滄粓宕滃▎蹇ヨ€块梺顒€绉撮悞鍨亜閹烘垵鈧骞婂Δ鍛厱闁靛鍨甸幊鎰板几閳ь剟姊婚崒娆戭槮缂傚秴锕よ灋闁秆勵殕閸婂潡鏌ㄩ弴鐐测偓褰掑磻閸屾稓绠鹃柛鈩兩戠亸浼存煟閹惧瓨绀嬮柡宀€鍠栭弻鍥晝閳ь剟鐛弽顐ょ＜闁肩⒈鍓涚敮娑㈡煏閸パ冾伂缂佺姵鐩弫鎰板川椤撶姷娼夐梻鍌欑閹碱偊鎯夋總绋跨獥闁哄稁鍘归埀顑跨閳诲酣骞橀弶鎴烆吋闂備線娼ч悧鍡椕洪妸鈺佸偍妞ゆ牗绋愮换鍡涙煟閹板吀绨婚柍褜鍓氶崹鍨暦瑜版帗鎯為柛锔诲幘閻撴捇姊虹涵鍛涧缂佺姵鍨块幃锟犲灳閹颁胶鍞甸柣鐘叉惈閵堜粙鏁撻悩鍐蹭簻闂佺粯鎸告绋款嚕閻戣姤顥婃い鎰╁灪婢跺嫭绻涢崣澶涜€挎い銏℃崌瀹曞爼顢楁担鍙夊濠电偠鎻徊鍧楁偤閺冨牆鍚规繛鍡樺姈閸欏繑鎱ㄥΔ鈧悧蹇涙偩閻㈠憡鐓ユ繝闈涚墕娴狅妇鈧灚婢樼€氼厾鎹㈠☉銏犵闁瑰鍋為崕鎾澄斿Δ濠佺胺闁告鍟块悾鐑芥偂鎼存ɑ鏂€闂佹悶鍎弬渚€宕戦幘娲绘晢闁告洦鍓涢崢浠嬫⒑缂佹◤顏勵嚕閸洖绠归柟閭﹀幒缁诲棙銇勯弽銊у暡闂婎剦鍓涢埀顒冾潐濞叉粓寮拠宸殨闁圭虎鍠楅崐鍫曟偣閸ヮ亜鐨虹紒澶嬫そ閺岀喖顢欑憴鍕彅濡炪倖鏌ㄧ换姗€銆佸▎鎾村亗閹肩补妲呭Λ鐔兼⒒閸屾艾鈧嘲霉閸パ屾禆闁靛ě鍛劶婵炶揪缍€椤曟艾煤椤忓秵鏅ｉ梺闈涚箳婵箖骞楅弴銏♀拺闁圭ǹ娴风粻鎾淬亜閿旇鐏ｇ紒顔款嚙閳藉鈻庡鍕泿闂備礁婀遍崑鎾汇€冮崨鏉戠柈妞ゆ牗鍩冮弸宥嗙箾閹存瑥鐏柍閿嬪笒闇夐柨婵嗘噺閸熺偤鏌熼崣澶嬪€愰柡灞剧〒閳ь剨缍嗛崜娆撳几濞戞埃鍋撳▓鍨珮闁稿妫濋獮蹇涙偐瀹割喖鏅犲銈嗘⒒閺咁偆绮欐担铏圭＝闁稿本鑹鹃埀顒傚厴閹偤鏁冩担瑙勫櫡婵犵數濮甸鏍垂闁秴绠伴柟鎯版閽冪喓鎲搁幋鐘典笉婵炴垯鍨归崡鎶芥煟閺冨牊鏁遍柣锝庡墰缁辨捇宕掑▎鎴濆濡炪們鍔嶉崝妤呭箲閵忕姭鏀介柛鈾€鏅滅紞搴♀攽閻愬弶鈻曞ù婊勭矌缁槒銇愰幒鎾跺幗闂佸綊鍋婇崢鑲╁緤缂佹ǜ浜滄い鎺嗗亾闁绘鎸搁～蹇撁洪鍕獩婵犵數濮撮幊搴ㄋ夊┑鍡╂富闁靛牆妫楅悘銉︺亜閿旂偓鏆柟顔诲嵆椤㈡岸鍩€椤掆偓閻ｉ攱绺界粙璇俱劑鏌曟径濠勫哺闁哄懐濞€瀵顓奸崱妯侯潯闂佽顔栭崰鏍敋闁秵鍋℃繝濠傚暟鏁堥梺鐐藉劵婵″洭骞戦崟顖毼╅柨鏇楀亾缁剧虎鍨跺铏圭磼濡⒈鏆″┑鐐插悑閻燂箓骞堥妸銉㈡闁靛繆妾ч幏娲⒑閸涘﹦绠撻悗姘煎枛椤洭濡舵径瀣幈闂侀潧顧€缁茶姤淇婇悜鑺ョ厵闂佸灝顑嗛妵婵嬫煙椤旂厧妲绘顏冨嵆瀹曘劑顢樿濮ｅ姊哄Ч鍥х労闁搞劑浜堕妴鍐╃節閸パ勭€悗骞垮劚椤︿即宕戦崟顖涚厱闊洦鑹炬禍褰掓煕濡湱鐭欐慨濠呮濞戠敻宕ㄩ鍛摋闂備胶枪椤戝洭宕伴弽顓炵畺濡わ絽鍟崑銊╂煕濞戞﹫鍔熼柛姗€娼ч—鍐Χ閸℃鍙嗛梺鍛娚戠划鎾崇暦閹达箑绠婚悹鍥у级椤ユ繈姊洪棃娑辨閻庨潧鑻…鍥籍閸啿鎷绘繛鎾村焹閸嬫挻绻涙担鍐插濞堜粙鐓崶銊︾妞ゎ偅娲熼弻锟犲炊閵夈儳浠奸梺绋胯閸旀垵顫忓ú顏嶆晢闁逞屽墰缁棃鎮介崨濠備簵闂佺粯妫侀崑鎰板矗閹剧粯鐓曢柕澶涚到婵＄晫绱掗埀顒勫焵椤掍胶绠鹃悗鐢登规牎闁汇埄鍨抽崑鐔肺ｉ幇鏉跨婵°倓绀佹禍褰掓倵鐟欏嫭绀€婵炶绠撳畷鎶藉级濞嗙偓瀵岄梺闈涚墕閸燁偊鎮橀鍛箚妞ゆ劑鍨归弳娆撴煟閿濆洤鍘撮柟顔瑰墲閹柨螣缂佹ɑ婢戦梻鍌欑婢瑰﹪宕戦崨顖涘床闁稿本绻嶉弶鐑樼節閻㈤潧啸妞わ綆鍠氬Σ鎰板即閵忕姷锛涢梺纭呮彧缁犳垹澹曡ぐ鎺撶厸鐎广儱楠告禍婵嬫煛閸℃鐭婇柍瑙勫灴閹晠宕ｆ径濠庢П闂備焦鎮堕崐鏇犫偓姘嵆瀵濡堕崶鈺冪厯闁荤姵浜介崝瀣垝闂堟稈鏀介柣鎰级閸ｈ棄鈹戦悙鈺佷壕婵犳鍠栭敃銉ヮ渻娴犲鈧礁螖閸涱厾鍔﹀銈嗗笒閸婂鎯岄幘缁樼厽闁靛繒濮甸崯鐐烘煕鐎ｎ亜鈧潡寮诲☉銏犵疀闁稿繐鎽滈崙褰掓⒑閸濄儱校闁告梹鐗滈幑銏犫攽閸モ晝鐦堥梺绋挎湰缁矂銆傞搹鍦＝濞达絾褰冩禍楣冩⒑閸撴彃浜為柛鐘冲姍瀹曪綀绠涢弬鍓х畾濡炪倖鐗楀銊︾閵忋倖鐓熼柟鐐綑婵秹鏌＄仦鍓ф创濠碘€崇埣瀹曘劑顢涘⿰鍐ㄐ熼梻鍌欑閹碱偊寮甸鍕剮妞ゆ牜鍋熷畵浣逛繆椤栨瑧绉块柡鍐ㄧ墕閻掑灚銇勯幒鎴濐仼缂佺姷濞€閺岀喖寮堕崹顔肩导闂佹悶鍎洪崜锕傚极瀹ュ棛绠鹃柟瀵稿仦閻ㄦ垿鏌ｉ敃鈧悧鎾愁潖閸濆嫅褔宕惰娴煎牆鈹戦悙鏉垮皟闁告劦浜濋幊鍐╃節閻㈤潧啸闁轰焦鎮傚畷鎴濃槈閵忊€冲壋婵炴潙鍚嬪娆戝鐟欏嫮绡€闂傚牊渚楅崕蹇曠磼閻欌偓閸ㄥ爼寮婚悢鍛婄秶濡わ絽鍟宥夋⒑缁嬫鍎忛悗姘嵆瀵鈽夊⿰鍛澑濠殿喗锕╅崗娑樞уΔ鍛拺闁告稑饪垫笟娑樏瑰⿰鍕畼闁瑰箍鍨归埞鎴犫偓锝庡亜娴犳椽姊婚崒姘卞缂佸鍔欓獮瀣倻閼恒儮鎷虹紓浣割儐椤戞瑩宕曢幇鐗堢厵闁告稑锕ラ崐鎰版煕閳瑰灝鐏╂い鎾冲悑瀵板嫭绻濋崒婵冨亾閸喒鏀介柣鎰綑閻忥箓鏌ｉ悢婵嗘搐閸屻劑姊洪鈧粔鐢告偂閻旂厧绠归柟纰卞幖閻忥絿绱掓径鎰锭闂囧绻濇繝鍌氼伀闁活厽甯楅妵鍕閳╁喚妫冨銈冨灪閿曘垺鎱ㄩ埀顒勬煥濞戞ê顏╂鐐村姍濮婅櫣鎷犻懠顒傤唺闂佺ǹ顑嗙粙鎺楀疾閼哥數顩烽悗锛扁偓閸嬫捇寮介鐔锋異闂佸啿鎼崯浼存晬濠婂牊鈷戠紓浣光棨椤忓棗顥氭い鎾朵紳閾忓厜鍋撳☉娆欎緵婵炲牅绮欓弻锝夊箛椤栨稑娑ч梺鍏兼緲濞硷繝寮婚悢鍏煎亗閹艰揪绲挎婵＄偑鍊戦崹鍝劽洪悢鐓庢槬婵°倕鎳庨悡娑樏归敐鍛Щ妞ゃ儲纰嶆穱濠囨倷椤忓嫧鍋撻弽顓熷亱婵犲﹤鐗嗙壕濠氭煟閹邦剚鎯堥柣鎾存礋閺岀喐娼忔ィ鍐╊€嶉梺鎶芥敱鐢繝寮诲☉銏╂晝闁挎繂娴傞弳鈥斥攽閻愬弶鍣规繛灞傚妿濡叉劙骞掗弬鍝勪壕闁挎繂楠稿楣冩煕閵堝棛鎳囬柡灞界Ф閹叉挳宕熼銈勭礉闂備浇顕栭崰妤呮偡瑜旈獮蹇涙偐娓氼垱些濠电偞娼欓崥瀣礉閺団懇鈧棃宕橀鍢壯囧箹缁厜鍋撻懠顒傛晨缂傚倸鍊烽懗鍓佸垝椤栫偛绀夋俊銈呮噹閻鏌涘☉鍗炲季婵炴挸顭烽弻鏇㈠醇濠靛洤娅х紓浣哄С閸楁娊骞冨Δ鈧～婵嬫⒒鐎靛憡鏅兼繝纰樷偓鍐茬骇闁告梹鐟ラ悾閿嬬附缁嬪灝宓嗛梺缁樻煥瀵墎鈧艾銈稿缁樻媴閸涘﹤鏆堢紓浣割儐閸ㄥ潡寮崘顔嘉ㄩ柨鏇楀亾缂佸墎鍋ら弻娑㈠即閵娿儳浠梺缁樻尰濞叉ɑ绌辨繝鍥舵晬婵犻潧妫楅幆鐐烘⒑濞茶骞楅悽顖涘浮閸╃偤骞嬮敂缁樻櫓闂佽崵鍠栭。锔界珶閺囥垺鈷掗柛灞剧懅閸斿秹鎮楃粭娑樻噽閻瑩鏌熺€电ǹ袥闁稿鎸搁～婵嬫偂鎼淬垻褰庢俊鐐€戦崹娲偡閳轰緡鍤曢柟缁㈠枛椤懘鏌嶉柨顖氫壕闂佸綊鏀卞钘夘潖濞差亜宸濆┑鐘插閻ｇ敻鏌ｆ惔銏ｅ闁搞垺褰冨畵鍕偡濠婂懎顣奸悽顖涱殜閹繝寮撮姀锛勫帾婵犵數鍊崘顭戜痪濠殿噯绲介悧鍡涒€旈崘顔嘉ч柛鎰╁妿娴犳儳鈹戦埥鍡椾簻闁哥喐鎸虫俊瀛樼瑹閳ь剙顕ｉ幘顔碱潊闁斥晛鍟悵鎶芥⒒娴ｅ憡鍟炴繛璇х畵瀹曘垺銈ｉ崘鈺佷患婵°倧绲介崯顖炲煕閹烘嚚褰掓晲閸涱喖鏆堥梺鍝ュ枔閸嬨倝寮婚敐澶嬪亜闁哄鍨垫慨锕傛煢濡崵绠栭柕鍥у瀵粙濡歌婵洭姊洪崨濠勬噧缂佺粯锕㈠璇测槈閵忕姈銊︺亜閺冨倸甯舵い顐邯濮婅櫣绱掑Ο璇茬闂佸摜鍠愰幐鎶界嵁閸愩劎鏆嬮柟浣冩珪閻庡妫呴銏″闁瑰皷鏅滅粋鎺撶附閸涘ň鎷洪柣鐘叉穿鐏忔瑧绮婚悧鍫涗簻闁哄洢鍔屽顕€鏌涢埞鎯т壕婵＄偑鍊栫敮鎺楀窗濮橆剦鐒介柟閭﹀幘缁犻箖鏌涘▎蹇ｆШ濠⒀呮暬閺岀喖顢欓崫鍕缂備浇娅ｉ弲顐ゅ垝濞嗘挸绠伴幖绮规嚕濮樿埖鈷掑ù锝堫潐閻忛亶鏌￠崨顔炬创鐎规洦鍨堕獮宥夘敊閻撳巩姘舵⒑闁偛鑻晶瀛樻叏婵犲啯銇濈€规洏鍔嶇换婵嬪磼濮ｆ寧娲樼换娑氣偓娑欘焽閻銇勯妸銉伐闁伙絿鍏橀獮瀣晝閳ь剛绮婚搹顐＄箚闁靛牆瀚澶愭煙瀹勯偊鐓兼慨濠呮缁瑩骞愭惔銏″闂備胶鍘х紞濠勭不閺嶎厼鏄ラ柍褜鍓氶妵鍕箳閹存繍浼屽┑鈽嗗亝閸ㄥ墎鎹㈠☉銏犲耿闁归偊鍓涙禒鐓庮渻閵堝骸浜濈紒璇插楠炴垿宕熼娑欏劒闂佽崵鍠愬妯款杺濠电姷顣槐鏇㈠磻閹达箑纾归柡宥庡亝閺嗘粓鏌熼悜姗嗘當缁炬儳缍婇弻娑樷槈濮楀牆濮涘銈傛櫊閺€杈╂崲濠靛洨绡€闁稿本绮岄·鈧梻浣虹帛閹哥偓鎱ㄩ悽鍨床婵炴垯鍨洪崵鎴澪涢悧鍫㈢畵婵炲牜鍙冨铏规嫚閺屻儳宕紓浣虹帛缁诲牆顕ｆ繝姘櫢闁绘ɑ褰冪粣娑橆渻閵堝棙灏靛┑顔芥尦閹繝鎮㈡總澶嬪瘜闂侀潧鐗嗛崯顐︽倶椤忓棛纾奸悗锝庡亜閻忔挳鏌曢崼顐＄凹閻庨潧銈稿鍫曞箣濠靛牏宕哄┑锛勫亼閸婃牠鎮ч幘璇茬９婵°倓鐒﹂崕鐔封攽閻樺弶澶勯柍閿嬪笒闇夐柨婵嗘川閹藉倿鏌涢妶鍡欐噰闁哄本鐩獮瀣偐閾忣偆褰х紓鍌欐祰娴滎剟宕戦妶鍛殾闁告鍊ｉ悢鍝勬瀳閺夊牄鍔戦弫婊堟⒒閸屾瑨鍏岀紒顕呭灦瀹曠銇愰幒鎴犳煣濡炪倖鍔х粻鎴︽儗濡ゅ懏鐓涢柛銉㈡櫅閺嬪倸霉濠婂嫮鐭掗柡宀€鍠栭幃婊兾熼搹閫涙樊闂備線鈧偛鑻晶浼存煛娴ｇ瓔鍤欓柣锝囧厴楠炲鏁冮埀顒傜不婵犳碍鐓犻柟闂寸劍濞懷囨煛鐎ｎ亜鈧灝顫忓ú顏勫窛濠电姴鍟ˇ鈺呮⒑閸涘﹥灏伴柣鈺婂灥濡喖姊洪棃娑辨Т闁哄懏绮撻崺娑㈠箣閻樺灚锛忓銈嗘尵閸嬬偤宕抽崷顓熷仏婵ǹ浜壕钘夈€掑顒佹悙闁哄绋撶槐鎺撳緞婵犲偆鏆㈤梺鍛婂笚鐢繝宕洪敓鐘插窛妞ゆ棃妫跨花濠氭⒒娴ｈ櫣甯涢柟绋挎啞椤ㄣ儳绮欑捄銊︽闂佺硶鍓濈粙鎺楀煕閹达附鈷掗柛顐ゅ枔閵嗘帞绱掗悩鎻掓毐闁宠鍨块、娆撳箚瑜嶉獮瀣⒑鐠団€虫灈婵炲皷鈧磭鏆﹂柣鏃傗拡閺佸洭鏌ｅΟ纰辨殰缂佸崬鍟块埞鎴︽倷閼搁潧娑х紓浣稿级鐎笛囨倶濞嗘垹纾藉ù锝呮惈閻濓繝鏌涢妷锝呭闁告ü绮欏娲礃閸欏鍎撻梺鎸庢磸閸婃繈骞冨鈧幃鈺呮倷閹存帞鐩庢俊鐐€栭崝鎴﹀垂閼姐倗涓嶉柤纰卞墰绾捐偐绱撴担璇＄劷缂佺姵鎸婚妵鍕敃閿濆洨鐤勫銈冨灪椤ㄥ﹤鐣烽幒妤佹櫆闁诡垎鍛惛婵犵數濮烽弫鍛婃叏閻戣棄鏋侀柟闂寸绾剧粯绻涢幋鐑嗙劯婵炴垶菤閺嬪酣鏌熼幆褏锛嶆い锔诲枛閳规垶骞婇柛濠冩礋楠炲﹥鎯旈埄鍐嚱闂傚倸鍊风粈渚€骞栭位鍥敍濠婂棗小闂備緡鍓欑粔瀵哥磼閳轰急褰掓偐瀹割喖鍓遍梺缁樻尵閸犳牠寮婚悢鍏肩劷闁挎洍鍋撳褜鍨堕弻宥囩磼濡崵顦ㄦ繛锝呮搐閿曨亪骞婇悩绛圭矗婵犻潧娲ㄧ粔椋庣磽閸屾瑨鍏岀紒顕呭灦閹嫰顢涢悙鑼枃闂佺粯蓱濡炲潡寮崱娑欑厓鐟滄粓宕滈悢濂夊殨闁告稑锕ラ崕鐔兼煏婵犲繐顩柛妯圭矙濮婃椽骞愭惔锝囩暤濡炪倧缂氶崡鍐差嚕椤愶附鐒肩€广儱妫楅埀顒傛暬濮婂宕奸悢琛℃（闂佺ǹ顑冮崹濠氬焵椤掍緡鍟忛柛鐘崇墵閹儲绺介崫銉ョウ闁诲函缍嗛崰妤呭磻閹扮増鐓曟い顓熷灥娴滄繄绱掓０婵嗕喊婵﹤顭峰畷鎺戭潩椤戣棄浜惧瀣捣閻棗銆掑锝呬壕濡ょ姷鍋為悧鐘汇€侀弴姘辩Т闂佹悶鍎洪崜锕傚极瀹ュ鐓熼柟閭﹀幗缂嶆垿鏌嶈閸撴繈宕洪弽顐ｅ床婵犻潧顑嗛ˉ鍫熺箾閹存繂鑸归柛鎾插嵆濮婃椽宕ㄦ繝鍛棟缂傚倸绉撮敃顏勵嚕婵犳碍鍋勯悶娑掆偓鍏呭闂佹眹鍨诲▍銉ф喆閸曨収娴勯悷婊冪箳濡叉劙骞掑Δ鈧悡銏ゆ煃瑜滈崜鐔风暦閹达箑绠荤紓浣骨氶幏缁樼箾鏉堝墽鍒版繝鈧崡鐑囪€垮ù鐘差儐閻撴洟鏌曟繛鍨妞ゃ儱顦伴妵鍕敇閻愬鈹涘銈忛檮婵炲﹪寮婚敐鍛傛棃鍩€椤掑嫭鐓€闁挎繂鎷嬪鏍磽娴ｈ偂鎴炲垔閹绢喗鐓㈡俊顖欒濡查攱鎱ㄧ憴鍕垫疁婵﹨娅ｅ☉鐢稿川椤旀儳娑ч梻浣告贡鏋い顓炲槻椤曪綁寮婚妷銉╁敹闂佸搫娲ㄩ崰搴㈢婵傚憡鐓熼煫鍥ㄦ礀娴犫晜淇婇銏狀伃闁诡喗蓱缁绘繈宕橀鍡欐闂備焦鐪归崹钘夘焽瑜庨悧搴ㄦ⒒娴ｅ憡鍟為柛銊ф暩缁辩偞绻濋崒銈呮濡炪倖鍔戦崺鍕触鐎ｎ喗鐓曢柍鈺佸暟閹冲懘鏌ｉ敃鈧悧濠勬崲濠靛鍋ㄩ梻鍫熷垁閵忋倖鐓曞┑鐘插€荤粔铏光偓瑙勬礉濞呮洜绮嬮幒鏂哄亾閿濆骸浜為柛娆忔閳规垿鎮欓弶鎴犱桓闂佽崵鍟欓崶銊ュ壒闂佸湱鍎ら〃鍡涙偂濞戞埃鍋撻崗澶婁壕闁诲函缍嗛崜娑溾叢缂傚倸鍊烽懗鑸垫叏閻㈠憡鍎庢い鏍ㄥ嚬閸ゆ洟鏌熺紒銏犳灍闁稿﹦鍏橀弻锝呂旈埀顒勬偋閸℃稒鍋傞柡鍥ュ灪閳锋帡鏌涚仦鍓ф噮妞わ讣绠撻弻鐔哄枈閸楃偘鍠婂Δ鐘靛仜缁绘帡鍩€椤掑﹦绉甸柛鐘崇墱婢规洜鎷犲ù瀣杸闂佺粯锚瀵爼骞栭幇顔剧＜闁绘ê纾晶鐢告煛鐏炵ǹ澧茬€垫澘瀚埀顒婄秮濞佳囧礉瀹€鍕拺闁硅偐鍋涢埀顒佺墪鐓ら柣鏃傚帶缁犳牗淇婇妶鍛櫤闁稿绻濋弻鐔封枔閸喗鐏撻悗瑙勬处閸嬪﹤顫忔繝姘＜婵﹩鍏橀崑鎾诲箹娴ｅ摜锛欓梺褰掓？缁€浣哄瑜版帗鐓欓梻鍌氼嚟椤︼妇鐥幆褏绉洪柡宀€鍠栧鑽も偓闈涘濡差喚绱掗悙顒€鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆撴倻濡桨鐢绘繝鐢靛Т閿曘倝宕板顑╃喖鍩€椤掑嫭鍊垫繛鍫濈仢閺嬬喖鏌熼鐓庘偓鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸閻忕偟纭堕崑鎾崇暦閸ャ劍鐣烽梻渚€鈧偛鑻晶瀵糕偓瑙勬磻閸楀啿顕ｆ禒瀣垫晣闁绘劗鏁搁弳顐︽煟閻斿摜鐭嬮柡灞诲姂閹箖鏌ㄧ€ｎ剟妾梺鍛婄☉閿曘倖绂嶅⿰鍫熲拺闁告稑锕︾粻鎾绘倵濮樼厧澧柍缁樻崌婵＄兘鍩￠崒妤佸闂備胶枪閺堫剟鎮疯钘濋柨鏇炲€归悡娆撴偣閸ュ洤鎳愰惁鍫ユ倵鐟欏嫭绀€婵犫偓闁秴绠熼柟缁㈠枛缁€瀣亜閹哄棗浜鹃悗娈垮枛濞尖€愁潖濞差亜鎹舵い鎾墲椤庢姊洪崫銉バｉ柣妤冨█楠炲啴鍨鹃幇浣瑰缓闂侀€炲苯澧寸€殿喖顭烽幃銏ゅ川婵犲嫮肖濠德板€х徊浠嬪疮椤栫儐鏁佺€广儱顦伴埛鎴犵磼鐎ｎ偒鍎ラ柛搴㈠姍閺屾盯寮埀顒勬偡閳哄懐宓侀柛鎰╁壆閺冨牆宸濇い鎾跺枎缁佽埖淇婇悙顏勨偓鏍箰妤ｅ啫纾规い鎰剁畱椤懘骞掗搹顐ゎ洸婵犲﹤鐗婇悡蹇擃熆閼哥數鈽夋い鈺婂墰缁辨帡鎮╅幇浣哄姺缂備浇椴哥敮妤€顭囪箛娑樜╅柨鏇楀亾闂傚绉瑰娲偡閺夋寧些闂佺娅曢敋闁伙絿鍏橀、鏇㈡晝閳ь剛绮堢€ｎ剛妫柣妤€鐓鍛洸闁告挆鈧崑鎾诲礂婢跺﹣澹曢梻浣告啞濞诧箓宕滃☉銏犲偍闂侇剙绉甸埛鎴︽⒒閸喕鍎愮憸鐗堝笒绾惧潡鏌熼幆鐗堫棄闁哄嫨鍎甸弻銊モ攽閸℃顦遍梺绋款儐閹告悂鍩㈤幘璇插瀭妞ゆ梻鏅禍顏呬繆閻愵亜鈧倝宕㈡ィ鍐ㄧ婵☆垯璀﹂崵鏇熴亜閹板墎鐣辩紒鐘崇⊕閵囧嫰骞樼捄杞版埛缂備焦鍔楅崑鐐垫崲濠靛鍋ㄩ梻鍫熷垁閵忋倖鍊垫慨妯哄船閸樺鈧娲忛崕闈涚暦閵娧€鍋撳☉娅辨岸骞忕紒妯肩閺夊牆澧介幃濂告煙閾忣偅宕屾い銏¤壘楗即宕ㄩ婊愮床闂佽崵濮村ú锕併亹閸愵喖鐒垫い鎺嗗亾婵炵》绻濋幃浼搭敊閽樺绐炴繝鐢靛仧閸嬫捇锝炲澶嬧拺闂傚牊绋撶粻鍐测攽椤旀儳鍘存い銏＄懇楠炴帡寮崒婊愮闯闂備胶枪閺堫剟鎮疯瀹曟繂顓奸崱鎰盎濡炪倖鎸鹃崑鐐哄窗濡皷鍋撶憴鍕８闁搞劋绮欓悰顕€寮介妸锕€顎撻梺绋跨箰椤︽壆鈧艾銈稿缁樻媴閸涘﹤鏆堢紓浣割儐閸ㄥ潡寮崘顔芥櫜闁搞儺鐓堥崑銊╂⒒娓氬洤澧紒澶屾暬閹繝寮撮姀锛勫幗濠碘槅鍨甸褎鏅堕娑氱闁割偆鍠撻惌宀勬煃瑜滈崜婵嬶綖婢跺⊕鍝勵潨閳ь剙鐣峰┑鍡忔瀻闁规崘娉涚粊锕傛⒑閹肩偛鍔撮柛鎾村哺瀹曟垹鈧綆鍠楅悡鏇熴亜閹邦喖孝闁诲繆鏅滈〃銉╂倷閺夋垵顫掗梺鍝勭焿缁查箖骞嗛弮鍫濈伋闁告劘灏欢鐔兼⒒娴ｅ懙褰掝敄閸℃稑绠查柛銉墮缁犳牗淇婇妶鍛櫤闁哄懏鐓￠獮鏍垝閸忓浜鹃柟棰佺劍妤犲嫰姊婚崒娆愵樂缂侀硸鍠氱划濠氬箣閿旇棄浜遍梺瑙勫礃椤曆呯不閺嶃劋绻嗛柕鍫濇噹閺嗙偤鏌涢悩鍙夘棦闁哄本鐩鎾Ω閵夈儺娼炬俊鐐€х拹鐔煎礉閹达箑钃熼柨婵嗩槸椤懘鏌嶆潪鎷屽厡濞寸媭鍙冮弻锝夊閳轰胶浠梺鍝ュУ閻楃娀鐛崘顭戞建闁逞屽墴閻涱噣骞掗幊铏⒐閹峰懘宕崟顐ゎ啈闂傚倸鍊烽懗鍓佸垝椤栫偛绀夐柡鍥ュ焺閺佸銇勯幘鍗炵仼闁绘挻绻堥弻鐔煎礈瑜忕敮娑㈡煟閹惧啿鏆熼柟鑼归オ浼村醇濠靛牜妲舵繝鐢靛仜濡瑩骞愰幖浣哥＜闁靛ň鏅滈悡娑氣偓骞垮劚閸燁偅淇婇崸妤佺厽婵犻潧妫涢崺锝夋煛瀹€瀣瘈鐎规洖鐖兼俊鐑藉Ψ瑜岄惀顏堟⒒娴ｇ懓鈻曢柡鈧柆宥呭瀭闁秆勵殔閽冪喐绻涢幋鐐冩岸寮ㄦ禒瀣厱闁斥晛鍠氬▓銏犆圭亸鏍т壕闂傚倸鍊风粈渚€鎮块崶顒夋晪鐟滄棁妫熼梺姹囧灮鏋柦鍐枑缁绘盯骞嬮悙鑼姲闂佺ǹ顑嗛幑鍥极瀹ュ纭€闁绘劕寮跺▓銊︾節閵忥綆娼愭繛鑼枎椤繑绻濆顒傦紲濠电偛妫欑敮鎺楀储閿熺姵鐓涢悗锝呭悁缁ㄥジ鏌曢崶褍顏鐐差儏閳规垿宕堕懜纰夌礂濠德板€楁慨鐑藉磻閻愬搫绀夋繛鍡樻尵瀹撲線鏌″鍐ㄥ闁荤喓鎳撻埞鎴︽偐閹绘帩浼€闂佸磭顑曢崐鏍崲濠靛鍋ㄩ梻鍫熺▓閺嬪懎鈹戦悙鏉垮皟闁搞儴鍩栭弲鐐烘⒑閸︻厼顣兼繝銏★耿瀵憡绗熼埀顒勫蓟濞戙垹鐒洪柛鎰典簴婵洭姊虹紒妯诲鞍婵炶尙鍠栭獮鍐潨閳ь剟骞冨▎鎾崇闁瑰搫妫欑€垫牠姊绘担鑺ャ€冮柣鎺炵畵閹囨偐鐠囪尪鎽曢梺鍝勬川閸犳挾绮绘ィ鍐╃厽闁逛即娼ф晶顖炴煟濠靛洦绀嬫慨濠冩そ楠炲酣鎳為妷锔芥闂備焦鎮堕崝灞筋焽閳ユ剚鍤曢悹鍥ㄧゴ濡插牊淇婇鐐存暠闁哄倵鍋撻梻鍌欒兌绾爼宕滃┑瀣ㄢ偓鍐疀閺傛鍤ら梺绋挎湰閸戠懓銆掓繝姘厪闁割偅绻堥妤€霉濠婂啫鈷旂紒杈ㄥ浮閹晠鎼归銏紦闁诲孩顔栭崰娑㈩敋瑜旈崺銉﹀緞閹邦剦娼婇梺缁橆焽閺佺ǹ顭囨径鎰拻濞撴埃鍋撻柍褜鍓涢崑娑㈡嚐椤栨稒娅犻悗鐢电《閸嬫挾鎲撮崟顒傤槬缂傚倸绉撮敃銈夋偩妞嬪海鐭欓悹鍥╁亾濡啫鐣峰鈧俊姝岊槹闁稿锕濠氬磼濞嗘埈妲梺纭咁嚋缁绘繂鐣峰┑瀣櫇闁稿本姘ㄩ悰銉╂⒑閸濆嫮鈻夐柛娆忕箳缁辩偤寮介鐔哄幈濠电偞鍨堕…鍥箺閻樼粯鐓熼柟鎹愭硾閺嬫垵菐閸パ嶈含妞ゃ垺绋戦埞鎴﹀炊閺堥潧鎽嬫繝鐢靛仜椤曨厽鎱ㄩ悽鐢电煓闁圭儤姊瑰畷鍙夌箾閹存瑥鐏╅梺鍗炴喘閺岋綁寮崶顭戜哗闂佹眹鍊曠€氼剟鍩為幋锔绘晩缁绢參鏀遍弫鎯р攽閿涘嫬浠╂俊顐㈠閹箖鎮滅粵瀣櫇闂佹寧娲嶉崑鎾剁磼閳ь剛鈧綆鍠楅悡鏇熴亜椤撶喎鐏ラ柍褜鍓ㄧ紞渚€銆侀弮鍫濈闁靛⿵闄勯弶鎼佹⒒娴ｈ櫣甯涢柛鏃€娲栬灒濠电姴浼ｉ敐鍥ㄥ枂闁告洦鍘鹃鏇㈡⒑閸涘﹦鎳冩い锕備憾閹本绻濋崶褏顢呴梻渚囧墮缁夌敻鎮″▎鎾寸厵婵炲牆鐏濋弸娑欍亜閿濆懌鍋㈤柡宀嬬節瀹曘劑顢欓崜褏鏉介梻浣告惈閺堫剛绮欓幘瀵割浄闁挎洖鍊哥粈鍫㈡喐婢舵劕鍑犻柡鍐ㄧ墛閳锋垹鎲搁悧鍫濅刊婵☆偅鍨圭槐鎺楊敋閸涱厾浠梺鐟扮畭閸ㄥ綊鍩ユ径濞炬瀻闁归偊鍠氳倴闂傚倷绀侀幉锟犲箰閸℃稑绀冮柕濞у啫绗撻梻鍌氬€搁崐鐑芥嚄閸洏鈧焦绻濋崘鈺佸伎闂侀潧鐗嗗ú銈夊垂濠靛鐓欓柣鎰靛墮婢ь垶鏌涢妶鍡樼闁哄瞼鍠栧鑽も偓闈涘濡差喚绱撴担鎻掍壕闂佸憡鍔﹂崰妤呮偂閸愵喗鍋犳繛鎴炲坊閸嬫捇宕楅崨顓ф缂傚倸鍊搁崐宄懊归崶顒夋晪鐟滃秷鐏嬪┑鐐叉閹稿憡顢婇梻浣告啞濞诧箓宕归幍顔句笉婵炴垯鍨洪悡鍐级閻愭潙顎滈柛蹇撹嫰闇夐柣姗€娼х敮鑸点亜椤忓嫬鏆ｅ┑鈥崇埣瀹曘劑顢欑紒銏＄倞闂傚倷绀侀幉锛勬箒缂備緡鍠楅悷鈺備繆閹绢喖绀冩い鏂挎閵娾晜鐓冮柛婵嗗閳ь剚鎮傞崺鈧い鎺戝€搁崢鎾煙椤旇偐绉虹€规洖鐖煎畷鎯邦槼鐎瑰憡绻堥弻娑樷枎韫囨泤銈囩磼鏉堛劍宕岀€规洘甯掗埢搴ㄥ箳閹存繂鑵愭繝鐢靛У椤旀牠宕板璺烘瀬濠电姵鑹剧粻姘舵煕閺囥劌鐏遍柡浣哥У閹便劌螣閻撳骸浠樺┑鐐茬墑閸旀垵顫忓ú顏勪紶闁告洖鐏氭晥闂備焦鎮堕崐褏绮婚幘鎰佸殨妞ゆ洍鍋撶€规洜鍘ч埞鎴﹀醇濠靛柊鏇炩攽閿涘嫬浜奸柛濠冪墱閺侇喗绻濋崶銊ユ畱闂佸壊鍋呭ú蹇涘焵椤掆偓閹虫ê顕ｆ繝姘ㄩ柨鏃€鍎抽獮妤呮⒒娓氣偓閳ь剚绋戝畵鍡樼箾娴ｅ啿瀚▍鐘炽亜閺嶎偄浠﹂柣鎾存礃缁绘盯宕卞Δ鍐唺缂備胶濮佃ぐ鍐絹闂佹悶鍎滃鍫濇儓婵＄偑鍊戦崹铏圭矙閹达负鍋戝ù鍏兼綑缁€鍐煏婵炲灝鈧鏁☉娆戠瘈闁汇垽娼ф禒婊堟煟鎺抽崝搴ㄥ礆閹烘绠婚柛鎾叉缁楀姊洪崫鍕枆闁告ǹ鍋愮划濠氬冀閵娿倗绠氶梺闈涚墕閸婂憡绂嶉幆褜娓婚柕鍫濋娴滄繄绱掔拠鑼ⅵ妤犵偛鍟埢搴ㄥ箼閸愨晜娅岄梻渚€鈧偛鑻晶瀛樸亜閵忊剝顥堥柡浣规崌閺佹捇鏁撻敓锟�
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
    //濠电姷鏁告慨鐑藉极閸涘﹥鍙忛柣鎴ｆ閺嬩線鏌涘☉姗堟敾闁告瑥绻橀弻锝夊箣閿濆棭妫勯梺鍝勵儎缁舵岸寮诲☉妯锋婵鐗婇弫楣冩⒑閸涘﹦鎳冪紒缁橈耿瀵鏁愭径濠勵吅闂佹寧绻傚Λ顓炍涢崟顖涒拺闁告繂瀚烽崕搴ｇ磼閼搁潧鍝虹€殿喛顕ч埥澶娢熼柨瀣垫綌婵犳鍠楅〃鍛存偋婵犲洤鏋佸Δ锝呭暞閳锋垿鏌涘☉姗堝姛闁瑰啿鍟扮槐鎺旂磼濡搫顫掑┑鐘亾濞撴埃鍋撴慨濠呮缁瑧鎹勯妸褜鍞堕梻浣告啞閼归箖顢栭崶鈺傤潟闁绘劕鎼粻锝夋煥閺冨倹娅曢柛姗€浜跺娲濞淬劌缍婂畷鏇㈡焼瀹ュ懐鍔﹀銈嗗坊閸嬫挻銇勯鐘插幋闁绘侗鍠楀鍕箛椤掑偆鍟囧┑鐐舵彧缁插潡骞婇幘瀵割洸闂侇剙绉甸埛鎴︽煕閹炬潙绲诲ù婊勭箘缁辨帞鎷犻幓鎺濅純闂佽鍠氶崗妯侯嚕婵犳艾唯闁挎棁顫夌€氳偐绱撻崒娆戭槮妞ゆ垵妫濋、鏍р枎閹惧磭锛熼棅顐㈡处閺岋絾绂嶅⿰鍫熺叆闁哄啫鍊告禍楣冩煛閸♀晛澧伴柍褜鍓氶鏍窗閺嶎厸鈧箑鐣￠柇锕€娈ㄩ梺鍦檸閸犳寮查鍕厱闁哄洢鍔岄獮妤佺節閵忊€崇伌婵﹨娅ｉ幏鐘诲箵閹烘垶鐦ｇ紓鍌氬€哥粔鏉懳涘▎鎴犵焿鐎广儱顦崘鈧銈嗘尵閸嬬喖鏁嶅☉銏♀拺閻熸瑥瀚粈鍐┿亜閺囧棗娲ょ涵鈧梺瑙勫劶婵倝鎮￠悢闀愮箚妞ゆ牗鑹鹃幃鎴︽煛閸″繑娅婇柡宀嬬秮楠炴帡骞嬪▎蹇庡垝闂備礁鎼惌澶屽緤妤ｅ啫绠氶柡鍐ㄧ墱閺佸秵绻濇繝鍌涘晽闁挎繂顦伴埛鎴︽煕濠靛嫬鍔氶弽鈥愁渻閵堝啫濡奸柨鏇ㄤ邯閻涱噣宕橀鑺ユ闂佺粯蓱瑜板啫鐣甸崱娑欌拺缂備焦蓱閳锋帞绱掔紒妯肩畼闁奸缚浜幏鐘差啅椤旇棄鐦滈梻渚€娼ч悧鍡椢涘▎鎴滅剨闁汇垹鎲￠悡鏇㈡煟濡崵鍙€闁告瑥瀚伴弻鐔碱敊閺傘倛鈧灝鈹戦敍鍕幋濠碉紕鍏樺畷姗€顢旈崨顓熺暚婵°倖顨忔禍娆撳础閸愯尙鏆﹂柣鏃傗拡閺佸洭鏌ｅΟ铏逛粵婵犮垺鐗犲濠氬磼濞嗘埈妲梺鍦拡閸嬪﹪骞嗘径瀣檮缂佸娉曢悰銉╂⒑閸濆嫮鈻夐柛妯虹秺閸┾偓妞ゆ垼娉曢ˇ锕傛煃鐠囨煡鍙勬鐐达耿瀵爼骞嬪⿰鍕簼闂傚倷娴囬褏鎹㈤幒妤€纾婚柣鎰惈绾惧潡鏌涢幇顓犮偞闁哄绉甸幈銊ヮ潨閸℃绠荤紓浣叉閸嬫挻绻濋悽闈涗粶闁绘妫濋幃妯衡攽鐎ｎ亜鍤戝┑鐐村灟閸ㄦ椽鎮￠妷鈺傜厸闁搞儲婀圭花缁樸亜閳哄﹤澧撮柡灞剧洴楠炴帡骞橀搹顐ョ檨婵＄偑鍊戦崹娲儎椤栫偛绠栨繛鍡樻尰閸ゆ垶銇勯幒鎴Ц闁轰焦鎮傚濠氬磼濞嗘埈妲梺瑙勭ゴ閸撴繄绮悢鑲烘棃宕ㄩ鐐碘偓顓㈡⒑閹勭闁稿鐒︾粋鎺戭潩閼哥數鍘甸梺璇″灡濠㈡ǹ顣块梻浣呵归敃锕傚极婵犳艾钃熼柕濞垮劗閺€浠嬫煕閳╁啰鎳冮柛銈庡墰缁辨挻鎷呴棃娑橆瀳闂侀潧鐗婇幃鍌氼嚕婵犳碍鏅插璺侯儐濞呮粓姊洪崨濠冨闁稿瀚划濠囶敊鐏忔牗鏂€濡炪倖姊婚妴瀣礉閻旇櫣纾兼い鏇炴噹閻忥附顨ラ悙鑼鐎规洏鍔戝鍫曞箣濠靛牏宕烘繝鐢靛Х閺佸憡鎱ㄩ幘顔肩９闁荤喐澹嬮弸鏃€鎱ㄥ璇蹭壕濠殿喖锕︾划顖炲箯閸涱垳椹抽悗锝庡亞缁夌兘姊绘担鍝勫姦闁告挻绻傜叅闁绘梻顑曢埀顑跨椤繈骞囨担鍏夋瀸闂傚倷鐒︾€笛呮崲閸曨垰纾绘繛鎴欏灩閻撴﹢鏌熺€电ǹ袥闁稿鎹囬弫鎰償濡桨澹曞銈嗘尰缁嬫帒鈻嶆繝鍥ㄧ厸閻忕偟鏅牎濠电偟鈷堟禍顏堢嵁韫囨洑娌柦妯侯槸閼垫劕鈹戦敍鍕杭闁稿﹥鐗犲濠氬Ω閳寡冩喘濡啫鈽夋潏銊х▉婵犵數鍋涘Ο濠冪濠靛瑤澶愬醇閻旇櫣顔曢梺鐟邦嚟閸嬬偤鎯冮幋锔界厽闁靛牆鎳庨顓熸叏婵犲懏顏犵紒顔界懇瀹曠娀鍩勯崘鈺傛瘞濠电姷鏁搁崑鐘典焊濞嗘挸绐楅柡宥庡幖缁犳牕霉閸忓吋鍎楅柡浣割儔閺屽秷顧侀柛鎾村哺楠炲牓濡搁埡浣哄姦濡炪倖甯掔€氼參鎮￠妷锔剧闁瑰浼濋鍫晜妞ゆ劧绲绘禍婊堟煛閸愶絽浜惧┑鐐跺皺閸犲酣锝炶箛鎾佹椽顢旈崨顓濈敾闂傚倷绶￠崣蹇曠不閹存績鏋旈柛顐ｆ礃閳锋垹绱掔€ｎ偒鍎ラ柛搴㈠灴閺屾盯鎮╁畷鍥р拰闂佺粯渚楅崰妤冩崲濠靛鐐婄憸蹇浰囬妸銉富闁靛牆妫欓埛鎺楁煛閸滀礁浜扮€规洏鍨介弻鍡楊吋閸″繑瀚奸梺鑽ゅ枑閻熴儳鈧凹鍓涚划鍫ュ礃閳瑰じ绨婚梺鍝勬川閸犳挻鏅堕鍛簻妞ゆ挾鍋為崰妯活殽閻愬弶鍠樻い銏＄墵婵偓闁绘ǹ顕у浼存倵鐟欏嫭绀冮柛搴°偢绡撻柛宀€鍋為ˉ濠冦亜閹烘埈妲稿褜鍣ｉ弻宥囨喆閸曨偆浼岄梺绯曟櫅鐎氭澘鐣峰鈧俊鎼佸Ψ閵夋劧濡囩槐鎾诲磼濞嗘帒鍘℃繝娈垮枤閺佸骞嗛崟顖ｆ晬婵鍘у▓銊╂⒑閸︻叀妾搁柛鐘愁殜瀹曟劙鎮滈懞銉у幘缂佺偓婢樺畷顒佹櫠椤曗偓閹粙顢涘☉娆忕３濠殿喖锕ら…宄扮暦閹烘埈娼╂い鎴ｆ娴滄儳顪冪€ｎ亝鎹ｉ柣顓炴閵嗘帒顫濋敐鍛婵°倗濮烽崑鐐恒€冮崨娣亼濞村吋娼欓柋鍥ㄧ節闂堟侗鍎愰柨娑欘殜濮婄粯绗熼埀顒€顭囪钘濇い鎰剁畱閻ょ偓绻涢幋鐐跺妞ゃ儲宀搁幃褰掑炊閵娧佸仦闂佽　鍋撳ù鐘差儐閻撶喐淇婇婵愬殭缂佹彃顭烽弻宥堫檨闁告挻鐟╅垾锕€鐣￠柇锕€娈ㄩ梺瑙勫劶婵倝寮插┑瀣厱閻忕偛澧介妴鎺楁煕濞嗗繑顥滈柍瑙勫灴閹晝绱掑Ο濠氭暘婵犵妲呴崑鍛存偡閳哄嫭锛傞梻浣虹帛椤洭寮崫銉ヮ棜濠靛倸鎲￠悡鍐⒑濞嗘儳鐏犲ù婊堢畺濮婅櫣绮欏▎鎯у壉闂佸湱鎳撳ú銈夛綖韫囨洜纾兼俊顖濐嚙椤庢捇姊洪崨濠勨槈闁挎洏鍊濆鎶藉醇閵夛腹鎷洪梺鍛婄☉閿曘儵鎮樺┑瀣厱婵炴垵宕悘锝夋煥濞戞瑧绠撻柍瑙勫灴椤㈡瑧鎲撮幒鎴炶础闁告帗甯￠幃娆擃敆娴ｇǹ浼庨梻浣告啞閸旀牜绮婇幘顔肩哗濞寸姴顑嗛悡鐔镐繆椤栨繃顏犻柨娑樼У閵囧嫰鏁愰崱娆忓绩闂佸搫鑻粔褰掑蓟閵娧€鍋撻敐搴濈敖濞寸厧鐭傚娲传閸曨剚鎷辩紓浣割儐閸ㄥ潡宕洪悙鍝勭闁挎洍鍋撴鐐灪娣囧﹪濡堕崪鍐╊暭缂備浇椴搁崹鍧楀蓟閿濆鍊烽柡澶嬪灥濮ｅ牊绻涚€涙鐭ゅù婊勭矒閿濈偠绠涘☉娆愬劒闂侀潻瀵岄崢楣冩偂閹剧粯鈷戠紒澶婃鐎氬嘲鈻撻弮鍌楀亾濞堝灝鏋ゅ褎顨堥幑銏犫槈濮橈絽浜炬繛鎴炵懐閻掍粙鏌ｉ鐑囪含闁哄矉缍佸浠嬵敃閵忕姳绮梻浣风串缁蹭粙宕查懠顒€寮叉俊鐐€曠换鎰偓姘煎櫍瀹曟繈鏁冮埀顒勨€旈崘顔嘉ч柛鈩冾殘閻熸劙姊婚崒姘仼缂佸鏁哥划瀣吋閸涱亜鐗氶梺鍓插亞閸犳捇宕㈤悽鍛娾拺闁告稑锕︾粻鎾绘倵濮樼厧鏋﹂柛濠冩尦濮婃椽鎳￠妶鍛咃綁鏌涢弮鈧〃鍡涘疾鐠轰綍鏃堝礃閳轰礁绨ユ繝鐢靛仦閸垶宕硅ぐ鎺撶厑闁搞儺鍓氶悡鐔兼煛閸ャ劋浜㈤柛蹇撶焸閺屾盯寮崼顐ｎ棖缂備浇椴搁幐濠氬箯閸涱喚顩烽悗锝庝簼閹虫瑩姊哄Ч鍥х労闁搞劎鏅幑銏犫攽鐎ｎ€儱霉閿濆洨銆婇柡瀣叄閺岀喖鎮欓浣虹▏濠电偛鐗婇崝娆忣潖閾忓厜鍋撻崷顓炐ｉ柕鍡楀暟缁辨帞鎷嬮崷顓犵槇閻庢鍠栭悥濂哥嵁閺嶃劍濯撮柛锔诲幖楠炴姊虹涵鍛棈闁规椿浜炲Σ鎰板即閻斾警娴勫┑鐐村灦閻熝呭姬閳ь剟姊洪棃娑氱畾婵℃彃鎳橀幃鐐寸鐎ｎ偆鍘撻悷婊勭矒瀹曟粌顫濈捄铏诡槱閻熸粎澧楃敮鎺旂玻濡や椒绻嗛柕鍫濇噹閺嗙喖鏌ｉ鐑嗘█闁哄瞼鍠栭幃婊冾潨閸℃鏆﹂梻浣告惈閹冲繒鎹㈤崼銉ヨ摕婵炴垶鐭▽顏堟煟閹伴潧澧版繛鍫涘€濆鐑樺濞嗘垹袦缂備胶濮甸悧鐘荤嵁閸愩劎鏆嬮柟闈涘暱閻楁岸姊洪崨濠傚Е闁哥姵鐗曢锝嗗鐎涙ǚ鎷洪柣鐘叉礌閳ь剝娅曢悘宥咁渻閵堝啫濡兼俊顐ｇ箖娣囧﹪宕奸弴鐔告珳婵犮垼娉涢敃锕偹囬妸鈺傗拺闁告劕寮剁拹锟犳煕鐎ｎ偅灏甸柍褜鍓氭穱鍝勎涢崟顖氱厴闁硅揪闄勯崐鐑芥煠閹间焦娑ф繛鎳峰懐纾介柛灞炬皑琚﹂梺绋款儐閹告悂鍩為幋锔藉€烽柛娆忣槸閻濇梻绱撴担鐟扮祷婵炲皷鈧剚鍤曟い鎰跺瘜閺佸﹪鎮樿箛鏃傚妞ゎ偄绉撮埞鎴︽倷閸欏妫炵紓浣割槸绾绢參鎳炴潏銊х瘈婵﹩鍘搁幏娲⒒閸屾氨澧涢柤鍐叉閵囨劙骞掗幋鐙呯吹闂備礁鎼悮顐﹀磿闁秴瑙︾憸鐗堝笚閻撴洟鏌曟径妯烘灈濠⒀屽灦閺岋綁骞樼€靛憡鍣梺闈涙搐鐎氭澘顕ｉ鍌涘珰闁圭粯甯╅悗鎶芥⒑鐠囨彃顒㈤柛鎴濈秺瀹曟澘顫濋澶婃閻熸粍妫冨畷娲焵椤掍降浜滈柟鐑樺灥椤忣亪鏌嶉柨瀣伌闁哄本绋戦埥澶婎潨閸喐鏆板┑鐘灱濞夋稓鈧凹鍓熷﹢浣糕攽閻樿宸ラ柟鍐插缁傛帗娼忛埞鎯т壕閻熸瑥瀚粈鍐磼椤旇姤灏板ǎ鍥э躬楠炴牗鎷呴懖婵勫妽閵囧嫰寮崶顭戞闂佷紮闄勭划鎾愁潖缂佹鐟归柍褜鍓欓…鍥樄闁诡啫鍥у耿婵＄偑鍨虹粙鎴﹀煝鎼淬劌绠ｆ繝濠傛噹婢ч箖姊绘担鍦菇闁搞劌缍婇獮澶愭晸閻樺啿浜楅梺鍝勬储閸ㄥ湱绮荤憴鍕闁挎繂楠告晶顕€鏌ｈ箛濠冩珔闁宠鍨块、娆撴嚃閳诡儸鍥ㄧ厸鐎光偓閳ь剟宕伴弽顓溾偓浣糕枎閹惧厖绱堕梺鍛婃处閸樿櫣妲愬▎鎾寸厽閹兼番鍊ゅ鎰箾閸欏澧柣锝囧厴椤㈡宕熼銈呭箳闂佺懓鍚嬮悾顏堝礉瀹€鈧划璇测槈濞嗗秳绨婚梺鐟版惈缁夌兘宕楀畝鍕厓鐟滄粓宕滈悢铏逛笉闁哄诞灞剧稁缂傚倷鐒﹂…鍥╁姬閳ь剟鏌熼懖鈺勊夐柍褜鍓濈亸娆撴儎鎼淬垻绡€闁汇垽娼ф禒鈺傘亜閺囩喓鐭岀紒顔碱煼楠炲鏁傞懖鈺冣棨闂傚⿴鍋勫ú锕傚传鎼淬垻顩茬憸鐗堝笚閻撴瑩鏌ｉ幋鐏活亪鎮橀埡鍛厽闁圭偓娼欓悘瀛樹繆椤愩垺鍤囨い銏℃礋婵偓闁炽儲鍓氬Σ杈╃磽閸屾瑧顦︽い鎴濇閺侇噣鏁撻悩鍙夋К濠电偞鍨崹鍦不閾忣偂绻嗛柕鍫濆閸忓矂鏌涘Ο鍝勮埞妞ゎ亜鍟存俊鍫曞幢濡も偓椤洭姊虹粙鍖℃敾婵炶尙鍠庨锝囩矙濞嗘儳鎮戦梺鎼炲劗閺呮繈鏁嶅⿰鍫熲拺闁告挻褰冩禍婵囩箾閸欏鐏存鐐差槹閵堬綁宕橀埡鍐ㄥ箰闁诲骸鍘滈崑鎾绘倵閿濆骸澧扮悮锔戒繆閵堝洤啸闁稿鍋熼弫顕€鍨鹃幇浣告闂佸湱铏庨崰妤呭磻閸℃褰掓晲閸偄娈愬┑鈽嗗亜鐎氭澘顫忔繝姘＜婵炲棙鍩堝Σ顕€姊哄Ч鍥р偓鏇灻洪銏℃櫜闁绘劖娼欑欢鐐烘煙闁箑澧柣搴弮濮婅櫣绮旈崱妤€顏存繛鍫熸⒒缁辨帡鎮╁畷鍥р拰闂佸搫鑻粔鐟扮暦椤愶箑绀嬫い鎰剁稻閻や線姊绘担鍛靛綊顢栭崱娑樼闁搞儜鍛闂佺粯鍨归悺鏃堝极閸ヮ剚鐓熼柟閭﹀幖婵倿鏌涢妸锔姐仢濠碉紕鏁诲畷鐔碱敍濮ｇ鍔戦弻銊╁棘閸喒鎸冮梺浼欑秬閸嬫劗妲愰幘瀛樺濞寸姴顑呴幗鐢告⒑閸︻厐褰掆€﹂悜鐣屽祦闊洦绋戝婵嬫煛婢跺鐏ｇ紒鍗炵埣濮婃椽宕ㄦ繝鍐槰闂佺硶鏅涢敃锕€顕ユ繝鍕磯闁靛⿵瀵屽鐔兼⒑閸︻厼鍔嬮柡宀嬬節瀹曟垿骞樼紒妯绘珳闁硅偐琛ラ埀顒冨皺閻╁孩绻濋悽闈涗沪闁诡垰鐭傚畷浼村冀椤撶偟鍘撮梺纭呮彧闂勫嫰寮查鍕厱闁哄洢鍔屾晶顖涙叏閿濆懐澧﹂柟顔筋殘閹叉挳宕熼鍌ゆФ闂備礁鎽滄慨鐢告偋閻樿违濞达絿纭跺Σ鍫熸叏濡も偓濡瑩宕撻棃娑辨富闁靛牆妫楅崸濠囨煕鐎ｎ偅宕岄柡灞稿墲閹峰懘妫冨☉鎺戜壕婵犻潧妫鏍磽娴ｈ偂鎴炲垔閹绢喗鐓曢柨鏃囶嚙楠炴鏌ら崹顔藉枠婵﹥妞介獮鏍倷閹绘帩鐎虫俊鐐€ら崢濂稿床閺屻儺鏁嬮柨婵嗩樈閺佸啴鏌曡箛濠冩珕婵℃ぜ鍔戦弻锝堢疀閺囩偘鎴烽梺绋款儐閹瑰洤鐣烽幎鑺ュ€婚柤鎭掑劗閹峰姊虹粙鎸庢拱闁煎綊绠栭崺鈧い鎺嶇劍閸婃劗鈧娲橀崝鏍囬悧鍫熷劅闁挎繂娲ㄩ崝璺衡攽閻愬瓨灏伴柛鈺佸暣瀹曟垿骞橀幖顓燁啍闂佺粯鍔曢顓熸櫠閸偁浜滈柕蹇婂墲缁€瀣煛娴ｇǹ鏆ｉ柛鈹惧亾濡炪倖甯掗崐鎼佀夊杈ㄥ枑闁绘鐗嗙粭姘舵煟閹捐泛鏋涢柡灞炬礉缁犳稒绻濋崘銊︾暯闂備線娼荤€靛矂宕㈡總鏉嗗顫濋鑺ユ杸闂佺粯锚绾绢參銆傞弻銉︾厽闁规儳顕幊鍥煛鐏炲墽娲存鐐疵灃闁逞屽墴楠炲﹥鎯旈敐鍥╃暥闂佽法鍠撴慨鐢稿煕閹达附鐓欓柟顖涙緲琚氶梺杞扮窔娴滃爼骞冩禒瀣垫晬闁靛牆娲ㄩ惁鍫ユ倵濞堝灝鏋︽い鏇嗗洤鐓″璺衡看濞尖晜銇勯幇鍓佸埌濠殿垯鍗冲缁樻媴閸涘﹤鏆堥梺鍦瑰Λ婵嗙暦閻旂⒈鏁冮柨婵嗘搐閽傚鈹戦悩娈挎殰缂佽鲸娲熷畷鎴﹀箣閿曗偓绾惧湱鎲歌箛娑樼閻庯綆鍠楅弲鏌ユ煕閵夈垺娅冪紒妤€顦靛铏规喆閸曨偆顦ㄥ銈嗘肠閸ャ劌鐝旈梺缁樻煥閸氬鎮￠悢鍏肩厸闁告劑鍔岄埀顒佹倐瀹曟繄鈧綆鍠楅崐鍨叏濮楀棗骞楅柣銊﹀灴閺岀喖宕ｆ径灞炬啓缂備胶濮甸惄顖氼嚕閹绢喗鍊婚柛鈩冿供濡儵姊婚崒娆掑厡妞ゎ偄顑夊畷鐔煎垂椤愩倖娈鹃梻鍌欒兌缁垶骞愰懡銈囩煓闁硅揪绠戣繚闂佸湱鍎ら崺鍫濐焽閳哄倶浜滈柟鍝勭Ф閸斿秹鏌熼鐣屾噰婵﹥妞藉畷顐﹀礋闂堟稑澹夐梻浣哄劦閺呪晠宕归崼鏇犲祦婵°倕鎳庨柋鍥煛閸モ晛浠滈柛鐐妼椤啴濡舵惔鈥冲箣闂侀€炲苯澧痪缁㈠幘缁柨煤椤忓懐鍘告繛杈剧悼閹虫挻鎱ㄥ鍥ｅ亾鐟欏嫭绀€闁靛牊鎮傞獮鍐Χ閸℃ê顎撻悗鐟板婢ф骞夐姀銈嗏拻濞达綀妫勯崥褰掓煕閻樺啿濮夐柛鎺撳浮瀹曞ジ濡烽妷褜鏀ㄩ柣搴ゎ潐濞叉牕煤閵婏妇鈻旂€广儱顦伴悡娑氣偓瑙勬惄閸犳牠寮甸鍌滅閹兼番鍔嶉埛鎴︽煙椤栧棗鎳愰濠囨⒑绾懏鐝柟鎼佺畺閸┾偓妞ゆ巻鍋撻柣蹇斿哺閹繝鏁撻悩鑼舵憰闂佽法鍠撴慨鎾倷婵犲洦鐓冮弶鐐靛椤﹂绱掗鑲╁煟婵﹥妞藉畷姗€骞撻幒鎾斺晛顪冮妶蹇曠暤婵炰匠鍥风稏闊洦鎷嬪ú顏嶆晜闁告粈鐒﹂ˉ鍫熺節閻㈤潧浠﹂柛銊ョ埣閹兘濡烽埞褍娲崺锟犲川椤旇瀚奸梻浣虹帛閸ㄧ喖寮插⿰鍐ｆ瀺闁瑰墽绮悡鏇㈡煏婵炲灝鈧洖鐣甸崱妯镐簻闁靛繆鈧啿鎽甸梺杞扮劍閸旀牕顕ラ崟顒傜瘈闁告洟娼ч弲锝嗙節閻㈤潧浠╅柟娲讳簽瀵板﹥绂掔€ｎ亞顔戝┑鐘诧工閻楀﹪宕曢幘缁樼厱闁哄洢鍔岄悘锟犳煃闁垮鐏撮柡灞剧洴楠炴﹢寮堕幋鐘点偡濠电偛鐡ㄧ划搴ㄥ磻閵堝钃熸繛鎴欏灪閸嬫劙鏌ц箛锝呬簻缁剧偓濞婇幃妤冩喆閸曨剛锛橀梺鎼炲妺缁瑩鐛崘鈺冾浄閻庯綆浜滅粣娑欑節閻㈤潧孝閻庢凹鍓熼悰顔嘉旈崨顔规嫼缂佺虎鍘奸幊搴ㄋ夊澶嬬厵婵炶尪顔婄花鐣屸偓鍨緲閿曘儵濡堕敐澶婄闁靛⿵绠掔欢銏ゆ⒒娴ｇǹ鏆遍柟纰卞亰楠炲﹥鎯旈妸銉ь槱闂佺粯顭囬崑銊︾濠婂牏鍙撻柛銉ｅ妽鐏忛潧顭胯濞茬喖寮诲☉銏犵疀闁靛ǹ鍎版竟鏇㈡⒒閸屾艾鈧嘲霉閸パ€鏋栭柡鍥ュ灩缁愭鏌熼幆褏鎽犻柛娆忕箻閺岋綁濮€閳惰泛缍婇幃锟犲即閵忥紕鍘甸柡澶婄墦缁犳牕顬婇鈧弻锝夊箻鐎涙顦ㄧ紓浣虹帛缁诲牆螞閸愩劉妲堟慨妤€妫旈幃锝夋⒒娴ｅ憡鍟炴慨濠勬嚀閻ｆ繈骞栨担姝屾憰闂佹寧绋戠€氀囧磻閹剧粯鏅查幖瀛樼箞閸嬫姊洪崨濠勬噧閻庢矮鍗冲濠氬灳瀹曞洦娈曢梺閫炲苯澧寸€规洑鍗抽獮妯兼嫚閼碱剛宕跺┑鐘垫暩婵潙煤閵堝洦鍏滈柍褜鍓熷铏圭磼濡搫顫嶅┑鐘灪閿曘垹顕ｉ幖浣搁唶闁绘棁娅ｉ惁鍫ユ⒑濮瑰洤鐏叉繛浣冲棌鍙撮梻鍌欒兌鏋い鎴濇楠炴劙骞栨担鑲濄儵鏌熼悜妯虹劸婵炲皷鏅犻弻銊モ攽閸℃ê娅㈡繝銏ｎ潐濞叉鎹㈠┑鍡忔灁闁割煈鍠楅悘鍫ユ⒑閹稿孩纾搁柛搴ㄤ憾閳ユ棃宕橀埡鍐炬祫闁诲函缍嗛崜娆戠矈閿曞倹鈷戠憸鐗堝笒娴滀即鏌涘Ο鍦煓鐎规洘娲熼幃銏ゅ礂閼测晛寮虫繝鐢靛█濞佳兾涘▎蹇ｅ晠婵犻潧顑嗛悡娑㈡煕閳╁啫濮囬柍褜鍓氶悧婊堝箲閵忕姭鏀介悗锝庡亜娴犳椽姊婚崒姘卞缂佸鍔欏畷顐⑽旈埀顒勨€旈崘顔嘉ч柛鈩冾焽椤︺劎绱撴笟鍥ф灈婵炲鍏樺畷姘跺箳閹惧墎鐦堝┑顔斤供閸撴盯顢欓幒妤佸€甸悷娆忓缁€鈧┑鐐额嚋缁犳垿鍩㈤幘娣亝闁告劏鏂侀幏鍝勨攽椤旂偓鍤€婵炲眰鍊濋幃姗€顢氶埀顒€顕ｉ幖渚囨晜闁割偆鍠撻崢浠嬫⒑閸︻厼鍔嬮柛銊潐缁傛帒鈽夐姀锛勫帾闂佹悶鍎滈崘鍙ョ磾婵°倗濮烽崑鐐垫暜閿熺姷宓侀悗锝庝簴閺€浠嬫煕閹扳晛濡哄瑙勬礋濮婃椽鎮℃惔顔界稐闂佺ǹ锕ュú鐔绘＂濠殿喗枪閸╂牠鎮￠姀鈥茬箚妞ゆ牗鐟ㄩ鐔镐繆椤愵剙顩柟鍙夋倐閹囧醇濠靛牏鎳栭梻浣告惈閹冲繘鎮￠敓鐘茶摕婵炴垯鍨瑰敮闂佹寧绻傞幊搴ㄢ€栭崼婵冩斀闁绘劘灏欐晶鏇㈡煟椤掑啫浜规俊鍙夊姍楠炴鈧稒锚椤庢捇姊洪懞銉冾亪藝閸楃倫鎺楀閵忋垻锛濇繛鎾磋壘濞层倕鈻嶅鈧弻娑㈠Ω閵娿儳鍘繛锝呮搐閿曨亪骞冮悾宀€鐭欓悹渚厛濡茶淇婇悙顏勨偓鏍偋椤撶姴绶ゅΔ锝呭暙閻ら箖鏌ㄥ┑鍡╂Ч闁抽攱鍨圭槐鎾存媴楠炲簱鍋撻悙鍝勭闁挎繂鍟╁Ч妤呮⒑閸涘﹤濮﹂柛鐘宠壘閻ｅ灚绗熼埀顒勫箖濡ゅ懏鏅查幖绮瑰墲閻忓秹姊虹粙鍖″伐闁诲繑姘ㄩ幑銏犫槈閵忕娀鍞跺┑鐘绘涧閻楀懘鎼规惔銊︹拺闁告繂瀚烽崕婊堟⒒閸曨偄顏┑锛勬暬瀹曠喖顢涘顒€绁梻浣虹《閸撴繈銆冮崱娑樼闁规壆澧楅悡鐔兼煙缂併垹鐏欓柛瀣崌楠炲洦鎷呴崫鍕毄闂備浇宕垫慨鎾箹椤愩儯浜归柛褎顨呴柋鍥煏婢跺牆鍔﹂柛鐐舵硾椤啴濡堕崨顖滎唶闂佺粯顨呴敃顏堝箖閿熺姴鍗抽柣鏃囨椤旀洟姊洪悷鎵憼缂佽鍊块幊婊嗐亹閹烘挾鍘介棅顐㈡搐閿曘倖鏅堕懠顒傜＜缂備焦顭囩粻鐐测攽椤旂懓浜鹃梻浣哄仺閸庤京澹曢鐔奉嚤閻忕偠袙閺€鑺ャ亜閺冨倶鈧螞濮橆厾绠鹃柛婊冨暟缁夘喗顨ラ悙鑼闁诡喒鏅濋幏鐘绘嚑椤掑鏂€闂傚倷绀佸﹢閬嶅磿閵堝鍨傞柛顐ｆ礃閸庡﹪鏌ㄥ┑鍡樺窛缁惧彞绮欓弻娑氫沪閹规劕顥濋梺閫炲苯澧柟鑺ョ矌閸掓帡寮崼婵堥獓闂佺懓鐤囧▍鏇犵矙閹达箑鐓濋幖娣妼缁狅綁鏌ｅΟ鍏兼毄闁告柨缍婇弻锝嗘償閿涘嫮鏆涢梺绋块叄娴滃爼鐛繝鍐ㄧ窞闁归偊鍓熼崬璺侯渻閵堝懐绠伴柣妤€锕幃锟犲即閵忥紕鍘搁梺鎼炲劘閸庤鲸淇婇悡搴唵鐟滄粓宕板璺虹劦妞ゆ帊绶￠崯蹇涙煕閻樺磭澧い顓炴喘閸ㄩ箖骞囨担鍦▉婵犵數鍋涘Ο濠冪濠靛瑤澶愬醇閻旇櫣顔曢梺鐟邦嚟閸嬬偤鎯冮幋鐐簻闁哄倸灏呴弨鑽ょ磼缂佹绠撻柍缁樻崌瀹曞綊顢欓悾灞借拫闂傚倷绀侀幉锟犳偡閵夆晛鐤い鎰跺瘜閺佸鏌ㄥ┑鍡╂Ц閹喖姊洪幐搴⑩拻闁惧繐楠搁湁闁告洦鍨遍悡鐔煎箹閹碱厼鐏ｇ紒澶屾暬閺屾稓鈧綆浜濋ˉ銏⑩偓瑙勬磸閸ㄨ櫣绮嬮幒鏂哄亾閿濆骸浜愰柟閿嬫そ閹嘲饪伴崨顓ф毉闁汇埄鍨辩敮妤冪矉瀹ュ棗顕遍悗娑欘焽閸橀亶姊洪棃娴ㄥ綊宕愰弴鐐垫殼闁糕剝绋掗悡娆愩亜閺嶃劏澹橀柡鍡悼閳ь剝顫夊ú锕傚磻婵犲倻鏆﹂柣鏂垮悑閸嬨劑姊婚崼鐔衡姇闁哄棌鏅犲娲嚒閵堝懏鐎剧紓渚囧枛閻倿寮绘繝鍥ㄦ櫜濠㈣泛锕﹂悾楣冩⒑閸涘﹥澶勯柛銊ャ偢瀵偄顓兼径瀣幗闂佸綊鍋婇崹浼存嫊婵傚憡鐓欓柤鎭掑劜缁€鈧梺瀹狀潐閸ㄥ潡骞冨▎鎾崇骇闁瑰濮抽幋鐑芥⒒娴ｈ鍋犻柛鏂跨箻閿濈偞寰勯幇顒傦紵濡炪倖鍔ч梽鍕煕閹烘垯鈧帒顫濋敐鍛婵犵數鍋橀崠鐘诲炊閵婏附鐝梻濠庡亜濞诧妇绮欓幋鐘差棜闁稿繗鍋愮弧鈧梻鍌氱墛缁嬫帞绮婇埡鍛厓鐟滄粓宕滃韬测偓鍐╃節閸屻倖缍庢繝鐢靛У閼瑰墽澹曢崗鑲╃闁糕剝锚婵附淇婇姘偓鑽ゆ閹惧鐟归柍褜鍓涘☉鐢稿箹娴ｅ摜鐛ュ┑顔角规ご鎼侊綖閺囥垺鐓熼柟閭﹀墻閸ょ喖鏌ｉ悢椋庣Ш闁哄苯绉烽¨渚€鏌涢幘瀵哥畵閾荤偤鏌曡箛瀣偓鏍煕閹达附鍋℃繛鍡楃箰椤忊晝绱掗埀顒勫礃閳哄啰顔曟繝銏ｆ硾椤戝洤煤鐎电硶鍋撶憴鍕闁硅櫕鎹囬崺鐐哄箣閿曗偓閻愬﹪鏌嶉崫鍕灓闁哥喎閰ｅ缁樻媴閾忓箍鈧﹪鏌涢幘瀵哥疄闁轰礁顑夊铏规嫚閺屻儳宕紓浣哄У閹瑰洭鐛崘銊庢棃鍩€椤掑嫸缍栨繝闈涱儛閺佸洭鏌ｉ幇顒€绾ч柟铏懇濮婄粯鎷呴崨濠冨創闂佹椿鍘奸ˇ杈╂閻愬鐟归柍褜鍓熼幃浼搭敋閳ь剙鐣烽崼鏇ㄦ晢濞撴艾娲﹂ˉ鍫熺節閻㈤潧浠﹂柛銊ョ埣閺佸啴鍩℃笟鍥╁姺婵炲濮撮鍡涘煕閹烘嚚褰掓晲閸ャ劌娈岄梺宕囩帛濞茬喖寮婚敓鐘茬劦妞ゆ帒瀚悞鑲┾偓骞垮劚閹虫劙藝椤斿皷鏀介柣鎰级椤ョ偤鏌熼崨濠冨€愰柟閿嬪灴閹垽宕楅懖鈺佸箺闂備礁鎼崯顐︽偋濠婂吘娑㈠箣閿旂晫鍘搁梺绯曞墲椤ㄥ繘宕曢弮鍫熺厸鐎光偓鐎ｎ剙鍩岄柧缁樼墵閺屽秷顧侀柛鎾跺枛楠炲啳顦圭€规洖宕—鍐磼濡棿绨村┑鐘茬棄閺夊簱鍋撻弴銏犵疇闊洦绋戠壕褰掓煟閵忕姵鍟為柛瀣у墲缁绘盯宕卞Δ鍐唶濡炪倕娴氭禍鐐垫閹烘鐒垫い鎺戝缁€鍐┿亜韫囨挻顥犻柨娑欑矊椤啴濡堕崱姗嗘⒖婵犳鍠撻崐鏇㈡偩瀹勬壆鏆嗛柛鏇ㄥ墰閸樿棄鈹戦悙鏉戠仴鐎规洦鍓欓埢宥咁吋閸ワ絽浜鹃悷娆忓缁€鈧梺缁樼墪閵堟悂濡存担鑲濇棃宕ㄩ鐘插Е婵＄偑鍊栫敮鎺斺偓姘煎墴瀹曞綊宕掗悙瀵稿幈闁诲繒鍋涙晶浠嬪煟閵堝悿鐟扳堪閸曨厾鐓夐梺鍝勭灱閸犳牠鐛澶嬫優妞ゆ劧绲胯ぐ鍛存⒒娓氣偓閳ь剛鍋涢懟顖炲礉椤栫偞鐓曟繛鎴濆船濞呮ɑ绻涢懖鈺佹灈婵﹤鎼叅閻犱礁纾惄搴ㄦ⒑娴兼瑧鎮兼い顐ｆ礋濮婂宕掑顑藉亾閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁诡垎鍐ｆ寖闂佺娅曢幑鍥灳閺冨牆绀冩い蹇庣娴滈箖鏌ㄥ┑鍡欏嚬缂併劎绮妵鍕箳鐎ｎ亞浠鹃梺闈涙搐鐎氫即鐛崶顒夋晬婵絾瀵ч幑鍥蓟閻斿摜鐟归柛顭戝枛椤牆顪冮妶搴′簼缂侇喗鎸搁悾鐑藉础閻愬秵妫冮崺鈧い鎺戝閻撴洟鏌熼悜姗嗘畷闁抽攱鍨圭槐鎾存媴婵埈浜炵划顓☆樄闁哄矉绻濆畷顏呮媴闂€鎰瀳婵°倗濮烽崑娑⑺囨潏鈺佸灊婵炲棗绻嗛弸搴ㄦ煙椤栧棗瀚埀顒夊灦濮婄粯鎷呯粙娆炬闂佺ǹ顑嗙敮鎺椻€﹂崸妤€鐒垫い鎺戝€荤壕浠嬫煕閹般劍娅嗙紒鈧€ｎ偅鍙忓┑鐘插亞閻撹偐鈧娲樼敮鎺楀煝鎼淬劌绠ｆい鎾跺晿濠婂牊鈷掑ù锝呮啞鐠愶繝鏌嶅畡鎵ⅵ鐎规洘鍨剁换婵嬪磼濠婂嫭顔曟俊鐐€栭悧婊堝磿閹版澘鏋侀柛鎰靛枟閻撱儵鏌￠崘銊﹀妞ゃ儱顦湁婵犲﹤鐗呴幉鐐繆椤愩垺鍤囨い銏℃礋婵偓闁炽儲鍓氬Σ杈╃磽閸屾瑧顦︽い鎴濇瀹曞綊宕稿Δ鈧粻姘舵倶閻愯泛鈻忛柡鍐ㄧ墕瀹告繃銇勯弮鈧崕鎶界嵁閹扮増鈷掑ù锝堫潐閸嬬娀鏌涙惔顔肩仸鐎规洘娲熼弻鍡楊吋閸℃せ鍋撻懜鐐逛簻闊洦鎸炬晶娑㈡煟閹捐泛鏋涢柡宀€鍠栭獮鎴﹀箛椤撶姰鈧劙姊洪崫鍕靛剱闁烩晩鍨跺濠氬灳瀹曞洦娈曢柣搴秵閸撴稖鈪靛┑掳鍊楁慨鐑藉磻濞戞◤娲敇閳ь兘鍋撴担绯曟瀻闁圭偓娼欏▓鐔兼⒑闂堟侗妾у┑鈥虫搐鍗遍柟杈鹃檮閳锋帡鏌涚仦鍓ф噮妞わ讣绠撻弻鐔哄枈閸楃偘绨婚柧鑽ゅ仦娣囧﹪濡堕崨顔兼闂佽绻戞繛濠囧蓟閵娾晛鍗抽柣鎰ゴ閸嬫捁銇愰幒鎴犲幈闂佸湱鍎ら〃鍡涙偂閺囥垺鍊堕柣鎰綑缁€鍐熆鐟欏嫸鑰块柡灞界Х椤т線鏌涢幘鏉戝摵妤犵偛鍟村畷鎺戭潩鏉堛劍顔曢梻浣告贡婢ф顭垮Ο鑲╀笉闁荤喐鍣磋ぐ鎺撴櫜闁告侗鍠楅崰鎰版⒑閹稿海鈯曢柣鐕傞檮缁岃鲸绻濋崶銊モ偓椋庘偓鐟板閸犳牠宕滈弶搴撴斀闁绘劕妯婇崵鐔兼煕韫囨棑鑰挎鐐叉閻ｆ繈宕熼銏ｂ偓鍨攽閿涘嫬浠滈柣銊у厴瀹曟垿骞樼拠鑼唺闂佽鎯岄崢浠嬪磽閻㈠憡鈷戦柟绋挎捣缁犳捇鏌熼崘鑼ｇ紒鏃傚枛瀵挳鎮㈤崜浣虹暰闂備線娼ч悧鍡椕洪妶鍛瘎闂傚倷鑳舵灙妞ゆ垵妫濋幆鍕敍濠靛浂妫ㄩ梻鍌欑劍閻綊宕圭紒妯间粴闂備礁鎼鍕嚄閸洖鐒垫い鎺嗗亾缂佺姴绉瑰畷鏇㈡焼瀹撱儱娲、娑樷堪閸涱垳鍔堕梺璇插嚱缂嶅棝宕戞担鍦洸婵犲﹤鐗婇悡娆撴煙娴ｅ啯鐝繛鍛嚇閺岋綀绠涢敐鍛亪闂傚洤顦扮换婵囩箾閹傚闂備礁鎲￠〃鍡樼箾婵犲偆鍤曞┑鐘崇閺呮煡鏌涢埄鍏狀亪宕濋崨瀛樷拺闁绘劘妫勯崝姘舵煕閻旂ǹ顕滄繛鐓庣箻瀹曘劎鈧稒菤閹疯櫣绱撴担鍓插剱妞ゆ垶鐟╁畷鏇㈠箛閻楀牏鍘甸梺鑽ゅ枔婢ф宕板Ο灏栧亾濞堝灝鏋︽い鏇嗗洤鐓″鑸靛姇椤懘鏌ｅΟ璇茬祷缂佷緡鍣ｅ缁樼瑹閳ь剙顭囪閻忔瑩姊虹粙鍧楀弰濞存粌鐖奸弫鎰版倷閸濆嫮鍘搁梺绋挎湰缁酣顢欓弴銏＄厽閹艰揪绲鹃弳鈺呭几椤忓牊顥夐悗锝庡枟閳锋垹绱掔€ｎ偒鍎ラ柛搴㈠姍閺屾盯寮埀顒勩€冮崨顓炵カ闂備礁婀辨晶妤€顭垮Ο鑲╀笉濠电姵纰嶉崐鍨箾閹寸儐浠炬い蹇撶墱閺佷線鏌熼崜褏甯涢柍閿嬪灴閺屾稑鈽夊鍫熸暰闁诲繐娴氶崢楣冨焵椤掍緡鍟忛柛鐘崇洴瀹曟椽寮介鐐垫煣闂侀潧顧€婵″洨寮ч埀顒勬⒑濮瑰洤鐏叉繛浣冲嫮顩锋繝濠傜墛閻撶喖骞掗幎钘夌閹兼番鍔岀粈鍡涙煙閻戞﹩娈曢柛瀣姍閹綊宕堕妸銉хシ闂佽瀵掗崣鍐蓟閿濆鍋愰柣鎴炃滈埀顒€鍟撮幃褰掑焵椤掑嫭鏅濋柍褜鍓熼垾锕傚炊椤掆偓閸愨偓閻熸粌顦靛銊︾鐎ｎ偆鍘藉┑鈽嗗灥濞咃綁寮搁崘顭戠唵閻熸瑥瀚烽悞浠嬫煟閵夘喕閭鐐叉椤︽煡鏌熼钘夌仼缂佽鲸甯炵槐鎺懳熼崗鐓庡灡闁诲氦顫夊ú鏍х暦椤掑啰浜介梻浣瑰劤缁绘﹢鍩€椤掍胶鈯曟俊顖氾攻缁绘繈鎮介棃娴躲儲銇勯敐鍕煓闁糕斁鍋撳銈嗗笒閸燁偊鎯冨ú顏呯厽妞ゆ挾鍠撻幊鍕煙娓氬灝濡界紒缁樼箞瀹曟帒鈽夊▎蹇撳闂備胶绮幐鍫曞磹濠靛绠栭悷娆忓婵挳鏌涘┑鍕姷缂佽京鍋ゅ娲倻閳哄倹鐝斿┑鐐存尦椤ユ挸危閹版澘绠婚悹鍥皺閿涙粌鈹戦缂存垿鎯侀悜钘夘潊闁靛牆妫涢崣鍡涙⒑閸濆嫭澶勯柨姘舵煃瑜滈崜姘辨崲閸儳宓侀柛鎰靛枛缁犳氨鎲歌箛娑樻辈閻庯綆鍠楅悡娆徝归敐鍛殲闁绘繍浜弻锟犲磼濡も偓娴滈箖姊婚崒姘偓鐑芥嚄閸撲焦鍏滈柛顐ｆ礀缁€鍫熺節闂堟侗鍎愰柛瀣剁節閺屻劑寮村Δ鈧禍楣冩⒑瀹曞洨甯涢柟鐟版搐椤曪綁骞橀纰辨綂闂佹娊鏁崑鎾绘煙闁垮銇濇慨濠勭帛缁楃喖宕惰缁ㄤ粙姊虹粙娆惧剱闁烩晩鍨堕獮濠囨偄绾拌鲸鏅┑鐐村灦閻燂箓鎮甸悜鑺モ拺闁革富鍙€濡炬悂鏌涢悩鎰佹疁闁诡噯绻濆鎾閿涘嫬甯楅梺鑽ゅ枑閻熴儳鈧凹鍓熷畷婵嬪Χ閸氥倗鎳撻オ浼村川椤撴繂顥氬┑鐑囩到濞层倝鏁冮鍫濈畺婵炲棙鎼╅弫鍌炴煕閺囨ê濡煎ù婊堢畺閺屸€愁吋鎼粹€茬凹闂佸搫妫欑划宀勫煘閹达附鍋愰柟缁樺俯娴尖偓闂備焦妞块崢鐓幬涘Δ鍛ラ柟鐑樺焾濞尖晠鏌ㄥ┑鍡樺櫢濠㈣娲熼弻锝嗘償閵堝孩缍堝┑鐐插级缁挸顕ｉ锝囩瘈婵ǜ鍎崑鎾存媴閼叉繃妫冨畷銊╊敇閻欌偓濡茶淇婇悙顏勨偓鏍暜閹烘纾归柟闂磋兌瀹撲線鏌涢鐘插姕闁抽攱甯掗湁闁挎繂鎳忛崯鐐烘偣閹板墎绡€闁哄本绋撻埀顒婄秵閸嬪懐浜搁鐔翠簻妞ゆ挾鍋為幖鎰版煃鐠囨煡鍙勬鐐达耿瀹曟宕ㄩ婊冣偓鐐烘⒒閸屾瑧鍔嶉柛搴＄－閹广垽骞囬弶鍨亶濠电姴锕ら悧鍡欏婵犳碍鐓欓柣鎰靛墯閻忛亶鏌＄€ｎ偅顥堥柡灞剧洴閳ワ箓骞嬪┑鍥╀憾闂備浇顕х换鎰矓閻熼偊娼栭柧蹇撴贡绾惧吋淇婇婵愬殭闁汇劍鍨垮娲焻閻愯尪瀚板褜鍠氶惀顏嗙磼閵忕姴绫嶉悗娈垮枟婵炲﹪寮崘顔肩劦妞ゆ帒瀚悡姗€鏌熸潏鍓х暠缁炬儳鍚嬬换婵婎槼闁圭ǹ鎽滅槐鐐寸節閸屾粍娈鹃梺璺ㄥ枔婵绮婚鈧弻銈夊箒閹烘垵濮㈤梺鍛娒肩划娆忣潖濞差亜浼犻柛鏇ㄥ亝濞堫厼鈹戦悙棰濆殝缂佺姵鎹囬獮濠傗枎閹寸偛鏋傞梺鍛婃处閸撴瑥顕ｉ崸妤佺厽闁绘柨鎽滈幊鍐倵濮橆剙妲婚柍缁樻尭楗即宕熼鈧鎸庣節閻㈤潧孝閻庢凹鍠氶弫顔尖槈閵忥紕鍘甸悗鐟板婢ф宕甸崶顒佺厵闁绘挸娴风粔娲煙椤旀儳鍘存鐐茬Ч椤㈡岸宕ㄩ婵婄箷闂傚倸鍊搁崐鎼佸磹妞嬪海鐭嗗〒姘ｅ亾妤犵偛顦甸崹楣冨箛娴ｅ湱绋侀柣鐔哥矌婢ф鏁Δ鍛；閻庯綆鍠楅悡鐔镐繆椤栨氨浠㈤柣銊︽そ閺岋繝宕遍鐘垫殼闂佸搫鐭夌紞渚€骞冮姀銏㈢煓婵炲棛鍋撻ˉ鎾绘⒒娴ｅ鈧偓闁稿鎸搁湁闁绘ê妯婇崕鎰版煟閹惧瓨绀冮柕鍥у楠炲洭宕滄担鐟颁还缂傚倷鑳舵慨鐢稿箲閸ヮ剙钃熼柨婵嗙墢閻も偓闂佺懓鐏濈€氼垶宕ラ鈶╂斀妞ゆ梻銆嬮弨缁樹繆閻愯埖顥夐柣锝囧厴閹粎绮电€ｎ偅娅嶉梻浣规灱閺呮盯宕板璺虹劦妞ゆ帊绶″▓婊勬叏婵犲啯銇濈€规洦鍋婃俊鐑藉Ψ閹板墎绋婚柕鍥у婵℃悂濡烽婵嗘儓闁诲氦顫夊ú鏍箹椤愶箑鐓″鑸靛姇椤懘鏌ｅΟ娲诲晱闁告艾鎳樺缁樻媴閻熼偊鍤嬪┑鈽嗗亗缁€浣界亱闂佸憡娲﹂崜娑€€呴柨瀣ㄤ簻闁规崘娉涙禍褰掓煛閳ь剚绂掔€ｎ偆鍘介梺褰掑亰閸撴瑧鐥閵囧嫰濡烽敂鍓х厒缂備浇椴哥敮鐐垫閹烘嚦鐔煎传閸曞灚缍掑┑锛勫亼閸婃垿宕归崫鍕庢盯宕橀鍏兼К闂佽法鍠撴慨瀵哥矆閸愨斂浜滈煫鍥ㄦ尰閿涙梻绱掓潏銊у弨婵﹦绮幏鍛存寠婢跺鐫忛梻浣稿暱閸㈡煡鎮ч幘宕囨殾婵犲﹤鍟犲Σ鍫熸叏濮楀棗骞楁繛鍫熸緲閳规垿鎮欓弶鎴犱户闂佹悶鍔嶅浠嬪箖闄囩粻娑樷槈濞嗘垵骞堥梻渚€娼чˇ浼村垂閻㈢ǹ绐楁慨姗嗗幖椤曢亶鏌涢幇鈺佸闁哄啫鐗婇崑鎰版⒒閸喓鈼ョ紒顔挎硾椤啴濡堕崱妤冧淮闂佺娅曢敃銏犵暦濞差亝鍊烽柛婵嗗椤撴椽姊洪幐搴㈢５闁稿鎹囬弻锝夊箛椤掑﹤濮涢梺鐟板级閻℃洜绮诲☉妯锋婵炲棗绻嬬划褔姊绘担绋挎毐闁圭⒈鍋婂畷顖炲箥椤旂⒈妫滄繛瀵稿Т椤戝棝鎮￠弴銏″€堕柣鎰絻閳锋棃鏌熼崘鍙夊殗闁诡喖缍婇獮鍥敂閸曨偆銈梻浣告惈閺堫剟鎯勯鐐茬畺闁宠桨鑳堕弳锕傛煕閵夋垟鍋撻柟铚傚嵆濮婄粯鎷呯粵瀣異闂佸摜鍣ラ崑濠傜暦閻旂⒈鏁囬柣妯虹仛椤矂姊婚崒娆掑厡閺嬵亪鎮跺鐓庝喊鐎规洘鍔欏鎾倷閳哄倻浜栨俊鐐€栭崝褔姊介崟顖氱厱闁硅揪闄勯悡鏇熺箾閹寸儑鍏柡鈧拠宸唵閻犲搫鎼顓㈡煛鐏炲墽娲村┑鈩冩倐閺佸啴鍩€椤掑媻澶屸偓锝庡墰绾惧吋銇勯弮鈧娆撳吹濞嗘挻鐓冮柦妯侯樈濡叉悂鏌嶇拠鏌ヮ€楁い鎾炽偢瀹曠喖顢橀悩铏珤闂傚倸鍊搁崐宄懊归崶顒婄稏濠㈣泛锕﹂弳锔戒繆椤栨縿鈧偓闁哄鐗楃换娑㈠箣閻戝棛鍔烽梺鐟邦潟閸庨亶鍩為幋鐐茬疇闂佺ǹ锕ラ〃鍡涘箞閵娾晜鍊婚柦妯侯槺閿涙盯姊虹紒妯哄濞存粈绮欓崺鈧い鎺嗗亾闁哥噥鍨抽幑銏犫攽鐎ｎ亞锛滈梺闈涚墕濡稓绮欐担铏圭＝闁稿本鑹鹃埀顒傚厴閹偤鏁冩担瑙勫櫡婵犵數濮甸鏍垂闁秴绠伴柟鎯版閽冪喐绻涢幋鐐冩艾危閸喐鍙忔俊銈傚亾闁绘鎸惧▎銏⑩偓闈涙憸绾捐偐绱撴担闈涚仼妤犵偞鍔欓弻娑氣偓锝庡亝瀹曞本鎱ㄦ繝鍌ょ吋鐎规洘甯掗埢搴ㄥ箳閹存繂鑵愰梻浣告惈椤戞劖绂嶉悙鍨潟闁圭儤顨呮儫闂佹寧鏌ㄩ～鏇熺閵忋倖鍊甸悷娆忓缁€鍐煕閺冣偓閻熴儵顢氶敐澶婄妞ゆ棁妫勬禍褰掓⒑閸︻厾甯涢悽顖滃仱瀵槒顦崇紒缁樼箞閹粙妫冨ù韬插劜閵囧嫰鏁愰崼鐕佹闁告浜堕弻娑樷槈濞嗘劗绋囬梺鎼炲妼閸婃悂婀侀梺绋跨箰閸氬绱為幋锔藉仭婵炲棙鐟ч悾鐢告煙椤旂瓔娈滈柡浣瑰姈閹棃鍩勯崘顏冮偗缂傚倸鍊风拋鏌ュ磻閹剧粯鐓曢悘鐐插⒔閵嗘帞绱掗埦鈧崑鎾斥攽閻樺灚鏆╁┑顔芥尦瀹曟劙骞栨担鍝ョ暫闂佸啿鎼幊蹇涙偂濞戙垺鐓曢柟鎵虫櫅婵″灝顭胯閻╊垶寮婚敓鐘插窛妞ゆ梹鍎冲銊╂⒑閸濆嫭婀扮紒瀣灴閸┿儲寰勯幇顒傤攨闂佺粯鍔樼亸娆擃敊閹寸偟绡€闁汇垽娼ф禒婊堟煟濡も偓閿曨亪骞冮敓鐘茬伋闁归鐒︾紞搴ㄦ⒑閹呯婵犫偓鏉堚晛顥氶柛蹇涙？缁诲棙銇勯弽銊х闁搞倖鐟╁缁樺緞婵犲嫅褎鎱ㄦ繝鍐┿仢闁圭绻濇俊鍫曞川椤旀儳缍掔紓鍌氬€烽懗鑸垫叏閻㈢ǹ绠扮紓浣诡焽閳瑰秴鈹戦悩鍙夊闁稿鍔庣槐鎺斺偓锝庡幗绾埖銇勯敂鑺ョ凡妞ゎ亜鍟存俊鍫曞幢濞嗗浚娼锋繝娈垮枟鑿ч柛鏃€鍨块獮鍡涘醇閵夈儳顦板銈嗙墬缁嬪牓骞忛搹鍦＝濞达絽澹婇崕鎰版倵缁楁稑鍠涢懓鍧楁煙濞堝灝鏋ょ痪鎯у悑閵囧嫰寮崶褌姹楅柡浣哥墕铻栭柣姗€娼ф禒婊勩亜閿旇鐏ラ柣蹇斿浮閺岋綁鎮欑€电硶鏋旈梺鍛婃尰缁诲牆顕ｉ鍕劦妞ゆ帒瀚埛鎺懨归敐鍫燁仩閻㈩垱鐩弻娑㈠籍閳ь剟鈥﹂崶銊ь洸缂佸绨遍弸搴ㄦ煙闁箑骞楅柣婵嗗槻閳规垿鎮欓弶鎴犱桓濠殿喗菧閸斿孩绔熼弴銏犵缂佹妗ㄧ花濠氭⒑閸濆嫬鈧粙鏁撻妷鈺佺；闁靛繈鍊栭悡鏇熺箾閸℃绠版い蹇ｅ弮閺岋紕浠﹂崜褎鍒涢梺绯曟櫇閸嬨倝鐛崱姘兼Ш婵犮垼顫夊ú鏍煘閹达附鏅柛鏇ㄥ亗閺夘參姊虹粙鍖℃敾闁绘绮撳顐︻敊閻ｅ瞼鐦堥梺鍛婃处閸擄箓宕戦妸鈺傗拺閻犳亽鍔岄弸鏂库攽椤旇姤灏﹂柟顔兼健椤㈡岸鍩€椤掑嫭鍋傛い鎰剁畱閻愬﹪鏌曟繛褉鍋撳┑顔兼喘濮婅櫣绱掑Ο璇查瀺濠电偠灏欓崰鏍ь嚕婵犳碍鏅查柛娑樺€婚崰鏍箹瑜版帗鍎岄柛婵勫劤椤︻垱绻濋悽闈浶涢柟宄板暣瀹曘劑顢欓懞銉ф闂傚倷绀侀幖顐︽儔婵傜ǹ绐楅柡宥庡幑閳ь兛绀侀埢搴ㄥ箻閸忓懐鐐婇梻浣告啞濞诧箓宕戦崟顖ょ稏婵ǹ浜壕浠嬫煕鐏炲墽鎳勭紒浣哄缁绘盯宕崘顏喰滈悗娈垮枟閻擄繝銆侀弴銏犖ч柛娑卞幘娴滅増绻濋悽闈涗粶婵☆偅鐟╅獮鎰板箹娴ｇǹ鐎梺绋挎湰缁嬪繑绂嶅⿰鍫熺厸闁告劑鍔庢晶娑㈡煛閸℃鐭掗柡灞剧〒閳ь剨缍嗛崑鍛暦鐏炰勘浜滈柕蹇婃閼板潡鏌熼鍝勭伈鐎规洘绮嶉幏鍛存惞鐟欏嫭鐦為梻鍌氬€搁崐椋庢濮橆剦鐒界憸鏃堝箖瑜斿畷鍗炩枎閹邦剙绨ラ梻浣告贡閸庛倝銆冮崨顖滅焼闁糕剝顦鸿ぐ鎺撳亗閹艰揪绲鹃幉濂告⒑缂佹ɑ灏版繛鑼枛瀵鏁愭径濠傚祮濠殿喗锕㈢涵绋课ｉ鐣岀閻庢稒顭囬惌鍡涙煕濡や礁鈻曠€殿喖顭烽幃銏㈠枈鏉堛劍娅栨繝娈垮枟閿氶柛鐔锋健椤㈡棃顢橀姀锛勫弰缂傚倷鐒﹂…鍥ㄦ櫠閻㈠憡鐓涢悘鐐垫櫕鍟稿銇卞倻绐旈柡灞剧洴楠炴鎹勯悜妯间邯闂備焦鎮堕崝鎴犵不閺嶎厼鏋侀柛鎰靛枛绾惧吋鎱ㄥΟ鍨厫闁哄棗鎳忕换婵嗩嚗闁垮绶查柍褜鍓氶幐鍓у垝閸喐濯寸紒顖涙礃閻忎礁鈹戦悩缁樻锭妞ゆ垵妫濆畷鎰版偨閸涘﹦鍘介梺闈涚箞閸婃牕鈽夎閵囧嫰鏁愰崨顓熷€梺闈涙搐鐎氱増淇婇幖浣肝ㄧ憸蹇涱敊婢跺瞼纾藉ù锝嚽归埀顒€鎽滅划鏃堝箻椤旇偐鍘撮梺纭呮彧缁犳垿鏌嬮崶顒佺厪闊洤顑呴悘顕€鏌涢弬璺ㄧШ婵﹦绮幏鍛村川婵犲懐顢呴梻浣呵圭花娲磹濠靛棛鏆﹂梻鍫熶緱濞尖晜銇勯幋鐘蹭沪婵＄偘绮欓妴渚€寮▎鎯ф倯闂佺硶鍓濊摫闁绘繃娲熷濠氬磼濞嗘埈妲梺瑙勭ゴ閸撴繄绮悢鑲烘梹鎷呮笟顖涢敜婵犵數濞€濞佳囨偋閸愵喖纾婚柟鐐墯濞尖晜銇勯幋鐐插姢濞寸媭鍠栭埞鎴︻敊绾嘲濮涚紓渚囧櫘閸ㄥ爼鐛箛娑樺窛闁哄鍨崇槐鍫曟⒑閸涘﹥澶勯柛瀣€婚崚鎺楊敍濞戞绠氶梺缁樺姦娴滄粓鍩€椤戞儳鈧繂鐣烽幋锕€宸濇い鎺戝悑濡炰粙骞冮姀鈽嗘Ч閹兼番鍨洪鏇㈡⒒娴ｅ憡璐″褎顨呴…鍨潨閳ь剙鐣烽敓鐘茬闁兼亽鍎抽崢浠嬫⒑瑜版帒浜伴柛鎾寸⊕缁傛帒鈽夐姀锛勫幍闂佸憡鍔栬ぐ鍐汲閻愮數纾肩紓浣诡焽缁犵偤鏌熼鑽ょ煓婵☆偄鍟湁闂婎剚褰冮崝锕傛煛鐏炵ǹ澧查柟宄版噽閹叉挳宕熼鐔蜂壕闁归偊鍠掗崑鎾舵喆閸曨剛顦ㄩ梺鎼炲妼濞硷繝鐛崘銊㈡瀻闊浄绲介弲鐘差渻閵堝棙鈷愰柛搴ゆ珪閹便劑鎮滃Ο鑲╃槇闂佹眹鍨藉褎绂掑⿰鍕箚妞ゆ劧绲块幊鍥殽閻愭潙濮嶇€规洦鍋婃俊鐑藉Ψ椤旂瓔妫ラ梺鑽ゅ枑缁孩鏅跺Δ鍐╂殰婵°倕鎳忛崑鍌炴煛閸モ晛鏋傚ù婊勭矋閵囧嫰骞樼捄鐑樼€婚梺璇茬箞閸庣敻寮婚敍鍕ㄥ亾閿濆啫濡奸柍褜鍓氱换鍫濐嚕婵犳碍鏅搁柣妯诲絻閼板灝顪冮妶鍡樺暗濠殿喚鍏樺鎶藉Χ婢跺鎷洪梺鍛婄箓鐎氼厼锕㈤悧鍫㈢闁告瑥顦辩粻妯肩磼椤旀鍤欓柍钘夘槸椤繈鏁愰崨顒€顥氶梻浣瑰缁诲倻鑺遍懖鈺勫С闁圭虎鍠楅悡鏇㈡倵閿濆骸浜濈€规洖鐬奸埀顒侇問閸犳洜鍒掑▎鎾扁偓浣割潨閳ь剟骞冨▎鎾崇煑濠㈣泛锕ゆ慨鍐⒒娴ｇ瓔鍤欏Δ鐘虫倐瀹曘垹饪伴崼婵堬紱闂佺懓澧界划顖炴偂閺囥垺鐓涢柛銉ｅ劚婵＄厧顭胯閸楁娊寮诲☉姗嗘建闁逞屽墰缁寮借濞兼牗绻涘顔荤盎鐎瑰憡绻傞埞鎴︽偐閹绘帗娈梻鍌氼槸缁夊綊骞冨畡鎵冲牚闁告洦鍓涢崙锟犳⒑閸濄儱鏋戞繛鍏肩懇閹箖鎮滈挊澶樻綂闂侀潧鐗嗛幊搴ｇ玻濞戞瑧绡€闁汇垽娼у瓭闂佸摜鍣ラ崑鍕敋閿濆閿ゆ俊銈勮兌閸樹粙姊虹紒妯荤叆闁硅姤绮撻獮濠囧礃椤旇棄鈧灚鎱ㄥ鍡楀婵℃彃顭烽弻宥囩磼濡纰嶅Δ妤婁簷閸楀啿鐣烽悢纰辨晢闁逞屽墮椤曪綁骞愭惔锝囩槇闂佹眹鍨藉褍鐡梻浣烘嚀閸熻法鎹㈠鈧妴渚€寮崼鐔蜂汗闂佹眹鍨婚弫鎼佹晬濠婂牊鐓涘璺猴功婢ф垿鏌涢弬璺ㄧ伇缂侇噮鍙冮獮鎺懳旀担鐟版畽闂備焦瀵х换鍌炈囨导瀛樺亗闁哄洨鍠撶弧鈧梻鍌氱墛缁嬫帡藟濠婂嫨浜滈煫鍥ㄦ尵閹界姷绱掔紒妯兼创鐎规洘顨婂畷妤呮偂鎼达綇绱﹀┑鐘愁問閸犳牠鏁冮敂鎯у灊妞ゆ牜鍋涚粻顖炴煕濞戞瑦缍戠€瑰憡绻傞埞鎴︽偐閸欏娅у銈冨灩閹虫ê顫忕紒妯诲缂佸瀵уВ鎰版⒑閹肩偛鐏柣鎿勭節楠炲啳顦规鐐疵悾鐑藉炊閵婏富鍟庨梻鍌欒兌閹虫捇顢氶銏犵；闁绘柨鎼慨顒勬煃瑜滈崜鐔奉潖閾忓湱鐭欓柟绋垮閹疯京绱撴笟鍥ф灈缂佸鎸抽崺銏ゅ箻濞ｎ剙浜濋梺鍛婂姀閺備線骞忓ú顏呪拺闁革富鍘愰悷鎳婅顦版惔锝勭瑝闂佺懓澧界换婵堟崲閸℃ǜ浜滈柟鏉垮閹ジ鏌曢崱妯虹闁逞屽墯椤旀牠宕锕€鐐婄憸蹇涘礉閿曗偓椤啴濡堕崱妤冪懆闁诲孩鍑归崣鍐春濞戙垹绠ｉ柨鏃傛櫕閸樺崬鈹戦悙鏉戠仸闁挎洦鍋勯蹇涘Ψ閵夈垺鏂€濡炪倖鏌ｉ崝宀勫箠閹邦喖顥氬ù鐘差儐閻撴洟鎮橀悙鎻掆挃闁愁垱娲熼弻锝夊Ω閿曗偓閻忔挳鏌＄仦璇测偓鏍Χ閿曞倸閿ゆ俊銈傚亾闁圭晫鏁哥槐鎾存媴閾忕懓绗＄紓浣筋嚙閸婂潡鏁愰悙娴嬫斀閻庯絽鐏氶弲锝呪攽閻愭潙鐏﹂柣鐔村劦椤㈡棁銇愰幒鎾嫽闂佺ǹ鏈悷褔藝閿曞倹鐓欓悹鍥囧懐锛熼梺鐟扮畭閸ㄨ棄鐣烽幒鎴旀敠闁诡垎鍌氼棜婵犳鍠楅…鍥储瑜嶉埢宥咁吋婢跺鍘遍梺宕囨嚀閻忔繈鎮橀鍡忓亾閸偅绶查悗姘嵆閻涱噣宕堕澶嬫櫍闂佺粯蓱瑜板啰绮婚悙鐑樷拻濞达絿鐡旈崵娆撴煕閹寸姵娅曠紒杈╁仱瀹曞崬螣绾拌鲸閿ゆ繝鐢靛Т閿曘倝鎮у⿰鍫濇瀬闁稿本绋忔禍婊堢叓閸ャ劍灏版い銉уХ閻ヮ亪骞嗚閸嬨垽鏌＄仦鍓с€掑ù鐙呯畵瀹曟粏顦俊鎻掔墕閳规垿鎮欑€涙ê鍓归梺闈╃秶缂嶄礁顕ｆ繝姘労闁告劏鏅涢鎾剁磽娴ｅ壊鍎愰悗绗涘啠鏋斿┑鐘崇閳锋垹绱掗娑欑闁哄缍婇弻娑氣偓锝庡亝鐏忣厽銇勯鍕殻濠碘€崇埣瀹曞崬螖閸愵亝鍣梻浣筋嚙鐎涒晠宕欒ぐ鎺戝偍闁告挆鍐ㄧ亰婵犵數濮甸懝鍓х不閾忣偂绻嗛柕鍫濆椤斿鏌熸搴ｅ笡缂佺粯绋掑蹇涘礈瑜忚ⅲ婵犵數鍋涢幏鎴犲緤閸ф鏋佹い鏂跨毞濡插牊绻涢崱妤冃＄紒銊嚙椤啴濡堕崱妯烘殫闂佺ǹ顑囬崑銈呯暦閺夎鐔沸ч崶锔剧泿闂備線娼х换鍡椢ｉ崨顓涙灁婵犲﹤鎳夐崑鎾斥枔閸喗鐏堝銈庡弮閺€杈ㄧ┍婵犲洦鍊婚柤鎭掑劜濞呭洭姊洪柅鐐茶嫰婢ь噣鏌嶇紒妯诲碍闁伙絾绻堝畷鐔碱敄閼恒儱顏圭紓鍌氬€风粈渚€藝椤栨娑樜旈崘锝呬壕婵﹩鍓欏Σ濠氭煃缂佹ɑ宕岀€殿喗鎸虫慨鈧柨娑樺楠炴姊绘担绛嬫綈闁稿孩濞婇、姘额敇閵忕姷鐤囬梺鍛婃处閸ㄩ亶鎮¤箛娑欑厱闁靛鍨甸崰姘閸愵喗鍊垫繛鍫濈仢濞呮﹢鏌涢敐蹇曠М鐎规洘妞介崺鈧い鎺嶉檷娴滄粓鏌熼悜妯虹仴妞ゅ繆鏅濋惀顏堝箚瑜庨崑銉╂煛瀹€鈧崰鏍嵁閸℃稒鍋嬮柛顐亝椤ュ姊绘担鍛靛湱鎮锕€绀夐柟杈剧畱缁犳牗淇婇妶鍛櫤闁稿鍔戦弻锝夊閵忕姳鍖栭梺閫炲苯澧柨鏇ㄤ邯瀵鈽夊锝呬壕闁挎繂楠告禍婵嬫倶韫囷絽寮柡灞界Ф缁辨帒螣鐠囪尙锛撻柣搴ゎ潐濞叉牜绱炴繝鍥モ偓浣糕枎閹炬潙浠奸柣蹇曞仜閸氬宕板顓犵瘈闁汇垽娼у暩闂佽桨鐒﹂幃鍌氱暦閹存績妲堥柕蹇曞Ь琚濋梺璇插嚱缂嶅棝宕板Δ鍛；闁跨喓濮甸悡鍐煕濠靛棗顏柛锝嗘そ閺岋繝宕ㄩ鎯у绩闂佸搫鐭夌紞渚€鐛崶顒夋晣闁绘﹩鍠栨禒锕傛⒒娴ｅ鈧偓闁稿鎸搁湁闁绘ê妯婇崕鎰版煟閹惧瓨绀嬮柡灞炬礃瀵板嫰宕煎┑鍡╃€抽梻浣虹帛閹稿宕濆Δ鍐╊潟闁圭偓鍓氶崥瀣煕閳╁喚娈樺ù鐓庤嫰閳规垿顢欑涵閿嬫暰濠碉紕鍋犲Λ鍕偩閻戠瓔鏁冮柨鏇楀亾缂佲偓鐎ｎ偁浜滈柟鐐墯濡插搫鈹戦悙鈺佷壕闂備礁鎼張顒勬儎椤栫偟宓佹慨妞诲亾鐎规洘锕㈤幊鐘垫崉閾忕懓骞嬫繝寰锋澘鈧牠銆冭箛鏃傤洸婵炲棙鎸哥涵鈧梺鍛婂姌鐏忔瑩寮抽敃鍌涚厽闁哄啫鍊甸幏锟犳煛娴ｅ憡鍠橀柡宀嬬到铻ｉ柧蹇涒偓娑氶┏婵＄偑鍊ら崑鍕儗閸屾凹娼栧┑鐘宠壘绾惧吋绻涢崱妯虹仴濠碘€茬矙濮婂搫煤鐠佸磭鐩庣紓鍌氱Т閿曘倝鎮鹃悿顖樹汗闁圭儤鎸搁惂鍕節閵忥絽鐓愭い顓炴喘瀹曟繈鎮滈懞銉ヤ画濠电姴锕ょ€氼剟鎮橀幘顔界厱闁宠桨鐒﹂崵鍥殽閻愯揪鑰挎い銏＄懅閹叉挳鏁愰崱妤婁紲濠电姷鏁搁崑鐘诲箵椤忓棗绶ゅù鐘差儏缁犵姵绻涢崱妯虹濞存粌缍婇弻鐔兼倻濡偐鐣洪梺鍝勬噽閸嬨倕顫忛搹鐟板闁哄洨鍋涢埛澶岀磽娴ｅ壊鍎愰柛銊ユ贡閸掓帗绻濆顒傞獓闁圭厧鐡ㄩ幐濠氬棘閳ь剟姊绘担铏瑰笡闁挎岸鏌ｉ妶鍛缂佹梻鍠庤灒濞撴凹鍨辩粋鍡涙⒑閸涘﹦鈽夐柣掳鍔戦崺娑㈠箣閿旂晫鍘卞┑鐐村灦閿曨偊寮ㄧ拠宸唵閻犲搫鎼顓㈡煛鐏炲墽娲撮柟顔规櫊瀹曟﹢骞撻幒鎾村殘闂傚倷娴囬鏍窗閺嵮岀劷闁跨喓濮撮弰銉╂煃瑜滈崜姘跺Φ閸曨垰绠抽柟瀛樼箥娴犻箖姊洪幎鑺ユ暠闁搞劌娼″璇测槈閵忊剝娅嗛柣鐘叉处瑜板啰绮荤憴鍕閻庢稒顭囬惌宀勬煕鐎ｎ偅灏甸柟骞垮灩閳藉濮€閻樻妲梻浣稿悑缁佹挳寮插⿰鍫濋棷濞寸厧鐡ㄩ崐鐢告偡濞嗗繐顏紒鈧崘顔界厱闁靛ě鍕瘓濡炪們鍨鸿摫缂佽櫣鏅划娆撳垂椤斿簱鍋撻崹顔规斀闁绘劕寮堕ˉ鐐烘煕閳轰胶澧︾€规洦鍨遍幆鏃堝Ω閿旇瀚奸梺鑽ゅТ濞诧箒銇愰崘顕呮晢闁靛繈鍨荤壕濂告倵閿濆骸浜滄繛鎼櫍閺屽秹濡烽婊呮殼閻庤娲栭悥濂搞€佸Δ浣瑰缂佸鐏濋ˉ妤佺節閻㈤潧啸闁轰礁鎲￠幈銊р偓鐢电《閸嬫挸顫濋悡搴㈢亾婵犮垼顫夊ú鐔煎箖閸撗傛勃缂佸銇樻竟鏇炍旈悩闈涗粶闁诲繑绻堝畷婵嗩潩閼哥數鍘甸梻浣哥仢椤戝懘鎮橀幘顔界厽闁瑰灝鍟晶瀛樸亜閵忊槅娈滅€规洘甯掕灃闁逞屽墯缁傚秹鎮欓鍌滎啎闂佸壊鍋呯换鍕閵忋倖鐓涢悗锝庡亞閳洟鏌曢崶褍顏い銏★耿閹晠骞撻幒鎴狀吅濠德板€楁慨鐑藉磻濞戙垹鐤悗娑櫳戦崣蹇涙煃瑜滈崜鐔煎蓟閺囥垹閱囨繝闈涙祩濡偞绻涚€涙鐭嬫い銊ワ躬瀵鎮㈤崗鐓庢疄闂佺粯顨呴悧鍡椻槈瑜忕槐鎾存媴閻熸壆绁峰┑鐐茬毞閳ь剚鍓氶崵鏇㈡煣韫囷絽浜滄い鏇憾閹鈽夊▎妯煎姺闂佸憡眉缁瑥顫忛搹鍦＜婵☆垵宕甸崣鍡涙⒑绾懎袚缂侇喗鎹囬獮鍡欎沪閸撗呯槇闂佸憡娲﹂崢楣冩晬濠婂嫮绡€闁靛骏绲剧涵楣冩煛閸偄澧寸€规洜鏁婚崺鈧い鎺嗗亾妞ゎ亜鍟存俊鍫曞幢濡皷鏁嶇紓鍌氬€哥粔宕囨濮樺墎宓侀柡宥庡厵娴滃綊鏌熼悜妯肩畺闁哄懏绻堝娲箰鎼达絿鐣靛┑鐐茬湴閸斿繘宕愰幘顔解拻闁稿本鑹鹃埀顒勵棑濞嗐垹顫濋澶屽姺閻熸粍妫冩俊鍫曨敂閸喎浠洪梻鍌氱墛缁嬫挾绮佃箛娑欌拺闁稿繗鍋愰妶鎾煛閸涱垰浠﹂柟渚垮姂瀹曟ê霉鐎ｎ偅鏉搁梻浣虹帛閿氱痪缁㈠弮閵嗗倿寮婚妷锔惧帗闂備礁鐏濋鍛存倶鐎涙ɑ鍙忓┑鐘插亞閻撹偐鈧娲栭妶鎼佸箖閵忋倕绠掗柟鍝勬娴滈箖鏌熼悜妯虹劸闁绘柨妫涢幉鍛婃償閳埖妞介幃銏ゆ偂鎼淬倖鎲伴梻浣虹帛濮婂宕㈣缁牓宕橀鐣屽幘缂佺偓婢樺畷顒佹櫠閻楀牄浜滈柡鍐ｅ亾閻㈩垽绻濋獮鍐ㄎ旈崘鈺佹瀭闂佸憡娲﹂崜娑⑺囬銏♀拺閺夌偟澧楃粊鐗堛亜閺囧棗娲﹂崑鈺呮煟閹达絾顥夐梺鍗炴喘閺岋繝宕堕妷銉ヮ瀴闂佸搫鎳岄崹铏规崲濞戞瑦缍囬柛鎾楀啫鐓傞梻浣告贡閳峰牓宕戞繝鍥ㄥ仒妞ゆ梻鈷堥崥瀣煕閳╁啯绀堢紒鐘冲哺濮婅櫣绱掑Ο鍝勑曟繛瀛樼矋缁捇宕洪埀顒併亜閹哄秷鍏屽褜浜濋妵鍕敃閵忋垻顔戦梺闈涙处閸旀瑩鐛幒妤€绠ｉ柣娆忔噽閸氬綊姊婚崒娆愮グ妞ゎ偄顦靛畷鏇㈡偂楠烆剚鐩畷鐔碱敍濮樻唻绱查梻渚€娼ц墝闁哄懏绮撳畷鎰版偨閸涘﹦鍘介梺闈涚箞閸ㄦ椽寮抽埡鍛厪闁糕剝顨愰煬顒勬煛鐏炲墽銆掗柍褜鍓ㄧ紞鍡涘磻閸涱垯鐒婃い鎾卞灪閻撳啰绱撴担鑲℃垵鐣峰畝鈧埀顒冾潐濞叉牜绱炴繝鍥モ偓浣糕枎閹炬潙浠奸柣蹇曞仜閸氬宕板顓犵瘈闁汇垽娼у暩闂佽桨鐒﹂幃鍌氱暦閹存績妲堥柕蹇曞Ь琚濋梺璇插嚱缂嶅棝宕板Δ鍛；闁跨喓濮甸悡鍐煕濠靛棗顏柛锝嗘そ閺岋繝宕ㄩ鎯у绩闂佸搫鐭夌紞渚€鐛崶顒夋晣闁绘﹩鍠栨禒锕傛⒒娴ｅ鈧偓闁稿鎸搁湁闁绘ê妯婇崕鎰版煕婵犲嫭鏆柡灞诲妼閳规垿宕卞▎蹇撴瘓缂傚倷闄嶉崝蹇旀叏閵堝桅闁告洦鍨伴～鍛存煟濡搫鏆辨繛鍫燁殔閳规垿顢欑涵鐑界反濠电偛鎷戠徊鍨ｉ幇鏉跨闁瑰啿纾崰鏍х暦椤愶箑绀嬮柛顭戝亞缁夊吋绻濋悽闈浶ユい锝堟鍗遍柛娑欐綑閸ㄥ倸霉閻撳海鎽犻柡鍛箖閵囧嫯绠涢幘璺侯暫缂備胶濮甸悧妤佺┍婵犲洤围闁告侗鍠栧▍锝囩磽娴ｅ搫啸缂侇噮鍨舵俊鐢稿礋椤栨氨鐫勯梺鎼炲劚濡瑧鏁敓鐘靛祦闊洦鎷嬪ú顏嶆晜闁告侗浜濈€氬ジ姊绘担铏瑰笡闁圭ǹ鎽滈埀顒€鐏氱敮鎺椻€﹂崶顒佸亜闁稿繗鍋愰崢杈ㄧ節閻㈤潧孝闁哥噥鍨崇划鍫⑩偓锝庡厴閸嬫挸鈻撻崹顔界亶闂佽鍠栭崐鍧楁偘椤旈敮鍋撻敐搴℃灍闁稿鍔欓弻锝夊閵忊晜娈扮紓浣稿船瀵埖绌辨繝鍥ㄥ€锋い蹇撳閸嬫捇寮介‖顒佺⊕缁楃喖鍩€椤掆偓閻ｇ兘骞嬮敃鈧粻娑欍亜閹烘垵鈧綊骞婇崘鈺冪瘈缁炬澘顦辩壕鍧楁煕鐎ｎ偄鐏寸€规洘鍔欐俊鑸靛緞婵犲倸浜跺┑鐘绘涧閸婃悂骞夐敓鐘茬９闁煎摜鏁哥弧鈧梻鍌氱墛娓氭宕曞⿰鍛＝鐎广儱妫涙晶顒傜磼缂佹绠栫紒缁樼箞瀹曟帒饪伴崘鐐瘒闂傚倷鑳剁划顖滄暜閹烘鍊舵慨妯挎硾妗呴梺鍛婃处閸ㄦ壆绮婚幎鑺ョ厱闁斥晛鍟ㄦ禒锕€顭跨憴鍕婵﹦绮幏鍛槹鎼存繆顩紓鍌欑劍瑜板啫顭囬垾宕囨殾婵°倐鍋撴い顐ｇ矒閸┾偓妞ゆ帒瀚繚婵炶揪绲跨涵璺何ｉ崼銉︾厪闊洤艌閸嬫捇寮妷銉ゅ闂佺粯鍨兼慨銈夋偂閻樼數妫柡澶婄仢閼哥懓顭胯閹告娊寮婚敐鍛婵炲棙鍔曠壕鍐测攽椤旂》鍔熺紒顕呭灦楠炲繘宕ㄧ€涙ê鈧粯淇婇婊冨妺闁伙綆鍓氭穱濠囨倷椤忓嫧鍋撻弽顬℃椽鏁冮崒姘亶婵炲濮撮鍛不閻愮儤鐓忓┑鐐靛亾濞呭棙銇勯妷銉у缂佺粯鐩獮瀣倷閼碱剛鎳栭梻浣筋嚙缁诲娆㈠顒夋綎闁惧繗顫夌€氭岸鏌嶉妷銉э紞闁逞屽墮椤兘寮诲鍫闂佸憡鎸鹃崰搴敋閿濆惟闁挎梻铏庡ù鍕煟鎼搭垳绉靛ù婊呭仱椤㈡瑦绻濋崶銊㈡嫼闁荤喐鐟ョ€氼剟宕濋懜鍏哥箚妞ゆ劑鍨归顐㈩熆鐟欏嫭绀夐柕鍥ㄥ姍楠炴帡骞樼捄鍝勭闂傚倷鐒﹂幃鍫曞磿椤栫偛纾块梺顒€绉甸崐鍫曟煃閸濆嫬鏆熺痪鎯с偢閺岋絽顫滈埀顒€顭囪閸┿垽宕奸悤浣诡啍闂佺粯鍔曢顓熸櫠闁秵鐓曢柟鐑樻尭濞搭噣鏌熼鎯у幋鐎殿喛鍩栭幆鏃堝箻閺夋垹褰鹃梻鍌氬€搁崐椋庣矆娓氣偓楠炴牠顢曢埗鑺ョ☉閳诲酣骞嬪┑鍡欎喊闂備礁婀遍崑鎾诲礈濮橆剦鐒介柡宥冨妿缁犲墽鈧懓澹婇崰鏇犺姳婵傚憡鐓曢柕鍫濇濞搭噣鏌＄仦鐐鐎规洜鍘ч埞鎴﹀炊瑜庨锟犳⒒娴ｇ瓔鍤欏Δ鐘叉啞閺呭爼鎮剧仦缁㈡綗闂佽鍎抽顓㈡偡瑜版帗鐓曢柕澶嬪灥鐎氼喗绂嶉鍫熲拻濞达綀顫夐妵鐔兼煕濡亽鍋㈢€规洘鍔欏畷褰掝敋閸涱厽顓跨紓鍌氬€烽悞锕傗€﹂崶鈺佸К闁逞屽墴濮婃椽骞栭悙鎻掑闂佸憡鏌ㄧ粔鎾煝瀹ュ應鍫柛鏇ㄥ幘閻﹀牓姊洪棃娑㈢崪缂佹彃澧藉☉鍨偅閸愨晝鍙嗛梺鍝勬祩娴滅偤鎮鹃悽鍛婄厵妞ゆ洍鍋撶紒鐘崇墵楠炲啫饪伴崗鍓у枛瀹曠兘顢橀悩鍨瘞闂傚倸鍊风欢姘焽瑜旈幃褔宕卞▎鎰簥闂佸湱鍎ら〃鍛矆閸℃褰掓偂鎼达絾鎲奸梺缁樻尰濞茬喖寮婚敐澶婂嵆婵﹢纭稿Σ顕€姊虹拠鏌ョ崪缂佺姵鎹囧濠氭偄绾拌鲸鏅╅梺璇″瀻閸屾侗娼熼梻鍌欒兌椤㈠﹤鈻嶉弴銏″亱闁绘ǹ顕ч弰銉╂煟閹邦剚鎯堢紒鐘虫皑閹插摜浠﹂崜褏褰鹃梺鍝勬川閸婏綁寮崼鐔蜂汗闂傚倸鐗婄粙鎰垝閸洘鈷戦悹鍥皺缁犺尙绱掔拠鑼闁伙絽鍢查埞鎴﹀幢閳哄倸鍏婃俊鐐€栭幐鑽ゆ崲閸曨厾涓嶆慨姗嗗墻濞撳鏌曢崼婵囶棞濠殿啫鍛＜闁圭粯甯炴禒銏ゆ偂閵堝鍊甸柣銏㈡暩閵嗗﹪鏌涚€ｎ偅灏甸柟鍙夋尦瀹曠喖顢楅崒锔惧枠闂傚倷鐒﹂幃鍫曞礉鐎ｎ剙鍨濇繛鍡楁禋閸ゆ洖鈹戦悩瀹犲妤犵偑鍨烘穱濠囧Χ閸滃啫浼愰梺璇″枟閸ㄥ灝顫忛搹鐟板闁哄洨鍋涢埛澶愭⒑闂堟稒宸濋柛瀣ㄥ€濋獮鍐偩瀹€鈧壕鍏间繆椤栨繂浜归柣锕€鐗撳鍝勑ч崶褏浼堝┑鐐板尃閸愵亝鎳冮梻鍌氬€烽悞锔锯偓绗涘厾楦跨疀濞戞锛欏┑鐘绘涧濡盯寮抽敂濮愪簻闁哄稁鍋勬禒婊呯磼閻樼數甯涢柕鍥у瀵噣宕惰濮规姊虹紒妯诲鞍闁搞劌鐏濋～蹇撁洪鍕獩婵犵數濮撮崯顐λ囬埡鍛拺闁硅偐鍋涙慨鍌毭瑰⿰鍐煟妤犵偛鍟村杈╃磼閻樺磭鈽夐柍钘夘槸閳诲酣骞嬪┑鍡欑杽闂傚倸鍊烽悞锕傚几婵傜ǹ鐤炬繛鎴欏灩缁愭鎱ㄥ鍡楀幋闁哄妫冮弻锟犲礃閵娧冾杸闂佺粯鎸婚惄顖炲蓟濞戙垹鐒洪柛鎰典簼閸Ｑ勭箾鐎电ǹ顫掗柛銉ｅ妿閸樻捇鎮峰⿰鍕煉鐎规洘绮岄埢搴ㄥ箻閺夋垟鍋撻懜鍨弿婵妫楁晶濠氭煟閺傛寧顥炲ǎ鍥э躬椤㈡稑顫濇潏銊ф闂備胶枪椤戝棝骞愰幖渚婄稏婵犻潧顑嗛崑鍌炲箹濞ｎ剙鐏ù婊冨缁辨捇宕掑▎鎴М闂佺ǹ锕ゅ﹢閬嶅极椤曗偓閺佹捇鎮╅崣澶屸偓顒勬⒑閸涘﹤濮﹂柛鐘崇墱婢规洟宕楅崗鐓庡伎濠碘槅鍨伴妵姗€宕ラ锔界厸闁糕剝娲栧畵鍡涙煛瀹€瀣М闁诡喓鍨藉畷顐﹀Ψ瑜忛崢鎰磽閸屾瑧顦︽い锔诲灦閹儲绺界粙璺ㄧ枃闂佺粯鍔楅崕銈夋倿閼测斁鍋撻獮鍨姎闁瑰弶锕㈡俊鎼佸煛閸屾粌寮抽梻浣告惈閸婃悂鎮樺┑瀣厱闁哄啫鐗婇悡鐔兼煥閺囨浜鹃梺缁橆殔缁绘帒危閹版澘绠虫俊銈傚亾缂佲偓閸垺鍠愮€广儱顦埀顒€鍊圭缓浠嬪川婵犲倻褰呴梺鍝勵槸閻楀嫰宕濇繝鍐洸婵犲﹤瀚ㄦ禍婊堟煙閺夎法浠涚紒鑸电洴濡懘顢楁径濠傗拫闂佸搫鐭夌槐鏇熺閿曞倸绀堢憸搴ㄥ礆濞戞瑧绡€缁剧増锚婢ф煡鏌熼鐓庘偓瑙勭┍婵犲洤閱囬柡鍥╁仜缁愭稑顪冮妶鍡樺暗濠殿喕鍗冲畷鎴﹀冀椤撶啿鎷洪柣鐘叉搐瀵爼宕径瀣ㄤ簻妞ゆ劑鍩勫Σ鎼佹偂閵堝鍙撻柛銉ｅ妿閳藉鐥幆褎鍋ラ柡宀€鍠栭獮鍡涙偋閸偅顥夐梻浣虹帛鐢帡鎮樺璺何﹂柛鏇ㄥ灠缁犳娊鏌熼幖顓炵仩濠殿喗绋撶槐鎾存媴閸撳弶楔闂佽桨绀侀…宄邦嚕鐠囨祴妲堥柕蹇娾偓鍏呮睏闁诲氦顫夊ú鏍洪妶澶婄厱闁割偆鍠撶弧鈧梺闈涢獜缂嶅棗顭囬幇顓犵闁告瑥顦遍惌灞句繆閸欏濮嶆鐐村浮楠炲﹤鐣烽崶褎鐏堥悗瑙勬礈閸忔﹢銆佸鈧幃鈺呭垂椤愶絿妲ユ繝鐢靛Х椤ｎ喚妲愰弴銏犵；闁硅揪绠戠壕褰掓煛瀹ュ骸骞楅柛瀣儐娣囧﹪濡堕崒姘婵＄偑鍊ら崑鍛崲閸喍绻嗛柟闂寸鍞梺褰掑亰閸樼厧霉椤曗偓閺岀喖顢欓悾灞惧櫚闂佽鍠楃划宀冪亽闂佺粯鎸告鍝ョ不閻愮儤鈷掑ù锝囩摂閸ゆ瑩鏌涢幋鐘虫珪缂佽京鍋ゅ畷鍗炩槈濡》绱遍梻浣告啞娓氭宕㈡ィ鍐ㄦ辈闁挎棃鏁崑鎾诲礂婢跺﹣澹曢梻渚€鈧偛鑻晶鎾煕閳规儳浜炬俊鐐€栫敮鎺楀窗濮橆剦鐒介柟閭﹀幘缁犻箖鏌涘▎蹇ｆ闁兼澘娼￠弻鏇㈠幢濡も偓閺嗭絿鈧娲栧畷顒冪亙婵犵數濮撮崐褰掝敊閸岀偞鈷掑ù锝呮啞閸熺偞绻涚拠褏鐣电€规洘绮岄埥澶婎潩椤掆偓琚ｉ梻渚€鈧偛鑻晶顖滅磼缂佹鈯曟繛鐓庣箻瀹曟粏顦查柛鈺佹湰缁绘繈濮€閵忊€虫畬闂佹寧娲忛崐娑㈡倶鐎ｎ亖鏀介柣妯款嚋瀹搞儵鏌ｅΔ鈧敃顏堝春濞戙垹绠虫俊銈勮兌閸橀亶鏌ｈ箛鏇炰沪鐎规洘蓱缁旂喎顫滈埀顒勫蓟瀹ュ牜妾ㄩ梺鍛婃尵閸犳牞妫㈤梺鍦亾閺嬪ジ寮告惔銊︾厪闊洦娲栭灞剧箾鐏忔牗娅婇柟顔款潐閵堬箓骞愭惔顔诲摋闂備礁鎲″Λ鎴犵不閹达腹鈧棃宕橀鍢壯囨煕閳╁喚娈橀柣鐔村姂濮婅櫣绮欓崠鈥充紣闂佺顕滅槐鏇犲垝鐎ｎ喖绠抽柟瀛樻煥閻楁岸姊洪崜鎻掍簽闁哥姵鎹囧畷銏ゅ箚瑜夐弨浠嬫煟閹邦垱纭鹃柦鍕悑閵囧嫰寮撮崱妤佸闁稿﹤鐖奸弻銊╂偄閸濆嫅锝夋煟閹惧娲撮柡灞剧☉閳藉宕￠悙鑼啋闂備胶纭堕弲顏嗗緤妤ｅ啫桅闁告洦鍨伴～鍛存煃閵夈儱鏆遍柟鐣屾暬濮婅櫣鍖栭弴鐔告緬闂佺ǹ顑嗛幐鎼佲€旈崘顔嘉ч柛鈩冾殘閻熴劑鏌ｆ惔銏犲毈闁告挾鍠栭獮蹇涘箣閿旇棄浜滈柣鐐寸▓閳ь剙鍘栨竟鏇炍旈悩闈涗粶闁诲繑绻堝畷婵嗩潩椤撶姷顔曢柣蹇曞仜閸婂憡淇婇崹顕呯唵閻熸瑥瀚粈瀣偓瑙勬礃椤ㄥ﹤顫忛懡銈傚亾闂堟稒鎲搁柣锝囧亾娣囧﹪鎮欓鍕ㄥ亾閺嶎厽鍋嬫俊銈呭暞瀹曡尙鈧箍鍎遍ˇ顖炲触鐟欏嫮绠鹃柛鈩兠悘鈺備繆椤愵偄鐏﹂柡宀€鍠栭、娑㈠幢濡も偓閺嗙偞鎱ㄩ敐鍡楀闁哄睙鍛＜婵☆垳鍘ч埛宀勬⒑閸濆嫮鐒跨紒韫矙閿濈偛饪伴崼婵堝姦濡炪倖宸婚崑鎾淬亜椤撶偞绌挎い锕€纾槐鎺楀磼濞戞鐟ㄥΔ妤婁簷閸楀啿鐣烽悢鐓庣厸闁逞屽墰閼鸿鲸绻濆顓涙嫼闂佽崵鍠愭竟鍡涘箺閻樼數纾奸悹鍥皺瀛濋柧浼欑秮閺屽秷顧侀柛鎾跺枛楠炲啫螖閳ь剟鍩㈤幘璇插瀭妞ゆ梻鏅ぐ顖炴⒒娴ｅ懙褰掝敄閸涘瓨鏅濋柕鍫濐槹閸嬧晝鈧懓瀚伴崑濠傤焽閵娾晜鐓冪憸婊堝礈閻旂厧违濞达絿纭堕弸搴ㄦ煙閻愵剚缍戞繛鍫熷姍濮婅櫣鈧湱濮甸妴鍐磼閳ь剚鎷呯粵瀣緭濠电姷鏁告慨鐑姐€傞鐐潟闁哄洢鍨圭壕濠氭煙鏉堝墽鐣辩痪鎯х秺閺屸€愁吋鎼粹€茬凹闂佸搫妫欑划鎾诲蓟閻斿吋鍊绘慨妤€妫欓悾鍫曟⒑濮瑰洤鈧倝宕抽敐澶婅摕闁绘柨鐨濋弸鏃堟煕椤垵鏋熼柣蹇旀尰缁绘盯骞嬮悙鏉戠缂備礁顦伴幐鍐诧耿娓氣偓濮婅櫣绱掑Ο蹇ｄ簻铻ｅ┑鐘叉搐绾惧潡鏌熼幍顔碱暭闁绘挸鍟村娲垂椤曞懎鍓板┑鐐存儗閸ｏ綁寮婚敍鍕勃闁告挆浣插亾閹烘嚚褰掓偑閸涱垳鏆ら悗瑙勬礃缁繘藝閹惰姤鐓曢柨婵嗛楠炴绱掓潏銊﹀鞍闁瑰嘲鎳橀幃鍧楊敍濮橆剙鑵愭繝鐢靛Л閹峰啴宕熼崹顐ゆ毉闂備胶鎳撶粻宥夊垂瑜版帒鐓″璺号堥弸搴ㄧ叓閸ャ劍绀堥柣娑卞櫍濮婅櫣鎷犻幓鎺戞瘣缂傚倸绉村Λ婵嗙暦閹达箑骞㈡繛瀛樻緲濞诧妇鈧絻鍋愰埀顒佺⊕閿氶柍褜鍓涢弫濠氬箖瀹勬壋鏋庨煫鍥ㄦ惄娴犲墽绱撴担鎻掍壕闂佸憡鍔戦崝澶愬绩娴犲鐓熼柟閭﹀灱閸ゅ妫呴澶婂闁逞屽墯椤旀牠宕抽鈧畷鎴炵節閸屾粍娈惧┑顔姐仜閸嬫挻銇勯姀锛勬噰闁硅櫕鐗犻崺锛勨偓锝庡墮缁ㄣ儲绻濋悽闈浶ｆい鏃€鐗犲畷鏉课旈崨顔芥珖闂侀潧绻堥崹娲礂濠婂牊鐓欓梻鍌氼嚟椤︼箓鏌ょ粙璺ㄧШ闁哄矉缍佹慨鈧柣妯哄暱閺嗗牓姊洪幎鑺ユ暠閻㈩垱甯″﹢渚€姊洪幐搴ｇ畵闁绘妫滈。璺ㄧ磽娴ｉ缚妾搁柛妯恒偢閹儲绺界粙鑳憰濠电偞鍨崹娲疾閺屻儳鍙撻柛銉ｅ妽缁€鍫ユ煛閸℃劕鈧洝鐏冮梺缁橈耿濞佳勭濠婂懐纾肩紓浣癸公閼拌法鈧鍣崑濠囩嵁濡偐纾兼俊顖滅帛椤忕喖姊绘担鑺ョ《闁革綇绠撻獮蹇涙晸閿燂拷
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆忣啅椤旇棄鐦滈梻渚€娼ч悧鍡椢涘Δ鍐當闁圭儤顨嗛悡銉╂煛閸ユ湹绨绘い鈺婂墰缁辨帡顢欓悾灞惧櫙缂備胶濮甸惄顖氼嚕椤曗偓瀹曟帒饪伴崨顕呮綋闂傚倸鍊风欢姘焽瑜庨〃銉ㄧ疀閺囩偟绛忛梺鍛婃寙閸曨偄骞戦梻浣虹帛閸旓箓宕楀▎鎰洸婵犲﹤鐗婇悡娆撴倵濞戞瑯鐒界紒鐘崇墵閺屾盯鎮╃粵纭呪偓鍧楁煛鐏炵偓绀嬬€规洖宕灃濞达絽鎲￠悾缁樹繆閵堝洤啸闁稿鍋ら獮鎴﹀炊椤掑倸绁﹂梺鐟扮摠閹﹪寮崼婵堝姦濡炪倖甯掗崐鑽ょ矆婵犲倵鏀介柣妯哄级閹兼劗绱掗悩宕囧⒌闁哄苯绉靛顏堝箥椤旇法鐛ラ梻浣告憸閸犲酣骞婅箛娑樜﹂柛鏇ㄥ灠缁秹鏌涚仦鎹愬濞寸姵蓱缁绘繈濮€閵忊€虫畬闂佺粯顨呯换鎺懳ｉ幇鏉跨閻庢稒锚椤庢捇姊洪崨濠勭畵閻庢凹浜濈粩鐔煎即閵忊檧鎷虹紓浣割儏濞硷繝顢撳Δ鍛厱闁绘棃鏀遍崑銉╂煃閵夘垳鐣电€规洖鐖奸、妤佹媴鐟欏嫷浠ч梻鍌欒兌缁垱鐏欓柣蹇撴禋娴滎亪鐛Δ鍛嵆闁绘劏鏅滈弬鈧梻浣虹帛閸旀牕顭囧▎鎾村€堕柣鏂垮悑閻撴洟鏌曟繛鍨姕閻犳劧绱曠槐鎺楊敊濞嗘儳娈梺瀹犳椤︻垶鍩㈡惔鈽嗗悑闁告洦鍘惧▔鍧楁⒒閸屾瑨鍏岄柟铏崌楠炴牠顢曢妶鍥╃厠闂佺粯鍨归悺鏃堝极婵犲偆鐔嗛柤鎼佹涧婵牓鏌ｉ幘瀵告创闁诡喗顨婇弫鎰償閳ヨ尙鏁栭梻浣烘嚀閸熷灝煤閿曞倸绠為柕濠忓缁♀偓闂佸憡渚楅崳顔界閳哄啰纾藉ù锝勭矙閸濇椽鎮介娑樼闁诲繑甯″娲箚瑜忕粻鐗堢節閳ь剟鏌嗗鍛枀濡炪倕绻愬Λ娑氬娴犲鐓曢悘鐐插⒔閹冲懘鎮归幇顏勫祮闁哄本鐩幃鈺呭箛娴ｅ湱鏉归梻浣筋嚃閸犳岸宕楀鈧畷娲焺閸愵亞鎳濋梺鎼炲劀閸曢潧鎮梻鍌欐祰瀹曞灚鎱ㄩ弶鎳ㄦ椽鏁傞崜褏鐒鹃梺鍝勵槹閸わ箓鏁愭径濠囧敹闂侀潧顧€婵″洭宕㈤鍫燁棅妞ゆ劑鍨烘径鍕箾閸欏澧柡鍡忔櫊濮婄粯鎷呴崷顓熻弴闂佺ǹ锕ラ崝娆忕暦閹达箑绠荤紓浣骨氶幏濠氭⒑缁嬫寧婀伴柣鐔村姂瀹曟鐣濋崟顐㈠殤濡炪倕绻愬Λ娑氬閼测晝纾藉ù锝咁潠椤忓懏鍙忕€广儱妫▓浠嬫煟閹邦厽鍎楁繛鍫熸⒐椤ㄦ粓顢旈崘銊ョギ閻庤娲滈崢褔鍩為幋锕€閱囨い鎰跺強閻斿吋鈷戦柤濮愬€曢弸鎴犵磼椤旂厧顒㈤柡鍛版硾铻栭柛鎰ㄦ櫆濞堟澘鈹戞幊閸婃洟骞婃惔銊﹀亗闁哄洢鍨洪悡娆撳级閸儳鐣烘俊缁㈠櫍閺岋綁骞樼€涙顦ㄧ紓浣虹帛缁诲牆螞閸愩劉妲堟慨妤€妫旈幃锝夋⒒娴ｅ憡鍟炴慨濠勬嚀閻ｆ繈骞栨担姝屾憰闂佹寧绋戠€氀囧磻閹剧粯鏅查幖瀛樼箞閸嬫姊洪崨濠勬噧閻庢矮鍗冲濠氬灳瀹曞洦娈曢梺閫炲苯澧寸€规洑鍗抽獮妯兼嫚閼碱剛宕跺┑鐘垫暩婵潙煤閵堝洦鍏滈柍褜鍓熷铏圭磼濡搫顫嶅┑鐘灪閿曘垹顕ｉ幖浣搁唶婵犻潧鍟弬鈧梻浣虹帛钃辨い鏃€鐗犲鎶筋敍閻愬鍘遍梺瑙勫劤椤曨厾绮婚悙鎼闁绘劖娼欐慨宥嗩殽閻愭煡鍙勯柟顔煎⒔娴狅箓鎮剧仦璇蹭喊闂傚倸鍊风粈浣革耿闁秴纾块柕鍫濐槶閳ь剙鍟村畷銊╊敍濠娾偓缁楀姊洪幐搴ｇ畵婵☆偄瀛╃粋宥呪堪閸喓鍘甸梺纭咁潐閸旀牜娑垫ィ鍐╃厸閻庯綆浜崣鍕煛鐏炲墽顬肩紒鐘崇洴楠炴瑩宕熼鍏煎創濠电姵顔栭崰鏍晝閿旀儳鍨濇い鏍仜缁狀垶鏌涘☉妯兼憼闁稿绻濆娲敇閵娧呮殸濠电偛鐗婄换鍕閹捐纾兼繛鍡樺姉閵堟澘顪冮妶鍡樿偁闁搞儜鍜冪串闂備胶顫嬮崟鍨暭閻庤鎸风粈渚€鍩為幋锔藉亹闁圭粯甯婄花浠嬫⒑閸涘﹨澹樻い鎴濐樀瀵鈽夐姀鐘靛姶闂佸憡鍔︽禍婵嬪闯椤曗偓閹嘲饪伴埀顒勫础瀹曞洨涓嶉柡宓本缍庡┑鐐叉▕娴滄粍瀵奸悩缁樼厱闁哄洢鍔屾晶顕€鏌涚€ｎ亞效婵﹥妞介獮鏍倷閹绘帩鐎存繝纰夌磿閸嬫鍒掑▎蹇曟殾闁告繂瀚уΣ鍫ユ煏韫囨洖啸妞ゆ柨锕娲偡閹殿喗鎲奸梺鑽ゅ枂閸ㄨ棄鐣烽幋锕€绠婚悹鍥皺椤斿﹪姊虹憴鍕剹闁告ü绮欏畷鎾绘偨閸涘ň鎷洪梺鍦瑰ù椋庣不閹剧粯鐓欓柛鎰皺鏍＄紓浣规⒒閸犳牕顕ｉ幘顔碱潊闁抽敮鍋撻柟椋庣帛缁绘盯骞橀弶鎴犲姲闂佺ǹ顑嗛幑鍥蓟瀹ュ牜妾ㄩ梺鍛婃尰閻╊垰鐣烽幋婵冩闁靛繆鈧櫕顓烘俊鐐€栭悧妤冪矙閹烘垟鏋嶉柣妯款梿瑜版帗鍋傞幖杈剧稻閹插ジ姊虹紒妯诲鞍婵炶尙鍠栭獮鍐ㄎ旈崨顔芥珳闁硅偐琛ラ埀顒冨皺閺佹牗绻濋悽闈涗粶鐎殿喖鐖奸獮鎰板箮閽樺鎽曢梺缁樻椤ユ捇寮抽敃鍌涚厱闁靛鍨电€氼喗绂嶉鍛箚闁绘劦浜滈埀顒佸灴瀹曞綊鎼归崷顓犵厯闂佺鎻粻鎴︽偂閳ユ剚鐔嗛悹鍝勫娇閸儱鍑犻幖娣妽閻撴瑩姊洪銊х暠闁哄绋戦埞鎴︽倷閸欏鎮欓梺瀹狀潐閸ㄥ潡寮澶婄妞ゆ劏鍓濆鈧梻鍌欒兌鏋柨鏇樺劦閹囨偐鐠囪尙鐣洪悷婊冪Ч閵堫亝瀵奸弶鎴﹀敹濠电娀娼ч悧鐐垫闁秵鈷掑ù锝堟鐢盯鏌涢弮鈧〃濠傜暦閺囥垹围濠㈣泛锕﹂敍娑㈡⒑缁嬭法鐏遍柛瀣仱閹€斥槈閵忥紕鍘遍梺闈涱檧缁蹭粙宕濆鑸电厽闊浄绲奸柇顖炴煛瀹€瀣М妤犵偞顭囬埀顒勬涧閹诧繝宕氬☉銏♀拺闁告繂瀚﹢浼存煟閳哄﹤鐏﹂柣娑卞櫍瀹曞爼顢楅埀顒傜矆閸緷褰掓晲閸よ棄缍婇幃锟犲磼濮橈絾鏂€闂佺粯鍔樼亸娆撳箺閻樼數纾兼い鏃囧Г鐏忕數绱掗纰辩吋鐎殿喖鈧噥妾ㄥ┑鐐插悑閻楃娀寮昏缁犳盯骞樼壕瀣攨缂備胶鍋撻崕鎶藉触鐎ｎ喖绠為柕濞垮剻閺冨牆鐒垫い鎺嗗亾閾荤偞绻涢崱妯诲鞍闁稿鏅犻弻娑滅疀濮橆兛姹楃紓浣叉閸嬫捇姊绘担鍦菇闁搞劏妫勯…鍥槼缂佸倸绉烽妵鎰板箳閹捐泛骞堥梺璇插嚱缂嶅棝宕戦崨顓犳殾閻忕偟鍋撻崣蹇撯攽閻樻彃鏆為柕鍥ㄧ箖娣囧﹪顢曢姀鐘虫闂佸疇妫勯ˇ顖炲煝鎼粹垾鐔访虹紒姗堥獜缂傚倸鍊搁崐鎼佸磹妞嬪孩濯奸柡灞诲劚绾惧鏌熼幑鎰滅憸鐗堝笚閺呮煡鏌涢妷锝呭闁绘繍浜濈换娑氣偓娑欘焽閻鏌ｉ悢鍙夋珔妞ゎ偄绻掔槐鎺懳熺拠宸偓鎾绘⒑閸涘﹦鈽夐柨鏇樺€濆鎶藉醇閻旇櫣顔曢梺瑙勫劤椤曨參銆傞崣澶堜簻闁靛ǹ鍎诲銉╂煟閿濆鏁遍悗闈涖偢瀵爼骞嬪┑鍡樻殢濠碉紕鍋戦崐鏍箰妤ｅ啫纾婚柣鏃傚帶缁€澶愭煥閺囨浜惧銈庝簻閸熷瓨淇婇崼鏇炲耿婵°倕鍟伴幊鍡涙⒒娴ｅ懙褰掓晝閵夆晛绠栭柛灞炬皑閺嗭箓鏌曡箛瀣偓鏇犵不缂佹ǜ浜滈柡鍐ㄥ€瑰▍鏇㈡煕濡粯鍊愰柡宀嬬秬缁犳盯寮撮悙鎵崟濠电姭鎷冮崒婊呯厯闂佺硶鏂傞崕鎻掝嚗閸曨垰绠涙い鎾跺Т鐢姊洪懡銈呮瀾闁荤喆鍎抽埀顒佸嚬閸撶喖骞冨Ο灏栧亾閻㈢數銆婇柛瀣尵閹叉挳宕熼鍌ゆК闁诲氦顫夊ú锕傚礈濠靛鍊堕柛鎰ㄦ櫇缁♀偓闂佹眹鍨藉褍鐨梻浣哄帶閸熸寧鏅舵惔銏㈢彾闁哄洨濮电€氭碍绻涢弶鎴剱闁哄倵鍋撻梻鍌欑劍鐎笛呯矙閹寸姭鍋撳鐓庡缂佸倸绉电缓浠嬪川婵犲嫬骞堝┑鐘垫暩婵挳宕悧鍫熸珷闁汇垻顣介崑鎾斥枔閸喗鐏堝銈庡弮閺€杈ㄧ┍婵犲洤绠瑰ù锝呮憸閸樺憡绻涙潏鍓хК婵炲拑缍佸鎼佸礃椤旇В鎷绘繝鐢靛Т妤犲憡绂嶅⿰鍕閻犲泧鍛殸闁绘挶鍊栨穱濠囶敍濠靛浂浠╅梺鍝勬４缁犳捇寮诲☉妯锋瀻闊浄绲炬缂傚倷璁插褔宕戦幘缁樷拻闁稿本鑹鹃埀顒佹倐閹勭節閸愵亞鎳濆┑掳鍊曢幊搴ｅ閸ф鐓欓柟娈垮枛椤ｅジ鏌ｉ幘瀛樼闁诡喗顨婇弫鎰板礃閵娿儺鐎冲┑鐘愁問閸犳牠宕愯ぐ鎺戠疄闁靛⿵濡囩弧鈧梺绋胯閸婃宕ョ€ｎ喗鍊甸悷娆忓缁€鈧梺闈涚墛閹倿鐛崘顔碱潊闁绘ê鐏氬▓婵嬫⒑鐟欏嫷鍟忛柛鐘冲浮瀹曟垿骞樺ú缁樻櫌闂佸憡娲﹂崗姗€骞忓ú顏呪拻闁稿本姘ㄦ晶娑氱磼鐎ｎ偅灏电紒顔碱煼閹瑩鎮滃Ο鐓庡箞濠电姷鏁告慨鎾疮閻楀牊娅犻柣銏犳啞閻撴瑦銇勯弴鐐搭棤缂佲檧鍋撻柣搴㈩問閸犳骞愰幎钘夌畺婵炲棙鎸搁悡娑㈡煕鐏炲墽鈯曟繝銏″灴濮婂宕掑▎鎺戝帯缂備緡鍣崹鍫曞极鐎ｎ喗鍊垫繛鍫濈仢閺嬫盯鏌ｉ弽顐㈠付闁伙綁顥撻埀顒€婀辨慨鐗堢瑜版帗鐓欓柣鎴炆戠亸鐢告煕濡搫鑸归柍瑙勫灴椤㈡瑩寮妶鍕繑闂備礁鎲￠幐璇差潩閵娾晛绠憸鐗堝笚閺咁剟鏌涢弴銊ュ箻闁挎稒绮撻弻鈩冨緞婵犲嫬顣堕梺鍛婃煥閼活垶婀佸銈嗘煥椤洘绂嶅⿰鍫熺厪闊洢鍎崇壕鍧楁煙閸愬弶鍠橀柡宀€鍠庨悾鐑藉炊瑜屽Σ鎰節绾板纾块柛蹇斏戦幈銊╁焵椤掑嫭鐓冮柕澶涚畱婢ь垰霉閸忓吋绀堢紒杈ㄦ崌瀹曟帒顫濋钘変壕闁归棿绀佺壕鍦偓鐟板閸ｇ銇愰幒鎴犲€為悷婊勭矊闇夐柡宥庡幗閻撳繐鈹戦悙鎴濆暙閺嗘瑩鏌ｈ箛鎿勫伐闁宠鍨块幃娆撳矗婢舵ɑ顥ｇ紓鍌欒兌缁垳鏁垾鎰佸殨闁规儼濮ら弲婵嬫煕鐏炲墽銆掗柛姗嗕簼缁绘繈鎮介棃娴躲垻绱掔紒妯哄闁靛棗鎳橀、妤呭磼濠婂拑绱查梻浣瑰▕閺侇噣宕戦幘缁樼厱閻庯綆鍋呭畷灞炬叏婵犲嫮甯涢柟宄版嚇瀹曘劍绻濋崒娑欑暭濠电姷鏁搁崑鐘活敋濠婂懐涓嶉柟鎯х－閺嗭箓鏌熸潏鍓х暠缂佲偓鐎ｎ偁浜滈柟鎹愭硾灏忕紓浣诡殔椤﹂潧顫忛搹鍦＜婵☆垰鎼～搴ㄦ⒑缁嬭法鏄傞柛濠冪箞瀹曞搫鈽夐姀鐘殿唺濠德板€撻懗鍫曞储闁秵鐓熼煫鍥ㄦ礀娴犳粌顭胯缁瑩鐛繝鍥х妞ゆ挾濮烽敍婊勭箾鏉堝墽鍒伴柟纰卞亰閵嗗倹绺介崨濠勫帾闂佹悶鍎滈崘鍙ョ磾婵＄偑鍊戦崹鍝劽洪悢鍛婂弿闁逞屽墴閺屽秹宕崟顐熷亾閻㈢ǹ鍑犻柛娑樼摠閻撶喖骞栧ǎ顒€鈧倕顭囬幇顓犵闁告瑥顥㈤鍡楀疾闂備礁鎼粙渚€宕㈡禒瀣；闁冲搫鎳忛悡鐔兼煙鏉堝墽绋绘い銉ヮ樀閺岋綁骞囬濠傚闂佺灏欐灙妤楊亙鍗冲畷妤呮倷椤掑倵妲堝Δ妤婁簷閸楀啿鐣峰鈧幃娆擃敆閳ь剟鎯佸⿰鍫熲拻濞撴埃鍋撴繛浣冲洦鍋嬮柛鈩冦亗濞戞ǚ鏋庨柟瀛樼矌閸撱劑姊哄Ч鍥х仼闁硅绻濋幃锟犲即閵忊€斥偓鐢告煥濠靛棝顎楀褜鍠栭湁闁绘ê纾惌宀勬煙椤曞懎鏋涢柍璇查叄楠炴鈧數枪楠炲牓姊绘担鍛婃儓婵炲眰鍨藉畷婵堜沪閸撗屾锤闂佽澹嗘晶妤呮偂濞戙垺鐓冪憸婊堝礈濮樿京鐭夐柟鐑樻煛閸嬫捇鏁愭惔鈥茬敖闂佸憡鐟ュΛ妤呭煘閹达附鍊烽柡澶嬪灩娴犳悂姊洪幐搴ｎ暡闁靛洤瀚粻娑欑節閸愨晝褰呴梻鍌氭搐椤︽壆鎹㈠┑鍥╃瘈闁稿本纰嶅▓褔姊虹粙娆惧剭闁搞劋绮欏濠氭晬閸曨亝鍕冮梺鍛婃寙閳ь剚鎱ㄩ崶鈺冪＝濞达綀娅ｇ敮娑氱磼鐠囪尙澧曢柣锝呭槻椤粓鍩€椤掍椒绻嗛柟闂寸鍞銈嗘⒒閺咁偆绮欓崼銉︹拻濞达絿鐡旈崵娆撴倵濞戞帗娅婃い銏＄懇瀵粙顢橀悙鐢靛炊婵犲痉鏉库偓鏇㈠箠韫囨稑纾婚柛宀€鍋為悡銉╂煟閺傛寧鎯堢悮銊╂⒑闁偛鑻崢鍝ョ磼閼镐絻澹樻い鏇秮椤㈡岸鍩€椤掆偓閻ｇ兘鎮℃惔妯绘杸闂佺硶鍓濋崺鍐磻閹捐鍨傛い鏃囶潐閺傗偓闂備胶绮崝鏇烆嚕閸泙澶娾堪閸曨厾顔曢柣搴ｆ暩鏋柛妯绘尦閺岀喖顢欓悡搴⑿╅梺瀹狀潐閸ㄥ綊鍩€椤掑﹦绉甸柛瀣у亾闂佸綊顥撶划顖滄崲濞戞瑦缍囬柛鎾楀嫬浠归梻浣哄劦閸撴繈寮婚妸鈹锯偓锕傚炊椤掆偓缁犳稒銇勯弮鍌涘殌濞存粓绠栭幃宄扳枎韫囨搩浠剧紓浣插亾闁告劦鍠楅悡銉︽叏濡灝鐓愰柣鎾跺枛閺屻劌鈹戦崱妯绘倷闂佸憡锚閻°劍绌辨繝鍥舵晝闁靛繒濮靛▓顓㈡⒑鐎圭姵顥夋い锔诲灦閿濈偛鈹戠€ｅ灚鏅ｉ梺缁樻磻缁€渚€寮查鈶╂斀闁绘ê鐏氶弳鈺呮煕鐎ｎ剙浠辨鐐村姈缁绘繂顫濋浣剐氶梻浣告啞閸旓箓宕伴弽顐㈩棜濠电姵纰嶉悡娆撴煕閹炬鎳庣粭锟犳⒑缂佹ɑ灏扮紒瀣崌閸┾偓妞ゆ巻鍋撻柛妯荤矒瀹曟垿骞樼紒妯煎帗閻熸粍绮撳畷婊堟偄妞嬪孩娈炬繛鏉戝悑濞兼瑩宕橀埀顒€顪冮妶鍡樺暗濠殿喖顕划濠囨晝閸屾稈鎷虹紒缁㈠幖閹冲繗銇愯缁辨帡鎮╅崘鑼患缂備緡鍠涢褔鍩ユ径鎰潊闁抽敮鍋撻柟宄邦煼濮婃椽宕滈幓鎺嶇敖闂佸摜濮甸幑鍥春婵犲洤鍗抽柣鏃傜節缁ㄥ姊洪崫鍕偓鎼佹倶濠靛鏁傛い鎰╁€楃壕濂稿级閸稑濡界€规洖鐬奸埀顒冾潐濞叉鏁幒妤€鐓濋幖娣妼缁犳娊鏌熺€涙绠撻柤鍨姈缁绘繈鎮介棃娴讹絾銇勯弮鈧悧鐘茬暦娴兼潙鍐€妞ゆ挾鍠庨埀顒冨煐閹便劌螣閸ф鎽甸悗瑙勬礀瀵墎鎹㈠☉銏犵闁绘劕鐏氶崳顔剧磽娴ｅ弶顎嗛柛瀣崌濮婄粯鎷呯憴鍕哗闂佺ǹ瀛╃划鎾崇暦閸楃倣鐔烘嫚閸欏鐦掗梻鍌氬€搁崐鎼佸磹閻戣姤鍤勯柛鎾茬閸ㄦ繃銇勯弽顭戝殼婵炴垶鐟ч悷褰掓煃瑜滈崜姘┍婵犲洤绠绘い鏃囧亹閸旓箑顪冮妶鍡楃瑨閻庢凹鍙冮幃锟犲即閵忊€斥偓鐢告煥濠靛棝顎楀ù婊勭箞閹ǹ绠涢敐鍛睄闂佸搫鑻粔褰掑箰婵犲啫绶炴俊顖涗憾閿曞倹鈷戦悗鍦閸ゆ瑥螖閻樺磭鎽冮柣蹇斿浮濮婅櫣绱掑Ο璇茬婵°倗濮撮幉锟犮€冨▎鎴犵＝闁稿本鑹鹃埀顒佹倐瀹曟劙骞栨担鍝ワ紮闂佺粯鍨兼慨銈夊吹閸曨垱鐓犵紒瀣硶娴犳盯鏌涘▎蹇曠闁哄矉缍佹慨鈧柍鎯版硾濠€杈╁垝婵犳艾绾ч幖瀛樻尰閺傗偓闂備胶绮敋闁汇倕娲︾粩鐔肺熼懖鈺冿紲闂佺粯锕㈠褎绂掗柆宥嗙厸閻忕偟鍋撶粈鍐磼缂佹顬兼い锔界叀閹顫濋悡搴♀拫閻庢鍠楅幃鍌氱暦閹烘鍊风紒顔款潐鐎氬ジ姊绘担鍛婂暈缂佸鍨块弫鍐Ψ閿曗偓缁剁偤鏌涢弴銊ョ仭闁绘挻娲樻穱濠囶敍濠婂啫浠橀柣銏╁灥閸╂牠濡甸崟顖氬嵆妞ゅ繐妫涜ⅵ濠电姷顣介崜婵嬪箖閸屾稐绻嗛柣鎴ｆ椤懘鏌嶉埡浣告殲婵犮垺鍨甸埞鎴︽晬閸曨偂鏉梺绋匡攻閻楁洟锝炶箛鏃傜瘈婵﹩鍎甸妷鈺傚€甸柨婵嗙凹閹查箖鏌涢悢閿嬪殗闁哄本娲樺鍕幢濡崵褰嗛梻浣虹帛閹稿鎮疯閹偓妞ゅ繐鐗嗙痪褔鏌涢…鎴濇灕闁糕晝鍋炵换娑氣偓娑欘焽閻绱掔拠鑼ⅵ鐎殿喖顭烽弫鎾绘偐閹绘帞鐛╂俊鐐€栭崝鎴﹀垂鐟欏嫭鍙忛柨鏇楀亾闁宠鍨块、娆戞兜闁垮鏆版繝纰夌磿閸嬫鍒掑▎鎾村仒妞ゆ洍鍋撶€殿喕绮欓、姗€鎮㈢亸浣镐壕闁绘垼濮ら悡娆戠磽娴ｅ顏嗙箔閹烘鎳氭慨妯垮煐閳锋帡鏌涚仦鍓ф噮妞わ讣绠撻弻鐔烘嫚瑜忕弧鈧悗瑙勬礃缁诲牆顕ｉ幘顔藉亜闁告稑锕﹁ぐ鍨攽閻愬樊鍤熷┑顔芥尦椤㈡牠宕ㄧ€涙ê鈧潡鏌涢…鎴濅簴濞存粍绮撻弻鐔煎传閸曨厜銈嗐亜閿旇偐鐣甸柡灞界Ф缁辨帒螣閼姐値妲伴梻渚€鈧稓鈹掗柛鏂跨Ф閹广垹鈹戠€ｎ亜绐涘銈嗘礀閹冲秹宕Δ鍛拻濞达絽鎽滅粔鐑樸亜閵夛附灏扮紒缁樼洴閸┾偓妞ゆ帊绶″▓浠嬫煟閹邦剛鎽犵€规洖鏈〃銉╂倷閺夋垹鐟ㄩ梺瀹狀潐閸ㄥ灝鐣烽崡鐑嗘富闁逞屽墮婵倹鎱ㄦ繝鍌ょ吋鐎规洘甯掗埢搴ㄥ箣椤撶啘婊堟⒒娴ｄ警鏀版い鏇熺矌閹广垹鈹戦崱娆愭閻熸粎澧楃敮妤呭磻閵娾晜鐓熼柕蹇嬪灪閺嗏晜銇勯敂璇茬仭缂佺粯绻堥幃浠嬫濞磋翰鍨介弻娑欑節閸屻倗鍚嬮悗娈垮枟婵炲﹪寮崒鐐村仼鐎光偓閳ь剟鎯侀崼鐔虹瘈闁汇垽娼у瓭闂佺ǹ锕ラ幃鍌氼嚕閾忣偆绡€闁搞儯鍔庨崢鎼佹煟鎼搭垳绉靛ù婊庝邯瀹曪綁宕熼娑氬幐閻庡厜鍋撻柍褜鍓熷畷浼村箻閼告娼熼梺鍦劋椤ㄥ懘锝為崨瀛樼厽婵☆垰鎼幃浣虹磼椤旇姤灏︽慨濠勭帛閹峰懘宕ㄩ棃娑氱Ш妞ゃ垺鐗犲畷鍗炩槈濡⒈鍞归梻浣规偠閸庢椽宕滃▎鎾冲嚑濞撴埃鍋撻柡灞剧洴楠炲洭妫冨☉娆戜邯缂備胶鍋撻崕鎶藉触鐎ｎ喖绠為柕濞垮剻閻旂厧浼犻柛鏇ㄥ枛閻撴﹢姊绘笟鈧埀顒傚仜閼活垱鏅舵导瀛樼厱閻庯綆鍋嗗ú鎾煃閵夛附顥堢€规洘锕㈤、娆撳床婢诡垰娲﹂悡鏇㈡煃閳轰礁鏋ゆ繛鍫燂耿閺岋綁鎮㈢粙鍨潚濠殿喖锕ㄥ▍锝囨閹烘嚦鐔访洪鍕杺缂傚倸鍊烽懗鑸垫叏閻㈢ǹ鍨傞柛褎顨呴弰銉╂煏韫囨洖顎岄柛姘儏椤法鎹勯悮鏉戜紣闂佸摜鍋戦崝宀勨€旈崘顔嘉ч柛鈩兠弳妤呮⒑閸濄儱鏋庢繛纭风節閻涱喛绠涢幘浣规そ椤㈡棃宕ㄩ鍛伖闂傚倷绀侀幉锛勭矙閹达附鏅濋柨鏇炲€搁悡鈥愁熆閼搁潧濮堥柣鎾卞劦閺岋繝宕堕…瀣典邯瀹曟繂饪伴崨顏勪壕闁割煈鍋呯欢鏌ユ倵濮橆厽绶查柣锝囧厴閹垻鍠婃潏銊︽珫婵犵數鍋為崹鍫曟嚌妤ｅ啰宓侀柛顐犲劜閳锋帒霉閿濆洨鎽傛繛鍏煎姍閺屾稒鎯旈姀鐘灆濡炪們鍨哄畝鎼併€佸鈧慨鈧柣妯兼暩閺嬪啴姊绘担瑙勫仩闁稿寒鍨跺畷婵囩節閸愵亶娲稿┑鐘诧工閻楀﹪鎮¤箛娑氬彄闁搞儜宥呬紣婵犫拃鍕弨闁诡喗枪缁犳盯鏁愰崨顓犵潉闂備礁鎼懟顖滅矓瑜版帒绠板┑鐘插暙缁剁偞淇婇婊冨姦闁哄鎳庨埞鎴︽偐閹颁礁鏅遍梺鍝ュУ鐢€崇暦閹达箑绠荤紓浣骨氶幏娲⒑鐟欏嫬鍔跺┑顔哄€濆鍐测堪閸喓鍘遍柣搴秵閸嬪懎鐣峰畝鍕厸鐎光偓閳ь剟宕伴弽顓炶摕闁靛ě鈧崑鎾绘晲鎼粹€茬按婵炲濮伴崹褰掑煘閹达富鏁婄痪顓㈡敱閺佹儳鈹戦敍鍕哗婵☆偄瀚伴幃楣冩倻閽樺）鈺呮煥閺傚灝鈷旈柣锕€鐗撳娲川婵犲啰锛橀梺鍛婃煥缁夊綊鏁愰悙宸悑濠㈣泛顑傞幏缁樼箾鏉堝墽鎮奸柟铏崌椤㈡艾饪伴崟顓狀啎闂佸湱绮敮鐐电不閼姐倐鍋撶憴鍕婵炶尙鍠栭悰顔芥償閵婏箑鐧勬繝銏ｆ硾閼活垱鎱ㄩ敂鎴掔箚闁绘劦浜滈埀顒佺墪椤斿繑绻濆顒傦紱闂佺懓澧界划顖炴偂閻旇偐鍙撻柛銉ｅ妽閻撱儵鏌涘┑鍥ㄣ仢闁哄瞼鍠栭、娆撳箚瑜嶉獮瀣⒑缁洘娅旂紒缁樼箓閻ｇ兘濡搁埡濠冩櫍闂佹枼鏅涢崯顖炲疮閸濆嫧鏀介柣妯虹仛閺嗏晠鏌涚€ｎ剙浠辩€规洖缍婇獮鍡涘级鐠恒劎鈻曟繝鐢靛Х椤ｄ粙宕滃┑濞夸汗闁告劦鍠楅崑瀣煙閹规劦鍤欓柦鍐枑缁绘盯骞嬮悜鍡欏姱闂佸搫鍟悧鍡欑矆閸愵喗鐓忓┑鐐茬仢閳ь剚鐗犲畷鐔煎礃椤旇В鎷绘繛杈剧悼閻℃棃宕靛▎鎴犵＜缂備焦锚閻忓瓨顨ラ悙鏉戠伌鐎规洘鍎奸¨浣圭箾閸忚偐澧紒缁樼☉椤斿繘顢欓懡銈呭毈闂備胶绮敮鎺椻€﹂悜钘夎摕闁挎繂顦粻娑欍亜閺嶃劋浜㈤柡浣告健閺岋絾鎯旈姀銏╂殹濠电姰鍨洪敃銏ゅ灳閿曞倹鍤勬い鏍电稻浜涘┑锛勫亼閸娿倝宕㈡ィ鍐ㄧ婵☆垯璀﹂崵鏇㈡偣閸ャ劎銈存俊鎻掔墛娣囧﹪顢涘☉姘辩厒闂佸摜濮甸悧鏇㈡偩瀹勬嫈鏃堝川椤撶媭妲梻浣稿悑缁佹挳寮插☉妯锋鐟滄柨顫忔繝姘＜婵炲棙甯掗崢锟犳⒑绾懏鐝柟绋垮⒔閸掓帞鎷犲顔兼倯婵犮垼娉涢敃銈夋倵椤撱垺鍋℃繝濠傛噹椤ｅジ鎮介娑樻诞闁诡噯绻濋崹楣冨箛娴ｅ搫鏁搁梻浣稿悑閹倸顭囪閹便劑宕堕妸锝勭盎濡炪倖鍔戦崹娲吹閸ヮ剚鐓涚€光偓鐎ｎ剙鍩岄柧浼欑稻缁绘盯鎳犳０婵嗘闂佸搫鎳樻禍璺侯潖濞差亜宸濆┑鐘叉噹椤ユ繈姊虹涵鍛彧闁挎碍銇勯銏㈢缂佽鲸甯掕灒閻犲洤妯婇埀顒佹崌濮婃椽骞嗚缁犳娊鏌熼搹顐ｅ暗缂侇噮鍙冮弫鎾绘偐閺傘儲瀚奸梻浣告啞缁嬫垿鏁冮妷鈺佺厱闁规壆澧楅悡鐔镐繆閵堝嫮鍔嶇紒鈧€ｎ喗鐓冮柕澶樺灣閻ｇ數鈧娲橀崕濂杆囬鈧弻锛勨偓锝庝邯閸欏嫰鏌″畝鈧崰鏍嵁閹达箑绠涙い鎾跺Т濞懷囨⒑閸濆嫷妲搁柣妤€妫濋幃褔宕卞☉妯煎幋闂佺懓顕崑娑氱不閻樼粯鈷戠紒瀣皡閺€缁樼箾閼碱剙鏋涢柣娑卞枟缁绘繈宕ㄩ婊勬緫婵犵數鍋為崹鍫曗€﹂崶顒€鍌ㄩ梺顒€绉甸埛鎴︽煟閻旂厧浜伴柛銈囧枛閺屾稓鈧綆鍓欐禒杈┾偓瑙勬穿缂嶁偓缂佺姵绋戦埥澶娾枎閹存繂绠洪梻鍌欒兌缁垶寮婚妸鈺佺妞ゆ劧璐熼埀顒€鍊圭缓浠嬪川婵犲嫬骞嶉梻浣呵圭换鎺楀储瑜庨幈銊╁炊椤掍胶鍘撻柣鐔哥懃鐎氼剟鎮橀幘顔界厱闁靛ň鏅濋悾娲煙閻撳海绉烘い銏℃礋閿濈偤顢橀悢绯曟灆闂傚倸鍊搁崐宄懊归崶顒夋晪闁哄稁鍘肩粣妤佷繆閵堝嫯鍏屽ù鐘虫皑缁辨捇宕掑▎鎺戝帯婵犳鍠氶弫濠氬箚閸曨垼鏁嶉柣鎰版涧缁侊箑鈹戦埥鍡楃仧缂侀硸鍠氬Σ鎰版晲閸℃瑧鐦堥梻鍌氱墛缁嬫帗寰勯崟顑句簻闁规崘鍩栧▍鎺楁⒒閸屾艾鈧悂宕愰幖浣哥９闁归棿绀佺壕褰掓煙闂傚顦︾痪鎯х秺閺岀喖姊荤€靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁诡垎鍐ｆ寖闂佺娅曢幑鍥灳閺冨牆绀冩い蹇庣娴滈箖鏌ㄥ┑鍡欏嚬缂併劌銈搁弻鐔兼儌閸濄儳袦闂佸搫鐭夌紞渚€銆佸鈧幃娆撳箹椤撶噥妫ч梻鍌欑窔濞佳兾涘▎鎴炴殰闁圭儤顨愮紞鏍ㄧ節闂堟侗鍎愰柡鍛叀閺屾稑鈽夐崡鐐差潻濡炪們鍎查懝楣冨煘閹寸偛绠犻梺绋匡攻椤ㄥ棝骞堥妸鈺傚€婚柦妯侯槺閿涙盯姊虹紒妯哄濞存粈绮欓崺鈧い鎺嗗亾闁哥喐娼欓悾鐑藉Ω閵夊洤閰ｉ、妤呭焵椤掍胶顩查柛蹇氬亹缁犻箖鏌℃径瀣仴闁诡喗鍨剁换娑㈠礂閼测晜鍣伴梺鍝勮嫰椤戝鐣烽崼鏇炍╃憸蹇涙儊閸儲鈷掗柛灞炬皑婢ф稓绱掔€ｎ偅灏伴柡鍛埣婵偓闁冲灈鏅涙禍楣冩偡濞嗗繐顏紒鈧埀顒勬⒑缂佹澧遍柛妯犲浄缍栭煫鍥ㄦ礈绾惧吋淇婇婵愬殭妞ゅ孩鎹囧娲箚瑜忕粻鎵磼閻樿尙效闁糕斁鍋撳銈嗗坊閸嬫捇鏌ㄩ弴銊ら偗鐎殿喛顕ч埥澶愬閻樻鍟嬮梺璇查叄濞佳囧箺濠婂牊鍋╅柤娴嬫櫇绾捐棄銆掑顒佹悙闁哄鍠栭弻锝夋偄閸欏鐝氶梺缁樹緱閸犳岸鍩€椤掑﹦绉甸柛鐘愁殜瀹曟垿骞囬鐟颁壕闁荤喐婢橀ˉ蹇涙煕閻樻剚娈滈挊鐔兼煥閻斿搫校闁抽攱甯掗湁闁挎繂瀚鐔哥箾閸忕厧娴柡灞剧洴瀵挳鎮欓崗鍝ラ┏闂備椒绱徊鍧楀礂濡警娼栭柧蹇撴贡绾惧吋淇婇婵愬殭闂傚绉撮埞鎴︽倷鐠鸿櫣姣㈤梺鍝ュУ閻楁洟顢氶敐鍡欑瘈婵﹩鍘兼禍婊堟⒑缁嬭法绠洪柛瀣姍瀹曟繈鎮滈懞銉㈡嫼闂佸湱枪鐎涒晝澹曢悽鍛婄厱閻庯綆鍋呯亸鐗堛亜閵婏絽鍔﹂柟顔界懄濞煎繘骞戦幇銊ヤ壕闁秆勵殕閻撴盯鎮橀悙闈涗壕缂佲偓鐎ｎ兘鍋撶憴鍕闁靛牊鎮傞獮鍐閵忋垻鐓撻梺鍓茬厛閸犳寮鍫熲拻闁稿本鐟чˇ锕傛煙绾板崬浜伴柟顖氭湰瀵板嫮浠︾紒銏＄稐闂備礁鎼ú銏ゅ垂瑜版帗鍊块柛顭戝亖娴滄粓鏌熼悜妯虹厐闁告梻鍠撶槐鎺撳緞鐏炵偓姣堥梺鍝勬湰缁嬫捇鍩€椤掑﹦绉靛ù婊勭箞瀹曠敻宕堕浣哥檮闂佺粯鏌ㄩ崥瀣偂閺囩喆浜滈柟閭﹀枛瀛濋梺鍛婃⒐缁捇寮婚敐澶婄閻庡灚鎮堕埀顒佸浮閺屽秷顧侀柛鎾寸懇瀹曘垹饪伴崼婵堬紱闂佺懓澧界划顖炴偂閸愵亝鍠愭繝濠傜墕缁€鍫ユ煟閺冨牜妫戦柡鍡畱闇夐柛蹇撳悑缂嶆垹绱掗悩宕団姇闁靛洤瀚伴獮鎺楀箣椤撶啘銊╂⒑閸涘﹥鈷掗柛蹇斆灋闁告稒鎯岄弫鍐煏韫囧ň鍋撻幇浣剐熼梻鍌欑閹碱偊鎯屾径宀€绀婂ù锝呮憸閺嗭箓鏌熸潏鍓х暠鐎瑰憡绻堥幃妤€鈽夊▎妯煎姺闂佸憡锕㈡禍璺侯潖閾忚鍏滈柛娑卞幘閸旂兘姊洪崨濠冪叆缂佸鎸抽崺銏狀吋婢跺﹦顔掔紓鍌欑劍椤洭宕㈤幘缁樺仭婵犲﹤瀚惌鎺斺偓瑙勬礃缁矂鍩㈡惔銊ョ闁绘鏁搁崢鐘绘⒒娴ｈ棄鍚归柛鐘崇墵瀹曟垶绻濋崒銈囧姺闂佸搫绉查崝搴ｇ不閸撗€鍋撻悷鏉款棌闁哥姵娲滈懞杈╂嫚鐟佷礁缍婇幊鏍煛娴ｅ弶鐎伴梻浣告惈閺堫剟鎯勯姘煎殨闁圭虎鍠栨儫闂侀潧顦崕鍝勵焽缂佹ü绻嗛柣鎰典簻閳ь剚娲滈埀顒佺▓閺呯娀骞嗛崟顖ｆ晬婵ê鍚嬬紞搴♀攽閻愬弶鈻曞ù婊勭矊椤斿繐鈹戦崱蹇旀杸闂佺粯蓱瑜板啴寮冲▎鎾粹拻濠㈣泛锕︽晥濠殿喖锕ュ浠嬪箖濠婂牆鐓涢柛灞剧〒瑜扮喖姊绘担瑙勫仩闁告柨绻愰埢鏂库槈閵忊€充患閻庣懓瀚崳鏉懨洪宥嗘櫆闂佸憡娲﹂崢鍏兼叏閿旀垝绻嗛柣鎰典簻閳ь剚鐗滈弫顔界節閸ャ劉鎸冮悗骞垮劚濡盯銆呴崣澶岀瘈濠电姴鍊绘晶娑㈡煟閹惧鎳囬柡灞剧☉閳规垿宕卞Δ濠佺棯婵＄偑鍊戦崕鑼垝瀹€鍕畳婵犵數濮撮敃銈夋偋韫囨稒鍋╅柣鎴炆戦崣蹇撯攽閻樻彃鏆為柕鍥ㄧ箘閳ь剝顫夊ú蹇涘礉瀹ュ洦宕叉繛鎴烇供濞尖晜銇勯幘璺轰户濞寸姴銈稿娲川婵犲倻鐟查柣銏╁灲缁绘繈鎮伴鍢夌喖鎼圭憴鍕啎闂備焦鎮堕崕顖炲礉瀹ュ鏁婇柡鍥ュ灪閳锋帒霉閿濆嫯顒熼柡鈧导瀛樼厵婵炶尪顔婄花鐣岀磼椤旂⒈鐓兼鐐差儔閹晠宕楅崫銉ф喒闂傚倷鑳堕崢褔锝為弴銏犵９婵°倕鍟ˉ姘舵煢濡警妫︾憸鐗堝笚閺呮煡鏌涢顐簻婵炲牊鍔欏铏圭磼濡櫣鐟愮紓渚囧枟閻熲晛顕ｇ拠娴嬫婵妫欏娲⒑闁偛鑻晶顖滅磼閸屾氨肖闁圭懓瀚粭鐔碱敍濞戞氨鈻夋繝寰锋澘鈧呯不閹烘鏅搁柕澶涘椤╄尙鎲搁悧鍫濈瑲闁绘挻鐟╅弻锝夊箣閻忔椿浜幃姗€鏁愭径瀣幈闂佺粯枪椤曟粓鐓浣瑰弿濠电姴鍟妵婵堚偓瑙勬磸閸斿秶鎹㈠☉銏犵婵犻潧鎳橀崑妤佺節绾版ǚ鍋撻崘宸殺缂備緡鍠楅悷褔骞戦崟顖毼╅柨鏃傜摂閸熷骸鈹戦悩鍨毄濠电偐鍋撳┑鐐板尃閸ヨ埖鏅為梺鍛婁緱閸樼晫鐥娑氱瘈闁汇垽娼цⅷ闂佹悶鍔庨崢褔鍩㈤弬搴撴闁靛繆鏅滈弲鐐烘⒑閹肩偛鍔撮柛鎾寸懇瀹曟帡濡搁埡鍌滃幍闂傚倸鍊搁顓㈠礉瀹ュ鐓冮梺鍨儏閻忔挳鏌＄仦鍓р槈闁宠棄顦～婊堝醇濠靛棭娼梻鍌欑劍濡炲潡宕㈤悡骞稑鈽夊顓ф綗闂佸湱鍎ら〃鍛存倿閸偁浜滈柟鍝勭Х閸忓矂鏌ｉ悢椋庣Ш闁哄苯绉烽¨渚€鏌涢幘璺烘灈妤犵偛鍟€靛ジ骞栭鐔告珦闂備焦鎮堕崹褰捤囬弶璇炬盯宕橀妸褎娈惧┑顔姐仜閸嬫挻銇勯姀鈩冾棃鐎规洟浜堕、姗€鎮╅懠顒€鎼哥紓鍌氬€搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌ｉ幋锝呅撻柛濠傛健閺屻劑寮撮悙娴嬪亾閻㈢ǹ鐒垫い鎺戯功閻ｇ數鈧娲栭悥濂稿春閿熺姴绀冪憸蹇涙偡濠靛鈷掑ù锝堟鐢盯鏌涢弮鈧〃鍫ュ箲閵忕姭鏀介柛顐ゅ枎鎼村﹪姊虹化鏇炲⒉閼垦兠瑰⿰鍕煉闁哄瞼鍠栭獮宥夋惞椤愶絿褰呴梻浣筋嚙缁诲棝寮查悩璇茶摕闁跨喓濮撮獮銏′繆椤栨艾鎮戦柛锝勫嵆濮婃椽宕崟顔碱伃缂備礁顦紞濠囩嵁閹邦厾绡€闁告劑鍔庣粣鐐测攽閳藉棗鐏″ù婊冪埣閺佸啴宕掑☉姘及濠电偞娼欓崥瀣偡瑜旈獮鍐╃附閸涘﹦鍘撻悷婊勭矒瀹曟粌鈽夐姀鐘电枃闂佽鍎抽悘鍫ュ磻閹炬剚娼╂い鎰╁灩缁侇噣姊洪棃娑欘棞闁稿﹤娼″顐﹀磼閻愭潙浠奸悗鍏夊亾闁逞屽墲閵囨劖銈ｉ崘鈹炬嫼闂佸憡绻傜€氼參宕掗妸鈺傜厱闁靛⿵闄勯妵婵嬫煕閳哄倻娲存鐐差儔閺佸倿鎮剧仦钘夌闂傚倷娴囬鏍储閻ｅ本鏆滈柟鐑橆殔缁€鍫熴亜閹捐泛鏋傚ù婊勭矒閻擃偊宕堕妸锕€鏆楁繝鈷€鍕创闁哄矉绻濆畷顏呮媴缁嬫娼绘俊鐐€ら崣鈧繛澶嬫礋楠炴垿宕熼姘辩厬閻庣懓瀚竟鍡樻櫠閺囥垹纭€闂侇剙绉甸悡鏇熴亜閹邦喖孝闁告梹绮撻弻锝夊箻瀹曞洨顔掗梺缁樻惄閸嬪﹤鐣烽崼鏇炍╅柨鏃傝檸濡差剟姊洪崫銉ユ灁閺嬵亝銇勯鍕殻濠碘剝鍎肩粻娑樷槈濞嗘ǚ鍋撳鍛斀闁绘﹩鍠栭悘顏堟煥閺囨ê鐏查柛銊╃畺瀵噣宕煎┑鍫О婵＄偑鍊栭弻銊ノｉ崼锝庢▌闂佸搫鏈惄顖炲春閸曨垰绀冩い蹇撴噹濞呮垶淇婇悙顏勨偓銈夊磹閵堝绀冮柨婵嗘閺夊憡淇婇悙顏勨偓鏍涙笟鈧敐鐐参旈埀顒勩偑娴兼潙閱囨繝闈涚墱濡茬兘姊绘担渚敯闁规椿浜炵划濠氬箣閻樺吀绗夐梺鍝勭▉閻忔盯寮崼鐔告珳闂佸憡渚楅崢钘夆枔椤撶喓绠剧痪鎯ь儏娴滅懓鈹戦悙鈺佷壕闂備礁鎼張顒勬儎椤栨凹鍤曢柡澶嬪殾閻旂厧浼犻柛鏇犳暩閳ь剚鎸冲缁樻媴閸涘﹥鍎撻梺绋匡功閺佽鐣烽幋锕€绠婚柡鍌樺劜閺傗偓闂備焦瀵х粙鎴犫偓姘煎墯缁傚秵绺介崨濠勫幈婵犵數濮撮崯鐗堟櫠椤忓懌浜滄い鎰╁灮缁犺尙绱掔紒妯肩畵妞ゎ偅绻堥、鏍煘閸喖濮﹂梺璇″枟椤ㄥ﹪寮幇顓熷劅闁斥晛鍟版禍顏嗙磽閸屾艾鈧鎷嬮弻銉ョ柧婵犻潧鐗婇～鏇㈡煙閹呮憼濠殿垱娼欒灃闁绘﹩鍠栨俊鐣岀磼椤旇偐肖缂侇喖顑呴濂稿川椤忓嫮澧梻浣稿閸嬪棝宕伴幘璇插偍闁瑰墽绮埛鎴︽煕濞戞﹫鍔熺紒鐘虫崌閹顫濋悡搴♀拫閻庢鍠楁繛濠傤嚕閹绢喖顫呴柨娑樺楠炲牓姊绘担鍛婅础闁稿簺鍊濆畷娲醇濠靛啯鐏侀梺琛″亾闁诡厼鈥�


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
    Unit* unit = builder->getUnit();
    Function* func = new Function(unit, nullptr);
    BasicBlock* entry = func->getEntry();


    Operand* addr;
    SymbolEntry* addr_se;
    addr_se = new IdentifierSymbolEntry(*dynamic_cast<IdentifierSymbolEntry*>(id->getSymPtr()));
    addr_se->setType(new PointerType(id->getSymPtr()->getType()));
    addr = new Operand(addr_se);
    dynamic_cast<IdentifierSymbolEntry*>(id->getSymPtr())->setAddr(addr);
    Instruction* g;
    //initVal->genCode();
    g = new GlobalInstruction(new Operand(id->getSymPtr()), new Operand(initVal->getSymPtr()), id->getSymPtr(), entry);
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
        Unit* unit = builder->getUnit();
        Function* func = new Function(unit, nullptr);
        BasicBlock* entry = func->getEntry();

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
        g = new GlobalInstruction(new Operand(id->getSymPtr()), initVal->getOperand(), se, entry);
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


    dst->get_se()->set_use_r0_r3(0);
    dst->get_se()->set_return();

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
            /*Function* func = builder->getInsertBB()->getParent();
            BasicBlock* entry = func->getEntry();*/
            Instruction* alloca;
            Operand* addr;
            SymbolEntry* addr_se;
            Type* type;
            type = new PointerType(para_expr->getSymPtr()->getType());
            addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
            if (ParaNodeStack[ParaNodeStack.size() - 1].size() < 4)
                addr_se->set_use_r0_r3(ParaNodeStack[ParaNodeStack.size() - 1].size());
            else
                addr_se->set_use_r0_r3(-1);
            addr = new Operand(addr_se);
            alloca = new AllocaInstruction(addr, para_expr->getSymPtr());                   // allocate space for local id in function stack.

            if (ParaNodeStack[ParaNodeStack.size() - 1].size() < 4)
            {
                //说明现在参数还不够4个。
                //这里要做点特别的。要做的不是在占上开辟空间，而是把这个变量和rx寄存器关联起来。
                ((AllocaInstruction*)alloca)->set_funct(ParaNodeStack[ParaNodeStack.size() - 1].size());
            }
            else
            {
                ((AllocaInstruction*)alloca)->set_funct(-1);
            }
            //最后把这个玩意push进去
            temp_data_bag temp_bg;



            dynamic_cast<IdentifierSymbolEntry*>(para_expr->getSymPtr())->setAddr(addr);
            SymbolEntry* temp_src_addr = new TemporarySymbolEntry(para_expr->getSymPtr()->getType(), SymbolTable::getLabel());
            Operand* temp_src = new Operand(temp_src_addr);


            temp_bg.addr = addr;
            temp_bg.insn = alloca;
            temp_bg.temp_src = temp_src;

            ParaNodeStack[ParaNodeStack.size() - 1].push_back(temp_bg);


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
        /*Operand* src2 = expr->getOperand();
        Operand* temp = src2;
        int opcode = UnaryInstruction::NOT;
        new UnaryInstruction(opcode, dst, temp, builder->getInsertBB());*/


        // if(闂傚倷鐒︾€笛囧礃婵犳艾绠柨鐕傛嫹2)
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



//闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆撴倻濡桨鐢绘繝鐢靛Т閿曘倝宕板顑╃喖鍩€椤掑嫭鍊垫繛鍫濈仢閺嬬喖鏌熼鐓庘偓鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸閻忕偟纭堕崑鎾崇暦閸ャ劍鐣烽梻渚€鈧偛鑻晶瀵糕偓瑙勬磻閸楀啿顕ｆ禒瀣垫晣闁绘劗鏁搁弳顐︽煟閻斿摜鐭嬮柡灞诲姂閹箖鏌ㄧ€ｎ剟妾梺鍛婄☉閿曘倖绂嶅⿰鍫熲拺闁告稑锕︾粻鎾绘倵濮樼厧澧柍缁樻崌婵＄兘鍩￠崒妤佸闂備胶枪閺堫剟鎮疯钘濋柨鏇炲€归悡娆撴偣閸ュ洤鎳愰惁鍫ユ倵鐟欏嫭绀€婵犫偓闁秴绠熼柟缁㈠枛缁€瀣亜閹哄棗浜鹃悗娈垮枛濞尖€愁潖濞差亜鎹舵い鎾墲椤庢姊洪崫銉バｉ柣妤冨█楠炲啴鍨鹃幇浣瑰缓闂侀€炲苯澧寸€殿喖顭烽幃銏ゅ川婵犲嫮肖濠德板€х徊浠嬪疮椤栫儐鏁佺€广儱顦伴埛鎴犵磼鐎ｎ偒鍎ラ柛搴㈠姍閺屾盯寮埀顒勬偡閳哄懐宓侀柛鎰╁壆閺冨牆宸濇い鎾跺枎缁佽埖淇婇悙顏勨偓鏍箰妤ｅ啫纾规い鎰剁畱椤懘骞掗搹顐ゎ洸婵犲﹤鐗婇悡蹇擃熆閼哥數鈽夋い鈺婂墰缁辨帡鎮╅幇浣哄姺缂備浇椴哥敮妤€顭囪箛娑樜╅柨鏇楀亾闂傚绉瑰娲偡閺夋寧些闂佺娅曢敋闁伙絿鍏橀、鏇㈡晝閳ь剛绮堢€ｎ剛妫柣妤€鐓鍛洸闁告挆鈧崑鎾诲礂婢跺﹣澹曢梻浣告啞濞诧箓宕滃☉銏犲偍闂侇剙绉甸埛鎴︽⒒閸喕鍎愮憸鐗堝笒绾惧潡鏌熼幆鐗堫棄闁哄嫨鍎甸弻銊モ攽閸℃顦遍梺绋款儐閹告悂鍩㈤幘璇插瀭妞ゆ梻鏅禍顏呬繆閻愵亜鈧倝宕㈡ィ鍐ㄧ婵☆垯璀﹂崵鏇熴亜閹板墎鐣辩紒鐘崇⊕閵囧嫰骞樼捄杞版埛缂備焦鍔楅崑鐐垫崲濠靛鍋ㄩ梻鍫熷垁閵忋倖鍊垫慨妯哄船閸樺鈧娲忛崕闈涚暦閵娧€鍋撳☉娅辨岸骞忕紒妯肩閺夊牆澧介幃濂告煙閾忣偅宕屾い銏¤壘楗即宕ㄩ婊愮床闂佽崵濮村ú锕併亹閸愵喖鐒垫い鎺嗗亾婵炵》绻濋幃浼搭敊閽樺绐炴繝鐢靛仧閸嬫捇锝炲澶嬧拺闂傚牊绋撶粻鍐测攽椤旀儳鍘存い銏＄懇楠炴帡寮崒婊愮闯闂備胶枪閺堫剟鎮疯瀹曟繂顓奸崱鎰盎濡炪倖鎸鹃崑鐐哄窗濡皷鍋撶憴鍕８闁搞劋绮欓悰顕€寮介妸锕€顎撻梺绋跨箰椤︽壆鈧艾銈稿缁樻媴閸涘﹤鏆堢紓浣割儐閸ㄥ潡寮崘顔芥櫜闁搞儺鐓堥崑銊╂⒒娓氬洤澧紒澶屾暬閹繝寮撮姀锛勫幗濠碘槅鍨甸褎鏅堕娑氱闁割偆鍠撻惌宀勬煃瑜滈崜婵嬶綖婢跺⊕鍝勵潨閳ь剙鐣峰┑鍡忔瀻闁规崘娉涚粊锕傛⒑閹肩偛鍔撮柛鎾村哺瀹曟垹鈧綆鍠楅悡鏇熴亜閹邦喖孝闁诲繆鏅滈〃銉╂倷閺夋垵顫掗梺鍝勭焿缁查箖骞嗛弮鍫濈伋闁告劘灏欢鐔兼⒒娴ｅ懙褰掝敄閸℃稑绠查柛銉墮缁犳牗淇婇妶鍛櫤闁哄懏鐓￠獮鏍垝閸忓浜鹃柟棰佺劍妤犲嫰姊婚崒娆愵樂缂侀硸鍠氱划濠氬箣閿旇棄浜遍梺瑙勫礃椤曆呯不閺嶃劋绻嗛柕鍫濇噹閺嗙偤鏌涢悩鍙夘棦闁哄本鐩鎾Ω閵夈儺娼炬俊鐐€х拹鐔煎礉閹达箑钃熼柨婵嗩槸椤懘鏌嶆潪鎷屽厡濞寸媭鍙冮弻锝夊閳轰胶浠梺鍝ュУ閻楃娀鐛崘顭戞建闁逞屽墴閻涱噣骞掗幊铏⒐閹峰懘宕崟顐ゎ啈闂傚倸鍊烽懗鍓佸垝椤栫偛绀夐柡鍥ュ焺閺佸銇勯幘鍗炵仼闁绘挻绻堥弻鐔煎礈瑜忕敮娑㈡煟閹惧啿鏆熼柟鑼归オ浼村醇濠靛牜妲舵繝鐢靛仜濡瑩骞愰幖浣哥＜闁靛ň鏅滈悡娑氣偓骞垮劚閸燁偅淇婇崸妤佺厽婵犻潧妫涢崺锝夋煛瀹€瀣瘈鐎规洖鐖兼俊鐑藉Ψ瑜岄惀顏堟⒒娴ｇ懓鈻曢柡鈧柆宥呭瀭闁秆勵殔閽冪喐绻涢幋鐐冩岸寮ㄦ禒瀣厱闁斥晛鍠氬▓銏犆圭亸鏍т壕闂傚倸鍊风粈渚€鎮块崶顒夋晪鐟滄棁妫熼梺姹囧灮鏋柦鍐枑缁绘盯骞嬮悙鑼姲闂佺ǹ顑嗛幑鍥极瀹ュ纭€闁绘劕寮跺▓銊︾節閵忥綆娼愭繛鑼枎椤繑绻濆顒傦紲濠电偛妫欑敮鎺楀储閿熺姵鐓涢悗锝呭悁缁ㄥジ鏌曢崶褍顏鐐差儏閳规垿宕堕懜纰夌礂濠德板€楁慨鐑藉磻閻愬搫绀夋繛鍡樻尵瀹撲線鏌″鍐ㄥ闁荤喓鎳撻埞鎴︽偐閹绘帩浼€闂佸磭顑曢崐鏍崲濠靛鍋ㄩ梻鍫熺▓閺嬪懎鈹戦悙鏉垮皟闁搞儴鍩栭弲鐐烘⒑閸︻厼顣兼繝銏★耿瀵憡绗熼埀顒勫蓟濞戙垹鐒洪柛鎰典簴婵洭姊虹紒妯诲鞍婵炶尙鍠栭獮鍐潨閳ь剟骞冨▎鎾崇闁瑰搫妫欑€垫牠姊绘担鑺ャ€冮柣鎺炵畵閹囨偐鐠囪尪鎽曢梺鍝勬川閸犳挾绮绘ィ鍐╃厽闁逛即娼ф晶顖炴煟濠靛洦绀嬫慨濠冩そ楠炲酣鎳為妷锔芥闂備焦鎮堕崝灞筋焽閳ユ剚鍤曢悹鍥ㄧゴ濡插牊淇婇鐐存暠闁哄倵鍋撻梻鍌欒兌绾爼宕滃┑瀣ㄢ偓鍐疀閺傛鍤ら梺绋挎湰閸戠懓銆掓繝姘厪闁割偅绻堥妤€霉濠婂啫鈷旂紒杈ㄥ浮閹晠鎼归銏紦闁诲孩顔栭崰娑㈩敋瑜旈崺銉﹀緞閹邦剦娼婇梺缁橆焽閺佺ǹ顭囨径鎰拻濞撴埃鍋撻柍褜鍓涢崑娑㈡嚐椤栨稒娅犻悗鐢电《閸嬫挾鎲撮崟顒傤槬缂傚倸绉撮敃銈夋偩妞嬪海鐭欓悹鍥╁亾濡啫鐣峰鈧俊姝岊槹闁稿锕濠氬磼濞嗘埈妲梺纭咁嚋缁绘繂鐣峰┑瀣櫇闁稿本姘ㄩ悰銉╂⒑閸濆嫮鈻夐柛娆忕箳缁辩偤寮介鐔哄幈濠电偞鍨堕…鍥箺閻樼粯鐓熼柟鎯х－缁犲鏌＄仦鍓ф创妞ゃ垺娲熼弫鎰板炊閳哄啯姣囬梻鍌欑窔濞艰崵寰婇挊澶涜€块弶鍫氭櫆椤洟鏌熼悜姗嗘當缂佺姵绋掗妵鍕箻鐠虹洅娑樷攽椤斿吋宸濈紒杈ㄦ尰閹峰懘鎼归悷鎵偧缂傚倷娴囬褔鎮ч幘鑽ゅ祦闁告劏鏅濋々鐑芥倵閿濆骸浜芥俊顐㈠暙閳规垿鎮欏▓鎸庮棑缂傚倸鐗呴崡鍐茬暦鐟欏嫮顩烽悗锝庝簽閻ｅ搫鈹戞幊閸婃劙宕戦幘娣簻妞ゆ劧绲跨粻鐐存叏婵犲懎鍚归柍褜鍓熷褔骞夐敓鐘茬柧闁稿繗鍋愮弧鈧紒鍓у鑿ら柛瀣崌瀹曟﹢鎳犻鍕礈闂傚倷绀侀幉鈥愁潖瑜版帗鍋￠柕澶嗘櫓閺佸鏌ㄥ┑鍡╂Ц缂佺姷绮妵鍕籍閸屾瀚涘┑鐐靛帶閿曨亜顫忓ú顏勭闁兼亽鍎插▓浼存⒑缁嬫鍎愰柟鐟版喘瀵鎮㈤崜鍙壭ч柟鑹版彧缁茬晫绮敍鍕＝濞达綀娅ｇ敮娑氱磼鐎ｎ偄绗掓い顓炴穿缁犳稑鈽夊Ο纰辨Ф闁荤喐绮岀粔鐟扮暦閸濆嫮鏆嗛柛鏇ㄥ厴閹疯櫣绱掔紒銏犲箹闁瑰啿姘﹂。璺ㄧ磽閸屾瑧璐伴柛鐘崇墪闇夐柣鎴ｆ缁€鍡涙煙閻戞﹩娈旂紒顐㈢Ч閺屾盯顢曢妶鍛€婚梺閫炲苯澧扮紒顕呭灡缁岃鲸绻濋崶鑸垫櫖闂佺粯鍔曢悺銊╂偟瀹曞洨纾藉ù锝夋涧婵″吋銇勯鐐叉Щ妞ゎ偄绻愮叅妞ゅ繐鎷嬪Λ鍐ㄢ攽閻愭潙鐏卞瀛樻倐椤㈡捇宕卞☉娆屾嫽闂佺ǹ鏈懝楣冾敂閳哄懏鍊垫慨妯煎帶婢ф挳鏌熼钘夊姢闁伙綇绻濋獮宥夘敊閼恒儳鏆﹂梻鍌欑窔閳ь剛鍋涢懟顖涙櫠閹绢喗鐓曢柍瑙勫劤娴滅偓淇婇悙顏勨偓鏍暜閹烘绐楁慨姗嗗墻閻掍粙鏌熼柇锕€骞樼紒鐘荤畺閺屾稑鈻庤箛锝嗩€嗛梺鍏兼緲濞硷繝寮婚弴銏犲耿婵°倐鍋撻柡鍡悼閳ь剝顫夊ú蹇涘礉瀹ュ拋鐒介煫鍥ㄧ☉缁€鍫㈡喐韫囨稑鐓熼柕鍫濇缁♀偓闂佹眹鍨藉褍鐡梻浣瑰濞插繘宕愬┑瀣畺鐟滄柨鐣烽崡鐐╂婵炲棗绻嗛崑鎾诲锤濡や胶鍘卞銈庡幗閸ㄥ灚绂嶉弽顬＄懓饪伴崟顓犵厜闂佸搫鐬奸崰鏍х暦椤愶箑绀嬫い鎺戭槹椤ワ絽鈹戦悙鑼憼缂侇喖绉瑰畷鏇㈠箮鐟欙絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯鍩勯崘鍓у姺闂佸疇顫夌粙鎾舵閹捐纾兼慨妯荤樂閿涘嫮纾奸柤鎼佹涧閸濆搫鈹戦垾宕囨憼缂佹鍠栭崺鈧い鎺嗗亾妞ゆ洩缍侀獮姗€顢欑喊杈ㄧ秱闂備焦鏋奸弲娑㈠疮椤栫偞鍋熼柡宥庡幗閳锋帒銆掑锝呬壕闂侀€炲苯澧伴柛瀣洴閹崇喖顢涘☉娆愮彿闂佽顔栭崰姘卞閸忕浜滈柡鍌氱仢閳锋棃鎮介娑氥€掔紒杈ㄥ笧缁辨帒螣閼测晝鏆ゆ俊鐐€ら崑鍛崲閸儯鈧礁顫滈埀顒勫箖濞嗘挸绾ч柛顭戝櫘閸ゆ瑩姊婚崒姘偓鎼佸磹閻戣姤鍊块柨鏇炲€哥粻鏍煕椤愶絾绀€缁剧偓瀵х换婵囩節閸屾粌顣虹紓浣插亾闁告劏鏂傛禍婊堢叓閸ャ劍灏靛褋鍨婚埀顒冾潐濞叉鍒掑畝鍕厴闁硅揪闄勯弲顒勬煕閺囥劌浜楅柡澶樺弮濮婅櫣绮欓崠鈩冩暰闂佹悶鍔岄悘婵嬫偩閻戣棄鍗抽柕蹇娾偓铏吇婵＄偑鍊栫敮濠勭矆娴ｈ鍎熷┑鐘叉处閳锋垿鏌涢幘鏉戠祷濞存粍绻勭槐鎺旀嫚閼碱儷銏°亜椤撶偞鍠樼€殿喕绮欐俊姝岊槾妞ゆ梹娲熼幃妤呮偡閺夋妫岄梺鍝ュУ濞叉粓鎳為柆宥呯缂佹妗ㄧ花濠氭⒑闂堟侗妲堕柛搴ゆ珪閺呰泛鈽夊杈╋紲缂傚倷鐒﹂…鍥虹€涙﹩娈介柣鎰▕閸庢棃鏌熼鐣屾噮闁圭懓瀚粭鐔碱敍濞戣鲸锛堝┑鐘殿暜缁辨洟宕戦幋锕€纾圭憸蹇擃嚗婵犲啰顩烽悗锝庝簽椤旀劕顪冮妶鍡楀Ё缂佺姵鍨块幃鈥愁潨閳ь剟寮婚悢鍛婄秶闁告挆鍛闂備胶绮幖顐ゆ崲濠靛钃熸繛鎴欏灩閻掓椽鏌涢幇鍓佺窗婵炲矈浜炵槐鎾存媴闂堟稑顬嬮梺鍛婎焼閸ワ絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯骞囬埡浣割瀳濡炪値鍓欓悧鎾愁潖濞差亜绠伴幖娣灩椤︹晜绻涚€涙鐭ゅù婊勭箖缁岃鲸绻濋崟銊ヤ壕婵炴垶鐟悞浠嬫煟閻旈绉洪柡灞界Х椤т線鏌涢幘瀵告噰妞ゃ垺宀搁弫鎰緞濡粯娅旈梻渚€鈧偛鑻晶顕€鏌ｉ敐鍥у幋闁诡喒鍓濋幆鏂课熺紒妯绘緫闂備浇顕ч崙鐣岀礊閸℃顩叉繝闈涱儏濮瑰弶銇勮箛鎾跺闁抽攱甯￠弻娑氫沪閸撗勫櫘濡炪倧璁ｇ粻鎾诲蓟閻斿搫鏋堥柛妤冨仒缁ㄥ鏌ｉ幘鍗炩偓婵嬪蓟閺囩喓绠鹃柣鎰靛墯閻濇棃姊哄Ч鍥р偓銈夊闯閿濆钃熸繛鎴炵矤濡茬厧顪冮妶鍐ㄥ闁硅櫕鍔楅崚鎺撶節濮樺吋鏅┑鐐村灦濮樸劑顢欓弴銏″€甸柣鐔告緲椤ュ繘鏌涢悩铏闁奸缚椴哥缓浠嬪川婵犲嫬骞楅梻浣筋潐瀹曟ê鈻斿☉銏犲嚑婵炴垯鍨洪悡娑氣偓鍏夊亾閻庯綆鍓欓崺宀勬煣娴兼瑧绉柡灞剧☉閳规垿宕卞Δ濠佹偅缂傚倷鐒﹂〃蹇涘矗閸愵煈娼栨繛宸簼閸嬪倿骞栫划瑙勵潑婵☆偄鏈换娑氣偓娑欘焽閻倕霉濠婂簼绨绘い鏇悼閹风姴霉鐎ｎ偒娼旈梻渚€娼х换鎺撴叏閻㈡潌澶娾攽鐎ｎ偆鍘介梺缁樏崢鏍嚋椤忓牊鐓曢柡鍌氭健閸欏嫮鈧娲樼换鍫ャ€佸☉銏″€烽柡澶嬪灍閸嬫捇宕归銈囶啎闂佸壊鍋呯换鍕閵忋倖鐓涢悗锝庡墮閺嬫盯鏌″畝鈧崰鏍€佸▎鎾村亗閹煎瓨锚娴滈箖鏌涜椤ㄥ棝宕戝Ο姹囦簻闁哄啫鐗婇弳婊堟煕鐎ｎ偅宕岄柡浣瑰姈閹棃鍩勯崘顏冩喚闂傚倷绀侀幖顐﹀箠韫囨洖鍨濋柟鎹愵嚙閽冪喖鏌ㄥ┑鍡╂Ч闁哄懏鐓￠弻娑樷槈濞嗘劗鍑″銈呮禋閸樿壈鐏冮梺缁橈耿濞佳勭閿曞倹鐓曢柡鍐ｅ亾闁荤喆鍎甸敐鐐剁疀濞戞瑦鍎柣鐔哥懃鐎氼剟鎮垫导瀛樷拺闁革富鍘剧敮娑㈡偨椤栨稑绗掔悮娆撴煙闁箑鏋ょ痪鍙ョ矙閺屾稓浠﹂悙顒傛閻庢稒绻傞埞鎴﹀煡閸℃ぞ绨奸梺鑽ゅ暀閸涱厼鐏婇柣搴秵娴滃爼宕ョ€ｎ亶鐔嗛悹铏瑰皑闊剟鏌涢悙鑼煟婵﹥妞藉畷顐﹀礋椤掆偓缁愭盯姊洪崫銉バ㈤悗娑掓櫇缁顓奸崨鍌涙瀹曘劑顢欑憴鍕伖缂傚倸鍊风粈渚€顢栭崼婵冩灃闁哄洨濮锋稉宥夋煙閹澘袚闁绘挸鍟伴埀顒傛嚀鐎氼厽绔熼崱妯绘珷闁哄洢鍨洪悡鍐磽娴ｈ偂鎴犱焊閹殿喚纾奸柛灞剧☉缁椦囨煃瑜滈崜銊х礊閸℃稑纾婚柛鈩冪☉閸屻劌霉閻樺樊鍎愰柍閿嬪灩閹叉悂鎮ч崼婵堢懆婵炲瓨绮堥崡鎶藉蓟濞戙垹惟闁靛牆鎳庣粊顕€姊虹拠鈥崇仩闁哥喐鎸抽獮鏍亹閹烘垶宓嶅銈嗘尵婵妲愰弻銉︹拻濞达綀娅ｇ敮娑㈡煥濮橆厺绻嗘い鏍ㄧ啲闊剟鏌熼鐟板⒉缂佽桨绮欏畷銊︾節閸曨偄绠伴梺璇查閸樻粓宕戦幘缁樺€甸柨婵嗛婢т即鏌ㄥ☉姘瀾缂佺粯绋撻埀顒傛暩鏋い搴㈡尭閳规垿鍨鹃搹顐㈡灎濡ょ姷鍋涚换姗€骞冮埡鍐╁珰闁肩⒈鍓﹂崬浠嬫⒒娴ｅ憡璐￠柛搴涘€楅幑銏ゅ礋椤栨氨鍔﹀銈嗗笒閸婃悂宕㈢€涙ɑ鍙忓┑鐘插鐢稓绱掑Δ鍐ㄦ灈闁糕斁鍋撳銈嗗笒鐎氼剟鎮為崹顐犱簻闁圭儤鍨甸埀顒€鎲＄粋鎺戔堪閸喓鍘惧┑鐐跺蔼椤曆囨倶閿熺姵鐓涢柛娑卞幘閸╋絿鈧娲樼划蹇浰囩€靛摜妫柛鎾楀嫭鐝梺閫炲苯澧紒鐘茬Ч瀹曟洘娼忛埞鎯т壕婵鍘у▍宥夋寠濠靛鐓ラ柡鍌涱儥濞肩喎霉濠婂嫮鐭嬮柕鍥у楠炴鎹勬潪鐗堝媰缂傚倷绀侀張顒傗偓绗涘洤桅闁告洦鍨遍弲婵嬫煃瑜滈崜娑欑珶閺囥埄鏁囬柣妯诲墯濞肩喖姊洪崷顓炲妺妞ゃ劌鎳愰悮鎯ь吋婢跺鍘遍柣蹇曞仜婢т粙銆傞弻銉︾厽閹兼番鍊ら崵娆愩亜椤忓嫬鏆ｅ┑陇鍩栭幆鏃堝灳閼碱剙鍤紓鍌氬€风欢锟犲窗濡ゅ懎绠查柛銉戝懏娈惧┑鐐叉▕娴滄粓鎮為崹顐犱簻闁瑰搫绉瑰宄懊瑰⿰鍕煉闁哄瞼鍠栧畷顐﹀礋椤掑顥ｅ┑鐐茬摠缁秹宕曢幎瑙ｂ偓鏃堝礃椤斿槈褔骞栫划鍏夊亾閼碱剛娉跨紓鍌氬€烽懗鍓佸垝椤栫偞鏅濋柕蹇嬪€楀畵渚€鏌″搴″幍濞存粌缍婇弻鐔兼倻濡櫣浠搁悗瑙勬尭缁夋挳鈥旈崘顔嘉ч柛鈩兠弳妤佺節濞堝灝鏋ら柛蹇斆悾鐑藉閵堝孩鏅梺缁樺姉鐞涖儵骞忕紒妯肩閺夊牆澧介崚浼存煙鐠囇呯瘈鐎规洦鍨堕幃娆撴倻濡厧骞楅梻浣烘嚀閻忔繈宕銏╁殨妞ゆ柨鐨烽弨浠嬫煃閵夈儳锛嶉柛鈺嬬悼閳ь剝顫夊ú蹇涘礉閹达负鈧礁鈻庨幘鏉戜簵闁硅偐琛ュΣ鍛涢幇顑芥斀闁绘ê鐏氶弳鈺佲攽椤旀儳鍘寸€殿噮鍋婂畷銊︾節閸愩劌浼庨梻浣虹帛閸ㄧ敻锝為弴銏犵劦妞ゆ巻鍋撻柛濠傛健楠炴劖绻濋崘顏嗗骄闂佸啿鎼鍥╃矓椤旈敮鍋撶憴鍕８闁告梹鍨甸锝夊醇閺囩偟顓洪梺缁樼懃閹虫劙鐛姀銈嗏拻闁稿本鐟чˇ锕傛煙濞村鍋撶搾浣规そ閺佸啴宕掑鎲嬬幢闂備礁婀遍崕銈夈€冮崱娑樼９闁割煈鍟旇ぐ鎺撳亹鐎瑰壊鍠栭崜閬嶆⒑缂佹ɑ灏柛搴ｆ暬瀵鏁愭径濠冾棟闂佸湱枪鐎涒晠宕曢幘缁樼厱闁绘柨鎲￠崐鎰版煙椤旂瓔娈橀柟鍙夋尦瀹曠喖顢楅埀顒勬儗椤斿墽纾藉ù锝嗗絻娴滅偓绻涢幘鏉戠劰闁稿鎹囬弻宥夊Ψ椤栨粎鏆ら悗瑙勬礈鏋摶鏍归敐鍛暈闁汇倕鍊垮濠氬磼濮橆兘鍋撻幖浣瑰亱濠电姴瀚惌娆撴煙鏉堟崘顓虹紓宥嗙墱閳ь剙鍘滈崑鎾绘煕閺囥劌澧伴柛姗€娼ч—鍐Χ閸℃瑥顫х紓渚囧枛閻倸顕ｇ粙搴撴婵炲棙鍔曞鎸庣節閻㈤潧孝闁哥噥鍨舵俊闈涒攽閸艾浜鹃悷娆忓缁€鍐煥閺囨ê鐏╅柣锝囧厴閺佹劙宕卞Δ鍐嵁闂佽鍑界紞鍡樼濠婂嫮鐝剁€广儱顦伴埛鎺楁煕鐏炲墽鎳嗛柛蹇撶焸瀵悂顢旈崼鐔哄弳闂佸搫娲﹂敋闁逞屽墯缁诲牓宕规ィ鍐╂櫇闁逞屽墴閹箖鏁撻悩鑼吋濡炪倖妫佹慨銈夘敊閺囥垺鈷掑ù锝囨嚀椤曟粎绱掔拠鎻掆偓鑳濡炪倖鐗滈崑鐔风暤娓氣偓閺屾洝绠涢妷褏锛熺紓浣哄█缁犳牠寮婚悢琛″亾閻㈢櫥瑙勭瑜旈弻鐔煎箵閹烘挸浠撮梺鍝勭灱閸犳牠骞冮崸妤婃晬婵炴垵褰夐崫妤冪磽閸屾瑦绁板瀛樻倐楠炴垿宕惰閺嗭箓鏌熼悜妯虹亶闁哄閰ｉ弻鐔衡偓娑欘焽缁犳挻銇勯鈧ˇ闈涱潖濞差亝瀵犲璺猴攻濞堢粯绻涚€涙鐭婇柣鏍帶椤曪絾绻濆顓熸珳婵犮垼娉涢敃锕傤敊閹烘鈷戦柛娑樷看濞堟﹢鏌涚€ｎ偆鈯曢柟渚垮姂閺佸啴宕掑☉鎺撳闂備礁鎲＄换鍌溾偓姘煎櫍閹﹢鏌嗗鍡欏幈闂佺粯枪濞呮洖鐣风仦缁㈡闁绘劘灏欑粻濠氭煙椤旇娅嗙紒妤冨枛椤㈡稑霉妫版繂澧扮紒杈ㄦ崌瀹曟帒鈻庨幒婵嗘暭婵犵绱曠€氬繘宕惰閿涙盯姊洪悷鏉库挃缂侇噮鍨堕崺娑㈠箳閹炽劌缍婇弫鎰板炊閳哄倹鍟掗梻浣规偠閸婃洟宕幘顔艰摕婵炴垯鍨洪崑鎰版煙妫颁胶鍔嶉柣銈勭窔濮婄儤娼幍顕呮М闂佸摜鍠庨悺銊╁箞閵娾晛鐒垫い鎺戝閻撳繘鏌涢锝囩畵闁哄棗锕弻娑氣偓锝庡亝鐏忎即鏌熷畡鐗堝殗鐎规洘绮撳畷锝嗗緞婵犲倸甯梻鍌欐祰瀹曞灚绻涢埀顒傜磽閸屾稖澹橀柍璇茬Ч閺佹劖寰勬繝鍕垫Х闂備焦瀵уú宥夊磻閹炬番浜滄い蹇撳閺嗭絽鈹戦垾宕囧煟鐎规洖宕灃闁告劦浜濋崳顖炴⒒娴ｇ瓔鍤欓悗娑掓櫊瀹曟瑨銇愰幒鎴狀槶闂佺粯姊婚崢褏绮婚鐐寸厵閺夊牓绠栧顕€鏌涚€ｎ亜顏柡灞剧缁犳稑顫濋鎸庣潖闂備礁鎲＄湁缂侇喗鐟ラ～蹇撁洪鍕炊闂佸憡娲﹂崑鈧柛瀣崌楠炴牗鎷呴悷鎵冲亾閼哥數绡€闂傚牊绋撻幊浣割熆鐠虹儤鎹ｉ柣銈傚亾闂備礁鎼崐鍛婃櫠閻ｅ瞼涓嶆慨妯垮煐閳锋垿鏌涢敂璇插箻閻㈩垱鐩幃浠嬵敍濠婂啩鎴风紓渚囧枟濡啴骞冨⿰鍐炬建闁糕剝锕╅崯宥夋⒒娴ｈ櫣甯涢柛鏃€娲熼、姘额敇閻斿憡鐝烽梻渚囧墮缁夌敻鎮￠弴銏＄厵闁绘垶蓱鐏忣厽绻涢幖顓炴灓闁逞屽墲椤煤濮椻偓閵嗗啴宕卞Δ濠傛闂佸憡娲﹂崜娑氬姬閳ь剚绻涙潏鍓у埌濠㈢懓锕よ灋婵犲﹤鐗婇埛鎺楁煕鐏炲墽鎳呮い锔肩畵閺岀喓鎷犺缁♀偓閻庤娲樼换鍌濈亽闂傚倸鍊搁顓㈠礈閵娿儮鏀介柣鎰级椤ョ偤鏌熺亸鏍ㄦ珚鐎殿噮鍋婂畷鍫曞Ω瑜忛鏇㈡倵閻熸澘顥忛柛鐘虫礈閼鸿鲸绺介崨濠勫帗闂備礁鐏濋鍛归鈧弻锛勪沪閸撗佲偓鎺懨归悪鍛暤鐎规洘绮忛ˇ鎶芥煕閿濆骸骞樼紒杈ㄦ尰閹峰懘鎮块姘腐濠电姭鎷冮崟鍨暦缂備礁鐭佹ご鍝ユ崲濠靛纾奸柕鍫濇閻︽粓姊绘担铏瑰笡闁圭ǹ鎲￠〃銉╁箹娓氬﹦绋忛梺鎼炲労閸撴岸鍩涢幋锔藉仯闁搞儻绲洪崑鎾绘惞椤愩倓澹曢梻鍌欒兌鏋い鎴濆暣瀹曟劕鈹戦崱鈺佹濡炪倖鍔х粻鎴犵矆閸愨斂浜滈柡鍐ㄦ搐娴滃綊鏌ㄥ☉娆戠煀闁宠鍨块、娆撳棘閵堝牃鍋撶捄銊ф／妞ゆ挾鍠愰崐鎰偓瑙勬处閸撴繈濡甸幇鏉跨闁圭虎鍨辩€氳棄鈹戦悙鑸靛涧缂傚秮鍋撳銈庡亜椤﹂潧鐣烽幋锔藉亹缂備焦顭囬崢閬嶆煙閸忚偐鏆橀柛銊ヮ煼閹瞼浠﹂惌顐㈢秺閹亪宕ㄩ婊勬闂備胶鎳撶壕顓㈠磻閵堝棔绻嗛柣鎴犵摂閺佸﹪鏌涜箛鏇炲付濞存粌鍚嬬换婵嬫偨闂堟刀銉︺亜閿濆骸鏋ゆ俊鍙夊姍瀵挳濮€閻樼绱遍梻浣侯攰閹活亞绮婚幋鐘差棜鐟滅増甯楅悡娑氣偓骞垮劚妤犲憡绂嶅┑瀣厸闁糕剝顭囬惌鎺楁煛瀹€鈧崰鏍箖濠婂吘鐔兼嚒閵堝懎绠伴梻鍌欒兌鏋い鎴濇楠炴劙骞栨笟濠勭◤濠电娀娼ч鍡涘磻閸岀偛绠圭紒顔款潐椤﹥绻涢懖鈺冨笡缂佺粯绻堥幃浠嬫濞戞鍕冮柣鐔哥矋濠㈡﹢宕锔光偓锕傚炊椤掆偓閻忔娊鏌熸０浣哄妽缂傚秴娴风槐鎾诲磼濞嗘垵濡介柦鍐ㄥ船閳规垿顢涘☉娆忓攭闂佸搫鐫欓崱娆戞澑闂佽鍎虫晶搴ㄥ汲濞戙垺鈷戦梺顐ゅ仜閼活垱鏅堕鈧弻銊╁即閵娿倝鍋楅悗娈垮枦椤曆囧煡婢舵劕顫呴柣妯活問閸熷姊虹拠鏌ュ弰婵炰匠鍥х婵犲﹤鐗婇崑顏堟煕閺囥劌鐏￠柍閿嬪灴濮婂宕奸悢琛″濡炪們鍎茬划鎾诲蓟濞戙垺鍋勫瀣嚱缁辩偟绱撴笟鍥ф灕妞ゆ泦鍥х叀濠㈣埖鍔曢～鍛存煟濡澧俊顐ゅ枑缁绘繈鎮介棃娑楃捕濠碘槅鍋呴悷鈺佺暦閺囥垹绀冩繛鏉戭儐閻忎焦绻濋棃娑樷偓鎼佸箟閿熺姴鐓曢柟鐑樺殮瑜版帗鏅查柛銉戝啫顬嗛梻浣规偠閸婃劙宕戦幘鏂ユ斀闁绘﹩鍋呮刊浼存煕濞戝崬鏋熼柣婵愪邯濮婃椽宕ㄦ繝鍌滅懖闁汇埄鍨界换婵嬪Υ娴ｇ硶妲堥柕蹇娾偓鏂ュ亾閻戣姤鐓犵痪鏉垮船婢ь喗顨ラ悙鑼ⅵ婵﹦绮幏鍛村川婵犲懐顢呴梻浣呵圭花娲磹濠靛棛鏆﹂梻鍫熶緱濞尖晠鏌ｉ幇顓炵亰婵顨婂娲捶椤撶偛濡洪梺瑙勭摃椤曆囧煝瀹ュ應鍫柛顐ゅ暱閹锋椽姊洪崨濠冨鞍鐟滄澘鍟粋宥呪攽閸垻锛滃銈嗘濡嫰鍩㈤弴鐕佹闁绘劘鎻懓璺ㄢ偓瑙勬礃缁秹骞忛崨瀛樺剬闁告縿鍎抽、鍛節绾板纾块柛瀣灴瀹曟劙寮介鐐殿唶婵°倧绲介崯顐ゅ婵犳碍鐓熼柡鍐ㄥ€哥敮鍫曟煛閸曗晛鍔﹂柡灞诲妼閳规垿宕遍埡鍌傃囨⒑缂佹ɑ灏伴柣鈺婂灦楠炲啫顫滈埀顒勫箖濞嗘挸绠甸柟鍝勬鐎垫牗绻濋悽闈涗粶鐎殿喖鐖奸獮鎰板箮閽樺鎽曢梺鏂ユ櫅閸燁垱鍒婇幘顔界厽闁绘柨鎲＄欢鍙夈亜椤愶絽鐏╃紒杈ㄦ尰閹峰懘宕崟鎴秮閺岋綁鍩℃繝鍌滀哗濡炪倖娲╃紞渚€鐛鈧、娆撴寠婢跺鐫忛梻鍌欑劍閸庡啿霉濮樿泛纾婚柟鐑橆殔閻ゎ噣鎮橀悙鎻掆挃缂佲檧鍋撻柣搴″悁閸楁娊寮ㄩ崡鐑嗙唵婵せ鍋撻柛鈹惧亾濡炪倖甯婄欢鈥斥枔閺囥垺鐓欏〒姘仢婵倿鏌熼鐣岀煉闁圭ǹ锕ュ鍕暆鐎ｎ剙鑰挎繝鐢靛Х閺佹悂宕戦悙鍝勫瀭闁告挷绀侀弸鍫⑩偓骞垮劚閹虫劙寮抽敃鍌涚厱婵炲棗娴氬Σ褰掓煕濞嗗繒绠插ǎ鍥э躬閹瑦锛愬┑鍡橆唲濠电偛鐡ㄧ划鎾剁不閺嶎厼绠栨俊銈傚亾闁崇粯鎹囧畷褰掝敊閻ｅ苯钂嬮梻鍌欒兌鏋い鎴濇嚇閺佸啴顢旈崼鐔蜂粧濡炪倖鏌ㄥΣ鍫ユ偄閸忕厧娈愰梺鍐叉惈閸犳稓妲愰崣澶岀瘈缁剧増蓱椤﹪鏌涢妸銉э紞闁告帗甯￠、娑㈡倻閸℃ɑ娅嗛梻浣虹帛閺屻劑宕ョ€ｎ喖鐓曢柟鐑樺殮瑜版帗鏅查柛顐ｇ渤閹惧墎妫い鎾跺仧閸╋綁鏌＄仦鍓ф创闁轰焦鍔樼粻娑㈠即閻樿櫕顔撳┑掳鍊楁慨鐑藉磻濞戞碍宕叉慨妞诲亾鐎殿噮鍋婇獮妯肩磼濡粯顏熼梻浣芥硶閸ｏ箓骞忛敓锟�?


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
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆撴倻濡桨鐢绘繝鐢靛Т閿曘倝宕板顑╃喖鍩€椤掑嫭鍊垫繛鍫濈仢閺嬬喖鏌熼鐓庘偓鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸閻忕偟纭堕崑鎾崇暦閸ャ劍鐣烽梻渚€鈧偛鑻晶瀵糕偓瑙勬磻閸楀啿顕ｆ禒瀣垫晣闁绘劗鏁搁弳顐︽煟閻斿摜鐭嬮柡灞诲姂閹箖鏌ㄧ€ｎ剟妾梺鍛婄☉閿曘倖绂嶅⿰鍫熲拺闁告稑锕︾粻鎾绘倵濮樼厧澧柍缁樻崌婵＄兘鍩￠崒妤佸闂備胶枪閺堫剟鎮疯钘濋柨鏇炲€归悡娆撴偣閸ュ洤鎳愰惁鍫ユ倵鐟欏嫭绀€婵犫偓闁秴绠熼柟缁㈠枛缁€瀣亜閹哄棗浜鹃悗娈垮枛濞尖€愁潖濞差亜鎹舵い鎾墲椤庢姊洪崫銉バｉ柣妤冨█楠炲啴鍨鹃幇浣瑰缓闂侀€炲苯澧寸€殿喖顭烽幃銏ゅ川婵犲嫮肖濠德板€х徊浠嬪疮椤栫儐鏁佺€广儱顦伴埛鎴犵磼鐎ｎ偒鍎ラ柛搴㈠姍閺屾盯寮埀顒勬偡閳哄懐宓侀柛鎰╁壆閺冨牆宸濇い鎾跺枎缁佽埖淇婇悙顏勨偓鏍箰妤ｅ啫纾规い鎰剁畱椤懘骞掗搹顐ゎ洸婵犲﹤鐗婇悡蹇擃熆閼哥數鈽夋い鈺婂墰缁辨帡鎮╅幇浣哄姺缂備浇椴哥敮妤€顭囪箛娑樜╅柨鏇楀亾闂傚绉瑰娲偡閺夋寧些闂佺娅曢敋闁伙絿鍏橀、鏇㈡晝閳ь剛绮堢€ｎ剛妫柣妤€鐓鍛洸闁告挆鈧崑鎾诲礂婢跺﹣澹曢梻浣告啞濞诧箓宕滃☉銏犲偍闂侇剙绉甸埛鎴︽⒒閸喕鍎愮憸鐗堝笒绾惧潡鏌熼幆鐗堫棄闁哄嫨鍎甸弻銊モ攽閸℃顦遍梺绋款儐閹告悂鍩㈤幘璇插瀭妞ゆ梻鏅禍顏呬繆閻愵亜鈧倝宕㈡ィ鍐ㄧ婵☆垯璀﹂崵鏇熴亜閹板墎鐣辩紒鐘崇⊕閵囧嫰骞樼捄杞版埛缂備焦鍔楅崑鐐垫崲濠靛鍋ㄩ梻鍫熷垁閵忋倖鍊垫慨妯哄船閸樺鈧娲忛崕闈涚暦閵娧€鍋撳☉娅辨岸骞忕紒妯肩閺夊牆澧介幃濂告煙閾忣偅宕屾い銏¤壘楗即宕ㄩ婊愮床闂佽崵濮村ú锕併亹閸愵喖鐒垫い鎺嗗亾婵炵》绻濋幃浼搭敊閽樺绐炴繝鐢靛仧閸嬫捇锝炲澶嬧拺闂傚牊绋撶粻鍐测攽椤旀儳鍘存い銏＄懇楠炴帡寮崒婊愮闯闂備胶枪閺堫剟鎮疯瀹曟繂顓奸崱鎰盎濡炪倖鎸鹃崑鐐哄窗濡皷鍋撶憴鍕８闁搞劋绮欓悰顕€寮介妸锕€顎撻梺绋跨箰椤︽壆鈧艾銈稿缁樻媴閸涘﹤鏆堢紓浣割儐閸ㄥ潡寮崘顔芥櫜闁搞儺鐓堥崑銊╂⒒娓氬洤澧紒澶屾暬閹繝寮撮姀锛勫幗濠碘槅鍨甸褎鏅堕娑氱闁割偆鍠撻惌宀勬煃瑜滈崜婵嬶綖婢跺⊕鍝勵潨閳ь剙鐣峰┑鍡忔瀻闁规崘娉涚粊锕傛⒑閹肩偛鍔撮柛鎾村哺瀹曟垹鈧綆鍠楅悡鏇熴亜閹邦喖孝闁诲繆鏅滈〃銉╂倷閺夋垵顫掗梺鍝勭焿缁查箖骞嗛弮鍫濈伋闁告劘灏欢鐔兼⒒娴ｅ懙褰掝敄閸℃稑绠查柛銉墮缁犳牗淇婇妶鍛櫤闁哄懏鐓￠獮鏍垝閸忓浜鹃柟棰佺劍妤犲嫰姊婚崒娆愵樂缂侀硸鍠氱划濠氬箣閿旇棄浜遍梺瑙勫礃椤曆呯不閺嶃劋绻嗛柕鍫濇噹閺嗙偤鏌涢悩鍙夘棦闁哄本鐩鎾Ω閵夈儺娼炬俊鐐€х拹鐔煎礉閹达箑钃熼柨婵嗩槸椤懘鏌嶆潪鎷屽厡濞寸媭鍙冮弻锝夊閳轰胶浠梺鍝ュУ閻楃娀鐛崘顭戞建闁逞屽墴閻涱噣骞掗幊铏⒐閹峰懘宕崟顐ゎ啈闂傚倸鍊烽懗鍓佸垝椤栫偛绀夐柡鍥ュ焺閺佸銇勯幘鍗炵仼闁绘挻绻堥弻鐔煎礈瑜忕敮娑㈡煟閹惧啿鏆熼柟鑼归オ浼村醇濠靛牜妲舵繝鐢靛仜濡瑩骞愰幖浣哥＜闁靛ň鏅滈悡娑氣偓骞垮劚閸燁偅淇婇崸妤佺厽婵犻潧妫涢崺锝夋煛瀹€瀣瘈鐎规洖鐖兼俊鐑藉Ψ瑜岄惀顏堟⒒娴ｇ懓鈻曢柡鈧柆宥呭瀭闁秆勵殔閽冪喐绻涢幋鐐冩岸寮ㄦ禒瀣厱闁斥晛鍠氬▓銏犆圭亸鏍т壕闂傚倸鍊风粈渚€鎮块崶顒夋晪鐟滄棁妫熼梺姹囧灮鏋柦鍐枑缁绘盯骞嬮悙鑼姲闂佺ǹ顑嗛幑鍥极瀹ュ纭€闁绘劕寮跺▓銊︾節閵忥綆娼愭繛鑼枎椤繑绻濆顒傦紲濠电偛妫欑敮鎺楀储閿熺姵鐓涢悗锝呭悁缁ㄥジ鏌曢崶褍顏鐐差儏閳规垿宕堕懜纰夌礂濠德板€楁慨鐑藉磻閻愬搫绀夋繛鍡樻尵瀹撲線鏌″鍐ㄥ闁荤喓鎳撻埞鎴︽偐閹绘帩浼€闂佸磭顑曢崐鏍崲濠靛鍋ㄩ梻鍫熺▓閺嬪懎鈹戦悙鏉垮皟闁搞儴鍩栭弲鐐烘⒑閸︻厼顣兼繝銏★耿瀵憡绗熼埀顒勫蓟濞戙垹鐒洪柛鎰典簴婵洭姊虹紒妯诲鞍婵炶尙鍠栭獮鍐潨閳ь剟骞冨▎鎾崇闁瑰搫妫欑€垫牠姊绘担鑺ャ€冮柣鎺炵畵閹囨偐鐠囪尪鎽曢梺鍝勬川閸犳挾绮绘ィ鍐╃厽闁逛即娼ф晶顖炴煟濠靛洦绀嬫慨濠冩そ楠炲酣鎳為妷锔芥闂備焦鎮堕崝灞筋焽閿熺姷宓侀柛鎰靛幑娴滃綊鏌熼悜妯虹仼闁稿﹦鍋ゅ娲箰鎼达絿鐣甸梺绋匡工閹诧紕绮嬪澶婄鐟滃宕戦幘鑸靛枂闁告洦鍓涢ˇ銉モ攽閻愰鍤嬬紓宥勭閻ｇ兘顢曢埗鑺ユ瀹曘劑顢欑憴鍕伖闂備浇宕垫慨鏉懨洪敃浣典汗闁告劦鍣弫鍡涙煏韫囧鈧牠鎮￠悢鎼炰簻闁圭偓顨呴崯顖炴偟濠靛鍊甸悷娆忓缁岃法绱掔紒妯肩疄闁绘侗鍠楅幆鏃堝Ω閿曗偓濞堢喖姊洪棃娑辨Ф闁稿孩澹嗛崚鎺曨槼濞ｅ洤锕幃娆擃敂閸曘劌浜鹃柡宓本缍庢繝鐢靛У閼归箖鎮￠弴銏＄厱妞ゆ劧绲跨粻銉╂煟閹哄秶鐭欓柡灞炬礃缁绘盯鎮欓浣哄絾闂備胶绮幐鎼佸疮娴兼潙绠熺紒瀣氨閸亪鏌涢锝囩畼妞わ富鍙冨铏圭磼濡儵鎷婚梺鍛婎焼閸℃瑧顔斿┑鐘垫暩閸嬬娀骞撻鍡楃筏閻犳亽鍔嶉崗婊堟煕椤愶絾绀€闁告垹濞€楠炴牕菐椤掆偓婵′粙鏌ｉ幘瀵告噰闁哄矉缍侀獮鍥濞戞﹩娼界紓鍌氬€哥粔鐢稿垂閸撲焦宕叉繛鎴欏灩闁卞洭鏌曡箛鏇炐ｉ柣蹇撶墕閳规垿顢欑涵閿嬫暰濠碉紕鍋犲Λ鍕亱闂佸憡鍔戦崝澶娢ｉ崼銉ョ骇闁割偒鍋嗛崢娑㈡煃瑜滈崜娆徫涘┑瀣摕闁挎繂妫欓崕鐔兼煏婵犲繘妾梺娆惧弮濮婃椽宕崟顓炩拡闂佸憡鎸荤粙鎾诲礆閹烘柡鍋撻敐搴℃灈缂佺姵濞婇弻锟犲炊閵夈儱顬夐梺瀹狀嚙閺堫剛鎹㈠┑鍡忔灁闁割煈鍠楅悘鍫ユ⒑閹稿孩纾搁柛濠冪箞閸ㄩ箖鏁冮埀顒勬偩閿熺姴绠ユい鏃囧Г椤旀洟姊绘担鍛婅础闁稿簺鍊濆畷褰掓偄閻撳骸鍤戦柟鍏肩暘閸斿秹鎮″▎鎾寸厵缁炬澘宕獮鏍殽閻愭惌娈橀柟顕呭櫍閺佸啴宕掑☉鎺撳闂備胶枪閺堫剟鎮疯钘濋柨鏃傛櫕缁♀偓闂侀€炲苯澧撮柡灞芥椤撳ジ宕ㄩ姘疄闂傚倷绀佹竟濠囧磻娓氣偓瀹曟洘娼忛埡渚囨闁荤姴娲︾粊鎾绩娴犲鐓熸俊顖濐嚙缁插鏌嶈閸撴瑩鈥﹀畡閭﹀殨闁规儼濮ら弲婵嬫煕鐏炲墽銆掗柛妯兼暬濮婅櫣绱掑鍡樼暥闂佺粯顨呴敃锕€鈻庨姀銈呰摕闁靛鑵归幏娲⒑閸涘⿴娈橀柛瀣仱瀹曟澘顫滈埀顒勫蓟閳ュ磭鏆嗛悗锝庡墰閻﹀牓鎮楃憴鍕闁绘牕銈稿畷娲焺閸愨晛顎撻梻浣哥仢椤戝懘鎮￠弴銏♀拻闁稿本鐟ч崝宥夋倵缁楁稑鎳愰惌娆撴煙閻戞﹩娈曢柛瀣剁節閺屽秵娼幍顔跨獥濠电偛鍚嬮悧妤冩崲濞戞﹩鍟呮い鏃囧吹閻╁孩绻涚壕瀣汗濠电偐鍋撻梺鍝勬湰閻╊垰顕ｉ幘顔嘉╅柕澶堝劤椤斿洭姊绘担鍛婃儓妞ゆ垵鎳庤灋婵犻潧妫鏍煣韫囨挻璐￠柣顓熺懄缁绘盯宕卞Ο鍝勫Б闂佸憡鎼╅崜鐔奉潖閾忓湱鐭欐繛鍡樺劤閸撻亶姊洪崷顓熷殌婵炲眰鍊濋敐鐐剁疀濞戞瑦鍎梺闈╁瘜閸橀箖鎮￠幘鏂ユ斀闁绘劕寮堕ˉ鐐烘煕鎼淬垹鈻曠€规洘绮撻弻鍡楊吋閸″繑瀚奸梻浣藉吹閸犳挻鏅跺Δ鍛柈闁绘劗鍎ら悡娆愩亜閺冨倸甯堕柍褜鍓欏鈥愁嚕椤愶箑绠涢柣妤€鐗嗛埀顒勬敱缁绘盯寮堕幋顓炲壉闂佸搫鍊甸崑鎾绘⒒娴ｇ瓔鍤欓柛鎴犳櫕瀵板﹥绂掔€ｎ偄鈧埖鎱ㄥΟ鎸庣【闁汇倝绠栭弻鏇＄疀鐎ｎ亖鍋撻弽顓炵厱闁瑰濮风壕濂告倵閿濆骸浜滄い鏇熺矋缁绘繈鍩€椤掑嫬绠ユい鏂垮⒔閿涙粓姊洪棃娴ㄥ綊宕愬Δ鍛剹婵炲棗绻嗛弨浠嬫煃閳轰礁鏆為柛濠冨姍閺屾盯鍩＄€ｎ剛鐦堥悗瑙勬礃鐢帟鐏冩繛杈剧到婢瑰﹥瀵奸弽顓熲拻闁稿本鑹鹃埀顒勵棑缁牊绗熼埀顒勭嵁婢舵劖鏅柛鏇ㄥ墮鎼村﹪姊虹粙鎸庢拱濠㈣娲熷畷鎴﹀箻閹颁焦鍍甸梺缁樻尭濞撮攱绂掗幖浣光拺闁圭ǹ娴风粻姗€鏌涚€ｃ劌鈧繂顕ｉ锕€绀冩い鏃囧亹閿涙粌鈹戦悙鏉戠仸闁煎綊绠栭悰顕€宕奸妷锔规嫽婵炶揪绲介幉锟犲箚閸儲鐓曢柣鏂挎惈閳诲牏鈧娲橀崹鍧楃嵁濡偐纾兼俊顖滃帶楠炲牓姊绘笟鈧褎鐏欓梺绋垮閻撯€崇暦閹达箑唯闁冲搫鍊婚崢钘夆攽閻愭潙鐏ョ€规洦鍓欓埢宥咁吋閸ワ絽浜鹃悷娆忓缁€鍐煥閺囨ê鐏查柕鍡曠閳诲酣骞嬪┑鍥┾偓鎶芥煛婢跺﹦澧戦柛鏂挎捣缁棃鏌嗗鍡忔嫽闂佺ǹ鏈悷褏绮ｉ弮鈧换娑欐媴閸愬弶澶勯柛瀣儔閺屾盯鍩勯崘顏佹缂備胶濮锋繛鈧鐐寸墬濞煎繘宕滆閸嬔囨⒑缂佹鐭岀紒顕呭灣閹广垹鈽夐姀鐘殿吅闂佺粯鍔曢悘姘跺闯椤斿墽纾藉ù锝呭级椤庡棝鏌涚€ｎ偅宕屾慨濠勭帛閹峰懘宕烽鐔诲即闂備焦鎮堕崝蹇撐涢崟顖椻偓锕傚炊椤忓秵些闂備胶鍎靛Σ鍛村矗閸愵煈娼栭柧蹇氼潐鐎氭岸鏌熺紒妯轰刊闁告柨顦靛娲川婵犲倻浼囧銈庡亜椤﹁京鍒掔€ｎ亶鍚嬪璺侯儏閳ь剛鍏橀弻娑樷枎韫囨挷鍠婄紓鍌氱С缁舵岸鎮伴鈧畷鍫曨敆閳ь剛鐥閺屾盯鈥﹂幋婵囩亪缂備椒绶￠崳锝咁潖缂佹鐟归柍褜鍓欓…鍥槾闁瑰箍鍨藉畷鍗炍旀繝鍕ㄥ亾閸撲焦鍠愰柡鍐ㄧ墢瀹撲線鐓崶銊︾缁炬儳鍚嬫穱濠囶敍濠靛棔姹楀銈嗘⒐濞茬喖寮婚埄鍐ㄧ窞濠电姴瀚。鐑樼節閳封偓閸屾粎鐓撻悗瑙勬礃绾板秶鈧絻鍋愰埀顒佺⊕椤洭宕㈤悽鍛娾拺闁稿繘妫块懜顏堟煕鎼淬垹鈻曢柟顖氳嫰閻ｆ繈鍩€椤掑倹顫曢柟鐑橆殢閺佸﹪鏌ら幁鎺戝姶婵☆偄鍟村铏规嫚閳ヨ櫕鐏嶇紓渚囧枛閻線鎮橀幒妤佲拻闁稿本顨呮禍楣冩⒑瑜版帒浜伴柛鐘宠壘閳绘棃宕稿Δ浣叉嫽闂佺ǹ鏈悷褏绮ｉ弮鈧换娑欐媴閸愬弶鐦介柕濞炬櫅閻掑灚銇勯幒鎴濐仾闁抽攱甯￠弻娑㈠即閵娿儰绨诲銈呯箚閺呯娀寮婚敐澶嬫櫜闁告侗鍘戒簺闂備椒绱徊鍓ф崲閸儱鏄ラ柍褜鍓氶妵鍕箳閹存繍浠奸梺缁樺笒閻忔岸濡甸崟顖氱闁瑰瓨绻冮悘鎾斥攽閻愭彃鎮戦柛鏃€鐗滈幑銏犫槈閵忕姷鐓戞繝銏ｅ煐缁嬫牠鍩€椤掆偓濞硷繝寮婚弴銏犲耿婵°倐鍋撻柡鍡悼閳ь剝顫夊ú蹇涘礉瀹ュ拋鐒介煫鍥ㄧ☉缁€鍫㈡喐韫囨稑鐓熼柕鍫濇缁♀偓闂佹眹鍨藉褍鐡梻浣瑰濞插繘宕愬┑瀣畺鐟滄柨鐣烽崡鐐╂婵炲棗绻嗛崑鎾诲锤濡や胶鍘卞銈庡幗閸ㄥ灚绂嶉弽顬＄懓饪伴崟顓犵厜闂佸搫鐬奸崰鏍х暦椤愶箑绀嬫い鎺戭槹椤ワ絽鈹戦悙鑼憼缂侇喖绉瑰畷鏇㈠箮鐟欙絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯鍩勯崘鍓у姺闂佸疇顫夌粙鎾舵閹捐纾兼慨妯荤樂閿涘嫮纾奸柤鎼佹涧閸濆搫鈹戦垾宕囨憼缂佹鍠栭崺鈧い鎺嗗亾妞ゆ洩缍侀獮姗€顢欑喊杈ㄧ秱闂備焦鏋奸弲娑㈠疮椤栫偞鍋熼柡宥庡幗閳锋帒銆掑锝呬壕闂侀€炲苯澧伴柛瀣洴閹崇喖顢涘☉娆愮彿闂佽顔栭崰姘卞閸忕浜滈柡鍌氱仢閳锋棃鎮介娑氥€掔紒杈ㄥ笧缁辨帒螣閼测晝鏆ゆ俊鐐€ら崑鍛崲閸儯鈧礁顫滈埀顒勫箖濞嗘挸绾ч柛顭戝櫘閸ゆ瑩姊婚崒姘偓鎼佸磹閻戣姤鍊块柨鏇炲€哥粻鏍煕椤愶絾绀€缁剧偓瀵х换婵囩節閸屾粌顣虹紓浣插亾闁告劏鏂傛禍婊堢叓閸ャ劍灏靛褋鍨婚埀顒冾潐濞叉鍒掑畝鍕厴闁硅揪闄勯弲顒勬煕閺囥劌浜楅柡澶樺弮濮婅櫣绮欓崠鈩冩暰闂佹悶鍔岄悘婵嬫偩閻戣棄鍗抽柕蹇娾偓铏吇婵＄偑鍊栫敮濠勭矆娴ｈ鍎熷┑鐘叉处閳锋垿鏌涢幘鏉戠祷濞存粍绻勭槐鎺旀嫚閼碱儷銏°亜椤撶偞鍠樼€殿喕绮欐俊姝岊槾妞ゆ梹娲熼幃妤呮偡閺夋妫岄梺鍝ュУ濞叉粓鎳為柆宥呯缂佹妗ㄧ花濠氭⒑闂堟侗妲堕柛搴ゆ珪閺呰泛鈽夊杈╋紲缂傚倷鐒﹂…鍥虹€涙﹩娈介柣鎰▕閸庢棃鏌熼鐣屾噮闁圭懓瀚粭鐔碱敍濞戣鲸锛堝┑鐘殿暜缁辨洟宕戦幋锕€纾圭憸蹇擃嚗婵犲啰顩烽悗锝庝簽椤旀劕顪冮妶鍡楀Ё缂佺姵鍨块幃鈥愁潨閳ь剟寮婚悢鍛婄秶闁告挆鍛闂備胶绮幖顐ゆ崲濠靛钃熸繛鎴欏灩閻掓椽鏌涢幇鍓佺窗婵炲矈浜炵槐鎾存媴闂堟稑顬嬮梺鍛婎焼閸ワ絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯骞囬埡浣割瀳濡炪値鍓欓悧鎾愁潖濞差亜绠伴幖娣灩椤︹晜绻涚€涙鐭ゅù婊勭箖缁岃鲸绻濋崟銊ヤ壕婵炴垶鐟悞浠嬫煟閻旈绉洪柡灞界Х椤т線鏌涢幘瀵告噰妞ゃ垺宀搁弫鎰緞濡粯娅旈梻渚€鈧偛鑻晶顕€鏌ｉ敐鍥у幋闁诡喒鍓濋幆鏂课熺紒妯绘緫闂備浇顕ч崙鐣岀礊閸℃顩叉繝闈涱儏濮瑰弶銇勮箛鎾跺闁抽攱甯￠弻娑氫沪閸撗勫櫘濡炪倧璁ｇ粻鎾诲蓟閻斿搫鏋堥柛妤冨仒缁ㄥ鏌ｉ幘鍗炩偓婵嬪蓟閺囩喓绠鹃柣鎰靛墯閻濇棃姊哄Ч鍥р偓銈夊闯閿濆钃熸繛鎴炵矤濡茬厧顪冮妶鍐ㄥ闁硅櫕鍔楅崚鎺撶節濮樺吋鏅┑鐐村灦濮樸劑顢欓弴銏″€甸柣鐔告緲椤ュ繘鏌涢悩铏闁奸缚椴哥缓浠嬪川婵犲嫬骞楅梻浣筋潐瀹曟ê鈻斿☉銏犲嚑婵炴垯鍨洪悡娑氣偓鍏夊亾閻庯綆鍓欓崺宀勬煣娴兼瑧绉柡灞剧☉閳规垿宕卞Δ濠佹偅缂傚倷鐒﹂〃蹇涘矗閸愵煈娼栨繛宸簼閸嬪倿骞栫划瑙勵潑婵☆偄鏈换娑氣偓娑欘焽閻倕霉濠婂簼绨绘い鏇悼閹风姴霉鐎ｎ偒娼旈梻渚€娼х换鎺撴叏閻㈡潌澶娾攽鐎ｎ偀鎷洪梺绋跨箳閸樠勬櫠椤栫偞鐓曢柕濞垮€曞畵鍡樸亜閵忊剝顥堟い銏★耿婵偓闁抽敮鍋撻柟閿嬫そ閺岋絾鎯旈埄鍐婵犳鍨奸崣鍐ㄧ暦闂堟稈鏋庨柟瀵稿Х閿涙粍绻濋姀锝嗙【闁挎洩濡囩划鏂棵洪鍛幗濠电偞鍨靛畷顒€鈻嶅鍥ｅ亾鐟欏嫭绀€闁绘牕銈搁妴浣肝旀担鐟邦€撻梻鍌楀亾闁归偊鍠氱粔鍧楁⒒閸屾艾鈧绮堟担鍦彾濠电姴娲ょ壕璇层€掑锝呬壕閻庤娲滈、濠囧Φ閹版澘绠抽柡鍌濇硶閵堬箓鏌ｆ惔锛勭暛闁稿酣浜惰棟濞村吋娼欓悡鏇㈡煙閻戞ê娈憸鐗堝笚閺呮煡鏌涘☉鍗炴灈闁哄棗鐗撳娲传閵夈儲鐏侀梺绋款儍閸婃繂顕ｇ拠娴嬫闁冲灈鏂侀崑鎾绘晝閸屾氨顓哄┑鐘绘涧閻楀懎顩奸幘缁樼厽閹兼番鍊ゅ鎰箾閸欏鑰跨€规洘绻傞埢搴ょ疀閿濆懏顓垮┑鐐差嚟婵挳顢栭幇鏉挎瀬闁告劦鍠楅悡鐔兼煛閸愩劍澶勯柤鐢垫嚀椤法鎲撮崟顒傤槬闂佸疇顫夐崹鍧椼€佸▎鎴犻┏閻庯綆鍓欐慨娲⒒娴ｅ懙褰掝敄閸℃稑绠板Δ锝呭暙閻掑灚銇勯幒宥堝厡闁哥喐鐓￠弻鐔煎礄閵堝棗顏�


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
                //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆撴倻濡桨鐢绘繝鐢靛Т閿曘倝宕板顑╃喖鍩€椤掑嫭鍊垫繛鍫濈仢閺嬬喖鏌熼鐓庘偓鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸閻忕偟纭堕崑鎾崇暦閸ャ劍鐣烽梻渚€鈧偛鑻晶瀵糕偓瑙勬磻閸楀啿顕ｆ禒瀣垫晣闁绘劗鏁搁弳顐︽煟閻斿摜鐭嬮柡灞诲姂閹箖鏌ㄧ€ｎ剟妾梺鍛婄☉閿曘倖绂嶅⿰鍫熲拺闁告稑锕︾粻鎾绘倵濮樼厧澧柍缁樻崌婵＄兘鍩￠崒妤佸闂備胶枪閺堫剟鎮疯钘濋柨鏇炲€归悡娆撴偣閸ュ洤鎳愰惁鍫ユ倵鐟欏嫭绀€婵犫偓闁秴绠熼柟缁㈠枛缁€瀣亜閹哄棗浜鹃悗娈垮枛濞尖€愁潖濞差亜鎹舵い鎾墲椤庢姊洪崫銉バｉ柣妤冨█楠炲啴鍨鹃幇浣瑰缓闂侀€炲苯澧寸€殿喖顭烽幃銏ゅ川婵犲嫮肖濠德板€х徊浠嬪疮椤栫儐鏁佺€广儱顦伴埛鎴犵磼鐎ｎ偒鍎ラ柛搴㈠姍閺屾盯寮埀顒勬偡閳哄懐宓侀柛鎰╁壆閺冨牆宸濇い鎾跺枎缁佽埖淇婇悙顏勨偓鏍箰妤ｅ啫纾规い鎰剁畱椤懘骞掗搹顐ゎ洸婵犲﹤鐗婇悡蹇擃熆閼哥數鈽夋い鈺婂墰缁辨帡鎮╅幇浣哄姺缂備浇椴哥敮妤€顭囪箛娑樜╅柨鏇楀亾闂傚绉瑰娲偡閺夋寧些闂佺娅曢敋闁伙絿鍏橀、鏇㈡晝閳ь剛绮堢€ｎ剛妫柣妤€鐓鍛洸闁告挆鈧崑鎾诲礂婢跺﹣澹曢梻浣告啞濞诧箓宕滃☉銏犲偍闂侇剙绉甸埛鎴︽⒒閸喕鍎愮憸鐗堝笒绾惧潡鏌熼幆鐗堫棄闁哄嫨鍎甸弻銊モ攽閸℃顦遍梺绋款儐閹告悂鍩㈤幘璇插瀭妞ゆ梻鏅禍顏呬繆閻愵亜鈧倝宕㈡ィ鍐ㄧ婵☆垯璀﹂崵鏇熴亜閹板墎鐣辩紒鐘崇⊕閵囧嫰骞樼捄杞版埛缂備焦鍔楅崑鐐垫崲濠靛鍋ㄩ梻鍫熷垁閵忋倖鍊垫慨妯哄船閸樺鈧娲忛崕闈涚暦閵娧€鍋撳☉娅辨岸骞忕紒妯肩閺夊牆澧介幃濂告煙閾忣偅宕屾い銏¤壘楗即宕ㄩ婊愮床闂佽崵濮村ú锕併亹閸愵喖鐒垫い鎺嗗亾婵炵》绻濋幃浼搭敊閽樺绐炴繝鐢靛仧閸嬫捇锝炲澶嬧拺闂傚牊绋撶粻鍐测攽椤旀儳鍘存い銏＄懇楠炴帡寮崒婊愮闯闂備胶枪閺堫剟鎮疯瀹曟繂顓奸崱鎰盎濡炪倖鎸鹃崑鐐哄窗濡皷鍋撶憴鍕８闁搞劋绮欓悰顕€寮介妸锕€顎撻梺绋跨箰椤︽壆鈧艾銈稿缁樻媴閸涘﹤鏆堢紓浣割儐閸ㄥ潡寮崘顔芥櫜闁搞儺鐓堥崑銊╂⒒娓氬洤澧紒澶屾暬閹繝寮撮姀锛勫幗濠碘槅鍨甸褎鏅堕娑氱闁割偆鍠撻惌宀勬煃瑜滈崜婵嬶綖婢跺⊕鍝勵潨閳ь剙鐣峰┑鍡忔瀻闁规崘娉涚粊锕傛⒑閹肩偛鍔撮柛鎾村哺瀹曟垹鈧綆鍠楅悡鏇熴亜閹邦喖孝闁诲繆鏅滈〃銉╂倷閺夋垵顫掗梺鍝勭焿缁查箖骞嗛弮鍫濈伋闁告劘灏欢鐔兼⒒娴ｅ懙褰掝敄閸℃稑绠查柛銉墮缁犳牗淇婇妶鍛櫤闁哄懏鐓￠獮鏍垝閸忓浜鹃柟棰佺劍妤犲嫰姊婚崒娆愵樂缂侀硸鍠氱划濠氬箣閿旇棄浜遍梺瑙勫礃椤曆呯不閺嶃劋绻嗛柕鍫濇噹閺嗙偤鏌涢悩鍙夘棦闁哄本鐩鎾Ω閵夈儺娼炬俊鐐€х拹鐔煎礉閹达箑钃熼柨婵嗩槸椤懘鏌嶆潪鎷屽厡濞寸媭鍙冮弻锝夊閳轰胶浠梺鍝ュУ閻楃娀鐛崘顭戞建闁逞屽墴閻涱噣骞掗幊铏⒐閹峰懘宕崟顐ゎ啈闂傚倸鍊烽懗鍓佸垝椤栫偛绀夐柡鍥ュ焺閺佸銇勯幘鍗炵仼闁绘挻绻堥弻鐔煎礈瑜忕敮娑㈡煟閹惧啿鏆熼柟鑼归オ浼村醇濠靛牜妲舵繝鐢靛仜濡瑩骞愰幖浣哥＜闁靛ň鏅滈悡娑氣偓骞垮劚閸燁偅淇婇崸妤佺厽婵犻潧妫涢崺锝夋煛瀹€瀣瘈鐎规洖鐖兼俊鐑藉Ψ瑜岄惀顏堟⒒娴ｇ懓鈻曢柡鈧柆宥呭瀭闁秆勵殔閽冪喐绻涢幋鐐冩岸寮ㄦ禒瀣厱闁斥晛鍠氬▓銏犆圭亸鏍т壕闂傚倸鍊风粈渚€鎮块崶顒夋晪鐟滄棁妫熼梺姹囧灮鏋柦鍐枑缁绘盯骞嬮悙鑼姲闂佺ǹ顑嗛幑鍥极瀹ュ纭€闁绘劕寮跺▓銊︾節閵忥綆娼愭繛鑼枎椤繑绻濆顒傦紲濠电偛妫欑敮鎺楀储閿熺姵鐓涢悗锝呭悁缁ㄥジ鏌曢崶褍顏鐐差儏閳规垿宕堕懜纰夌礂濠德板€楁慨鐑藉磻閻愬搫绀夋繛鍡樻尵瀹撲線鏌″鍐ㄥ闁荤喓鎳撻埞鎴︽偐閹绘帩浼€闂佸磭顑曢崐鏍崲濠靛鍋ㄩ梻鍫熺▓閺嬪懎鈹戦悙鏉垮皟闁搞儴鍩栭弲鐐烘⒑缁洖澧茬紒瀣灩婢规洝銇愰幒鎾嫼闂佸壊鐓堥崳顕€宕曞鍚ょ懓饪伴崟顓犵厜闂佸搫鐬奸崰鏍х暦椤愶箑绀嬫い鎺戭槹椤ワ絽鈹戦悙鑼憼缂侇喖绉瑰畷鏇㈠箮鐟欙絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾稑鈽夊鍡楁閻庤娲︽禍顏勵潖濞差亝鍋￠柡澶嬪椤斺偓闂備胶鎳撻崵鏍箯閿燂拷
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
    //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆撴倻濡桨鐢绘繝鐢靛Т閿曘倝宕板顑╃喖鍩€椤掑嫭鍊垫繛鍫濈仢閺嬬喖鏌熼鐓庘偓鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸閻忕偟纭堕崑鎾崇暦閸ャ劍鐣烽梻渚€鈧偛鑻晶瀵糕偓瑙勬磻閸楀啿顕ｆ禒瀣垫晣闁绘劗鏁搁弳顐︽煟閻斿摜鐭嬮柡灞诲姂閹箖鏌ㄧ€ｎ剟妾梺鍛婄☉閿曘倖绂嶅⿰鍫熲拺闁告稑锕︾粻鎾绘倵濮樼厧澧柍缁樻崌婵＄兘鍩￠崒妤佸闂備胶枪閺堫剟鎮疯钘濋柨鏇炲€归悡娆撴偣閸ュ洤鎳愰惁鍫ユ倵鐟欏嫭绀€婵犫偓闁秴绠熼柟缁㈠枛缁€瀣亜閹哄棗浜鹃悗娈垮枛濞尖€愁潖濞差亜鎹舵い鎾墲椤庢姊洪崫銉バｉ柣妤冨█楠炲啴鍨鹃幇浣瑰缓闂侀€炲苯澧寸€殿喖顭烽幃銏ゅ川婵犲嫮肖濠德板€х徊浠嬪疮椤栫儐鏁佺€广儱顦伴埛鎴犵磼鐎ｎ偒鍎ラ柛搴㈠姍閺屾盯寮埀顒勬偡閳哄懐宓侀柛鎰╁壆閺冨牆宸濇い鎾跺枎缁佽埖淇婇悙顏勨偓鏍箰妤ｅ啫纾规い鎰剁畱椤懘骞掗搹顐ゎ洸婵犲﹤鐗婇悡蹇擃熆閼哥數鈽夋い鈺婂墰缁辨帡鎮╅幇浣哄姺缂備浇椴哥敮妤€顭囪箛娑樜╅柨鏇楀亾闂傚绉瑰娲偡閺夋寧些闂佺娅曢敋闁伙絿鍏橀、鏇㈡晝閳ь剛绮堢€ｎ剛妫柣妤€鐓鍛洸闁告挆鈧崑鎾诲礂婢跺﹣澹曢梻浣告啞濞诧箓宕滃☉銏犲偍闂侇剙绉甸埛鎴︽⒒閸喕鍎愮憸鐗堝笒绾惧潡鏌熼幆鐗堫棄闁哄嫨鍎甸弻銊モ攽閸℃顦遍梺绋款儐閹告悂鍩㈤幘璇插瀭妞ゆ梻鏅禍顏呬繆閻愵亜鈧倝宕㈡ィ鍐ㄧ婵☆垯璀﹂崵鏇熴亜閹板墎鐣辩紒鐘崇⊕閵囧嫰骞樼捄杞版埛缂備焦鍔楅崑鐐垫崲濠靛鍋ㄩ梻鍫熷垁閵忋倖鍊垫慨妯哄船閸樺鈧娲忛崕闈涚暦閵娧€鍋撳☉娅辨岸骞忕紒妯肩閺夊牆澧介幃濂告煙閾忣偅宕屾い銏¤壘楗即宕ㄩ婊愮床闂佽崵濮村ú锕併亹閸愵喖鐒垫い鎺嗗亾婵炵》绻濋幃浼搭敊閽樺绐炴繝鐢靛仧閸嬫捇锝炲澶嬧拺闂傚牊绋撶粻鍐测攽椤旀儳鍘存い銏＄懇楠炴帡寮崒婊愮闯闂備胶枪閺堫剟鎮疯瀹曟繂顓奸崱鎰盎濡炪倖鎸鹃崑鐐哄窗濡皷鍋撶憴鍕８闁搞劋绮欓悰顕€寮介妸锕€顎撻梺绋跨箰椤︽壆鈧艾銈稿缁樻媴閸涘﹤鏆堢紓浣割儐閸ㄥ潡寮崘顔芥櫜闁搞儺鐓堥崑銊╂⒒娓氬洤澧紒澶屾暬閹繝寮撮姀锛勫幗濠碘槅鍨甸褎鏅堕娑氱闁割偆鍠撻惌宀勬煃瑜滈崜婵嬶綖婢跺⊕鍝勵潨閳ь剙鐣峰┑鍡忔瀻闁规崘娉涚粊锕傛⒑閹肩偛鍔撮柛鎾村哺瀹曟垹鈧綆鍠楅悡鏇熴亜閹邦喖孝闁诲繆鏅滈〃銉╂倷閺夋垵顫掗梺鍝勭焿缁查箖骞嗛弮鍫濈伋闁告劘灏欢鐔兼⒒娴ｅ懙褰掝敄閸℃稑绠查柛銉墮缁犳牗淇婇妶鍛櫤闁哄懏鐓￠獮鏍垝閸忓浜鹃柟棰佺劍妤犲嫰姊婚崒娆愵樂缂侀硸鍠氱划濠氬箣閿旇棄浜遍梺瑙勫礃椤曆呯不閺嶃劋绻嗛柕鍫濇噹閺嗙偤鏌涢悩鍙夘棦闁哄本鐩鎾Ω閵夈儺娼炬俊鐐€х拹鐔煎礉閹达箑钃熼柨婵嗩槸椤懘鏌嶆潪鎷屽厡濞寸媭鍙冮弻锝夊閳轰胶浠梺鍝ュУ閻楃娀鐛崘顭戞建闁逞屽墴閻涱噣骞掗幊铏⒐閹峰懘宕崟顐ゎ啈闂傚倸鍊烽懗鍓佸垝椤栫偛绀夐柡鍥ュ焺閺佸銇勯幘鍗炵仼闁绘挻绻堥弻鐔煎礈瑜忕敮娑㈡煟閹惧啿鏆熼柟鑼归オ浼村醇濠靛牜妲舵繝鐢靛仜濡瑩骞愰幖浣哥＜闁靛ň鏅滈悡娑氣偓骞垮劚閸燁偅淇婇崸妤佺厽婵犻潧妫涢崺锝夋煛瀹€瀣瘈鐎规洖鐖兼俊鐑藉Ψ瑜岄惀顏堟⒒娴ｇ懓鈻曢柡鈧柆宥呭瀭闁秆勵殔閽冪喐绻涢幋鐐冩岸寮ㄦ禒瀣厱闁斥晛鍠氬▓銏犆圭亸鏍т壕闂傚倸鍊风粈渚€鎮块崶顒夋晪鐟滄棁妫熼梺姹囧灮鏋柦鍐枑缁绘盯骞嬮悙鑼姲闂佺ǹ顑嗛幑鍥极瀹ュ纭€闁绘劕寮跺▓銊︾節閵忥綆娼愭繛鑼枎椤繑绻濆顒傦紲濠电偛妫欑敮鎺楀储閿熺姵鐓涢悗锝呭悁缁ㄥジ鏌曢崶褍顏鐐差儏閳规垿宕堕懜纰夌礂濠德板€楁慨鐑藉磻閻愬搫绀夋繛鍡樻尵瀹撲線鏌″鍐ㄥ闁荤喓鎳撻埞鎴︽偐閹绘帩浼€闂佸磭顑曢崐鏍崲濠靛鍋ㄩ梻鍫熺▓閺嬪懎鈹戦悙鏉垮皟闁搞儴鍩栭弲鐐烘⒑閸︻厼顣兼繝銏★耿瀵憡绗熼埀顒勫蓟濞戙垹鐒洪柛鎰亾閳诲秹鎮峰⿰鍛暭閻㈩垱顨婂鏌ュ蓟閵夛妇鍘遍梺闈涱檧缁茶姤淇婃禒瀣厽婵犲灚鍔掔花缁樸亜椤忓嫬鏆ｅ┑鈥崇埣瀹曟﹢濡歌閹綁姊绘担渚敯婵炲懏娲熼弫瀣渻閵堝骸浜滅紒缁樺笧濡叉劙骞掗幊宕囧枛閹虫牠鍩￠崒姘杽闂傚倸鍊风欢姘跺焵椤掑倸浠滈柤娲诲灡閺呭爼骞橀鐣屽幈闂佸疇顫夐崕铏閻愵兛绻嗛柣鎰典簻閳ь剚鐗滈弫顕€骞掑Δ鈧壕褰掓煕濞戞﹫鍔熸い鈺呮敱缁绘繃绻濋崒婊冾杸闂佺粯鎸炬慨鐢垫崲濞戙垺鍤戝Λ鐗堢箓濞堫參姊虹拠鏌ョ崪缂佺粯绻堝濠氭晸閻樻彃绐涘銈嗘尵婵挳鎮￠悢濂夋富闁靛牆鍟悘顏堟煟閻斿弶娅婃鐐插暣楠炲棜顦撮柡鍡楁閺屽秷顧侀柛鎾跺枛楠炲棙绗熼埀顒€鐣锋總绋课ㄩ柨鏃囶潐鐎氬ジ姊婚崒娆戣窗闁稿妫濆畷鎴濃槈閵忊€虫濡炪倖鐗楃粙鎺戔枍閻樼粯鐓欑紓浣靛灩閺嬬喖鏌ｉ幘瀵告创闁诡喗锕㈤幃娆戞崉閻╂帗鎸婚妵鍕Ψ閿斿墽鐓侀梺闈涙搐鐎氫即鐛鈧幃娆撴寠婢跺鍨濋梺璇叉唉椤煤韫囨稑绀夐柟杈捐礋閳ь兛绀侀埥澶愬閻樻爠鍥ㄧ厱闁靛鍨靛Λ宀勫磻閹捐閿ゆ俊銈勮閹峰姊虹粙鎸庢拱闁煎綊绠栭崺鈧い鎺戝濡垶绻涢崱鎰仼妞ゎ偅绻堥、妤佸緞鐏炶棄楔闂佽崵鍠愮划宥呂涢崘顔惧祦闁告劦鍙庡Ο鍕倵鐟欏嫭绀€鐎规洦鍓熷﹢渚€姊洪崫鍕垫Ч閻庣瑳鍥х獥婵☆垱鐪规禍婊堟煏韫囥儳纾挎繛鍙夋尦閺岀喖顢欓悾灞惧櫚濡ょ姷鍋為幑鍥嵁閹烘绠ｉ柣鎴濇閹绻濋悽闈浶ラ柡浣规倐瀹曟垿鎮欓崫鍕唶婵犵數濮甸懝楣冨及閵夆晜鐓曢柟浼存涧閺嬫稓鐥幆褜鐓奸柡宀€鍠栧畷婊嗩槾閻㈩垱鐩弻锝夊箻鐎垫悶鈧帡鏌嶈閸撴瑩宕㈠⿰鍫濈；闁瑰墽绮悡鍐喐濠婂牆绀堥柣鏃堫棑閺嗭箑霉閸忓吋缍戦柛鎰ㄥ亾婵＄偑鍊栭幐鐐叏鐎靛摜鐭堥柨鏇炲€归埛鎴犵棯椤撶偞鍣虹憸鎶婂懐纾奸柣妯哄暱閻忊晝绱掗娆惧殭闁宠棄顦埢搴☆吋閸曨厾娉块梻鍌欑濠€閬嶅磿閵堝鍨傞柣銏⑶归悞鍨亜閹哄棗浜鹃梺纭呭Г缁挸顕ｉ锔绘晪闁逞屽墮閻ｉ绮欑拠鐐⒐閹峰懘宕ｆ径濠庝紲濠电姷鏁搁崑鐘诲箵椤忓棗绶ゅù鐘差儏缁犵姵绻涢崱妯虹濞存粌缍婇弻鐔兼倻濡偐鐣洪梺鍝勬噽閸嬨倕顫忛搹鐟板闁哄洨鍋涢埛澶岀磼閻愵剚绶茬紒澶婄秺閻涱喗绻濋崟顏呭媰闁荤喐鐟辩徊楣冨棘閳ь剟姊绘担铏瑰笡闁搞劑娼х叅闁靛ň鏅涚粈鍡涙煟濡も偓閻楀繒绮绘ィ鍐╁€堕柣鎰絻閳ь剚鎮傞崺鈧い鎺嶈兌缁犵偟鈧鍣崑濠傜暦濮椻偓椤㈡岸宕ㄩ鑺ョ彆闂傚倷绀侀幉鈩冪瑹濡ゅ懎绀堟繛鎴炴皑娑撳秵鎱ㄥΟ鍨厫闁抽攱鍨块弻娑㈠箻閺夋垹绁锋繛瀛樼矋缁诲啰鎹㈠☉銏犵煑濠㈣泛鑻埅鐟邦渻閵堝簼绨婚柛鐔告尦楠炴劖绻濋崘顏嗗骄濠电娀娼уΛ娑氱矓濞差亝鐓涘ù锝囨嚀婵秶鈧娲滈弫璇茬暦閹扮増鍋嗗ù锝呮贡缁夐绱撻崒娆戝妽婵﹤顭峰畷浼村箻鐠囪尙鍔﹀銈嗗笒閿曪妇绮旈悽鍛婄厱闁规儳顕妴鎺戭熆鐟欏嫭绀冪紒鍌涘笧閳ь剨缍嗛崑鍡涘储闁秵鐓熼煫鍥ㄦ礀娴犳粌顭胯缁瑩鐛繝鍥у窛妞ゆ棁妗ㄧ花濠氭⒑閹稿孩鐓ラ柕鍫⑶归‖濠囶敋閳ь剟寮婚弴銏犵倞鐟滃秹顢旈銏＄厸濞达絿枪閳ь剙顭烽獮蹇涙偐缂佹ê娈ゅ銈嗗灦鐎笛囥€佸鈧濠氬磼濞嗘劗銈板銈嗘礃閻楃姴鐣锋导鏉戠閻犲洩灏欓敍鐔兼⒑閼测斁鎷￠柛鎾寸〒濡叉劙鏁冮崒娑氬幗闂佹寧绻傞幊鎰版倶闁秵鐓熸い鎾跺枔閹冲洭鏌＄仦璇测偓婵嬬嵁閸ャ劍濯撮柛婵勫劙閻㈠姊绘担渚敯鐎殿喗鎹囧畷锟犲礃瀹割喗缍庣紓鍌欑劍钃卞┑顖涙尦閺屟嗙疀閺囩喎娈岀紓浣广仜閳ь剚鏋奸弨浠嬫煃閽樺顥滃ù婊勭矒閺屾盯鎮㈡笟顖涱棑濠碘槅鍋侀崹钘夘潖閾忚鍠嗛柛鏇ㄥ亞椤︺劌顪冮妶鍡樿偁闁告劕鍞查崶褑鎽曢梺闈涱檧缁犳垿鍩€椤掑倹鏆柡灞诲妼閳规垿宕卞☉鎵佸亾濡ゅ懏鐓涢悗锝庡墮閺嬫盯鏌″畝瀣М妤犵偞顭囬埀顒傛暩绾泛危閸繍娓婚柕鍫濋瀵劍绻涙径瀣妤犵偞鍨垮畷鐔碱敆閸屾粍鍤屾俊鐐€栭悧妤呮嚌閻愵剛鐝堕柛顐ゅ枍缁诲棝鏌ｉ幇鍏哥盎闁逞屽墯閻楁洜鍙呭┑鈽嗗灣閸樠囧垂濠靛牃鍋撻獮鍨姎婵炲眰鍔戦幆灞惧緞鐏炵ǹ浜炬鐐茬仢閸旀岸鏌熼崘宸Ц閸楅亶鏌涢鐘插姕闁绘挸绻橀悡顐﹀炊閵夈儱濮㈢紒鐐劤濞硷繝寮诲☉妯滅喖宕烽鐘靛幆婵犳鍠栭敃銊モ枍閿濆洦顫曢柟鐑樺殾閻斿吋鍤掗柕鍫濇噹濮规煡姊婚崒娆掑厡妞ゃ垹锕ラ幈銊╁Χ閸パ冪亰闂佽宕橀褏绮诲鎵佸亾閸忓浜鹃梺閫炲苯澧撮柛鈹惧亾濡炪倖甯掗敃锔剧矓闂堟耽鐟扳堪閸曨厼鈷岄悗娈垮枔閸斿秶绮嬮幒鏂哄亾閿濆骸浜為柛妯绘崌濮婅櫣鈧湱濮甸妴鍐磼閳ь剚绗熼埀顒€顕ｉ幎鑺ュ€烽柣鎴烆焽閸樿鲸绻濋悽闈浶㈤柛鐕佸亝閹便劍寰勯幇顓犲帗閻熸粍绮撳畷婊冣攽鐎ｅ墎绋忔繝銏ｆ硾閳洖煤椤忓嫬鍞ㄥ銈嗘尵閸犲孩绂嶉鐐粹拺缂佸娉曠粻缁樹繆椤愩儲纭堕柟骞垮灲瀹曞崬螣鐠囨彃浼庢繝鐢靛█濞佳兾涘☉銏″€块柣鎰靛厸缁诲棝鏌熺紒妯轰刊闁绘挸銈搁弻鐔碱敊缁涘鐣堕梺宕囩帛閹瑰洤鐣疯ぐ鎺濇晩闁伙絽濂旈幉鐐節閻㈤潧啸闁轰焦鎮傚畷鎴濃槈閵忊剝娅囬梺闈涚墕椤р偓缂傚秵鐗犻悡顐﹀炊閵婏妇顦紒鐐劤閸氬绌辨繝鍥舵晬婵犲灚鍔曞▓顓烆渻閵堝懎顒㈤柟鐟版搐椤繐煤椤忓嫮鐣鹃悷婊冪箳缁骞庨懞銉у幍闂佹儳娴氶崑鍛焊閻㈢數纾兼い鏇炴噹瀵噣鏌涢妸鈺冪暫鐎殿噮鍓熸俊鐑芥晜閼恒儲绶繝纰夌磿閸嬫垿宕愰弽褜娼栧┑鐘宠壘杩濋柣搴秵閸犳牜绮堟径鎰厽闁哄倸鐏濋幃鎴︽煟閹惧鎳囬柡灞熷棛鐤€闁挎繂鎳嶇花濂告倵鐟欏嫭绀€闁活厼鍊垮璇差吋婢跺á銊╂煥閺冨倻鎽傛俊顐㈠暣閹鎲撮崟顒傤槰闂佹寧娲忛崹浠嬪Υ娴ｇ硶鏋庨柟鐐綑娴滄鏌熼懝鐗堝涧缂佸弶妞藉畷鎴﹀箻濞茬粯鏅ｉ梺缁樺灥濡瑧鈧潧鐭傚娲濞戞艾顣哄┑鈽嗗亝閻熝呭垝閸儱鐒垫い鎺戝閳锋垹鐥鐐村櫤闁绘繍浜弻娑㈠籍閳ь剟鎮烽妷鈺傚仼鐎瑰嫭澹嬮弨浠嬫倵閿濆骸浜滈柍褜鍓欓…鐑藉蓟閵堝绠掗柟鐑樺灥婵姊鸿ぐ鎺撴暠婵＄偠妫勯～蹇撁洪鍕祶濡炪倖鎸撮崑鎾绘煟閻旂ǹ娅炵憸鐗堝笚閸婂鏌﹀Ο渚Ш闁告瑢鍋撴繝鐢靛О閸ㄥジ宕洪弽顓炵闁哄稁鍘肩壕褰掓煕椤垵浜濋柛娆忕箲娣囧﹪顢涘⿰鍐ㄤ粯闁汇埄鍨伴悥濂稿蓟閿涘嫧鍋撻敐搴′簽婵炲弶鎸抽弻鐔兼寠婢跺苯鐓熼梺璇″枟閻熴儴褰佸銈嗗灱濞夋洟鎯勬惔銊︹拻濞达絽鎲￠幆鍫㈢磼鐎ｂ晝绐旂€规洘鍨垮畷鐔碱敆閸屻倖绁梻浣瑰濞叉牠宕愯ぐ鎺撳亗闁绘柨鍚嬮悡娆撴煙椤栧棗鑻▓鍫曟⒑鐎圭姵顥夋い锔垮嵆婵＄敻宕熼锝嗘櫍闂佹枼鏅涢崯浼村Υ婵犲洦鈷戦柛锔诲幗閸も偓闁诲孩鍑归崢鍓у垝椤撱垺鍋勭痪鎷岄哺閺咁剙鈹戦鏂や緵闁告挻鐟╄棢婵犻潧顑嗛埛鎺楁煕鐏炲墽鎳嗛柛蹇撶焸瀵悂顢旈崱娆戯紲闂侀潧饪甸梽鍕Φ濠靛牃鍋撶憴鍕婵炶尙鍠庨悾鐑芥晲閸℃绐為柣蹇曞仜閹诧繝寮棃娑掓斀闁绘ê鐏氶弳鈺佲攽椤旇姤缍戦悡銈夋煏閸繃宸濆☉鎾崇Ч閺岋綁濮€閻樺啿鏆堥梺缁樻尰閻熲晠寮婚敐澶婄闁绘劕妫欓崹鍧楀春濞戙垹绠抽柟瀹犳珪濡啫顕ｉ鈧畷濂告偄閸欏顏烘繝鐢靛仩閹活亞寰婇崸妤佸仱闁哄啫鐗嗛崥褰掓煕閹伴潧鏋熼柣鎾存礋閺岋絽螣鐠囨彃顫┑鐐茬墛缁捇寮婚弴銏犲耿婵°倐鍋撻柡鍡悼閳ь剝顫夊ú蹇涘礉瀹ュ拋鐒介煫鍥ㄧ☉缁€鍫㈡喐韫囨稑鐓熼柕鍫濇缁♀偓闂佹眹鍨藉褔鍩㈤崼鐔虹濞达絽鍟垮ù鍌炲极閸曨垱鐓欓柣妤€鐗婄欢鏌ユ煛鐎ｂ晝绐旈柡灞剧洴閸╁嫰宕橀浣割潓闂備胶绮幐璇裁洪悢鐓庣畺婵せ鍋撻柟顔界懇楠炴捇骞掗崱妯虹槺闂傚倷娴囧銊╂倿閿曞倹鍋嬮柣妯款嚙閽冪喖鏌￠崶鈺佹灁缂佺娀绠栭弻锝夊箛闂堟稑顫梺缁樼箖濞茬喎顫忓ú顏勭闁兼亽鍎查弳鐘绘⒑閹肩偛濡兼繛灏栤偓鎰佸殨閻犲洦绁村Σ鍫熶繆椤栫偞鏁遍柡鍌楀亾闂傚倷鑳剁涵鍫曞礈濠靛鈧啳绠涢弬娆惧殼闂佺ǹ鏈崙鐟般€掓繝姘厪闁割偅绻堥妤€霉濠婂啫鈷旂紒杈ㄥ浮閹晠鎼归銏紦闁诲孩顔栭崰娑㈩敋瑜旈崺銉﹀緞閹邦剦娼婇梺缁橆焽閺佺ǹ顭囨径鎰拻濞撴埃鍋撻柍褜鍓涢崑娑㈡嚐椤栨稒娅犻悗鐢电《閸嬫挾鎲撮崟顒傤槬缂傚倸绉撮敃銈夋偩妞嬪海鐭欓悹鍥╁亾濡啫鐣峰鈧俊姝岊槹闁稿锕濠氬磼濞嗘埈妲梺纭咁嚋缁绘繂鐣峰┑瀣櫇闁稿本姘ㄩ悰銉╂⒑閸濆嫮鈻夐柛娆忕箳缁辩偤寮介鐔哄幈濠电偞鍨堕…鍥箺閻樼粯鐓熼柟鎯х－缁犲鏌＄仦鍓ф创妞ゃ垺娲熼弫鎰板炊閳哄啯姣囬梻鍌欑窔濞艰崵寰婇挊澶涜€块弶鍫氭櫆椤洟鏌熼悜姗嗘當缂佺姵绋掗妵鍕箻鐠虹洅娑樷攽椤斿吋宸濈紒杈ㄦ尰閹峰懘鎼归悷鎵偧缂傚倷娴囬褔鎮ч幘鑽ゅ祦闁告劏鏅濋々鐑芥倵閿濆骸浜芥俊顐㈠暙閳规垿鎮欏▓鎸庮棑缂傚倸鐗呴崡鍐茬暦鐟欏嫮顩烽悗锝庝簽閻ｅ搫鈹戞幊閸婃劙宕戦幘娣簻妞ゆ劧绲跨粻鐐存叏婵犲懎鍚归柍褜鍓熷褔骞夐敓鐘茬柧闁稿繗鍋愮弧鈧紒鍓у鑿ら柛瀣崌瀹曟﹢鎳犻鍕礈闂傚倷绀侀幉鈥愁潖瑜版帗鍋￠柕澶嗘櫓閺佸鏌ㄥ┑鍡╂Ц缂佺姷绮妵鍕籍閸屾瀚涘┑鐐靛帶閿曨亜顫忓ú顏勭闁兼亽鍎插▓浼存⒑缁嬫鍎愰柟鐟版喘瀵鎮㈤崜鍙壭ч柟鑹版彧缁茬晫绮敍鍕＝濞达綀娅ｇ敮娑氱磼鐎ｎ偄绗掓い顓炴穿缁犳稑鈽夊Ο纰辨Ф闁荤喐绮岀粔鐟扮暦閸濆嫮鏆嗛柛鏇ㄥ厴閹疯櫣绱掔紒銏犲箹闁瑰啿姘﹂。璺ㄧ磽閸屾瑧璐伴柛鐘崇墪闇夐柣鎴ｆ缁€鍡涙煙閻戞﹩娈旂紒顐㈢Ч閺屾盯顢曢妶鍛€婚梺閫炲苯澧扮紒顕呭灡缁岃鲸绻濋崶鑸垫櫖闂佺粯鍔曢悺銊╂偟瀹曞洨纾藉ù锝夋涧婵″吋銇勯鐐叉Щ妞ゎ偄绻愮叅妞ゅ繐鎷嬪Λ鍐ㄢ攽閻愭潙鐏卞瀛樻倐椤㈡捇宕卞☉娆屾嫽闂佺ǹ鏈懝楣冾敂閳哄懏鍊垫慨妯煎帶婢ф挳鏌熼钘夊姢闁伙綇绻濋獮宥夘敊閼恒儳鏆﹂梻鍌欑窔閳ь剛鍋涢懟顖涙櫠閹绢喗鐓曢柍瑙勫劤娴滅偓淇婇悙顏勨偓鏍暜閹烘绐楁慨姗嗗墻閻掍粙鏌熼柇锕€骞樼紒鐘荤畺閺屾稑鈻庤箛锝嗩€嗛梺鍏兼緲濞硷繝寮婚弴銏犲耿婵°倐鍋撻柡鍡悼閳ь剝顫夊ú蹇涘礉瀹ュ拋鐒介煫鍥ㄧ☉缁€鍫㈡喐韫囨稑鐓熼柕鍫濇缁♀偓闂佹眹鍨藉褍鐡梻浣瑰濞插繘宕愬┑瀣畺鐟滄柨鐣烽崡鐐╂婵炲棗绻嗛崑鎾诲锤濡や胶鍘卞銈庡幗閸ㄥ灚绂嶉弽顬＄懓饪伴崟顓犵厜闂佸搫鐬奸崰鏍х暦椤愶箑绀嬫い鎺戭槹椤ワ絽鈹戦悙鑼憼缂侇喖绉瑰畷鏇㈠箮鐟欙絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯鍩勯崘鍓у姺闂佸疇顫夌粙鎾舵閹捐纾兼慨妯荤樂閿涘嫮纾奸柤鎼佹涧閸濆搫鈹戦垾宕囨憼缂佹鍠栭崺鈧い鎺嗗亾妞ゆ洩缍侀獮姗€顢欑喊杈ㄧ秱闂備焦鏋奸弲娑㈠疮椤栫偞鍋熼柡宥庡幗閳锋帒銆掑锝呬壕闂侀€炲苯澧伴柛瀣洴閹崇喖顢涘☉娆愮彿闂佽顔栭崰姘卞閸忕浜滈柡鍌氱仢閳锋棃鎮介娑氥€掔紒杈ㄥ笧缁辨帒螣閼测晝鏆ゆ俊鐐€ら崑鍛崲閸儯鈧礁顫滈埀顒勫箖濞嗘挸鐓涘ù锝呭槻椤ユ岸姊绘担鍦菇闁搞劌缍婇獮澶愭晸閻樿尙鍘遍梺鍦劋椤ㄥ棝鎮￠弴鐔翠簻闁规壋鏅涢埀顒佹礋瀵悂寮崼鐔哄帗濡炪倖鐗楃粙鎺旂矆鐎ｎ喗鐓涚€光偓閳ь剟宕伴弽褏鏆﹂柨婵嗘缁剁偤鎮楅敐搴濈按闁稿鎹囬獮鍥偋閸垹骞愰柣搴″帨閸嬫捇鏌嶈閸撶喎鐣锋导鏉戝唨妞ゆ挾鍋熼鎰版偡濠婂懎顣奸悽顖楁櫊瀵偊宕橀鐣屽帾闂佸壊鍋呯换鍐啅濠靛鐓涘璺洪琚氱紓浣虹帛缁诲倿鍩㈤幘璇插瀭妞ゆ梻鏅禍顏堟⒒娴ｇ懓顕滄繛鎻掔箻瀹曟洟鏌嗗浣插亾閺冨牆绀冩い鏃囨閻у嫭绻濋姀鐘插辅闁哄懏绮撻幆鍕敍濮樿鲸娈惧┑顔姐仜閸嬫挸鈹戦埄鍐╁€愬┑顔瑰亾闂佸疇妫勫Λ娆戠礊瀹€鍕拻濞达絽鎲￠幆鍫熴亜閿旇鐏﹂柟顔ㄥ洤绀嬫い鏍ㄦ皑椤︻參姊绘笟鍥у缂佸娼欏ú鍨攽閻橆喖鐏辨繛澶嬬洴椤㈡牠宕掗悙鑼唵闂佹儳绻愬﹢杈╁閽樺褰掓晲閸涱収妫岄梺绋块缁绘垹鎹㈠☉姘厹濡炲娴烽惁鍫濐渻閵堝啫鐏柟鍛婄摃閻忓啴姊洪崨濠傚Ё缂佽尪濮ょ粋宥嗐偅閸愨晛浠┑鐐叉缁绘劙顢旈鍕电唵閻犲搫鎼顓㈡煛鐏炲墽娲存鐐搭焽閹瑰嫰鎮滃Ο闂撮偗闂佽姘﹂～澶娒哄Ο渚富闁兼亽鍎查崣蹇涙煃瑜滈崜鐔煎蓟閺囥垹閱囨繝闈涙搐濞呇冾渻閵堝棙鈷愰悽顖ょ節瀵鏁愭径瀣汗闂佸憡鐟ラˇ閬嶅煘濞戙垺鐓熼煫鍥ㄦ尵缁犳煡鏌ｉ悢鍙夋珔闁伙絿鍏樻慨鈧柕鍫濇噽椤旀劖绻涙潏鍓у埌闁告ɑ绮撻獮蹇撁洪鍛嫼闂佸憡绋戦敃锕傚煡婢舵劖鐓ラ柡鍥朵簽閹吋绻涢崱鎰伈闁哄苯妫楅濂稿幢濞嗗繐绠為梻鍌欒兌缁垶鏁嬮梺鍝ュ枎濞尖€崇暦椤愶附鎯為柛蹇擃槸娴滈箖鎮峰▎蹇擃仾缁剧偓鎮傞弻娑㈠Ω閵婏富妫勯梺浼欑悼閺屽濡甸幇鏉跨婵犻潧娲﹀▍鍥⒒娴ｇ懓顕滅紒瀣灩閳ь剚鍑归崳锝夊箖閻愵兙鍋呴柛鎰ㄦ杹閹风粯绻涙潏鍓хК婵炲拑绲块弫顔尖槈閵忥紕鍘遍梺鍝勫暊閸嬫挻绻涢崣澶屽⒊闁诲繑甯楃换婵嬫偨闂堟刀銏＄箾鐏炲倸鈧绔熼弴銏犻敜婵°倕鍟ぐ鍕⒑閹肩偛鍔橀柛鏂跨Ф閳ь剚鑹鹃…鐑藉蓟閿熺姴绀冮柕濞у喚鏉搁梻浣呵归鍡涘礉濞嗘挸钃熺憸鎴犵不濞戙垺鏅查柛娑卞墰閻╁酣姊绘笟鈧埀顒傚仜閼活垱鏅堕鐐寸厱闁哄啠鍋撴い銊ワ躬瀹曟椽鍩€椤掍降浜滈柟鐑樺灥閺嬨倖绻涢崗鐓庡缂佺粯鐩獮鎾诲箳閺冨偆鍟嬮柣搴ゎ潐濞叉繈锝炴径宀€鐭夐柟鐑樻煛閸嬫捇鏁愭惔鈥愁潻缂備椒绶ょ粻鎴﹀煘閹达富鏁婂┑顔藉姃缁爼姊虹粙娆惧剱闁圭ǹ顭锋俊鐢稿礋椤栨凹娼婇梺鎸庣☉鐎氼參宕虫导瀛樷拺閻犲洠鈧櫕鐏堟繝鈷€鍡椥撶紒鍌涘浮閺佸啴宕掑☉妯兼濠电姰鍨煎▔娑㈡儗閸儱鑸归柤鍝ユ暩缁♀偓闂佹眹鍨藉褍鏆╅梻浣芥〃閻掞箓骞冮崒姘辨殾闁硅揪闄勯崑鎰版偣閸ヮ亜鐨烘い鏂胯嫰椤啴濡堕崨顖滎唶濠碘槅鍋呯粙鎾跺垝閸喓绡€闁搞儯鍔庨崢鎾绘偡濠婂嫮鐭掔€规洘绮岄～婵堟崉娴ｆ洩绠撻弻娑㈠即閵娿儳浼囬梺杞扮閸熸挳寮婚弴锛勭杸濠电姴鍟悵鏇犵磼濡や礁鐏存慨濠勭帛閹峰懏顦版惔婵婎洬缂傚倷娴囧鎾跺垝濞嗗繒鏆﹂柡灞诲劚閽冪喖鏌曟径娑氱暠濞寸姷鍘ч埞鎴︽偐缂佹ɑ閿銈嗗灥閹虫﹢骞冮悙鍝勫瀭妞ゆ劗濮崇花濠氭⒑閸︻厼鍔嬮柛銊ф暬椤㈡棃鍩￠崨顔惧幗闂佸湱鍎ら崹瑙勭濞戙垺鐓忛柛銉戝喚浼冨Δ鐘靛仦鐢帡顢樻總绋跨倞妞ゅ繐妫涘畷鍫曟⒒閸屾瑧顦﹂柟鑺ョ矒瀹曠増鎯旈敐鍡楀簥濠殿喗銇涢崑鎾垛偓娈垮枦椤曆囧煡婢舵劕顫呴柍鈺佸暞閻濇娊姊虹涵鍛汗閻炴稏鍎靛畷婊冣攽鐎ｎ偄浠у┑鐘绘涧椤戝棝宕愰悽鍛婂仭婵炲棗绻愰顏嗙磼閳ь剟宕橀鍡欙紲濡炪倖妫佹慨銈呯暦鐏炵虎娈介柣鎰絻閺嗭絽鈹戦鐟颁壕闂備線娼ч悧鍡椢涘畝鍕鐟滅増甯楅埛鎴犳喐閻楀牆绗掑ù婊€鍗抽弻娑㈡偐閸愭彃鎽甸梺璇″枤閸嬫捇濡堕敐澶婄闁冲湱鍋撶€氳棄鈹戦悙鑸靛涧缂佸弶瀵ч悘娆忣渻閵堝啫鍔氱紒缁橈耿瀵鈽夊锝呬壕闁挎繂绨肩花濂告煕閿濆懐绉洪柟顕嗙節婵¤埖寰勭€ｎ剙骞愰柣搴＄畭閸庤鲸顨ラ幖浣哄祦婵☆垰鐨烽崑鎾舵喆閸曨剙鐭紓浣藉煐瀹€绋款嚕婵犳艾鍗抽柨娑樺閺夊憡绻濋悽闈涗沪闁稿氦娅曠粋宥嗐偅閸愨晝鍘剧紒鐐緲瀹曨剚鏅舵导瀛樼厽闁挎繂顦遍悾鐑樻叏婵犲嫮甯涚紒妤冨枛閸┾偓妞ゆ巻鍋撴い顓炴穿椤﹀綊鏌熼銊ユ搐楠炪垺绻涢幋鐑嗙劷妞ゆ柨妫濆娲偡閹殿喗鎲肩紓浣筋嚙缁夋挳鈥﹂崶顒€鍐€闁靛ǹ鍊楃粻姘舵⒑闂堟稓澧曢柟鍐查叄瀵娊鎮㈤搹瑙勶紡闂佽鍨庢担闀愬垝闂備礁鎼惉濂稿窗閺嶎厾宓侀柟鎹愵嚙缁犺櫕绻濋棃娑欘棞濠㈢懓顑夊缁樻媴妞嬪簼瑕嗙紓浣藉紦缁瑩骞冨Ο渚僵閻犻缚娅ｉ悿鍕⒑闂堟稈搴峰┑鈥虫川瀵囧焵椤掑嫭鈷戠紒瀣劵椤箓鏌涙繝鍐炬疁鐎规洏鍎抽埀顒婄秵閸犳宕愰悽鍛婂仭婵炲棗绻愰顏嗙磼閳ь剟宕橀钘変缓濡炪倖鐗楃粙鎴澝归鈧弻娑㈠煘閸喚浠鹃梺璇″灡濡啯淇婇幖浣肝ㄧ憸蹇涘级閸涘﹣绻嗛柣鎰典簻閳ь剚鐗犲濠氬Ω閳轰胶顔愬銈嗗姧缁插潡銆呴悜鑺ョ厵闁绘垶锚閻忓秹鏌￠埀顒佺鐎ｎ偆鍘藉┑鈽嗗灥濞咃絾绂掑☉銏＄厸闁糕€崇箲濞呭﹪鏌＄仦鍓с€掗柍褜鍓ㄧ紞鍡涘磻閸℃娲箻椤旇棄鐧勫┑鐘绘涧椤戝棝鎮″☉姗嗙唵閻犺桨璀﹂崕婊呯磼閹典焦娅嗙紒缁樼☉椤斿繘顢欓懡銈呭毈闂備胶枪椤戝懐鈧凹鍓熼垾鏃堝礃椤忎礁浜鹃柨婵嗙凹缁ㄧ粯銇勯幒瀣仾缂佺粯鐩畷濂稿閻樺崬顥氱紓鍌欒兌缁垶鎯勯姘煎殨闁割偅娲栫粻锝嗙節闂堟稒鎼愰柍褜鍓﹂崹浼村煘閹达附鍊烽柟缁樺笚閸婎垶姊洪懡銈呮殌闁告侗鍘奸悘濠囨煟閻樺弶澶勭紒浣规綑鍗遍柛顐犲劜閻撳繘鐓崶銉ュ姢缁炬儳娼￠弻娑樜熼悜妯烘殘缂備胶绮换鍫熸叏閳ь剟鏌ㄥ┑鍡橆棤闁靛棙鍔欏娲箰鎼淬垻鈹涢梻鍌氬鐎氼喚鍒掔€ｎ喖绠虫俊銈傚亾妞ゎ偄鎳橀弻鏇㈠醇濠靛浂妫ら梺鍛婏耿娴滆泛顫忓ú顏咁棃婵炴垶鑹鹃。鍝勨攽閳藉棗浜濇い銊ワ躬楠炲啫顫滈埀顒勫极閸岀偛绀堢憸蹇涙晬濠婂懐纾介柛灞剧懅閸斿秵銇勯鐐村窛缂侇喖顭烽幃娆撴倻濡厧骞嶆俊鐐€栧Λ浣哥暦閻㈠憡鍎楁繛鍡樺姃缁诲棙銇勯幇鈺佺仼闁哄棙鐟╁Λ浣瑰緞閹邦厼浠繛杈剧到閹诧紕鎷归敍鍕＜濠㈣埖锚濞呭秹鏌″畝瀣？濞寸媴濡囬幏鐘裁圭€ｎ亙澹曞┑鐘绘涧椤戝懐绮婚弽顓熺厽闁硅揪绲鹃ˉ澶岀磼閻欐瑥娲ょ粻褰掓倵濞戞鎴犱焊娴煎瓨鍊甸柛顭戝亞婢ц京绱掓潏銊﹀磳鐎规洘甯掗埢搴ㄥ箣閿濆洨宕堕梻鍌欑閹芥粍鎱ㄩ悽鍛婂亱闁绘ǹ顕ч悘鎶芥煥閺囩偛鈧綊宕愮拠娴嬫斀妞ゆ棁妫勬慨鈧梺绋款儐閹稿骞忛崨顖氬闁哄洨鍣ュ鏃堟⒒娓氣偓濞佳呮崲閸℃稑鐒垫い鎺嶇劍閻忛亶鏌涚€ｎ偅宕岄柟顔界矒閹崇偤濡烽妷銏犱壕闁汇垹鎲￠悡鐔兼煏韫囧鐏い蹇嬪灮閹叉悂鎮ч崼婵堫儌閻庤鎸烽悞锔界┍婵犲洤围闁告侗鍠栧▍銈夋⒑閹惰姤鏁遍悽顖涘浮濠€渚€姊洪幐搴ｇ畵閻庢凹鍨堕、妤呮偄閸忚偐鍘搁梺閫炲苯澧悗浣冨亹閳ь剚绋掗…鍥储闂堟稈鏀介柣鎰硾閽勫吋銇勯弬鍖″姛闁兼椽浜堕幃鍓т沪缁嬪じ澹曢梺鍓茬厛閸嬪懐娆㈤弻銉︾叆闁哄洢鍔嬬花濠氭煟閿濆洤鍘存鐐叉喘閹囧醇濠靛懐鏁栨繝鐢靛仩閹活亞绱為埀顒併亜椤愩埄妯€闁诡垯绶氶弫鎰緞鐎Ｑ勫闂備胶枪閺堫剟鎳濇ィ鍐ㄧ劦妞ゆ帊鐒﹂崐鎰偓瑙勬礃閸旀牠藝閻楀牊鍎熼柨婵嗘川閸旇泛鈹戦悙瀛樺鞍闁糕晛鍟村畷鎴﹀箻閹碱厽顔旈梺缁樺姇椤曨厽鏅堕崹顐犱簻闁靛繆鍓濈粈瀣煛娴ｇǹ鏆ｉ柛鈹惧亾濡炪倖甯掗崐缁樼▔瀹ュ鐓熸俊顖氱仢閸氬綊鏌涘▎鎰磳闁哄本鐩俊鐑芥晲閸涱収鐎烽梻浣哥枃濡嫰藝鏉堚晜顫曢柟鐑橆殕閸嬫劗鈧娲栧ú銈夊煕瀹€鍕拺闁告繂瀚﹢鎵磼鐎ｎ偄鐏撮柟顔藉劤閳规垹鈧綆浜為崝锕€顪冮妶鍡楃瑨閻庢凹鍓涙竟鏇犵磼濡偐顔曢梺鐟扮摠閻熴儵鎮橀鍫熷€垫慨姗嗗墯閸ゅ洭鏌″畝瀣М鐎殿噮鍓熷畷褰掝敊閽樺鍋撻鐐粹拺缂備焦蓱鐏忣亪鏌涙惔锝嗘毈鐎殿喖顭烽弫鎰緞婵犲喚妫熼梻浣告贡鏋悗娑掓櫊瀹曟洟骞樼紒妯衡偓鍨箾閹寸偟鎳呯紒鐘愁焽缁辨帗娼忛妸銉﹁癁闂佽鍠楅悷鈺呫€佸Δ鍛劦妞ゆ帒瀚粈澶愭煏閸繍妲归柣鎾存礃閹便劌顫滈崱妤€鈷掗梺绋垮閸旀妲愰幒鎴犳殕闁告劦鍠栭幗鐢电磽娴ｈ櫣甯涚紒璇茬墕閻ｇ兘宕奸弴鐔锋疂闂佸壊鐓堥崑鍛存偟椤愨懇鏀介柣妯虹仛閺嗏晛鈹戦悙鈺佷壕婵犵數鍋橀崠鐘诲川椤忓嫪澹曞┑顔角氶崑鎾绘煕閵夋垵鎷戠槐閬嶆⒒娴ｇ瓔娼愮€规洘锕㈤、姘愁樄闁诡喚鍋ゅ畷褰掝敃閻樿京鐩庨梻浣告贡閸庛倝宕归悽鍓叉晜妞ゅ繐鐗婇悡鐔兼煙閹呮憼缂佲偓鐎ｎ喗鐓欐い鏃傜摂濞堟粓鏌℃担鐟板闁诡垱妫冮崹楣冩嚑椤掆偓閸ゎ剟姊虹拠鍙夊攭妞ゎ偄顦叅闁哄稁鍘介崕鏂库攽閸屾簱褰掓倿閸偁浜滈柟鍝勭Ф鐠愪即鏌涢悢椋庣闁哄本鐩幃鈺呮惞椤愩値妲堕梻浣告啞鐢鏁幒妤€鐓濋幖娣妼缁犲鏌熼崗鍝ヮ槮濞存粍鍎抽妴鎺戭潩閿濆懍澹曢柣搴㈩問閸犳骞愰搹顐ｅ弿闁逞屽墴閺屽秹濡烽妷銉ョ缂備降鍔岄…宄邦潖缂佹ɑ濯撮柛婵嗗婢规洟姊洪悡搴ｇШ缂佺姵鐗曢悾鐑藉箛閺夎法顔愭繛杈剧岛閸嬫挸顭跨捄琛＄湅婵炲皷鏅滈妵鍕箻鐠虹洅銉╂煟閿曗偓閻楀棝鍩為幋锔藉€烽柤纰卞墮椤ｆ椽姊虹拠鑼缂佽鐗嗛锝夊箹娴ｇ懓浜滈梺绋跨箺閸嬫劙宕ｉ崱娑欌拺闁告繂瀚径鍕亜閵娿儲顥滈柍璇茬У缁绘繈宕熼妸銉ゅ闁荤喐鐟ョ€氼厾绮堥崘顏嗙＜妞ゆ棁鍋愰悞鎼佹煕閳瑰灝鍔滅€垫澘瀚换婵囨償閵忕姴鍘為梻浣告惈椤︻垶鎮ч崘顔肩柧婵犻潧顑呴崒銊╂煟濡櫣锛嶇紒鈾€鍋撻梻浣圭湽閸ㄨ棄顭囪缁傛帒饪伴崟顏嗙畾濡炪倖鐗楅悢顒勫绩閼姐倗纾奸柛灞炬皑瀛濋梺瀹狀潐閸ㄥ綊鍩€椤掑﹦绉甸柛瀣缁傚秴螖閸涱喒鎷虹紓鍌欑劍钃遍柣鎾卞劦閺岀喖鎳為妷褏鐓傜紓渚囧枤缁垶濡堕敐澶婄闁冲搫鍟獮宥夋⒑鐠囨彃顒㈢紒瀣浮閳ワ箓宕堕埡鍐х瑝闂佺懓澧界划顖炴偂閻樿櫕鍙忔繝闈涙閻掔偓绻涢崗鐓庡缂佽鲸甯炵槐鎺懳熼崗鐓庡灡闁诲氦顫夊ú蹇涘礉閹达负鈧礁顫滈埀顒勫箖閳轰絼鐔虹矙鎼存挸浜炬繝闈涱儏閽冪喓鈧箍鍎遍悧婊冾瀶閵娾晜鈷戦柛娑橈攻鐏忔壆鈧厜鍋撻柟闂撮檷閳ь兛绀侀埢搴ㄥ箻閸愭彃鏁ゆ俊鐐€栭幐楣冨磻閻斿吋鍋橀柕澶堝劗閺€浠嬪箳閹惰棄纾归柡鍥ュ灩缁犵娀骞栨潏鍓ф偧濞戞挸绉撮湁闁绘挸娴烽幗鐘绘煟閹垮嫮绉柣鎿冨亰瀹曡埖顦版惔鈥崇厒濠电偛鐡ㄧ划宥囨崲閸儱钃熸繛鎴炃氶弸搴ㄧ叓閸ャ劍绀堟い鏃€甯″娲传閸曨喖顏紓浣割槺閸忔鐦繛鎾村焹閸嬫捇鏌＄仦鍓ф创妤犵偞顭囬幑鍕倻濡棿閭梺璇插椤旀牠宕板☉銏╂晪鐟滄棃銆佸鈧畷妤呮偂鎼达絿鐛┑鐘垫暩婵鈧凹鍓熼悰顕€骞囬鑺ユ杸闂佺粯鍔橀崺鏍亹瑜忕槐鎺楃叓椤撶姷鐓撳Δ鐘靛仜椤︾敻宕规ィ鍐ㄦ闁靛濡囨禍鏍⒒娴ｅ憡璐＄紒顕呭灣閺侇噣鍩℃担纰樺亾閹烘挾绡€婵﹩鍘鹃崢顏堟⒑閸撴彃浜濈紒璇插暣瀹曟娊鎮惧畝鈧壕濂告煟濡搫鏆遍柍缁樻礋閺岋紕浠﹂崜褎鍒涢梺璇″枓閺呮盯鎮鹃悜钘夌倞鐟滃宕ラ崒娑楃箚闁绘劦浜滈埀顑惧€濆畷鎴﹀礋椤撶喎搴婇梺鍦劋濮婂綊锝為弴銏＄厵闁绘垶锚濞堥箖鏌ｉ弮鍌氬妺閻庢碍姘ㄩ埀顒傛嚀婢瑰﹪宕伴幒妤€纾婚柟鎹愵嚙缁犺崵绱撴担楠ㄦ岸骞忓ú顏呪拺闁告稑锕﹂埥澶愭煥閺囶亞鐣甸柛鈹垮劜瀵板嫰骞囬鐘插汲闂備礁鎼ú锕傛晪闂佸憡妫戞俊鍥焵椤掑喚娼愭繛鍙壝叅闁绘棁鍋愬畵浣逛繆椤栨繂浜圭痪鎯у悑娣囧﹪顢涢悙瀛樻殸闁诲孩淇洪崑鎰閹捐纾兼繛鍡樺焾濡差喖顪冮妶鍡楃仴闁硅櫕鍔栫粋宥夊箹娴ｇǹ绐涘銈嗘寙閸愨晜鐝氶梻鍌欑窔濞佳囁囬锕€鐤炬繝濠傜墛閸嬪倿鏌ｉ弬鍨倯闁绘挻绋撻埀顒€绠嶉崕杈殽閸涘﹦顩叉繝濠傜墛閸嬨劍銇勯弽銊︾殤闁糕晪缍侀弻鈥崇暆鐎ｎ剛袦闂佽桨鐒﹂崝娆忕暦閹偊妲诲Δ鐘靛仜椤戝懓鐏冮梺缁橈耿濞佳勭濠婂嫮绠剧€瑰壊鍠栭獮鏍ㄣ亜閺囶亞绉€规洖銈稿鎾倷閼碱兘鍋撻鐑嗘富闁靛牆妫欓悡銉╂煕濮橆剦鍎旂€殿喖鐤囩粻娑㈠箻椤栨稒鏉搁梻浣瑰缁嬫垹鈧凹鍓氱粋宥嗙附閸涘﹦鍘辨繝鐢靛Т閸熺増鏅堕悽纰樺亾鐟欏嫭绀冪紒璇插€介悘鍐⒑閸涘﹣绶遍柛姗€绠栭幃浼村Ψ閳哄倻鍘甸梺绋跨箺閸嬫劙寮冲鈧弻娑㈠Ω閵夘喚鍚嬮梺闈涙閹虫ê顕ｆ繝姘ㄩ柨鏃€鍎抽獮妤呮⒒娓氣偓閳ь剚绋戝畵鍡樼箾娴ｅ啿瀚▍鐘炽亜閺嶎偄浠﹂柣鎾存礃缁绘盯宕卞Δ鍐唺缂備胶濮甸悡锟犲蓟閻斿摜鐟归柛顭戝枛椤牓鏌х紒妯煎⒌闁哄苯绉烽¨渚€鏌涢幘瀵告噯闁诲繐顑呴埞鎴炲箠闁稿﹥鎹囬幃鐐烘晝閳ь剟鍩㈠澶嬪亹缂備焦顭囬崢闈涒攽閻愯尙澧曢柣蹇旂箞瀵悂濡堕崶鈺傦紡闂佸憡鎸嗛崘銊ょ帛婵＄偑鍊ら崣鈧繛澶嬫礋楠炴垿宕熼鍌滄嚌濡炪倖鐗楅懝鐐珶婢舵劖鈷掑〒姘ｅ亾婵炰匠鍏犳椽鏁傞悙顒€搴婇梺鍓插亖閸庨亶鎷戦悢鍏肩厓闁靛鍎抽敍宥夋煛閸℃顥㈤柟顔筋殜閺佹劖鎯旈垾鎰佹骄闂備焦瀵х粙鎾诲窗閹邦喗宕叉繛鎴炲焹閸嬫捇鎮介惂璇茬秺椤㈡捇骞橀弬銉︽杸闂佺偨鍎村▍鏇㈠窗濡椿娈介柣鎰皺缁犲鏌熼瑙勬珖闁归濞€閹崇娀顢楁径濠冩澑闂傚倸鍊风粈浣革耿闁秴纾块柕鍫濇处瀹曟煡鏌涢幇鍏哥敖缂佲偓婵犲洦鐓曢柍鈺佸暟閳洟鏌嶉柨瀣诞闁哄本绋撴禒锕傚礈瑜庨崳顓犵磽娴ｇ柉鍏屽褎顨婃俊鐢稿礋椤栨艾宓嗛梺缁樻煥閹碱偊鍩涘畝鍕€甸悷娆忓绾惧鏌涘Δ浣糕枙闁绘侗鍠栬灒闁稿繒鍘ч悵浼存⒑閸︻厾甯涢悽顖滃仱閸┾偓妞ゆ帒顦顔芥叏婵犲啯銇濋柟宕囧仱婵＄兘宕橀崣銉╁仐闁芥ɑ绻堝娲敆閳ь剛绮旈弶鎳堆囧蓟閵夛妇鍙嗛梺鍝勬川閸嬫盯鍩€椤掆偓缂嶅﹪骞冮檱缁犳盯骞橀娑欐澑闂備胶绮灙閻忓繑鐟х划濠氭偋閸稐绨婚梺鍐叉惈閿曘倖鏅堕幍顔剧＜閺夊牄鍔屽ù顕€鏌涢埡瀣暤闁糕斁鍋撳銈嗗笒鐎氼剛澹曟繝姘厵闁告挆鍛闂佹娊鏀辩敮鎺楁箒闂佹寧绻傞幊蹇涘疮閻愮儤鐓曢柣鏂挎惈娴狅妇绱掔紒妯笺€掗柟椋庡Ь椤﹁淇婄紒銏犳灓缂佽鲸甯″畷鎺戭潩濮ｆ鍥ㄧ厵妞ゆ梻鏅幊鍥ㄤ繆椤愩垹鏆欓柍钘夘槹濞煎繘鈥﹂幋鐑囩礀闂傚倸鍊搁崐鐑芥嚄閸洩缍栭悗锝庡枛缁€瀣煕椤垵浜為柡鍡畵濮婄粯鎷呴悷鎵虫灆闂佽　鍋撻弶鍫氭櫆閺嗘粓鏌ㄩ悢鍝勑㈤柦鍐枑缁绘盯骞嬮悙鐢靛彎濠电偛鐭堟禍顏堝蓟濞戙垹绠绘俊銈傚亾閻庢凹鍓欓埢宥呂熺拋宕囩畾闂佺粯鍔︽禍婊堝焵椤戭剙鎳忔刊濂告煥濠靛棙顥欑紒璇叉閺屾盯濡烽姀鈩冪彆濡炪値鍋呭ú鐔煎蓟閻斿吋鍊锋い鎺嗗亾濠⒀屽灦閺屾稑顫濋澶婂壎闂佸搫鏈粙鎴︺偑娴兼潙绀冮柕濠忛檮椤旀垿姊绘担鍛婂暈闁告垵缍婂畷鎴炵節閸パ嗘憰闂佹寧娲嶉崑鎾绘懚閿濆鐓熼柟鎯у暱椤掋垽鏌ｈ箛鎾跺ⅵ婵﹥妞藉畷顐﹀礋椤撶姳绱橀梻浣告憸婵敻鎮ч悩缁樺仒妞ゆ洍鍋撴鐐差儔閺佸啴鍩€椤掑倻涓嶆繛鎴欏灪閳锋帡鏌涢銈呮灁闁愁垱娲橀妵鍕棘閸噮浼€缂備浇椴搁幑鍥х暦閹烘垟鏋庨柟鐑樼箓閺佸ジ姊绘担鍛靛綊顢栭崱娑樼闁搞儺鍓欓拑鐔哥箾閹寸偟鐓繛宀婁邯閺屾盯骞樺璇蹭壕婵犳鍠栧ú顓烆潖濞差亜宸濆┑鐘插閸Ｑ呯磽娴ｈ櫣甯涢柛銊ュ缁碍娼忛妸褏鐦堥梺鎼炲劘閸斿矂鍩€椤掆偓閸㈣尪鐏嬮梺缁橆殔閻楀繒绮婚幘瀵哥闁割偒鍋勯獮妤冪磼缂佹銆掗柍褜鍓氱粙鎺椻€﹂崶顒佸剹闁圭儤姊荤壕濂告煟濡櫣锛嶅褍鐏氶〃銉╂倷閹绘帗娈柧缁樼墵閺屽秷顧侀柛鎾跺枛瀹曟椽濮€閵堝懐顔掗柣鐘叉处瑜板啴鎮楅鐑嗘富闁靛牆妫欓埛鎺楁煛閸滀礁浜柕鍥ㄥ姍椤㈡盯鎮欑划瑙勫濠电偠鎻徊浠嬪箠濞嗘帇浜归柟鐑樼箖閺呯偤姊洪崨濠勨槈闁宦板姂瀵彃鈹戦崶銉ょ盎闂婎偄娲﹂幐鐐櫠濞戙垺鐓曢悗锝庡€栭幋锕€桅闁告洦鍨板Λ姗€鏌熺粙鍨槰婵炲吋妫冨铏圭磼濡儵鎷婚梺鍛婎焼閸涱垳鐒奸梺绋跨灱閸嬬偤鎮￠悢鍏肩厵闁硅鍔栭悵顏堟煙閻у摜绉柡灞诲妼閳藉螣閼测晩鍟嬫繝娈垮枛閿曪妇鍒掗鐐茬闁告稑鐡ㄩ崑锟犳煛婢跺﹦浠㈢悮妯肩磽閸屾艾鈧兘鎳楅崜浣稿灊妞ゆ牜鍋涚粈澶愭煛閸ャ儱鐏柡鍛箞閺屾洝绠涚€ｎ亖鍋撻弽顓熷€块柤娴嬫杹閸嬫捇鐛崹顔煎濠碘槅鍋呴〃鍡欑矉閹烘纭€闁绘劏鏅滈弬鈧俊鐐€栭弻銊╋綖閺囩喓顩锋繝濠傜墛閻撴洟鎮楅敐搴′簼閻忓繋鍗抽弻鐔哥瑹閸喖顬夌紓浣虹帛缁诲牆鐣烽悢纰辨晣闁绘劖顔栭崑褔姊婚崒娆掑厡闁硅櫕鎹囧畷鏌ュ蓟閵夈儳鐤囧┑顔姐仜閸嬫挻顨ラ悙瀵稿⒊闁靛洦鍔欓獮鎺楀箻閸忓懐鏁鹃梻鍌欐祰椤顢欓弽顓為棷妞ゆ洍鍋撶€规洘鍨奸ˇ瑙勬叏婵犲偆鐓肩€规洘甯掗埢搴ㄥ箳閹存繂鑵愮紓鍌氬€风欢锟犲闯椤曗偓瀹曞綊宕奸弴鐐嶃儵鏌涢幇闈涙灈妤犵偑鍨烘穱濠囧Χ閸滃啯鏁剧紓鍌氱Т闁帮綁寮婚敐澶嬪亹闁告瑥顦ˇ鈺冪磽娴ｉ鍔嶉柟绋垮暱椤曪綁宕ㄦ繝鍐ㄥ妳闂侀潧饪垫俊鍥储椤忓牊顥婃い鎰╁灪婢跺嫭绻涢崣澶屽⒌妞ゃ垺鎸炬禒锕傚礈瑜忛鏇㈡煟韫囨挾绠查柣妤侇殔閳诲秵绻濋崶銊у幍濡炪倖姊婚崑鎾诲吹閳ь剟鏌ｉ幘鍗炩偓鏍Φ閸曨喚鐤€闁圭偓鎯屽Λ銈囩磽娴ｇ懓濮堟い銊ワ工椤繘鎼圭憴鍕幑闂佸憡渚楅崢婊堝箻鐎靛摜顔曢梺鑲┾拡閸撴瑩寮告惔鈧簻妞ゆ劑鍨荤粻浼存偂閵堝棎浜滈煫鍥ㄦ尰婵吋淇婇銏犳殭闁宠鍨块幃娆戔偓闈涙啞濞堫剟姊虹粙鍧楀弰濞存粌鐖奸獮鍐樄妤犵偞锕㈤獮鍥ㄦ媴閸涘⿴鍚欓梻鍌欑濠€杈╁垝椤栨凹鍟呭┑鐘宠壘闁卞洦銇勮箛鎾跺闁绘挻娲橀妵鍕箛閸撲焦鍋х紓浣哄У閿曘垽寮婚妸銉㈡婵炲棙鍨堕崚娑樷攽椤旂》鏀绘俊鐐舵閻ｇ兘濡搁敂鍓х槇闂佸憡鍔﹂悡鍫澪ｉ柆宥嗏拻濞达綀顫夐崑鐘绘煕閺冣偓閸ㄥ墎绮嬪澶婇唶闁哄洨鍋熼悾娲⒑鐠恒劌鏋斿┑顔芥尦瀹曪繝骞庨懞銉у幗闂佸搫鍟ú锕傤敂閻樼偨浜滈柡鍐ｅ亾婵炶尙鍠庨～蹇撁洪鍛画闂備緡鍙忕粻鎴濃枔閸洘鈷戠紒瀣仢椤掋垽鏌涢妸銉т虎闁伙絽鍢茶灃闁告粈鐒﹂弲銏ゆ⒑閸涘﹥澶勯柛鎾寸洴瀹曘垽鏌嗗鍡忔嫼闂佸憡绻傜€氼剟寮冲▎鎾寸厱濠电姴鍟崐鎰偓娈垮枛閹诧繝骞嗛弮鍫澪╅柨鏃傜帛閳锋牕鈹戦悩顔肩伇闁糕晜鐗犲畷婵嬪冀椤撶喎浜楅梺缁樻煥閸氬鎮″▎鎾寸厽闁逛即娼ф晶顕€鏌嶇紒妯荤闁靛洤瀚幆鏃堝閳哄倻鏆﹂梻渚€娼уú銈団偓姘煎灣缁鈽夐姀鈩冩珳闂佸憡渚楅崢楣冾敊閹达附鈷掑〒姘ｅ亾闁逞屽墯缁嬫捇鍩為幒妤佺厱闁哄倽娉曡倴闂佺懓绠嶉崹褰掑煘閹寸姭鍋撻敐搴濈敖妞わ负鍔戝鍝勭暦閸ャ劌娈岄梺闈涙处閿曘垽骞栫憴鍕劅闁靛⿵濡囬崢浠嬫煙閸忚偐鏆橀柛濞垮€曢…鍥箛椤撶姷顔曢梺鍛婄懃椤﹂亶鎯岄幒鏂哄亾鐟欏嫭绀冩い銊ユ嚇閿濈偛饪伴崼婵堝姦濡炪倖甯掔€氼剝绻氬┑鐐舵彧缁茶姤绔熸繝鍌ょ劷闁靛ň鏅滈悡鍐喐濠婂牆绀堥柣鏃囧亹瀹撲線鏌涢幇闈涙灈妞ゎ偄鎳橀弻鏇㈠醇濠靛洤娅㈢紓浣稿船閻栫厧顫忓ú顏勫窛濠电姴鍟ˇ鈺呮⒑缁嬪簱搴风紓宥勭窔閵嗕線寮撮姀鐘靛幀闂佸啿鎼崐褰掑级閹间焦鈷掑ù锝堟鐢盯鏌ㄥ鑸电厽闊洦鏌ㄩ崫铏光偓娈垮枟婵炲﹪宕洪敓鐘插窛妞ゆ梹鍎抽獮鍫ユ⒑鐠囨彃鍤辩紓宥呮瀹曟粌鈻庨幇顕呮祫闁哄鐗滈悡鍫濃枔娴犲鐓熼柟閭﹀幗缂嶆垵鈹戦纰辨Ш缂佽鲸甯￠、娆撴偩鐏炴儳娅氶柣搴ゎ潐濞叉粍绻涢埀顒佷繆妤ｅ啩鎲鹃柡灞界Х椤т線鏌涢幘瀵告噧妞ゎ厼娲崹楣冨箛娴ｅ憡顓块梻浣稿閸嬪懎煤閺嶎厽鐓侀柛銉ｅ妽閸欏繑鎱ㄥΔ鈧Λ妤呯嵁濡ゅ懏鐓熸繝闈涙处椤ュ牊鎱ㄦ繝鍐┿仢妤犵偞鐗犻幃娆撳箵閹烘繄鈧绱撻崒娆愮グ妞ゆ泦鍥ㄥ亱闁圭偓鍓氶崵鏇㈡偣閸パ勨枙闁告艾顑呴…璺ㄦ崉閻氬瓨顣肩紓浣稿船閻栫厧顫忓ú顏勫窛濠电姴鍟ˇ鈺呮⒑閸涘﹥灏伴柣鐔村劦閸┿垹顓兼径瀣珳闂佹悶鍎弲婵嬫晬濠靛洨绠鹃弶鍫濆⒔閸掍即鏌熺喊鍗炰簻闁哄棌鏅濈槐鎾诲磼濞嗘垼绐楅梺闈╃秵閸ｏ絽鐣疯ぐ鎺撳殝闁稿繐鎽滅粻姘舵⒑濮瑰洤鐏弸顏嗙磼閳ь剛鈧綆鍋佹禍婊堟煛瀹ュ啫濡介柣銊﹀灴閺岋綁顢曢姀鐙€浼冨┑顔硷龚濞咃絿鍒掑▎鎾崇閹兼番鍨虹€氬磭绱撻崒娆戭槮妞ゆ垵妫涢弫顕€鏁撻悩鑼暫闂佹眹鍨规竟濠囧极閸岀偞鐓曟い鎰╁€曢弳閬嶆煙闁垮銇濇慨濠冩そ瀹曘劍绻濋崟銊︻潔闂備焦瀵уú鈺冪不閹捐崵宓佸┑鐘叉搐缁€鍐煃鐞涒€充壕缂備浇顕уΛ娆撳Φ閸曨垰绠涢柛鎾茬劍閸嬔呯磽娴ｅ搫校闁圭ǹ顭锋俊鐢稿礋椤栵絾鏅濋梺闈涚箞閸ㄥ顢欓幒妤佲拺闁革富鍘肩敮鍓佺磽瀹ュ拑宸ラ柣锝囧厴閹粎绮电€ｎ偅娅嶉梻浣虹帛閻熴儵骞婇幇顓熷劅濠电姴鍊甸弨浠嬫煟濡椿鍟忛柡鍡╁灡娣囧﹪骞撻幒鎾虫畻閻庤娲栫紞濠偽涢崘銊㈡婵﹩鍘介鏇㈡⒒娴ｅ湱婀介柛銊х帛閻忔瑩姊哄ú璇插箺妞ゃ劌锕璇测槈閵忕姷鍔撮梺鍛婂姦娴滄牗鎱ㄩ崶顒佲拺闁规儼濮ら弫閬嶆煕閵娿儳绉洪柟顖楀亾濡炪倕绻愰悧婊堝极閸ヮ剚鐓熼柟閭﹀幗缂嶆垶銇勬惔銏㈡创婵﹥妞介獮鏍倷濞村浜鹃柡鍥ュ焺閺佸嫰鏌涢妷銏℃珕妞ゎ偅娲熼弻鐔兼倻濡儵鎷荤紒鐐劤閸氬绌辨繝鍥舵晬婵犻潧妫斿Ч妤呮⒑濮瑰洤鈧洜鈧凹鍓濋悘鍐⒑缂佹﹫鑰挎繛浣冲嫮顩锋繝濠傜墛閻撴洟鏌ｉ弴姘鳖槮濞存粈鍗抽弻銊モ攽閸繀绮跺Δ妤婁簷閸楀啿鐣烽悡搴樻斀闁割偅绻傝婵犵绱曢崑鎴﹀磹閺嶎偅鏆滈柟鐑樻煛閸嬫挸顫濋妷銉ヮ潎闂佺硶鏂侀崑鎾愁渻閵堝棗鐏辨繛澶嬫礋钘濋柨鏂款潟娴滄粍銇勯幇鍓佹偧缂佺姵锕㈤弻鐔兼偡閺夋浼冮梺鍦帶缂嶅﹪骞冮悜钘夌骇闁哥喎鍟ú鏍煘閹达附鏅柛鏇ㄥ亗閺夘參姊虹粙鍖℃敾闁绘绮撳顐︻敋閳ь剟鐛幒妤€妫橀柛婵嗗婢规洖鈹戦绛嬬劷闁告鍕珷闁圭虎鍠楅悡娆撴⒑閸噮鍎忛柣蹇旀尦閺岀喖顢欓崫鍕紙閻庤娲橀〃濠囧箠閺嶎厼鐓涘ù锝呮憸椤旀垿姊婚崒姘偓椋庣矆娓氣偓楠炴牠顢曢敃鈧壕褰掓煙闂傚鍔嶉柛濠呭煐缁绘繈妫冨☉姘櫏婵犮垼娉涘ù鐑芥偪閳ь剟姊洪崫鍕偓鍛婃櫠閻ｅ瞼涓嶆慨妯垮煐閳锋垿鏌涢敂璇插箻閻㈩垱鐩幃浠嬵敍濠婂啩鎴风紓渚囧枟濡啴骞冨⿰鍐炬建闁糕剝锕╅崯宥夋⒒娴ｈ櫣甯涢柛鏃€娲熼、姘额敇閻斿憡鐝烽梻渚囧墮缁夌敻鎮￠弴銏＄厵闁绘垶蓱鐏忣厽绻涢幖顓炴灓闁逞屽墲椤煤濮椻偓閵嗗啴宕卞Δ濠傛闂佸憡娲﹂崜娑氬姬閳ь剚绻涙潏鍓у埌濠㈢懓锕よ灋婵犲﹤鐗婇埛鎺楁煕鐏炲墽鎳呮い锔肩畵閺岀喓鎷犺缁♀偓閻庤娲樼换鍌濈亽闂傚倸鍊搁顓㈠礈閵娿儮鏀介柣鎰级椤ョ偤鏌熺亸鏍ㄦ珚鐎殿噮鍋婂畷鍫曞Ω瑜忛鏇㈡倵閻熸澘顥忛柛鐘虫礈閼鸿鲸绺介崨濠勫帗闂備礁鐏濋鍛归鈧弻锛勪沪閸撗佲偓鎺懨归悪鍛暤鐎规洘绮忛ˇ鎶芥煕閿濆骸骞樼紒杈ㄦ尰閹峰懘鎮块姘腐濠电姭鎷冮崟鍨暦缂備礁鐭佹ご鍝ユ崲濠靛纾奸柕鍫濇閻︽粓姊绘担铏瑰笡闁圭ǹ鎲￠〃銉╁箹娓氬﹦绋忛梺鎼炲労閸撴岸鍩涢幋锔藉仯闁搞儻绲洪崑鎾绘惞椤愩倓澹曢梻鍌欒兌鏋い鎴濆暣瀹曟劕鈹戦崱鈺佹濡炪倖鍔х粻鎴犵矆閸愨斂浜滈柡鍐ㄦ搐娴滃綊鏌ㄥ☉娆戠煀闁宠鍨块、娆撳棘閵堝牃鍋撶捄銊ф／妞ゆ挾鍠愰崐鎰偓瑙勬处閸撴繈濡甸幇鏉跨闁圭虎鍨辩€氳棄鈹戦悙鑸靛涧缂傚秮鍋撳銈庡亜椤﹂潧鐣烽幋锔藉亹缂備焦顭囬崢閬嶆煙閸忚偐鏆橀柛銊ヮ煼閹瞼浠﹂惌顐㈢秺閹亪宕ㄩ婊勬闂備胶鎳撶壕顓㈠磻閵堝棔绻嗛柣鎴犵摂閺佸﹪鏌涜箛鏇炲付濞存粌鍚嬬换婵嬫偨闂堟刀銉︺亜閿濆骸鏋ゆ俊鍙夊姍瀵挳濮€閻樼绱遍梻浣侯攰閹活亞绮婚幋鐘差棜鐟滅増甯楅悡娑氣偓骞垮劚妤犲憡绂嶅┑瀣厸闁糕剝顭囬惌鎺楁煛瀹€鈧崰鏍箖濠婂吘鐔兼嚒閵堝懎绠伴梻鍌欒兌鏋い鎴濇楠炴劙骞栨笟濠勭◤濠电娀娼ч鍡涘磻閸岀偛绠圭紒顔款潐椤﹥绻涢懖鈺冨笡缂佺粯绻堥幃浠嬫濞戞鍕冮柣鐔哥矋濠㈡﹢宕锔光偓锕傚炊椤掆偓閻忔娊鏌熸０浣哄妽缂傚秴娴风槐鎾诲磼濞嗘垵濡介柦鍐ㄥ船閳规垿顢涘☉娆忓攭闂佸搫鐫欓崱娆戞澑闂佽鍎虫晶搴ㄥ汲濞戙垺鈷戦梺顐ゅ仜閼活垱鏅堕鈧弻銊╁即閵娿倝鍋楅悗娈垮枦椤曆囧煡婢舵劕顫呴柣妯活問閸熷姊虹拠鏌ュ弰婵炰匠鍥х婵犲﹤鐗婇崑顏堟煕閺囥劌鐏￠柍閿嬪灴濮婂宕奸悢琛″濡炪們鍎茬划鎾诲蓟濞戙垺鍋勫瀣嚱缁辩偟绱撴笟鍥ф灕妞ゆ泦鍥х叀濠㈣埖鍔曢～鍛存煟濡澧俊顐ゅ枑缁绘繈鎮介棃娑楃捕濠碘槅鍋呴悷鈺佺暦閺囥垹绀冩繛鏉戭儐閻忎焦绻濋棃娑樷偓鎼佸箟閿熺姴鐓曢柟鐑樺殮瑜版帗鏅查柛銉戝啫顬嗛梻浣规偠閸婃劙宕戦幘鏂ユ斀闁绘﹩鍋呮刊浼存煕濞戝崬鏋熼柣婵愪邯濮婃椽宕ㄦ繝鍌滅懖闁汇埄鍨界换婵嬪Υ娴ｇ硶妲堥柕蹇娾偓鏂ュ亾閻戣姤鐓犵痪鏉垮船婢ь喗顨ラ悙鑼ⅵ婵﹦绮幏鍛村川婵犲懐顢呴梻浣呵圭花娲磹濠靛棛鏆﹂梻鍫熶緱濞尖晠鏌ｉ幇顓炵亰婵顨婂娲捶椤撶偛濡洪梺瑙勭摃椤曆囧煝瀹ュ應鍫柛顐ゅ暱閹锋椽姊洪崨濠冨鞍鐟滄澘鍟粋宥呪攽閸垻锛滃銈嗘濡嫰鍩㈤弴鐕佹闁绘劘鎻懓璺ㄢ偓瑙勬礃缁秹骞忛崨瀛樺剬闁告縿鍎抽、鍛節绾板纾块柛瀣灴瀹曟劙寮介鐐殿唶婵°倧绲介崯顐ゅ婵犳碍鐓熼柡鍐ㄥ€哥敮鍫曟煛閸曗晛鍔﹂柡灞诲妼閳规垿宕遍埡鍌傃囨⒑缂佹ɑ灏伴柣鈺婂灦楠炲啫顫滈埀顒勫箖濞嗘挸绠甸柟鍝勬鐎垫牗绻濋悽闈涗粶鐎殿喖鐖奸獮鎰板箮閽樺鎽曢梺缁樻⒒閳峰牓寮繝鍥ㄥ仭婵炲棗绻愰顏堟煟濠靛洤鍝洪柟顔筋殘閹叉挳宕熼鍌ゆО闂備焦瀵уú蹇涘垂娴犲鍋樻い鏇楀亾妤犵偞顭囬幏鐘绘嚑椤掑鏁藉┑鐘垫暩婵炩偓婵炰匠鍏犳椽濡歌閻瑩鏌涢鐘插姕闁抽攱甯掗湁闁挎繂娲ㄩ幗鍌炴煕閵堝棛鎳囬柟顔筋殔閳藉骞掗幘瀵稿綃婵＄偑鍊戦崹娲偡閳轰緡鍤曞ù鐘差儛閺佸洭鏌ｉ幇顔芥毄鐎规洖鐖煎缁樻媴閸涘﹥鍎撶紓浣割儎缁舵艾鐣烽姀銈嗗殐闁冲搫鍟╃粭澶愭⒑缂佹ɑ鐓ラ柛姘儔閹繝濡烽敂鍓ь啎闂佺懓顕导婵嬵敂閸偅鏅滃銈呯箰濡矂宕戦幘鑸靛枂闁告洦鍓涢ˇ銉╂倵濞堝灝鏋涘褍閰ｉ獮鎴﹀閻橆偅鏂€闁诲函缍嗛崑鎺懳涢崘銊㈡斀闁绘劖娼欓悘锕傛煟閻旇鍚俊鍙夊姍閹瑥霉鐎ｎ偅鏉搁梻浣虹帛閸旀浜稿▎寰帡鎮欓悜妯煎幐闂佸憡渚楅崰姘舵儗閹烘垟鏀介柨娑樺閺嗩剛鈧娲滈崰鏍€佸☉姗嗘僵濡插本鐗曢弫浠嬫⒒閸屾瑨鍏岄柟铏崌閹椽濡歌瀹曟煡鏌嶈閸撴稓妲愰幒妤€惟闁靛牆娲ㄩ悡鎾斥攽椤旂》鏀绘俊鐐舵閻ｇ兘顢曢敃鈧敮闂侀潧顦崐鏍春濞戙垺鈷掑ù锝堝Г閵嗗啯绻涢弶鎴炲枠妞ゃ垺淇洪ˇ褰掓煥濠靛牆浠╃紒鐘崇洴瀵挳鎮╃喊澶屽簥濠电姷顣藉Σ鍛村垂閻㈠壊鏁嬬憸鏃堛€侀幘璇茬缂備焦菤閹锋椽姊虹粙璺ㄧ婵☆偄娼￠幊婊堫敂閸喓鍘遍柟鍏肩暘閸ㄥ綊鍩㈤弴鐐╂斀闁炽儱纾幗鐘电磼缂佹绠炵€规洖鐖兼俊鎼佸閳ユ剚浼栭梻鍌氬€搁崐鎼佸磹閹间焦鍋嬮柛鎰靛枛閻ょ偓绻濋棃娑氬ⅱ闁活厽鎹囬弻娑㈠箻閼艰泛鍘＄紒鐐劤閵堟悂寮婚敐鍛傜喖骞愭惔锝呮锭闁诲氦顫夊ú鏍儗閸岀偛钃熼柨婵嗘噳濡插牓鏌涘Δ鍐ㄤ沪闁诲繑娲滅槐鎾存媴閸濆嫅锝囩磼鐎ｎ偄鐏撮柛鈹垮劜瀵板嫭绻涢悙顒傗偓濠氭⒑瑜版帒浜伴柛娆忓缁傛帟顦规慨濠傤煼瀹曟帒鈻庨幇顔哄仒婵犵數鍋涢ˇ鏉棵洪悢椋庢殾闁规儼濮ら弲婵嬫煕鐏炵偓鐨戞い鏂挎濮婅櫣鎹勯妸銉︾彚闂佺懓鍤栭幏锟�
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
        //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆撴倻濡桨鐢绘繝鐢靛Т閿曘倝宕板顑╃喖鍩€椤掑嫭鍊垫繛鍫濈仢閺嬬喖鏌熼鐓庘偓鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸閻忕偟纭堕崑鎾崇暦閸ャ劍鐣烽梻渚€鈧偛鑻晶瀵糕偓瑙勬磻閸楀啿顕ｉ崐鐕佹Ь闂佸搫妫撮梽鍕崲濠靛洨绡€闁稿本鍐荤槐鐐测攽閳╁啰鍙€缂佺姵鐗犲濠氭晲婢跺⿴娼婇梺瀹犳〃閼宠埖绂掗銏♀拺闁绘垟鏅滃▍鎾绘煕閵娿劍纭炬い鏇悼閹风姴霉鐎ｎ偒娼旀繝娈垮枟閿曨偆寰婇懞銉ь洸濡わ絽鍟悡銉︾節闂堟稒顥犻柛鎴濇贡缁辨帡濡搁妷顔惧悑闂佸搫鑻粔鐑铰ㄦ笟鈧弻娑㈠箻鐎靛憡鍣紓浣戒含閸嬬偟鎹㈠┑瀣鐟滃繒绮诲鑸碘拺婵懓娲ら悘顕€寮搁鍡欑＜闁逞屽墰閳ь剨缍嗛崑浣圭濠婂牊鐓涚€广儱鍟俊浠嬫煟閵婏箑鐏撮柡宀嬬秮婵″爼宕掑顐㈩棜闂傚倸鍊搁崐鎼佸磹妞嬪海鐭嗗〒姘ｅ亾闁诡喖娼″畷鎯邦檨婵炲瓨鐗犻弻鏇熺箾瑜嶇€氼剟顢撳☉銏♀拺闁告繂瀚～锕傛煕鎼淬倓鍚瑙勬礃缁绘繂顫濋鐘插箥闂備胶顢婇～澶愬礉閿曞倸鍚规繛鍡樺灩绾惧ジ鏌ｅΟ鐑樻儓闁告艾婀辩槐鎺撴綇閵婏箑闉嶉梺鐟板槻閹冲繘骞堥妸鈺佺妞ゆ挻绻堥崑妤呮⒑缁洘鏉归柛瀣尭椤啴濡堕崱妤€娼戦梺绋款儐閹瑰洭寮婚敐鍫㈢杸闁哄啫鍊婚悿鍕⒑缁洘娅囬柛瀣ㄥ€濋悰顔锯偓锝庡枟閺呮粓鏌﹀Ο渚Х婵顨婂缁樻媴閸涘﹤鏆堥梺鑽ゅ枂閸庝絻妫熸繛鏉戝悑濞兼瑧绮婚弽銊х闁糕剝锚閻忊晝鐥幑鎰棄闂囧鏌ㄥ┑鍡樺櫤闁诡垰鐗撻弻锝呪攽閹惧墎鐦堥梺鍝勬湰閻╊垱淇婇悜鑺ユ櫜闁告侗鍙庨悗鐑芥⒒娴ｅ憡鍟為柨姘扁偓瑙勬处閸撶喖鍨鹃弮鍫濈妞ゆ柨妲堣閺屾盯鍩勯崘鐐暥閻炴稖鍋愮槐鎾诲磼濞嗘垵濡介悷婊勬緲閸燁偊鈥﹂崶顒佹櫢闁绘ê寮妷鈺傜厱鐎光偓閳ь剟宕戝☉銏″剹婵°倐鍋撴い顓℃硶閹瑰嫰鎼归崷顓濈礃婵＄偑鍊栧鐟懊洪悢濂夋綎婵炲樊浜滃婵嗏攽閻樿精鍏岀紒璁崇窔濮婃椽宕ㄦ繛鎺曞亹閸掓帒鈻庤箛鏇熸闂佹眹鍨绘灙闁绘劕锕ラ妵鍕箳瀹ュ牆鍘￠梺鍛娚戦幃鍌炲箖濡ゅ懎绀傚璺猴梗婢规洟姊虹拠鎻掑毐缂傚秴妫濆畷婊冣槈濠ф儳寰嶆繝鐢靛Х閺佸憡鎱ㄩ悜濮愨偓鍌涚鐎ｎ亞顦┑鐘绘涧椤戝懐绮婚弽顓熺厱鐎光偓閳ь剟宕戝☉銏犲嚑濞撴埃鍋撻柡灞剧洴楠炲洭妫冨☉娆戜邯闂備胶绮敮鎺楁倶濮樿泛桅闁告洦鍨扮粻鎶芥煙鐎涙绠ュù鐘荤畺濮婅櫣绮欓崠鈥充紣闂佺粯鐗曢妶绋款嚕婵犳碍鍋勯柧蹇撴贡閿涙粓姊洪棃娑氱濠殿喚鏁婚幃妯侯吋婢跺鎷虹紓鍌欑劍钃遍悘蹇斿缁辨帞鈧綆鍋呭畷宀勬煕閳规儳浜炬俊鐐€栧濠氬磻閹剧粯鐓曟俊顖濆吹閻帞鈧娲橀崹鍧楃嵁濡偐纾兼俊顖滃帶鐢鏌ｉ悢鍝ョ煁缂侇喗鎸搁悾宄扳堪閸喎浜滄俊鐐差儏鐎涒晛鈻撻悙顒傜闁哄鍨甸幃鎴炵箾閸忚偐鎳呯紒顔款嚙閳藉濮€閿涘嫬骞堥梻濠庡亜濞村嫮寰婇懞銉ь洸婵犲﹤瀚粻鍓р偓鐟板閸犳牕顕ｉ绛嬫闁绘劖褰冮弳銏°亜閿曗偓濠€閬嶅焵椤掑喚娼愭繛鍙夌墵婵″爼骞栨担鍝ョ暫闂侀潧绻掓慨鐑藉汲鐎ｎ喗鐓涘璺哄瘨閸わ箓鏌曢崼婵愭Ч闁抽攱甯￠弻娑氫沪閸撗勫櫙闂佺ǹ绻愰惌鍌炲蓟濞戞埃鍋撻敐鍐ㄥ濠殿喖鍊婚埀顒冾潐濞叉ê煤閻旂厧钃熼柛鈩冾殢閸氬鏌涘☉鍗炵仭闁绘帡绠栧娲嚒閵堝憛锝吤瑰⿰鈧划娆忕暦濠靛洦鍎熼柕蹇婃噰閺嬫牠鎮楅獮鍨姎妞わ缚鍗抽幃锟犲Ψ閿斿墽鐦堥梻鍌氱墛缁嬫帡鏁嶅鍡曠箚闁圭粯甯炵粔娲煛鐏炵晫效闁哄被鍔庨埀顒婄秵娴滅偤鎮烽妸鈺傗拻闁搞儜灞锯枅闂佸搫琚崝宀勫煘閹达箑骞㈡繛鍡楁禋閺嗩偊鏌ｉ悙瀵割暡鐎光偓缁嬫娼栨繛宸簼椤ュ牊绻涢幋鐐垫噽婵☆偆鍋ゅ娲传閵夈儲鐎鹃梺鍦归崯鍧楊敋閿濆棛绡€婵﹩鍘藉▍婊勭節閵忥絾纭鹃柨鏇畵椤㈡瑦绻濋崶銊㈡嫼缂傚倷鐒﹂敋濠殿喖顦甸弻鈩冩媴鐟欏嫬鈧劗鈧鍠楁繛濠冧繆閸洖宸濋柛灞剧⊕椤ュ牓鏌熼瑙勬珚婵☆偄鍟埥澶娢旀担绋款€忛梻鍌氬€烽悞锕傚箖閸洖绀夌€光偓閸曨剙鈧埖绻濋棃娑卞剱闁稿鏅犻弻锝夊箣閿濆棭妫勭紓浣哄У瀹€绋款潖濞差亶鏁囬柕濞炬櫆椤庡秴顪冮妶鍛搭€楁繛宸幖椤繒绱掑Ο璇差€撻柣鐔哥懃鐎氼剚绂掗埡鍛拺缂佸灏呴弨濠氭煟濡や焦绀堥柛娆忔噹閳规垿顢欐慨鎰捕闂佺ǹ顑嗛幐鎼佸煘閹达附鏅柛鏇ㄥ墯濮ｅ牓鎮楃憴鍕闁哥姵鐗犻妴浣糕槈濮楀棙鍍垫俊鎻掓湰閻楁洟寮禒瀣拻闁稿本鐟ㄩ崗宀勬煙閾忣偅宕岄柟顔芥そ婵℃瓕顦崇€规洝灏欓埀顒€绠嶉崕鍗炍涘☉鈧偓鍛暋閹佃櫕鏂€闂佺ǹ鏈喊宥夊疮閻愮儤鐓熼柟鎯х摠缁€瀣煛鐏炵偓绀冪€垫澘瀚埥澶愬閳藉棌鍋撳澶嬬厽闊洦鎹囬悰婊堟⒒閸曨偄顏鐐茬墦婵℃悂鏁傞崜褏妲囬梻浣告啞娓氭宕抽鐣岊浄闁冲搫鎳忛埛鎴︽煕濞戞﹫鍔熷褜鍓熼弻锝呂旈崘銊愩垺銇勯弴顏嗙М妤犵偞锕㈤、娆撴寠婢跺棗浜鹃梺鍨儑缁犻箖鏌℃径瀣劸闁稿孩鍔欓幃妤€顫濋崡鐐寸€婚梺瀹狀潐閸ㄥ潡寮澶婄妞ゆ劏鍓濆鈧梻鍌欒兌鏋柨鏇樺劦瀹曞綊宕归鐐濠电娀娼уΛ娑㈠汲閸℃稒鐓冪憸婊堝礈閻旇偐宓侀柟杈剧畱椤懘鏌曢崼婵囧櫧妞ゆ挻妞藉铏圭磼濮楀牅绶甸梺绋款儏鐎氼厾绮嬪鍡愬亝闁告劏鏂侀幏濠氭⒑閸撴彃浜為柛鐘虫崌閸╋綁濡舵径瀣幈闁诲函绲婚崝宀勬倶椤忓懌浜滈柕濠忕到閸旓箓鏌熼鐣屾噮闁逞屽墰閺佸憡鐏欓梺鍐插槻閼活垵鐏冮梺缁橈耿濞佳勭濠婂嫮绠鹃柟缁㈠櫘濡垿鏌涢幒鎾崇瑨妞ゎ偅绻勯幑鍕传閸曨厼缍佺紓鍌氬€峰ù鍥р枖濞戔偓鈧懘鎮介—纾榬y闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆撴倻濡桨鐢绘繝鐢靛Т閿曘倝宕板顑╃喖鍩€椤掑嫭鍊垫繛鍫濈仢閺嬬喖鏌熼鐓庘偓鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸閻忕偟纭堕崑鎾崇暦閸ャ劍鐣烽梻渚€鈧偛鑻晶瀵糕偓瑙勬磻閸楀啿顕ｆ禒瀣垫晣闁绘劗鏁搁弳顐︽煟閻斿摜鐭嬮柡灞诲姂閹箖鏌ㄧ€ｎ剟妾梺鍛婄☉閿曘倖绂嶅⿰鍫熲拺闁告稑锕︾粻鎾绘倵濮樼厧澧柍缁樻崌婵＄兘鍩￠崒妤佸闂備胶枪閺堫剟鎮疯钘濋柨鏇炲€归悡娆撴偣閸ュ洤鎳愰惁鍫ユ倵鐟欏嫭绀€婵犫偓闁秴绠熼柟缁㈠枛缁€瀣亜閹哄棗浜鹃悗娈垮枛濞尖€愁潖濞差亜鎹舵い鎾墲椤庢姊洪崫銉バｉ柣妤冨█楠炲啴鍨鹃幇浣瑰缓闂侀€炲苯澧寸€殿喖顭烽幃銏ゅ川婵犲嫮肖濠德板€х徊浠嬪疮椤栫儐鏁佺€广儱顦伴埛鎴犵磼鐎ｎ偒鍎ラ柛搴㈠姍閺屾盯寮埀顒勬偡閳哄懐宓侀柛鎰╁壆閺冨牆宸濇い鎾跺枎缁佽埖淇婇悙顏勨偓鏍箰妤ｅ啫纾规い鎰剁畱椤懘骞掗搹顐ゎ洸婵犲﹤鐗婇悡蹇擃熆閼哥數鈽夋い鈺婂墰缁辨帡鎮╅幇浣哄姺缂備浇椴哥敮妤€顭囪箛娑樜╅柨鏇楀亾闂傚绉瑰娲偡閺夋寧些闂佺娅曢敋闁伙絿鍏橀、鏇㈡晝閳ь剛绮堢€ｎ剛妫柣妤€鐓鍛洸闁告挆鈧崑鎾诲礂婢跺﹣澹曢梻浣告啞濞诧箓宕滃☉銏犲偍闂侇剙绉甸埛鎴︽⒒閸喕鍎愮憸鐗堝笒绾惧潡鏌熼幆鐗堫棄闁哄嫨鍎甸弻銊モ攽閸℃顦遍梺绋款儐閹告悂鍩㈤幘璇插瀭妞ゆ梻鏅禍顏呬繆閻愵亜鈧倝宕㈡ィ鍐ㄧ婵☆垯璀﹂崵鏇熴亜閹板墎鐣辩紒鐘崇⊕閵囧嫰骞樼捄杞版埛缂備焦鍔楅崑鐐垫崲濠靛鍋ㄩ梻鍫熷垁閵忋倖鍊垫慨妯哄船閸樺鈧娲忛崕闈涚暦閵娧€鍋撳☉娅辨岸骞忕紒妯肩閺夊牆澧介幃濂告煙閾忣偅宕屾い銏¤壘楗即宕ㄩ婊愮床闂佽崵濮村ú锕併亹閸愵喖鐒垫い鎺嗗亾婵炵》绻濋幃浼搭敊閽樺绐炴繝鐢靛仧閸嬫捇锝炲澶嬧拺闂傚牊绋撶粻鍐测攽椤旀儳鍘存い銏＄懇楠炴帡寮崒婊愮闯闂備胶枪閺堫剟鎮疯瀹曟繂顓奸崱鎰盎濡炪倖鎸鹃崑鐐哄窗濡皷鍋撶憴鍕８闁搞劋绮欓悰顕€寮介妸锕€顎撻梺绋跨箰椤︽壆鈧艾銈稿缁樻媴閸涘﹤鏆堢紓浣割儐閸ㄥ潡寮崘顔芥櫜闁搞儺鐓堥崑銊╂⒒娓氬洤澧紒澶屾暬閹繝寮撮姀锛勫幗濠碘槅鍨甸褎鏅堕娑氱闁割偆鍠撻惌宀勬煃瑜滈崜婵嬶綖婢跺⊕鍝勵潨閳ь剙鐣峰┑鍡忔瀻闁规崘娉涚粊锕傛⒑閹肩偛鍔撮柛鎾村哺瀹曟垹鈧綆鍠楅悡鏇熴亜閹邦喖孝闁诲繆鏅滈〃銉╂倷閺夋垵顫掗梺鍝勭焿缁查箖骞嗛弮鍫濈伋闁告劘灏欢鐔兼⒒娴ｅ懙褰掝敄閸℃稑绠查柛銉墮缁犳牗淇婇妶鍛櫤闁哄懏鐓￠獮鏍垝閸忓浜鹃柟棰佺劍妤犲嫰姊婚崒娆愵樂缂侀硸鍠氱划濠氬箣閿旇棄浜遍梺瑙勫礃椤曆呯不閺嶃劋绻嗛柕鍫濇噹閺嗙偤鏌涢悩鍙夘棦闁哄本鐩鎾Ω閵夈儺娼炬俊鐐€х拹鐔煎礉閹达箑钃熼柨婵嗩槸椤懘鏌嶆潪鎷屽厡濞寸媭鍙冮弻锝夊閳轰胶浠梺鍝ュУ閻楃娀鐛崘顭戞建闁逞屽墴閻涱噣骞掗幊铏⒐閹峰懘宕崟顐ゎ啈闂傚倸鍊烽懗鍓佸垝椤栫偛绀夐柡鍥ュ焺閺佸銇勯幘鍗炵仼闁绘挻绻堥弻鐔煎礈瑜忕敮娑㈡煟閹惧啿鏆熼柟鑼归オ浼村醇濠靛牜妲舵繝鐢靛仜濡瑩骞愰幖浣哥＜闁靛ň鏅滈悡娑氣偓骞垮劚閸燁偅淇婇崸妤佺厽婵犻潧妫涢崺锝夋煛瀹€瀣瘈鐎规洖鐖兼俊鐑藉Ψ瑜岄惀顏堟⒒娴ｇ懓鈻曢柡鈧柆宥呭瀭闁秆勵殔閽冪喐绻涢幋鐐冩岸寮ㄦ禒瀣厱闁斥晛鍠氬▓銏犆圭亸鏍т壕闂傚倸鍊风粈渚€鎮块崶顒夋晪鐟滄棁妫熼梺姹囧灮鏋柦鍐枑缁绘盯骞嬮悙鑼姲闂佺ǹ顑嗛幑鍥极瀹ュ纭€闁绘劕寮跺▓銊︾節閵忥綆娼愭繛鑼枎椤繑绻濆顒傦紲濠电偛妫欑敮鎺楀储閿熺姵鐓涢悗锝呭悁缁ㄥジ鏌曢崶褍顏鐐差儏閳规垿宕堕懜纰夌礂濠德板€楁慨鐑藉磻閻愬搫绀夋繛鍡樻尵瀹撲線鏌″鍐ㄥ闁荤喓鎳撻埞鎴︽偐閹绘帩浼€闂佸磭顑曢崐鏍崲濠靛鍋ㄩ梻鍫熺▓閺嬪懎鈹戦悙鏉垮皟闁搞儴鍩栭弲鐐烘⒑閸︻厼顣兼繝銏★耿瀵憡绗熼埀顒勫蓟濞戙垹鐒洪柛鎰典簴婵洭姊虹紒妯诲鞍婵炶尙鍠栭獮鍐潨閳ь剟骞冨▎鎾崇闁瑰搫妫欑€垫牠姊绘担鑺ャ€冮柣鎺炵畵閹囨偐鐠囪尪鎽曢梺鍝勬川閸犳挾绮绘ィ鍐╃厽闁逛即娼ф晶顖炴煟濠靛洦绀嬫慨濠冩そ楠炲酣鎳為妷锔芥闂備焦鎮堕崝灞筋焽閳ユ剚鍤曢悹鍥ㄧゴ濡插牊淇婇鐐存暠闁哄倵鍋撻梻鍌欒兌绾爼宕滃┑瀣ㄢ偓鍐疀閺傛鍤ら梺绋挎湰閸戠懓銆掓繝姘厪闁割偅绻堥妤€霉濠婂啫鈷旂紒杈ㄥ浮閹晠鎼归銏紦闁诲孩顔栭崰娑㈩敋瑜旈崺銉﹀緞閹邦剦娼婇梺缁橆焽閺佺ǹ顭囨径鎰拻濞撴埃鍋撻柍褜鍓涢崑娑㈡嚐椤栨稒娅犻悗鐢电《閸嬫挾鎲撮崟顒傤槬缂傚倸绉撮敃銈夋偩妞嬪海鐭欓悹鍥╁亾濡啫鐣峰鈧俊姝岊槹闁稿锕濠氬磼濞嗘埈妲梺纭咁嚋缁绘繂鐣峰┑瀣櫇闁稿本姘ㄩ悰銉╂⒑閸濆嫮鈻夐柛娆忕箳缁辩偤寮介鐔哄幈濠电偞鍨堕…鍥箺閻樼粯鐓熼柟鎯х－缁犲鏌＄仦鍓ф创妞ゃ垺娲熼弫鎰板炊閳哄啯姣囧┑锛勫亼閸婃垿宕曢幎钘夌；闁靛牆顦拑鐔哥箾閹寸偟鎳勯柛搴ｅ枑閵囧嫰寮▎鍓у悑闂佺ǹ顑嗛〃鍛村煘閹达富鏁婄痪顓㈡敱閺佹儳鈹戦敍鍕粧缂侇喗鐟╅悰顔界節閸ャ劍娅㈤梺缁樏顒冾樄闁哄本绋戦埥澶愬础閻愬浜鹃梻渚€鈧偛鑻晶顖炴煕鎼淬垹鈻曢柛鈺冨仱楠炲鏁傞挊澶夋睏婵＄偑鍊栧Λ渚€宕戦幇顓狀浄濡わ絽鍟埛鎴︽煙缁嬪灝顒㈢痪鐐倐閺屾盯濡搁妶鍥╃厯闂佽鍠曢崡鎶姐€佸璺虹劦妞ゆ巻鍋撻柣锝囧厴瀹曪繝鎮欓埡鍌ゆ綌婵犵數鍋涘Λ妤冩崲閸℃稑纾婚柟鍓х帛閸婂鏌ら幁鎺戝姢闁告ü绮欏娲偡闁箑娈堕梺绋匡攻閸ㄧ敻锝炲┑鍫熷磯闁惧繗顕栭崬鐢电磽閸屾艾鈧兘鎮為敃鍌椻偓锕傚炊閳哄啩绗夋俊銈忕到閸燁偆鐥閺屾盯顢曢敐鍥╃暤闂佹娊鏀卞Λ鍐蓟閻旂⒈鏁婄紒妤€鎼禒顕€姊洪崫鍕効缂傚秳绶氬畷娲礋椤栨氨顦ㄩ梺鍐叉惈閸燁偄顕ｉ搹顐＄箚闁绘劦浜滈埀顑惧€濆畷銏ゆ偂楠炵喐妞介、姗€濮€閵忕姷銈﹂梻浣规偠閸庢椽宕滃璺虹；閻庯綆鍠楅悡鏇熴亜閹邦喖孝闁诲繆鏅滈〃銉╂倷閼碱剙鈪靛┑顔硷攻濡炰粙骞冮悜钘夌骇闁圭ǹ楠搁埀顒夊灦濮婄儤瀵煎▎鎴犘ｆ繛瀛樼矤娴滄粓鎮炬搴㈠厹闁告侗鍋傚Ч妤呮⒑閸︻厼鍔嬮柛銊ョ秺瀹曘垹煤椤忓應鎷洪梺鍛婄箓鐎氼厼顔忓┑瀣厱闁挎繂鍟崝姘舵煃缂佹ɑ灏扮紒鍌涘笧閳ь剨缍嗛崑鍡涘储娴犲鈷戠憸鐗堝笒娴滀即鏌涢妸銉ｅ仮鐎规洜鏁搁埀顒婄秵娴滆泛顭囬埄鍐︿簻闁规澘澧庣粙濠氭煟閵婏箑鐏╃紒杈ㄥ笚濞煎繘濡搁妷锕佺檨闂備浇顕栭崰鎺楀疾濞戙垺鍋╅柣鎴ｆ閻愬﹪鏌曟繛鍨姎闁诲骏绻濆铏规嫚閹绘帩鍔夊銈嗘⒐閻楃姴鐣烽弶璇炬棃鍩€椤掑嫧鈧箓宕惰閺嬪酣鏌熼悜妯荤叄缂併劎鍏樺濠氬磼濞嗘垹鐛㈤梺閫炲苯澧伴柛瀣洴閹崇喖顢涘☉娆愮彿濡炪倖鐗楃划搴ｅ婵傚憡鐓曢悘鐐靛亾閻ㄦ垿寮崼銉︹拺閻犲洠鈧櫕鐏嶉梺鑽ゅ暱閺呯娀濡存担鑲濇梹鎷呴悷閭︹偓鎾剁磽娴ｅ壊鍎忛柕鍥焽濡叉劙鎮㈢亸浣规杸闂佺粯蓱閸撴岸宕箛娑欑厱闁绘ɑ鍓氬▓婊堟煏閸℃鏆ｇ€规洏鍔戦、姗€宕堕妸褉妲堥柧缁樼墪闇夐柨婵嗙墱濞兼劙鏌ｅ┑鍫濆幋婵﹨娅ｉ幉鎾礋闂堟稒娅涢梻浣告憸婵潧顫濋妸锔剧彾闁哄洢鍨洪崑鍌炲箹閹碱厼鏋ら柛鐐垫暬閺岋綁鎮╅悜妯糕偓鍐偣閳ь剟鏁冮崒姘憋紵闂佽澹嗘晶妤呮偂韫囨稒鐓曢柕澶涚到婵¤法绱掗悩闈涗槐闁哄矉绱曟禒锔惧寲閺囩偘澹曢悗瑙勬礀濞诧箑鈻撻弴銏＄厽閹兼惌鍨崇粔鐢告煕閹惧娲撮柡浣哥Ч閹垽宕妷褜鍟庨梺璇叉捣閺佸憡鐏欏銈呭閹告悂鍩為幋锕€鐏抽柧蹇ｅ亜閺嬬姴螖閻橀潧浠滅紒缁橈耿楠炲啴鍩￠崘鈺佺彴闂佽偐鈷堥崜锕傚船娴犲鈷掑ù锝呮啞鐠愶繝鏌涙惔娑樷偓妤呭箲閵忋倕绀冩い蹇撴椤︻垶姊虹化鏇炲⒉缂佸甯￠崺娑㈠箳濡や胶鍘遍柣蹇曞仜婢т粙骞婇崟顓犵＜濞达絽鎼。濂告煏閸パ冾伃鐎殿喗鎸抽幃銏ゅ传閸曘劍瀚查梺璇叉唉椤煤閹达箑纭€闁瑰墎鏅畵浣逛繆椤栨艾鎮戦悗姘哺閺岀喓绱掑Ο铏诡儌闂侀€炲苯澧婚柛妯兼櫕閹广垹鈽夊锝呬壕闁汇垻娅ラ悷鎵懃濠碉紕鍋戦崐鏍垂閻㈢ǹ绠犻柟閭﹀枛閸ㄦ繂鈹戦悩瀹犲闁圭鍩栭妵鍕箻鐠虹儤鐎婚梺鍝勵儐閸ㄥ灝顫忛搹鍦＜婵妫欓悾鍫曟⒑缂佹﹩娈旀俊顐ｇ〒閸掓帡顢橀悜鍡樺兊闂佺厧鎽滈弫鎼佸储闁秵鐓熼幖鎼灣閸掓澘顭胯濞村嘲顕ｈ閸┾偓妞ゆ帒瀚埛鎴︽煕濠靛棗顏╂い锔诲墴閺屾盯骞樼€靛摜鐣靛銈嗘磸閸庨潧鐣烽悢纰辨晣婵炴垶眉婢规洟姊洪幐搴ｇ畵缂併劌銈搁獮澶嬨偅閸愨晝鍘遍梺闈涚墕閹峰宕曢弮鈧幈銊︾節閸涱噮浠╃紓渚囧枟閻熲晛鐣疯ぐ鎺濇晩闁绘劦鍎烽悙娴嬫斀闁绘劘灏欓幗鐘电磼椤旀儳校缂佸倸绉撮オ浼村醇椤掑倻鈼ら梻濠庡亜濞诧箑顫忛懡銈呭К闁逞屽墮閳规垿顢欓弬銈勭返闂佸憡鎸婚惄顖炲极瀹ュ應鍫柛鎰剁稻閺傗偓婵＄偑鍊栧褰掑几缂佹鐟规繛鎴欏灪閻撴洟鐓崶銊︻棖闁兼媽娉曢埀顒冾潐濞叉﹢宕归崸妤冨祦婵☆垰鍚嬬€氭岸鏌ょ喊鍗炲⒒闁哥喎娲ら埞鎴︽倷閼搁潧娑х紓浣藉紦缁瑩鐛Δ鈧オ浼村醇閻斿憡鐝繝鐢靛仜濡瑩骞愭繝姘厱闁瑰濮甸崰鎰版煟濡も偓閻楀棛绮鑸电叆婵炴垶鑹鹃弸娑㈡煛瀹€瀣М妤犵偞甯￠幃娆撴偨閸偅顔撻梺璇叉唉椤煤濮椻偓瀹曠銇愰幒鎴犲姦濡炪倖甯掗敃锔剧矓閻㈠憡鐓曢柣妯诲墯濞堟粓鏌熼鍡欑瘈鐎殿喗鎸虫慨鈧柨娑樺楠炲秴鈹戦悙宸殶闁告鍥ㄥ仱闁靛ǹ鍎Σ鍫㈡喐閺冨牆钃熼柨娑樺閸嬫捇鏁愭惔鈥茬敖濠电偛寮堕幐鎶藉蓟閳ュ磭鏆嗛悗锝庡墰琚﹀┑鐘殿暯閳ь剙纾崺锝団偓瑙勬礃閸庡ジ藝閸楃儐鐔嗛悷娆忓缁€鍫㈢磼鏉堛劍灏伴柟宄版嚇閹煎綊鎮烽幍顕呭仹缂傚倸鍊峰ù鍥敋瑜斿畷顖涘閺夋垹顔夐梺闈涚箳婵參寮ㄦ禒瀣€甸柨婵嗙凹缁ㄤ粙鏌ㄥ☉娆戠煀闁宠鍨块幃娆撳级閹寸姳妗撻梺钘夊暣娴滃爼寮婚敐鍛斀闁搞儯鍔岄崬澶娾攽椤旂》鍔熺紒顕呭灦楠炲繘宕ㄩ弶鎴濈獩闂傚倸鐗婄粙鎾剁矓閻戞ǜ浜滈柕蹇ョ磿婢х數鈧娲栧畷顒勫煡婢跺á鐔哄枈閸楃偞鏆梻鍌氬€风粈浣虹礊婵犲伣娑氭崉閵婏富鍋ㄩ梺闈涚墕濞层劎绮堟繝鍥ㄧ厸鐎广儱鍟俊璺ㄧ磼閳锯偓閸嬫捇姊婚崒姘偓鎼佹偋婵犲啰鐟规俊銈呮噹绾炬寧绻涢幋娆忕仾闁绘挻绋戦湁闁挎繂鎳忛崯鐐淬亜閵夛絽鐏查柡宀嬬磿娴狅箓宕滆閳ь剚甯掗湁闁绘瑥鎳愰悾鐢碘偓瑙勬礃缁繘藝閹惰姤鐓涢柛娑卞幑閸嬨垺鎱ㄦ繝鍐┿仢婵☆偄鍟湁闁靛鍎虫晶锔锯偓娈垮枟瑜板啴銈导鏉戦唶婵犻潧鐗嗛獮鍫ユ⒒娴ｄ警鏀伴柟娲讳邯濮婁粙宕熼娑樹簵闂佸搫娲ㄩ崰鎾跺姬閳ь剟姊婚崒姘卞缂佸甯¤棢婵犲﹤瀚ㄦ禍婊堢叓閸ラ鍒板褜鍨堕弻鏇㈠炊瑜嶉顓熴亜閵忊剝绀嬫い銏☆殜瀹曟帒饪伴崟顒夊晫闂傚倸鍊风粈渚€骞栭锕€瀚夋い鎺戝閸庡孩銇勯弽銊ュ毈闁搞倖娲橀妵鍕即濡も偓娴滈箖姊虹化鏇熸澒闁稿鎸搁—鍐Χ閸℃鐟ㄩ柣搴㈠嚬閸撶喎鐣疯ぐ鎺戠＜闁绘劕顕崢閬嶆偡濠婂啴鍙勯柕鍡楀暣瀹曠厧鈹戦崼鐔割啌濠电偞鎸婚崺鍐磻閹剧粯鐓涢悘鐐插⒔濞插瓨顨ラ悙鍙夊枠婵☆偄鍟埥澶娾枎閹存粓鍋楁繝纰夌磿閸嬫垿宕愰妶澶婄；闁告洦鍨扮粻鐘虫叏濡炶浜鹃悗娈垮枛椤嘲顕ｉ幘顔碱潊闁绘ǹ顕ч弫瑙勭節閻㈤潧鈻堟繛浣冲厾娲Χ閸涱亝鐎洪梺鎸庣箓濞层劎澹曢挊澹濆綊鏁愰崶銊ユ畬婵犳鍠栭悧蹇曟閹烘柡鍋撻敐搴′簻缂佹う鍥ㄧ厵妤犵偛鐏濋悘鈺呮煃鐟欏嫬鐏╅柍褜鍓ㄧ紞鍡涘磻閸曨剛顩锋繛宸簼閳锋帡鏌涚仦鐐殤濠⒀勭洴閺屾盯骞掗崱妞惧闂傚倷绶氬褏鎹㈤崼銉ョ９闁哄稁鍘肩壕褰掓煕椤垵浜濋柛娆忕箲閹便劌顪冪拠韫婵犵妲呴崑澶娾枖閺囥垺鍤嶉梺顒€绉甸崵宥夋煏婢跺牆鈧绮诲鑸碘拺闁告稑锕﹂幊鍐┿亜閿旇鐏￠柟渚垮妼椤撳ジ宕堕敐鍛濠电偛鐗嗛悘婵嬪几閻斿吋鐓欐慨婵嗘湰閻濐亪鏌熸笟鍨闁糕斁鍋撳銈嗗笒鐎氼參鎮￠崘顔解拺婵炲棙鍎抽悘鐘裁瑰⿰鍫㈢暫婵﹪缂氶妵鎰板箳濠靛浂妫栫紓鍌欑贰閸ｎ噣宕归幎钘夌闁靛繒濮Σ鍫熺箾閸℃ê濮夌紒澶婄埣濮婃椽宕ㄦ繝鍐ㄧ樂闂佸憡鍔戦崝搴ㄥ储閹烘鈷掗柛灞剧懆閸忓本銇勯姀鐙呮敾闁逛究鍔戞俊鍫曞炊瑜嶉悘濠囨煙閼圭増褰х紒韫矙瀹曠懓鈹戠€ｎ偆鍘搁梺鍛婂姂閸斿孩鏅跺☉銏＄厱濠电姴瀚敮娑樓庨崶褝韬い銏＄洴閹瑧鈧稒顭囪ぐ鍝ョ磽閸屾瑩妾烽柛鏂跨焸閳ワ箑鐣￠柇锔界稁濠电偛妯婃禍婵嬎夐崼鐔虹闁硅揪缍侀崫鐑樸亜鎼粹剝顥㈡慨濠傤煼瀹曟帒顫濋澶夋偅闂佽瀛╅崙褰掑礈閻旂厧绠栭柟顖嗗懏娈濋梺瑙勵問閸犳鈻撻弴銏♀拺闂侇偆鍋涢懟顖涙櫠閹绢喗鐓曢柍瑙勫劤娴滅偓淇婇悙顏勨偓鏍暜閹烘绐楁慨姗嗗墻閻掍粙鏌熼柇锕€骞樼紒鐘荤畺閺屾稑鈻庤箛锝嗩€嗛梺鍏兼緲濞硷繝寮婚弴銏犲耿婵°倐鍋撻柡鍡悼閳ь剝顫夊ú蹇涘礉瀹ュ拋鐒介煫鍥ㄧ☉缁€鍫㈡喐韫囨稑鐓熼柕鍫濇缁♀偓闂佹眹鍨藉褍鐡梻浣瑰濞插繘宕愬┑瀣畺鐟滄柨鐣烽崡鐐╂婵炲棗绻嗛崑鎾诲锤濡や胶鍘卞銈庡幗閸ㄥ灚绂嶉弽顬＄懓饪伴崟顓犵厜闂佸搫鐬奸崰鏍х暦椤愶箑绀嬫い鎺戭槹椤ワ絽鈹戦悙鑼憼缂侇喖绉瑰畷鏇㈠箮鐟欙絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯鍩勯崘鍓у姺闂佸疇顫夌粙鎾舵閹捐纾兼慨妯荤樂閿涘嫮纾奸柤鎼佹涧閸濆搫鈹戦垾宕囨憼缂佹鍠栭崺鈧い鎺嗗亾妞ゆ洩缍侀獮姗€顢欑喊杈ㄧ秱闂備焦鏋奸弲娑㈠疮椤栫偞鍋熼柡宥庡幗閳锋帒銆掑锝呬壕闂侀€炲苯澧伴柛瀣洴閹崇喖顢涘☉娆愮彿闂佽顔栭崰姘卞閸忕浜滈柡鍌氱仢閳锋棃鎮介娑氥€掔紒杈ㄥ笧缁辨帒螣閼测晝鏆ゆ俊鐐€ら崑鍛崲閸儯鈧礁顫滈埀顒勫箖濞嗘挸绾ч柛顭戝櫘閸ゆ瑩姊婚崒姘偓鎼佸磹閻戣姤鍊块柨鏇炲€哥粻鏍煕椤愶絾绀€缁剧偓瀵х换婵囩節閸屾粌顣虹紓浣插亾闁告劏鏂傛禍婊堢叓閸ャ劍灏靛褋鍨婚埀顒冾潐濞叉鍒掑畝鍕厴闁硅揪闄勯弲顒勬煕閺囥劌浜楅柡澶樺弮濮婅櫣绮欓崠鈩冩暰闂佹悶鍔岄悘婵嬫偩閻戣棄鍗抽柕蹇娾偓铏吇婵＄偑鍊栫敮濠勭矆娴ｈ鍎熷┑鐘叉处閳锋垿鏌涢幘鏉戠祷濞存粍绻勭槐鎺旀嫚閼碱儷銏°亜椤撶偞鍠樼€殿喕绮欐俊姝岊槾妞ゆ梹娲熼幃妤呮偡閺夋妫岄梺鍝ュУ濞叉粓鎳為柆宥呯缂佹妗ㄧ花濠氭⒑闂堟侗妲堕柛搴ゆ珪閺呰泛鈽夊杈╋紲缂傚倷鐒﹂…鍥虹€涙﹩娈介柣鎰▕閸庢棃鏌熼鐣屾噮闁圭懓瀚粭鐔碱敍濞戣鲸锛堝┑鐘殿暜缁辨洟宕戦幋锕€纾圭憸蹇擃嚗婵犲啰顩烽悗锝庝簽椤旀劕顪冮妶鍡楀Ё缂佺姵鍨块幃鈥愁潨閳ь剟寮婚悢鍛婄秶闁告挆鍛闂備胶绮幖顐ゆ崲濠靛钃熸繛鎴欏灩閻掓椽鏌涢幇鍓佺窗婵炲矈浜炵槐鎾存媴闂堟稑顬嬮梺鍛婎焼閸ワ絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯骞囬埡浣割瀳濡炪値鍓欓悧鎾愁潖濞差亜绠伴幖娣灩椤︹晜绻涚€涙鐭ゅù婊勭箖缁岃鲸绻濋崟銊ヤ壕婵炴垶鐟悞浠嬫煟閻旈绉洪柡灞界Х椤т線鏌涢幘瀵告噰妞ゃ垺宀搁弫鎰緞濡粯娅旈梻渚€鈧偛鑻晶顕€鏌ｉ敐鍥у幋闁诡喒鍓濋幆鏂课熺紒妯绘緫闂備浇顕ч崙鐣岀礊閸℃顩叉繝闈涱儏濮瑰弶銇勮箛鎾跺闁抽攱甯￠弻娑氫沪閸撗勫櫘濡炪倧璁ｇ粻鎾诲蓟閻斿搫鏋堥柛妤冨仒缁ㄥ鏌ｉ幘鍗炩偓婵嬪蓟閺囩喓绠鹃柣鎰靛墯閻濇棃姊哄Ч鍥р偓銈夊闯閿濆钃熸繛鎴炵矤濡茬厧顪冮妶鍐ㄥ闁硅櫕鍔楅崚鎺撶節濮樺吋鏅┑鐐村灦濮樸劑顢欓弴銏″€甸柣鐔告緲椤ュ繘鏌涢悩铏闁奸缚椴哥缓浠嬪川婵犲嫬骞楅梻浣筋潐瀹曟ê鈻斿☉銏犲嚑婵炴垯鍨洪悡娑氣偓鍏夊亾閻庯綆鍓欓崺宀勬煣娴兼瑧绉柡灞剧☉閳规垿宕卞Δ濠佹偅缂傚倷鐒﹂〃蹇涘矗閸愵煈娼栨繛宸簼閸嬪倿骞栫划瑙勵潑婵☆偄鏈换娑氣偓娑欘焽閻倕霉濠婂簼绨绘い鏇悼閹风姴霉鐎ｎ偒娼旈梻渚€娼х换鎺撴叏閻㈡潌澶娾攽鐎ｎ偆鍘介梺缁樏崢鏍嚋椤忓牊鐓曢柡鍌氭健閸欏嫮鈧娲樼换鍫ャ€佸☉銏″€烽柡澶嬪灍閸嬫捇宕归銈囶啎闂佸壊鍋呯换鍕閵忋倖鐓涢悗锝庡墮閺嬫盯鏌″畝鈧崰鏍€佸▎鎾村亗閹煎瓨锚娴滈箖鏌涜椤ㄥ棝宕戝Ο姹囦簻闁哄啫鐗婇弳婊堟煕鐎ｎ偅宕岄柡浣瑰姈閹棃鍩勯崘顏冩喚闂傚倷绀侀幖顐﹀箠韫囨洖鍨濋柟鎹愵嚙閽冪喖鏌ㄥ┑鍡╂Ч闁哄懏鐓￠弻娑樷槈濞嗘劗鍑″銈呮禋閸樿壈鐏冮梺缁橈耿濞佳勭閿曞倹鐓曢柡鍐ｅ亾闁荤喆鍎甸敐鐐剁疀濞戞瑦鍎柣鐔哥懃鐎氼剟鎮垫导瀛樷拺闁革富鍘剧敮娑㈡偨椤栨稑绗掔悮娆撴煙闁箑鏋ょ痪鍙ョ矙閺屾稓浠﹂悙顒傛閻庢稒绻傞埞鎴﹀煡閸℃ぞ绨奸梺鑽ゅ暀閸涱厼鐏婇柣搴秵娴滃爼宕ョ€ｎ亶鐔嗛悹铏瑰皑闊剟鏌涢悙鑼煟婵﹥妞藉畷顐﹀礋椤掆偓缁愭盯姊洪崫銉バ㈤悗娑掓櫇缁顓奸崨鍌涙瀹曘劑顢欑憴鍕伖缂傚倸鍊风粈渚€顢栭崼婵冩灃闁哄洨濮锋稉宥夋煙閹澘袚闁绘挸鍟伴埀顒傛嚀鐎氼厽绔熼崱妯绘珷闁哄洢鍨洪悡鍐磽娴ｈ偂鎴犱焊閹殿喚纾奸柛灞剧☉缁椦囨煃瑜滈崜銊х礊閸℃稑纾婚柛鈩冪☉閸屻劌霉閻樺樊鍎愰柍閿嬪灩閹叉悂鎮ч崼婵堢懆婵炲瓨绮堥崡鎶藉蓟濞戙垹惟闁靛牆鎳庣粊顕€姊虹拠鈥崇仩闁哥喐鎸抽獮鏍亹閹烘垶宓嶅銈嗘尵婵妲愰弻銉︹拻濞达綀娅ｇ敮娑㈡煥濮橆厺绻嗘い鏍ㄧ啲闊剟鏌熼鐟板⒉缂佽桨绮欏畷銊︾節閸曨偄绠伴梺璇查閸樻粓宕戦幘缁樼厓鐟滄粓宕滈悢椋庢殾濞村吋娼欓崘鈧銈嗘尰婢规洟宕戦幘瀛樺缂侇垱娲橀悗濠氭⒑閸︻厼浜炬繛鍏肩懄缁傛帗绺介崨濠傗偓鐢告煕韫囨搩妲稿ù婊堢畺濮婃椽鏌呴悙鑼跺濠⒀勬尦閺岀喖顢欓悷鏉库拤閻庡灚婢樼€氫即鐛崶顒夋晣婵炴垶鐟ラ褰掓⒒閸屾瑦绁扮€规洜鏁诲畷浼村箛椤撶姷褰鹃梺绯曞墲缁嬫帡寮查鍌楀亾楠炲灝鍔氭い锔垮嵆閹繝寮撮姀鈥斥偓鐢告煥濠靛棗鏆欏┑鈥炽偢閺屽秷顧侀柛鎾村哺閹兾旈崘顏嗙厰闁哄鐗勯崝搴ｅ姬閳ь剟姊洪幖鐐插姌闁告柨鐭傞崺鈧い鎺嗗亾妞ゎ厾鍏樺濠氬Χ婢跺﹦鐣抽梺鍦劋閸ㄥ灚鎱ㄩ弴鐐╂斀闁绘劕寮堕崳瑙勪繆椤愶絿銆掓俊鍙夊姍楠炴鈧稒锚椤庢捇姊洪棃鈺佺槣闁告枪鍗遍柛顐ｇ箥濞撳鏌曢崼婵囶棞闁诲繈鍎甸幃妤€顫濋銏犵ギ闂佺粯渚楅崰姘跺焵椤掑﹦绉甸柛鐘愁殜瀹曟劖绻濆顓犲幘闂佽鍘界敮鎺楀礉濡ゅ懏鐓欑€瑰嫰鍋婇悡鍏兼叏婵犲懏顏犵紒顔界懇楠炴劖鎯旈姀鈥愁伆闂傚倷鑳堕崢褏绱炴繝鍕笉闁哄稁鍘奸弰銉╂煃瑜滈崜姘跺Φ閸曨垰绠抽柟瀛樼箥娴煎苯鈹戦埥鍡椾簼闁搞劌婀卞Σ鎰板箳濡ゅ﹥鏅╅梺鍏肩ゴ閺呮盯路閳ь剛绱撻崒娆戝妽妞ゃ劌鐗撳畷浼村冀椤撶偞妲梺閫炲苯澧柕鍥у楠炴帡骞嬪┑鎰磻闁诲氦顫夐幐椋庢濮樿泛钃熼柍銉﹀墯閸氬骞栫划鍏夊亾閸愬樊鍔€闂傚倷娴囧銊х矆娓氣偓瀹曨垶骞橀鑹版憰闂侀潧艌閺呮盯宕￠搹顐＄箚闁靛牆鍊告禍鐐箾鐎涙鐭婂褏鏅Σ鎰板箳濡や礁鈧攱銇勯幒鍡椾壕闂佸憡鏌ｉ崐妤冩閹炬剚鍚嬮柛婊冨暢閸氼偊鎮楀▓鍨灕妞ゆ泦鍥х叀濠㈣埖鍔曢～鍛存煃閸濆嫬鈧懓鈻嶉崶顒佲拻濞达絿鎳撻婊呯磼鐠囨彃鈧瓕鐏嬪┑鐐村灍閹崇偤宕堕鈧敮閻熸粌绻掓竟鏇熺附閸涘﹦鍘介梺閫涘嵆濞佳勬櫠椤曗偓閺屾盯寮拠娴嬪亾濡ゅ啯顫曢柟鐐墯閸氬鏌涘⿰鍐ㄦ殺闁告凹鍋婇幃妤€鈻撻崹顔界亪濡炪値鍘鹃崗妯虹暦鐟欏嫨鍋呴柛鎰╁妿閻も偓濠电偠鎻徊浠嬪箟閿熺姴纾规い鏍仦閳锋垹鐥鐐村櫣濞存粌缍婇幃璺衡槈閺嵮冨Е闂佺硶鏂侀崑鎾愁渻閵堝棗绗掗柛鐕佸亰閹啫煤椤忓懐鍘介梺鍝勭▉閸樻椽骞夐悙顒佸弿濠电姴瀚敮娑㈡煙瀹勭増鍤囩€规洏鍔戝Λ鍐ㄢ槈濮樻瘷銊ヮ渻閵堝啫鐏柣妤冨Т閻ｇ兘宕￠悙鈺傤潔濠碘槅鍨抽埛鍫ュ船閸洘鈷掑ù锝呮憸缁夌儤淇婇銉︾《婵炲棎鍨芥俊鍫曞炊閳哄喚妲搁梻浣告惈缁夋煡宕濇惔锝呭К闁逞屽墴濮婂宕掑鍗烆杸婵炴挻纰嶉〃濠傜暦閺囩偐妲堥柕蹇ョ磿閸樺憡绻涙潏鍓у埌闁硅绻濊棢闁靛繈鍊栭悡娑㈡倶閻愭彃鈷旈柕鍡樺笒闇夐柣娆忔噽閻ｇ數鈧娲樼划蹇浰囬幎鑺ョ厸闁告侗鍘归崑銏℃叏婵犲啯銇濇俊顐㈠暙闇夐柕澶堝劤婢э妇鈧鍠楄ぐ鍐偑娴兼潙閱囨繝闈涚墕楠炲牓姊绘担渚敯闁规椿浜浠嬪礋椤栨稑浜楅梺鍝勬川閸犳挾寮ч埀顒勬⒒閸屾氨澧涚紒瀣浮钘熸繝濠傚娴滄粓鐓崶椋庡埌濠⒀屽灦閺屾洟宕惰椤忣厽銇勯姀鈩冪妞ゃ垺顨婂畷鎺戔堪閸曨剦鍟岄梻鍌氬€风粈渚€骞栭锕€瀚夋い鎺戝閸庡孩銇勯弽銊ュ毈闁搞倖娲橀妵鍕即濡も偓娴滈箖姊虹化鏇熸澒闁稿鎸搁—鍐Χ閸℃鐟ㄩ柣搴㈠嚬閸撶喎鐣疯ぐ鎺戠＜闁绘劕顕崢閬嶆偡濠婂啴鍙勯柕鍡楀暣瀹曠厧鈹戦崼鐔割啌濠电偞鎸婚崺鍐磻閹剧粯鐓涢悘鐐插⒔濞插瓨顨ラ悙鍙夊枠婵☆偄鍟埥澶娾枎閹存粓鍋楁繝纰夌磿閸嬫垿宕愰妶澶婄；闁告洦鍨扮粻鐘虫叏濡炶浜鹃悗娈垮枛椤嘲顕ｉ幘顔碱潊闁绘ǹ顕ч弫瑙勭節閻㈤潧鈻堟繛浣冲厾娲Χ閸涱亝鐎洪梺鎸庣箓濞层劎澹曢挊澹濆綊鏁愰崶銊ユ畬婵犳鍠栭悧蹇曟閹烘柡鍋撻敐搴′簻缂佹う鍥ㄧ厵妤犵偛鐏濋悘鈺呮煃鐟欏嫬鐏╅柍褜鍓ㄧ紞鍡涘磻閸曨剛顩锋繛宸簼閳锋帡鏌涚仦鐐殤濠⒀勭洴閺屾盯骞掗崱妞惧闂傚倷绶氬褏鎹㈤崼銉ョ９闁哄稁鍘肩壕褰掓煕椤垵浜濋柛娆忕箲閹便劌顪冪拠韫婵犵妲呴崑澶娾枖閺囥垺鍤嶉梺顒€绉甸崵宥夋煏婢跺牆鈧绮诲鑸碘拺闁告稑锕﹂幊鍐┿亜閿旇鐏￠柟渚垮妼椤撳ジ宕堕敐鍛濠电偛鐗嗛悘婵嬪几閻斿吋鐓欐慨婵嗘湰閻濐亪鏌熸笟鍨闁糕斁鍋撳銈嗗笒鐎氼參鎮￠崘顔解拺婵炲棙鍎抽悘鐘裁瑰⿰鍫㈢暫婵﹪缂氶妵鎰板箳濠靛浂妫栫紓鍌欑贰閸ｎ噣宕归幎钘夌闁靛繒濮Σ鍫熺箾閸℃ê濮夌紒澶婄埣濮婃椽宕ㄦ繝鍐ㄧ樂闂佸憡鍔戦崝搴ㄥ储閹烘鈷掗柛灞剧懆閸忓本銇勯姀鐙呮敾闁逛究鍔戞俊鍫曞炊瑜嶉悘濠囨煙閼圭増褰х紒韫矙瀹曠懓鈹戠€ｎ偆鍘搁梺鍛婂姂閸斿孩鏅跺☉銏＄厱濠电姴瀚敮娑樓庨崶褝韬い銏＄洴閹瑧鈧稒顭囪ぐ鍝ョ磽閸屾瑩妾烽柛鏂跨焸閳ワ箑鐣￠柇锔界稁濠电偛妯婃禍婵嬎夐崼鐔虹闁硅揪缍侀崫鐑樸亜鎼粹剝顥㈡慨濠傤煼瀹曟帒顫濋澶夋偅闂佽瀛╅崙褰掑礈閻旂厧绠栭柟顖嗗懏娈濋梺瑙勵問閸犳鈻撻弴銏♀拺闂侇偆鍋涢懟顖涙櫠閹绢喗鐓曢柍瑙勫劤娴滅偓淇婇悙顏勨偓鏍暜閹烘绐楁慨姗嗗墻閻掍粙鏌熼柇锕€骞樼紒鐘荤畺閺屾稑鈻庤箛锝嗩€嗛梺鍏兼緲濞硷繝寮婚弴銏犲耿婵°倐鍋撻柡鍡悼閳ь剝顫夊ú蹇涘礉瀹ュ拋鐒介煫鍥ㄧ☉缁€鍫㈡喐韫囨稑鐓熼柕鍫濇缁♀偓闂佹眹鍨藉褍鐡梻浣瑰濞插繘宕愬┑瀣畺鐟滄柨鐣烽崡鐐╂婵炲棗绻嗛崑鎾诲锤濡や胶鍘卞銈庡幗閸ㄥ灚绂嶉弽顬＄懓饪伴崟顓犵厜闂佸搫鐬奸崰鏍х暦椤愶箑绀嬫い鎺戭槹椤ワ絽鈹戦悙鑼憼缂侇喖绉瑰畷鏇㈠箮鐟欙絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯鍩勯崘鍓у姺闂佸疇顫夌粙鎾舵閹捐纾兼慨妯荤樂閿涘嫮纾奸柤鎼佹涧閸濆搫鈹戦垾宕囨憼缂佹鍠栭崺鈧い鎺嗗亾妞ゆ洩缍侀獮姗€顢欑喊杈ㄧ秱闂備焦鏋奸弲娑㈠疮椤栫偞鍋熼柡宥庡幗閳锋帒銆掑锝呬壕闂侀€炲苯澧伴柛瀣洴閹崇喖顢涘☉娆愮彿闂佽顔栭崰姘卞閸忕浜滈柡鍌氱仢閳锋棃鎮介娑氥€掔紒杈ㄥ笧缁辨帒螣閼测晝鏆ゆ俊鐐€ら崑鍛崲閸儯鈧礁顫滈埀顒勫箖濞嗘挸绾ч柛顭戝櫘閸ゆ瑩姊婚崒姘偓鎼佸磹閻戣姤鍊块柨鏇炲€哥粻鏍煕椤愶絾绀€缁剧偓瀵х换婵囩節閸屾粌顣虹紓浣插亾闁告劏鏂傛禍婊堢叓閸ャ劍灏靛褋鍨婚埀顒冾潐濞叉鍒掑畝鍕厴闁硅揪闄勯弲顒勬煕閺囥劌浜楅柡澶樺弮濮婅櫣绮欓崠鈩冩暰闂佹悶鍔岄悘婵嬫偩閻戣棄鍗抽柕蹇娾偓铏吇婵＄偑鍊栫敮濠勭矆娴ｈ鍎熷┑鐘叉处閳锋垿鏌涢幘鏉戠祷濞存粍绻勭槐鎺旀嫚閼碱儷銏°亜椤撶偞鍠樼€殿喕绮欐俊姝岊槾妞ゆ梹娲熼幃妤呮偡閺夋妫岄梺鍝ュУ濞叉粓鎳為柆宥呯缂佹妗ㄧ花濠氭⒑闂堟侗妲堕柛搴ゆ珪閺呰泛鈽夊杈╋紲缂傚倷鐒﹂…鍥虹€涙﹩娈介柣鎰▕閸庢棃鏌熼鐣屾噮闁圭懓瀚粭鐔碱敍濞戣鲸锛堝┑鐘殿暜缁辨洟宕戦幋锕€纾圭憸蹇擃嚗婵犲啰顩烽悗锝庝簽椤旀劕顪冮妶鍡楀Ё缂佺姵鍨块幃鈥愁潨閳ь剟寮婚悢鍛婄秶闁告挆鍛闂備胶绮幖顐ゆ崲濠靛钃熸繛鎴欏灩閻掓椽鏌涢幇鍓佺窗婵炲矈浜炵槐鎾存媴闂堟稑顬嬮梺鍛婎焼閸ワ絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯骞囬埡浣割瀳濡炪値鍓欓悧鎾愁潖濞差亜绠伴幖娣灩椤︹晜绻涚€涙鐭ゅù婊勭箖缁岃鲸绻濋崟銊ヤ壕婵炴垶鐟悞浠嬫煟閻旈绉洪柡灞界Х椤т線鏌涢幘瀵告噰妞ゃ垺宀搁弫鎰緞濡粯娅旈梻渚€鈧偛鑻晶顕€鏌ｉ敐鍥у幋闁诡喒鍓濋幆鏂课熺紒妯绘緫闂備浇顕ч崙鐣岀礊閸℃顩叉繝闈涱儏濮瑰弶銇勮箛鎾跺闁抽攱甯￠弻娑氫沪閸撗勫櫘濡炪倧璁ｇ粻鎾诲蓟閻斿搫鏋堥柛妤冨仒缁ㄥ鏌ｉ幘鍗炩偓婵嬪蓟閺囩喓绠鹃柣鎰靛墯閻濇梻绱掗悙顒€鍔ゆい顓犲厴瀵鏁愭径濠冾棟闂佸壊鐓堥崳顔嘉涚仦瑙ｆ斀闁绘劘灏欐晶鏃傜磼椤旂晫鎳囩€殿喛顕ч埥澶愬閻樻牓鍔嶉妵鍕籍閸屾艾浠樺銈庡亽娴滎亜顫忓ú顏勭闁兼亽鍎查弳鐘绘⒑閹肩偛濡兼繝鈧崡鐐嶏綁骞囬弶璺啋闁诲孩绋掗敋妞ゅ孩鎸荤换婵嗏枔閸喗鐏嶉梺绯曟櫅閻楀﹦绮嬪澶樻晜闁割偅绻勯悾鍝勨攽椤斿浠滈柛瀣尭閳规垿鏁嶉崟顐㈠箣闂佽桨绀侀崐濠氬箲閸曨垰惟鐟滃繘寮抽弶搴撴斀闁挎稑瀚禍濂告煕婵犲啰澧电€规洘绻嗛ˇ瀵哥磼鏉堚晛浠辩€规洖宕—鍐磼濡棿绨撮梻鍌欑閹碱偄煤閵忋倕鍨傛繛宸簼閸嬪倹绻涢幋娆忕仾闁绘挻娲樼换娑㈠箣濠靛棜鍩炲Δ鐘靛仦閿曘垽寮诲☉銏犵厸閻庯綆鍓涜摫婵＄偑鍊戦崝濠囧磿閻㈢ǹ绠栨繛鍡樺姈缂嶅洭鏌熺憴鍕闁稿鎹囬獮鎰償濞戞鐩庨梻渚€娼ф蹇曟閺囥垹绠犻柛銉墯閻撴瑦銇勯弮鍌氬付闁抽攱鏌ㄩ湁闁绘瑥鎳愰悾鐢告煙缁嬪尅鏀荤€垫澘瀚悾婵囩節閸屾稒姣嗗┑鐘垫暩婵兘寮崨濠冨弿闁绘垼妫勯弸渚€鏌熼梻瀵割槮缁炬儳顭烽弻锝呂熷▎鎯ф缂備胶濮撮悘姘跺Φ閸曨喚鐤€闁圭偓鎯屽Λ銈囩磽娴ｆ彃浜炬繝鐢靛Т濞诧箓鎮￠崘顏呭枑婵犲﹤鐗嗙粈鍫熸叏濡寧纭惧鍛存⒑閸涘﹥澶勯柛銊ャ偢瀵偊宕橀鐣屽帾闂婎偄娲ら敃銉モ枍閸℃瑧绠旈柟杈鹃檮閳锋帒霉閿濆妫戝☉鎾瑰皺缁辨帡鐓鐘电厯閻庤娲橀悡鈥愁嚕婵犳艾唯闁挎洍鍋撳ù鐘靛帶閳规垿鎮╃紒妯婚敪濡炪倖鍨甸幊姗€骞冮悙鍝勫瀭妞ゆ劗濮崇花濠氭⒑閸︻厼鍔嬮柛銊ф暬椤㈡棃鍩￠崨顔惧幗闂佸湱鍎ら崹瑙勭濞戙垺鐓忛柛銉戝喚浼冨Δ鐘靛仦鐢帡顢樻總绋跨倞妞ゅ繐妫涘畷鍫曟⒒閸屾瑧顦﹂柟鑺ョ矒瀹曠増鎯旈敐鍡楀簥濠殿喗銇涢崑鎾垛偓娈垮枦椤曆囧煡婢舵劕顫呴柍鈺佸暞閻濇娊姊虹涵鍛汗閻炴稏鍎靛畷婊冣攽鐎ｎ偄浠у┑鐘绘涧椤戝棝宕愰悽鍛婂仭婵炲棗绻愰顏嗙磼閳ь剟宕橀鍡欙紲濡炪倖妫佹慨銈呯暦鐏炵虎娈介柣鎰絻閺嗭絽鈹戦鐟颁壕闂備線娼ч悧鍡椢涘畝鍕鐟滅増甯楅埛鎴犳喐閻楀牆绗掑ù婊€鍗抽弻娑㈡偐閸愭彃鎽甸梺璇″枤閸嬫捇濡堕敐澶婄闁冲湱鍋撶€氳棄鈹戦悙鑸靛涧缂佸弶瀵ч悘娆忣渻閵堝啫鍔氱紒缁橈耿瀵鈽夊锝呬壕闁挎繂绨肩花濂告煕閿濆懐绉洪柟顕嗙節婵¤埖寰勭€ｎ剙骞愰柣搴＄畭閸庤鲸顨ラ幖浣哄祦婵☆垰鐨烽崑鎾舵喆閸曨剙鐭紓浣藉煐瀹€绋款嚕婵犳艾鍗抽柨娑樺閺夊憡绻濋悽闈涗沪闁稿氦娅曠粋宥嗐偅閸愨晝鍘剧紒鐐緲瀹曨剚鏅舵导瀛樼厽闁挎繂顦遍悾鐑樻叏婵犲嫮甯涚紒妤冨枛閸┾偓妞ゆ巻鍋撴い顓炴穿椤﹀綊鏌熼銊ユ搐楠炪垺绻涢幋鐑嗙劷妞ゆ柨妫濆娲偡閹殿喗鎲肩紓浣筋嚙缁夋挳鈥﹂崶顒€鍐€闁靛ǹ鍊楃粻姘舵⒑闂堟稓澧曢柟鍐查叄瀵娊鎮㈤搹瑙勶紡闂佽鍨庢担闀愬垝闂備礁鎼惉濂稿窗閺嶎厾宓侀柟鎹愵嚙缁犺櫕绻濋棃娑欘棞濠㈢懓顑夊缁樻媴妞嬪簼瑕嗙紓鍌氱С閻掞妇绮嬪澶婇唶闁哄洨鍋犻幗鏇㈡倵楠炲灝鍔氭繛璇х畵瀹曚即宕卞☉娆戝幈闂佸搫娲㈤崝灞剧閻愯褰掓偂鎼达絿鍔┑顔硷龚濞咃綁骞忛悩璇茬伋闁惧浚鍋嗘禍鍫曟⒒娴ｈ棄鍚归柛鐔锋健瀵彃鈽夐姀鈺傛櫔闂佹寧绻傞ˇ顖炴煁閸ヮ剚鐓涢柛銉㈡櫅娴犙勩亜閹惧瓨銇濇慨濠呮缁辨帒螣閹捐娼愰柕鍥ㄦ楠炴牗鎷呯喊妯轰壕濞达絽澹婂銊╂煃瑜滈崜娆擄綖韫囨梻绡€婵﹩鍓涢敍婊冣攽閻愬弶顥為柛鏃€顨婃俊鍫曞级濞嗙偓瀵岄梺闈涚墕妤犲憡绂嶅⿰鍕╀簻闁挎梻鍋撻弳顒侇殽閻愭彃鏆欓悡銈嗐亜韫囨挸顏╃€规挸妫濆娲濞戞氨鐣鹃梺鍝勬噺缁挸顕ｉ弻銉晜闁割偆鍠庢禒顓炩攽閻樿宸ラ悗姘煎弮钘熼柛顐犲劜閻撴洟鏌曟繛鐐珖闁伙綀娅ｉ埀顒侇問閸ｎ噣宕抽敐鍛殾闁绘挸绨堕弨浠嬫煕椤愵偄浜濈悮銊╂⒒閸屾艾鈧绮堟笟鈧畷顖炲锤濡も偓缁犺銇勯幇鍓佺暠缂佹劖顨嗛幈銊ヮ潨閸℃濮冮悷婊呭鐢帞绮堢€ｎ偁浜滈柡宥冨妿閵嗘帡鏌涘Ο鍦煓闁哄瞼鍠栧畷妤呭礂閻撳骸澹掗梻浣规偠閸斿瞼澹曢鐘插灊闁哄啫鐗婇ˉ鍫熺箾濞ｎ剙鐏柟铏崌閵堫亝瀵奸弶鎴狀槹濡炪倖鎸炬慨鐑藉级娴犲鈷掑ù锝囩摂閸ゅ啴鏌涢悩鎰佹疁闁靛棗鍟换婵嬪炊閵夈垹浜惧ù锝堝€介弮鍫濆窛妞ゆ挾濯Σ瑙勪繆閻愵亜鈧牠宕归崗鍏煎弿闁靛牆娲犻崑鎾愁潩椤戞儳浠┑顔硷攻濡炰粙骞婇敓鐘参ч柛娑卞枤閳ь剟绠栧娲嚒閵堝懏鐎梺绋垮婵炲﹪宕洪埀顒併亜閹哄秶顦﹂柛銈庡墴閺屾盯骞樼€垫悶鈧帡鏌涢幒鎾虫诞鐎规洖銈告俊鐑藉Ψ瑜滃Σ鐗堜繆閻愵亜鈧牕顫忔繝姘柧妞ゆ劧绠戦崒銊╂⒑椤掆偓缁夌敻鍩涢幋锔界厱闁归偊鍓欑痪褔鏌熼姘卞ⅵ闁哄本绋掔换婵嬪礋椤撶偟顐奸梻浣告惈閹冲酣鎮ユ總绋跨畺闁伙絽鑻弸鍫熶繆椤栨繃銆冨瑙勬礋閺岋綁鎮㈤崫銉﹀櫑闁诲孩鍑归崢鍓у垝閸儱绀冩い鏃傛櫕閸橀亶姊洪棃娴ㄥ綊宕愬Δ鍐焾闁绘垼濮ら悡鐔肩叓閸ャ劍鐓ユ繛鎼櫍閺屸€崇暆鐎ｎ剛袦闂佽鍠掗弲婊冾焽韫囨稑鐓涘┑鐘插敪濠婂懐纾藉ù锝嚽归埛鏃堟煣韫囨捇鍙勭€规洏鍨藉畷婊嗩槻闁搞劍绻堥幃褰掑炊瑜庨埢鏇熺箾鐏忔牗娅婇柡灞诲€濆畷顐﹀Ψ椤旇姤鐦滈梻浣哥秺閺€鍗烆渻閽樺娼栧┑鐘宠壘绾惧吋鎱ㄥΟ鍝勮埞妞ゃ倐鍋撶紓鍌氬€风粈渚€顢栭崱娑樼濠电姵鑹鹃悿楣冩煕椤愶絾绀€闁告濞婇弻鏇＄疀閵壯咃紵闂佸憡枪妞村摜鎹㈠┑瀣仺闂傚牊鍒€閵忋倖鐓曞┑鐘插€荤粔铏光偓瑙勬处閸ㄥ爼骞冨▎鎾村仺闁汇垻顣槐鏌ユ⒒娴ｅ懙褰掑嫉椤掆偓椤繈濡搁敂鎯р叞闂傚倸鍊风欢姘焽瑜旈幃褔鎮欓悽鐢殿槸婵炶揪绲介幗婊堝汲閿旇姤鍙忔俊鐐额嚙娴滈箖姊虹化鏇熸珖闁稿鍊濋獮鍐ㄢ枎閹垮啯鏅㈤梺閫炲苯澧存い銏℃閵囨劙骞掗幘顖涘闂備胶枪閺堫剟鎮烽妸鈺佺閻忕偘鍕樻禍婊堟煏韫囨洖顎撻棅顒夊墰閳ь剝顫夊ú姗€宕归崸妤冨祦闁搞儺鍓欑痪褔鎮规笟顖滃帨缂佽精椴哥换婵嬫偨闂堟稐娌梺鍓茬厛閸ㄨ泛鐣风涵鍛汗闁圭儤鎸搁埀顒€娼￠弻娑⑩€﹂幋婵呯按婵炲瓨绮嶇划鎾诲蓟閻旂厧浼犻柛鏇ㄥ帨閵夆晜鐓曢煫鍥ㄦ崄鐎氱増銇勯鈥冲姷妞わ附鎸抽弻鈩冩媴閸撹尙鍚嬮悗娈垮枛椤兘骞冮姀銏犳瀳閺夊牄鍔嶅▍鎾绘⒒娓氣偓濞佳勭仚闂佺ǹ楠稿璺侯嚗閸曨垰绀嬫い鏍ㄧ〒閸橀亶鏌ｆ惔顖滅У闁稿瀚幈銊ヮ吋婢跺鍘搁柣蹇曞仩椤曆囧焵椤掍胶澧垫鐐茬墦婵℃悂鍩℃担铏规澑婵＄偑鍊栭崹鐓幬涢崟顒傤洸濡わ絽鍟悡娑㈡煕閵夈垺娅呴崯鎼佹⒑濞茶骞栭柣妤佹崌瀵濡搁埡浣稿祮闂佺粯鍔栫粊鎾磻閹惧箍浜归柟骞垮€曢ˇ鎵崲濠靛绀冮柨鏃囨硶閻╁酣姊绘担绛嬫綈濠㈢懓顑夊鎻掆槈閵忊€斥偓鑸点亜韫囨挸顏撮柣鏂挎閹綊宕崟顒佸創濡炪倕娴氶崑鍡欐閹烘挻缍囬柕濞垮劤椤戝倿姊洪柅鐐茶嫰婢у墽绱撳鍛棦鐎规洘鍨垮畷鐔碱敃閳ь剝銇愰幒婵囨櫓闂備焦顑欓崹鐗堢妤ｅ啯鍋℃繛鍡楃箰椤忣亞绱掗埀顒勫礃椤旂晫鍘遍梺鍝勫暊閸嬫捇鏌ｅΔ浣虹煉妤犵偞鍨挎慨鈧柣姗嗗亝閺傗偓闂備胶纭跺褔寮插⿰鍫濈＝闂傚牊渚楀〒濠氭煏閸繃顥為悘蹇涙涧閳规垿顢欓懞銉ュ攭闂佺粯渚楅崰鏍綖濠婂牆鐒垫い鎺嗗亾妞ゎ偄绻掔槐鎺懳熺拠宸偓鎾剁磽娴ｅ湱鈽夋い鎴濇噹閳绘捇骞囬悧鍫氭嫼闂佸憡绋戦オ鎾倿娴犲鐓涢柛婊€绀佹禍鎵偓娈垮櫘閸嬪﹪銆佸璺虹劦妞ゆ帒瀚畵渚€鏌熼悜妯烘闁哄啫鐗嗛悞鍨亜閹烘垵鈧鎯岄崱妤婄唵閻犺桨璀﹂崕鎴犵磼閳锯偓閸嬫挸鈹戦悩鍨毄濠殿喗鎸冲畷鎰板箹娓氬洦鏅滃銈嗗姂閸婃澹曟總鍛婄厽婵☆垰鐏濋崥褰掓煟閿濆棙銇濋柡宀嬬磿娴狅箓宕滆濡插牓姊虹€圭姵顥夋い锕傛涧閻ｇ兘鏁撻悩鍐测偓鐑芥煙濞堝灝鏋ゆ繛澶婃健濮婂宕掑▎鎰偘濡炪倖娉﹂崨顔煎簥闂佸綊鍋婇崜锕傚吹閺囥垺鐓曟い鎰剁稻缁€鈧紓浣哄Х閸嬨倝寮婚弴鐔虹瘈闊洦鎼╁ú顓犵磽娴ｅ弶顎嗛柛瀣崌濮婄粯鎷呴崨濠傛殘闂佸憡鏌ㄧ换妯侯嚕椤掑嫬纾兼繛鎴炲嚬濞村嫰姊洪棃娑氱疄闁稿﹥鐗犲畷鎰版偨閸涘﹤浠┑鐐叉缁绘劙顢旈鍕电唵閻犲搫鎼顓㈡煛鐏炲墽銆掗柍褜鍓ㄧ紞鍡樼瀹勯偊鍟呴柕澶嗘櫆閻撶喖鏌ㄥ┑鍡樻悙闁告ɑ鎸抽弻鐔碱敊閸濆嫮浼勯梺鍝ュТ閿曘儱顭囪箛娑樜╅柨鏃傛櫕瑜板洭姊婚崒姘偓鐑芥嚄閸撲礁鍨濇い鏍嚤濞戞瑦濯撮柟鑲╁亹閸嬫捇骞掗弮鈧崰鍡涙煕閺囥劌骞樼憸浼寸畺濮婃椽宕崟顒€鍋嶉梺鎼炲妽濡炰粙宕哄☉銏犵闁挎梻鏅崢閬嶆⒑閸︻厼浜炬繛鍏肩懅瀵板﹦鎷犵憗浣烘嚀楗即宕熼鐘垫毇缂傚倷鑳剁划顖炴儎椤栨氨鏆﹂柨婵嗘缁剁偤鎮橀悙缂庢帡骞忛垾鏂ユ斀闁绘﹩鍠栭悘杈ㄣ亜椤愩埄妲搁悡銈嗙節闂堟侗鍎忓鍛存⒑閸︻厼顣兼繝銏☆焽缁骞庨懞銉у幍闂佺顫夐崝鏍兜閻愵剦娈介柣鎰綑婵秹鏌″畝瀣ɑ闁诡垱妫冮弫宥夊礋椤撶喐顔嗛梻鍌欒兌鏋慨姗堥檮閵囨棃骞栨担纰樺亾閸愵喖唯闁宠桨绀佺粣娑欑節閻㈤潧孝閻庢艾鍢茬叅闁归棿鐒﹂埛鎴︽偡濞嗗繐顏╂い銉ヮ儏椤儻顦虫い銊ワ躬楠炲啴鏁撻悩鎻掑祮闂侀潧绻嗛埀顒佹灱閸嬫捇鎮介崨濠勫弳濠电娀娼уΛ婵嬵敁濡も偓闇夋繝濠傚缁犵偤鏌熼瑙掑湱绮诲☉銏℃櫜闁告侗鍘鹃鎴︽⒒娓氣偓濞佳兠洪妶鍥ｅ亾濮橆偄宓嗛柕鍡曠窔瀵挳濮€閳╁啯鐝抽梻浣规偠閸庮噣寮插┑瀣骇闁兼亽鍎扮换鍡涙煟閹板吀绨婚柍褜鍓氶崹鍨暦閻熸噴娲敂閸屾浜鹃柛鎰ㄦ櫇缁♀偓闂佺ǹ鏈划宥呪枔妤ｅ啯鈷戦柛锔诲幖閸斿鏌涢妸褍鏋涚€殿喓鍔戦弫鍐磼濞戞艾骞楅梻浣筋潐濠㈡﹢宕ラ埀顒傜磼閵婎煈鍤欓柍瑙勫灴椤㈡瑩鎮℃惔妯轰壕鐟滅増甯掔粻鐐烘煏婵犲繐顩紒鈾€鍋撻梻浣告啞閸斿繘寮崒娑氼浄闁靛繈鍊栭崐鐢告偡濞嗗繐顏璺哄閺屾稓鈧綆浜峰銉╂煟閿濆洤鍘撮柟顔哄灮閸犲﹥娼忛妸锔界彣闂傚倷绀侀幉鈩冪瑹濡ゅ懎绐楁繛鎴欏灩閻ゅ墽鎲搁弬娆炬綎濠电姵鑹剧壕鍏肩箾閸℃ê鐒炬俊宸櫍濮婅櫣鎲撮崟顓滃仦闂侀€炲苯澧柛鎾磋壘椤洭寮介锝呮瀾闂佺粯顨呴悧鍡欑箔閹烘鐓曢柕濞垮劚閳ь剙娼″璇测槈濠婂孩歇婵＄偑鍊戦崝宀勬偋閸℃稒鍋╅柛顭戝亞閻熷綊鏌嶈閸撶喎顕ｇ拠娴嬫闁靛繒濮烽悿鈧梻鍌氬€搁悧濠勭矙閹烘鍊堕柛顐ｇ箥濞撳鏌曢崼婵嬵€楀ù婊勭矒閺岀喖鎼归顒冣偓璺ㄢ偓娈垮枛椤兘寮幇鏉垮窛闁稿本绮岄弸娑㈡煙閻撳海绉洪柡灞芥噹閳藉顫濋崹娑欐暯濠电姷鏁告慨鐑藉极閹间礁纾绘繛鎴欏灪閸嬪鈹戦悩鎻掝仾鐎规洘鐓￠弻娑㈩敃閿濆洨顓煎┑鐐插悑閻楁粎妲愰幘瀛樺閻犲浄绱曢崝鐑芥⒑閼姐倕鏋庣紓宥咃躬瀵鈽夐姀鐘栥劑鏌熺€涙绠撻柡鍡樻礈缁辨帗娼忛埡浣圭亪濠殿喖锕ら…宄扮暦閹烘埈娼╂い鎴ｆ娴滈箖鏌涘┑鍕姉闁稿鎸搁～婵嬫偂鎼粹槅娼鹃梻渚€鈧偛鑻晶顖炴煟椤撗冩珝鐎规洩绻濆畷妯侯啅椤斿吋顓挎俊鐐€栭崝鎴﹀春閸曨垰瑙﹂悗锝庡枟閻撴洟鏌嶉埡浣告殶闁瑰啿鎳橀弻鐔兼惞椤愩倗鐤勫┑顔硷攻濡炰粙寮婚崨瀛樺€烽柤鑹版硾椤忣厽绻濋埛鈧仦鑺ョ亶闂侀潧娲ょ€氫即銆侀弴銏狀潊闁靛繈鍩勯崬铏圭磽閸屾瑦绁板鏉戞憸閺侇噣骞掗弴鐘辫埅闂備浇宕垫慨鏉懨洪妶鍛傜喐绻濋崶褏鍔﹀銈嗗笂閻掞箑鐣风仦鐐弿濠电姴鍟妵婵嬫煙缁涘湱绡€濠碘€崇埣瀹曘劑顢欓崣銉╁仐闂傚倸鍊烽懗鑸电仚濡炪倖鍨甸幊姗€寮崘顕呮晜闁告洦鍘藉▓楣冩⒑濮瑰洤鐏い锝勭矙瀹曟垿骞樼紒妯绘珳闁硅偐琛ラ埀顒佸墯濞煎墽绱撴担鍝勪壕鐎规洘锚铻為柛鏇ㄥ灡缁犳帡姊绘担鐟邦嚋缂佽鍊哥叅闁靛ň鏅涚粻娲煕瀹€鈧崑鐐烘偂閺囥垺鐓忛柛顐ｇ箖椤ョ娀鏌熼崘鍙夊枠闁哄矉绱曟禒锕傚礈瑜夋慨鍥╃磽娴ｈ櫣甯涚紒璇茬墕閻ｇ兘骞掑Δ鈧洿闂佹悶鍎滈崟顐熷亾椤栨埃鏀介幒鎶藉磹濡や焦鍙忛柣鎴ｆ绾惧鏌ｉ幇顒佹儓闁搞劌鍊块弻鐔虹矙閸噮鍔夐梺鍛婄懃缁绘﹢寮诲鍫闂佸憡鎸鹃崰鏍ь嚕婵犳艾鍗抽柕蹇曞█閸炶泛鈹戦悩缁樻锭婵炲眰鍊曢埢宥堫槻妞ゎ亜鍟存俊鍫曞幢濡椽鐎烘繝纰夌磿閸嬬姴螞閸曨垽缍栭煫鍥ㄧ⊕閹偤鏌涢敂璇插箻闁挎稒绮岄埞鎴︻敊缁涘鐣堕梺缁橆殔閹虫劕鈻庨姀銈嗗仺闁告稑艌閹疯櫣绱撴担鍓插剱妞ゆ垶鐟╁畷鏇＄疀濞戞瑥鈧灚鎱ㄥΟ鐓庡付妤犵偞顭囩槐鎺楊敊绾板崬鍓板銈嗘尭閸氬顕ラ崟顓涘亾閿濆簼绨撮柛瀣崌閺屻劎鈧絻鍔嬬花濠氭⒑閻熺増鎯堢紒澶婄埣钘濋柨鏃堟暜閸嬫挸鈻撻崹顔界亪闂佽绻戠换鍫ュ春閻愬搫绠ｉ柨鏃傜帛閺呪晝绱撴担鍦槈妞ゆ垵妫濋幃鍓ф崉鐞涒剝鏂€闂佸疇妫勫Λ妤佺濠靛洢浜滈柕濞垮劜椤ャ垻鈧鍠楅悡鈥崇暦婵傜ǹ鍗抽柣鎰問閸氬淇婇悙顏勨偓鏍蓟閵婏附娅犲ù鐘差儐閺咁剚绻濇繝鍌氼伀缂佲檧鍋撻梻渚€娼ф蹇曞緤閸撗勫厹闁绘劦鍏欐禍婊堟煙鐎涙绠栨い銉ｅ灩閳规垿鏁撻悩铏敪闂佸疇顫夐崹褰掑焵椤掑﹦绉甸柛瀣瀹曘垽骞栨担鐟扳偓鐢告煕韫囨搩妲稿ù婊堢畺閺岋絾鎯旈婊呅ｉ梺鍛婃尰閻╊垶寮澶嬪亜闁告縿鍎抽鏇㈡煟鎼达絾鏆╅弸顏呫亜鎼淬垺灏柍瑙勫灴閸ㄩ箖鎼归銏＄亷婵°倗濮烽崑娑氭崲濮椻偓楠炲啴鍩￠崘鈺佺彴闂佽偐鈷堥崜锕傚船娴犲鈷掑ù锝呮啞鐠愶繝鏌涙惔娑樷偓妤呭箲閵忋倕绀冩い蹇撴椤︻垶姊虹化鏇炲⒉缂佸甯￠崺娑㈠箳濡や胶鍘遍柣蹇曞仜婢т粙骞婇崟顓犵＜濞达絽鎼。濂告煏閸パ冾伃鐎殿喗鎸抽幃銏ゅ传閸曘劍瀚查梺璇叉唉椤煤閹达箑纭€闁瑰墎鏅畵浣逛繆椤栨艾鎮戦悗姘哺閺岀喓绱掑Ο铏诡儌闂侀€炲苯澧婚柛娆忓暙椤繐煤椤忓懎娈熼梺闈涱槸閸犳碍绂嶉鍥棨闂傚倷绶￠崜娆戠矓閺夋嚩褔寮婚妷锔惧弳闂佸搫娲ㄩ崑娑㈠焵椤掆偓缂嶅﹪骞冮檱缁犳盯骞橀娑欐澑闂備胶绮灙閻忓繑鐟х划濠氭偋閸稐绨婚梺鍐叉惈閿曘倖鏅堕幍顔剧＜閺夊牄鍔屽ù顕€鏌涢埡瀣暤闁糕斁鍋撳銈嗗笒鐎氼剛澹曟繝姘厵闁告挆鍛闂佹娊鏀辩敮鎺楁箒闂佹寧绻傞幊蹇涘疮閻愮儤鐓曢柣鏂挎惈娴狅妇绱掔紒妯笺€掗柟椋庡Ь椤﹁淇婄紒銏犳灓缂佽鲸甯″畷鎺戭潩濮ｆ鍥ㄧ厵妞ゆ梻鏅幊鍥ㄤ繆椤愩垹鏆欓柍钘夘槹濞煎繘鈥﹂幋鐑囩礀闂傚倸鍊搁崐鐑芥嚄閸洩缍栭悗锝庡枛缁€瀣煕椤垵浜為柡鍡畵濮婄粯鎷呴悷鎵虫灆闂佽　鍋撻弶鍫氭櫆閺嗘粓鏌ㄩ悢鍝勑㈤柦鍐枑缁绘盯骞嬮悙鐢靛彎濠电偛鐭堟禍顏堝蓟濞戙垹绠绘俊銈傚亾閻庢凹鍓欓埢宥呂熺拋宕囩畾闂佺粯鍔︽禍婊堝焵椤戭剙鎳忔刊濂告煥濠靛棙顥欑紒璇叉閺屾盯濡烽姀鈩冪彆濡炪値鍋呭ú鐔煎蓟閻斿吋鍊锋い鎺嗗亾濠⒀屽灦閺屾稑顫濋澶婂壎闂佸搫鏈粙鎴︺偑娴兼潙绀冮柕濠忛檮椤旀垿姊绘担鍛婂暈闁告垵缍婂畷鎴炵節閸パ嗘憰闂佹寧娲嶉崑鎾绘懚閿濆鐓熼柟鎯у暱椤掋垽鏌ｈ箛鎾跺ⅵ婵﹥妞藉畷顐﹀礋椤撶姳绱橀梻浣告憸婵敻鎮ч悩缁樺仒妞ゆ洍鍋撴鐐差儔閺佸啴鍩€椤掑倻涓嶆繛鎴欏灪閳锋帡鏌涢銈呮灁闁愁垱娲橀妵鍕棘閸噮浼€缂備浇椴搁幑鍥х暦閹烘垟鏋庨柟鐑樼箓閺佸ジ姊绘担鍛靛綊顢栭崱娑樼闁搞儺鍓欓拑鐔哥箾閹寸偟鐓繛宀婁邯閺屾盯骞樺璇蹭壕婵犳鍠栧ú顓烆潖濞差亜宸濆┑鐘插閸Ｑ呯磽娴ｈ櫣甯涢柛銊ュ缁碍娼忛妸褏鐦堥梺鎼炲劘閸斿矂鍩€椤掆偓閸㈣尪鐏嬮梺缁橆殔閻楀繒绮婚幘瀵哥闁割偒鍋勯獮妤冪磼缂佹銆掗柍褜鍓氱粙鎺椻€﹂崶顒佸剹闁圭儤姊荤壕濂告煟濡櫣锛嶅褍鐏氶〃銉╂倷閹绘帗娈柧缁樼墵閺屽秷顧侀柛鎾跺枛瀹曟椽濮€閵堝懐顔掗柣鐘叉处瑜板啴鎮楅鐑嗘富闁靛牆妫欓埛鎺楁煛閸滀礁浜柕鍥ㄥ姍椤㈡盯鎮欑划瑙勫濠电偠鎻徊浠嬪箠濞嗘帇浜归柟鐑樼箖閺呯偤姊洪崨濠勨槈闁宦板姂瀵彃鈹戦崶銉ょ盎闂婎偄娲﹂幐鐐櫠濞戙垺鐓曢悗锝庡€栭幋锕€桅闁告洦鍨板Λ姗€鏌熺粙鍨槰婵炲吋妫冨铏圭磼濡儵鎷婚梺鍛婎焼閸涱垳鐒奸梺绋跨灱閸嬬偤鎮￠悢鍏肩厵闁硅鍔栭悵顏堟煙閻у摜鎮奸柟鍙夋倐閹囧醇閻斿嘲鏀繝娈垮枛閿曪妇鍒掗鐐茬闁告稒娼欏婵嗏攽閻樻彃鈧懓鈻撳鈧缁樻媴閽樺鎯為梺鍝ュТ濡繂鐣烽弴鐔稿闁告劏鏅╁Λ銊╂⒒閸屾瑦绁板┑顔哄€濋弫瀣⒑閹稿海鈽夋い锔诲灦閸┿儲寰勯幇顒夋綂闂佺偨鍎遍崢鏍姳婵犳碍鈷掑ù锝呮嚈閸︻厽宕查柟鎵閸庡秹鏌ｉ幋锝嗩棄缂佺媭鍨崇槐鎾存媴鐠囷紕鍔风紓浣哄█缁犳牠寮婚悢鍏煎€锋い鎺嶈兌缁嬪洭姊虹紒妯虹瑨缂傚秴锕璇测槈濮橈絽浜鹃柨婵嗛娴滄繈鎮樿箛鏂款棆缂佽鲸甯炵槐鎺懳熺亸鏍ь潓闂備胶鎳撻崲鏌ュ箠濡櫣鏆︽慨妞诲亾鐎规洩绲惧鍕節鎼粹懣鐔兼⒒閸屾瑨鍏岄弸顏呫亜閹存繃鍤囨い銏＄墬瀵板嫰骞囬鍐╂啺婵犵數鍋為崹顖炲垂瑜版帗鍊挎繛宸簼閻撴洟鏌熼弶鍨暢缂佽鲸澹嗙槐鎺楀Ω閳哄倹閿梺瀹狀潐閸ㄥ潡骞冨▎鎾崇骇闁瑰濮冲鎾翠繆閻愵亜鈧倝宕㈡總绋跨９闁割煈鍣崵鏇熴亜閺囨浜鹃悗瑙勬礈閸樠囧煘閹达箑鐐婇柕澶堝€涘绋库攽閻樺灚鏆╅柛瀣█楠炴捇顢旈崱娆戭槸闂侀€炲苯澧柕鍥у椤㈡洟濮€閳哄倐锕傛⒑鐎圭媭鍤欑紒澶屾嚀閻ｇ兘宕￠悙鈺傤潔濠电偛妫楅張顒傗偓姘虫閳规垿鎮欓懜闈涙锭缂備礁寮剁€笛囨倶濞嗗浚娓婚柕鍫濆暙閸旀氨绱掔紒姗堣€跨€殿喛顕ч埥澶婎潩閿濆懍澹曢梺鎸庣箓缁ㄨ偐鑺辨總鍛婄厱闁挎繂鎳愮粔娲煙椤曞棛绡€濠碘€崇埣瀹曞崬螣閸濆嫪绱熷┑鐘殿暜缁辨洟宕戦悩娲绘晪鐟滃海绮嬪鍡愬亝闁告劏鏂侀幏铏圭磽娓氬洤鐏℃繛鍙壝埢宥夊幢濞戞瑧鍘介梺鍐叉惈閿曘倝鎮橀敃鍌涚厽婵炴垵宕弸銈夋煃瑜滈崜銊х礊閸℃稑绐楁俊銈呮噹閻ょ偓绻濇繝鍌滃闁绘挶鍨烘穱濠囶敍濞嗘帩鍔呭┑陇灏欓崗姗€寮诲☉銏犲唨闁靛ě鍐ｆ嫲婵犳鍠栭敃锔惧垝椤栫偛绠柛娑卞灣妞规娊鎮楅敐搴濈凹闁伙絼鍗冲缁樻媴閸涘﹤鏆堝銈庡幖濞差厼顕ｉ弻銉晝闁挎洍鍋撻柡瀣╃劍閵囧嫰寮村Δ鈧禍鎯旈悩闈涗沪閻㈩垽绻濋悰顔碱潨閳ь剟骞婇悩娲绘晢闁稿本绮嶅В搴♀攽閻樺灚鏆╁┑鐐╁亾濠电偘鍖犻崶褏锛欓梺褰掓？缁€浣虹不閻斿吋鐓ラ柣鏂挎惈鏍￠梺绋款儏閸婂湱鎹㈠☉銏犵婵炲棗绻掓禒楣冩⒑缁嬪尅鍔熼柡浣割煼瀵濡搁妷銏℃杸闂佺硶鍓濇笟妤呭焵椤掍緡娈旈棁澶嬬節婵犲倹鍣瑰ù鐘讳憾閺屸€崇暆閳ь剟宕伴弽褏鏆︽慨妞诲亾妤犵偞鎹囬獮鎺戔攽閸涱垳锛為梻鍌氬€风粈渚€骞栭锕€瀚夋い鎺嗗亾閻撱倝鏌ｉ弮鍫闁哄棴绠撻弻鐔碱敍閻愯弓鍠婂┑陇灏褔鈥旈崘顔嘉ч柛娑卞枤椤╀即姊洪崨濞掝亪濡堕幖浣告槬闁绘劕鎼粻锝夋煥閺冨倻甯涢柡鍛櫊濮婃椽鎳￠妶鍛亪濡炪倧缍囬梽鍕博閻旂厧鍗抽柕蹇婃閹峰姊虹粙鎸庢拱闁荤啙鍥х鐎广儱顦伴崐鐢电磼濡や胶鈽夐柟铏姍閹苯鈻庨幘瀵稿幍闁诲海鏁告灙鐞氥儵姊虹粙娆惧剱闁瑰憡鎮傞敐鐐测攽鐎ｎ偄浜楅柟鍏肩暘閸ㄦ槒銇愭惔銊︹拻濞达絿鐡旈崵鍐煕閻樺啿娴€规洘绮岄埞鎴﹀幢閳轰焦顔傞梻浣告啞濞诧箓宕滃☉姘变笉濡わ絽鍟悡蹇涚叓閸パ嶅伐濠⒀冨级閵囧嫰濡烽妷褍鈪甸梺鍝勭灱閸犳牠銆佸▎鎴炲枂闁告洦鍓涜ぐ瀣節绾版ɑ顫婇柛瀣瀹曨垶顢曢敃鈧悡鈥愁熆鐠哄ソ锟犲籍閸繄顦ㄩ梺瀹犳〃閼冲爼藝閺夊簱鏀介柣妯活問閺嗘粎绱掓潏銊︾鐎规洘鍨块獮瀣晝閳ь剛澹曢崷顓熷枑闊洦娲栭崹婵嗏攽閻樺疇澹樼紒鐘差煼閹妫冨☉姗嗘濠电偛寮堕幃鍌氼潖濞差亜绀堥柟缁樺笂缁ㄧ厧鈹戦悙鎻掔骇闁绘濮撮锝嗗閺夋嚦銊╂煃閸濆嫬鈧宕㈤幘缁樺仭婵犲﹤瀚惌鎺斺偓瑙勬礃缁矂锝炲┑鍫熷磯濞达絾娲╃粻鎾诲蓟濞戙垹鍗抽柕濞垮劤娴犫晝绱撴担鍝勑ｅ┑鐐诧躬瀵鎮㈤悡搴ｎ啇濡炪倖鎸鹃崑鐔哥閳哄倻绡€婵炲牆鐏濋弸鐔虹磼鐎ｎ偅灏垫俊鍙夊姍楠炴鈧稒锚椤庢捇姊洪崨濠冨碍鐎殿喛鍩栭〃娆撴⒒閸屾瑦绁版俊妞煎妿缁牊绗熼埀顒勫灳閿曗偓閻ｏ繝鏌囬敃鈧▓銊╂煟閻樺弶澶勭紒浣规綑鍗遍柛顐ゅ枑閸欏繑鎱ㄥΔ鈧悧蹇涘礆閺夋５鐟扳堪閸垻鏆梺鍝勮閸斿矂鍩為幋锕€骞㈡繛鍡楃箰濮ｅ牏绱撻崒娆撴闁告柨娴烽崚鎺楊敍閻愬弬锕傛煕閺囥劌鏋ら柣銈傚亾闂備焦瀵х换鍌炲箠鎼达絿涓嶆繛宸簼閳锋垿姊婚崼鐔衡姇闁瑰吋鍔欓幃妤€顫濋崡鐑嗘毉闂侀€涚┒閸旀垿宕洪埀顒併亜閹烘垵顏柣鎾冲暣閺屽秵娼幍顕呮М闂佸搫顑冮崐鏍ㄧ┍婵犲浂鏁冮柕蹇曞У濞堝姊虹拠鈥虫灍妞ゃ劌鐗撻獮澶愬箻椤旇偐顦板銈嗗笒閸嬪棗危椤掑嫭鈷掑〒姘ｅ亾婵炰匠鍥ㄥ亱闊洦姊绘禒姘繆閻愵亜鈧牕煤閳哄啰绀婂〒姘ｅ亾闁绘侗鍠栭～婊堝焵椤掑嫨鈧礁鈻庨幘鏉戠檮婵犮垼娉涢ˇ閬嶆儎鎼淬劍鈷掗柛灞剧懅椤︼箓鏌熺拠褏绡€闁诡喚鍏樻俊鐑藉煘瑜嶅ú顓€€佸☉銏″€婚柛鈩兩戝▍鍡涙煟閻斿摜鐭屽褎顨堥弫顔嘉旈崨顓犲幈闂佸湱鍎ら崵姘炽亹閹烘挻娅滈梺鍓插亞閸ｃ儱螞閸曨垱鈷戦柛娑橈攻閻ㄦ垿鏌熼搹顐ｅ磳闁糕晝鍋ら獮瀣晜缂佹ɑ娅撻梻浣稿悑缁佹挳寮插▎鎴濆灊闁汇垻鏁哥壕钘壝归敐鍤藉綊鍩€椤掆偓閹芥粎鍒掗弮鍫熷仺闁汇垻鏁搁悞鍧楁⒑閸︻厼鍔嬮柡宀嬬節瀹曟垿骞樼紒妯绘珳闁圭厧鐡ㄧ换鍕极閺嵮屾富闁靛牆绻愰惁婊堟煕閿濆繒绉€殿喖顭烽弫鎰板醇閵忋垺婢戦梻浣告惈濞层劑宕戝鈧畷顐⑽旈崨顔间哗濠殿喗锕╅崢楣冨箠閸ヮ剚鐓涚€光偓鐎ｎ剛锛熼梺閫炲苯澧剧紓宥呮缁傚秹鎮欑€涙ɑ鐝￠梻鍌氬€风粈渚€骞栭锔藉剶濠靛倻枪缁愭鏌″搴″箻鐎规挷鑳堕埀顒€绠嶉崕閬嵥囨禒瀣瀬濠电姴瀚Λ顖炴煙瀹勬壆鐒搁柛婵堝劋閹便劑鏁愰崪浣光枅濠殿喖锕︾划顖炲箯閸涘瓨鎯為柣鐔稿椤愬ジ姊绘担鍛婅础閺嬵亝銇勯鐘插幋妤犵偛鍟撮幃婊堟寠婢跺苯骞愰梻浣告啞娓氭宕ｉ埀顒勬煙閼恒儲绀€闁宠鍨块幃娆撳级閹寸姳鎴烽梻浣规偠閸斿苯锕㈡潏鈺佸灊濠电姵纰嶉弲鏌ユ煕閳╁啰鎳呮い鏃€娲熷铏瑰寲閺囩偛鈷夊銈冨妼閻楀繒鍒掔拠宸僵闁煎摜鏁搁崢閬嶆煟鎼搭垳绉甸柛瀣噹閻ｅ嘲鐣濋埀顒勫焵椤掍緡鍟忛柛鐘虫礈閸掓帒鈻庨幘鎵佸亾娓氣偓瀵挳濮€閳ュ厖绨婚梻浣哄亾閼归箖宕愰幖浣€澶婎潩椤撶姷鐦堝┑鐐茬墕閻忔繈寮搁悢鎼炰簻妞ゆ挾鍋為崰妯尖偓瑙勬礃閸ㄥ潡鐛鈧畷鐟扳槈濡懓顥氭繝娈垮枟椤洭宕戦幘鍨涘亾濮樼偓瀚�?s闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆撴倻濡桨鐢绘繝鐢靛Т閿曘倝宕板顑╃喖鍩€椤掑嫭鍊垫繛鍫濈仢閺嬬喖鏌熼鐓庘偓鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸鐎光偓鐎ｎ剛锛熸繛瀵稿婵″洭宕犻弽顓炲嵆闁绘劖婢橀ˉ姘舵⒒娴ｅ摜绉烘い锝忓缁牊绗熼埀顒勭嵁閸愵喖绠ｉ柨鏃傛櫕閸樼敻姊洪崗鑲┿偞闁哄懐鍋犲Λ銏ゆ⒒娓氣偓閳ь剛鍋涢懟顖涙櫠椤斿墽妫紓浣靛灩瀵喗銇勯姀锛勬噰妤犵偛娲幃褔宕煎顓烆伜婵犵數鍋犻幓顏嗙礊閳ь剚銇勯銏╂█濠碉紕鏁婚幃銏ゅ礂閼测晛骞堟繝纰樻閸ㄦ澘锕㈤柆宥嗗剹婵炲棙鎸婚悡蹇涙煕閳╁喚娈旈柡鍡悼閳ь剝顫夊ú鏍х暦椤掑嫬鐓濋幖娣妼缁犳稒銇勯幒鍡椾壕缂備降鍔婇崕鑼崲濠靛鍋ㄩ梻鍫熺◥濞岊亪姊洪幖鐐插闁绘牕銈稿顐﹀礃椤旇偐鍘搁梺鍛婁緱閸橀箖寮婚崼銉︹拺闁告稑锕ｇ欢閬嶆煕閻樿櫕灏扮紒鍌氱Т楗即宕奸悢鍝勫箰闂備礁鎲￠崝鎴﹀礉瀹ュ绠归柟鎵閻撴洘鎱ㄥ鍡楀缂佸妞介弻锛勪沪閸撗勫垱濡ょ姷鍋涢鍛村煘閹达箑閱囬柍鎯帮骏閸斿矂鍩為幋锔绘晩缁绢厾鍏樼欢鏉戔攽閻愬弶瀚呯紒鎻掓健瀹曟岸骞掗幋鏃€鐎婚棅顐㈡搐閿曘儵鎮楁繝姘拺閻犲洠鈧磭鈧鏌涢幇鐢靛帥婵☆偄锕濠氬磼濮橆兘鍋撻悜鑺ュ殑闁告挷绀侀崹婵囥亜閺嶎偄浜奸柍褜鍓欓崯鏉戠暦婵傚憡鍋勯柧蹇氼嚃閸熷酣姊绘笟鈧埀顒傚仜閼活垱鏅堕娑栦簻闁靛⿵绲介崝锕傛煙椤旂晫鎳囨鐐存崌楠炴帡骞橀幖顓炴暢闂傚倷绀佹晶鐣屽垝椤栫偞鍋嬮柣妯款嚙閽冪喓鈧箍鍎遍悧婊冾瀶閵娾晜鈷戦柛娑橈攻鐏忕敻鏌涢悩宕囧ⅹ闁伙絽鍢茬叅妞ゅ繐瀚€閵娾晜鐓ラ柡鍥殔娴滃墽绱撴担绋款暢闁稿鐩崺鐐哄箣閿旇棄鈧兘姊婚崼鐔衡棩濠㈣娲熷铏规嫚閺屻儺鈧鏌熺喊鍗炰喊闁炽儲妫冨畷姗€顢欓崲澹洦鐓曢柍鈺佸幘椤忓牊鍎婇柣鎴ｅГ閳锋垹绱掔€ｎ厽纭堕柣蹇涗憾閺屾稓鈧綆鍋嗛埊鏇犵磼閸屾稑娴鐐叉喘椤㈡ê鈹戦崼銏㈡毎闂傚倷鑳剁划顖炲礉閿旂晫顩叉繝濠傜墕閻掑灚銇勯幒宥囶槮闁诲繐顕埀顒冾潐濞叉牜绱炴繝鍌滄殾妞ゆ劧绠戠粈瀣箹濞ｎ剙鐏繛鎻掝嚟閳ь剝顫夊ú姗€鏁冮姀鈥茬箚婵繂鐭堝Σ鐓庘攽閻愯尙澧㈤柛瀣尵閹广垹鈹戠€ｎ偒妫冨┑鐐村灦鑿ら柛瀣尵缁辨帒螣闂傚娉婇梻鍌氬€风粈渚€骞夐敓鐘偓鍐幢濡偐顔曟繝鐢靛Т濞层倝鎷戦悢鍏肩叆婵犻潧妫Σ鍛婁繆椤愶綇鑰块柡灞剧洴閳ワ箓骞嬪┑鍥╀壕濠德板€楁慨鏉懨洪銏犺摕闁挎繂妫欓崕鐔兼煏韫囥儳鍒版い銉︾箞濮婃椽妫冨☉姘叡闁哄浜濋妵鍕敃閵忋垻顔囬柣鎾卞€栭妵鍕疀閹炬潙娅ｅ┑鐐茬墑閸庨潧顫忓ú顏咁棃婵炴垶鑹鹃。娲⒑閻熸澘绾ч柣鈺婂灦瀵偊骞囬弶鑳曢梺闈涚墕濞诧箓宕甸埀顒€螖濡ゅ﹣绨烽柛妤佸▕瀹曟椽鍩€椤掍降浜滈柟鐑樺灥閳ь剚鎮傚畷鐟扳攽閸モ晝顔曢梺绯曞墲椤ㄥ牏绮婚幎鑺ョ厸闁糕槅鍙冨顔剧磼缂佹绠炴俊顐㈠暙閳藉鈻庡Ο浼欓獜闂傚倷绀侀幖顐⑽涘Δ鈧灋婵犻潧顑呴拑鐔哥箾閹存瑥鐏╅柛妤佸▕閺屾洘绻涢崹顔煎缂備降鍔岄…宄邦潖濞差亜绠归柣鎰ゴ閸嬫捇寮介鐐茬€梺绉嗗嫷娈旈柦鍐枛閺岋綁寮崹顔藉€梺鎼炲€曢崯鎾蓟瀹ュ浼犻柛鏇ㄥ亽閳ь剚鑹鹃…璺ㄦ崉娓氼垰鍓紓浣筋嚙濡繈寮婚悢铏圭＜婵☆垵娅ｉ悷鏌ユ⒑閹肩偛鈧洖煤椤撱垹钃熼柣鏃傗拡閺佸﹪鏌涘┑鍡楊伀濞寸厧鐗撻幃妤冩喆閸曨剛顦ラ悗瑙勬处閸撶喖宕洪妷锕€绶炲┑鐐靛亾閻庡妫呴銏℃悙婵炲鍏樺顐﹀箚瑜夐弨浠嬫煟濡櫣浠涢柡鍡忔櫊閺屾稓鈧綆鍋嗛埥澶愭懚閻愮繝绻嗛柕鍫濇噹閺嗙偟鈧娲橀悡锟犲蓟閻旂ǹ顕遍柡澶嬪灣閸犲﹪姊烘潪鎵槮闁稿﹤缍婃俊鐢稿礋椤栨氨鐤€闂佸壊鍋呭ú姗€顢撳澶嬧拺缂備焦蓱鐏忎即鏌ｉ悢鍙夋珚鐎殿喛顕ч埥澶愬閻樼數鏉告俊鐐€栧濠氬磻閹惧墎纾奸柣娆愮懃濞诧箓鎮￠弴銏＄厪闁割偅绻冮崳娲煕閻樼鑰块柡宀嬬磿娴狅妇鍖栭弴鐐板閻庤娲栧ú锕€鈻撻弴銏♀拺闂侇偆鍋涢懟顖涙櫠鐎电硶鍋撶憴鍕；闁告濞婇悰顕€宕堕鈧粈鍌涖亜閹扳晛鐏柛鐘筹耿閺岀喖顢欓妸銉︽悙闂佸崬娲弻锝夊棘閸喗些婵犵鍓濇繛濠傤潖缂佹ɑ濯撮柛婵勫劜閻庮喗淇婇悙瀛樼稇闁硅姤绮撳顐︻敋閳ь剟宕洪敓鐘插窛妞ゆ挾濯Σ鑸电節濞堝灝鏋熼弸顏呯箾婢跺鈯曟い銏狅躬濮婄粯鎷呴崨濠傛殘濠电偠顕滅粻鎾崇暦瑜版帗鐒肩€广儱鎳愰弻褍鈹戦悩璇у伐闁绘锕幃锟犲閳ヨ尙绠氬銈嗙墬閻熴劑顢楅悢闀愮箚闁告瑥顦扮亸锕傛煛瀹€瀣？闁逞屽墾缂嶅棝宕戦崱娑樼疇闁搞儯鍔嶉崣蹇撯攽閻樺弶鍣烘い蹇曞█閺岀喐顦版惔鈾€鏋呴梺鎼炲姂缁犳牠骞栬ぐ鎺濇晝闁靛牆瀚崰姘節绾板纾块柛瀣灴瀹曟劙骞嬮敃鈧粈澶屸偓鍏夊亾闁告洦鍋嗛悿鍥р攽椤斿浠滈柛瀣崌閺屸剝鎷呴悷鏉款潚闂佽鍠楅悷銉╂箒闁诲函缍嗛崑鍛村级閹间焦鈷戦悹鍥ㄥ絻閸よ京绱掗煫顓犵煓闁诡喗锚椤繄鎹勯搹瑙勭叄闂備線娼х换鍫ュ磹閺嶎厼鐓曢柟瀵稿Х绾惧ジ鎮楅敐搴′簻妞ゆ洘绮嶇换婵嬪焵椤掑嫬绠ユい鏂垮⒔閿涙粓姊洪棃娴ㄥ綊宕愬Δ鍛剹婵炲棗绻嗛弨浠嬫煃閳轰礁鏆為柛濠冨姍閺屾盯鍩＄€ｎ剛鐦堥悗瑙勬礃鐢帟鐏冩繛杈剧到婢瑰﹥瀵奸弽顓熲拻闁稿本鑹鹃埀顒勵棑缁牊绗熼埀顒勭嵁婢舵劖鏅柛鏇ㄥ墮鎼村﹪姊虹粙鎸庢拱濠㈣娲熷畷鎴﹀箻閹颁焦鍍甸梺缁樻尭濞撮攱绂掗幖浣光拺闁圭ǹ娴风粻姗€鏌涚€ｃ劌鈧繂顕ｉ锕€绀冩い鏃囧亹閿涙粌鈹戦悙鏉戠仸闁煎綊绠栭悰顕€宕奸妷锔规嫽婵炶揪绲介幉锟犲箚閸儲鐓曢柣鏂挎惈閳诲牏鈧娲橀崹鍧楃嵁濡偐纾兼俊顖滃帶楠炲牓姊绘笟鈧褎鐏欓梺绋垮閻撯€崇暦閹达箑唯闁冲搫鍊婚崢钘夆攽閻愭潙鐏ョ€规洦鍓欓埢宥咁吋閸ワ絽浜鹃悷娆忓缁€鍐煥閺囨ê鐏查柕鍡曠閳诲酣骞嬪┑鍥┾偓鎶芥煛婢跺﹦澧戦柛鏂挎捣缁棃鏌嗗鍡忔嫽闂佺ǹ鏈悷褏绮ｉ弮鈧换娑欐媴閸愬弶澶勯柛瀣儔閺屾盯鍩勯崘顏佹缂備胶濮锋繛鈧鐐寸墬濞煎繘宕滆閸嬔囨⒑缂佹鐭岀紒顕呭灣閹广垹鈽夐姀鐘殿吅闂佺粯鍔曢悘姘跺闯椤斿墽纾藉ù锝呭级椤庡棝鏌涚€ｎ偅宕屾慨濠勭帛閹峰懘宕烽鐔诲即闂備焦鎮堕崝蹇撐涢崟顖椻偓锕傚炊椤忓秵些闂備胶鍎靛Σ鍛村矗閸愵煈娼栭柧蹇氼潐鐎氭岸鏌熺紒妯轰刊闁告柨顦靛娲川婵犲倻浼囧銈庡亜椤﹁京鍒掔€ｎ亶鍚嬪璺侯儏閳ь剛鍏橀弻娑樷枎韫囨挷鍠婄紓鍌氱С缁舵岸鎮伴鈧畷鍫曨敆閳ь剛鐥閺屾盯鈥﹂幋婵囩亪缂備椒绶￠崳锝咁潖缂佹鐟归柍褜鍓欓…鍥槾闁瑰箍鍨藉畷鍗炍旀繝鍕ㄥ亾閸撲焦鍠愰柡鍐ㄧ墢瀹撲線鐓崶銊︾缁炬儳鍚嬫穱濠囶敍濠靛棔姹楀銈嗘⒐濞茬喖寮婚埄鍐ㄧ窞濠电姴瀚。鐑樼節閳封偓閸屾粎鐓撻悗瑙勬礃绾板秶鈧絻鍋愰埀顒佺⊕椤洭宕㈤悽鍛娾拺闁稿繘妫块懜顏堟煕鎼淬垹鈻曢柟顖氳嫰閻ｆ繈鍩€椤掑倹顫曢柟鐑橆殢閺佸﹪鏌ら幁鎺戝姶婵☆偄鍟村铏规嫚閳ヨ櫕鐏嶇紓渚囧枛閻線鎮橀幒妤佲拻闁稿本顨呮禍楣冩⒑瑜版帒浜伴柛鐘宠壘閳绘棃宕稿Δ浣叉嫽闂佺ǹ鏈悷褏绮ｉ弮鈧换娑欐媴閸愬弶鐦介柕濞炬櫅閻掑灚銇勯幒鎴濐仾闁抽攱甯￠弻娑㈠即閵娿儰绨诲銈呯箚閺呯娀寮婚敐澶嬫櫜闁告侗鍘戒簺闂備椒绱徊鍓ф崲閸儱鏄ラ柍褜鍓氶妵鍕箳閹存繍浠奸梺缁樺笒閻忔岸濡甸崟顖氱闁瑰瓨绻冮悘鎾斥攽閻愭彃鎮戦柛鏃€鐗滈幑銏犫槈閵忕姷鐓戞繝銏ｅ煐缁嬫牠鍩€椤掆偓濞硷繝寮婚弴銏犲耿婵°倐鍋撻柡鍡悼閳ь剝顫夊ú蹇涘礉瀹ュ拋鐒介煫鍥ㄧ☉缁€鍫㈡喐韫囨稑鐓熼柕鍫濇缁♀偓闂佹眹鍨藉褍鐡梻浣瑰濞插繘宕愬┑瀣畺鐟滄柨鐣烽崡鐐╂婵炲棗绻嗛崑鎾诲锤濡や胶鍘卞銈庡幗閸ㄥ灚绂嶉弽顬＄懓饪伴崟顓犵厜闂佸搫鐬奸崰鏍х暦椤愶箑绀嬫い鎺戭槹椤ワ絽鈹戦悙鑼憼缂侇喖绉瑰畷鏇㈠箮鐟欙絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯鍩勯崘鍓у姺闂佸疇顫夌粙鎾舵閹捐纾兼慨妯荤樂閿涘嫮纾奸柤鎼佹涧閸濆搫鈹戦垾宕囨憼缂佹鍠栭崺鈧い鎺嗗亾妞ゆ洩缍侀獮姗€顢欑喊杈ㄧ秱闂備焦鏋奸弲娑㈠疮椤栫偞鍋熼柡宥庡幗閳锋帒銆掑锝呬壕闂侀€炲苯澧伴柛瀣洴閹崇喖顢涘☉娆愮彿闂佽顔栭崰姘卞閸忕浜滈柡鍐ㄥ€甸幏锟犳煟閹剧偨鍋㈤柡宀€鍠撶划鐢稿捶椤撶姷妲囧┑鐘殿暯閳ь剙纾崺锝団偓瑙勬礀閻栧ジ宕洪敍鍕ㄥ亾閿濆骸澧存俊顐㈠槻閳规垿鎮╅崹顐ｆ瘎闂佺ǹ顑囨繛鈧鐐存崌椤㈡棃宕卞Δ鍐摌濠电偛顕慨鎾敄閹寸姷绀婇柡宥庡幗閻撴稓鈧箍鍎辨绋款嚕妤ｅ啯鐓涘ù锝嚽归埀顒€娼″璇测槈閵忕姴宓嗗┑鈽嗗灠閹碱偊鈥栨径鎰拺閻犲洩灏欑粻锝囩磼閼镐絻澹樻い顐㈢箻閹煎綊宕烽鐙呯床婵犵妲呴崹闈涚暦閻㈢ǹ鐭楅柛鈩冪⊕閳锋垿鏌涘☉姗堥練濠㈣蓱閵囧嫰骞嬪┑鎰枅閻庤娲栭幖顐﹀煡婢舵劕顫呴柣妯活問閸氬懘姊绘担铏瑰笡妞ゎ厼娲顐﹀箹娴ｈ娅栧┑鐘诧工閻楀﹪鎮￠悢鐓庣闁圭⒈鍘奸悘锝囩磼婢跺本鏆柡灞剧〒閳ь剨缍嗘禍婊堫敂椤撶喆浜滈柕寰涖倕顥濆Δ妤婁簷閸楀啿鐣烽悢纰辨晣闁绘ê鐏氶～宀勬⒒閸屾瑧绐旈柍褜鍓涢崑娑㈡嚐椤栨稒娅犻悗娑欙供濞堜粙鏌ｉ幇顖氱毢閺佸牆顪冮妶鍐ㄧ仾闁荤啿鏅涢悾宄扳堪閸♀晜顎夌紓鍌欑瀹曨剙顕ｉ懜鐢碘攳濠电姴娲﹂崐閿嬨亜韫囨挸顏ら柛瀣崌瀵粙鈥栭浣衡姇缂佹鍠栭崺鈧い鎺嗗亾闁伙絿鍏橀獮瀣晝閳ь剛绮诲☉娆嶄簻闁规崘娉涢弸鎴︽煟濠靛啫澧茬紒缁樼箘閸犲﹥寰勫畝鈧敍鐔兼⒑缁嬫鍎愰柛銊ョ仢閻ｇ兘骞囬弶璺吅闂佹寧娲嶉崑鎾绘煕閵娿儱鈧潡寮诲☉妯锋斀闁糕剝顨堝绋库攽閻愭潙鐏熼柛銊ユ贡缁牓宕橀埞澶哥盎闂佸搫鍟ú銈嗙瑜版帗鐓熼柨婵嗘閵囨繃鎱ㄦ繝鍐┿仢婵☆偄鍟埥澶婎潩鏉堚晜婢栭梻鍌欒兌鏋柨鏇樺劚鐓ゆ繝濠傛噹閸ㄦ繈鏌涢鐘插姎缁炬儳銈搁弻锝夊箛椤掑娈堕梺鍛娚戦崕鎶藉煘閹达附鍊烽悗娑欘焽缁嬪洤顪冮妶鍡楃仴婵☆偅顨婇崺銏狀吋婢跺﹤绐涘銈嗘礀閹冲繘顢欓弮鍫熲拺缂備焦锚婵矂鎮樿箛鏃傛噭闁哄懌鍎靛缁樻媴閸涘﹥鍎撻梺娲诲幖椤﹀崬顕ユ繝鍐﹀亝闁告劑鍔庨悾楣冩⒑閸濆嫬鏆欓柣妤€锕鏌ュ蓟閵夛妇鍘遍梺鏂ユ櫅閸犳艾鈻撻弴銏＄厽闁挎繂楠告晶瀛樻叏婵犲懏顏犻柟椋庡█楠炴捇骞掑┑鍡椢ㄩ梻鍌欑閹诧繝宕归悽鐢电濠电姴鍊婚弳锕傛煏韫囥儳纾块梻鍕缁辨帡顢涘☉娆戯紩闂佺ǹ顑嗛幑鍥极閸愵喖纾兼慨姗嗗墰閳ь剦鍘奸—鍐Χ閸℃鍘介梺绋垮婵炲﹤鐣锋导鏉戠疀闁哄娉曢鍡涙⒑閸涘﹦鈯曟繛鍏肩懇瀹曪繝鎼归鐘辩盎闂婎偄娲﹂幐濠毸夐悩鍨仏闁挎洖鍊归埛鎺懨归敐鍛暈閻犳劏鍓濋妵鍕箳閺囩偟姣㈤梺鐟扮畭閸ㄥ綊鍩為幋锕€鐐婇柨鏂垮⒔閻ｉ箖姊绘担绋款棌闁稿鎳庣叅闁哄稁鍋勯ˉ姘舵煕韫囨挸鎮戦柛娆忕箲閹便劌螖閳ь剙螞濡や胶顩叉繝濠傚娴滄粓鏌曟径娑氱暠闁告繄鍎ら幈銊︾節閸愨斂浠㈤梺鍦劜缁绘繃淇婇崼鏇炲窛妞ゆ棃鏁崑鎾淬偅閸愨斁鎷洪梺鐓庮潟閸婃洟寮搁崒鐐寸厱閹兼番鍨归埢鏇㈡煕閳规儳浜炬俊鐐€栫敮濠勭矆娓氣偓瀹曠敻顢楅崟顒傚幈濠碘槅鍨伴幖顐﹀煕閺冨牊鐓曢柟鐑樻尭濞搭喚鈧娲栭妶鍛婁繆閻戣棄唯闁宠桨鐒﹂弳顏勨攽閻樺灚鏆╁┑顔芥尦閹ê顫濈捄铏诡槷濠德板€曢幊搴ㄦ嚋瑜版帗鐓熼柕蹇曞У閸熺偤鏌涚€Ｑ冨⒉缂佺粯绻冪换婵嬪磼濮橆厽顔掗梻浣告啞閻熴儳鈧矮鍗冲濠氬即閵忕姴鑰垮┑掳鍊愰崑鎾绘煛閸℃瑥浠х紒杈ㄥ笧缁辨帒螣閼测晝鏉介柣搴ゎ潐濞叉牠鎮ユ總绋挎槬闁斥晛鍟刊鎾煙椤撶喎绗掑ù婊€鍗冲濠氬磼濮橆兘鍋撻幖浣靛亼闁圭虎鍠栫壕濠氭煕閺囥劌鈧煤椤忓懏娅嗛梺鍛婃寙閸涱垰骞嗛梻鍌欒兌閹虫捇鎮洪妸褎宕茬€广儱顦悿顕€鏌涘☉鍗炴灓缂佲檧鍋撻梻鍌氬€搁悧濠勭矙閹烘纾奸柕濞垮剭閻熸壋鍫柛鏇ㄥ幘閻撳姊洪崫鍕伇闁哥姵鐗犻獮鍐ㄢ枎閹垮啯鏅㈡繝銏ｆ硾椤戝洭寮抽妷鈺傗拻濞达絽鎲￠幉鎼佹煕閿濆啫鍔︾€规洖婀遍幑鍕惞鐟欏嫭顔曢梻渚€鈧稑宓嗛柛瀣躬瀵偄顓奸崱妯哄伎濠殿喗顨呴悧蹇曠矙婵犳碍鐓涢柛灞绢殔娴滈箖姊婚崒姘偓鐑芥嚄閸撲礁鍨濇い鏍仦閺呮繈鏌嶉崫鍕櫣缂佹劖顨婇弻鈥愁吋鎼粹€崇闂佺ǹ锕ら悥鐓庮嚕閸洖閱囨繛鎴灻‖瀣磽娴ｅ搫校闁稿孩濞婇垾锔炬崉閵婏箑纾繛鎾村嚬閸ㄤ即宕滈崘娴嬫斀闁绘劖娼欑徊濠氭煟鎺抽崝鎴炰繆閻㈢ǹ绀嬫い鏍ㄥ嚬濞煎﹪姊洪幐搴ｂ槈閻庢凹鍓熼崺娑樼暆閸曨剙鈧敻鎮峰▎蹇擃仾缂佲偓閳ь剟鎮楃憴鍕闁告挾鍠栭崹楣冨籍閸繄顦ㄥ銈呯箰濡盯鏁嶅⿰鍐ｆ斀闁宠棄妫楅悘鐘绘煟閵娧冨幋鐎规洘鍨块獮妯兼惥娴ｈ銇濇い銏℃瀹曘劑顢旈崨顓т紪闂傚倸鍊搁崐鎼佸磹閹间焦鍋嬪┑鐘插閻瑩鏌熼幆褍顣冲☉鎾崇Ч閺岋綁骞囬鐓庡闂佺粯鎸诲ú鏍煘閹达附鍋愰柟缁樺笚濞堣埖绻濋埛鈧崨顔芥瘓闂佸搫鏈惄顖炲春閿熺姴纾兼繝濠傚閸ㄨ鲸淇婇妶鍥ラ柛瀣洴椤㈡牗寰勯幇顒佺€梺绋跨灱閸嬬偤宕戦埡鍛€堕柣鎰煐椤ュ绱掗悩闈涗粶闁宠鍨块幃鈺呭垂椤愶絾鐦庡┑鐘垫暩閸嬫劙宕戦幘鏂ユ斀闁绘劖婢樼亸鍐煕韫囨洖浜剧紒瀣灱閻忓啴姊洪崨濠勭焼闁绘帪绠撳鎻掆堪閸啿鎷洪梺鍛婂姇瀵爼骞嗛崼銉︾厵闁告劘灏欑粻濠氭煙椤曞棛绡€鐎规洘锕㈤、鏃堝川椤撶喓鈧偓绻濋悽闈浶㈤柨鏇樺€濋幃褔宕卞▎鎴犵劶闂佸壊鍋嗛崰鎾剁不妤ｅ啯鐓曢柍鈺佸彁閹寸姳鐒婂ù鐓庣摠閻撴洟鎮楅敐搴濇喚婵☆垪鍋撻梻浣告惈閻绱炴笟鈧悰顔碱潨閳ь剟骞栬ぐ鎺濇晝闁挎繂鎳樺Λ鐑芥⒒閸屾瑧顦﹂柟纰卞亜铻為悗闈涙憸娑撳秹鏌熼幑鎰靛殭闁藉啰鍠栭弻锝夊棘閸喗鍊梺绋款儏椤戝寮诲☉銏╂晝闁靛牆鎳忛悘渚€姊洪崨濠傜厐缂佺粯绻傞～蹇撁洪鍕獩婵犵數濮撮崐鎼侊綖閸涘瓨鈷戠紒瀣儥閸庢劙鏌熼崫銉у笡缂佸矁椴哥换婵嬪炊閵娿儮鍋撻柨瀣ㄤ簻闊洦鎸搁銈夋煕鐎ｎ偅宕岀€殿喕绮欓、姗€鎮欏▓鎸幮ュ┑鐘茬棄閺夊簱鍋撻弴銏犵疇閹艰揪绲块悳缁樹繆閵堝懏鍣圭紒鐘侯潐缁绘盯鏁愭惔鈥愁潻闂佽娴氶崰妤呭Φ閸曨垰鐓涢柛灞剧矊瀵劑姊洪崫鍕効缂傚秳绶氶悰顕€宕堕渚囨濠电偞鍨堕敃鈺侇焽娴犲鈷掑ù锝呮啞閸熺偤鏌ｉ悢鏉戝姢闁瑰箍鍨介獮瀣晝閳ь剛绮婚悙鐑樼厪濠电偛鐏濋崜濠氭煛閸愩劎澧曢柣鎺戠仛閵囧嫰骞掗崱妞惧闂佹崘宕甸崑鐐寸┍婵犲洤围闁告洦鍘兼俊钘夆攽閻愬弶鍣藉┑鐐╁亾闂佸搫鐬奸崰搴ㄥ煝閹捐鍨傛い鏃傛櫕娴滃墽绱撴担鐟板姢濠⒀傜矙瀹曘垼顦归柍銉閹瑰嫰濡搁敃鈧壕顖涚箾閹炬潙鍤柛銊ゅ嵆瀹曟粓鏌嗗鍡忔嫼闂傚倸鐗婄粙鎺撳緞閸曨厾纾奸悹鍥皺婢э妇鈧鍠栭…鐑藉垂閹呮殾闁搞儯鍔嶉悾鐑芥⒑鐠囨彃鍤辩紓宥呮瀹曟粌鈽夐姀鈩冩珫闂佺粯鏌ㄩ幗婊呯不妤ｅ啯鍊甸柣銏㈡瑜版帩鏁侀柟鐐灱閺€浠嬫煃閵夈劍鐝柛鐘成戦〃銉╂倷閼碱剛顔掗梺璇″枓閺呮繈骞忛悩缁樺殤妞ゆ帒顦弬鈧梻鍌氬€风粈浣革耿闁秴纾块柕鍫濇处瀹曟煡鏌涢幇鍏哥敖缂佲偓婵犲洦鐓曢柍鈺佸暟閳洟鏌嶉柨瀣诞闁哄本绋撴禒锕傚礈瑜庨崳顓犵磽娴ｇ柉鍏屽褎顨婃俊鐢稿礋椤栨艾宓嗛梺缁樻煥閹碱偊鍩涘畝鍕€甸悷娆忓绾惧鏌涘Δ浣糕枙闁绘侗鍠栬灒闁稿繒鍘ч悵浼存⒑閸︻厾甯涢悽顖滃仱閸┾偓妞ゆ帒顦顔芥叏婵犲啯銇濋柟宕囧仱婵＄兘宕橀崣銉╁仐闁芥ɑ绻堝娲敆閳ь剛绮旈弶鎳堆囧蓟閵夛妇鍙嗛梺鍝勬川閸嬫盯鍩€椤掆偓缂嶅﹪骞冮檱缁犳盯骞橀娑欐澑闂備胶绮灙閻忓繑鐟х划濠氭偋閸稐绨婚梺鍐叉惈閿曘倖鏅堕幍顔剧＜閺夊牄鍔屽ù顕€鏌涢埡瀣暤闁糕斁鍋撳銈嗗笒鐎氼剛澹曟繝姘厵闁告挆鍛闂佹娊鏀辩敮鎺楁箒闂佹寧绻傞幊蹇涘疮閻愮儤鐓曢柣鏂挎惈娴狅妇绱掔紒妯笺€掗柟椋庡Ь椤﹁淇婄紒銏犳灓缂佽鲸甯″畷鎺戭潩濮ｆ鍥ㄧ厵妞ゆ梻鏅幊鍥ㄤ繆椤愩垹鏆欓柍钘夘槹濞煎繘鈥﹂幋鐑囩礀闂傚倸鍊搁崐鐑芥嚄閸洩缍栭悗锝庡枛缁€瀣煕椤垵浜為柡鍡畵濮婄粯鎷呴悷鎵虫灆闂佽　鍋撻弶鍫氭櫆閺嗘粓鏌ㄩ悢鍝勑㈤柦鍐枑缁绘盯骞嬮悙鐢靛彎濠电偛鐭堟禍顏堝蓟濞戙垹绠绘俊銈傚亾閻庢凹鍓欓埢宥呪枎閹剧补鎷绘繛杈剧到閹虫瑩宕烽鐔峰簥闂佽澹嗘晶妤呭磿濡ゅ懏鐓曠€光偓閳ь剟宕戝☉銏″珔闁绘柨鍚嬮悡鐔兼煛閸屾氨浠㈤柟顔藉灴閺岀喖顢涘⿰鍐ф勃闂侀潧娲ょ€氫即鐛崶顒€绀堝ù锝嚽归悡鍌滅磽娴ｇǹ鈷旈柛瀣尰缁绘稒绻濋崶褏鐣哄┑掳鍊曢崯顖炲窗閸℃稒鐓曢柡鍥ュ妼婢х増銇勯敂璇插籍婵﹦鍎ゅ顏堟偋閸偓鍋樻俊鐐€栧▔锕傚炊閼稿灚娅旈梻浣瑰缁诲倸螞濞戔偓鈧懘鎮滈懞銉モ偓鐢告煥濠靛棙鍣藉ù鐘崇洴閺岋繝宕崘顏喰滃┑顔硷龚濞咃絿妲愰幒鎳崇喖宕崟鍨秾濠碉紕鍋戦崐鏍ь潖瑜版帗鍋嬮柣妯垮皺閺嗭附銇勯幒鎴濐仾闁稿鍔戦弻娑樷槈濡垵鐗撻獮蹇曠磼濡偐顔曢柡澶婄墕婢х晫绮旈鈧埞鎴︻敊閸濆嫧鍋撳Δ鈧埥澶愭偨缁嬪潡鍞堕梺鍝勬川閸犳捇宕㈤柆宥嗏拺闁圭ǹ娴风粻鎾剁磼缂佹ê娴柟顕€绠栧畷褰掝敃椤愶綆鍟嶉梻浣虹帛閸旀浜稿▎鎴犱笉濠电姵纰嶉悡娑樏归敐鍫綈鐎规洖鐭傞弻鈩冩媴鐟欏嫬纾抽梺杞扮劍閹瑰洭寮幘缁樻櫢闁跨噦鎷�
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
        fprintf(stderr, "Oops!闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆撴倻濡桨鐢绘繝鐢靛Т閿曘倝宕板顑╃喖鍩€椤掑嫭鍊垫繛鍫濈仢閺嬬喖鏌熼鐓庘偓鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸閻忕偟纭堕崑鎾崇暦閸ャ劍鐣烽梻渚€鈧偛鑻晶瀵糕偓瑙勬磻閸楀啿顕ｆ禒瀣垫晣闁绘劗鏁搁弳顐︽煟閻斿摜鐭嬮柡灞诲姂閹箖鏌ㄧ€ｎ剟妾梺鍛婄☉閿曘倖绂嶅⿰鍫熲拺闁告稑锕︾粻鎾绘倵濮樼厧澧柍缁樻崌婵＄兘鍩￠崒妤佸闂備胶枪閺堫剟鎮疯钘濋柨鏇炲€归悡娆撴偣閸ュ洤鎳愰惁鍫ユ倵鐟欏嫭绀€婵犫偓闁秴绠熼柟缁㈠枛缁€瀣亜閹哄棗浜鹃悗娈垮枛濞尖€愁潖濞差亜鎹舵い鎾墲椤庢姊洪崫銉バｉ柣妤冨█楠炲啴鍨鹃幇浣瑰缓闂侀€炲苯澧寸€殿喖顭烽幃銏ゅ川婵犲嫮肖濠德板€х徊浠嬪疮椤栫儐鏁佺€广儱顦伴埛鎴犵磼鐎ｎ偒鍎ラ柛搴㈠姍閺屾盯寮埀顒勬偡閳哄懐宓侀柛鎰╁壆閺冨牆宸濇い鎾跺枎缁佽埖淇婇悙顏勨偓鏍箰妤ｅ啫纾规い鎰剁畱椤懘骞掗搹顐ゎ洸婵犲﹤鐗婇悡蹇擃熆閼哥數鈽夋い鈺婂墰缁辨帡鎮╅幇浣哄姺缂備浇椴哥敮妤€顭囪箛娑樜╅柨鏇楀亾闂傚绉瑰娲偡閺夋寧些闂佺娅曢敋闁伙絿鍏橀、鏇㈡晝閳ь剛绮堢€ｎ剛妫柣妤€鐓鍛洸闁告挆鈧崑鎾诲礂婢跺﹣澹曢梻浣告啞濞诧箓宕滃☉銏犲偍闂侇剙绉甸埛鎴︽⒒閸喕鍎愮憸鐗堝笒绾惧潡鏌熼幆鐗堫棄闁哄嫨鍎甸弻銊モ攽閸℃顦遍梺绋款儐閹告悂鍩㈤幘璇插瀭妞ゆ梻鏅禍顏呬繆閻愵亜鈧倝宕㈡ィ鍐ㄧ婵☆垯璀﹂崵鏇熴亜閹板墎鐣辩紒鐘崇⊕閵囧嫰骞樼捄杞版埛缂備焦鍔楅崑鐐垫崲濠靛鍋ㄩ梻鍫熷垁閵忋倖鍊垫慨妯哄船閸樺鈧娲忛崕闈涚暦閵娧€鍋撳☉娅辨岸骞忕紒妯肩閺夊牆澧介幃濂告煙閾忣偅宕屾い銏¤壘楗即宕ㄩ婊愮床闂佽崵濮村ú锕併亹閸愵喖鐒垫い鎺嗗亾婵炵》绻濋幃浼搭敊閽樺绐炴繝鐢靛仧閸嬫捇锝炲澶嬧拺闂傚牊绋撶粻鍐测攽椤旀儳鍘存い銏＄懇楠炴帡寮崒婊愮闯闂備胶枪閺堫剟鎮疯瀹曟繂顓奸崱鎰盎濡炪倖鎸鹃崑鐐哄窗濡皷鍋撶憴鍕８闁搞劋绮欓悰顕€寮介妸锕€顎撻梺绋跨箰椤︽壆鈧艾銈稿缁樻媴閸涘﹤鏆堢紓浣割儐閸ㄥ潡寮崘顔芥櫜闁搞儺鐓堥崑銊╂⒒娓氬洤澧紒澶屾暬閹繝寮撮姀锛勫幗濠碘槅鍨甸褎鏅堕娑氱闁割偆鍠撻惌宀勬煃瑜滈崜婵嬶綖婢跺⊕鍝勵潨閳ь剙鐣峰┑鍡忔瀻闁规崘娉涚粊锕傛⒑閹肩偛鍔撮柛鎾村哺瀹曟垹鈧綆鍠楅悡鏇熴亜閹邦喖孝闁诲繆鏅滈〃銉╂倷閺夋垵顫掗梺鍝勭焿缁查箖骞嗛弮鍫濈伋闁告劘灏欢鐔兼⒒娴ｅ懙褰掝敄閸℃稑绠查柛銉墮缁犳牗淇婇妶鍛櫤闁哄懏鐓￠獮鏍垝閸忓浜鹃柟棰佺劍妤犲嫰姊婚崒娆愵樂缂侀硸鍠氱划濠氬箣閿旇棄浜遍梺瑙勫礃椤曆呯不閺嶃劋绻嗛柕鍫濇噹閺嗙偤鏌涢悩鍙夘棦闁哄本鐩鎾Ω閵夈儺娼炬俊鐐€х拹鐔煎礉閹达箑钃熼柨婵嗩槸椤懘鏌嶆潪鎷屽厡濞寸媭鍙冮弻锝夊閳轰胶浠梺鍝ュУ閻楃娀鐛崘顭戞建闁逞屽墴閻涱噣骞掗幊铏⒐閹峰懘宕崟顐ゎ啈闂傚倸鍊烽懗鍓佸垝椤栫偛绀夐柡鍥ュ焺閺佸銇勯幘鍗炵仼闁绘挻绻堥弻鐔煎礈瑜忕敮娑㈡煟閹惧啿鏆熼柟鑼归オ浼村醇濠靛牜妲舵繝鐢靛仜濡瑩骞愰幖浣哥＜闁靛ň鏅滈悡娑氣偓骞垮劚閸燁偅淇婇崸妤佺厽婵犻潧妫涢崺锝夋煛瀹€瀣瘈鐎规洖鐖兼俊鐑藉Ψ瑜岄惀顏堟⒒娴ｇ懓鈻曢柡鈧柆宥呭瀭闁秆勵殔閽冪喐绻涢幋鐐冩岸寮ㄦ禒瀣厱闁斥晛鍠氬▓銏犆圭亸鏍т壕闂傚倸鍊风粈渚€鎮块崶顒夋晪鐟滄棁妫熼梺姹囧灮鏋柦鍐枑缁绘盯骞嬮悙鑼姲闂佺ǹ顑嗛幑鍥极瀹ュ纭€闁绘劕寮跺▓銊︾節閵忥綆娼愭繛鑼枎椤繑绻濆顒傦紲濠电偛妫欑敮鎺楀储閿熺姵鐓涢悗锝呭悁缁ㄥジ鏌曢崶褍顏鐐差儏閳规垿宕堕懜纰夌礂濠德板€楁慨鐑藉磻閻愬搫绀夋繛鍡樻尵瀹撲線鏌″鍐ㄥ闁荤喓鎳撻埞鎴︽偐閹绘帩浼€闂佸磭顑曢崐鏍崲濠靛鍋ㄩ梻鍫熺▓閺嬪懎鈹戦悙鏉垮皟闁搞儴鍩栭弲鐐烘⒑閸︻厼顣兼繝銏★耿瀵憡绗熼埀顒勫蓟濞戙垹鐒洪柛鎰典簴婵洭姊虹紒妯诲鞍婵炶尙鍠栭獮鍐潨閳ь剟骞冨▎鎾崇闁瑰搫妫欑€垫牠姊绘担鑺ャ€冮柣鎺炵畵閹囨偐鐠囪尪鎽曢梺鍝勬川閸犳挾绮绘ィ鍐╃厽闁逛即娼ф晶顖炴煟濠靛洦绀嬫慨濠冩そ楠炲酣鎳為妷锔芥闂備焦鎮堕崝灞筋焽閳ユ剚鍤曢悹鍥ㄧゴ濡插牊淇婇鐐存暠闁哄倵鍋撻梻鍌欒兌绾爼宕滃┑瀣ㄢ偓鍐疀閺傛鍤ら梺绋挎湰閸戠懓銆掓繝姘厪闁割偅绻堥妤€霉濠婂啫鈷旂紒杈ㄥ浮閹晠鎼归銏紦闁诲孩顔栭崰娑㈩敋瑜旈崺銉﹀緞閹邦剦娼婇梺缁橆焽閺佺ǹ顭囨径鎰拻濞撴埃鍋撻柍褜鍓涢崑娑㈡嚐椤栨稒娅犻悗鐢电《閸嬫挾鎲撮崟顒傤槬缂傚倸绉撮敃銈夋偩妞嬪海鐭欓悹鍥╁亾濡啫鐣峰鈧俊姝岊槹闁稿锕濠氬磼濞嗘埈妲梺纭咁嚋缁绘繂鐣峰┑瀣櫇闁稿本姘ㄩ悰銉╂⒑閸濆嫮鈻夐柛娆忕箳缁辩偤寮介鐔哄幈濠电偞鍨堕…鍥箺閻樼粯鐓熼柟鎯х－缁犲鏌＄仦鍓ф创妞ゃ垺娲熼弫鎰板炊閳哄啯姣囬梻鍌欑窔濞艰崵寰婇挊澶涜€块弶鍫氭櫆椤洟鏌熼悜姗嗘當缂佺姵绋掗妵鍕箻鐠虹洅娑樷攽椤斿吋宸濈紒杈ㄦ尰閹峰懘鎼归悷鎵偧缂傚倷娴囬褔鎮ч幘鑽ゅ祦闁告劏鏅濋々鐑芥倵閿濆骸浜芥俊顐㈠暙閳规垿鎮欏▓鎸庮棑缂傚倸鐗呴崡鍐茬暦鐟欏嫮顩烽悗锝庝簽閻ｅ搫鈹戞幊閸婃劙宕戦幘娣簻妞ゆ劧绲跨粻鐐存叏婵犲懎鍚归柍褜鍓熷褔骞夐敓鐘茬柧闁稿繗鍋愮弧鈧紒鍓у鑿ら柛瀣崌瀹曟﹢鎳犻鍕礈闂傚倷绀侀幉鈥愁潖瑜版帗鍋￠柕澶嗘櫓閺佸鏌ㄥ┑鍡╂Ц缂佺姷绮妵鍕籍閸屾瀚涘┑鐐靛帶閿曨亜顫忓ú顏勭闁兼亽鍎插▓浼存⒑缁嬫鍎愰柟鐟版喘瀵鎮㈤崜鍙壭ч柟鑹版彧缁茬晫绮敍鍕＝濞达綀娅ｇ敮娑氱磼鐎ｎ偄绗掓い顓炴穿缁犳稑鈽夊Ο纰辨Ф闁荤喐绮岀粔鐟扮暦閸濆嫮鏆嗛柛鏇ㄥ厴閹疯櫣绱掔紒銏犲箹闁瑰啿姘﹂。璺ㄧ磽閸屾瑧璐伴柛鐘崇墪闇夐柣鎴ｆ缁€鍡涙煙閻戞﹩娈旂紒顐㈢Ч閺屾盯顢曢妶鍛€婚梺閫炲苯澧扮紒顕呭灡缁岃鲸绻濋崶鑸垫櫖闂佺粯鍔曢悺銊╂偟瀹曞洨纾藉ù锝夋涧婵″吋銇勯鐐叉Щ妞ゎ偄绻愮叅妞ゅ繐鎷嬪Λ鍐ㄢ攽閻愭潙鐏卞瀛樻倐椤㈡捇宕卞☉娆屾嫽闂佺ǹ鏈懝楣冾敂閳哄懏鍊垫慨妯煎帶婢ф挳鏌熼钘夊姢闁伙綇绻濋獮宥夘敊閼恒儳鏆﹂梻鍌欑窔閳ь剛鍋涢懟顖涙櫠閹绢喗鐓曢柍瑙勫劤娴滅偓淇婇悙顏勨偓鏍暜閹烘绐楁慨姗嗗墻閻掍粙鏌熼柇锕€骞樼紒鐘荤畺閺屾稑鈻庤箛锝嗩€嗛梺鍏兼緲濞硷繝寮婚弴銏犲耿婵°倐鍋撻柡鍡悼閳ь剝顫夊ú蹇涘礉瀹ュ拋鐒介煫鍥ㄧ☉缁€鍫㈡喐韫囨稑鐓熼柕鍫濇缁♀偓闂佹眹鍨藉褍鐡梻浣瑰濞插繘宕愬┑瀣畺鐟滄柨鐣烽崡鐐╂婵炲棗绻嗛崑鎾诲锤濡や胶鍘卞銈庡幗閸ㄥ灚绂嶉弽顬＄懓饪伴崟顓犵厜闂佸搫鐬奸崰鏍х暦椤愶箑绀嬫い鎺戭槹椤ワ絽鈹戦悙鑼憼缂侇喖绉瑰畷鏇㈠箮鐟欙絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯鍩勯崘鍓у姺闂佸疇顫夌粙鎾舵閹捐纾兼慨妯荤樂閿涘嫮纾奸柤鎼佹涧閸濆搫鈹戦垾宕囨憼缂佹鍠栭崺鈧い鎺嗗亾妞ゆ洩缍侀獮姗€顢欑喊杈ㄧ秱闂備焦鏋奸弲娑㈠疮椤栫偞鍋熼柡宥庡幗閳锋帒銆掑锝呬壕闂侀€炲苯澧伴柛瀣洴閹崇喖顢涘☉娆愮彿闂佽顔栭崰姘卞閸忕浜滈柡鍌氱仢閳锋棃鎮介娑氥€掔紒杈ㄥ笧缁辨帒螣閼测晝鏆ゆ俊鐐€ら崑鍛崲閸儯鈧礁顫滈埀顒勫箖濞嗘挸绾ч柛顭戝櫘閸ゆ瑦绻濋悽闈浶㈤柣銉ヮ樀瀵埖鎯旈幘鍏呭閻熸粍鏌ㄩ～蹇旂鐎ｎ亞顦板銈嗘尰缁嬫垶绂嶉崜褏纾兼俊銈勮兌閳藉绻涢弶鎴濃偓鍨嚕椤愶箑纾奸柣鎰嚟閸樿棄鈹戦埥鍡楃仩闁告艾顑夊鎶藉煛閸愵亝锛忛梺璇″瀻閸愨晛鈧垳绱撴担铏瑰笡闁烩晩鍨堕悰顔锯偓锝庡枟閸婂鏌涢埄鍐噧濠殿喛娅ｇ槐鎾诲磼濞嗘帒鍘℃繝娈垮櫍椤ユ捇宕氶幒妤婃晬闁绘劘灏欓悿鍕⒑闂堟稈搴峰┑鈥虫川瀵囧焵椤掑嫭鈷戦柛娑橈工婵偓闂佸搫鍊搁崐鍦矉閹烘顫呴柕鍫濇閳ь剛鏁婚幃宄扳枎韫囨搩浠剧紓浣插亾闁割偁鍎查悡鏇㈢叓閸ャ劍顥栭柤鏉挎健閺屾洟宕遍弴鐙€妲柧浼欑秮閹泛鈽夐幒鎴濃拤闂佽桨鑳舵繛鈧慨濠呮閹风娀鍨惧畷鍥ｅ亾婵犳碍鐓曢悘鐐村劤閸ゎ剚淇婇锝囩煉婵﹦绮幏鍛驳鐎ｎ亝顔勯梻浣告啞閸ㄥ綊寮查銈嗩潟闁绘劕鎼獮銏＄箾閹寸儐鐒介柣娑栧劦濮婃椽宕崟顓涙瀱闂佺ǹ顑呭Λ娆戠矙婢跺⿴鍚嬮柛鈩冨姃缁ㄥ姊洪崫鍕枆闁稿瀚粋鎺楁晝閸屾稓鍘介梺瑙勫劤瀹曨剟宕濋敃鍌涚厸鐎光偓鐎ｎ剛袦閻庢鍠楅幐鎶藉箖濞嗗緷鍦偓锝庝簷婢规洟姊洪棃鈺佺槣闁告ɑ鎮傚畷鎴﹀箻閹颁焦鍍甸梺缁樺姦閸撴瑩顢旈敓锟�");//闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆撴倻濡桨鐢绘繝鐢靛Т閿曘倝宕板顑╃喖鍩€椤掑嫭鍊垫繛鍫濈仢閺嬬喖鏌熼鐓庘偓鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸閻忕偟纭堕崑鎾崇暦閸ャ劍鐣烽梻渚€鈧偛鑻晶瀵糕偓瑙勬磻閸楀啿顕ｆ禒瀣垫晣闁绘劗鏁搁弳顐︽煟閻斿摜鐭嬮柡灞诲姂閹箖鏌ㄧ€ｎ剟妾梺鍛婄☉閿曘倖绂嶅⿰鍫熲拺闁告稑锕︾粻鎾绘倵濮樼厧澧柍缁樻崌婵＄兘鍩￠崒妤佸闂備胶枪閺堫剟鎮疯钘濋柨鏇炲€归悡娆撴偣閸ュ洤鎳愰惁鍫ユ倵鐟欏嫭绀€婵犫偓闁秴绠熼柟缁㈠枛缁€瀣亜閹哄棗浜鹃悗娈垮枛濞尖€愁潖濞差亜鎹舵い鎾墲椤庢姊洪崫銉バｉ柣妤冨█楠炲啴鍨鹃幇浣瑰缓闂侀€炲苯澧寸€殿喖顭烽幃銏ゅ川婵犲嫮肖濠德板€х徊浠嬪疮椤栫儐鏁佺€广儱顦伴埛鎴犵磼鐎ｎ偒鍎ラ柛搴㈠姍閺屾盯寮埀顒勬偡閳哄懐宓侀柛鎰╁壆閺冨牆宸濇い鎾跺枎缁佽埖淇婇悙顏勨偓鏍箰妤ｅ啫纾规い鎰剁畱椤懘骞掗搹顐ゎ洸婵犲﹤鐗婇悡蹇擃熆閼哥數鈽夋い鈺婂墰缁辨帡鎮╅幇浣哄姺缂備浇椴哥敮妤€顭囪箛娑樜╅柨鏇楀亾闂傚绉瑰娲偡閺夋寧些闂佺娅曢敋闁伙絿鍏橀、鏇㈡晝閳ь剛绮堢€ｎ剛妫柣妤€鐓鍛洸闁告挆鈧崑鎾诲礂婢跺﹣澹曢梻浣告啞濞诧箓宕滃☉銏犲偍闂侇剙绉甸埛鎴︽⒒閸喓鈻撻幖鎾棑缁辨帡鎮╅崘鑼紝闂佺硶鏂傞崹钘夘嚕閹绢喖顫呴柨娑樺楠炲牓姊绘担鍝ユ瀮妞ゎ偄顦靛畷鏇㈡惞椤愩値娲搁梺缁樺姦閸忔稓鎹㈤崱娑欑厽闁规澘鍚€缁ㄥ鏌嶈閸撴岸鎮у⿰鍫濇瀬妞ゆ洍鍋撴鐐村笒铻栭柍褜鍓涙竟鏇㈠捶椤撶喎鏋戦梺缁橆殔閻楀棛绮幒妤佺厱婵﹩鍓涚粔娲煛鐏炵晫啸妞ぱ傜窔閺屾盯骞樼捄鐑樼亪閻庢鍠栭…鐑藉极閹版澘骞㈡俊顖氭惈婵℃娊姊绘笟鈧褏鎹㈤幒鎾村弿闁哄鍤╅幒妤€閱囬柕澶涜吂閹锋椽姊洪崨濠勨槈闁愁垱娲熼、鏃堝醇濠㈩€板洦鐓欓梻鍌氼嚟椤︼箓鏌ｈ箛鏂库枙闁哄被鍔戝鎾倷濞村浜鹃柟闂寸閸ㄥ倿鏌涚仦鍓р棨濞存粍绮撻弻锟犲磼濠垫劕娈梺鍛婃煟閸婃繈寮诲☉銏″亹鐎规洖娲ら埛灞轿旈悩闈涗粶闁挎洦浜滈锝嗙鐎ｅ灚鏅ｉ梺缁樺姍濞佳囩嵁閹扮増鈷掑ù锝呮啞閸熺偤鏌涢弮鎾绘缂佸倸绉撮オ浼村礋椤掆偓瀵潡姊虹紒妯诲碍缂併劌鐖煎銊︾鐎ｎ偆鍙嗗┑鐐村灦閿氭い蹇婃櫅闇夋繝濠傚暔閸嬨垽鏌＄仦鍓р姇缂佺粯绻堝畷姗€鍩￠崘鐐ら梻鍌欒兌閹虫挾绮诲澶婂瀭濞寸姴顑囧畵渚€鏌涢妷顔煎闁稿顑夐弻娑㈩敃閿濆洨鐣辩紓鍌氬暞濞叉牠鍩為幋锔藉亹闁割煈鍋呭В鍕節濞堝灝鏋熸繛鍏肩懅閸欏懘姊洪棃娑氬妞わ缚鍗冲畷鎴﹀磼閻愬鍘搁梺鎼炲劘閸庨亶鎮橀鍫熺厽闁规儳顕幊鍥┾偓娈垮枛閻栫厧鐣锋總绋课ㄧ憸蹇涘汲椤愨懇鏀介柨娑樺濡炬悂鏌涢敐蹇曠М鐎殿喛顕ч埥澶婎潩閿濆懍澹曢梺鎸庣箓閹冲酣銆傚畷鍥╃＜濠㈣埖锚濞呭秹鏌＄仦绯曞亾瀹曞洦娈曢梺閫炲苯澧寸€规洑鍗冲浠嬵敇濠ф儳浜惧ù锝呭濞笺劑鏌嶈閸撶喖鐛崘顔碱潊闁宠棄妫欐晥闂佺澹堥幓顏嗗緤妤ｅ啫闂い鏍ㄧ〒缁♀偓闂佹眹鍨藉褍鏆╂俊鐐€х徊鑲╁垝濞嗘挸鏄ラ柍褜鍓氶妵鍕箳閹存績鍋撻崨濠勵浄婵炲樊浜濋悡鏇㈡倵閿濆啫濡奸柣蹇旂叀閺岋紕浠︾化鏇炰壕鐎规洖娲﹀▓鏇㈡煟鎼搭垳绉甸柛鎾寸洴閺佸秹鎮㈤崗灏栨嫼闂備緡鍋嗛崑娑㈡嚐椤栨稒娅犻柛鎾楀懐锛濋悗骞垮劚閹冲繘藟閵忊懇鍋撶憴鍕闁搞劌鐏濋悾鐑芥偄绾拌鲸鏅┑鐐村灦鑿ら柛瀣崌瀹曞ジ寮撮悢鍝勫箥缂傚倸鍊烽梽宥夊垂瑜版帒鍑犻柣鏂垮悑閻撶喖鏌熼幆褜鍤熼柕鍡樺浮閺屽秷顧侀柛鎾寸懇瀹曘垹饪伴崼婵堬紱闂佺懓澧界划顖炲磹閻㈠憡鐓ユ繝闈涙椤庢顭胯閸楁娊寮婚妸銉㈡婵﹩鍓氶悘鍫濃攽椤旂》鏀绘俊鐐扮矙瀵宕ㄧ€涙ê鈧兘鏌ら懝鐗堢【妞ゅ浚鍘界换婵嬪煕閳ь剟宕橀妸銏″瘱缂傚倷娴囨ご鎼佸箰閹间緡鏁囧┑鍌溓瑰婵囥亜閺囩偞鍣洪柨娑楄兌缁辨捇宕掑▎鎴М闂佺ǹ锕ら幗婊呭垝閺冨牊鍋￠柟娈垮枟濞堢偓绻濋棃娑樷偓缁樼仚闂佸憡顨嗘繛濠囧蓟閿濆绫嶉柛銉ｅ姀閸氬倻绱撴担闈涘闁轰浇顕ч～蹇曠磼濡顎撻梺鍛婄洴濞佳冿耿闁秴绠查柕蹇嬪€曠粻铏繆閵堝倸浜鹃梺缁樻尰濞茬喖寮诲☉銏╂晝闁挎繂娲ら々顒勬⒑閸濆嫷鍎愰柡灞诲姂濠€渚€姊虹紒妯忣亜螣婵犲洤纾块柟鎵閻撶喖鏌熼幑鎰【闁哄鐩弻鈥崇暆閳ь剟宕伴弽顓炶摕闁搞儺鍓氶弲婵嬫煃瑜滈崜鐔煎箖濮椻偓閹晝绱掑Ο鐓庡箺闂備線娼ф灙闁告柨绉归、鏃堟偐閻㈢數锛滈柡澶婄墑閸斿苯霉椤曗偓閺岀喖顢欑憴鍕彅濡炪倖鏌ㄧ换姗€銆佸▎鎾崇畾鐟滃酣宕愰鐐粹拻闁稿本鐟чˇ锕傛煙绾板崬浜伴柟顖氼槹缁虹晫绮欑捄銊у炊闂備礁鎼粙渚€宕㈣ぐ鎺戠劦妞ゆ垼娉曢ˇ锕傛煃鐠囨煡鍙勬鐐达耿瀵爼骞嬪⿰鍕簼闂備浇顕ф鎼佸储濠婂牆纾婚柟鍓х帛閻撳啰鎲稿⿰鍫濈闁绘梻鍘х粻鏍煃閳轰礁鏆欑紒鍓佸仱閺岀喖鏌囬敃鈧獮鎰版煙闁垮銇濇慨濠冩そ閹筹繝濡堕崨顔锯偓鐐節閵忋垺鍤€婵☆偅绻堝顐㈩吋閸滀礁鎮戞繝銏ｆ硾椤戝洭宕㈤柆宥嗙厽閹兼惌鍨崇粊鐑芥煕閺傝法鐒搁柛鈹惧亾濡炪倖甯婇懗鑸垫櫠椤掑倻纾肩€光偓閸愵喖鎽电紓浣虹帛缁诲牆鐣烽崼鏇炍╅柕澶堝灩娴滄儳鈹戦悩宕囶暡闁绘挾濞€閺岀喖顢橀悢椋庣懆闂佸憡姊圭划宥囨崲濞戙垹宸濇い鎾跺櫏濡倗绱撴担浠嬪摵闁圭ǹ顭烽獮蹇涘川閺夋垶宓嶅銈嗘尰缁嬫帡锝炲鍡曠箚闁绘劦浜滈埀顒佺墵瀹曞綊鎮界喊妯轰壕婵﹩鍋勫畵鍡涙煟濞戝崬鏋ら柍褜鍓ㄧ紞鍡涘窗濡ゅ懎纾瑰┑鐘崇閻撶娀鏌熼鐔风瑨闁告梹顨婇弻鐔风暋闁妇鍚嬪┑顔硷龚濞咃絿鍒掑▎鎾崇閹兼番鍨虹€氭娊姊绘担鍝勫付缂傚秴锕︾划濠氬冀椤撶偞妲梺閫炲苯澧柕鍥у楠炴帡骞嬪┑鎰礉婵犵妲呴崑鍛村垂閸︻厽顫曢柟鐑橆殢閺佸﹪鏌ゅù瀣珔妞ゃ倐鍋撶紓鍌氬€烽悞锕傘€冮崼銉ョ獥闁哄稁鍘奸弰銉╂煃瑜滈崜姘跺Φ閸曨垰绠抽柟瀛樼箥娴尖偓闁诲氦顫夐幐椋庢濮樿泛钃熼柍銉﹀墯閸氬骞栫划鍏夊亾閸愬樊鍔€闂傚倷娴囧銊х矆娓氣偓瀹曨垶骞橀鑹版憰闂侀潧艌閺呮盯宕￠搹顐＄箚闁靛牆鍊告禍鐐箾鐎涙鐭婂褏鏅Σ鎰板箳濡や礁鈧攱銇勯幒鍡椾壕闂佸憡鏌ｉ崐妤冩閹炬剚鍚嬮柛婊冨暢閸氼偊鎮楀▓鍨灕妞ゆ泦鍥х叀濠㈣埖鍔曢～鍛存煃閸濆嫬鈧懓鈻嶉崶顒佲拻濞达絿鎳撻婊呯磼鐠囨彃鈧瓕鐏嬪┑鐐村灍閹崇偤宕堕鈧敮閻熸粌绻掓竟鏇熺附閸涘﹦鍘介梺閫涘嵆濞佳勬櫠椤曗偓閺屾盯寮拠娴嬪亾濡ゅ啯顫曢柟鐐墯閸氬鏌涘⿰鍐ㄦ殺闁告凹鍋婇幃妤€鈻撻崹顔界亪濡炪値鍘鹃崗妯虹暦鐟欏嫨鍋呴柛鎰╁妿閻も偓濠电偠鎻徊浠嬪箟閿熺姴纾规い鏍仦閳锋垹鐥鐐村櫣濞存粌缍婇幃璺衡槈閺嵮冨Е闂佺硶鏂侀崑鎾愁渻閵堝棗绗掗柛鐕佸亰閹啫煤椤忓懐鍘介梺鍝勭▉閸樻椽骞夐悙顒佸弿濠电姴瀚敮娑㈡煙瀹勭増鍤囩€规洏鍔戝Λ鍐ㄢ槈濮樻瘷銊ヮ渻閵堝啫鐏柣妤冨Т閻ｇ兘宕￠悙鈺傤潔濠碘槅鍨抽埛鍫ュ船閸洘鈷掑ù锝呮憸缁夌儤淇婇銉︾《婵炲棎鍨芥俊鍫曞炊閳哄喚妲搁梻浣告惈缁夋煡宕濇惔锝呭К闁逞屽墴濮婂宕掑鍗烆杸婵炴挻纰嶉〃濠傜暦閺囩偐妲堥柕蹇ョ磿閸樺憡绻涙潏鍓у埌闁硅绻濊棢闁靛繈鍊栭悡娑㈡倶閻愭彃鈷旈柕鍡樺笒闇夐柣娆忔噽閻ｇ數鈧娲樼划蹇浰囬幎鑺ョ厸闁告侗鍘归崑銏℃叏婵犲啯銇濇俊顐㈠暙闇夐柕澶堝劤婢э妇鈧鍠楄ぐ鍐偑娴兼潙閱囨繝闈涚墕楠炲牓姊绘担渚敯闁规椿浜浠嬪礋椤栨稑浜楅梺鍝勬川閸犳挾寮ч埀顒勬⒒閸屾氨澧涚紒瀣浮钘熸繝濠傚娴滄粓鐓崶椋庡埌濠⒀屽灦閺屾洟宕惰椤忣厽銇勯姀鈩冪妞ゃ垺顨婂畷鎺戔堪閸曨剦鍟岄梻鍌氬€风粈渚€骞栭锕€瀚夋い鎺戝閸庡孩銇勯弽銊ュ毈闁搞倖娲橀妵鍕即濡も偓娴滈箖姊虹化鏇熸澒闁稿鎸搁—鍐Χ閸℃鐟ㄩ柣搴㈠嚬閸撶喎鐣疯ぐ鎺戠＜闁绘劕顕崢閬嶆偡濠婂啴鍙勯柕鍡楀暣瀹曠厧鈹戦崼鐔割啌濠电偞鎸婚崺鍐磻閹剧粯鐓涢悘鐐插⒔濞插瓨顨ラ悙鍙夊枠婵☆偄鍟埥澶娾枎閹存粓鍋楁繝纰夌磿閸嬫垿宕愰妶澶婄；闁告洦鍨扮粻鐘虫叏濡炶浜鹃悗娈垮枛椤嘲顕ｉ幘顔碱潊闁绘ǹ顕ч弫瑙勭節閻㈤潧鈻堟繛浣冲厾娲Χ閸涱亝鐎洪梺鎸庣箓濞层劎澹曢挊澹濆綊鏁愰崶銊ユ畬婵犳鍠栭悧蹇曟閹烘柡鍋撻敐搴′簻缂佹う鍥ㄧ厵妤犵偛鐏濋悘鈺呮煃鐟欏嫬鐏╅柍褜鍓ㄧ紞鍡涘磻閸曨剛顩锋繛宸簼閳锋帡鏌涚仦鐐殤濠⒀勭洴閺屾盯骞掗崱妞惧闂傚倷绶氬褏鎹㈤崼銉ョ９闁哄稁鍘肩壕褰掓煕椤垵浜濋柛娆忕箲閹便劌顪冪拠韫婵犵妲呴崑澶娾枖閺囥垺鍤嶉梺顒€绉甸崵宥夋煏婢跺牆鈧绮诲鑸碘拺闁告稑锕﹂幊鍐┿亜閿旇鐏￠柟渚垮妼椤撳ジ宕堕敐鍛濠电偛鐗嗛悘婵嬪几閻斿吋鐓欐慨婵嗘湰閻濐亪鏌熸笟鍨闁糕斁鍋撳銈嗗笒鐎氼參鎮￠崘顔解拺婵炲棙鍎抽悘鐘裁瑰⿰鍫㈢暫婵﹪缂氶妵鎰板箳濠靛浂妫栫紓鍌欑贰閸ｎ噣宕归幎钘夌闁靛繒濮Σ鍫熺箾閸℃ê濮夌紒澶婄埣濮婃椽宕ㄦ繝鍐ㄧ樂闂佸憡鍔戦崝搴ㄥ储閹烘鈷掗柛灞剧懆閸忓本銇勯姀鐙呮敾闁逛究鍔戞俊鍫曞炊瑜嶉悘濠囨煙閼圭増褰х紒韫矙瀹曠懓鈹戠€ｎ偆鍘搁梺鍛婂姂閸斿孩鏅跺☉銏＄厱濠电姴瀚敮娑樓庨崶褝韬い銏＄洴閹瑧鈧稒顭囪ぐ鍝ョ磽閸屾瑩妾烽柛鏂跨焸閳ワ箑鐣￠柇锔界稁濠电偛妯婃禍婵嬎夐崼鐔虹闁硅揪缍侀崫鐑樸亜鎼粹剝顥㈡慨濠傤煼瀹曟帒顫濋澶夋偅闂佽瀛╅崙褰掑礈閻旂厧绠栭柟顖嗗懏娈濋梺瑙勵問閸犳鈻撻弴銏♀拺闂侇偆鍋涢懟顖涙櫠閹绢喗鐓曢柍瑙勫劤娴滅偓淇婇悙顏勨偓鏍暜閹烘绐楁慨姗嗗墻閻掍粙鏌熼柇锕€骞樼紒鐘荤畺閺屾稑鈻庤箛锝嗩€嗛梺鍏兼緲濞硷繝寮婚弴銏犲耿婵°倐鍋撻柡鍡悼閳ь剝顫夊ú蹇涘礉瀹ュ拋鐒介煫鍥ㄧ☉缁€鍫㈡喐韫囨稑鐓熼柕鍫濇缁♀偓闂佹眹鍨藉褍鐡梻浣瑰濞插繘宕愬┑瀣畺鐟滄柨鐣烽崡鐐╂婵炲棗绻嗛崑鎾诲锤濡や胶鍘卞銈庡幗閸ㄥ灚绂嶉弽顬＄懓饪伴崟顓犵厜闂佸搫鐬奸崰鏍х暦椤愶箑绀嬫い鎺戭槹椤ワ絽鈹戦悙鑼憼缂侇喖绉瑰畷鏇㈠箮鐟欙絺鍋撻弮鍫濈妞ゆ柨妲堣閺屾盯鍩勯崘鍓у姺闂佸疇顫夌粙鎾舵閹捐纾兼慨妯荤樂閿涘嫮纾奸柤鎼佹涧閸濆搫鈹戦垾宕囨憼缂佹鍠栭崺鈧い鎺嗗亾妞ゆ洩缍侀獮姗€顢欑喊杈ㄧ秱闂備焦鏋奸弲娑㈠疮椤栫偞鍋熼柡宥庡幗閳锋帒銆掑锝呬壕闂侀€炲苯澧伴柛瀣洴閹崇喖顢涘☉娆愮彿闂佽顔栭崰姘卞閸忕浜滈柡鍐ㄥ€甸幏锟犳煟閹剧偨鍋㈤柡宀€鍠撶划鐢稿捶椤撶姷妲囧┑鐘殿暯閳ь剙纾崺锝団偓瑙勬礀閻栧ジ宕洪敍鍕ㄥ亾閿濆骸澧存俊顐㈠槻閳规垿鎮╅崹顐ｆ瘎闂佺ǹ顑囨繛鈧鐐存崌椤㈡棃宕卞Δ鍐摌濠电偛顕慨鎾敄閹寸姷绀婇柡宥庡幗閻撴稓鈧箍鍎辨绋款嚕妤ｅ啯鐓涘ù锝嚽归埀顒€娼″璇测槈閵忊晜鏅濋梺鎸庣箓濞层劑鎮鹃悡搴富闁靛牆绻愰惁婊堟煕濞嗗繐鏆欐い顐㈢箰鐓ゆい蹇撴噹濞堛儵姊洪棃娑氬闁瑰啿绻橀幃姗€宕奸妷锔规嫼缂備礁顑嗛娆撳磿韫囨稒鐓ラ柡鍥埀顒佺箖娣囧﹪骞橀鑲╊唺闂佽鍎虫晶搴ㄦ儊閸績鏀芥い鏃€鏋绘笟娑㈡煕濮椻偓缁犳牕鐣烽姀銈庢晜闁割偆鍟块幏娲⒑閸涘﹥灏扮憸鏉垮暞缁傚秴鈹戦崼銏紲闁荤姴娲﹁ぐ鍐汲閿濆瀵犻柣鏂挎憸缁♀偓婵犵數濮撮崐鎼佸汲婵犲洦鐓涢柍褜鍓熼幊鐐哄Ψ閿濆嫮鐩庨梻浣告惈閸婁粙顢氳娣囧﹥绺介崨濠勫幍闂佹儳娴氶崑鍕叏婢跺⿴鐔嗛悷娆忓缁€鍐磼閾忚娅曠紒顔界懅閹瑰嫰濡搁埡鍐ㄧ闂傚倸鍊风粈浣圭珶婵犲洤纾婚柛娑卞灣閻瑩鏌熺€电ǹ浠掔紒璇叉閺岋綁鏁愰崨顖滀紘缂佺偓鍎抽妶鎼佸蓟瀹ュ牜妾ㄩ梺鍛婃尵閸犲酣鎮惧畡閭︽建闁逞屽墴閵嗕線寮崼婵嗚€垮┑鐐叉閻熝勬償婵犲洦鈷掑ù锝囩摂閸ゆ瑩鏌涢幋鐘虫珪缂佽京鍋ゅ畷鍗炍熺喊杈ㄩ敜闂佽崵濮惧銊х礊瀹ュ鍋勯柣鎾虫捣閸旓箑顪冮妶鍡楃瑨閻庢凹鍙冨畷鎰板垂椤愩倗顔曢梺鍓插亝缁诲嫭绂掗姀銈嗙厸閻庯綆鍋嗛埊鏇㈡煏閸パ冾伃妞ゃ垺锕㈡慨鈧柍鈺佸暞濞堢ǹ鈹戦悜鍥╁埌婵炲眰鍊濋弫鍐敂閸繄鐣哄┑鐘诧工閻楀﹪宕靛澶嬬厪濠㈣埖鐩顕€鏌ｅ┑鍥ㄧ婵﹦绮幏鍛驳鐎ｎ亝鐣伴梻浣规偠閸斿矂鏌婇敐鍛殾婵せ鍋撴い銏℃煥椤撳ジ宕ㄩ灏栧亾椤栫偞鈷掗柛灞捐壘閳ь剚鎮傚畷鎰暋閹冲﹤缍婂畷鍫曨敆婢跺娅旈柣鐔哥矊缁夌懓锕㈡笟鈧铏圭磼濡櫣浼囧┑鈽嗗亜鐎氼喚鍒掓繝姘亹缂備焦顭囬崢鐢告⒑鐠団€崇€婚柛娑卞墴閸嬫姊绘担鍛靛綊顢栭崱娑樼闁归棿绶ょ紞鏍ㄧ節闂堟侗鍎愰柛瀣€块獮鏍ㄦ綇閸撗咃紵濡炪倖姊归崝鏍ㄧ┍婵犲洦鍊锋い蹇撳閸嬫捇濡舵径濠傚亶婵犻潧鍊搁幉锟犲磹閼哥數绡€闂傚牊鍐婚崝鐔访瑰⿰鍡橆棄闁宠鍨块幃娆撴嚋闂堟稒閿紓鍌欑瀵爼宕曞畷鍥у疾闂備礁鍟块幖顐﹀疮閹殿喚鐭嗗┑鐘插€堕埀顒佸笒椤繈鏁愰崨顒€顥氬┑掳鍊楁慨鐑藉磻濞戙垺鏅濇い蹇撶墕閺嬩線鏌熼崜褏甯涢柛瀣姍閺屾稑鈻庤箛鎾亾閹达箑鐐婇柕濞у啫绠為梻浣筋嚙缁绘帡宕戝☉娆愭珷濞寸姴顑嗛崑鍌涖亜閺嶃劌鍤柣鏂挎閹嘲鈻庤箛鎿冧痪缂備讲鍋撻悗锝庡枟閻撴稓鈧厜鍋撻悗锝庡墰琚﹂梻浣筋嚃閸犳捇宕归挊澶屾殾妞ゆ劧绠戝敮闂佹寧娲嶉崑鎾寸箾閸涱喖鐏存慨濠呮閹风娀骞撻幒婵嗗Ψ闂備礁鎲￠崹鐢电礊婵犲倻鏆﹂柕蹇嬪€栭崐閿嬨亜閹烘垵浜炴繛鍙夋倐濮婇缚銇愰幒鎿勭吹缂備浇顔愮换婵嗩嚕椤愶箑鐐婃い鎺嶈兌閸樿棄鈹戦埥鍡楃仩闁告艾顑夊鎶藉煛閸涱喚鍙嗗┑鐐村灦閻熝囨儗瀹€鈧槐鎺撴綇閵婏箑纾抽悗瑙勬礃鐢帡鍩㈡惔銊ョ婵犻潧妫悗鎾⒒閸屾瑧鍔嶉悗绗涘厾娲晝閸屾氨锛涙繝鐢靛Т濞层倗绮婚悩缁樼厵闁硅鍔曢悡鎰偓瑙勬礀椤︾敻寮婚弴鐔虹闁割煈鍠氭禒鍏肩箾鐎涙鐭岄柛瀣尵閹广垹鈽夐姀鐘诲敹濠电娀娼ч悧鍛存惞鎼淬劍鈷戦柛婵嗗閸庢劗绱掔€ｎ偄娴┑锛勬暬瀹曠喖顢涢敐鍡樻珝闂備線娼х换鍡涘礈濠靛姹查柣妯虹－缁犻箖鏌ㄥ┑鍡樺櫤闁瑰吋鍔欓弻銊╁即閵娿倝鍋楅梺缁樹緱閸ｏ絽顕ｆ禒瀣╅柨鏇楀亾妞ゅ孩鎹囧娲川婵犲嫮绱伴梺绋块叄娴滃爼鎮伴鈧獮鍥偋閸碍瀚藉┑鐐舵彧缁蹭粙骞婂▎鎺嬩汗闁圭儤鍨甸悵妯侯渻閵堝棗绗掗悗姘煎弮瀹曟洖螖娴ｅ吀绨婚梺鍝勫暊閸嬫挾鈧娲樿摫闁逛究鍔戝畷妯好圭€ｎ偅鏉搁梻浣虹帛閿氱痪缁㈠弮閵嗗倿寮婚妷锔惧帗闂備礁鐏濋鍛箔閹烘顥嗗鑸靛姈閻撱儲绻濋棃娑欘棡闁革絾妞介弻娑㈡偄閸濆嫪妲愰梺鍝勬湰閻╊垶骞冮姀銈呬紶闁告洦浜濋崺娑氱磽閸屾瑦绁板鏉戞憸閺侇噣鎮欓崫鍕姦濡炪倖甯掗敃锔剧矓閻㈠憡鐓曢悗锝庝簻椤忣參鏌熼鏂よ€挎鐐达耿椤㈡瑩鎮剧仦钘夌婵犵數鍋犻幓顏嗗緤閹稿海浠氶梻浣告惈濡绱炴笟鈧濠氬焺閸愨晛顎撶紓浣割儏缁ㄩ亶宕戦幘璇查敜婵°倐鍋撶紒鐘虫緲閳规垿鎮╅幓鎺撴缂備礁澧庨崑鐐寸┍婵犲浂鏁嶆繝濠傚暙婵″搫顪冮妶鍡樷拹闁圭懓娲ら～蹇旂節濮橆剛锛滃┑鐐叉閸╁牆危椤曗偓濮婅櫣鎲撮崟顓滃仦闂侀€炲苯澧茬紒澶樺枛鐓ゆい蹇撴噺濞呭洭姊虹粙鎸庢拱闁煎綊绠栭幃妤咁敇閵忊檧鎷绘繛鎾磋壘濞层倖鏅堕鍓х＜濠㈣泛顑嗙亸锕傛煙椤曞棛绡€鐎殿喗鎸虫慨鈧柨娑樺楠炲秹姊虹拠鍙夋崳闁轰礁鎲￠悘娆撴煛瀹ュ繒绡€婵﹥妞藉畷顐﹀Ψ閵夋劧缍侀弻娑㈠籍閳ь剟鎮烽埡渚囧殨濠电姵纰嶉弲鏌ユ煕濞戞ê鈧懓螞閸愵喖鏄ラ柍褜鍓氶妵鍕箳閹存繍浠鹃梺绋款儏椤戝寮诲☉銏犵労闁告劕澧介崝鐑芥⒑瀹曞洦鍤€缂佸鏁绘俊鐢稿礋椤栨艾鍞ㄩ梺闈浤涢埀顒€螞閳哄啰纾藉ù锝囩摂閸ゆ瑦淇婇锝囨噰濠碉紕鏁诲畷鐔碱敍濮樿京鏉搁梻浣告啞閹搁绮堟笟鈧畷鍝勭暆閸曨兘鎷洪柣鐘叉穿鐏忔瑧绮婚弻銉︾厵闁告稑锕ら埢鏇犫偓娈垮枛椤兘寮澶婄妞ゅ繐鎳庢刊浼存⒒娴ｅ憡鍟為柟绋挎閻ｆ繈鍩€椤掍礁顕遍柛銉㈡櫇绾句粙鏌涚仦鍓ф噯闁稿繐鑻埞鎴︻敊閻愵剚姣堥梺璇″灠鐎氫即銆佸☉姗嗘僵闁绘挸瀛╅鎸庝繆閻愵亜鈧牕煤瀹ュ纾婚柟鐐儗濞堜粙鏌ｉ幇顖涘涧闁兼媽娉曢埀顒冾潐濞叉﹢銆冮崱妤婂殫闁告洦鍓涚弧鈧繛杈剧到婢瑰﹤螞濠婂牊鈷掑ù锝堟閸氱懓鈹戦鑲╀粵缂佸倸绉电粋鎺斺偓锝庝簽椤︻喗绻濋姀锝呯厫闁告梹娲滄竟鏇熺節濮橆厾鍘甸梺缁樺姦閸撴岸鎮橀柆宥嗙厸閻庯綆鍋呴ˉ鍫ユ煛鐏炲墽娲撮柍銉畵楠炲鈹戦崶鈺€鎲鹃梻鍌欒兌缁垳鎹㈠澶婇棷闁挎繂鎷嬪鏍煣韫囨挻璐￠柣顓熺懄缁绘盯宕卞Ο鍝勫Б闂佸憡鎸烽崡鍐差潖濞差亜绠伴幖娣灩椤︹晜绻涚€涙鐭ゅù婊庝邯閻涱噣宕橀埡渚囧殼闂佸湱鈷堥崢浠嬪疾閳哄懏鈷戦柟鑲╁仜閻忊晜銇勯敃鍌欐喚鐎规洑鍗冲浠嬵敇閻愮數鏋冮梻浣告惈缁嬩線宕㈡總鍛婂亗婵炴垯鍨洪悡鏇㈡煙闁箑鐏＄痪顓炵埣楠炴牠寮堕幋顖氫紣闂佸疇顫夐崹鍧楀箖閳轰讲鏋旈柟绋垮閻︽粍绻濈喊妯哄⒉鐟滄澘鍟撮獮濠偽熼搹瑙勬闂佽崵鍠愭竟鍡涘汲閿曞倹鐓曢柕澶嬪灥閸熺増绂嶉悧鍫滅箚闁绘劦浜滈埀顒佺墵閹冣堪閸繄锛熼梺鍦濠㈡﹢寮告笟鈧弻娑樼暆閳ь剟宕戦悙鐑樺亗闁绘柨鍚嬮悡娑㈡煕鐏炵偓鐨戝ù鐘灲閺岀喖顢欓挊澶屼紝闂佸搫鑻粔褰掑蓟閵娧€鍋撻敐搴濈敖闁荤喆鍔戝濠氬炊瑜滃Ο鈧梺鍝勮閸斿矂鍩為幋锕€骞㈡繛鍡楁禋閺嗩偊鏌ｉ悙瀵割暡婵ǜ鍔庡Σ鎰板箳閺冨倻锛滈梺闈涚箳婵绮昏ぐ鎺撯拺缂備焦顭囨晶顏堟煏閸懚褰掝敋閿濆洦瀚氭繛鏉戭儐椤秹姊洪棃娑氱畾闁告挻绻傞埢宥呪堪閸噥妫呭銈嗗姂閸ㄧ儤寰勯崟顒傜闁告瑥顦遍惌鎺斺偓娈垮枟婵炲﹥淇婇崼鏇炲窛闁稿本绋掗ˉ鍫ユ煙椤旇娅婃俊顐㈠暙閳藉螖閸愨晛绀嬬紓鍌氬€搁崐鐑芥嚄閸撲礁鍨濇い鏍仦閺呮繈鏌曡箛鏇烆€岄柛銈嗘礃閵囧嫰骞囬崜浣烘殸缂備胶濮惧畷鐢垫閹惧瓨濯撮柤娴嬪墲閸ㄧ敻鍩㈠澶婎潊闁靛牆妫岄幏娲煟閻樺厖鑸柛鏂胯嫰閳诲秹骞囬悧鍫㈠幍闂佸憡鍨崐鏍偓姘炬嫹?
        assert(se != nullptr);      //闂傚倸鍊搁崐鎼佸磹閹间礁纾归柟闂寸绾惧綊鏌熼梻瀵割槮缁炬儳缍婇弻鐔兼⒒鐎靛壊妲紒鐐劤缂嶅﹪寮婚悢鍏尖拻閻庨潧澹婂Σ顔剧磼閻愵剙鍔ょ紓宥咃躬瀵鎮㈤崗灏栨嫽闁诲酣娼ф竟濠偽ｉ鍓х＜闁绘劦鍓欓崝銈嗙節閳ь剟鏌嗗鍛姦濡炪倖甯掗崐褰掑吹閳ь剟鏌ｆ惔銏犲毈闁告瑥鍟悾宄扮暦閸パ屾闁诲函绲婚崝瀣уΔ鍛拺闁革富鍘奸崝瀣煕閵娿儳绉虹€规洘鍔欓幃娆撴倻濡桨鐢绘繝鐢靛Т閿曘倝宕板顑╃喖鍩€椤掑嫭鍊垫繛鍫濈仢閺嬬喖鏌熼鐓庘偓鎼侇敋閿濆棛顩烽悗锝呯仛閺咃綁姊虹紒妯荤叆闁圭⒈鍋婇悰顕€骞囬悧鍫氭嫽婵炶揪缍€椤宕戦悩缁樼厱閹兼惌鍠栭悘锔锯偓瑙勬礃濞茬喖寮婚崱妤婂悑闁糕剝銇涢崑鎾诲醇閺囩喓鍘撻梺鍛婄箓鐎氼剟寮抽悢铏规／闁诡垎浣镐划闂佸搫鐬奸崰鏍蓟閸ヮ剚鏅濋柍褜鍓欓锝夋倷椤掑倻顔曢梺鍓插亞閸犳捇鎯岄幒妤佺厸鐎光偓鐎ｎ剛锛熸繛瀵稿婵″洭宕犻弽顓炲嵆闁绘劖婢橀ˉ姘舵⒒娴ｅ摜绉烘い锝忓缁牊绗熼埀顒勭嵁閸愵喖绠ｉ柨鏃傛櫕閸樼敻姊洪悡搴㈣础濠⒀勵殙閵囨劘顦寸紒杈ㄥ浮閹崇姷鎹勬潪鐗堢潖闂備礁鎲＄敮妤冩暜閳ュ磭鏆﹀┑鍌溓归崡鎶芥煟閺傚灝绾ч悗姘冲亹缁辨捇宕掑顑藉亾閻戣姤鍊块柨鏇氱劍閹冲矂姊绘担铏瑰笡闁圭ǹ鐖煎畷鏇㈡偨缁嬭儻鎽曢梺缁樻椤旀牠宕烽娑樹壕闁挎繂绨肩花濠氭煃瀹勯偊妯€婵﹥妞藉畷銊︾節閸屾粎鎳嗛梻浣瑰濞诧附绂嶉鍫熷仒妞ゆ梻鈷堝銊╂煃瑜滈崜鐔煎Υ娴ｇ硶鏋庨柟鐐綑娴犲ジ鏌ｈ箛鏇炰哗婵☆偄瀚板畷锝嗙節閸ャ劉鎷婚梺绋挎湰閸╁啴鎯囩€ｎ喗鐓曢悗锝庝簻椤忊晠鎮￠妶澶嬬叆闁哄洨鍋涢埀顒€鎽滅划缁樺鐎涙鍘遍梺鍦亾椤ㄥ懘骞婇幇鐗堝亗閻庯綆浜栭弨浠嬫煟閹邦剙绾ч柍缁樻礀闇夋繝濠傚缁犵偤鏌熼鍡欑瘈鐎规洘锕㈤、娆撴偩鐏炶棄绠洪梻鍌欒兌缁垰顫忕紒妯间粴婵＄偑鍊戦崕閬嶎敄閸モ晜顫曢柟鐑橆殕閸嬫劗鈧懓澹婇崰姘跺触鐎ｎ喗鈷戦柛婵嗗椤︻剟鏌嶈閸撴瑧澹曢銏犳辈闁糕剝绋掗崐鐢告煥濠靛棝顎楀褎澹嗛幃顕€鏁冮崒娑掓嫽婵炶揪绲块悺鏃堝吹閸愵喗鐓曢柣妯挎珪瀹曞瞼鈧娲栫紞濠囧箰婵犲啫绶為悘鐐靛亾椤旀洘淇婇悙顏勨偓鏍礉瑜忕划濠氬箣濠靛洦娈伴梻鍌氬€烽悞锔锯偓绗涘厾鍝勵吋婢跺﹦锛涢梺瑙勫劤椤曨厾绮绘ィ鍐╃厱婵炴垵宕弸娑㈡煟韫囨挾鎽犻柟渚垮妽缁绘繈宕ㄩ鍛摋闂備礁缍婇ˉ鎾寸箾閳ь剚顨ラ悙瀛樺磳妤犵偞甯掗埞鎴﹀幢濡炩偓鍔戝濠氬磼濞嗘埈妲梺纭咁嚋缁绘繂鐣疯ぐ鎺撳仺闁汇垻鏁搁ˇ顖涚節閻㈤潧孝闁稿﹤顭峰鎶芥晜閻ｅ瞼鐦堟繝鐢靛Т閸婄粯鏅跺☉銏＄厱閻庯綆浜滈顐ょ磼鏉堛劌绗掗摶鏍煕閺囨鏉归柛瀣崌閸┾偓妞ゆ帒瀚悡鏇㈡倵閿濆骸浜濋悘蹇斿缁辨帒螖閸愩劍鐏堥梺绯曟杹閸嬫挸顪冮妶鍡楀潑闁稿鎸婚妵鍕即椤忓棛袦濡炪們鍨哄畝鎼佸春閳ь剚銇勯幒鍡椾壕闂佷紮绲块崗姗€鐛幒妤€绠ｉ柡鍐ｅ亾妞ゎ偄绉撮埞鎴﹀煡閸℃浠梺闈涙椤戝洤危閹扮増鏅搁柣妯绘灱閹风粯绻涙潏鍓хК婵☆偄瀚板畷銉ㄣ亹閹烘挾鍘遍柣搴秵閸撴瑦绂掕缁辨帡顢欓悾灞惧櫚閻庤娲栭妶鍛婁繆閻戣棄唯鐟滄粌危閼哥數绡€闁汇垽娼ч埢鍫熺箾娴ｅ啿娴傞弫鍕煙鐎电ǹ啸缂佲偓婵犲洦鐓涚€广儱楠告禍鎰版煕鐎ｎ偅宕岄柣娑卞櫍瀹曞綊顢欓悡搴經濠碉紕鍋戦崐褏绱撳璺虹闁规儼妫勯弰銉︾箾閹存瑥鐏╃紒鐙呯秮閺屻劑寮崒娑欑彧闂佸憡锕㈡禍璺侯潖濞差亜绠伴幖娣焺濮婂灝顪冮妶鍡樿偁闁告劖鍎冲▓銊╂⒑闂堟稈搴风紓鍌涙皑閹广垽宕卞☉娆戝幗闂侀€涘嵆濞佳勬櫠椤栫偞鐓曞┑鐘插暙婵秹鏌″畝瀣？濞寸媴绠撳畷婊嗩槼闁告帗绋掔换婵堝枈濡搫鈷夐梺鐑╁墲濞茬喕妫熼梺鍛婄懃椤︻厽绂嶅⿰鍫熺叆婵犻潧妫涙晶鏇灻瑰⿰鍐╄础缂佽鲸甯楀蹇涘Ω閵夛箒鐧侀柣搴ゎ潐濞叉鏁幒妤嬬稏婵犻潧鏌婇幒鎴旀瀻婵炲棙鍨硅ⅵ闂備礁鎼張顒€煤閻旈鏆﹀┑鍌氬閺佸棝鏌涚仦鍓ф晼婵犲﹤鐗婇崐鐢告偡濞嗗繐顏紒鈧崘顔界叆闁哄洦锚閻忚尙鈧娲樺浠嬪箖濞嗘挻鍊绘俊顖濄€€閸嬫捇鏌ㄧ€ｃ劋绨婚梺鐟版惈濡绂嶉幆褜娓婚柕鍫濇噽缁犳娊鏌涙惔娑樷偓妤呭箲閵忕姭鏀介悗锝庡亜娴犳椽姊婚崒姘卞缂佸鍔欏畷顐⑽旈崨顔规嫼闁荤喐鐟ョ€氼參濡甸悢璁跨懓饪伴崨顓濇埛濠碘€冲级閸旀瑩骞冨▎鎾充紶闁告洦鍘兼慨锔戒繆閻愵亜鈧牕顔忔繝姘；闁瑰墽绮悡鍐偡濞嗗繐顏╅柣蹇撶摠閵囧嫰濮€閿涘嫭鍣伴悗瑙勬处閸嬪﹤鐣烽悢鐓幬╅柕澶堝€曢ˉ姘節瀵伴攱婢橀埀顒佸姍瀹曟垿骞樼紒妯煎帗闂佽姤锚椤﹁棄螣閳ь剟姊虹拠鈥虫灓闁哄拋鍋嗛崚鎺戔枎閹炬娊鏁滃┑掳鍊愰崑鎾绘煛娴ｅ摜绉烘慨濠勭帛閹峰懘宕ㄦ繝鍐ㄥ壍婵犵數鍋涢惇浼村礉閹存繄鏆﹀ù鍏兼綑缁犲鎮归崶銊ョ祷闁稿绉瑰缁樼瑹閸パ冾潻缂備礁顦遍弫濠氬春濞戙垹绠ｉ柣妯兼暩閿涙繃绻涙潏鍓у埌闁告ɑ绮岃灋闁靛⿵璐熸禍婊堢叓閸ャ劍灏靛褎娲熼弻锝夊Χ鎼粹剝鐝濋梺鍝勭灱閸犳挾鍒掑▎鎾冲瀭妞ゆ洖鎳庢俊鐑芥⒒娴ｅ憡鍟為柣鐔村劤閹广垽骞掑Δ鈧悞鍨亜閹哄棗浜剧紓浣哄Т缁夌懓鐣烽弴銏犵闁诲繒绮浠嬪箖閳哄啯瀚氶柤纰卞墾缁遍亶姊绘担鍛婅础闁稿鎹囧鍛婄附缁嬪灝鍤戦梺鍝勫暙閻楀﹪鎮￠弴鐔翠簻妞ゆ挾鍠庨悘銉╂煛鐎ｎ剙鏋涢柡宀嬬秬缁犳盯骞樼捄琛″徍婵犳鍠栭敃銊モ枍閿濆應妲堥柣銏⑶瑰钘壝归敐鍛暈闁汇倕鍊荤槐鎾诲磼濞嗘劗銈版俊鐐差嚟鏋悡銈夋煟閺冨洤浜圭€规挷绶氶弻鐔告綇閸撗呮殸濡炪値鍋勯幊姗€寮婚敐鍜佹建闁糕剝銇炵花濂告⒑閸濆嫬鈧敻宕戦幘缁樷拻闁稿本鐟ㄩ崗宀€鐥鈩冪【妞ゎ剙锕ㄩˇ鍦偓娈垮枟瑜板啴銈导鏉戦唶婵犻潧鐗炵槐鎶芥⒒閸屾艾鈧悂鎮ф繝鍐懝婵°倐鍋撻柕鍡樺笚缁绘繂顫濋鐘插妇闂備礁澹婇崑鍛崲閸愵啟澶娾堪閸涱垳锛滃┑掳鍊撶粈浣该归鈧弻锛勪沪閸撗佲偓鎺楁煃瑜滈崜銊х礊閸℃顩叉繝闈涚懁婢舵劕顫呴柍鍨涙櫅娴滈箖鎮峰▎蹇擃仾濠㈣泛瀚伴弻娑㈠箻鐠虹儤鐏堝Δ鐘靛仜閸燁垳绮嬮幒鏂哄亾閿濆簼绨荤紒鎰⊕缁绘繈鎮介棃娑楁埛闂佺ǹ顑嗛幐楣冨焵椤掍椒浜㈡俊顐㈠閸╃偤骞嬮敂钘夆偓鐑芥煠閹间焦娑ф繛鎳峰懐纾介柛灞炬皑琚﹂梺绋款儐閹告悂鍩為幋锔藉€烽柛娆忣槸閻濇梻绱撴担鐟扮祷婵炲皷鈧剚鍤曟い鎰跺瘜閺佸﹪鎮樿箛鏃傚妞ゎ偄绉瑰娲濞戙垻宕紓浣介哺濞茬喎鐣烽姀銈嗙劶鐎广儱妫岄幏娲⒒閸屾氨澧涢柤鍐叉閵囨劙骞掗幋鐙呯吹闂傚倷绶￠崜娆戠矓鐎靛摜涓嶉柡宥庡幗閻撴瑩寮堕崼銉х暫婵＄虎鍣ｉ弻锝夊箻鐎靛憡鍣ч梺闈涙鐢帡锝炲┑瀣垫晢闁稿本鐟ㄥ绋库攽閻樻剚鍟忛柛鐘崇墵瀹曟劙骞栨担鍝ュ幋闂佺鎻梽鍕敁閹扮増鈷戠紒顖涙礃濞呭棝鏌ｅΔ浣圭妞ゃ垺宀告俊鐑藉煛娴ｄ警妲规俊鐐€栫敮鎺楀磹婵犳碍鍎婇柛顐犲劜閳锋垿鏌涘☉姗堟敾濠㈣泛瀚伴弻娑㈠箻鐎靛憡鍒涢悗瑙勬穿缂嶄礁鐣烽悢纰辨晬婵炴垶甯楃€氫粙姊绘担鍛靛綊寮甸鍕仭鐟滄棁妫熼梺鎸庢煥婢х晫澹曢悾灞稿亾楠炲灝鍔氶柟閿嬪灥椤斿繘濡烽埡鍌涘殙闂佺粯鍔楅崕銈夋偂閺囩姵鍠愰幖娣妸閳ь剙鍟村畷鍗炩槈濡》绱梻浣侯潒閸曞灚鐣烽梺缁樻尪閸庣敻寮婚敓鐘茬倞闁宠桨妞掔划鍫曟⒑闁偛鑻晶顔剧磽瀹ュ拑宸ラ柣锝囧厴楠炲鏁冮埀顒傚閸忚偐绠鹃柟瀵稿仦閻ㄦ垶銇勯弮鈧ú鐔奉潖濞差亝鍋￠柟娈垮枟閹插ジ姊洪懡銈呮瀭闁稿海鏁婚獮鍐潨閳ь剟銆侀弮鍫濋唶闁绘柨鎼獮妤呮⒒娴ｇ瓔娼愰柛搴㈠▕閹椽濡歌閻棝鎮楅敐搴℃灍闁绘挻鐟﹂妵鍕籍閳ь剟寮告繝姘殌闁秆勵殕閻撴瑦銇勯弮鍌涙珪闁瑰啿娲﹂〃銉╂倷閼碱儷褎銇勯姀锛勬噰闁诡喗鐟╅、妤佹媴閻戞鎺戔攽閻樻鏆滅紒杈ㄦ礋瀹曟垿骞嬮敃鈧壕褰掓煛閸モ晛校鐎规洘鐓￠弻鐔煎箥椤旂⒈鏆梺绋款儏椤戝寮诲☉銏犵労闁告劦浜栨慨鍥⒑缁嬪尅宸ユ繛纭风節瀵鈽夐埗鈹惧亾閿曞倸绠ｆ繝闈涙噽閹稿鈹戦悙鑼憼缂侇喖绉堕崚鎺楀箻鐠囪尪鎽曢梺缁樻⒒閸樠勫閻樼粯鐓曢柡鍥ュ妼鐢劑鏌曟径鍡樻珕闁绘挻娲熼悡顐﹀炊閵婏箑纾╅柣搴㈡皑婢ф濡甸崟顖ｆ晣闁炽儱鍟挎慨宄邦渻閵堝繐顩柟铏崌濠€浣糕攽閻樿宸ラ柛鐔跺嵆瀹曟椽鏁愰崶锝呬壕婵炲牆鐏濋弸锔姐亜閺囧棗娴傞弫鍥р攽閸屾簱褰掓煁閸ャ劊浜滈柟鏉垮缁夌敻鏌嶈閸撴瑥煤椤撶儐娼栫紓浣股戞刊鎾煕濞戞﹫宸ラ柡鍡楃墦濮婅櫣鎲撮崟顓熸啓濡炪倖娉﹂崶銊ヤ患濠电娀娼ч鍛不閻熸噴褰掓晲閸涱喗鍠愰梺姹囧€曞ú銈夊煘閹达附鍋愮紓浣股戦柨顓烆渻閵堝棗鐏ラ柟铏姉閸掓帡宕奸妷銉╁敹闂侀潧绻嗛埀顒冩珪閻庨箖姊绘繝搴′簻婵炶绠戠叅婵犲﹤鐗婇崕妤呮煕閳╁啨浠滈柡鈧禒瀣厽婵☆垵顕х徊濠氭煃瑜滈崜娑㈠极鐠囧樊鍤曢柕濠忓椤╃兘鎮楅敐搴′簽闁告ǹ妫勯埞鎴﹀煡閸℃浠╅梺鍦拡閸嬪棝鎯€椤忓浂妯勯梺鍝勭灱閸犳牕鐣峰Δ鍛亗閹肩补妲呭姘舵⒒娴ｅ憡鎯堥柣顓烆槺缁骞樺畷鍥ㄦ濠殿喗銇涢崑鎾淬亜閵忊剝顥堢€规洜鍠栭、妤呭磼濞嗘挸浠愰梻鍌氬€搁崐椋庣矆娓氣偓楠炴牠顢曢敃鈧€氬銇勯幒鍡椾壕闁绘挶鍊栨穱濠囶敍濮橆剚鍊悗瑙勬礀瀵墎鎹㈠☉銏犵婵炲棗绻掓禒楣冩⒑缁嬪尅韬柡鈧柆宥呂﹂柛鏇ㄥ灠缁犲磭鈧箍鍎遍悧鍡涘储閿熺姵鍋℃繝濠傚缁跺弶淇婇锝囩畵妞ゎ偄绻掔槐鎺懳熺拠宸偓鎾绘⒑閸涘﹦娲存繛浣冲懐绀婂┑鐘叉储閳ь兛绶氬浠嬵敇閻愯尙鐛╂俊鐐€栭悧妤呫€冮崨顔绢洸闁绘劦鍓氶崣蹇旀叏濡も偓濡绂嶅⿰鍛亾鐟欏嫭灏俊顐ｇ洴閸┾偓妞ゆ帊绶￠崯蹇涙煕閻樺磭澧甸柍銉畵閹粓鎸婃径瀣偓顒勬⒑瑜版帒浜伴柛妯垮亹濞嗐垽鎮欏ù瀣杸闂佺粯蓱瑜板啴顢旈幘顔界厱婵﹩鍓氶崵鍥ㄦ叏婵犲嫮甯涢柟宄版嚇閹煎綊鎮烽幍顕呭仹闂傚倷鐒﹂幃鍫曞礉閹存繍鐒界憸鎴︽倶鐎ｎ亖鏀介柣鎰綑閻忕喖鏌涢妸銉﹁础缂侇喖鐗婂鍕箛椤撶姴骞楅梻浣侯攰閹活亞寰婇崐鐕佹毐闂傚倷鐒︽繛濠囧绩闁秴鍨傞柛褎顨呴拑鐔哥箾閹寸們姘跺绩娴犲鐓曢柍鈺佸枤濞堛垹霉閻樿崵鐣烘慨濠冩そ楠炴劖鎯旈敐鍌氼潓婵犳鍠栭敃銊ョ暦闂堟党锝夊箛閺夎法顔婂┑掳鍊撶粈浣圭瑜版帗鈷戦柟顖嗗嫮顩伴梺绋款儏鐎氼喗绔熼弴銏犵闁兼亽鍎遍埀顒€鐏氱换娑㈠醇濠靛牅铏庨梺鍝勵儐缁嬫帡濡甸崟顖ｆ晣闁绘ɑ褰冮獮瀣倵鐟欏嫭绀冩俊鐐舵閻ｇ兘鎮㈢喊杈ㄦ櫖濠殿喗锕㈢涵绋课ｆ导瀛樷拻濞撴埃鍋撴繛浣冲棗娅ｉ梻浣告啞娓氭宕归幍顔剧焾闁挎洍鍋撻柍瑙勫灴閹晠宕归锝嗙槑濠电姵顔栭崰姘跺礂濡警鍤曟い鎰剁畱瀹告繂鈹戦悩鎻掓殭鐎殿喖娼￠弻锝嗘償閿濆棙姣勭紒缁㈠幖閻栫厧鐣烽幋锕€绠婚柟棰佺劍閸嶇敻姊虹紒妯诲碍濡ょ姵鎮傞崺娑氣偓锝庡枟閳锋垹绱掗娑欑婵炲懎鎳橀弻鐔兼惞椤愵剝鈧法鈧鍠栭…宄扮暦閸楃倣鏃€绻濋崒娑樷偓顖炴⒒娴ｈ櫣銆婇柍褜鍓欑壕顓犵不閺屻儲鍊垫慨妯哄暱娴滃湱绱掓潏銊﹀磳鐎规洘甯掗埢搴ㄥ箣椤撶啘婊勪繆閻愵亜鈧牠宕归棃娴虫稑鈹戠€ｃ劉鍋撴笟鈧鍊燁槷闁哄閰ｉ弻鐔煎箚瑜忛敍宥夋煥濞戞艾鏋涙慨濠呮閹叉挳宕熼顐ｎ棆濠电姭鎷冮崟鍨杹闂佽鍠掗埀顒佹灱閺嬪酣鏌熼幆褜鍤熼柛妯兼暬濮婂宕掑顑藉亾婵犳澶愬箛閺夎法顦ч悗鍏夊亾闁告洦鍓涢崢浠嬫⒑閹稿海绠撴俊顐ｇ懇婵￠潧鈹戠€ｎ偆鍘搁柣蹇曞仜婢ц棄煤閹绢喗鐓曢柍鍝勫暙娴犺鲸顨ラ悙鍙夘棥妞わ箑宕…璺ㄦ喆閸曨剛顦ラ梺瀹狀潐閸ㄥ潡寮澶婄妞ゆ劏鍓濆鈧梻鍌欒兌鏋柨鏇樺劦閹冣堪閸繃鐎銈嗘⒒閺咁偆寮ч埀顒勬⒑缁嬫鍎涢柛銊﹀缁辩偞绻濋崒婊勬闂佸壊鍋呭ú鏍ㄥ劔闁荤喐绮岀换妤呭Φ閹版澘绠氱憸澶愬绩娴犲鐓冮柦妯侯槹椤ユ粓鏌ｈ箛瀣姦闁哄本绋撻埀顒婄秵閸嬪嫭鎱ㄦ径濠庣唵閻熸瑥瀚粈瀣煙椤旂厧鈷斿ù鐙呭缁瑦寰勭粙娆炬濠德板€楁慨鐑藉磻濞戙垺鍊舵繝闈涱儏缁€澶嬫叏濡灝鐓愰柣鎾寸懇濮婃椽宕归鍛壈闂佽绻戦幐鎶藉蓟閿濆绠奸柛鎰╁妼閳峰顪冮妶鍐ㄧ仾妞ゃ劌锕ら悾鐑藉箳閹宠櫕妫冮崺鈧い鎺嶈兌椤╂煡鏌ｉ幇闈涘缂佺娀绠栭弻娑㈩敃閿濆洨鐣兼繛瀵稿О閸ㄤ粙寮婚敍鍕ㄥ亾閿濆骸浜為柍顖涙礋閺屾洟宕惰椤忣厽銇勯姀鈽呰€垮┑顔瑰亾闂佹枼鏅涢崯顖氣枔缁嬪簱鏀介柣鎴濇川閸掔増绻涚仦鍌氣偓婵嬪极閸愵噮鏁傞柛顐犲灩缁侊箓姊洪崫鍕犻柛鏂跨У閸掑﹪骞橀鐣屽幈濠电娀娼х€氼剟宕濆顓ф闁绘劖娼欐慨宥嗘叏婵犲嫮甯涢柟宄版嚇瀹曘劑妫冨☉姘毙ㄩ悗娈垮枤閺佸銆佸Δ鍛妞ゆ巻鍋撻柛鎾舵嚀椤啴濡堕崱妯锋嫻閻庤娲樿摫闁逛究鍔戝畷妯好圭€ｎ偅鏉搁梻浣虹帛閿氱痪缁㈠弮閵嗗倿寮婚妷锔惧帗闂備礁鐏濋鍛箔閹烘顥嗗鑸靛姈閻撱儲绻濋棃娑欘棡闁革絾妞介弻娑㈡偄閸濆嫪妲愰梺鍝勬湰閻╊垶骞冮姀銈呬紶闁告洦浜濋崺娑氱磽閸屾瑦绁板鏉戞憸閺侇噣鎮欓崫鍕姦濡炪倖甯掗敃锔剧矓閻㈠憡鐓曢悗锝庝簻椤忣參鏌熼鏂よ€挎鐐达耿椤㈡瑩鎮剧仦钘夌婵犵數鍋犻幓顏嗗緤閹稿海浠氶梻浣告惈濡绱炴笟鈧濠氬焺閸愨晛顎撶紓浣割儏缁ㄩ亶宕戦幘璇查敜婵°倐鍋撶紒鐘虫緲閳规垿鎮╅幓鎺撴缂備礁澧庨崑鐐寸┍婵犲浂鏁嶆繝濠傚暙婵″搫顪冮妶鍡樷拹闁圭懓娲ら～蹇旂節濮橆剛锛滃┑鐐叉閸╁牆危椤曗偓濮婅櫣鎲撮崟顓滃仦闂侀€炲苯澧茬紒澶樺枛鐓ゆい蹇撴噺濞呭洭姊虹粙鎸庢拱闁煎綊绠栭幃妤咁敇閵忊檧鎷绘繛鎾磋壘濞层倖鏅堕鍓х＜濠㈣泛顑嗙亸锕傛煙椤曞棛绡€鐎殿喗鎸虫慨鈧柨娑樺楠炲秹姊洪崫鍕垫Ц闁绘鎸剧划濠氬冀瑜滈悗鑸点亜閺囨浜鹃梺鍝勭焿缂嶄線鐛Ο鍏煎磯闁绘垶顭囨禍顏堟⒒娴ｅ憡鎯堥柣顓烆槺缁辩偞鎷呴崜鎻掓濡炪倖甯掔€氼剛绮绘导鏉戠缂侇喚鎳撴晶鍙夌箾閸噥鐒界紒杈ㄦ崌瀹曟帒鈻庨幇顔哄仒婵犵數鍋涢ˇ鎵矙閹达富鏁嬮柨婵嗩槸缁€鍫澝归敐鍥ㄥ殌閹兼潙锕ら埞鎴︽倷閺夋垹浠ч梺鎼炲妼缂嶅﹪寮荤€ｎ喖鐐婇柕濠忕畱瀵灝鈹戦绛嬫當婵☆偅顨婇悰顔嘉旈崨顔惧幗濠电娀娼уú銈夊传閻戞ɑ鍙忓┑鐘插暞閵囨繄鈧娲﹂崑濠傜暦閻旂⒈鏁囬柣妯夸含缁€鍐⒒閸屾瑧顦﹂柟鑺ョ矋閹便劑鎮介崨濠備罕闂佸搫娲㈤崹娲磻閳哄啠鍋撻悷鏉款仾濠㈢懓顑夊鍛婃償閵婏妇鍘甸梺缁樺姦閸撴稓绮ｉ悙鍨枑闁哄啫鐗嗛拑鐔哥箾閹存瑥鐏╅柣鎺撴そ閺屾盯骞囬闂村濡炪倖鎹侀～澶屾崲濞戞埃鍋撻崹顐ｅ仩闁逞屽墮椤戝鐛箛娑欐櫢闁跨噦鎷�
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




