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
std::vector<Operand *> para_operands;// 闂佽瀛╃粙鎺楁晪闂佹悶鍊濇禍璺侯嚕椤愶箑绠€光偓閳ь剙鈻撻崼鏇熺厽闁逞屽墮婵℃寧绻涢崼鐔风伌鐎规洩缍佸畷鎺戔槈濞嗘劦鍟€闂備焦鐪归崝宀勫垂缁卞墖
LoopManager loop_manager;
BasicBlock* temp_end_bb;
BasicBlock* temp_then_bb;
ExprNode* temp_cond_expr;

Constant* cal_expr(ExprNode* node2cal)
{
    float cal_result = node2cal->cal_expr_val();
    int cal_result_i = (int)cal_result;
    //std::cout << cal_result_i << std::endl;
    if (node2cal->cal_expr_val() != PRE_CAL_ERROR_MEETING_VAL)
    {
        SymbolEntry* se;
        if (node2cal->getSymPtr()->getType()->isFLOAT())
        {
            se = new ConstantSymbolEntry(TypeSystem::floatType, cal_result);
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
    // 闂備礁婀辩划顖炲礉閺囩喐娅犻柣妯虹－閻濆爼鏌ｅΔ鈧悧濠囷綖閿燂拷
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

    if (paraStmt != nullptr)
        paraStmt->genCode();
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
    //    //闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺岀喖鎮ч崼鐔哄嚒闂佸憡鍨规慨鎾煘閹达附鍋愰悗鍦Т椤ユ繄绱撴担鍝勵€岄柛銊ョ埣瀵鏁愭径濠勵吅闂佹寧绻傞幉娑㈠箻缂佹鍘搁梺鍛婁緱閸犳宕愰幇鐗堢厸鐎光偓鐎ｎ剛鐦堥悗瑙勬礃鐢帟鐏掗柣鐐寸▓閳ь剙鍘栨竟鏇㈡⒑閸濆嫮鈻夐柛瀣у亾闂佺ǹ顑嗛幐鎼侊綖濠靛鏁嗛柛灞剧敖閵娾晜鈷戦柛婵嗗椤箓鏌涢弮鈧崹鍧楃嵁閸愵喖顫呴柕鍫濇噹缁愭稒绻濋悽闈浶㈤悗姘间簽濡叉劙寮撮姀鈾€鎷绘繛杈剧到閹芥粎绮旈悜妯镐簻闁靛闄勫畷宀€鈧娲橀〃鍛达綖濠婂牆鐒垫い鎺嗗亾妞ゆ洩缍侀、鏇㈡晝閳ь剛绮绘繝姘仯闁搞儜鍐獓濡炪們鍎茬换鍫濐潖濞差亝顥堟繛鎴炶壘椤ｅ搫鈹戦埥鍡椾簼妞ゃ劌锕妴渚€寮崒娑樼／闁荤偞绋堥崜婵嬶綖瀹ュ鈷戦柛锔诲弨濡炬悂鏌涢妸褍顣肩紒鍌氱У閵堬綁宕橀埞鐐濠电偠鎻紞鈧┑顔哄€楀☉鐢稿醇閺囩喓鍘遍梺缁樏崯鎸庢叏瀹ュ洠鍋撳▓鍨灈闁硅绱曢幑銏犫攽閸♀晜鍍靛銈嗗笒閸嬪棝宕悽鍛娾拻濞达綀娅ｇ敮娑㈡煕閵娿儱鎮戦柟渚垮姂婵¤埖寰勬繝鍕叄闂備礁缍婂Λ鍧楁倿閿曞倸纾婚柍鈺佸暟缁犻箖鏌涢埄鍐剧劷闁瑰啿鎳愮槐鎺撴媴閸濆嫬骞嬮梺鍝勮嫰缁夌兘篓娓氣偓閺屾盯骞橀崡鐐差潎濡ょ姷鍋涚换姗€寮幘缁樻櫢闁跨噦鎷�
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
    //闂傚倸鍊搁崐鎼佸磹妞嬪海鐭嗗〒姘ｅ亾妤犵偛顦甸弫宥夊礋椤掍焦顔囬梻浣稿暱閹碱偊骞婃惔锝咁棜濠电姵纰嶉悡鏇㈡煛閸ャ儱濡兼鐐搭殘閳ь剝顫夊ú姗€鎮￠敓鐘茶摕闁跨喓濮撮悙濠囨煃鏉炴壆鍔嶉柣蹇庣椤啴濡堕崒娑欑彆闂佺粯鎼换婵嗩嚕鐠囨祴妲堥柕蹇曞Х閸旀挳姊虹粙璺ㄧ濠殿垼鍘界粋宥呪攽鐎ｎ亞锛熼梺缁樼矤閹界憘e闂傚倸鍊搁崐鎼佸磹妞嬪海鐭嗗〒姘ｅ亾妤犵偛顦甸弫宥夊礋椤愩垻浜伴柣搴″帨閸嬫捇鏌涢弴鐐典粵闁哄應鏅犲铏规兜閸涱厼鎯炵紓浣哥焿缁狀垶姊婚崒姘偓鎼佸磹閻戣姤鍊块柨鏇炲€归崕鎴犳喐閻楀牆绗掔紒鈧径灞稿亾閸忓浜鹃梺閫炲苯澧撮柛鈹惧亾濡炪倖甯婄粈渚€宕甸鍕厱闁靛ǹ鍎抽崺锝夋煏閸℃洜顦︽い顐ｇ箘閹瑰嫭绗熼娑氱П闂傚倷绶氬褔鎮ч崱娑樼疇闁规壆澧楅崑鍌炴煙鏉堥箖妾柍閿嬪灴濮婂宕奸悢鍓佺箒濠碘剝褰冮悥濂稿蓟濞戞瑦鍎熼柕蹇嬪灩瀵劑鎮楃憴鍕闁搞劍瀵ф穱濠囨倻閽樺鍘搁梺绋挎湰缁矂鐛幇鐗堚拻濞达絿枪椤ュ繘鏌涚€ｎ亝鍤囬柟铏箞閹瑩顢楅崒銈嗛敜濠德板€х徊浠嬪疮椤栫偛鐓曢柟杈鹃檮閻撴洘绻涢幋鐑囧叕濮掝偅鑹鹃…璺ㄦ喆閸曠數鍔风紓浣介哺鐢帡鎮惧┑瀣劦妞ゆ帒瀚粻鏌ユ煕閵夘喖澧柍閿嬪笒闇夐柨婵嗙墱濞兼劙鏌涚€ｎ剙鈻堥柡灞剧⊕閹棃濮€閻橆偅鐏嗛梻浣虹《閺備線宕戦幘鎰佹富闁靛牆妫楅崸濠囨煕鐎ｎ偅宕屾慨濠冩そ閹墽浠︾粙澶稿閻庡厜鍋撻柛鏇ㄥ厴閹锋椽姊洪悡搴綗闁稿﹥娲栭悺顓熺節閻㈤潧浠︽繛鍏肩懇瀹曟劙宕归鐐闂侀潧绻堥崐鏍吹瀹€鍕厾闁告挻褰冮崢鍛婃叏鐟欏嫷娈滄慨濠呮缁辨帒螣閸濆嫷娼撻梻浣告贡閳峰牓宕戞繝鍥ц摕闁靛ň鏅涘洿婵犮垼娉涢鍥矗閸℃稒鈷戠紓浣股戠粈鈧梺绋匡工濠€閬嶅焵椤掍胶鍟查柟鍑ゆ嫹
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
    //Operand* src;
    //Constant* cal_result = cal_expr_manager.cal_expr(expr);
    //if (cal_result != nullptr)
    //{
    //    //cal_result->genCode();
    //    src = cal_result->getOperand();
    //}
    //else
    //{
    //    expr->genCode();
    //    src = expr->getOperand();
    //}
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
        // 闂傚倸鍊搁崐鎼佸磹妞嬪海鐭嗗〒姘ｅ亾妤犵偛顦甸弫鎾绘偐閹绘帞鈧參姊虹粙璺ㄧ闁告艾顑夊畷锝堢疀濞戞瑧鍘梺鍓插亝缁诲秴危閸涘﹥鍙忛悷娆忓閸欌偓闂佸搫鐭夌紞浣割嚕椤掑嫬绠伴幖绮瑰墲濞堟ê鈹戦悩鎰佸晱闁搞劑浜堕獮鎰板箮閽樺鎽曢梺缁樻⒒閸樠呯不濮樿鲸鍠愰煫鍥ㄦ⒒椤╁弶銇勮箛鎾跺闁绘挻绋戦湁闁挎繂姣ヨぐ鎺濇晜妞ゆ挶鍨洪悡娑㈡倶閻愰鍤欏┑顔煎€块弻鐔碱敊閸濆嫧鍋撳┑鍡欐殾闁圭儤鍨熷Σ鍫熸叏濡も偓濡梻妲愰敓锟�?闂傚倸鍊搁崐鎼佸磹妞嬪海鐭嗗〒姘ｅ亾妤犵偛顦甸弫宥夊礋椤掍焦顔囬梻浣规偠閸庢椽宕滃▎鎴濐棜鐟滅増甯楅悡娑氣偓骞垮劚妤犳悂鐛弽顓熺參闁告劦浜滈弸娑㈡煛鐏炲墽娲村┑锛勫厴椤㈡瑩鎮℃惔顔戒氦闂傚倷绀侀幉锟犳偡閵夆晜鏅濋柕澶嗘櫅妗呴梺鍛婃处閸ㄤ即宕橀埀顒勬⒑闂堟丹娑氫沪閻愵剦鍟囬梻鍌氬€烽懗鑸电仚婵°倗濮甸幃鍌炲极閸愵喗鏅滈柣鎰靛墻濞肩喖姊洪崷顓炲妺闁搞倧绠撳顕€宕掑Δ鈧禍楣冩煥濠靛棝顎楀褌鍗抽弻锟犲幢濞嗗繆鏋呭┑顔硷龚濞咃綁骞忛悩璇茬伋闁惧浚鍋嗘禍顏嗙磽閸屾瑧鍔嶉柛鏃€鐗犲銊у寲缁涳拷
    }
    // 闂傚倸鍊搁崐鎼佸磹妞嬪海鐭嗗〒姘ｅ亾妤犵偛顦甸弫鎾绘偐閹绘帞鈧參姊虹粙璺ㄧ闁告艾顑夊畷锝堢疀濞戞瑧鍘梺鍓插亝缁诲秴危閸涘﹥鍙忛悷娆忓閸欌偓闂佸搫鐭夌紞浣割嚕椤掑嫬绠伴幖绮瑰墲濞堟ê鈹戦悩鎰佸晱闁搞劑浜堕獮鎰板箮閽樺鎽曢梺缁樻⒒閸樠呯不閺屻儲鐓欓梺顓ㄧ畱婢х増鎱ㄩ敐鍐ㄧ伈婵﹥妞介幊锟犲Χ閸涱喚鈧墽绱撴担绛嬪殭闁绘鎸搁锝嗙節濮橆儵鈺呮煃閸濆嫬鈧憡绂嶉悙鐑樷拺缂佸瀵у﹢鎵磼鐎ｎ偄鐏存い銏℃閺佹捇鏁撻敓锟�?闂傚倸鍊搁崐鎼佸磹妞嬪海鐭嗗〒姘ｅ亾妤犵偛顦甸崺鍕礃椤忓啰娉婇梻浣筋嚙濮橈箓锝炴径濞掓椽鎮㈡總澶屽姺閻熸粍妫冨顐﹀礃椤旂⒈娼婇梺闈涚墕閹冲海绮ｅ☉銏＄厽閹兼番鍨婚埊鏇㈡煥濮樿埖鐓曢柟鐐綑閳绘洟鏌″畝瀣ɑ闁诡垱妫冮、娑樷堪閸涘拑缍佸铏圭矙濞嗘儳鍓伴梺纭呮珪閸旀瑩骞冮悿顖ｆ▌濠殿喖锕︾划顖炲箯閸涙潙宸濆┑鐘插€瑰▓姗€姊绘担铏瑰笡婵☆偄绻樺畷婊冾潩椤撶姭鏀虫繝鐢靛Т濞层倝鎮欐繝鍐︿簻闁圭儤鍩堝Σ褰掓煛閸涱垰浠︾紒缁樼洴楠炴﹢寮堕幋鐘插Р闂備胶枪椤戝棝骞愭ィ鍐ㄧ劦妞ゆ帒鍠氬鎰箾閹绘帞绠荤€规洘娲熷Λ鍐ㄢ槈濮橆厽顔曢梻浣虹帛閹稿摜鑺遍崼鏇炵哗濞寸姴顑嗛悡娆撴煙椤栧棗鍠氶弳銏ゆ⒑缂佹ɑ灏伴柛銊ョ仢椤繒绱掑Ο璇差€撻梺鍏间航閸庢娊濡存繝鍥ㄢ拺闂傚牃鏅濈粔鍓佺磼閻樿櫕宕岄柣娑卞枦缁犳稑鈽夊▎鎰仧闂備浇娉曢崳锕傚箯閿燂拷?
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
       isreturn=true;//闂傚倸鍊搁崐宄懊归崶褏鏆﹂柛顭戝亝閸欏繘鏌℃径瀣婵炲樊浜滃洿婵犮垼娉涢鍛闁秵鈷戦梻鍫熻儐瑜版帒纾挎繛鍡樻尰閺呮彃顭跨捄鐚村姛妞ゆ挻妞藉娲箰鎼淬埄姊垮┑鈽嗗亜鐎氭澘鐣峰⿰鍫濈闁兼亽鍎幏铏圭磽閸屾瑧鍔嶉拑閬嶆煟閹惧崬鍔﹂柡宀€鍠撻幏鐘侯槾缁炬崘娉曠槐鎺楊敊绾拌京鍚嬪Δ鐘靛仦鐢帟鐏冮梺閫炲苯澧伴柣銉海椤﹀綊鏌＄仦鐣屝х€规洦鍋婂畷鐔碱敃閻旇渹澹曢悷婊呭鐢宕戦崒鐐寸厪濠㈣埖绋戦々顒傜磼閻樿崵鐣洪柡灞剧☉閳诲氦绠涢敐鍠般劍绻濋埛鈧崨顔芥瘓闂佸搫鐬奸崰鏍€佸☉姗嗘僵妞ゆ帊鐒﹂鎺楁煟鎼淬値娼愭繛鍙夌矒瀹曚即寮介婧惧亾娓氣偓瀵噣宕煎┑鍡氣偓鍧楁⒑鐎圭姵銆冩俊鐐村笧缁辨捇骞樼紒妯锋嫽闂佺ǹ鏈悷锔剧矈閻楀牏绠惧璺侯儐缁€瀣偓瑙勬礃婵炲﹪寮崘顔肩＜婵﹢纭搁崬鐢告⒒娴ｈ姤纭堕柛锝忕畵楠炲繘鏁撻敓锟�?
        retVal=retValue->getSymPtr()->getType(); //闂傚倸鍊搁崐椋庣矆娓氣偓楠炴牠顢曚綅閸ヮ剦鏁冮柨鏇楀亾闁汇倗鍋撶换婵囩節閸屾粌顣洪梺钘夊暟閸犳牠寮婚弴鐔虹闁割煈鍠栨竟鍕攽閻愬弶鈻曞ù婊勭矒瀹曟﹢鍩€椤掆偓椤啴濡堕崱妯烘殫闂佺ǹ顑囬崰鏍极瀹ュ應鏋庨柟鍓цˉ閹锋椽姊虹粙璺ㄧ闁稿鍔欐俊鍫曞箻椤旂晫鍘搁梺鍛婁緱閸ㄧ増绂掗柆宥嗙叆婵犻潧鐗嗘禒閬嶆煛娴ｇǹ鏆ｇ€规洘甯掗埥澶婎潩椤掆偓濮规煡姊婚崒娆掑厡缂侇噮鍨扮叅闁哄稁鍘肩壕褰掓煕閿旇骞愰柛瀣尭椤繈鎮欓鈧锟�?
    }


    if (retValue != nullptr)
        retValue->typeCheck();

}

void AssignStmt::typeCheck()
{
    // Todo
    //闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺岀喖鎮ч崼鐔哄嚒闂佸憡鍨规慨鎾煘閹达附鍋愰悗鍦Т椤ユ繄绱撴担鍝勵€岄柛銊ョ埣瀵鏁愭径濠勵吅闂佹寧绻傞幉娑㈠箻缂佹鍘搁梺鍛婁緱閸ゆ壆娑甸崨顖欑胺濠电姴鐥夐弶搴撳亾閺囩姭鍋撳鐓庡籍妞ゃ垺妫冮幃鈺冪磼濡厧甯鹃梻浣虹《閸撴繈銆冭箛娑樼？闁哄啠鍋撴い銊ｅ劦閹瑩鎳犻鍌ゅ晪闂備礁鎼悮顐﹀礉鎼淬劌鐒垫い鎺戯功缁佺兘鏌涢弬鎸庢拱缂佸倸绉电€佃偐鈧稒顭囬崢鍛婄節閵忥絾纭鹃柨鏇檮閺呭爼濮€閵堝棛鍘遍柣搴秵閸嬪懐浜搁悽鍛婄厓鐟滄粓宕滃鑸靛殞闁诡垎宥堚偓鍧楁⒑椤掆偓缁夊绮婚婊勫枑闁绘鐗嗘穱顖炴煛娴ｇǹ鏆ｉ柡灞诲姂瀵噣宕堕‖顔芥尰缁绘盯宕ㄩ銏紙濠殿喖锕ュ钘夌暦椤愶箑绀嬫い鎰枎娴滈箖鏌涢锝嗙缁炬儳缍婇弻鈥愁吋鎼粹€茬爱闂佺ǹ顑嗛幐鎼侊綖濠靛鏁嗛柛灞剧敖閵娾晜鈷戦柛婵嗗椤箓鏌涢弮鈧崹鍧楃嵁閸愵喖顫呴柕鍫濇噹缁愭稒绻濋悽闈浶㈤悗姘间簽濡叉劙寮撮姀鈾€鎷绘繛杈剧到閹芥粎绮旈悜妯镐簻闁靛闄勫畷宀€鈧娲橀〃鍛扮亽闂佺厧顫曢崐銈吤归崟顐富闁靛牆妫涙晶閬嶆煕鐎ｎ剙浠遍柟顕嗙節婵＄兘鍩￠崒婊冨箺闂備礁鎼崐鍦偓绗涘洤鐤柣鎰劋閻撶喖鐓崶椋庡埌婵炲懎绉归弻鈥崇暆閳ь剟宕伴幇鍏洭鎮ч崼鐔峰妳闂佺偨鍎遍崯璺ㄧ不閿濆鈷掑ù锝堝Г閵嗗啴鏌ｉ幒鐐电暤妤犵偞鍔欓獮鍡氼檨闁搞倖顨呴埞鎴︽偐閹绘帊绨藉┑鐐烘？閸楁娊寮婚敐澶娢╅柕蹇曞С婢规檮t闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺岀喖鎮ч崼鐔哄嚒闂佸憡鍨规慨鎾煘閹达附鍋愰悗鍦Т椤ユ繄绱撴担鍝勵€岄柛銊ョ埣瀵濡搁埡鍌氫簽闂佺ǹ鏈粙鎴︻敂閿燂拷
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





//闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺岀喖鎮ч崼鐔哄嚒闂佸憡鍨规慨鎾煘閹达附鍋愰悗鍦Т椤ユ繄绱撴担鍝勵€岄柛銊ョ埣瀵鏁愭径濠勵吅闂佹寧绻傞幉娑㈠箻缂佹鍘搁梺鍛婁緱閸犳宕愰幇鐗堢厸鐎光偓鐎ｎ剛鐦堥悗瑙勬礃鐢帟鐏掗柣鐐寸▓閳ь剙鍘栨竟鏇㈡⒑閸濆嫮鈻夐柛瀣у亾闂佺ǹ顑嗛幐鎼侊綖濠靛鏁嗛柛灞剧敖閵娾晜鈷戦柛婵嗗椤箓鏌涢弮鈧崹鍧楃嵁閸愵喖顫呴柕鍫濇噹缁愭稒绻濋悽闈浶㈤悗姘间簽濡叉劙寮撮姀鈾€鎷绘繛杈剧到閹芥粎绮旈悜妯镐簻闁靛闄勫畷宀€鈧娲橀〃鍛达綖濠婂牆鐒垫い鎺嗗亾妞ゆ洩缍侀、鏇㈡晝閳ь剛绮绘繝姘仯闁搞儜鍐獓濡炪們鍎茬换鍫濐潖濞差亝顥堟繛鎴炶壘椤ｅ搫鈹戦埥鍡椾簼妞ゃ劌锕妴渚€寮崼婵嬪敹闂佸搫娲ㄩ崯鍧楀箯濞差亝鐓熼柣妯哄帠閼割亪鏌涢弬璺ㄧ劯鐎殿喗鎮傞獮瀣晜閻ｅ苯骞愰梺璇插嚱缂嶅棙绂嶉崼鏇熷亗闁稿繒鈷堝▓浠嬫煟閹邦垰鐨虹紒鐘差煼閺岀喖顢欓悾宀€鐓夐梺鐟扮－閸嬨倖淇婇悜鑺ユ櫆缂佹稑顑勯幋鐑芥⒒閸屾艾鈧绮堟笟鈧獮鏍敃閿曗偓绾惧綊鏌涢锝嗙缁炬儳缍婇弻鈥愁吋鎼粹€茬爱闂佺ǹ顑嗛幐鎼侊綖濠靛鏁嗛柛灞剧敖閵娾晜鈷戦柛婵嗗椤箓鏌涢弮鈧崹鍧楃嵁閸愵喖顫呴柕鍫濇噹缁愭稒绻濋悽闈浶㈤悗姘间簽濡叉劙寮撮姀鈾€鎷绘繛杈剧到閹芥粎绮旈悜妯镐簻闁靛闄勫畷宀€鈧娲橀〃鍛达綖濠婂牆鐒垫い鎺嗗亾妞ゆ洩缍侀、鏇㈡晝閳ь剛绮绘繝姘仯闁搞儜鍐獓濡炪們鍎茬换鍫濐潖濞差亝顥堟繛鎴炶壘椤ｅ搫鈹戦埥鍡椾簼妞ゃ劌锕妴渚€寮崼婵嬪敹闂佸搫娲ㄩ崯鍧楀箯濞差亝鐓熼柣妯哄帠閼割亪鏌涢弬璺ㄧ劯鐎殿喗鎮傞獮瀣晜閻ｅ苯骞愰梺璇插嚱缂嶅棙绂嶉崼鏇熷亗闁稿繒鈷堝▓浠嬫煟閹邦垰鐨虹紒鐘差煼閺屸剝鎷呯憴鍕３闂佽桨鐒﹂幑鍥极閹剧粯鏅搁柨鐕傛嫹
/*闂傚倸鍊搁崐鐑芥嚄閸洖绠犻柟鍓х帛閸嬨倝鏌曟繛鐐珕闁哄懏绻堥弻鏇＄疀鐎ｎ亖鍋撻弴銏犲嚑濞撴埃鍋撻柡宀€鍠栭獮鎴﹀箛闂堟稒顔勬繝纰樻閸嬪懘鏁冮姀銈呰摕闁哄洢鍨归柋鍥ㄧ節闂堟稒绁╂俊顐ゅ仜椤啴濡堕崨顖滎唶闂佺粯鐗滈崢褔锝炶箛娑欐優閻熸瑥瀚壕顖炴⒑闂堟胆褰掑磿閺屻儲鍊靛Δ锝呭暞閳锋垿鏌涘┑鍡楊伌闁稿骸绻戦妵鍕敇閻樻彃骞嬮梺缁樹緱閸犳稓绮诲☉妯锋婵炲棗绻嗛崑鎾寸節濮橆厾鍘鹃梺璇″幗鐢帡宕濆⿰鍕闁告侗鍋勯悘鍙夋叏婵犲啯銇濈€规洦鍋婂畷鐔碱敃閻旇渹澹曢梺鍓插亝濞叉牜澹曡ぐ鎺撶厸鐎广儱楠告禍鎰版煕鐎ｎ偅灏い顐ｇ箞閹瑩顢楅埀顒勵敂閿燂拷

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
    //闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺岀喖鎮ч崼鐔哄嚒闂佸憡鍨规慨鎾煘閹达附鍋愰悗鍦Т椤ユ繄绱撴担鍝勵€岄柛銊ョ埣瀵鏁愭径濠勵吅闂佹寧绻傞幉娑㈠箻缂佹鍘搁梺鍛婁緱閸犳宕愰幇鐗堢厸鐎光偓鐎ｎ剛鐦堥悗瑙勬礃鐢帟鐏掗柣鐐寸▓閳ь剙鍘栨竟鏇㈡⒑閸濆嫮鈻夐柛瀣у亾闂佺ǹ顑嗛幐鎼侊綖濠靛鏁嗗ù锝囨櫕椤㈠懘姊绘担渚敯閻忓繑鐟﹂弲璺何旈崘顏嗙暥闂佺粯姊婚崢褔鎮為崹顐犱簻闁圭儤鍨甸鈺呮煛鐎ｂ晝绐旈柡宀€鍠栭獮鎴﹀箛闂堟稒顔勬繝纰樻閸嬪懘鏁冮姀銈呰摕闁哄洢鍨归柋鍥ㄧ節闂堟稒绁╂俊顐ゅ仜椤啴濡堕崨顖滎啈闂佺硶鏅滈悧鐘荤嵁閸儱惟闁宠桨鑳堕濠囨⒑閻熼偊鍤熼柛瀣枛婵″爼鍩￠崨顔规嫼缂備礁顑嗛娆撳磿閹扮増鐓欓柣鐔哄閸犳﹢鏌ｅ☉鍗炴灕缂佺姵绋戦埥澶娾枎韫囧﹤浜惧┑鐘崇閻撶娀鏌熼鐔风瑨闁告梹绮嶇换娑㈠川椤愩垻浼堝┑顔硷攻濡炶棄鐣烽锕€绀嬫い鎰枎娴滈箖鏌涢锝嗙缁炬儳缍婇弻鈥愁吋鎼粹€茬爱闂佺ǹ顑嗛幐鎼侊綖濠靛鏁嗛柛灞剧敖閵娾晜鈷戦柛婵嗗椤箓鏌涢弮鈧崹鍧楃嵁閸愵喖顫呴柕鍫濇噹缁愭稒绻濋悽闈浶㈤悗姘间簽濡叉劙寮撮姀鈾€鎷绘繛杈剧到閹芥粎绮旈悜妯镐簻闁靛闄勫畷宀€鈧娲橀〃鍛达綖濠婂牆鐒垫い鎺嗗亾妞ゆ洩缍侀、鏇㈡晝閳ь剛绮绘繝姘仯闁搞儜鍐獓濡炪們鍎茬换鍫濐潖濞差亝顥堟繛鎴炶壘椤ｅ搫鈹戦埥鍡椾簼妞ゃ劌锕妴渚€寮崼婵嬪敹闂佸搫娲ㄩ崯鍧楀箯閸濆嫷娓婚柕鍫濇缁楁帡鏌ㄩ弴顏勵洭缂侇喗妫冮、姘跺焵椤掑嫬钃熼柍鈺佸暞婵挳鎮峰▎蹇擃仼缁剧偓濞婇幃妤€鈻撻崹顔界彯闂佸憡鎸鹃崰鎰┍婵犲洤閱囬柡鍥╁仜閼板灝鈹戞幊閸婃洟鏁冮敐鍥潟闁挎洖鍊归埛鎺楁煕鐏炲墽鎳勭紒浣哄缁绘稒寰勭€ｎ偆顦伴悗瑙勬磻閸楁娊鐛鈧畷婊勬媴閾忕懓濮介梻鍌欐祰濞夋洟宕抽敃鍌氱闁跨噦鎷�32闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺岀喖鎮ч崼鐔哄嚒闂佸憡鍨规慨鎾煘閹达附鍋愰悗鍦Т椤ユ繄绱撴担鍝勵€岄柛銊ョ埣瀵鏁愭径濠勵吅闂佹寧绻傞幉娑㈠箻缂佹鍘搁梺鍛婁緱閸犳宕愰幇鐗堢厸鐎光偓鐎ｎ剛鐦堥悗瑙勬礃鐢帟鐏掗柣鐐寸▓閳ь剙鍘栨竟鏇㈡⒑閸濆嫮鈻夐柛瀣у亾闂佺ǹ顑嗛幐鎼侊綖濠靛绠涘ù锝呮贡缁夐绱撻崒娆戭槮妞ゆ垵鎳樿棟妞ゆ牗绮屽鍙変繆閻愵亜鈧洜鎹㈤幇鏉跨疇闊洦鎸撮弸宥夋煏韫囧鈧牠鎮￠悢鍏肩厽闁哄倹瀵ч幉鎼佹煙閸愭彃顏柟顔荤矙椤㈡稑鈽夊Ο鏄忕檨闁诲氦顫夊ú婊堝极鐠囧樊鍤曞ù鐘差儛閺佸洭鏌ｉ幇顓у晱闁哥偟鏁诲缁樻媴閾忕懓绗￠梺鎼炲妼閸氬骞堥妸鈺佄у璺猴功閺屟囨⒑瑜版帗锛熼柣鎺炵畵閹€斥槈閵忥紕鍘遍柣蹇曞仒缁犳瑩顢旈崼婵堝姦濡炪倖甯婇懗鑸垫櫠闁秵鐓冪憸婊堝礈濮樿埖鍤岄柛鎾楀懐顦繝鐢靛Т濞层倝鎷戦悢鍏肩厪濠电偟鍋撳▍鍡涙煕鐎ｎ亜顏柡灞剧缁犳盯骞橀搹顐⑩偓顖炴⒑闂堚晝绋诲┑鐐诧工椤繐煤椤忓嫪绱堕梺鍛婃处閸樻粓濮€閿涘嫮顔曢梺鍝勵槹閸ㄧ敻顢旈锝勭箚闁告瑥顦慨鍥煃鐟欏嫬鐏寸€规洖鐖奸崺鈩冪節閸愵亗鍋掗梻鍌氬€风欢姘跺焵椤掑倸浠滈柤娲诲灡閺呭爼顢涘⿰鍛紲闁诲函缍嗘禍鐑界叕椤掑倵鍋撶憴鍕闁硅姤绮嶇粩鐔煎即閵忕姷鐤勭紓浣稿级閻╊垰顫忔繝姘＜婵炲棙鍔楅妶鏉库攽閻愬樊妲规繛鑼枎閻ｉ攱瀵奸弶鎴犵杸闂佸搫顦悘婵嬪吹閹寸偟绡€闁汇垽娼ф牎闂佽偐鎳撴晶鐣屽垝婵犳凹鏁嶆繝濠傛噽閿涙粓鏌ｈ箛鏇炰户闁哄拋鍋婇幃鏉款嚕鐎瑰窐闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺岀喖鎮ч崼鐔哄嚒闂佸憡鍨规慨鎾煘閹达附鍋愰悗鍦Т椤ユ繄绱撴担鍝勵€岄柛銊ョ埣瀵鏁愭径濠勵吅闂佹寧绻傞幉娑㈠箻缂佹鍘搁梺鍛婁緱閸犳宕愰幇鐗堢厸鐎光偓鐎ｎ剛鐦堥悗瑙勬礃鐢帟鐏掗柣鐐寸▓閳ь剙鍘栨竟鏇㈡⒑閸濆嫮鈻夐柛瀣у亾闂佺ǹ顑嗛幐鎼侊綖濠靛鏁嗛柛灞剧敖閵娾晜鈷戦柛婵嗗椤箓鏌涢弮鈧崹鍧楃嵁閸愵喖顫呴柣妯虹仛濞堟繈姊洪崫鍕垫Ъ婵炲娲滃Σ鎰版晝閸屾稈鎷绘繛杈剧到閹芥粓寮搁崘鈺€绻嗘い鎰╁灩椤忣厽顨ラ悙璇ч練缂佺姵鐩鏉懳旈埀顒佺閻愵剚鍙忔俊顖滃帶娴滈箖鎮楀鐐
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
    //濠电姷鏁告慨鐑藉极閸涘﹥鍙忓ù鍏兼綑閸ㄥ倻鎲搁悧鍫濈瑲闁稿顦甸弻鏇＄疀鐎ｎ亖鍋撻弴鐘电焼閻庯綆鍠楅悡鏇熴亜椤撶喎鐏ュù婊勭箞閺岀喖顢欓悾灞惧櫚濠殿喖锕︾划顖炲箯閸涱垳椹抽悗锝庝簼閻ｄ即姊绘担鍛婂暈闁规瓕顕ч～婵嬪Ω閳轰胶顔夐梺闈涚箞閸婃洟鐛姀锛勭闁瑰鍋熼幊鍐煕閹捐娼愬ǎ鍥э躬婵℃儼绠涢弴鐐茬厒闂備礁鎽滈崳銉╁磻婵犲倻鏆﹂柨婵嗘缁♀偓闂佸憡鍔х紓姘辫姳婵犳碍鈷戦柛婵嗗琚梺绋块叄娴滃爼宕洪埀顒併亜閹哄秶璐版繛鍫燁焽缁辨帡顢欓懖鈺侇杸閻庡灚婢樼€氼喗绂掗敃鍌氱畾鐟滃酣鎮楁ィ鍐┾拻濞达綀娅ｇ敮娑欍亜閵娿儲鍤囬柟顔ㄥ嫮绡€闁告洦浜為崜銊ヮ渻閵堝棙灏甸柛搴㈠姍閹垽宕卞▎鎰瀾闂婎偄娲﹀褰掑矗韫囨稒鈷掗柛顐ゅ枍缁堕亶鏌ｉ幒鏇燁棄闁宠鍨块幃鈺冣偓鍦Т椤ユ繄绱撴担鍝勑ｇ紒瀣浮婵＄敻宕熼姘敤闂佹眹鍨婚弫姝屾懌闂傚倷绀侀幖顐﹀磹閻㈢ǹ纾婚柟鍓х帛閳锋帡鏌涚仦鍓ф噮妞わ讣绠撻弻鐔烘嫚瑜忕壕鍧楁煛娓氬洤鏋涢摶鏍归敐鍫燁仩鐟滀即绠栧铏瑰寲閺囩偛鈷夐梺闈涙处閻╊垰顕ｉ幖浣哥睄闁割偆鍠撻崢鐢告煟鎼达絾鏆╅弸顏堝冀閿熺姵鈷戦柛婵勫劚閺嬪酣鏌熺喊鍗炰喊婵犫偓娓氣偓濮婃椽骞愭惔锝囩暤濠电偠顕滅粻鎾崇暦閻㈢ǹ鐒垫い鎺戝€荤壕钘壝归敐鍥ㄥ殌缂佽尙绮妵鍕敇閻樻彃骞嬮悗娈垮枦椤曆囧煡婢跺ň鍫柛娑卞灡濠㈡垿姊虹拠鎻掑毐缂傚秴妫欑粋宥呪堪閸曨剙寮块梺闈涚箳婵參寮ㄦ禒瀣厽闁归偊鍨伴惃鍝勵熆瑜庨惄顖炲蓟濞戙垹惟闁靛／鍌濇闂備椒绱紞渚€藟閹捐泛鍨濇繛鍡樺灦鐎氭岸鏌ｉ幇闈涘闁抽攱鍔欓弻鐔风暋閻楀牆娈楅悗瑙勬礃缁捇鐛崶顒€鍨傛い鎰╁€х槐鐪宔n闂傚倸鍊搁崐鎼佸磹妞嬪孩顐芥慨姗嗗墻閻掔晫鎲稿鍫罕闂備礁鎼崯顐︽偋韫囨稑鐭楅柡鍥ュ灪閻撶喖鏌ｉ幇顔芥毄闁哄棗鈥﹂梻鍌氬€搁崐鎼佸磹妞嬪海鐭嗗〒姘ｅ亾妤犵偛顦甸弫宥夊礋椤愩垻浜伴柣搴″帨閸嬫捇鏌涢弴銊ュ濞寸姾鍋愮槐鎾存媴閸撳弶效婵炲瓨绮犻崹璺侯潖婵犳艾纾兼繛鍡樺灥婵′粙姊哄ú璁崇盎缂傚秴锕ら悾宄扳攽閸℃瑦娈曢梺閫炲苯澧撮柛鈹惧亾濡炪倖甯婄粈渚€宕甸鍕厱闁圭偓娼欓崫铏光偓娈垮枟閻撯€崇暦婵傜ǹ鍗抽柣鏃囨腹濮规姊绘担铏瑰笡闁告梹鐗犲畷鐟扳攽閸繂鐏婇梺鍦檸閸犳鍩涢幋锔藉仭婵炲棗绻愰鈺冪磼閼碱剛甯涢柕鍥у楠炲鈹戦崶褏鐛╃紓鍌欑贰閸犳牠鎮ф繝鍌ゅ殫闁告洦鍨扮粻娑欍亜閹捐泛孝妤犵偞顨婂缁樻媴閸涘﹨纭€闂佺ǹ绨洪崐鏇犲弲濠碘槅鍨伴崥瀣礂濠婂吘鏃堟晲閸涱厽娈查梺缁樻尰閻燂箓濡甸崟顖氱睄闁稿本绮嶉幉妯衡攽閻愯尙澧涚紒顔奸叄閸┾偓妞ゆ帊绶￠崯蹇涙煕閻樺磭澧甸柟顔哄劜缁虹晫绮欓崹顔锯偓顓熶繆閵堝繒鍒伴柛鐕佸亰閹苯螖閳ь剟鈥﹂崸妤佸殝鐎电増顨忔禍婵堢矉閹烘嚚娲敂閸涱亝瀚奸梻浣哄帶椤洟宕愰弴鐐垫懃濠电姷鏁搁崑鐐差焽濞嗘挸纾归柛顭戝枤閺嗭箓鏌曡箛瀣偓鏍吹瀹€鍕厾闁告挻褰冮崢鍛婃叏鐟欏嫷娈滄慨濠呮缁辨帒螣閸濆嫷娼撻梻浣告贡閳峰牓宕戞繝鍌滄殾闁挎繂妫涚弧鈧梺鍛婂姧缂傛氨鑺辨繝姘拺闁告繂瀚埢澶愭煕鐎ｎ亜顏紒鍌涘浮閺佸啴鍩€椤掑嫭绠掗梻浣虹帛椤洭顢楅弻銉﹀殌闁秆勵殕閻撴瑦銇勯弮鈧崕鎶藉储鐎电硶鍋撳▓鍨灍婵炲吋鐟╅敐鐐测攽閸ヨ埖缍戦梺鍝勭Р閸撳崬饪伴崟顓犵槇闂佹眹鍨藉褑鈪撮梻浣呵归鍡涘箰婵犳艾桅闁告洦鍠氶悿鈧梺鍦亾濞兼瑩宕戝鍡欑閻庢稒顭囬惌濠勭磽瀹ュ拑韬€殿喖顭烽幃銏ゆ偂鎼达絿鏆伴柣鐔哥矊缁夌懓顕ｉ搹顐ｇ秶闁靛⿵绲肩花濠氭⒑闂堟稓澧曢柟鍐茬箻瀹曠敻宕堕浣哄幍濡炪倖姊归崕鎶藉储鐎电硶鍋撳▓鍨灍婵炲吋鐟╅敐鐐测攽鐎ｎ亞顢呴梺缁樺姇濡﹪鎮楅幘顔解拺闁煎鍊曢弸鎴犵磼椤旇偐孝妞ゎ厼鐏濋～婊堝焵椤掆偓閻ｇ兘顢涢悙鑼獓闂佺懓顕慨闈涒枔閹€鏀介幒鎶藉磹閺囥垹绠犻幖绮瑰墲閺嗘粓姊洪懡銈呮瀾缂侇喖鐭傞弻濠囨晲婢跺﹦鐣烘俊銈忕到閸燁垶鎮為懖鈹惧亾楠炲灝鍔氭俊顐ｇ洴瀹曘垽鏌嗗鍡椾画濠电姴锕ら崯鎾矗閸曨垱鐓忛柛鈩冾殔閺嗙偟鈧灚婢樼€氼喗绂掗敃鍌氱畾鐟滃秶鑺遍妷锔剧瘈缁剧増蓱椤﹪鏌涢妸銈呭祮鐎规洏鍎抽埀顒婄秵閸犳牜澹曠紒妯圭箚妞ゆ牗姘ㄥВ鐐烘煕鐎ｎ偅宕岀€规洘顨嗗鍕節娴ｅ壊妫滈梻鍌氬€风欢姘跺焵椤掑倸浠滈柤娲诲灡閺呭爼顢氶埀顒勫蓟瀹ュ洨纾兼俊顖濐嚙绾惧啿螖閻橀潧浠滄い鎴濐樀瀵偊宕橀鑲╁姦濡炪倖甯掗崯鎵不娴犲鈷掗柛灞剧懄缁佺増绻涙径瀣鐎规洘濞婇弫鍐磼濮橀硸鍞甸梻浣芥硶閸ｏ箓骞忛敓锟�
    //闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊椤掑﹦绋忔繝銏ｆ硾椤戝洭銆呴幓鎹楀綊鎮╁顔煎壈缂佺偓鍎冲锟犲蓟濞戙垹绠涙い鎺嶈閺嬫瑩姊虹紒妯虹瑐缂侇喗鐟╁璇测槈閵忕姷鐤€闂佺ǹ绻愰幗婊堝礄瑜版帗鈷戦柛婵勫劚閺嬪酣鏌熺喊鍗炰喊濠碘剝鎸抽獮鎺楀箣椤撶喎鍏婇梻浣瑰缁诲倿骞婂鍡欑彾闁哄洢鍨洪埛鎺戙€掑锝呬壕闂侀€炲苯澧伴柛瀣洴閹崇喖顢涘☉娆愮彿濡炪倖鐗滈崑鐐烘偂閺囥垺鐓涢柛鎰剁到娴滈箖姊虹紒姗嗘畼濠电偐鍋撻梺闈涙缁€浣界亙闂佸憡渚楅崰鏍焵椤掑倸鍘撮柡灞剧☉閳规垿宕卞Δ濠佺磻婵＄偑鍊栭弻銊╁箠韫囨洘宕叉繝闈涱儐閸嬨劑姊婚崼鐔峰瀬闁靛繈鍊栭悡鏇炩攽閻樺弶鍣归柛姘贡缁辨帡顢欓懞銉㈡嫻缂備浇妗ㄧ划娆忕暦閵婏妇绡€闁告洦鍋勭粻鐐烘⒒閸屾瑦绁版い顐㈩樀瀹曟洖鐣烽崶褜妫滃銈嗘尵閸嬬偛袙閸曨偀鏀介柨娑樺娴滃ジ鏌涙繝鍐ㄥ鐎殿喗鐓￠、鏃堝幢濞嗗海鐟濋梻浣筋潐瀹曟ê鐣烽棃娑氭殼濞撴埃鍋撻柡宀€鍠栭獮鎴﹀箛闂堟稒顔勭紓鍌欒兌婵灚绻涙繝鍥ц摕闁绘棁銆€閸嬫挸鈽夊▎妯煎姺缂備胶濮甸幐濠氬箞閵娧勫枂闁告洦鍘煎銊モ攽椤旂》宸ラ柟纰卞亝閺呫儵姊洪幖鐐插妧闁告劕澧介埀顒佸▕濮婅櫣鎷犻幓鎺濆妷闂佸憡鍨电紞濠傜暦濠婂啠鏋庨柟缁㈠灙閸嬫捇鏁冮崒姘卞€炲銈嗗笂閻掞箑鈻嶉弽顓熲拺闁告稑锕ユ径鍕煕閹惧鎳囨い銏℃尭閳藉濮€閳锯偓閹锋椽姊洪崨濠勨槈闁挎洏鍎抽埀顒€鐏氶悷鈺呭蓟濞戞ǚ妲堥柛妤冨仦閻忔捇姊洪柅鐐茶嫰婢у鏌涘Ο鍏兼珪闁瑰箍鍨归埥澶愬閻樻鍚呴梻浣虹帛閸旀牕顭囧▎鎾虫辈婵°倕鎳忛埛鎴炴叏閻熺増鎼愬┑鈥虫健閹ǹ绠涢幘棰濇殥n闂傚倸鍊搁崐鎼佸磹妞嬪海鐭嗗〒姘ｅ亾妤犵偞顨呴オ浼村醇濠靛牏宕堕梻浣虹《閸撴繄绮欓幋鐘差棜闁秆勵殕閻撴洟鏌熼柇锕€骞橀柕鍫熸尦閺岋紕鈧綆鍋呴ˉ鍫ユ煛瀹€鈧崰鎰焽韫囨柣鍋呴柛鎰ㄦ櫓閳ь剙绉瑰娲传閸曨剙顎涢梺鍛婃尰閻╊垶宕洪埀顒併亜閹哄棗浜鹃梺鍛娚戠划鎾崇暦閹达箑绠荤紓浣诡焽閸橀亶姊鸿ぐ鎺戜喊闁告ê銈稿畷婵嬪Χ婢跺鍘搁梺鍛婄矆缁€渚€鐛Δ鍐＜缂備焦顭囩粻鐐翠繆椤愩垹鏆欓柍钘夘槸铻ｇ€瑰嫰顣︽竟鏇㈡⒑閼恒儍顏堟晬婢跺ň妲堥柕蹇曞Х椤︽澘顪冮妶鍡欏婵ǜ鍔戦幃楣冨传閸曘劍鏂€濡炪倖姊婚妴瀣绩缂佹ǜ浜滈柍鍝勫暕閸掑ode


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
      if (initVal != nullptr)
        initVal->genCode();
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
    para_operands.clear();
    para_node->genCode();
    }
    //std::cout<<"paranode_gencode"<<std::endl;
    BasicBlock *bb = builder->getInsertBB();
    // fprintf("ssssssssss%s")
    new CallInstruction(dst, symbolEntry, para_operands, bb);
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
        //std::cout<<"fuck expr"<<std::endl;
        para_operands.push_back(para_expr->getOperand());
        para_expr->genCode();
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
        // if(！2)
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



//闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺岀喖鎮ч崼鐔哄嚒闂佸憡鍨规慨鎾煘閹达附鍋愰悗鍦Т椤ユ繄绱撴担鍝勵€岄柛銊ョ埣瀵鏁愭径濠勵吅闂佹寧绻傞幉娑㈠箻缂佹鍘搁梺鍛婁緱閸犳宕愰幇鐗堢厸鐎光偓鐎ｎ剛鐦堥悗瑙勬礃鐢帟鐏掗柣鐐寸▓閳ь剙鍘栨竟鏇㈡⒑閸濆嫮鈻夐柛瀣у亾闂佺ǹ顑嗛幐鎼侊綖濠靛鏁嗛柛灞剧敖閵娾晜鈷戦柛婵嗗椤箓鏌涢弮鈧崹鍧楃嵁閸愵喖顫呴柕鍫濇噹缁愭稒绻濋悽闈浶㈤悗姘间簽濡叉劙寮撮姀鈾€鎷绘繛杈剧到閹芥粎绮旈悜妯镐簻闁靛闄勫畷宀€鈧娲橀〃鍛达綖濠婂牆鐒垫い鎺嗗亾妞ゆ洩缍侀、鏇㈡晝閳ь剛绮绘繝姘仯闁搞儜鍐獓濡炪們鍎茬换鍫濐潖濞差亝顥堟繛鎴炶壘椤ｅ搫鈹戦埥鍡椾簼妞ゃ劌锕妴渚€寮崼婵嬪敹闂佸搫娲ㄩ崯鍧楀箯濞差亝鐓熼柣妯哄帠閼割亪鏌涢弬璺ㄧ劯鐎殿喗鎮傞獮瀣晜閻ｅ苯骞愰梺璇插嚱缂嶅棙绂嶉崼鏇熷亗闁稿繒鈷堝▓浠嬫煟閹邦垰鐨虹紒鐘差煼閺岋紕浠︾拠鎻掝瀷闂侀€炲苯澧剧紓宥勭椤洩顦崇紒鏃傚枎椤粓鍩€椤掆偓椤繘鎼归崷顓犵厯闂佸憡鐓妶鍛創濠电姷顣藉Σ鍛村磻閸曨偒鐒介柨鐔哄Х瀹撲礁顭块懜闈涘閹喖姊洪崘鍙夋儓闁稿﹤顭峰畷锝夊幢濞戞瑢鎷虹紓鍌欑劍钃遍柣鎾卞劦閺岀喓鎷犺缁♀偓闂佽鍠楅敃銏ゃ€佸璺虹劦妞ゆ帒瀚拑鐔兼煟閺傚灝顥忔俊鎻掔秺楠炴牜鍒掗崗澶婁壕闁告劘灏欓娲⒒閸屾瑦绁版俊妞煎姂閹偤鏁冮崒姘鳖唹闂佹悶鍎滅仦鑺ヮ吙婵＄偑鍊栫敮鎺斺偓姘煎墮濞插潡姊绘担铏广€婇柛鎾寸箞閵嗗啳绠涢弬娆惧殼闂佺懓澧界划顖炲煕閹烘嚚褰掓晲閸粳鎾剁棯椤撶偟鍩ｉ柡灞剧洴閺佹劘绠涢弴鐘虫婵°倗濮烽崑娑氭崲濮椻偓瀵偊骞樼紒妯绘闂佽法鍣﹂幏锟�?


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
    //闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺岀喖鎮ч崼鐔哄嚒闂佸憡鍨规慨鎾煘閹达附鍋愰悗鍦Т椤ユ繄绱撴担鍝勵€岄柛銊ョ埣瀵鏁愭径濠勵吅闂佹寧绻傞幉娑㈠箻缂佹鍘搁梺鍛婁緱閸犳宕愰幇鐗堢厸鐎光偓鐎ｎ剛鐦堥悗瑙勬礃鐢帟鐏掗柣鐐寸▓閳ь剙鍘栨竟鏇㈡⒑閸濆嫮鈻夐柛瀣у亾闂佺ǹ顑嗛幐鎼侊綖濠靛鏁嗛柛灞剧敖閵娾晜鈷戦悹鍥ｂ偓宕囦哗闁诲孩鐭崡鎶芥偘椤旂⒈娼ㄩ柍褜鍓熼妴浣糕枎閹炬潙娈熼梺闈涱檧鐎靛本绂掕濮婂宕掑▎鎴М闂佸湱鈷堥崑濠傜暦閹版澘閱囬柡鍥╁仧閿涙瑩姊洪崫鍕枆闁稿鍋撻梺绋款儐閹告悂锝炲┑瀣垫晢闁稿本绨介妸鈺傗拺闁告繂瀚～锕傛煕閺冣偓閸ㄥ潡鐛崘顔碱潊闁靛牆鎳庣粣娑欑節閻㈤潧孝閻庢凹浜炲Σ鎰板即閵忊檧鎷绘繛杈剧到閹芥粎绮旈悜妯镐簻闁靛闄勫畷宀€鈧娲橀〃鍛达綖濠婂牆鐒垫い鎺嗗亾妞ゆ洩缍侀、鏇㈡晝閳ь剛绮绘繝姘仯闁搞儜鍐獓濡炪們鍎茬换鍫濐潖濞差亝顥堟繛鎴炶壘椤ｅ搫鈹戦埥鍡椾簼妞ゃ劌锕妴渚€寮崼婵嬪敹闂佸搫娲ㄩ崯鍧楀箯濞差亝鐓熼柣妯哄帠閼割亪鏌涢弬鑳闁宠绉归獮鍥偋閸垹骞嶉梻浣告啞閸垶宕愰弽顐熷亾濮樼偓瀚�


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
                //闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺岀喖鎮ч崼鐔哄嚒闂佸憡鍨规慨鎾煘閹达附鍋愰悗鍦Т椤ユ繄绱撴担鍝勵€岄柛銊ョ埣瀵鏁愭径濠勵吅闂佹寧绻傞幉娑㈠箻缂佹鍘搁梺鍛婁緱閸犳宕愰幇鐗堢厸鐎光偓鐎ｎ剛鐦堥悗瑙勬礃鐢帟鐏掗柣鐐寸▓閳ь剙鍘栨竟鏇㈡⒑閸濆嫮鈻夐柛瀣у亾闂佺ǹ顑嗛幐鎼侊綖濠靛鍊锋い鎺嗗亾妞ゅ骏鎷�
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
    //闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺岀喖鎮ч崼鐔哄嚒闂佸憡鍨规慨鎾煘閹达附鍋愰悗鍦Т椤ユ繄绱撴担鍝勵€岄柛銊ョ埣瀵鏁愭径濠勵吅闂佹寧绻傞幉娑㈠箻缂佹鍘搁梺鍛婁緱閸犳宕愰幇鐗堢厸鐎光偓鐎ｎ剛鐦堥悗瑙勬礃鐢帟鐏掗柣鐐寸▓閳ь剙鍘栨竟鏇㈡⒑閸濆嫮鈻夐柛瀣у亾闂佺ǹ顑嗛幐鎼侊綖濠靛鏁嗗ù锝囨櫕椤㈠懘姊绘笟鈧埀顒傚仜閼活垱鏅堕鐣岀瘈闁逞屽墴閺屽棗顓奸崨顖ょ幢闂備線娼ч悧鍡浰囬鐐插嚑濞撴埃鍋撻柡宀€鍠栭獮鎴﹀箛椤掑倸甯块梻浣呵归鍡氭懌婵烇絽娲ら敃顏呬繆閸洖妞藉ù锝嚽规竟瀣磽閸屾瑧顦︽い锔诲灦閹儲绺界粙鑳憰闂佺粯妫冮ˉ鎾诲汲鐎ｎ喗鐓涘璺侯儏椤曟粍绂嶅☉銏＄厽闁绘柨鎽滈惌瀣攽閳╁啯鍊愮€规洖鐖兼俊姝岊槻鐎殿喗濞婂缁樻媴閾忕懓绗″┑顔硷功閹虫捇鈥旈崘顔煎瀭妞ゆ洖鎳愰崝鐑芥⒑閹稿海鈽夐悗姘煎枛铻炴い鏍仦閻撴稑顭跨捄渚剾闁稿簺鍎叉穱濠囧箵閹烘梻楔闂佽鍠楅〃鍛达綖濠靛鏁嗛柛灞剧敖閵娾晜鈷戦柛婵嗗椤箓鏌涢弮鈧崹鍧楃嵁閸愵喖顫呴柕鍫濇噹缁愭稒绻濋悽闈浶㈤悗姘间簽濡叉劙寮撮姀鈾€鎷绘繛杈剧到閹芥粎绮旈悜妯镐簻闁靛闄勫畷宀€鈧娲橀〃鍛达綖濠婂牆鐒垫い鎺嗗亾妞ゆ洩缍侀、鏇㈡晝閳ь剛绮绘繝姘€甸梻鍫熺⊕閹叉悂鏌熼搹顐ょ疄婵﹥妞藉Λ鍐ㄢ槈鏉堛剱銈夋⒑缁嬪潡顎楃€规洦鍓熼崺銏ゅ箻鐠囪尙鍘搁梺鍛婁緱閸犳岸宕㈤崹顐＄箚闁绘劕妯婇崕蹇撐旈悩铏€愮€规洏鍨介獮姗€顢欓悾灞藉箰濠电偠鎻紞鈧い顐㈩槸閻ｅ嘲鐣濋埀顒勫焵椤掍緡鍟忛柛锝庡櫍瀹曟粓鎮㈤梹鎰畾闂佸壊鍋呭ú鏍嵁閵忊€茬箚闁靛牆鎷戝妤冪磼閹插鐣垫慨濠傤煼瀹曟帒鈻庨幇顔哄仒婵犵數鍋涢ˇ鏉棵洪悢椋庢殾濞村吋娼欑粻娑㈡煛婢跺﹦浠㈤柛銈嗗灦缁绘繈鎮介棃娴讹綁鏌ら悷鏉库挃缂侇喖顭烽、娑㈡倷鐎电ǹ骞楅梻浣侯攰閹活亞寰婇崐鐕佹毐闂傚倷绀侀幖顐﹀箠韫囨稑绠栭柛宀€鍋涢拑鐔兼煥閻斿搫孝缁炬儳鍚嬫穱濠囶敍濠靛嫧鍋撻埀顒勬煕鐎ｎ偅宕岀€殿喕绮欓、鏇㈡偄閾氬倸顥氭繝娈垮枟閿曗晠宕滃☉姘辩煋婵炲樊浜濋悡鐔哥箾閹存繂鑸规繛鍛Ф閳ь剚顔栭崰妤呭箰閾忣偅鍙忛柍褜鍓熼弻宥夊煛娴ｅ憡娈插銈呯箳閸犳牕顫忕紒妯诲闁告稑锕ラ崕鎾绘⒑缁嬪尅宸ラ柣鏍帶閻ｇ兘骞囬弶鍨敤濡炪倖鍔楅崰搴㈢閻愵剚鍙忔慨妤€妫楁晶鎵磼婢跺銇濋柡宀嬬磿娴狅妇鎷犻幓鎺濇綆闂備浇顕栭崰鎾诲垂閽樺鏆﹂柕濠忓缁♀偓闂佸憡娲︽禍鐐靛閸ф鈷掗柛灞剧懅椤︼妇绱撳鍜冭含閽樼喖鏌熼幑鎰靛殭缂佲偓閸屾稒鍙忔俊鐐额嚙娴滈箖鎮楀▓鍨珮闁稿锕悰顔嘉熼崗鐓庣／闂佹儳绻愬﹢閬嶆偤濮椻偓濮婄粯鎷呴搹鐟扮闂佺粯顨嗗璇测枎閵忕媭娼╅悹娲細閹芥洟姊洪棃娴ュ牓寮插⿰鍫濈；闁告洦鍨遍悡鏇熺箾閹寸儑鍏柛鏃傚枎闇夐柣鎾抽椤忣厽鎱ㄦ繝鍐┿仢鐎规洜鍏橀、姗€鎮㈤崫鍕┑鐘垫暩閸嬬偤骞愮粙鍖¤€块弶鍫氭櫅閸ㄦ繈鏌嶉崫鍕櫣缂佺姷绮妵鍕籍閸屾稒鐝梺鍛婄懃濡繂顫忛搹鍦＜婵☆垵宕甸崣鍡涙⒑绾拋鍤嬬紒缁樼箞楠炲啫顫滈埀顒勩€佸璺虹劦妞ゆ帒瀚拑鐔兼煟閺傚灝顥忔俊鎻掔秺楠炴牜鍒掗崗澶婁壕闁告劘灏欓娲⒒閸屾瑦绁版俊妞煎姂閹偤鏁冮崒姘鳖唹闂佹悶鍎滅仦鑺ヮ吙婵＄偑鍊栫敮鎺斺偓姘煎墮濞插潡姊绘担铏广€婇柛鎾寸箞閵嗗啳绠涢弬娆惧殼闂佺懓澧界划顖炲煕閹烘嚚褰掓晲閸粳鎾剁棯椤撶偟鍩ｉ柡灞剧洴閹晠宕橀幓鎺濇綒闁诲孩顔栭崰鏍箟閿涘嫭宕叉繝闈涙－濞尖晜銇勯幒鎴濅簽闁哥偟鏁诲缁樻媴閼恒儳銆婇梺闈╃秶缁犳捇鐛箛娑欐櫢闁跨噦鎷�
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
        //闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺岀喖宕滆鐢盯鏌涚€ｎ亜鈧綊濡甸崟顖氱疀闁宠桨璁查崑鎾诲箻缂佹ê鈧灝鈹戦悩鍙夋悙缂佺嫏鍥ㄧ厱妞ゆ劧绲块。鍙夌箾閸涱厽鍤囬柡宀嬬秮婵℃悂鏁冮埀顒勬倶閹绢喗鐓涚€光偓閳ь剟宕伴幇鍏洭鎮ч崼鐔峰妳闂佺偨鍎遍崯璺ㄧ不閿濆鈷掑ù锝堝Г閵嗗啴鏌ｉ幒鐐电暤妤犵偞鍨垮畷鎯邦檨闁搞倖娲橀妵鍕箳閹存繍浠奸柛銉︽尦濮婅櫣鍖栭弴鐐测拤濡炪們鍔岀换鎴犫偓闈涖偢楠炴牠骞庣粚鏈緊lentry闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺岀喖鎮ч崼鐔哄嚒闂佸憡鍨规慨鎾煘閹达附鍋愰悗鍦Т椤ユ繄绱撴担鍝勵€岄柛銊ョ埣瀵鏁愭径濠勵吅闂佹寧绻傞幉娑㈠箻缂佹鍘搁梺鍛婁緱閸犳宕愰幇鐗堢厸鐎光偓鐎ｎ剛鐦堥悗瑙勬礃鐢帟鐏掗柣鐐寸▓閳ь剙鍘栨竟鏇㈡⒑閸濆嫮鈻夐柛瀣у亾闂佺ǹ顑嗛幐鎼侊綖濠靛鏁嗛柛灞剧敖閵娾晜鈷戦柛婵嗗椤箓鏌涢弮鈧崹鍧楃嵁閸愵喖顫呴柕鍫濇閸庮亪姊洪懡銈呮瀾婵犮垺锕㈤獮鍡涙倷閻戞ǚ鎷绘繛杈剧悼閻℃柨顭囬幇鐗堢厱閹兼番鍨归埢鏇燁殽閻愯榫氱紒鐘崇洴瀵潙螖閳ь剚绂嶉悙顒佸弿婵妫楁晶鎵磼婢跺銇濋柡宀嬬磿娴狅妇鎷犻幓鎺濇綆闂備浇顕栭崰鎾诲垂娴犲绠圭憸鐗堝笚閸嬶繝鏌℃径瀣嚋闁稿寒浜濠氬磼濮橆兘鍋撻幖浣哥９闁归棿鐒﹂崑瀣煕閹伴潧鏋涢柣銈囧亾缁绘繃绻濋崒娑樻珴闂佺ǹ顑嗛幐鎼侊綖濠靛鏁嗛柛灞剧敖閵娾晜鈷戦柛婵嗗椤箓鏌涢弮鈧崹鍧楃嵁閸愵喖顫呴柕鍫濇噹缁愭稒绻濋悽闈浶㈤悗姘间簽濡叉劙寮撮姀鈾€鎷绘繛杈剧到閹芥粎绮旈悜妯镐簻闁靛闄勫畷宀€鈧娲橀〃鍛达綖濠婂牆鐒垫い鎺嗗亾妞ゆ洩缍侀、鏇㈡晝閳ь剛绮绘繝姘仯闁搞儜鍐獓濡炪們鍎茬换鍫濐潖濞差亝顥堟繛鎴炶壘椤ｅ搫鈹戦埥鍡椾簼妞ゃ劌锕妴渚€寮崼婵嬪敹闂佸搫娲ㄩ崯鍧楀箯濞差亝鐓熼柣妯哄帠閼割亪鏌涢弬璺ㄧ劯鐎殿喗鎮傞獮瀣晜閻ｅ苯骞愰梺璇插嚱缂嶅棙绂嶉崼鏇熷亗闁稿繒鈷堝▓浠嬫煟閹邦垰鐨虹紒鐘差煼閺岀喖顢欓悾宀€鐓夐梺鐟扮－閸嬨倖淇婇悜鑺ユ櫆缂佹稑顑勯幋鐑芥⒒閸屾艾鈧绮堟笟鈧獮鏍敃閿曗偓绾惧綊鏌涢锝嗙缁炬儳缍婇弻鈥愁吋鎼粹€茬爱闂佺ǹ顑嗛幐鎼侊綖濠靛鏁嗛柛灞剧敖閵娾晜鈷戦柛婵嗗椤箓鏌涢弮鈧崹鍧楃嵁閸愵喖顫呴柕鍫濇噹缁愭稒绻濋悽闈浶㈤悗姘间簽濡叉劙寮撮姀鈾€鎷绘繛杈剧到閹芥粎绮旈悜妯镐簻闁靛闄勫畷宀€鈧娲橀〃鍛达綖濠婂牆鐒垫い鎺嗗亾妞ゆ洩缍侀、鏇㈡晝閳ь剛绮绘繝姘仯闁搞儜鍐獓濡炪們鍎茬换鍫濐潖濞差亝顥堟繛鎴炶壘椤ｅ搫鈹戦埥鍡椾簼妞ゃ劌锕妴渚€寮崼婵嬪敹闂佸搫娲ㄩ崑鐔割殽閸ヮ剚鈷戠紓浣姑悘銉︿繆椤愶綆娈曢柡鍛埣楠炴﹢顢欓悾灞藉箰濠电偠鎻紞鈧い顐㈩槸閻ｅ嘲鐣濋埀顒勫焵椤掍緡鍟忛柛锝庡櫍瀹曟粓鎮㈤梹鎰畾闂佸壊鍋呭ú鏍嵁閵忊€茬箚闁靛牆鎷戝妤冪磼閹插鐣遍柍瑙勫灴閹瑩寮堕幋鐘辨闂備浇顫夐悺鏇㈩敋瑜旈垾鏃堝礃閳哄喚娴勯柣搴秵娴滄繈鎮楅鍕拺閻熸瑥瀚粈鍐煕閳哄倻澧电€规洘鍨佃灒閻犱礁纾粻姘舵⒑闂堟稓澧曢柛濠傛啞缁傚秵銈ｉ崘鈹炬嫼闂佺鍋愰崑娑㈠焵椤掍胶澧甸柟顔ㄥ洦鍋愰柤濮愬€曠粊锕傛椤愩垺澶勭紒瀣浮瀹曟劖绻濆顓犲幘闂佽鍘界敮鎺楀礉濠婂嫮绠鹃柛娑卞亜閻忓弶鎱ㄦ繝鍐┿仢鐎规洦鍋婂畷鐔碱敃閻旇渹澹曢梺鍓插亝濞叉牜澹曡ぐ鎺撶厸鐎广儱楠告禍鎰版煕鐎ｎ偅灏い顐ｇ箞椤㈡宕掑┃鐐姂濮婃椽宕崟顕呮蕉闂佸憡姊归崹鍧楃嵁閸愵喖顫呴柣妯虹仛濞堟繈姊绘笟鍥у伎闂傚嫬瀚伴弫宥嗙瑹閳ь剙顫忕紒妯诲闁荤喖鍋婇崵瀣磽娴ｇ瓔鍤欓柛濠傤煼楠炴垿濮€閵堝懐顓洪梺缁橈供閸嬪嫭绂嶉悙顒佸弿婵妫楁晶鎵磼婢跺銇濋柡宀嬬磿娴狅妇鎷犻幓鎺濇綆闂備浇顕栭崰鎾诲垂閽樺鏆﹂柕濠忓缁♀偓闂佸憡娲︽禍鐐靛閸ф鈷掗柛灞剧懅椤︼妇绱撳鍜冭含閽樼喖鏌熼幑鎰靛殭缂佲偓閸屾稒鍙忔俊鐐额嚙娴滈箖鎮楀▓鍨珮闁稿锕悰顔嘉熼崗鐓庣彴闂佸憡鐟ラˇ钘壩涢悢鍏尖拻濞撴埃鍋撴繛浣冲洦鍋嬮柛鈩冦亗濞戞鏃堝椽娴ｈ娅嗛梻浣稿閸嬪懎煤濮椻偓閸╂盯骞嬮敂钘変化闂佽鍘界敮鎺撲繆婵傚憡鐓涢悗锝庡亞閹冲嫰鏌曢崶褍顏┑顔瑰亾闂佹寧绋戠€氼噣鎮℃径濞㈡棃鎮╅棃娑楃捕濡炪倧绠撴禍鍫曞春閳ь剚銇勯幒鎴姛缂佸鏁婚弻娑氣偓锝庝簻椤忣厽銇勯姀鈩冾棃鐎规洖銈稿鎾偄閸欏顏归梻浣藉吹婵灚绂嶆禒瀣閻忕偠袙閺嬪秹鏌熼幍顔碱暭闁绘挻鐟╅弻娑㈠箣濞嗗繆鍋撻弽顐熷亾濮樼偓瀚�?s闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺屸€愁吋鎼粹€崇闂佹椿鍘介〃鍡欐崲濞戞﹩鍟呮い鏃囧吹閸戝綊姊虹粙娆惧剱闁绘濞€楠炲啫螖閳ь剟鍩ユ径濞炬瀺妞ゆ挆鍌炲仐濡炪們鍨哄Λ鍐ㄧ暦婵傜ǹ鍗抽柣鏂垮级鐎氬ジ姊洪懡銈呅ｅù婊€绮欏畷婵嗙暆閸曨偄鍤戦柟鑲╄ˉ濡插懘鎮㈤崱娑欏仭婵炲棗绻愰顐︽煙閸愬弶鍤囬柡宀€鍠愰ˇ鐗堟償閳辨帩鍓氶妵鍕晜閽樺鏋欓梺璇″枟椤ㄥ懘锝炲┑瀣垫晢闁稿本绨介妸鈺傗拺闁告繂瀚～锕傛煕閺冣偓閸ㄥ潡鐛崘顔碱潊闁靛牆鎳庣粣娑欑節閻㈤潧孝閻庢凹浜炲Σ鎰板即閵忊檧鎷绘繛杈剧到閹芥粎绮旈悜妯镐簻闁靛闄勫畷宀€鈧娲橀〃鍛达綖濠婂牆鐒垫い鎺嗗亾妞ゆ洩缍侀、鏇㈡晝閳ь剛绮婚懡銈囩＝濞达絽顫栭鍕ㄦ灁闁绘柨鍚嬮埛鎴犵磼鐎ｎ偄顕滄繝鈧幍顔剧＜妞ゆ棁鍋愭晥濡炪們鍨哄Λ鍐箖閵忋倕绀傞柤娴嬫櫅楠炲秹鏌ｆ惔锝嗗殌閻庢凹鍘奸…鍨熸担鏇秮閹筹繝濡堕崒姘闁荤喐鐟ョ€氼厾绮堥崼婵愮唵鐟滃酣銆冩繝鍥╁祦闊洦绋戝婵囥亜閹炬鍟崐顖氣攽閻橆喖鐏辨繛澶嬬洴椤㈡牠宕卞缁樼€洪梺闈涚箞閸婃牠鍩涢幋锔界厱婵犻潧妫楅鈺呮煃瑜滈崜娆撴偉閻撳海鏆﹂柟鐗堟緲閸愨偓濡炪倖鍔楅崰搴㈢閻愵剚鍙忔慨妤€妫楁晶鎵磼婢跺銇濋柡宀嬬磿娴狅妇鎷犻幓鎺濇綆闂備浇顕栭崰鎾诲垂閽樺鏆﹂柕濠忓缁♀偓闂佸憡娲︽禍鐐靛閸ф鈷掗柛灞剧懆閸忓矂鏌熼搹顐ｅ磳鐎规洜鏁诲浠嬵敇閻愭鍞甸梻浣芥硶閸ｏ箓骞忛敓锟�
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
        fprintf(stderr, "Oops!闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺岀喖鎮ч崼鐔哄嚒闂佸憡鍨规慨鎾煘閹达附鍋愰悗鍦Т椤ユ繄绱撴担鍝勵€岄柛銊ョ埣瀵鏁愭径濠勵吅闂佹寧绻傞幉娑㈠箻缂佹鍘搁梺鍛婁緱閸犳宕愰幇鐗堢厸鐎光偓鐎ｎ剛鐦堥悗瑙勬礃鐢帟鐏掗柣鐐寸▓閳ь剙鍘栨竟鏇㈡⒑閸濆嫮鈻夐柛瀣у亾闂佺ǹ顑嗛幐鎼侊綖濠靛鏁嗛柛灞剧敖閵娾晜鈷戦柛婵嗗椤箓鏌涢弮鈧崹鍧楃嵁閸愵喖顫呴柕鍫濇噹缁愭稒绻濋悽闈浶㈤悗姘间簽濡叉劙寮撮姀鈾€鎷绘繛杈剧到閹芥粎绮旈悜妯镐簻闁靛闄勫畷宀€鈧娲橀〃鍛达綖濠婂牆鐒垫い鎺嗗亾妞ゆ洩缍侀、鏇㈡晝閳ь剛绮绘繝姘仯濡わ附瀵ч鐘电磼濞戞绠绘慨濠冩そ瀹曠兘顢樺┃鎯т壕閻庯綆鍠栫粻顖炴煥閻曞倹瀚�");//闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺岀喖鎮ч崼鐔哄嚒闂佸憡鍨规慨鎾煘閹达附鍋愰悗鍦Т椤ユ繄绱撴担鍝勵€岄柛銊﹀▕閸┾偓妞ゆ帒鍊归崵鈧柣搴㈠嚬閸樺ジ鈥﹂崹顔ョ喖鎳栭埡鍐姼婵＄偑鍊曠换鎰版偋婵犲洤鐓曢柟杈鹃檮閸嬶綁鏌熼鐔风瑨濠碘€炽偢閺岋紕鈧綆鍋勯悘鎾煛瀹€瀣？闁逞屽墾缂嶅棙绂嶉崼鏇熷亗闁稿繒鈷堝▓浠嬫煟閹邦垰鐨虹紒鐘差煼閺岀喖顢欓悾宀€鐓夐梺鐟扮－閸嬨倖淇婇悜鑺ユ櫆缂佹稑顑勯幋鐑芥⒒閸屾艾鈧绮堟笟鈧獮鏍敃閿曗偓绾惧綊鏌涢锝嗙缁炬儳缍婇弻鈥愁吋鎼粹€茬爱闂佺ǹ顑嗛幐鎼侊綖濠靛鏁嗛柛灞剧敖閵娾晜鈷戦柛婵嗗椤箓鏌涢弮鈧崹鍧楃嵁閸愵喖顫呴柕鍫濇噹缁愭稒绻濋悽闈浶㈤悗姘间簽濡叉劙寮撮姀鈾€鎷绘繛杈剧到閹芥粎绮旈悜妯镐簻闁靛闄勫畷宀€鈧娲橀〃鍛达綖濠婂牆鐒垫い鎺嗗亾妞ゆ洩缍侀、鏇㈡晝閳ь剛绮婚懡銈囩＝濞达絽顫栭鍫濈闁荤喖鍋婂〒濠氭煏閸繃顥滃┑顔ㄥ懐纾奸弶鍫涘妽缁€鍐磼鐎ｎ亶妯€鐎殿喖顭锋俊鐑芥晜閹冪闂佽娴烽幊鎾垛偓姘煎幖椤灝螣娴ｆ洩缍侀幊锟犲Χ閸屾矮澹曢柣鐔哥懃鐎氼厾浜搁鐘电＜缂備焦锕懓璺ㄢ偓娈垮枛椤攱淇婇幖浣哥厸闁稿本鐭花濠氭⒒娴ｅ憡鍟炵紒瀣灴閺佸啴鏁傞挊澶樺殼闂佸憡娲﹂崹閬嶅煕閹寸姵鍠愰柣妤€鐗嗘穱顖涗繆椤愶絽鐏撮柡灞剧缁犳盯寮幘鍏夊亾鐠恒劉鍋撳▓鍨珮闁稿锕悰顔嘉熼懖鈺冿紲濠碘槅鍨崇划顖烆敂閻斿吋鈷掑ù锝堝Г绾爼鏌涢敐蹇曠暤妤犵偛绻橀弫鎾绘晸閿燂拷?
        assert(se != nullptr);      //闂傚倸鍊搁崐鎼佸磹瀹勬噴褰掑炊瑜夐弸鏍煛閸ャ儱鐏╃紒鎰殜閺屸€愁吋鎼达絼绮堕梺缁樻煥閹测剝鍒婇幘顔界叄闊洦鎸剧粔娲煃閻熸壆绠婚柡宀嬬秮閹垽宕妷锕€娅戦梺璇插閸戝綊宕滈悢绗衡偓渚€寮崒娑樼／闁荤偞绋堥崜婵嬶綖瀹ュ鈷戦柛锔诲弨濡炬悂鏌涢妸褍顣肩紒鍌氱У閵堬綁宕橀埞鐐濠电偠鎻紞鈧┑顔哄€楀☉鐢稿醇閺囩喓鍘遍梺缁樏崯鎸庢叏瀹ュ洠鍋撳▓鍨灈闁硅绱曢幑銏犫攽閸♀晜鍍靛銈嗗笒閸嬪棝宕悽鍛娾拻濞达綀娅ｇ敮娑㈡煕閵娿儱鎮戦柟渚垮姂婵¤埖寰勬繝鍕叄闂備礁缍婂Λ鍧楁倿閿曞倹鍋傛繛鎴欏灪閻撴洟鎮橀悙鎵暯妞ゅ繐鐗嗛悞鍨亜閹哄秷鍏屽褔浜堕弻宥堫檨闁告挻宀搁幊婵嬪礈瑜忕粈濠傗攽閻樺弶鎼愰柦鍐枛閺屾洘绻涢悙顒佺彆闂佺ǹ顑呯€氫即寮婚敐澶婄闁绘垵妫涢崝鐑芥⒑閸濆嫭锛旂紒鐘虫崌瀵鍩勯崘銊х獮婵犵數濮寸€氼剟鍩呴柆宥嗏拺缂佸顑欓崕鎰版煙閹间胶鐣洪柛鈹惧亾濡炪倖甯婂ù鍥ㄧ珶濡偐纾界€广儱鎷戦煬顒傗偓娈垮枛椤攱淇婇幖浣哥厸闁稿本鐭花濠氭⒒娴ｅ憡鍟炵紒瀣灴閺佸啴鏁傞挊澶樺殼闂佸憡娲﹂崹閬嶅煕閹达附鐓熼柣鏂挎啞缁跺弶绻涢崼姘珚闁哄瞼鍠栧畷娆撳Χ閸℃浼�
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




