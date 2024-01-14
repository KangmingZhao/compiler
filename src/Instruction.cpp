#include "Instruction.h"
#include "BasicBlock.h"
#include <iostream>
#include "Function.h"
#include "Type.h"
#include <string>
extern FILE *yyout;

Instruction::Instruction(unsigned instType, BasicBlock *insert_bb)
{
    prev = next = this;
    opcode = -1;
    this->instType = instType;

    if (insert_bb != nullptr)
    {
        insert_bb->insertBack(this);
        parent = insert_bb;
    }
}

Instruction::~Instruction()
{
    parent->remove(this);
}

BasicBlock *Instruction::getParent()
{
    return parent;
}

void Instruction::setParent(BasicBlock *bb)
{
    parent = bb;
}

void Instruction::setNext(Instruction *inst)
{
    next = inst;
}

void Instruction::setPrev(Instruction *inst)
{
    prev = inst;
}

Instruction *Instruction::getNext()
{
    return next;
}

Instruction *Instruction::getPrev()
{
    return prev;
}

BinaryInstruction::BinaryInstruction(unsigned opcode, Operand *dst, Operand *src1, Operand *src2, BasicBlock *insert_bb) : Instruction(BINARY, insert_bb)
{
    this->opcode = opcode;
    operands.push_back(dst);
    operands.push_back(src1);
    operands.push_back(src2);
    dst->setDef(this);
    src1->addUse(this);
    src2->addUse(this);
}

BinaryInstruction::~BinaryInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
    operands[2]->removeUse(this);
}

void BinaryInstruction::output() const
{
    std::string s1, s2, s3, op, type;
    s1 = operands[0]->toStr();
    s2 = operands[1]->toStr();
    s3 = operands[2]->toStr();
    type = operands[0]->getType()->toStr();
    switch (opcode)
    {
    case ADD:
        op = "add";
        break;
    case SUB:
        op = "sub";
        break;
    case MUL:
        op = "mul";
        break;
    case DIV:
        op = "sdiv";
        break;
    case MOD:
        op = "srem";
        break;
    default:
        break;
    }
    fprintf(yyout, "  %s = %s %s %s, %s\n", s1.c_str(), op.c_str(), type.c_str(), s2.c_str(), s3.c_str());
}

CmpInstruction::CmpInstruction(unsigned opcode, Operand *dst, Operand *src1, Operand *src2, BasicBlock *insert_bb) : Instruction(CMP, insert_bb)
{
    this->opcode = opcode;
    operands.push_back(dst);
    operands.push_back(src1);
    operands.push_back(src2);
    dst->setDef(this);
    src1->addUse(this);
    src2->addUse(this);
    dst->get_se()->setType(TypeSystem::boolType);
}

CmpInstruction::~CmpInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
    operands[2]->removeUse(this);
}

void CmpInstruction::output() const
{

    std::string s1, s2, s3, op, type;
    s1 = operands[0]->toStr();
    s2 = operands[1]->toStr();
    s3 = operands[2]->toStr();
    type = operands[1]->getType()->toStr();

    // SymbolEntry* se = new TemporarySymbolEntry(operands[1]->getType(), SymbolTable::getLabel());
    // Operand* n = new Operand(se);
    // SymbolEntry* se2 = new TemporarySymbolEntry(operands[1]->getType(), SymbolTable::getLabel());
    // Operand* n2 = new Operand(se2);
    // if (operands[1]->getType()->toStr() != type)
    //{
    //     //std::cout<<"fuck"<<std::endl;
    //    // std::cout<<dst->toStr()<<std::endl;
    //     //std::cout<<src->toStr()<<std::endl;
    //     //new ZextInstruction(n, operands[1], builder->getInsertBB());
    //     s2 = n->toStr();
    //     fprintf(yyout, "  %s = zext i1 %s to %s\n", s2.c_str(),operands[1]->toStr().c_str(),type.c_str());
    // }
    // if (operands[2]->getType()->toStr() != type)
    //{
    //     //std::cout<<"fuck"<<std::endl;
    //    // std::cout<<dst->toStr()<<std::endl;
    //     //std::cout<<src->toStr()<<std::endl;
    //     //new ZextInstruction(n2, operands[2], builder->getInsertBB());
    //     s3 = n2->toStr();
    //     fprintf(yyout, "  %s = zext i1 %s to %s\n", s3.c_str(), operands[2]->toStr().c_str(), type.c_str());
    // }

    switch (opcode)
    {
    case E:
        op = "eq";
        break;
    case NE:
        op = "ne";
        break;
    case L:
        op = "slt";
        break;
    case LE:
        op = "sle";
        break;
    case G:
        op = "sgt";
        break;
    case GE:
        op = "sge";
        break;
    default:
        op = "";
        break;
    }

    fprintf(yyout, "  %s = icmp %s %s %s, %s\n", s1.c_str(), op.c_str(), type.c_str(), s2.c_str(), s3.c_str());
}

UncondBrInstruction::UncondBrInstruction(BasicBlock *to, BasicBlock *insert_bb) : Instruction(UNCOND, insert_bb)
{
    branch = to;
    // std::cout << "fuck\n";
    // this->output();
    // insert_bb->output();
}

void UncondBrInstruction::output() const
{
    // std::cout << branch->getNo() <<std::endl;
    fprintf(yyout, "  br label %%B%d\n", branch->getNo());
}

void UncondBrInstruction::setBranch(BasicBlock *bb)
{
    branch = bb;
}

BasicBlock *UncondBrInstruction::getBranch()
{
    return branch;
}

CondBrInstruction::CondBrInstruction(BasicBlock *true_branch, BasicBlock *false_branch, Operand *cond, BasicBlock *insert_bb) : Instruction(COND, insert_bb)
{
    this->true_branch = true_branch;
    this->false_branch = false_branch;
    cond->addUse(this);
    operands.push_back(cond);
}

CondBrInstruction::~CondBrInstruction()
{
    operands[0]->removeUse(this);
}

void CondBrInstruction::output() const
{
    std::string cond, type;
    cond = operands[0]->toStr();
    type = operands[0]->getType()->toStr();
    int true_label = true_branch->getNo();
    int false_label = false_branch->getNo();
    fprintf(yyout, "  br %s %s, label %%B%d, label %%B%d\n", type.c_str(), cond.c_str(), true_label, false_label);
}

void CondBrInstruction::setFalseBranch(BasicBlock *bb)
{
    false_branch = bb;
}

BasicBlock *CondBrInstruction::getFalseBranch()
{
    return false_branch;
}

void CondBrInstruction::setTrueBranch(BasicBlock *bb)
{
    true_branch = bb;
}

BasicBlock *CondBrInstruction::getTrueBranch()
{
    return true_branch;
}

RetInstruction::RetInstruction(Operand *src, BasicBlock *insert_bb) : Instruction(RET, insert_bb)
{

    if (src != nullptr)
    {
        operands.push_back(src);
        src->addUse(this);
    }
}

RetInstruction::~RetInstruction()
{
    if (!operands.empty())
        operands[0]->removeUse(this);
}

void RetInstruction::output() const
{





    if (operands.empty())
    {
        fprintf(yyout, "  ret void\n");
    }
    else
    {
        std::string ret, type;
        ret = operands[0]->toStr();
        type = operands[0]->getType()->toStr();
        fprintf(yyout, "  ret %s %s\n", type.c_str(), ret.c_str());
    }
}

AllocaInstruction::AllocaInstruction(Operand *dst, SymbolEntry *se, BasicBlock *insert_bb) : Instruction(ALLOCA, insert_bb)
{
    operands.push_back(dst);
    dst->setDef(this);
    this->se = se;
}

AllocaInstruction::~AllocaInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
}

void AllocaInstruction::output() const
{
    std::string dst, type;
    dst = operands[0]->toStr();
    type = se->getType()->toStr();
    fprintf(yyout, "  %s = alloca %s, align 4\n", dst.c_str(), type.c_str());
}

AllocaArrInstruction::AllocaArrInstruction(Operand *dst, SymbolEntry *se, BasicBlock *insert_bb) : Instruction(ALLOCA, insert_bb)
{
    operands.push_back(dst);
    dst->setDef(this);
    this->se = se;
}

AllocaArrInstruction::~AllocaArrInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
}

void AllocaArrInstruction::output() const
{
    std::string dst, type;
    dst = operands[0]->toStr();
    type = se->getType()->toStr();
    fprintf(yyout, "  %s = alloca %s, align 4\n", dst.c_str(), type.c_str());
}

LoadInstruction::LoadInstruction(Operand *dst, Operand *src_addr, BasicBlock *insert_bb) : Instruction(LOAD, insert_bb)
{
    operands.push_back(dst);
    operands.push_back(src_addr);
    dst->setDef(this);
    src_addr->addUse(this);
}

LoadInstruction::~LoadInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
}

void LoadInstruction::output() const
{
    std::string dst = operands[0]->toStr();
    std::string src = operands[1]->toStr();
    std::string src_type;
    std::string dst_type;
    dst_type = operands[0]->getType()->toStr();
    src_type = operands[1]->getType()->toStr();
    fprintf(yyout, "  %s = load %s, %s %s, align 4\n", dst.c_str(), dst_type.c_str(), src_type.c_str(), src.c_str());
}

StoreInstruction::StoreInstruction(Operand *dst_addr, Operand *src, BasicBlock *insert_bb) : Instruction(STORE, insert_bb)
{
    operands.push_back(dst_addr);
    operands.push_back(src);
    dst_addr->addUse(this);
    src->addUse(this);
}

StoreInstruction::StoreInstruction(Operand *dst_addr, Operand *src, BasicBlock *insert_bb, bool whether_para) : Instruction(STORE, insert_bb)
{

    this->whether_para = whether_para;
    operands.push_back(dst_addr);
    operands.push_back(src);
    dst_addr->addUse(this);
    src->addUse(this);
}

StoreInstruction::~StoreInstruction()
{
    operands[0]->removeUse(this);
    operands[1]->removeUse(this);
}

void StoreInstruction::output() const
{
    std::string dst = operands[0]->toStr();
    std::string src = operands[1]->toStr();
    std::string dst_type = operands[0]->getType()->toStr();
    std::string src_type = operands[1]->getType()->toStr();

    fprintf(yyout, "  store %s %s, %s %s, align 4\n", src_type.c_str(), src.c_str(), dst_type.c_str(), dst.c_str());
}
MachineOperand *Instruction::genMachineOperand(Operand *ope)
{
    auto se = ope->getEntry();
    MachineOperand *mope = nullptr;
    if (se->isConstant())
        mope = new MachineOperand(MachineOperand::IMM, dynamic_cast<ConstantSymbolEntry *>(se)->getValue());
    else if (se->isTemporary())
        mope = new MachineOperand(MachineOperand::VREG, dynamic_cast<TemporarySymbolEntry *>(se)->getLabel());
    else if (se->isVariable())
    {
        auto id_se = dynamic_cast<IdentifierSymbolEntry *>(se);
        if (id_se->isGlobal())
            mope = new MachineOperand(id_se->toStr().c_str());
        else
            exit(0);
    }
    else if (se->isConstIdentifer())
    {
        auto id_se = dynamic_cast<IdentifierSymbolEntry*>(se);
        if (id_se->isGlobal())
            mope = new MachineOperand(id_se->toStr().c_str());
        else
            exit(0);
    }
    return mope;
}

MachineOperand *Instruction::genMachineReg(int reg)
{
    return new MachineOperand(MachineOperand::REG, reg);
}

MachineOperand *Instruction::genMachineVReg()
{
    return new MachineOperand(MachineOperand::VREG, SymbolTable::getLabel());
}

MachineOperand *Instruction::genMachineImm(int val)
{
    return new MachineOperand(MachineOperand::IMM, val);
}

MachineOperand *Instruction::genMachineLabel(int block_no)
{
    std::ostringstream buf;
    buf << ".L" << block_no;
    std::string label = buf.str();
    return new MachineOperand(label);
}

void AllocaInstruction::genMachineCode(AsmBuilder *builder)
{
    /* HINT:
     * Allocate stack space for local variabel
     * Store frame offset in symbol entry */
    if (funct)
    {
        if (need_register == -1)
        {
            // alloc the stack like
            auto cur_func = builder->getFunction();
            int offset = cur_func->AllocSpace(4);
            dynamic_cast<TemporarySymbolEntry *>(operands[0]->getEntry())->setOffset(offset);
            // std::cout << offset << std::endl;
            // std::cout << dynamic_cast<TemporarySymbolEntry*>(operands[0]->getEntry())->toStr() << std::endl;
        }
        else
        {
            // do nothing here, r0 - r3 have been alloc in the ParaNode gencode.
            // std::cout << dynamic_cast<TemporarySymbolEntry*>(operands[0]->getEntry())->getLabel() << std::endl;
        }
    }
    else
    {
        auto cur_func = builder->getFunction();
        int offset = cur_func->AllocSpace(4);
        dynamic_cast<TemporarySymbolEntry *>(operands[0]->getEntry())->setOffset(-offset);
    }
}

void LoadInstruction::genMachineCode(AsmBuilder *builder)
{
    if (operands[0]->get_se()->get_use_r0_r3() != -1 && operands[1]->get_se()->get_use_r0_r3() == -1)
    {
        MachineInstruction *cur_inst = nullptr;
        auto cur_block = builder->getBlock();
        MachineOperand *dst = genMachineReg(operands[0]->get_se()->get_use_r0_r3());
        MachineOperand *src = genMachineOperand(operands[1]);

        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src);
        cur_block->InsertInst(cur_inst);
        return;
    }
    else if (operands[1]->get_se()->get_use_r0_r3() != -1 && operands[0]->get_se()->get_use_r0_r3() == -1)
    {
        MachineInstruction *cur_inst = nullptr;
        auto cur_block = builder->getBlock();
        MachineOperand *dst = genMachineOperand(operands[0]);
        MachineOperand *src = genMachineReg(operands[1]->get_se()->get_use_r0_r3());

        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src);
        cur_block->InsertInst(cur_inst);
        return;
    }
    else if (operands[1]->get_se()->get_use_r0_r3() != -1 && operands[0]->get_se()->get_use_r0_r3() != -1)
    {
        MachineInstruction *cur_inst = nullptr;
        auto cur_block = builder->getBlock();
        MachineOperand *dst = genMachineReg(operands[0]->get_se()->get_use_r0_r3());
        MachineOperand *src = genMachineReg(operands[1]->get_se()->get_use_r0_r3());

        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src);
        cur_block->InsertInst(cur_inst);
        return;
    }
    /*if (operands[0]->get_se()->get_use_r0_r3() != -1)
    {
        MachineInstruction* cur_inst = nullptr;
        auto cur_block = builder->getBlock();
        MachineOperand* dst = genMachineReg(operands[0]->get_se()->get_use_r0_r3());
        MachineOperand* src = genMachineOperand(operands[1]);

        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, src, dst);
        cur_block->InsertInst(cur_inst);
        return;
    }*/

    // std::cout << operands[0]->get_se()->get_use_r0_r3() << std::endl;
    auto cur_block = builder->getBlock();
    MachineInstruction *cur_inst = nullptr;
    // Load global operand
    if (
        (operands[1]->getEntry()->isVariable() || operands[1]->getEntry()->isConstIdentifer())
        && dynamic_cast<IdentifierSymbolEntry *>(operands[1]->getEntry())->isGlobal())
    {
        auto dst = genMachineOperand(operands[0]);
        auto internal_reg1 = genMachineVReg();
        auto internal_reg2 = new MachineOperand(*internal_reg1);
        auto src = genMachineOperand(operands[1]);
        // example: load r0, addr_a
        cur_inst = new LoadMInstruction(cur_block, internal_reg1, src);

        cur_block->InsertInst(cur_inst);
        // example: load r1, [r0]
        cur_inst = new LoadMInstruction(cur_block, dst, internal_reg2);
        cur_block->InsertInst(cur_inst);
    }
    // Load local operand
    else if (operands[1]->getEntry()->isTemporary() && operands[1]->getDef() && operands[1]->getDef()->isAlloc())
    {
        // example: load r1, [r0, #4]
        auto dst = genMachineOperand(operands[0]);
        auto src1 = genMachineReg(11);
        auto src2 = genMachineImm(dynamic_cast<TemporarySymbolEntry *>(operands[1]->getEntry())->getOffset());
        cur_inst = new LoadMInstruction(cur_block, dst, src1, src2);
        cur_block->InsertInst(cur_inst);
    }
    // Load operand from temporary variable
    else
    {
        //问题出这了。
        // example: load r1, [r0]
        auto dst = genMachineOperand(operands[0]);
        auto src = genMachineOperand(operands[1]);
        cur_inst = new LoadMInstruction(cur_block, dst, src);


        cur_block->InsertInst(cur_inst);
    }
}

void StoreInstruction::genMachineCode(AsmBuilder *builder)
{
    // TODO
    if (whether_para)
        return;
    if (operands[0]->get_se()->get_use_r0_r3() != -1 && operands[1]->get_se()->get_use_r0_r3() == -1)
    {
        MachineInstruction *cur_inst = nullptr;
        auto cur_block = builder->getBlock();
        MachineOperand *dst = genMachineReg(operands[0]->get_se()->get_use_r0_r3());
        MachineOperand *src = genMachineOperand(operands[1]);

        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src);
        cur_block->InsertInst(cur_inst);
        return;
    }
    else if (operands[1]->get_se()->get_use_r0_r3() != -1 && operands[0]->get_se()->get_use_r0_r3() == -1)
    {
        MachineInstruction *cur_inst = nullptr;
        auto cur_block = builder->getBlock();
        MachineOperand *dst = genMachineOperand(operands[0]);
        MachineOperand *src = genMachineReg(operands[1]->get_se()->get_use_r0_r3());

        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src);
        cur_block->InsertInst(cur_inst);
        return;
    }
    else if (operands[1]->get_se()->get_use_r0_r3() != -1 && operands[0]->get_se()->get_use_r0_r3() != -1)
    {
        MachineInstruction *cur_inst = nullptr;
        auto cur_block = builder->getBlock();
        MachineOperand *dst = genMachineReg(operands[0]->get_se()->get_use_r0_r3());
        MachineOperand *src = genMachineReg(operands[1]->get_se()->get_use_r0_r3());

        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src);
        cur_block->InsertInst(cur_inst);
        return;
    }

    auto cur_block = builder->getBlock();
    MachineInstruction *cur_inst = nullptr;
    MachineOperand *dst = genMachineOperand(operands[0]);
    MachineOperand *src = genMachineOperand(operands[1]);

    // store immediate
    if (operands[1]->getEntry()->isConstant())
    {
        auto dst1 = genMachineVReg();
        cur_inst = new LoadMInstruction(cur_block, dst1, src);
        cur_block->InsertInst(cur_inst);
        src = new MachineOperand(*dst1);
    }

    // store global operand
    if (operands[0]->getEntry()->isVariable() && dynamic_cast<IdentifierSymbolEntry *>(operands[0]->getEntry())->isGlobal())
    {
        MachineOperand *internal_reg1 = genMachineVReg();
        // store r1, [r0]
        cur_inst = new LoadMInstruction(cur_block, internal_reg1, dst);
        cur_block->InsertInst(cur_inst);
        cur_inst = new StoreMInstruction(cur_block, src, internal_reg1);
        cur_block->InsertInst(cur_inst);
    }
    // store local operand
    else if (operands[0]->getEntry()->isTemporary() && operands[0]->getDef() && operands[0]->getDef()->isAlloc())
    {
        //  store r1, [r0, #4]
        auto src1 = genMachineReg(11);
        int offset = dynamic_cast<TemporarySymbolEntry *>(operands[0]->getEntry())->getOffset();
        auto src2 = genMachineImm(offset);

        // 如果参数数量大于3 那么需要到栈里去算偏移量 尚未完善！！

        cur_inst = new StoreMInstruction(cur_block, src, src1, src2);
        cur_block->InsertInst(cur_inst);
    }
}

void BinaryInstruction::genMachineCode(AsmBuilder *builder)
{
    // TODO:
    // complete other instructions
    auto cur_block = builder->getBlock();
    auto dst = genMachineOperand(operands[0]);
    auto src1 = genMachineOperand(operands[1]);
    auto src2 = genMachineOperand(operands[2]);
    /* HINT:
     * The source operands of ADD instruction in ir code both can be immediate num.
     * However, it's not allowed in assembly code.
     * So you need to insert LOAD/MOV instrucrion to load immediate num into register.
     * As to other instructions, such as MUL, CMP, you need to deal with this situation, too.*/
    MachineInstruction *cur_inst = nullptr;
    if (src1->isImm())
    {
        auto internal_reg = genMachineVReg();
        cur_inst = new LoadMInstruction(cur_block, internal_reg, src1);
        cur_block->InsertInst(cur_inst);
        src1 = new MachineOperand(*internal_reg);
    }
    switch (opcode)
    {
    case ADD:
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, dst, src1, src2);
        break;

    case SUB:
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::SUB, dst, src1, src2);
        break;

    case MUL:
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::MUL, dst, src1, src2);
        break;

    case DIV:
        //要亲命了，sdiv不支持立即数作为最后一个操作数。所以这里不能大家用一样的。
        //你这个小坏蛋，让我单独给你点颜色看看
        if (src2->isImm())
        {
            auto tempReg = genMachineVReg();
            cur_block->InsertInst(
                new LoadMInstruction(cur_block, tempReg, src2)
            );
            cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::DIV, dst, src1, tempReg);
        }
        else
            cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::DIV, dst, src1, src2);
        break;

    case MOD:
        //你更是个坏蛋！arm汇编没有取模，只能自己手动div然后sub
        if (src2->isImm())
        {
            auto tempReg = genMachineVReg();
            cur_block->InsertInst(
                new LoadMInstruction(cur_block, tempReg, src2)
            );
            cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::DIV, dst, src1, tempReg);
        }
        else
            cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::DIV, dst, src1, src2);
        cur_block->InsertInst(cur_inst);
        //先求出除法。dst的结果是src1//src2
        

        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::MUL, dst, dst, src2);
        cur_block->InsertInst(cur_inst);
        //然后得到dst是src2 * 刚刚求的除法的结果。

        //最后拿src1减去刚刚的dst就是答案了。这里还手动寄存器优化了消暑
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::SUB, dst, src1, dst);
        break;

    case AND:
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::AND, dst, src1, src2);
        break;

    case OR:
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::OR, dst, src1, src2);
        break;

    default:
        break;
    }
    cur_block->InsertInst(cur_inst);
}

void CmpInstruction::genMachineCode(AsmBuilder *builder)
{
    // TODO
    auto cur_block = builder->getBlock();
    MachineOperand *src1 = genMachineOperand(operands[1]);
    MachineOperand *src2 = genMachineOperand(operands[2]);
    MachineInstruction *cur_inst = nullptr;
    // load 立即数
    if (src1->isImm())
    {
        auto internal_reg = genMachineVReg();
        cur_inst = new LoadMInstruction(cur_block, internal_reg, src1);
        cur_block->InsertInst(cur_inst);
        src1 = new MachineOperand(*internal_reg);
    }
    if (src2->isImm() &&
        ((ConstantSymbolEntry *)(operands[2]->getEntry()))->getValue() > 255)
    {

        auto internal_reg = genMachineVReg();
        cur_inst = new LoadMInstruction(cur_block, internal_reg, src2);
        cur_block->InsertInst(cur_inst);
        src2 = new MachineOperand(*internal_reg);
    }

    cur_inst = new CmpMInstruction(cur_block, src1, src2, opcode);
    cur_block->InsertInst(cur_inst);
    builder->setCmpOpcode(opcode); /* 在builder中记录cmp指令的条件码，用于条件分支指令 */

    switch (opcode) /*  enum {E, NE, L, GE, G, LE}; */
    {
    case E:
    {
        auto dst = genMachineOperand(operands[0]);
        auto trueOperand = genMachineImm(1);
        auto falseOperand = genMachineImm(0);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, trueOperand, E);
        cur_block->InsertInst(cur_inst);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, falseOperand, NE);
        cur_block->InsertInst(cur_inst);
        break;
    }
    case NE:
    {
        auto dst = genMachineOperand(operands[0]);
        auto trueOperand = genMachineImm(1);
        auto falseOperand = genMachineImm(0);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, trueOperand, NE);
        cur_block->InsertInst(cur_inst);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, falseOperand, E);
        cur_block->InsertInst(cur_inst);
        break;
    }
    case L:
    {
        auto dst = genMachineOperand(operands[0]);
        auto trueOperand = genMachineImm(1);
        auto falseOperand = genMachineImm(0);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, trueOperand, L);
        cur_block->InsertInst(cur_inst);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, falseOperand, GE);
        cur_block->InsertInst(cur_inst);
        break;
    }
    case GE:
    {
        auto dst = genMachineOperand(operands[0]);
        auto trueOperand = genMachineImm(1);
        auto falseOperand = genMachineImm(0);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, trueOperand, GE);
        cur_block->InsertInst(cur_inst);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, falseOperand, L);
        cur_block->InsertInst(cur_inst);
        break;
    }
    case G:
    {
        auto dst = genMachineOperand(operands[0]);
        auto trueOperand = genMachineImm(1);
        auto falseOperand = genMachineImm(0);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, trueOperand, G);
        cur_block->InsertInst(cur_inst);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, falseOperand, LE);
        cur_block->InsertInst(cur_inst);
        break;
    }
    case LE:
    {
        auto dst = genMachineOperand(operands[0]);
        auto trueOperand = genMachineImm(1);
        auto falseOperand = genMachineImm(0);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, trueOperand, LE);
        cur_block->InsertInst(cur_inst);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, falseOperand, G);
        cur_block->InsertInst(cur_inst);
        break;
    }
    default:
        break;
    }
}

void UncondBrInstruction::genMachineCode(AsmBuilder *builder)
{
    // TODO
    auto cur_block = builder->getBlock();
    // 设置跳转到的目标分支
    MachineOperand *dst = genMachineLabel(branch->getNo());
    auto cur_inst = new BranchMInstruction(cur_block, BranchMInstruction::B, dst);
    cur_block->InsertInst(cur_inst);
}

void CondBrInstruction::genMachineCode(AsmBuilder *builder)
{
    // TODO
    auto cur_block = builder->getBlock();
    MachineOperand *dst = genMachineLabel(true_branch->getNo());
    auto cur_inst = new BranchMInstruction(cur_block, BranchMInstruction::B, dst, builder->getCmpOpcode());
    cur_block->InsertInst(cur_inst);
    dst = genMachineLabel(false_branch->getNo());
    cur_inst = new BranchMInstruction(cur_block, BranchMInstruction::B, dst);
    cur_block->InsertInst(cur_inst);
}

void RetInstruction::genMachineCode(AsmBuilder *builder)
{
    // TODO
    /* HINT:
     * 1. Generate mov instruction to save return value in r0
     * 2. Restore callee saved registers and sp, fp
     * 3. Generate bx instruction */
    auto cur_block = builder->getBlock();
    


    //mov保存返回值到r0
    if(!operands.empty()){//有返回值
        auto dst = new MachineOperand(MachineOperand::REG,0);
        auto src = genMachineOperand(operands[0]);
        auto cur_inst =new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src);
        cur_block->InsertInst(cur_inst);
    }
    //调整栈帧 保存寄存器sp
    auto sp = new MachineOperand(MachineOperand::REG, 13);
    auto funcSize =new MachineOperand(MachineOperand::IMM, builder->getFunction()->AllocSpace(0));//开辟函数大小空间
    cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::ADD,sp, sp, funcSize));
    //bx指令，跳转lr寄存器
    auto lr = new MachineOperand(MachineOperand::REG, 14);
    cur_block->InsertInst(new BranchMInstruction(cur_block, BranchMInstruction::BX, lr));
}

GlobalInstruction::GlobalInstruction(Operand *dst, Operand *expr, SymbolEntry *se, BasicBlock *insertBB) : Instruction(GLOBAL, insertBB)
{
    // std::cout<<"globalInstruction iniitial"<<std::endl;

    operands.push_back(dst);
    operands.push_back(expr);
    dst->setDef(this);
    this->se = se;
}
GlobalInstruction::~GlobalInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
}

void GlobalInstruction::output() const
{
    // 如果这是个声明
    // std::cout<<"fuck output"<<std::endl;
    if (operands[1] == nullptr)
    {
        // std::cout<<"non initial"<<std::endl;
        //fprintf(yyout, "%s = global %s 0, align 4 \n", operands[0]->toStr().c_str(), operands[0]->getType()->toStr().c_str());
    }
    else
    {
        //fprintf(yyout, "%s = global %s %s, align 4 \n", operands[0]->toStr().c_str(), operands[0]->getType()->toStr().c_str(), operands[1]->toStr().c_str());
    }
}

void GlobalInstruction::genMachineCode(AsmBuilder *builder)
{
    Global_init_bag *temp = new Global_init_bag();
    temp->op = operands[0];
    if (operands[1] != nullptr)
        temp->init_value = operands[1];

    builder->getUnit()->InsertGlobal(temp);
}

CallInstruction::CallInstruction(Operand *dst,
                                 SymbolEntry *func,
                                 std::vector<Operand *> params,
                                 BasicBlock *insert_bb)
    : Instruction(CALL, insert_bb), func(func)
{
    Operand *dst_ = dst;
    dst_->change_funct_type();
    operands.push_back(dst_);
    if (dst_)
        dst_->setDef(this);
    // std::cout<<dst->getType()->toStr();

    for (auto param : params)
    {
        operands.push_back(param);
        param->addUse(this);
    }
}

void CallInstruction::output() const
{
    fprintf(yyout, "  ");
    FunctionType *type = (FunctionType *)(func->getType());
    if (operands[0] && !type->returnType->isVoid())
    {
        // std::cout<<operands[0]<<std::endl;
        fprintf(yyout, "%s =", operands[0]->toStr().c_str());
    }
    fprintf(yyout, "call %s %s(", type->getRetType()->toStr().c_str(),
            func->toStr().c_str());
    for (long unsigned int i = 1; i < operands.size(); i++)
    {
        if (i != 1)
            fprintf(yyout, ", ");
        fprintf(yyout, "%s %s", operands[i]->getType()->toStr().c_str(),
                operands[i]->toStr().c_str());
    }
    fprintf(yyout, ")\n");
}
void CallInstruction::genMachineCode(AsmBuilder *builder)
{
    // operands[0]是dst，后面的一大坨全是参数。
    auto cur_block = builder->getBlock();
    auto dst = genMachineOperand(new Operand(func));

    // 在bl之前，得先把这些够吧参数全部存入。但是现在寄存器还没有实现所以先鸽一会儿。到时候传入的参数
    // 也许应该是点Operand以外的别的东西。
    /*
    这里是push各自参数
    */

    // 好吧听说不能破罐破摔，非得前四个r0-r3
    ////好吧因为我们的函数的para乱写所以现在只能反着来了。
    // for (unsigned p = 1; p < (unsigned)operands.size() ; p++)
    //{
    //     MachineOperand* now = genMachineOperand(operands[p]);
    //     MachineOperand* r0 = new MachineOperand(MachineOperand::REG, 0);
    //     if (now->isImm() || now->isLabel())
    //         cur_block->InsertInst(
    //             (new LoadMInstruction(cur_block, r0, now))
    //         );
    //     else
    //     {
    //         //如果在寄存器里就直接倒腾寄存器。虽然好像可以直接把当前寄存器给传过去来着？
    //         cur_block->InsertInst(
    //             (new MovMInstruction(cur_block, MovMInstruction::MOV, r0, now))
    //         );
    //     }
    //     cur_block->InsertInst(new PushMInstrcuton(cur_block, r0));
    // }

    for (unsigned p = (unsigned)operands.size() - 1; p > 4; p--)
    {
        MachineOperand *now = genMachineOperand(operands[p]);
        MachineOperand *r0 = new MachineOperand(MachineOperand::REG, 0);
        if (now->isImm() || now->isLabel())
            cur_block->InsertInst(
                (new LoadMInstruction(cur_block, r0, now)));
        else
        {
            // 如果在寄存器里就直接倒腾寄存器。虽然好像可以直接把当前寄存器给传过去来着？
            cur_block->InsertInst(
                (new MovMInstruction(cur_block, MovMInstruction::MOV, r0, now)));
        }
        cur_block->InsertInst(new PushMInstrcuton(cur_block, r0));
    }
    // 我们的函数调用默认是从#-4开始取数据，所以如果使用r0-r3的话会不太好搞
    for (unsigned i = 0; i < (unsigned)operands.size() - 1 && i < 4; i++)
    {
        // 高贵的寄存器里只能放0~3。
        // 这里如果是立即数或者是在内存中的变量才load
        MachineOperand *now = genMachineOperand(operands[i + 1]);
        if (now->isImm() || now->isLabel())
            cur_block->InsertInst(
                (new LoadMInstruction(cur_block, new MachineOperand(MachineOperand::REG, i), now)));
        else
        {
            // 如果在寄存器里就直接倒腾寄存器。虽然好像可以直接把当前寄存器给传过去来着？
            cur_block->InsertInst(
                (new MovMInstruction(cur_block, MovMInstruction::MOV, new MachineOperand(MachineOperand::REG, i), now)));
        }
    }

    MachineInstruction *cur_inst = nullptr;
    cur_inst = new MachineFunctCall(cur_block, dst, MachineInstruction::NONE);
    cur_block->InsertInst(cur_inst);
}

UnaryInstruction::UnaryInstruction(unsigned opcode, Operand *dst, Operand *src, BasicBlock *insert_bb) : Instruction(UNARY, insert_bb)
{
    this->opcode = opcode;
    operands.push_back(dst);
    operands.push_back(src);
    dst->setDef(this);
    src->addUse(this);
}

UnaryInstruction::~UnaryInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
}

void UnaryInstruction::output() const
{
    std::string s1, s2, type;
    s1 = operands[0]->toStr();
    s2 = operands[1]->toStr();
    type = operands[0]->getType()->toStr();
    switch (opcode)
    {
    case ADD:
        fprintf(yyout, "  %s = add %s 0, %s\n", s1.c_str(), type.c_str(), s2.c_str());
        break;
    case SUB:
        fprintf(yyout, "  %s = sub %s 0, %s\n", s1.c_str(), type.c_str(), s2.c_str());
        break;
    case NOT:
        fprintf(yyout, "  %s = not %s %s\n", s1.c_str(), type.c_str(), s2.c_str());
        break;
    default:
        break;
    }
}

void UnaryInstruction::genMachineCode(AsmBuilder *)
{
}

NotInstruction::NotInstruction(Operand *dst, Operand *src, BasicBlock *insert_bb) : Instruction(NOT, insert_bb)
{
    operands.push_back(dst);
    operands.push_back(src);
    dst->setDef(this);
    src->addUse(this);

    dst->get_se()->setType(TypeSystem::boolType);
}

NotInstruction::~NotInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
}

void NotInstruction::genMachineCode(AsmBuilder *)
{
}

void NotInstruction::output() const
{
    std::string s1, s2, type;
    s1 = operands[0]->toStr();
    s2 = operands[1]->toStr();
    type = operands[0]->getType()->toStr();

    fprintf(yyout, "  %s = xor %s %s, true\n", s1.c_str(), type.c_str(), s2.c_str());
}

ZextInstruction::ZextInstruction(Operand *dst, Operand *src, BasicBlock *insertBB) : Instruction(TEMP, insertBB)
{
    operands.push_back(dst);
    operands.push_back(src);
}

void ZextInstruction::genMachineCode(AsmBuilder *)
{
}

void ZextInstruction::output() const
{
    fprintf(yyout, "  %s = zext i1 %s to i32\n", operands[0]->toStr().c_str(), operands[1]->toStr().c_str());
}