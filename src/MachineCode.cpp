#include "MachineCode.h"
extern FILE* yyout;
#include<Type.h>
#include "Operand.h"
#include<iostream>

MachineOperand::MachineOperand(int tp, int val)
{
    this->type = tp;
    if(tp == MachineOperand::IMM)
        this->val = val;
    else 
        this->reg_no = val;
}

MachineOperand::MachineOperand(std::string label)
{
    this->type = MachineOperand::LABEL;
    this->label = label;
}

bool MachineOperand::operator==(const MachineOperand&a) const
{
    if (this->type != a.type)
        return false;
    if (this->type == IMM)
        return this->val == a.val;
    return this->reg_no == a.reg_no;
}

bool MachineOperand::operator<(const MachineOperand&a) const
{
    if(this->type == a.type)
    {
        if(this->type == IMM)
            return this->val < a.val;
        return this->reg_no < a.reg_no;
    }
    return this->type < a.type;

    if (this->type != a.type)
        return false;
    if (this->type == IMM)
        return this->val == a.val;
    return this->reg_no == a.reg_no;
}

void MachineOperand::PrintReg()
{
    switch (reg_no)
    {
    case 11:
        fprintf(yyout, "fp");
        break;
    case 13:
        fprintf(yyout, "sp");
        break;
    case 14:
        fprintf(yyout, "lr");
        break;
    case 15:
        fprintf(yyout, "pc");
        break;
    default:
        fprintf(yyout, "r%d", reg_no);
        break;
    }
}

void MachineOperand::output() 
{
    /* HINT：print operand
    * Example:
    * immediate num 1 -> print #1;
    * register 1 -> print r1;
    * lable addr_a -> print addr_a; */
    switch (this->type)
    {
    case IMM:
        fprintf(yyout, "#%d", this->val);
        break;
    case VREG:
        fprintf(yyout, "v%d", this->reg_no);
        break;
    case REG:
        PrintReg();
        break;
    case LABEL:
        if (this->label.substr(0, 2) == ".L")
            fprintf(yyout, "%s", this->label.c_str());
        else
            fprintf(yyout, "addr_%s", this->label.c_str());
    default:
        break;
    }
}

void MachineInstruction::PrintCond()
{
    // TODO
    switch (cond)
    {
    case LT:
        fprintf(yyout, "lt");
        break;
    case GT:
        fprintf(yyout, "gt");
        break;
    case EQ:
        fprintf(yyout, "eq");
        break;
    case NE:
        fprintf(yyout, "ne");
        break;
    case LE:
        fprintf(yyout, "le");
        break;
    case GE:
        fprintf(yyout, "ge");
        break;
    default:
        break;
    }
}

BinaryMInstruction::BinaryMInstruction(
    MachineBlock* p, int op, 
    MachineOperand* dst, MachineOperand* src1, MachineOperand* src2, 
    int cond)
{
    this->parent = p;
    this->type = MachineInstruction::BINARY;
    this->op = op;
    this->cond = cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src1);
    this->use_list.push_back(src2);
    dst->setParent(this);
    src1->setParent(this);
    src2->setParent(this);
}

void BinaryMInstruction::output() 
{
    // TODO: 
    // Complete other instructions
    /*
    大草了这里可能要考虑返回64位结果的情况，先看看需不需要吧后面如果需要可能还要对寄存器高很多操作。
    这里咱们没有无符号整数类型，就默认只写有符号数了。
    */
    if (this->op < arithmetic_end)
    {
        switch (this->op)
        {
        case BinaryMInstruction::ADD:
            fprintf(yyout, "\tadd ");
            break;
        case BinaryMInstruction::SUB:
            fprintf(yyout, "\tsub ");
            break;
        case BinaryMInstruction::MUL:
            fprintf(yyout, "\tmul ");
            break;
        case BinaryMInstruction::DIV:
            fprintf(yyout, "\tsdiv ");
            break;
        case BinaryMInstruction::MOD:
            fprintf(yyout, "\tadd ");
            break;
        default:
            break;
        }
        this->PrintCond();
        this->def_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[1]->output();
        fprintf(yyout, "\n");
    }
    else if (this->op < logical_end)
    {
        switch (this->op)
        {
        case BinaryMInstruction::AND:
            fprintf(yyout, "\tand ");
            break;
        case BinaryMInstruction::OR:
            fprintf(yyout, "\torr ");
            break;
        default:
            break;
        }
        this->PrintCond();
        this->def_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[1]->output();
        fprintf(yyout, "\n");
    }
}

LoadMInstruction::LoadMInstruction(MachineBlock* p,
    MachineOperand* dst, MachineOperand* src1, MachineOperand* src2,
    int cond)
{
    this->parent = p;
    this->type = MachineInstruction::LOAD;
    this->op = -1;
    this->cond = cond;
    this->def_list.push_back(dst);
    this->use_list.push_back(src1);
    if (src2)
        this->use_list.push_back(src2);
    dst->setParent(this);
    src1->setParent(this);
    if (src2)
        src2->setParent(this);
}

void LoadMInstruction::output()
{
    fprintf(yyout, "\tldr ");
    this->def_list[0]->output();
    fprintf(yyout, ", ");

    // Load immediate num, eg: ldr r1, =8
    if(this->use_list[0]->isImm())
    {
        fprintf(yyout, "=%d\n", this->use_list[0]->getVal());
        return;
    }

    // Load address
    if(this->use_list[0]->isReg()||this->use_list[0]->isVReg())
        fprintf(yyout, "[");

    this->use_list[0]->output();
    if( this->use_list.size() > 1 )
    {
        fprintf(yyout, ", ");
        this->use_list[1]->output();
    }

    if(this->use_list[0]->isReg()||this->use_list[0]->isVReg())
        fprintf(yyout, "]");
    fprintf(yyout, "\n");
}

StoreMInstruction::StoreMInstruction(MachineBlock* p,
    MachineOperand* src1, MachineOperand* src2, MachineOperand* src3, 
    int cond)
{
    // TODO
    this->parent = p;
    this->type = MachineInstruction::STORE;
    this->op = -1;
    this->cond = cond;
    this->def_list.push_back(src1);
    this->use_list.push_back(src2);
    if (src3)
        this->use_list.push_back(src3);
    src1->setParent(this);
    src2->setParent(this);
    if (src3)
        src3->setParent(this);
}

void StoreMInstruction::output()
{
    // TODO

    fprintf(yyout, "\tstr ");
    this->def_list[0]->output();
    fprintf(yyout, ", ");

    // Load immediate num, eg: str r1, =8
    if (this->use_list[0]->isImm())
    {
        //我们不可能到达这里。
        
        fprintf(yyout, "what happen bro? :(");
        //return;
    }

    fprintf(yyout, "[");

    this->use_list[0]->output();
    if (this->use_list.size() > 1)
    {
        fprintf(yyout, ", ");
        this->use_list[1]->output();
    }

    fprintf(yyout, "]");
    fprintf(yyout, "\n");

    

}

MovMInstruction::MovMInstruction(MachineBlock* p, int op, 
    MachineOperand* dst, MachineOperand* src,
    int cond)
{
    // TODO
    if (op == MovMInstruction::MOV)
    {
        this->parent = p;
        this->op=op;
        this->type = MovMInstruction::MOV;
        this->def_list.push_back(dst);
        this->use_list.push_back(src);
        dst->setParent(this);
        src->setParent(this);
    }
}

void MovMInstruction::output() 
{
    // TODO
    fprintf(yyout, "\tmov ");
    PrintCond();
    fprintf(yyout, " ");
    def_list[0]->output();
    fprintf(yyout, ", ");
    use_list[0]->output();
    use_list[0]->setReg(def_list[0]->getReg());
    fprintf(yyout, "\n");

}

BranchMInstruction::BranchMInstruction(MachineBlock* p, int op, 
    MachineOperand* dst, 
    int cond)
{
    // TODO
    
}

void BranchMInstruction::output()
{
    // TODO
}

CmpMInstruction::CmpMInstruction(MachineBlock* p, 
    MachineOperand* src1, MachineOperand* src2, 
    int cond)
{
    // TODO
     this->parent = p;
    this->type = MachineInstruction::CMP;
    this->op = -1;
    this->cond = cond;
    this->use_list.push_back(src1);
    this->use_list.push_back(src2);
    src1->setParent(this);
    src2->setParent(this);
}

void CmpMInstruction::output()
{
    // TODO
    // Jsut for reg alloca test
    // delete it after test
    fprintf(yyout, "\tcmp ");
    this->use_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[1]->output();
    fprintf(yyout, "\n");
}

StackMInstrcuton::StackMInstrcuton(MachineBlock* p, int op, 
    MachineOperand* src,
    int cond)
{
    // TODO
}

void StackMInstrcuton::output()
{
    // TODO
}


MachineFunctCall::MachineFunctCall(MachineBlock* p, MachineOperand* dst, 
    int cond)
{
    this->parent = p;
    dst->setParent(this);
    this->def_list.push_back(dst);
}
void MachineFunctCall::output()
{

    //然后润到函数
    const char* func_name = def_list[0]->getLabel().c_str() + 1;
    fprintf(yyout, "\tbl %s\n", func_name);
}



MachineFunction::MachineFunction(MachineUnit* p, SymbolEntry* sym_ptr) 
{ 
    this->parent = p; 
    this->sym_ptr = sym_ptr; 
    this->stack_size = 0;
}
MachineFunction::MachineFunction(MachineUnit* p, SymbolEntry* sym_ptr, std::vector<Operand*> params)
{
    this->parent = p;
    this->sym_ptr = sym_ptr;
    this->stack_size = 0;
    this->params = params;

    /*for (unsigned i = 0; i < (unsigned)params.size(); i++)
    {
        int offset = AllocSpace(4);

        dynamic_cast<TemporarySymbolEntry*>(params[i]->getEntry())->setOffset(-offset);
    }*/
}

void MachineBlock::output()
{
    fprintf(yyout, ".L%d:\n", this->no);
    for(auto iter : inst_list)
        iter->output();
}

void MachineFunction::output()
{
    if (this->sym_ptr == nullptr)
        return;
    const char *func_name = this->sym_ptr->toStr().c_str() + 1;
    fprintf(yyout, "\t.global %s\n", func_name);
    fprintf(yyout, "\t.type %s , %%function\n", func_name);
    fprintf(yyout, "%s:\n", func_name);
    // TODO
    /* Hint:
    *  1. Save fp
    *  2. fp = sp
    *  3. Save callee saved register
    *  4. Allocate stack space for local variable */

    fprintf(yyout, "\tstr fp, [sp, #%d]!\n", 
        (int)params.size() > 4 ?  ((int)params.size() - 4)* 4 + 4 : 4
    );

    fprintf(yyout, "\tmov fp, sp\n");

   /* if ((unsigned)params.size())
    {
        
        fprintf(yyout, "\tpush {");
        for (unsigned i = 0; i < (unsigned)params.size() - 4; i++)
        {
            fprintf(yyout, "r%d,", i + 4);
        }
        fprintf(yyout, "lr}\n");
    }*/

    // Allocate stack space for local variables (adjust as needed)
    fprintf(yyout, "\tsub sp, sp, #114514\n");
    
    // Traverse all the block in block_list to print assembly code.
    for(auto iter : block_list)
        iter->output();
}



PushMInstrcuton::PushMInstrcuton(MachineBlock * p, std::vector<MachineOperand*> params)
{
    this->parent = p;
    this->def_list = params;
}
PushMInstrcuton::PushMInstrcuton(MachineBlock* p, MachineOperand* dst)
{
    this->parent = p;
    this->def_list.push_back(dst);
}
PushMInstrcuton::PushMInstrcuton(MachineBlock* p)
{
    this->parent = p;
    lr = 1;
}
void PushMInstrcuton::output() 
{
    if (lr)
    {
        fprintf(yyout, "\tpush {lr} \n");
    }
    fprintf(yyout, "\tpush {");
    for (unsigned i = 0; i < (unsigned)def_list.size(); i++)
    {
        def_list[i]->output();
    }
    fprintf(yyout, "}\n");
}



void MachineUnit::PrintGlobalDecl()
{
    // TODO:
    // You need to print global variable/const declarition code;
    for (auto global_i : global_list)
    {
        const char* i_name = global_i->op->toStr().c_str() + 1;
        fprintf(yyout, ".global %s\n%s:\n", i_name, i_name);
        //如果没有初始值
        if (global_i->init_value == nullptr)
        {
            int kind_size = global_i->op->getType()->getKindValue();
            if(kind_size != TYPE_ERROR)
                fprintf(yyout, "\t.space %d\n", kind_size);
            else
                fprintf(yyout, "\tsomething wrong man!" );
        }
        else
        {
            //std::cout << global_i.init_value->get_se()->toStr() << std::endl;;
            fprintf(yyout, "\t.word %s\n", global_i->init_value->toStr().c_str());
        }
    }
}

void MachineUnit::output()
{
    // TODO
    /* Hint:
    * 1. You need to print global variable/const declarition code;
    * 2. Traverse all the function in func_list to print assembly code;
    * 3. Don't forget print bridge label at the end of assembly code!! */
    fprintf(yyout, "\t.arch armv8-a\n");
    fprintf(yyout, "\t.arch_extension crc\n");
    fprintf(yyout, "\t.arm\n");
    PrintGlobalDecl();
    for(auto iter : func_list)
        iter->output();
}
