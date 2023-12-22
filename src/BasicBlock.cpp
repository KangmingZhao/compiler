#include "BasicBlock.h"
#include "Function.h"
#include <algorithm>
#include "Type.h"

extern FILE* yyout;

void LinkBB(BasicBlock* pre, BasicBlock* suc)
{
    pre->addSucc(suc);
    suc->addPred(pre);
}
// insert the instruction to the front of the basicblock.
void BasicBlock::insertFront(Instruction *inst)
{
    insertBefore(inst, head->getNext());
}

// insert the instruction to the back of the basicblock.
void BasicBlock::insertBack(Instruction *inst) 
{
    insertBefore(inst, head);
}

// insert the instruction dst before src.
void BasicBlock::insertBefore(Instruction *dst, Instruction *src)
{
    // Todo
    src->getPrev()->setNext(dst); 
    dst->setPrev(src->getPrev()); 
    src->setPrev(dst); 
    dst->setNext(src);

    dst->setParent(this);
}

// remove the instruction from intruction list.
void BasicBlock::remove(Instruction *inst)
{
    inst->getPrev()->setNext(inst->getNext());
    inst->getNext()->setPrev(inst->getPrev());
}

void BasicBlock::output() const
{
    fprintf(yyout, "B%d:", no);

    if (!pred.empty())
    {
        fprintf(yyout, "%*c; preds = %%B%d", 32, '\t', pred[0]->getNo());
        for (auto i = pred.begin() + 1; i != pred.end(); i++)
            fprintf(yyout, ", %%B%d", (*i)->getNo());
    }
    fprintf(yyout, "\n");
    if (head->getNext() != head)//otherwise, there is no any shit in the fucking block, printing whom will lead to synax error
    {
        for (auto i = head->getNext(); i != head; i = i->getNext())
            i->output();
    }
    else
    {
        //世界上真的存在没有指令但是有很多后继的结点吗?
        if (succ.size())
        {
            for (auto succ_bb : succ)
            {
                fprintf(yyout, "  br label %%B%d\n", succ_bb->no);
            }
        }
        else
        {
            if (parent->getSymPtr()->getType()->isVoid())
            {
                fprintf(yyout, "  ret void\n");
            }
            else
            {
                fprintf(yyout, "  ret %s %d\n", parent->getSymPtr()->getType()->toStr_for_funct().c_str(), 0);
            }
        }
    }


    
    
}

void BasicBlock::addSucc(BasicBlock *bb)
{
    succ.push_back(bb);
}

// remove the successor basicclock bb.
void BasicBlock::removeSucc(BasicBlock *bb)
{
    succ.erase(std::find(succ.begin(), succ.end(), bb));
}

void BasicBlock::addPred(BasicBlock *bb)
{
    pred.push_back(bb);
}

// remove the predecessor basicblock bb.
void BasicBlock::removePred(BasicBlock *bb)
{
    pred.erase(std::find(pred.begin(), pred.end(), bb));
}

void BasicBlock::genMachineCode(AsmBuilder* builder) 
{
    auto cur_func = builder->getFunction();
    auto cur_block = new MachineBlock(cur_func, no);
    builder->setBlock(cur_block);
    for (auto i = head->getNext(); i != head; i = i->getNext())
    {
        i->genMachineCode(builder);
    }
    cur_func->InsertBlock(cur_block);
}

BasicBlock::BasicBlock(Function *f)
{
    this->no = SymbolTable::getLabel();
    f->insertBlock(this);
    parent = f;
    head = new DummyInstruction();
    head->setParent(this);
}

BasicBlock::~BasicBlock()
{
    Instruction *inst;
    inst = head->getNext();
    while (inst != head)
    {
        Instruction *t;
        t = inst;
        inst = inst->getNext();
        delete t;
    }
    for(auto &bb:pred)
        bb->removeSucc(this);
    for(auto &bb:succ)
        bb->removePred(this);
    parent->remove(this);
}
