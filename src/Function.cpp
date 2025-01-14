#include "Function.h"
#include "Unit.h"
#include "Type.h"
#include <list>
#include "Ast.h"
#include "Operand.h"
#include "Instruction.h"

extern FILE* yyout;

Function::Function(Unit *u, SymbolEntry *s)
{
    u->insertFunc(this);
    entry = new BasicBlock(this);
    sym_ptr = s;
    parent = u;
}

Function::~Function()
{
    auto delete_list = block_list;
    for (auto &i : delete_list)
        delete i;
    parent->removeFunc(this);
}

// remove the basicblock bb from its block_list.
void Function::remove(BasicBlock *bb)
{
    block_list.erase(std::find(block_list.begin(), block_list.end(), bb));
}

void Function::output() const
{
    if (sym_ptr == nullptr)
        return;
    FunctionType* funcType = dynamic_cast<FunctionType*>(sym_ptr->getType());
    Type *retType = funcType->getRetType();
    if (para_list.size() == 0)
    {
        fprintf(yyout, "define %s %s() {\n", retType->toStr().c_str(), sym_ptr->toStr().c_str());
        std::set<BasicBlock*> v;
        std::list<BasicBlock*> q;
        q.push_back(entry);
        v.insert(entry);
        while (!q.empty())
        {
            auto bb = q.front();
            q.pop_front();
            bb->output();
            for (auto succ = bb->succ_begin(); succ != bb->succ_end(); succ++)
            {
                if (v.find(*succ) == v.end())
                {
                    v.insert(*succ);
                    q.push_back(*succ);
                }
            }
        }
        fprintf(yyout, "}\n");
    }
    else
    {
        fprintf(yyout, "define %s %s(", retType->toStr().c_str(), sym_ptr->toStr().c_str());
        for (int i = 0; i < (int)para_list.size(); i++)
        {
            if (i == 0)
            {
                fprintf(yyout, "%s %s", para_list[i]->getType()->toStr().c_str(), para_list[i]->toStr().c_str());
            }
            else
            {
                fprintf(yyout, ",%s %s", para_list[i]->getType()->toStr().c_str(), para_list[i]->toStr().c_str());
            }
        }
        fprintf(yyout, ") {\n");

            
        std::set<BasicBlock*> v;
        std::list<BasicBlock*> q;
        q.push_back(entry);
        v.insert(entry);
        while (!q.empty())
        {
            auto bb = q.front();
            q.pop_front();
            bb->output();
            for (auto succ = bb->succ_begin(); succ != bb->succ_end(); succ++)
            {
                if (v.find(*succ) == v.end())
                {
                    v.insert(*succ);
                    q.push_back(*succ);
                }
            }
        }
        fprintf(yyout, "}\n");
    }
}

void Function::genMachineCode(AsmBuilder* builder) 
{
    auto cur_unit = builder->getUnit();
    MachineFunction* cur_func;
    if ((unsigned)para_list.size())
    {
        cur_func = new MachineFunction(cur_unit, this->sym_ptr, para_list);
    }
    else
    {
        cur_func = new MachineFunction(cur_unit, this->sym_ptr);
    }
    builder->setFunction(cur_func);    

    std::map<BasicBlock*, MachineBlock*> map;
    for(auto block : block_list)
    {
        block->genMachineCode(builder);
        map[block] = builder->getBlock();
    }
    // Add pred and succ for every block
    for(auto block : block_list)
    {
        auto mblock = map[block];
        for (auto pred = block->pred_begin(); pred != block->pred_end(); pred++)
            mblock->addPred(map[*pred]);
        for (auto succ = block->succ_begin(); succ != block->succ_end(); succ++)
            mblock->addSucc(map[*succ]);
    }
    cur_unit->InsertFunc(cur_func);

}
