#ifndef __IRBUILDER_H__
#define __IRBUILDER_H__

class Unit;
class Function;
class BasicBlock;
class ControlFlowGraph;

class IRBuilder
{
private:
    Unit *unit;
    BasicBlock *insertBB;   // The current basicblock that instructions should be inserted into.
    ControlFlowGraph* cfg;

public:
    IRBuilder(Unit*unit) : unit(unit){};
    void set_cfg(BasicBlock* entry) { cfg = new ControlFlowGraph(entry); };
    void build_link() { cfg->build_BB_link(); };
    void add_link(BasicBlock* in_bb, BasicBlock* exit_bb) { cfg->add_map(in_bb, exit_bb); };
    void setInsertBB(BasicBlock*bb){insertBB = bb;};
    Unit* getUnit(){return unit;};
    BasicBlock* getInsertBB(){return insertBB;};
};

#endif