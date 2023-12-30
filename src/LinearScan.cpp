#include <algorithm>
#include "LinearScan.h"
#include "MachineCode.h"
#include "LiveVariableAnalysis.h"

LinearScan::LinearScan(MachineUnit *unit)
{
    this->unit = unit;
    for (int i = 4; i < 11; i++)
        regs.push_back(i);
}

void LinearScan::allocateRegisters()
{
    for (auto &f : unit->getFuncs())
    {
        func = f;
        bool success;
        success = false;
        while (!success)        // repeat until all vregs can be mapped
        {
            computeLiveIntervals();
            success = linearScanRegisterAllocation();
            if (success)        // all vregs can be mapped to real regs
                modifyCode();
            else                // spill vregs that can't be mapped to real regs
                genSpillCode();
        }
    }
}

void LinearScan::insertToActive(Interval *handled_interval)
{
     if (active.size() == 0)
    {
        active.push_back(handled_interval);
        return;
    }
    for (auto it = active.begin(); it != active.end(); it++)
    {
        if ((*it)->end > handled_interval->end)
        {
            active.insert(it, 1, handled_interval);
            return;
        }
    }
    active.push_back(handled_interval);
    return;
}

void LinearScan::makeDuChains()
{
    LiveVariableAnalysis lva;
    lva.pass(func);
    du_chains.clear();
    int i = 0;
    std::map<MachineOperand, std::set<MachineOperand *>> liveVar;
    for (auto &bb : func->getBlocks())
    {
        liveVar.clear();
        for (auto &t : bb->getLiveOut())
            liveVar[*t].insert(t);
        int no;
        no = i = bb->getInsts().size() + i;
        for (auto inst = bb->getInsts().rbegin(); inst != bb->getInsts().rend(); inst++)
        {
            (*inst)->setNo(no--);
            for (auto &def : (*inst)->getDef())
            {
                if (def->isVReg())
                {
                    auto &uses = liveVar[*def];
                    du_chains[def].insert(uses.begin(), uses.end());
                    auto &kill = lva.getAllUses()[*def];
                    std::set<MachineOperand *> res;
                    set_difference(uses.begin(), uses.end(), kill.begin(), kill.end(), inserter(res, res.end()));
                    liveVar[*def] = res;
                }
            }
            for (auto &use : (*inst)->getUse())
            {
                if (use->isVReg())
                    liveVar[*use].insert(use);
            }
        }
    }
}

void LinearScan::computeLiveIntervals()
{
    makeDuChains();
    intervals.clear();
    for (auto &du_chain : du_chains)
    {
        int t = -1;
        for (auto &use : du_chain.second)
            t = std::max(t, use->getParent()->getNo());
        Interval *interval = new Interval({du_chain.first->getParent()->getNo(), t, false, 0, 0, {du_chain.first}, du_chain.second});
        intervals.push_back(interval);
    }
    for (auto& interval : intervals) {
        auto uses = interval->uses;
        auto begin = interval->start;
        auto end = interval->end;
        for (auto block : func->getBlocks()) {
            auto liveIn = block->getLiveIn();
            auto liveOut = block->getLiveOut();
            bool in = false;
            bool out = false;
            for (auto use : uses)
                if (liveIn.count(use)) {
                    in = true;
                    break;
                }
            for (auto use : uses)
                if (liveOut.count(use)) {
                    out = true;
                    break;
                }
            if (in && out) {
                begin = std::min(begin, (*(block->begin()))->getNo());
                end = std::max(end, (*(block->rbegin()))->getNo());
            } else if (!in && out) {
                for (auto i : block->getInsts())
                    if (i->getDef().size() > 0 &&
                        i->getDef()[0] == *(uses.begin())) {
                        begin = std::min(begin, i->getNo());
                        break;
                    }
                end = std::max(end, (*(block->rbegin()))->getNo());
            } else if (in && !out) {
                begin = std::min(begin, (*(block->begin()))->getNo());
                int temp = 0;
                for (auto use : uses)
                    if (use->getParent()->getParent() == block)
                        temp = std::max(temp, use->getParent()->getNo());
                end = std::max(temp, end);
            }
        }
        interval->start = begin;
        interval->end = end;
    }
    bool change;
    change = true;
    while (change)
    {
        change = false;
        std::vector<Interval *> t(intervals.begin(), intervals.end());
        for (size_t i = 0; i < t.size(); i++)
            for (size_t j = i + 1; j < t.size(); j++)
            {
                Interval *w1 = t[i];
                Interval *w2 = t[j];
                if (**w1->defs.begin() == **w2->defs.begin())
                {
                    std::set<MachineOperand *> temp;
                    set_intersection(w1->uses.begin(), w1->uses.end(), w2->uses.begin(), w2->uses.end(), inserter(temp, temp.end()));
                    if (!temp.empty())
                    {
                        change = true;
                        w1->defs.insert(w2->defs.begin(), w2->defs.end());
                        w1->uses.insert(w2->uses.begin(), w2->uses.end());
                        // w1->start = std::min(w1->start, w2->start);
                        // w1->end = std::max(w1->end, w2->end);
                        auto w1Min = std::min(w1->start, w1->end);
                        auto w1Max = std::max(w1->start, w1->end);
                        auto w2Min = std::min(w2->start, w2->end);
                        auto w2Max = std::max(w2->start, w2->end);
                        w1->start = std::min(w1Min, w2Min);
                        w1->end = std::max(w1Max, w2Max);
                        auto it = std::find(intervals.begin(), intervals.end(), w2);
                        if (it != intervals.end())
                            intervals.erase(it);
                    }
                }
            }
    }
    sort(intervals.begin(), intervals.end(), compareStart);
}

bool LinearScan::linearScanRegisterAllocation()
{
    // Todo
    /*
        active ←{}
        foreach live interval i, in order of increasing start point
            ExpireOldIntervals(i)
            if length(active) = R then
                SpillAtInterval(i)
            else
                register[i] ← a register removed from pool of free registers
                add i to active, sorted by increasing end point
    */
    bool success = true;
    active.clear();
    regs.clear();

    for (int i = 4; i < 11; i++)
        regs.push_back(i);
    // for each intervals we begin the register allocation
    for (auto &i : intervals)
    {
         /*
        遍历active 列表，
        看该列表中是否存在结束时间早于unhandled interval 的interval，
        若有，则回收其寄存器
        */
        expireOldIntervals(i);
        
        if (regs.empty())
        {
            spillAtInterval(i);
            success = false;
        }
        else
        {
            i->rreg = regs.front();
            regs.erase(regs.begin());
            insertToActive(i);
        }
    }
    return success;
}

void LinearScan::modifyCode()
{
    for (auto &interval : intervals)
    {
        func->addSavedRegs(interval->rreg);
        for (auto def : interval->defs)
            def->setReg(interval->rreg);
        for (auto use : interval->uses)
            use->setReg(interval->rreg);
    }
}

void LinearScan::genSpillCode()
{
  for (auto &interval : intervals)
    {
        if (!interval->spill)
            continue;
        auto cur_func = func;
        MachineInstruction *cur_inst = 0;
        MachineBlock *cur_block;
        int offset = cur_func->AllocSpace(4);
        for (auto use : interval->uses)
        {
            auto reg = new MachineOperand(*use);
            cur_block = use->getParent()->getParent();
            auto useinst = use->getParent();
            cur_inst = new LoadMInstruction(cur_block, reg, new MachineOperand(MachineOperand::REG, 11), new MachineOperand(MachineOperand::IMM, -offset));
            for (auto i = cur_block->getInsts().begin(); i != cur_block->getInsts().end(); i++)
            {
                if (*i == useinst)
                {
                    cur_block->getInsts().insert(i, 1, cur_inst);
                    break;
                }
            }
        }
        for (auto def : interval->defs)
        {
            auto reg = new MachineOperand(*def);
            cur_block = def->getParent()->getParent();
            auto definst = def->getParent();
            cur_inst = new StoreMInstruction(cur_block, reg, new MachineOperand(MachineOperand::REG, 11), new MachineOperand(MachineOperand::IMM, -offset));
            for (auto i = cur_block->getInsts().begin(); i != cur_block->getInsts().end(); i++)
            {
                if (*i == definst)
                {
                    i++;
                    cur_block->getInsts().insert(i, 1, cur_inst);
                    break;
                }
            }
        }
    }
}

void LinearScan::expireOldIntervals(Interval *interval)
{
    // Todo
    /*
        foreach interval j in active, in order of increasing end point
            if endpoint[j] ≥ startpoint[i] then
                return
            remove j from active
            add register[j] to pool of free registers
    */
   /*remove the intervals that are not active
   */
    auto it = active.begin();
    while (it != active.end())
    {
        if ((*it)->end >= interval->start)
            return;
        // add register to free pool
        regs.push_back((*it)->rreg);
        it = active.erase(find(active.begin(), active.end(), *it));
        sort(regs.begin(), regs.end());
    }
}
/*
具体为，
选择策略就是看谁的活跃区间结束时间更晚，
，
*/
void LinearScan::spillAtInterval(Interval *interval)
{
    // Todo
    /*
        spill ← last interval in active
        if endpoint[spill] > endpoint[i] then
            register[i] ← register[spill]
            location[spill] ← new stack location
            remove spill from active
            add i to active, sorted by increasing end point
        else
            location[i] ← new stack location

    */
   //在active列表中最后一个interval 和当前unhandled interval 中选择一个interval 将其溢出到栈中
    auto spill = active.back();
    if (spill->end > interval->end)
    {
        
        /*如果是active 列表中的interval 结束时间更晚，需要置位
其spill 标志位，并将其占用的寄存器分配给unhandled interval，再按照unhandled interval
活跃区间结束位置，将其插入到active 列表中。*/
        spill->spill = true;
        interval->rreg = spill->rreg;
        insertToActive(interval);
    }
    else
    {
        //如果是unhandled interval 的结束时间更晚，只需要置位其spill 标志位即可
        interval->spill = true;
    }
}

bool LinearScan::compareStart(Interval *a, Interval *b)
{
    return a->start < b->start;
}