#ifndef __INSTRUCTION_H__
#define __INSTRUCTION_H__

#include "Operand.h"
#include "AsmBuilder.h"
#include <vector>
#include <map>
#include <sstream>
class BasicBlock;

class Instruction
{
public:
    Instruction(unsigned instType, BasicBlock *insert_bb = nullptr);
    virtual ~Instruction();
    BasicBlock *getParent();
    bool isUncond() const {return instType == UNCOND;};
    bool isCond() const {return instType == COND;};
    bool isAlloc() const {return instType == ALLOCA;};
    bool isRet() const {return instType==RET;};
    void setParent(BasicBlock *);
    void setNext(Instruction *);
    void setPrev(Instruction *);
    Instruction *getNext();
    Instruction *getPrev();
    virtual Operand *getDef() { return nullptr; }
    virtual std::vector<Operand *> getUse() { return {}; }
    virtual void output() const = 0;
    MachineOperand* genMachineOperand(Operand*);
    MachineOperand* genMachineReg(int reg);
    MachineOperand* genMachineVReg();
    MachineOperand* genMachineImm(int val);
    MachineOperand* genMachineLabel(int block_no);
    virtual void genMachineCode(AsmBuilder*) = 0;
protected:
    unsigned instType;
    unsigned opcode;
    Instruction *prev;
    Instruction *next;
    BasicBlock *parent;
    std::vector<Operand*> operands;
    enum {BINARY, COND, UNCOND, RET, LOAD, STORE, CMP, ALLOCA,GLOBAL,CALL,UNARY,NOT,TEMP};
};

// meaningless instruction, used as the head node of the instruction list.
class DummyInstruction : public Instruction
{
public:
    DummyInstruction() : Instruction(-1, nullptr) {};
    void output() const {};
    void genMachineCode(AsmBuilder*) {};
};

class AllocaInstruction : public Instruction
{
    bool funct = 0;

    //-1 means need no need for reg, 0-3 means the index of reg.
    int need_register = -1;
public:
    void set_funct(int need_register = -1) { funct = 1; this->need_register = need_register; };
    AllocaInstruction(Operand *dst, SymbolEntry *se, BasicBlock *insert_bb = nullptr);
    ~AllocaInstruction();
    void output() const;
    Operand *getDef() { return operands[0]; }
     void genMachineCode(AsmBuilder*);
private:
    SymbolEntry *se;
};

class AllocaArrInstruction : public Instruction
{
public:
    AllocaArrInstruction(Operand* dst, SymbolEntry* se, BasicBlock* insert_bb = nullptr);
    ~AllocaArrInstruction();
    void output() const;
    Operand* getDef() { return operands[0]; }
private:
    SymbolEntry* se;
};

class LoadInstruction : public Instruction
{
public:
    LoadInstruction(Operand *dst, Operand *src_addr, BasicBlock *insert_bb = nullptr);
    ~LoadInstruction();
    void output() const;
    Operand *getDef() { return operands[0]; }
    std::vector<Operand *> getUse() { return {operands[1]}; }
    void genMachineCode(AsmBuilder*);
};

class StoreInstruction : public Instruction
{
    bool whether_para = 0;
public:
    StoreInstruction(Operand *dst_addr, Operand *src, BasicBlock *insert_bb = nullptr);
    StoreInstruction(Operand* dst_addr, Operand* src, BasicBlock* insert_bb, bool whether_para);
    ~StoreInstruction();
    void output() const;
    std::vector<Operand *> getUse() { return {operands[0], operands[1]}; }
    void genMachineCode(AsmBuilder*);
};

class BinaryInstruction : public Instruction
{
public:
    BinaryInstruction(unsigned opcode, Operand *dst, Operand *src1, Operand *src2, BasicBlock *insert_bb = nullptr);
    ~BinaryInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
    //enum {SUB, ADD, AND, OR};
    enum {
        //arithmetic
        ADD, SUB, MUL, DIV, MOD,

        //logic
        AND, OR,

        //else
        INCREMENT_BEFORE, INCREMENT_AFTER, DECREMENT_BEFORE, DECREMENT_AFTER
    };

    Operand *getDef() { return operands[0]; }
    std::vector<Operand *> getUse() { return {operands[1], operands[2]}; }
};

class CmpInstruction : public Instruction
{
public:
    CmpInstruction(unsigned opcode, Operand *dst, Operand *src1, Operand *src2, BasicBlock *insert_bb = nullptr);
    ~CmpInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
    enum {E, NE, L, GE, G, LE};
    Operand *getDef() { return operands[0]; }
    std::vector<Operand *> getUse() { return {operands[1], operands[2]}; }
};

// unconditional branch
class UncondBrInstruction : public Instruction
{
public:
    UncondBrInstruction(BasicBlock*, BasicBlock *insert_bb = nullptr);
    void output() const;
    void setBranch(BasicBlock *);
    BasicBlock *getBranch();
    void genMachineCode(AsmBuilder*);
    BasicBlock **patchBranch() {return &branch;};
protected:
    BasicBlock *branch;
};

// conditional branch
class CondBrInstruction : public Instruction
{
public:
    CondBrInstruction(BasicBlock*, BasicBlock*, Operand *, BasicBlock *insert_bb = nullptr);
    ~CondBrInstruction();
    void output() const;
    void setTrueBranch(BasicBlock*);
    BasicBlock* getTrueBranch();
    void setFalseBranch(BasicBlock*);
    BasicBlock* getFalseBranch();
    void genMachineCode(AsmBuilder*);
    BasicBlock **patchBranchTrue() {return &true_branch;};
    BasicBlock **patchBranchFalse() {return &false_branch;};
    std::vector<Operand *> getUse() { return {operands[0]}; }

protected:
    BasicBlock* true_branch;
    BasicBlock* false_branch;
};

class RetInstruction : public Instruction
{
public:
    RetInstruction(Operand *src, BasicBlock *insert_bb = nullptr);
    ~RetInstruction();
    std::vector<Operand *> getUse()
    {
        if (operands.size())
            return {operands[0]};
        else
            return {};
    }
    void output() const;
    void genMachineCode(AsmBuilder*);
};

class GlobalInstruction: public Instruction
{
public:
    // dst为变量名 expr为变量初始值 
    GlobalInstruction(Operand *dst, Operand *expr, SymbolEntry *se, BasicBlock *insertBB = nullptr);
    ~GlobalInstruction();
    void output() const;
    Operand *getDef() { return operands[0]; }
    void genMachineCode(AsmBuilder*);
private:
    SymbolEntry *se;

};

class CallInstruction : public Instruction
{
private:
    SymbolEntry *func;

public:
    CallInstruction(Operand *dst,
                    SymbolEntry *func,
                    std::vector<Operand *> params,
                    BasicBlock *insert_bb = nullptr);
    void output() const;
    void genMachineCode(AsmBuilder*);
};
class UnaryInstruction : public Instruction
{
public:
    UnaryInstruction(unsigned opcode, Operand *dst, Operand *src, BasicBlock *insert_bb = nullptr);
    ~UnaryInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
    enum
    {
        ADD,
        SUB,
        NOT
    };
};

class NotInstruction : public Instruction
{
public:
    NotInstruction(Operand *dst, Operand *src, BasicBlock *insert_bb = nullptr);
    ~NotInstruction();
    void genMachineCode(AsmBuilder*);
    void output() const;
};
class ZextInstruction : public Instruction
{
public:
    ZextInstruction(Operand *dst, Operand *src, BasicBlock *insertBB = nullptr);
    ~ZextInstruction(){};
    void genMachineCode(AsmBuilder*);
    void output() const;
};
#endif