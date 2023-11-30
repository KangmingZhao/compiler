#ifndef __AST_H__
#define __AST_H__

#include <fstream>
#include "Operand.h"

#define ERROR_MESSAGE_WRITE_INTO_AST 1

#define LEGAL_VAR 0
#define NOT_DEFINED 1
#define REDEFINATION 2

class SymbolEntry;
class Unit;
class Function;
class BasicBlock;
class Instruction;
class IRBuilder;

class Node
{
private:
    static int counter;
    int seq;
protected:
    std::vector<BasicBlock**> true_list;
    std::vector<BasicBlock**> false_list;
    static IRBuilder *builder;
    void backPatch(std::vector<BasicBlock**> &list, BasicBlock*target);
    std::vector<BasicBlock**> merge(std::vector<BasicBlock**> &list1, std::vector<BasicBlock**> &list2);

public:
    Node();
    int getSeq() const {return seq;};
    static void setIRBuilder(IRBuilder*ib) {builder = ib;};
    virtual void output(int level) = 0;
    virtual void typeCheck() = 0;
    virtual void genCode() = 0;
    std::vector<BasicBlock**>& trueList() {return true_list;}
    std::vector<BasicBlock**>& falseList() {return false_list;}
};

class ExprNode : public Node
{
protected:
    SymbolEntry *symbolEntry;
    Operand *dst;   // The result of the subtree is stored into dst.
public:
    ExprNode(SymbolEntry *symbolEntry) : symbolEntry(symbolEntry){};
    std::string get_name();
    SymbolEntry* get_symbolEntry() { return symbolEntry; };
    Operand* getOperand() {return dst;};
    SymbolEntry* getSymPtr() {return symbolEntry;};
};


class UnaryExpr : public ExprNode
{
private:
    int op;
    ExprNode* expr;
public:
    enum { ADD, SUB, NOT };
    UnaryExpr(SymbolEntry* se, int op, ExprNode* expr) : ExprNode(se), op(op), expr(expr) {};
    void output(int level);
    void typeCheck();
    void genCode();
};



class BinaryExpr : public ExprNode
{
private:
    bool is_crement;
    int op;
    ExprNode* expr1, * expr2;
    ExprNode* ID;
public:
    enum { ADD, SUB, MUL, DIV, MOD, AND, OR, LESS, GREATER, INCREMENT_BEFORE, INCREMENT_AFTER, DECREMENT_BEFORE, DECREMENT_AFTER, LESSEQUAL, GREATEREQUAL, EQUAL, NOTEQUAL };
    BinaryExpr(SymbolEntry* se, int op, ExprNode* expr1, ExprNode* expr2) : ExprNode(se), op(op), expr1(expr1), expr2(expr2) { is_crement = 0; };
    BinaryExpr(SymbolEntry* se, int op, ExprNode* ID) : ExprNode(se), op(op), ID(ID) { is_crement = 1; };
    void output(int level);
    void typeCheck();
    void genCode();
};

class Constant : public ExprNode
{
public:
    Constant(SymbolEntry *se) : ExprNode(se){dst = new Operand(se);};
    void output(int level);
    void typeCheck();
    void genCode();
};



class InitNode
{
    bool is_exp;//很多initNode结点起到的是连接的作用，我们要输出的是存了表达式的结点。
    InitNode* node1;
    InitNode* node2;
    ExprNode* value_here;

    bool is_checkpoint;//这个东西是输出维度时用的，一个checkpoint结点代表着，这个结点是一个新的{}结点。
public:
    InitNode(ExprNode* value_here) :value_here(value_here) { is_exp = 1; is_checkpoint = 0; };
    InitNode(InitNode* node1, InitNode* node2) :node1(node1), node2(node2) { is_exp = 0; is_checkpoint = 0; };
    void output(int level, int dim, int* dim_record);
    void i_m_checkpoint()
    {
        is_checkpoint = 1;
    }
    void typeCheck();
    void genCode();
};

class ArrDimNode : public Node //: public ExprNode
{
    ArrDimNode* arr1;
    ArrDimNode* arr2;
    ExprNode* dimension_size;
    bool is_link;
public:
    ArrDimNode(ArrDimNode* arr1, ArrDimNode* arr2) : arr1(arr1), arr2(arr2), dimension_size(nullptr) { is_link = 1; };
    ArrDimNode(ExprNode* dimension_size) : dimension_size(dimension_size) { is_link = 0; };
    void output(int level);
    void typeCheck();
    void genCode();
};

class ParaNode : public Node
{
    ParaNode* para1;
    ParaNode* para2;
    ExprNode* para_expr;
    SymbolEntry* ID_se;
    bool is_link;
public:
    ParaNode(ParaNode* para1, ParaNode* para2) : para1(para1), para2(para2), para_expr(nullptr) { is_link = 1; };
    ParaNode(ExprNode* para_expr) : para_expr(para_expr) { is_link = 0; };
    ParaNode(SymbolEntry* ID_se) : ID_se(ID_se) { is_link = 0; };
    void output(int level);
    void typeCheck();
    void genCode();
};
class FunctCall : public ExprNode
{
    ParaNode* para_node;
public:
    FunctCall(SymbolEntry* se, ParaNode* para_node) : ExprNode(se), para_node(para_node) {};
    void output(int level);
    void typeCheck();
    void genCode();
};


class Id : public ExprNode
{
    //ArrDimNode** Dimension;
    int id_type;
    ArrDimNode* Dimension;
    InitNode* Init;
    int define_state = LEGAL_VAR;
public:
    enum { DEFAULT, INT_ARRAY, FUNCT };

    Id(SymbolEntry *se) : ExprNode(se), id_type(DEFAULT), Dimension(nullptr), Init(nullptr) {SymbolEntry *temp = new TemporarySymbolEntry(se->getType(), SymbolTable::getLabel()); dst = new Operand(temp);};
    
    
    
    //我震惊，原来ExprNode(se),  Dimension(nullptr).id_type(DEFAULT) 这种初始化方式，初始化的顺序要和声明的一样……c++白学了
    Id(SymbolEntry* se, ArrDimNode* Dimension, InitNode* Init) : ExprNode(se), id_type(INT_ARRAY), Dimension(Dimension), Init(Init) { reset_dim_record(); };
    Id(SymbolEntry* se, ArrDimNode* Dimension) : ExprNode(se), id_type(INT_ARRAY), Dimension(Dimension), Init(nullptr) { reset_dim_record(); };
    
    Id(SymbolEntry* se, int define_state) : ExprNode(se), id_type(DEFAULT), Dimension(nullptr), Init(nullptr), define_state(define_state) { SymbolEntry* temp = new TemporarySymbolEntry(se->getType(), SymbolTable::getLabel()); dst = new Operand(temp); };
    Id(SymbolEntry* se, ArrDimNode* Dimension, InitNode* Init, int define_state) : ExprNode(se), id_type(INT_ARRAY), Dimension(Dimension), Init(Init), define_state(define_state) { reset_dim_record(); };
    Id(SymbolEntry* se, ArrDimNode* Dimension, int define_state) : ExprNode(se), id_type(INT_ARRAY), Dimension(Dimension), Init(nullptr), define_state(define_state) { reset_dim_record(); };

    ArrDimNode* getDimension() { return Dimension; };

    void output(int level);
    void typeCheck();
    void genCode();

    int dim_record[10];
    void reset_dim_record()
    {
        for (int i = 0; i < 10; i++)
        {
            dim_record[i] = -1;
        }
    }
};

class StmtNode : public Node
{};

class CompoundStmt : public StmtNode
{
private:
    StmtNode *stmt;
public:
    CompoundStmt(StmtNode *stmt) : stmt(stmt) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class SeqNode : public StmtNode
{
private:
    StmtNode *stmt1, *stmt2;
public:
    SeqNode(StmtNode *stmt1, StmtNode *stmt2) : stmt1(stmt1), stmt2(stmt2){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class DeclStmt : public StmtNode
{
private:
public:
    Id* id;
    DeclStmt(Id *id) : id(id){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class DeclInitStmt : public DeclStmt//////////////////////////////////////////////////
{
private:
    ExprNode* initVal;
public:
    DeclInitStmt(Id* id, ExprNode* initVal) :DeclStmt(id), initVal(initVal) {};
    void output(int level);
    void typeCheck();
    void genCode();
};



class ConstDeclInitStmt : public DeclStmt
{
private:
    ExprNode* initVal;
public:
    ConstDeclInitStmt(Id* id, ExprNode* initVal) :DeclStmt(id), initVal(initVal) {};
    void output(int level);
    void typeCheck();
    void genCode();
};
class DeclList : public StmtNode
{
protected:
    StmtNode* decl1, * decl2;
public:
    DeclList(StmtNode* decl1, StmtNode* decl2) :decl1(decl1), decl2(decl2) {};
    void output(int level);
    void typeCheck();
    void genCode();
};
class ConstDeclList : public StmtNode
{
protected:
    StmtNode* decl1, * decl2;
public:
    ConstDeclList(StmtNode* decl1, StmtNode* decl2) :decl1(decl1), decl2(decl2) {};
    void output(int level);
    void typeCheck();
    void genCode();
};///////
class EmptyStmt : public StmtNode {

public:
    EmptyStmt() {};
    void output(int level);
    void typeCheck() {};
    void genCode() {};
};




class IfStmt : public StmtNode
{
private:
    ExprNode *cond;
    StmtNode *thenStmt;
public:
    IfStmt(ExprNode *cond, StmtNode *thenStmt) : cond(cond), thenStmt(thenStmt){};
    void output(int level);
    void typeCheck();
    void genCode();
};

class IfElseStmt : public StmtNode
{
private:
    ExprNode *cond;
    StmtNode *thenStmt;
    StmtNode *elseStmt;
public:
    IfElseStmt(ExprNode *cond, StmtNode *thenStmt, StmtNode *elseStmt) : cond(cond), thenStmt(thenStmt), elseStmt(elseStmt) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class ReturnStmt : public StmtNode
{
private:
    ExprNode *retValue;
public:
    ReturnStmt(ExprNode*retValue) : retValue(retValue) {};
    ReturnStmt() : retValue(nullptr) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class AssignStmt : public StmtNode
{
private:
    ExprNode *lval;
    ExprNode *expr;
public:
    AssignStmt(ExprNode *lval, ExprNode *expr) : lval(lval), expr(expr) {};
    void output(int level);
    void typeCheck();
    void genCode();
};

class FunctionDef : public StmtNode
{
private:
    SymbolEntry *se;
    StmtNode *stmt;    
    ParaNode* paraStmt;
public:
    FunctionDef(SymbolEntry *se, StmtNode *stmt) : se(se), stmt(stmt){};
    FunctionDef(SymbolEntry* se, StmtNode* stmt, ParaNode* paraStmt) : se(se), stmt(stmt), paraStmt(paraStmt) {};
    void output(int level);
    void typeCheck();
    void genCode();
    SymbolEntry *getSymbolEntry() { return this->se; };
};





class WhileStmt : public StmtNode
{
private:
    ExprNode* cond;
    StmtNode* doStmt;
public:
    WhileStmt(ExprNode* cond, StmtNode* doStmt) : cond(cond), doStmt(doStmt) {};
    //艹，忘记这里只是单纯的翻译不用我执行了，我这里只是把while表示出来即可不用真的判断条件然后循环。
    void output(int level);
    void typeCheck();
    void genCode();
};
class BreakStmt : public StmtNode
{
    StmtNode* who_2_break;
    bool whether_valid;
public:
    BreakStmt(StmtNode* who_2_break) : who_2_break(who_2_break) , whether_valid(1){};
    BreakStmt() : who_2_break(nullptr), whether_valid(0) {};
    void output(int level);
    void match_with_loop(StmtNode* who_2_break) { this->who_2_break = who_2_break; whether_valid = 1; }
    void typeCheck();
    void genCode();

};
class ContinueStmt : public StmtNode
{
    StmtNode* who_2_continue;
    bool whether_valid;
public:
    ContinueStmt(StmtNode* who_2_continue) : who_2_continue(who_2_continue), whether_valid(1) {};
    ContinueStmt() : who_2_continue(nullptr), whether_valid(0) {};

    void output(int level);
    void match_with_loop(StmtNode* who_2_continue) { this->who_2_continue = who_2_continue; whether_valid = 1; }

    void typeCheck();
    void genCode();

};
class DoNothingStmt : public StmtNode
{
private:
    ExprNode* do_nothing_node;
public:
    DoNothingStmt(ExprNode* do_nothing_node) : do_nothing_node(do_nothing_node) {};
    //艹，忘记这里只是单纯的翻译不用我执行了，我这里只是把while表示出来即可不用真的判断条件然后循环。
    void output(int level);
    void typeCheck();
    void genCode();
};



class Ast
{
private:
    Node* root;
public:
    Ast() {root = nullptr;}
    void setRoot(Node*n) {root = n;}
    void output();
    void typeCheck();
    void genCode(Unit *unit);
};

#endif
