#ifndef __AST_H__
#define __AST_H__

#include <fstream>
#include "Operand.h"
#include "Type.h"
#include <unordered_map>
#include<iostream>


#define ERROR_MESSAGE_WRITE_INTO_AST 0
#define PRE_CAL_ERROR_MEETING_VAL -1145141919

#define LEGAL_VAR 0
#define NOT_DEFINED 1
#define REDEFINATION 2

class SymbolEntry;
class Unit;
class Function;
class BasicBlock;
class Instruction;
class CondBrInstruction;
class IRBuilder;


class ControlFlowGraph
{
    BasicBlock* entry;
    std::unordered_map<BasicBlock*, std::vector<BasicBlock*>> map_blocks;
public:
    ControlFlowGraph(BasicBlock* entry) :entry(entry) {};
    void add_map(BasicBlock* in_bb, BasicBlock* exit_bb) { map_blocks[in_bb].push_back(exit_bb); };
    void build_BB_link();
};

class LoopManager
{
    std::vector<BasicBlock*> end_loop_bb_stack;//used for break. directly goto end
    std::vector<BasicBlock*> loop_cond_bb_stack;//used for continue. go back to cond to check whether enter next loop
public:
    bool empty() { return end_loop_bb_stack.size() == 0; };
    int num_of_loop_bb_in_stack = 0;
    int new_loop_enter(BasicBlock* p_cond_bb, BasicBlock* p_end_bb)
    {
        end_loop_bb_stack.push_back(p_end_bb);
        loop_cond_bb_stack.push_back(p_cond_bb);
        num_of_loop_bb_in_stack++;
        return num_of_loop_bb_in_stack;
    }
    void loop_end(int recorded_loop_bb_in_stack)
    {
        if (recorded_loop_bb_in_stack == num_of_loop_bb_in_stack)
        {
            end_loop_bb_stack.pop_back();
            loop_cond_bb_stack.pop_back();
            num_of_loop_bb_in_stack--;
        }
    }
    BasicBlock* get_end_bb() { return end_loop_bb_stack[end_loop_bb_stack.size() - 1]; };
    BasicBlock* get_cond_bb() { return loop_cond_bb_stack[loop_cond_bb_stack.size() - 1]; };
};


class Node
{
private:
    static int counter;
    int seq;
protected:
    //std::vector<BasicBlock**> true_list;
    //std::vector<BasicBlock**> false_list;
    static IRBuilder *builder;
    //void backPatch(std::vector<BasicBlock**> &list, BasicBlock*target);
    //std::vector<BasicBlock**> merge(std::vector<BasicBlock**> &list1, std::vector<BasicBlock**> &list2);


    std::vector<CondBrInstruction*> i_true_list;
    std::vector<CondBrInstruction*> i_false_list;
    void ibackPatch(std::vector<CondBrInstruction*>& list, BasicBlock* target, bool is_true);
    std::vector<CondBrInstruction*> merge(std::vector<CondBrInstruction*>& list1, std::vector<CondBrInstruction*>& list2);

public:
    Node();
    int getSeq() const {return seq;};
    static void setIRBuilder(IRBuilder*ib) {builder = ib;};
    virtual void output(int level) = 0;
    virtual void typeCheck() = 0;
    virtual void genCode() = 0;

    virtual void mergeConstExp() = 0;


    //std::vector<BasicBlock**>& trueList() {return true_list;}
    //std::vector<BasicBlock**>& falseList() {return false_list;}

    std::vector<CondBrInstruction*>& itrueList() { return i_true_list; }
    std::vector<CondBrInstruction*>& ifalseList() { return i_false_list; }

    bool is_not_val = 1;
};

class ExprNode : public Node
{
protected:
    SymbolEntry *symbolEntry;
    Operand *dst;   // The result of the subtree is stored into dst.
public:
    bool need_2_be_bool;
    ExprNode(SymbolEntry* symbolEntry) : symbolEntry(symbolEntry), need_2_be_bool(0) {};
    std::string get_name();
    SymbolEntry* get_symbolEntry() { return symbolEntry; };
    Operand* getOperand() {return dst;};
    SymbolEntry* getSymPtr() {return symbolEntry;};

    //void setSymPtr(SymbolEntry* se) { symbolEntry = se; };
    virtual double cal_expr_val() = 0;
    bool judge_is_not_val(ExprNode* expr) { return expr->is_not_val ? 1 : 0; };

};

class Constant : public ExprNode
{
public:
    Constant(SymbolEntry* se) : ExprNode(se) { dst = new Operand(se); };
    void output(int level);
    void typeCheck();
    void genCode();

    double cal_expr_val()
    {
        
        return ((ConstantSymbolEntry*)getSymPtr())->getValue();
    }
    void mergeConstExp() {};
};

Constant* cal_expr(ExprNode* node2cal);


class UnaryExpr : public ExprNode
{
private:
    int op;
    ExprNode* expr;
public:
    enum { ADD, SUB, NOT };
    UnaryExpr(SymbolEntry* se, int op, ExprNode* expr) : ExprNode(se), op(op), expr(expr) {
        is_not_val = judge_is_not_val(expr); 
        dst = new Operand(se);
    };
    void output(int level);
    void typeCheck();
    void genCode();

    //我操这个超多三目运算符尊都好机掰炫酷，我尊牛逼！感谢坤坤
    double cal_expr_val() {
        return 
            expr->is_not_val ? 
            (
            op == ADD ? expr->cal_expr_val() : 
            op == SUB ? -1 * expr->cal_expr_val() : 
            (expr->cal_expr_val() == 0 ? 1 : 0)
            )
            :
            PRE_CAL_ERROR_MEETING_VAL;
    }

    void mergeConstExp()
    {
        if (expr != nullptr)
        {
            Constant* cal_result = cal_expr(expr);
            if (cal_result != nullptr)
                expr = cal_result;
            else
                expr->mergeConstExp();
        }
    }
};



class BinaryExpr : public ExprNode
{
private:
    bool is_crement;
    int op;
    ExprNode* expr1, * expr2;
    ExprNode* ID;
public:
    enum {
        //arithmetic
        ADD, SUB, MUL, DIV, MOD,
        arithmeticEnd,

        //logic
        AND, OR, 
        logicEnd,

        //relation
        LESS, GREATER, LESSEQUAL, GREATEREQUAL, EQUAL, NOTEQUAL,
        relationEnd,

        //else
        INCREMENT_BEFORE, INCREMENT_AFTER, DECREMENT_BEFORE, DECREMENT_AFTER };

    BinaryExpr(SymbolEntry* se, int op, ExprNode* expr1, ExprNode* expr2) : ExprNode(se), op(op), expr1(expr1), expr2(expr2) { 
        is_crement = 0; 
        is_not_val = (judge_is_not_val(expr1) && judge_is_not_val(expr2));
        dst = new Operand(se);
    };
    BinaryExpr(SymbolEntry* se, int op, ExprNode* ID) : ExprNode(se), op(op), ID(ID) { 
        is_crement = 1; 
        dst = new Operand(se);
    };
    void output(int level);
    void typeCheck();
    void genCode();

    double cal_expr_val()
    {
        //艹！这么多运算符要写死我啊！
        
        return
            (expr1->is_not_val && expr2->is_not_val) ?
            (
                op == ADD ? expr1->cal_expr_val() + expr2->cal_expr_val() :
                op == SUB ? expr1->cal_expr_val() - expr2->cal_expr_val() :
                op == MUL ? expr1->cal_expr_val() * expr2->cal_expr_val() :
                op == DIV ? expr1->cal_expr_val() / expr2->cal_expr_val() :
                (op == MOD && expr1->get_symbolEntry()->getType()->isInt() && expr2->get_symbolEntry()->getType()->isInt()) ? (int)expr1->cal_expr_val() % (int)expr2->cal_expr_val() :
                op == AND ? expr1->cal_expr_val() && expr2->cal_expr_val() :
                op == OR ? expr1->cal_expr_val() || expr2->cal_expr_val() :
                op == LESS ? expr1->cal_expr_val() < expr2->cal_expr_val() :
                op == GREATER ? expr1->cal_expr_val() > expr2->cal_expr_val() :
                op == LESSEQUAL ? expr1->cal_expr_val() <= expr2->cal_expr_val() :
                op == GREATEREQUAL ? expr1->cal_expr_val() >= expr2->cal_expr_val() :
                op == EQUAL ? expr1->cal_expr_val() == expr2->cal_expr_val() :
                op == NOTEQUAL ? expr1->cal_expr_val() != expr2->cal_expr_val() :
                1
                )
            :
            PRE_CAL_ERROR_MEETING_VAL;
    }


    void mergeConstExp()
    {
        if (expr1 != nullptr)
        {
            Constant* cal_result = cal_expr(expr1);
            if (cal_result != nullptr)
                expr1 = cal_result;
            else
                expr1->mergeConstExp();
        }

        if (expr2 != nullptr)
        {

            Constant* cal_result = cal_expr(expr2);
            if (cal_result != nullptr)
                expr2 = cal_result;
            else
                expr2->mergeConstExp();

        }

        if (ID != nullptr)
            ID->mergeConstExp();
    }
};



//class CalExprManager
//{
//public:
//    Constant* cal_expr(ExprNode* node2cal);
//    
//};

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

    void mergeConstExp()
    {
        if (value_here != nullptr)
        {
            Constant* cal_result = cal_expr(value_here);
            if (cal_result != nullptr)
                value_here = cal_result;
            else
                value_here->mergeConstExp();
        }

        if(node1!=nullptr)
            node1->mergeConstExp();

        if (node2 != nullptr)
            node2->mergeConstExp();
    }

};

class ArrDimNode : public Node //: public ExprNode
{
    ArrDimNode* arr1;
    ArrDimNode* arr2;
    ExprNode* dimension_size;
    bool is_link;

    enum ArrDimNode_TYPE {INT, FLOAT};
    ArrDimNode_TYPE node_type;

public:
    enum ArrDimNode_STATE { ACCESS, INIT };
    ArrDimNode_STATE node_state;
    ArrDimNode(ArrDimNode* arr1, ArrDimNode* arr2) : arr1(arr1), arr2(arr2), dimension_size(nullptr) { 
        is_link = 1;
    };
    ArrDimNode(ExprNode* dimension_size, ArrDimNode_STATE node_state) : dimension_size(dimension_size), node_state(node_state){
        is_link = 0;
        is_not_val = judge_is_not_val(dimension_size);
    };
    void output(int level);
    void typeCheck();
    void genCode();


    bool judge_is_not_val(ExprNode* expr) { return expr->is_not_val ? 1 : 0; };
    double cal_expr_val() {

        if (is_link)
        {
            return arr1->cal_expr_val() + arr2->cal_expr_val();
        }
        else
        {
            return dimension_size->is_not_val ? dimension_size->cal_expr_val() : PRE_CAL_ERROR_MEETING_VAL;
        }
    }


    void mergeConstExp()
    {
        if (dimension_size != nullptr)
        {
            Constant* cal_result = cal_expr(dimension_size);
            if (cal_result != nullptr)
                dimension_size = cal_result;
            else
                dimension_size->mergeConstExp();
        }

        if(arr1 != nullptr)
            arr1->mergeConstExp();
        if(arr2!=nullptr)
            arr2->mergeConstExp();
    }
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
    ParaNode(ExprNode* para_expr) : para_expr(para_expr) { 
        is_link = 0; 
        is_not_val = judge_is_not_val(para_expr);
    };
    ParaNode(SymbolEntry* ID_se) : ID_se(ID_se) { is_link = 0; };
    void output(int level);
    void typeCheck();
    void genCode();

    bool judge_is_not_val(ExprNode* expr) { return expr->is_not_val ? 1 : 0; };
    double cal_expr_val() {
        if (is_link)
        {
            return para1->cal_expr_val() + para2->cal_expr_val();
        }
        else
        {
            return is_not_val ? para_expr->cal_expr_val() : PRE_CAL_ERROR_MEETING_VAL;
        }
    }

    void mergeConstExp()
    {
        Constant* cal_result = cal_expr(para_expr);
        if (cal_result != nullptr)
            para_expr = cal_result;
        else
            para_expr->mergeConstExp();


        if(para1 != nullptr)
            para1->mergeConstExp();
        if(para2!=nullptr)
            para2->mergeConstExp();
    }
};


class FunctCall : public ExprNode
{
    ParaNode* para_node;
public:
    FunctCall(SymbolEntry* se, ParaNode* para_node) : ExprNode(se), para_node(para_node) {
        is_not_val = 0;
    if (symbolEntry)
    {
        // 构造目标寄存器的保存操作数
        if (se->getType() != TypeSystem::voidType)
        {
             SymbolEntry *temp =new TemporarySymbolEntry(symbolEntry->getType(), SymbolTable::getLabel());
                dst = new Operand(temp);
        }     
        else{
        dst=new Operand(se);
    }
    }
 
    };
    void output(int level);
    void typeCheck();
    void genCode();

    double cal_expr_val() { return PRE_CAL_ERROR_MEETING_VAL; }//这里先默认函数返回的是非const了。不然尊都写不出来了

    void mergeConstExp() {  };

};

//令人欣慰的是，c++的编译器并不会对数组越界进行检查，所以这里就可以合法摸鱼了（
//我们现在需要的就是维度检查。
//而且试了试，发现c++编译器并不会对	int arr[2][2] = { {1,2} };这样的语句报错。
class Id : public ExprNode
{
    //ArrDimNode** Dimension;
    int id_type;
    ArrDimNode* Dimension;
    InitNode* Init;
    int define_state = LEGAL_VAR;
    int dimension_size;

    void init_val_state(SymbolEntry* se) {
        if (se->isConstIdentifer())
            is_not_val = 1;
        else
            is_not_val = 0;
        dst = new Operand(se);
        //注意，这个函数这里会把dst覆盖掉，所以它应该在声明为临时identifier之前调用。
    }
public:
    enum { DEFAULT, INT_ARRAY, FUNCT };

    Id(SymbolEntry* se) : ExprNode(se), id_type(DEFAULT), Dimension(nullptr), Init(nullptr) { init_val_state(se); SymbolEntry* temp = new TemporarySymbolEntry(se->getType(), SymbolTable::getLabel()); dst = new Operand(temp); };
    
    
    
    //我震惊，原来ExprNode(se),  Dimension(nullptr).id_type(DEFAULT) 这种初始化方式，初始化的顺序要和声明的一样……c++白学了
    Id(SymbolEntry* se, ArrDimNode* Dimension, InitNode* Init) : ExprNode(se), id_type(INT_ARRAY), Dimension(Dimension), Init(Init) { reset_dim_record();  init_val_state(se);};
    Id(SymbolEntry* se, ArrDimNode* Dimension) : ExprNode(se), id_type(INT_ARRAY), Dimension(Dimension), Init(nullptr) { reset_dim_record(); init_val_state(se);};
    
    Id(SymbolEntry* se, int define_state) : ExprNode(se), id_type(DEFAULT), Dimension(nullptr), Init(nullptr), define_state(define_state) { init_val_state(se); SymbolEntry* temp = new TemporarySymbolEntry(se->getType(), SymbolTable::getLabel()); dst = new Operand(temp); };
    
    
    //用于数组初始化
    Id(SymbolEntry* se, ArrDimNode* Dimension, InitNode* Init, int define_state, int dimension_size) : ExprNode(se), id_type(INT_ARRAY), Dimension(Dimension), Init(Init), define_state(define_state), dimension_size(dimension_size) { reset_dim_record(); se->update_arr_dimension_recorder(dimension_size); init_val_state(se);};
    //用于读取数组下标
    Id(SymbolEntry* se, ArrDimNode* Dimension, int define_state) : ExprNode(se), id_type(INT_ARRAY), Dimension(Dimension), Init(nullptr), define_state(define_state) { init_val_state(se); SymbolEntry* temp = new TemporarySymbolEntry(se->getType(), SymbolTable::getLabel()); dst = new Operand(temp); reset_dim_record();};

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

    double cal_expr_val() { 
        if (getSymPtr()->isConstIdentifer())
        {
            return ((IdentifierSymbolEntry*)getSymPtr())->getValueExpr()->cal_expr_val();
        }
        else
            return PRE_CAL_ERROR_MEETING_VAL;
            
    }//直接返回非法。先不考虑const

    void mergeConstExp() { 
        if(Init!=nullptr)
            Init->mergeConstExp();
        if(Dimension!=nullptr)
            Dimension->mergeConstExp(); 
    };

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

    void mergeConstExp() {
        if (stmt != nullptr)
            stmt->mergeConstExp();
    };

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

    void mergeConstExp() {
        if (stmt1 != nullptr)
            stmt1->mergeConstExp();
        if (stmt2 != nullptr)
            stmt2->mergeConstExp();
    };

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

    void mergeConstExp() {
        if (id != nullptr)
            id->mergeConstExp();
    };

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


    void mergeConstExp()
    {
        if (initVal != nullptr)
        {
            Constant* cal_result = cal_expr(initVal);
            if (cal_result != nullptr)
                initVal = cal_result;
            else
                initVal->mergeConstExp();
        }
    }
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


    void mergeConstExp()
    {
        if (initVal != nullptr)
        {
            Constant* cal_result = cal_expr(initVal);
            if (cal_result != nullptr)
                initVal = cal_result;
            else
                initVal->mergeConstExp();
        }
    }
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

    void mergeConstExp() {
        if (decl1 != nullptr)
            decl1->mergeConstExp();
        if (decl2 != nullptr)
            decl2->mergeConstExp();
    };

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

    void mergeConstExp() {
        if (decl1 != nullptr)
            decl1->mergeConstExp();
        if (decl2 != nullptr)
            decl1->mergeConstExp();
    };

};///////
class EmptyStmt : public StmtNode {

public:
    EmptyStmt() {};
    void output(int level);
    void typeCheck() {};
    void genCode() {};

    void mergeConstExp() {};

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


    void mergeConstExp()
    {
        if (cond != nullptr)
        {
            Constant* cal_result = cal_expr(cond);
            if (cal_result != nullptr)
                cond = cal_result;
            else
                cond->mergeConstExp();
        }
        if (thenStmt != nullptr)
        {
            thenStmt->mergeConstExp();
        }
    }
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


    void mergeConstExp()
    {
        if (cond != nullptr)
        {
            Constant* cal_result = cal_expr(cond);
            if (cal_result != nullptr)
                cond = cal_result;
            else
                cond->mergeConstExp();
        }
        if (thenStmt != nullptr)
            thenStmt->mergeConstExp();
        if (elseStmt != nullptr)
            elseStmt->mergeConstExp();
    }
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

    void mergeConstExp()
    {
        if (retValue != nullptr)
        {
            Constant* cal_result = cal_expr(retValue);
            if (cal_result != nullptr)
                retValue = cal_result;
        }
    }
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


    void mergeConstExp()
    {
        if (expr != nullptr)
        {
            Constant* cal_result = cal_expr(expr);
            if (cal_result != nullptr)
                expr = cal_result;
            else
                expr->mergeConstExp();
        }
    }
};

class FunctionDef : public StmtNode
{
private:
    SymbolEntry *se;
    StmtNode *stmt;    
    ParaNode* paraStmt;
public:
    FunctionDef(SymbolEntry* se, StmtNode* stmt) : se(se), stmt(stmt), paraStmt(nullptr) {};
    FunctionDef(SymbolEntry* se, StmtNode* stmt, ParaNode* paraStmt) : se(se), stmt(stmt), paraStmt(paraStmt) {};
    void output(int level);
    void typeCheck();
    void genCode();
    SymbolEntry *getSymbolEntry() { return this->se; };

    void mergeConstExp() {
        if (stmt != nullptr)
            stmt->mergeConstExp();
    };

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


    void mergeConstExp()
    {
        if (cond != nullptr)
        {
            Constant* cal_result = cal_expr(cond);
            if (cal_result != nullptr)
                cond = cal_result;
            else
                cond->mergeConstExp();
        }
        if (doStmt != nullptr)
            doStmt->mergeConstExp();
    }
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

    void mergeConstExp() {};

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

    void mergeConstExp() {};


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
    void mergeConstExp() {};

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
    void mergeConstExp();
    void genCode(Unit *unit);
};

#endif
