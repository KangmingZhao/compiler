#ifndef __AST_H__
#define __AST_H__

#include <fstream>

class SymbolEntry;



class Node
{
private:
    static int counter;
    int seq;
public:
    Node();
    int getSeq() const {return seq;};
    virtual void output(int level) = 0;
};

class ExprNode : public Node
{
protected:
    SymbolEntry *symbolEntry;
public:
    ExprNode(SymbolEntry *symbolEntry) : symbolEntry(symbolEntry){};
    std::string get_name();
};

class BinaryExpr : public ExprNode
{
private:
    bool is_crement;
    int op;
    ExprNode *expr1, *expr2;
    ExprNode* ID;
public:
    enum {ADD, SUB, MUL, DIV, MOD, AND, OR, LESS, INCREMENT_BEFORE, INCREMENT_AFTER, DECREMENT_BEFORE, DECREMENT_AFTER};
    BinaryExpr(SymbolEntry* se, int op, ExprNode* expr1, ExprNode* expr2) : ExprNode(se), op(op), expr1(expr1), expr2(expr2) { is_crement = 0; };
    BinaryExpr(SymbolEntry* se, int op, ExprNode* ID) : ExprNode(se), op(op), ID(ID) { is_crement = 1; };
    void output(int level);
};



class Constant : public ExprNode
{
public:
    Constant(SymbolEntry *se) : ExprNode(se){};
    void output(int level);
};




class ArrDimNode: public Node //: public ExprNode
{
    ArrDimNode* arr1;
    ArrDimNode* arr2;
    ExprNode *dimension_size;
    bool is_link;
public:
    ArrDimNode(ArrDimNode* arr1, ArrDimNode* arr2) : arr1(arr1), arr2(arr2), dimension_size(nullptr) { is_link = 1; };
    ArrDimNode(ExprNode* dimension_size) : dimension_size(dimension_size) { is_link = 0; };
    void output(int level);
};


class Id : public ExprNode
{
    //ArrDimNode** Dimension;
    int id_type;
    ArrDimNode* Dimension;
public:
    enum { DEFAULT, INT_ARRAY };
    //���𾪣�ԭ��ExprNode(se),  Dimension(nullptr).id_type(DEFAULT) ���ֳ�ʼ����ʽ����ʼ����˳��Ҫ��������һ������c++��ѧ��
    Id(SymbolEntry* se) : ExprNode(se), id_type(DEFAULT), Dimension(nullptr) { };
    Id(SymbolEntry* se, ArrDimNode* Dimension) : ExprNode(se), id_type(INT_ARRAY), Dimension(Dimension) {};
    ArrDimNode* getDimension() { return Dimension; };
    void output(int level);
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
};

class SeqNode : public StmtNode
{
private:
    StmtNode *stmt1, *stmt2;
public:
    SeqNode(StmtNode *stmt1, StmtNode *stmt2) : stmt1(stmt1), stmt2(stmt2){};
    //����parser.y�����������������ǰѱ��ʽ���͵������ʽƴ��һ���õġ�
    void output(int level);
};

class DeclStmt : public StmtNode
{
private:
    Id *id;
public:
    DeclStmt(Id *id) : id(id){};
    void output(int level);
};

class IfStmt : public StmtNode
{
private:
    ExprNode *cond;
    StmtNode *thenStmt;
public:
    IfStmt(ExprNode *cond, StmtNode *thenStmt) : cond(cond), thenStmt(thenStmt){};
    void output(int level);
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
};

class ReturnStmt : public StmtNode
{
private:
    ExprNode *retValue;
public:
    ReturnStmt(ExprNode*retValue) : retValue(retValue) {};
    void output(int level);
};

class AssignStmt : public StmtNode
{
private:
    ExprNode *lval;
    ExprNode *expr;
public:
    AssignStmt(ExprNode *lval, ExprNode *expr) : lval(lval), expr(expr) {};
    void output(int level);
};

class FunctionDef : public StmtNode
{
private:
    SymbolEntry *se;
    StmtNode *stmt;
public:
    FunctionDef(SymbolEntry *se, StmtNode *stmt) : se(se), stmt(stmt){};
    void output(int level);
};

/*�����������������������������������������������������Լ��ӵġ�������������������������������������������*/
class WhileStmt : public StmtNode
{
private:
    ExprNode* cond;
    StmtNode* doStmt;
public:
    WhileStmt(ExprNode* cond, StmtNode* doStmt) : cond(cond), doStmt(doStmt){};
    //ܳ����������ֻ�ǵ����ķ��벻����ִ���ˣ�������ֻ�ǰ�while��ʾ�������ɲ�������ж�����Ȼ��ѭ����
    void output(int level);
};


class DoNothingStmt : public StmtNode
{
private:
    ExprNode* do_nothing_node;
public:
    DoNothingStmt(ExprNode* do_nothing_node) : do_nothing_node(do_nothing_node){};
    //ܳ����������ֻ�ǵ����ķ��벻����ִ���ˣ�������ֻ�ǰ�while��ʾ�������ɲ�������ж�����Ȼ��ѭ����
    void output(int level);
};

//class ForStmt : public StmtNode
//{
//private:
//
//
//    ExprNode* cond;
//
//    StmtNode* doStmt;
//public:
//    void output(int level);
//};

/*��������������������������������������������������������������������������������������������������������*/






class Ast
{
private:
    Node* root;
public:
    Ast() {root = nullptr;}
    void setRoot(Node*n) {root = n;}
    void output();
};

#endif
