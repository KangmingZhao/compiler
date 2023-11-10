#include "Ast.h"
#include "SymbolTable.h"
#include <string>
#include "Type.h"

extern FILE *yyout;
int Node::counter = 0;

Node::Node()
{
    seq = counter++;
}

void Ast::output()
{
    fprintf(yyout, "program\n");
    if(root != nullptr)
        root->output(4);
}

std::string ExprNode::get_name()
{
    return symbolEntry->toStr();
}

void BinaryExpr::output(int level)
{
    std::string op_str;
    switch(op)
    {
        case ADD:
            op_str = "add";
            break;
        case SUB:
            op_str = "sub";
            break;
        case MUL:
            op_str = "mul";
            break;
        case DIV:
            op_str = "div";
            break;
        case MOD:
            op_str = "mod";
            break;
        case AND:
            op_str = "and";
            break;
        case OR:
            op_str = "or";
            break;
        case LESS:
            op_str = "less";
            break;
        case INCREMENT_BEFORE:
            op_str = "increment_before";
            break;
        case DECREMENT_AFTER:
            op_str = "decrement_after";
            break;
        case INCREMENT_AFTER:
            op_str = "increment_after";
            break;
        case DECREMENT_BEFORE:
            op_str = "decrement_after";
         case GREATER:
            op_str = "greater";
            break;
        case LESSEQUAL:
            op_str = "less equal";
            break;
        case GREATEREQUAL:
            op_str = "greater equal";
            break;
        case EQUAL:
            op_str = "equal";
            break;
        case NOTEQUAL:
            op_str = "not equal";
            break;
    }
    fprintf(yyout, "%*cBinaryExpr\top: %s\n", level, ' ', op_str.c_str());
    if (is_crement)
    {
        ID->output(level + 4);
    }
    else
    {
        expr1->output(level + 4);
        expr2->output(level + 4);
    }
}

void UnaryExpr::output(int level)
{
    std::string op_str;
    switch(op)
    {
        case ADD:
            op_str = "add";
            break;
        case SUB:
            op_str = "sub";
            break;
    }
    fprintf(yyout, "%*cUnaryExpr\top: %s\n", level, ' ', op_str.c_str());
    expr->output(level + 4);
}
void Constant::output(int level)
{
    std::string type, value;
    type = symbolEntry->getType()->toStr();
    value = symbolEntry->toStr();
    if (this->symbolEntry->getType()->isInt())
    {
        fprintf(yyout, "%*cIntegerLiteral\tvalue: %s\ttype: %s\n", level, ' ',
            value.c_str(), type.c_str());
    }
    else if (this->symbolEntry->getType()->isFLOAT())
    {
        //好就好在symbolentry里面已经把浮点数、整数都转化为字符串了，这里就直接%s就好了不用在管占位符了
        fprintf(yyout, "%*cFLOATLiteral\tvalue: %s\ttype: %s\n", level, ' ',
            value.c_str(), type.c_str());
    }
}



void InitNode::output(int level, int dim, int* dim_record)
{
    if (is_checkpoint)
    {
        for (int i = dim + 1; i < 10; i++)
        {
            dim_record[i] = -1;
        }
        dim_record[dim]++;
        if (is_exp)
        {
            value_here->output(level);
            fprintf(yyout, "\t\t\t\tposition in the arr: ");
            for (int i = 0; i < 10 && dim_record[i] != -1; i++)
            {
                fprintf(yyout, " %d ", dim_record[i]);
            }
        }
        else
        {
            node1->output(level, dim + 1, dim_record);
            node2->output(level, dim + 1, dim_record);
        }
    }
    else
    {
        if (is_exp)
        {
            value_here->output(level);
            dim_record[dim]++;
            fprintf(yyout, "\t\t\t\tposition in the arr: ");
            for (int i = 0; i < 10 && dim_record[i] != -1; i++)
            {
                fprintf(yyout, " %d ", dim_record[i]);
            }
            fprintf(yyout, "\n");
        }
        else
        {
            node1->output(level, dim, dim_record);
            node2->output(level, dim, dim_record);
        }
    }
}

void ArrDimNode::output(int level)
{
    if (is_link)
    {
        arr1->output(level);
        arr2->output(level);
    }
    else
    {
        fprintf(yyout, "%*c\t\tdimension_size:\n", level, ' ');
        dimension_size->output(level + 20);
    }
}

void Id::output(int level)
{
    std::string name, type;
    int scope;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    scope = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getScope();
    fprintf(yyout, "%*cId\tname: %s\tscope: %d\ttype: %s\n", level, ' ',
            name.c_str(), scope, type.c_str());
    if (id_type == INT_ARRAY)
    {
        Dimension->output(level + 4);
        if (Init != nullptr)
        {
            Init->output(level + 4, 0, dim_record);
        }
    }
    /*if (symbolEntry->getType()->isINT_ARRAY())
    {
        symbolEntry->get_expr()->output(level + 4);
    }*/
}

void CompoundStmt::output(int level)
{
    fprintf(yyout, "%*cCompoundStmt\n", level, ' ');
    stmt->output(level + 4);
}

void SeqNode::output(int level)
{
    fprintf(yyout, "%*cSequence\n", level, ' ');
    stmt1->output(level + 4);
    stmt2->output(level + 4);
}

void DeclStmt::output(int level)
{
    fprintf(yyout, "%*cDeclStmt\n", level, ' ');
    id->output(level + 4);
}

void IfStmt::output(int level)
{
    fprintf(yyout, "%*cIfStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
}

void WhileStmt::output(int level)
{
    //不用实现，只是单纯的翻译出来的话，和if是一样的捏。
    fprintf(yyout, "%*cWhileStmt\n", level, ' ');
    cond->output(level + 4);
    doStmt->output(level + 4);
}

void DoNothingStmt::output(int level)
{
    fprintf(yyout, "%*cWhileStmt\n", level, ' ');
    do_nothing_node->output(level + 4);
}


void IfElseStmt::output(int level)
{
    fprintf(yyout, "%*cIfElseStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
    elseStmt->output(level + 4);
}

void ReturnStmt::output(int level)
{
    fprintf(yyout, "%*cReturnStmt\n", level, ' ');
    retValue->output(level + 4);
}

void AssignStmt::output(int level)
{
    fprintf(yyout, "%*cAssignStmt\n", level, ' ');
    lval->output(level + 4);
    expr->output(level + 4);
}

void FunctionDef::output(int level)
{
    std::string name, type;
    name = se->toStr();
    type = se->getType()->toStr();
    fprintf(yyout, "%*cFunctionDefine function name: %s, type: %s\n", level, ' ', 
            name.c_str(), type.c_str());
    stmt->output(level + 4);
}


