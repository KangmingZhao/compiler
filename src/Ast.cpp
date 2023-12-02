#include "Ast.h"
#include "SymbolTable.h"
#include "Unit.h"
#include "Instruction.h"
#include "IRBuilder.h"
#include <string>
#include <assert.h>
#include<algorithm>

extern FILE *yyout;
int Node::counter = 0;
IRBuilder* Node::builder = nullptr;
bool isreturn=false;
Type *retVal;
std::vector<Type*> paramVector;
Node::Node()
{
    seq = counter++;
}

void Node::backPatch(std::vector<BasicBlock**> &list, BasicBlock*target)
{
    for(auto &bb:list)
        *bb = target;
}

std::vector<BasicBlock**> Node::merge(std::vector<BasicBlock**> &list1, std::vector<BasicBlock**> &list2)
{
    std::vector<BasicBlock**> res(list1);
    res.insert(res.end(), list2.begin(), list2.end());
    return res;
}

void Ast::genCode(Unit *unit)
{
    IRBuilder *builder = new IRBuilder(unit);
    Node::setIRBuilder(builder);
    root->genCode();
}

void FunctionDef::genCode()
{
    Unit *unit = builder->getUnit();
    Function *func = new Function(unit, se);
    BasicBlock *entry = func->getEntry();
    // set the insert point to the entry basicblock of this function.
    builder->setInsertBB(entry);

    stmt->genCode();

    /**
     * Construct control flow graph. You need do set successors and predecessors for each basic block.
     * Todo
    */
   
}

void BinaryExpr::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    Function *func = bb->getParent();
    if (op < arithmeticEnd)
    {
        //arithmetic op
        expr1->genCode();
        expr2->genCode();
        Operand* src1 = expr1->getOperand();
        Operand* src2 = expr2->getOperand();
        int opcode;
        switch (op)
        {
        case ADD:
            opcode = BinaryInstruction::ADD;
            break;
        case SUB:
            opcode = BinaryInstruction::SUB;
            break;
        case MUL:
            opcode = BinaryInstruction::MUL;
            break;
        case DIV:
            opcode = BinaryInstruction::DIV;
            break;
        case MOD:
            opcode = BinaryInstruction::MOD;
            break;
        default:
            opcode = -1;
            break;
        }
        new BinaryInstruction(opcode, dst, src1, src2, bb);
    }
    else if (op > arithmeticEnd && op < logicEnd)
    {
        //logical op
        switch (op)
        {
        case AND:

            BasicBlock* trueBB = new BasicBlock(func);  // if the result of lhs is true, jump to the trueBB.
            expr1->genCode();
            backPatch(expr1->trueList(), trueBB);
            builder->setInsertBB(trueBB);               // set the insert point to the trueBB so that intructions generated by expr2 will be inserted into it.
            expr2->genCode();
            true_list = expr2->trueList();
            false_list = merge(expr1->falseList(), expr2->falseList());
            break;
        //case OR:

        //    break;
        }
    }
    else if (op > logicEnd && op < relationEnd)
    {
        //relation op
    }
    else
    {
        //else
    }


    //if (op == AND)
    //{
    //    //��������һ���⡣
    //    
    //}
    //else if(op == OR)
    //{
    //    // Todo
    //}
    //else if(op >= LESS && op <= GREATER)
    //{
    //    // Todo
    //}
    //else if(op < arithmeticEnd)
    //{
    //}
}

void Constant::genCode()
{
    // we don't need to generate code.
}

void Id::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    Operand *addr = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getAddr();
    new LoadInstruction(dst, addr, bb);
}

void IfStmt::genCode()
{
    Function *func;
    BasicBlock *then_bb, *end_bb;

    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);

    cond->genCode();
    backPatch(cond->trueList(), then_bb);
    backPatch(cond->falseList(), end_bb);

    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    builder->setInsertBB(end_bb);
}

void IfElseStmt::genCode()
{
    // Todo
}

void CompoundStmt::genCode()
{
    // Todo
    if(stmt != nullptr)
        stmt->genCode();
}

void SeqNode::genCode()
{
    // Todo
    if (stmt1 != nullptr)
        stmt1->genCode();
    if (stmt2 != nullptr)
        stmt2->genCode();
}

void DeclStmt::genCode()
{
    IdentifierSymbolEntry *se = dynamic_cast<IdentifierSymbolEntry *>(id->getSymPtr());
    if(se->isGlobal())
    {
        //std::cout << "fuck" << std::endl;
        Operand *addr;
        SymbolEntry *addr_se;
        addr_se = new IdentifierSymbolEntry(*se);
        addr_se->setType(new PointerType(se->getType()));
        addr = new Operand(addr_se);
        se->setAddr(addr);
    }
    else if(se->isLocal())
    {
        //std::cout << "you" << std::endl;
        Function *func = builder->getInsertBB()->getParent();
        BasicBlock *entry = func->getEntry();
        Instruction *alloca;
        Operand *addr;
        SymbolEntry *addr_se;
        Type *type;
        type = new PointerType(se->getType());
        addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
        addr = new Operand(addr_se);
        alloca = new AllocaInstruction(addr, se);                   // allocate space for local id in function stack.
        entry->insertFront(alloca);                                 // allocate instructions should be inserted into the begin of the entry block.
        se->setAddr(addr);                                          // set the addr operand in symbol entry so that we can use it in subsequent code generation.
    }
    else
    {
        std::cout << "what the hell are you trying to decl?!" << std::endl;
    }
}

void ReturnStmt::genCode()
{
    // Todo
    retValue->genCode();
}

void AssignStmt::genCode()
{
    BasicBlock *bb = builder->getInsertBB();
    expr->genCode();
    Operand *addr = dynamic_cast<IdentifierSymbolEntry*>(lval->getSymPtr())->getAddr();
    Operand *src = expr->getOperand();
    /***
     * We haven't implemented array yet, the lval can only be ID. So we just store the result of the `expr` to the addr of the id.
     * If you want to implement array, you have to caculate the address first and then store the result into it.
     */
    new StoreInstruction(addr, src, bb);
}

void Ast::typeCheck()
{
    if(root != nullptr)
        root->typeCheck();
}

void FunctionDef::typeCheck()
{
    // Todo
    SymbolEntry *se = this->getSymbolEntry();
    Type *ret = ((FunctionType *)(se->getType()))->getRetType();
    if (stmt == nullptr&&ret != TypeSystem::voidType)
    {   
         fprintf(stderr, "function\'%s\'misses return\n",se->toStr().c_str());
        // 函数体空�?判断是否符合void
    }
    // 函数体不�?去看看是否符合声�?
    else{
        isreturn=false;
        stmt->typeCheck();
        if(!isreturn && ret != TypeSystem::voidType){//don't have return and not void
            fprintf(stderr, "function  \'%s\'misses return\n",se->toStr().c_str());
        }
        // has return  and wrong return  
        else if(isreturn&&ret!=TypeSystem::voidType)
        {
            if(ret!=retVal)
            {
            fprintf(stderr, "function \'%s\'has wrong return \n",se->toStr().c_str());
            }
        }

        }

    
    // if (stmt != nullptr)
    //     stmt->typeCheck();
    if(paraStmt != nullptr)
        paraStmt->typeCheck();
}


void BinaryExpr::typeCheck()
{
    // Todo
    /*Type* type1 = expr1->getSymPtr()->getType();
    Type * type2 = expr2->getSymPtr()->getType();
    if (type1 != type2)
    {
        fprintf(stderr, "type %s and %s mismatch \n",
          type1->toStr().c_str(), type2->toStr().c_str());
        if (ERROR_MESSAGE_WRITE_INTO_AST)
        {
            fprintf(yyout, "type %s and %s mismatch \n",
                type1->toStr().c_str(), type2->toStr().c_str());
        }
    }*/

    Type* type1 = expr1->getSymPtr()->getType();
    Type* type2 = expr2->getSymPtr()->getType();

    Type* Prior = type1->getKindValue() > type2->getKindValue() ? type1 : type2;
    Type* later = type1->getKindValue() < type2->getKindValue() ? type1 : type2;


    if (type1->get_range() != type2->get_range())
    {
        fprintf(stderr, "type %s and %s mismatch \n",
            type1->toStr().c_str(), type2->toStr().c_str());
        if (ERROR_MESSAGE_WRITE_INTO_AST)
        {
            fprintf(yyout, "type %s and %s mismatch \n",
                type1->toStr().c_str(), type2->toStr().c_str());
        }
    }
    else
    {
        if (type1->getKindValue() != type2->getKindValue())
        {
            fprintf(stderr, "type %s and %s mismatch , but we convert %s to %s for you \n",
                type1->toStr().c_str(), type2->toStr().c_str(),
                later->toStr().c_str(), Prior->toStr().c_str());
            if (ERROR_MESSAGE_WRITE_INTO_AST)
            {
                fprintf(yyout, "type %s and %s mismatch , but we convert %s to %s for you \n",
                    type1->toStr().c_str(), type2->toStr().c_str(), later->toStr().c_str(), Prior->toStr().c_str());
            }
        }
    }



    if (expr1 != nullptr)
        expr1->typeCheck();
    if (expr2 != nullptr)
        expr2->typeCheck();

}

void Constant::typeCheck()
{
    // Todo
}

void Id::typeCheck()
{
    // Todo


    if (Dimension != nullptr)
        Dimension->typeCheck();
    if (Init != nullptr)
        Init->typeCheck();

}

void IfStmt::typeCheck()
{
    // Todo


    if (cond != nullptr)
        cond->typeCheck();
    if (thenStmt != nullptr)
        thenStmt->typeCheck();

}

void IfElseStmt::typeCheck()
{
    // Todo


    if (cond != nullptr)
        cond->typeCheck();
    if (thenStmt != nullptr)
        thenStmt->typeCheck();
    if (elseStmt != nullptr)
        elseStmt->typeCheck();
}

void CompoundStmt::typeCheck()
{
    // Todo  
    if (stmt != nullptr)
        stmt->typeCheck();
}

void SeqNode::typeCheck()
{
    // Todo
    if (stmt1 != nullptr)
        stmt1->typeCheck();
    if (stmt2 != nullptr)
        stmt2->typeCheck();
}

void DeclStmt::typeCheck()
{
    // Todo
    if (id != nullptr)
        id->typeCheck();
}

void ReturnStmt::typeCheck()
{
    // Todo
    if(retValue)
    {
       isreturn=true;//说明真有返回的东�?
        retVal=retValue->getSymPtr()->getType(); //返回值类�?
    }


    if (retValue != nullptr)
        retValue->typeCheck();

}

void AssignStmt::typeCheck()
{
    // Todo
    //���ȼ�鸴�ƶ����ǲ���const��
    if (lval->get_symbolEntry()->isConstIdentifer())
    {
        fprintf(stderr, "identifier \"%s\" is const\n", lval->get_name().c_str());
    }


    Type* type1 = lval->getSymPtr()->getType();
    Type* type2 = expr->getSymPtr()->getType();

    if (type1->get_range() != type2->get_range())
    {
        fprintf(stderr, "type %s and %s mismatch \n",
            type1->toStr().c_str(), type2->toStr().c_str());
        if (ERROR_MESSAGE_WRITE_INTO_AST)
        {
            fprintf(yyout, "type %s and %s mismatch \n",
                type1->toStr().c_str(), type2->toStr().c_str());
        }
    }
    else
    {
        if (type1->getKindValue() != type2->getKindValue())
        {
            fprintf(stderr, "type %s and %s mismatch , but we convert %s to %s for you \n",
                type1->toStr().c_str(), type2->toStr().c_str(), type2->toStr().c_str(), type1->toStr().c_str());
            if (ERROR_MESSAGE_WRITE_INTO_AST)
            {
                fprintf(yyout, "type %s and %s mismatch , but we convert %s to %s for you \n",
                    type1->toStr().c_str(), type2->toStr().c_str(), type2->toStr().c_str(), type1->toStr().c_str());
            }
        }
    }


    if (lval != nullptr)
        lval->typeCheck();
    if (expr != nullptr)
        expr->typeCheck();
    //if (type1 != type2)
    //{
    //    fprintf(stderr, "type %s and %s mismatch \n",
    //        type1->toStr().c_str(), type2->toStr().c_str());
    //    if (ERROR_MESSAGE_WRITE_INTO_AST)
    //    {
    //        fprintf(yyout, "type %s and %s mismatch \n",
    //            type1->toStr().c_str(), type2->toStr().c_str());
    //    }
    //}
}





//������������������
/*ս������

UnaryExpr 
InitNode
ArrDimNode
ParaNode
FunctCall
DeclInitStmt
ConstDeclInitStmt
DeclList
ConstDeclList
WhileStmt
BreakStmt
ContinueStmt
DoNothingStmt*/

void ArrDimNode::typeCheck()
{
    //����Ҫ��鴫�������ǲ���i32����Ϊ�Ƿ����±꣬ʲôvoid��float�����С�
    if (!dimension_size->get_symbolEntry()->getType()->isInt())
    {
        fprintf(stderr, "i32 is needed, but %s is given \n",
            dimension_size->get_symbolEntry()->getType()->toStr().c_str());
    }



    if (arr1 != nullptr)
        arr1->typeCheck();
    if (arr2 != nullptr)
        arr2->typeCheck();
    if (dimension_size != nullptr)
        dimension_size->typeCheck();
}
void ArrDimNode::genCode()
{

}

void DoNothingStmt::typeCheck()
{

    if (do_nothing_node != nullptr)
        do_nothing_node->typeCheck();
}
void DoNothingStmt::genCode()
{

}

void ContinueStmt::typeCheck()
{
    if (who_2_continue != nullptr)
        who_2_continue->typeCheck();
}
void ContinueStmt::genCode()
{

}

void BreakStmt::typeCheck()
{

    if (who_2_break != nullptr)
        who_2_break->typeCheck();
}
void BreakStmt::genCode()
{

}

void WhileStmt::typeCheck()
{

    if (cond != nullptr)
        cond->typeCheck();

    if (doStmt != nullptr)
        doStmt->typeCheck();
}
void WhileStmt::genCode()
{

}

void ConstDeclList::typeCheck()
{

    if (decl1 != nullptr)
        decl1->typeCheck();

    if (decl2 != nullptr)
        decl2->typeCheck();
}
void ConstDeclList::genCode()
{

}

void DeclList::typeCheck()
{

    if (decl1 != nullptr)
        decl1->typeCheck();

    if (decl2 != nullptr)
        decl2->typeCheck();
}
void DeclList::genCode()
{

}

void ConstDeclInitStmt::typeCheck()
{

    if (initVal != nullptr)
        initVal->typeCheck();

}
void ConstDeclInitStmt::genCode()
{

}

void DeclInitStmt::typeCheck()
{
    if (initVal != nullptr)
        initVal->typeCheck();
}
void DeclInitStmt::genCode()
{

}

void FunctCall::typeCheck()
{
    paramVector.clear();
    if (para_node != nullptr)
        para_node->typeCheck();
  
     int paramR_num=paramVector.size();
     SymbolEntry* s =this->symbolEntry;
    FunctionType* type=(FunctionType*)s->getType();
    int paramF_num=type->paramsType.size();
     if(paramR_num!=paramF_num)
     {
          //printf("Fnum:%d,Rnum:%d\n",paramF_num,paramR_num);
          fprintf(stderr, "function  \'%s\'has wrong  params num!\n",symbolEntry->toStr().c_str());
    }
    std::vector<Type*> paramR = paramVector;
    std::vector<Type*> paramF = type->paramsType;
    bool equal = true;
    if (paramR.size() == paramF.size())
    {
    
    for (size_t i = 0; i < paramR.size(); ++i)
    {
        if (paramR[i] != paramF[i])
        {
            equal = false;
            break;
        }
    }
    }
    if(!equal)
    {
         fprintf(stderr, "function  \'%s\'has wrong params type\n",symbolEntry->toStr().c_str());
    }
    
}
void FunctCall::genCode()
{

}

void ParaNode::typeCheck()
{
    
    if(!is_link)
    {
        Type *type=para_expr->getSymPtr()->getType();
        paramVector.push_back(type);
    }
    if (para1 != nullptr)
        para1->typeCheck();
    if (para2 != nullptr)
        para2->typeCheck();
    if (para_expr != nullptr)
        para_expr->typeCheck();
}
void ParaNode::genCode()
{

}

void InitNode::typeCheck()
{
    if (node1 != nullptr)
        node1->typeCheck();
    if (node2 != nullptr)
        node2->typeCheck();
    if (value_here != nullptr)
        value_here->typeCheck();
}
void InitNode::genCode()
{

}

void UnaryExpr::typeCheck()
{
    if (expr != nullptr)
        expr->typeCheck();
}
void UnaryExpr::genCode()
{

}




//����������ֵֹĶ�����?


std::string ExprNode::get_name()
{
    return symbolEntry->toStr();
}









void BinaryExpr::output(int level)
{
    float temp_store = cal_expr_val();
    if (temp_store != PRE_CAL_ERROR_MEETING_VAL)
    {
        if (getSymPtr()->getType()->isFLOAT())
            fprintf(yyout, "%*c\tExprValue:\t%f\n", level, ' ', temp_store);
        else if (getSymPtr()->getType()->isInt())
            fprintf(yyout, "%*c\tExprValue:\t%d\n", level, ' ', (int)temp_store);
        return;
    }
    //����ֱ�������


    //typeCheck();
    std::string op_str;
    switch (op)
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
    float temp_store = cal_expr_val();
    if (temp_store != PRE_CAL_ERROR_MEETING_VAL)
    {
        if (getSymPtr()->getType()->isFLOAT())
            fprintf(yyout, "%*c\tExprValue:\t%f\n", level, ' ', temp_store);
        else if (getSymPtr()->getType()->isInt())
            fprintf(yyout, "%*c\tExprValue:\t%d\n", level, ' ', (int)temp_store);
        return;
    }


    std::string op_str;
    switch (op)
    {
    case ADD:
        op_str = "add";
        break;
    case SUB:
        op_str = "sub";
        break;
    case NOT:
        op_str = "NOT";
        break;
    }
    fprintf(yyout, "%*cUnaryExpr\top: %s\n", level, ' ', op_str.c_str());
    expr->output(level + 4);
}
void FunctCall::output(int level)
{
    fprintf(yyout, "%*ccall_funct %s\n", level, ' ', get_symbolEntry()->toStr().c_str());
    if (para_node != nullptr)
    {
        para_node->output(level + 4);
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
        //typeCheck();
        if(node_state == ACCESS)
            fprintf(yyout, "%*c\t\taccessing_pos:\n", level, ' ');
        else
            fprintf(yyout, "%*c\t\tdimension_size:\n", level, ' ');
        if (is_not_val)
            fprintf(yyout, "%*c\t\t%d\n", level, ' ', (int)dimension_size->cal_expr_val());
            //std::cout << dimension_size->cal_expr_val() << std::endl;
        else
        {
            if (node_state == ACCESS)
                dimension_size->output(level + 20);
            else
                //����
                fprintf(stderr, "not a const \n");
        }
        //dimension_size->output(level + 20);
    }
}



void ParaNode::output(int level)
{
    if (is_link)
    {
        para1->output(level);
        para2->output(level);
    }
    else
    {
        fprintf(yyout, "%*c\t\tpara expr:\n", level, ' ');
        para_expr->output(level + 20);
    }
}
void DeclInitStmt::output(int level)
{
    fprintf(yyout, "%*cDeclInitStmt\n", level, ' ');
    id->output(level + 4);
    initVal->output(level + 4);
}
void ConstDeclInitStmt::output(int level)
{
    fprintf(yyout, "%*cConstDeclInitStmt\n", level, ' ');
    id->output(level + 4);
    initVal->output(level + 4);
}
void DeclList::output(int level)
{
    fprintf(yyout, "%*cDeclList\n", level, ' ');
    decl1->output(level + 4);
    decl2->output(level + 4);
}
void ConstDeclList::output(int level)
{
    fprintf(yyout, "%*cConstDeclList\n", level, ' ');
    decl1->output(level + 4);
    decl2->output(level + 4);
}
void WhileStmt::output(int level)
{
    //����ʵ�֣�ֻ�ǵ����ķ�������Ļ�����if��һ������
    fprintf(yyout, "%*cWhileStmt\n", level, ' ');
    cond->output(level + 4);
    if (doStmt != nullptr)
    {
        doStmt->output(level + 4);
    }
}
void BreakStmt::output(int level)
{
    if(whether_valid)
        fprintf(yyout, "%*cBreak\n", level, ' ');
    else
    {
        fprintf(stderr, "break not in loop \n");
        fprintf(yyout, "%*cerror,break not in loop\n", level, ' ');
    }
}
void ContinueStmt::output(int level)
{
    if (whether_valid)
        fprintf(yyout, "%*cContinue\n", level, ' ');
    else
    {
        fprintf(stderr, "continue not in loop \n");
        fprintf(yyout, "%*cerror,continue not in loop\n", level, ' ');
    }
}
void EmptyStmt::output(int level) 
{

}
void DoNothingStmt::output(int level)
{
    fprintf(yyout, "%*cWhileStmt\n", level, ' ');
    do_nothing_node->output(level + 4);
}



void Ast::output()
{
    fprintf(yyout, "program\n");
    if(root != nullptr)
        root->output(4);
}

void Constant::output(int level)
{
    //std::string type, value;
    //type = symbolEntry->getType()->toStr();
    //value = symbolEntry->toStr();
    //fprintf(yyout, "%*cIntegerLiteral\tvalue: %s\ttype: %s\n", level, ' ',
    //        value.c_str(), type.c_str());
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
        //�þͺ���symbolentry�����Ѿ��Ѹ�������������ת��Ϊ�ַ����ˣ������ֱ��?s�ͺ��˲����ڹ�ռλ����
        fprintf(yyout, "%*cFLOATLiteral\tvalue: %s\ttype: %s\n", level, ' ',
            value.c_str(), type.c_str());
    }
}

void Id::output(int level)
{
    /*std::string name, type;
    int scope;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    scope = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getScope();
    fprintf(yyout, "%*cId\tname: %s\tscope: %d\ttype: %s\n", level, ' ',
            name.c_str(), scope, type.c_str());*/
    
    std::string name, type;
    int scope;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    scope = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getScope();
    if (ERROR_MESSAGE_WRITE_INTO_AST)
    {
        if (define_state == NOT_DEFINED)
        {
            fprintf(yyout, "%*c\twarning,identifier %s is not defined\n", level, ' ',
                name.c_str());
        }
        else if (define_state == REDEFINATION)
        {
            fprintf(yyout, "%*c\twarning,identifier %s is redefined\n", level, ' ',
                name.c_str());
        }
    }
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
}

void CompoundStmt::output(int level)
{
    fprintf(yyout, "%*cCompoundStmt\n", level, ' ');
    stmt->output(level + 4);
}

void SeqNode::output(int level)
{
    stmt1->output(level);
    stmt2->output(level);
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

void IfElseStmt::output(int level)
{
    fprintf(yyout, "%*cIfElseStmt\n", level, ' ');
    cond->output(level + 4);
 /* thenStmt->output(level + 4);
    elseStmt->output(level + 4);*/

    if (thenStmt != nullptr)
    {
        thenStmt->output(level + 4);
    }
    if (elseStmt != nullptr)
    {
        elseStmt->output(level + 4);
    }
}

void ReturnStmt::output(int level)
{
    fprintf(yyout, "%*cReturnStmt\n", level, ' ');
    if(retValue!=nullptr)
        retValue->output(level + 4);
}

void AssignStmt::output(int level)
{
    typeCheck();
    fprintf(yyout, "%*cAssignStmt\n", level, ' ');
    lval->output(level + 4);

    //std::cout << expr->cal_expr_val() << std::endl;
    expr->output(level + 4);
}

void FunctionDef::output(int level)
{
    //std::string name, type;
    //name = se->toStr();
    //type = se->getType()->toStr();
    //fprintf(yyout, "%*cFunctionDefine function name: %s, type: %s\n", level, ' ', 
    //        name.c_str(), type.c_str());
    //stmt->output(level + 4);

    std::string name, type;
    if (se == nullptr)
    {
        fprintf(stderr, "Oops!�������\n");//��ӡ�������û�ж���?
        assert(se != nullptr);      //�׳�һ�����Դ���
    }
    name = se->toStr();
    type = se->getType()->toStr();
    fprintf(yyout, "%*cFunctionDefine function name: %s, type: %s\n", level, ' ',
        name.c_str(), type.c_str());
    if (paraStmt != nullptr)
        paraStmt->output(level + 4);
    if (stmt != nullptr)
        stmt->output(level + 4);
}
