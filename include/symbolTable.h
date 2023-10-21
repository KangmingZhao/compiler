
#include<string>
#include<map>
using namespace std;
class symbolTableEntry
{
public:
    int value;
    int lineno; // 行号
    int offset; //列号
    symbolTableEntry(int v,int l,int o)
    {
        value=v;
        lineno=l;
        offset=o;
    }
};


class symbolTable{
public:

symbolTable(){
    prev=nullptr;
    scope=0;
}

symbolTable(symbolTable * prev){
    this->prev=prev;
    this->scope=prev->scope+1;// 作用域增大
}


//查找
symbolTableEntry* find(const string name)
{
//查找到对应标识符在的符号表里
    symbolTable *p;
    do{
        // 返回值为一符号表里的一条信息记录 
        // 通过name去匹配 不同的作用域级均有一张表 通过prev指针串联
        auto iter=p->sbt.find(name);
        if(iter!=p->sbt.end())
        {
            return p->sbt[name];
        }

    }while(p!=nullptr);
    // 没找到返回空
    return nullptr;
}
//插入
void insert(const string name,symbolTableEntry * entry)
{
    // 把这一行信息都插入到对应作用域的符号表当中
       auto iter=this->sbt.find(name);
        if(iter!=this->sbt.end())
        {
            return;
        }
    sbt[name]=entry;
}
//初始化信息
int setEntryVal(const string name,int value)
{
    symbolTableEntry* entry=this->find(name);
    if(entry==nullptr)
    {
        return 0;
    }
    else
    {
        entry->value=value;
        return 1;
    }
}

private:  
    map<string,symbolTableEntry*> sbt;
    symbolTable* prev;// 前一个
    int scope;// 作用域级
};