#ifndef __SYMBOLTABLE_H__
#define __SYMBOLTABLE_H__

#include <string>
#include <map>

class Type;

class SymbolEntry
{
private:
    int kind;
protected:
    enum {CONSTANT, VARIABLE, TEMPORARY};
    Type *type;

public:
    SymbolEntry(Type *type, int kind);
    virtual ~SymbolEntry() {};
    bool isConstant() const {return kind == CONSTANT;};
    bool isTemporary() const {return kind == TEMPORARY;};
    bool isVariable() const {return kind == VARIABLE;};
    /*
    CONSTANT�����ö�ٳ�Ա�������ڱ�ʾ�����������ڱ�����������ڱ�ʾ���ɸ��ĵ���ֵ��

    VARIABLE�����ö�ٳ�Ա�������ڱ�ʾ������ͨ��ָ���ǿ����ڳ����и�ֵ���޸ĵ����ݡ�

    TEMPORARY�����ö�ٳ�Ա�������ڱ�ʾ��ʱֵ����ʱ������ͨ�������м���������������;��*/
    //�����Ӧ���������ж���ident�����͵ġ�
    Type* getType() {return type;};
    virtual std::string toStr() = 0;
    // You can add any function you need here.
};


/*  
    Symbol entry for literal constant. Example:

    int a = 1;

    Compiler should create constant symbol entry for literal constant '1'.
*/
//Ҳ����˵��֥ʿ����
class ConstantSymbolEntry : public SymbolEntry
{
private:
    int value;
    float value_f;

public:
    ConstantSymbolEntry(Type *type, int value);
    ConstantSymbolEntry(Type* type, float value);
    virtual ~ConstantSymbolEntry() {};
    //int getValue() const {return value;};
    std::string toStr();
    // You can add any function you need here.
};


/* 
    Symbol entry for identifier. Example:

    int a;
    int b;
    void f(int c)
    {
        int d;
        {
            int e;
        }
    }

    Compiler should create identifier symbol entries for variables a, b, c, d and e:

    | variable | scope    |
    | a        | GLOBAL   |
    | b        | GLOBAL   |
    | c        | PARAM    |
    | d        | LOCAL    |
    | e        | LOCAL +1 |
*/
//֥ʿ��������ȫ�ֵģ����������ر�����
class IdentifierSymbolEntry : public SymbolEntry
{
private:
    enum {GLOBAL, PARAM, LOCAL};
    std::string name;
    int scope;
    // You can add any field you need here.

public:
    IdentifierSymbolEntry(Type *type, std::string name, int scope);
    virtual ~IdentifierSymbolEntry() {};
    std::string toStr();
    int getScope() const {return scope;};
    // You can add any function you need here.
};


/* 
    Symbol entry for temporary variable created by compiler. Example:

    int a;
    a = 1 + 2 + 3;

    The compiler would generate intermediate code like:

    t1 = 1 + 2
    t2 = t1 + 3
    a = t2

    So compiler should create temporary symbol entries for t1 and t2:

    | temporary variable | label |
    | t1                 | 1     |
    | t2                 | 2     |
*/
//������������������ֱ�Ӵ���1+2+3.���ǰ������ɺü������֡�
class TemporarySymbolEntry : public SymbolEntry
{
private:
    int label;
public:
    TemporarySymbolEntry(Type *type, int label);
    virtual ~TemporarySymbolEntry() {};
    std::string toStr();
    // You can add any function you need here.
};

// symbol table managing identifier symbol entries
//������֥ʿident�Ĺ����ࡣ��ά��һ��symbolTable������������ӳ�䵽�����ident�Ľ�����ű�ķ�ʽ����ʼ���ķ�ʽ����
class SymbolTable
{
private:
    std::map<std::string, SymbolEntry*> symbolTable;
    SymbolTable *prev;/*ʵ�ַ��ű��Ƕ�׽ṹ��
    ���ű��Ƕ�׼�����������ʾ��ͬ��������Scope���ġ�
    �ڱ�������У���ͬ�Ĵ���顢�����������������ṹ���γɲ�ͬ��������
    ���������ı�ʶ��ͨ�����в�ͬ�Ŀɼ��Ժ��������ڡ�
    */
    int level;//������˵����������������������ʱ���������
    static int counter;//ͨ����������Ψһ�ı�ǩ�����ơ���һ����+1����ܺ���

    bool is_loop_block = 0;
public:
    SymbolTable();
    SymbolTable(SymbolTable* prev);

    void i_m_loop() { is_loop_block = 1; }
    bool is_loop() { return is_loop_block; }

    void install(std::string name, SymbolEntry* entry);
    SymbolEntry* lookup(std::string name);
    SymbolTable* getPrev() {return prev;};
    int getLevel() {return level;};
    static int getLabel() {return counter++;};
};

extern SymbolTable *identifiers;
extern SymbolTable *globals;

#endif
