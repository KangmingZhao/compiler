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
    CONSTANT：这个枚举成员可能用于表示常量，例如在编程语言中用于表示不可更改的数值。

    VARIABLE：这个枚举成员可能用于表示变量，通常指的是可以在程序中赋值和修改的数据。

    TEMPORARY：这个枚举成员可能用于表示临时值或临时变量，通常用于中间计算或其他短暂用途。*/
    //这个类应该是用来判断这ident的类型的。
    Type* getType() {return type;};
    virtual std::string toStr() = 0;
    // You can add any function you need here.
};


/*  
    Symbol entry for literal constant. Example:

    int a = 1;

    Compiler should create constant symbol entry for literal constant '1'.
*/
//也就是说，芝士常量
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
//芝士变量，有全局的，参数，本地变量。
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
//看起来编译器并不是直接处理1+2+3.它是把这个拆成好几个部分。
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
//看起来芝士ident的管理类。它维护一个symbolTable，可以由名字映射到具体的ident的进入符号表的方式（初始化的方式）。
class SymbolTable
{
private:
    std::map<std::string, SymbolEntry*> symbolTable;
    SymbolTable *prev;/*实现符号表的嵌套结构。
    符号表的嵌套级别是用来表示不同的作用域（Scope）的。
    在编程语言中，不同的代码块、函数、类或其他程序结构会形成不同的作用域，
    其中声明的标识符通常具有不同的可见性和生命周期。
    */
    int level;//具体来说，这个东西的作用是在输出时搞好缩进！
    static int counter;//通常用于生成唯一的标签或名称。来一个就+1，这很合理。

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
