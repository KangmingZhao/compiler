#ifndef __SYMBOLTABLE_H__
#define __SYMBOLTABLE_H__

#include <string>
#include <map>

class ExprNode;
class Type;
class Operand;

class SymbolEntry
{
private:
    int kind;
protected:
    enum {CONSTANT, VARIABLE, TEMPORARY, CONSTIDENTIFER};
    Type *type;

    int arr_dimension_recorder = 0;

    //-1 means it do not use 
    int use_r0_r3 = -1;
    bool is_return_val = 0;


public:
    void set_use_r0_r3(int use_r0_r3) { this->use_r0_r3 = use_r0_r3; };
    int get_use_r0_r3() { return use_r0_r3; };
    bool is_return() { return is_return_val; };
    void set_return() { is_return_val = 1; };

    SymbolEntry(Type *type, int kind);
    virtual ~SymbolEntry() {};
    bool isConstant() const {return kind == CONSTANT;};
    bool isTemporary() const {return kind == TEMPORARY;};
    bool isVariable() const {return kind == VARIABLE;};
    bool isConstIdentifer() const { return kind == CONSTIDENTIFER; };
    Type* getType() {return type;};
    void changeType(Type* new_type);
    
    void setType(Type *type) {this->type = type;};
    virtual std::string toStr() = 0;
    // You can add any function you need here.


    void update_arr_dimension_recorder(int n_arr_dimension_recorder) { arr_dimension_recorder = n_arr_dimension_recorder; };
    int get_arr_dimension_recorder() { return arr_dimension_recorder; };
};


/*  
    Symbol entry for literal constant. Example:

    int a = 1;

    Compiler should create constant symbol entry for literal constant '1'.
*/
class ConstantSymbolEntry : public SymbolEntry
{
private:
    int value;
    float value_f;
    int data_type;


public:
    ConstantSymbolEntry(Type *type, int value);
    ConstantSymbolEntry(Type* type, float value);
    virtual ~ConstantSymbolEntry() {};
    double getValue() const {
        if(data_type == 1)
            return (double)value;
        else 
            return (double)value_f;
    };
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
class IdentifierSymbolEntry : public SymbolEntry
{
private:
    enum {GLOBAL, PARAM, LOCAL};
    std::string name;
    int scope;
    Operand *addr;  // The address of the identifier.
    // You can add any field you need here.

    ExprNode* valueExpr;

public:
    IdentifierSymbolEntry(Type *type, std::string name, int scope);
    IdentifierSymbolEntry(Type* type, std::string name, int scope, ExprNode* valueExpr);
    virtual ~IdentifierSymbolEntry() {};
    std::string toStr();
    bool isGlobal() const {return scope == GLOBAL;};
    bool isParam() const {return scope == PARAM;};
    bool isLocal() const {return scope >= LOCAL;};
    int getScope() const {return scope;};
    void setAddr(Operand *addr) {this->addr = addr;};
    Operand* getAddr() {return addr;};    

    // You can add any function you need here.
    void setFuncType(Type* t) { type = t; }
    ExprNode* getValueExpr() { return valueExpr; };
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
class TemporarySymbolEntry : public SymbolEntry
{
private:
  int stack_offset;
    int label;
public:
    TemporarySymbolEntry(Type *type, int label);
    virtual ~TemporarySymbolEntry() {};
    std::string toStr();
    int getLabel() const {return label;};
    void setOffset(int offset) { this->stack_offset = offset; };
    int getOffset() { return this->stack_offset; };
    // You can add any function you need here.
};

// symbol table managing identifier symbol entries
class SymbolTable
{
private:
    std::map<std::string, SymbolEntry*> symbolTable;
    SymbolTable *prev;
    int level;
    static int counter;

public:
    SymbolTable();
    SymbolTable(SymbolTable *prev);
    void install(std::string name, SymbolEntry* entry);
    SymbolEntry* lookup(std::string name);

    SymbolEntry* lookup_in_present_domain(std::string name);


    SymbolTable* getPrev() {return prev;};
    int getLevel() {return level;};
    static int getLabel() {return counter++;};

};

extern SymbolTable *identifiers;
extern SymbolTable *globals;

#endif
