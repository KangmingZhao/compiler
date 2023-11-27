#ifndef __TYPE_H__
#define __TYPE_H__
#include <vector>
#include <string>

class Type
{
private:
    int kind;
protected:
    enum {INT, VOID, FUNC, PTR, INT_ARRAY, FLOAT, FLOAT_ARRAY};
public:
    Type(int kind) : kind(kind) {};
    virtual ~Type() {};
    virtual std::string toStr() = 0;
    bool isInt() const {return kind == INT;};
    bool isVoid() const {return kind == VOID;};
    bool isFunc() const {return kind == FUNC;};
    bool isINT_ARRAY() const { return kind == INT_ARRAY; };
    bool isFLOAT() const { return kind == FLOAT; };
    bool isFLOAT_ARRAY() const { return kind == FLOAT_ARRAY; };
};

class IntType : public Type
{
private:
    int size;
public:
    IntType(int size) : Type(Type::INT), size(size){};
    std::string toStr();
};

class INT_arrayType : public Type
{
public:
    INT_arrayType() : Type(Type::INT_ARRAY) {};
    std::string toStr();
};
class FloatType : public Type
{
private:
    int size;
public:
    FloatType(int size) : Type(Type::FLOAT), size(size) {};
    std::string toStr();
};

class FLOAT_arrayType : public Type
{
public:
    FLOAT_arrayType() : Type(Type::FLOAT_ARRAY) {};
    std::string toStr();
};

class VoidType : public Type
{
public:
    VoidType() : Type(Type::VOID){};
    std::string toStr();
};

class FunctionType : public Type
{
private:
    Type *returnType;
public:
    std::vector<Type*> paramsType;
    FunctionType(Type* returnType, std::vector<Type*> paramsType) : 
    Type(Type::FUNC), returnType(returnType), paramsType(paramsType){};
    Type* getRetType() {return returnType;};
    std::string toStr();
};

class PointerType : public Type
{
private:
    Type *valueType;
public:
    PointerType(Type* valueType) : Type(Type::PTR) {this->valueType = valueType;};
    std::string toStr();
};

class TypeSystem
{
private:
    static IntType commonInt;
    static IntType commonBool;
    static VoidType commonVoid;
    static FloatType commonFloat;
public:
    static Type *intType;
    static Type *voidType;
    static Type *boolType;

    static Type* int_arrayType;
    static Type* floatType;
};

#endif
