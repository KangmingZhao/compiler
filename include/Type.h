#ifndef __TYPE_H__
#define __TYPE_H__
#include <vector>
#include <string>

class Type
{
private:
    int kind;
protected:
    /*enum {INT, VOID, FUNC, PTR, INT_ARRAY, FLOAT, FLOAT_ARRAY,
        ERROR
    };*/
    //������������һ�£���ֵ�Ĵ�С�������ȼ�������ʽת��ʱ�����ȼ��͵ĻᱻתΪ���ȼ��ߵġ�
    enum {
        ERROR,
        VOID, FUNC, PTR,

        //������һ�����ᱻ��ֵ��������ʽת��ʱ���ж��ǲ�����ͬһ���������棬���������ô�ܾ���ʽת����
        //��ͬһ�������ڵ�ת��������Ǹ�ֵ��䣬��ô����Ҫ����˭����ʿת�����������ȼ��͵ı�Ϊ���ȼ��ߵġ�
        from_now_on_is_array,
        INT_ARRAY, FLOAT_ARRAY,

        //�����߶�
        from_now_on_is_single_data,
        INT,FLOAT,  
    };
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

    int getKindValue() { return kind; };
    int get_range()
    {
        if (kind < from_now_on_is_array)
            return 0;
        else if (kind < from_now_on_is_single_data)
            return 1;
        else
            return 2;
    }
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
public:
    Type* returnType;
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

class ERROR_OCUPIER : public Type
{
public:
    ERROR_OCUPIER() :Type(Type::ERROR) {};
    //���������������Щδ�����IDռλ�õ�type����֤�����ܼ���ִ����ȥ
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
