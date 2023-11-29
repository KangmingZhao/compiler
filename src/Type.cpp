#include "Type.h"
#include <sstream>
#include "Ast.h"


IntType TypeSystem::commonInt = IntType(32);
IntType TypeSystem::commonBool = IntType(1);
VoidType TypeSystem::commonVoid = VoidType();
FloatType TypeSystem::commonFloat = FloatType(32);

Type* TypeSystem::intType = &commonInt;
Type* TypeSystem::voidType = &commonVoid;
Type* TypeSystem::boolType = &commonBool;
Type* TypeSystem::floatType = &commonFloat;



std::string ERROR_OCUPIER::toStr()
{
    std::ostringstream buffer;
    buffer << "Oops!Something Wrong";
    return buffer.str();
}


std::string IntType::toStr()
{
    std::ostringstream buffer;
    buffer << "i" << size;
    return buffer.str();
}

std::string INT_arrayType::toStr()
{
    return "int_array";
}

std::string FloatType::toStr()
{
    std::ostringstream buffer;
    buffer << "f" << size;
    return buffer.str();
}
std::string FLOAT_arrayType::toStr()
{
    return "float_array";
}

std::string VoidType::toStr()
{
    return "void";
}

std::string FunctionType::toStr()
{
    std::ostringstream buffer;
    buffer << returnType->toStr() << "()";
    return buffer.str();
}

std::string PointerType::toStr()
{
    std::ostringstream buffer;
    buffer << valueType->toStr() << "*";
    return buffer.str();
}
