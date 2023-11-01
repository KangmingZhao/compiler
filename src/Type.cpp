#include "Type.h"
#include <sstream>
#include "Ast.h"

IntType TypeSystem::commonInt = IntType(4);
VoidType TypeSystem::commonVoid = VoidType();

Type* TypeSystem::intType = &commonInt;
Type* TypeSystem::voidType = &commonVoid;

std::string IntType::toStr()
{
    return "int";
}

std::string INT_arrayType::toStr()
{
    return "int_array";
}
//ExprNode* INT_arrayType::get_expr()
//{
//    return expr;
//}


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
