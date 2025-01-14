#include "Operand.h"
#include <sstream>
#include <algorithm>
#include <string.h>
#include "Type.h"

std::string Operand::toStr() const
{
    return se->toStr();
}

void Operand::removeUse(Instruction *inst)
{
    auto i = std::find(uses.begin(), uses.end(), inst);
    if(i != uses.end())
        uses.erase(i);
}

void Operand::change_funct_type() {
    if (se->getType()->isFunc()) {
        se->setType(((FunctionType*)se->getType())->returnType);
    }
}
void Operand::set_bool()
{
    se->setType(TypeSystem::boolType);
}