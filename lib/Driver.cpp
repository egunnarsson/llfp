
#include "Driver.h"

namespace llfp
{

bool Driver::empty() const
{
    return pendingGeneration.empty();
}

void Driver::push(FunctionIdentifier function)
{
    pendingGeneration.push_back(function);
}

void Driver::push(type::TypeInstPtr type)
{
    pendingTypeGenerations.push_back(type);
}

FunctionIdentifier Driver::pop()
{
    assert(!pendingGeneration.empty());
    auto function = pendingGeneration.back();
    pendingGeneration.pop_back();
    return function;
}

type::TypeInstPtr Driver::popType()
{
    if (pendingTypeGenerations.empty())
    {
        return nullptr;
    }
    auto type = pendingTypeGenerations.back();
    pendingTypeGenerations.pop_back();
    return type;
}

} // namespace llfp
