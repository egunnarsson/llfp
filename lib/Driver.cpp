
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

FunctionIdentifier Driver::pop()
{
    assert(!pendingGeneration.empty());
    auto function = pendingGeneration.back();
    pendingGeneration.pop_back();
    return function;
}

}
