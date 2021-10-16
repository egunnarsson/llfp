#pragma once

#include <vector>

#include "IModule.h"
#include "Type.h"


namespace llfp
{

struct FunctionIdentifier
{
    FunAst ast;
    std::vector<type::TypePtr>* types;
};

class Driver
{
    std::vector<FunctionIdentifier> pendingGeneration;

public:

    bool               empty() const;
    void               push(FunctionIdentifier function);
    FunctionIdentifier pop();

};

} // namespace llfp
