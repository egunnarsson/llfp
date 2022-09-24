#pragma once

#include <vector>

#include "IModule.h"
#include "Type/TypeInstance.h"


namespace llfp
{

struct FunctionIdentifier
{
    FunAst ast;
    std::vector<type::TypeInstPtr>* types;
};

class Driver
{
    std::vector<FunctionIdentifier> pendingGeneration;
    std::vector<type::TypeInstPtr>  pendingTypeGenerations;

public:

    bool               empty() const;
    void               push(FunctionIdentifier function);
    void               push(type::TypeInstPtr type);
    FunctionIdentifier pop();
    type::TypeInstPtr  popType();

};

} // namespace llfp
