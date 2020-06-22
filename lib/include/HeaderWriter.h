#pragma once

#pragma warning(push, 0)

#include "llvm/Support/raw_os_ostream.h"

#pragma warning(pop)

#include "Ast.h"


namespace llfp
{

class HeaderWriter
{

public:

    // HeaderGen(std::string useNamespace) // option for c++ header

    void write(llvm::raw_ostream &os, ast::Module &module);
};

} // llfp
