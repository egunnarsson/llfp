#pragma once

#include "Module.h"

#pragma warning(push, 0)

#include <llvm/Support/raw_os_ostream.h>

#pragma warning(pop)


namespace llfp
{

class HeaderWriter
{

public:

    // HeaderGen(std::string useNamespace) // option for c++ header

    void write(llvm::raw_ostream& os, llfp::SourceModule& module);
};

} // namespace llfp
