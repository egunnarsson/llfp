#pragma once

#pragma warning(push, 0)

#include <llvm/ADT/StringRef.h>

#pragma warning(pop)

namespace llfp
{

struct SourceLocation
{
    int             Line;
    int             Column;
    llvm::StringRef File;
};

} // namespace llfp
