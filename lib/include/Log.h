#pragma once

#pragma warning(push, 0)

#include <llvm/Support/raw_ostream.h>

#pragma warning(pop)

#include "Common.h"


namespace llfp
{

#if __cplusplus > 201700L
#else

namespace detail
{

inline void logRecursive() {}

template<class T, class... Args>
void logRecursive(T value, Args&&... args)
{
    llvm::errs() << value;
    logRecursive(std::forward<Args>(args)...);
}

} // detail

#endif

// Log function

template<class... Args>
void Log(const SourceLocation &location, Args&&... args)
{
    llvm::errs() << location.File << '(' << location.Line << ',' << location.Column << "): ";
#if __cplusplus > 201700L
    (llvm::errs() << ... << args) << '\n';
#else
    detail::logRecursive(std::forward<Args>(args)...);
    llvm::errs() << '\n';
#endif
}

} // llfp
