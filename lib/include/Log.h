#pragma once

#include "Common/SourceLocation.h"

#pragma warning(push, 0)

#include <llvm/Support/raw_ostream.h>

#pragma warning(pop)


namespace llfp
{

namespace detail
{

#if __cplusplus > 201700L
#else

inline void logRecursive() {}

template<class T, class... Args>
void logRecursive(T value, Args&&... args)
{
    llvm::errs() << value;
    logRecursive(std::forward<Args>(args)...);
}

#endif

void logSourceLocation(llvm::raw_ostream& out, const SourceLocation& location);

} // namespace detail


// Log function

template<class... Args>
void Log(llvm::raw_ostream& out, const SourceLocation& location, Args&&... args)
{
    detail::logSourceLocation(out, location);
#if __cplusplus > 201700L
    (out << ... << args) << '\n';
#else
    detail::logRecursive(std::forward<Args>(args)...);
    llvm::errs() << '\n';
#endif
}

template<class... Args>
void Log(const SourceLocation& location, Args&&... args)
{
    Log(llvm::errs(), location, std::forward<Args>(args)...);
}


} // namespace llfp
