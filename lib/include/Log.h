#pragma once

#include "Common/SourceLocation.h"

#pragma warning(push, 0)

#include <llvm/Support/raw_ostream.h>

#pragma warning(pop)


namespace llfp
{

namespace detail
{

void logSourceLocation(llvm::raw_ostream& out, const SourceLocation& location);

} // namespace detail

template<class... Args>
void Log(llvm::raw_ostream& out, const SourceLocation& location, Args&&... args)
{
    detail::logSourceLocation(out, location);
    (out << ... << args) << '\n';
}

template<class... Args>
void Log(const SourceLocation& location, Args&&... args)
{
    Log(llvm::errs(), location, std::forward<Args>(args)...);
}

} // namespace llfp
