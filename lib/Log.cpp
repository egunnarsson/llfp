
#include "Log.h"
#include "Source.h"

namespace llfp::detail
{

void logSourceLocation(llvm::raw_ostream& out, const SourceLocation& location)
{
    out << location.File->line(location.Line) << '\n';
    for (int i = 1; i < location.Column; ++i)
    {
        out << ' ';
    }
    out << "^\n";
    out << location.File->name() << '(' << location.Line << ',' << location.Column << "): ";
}

} // namespace llfp::detail
