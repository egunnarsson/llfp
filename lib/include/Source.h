#pragma once

#include "Common/SourceLocation.h"

#pragma warning(push, 0)

#include <llvm/ADT/StringRef.h>

#pragma warning(pop)

#include <string>


namespace llfp
{

class Source;

class SourceIterator
{
    const Source*               source_;
    std::string::const_iterator it_;
    SourceLocation              location_;

public:

    SourceIterator(const Source* source);
    SourceIterator(const Source* source, std::string::const_iterator it);

    SourceLocation location() const;

    char            operator*() const;
    SourceIterator& operator++();
    bool            operator==(const SourceIterator& RHS) const;
    bool            operator!=(const SourceIterator& RHS) const;
};

class Source
{
    std::string name_;
    std::string buffer_;

    friend SourceIterator;

public:

    Source(std::string name, std::string source);

    SourceIterator begin() const;
    SourceIterator end() const;

    const std::string& name() const;
    llvm::StringRef    line(size_t index) const;
};

} // namespace llfp