
#include "Source.h"

namespace llfp
{

SourceIterator::SourceIterator(const Source* source)
    : source_{ source },
      it_{ source_->buffer_.begin() },
      location_{ 1, 0, source_ }
{}

SourceIterator::SourceIterator(const Source* source, std::string::const_iterator it)
    : source_{ source },
      it_{ it },
      location_{ -1, -1, source_ }
{}

SourceLocation SourceIterator::location() const { return location_; }
char SourceIterator::operator*() const { return *it_; }

SourceIterator& SourceIterator::operator++()
{
    if (*it_ != '\n')
    {
        ++location_.Column;
    }
    else
    {
        location_.Column = 0;
        ++location_.Line;
    }
    ++it_;
    return *this;
}

bool SourceIterator::operator==(const SourceIterator& RHS) const { return it_ == RHS.it_; }
bool SourceIterator::operator!=(const SourceIterator& RHS) const { return it_ != RHS.it_; }


Source::Source(std::string name, std::string source)
    : name_{ std::move(name) },
      buffer_{ std::move(source) }
{}

SourceIterator Source::begin() const { return SourceIterator{ this }; }
SourceIterator Source::end() const { return SourceIterator{ this, buffer_.end() }; }

const std::string& Source::name() const
{
    return name_;
}

llvm::StringRef Source::line(size_t line) const
{
    if (line == 0)
    {
        return "";
    }
    --line;

    const auto end = buffer_.end();
    auto       it  = buffer_.begin();
    while (line > 0)
    {
        it = std::find(it, end, '\n');
        if (it == end)
        {
            return "";
        }
        ++it;
        --line;
    }

    const auto it2  = std::find(it, end, '\n');
    const auto size = static_cast<size_t>(it2 - it);
    return size == 0 ? llvm::StringRef{} : llvm::StringRef{ &(*it), size };
}

} // namespace llfp