#pragma once

namespace llfp
{

class Source;

struct SourceLocation
{
    int           Line;
    int           Column;
    const Source* File;
};

} // namespace llfp
