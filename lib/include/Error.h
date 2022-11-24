#pragma once

#include "Common/SourceLocation.h"

#include <stdexcept>


namespace llfp
{

class Error : public std::runtime_error
{
public:

    Error(const char* msg)
        : std::runtime_error(msg)
    {}
    Error(const std::string& msg)
        : std::runtime_error(msg)
    {}
};

class ErrorLocation : public std::runtime_error
{
    SourceLocation sourceLocation;

public:

    ErrorLocation(SourceLocation sourceLocation_, const char* msg)
        : std::runtime_error(msg),
          sourceLocation{ sourceLocation_ }
    {}
    ErrorLocation(SourceLocation sourceLocation_, const std::string& msg)
        : std::runtime_error(msg),
          sourceLocation{ sourceLocation_ }
    {}

    const SourceLocation& location() const
    {
        return sourceLocation;
    }
};

} // namespace llfp
