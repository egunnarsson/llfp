#pragma once

#include <stdexcept>

namespace llfp
{

class Error : public std::runtime_error
{
public:
    Error(const char* msg) : std::runtime_error(msg) {}
    Error(const std::string& msg) : std::runtime_error(msg) {}
};

}
