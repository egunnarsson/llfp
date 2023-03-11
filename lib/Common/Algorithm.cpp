
#include "Common/Algorithm.h"


namespace llfp
{

std::vector<std::string_view> str_split(const std::string& str, char c)
{
    std::vector<std::string_view> result;

    size_t begin = 0;
    size_t end   = 0;
    while ((end = str.find(c, begin)) != std::string::npos)
    {
        result.push_back(std::string_view{ &str[begin], end - begin });
        begin = end + 1;
    }
    end = str.size();
    result.push_back(std::string_view{ &str[begin], end - begin });

    return result;
}

bool str_replace(std::string& str, std::string_view from, std::string_view to)
{
    size_t start_pos = str.find(from);
    if (start_pos == std::string::npos)
        return false;
    str.replace(start_pos, from.length(), to);
    return true;
}

} // namespace llfp
