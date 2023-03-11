#pragma once

#include <algorithm>
#include <limits>
#include <string>
#include <unordered_map>
#include <vector>


namespace llfp
{

template<class T>
T find(const std::unordered_map<std::string, T>& map, const std::string& key, T defaultValue = nullptr)
{
    auto it = map.find(key);
    if (it == map.end())
    {
        return defaultValue;
    }
    return it->second;
}

template<class T, class U>
bool contains(const std::vector<T>& list, const U& value)
{
    return std::any_of(list.begin(), list.end(), [&value](const T& item) { return item == value; });
}

constexpr auto npos = static_cast<size_t>(-1);

inline bool checkIndex(size_t i)
{
    return npos != i;
}

template<class T, class P>
size_t findIndex(const std::vector<T>& list, const P& pred)
{
    auto it = std::find_if(list.begin(), list.end(), pred);
    if (it == list.end()) { return npos; }
    auto distance = std::distance(list.begin(), it);
    return static_cast<size_t>(distance);
}

template<class T, class P>
bool erase_first_of(std::vector<T>& list, const P& pred)
{
    auto it = std::find_if(list.begin(), list.end(), pred);
    if (it != list.end())
    {
        if (it != list.end() - 1)
        {
            *it = std::move(list.back());
        }
        list.pop_back();
        return true;
    }
    return false;
}

std::vector<std::string_view> str_split(const std::string& str, char c);
bool                          str_replace(std::string& str, std::string_view from, std::string_view to);

} // namespace llfp
