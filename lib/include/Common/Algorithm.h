#pragma once

#include <algorithm>
#include <string>
#include <unordered_map>
#include <vector>


namespace llfp
{

template<class T>
T find(const std::unordered_map<std::string, T> &map, const std::string &key, T defaultValue = nullptr)
{
    auto it = map.find(key);
    if (it == map.end())
    {
        return defaultValue;
    }
    return it->second;
}

template<class T, class U>
bool contains(const std::vector<T> &list, const U& value)
{
    return std::any_of(list.begin(), list.end(), [&value](const T& item) { return item == value; });
}

} // llfp
