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

template<class T>
constexpr T npos = T(-1);

template<class T>
bool checkIndex(T i)
{
    return npos<T> != i;
}

template<class T, class P>
typename std::vector<T>::size_type findIndex(const std::vector<T>& list, const P& pred)
{
    auto it = std::find_if(list.begin(), list.end(), pred);
    if (it == list.end()) { return npos<typename std::vector<T>::size_type>; }
    auto distance = std::distance(list.begin(), it);
    return static_cast<std::vector<T>::size_type>(distance);
}

} // namespace llfp
