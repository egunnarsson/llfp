#pragma once

#include <string>
#include <unordered_map>

#pragma warning(push, 0)

#include <llvm/ADT/Hashing.h>
#include <llvm/ADT/StringRef.h>

#pragma warning(pop)

namespace llfp
{

struct SourceLocation
{
    int             Line;
    int             Column;
    llvm::StringRef File;
};

struct GlobalIdentifier
{
    std::string moduleName;
    std::string name;

    GlobalIdentifier() = default;
    GlobalIdentifier(std::string moduleName_, std::string name_) :
        moduleName{ std::move(moduleName_) },
        name{ std::move(name_) }
    {}

    static GlobalIdentifier split(const std::string& fullName)
    {
        auto split = llvm::StringRef{ fullName }.split(':');
        return split.second.empty() ?
            GlobalIdentifier{ "", fullName }:
            GlobalIdentifier{ split.first.str(), split.second.str() };
    }

    std::string str() const
    {
        return moduleName.empty() ? name : moduleName + ':' + name;
    }

    bool operator ==(const GlobalIdentifier &id) const
    {
        return moduleName == id.moduleName && name == id.name;
    }

    bool operator !=(const GlobalIdentifier &id) const
    {
        return moduleName != id.moduleName || name != id.name;
    }
};

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


namespace std
{

template<> struct hash<llfp::GlobalIdentifier>
{
    std::size_t operator()(llfp::GlobalIdentifier const& id) const noexcept
    {
        return llvm::hash_combine(llvm::hash_value(id.name), llvm::hash_value(id.moduleName));
    }
};

} // std
