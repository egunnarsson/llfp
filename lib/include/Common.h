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

} // llfp
