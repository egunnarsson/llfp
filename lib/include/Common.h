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

struct GlobalIdentifierRef
{
    llvm::StringRef moduleName;
    llvm::StringRef name;

    std::string str() const
    {
        return moduleName.str() + ':' + name.str();
    }

    bool operator ==(const GlobalIdentifierRef &id) const
    {
        return moduleName == id.moduleName && name == id.name;
    }
};

struct GlobalIdentifier
{
    std::string moduleName;
    std::string name;

    std::string str() const
    {
        return moduleName + ':' + name;
    }

    operator GlobalIdentifierRef() const { return { moduleName, name }; }

    bool operator ==(const GlobalIdentifier &id) const
    {
        return moduleName == id.moduleName && name == id.name;
    }

    bool operator !=(const GlobalIdentifier &id) const
    {
        return moduleName != id.moduleName || name != id.name;
    }

    bool operator ==(GlobalIdentifierRef id) const
    {
        return moduleName == id.moduleName && name == id.name;
    }

    bool operator !=(GlobalIdentifierRef id) const
    {
        return moduleName != id.moduleName || name != id.name;
    }
};

/**
Careful with this!
*/
struct GlobalIdentifierRefHash
{
    size_t operator()(llfp::GlobalIdentifierRef x) const
    {
        auto a = llvm::hash_value(x.moduleName);
        auto b = llvm::hash_value(x.name);
        return llvm::hash_combine(a, b);
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
