#pragma once

#pragma warning(push, 0)

#include <llvm/ADT/Hashing.h>
#include <llvm/ADT/StringRef.h>

#pragma warning(pop)

#include <functional>
#include <string>


namespace llfp
{

struct GlobalIdentifier
{
    std::string moduleName;
    std::string name;

    GlobalIdentifier() = default;
    GlobalIdentifier(std::string moduleName_, std::string name_)
        : moduleName{ std::move(moduleName_) },
          name{ std::move(name_) }
    {}

    static GlobalIdentifier split(llvm::StringRef fullName)
    {
        auto split = fullName.split(':');
        return split.second.empty() ? GlobalIdentifier{ "", fullName.str() } : GlobalIdentifier{ split.first.str(), split.second.str() };
    }

    std::string str() const
    {
        return moduleName.empty() ? name : moduleName + ':' + name;
    }

    bool operator==(const GlobalIdentifier& id) const
    {
        return moduleName == id.moduleName && name == id.name;
    }

    bool operator!=(const GlobalIdentifier& id) const
    {
        return moduleName != id.moduleName || name != id.name;
    }
};

} // namespace llfp

template<>
struct std::hash<llfp::GlobalIdentifier>
{
    std::size_t operator()(llfp::GlobalIdentifier const& id) const noexcept
    {
        return llvm::hash_combine(llvm::hash_value(id.name), llvm::hash_value(id.moduleName));
    }
};
