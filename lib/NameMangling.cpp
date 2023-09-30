
#include "NameMangling.h"


namespace llfp
{

// [%@][-a-zA-Z$._][-a-zA-Z$._0-9]*
std::string getMangledName(const ImportedModule& mod, const ast::Function* function, const llvm::ArrayRef<const type::TypeInstance*> types)
{
    assert(!types.empty());
    if (function->exported)
    {
        return getExportedName(mod, function);
    }

    std::string result;
    result += mod.name();
    result += ':';
    result += function->name;
    for (auto type : types)
    {
        result += '$';
        result += type->identifier().str();
    }
    return result;
}

namespace
{

void mangleTypeVars(std::string& name, const ast::Data* data, const std::map<std::string, type::Identifier>& typeVariables)
{
    for (auto& typeVar : data->typeVariables)
    {
        auto it = typeVariables.find(typeVar);
        assert(it != typeVariables.end());
        name += '_';
        name += it->second.str();
    }
}

} // namespace

std::string getMangledName(const ImportedModule& mod, const ast::Data* data, const std::map<std::string, type::Identifier>& typeVariables)
{
    assert(data->constructors.size() == 1);
    assert(data->typeVariables.size() == typeVariables.size());
    auto result = mod.name() + '_' + data->name;
    mangleTypeVars(result, data, typeVariables);
    return result;
}

std::string getMangledName(const ImportedModule& mod, const ast::Data* data, size_t constructorIndex, const std::map<std::string, type::Identifier>& typeVariables)
{
    assert(data->constructors.size() > 1);
    assert(data->typeVariables.size() == typeVariables.size());
    auto result = mod.name() + '_' + data->name;
    mangleTypeVars(result, data, typeVariables);
    result += '_' + data->constructors.at(constructorIndex).name;
    return result;
}

std::string getMangledName(const ImportedModule& mod, const char* internalFunctionName, type::TypeInstPtr type)
{
    return '$' + mod.name() + ':' + internalFunctionName + "$" + type->identifier().str();
}

std::string getExportedName(const ImportedModule& mod, const ast::Function* function)
{
    return mod.name() + '_' + function->name;
}

bool fullyQualifiedName(const ImportedModule& mod, type::Identifier& identifier, const ast::TypeIdentifier& tid)
{
    // check primitve type
    if (tid.parameters.empty() && tid.identifier.moduleName.empty())
    {
        type::Identifier id{ tid.identifier, {} };
        if (id.name.name.empty() || type::isPrimitive(id))
        {
            identifier = std::move(id);
            return true;
        }
    }

    auto ast = mod.lookupType(tid.identifier);
    if (ast.importedModule == nullptr || ast.data == nullptr)
    {
        return false;
    }

    identifier.name = { ast.importedModule->name(), ast.data->name };
    assert(identifier.parameters.size() == 0);
    for (auto& param : tid.parameters)
    {
        identifier.parameters.push_back({});
        if (!fullyQualifiedName(mod, identifier.parameters.back(), param))
        {
            return false;
        }
    }
    return true;
}

} // namespace llfp
