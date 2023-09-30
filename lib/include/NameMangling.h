#pragma once

#include "Ast.h"
#include "Type/TypeInstance.h"

#pragma warning(push, 0)

#include <llvm/ADT/ArrayRef.h>

#pragma warning(pop)

#include <map>
#include <string>


namespace llfp
{

std::string getMangledName(const ImportedModule& mod, const ast::Function* function, const llvm::ArrayRef<const type::TypeInstance*> types);
std::string getMangledName(const ImportedModule& mod, const ast::Data* data, const std::map<std::string, type::Identifier>& typeVariables = {});
std::string getMangledName(const ImportedModule& mod, const ast::Data* data, size_t constructorIndex, const std::map<std::string, type::Identifier>& typeVariables = {});
std::string getMangledName(const ImportedModule& mod, const char* internalFunctionName, type::TypeInstPtr type);
std::string getExportedName(const ImportedModule& mod, const ast::Function* function);
bool        fullyQualifiedName(const ImportedModule& mod, type::Identifier& identifier, const ast::TypeIdentifier& tid);

} // namespace llfp
