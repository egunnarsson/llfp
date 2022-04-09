#pragma once

/*
1. compare, X == Y
2. call infers arguments to get function instance?
3. fieldExp infer LHS before generating it
4. letExp if var has no type, infer before generating

is it ok to fix the type in these cases?
2. No. Calling with literals might not fix to the right type
*/

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#pragma warning(push, 0)

#include "llvm/ADT/StringRef.h"
#include "llvm/IR/LLVMContext.h"

#pragma warning(pop)

#include "Ast.h"
#include "Common/GlobalIdentifier.h"
#include "Type/TypeInstance.h"
#include "String/StringConstants.h"


namespace llfp
{

class ImportedModule;
class SourceModule;
class GlobalContext;

namespace type
{

inline constexpr llvm::StringLiteral AllTypes[] {
    id::Bool,
    id::I8, id::I16, id::I32, id::I64, id::I128,
    id::U8, id::U16, id::U32, id::U64, id::U128,
    id::Half, id::Float, id::Double,
    id::Char
};

class TypeContext
{
    llvm::LLVMContext& llvmContext; // to create types
    SourceModule*      sourceModule; // to do global lookups (actually need parent), qualify names for equals function
    GlobalContext*     globalContext;

    std::unordered_map<Identifier, std::unique_ptr<TypeInstance>> types;
    std::unordered_map<const ast::Function*, std::unique_ptr<hm::TypeAnnotation>> annotations;

    TypeInstance* boolType;
    TypeInstance* i64Type;
    TypeInstance* u64Type;
    TypeInstance* doubleType;
    TypeInstance* charType;

public:

    TypeContext(llvm::LLVMContext &llvmContext_, SourceModule *sourceModule_, GlobalContext* globalContext_);

    const hm::TypeAnnotation& getAnnotation(const ImportedModule* module, const ast::Function* ast);

    TypeInstPtr getTypeFromAst(const ast::TypeIdentifier& identifier);
    TypeInstPtr getTypeFromAst(const ast::TypeIdentifier& identifier, const ImportedModule* lookupModule);
    bool equals(TypeInstPtr type, const ast::TypeIdentifier& identifier);
    bool equals(TypeInstPtr type, llvm::StringRef identifier);

    // type check, and fix if literal etc
    bool check(hm::TypeAnnotation& context, TypeInstPtr inst, const hm::TypePtr& t);
    // a bit temporary
    TypeInstPtr constructTypeUsingAnnotationStuff(hm::TypeAnnotation& context, const ast::Node& node);

    TypeInstPtr getBool();
    TypeInstPtr getChar();
    TypeInstPtr getI64();
    TypeInstPtr getU64();
    TypeInstPtr getDouble();

    TypeInstPtr getType(const Identifier& identifier);
    //const TypeInstance& getType(const hm::TypePtr& type);

private:

    TypeInstance* addType(llvm::StringLiteral name, llvm::Type* llvmType, std::initializer_list<llvm::StringRef> typeClasses);
};

} // namespace type
} // namespace llfp
