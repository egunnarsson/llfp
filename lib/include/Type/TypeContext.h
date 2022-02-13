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


namespace llfp
{

class ImportedModule;
class SourceModule;
class GlobalContext;

namespace type
{

// names of built in primitive types
namespace name
{

#define TypeID(id, name) constexpr llvm::StringLiteral id{name};

TypeID(Bool, "bool");

TypeID(I8, "i8");
TypeID(I16,"i16");
TypeID(I32, "i32");
TypeID(I64, "i64");
TypeID(I128, "i128");

TypeID(U8, "u8");
TypeID(U16, "u16");
TypeID(U32, "u32");
TypeID(U64, "u64");
TypeID(U128, "u128");

TypeID(Half, "half");
TypeID(Float, "float");
TypeID(Double, "double");

TypeID(Char, "char");

#undef TypeID

constexpr llvm::StringLiteral AllTypes[] {
    Bool,
    I8, I16, I32, I64, I128,
    U8, U16, U32, U64, U128,
    Half, Float, Double,
    Char
};

} // namespace name

/*
namespace typeclass
{

// Num
// Integer
// Bool
// Signed
// Floating

}

enum TypeClass
{
    Num,
    Fractional,
    Eq,
    Ord,
    Bits
};
*/

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
    TypeInstPtr constructTypeUsingAnnotationStuff(hm::TypeAnnotation& context, const ast::Exp& exp);

    TypeInstPtr getBool();
    TypeInstPtr getChar();
    TypeInstPtr getI64();
    TypeInstPtr getU64();
    TypeInstPtr getDouble();

    TypeInstPtr getType(const Identifier& identifier);
    //const TypeInstance& getType(const hm::TypePtr& type);

private:

    TypeInstance* addType(llvm::StringLiteral name, llvm::Type* llvmType, std::vector<std::string> typeClasses);
};

} // namespace type
} // namespace llfp
