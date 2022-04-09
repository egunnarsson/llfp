
#include <algorithm>
#include <iterator>
#include <utility>

#pragma warning(push, 0)

#include "llvm/Support/FormatVariadic.h"

#pragma warning(pop)

#include "Common/Algorithm.h"
#include "Error.h"
#include "GlobalContext.h"
#include "Log.h"
#include "Module.h"

#include "Type/TypeContext.h"


namespace llfp
{
namespace type
{

namespace
{

// assumes basic type
class NaiveTypeConstructor : public hm::TypeVisitor
{
public:

    TypeContext* context;
    TypeInstPtr result = nullptr;

    NaiveTypeConstructor(TypeContext* context_) : context{ context_ } {}

    void visit(hm::TypeVar& t) override
    {
        //assert(t.fields.empty());

        if (std::any_of(t.typeClasses.begin(), t.typeClasses.end(), [](const std::string& s) { return s == id::Floating; }))
        {
            result = context->getDouble();
        }
        else if (std::any_of(t.typeClasses.begin(), t.typeClasses.end(), [](const std::string& s) { return s == id::Signed; }))
        {
            result = context->getI64();
        }
        else
        {
            result = context->getU64();
        }
    }

    void visit(hm::TypeConstant& t) override
    {
        auto id = GlobalIdentifier::split(t.id);
        assert(!id.moduleName.empty() || t.fields.empty());
        result = context->getType(type::Identifier{ std::move(id), {} });
    }

    void visit(hm::FunctionType&) override
    {
    }
};

} // namespace


#define LLVM_TYPE(func) llvm::Type::get##func##Ty(llvmContext)

TypeContext::TypeContext(llvm::LLVMContext& llvmContext_, SourceModule* sourceModule_, GlobalContext* globalContext_) :
    llvmContext{ llvmContext_ },
    sourceModule{ sourceModule_ },
    globalContext{ globalContext_ }
{
    boolType      = addType(id::Bool,    LLVM_TYPE(Int1), { id::Eq });

    /*i8Type    =*/ addType(id::I8,      LLVM_TYPE(Int8),   { id::Eq, id::Ord, id::Num, id::Integer, id::Signed });
    /*i16Type   =*/ addType(id::I16,     LLVM_TYPE(Int16),  { id::Eq, id::Ord, id::Num, id::Integer, id::Signed });
    /*i32Type   =*/ addType(id::I32,     LLVM_TYPE(Int32),  { id::Eq, id::Ord, id::Num, id::Integer, id::Signed });
    i64Type       = addType(id::I64,     LLVM_TYPE(Int64),  { id::Eq, id::Ord, id::Num, id::Integer, id::Signed });
    /*i128Type  =*/ addType(id::I128,    LLVM_TYPE(Int128), { id::Eq, id::Ord, id::Num, id::Integer, id::Signed });

    /*u8Type    =*/ addType(id::U8,      LLVM_TYPE(Int8),   { id::Eq, id::Ord, id::Num, id::Integer });
    /*u16Type   =*/ addType(id::U16,     LLVM_TYPE(Int16),  { id::Eq, id::Ord, id::Num, id::Integer });
    /*u32Type   =*/ addType(id::U32,     LLVM_TYPE(Int32),  { id::Eq, id::Ord, id::Num, id::Integer });
    u64Type       = addType(id::U64,     LLVM_TYPE(Int64),  { id::Eq, id::Ord, id::Num, id::Integer });
    /*u128Type  =*/ addType(id::U128,    LLVM_TYPE(Int128), { id::Eq, id::Ord, id::Num, id::Integer });

    /*halfType  =*/ addType(id::Half,   LLVM_TYPE(Half),    { id::Eq, id::Ord, id::Num, id::Floating, id::Signed });
    /*floatType =*/ addType(id::Float,  LLVM_TYPE(Float),   { id::Eq, id::Ord, id::Num, id::Floating, id::Signed });
    doubleType    = addType(id::Double, LLVM_TYPE(Double),  { id::Eq, id::Ord, id::Num, id::Floating, id::Signed });

    charType      = addType(id::Char,   LLVM_TYPE(Int8), { id::Eq, id::Ord });
}

#undef LLVM_TYPE

const hm::TypeAnnotation& TypeContext::getAnnotation(const ImportedModule* module, const ast::Function* ast)
{
    auto it = annotations.find(ast);
    if (it == annotations.end())
    {
        auto ptr = std::make_unique<hm::TypeAnnotation>(llfp::hm::inferType(module->name(), *ast));
        auto it2 = annotations.insert({ ast, std::move(ptr) });
        return *it2.first->second;
    }
    return *it->second;
}

// will throw on error, return not needed?
bool TypeContext::check(hm::TypeAnnotation& context, TypeInstPtr typeInstance, const hm::TypePtr& t)
{
    auto t2 = typeInstance->getType();
    auto subs = hm::TypeUnifier::unify(t, t2); //TODO: order? always keep t2
    for (auto& sub : subs)
    {
        context.substitute(sub);
        //TODO;// also sub in subs!!!
        // TODO; use some function in hm namespace
    }
    return true;
}

TypeInstPtr TypeContext::constructTypeUsingAnnotationStuff(hm::TypeAnnotation& context, const ast::Node& node)
{
    auto type = context.get(&node);

    NaiveTypeConstructor visitor{ this };
    type->accept(&visitor);

    if (visitor.result == nullptr)
    {
        throw Error("NaiveTypeConstructor is bad");
    }

    check(context, visitor.result, type);

    return visitor.result;
}

TypeInstance* TypeContext::addType(llvm::StringLiteral name, llvm::Type* llvmType, std::initializer_list<llvm::StringRef> typeClassesRef)
{
    std::vector<std::string> typeClasses;
    for (const auto ref : typeClassesRef) { typeClasses.push_back(ref.str()); }
    auto it = types.insert({
        Identifier{ {"", name.str()}, {} },
        std::make_unique<TypeInstanceBasic>(name, llvmType, std::move(typeClasses)) });
    return it.first->second.get();
}

/**
Does a lookup of type based on the imports of the sourceModule of the TypeContext. Fully qualifies the type and makes sure all types
are visible from the sourceModule before calling getType.
*/
TypeInstPtr TypeContext::getTypeFromAst(const ast::TypeIdentifier& identifier)
{
    return getTypeFromAst(identifier, sourceModule);
}

TypeInstPtr TypeContext::getTypeFromAst(const ast::TypeIdentifier& identifier, const ImportedModule* lookupModule)
{
    Identifier id;
    if (lookupModule->fullyQualifiedName(id, identifier))
    {
        return getType(id);
    }
    throw Error(std::string{ "failed to fully qualify name: " } + identifier.str());
}

/**
Get or create a type. The identifier is assumed to be fully qualified and of a concrete type.
*/
TypeInstPtr TypeContext::getType(const Identifier& identifier)
{
    {
        auto it = types.find(identifier);
        if (it != types.end())
        {
            return it->second.get();
        }
    }

    auto ast = globalContext->lookupTypeGlobal(identifier.name);
    if (ast.data != nullptr)
    {
        if (ast.data->typeVariables.size() != identifier.parameters.size())
        {
            throw Error(std::string{ "type arity mismatch between " } + identifier.str() + " and " + ast.importedModule->name() + ':' + ast.data->name + '/' + std::to_string(ast.data->typeVariables.size()));
        }

        std::vector<std::string> typeClasses; // TODO!!!

        auto llvmType = llvm::StructType::create(llvmContext, "");
        auto tmpPtr = std::make_unique<TypeInstanceStruct>(identifier, ast.importedModule, ast.data, llvmType, typeClasses);
        auto typePtr = tmpPtr.get();
        auto it2 = types.insert({ identifier, std::move(tmpPtr) });
        assert(it2.second);

        // generate body

        std::map<std::string, Identifier> typeVariables; // I think we need to build this
        for (int i = 0; i < ast.data->typeVariables.size(); ++i)
        {
            typeVariables[ast.data->typeVariables[i]] = identifier.parameters[i];
        }

        std::vector<const TypeInstance*> fieldTypes;
        for (auto& field : ast.data->fields)
        {
            const TypeInstance* fieldType = nullptr;
            if (field.type.identifier.moduleName.empty())
            {
                auto it = typeVariables.find(field.type.identifier.name);
                if (it != typeVariables.end())
                {
                    fieldType = getType(it->second);
                }
            }
            if (fieldType == nullptr)
            {
                fieldType = getTypeFromAst(field.type, ast.importedModule);
            }

            if (fieldType == nullptr)
            {
                Log(field.location, "unknown type: ", field.type.identifier.str());
                throw Error("");
            }
            fieldTypes.push_back(std::move(fieldType));
        }

        llvmType->setName(ast.importedModule->getMangledName(ast.data, fieldTypes));
        typePtr->setFields(std::move(fieldTypes));

        return typePtr;
    }

    throw Error(std::string{ "unknown data " } + identifier.name.str());
}

bool TypeContext::equals(TypeInstPtr type, const ast::TypeIdentifier& identifier)
{
    Identifier id;
    if (sourceModule->fullyQualifiedName(id, identifier))
    {
        return type->identifier() == id;
    }
    return false;
}

bool TypeContext::equals(TypeInstPtr type, llvm::StringRef identifier)
{
    return checkBasicType(type->identifier(), identifier);
}

TypeInstPtr TypeContext::getBool() { return boolType; }
TypeInstPtr TypeContext::getChar() { return charType; }
TypeInstPtr TypeContext::getI64() { return i64Type; }
TypeInstPtr TypeContext::getU64() { return u64Type; }
TypeInstPtr TypeContext::getDouble() { return doubleType; }

} // namespace Type
} // namespace llfp
