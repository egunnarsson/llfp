
#include "Type/TypeContext.h"

#include "Common/Algorithm.h"
#include "Error.h"
#include "GlobalContext.h"
#include "Log.h"
#include "Module.h"
#include "NameMangling.h"

#pragma warning(push, 0)

#include <llvm/Support/FormatVariadic.h>

#pragma warning(pop)

#include <algorithm>
#include <iterator>
#include <utility>


namespace llfp::type
{

namespace
{

class TypeIdentifierBuilder : public hm::TypeVisitor
{
public:

    type::Identifier result;

    void visit(hm::UnboundTypeVar&) override
    {
        throw Error("TypeIdentifierBuilder type parameter was not a type constant");
    }

    void visit(hm::BoundTypeVar& t) override
    {
        std::vector<Identifier> parameters;
        for (auto& param : t.parameters_)
        {
            TypeIdentifierBuilder visitor;
            param->accept(&visitor);
            assert(!visitor.result.name.name.empty()); // should throw if fail
            parameters.push_back(std::move(visitor.result));
        }
        assert(!t.ast_.empty());
        result = type::Identifier{ { t.ast_.importedModule->name(), t.ast_.data->name }, std::move(parameters) };
    }

    void visit(hm::TypeConstant& t) override
    {
        const auto              constantId      = llvm::StringRef{ t.id_ };
        const auto              firstBraceIndex = constantId.find_first_of('[');
        auto                    id              = GlobalIdentifier::split(constantId.substr(0, firstBraceIndex));
        std::vector<Identifier> parameters;
        for (auto& param : t.parameters_)
        {
            TypeIdentifierBuilder visitor;
            param->accept(&visitor);
            assert(!visitor.result.name.name.empty());
            parameters.push_back(std::move(visitor.result));
        }
        result = type::Identifier{ std::move(id), std::move(parameters) };
    }

    void visit(hm::FunctionType&) override
    {
        throw Error("TypeIdentifierBuilder type parameter was not a type constant");
    }
};

// assumes basic type
class NaiveTypeConstructor : public hm::TypeVisitor
{
public:

    TypeContext* context;
    TypeInstPtr  result = nullptr;

    NaiveTypeConstructor(TypeContext* context_)
        : context{ context_ }
    {}

    void visit(hm::UnboundTypeVar& t) override
    {
        if (t.constructors_.empty())
        {
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
        else
        {
            if (t.constructors_.empty())
            {
                // lookup type based on fields?
            }
            else
            {
                result = context->getTypeFromConstructor(*t.constructors_.begin());
            }
        }
    }

    void visit(hm::BoundTypeVar& t) override
    {
        /* if (std::all_of(t.parameters_ are const))
        {

        }*/
        TypeIdentifierBuilder visitor;
        t.accept(&visitor);
        assert(!visitor.result.name.moduleName.empty() || t.ast_.empty()); // either have module name, or be basic type without fields
        result = context->getType(std::move(visitor.result));
    }

    void visit(hm::TypeConstant& t) override
    {
        TypeIdentifierBuilder visitor;
        t.accept(&visitor);
        assert(!visitor.result.name.moduleName.empty() || t.ast_.empty()); // either have module name, or be basic type without fields
        result = context->getType(std::move(visitor.result));
    }

    void visit(hm::FunctionType&) override
    {
    }
};

} // namespace


#define LLVM_TYPE(func) llvm::Type::get##func##Ty(llvmContext)

TypeContext::TypeContext(llvm::LLVMContext& llvmContext_, SourceModule* sourceModule_, GlobalContext* globalContext_)
    : llvmContext{ llvmContext_ },
      sourceModule{ sourceModule_ },
      globalContext{ globalContext_ }
{
    boolType = addType(id::Bool, LLVM_TYPE(Int1), { id::Eq });

    /*i8Type    =*/addType(id::I8, LLVM_TYPE(Int8), { id::Eq, id::Ord, id::Num, id::Integer, id::Signed });
    /*i16Type   =*/addType(id::I16, LLVM_TYPE(Int16), { id::Eq, id::Ord, id::Num, id::Integer, id::Signed });
    /*i32Type   =*/addType(id::I32, LLVM_TYPE(Int32), { id::Eq, id::Ord, id::Num, id::Integer, id::Signed });
    i64Type = addType(id::I64, LLVM_TYPE(Int64), { id::Eq, id::Ord, id::Num, id::Integer, id::Signed });
    /*i128Type  =*/addType(id::I128, LLVM_TYPE(Int128), { id::Eq, id::Ord, id::Num, id::Integer, id::Signed });

    /*u8Type    =*/addType(id::U8, LLVM_TYPE(Int8), { id::Eq, id::Ord, id::Num, id::Integer });
    /*u16Type   =*/addType(id::U16, LLVM_TYPE(Int16), { id::Eq, id::Ord, id::Num, id::Integer });
    /*u32Type   =*/addType(id::U32, LLVM_TYPE(Int32), { id::Eq, id::Ord, id::Num, id::Integer });
    u64Type = addType(id::U64, LLVM_TYPE(Int64), { id::Eq, id::Ord, id::Num, id::Integer });
    /*u128Type  =*/addType(id::U128, LLVM_TYPE(Int128), { id::Eq, id::Ord, id::Num, id::Integer });

    /*halfType  =*/addType(id::Half, LLVM_TYPE(Half), { id::Eq, id::Ord, id::Num, id::Floating, id::Signed });
    /*floatType =*/addType(id::Float, LLVM_TYPE(Float), { id::Eq, id::Ord, id::Num, id::Floating, id::Signed });
    doubleType = addType(id::Double, LLVM_TYPE(Double), { id::Eq, id::Ord, id::Num, id::Floating, id::Signed });

    charType = addType(id::Char, LLVM_TYPE(Int8), { id::Eq, id::Ord });
}

#undef LLVM_TYPE

const hm::TypeAnnotation& TypeContext::getAnnotation(const ImportedModule* mod, const ast::Function* ast)
{
    auto it = annotations.find(ast);
    if (it != annotations.end())
    {
        return *it->second;
    }
    auto ptr = std::make_unique<hm::TypeAnnotation>(llfp::hm::inferType(mod, *ast));
    auto it2 = annotations.insert({ ast, std::move(ptr) });
    return *it2.first->second;
}

const hm::FunTypePtr& TypeContext::getAnnotation(const ImportedModule* mod, const ast::Class* class_, const ast::FunctionDeclaration* ast)
{
    auto it = annotationsDecls.find(ast);
    if (it != annotationsDecls.end())
    {
        return it->second;
    }
    auto it2 = annotationsDecls.insert({ ast, hm::inferType(mod, *class_, *ast) });
    return it2.first->second;
}

// will throw on error, return not needed?
bool TypeContext::check(hm::TypeAnnotation& context, TypeInstPtr typeInstance, const hm::TypePtr& t)
{
    auto t2   = typeInstance->getType();
    auto subs = hm::TypeUnifier::unify(t, t2); // TODO: order? always keep t2
    for (auto& sub : subs)
    {
        context.substitute(sub);
        // TODO;// also sub in subs!!!
        //  TODO; use some function in hm namespace
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
    auto it = types.insert({ Identifier{ { "", name.str() }, {} },
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
    if (fullyQualifiedName(*lookupModule, id, identifier))
    {
        return getType(id);
    }
    throw Error(std::string{ "failed to fully qualify name: " } + identifier.str());
}

namespace
{

void replaceTypeVars(ast::TypeIdentifier& id, const std::map<std::string, Identifier>& typeVariables)
{
    if (id.identifier.moduleName.empty())
    {
        auto it = typeVariables.find(id.identifier.name);
        if (it != typeVariables.end())
        {
            id.identifier = it->second.name;
        }
    }
    for (auto& param : id.parameters)
    {
        replaceTypeVars(param, typeVariables);
    }
}

} // namespace

TypeInstPtr TypeContext::getTypeFromAst(const ast::TypeIdentifier& identifier, const ImportedModule* lookupModule, const std::map<std::string, Identifier>& typeVariables)
{
    if (typeVariables.empty())
    {
        return getTypeFromAst(identifier, lookupModule);
    }
    else
    {
        ast::TypeIdentifier idCopy = identifier;
        replaceTypeVars(idCopy, typeVariables);
        return getTypeFromAst(idCopy, lookupModule);
    }
}

std::vector<TypeInstPtr> TypeContext::getFieldTypes(llfp::DataAst ast, const std::vector<llfp::ast::Field>& fields, const std::map<std::string, Identifier>& typeVariables)
{
    std::vector<TypeInstPtr> result;
    for (auto& field : fields)
    {
        TypeInstPtr fieldType = nullptr;
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
            fieldType = getTypeFromAst(field.type, ast.importedModule, typeVariables);
        }

        if (fieldType == nullptr)
        {
            Log(field.location, "unknown type: ", field.type.identifier.str());
            throw Error("");
        }
        result.push_back(std::move(fieldType));
    }
    return result;
}

TypeInstPtr TypeContext::makeTypeInstanceAggregate(const Identifier& identifier, llfp::DataAst ast, std::vector<std::string> typeClasses, const std::map<std::string, Identifier>& typeVariables)
{
    auto llvmType = llvm::StructType::create(llvmContext, "");
    auto tmpPtr   = std::make_unique<TypeInstanceAggregate>(identifier, ast.importedModule, ast.data, llvmType, typeClasses);
    auto typePtr  = tmpPtr.get();
    auto it2      = types.insert({ identifier, std::move(tmpPtr) });
    assert(it2.second);

    auto& constructor = ast.data->constructors.front();
    auto  fieldTypes  = getFieldTypes(ast, constructor.fields, typeVariables);

    llvmType->setName(getMangledName(*ast.importedModule, ast.data, typeVariables));
    typePtr->setFields(std::move(fieldTypes));

    return typePtr;
}

TypeInstPtr TypeContext::makeTypeInstanceVariant(const Identifier& identifier, llfp::DataAst ast, std::vector<std::string> typeClasses, const std::map<std::string, Identifier>& typeVariables)
{
    auto llvmPtrType = llvm::Type::getInt8PtrTy(llvmContext);
    auto tmpPtr      = std::make_unique<TypeInstanceVariant>(identifier, llvmPtrType, ast.importedModule, ast.data, std::move(typeClasses));
    auto typePtr     = tmpPtr.get();
    auto it2         = types.insert({ identifier, std::move(tmpPtr) });
    assert(it2.second);

    std::vector<TypeConstructor> constructors;
    for (auto it : llvm::enumerate(ast.data->constructors))
    {
        auto llvmType   = llvm::StructType::create(llvmContext, "");
        auto fieldTypes = getFieldTypes(ast, it.value().fields, typeVariables);

        llvmType->setName(getMangledName(*ast.importedModule, ast.data, it.index(), typeVariables));

        std::vector<llvm::Type*> llvmTypes;
        std::transform(fieldTypes.begin(), fieldTypes.end(), std::back_inserter(llvmTypes),
                       [](const TypeInstPtr& type) { return type->llvmType(); });
        llvmType->setBody(llvmTypes);

        constructors.push_back(TypeConstructor{ &it.value(), std::move(fieldTypes), llvmType });
    }
    typePtr->setConstructors(std::move(constructors));

    return typePtr;
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
            throw Error(std::string{ "type arity mismatch between " } + identifier.str() + " and " +
                        ast.importedModule->name() + ':' + ast.data->name + '/' + std::to_string(ast.data->typeVariables.size()));
        }
        assert(ast.data->typeVariables.size() == identifier.parameters.size());

        std::vector<std::string> typeClasses; // TODO!!!

        std::map<std::string, Identifier> typeVariables; // I think we need to build this
        for (int i = 0; i < ast.data->typeVariables.size(); ++i)
        {
            typeVariables[ast.data->typeVariables[i]] = identifier.parameters[i];
        }

        if (ast.data->constructors.size() == 1)
        {
            return makeTypeInstanceAggregate(identifier, ast, typeClasses, typeVariables);
        }
        else
        {
            return makeTypeInstanceVariant(identifier, ast, typeClasses, typeVariables);
        }
    }

    throw Error(std::string{ "unknown type " } + identifier.name.str());
}

TypeInstPtr TypeContext::getTypeFromConstructor(const std::string& name)
{
    auto       id  = GlobalIdentifier::split(name);
    auto       ast = sourceModule->lookupConstructor(id);
    Identifier tid{ id, {} };
    if (ast.data->typeVariables.size() != tid.parameters.size())
    {
        // throw Error(std::string{ "type arity mismatch between " } + tid.str() + " and " + ast.importedModule->name() + ':' + ast.data->name + '/' + std::to_string(ast.data->typeVariables.size()));
        throw Error(std::string{ "not implemented, cannot deduce type parameters for " } + id.str());
    }
    if (ast.data->constructors.size() == 1)
    {
        return makeTypeInstanceAggregate(tid, ast, {}, {});
    }
    else
    {
        return makeTypeInstanceVariant(tid, ast, {}, {});
    }
}

bool TypeContext::equals(TypeInstPtr type, const ast::TypeIdentifier& identifier)
{
    Identifier id;
    if (fullyQualifiedName(*sourceModule, id, identifier))
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

} // namespace llfp::type
