
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

#include "Type.h"


namespace llfp
{
namespace type
{

namespace
{

bool checkBasicType(const Identifier& id, llvm::StringRef name)
{
    return id.parameters.empty() && id.name.moduleName.empty() && id.name.name == name;
}

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

        if (std::any_of(t.typeClasses.begin(), t.typeClasses.end(), [](const std::string& s) { return s == "Floating"; }))
        {
            result = context->getDouble();
        }
        else if (std::any_of(t.typeClasses.begin(), t.typeClasses.end(), [](const std::string& s) { return s == "Signed"; }))
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


bool isPrimitive(const ast::TypeIdentifier& id)
{
    if (id.parameters.empty() && id.identifier.moduleName.empty())
    {
        for (auto& type : name::AllTypes)
        {
            if (type == id.identifier.name)
            {
                return true;
            }
        }
    }
    return false;
}

bool isPrimitive(const Identifier &id)
{
    if (id.parameters.empty() && id.name.moduleName.empty())
    {
        for (auto& type : name::AllTypes)
        {
            if (type == id.name.name)
            {
                return true;
            }
        }
    }
    return false;
}


std::string Identifier::str() const
{
    std::string result = name.str();

    if (!parameters.empty())
    {
        result += '[';
        for (auto& arg : parameters)
        {
            result += arg.str();
            result += ',';
        }
        result.back() = ']';
    }

    return result;
}


TypeInstance::TypeInstance(Identifier identifier, std::vector<std::string> typeClasses_):
    identifier_{std::move(identifier)},
    typeClasses{std::move(typeClasses_)}
{
}

// Creates a new hm type
std::shared_ptr<hm::TypeConstant> TypeInstance::getType() const
{
    std::shared_ptr<hm::TypeConstant> type = std::make_shared<hm::TypeConstant>(identifier_.name.str());
    for (auto& typeClass : typeClasses)
    {
        type->typeClasses.insert(typeClass);
    }
    /*assert(ast->fields.size() == fields.size());
    for (size_t i = 0; i < fields.size(); ++i)
    {
        type->fields.insert({ast->fields[i].name, fields[i]->getType()});
    }*/
    return type;
}

const Identifier& TypeInstance::identifier() const
{
    return identifier_;
}

TypeInstPtr TypeInstance::getTypeParameter(int) const
{
    assert(false);
    return nullptr;
}

bool TypeInstance::isNum() const
{
    return llfp::contains(typeClasses, std::string{ "Num" });
}

bool TypeInstance::isInteger() const
{
    return llfp::contains(typeClasses, std::string{ "Integer" });
}

bool TypeInstance::isBool() const
{
    return checkBasicType(identifier_, "bool");
}

bool TypeInstance::isFloating() const
{
    return llfp::contains(typeClasses, std::string{ "Floating" });
}

bool TypeInstance::isSigned()const
{
    return llfp::contains(typeClasses, std::string{ "Signed" });
}

unsigned int TypeInstance::getFieldIndex(const std::string&) const
{
    return InvalidIndex;
}

const TypeInstance* TypeInstance::getFieldType(unsigned int) const
{
    assert(false);
    throw Error("internal error (TypeInstance::getFieldType)");
    //return nullptr;
}

unsigned int TypeInstance::getFieldCount() const
{
    return 0;
}


TypeInstanceBasic::TypeInstanceBasic(llvm::StringLiteral name, llvm::Type* llvmType, std::vector<std::string> typeClasses_) :
    TypeInstance({ {"", name.str()}, {} }, std::move(typeClasses_)),
    llvmType_{ llvmType }
{
}

llvm::Type* TypeInstanceBasic::llvmType() const
{
    return llvmType_;
}

bool TypeInstanceBasic::isStructType() const
{
    return false;
}


TypeInstanceStruct::TypeInstanceStruct(Identifier identifier, const ImportedModule* module_, const ast::Data* ast_, llvm::StructType* llvmType, std::vector<std::string> typeClasses) :
    TypeInstance(std::move(identifier), std::move(typeClasses)),
    llvmType_{ llvmType },
    module{ module_ },
    ast{ ast_ }
{
}

std::shared_ptr<hm::TypeConstant> TypeInstanceStruct::getType() const
{
    auto type = TypeInstance::getType();
    size_t i = 0;
    for (const auto& field : fields)
    {
        type->fields.insert({ast->fields[i].name, field->getType()});
        ++i;
    }
    return type;
}

llvm::Type* TypeInstanceStruct::llvmType() const
{
    return llvmType_;
}

bool TypeInstanceStruct::isStructType() const
{
    return true;
}

unsigned int TypeInstanceStruct::getFieldIndex(const std::string& fieldIdentifier) const
{
    assert(ast->fields.size() == fields.size());
    const unsigned int size = static_cast<unsigned int>(ast->fields.size());
    for (unsigned int i = 0; i < size; ++i)
    {
        if (ast->fields[i].name == fieldIdentifier) { return i; }
    }
    return InvalidIndex;
}

TypeInstPtr TypeInstanceStruct::getFieldType(unsigned int index) const
{
    assert(index < fields.size());
    return fields[index];
}

unsigned int TypeInstanceStruct::getFieldCount() const
{
    return static_cast<unsigned int>(fields.size());
}

void TypeInstanceStruct::setFields(std::vector<const TypeInstance*> fieldTypes)
{
    assert(fields.size() == 0);
    assert(parameters.size() == 0);
    assert(ast->fields.size() == fieldTypes.size());

    for (auto& typeVar : ast->typeVariables)
    {
        const auto findType = [&typeVar](const ast::TypeIdentifier& typeId, TypeInstPtr type) -> TypeInstPtr
        {
            auto findType_impl = [&typeVar](const ast::TypeIdentifier& typeId, TypeInstPtr type, auto& find_ref) -> TypeInstPtr
            {
                if (typeId.identifier.moduleName.empty() && typeId.identifier.name == typeVar)
                {
                    assert(typeId.parameters.empty());
                    return type;
                }

                //assert(typeId.parameters.size() == type->getTypeParameterCount());
                for (unsigned int i = 0; i < typeId.parameters.size(); ++i)
                {
                    auto result = find_ref(typeId.parameters[i], type->getTypeParameter(i), find_ref);
                    if (result != nullptr) { return result; }
                }

                return nullptr;
            };
            return findType_impl(typeId, type, findType_impl);
        };

        TypeInstPtr result = nullptr;
        for (unsigned int i = 0; i < fieldTypes.size(); ++i)
        {
            result = findType(ast->fields[i].type, fieldTypes[i]);
            if (result != nullptr) { break; }
        }
        assert(result != nullptr);
        parameters.push_back(result);
    }

    if (llvmType_ != nullptr)
    {
        std::vector<llvm::Type*> llvmTypes;
        std::transform(fieldTypes.begin(), fieldTypes.end(), std::back_inserter(llvmTypes),
            [](const TypeInstPtr& type) {return type->llvmType(); });

        llvmType_->setBody(llvmTypes);
    }
    fields = std::move(fieldTypes);
}


#define LLVM_TYPE(func) llvm::Type::get##func##Ty(llvmContext)

TypeContext::TypeContext(llvm::LLVMContext& llvmContext_, SourceModule* sourceModule_, GlobalContext* globalContext_) :
    llvmContext{ llvmContext_ },
    sourceModule{ sourceModule_ },
    globalContext{ globalContext_ }
{
    boolType      = addType(name::Bool,    LLVM_TYPE(Int1), { "Eq" });

    /*i8Type    =*/ addType(name::I8,      LLVM_TYPE(Int8),   { "Eq", "Ord", "Num", "Integer", "Signed" });
    /*i16Type   =*/ addType(name::I16,     LLVM_TYPE(Int16),  { "Eq", "Ord", "Num", "Integer", "Signed" });
    /*i32Type   =*/ addType(name::I32,     LLVM_TYPE(Int32),  { "Eq", "Ord", "Num", "Integer", "Signed" });
    i64Type       = addType(name::I64,     LLVM_TYPE(Int64),  { "Eq", "Ord", "Num", "Integer", "Signed" });
    /*i128Type  =*/ addType(name::I128,    LLVM_TYPE(Int128), { "Eq", "Ord", "Num", "Integer", "Signed" });

    /*u8Type    =*/ addType(name::U8,      LLVM_TYPE(Int8),   { "Eq", "Ord", "Num", "Integer" });
    /*u16Type   =*/ addType(name::U16,     LLVM_TYPE(Int16),  { "Eq", "Ord", "Num", "Integer" });
    /*u32Type   =*/ addType(name::U32,     LLVM_TYPE(Int32),  { "Eq", "Ord", "Num", "Integer" });
    u64Type       = addType(name::U64,     LLVM_TYPE(Int64),  { "Eq", "Ord", "Num", "Integer" });
    /*u128Type  =*/ addType(name::U128,    LLVM_TYPE(Int128), { "Eq", "Ord", "Num", "Integer" });

    /*halfType  =*/ addType(name::Half,   LLVM_TYPE(Half),    { "Eq", "Ord", "Num", "Floating", "Signed" });
    /*floatType =*/ addType(name::Float,  LLVM_TYPE(Float),   { "Eq", "Ord", "Num", "Floating", "Signed" });
    doubleType    = addType(name::Double, LLVM_TYPE(Double),  { "Eq", "Ord", "Num", "Floating", "Signed" });

    charType      = addType(name::Char,   LLVM_TYPE(Int8), { "Eq", "Ord" });
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

TypeInstPtr TypeContext::constructTypeUsingAnnotationStuff(hm::TypeAnnotation& context, const ast::Exp& exp)
{
    auto type = context.get(&exp);

    NaiveTypeConstructor visitor{ this };
    type->accept(&visitor);

    if (visitor.result == nullptr)
    {
        throw Error("NaiveTypeConstructor is bad");
    }

    check(context, visitor.result, type);

    return visitor.result;
}

TypeInstance* TypeContext::addType(llvm::StringLiteral name, llvm::Type* llvmType, std::vector<std::string> typeClasses)
{
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
