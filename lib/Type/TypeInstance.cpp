
#include <algorithm>
#include <iterator>
#include <utility>

#pragma warning(push, 0)

#include "llvm/Support/FormatVariadic.h"
#include "llvm/IR/Constants.h"

#pragma warning(pop)

#include "Common/Algorithm.h"
#include "Error.h"
#include "GlobalContext.h"
#include "Log.h"
#include "Module.h"
#include "Type/TypeContext.h"

#include "Type/TypeInstance.h"


namespace llfp::type
{

bool checkBasicType(const Identifier& id, llvm::StringRef name)
{
    return id.parameters.empty() && id.name.moduleName.empty() && id.name.name == name;
}

bool isPrimitive(const ast::TypeIdentifier& id)
{
    if (id.parameters.empty() && id.identifier.moduleName.empty())
    {
        for (auto& type : AllTypes)
        {
            if (type == id.identifier.name)
            {
                return true;
            }
        }
    }
    return false;
}

bool isPrimitive(const Identifier& id)
{
    if (id.parameters.empty() && id.name.moduleName.empty())
    {
        for (auto& type : AllTypes)
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


TypeInstance::TypeInstance(Identifier identifier, std::vector<std::string> typeClasses_) :
    identifier_{ std::move(identifier) },
    typeClasses{ std::move(typeClasses_) }
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
    return llfp::contains(typeClasses, id::Num.str());
}

bool TypeInstance::isInteger() const
{
    return llfp::contains(typeClasses,  id::Integer.str());
}

bool TypeInstance::isBool() const
{
    return checkBasicType(identifier_, id::Bool);
}

bool TypeInstance::isFloating() const
{
    return llfp::contains(typeClasses, id::Floating.str());
}

bool TypeInstance::isSigned()const
{
    return llfp::contains(typeClasses, id::Signed.str());
}

const ConstructorList& TypeInstance::getConstructors() const
{
    static ConstructorList empty;
    assert(false);
    return empty;
}

unsigned int TypeInstance::getFieldIndex(const std::string&) const { return InvalidIndex; }
unsigned int TypeInstance::getFieldIndex(const std::string&, const std::string&) const { return InvalidIndex; }

TypeInstPtr TypeInstance::getFieldType(unsigned int) const
{
    assert(false);
    throw Error("internal error (TypeInstance::getFieldType)");
}

TypeInstPtr TypeInstance::getFieldType(const std::string&, unsigned int) const
{
    assert(false);
    throw Error("internal error (TypeInstance::getFieldType)");
}

unsigned int TypeInstance::getFieldCount() const { return 0; }
unsigned int TypeInstance::getFieldCount(const std::string&) const { return 0; }


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

llvm::TypeSize TypeInstanceBasic::getSize(const llvm::Module* llvmModule) const
{
    return llvmModule->getDataLayout().getTypeAllocSize(llvmType_);
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
        type->fields.insert({ast->constructors.front().fields[i].name, field->getType()});
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

llvm::TypeSize TypeInstanceStruct::getSize(const llvm::Module* llvmModule) const
{
    return llvmModule->getDataLayout().getTypeAllocSize(llvmType_);
}

unsigned int TypeInstanceStruct::getFieldIndex(const std::string& fieldIdentifier) const
{
    assert(ast->constructors.size() == 1);
    auto& constructor = ast->constructors.front();
    assert(constructor.fields.size() == fields.size());
    const unsigned int size = static_cast<unsigned int>(constructor.fields.size());
    for (unsigned int i = 0; i < size; ++i)
    {
        if (constructor.fields[i].name == fieldIdentifier) { return i; }
    }
    return InvalidIndex;
}

unsigned int TypeInstanceStruct::getFieldIndex(const std::string& constructorName, const std::string& fieldIdentifier) const
{
    if (ast->constructors.front().name != constructorName) { throw Error("unknown constructor"); }
    const auto& astFields = ast->constructors.front().fields;
    assert(astFields.size() == fields.size());
    const unsigned int size = static_cast<unsigned int>(astFields.size());
    for (unsigned int i = 0; i < size; ++i)
    {
        if (astFields[i].name == fieldIdentifier) { return i; }
    }
    return InvalidIndex;
}

TypeInstPtr TypeInstanceStruct::getFieldType(unsigned int index) const
{
    assert(ast->constructors.size() == 1);
    assert(index < fields.size());
    return fields[index];
}

TypeInstPtr TypeInstanceStruct::getFieldType(const std::string& constructorName, unsigned int index) const
{
    if (ast->constructors.front().name != constructorName) { throw Error("unknown constructor"); }
    assert(index < fields.size());
    return fields[index];
}

unsigned int TypeInstanceStruct::getFieldCount() const
{
    assert(ast->constructors.size() == 1);
    return static_cast<unsigned int>(fields.size());
}

unsigned int TypeInstanceStruct::getFieldCount(const std::string& constructorName) const
{
    if (ast->constructors.front().name != constructorName) { throw Error("unknown constructor"); }
    return static_cast<unsigned int>(fields.size());
}

namespace {

TypeInstPtr findTypeOfTypeVar(const std::string& typeVar, const ast::TypeIdentifier& typeId, TypeInstPtr type)
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
}

} // namespace

void TypeInstanceStruct::setFields(FieldList fieldTypes)
{
    assert(fields.size() == 0);
    assert(parameters.size() == 0);
    assert(ast->constructors.size() == 1);
    const auto& astFields = ast->constructors.front().fields;
    assert(astFields.size() == fieldTypes.size());

    for (auto& typeVar : ast->typeVariables)
    {
        TypeInstPtr result = nullptr;
        for (unsigned int i = 0; i < fieldTypes.size(); ++i)
        {
            result = findTypeOfTypeVar(typeVar, astFields[i].type, fieldTypes[i]);
            if (result != nullptr) { break; }
        }
        assert(result != nullptr);
        parameters.push_back(result);
    }

    if (llvmType_ != nullptr) // when is this not null?
    {
        std::vector<llvm::Type*> llvmTypes;
        std::transform(fieldTypes.begin(), fieldTypes.end(), std::back_inserter(llvmTypes),
            [](const TypeInstPtr& type) {return type->llvmType(); });

        llvmType_->setBody(llvmTypes);
    }
    fields = std::move(fieldTypes);
}


TypeInstanceVariant::TypeInstanceVariant(
    Identifier identifier,
    llvm::PointerType* llvmType,
    const ImportedModule* module_,
    const ast::Data* ast_,
    std::vector<std::string> typeClasses) :
    TypeInstance(std::move(identifier), std::move(typeClasses)),
    llvmType_{ llvmType },
    module{ module_ },
    ast{ ast_ }
{
}

std::shared_ptr<hm::TypeConstant> TypeInstanceVariant::getType() const
{
    auto type = TypeInstance::getType();
    size_t constructorIndex = 0;
    for (const auto& constructor : ast->constructors)
    {
        type->constructors.push_back(constructor.name);
        size_t fieldIndex = 0;
        for (const auto& field : constructors[constructorIndex].fields)
        {
            type->fields.insert({ constructor.fields[fieldIndex].name, field->getType() });
            ++fieldIndex;
        }
        ++constructorIndex;
    }
    assert(!(!type->fields.empty() && type->constructors.size() > 1));
    return type;
}

llvm::Type* TypeInstanceVariant::llvmType() const
{
    return llvmType_;
}

bool TypeInstanceVariant::isStructType() const
{
    return true;
}

llvm::TypeSize TypeInstanceVariant::getSize(const llvm::Module* llvmModule) const
{
    llvm::TypeSize size = llvm::TypeSize::getFixed(0);
    for (auto& constructor : constructors)
    {
        auto variantSize = llvmModule->getDataLayout().getTypeAllocSize(constructor.llvmType_);
        if (variantSize.getFixedSize() > size.getFixedSize())
        {
            size = variantSize;
        }
    }
    return size;
}

const ConstructorList& TypeInstanceVariant::getConstructors() const
{
    return constructors;
}

unsigned int TypeInstanceVariant::getFieldIndex(const std::string&) const
{
    assert(false);
    return InvalidIndex;
}

unsigned int TypeInstanceVariant::getFieldIndex(const std::string& constructor, const std::string& fieldIdentifier) const
{
    auto i = findIndex(ast->constructors, [&constructor](const ast::DataConstructor& c) { return c.name == constructor; });
    if (!checkIndex(i)) { throw Error("unknown constructor"); }
    auto j = findIndex(ast->constructors[i].fields, [&fieldIdentifier](const ast::Field& f) { return f.name == fieldIdentifier; });
    if (!checkIndex(j)) { return InvalidIndex; }
    return static_cast<unsigned int>(j);
}

const TypeInstance* TypeInstanceVariant::getFieldType(unsigned int) const
{
    assert(false);
    return nullptr;
}

const TypeInstance* TypeInstanceVariant::getFieldType(const std::string& constructor, unsigned int index) const
{
    auto i = findIndex(ast->constructors, [&constructor](const ast::DataConstructor& c) { return c.name == constructor; });
    if (!checkIndex(i)) { throw Error("unknown constructor"); }
    return constructors[i].fields[index];
}

unsigned int TypeInstanceVariant::getFieldCount() const
{
    assert(false);
    return 0;
}

unsigned int TypeInstanceVariant::getFieldCount(const std::string& constructor) const
{
    auto i = findIndex(ast->constructors, [&constructor](const ast::DataConstructor& c) { return c.name == constructor; });
    if (!checkIndex(i)) { throw Error("unknown constructor"); }
    return static_cast<unsigned int>(constructors[i].fields.size());
}

void TypeInstanceVariant::setConstructors(ConstructorList constructors_)
{
    for (auto& typeVar : ast->typeVariables)
    {
        TypeInstPtr result = nullptr;
        for (unsigned int constructorIndex = 0; constructorIndex < constructors_.size(); ++constructorIndex)
        {
            const auto& astFields = ast->constructors[constructorIndex].fields;
            const auto& constructor = constructors_[constructorIndex];

            assert(astFields.size() == constructor.fields.size());

            for (unsigned int fieldIndex = 0; fieldIndex < constructor.fields.size(); ++fieldIndex)
            {
                result = findTypeOfTypeVar(typeVar, astFields[fieldIndex].type, constructor.fields[fieldIndex]);
                if (result != nullptr) { break; }
            }
            if (result != nullptr) { break; }
        }
        assert(result != nullptr);
        parameters.push_back(result);
    }

    constructors = std::move(constructors_);
}

} // namespace llfp::type
