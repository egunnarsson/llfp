
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

#include "Type/TypeInstance.h"


namespace llfp
{
namespace type
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
    return llfp::contains(typeClasses, std::string{ id::Num });
}

bool TypeInstance::isInteger() const
{
    return llfp::contains(typeClasses, std::string{ id::Integer });
}

bool TypeInstance::isBool() const
{
    return checkBasicType(identifier_, id::Bool);
}

bool TypeInstance::isFloating() const
{
    return llfp::contains(typeClasses, std::string{ id::Floating });
}

bool TypeInstance::isSigned()const
{
    return llfp::contains(typeClasses, std::string{ id::Signed });
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

} // namespace Type
} // namespace llfp
