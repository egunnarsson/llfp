
#include "Type/TypeInstance.h"

#include "Common/Algorithm.h"
#include "Error.h"
#include "Log.h"
#include "Module.h"
#include "Type/TypeContext.h"

#pragma warning(push, 0)

#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/Constants.h>
#include <llvm/Support/FormatVariadic.h>

#pragma warning(pop)

#include <algorithm>
#include <iterator>
#include <utility>


namespace
{

template<typename List>
const List& assertFalse()
{
    static List emptyList;
    assert(false);
    return emptyList;
}

} // namespace

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


TypeInstance::TypeInstance(Identifier identifier, std::vector<std::string> typeClasses_)
    : identifier_{ std::move(identifier) },
      typeClasses{ std::move(typeClasses_) }
{
}

std::shared_ptr<hm::TypeConstant> TypeInstance::getType(std::map<const TypeInstance*, std::shared_ptr<hm::TypeConstant>>& types) const
{
    auto it = types.find(this);
    if (it != types.end())
    {
        return it->second;
    }

    auto type   = std::make_shared<hm::TypeConstant>(identifier_.str());
    types[this] = type;

    for (auto& typeClass : typeClasses)
    {
        type->typeClasses.insert(typeClass);
    }
    type->parameters.emplace();
    for (auto paramIt : llvm::enumerate(identifier_.parameters))
    {
        auto paramInstance = getTypeParameter(paramIt.index());
        type->parameters->push_back(paramInstance->getType(types));
    }

    return type;
}

// Creates a new hm type
std::shared_ptr<hm::TypeConstant> TypeInstance::getType() const
{
    std::map<const TypeInstance*, std::shared_ptr<hm::TypeConstant>> types;
    return getType(types);
}

const ImportedModule* TypeInstance::getModule() const
{
    return nullptr;
}


const Identifier& TypeInstance::identifier() const
{
    return identifier_;
}

TypeInstPtr TypeInstance::getTypeParameter(size_t) const
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
    return llfp::contains(typeClasses, id::Integer.str());
}

bool TypeInstance::isBool() const
{
    return checkBasicType(identifier_, id::Bool);
}

bool TypeInstance::isFloating() const
{
    return llfp::contains(typeClasses, id::Floating.str());
}

bool TypeInstance::isSigned() const
{
    return llfp::contains(typeClasses, id::Signed.str());
}

ConstructorList TypeInstance::getConstructors() const
{
    return assertFalse<ConstructorList>();
}

unsigned int TypeInstance::getFieldIndex(const std::string&) const { return InvalidIndex; }
unsigned int TypeInstance::getFieldIndex(const std::string&, const std::string&) const { return InvalidIndex; }

const FieldList& TypeInstance::getFields() const { return assertFalse<FieldList>(); }
const FieldList& TypeInstance::getFields(const std::string& constructor) const { return assertFalse<FieldList>(); }

TypeInstanceBasic::TypeInstanceBasic(llvm::StringLiteral name, llvm::Type* llvmType, std::vector<std::string> typeClasses_)
    : TypeInstance({ { "", name.str() }, {} }, std::move(typeClasses_)),
      llvmType_{ llvmType }
{
}

llvm::Type* TypeInstanceBasic::llvmType() const { return llvmType_; }
bool        TypeInstanceBasic::isBasicType() const { return true; }
bool        TypeInstanceBasic::isRefType() const { return false; }
bool        TypeInstanceBasic::containsRefTypes() const { return false; }

llvm::TypeSize TypeInstanceBasic::getSize(const llvm::Module* llvmModule, size_t constructorIndex) const
{
    assert(constructorIndex == 0);
    return llvmModule->getDataLayout().getTypeAllocSize(llvmType_);
}

TypeInstanceAggregate::TypeInstanceAggregate(Identifier identifier, const ImportedModule* module_, const ast::Data* ast_, llvm::StructType* llvmType, std::vector<std::string> typeClasses)
    : TypeInstance(std::move(identifier), std::move(typeClasses)),
      constructor{ &ast_->constructors.at(0), {}, llvmType },
      ast{ ast_ },
      module{ module_ }
{
}

std::shared_ptr<hm::TypeConstant> TypeInstanceAggregate::getType(std::map<const TypeInstance*, std::shared_ptr<hm::TypeConstant>>& types) const
{
    auto it = types.find(this);
    if (it != types.end())
    {
        return it->second;
    }

    auto type = TypeInstance::getType(types);
    for (const auto& field : llvm::enumerate(constructor.fields))
    {
        type->fields.insert({ constructor.ast->fields[field.index()].name, field.value()->getType(types) });
    }
    return type;
}

const ImportedModule* TypeInstanceAggregate::getModule() const
{
    return module;
}

llvm::Type* TypeInstanceAggregate::llvmType() const { return constructor.llvmType_; }
bool        TypeInstanceAggregate::isBasicType() const { return false; }
bool        TypeInstanceAggregate::isRefType() const { return false; }

bool TypeInstanceAggregate::containsRefTypes() const
{
    for (auto& field : constructor.fields)
    {
        if (field->isRefType() || field->containsRefTypes())
        {
            return true;
        }
    }
    return false;
}

TypeInstPtr TypeInstanceAggregate::getTypeParameter(size_t index) const
{
    return parameters.at(index);
}

llvm::TypeSize TypeInstanceAggregate::getSize(const llvm::Module* llvmModule, size_t constructorIndex) const
{
    assert(constructorIndex == 0);
    return llvmModule->getDataLayout().getTypeAllocSize(constructor.llvmType_);
}

ConstructorList TypeInstanceAggregate::getConstructors() const
{
    return constructor;
}

unsigned int TypeInstanceAggregate::getFieldIndex(const std::string& fieldIdentifier) const
{
    assert(ast->constructors.size() == 1);
    auto& astConstructor = *constructor.ast;
    assert(astConstructor.fields.size() == constructor.fields.size());
    const unsigned int size = static_cast<unsigned int>(astConstructor.fields.size());
    for (unsigned int i = 0; i < size; ++i)
    {
        if (astConstructor.fields[i].name == fieldIdentifier) { return i; }
    }
    return InvalidIndex;
}

unsigned int TypeInstanceAggregate::getFieldIndex(const std::string& constructorName, const std::string& fieldIdentifier) const
{
    if (ast->name != constructorName) { throw Error("unknown constructor"); }
    const auto& astFields = constructor.ast->fields;
    assert(astFields.size() == constructor.fields.size());
    const unsigned int size = static_cast<unsigned int>(astFields.size());
    for (unsigned int i = 0; i < size; ++i)
    {
        if (astFields[i].name == fieldIdentifier) { return i; }
    }
    return InvalidIndex;
}

const FieldList& TypeInstanceAggregate::getFields() const { return constructor.fields; }
const FieldList& TypeInstanceAggregate::getFields(const std::string& constructorName) const
{
    if (ast->name != constructorName) { throw Error("unknown constructor"); }
    return constructor.fields;
}

namespace
{

TypeInstPtr findTypeOfTypeVar(const std::string& typeVar, const ast::TypeIdentifier& typeIdIn, TypeInstPtr type)
{
    auto findType_impl = [&typeVar](const ast::TypeIdentifier& typeId, TypeInstPtr type, auto& find_ref) -> TypeInstPtr {
        if (typeId.identifier.moduleName.empty() && typeId.identifier.name == typeVar)
        {
            assert(typeId.parameters.empty());
            return type;
        }

        // assert(typeId.parameters.size() == type->getTypeParameterCount());
        for (size_t i = 0; i < typeId.parameters.size(); ++i)
        {
            auto result = find_ref(typeId.parameters[i], type->getTypeParameter(i), find_ref);
            if (result != nullptr) { return result; }
        }

        return nullptr;
    };
    return findType_impl(typeIdIn, type, findType_impl);
}

} // namespace

void TypeInstanceAggregate::setFields(FieldList fieldTypes)
{
    assert(constructor.fields.size() == 0);
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

    if (constructor.llvmType_ != nullptr) // when is this not null?
    {
        std::vector<llvm::Type*> llvmTypes;
        std::transform(fieldTypes.begin(), fieldTypes.end(), std::back_inserter(llvmTypes),
                       [](const TypeInstPtr& type) { return type->llvmType(); });

        constructor.llvmType_->setBody(llvmTypes);
    }
    constructor.fields = std::move(fieldTypes);

    assert(this->ast->constructors.front().fields.size() == this->constructor.fields.size());
}


llvm::IntegerType* TypeInstanceVariant::getEnumType(llvm::LLVMContext& context, const TypeInstance* type)
{
    // c++-20
    // auto integerSize = std::bit_width(type->getConstructors().size())
    return llvm::IntegerType::getInt16Ty(context);
}

TypeInstanceVariant::TypeInstanceVariant(
    Identifier               identifier,
    llvm::PointerType*       llvmType,
    const ImportedModule*    module_,
    const ast::Data*         ast_,
    std::vector<std::string> typeClasses)
    : TypeInstance(std::move(identifier), std::move(typeClasses)),
      llvmType_{ llvmType },
      module{ module_ },
      ast{ ast_ }
{
}

std::shared_ptr<hm::TypeConstant> TypeInstanceVariant::getType(std::map<const TypeInstance*, std::shared_ptr<hm::TypeConstant>>& types) const
{
    auto it = types.find(this);
    if (it != types.end())
    {
        return it->second;
    }

    auto type = TypeInstance::getType(types);
    for (auto [astConstructor, constructor] : llvm::zip(ast->constructors, constructors))
    {
        type->constructors.insert(astConstructor.name);
    }
    assert(!(!type->fields.empty() && type->constructors.size() > 1));
    return type;
}

const ImportedModule* TypeInstanceVariant::getModule() const
{
    return module;
}

llvm::Type* TypeInstanceVariant::llvmType() const { return llvmType_; }
bool        TypeInstanceVariant::isBasicType() const { return false; }
bool        TypeInstanceVariant::isRefType() const { return true; }

bool TypeInstanceVariant::containsRefTypes() const
{
    for (auto& constructor : constructors)
    {
        for (auto& field : constructor.fields)
        {
            if (field->isRefType() || field->containsRefTypes())
            {
                return true;
            }
        }
    }
    return false;
}

TypeInstPtr TypeInstanceVariant::getTypeParameter(size_t index) const
{
    return parameters.at(index);
}

llvm::TypeSize TypeInstanceVariant::getSize(const llvm::Module* llvmModule, size_t constructorIndex) const
{
    auto& constructor = constructors.at(constructorIndex);
    return llvmModule->getDataLayout().getTypeAllocSize(constructor.llvmType_);
}

ConstructorList TypeInstanceVariant::getConstructors() const
{
    return ConstructorList{ &constructors.at(0), constructors.size() };
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

const FieldList& TypeInstanceVariant::getFields() const
{
    return assertFalse<FieldList>();
}

const FieldList& TypeInstanceVariant::getFields(const std::string& constructor) const
{
    auto i = findIndex(ast->constructors, [&constructor](const ast::DataConstructor& c) { return c.name == constructor; });
    if (!checkIndex(i)) { throw Error("unknown constructor"); }
    return constructors[i].fields;
}

void TypeInstanceVariant::setConstructors(std::vector<TypeConstructor> constructors_)
{
    for (auto& typeVar : ast->typeVariables)
    {
        TypeInstPtr result = nullptr;
        for (unsigned int constructorIndex = 0; constructorIndex < constructors_.size(); ++constructorIndex)
        {
            const auto& astFields   = ast->constructors[constructorIndex].fields;
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
