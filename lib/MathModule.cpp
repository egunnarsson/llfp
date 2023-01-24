
#include "MathModule.h"

#include "GlobalContext.h"
#include "Parser.h"
#include "String/StringConstants.h"
#include "Type/TypeInstance.h"

#include <unordered_map>


namespace llfp
{
/*

class Bounded a
{
    a maximum();
    a minimum();
}

pi :: Frac
e :: Frac

*/

namespace
{

constexpr auto code = R"x(

module math();

class Signed a
{
    a abs(a x);
#    a sign(a x);
}

class Fractional a
{
    a sqrt(a x);
    a pow(a x);

    a sin(a x);
    a cos(a x);
    a tan(a x);

    a exp(a x);
    a exp2(a x);
    a log(a x);
    a log10(a x);
    a log2(a x);

    a floor(a x);
    a ceil(a x);
    a round(a x);
}

)x";

void addInstance(const std::unique_ptr<ast::Module>& astModule, const std::unique_ptr<ast::Class>& classPtr, llvm::StringRef type)
{
    auto location        = classPtr->location;
    auto classIdentifier = GlobalIdentifier{ "math", classPtr->name };
    auto typeArgument    = ast::TypeIdentifier{ { "", type.str() }, {} };

    std::vector<std::unique_ptr<ast::Function>> functions;
    for (auto& function : classPtr->functions)
    {
        std::vector<std::unique_ptr<ast::Parameter>> parameters;
        for (auto& param : function->parameters)
        {
            parameters.push_back(std::make_unique<ast::Parameter>(param->location, typeArgument, param->identifier));
        }

        functions.push_back(std::make_unique<ast::Function>(location, function->name, typeArgument, std::move(parameters), nullptr, false));
    }

    auto instancePtr = std::make_unique<ast::ClassInstance>(location, std::move(classIdentifier), std::move(typeArgument), std::move(functions));
    astModule->classInstances.push_back(std::move(instancePtr));
}

} // namespace

MathModule::MathModule()
    : source_{ "math.llf", code }
{
    auto lexer  = lex::Lexer(&source_);
    auto parser = parse::Parser(&lexer);
    astModule_  = parser.parse();

    for (auto& classPtr : astModule_->classes)
    {
        if (classPtr->name == id::Signed)
        {
            // addInstance(astModule, classPtr, id::I8);
            // addInstance(astModule, classPtr, id::I16);
            addInstance(astModule_, classPtr, id::I32);
            addInstance(astModule_, classPtr, id::I64);
            // addInstance(astModule, classPtr, id::I128);
        }
        addInstance(astModule_, classPtr, id::Float);
        addInstance(astModule_, classPtr, id::Double);
    }
}

void MathModule::addToGlobalContext(GlobalContext& context)
{
    context.addModule(this);

    for (auto& classInstance : astModule_->classInstances)
    {
        for (auto& fun : classInstance->functions)
        {
            type::Identifier typeId{ classInstance->typeArgument.identifier, {} };
            context.addFunctionInstance(fun->name, std::move(typeId), { this, fun.get() });
        }
    }
}

const std::string& MathModule::name() const
{
    return astModule_->name;
}

FunAst MathModule::getFunction(const std::string& name)
{
    // we only have instances
    return { nullptr, nullptr };
}

FunDeclAst MathModule::getFunctionDecl(const std::string& name)
{
    // TODO: make some lookup table
    for (auto& classPtr : astModule_->classes)
    {
        for (auto& fun : classPtr->functions)
        {
            if (fun->name == name)
            {
                return { this, classPtr.get(), fun.get() };
            }
        }
    }
    return { nullptr, nullptr, nullptr };
}

DataAst MathModule::getType(const std::string& name) const
{
    assert(false);
    return { nullptr, nullptr };
}

DataAst MathModule::getConstructor(const std::string& name) const
{
    assert(false);
    return { nullptr, nullptr };
}

std::string MathModule::getMangledName(const ast::Function* function, const llvm::ArrayRef<const type::TypeInstance*> types) const
{
    assert(types.size() <= 2 && types.size() > 0);
    auto&      typeName = types[0]->identifier().name.name;
    const bool isFloat  = typeName == id::Float;
    const bool isInt32  = typeName == id::I32;
    if (function->name == "abs")
    {
        if (types[0]->isFloating())
        {
            return isFloat ? "fabsf" : "fabs";
        }
        else
        {
            return isInt32 ? "abs" : "labs";
        }
    }
    else
    {
        return isFloat ? function->name + 'f' : function->name;
    }
}

std::string MathModule::getMangledName(const ast::Data* data) const
{
    assert(false);
    return "";
}

std::string MathModule::getMangledName(const ast::Data* data, size_t constructorIndex) const
{
    assert(false);
    return "";
}

std::string MathModule::getMangledName(const char* internalFunctionName, const type::TypeInstance* type) const
{
    assert(false);
    return "";
}

std::string MathModule::getExportedName(const ast::Function* function) const
{
    assert(false);
    return "";
}

bool MathModule::fullyQualifiedName(type::Identifier& identifier, const ast::TypeIdentifier& tid) const
{
    return false;
}

// Lookup global
FunAst MathModule::lookupFunction(const GlobalIdentifier& identifier)
{
    assert(false);
    return { nullptr, nullptr };
}

FunDeclAst MathModule::lookupFunctionDecl(const GlobalIdentifier& identifier)
{
    assert(false);
    return { nullptr, nullptr, nullptr };
}

DataAst MathModule::lookupType(const GlobalIdentifier&) const
{
    assert(false);
    return { nullptr, nullptr };
}

DataAst MathModule::lookupConstructor(const GlobalIdentifier&) const
{
    assert(false);
    return { nullptr, nullptr };
}

} // namespace llfp
