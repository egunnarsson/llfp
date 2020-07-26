
#include "Log.h"

#include "Module.h"


namespace llfp
{

ImportedModule::~ImportedModule() {}


SourceModule::SourceModule(std::string path_) :
    path { std::move(path_) }
{}

SourceModule::~SourceModule() {}

bool SourceModule::setAST(std::unique_ptr<ast::Module> module)
{
    bool result = true;

    astModule = std::move(module);

    for (auto &publicDecl : astModule->publicDeclarations)
    {
        auto predicate = [&publicDecl](std::unique_ptr<ast::FunctionDeclaration> &function) { return publicDecl.name == function->name; };
        auto it = std::find_if(astModule->functionDeclarations.begin(), astModule->functionDeclarations.end(), predicate);
        if (it != astModule->functionDeclarations.end())
        {
            auto &function = *it;
            functions.insert(std::make_pair(function->name, function.get()));
        }
        else
        {
            Log(publicDecl.location, "function declared as public is not defined ", publicDecl.name);
            result = false;
        }
    }

    return result;
}

bool SourceModule::addImportedModules(const std::vector<const ImportedModule*> &moduleList)
{
    bool result = true;

    for (auto &importDecl : astModule->imports)
    {
        auto predicate = [&importDecl](const ImportedModule *module) { return module->name() == importDecl.name; };
        auto it = std::find_if(moduleList.begin(), moduleList.end(), predicate);
        if (it != moduleList.end())
        {
            auto module = *it;
            importedModules.insert(std::make_pair(module->name(), module));
        }
        else
        {
            Log(importDecl.location, "unresolved imported module ", importDecl.name);
            result = false;
        }
    }

    return result;
}

void SourceModule::setLLVM(std::unique_ptr<llvm::Module> module)
{
    llvmModule = std::move(module);
}

llvm::LLVMContext& SourceModule::context()
{
    return llvmContext;
}

const std::string& SourceModule::filePath() const
{
    return path;
}

const std::string& SourceModule::name() const
{
    return astModule->name;
}

const ast::FunctionDeclaration* SourceModule::getFunction(const std::string &name) const
{
    auto it = functions.find(name);
    if (it == functions.end())
    {
        return nullptr;
    }
    return it->second;
}

std::string SourceModule::getFullFunctionName(const ast::FunctionDeclaration *function) const
{
    return getFullFunctionName(function->name);
}

std::string SourceModule::getFullFunctionName(llvm::StringRef functionName) const
{
    std::string tmp;
    tmp += astModule->name;
    tmp += '_';
    tmp += functionName;
    return tmp;
}

const std::unordered_map<std::string, const ImportedModule*>& SourceModule::getImportedModules() const
{
    return importedModules;
}

ast::Module* SourceModule::getAST()
{
    return astModule.get();
}

llvm::Module* SourceModule::getLLVM()
{
    return llvmModule.get();
}

}