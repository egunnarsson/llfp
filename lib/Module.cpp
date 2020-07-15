
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
    astModule = std::move(module);
    auto publics = astModule->publicDeclarations;
    for (auto &function : astModule->functionDeclarations)
    {
        auto it = std::find(publics.begin(), publics.end(), function->name);
        if (it != publics.end())
        {
            functions.insert(std::make_pair(function->name, function.get()));
        }
    }

    // make sure everything listed as public is available
    for (auto &functionName : publics)
    {
        if (functions.find(functionName) == functions.end())
        {
            llvm::errs() << "function declared as public is not defined " << functionName;
            return false;
        }
    }

    return true;
}

bool SourceModule::addImportedModules(const std::vector<const ImportedModule*> &moduleList)
{
    auto &imports = astModule->imports;
    for (auto module : moduleList)
    {
        auto it = std::find(imports.begin(), imports.end(), module->name());
        if (it != imports.end())
        {
            importedModules.insert(std::make_pair(module->name(), module));
        }
    }

    // make sure everything was resolved
    for (auto &moduleName : imports)
    {
        if (importedModules.find(moduleName) == importedModules.end())
        {
            llvm::errs() << "unresolved imported module " << moduleName;
            return false;
        }
    }

    return true;
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