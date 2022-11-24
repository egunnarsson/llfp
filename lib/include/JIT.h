#pragma once

#pragma warning(push, 0)

#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/Mangling.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/IR/DataLayout.h"

#pragma warning(pop)

#include <memory>


namespace llfp
{

class JIT
{
    std::unique_ptr<llvm::orc::ExecutionSession> ES;

    llvm::DataLayout             DL;
    llvm::orc::MangleAndInterner Mangle;

    llvm::orc::RTDyldObjectLinkingLayer ObjectLayer;
    llvm::orc::IRCompileLayer           CompileLayer;

    llvm::orc::JITDylib& MainJD;

public:

    JIT(std::unique_ptr<llvm::orc::ExecutionSession> ES, llvm::orc::JITTargetMachineBuilder JTMB, llvm::DataLayout DL);
    ~JIT();

    static llvm::Expected<std::unique_ptr<JIT>> Create();

    const llvm::DataLayout& getDataLayout() const { return DL; }
    llvm::orc::JITDylib&    getMainJITDylib() { return MainJD; }

    llvm::Error addModule(llvm::orc::ThreadSafeModule TSM, llvm::orc::ResourceTrackerSP RT = nullptr);

    llvm::Expected<llvm::JITEvaluatedSymbol> lookup(llvm::StringRef Name);
};

} // namespace llfp
