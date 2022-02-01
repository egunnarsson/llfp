
#pragma warning(push, 0)

#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"

#pragma warning(pop)

#include "JIT.h"


namespace llfp
{

JIT::JIT(std::unique_ptr<llvm::orc::ExecutionSession> ES, llvm::orc::JITTargetMachineBuilder JTMB, llvm::DataLayout DL) :
    ES(std::move(ES)),
    DL(std::move(DL)),
    Mangle(*this->ES, this->DL),
    ObjectLayer(*this->ES, []() { return std::make_unique<llvm::SectionMemoryManager>(); }),
    CompileLayer(*this->ES, ObjectLayer, std::make_unique<llvm::orc::ConcurrentIRCompiler>(std::move(JTMB))),
    MainJD(this->ES->createBareJITDylib("<main>"))
{
    MainJD.addGenerator(llvm::cantFail(llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(DL.getGlobalPrefix())));
    ObjectLayer.setOverrideObjectFlagsWithResponsibilityFlags(true);
    ObjectLayer.setAutoClaimResponsibilityForObjectSymbols(true); // ? https://stackoverflow.com/questions/57733912/llvm-asserts-resolving-symbol-outside-this-responsibility-set
}

JIT::~JIT()
{
    if (auto Err = ES->endSession())
        ES->reportError(std::move(Err));
}

llvm::Expected<std::unique_ptr<JIT>> JIT::Create()
{
    auto EPC = llvm::orc::SelfExecutorProcessControl::Create();
    if (!EPC)
        return EPC.takeError();

    auto ES = std::make_unique<llvm::orc::ExecutionSession>(std::move(*EPC));

    llvm::orc::JITTargetMachineBuilder JTMB(
        ES->getExecutorProcessControl().getTargetTriple());

    auto DL = JTMB.getDefaultDataLayoutForTarget();
    if (!DL)
        return DL.takeError();

    return std::make_unique<JIT>(std::move(ES), std::move(JTMB),
        std::move(*DL));
}

llvm::Error JIT::addModule(llvm::orc::ThreadSafeModule TSM, llvm::orc::ResourceTrackerSP RT)
{
    if (!RT)
        RT = MainJD.getDefaultResourceTracker();
    return CompileLayer.add(RT, std::move(TSM));
}

llvm::Expected<llvm::JITEvaluatedSymbol> JIT::lookup(llvm::StringRef Name)
{
    return ES->lookup({ &MainJD }, Mangle(Name));
}

} // namespace llfp
