#pragma once

#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"
#include "llvm/IR/Mangler.h"
#include "llvm/Support/DynamicLibrary.h"

namespace llvm {
namespace orc {

// ----------------------------------------------------------------------------

class StatelessJit
{
public:
  using ObjectLayer_t = ObjectLinkingLayer<>;
  using CompileLayer_t = IRCompileLayer<ObjectLayer_t>;
  using ModuleHandle_t = CompileLayer_t::ModuleSetHandleT;

  StatelessJit(TargetMachine* targetMachine)
    : DL(targetMachine->createDataLayout())
    , CompileLayer(ObjectLayer, SimpleCompiler(*targetMachine))
  {
    TM = std::unique_ptr<TargetMachine>(targetMachine);
    llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
  }

  const TargetMachine &getTargetMachine() const { return *TM; }

  auto createSymbolResolver()
  {
    // We need a memory manager to allocate memory and resolve symbols for this
    // new module. Create one that resolves symbols by looking back into the JIT.
    auto externalLookup_f = [&](const std::string &Name) {
      if (auto Sym = findMangledSymbol(Name))
        return RuntimeDyld::SymbolInfo(Sym.getAddress(), Sym.getFlags());

      return RuntimeDyld::SymbolInfo(nullptr);
    };

    auto dylibLookup_f = [](const std::string &S) {
      return nullptr;
    };

    return createLambdaResolver(externalLookup_f, dylibLookup_f);
  }

  template<class SymbolResolver_t>
  ModuleHandle_t submitModule(std::unique_ptr<Module> module, SymbolResolver_t resolver)
  {
    std::vector<decltype(module)> singleItemSet;
    singleItemSet.push_back(std::move(module));

    ModuleHandle_t handle =
      CompileLayer.addModuleSet(
        std::move(singleItemSet),
        make_unique<SectionMemoryManager>(),
        std::move(resolver));

    ModuleHandles.push_back(handle);
    return handle;
  }

  ModuleHandle_t addModule(std::unique_ptr<Module> module)
  {
    auto symbolResolver = createSymbolResolver();
    auto moduleHandle = submitModule(std::move(module), std::move(symbolResolver));

    return moduleHandle;
  }

  void removeModule(ModuleHandle_t handle) 
  {
    auto handle_it = 
      std::find(ModuleHandles.begin(), ModuleHandles.end(), handle);

    ModuleHandles.erase(handle_it);
    CompileLayer.removeModuleSet(handle);
  }

  void clearModules() 
  {
    for (auto& handle : ModuleHandles)
      CompileLayer.removeModuleSet(handle);

    ModuleHandles.clear();
  }

  JITSymbol findSymbol(const std::string Name) 
  {
    return findMangledSymbol(mangle(Name));
  }

private:
  std::string mangle(const std::string &Name) 
  {
    std::string MangledName;
    {
      raw_string_ostream MangledNameStream(MangledName);
      Mangler::getNameWithPrefix(MangledNameStream, Name, DL);
    }
    return MangledName;
  }

  JITSymbol findMangledSymbol(const std::string &Name)
  {
    bool exportedSymbolsOnly = false;

    // Search modules in reverse order: from last added to first added.
    // This is the opposite of the usual search order for dlsym, but makes more
    // sense in a REPL where we want to bind to the newest available definition.
    for (auto modHandle : make_range(ModuleHandles.rbegin(), ModuleHandles.rend()))
      if (auto symbol = CompileLayer.findSymbolIn(modHandle, Name, exportedSymbolsOnly))
        return symbol;

    // If we can't find the symbol in the JIT, try looking in the host process.
    if (auto SymAddr = RTDyldMemoryManager::getSymbolAddressInProcess(Name))
      return JITSymbol(SymAddr, JITSymbolFlags::Exported);

    return nullptr;
  }

  const DataLayout DL;
  ObjectLayer_t ObjectLayer;
  CompileLayer_t CompileLayer;
  std::unique_ptr<TargetMachine> TM;
  std::vector<ModuleHandle_t> ModuleHandles;
};

// ----------------------------------------------------------------------------

}
}
