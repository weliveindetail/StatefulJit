#pragma once

#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"

namespace llvm {
  namespace orc {

// ----------------------------------------------------------------------------

class StatelessJit
{
  using ObjectLayer_t = ObjectLinkingLayer<>;
  using CompileLayer_t = IRCompileLayer<ObjectLayer_t>;
  using ModuleHandle_t = CompileLayer_t::ModuleSetHandleT;

public:
  StatelessJit(TargetMachine *targetMachine_rawptr);

  // avoid copying
  StatelessJit(const StatelessJit& tmpl) = delete;
  StatelessJit& operator=(const StatelessJit& tmpl) = delete;

  const TargetMachine &getTargetMachine() const { 
    return *TM; 
  }

  ModuleHandle_t addModule(std::unique_ptr<Module> module);
  void removeModule(ModuleHandle_t handle);
  void clearModules();

  JITSymbol findSymbol(const std::string Name);

private:
  std::string mangle(const std::string &Name);
  JITSymbol findMangledSymbol(const std::string &Name);

  auto createSymbolResolver();

  template<class SymbolResolver_t>
  ModuleHandle_t submitModule(
    std::unique_ptr<Module> module, 
    SymbolResolver_t resolver);

  const DataLayout DL;
  ObjectLayer_t ObjectLayer;
  CompileLayer_t CompileLayer;
  std::vector<ModuleHandle_t> ModuleHandles;
  std::unique_ptr<TargetMachine> TM;
};

// ----------------------------------------------------------------------------

  }
}
