#pragma once

#include <string>
#include <unordered_map>

#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/GlobalMappingLayer.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"

namespace llvm {
  namespace orc {

// ----------------------------------------------------------------------------

class StatelessJit
{
  using ObjectLayer_t = ObjectLinkingLayer<>;
  using MappingLayer_t = GlobalMappingLayer<ObjectLayer_t>;
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

  /// add an existing object (function or pointer) via its
  /// mangled name. This function is best used for unmangled
  /// c style names.
  void addGlobalMapping(StringRef Name, void* Addr);

  ModuleHandle_t addModule(std::unique_ptr<Module> module);
  void removeModule(ModuleHandle_t handle);
  void clearModules();

  JITSymbol findSymbol(const std::string Name);

  std::unordered_map<std::string, int> mapIdsByName;
  std::unordered_map<int, void*> mapMemLocationsById;

  bool hasMemLocation(int varId);
  void* getMemLocation(int varId);
  void submitMemLocation(int varId, void* ptr);
  int getOrCreateStatefulVariable(std::string name);

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
  MappingLayer_t MappingLayer;
  std::vector<ModuleHandle_t> ModuleHandles;
  std::unique_ptr<TargetMachine> TM;

  int statefulVariableNextId = 1;
};

// ----------------------------------------------------------------------------

  }
}
