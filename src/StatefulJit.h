#pragma once

#include <string>
#include <unordered_map>

#include <llvm/IR/LLVMContext.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/GlobalMappingLayer.h>
#include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>

#include "AST.h"

namespace llvm {
  namespace orc {

// ----------------------------------------------------------------------------

class StatefulJit
{
  using ObjectLayer_t = ObjectLinkingLayer<>;
  using CompileLayer_t = IRCompileLayer<ObjectLayer_t>;
  using MappingLayer_t = GlobalMappingLayer<CompileLayer_t>;
  using ModuleHandle_t = CompileLayer_t::ModuleSetHandleT;

public:
  StatefulJit(TargetMachine *targetMachine_rawptr);

  // avoid copying
  StatefulJit(const StatefulJit& tmpl) = delete;
  StatefulJit& operator=(const StatefulJit& tmpl) = delete;

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

  struct VarDefinition
  {
    VarDefinition();
    VarDefinition(llvm::Type* type);

    int NameId;
    llvm::Type* VarTy;

    static const int dummyInvalidInstanceId;
    static int nextStatefulVariableInstanceId;
  };

  std::unordered_map<std::string, VarDefinition> mapDefsByName;
  std::unordered_map<int, void*> mapMemLocationsById;

  bool hasMemLocation(int varId);
  void* getMemLocation(int varId);
  void submitMemLocation(int varId, void* ptr);
  int getOrCreateStatefulVariable(std::string name, llvm::Type* ty);

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
};

// ----------------------------------------------------------------------------

  }
}
