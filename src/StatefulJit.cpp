#include "StatefulJit.h"

#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/IR/Mangler.h"
#include "llvm/Support/DynamicLibrary.h"

// ----------------------------------------------------------------------------

namespace llvm {
  namespace orc {

// ----------------------------------------------------------------------------

const int StatefulJit::VarDefinition::dummyInvalidInstanceId = 0;
int StatefulJit::VarDefinition::nextStatefulVariableInstanceId = 0;

// ----------------------------------------------------------------------------

StatefulJit::StatefulJit(TargetMachine *targetMachine_rawptr)
  : TM(targetMachine_rawptr)
  , DL(targetMachine_rawptr->createDataLayout())
  , CompileLayer(ObjectLayer, SimpleCompiler(*targetMachine_rawptr))
  , MappingLayer(CompileLayer)
{
  llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
  VarDefinition::nextStatefulVariableInstanceId = 1;
}

// ----------------------------------------------------------------------------

// default ctor for std::map
StatefulJit::VarDefinition::VarDefinition()
  : NameId(dummyInvalidInstanceId)
  , VarTy(llvm::Type::getVoidTy(llvm::getGlobalContext()))
{
}

// ----------------------------------------------------------------------------

StatefulJit::VarDefinition::VarDefinition(llvm::Type* ty)
  : NameId(nextStatefulVariableInstanceId++), VarTy(ty)
{
}

// ----------------------------------------------------------------------------

int StatefulJit::getOrCreateStatefulVariable(std::string name, llvm::Type* ty)
{
  auto it = mapDefsByName.find(name);

  // insert
  if (it == mapDefsByName.end())
  {
    VarDefinition newVariable(ty);
    mapDefsByName[name] = newVariable;
    mapMemLocationsById[newVariable.NameId] = nullptr;

    return newVariable.NameId;
  }

  // reuse
  VarDefinition& existingVariable = it->second;
  if (it->second.VarTy == ty)
  {
    return it->second.NameId;
  }

  // overwrite
  {
    // old definition disappears, so we don't need it anymore
    mapMemLocationsById.erase(it->second.NameId);

    // new definition takes over the name slot with a new id
    VarDefinition replacingVariable(ty);
    mapDefsByName[name] = replacingVariable;
    mapMemLocationsById[replacingVariable.NameId] = nullptr;

    return replacingVariable.NameId;
  }
}

bool StatefulJit::hasMemLocation(int varId)
{
  auto it = mapMemLocationsById.find(varId);
  bool invalid = (it == mapMemLocationsById.end() || it->second == nullptr);

  return !invalid;
}

void* StatefulJit::getMemLocation(int varId)
{
  assert(hasMemLocation(varId));
  return mapMemLocationsById.at(varId);
}

void StatefulJit::submitMemLocation(int varId, void* ptr)
{
  mapMemLocationsById[varId] = ptr;
}

// ----------------------------------------------------------------------------

void StatefulJit::addGlobalMapping(StringRef Name, void* Addr)
{
  MappingLayer.setGlobalMapping(mangle(Name), TargetAddress(Addr));
}

// ----------------------------------------------------------------------------

auto StatefulJit::createSymbolResolver()
{
  // We need a memory manager to allocate memory and resolve symbols for this
  // new module. Create one that resolves symbols by looking back into the JIT.
  auto externalLookup_f = [&](const std::string &name) {
    if (auto Sym = findMangledSymbol(name))
      return RuntimeDyld::SymbolInfo(Sym.getAddress(), Sym.getFlags());

    return RuntimeDyld::SymbolInfo(nullptr);
  };

  auto dylibLookup_f = [](const std::string &S) {
    return nullptr;
  };

  return createLambdaResolver(externalLookup_f, dylibLookup_f);
}

// ----------------------------------------------------------------------------

template<class SymbolResolver_t>
auto StatefulJit::submitModule(
  std::unique_ptr<Module> module,
  SymbolResolver_t resolver)
  -> ModuleHandle_t
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

// ----------------------------------------------------------------------------

auto StatefulJit::findSymbol(const std::string name) -> JITSymbol
{
  return findMangledSymbol(mangle(name));
}

// ----------------------------------------------------------------------------

auto StatefulJit::mangle(const std::string &name) -> std::string
{
  std::string MangledName;
  {
    raw_string_ostream MangledNameStream(MangledName);
    Mangler::getNameWithPrefix(MangledNameStream, name, DL);
  }
  return MangledName;
}

// ----------------------------------------------------------------------------

auto StatefulJit::findMangledSymbol(const std::string &name) -> JITSymbol
{
  bool exportedSymbolsOnly = false;

  // Search modules in reverse order: from last added to first added.
  // This is the opposite of the usual search order for dlsym, but makes more
  // sense in a REPL where we want to bind to the newest available definition.
  for (auto modHandle : make_range(ModuleHandles.rbegin(), ModuleHandles.rend()))
    if (auto symbol = CompileLayer.findSymbolIn(modHandle, name, exportedSymbolsOnly))
      return symbol;

  // look up in added globals
  if (auto SymAddr = MappingLayer.findSymbol(name, exportedSymbolsOnly))
    return SymAddr;

  // look up in the host process
  if (auto SymAddr = RTDyldMemoryManager::getSymbolAddressInProcess(name))
    return JITSymbol(SymAddr, JITSymbolFlags::Exported);

  return nullptr;
}

// ----------------------------------------------------------------------------

auto StatefulJit::addModule(
  std::unique_ptr<Module> module)
  -> ModuleHandle_t
{
  auto symbolResolver = createSymbolResolver();
  auto moduleHandle = submitModule(std::move(module), std::move(symbolResolver));

  return moduleHandle;
}

// ----------------------------------------------------------------------------

void StatefulJit::removeModule(ModuleHandle_t handle)
{
  auto handle_it =
    std::find(ModuleHandles.begin(), ModuleHandles.end(), handle);

  ModuleHandles.erase(handle_it);
  CompileLayer.removeModuleSet(handle);
}

// ----------------------------------------------------------------------------

void StatefulJit::clearModules()
{
  for (auto& handle : ModuleHandles)
    CompileLayer.removeModuleSet(handle);

  ModuleHandles.clear();
}

// ----------------------------------------------------------------------------

  }
}