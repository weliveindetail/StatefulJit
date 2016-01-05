#include "StatelessJit.h"

#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/IR/Mangler.h"
#include "llvm/Support/DynamicLibrary.h"

// ----------------------------------------------------------------------------

namespace llvm {
  namespace orc {

// ----------------------------------------------------------------------------

StatelessJit::StatelessJit(TargetMachine *targetMachine_rawptr)
  : TM(targetMachine_rawptr)
  , DL(targetMachine_rawptr->createDataLayout())
  , CompileLayer(ObjectLayer, SimpleCompiler(*targetMachine_rawptr))
  , MappingLayer(ObjectLayer)
{
  llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
}

// ----------------------------------------------------------------------------

int StatelessJit::getOrCreateStatefulVariable(std::string name)
{
  if (mapIdsByName.find(name) == mapIdsByName.end())
  {
    int newNameId = statefulVariableNextId++;

    mapIdsByName[name] = newNameId;
    mapMemLocationsById[newNameId] = nullptr;
  }

  return mapIdsByName.at(name);
}

bool StatelessJit::hasMemLocation(int varId)
{
  auto it = mapMemLocationsById.find(varId);
  bool invalid = (it == mapMemLocationsById.end() || it->second == nullptr);

  return !invalid;
}

void* StatelessJit::getMemLocation(int varId)
{
  assert(hasMemLocation(varId));
  return mapMemLocationsById.at(varId);
}

void StatelessJit::submitMemLocation(int varId, void* ptr)
{
  mapMemLocationsById[varId] = ptr;
}

// ----------------------------------------------------------------------------

void StatelessJit::addGlobalMapping(StringRef Name, void* Addr)
{
  MappingLayer.setGlobalMapping(mangle(Name), TargetAddress(Addr));
}

// ----------------------------------------------------------------------------

auto StatelessJit::createSymbolResolver()
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
auto StatelessJit::submitModule(
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

auto StatelessJit::findSymbol(const std::string name) -> JITSymbol
{
  return findMangledSymbol(mangle(name));
}

// ----------------------------------------------------------------------------

auto StatelessJit::mangle(const std::string &name) -> std::string
{
  std::string MangledName;
  {
    raw_string_ostream MangledNameStream(MangledName);
    Mangler::getNameWithPrefix(MangledNameStream, name, DL);
  }
  return MangledName;
}

// ----------------------------------------------------------------------------

auto StatelessJit::findMangledSymbol(const std::string &name) -> JITSymbol
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

auto StatelessJit::addModule(
  std::unique_ptr<Module> module)
  -> ModuleHandle_t
{
  auto symbolResolver = createSymbolResolver();
  auto moduleHandle = submitModule(std::move(module), std::move(symbolResolver));

  return moduleHandle;
}

// ----------------------------------------------------------------------------

void StatelessJit::removeModule(ModuleHandle_t handle)
{
  auto handle_it =
    std::find(ModuleHandles.begin(), ModuleHandles.end(), handle);

  ModuleHandles.erase(handle_it);
  CompileLayer.removeModuleSet(handle);
}

// ----------------------------------------------------------------------------

void StatelessJit::clearModules()
{
  for (auto& handle : ModuleHandles)
    CompileLayer.removeModuleSet(handle);

  ModuleHandles.clear();
}

// ----------------------------------------------------------------------------

  }
}