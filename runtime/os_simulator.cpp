#include "os_simulator.h"

#include "block_manager.h"

void OSSimulator::ResetState() {
  memory_handler->FreeAllMemory();
  block_manager.UnblockAll();
}
bool OSSimulator::CanThreadContinue(std::size_t number) { return true; }

void OSSimulator::UpdateState() {}
