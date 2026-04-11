#include "os_simulator.h"

#include "block_manager.h"

void OSSimulator::ResetState() {
  block_manager.UnblockAll();
  memory_handler->FreeAllMemory();
}
bool OSSimulator::CanThreadContinue(std::size_t number) { return true; }

void OSSimulator::UpdateState() {}
