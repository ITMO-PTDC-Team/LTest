#include "os_simulator.h"

void OSSimulator::ResetState() { memory_handler->FreeAllMemory(); }
bool OSSimulator::CanThreadContinue(std::size_t number) { return true; }

void OSSimulator::UpdateState() {}
