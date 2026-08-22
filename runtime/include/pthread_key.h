#pragma once

#include <pthread.h>

namespace ltest {

// LTest does not model thread-local storage yet.  While running under the
// simulator, a key therefore has one value shared by every simulated thread.
void RegisterRealPthreadKey(pthread_key_t key);
int CreateMockPthreadKey(pthread_key_t* key);
bool IsMockPthreadKey(pthread_key_t key);
void DeletePthreadKey(pthread_key_t key);
void* GetPthreadKeyValue(pthread_key_t key);
int SetPthreadKeyValue(pthread_key_t key, const void* value);

// Drop keys created by the simulated round and clear values belonging to real
// process keys before allocations tracked for the round are released. LTest
// does not model thread exit, so it deliberately does not run pthread-key
// destructors.
void ResetPthreadKeyValues();
bool ShouldUseMockPthreadKeys();

}  // namespace ltest
