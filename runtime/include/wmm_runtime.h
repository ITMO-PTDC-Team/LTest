#pragma once

#include <cstdint>

// коды операций rmw, которые llvmpass передаёт в runtime
// нужны, чтобы одна функция __ltest_wmm_rmw_ обслуживала все rmw
// соответствие задаётся в yieldpass.cpp - AtomicRMWInst::getOperation()
enum LTestRmwOp : int {
  LTEST_RMW_ADD = 0,
  LTEST_RMW_SUB = 1,
  LTEST_RMW_AND = 2,
  LTEST_RMW_OR = 3,
  LTEST_RMW_XOR = 4,
  LTEST_RMW_XCHG = 5,
  LTEST_RMW_MAX = 6,
  LTEST_RMW_MIN = 7,
  LTEST_RMW_UMAX = 8,
  LTEST_RMW_UMIN = 9,
};

#ifdef __cplusplus
extern "C" {
#endif

// strategy's choice for read candidates
// index in [0, candidates_count)
int __ltest_wmm_choice(int candidates_count);

bool __ltest_wmm_load_i1(void* addr, int order);
int8_t __ltest_wmm_load_i8(void* addr, int order);
int16_t __ltest_wmm_load_i16(void* addr, int order);
int32_t __ltest_wmm_load_i32(void* addr, int order);
int64_t __ltest_wmm_load_i64(void* addr, int order);

// candidate's load api for llvmpass 
int __ltest_wmm_load_candidates_i1(void* addr, int order);
int __ltest_wmm_load_candidates_i8(void* addr, int order);
int __ltest_wmm_load_candidates_i16(void* addr, int order);
int __ltest_wmm_load_candidates_i32(void* addr, int order);
int __ltest_wmm_load_candidates_i64(void* addr, int order);

bool __ltest_wmm_load_apply_i1(int choice_index);
int8_t __ltest_wmm_load_apply_i8(int choice_index);
int16_t __ltest_wmm_load_apply_i16(int choice_index);
int32_t __ltest_wmm_load_apply_i32(int choice_index);
int64_t __ltest_wmm_load_apply_i64(int choice_index);


void __ltest_wmm_store_i1(void* addr, int order, bool value);
void __ltest_wmm_store_i8(void* addr, int order, int8_t value);
void __ltest_wmm_store_i16(void* addr, int order, int16_t value);
void __ltest_wmm_store_i32(void* addr, int order, int32_t value);
void __ltest_wmm_store_i64(void* addr, int order, int64_t value);


struct LTestCmpXchgResult_i1 {
  bool old;
  bool success;
};

struct LTestCmpXchgResult_i8 {
  int8_t old;
  bool success;
};

struct LTestCmpXchgResult_i16 {
  int16_t old;
  bool success;
};

struct LTestCmpXchgResult_i32 {
  int32_t old;
  bool success;
};

struct LTestCmpXchgResult_i64 {
  int64_t old;
  bool success;
};


int8_t __ltest_wmm_rmw_i8(void* addr, int order, int op, int8_t operand);
int16_t __ltest_wmm_rmw_i16(void* addr, int order, int op, int16_t operand);
int32_t __ltest_wmm_rmw_i32(void* addr, int order, int op, int32_t operand);
int64_t __ltest_wmm_rmw_i64(void* addr, int order, int op, int64_t operand);

LTestCmpXchgResult_i1 __ltest_wmm_cmpxchg_i1(
    void* addr,
    bool expected,
    bool desired,
    int success_order,
  int failure_order,
  bool is_weak);

LTestCmpXchgResult_i8 __ltest_wmm_cmpxchg_i8(
    void* addr,
    int8_t expected,
    int8_t desired,
    int success_order,
  int failure_order,
  bool is_weak);

LTestCmpXchgResult_i16 __ltest_wmm_cmpxchg_i16(
    void* addr,
    int16_t expected,
    int16_t desired,
    int success_order,
  int failure_order,
  bool is_weak);

LTestCmpXchgResult_i32 __ltest_wmm_cmpxchg_i32(
    void* addr,
    int32_t expected,
    int32_t desired,
    int success_order,
  int failure_order,
  bool is_weak);

LTestCmpXchgResult_i64 __ltest_wmm_cmpxchg_i64(
    void* addr,
    int64_t expected,
    int64_t desired,
    int success_order,
  int failure_order,
  bool is_weak);


#ifdef __cplusplus
}
#endif
