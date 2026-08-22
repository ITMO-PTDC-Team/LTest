#pragma once

#include <gmock/gmock.h>

#include "gmock/gmock.h"
#include "lib.h"
#include "value_wrapper.h"

class MockTask : public ltest::CoroBase {
 public:
  MOCK_METHOD(std::shared_ptr<CoroBase>, Restart, (void*), (override));
  MOCK_METHOD(void, Resume, (), ());
  MOCK_METHOD(bool, IsReturned, (), (const));
  MOCK_METHOD(ltest::ValueWrapper, GetRetVal, (), (const, override));
  MOCK_METHOD(std::string_view, GetName, (), (const, override));
  MOCK_METHOD(std::vector<std::string>, GetStrArgs, (), (const, override));
  MOCK_METHOD(void*, GetArgs, (), (const, override));
  MOCK_METHOD(bool, IsSuspended, (), (const));
  MOCK_METHOD(void, Terminate, (), ());
  MOCK_METHOD(void, TerminateWith, (ltest::ValueWrapper), ());
  ~MockTask() override { this->returned = true; }
};
