#pragma once

#include <gmock/gmock.h>

#include "gmock/gmock.h"
#include "lib.h"

class MockTask : public CoroBase {
 public:
  MOCK_METHOD(std::shared_ptr<CoroBase>, Restart, (void*), (override));
  MOCK_METHOD(void, Resume, (), ());
  MOCK_METHOD(bool, IsReturned, (), (const));
  MOCK_METHOD(int, GetRetVal, (), (const, override));
  MOCK_METHOD(std::string_view, GetName, (), (const, override));
  MOCK_METHOD(std::vector<std::string>, GetStrArgs, (), (const, override));
  MOCK_METHOD(void*, GetArgs, (), (const, override));
  MOCK_METHOD(bool, IsSuspended, (), (const));
  MOCK_METHOD(void, Terminate, (), ());
  MOCK_METHOD(void, SetToken, (std::shared_ptr<Token>), ());
  MOCK_METHOD(int, Run, (), (override));
  virtual ~MockTask() { is_returned = true; }
};
