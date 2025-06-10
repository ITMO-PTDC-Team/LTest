#pragma once

#include <gmock/gmock.h>

#include "gmock/gmock.h"
#include "lib.h"
#include "value_wrapper.h"

class MockTask : public CoroBase {
 public:
  MOCK_METHOD(Task, Restart, (void*), (override));
  MOCK_METHOD(void, Resume, (), ());
  MOCK_METHOD(bool, IsReturned, (), (const));
  MOCK_METHOD(ValueWrapper, GetRetVal, (), (const, override));
  MOCK_METHOD(std::string_view, GetName, (), (const, override));
  MOCK_METHOD(std::vector<std::string>, GetStrArgs, (), (const, override));
  MOCK_METHOD(void*, GetArgs, (), (const, override));
  MOCK_METHOD(bool, IsSuspended, (), (const));
  MOCK_METHOD(void, Terminate, (), ());
  MOCK_METHOD(void, SetToken, (std::shared_ptr<Token>), ());
  MOCK_METHOD(void, SetStrArgsAndName,
              (std::string_view,
               std::function<std::vector<std::string>(std::shared_ptr<void>)>),
              (override));
  virtual ~MockTask() { is_returned = true; }
};
