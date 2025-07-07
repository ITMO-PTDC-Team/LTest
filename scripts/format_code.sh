#!/usr/bin/env bash
find runtime -iname *.h -o -iname *.cpp | xargs clang-format -style=Google -i
find clangpass -iname *.h -o -iname *.cpp | xargs clang-format -style=Google -i
find syscall_intercept -iname *.h -o -iname *.cpp | xargs clang-format -style=Google -i
find test -iname *.h -o -iname *.cpp | xargs clang-format -style=Google -i
find codegen -iname *.h -o -iname *.cpp | xargs clang-format -style=Google -i
find verifying -iname *.h -o -iname *.cpp | xargs clang-format -style=Google -i
