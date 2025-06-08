
find_package(Python3 REQUIRED)
# find_program(LIT_EXECUTABLE NAMES lit lit.py)
# if(NOT LIT_EXECUTABLE)
#     message(FATAL_ERROR "Could not find lit testing tool")
# endif()

# add_test(
#     NAME "coyield"
#     COMMAND ${LIT_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR} --filter-out  
#     ${CMAKE_CURRENT_SOURCE_DIR}/coyield/.* --verbose 
# )
# set_tests_properties("coyield"
#     PROPERTIES
#     LABELS "llvm-pass")