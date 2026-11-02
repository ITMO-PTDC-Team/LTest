cmake_minimum_required(VERSION 3.6)
set (SOURCE_FILES
        lib.cpp
        lin_check.cpp
        logger.cpp
        verifying.cpp 
        generators.cpp
        coro_ctx_guard.cpp
        os_simulator.cpp
        fork_round.cpp
        attempt.cpp
        thread_tls.cpp
        io.cpp
        epoll.cpp
        context/donor_tls.cpp
)

add_library(runtime SHARED ${SOURCE_FILES})
find_package(Boost 1.81.0 EXACT CONFIG REQUIRED)
foreach(component assert config core mp11 pool predef smart_ptr)
    if(NOT TARGET Boost::${component})
        add_library(Boost::${component} ALIAS Boost::headers)
    endif()
endforeach()
set(BOOST_SUPERPROJECT_VERSION 1.81.0)
# Embed the private context implementation in the runtime shared library.
set(BUILD_SHARED_LIBS OFF)
add_subdirectory(${PROJECT_SOURCE_DIR}/third_party/boost_context
                 ${PROJECT_BINARY_DIR}/third_party/boost_context)
set_target_properties(boost_context PROPERTIES POSITION_INDEPENDENT_CODE ON)
# Keep private TLS-switching contexts separate from application coroutine code.
foreach(symbol make_fcontext jump_fcontext ontop_fcontext boost_context_set_tls)
    target_compile_definitions(boost_context PUBLIC ${symbol}=ltest_${symbol})
endforeach()
target_link_libraries(runtime PUBLIC $<BUILD_INTERFACE:Boost::context>
        PRIVATE gflags)
find_package(Threads REQUIRED)
target_link_libraries(runtime PRIVATE
    ${CMAKE_DL_LIBS} Threads::Threads syscall_intercept)
target_include_directories(runtime PRIVATE include)
target_link_options(runtime PRIVATE ${CMAKE_ASAN_FLAGS})
target_compile_options(runtime PRIVATE ${CMAKE_ASAN_FLAGS})
