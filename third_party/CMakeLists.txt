include(FetchContent)

FetchContent_Declare(
        fuzztest
        GIT_REPOSITORY https://github.com/google/fuzztest.git
        GIT_TAG         2025-02-14
        GIT_SHALLOW TRUE
        SYSTEM
)

FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG        v1.16.0
        GIT_SHALLOW TRUE
        SYSTEM
)

# FetchContent_Declare(
#         gflags
# 	GIT_REPOSITORY	https://github.com/gflags/gflags.git
# 	GIT_TAG			master
#         GIT_SHALLOW TRUE
#         SYSTEM
# )

FetchContent_MakeAvailable(fuzztest googletest)
