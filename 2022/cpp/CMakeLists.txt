cmake_minimum_required(VERSION 3.23)
project(aoc2022)

set(CMAKE_CXX_STANDARD 23)

include_directories(include)

add_executable(aoc2022 main.cpp src/day01.cpp src/day02.cpp src/day02.cpp src/day03.cpp include/day03.h)

# Google Test bits
include(FetchContent)
FetchContent_Declare(
        googletest
        # Specify the commit you depend on and update it regularly.
        URL https://github.com/google/googletest/archive/58d77fa8070e8cec2dc1ed015d66b454c8d78850.zip
        DOWNLOAD_EXTRACT_TIMESTAMP true
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

project(aoc2022_tests)

add_executable(aoc2022_tests_run tests/day01_tests.cpp src/day01.cpp src/day02.cpp src/day02.cpp tests/day02_tests.cpp src/day03.cpp include/day03.h tests/day03_tests.cpp)

target_link_libraries(aoc2022_tests_run gtest gtest_main)