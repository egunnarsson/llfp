#cmake -G "Visual Studio 15 2017 Win64" ..

cmake_minimum_required(VERSION 3.4.3)
project(LLFP VERSION 0.1)

set(CMAKE_CXX_STANDARD 17)
# set(CMAKE_CXX_STANDARD_REQUIRED ON) not required

set(LLFP_GTEST_PATH "" CACHE FILEPATH "Where to look for google test")
# find_package(GTest REQUIRED CONFIG PATHS ${LLFP_GTEST_PATH} NO_DEFAULT_PATH)

if (LLFP_GTEST_PATH)
    set(BUILD_GMOCK OFF CACHE BOOL "Builds the googlemock subproject")
    set(INSTALL_GTEST OFF CACHE BOOL "Enable installation of googletest. (Projects embedding googletest may want to turn this OFF.)")
    add_subdirectory(${LLFP_GTEST_PATH})
    set(GTEST_INCLUDE_DIR "${LLFP_GTEST_PATH}/googletest/include")
# set (GMOCK_INCLUDE_DIR "${LLFP_GTEST_PATH}/googlemock/include")
endif()

set(LLFP_LLVM_PATH "" CACHE FILEPATH "Where to look for llvm")
find_package(LLVM REQUIRED CONFIG PATHS ${LLFP_LLVM_PATH} NO_DEFAULT_PATH)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set your project compile flags.
# E.g. if using the C++ header files
# you will need to enable C++11 support
# for your compiler.

add_definitions(${LLVM_DEFINITIONS})

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs core executionengine object orcjit support native)

set (LLFP_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/lib/include")

add_subdirectory(lib)
add_subdirectory(main)

if (LLFP_GTEST_PATH)
    add_subdirectory(test)
endif()
