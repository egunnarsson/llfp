#cmake -G "Visual Studio 15 2017 Win64" ..

cmake_minimum_required(VERSION 3.4.3)
project(LLFP VERSION 0.1)

set(LLVM_PATH_TO_USE "" CACHE STRING "Where to look for llvm")
find_package(LLVM REQUIRED CONFIG PATHS ${LLVM_PATH_TO_USE} NO_DEFAULT_PATH)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set your project compile flags.
# E.g. if using the C++ header files
# you will need to enable C++11 support
# for your compiler.

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Now build our tools
add_executable(llfp main.cpp Ast.cpp Codegen.cpp Lexer.cpp Parser.cpp Type.cpp Ast.h Codegen.h Lexer.h Parser.h Type.h)

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs core executionengine object orcjit support native)
#Core
#ExecutionEngine
#Object
#OrcJIT
#Support
#native

# Link against LLVM libraries
target_link_libraries(llfp ${llvm_libs})
