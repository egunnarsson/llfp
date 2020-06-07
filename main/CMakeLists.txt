
include_directories(${LLVM_INCLUDE_DIRS} ${LLFP_INCLUDE_DIR})

add_executable(llfp main.cpp)
target_link_libraries(llfp llfp_lib)
