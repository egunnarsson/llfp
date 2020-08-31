
include_directories(${LLVM_INCLUDE_DIRS} ${LLFP_INCLUDE_DIR} ${GTEST_INCLUDE_DIR})

add_executable(lexer_test lexer_test.cpp)
target_link_libraries(lexer_test llfp_lib gtest gtest_main)

add_executable(parser_test parser_test.cpp AstEquality.cpp AstTinyPrint.cpp AstEquality.h AstTinyPrint.h)
target_link_libraries(parser_test llfp_lib gtest gtest_main)

add_executable(codegen_test codegen_test.cpp)
target_link_libraries(codegen_test llfp_lib gtest gtest_main)
