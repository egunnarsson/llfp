
include_directories(${LLVM_INCLUDE_DIRS} ${LLFP_INCLUDE_DIR})

add_library(llfp_lib Ast.cpp Codegen.cpp Lexer.cpp Parser.cpp Type.cpp include/Ast.h include/Codegen.h include/Lexer.h include/Parser.h include/Type.h)

# Link against LLVM libraries
target_link_libraries(llfp_lib ${llvm_libs})
