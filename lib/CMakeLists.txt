
include_directories(${LLVM_INCLUDE_DIRS} ${LLFP_INCLUDE_DIR})

add_library(llfp_lib Ast.cpp Codegen.cpp HeaderWriter.cpp Lexer.cpp Module.cpp Parser.cpp Type.cpp include/Ast.h include/Codegen.h include/HeaderWriter.h include/Lexer.h include/Module.h include/Parser.h include/Type.h)

if (MSVC)
    target_compile_options(llfp_lib PUBLIC /W4)
endif (MSVC)

# Link against LLVM libraries
target_link_libraries(llfp_lib ${llvm_libs})
