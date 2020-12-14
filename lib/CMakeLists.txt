
include_directories(${LLVM_INCLUDE_DIRS} ${LLFP_INCLUDE_DIR})

add_library(llfp_lib
	Ast.cpp
	Codegen.cpp
	HeaderWriter.cpp
	Lexer.cpp
	Module.cpp
	Parser.cpp
	Type.cpp
	include/Ast.h
	include/Codegen.h
	include/Common.h
	include/HeaderWriter.h
	include/Lexer.h
	include/Log.h
	include/Module.h
	include/Parser.h
	include/Type.h
)

if (MSVC)
	target_compile_options(llfp_lib PUBLIC /W4 /permissive-
		/D_SILENCE_CXX17_ITERATOR_BASE_CLASS_DEPRECATION_WARNING) # llvm iterator_facade_base uses std::iterator as base
endif (MSVC)
if ((MSVC) AND (MSVC_VERSION GREATER_EQUAL 1914))
	target_compile_options(llfp_lib PUBLIC "/Zc:__cplusplus")
endif()
if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    target_compile_options(llfp_lib PUBLIC "-fno-rtti")
endif()

# Link against LLVM libraries
target_link_libraries(llfp_lib ${llvm_libs})
