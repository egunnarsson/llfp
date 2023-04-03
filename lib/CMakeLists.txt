
include_directories(${LLVM_INCLUDE_DIRS} ${LLFP_INCLUDE_DIR})

add_library(llfp_lib
	Ast.cpp
	Codegen.cpp
	Common/Algorithm.cpp
	Driver.cpp
	GlobalContext.cpp
	HeaderWriter.cpp
	JIT.cpp
	Lexer.cpp
	llfp.cpp
	Log.cpp
	MathModule.cpp
	Module.cpp
	Parser.cpp
	ResolveIdentifiers.cpp
	Source.cpp
	Type/TypeContext.cpp
	Type/TypeInference.cpp
	Type/TypeInstance.cpp
	include/Ast.h
	include/Codegen.h
	include/Common/Algorithm.h
	include/Common/GlobalIdentifier.h
	include/Common/SourceLocation.h
	include/Driver.h
	include/Error.h
	include/GlobalContext.h
	include/HeaderWriter.h
	include/IModule.h
	include/JIT.h
	include/Lexer.h
	include/llfp.h
	include/Log.h
	include/MathModule.h
	include/Module.h
	include/Parser.h
	include/ResolveIdentifiers.h
	include/Source.h
	include/String/StringConstants.h
	include/Type/TypeContext.h
	include/Type/TypeInference.h
	include/Type/TypeInstance.h
)

if (MSVC)
	target_compile_options(llfp_lib PUBLIC /W4 /permissive- /external:I ${LLVM_INCLUDE_DIRS} /external:W0
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
