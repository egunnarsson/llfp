#pragma once

#include <llvm/ADT/StringRef.h>


namespace llfp::id
{

#define Constant(var, name) inline constexpr llvm::StringLiteral var{name};

// Primitive types
Constant(Bool, "bool");
Constant(I8, "i8");
Constant(I16, "i16");
Constant(I32, "i32");
Constant(I64, "i64");
Constant(I128, "i128");

Constant(U8, "u8");
Constant(U16, "u16");
Constant(U32, "u32");
Constant(U64, "u64");
Constant(U128, "u128");

Constant(Half, "half");
Constant(Float, "float");
Constant(Double, "double");

Constant(Char, "char");
Constant(String, "string");

// Type classes
Constant(Eq, "Eq");
Constant(Ord, "Ord");
Constant(Num, "Num");
Constant(Integer, "Integer");
Constant(Signed, "Signed");
Constant(Floating, "Floating");

// Keywords
Constant(Module, "module");
Constant(Import, "import");
Constant(Export, "export");
Constant(Data, "data");
Constant(Class, "class");
Constant(Instance, "instance");

Constant(If, "if");
Constant(Then, "then");
Constant(Else, "else");
Constant(Let, "let");
Constant(In, "in");
Constant(Case, "case");
Constant(Of, "of");
Constant(End, "end");
Constant(True, "true");
Constant(False, "false");

#undef Constant

} // namespace llfp::id
