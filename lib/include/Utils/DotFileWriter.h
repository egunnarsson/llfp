#pragma once

#include "Ast.h"

#include <llvm/Support/raw_os_ostream.h>


namespace llfp::utils::dot
{

void writeDotFile(llvm::raw_ostream& os, const llfp::ast::Module& module);

}