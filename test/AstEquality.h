#pragma once

#include "Ast.h"

namespace llfp::ast
{

bool operator==(Exp &e1, Exp &e2);
bool operator==(const LetExp &e1, const LetExp &e2);
bool operator==(const IfExp &e1, const IfExp &e2);
bool operator==(const CaseExp &e1, const CaseExp &e2);
bool operator==(const BinaryExp &e1, const BinaryExp &e2);
bool operator==(const UnaryExp &e1, const UnaryExp &e2);
bool operator==(const LiteralExp &e1, const LiteralExp &e2);
bool operator==(const CallExp &e1, const CallExp &e2);
bool operator==(const VariableExp &e1, const VariableExp &e2);
bool operator==(const ConstructorExp &e1, const ConstructorExp &e2);

bool operator==(const Parameter &p1, const Parameter &p2);
bool operator==(const ImportDeclaration &i1, const ImportDeclaration &i2);
bool operator==(const PublicDeclaration &p1, const PublicDeclaration &p2);
bool operator==(const NamedArgument &n1, const NamedArgument &n2);

bool operator==(const FunctionDeclaration &f1, const FunctionDeclaration &f2);
bool operator==(const Module &m1, const Module &m2);

} // llfp::ast
