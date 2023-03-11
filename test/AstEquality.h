#pragma once

#include "Ast.h"

namespace llfp::ast
{

bool operator==(const TypeIdentifier& t1, const TypeIdentifier& t2);

bool operator==(const Clause& c1, const Clause& c2);
bool operator==(Pattern& p1, Pattern& p2);
bool operator==(const BoolPattern& p1, const BoolPattern& p2);
bool operator==(const IdentifierPattern& p1, const IdentifierPattern& p2);
bool operator==(const IntegerPattern& p1, const IntegerPattern& p2);
bool operator==(const FloatPattern& p1, const FloatPattern& p2);
bool operator==(const CharPattern& p1, const CharPattern& p2);
bool operator==(const StringPattern& p1, const StringPattern& p2);
bool operator==(const NamedArgumentPattern& p1, const NamedArgumentPattern& p2);
bool operator==(const ConstructorPattern& p1, const ConstructorPattern& p2);

bool operator==(Exp& e1, Exp& e2);
bool operator==(const LetExp& e1, const LetExp& e2);
bool operator==(const IfExp& e1, const IfExp& e2);
bool operator==(const CaseExp& e1, const CaseExp& e2);
bool operator==(const BinaryExp& e1, const BinaryExp& e2);
bool operator==(const UnaryExp& e1, const UnaryExp& e2);
bool operator==(const LiteralExp& e1, const LiteralExp& e2);
bool operator==(const CallExp& e1, const CallExp& e2);
bool operator==(const VariableExp& e1, const VariableExp& e2);
bool operator==(const ConstructorExp& e1, const ConstructorExp& e2);
bool operator==(const IntrinsicExp& e1, const IntrinsicExp& e2);

bool operator==(const Parameter& p1, const Parameter& p2);
bool operator==(const Function& f1, const Function& f2);
bool operator==(const FunctionDeclaration& f1, const FunctionDeclaration& f2);
bool operator==(const Class& c1, const Class& c2);
bool operator==(const ClassInstance& c1, const ClassInstance& c2);
bool operator==(const Import& i1, const Import& i2);
bool operator==(const Public& p1, const Public& p2);
bool operator==(const NamedArgument& n1, const NamedArgument& n2);

bool operator==(const Module& m1, const Module& m2);

} // namespace llfp::ast
