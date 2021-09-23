#pragma once
/*
 ostream << operator for Ast classes made to print in a tiny format that is easy to compare
 */

#include <ostream>

#include "Parser.h"

std::ostream& operator<<(std::ostream& os, const llfp::GlobalIdentifier& i);
std::ostream& operator<<(std::ostream& os, const llfp::ast::TypeIdentifier& t);
std::ostream& operator<<(std::ostream& os, const llfp::ast::Field& f);
std::ostream& operator<<(std::ostream& os, const llfp::ast::Data& d);
//TODO: need a const visitor for this to be const
std::ostream& operator<<(std::ostream& os, llfp::ast::Exp& e);
std::ostream& operator<<(std::ostream& os, const llfp::ast::Parameter& p);
std::ostream& operator<<(std::ostream& os, const llfp::ast::Function& f);
std::ostream& operator<<(std::ostream& os, const llfp::ast::FunctionDeclaration& f);
std::ostream& operator<<(std::ostream& os, const llfp::ast::Class& f);
std::ostream& operator<<(std::ostream& os, const llfp::ast::ClassInstance& f);
std::ostream& operator<<(std::ostream& os, const llfp::ast::Public& p);
std::ostream& operator<<(std::ostream& os, const llfp::ast::Import& i);
std::ostream& operator<<(std::ostream& os, const llfp::ast::Module &m);
std::ostream& operator<<(std::ostream& os, const llfp::ast::NamedArgument& i);

// maybe rename this to avoid operators for std types in global namespace?
template<class T>
std::ostream& operator<<(std::ostream &os, const std::unique_ptr<T> &ptr)
{
    if (ptr == nullptr) return os << "nullptr";
    return os << *ptr;
}

template<class T>
std::ostream& operator<<(std::ostream &os, const std::vector<T> &v)
{
    os << "[";
    for (auto &e : v)
    {
        os << e << ", ";
    }
    return os << "]";
}
