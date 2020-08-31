#pragma once
/*
 ostream << operator for Ast classes made to print in a tiny format that is easy to compare
 */

#include <ostream>

#include "Parser.h"

std::ostream& operator<<(std::ostream &os, const llfp::ast::Module &m);
//TODO: need a const visitor for this to be const
std::ostream& operator<<(std::ostream &os, llfp::ast::Exp &e);
std::ostream& operator<<(std::ostream &os, const llfp::ast::FunctionDeclaration &f);

std::ostream& operator<<(std::ostream &os, const llfp::ast::Parameter &p);
std::ostream& operator<<(std::ostream &os, const llfp::ast::PublicDeclaration &p);
std::ostream& operator<<(std::ostream &os, const llfp::ast::ImportDeclaration &i);

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
