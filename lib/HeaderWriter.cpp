
#include <cctype>
#include <unordered_map>
#include <string>

#pragma warning(push, 0)

#include "llvm/ADT/StringExtras.h"

#pragma warning(pop)

#include "Type.h"

#include "HeaderWriter.h"


namespace llfp
{

namespace
{

const char* convertType(const std::string &type)
{
    static std::unordered_map<std::string, const char*> map {
        {type::name::Bool, "bool"},

        {type::name::I8, "int8_t"},
        {type::name::I16, "int16_t"},
        {type::name::I32, "int32_t"},
        {type::name::I64, "int64_t"},
        // {type::name::I128, "i128"}, // unsupported

        {type::name::U8, "uint8_t"},
        {type::name::U16, "uint16_t"},
        {type::name::U32, "uint32_t"},
        {type::name::U64, "uint64_t"},
        // {type::name::U128, "u128"}, // unsupported

        // {type::name::Half, "half"}, // unsupported
        {type::name::Float, "float"},
        {type::name::Double, "double"},
        // long double usually 80 bit?

        {type::name::Char, "unsigned char"},
    };

    auto it = map.find(type);
    if (it != map.end())
    {
        return it->second;
    }

    // user type
    // for () {]

    // TODO: If we end up here we should delete the output file
    llvm::errs() << "unsupported C type (" << type << ") in exported function\n";
    return nullptr;
}

void writeParameter(llvm::raw_ostream &os, std::unique_ptr<ast::Parameter> &param)
{
    os << convertType(param->typeName) << ' ' << param->identifier;
}

} // namespace

void HeaderWriter::write(llvm::raw_ostream &os, llfp::SourceModule &module)
{
    auto headerGuard = module.name();
    for (auto &c : headerGuard) c = llvm::toUpper(c);
    headerGuard += "_H";

    os << "#ifndef " << headerGuard << '\n';
    os << "#define " << headerGuard << "\n\n";

    os << "#include <stdint.h>\n";
    os << "#include <stdbool.h>\n\n";

    os << "#ifdef __cplusplus\n";
    os << "extern \"C\" {\n";
    os << "#endif\n\n";

    // for () {} user types

    for (auto &f : module.getAST()->functionDeclarations)
    {
        os << convertType(f->typeName) << ' ' << module.getFullFunctionName(f.get()) << '(';
        if (f->parameters.size() >= 1)
        {
            writeParameter(os, f->parameters.front());
            if (f->parameters.size() >= 2)
            {
                for (auto it = f->parameters.begin() + 1; it != f->parameters.end(); ++it)
                {
                    os << ", ";
                    writeParameter(os, *it);
                }
            }
        }
        
        os << ");\n";
    }

    os << "\n#ifdef __cplusplus\n";
    os << "}\n";
    os << "#endif\n\n";

    os << "#endif\n";
}

} // llfp
