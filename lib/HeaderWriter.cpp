
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

std::string convertType(llfp::SourceModule &module, GlobalIdentifier& type)
{
    static std::unordered_map<std::string, llvm::StringRef> map {
        {type::name::Bool.str(), "bool"},

        {type::name::I8.str(), "int8_t"},
        {type::name::I16.str(), "int16_t"},
        {type::name::I32.str(), "int32_t"},
        {type::name::I64.str(), "int64_t"},
        // {type::name::I128, "i128"}, // unsupported

        {type::name::U8.str(), "uint8_t"},
        {type::name::U16.str(), "uint16_t"},
        {type::name::U32.str(), "uint32_t"},
        {type::name::U64.str(), "uint64_t"},
        // {type::name::U128, "u128"}, // unsupported

        // {type::name::Half, "half"}, // unsupported
        {type::name::Float.str(), "float"},
        {type::name::Double.str(), "double"},
        // long double usually 80 bit?

        {type::name::Char.str(), "unsigned char"},
    };

    if (type.moduleName.empty())
    {
        auto it = map.find(type.name);
        if (it != map.end())
        {
            return it->second.str();
        }
    }

    const ImportedModule *importedModule;
    const ast::DataDeclaration *typeDeclaration;
    std::tie(importedModule, typeDeclaration) = module.lookupType(type);
    if (importedModule != nullptr && typeDeclaration != nullptr)
    {
        if (typeDeclaration->typeVariables.empty())
        {
            return importedModule->getMangledName(typeDeclaration, {});
        }
        else
        {
            llvm::errs() << "cannot export data type with type variables: " << importedModule->name() << ':' << typeDeclaration->name;
            return std::string();
        }
    }

    // TODO: If we end up here we should delete the output file
    llvm::errs() << "unsupported C type (" << type.str() << ") in exported function\n";
    return std::string();
}

void writeParameter(llvm::raw_ostream &os, llfp::SourceModule &module, std::unique_ptr<ast::Parameter> &param)
{
    os << convertType(module, param->type.identifier) << ' ' << param->identifier;
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

    // for data used from other modules we need to #include ""

    // TODO: C requires these to be in order if they refer to each other
    for (auto &d : module.getAST()->dataDeclarations)
    {
        // TODO: instead we should mark them as needed?
        // otherwise we need to check recursively if all children are also exported
        // for all types in exported functions
        // if not it's an error
        if (!d->exported)
        {
            continue;
        }
        if (!d->typeVariables.empty())
        {
            llvm::errs() << "cannot export data type with type variables: " << module.name() << ':' << d->name;
            continue;
        }
        os << "struct " << module.getMangledName(d.get(), {}) << "\n{\n";
        for (auto &f : d->fields)
        {
            os << '\t' << convertType(module, f.type.identifier) << ' ' << f.name << ";\n";
        }
        os << "};\n\n";
    }

    for (auto &f : module.getAST()->functionDeclarations)
    {
        if (!f->exported) { continue; }

        os << convertType(module, f->type.identifier) << ' ' << module.getExportedName(f.get()) << '(';
        if (f->parameters.size() >= 1)
        {
            writeParameter(os, module, f->parameters.front());
            if (f->parameters.size() >= 2)
            {
                for (auto it = f->parameters.begin() + 1; it != f->parameters.end(); ++it)
                {
                    os << ", ";
                    writeParameter(os, module, *it);
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
