
#include "HeaderWriter.h"

#include "Type/TypeContext.h"

#pragma warning(push, 0)

#include <llvm/ADT/StringExtras.h>

#pragma warning(pop)

#include <cctype>
#include <string>
#include <unordered_map>


namespace llfp
{

namespace
{

std::string convertType(llfp::SourceModule& module, const GlobalIdentifier& type)
{
    static std::unordered_map<std::string, llvm::StringRef> map{
        { id::Bool.str(), "bool" },

        { id::I8.str(), "int8_t" },
        { id::I16.str(), "int16_t" },
        { id::I32.str(), "int32_t" },
        { id::I64.str(), "int64_t" },
        // {id::I128, "i128"}, // unsupported

        { id::U8.str(), "uint8_t" },
        { id::U16.str(), "uint16_t" },
        { id::U32.str(), "uint32_t" },
        { id::U64.str(), "uint64_t" },
        // {id::U128, "u128"}, // unsupported

        // {id::Half, "half"}, // unsupported
        { id::Float.str(), "float" },
        { id::Double.str(), "double" },
        // long double usually 80 bit?

        { id::Char.str(), "unsigned char" },
    };

    if (type.moduleName.empty())
    {
        auto it = map.find(type.name);
        if (it != map.end())
        {
            return it->second.str();
        }
    }

    auto ast = module.lookupType(type);
    if (ast.importedModule != nullptr && ast.data != nullptr)
    {
        if (ast.data->typeVariables.empty())
        {
            auto typeName = ast.importedModule->getMangledName(ast.data, {});
            typeName += '*'; // all user types passed by ptr
            return typeName;
        }
        else
        {
            llvm::errs() << "cannot export data type with type variables: " << ast.importedModule->name() << ':' << ast.data->name;
            return std::string();
        }
    }

    // TODO: If we end up here we should delete the output file
    llvm::errs() << "unsupported C type (" << type.str() << ") in exported function\n";
    return std::string();
}

void writeParameter(llvm::raw_ostream& os, llfp::SourceModule& module, std::unique_ptr<ast::Parameter>& param)
{
    auto type = convertType(module, param->type.identifier);
    if (type.back() == '*')
    {
        os << "const ";
    }
    os << type << ' ' << param->identifier;
}

} // namespace

void HeaderWriter::write(llvm::raw_ostream& os, llfp::SourceModule& module)
{
    auto headerGuard = module.name();
    for (auto& c : headerGuard) c = llvm::toUpper(c);
    headerGuard += "_H";

    os << "#ifndef " << headerGuard << "\n"
                                       "#define "
       << headerGuard << "\n\n"

                         "#include <stdint.h>\n"
                         "#include <stdbool.h>\n\n"

                         "#ifdef __cplusplus\n"
                         "extern \"C\" {\n"
                         "#endif\n\n";

    // for data used from other modules we need to #include ""

    // TODO: C requires these to be in order if they refer to each other
    for (auto& d : module.getAST()->datas)
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

        auto writeStruct = [&module, &os](const std::string& name, const std::vector<ast::Field>& fields) {
            os << "struct " << name << "\n{\n";
            for (auto& f : fields)
            {
                os << '\t' << convertType(module, f.type.identifier) << ' ' << f.name << ";\n";
            }
            os << "};\n\n";
        };

        if (d->constructors.size() > 1)
        {
            for (int i = 0; i < d->constructors.size(); ++i)
            {
                writeStruct(module.getMangledName(d.get(), i), d->constructors[i].fields);
            }

            // make union struct
            if (d->constructors.size() > 1)
            {
                os << "struct " << module.getMangledName(d.get()) << "\n{\n"
                                                                     "\tint type;\n" //TODO: this type need to match TypeInstanceVariant::getEnumType
                                                                     "\tunion {\n";
                for (int i = 0; i < d->constructors.size(); ++i)
                {
                    os << "\t\tstruct " << module.getMangledName(d.get(), i) << ' ' << d->constructors[i].name << ";\n";
                }
                os << "\t};\n"
                "};\n\n";
            }
        }
        else
        {
            writeStruct(module.getMangledName(d.get()), d->constructors.at(0).fields);
        }
    }

    for (auto& f : module.getAST()->functions)
    {
        if (!f->exported) { continue; }

        auto            retType     = convertType(module, f->type.identifier);
        const bool      retUserType = retType.back() == '*';
        llvm::StringRef funRetType  = retUserType ? llvm::StringLiteral("void") : llvm::StringRef(retType);

        os << funRetType << ' ' << module.getExportedName(f.get()) << '(';

        if (retUserType)
        {
            os << retType << " result";
        }

        if (f->parameters.size() >= 1)
        {
            if (retUserType)
            {
                os << ", ";
            }

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

    os << "\n#ifdef __cplusplus\n"
          "}\n"
          "#endif\n\n"

          "#endif\n";
}

} // namespace llfp
