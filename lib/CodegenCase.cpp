/*
Not finished.
Complicated solution to do branches in case exp.
Hopefully not needed if -jump-threading optimizations works.
*/

#if 0

bool valueEqual(const ast::BoolPattern& p1, const ast::BoolPattern& p2) { return p1.value == p2.value; }
bool valueEqual(const ast::IdentifierPattern& p1, const ast::IdentifierPattern& p2) { return p1.value == p2.value; }
bool valueEqual(const ast::IntegerPattern& p1, const ast::IntegerPattern& p2) { return p1.value == p2.value; }
bool valueEqual(const ast::FloatPattern& p1, const ast::FloatPattern& p2) { return p1.value == p2.value; }
bool valueEqual(const ast::CharPattern& p1, const ast::CharPattern& p2) { return p1.value == p2.value; }
bool valueEqual(const ast::StringPattern& p1, const ast::StringPattern& p2) { return p1.value == p2.value; }
bool valueEqual(const ast::ConstructorPattern& p1, const ast::ConstructorPattern& p2) { return p1.identifier == p2.identifier; }

template<class T>
class PatternEqualT : public ast::PatternVisitor
{
public:
    T& first;
    bool result = false;
    PatternEqualT(T& p) : first{ p } {}

    template<class U>
    void visit_(U& second)
    {
        if constexpr (std::is_same<T, U>::value)
        {
            result = valueEqual(first, second);
        }
        else
        {
            result = false;
        }
    }

    void visit(ast::BoolPattern& pattern) override { visit_(pattern); }
    void visit(ast::IdentifierPattern& pattern) override { visit_(pattern); }
    void visit(ast::IntegerPattern& pattern) override { visit_(pattern); }
    void visit(ast::FloatPattern& pattern) override { visit_(pattern); }
    void visit(ast::CharPattern& pattern) override { visit_(pattern); }
    void visit(ast::StringPattern& pattern) override { visit_(pattern); }
    void visit(ast::ConstructorPattern& pattern) override { visit_(pattern); }
};

class PatternEqual : public ast::PatternVisitor
{
    ast::Pattern& other;

public:

    bool result = false;

    PatternEqual(ast::Pattern& other_) : other{ other_ } {}

    template<class T>
    void visit_(T& p)
    {
        PatternEqualT<T> visitor{ p };
        other.accept(&visitor);
        result = visitor.result;
    }

    void visit(ast::BoolPattern& pattern) override { visit_(pattern); }
    void visit(ast::IdentifierPattern& pattern) override { visit_(pattern); }
    void visit(ast::IntegerPattern& pattern) override { visit_(pattern); }
    void visit(ast::FloatPattern& pattern) override { visit_(pattern); }
    void visit(ast::CharPattern& pattern) override { visit_(pattern); }
    void visit(ast::StringPattern& pattern) override { visit_(pattern); }
    void visit(ast::ConstructorPattern& pattern) override { visit_(pattern); }
};

bool valueEqual(ast::Pattern& p1, ast::Pattern& p2)
{
    PatternEqual visitor{ p1 };
    p2.accept(&visitor);
    return visitor.result;
}

class PatternCompareBuilder : public ast::PatternVisitor
{
public:
    llvm::IRBuilder<>& irBuilder;
    llvm::Value* value = nullptr;
    llvm::Value* result = nullptr;

    PatternCompareBuilder(llvm::IRBuilder<>& irBuilder_, llvm::Value* value_) :
        irBuilder{ irBuilder_ },
        value{ value_ }
    {}

    void visit(ast::BoolPattern& pattern) override
    {
        result = irBuilder.CreateCmp(llvm::CmpInst::ICMP_EQ, value, pattern.value ? llvm::ConstantInt::getTrue(irBuilder.getContext()) : llvm::ConstantInt::getFalse(irBuilder.getContext()), "boolPattern");
    }

    void visit(ast::IdentifierPattern& pattern) override
    {
        result = llvm::ConstantInt::getTrue(irBuilder.getContext());
    }

    void visit(ast::IntegerPattern& pattern) override
    {
        unsigned int numBits = value->getType()->getIntegerBitWidth();
        auto patternValue = llvm::ConstantInt::get(llvm::IntegerType::get(irBuilder.getContext(), numBits), pattern.value, 10);
        result = irBuilder.CreateCmp(llvm::CmpInst::ICMP_EQ, value, patternValue, "integerPattern");
    }

    void visit(ast::FloatPattern& pattern) override
    {
        auto patternValue = llvm::ConstantFP::get(value->getType(), pattern.value);
        result = irBuilder.CreateFCmp(llvm::CmpInst::FCMP_OEQ, value, patternValue, "floatPattern");
    }

    void visit(ast::CharPattern& pattern) override
    {
        assert(pattern.value.size() == 1);
        auto patternValue = llvm::ConstantInt::get(value->getType(), static_cast<uint8_t>(pattern.value[0]));
        result = irBuilder.CreateCmp(llvm::CmpInst::ICMP_EQ, value, patternValue, "charPattern");
    }

    void visit(ast::StringPattern& pattern) override
    {
        assert(false);
    }

    void visit(ast::ConstructorPattern& pattern) override
    {
        // for now constructors only have one instance
        result = llvm::ConstantInt::getTrue(irBuilder.getContext());
    }
};

/*
T - .
|
a -------- b - _
|          |
true -> _  _
|       |
y       x
*/
struct PatternBranch
{
    // type?
    // leaf with exp?
    // covered values
    // or the value?

    ast::Pattern* pattern = nullptr;
    llvm::BasicBlock* block = nullptr;
    llvm::Value* value = nullptr;
    std::unique_ptr<PatternBranch> nextSibling = nullptr;
    std::unique_ptr<PatternBranch> firstChild = nullptr; // maybe something else? like root? trunk? so that it can make value for all the branches to test against
    //PatternBranch*                 failBranch  = nullptr; // can decude this?
    llvm::BasicBlock* leafExp = nullptr;

    PatternBranch(ast::Pattern* pattern_, llvm::BasicBlock* block_, llvm::Value* value_) :
        pattern{ pattern_ },
        block{ block_ },
        value{ value_ }
    {}

    static PatternBranch* findSibling(PatternBranch* branch, ast::Pattern& pattern)
    {
        if (branch == nullptr)
        {
            return nullptr;
        }
        if (valueEqual(*branch->pattern, pattern))
        {
            return branch;
        }
        return findSibling(branch->nextSibling.get(), pattern);
    }

    void build(llvm::IRBuilder<>& builder, llvm::BasicBlock* failBlock)
    {
        // test
        // jump to firstChild or leaf
        // fail
        // jump to nextSibling
        // or up...

        builder.SetInsertPoint(block);

        PatternCompareBuilder compareBuilder{ builder, value };
        pattern->accept(&compareBuilder);
        llvm::Value* cond = compareBuilder.result;

        assert((leafExp == nullptr && firstChild.get() != nullptr)
            || (leafExp != nullptr && firstChild.get() == nullptr));
        llvm::BasicBlock* trueBlock = leafExp != nullptr ? leafExp : firstChild->block;

        // find last sibiling check if variable
        ast::IdentifierPattern identPattern{ {}, "_" };
        auto variableSibling = findSibling(nextSibling.get(), identPattern);
        llvm::BasicBlock* falseBlock = variableSibling != nullptr ? variableSibling->block : failBlock;

        builder.CreateCondBr(cond, trueBlock, falseBlock);

        if (firstChild != nullptr) { firstChild->build(builder, falseBlock); }
        if (nextSibling != nullptr) { nextSibling->build(builder, failBlock); }
    }

    //bool isComplete(); // ?
};

struct PatternBuilderStackItem
{
    ast::Pattern* pattern;
    llvm::Value* recordValue;
    unsigned int  valueIndex;
};

struct PatternBuilderContext
{
    llvm::BasicBlock* exp;
    llvm::LLVMContext& llvmContext;
    llvm::IRBuilder<>& irBuilder;
    std::stack<PatternBuilderStackItem> stack;
};

class PatternBranchBuilder : public ast::PatternVisitor
{
public:

    // context?
    PatternBuilderContext& context;
    PatternBranch* parent;
    llvm::Value* value;

    PatternBranchBuilder(PatternBuilderContext& context_, PatternBranch* parent_, llvm::Value* value_) :
        context{ context_ },
        parent{ parent_ },
        value{ value_ }
    {}

    void visitNext(PatternBranch* branch)
    {
        auto next = context.stack.top();

        // make BB and value? except we dont know if this is duplicate yet
        // it should be the parent that is responsible for providing the value?
        context.irBuilder.SetInsertPoint();
        auto argValue = context.irBuilder.CreateExtractValue(next.recordValue, { next.valueIndex });

        PatternBranchBuilder builder{ context, branch, argValue };
        context.stack.pop();
        next.pattern->accept(&builder);
    }

    void visit_(ast::Pattern& pattern)
    {
        PatternBranch* branch = PatternBranch::findSibling(parent->firstChild.get(), pattern);

        if (branch != nullptr && context.stack.empty())
        {
            throw ErrorLocation(pattern.location, "pattern already defined");
        }

        if (branch == nullptr)
        {
            // insert new branch
            auto prevChild = std::move(parent->firstChild);
            parent->firstChild = std::make_unique<PatternBranch>(&pattern, llvm::BasicBlock::Create(context.llvmContext));
            parent->firstChild->nextSibling = std::move(prevChild);
            branch = parent->firstChild.get();
        }

        if (context.stack.empty())
        {
            branch->leafExp = context.exp;
        }
        else
        {
            visitNext(branch);
        }
    }

    void visit(ast::BoolPattern& pattern) override { visit_(pattern); }

    void visit(ast::IdentifierPattern& pattern) override
    {
        PatternBranch* branch = parent->firstChild.get();
        if (branch != nullptr)
        {
            while (branch->nextSibling != nullptr)
            {
                branch = branch->nextSibling.get();
            }
        }

        if (branch == nullptr)
        {
            // insert
            parent->firstChild = std::make_unique<PatternBranch>(&pattern, llvm::BasicBlock::Create(context.llvmContext));
            branch = parent->firstChild.get();
        }
        else if (!valueEqual(pattern, *branch->pattern))
        {
            // insert
            branch->nextSibling = std::make_unique<PatternBranch>(&pattern, llvm::BasicBlock::Create(context.llvmContext));
            branch = branch->nextSibling.get();
        }
        else if (context.stack.empty())
        {
            // a variable branch already exists
            throw ErrorLocation(pattern.location, "pattern already defined");
        }

        if (!context.stack.empty())
        {
            visitNext(branch);
        }
    }

    void visit(ast::IntegerPattern& pattern) override { visit_(pattern); }
    void visit(ast::FloatPattern& pattern) override { visit_(pattern); }
    void visit(ast::CharPattern& pattern) override { visit_(pattern); }
    void visit(ast::StringPattern& pattern) override { visit_(pattern); }

    void visit(ast::ConstructorPattern& pattern) override
    {
        PatternBranch* branch = PatternBranch::findSibling(parent->firstChild.get(), pattern);

        if (branch == nullptr)
        {
            // insert new branch
            auto prevChild = std::move(parent->firstChild);
            parent->firstChild = std::make_unique<PatternBranch>(&pattern, llvm::BasicBlock::Create(context.llvmContext));
            parent->firstChild->nextSibling = std::move(prevChild);
            branch = parent->firstChild.get();

            if (context.stack.empty() && pattern.arguments.empty())
            {
                branch->leafExp = context.exp;
                return;
            }
        }
        else
        {
            if (context.stack.empty() && pattern.arguments.empty())
            {
                throw ErrorLocation(pattern.location, "pattern already defined");
            }
        }

        unsigned int index = 0;
        for (auto& arg : pattern.arguments)
        {
            //context.irBuilder.SetInsertPoint();
            //auto argValue = context.irBuilder.CreateExtractValue(value, { index++ });
            context.stack.push({ arg.pattern.get(), value, index++ });
        }

        visitNext(branch);
    }
};

#endif
