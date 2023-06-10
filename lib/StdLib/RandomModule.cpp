
#include "StdLib/RandomModule.h"


namespace llfp
{

namespace
{

constexpr auto code = R"x(

module random(seed, rand, uniform);

# 48271, 0, 2147483647
data State = {
    u32 a;
    u32 c;
    u32 m;
    u32 value;
};

State seed(u32 s) = State{48271, 0, 2147483647, s};

State rand(State s) =
    let next = (s.a * s.value + s.c) % s.m;
    in State{s.a, s.c, s.m, next};

double uniform(State s) = 1;

)x";

} // namespace

const Source& RandomModule::getSource()
{
    static Source source{ "random.llf", code };
    return source;
}

} // namespace llfp
