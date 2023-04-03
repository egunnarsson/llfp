
#include "MathModule.h"


namespace llfp
{

namespace
{

constexpr auto code = R"x(

module math();

#{
# Lossy conversion (double -> float, i32 -> i8, u32 -> i32, i32 -> u32)
class Convertible a b
{
    b convert(a x);
}

# (float -> double, i8 -> i16)
class Promotable a b
{
    b promote(a x);
}

b bitcast(a x) = @;

}#

class Bounded a
{
    a minimum();
    a maximum();
}

instance Bounded i8
{
    i8 minimum() = -128;
    i8 maximum() = 127;
}

instance Bounded u8
{
    u8 minimum() = 0;
    u8 maximum() = 255;
}

instance Bounded i16
{
    i16 minimum() = -32768;
    i16 maximum() = 32767;
}

instance Bounded u16
{
    u16 minimum() = 0;
    u16 maximum() = 65535;
}

instance Bounded i32
{
    i32 minimum() = -2147483648;
    i32 maximum() = 2147483647;
}

instance Bounded u32
{
    u32 minimum() = -4294967296;
    u32 maximum() = 4294967295;
}

instance Bounded i64
{
    i64 minimum() = -9223372036854775808;
    i64 maximum() = 9223372036854775807;
}

instance Bounded u64
{
    u64 minimum() = 0;
    u64 maximum() = 18446744073709551615;
}

class Signed a
{
    a abs(a x);
    a sign(a x);
}

class Floating a
{
    a sqrt(a x);
    a pow(a x);

    a sin(a x);
    a cos(a x);
    a tan(a x);

    a exp(a x);
    a exp2(a x);
    a log(a x);
    a log10(a x);
    a log2(a x);

    a floor(a x);
    a ceil(a x);
    a round(a x);

    a pi();
    a e();
}

instance Floating float
{
    float sqrt(float x) = @sqrtf'float'float(x);
    float pow(float x) = @powf'float'float(x);

    float sin(float x) = @sinf'float'float(x);
    float cos(float x) = @cosf'float'float(x);
    float tan(float x) = @tanf'float'float(x);

    float exp(float x) = @expf'float'float(x);
    float exp2(float x) = @exp2f'float'float(x);
    float log(float x) = @logf'float'float(x);
    float log10(float x) = @log10f'float'float(x);
    float log2(float x) = @log2f'float'float(x);

    float floor(float x) = @floorf'float'float(x);
    float ceil(float x) = @ceilf'float'float(x);
    float round(float x) = @roundf'float'float(x);

    float pi() = 3.1415926535897932384;
    float e()  = 2.7182818284590452353;
}

instance Floating double
{
    double sqrt(double x) = @sqrt'double'double(x);
    double pow(double x) = @pow'double'double(x);

    double sin(double x) = @sin'double'double(x);
    double cos(double x) = @cos'double'double(x);
    double tan(double x) = @tan'double'double(x);

    double exp(double x) = @exp'double'double(x);
    double exp2(double x) = @exp2'double'double(x);
    double log(double x) = @log'double'double(x);
    double log10(double x) = @log10'double'double(x);
    double log2(double x) = @log2'double'double(x);

    double floor(double x) = @floor'double'double(x);
    double ceil(double x) = @ceil'double'double(x);
    double round(double x) = @round'double'double(x);

    double pi() = 3.1415926535897932384;
    double e()  = 2.7182818284590452353;
}

)x";

} // namespace

const Source& MathModule::getSource()
{
    static Source source{ "math.llf", code };
    return source;
}

} // namespace llfp
