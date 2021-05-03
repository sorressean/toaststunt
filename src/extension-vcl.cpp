/**
* Vectorclass extensions.
*/
#include "functions.h"      // register builtins
#include "list.h" //listappend, etc
#include "utils.h" //free_var plus many others

#define VCL_NAMESPACE VCL
#include "dependencies/vectorclass/vectorclass.h"
#include "dependencies/vectorclass/vectormath_trig.h"
#include "dependencies/vectorclass/vectormath_hyp.h"

#include <cmath>
#include <vector>

using namespace std;

static bool is_list_valid_vector(const Var& list)
{
    if (list.v.list[0].v.num != 3)
        return false;

    for (auto index = 1; index <= 3; ++index)
        {
            const auto valueType = list.v.list[index].type;
            if (valueType != TYPE_INT && valueType != TYPE_FLOAT)
                return false;
        }

    return true;
}

/**
* list_to_vector presupposes you have already checked that
* the vector is valid using is_list_valid_vector.
* The smallest vector is a vec4 of doubles.
*/
static void list_to_Vec4f(const Var& list, VCL::Vec4f& vec)
{
    float values[4] = {0.0f, 0.0f, 0.0f, 0.0f};

    for (auto index = 1; index <=3; ++index)
        {
            values[index - 1] =(
                                   list.v.list[index].type == TYPE_INT?
                                   (double)list.v.list[index].v.num
                                   : (double)list.v.list[index].v.fnum);
        }

    vec.load(values);
}

static void Vec4i_to_list(const VCL::Vec4i& vec, Var& list)
{
    int values[4] = {0,0,0,0};
    vec.store(values);

    for (int index = 0; index < 3; ++index)
        {
            list = listappend(list, Var::new_int(values[index]));
        }
}

static void Vec4f_to_list(const VCL::Vec4f& vec, Var& list)
{
    float values[4] = {0.0f, 0.0f, 0.0f, 0.0f};
    vec.store(values);

    for (int index = 0; index < 3; ++index)
        {
            list = listappend(list, Var::new_float(values[index]));
        }
}

static package
bf_vec3_exponent(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vec;
    list_to_Vec4f(arglist.v.list[1], vec);
    free_var(arglist);
    VCL::Vec4i results;
    results = VCL::exponent(vec);
    auto resultsList = new_list(0);
    Vec4i_to_list(results, resultsList);
    return make_var_pack(resultsList);
}

static package
bf_vec3_fraction(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vec;
    list_to_Vec4f(arglist.v.list[1], vec);
    free_var(arglist);
    VCL::Vec4f results;
    results = VCL::fraction(vec);
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);
    return make_var_pack(resultsList);
}

static package
bf_vec3_sqrt(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vec;
    list_to_Vec4f(arglist.v.list[1], vec);
    free_var(arglist);
    VCL::Vec4f results;
    results = VCL::sqrt(vec);
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);
    return make_var_pack(resultsList);
}

static package
bf_vec3_pow(Var arglist, Byte next, void *vdata, Objid progr)
{
    const auto stype = arglist.v.list[2].type;
    if (stype != TYPE_FLOAT && stype != TYPE_LIST)
        {
            free_var(arglist);
            return make_error_pack(E_TYPE);
        }
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }
    auto resultsList = new_list(0);

    if (stype == TYPE_FLOAT)
        {
            const float exponent = arglist.v.list[2].v.fnum;
            VCL::Vec4f vec;
            list_to_Vec4f(arglist.v.list[1], vec);
            free_var(arglist);
            VCL::Vec4f results;
            results = VCL::pow(vec, exponent);
            Vec4f_to_list(results, resultsList);
        }
    if (stype == TYPE_LIST)
        {
            VCL::Vec4f a, b;
            list_to_Vec4f(arglist.v.list[1], a);
            list_to_Vec4f(arglist.v.list[2], b);
            free_var(arglist);
            VCL::Vec4f results;
            results = VCL::pow(a, b);
            Vec4f_to_list(results, resultsList);
        }

    return make_var_pack(resultsList);
}

static package
bf_vec3_mul_add(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]) || !is_list_valid_vector(arglist.v.list[2]) || !is_list_valid_vector(arglist.v.list[3]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA, vecB, vecC;
    list_to_Vec4f(arglist.v.list[1], vecA);
    list_to_Vec4f(arglist.v.list[2], vecB);
    list_to_Vec4f(arglist.v.list[3], vecC);
    free_var(arglist);

    VCL::Vec4f results;
    results = VCL::mul_add(vecA, vecB, vecC);
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);
    return make_var_pack(resultsList);
}

static package
bf_vec3_mul_sub(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]) || !is_list_valid_vector(arglist.v.list[2]) || !is_list_valid_vector(arglist.v.list[3]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA, vecB, vecC;
    list_to_Vec4f(arglist.v.list[1], vecA);
    list_to_Vec4f(arglist.v.list[2], vecB);
    list_to_Vec4f(arglist.v.list[3], vecC);
    free_var(arglist);

    VCL::Vec4f results;
    results = VCL::mul_sub(vecA, vecB, vecC);
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);
    return make_var_pack(resultsList);
}

static package
bf_vec3_add(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]) || !is_list_valid_vector(arglist.v.list[2]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA, vecB;
    list_to_Vec4f(arglist.v.list[1], vecA);
    list_to_Vec4f(arglist.v.list[2], vecB);
    free_var(arglist);

    VCL::Vec4f results = vecA + vecB;
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);
    return make_var_pack(resultsList);
}

static package
bf_vec3_sub(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]) || !is_list_valid_vector(arglist.v.list[2]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA, vecB;
    list_to_Vec4f(arglist.v.list[1], vecA);
    list_to_Vec4f(arglist.v.list[2], vecB);
    free_var(arglist);

    VCL::Vec4f results = vecA - vecB;
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);
    return make_var_pack(resultsList);
}

static package
bf_vec3_mul(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]) || !is_list_valid_vector(arglist.v.list[2]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA, vecB;
    list_to_Vec4f(arglist.v.list[1], vecA);
    list_to_Vec4f(arglist.v.list[2], vecB);
    free_var(arglist);

    VCL::Vec4f results = vecA * vecB;
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);
    return make_var_pack(resultsList);
}

static package
bf_vec3_div(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]) || !is_list_valid_vector(arglist.v.list[2]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA, vecB;
    list_to_Vec4f(arglist.v.list[1], vecA);
    list_to_Vec4f(arglist.v.list[2], vecB);
    free_var(arglist);

    VCL::Vec4f results = vecA / vecB;
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);
    return make_var_pack(resultsList);
}

inline static float dotproduct(const VCL::Vec4f& a, const VCL::Vec4f& b)
{
    VCL::Vec4f mul = a * b;
    const auto sum = VCL::horizontal_add(mul);
    return sum;
}

static package
bf_vec3_dot(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]) || !is_list_valid_vector(arglist.v.list[2]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA, vecB;
    list_to_Vec4f(arglist.v.list[1], vecA);
    list_to_Vec4f(arglist.v.list[2], vecB);
    free_var(arglist);

    return make_var_pack(Var::new_float(dotproduct(vecA, vecB)));
}

static package
bf_vec3_length(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA;
    list_to_Vec4f(arglist.v.list[1], vecA);
    free_var(arglist);

    const auto dot = dotproduct(vecA, vecA);
    const auto length = sqrt(dot);
    return make_var_pack(Var::new_float(length));
}

static package
bf_vec3_sin(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA;
    list_to_Vec4f(arglist.v.list[1], vecA);
    free_var(arglist);

    const auto results = VCL::sin(vecA);
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);

    return make_var_pack(resultsList);
}

static package
bf_vec3_cos(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA;
    list_to_Vec4f(arglist.v.list[1], vecA);
    free_var(arglist);

    const auto results = VCL::cos(vecA);
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);

    return make_var_pack(resultsList);
}

static package
bf_vec3_tan(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA;
    list_to_Vec4f(arglist.v.list[1], vecA);
    free_var(arglist);

    const auto results = VCL::tan(vecA);
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);

    return make_var_pack(resultsList);
}

static package
bf_vec3_asin(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA;
    list_to_Vec4f(arglist.v.list[1], vecA);
    free_var(arglist);

    const auto results = VCL::asin(vecA);
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);

    return make_var_pack(resultsList);
}

static package
bf_vec3_acos(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA;
    list_to_Vec4f(arglist.v.list[1], vecA);
    free_var(arglist);

    const auto results = VCL::acos(vecA);
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);

    return make_var_pack(resultsList);
}

static package
bf_vec3_atan(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA;
    list_to_Vec4f(arglist.v.list[1], vecA);
    free_var(arglist);

    const auto results = VCL::atan(vecA);
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);

    return make_var_pack(resultsList);
}

static package
bf_vec3_sinh(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA;
    list_to_Vec4f(arglist.v.list[1], vecA);
    free_var(arglist);

    const auto results = VCL::sinh(vecA);
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);

    return make_var_pack(resultsList);
}

static package
bf_vec3_cosh(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA;
    list_to_Vec4f(arglist.v.list[1], vecA);
    free_var(arglist);

    const auto results = VCL::cosh(vecA);
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);

    return make_var_pack(resultsList);
}

static package
bf_vec3_tanh(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA;
    list_to_Vec4f(arglist.v.list[1], vecA);
    free_var(arglist);

    const auto results = VCL::tanh(vecA);
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);

    return make_var_pack(resultsList);
}

static package
bf_vec3_asinh(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA;
    list_to_Vec4f(arglist.v.list[1], vecA);
    free_var(arglist);

    const auto results = VCL::asinh(vecA);
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);

    return make_var_pack(resultsList);
}

static package
bf_vec3_acosh(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA;
    list_to_Vec4f(arglist.v.list[1], vecA);
    free_var(arglist);

    const auto results = VCL::acosh(vecA);
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);

    return make_var_pack(resultsList);
}

static package
bf_vec3_atanh(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_list_valid_vector(arglist.v.list[1]))
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    VCL::Vec4f vecA;
    list_to_Vec4f(arglist.v.list[1], vecA);
    free_var(arglist);

    const auto results = VCL::atanh(vecA);
    auto resultsList = new_list(0);
    Vec4f_to_list(results, resultsList);

    return make_var_pack(resultsList);
}

void register_vcl_extensions()
{
    register_function("vector3_exponent", 1, 1, bf_vec3_exponent, TYPE_LIST);
    register_function("vector3_fraction", 1, 1, bf_vec3_fraction, TYPE_LIST);
    register_function("vector3_pow", 2, 2, bf_vec3_pow, TYPE_LIST, TYPE_ANY);
    register_function("vector3_sqrt", 1, 1, bf_vec3_sqrt, TYPE_LIST);
    register_function("vector3_mul_add", 3, 3, bf_vec3_mul_add, TYPE_LIST, TYPE_LIST, TYPE_LIST);
    register_function("vector3_mul_sub", 3, 3, bf_vec3_mul_sub, TYPE_LIST, TYPE_LIST, TYPE_LIST);
    register_function("vector3_add", 2, 2, bf_vec3_add, TYPE_LIST, TYPE_LIST);
    register_function("vector3_sub", 2, 2, bf_vec3_sub, TYPE_LIST, TYPE_LIST);
    register_function("vector3_mul", 2, 2, bf_vec3_mul, TYPE_LIST, TYPE_LIST);
    register_function("vector3_div", 2, 2, bf_vec3_div, TYPE_LIST, TYPE_LIST);
    register_function("vector3_dot", 2, 2, bf_vec3_dot, TYPE_LIST, TYPE_LIST);
    register_function("vector3_length", 1, 1, bf_vec3_length, TYPE_LIST);
    register_function("vector3_sin", 1, 1, bf_vec3_sin, TYPE_LIST);
    register_function("vector3_cos", 1, 1, bf_vec3_cos, TYPE_LIST);
    register_function("vector3_tan", 1, 1, bf_vec3_tan, TYPE_LIST);
    register_function("vector3_asin", 1, 1, bf_vec3_asin, TYPE_LIST);
    register_function("vector3_acos", 1, 1, bf_vec3_acos, TYPE_LIST);
    register_function("vector3_atan", 1, 1, bf_vec3_atan, TYPE_LIST);
    register_function("vector3_sinh", 1, 1, bf_vec3_sinh, TYPE_LIST);
    register_function("vector3_cosh", 1, 1, bf_vec3_cosh, TYPE_LIST);
    register_function("vector3_tanh", 1, 1, bf_vec3_tanh, TYPE_LIST);
    register_function("vector3_asinh", 1, 1, bf_vec3_asinh, TYPE_LIST);
    register_function("vector3_acosh", 1, 1, bf_vec3_acosh, TYPE_LIST);
    register_function("vector3_atanh", 1, 1, bf_vec3_atanh, TYPE_LIST);
}
