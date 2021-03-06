/**
* While not all of these extensions are mine, this file exists to separate the functions I add to this fork away from the lisdude extensions.
* This should theoretically mean that life doesn't break when I merge every time.
*/
#include <ctype.h>
#include <algorithm>
#include <iterator>
#include <sstream>
#include <vector>

#include "dependencies/strnatcmp.c" // natural sorting

#include "background.h"
#include "collection.h" //ismember
#include "db.h"
#include "db_private.h"
#include "functions.h"      // register builtins
#include "list.h" //listappend, etc
#include "log.h"            // oklog()
#include "map.h" //mapforeach, etc
#include "utils.h" //free_var plus many others

#include <boost/algorithm/clamp.hpp>
#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <boost/accumulators/statistics/mean.hpp>
#include <boost/accumulators/statistics/kurtosis.hpp>
#include <boost/accumulators/statistics/count.hpp>
#include <boost/accumulators/statistics/max.hpp>
#include <boost/accumulators/statistics/min.hpp>
#include <boost/accumulators/statistics/skewness.hpp>
#include <boost/accumulators/statistics/sum.hpp>
#include <boost/accumulators/statistics/variance.hpp>

typedef int (*IsCharCallback)(int c);

using namespace std;
using namespace boost::accumulators;

static Var
list_assoc(Var& vtarget, Var& vlist, const int vindex)
{
    const auto length = vlist.v.list[0].v.num;
    for (int i = 1; i <= length; ++i)
        {
            if (vlist.v.list[i].type == TYPE_LIST &&
                    vlist.v.list[i].v.list[0].v.num >= vindex &&
                    equality(vlist.v.list[i].v.list[vindex], vtarget, 0))
                {
                    return var_dup(vlist.v.list[i]);
                }
        }
    return new_list(0);
}

static int
list_iassoc(Var& vtarget, Var& vlist, const int vindex)
{
    const auto length = vlist.v.list[0].v.num;
    for (int i = 1; i <= length; ++i)
        {
            if (vlist.v.list[i].type == TYPE_LIST &&
                    vlist.v.list[i].v.list[0].v.num >= vindex &&
                    equality(vlist.v.list[i].v.list[vindex], vtarget, 0))
                {
                    return i;
                }
        }
    return 0;
}

static package
bf_iassoc(Var arglist, Byte next, void *vdata, Objid progr)
{
    /* (ANY, LIST[, INT]) */
    const auto index = (arglist.v.list[0].v.num == 3?arglist.v.list[3].v.num : 1);
    if (index < 1)
        {
            free_var(arglist);
            return make_error_pack(E_RANGE);
        }

    Var r = Var::new_int(list_iassoc(arglist.v.list[1], arglist.v.list[2], index));

    free_var(arglist);
    return make_var_pack(r);
} /* end bf_listiassoc() */

static package
bf_assoc(Var arglist, Byte next, void *vdata, Objid progr)
{
    /* (ANY, LIST[, INT]) */
    const auto index = (arglist.v.list[0].v.num == 3 ? arglist.v.list[3].v.num : 1);
    if (index < 1)
        {
            free_var(arglist);
            return make_error_pack(E_RANGE);
        }

    Var r = list_assoc(arglist.v.list[1], arglist.v.list[2], index);

    free_var(arglist);
    return make_var_pack(r);
}

static int do_maphasvalue(Var key, Var value, void *data, int first)
{
    Var* search = (Var*)data;
    return equality(value, *search, 1);
}
static package bf_maphasvalue(Var arglist, Byte next, void *vdata, Objid progr)
{
    const auto result = mapforeach(arglist.v.list[1], do_maphasvalue, &arglist.v.list[2]);
    free_var(arglist);

    Var ret = Var::new_int(result);
    return make_var_pack(ret);
}

/**
* Intersection, difference, union are all taken from Goblin's extension pack and modified.
*/
static package
bf_intersection(Var arglist, Byte next, void *vdata, Objid progr)
{
    for (int x = 2; x <= arglist.v.list[0].v.num; ++x)
        {
            if (arglist.v.list[x].type != TYPE_LIST)
                {
                    free_var(arglist);
                    return make_error_pack(E_TYPE);
                }
        }

    Var r = arglist.v.list[0].v.num ? var_dup(arglist.v.list[1]) : new_list(0);

    if (arglist.v.list[0].v.num > 1)
        {
            int x, y;
            for (int x = 2; x <= arglist.v.list[0].v.num; x++)
                {
                    if (r.v.list[0].v.num < arglist.v.list[x].v.list[0].v.num)
                        {
                            for (int y = 1; y <= r.v.list[0].v.num; y++)
                                {
                                    if (!ismember(r.v.list[y], arglist.v.list[x], 0))
                                        {
                                            r = listdelete(r, y);
                                            y--;
                                        }
                                }
                        }
                    else
                        {
                            for (y = 1; y <= arglist.v.list[x].v.list[0].v.num; y++)
                                {
                                    if (!ismember(arglist.v.list[x].v.list[y], r, 0))
                                        {
                                            arglist.v.list[x] = listdelete(arglist.v.list[x], y);
                                            y--;
                                        }
                                }
                            free_var(r);
                            r = var_dup(arglist.v.list[x]);
                        }
                }
        }

    free_var(arglist);
    return make_var_pack(r);
}

static package
bf_diff(Var arglist, Byte next, void *vdata, Objid progr)
{
    Var result = var_dup(arglist.v.list[1]);
    int x, y;

    for (x = 2; x <= arglist.v.list[0].v.num; x++)
        {
            if (arglist.v.list[x].type != TYPE_LIST)
                {
                    free_var(result);
                    free_var(arglist);
                    return make_error_pack(E_TYPE);
                }
            for (y = 1; y <= arglist.v.list[x].v.list[0].v.num; y++)
                {
                    result = setremove(result, arglist.v.list[x].v.list[y]);
                }
        }

    free_var(arglist);
    return make_var_pack(result);
}

static package
bf_union(Var arglist, Byte next, void *vdata, Objid progr)
{
    Var result = arglist.v.list[0].v.num ? var_dup(arglist.v.list[1]) : new_list(0);
    int x, y;

    for (x = 2; x <= arglist.v.list[0].v.num; x++)
        {
            if (arglist.v.list[x].type != TYPE_LIST)
                {
                    free_var(arglist);
                    free_var(result);
                    return make_error_pack(E_TYPE);
                }
            for (y = 1; y <= arglist.v.list[x].v.list[0].v.num; y++)
                {
                    result = setadd(result, arglist.v.list[x].v.list[y]);
                }
        }

    free_var(arglist);
    return make_var_pack(result);
}

/**
* The following builtin is made to help combining of sets.
* It replaces the following moo code (assuming s and t are both sets):
* for i in (s)
* t = setadd(t, i);
* endfor
* This is also much faster because we create the set before adding it to the moo list.
*/
static package
bf_set_merge(Var arglist, Byte next, void *vdata, Objid progr)
{
    Var newList = list_dup(arglist.v.list[1]);
//now add the second one.
    const auto length = arglist.v.list[2].v.list[0].v.num;
    for (int index = 1; index <= length; ++index)
        {
            if (!ismember(arglist.v.list[2].v.list[index], newList, 0))
                {
                    Var element = var_ref(arglist.v.list[2].v.list[index]);
                    newList = listappend(newList, element);
                }
        }
    free_var(arglist);
    return make_var_pack(newList);
}

static package
bf_bit_or(Var arglist, Byte next, void *vdata, Objid progr)
{
    int a = arglist.v.list[1].v.num;
    int b = arglist.v.list[2].v.num;
    free_var(arglist);

    return make_var_pack(Var::new_int(a|b));
}
static package
bf_bit_and(Var arglist, Byte next, void *vdata, Objid progr)
{
    int a = arglist.v.list[1].v.num;
    int b = arglist.v.list[2].v.num;
    free_var(arglist);

    return make_var_pack(Var::new_int(a&b));
}
static package
bf_bit_xor(Var arglist, Byte next, void *vdata, Objid progr)
{
    int a = arglist.v.list[1].v.num;
    int b = arglist.v.list[2].v.num;
    free_var(arglist);

    return make_var_pack(Var::new_int(a^b));
}
static package
bf_bit_not(Var arglist, Byte next, void *vdata, Objid progr)
{
    int a = arglist.v.list[1].v.num;
    free_var(arglist);

    return make_var_pack(Var::new_int(~a));
}

static unsigned int count_all_list_elements(const Var& list)
{
    const auto length = list.v.list[0].v.num;
    if (length == 0)
        return length;

    unsigned int count = length;
    for (unsigned int i = 1; i <= length; ++i)
        {
            if (list.v.list[i].type == TYPE_LIST)
                {
                    count += count_all_list_elements(list.v.list[i])-1;
                }
        }

    return count;
}

static unsigned int list_vectorize(const Var& list, const Var& values, unsigned  int position = 1)
{
    const auto count = list.v.list[0].v.num;
    if (count == 0)
        return position;

    for (unsigned int index = 1; index <= count; ++index)
        {
            if (list.v.list[index].type == TYPE_LIST)
                {
                    position = list_vectorize(list.v.list[index], values, position);
                    continue;
                }
            values.v.list[position]=var_dup(list.v.list[index]);
            position+=1;
        }

    return position;
}

static package
bf_list_flatten(Var arglist, Byte next, void *vdata, Objid progr)
{
    const auto all_elements = count_all_list_elements(arglist.v.list[1]);
    auto ret = new_list(all_elements);
    list_vectorize(arglist.v.list[1], ret);
    free_var(arglist);

    return make_var_pack(ret);
}

static inline void add_variable_to_stream(stringstream& st, const Var& v)
{
    if (v.type != TYPE_STR)
        st << VarToString(v);
    else
        st << v.v.str;
}

static package bf_join(Var arglist, Byte next, void *vdata, Objid progr)
{
    const auto argLength = arglist.v.list[0].v.num;
    const auto listLength = arglist.v.list[1].v.list[0].v.num;
    if (listLength == 0)
        {
            free_var(arglist);
            return make_var_pack(Var::new_string(""));
        }

    stringstream st;
    const char* sep = (argLength == 1? nullptr : arglist.v.list[2].v.str);
    for (unsigned int index = 1; index <= listLength-1; ++index)
        {
            add_variable_to_stream(st, arglist.v.list[1].v.list[index]);
            st << (sep == nullptr? " " : sep);
        }
    add_variable_to_stream(st, arglist.v.list[1].v.list[listLength]);
    free_var(arglist);
    return make_var_pack(Var::new_string(st.str().c_str()));
}

static package bf_list_remove_duplicates(Var arglist, Byte next, void *vdata, Objid progr)
{
    const auto listLength = arglist.v.list[1].v.list[0].v.num;
    Var ret = new_list(0);
    for (unsigned int index = 1; index <= listLength; ++index)
        {
            if (!ismember(arglist.v.list[1].v.list[index], ret, 0))
                {
                    Var element = var_ref(arglist.v.list[1].v.list[index]);
                    ret = listappend(ret, element);
                }
        }

    free_var(arglist);
    return make_var_pack(ret);
}

static inline vector<Objid> all_contents(const Var& object)
{
    vector<Objid> objids;
    const auto topObject = dbpriv_find_object(object.v.obj);
    const auto objectCount = listlength(topObject->contents);
    for (int index = 1; index <= objectCount; ++index)
        {
            objids.push_back(topObject->contents.v.list[index].v.obj);
            const auto nestedContents = all_contents(topObject->contents.v.list[index]);
            if (nestedContents.size())
                objids.insert(objids.end(), nestedContents.begin(), nestedContents.end());
        }

    return objids;
}

static package bf_all_contents(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!valid(arglist.v.list[1].v.obj))
        {
            free_var(arglist);
            return make_error_pack(E_INVIND);
        }

    const auto contents = all_contents(arglist.v.list[1]);
    free_var(arglist);

    const auto length = contents.size();
    Var ret = new_list(length);
    for (unsigned int index = 1; index <= length; ++index)
        {
            ret.v.list[index] = Var::new_obj(contents[index-1]);
        }

    return make_var_pack(ret);
}

static package
bf_clamp(Var arglist, Byte next, void *vdata, Objid progr)
{
    const auto firstType = arglist.v.list[1].type;
    if (firstType != TYPE_INT && firstType != TYPE_FLOAT)
        {
            free_var(arglist);
            return make_error_pack(E_TYPE);
        }
    if (arglist.v.list[2].type != firstType || arglist.v.list[3].type != firstType)
        {
            free_var(arglist);
            return make_error_pack(E_TYPE);
        }

    const auto value = (firstType == TYPE_INT? arglist.v.list[1].v.num : arglist.v.list[1].v.fnum);
    const auto lower = (firstType == TYPE_INT? arglist.v.list[2].v.num : arglist.v.list[2].v.fnum);
    const auto upper = (firstType == TYPE_INT? arglist.v.list[3].v.num : arglist.v.list[3].v.fnum);
    free_var(arglist);
    const auto result = boost::algorithm::clamp(value, lower, upper);
    const auto returnVar = (firstType == TYPE_INT? Var::new_int(result) : Var::new_float(result));
    return make_var_pack(returnVar);
}

/*
*Collects various stats.
*/
static void collect_stats_callback(Var arglist, Var* ret)
{
    *ret = new_map();
    vector<double> values;
    const auto listLength = arglist.v.list[1].v.list[0].v.num;
    for (unsigned int i = 1; i <= listLength; ++i)
        {
            const auto elementType = arglist.v.list[1].v.list[i].type;
            if (elementType != TYPE_INT && elementType != TYPE_FLOAT)
                {
                    free_var(arglist);
                    return;
                }
            values.push_back((elementType == TYPE_INT? arglist.v.list[1].v.list[i].v.num : arglist.v.list[1].v.list[i].v.fnum));
        }

    accumulator_set<double, stats<tag::variance(lazy), tag::sum, tag::skewness, tag::min, tag::max, tag::kurtosis, tag::count, tag::mean>> acc;
    for (const auto& value: values)
        {
            acc(value);
        }
    *ret = mapinsert(*ret, str_dup_to_var("count"), Var::new_int(boost::accumulators::count(acc)));
    *ret = mapinsert(*ret, str_dup_to_var("kurtosis"), Var::new_float(kurtosis(acc)));
    *ret = mapinsert(*ret, str_dup_to_var("max"), Var::new_float(boost::accumulators::max(acc)));
    *ret = mapinsert(*ret, str_dup_to_var("min"), Var::new_float(boost::accumulators::min(acc)));
    *ret = mapinsert(*ret, str_dup_to_var("mean"), Var::new_float(boost::accumulators::mean(acc)));
    *ret = mapinsert(*ret, str_dup_to_var("skewness"), Var::new_float(boost::accumulators::skewness(acc)));
    *ret = mapinsert(*ret, str_dup_to_var("sum"), Var::new_float(boost::accumulators::sum(acc)));
    *ret = mapinsert(*ret, str_dup_to_var("variance"), Var::new_float(boost::accumulators::variance(acc)));
}

static package
bf_collect_stats(Var arglist, Byte next, void *vdata, Objid progr)
{
    char *human_string = nullptr;
    asprintf(&human_string, "collecting stats for %" PRIdN " element list", arglist.v.list[1].v.list[0].v.num);
    return background_thread(collect_stats_callback, &arglist, human_string);
}

static inline double distance3(const double x1, const double y1, const double z1, const double x2, const double y2, const double z2)
{
    double result = 0;
    result = pow(x1-x2, 2);
    result += pow(y1-y2, 2);
    result += pow(z1-z2, 2);
    return sqrt(result);
}

/**
* mdistance({x,y,z}, {{x,y,z,unused},{x,y,z,unused}...}, cutoff) => {{distance,unused}, {distance,unused}}
*/
static package
bf_mdistance(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (arglist.v.list[1].v.list[0].v.num != 3)
        {
            free_var(arglist);
            return make_error_pack(E_RANGE);
        }

    for (auto index = 1; index <= 3; ++index)
        {
            const auto valueType = (arglist.v.list[1].v.list[index].type);

            if (valueType != TYPE_INT && valueType != TYPE_FLOAT)
                {
                    free_var(arglist);
                    return make_error_pack(E_TYPE);
                }
        }

    const auto count = arglist.v.list[2].v.list[0].v.num;
//sanity check the list of objects first.
    for (auto index = 1; index <= count; ++index)
        {
 
            if (arglist.v.list[2].v.list[index].type != TYPE_LIST)    
            {
                    free_var(arglist);
                    return make_error_pack(E_TYPE);
                }
      
     if (arglist.v.list[2].v.list[index].v.list[0].v.num < 3)
                {
                    free_var(arglist);
                    return make_error_pack(E_RANGE);
                }
            for (auto value = 1; value <= 3; ++value)
                {
                    const auto valueType = arglist.v.list[2].v.list[index].v.list[value].type;
                    if (valueType != TYPE_INT && valueType != TYPE_FLOAT)
                        {
                            free_var(arglist);
                            return make_error_pack(E_TYPE);
                        }
                }
        }

    const double x1 = (arglist.v.list[1].v.list[1].type == TYPE_INT?
                       (float)(arglist.v.list[1].v.list[1].v.num)
                       :arglist.v.list[1].v.list[1].v.fnum);
    const double y1 = (arglist.v.list[1].v.list[2].type == TYPE_INT?
                       (float)(arglist.v.list[1].v.list[2].v.num)
                       :arglist.v.list[1].v.list[2].v.fnum);
    const double z1 = (arglist.v.list[1].v.list[3].type == TYPE_INT?
                       (float)(arglist.v.list[1].v.list[3].v.num)
                       :arglist.v.list[1].v.list[3].v.fnum);
    const double cutoff = (arglist.v.list[3].type == TYPE_INT?
                           (float)(arglist.v.list[3].v.num)
                           :arglist.v.list[3].v.fnum);

    Var distances = new_list(0);

    for (auto index = 1; index <= count; ++index)
        {
            const double x2 = (arglist.v.list[2].v.list[index].v.list[1].type == TYPE_INT?
                               (float)(arglist.v.list[2].v.list[index].v.list[1].v.num)
                               :arglist.v.list[2].v.list[index].v.list[1].v.fnum);
            const double y2 = (arglist.v.list[2].v.list[index].v.list[2].type == TYPE_INT?
                               (float)(arglist.v.list[2].v.list[index].v.list[2].v.num)
                               :arglist.v.list[2].v.list[index].v.list[2].v.fnum);
            const double z2 = (arglist.v.list[2].v.list[index].v.list[3].type == TYPE_INT?
                               (float)(arglist.v.list[2].v.list[index].v.list[3].v.num)
                               :arglist.v.list[2].v.list[index].v.list[3].v.fnum);
            const auto distance = distance3(x1, y1, z1, x2, y2, z2);
            if (distance > cutoff)
                continue;

            Var result = new_list(0);
            result = listappend(result, Var::new_float(distance));

//now append extra data from the list.
            const auto listSize = arglist.v.list[2].v.list[index].v.list[0].v.num;
            if (listSize > 3)
                {
                    for (auto secondIndex = 4; secondIndex <= listSize; ++secondIndex)
                        {
                            result = listappend(result, var_dup(arglist.v.list[2].v.list[index].v.list[secondIndex]));
                        }
                }
            distances=listappend(distances, result);
        }

    free_var(arglist);
    return make_var_pack(distances);
}

struct AlistCompare
{
    AlistCompare(const Var *Arglist, const bool Natural, const int index) :
        m_Arglist(Arglist), m_Natural(Natural), m_index(index)
    {
    }

    bool operator()(const size_t a, const size_t b) const
    {
        const Var lhs = m_Arglist[a].v.list[m_index];
        const Var rhs = m_Arglist[b].v.list[m_index];

        switch (rhs.type)
            {
            case TYPE_INT:
                return lhs.v.num < rhs.v.num;
            case TYPE_FLOAT:
                return lhs.v.fnum < rhs.v.fnum;
            case TYPE_OBJ:
                return lhs.v.obj < rhs.v.obj;
            case TYPE_ERR:
                return ((int) lhs.v.err) < ((int) rhs.v.err);
            case TYPE_STR:
                return (m_Natural ? strnatcasecmp(lhs.v.str, rhs.v.str) : strcasecmp(lhs.v.str, rhs.v.str)) < 0;
            default:
                errlog("Unknown type in alist sort compare: %d\n", rhs.type);
                return 0;
            }
    }
    const Var *m_Arglist;
    const bool m_Natural;
    const int m_index;
};

static void sort_alist_callback(Var arglist, Var *ret)
{
    const int nargs = arglist.v.list[0].v.num;
    const int list_to_sort = (nargs >= 2 && arglist.v.list[2].v.list[0].v.num > 0 ? 2 : 1);
    const int index = (nargs >= 3 ? arglist.v.list[3].v.num : 1);
    const bool natural = (nargs >= 4 && is_true(arglist.v.list[4]));
    const bool reverse = (nargs >= 5 && is_true(arglist.v.list[5]));

    if (arglist.v.list[list_to_sort].v.list[0].v.num == 0)
        {
            *ret = new_list(0);
            return;
        }
    else if (list_to_sort == 2 && arglist.v.list[1].v.list[0].v.num != arglist.v.list[2].v.list[0].v.num)
        {
            ret->type = TYPE_ERR;
            ret->v.err = E_INVARG;
            return;
        }
    else if (index < 1)
        {
            ret->type = TYPE_ERR;
            ret->v.err = E_RANGE;
            return;
        }

    const Num list_length = arglist.v.list[list_to_sort].v.list[0].v.num;
    // Create and sort a vector of indices rather than values. This makes it easier to sort a list by another list.
    vector<size_t> values(list_length);
//validate list.
    for (size_t count = 1; count <= list_length; ++count)
        {
            if (arglist.v.list[list_to_sort].v.list[count].type != TYPE_LIST)
                {
                    ret->type = TYPE_ERR;
                    ret->v.err = E_INVARG;
                    return;
                }
            else if (arglist.v.list[list_to_sort].v.list[count].v.list[0].v.num < index)
                {
                    ret->type = TYPE_ERR;
                    ret->v.err = E_RANGE;
                    return;
                }
            values[count-1] = count;
        }

//this means that we need a second loop through the list, but it makes the code more clean
    const auto first_type = arglist.v.list[list_to_sort].v.list[1].v.list[index].type;
    for (size_t count = 2; count <= list_length; ++count)
        {
            const auto type = arglist.v.list[list_to_sort].v.list[count].v.list[index].type;
            if (type != first_type || type == TYPE_LIST || type == TYPE_MAP || type == TYPE_ANON || type == TYPE_WAIF)
                {
                    ret->type = TYPE_ERR;
                    ret->v.err = E_TYPE;
                    return;
                }
        }

    sort(values.begin(), values.end(), AlistCompare(arglist.v.list[list_to_sort].v.list, natural, index));
    if (reverse)
        std::reverse(begin(values), end(values));

    *ret = new_list(values.size());
    int moo_list_pos = 0;
    for (const auto &it : values)
        {
            ret->v.list[++moo_list_pos] = list_dup(arglist.v.list[1].v.list[it]);
        }
}

static package
bf_sort_alist(Var arglist, Byte next, void *vdata, Objid progr)
{
    char *human_string = nullptr;
    asprintf(&human_string, "sorting %" PRIdN " element alist", arglist.v.list[1].v.list[0].v.num);

    return background_thread(sort_alist_callback, &arglist, human_string);
}

static int FilterCharactersWithCallback(const Var& string, IsCharCallback callback)
{
    const auto length = memo_strlen(string.v.str);
    if (length == 0)
        {
            return 0;
        }

    const char* stringData = string.v.str;
    for (int i = 0; i < length; ++i)
        {
            if (!callback(stringData[i]))
                {
                    return 0;
                }
        }

    return 1;
}

static package
bf_isalnum(Var arglist, Byte next, void *vdata, Objid progr)
{
    const auto result = FilterCharactersWithCallback(arglist.v.list[1], isalnum);
    free_var(arglist);
    return make_var_pack(Var::new_int(result));
}

static package
bf_isalpha(Var arglist, Byte next, void *vdata, Objid progr)
{
    const auto result = FilterCharactersWithCallback(arglist.v.list[1], isalpha);
    free_var(arglist);
    return make_var_pack(Var::new_int(result));
}

static package
bf_isdigit(Var arglist, Byte next, void *vdata, Objid progr)
{
    const auto result = FilterCharactersWithCallback(arglist.v.list[1], isdigit);
    free_var(arglist);
    return make_var_pack(Var::new_int(result));
}

static package
bf_isprint(Var arglist, Byte next, void *vdata, Objid progr)
{
    const auto result = FilterCharactersWithCallback(arglist.v.list[1], isprint);
    free_var(arglist);
    return make_var_pack(Var::new_int(result));
}

static package
bf_ispunct(Var arglist, Byte next, void *vdata, Objid progr)
{
    const auto result = FilterCharactersWithCallback(arglist.v.list[1], ispunct);
    free_var(arglist);
    return make_var_pack(Var::new_int(result));
}

/**
*When you have a set of nested maps, it's faster to be able to call this builtin.
* map_get_recursive({"a", "b", "c"}, ["a"->["b"->["c"->32]]]) => 32;
*/
static package
bf_map_get_recursive(Var arglist, Byte next, void *vdata, Objid progr)
{
    const auto keycount  = arglist.v.list[1].v.list[0].v.num;
    if (!keycount)
        {
            free_var(arglist);
            return make_error_pack(E_RANGE);
        }

    for (int i = 1; i <= keycount; ++i)
        {
            const auto type = arglist.v.list[1].v.list[i].type;
            if (type != TYPE_STR && type != TYPE_INT && type != TYPE_FLOAT)
                {
                    free_var(arglist);
                    return make_error_pack(E_RANGE);
                }
        }

    Var value;
    auto node = maplookup(arglist.v.list[2], arglist.v.list[1].v.list[1], &value, 0);
    if (!node)
        {
            free_var(arglist);
            return make_error_pack(E_RANGE);
        }
    for (int i = 2; i <= keycount; ++i)
        {
            if (value.type != TYPE_MAP)
                {
                    free_var(arglist);
                    return make_error_pack(E_INVARG);
                }
            node = maplookup(value, arglist.v.list[1].v.list[i], &value, 0);
            if (!node)
                {
                    free_var(arglist);
                    return make_error_pack(E_RANGE);
                }
        }

    value = var_ref(value);
    free_var(arglist);
    return make_var_pack(value);
}

void register_sorressean_extensions()
{
    register_function("assoc", 2, 3, bf_assoc, TYPE_ANY, TYPE_LIST, TYPE_INT);
    register_function("iassoc", 2, 3, bf_iassoc, TYPE_ANY, TYPE_LIST, TYPE_INT);
    register_function("maphasvalue", 2, 2, bf_maphasvalue, TYPE_MAP, TYPE_ANY);
    register_function("intersection", 1, -1, bf_intersection, TYPE_LIST);
    register_function("difference", 1, -1, bf_diff, TYPE_LIST);
    register_function("union", 1, -1, bf_union, TYPE_LIST);
    register_function("set_merge", 2, 2, bf_set_merge, TYPE_LIST, TYPE_LIST);
    register_function("listflatten", 1, 1, bf_list_flatten, TYPE_LIST);
    register_function("join", 1, 2, bf_join, TYPE_LIST, TYPE_STR);
    register_function("listremove_duplicates", 1, 1, bf_list_remove_duplicates, TYPE_LIST);
    register_function("all_contents", 1, 1, bf_all_contents, TYPE_OBJ);
    register_function("bit_or", 2, 2, bf_bit_or, TYPE_INT, TYPE_INT);
    register_function("bit_and", 2, 2, bf_bit_and, TYPE_INT, TYPE_INT);
    register_function("bit_xor", 2, 2, bf_bit_xor, TYPE_INT, TYPE_INT);
    register_function("bit_not", 1, 1, bf_bit_not, TYPE_INT);
    register_function("clamp", 3, 3, bf_clamp, TYPE_NUMERIC, TYPE_NUMERIC, TYPE_NUMERIC);
    register_function("collect_stats", 1, 1, bf_collect_stats, TYPE_LIST);
    register_function("mdistance", 3, 3, bf_mdistance, TYPE_LIST, TYPE_LIST, TYPE_FLOAT);
    register_function("sort_alist", 1, 5, bf_sort_alist, TYPE_LIST, TYPE_LIST, TYPE_INT, TYPE_INT, TYPE_INT);
    register_function("isalnum", 1, 1, bf_isalnum, TYPE_STR);
    register_function("isalpha", 1, 1, bf_isalpha, TYPE_STR);
    register_function("isdigit", 1, 1, bf_isdigit, TYPE_STR);
    register_function("isprint", 1, 1, bf_isprint, TYPE_STR);
    register_function("ispunct", 1, 1, bf_ispunct, TYPE_STR);
    register_function("map_get_recursive", 2, 2, bf_map_get_recursive, TYPE_LIST, TYPE_MAP);
}
