/**
* While not all of these extensions are mine, this file exists to separate the functions I add to this fork away from the lisdude extensions.
* This should theoretically mean that life doesn't break when I merge every time.
*/
#include "collection.h" //ismember
#include "functions.h"      // register builtins
#include "list.h" //listappend, etc
#include "log.h"            // oklog()
#include "map.h" //mapforeach, etc
#include "utils.h" //free_var plus many others

static Var
list_assoc(Var vtarget, Var vlist, const int vindex)
{
    const int length = vlist.v.list[0].v.num;
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
list_iassoc(Var vtarget, Var vlist, const int vindex)
{
    const int length = vlist.v.list[0].v.num;
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
    const int index = (arglist.v.list[0].v.num == 3?arglist.v.list[3].v.num : 1);
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
    const int index = (arglist.v.list[0].v.num == 3 ? arglist.v.list[3].v.num : 1);
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
    const int result = mapforeach(arglist.v.list[1], do_maphasvalue, &arglist.v.list[2]);
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
    for (int index = 1; index <= arglist.v.list[2].v.list[0].v.num; ++index)
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
bf_list_reverse(Var arglist, Byte next, void *vdata, Objid progr)
{
    const int length = arglist.v.list[1].v.list[0].v.num;
    if (length <= 0)
        {
            free_var(arglist);
            return make_error_pack(E_RANGE);
        }

    Var reversed = new_list(0);
    for (int i = length; i >= 1; --i)
        {
            Var element = var_ref(arglist.v.list[1].v.list[i]);
            reversed = listappend(reversed, element);
        }

    free_var(arglist);
    return make_var_pack(reversed);
}

static package bf_bit_or(Var arglist, Byte next, void *vdata, Objid progr)
{
    int a = arglist.v.list[1].v.num;
    int b = arglist.v.list[2].v.num;
    free_var(arglist);

    return make_var_pack(Var::new_int(a|b));
}
static package bf_bit_and(Var arglist, Byte next, void *vdata, Objid progr)
{
    int a = arglist.v.list[1].v.num;
    int b = arglist.v.list[2].v.num;
    free_var(arglist);

    return make_var_pack(Var::new_int(a&b));
}
static package bf_bit_xor(Var arglist, Byte next, void *vdata, Objid progr)
{
    int a = arglist.v.list[1].v.num;
    int b = arglist.v.list[2].v.num;
    free_var(arglist);

    return make_var_pack(Var::new_int(a^b));
}
static package bf_bit_not(Var arglist, Byte next, void *vdata, Objid progr)
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

static unsigned int list_vectorize(const Var& list, Var& values, unsigned  int position = 1)
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

static package bf_list_flatten(Var arglist, Byte next, void *vdata, Objid progr)
{
const auto all_elements = count_all_list_elements(arglist.v.list[1]);
auto ret = new_list(all_elements);
list_vectorize(arglist.v.list[1], ret);
free_var(arglist);

return make_var_pack(ret);
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
    register_function("listreverse", 1, 1, bf_list_reverse, TYPE_LIST);
	    register_function("listflatten", 1, 1, bf_list_flatten, TYPE_LIST);
    register_function("bit_or", 2, 2, bf_bit_or, TYPE_INT, TYPE_INT);
    register_function("bit_and", 2, 2, bf_bit_and, TYPE_INT, TYPE_INT);
    register_function("bit_xor", 2, 2, bf_bit_xor, TYPE_INT, TYPE_INT);
    register_function("bit_not", 1, 1, bf_bit_not, TYPE_INT);
}
