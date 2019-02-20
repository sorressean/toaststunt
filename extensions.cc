#include <set>
#include <math.h>           // sqrt, atan, round, etc
#include "collection.h" //ismember
#include "functions.h"      // register builtins
#include "db_private.h"
#include "log.h"            // oklog()
#include "map.h"
#include "utils.h"          // streams
#include "numbers.h"        // new_float()
#include "list.h"           // listappend and friends
#include "my-stdlib.h"      // rand()
#include "random.h"         // random() (nowai)
#include "server.h"         // panic()
#include <sys/time.h>       // getrusage
#include <sys/resource.h>   // getrusage
#ifndef __FreeBSD__
    #include <sys/sysinfo.h>    // CPU usage
#endif
#include "extension-background.h"   // Threads
#ifdef __MACH__
#include <mach/clock.h>     // Millisecond time for OS X
#endif

using namespace std;

/**
* On FreeBSD, CLOCK_MONOTONIC_RAW is simply CLOCK_MONOTONIC
*/
#ifdef __FreeBSD__
    #define CLOCK_MONOTONIC_RAW CLOCK_MONOTONIC
#endif
/**
* BSD doesn't support sysinfo.
* There are probably other ways to get CPU info, but for the sake of compilation we'll just return all 0 for CPU usage on BSD for now.
*/
#ifdef __FreeBSD__
    #define _MOO_NO_CPU_USAGE
#endif

/* Returns a float of the time (including milliseconds)
   Optional arguments specify monotonic time; 1: Monotonic. 2. Monotonic raw.
   (seconds since an arbitrary period of time. More useful for timing
   since its not affected by NTP or other time changes.) */
    static package
bf_ftime(Var arglist, Byte next, void *vdata, Objid progr)
{
#ifdef __MACH__
    // OS X only provides SYSTEM_CLOCK for monotonic time, so our arguments don't matter.
    clock_id_t clock_type = (arglist.v.list[0].v.num == 0 ? CALENDAR_CLOCK : SYSTEM_CLOCK);
#else
    // Other OSes provide MONOTONIC_RAW and MONOTONIC, so we'll check args for 2(raw) or 1.
    clockid_t clock_type = 0;
    if (arglist.v.list[0].v.num == 0)
        clock_type = CLOCK_REALTIME;
    else
        clock_type = arglist.v.list[1].v.num == 2 ? CLOCK_MONOTONIC_RAW : CLOCK_MONOTONIC;
#endif

    struct timespec ts;

#ifdef __MACH__
    // OS X lacks clock_gettime, use clock_get_time instead
    clock_serv_t cclock;
    mach_timespec_t mts;
    host_get_clock_service(mach_host_self(), clock_type, &cclock);
    clock_get_time(cclock, &mts);
    mach_port_deallocate(mach_task_self(), cclock);
    ts.tv_sec = mts.tv_sec;
    ts.tv_nsec = mts.tv_nsec;
#else
    clock_gettime(clock_type, &ts);
#endif

    Var r;
    r.type = TYPE_FLOAT;
    r.v.fnum = (double)ts.tv_sec + (double)ts.tv_nsec / 1000000000.0;

    free_var(arglist);
    return make_var_pack(r);
}

/* Locate an object in the database by name more quickly than is possible in-DB. */
    static package
bf_locate_by_name(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_wizard(progr))
    {
        free_var(arglist);
        return make_error_pack(E_PERM);
    }

    Var ret = new_list(0), name, object;
    object.type = TYPE_OBJ;

    int case_matters = is_true(arglist.v.list[2]);
    int string_length = memo_strlen(arglist.v.list[1].v.str);

    for (int x = 1; x < db_last_used_objid(); x++)
    {
        if (!valid(x))
            continue;

        object.v.obj = x;
        db_find_property(object, "name", &name);
        if (strindex(name.v.str, memo_strlen(name.v.str), arglist.v.list[1].v.str, string_length, case_matters))
            ret = listappend(ret, object);
    }

    free_var(arglist);
    return make_var_pack(ret);
}

/* Calculates the distance between two n-dimensional sets of coordinates. */
    static package
bf_distance(Var arglist, Byte next, void *vdata, Objid progr)
{
    double ret = 0.0, tmp = 0.0;
    int count;

    for (count = 1; count <= arglist.v.list[1].v.list[0].v.num; count++)
    {
        if ((arglist.v.list[1].v.list[count].type != TYPE_INT && arglist.v.list[1].v.list[count].type != TYPE_FLOAT) || (arglist.v.list[2].v.list[count].type != TYPE_INT && arglist.v.list[2].v.list[count].type != TYPE_FLOAT))
        {
            free_var(arglist);
            return make_error_pack(E_TYPE);
        }
        else
        {
            tmp = (arglist.v.list[2].v.list[count].type == TYPE_INT ? (double)arglist.v.list[2].v.list[count].v.num : arglist.v.list[2].v.list[count].v.fnum) - (arglist.v.list[1].v.list[count].type == TYPE_INT ? (double)arglist.v.list[1].v.list[count].v.num : arglist.v.list[1].v.list[count].v.fnum);
            ret = ret + (tmp * tmp);
        }
    }

    free_var(arglist);

    Var s;
    s.type = TYPE_FLOAT;
    s.v.fnum = sqrt(ret);

    return make_var_pack(s);
}

/* Calculates the bearing between two sets of three dimensional floating point coordinates. */
    static package
bf_relative_heading(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (arglist.v.list[1].v.list[1].type != TYPE_FLOAT || arglist.v.list[1].v.list[2].type != TYPE_FLOAT || arglist.v.list[1].v.list[3].type != TYPE_FLOAT || arglist.v.list[2].v.list[1].type != TYPE_FLOAT || arglist.v.list[2].v.list[2].type != TYPE_FLOAT || arglist.v.list[2].v.list[3].type != TYPE_FLOAT) {
        free_var(arglist);
        return make_error_pack(E_TYPE);
    }

    double dx = arglist.v.list[2].v.list[1].v.fnum - arglist.v.list[1].v.list[1].v.fnum;
    double dy = arglist.v.list[2].v.list[2].v.fnum - arglist.v.list[1].v.list[2].v.fnum;
    double dz = arglist.v.list[2].v.list[3].v.fnum - arglist.v.list[1].v.list[3].v.fnum;

    double xy = 0.0;
    double z = 0.0;

    xy = atan2(dy, dx) * 57.2957795130823;

    if (xy < 0.0)
        xy = xy + 360.0;

    z = atan2(dz, sqrt((dx * dx) + (dy * dy))) * 57.2957795130823;

    Var s = new_list(2);
    s.v.list[1].type = TYPE_INT;
    s.v.list[1].v.num = (int)xy;
    s.v.list[2].type = TYPE_INT;
    s.v.list[2].v.num = (int)z;

    free_var(arglist);

    return make_var_pack(s);
}

/* Returns total memory usage, resident set size, shared pages, text/code, and data + stack. */
    static package
bf_memory_usage(Var arglist, Byte next, void *vdata, Objid progr)
{
    // Values are returned in pages. To get KB, multiply by 4.
    free_var(arglist);

    long double size, resident, share, text, lib, data, dt;

    FILE *f = fopen("/proc/self/statm", "r");

    if (!f)
        return make_error_pack(E_NACC);

    if (fscanf(f, "%Lf %Lf %Lf %Lf %Lf %Lf %Lf",
                &size, &resident, &share, &text, &lib, &data, &dt) != 7)
    {
        fclose(f);
        return make_error_pack(E_NACC);
    }

    fclose(f);

    Var s = new_list(5);
    s.v.list[1].type = TYPE_FLOAT;
    s.v.list[2].type = TYPE_FLOAT;
    s.v.list[3].type = TYPE_FLOAT;
    s.v.list[4].type = TYPE_FLOAT;
    s.v.list[5].type = TYPE_FLOAT;
    s.v.list[1].v.fnum = size;           // Total program size
    s.v.list[2].v.fnum = resident;       // Resident set size
    s.v.list[3].v.fnum = share;          // Shared pages from shared mappings
    s.v.list[4].v.fnum = text;           // Text (code)
    s.v.list[5].v.fnum = data;           // Data + stack

    return make_var_pack(s);
}

/* Return resource usage information from the operating system.
 * Values returned: {{load averages}, user time, system time, page reclaims, page faults, block input ops, block output ops, voluntary context switches, involuntary context switches, signals received
 * Divide load averages by 65536. */
    static package
bf_usage(Var arglist, Byte next, void *vdata, Objid progr)
{
    free_var(arglist);
    if (!is_wizard(progr))
        return make_error_pack(E_PERM);

    Var r = new_list(9);
    Var cpu = new_list(3);

    // Setup all of our types ahead of time.
    int x = 0;
    for (x = 3; x <= r.v.list[0].v.num; x++)
        r.v.list[x].type = TYPE_INT;

    for (x = 1; x <= 3; x++)
        cpu.v.list[x] = Var::new_int(0); //initialize to all 0

    #ifndef _MOO_NO_CPU_USAGE
    /*** Begin CPU load averages ***/
        struct sysinfo sys_info;
        int info_ret = sysinfo(&sys_info);

        for (x = 0; x < 3; x++)
            cpu.v.list[x+1].v.num = (info_ret != 0 ? 0 : sys_info.loads[x]);
    #endif
    
    /*** Now rusage ***/
    struct rusage usage;
    getrusage(RUSAGE_SELF, &usage);

    r.v.list[1].type = TYPE_FLOAT;
    r.v.list[2].type = TYPE_FLOAT;
    r.v.list[1].v.fnum =(double)usage.ru_utime.tv_sec + (double)usage.ru_utime.tv_usec / CLOCKS_PER_SEC;
    r.v.list[2].v.fnum = (double)usage.ru_stime.tv_sec + (double)usage.ru_stime.tv_usec / CLOCKS_PER_SEC;
    r.v.list[3].v.num = usage.ru_minflt;
    r.v.list[4].v.num = usage.ru_majflt;
    r.v.list[5].v.num = usage.ru_inblock;
    r.v.list[6].v.num = usage.ru_oublock;
    r.v.list[7].v.num = usage.ru_nvcsw;
    r.v.list[8].v.num = usage.ru_nivcsw;
    r.v.list[9].v.num = usage.ru_nsignals;

    // Add in our load averages.
    r = listinsert(r, cpu, 1);
    return make_var_pack(r);
}

/* Unceremoniously exit the server, creating a panic dump of the database. */
    static package
bf_panic(Var arglist, Byte next, void *vdata, Objid progr)
{
    const char *msg;

    if(!is_wizard(progr)) {
        free_var(arglist);
        return make_error_pack(E_PERM);
    }

    if(arglist.v.list[0].v.num) {
        msg=str_dup(arglist.v.list[1].v.str);
    } else {
        msg="";
    }

    free_var(arglist);
    panic(msg);

    return make_error_pack(E_NONE);
}

/* Return a random floating point value between 0.0..args[1] or args[1]..args[2] */
    static package
bf_frandom(Var arglist, Byte next, void *vdata, Objid progr)
{
    double fmin = (arglist.v.list[0].v.num > 1 ? arglist.v.list[1].v.fnum : 0.0);
    double fmax = (arglist.v.list[0].v.num > 1 ? arglist.v.list[2].v.fnum : arglist.v.list[1].v.fnum);

    free_var(arglist);

    double f = (double)rand() / RAND_MAX;
    f = fmin + f * (fmax - fmin);

    Var ret;
    ret.type = TYPE_FLOAT;
    ret.v.fnum = f;

    return make_var_pack(ret);

}

/* Round numbers to the nearest integer value to args[1] */
    static package
bf_round(Var arglist, Byte next, void *vdata, Objid progr)
{
    double r = round((double)arglist.v.list[1].v.fnum);

    free_var(arglist);

    Var ret;
    ret.type = TYPE_FLOAT;
    ret.v.fnum = r;

    return make_var_pack(ret);
}

/* Return a list of substrings of an argument separated by a break. */
    static package
bf_explode(Var arglist, Byte next, void *vdata, Objid progr)
{
    int nargs = arglist.v.list[0].v.num;
    Stream *brk = new_stream(2);
    stream_add_string(brk, (nargs > 1) ? arglist.v.list[2].v.str : " ");
    Var r;

    if (strcmp(stream_contents(brk), "") == 0) {
        // Do we want to break it into letters here?
        r.type = TYPE_ERR;
        r.v.err = E_INVARG;
    } else {
        r = new_list(0);
        int i, l = stream_length(brk);
        Stream *tmp = new_stream(memo_strlen(arglist.v.list[1].v.str)+1);
        stream_add_string(tmp, arglist.v.list[1].v.str);
        stream_add_string(tmp, stream_contents(brk));

        Var subject;
        subject.type = TYPE_STR;
        subject.v.str = str_dup(reset_stream(tmp));
        free_stream(tmp);

        while (memo_strlen(subject.v.str)) {
            if ((i = strindex(subject.v.str, memo_strlen(subject.v.str), stream_contents(brk), stream_length(brk), 0)) > 1) {
                r = listappend(r, substr(var_dup(subject), 1, i - 1));
            }
            subject = substr(subject, i + l, memo_strlen(subject.v.str));
        }
        free_var(subject);
    }
    free_var(arglist);
    free_stream(brk);
    return make_var_pack(r);
}

/* Return a list of objects of parent, optionally with a player flag set.
 * With only one argument, player flag is assumed to be the only condition.
 * With two arguments, parent is the only condition.
 * With three arguments, parent is checked first and then the player flag is checked.
 * occupants(LIST objects, OBJ parent, ?INT player flag set)
 */
static package
bf_occupants(Var arglist, Byte next, void *vdata, Objid progr)
{				/* (object) */
    Var ret = new_list(0);
    int nargs = arglist.v.list[0].v.num;
    Var contents = arglist.v.list[1];
    int content_length = contents.v.list[0].v.num;
    bool check_parent = nargs == 1 ? false : true;
    Var parent = check_parent ? arglist.v.list[2] : nothing;
    bool check_player_flag = (nargs == 1 || (nargs > 2 && is_true(arglist.v.list[3])));

    for (int x = 1; x <= content_length; x++) {
        Objid oid = contents.v.list[x].v.obj;
        if (valid(oid)
            && (!check_parent ? 1 : db_object_isa(Var::new_obj(oid), parent))
            && (!check_player_flag || (check_player_flag && is_user(oid))))
        {
            ret = setadd(ret, Var::new_obj(oid));
        }
    }

    free_var(arglist);
    return make_var_pack(ret);
}

/* Return a list of nested locations for an object
 * For objects in $nothing (#-1), this returns an empty list.
 * locations(OBJ object)
 */
static package
bf_locations(Var arglist, Byte next, void *vdata, Objid progr)
{    
    Objid what = arglist.v.list[1].v.obj;

    free_var(arglist);

    if (!valid(what))
        return make_error_pack(E_INVIND);

    Var locs = new_list(0);

    Objid loc = db_object_location(what);

    while (valid(loc)) {
        locs = setadd(locs, Var::new_obj(loc));
        loc = db_object_location(loc);
    }

    return make_var_pack(locs);
}

/* Return a symbol for the ASCII value associated. */
static package
bf_chr(Var arglist, Byte next, void *vdata, Objid progr)
{
    Var r;
    char str[2];

    switch (arglist.v.list[1].type) {
        case TYPE_INT:
            if ((arglist.v.list[1].v.num < 1) || (arglist.v.list[1].v.num > 255)) {
                free_var(arglist);
                return make_error_pack(E_INVARG);
            } else if (arglist.v.list[1].v.num < 32 && !is_wizard(progr)) {
                free_var(arglist);
                return make_error_pack(E_PERM);
            }
            str[0] = (char) arglist.v.list[1].v.num;
            str[1] = '\0';
            r.type = TYPE_STR;
            r.v.str = str_dup(str);
            break;
        case TYPE_STR:
            if (!(r.v.num = (int) arglist.v.list[1].v.str[0])) {
                free_var(arglist);
                return make_error_pack(E_INVARG);
            }
            r.type = TYPE_INT;
            break;
        default:
            free_var(arglist);
            return make_error_pack(E_TYPE);
    }

    free_var(arglist);
    return make_var_pack(r);
}

// ============= ANSI ===============
    static package
bf_parse_ansi(Var arglist, Byte next, void *vdata, Objid progr)
{
#define ANSI_TAG_TO_CODE(tag, code, case_matters)			\
    {									\
        stream_add_strsub(str, reset_stream(tmp), tag, code, case_matters); \
        stream_add_string(tmp, reset_stream(str));				\
    }

    Var r;
    r.type = TYPE_STR;

    Stream *str = new_stream(50);
    Stream *tmp = new_stream(50);
    const char *random_codes[] = {"\e[31m", "\e[32m", "\e[33m", "\e[34m", "\e[35m", "\e[35m", "\e[36m"};

    stream_add_string(tmp, arglist.v.list[1].v.str);
    free_var(arglist);

    ANSI_TAG_TO_CODE("[red]",        "\e[31m",   0);
    ANSI_TAG_TO_CODE("[green]",      "\e[32m",   0);
    ANSI_TAG_TO_CODE("[yellow]",     "\e[33m",   0);
    ANSI_TAG_TO_CODE("[blue]",       "\e[34m",   0);
    ANSI_TAG_TO_CODE("[purple]",     "\e[35m",   0);
    ANSI_TAG_TO_CODE("[cyan]",       "\e[36m",   0);
    ANSI_TAG_TO_CODE("[normal]",     "\e[0m",    0);
    ANSI_TAG_TO_CODE("[inverse]",    "\e[7m",    0);
    ANSI_TAG_TO_CODE("[underline]",  "\e[4m",    0);
    ANSI_TAG_TO_CODE("[bold]",       "\e[1m",    0);
    ANSI_TAG_TO_CODE("[bright]",     "\e[1m",    0);
    ANSI_TAG_TO_CODE("[unbold]",     "\e[22m",   0);
    ANSI_TAG_TO_CODE("[blink]",      "\e[5m",    0);
    ANSI_TAG_TO_CODE("[unblink]",    "\e[25m",   0);
    ANSI_TAG_TO_CODE("[magenta]",    "\e[35m",   0);
    ANSI_TAG_TO_CODE("[unbright]",   "\e[22m",   0);
    ANSI_TAG_TO_CODE("[white]",      "\e[37m",   0);
    ANSI_TAG_TO_CODE("[gray]",       "\e[1;30m", 0);
    ANSI_TAG_TO_CODE("[grey]",       "\e[1;30m", 0);
    ANSI_TAG_TO_CODE("[beep]",       "\a",       0);
    ANSI_TAG_TO_CODE("[black]",      "\e[30m",   0);
    ANSI_TAG_TO_CODE("[b:black]",   "\e[40m",   0);
    ANSI_TAG_TO_CODE("[b:red]",     "\e[41m",   0);
    ANSI_TAG_TO_CODE("[b:green]",   "\e[42m",   0);
    ANSI_TAG_TO_CODE("[b:yellow]",  "\e[43m",   0);
    ANSI_TAG_TO_CODE("[b:blue]",    "\e[44m",   0);
    ANSI_TAG_TO_CODE("[b:magenta]", "\e[45m",   0);
    ANSI_TAG_TO_CODE("[b:purple]",  "\e[45m",   0);
    ANSI_TAG_TO_CODE("[b:cyan]",    "\e[46m",   0);
    ANSI_TAG_TO_CODE("[b:white]",   "\e[47m",   0);

    char *t = reset_stream(tmp);
    while (*t) {
        if (!strncasecmp(t, "[random]", 8)) {
            stream_add_string(str, random_codes[RANDOM() % 6]);
            t += 8;
        } else
            stream_add_char(str, *t++);
    }

    stream_add_strsub(tmp, reset_stream(str), "[null]", "", 0);

    ANSI_TAG_TO_CODE("[null]", "", 0);

    r.v.str = str_dup(reset_stream(tmp));

    free_stream(tmp);
    free_stream(str);
    return make_var_pack(r);

#undef ANSI_TAG_TO_CODE
}


    static package
bf_remove_ansi(Var arglist, Byte next, void *vdata, Objid progr)
{

#define MARK_FOR_REMOVAL(tag)					\
    {								\
        stream_add_strsub(tmp, reset_stream(tmp), tag, "", 0);	\
    }
    Var r;
    Stream *tmp;

    tmp = new_stream(50);
    stream_add_string(tmp, arglist.v.list[1].v.str);
    free_var(arglist);

    MARK_FOR_REMOVAL("[red]");
    MARK_FOR_REMOVAL("[green]");
    MARK_FOR_REMOVAL("[yellow]");
    MARK_FOR_REMOVAL("[blue]");
    MARK_FOR_REMOVAL("[purple]");
    MARK_FOR_REMOVAL("[cyan]");
    MARK_FOR_REMOVAL("[normal]");
    MARK_FOR_REMOVAL("[inverse]");
    MARK_FOR_REMOVAL("[underline]");
    MARK_FOR_REMOVAL("[bold]");
    MARK_FOR_REMOVAL("[bright]");
    MARK_FOR_REMOVAL("[unbold]");
    MARK_FOR_REMOVAL("[blink]");
    MARK_FOR_REMOVAL("[unblink]");
    MARK_FOR_REMOVAL("[magenta]");
    MARK_FOR_REMOVAL("[unbright]");
    MARK_FOR_REMOVAL("[white]");
    MARK_FOR_REMOVAL("[gray]");
    MARK_FOR_REMOVAL("[grey]");
    MARK_FOR_REMOVAL("[beep]");
    MARK_FOR_REMOVAL("[black]");
    MARK_FOR_REMOVAL("[b:black]");
    MARK_FOR_REMOVAL("[b:red]");
    MARK_FOR_REMOVAL("[b:green]");
    MARK_FOR_REMOVAL("[b:yellow]");
    MARK_FOR_REMOVAL("[b:blue]");
    MARK_FOR_REMOVAL("[b:magenta]");
    MARK_FOR_REMOVAL("[b:purple]");
    MARK_FOR_REMOVAL("[b:cyan]");
    MARK_FOR_REMOVAL("[b:white]");
    MARK_FOR_REMOVAL("[random]");
    MARK_FOR_REMOVAL("[null]");

    r.type = TYPE_STR;
    r.v.str = str_dup(reset_stream(tmp));

    free_stream(tmp);
    return make_var_pack(r);

#undef MARK_FOR_REMOVAL
}
//==============================================================

static void do_deep_contents(set<Objid> &objids, Var &branch, Var& parent, bool perform_isa=false)
{
    if (branch.type != TYPE_OBJ)
        return;

    auto objid = branch.v.obj;
    if (!valid(objid))
        return;
    auto branch_obj = dbpriv_find_object(objid);
    if (!branch_obj)
        return;

    if (perform_isa)
        {
            if (db_object_isa(branch, parent))
                {
                    objids.insert(branch.v.obj);
                }
        }
    else
        {
            objids.insert(branch.v.obj);
        }

    for (int i = 1; i <= branch_obj->contents.v.list[0].v.num; ++i)
        {
            do_deep_contents(objids, branch_obj->contents.v.list[i], parent, perform_isa);
        }

    return;
}

/**
* Deep_contents
* This function retrieves recursively the contents of all objects and merges them into one set.
* Pass as a second argument an object if you want an isa to be used.
* For example, deep_contents(#1234, $player)
* will retrieve all players nested in that object.
* A few notes:
* We use std::set to make sure that we are performing set operations.
* This probably isn't needed; if you've got an object appearing in two different contents chances are something went wrong, but better safe than sorry.
* This set also serves the duel purpose of constructing a set without having to call setadd multiple times on a MOO list.
* This is much quicker because we can keep adding to this and then get the final count of elements as the size of the list, which requires less reallocation of the list itself.
*/
static package
bf_deep_contents(Var arglist, Byte next, void *vdata, Objid progr)
{
    bool perform_isa=false;
    Objid what = arglist.v.list[1].v.obj;
    if (!valid(what))
        {
            free_var(arglist);
            return make_error_pack(E_INVIND);
        }

    Var parent;
    if (arglist.v.list[0].v.num == 2)
        {
            parent = var_dup(arglist.v.list[2]);
            perform_isa = true;
        }
    else
        {
            parent = var_ref(nothing);
        }

    set<Objid> objids;
    do_deep_contents(objids, arglist.v.list[1], parent, perform_isa);
    objids.erase(what);
    auto set_size = objids.size();
    Var array = new_list(set_size);
    int index = 1;
    for (auto it: objids)
        {
            array = listset(array, Var::new_obj(it), index);
            ++index;
        }

    free_var(arglist);
    free_var(parent);
    return make_var_pack(array);
}

/**
*The next two builtins are going to help with things like this:
* if (!(foo in (mapkeys(bar))))
* etc.
* Simplified down to if (!contains_key(bar, foo))
* contains_value works the exact same way except that it looks for values.
* There is not a performance improvement using in vs contains_key, they both have to iterate over the entire map.
*/
static int do_contains_key(Var key, Var value, void *data, int first)
{
    Var* search = (Var*)data;
    return equality(key, *search, 1);
}
static package bf_contains_key(Var arglist, Byte next, void *vdata, Objid progr)
{
    const int result = mapforeach(arglist.v.list[1], do_contains_key, &arglist.v.list[2]);
    free_var(arglist);

    Var ret;
    ret.type=TYPE_INT;
    ret.v.num = result;
    return make_var_pack(ret);
}

static int do_contains_value(Var key, Var value, void *data, int first)
{
    Var* search = (Var*)data;
    return equality(value, *search, 1);
}
static package bf_contains_value(Var arglist, Byte next, void *vdata, Objid progr)
{
    const int result = mapforeach(arglist.v.list[1], do_contains_value, &arglist.v.list[2]);
    free_var(arglist);

    Var ret;
    ret.type=TYPE_INT;
    ret.v.num = result;
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
            int y = 0;
            int x = 0;
            for (x = 2; x <= arglist.v.list[0].v.num; x++)
                {
                    if (r.v.list[0].v.num < arglist.v.list[x].v.list[0].v.num)
                        {
                            for (y = 1; y <= r.v.list[0].v.num; y++)
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
* The following extensions are taken from martian's list extensions and modified.
*/
static Var list_assoc(Var& vtarget, Var& vlist, const int vindex)
{
    for (int i = 1; i <= vlist.v.list[0].v.num; ++i)
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

static int list_iassoc(Var& vtarget, Var& vlist, const int vindex)
{
    for (int i = 1; i <= vlist.v.list[0].v.num; ++i)
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

static void InitListToZero(Var list)
{
    const Var z = Var::new_int(0);

    for(int i=1; i <= list.v.list[0].v.num; ++i)
        list.v.list[i]=z;
}

static package
bf_iassoc(Var arglist, Byte next, void *vdata, Objid progr)
{
    /* (ANY, LIST[, INT]) */
    int index = 1;
    if (arglist.v.list[0].v.num == 3)
        index = arglist.v.list[3].v.num;

    if (index < 1)
        {
            free_var(arglist);
            return make_error_pack(E_RANGE);
        }

    Var r = Var::new_int(list_iassoc(arglist.v.list[1], arglist.v.list[2], index));

    free_var(arglist);
    return make_var_pack(r);
}

static package
bf_assoc(Var arglist, Byte next, void *vdata, Objid progr)
{
    /* (ANY, LIST[, INT]) */
    int index = 1;
    if (arglist.v.list[0].v.num == 3)
        index = arglist.v.list[3].v.num;

    if (index < 1)
        {
            free_var(arglist);
            return make_error_pack(E_RANGE);
        }

    Var r = list_assoc(arglist.v.list[1], arglist.v.list[2], index);

    free_var(arglist);
    return make_var_pack(r);
}

static package
bf_slice(Var arglist, Byte next, void *vdata, Objid progr)
{
    const int length = arglist.v.list[0].v.num;
    if(length < 0)
        {
            free_var(arglist);
            return make_error_pack(E_INVARG);
        }

    Var ret = new_list(length);
    InitListToZero(ret);

    int c = 0;
    if(arglist.v.list[0].v.num == 2)
        c = arglist.v.list[2].v.num;
    else
        c = 1;

    const Var list=arglist.v.list[1];
    for(int i = 1; i <= length; ++i)
        if( list.v.list[i].type != TYPE_LIST || list.v.list[i].v.list[0].v.num < c )
            {
                free_var(ret);
                free_var(arglist);
                return make_error_pack(E_INVARG);
            }
        else
            {
                ret.v.list[i] = var_dup(list.v.list[i].v.list[c]);
            }

    free_var(arglist);
    return make_var_pack(ret);
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
            newList = setadd(newList, arglist.v.list[2].v.list[index]);
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

    Var reversed = new_list(length);
    int end = length;
    for (int i = 1; i <= length; ++i)
    {
        reversed = listset(reversed, arglist.v.list[1].v.list[i], end);
        --end;
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

    void
register_extensions()
{
    register_function("frandom", 1, 2, bf_frandom, TYPE_FLOAT, TYPE_FLOAT);
    register_function("round", 1, 1, bf_round, TYPE_FLOAT);
    register_function("distance", 2, 2, bf_distance, TYPE_LIST, TYPE_LIST);
    register_function("relative_heading", 2, 2, bf_relative_heading, TYPE_LIST, TYPE_LIST);
    register_function("memory_usage", 0, 0, bf_memory_usage);
    register_function("usage", 0, 0, bf_usage);
    register_function("ftime", 0, 1, bf_ftime, TYPE_INT);
    register_function("panic", 0, 1, bf_panic, TYPE_STR);
    register_function("locate_by_name", 1, 2, bf_locate_by_name, TYPE_STR, TYPE_INT);
    register_function("explode", 1, 2, bf_explode, TYPE_STR, TYPE_STR);
    register_function("occupants", 1, 3, bf_occupants, TYPE_LIST, TYPE_OBJ, TYPE_INT);
    register_function("locations", 1, 1, bf_locations, TYPE_OBJ);
    register_function("chr", 1, 1, bf_chr, TYPE_INT);
    register_function("deep_contents", 1, 2, bf_deep_contents, TYPE_OBJ, TYPE_OBJ);
    register_function("contains_key", 2, 2, bf_contains_key, TYPE_MAP, TYPE_ANY);
    register_function("contains_value", 2, 2, bf_contains_value, TYPE_MAP, TYPE_ANY);
    register_function("intersection", 1, -1, bf_intersection, TYPE_LIST);
    register_function("difference", 1, -1, bf_diff, TYPE_LIST);
    register_function("union", 1, -1, bf_union, TYPE_LIST);
    register_function("iassoc", 2, 3, bf_iassoc, TYPE_ANY, TYPE_LIST, TYPE_INT);
    register_function("assoc", 2, 3, bf_assoc, TYPE_ANY, TYPE_LIST, TYPE_INT);
    register_function("slice", 1, 2, bf_slice, TYPE_LIST, TYPE_INT);
    register_function("set_merge", 2, 2, bf_set_merge, TYPE_LIST, TYPE_LIST);
    register_function("listreverse", 1, 1, bf_list_reverse, TYPE_LIST);
    register_function("bit_or", 2, 2, bf_bit_or, TYPE_INT, TYPE_INT);
    register_function("bit_and", 2, 2, bf_bit_and, TYPE_INT, TYPE_INT);
    register_function("bit_xor", 2, 2, bf_bit_xor, TYPE_INT, TYPE_INT);
    register_function("bit_not", 1, 1, bf_bit_not, TYPE_INT);
    // ======== ANSI ===========
    register_function("parse_ansi", 1, 1, bf_parse_ansi, TYPE_STR);
    register_function("remove_ansi", 1, 1, bf_remove_ansi, TYPE_STR);
}
