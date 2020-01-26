#include "options.h"

#ifdef SQLITE3_FOUND

#include <unordered_map>

#include "sqlite.h"
#include "background.h"
#include "functions.h"
#include "numbers.h"
#include "utils.h"
#include "list.h"
#include "storage.h"
#include "log.h"
#include "server.h"
#include "map.h"

// Map of open connections
static std::unordered_map <int, sqlite_conn> sqlite_connections;
// Next database handle. This will get reset to 1 when all connections get closed.
static int next_sqlite_handle = 1;

/* Open an SQLite database.
 * Args: STR <path to database>, [INT options] */
    static package
bf_sqlite_open(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_wizard(progr))
    {
        free_var(arglist);
        return make_error_pack(E_PERM);
    }

    int index = next_handle();
    if (index == -1)
    {
        // We've exceeded SQLITE_MAX_HANDLES and the $server_options setting and didn't allocate anything.
        free_var(arglist);
        return make_raise_pack(E_QUOTA, "Too many database connections open.", var_ref(zero));
    }

    /* NOTE: This relies on having FileIO. If you don't, you'll need
     *       a function to resolve a SAFE path. */
    const char *path = file_resolve_path(arglist.v.list[1].v.str);
    if (path == nullptr)
    {
        free_var(arglist);
        return make_error_pack(E_INVARG);
    }

    int dup_check = database_already_open(path);
    if (dup_check != -1)
    {
        free_var(arglist);
        char ohno[50];
        sprintf(ohno, "Database already open with handle: %i", dup_check);
        return make_raise_pack(E_INVARG, ohno, var_ref(zero));
    }

    index = allocate_handle();
    sqlite_conn *handle = &sqlite_connections[index];

    if (arglist.v.list[0].v.num >= 2)
        handle->options = arglist.v.list[2].v.num;

    free_var(arglist);

    int rc = sqlite3_open(path, &handle->id);

    if (rc != SQLITE_OK)
    {
        const char *err = sqlite3_errmsg(handle->id);
        deallocate_handle(index, false);
        return make_raise_pack(E_NONE, err, var_ref(zero));
    } else {
        handle->path = str_dup(path);
        Var r;
        r.type = TYPE_INT;
        r.v.num = index;
        return make_var_pack(r);
    }
}

/* Close an SQLite database.
 * Args: INT <database handle> */
    static package
bf_sqlite_close(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_wizard(progr))
    {
        free_var(arglist);
        return make_error_pack(E_PERM);
    }

    int index = arglist.v.list[1].v.num;
    free_var(arglist);

    if (!valid_handle(index))
        return make_raise_pack(E_INVARG, "Invalid database handle", var_ref(zero));

    sqlite_conn *handle = &sqlite_connections[index];
    if (handle->locks > 0)
        return make_raise_pack(E_PERM, "Handle can't be closed until all worker threads are finished", var_ref(zero));

    deallocate_handle(index, false);

    return no_var_pack();
}

/* Return a list of open SQLite database handles. */
    static package
bf_sqlite_handles(Var arglist, Byte next, void *vdata, Objid progr)
{
    free_var(arglist);

    if (!is_wizard(progr))
        return make_error_pack(E_PERM);

    Var r = new_list(sqlite_connections.size());

    int count = 0;
    for (auto& it : sqlite_connections)
        r.v.list[++count] = Var::new_int(it.first);

    return make_var_pack(r);
}

/* Return information about the specified SQLite database handle.
 * Args: <INT database handle> */
    static package
bf_sqlite_info(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_wizard(progr))
    {
        free_var(arglist);
        return make_error_pack(E_PERM);
    }

    int index = arglist.v.list[1].v.num;
    free_var(arglist);

    if (!valid_handle(index))
        return make_error_pack(E_INVARG);

    sqlite_conn *handle = &sqlite_connections[index];

    Var ret = new_map();
    ret = mapinsert(ret, str_dup_to_var("path"), str_dup_to_var(handle->path));
    ret = mapinsert(ret, str_dup_to_var("parse_types"), Var::new_int(handle->options & SQLITE_PARSE_TYPES ? 1 : 0));
    ret = mapinsert(ret, str_dup_to_var("parse_objects"), Var::new_int(handle->options & SQLITE_PARSE_OBJECTS ? 1 : 0));
    ret = mapinsert(ret, str_dup_to_var("sanitize_strings"), Var::new_int(handle->options & SQLITE_SANITIZE_STRINGS ? 1 : 0));
    ret = mapinsert(ret, str_dup_to_var("locks"), Var::new_int(handle->locks));

    return make_var_pack(ret);
}

/* The function responsible for the actual execute call.
 * Contains functionality shared by both the threaded and
 * unthreaded builtins. */
void sqlite_execute_thread_callback(Var args, Var *r)
{
    int index = args.v.list[1].v.num;
    if (!valid_handle(index))
    {
        r->type = TYPE_ERR;
        r->v.err = E_INVARG;
        return;
    }

    const char *query = args.v.list[2].v.str;
    sqlite_conn *handle = &sqlite_connections[index];
    sqlite3_stmt *stmt;

    int rc = sqlite3_prepare_v2(handle->id, query, -1, &stmt, nullptr);
    if (rc != SQLITE_OK)
    {
        const char *err = sqlite3_errmsg(handle->id);
        r->type = TYPE_STR;
        r->v.str = str_dup(err);
        return;
    }

    handle->locks++;

    /* Take args[3] and bind it into the appropriate locations for SQLite
     * (e.g. in the query values (?, ?, ?) args[3] would be {5, "oh", "hello"}) */
    for (int x = 1; x <= args.v.list[3].v.list[0].v.num; x++)
    {
        switch (args.v.list[3].v.list[x].type)
        {
            case TYPE_STR:
                sqlite3_bind_text(stmt, x, args.v.list[3].v.list[x].v.str, -1, nullptr);
                break;
            case TYPE_INT:
                sqlite3_bind_int(stmt, x, args.v.list[3].v.list[x].v.num);
                break;
            case TYPE_FLOAT:
                sqlite3_bind_double(stmt, x, args.v.list[3].v.list[x].v.fnum);
                break;
            case TYPE_OBJ:
                sqlite3_bind_text(stmt, x, str_dup(reset_stream(object_to_string(&args.v.list[3].v.list[x]))),  -1, nullptr);
                break;
        }
    }

    rc = sqlite3_step(stmt);
    int col = sqlite3_column_count(stmt);

    *r = new_list(0);

    while (rc == SQLITE_ROW)
    {
        Var row = new_list(0);
        for (int x = 0; x < col; x++)
        {
            // Ideally we would know the type and use sqlite3_column<TYPE> but we don't!
            char *str = (char*)sqlite3_column_text(stmt, x);

            if (handle->options & SQLITE_SANITIZE_STRINGS)
                sanitize_string_for_moo(str);

            Var s;
            if (!(handle->options & SQLITE_PARSE_TYPES))
            {
                s.type = TYPE_STR;
                s.v.str = str_dup(str);
            } else {
                s = string_to_moo_type(str, handle->options & SQLITE_PARSE_OBJECTS, handle->options & SQLITE_SANITIZE_STRINGS);
            }
            row = listappend(row, s);
        }
        *r = listappend(*r, row);
        rc = sqlite3_step(stmt);
    }

    /* TODO: Reset the prepared statement bindings and cache it.
     *       (Remove finalize when that happens) */
    sqlite3_finalize(stmt);

    handle->locks--;
}

/* Creates and executes a prepared statement.
 * Args: INT <database handle>, STR <SQL query>, LIST <values>, BOOL <threaded>
 * e.g. sqlite_execute(0, 'INSERT INTO test VALUES (?, ?);', {5, #5})
 * TODO: Cache prepared statements? */
    static package
bf_sqlite_execute(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_wizard(progr))
    {
        free_var(arglist);
        return make_error_pack(E_PERM);
    }

        char *human_string = nullptr;
        asprintf(&human_string, "sqlite_execute: %s", arglist.v.list[2].v.str);

        return background_thread(sqlite_execute_thread_callback, &arglist, human_string);
}

/* The function responsible for the actual query call.
 * Contains functionality shared by both the threaded and
 * unthreaded builtins. */
void sqlite_query_thread_callback(Var args, Var *r)
{
    int index = args.v.list[1].v.num;

    if (!valid_handle(index))
    {
        r->type = TYPE_ERR;
        r->v.err = E_INVARG;
        return;
    }

    const char *query = args.v.list[2].v.str;
    char *err_msg = nullptr;

    sqlite_result *thread_handle = (sqlite_result*)mymalloc(sizeof(sqlite_result), M_STRUCT);
    thread_handle->connection = &sqlite_connections[index];
    thread_handle->last_result = new_list(0);
    thread_handle->include_headers = args.v.list[0].v.num > 2 && is_true(args.v.list[3]);

    thread_handle->connection->locks++;

    int rc = sqlite3_exec(thread_handle->connection->id, query, callback, thread_handle, &err_msg);

    thread_handle->connection->locks--;

    if (rc != SQLITE_OK)
    {
        r->type = TYPE_STR;
        r->v.str = str_dup(err_msg);
        sqlite3_free(err_msg);
    } else {
        *r = var_dup(thread_handle->last_result);
        free_var(thread_handle->last_result);
    }
    //sqlite3_db_release_memory(thread_handle->connection->id);
    myfree(thread_handle, M_STRUCT);
}


/* Execute an SQL command.
 * Args: INT <database handle>, STR <query>, BOOL <threaded> */
    static package
bf_sqlite_query(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_wizard(progr))
    {
        free_var(arglist);
        return make_error_pack(E_PERM);
    }

        char *human_string = nullptr;
        asprintf(&human_string, "sqlite_query: %s", arglist.v.list[2].v.str);

        return background_thread(sqlite_query_thread_callback, &arglist, human_string);
}

/* Identifies the row ID of the last insert command.
 * (this was an early API test, I don't really see the usefulness) */
    static package
bf_sqlite_last_insert_row_id(Var arglist, Byte next, void *vdata, Objid progr)
{
    if (!is_wizard(progr))
    {
        free_var(arglist);
        return make_error_pack(E_PERM);
    }

    int index = arglist.v.list[1].v.num;
    free_var(arglist);

    if (!valid_handle(index))
        return make_error_pack(E_INVARG);

    sqlite_conn *handle = &sqlite_connections[index];

    Var r;
    r.type = TYPE_INT;
    r.v.num = sqlite3_last_insert_rowid(handle->id);

    return make_var_pack(r);
}

/* Set run-time limits */
    static package
bf_sqlite_limit(Var arglist, Byte next, void *vdata, Objid progr)
{
    static const struct {
        const char *str;
        int value;
    } categories[] = {
        {"LIMIT_LENGTH", 0},
        {"LIMIT_SQL_LENGTH", 1},
        {"LIMIT_COLUMN", 2},
        {"LIMIT_EXPR_DEPTH", 3},
		{"LIMIT_COMPOUND_SELECT", 4},
		{"LIMIT_VDBE_OP", 5},
		{"LIMIT_FUNCTION_ARG", 6},
		{"LIMIT_ATTACHED", 7},
		{"LIMIT_LIKE_PATTERN_LENGTH", 8},
		{"LIMIT_VARIABLE_NUMBER", 9},
		{"LIMIT_TRIGGER_DEPTH", 10},
		{"LIMIT_WORKER_THREADS", 11}
    };

    static const int max_categories = sizeof(categories) / sizeof(categories[0]);

    if (!is_wizard(progr))
    {
        free_var(arglist);
        return make_error_pack(E_PERM);
    }

    int index = arglist.v.list[1].v.num;
    if (!valid_handle(index)) {
        free_var(arglist);
        return make_error_pack(E_INVARG);
    }

    int category = -1;
    int new_value = arglist.v.list[3].v.num;

    if (arglist.v.list[2].type == TYPE_STR) {
        const char *player_category = arglist.v.list[2].v.str;
        for (auto category_name : categories)
            if (!strcmp(player_category, category_name.str)) {
                category = category_name.value;
                break;
            }
    } else if (arglist.v.list[2].type == TYPE_INT) {
        category = arglist.v.list[2].v.num;
    }

    free_var(arglist);

    if (category < 0 || category > max_categories - 1)
        return make_error_pack(E_INVARG);

    sqlite_conn *handle = &sqlite_connections[index];

    Var r;
    r.type = TYPE_INT;
    r.v.num = sqlite3_limit(handle->id, category, new_value);

    return make_var_pack(r);
}

/* -------------------------------------------------------- */

/* Return true if a handle is valid and active. */
bool valid_handle(int handle)
{
    if (handle < 0 || handle >= next_sqlite_handle || sqlite_connections.count(handle) == 0)
        return false;

    return true;
}

/* Return the index of the next handle.
 * If we've exceeded our maximum connection limit, -1 will be returned.
 * Otherwise, a valid SQLite handle integer is returned. */
int next_handle()
{
    if (sqlite_connections.size() >= server_int_option("sqlite_max_handles", SQLITE_MAX_HANDLES))
        return -1;

    return next_sqlite_handle;
}

/* Create an empty connection and add it to the open connection map. */
int allocate_handle()
{
    int handle = next_handle();
    if (handle == -1)
        return -1;

    next_sqlite_handle++;

    sqlite_conn connection;
    connection.path = nullptr;
    connection.options = SQLITE_PARSE_TYPES | SQLITE_PARSE_OBJECTS;
    connection.locks = 0;

    sqlite_connections[handle] = connection;

    return handle;
}

/* Free up memory and remove a handle from the connection map. */
void deallocate_handle(int handle, bool shutdown)
{
    sqlite_conn conn = sqlite_connections[handle];

    sqlite3_close(conn.id);
    if (conn.path != nullptr)
        free_str(conn.path);
    if (!shutdown)
        sqlite_connections.erase(handle);

    if (sqlite_connections.size() == 0)
        next_sqlite_handle = 1;
}


/* Check if a database at 'path' is already open.
 * If so, return its handle. Otherwise, return -1. */
int database_already_open(const char *path)
{
    for (auto &it : sqlite_connections)
    {
        if (it.second.path != nullptr && strcmp(it.second.path, path) == 0)
            return it.first;
    }

    return -1;
}

/* The callback function that sqlite will call on each row. */
int callback(void *index, int argc, char **argv, char **azColName)
{
    sqlite_result *thread_handle = (sqlite_result*)index;
    sqlite_conn *handle = thread_handle->connection;

    Var ret = new_list(0);

    for (int i = 0; i < argc; i++)
    {
        Var s;
        if (!(handle->options & SQLITE_PARSE_TYPES))
        {
            s.type = TYPE_STR;

            if (handle->options & SQLITE_SANITIZE_STRINGS)
                sanitize_string_for_moo(argv[i]);

            s.v.str = str_dup(argv[i]);
        } else {
            s = string_to_moo_type(argv[i], handle->options & SQLITE_PARSE_OBJECTS, handle->options & SQLITE_SANITIZE_STRINGS);
        }

        if (thread_handle->include_headers) {
            Var tmp_value = new_list(2);
            tmp_value.v.list[1] = str_dup_to_var(azColName[i]);
            tmp_value.v.list[2] = s;
            ret = listappend(ret, tmp_value);
        } else {
            ret = listappend(ret, s);
        }
    }

    thread_handle->last_result = listappend(thread_handle->last_result, ret);

    return 0;
}

/* The MOO database really dislikes newlines, so we'll want to strip them.
 * I like what MOOSQL did here by replacing them with tabs, so we'll do that.
 * TODO: Check the performance impact of this being on by default with long strings. */
void sanitize_string_for_moo(char *string)
{
    if (!string)
        return;

    char *p = string;

    while (*p)
    {
        if (*p == '\n')
            *p = '\t';

        ++p;
    }
}

/* Take a result string and convert it into a MOO type.
 * Return a Var of the appropriate MOO type for the value.
 * TODO: Try to parse strings containing MOO lists? */
Var string_to_moo_type(char* str, bool parse_objects, bool sanitize_string)
{
    Var s;

    if (str == nullptr)
    {
        s.type = TYPE_STR;
        s.v.str = str_dup("NULL");
        return s;
    }

    double double_test = 0.0;
    Num int_test = 0;

    if (str[0] == '#' && parse_objects && parse_number(str + 1, &int_test, 0) == 1)
    {
        // Add one to the pointer to skip over the # and check the rest for numeracy
        s.type = TYPE_OBJ;
        s.v.obj = int_test;
    } else if (parse_number(str, &int_test, 0) == 1) {
        s.type = TYPE_INT;
        s.v.num = int_test;
    } else if (parse_float(str, &double_test) == 1)
    {
        s.type = TYPE_FLOAT;
        s.v.fnum = double_test;
    } else {
        if (sanitize_string)
            sanitize_string_for_moo(str);
        s.type = TYPE_STR;
        s.v.str = str_dup(str);
    }
    return s;
}

/* Converts a MOO object (supplied to a prepared statement) into a string similar to
 * tostr(#xxx) */
Stream* object_to_string(Var *thing)
{
    static Stream *s = nullptr;

    if (!s)
        s = new_stream(11);

    stream_printf(s, "#%d", thing->v.num);

    return s;
}

/* Clean up when the server shuts down. */
void sqlite_shutdown()
{
    for (auto const& x : sqlite_connections)
        deallocate_handle(x.first, true);
}

void
register_sqlite() {
    oklog("REGISTER_SQLITE: v%s (SQLite Library v%s)\n", SQLITE_MOO_VERSION, sqlite3_libversion());
      if (sqlite3_threadsafe() > 0) {
        int retCode = sqlite3_config(SQLITE_CONFIG_SERIALIZED);
        if (retCode != SQLITE_OK) {
            errlog("SQLite couldn't be set to serialized.\n");
        }
    } else {
        applog(LOG_WARNING, "SQLite is not compiled to be thread-safe. BEWARE!");
    }

    register_function("sqlite_open", 1, 2, bf_sqlite_open, TYPE_STR, TYPE_INT);
    register_function("sqlite_close", 1, 1, bf_sqlite_close, TYPE_INT);
    register_function("sqlite_handles", 0, 0, bf_sqlite_handles);
    register_function("sqlite_info", 1, 1, bf_sqlite_info, TYPE_INT);
    register_function("sqlite_query", 2, 3, bf_sqlite_query, TYPE_INT, TYPE_STR, TYPE_ANY);
    register_function("sqlite_execute", 3, 3, bf_sqlite_execute, TYPE_INT, TYPE_STR, TYPE_LIST);
    register_function("sqlite_last_insert_row_id", 1, 1, bf_sqlite_last_insert_row_id, TYPE_INT);
    register_function("sqlite_limit", 3, 3, bf_sqlite_limit, TYPE_INT, TYPE_ANY, TYPE_INT);
}

#else /* SQLITE3_FOUND */
void register_sqlite(void) { }
void sqlite_shutdown(void) { }
#endif
