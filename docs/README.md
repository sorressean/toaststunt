This is my fork of ToastStunt. I will keep it updated with their mainline and submit pull requests for features that might be useful, but I've also added extra builtins that may or may not be of any use to anyone. You can see the difference by comparing the two feature lists.

See `README.stunt' for general information on Stunt.
# ToastStunt

ToastStunt is a fork of the LambdaMOO / Stunt server. It has a number of features that were found useful while developing [Miriani](https://www.toastsoft.net) and [ChatMud](https://www.chatmud.com/), a mostly complete list of which can be found below.

* [Features](#features)
* [Build Instructions](#build-instructions)
  * [Debian/Ubuntu](#debian-ubuntu)
  * [REL/CentOS](#rel-centos)
  * [Gentoo](#gentoo)
  * [macOS](#macos)
* [Function Documentation](https://github.com/lisdude/toaststunt-documentation)
* [ToastCore](https://github.com/lisdude/toastcore)
* [Support and Development](#support-and-development)
* [Stunt Information](README.stunt)

## Features

- SQLite [functions: sqlite_open(), sqlite_close(), sqlite_handle(), sqlite_info(), sqlite_query(), sqlite_execute()]
- Perl Compatible Regular Expressions (PCRE) [functions: pcre_match(), pcre_replace]
- Simplex noise (implemented but never actually tested / used)
- [Argon2id hashing](https://github.com/P-H-C/phc-winner-argon2) [functions: argon2(), argon2_verify()]
- 32-bit and 64-bit versions ($maxint and $minint set automatically)
- #0:server_stopped support (called on shutdown or when a signal is received)

- Waifs
    - Call :pre_destroy on waifs when they're destroyed
    - A WAIF type (so typeof(some_waif) == WAIF)
    - Waif dict patch (so waif[x] and waif[x] = y will call the :_index and :_set_index verbs on the waif)
    - '-w' command line option to convert existing databases with a different waif type to the new waif type
    - waif_stats (show how many instances of each class of waif exist, how many waifs are pending recycling, and how many waifs in total exist)
    - Parser recognition for waif properties (e.g. thing.:property)

- Basic threading support:
    - background.cc (a library, of sorts, to make it easier to thread builtins)
    - Threaded builtins: sqlite_query, sqlite_execute, locate_by_name, sort

- FileIO improvements:
    - Faster reading
    - Open as many files as you want, configurable with FILE_IO_MAX_FILES or $server_options.file_io_max_files
    - file_handles() (returns a list of open files)
    - file_grep() (search for a string in a file (kind of FUP in FIO, don't tell))
    - file_count_lines() (counts the number of lines in a file)

- ANSI:
    - Parse_ansi() (parses color tags into their ANSI equivalents)
    - remove_ansi() (strips ANSI tags from strings)

- syntax:
    - Added syntax sugar for loops; for foo in (bar) becomes for (foo:bar), for v,k in (map) becomes for (v,k:map). Of course, the other versions work as well; these just feel easier to use.

- Telnet:
    - Capture IAC commands and pass them to listener:do_out_of_band_command() for the database to handle.

- Primitive types:
    - Support calling verbs on an object prototype ($obj_proto). Counterintuitively, this will only work for types of OBJ that are invalid. This can come in useful for un-logged-in connections (i.e. creating a set of convenient utilities for dealing with negative connections in-MOO).

- Maps
    - maphaskey() (check if a key exists in a map. Looks nicer than `!(x in mapkeys(map))` and is faster when not dealing with hundreds of keys)

- Profiling
    - finished_tasks() (returns a list of the last X tasks to finish executing, including their total execution time) [see options.h below]
    - Set a maximum lag threshold (can be overridden with $server_options.task_lag_threshold) that, when exceeded, will make a note in the server log and call #0:handle_lagging_task with arguments: {callers, execution time}

- Options.h configuration:
    - LOG_CODE_CHANGES (causes .program and set_verb_code to add a line to the server log indicating the object, verb, and programmer)
    - OWNERSHIP_QUOTA (disable the server's builtin quota management)
    - USE_ANCESTOR_CACHE (enable a cache of an object's ancestors to speed up property lookups)
    - UNSAFE_FIO (skip character by character line verification, trading off potential safety for speed)
    - LOG_EVALS (add an entry to the server log any time eval is called)
    - ONLY_32_BITS (switch from 64-bit integers back to 32-bit)
    - MAX_LINE_BYTES (unceremoniously close connections that send lines exceeding this value to prevent memory allocation panics)
    - DEFAULT_LAG_THRESHOLD (the number of seconds allowed before a task is considered laggy and triggers #0:handle_lagging_task)
    - SAVE_FINISHED_TASKS (enable the finished_tasks function and define how many tasks get saved by default) [default can be overridden with $server_options.finished_tasks_limit]

- Additional builtins:
    - frandom (random floats)
    - distance (calculate the distance between an arbitrary number of points)
    - relative_heading (a relative bearing between two coordinate sets)
    - memory_usage (total memory used, resident set size, shared pages, text, data + stack)
    - ftime (precise time, including an argument for monotonic timing)
    - locate_by_name (quickly locate objects by their .name property)
    - usage (returns {load averages}, user time, system time, page reclaims, page faults, block input ops, block output ops, voluntary context switches, involuntary context switches, signals received)
    - explode (serverified version of the LambdaCore verb on $string_utils)
    - slice (serverified version of the LambdaCore verb on $list_utils)
    - occupants (return a list of objects of parent parent, optionally with a player flag check)
    - spellcheck (uses Aspell to check spelling)
    - locations (recursive location function)
    - clear_ancestor_cache (clears the ancestor cache manually)
    - chr (return extended ASCII characters; characters that can corrupt your database are considered invalid)
    - deep_contents (get all nested contents of an object. Provide as a second argument a parent if you want only descendants of that parent returned)
    - contains_key/contains_value (Determines if the map contains the specified key/value)
    - intersection, union, diff (set operations, taken and modified from Goblin's extension pack)
    - assoc, iassoc, slice (taken from Martian's list extensions).
    - set_merge (merges two lists together using setadd)
    - bit_or, bit_and, bit_xor, bit_not (perform bit operations on integers)
    - list_reverse (reverse a list; more performant than goblin's widely used extension)
    - reseed_random (reseed the random number generator)
    - yin (yield if needed. Replicates :suspend_if_needed and ticks_left() checks)
    - sort (a significantly faster replacement for the :sort verb. Also allows for natural sort order and reverse sorting)

- Miscellaneous changes:
    - Numeric IP addresses in connection_name
    - Detect connections from TCP proxies using the HAProxy Proxy protocol and rewrite the source IP as appropriate (controllable with $server_options.proxy_rewrite)
    - .last_move (a map of an object's last location and the time() it moved)
    - Sub-second fork and suspend
    - Call 'do_blank_command' on listening objects when a blank command is issued
    - Allow `"string" in "some other string"` as a shortcut for index()
    - Allow exec to set environment variables with a new argument
    - Bandaid over an issue where emptylist loses all references and gets freed, causing a server panic
    - Change the server log message when calling switch_player()
    - Complete deprecation of tonum() in favor of toint()
    - Move #0.dump_interval to $server_options.dump_interval
    - Rename recycle() to destroy() (also call pre_destroy rather than recycle verbs)
    - New argument to notify() to suppress the newline
    - Support object lists in isa() as well as an optional third argument to return the matching parent rather than simply true or false
    - New argument to move() to effectively listinsert() the object into the destination's .contents
    - New argument to is_member() for controlling case sensitivity of equality comparisons. No third argument or a true value results in standard functionality; a false value as the third argument results in case not mattering at all

## Build Instructions
### **Debian/Ubuntu**
```bash
apt install build-essential bison gperf autoconf libsqlite3-dev libaspell-dev libpcre3-dev nettle-dev g++
autoconf
./configure
make
```

### **REL/CentOS**
```bash
yum group install -y "Development Tools"
yum install -y sqlite-devel pcre-devel aspell-devel nettle-devel gperf   centos-release-scl
yum install -y devtoolset-7
scl enable devtoolset-7 bash
autoconf
./configure
make
exit
```

### **Gentoo**
```bash
emerge dev-db/sqlite app-text/aspell app-crypt/argon2
autoconf
./configure
make
```

### **macOS**
Installing dependencies requires [Homebrew](https://brew.sh/).

Follow the instructions in the notes section below to compile and install Argon2. **NOTE**: In the last step, the install prefix should be changed to `/usr/local`

```bash
brew install autoconf pcre aspell nettle
autoconf
./configure
make
```

## **Notes**
### Argon2
Many distributions do not include [Libargon2](https://github.com/P-H-C/phc-winner-argon2) which is required for Argon2id password hashing. As such, it has been included as a Git submodule in this repository. To build it yourself, follow these steps:

1. Inside of the ToastStunt repository, checkout all available submodules: `git submodule update --init`
2. `cd dependencies/phc-winner-argon2`
3. Build the library: `make`
4. Install it on your system: `make install PREFIX=/usr`

**NOTE**: macOS users should instead use `make install PREFIX=/usr/local` in step 4.

### Stuck seeding from /dev/random
It can take some time to seed if your system is low on entropy. If you find startup hangs here, there are a couple of options:

1. Install `haveged` to generate entropy.
2. Edit `options.h` and change the value of MINIMUM_SEED_ENTROPY to something smaller, like 20.

### Login screen not showing
Due to the way proxy detection works, if you're connecting to your MOO from localhost, you won't see the login screen. This is a minor inconvenience and shouldn't affect your ability to actually use your MOO. However, if it bothers you, you can disable HAProxy rewriting:

1. `@prop $server_options.proxy_rewrite 0`
2. `;load_server_options()`

## Support and Development

Realtime support and collaborative discussion for ToastStunt primarily takes place on the 'toaststunt' channel on ChatMUD. Barring this, the [Miriani Message Boards](https://board.toastsoft.net/) are another good resource for assistance.
