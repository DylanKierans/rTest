# Must init the autoconf setup
# 1st param is project name, 2nd is version no., 3rd is bug report address
AC_INIT([rTrace], [0.0.3])

# Safety checks in case overwritten --srcdir
AC_CONFIG_SRCDIR([DESCRIPTION])

# Store the auxiliary build tools (e.g., install-sh, config.sub, config.guess)
# in this dir (build-aux)
AC_CONFIG_AUX_DIR([tools/build-aux])
AC_CONFIG_MACRO_DIR([tools])

# Init automake, and specify this program use relaxed structures.
# i.e. this program doesn't follow the gnu coding standards, and doesn't have
# ChangeLog, COPYING, AUTHORS, INSTALL, README etc. files.
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

# Compilers used by R
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
    echo "could not determine R_HOME"
    exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`

CXX=`"${R_HOME}/bin/R" CMD config CXX`
if test -z "$CXX"; then
    AC_MSG_ERROR([No C++ compiler is available])
fi
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_LANG(C++)

# Check for C compiler
#AC_PROG_CC

###################
# Look for libraries
## AC_CHECK_LIB (library, function, [action-if-found], [action-if-not-found], [other-libraries])
## AC_ARG_ENABLE(option-name, help-string, action-if-present, action-if-not-present)
## AC_ARG_WITH(option-name, help-string, action-if-present, action-if-not-present)

## Zeromq
AC_ARG_WITH(zeromq, [AS_HELP_STRING([--with-zeromq=PATH], [specify prefix directory for installed zeromq package.])])
AS_IF([test "X$with_zeromq" = "X"], [
    AC_CHECK_HEADERS([zmq.h sys/wait.h], [],
        [AC_MSG_ERROR([Unable to find zmq.h])])

    AC_SEARCH_LIBS(zmq_ctx_new, zmq, [],
        [AC_MSG_ERROR([Unable to find zmq libs])])
    ], [
    # User provided path
    user_libs="-L$with_zeromq/lib -lzmq -Wl,-rpath=$with_zeromq/lib $user_libs"
    user_inc="-I$with_zeromq/include $user_inc"
    ])

## OTF2
AC_ARG_WITH(otf2, [AS_HELP_STRING([--with-otf2=PATH], [specify prefix directory for installed otf2 package.])])
if test "X$with_otf2" = "X"; then 
    AC_CHECK_HEADERS([otf2/otf2.h], [],
        [AC_MSG_ERROR([Unable to find otf2/otf2.h])])

    AC_SEARCH_LIBS(OTF2_Archive_Open, otf2, [],
        [AC_MSG_ERROR([Unable to find otf2 libs])])
else
    # User provided path
    user_libs="-L$with_otf2/lib -lotf2 -Wl,-rpath=$with_otf2/lib $user_libs"
    user_inc="-I$with_otf2/include $user_inc"
fi

### PMPMEAS DEPENDENCIES
AC_ARG_ENABLE(metrics, [AS_HELP_STRING([--disable-metrics], [do not use metrics (default: yes)])])

### PAPI - optional if metrics enabled
AC_ARG_ENABLE(papi, [AS_HELP_STRING([--disable-papi], [do not use papi for metric collection (default: yes)])])
AC_ARG_WITH(papi, [AS_HELP_STRING([--with-papi=PATH], [specify prefix directory for installed papi package.])])

### Perf - requirement if metrics enabled
AC_ARG_WITH(perf, [AS_HELP_STRING([--with-perf=PATH], [specify prefix directory for installed perf package.])])

if test "X$enable_metrics" != "Xno"; then
    if test "X$enable_papi" != "Xno"; then
        if test "X$with_papi" = "X"; then 
            AC_CHECK_HEADERS([papi.h], [],
                [AC_MSG_ERROR([Unable to find papi.h])])

            AC_SEARCH_LIBS(PAPI_library_init, papi, [],
                [AC_MSG_ERROR([Unable to find papi libs])])

            PAPICNTMAX=$(papi_avail | grep "Number Hardware Counters :" | sed -e "s|Number Hardware Counters : ||")
        else
            # User provided path
            user_libs="-L$with_papi/lib -lpapi -Wl,-rpath=$with_papi/lib $user_libs"
            user_inc="-I$with_papi/include $user_inc"
            PAPICNTMAX=$($with_papi/bin/papi_avail | grep "Number Hardware Counters :" | sed -e "s|Number Hardware Counters : ||")
        fi

        AC_DEFINE(USEPAPI, [], [Enable usage of papi to collect hwpc])
        AC_SUBST(PAPICNTMAX)
    fi

    if test "X$with_perf" = "X"; then 
        AC_CHECK_HEADERS([linux/perf_event.h], [], 
            [AC_MSG_ERROR([Unable to find linux/perf_event.h])])
    else
        user_libs="-Wl,-rpath=$with_perf/lib $user_libs"
        user_inc="-I$with_perf/include $user_inc"
    fi 

    AC_DEFINE(_COLLECT_METRICS, [], [Enable rTrace metric collection])
    AC_DEFINE(PMPMEAS_USERTRACE, [], [Tell pmpmeas to compile in R compatible way])
fi

USER_LIBS="$user_libs"
USER_INC="$user_inc"
AC_SUBST(USER_LIBS)
AC_SUBST(USER_INC)

## PMPMEAS OBJECTS

# Dynamically generate list of sources from subdirectories via shell
PMPMEAS_C_SOURCE="$(cd src && ls pmpmeas/src/lib/*.c | tr '\n' ' ')"
PMPMEAS_CPP_SOURCE="$(cd src && ls pmpmeas/src/lib/*.cpp | tr '\n' ' ')"
AC_SUBST(PMPMEAS_C_SOURCE)
AC_SUBST(PMPMEAS_CPP_SOURCE)

###################

# Tells automake to create header file with AC_DEFINE statements
AC_CONFIG_HEADERS([src/config.h])

# Tells automake to create a Makefile
# See https://www.gnu.org/software/automake/manual/html_node/Requirements.html
AC_CONFIG_FILES([src/Makevars])

# Generate the output
AC_OUTPUT
