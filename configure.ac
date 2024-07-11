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

## Zeromq
AC_CHECK_HEADERS([zmq.h sys/wait.h], [],
    [AC_MSG_ERROR([Unable to find zmq.h])])

AC_SEARCH_LIBS(zmq_ctx_new, zmq, [],
    [AC_MSG_ERROR([Unable to find zmq libs])])

## OTF2
AC_CHECK_HEADERS([otf2/otf2.h], [],
    [AC_MSG_ERROR([Unable to find otf2/otf2.h])])

AC_SEARCH_LIBS(OTF2_Archive_Open, otf2, [],
    [AC_MSG_ERROR([Unable to find otf2 libs])])

## PMPMEAS DEPENDENCIES
AC_ARG_ENABLE(metrics, [AS_HELP_STRING([--disable-metrics], [do not use metrics (default: yes)])])

if test "$enable_metrics" != no
then
    ### PAPI - sub-option if metrics enabled
	AC_ARG_WITH(papi, [AS_HELP_STRING([--without-papi], [do not use papi for metric collection (default: yes)])])
    if test "X$with_papi" != "Xno"
    then 
        AC_CHECK_HEADERS([papi.h], [],
            [AC_MSG_ERROR([Unable to find papi.h])])

        AC_SEARCH_LIBS(PAPI_library_init, papi, [],
            [AC_MSG_ERROR([Unable to find papi libs])])
        AC_DEFINE(USEPAPI, [], [Enable usage of papi to collect hwpc])

        PAPICNTMAX=$(papi_avail | grep "Number Hardware Counters :" | sed -e "s|Number Hardware Counters : ||")
        AC_SUBST(PAPICNTMAX)
    fi

    ### Perf
    AC_CHECK_HEADERS([linux/perf_event.h], [], 
        [AC_MSG_ERROR([Unable to find perf])])

    AC_DEFINE(_COLLECT_METRICS, [], [Enable rTrace metric collection])
    AC_DEFINE(RTRACE_SUPPORT, [], [Tell pmpmeas to compile in R compatible way])
fi

## PMPMEAS OBJECTS

# Dynamically generate list of sources from subdirectories via shell
PMPMEAS_C_SOURCE="$(cd src && ls pmpmeas/src/lib/*.c | tr '\n' ' ')"
PMPMEAS_CPP_SOURCE="$(cd src && ls pmpmeas/src/lib/*.cc | tr '\n' ' ')"
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
