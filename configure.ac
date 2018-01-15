#
# Semigroups GAP package
#
# This file is part of the build system of a GAP kernel extension.
# Requires GNU autoconf, GNU automake and GNU libtool.
#

dnl ##
dnl ## Setup autoconf
dnl ##
AC_PREREQ([2.68])
AC_INIT([semigroups], m4_esyscmd([tr -d '\n' < VERSION]))
AC_CONFIG_SRCDIR([src/pkg.cc])
AC_CONFIG_HEADER([src/_pkgconfig.h:cnf/pkgconfig.h.in])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([cnf])
AC_CONFIG_SUBDIRS([src/libsemigroups])

dnl ##
dnl ## Get canonical host info
dnl ##
AC_CANONICAL_HOST

dnl ##
dnl ## Setup automake
dnl ##
AM_INIT_AUTOMAKE([1.11 -Wall foreign subdir-objects])
AM_SILENT_RULES([yes])
AM_PROG_AR

AX_PREFIX_CONFIG_H([src/semigroups-config.h],[],[src/_pkgconfig.h])

dnl ## abs_top_builddir seems to hold the top build dir for the subpackage
dnl ## src/libsemigroups which is why this contains ../.. 
dnl ## FIXME figure out how to do this properly
AC_PREFIX_DEFAULT('${abs_top_builddir}/../../bin/')

dnl ##
dnl ## Set the language
dnl ##
AC_PROG_CXX
AC_LANG([C++])

AX_CXX_COMPILE_STDCXX_11(,[mandatory])

dnl ##
dnl ## Setup libtool (for building the GAP kernel extension)
dnl ##
LT_PREREQ([2.4.2])
LT_INIT([disable-static dlopen win32-dll])

dnl ##
dnl ## Locate the GAP root dir
dnl ##
AC_FIND_GAP

dnl ##
dnl ## Detect pointer size to distinguish 32 and 64 bit builds
dnl ##
AC_CHECK_SIZEOF([void **])

dnl ##
dnl ## Check for pthread, this seems to be required to compile with GCC
dnl ##
AX_PTHREAD(,[AC_MSG_ERROR([pthread is required])])
AC_CHECK_LIB([pthread], [pthread_create])

dnl ##
dnl ## Detect Windows resp. Cygwin
dnl ##
case $host_os in
  *cygwin* ) CYGWIN=yes;;
         * ) CYGWIN=no;;
esac
AM_CONDITIONAL([SYS_IS_CYGWIN], [test "$CYGWIN" = "yes"])
if test "$CYGWIN" = "yes"; then
  AC_DEFINE(SYS_IS_CYGWIN32, 1, are we on CYGWIN?)
else
  AC_DEFINE(SYS_IS_CYGWIN32, 0, are we on CYGWIN?)
fi

dnl ## Check for libsemigroups
AC_CHECK_FILE(
   [src/libsemigroups/src/semigroups.h],
   [],
   [AC_MSG_ERROR([libsemigroups is required, clone or download the repo from
    https://github.com/james-d-mitchell/libsemigroups into the src directory])]
   )

dnl ## Check for a version of libsemigroups with VERSION file
AC_CHECK_FILE(
   [src/libsemigroups/VERSION],
   [],
   [AC_MSG_ERROR([libsemigroups version 0.6.1 or higher is required])]
   )

AC_MSG_CHECKING([libsemigroups version])
REQUI_LIBSEMIGROUPS_VERSION="$(cat .LIBSEMIGROUPS_VERSION)"
FOUND_LIBSEMIGROUPS_VERSION="$(cat src/libsemigroups/VERSION)"
AC_MSG_RESULT([$FOUND_LIBSEMIGROUPS_VERSION])
AX_COMPARE_VERSION($FOUND_LIBSEMIGROUPS_VERSION, 
                   [ge], 
                   $REQUI_LIBSEMIGROUPS_VERSION,
                   [], 
                   [AC_MSG_ERROR([libsemigroups version $REQUI_LIBSEMIGROUPS_VERSION or higher is required])]
                  )

dnl ## User setting: Debug mode (off by default)
AC_ARG_ENABLE([debug],
    [AS_HELP_STRING([--enable-debug], [enable debug mode])],
    [AC_DEFINE([KERNEL_DEBUG], [1], [define if building in debug mode])],
    [enable_debug=no]
    )
AC_MSG_CHECKING([whether to enable debug mode])
AC_MSG_RESULT([$enable_debug])

## TODO check that the compiler supports these flags
AS_IF([test "x$enable_debug" = xyes],
    [AC_SUBST(CXXFLAGS, '-g3 -O0 -Wall -Wextra -Wno-unused-parameter -Wtype-limits -Wformat-security -Wpointer-arith -Wno-div-by-zero -Wreturn-type -Wswitch -Wsizeof-array-argument -Wunused-label -fno-omit-frame-pointer -Wno-c++11-extensions -Wshadow -Wint-conversion')])

AS_IF([test "x$enable_debug" = xyes],
    [AC_SUBST(CFLAGS, '-g3 -O0 -Wall -Wextra -Wno-unused-parameter -Wtype-limits -Wformat-security -Wpointer-arith -Wno-div-by-zero -Wreturn-type -Wswitch -Wsizeof-array-argument -Wunused-label -fno-omit-frame-pointer -Wshadow -Wint-conversion')])

AM_CONDITIONAL([KERNEL_DEBUG], [test "x$enable_debug" = xyes])

dnl ##
dnl ## Output everything
dnl ##
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
