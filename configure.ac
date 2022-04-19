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
AC_INIT([semigroups], m4_esyscmd([tr -d '\n' < .VERSION]))
AC_CONFIG_SRCDIR([src/pkg.cpp])
AC_CONFIG_HEADERS([src/_pkgconfig.h:cnf/pkgconfig.h.in])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([cnf])

dnl ##
dnl ## Get canonical host info
dnl ##
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

dnl ##
dnl ## Setup automake
dnl ##
AM_INIT_AUTOMAKE([1.11 -Wall foreign subdir-objects nostdinc])
AM_SILENT_RULES([yes])
AM_PROG_AR

AX_PREFIX_CONFIG_H([src/semigroups-config.hpp],[],[src/_pkgconfig.h])

dnl ## abs_top_builddir seems to hold the top build dir for the subpackage
dnl ## libsemigroups which is why this contains ../ 
AC_PREFIX_DEFAULT('${abs_top_builddir}/../bin/')

dnl ##
dnl ## Set the language
dnl ##
AC_PROG_CXX
AC_LANG([C++])

AX_CXX_COMPILE_STDCXX_14(,[mandatory])

dnl ##
dnl ## Setup libtool (for building the GAP kernel extension)
dnl ##
LT_PREREQ([2.4.2])
LT_INIT([disable-static dlopen win32-dll])

dnl ##
dnl ## Locate the GAP root dir
dnl ##
FIND_GAP

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

dnl ## Check for libsemigroups
AX_CHECK_LIBSEMIGROUPS

dnl ## User setting: Debug mode (off by default)
AC_ARG_ENABLE([debug],
    [AS_HELP_STRING([--enable-debug], [enable debug mode])],
    [AC_DEFINE([KERNEL_DEBUG], [1], [define if building in debug mode])],
    [enable_debug=no]
    )
AC_MSG_CHECKING([whether to enable debug mode])
AC_MSG_RESULT([$enable_debug])

AM_CONDITIONAL([KERNEL_DEBUG], [test "x$enable_debug" = xyes])

# Check if HPCombi is enable, and available
AX_CHECK_HPCOMBI()

dnl ##
dnl ## Output everything
dnl ##
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
