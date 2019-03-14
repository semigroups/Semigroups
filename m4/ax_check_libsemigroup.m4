dnl handle libsemigroups checks
dnl
dnl if --with-external-libsemigroups is supplied,
dnl use it if it is known to pkg-config and is new enough;
dnl otherwise use the included version
dnl
AC_DEFUN([AX_CHECK_LIBSEMIGROUPS], [
  AC_ARG_WITH([external-libsemigroups],
	      [AC_HELP_STRING([--with-external-libsemigroups],
			      [use the external libsemigroups])])
  REQUI_LIBSEMIGROUPS_VERSION="$(cat .LIBSEMIGROUPS_VERSION)"
  need_included_libsemigroups=yes
  if test "$with_external_libsemigroups" = yes;  then
	m4_ifdef([PKG_CHECK_MODULES], [
	PKG_CHECK_MODULES([LIBSEMIGROUPS], 
                          [libsemigroups >= $REQUI_LIBSEMIGROUPS_VERSION],
			  [need_included_libsemigroups=no],
			  [need_included_libsemigroups=yes])],
	[AC_MSG_NOTICE([ignoring flag --with-external-libsemigroups, the Semigroups configure file was created on a system without m4 macros for pkg-config available...])])
  fi
  if test "$need_included_libsemigroups" = yes;  then
	AC_MSG_NOTICE([using included libsemigroups...])
  	AC_CHECK_FILE(
   		[libsemigroups/src/semigroups.h],
   		[],
   		[AC_MSG_ERROR([libsemigroups is required, clone or download the repo from https://github.com/james-d-mitchell/libsemigroups into this directory])])

	AC_CHECK_FILE(
   		[libsemigroups/VERSION],
   		[],
		[AC_MSG_ERROR([libsemigroups version $REQUI_LIBSEMIGROUPS_VERSION or higher is required])])

	AC_MSG_CHECKING([libsemigroups version])
	FOUND_LIBSEMIGROUPS_VERSION="$(cat libsemigroups/VERSION)"
	AC_MSG_RESULT([$FOUND_LIBSEMIGROUPS_VERSION])
	AX_COMPARE_VERSION($FOUND_LIBSEMIGROUPS_VERSION,
                   [ge],
                   $REQUI_LIBSEMIGROUPS_VERSION,
                   [],
                   [AC_MSG_ERROR([libsemigroups version $REQUI_LIBSEMIGROUPS_VERSION or higher is required])]
                  )
	AC_SUBST(LIBSEMIGROUPS_CFLAGS, ['-I./bin/include'])
	AC_SUBST(LIBSEMIGROUPS_LIBS, ['bin/lib/libsemigroups.la'])
        AC_CONFIG_SUBDIRS([libsemigroups])
  fi

  AM_CONDITIONAL([WITH_INCLUDED_LIBSEMIGROUPS], [test "$need_included_libsemigroups" = yes])
])
