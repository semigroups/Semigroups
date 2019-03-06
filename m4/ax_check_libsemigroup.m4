dnl handle libsemigroups checks
dnl
dnl if --with-included-libsemigroups is supplied, always use the included version
dnl otherwise, use an external version if it is known to pkg-config and is new enough
dnl failing the latter, use the included version
dnl
AC_DEFUN([AX_CHECK_LIBSEMIGROUPS], [
  AC_ARG_WITH([included-libsemigroups],
	      [AC_HELP_STRING([--with-included-libsemigroups],
			      [use the libsemigroups included here])])
  REQUI_LIBSEMIGROUPS_VERSION="$(cat .LIBSEMIGROUPS_VERSION)"
  need_included_libsemigroups=yes
  PKG_CHECK_MODULES([LIBSEMIGROUPS], [libsemigroups >= $REQUI_LIBSEMIGROUPS_VERSION], 
  	[need_included_libsemigroups=no],
  	[need_included_libsemigroups=yes])

  if test "$with_included_libsemigroups" = yes -o "$need_included_libsemigroups" = yes;  then
	AC_MSG_NOTICE([Using included libsemigroups.])
  	AC_CHECK_FILE(
   		[libsemigroups/src/semigroups.h],
   		[],
   		[AC_MSG_ERROR([libsemigroups is required, clone or download the repo from
    			https://github.com/james-d-mitchell/libsemigroups into this directory])])

	AC_CHECK_FILE(
   		[libsemigroups/VERSION],
   		[],
   		[AC_MSG_ERROR([libsemigroups version 0.6.1 or higher is required])])

	AC_MSG_CHECKING([libsemigroups version])
	FOUND_LIBSEMIGROUPS_VERSION="$(cat libsemigroups/VERSION)"
	AC_MSG_RESULT([$FOUND_LIBSEMIGROUPS_VERSION])
	AX_COMPARE_VERSION($FOUND_LIBSEMIGROUPS_VERSION,
                   [ge],
                   $REQUI_LIBSEMIGROUPS_VERSION,
                   [],
                   [AC_MSG_ERROR([libsemigroups version $REQUI_LIBSEMIGROUPS_VERSION or higher is required])]
                  )
	AC_SUBST(LIBSEMIGROUPS_CFLAGS, ['-Ilibsemigroups'])
	AC_SUBST(LIBSEMIGROUPS_LIBS, ['libsemigroups/libsemigroups.la'])
  fi
  AC_CONFIG_SUBDIRS([libsemigroups])

  AM_CONDITIONAL([WITH_INCLUDED_LIBSEMIGROUPS], [test "$with_included_libsemigroups" = yes -o "$need_included_libsemigroups" = yes])
])
