dnl handle libsemigroups checks
dnl
dnl if --with-external-libsemigroups is supplied,
dnl use it if it is known to pkg-config and is new enough;
dnl otherwise use the included version
dnl
AC_DEFUN([AX_CHECK_LIBSEMIGROUPS], [
  AC_ARG_WITH([external-libsemigroups],
	      [AS_HELP_STRING([--with-external-libsemigroups],[use the external libsemigroups])])
  REQUI_LIBSEMIGROUPS_VERSION="$(cat .LIBSEMIGROUPS_VERSION)"
  need_included_libsemigroups=yes
  if test "$with_external_libsemigroups" = yes;  then
	m4_ifdef([PKG_CHECK_MODULES], [
	PKG_CHECK_MODULES([LIBSEMIGROUPS], 
                          [libsemigroups >= $REQUI_LIBSEMIGROUPS_VERSION],
			  [need_included_libsemigroups=no],
			  [need_included_libsemigroups=yes])],
	[AC_MSG_NOTICE(m4_normalize([
                        ignoring flag --with-external-libsemigroups, the Semigroups configure file
                        was created on a system without m4 macros for pkg-config available...]))])
  fi
  if test "$need_included_libsemigroups" = yes;  then
	AC_MSG_NOTICE([using included libsemigroups...])
  	AS_IF(
   		[test -f libsemigroups/include/libsemigroups/libsemigroups.hpp],
   		[],
   		dnl Temporary workaround for backward compatibility with
   		dnl libsemigroups pre PR 172: Includes in libsemigroups/
   		[AS_IF(
   		   [test -f libsemigroups/include/libsemigroups.hpp],
                   [],
                   [AC_MSG_ERROR(m4_normalize([
                                  libsemigroups is required, clone or download the repo from
                                  https://github.com/libsemigroups/libsemigroups into this directory]))])])

        dnl Temporary workaround for compatibility with dev version of
        dnl libsemigroups which doesn't contain .VERSION file by default
	AS_IF(
   		[test -f libsemigroups/.VERSION],
   		[],
        [AS_IF([test -f libsemigroups/etc/version-number.sh], 
           [cd libsemigroups && etc/version-number.sh > .TMP_VERSION && mv .TMP_VERSION .VERSION && cd ..],
           [AC_MSG_ERROR([cannot determine the version of libsemigroups])])])
    
	AC_MSG_CHECKING([libsemigroups version])
	FOUND_LIBSEMIGROUPS_VERSION="$(cat libsemigroups/.VERSION)"
	AC_MSG_RESULT([$FOUND_LIBSEMIGROUPS_VERSION])
	AX_COMPARE_VERSION($FOUND_LIBSEMIGROUPS_VERSION,
                   [ge],
                   $REQUI_LIBSEMIGROUPS_VERSION,
                   [],
                   [AC_MSG_ERROR([libsemigroups version $REQUI_LIBSEMIGROUPS_VERSION or higher is required])]
                  )
        AC_SUBST(LIBSEMIGROUPS_CFLAGS, ['-I./bin/include -I./bin/include/libsemigroups'])
        AC_SUBST(LIBSEMIGROUPS_LIBS, ['-L./bin/lib -lsemigroups'])
        AC_CONFIG_SUBDIRS([libsemigroups])

    AC_SUBST([LIBSEMIGROUPS_RPATH],['-Wl,-rpath,$(abs_top_builddir)/bin/lib'])
  else 
        LIBSEMIGROUPS_VERSION="$(pkg-config --modversion libsemigroups)"
	AC_MSG_NOTICE([using external libsemigroups $LIBSEMIGROUPS_VERSION])
        PKG_CHECK_VAR([LIBSEMIGROUPS_RPATH], [libsemigroups], [libdir],
              [AC_SUBST([LIBSEMIGROUPS_RPATH],[-Wl,-rpath,${LIBSEMIGROUPS_RPATH}])])
  fi

  AS_IF([test "x$need_included_libsemigroups" = xyes],
        AC_SUBST(WITH_INCLUDED_LIBSEMIGROUPS, yes))
])
