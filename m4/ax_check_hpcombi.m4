dnl handle HPCombi checks
dnl The basic idea is that we only check here for the things that we need to
dnl set if HPCombi is enable/disabled, and we leave it up to libsemigroups itself 
dnl to handle the other checks as to whether or not HPCombi can be used.

AC_DEFUN([AX_CHECK_HPCOMBI],
 [AS_IF([test "x$enable_hpcombi" != xno], [
  m4_define([ax_hpcombi_cxxflags_variable],[HPCOMBI_CXXFLAGS])

  dnl # Check if the flags required for HPCombi are supported
  AX_CHECK_COMPILE_FLAG(-march=avx,
                        AX_APPEND_FLAG(-march=avx,
                                       [ax_hpcombi_cxxflags_variable]),
                        [AC_MSG_WARN([flag -march=avx not supported])
                         enable_hpcombi=no])

  AX_CHECK_COMPILE_FLAG(-flax-vector-conversions, 
                        AX_APPEND_FLAG(-flax-vector-conversions,
                                       [ax_hpcombi_cxxflags_variable]),
                        [AC_MSG_WARN([flag -flax-vector-conversions not supported])
                         enable_hpcombi=no])

  dnl # check for HPCombi's preprocessor macro
  AC_MSG_CHECKING([for HPCOMBI_CONSTEXPR_FUN_ARGS])
    AC_COMPILE_IFELSE( 
                    [AC_LANG_PROGRAM( 
                        [[using T = int; constexpr int exec(T f()) { return f(); }
                        constexpr int foo() { return 1; }
                        static_assert(exec(foo) == 1, "Failed exec");]]
                      )],
                     [hpcombi_constexpr_fun_args=yes],
                     [hpcombi_constexpr_fun_args=no]
                   )
  AC_MSG_RESULT([$hpcombi_constexpr_fun_args])
 ])

  AM_CONDITIONAL([HPCOMBI_CONSTEXPR_FUN_ARGS], 
                 [test "x$hpcombi_constexpr_fun_args" = xyes])
    
  AS_IF([test "x$enable_hpcombi" != xno],
        AC_SUBST(ax_hpcombi_cxxflags_variable))
])
