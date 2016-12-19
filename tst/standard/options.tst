#############################################################################
##
#W  standard/options.tst
#Y  Copyright (C) 2016                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/options.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();
gap> SEMIGROUPS.DefaultOptionsRec.acting := true;;

# SEMIGROUPS.ProcessOptionsRec
gap> SEMIGROUPS.ProcessOptionsRec(rec(hashlen := 103));
rec( acting := true, batch_size := 8192, cong_by_ker_trace_threshold := 100000
    , hashlen := rec( L := 107, M := 29, S := 2 ), nr_threads := 4, 
  regular := false, report := false, small := false )

# SEMIGROUPS.OptionsRec
gap> SEMIGROUPS.OptionsRec(TrivialSemigroup());
rec( acting := true, batch_size := 8192, cong_by_ker_trace_threshold := 100000
    , hashlen := rec( L := 25013, M := 6257, S := 251 ), nr_threads := 4, 
  regular := false, report := false, small := false )
gap> SEMIGROUPS.OptionsRec(Group(()));
rec( acting := true, batch_size := 8192, cong_by_ker_trace_threshold := 100000
    , hashlen := rec( L := 25013, M := 6257, S := 251 ), nr_threads := 4, 
  regular := false, report := false, small := false )

# 
gap> SEMIGROUPS.DefaultOptionsRec.acting := false;;

# SEMIGROUPS.ProcessOptionsRec
gap> SEMIGROUPS.ProcessOptionsRec(rec(hashlen := 103));
rec( acting := false, batch_size := 8192, 
  cong_by_ker_trace_threshold := 100000, 
  hashlen := rec( L := 107, M := 29, S := 2 ), nr_threads := 4, 
  regular := false, report := false, small := false )

# SEMIGROUPS.OptionsRec
gap> SEMIGROUPS.OptionsRec(TrivialSemigroup());
rec( acting := false, batch_size := 8192, 
  cong_by_ker_trace_threshold := 100000, 
  hashlen := rec( L := 25013, M := 6257, S := 251 ), nr_threads := 4, 
  regular := false, report := false, small := false )
gap> SEMIGROUPS.OptionsRec(Group(()));
rec( acting := false, batch_size := 8192, 
  cong_by_ker_trace_threshold := 100000, 
  hashlen := rec( L := 25013, M := 6257, S := 251 ), nr_threads := 4, 
  regular := false, report := false, small := false )

#
gap> STOP_TEST("Semigroups package: standard/options.tst");
