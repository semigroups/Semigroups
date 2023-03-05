#############################################################################
##
#W  extreme/freeband.tst
#Y  Copyright (C) 2016                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local S
gap> START_TEST("Semigroups package: extreme/freeband.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Test for previous issue of wrong methods apply to free bands. These tests
# check that IsomorphismTransformationSemigroup works for free bands below and
# above the threshold for them being CanUseFroidurePin.
gap> S := FreeBand(1);
<free band on the generators [ x1 ]>
gap> AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group(())>
gap> S := FreeBand(3);
<free band on the generators [ x1, x2, x3 ]>
gap> IsomorphismTransformationSemigroup(S);
<free band on the generators [ x1, x2, x3 ]> -> 
<transformation semigroup of size 159, degree 160 with 3 generators>
gap> S := FreeBand(4);
<free band on the generators [ x1, x2, x3, x4 ]>
gap> AsSemigroup(IsTransformationSemigroup, S);
<transformation semigroup of size 332380, degree 332381 with 4 generators>

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/freeband.tst");
