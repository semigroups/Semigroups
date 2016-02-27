#############################################################################
##
##  standard/fpsemi.tst
#Y  Copyright (C) 2015                                   Wilfred A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/fpsemi.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#T# AsFpSemigroup 1: trivial semigroup
gap> S := TrivialSemigroup();
<trivial transformation group of degree 0 with 1 generator>
gap> S := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1 ]>
gap> RelationsOfFpSemigroup(S);
[ [ s1^2, s1 ] ]

#T# AsFpSemigroup 2: 2 element semilattice
gap> S := Semigroup(PartialPerm([1]), PartialPerm([], []));
<partial perm monoid of rank 1 with 2 generators>
gap> S := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2 ]>
gap> RelationsOfFpSemigroup(S);
[ [ s1^2, s1 ], [ s1*s2, s2 ], [ s2*s1, s2 ], [ s2^2, s2 ] ]

#T# AsFpMonoid 1: trivial semigroup
#gap> S := TrivialSemigroup();
#<trivial transformation group of degree 0 with 1 generator>
#gap> S := AsFpMonoid(S);
#<fp monoid on the generators [ m1 ]>

#T# AsFpMonoid 2: 2 element semilattice
#gap> S := Semigroup(PartialPerm([1]), PartialPerm([], []));
#<partial perm monoid of rank 1 with 2 generators>
#gap> S := AsMonoid(IsFpMonoid, S);
#<fp monoid on the generators [ m1, m2 ]>
#gap> RelationsOfFpMonoid(S);
#[ [ m1^2, m1 ], [ m1*m2, m2 ], [ m2*m1, m2 ], [ m2^2, m2 ] ]

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);

#E#
gap> STOP_TEST("Semigroups package: standard/fpsemi.tst");
