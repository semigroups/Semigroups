#############################################################################
##
##  standard/fp/fpsemi.tst
#Y  Copyright (C) 2015                                   Wilfred A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/fp/fpsemi.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS_StartTest();

#T# AsFpSemigroup 1: trivial semigroup
gap> S := TrivialSemigroup();
<trivial transformation group of degree 0 with 0 generators>
gap> S := AsFpSemigroup(S);
<fp semigroup on the generators [ s1 ]>
gap> RelationsOfFpSemigroup(S);
[ [ s1^2, s1 ] ]

#T# AsFpSemigroup 2: 2 element semilattice
gap> S := Semigroup(PartialPerm([1]), PartialPerm([], []));
<commutative partial perm monoid of rank 1 with 1 generator>
gap> S := AsFpSemigroup(S);
<fp semigroup on the generators [ s1, s2 ]>
gap> RelationsOfFpSemigroup(S);
[ [ s1^2, s1 ], [ s1*s2, s2 ], [ s2*s1, s2 ], [ s2^2, s2 ] ]

#T# AsFpMonoid 1: trivial semigroup
gap> S := TrivialSemigroup();
<trivial transformation group of degree 0 with 0 generators>
gap> S := AsFpMonoid(S);
<fp group on the generators [  ]>

#T# AsFpMonoid 2: 2 element semilattice
gap> S := Semigroup(PartialPerm([1]), PartialPerm([], []));
<commutative partial perm monoid of rank 1 with 1 generator>
gap> S := AsFpMonoid(S);
<fp monoid on the generators [ m1 ]>
gap> RelationsOfFpMonoid(S);
[ [ m1^2, m1 ] ]

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);

#E#
gap> STOP_TEST("Semigroups package: standard/fp/fpsemi.tst");
