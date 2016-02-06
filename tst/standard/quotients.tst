#############################################################################
##
#W  standard/quotients.tst
#Y  Copyright (C) 2015                                  James D. Mitchell 
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/quotients.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#T# quotients, OneImmutable
gap> S := PartitionMonoid(4);
<regular bipartition monoid of degree 4 with 4 generators>
gap> cong := SemigroupCongruence(S, [S.3, S.4]);
<semigroup congruence over <regular bipartition monoid of degree 4 with 4 
 generators> with 1 generating pairs>
gap> T := S / cong;;
gap> Size(T);
25
gap> One(T);
<congruence class of <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], 
 [ 4, -4 ]>>

#T# quotients, GeneratorsOfSemigroup
gap> S := JonesMonoid(5);
<regular bipartition monoid of degree 5 with 4 generators>
gap> I := SemigroupIdeal(S, S.4);
<regular bipartition semigroup ideal of degree 5 with 1 generator>
gap> J := SemigroupIdeal(I, Bipartition([[1, -3], [2, -4], [3, 4], [5, -5], 
> [-1, -2]]));
<regular bipartition semigroup ideal of degree 5 with 1 generator>
gap> T := I / J;;
gap> HasGeneratorsOfMagma(T);
false
gap> GeneratorsOfSemigroup(T);
[ <congruence class of <bipartition: [ 1, -3 ], [ 2, -4 ], [ 3, 4 ], 
     [ 5, -5 ], [ -1, -2 ]>> ]

#T# quotients, Rees quotient
gap> S := PartitionMonoid(4);
<regular bipartition monoid of degree 4 with 4 generators>
gap> I := SemigroupIdeal(S, S.4);
<regular bipartition semigroup ideal of degree 4 with 1 generator>
gap> T := S / I;;
gap> Size(T);
25

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(x);

#E#
gap> STOP_TEST("Semigroups package: standard/quotients.tst");
