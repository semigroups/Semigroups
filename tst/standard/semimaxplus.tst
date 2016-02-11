#############################################################################
##
#W  standard/semimaxplus.tst
#Y  Copyright (C) 2015                                  James D. Mitchell
##                                                       
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semimaxplus.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# semimaxplus: C++ code working, for max-plus matrix semigroup
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[0, -4], [-4, -1]]),
>                   Matrix(IsMaxPlusMatrix, [[0, -3], [-3, -1]]));
<semigroup of 2x2 max-plus matrices with 2 generators>
gap> Size(S);
26
gap> NrDClasses(S);
23
gap> NrRClasses(S);
24
gap> NrLClasses(S);
24
gap> NrHClasses(S);
26
gap> NrIdempotents(S);
4
gap> MultiplicativeZero(S);
fail

# semimaxplus: C++ code working, for natural matrix semigroup
gap> S := Monoid(Matrix(IsNTPMatrix, [[0, 1, 0], [1, 1, 0], [0, 1, 0]], 1, 2),
>                Matrix(IsNTPMatrix, [[1, 0, 0], [1, 0, 1], [1, 0, 0]], 1, 2));
<monoid of 3x3 ntp matrices with 2 generators>
gap> Size(S);
37
gap> Length(RelationsOfFpMonoid(Range(IsomorphismFpMonoid(S))));
12
gap> GenericSemigroupData(S);
<closed semigroup data with 37 elements, 17 relations, max word length 7>
gap> NrDClasses(S);
8
gap> NrRClasses(S);
14
gap> NrLClasses(S);
17
gap> NrHClasses(S);
35
gap> NrIdempotents(S);
20
gap> MultiplicativeZero(S);
fail

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);

#E# 
gap> STOP_TEST("Semigroups package: standard/semimaxplus.tst");
