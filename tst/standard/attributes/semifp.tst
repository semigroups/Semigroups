#############################################################################
##
#W  standard/attributes/semifp.tst
#Y  Copyright (C) 2017-2022                                 Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local F, S
gap> START_TEST("Semigroups package: standard/attributes/semifp.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#  properties: IndecomposableElements, for an fp semigroup, 1
gap> F := FreeSemigroup(1);
<free semigroup on the generators [ s1 ]>
gap> S := F / [];;
gap> IndecomposableElements(S);
[ s1 ]
gap> S := F / [[F.1, F.1]];
<fp semigroup with 1 generator and 1 relation of length 3>
gap> IndecomposableElements(S);
[ s1 ]
gap> S := F / [[F.1 ^ 3, F.1]];
<fp semigroup with 1 generator and 1 relation of length 5>
gap> IndecomposableElements(S);
[  ]
gap> S := F / [[F.1 ^ 3, F.1 ^ 2]];
<fp semigroup with 1 generator and 1 relation of length 6>
gap> IndecomposableElements(S);
[ s1 ]
gap> S := F / [[F.1 ^ 3, F.1]];
<fp semigroup with 1 generator and 1 relation of length 5>
gap> IsMonoidAsSemigroup(S);
true
gap> HasIsSurjectiveSemigroup(S);
true
gap> IndecomposableElements(S);
[  ]
gap> F := FreeSemigroup(3);
<free semigroup on the generators [ s1, s2, s3 ]>
gap> S := F / [[F.2, F.1], [F.2 ^ 3, F.2], [F.2, F.3]];
<fp semigroup with 3 generators and 3 relations of length 11>
gap> IndecomposableElements(S);
[  ]

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/attributes/semifp.tst");
