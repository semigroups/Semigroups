#############################################################################
##
#W  standard/attributes/reesmat-iso.tst
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/attributes/reesmat-iso.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#
gap> R := ReesZeroMatrixSemigroup( Group( [ (2,8), (2,8,6) ] ),
> [ [ 0, (2,8), 0, 0, 0, (2,8,6) ], [ (), 0, (2,8,6), (2,6), (2,6,8), 0 ],
>  [ (2,8,6), 0, (2,6,8), (2,8), (), 0 ], [ (2,8,6), 0, (2,6,8), (2,8), (), 0 ]
>    , [ 0, (2,8,6), 0, 0, 0, (2,8) ], [ (2,8,6), 0, (2,6,8), (2,8), (), 0 ]
> ] );;
gap> AutomorphismGroup(R);
<automorphism group of <Rees 0-matrix semigroup 6x6 over Group([ (2,8), (2,8,
6) ])> with 37 generators>
gap> Size(last);
72

#T# SEMIGROUPS_UnbindVariables

#E#
gap> STOP_TEST( "Semigroups package: standard/attributes/reesmat-iso.tst");
