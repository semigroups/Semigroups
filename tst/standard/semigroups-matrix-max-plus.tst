#############################################################################
##
#W  semigroups-matrix-max-plus.tst
#Y  Copyright (C) 2015                                  James D. Mitchell
##                                                       
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

gap> START_TEST("Semigroups package: semigroups-matrix-max-plus.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

# semigroups-matrix-max-plus: C++ code working
gap> S := Semigroup(MaxPlusMatrixNC([[0, -4], [-4, -1]]),
>                   MaxPlusMatrixNC([[0, -3], [-3, -1]]));
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

#E# 
gap> STOP_TEST( "Semigroups package: semigroups-matrix-max-plus.tst");
