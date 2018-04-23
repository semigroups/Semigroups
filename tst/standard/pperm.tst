#############################################################################
##
##  standard/pperm.tst
#Y  Copyright (C) 2017                                      Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/pperm.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# pperm: CyclesOfPartialPerm
gap> x := PartialPerm([14, 4, 13, 2, 6, 0, 0, 0, 10, 0, 3, 0, 1, 12]);
[5,6][9,10][11,3,13,1,14,12](2,4)
gap> CyclesOfPartialPerm(x);
[ [ 4, 2 ] ]

# 
gap> x := PartialPerm([14, 4, 13, 2, 6, 0, 0, 0, 10, 0, 3, 0, 1, 12]);
[5,6][9,10][11,3,13,1,14,12](2,4)
gap> IndexPeriodOfSemigroupElement(x);
[ 6, 2 ]

# SEMIGROUPS_UnbindVariables
gap> Unbind(x);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/pperm.tst");
