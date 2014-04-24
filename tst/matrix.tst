#############################################################################
##
#W  matrix.tst
#Y  Copyright (C) 2014                               Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: matrix.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels and user preferences
gap> SemigroupsStartTest();


#
#
gap> SemigroupsStopTest();
gap> STOP_TEST( "Semigroups package: matrix.tst", 10000);
