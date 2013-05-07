#############################################################################
##
#W  testcompiled.tst
#Y  Copyright (C) 2011-12                               James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# ReadTest(Filename(DirectoriesPackageLibrary("semigroups","tst"),
# "testcompiled.tst"));
gap> START_TEST("Semigroups package: testcompiled.tst");
gap> LoadPackage( "semigroups", false );;

#
gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoSemigroups:=InfoLevel(InfoSemigroups);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoSemigroups, 0);

gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoSemigroups, InfoLevelInfoSemigroups);;
gap> Unbind(InfoLevelInfoSemigroups);; Unbind(InfoLevelInfoWarning);;
gap> STOP_TEST( "Semigroups package: testcompiled.tst", 10000);
