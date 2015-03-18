#############################################################################
##
#W  attributes.tst
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: attributes.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#T# AttributesTest1: MultiplicativeZero
gap> t := Transformation( [ 1 ] );;
gap> s := Semigroup(t);
<trivial transformation group>
gap> MultiplicativeZero(s) = t;
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(s);

#E#
gap> STOP_TEST( "Semigroups package: attributes.tst");

