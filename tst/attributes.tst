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

# MultiplicativeZero for T_1 (previously this crashed)
gap> s := Semigroup(t); # with displaying the semigroup
<trivial transformation group>
gap> MultiplicativeZero(s) = t;
true
gap> s := Semigroup(t);; # not displaying the semigroup
gap> MultiplicativeZero(s) = t;
true

# MultiplicativeZero for trivial transformation monoid with different rep.
gap> t := Transformation( [ 2, 2, 3, 3 ] );;
gap> s := Semigroup(t);;
gap> MultiplicativeZero(s) = t;
true

# MultiplicativeZero: issue #121 on Bitbucket
gap> s := Semigroup([
> Transformation( [ 1, 2, 1 ] ),
> Transformation( [ 1, 2, 2 ] ) ]);;
gap> MultiplicativeZero(s);
fail
gap> ForAny(s, x -> IsMultiplicativeZero(s, x));
false

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(s);
gap> Unbind(t);

#E#
gap> STOP_TEST( "Semigroups package: attributes.tst");

