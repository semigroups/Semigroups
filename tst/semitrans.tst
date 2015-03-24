############################################################################
##
#W  semitrans.tst
#Y  Copyright (C) 2015                                     Wilfred Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: semitrans.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();;

#T# SemiTransTest1
# RepresentativeOfMinimalIdeal and IsSynchronizingSemigroup for T_1
gap> s := Semigroup(Transformation([1]));;
gap> RepresentativeOfMinimalIdeal(s);
IdentityTransformation
gap> IsSynchronizingSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, 1);
true
gap> IsSynchronizingSemigroup(s, 2);
false

#T# SemiTransTest2
gap> ForAll([2 .. 10], x ->
> IsSynchronizingSemigroup(FullTransformationMonoid(x)));
true
gap> for n in [2 .. 10] do
>   Print(RepresentativeOfMinimalIdeal(FullTransformationMonoid(n)), "\n");
> od;
Transformation( [ 1, 1 ] )
Transformation( [ 1, 1, 1 ] )
Transformation( [ 1, 1, 1, 1 ] )
Transformation( [ 1, 1, 1, 1, 1 ] )
Transformation( [ 1, 1, 1, 1, 1, 1 ] )
Transformation( [ 1, 1, 1, 1, 1, 1, 1 ] )
Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1 ] )
Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ] )
Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] )

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(s);

#E#
gap> STOP_TEST("Semigroups package: semitrans.tst");
