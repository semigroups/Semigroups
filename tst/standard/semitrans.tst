############################################################################
##
#W  standard/semitrans.tst
#Y  Copyright (C) 2015                                     Wilfred Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semitrans.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();;

#T# SemiTransTest1
# RepresentativeOfMinimalIdeal and IsSynchronizingSemigroup for T_n
gap> S := Semigroup(Transformation([1]));;
gap> RepresentativeOfMinimalIdeal(S);
IdentityTransformation
gap> IsSynchronizingSemigroup(S);
false
gap> IsSynchronizingSemigroup(S, 1);
true
gap> IsSynchronizingSemigroup(S, 2);
false
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

#T# SemiTransTest2
# IsSynchronizingSemigroup
gap> S := Semigroup([
> Transformation([1, 1, 4, 3, 1]),
> Transformation([2, 1, 3, 4, 2])]);
<transformation semigroup of degree 5 with 2 generators>
gap> IsSynchronizingSemigroup(S);
false
gap> HasRepresentativeOfMinimalIdeal(S);
false
gap> S := Semigroup(S);;
gap> RepresentativeOfMinimalIdeal(S);
Transformation( [ 1, 1, 4, 3, 1 ] )
gap> IsSynchronizingSemigroup(S);
false
gap> S := Semigroup([
> Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
> Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]),
> Transformation([7, 1, 4, 3, 2, 7, 7, 6, 6, 5])]);
<transformation semigroup of degree 10 with 3 generators>
gap> IsSynchronizingSemigroup(S);
true
gap> HasRepresentativeOfMinimalIdeal(S);
false
gap> S := Semigroup(S);;
gap> RepresentativeOfMinimalIdeal(S);
Transformation( [ 7, 7, 7, 7, 7, 7, 7, 7, 7, 7 ] )
gap> IsSynchronizingSemigroup(S);
true
gap> S := Semigroup(S);;
gap> MultiplicativeZero(S);
fail
gap> HasRepresentativeOfMinimalIdeal(S);
true
gap> IsSynchronizingSemigroup(S);
true
gap> S := Semigroup([
> Transformation([4, 6, 5, 4, 3, 9, 10, 2, 2, 9]),
> Transformation([5, 7, 10, 4, 6, 7, 4, 1, 1, 3]),
> Transformation([6, 7, 9, 4, 2, 4, 7, 5, 9, 7])]);
<transformation semigroup of degree 10 with 3 generators>
gap> IsSynchronizingSemigroup(S);
true
gap> HasRepresentativeOfMinimalIdeal(S);
false
gap> S := Semigroup(S);;
gap> RepresentativeOfMinimalIdeal(S);
Transformation( [ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 ] )
gap> IsSynchronizingSemigroup(S);
true
gap> HasMultiplicativeZero(S);
false
gap> S := Semigroup(S);;
gap> MultiplicativeZero(S);
Transformation( [ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 ] )
gap> IsSynchronizingSemigroup(S);
true
gap> S := Semigroup(Transformation([1, 2, 2, 3]));
<commutative transformation semigroup of degree 4 with 1 generator>
gap> MultiplicativeZero(S);
Transformation( [ 1, 2, 2, 2 ] )
gap> IsSynchronizingSemigroup(S);
false

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(n);

#E#
gap> STOP_TEST("Semigroups package: standard/semitrans.tst");
