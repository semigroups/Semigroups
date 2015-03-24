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
# RepresentativeOfMinimalIdeal and IsSynchronizingSemigroup for T_n
gap> s := Semigroup(Transformation([1]));;
gap> RepresentativeOfMinimalIdeal(s);
IdentityTransformation
gap> IsSynchronizingSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, 1);
true
gap> IsSynchronizingSemigroup(s, 2);
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
gap> s := Semigroup([
> Transformation([ 1, 1, 4, 3, 1 ]),
> Transformation([ 2, 1, 3, 4, 2 ]) ]);
<transformation semigroup on 5 pts with 2 generators>
gap> IsSynchronizingSemigroup(s);
false
gap> HasRepresentativeOfMinimalIdeal(s);
false
gap> s := Semigroup(s);;
gap> RepresentativeOfMinimalIdeal(s);
Transformation( [ 1, 1, 4, 3, 1 ] )
gap> IsSynchronizingSemigroup(s);
false
gap> s := Semigroup([
> Transformation( [ 2, 6, 7, 2, 6, 9, 9, 1, 1, 5 ] ), 
> Transformation( [ 3, 8, 1, 9, 9, 4, 10, 5, 10, 6 ] ), 
> Transformation( [ 7, 1, 4, 3, 2, 7, 7, 6, 6, 5 ] ) ]);
<transformation semigroup on 10 pts with 3 generators>
gap> IsSynchronizingSemigroup(s);
true
gap> HasRepresentativeOfMinimalIdeal(s);
false
gap> s := Semigroup(s);;
gap> RepresentativeOfMinimalIdeal(s);
Transformation( [ 7, 7, 7, 7, 7, 7, 7, 7, 7, 7 ] )
gap> IsSynchronizingSemigroup(s);
true
gap> s := Semigroup(s);;
gap> MultiplicativeZero(s);
fail
gap> HasRepresentativeOfMinimalIdeal(s);
true
gap> IsSynchronizingSemigroup(s);
true
gap> s := Semigroup([
> Transformation( [ 4, 6, 5, 4, 3, 9, 10, 2, 2, 9 ] ), 
> Transformation( [ 5, 7, 10, 4, 6, 7, 4, 1, 1, 3 ] ), 
> Transformation( [ 6, 7, 9, 4, 2, 4, 7, 5, 9, 7 ] ) ]);
<transformation semigroup on 10 pts with 3 generators>
gap> IsSynchronizingSemigroup(s);
true
gap> HasRepresentativeOfMinimalIdeal(s);
false
gap> s := Semigroup(s);;
gap> RepresentativeOfMinimalIdeal(s);
Transformation( [ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 ] )
gap> IsSynchronizingSemigroup(s);
true
gap> HasMultiplicativeZero(s);
false
gap> s := Semigroup(s);;
gap> MultiplicativeZero(s);
Transformation( [ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 ] )
gap> IsSynchronizingSemigroup(s);
true
gap> s := Semigroup(Transformation([ 1, 2, 2, 3 ]));
<commutative transformation semigroup on 4 pts with 1 generator>
gap> MultiplicativeZero(s);
Transformation( [ 1, 2, 2, 2 ] )
gap> IsSynchronizingSemigroup(s);
false

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(s);
gap> Unbind(n);

#E#
gap> STOP_TEST("Semigroups package: semitrans.tst");
