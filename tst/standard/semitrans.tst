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

#T# SemiTransTest3
# FixedPoints for a transformation semigroup with generators
gap> S := FullTransformationMonoid(3);;
gap> FixedPoints(S);
[  ]
gap> S := Semigroup([
> Transformation([1, 2, 4, 4, 5, 6]),
> Transformation([1, 1, 3, 4, 6, 6]),
> Transformation([1, 1, 3, 4, 5, 6])]);;
gap> FixedPoints(S);
[ 1, 4, 6 ]

#T# SemiTransTest4
# MovedPoints for a transformation semigroup with generators
gap> S := FullTransformationMonoid(4);;
gap> MovedPoints(S);
[ 1 .. 4 ]
gap> S := Semigroup([
> Transformation([1, 2, 3, 4, 5, 6]),
> Transformation([1, 1, 3, 4, 6, 6]),
> Transformation([1, 1, 4, 4, 5, 6])]);;
gap> MovedPoints(S);
[ 2, 3, 5 ]

#T# SemiTransTest5
# \^ for a transformation semigroup and a transformation or a permutation
gap> S := FullTransformationMonoid(4);;
gap> S ^ () = S;
true
gap> S ^ (1, 3) = S;
true
gap> S := S ^ (1, 5)(2, 4, 6)(3, 7);
<transformation monoid of degree 7 with 3 generators>
gap> Size(S) = 4 ^ 4;
true
gap> GeneratorsOfSemigroup(S);
[ IdentityTransformation, Transformation( [ 1, 2, 3, 7, 4, 5, 6 ] ), 
  Transformation( [ 1, 2, 3, 5, 4 ] ), Transformation( [ 1, 2, 3, 4, 5, 5 ] ) 
 ]
gap> S := Semigroup([Transformation([2, 6, 7, 2, 6, 1, 1, 5]), 
>  Transformation([3, 8, 1, 4, 5, 6, 7, 1]), 
>  Transformation([4, 3, 2, 7, 7, 6, 6, 5])]);;
gap> GeneratorsOfSemigroup(S ^ (1, 7, 8, 6, 10)(3, 9, 5, 4));
[ Transformation( [ 1, 10, 2, 10, 5, 4, 2, 7, 8, 7 ] ), 
  Transformation( [ 1, 6, 3, 4, 5, 7, 9, 8, 7 ] ), 
  Transformation( [ 1, 9, 8, 8, 5, 4, 3, 10, 2, 10 ] ) ]

#T# SemiTransTest6
# DigraphOfActionOnPoints for a transformation semigroup (and a pos int)
gap> DigraphOfActionOnPoints(FullTransformationSemigroup(4));
<digraph with 4 vertices, 9 edges>
gap> OutNeighbours(last);
[ [ 1, 2 ], [ 1, 2, 3 ], [ 3, 4 ], [ 1, 4 ] ]
gap> S := Semigroup([Transformation([2, 6, 7, 2, 6, 1, 1, 5]), 
>  Transformation([3, 8, 1, 4, 5, 6, 7, 1]), 
>  Transformation([4, 3, 2, 7, 7, 6, 6, 5])]);;
gap> DigraphOfActionOnPoints(S);
<digraph with 8 vertices, 22 edges>
gap> OutNeighbours(last);
[ [ 2, 3, 4 ], [ 3, 6, 8 ], [ 1, 2, 7 ], [ 2, 4, 7 ], [ 5, 6, 7 ], [ 1, 6 ], 
  [ 1, 6, 7 ], [ 1, 5 ] ]

#T# SemiTransTest7
# SEMIGROUPS.SmallestElementRClass
gap> x := Transformation([1, 1, 2]);;
gap> S := Semigroup(x, rec(generic := false));
<commutative transformation semigroup of degree 3 with 1 generator>
gap> SEMIGROUPS.SmallestElementRClass(RClass(S, x)) = x;
true
gap> S := FullTransformationMonoid(5);;
gap> S := Semigroup(S, rec(generic := false));;
gap> R := RClass(S, Transformation([3, 2, 4, 2, 1]));
<Green's R-class: Transformation( [ 3, 2, 4, 2, 1 ] )>
gap> SEMIGROUPS.SmallestElementRClass(R);
Transformation( [ 1, 2, 3, 2, 4 ] )
gap> S := Semigroup([Transformation([4, 4, 3, 6, 5, 1]), 
>  Transformation([5, 4, 5, 4, 6, 6])]);;
gap> S := Semigroup(S, rec(generic := false));;
gap> R := RClass(S, Transformation([4, 4, 5, 6, 6, 5]));
<Green's R-class: Transformation( [ 4, 4, 5, 6, 6, 5 ] )>
gap> SEMIGROUPS.SmallestElementRClass(R);
Transformation( [ 1, 1, 5, 4, 4, 5 ] )

#T# SemiTransTest8
# SEMIGROUPS.LargestElementRClass and SEMIGROUPS.SmallestElementRClass
gap> x := Transformation([1, 1, 2]);;
gap> S := Semigroup(x, rec(generic := false));
<commutative transformation semigroup of degree 3 with 1 generator>
gap> SEMIGROUPS.LargestElementRClass(RClass(S, x)) = x;
true
gap> S := FullTransformationMonoid(5);;
gap> S := Semigroup(S, rec(generic := false));;
gap> R := RClass(S, Transformation([4, 2, 4, 1, 2]));
<Green's R-class: Transformation( [ 1, 3, 1, 2, 3 ] )>
gap> SEMIGROUPS.LargestElementRClass(R);
Transformation( [ 5, 4, 5, 3, 4 ] )
gap> S := Semigroup([Transformation([4, 4, 3, 6, 5, 1]), 
>  Transformation([5, 4, 5, 4, 6, 6])]);;
gap> S := Semigroup(S, rec(generic := false));;
gap> R := RClass(S, Transformation([4, 4, 5, 6, 6, 5]));
<Green's R-class: Transformation( [ 4, 4, 5, 6, 6, 5 ] )>
gap> SEMIGROUPS.LargestElementRClass(R);
Transformation( [ 6, 6, 5, 1, 1, 5 ] )

#T# SemiTransTest9
# Idempotents for a transformation semigroup and a pos int
gap> Idempotents(FullTransformationMonoid(3), 4);
[  ]
gap> Idempotents(FullTransformationMonoid(3), 3);
[ IdentityTransformation ]
gap> Idempotents(FullTransformationMonoid(3), 2);
[ Transformation( [ 1, 2, 1 ] ), Transformation( [ 1, 2, 2 ] ), 
  Transformation( [ 3, 2, 3 ] ), Transformation( [ 2, 2 ] ), 
  Transformation( [ 1, 3, 3 ] ), Transformation( [ 1, 1 ] ) ]
gap> Idempotents(FullTransformationMonoid(3), 1);
[ Transformation( [ 1, 1, 1 ] ), Transformation( [ 2, 2, 2 ] ), 
  Transformation( [ 3, 3, 3 ] ) ]
gap> S := Semigroup([Transformation([5, 1, 3, 1, 4, 2, 5, 2]), 
>  Transformation([7, 1, 7, 4, 2, 5, 6, 3]), 
>  Transformation([8, 4, 6, 5, 7, 8, 8, 7])]);;
gap> Idempotents(S, 9);
[  ]
gap> Length(Idempotents(S, 3));
988
gap> AsSet(Idempotents(S, 1));
[ Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 3, 3, 3, 3, 3, 3, 3, 3 ] ), 
  Transformation( [ 4, 4, 4, 4, 4, 4, 4, 4 ] ), 
  Transformation( [ 5, 5, 5, 5, 5, 5, 5, 5 ] ), 
  Transformation( [ 6, 6, 6, 6, 6, 6, 6, 6 ] ), 
  Transformation( [ 7, 7, 7, 7, 7, 7, 7, 7 ] ), 
  Transformation( [ 8, 8, 8, 8, 8, 8, 8, 8 ] ) ]

#T# SemiTransTest10
# IteratorSorted for a transformation semigroup
gap> true;;

#T# SemiTransTest11
# IteratorSorted for a transformation semigroup R-class
gap> true;;

#T# SemiTransTest12
# IsTransformationSemigroupGreensClass
gap> S := Semigroup(PartialPerm([2, 3], [1, 4]));;
gap> R := RClass(S, RepresentativeOfMinimalIdeal(S));
<Green's R-class: <empty partial perm>>
gap> IsTransformationSemigroupGreensClass(R);
false
gap> S := Semigroup([Transformation([1, 3, 4, 1, 3, 5]), 
>  Transformation([5, 1, 6, 1, 6, 3])]);;
gap> R := HClass(S, Transformation([4, 5, 3, 4, 5, 5]));
<Green's H-class: Transformation( [ 4, 5, 3, 4, 5, 5 ] )>
gap> IsTransformationSemigroupGreensClass(R);
true

#T# SemiTransTest13
# EndomorphismMonoid
gap> gr := Digraph([[1, 2], [1, 2]]);;
gap> GeneratorsOfEndomorphismMonoidAttr(gr);
[ Transformation( [ 2, 1 ] ), IdentityTransformation, 
  Transformation( [ 1, 1 ] ) ]
gap> S := EndomorphismMonoid(gr);
<transformation monoid of degree 2 with 2 generators>
gap> Elements(S);
[ Transformation( [ 1, 1 ] ), IdentityTransformation, 
  Transformation( [ 2, 1 ] ), Transformation( [ 2, 2 ] ) ]
gap> S := EndomorphismMonoid(Digraph([[1, 2], [1, 2]]), [1, 1]);
<transformation monoid of degree 2 with 2 generators>
gap> Elements(S);
[ Transformation( [ 1, 1 ] ), IdentityTransformation, 
  Transformation( [ 2, 1 ] ), Transformation( [ 2, 2 ] ) ]
gap> S := EndomorphismMonoid(Digraph([[1, 2], [1, 2]]));
<transformation monoid of degree 2 with 2 generators>
gap> Elements(S);
[ Transformation( [ 1, 1 ] ), IdentityTransformation, 
  Transformation( [ 2, 1 ] ), Transformation( [ 2, 2 ] ) ]
gap> IsFullTransformationMonoid(S);
true
gap> S := EndomorphismMonoid(Digraph([[2], [2]]));
<commutative transformation monoid of degree 2 with 1 generator>
gap> Elements(S);
[ IdentityTransformation, Transformation( [ 2, 2 ] ) ]
gap> S := EndomorphismMonoid(Digraph([[2], [2]]), [1, 1]);
<commutative transformation monoid of degree 2 with 1 generator>
gap> Elements(S);
[ IdentityTransformation, Transformation( [ 2, 2 ] ) ]
gap> S := EndomorphismMonoid(Digraph([[2], [2]]), [1, 2]);
<trivial transformation group of degree 0 with 1 generator>

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(R);
gap> Unbind(x);
gap> Unbind(n);
gap> Unbind(gr);

#E#
gap> STOP_TEST("Semigroups package: standard/semitrans.tst");
