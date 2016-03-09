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

#T# BruteForceIsoCheck helper functions
gap> BruteForceIsoCheck := function(iso)
>   local x, y;
>   if not IsInjective(iso) or not IsSurjective(iso) then
>     return false;
>   fi;
>   for x in Source(iso) do
>     for y in Source(iso) do
>       if x ^ iso * y ^ iso <> (x * y) ^ iso then
>         return false;
>       fi;
>     od;
>   od;
>   return true;
> end;;
gap> BruteForceInverseCheck := function(map)
> local inv;
>   inv := InverseGeneralMapping(map);
>   return ForAll(Source(map), x -> x = (x ^ map) ^ inv)
>     and ForAll(Range(map), x -> x = (x ^ inv) ^ map);
> end;;

#T# isomorphism from RMS to transformation semigroup
gap> S := RectangularBand(IsReesMatrixSemigroup, 5, 5);;
gap> map := IsomorphismSemigroup(IsTransformationSemigroup, S);
MappingByFunction( <Rees matrix semigroup 5x5 over Group(())>, 
<transformation semigroup of size 25, degree 26 with 5 generators>
 , function( x ) ... end, function( x ) ... end )
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# isomorphism from RZMS to transformation semigroup
gap> S := ZeroSemigroup(IsReesZeroMatrixSemigroup, 10);;
gap> map := IsomorphismSemigroup(IsTransformationSemigroup, S);
MappingByFunction( <Rees 0-matrix semigroup 9x1 over Group(())>, 
<transformation semigroup of size 10, degree 11 with 9 generators>
 , function( x ) ... end, function( x ) ... end )
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# isomorphism from fp semigroup to transformation semigroup
gap> S := AsSemigroup(IsFpSemigroup, JonesMonoid(5));;
gap> map := IsomorphismSemigroup(IsTransformationSemigroup, S);
MappingByFunction( <fp semigroup on the generators [ s1, s2, s3, s4, s5 ]>, 
<transformation semigroup of degree 43 with 5 generators>
 , function( x ) ... end )
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# isomorphism from pbr semigroup to transformation semigroup
gap> S := FullPBRMonoid(1);;
gap> map := IsomorphismSemigroup(IsTransformationSemigroup, S);
MappingByFunction( <pbr monoid of size 16, degree 1 with 4 generators>, 
<transformation monoid of size 16, degree 16 with 4 generators>
 , function( x ) ... end, function( x ) ... end )
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# isomorphism from bipartition semigroup to transformation semigroup
gap> S := Semigroup(
> Bipartition([[1, 4, 6, 7, 8, 10], [2, 5, -1, -2, -8],
>              [3, -3, -6, -7, -9], [9, -4, -5], [-10]]),
> Bipartition([[1, 2, 6, 7, -3, -4, -6], [3, 4, 5, 10, -2, -10],
>              [8, -8], [9, -1], [-5], [-7, -9]]));;
gap> map := IsomorphismSemigroup(IsTransformationSemigroup, S);
MappingByFunction( <bipartition semigroup of size 7, degree 10 with 2 
 generators>, <transformation semigroup of size 7, degree 8 with 2 generators>
 , function( x ) ... end, function( x ) ... end )
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# isomorphism from block bijection semigroup to transformation semigroup
gap> S := Semigroup([Bipartition([[1, 3, -2, -5], [2, 4, -1], [5, -3, -4]]),
>  Bipartition([[1, 3, -1], [2, 4, -2, -3], [5, -4, -5]]),
>  Bipartition([[1, 4, 5, -2], [2, 3, -1, -3, -4, -5]]),
>  Bipartition([[1, -5], [2, 3, -1, -2], [4, -4], [5, -3]]),
>  Bipartition([[1, 2, -2], [3, -3, -4, -5], [4, 5, -1]])]);
<block bijection semigroup of degree 5 with 5 generators>
gap> map := IsomorphismSemigroup(IsTransformationSemigroup, S);
MappingByFunction( <block bijection semigroup of size 54, degree 5 with 5 
 generators>, <transformation semigroup of size 54, degree 55 with 5 
 generators>, function( x ) ... end, function( x ) ... end )
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# isomorphism from transformation semigroup to transformation semigroup
gap> S := Semigroup(Transformation([5, 2, 2, 3, 2]),
>                   Transformation([1, 4, 2, 3, 4]));;
gap> map := IsomorphismSemigroup(IsTransformationSemigroup, S);
MappingByFunction( <transformation semigroup of degree 5 with 2 generators>, 
<transformation semigroup of degree 5 with 2 generators>
 , function( object ) ... end, function( object ) ... end )
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# isomorphism from partial perm semigroup to transformation semigroup
gap> S := Semigroup(PartialPerm([1, 2, 3, 4], [4, 5, 1, 2]),
>                   PartialPerm([1, 2, 4], [1, 3, 5]));;
gap> map := IsomorphismSemigroup(IsTransformationSemigroup, S);
MappingByFunction( <partial perm semigroup of rank 4 with 2 generators>, 
<transformation semigroup of degree 6 with 2 generators>
 , function( x ) ... end, <Operation "AsPartialPerm"> )
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# isomorphism from boolean mat semigroup to transformation semigroup
gap> S := Monoid(Matrix(IsBooleanMat, [[0, 1], [1, 0]]),
>                Matrix(IsBooleanMat, [[0, 1], [1, 0]]),
>                Matrix(IsBooleanMat, [[1, 0], [1, 1]]),
>                Matrix(IsBooleanMat, [[1, 0], [0, 0]]));;
gap> map := IsomorphismSemigroup(IsTransformationSemigroup, S);
MappingByFunction( <monoid of 2x2 boolean matrices with 4 generators>, 
<transformation monoid of degree 4 with 4 generators>
 , function( x ) ... end, function( x ) ... end )
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# isomorphism from max plus mat semigroup to transformation semigroup
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[0, -4], [-4, -1]]),
>                   Matrix(IsMaxPlusMatrix, [[0, -3], [-3, -1]]));;
gap> map := IsomorphismSemigroup(IsTransformationSemigroup, S);
MappingByFunction( <semigroup of size 26, 2x2 max-plus matrices with 2 
 generators>, <transformation semigroup of size 26, degree 27 with 2 
 generators>, function( x ) ... end, function( x ) ... end )
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# isomorphism from min plus mat semigroup to transformation semigroup
gap> S := Semigroup(Matrix(IsMinPlusMatrix, [[0, 4], [4, 1]]),
>                   Matrix(IsMinPlusMatrix, [[0, 3], [3, 1]]));;
gap> map := IsomorphismSemigroup(IsTransformationSemigroup, S);
MappingByFunction( <semigroup of size 26, 2x2 min-plus matrices with 2 
 generators>, <transformation semigroup of size 26, degree 27 with 2 
 generators>, function( x ) ... end, function( x ) ... end )
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# isomorphism from tropical max plus mat semigroup to transformation
#   semigroup
gap> S := Semigroup(Matrix(IsTropicalMaxPlusMatrix, [[0, 4], [4, 1]], 10),
>                   Matrix(IsTropicalMaxPlusMatrix, [[0, 3], [3, 1]], 10));;
gap> map := IsomorphismSemigroup(IsTransformationSemigroup, S);
MappingByFunction( <semigroup of size 13, 2x2 tropical max-plus matrices with 
 2 generators>, <transformation semigroup of size 13, degree 14 with 2 
 generators>, function( x ) ... end, function( x ) ... end )
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# isomorphism from tropical min plus mat semigroup to transformation
#   semigroup
gap> S := Semigroup(Matrix(IsTropicalMinPlusMatrix, [[0, 4], [4, 1]], 5),
>                   Matrix(IsTropicalMinPlusMatrix, [[0, 3], [3, 1]], 5));;
gap> map := IsomorphismSemigroup(IsTransformationSemigroup, S);
MappingByFunction( <semigroup of size 18, 2x2 tropical min-plus matrices with 
 2 generators>, <transformation semigroup of size 18, degree 19 with 2 
 generators>, function( x ) ... end, function( x ) ... end )
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# isomorphism from ntp mat semigroup to transformation
#   semigroup. This is the general linear semigroup over the field with 3
#   elements
gap> S := Monoid(
> Matrix(IsNTPMatrix, [[1, 0, 0],
>                      [0, 2, 0],
>                      [0, 0, 2]],
>                     0, 3),
> Matrix(IsNTPMatrix, [[1, 0, 2],
>                      [1, 0, 0],
>                      [0, 1, 0]],
>                     0, 3),
> Matrix(IsNTPMatrix, [[2, 0, 0],
>                      [0, 2, 0],
>                      [0, 0, 0]],
>                     0, 3));
<monoid of 3x3 ntp matrices with 3 generators>
gap> IsomorphismSemigroup(IsTransformationSemigroup, S);
MappingByFunction( <monoid of size 19683, 3x3 ntp matrices with 3 generators>
 , <transformation monoid of size 19683, degree 19683 with 3 generators>
 , function( x ) ... end, function( x ) ... end )

#T# isomorphism from ntp mat semigroup to transformation
#   semigroup. This is the general linear semigroup over the field with 2
#   elements
gap> S := Monoid(
> Matrix(IsNTPMatrix, [[1, 1], [0, 1]], 0, 2),
> Matrix(IsNTPMatrix, [[0, 1], [1, 0]], 0, 2),
> Matrix(IsNTPMatrix, [[1, 0], [0, 0]], 0, 2));
<monoid of 2x2 ntp matrices with 3 generators>
gap> map := IsomorphismSemigroup(IsTransformationSemigroup, S);
MappingByFunction( <monoid of size 16, 2x2 ntp matrices with 3 generators>, 
<transformation monoid of size 16, degree 16 with 3 generators>
 , function( x ) ... end, function( x ) ... end )
gap> BruteForceInverseCheck(map);
true
gap> BruteForceIsoCheck(map);
true

#T# isomorphism from an integer mat semigroup to transformation semigroup
gap> S := Semigroup(
>  Matrix(IsIntegerMatrix, [[0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
>        , [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]),
>  Matrix(IsIntegerMatrix, [[0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
>        , [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]),
>  Matrix(IsIntegerMatrix, [[1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
>        , [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]),
>  Matrix(IsIntegerMatrix, [[0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
>        , [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>      [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]));;
gap> map := IsomorphismSemigroup(IsTransformationSemigroup, S);
MappingByFunction( <semigroup of size 16, 17x17 integer matrices with 4 
 generators>, <transformation semigroup of size 16, degree 17 with 4 
 generators>, function( x ) ... end, function( x ) ... end )
gap> BruteForceInverseCheck(map);
true
gap> BruteForceIsoCheck(map);
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(R);
gap> Unbind(x);
gap> Unbind(n);
gap> Unbind(gr);

#E#
gap> STOP_TEST("Semigroups package: standard/semitrans.tst");
