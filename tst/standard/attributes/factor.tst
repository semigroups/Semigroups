#############################################################################
##
#W  standard/attributes/factor.tst
#Y  Copyright (C) 2011-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local S, T, U, acting, an, gens, o, regular, s, x, y
gap> START_TEST("Semigroups package: standard/attributes/factor.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# FactorTest1: Factorization for a transformation semigroup 1
gap> S := Semigroup(Transformation([3, 4, 1, 2, 1]),
>                   Transformation([4, 2, 1, 5, 5]),
>                   Transformation([4, 2, 2, 2, 4]));;
gap> ForAll(S, x -> EvaluateWord(GeneratorsOfSemigroup(S), 
>                                Factorization(S, x)) = x);
true

# FactorTest2: Factorization for a transformation semigroup 2
gap> gens := [Transformation([1, 3, 2, 3]),
> Transformation([1, 4, 1, 2]),
> Transformation([2, 4, 1, 1]),
> Transformation([3, 4, 2, 2])];;
gap> s := Semigroup(gens);;
gap> ForAll(s, f -> EvaluateWord(gens, Factorization(s, f)) = f);
true

# FactorTest3: Factorization for a transformation semigroup 3
gap> gens := [
> Transformation([4, 6, 5, 2, 1, 3]),
> Transformation([6, 3, 2, 5, 4, 1]),
> Transformation([1, 2, 4, 3, 5, 6]),
> Transformation([3, 5, 6, 1, 2, 3]),
> Transformation([5, 3, 6, 6, 6, 2]),
> Transformation([2, 3, 2, 6, 4, 6]),
> Transformation([2, 1, 2, 2, 2, 4]),
> Transformation([4, 4, 1, 2, 1, 2])];;
gap> s := Semigroup(gens);;
gap> ForAll(s, f -> EvaluateWord(gens, Factorization(s, f)) = f);
true

# FactorTest4: Factorization for a transformation semigroup 4
gap> gens := [
> Transformation([3, 4, 1, 2, 1]),
> Transformation([4, 2, 1, 5, 5]),
> Transformation([4, 2, 2, 2, 4])];;
gap> s := Semigroup(gens);;
gap> ForAll(s, f -> EvaluateWord(gens, Factorization(s, f)) = f);
true

# FactorTest5: Factorization for a transformation semigroup 5
gap> gens := [Transformation([1, 3, 2, 3]),
> Transformation([1, 4, 1, 2]),
> Transformation([2, 4, 1, 1]),
> Transformation([3, 4, 2, 2])];;
gap> s := Semigroup(gens);;
gap> ForAll(s, f -> EvaluateWord(gens, Factorization(s, f)) = f);
true

# FactorTest6: Factorization for a transformation monoid 1
gap> gens := [Transformation([1, 3, 4, 1]),
> Transformation([2, 4, 1, 2]),
> Transformation([3, 1, 1, 3]),
> Transformation([3, 3, 4, 1])];;
> s := Monoid(gens);;
gap> ForAll(s, f -> EvaluateWord(GeneratorsOfSemigroup(s),
> Factorization(s, f)) = f);
true

# FactorTest7: Factorization for a transformation monoid 2
gap> gens := [Transformation([1, 3, 2, 3]),
> Transformation([1, 4, 1, 2]),
> Transformation([3, 4, 2, 2]),
> Transformation([4, 1, 2, 1])];;
> s := Monoid(gens);;
gap> ForAll(s, f -> EvaluateWord(GeneratorsOfSemigroup(s),
> Factorization(s, f)) = f);
true

# FactorTest8: Factorization for a transformation monoid 3
gap> gens := [Transformation([1, 3, 4, 1]),
> Transformation([2, 4, 1, 2]),
> Transformation([3, 1, 1, 3]),
> Transformation([3, 3, 4, 1])];;
gap> s := Monoid(gens);;
gap> ForAll(s, f -> EvaluateWord(GeneratorsOfSemigroup(s),
> Factorization(s, f)) = f);
true

# FactorTest9: Factorization for a transformation monoid 4
gap> gens := [Transformation([1, 3, 2, 3]),
> Transformation([1, 4, 1, 2]),
> Transformation([3, 4, 2, 2]),
> Transformation([4, 1, 2, 1])];;
gap> s := Monoid(gens);;
gap> ForAll(s, f -> EvaluateWord(GeneratorsOfSemigroup(s),
> Factorization(s, f)) = f);
true

# FactorTest10: Factorization for a partial perm semigroup
gap> gens := [PartialPerm([1, 2], [3, 1]),
> PartialPerm([1, 2, 3], [1, 3, 4]),
> PartialPerm([1, 2, 3], [2, 4, 1]),
> PartialPerm([1, 3, 4], [3, 4, 1])];;
gap> s := Semigroup(gens);;
gap> ForAll(s, f -> EvaluateWord(GeneratorsOfSemigroup(s),
> Factorization(s, f)) = f);
true

# FactorTest11: Factorization for a partial perm inverse semigroup
gap> gens := [PartialPerm([1, 2, 4, 5], [2, 6, 1, 4]),
>  PartialPerm([1, 2, 5], [4, 3, 6]),
>  PartialPerm([1, 3, 4, 5], [5, 1, 6, 4]),
>  PartialPerm([1, 3, 4, 5], [5, 2, 6, 1])];;
gap> s := InverseSemigroup(gens);;
gap> ForAll(s, f -> EvaluateWord(GeneratorsOfSemigroup(s),
> Factorization(s, f)) = f);
true

# FactorTest12: Factorization for a known regular semigroup
gap> S := OrderEndomorphisms(7);;
gap> ForAll(S, x -> EvaluateWord(GeneratorsOfSemigroup(S),
>                                Factorization(S, x)) = x);
true

# Test mutability of factorisation and minimal factorisation
gap> S := OrderEndomorphisms(7);;
gap> Factorization(S, Transformation([2, 4, 4, 4, 7, 7, 7]));
[ 7, 8, 4, 5, 6, 7, 2, 2, 3, 4, 2, 6, 5, 7, 8 ]
gap> IsMutable(last);
true
gap> MinimalFactorization(S, Transformation([2, 4, 4, 4, 7, 7, 7]));
[ 4, 3, 5, 7, 8 ]
gap> IsMutable(last);
true

# FactorTest13: Factorization regularity is learned after creation
gap> S := SingularTransformationSemigroup(5);
<regular transformation semigroup ideal of degree 5 with 1 generator>
gap> S := Semigroup(GeneratorsOfSemigroup(S));;
gap> IsRegularSemigroup(S);
true
gap> ForAll(S, x -> EvaluateWord(GeneratorsOfSemigroup(S),
>                                Factorization(S, x)) = x);
true

# factor: Factorization, error, 1/5
gap> S := Semigroup([Transformation([1, 3, 4, 1]),
> Transformation([3, 1, 1, 3])], rec(acting := true));;
gap> x := PartialPerm([1, 2, 3], [1, 2, 3]);;
gap> Factorization(S, x);
Error, the 2nd argument (a mult. elt.) must belong to the 1st argument (a semi\
group)

# factor: Factorization, error, 2/5
gap> S := Semigroup([Transformation([1, 3, 4, 1]),
> Transformation([3, 1, 1, 3])]);;
gap> x := PartialPerm([1, 2, 3], [1, 2, 3]);;
gap> MinimalFactorization(S, x);
Error, the 2nd argument (a mult. elt.) must belong to the 1st argument (a semi\
group)

# factor: Factorization, error, 3/5
gap> S := DualSymmetricInverseMonoid(3);;
gap> S := InverseSemigroup(S, rec(acting := true));;
gap> x := PartialPerm([1, 2, 3], [1, 2, 3]);;
gap> Factorization(S, x);
Error, the 2nd argument (a mult. elt.) must belong to the 1st argument (a semi\
group)

# factor: Factorization, error, 4/5
gap> S := Semigroup(OrderEndomorphisms(3),
>                   rec(acting := true, regular := true));;
gap> x := PartialPerm([1, 2, 3], [1, 2, 3]);;
gap> Factorization(S, x);
Error, the 2nd argument (a mult. elt.) must belong to the 1st argument (a semi\
group)

# factor: Factorization, error, 5/5
gap> S := Semigroup(OrderEndomorphisms(3), rec(acting := true));;
gap> o := LambdaOrb(S);;
gap> Factorization(o, 2, (1, 2));
Error, the 3rd argument <p> does not belong to the Schutzenberger group

# factor: Factorization, () in SchutzenbergerGroup, fail, 1
gap> S := Semigroup(Transformation([1, 1, 2, 3]), rec(acting := true));;
gap> o := LambdaOrb(S);;
gap> Factorization(o, 2, ());
fail

# factor: Factorization, () in SchutzenbergerGroup, 1
gap> S := InverseSemigroup(PartialPerm([1]), rec(acting := true));;
gap> o := LambdaOrb(S);;
gap> Factorization(o, 2, ());
[ 1, -1 ]

# factor: Factorization, () in SchutzenbergerGroup, 2
gap> S := InverseSemigroup(PartialPerm([2, 3, 1]), PartialPerm([2, 1, 3]),
>                          rec(acting := true));;
gap> o := LambdaOrb(S);;
gap> Factorization(o, 2, ());
[ 2, -2 ]

# factor: test for epimorphism from free group returning a word with negative
# powers.
gap> S := Semigroup(FullTransformationMonoid(8), rec(acting := true));;
gap> x := AsTransformation((1, 2, 3, 5));
Transformation( [ 2, 3, 5, 4, 1 ] )
gap> Factorization(S, x);
[ 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, 
  3, 2, 3, 2, 3 ]

# factor: test for use of minimal factorization it is known, non-regular.
gap> S := Semigroup(FullTransformationMonoid(5));;
gap> x := Transformation([4, 3, 2, 4, 1]);;
gap> MinimalFactorization(S, x);
[ 2, 3, 4, 2, 3, 2, 2, 3, 2 ]
gap> Factorization(S, x);
[ 2, 3, 4, 2, 3, 2, 2, 3, 2 ]

# factor: test for use of minimal factorization it is known, regular.
gap> S := PartitionMonoid(4);;
gap> x := Bipartition([[1, 3, -1, -4], [2, 4, -2], [-3]]);;
gap> MinimalFactorization(S, x);
[ 2, 3, 2, 5, 2, 2, 5, 4, 3, 2 ]
gap> Factorization(S, x);
[ 2, 3, 2, 5, 2, 2, 5, 4, 3, 2 ]

# factor: test for use of minimal factorization it is known, regular.
gap> S := SymmetricInverseMonoid(6);;
gap> x := PartialPerm([1, 2, 5], [6, 2, 4]);;
gap> GeneratorsOfSemigroup(S);
[ <identity partial perm on [ 1, 2, 3, 4, 5, 6 ]>, (1,2,3,4,5,6), 
  (1,2)(3)(4)(5)(6), [6,5,4,3,2,1], [1,2,3,4,5,6] ]
gap> MinimalFactorization(S, x);
[ 5, 2, 5, 5, 2, 3 ]
gap> Factorization(S, x);
[ 5, 2, 5, 5, 2, 3 ]

# factor: NonTrivialFactorization, for an fp semigroup, 1
gap> S := FreeSemigroup(1);;
gap> NonTrivialFactorization(S, S.1 ^ 2);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `NonTrivialFactorization' on 2 arguments

# factor: NonTrivialFactorization, for an CanUseFroidurePin semigroup,
# error, 1
gap> S := FullPBRMonoid(1);;
gap> NonTrivialFactorization(S, EmptyPartialPerm());
Error, the 2nd argument (a mult. elt.) must belong to the 1st argument (a semi\
group)

# factor: NonTrivialFactorization, for an CanUseFroidurePin semigroup, 1

# Monogenic semigroup as a non-acting partial perm semigroup
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 3, 2);;
gap> S := Semigroup(S, rec(acting := false));
<commutative partial perm semigroup of rank 4 with 1 generator>
gap> NonTrivialFactorization(S, S.1);
fail
gap> S.1 in IndecomposableElements(S);
true
gap> NonTrivialFactorization(S, S.1 ^ 2);
[ 1, 1 ]

# Monogenic semigroup as a PBR semigroup, with known in-neighbours
gap> S := MonogenicSemigroup(IsPBRSemigroup, 4, 3);
<commutative non-regular pbr semigroup of size 6, degree 7 with 1 generator>
gap> InNeighbours(RightCayleyDigraph(S));
[ [  ], [ 1 ], [ 2 ], [ 3, 6 ], [ 4 ], [ 5 ] ]
gap> NonTrivialFactorization(S, S.1);
fail
gap> S.1 in IndecomposableElements(S);
true
gap> NonTrivialFactorization(S, S.1);
fail
gap> NonTrivialFactorization(S, S.1 ^ 2);
[ 1, 1 ]

# factor: NonTrivialFactorization, for an CanUseFroidurePin semigroup, 2
gap> S := FullPBRMonoid(1);;
gap> Factorization(S, EmptyPBR(1));
[ 1, 3 ]
gap> NonTrivialFactorization(S, EmptyPBR(1));
[ 1, 3 ]

# factor: NonTrivialFactorization, for an CanUseFroidurePin semigroup, 3
gap> gens := [
>  Matrix(IsBooleanMat, [[1, 0, 0], [1, 0, 0], [1, 1, 1]]),
>  Matrix(IsBooleanMat, [[0, 0, 1], [0, 1, 0], [0, 1, 0]]),
>  Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 1], [0, 0, 0]])];;
gap> S := Semigroup(gens);
<semigroup of 3x3 boolean matrices with 3 generators>
gap> x := List(gens, g -> NonTrivialFactorization(S, g));
[ [ 1, 1 ], fail, [ 3, 3 ] ]
gap> EvaluateWord(gens, x[1]) = gens[1] and EvaluateWord(gens, x[3]) = gens[3];
true

# factor: NonTrivialFactorization, for an acting semigroup, error, 1
gap> S := Semigroup(IdentityTransformation, rec(acting := true));
<trivial transformation group of degree 0 with 1 generator>
gap> NonTrivialFactorization(S, EmptyPBR(1));
Error, the 2nd argument (a mult. elt.) must belong to the 1st argument (a semi\
group)

# factor: NonTrivialFactorization, for an acting semigroup, 1
gap> gens := [
>  Transformation([2, 4, 3, 2, 6, 2]),
>  Transformation([2, 1, 5, 1, 1, 1]),
>  Transformation([4, 3, 3, 2, 4, 3]),
>  Transformation([6, 2, 1, 5, 6, 6]),
>  Transformation([3, 5, 2, 1, 3, 3])];;
gap> S := Semigroup(gens, rec(acting := true));;
gap> x := List(gens, g -> NonTrivialFactorization(S, g));
[ [ 1, 5, 1, 5, 1, 5, 1, 5, 1 ], 
  [ 2, 1, 5, 2, 1, 5, 1, 1, 4, 1, 5, 1, 1, 4, 1, 5, 1, 1, 4 ], [ 3, 1, 1 ], 
  [ 5, 1, 5, 1, 5, 1, 5, 1, 4 ], [ 5, 1, 5, 1, 5, 1, 5, 1, 5 ] ]
gap> ForAll([1 .. 5], i -> EvaluateWord(gens, x[i]) = gens[i]);
true

# Re-order the generators
gap> gens := [gens[4], gens[1], gens[2], gens[5], gens[3]];;
gap> S := Semigroup(gens, rec(acting := true));
<transformation semigroup of degree 6 with 5 generators>
gap> x := List(gens, g -> NonTrivialFactorization(S, g));
[ [ 4, 2, 4, 2, 4, 2, 4, 2, 1 ], [ 2, 4, 2, 4, 2, 4, 2, 4, 2 ], 
  [ 3, 2, 4, 2, 4, 2, 4, 2, 4 ], [ 4, 2, 4, 2, 4, 2, 4, 2, 4 ], 
  [ 5, 1, 2, 1, 2, 1, 2, 4, 2, 2 ] ]
gap> ForAll([1 .. 5], i -> EvaluateWord(gens, x[i]) = gens[i]);
true

# factor: NonTrivialFactorization, for an acting semigroup, 2
gap> S := MonogenicSemigroup(IsBipartitionSemigroup, 4, 1);;

# With known indecomposable elements
gap> S := Semigroup(S, rec(acting := true));
<commutative block bijection semigroup of degree 5 with 1 generator>
gap> x := IndecomposableElements(S);
[ <block bijection: [ 1, 2, -1, -5 ], [ 3, -2 ], [ 4, -3 ], [ 5, -4 ]> ]
gap> x := IndecomposableElements(S)[1];;
gap> NonTrivialFactorization(S, x);
fail
gap> NonTrivialFactorization(S, x ^ 2);
[ 1, 1 ]

# Without known incomposable elements
gap> S := Semigroup(S, rec(acting := true));
<commutative block bijection semigroup of degree 5 with 1 generator>
gap> NonTrivialFactorization(S, x);
fail
gap> NonTrivialFactorization(S, x ^ 2);
[ 1, 1 ]

# As a monoid
gap> S := Monoid(S, rec(acting := true));
<commutative block bijection monoid of degree 5 with 1 generator>
gap> NonTrivialFactorization(S, x);
[ 2, 1 ]
gap> NonTrivialFactorization(S, x ^ 2);
[ 2, 2 ]

# With more generators
gap> S := Semigroup(x, x ^ 2);
<block bijection semigroup of degree 5 with 2 generators>
gap> NonTrivialFactorization(S, x);
fail
gap> NonTrivialFactorization(S, x * x);
[ 1, 1 ]

# With generators in a different order
gap> S := Semigroup(x ^ 2, x);
<block bijection semigroup of degree 5 with 2 generators>
gap> NonTrivialFactorization(S, x);
fail
gap> NonTrivialFactorization(S, x * x);
[ 2, 2 ]

# factor: NonTrivialFactorization, for an acting semigroup, 3
gap> gens := [
>  Transformation([4, 4, 7, 4, 4, 4, 8, 8, 8, 1]),
>  Transformation([4, 4, 8, 4, 4, 4, 8, 8, 8, 2]),
>  Transformation([5, 6, 3, 5, 5, 6, 9, 9, 9, 3]),
>  Transformation([4, 4, 7, 4, 4, 4, 8, 8, 8, 7]),
>  Transformation([5, 5, 9, 5, 5, 5, 9, 9, 9, 6])];;
gap> S := Semigroup(gens, rec(acting := true));
<transformation semigroup of degree 10 with 5 generators>
gap> x := List(gens, g -> NonTrivialFactorization(S, g));
[ fail, fail, [ 3, 3 ], [ 3, 4 ], [ 5, 3 ] ]
gap> ForAll([1 .. Length(gens)],
>           i -> x[i] = fail or EvaluateWord(gens, x[i]) = gens[i]);
true

# factor: NonTrivialFactorization, for an acting semigroup, 4
gap> gens := [
>  Transformation([1, 6, 6, 5, 5, 1]),
>  Transformation([3, 5, 2, 5, 4, 3, 6]),
>  Transformation([5, 6, 4, 6, 7, 3, 2]),
>  Transformation([1, 2, 2, 6, 4, 3, 5]),
>  Transformation([6, 3, 6, 7, 5, 6, 1]),
>  Transformation([2, 6, 7, 6, 1, 3, 4])];;
gap> S := Semigroup(gens, rec(acting := true));
<transformation semigroup of degree 7 with 6 generators>
gap> x := List(gens, g -> NonTrivialFactorization(S, g));
[ [ 1, 3, 3, 1, 3, 3, 1 ], [ 6, 4, 3, 6, 4, 3, 2 ], fail, fail, 
  [ 5, 3, 5, 3, 5 ], fail ]
gap> ForAll([1 .. Length(gens)],
>           i -> x[i] = fail or EvaluateWord(gens, x[i]) = gens[i]);
true

# Repeat, with known PartialOrderOfDClasses
gap> PartialOrderOfDClasses(S);;
gap> x := List(gens, g -> NonTrivialFactorization(S, g));
[ [ 1, 3, 3, 1, 3, 3, 1 ], [ 6, 4, 3, 6, 4, 3, 2 ], fail, fail, 
  [ 5, 3, 5, 3, 5 ], fail ]

# factor: NonTrivialFactorization, for an acting semigroup, 5
gap> gens := [
>  Bipartition([[1, 3, 7, -2], [2, -5], [4], [5, -1, -3], [6], [-4, -6], [-7]]),
>  Bipartition([[1, 3, 5, -6], [2, 4, 6, 7, -2], [-1, -5], [-3], [-4], [-7]]),
>  Bipartition([[1, 3, -5], [2, 4, 7, -2, -4], [5], [6, -1, -6], [-3], [-7]]),
>  Bipartition([[1, 2, 7, -5], [3, 6, -4], [4, -1], [5, -2, -3, -6], [-7]]),
>  Bipartition([[1, 2, 3, 4, 7, -3], [5], [6, -1, -2, -5, -7], [-4], [-6]]),
>  Bipartition([[1, 3, -5], [2, 6, -3], [4, 5, -6], [7, -1, -7], [-2], [-4]])];;
gap> S := Semigroup(gens, rec(acting := true));
<bipartition semigroup of degree 7 with 6 generators>
gap> x := List(gens, g -> NonTrivialFactorization(S, g));
[ [ 1, 1, 1, 1 ], [ 2, 3, 1, 2, 3, 1, 2 ], fail, fail, 
  [ 3, 1, 2, 3, 1, 2, 5 ], fail ]
gap> ForAll([1 .. Length(gens)],
>           i -> x[i] = fail or EvaluateWord(gens, x[i]) = gens[i]);
true

# factor: NonTrivialFactorization, for an acting semigroup, 6
gap> S := Monoid([
>  Transformation([2, 3, 4, 5, 6, 7, 8, 1]),
>  Transformation([2, 1]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 1]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 9])], rec(acting := true));
<transformation monoid of degree 11 with 4 generators>
gap> gens := GeneratorsOfSemigroup(S);;
gap> x := List(GeneratorsOfSemigroup(S), g -> NonTrivialFactorization(S, g));
[ [ 1, 1 ], [ 2, 1 ], [ 3, 1 ], [ 4, 1 ], [ 5, 1 ] ]
gap> ForAll([1 .. Length(gens)], i -> EvaluateWord(gens, x[i]) = gens[i]);
true

# factor: NonTrivialFactorization, for an acting semigroup, 7
gap> gens := [
>  Transformation([2, 3, 4, 5, 6, 7, 8, 1, 8, 8, 8]),
>  Transformation([2, 1, 3, 4, 5, 6, 7, 8, 8, 8, 8]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 1, 8, 8, 8]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 9])];;
gap> S := Semigroup(gens, rec(acting := true));
<transformation semigroup of degree 11 with 4 generators>
gap> x := List(gens, g -> NonTrivialFactorization(S, g));
[ [ 1, 2, 2 ], [ 2, 2, 2 ], [ 3, 2, 2 ], fail ]
gap> ForAll([1 .. Length(gens)],
>           i -> x[i] = fail or EvaluateWord(gens, x[i]) = gens[i]);
true

# factor: NonTrivialFactorization, for a semigroup, 8
gap> gens := [
>  Transformation([1, 6, 6, 5, 5, 1]),
>  Transformation([3, 5, 2, 5, 4, 3, 6]),
>  Transformation([5, 6, 4, 6, 7, 3, 2]),
>  Transformation([1, 2, 2, 6, 4, 3, 5]),
>  Transformation([6, 3, 6, 7, 5, 6, 1]),
>  Transformation([2, 6, 7, 6, 1, 3, 4])];;
gap> S := Semigroup(gens, rec(acting := true));
<transformation semigroup of degree 7 with 6 generators>
gap> x := List(gens, x -> NonTrivialFactorization(S, x));;
gap> y := Filtered([1 .. 6], i -> x[i] <> fail);
[ 1, 2, 5 ]
gap> ForAll(y, i -> EvaluateWord(gens, x[i]) = gens[i]);
true

# factor: NonTrivialFactorization, for a semigroup, 9
gap> gens := [
>  Transformation([2, 3, 4, 5, 6, 7, 8, 1]),
>  Transformation([2, 1]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 1]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 9])];;
gap> S := Monoid(gens, rec(acting := true));
<transformation monoid of degree 11 with 4 generators>
gap> x := List(gens, x -> NonTrivialFactorization(S, x));;
gap> ForAll([1 .. 4],
>           i -> EvaluateWord(GeneratorsOfSemigroup(S), x[i]) = gens[i]);
true
gap> S := Semigroup(gens, rec(acting := true));
<transformation semigroup of degree 11 with 4 generators>
gap> x := List(gens, x -> NonTrivialFactorization(S, x));;
gap> ForAll([1 .. 4], i -> EvaluateWord(gens, x[i]) = gens[i]);
true

# factor: NonTrivialFactorization, for a semigroup, 10
gap> gens := [
>  Transformation([2, 3, 4, 5, 6, 7, 8, 1, 8, 8, 8]),
>  Transformation([2, 1, 3, 4, 5, 6, 7, 8, 8, 8, 8]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 1, 8, 8, 8]),
>  Transformation([1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 9])];;
gap> S := Semigroup(gens, rec(acting := true));
<transformation semigroup of degree 11 with 4 generators>
gap> x := List(gens, x -> NonTrivialFactorization(S, x));;
gap> y := Filtered([1 .. 4], i -> x[i] <> fail);
[ 1, 2, 3 ]
gap> ForAll(y, i -> EvaluateWord(gens, x[i]) = gens[i]);
true

# factor: NonTrivialFactorization, for CanUseFroidurePin and acting
# semigroups, 1
gap> gens := [
>  PBR([[1, 2], [-2, 1, 2]], [[-1, 1, 2], [-2, 1, 2]]),
>  PBR([[1], []], [[-2, -1, 2], [-1]]),
>  PBR([[-1, 1, 2], [-2, -1, 1, 2]], [[-2, -1, 1, 2], [-1, 1, 2]]),
>  PBR([[-2, -1, 1, 2], [-2, -1, 1, 2]], [[-2, 1, 2], [-1]]),
>  PBR([[-2, 1], [-2, -1, 2]], [[-2, -1], [-2, -1, 2]]),
>  PBR([[2], []], [[], [-2]]),
>  PBR([[-2, -1, 2], [-2, -1, 1, 2]], [[-2, -1, 1, 2], [-2, -1, 1, 2]]),
>  PBR([[-2, 2], []], [[-1, 1], [-2, 1]]),
>  PBR([[-2], [-1]], [[-2, 2], [-1, 1]])];;
gap> S := Semigroup(gens);
<pbr semigroup of degree 2 with 9 generators>
gap> x := List(gens, x -> NonTrivialFactorization(S, x));
[ [ 3, 9, 8 ], [ 2, 2 ], [ 3, 9, 9 ], [ 4, 9, 9 ], [ 5, 9, 9 ], [ 6, 6 ], 
  [ 7, 9 ], [ 9, 9, 8 ], [ 9, 9, 9 ] ]
gap> ForAll([1 .. Length(gens)], i -> EvaluateWord(gens, x[i]) = gens[i]);
true
gap> S := AsSemigroup(IsBipartitionSemigroup, S);;
gap> S := Semigroup(S, rec(acting := true));
<bipartition semigroup of degree 226 with 9 generators>
gap> gens := GeneratorsOfSemigroup(S);;
gap> x := List(gens, x -> NonTrivialFactorization(S, x));
[ [ 3, 9, 8 ], [ 2, 2 ], [ 3, 9, 9 ], [ 4, 9, 9 ], [ 5, 9, 9 ], [ 6, 6 ], 
  [ 7, 9 ], [ 9, 9, 8 ], [ 9, 9, 9 ] ]
gap> ForAll([1 .. Length(gens)], i -> EvaluateWord(gens, x[i]) = gens[i]);
true

# factor: NonTrivialFactorization, for an inverse acting semigroup, error, 1
gap> S := SymmetricInverseMonoid(1);;
gap> NonTrivialFactorization(S, IdentityTransformation);
Error, the 2nd argument (a mult. elt.) must belong to the 1st argument (a semi\
group)

# factor: NonTrivialFactorization, for an inverse acting semigroup, 1
gap> S := SymmetricInverseMonoid(4);
<symmetric inverse monoid of degree 4>
gap> gens := GeneratorsOfSemigroup(S);;

# As semigroup
gap> S := Semigroup(gens, rec(acting := true));
<partial perm monoid of rank 4 with 4 generators>
gap> x := List(gens, x -> NonTrivialFactorization(S, x));
[ [ 1, 1 ], [ 2, 1 ], [ 3, 1 ], [ 4, 1 ], [ 5, 1 ] ]
gap> ForAll([1 .. Length(gens)], i -> EvaluateWord(gens, x[i]) = gens[i]);
true

# As inverse semigroup
gap> S := InverseSemigroup(gens, rec(acting := true));
<inverse partial perm monoid of rank 4 with 4 generators>
gap> x := List(gens, x -> NonTrivialFactorization(S, x));
[ [ 1, -1, 1 ], [ 2, -2, 2 ], [ 3, -3, 3 ], [ 4, -4, 4 ], [ 5, -5, 5 ] ]
gap> ForAll([1 .. Length(gens)], i -> EvaluateWord(gens, x[i]) = gens[i]);
true

# factor: NonTrivialFactorization, for an inverse acting semigroup, 2
gap> gens := [
>  Bipartition([[1, 2, -1, -2], [3, -4], [4, -3]]),
>  Bipartition([[1, 2, -1, -4], [3, -2], [4, -3]]),
>  Bipartition([[1, 2, -1, -3], [3, -4], [4, -2]]),
>  Bipartition([[1, 2, -2, -4], [3, -1], [4, -3]]),
>  Bipartition([[1, 2, -3], [3, -1, -2], [4, -4]]),
>  Bipartition([[1, 2, -3, -4], [3, -1], [4, -2]]),
>  Bipartition([[1, 2, -2, -3], [3, -4], [4, -1]]),
>  Bipartition([[1, 3, -1, -2], [2, -4], [4, -3]]),
>  Bipartition([[1, -4], [2, 3, -1, -2], [4, -3]]),
>  Bipartition([[1, 4, -1, -2], [2, -3], [3, -4]]),
>  Bipartition([[1, -3], [2, 4, -1, -2], [3, -4]]),
>  Bipartition([[1, -3], [2, -4], [3, 4, -1, -2]])];;

# As semigroup
gap> S := Semigroup(gens, rec(acting := true));
<block bijection semigroup of degree 4 with 12 generators>
gap> x := List(gens, x -> NonTrivialFactorization(S, x));;
gap> ForAll([1 .. Length(gens)], i -> EvaluateWord(gens, x[i]) = gens[i]);
true

# As inverse semigroup
gap> S := InverseSemigroup(gens, rec(acting := true));
<inverse block bijection semigroup of degree 4 with 12 generators>
gap> x := List(gens, x -> NonTrivialFactorization(S, x));;
gap> ForAll([1 .. Length(gens)], i -> EvaluateWord(gens, x[i]) = gens[i]);
true
gap> NonTrivialFactorization(S, gens[1] ^ 2) = Factorization(S, gens[1] ^ 2);
true

# Factorization for an inverse semigroup - code coverage
gap> x := PartialPerm([1, 2, 3, 6], [6, 4, 5, 1]);
[2,4][3,5](1,6)
gap> S := SymmetricInverseMonoid(5);
<symmetric inverse monoid of degree 5>
gap> Factorization(S, x);
Error, the 2nd argument (a mult. elt.) must belong to the 1st argument (a semi\
group)
gap> S := AsSet(SymmetricInverseMonoid(3));;
gap> T := InverseSemigroup(GeneratorsSmallest(SymmetricInverseMonoid(3)));
<inverse partial perm monoid of rank 3 with 14 generators>
gap> ForAll(S, x -> EvaluateWord(GeneratorsOfSemigroup(T), 
>                                Factorization(T, x)) = x);
true

# Factorization for an regular semigroup - code coverage
gap> U := FullTransformationMonoid(3);;
gap> S := AsSet(U);;
gap> T := Semigroup(GeneratorsSmallest(U));;
gap> IsRegularSemigroup(T);
true
gap> ForAll(S, x -> EvaluateWord(GeneratorsOfSemigroup(T), 
>                                Factorization(T, x)) = x);
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/attributes/factor.tst");
