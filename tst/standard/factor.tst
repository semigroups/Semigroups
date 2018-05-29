#############################################################################
##
#W  standard/factor.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/factor.tst");
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
Error, Semigroups: Factorization: usage,
the second argument <x> is not an element of the first argument <S>,

# factor: Factorization, error, 2/5
gap> S := Semigroup([Transformation([1, 3, 4, 1]),
> Transformation([3, 1, 1, 3])]);;
gap> x := PartialPerm([1, 2, 3], [1, 2, 3]);;
gap> MinimalFactorization(S, x);
Error, Semigroups: MinimalFactorization:
the second argument <x> is not an element of the first argument <S>,

# factor: Factorization, error, 3/5
gap> S := DualSymmetricInverseMonoid(3);;
gap> S := InverseSemigroup(S, rec(acting := true));;
gap> x := PartialPerm([1, 2, 3], [1, 2, 3]);;
gap> Factorization(S, x);
Error, Semigroups: Factorization: usage,
the second argument <x> is not an element of the first argument <S>,

# factor: Factorization, error, 4/5
gap> S := Semigroup(OrderEndomorphisms(3),
>                   rec(acting := true, regular := true));;
gap> x := PartialPerm([1, 2, 3], [1, 2, 3]);;
gap> Factorization(S, x);
Error, Semigroups: Factorization: usage,
the second argument <x> is not an element of the first argument <S>,

# factor: Factorization, error, 5/5
gap> S := Semigroup(OrderEndomorphisms(3), rec(acting := true));;
gap> o := LambdaOrb(S);;
gap> Factorization(o, 2, (1, 2));
Error, Semigroups: Factorization: usage,
the third argument <p> does not belong to the Schutzenberger group,

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

# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(gens);
gap> Unbind(o);
gap> Unbind(s);
gap> Unbind(x);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/factor.tst");
