############################################################################
##
#W  standard/semigroups/semidp.tst
#Y  Copyright (C) 2017-2022                                 Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local BruteForceHomomCheck, D, ProductCheck, S, T, e, embeds, gens, i, list
#@local nrfactors, p, projects, s, smap, t, tmap
gap> START_TEST("Semigroups package: standard/semigroups/semidp.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();;

################################################################################
# Helper functions
################################################################################

#  semifp: Embedding and Projection testers
gap> BruteForceHomomCheck := function(map)
>   local s, t, smap, tmap, S;
>   S := Source(map);
>   for s in S do
>     for t in S do
>       smap := s ^ map;
>       tmap := t ^ map;
>       if smap * tmap <> (s * t) ^ map then
>         return false;
>       fi;
>     od;
>   od;
>   return true;
> end;;
gap> ProductCheck := function(product, arguments, homom)
>   local e, p, i;
>   for i in [1 .. Length(arguments)] do
>     e := Embedding(product, i);
>     p := Projection(product, i);
>     if arguments[i] <> Source(e)
>         or arguments[i] <> Range(p)
>         or (homom and not BruteForceHomomCheck(e))
>         or (homom and not BruteForceHomomCheck(p))
>         or not ForAll(arguments[i], s -> (s ^ e) ^ p = s) then
>       return false;
>     fi;
>   od;
>   return true;
> end;;

################################################################################
# Testing errors
################################################################################

#  semidp: SEMIGROUPS.DirectProductOp, errors, 1
gap> SEMIGROUPS.DirectProductOp(fail, fail, fail, fail, fail);
Error, the 1st argument is not a non-empty list
gap> SEMIGROUPS.DirectProductOp([], fail, fail, fail, fail);
Error, the 1st argument is not a non-empty list

#  semidp: DirectProductOp, for transformation semigroups, errors, 1
gap> DirectProductOp([], Monoid(Transformation([1, 1])));
Error, the 1st argument (a list) is not non-empty
gap> DirectProductOp([], Semigroup(Transformation([1, 1])));
Error, the 1st argument (a list) is not non-empty
gap> DirectProductOp([Semigroup(PartialPerm([1 .. 3]))],
> Monoid(Transformation([1, 1])));
Error, the 2nd argument is not one of the semigroups contained in the 1st argu\
ment (a list)
gap> DirectProductOp([Semigroup(PartialPerm([1 .. 3]))],
> Semigroup(Transformation([1, 1])));
Error, the 2nd argument is not one of the semigroups contained in the 1st argu\
ment (a list)

#  semidp: DirectProductOp, for partial perm semigroups, errors, 1
gap> DirectProductOp([], Semigroup(PartialPerm([1 .. 3])));
Error, the 1st argument (a list) is not non-empty
gap> DirectProductOp([Semigroup(IdentityTransformation)],
> Semigroup(PartialPerm([1 .. 3])));
Error, the 2nd argument is not one of the semigroups contained in the 1st argu\
ment (a list)

#  semidp: DirectProductOp, for bipartition semigroups, errors, 1
gap> DirectProductOp([], PartitionMonoid(1));
Error, the 1st argument (a list) is not non-empty
gap> DirectProductOp([PartitionMonoid(1)], PartitionMonoid(2));
Error, the 2nd argument is not one of the semigroups contained in the 1st argu\
ment (a list)

#  semidp: DirectProductOp, for PBR semigroups, errors, 1
gap> DirectProductOp([], FullPBRMonoid(1));
Error, the 1st argument (a list) is not non-empty
gap> DirectProductOp([FullPBRMonoid(1)], FullPBRMonoid(2));
Error, the 2nd argument is not one of the semigroups contained in the 1st argu\
ment (a list)

#  semidp: DirectProductOp, for a mix of semigroups, errors, 1
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[(), (1, 2)], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
gap> T := ReesMatrixSemigroup(SymmetricGroup(4), [[(1, 4, 3)], [()]]);
<Rees matrix semigroup 1x2 over Sym( [ 1 .. 4 ] )>
gap> DirectProductOp([], S);
Error, the 1st argument (a list) is not non-empty
gap> DirectProductOp([T], S);
Error, the 2nd argument is not one of the semigroups contained in the 1st argu\
ment (a list)
gap> DirectProductOp([S, T, FreeSemigroup(1)], S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `DirectProductOp' on 2 arguments

################################################################################
# Testing transformation semigroups
################################################################################

#  semidp: DirectProduct, for transformation semigroups, 1
gap> S := ZeroSemigroup(IsTransformationSemigroup, 2);
<commutative non-regular transformation semigroup of size 2, degree 3 with 1 
 generator>
gap> D := DirectProduct(S);;
gap> ProductCheck(D, [S], true);
true
gap> S = D;
true

#  semidp: DirectProduct, for transformation semigroups, 2
gap> S := ZeroSemigroup(IsTransformationSemigroup, 2);
<commutative non-regular transformation semigroup of size 2, degree 3 with 1 
 generator>
gap> D := DirectProduct(S, S, S);;
gap> IsRegularSemigroup(D);
false
gap> D;
<commutative non-regular transformation semigroup of size 8, degree 9 with 7 
 generators>
gap> HasIndecomposableElements(D);
true
gap> IndecomposableElements(D);
[ Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 8 ] ), 
  Transformation( [ 1, 1, 1, 4, 4, 5, 7, 7, 7 ] ), 
  Transformation( [ 1, 1, 1, 4, 4, 5, 7, 7, 8 ] ), 
  Transformation( [ 1, 1, 2, 4, 4, 4, 7, 7, 7 ] ), 
  Transformation( [ 1, 1, 2, 4, 4, 4, 7, 7, 8 ] ), 
  Transformation( [ 1, 1, 2, 4, 4, 5, 7, 7, 7 ] ), 
  Transformation( [ 1, 1, 2, 4, 4, 5, 7, 7, 8 ] ) ]
gap> HasMinimalSemigroupGeneratingSet(D);
true
gap> MinimalSemigroupGeneratingSet(D) = IndecomposableElements(D);
true
gap> ProductCheck(D, [S, S, S], true);
true
gap> D := Semigroup(D);
<transformation semigroup of degree 9 with 7 generators>
gap> IsZeroSemigroup(D) and Size(D) = 8;
true
gap> IndecomposableElements(D);
[ Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 8 ] ), 
  Transformation( [ 1, 1, 1, 4, 4, 5, 7, 7, 7 ] ), 
  Transformation( [ 1, 1, 1, 4, 4, 5, 7, 7, 8 ] ), 
  Transformation( [ 1, 1, 2, 4, 4, 4, 7, 7, 7 ] ), 
  Transformation( [ 1, 1, 2, 4, 4, 4, 7, 7, 8 ] ), 
  Transformation( [ 1, 1, 2, 4, 4, 5, 7, 7, 7 ] ), 
  Transformation( [ 1, 1, 2, 4, 4, 5, 7, 7, 8 ] ) ]

#  semidp: DirectProduct, for transformation semigroups, 3
gap> S := ZeroSemigroup(IsTransformationSemigroup, 3);
<commutative non-regular transformation semigroup of size 3, degree 4 with 2 
 generators>
gap> D := DirectProduct(S, S);;
gap> IsRegularSemigroup(D);
false
gap> D;
<commutative non-regular transformation semigroup of size 9, degree 8 with 8 
 generators>
gap> ProductCheck(D, [S, S], true);
true
gap> D := Semigroup(D);
<transformation semigroup of degree 8 with 8 generators>
gap> IsZeroSemigroup(D) and Size(D) = 9;
true

#  semidp: DirectProduct, for transformation semigroups, 4
gap> S := FullTransformationMonoid(3);
<full transformation monoid of degree 3>
gap> D := DirectProduct(S, S);;
gap> IsRegularSemigroup(D);
true
gap> D;
<regular transformation monoid of size 729, degree 6 with 6 generators>
gap> ProductCheck(D, [S, S], false);
true
gap> HasIndecomposableElements(D);
true
gap> IndecomposableElements(D);
[  ]
gap> Set(GeneratorsOfMonoid(D));
[ Transformation( [ 1, 2, 1 ] ), Transformation( [ 1, 2, 3, 4, 5, 4 ] ), 
  Transformation( [ 1, 2, 3, 5, 4 ] ), Transformation( [ 1, 2, 3, 5, 6, 4 ] ),
  Transformation( [ 2, 1 ] ), Transformation( [ 2, 3, 1 ] ) ]
gap> Size(D) = Size(S) ^ 2 and Size(D) = 27 ^ 2;
true

#  semidp: DirectProduct, for transformation semigroups, 5
gap> S := Semigroup([
> Transformation([1, 2, 3, 3, 3]),
> Transformation([1, 1, 3, 3, 3])]);
<transformation semigroup of degree 5 with 2 generators>
gap> D := DirectProduct(S, S);;
gap> ProductCheck(D, [S, S], true);
true

#  semidp: DirectProduct, for transformation semigroups, 6
gap> list := [
>  Semigroup([Transformation([1, 2, 3, 3, 3])]),
>  Semigroup([Transformation([2, 1])])];;
gap> D := DirectProduct(list);
<commutative transformation semigroup of degree 7 with 2 generators>
gap> ProductCheck(D, list, true);
true
gap> Elements(D);
[ Transformation( [ 1, 2, 3, 3, 3 ] ), 
  Transformation( [ 1, 2, 3, 3, 3, 7, 6 ] ) ]
gap> Size(D);
2

#  semidp: DirectProduct, for transformation semigroups, 7
gap> list := [
>  Semigroup([Transformation([2, 1])]),
>  Semigroup([Transformation([1, 2, 3, 3, 3])])];;
gap> D := DirectProduct(list);
<commutative transformation semigroup of degree 7 with 2 generators>
gap> ProductCheck(D, list, true);
true
gap> Elements(D);
[ Transformation( [ 1, 2, 3, 4, 5, 5, 5 ] ), 
  Transformation( [ 2, 1, 3, 4, 5, 5, 5 ] ) ]
gap> Size(D);
2

#  semidp: DirectProduct, for transformation semigroups, 8
gap> list := [
>  Semigroup([Transformation([1, 2, 3, 3, 3])]),
>  Semigroup([Transformation([2, 1])]),
>  Semigroup([Transformation([1, 1, 2, 3, 4])])];;
gap> D := DirectProduct(list);;
gap> ProductCheck(D, list, true);
true
gap> Size(D);
8
gap> Elements(D);
[ Transformation( [ 1, 2, 3, 3, 3, 6, 7, 8, 8, 8, 8, 8 ] ), 
  Transformation( [ 1, 2, 3, 3, 3, 6, 7, 8, 8, 8, 8, 9 ] ), 
  Transformation( [ 1, 2, 3, 3, 3, 6, 7, 8, 8, 8, 9, 10 ] ), 
  Transformation( [ 1, 2, 3, 3, 3, 6, 7, 8, 8, 9, 10, 11 ] ), 
  Transformation( [ 1, 2, 3, 3, 3, 7, 6, 8, 8, 8, 8, 8 ] ), 
  Transformation( [ 1, 2, 3, 3, 3, 7, 6, 8, 8, 8, 8, 9 ] ), 
  Transformation( [ 1, 2, 3, 3, 3, 7, 6, 8, 8, 8, 9, 10 ] ), 
  Transformation( [ 1, 2, 3, 3, 3, 7, 6, 8, 8, 9, 10, 11 ] ) ]

################################################################################
# Testing partial perm semigroups
################################################################################

#  semidp: DirectProduct, for partial perm semigroups, 1
gap> S := Semigroup([PartialPerm([3], [3]), PartialPerm([2], [1])]);;
gap> Size(S);
3
gap> D := DirectProduct(S);;
gap> ProductCheck(D, [S], true);
true
gap> S = D;
true

#  semidp: DirectProduct, for partial perm semigroups, 2
gap> S := Semigroup([PartialPerm([3], [3]), PartialPerm([2], [1])]);;
gap> D := DirectProduct(S, S);;
gap> ProductCheck(D, [S, S], true);
true
gap> Size(Semigroup(D)) = 9;
true

#  semidp: DirectProduct, for partial perm semigroups, 3
gap> T := SymmetricInverseMonoid(3);
<symmetric inverse monoid of degree 3>
gap> D := DirectProduct(T, T, T);
<inverse partial perm monoid of rank 9 with 9 generators>
gap> ProductCheck(D, [T, T, T], false);
true
gap> D := InverseMonoid(D);
<inverse partial perm monoid of rank 9 with 9 generators>
gap> Size(D) = Size(T) ^ 3;
true

#  semidp: DirectProduct, for partial perm semigroups, 4
gap> S := ZeroSemigroup(IsPartialPermSemigroup, 3);
<commutative non-regular partial perm semigroup of size 3, rank 2 with 2 
 generators>
gap> T := SymmetricInverseMonoid(3);
<symmetric inverse monoid of degree 3>
gap> D := DirectProduct(S, T, S);;
gap> IsRegularSemigroup(D);
false
gap> D;
<non-regular partial perm semigroup of size 306, rank 7 with 272 generators>
gap> ProductCheck(D, [S, T, S], false);
true

#  semidp: DirectProduct, for partial perm semigroups, 5
gap> T := SymmetricInverseMonoid(3);
<symmetric inverse monoid of degree 3>
gap> T := SemigroupIdeal(T, PartialPerm([1, 2]));;
gap> T := InverseSemigroup(T);;
gap> D := DirectProduct(T, T, T);;
gap> ProductCheck(D, [T, T, T], false);
true
gap> D := Semigroup(D);;
gap> Size(D) = Size(T) ^ 3;
true

#  semidp: DirectProduct, for partial perm semigroups, 6
gap> S := DirectProduct(SymmetricInverseMonoid(3), SymmetricInverseMonoid(3));
<inverse partial perm monoid of rank 6 with 6 generators>
gap> Size(S) = 34 ^ 2;
true

#  semidp: DirectProduct, for partial perm semigroups, 7
gap> S := Semigroup([PartialPerm([3], [3]), PartialPerm([2], [1])]);
<partial perm semigroup of rank 2 with 2 generators>
gap> Size(S);
3
gap> DirectProduct(S) = S;
true

#  semidp: DirectProduct, for partial perm semigroups, 8
gap> S := Semigroup([PartialPerm([3], [3]), PartialPerm([2], [1])]);
<partial perm semigroup of rank 2 with 2 generators>
gap> T := DirectProduct(S, S);
<partial perm semigroup of size 9, rank 4 with 6 generators>
gap> ProductCheck(T, [S, S], true);
true
gap> T := Semigroup(T);
<partial perm semigroup of rank 4 with 6 generators>
gap> Size(T) = Size(S) ^ 2;
true
gap> D := DirectProduct(S, S, S);
<partial perm semigroup of size 27, rank 6 with 20 generators>
gap> ProductCheck(D, [S, S, S], false);
true
gap> D = DirectProduct(T, S);
true
gap> D := Semigroup(D);
<partial perm semigroup of rank 6 with 20 generators>
gap> Size(D) = Size(S) ^ 3;
true

#  semidp: DirectProductOp, for partial perm semigroups, 9
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 5, 3);
<commutative non-regular partial perm semigroup of size 7, rank 7 with 1 
 generator>
gap> T := SymmetricInverseMonoid(3);
<symmetric inverse monoid of degree 3>
gap> D := DirectProduct(S, T);;
gap> IsRegularSemigroup(D);
false
gap> D;
<non-regular partial perm semigroup of size 238, rank 10 with 34 generators>
gap> ProductCheck(D, [S, T], false);
true
gap> D := Semigroup(D);
<partial perm semigroup of rank 10 with 34 generators>
gap> Size(D);
238
gap> Size(D) = Size(S) * Size(T);
true

################################################################################
# Testing bipartition semigroups
################################################################################

#  semidp: DirectProductOp, for bipartition semigroups, 1
gap> S := Semigroup([
>  Bipartition([[1, 3], [2, 4, -2], [5, -1, -3, -4], [-5]]),
>  Bipartition([[1, 2, 3, 4, 5, -1, -5], [-2], [-3], [-4]]),
>  Bipartition([[1, 2, 3, -1, -5], [4], [5, -2, -3, -4]])]);;
gap> T := Semigroup([
>  Bipartition([[1, 3, 4, 5, 6, -1, -4], [2, -2, -3, -5, -6]]),
>  Bipartition([[1, 2, 3, 4, 5, -5], [6, -1, -2, -3, -4], [-6]])]);;
gap> D := DirectProduct(S, T);
<bipartition semigroup of size 100, degree 11 with 46 generators>
gap> ProductCheck(D, [S, T], false);
true
gap> D := Semigroup(D);
<bipartition semigroup of degree 11 with 46 generators>
gap> Size(D);
100
gap> Size(S) * Size(T);
100
gap> D := DirectProduct(T, S);
<bipartition semigroup of size 100, degree 11 with 46 generators>
gap> ProductCheck(D, [T, S], false);
true
gap> D := Semigroup(D);
<bipartition semigroup of degree 11 with 46 generators>
gap> Size(D);
100

#  semidp: DirectProductOp, for bipartition semigroups, 2
gap> S := ZeroSemigroup(IsBipartitionSemigroup, 3);
<commutative non-regular bipartition semigroup of size 3, degree 4 with 2 
 generators>
gap> T := ZeroSemigroup(IsBipartitionSemigroup, 5);
<commutative non-regular bipartition semigroup of size 5, degree 5 with 4 
 generators>
gap> D := DirectProduct(S, T);;
gap> IsRegularSemigroup(D);
false
gap> D;
<commutative non-regular bipartition semigroup of size 15, degree 9 with 14 
 generators>
gap> ProductCheck(D, [S, T], true);
true
gap> D := Semigroup(D);;
gap> IsZeroSemigroup(D) and Size(D) = Size(S) * Size(T);
true
gap> D := DirectProduct(T, S);;
gap> IsRegularSemigroup(D);
false
gap> D;
<commutative non-regular bipartition semigroup of size 15, degree 9 with 14 
 generators>
gap> ProductCheck(D, [T, S], true);
true
gap> D := Semigroup(D);
<bipartition semigroup of degree 9 with 14 generators>
gap> IsZeroSemigroup(D) and Size(D) = Size(S) * Size(T);
true
gap> D := DirectProduct(S, PartitionMonoid(2), T);;
gap> IsRegularSemigroup(D);
false
gap> D;
<non-regular bipartition semigroup of size 225, degree 11 with 210 generators>
gap> ProductCheck(D, [S, PartitionMonoid(2), T], false);
true
gap> D := Semigroup(D);
<bipartition semigroup of degree 11 with 210 generators>
gap> Size(D);
225

#  semidp: DirectProductOp, for bipartition semigroups, 3
gap> S := PartitionMonoid(2);
<regular bipartition *-monoid of size 15, degree 2 with 3 generators>
gap> D := DirectProduct(S, S, S, S);;
gap> IsRegularSemigroup(D);
true
gap> D;
<regular bipartition monoid of size 50625, degree 8 with 12 generators>
gap> ProductCheck(D, [S, S, S, S], false);
true
gap> D := Monoid(D);
<bipartition monoid of degree 8 with 12 generators>
gap> Size(D) = 15 ^ 4;
true

#  semidp: DirectProductOp, for bipartition semigroups, 4
gap> list := [
>  Semigroup([
>    Bipartition([[1, 2, 3, -3], [-1, -2]]),
>    Bipartition([[1, 2, 3, -1, -2], [-3]])]),
>  Semigroup([
>    Bipartition([[1], [2, -2], [3, -3], [-1]]),
>    Bipartition([[1, 2, -1, -2, -3], [3]])]),
>  Semigroup([Bipartition([[1, 3, -1, -2, -3], [2]]),
>    Bipartition([[1, 3, -1, -2], [2, -3]])])];;
gap> D := DirectProduct(list);;
gap> ProductCheck(D, list, true);
true

################################################################################
# Testing PBR semigroups
################################################################################

#  semidp: DirectProductOp, for PBR semigroups, 1
gap> S := FullPBRMonoid(1);;
gap> D := DirectProduct(S);;
gap> IsRegularSemigroup(D);
true
gap> D;
<regular pbr monoid of size 16, degree 1 with 4 generators>
gap> ProductCheck(D, [S], true);
true
gap> D := DirectProduct(S, S);;
gap> IsRegularSemigroup(D);
true
gap> D;
<regular pbr monoid of size 256, degree 2 with 8 generators>
gap> ProductCheck(D, [S, S], false);
true
gap> Size(D);
256

#  semidp: DirectProductOp, for PBR semigroups, 2
gap> list := [
>  Semigroup([
>    PBR([[1, 2], [-2, 1, 2]], [[-1, 1], [-2, -1, 1]]),
>    PBR([[2], [-2, 1, 2]], [[1, 2], [-2, 1]])]),
>  Semigroup([
>    PBR([[-1, 1]], [[1]])]),
>  Semigroup([
>    PBR([[], [-2, -1]], [[], [2]]),
>    PBR([[-2, -1], []], [[2], []])])];;
gap> D := DirectProduct(list);
<pbr semigroup of size 35, degree 5 with 15 generators>
gap> ProductCheck(D, list, true);
true

################################################################################
# Testing mixing semigroups
################################################################################

#  semidp: DirectProductOp, for a mix of semigroups, 1
gap> S := [
> PartitionMonoid(1),
> FullTransformationMonoid(1),
> SymmetricInverseMonoid(1)];;
gap> D := DirectProductOp(S, S[1]);
<commutative transformation monoid of degree 4 with 2 generators>
gap> D := DirectProductOp(S, S[2]);
<commutative transformation monoid of degree 4 with 2 generators>
gap> D := DirectProductOp(S, S[3]);
<commutative transformation monoid of degree 4 with 2 generators>
gap> last = last2 and last2 = last3;
true
gap> ProductCheck(D, S, true);
true
gap> Size(D);
4

#  semidp: DirectProductOp, for a mix of semigroups, 2
gap> S := [
> PartitionMonoid(1),
> FullPBRMonoid(1),
> FullTransformationMonoid(1),
> SymmetricInverseMonoid(1)];;
gap> D := DirectProductOp(S, S[2]);
<transformation monoid of degree 20 with 6 generators>
gap> ProductCheck(D, S, false);
true

#  semidp: DirectProductOp, for a mix of semigroups, 3
gap> S := PartitionMonoid(1);;
gap> T := FullTransformationMonoid(1);;
gap> D := DirectProduct(S, T);;
gap> IsInverseSemigroup(D);
true
gap> D;
<commutative inverse transformation monoid of size 2, degree 2 with 1 
 generator>
gap> ProductCheck(D, [S, T], true);
true
gap> D := DirectProduct(T, S);;
gap> IsInverseMonoid(D);
true
gap> D;
<commutative inverse transformation monoid of size 2, degree 2 with 1 
 generator>
gap> ProductCheck(D, [T, S], true);
true

################################################################################
# Other special direct product functions
################################################################################

# Embedding

#  semidp: Embedding, for a semigroup with direct product info, 1
gap> S := LeftZeroSemigroup(3);;
gap> Embedding(S, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `Embedding' on 2 arguments
gap> D := DirectProduct(S, S, S);;
gap> Unbind(SemigroupDirectProductInfo(D).embeddings);
gap> Embedding(D, 4);
Error, the 2nd argument (a pos. int.) is not in the range [1 .. 3]
gap> Embedding(D, 1);
<transformation semigroup of size 3, degree 4 with 3 generators> -> 
<transformation semigroup of size 27, degree 12 with 27 generators>
gap> Embedding(D, 1) = last;
true
gap> Unbind(SemigroupDirectProductInfo(D).embeddings[1]);
gap> Unbind(SemigroupDirectProductInfo(D).nrfactors);
gap> Embedding(D, 1);
Error, the direct product information for the 1st argument (a semigroup) is co\
rrupted, please re-create the object
gap> SemigroupDirectProductInfo(D).nrfactors := 3;;
gap> Embedding(D, 3);
<transformation semigroup of size 3, degree 4 with 3 generators> -> 
<transformation semigroup of size 27, degree 12 with 27 generators>
gap> Unbind(SemigroupDirectProductInfo(D).embeddings[3]);
gap> Unbind(SemigroupDirectProductInfo(D).embedding);
gap> Embedding(D, 3);
Error, the direct product information for the 1st argument (a semigroup) is co\
rrupted, please re-create the object

#  semidp: Embedding and Projection, for a semigroup with direct product info
gap> list := [
>  Semigroup([
>    Transformation([5, 1, 4, 1, 1]),
>    Transformation([5, 3, 3, 4, 5]),
>    Transformation([4, 1, 4, 5, 1])]),
>  Semigroup([
>    Transformation([6, 4, 4, 4, 6, 5, 2]),
>    Transformation([6, 5, 1, 7, 5, 1, 7])]),
>  Semigroup([
>    Transformation([3, 2, 2]),
>    Transformation([2, 1, 1]),
>    Transformation([2, 1, 2])])];
[ <transformation semigroup of degree 5 with 3 generators>, 
  <transformation semigroup of degree 7 with 2 generators>, 
  <transformation semigroup of degree 3 with 3 generators> ]
gap> D := DirectProduct(list);;
gap> e := [];;
gap> e[1] := Embedding(D, 1);;
gap> e[2] := Embedding(D, 2);;
gap> e[3] := Embedding(D, 3);;
gap> p := [];;
gap> p[1] := Projection(D, 1);;
gap> p[2] := Projection(D, 2);;
gap> p[3] := Projection(D, 3);;
gap> ProductCheck(D, list, false);
true
gap> gens := List([1 .. 3],
>         i -> List(GeneratorsOfSemigroup(list[i]), x -> x ^ e[i]));;
gap> embeds := List([1 .. 3], i -> Semigroup(gens[i]));
[ <transformation semigroup of degree 15 with 3 generators>, 
  <transformation semigroup of degree 15 with 2 generators>, 
  <transformation semigroup of degree 15 with 3 generators> ]
gap> ForAll([1 .. 3], i -> Size(list[i]) = Size(embeds[i]));
true
gap> projects := List([1 .. 3], i -> ImagesSet(p[i], embeds[i]));;
gap> ForAll([1 .. 3], i -> Size(list[i]) = Size(projects[i]));
true

# Projection

#  semidp: Projection, for a semigroup with direct product info, 1
gap> S := RightZeroSemigroup(3);;
gap> Projection(S, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `Projection' on 2 arguments
gap> D := DirectProduct(S, S, S);;
gap> Unbind(SemigroupDirectProductInfo(D).projections);
gap> Projection(D, 4);
Error, the 2nd argument (a pos. int.) is not in the range [1 .. 3]
gap> Projection(D, 1);
<transformation semigroup of size 27, degree 9 with 27 generators> -> 
<transformation semigroup of size 3, degree 3 with 3 generators>
gap> Projection(D, 1) = last;
true
gap> Unbind(SemigroupDirectProductInfo(D).projections[1]);
gap> Unbind(SemigroupDirectProductInfo(D).nrfactors);
gap> Projection(D, 1);
Error, the direct product information for the 1st argument (a semigroup) is co\
rrupted, please re-create the object
gap> SemigroupDirectProductInfo(D).nrfactors := 3;;
gap> Projection(D, 3);
<transformation semigroup of size 27, degree 9 with 27 generators> -> 
<transformation semigroup of size 3, degree 3 with 3 generators>
gap> Unbind(SemigroupDirectProductInfo(D).projections[3]);
gap> Unbind(SemigroupDirectProductInfo(D).projection);
gap> Projection(D, 3);
Error, the direct product information for the 1st argument (a semigroup) is co\
rrupted, please re-create the object

# Size

#  semidp: Size, for a semigroup with direct product info, 1
gap> S := Semigroup(FullTransformationMonoid(2));;
gap> D := DirectProduct(S, S, S);;
gap> HasSize(D);
false
gap> Size(D) = Size(S) ^ 3;
true
gap> Size(D);
64

#  semidp: Size, for a semigroup with direct product info, 2
gap> S := Monoid([Transformation([2, 1]), Transformation([1, 1])]);;
gap> Size(DirectProduct(S));
4

# IsCommutative

#  semidp: IsCommutativeSemigroup, for a semigroup with direct product info, 1
gap> S := IdempotentGeneratedSubsemigroup(DualSymmetricInverseMonoid(4));;
gap> T := SymmetricInverseMonoid(1);;
gap> D := DirectProduct(S, T);;
gap> IsCommutativeSemigroup(D);
true

#  semidp: IsCommutativeSemigroup, for a semigroup with direct product info, 2
gap> S := IdempotentGeneratedSubsemigroup(DualSymmetricInverseMonoid(4));;
gap> T := FullTransformationMonoid(2);;
gap> D := DirectProduct(S, T);;
gap> IsCommutativeSemigroup(D);
false

#  semidp: IsCommutativeSemigroup, for a semigroup with direct product info, 3
gap> S := Monoid([Transformation([2, 1]), Transformation([1, 1])]);;
gap> IsCommutativeSemigroup(DirectProduct(S));
false

#  semidp: DirectProduct, for a semigroup with repeated generators
gap> S := Semigroup([
>  PBR([[-2, 1, 2], [-2, -1, 2]], [[-2, -1, 1, 2], [-2, -1, 1, 2]]),
>  PBR([[-2, -1, 1, 2], [-2, -1, 1, 2]], [[-2, -1, 1, 2], [-2, -1, 1, 2]]),
>  PBR([[-2, -1, 1, 2], [-2, -1, 1, 2]], [[-2, -1, 1, 2], [-2, -1, 1, 2]]),
>  PBR([[-2, -1, 1, 2], [-1]], [[-2, 1], [-2, -1, 1, 2]]),
>  PBR([[-2, 1], [-2, 2]], [[-2, -1], [-2, -1, 2]]),
>  PBR([[-2, -1, 1, 2], [-2, -1, 2]], [[-2, -1, 1, 2], [-2, -1, 1, 2]]),
>  PBR([[-1], [-2]], [[-2, 1], [2]])]);;
gap> Size(S);
15
gap> Size(Semigroup(DirectProduct(S, S)));
225

#  semidp: DirectProduct, for very large semigroups
gap> S := FullTransformationMonoid(50);;
gap> S := DirectProduct(S, S, S);;
gap> Size(S);
700649232162408535461864791644958065640130970938257885878534141944895541342930\
300743319094181060791015625000000000000000000000000000000000000000000000000000\
000000000000000000000000000000000000000000000000000000000000000000000000000000\
000000000000000000000

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/semigroups/semidp.tst");
