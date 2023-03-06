#############################################################################
##
#W  standard/semigroups/semipperm.tst
#Y  Copyright (C) 2011-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local BruteForceInverseCheck, BruteForceIsoCheck, C, F, H1, H2, I, J, L, R, S
#@local T, V, acting, empty_map, en, f, f1, f2, g, h, inv, iso, map, rels, rho
#@local s, symmetric, x, y
gap> START_TEST("Semigroups package: standard/semigroups/semipperm.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# SmallerDegreeTest5: SmallerDegreePartialPermRepresentation Issue 2:
# Example where the degree being returned was greater than the original degree
gap> f1 := PartialPerm([2, 1, 0, 0, 4]);;
gap> f2 := PartialPerm([1, 2, 3, 5]);;
gap> f := InverseSemigroup(f1, f2);;
gap> F := SmallerDegreePartialPermRepresentation(f);;
gap> NrMovedPoints(f);
4
gap> NrMovedPoints(Image(F));
4
gap> Size(f);
15
gap> Size(Image(F));
15

# SmallerDegreeTest6: SmallerDegreePartialPermRepresentation:
# Example where using SupermumIdempotents helps to give a better result 
gap> f1 := PartialPermNC([2, 1, 4, 5, 3, 7, 6, 9, 10, 8]);;
gap> f2 := PartialPermNC([2, 1, 0, 0, 0, 7, 6]);;
gap> f := InverseSemigroup(f1, f2);;
gap> F := SmallerDegreePartialPermRepresentation(f);;
gap> NrMovedPoints(f);
10
gap> NrMovedPoints(Image(F));
5
gap> Size(f);
8
gap> Size(Image(F));
8

# SmallerDegreeTest7: SmallerDegreePartialPermRepresentation:
# Example where the degree is reduced but not the number of moved points
gap> f1 := PartialPermNC([1, 2, 3, 4, 5, 6, 10, 11, 15, 16, 17, 18],
> [7, 5, 11, 8, 4, 2, 20, 14, 12, 17, 9, 3]);;
gap> f2 := PartialPermNC([1, 2, 3, 6, 8, 10, 12, 15, 16, 17, 18, 19],
> [2, 4, 14, 3, 17, 7, 9, 16, 15, 10, 11, 1]);;
gap> f := InverseSemigroup(f1, f2);;
gap> F := SmallerDegreePartialPermRepresentation(f);;
gap> NrMovedPoints(f);
19
gap> NrMovedPoints(Image(F));
19
gap> ActionDegree(f);
20
gap> ActionDegree(Image(F));
19
gap> Size(f);
2982
gap> Size(Image(F));
2982

# SmallerDegreeTest8: SmallerDegreePartialPermRepresentation:
# Example made complicated by right regular representation of Sym(5).
gap> S := SymmetricGroup(5);
Sym( [ 1 .. 5 ] )
gap> rho := ActionHomomorphism(S, S);
<action homomorphism>
gap> T := Image(rho);
<permutation group with 2 generators>
gap> H1 := [];;
gap> H2 := [];;
gap> for x in Elements(T) do
>   L := [];
>   for y in [1 .. 120] do
>     Add(L, y ^ x);
>   od;
>   g := PartialPerm(L);
>   Add(H2, g);
>   Add(L, 121);
>   Add(L, 122);
>   f := PartialPerm(L);
>   Add(H1, f);
> od;
gap> J := [1 .. 120];;
gap> Add(J, 122);
gap> Add(J, 121);
gap> h := PartialPerm(J);
<partial perm on 122 pts with degree 122, codegree 122>
gap> V := InverseSemigroup(H1, H2, h);
<inverse partial perm monoid of rank 122 with 240 generators>
gap> iso := SmallerDegreePartialPermRepresentation(V);;
gap> ActionDegree(Range(iso)) <= 12;  # Genuine minimum degree of V is 7.
true

# SemiPPermTest4: RepresentativeOfMinimalIdeal
gap> empty_map := PartialPerm([], []);;

### Semigroups containing the empty partial perm

# S = {empty_map}
gap> s := Semigroup(empty_map);
<trivial partial perm group of rank 0 with 1 generator>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# S = 0-simple semigroup of order 2
gap> s := Semigroup(empty_map, PartialPerm([1], [1]));
<partial perm monoid of rank 1 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# empty_map is a generator 
gap> s := Semigroup(PartialPerm([1, 2, 3], [1, 3, 4]), empty_map);
<partial perm semigroup of rank 3 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Length(DomainOfPartialPermCollection) of size 1
gap> s := Semigroup(PartialPerm([2], [1]));
<commutative partial perm semigroup of rank 1 with 1 generator>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Length(DomainOfPartialPermCollection) of size 1
gap> s := Semigroup(PartialPerm([2], [2]), PartialPerm([2], [3]));
<partial perm semigroup of rank 1 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Length(DomainOfPartialPermCollection) of size 1
gap> s := Semigroup(PartialPerm([2], [4]), PartialPerm([2], [3]));
<partial perm semigroup of rank 1 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Length(ImageOfPartialPermCollection) of size 1
gap> s := Semigroup(PartialPerm([2], [2]), PartialPerm([3], [2]));
<partial perm semigroup of rank 2 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Length(ImageOfPartialPermCollection) of size 1
gap> s := Semigroup(PartialPerm([4], [2]), PartialPerm([3], [2]));
<partial perm semigroup of rank 2 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Construction of graph reveals that empty_map in S
gap> s := Semigroup(PartialPerm([2, 0, 0, 4, 0]),
> PartialPerm([3, 0, 0, 0, 5]));
<partial perm semigroup of rank 3 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Rank 1 generator is not idempotent
gap> s := Semigroup(PartialPerm([3], [2]), PartialPerm([2], [1]));
<partial perm semigroup of rank 2 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Rank 1 generator is not idempotent
gap> s := Semigroup(PartialPerm([2], [1]), PartialPerm([3], [2]));
<partial perm semigroup of rank 2 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

# Analysis of graph reveals that empty_map in S (but not construction)
gap> s := Semigroup(PartialPerm([3, 2, 0]), PartialPerm([2, 3, 0]));
<partial perm semigroup of rank 2 with 2 generators>
gap> RepresentativeOfMinimalIdeal(s) = empty_map;
true
gap> empty_map in s;
true

### Semigroups not containing the empty partial perm

# Semigroup with multiplicative zero = empty_map
gap> s := Semigroup(
> PartialPerm(
>   [1, 2, 3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 17],
>   [5, 7, 1, 3, 6, 9, 8, 15, 2, 18, 13, 20, 17, 4]),
> PartialPerm(
>   [1, 2, 3, 4, 5, 6, 7, 9, 12, 13, 17, 18, 19, 20],
>   [9, 2, 5, 12, 4, 11, 17, 8, 14, 13, 1, 18, 3, 16]),
> PartialPerm(
>   [1, 2, 3, 4, 5, 6, 8, 10, 11, 13, 14, 15, 20],
>   [14, 3, 12, 4, 18, 15, 5, 16, 8, 13, 10, 9, 20]));
<partial perm semigroup of rank 19 with 3 generators>
gap> RepresentativeOfMinimalIdeal(s);
<identity partial perm on [ 13 ]>
gap> last in s;
true
gap> empty_map in s;
false

# Trivial partial perm semigroup: GAP knows that it is simple at creation
gap> s := Semigroup(PartialPerm([2], [2]));
<trivial partial perm group of rank 1 with 1 generator>
gap> HasIsSimpleSemigroup(s);
true
gap> RepresentativeOfMinimalIdeal(s);
<identity partial perm on [ 2 ]>
gap> last in s;
true
gap> empty_map in s;
false

# Trivial partial perm semigroup: GAP does not know that it is simple
gap> s := Semigroup(PartialPerm([2], [2]), PartialPerm([2], [2]));
<trivial partial perm group of rank 1 with 1 generator>
gap> HasIsSimpleSemigroup(s);
true
gap> RepresentativeOfMinimalIdeal(s);
<identity partial perm on [ 2 ]>
gap> last in s;
true
gap> empty_map in s;
false

# Group as partial perm semigroup
gap> s := Semigroup(PartialPerm([2, 3], [3, 2]));
<commutative partial perm semigroup of rank 2 with 1 generator>
gap> HasIsGroupAsSemigroup(s);
false
gap> RepresentativeOfMinimalIdeal(s);
<identity partial perm on [ 2, 3 ]>
gap> IsGroupAsSemigroup(s);
true
gap> RepresentativeOfMinimalIdeal(s) in s;
true
gap> empty_map in s;
false

# helper functions
gap> BruteForceIsoCheck := function(iso)
>   local x, y;
>   if not IsInjective(iso) or not IsSurjective(iso) then
>     return false;
>   fi;
>   for x in Generators(Source(iso)) do
>     for y in Generators(Source(iso)) do
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

# AsSemigroup: 
#   convert from IsPBRSemigroup to IsPartialPermSemigroup
gap> S := Semigroup([
> PBR([[-2], [-2]], [[], [1, 2]])]);
<commutative pbr semigroup of degree 2 with 1 generator>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<trivial partial perm group of rank 1 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsFpSemigroup to IsPartialPermSemigroup
gap> F := FreeSemigroup(2);; AssignGeneratorVariables(F);;
gap> rels := [[s1 ^ 2, s1],
>             [s1 * s2, s2],
>             [s2 * s1, s2],
>             [s2 ^ 2, s2]];;
gap> S := F / rels;
<fp semigroup with 2 generators and 4 relations of length 14>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<inverse partial perm monoid of size 2, rank 2 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBipartitionSemigroup to IsPartialPermSemigroup
gap> S := InverseSemigroup([
> Bipartition([[1, -1]]),
> Bipartition([[1], [-1]])]);
<commutative inverse bipartition monoid of degree 1 with 1 generator>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<commutative inverse partial perm monoid of rank 1 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTransformationSemigroup to IsPartialPermSemigroup
gap> S := Semigroup([
> Transformation([2, 2])]);
<commutative transformation semigroup of degree 2 with 1 generator>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<trivial partial perm group of rank 1 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBooleanMatSemigroup to IsPartialPermSemigroup
gap> S := Semigroup([
> Matrix(IsBooleanMat,
>        [[false, true], [false, true]])]);
<commutative semigroup of 2x2 boolean matrices with 1 generator>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<trivial partial perm group of rank 1 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMaxPlusMatrixSemigroup to IsPartialPermSemigroup
gap> S := Semigroup([
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, 0],
>         [-infinity, 0]])]);
<commutative semigroup of 2x2 max-plus matrices with 1 generator>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<trivial partial perm group of rank 1 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMinPlusMatrixSemigroup to IsPartialPermSemigroup
gap> S := Semigroup([
> Matrix(IsMinPlusMatrix,
>        [[infinity, 0],
>         [infinity, 0]])]);
<commutative semigroup of 2x2 min-plus matrices with 1 generator>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<trivial partial perm group of rank 1 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsPartialPermSemigroup
gap> S := Semigroup([
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, 0],
>         [-infinity, 0]])]);
<commutative semigroup of 2x2 projective max-plus matrices with 1 generator>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<trivial partial perm group of rank 1 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsIntegerMatrixSemigroup to IsPartialPermSemigroup
gap> S := Semigroup([
> Matrix(Integers,
>        [[0, 1],
>         [0, 1]])]);
<commutative semigroup of 2x2 integer matrices with 1 generator>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<trivial partial perm group of rank 1 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsPartialPermSemigroup
gap> S := Semigroup([
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, 0],
>         [-infinity, 0]], 4)]);
<commutative semigroup of 2x2 tropical max-plus matrices with 1 generator>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<trivial partial perm group of rank 1 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsPartialPermSemigroup
gap> S := Semigroup([
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, 0],
>         [infinity, 0]], 1)]);
<commutative semigroup of 2x2 tropical min-plus matrices with 1 generator>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<trivial partial perm group of rank 1 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsNTPMatrixSemigroup to IsPartialPermSemigroup
gap> S := Semigroup([
> Matrix(IsNTPMatrix,
>        [[0, 1],
>         [0, 1]], 2, 5)]);
<commutative semigroup of 2x2 ntp matrices with 1 generator>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<trivial partial perm group of rank 1 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPBRMonoid to IsPartialPermSemigroup
gap> S := Monoid([
> PBR([[-2], [-3], [-3]], [[], [1], [2, 3]]),
> PBR([[-3], [-1], [-3]], [[2], [], [1, 3]])]);
<pbr monoid of degree 3 with 2 generators>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<inverse partial perm monoid of size 6, rank 6 with 3 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsFpMonoid to IsPartialPermSemigroup
gap> F := FreeMonoid(3);; AssignGeneratorVariables(F);;
gap> rels := [[m1 ^ 2, m1],
>         [m1 * m2, m2],
>         [m1 * m3, m3],
>         [m2 * m1, m2],
>         [m3 * m1, m3],
>         [m3 ^ 2, m2 ^ 2],
>         [m2 ^ 3, m2 ^ 2],
>         [m2 ^ 2 * m3, m2 ^ 2],
>         [m2 * m3 * m2, m2],
>         [m3 * m2 ^ 2, m2 ^ 2],
>         [m3 * m2 * m3, m3]];;
gap> S := F / rels;
<fp monoid with 3 generators and 11 relations of length 45>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<inverse partial perm monoid of size 7, rank 7 with 4 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBipartitionMonoid to IsPartialPermSemigroup
gap> S := InverseMonoid([
> Bipartition([[1, -2], [2], [-1]]),
> Bipartition([[1, -1], [2, -2]]),
> Bipartition([[1], [2, -1], [-2]])]);
<inverse bipartition monoid of degree 2 with 2 generators>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<inverse partial perm monoid of rank 2 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTransformationMonoid to IsPartialPermSemigroup
gap> S := Monoid([
> Transformation([2, 3, 3]), Transformation([3, 1, 3])]);
<transformation monoid of degree 3 with 2 generators>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);;
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBooleanMatMonoid to IsPartialPermSemigroup
gap> S := Monoid([
> Matrix(IsBooleanMat,
>        [[false, true, false],
>         [false, false, true],
>         [false, false, true]]),
> Matrix(IsBooleanMat,
>        [[false, false, true],
>         [true, false, false],
>         [false, false, true]])]);
<monoid of 3x3 boolean matrices with 2 generators>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<inverse partial perm monoid of size 6, rank 6 with 3 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMaxPlusMatrixMonoid to IsPartialPermSemigroup
gap> S := Monoid([
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, 0, -infinity],
>         [-infinity, -infinity, 0],
>         [-infinity, -infinity, 0]]),
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, -infinity, 0],
>         [0, -infinity, -infinity],
>         [-infinity, -infinity, 0]])]);
<monoid of 3x3 max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<inverse partial perm monoid of size 6, rank 6 with 3 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMinPlusMatrixMonoid to IsPartialPermSemigroup
gap> S := Monoid([
> Matrix(IsMinPlusMatrix,
>        [[infinity, 0, infinity],
>         [infinity, infinity, 0],
>         [infinity, infinity, 0]]),
> Matrix(IsMinPlusMatrix,
>        [[infinity, infinity, 0],
>         [0, infinity, infinity],
>         [infinity, infinity, 0]])]);
<monoid of 3x3 min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<inverse partial perm monoid of size 6, rank 6 with 3 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixMonoid to IsPartialPermSemigroup
gap> S := Monoid([
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, 0, -infinity],
>         [-infinity, -infinity, 0],
>         [-infinity, -infinity, 0]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, -infinity, 0],
>         [0, -infinity, -infinity],
>         [-infinity, -infinity, 0]])]);
<monoid of 3x3 projective max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<inverse partial perm monoid of size 6, rank 6 with 3 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsIntegerMatrixMonoid to IsPartialPermSemigroup
gap> S := Monoid([
> Matrix(Integers,
>        [[0, 1, 0],
>         [0, 0, 1],
>         [0, 0, 1]]),
> Matrix(Integers,
>        [[0, 0, 1],
>         [1, 0, 0],
>         [0, 0, 1]])]);
<monoid of 3x3 integer matrices with 2 generators>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<inverse partial perm monoid of size 6, rank 6 with 3 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixMonoid to IsPartialPermSemigroup
gap> S := Monoid([
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, 0, -infinity],
>         [-infinity, -infinity, 0],
>         [-infinity, -infinity, 0]], 5),
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, -infinity, 0],
>         [0, -infinity, -infinity],
>         [-infinity, -infinity, 0]], 5)]);
<monoid of 3x3 tropical max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<inverse partial perm monoid of size 6, rank 6 with 3 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixMonoid to IsPartialPermSemigroup
gap> S := Monoid([
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, 0, infinity],
>         [infinity, infinity, 0],
>         [infinity, infinity, 0]], 2),
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, infinity, 0],
>         [0, infinity, infinity],
>         [infinity, infinity, 0]], 2)]);
<monoid of 3x3 tropical min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<inverse partial perm monoid of size 6, rank 6 with 3 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsNTPMatrixMonoid to IsPartialPermSemigroup
gap> S := Monoid([
> Matrix(IsNTPMatrix,
>        [[0, 1, 0],
>         [0, 0, 1],
>         [0, 0, 1]], 2, 4),
> Matrix(IsNTPMatrix,
>        [[0, 0, 1],
>         [1, 0, 0],
>         [0, 0, 1]], 2, 4)]);
<monoid of 3x3 ntp matrices with 2 generators>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<inverse partial perm monoid of size 6, rank 6 with 3 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsPBRSemigroup to IsPartialPermMonoid
gap> S := Semigroup([
> PBR([[-2], [-1]], [[2], [1]])]);
<commutative pbr semigroup of degree 2 with 1 generator>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<commutative inverse partial perm monoid of size 2, rank 2 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsFpSemigroup to IsPartialPermMonoid
gap> F := FreeSemigroup(1);; AssignGeneratorVariables(F);;
gap> rels := [[s1 ^ 3, s1]];;
gap> S := F / rels;
<fp semigroup with 1 generator and 1 relation of length 5>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<commutative inverse partial perm monoid of size 2, rank 2 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsBipartitionSemigroup to IsPartialPermMonoid
gap> S := InverseSemigroup([
> Bipartition([[1, -2], [2, -1]])]);
<block bijection group of degree 2 with 1 generator>
gap> T := AsMonoid(IsPartialPermMonoid, S);;
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsBipartitionSemigroup to IsPartialPermMonoid
gap> S := Semigroup([
> Bipartition([[1, -2], [2, -1]])]);
<block bijection group of degree 2 with 1 generator>
gap> T := AsMonoid(IsPartialPermMonoid, S);;
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTransformationSemigroup to IsPartialPermMonoid
gap> S := Semigroup([
> Transformation([2, 1])]);
<commutative transformation semigroup of degree 2 with 1 generator>
gap> T := AsMonoid(IsPartialPermMonoid, S);;
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsBooleanMatSemigroup to IsPartialPermMonoid
gap> S := Semigroup([
> Matrix(IsBooleanMat,
>        [[false, true], [true, false]])]);
<commutative semigroup of 2x2 boolean matrices with 1 generator>
gap> T := AsMonoid(IsPartialPermMonoid, S);;
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsMaxPlusMatrixSemigroup to IsPartialPermMonoid
gap> S := Semigroup([
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, 0], [0, -infinity]])]);
<commutative semigroup of 2x2 max-plus matrices with 1 generator>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<commutative inverse partial perm monoid of size 2, rank 2 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsMinPlusMatrixSemigroup to IsPartialPermMonoid
gap> S := Semigroup([
> Matrix(IsMinPlusMatrix,
>        [[infinity, 0], [0, infinity]])]);
<commutative semigroup of 2x2 min-plus matrices with 1 generator>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<commutative inverse partial perm monoid of size 2, rank 2 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsPartialPermMonoid
gap> S := Semigroup([
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, 0], [0, -infinity]])]);
<commutative semigroup of 2x2 projective max-plus matrices with 1 generator>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<commutative inverse partial perm monoid of size 2, rank 2 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsIntegerMatrixSemigroup to IsPartialPermMonoid
gap> S := Semigroup([
> Matrix(Integers,
>        [[0, 1], [1, 0]])]);
<commutative semigroup of 2x2 integer matrices with 1 generator>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<commutative inverse partial perm monoid of size 2, rank 2 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsPartialPermMonoid
gap> S := Semigroup([
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, 0], [0, -infinity]], 5)]);
<commutative semigroup of 2x2 tropical max-plus matrices with 1 generator>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<commutative inverse partial perm monoid of size 2, rank 2 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsPartialPermMonoid
gap> S := Semigroup([
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, 0], [0, infinity]], 3)]);
<commutative semigroup of 2x2 tropical min-plus matrices with 1 generator>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<commutative inverse partial perm monoid of size 2, rank 2 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsNTPMatrixSemigroup to IsPartialPermMonoid
gap> S := Semigroup([
> Matrix(IsNTPMatrix,
>        [[0, 1], [1, 0]], 3, 2)]);
<commutative semigroup of 2x2 ntp matrices with 1 generator>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<commutative inverse partial perm monoid of size 2, rank 2 with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsPBRMonoid to IsPartialPermMonoid
gap> S := Monoid([
> PBR([[-1], [-4], [-3], [-4]], [[1], [], [3], [2, 4]]),
> PBR([[-3], [-4], [-4], [-4]], [[], [], [1], [2, 3, 4]]),
> PBR([[-4], [-4], [-1], [-4]], [[3], [], [], [1, 2, 4]])]);
<pbr monoid of degree 4 with 3 generators>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<inverse partial perm monoid of size 7, rank 7 with 4 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsFpMonoid to IsPartialPermMonoid
gap> F := FreeMonoid(2);; AssignGeneratorVariables(F);;
gap> rels := [[m2 ^ 2, m1 ^ 2],
>         [m1 ^ 3, m1 ^ 2],
>         [m1 ^ 2 * m2, m1 ^ 2],
>         [m1 * m2 * m1, m1],
>         [m2 * m1 ^ 2, m1 ^ 2],
>         [m2 * m1 * m2, m2]];;
gap> S := F / rels;
<fp monoid with 2 generators and 6 relations of length 29>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<inverse partial perm monoid of size 6, rank 6 with 3 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsBipartitionMonoid to IsPartialPermMonoid
gap> S := InverseMonoid([
> Bipartition([[1, -3], [2], [3], [-1], [-2]]),
> Bipartition([[1, -1], [2], [3, -3], [-2]]),
> Bipartition([[1], [2], [3, -1], [-2], [-3]])]);
<inverse bipartition monoid of degree 3 with 3 generators>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<inverse partial perm monoid of size 7, rank 7 with 4 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTransformationMonoid to IsPartialPermMonoid
gap> S := Monoid([
> Transformation([1, 4, 3, 4]),
> Transformation([3, 4, 4, 4]),
> Transformation([4, 4, 1, 4])]);
<transformation monoid of degree 4 with 3 generators>
gap> T := AsMonoid(IsPartialPermMonoid, S);;
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsBooleanMatMonoid to IsPartialPermMonoid
gap> S := Monoid([
> Matrix(IsBooleanMat,
>        [[true, false, false, false],
>         [false, false, false, true],
>         [false, false, true, false],
>         [false, false, false, true]]),
> Matrix(IsBooleanMat,
>        [[false, false, true, false],
>         [false, false, false, true],
>         [false, false, false, true],
>         [false, false, false, true]]),
> Matrix(IsBooleanMat,
>        [[false, false, false, true],
>         [false, false, false, true],
>         [true, false, false, false],
>         [false, false, false, true]])]);
<monoid of 4x4 boolean matrices with 3 generators>
gap> T := AsMonoid(IsPartialPermMonoid, S);;
gap> RankOfPartialPermSemigroup(T);
4
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsMaxPlusMatrixMonoid to IsPartialPermMonoid
gap> S := Monoid([
> Matrix(IsMaxPlusMatrix,
>        [[0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0],
>         [-infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, -infinity, 0]]),
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, -infinity, 0],
>         [-infinity, -infinity, -infinity, 0],
>         [-infinity, -infinity, -infinity, 0]]),
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, -infinity, -infinity, 0],
>         [-infinity, -infinity, -infinity, 0],
>         [0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0]])]);
<monoid of 4x4 max-plus matrices with 3 generators>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<inverse partial perm monoid of size 7, rank 7 with 4 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsMinPlusMatrixMonoid to IsPartialPermMonoid
gap> S := Monoid([
> Matrix(IsMinPlusMatrix,
>        [[0, infinity, infinity, infinity],
>         [infinity, infinity, infinity, 0],
>         [infinity, infinity, 0, infinity],
>         [infinity, infinity, infinity, 0]]),
> Matrix(IsMinPlusMatrix,
>        [[infinity, infinity, 0, infinity],
>         [infinity, infinity, infinity, 0],
>         [infinity, infinity, infinity, 0],
>         [infinity, infinity, infinity, 0]]),
> Matrix(IsMinPlusMatrix,
>        [[infinity, infinity, infinity, 0],
>         [infinity, infinity, infinity, 0],
>         [0, infinity, infinity, infinity],
>         [infinity, infinity, infinity, 0]])]);
<monoid of 4x4 min-plus matrices with 3 generators>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<inverse partial perm monoid of size 7, rank 7 with 4 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsProjectiveMaxPlusMatrixMonoid to IsPartialPermMonoid
gap> S := Monoid([
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0],
>         [-infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, -infinity, 0]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, -infinity, 0],
>         [-infinity, -infinity, -infinity, 0],
>         [-infinity, -infinity, -infinity, 0]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, -infinity, -infinity, 0],
>         [-infinity, -infinity, -infinity, 0],
>         [0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0]])]);
<monoid of 4x4 projective max-plus matrices with 3 generators>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<inverse partial perm monoid of size 7, rank 7 with 4 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsIntegerMatrixMonoid to IsPartialPermMonoid
gap> S := Monoid([
> Matrix(Integers,
>        [[1, 0, 0, 0],
>         [0, 0, 0, 1],
>         [0, 0, 1, 0],
>         [0, 0, 0, 1]]),
> Matrix(Integers,
>        [[0, 0, 1, 0],
>         [0, 0, 0, 1],
>         [0, 0, 0, 1],
>         [0, 0, 0, 1]]),
> Matrix(Integers,
>        [[0, 0, 0, 1],
>         [0, 0, 0, 1],
>         [1, 0, 0, 0],
>         [0, 0, 0, 1]])]);
<monoid of 4x4 integer matrices with 3 generators>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<inverse partial perm monoid of size 7, rank 7 with 4 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTropicalMaxPlusMatrixMonoid to IsPartialPermMonoid
gap> S := Monoid([
> Matrix(IsTropicalMaxPlusMatrix,
>        [[0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0],
>         [-infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, -infinity, 0]], 5),
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, -infinity, 0],
>         [-infinity, -infinity, -infinity, 0],
>         [-infinity, -infinity, -infinity, 0]], 5),
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, -infinity, -infinity, 0],
>         [-infinity, -infinity, -infinity, 0],
>         [0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0]], 5)]);
<monoid of 4x4 tropical max-plus matrices with 3 generators>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<inverse partial perm monoid of size 7, rank 7 with 4 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTropicalMinPlusMatrixMonoid to IsPartialPermMonoid
gap> S := Monoid([
> Matrix(IsTropicalMinPlusMatrix,
>        [[0, infinity, infinity, infinity],
>         [infinity, infinity, infinity, 0],
>         [infinity, infinity, 0, infinity],
>         [infinity, infinity, infinity, 0]], 3),
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, infinity, 0, infinity],
>         [infinity, infinity, infinity, 0],
>         [infinity, infinity, infinity, 0],
>         [infinity, infinity, infinity, 0]], 3),
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, infinity, infinity, 0],
>         [infinity, infinity, infinity, 0],
>         [0, infinity, infinity, infinity],
>         [infinity, infinity, infinity, 0]], 3)]);
<monoid of 4x4 tropical min-plus matrices with 3 generators>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<inverse partial perm monoid of size 7, rank 7 with 4 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsNTPMatrixMonoid to IsPartialPermMonoid
gap> S := Monoid([
> Matrix(IsNTPMatrix,
>        [[1, 0, 0, 0],
>         [0, 0, 0, 1],
>         [0, 0, 1, 0],
>         [0, 0, 0, 1]], 1, 2),
> Matrix(IsNTPMatrix,
>        [[0, 0, 1, 0],
>         [0, 0, 0, 1],
>         [0, 0, 0, 1],
>         [0, 0, 0, 1]], 1, 2),
> Matrix(IsNTPMatrix,
>        [[0, 0, 0, 1],
>         [0, 0, 0, 1],
>         [1, 0, 0, 0],
>         [0, 0, 0, 1]], 1, 2)]);
<monoid of 4x4 ntp matrices with 3 generators>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<inverse partial perm monoid of size 7, rank 7 with 4 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsReesMatrixSemigroup to IsPartialPermSemigroup
gap> R := ReesMatrixSemigroup(Group([(1, 2)]), [[()]]);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
gap> T := AsSemigroup(IsPartialPermSemigroup, R);
<partial perm group of size 2, rank 2 with 1 generator>
gap> Size(R) = Size(T);
true
gap> NrDClasses(R) = NrDClasses(T);
true
gap> NrRClasses(R) = NrRClasses(T);
true
gap> NrLClasses(R) = NrLClasses(T);
true
gap> NrIdempotents(R) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid
#   convert from IsReesMatrixSemigroup to IsPartialPermMonoid
gap> R := ReesMatrixSemigroup(Group([(1, 2)]), [[(1, 2)]]);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
gap> T := AsMonoid(IsPartialPermMonoid, R);
<commutative inverse partial perm monoid of size 2, rank 2 with 1 generator>
gap> Size(R) = Size(T);
true
gap> NrDClasses(R) = NrDClasses(T);
true
gap> NrRClasses(R) = NrRClasses(T);
true
gap> NrLClasses(R) = NrLClasses(T);
true
gap> NrIdempotents(R) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsReesZeroMatrixSemigroup to IsPartialPermSemigroup
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2)]),
>                                 [[(1, 2)]]);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
gap> T := AsSemigroup(IsPartialPermSemigroup, R);
<inverse partial perm semigroup of size 3, rank 3 with 2 generators>
gap> Size(R) = Size(T);
true
gap> NrDClasses(R) = NrDClasses(T);
true
gap> NrRClasses(R) = NrRClasses(T);
true
gap> NrLClasses(R) = NrLClasses(T);
true
gap> NrIdempotents(R) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid
#   convert from IsReesZeroMatrixSemigroup to IsPartialPermMonoid
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[()]]);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
gap> T := AsMonoid(IsPartialPermMonoid, R);
<inverse partial perm monoid of size 3, rank 3 with 2 generators>
gap> Size(R) = Size(T);
true
gap> NrDClasses(R) = NrDClasses(T);
true
gap> NrRClasses(R) = NrRClasses(T);
true
gap> NrLClasses(R) = NrLClasses(T);
true
gap> NrIdempotents(R) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from graph inverse to IsPartialPermSemigroup
gap> S := GraphInverseSemigroup(Digraph([[2], []]));
<finite graph inverse semigroup with 2 vertices, 1 edge>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<inverse partial perm semigroup of size 6, rank 6 with 4 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from perm group to IsPartialPermSemigroup
gap> S := DihedralGroup(IsPermGroup, 6);
Group([ (1,2,3), (2,3) ])
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<partial perm group of rank 3 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from perm group to IsPartialPermMonoid
gap> S := DihedralGroup(IsPermGroup, 6);
Group([ (1,2,3), (2,3) ])
gap> T := AsMonoid(IsPartialPermMonoid, S);
<partial perm group of rank 3 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from non-perm group to IsPartialPermSemigroup
gap> S := DihedralGroup(6);
<pc group of size 6 with 2 generators>
gap> T := AsSemigroup(IsPartialPermSemigroup, S);;
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from non-perm group to IsPartialPermMonoid
gap> S := DihedralGroup(6);
<pc group of size 6 with 2 generators>
gap> T := AsMonoid(IsPartialPermMonoid, S);
<inverse partial perm monoid of size 6, rank 6 with 5 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBlockBijectionSemigroup to IsPartialPermSemigroup
gap> S := InverseSemigroup(Bipartition([[1, -1, -3], [2, 3, -2]]));;
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<inverse partial perm semigroup of size 5, rank 5 with 2 generators>
gap> IsInverseSemigroup(T);
true
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBlockBijectionMonoid to IsPartialPermMonoid
gap> S := InverseMonoid([
> Bipartition([[1, -1, -3], [2, 3, -2]])]);;
gap> T := AsMonoid(IsPartialPermMonoid, S);
<inverse partial perm monoid of size 6, rank 6 with 3 generators>
gap> IsInverseMonoid(T);
true
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsPartialPermMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBlockBijectionMonoid to IsPartialPermSemigroup
gap> S := InverseMonoid([
> Bipartition([[1, -1, -3], [2, 3, -2]])]);;
gap> T := AsSemigroup(IsPartialPermSemigroup, S);
<inverse partial perm monoid of size 6, rank 6 with 3 generators>
gap> IsInverseSemigroup(T) and IsMonoidAsSemigroup(T);
true
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPartialPermSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# Test RandomSemigroup
gap> RandomSemigroup(IsPartialPermSemigroup);;
gap> RandomSemigroup(IsPartialPermSemigroup, 2);;
gap> RandomSemigroup(IsPartialPermSemigroup, 2, 2);;
gap> RandomInverseSemigroup(IsPartialPermSemigroup);;
gap> RandomInverseSemigroup(IsPartialPermSemigroup, 2);;
gap> RandomInverseSemigroup(IsPartialPermSemigroup, 2, 2);;
gap> RandomMonoid(IsPartialPermMonoid);;
gap> RandomMonoid(IsPartialPermMonoid, 2);;
gap> RandomMonoid(IsPartialPermMonoid, 2, 2);;
gap> RandomInverseMonoid(IsPartialPermMonoid);;
gap> RandomInverseMonoid(IsPartialPermMonoid, 2);;
gap> RandomInverseMonoid(IsPartialPermMonoid, 2, 2);;

# Test Idempotents, for partial perm semigroup and rank
gap> S := Semigroup(SymmetricInverseMonoid(3));
<partial perm monoid of rank 3 with 4 generators>
gap> Idempotents(S, 0);
[ <empty partial perm> ]
gap> Set(Idempotents(S, 1));
[ <identity partial perm on [ 1 ]>, <identity partial perm on [ 2 ]>, 
  <identity partial perm on [ 3 ]> ]
gap> Idempotents(S, 2);
[ <identity partial perm on [ 1, 2 ]>, <identity partial perm on [ 2, 3 ]>, 
  <identity partial perm on [ 1, 3 ]> ]
gap> Idempotents(S, 3);
[ <identity partial perm on [ 1, 2, 3 ]> ]

# Test Enumerator for symmetric inverse monoid
gap> en := Enumerator(SymmetricInverseMonoid(3));
<enumerator of <symmetric inverse monoid of degree 3>>
gap> en[35];
fail
gap> Position(en, PartialPerm([1 .. 4]));
fail
gap> ForAll(en, x -> x = en[Position(en, x)]);
true
gap> ForAll([1 .. Length(en)], i -> i = Position(en, en[i]));
true

# Test IsomorphismPartialPermSemigroup
gap> S := Semigroup([
> PartialPerm([1, 2], [1, 2]),
>  PartialPerm([1, 3], [2, 1]), PartialPerm([1, 2], [3, 2])]);
<partial perm semigroup of rank 3 with 3 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);;
gap> IsomorphismPartialPermSemigroup(T);
<bipartition semigroup of degree 3 with 3 generators> -> 
<partial perm semigroup of rank 3 with 3 generators>
gap> I := SemigroupIdeal(T, T.1 * T.2 * T.3);;
gap> IsomorphismPartialPermSemigroup(I);;

#  semipperm: IsomorphismPartialPermSemigroup, for a group as semigroup, 1
gap> S := DihedralGroup(8);
<pc group of size 8 with 3 generators>
gap> IsGroupAsSemigroup(S);
true
gap> map := IsomorphismPartialPermSemigroup(S);;
gap> Source(map);
<pc group of size 8 with 3 generators>
gap> Size(Range(map));
8
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#  semipperm: IsomorphismPartialPermSemigroup, for a group as semigroup, 2
gap> S := Semigroup([
>  PartialPerm([1, 2, 3, 4, 5], [2, 3, 4, 5, 1]),
>  PartialPerm([1, 2, 3, 4, 5], [2, 1, 3, 4, 5])]);;
gap> IsGroupAsSemigroup(S);
true
gap> map := IsomorphismPartialPermSemigroup(S);;
gap> Source(map) = Range(map);
true
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#  semipperm: IsomorphismPartialPermSemigroup, for a zero group, 1
gap> S := Range(InjectionZeroMagma(SymmetricGroup(5)));
<Sym( [ 1 .. 5 ] ) with 0 adjoined>
gap> map := IsomorphismPartialPermSemigroup(S);
<Sym( [ 1 .. 5 ] ) with 0 adjoined> -> <inverse partial perm monoid of rank 5 
  with 3 generators>
gap> Range(map);
<inverse partial perm monoid of rank 5 with 3 generators>
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#  semipperm: IsomorphismPartialPermSemigroup, for a zero group, 2
gap> S := Range(InjectionZeroMagma(TrivialGroup(IsPcGroup)));
<<pc group of size 1 with 0 generators> with 0 adjoined>
gap> map := IsomorphismPartialPermSemigroup(S);;
gap> Range(map);
<symmetric inverse monoid of degree 1>
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#  semipperm: IsomorphismPartialPermSemigroup, for a zero group, 3
gap> S := Range(InjectionZeroMagma(SL(2, 2)));
<SL(2,2) with 0 adjoined>
gap> map := IsomorphismPartialPermSemigroup(S);
<SL(2,2) with 0 adjoined> -> <inverse partial perm monoid of rank 3 with 3 
  generators>
gap> Range(map);
<inverse partial perm monoid of rank 3 with 3 generators>
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# Test GroupOfUnits
gap> S := SymmetricInverseMonoid(4);;
gap> GroupOfUnits(S);
<partial perm group of rank 4 with 2 generators>
gap> GroupOfUnits(SemigroupIdeal(S, S.3));
fail

# Test One for an ideal
gap> S := SymmetricInverseMonoid(4);;
gap> I := SemigroupIdeal(S, S.3);;
gap> One(I);
fail
gap> I := SemigroupIdeal(S, S.1);;
gap> One(I);
<identity partial perm on [ 1, 2, 3, 4 ]>
gap> S := SymmetricInverseMonoid(4);;
gap> I := SemigroupIdeal(S, S.3);;
gap> GeneratorsOfSemigroup(I);;
gap> One(I);
fail
gap> I := SemigroupIdeal(S, S.1);;
gap> GeneratorsOfSemigroup(I);;
gap> One(I);
<identity partial perm on [ 1, 2, 3, 4 ]>

# Test Codegree/degree/rank for an ideal
gap> S := SymmetricInverseMonoid(4);;
gap> I := SemigroupIdeal(S, S.3);;
gap> CodegreeOfPartialPermSemigroup(I);
4
gap> DegreeOfPartialPermSemigroup(I);
4
gap> RankOfPartialPermSemigroup(I);
4

# Test ComponentRepsOfPartialPermSemigroup
gap> S := Semigroup([
>  PartialPerm([1, 2, 3, 5, 6, 7, 8], [9, 6, 11, 2, 3, 10, 4]),
>  PartialPerm([1, 2, 3, 4, 5, 7, 10], [2, 6, 9, 3, 5, 10, 1]),
>  PartialPerm([1, 3, 4, 5, 6, 8], [8, 9, 1, 10, 6, 11]),
>  PartialPerm([1, 2, 3, 5, 6, 7, 8], [7, 3, 4, 5, 2, 11, 8]),
>  PartialPerm([1, 2, 3, 4, 5, 6, 7], [1, 10, 8, 6, 4, 9, 5])]);;
gap> ComponentRepsOfPartialPermSemigroup(S);
[ 1 ]
gap> ComponentsOfPartialPermSemigroup(S);
[ [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] ]
gap> I := SemigroupIdeal(S, S.5);;
gap> ComponentRepsOfPartialPermSemigroup(I);
[ 1 ]
gap> CyclesOfPartialPermSemigroup(I);
[ [ 1, 2, 6, 3, 4, 8, 10, 7, 5 ] ]
gap> CyclesOfPartialPermSemigroup(S);
[ [ 1, 2, 6, 3, 4, 8, 10, 7, 5 ] ]

# Test NaturalLeqInverseSemigroup
gap> S := Semigroup([
>  PartialPerm([1, 2, 3, 5, 6, 7, 8], [9, 6, 11, 2, 3, 10, 4]),
>  PartialPerm([1, 2, 3, 4, 5, 7, 10], [2, 6, 9, 3, 5, 10, 1]),
>  PartialPerm([1, 3, 4, 5, 6, 8], [8, 9, 1, 10, 6, 11]),
>  PartialPerm([1, 2, 3, 5, 6, 7, 8], [7, 3, 4, 5, 2, 11, 8]),
>  PartialPerm([1, 2, 3, 4, 5, 6, 7], [1, 10, 8, 6, 4, 9, 5])]);;
gap> NaturalLeqInverseSemigroup(S);
Error, the argument is not an inverse semigroup

# Test SmallerDegreePartialPermRepresentation for CanUseFroidurePin
# semigroup
gap> S := Range(VagnerPrestonRepresentation(DualSymmetricInverseMonoid(3)));;
gap> S := Semigroup(S, rec(acting := false));
<partial perm monoid of rank 25 with 3 generators>
gap> SmallerDegreePartialPermRepresentation(S);
<inverse partial perm monoid of size 25, rank 25 with 3 generators> -> 
<inverse partial perm monoid of rank 6 with 3 generators>

#  GeneratorsOfGroup
gap> S := Group([], PartialPerm([1, 2]));;
gap> GeneratorsOfGroup(S);
[  ]
gap> GeneratorsOfSemigroup(S);
[ <identity partial perm on [ 1, 2 ]> ]

# Operator \<
gap> S := Semigroup([
>  PartialPerm([1, 2, 3, 5, 6, 7, 8], [9, 6, 11, 2, 3, 10, 4]),
>  PartialPerm([1, 2, 3, 4, 5, 7, 10], [2, 6, 9, 3, 5, 10, 1]),
>  PartialPerm([1, 3, 4, 5, 6, 8], [8, 9, 1, 10, 6, 11]),
>  PartialPerm([1, 2, 3, 5, 6, 7, 8], [7, 3, 4, 5, 2, 11, 8]),
>  PartialPerm([1, 2, 3, 4, 5, 6, 7], [1, 10, 8, 6, 4, 9, 5])]);;
gap> S < S;
false
gap> T := SymmetricInverseMonoid(4);;
gap> T < S;
true
gap> T > S;
false
gap> T = S;
false
gap> T < T;
false

# CodegreeOfPartialPermSemigroup
gap> S := MinimalIdeal(SymmetricInverseMonoid(3));
<partial perm group of rank 0>
gap> CodegreeOfPartialPermSemigroup(S);
0
gap> DegreeOfPartialPermSemigroup(S);
0

# DigraphOfActionOnPoints
gap> S := SymmetricInverseMonoid(3);
<symmetric inverse monoid of degree 3>
gap> DigraphOfActionOnPoints(S, -1);
Error, the 2nd argument (an integer) must be non-negative
gap> DigraphOfActionOnPoints(S, 0);
<immutable empty digraph with 0 vertices>
gap> DigraphOfActionOnPoints(S);
<immutable multidigraph with 3 vertices, 13 edges>
gap> DigraphOfActionOnPoints(S, 3);
<immutable multidigraph with 3 vertices, 13 edges>

# SmallerDegreePartialPermRepresentation for a non-partial perm semigroup
gap> S := UniformBlockBijectionMonoid(4);
<inverse block bijection monoid of degree 4 with 3 generators>
gap> map := SmallerDegreePartialPermRepresentation(S);
CompositionMapping( <inverse partial perm monoid of size 131, rank 131 with 3 
  generators> -> <inverse partial perm monoid of rank 10 with 3 generators>, 
<inverse block bijection monoid of size 131, degree 4 with 3 generators> -> 
<inverse partial perm monoid of size 131, rank 131 with 3 generators> )
gap> S.1 ^ map in Range(map);
true

# Issue 817
gap> S := DualSymmetricInverseMonoid(4);
<inverse block bijection monoid of degree 4 with 3 generators>
gap> C := SemigroupCongruence(S,
> [Bipartition([[1, 2, 3, 4, -1, -2, -3, -4]]),
>  Bipartition([[1, -1], [2, -2], [3, -3], [4, -4]])]);
<2-sided semigroup congruence over <inverse block bijection monoid 
 of size 339, degree 4 with 3 generators> with 1 generating pairs>
gap> map := SmallerDegreePartialPermRepresentation(Source(C));
CompositionMapping( <inverse partial perm monoid of size 339, rank 339 with 3 
  generators> -> <inverse partial perm monoid of rank 14 with 3 generators>, 
<inverse block bijection monoid of size 339, degree 4 with 3 generators> -> 
<inverse partial perm monoid of size 339, rank 339 with 3 generators> )
gap> List(GeneratingPairsOfSemigroupCongruence(C), x -> OnTuples(x, map));
[ [ <empty partial perm>, 
      <identity partial perm on 
        [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 ]> ] ]

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/semigroups/semipperm.tst");
