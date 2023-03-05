#############################################################################
##
#W  standard/semigroups/semiffmat.tst
#Y  Copyright (C) 2015-2022                              James D. Mitchell
##                                                       
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local BruteForceInverseCheck, BruteForceIsoCheck, F, H, S, T, U, acting, coll
#@local inv, map, rels, x, y
gap> START_TEST("Semigroups package: standard/semigroups/semiffmat.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# BruteForceIsoCheck helper functions
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

# Issue 210
gap> x := Matrix(GF(2 ^ 2),
> [[Z(2 ^ 2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [Z(2 ^ 2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)]]);;
gap> S := Monoid(x, rec(acting := false));
<commutative monoid of 6x6 matrices over GF(2^2) with 1 generator>
gap> HasIsFinite(S);
true
gap> Size(S);
7

# Issue 211
gap> S := FullMatrixMonoid(3, 3);;
gap> One(S) in S;
true
gap> H := GroupHClass(DClass(S, One(S)));
<Green's H-class: <matrix object of dimensions 3x3 over GF(3)>>
gap> IsomorphismPermGroup(H);;

# Test AsSemigroup
gap> S := Semigroup([
> Z(3) * [[1, 0, 0], [1, 1, 0], [0, 1, 0]],
> Z(3) * [[0, 0, 0], [0, 0, 1], [0, 1, 0]]]);
<semigroup with 2 generators>
gap> T := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S);
<semigroup of 3x3 matrices over GF(3) with 2 generators>
gap> AsSemigroup(IsMatrixOverFiniteFieldSemigroup, GF(9), S);
<semigroup of 3x3 matrices over GF(3^2) with 2 generators>
gap> AsSemigroup(IsMatrixOverFiniteFieldSemigroup, GF(9), T);
<semigroup of 3x3 matrices over GF(3^2) with 2 generators>
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, GF(3), S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, GF(7), S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, GF(3), T);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, GF(7), T);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPBRSemigroup to IsMatrixOverFiniteFieldSemigroup
gap> S := Semigroup([
> PBR([[-1], [-4], [-3], [-4], [-1]], [[1, 5], [], [3], [2, 4], []]),
> PBR([[-3], [-2], [-3], [-3], [-2]], [[], [2, 5], [1, 3, 4], [], []])]);
<pbr semigroup of degree 5 with 2 generators>
gap> T := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S);
<semigroup of 5x5 matrices over GF(2) with 2 generators>
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
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMatrixOverFiniteFieldSemigroup to IsMatrixOverFiniteFieldSemigroup
gap> S := Semigroup([
> Matrix(GF(2),
>        [[Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>         [0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2)],
>         [0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2)],
>         [0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2)],
>         [Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)]]),
> Matrix(GF(2),
>        [[0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2)],
>         [0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>         [0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2)],
>         [0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2)],
>         [0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2)]])]);
<semigroup of 5x5 matrices over GF(2) with 2 generators>
gap> T := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S);
<semigroup of 5x5 matrices over GF(2) with 2 generators>
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
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsFpSemigroup to IsMatrixOverFiniteFieldSemigroup
gap> F := FreeSemigroup(2);; AssignGeneratorVariables(F);;
gap> rels := [[s1 ^ 2, s1], [s2 ^ 2, s2],
>             [s1 * s2 * s1, s1 * s2],
>             [s2 * s1 * s2, s1 * s2]];;
gap> S := F / rels;
<fp semigroup with 2 generators and 4 relations of length 18>
gap> T := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S);
<semigroup of 5x5 matrices over GF(2) with 2 generators>
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
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBipartitionSemigroup to IsMatrixOverFiniteFieldSemigroup
gap> S := Semigroup([
> Bipartition([[1, 5, -1], [2, 4, -4], [3, -3], [-2], [-5]]),
> Bipartition([[1, 3, 4, -3], [2, 5, -2], [-1], [-4], [-5]])]);
<bipartition semigroup of degree 5 with 2 generators>
gap> T := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S);
<semigroup of 5x5 matrices over GF(2) with 2 generators>
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
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTransformationSemigroup to IsMatrixOverFiniteFieldSemigroup
gap> S := Semigroup([
> Transformation([1, 4, 3, 4, 1]), Transformation([3, 2, 3, 3, 2])]);
<transformation semigroup of degree 5 with 2 generators>
gap> T := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S);
<semigroup of 5x5 matrices over GF(2) with 2 generators>
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
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBooleanMatSemigroup to IsMatrixOverFiniteFieldSemigroup
gap> S := Semigroup([
> Matrix(IsBooleanMat,
>        [[true, false, false, false, false],
>         [false, false, false, true, false],
>         [false, false, true, false, false],
>         [false, false, false, true, false],
>         [true, false, false, false, false]]),
> Matrix(IsBooleanMat,
>        [[false, false, true, false, false],
>         [false, true, false, false, false],
>         [false, false, true, false, false],
>         [false, false, true, false, false],
>         [false, true, false, false, false]])]);
<semigroup of 5x5 boolean matrices with 2 generators>
gap> T := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S);
<semigroup of 5x5 matrices over GF(2) with 2 generators>
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
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMaxPlusMatrixSemigroup to IsMatrixOverFiniteFieldSemigroup
gap> S := Semigroup([
> Matrix(IsMaxPlusMatrix,
>        [[0, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity]]),
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity]])]);
<semigroup of 5x5 max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S);
<semigroup of 5x5 matrices over GF(2) with 2 generators>
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
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMinPlusMatrixSemigroup to IsMatrixOverFiniteFieldSemigroup
gap> S := Semigroup([
> Matrix(IsMinPlusMatrix,
>        [[0, infinity, infinity, infinity, infinity],
>         [infinity, infinity, infinity, 0, infinity],
>         [infinity, infinity, 0, infinity, infinity],
>         [infinity, infinity, infinity, 0, infinity],
>         [0, infinity, infinity, infinity, infinity]]),
> Matrix(IsMinPlusMatrix,
>        [[infinity, infinity, 0, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity]])]);
<semigroup of 5x5 min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S);
<semigroup of 5x5 matrices over GF(2) with 2 generators>
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
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsMatrixOverFiniteFieldSemigroup
gap> S := Semigroup([
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[0, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity]])]);
<semigroup of 5x5 projective max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S);
<semigroup of 5x5 matrices over GF(2) with 2 generators>
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
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsIntegerMatrixSemigroup to IsMatrixOverFiniteFieldSemigroup
gap> S := Semigroup([
> Matrix(Integers,
>        [[1, 0, 0, 0, 0],
>         [0, 0, 0, 1, 0],
>         [0, 0, 1, 0, 0],
>         [0, 0, 0, 1, 0],
>         [1, 0, 0, 0, 0]]),
> Matrix(Integers,
>        [[0, 0, 1, 0, 0],
>         [0, 1, 0, 0, 0],
>         [0, 0, 1, 0, 0],
>         [0, 0, 1, 0, 0],
>         [0, 1, 0, 0, 0]])]);
<semigroup of 5x5 integer matrices with 2 generators>
gap> T := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S);
<semigroup of 5x5 matrices over GF(2) with 2 generators>
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
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsMatrixOverFiniteFieldSemigroup
gap> S := Semigroup([
> Matrix(IsTropicalMaxPlusMatrix,
>        [[0, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity]], 3),
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity]], 3)]);
<semigroup of 5x5 tropical max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S);
<semigroup of 5x5 matrices over GF(2) with 2 generators>
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
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsMatrixOverFiniteFieldSemigroup
gap> S := Semigroup([
> Matrix(IsTropicalMinPlusMatrix,
>        [[0, infinity, infinity, infinity, infinity],
>         [infinity, infinity, infinity, 0, infinity],
>         [infinity, infinity, 0, infinity, infinity],
>         [infinity, infinity, infinity, 0, infinity],
>         [0, infinity, infinity, infinity, infinity]], 3),
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, infinity, 0, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity]], 3)]);
<semigroup of 5x5 tropical min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S);
<semigroup of 5x5 matrices over GF(2) with 2 generators>
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
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsNTPMatrixSemigroup to IsMatrixOverFiniteFieldSemigroup
gap> S := Semigroup([
> Matrix(IsNTPMatrix,
>        [[1, 0, 0, 0, 0],
>         [0, 0, 0, 1, 0],
>         [0, 0, 1, 0, 0],
>         [0, 0, 0, 1, 0],
>         [1, 0, 0, 0, 0]], 1, 5),
> Matrix(IsNTPMatrix,
>        [[0, 0, 1, 0, 0],
>         [0, 1, 0, 0, 0],
>         [0, 0, 1, 0, 0],
>         [0, 0, 1, 0, 0],
>         [0, 1, 0, 0, 0]], 1, 5)]);
<semigroup of 5x5 ntp matrices with 2 generators>
gap> T := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S);
<semigroup of 5x5 matrices over GF(2) with 2 generators>
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
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# IsomorphismSemigroup convert from semigroup of partial perms to
# IsMatrixOverFiniteFieldSemigroup with a field other than GF(2)
gap> S := Semigroup(PartialPerm([1, 2, 3], [3, 1, 2]),
> PartialPerm([1], [1]));;
gap> map := IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, GF(7), S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# Test AsMonoid/IsomorphismMonoid
gap> S := Semigroup(Transformation([1, 2, 2, 2, 2]),
>                   Transformation([2, 1, 1, 1, 1]));
<transformation semigroup of degree 5 with 2 generators>
gap> AsMonoid(IsMatrixOverFiniteFieldMonoid, S);
<commutative monoid of 2x2 matrices over GF(2) with 1 generator>
gap> map := IsomorphismMonoid(IsMatrixOverFiniteFieldMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> T := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S);
<semigroup of 5x5 matrices over GF(2) with 2 generators>
gap> AsMonoid(T);
<commutative monoid of 2x2 matrices over GF(2) with 1 generator>
gap> U := Semigroup(List(Generators(T), AsList));
<semigroup of 5x5 matrices over GF(2) with 2 generators>
gap> AsMonoid(IsMatrixOverFiniteFieldMonoid, U);
<commutative monoid of 2x2 matrices over GF(2) with 1 generator>
gap> map := IsomorphismMonoid(IsMatrixOverFiniteFieldMonoid, U);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismMonoid(IsMatrixOverFiniteFieldMonoid, GF(3), S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# Test AsMonoid
gap> S := Semigroup([
>  Z(3) * [[1, 0, 0],
>         [1, 1, 0],
>         [0, 1, 0]],
>  Z(3) * [[0, 0, 0],
>         [0, 0, 1],
>         [0, 1, 0]]]);;
gap> S := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S);;
gap> AsMonoid(S);
fail
gap> S := GeneralLinearMonoid(2, 2);;
gap> map := IsomorphismMonoid(IsMatrixOverFiniteFieldMonoid, S);;
gap> S := GeneralLinearMonoid(2, 2);;
gap> AsMonoid(IsMatrixOverFiniteFieldMonoid, GF(4), S);
<monoid of 2x2 matrices over GF(2^2) with 3 generators>
gap> AsMonoid(IsMatrixOverFiniteFieldMonoid, GF(3), S);
<monoid of 16x16 matrices over GF(3) with 3 generators>

# Test PrintString
gap> PrintString(GLM(3, 3));
"GLM(3, 3 ^ 1)"
gap> PrintString(GLM(3, 9));
"GLM(3, 3 ^ 2)"
gap> PrintObj(GLM(3, 9)); "this string";
GLM(3, 3 ^ 2)"this string"

# ViewObj
gap> GLM(3, 9);
<general linear monoid 3x3 over GF(3^2)>

# RandomSemigroup
gap> RandomSemigroup(IsMatrixOverFiniteFieldSemigroup);;
gap> RandomSemigroup(IsMatrixOverFiniteFieldSemigroup, 2);;
gap> RandomSemigroup(IsMatrixOverFiniteFieldSemigroup, 2, 3);;
gap> RandomSemigroup(IsMatrixOverFiniteFieldSemigroup, 2, 3, GF(7));;
gap> RandomSemigroup(IsMatrixOverFiniteFieldSemigroup, 2, 3, GF(7), [1, 3]);;
gap> RandomSemigroup(IsMatrixOverFiniteFieldSemigroup, "a");
Error, the 2nd argument (number of generators) is not a pos int
gap> RandomSemigroup(IsMatrixOverFiniteFieldSemigroup, 1, "a");
Error, the 3rd argument (matrix dimension) is not a pos int
gap> RandomSemigroup(IsMatrixOverFiniteFieldSemigroup, 1, 2, Integers);
Error, the 4th argument is not a finite field
gap> RandomSemigroup(IsMatrixOverFiniteFieldSemigroup, 2, 3, GF(7), [0]);;
Error, the 5th argument (matrix ranks) is not a list of pos ints
gap> RandomSemigroup(IsMatrixOverFiniteFieldSemigroup, 2, 3, GF(7), [1], 10);;
Error, there must be at most 5 arguments

# RandomInverseSemigroup
gap> RandomInverseSemigroup(IsMatrixOverFiniteFieldSemigroup);;
gap> RandomInverseSemigroup(IsMatrixOverFiniteFieldSemigroup, 2);;
gap> RandomInverseSemigroup(IsMatrixOverFiniteFieldSemigroup, 2, 3);;
gap> RandomInverseSemigroup(IsMatrixOverFiniteFieldSemigroup, 2, 3, GF(7));;
gap> RandomInverseSemigroup(IsMatrixOverFiniteFieldSemigroup, 2, 3, GF(7), [1, 3]);;
gap> RandomInverseSemigroup(IsMatrixOverFiniteFieldSemigroup, "a");
Error, the 2nd argument (number of generators) is not a pos int
gap> RandomInverseSemigroup(IsMatrixOverFiniteFieldSemigroup, 1, "a");
Error, the 3rd argument (matrix dimension) is not a pos int
gap> RandomInverseSemigroup(IsMatrixOverFiniteFieldSemigroup, 1, 2, Integers);
Error, the 4th argument is not a finite field
gap> RandomInverseSemigroup(IsMatrixOverFiniteFieldSemigroup, 2, 3, GF(7), [0]);;
Error, the 5th argument (matrix ranks) is not a list of pos ints
gap> RandomInverseSemigroup(IsMatrixOverFiniteFieldSemigroup, 2, 3, GF(7), [1], 10);;
Error, there must be at most 5 arguments

# RandomMonoid
# The first test here is sometimes very slow so is commented out
# gap> RandomMonoid(IsMatrixOverFiniteFieldMonoid);;
gap> RandomMonoid(IsMatrixOverFiniteFieldMonoid, 2);;
gap> RandomMonoid(IsMatrixOverFiniteFieldMonoid, 2, 3);;
gap> RandomMonoid(IsMatrixOverFiniteFieldMonoid, 2, 3, GF(7));;
gap> RandomMonoid(IsMatrixOverFiniteFieldMonoid, 2, 3, GF(7), [1, 3]);;
gap> RandomMonoid(IsMatrixOverFiniteFieldMonoid, "a");
Error, the 2nd argument (number of generators) is not a pos int
gap> RandomMonoid(IsMatrixOverFiniteFieldMonoid, 1, "a");
Error, the 3rd argument (matrix dimension) is not a pos int
gap> RandomMonoid(IsMatrixOverFiniteFieldMonoid, 1, 2, Integers);
Error, the 4th argument is not a finite field
gap> RandomMonoid(IsMatrixOverFiniteFieldMonoid, 2, 3, GF(7), [0]);;
Error, the 5th argument (matrix ranks) is not a list of pos ints
gap> RandomMonoid(IsMatrixOverFiniteFieldMonoid, 2, 3, GF(7), [1], 10);;
Error, there must be at most 5 arguments

# RandomInverseMonoid
gap> RandomInverseMonoid(IsMatrixOverFiniteFieldMonoid);;
gap> RandomInverseMonoid(IsMatrixOverFiniteFieldMonoid, 2);;
gap> RandomInverseMonoid(IsMatrixOverFiniteFieldMonoid, 2, 3);;
gap> RandomInverseMonoid(IsMatrixOverFiniteFieldMonoid, 2, 3, GF(7));;
gap> RandomInverseMonoid(IsMatrixOverFiniteFieldMonoid, 2, 3, GF(7), [1, 3]);;
gap> RandomInverseMonoid(IsMatrixOverFiniteFieldMonoid, "a");
Error, the 2nd argument (number of generators) is not a pos int
gap> RandomInverseMonoid(IsMatrixOverFiniteFieldMonoid, 1, "a");
Error, the 3rd argument (matrix dimension) is not a pos int
gap> RandomInverseMonoid(IsMatrixOverFiniteFieldMonoid, 1, 2, Integers);
Error, the 4th argument is not a finite field
gap> RandomInverseMonoid(IsMatrixOverFiniteFieldMonoid, 2, 3, GF(7), [0]);;
Error, the 5th argument (matrix ranks) is not a list of pos ints
gap> RandomInverseMonoid(IsMatrixOverFiniteFieldMonoid, 2, 3, GF(7), [1], 10);;
Error, there must be at most 5 arguments

# IsGeneratorsOfSemigroup
gap> coll := [
>  Matrix(GF(2),
>         [[Z(2) ^ 0, 0 * Z(2)],
>          [Z(2) ^ 0, Z(2) ^ 0]]),
>  Matrix(GF(2),
>         [[Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0],
>          [0 * Z(2), Z(2) ^ 0, 0 * Z(2)],
>          [Z(2) ^ 0, 0 * Z(2), 0 * Z(2)]])];;
gap> IsGeneratorsOfSemigroup(coll);
false

# GroupOfUnits
gap> GroupOfUnits(GLM(2, 2));
<group of 2x2 matrices over GF(2) with 2 generators>

# IsGeneratorsOfInverseSemigroup
gap> IsGeneratorsOfInverseSemigroup(Generators(GroupOfUnits(GLM(2, 2))));
true
gap> IsGeneratorsOfInverseSemigroup(Generators(GLM(2, 2)));
false

# FakeOne
gap> coll := [
>  Matrix(GF(2),
>         [[Z(2) ^ 0, 0 * Z(2)],
>          [Z(2) ^ 0, Z(2) ^ 0]]),
>  Matrix(GF(2),
>         [[Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0],
>          [0 * Z(2), Z(2) ^ 0, 0 * Z(2)],
>          [Z(2) ^ 0, 0 * Z(2), 0 * Z(2)]])];;
gap> FakeOne(coll);
Error, Assertion failure

# GroupOfUnits, for not a monoid
gap> S := GLM(2, 2);
<general linear monoid 2x2 over GF(2)>
gap> SemigroupIdeal(S, S.3);
<regular semigroup ideal of 2x2 matrices over GF(2) with 1 generator>
gap> GroupOfUnits(last);
fail

# MatrixOverFiniteFieldSchutzGrpElement error
gap> x := Matrix(GF(3),
> [[0 * Z(3), 0 * Z(3), 0 * Z(3)], [0 * Z(3), 0 * Z(3), 0 * Z(3)],
>  [0 * Z(3), Z(3), Z(3) ^ 0]]);;
gap> y := Matrix(GF(3),
> [[0 * Z(3), 0 * Z(3), 0 * Z(3)], [0 * Z(3), 0 * Z(3), 0 * Z(3)],
>  [0 * Z(3), 0 * Z(3), 0 * Z(3)]]);;
gap> MatrixOverFiniteFieldSchutzGrpElement(GLM(3, 3), x, y);
Error, Assertion failure

# MatrixOverFiniteFieldStabilizerAction
gap> y := Matrix(GF(3),
> [[0 * Z(3), 0 * Z(3), 0 * Z(3)], [0 * Z(3), 0 * Z(3), 0 * Z(3)],
>  [0 * Z(3), 0 * Z(3), 0 * Z(3)]]);;
gap> x := Matrix(GF(3),
> [[0 * Z(3), 0 * Z(3), 0 * Z(3)], [0 * Z(3), 0 * Z(3), 0 * Z(3)],
>  [0 * Z(3), Z(3), Z(3) ^ 0]]);;
gap> MatrixOverFiniteFieldStabilizerAction(GLM(3, 3),
> Matrix(GF(3),
> [[Z(3)]]), x);
[ [ 0*Z(3) ] ]

# MatrixOverFiniteFieldLambdaConjugator
gap> y := Matrix(GF(3),
> [[0 * Z(3), 0 * Z(3), 0 * Z(3)], [0 * Z(3), 0 * Z(3), 0 * Z(3)],
>  [0 * Z(3), 0 * Z(3), Z(3) ^ 0]]);;
gap> MatrixOverFiniteFieldLambdaConjugator(GLM(3, 3), y, y);
[ [ Z(3)^0 ] ]

# MatrixOverFiniteFieldIdempotentCreator
gap> y := Matrix(GF(3),
> [[0 * Z(3), 0 * Z(3), 0 * Z(3)], [0 * Z(3), 0 * Z(3), 0 * Z(3)],
>  [0 * Z(3), 0 * Z(3), Z(3) ^ 0]]);;
gap> MatrixOverFiniteFieldIdempotentCreator
> (GLM(3, 3), RowSpaceBasis(y), RowSpaceBasis(y));
[ [ 0*Z(3), 0*Z(3), 0*Z(3) ], [ 0*Z(3), 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), Z(3)^0 ] ]

# IsomorphismSemigroup for IsMatrixOverSemiringSemigroup and
# IsMatrixOverFiniteFieldSemigroup
gap> S := GLM(2, 2);
<general linear monoid 2x2 over GF(2)>
gap> T := Range(IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, S));
<general linear monoid 2x2 over GF(2)>
gap> IsIdenticalObj(S, T);
true
gap> T := Range(IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, GF(2), S));
<general linear monoid 2x2 over GF(2)>
gap> IsIdenticalObj(S, T);
true
gap> S := GLM(2, 4);
<general linear monoid 2x2 over GF(2^2)>
gap> T := Range(IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, GF(2), S));
<monoid of 256x256 matrices over GF(2) with 3 generators>

# IsomorphismSemigroup for a non-field
gap> S := FullTransformationMonoid(2);
<full transformation monoid of degree 2>
gap> IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, Integers, S);
Error, the 2nd argument (a ring) must be a finite field
gap> IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup, Integers, GLM(2, 2));
Error, the 2nd argument (a ring) must be a finite field

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/semigroups/semiffmat.tst");
