#############################################################################
##
#W  standard/semigroups/semibipart.tst
#Y  Copyright (C) 2015-2022                              James D. Mitchell
##                                                       
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local A, B, BruteForceInverseCheck, BruteForceIsoCheck, C, F, R, S, T, an
#@local gr1, gr2, inv, map, o1, o2, rels, x, y
gap> START_TEST("Semigroups package: standard/semigroups/semibipart.tst");
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

# IsomorphismSemigroup for IsBlockBijection checks if the semigroup is inverse
# inside the method and not in the filter to enter the method.
gap> S := ReesMatrixSemigroup(Group([(1, 2)]), [[()]]);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
gap> S := AsSemigroup(IsBlockBijectionSemigroup, S);
<block bijection group of size 2, degree 3 with 1 generator>
gap> IsomorphismSemigroup(IsBlockBijectionSemigroup,
> FullTransformationMonoid(2));
Error, the 2nd argument must be an inverse semigroup

# AsSemigroup: 
#   convert from IsPBRSemigroup to IsBipartitionSemigroup
gap> S := Semigroup([
> PBR([[-2, 1, 2], [-2, 1, 2]], [[-1], [-2, 1, 2]]),
> PBR([[-1, 1, 2], [-1, 1, 2]], [[-1, 1, 2], [-2]])]);
<pbr semigroup of degree 2 with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of size 2, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsFpSemigroup to IsBipartitionSemigroup
gap> F := FreeSemigroup(2);; AssignGeneratorVariables(F);;
gap> rels := [[s1 ^ 2, s1], [s1 * s2, s2], [s2 * s1, s1], [s2 ^ 2, s2]];;
gap> S := F / rels;
<fp semigroup with 2 generators and 4 relations of length 14>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of size 2, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBipartitionSemigroup to IsBipartitionSemigroup
gap> S := Semigroup([
> Bipartition([[1, 2, -2], [-1]]),
> Bipartition([[1, 2, -1], [-2]])]);
<bipartition semigroup of degree 2 with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of degree 2 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTransformationSemigroup to IsBipartitionSemigroup
gap> S := Semigroup([
> Transformation([2, 2]), Transformation([1, 1])]);
<transformation semigroup of degree 2 with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of degree 2 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBooleanMatSemigroup to IsBipartitionSemigroup
gap> S := Semigroup([
> Matrix(IsBooleanMat,
>        [[false, true], [false, true]]),
> Matrix(IsBooleanMat,
>        [[true, false], [true, false]])]);
<semigroup of 2x2 boolean matrices with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of degree 2 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMaxPlusMatrixSemigroup to IsBipartitionSemigroup
gap> S := Semigroup([
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, 0], [-infinity, 0]]),
> Matrix(IsMaxPlusMatrix,
>        [[0, -infinity], [0, -infinity]])]);
<semigroup of 2x2 max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of size 2, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMinPlusMatrixSemigroup to IsBipartitionSemigroup
gap> S := Semigroup([
> Matrix(IsMinPlusMatrix,
>        [[infinity, 0], [infinity, 0]]),
> Matrix(IsMinPlusMatrix,
>        [[0, infinity], [0, infinity]])]);
<semigroup of 2x2 min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of size 2, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsBipartitionSemigroup
gap> S := Semigroup([
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, 0], [-infinity, 0]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[0, -infinity], [0, -infinity]])]);
<semigroup of 2x2 projective max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of size 2, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsIntegerMatrixSemigroup to IsBipartitionSemigroup
gap> S := Semigroup([
> Matrix(Integers,
>        [[0, 1], [0, 1]]),
> Matrix(Integers,
>        [[1, 0], [1, 0]])]);
<semigroup of 2x2 integer matrices with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of size 2, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsBipartitionSemigroup
gap> S := Semigroup([
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, 0], [-infinity, 0]], 5),
> Matrix(IsTropicalMaxPlusMatrix,
>        [[0, -infinity], [0, -infinity]], 5)]);
<semigroup of 2x2 tropical max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of size 2, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsBipartitionSemigroup
gap> S := Semigroup([
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, 0], [infinity, 0]], 5),
> Matrix(IsTropicalMinPlusMatrix,
>        [[0, infinity], [0, infinity]], 5)]);
<semigroup of 2x2 tropical min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of size 2, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsNTPMatrixSemigroup to IsBipartitionSemigroup
gap> S := Semigroup([
> Matrix(IsNTPMatrix,
>        [[0, 1], [0, 1]], 1, 4),
> Matrix(IsNTPMatrix,
>        [[1, 0], [1, 0]], 1, 4)]);
<semigroup of 2x2 ntp matrices with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of size 2, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPBRMonoid to IsBipartitionSemigroup
gap> S := Monoid([
> PBR([[-1, 1], [-2, 2], [-3, 3]], [[-1, 1], [-2, 2], [-3, 3]]),
> PBR([[-3, -2, 1, 2, 3], [-3, -2, 1, 2, 3], [-3, -2, 1, 2, 3]],
>     [[-1], [-3, -2, 1, 2, 3], [-3, -2, 1, 2, 3]]),
> PBR([[-3, 1, 2, 3], [-3, 1, 2, 3], [-3, 1, 2, 3]],
>     [[-2, -1], [-2, -1], [-3, 1, 2, 3]])]);
<pbr monoid of degree 3 with 3 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition monoid of size 4, degree 4 with 3 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsFpMonoid to IsBipartitionSemigroup
gap> F := FreeMonoid(3);; AssignGeneratorVariables(F);;
gap> rels := [[m1 ^ 2, m1], [m1 * m2, m2], [m1 * m3, m3], [m2 * m1, m2],
> [m2 ^ 2, m2], [m2 * m3, m3], [m3 * m1, m3], [m3 * m2, m2], [m3 ^ 2, m3]];;
gap> S := F / rels;
<fp monoid with 3 generators and 9 relations of length 30>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition monoid of size 4, degree 4 with 3 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBipartitionMonoid to IsBipartitionSemigroup
gap> S := Monoid([
> Bipartition([[1, 2, 3, -2, -3], [-1]]),
> Bipartition([[1, 2, 3, -3], [-1, -2]])]);
<bipartition monoid of degree 3 with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition monoid of degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTransformationMonoid to IsBipartitionSemigroup
gap> S := Monoid([
> Transformation([2, 2, 2]), Transformation([3, 3, 3])]);
<transformation monoid of degree 3 with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition monoid of degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBooleanMatMonoid to IsBipartitionSemigroup
gap> S := Monoid([
> Matrix(IsBooleanMat,
>   [[false, true, false], [false, true, false], [false, true, false]]),
> Matrix(IsBooleanMat,
>   [[false, false, true], [false, false, true], [false, false, true]])]);
<monoid of 3x3 boolean matrices with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition monoid of degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMaxPlusMatrixMonoid to IsBipartitionSemigroup
gap> x := -infinity;;
gap> S := Monoid([
> Matrix(IsMaxPlusMatrix,
> [[x, 0, x],
>  [x, 0, x],
>  [x, 0, x]]),
> Matrix(IsMaxPlusMatrix,
>  [[x, x, 0],
>   [x, x, 0],
>   [x, x, 0]])]);
<monoid of 3x3 max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition monoid of size 3, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMinPlusMatrixMonoid to IsBipartitionSemigroup
gap> S := Monoid([
> Matrix(IsMinPlusMatrix,
>  [[infinity, 0, infinity],
>   [infinity, 0, infinity],
>   [infinity, 0, infinity]]),
> Matrix(IsMinPlusMatrix,
>  [[infinity, infinity, 0],
>   [infinity, infinity, 0],
>   [infinity, infinity, 0]])]);
<monoid of 3x3 min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition monoid of size 3, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixMonoid to IsBipartitionSemigroup
gap> x := -infinity;;
gap> S := Monoid([
> Matrix(IsProjectiveMaxPlusMatrix,
>   [[x, 0, x],
>    [x, 0, x],
>    [x, 0, x]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>   [[x, x, 0],
>   [x, x, 0],
>   [x, x, 0]])]);
<monoid of 3x3 projective max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition monoid of size 3, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsIntegerMatrixMonoid to IsBipartitionSemigroup
gap> S := Monoid([
> Matrix(Integers,
>        [[0, 1, 0], [0, 1, 0], [0, 1, 0]]),
> Matrix(Integers,
>        [[0, 0, 1], [0, 0, 1], [0, 0, 1]])]);
<monoid of 3x3 integer matrices with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition monoid of size 3, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixMonoid to IsBipartitionSemigroup
gap> x := -infinity;;
gap> S := Monoid([
> Matrix(IsTropicalMaxPlusMatrix,
>  [[x, 0, x],
>   [x, 0, x],
>   [x, 0, x]], 2),
> Matrix(IsTropicalMaxPlusMatrix,
>  [[x, x, 0],
>   [x, x, 0],
>   [x, x, 0]], 2)]);
<monoid of 3x3 tropical max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition monoid of size 3, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixMonoid to IsBipartitionSemigroup
gap> S := Monoid([
> Matrix(IsTropicalMinPlusMatrix,
>   [[infinity, 0, infinity],
>    [infinity, 0, infinity],
>    [infinity, 0, infinity]], 5),
> Matrix(IsTropicalMinPlusMatrix,
>   [[infinity, infinity, 0],
>    [infinity, infinity, 0],
>    [infinity, infinity, 0]], 5)]);
<monoid of 3x3 tropical min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition monoid of size 3, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsNTPMatrixMonoid to IsBipartitionSemigroup
gap> S := Monoid([
> Matrix(IsNTPMatrix,
>        [[0, 1, 0], [0, 1, 0], [0, 1, 0]], 1, 5),
> Matrix(IsNTPMatrix,
>        [[0, 0, 1], [0, 0, 1], [0, 0, 1]], 1, 5)]);
<monoid of 3x3 ntp matrices with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition monoid of size 3, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsPBRSemigroup to IsBipartitionMonoid
gap> S := Semigroup([
> PBR([[-2, 1], [-1, 2], [-3, 3], [-4, 4]],
>     [[-1, 2], [-2, 1], [-3, 3], [-4, 4]]),
> PBR([[1, 3], [-3, 2, 4], [1, 3], [-3, 2, 4]],
>     [[-4, -2, -1], [-4, -2, -1], [-3, 2, 4], [-4, -2, -1]]),
> PBR([[-4, -3, -2, -1, 1, 2, 3, 4], [-4, -3, -2, -1, 1, 2, 3, 4],
>      [-4, -3, -2, -1, 1, 2, 3, 4], [-4, -3, -2, -1, 1, 2, 3, 4]],
>     [[-4, -3, -2, -1, 1, 2, 3, 4], [-4, -3, -2, -1, 1, 2, 3, 4],
>      [-4, -3, -2, -1, 1, 2, 3, 4], [-4, -3, -2, -1, 1, 2, 3, 4]])]);
<pbr semigroup of degree 4 with 3 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 8, degree 8 with 3 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsFpSemigroup to IsBipartitionMonoid
gap> F := FreeSemigroup(3);; AssignGeneratorVariables(F);;
gap> rels := [[s1 * s3, s3], [s2 * s1, s2], [s2 ^ 2, s2], [s3 * s1, s3],
>             [s3 ^ 2, s3], [s1 ^ 3, s1], [s1 ^ 2 * s2, s2], [s2 * s3 * s2, s2],
>             [s3 * s2 * s3, s3]];;
gap> S := F / rels;
<fp semigroup with 3 generators and 9 relations of length 34>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 8, degree 8 with 3 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsBipartitionMonoid to IsBipartitionMonoid
gap> S := Semigroup([
> Bipartition([[1, -2], [2, -1], [3, -3], [4, -4]]),
> Bipartition([[1, 3], [2, 4, -3], [-1, -2, -4]]),
> Bipartition([[1, 2, 3, 4, -1, -2, -3, -4]])]);
<bipartition semigroup of degree 4 with 3 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 8, degree 8 with 3 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTransformationSemigroup to IsBipartitionMonoid
gap> S := Semigroup([
> Transformation([4, 2, 3, 1]),
> Transformation([5, 2, 7, 2, 5, 2, 7, 5]),
> Transformation([3, 6, 3, 3, 8, 6, 3, 8])]);
<transformation semigroup of degree 8 with 3 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of degree 8 with 3 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsBooleanMatSemigroup to IsBipartitionMonoid
gap> S := Semigroup([
> Matrix(IsBooleanMat,
>        [[false, false, false, true, false, false, false, false],
>         [false, true, false, false, false, false, false, false],
>         [false, false, true, false, false, false, false, false],
>         [true, false, false, false, false, false, false, false],
>         [false, false, false, false, true, false, false, false],
>         [false, false, false, false, false, true, false, false],
>         [false, false, false, false, false, false, true, false],
>         [false, false, false, false, false, false, false, true]]),
> Matrix(IsBooleanMat,
>        [[false, false, false, false, true, false, false, false],
>         [false, true, false, false, false, false, false, false],
>         [false, false, false, false, false, false, true, false],
>         [false, true, false, false, false, false, false, false],
>         [false, false, false, false, true, false, false, false],
>         [false, true, false, false, false, false, false, false],
>         [false, false, false, false, false, false, true, false],
>         [false, false, false, false, true, false, false, false]]),
> Matrix(IsBooleanMat,
>        [[false, false, true, false, false, false, false, false],
>         [false, false, false, false, false, true, false, false],
>         [false, false, true, false, false, false, false, false],
>         [false, false, true, false, false, false, false, false],
>         [false, false, false, false, false, false, false, true],
>         [false, false, false, false, false, true, false, false],
>         [false, false, true, false, false, false, false, false],
>         [false, false, false, false, false, false, false, true]])]);
<semigroup of 8x8 boolean matrices with 3 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of degree 8 with 3 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsMaxPlusMatrixSemigroup to IsBipartitionMonoid
gap> x := -infinity;;
gap> S := Semigroup([
> Matrix(IsMaxPlusMatrix,
>        [[x, x, x, 0, x, x, x, x],
>         [x, 0, x, x, x, x, x, x],
>         [x, x, 0, x, x, x, x, x],
>         [0, x, x, x, x, x, x, x],
>         [x, x, x, x, 0, x, x, x],
>         [x, x, x, x, x, 0, x, x],
>         [x, x, x, x, x, x, 0, x],
>         [x, x, x, x, x, x, x, 0]]),
> Matrix(IsMaxPlusMatrix,
>        [[x, x, x, x, 0, x, x, x],
>         [x, 0, x, x, x, x, x, x],
>         [x, x, x, x, x, x, 0, x],
>         [x, 0, x, x, x, x, x, x],
>         [x, x, x, x, 0, x, x, x],
>         [x, 0, x, x, x, x, x, x],
>         [x, x, x, x, x, x, 0, x],
>         [x, x, x, x, 0, x, x, x]]),
> Matrix(IsMaxPlusMatrix,
>        [[x, x, 0, x, x, x, x, x],
>         [x, x, x, x, x, 0, x, x],
>         [x, x, 0, x, x, x, x, x],
>         [x, x, 0, x, x, x, x, x],
>         [x, x, x, x, x, x, x, 0],
>         [x, x, x, x, x, 0, x, x],
>         [x, x, 0, x, x, x, x, x],
>         [x, x, x, x, x, x, x, 0]])]);
<semigroup of 8x8 max-plus matrices with 3 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 8, degree 8 with 3 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsMinPlusMatrixSemigroup to IsBipartitionMonoid
gap> S := Semigroup([
> Matrix(IsMinPlusMatrix,
>  [[infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity],
>   [infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity],
>   [0, infinity, infinity, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity],
>   [infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity],
>   [infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity],
>   [infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0]]),
> Matrix(IsMinPlusMatrix,
>  [[infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity],
>   [infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity],
>   [infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity],
>   [infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity],
>   [infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity]]),
> Matrix(IsMinPlusMatrix,
>  [[infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity],
>   [infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0],
>   [infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity],
>   [infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0]])]);
<semigroup of 8x8 min-plus matrices with 3 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 8, degree 8 with 3 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsBipartitionMonoid
gap> x := -infinity;;
gap> S := Semigroup([
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[x, x, x, 0, x, x, x, x],
>         [x, 0, x, x, x, x, x, x],
>         [x, x, 0, x, x, x, x, x],
>         [0, x, x, x, x, x, x, x],
>         [x, x, x, x, 0, x, x, x],
>         [x, x, x, x, x, 0, x, x],
>         [x, x, x, x, x, x, 0, x],
>         [x, x, x, x, x, x, x, 0]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[x, x, x, x, 0, x, x, x],
>         [x, 0, x, x, x, x, x, x],
>         [x, x, x, x, x, x, 0, x],
>         [x, 0, x, x, x, x, x, x],
>         [x, x, x, x, 0, x, x, x],
>         [x, 0, x, x, x, x, x, x],
>         [x, x, x, x, x, x, 0, x],
>         [x, x, x, x, 0, x, x, x]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[x, x, 0, x, x, x, x, x],
>         [x, x, x, x, x, 0, x, x],
>         [x, x, 0, x, x, x, x, x],
>         [x, x, 0, x, x, x, x, x],
>         [x, x, x, x, x, x, x, 0],
>         [x, x, x, x, x, 0, x, x],
>         [x, x, 0, x, x, x, x, x],
>         [x, x, x, x, x, x, x, 0]])]);
<semigroup of 8x8 projective max-plus matrices with 3 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 8, degree 8 with 3 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsIntegerMatrixSemigroup to IsBipartitionMonoid
gap> S := Semigroup([
> Matrix(Integers,
>        [[0, 0, 0, 1, 0, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0, 0, 0],
>         [1, 0, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 1, 0, 0, 0],
>         [0, 0, 0, 0, 0, 1, 0, 0],
>         [0, 0, 0, 0, 0, 0, 1, 0],
>         [0, 0, 0, 0, 0, 0, 0, 1]]),
> Matrix(Integers,
>        [[0, 0, 0, 0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 1, 0],
>         [0, 1, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 1, 0],
>         [0, 0, 0, 0, 1, 0, 0, 0]]),
> Matrix(Integers,
>        [[0, 0, 1, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 1, 0, 0],
>         [0, 0, 1, 0, 0, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 1],
>         [0, 0, 0, 0, 0, 1, 0, 0],
>         [0, 0, 1, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 1]])]);
<semigroup of 8x8 integer matrices with 3 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 8, degree 8 with 3 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsBipartitionMonoid
gap> x := -infinity;;
gap> S := Semigroup([
> Matrix(IsTropicalMaxPlusMatrix,
>  [[x, x, x, 0, x, x, x, x],
>   [x, 0, x, x, x, x, x, x],
>   [x, x, 0, x, x, x, x, x],
>   [0, x, x, x, x, x, x, x],
>   [x, x, x, x, 0, x, x, x],
>   [x, x, x, x, x, 0, x, x],
>   [x, x, x, x, x, x, 0, x],
>   [x, x, x, x, x, x, x, 0]], 5),
> Matrix(IsTropicalMaxPlusMatrix,
>  [[x, x, x, x, 0, x, x, x],
>   [x, 0, x, x, x, x, x, x],
>   [x, x, x, x, x, x, 0, x],
>   [x, 0, x, x, x, x, x, x],
>   [x, x, x, x, 0, x, x, x],
>   [x, 0, x, x, x, x, x, x],
>   [x, x, x, x, x, x, 0, x],
>   [x, x, x, x, 0, x, x, x]], 5),
> Matrix(IsTropicalMaxPlusMatrix,
>  [[x, x, 0, x, x, x, x, x],
>   [x, x, x, x, x, 0, x, x],
>   [x, x, 0, x, x, x, x, x],
>   [x, x, 0, x, x, x, x, x],
>   [x, x, x, x, x, x, x, 0],
>   [x, x, x, x, x, 0, x, x],
>   [x, x, 0, x, x, x, x, x],
>   [x, x, x, x, x, x, x, 0]], 5)]);
<semigroup of 8x8 tropical max-plus matrices with 3 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 8, degree 8 with 3 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsBipartitionMonoid
gap> S := Semigroup([
> Matrix(IsTropicalMinPlusMatrix,
>  [[infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity],
>   [infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity],
>   [0, infinity, infinity, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity],
>   [infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity],
>   [infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity],
>   [infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0]],
>  3),
> Matrix(IsTropicalMinPlusMatrix,
>  [[infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity],
>   [infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity],
>   [infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity],
>   [infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity],
>   [infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity]],
>  3),
> Matrix(IsTropicalMinPlusMatrix,
>  [[infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity],
>   [infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0],
>   [infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity],
>   [infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity],
>   [infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0]],
>  3)]);
<semigroup of 8x8 tropical min-plus matrices with 3 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 8, degree 8 with 3 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsNTPMatrixSemigroup to IsBipartitionMonoid
gap> S := Semigroup([
> Matrix(IsNTPMatrix,
>        [[0, 0, 0, 1, 0, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0, 0, 0],
>         [1, 0, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 1, 0, 0, 0],
>         [0, 0, 0, 0, 0, 1, 0, 0],
>         [0, 0, 0, 0, 0, 0, 1, 0],
>         [0, 0, 0, 0, 0, 0, 0, 1]], 3, 4),
> Matrix(IsNTPMatrix,
>        [[0, 0, 0, 0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 1, 0],
>         [0, 1, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 1, 0],
>         [0, 0, 0, 0, 1, 0, 0, 0]], 3, 4),
> Matrix(IsNTPMatrix,
>        [[0, 0, 1, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 1, 0, 0],
>         [0, 0, 1, 0, 0, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 1],
>         [0, 0, 0, 0, 0, 1, 0, 0],
>         [0, 0, 1, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 1]], 3, 4)]);
<semigroup of 8x8 ntp matrices with 3 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 8, degree 8 with 3 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsPBRMonoid to IsBipartitionMonoid
gap> S := Monoid([
> PBR([[-2], [-2], [-5], [-7], [-5], [-9], [-7], [-10], [-9], [-10],
>      [-12], [-12]],
>     [[], [1, 2], [], [], [3, 5], [], [4, 7], [], [6, 9], [8, 10], [],
>      [11, 12]]),
> PBR([[-3], [-4], [-6], [-4], [-8], [-6], [-4], [-8], [-11], [-8], [-11],
>      [-11]],
>     [[], [], [1], [2, 4, 7], [], [3, 6], [], [5, 8, 10], [], [], [9, 11, 12],
>      []])]);
<pbr monoid of degree 12 with 2 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 12, degree 12 with 2 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsFpMonoid to IsBipartitionMonoid
gap> F := FreeMonoid(2);; AssignGeneratorVariables(F);;
gap> rels := [[m1 ^ 2, m1],
>         [m1 * m2 ^ 2, m1 * m2],
>         [m2 ^ 3, m2 ^ 2],
>         [(m1 * m2) ^ 2, m1 * m2]];;
gap> S := F / rels;
<fp monoid with 2 generators and 4 relations of length 21>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 12, degree 12 with 2 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsBipartitionMonoid to IsBipartitionMonoid
gap> S := Monoid([
> Bipartition([[1, 2, -2], [3], [4], [-1, -3], [-4]]),
> Bipartition([[1, 4, -4], [2], [3, -1, -2, -3]])]);
<bipartition monoid of degree 4 with 2 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of degree 4 with 2 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTransformationMonoid to IsBipartitionMonoid
gap> S := Monoid([
> Transformation([2, 2, 5, 7, 5, 9, 7, 10, 9, 10, 12, 12]),
> Transformation([3, 4, 6, 4, 8, 6, 4, 8, 11, 8, 11, 11])]);
<transformation monoid of degree 12 with 2 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of degree 12 with 2 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsBooleanMatMonoid to IsBipartitionMonoid
gap> S := Monoid([
> Matrix(IsBooleanMat,
>        [[false, true, false, false, false, false, false, false, false, false,
>          false, false],
>         [false, true, false, false, false, false, false, false, false, false,
>          false, false],
>         [false, false, false, false, true, false, false, false, false, false,
>          false, false],
>         [false, false, false, false, false, false, true, false, false, false,
>          false, false],
>         [false, false, false, false, true, false, false, false, false, false,
>          false, false],
>         [false, false, false, false, false, false, false, false, true, false,
>          false, false],
>         [false, false, false, false, false, false, true, false, false, false,
>          false, false],
>         [false, false, false, false, false, false, false, false, false, true,
>          false, false],
>         [false, false, false, false, false, false, false, false, true, false,
>          false, false],
>         [false, false, false, false, false, false, false, false, false, true,
>          false, false],
>         [false, false, false, false, false, false, false, false, false, false,
>          false, true],
>         [false, false, false, false, false, false, false, false, false, false,
>          false, true]]),
> Matrix(IsBooleanMat,
>        [[false, false, true, false, false, false, false, false, false, false,
>          false, false],
>         [false, false, false, true, false, false, false, false, false, false,
>          false, false],
>         [false, false, false, false, false, true, false, false, false, false,
>          false, false],
>         [false, false, false, true, false, false, false, false, false, false,
>          false, false],
>         [false, false, false, false, false, false, false, true, false, false,
>          false, false],
>         [false, false, false, false, false, true, false, false, false, false,
>          false, false],
>         [false, false, false, true, false, false, false, false, false, false,
>          false, false],
>         [false, false, false, false, false, false, false, true, false, false,
>          false, false],
>         [false, false, false, false, false, false, false, false, false, false,
>          true, false],
>         [false, false, false, false, false, false, false, true, false, false,
>          false, false],
>         [false, false, false, false, false, false, false, false, false, false,
>          true, false],
>         [false, false, false, false, false, false, false, false, false, false,
>          true, false]])]);
<monoid of 12x12 boolean matrices with 2 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of degree 12 with 2 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsMaxPlusMatrixMonoid to IsBipartitionMonoid
gap> x := -infinity;;
gap> S := Monoid([
> Matrix(IsMaxPlusMatrix,
>        [[x, 0, x, x, x, x, x, x, x, x, x, x],
>         [x, 0, x, x, x, x, x, x, x, x, x, x],
>         [x, x, x, x, 0, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, 0, x, x, x, x, x],
>         [x, x, x, x, 0, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, x, x, 0, x, x, x],
>         [x, x, x, x, x, x, 0, x, x, x, x, x],
>         [x, x, x, x, x, x, x, x, x, 0, x, x],
>         [x, x, x, x, x, x, x, x, 0, x, x, x],
>         [x, x, x, x, x, x, x, x, x, 0, x, x],
>         [x, x, x, x, x, x, x, x, x, x, x, 0],
>         [x, x, x, x, x, x, x, x, x, x, x, 0]]),
> Matrix(IsMaxPlusMatrix,
>        [[x, x, 0, x, x, x, x, x, x, x, x, x],
>         [x, x, x, 0, x, x, x, x, x, x, x, x],
>         [x, x, x, x, x, 0, x, x, x, x, x, x],
>         [x, x, x, 0, x, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, x, 0, x, x, x, x],
>         [x, x, x, x, x, 0, x, x, x, x, x, x],
>         [x, x, x, 0, x, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, x, 0, x, x, x, x],
>         [x, x, x, x, x, x, x, x, x, x, 0, x],
>         [x, x, x, x, x, x, x, 0, x, x, x, x],
>         [x, x, x, x, x, x, x, x, x, x, 0, x],
>         [x, x, x, x, x, x, x, x, x, x, 0, x]])]);
<monoid of 12x12 max-plus matrices with 2 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 12, degree 12 with 2 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsMinPlusMatrixMonoid to IsBipartitionMonoid
gap> x := infinity;;
gap> S := Monoid([
> Matrix(IsMinPlusMatrix,
>        [[x, 0, x, x, x, x, x, x, x, x, x, x],
>         [x, 0, x, x, x, x, x, x, x, x, x, x],
>         [x, x, x, x, 0, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, 0, x, x, x, x, x],
>         [x, x, x, x, 0, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, x, x, 0, x, x, x],
>         [x, x, x, x, x, x, 0, x, x, x, x, x],
>         [x, x, x, x, x, x, x, x, x, 0, x, x],
>         [x, x, x, x, x, x, x, x, 0, x, x, x],
>         [x, x, x, x, x, x, x, x, x, 0, x, x],
>         [x, x, x, x, x, x, x, x, x, x, x, 0],
>         [x, x, x, x, x, x, x, x, x, x, x, 0]]),
> Matrix(IsMinPlusMatrix,
>        [[x, x, 0, x, x, x, x, x, x, x, x, x],
>         [x, x, x, 0, x, x, x, x, x, x, x, x],
>         [x, x, x, x, x, 0, x, x, x, x, x, x],
>         [x, x, x, 0, x, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, x, 0, x, x, x, x],
>         [x, x, x, x, x, 0, x, x, x, x, x, x],
>         [x, x, x, 0, x, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, x, 0, x, x, x, x],
>         [x, x, x, x, x, x, x, x, x, x, 0, x],
>         [x, x, x, x, x, x, x, 0, x, x, x, x],
>         [x, x, x, x, x, x, x, x, x, x, 0, x],
>         [x, x, x, x, x, x, x, x, x, x, 0, x]])]);
<monoid of 12x12 min-plus matrices with 2 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 12, degree 12 with 2 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsProjectiveMaxPlusMatrixMonoid to IsBipartitionMonoid
gap> x := -infinity;;
gap> S := Monoid([
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[x, 0, x, x, x, x, x, x, x, x, x, x],
>         [x, 0, x, x, x, x, x, x, x, x, x, x],
>         [x, x, x, x, 0, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, 0, x, x, x, x, x],
>         [x, x, x, x, 0, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, x, x, 0, x, x, x],
>         [x, x, x, x, x, x, 0, x, x, x, x, x],
>         [x, x, x, x, x, x, x, x, x, 0, x, x],
>         [x, x, x, x, x, x, x, x, 0, x, x, x],
>         [x, x, x, x, x, x, x, x, x, 0, x, x],
>         [x, x, x, x, x, x, x, x, x, x, x, 0],
>         [x, x, x, x, x, x, x, x, x, x, x, 0]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[x, x, 0, x, x, x, x, x, x, x, x, x],
>         [x, x, x, 0, x, x, x, x, x, x, x, x],
>         [x, x, x, x, x, 0, x, x, x, x, x, x],
>         [x, x, x, 0, x, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, x, 0, x, x, x, x],
>         [x, x, x, x, x, 0, x, x, x, x, x, x],
>         [x, x, x, 0, x, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, x, 0, x, x, x, x],
>         [x, x, x, x, x, x, x, x, x, x, 0, x],
>         [x, x, x, x, x, x, x, 0, x, x, x, x],
>         [x, x, x, x, x, x, x, x, x, x, 0, x],
>         [x, x, x, x, x, x, x, x, x, x, 0, x]])]);
<monoid of 12x12 projective max-plus matrices with 2 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 12, degree 12 with 2 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsIntegerMatrixMonoid to IsBipartitionMonoid
gap> S := Monoid([
> Matrix(Integers,
>        [[0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
>         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]]),
> Matrix(Integers,
>        [[0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
>         [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
>         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0]])]);
<monoid of 12x12 integer matrices with 2 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 12, degree 12 with 2 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTropicalMaxPlusMatrixMonoid to IsBipartitionMonoid
gap> x := -infinity;;
gap> S := Monoid([
> Matrix(IsTropicalMaxPlusMatrix,
>        [[x, 0, x, x, x, x, x, x, x, x, x, x],
>         [x, 0, x, x, x, x, x, x, x, x, x, x],
>         [x, x, x, x, 0, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, 0, x, x, x, x, x],
>         [x, x, x, x, 0, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, x, x, 0, x, x, x],
>         [x, x, x, x, x, x, 0, x, x, x, x, x],
>         [x, x, x, x, x, x, x, x, x, 0, x, x],
>         [x, x, x, x, x, x, x, x, 0, x, x, x],
>         [x, x, x, x, x, x, x, x, x, 0, x, x],
>         [x, x, x, x, x, x, x, x, x, x, x, 0],
>         [x, x, x, x, x, x, x, x, x, x, x, 0]], 5),
> Matrix(IsTropicalMaxPlusMatrix,
>        [[x, x, 0, x, x, x, x, x, x, x, x, x],
>         [x, x, x, 0, x, x, x, x, x, x, x, x],
>         [x, x, x, x, x, 0, x, x, x, x, x, x],
>         [x, x, x, 0, x, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, x, 0, x, x, x, x],
>         [x, x, x, x, x, 0, x, x, x, x, x, x],
>         [x, x, x, 0, x, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, x, 0, x, x, x, x],
>         [x, x, x, x, x, x, x, x, x, x, 0, x],
>         [x, x, x, x, x, x, x, 0, x, x, x, x],
>         [x, x, x, x, x, x, x, x, x, x, 0, x],
>         [x, x, x, x, x, x, x, x, x, x, 0, x]], 5)]);
<monoid of 12x12 tropical max-plus matrices with 2 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 12, degree 12 with 2 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTropicalMinPlusMatrixMonoid to IsBipartitionMonoid
gap> x := infinity;;
gap> S := Monoid([
> Matrix(IsTropicalMinPlusMatrix,
>        [[x, 0, x, x, x, x, x, x, x, x, x, x],
>         [x, 0, x, x, x, x, x, x, x, x, x, x],
>         [x, x, x, x, 0, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, 0, x, x, x, x, x],
>         [x, x, x, x, 0, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, x, x, 0, x, x, x],
>         [x, x, x, x, x, x, 0, x, x, x, x, x],
>         [x, x, x, x, x, x, x, x, x, 0, x, x],
>         [x, x, x, x, x, x, x, x, 0, x, x, x],
>         [x, x, x, x, x, x, x, x, x, 0, x, x],
>         [x, x, x, x, x, x, x, x, x, x, x, 0],
>         [x, x, x, x, x, x, x, x, x, x, x, 0]], 2),
> Matrix(IsTropicalMinPlusMatrix,
>        [[x, x, 0, x, x, x, x, x, x, x, x, x],
>         [x, x, x, 0, x, x, x, x, x, x, x, x],
>         [x, x, x, x, x, 0, x, x, x, x, x, x],
>         [x, x, x, 0, x, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, x, 0, x, x, x, x],
>         [x, x, x, x, x, 0, x, x, x, x, x, x],
>         [x, x, x, 0, x, x, x, x, x, x, x, x],
>         [x, x, x, x, x, x, x, 0, x, x, x, x],
>         [x, x, x, x, x, x, x, x, x, x, 0, x],
>         [x, x, x, x, x, x, x, 0, x, x, x, x],
>         [x, x, x, x, x, x, x, x, x, x, 0, x],
>         [x, x, x, x, x, x, x, x, x, x, 0, x]], 2)]);
<monoid of 12x12 tropical min-plus matrices with 2 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 12, degree 12 with 2 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsNTPMatrixMonoid to IsBipartitionMonoid
gap> S := Monoid([
> Matrix(IsNTPMatrix,
>        [[0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
>         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]], 5, 5),
> Matrix(IsNTPMatrix,
>        [[0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
>         [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
>         [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0]], 5, 5)]);
<monoid of 12x12 ntp matrices with 2 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<bipartition monoid of size 12, degree 12 with 2 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsReesMatrixSemigroup to IsBipartitionSemigroup
gap> R := ReesMatrixSemigroup(Group([(1, 2)]), [[()]]);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
gap> T := AsSemigroup(IsBipartitionSemigroup, R);
<block bijection group of size 2, degree 2 with 1 generator>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid
#   convert from IsReesMatrixSemigroup to IsBipartitionMonoid
gap> R := ReesMatrixSemigroup(Group([(1, 2)]), [[(1, 2)]]);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
gap> T := AsMonoid(IsBipartitionMonoid, R);
<block bijection group of size 2, degree 2 with 1 generator>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsReesZeroMatrixSemigroup to IsBipartitionSemigroup
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2)]),
>                                 [[(1, 2)]]);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
gap> T := AsSemigroup(IsBipartitionSemigroup, R);
<bipartition semigroup of size 3, degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid
#   convert from IsReesZeroMatrixSemigroup to IsBipartitionMonoid
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[()]]);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
gap> T := AsMonoid(IsBipartitionMonoid, R);
<bipartition monoid of size 3, degree 3 with 2 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from graph inverse to IsBipartitionSemigroup
gap> S := GraphInverseSemigroup(Digraph([[2], []]));
<finite graph inverse semigroup with 2 vertices, 1 edge>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of size 6, degree 7 with 4 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBlockBijectionSemigroup to IsBipartitionSemigroup
gap> S := InverseSemigroup(Bipartition([[1, -1, -3], [2, 3, -2]]));;
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<inverse block bijection semigroup of degree 3 with 1 generator>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBlockBijectionMonoid to IsBipartitionMonoid
gap> S := InverseMonoid([
> Bipartition([[1, -1, -3], [2, 3, -2]])]);;
gap> T := AsMonoid(IsBipartitionMonoid, S);
<inverse block bijection monoid of degree 3 with 1 generator>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBlockBijectionMonoid to IsBipartitionSemigroup
gap> S := InverseMonoid([
> Bipartition([[1, -1, -3], [2, 3, -2]])]);;
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<inverse block bijection monoid of degree 3 with 1 generator>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPartialPermSemigroup to IsBlockBijectionSemigroup
#   for an inverse semigroup
gap> S := SymmetricInverseMonoid(2);;
gap> T := AsSemigroup(IsBlockBijectionSemigroup, S);
<inverse block bijection monoid of degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBlockBijectionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPartialPermSemigroup to IsBlockBijectionSemigroup
#   for a non-inverse semigroup
gap> S := Semigroup([
> PartialPerm([1, 2, 4], [4, 2, 3]),
>  PartialPerm([1, 3, 4], [1, 4, 3]),
>  PartialPerm([1, 2, 3, 4], [3, 4, 1, 2]),
>  PartialPerm([1, 3, 4], [2, 4, 1])]);;
gap> T := AsSemigroup(IsBlockBijectionSemigroup, S);
<block bijection semigroup of degree 5 with 4 generators>
gap> IsInverseSemigroup(T);
false
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
gap> map := IsomorphismSemigroup(IsBlockBijectionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsReesMatrixSemigroup to IsBipartitionSemigroup
gap> R := ReesMatrixSemigroup(Group([(1, 2)]), [[(1, 2), (1, 2)], [(), ()]]);
<Rees matrix semigroup 2x2 over Group([ (1,2) ])>
gap> T := AsSemigroup(IsBipartitionSemigroup, R);
<bipartition semigroup of size 8, degree 9 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid
#   convert from IsReesMatrixSemigroup to IsBipartitionMonoid
gap> R := ReesMatrixSemigroup(Group([(1, 2)]), [[(1, 2)]]);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
gap> T := AsMonoid(IsBipartitionMonoid, R);
<block bijection group of size 2, degree 2 with 1 generator>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsReesZeroMatrixSemigroup to IsBipartitionSemigroup
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2)]),
>                                 [[(1, 2), (1, 2)], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
gap> T := AsSemigroup(IsBipartitionSemigroup, R);
<bipartition semigroup of size 9, degree 10 with 3 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid
#   convert from IsReesZeroMatrixSemigroup to IsBipartitionMonoid
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[(1, 2)]]);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
gap> T := AsMonoid(IsBipartitionMonoid, R);
<bipartition monoid of size 3, degree 3 with 2 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from graph inverse to IsBipartitionSemigroup
gap> S := GraphInverseSemigroup(Digraph([[2], []]));
<finite graph inverse semigroup with 2 vertices, 1 edge>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of size 6, degree 7 with 4 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from free band to IsBipartitionSemigroup
gap> S := FreeBand(2);
<free band on the generators [ x1, x2 ]>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of size 6, degree 7 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from perm group to IsBipartitionSemigroup
gap> S := DihedralGroup(IsPermGroup, 6);
Group([ (1,2,3), (2,3) ])
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<block bijection group of degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from perm group to IsBipartitionMonoid
gap> S := DihedralGroup(IsPermGroup, 6);
Group([ (1,2,3), (2,3) ])
gap> T := AsMonoid(IsBipartitionMonoid, S);
<block bijection group of degree 3 with 2 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from non-perm group to IsBipartitionSemigroup
gap> S := DihedralGroup(6);
<pc group of size 6 with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<block bijection group of size 6, degree 6 with 5 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from non-perm group to IsBipartitionMonoid
gap> S := DihedralGroup(6);
<pc group of size 6 with 2 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<block bijection group of size 6, degree 6 with 5 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBlockBijectionSemigroup to IsBipartitionSemigroup
gap> S := InverseSemigroup(Bipartition([[1, -1, -3], [2, 3, -2]]));;
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<inverse block bijection semigroup of degree 3 with 1 generator>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBlockBijectionMonoid to IsBipartitionMonoid
gap> S := InverseMonoid([
> Bipartition([[1, -1, -3], [2, 3, -2]])]);;
gap> T := AsMonoid(IsBipartitionMonoid, S);
<inverse block bijection monoid of degree 3 with 1 generator>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBlockBijectionMonoid to IsBipartitionSemigroup
gap> S := InverseMonoid([
> Bipartition([[1, -1, -3], [2, 3, -2]])]);;
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<inverse block bijection monoid of degree 3 with 1 generator>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPartialPermSemigroup to IsBipartitionSemigroup
gap> S := InverseSemigroup(PartialPerm([1, 2], [2, 1]),
>                          PartialPerm([1, 2], [3, 1]));
<inverse partial perm semigroup of rank 3 with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<inverse bipartition semigroup of degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> S := Semigroup(PartialPerm([1, 2], [2, 1]),
>                   PartialPerm([1, 2], [3, 1]));
<partial perm semigroup of rank 2 with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPartialPermMonoid to IsBipartitionMonoid
gap> S := InverseMonoid(PartialPerm([1, 2], [2, 1]),
>                       PartialPerm([1, 2], [3, 1]));
<inverse partial perm monoid of rank 3 with 2 generators>
gap> T := AsMonoid(IsBipartitionMonoid, S);
<inverse bipartition monoid of degree 3 with 2 generators>
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
gap> map := IsomorphismMonoid(IsBipartitionMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPartialPermMonoid to IsBipartitionSemigroup
gap> S := InverseMonoid(PartialPerm([1, 2], [2, 1]),
>                       PartialPerm([1, 2], [3, 1]));
<inverse partial perm monoid of rank 3 with 2 generators>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);
<inverse bipartition monoid of degree 3 with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from an ideal to IsBipartitionSemigroup
gap> S := SingularTransformationMonoid(3);;
gap> T := AsSemigroup(IsBipartitionSemigroup, S);;
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
gap> map := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from an ideal to IsBlockBijectionSemigroup
gap> S := SymmetricInverseMonoid(3);;
gap> S := SemigroupIdeal(S, S.3);;
gap> T := AsSemigroup(IsBlockBijectionSemigroup, S);
<inverse bipartition semigroup ideal of degree 4 with 1 generator>
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
gap> map := IsomorphismSemigroup(IsBlockBijectionSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# Test IsomorphismSemigroup
# for IsBlockBijectionSemigroup and IsBlockBijectionSemigroup
gap> S := DualSymmetricInverseMonoid(3);
<inverse block bijection monoid of degree 3 with 3 generators>
gap> map := IsomorphismSemigroup(IsBlockBijectionSemigroup, S);
<inverse block bijection monoid of degree 3 with 3 generators> -> 
<inverse block bijection monoid of degree 3 with 3 generators>

# Test RandomSemigroup
gap> S := RandomSemigroup(IsBipartitionSemigroup, 2, 5);;
gap> IsBipartitionSemigroup(S);
true
gap> DegreeOfBipartitionSemigroup(S);
5
gap> Length(GeneratorsOfSemigroup(S));
2
gap> S := RandomSemigroup(IsBlockBijectionSemigroup, 2, 5);;
gap> IsBlockBijectionSemigroup(S);
true
gap> DegreeOfBipartitionSemigroup(S);
5
gap> Length(GeneratorsOfSemigroup(S));
2

# Test RandomMonoid
gap> S := RandomMonoid(IsBipartitionMonoid, 4, 3);;
gap> DegreeOfBipartitionSemigroup(S);
3
gap> Length(GeneratorsOfMonoid(S));
4
gap> S := RandomMonoid(IsBlockBijectionMonoid, 2, 5);;
gap> IsBlockBijectionMonoid(S);
true
gap> DegreeOfBipartitionSemigroup(S);
5
gap> Length(GeneratorsOfMonoid(S));
2

# Test RandomInverseSemigroup
gap> S := RandomInverseSemigroup(IsBlockBijectionSemigroup, 2, 5);;
gap> IsBlockBijectionSemigroup(S);
true
gap> IsInverseSemigroup(S);
true
gap> DegreeOfBipartitionSemigroup(S);
5
gap> Length(GeneratorsOfInverseSemigroup(S));
2

# Test RandomInverseMonoid
gap> S := RandomInverseMonoid(IsBlockBijectionMonoid, 2, 5);;
gap> IsBlockBijectionMonoid(S);
true
gap> IsInverseMonoid(S);
true
gap> DegreeOfBipartitionSemigroup(S);
5
gap> Length(GeneratorsOfInverseMonoid(S));
2

# Test SemigroupViewStringPrefix
gap> S := Monoid([
>  Bipartition([[1, 3], [2, -2], [-1], [-3]]),
>  Bipartition([[1, -1], [2, -2], [3, -3]]),
>  Bipartition([[1, -1], [2, -2], [3], [-3]]),
>  Bipartition([[1], [2, -2], [3], [-1, -3]])]);;
gap> IsStarSemigroup(S);
true
gap> SemigroupViewStringPrefix(S);
"\>bipartition\< *-"
gap> S := Semigroup(DualSymmetricInverseMonoid(4));;
gap> IsStarSemigroup(S);
true
gap> SemigroupViewStringPrefix(S);
"\>block bijection\< *-"

# Test GroupOfUnits
gap> S := Semigroup(
> [Bipartition([[1, 5, 6, 10, 11, 12, 25, -3, -6, -16, -26, -27],
>     [2, 3, 4, 7, 13, 16, 17, 21, -2, -4, -13, -22, -24],
>     [8, 14, 18, -5, -7, -10, -19, -21],
>     [9, 15, 19, 20, 22, -1, -9, -14, -17, -23],
>     [23, 24, 26, 27, -8, -11, -18, -20], [-12, -15, -25]]),
>  Bipartition([[1, -1], [2, -2], [3, -3], [4, -4], [5, -5],
>     [6, -6], [7, -7], [8, -8], [9, -9], [10, -10], [11, -11],
>     [12, -12], [13, -14], [14, -16], [15, -17], [16, -19],
>     [17, -20], [18, -21], [19, -22], [20, -23], [21, -24],
>     [22, -13], [23, -25], [24, -26], [25, -15], [26, -27],
>     [27, -18]]),
>  Bipartition([[1, -2], [2, -3], [3, -5],
>     [4, -10], [5, -1], [6, -11], [7, -12], [8, -6], [9, -8],
>     [10, -7], [11, -9], [12, -4], [13, -13], [14, -14],
>     [15, -18], [16, -16], [17, -21], [18, -15], [19, -19],
>     [20, -24], [21, -17], [22, -22], [23, -26], [24, -20],
>     [25, -27], [26, -23], [27, -25]]),
>  Bipartition([[1, -4], [2, -6], [3, -7], [4, -8], [5, -9],
>     [6, -10], [7, -11], [8, -1], [9, -12], [10, -2], [11, -3],
>     [12, -5], [13, -15], [14, -17], [15, -18], [16, -20],
>     [17, -21], [18, -13], [19, -23], [20, -24], [21, -14],
>     [22, -25], [23, -26], [24, -16], [25, -27], [26, -19],
>     [27, -22]])]);;
gap> GroupOfUnits(S);
<block bijection group of degree 27 with 3 generators>
gap> StructureDescription(last);
"C5 x (C3 : C4)"
gap> S := Semigroup([
>  Bipartition([[1, 3], [2, -2], [-1], [-3]]),
>  Bipartition([[1, -1], [2, -2], [3], [-3]]),
>  Bipartition([[1], [2, -2], [3], [-1, -3]])]);;
gap> GroupOfUnits(S);
fail

# Test IsBlockBijectionSemigroup for an ideal
gap> IsBlockBijectionSemigroup(MinimalIdeal(PartitionMonoid(3)));
false
gap> IsBlockBijectionSemigroup(SingularDualSymmetricInverseMonoid(3));
true
gap> IsBlockBijectionSemigroup(OrderEndomorphisms(3));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `IsBlockBijectionSemigroup' on 1 argumen\
ts

# Test IsPartialPermBipartitionSemigroup for an ideal
gap> IsPartialPermBipartitionSemigroup(MinimalIdeal(PartitionMonoid(2)));
false
gap> S := AsSemigroup(IsBipartitionSemigroup, SymmetricInverseMonoid(2));;
gap> IsPartialPermBipartitionSemigroup(S);
true
gap> IsPartialPermBipartitionSemigroup(MinimalIdeal(S));
true
gap> S := Semigroup(AsSemigroup(IsBipartitionSemigroup,
> SymmetricInverseMonoid(2)));;
gap> IsPartialPermBipartitionSemigroup(MinimalIdeal(S));
true
gap> IsPartialPermBipartitionSemigroup(OrderEndomorphisms(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `IsPartialPermBipartitionSemigroup' on 1\
 arguments
gap> IsPermBipartitionGroup(MinimalIdeal(PartitionMonoid(2)));
false
gap> S := AsSemigroup(IsBipartitionSemigroup, SymmetricGroup(2));;
gap> IsPermBipartitionGroup(S);
true
gap> IsPermBipartitionGroup(MinimalIdeal(S));
true
gap> S := Semigroup(AsSemigroup(IsBipartitionSemigroup,
> SymmetricInverseMonoid(2)));;
gap> IsPermBipartitionGroup(MinimalIdeal(S));
false
gap> IsPermBipartitionGroup(SingularDualSymmetricInverseMonoid(3));
false
gap> IsPermBipartitionGroup(OrderEndomorphisms(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `IsPermBipartitionGroup' on 1 arguments

# Test NaturalLeqInverseSemigroup
gap> NaturalLeqInverseSemigroup(DualSymmetricInverseMonoid(3));
<Operation "NaturalLeqBlockBijection">
gap> S := SymmetricInverseMonoid(3);;
gap> NaturalLeqInverseSemigroup(AsSemigroup(IsBipartitionSemigroup,
>                                           S));
<Operation "NaturalLeqPartialPermBipartition">
gap> AsSemigroup(IsTransformationSemigroup, S);
<transformation monoid of degree 4 with 4 generators>
gap> AsSemigroup(IsBipartitionSemigroup, last);
<bipartition monoid of degree 4 with 4 generators>
gap> NaturalLeqInverseSemigroup(last);
function( x, y ) ... end
gap> NaturalLeqInverseSemigroup(PartitionMonoid(2));
Error, the argument is not an inverse semigroup

# Test NaturalPartialOrder
gap> NaturalPartialOrder(DualSymmetricInverseMonoid(3));
[ [  ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], 
  [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1, 2, 9, 16 ], 
  [ 1, 3, 8, 16 ], [ 1, 2, 11, 18 ], [ 1, 3, 11, 19 ], [ 1, 5, 8, 18 ], 
  [ 1, 5, 9, 19 ] ]
gap> o1 := NaturalPartialOrder(AsSemigroup(IsBipartitionSemigroup,
>                                    SymmetricInverseMonoid(3)));
[ [ 2, 6, 7, 26, 27, 33, 34 ], [ 7, 27, 34 ], [ 4, 5, 7, 29, 30, 32, 34 ], 
  [ 7, 30, 34 ], [ 7, 32, 34 ], [ 7, 33, 34 ], [ 34 ], 
  [ 9, 13, 14, 23, 24, 33, 34 ], [ 14, 24, 34 ], 
  [ 11, 12, 14, 28, 30, 31, 34 ], [ 14, 30, 34 ], [ 14, 31, 34 ], 
  [ 14, 33, 34 ], [ 34 ], [ 16, 20, 21, 22, 24, 32, 34 ], [ 21, 24, 34 ], 
  [ 18, 19, 21, 25, 27, 31, 34 ], [ 21, 27, 34 ], [ 21, 31, 34 ], 
  [ 21, 32, 34 ], [ 34 ], [ 24, 32, 34 ], [ 24, 33, 34 ], [ 34 ], 
  [ 27, 31, 34 ], [ 27, 33, 34 ], [ 34 ], [ 30, 31, 34 ], [ 30, 32, 34 ], 
  [ 34 ], [ 34 ], [ 34 ], [ 34 ], [  ] ]
gap> gr1 := DigraphReflexiveTransitiveClosure(Digraph(o1));
<immutable preorder digraph with 34 vertices, 139 edges>
gap> o2 := NaturalPartialOrder(SymmetricInverseMonoid(3));
[ [  ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1, 2, 6 ], [ 1, 2, 7 ], 
  [ 1, 3, 5 ], [ 1, 3, 7 ], [ 1, 4, 5 ], [ 1, 4, 6 ], [ 1 ], [ 1 ], [ 1 ], 
  [ 1, 5, 15 ], [ 1, 5, 16 ], [ 1, 6, 14 ], [ 1, 6, 16 ], [ 1, 7, 14 ], 
  [ 1, 7, 15 ], [ 1, 2, 15 ], [ 1, 2, 16 ], [ 1, 2, 6, 8, 16, 20, 24 ], 
  [ 1, 2, 7, 9, 15, 22, 23 ], [ 1, 3, 14 ], [ 1, 3, 16 ], 
  [ 1, 3, 5, 10, 16, 18, 28 ], [ 1, 3, 7, 11, 14, 21, 27 ], [ 1, 4, 14 ], 
  [ 1, 4, 15 ], [ 1, 4, 5, 12, 15, 17, 32 ], [ 1, 4, 6, 13, 14, 19, 31 ] ]
gap> gr2 := DigraphReflexiveTransitiveClosure(Digraph(o2));
<immutable preorder digraph with 34 vertices, 139 edges>
gap> IsomorphismDigraphs(gr1, gr2);
(1,25,19,31,14,3,26,20,32,15,33,16,12,27,6,24,5,23,18,13,28,21,4,9,10,30,7,2,
8,29,22,17,34)

# Test AsMonoid for 1 arg
gap> S := Semigroup(Transformation([2, 2, 3, 4]));
<commutative transformation semigroup of degree 2 with 1 generator>
gap> AsMonoid(S);
<trivial transformation group of degree 0 with 1 generator>
gap> T := AsSemigroup(IsBipartitionSemigroup, S);;
gap> IsMonoid(T);
false
gap> IsMonoidAsSemigroup(T);
true
gap> AsMonoid(T);
<trivial block bijection group of degree 1 with 1 generator>
gap> S := Semigroup(Bipartition([[1, 2, -1, -3], [3, -2]]),
>                   Bipartition([[1, 2, -2, -3], [3, -1]]),
>                   Bipartition([[1, 2, -3], [3, -1, -2]]),
>                   Bipartition([[1, 3, -1, -2], [2, -3]]),
>                   Bipartition([[1, -3], [2, 3, -1, -2]]));
<block bijection semigroup of degree 3 with 5 generators>
gap> S := AsMonoid(S);
fail

# Test GeneratorsOfInverseSemigroup
gap> S := Monoid(Bipartition([[1, -2], [2, -3], [3, -1]]),
>                Bipartition([[1, -2], [2, -1], [3, -3]]),
>                Bipartition([[1], [2, -1], [3, -2], [-3]]),
>                Bipartition([[1, -2], [2, -3], [3], [-1]]));;
gap> IsInverseSemigroup(S);
true
gap> HasGeneratorsOfInverseSemigroup(S);
false
gap> GeneratorsOfInverseSemigroup(S);
[ <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ]>, 
  <block bijection: [ 1, -2 ], [ 2, -3 ], [ 3, -1 ]>, 
  <block bijection: [ 1, -2 ], [ 2, -1 ], [ 3, -3 ]>, 
  <bipartition: [ 1 ], [ 2, -1 ], [ 3, -2 ], [ -3 ]> ]
gap> InverseSemigroup(last) = S;
true
gap> GeneratorsOfInverseMonoid(S);
[ <block bijection: [ 1, -2 ], [ 2, -3 ], [ 3, -1 ]>, 
  <block bijection: [ 1, -2 ], [ 2, -1 ], [ 3, -3 ]>, 
  <bipartition: [ 1 ], [ 2, -1 ], [ 3, -2 ], [ -3 ]> ]
gap> InverseMonoid(last) = S;
true

# Operator < 
gap> A := Semigroup(Bipartition([[1, 2], [-1], [-2]]),
> Bipartition([[1, 2, -1, -2]]));
<bipartition semigroup of degree 2 with 2 generators>
gap> B := Semigroup([Bipartition([[1, 2, -2], [-1]]),
>  Bipartition([[1, 2, -1], [-2]])]);
<bipartition semigroup of degree 2 with 2 generators>
gap> C := Monoid([Bipartition([[1, -1]])]);
<trivial block bijection group of degree 1 with 1 generator>
gap> A < B;
true
gap> B < A;
false
gap> B = A;
false
gap> C = A;
false
gap> C < A;
true
gap> C < B;
true

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/semigroups/semibipart.tst");
