############################################################################
##
#W  standard/semigroups/semirms.tst
#Y  Copyright (C) 2015-2022                              James D. Mitchell
##                                                          Wilf A. Wilson
##                                                              Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local BruteForceInverseCheck, BruteForceIsoCheck, F, G, H, I, R, RR, S, T, U
#@local a, an, b, c, comps, d, data, e, filename, first_occurrence, func, ht, i
#@local id, idems, inv, iso, map, mat, rels, x, y, z, zero
gap> START_TEST("Semigroups package: standard/semigroups/semirms.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();;

# helper functions
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

# AsSemigroup: 
#   convert from IsPBRSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup([
> PBR([[-1], [-5], [-1], [-1], [-5], [-5], [-1]],
>     [[1, 3, 4, 7], [], [], [], [2, 5, 6], [], []]),
> PBR([[-4], [-2], [-4], [-4], [-2], [-2], [-2]],
>     [[], [2, 5, 6, 7], [], [1, 3, 4], [], [], []]),
> PBR([[-3], [-6], [-3], [-3], [-6], [-6], [-3]],
>     [[], [], [1, 3, 4, 7], [], [], [2, 5, 6], []])]);
<pbr semigroup of degree 7 with 3 generators>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 2x3 over Group(())>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsFpSemigroup to IsReesMatrixSemigroup
gap> F := FreeSemigroup(3);; AssignGeneratorVariables(F);;
gap> rels := [[s1 ^ 2, s1],
>         [s1 * s3, s3],
>         [s2 ^ 2, s2],
>         [s3 * s1, s1],
>         [s3 * s2, s1 * s2],
>         [s3 ^ 2, s3],
>         [s1 * s2 * s1, s1],
>         [s1 * s2 * s3, s3],
>         [s2 * s1 * s2, s2]];;
gap> S := F / rels;
<fp semigroup with 3 generators and 9 relations of length 34>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 2x3 over Group(())>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBipartitionSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup([
> Bipartition([[1, 3, 4, 7, -1], [2, 5, 6, -5], [-2], [-3], [-4], [-6], [-7]]),
> Bipartition([[1, 3, 4, -4], [2, 5, 6, 7, -2], [-1], [-3], [-5], [-6], [-7]]),
> Bipartition([[1, 3, 4, 7, -3], [2, 5, 6, -6], [-1], [-2], [-4], [-5], [-7]])]);
<bipartition semigroup of degree 7 with 3 generators>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 2x3 over Group(())>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTransformationSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup([
> Transformation([1, 5, 1, 1, 5, 5, 1]),
> Transformation([4, 2, 4, 4, 2, 2, 2]),
> Transformation([3, 6, 3, 3, 6, 6, 3])]);
<transformation semigroup of degree 7 with 3 generators>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 2x3 over Group(())>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBooleanMatSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup([
> Matrix(IsBooleanMat,
>        [[true, false, false, false, false, false, false],
>         [false, false, false, false, true, false, false],
>         [true, false, false, false, false, false, false],
>         [true, false, false, false, false, false, false],
>         [false, false, false, false, true, false, false],
>         [false, false, false, false, true, false, false],
>         [true, false, false, false, false, false, false]]),
> Matrix(IsBooleanMat,
>        [[false, false, false, true, false, false, false],
>         [false, true, false, false, false, false, false],
>         [false, false, false, true, false, false, false],
>         [false, false, false, true, false, false, false],
>         [false, true, false, false, false, false, false],
>         [false, true, false, false, false, false, false],
>         [false, true, false, false, false, false, false]]),
> Matrix(IsBooleanMat,
>        [[false, false, true, false, false, false, false],
>         [false, false, false, false, false, true, false],
>         [false, false, true, false, false, false, false],
>         [false, false, true, false, false, false, false],
>         [false, false, false, false, false, true, false],
>         [false, false, false, false, false, true, false],
>         [false, false, true, false, false, false, false]])]);
<semigroup of 7x7 boolean matrices with 3 generators>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 2x3 over Group(())>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMaxPlusMatrixSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup([
> Matrix(IsMaxPlusMatrix,
>        [[0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity]]),
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity]]),
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity]])]);
<semigroup of 7x7 max-plus matrices with 3 generators>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 2x3 over Group(())>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMinPlusMatrixSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup([
> Matrix(IsMinPlusMatrix,
>        [[0, infinity, infinity, infinity, infinity, infinity, infinity],
>         [infinity, infinity, infinity, infinity, 0, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity, infinity, infinity],
>         [infinity, infinity, infinity, infinity, 0, infinity, infinity],
>         [infinity, infinity, infinity, infinity, 0, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity, infinity, infinity]]),
> Matrix(IsMinPlusMatrix,
>        [[infinity, infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity, infinity, infinity],
>         [infinity, infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity, infinity, infinity]]),
> Matrix(IsMinPlusMatrix,
>        [[infinity, infinity, 0, infinity, infinity, infinity, infinity],
>         [infinity, infinity, infinity, infinity, infinity, 0, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity, infinity],
>         [infinity, infinity, infinity, infinity, infinity, 0, infinity],
>         [infinity, infinity, infinity, infinity, infinity, 0, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity, infinity]])]);
<semigroup of 7x7 min-plus matrices with 3 generators>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 2x3 over Group(())>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup([
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity]])]);
<semigroup of 7x7 projective max-plus matrices with 3 generators>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 2x3 over Group(())>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsIntegerMatrixSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup([
> Matrix(Integers,
>        [[1, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 1, 0, 0],
>         [1, 0, 0, 0, 0, 0, 0],
>         [1, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 1, 0, 0],
>         [0, 0, 0, 0, 1, 0, 0],
>         [1, 0, 0, 0, 0, 0, 0]]),
> Matrix(Integers,
>        [[0, 0, 0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0, 0],
>         [0, 0, 0, 1, 0, 0, 0],
>         [0, 0, 0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0, 0]]),
> Matrix(Integers,
>        [[0, 0, 1, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 1, 0],
>         [0, 0, 1, 0, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 1, 0],
>         [0, 0, 0, 0, 0, 1, 0],
>         [0, 0, 1, 0, 0, 0, 0]])]);
<semigroup of 7x7 integer matrices with 3 generators>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 2x3 over Group(())>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup([
> Matrix(IsTropicalMaxPlusMatrix,
>      [[0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity],
>       [-infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity],
>       [0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity],
>       [0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity],
>       [-infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity],
>       [-infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity],
>       [0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity]],
>      1),
> Matrix(IsTropicalMaxPlusMatrix,
>      [[-infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity],
>       [-infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity],
>       [-infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity],
>       [-infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity],
>       [-infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity],
>       [-infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity],
>       [-infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity]],
>      1),
> Matrix(IsTropicalMaxPlusMatrix,
>      [[-infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity],
>       [-infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity],
>       [-infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity],
>       [-infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity],
>       [-infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity],
>       [-infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity],
>       [-infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity]],
>      1)]);
<semigroup of 7x7 tropical max-plus matrices with 3 generators>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 2x3 over Group(())>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup([
> Matrix(IsTropicalMinPlusMatrix,
>        [[0, infinity, infinity, infinity, infinity, infinity, infinity],
>         [infinity, infinity, infinity, infinity, 0, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity, infinity, infinity],
>         [infinity, infinity, infinity, infinity, 0, infinity, infinity],
>         [infinity, infinity, infinity, infinity, 0, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity, infinity, infinity]], 3),
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity, infinity, infinity],
>         [infinity, infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity, infinity, infinity]], 3),
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, infinity, 0, infinity, infinity, infinity, infinity],
>         [infinity, infinity, infinity, infinity, infinity, 0, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity, infinity],
>         [infinity, infinity, infinity, infinity, infinity, 0, infinity],
>         [infinity, infinity, infinity, infinity, infinity, 0, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity, infinity]],
>        3)]);
<semigroup of 7x7 tropical min-plus matrices with 3 generators>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 2x3 over Group(())>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsNTPMatrixSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup([
> Matrix(IsNTPMatrix,
>        [[1, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 1, 0, 0],
>         [1, 0, 0, 0, 0, 0, 0],
>         [1, 0, 0, 0, 0, 0, 0],
>         [0, 0, 0, 0, 1, 0, 0],
>         [0, 0, 0, 0, 1, 0, 0],
>         [1, 0, 0, 0, 0, 0, 0]], 4, 1),
> Matrix(IsNTPMatrix,
>        [[0, 0, 0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0, 0],
>         [0, 0, 0, 1, 0, 0, 0],
>         [0, 0, 0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0, 0]], 4, 1),
> Matrix(IsNTPMatrix,
>        [[0, 0, 1, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 1, 0],
>         [0, 0, 1, 0, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0, 0],
>         [0, 0, 0, 0, 0, 1, 0],
>         [0, 0, 0, 0, 0, 1, 0],
>         [0, 0, 1, 0, 0, 0, 0]], 4, 1)]);
<semigroup of 7x7 ntp matrices with 3 generators>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 2x3 over Group(())>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPBRSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup([
> PBR([[-4], [-5], [-3], [-1], [-2], [-1]],
>     [[4, 6], [5], [3], [1], [2], []]),
> PBR([[-1], [-2], [-3], [-4], [-5], [-2]],
>     [[1], [2, 6], [3], [4], [5], []]),
> PBR([[-3], [-3], [-3], [-3], [-3], [-3]],
>     [[], [], [1, 2, 3, 4, 5, 6], [], [], []])]);
<pbr semigroup of degree 6 with 3 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 2x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsFpSemigroup to IsReesZeroMatrixSemigroup
gap> F := FreeSemigroup(3);; AssignGeneratorVariables(F);;
gap> rels := [[s1 * s2, s1],
>         [s1 * s3, s3],
>         [s2 ^ 2, s2],
>         [s2 * s3, s3],
> [s3 * s1, s3],
>         [s3 * s2, s3],
>         [s3 ^ 2, s3],
>         [s1 ^ 3, s1],
>         [s2 * s1 ^ 2, s2]];;
gap> S := F / rels;
<fp semigroup with 3 generators and 9 relations of length 32>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 2x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBipartitionSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup([
> Bipartition([[1, -4], [2, -5], [3, -3], [4, 6, -1], [5, -2], [-6]]),
> Bipartition([[1, -1], [2, 6, -2], [3, -3], [4, -4], [5, -5], [-6]]),
> Bipartition([[1, 2, 3, 4, 5, 6, -3], [-1], [-2], [-4], [-5], [-6]])]);
<bipartition semigroup of degree 6 with 3 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> (IsActingSemigroup(S) and UnderlyingSemigroup(T) = Group((1, 4)(2, 5)))
> or (not IsActingSemigroup(S) and  UnderlyingSemigroup(T) = Group((1, 2)));
true
gap> Length(Rows(T)) = 2 and Length(Columns(T)) = 1;
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTransformationSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup([
> Transformation([4, 5, 3, 1, 2, 1]),
> Transformation([1, 2, 3, 4, 5, 2]),
> Transformation([3, 3, 3, 3, 3, 3])]);
<transformation semigroup of degree 6 with 3 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> (IsActingSemigroup(S) and UnderlyingSemigroup(T) = Group((1, 4)(2, 5)))
> or (not IsActingSemigroup(S) and  UnderlyingSemigroup(T) = Group((1, 2)));
true
gap> Length(Rows(T)) = 2 and Length(Columns(T)) = 1;
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBooleanMatSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup([
> Matrix(IsBooleanMat,
>        [[false, false, false, true, false, false],
>         [false, false, false, false, true, false],
>         [false, false, true, false, false, false],
>         [true, false, false, false, false, false],
>         [false, true, false, false, false, false],
>         [true, false, false, false, false, false]]),
> Matrix(IsBooleanMat,
>        [[true, false, false, false, false, false],
>         [false, true, false, false, false, false],
>         [false, false, true, false, false, false],
>         [false, false, false, true, false, false],
>         [false, false, false, false, true, false],
>         [false, true, false, false, false, false]]),
> Matrix(IsBooleanMat,
>        [[false, false, true, false, false, false],
>         [false, false, true, false, false, false],
>         [false, false, true, false, false, false],
>         [false, false, true, false, false, false],
>         [false, false, true, false, false, false],
>         [false, false, true, false, false, false]])]);
<semigroup of 6x6 boolean matrices with 3 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 2x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMaxPlusMatrixSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup([
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, -infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity, -infinity]]),
> Matrix(IsMaxPlusMatrix,
>        [[0, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, 0, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity]]),
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity]])]);
<semigroup of 6x6 max-plus matrices with 3 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 2x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMinPlusMatrixSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup([
> Matrix(IsMinPlusMatrix,
>        [[infinity, infinity, infinity, 0, infinity, infinity],
>         [infinity, infinity, infinity, infinity, 0, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity, infinity]]),
> Matrix(IsMinPlusMatrix,
>        [[0, infinity, infinity, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, infinity, infinity, 0, infinity, infinity],
>         [infinity, infinity, infinity, infinity, 0, infinity],
>         [infinity, 0, infinity, infinity, infinity, infinity]]),
> Matrix(IsMinPlusMatrix,
>        [[infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity]])]);
<semigroup of 6x6 min-plus matrices with 3 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 2x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup([
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, -infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity, -infinity]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[0, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, 0, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity]])]);
<semigroup of 6x6 projective max-plus matrices with 3 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 2x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsIntegerMatrixSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup([
> Matrix(Integers,
>        [[0, 0, 0, 1, 0, 0],
>         [0, 0, 0, 0, 1, 0],
>         [0, 0, 1, 0, 0, 0],
>         [1, 0, 0, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0],
>         [1, 0, 0, 0, 0, 0]]),
> Matrix(Integers,
>        [[1, 0, 0, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0],
>         [0, 0, 0, 1, 0, 0],
>         [0, 0, 0, 0, 1, 0],
>         [0, 1, 0, 0, 0, 0]]),
> Matrix(Integers,
>        [[0, 0, 1, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0]])]);
<semigroup of 6x6 integer matrices with 3 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 2x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup([
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, -infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, 0, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity, -infinity]], 1),
> Matrix(IsTropicalMaxPlusMatrix,
>        [[0, -infinity, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, 0, -infinity, -infinity],
>         [-infinity, -infinity, -infinity, -infinity, 0, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity, -infinity]], 1),
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, -infinity, 0, -infinity, -infinity, -infinity]], 1)]);
<semigroup of 6x6 tropical max-plus matrices with 3 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 2x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup([
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, infinity, infinity, 0, infinity, infinity],
>         [infinity, infinity, infinity, infinity, 0, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity, infinity]], 3),
> Matrix(IsTropicalMinPlusMatrix,
>        [[0, infinity, infinity, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, infinity, infinity, 0, infinity, infinity],
>         [infinity, infinity, infinity, infinity, 0, infinity],
>         [infinity, 0, infinity, infinity, infinity, infinity]], 3),
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity],
>         [infinity, infinity, 0, infinity, infinity, infinity]], 3)]);
<semigroup of 6x6 tropical min-plus matrices with 3 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 2x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsNTPMatrixSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup([
> Matrix(IsNTPMatrix,
>        [[0, 0, 0, 1, 0, 0],
>         [0, 0, 0, 0, 1, 0],
>         [0, 0, 1, 0, 0, 0],
>         [1, 0, 0, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0],
>         [1, 0, 0, 0, 0, 0]], 4, 1),
> Matrix(IsNTPMatrix,
>        [[1, 0, 0, 0, 0, 0],
>         [0, 1, 0, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0],
>         [0, 0, 0, 1, 0, 0],
>         [0, 0, 0, 0, 1, 0],
>         [0, 1, 0, 0, 0, 0]], 4, 1),
> Matrix(IsNTPMatrix,
>        [[0, 0, 1, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0],
>         [0, 0, 1, 0, 0, 0]], 4, 1)]);
<semigroup of 6x6 ntp matrices with 3 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 2x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPBRMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid([
> PBR([[-3], [-2], [-1]], [[3], [2], [1]]),
> PBR([[-2], [-2], [-2]], [[], [1, 2, 3], []])]);
<pbr monoid of degree 3 with 2 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsFpMonoid to IsReesZeroMatrixSemigroup
gap> F := FreeMonoid(2);; AssignGeneratorVariables(F);;
gap> rels := [[m1 ^ 2, One(F)],
>         [m1 * m2, m2],
>         [m2 * m1, m2],
>         [m2 ^ 2, m2]];;
gap> S := F / rels;
<fp monoid with 2 generators and 4 relations of length 13>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBipartitionMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid([
> Bipartition([[1, -3], [2, -2], [3, -1]]),
> Bipartition([[1, 2, 3, -2], [-1], [-3]])]);
<bipartition monoid of degree 3 with 2 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> (IsActingSemigroup(S) and UnderlyingSemigroup(T) = Group([(1, 3)]))
> or (not IsActingSemigroup(S) and UnderlyingSemigroup(T) = Group([(1, 2)]));
true
gap> Length(Rows(T)) = 1 and Length(Columns(T)) = 1;
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTransformationMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid([
> Transformation([3, 2, 1]), Transformation([2, 2, 2])]);
<transformation monoid of degree 3 with 2 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> (IsActingSemigroup(S) and UnderlyingSemigroup(T) = Group([(1, 3)]))
> or (not IsActingSemigroup(S) and UnderlyingSemigroup(T) = Group([(1, 2)]));
true
gap> Length(Rows(T)) = 1 and Length(Columns(T)) = 1;
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBooleanMatMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid([
> Matrix(IsBooleanMat,
>        [[false, false, true], [false, true, false],
>         [true, false, false]]),
> Matrix(IsBooleanMat,
>        [[false, true, false],
>         [false, true, false],
>         [false, true, false]])]);
<monoid of 3x3 boolean matrices with 2 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMaxPlusMatrixMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid([
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, -infinity, 0],
>         [-infinity, 0, -infinity],
>         [0, -infinity, -infinity]]),
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, 0, -infinity],
>         [-infinity, 0, -infinity],
>         [-infinity, 0, -infinity]])]);
<monoid of 3x3 max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMinPlusMatrixMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid([
> Matrix(IsMinPlusMatrix,
>        [[infinity, infinity, 0],
>         [infinity, 0, infinity],
>         [0, infinity, infinity]]),
> Matrix(IsMinPlusMatrix,
>        [[infinity, 0, infinity],
>         [infinity, 0, infinity],
>         [infinity, 0, infinity]])]);
<monoid of 3x3 min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid([
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, -infinity, 0],
>         [-infinity, 0, -infinity],
>         [0, -infinity, -infinity]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, 0, -infinity],
>         [-infinity, 0, -infinity],
>         [-infinity, 0, -infinity]])]);
<monoid of 3x3 projective max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsIntegerMatrixMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid([
> Matrix(Integers,
>        [[0, 0, 1],
>         [0, 1, 0],
>         [1, 0, 0]]),
> Matrix(Integers,
>        [[0, 1, 0],
>         [0, 1, 0],
>         [0, 1, 0]])]);
<monoid of 3x3 integer matrices with 2 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid([
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, -infinity, 0],
>         [-infinity, 0, -infinity],
>         [0, -infinity, -infinity]], 1),
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, 0, -infinity],
>         [-infinity, 0, -infinity],
>         [-infinity, 0, -infinity]], 1)]);
<monoid of 3x3 tropical max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid([
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, infinity, 0],
>         [infinity, 0, infinity],
>         [0, infinity, infinity]], 3),
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, 0, infinity],
>         [infinity, 0, infinity],
>         [infinity, 0, infinity]], 3)]);
<monoid of 3x3 tropical min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsNTPMatrixMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid([
> Matrix(IsNTPMatrix,
>        [[0, 0, 1],
>         [0, 1, 0],
>         [1, 0, 0]], 4, 1),
> Matrix(IsNTPMatrix,
>        [[0, 1, 0],
>         [0, 1, 0],
>         [0, 1, 0]], 4, 1)]);
<monoid of 3x3 ntp matrices with 2 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsFpMonoid to IsReesZeroMatrixSemigroup
gap> F := FreeMonoid(1);; AssignGeneratorVariables(F);;
gap> rels := [[m1 ^ 2, m1]];;
gap> S := F / rels;
<fp monoid with 1 generator and 1 relation of length 4>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group(())>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPBRMonoid to IsReesMatrixSemigroup
gap> S := Monoid([
> PBR([[-2], [-1]], [[2], [1]])]);
<commutative pbr monoid of degree 2 with 1 generator>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsFpMonoid to IsReesMatrixSemigroup
gap> F := FreeMonoid(1);; AssignGeneratorVariables(F);;
gap> rels := [[m1 ^ 2, One(F)]];;
gap> S := F / rels;
<fp monoid with 1 generator and 1 relation of length 3>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBipartitionMonoid to IsReesMatrixSemigroup
gap> S := InverseMonoid([
> Bipartition([[1, -1], [2, -2]]),
> Bipartition([[1, -2], [2, -1]])]);
<block bijection group of degree 2 with 1 generator>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBipartitionSemigroup to IsReesMatrixSemigroup
gap> S := InverseSemigroup([
> Bipartition([[1, -1], [2, -2]]),
> Bipartition([[1, -2], [2, -1]])]);
<block bijection group of degree 2 with 1 generator>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBipartitionMonoid to IsReesZeroMatrixSemigroup
gap> S := InverseMonoid(Bipartition([[1, -1], [2, -2]]),
> Bipartition([[1, -2], [2, -1]]),
> Bipartition([[1, 2, -1, -2]]));
<inverse block bijection monoid of degree 2 with 2 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBipartitionSemigroup to IsReesZeroMatrixSemigroup
gap> S := InverseSemigroup(Bipartition([[1, -1], [2, -2]]),
> Bipartition([[1, -2], [2, -1]]),
> Bipartition([[1, 2, -1, -2]]));
<inverse block bijection monoid of degree 2 with 2 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPartialPermSemigroup to IsReesMatrixSemigroup
gap> S := InverseSemigroup(PartialPerm([1, 2], [2, 1]));;
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPartialPermMonoid to IsReesMatrixSemigroup
gap> S := InverseMonoid(PartialPerm([1, 2], [2, 1]));;
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPartialPermSemigroup to IsReesZeroMatrixSemigroup
gap> S := InverseSemigroup(PartialPerm([1, 2], [2, 1]),
>                          PartialPerm([]));;
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPartialPermMonoid to IsReesZeroMatrixSemigroup
gap> S := InverseMonoid(PartialPerm([1, 2], [2, 1]),
>                       PartialPerm([]));;
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTransformationMonoid to IsReesMatrixSemigroup
gap> S := Monoid([
> Transformation([2, 1])]);
<commutative transformation monoid of degree 2 with 1 generator>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBooleanMatMonoid to IsReesMatrixSemigroup
gap> S := Monoid([
> Matrix(IsBooleanMat,
>        [[false, true], [true, false]])]);
<commutative monoid of 2x2 boolean matrices with 1 generator>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMaxPlusMatrixMonoid to IsReesMatrixSemigroup
gap> S := Monoid([
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, 0], [0, -infinity]])]);
<commutative monoid of 2x2 max-plus matrices with 1 generator>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMinPlusMatrixMonoid to IsReesMatrixSemigroup
gap> S := Monoid([
> Matrix(IsMinPlusMatrix,
>        [[infinity, 0], [0, infinity]])]);
<commutative monoid of 2x2 min-plus matrices with 1 generator>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixMonoid to IsReesMatrixSemigroup
gap> S := Monoid([
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, 0], [0, -infinity]])]);
<commutative monoid of 2x2 projective max-plus matrices with 1 generator>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsIntegerMatrixMonoid to IsReesMatrixSemigroup
gap> S := Monoid([
> Matrix(Integers,
>        [[0, 1], [1, 0]])]);
<commutative monoid of 2x2 integer matrices with 1 generator>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixMonoid to IsReesMatrixSemigroup
gap> S := Monoid([
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, 0], [0, -infinity]], 1)]);
<commutative monoid of 2x2 tropical max-plus matrices with 1 generator>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixMonoid to IsReesMatrixSemigroup
gap> S := Monoid([
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, 0], [0, infinity]], 3)]);
<commutative monoid of 2x2 tropical min-plus matrices with 1 generator>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsNTPMatrixMonoid to IsReesMatrixSemigroup
gap> S := Monoid([
> Matrix(IsNTPMatrix,
>        [[0, 1], [1, 0]], 4, 1)]);
<commutative monoid of 2x2 ntp matrices with 1 generator>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsFpMonoid to IsReesMatrixSemigroup
gap> F := FreeMonoid(0);; AssignGeneratorVariables(F);;
gap> rels := [];;
gap> S := F / rels;;
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group(())>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPermGroup to IsReesMatrixSemigroup
gap> S := DihedralGroup(IsPermGroup, 4);
Group([ (1,2), (3,4) ])
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,3)(2,4), (1,4)(2,3) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsGroup to IsReesMatrixSemigroup
gap> S := DihedralGroup(4);
<pc group of size 4 with 2 generators>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group([ (1,2)(3,4), (1,3)(2,4) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsReesMatrixSemigroup to IsReesMatrixSemigroup
gap> S := ReesMatrixSemigroup(Group([(1, 2)]), [[(1, 2), (1, 2)], [(), ()]]);
<Rees matrix semigroup 2x2 over Group([ (1,2) ])>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 2x2 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsReesZeroMatrixSemigroup to IsReesZeroMatrixSemigroup
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2)]),
>                                 [[(1, 2), (1, 2)], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
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
gap> map := IsomorphismSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsGraphInverseSemigroup to IsReesMatrixSemigroup
gap> S := GraphInverseSemigroup(Digraph([[]]));
<finite graph inverse semigroup with 1 vertex, 0 edges>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group(())>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from a free band to IsReesMatrixSemigroup
gap> S := FreeBand(1);
<free band on the generators [ x1 ]>
gap> T := AsSemigroup(IsReesMatrixSemigroup, S);
<Rees matrix semigroup 1x1 over Group(())>
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
gap> map := IsomorphismSemigroup(IsReesMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# RMSNormalization 1:
# for a Rees matrix semigroup over a group without an inverse op
gap> G := Semigroup([
> Transformation([4, 4, 2, 3, 4]),
> Transformation([4, 4, 3, 2, 4])]);
<transformation semigroup of degree 5 with 2 generators>
gap> IsGroup(G);
false
gap> IsGroupAsSemigroup(G);
true
gap> mat := [
> [Transformation([4, 4, 2, 3, 4]), Transformation([4, 4, 3, 2, 4])],
> [Transformation([2, 2, 4, 3, 2]), Transformation([3, 3, 4, 2, 3])]];;
gap> R := ReesMatrixSemigroup(G, mat);;
gap> iso := RMSNormalization(R);;
gap> inv := InverseGeneralMapping(iso);;
gap> ForAll(R, x -> (x ^ iso) ^ inv = x);
true

# RMSNormalization 2:
# for a Rees matrix semigroup over IsGroup
gap> R := ReesMatrixSemigroup(SymmetricGroup(4),
> [[(1, 3, 2), (), (1, 4, 2)],
>  [(), (1, 3)(2, 4), (1, 2, 3, 4)],
>  [(3, 4), (1, 3), (1, 2, 4, 3)],
>  [(), (2, 4, 3), (1, 2)]]);
<Rees matrix semigroup 3x4 over Sym( [ 1 .. 4 ] )>
gap> iso := RMSNormalization(R);;
gap> Matrix(Range(iso));
[ [ (), (), () ], [ (), (1,2,4), (1,4) ], [ (), (1,2)(3,4), (1,4)(2,3) ], 
  [ (), (1,3)(2,4), (1,4,3,2) ] ]

# RMSNormalization 3:
# error checking
gap> G := FullTransformationMonoid(4);;
gap> RMSNormalization(ReesMatrixSemigroup(G, [[IdentityTransformation]]));
Error, the underlying semigroup of the argument (a  subsemigroup of a Rees mat\
rix semigroup) does not satisfy IsGroupAsSemigroup

# RZMSNormalization 1:
# for a Rees 0-matrix semigroup over a group without an inverse op
gap> G := Semigroup([
> Transformation([1, 3, 2, 1]),
> Transformation([3, 1, 2, 3])]);
<transformation semigroup of degree 4 with 2 generators>
gap> IsGroup(G);
false
gap> IsGroupAsSemigroup(G);
true
gap> mat := [
> [Transformation([2, 1, 3, 2]), 0, 0],
> [0, 0, Transformation([2, 3, 1, 2])],
> [0, Transformation([3, 1, 2, 3]), Transformation([2, 3, 1, 2])]];;
gap> R := ReesZeroMatrixSemigroup(G, mat);;
gap> iso := RZMSNormalization(R);;
gap> inv := InverseGeneralMapping(iso);;
gap> ForAll(R, x -> (x ^ iso) ^ inv = x);
true

# RZMSNormalization 2:
# for a Rees 0-matrix semigroup over the symmetric group S4
gap> G := SymmetricGroup(4);;
gap> R := ReesZeroMatrixSemigroup(G,
> [[0, (1, 4)(2, 3), 0], [0, 0, (4, 2, 3)], [(2, 4, 3), 0, 0]]);
<Rees 0-matrix semigroup 3x3 over Sym( [ 1 .. 4 ] )>
gap> IsInverseSemigroup(R);
true
gap> iso := RZMSNormalization(R);
<Rees 0-matrix semigroup 3x3 over Sym( [ 1 .. 4 ] )> -> 
<Rees 0-matrix semigroup 3x3 over Sym( [ 1 .. 4 ] )>
gap> S := Range(iso);
<Rees 0-matrix semigroup 3x3 over Sym( [ 1 .. 4 ] )>
gap> Matrix(S);
[ [ (), 0, 0 ], [ 0, (), 0 ], [ 0, 0, () ] ]
gap> inv := InverseGeneralMapping(iso);;
gap> x := MultiplicativeZero(R) ^ iso;
0
gap> x ^ inv = MultiplicativeZero(R);
true
gap> x := RMSElement(R, 1, (), 1);
(1,(),1)
gap> x ^ iso;
(1,(2,4,3),2)
gap> (x ^ iso) ^ inv = x;
true
gap> G := SymmetricGroup(4);;
gap> mat := [
> [0, 0, (1, 3, 2), 0, (), 0, 0, (1, 2, 3)],
> [(), 0, 0, 0, 0, (1, 3, 4, 2), 0, (2, 4)],
> [0, 0, 0, (1, 2, 3), 0, 0, (1, 3, 2), 0],
> [0, 0, 0, 0, 0, 0, (1, 4, 2, 3), 0],
> [(), (1, 2, 3), (1, 2), 0, 0, 0, 0, 0],
> [0, (), 0, 0, 0, (1, 2), 0, 0]];;
gap> R := ReesZeroMatrixSemigroup(G, mat);
<Rees 0-matrix semigroup 8x6 over Sym( [ 1 .. 4 ] )>
gap> iso := RZMSNormalization(R);
<Rees 0-matrix semigroup 8x6 over Sym( [ 1 .. 4 ] )> -> 
<Rees 0-matrix semigroup 8x6 over Sym( [ 1 .. 4 ] )>
gap> S := Range(iso);
<Rees 0-matrix semigroup 8x6 over Sym( [ 1 .. 4 ] )>

# check that mat is in the 'normal' form
gap> mat := Matrix(S);
[ [ (), (), (), 0, 0, 0, 0, 0 ], [ (), 0, 0, (), (), 0, 0, 0 ], 
  [ 0, 0, (), (1,4,2), 0, (), 0, 0 ], [ 0, 0, 0, 0, (), (2,3,4), 0, 0 ], 
  [ 0, 0, 0, 0, 0, 0, (), () ], [ 0, 0, 0, 0, 0, 0, 0, () ] ]
gap> first_occurrence := l -> First([1 .. Length(l)], i -> l[i] <> 0);;
gap> x := Length(mat);;
gap> ForAll([1 .. x - 1],
> i -> first_occurrence(mat[i]) <= first_occurrence(mat[i + 1]));
true
gap> ForAll([1 .. Length(mat[1]) - 1], i ->
> first_occurrence(mat{[1 .. x]}[i]) <= first_occurrence(mat{[1 .. x]}[i + 1]));
true

# check that the connected components are grouped together
gap> comps := RZMSConnectedComponents(S);
[ [ [ 1, 2, 3, 4, 5, 6 ], [ 1, 2, 3, 4 ] ], [ [ 7, 8 ], [ 5, 6 ] ] ]
gap> Concatenation(List(comps, x -> x[1])) = Rows(R);
true
gap> Concatenation(List(comps, x -> x[2])) = Columns(R);
true

# RZMSNormalization 3:
# for a Rees 0-matrix semigroup over a non-IsGroup group,
# and a non-group semigroup
gap> T := FullTransformationMonoid(4);;
gap> G := GroupOfUnits(T);;
gap> R := ReesZeroMatrixSemigroup(T,
> [[0, IdentityTransformation], [IdentityTransformation, 0]]);
<Rees 0-matrix semigroup 2x2 over <full transformation monoid of degree 4>>
gap> RZMSNormalization(R);;
gap> Matrix(Range(last));
[ [ IdentityTransformation, 0 ], [ 0, IdentityTransformation ] ]
gap> R := ReesZeroMatrixSemigroup(G, [[Transformation([3, 1, 4, 2])]]);;
gap> RZMSNormalization(R);;
gap> Matrix(Range(last));
[ [ IdentityTransformation ] ]

# RZMSNormalization 4:
# for a Rees 0-matrix semigroup with some all-zero rows/columns
gap> G := Group(());;
gap> R := ReesZeroMatrixSemigroup(G, [[0, 0], [0, ()]]);;
gap> Matrix(Range(RZMSNormalization(R)));
[ [ (), 0 ], [ 0, 0 ] ]
gap> R := ReesZeroMatrixSemigroup(G, [[0, 0], [(), ()]]);;
gap> Matrix(Range(RZMSNormalization(R)));
[ [ (), () ], [ 0, 0 ] ]
gap> R := ReesZeroMatrixSemigroup(G, [[0, ()], [0, ()]]);;
gap> Matrix(Range(RZMSNormalization(R)));
[ [ (), 0 ], [ (), 0 ] ]

# IsInverseSemigroup for a Rees 0-matrix semigroup 1:
# easy true examples
gap> R := ReesZeroMatrixSemigroup(Group(()), [[()]]);
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> IsInverseSemigroup(R);
true
gap> IsInverseSemigroup(AsSemigroup(IsTransformationSemigroup, R));
true

#
gap> T := Semigroup(Transformation([2, 1]));
<commutative transformation semigroup of degree 2 with 1 generator>
gap> IsGroupAsSemigroup(T);
true
gap> R := ReesZeroMatrixSemigroup(T, [[Transformation([2, 1])]]);;
gap> IsInverseSemigroup(R);
true
gap> IsInverseSemigroup(AsSemigroup(IsTransformationSemigroup, R));
true

# IsInverseSemigroup for a Rees 0-matrix semigroup 2:
# false because of underlying semigroup
gap> x := Transformation([1, 1, 2]);;
gap> T := Semigroup(x);;
gap> IsInverseSemigroup(T);
false
gap> R := ReesZeroMatrixSemigroup(T, [[0, x], [0, x ^ 2]]);;
gap> IsInverseSemigroup(R);
false
gap> IsInverseSemigroup(AsSemigroup(IsTransformationSemigroup, R));
false

# T is known not to be regular
gap> T := Semigroup(x);;
gap> IsRegularSemigroup(T);
false
gap> R := ReesZeroMatrixSemigroup(T, [[0, x], [0, x ^ 2]]);;
gap> IsInverseSemigroup(R);
false
gap> IsInverseSemigroup(AsSemigroup(IsTransformationSemigroup, R));
false

# T is known not to be a monoid
gap> T := Semigroup(x);;
gap> IsMonoidAsSemigroup(T);
false
gap> R := ReesZeroMatrixSemigroup(T, [[0, x], [0, x ^ 2]]);;
gap> IsInverseSemigroup(R);
false
gap> IsInverseSemigroup(AsSemigroup(IsTransformationSemigroup, R));
false

# T is known not to have group of units
gap> T := Semigroup(x);;
gap> GroupOfUnits(T);
fail
gap> R := ReesZeroMatrixSemigroup(T, [[0, x], [0, x ^ 2]]);;
gap> IsInverseSemigroup(R);
false
gap> IsInverseSemigroup(AsSemigroup(IsTransformationSemigroup, R));
false

# T does not have a group of units
gap> T := Semigroup(x);;
gap> R := ReesZeroMatrixSemigroup(T, [[x, 0], [0, x ^ 2]]);;
gap> IsInverseSemigroup(R);
false
gap> IsInverseSemigroup(AsSemigroup(IsTransformationSemigroup, R));
false

# IsInverseSemigroup for a Rees 0-matrix semigroup 3:
# false because of matrix
gap> S := Semigroup(SymmetricInverseMonoid(3));
<partial perm monoid of rank 3 with 4 generators>
gap> id := Identity(S);
<identity partial perm on [ 1, 2, 3 ]>
gap> zero := MultiplicativeZero(S);
<empty partial perm>

# Non-diagonal matrix: Rows or columns without precisely one non-zero entry
gap> R := ReesZeroMatrixSemigroup(S, [[0, id, 0], [id, 0, 0], [0, 0, 0]]);;
gap> IsInverseSemigroup(R);
false
gap> R := ReesZeroMatrixSemigroup(S, [[0, 0, 0], [id, 0, 0], [0, id, 0]]);;
gap> IsInverseSemigroup(R);
false
gap> R := ReesZeroMatrixSemigroup(S, [[0, 0, id], [id, id, 0], [0, id, 0]]);;
gap> IsInverseSemigroup(R);
false
gap> R := ReesZeroMatrixSemigroup(S, [[0, id, 0], [0, id, 0], [0, id, 0]]);;
gap> IsInverseSemigroup(R);
false
gap> R := ReesZeroMatrixSemigroup(S, [[id, 0, 0], [id, id, 0], [0, id, 0]]);;
gap> IsInverseSemigroup(R);
false
gap> R := ReesZeroMatrixSemigroup(S, [[zero, id]]);;
gap> IsInverseSemigroup(R);  # Non-square matrix
false
gap> R := ReesZeroMatrixSemigroup(S, [[id, 0, 0], [0, 0, id], [0, zero, 0]]);;
gap> IsInverseSemigroup(R);  # Matrix entries not in the group of units
false
gap> y := PartialPerm([1, 2, 0]);
<identity partial perm on [ 1, 2 ]>
gap> R := ReesZeroMatrixSemigroup(S, [[id, 0, 0], [0, 0, id], [0, y, 0]]);;
gap> IsInverseSemigroup(R);
false
gap> T := FullTransformationMonoid(3);;
gap> R := ReesZeroMatrixSemigroup(T, [[Identity(T)]]);;
gap> IsInverseSemigroup(R);  # Semigroup is not an inverse monoid
false
gap> y := PartialPerm([3, 2, 1]);;
gap> R := ReesZeroMatrixSemigroup(S, [[id, 0, 0], [0, id, 0], [0, 0, y]]);;
gap> IsInverseSemigroup(R);
true

# NrIdempotents and Idempotents 1:
# for an inverse Rees 0-matrix semigroup
gap> S := SymmetricInverseMonoid(4);
<symmetric inverse monoid of degree 4>
gap> x := PartialPerm([2, 1, 4, 3]);;
gap> y := PartialPerm([2, 4, 3, 1]);;
gap> R := ReesZeroMatrixSemigroup(S, [[0, x], [y, 0]]);
<Rees 0-matrix semigroup 2x2 over <symmetric inverse monoid of degree 4>>
gap> IsInverseSemigroup(R);
true
gap> NrIdempotents(R);
33
gap> NrIdempotents(R) = NrIdempotents(S) * Length(Rows(R)) + 1;
true
gap> idems := Idempotents(R);;
gap> IsDuplicateFreeList(idems);
true
gap> Length(idems) = NrIdempotents(R);
true
gap> ForAll(idems, IsIdempotent);
true

# NrIdempotents and Idempotents 2:
# for an inverse Rees 0-matrix semigroup over a group
gap> R := ReesZeroMatrixSemigroup(Group(()), [[()]]);
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> NrIdempotents(R);
2
gap> Idempotents(R);
[ 0, (1,(),1) ]
gap> AsSet(Idempotents(R)) = Elements(R);
true
gap> IsBand(R);
true
gap> x := Transformation([2, 1]);;
gap> T := Semigroup(x);
<commutative transformation semigroup of degree 2 with 1 generator>
gap> R := ReesZeroMatrixSemigroup(T, [[x, 0], [x, x ^ 2]]);;
gap> NrIdempotents(R);
4
gap> Idempotents(R);
[ 0, (1,Transformation( [ 2, 1 ] ),1), (1,Transformation( [ 2, 1 ] ),2), 
  (2,IdentityTransformation,2) ]
gap> ForAll(Idempotents(R), IsIdempotent);
true
gap> x := Transformation([1, 1, 2]);;
gap> T := Semigroup(x);
<commutative transformation semigroup of degree 3 with 1 generator>
gap> R := ReesZeroMatrixSemigroup(T, [[x, 0], [0, x ^ 2]]);;
gap> NrIdempotents(R);
3
gap> i := ShallowCopy(Idempotents(R));;
gap> Sort(i);
gap> i;
[ 0, (1,Transformation( [ 1, 1, 1 ] ),1), (2,Transformation( [ 1, 1, 1 ] ),2) 
 ]
gap> ForAll(Idempotents(R), IsIdempotent);
true

# NrIdempotents and Idempotents 3:
# for an sub-RZMS of an Rees 0-matrix semigroup
gap> S := SymmetricInverseMonoid(4);;
gap> x := PartialPerm([2, 1, 4, 3]);;
gap> y := PartialPerm([2, 4, 3, 1]);;
gap> z := PartialPerm([0, 0, 0, 0]);;
gap> R := ReesZeroMatrixSemigroup(S, [[x, x, 0], [y, 0, 0], [0, 0, x]]);;
gap> IsInverseSemigroup(R);
false

#
gap> T := Semigroup(RMSElement(R, 1, x, 1));;
gap> IsReesZeroMatrixSemigroup(T);
false
gap> NrIdempotents(T);
1
gap> Idempotents(T);
[ (1,(1,2)(3,4),1) ]
gap> T := Semigroup(RMSElement(R, 1, x, 1));;
gap> IsReesZeroMatrixSemigroup(T);
false
gap> NrIdempotents(T);
1
gap> Idempotents(T);
[ (1,(1,2)(3,4),1) ]
gap> T := Semigroup(RMSElement(R, 1, y ^ -1, 2));;
gap> IsInverseSemigroup(T);
true
gap> NrIdempotents(T);
1
gap> T := Semigroup(RMSElement(R, 1, y ^ -1, 2));;
gap> IsInverseSemigroup(T);
true
gap> Idempotents(T);
[ (1,(1,4,2)(3),2) ]
gap> T := Semigroup(RMSElement(R, 1, y ^ -1, 2));;
gap> NrIdempotents(T);
1
gap> T := Semigroup(RMSElement(R, 1, y ^ -1, 2));;
gap> Idempotents(T);
[ (1,(1,4,2)(3),2) ]
gap> T := Semigroup(RMSElement(R, 1, y ^ -1, 2));;
gap> SetIsInverseSemigroup(T, true);
gap> Idempotents(T);
[ (1,(1,4,2)(3),2) ]

#
gap> S := SymmetricInverseMonoid(4);;
gap> x := PartialPerm([2, 1, 4, 3]);;
gap> y := PartialPerm([2, 4, 3, 1]);;
gap> z := PartialPerm([0, 0, 0, 0]);;
gap> R := ReesZeroMatrixSemigroup(S, [[x, x, 0], [y, 0, 0], [0, 0, x]]);;
gap> IsInverseSemigroup(R);
false
gap> T := ReesZeroMatrixSubsemigroup(R, [2, 3], S, [1, 2, 3]);
<Rees 0-matrix semigroup 2x3 over <symmetric inverse monoid of degree 4>>
gap> IsInverseSemigroup(T);
false
gap> T := ReesZeroMatrixSubsemigroup(R, [2, 3], S, [1, 2]);
<Rees 0-matrix semigroup 2x2 over <symmetric inverse monoid of degree 4>>
gap> IsInverseSemigroup(T);
false
gap> T := ReesZeroMatrixSubsemigroup(R, [1, 2], S, [2, 3]);
<Rees 0-matrix semigroup 2x2 over <symmetric inverse monoid of degree 4>>
gap> IsInverseSemigroup(T);
false
gap> T := ReesZeroMatrixSubsemigroup(R, [2, 3], S, [1, 3]);;
gap> IsInverseSemigroup(T) and NrIdempotents(T) = 33;
true
gap> idems := Idempotents(T);;
gap> ForAll(idems, IsIdempotent);
true

# MatrixEntries: Test for Issue #164
gap> mat := [
>  [Bipartition([[1, 2, 3, -2, -3], [-1]]), 0, 0, 0],
>  [0, Bipartition([[1, 3, -1], [2, -2, -3]]), 0,
>   Bipartition([[1, -1], [2, 3], [-2], [-3]])],
>  [0, 0, Bipartition([[1, 2, 3, -3], [-1], [-2]]), 0]];
[ [ <bipartition: [ 1, 2, 3, -2, -3 ], [ -1 ]>, 0, 0, 0 ], 
  [ 0, <block bijection: [ 1, 3, -1 ], [ 2, -2, -3 ]>, 0, 
      <bipartition: [ 1, -1 ], [ 2, 3 ], [ -2 ], [ -3 ]> ], 
  [ 0, 0, <bipartition: [ 1, 2, 3, -3 ], [ -1 ], [ -2 ]>, 0 ] ]
gap> R := ReesZeroMatrixSemigroup(PartitionMonoid(3), mat);;
gap> MatrixEntries(R);
[ 0, <bipartition: [ 1, 2, 3, -2, -3 ], [ -1 ]>, 
  <bipartition: [ 1, 2, 3, -3 ], [ -1 ], [ -2 ]>, 
  <block bijection: [ 1, 3, -1 ], [ 2, -2, -3 ]>, 
  <bipartition: [ 1, -1 ], [ 2, 3 ], [ -2 ], [ -3 ]> ]
gap> mat := [
>  [Bipartition([[1, 2], [3, -1, -2], [-3]]),
>   Bipartition([[1, -2], [2, 3, -3], [-1]])],
>  [Bipartition([[1, 2, -1], [3], [-2, -3]]),
>   Bipartition([[1, 3, -1], [2, -2, -3]])],
>  [Bipartition([[1, 2, -2, -3], [3, -1]]),
>   Bipartition([[1, -1, -2], [2, 3, -3]])]];
[ [ <bipartition: [ 1, 2 ], [ 3, -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1, -2 ], [ 2, 3, -3 ], [ -1 ]> ], 
  [ <bipartition: [ 1, 2, -1 ], [ 3 ], [ -2, -3 ]>, 
      <block bijection: [ 1, 3, -1 ], [ 2, -2, -3 ]> ], 
  [ <block bijection: [ 1, 2, -2, -3 ], [ 3, -1 ]>, 
      <block bijection: [ 1, -1, -2 ], [ 2, 3, -3 ]> ] ]
gap> R := ReesZeroMatrixSemigroup(PartitionMonoid(3), mat);;
gap> MatrixEntries(R);
[ <bipartition: [ 1, 2, -1 ], [ 3 ], [ -2, -3 ]>, 
  <block bijection: [ 1, 2, -2, -3 ], [ 3, -1 ]>, 
  <bipartition: [ 1, 2 ], [ 3, -1, -2 ], [ -3 ]>, 
  <block bijection: [ 1, 3, -1 ], [ 2, -2, -3 ]>, 
  <block bijection: [ 1, -1, -2 ], [ 2, 3, -3 ]>, 
  <bipartition: [ 1, -2 ], [ 2, 3, -3 ], [ -1 ]> ]

# IsomorphismReesMatrixSemigroup, infinite
gap> IsomorphismReesMatrixSemigroup(FreeInverseSemigroup(2));
Error, the argument must be a finite simple semigroup

# IsomorphismReesZeroMatrixSemigroup, infinite
gap> IsomorphismReesZeroMatrixSemigroup(FreeSemigroup(2));
Error, the argument must be a finite 0-simple semigroup

# IsomorphismReesZeroMatrixSemigroup, error, 1/1
gap> IsomorphismReesZeroMatrixSemigroup(RegularBooleanMatMonoid(2));
Error, the argument (a semigroup) is not a 0-simple semigroup

# IsomorphismReesMatrixSemigroup: for a simple semigroup
gap> S := SemigroupIdeal(
> Semigroup([
>   Bipartition([[1, 2, 3, 6, 7, 8, -2, -4, -5, -6], [4, 5, -1, -8], [-3],
>                [-7]]),
>   Bipartition([[1, 5, 8], [2, 7, -3, -6], [3, 4, -4, -7], [6, -1, -5],
>                [-2, -8]])]),
> [Bipartition([[1, 2, 3, 4, 5, 6, 7, 8, -1, -2, -4, -5, -6, -8], [-3],
>               [-7]])]);;
gap> IsomorphismReesMatrixSemigroup(S);;

# IsomorphismReesMatrixSemigroup: for a 0-simple semigroup 1/2
gap> S := Semigroup([
> Transformation([1, 1, 5, 1, 3, 1, 9, 1, 7, 5]),
>   Transformation([1, 1, 2, 1, 4, 1, 6, 1, 8, 2]),
>   Transformation([1, 5, 1, 3, 1, 9, 1, 7, 1, 7])]);;
gap> IsomorphismReesZeroMatrixSemigroup(S);;

# IsomorphismReesMatrixSemigroup: for a 0-simple semigroup 2/2
gap> S := Semigroup([
> Transformation([1, 1, 5, 1, 3, 1, 9, 1, 7, 5]),
>   Transformation([1, 1, 2, 1, 4, 1, 6, 1, 8, 2]),
>   Transformation([1, 5, 1, 3, 1, 9, 1, 7, 1, 7])]);;
gap> S := Semigroup(MultiplicativeZero(S), S);;
gap> IsomorphismReesZeroMatrixSemigroup(S);;

# IsomorphismReesMatrixSemigroup: for a non-simple or non-0-simple
gap> S := Semigroup(Transformation([2, 1]), Transformation([2, 2]));;
gap> IsomorphismReesMatrixSemigroup(S);
Error, the argument (a semigroup) is not simple

# IsomorphismReesZeroMatrixSemigroup, bug 1/1
gap> S := Semigroup(PartialPerm([1]), PartialPerm([]));
<partial perm monoid of rank 1 with 2 generators>
gap> IsomorphismReesMatrixSemigroup(S);
Error, the argument (a semigroup) is not simple
gap> IsomorphismReesZeroMatrixSemigroup(S);;
gap> Size(Range(last));
2

# ChooseHashFunction: Test for RZMS elements over pc group
gap> G := SmallGroup(4, 2);;
gap> a := AsList(G)[1];; b := AsList(G)[2];;
gap> mat := [[a, 0, b], [b, 0, 0], [0, a, b]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> x := RMSElement(S, 1, a, 2);;
gap> func := ChooseHashFunction(x, 25531).func;;
gap> data := ChooseHashFunction(x, 25531).data;
[ 101, 25531 ]
gap> x := RMSElement(S, 2, b, 3);;
gap> func(x, data);;
gap> x := MultiplicativeZero(S);;
gap> func(x, data);;

# ChooseHashFunction: Test for RZMS elements over transformation semigroups
gap> G := FullTransformationMonoid(3);;
gap> a := AsList(G)[1];; b := AsList(G)[2];;
gap> mat := [[a, 0, b], [b, 0, 0], [0, a, b]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> x := RMSElement(S, 1, a, 2);;
gap> func := ChooseHashFunction(x, 25531).func;;
gap> data := ChooseHashFunction(x, 25531).data;
25531
gap> func(x, data);;
gap> x := MultiplicativeZero(S);;
gap> func(x, data);;
gap> func := ChooseHashFunction(x, 25531).func;;
gap> data := ChooseHashFunction(x, 25531).data;;
gap> func(x, data);;

# ChooseHashFunction: Test for RZMS elements over bipartition semigroups
gap> G := BrauerMonoid(3);;
gap> a := AsList(G)[1];; b := AsList(G)[2];;
gap> mat := [[a, 0, b], [b, 0, 0], [0, a, b]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> x := MultiplicativeZero(S);;
gap> func := ChooseHashFunction(x, 25531).func;;
gap> data := ChooseHashFunction(x, 25531).data;
25531
gap> func(x, data);;
gap> func := ChooseHashFunction(x, 25531).func;;
gap> data := ChooseHashFunction(x, 25531).data;;
gap> func(x, data);;

# HashTables: Over a pc group
gap> G := SmallGroup(32, 2);;
gap> a := G.1;; b := G.2;; c := G.3;; d := G.4;; e := G.5;;
gap> mat := [[a, 0, c, b, 0, a, e],
> [b, 0, 0, e, a, b, a],
> [0, a, b, a, b, d, d],
> [a, b, c, d, e, 0, a],
> [e, a, 0, b, d, e, e],
> [a, b, c, e, 0, 0, 0],
> [e, a, 0, b, d, e, a]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> x := RMSElement(S, 1, a, 1);;
gap> ht := HTCreate(x);;
gap> for x in S do
> HTAdd(ht, x, true);
> od;

# ChooseHashFunction: Test for RZMS elements over a group we can't hash yet
gap> F := FreeGroup("a", "b");;
gap> G := F / [F.1 ^ 2, F.2 ^ 3, (F.1 * F.2) ^ 5];;
gap> a := AsList(G)[1];; b := AsList(G)[2];;
gap> mat := [[a, 0, b], [b, 0, 0], [0, a, b]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> x := MultiplicativeZero(S);;
gap> func := ChooseHashFunction(x, 25531).func;;

# RandomSemigroup
gap> RandomSemigroup(IsReesMatrixSemigroup);;
gap> RandomSemigroup(IsReesMatrixSemigroup, 2);;
gap> RandomSemigroup(IsReesMatrixSemigroup, 2, 2);;
gap> RandomSemigroup(IsReesMatrixSemigroup, 2, 2, Group(()));;
gap> RandomSemigroup(IsReesMatrixSemigroup, "a");
Error, the 2nd argument (number of rows) must be a positive integer
gap> RandomSemigroup(IsReesMatrixSemigroup, 2, "a");
Error, the 3rd argument (number of columns) must be a positive integer
gap> RandomSemigroup(IsReesMatrixSemigroup, 2, 2, "a");
Error, the 4th argument must be a permutation group
gap> RandomSemigroup(IsReesMatrixSemigroup, 2, 2, Group(()), 1);
Error, expected at most 3 arguments, found 4
gap> RandomSemigroup(IsReesZeroMatrixSemigroup);;
gap> RandomSemigroup(IsReesZeroMatrixSemigroup, 2);;
gap> RandomSemigroup(IsReesZeroMatrixSemigroup, 2, 2);;
gap> RandomSemigroup(IsReesZeroMatrixSemigroup, 2, 2, Group(()));;
gap> RandomSemigroup(IsReesZeroMatrixSemigroup, "a");
Error, the 2nd argument (number of rows) must be a positive integer
gap> RandomSemigroup(IsReesZeroMatrixSemigroup, 2, "a");
Error, the 3rd argument (number of columns) must be a positive integer
gap> RandomSemigroup(IsReesZeroMatrixSemigroup, 2, 2, "a");
Error, the 4th argument must be a permutation group
gap> RandomSemigroup(IsReesZeroMatrixSemigroup, 2, 2, Group(()), 1);
Error, expected at most 3 arguments, found 4
gap> RandomSemigroup(IsReesZeroMatrixSemigroup and IsRegularSemigroup);;
gap> RandomSemigroup(IsReesZeroMatrixSemigroup and IsRegularSemigroup, 2);;
gap> RandomSemigroup(IsReesZeroMatrixSemigroup and IsRegularSemigroup, 2, 2);;
gap> RandomSemigroup(IsReesZeroMatrixSemigroup and IsRegularSemigroup, 2, 2,
> Group(()));;
gap> RandomSemigroup(IsReesZeroMatrixSemigroup and IsRegularSemigroup, "a");
Error, the 2nd argument (number of rows) must be a positive integer
gap> RandomSemigroup(IsReesZeroMatrixSemigroup and IsRegularSemigroup, 2, "a");
Error, the 3rd argument (number of columns) must be a positive integer
gap> RandomSemigroup(IsReesZeroMatrixSemigroup and IsRegularSemigroup, 2, 2,
> "a");
Error, the 4th argument must be a permutation group
gap> RandomSemigroup(IsReesZeroMatrixSemigroup and IsRegularSemigroup, 2, 2,
> Group(()), 1);
Error, expected at most 3 arguments, found 4
gap> RandomSemigroup(IsReesZeroMatrixSemigroup and IsRegularSemigroup, 3, 2);;

# Test RMSElementNC
gap> R := ReesMatrixSemigroup(SymmetricGroup(4),
> [[(1, 3, 2), (), (1, 4, 2)],
>  [(), (1, 3)(2, 4), (1, 2, 3, 4)],
>  [(3, 4), (1, 3), (1, 2, 4, 3)],
>  [(), (2, 4, 3), (1, 2)]]);
<Rees matrix semigroup 3x4 over Sym( [ 1 .. 4 ] )>
gap> x := RMSElementNC(R, 1, (1, 2), 1);
(1,(1,2),1)
gap> x in R;
true

# semirms: MultiplicativeZero, for a Rees 0-matrix semigroup, 1
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(4),
> [[(1, 3, 2), (), (1, 4, 2)],
>  [(), (1, 3)(2, 4), (1, 2, 3, 4)],
>  [(3, 4), 0, (1, 2, 4, 3)],
>  [(), (2, 4, 3), (1, 2)]]);
<Rees 0-matrix semigroup 3x4 over Sym( [ 1 .. 4 ] )>
gap> R := Semigroup(R);;
gap> MultiplicativeZero(R);
0

# semirms: MultiplicativeZero, for a Rees 0-matrix subsemigroup, 1
gap> R := ReesZeroMatrixSemigroup(Group(()), [[()]]);;
gap> U := Semigroup(RMSElement(R, 1, (), 1));;
gap> MultiplicativeZero(U);
(1,(),1)
gap> IsTrivial(U);
true
gap> R := ReesZeroMatrixSemigroup(
>  Semigroup([Transformation([1, 1]), Transformation([2, 2])]),
>  [[Transformation([1, 1])]]);;
gap> U := Semigroup(RMSElement(R, 1, Transformation([1, 1]), 1));
<subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator>
gap> MultiplicativeZero(U);
(1,Transformation( [ 1, 1 ] ),1)

# semirms: MultiplicativeZero, for a Rees 0-matrix subsemigroup, 2
gap> G := SymmetricGroup(3);;
gap> R := ReesZeroMatrixSemigroup(G,
> [[(), 0, (1, 2)],
>  [(), (1, 3, 2), (1, 2)],
>  [(1, 3), 0, (1, 3, 2)]]);
<Rees 0-matrix semigroup 3x3 over Sym( [ 1 .. 3 ] )>
gap> U := ReesZeroMatrixSubsemigroup(R, [1], G, [1, 3]);
<subsemigroup of 3x3 Rees 0-matrix semigroup with 3 generators>
gap> MultiplicativeZero(U);
fail
gap> U := ReesZeroMatrixSubsemigroup(R, [1, 3], G, [1]);
<subsemigroup of 3x3 Rees 0-matrix semigroup with 3 generators>
gap> MultiplicativeZero(U);
fail
gap> U := ReesZeroMatrixSubsemigroup(R, [1, 3], G, [1, 3]);
<subsemigroup of 3x3 Rees 0-matrix semigroup with 4 generators>
gap> MultiplicativeZero(U);
fail
gap> U := ReesZeroMatrixSubsemigroup(R, [3], G, [3]);
<subsemigroup of 3x3 Rees 0-matrix semigroup with 2 generators>
gap> MultiplicativeZero(U);
fail
gap> U := ReesZeroMatrixSubsemigroup(R, [1], Group(()), [1]);
<subsemigroup of 3x3 Rees 0-matrix semigroup with 1 generator>
gap> MultiplicativeZero(U);
(1,(),1)

# Test IsomorphismPermGroup
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(4),
> [[(1, 3, 2), (), (1, 4, 2)],
>  [(), (1, 3)(2, 4), (1, 2, 3, 4)],
>  [(3, 4), 0, (1, 2, 4, 3)],
>  [(), (2, 4, 3), (1, 2)]]);
<Rees 0-matrix semigroup 3x4 over Sym( [ 1 .. 4 ] )>
gap> IsomorphismPermGroup(R);
Error, the argument (a semigroup) does not satisfy IsGroupAsSemigroup
gap> S := Semigroup(MultiplicativeZero(R));;
gap> IsomorphismPermGroup(S);
<subsemigroup of 3x4 Rees 0-matrix semigroup with 1 generator> -> Group(())
gap> map := IsomorphismPermGroup(S);
<subsemigroup of 3x4 Rees 0-matrix semigroup with 1 generator> -> Group(())
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> S := Semigroup(RMSElementNC(R, 1, (1, 3), 1));;
gap> map := IsomorphismPermGroup(S);;
gap> Range(map);
Group([ (1,2) ])
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# Test GroupOfUnits
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(4),
> [[(1, 3, 2), (), (1, 4, 2)],
>  [(), (1, 3)(2, 4), (1, 2, 3, 4)],
>  [(3, 4), 0, (1, 2, 4, 3)],
>  [(), (2, 4, 3), (1, 2)]]);;
gap> GroupOfUnits(R);
fail
gap> S := Semigroup(MultiplicativeZero(R));;
gap> GroupOfUnits(S);
<subsemigroup of 3x4 Rees 0-matrix semigroup with 1 generator>
gap> S := Semigroup(RMSElementNC(R, 1, (1, 3), 1));;
gap> GroupOfUnits(S);
<subsemigroup of 3x4 Rees 0-matrix semigroup with 2 generators>
gap> S := Semigroup(RMSElementNC(R, 2, (1, 3), 3));;
gap> GroupOfUnits(S);
fail

# Test Random
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(4),
> [[(1, 3, 2), (), (1, 4, 2)],
>  [(), (1, 3)(2, 4), (1, 2, 3, 4)],
>  [(3, 4), 0, (1, 2, 4, 3)],
>  [(), (2, 4, 3), (1, 2)]]);;
gap> Random(R) in R;
true

# Test ViewString for a Rees 0-matrix semigroup ideal
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(1), [[()]]);
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> R := Semigroup(GeneratorsOfSemigroup(R));
<subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators>
gap> IsTrivial(MinimalIdeal(R));
true
gap> MinimalIdeal(R);
<trivial group>
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(4),
> [[(1, 3, 2), (), (1, 4, 2)],
>  [(), (1, 3)(2, 4), (1, 2, 3, 4)],
>  [(3, 4), 0, (1, 2, 4, 3)],
>  [(), (2, 4, 3), (1, 2)]]);;
gap> I := SemigroupIdeal(R, MultiplicativeZero(R));
<regular Rees 0-matrix semigroup ideal with 1 generator>
gap> IsTrivial(I);
true
gap> I;;
gap> I := SemigroupIdeal(R, MultiplicativeZero(R));
<regular Rees 0-matrix semigroup ideal with 1 generator>
gap> IsCommutative(I) and IsSimpleSemigroup(I);
true
gap> I;;
gap> I := SemigroupIdeal(R, RMSElementNC(R, 2, (1, 3), 1));
<regular Rees 0-matrix semigroup ideal with 1 generator>
gap> IsCommutative(I);
false
gap> I;
<regular Rees 0-matrix semigroup ideal with 1 generator>
gap> IsZeroSimpleSemigroup(I);
true
gap> I;
<0-simple regular Rees 0-matrix semigroup ideal with 1 generator>
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(4),
> [[(), 0], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Sym( [ 1 .. 4 ] )>
gap> I := SemigroupIdeal(R, RMSElementNC(R, 2, (), 2));
<regular Rees 0-matrix semigroup ideal with 1 generator>
gap> IsInverseSemigroup(I);
true
gap> I;
<inverse Rees 0-matrix semigroup ideal with 1 generator>
gap> I := SemigroupIdeal(R, RMSElementNC(R, 2, (1, 3), 1),
>                           RMSElementNC(R, 2, (1, 3), 3));
<regular Rees 0-matrix semigroup ideal with 2 generators>
gap> I := SemigroupIdeal(R, RMSElementNC(R, 2, (1, 3), 3));
<regular Rees 0-matrix semigroup ideal with 1 generator>
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(4),
> [[0, 0], [0, 0]]);
<Rees 0-matrix semigroup 2x2 over Sym( [ 1 .. 4 ] )>
gap> I := SemigroupIdeal(R, RMSElementNC(R, 2, (1, 3), 2));
<Rees 0-matrix semigroup ideal with 1 generator>
gap> IsRegularSemigroup(I);
false
gap> I;
<commutative non-regular Rees 0-matrix semigroup ideal with 1 generator>
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(4),
> [[(), ()], [(), ()]]);
<Rees 0-matrix semigroup 2x2 over Sym( [ 1 .. 4 ] )>
gap> S := Semigroup(DClass(R, RMSElement(R, 1, (), 1)));
<subsemigroup of 2x2 Rees 0-matrix semigroup with 96 generators>
gap> IsSimpleSemigroup(S);
true
gap> I := SemigroupIdeal(S, S.1);
<regular Rees 0-matrix semigroup ideal with 1 generator>
gap> IsSimpleSemigroup(I);
true
gap> I;
<simple Rees 0-matrix semigroup ideal with 1 generator>

# Test MatrixEntries for an RMS
gap> R := ReesMatrixSemigroup(SymmetricGroup(4),
> [[(1, 3, 2), (), (1, 4, 2)],
>  [(), (1, 3)(2, 4), (1, 2, 3, 4)],
>  [(3, 4), (1, 3), (1, 2, 4, 3)],
>  [(), (2, 4, 3), (1, 2)]]);
<Rees matrix semigroup 3x4 over Sym( [ 1 .. 4 ] )>
gap> MatrixEntries(R);
[ (), (3,4), (2,4,3), (1,2), (1,2,3,4), (1,2,4,3), (1,3,2), (1,3), 
  (1,3)(2,4), (1,4,2) ]

# Test GreensHClassOfElement for RZMS
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(4),
> [[(1, 3, 2), (), (1, 4, 2)],
>  [(), (1, 3)(2, 4), (1, 2, 3, 4)],
>  [(3, 4), 0, (1, 2, 4, 3)],
>  [(), (2, 4, 3), (1, 2)]]);;
gap> H := GreensHClassOfElement(R, 1, 1);
<Green's H-class: (1,(),1)>
gap> IsGroupHClass(H);
true
gap> H := GreensHClassOfElement(R, 2, 3);
<Green's H-class: (2,(),3)>
gap> IsGroupHClass(H);
false
gap> H := GreensHClassOfElement(R, 3, 2);
<Green's H-class: (3,(),2)>
gap> IsGroupHClass(H);
true

# Test Idempotents 
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(4),
> [[(1, 3, 2), (), (1, 4, 2)],
>  [(), (1, 3)(2, 4), (1, 2, 3, 4)],
>  [(3, 4), 0, (1, 2, 4, 3)],
>  [(), (2, 4, 3), (1, 2)]]);;
gap> Idempotents(R);
[ 0, (1,(1,2,3),1), (1,(),2), (1,(3,4),3), (1,(),4), (2,(),1), 
  (2,(1,3)(2,4),2), (2,(2,3,4),4), (3,(1,2,4),1), (3,(1,4,3,2),2), 
  (3,(1,3,4,2),3), (3,(1,2),4) ]
gap> G := AsSemigroup(IsTransformationSemigroup, SymmetricGroup(4));
<transformation group of size 24, degree 4 with 2 generators>
gap> R := ReesZeroMatrixSemigroup(G, [[IdentityTransformation,
>                                      IdentityTransformation]]);
<Rees 0-matrix semigroup 2x1 over <transformation group of size 24, 
  degree 4 with 2 generators>>
gap> Idempotents(R);
[ 0, (1,IdentityTransformation,1), (2,IdentityTransformation,1) ]
gap> R := ReesZeroMatrixSemigroup(ZeroSemigroup(2),
> [[Transformation([1, 1, 2]), Transformation([1, 1, 2])]]);
<Rees 0-matrix semigroup 2x1 over <commutative non-regular transformation 
  semigroup of size 2, degree 3 with 1 generator>>
gap> Idempotents(R);
[ (1,Transformation( [ 1, 1, 1 ] ),1), (2,Transformation( [ 1, 1, 1 ] ),1), 0 
 ]

# IsIdempotentGenerated, for an RZMS
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(4),
> [[(1, 3, 2), (), (1, 4, 2)],
>  [(), (1, 3)(2, 4), (1, 2, 3, 4)],
>  [(3, 4), 0, (1, 2, 4, 3)],
>  [(), (2, 4, 3), (1, 2)]]);;
gap> IsIdempotentGenerated(R);
true
gap> R := PrincipalFactor(DClass(FullTransformationMonoid(5),
> Transformation([1, 1, 2, 3, 4])));;
gap> IsIdempotentGenerated(R);
true
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(4),
> [[(), 0], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Sym( [ 1 .. 4 ] )>
gap> IsIdempotentGenerated(R);
false

# ReesZeroMatrixSubsemigroup is not a ReesZeroMatrixSemigroup
gap> R := ReesZeroMatrixSemigroup(Group(()), [[(), 0], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Group(())>
gap> S := Semigroup(Idempotents(R));
<subsemigroup of 2x2 Rees 0-matrix semigroup with 3 generators>
gap> IsIdempotentGenerated(S);
true

# UnderlyingSemigroup is not IsGroup
gap> R := ReesZeroMatrixSemigroup(Semigroup(Transformation([2, 1])),
> [[Transformation([2, 1])]]);;
gap> IsIdempotentGenerated(R);
false
gap> R := ReesZeroMatrixSemigroup(Semigroup(Transformation([2, 1])),
> [[IdentityTransformation, IdentityTransformation],
>  [IdentityTransformation, Transformation([2, 1])]]);;
gap> IsIdempotentGenerated(R);
true

# UnderlyingSemigroup is not IsGroupAsSemigroup
gap> mat := [[
>  Transformation([2, 3, 1]),
>  Transformation([2, 1]),
>  Transformation([1, 2, 1])]];;
gap> R := ReesZeroMatrixSemigroup(FullTransformationMonoid(3), mat);
<Rees 0-matrix semigroup 3x1 over <full transformation monoid of degree 3>>
gap> IsIdempotentGenerated(R);
false
gap> R = Semigroup(Idempotents(R));
false
gap> S := Monoid([
>  Transformation([1, 2, 3, 1]), Transformation([2, 1, 3, 1]),
>  Transformation([2, 3, 3, 1]), Transformation([3, 2, 3, 1]),
>  Transformation([3, 3, 1, 2]), Transformation([3, 4, 1, 1]),
>  Transformation([4, 1, 2, 2]), Transformation([4, 2, 3, 3])]);;
gap> R := ReesZeroMatrixSemigroup(S, [[IdentityTransformation]]);;
gap> IsIdempotentGenerated(R);
true

# IsIdempotentGenerated, for an RMS
gap> R := ReesMatrixSemigroup(SymmetricGroup(4),
> [[(1, 3, 2), (), (1, 4, 2)],
>  [(), (1, 3)(2, 4), (1, 2, 3, 4)],
>  [(3, 4), (), (1, 2, 4, 3)],
>  [(), (2, 4, 3), (1, 2)]]);;
gap> IsIdempotentGenerated(R);
true

# ReesMatrixSubsemigroup is not a ReesMatrixSemigroup
gap> R := ReesMatrixSemigroup(SymmetricInverseMonoid(1),
> [[PartialPerm([]), PartialPerm([])]]);
<Rees matrix semigroup 2x1 over <symmetric inverse monoid of degree 1>>
gap> S := Semigroup(RMSElement(R, 1, PartialPerm([1]), 1),
>                   RMSElement(R, 2, PartialPerm([]), 1));
<subsemigroup of 2x1 Rees matrix semigroup with 2 generators>
gap> IsIdempotentGenerated(S);
false

# UnderlyingSemigroup is not IsGroup
gap> R := ReesMatrixSemigroup(Semigroup(Transformation([2, 1])),
> [[Transformation([2, 1])]]);
<Rees matrix semigroup 1x1 over <commutative transformation semigroup of 
  degree 2 with 1 generator>>
gap> IsIdempotentGenerated(R);
false
gap> R := ReesMatrixSemigroup(Semigroup(Transformation([2, 1])),
> [[IdentityTransformation, IdentityTransformation],
>  [IdentityTransformation, Transformation([2, 1])]]);
<Rees matrix semigroup 2x2 over <commutative transformation semigroup of 
  degree 2 with 1 generator>>
gap> IsIdempotentGenerated(R);
true

# UnderlyingSemigroup is not IsGroupAsSemigroup
gap> mat := [[
>  Transformation([2, 3, 1]),
>  Transformation([2, 1]),
>  Transformation([1, 2, 1])]];;
gap> R := ReesMatrixSemigroup(FullTransformationMonoid(3), mat);
<Rees matrix semigroup 3x1 over <full transformation monoid of degree 3>>
gap> IsIdempotentGenerated(R);
false
gap> R = Semigroup(Idempotents(R));
false
gap> S := Monoid([
>  Transformation([1, 2, 3, 1]), Transformation([2, 1, 3, 1]),
>  Transformation([2, 3, 3, 1]), Transformation([3, 2, 3, 1]),
>  Transformation([3, 3, 1, 2]), Transformation([3, 4, 1, 1]),
>  Transformation([4, 1, 2, 2]), Transformation([4, 2, 3, 3])]);;
gap> R := ReesMatrixSemigroup(S, [[IdentityTransformation]]);
<Rees matrix semigroup 1x1 over <transformation monoid of degree 4 with 8 
  generators>>
gap> IsIdempotentGenerated(R);
true

# Test Size for infinite RMS and RZMS
gap> S := FreeSemigroup(2);;
gap> R := ReesZeroMatrixSemigroup(S, [[S.1]]);
<Rees 0-matrix semigroup 1x1 over <free semigroup on the generators 
 [ s1, s2 ]>>
gap> Size(R);
infinity
gap> R := ReesMatrixSemigroup(S, [[S.1]]);
<Rees matrix semigroup 1x1 over <free semigroup on the generators [ s1, s2 ]>>
gap> Size(R);
infinity

# Representative
gap> R := ReesMatrixSemigroup(Group((1, 2)), [[(), (1, 2)], [(1, 2), ()]]);;
gap> Representative(R);
(1,(),1)

# Pickling
gap> filename := Filename(DirectoryTemporary(), "rms.p");;
gap> R := ReesMatrixSemigroup(Group((1, 2)), [[(), (1, 2)], [(1, 2), ()]]);
<Rees matrix semigroup 2x2 over Group([ (1,2) ])>
gap> WriteGenerators(filename, [R]);
IO_OK
gap> RR := ReadGenerators(filename)[1];
<Rees matrix semigroup 2x2 over Group([ (1,2) ])>
gap> Matrix(RR) = Matrix(R); 
true
gap> UnderlyingSemigroup(RR) = UnderlyingSemigroup(R); 
true
gap> R := ReesZeroMatrixSemigroup(Group((1, 2)), [[(), (1, 2)], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
gap> WriteGenerators(filename, [R]);
IO_OK
gap> RR := ReadGenerators(filename)[1];
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
gap> Matrix(RR) = Matrix(R); 
true
gap> UnderlyingSemigroup(RR) = UnderlyingSemigroup(R); 
true
gap> R := ReesMatrixSemigroup(Group((1, 2)), [[(), (1, 2)], [(1, 2), ()]]);
<Rees matrix semigroup 2x2 over Group([ (1,2) ])>
gap> IO_Pickle(IO_File(filename, "r"), R);
IO_Error
gap> R := ReesZeroMatrixSemigroup(Group((1, 2)), [[(), (1, 2)], [(1, 2), ()]]);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
gap> IO_Pickle(IO_File(filename, "r"), R);
IO_Error
gap> filename := "bananas";
"bananas"
gap> IO_Unpicklers.RMSX(filename);
IO_Error
gap> IO_Unpicklers.RZMS(filename);
IO_Error

# Idempotents
gap> mat := [[
>  Transformation([2, 3, 1]),
>  Transformation([2, 1]),
>  Transformation([1, 2, 1])]];;
gap> R := ReesZeroMatrixSemigroup(FullTransformationMonoid(3), mat);
<Rees 0-matrix semigroup 3x1 over <full transformation monoid of degree 3>>
gap> Idempotents(R); 
[ (1,Transformation( [ 1, 1, 1 ] ),1), (1,Transformation( [ 1, 1, 2 ] ),1), 
  (1,Transformation( [ 2, 1, 2 ] ),1), (1,Transformation( [ 2, 2, 2 ] ),1), 
  (1,Transformation( [ 3, 1, 1 ] ),1), (1,Transformation( [ 3, 1, 2 ] ),1), 
  (1,Transformation( [ 3, 1, 3 ] ),1), (1,Transformation( [ 3, 2, 2 ] ),1), 
  (1,Transformation( [ 3, 3, 2 ] ),1), (1,Transformation( [ 3, 3, 3 ] ),1), 
  (2,Transformation( [ 1, 1, 1 ] ),1), (2,Transformation( [ 1, 1 ] ),1), 
  (2,Transformation( [ 2, 1, 1 ] ),1), (2,Transformation( [ 2, 1, 2 ] ),1), 
  (2,Transformation( [ 2, 1 ] ),1), (2,Transformation( [ 2, 2, 2 ] ),1), 
  (2,Transformation( [ 2, 2 ] ),1), (2,Transformation( [ 2, 3, 3 ] ),1), 
  (2,Transformation( [ 3, 1, 3 ] ),1), (2,Transformation( [ 3, 3, 3 ] ),1), 
  (3,Transformation( [ 1, 1, 1 ] ),1), (3,Transformation( [ 1, 2, 1 ] ),1), 
  (3,Transformation( [ 1, 2, 2 ] ),1), (3,Transformation( [ 2, 2, 2 ] ),1), 
  (3,Transformation( [ 3, 2, 2 ] ),1), (3,Transformation( [ 3, 2, 3 ] ),1), 
  (3,Transformation( [ 3, 3, 3 ] ),1), 0 ]
gap> NrIdempotents(R) = Length(Idempotents(R));
true
gap> NrIdempotents(R);
28
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2)]),
>                                 [[(1, 2), (1, 2)], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
gap> Idempotents(S);
[ 0, (1,(1,2),1), (2,(1,2),1), (2,(),2) ]
gap> NrIdempotents(S) = Length(Idempotents(S));
true
gap> NrIdempotents(S);
4

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/semigroups/semirms.tst");
