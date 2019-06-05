#############################################################################
##
##  standard/semifp.tst
#Y  Copyright (C) 2015                                      Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semifp.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Test IsomorphismFpMonoid, 
gap> S := Monoid(Transformation([1, 3, 4, 1, 3]),
>                Transformation([2, 4, 1, 5, 5]),
>                Transformation([2, 5, 3, 5, 3]),
>                Transformation([4, 1, 2, 2, 1]),
>                Transformation([5, 5, 1, 1, 3]));;
gap> map := IsomorphismFpMonoid(S);
MappingByFunction( <transformation monoid of degree 5 with 5 generators>
 , <fp monoid on the generators [ m1, m2, m3, m4, m5 
 ]>, function( x ) ... end, function( x ) ... end )
gap> inv := InverseGeneralMapping(map);
MappingByFunction( <fp monoid on the generators [ m1, m2, m3, m4, m5 ]>, 
<transformation monoid of degree 5 with 5 generators>
 , function( x ) ... end, function( x ) ... end )
gap> ForAll(S, x -> (x ^ map) ^ inv = x);
true
gap> map := IsomorphismFpSemigroup(S);
MappingByFunction( <transformation monoid of degree 5 with 5 generators>
 , <fp semigroup on the generators [ s1, s2, s3, s4, s5, s6 
 ]>, function( x ) ... end, function( x ) ... end )
gap> inv := InverseGeneralMapping(map);
MappingByFunction( <fp semigroup on the generators [ s1, s2, s3, s4, s5, s6 
 ]>, <transformation monoid of degree 5 with 5 generators>
 , function( x ) ... end, function( x ) ... end )
gap> ForAll(S, x -> (x ^ map) ^ inv = x);
true

# Test IsomorphismFpMonoid, infinite
gap> IsomorphismFpMonoid(FreeMonoid(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `IsomorphismFpMonoid' on 1 arguments

# Test IsomorphismFpSemigroup, infinite
gap> IsomorphismFpSemigroup(FreeInverseSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `IsomorphismFpSemigroup' on 1 arguments

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

# AsFpSemigroup 1: trivial semigroup
gap> S := TrivialSemigroup();
<trivial transformation group of degree 0 with 1 generator>
gap> S := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1 ]>
gap> RelationsOfFpSemigroup(S);
[ [ s1^2, s1 ] ]

# AsFpSemigroup 2: 2 element semilattice
gap> S := Semigroup(PartialPerm([1]), PartialPerm([], []));
<partial perm monoid of rank 1 with 2 generators>
gap> S := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2 ]>
gap> RelationsOfFpSemigroup(S);
[ [ s1^2, s1 ], [ s1*s2, s2 ], [ s2*s1, s2 ], [ s2^2, s2 ] ]

# AsFpMonoid 1: trivial semigroup
gap> S := TrivialSemigroup();
<trivial transformation group of degree 0 with 1 generator>
gap> S := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [  ]>

# AsFpMonoid 2: 2 element semilattice
gap> S := Semigroup(PartialPerm([1]), PartialPerm([], []));
<partial perm monoid of rank 1 with 2 generators>
gap> S := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1 ]>
gap> RelationsOfFpMonoid(S);
[ [ m1^2, m1 ] ]

# AsSemigroup: 
#   convert from IsPBRSemigroup to IsFpSemigroup
gap> S := Semigroup([
> PBR([[-1], [-3], [-3]], [[1], [], [2, 3]]),
> PBR([[-2], [-3], [-3]], [[], [1], [2, 3]])]);
<pbr semigroup of degree 3 with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsFpSemigroup to IsFpSemigroup
gap> F := FreeSemigroup(2);; AssignGeneratorVariables(F);;
gap> rels := [[s1 ^ 2, s1], [s1 * s2, s2], [s2 ^ 2, s2 * s1]];;
gap> S := F / rels;
<fp semigroup on the generators [ s1, s2 ]>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBipartitionSemigroup to IsFpSemigroup
gap> S := Semigroup([
> Bipartition([[1, -1], [2], [-2]]),
> Bipartition([[1, -2], [2], [-1]])]);
<bipartition semigroup of degree 2 with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTransformationSemigroup to IsFpSemigroup
gap> S := Semigroup([
> Transformation([1, 3, 3]), Transformation([2, 3, 3])]);
<transformation semigroup of degree 3 with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBooleanMatSemigroup to IsFpSemigroup
gap> S := Semigroup([
> Matrix(IsBooleanMat,
>        [[true, false, false],
>       [false, false, true],
>       [false, false, true]]),
> Matrix(IsBooleanMat,
>        [[false, true, false],
> [false, false, true],
> [false, false, true]])]);
<semigroup of 3x3 boolean matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMaxPlusMatrixSemigroup to IsFpSemigroup
gap> S := Semigroup([
> Matrix(IsMaxPlusMatrix,
>        [[0, -infinity, -infinity],
> [-infinity, -infinity, 0],
> [-infinity, -infinity, 0]]),
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, 0, -infinity],
> [-infinity, -infinity, 0],
> [-infinity, -infinity, 0]])]);
<semigroup of 3x3 max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMinPlusMatrixSemigroup to IsFpSemigroup
gap> S := Semigroup([
> Matrix(IsMinPlusMatrix,
>        [[0, infinity, infinity],
> [infinity, infinity, 0],
> [infinity, infinity, 0]]),
> Matrix(IsMinPlusMatrix,
>        [[infinity, 0, infinity],
> [infinity, infinity, 0],
> [infinity, infinity, 0]])]);
<semigroup of 3x3 min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsFpSemigroup
gap> S := Semigroup([
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[0, -infinity, -infinity],
> [-infinity, -infinity, 0],
> [-infinity, -infinity, 0]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, 0, -infinity],
> [-infinity, -infinity, 0],
> [-infinity, -infinity, 0]])]);
<semigroup of 3x3 projective max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsIntegerMatrixSemigroup to IsFpSemigroup
gap> S := Semigroup([
> Matrix(IsIntegerMatrix,
>        [[1, 0, 0],
> [0, 0, 1],
> [0, 0, 1]]),
> Matrix(IsIntegerMatrix,
>        [[0, 1, 0],
> [0, 0, 1],
> [0, 0, 1]])]);
<semigroup of 3x3 integer matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsFpSemigroup
gap> S := Semigroup([
> Matrix(IsTropicalMaxPlusMatrix,
>        [[0, -infinity, -infinity],
> [-infinity, -infinity, 0],
> [-infinity, -infinity, 0]], 3),
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, 0, -infinity],
> [-infinity, -infinity, 0],
> [-infinity, -infinity, 0]], 3)]);
<semigroup of 3x3 tropical max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsFpSemigroup
gap> S := Semigroup([
> Matrix(IsTropicalMinPlusMatrix,
>        [[0, infinity, infinity],
> [infinity, infinity, 0],
> [infinity, infinity, 0]], 3),
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, 0, infinity],
> [infinity, infinity, 0],
> [infinity, infinity, 0]], 3)]);
<semigroup of 3x3 tropical min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsNTPMatrixSemigroup to IsFpSemigroup
gap> S := Semigroup([
> Matrix(IsNTPMatrix,
>        [[1, 0, 0],
> [0, 0, 1],
> [0, 0, 1]], 3, 4),
> Matrix(IsNTPMatrix,
>        [[0, 1, 0],
> [0, 0, 1],
> [0, 0, 1]], 3, 4)]);
<semigroup of 3x3 ntp matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPBRMonoid to IsFpSemigroup
gap> S := Monoid([
> PBR([[-2], [-2], [-3]], [[], [1, 2], [3]]),
> PBR([[-1], [-3], [-3]], [[1], [], [2, 3]])]);
<pbr monoid of degree 3 with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2, s3 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsFpMonoid to IsFpSemigroup
gap> F := FreeMonoid(2);; AssignGeneratorVariables(F);;
gap> rels := [[m1 ^ 2, m1], [m2 ^ 2, m2], [m1 * m2 * m1, m1 * m2],
> [m2 * m1 * m2, m1 * m2]];;
gap> S := F / rels;
<fp monoid on the generators [ m1, m2 ]>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ <identity ...>, m1, m2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBipartitionMonoid to IsFpSemigroup
gap> S := Monoid([
> Bipartition([[1, 2, -2], [3, -3], [-1]]),
> Bipartition([[1, -1], [2, 3, -3], [-2]])]);
<bipartition monoid of degree 3 with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2, s3 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTransformationMonoid to IsFpSemigroup
gap> S := Monoid([
> Transformation([2, 2]), Transformation([1, 3, 3])]);
<transformation monoid of degree 3 with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2, s3 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBooleanMatMonoid to IsFpSemigroup
gap> S := Monoid([
> Matrix(IsBooleanMat,
>        [[false, true, false],
> [false, true, false],
> [false, false, true]]),
> Matrix(IsBooleanMat,
>        [[true, false, false],
> [false, false, true],
> [false, false, true]])]);
<monoid of 3x3 boolean matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2, s3 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMaxPlusMatrixMonoid to IsFpSemigroup
gap> S := Monoid([
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, 0, -infinity],
> [-infinity, 0, -infinity],
> [-infinity, -infinity, 0]]),
> Matrix(IsMaxPlusMatrix,
>        [[0, -infinity, -infinity],
> [-infinity, -infinity, 0],
> [-infinity, -infinity, 0]])]);
<monoid of 3x3 max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2, s3 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsMinPlusMatrixMonoid to IsFpSemigroup
gap> S := Monoid([
> Matrix(IsMinPlusMatrix,
>        [[infinity, 0, infinity],
> [infinity, 0, infinity],
> [infinity, infinity, 0]]),
> Matrix(IsMinPlusMatrix,
>        [[0, infinity, infinity],
> [infinity, infinity, 0],
> [infinity, infinity, 0]])]);
<monoid of 3x3 min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2, s3 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixMonoid to IsFpSemigroup
gap> S := Monoid([
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, 0, -infinity],
> [-infinity, 0, -infinity],
> [-infinity, -infinity, 0]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[0, -infinity, -infinity],
> [-infinity, -infinity, 0],
> [-infinity, -infinity, 0]])]);
<monoid of 3x3 projective max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2, s3 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsIntegerMatrixMonoid to IsFpSemigroup
gap> S := Monoid([
> Matrix(IsIntegerMatrix,
>        [[0, 1, 0],
> [0, 1, 0],
> [0, 0, 1]]),
> Matrix(IsIntegerMatrix,
>        [[1, 0, 0],
> [0, 0, 1],
> [0, 0, 1]])]);
<monoid of 3x3 integer matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2, s3 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixMonoid to IsFpSemigroup
gap> S := Monoid([
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, 0, -infinity],
> [-infinity, 0, -infinity],
> [-infinity, -infinity, 0]], 4),
> Matrix(IsTropicalMaxPlusMatrix,
>        [[0, -infinity, -infinity],
> [-infinity, -infinity, 0],
> [-infinity, -infinity, 0]], 4)]);
<monoid of 3x3 tropical max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2, s3 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixMonoid to IsFpSemigroup
gap> S := Monoid([
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, 0, infinity],
> [infinity, 0, infinity],
> [infinity, infinity, 0]], 2),
> Matrix(IsTropicalMinPlusMatrix,
>        [[0, infinity, infinity],
> [infinity, infinity, 0],
> [infinity, infinity, 0]], 2)]);
<monoid of 3x3 tropical min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2, s3 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsNTPMatrixMonoid to IsFpSemigroup
gap> S := Monoid([
> Matrix(IsNTPMatrix,
>        [[0, 1, 0],
> [0, 1, 0],
> [0, 0, 1]], 1, 1),
> Matrix(IsNTPMatrix,
>        [[1, 0, 0],
> [0, 0, 1],
> [0, 0, 1]], 1, 1)]);
<monoid of 3x3 ntp matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2, s3 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsPBRSemigroup to IsFpMonoid
gap> S := Semigroup([
> PBR([[-1], [-2], [-2], [-2], [-2]], [[1], [2, 3, 4, 5], [], [], []]),
> PBR([[-2], [-1], [-1], [-1], [-1]], [[2, 3, 4, 5], [1], [], [], []])]);
<pbr semigroup of degree 5 with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsFpSemigroup to IsFpMonoid
gap> F := FreeSemigroup(2);; AssignGeneratorVariables(F);;
gap> rels := [[s1 ^ 2, s1], [s1 * s2, s2], [s2 * s1, s2], [s2 ^ 2, s1]];;
gap> S := F / rels;
<fp semigroup on the generators [ s1, s2 ]>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsBipartitionSemigroup to IsFpMonoid
gap> S := Semigroup([
> Bipartition([[1, -1], [2, 3, 4, 5, -2], [-3], [-4], [-5]]),
> Bipartition([[1, -2], [2, 3, 4, 5, -1], [-3], [-4], [-5]])]);
<bipartition semigroup of degree 5 with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTransformationSemigroup to IsFpMonoid
gap> S := Semigroup([
> Transformation([1, 2, 2, 2, 2]), Transformation([2, 1, 1, 1, 1])]);
<transformation semigroup of degree 5 with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsBooleanMatSemigroup to IsFpMonoid
gap> S := Semigroup([
> Matrix(IsBooleanMat,
>        [[true, false, false, false, false],
>       [false, true, false, false, false],
>         [false, true, false, false, false],
>         [false, true, false, false, false],
>         [false, true, false, false, false]]),
> Matrix(IsBooleanMat,
>        [[false, true, false, false, false],
>         [true, false, false, false, false],
>         [true, false, false, false, false],
>         [true, false, false, false, false],
>         [true, false, false, false, false]])]);
<semigroup of 5x5 boolean matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsMaxPlusMatrixSemigroup to IsFpMonoid
gap> S := Semigroup([
> Matrix(IsMaxPlusMatrix,
>        [[0, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity]]),
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, 0, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity]])]);
<semigroup of 5x5 max-plus matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsMinPlusMatrixSemigroup to IsFpMonoid
gap> S := Semigroup([
> Matrix(IsMinPlusMatrix,
>        [[0, infinity, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity]]),
> Matrix(IsMinPlusMatrix,
>        [[infinity, 0, infinity, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity]])]);
<semigroup of 5x5 min-plus matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsFpMonoid
gap> S := Semigroup([
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[0, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, 0, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity]])]);
<semigroup of 5x5 projective max-plus matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsIntegerMatrixSemigroup to IsFpMonoid
gap> S := Semigroup([
> Matrix(IsIntegerMatrix,
>        [[1, 0, 0, 0, 0],
>         [0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0]]),
> Matrix(IsIntegerMatrix,
>        [[0, 1, 0, 0, 0],
>         [1, 0, 0, 0, 0],
>         [1, 0, 0, 0, 0],
>         [1, 0, 0, 0, 0],
>         [1, 0, 0, 0, 0]])]);
<semigroup of 5x5 integer matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsFpMonoid
gap> S := Semigroup([
> Matrix(IsTropicalMaxPlusMatrix,
>        [[0, -infinity, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity],
>         [-infinity, 0, -infinity, -infinity, -infinity]], 3),
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, 0, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity],
>         [0, -infinity, -infinity, -infinity, -infinity]], 3)]);
<semigroup of 5x5 tropical max-plus matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsFpMonoid
gap> S := Semigroup([
> Matrix(IsTropicalMinPlusMatrix,
>        [[0, infinity, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity],
>         [infinity, 0, infinity, infinity, infinity]], 5),
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, 0, infinity, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity],
>         [0, infinity, infinity, infinity, infinity]], 5)]);
<semigroup of 5x5 tropical min-plus matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsNTPMatrixSemigroup to IsFpMonoid
gap> S := Semigroup([
> Matrix(IsNTPMatrix,
>        [[1, 0, 0, 0, 0],
>         [0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0]], 5, 1),
> Matrix(IsNTPMatrix,
>        [[0, 1, 0, 0, 0],
>         [1, 0, 0, 0, 0],
>         [1, 0, 0, 0, 0],
>         [1, 0, 0, 0, 0],
>         [1, 0, 0, 0, 0]], 5, 1)]);
<semigroup of 5x5 ntp matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsPBRMonoid to IsFpMonoid
gap> S := Monoid([
> PBR([[-3], [-2], [-3]], [[], [2], [1, 3]]),
> PBR([[-2], [-2], [-1]], [[3], [1, 2], []])]);
<pbr monoid of degree 3 with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1, m2 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsFpMonoid to IsFpMonoid
gap> F := FreeMonoid(2);; AssignGeneratorVariables(F);;
gap> rels := [[m1 ^ 2, m1], [m1 * m2 * m1, m1], [m1 * m2 ^ 2, m2 ^ 2],
> [m2 * m1 * m2, m2], [m2 ^ 2 * m1, m2 ^ 2], [m2 ^ 3, m2 ^ 2]];;
gap> S := F / rels;
<fp monoid on the generators [ m1, m2 ]>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1, m2 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsBipartitionMonoid to IsFpMonoid
gap> S := Monoid([
> Bipartition([[1, 3, -3], [2, -2], [-1]]),
> Bipartition([[1, 2, -2], [3, -1], [-3]])]);
<bipartition monoid of degree 3 with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1, m2 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTransformationMonoid to IsFpMonoid
gap> S := Monoid([
> Transformation([3, 2, 3]), Transformation([2, 2, 1])]);
<transformation monoid of degree 3 with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1, m2 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsBooleanMatMonoid to IsFpMonoid
gap> S := Monoid([
> Matrix(IsBooleanMat,
>        [[false, false, true],
>         [false, true, false],
>         [false, false, true]]),
> Matrix(IsBooleanMat,
>        [[false, true, false],
>         [false, true, false],
>         [true, false, false]])]);
<monoid of 3x3 boolean matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1, m2 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsMaxPlusMatrixMonoid to IsFpMonoid
gap> S := Monoid([
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, -infinity, 0],
>         [-infinity, 0, -infinity],
>         [-infinity, -infinity, 0]]),
> Matrix(IsMaxPlusMatrix,
>        [[-infinity, 0, -infinity],
>         [-infinity, 0, -infinity],
>         [0, -infinity, -infinity]])]);
<monoid of 3x3 max-plus matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1, m2 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsMinPlusMatrixMonoid to IsFpMonoid
gap> S := Monoid([
> Matrix(IsMinPlusMatrix,
>        [[infinity, infinity, 0],
>         [infinity, 0, infinity],
>         [infinity, infinity, 0]]),
> Matrix(IsMinPlusMatrix,
>        [[infinity, 0, infinity],
>         [infinity, 0, infinity],
>         [0, infinity, infinity]])]);
<monoid of 3x3 min-plus matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1, m2 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsProjectiveMaxPlusMatrixMonoid to IsFpMonoid
gap> S := Monoid([
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, -infinity, 0],
>         [-infinity, 0, -infinity],
>         [-infinity, -infinity, 0]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>        [[-infinity, 0, -infinity],
>         [-infinity, 0, -infinity],
>         [0, -infinity, -infinity]])]);
<monoid of 3x3 projective max-plus matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1, m2 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsIntegerMatrixMonoid to IsFpMonoid
gap> S := Monoid([
> Matrix(IsIntegerMatrix,
>        [[0, 0, 1],
>         [0, 1, 0],
>         [0, 0, 1]]),
> Matrix(IsIntegerMatrix,
>        [[0, 1, 0],
>         [0, 1, 0],
>         [1, 0, 0]])]);
<monoid of 3x3 integer matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1, m2 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTropicalMaxPlusMatrixMonoid to IsFpMonoid
gap> S := Monoid([
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, -infinity, 0],
>         [-infinity, 0, -infinity],
>         [-infinity, -infinity, 0]], 2),
> Matrix(IsTropicalMaxPlusMatrix,
>        [[-infinity, 0, -infinity],
>         [-infinity, 0, -infinity],
>         [0, -infinity, -infinity]], 2)]);
<monoid of 3x3 tropical max-plus matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1, m2 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsTropicalMinPlusMatrixMonoid to IsFpMonoid
gap> S := Monoid([
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, infinity, 0],
>         [infinity, 0, infinity],
>         [infinity, infinity, 0]],
>        2),
> Matrix(IsTropicalMinPlusMatrix,
>        [[infinity, 0, infinity],
>         [infinity, 0, infinity],
>         [0, infinity, infinity]],
>        2)]);
<monoid of 3x3 tropical min-plus matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1, m2 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid: 
#   convert from IsNTPMatrixMonoid to IsFpMonoid
gap> S := Monoid([
> Matrix(IsNTPMatrix,
>        [[0, 0, 1],
>         [0, 1, 0],
>         [0, 0, 1]], 1, 4),
> Matrix(IsNTPMatrix,
>        [[0, 1, 0],
>         [0, 1, 0],
>         [1, 0, 0]], 1, 4)]);
<monoid of 3x3 ntp matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1, m2 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsReesMatrixSemigroup to IsFpSemigroup
gap> R := ReesMatrixSemigroup(Group([(1, 2)]), [[(1, 2), (1, 2)],
>         [(), ()]]);
<Rees matrix semigroup 2x2 over Group([ (1,2) ])>
gap> T := AsSemigroup(IsFpSemigroup, R);
<fp semigroup on the generators [ s1, s2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid
#   convert from IsReesMatrixSemigroup to IsFpMonoid
gap> R := ReesMatrixSemigroup(Group([(1, 2)]), [[(1, 2)]]);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
gap> T := AsMonoid(IsFpMonoid, R);
<fp monoid on the generators [ m1 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsReesZeroMatrixSemigroup to IsFpSemigroup
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2)]),
>                                 [[(1, 2), (1, 2)], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
gap> T := AsSemigroup(IsFpSemigroup, R);
<fp semigroup on the generators [ s1, s2, s3 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsMonoid
#   convert from IsReesZeroMatrixSemigroup to IsFpMonoid
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[(1, 2)]]);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
gap> T := AsMonoid(IsFpMonoid, R);
<fp monoid on the generators [ m1, m2 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from graph inverse to IsFpSemigroup
gap> S := GraphInverseSemigroup(Digraph([[2], []]));
<finite graph inverse semigroup with 2 vertices, 1 edge>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2, s3, s4 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from perm group to IsFpSemigroup
gap> S := DihedralGroup(IsPermGroup, 6);
Group([ (1,2,3), (2,3) ])
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ <identity ...>, F1^-1, F1, F2^-1, F2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from perm group to IsFpMonoid
gap> S := DihedralGroup(IsPermGroup, 6);
Group([ (1,2,3), (2,3) ])
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ F1, F1^-1, F2, F2^-1 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from non-perm group to IsFpSemigroup
gap> S := DihedralGroup(6);
<pc group of size 6 with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ <identity ...>, F1^-1, F1, F2^-1, F2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from non-perm group to IsFpMonoid
gap> S := DihedralGroup(6);
<pc group of size 6 with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ F1, F1^-1, F2, F2^-1 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBlockBijectionSemigroup to IsFpSemigroup
gap> S := InverseSemigroup(Bipartition([[1, -1, -3], [2, 3, -2]]));;
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBlockBijectionMonoid to IsFpMonoid
gap> S := InverseMonoid([
> Bipartition([[1, -1, -3], [2, 3, -2]])]);;
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1, m2 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsBlockBijectionMonoid to IsFpSemigroup
gap> S := InverseMonoid([
> Bipartition([[1, -1, -3], [2, 3, -2]])]);;
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2, s3 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPartialPermSemigroup to IsFpSemigroup
gap> S := InverseSemigroup(PartialPerm([1, 2], [2, 1]),
>                          PartialPerm([1, 2], [3, 1]));
<inverse partial perm semigroup of rank 3 with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2, s3 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPartialPermMonoid to IsFpMonoid
gap> S := InverseMonoid(PartialPerm([1, 2], [2, 1]),
>                       PartialPerm([1, 2], [3, 1]));
<inverse partial perm monoid of rank 3 with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1, m2, m3 ]>
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
gap> map := IsomorphismMonoid(IsFpMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AsSemigroup: 
#   convert from IsPartialPermMonoid to IsFpSemigroup
gap> S := InverseMonoid(PartialPerm([1, 2], [2, 1]),
>                       PartialPerm([1, 2], [3, 1]));
<inverse partial perm monoid of rank 3 with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2, s3, s4 ]>
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
gap> map := IsomorphismSemigroup(IsFpSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# finite group to fp semigroup
gap> G := AlternatingGroup(5);;
gap> map := IsomorphismSemigroup(IsFpSemigroup, G);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# finite group to fp monoid
gap> G := AlternatingGroup(5);;
gap> map := IsomorphismMonoid(IsFpMonoid, G);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#
gap> S := Semigroup([
> Transformation([2, 4, 3, 4]),
> Transformation([3, 3, 2, 3]),
> Transformation([3, 4, 4, 1])]);;
gap> F := AsSemigroup(IsFpSemigroup, S);;
gap> x := F.1 * F.2 * F.3; y := F.2 ^ 2 * F.3;
s1*s2*s3
s2^2*s3
gap> x = y;
true
gap> x := F.2 ^ 2 * F.1 * F.2 ^ 2 * F.3; y := F.1 ^ 2 * F.2 * F.3 ^ 4;
s2^2*s1*s2^2*s3
s1^2*s2*s3^4
gap> x = y;
true
gap> x := F.1 * F.1; y := F.2 * F.3;
s1^2
s2*s3
gap> x = y;
false

# Test IsomorphismFpMonoid for semigroup with duplicate generators
gap> S := Semigroup(Matrix(IsBooleanMat,
>        [[0, 1],
>         [1, 0]]),
> Matrix(IsBooleanMat,
>        [[1, 0],
>         [1, 1]]),
> Matrix(IsBooleanMat,
>        [[1, 0],
>         [0, 0]]),
> Matrix(IsBooleanMat,
>        [[1, 0],
>         [0, 0]]));;
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1, m2, m3, m4 ]>
gap> RelationsOfFpMonoid(T);
[ [ m1^2, <identity ...> ], [ m4, m3 ], [ m1*m4, m1*m3 ], [ m2^2, m2 ], 
  [ m2*m4, m2*m3 ], [ m3*m2, m3 ], [ m3^2, m3 ], [ m3*m4, m3 ], 
  [ m1*m2*m3, m2*m3 ], [ m2*m1^2, m2 ], [ m2*m1*m3, m1*m3 ], [ m3*m1^2, m3 ], 
  [ (m1*m2)^2, m2*m1*m2 ], [ (m1*m3)^2, m3*m1*m3 ], [ (m2*m1)^2, m2*m1*m2 ], 
  [ m2*m3*m1*m2, m2*m1*m2 ], [ m2*m3*m1*m3, m3*m1*m3 ], 
  [ m3*m1*m2*m1, m3*m1*m2 ], [ (m3*m1)^2, m3*m1*m3 ] ]
gap> Size(T);
16
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2, s3, s4 ]>
gap> RelationsOfFpSemigroup(T);
[ [ s4, s3 ], [ s1*s4, s1*s3 ], [ s2^2, s2 ], [ s2*s4, s2*s3 ], 
  [ s3*s2, s3 ], [ s3^2, s3 ], [ s3*s4, s3 ], [ s1^3, s1 ], [ s1^2*s2, s2 ], 
  [ s1^2*s3, s3 ], [ s1*s2*s3, s2*s3 ], [ s2*s1^2, s2 ], [ s2*s1*s3, s1*s3 ], 
  [ s3*s1^2, s3 ], [ (s1*s2)^2, s2*s1*s2 ], [ (s1*s3)^2, s3*s1*s3 ], 
  [ (s2*s1)^2, s2*s1*s2 ], [ s2*s3*s1*s2, s2*s1*s2 ], 
  [ s2*s3*s1*s3, s3*s1*s3 ], [ s3*s1*s2*s1, s3*s1*s2 ], 
  [ (s3*s1)^2, s3*s1*s3 ] ]
gap> Size(T);
16

# Test IsomorphismFpSemigroup for semigroup with multiple duplicate generators
gap> S := Semigroup([
> Matrix(IsBooleanMat,
>        [[0, 1],
>         [1, 0]]),
> Matrix(IsBooleanMat,
>        [[1, 0],
>         [1, 1]]),
> Matrix(IsBooleanMat,
>        [[1, 0],
>         [1, 1]]),
> Matrix(IsBooleanMat,
>        [[1, 0],
>         [0, 0]]),
> Matrix(IsBooleanMat,
>        [[1, 0],
>         [0, 0]]),
> Matrix(IsBooleanMat,
>        [[0, 1],
>         [1, 0]]),
> Matrix(IsBooleanMat,
>        [[1, 0],
>         [0, 0]]),
> Matrix(IsBooleanMat,
>        [[1, 0],
>         [1, 1]]),
> Matrix(IsBooleanMat,
>        [[1, 0],
>         [0, 0]]),
> Matrix(IsBooleanMat,
>        [[1, 0],
>         [0, 0]]),
> Matrix(IsBooleanMat,
>        [[0, 1],
>         [1, 0]])]);;
gap> T := AsSemigroup(IsFpSemigroup, S);;
gap> Size(T) = Size(S);
true

# Test ExtRepOfObj for an element of an fp monoid
gap> S := RegularBooleanMatMonoid(2);;
gap> T := AsMonoid(IsFpMonoid, S);;
gap> ExtRepOfObj(T.1);
[ 1, 1 ]

# Test RandomSemigroup
gap> RandomSemigroup(IsFpSemigroup);;
gap> RandomSemigroup(IsFpSemigroup, 3);;
gap> RandomSemigroup(IsFpSemigroup, 5, 5);;
gap> RandomSemigroup(IsFpSemigroup, "a");
Error, Semigroups: SEMIGROUPS_ProcessRandomArgsCons: usage,
the parameter must be pos ints,

# Test RandomMonoid
gap> RandomMonoid(IsFpMonoid);;
gap> RandomMonoid(IsFpMonoid, 3);;
gap> RandomMonoid(IsFpMonoid, 5, 5);;
gap> RandomMonoid(IsFpMonoid, "a");
Error, Semigroups: SEMIGROUPS_ProcessRandomArgsCons: usage,
the parameter must be pos ints,

# Test RandomInverseSemigroup
gap> RandomInverseSemigroup(IsFpSemigroup);;
gap> RandomInverseSemigroup(IsFpSemigroup, 3);;
gap> RandomInverseSemigroup(IsFpSemigroup, 5, 5);;
gap> RandomInverseSemigroup(IsFpSemigroup, "a");
Error, Semigroups: SEMIGROUPS_ProcessRandomArgsCons: usage,
the parameter must be pos ints,

# Test RandomInverseMonoid
gap> RandomInverseMonoid(IsFpMonoid);;
gap> RandomInverseMonoid(IsFpMonoid, 3);;
gap> RandomInverseMonoid(IsFpMonoid, 5, 5);;
gap> RandomInverseMonoid(IsFpMonoid, "a");
Error, Semigroups: SEMIGROUPS_ProcessRandomArgsCons: usage,
the parameter must be pos ints,

# Test AsMonoid
gap> S := SingularTransformationMonoid(3);
<regular transformation semigroup ideal of degree 3 with 1 generator>
gap> GeneratorsOfSemigroup(S);;
gap> S := AsSemigroup(IsFpSemigroup, S);;
gap> AsMonoid(S);
fail
gap> S := Semigroup(Transformation([1, 2, 3, 3, 3]));
<commutative transformation semigroup of degree 5 with 1 generator>
gap> S := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1 ]>
gap> AsMonoid(S);
<fp monoid on the generators [  ]>

# Test IsomorphismFpMonoid, fail
gap> S := SingularTransformationMonoid(3);
<regular transformation semigroup ideal of degree 3 with 1 generator>
gap> IsomorphismFpMonoid(S);
Error, Semigroups: IsomorphismFpMonoid: usage,
the first argument (a semigroup) must satisfy `IsMonoidAsSemigroup`,

# Test MultiplicativeZero
gap> F := FreeSemigroup(2);;
gap> S := F / [[F.1, F.2 * F.1], [F.1, F.1 * F.2], [F.1 ^ 2, F.1]];;
gap> MultiplicativeZero(S) = S.1;
true
gap> F := FreeMonoid(2);;
gap> S := F / [[F.1, F.2 * F.1], [F.1, F.1 * F.2], [F.1 ^ 2, F.1]];;
gap> MultiplicativeZero(S) = S.1;
true

# Test SEMIGROUPS.WordToExtRepWord
gap> w := [1 .. 100] * 0 + 1;
[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
gap> SEMIGROUPS.WordToExtRepObj(w);
[ 1, 100 ]
gap> w := Concatenation(w, [1 .. 50] * 0 + 2);
[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ]
gap> SEMIGROUPS.WordToExtRepObj(w);
[ 1, 100, 2, 50 ]
gap> w := Concatenation(w, [1] * 3);
[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
  3 ]
gap> SEMIGROUPS.WordToExtRepObj(w);
[ 1, 100, 2, 50, 3, 1 ]
gap> w := Concatenation(w, [1 .. 4] * 0 + 1);
[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
  3, 1, 1, 1, 1 ]
gap> SEMIGROUPS.WordToExtRepObj(w);
[ 1, 100, 2, 50, 3, 1, 1, 4 ]

# Test SEMIGROUPS.WordToString
gap> w := [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 1,
> 1, 1, 1, 1, 1, 1, 1];;
gap> SEMIGROUPS.WordToString(w);
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\
aaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbcaaaa\
aaaa"
gap> SEMIGROUPS.WordToString([100]);
Error, SEMIGROUPS.WordToString: the maximum value in the argument be at most 
52,

# Test SEMIGROUPS.ExtRepObjToString
gap> SEMIGROUPS.ExtRepObjToString(SEMIGROUPS.WordToExtRepObj(w));
"a ^ 100b ^ 50ca ^ 8"
gap> SEMIGROUPS.ExtRepObjToString([100, 1]);
Error, SEMIGROUPS.ExtRepObjToString: the maximum value in an odd position of t\
he argument must be at most 52,

# Test IsomorphismFpSemigroup (for factorizable inverse monoids)
gap> S := SymmetricInverseMonoid(4);;
gap> iso := IsomorphismFpSemigroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> S := InverseSemigroup(
> [PartialPerm([1, 2, 3, 4, 5, 6, 7, 8], [2, 4, 8, 6, 3, 1, 5, 7]),
> PartialPerm([1, 2, 3, 4, 5, 6, 7, 8], [3, 5, 4, 7, 6, 8, 1, 2]),
> PartialPerm([1, 2, 3, 4, 5, 6, 7, 8], [4, 6, 7, 1, 8, 2, 3, 5]),
> PartialPerm([], [])]);;
gap> iso := IsomorphismFpSemigroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> S := InverseSemigroup(
> [PartialPerm([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], [9, 10, 8, 2, 1, 7, 5, 4, 6, 3]),
> PartialPerm([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], [10, 9, 3, 6, 5, 2, 8, 1, 4, 7]),
> PartialPerm([], [])]);;
gap> iso := IsomorphismFpSemigroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> tst := [InverseMonoid([PartialPerm([1, 2, 3, 4, 5], [4, 5, 2, 3, 1]),
> PartialPerm([1, 3], [1, 3])]), 
> InverseMonoid([PartialPerm([1, 2, 3, 4], [1, 2, 3, 4]), 
> PartialPerm([1, 2, 3, 4, 5], [3, 1, 5, 4, 2])]),
> InverseMonoid([PartialPerm([1, 2, 3, 4, 5], [5, 4, 2, 3, 1]),
> PartialPerm([1, 2, 4], [1, 2, 4])]),
> InverseMonoid([PartialPerm([1, 2, 5], [2, 1, 5]),
> PartialPerm([1, 2], [1, 2])]),
> InverseMonoid([PartialPerm([1, 2, 3], [1, 4, 5]),
> PartialPerm([1, 2, 3, 4, 5], [1, 5, 4, 2, 3])]),
> InverseMonoid([PartialPerm([1, 2, 3, 4, 5], [3, 1, 5, 4, 2]),
> PartialPerm([1, 2, 3, 4, 5], [5, 1, 3, 4, 2])]),
> InverseMonoid([PartialPerm([1, 2, 5], [2, 3, 5]),
> PartialPerm([1, 2, 3, 5], [2, 3, 1, 5])]),
> InverseMonoid([PartialPerm([1, 2, 3, 4, 5], [4, 2, 3, 1, 5]),
> PartialPerm([1, 2, 3, 4, 5], [5, 3, 2, 1, 4])]),
> InverseMonoid([PartialPerm([1, 2, 3, 5], [2, 1, 3, 5]),
> PartialPerm([1, 2, 3, 5], [5, 2, 1, 3])]),
> InverseMonoid([PartialPerm([1, 2, 3], [5, 4, 1]),
> PartialPerm([1, 2, 3, 4, 5], [2, 3, 5, 1, 4])]),
> InverseMonoid([PartialPerm([1, 2, 3, 4, 5], [4, 3, 5, 2, 1]),
> PartialPerm([1, 2, 4, 5], [5, 4, 2, 1]),
> PartialPerm([1, 4], [3, 2]), PartialPerm([1, 2, 3, 4, 5], [2, 3, 5, 1, 4]),
> PartialPerm([1, 2, 5], [2, 3, 4])]),
> InverseMonoid([PartialPerm([1, 2, 3, 4, 5], [2, 4, 1, 5, 3]),
> PartialPerm([1, 3, 4], [2, 1, 3]),
> PartialPerm([1, 2, 3, 4, 5], [4, 1, 2, 5, 3]), PartialPerm([1, 3], [5, 4]),
> PartialPerm([1, 3, 5], [2, 4, 1])])];;
gap> ForAll(tst, IsFactorisableInverseMonoid);
true
gap> ForAll(tst, S -> BruteForceIsoCheck(IsomorphismFpSemigroup(S)));
true
gap> ForAll(tst{[1 .. 10]}, S -> BruteForceInverseCheck(IsomorphismFpSemigroup(S)));
true

# Test EvaluateExtRepObjWord
gap> F := FreeSemigroup(4);;
gap> x := EvaluateExtRepObjWord(Generators(F), [1, 4, 2, 5, 3, 1, 2, 1]);
s1^4*s2^5*s3*s2
gap> ExtRepOfObj(x) = [1, 4, 2, 5, 3, 1, 2, 1];
true
gap> EvaluateExtRepObjWord(Generators(F), []);
Error, Semigroups: EvaluateExtRepObjWord, the second argument must be a non-em\
pty list
gap> F := FreeMonoid(4);;
gap> x := EvaluateExtRepObjWord(Generators(F), [1, 4, 2, 5, 3, 1, 2, 1]);
m1^4*m2^5*m3*m2
gap> ExtRepOfObj(x) = [1, 4, 2, 5, 3, 1, 2, 1];
true
gap> EvaluateExtRepObjWord(Generators(F), []);
<identity ...>

# SEMIGROUPS_UnbindVariables
gap> Unbind(BruteForceInverseCheck);
gap> Unbind(BruteForceIsoCheck);
gap> Unbind(F);
gap> Unbind(G);
gap> Unbind(R);
gap> Unbind(S);
gap> Unbind(T);
gap> Unbind(inv);
gap> Unbind(map);
gap> Unbind(rels);
gap> Unbind(x);
gap> Unbind(y);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/semifp.tst");
