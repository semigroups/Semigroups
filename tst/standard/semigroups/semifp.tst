#############################################################################
##
##  standard/semigroups/semifp.tst
#Y  Copyright (C) 2015-2022                                 Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

## We don't use local variables in this test file because it doesn't play nice
## with AssignGeneratorVariables, which is used repeatedly here.

gap> START_TEST("Semigroups package: standard/semigroups/semifp.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#
gap> Noop := 0;;
gap> TestEnumerator := function(en)
> return ForAll(en, x -> en[Position(en, x)] = x) 
> and ForAll([1 .. Length(en)], i -> Position(en, en[i]) = i)
> and ForAll(en, x -> x in en)
> and ForAll([1 .. Length(en)], i -> IsBound(en[i]))
> and ForAll([Length(en) + 1 .. Length(en) + 10], i -> not IsBound(en[i]));
> end;;
gap> TestIterator := function(S, it)
> local LoopIterator;
> LoopIterator := function(it)
>   local valid, len, x;
>   valid := true;;
>   len := 0;
>   for x in it do 
>     len := len + 1;
>     if not x in S then 
>       valid := false;
>       break;
>     fi;
>   od;
>   return valid and IsDoneIterator(it) and len = Size(S);
> end;
> return LoopIterator(it) and LoopIterator(ShallowCopy(it));
> end;;

# Test IsomorphismFpMonoid, 
gap> S := Monoid(Transformation([1, 3, 4, 1, 3]),
>                Transformation([2, 4, 1, 5, 5]),
>                Transformation([2, 5, 3, 5, 3]),
>                Transformation([4, 1, 2, 2, 1]),
>                Transformation([5, 5, 1, 1, 3]));;
gap> map := IsomorphismFpMonoid(S);
<transformation monoid of degree 5 with 5 generators> -> 
<fp monoid with 5 generators and 608 relations of length 4953>
gap> inv := InverseGeneralMapping(map);
<fp monoid with 5 generators and 608 relations of length 4953> -> 
<transformation monoid of degree 5 with 5 generators>
gap> ForAll(S, x -> (x ^ map) ^ inv = x);
true
gap> map := IsomorphismFpSemigroup(S);;
gap> inv := InverseGeneralMapping(map);;
gap> ForAll(S, x -> (x ^ map) ^ inv = x);
true

# Test IsomorphismFpMonoid, infinite
gap> IsomorphismFpMonoid(FreeMonoid(2));
<free monoid on the generators [ m1, m2 ]> -> 
<fp monoid with 2 generators and 0 relations of length 2>

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
<fp semigroup with 1 generator and 1 relation of length 4>
gap> RelationsOfFpSemigroup(S);
[ [ s1^2, s1 ] ]

# AsFpSemigroup 2: 2 element semilattice
gap> S := Semigroup(PartialPerm([1]), PartialPerm([], []));
<partial perm monoid of rank 1 with 2 generators>
gap> S := AsSemigroup(IsFpSemigroup, S);
<fp semigroup with 2 generators and 4 relations of length 14>
gap> RelationsOfFpSemigroup(S);
[ [ s1^2, s1 ], [ s1*s2, s2 ], [ s2*s1, s2 ], [ s2^2, s2 ] ]

# AsFpMonoid 1: trivial semigroup
gap> S := TrivialSemigroup();
<trivial transformation group of degree 0 with 1 generator>
gap> S := AsMonoid(IsFpMonoid, S);;

# AsFpMonoid 2: 2 element semilattice
gap> S := Semigroup(PartialPerm([1]), PartialPerm([], []));
<partial perm monoid of rank 1 with 2 generators>
gap> S := AsMonoid(IsFpMonoid, S);
<fp monoid with 1 generator and 1 relation of length 4>
gap> RelationsOfFpMonoid(S);
[ [ m1^2, m1 ] ]

# AsSemigroup: 
#   convert from IsPBRSemigroup to IsFpSemigroup
gap> S := Semigroup([
> PBR([[-1], [-3], [-3]], [[1], [], [2, 3]]),
> PBR([[-2], [-3], [-3]], [[], [1], [2, 3]])]);
<pbr semigroup of degree 3 with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup with 2 generators and 3 relations of length 12>
gap> Size(S) = Size(T);
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
<fp semigroup with 2 generators and 3 relations of length 12>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup with 2 generators and 3 relations of length 12>
gap> Size(S) = Size(T);
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
<fp semigroup with 2 generators and 3 relations of length 12>
gap> Size(S) = Size(T);
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
<fp semigroup with 2 generators and 3 relations of length 12>
gap> Size(S) = Size(T);
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
<fp semigroup with 2 generators and 3 relations of length 12>
gap> Size(S) = Size(T);
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
<fp semigroup with 2 generators and 3 relations of length 12>
gap> Size(S) = Size(T);
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
<fp semigroup with 2 generators and 3 relations of length 12>
gap> Size(S) = Size(T);
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
<fp semigroup with 2 generators and 3 relations of length 12>
gap> Size(S) = Size(T);
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
> Matrix(Integers,
>        [[1, 0, 0],
> [0, 0, 1],
> [0, 0, 1]]),
> Matrix(Integers,
>        [[0, 1, 0],
> [0, 0, 1],
> [0, 0, 1]])]);
<semigroup of 3x3 integer matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup with 2 generators and 3 relations of length 12>
gap> Size(S) = Size(T);
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
<fp semigroup with 2 generators and 3 relations of length 12>
gap> Size(S) = Size(T);
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
<fp semigroup with 2 generators and 3 relations of length 12>
gap> Size(S) = Size(T);
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
<fp semigroup with 2 generators and 3 relations of length 12>
gap> Size(S) = Size(T);
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
<fp semigroup with 3 generators and 9 relations of length 34>
gap> Size(S) = Size(T);
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
<fp monoid with 2 generators and 4 relations of length 18>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup with 3 generators and 9 relations of length 34>
gap> Size(S) = Size(T);
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
<fp semigroup with 3 generators and 9 relations of length 34>
gap> Size(S) = Size(T);
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
<fp semigroup with 3 generators and 9 relations of length 34>
gap> Size(S) = Size(T);
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
<fp semigroup with 3 generators and 9 relations of length 34>
gap> Size(S) = Size(T);
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
<fp semigroup with 3 generators and 9 relations of length 34>
gap> Size(S) = Size(T);
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
<fp semigroup with 3 generators and 9 relations of length 34>
gap> Size(S) = Size(T);
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
<fp semigroup with 3 generators and 9 relations of length 34>
gap> Size(S) = Size(T);
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
> Matrix(Integers,
>        [[0, 1, 0],
> [0, 1, 0],
> [0, 0, 1]]),
> Matrix(Integers,
>        [[1, 0, 0],
> [0, 0, 1],
> [0, 0, 1]])]);
<monoid of 3x3 integer matrices with 2 generators>
gap> T := AsSemigroup(IsFpSemigroup, S);
<fp semigroup with 3 generators and 9 relations of length 34>
gap> Size(S) = Size(T);
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
<fp semigroup with 3 generators and 9 relations of length 34>
gap> Size(S) = Size(T);
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
<fp semigroup with 3 generators and 9 relations of length 34>
gap> Size(S) = Size(T);
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
<fp semigroup with 3 generators and 9 relations of length 34>
gap> Size(S) = Size(T);
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
<fp monoid with 1 generator and 1 relation of length 3>
gap> Size(S) = Size(T);
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
<fp semigroup with 2 generators and 4 relations of length 14>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid with 1 generator and 1 relation of length 3>
gap> Size(S) = Size(T);
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
<fp monoid with 1 generator and 1 relation of length 3>
gap> Size(S) = Size(T);
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
<fp monoid with 1 generator and 1 relation of length 3>
gap> Size(S) = Size(T);
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
<fp monoid with 1 generator and 1 relation of length 3>
gap> Size(S) = Size(T);
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
<fp monoid with 1 generator and 1 relation of length 3>
gap> Size(S) = Size(T);
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
<fp monoid with 1 generator and 1 relation of length 3>
gap> Size(S) = Size(T);
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
<fp monoid with 1 generator and 1 relation of length 3>
gap> Size(S) = Size(T);
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
> Matrix(Integers,
>        [[1, 0, 0, 0, 0],
>         [0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0],
>         [0, 1, 0, 0, 0]]),
> Matrix(Integers,
>        [[0, 1, 0, 0, 0],
>         [1, 0, 0, 0, 0],
>         [1, 0, 0, 0, 0],
>         [1, 0, 0, 0, 0],
>         [1, 0, 0, 0, 0]])]);
<semigroup of 5x5 integer matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid with 1 generator and 1 relation of length 3>
gap> Size(S) = Size(T);
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
<fp monoid with 1 generator and 1 relation of length 3>
gap> Size(S) = Size(T);
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
<fp monoid with 1 generator and 1 relation of length 3>
gap> Size(S) = Size(T);
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
<fp monoid with 1 generator and 1 relation of length 3>
gap> Size(S) = Size(T);
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
<fp monoid with 2 generators and 6 relations of length 28>
gap> Size(S) = Size(T);
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
<fp monoid with 2 generators and 6 relations of length 28>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid with 2 generators and 6 relations of length 28>
gap> Size(S) = Size(T);
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
<fp monoid with 2 generators and 6 relations of length 28>
gap> Size(S) = Size(T);
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
<fp monoid with 2 generators and 6 relations of length 28>
gap> Size(S) = Size(T);
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
<fp monoid with 2 generators and 6 relations of length 28>
gap> Size(S) = Size(T);
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
<fp monoid with 2 generators and 6 relations of length 28>
gap> Size(S) = Size(T);
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
<fp monoid with 2 generators and 6 relations of length 28>
gap> Size(S) = Size(T);
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
<fp monoid with 2 generators and 6 relations of length 28>
gap> Size(S) = Size(T);
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
> Matrix(Integers,
>        [[0, 0, 1],
>         [0, 1, 0],
>         [0, 0, 1]]),
> Matrix(Integers,
>        [[0, 1, 0],
>         [0, 1, 0],
>         [1, 0, 0]])]);
<monoid of 3x3 integer matrices with 2 generators>
gap> T := AsMonoid(IsFpMonoid, S);
<fp monoid with 2 generators and 6 relations of length 28>
gap> Size(S) = Size(T);
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
<fp monoid with 2 generators and 6 relations of length 28>
gap> Size(S) = Size(T);
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
<fp monoid with 2 generators and 6 relations of length 28>
gap> Size(S) = Size(T);
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
<fp monoid with 2 generators and 6 relations of length 28>
gap> Size(S) = Size(T);
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
<fp semigroup with 2 generators and 4 relations of length 19>
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
<fp monoid with 1 generator and 1 relation of length 3>
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
<fp semigroup with 3 generators and 11 relations of length 50>
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
<fp monoid with 2 generators and 4 relations of length 13>
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
<fp semigroup with 4 generators and 16 relations of length 63>
gap> Size(S) = Size(T);
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
<fp semigroup with 5 generators and 16 relations of length 57>
gap> Size(S) = Size(T);
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
<fp monoid with 4 generators and 7 relations of length 22>
gap> Size(S) = Size(T);
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
<fp semigroup with 5 generators and 16 relations of length 57>
gap> Size(S) = Size(T);
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
<fp monoid with 4 generators and 7 relations of length 22>
gap> Size(S) = Size(T);
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
<fp semigroup with 2 generators and 6 relations of length 29>
gap> Size(S) = Size(T);
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
<fp monoid with 2 generators and 6 relations of length 29>
gap> Size(S) = Size(T);
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
<fp semigroup with 3 generators and 11 relations of length 45>
gap> Size(S) = Size(T);
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
<fp semigroup with 3 generators and 15 relations of length 87>
gap> Size(S) = Size(T);
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
<fp monoid with 3 generators and 15 relations of length 87>
gap> Size(S) = Size(T);
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
<fp semigroup with 4 generators and 22 relations of length 109>
gap> Size(S) = Size(T);
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
<fp monoid with 4 generators and 19 relations of length 95>
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
<fp semigroup with 4 generators and 21 relations of length 105>
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
Error, the arguments must be positive integers

# Test RandomMonoid
gap> RandomMonoid(IsFpMonoid);;
gap> RandomMonoid(IsFpMonoid, 3);;
gap> RandomMonoid(IsFpMonoid, 5, 5);;
gap> RandomMonoid(IsFpMonoid, "a");
Error, the arguments must be positive integers

# Test RandomInverseSemigroup
gap> RandomInverseSemigroup(IsFpSemigroup);;
gap> RandomInverseSemigroup(IsFpSemigroup, 3);;
gap> RandomInverseSemigroup(IsFpSemigroup, 5, 5);;
gap> RandomInverseSemigroup(IsFpSemigroup, "a");
Error, the arguments must be positive integers

# Test RandomInverseMonoid
gap> RandomInverseMonoid(IsFpMonoid);;
gap> RandomInverseMonoid(IsFpMonoid, 3);;
gap> RandomInverseMonoid(IsFpMonoid, 5, 5);;
gap> RandomInverseMonoid(IsFpMonoid, "a");
Error, the arguments must be positive integers

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
<fp semigroup with 1 generator and 1 relation of length 4>
gap> AsMonoid(S);;

# Test IsomorphismFpMonoid, fail
gap> S := SingularTransformationMonoid(3);
<regular transformation semigroup ideal of degree 3 with 1 generator>
gap> IsomorphismFpMonoid(S);
Error, the 1st argument (a semigroup) must satisfy `IsMonoidAsSemigroup`

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
Error, the argument be at most 52

# Test SEMIGROUPS.ExtRepObjToString
gap> SEMIGROUPS.ExtRepObjToString(SEMIGROUPS.WordToExtRepObj(w));
"a ^ 100b ^ 50ca ^ 8"
gap> SEMIGROUPS.ExtRepObjToString([100, 1]);
Error, the maximum value in an odd position of the argument must be at most 52

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
Error, the second argument must be a non-empty list
gap> F := FreeMonoid(4);;
gap> x := EvaluateExtRepObjWord(Generators(F), [1, 4, 2, 5, 3, 1, 2, 1]);
m1^4*m2^5*m3*m2
gap> ExtRepOfObj(x) = [1, 4, 2, 5, 3, 1, 2, 1];
true
gap> EvaluateExtRepObjWord(Generators(F), []);
<identity ...>

# TestAssignGeneratorVariables for fp semigroups
gap> f := FreeSemigroup("x", "y");;
gap> s := f / [[f.1 * f.2, f.2 * f.1]];;
gap> AssignGeneratorVariables(s);;
gap> x * y = y * x;
true

# TestAssignGeneratorVariables for fp monoids
gap> f := FreeMonoid("x", "y");;
gap> s := f / [[f.1 * f.2, f.2 * f.1]];;
gap> AssignGeneratorVariables(s);;
gap> x * y = y * x;
true

# Test ParseRelations
gap> f := FreeSemigroup("x", "y", "e");                 
<free semigroup on the generators [ x, y, e ]>
gap> ParseRelations(GeneratorsOfSemigroup(f), "ex=x=xe, ey=y=ye, xy = e");
[ [ e*x, x ], [ x, x*e ], [ e*y, y ], [ y, y*e ], [ x*y, e ] ]
gap> f := FreeSemigroup("x", "y", "a", "b", "X");                              
<free semigroup on the generators [ x, y, a, b, X ]>
gap> ParseRelations(GeneratorsOfSemigroup(f), ",x=X^3(yx^2)=a,b(aX)^3x=XXXX");
[ [ x, X^3*y*x^2 ], [ X^3*y*x^2, a ], [ b*(a*X)^3*x, X^4 ] ]
gap> ParseRelations(GeneratorsOfSemigroup(f), "yx=x= ((a)b^2y)^50");
[ [ y*x, x ], [ x, (a*b^2*y)^50 ] ]
gap> f := FreeSemigroup("x", "y", "a", "b", "X", "@");;
gap> ParseRelations(GeneratorsOfSemigroup(f), "x=y");
Error, expected the 1st argument to be a list of a free semigroup or monoid ge\
nerators represented by a single alphabet letter but found @
gap> f := FreeSemigroup("x", "y");;
gap> ParseRelations(GeneratorsOfSemigroup(f), "x=()");
Error, expected the 2nd argument to be a string listing the relations of a sem\
igroup but found an = symbol which isn't pairing two words
gap> ParseRelations(GeneratorsOfSemigroup(f), "x=(");
Error, expected the number of open brackets to match the number of closed brac\
kets
gap> ParseRelations(GeneratorsOfSemigroup(f), "x=(^2)");
Error, expected ^ to be preceded by a ) or a generator but found (
gap> ParseRelations(GeneratorsOfSemigroup(f), "x=a^");
Error, expected ^ to be followed by a positive integer but found end of string
gap> ParseRelations(GeneratorsOfSemigroup(f), "x=x^y");
Error, expected ^ to be followed by a positive integer but found y
gap> ParseRelations(GeneratorsOfSemigroup(f), "x=^y");
Error, expected ^ to be preceded by a ) or a generator but found beginning of \
string
gap> ParseRelations(GeneratorsOfSemigroup(f), "x=a");
Error, expected a free semigroup generator but found a
gap> ParseRelations(GeneratorsOfSemigroup(f), "x=y^0");
Error, expected ^ to be followed by a positive integer but found 0
gap> ParseRelations(GeneratorsOfSemigroup(f), "x=");
Error, expected the 2nd argument to be a string listing the relations of a sem\
igroup but found an = symbol which isn't pairing two words

#
gap> f := FreeMonoid("x", "y", "a", "b", "X");                              
<free monoid on the generators [ x, y, a, b, X ]>
gap> ParseRelations(GeneratorsOfMonoid(f),
> "x=X^3(yx^2)=a,b(aX)^3x=XXXX,XXX=1");
[ [ x, X^3*y*x^2 ], [ X^3*y*x^2, a ], [ b*(a*X)^3*x, X^4 ], 
  [ X^3, <identity ...> ] ]
gap> f := FreeMonoid("x", "y", "e");                 
<free monoid on the generators [ x, y, e ]>
gap> ParseRelations(GeneratorsOfMonoid(f), "ex=x=xe=1, ey=y=ye, xy = e, 1 = 1");
[ [ e*x, x ], [ x, x*e ], [ x*e, <identity ...> ], [ e*y, y ], [ y, y*e ], 
  [ x*y, e ], [ <identity ...>, <identity ...> ] ]
gap> f := FreeMonoid("x", "y", "a", "b", "X");                              
<free monoid on the generators [ x, y, a, b, X ]>
gap> ParseRelations(GeneratorsOfMonoid(f), ",x=X^3(yx^2)=a,b(aX)^3x=XXXX=1^1=1");
[ [ x, X^3*y*x^2 ], [ X^3*y*x^2, a ], [ b*(a*X)^3*x, X^4 ], 
  [ X^4, <identity ...> ], [ <identity ...>, <identity ...> ] ]
gap> ParseRelations(GeneratorsOfMonoid(f), "1=yx=x= ((a)b^2y)^50=1");
[ [ <identity ...>, y*x ], [ y*x, x ], [ x, (a*b^2*y)^50 ], 
  [ (a*b^2*y)^50, <identity ...> ] ]
gap> f := FreeMonoid("x", "y", "a", "b", "X", "@");;
gap> ParseRelations(GeneratorsOfMonoid(f), "1=x^1=y");
Error, expected the 1st argument to be a list of a free semigroup or monoid ge\
nerators represented by a single alphabet letter but found @
gap> f := FreeMonoid("x", "y");;
gap> ParseRelations(GeneratorsOfMonoid(f), "1=()");
Error, expected the 2nd argument to be a string listing the relations of a sem\
igroup but found an = symbol which isn't pairing two words
gap> ParseRelations(GeneratorsOfMonoid(f), "1=(");
Error, expected the number of open brackets to match the number of closed brac\
kets
gap> ParseRelations(GeneratorsOfMonoid(f), "x=(^1)");
Error, expected ^ to be preceded by a ) or a generator but found (
gap> ParseRelations(GeneratorsOfMonoid(f), "1=a^");
Error, expected ^ to be followed by a positive integer but found end of string
gap> ParseRelations(GeneratorsOfMonoid(f), "1=x^y");
Error, expected ^ to be followed by a positive integer but found y
gap> ParseRelations(GeneratorsOfMonoid(f), "1=^y");
Error, expected ^ to be preceded by a ) or a generator but found beginning of \
string
gap> ParseRelations(GeneratorsOfMonoid(f), "1=x=a");
Error, expected a free semigroup generator but found a
gap> ParseRelations(GeneratorsOfMonoid(f), "1=x=y^0");
Error, expected ^ to be followed by a positive integer but found 0
gap> ParseRelations(GeneratorsOfMonoid(f), "1=");
Error, expected the 2nd argument to be a string listing the relations of a sem\
igroup but found an = symbol which isn't pairing two words

# Test ElementOfFpSemigroup
gap> f := FreeSemigroup("x", "y");;
gap> x := f.1;;
gap> y := f.2;;
gap> s := f / [[x * y, y * x]];
<fp semigroup with 2 generators and 1 relation of length 6>
gap> a := ElementOfFpSemigroup(s, x * y);
x*y
gap> b := ElementOfFpSemigroup(s, y * x);
y*x
gap> x * y = y * x;
false
gap> a = b;
true

# Test ElementOfFpMonoid
gap> f := FreeMonoid("x", "y");;
gap> x := f.1;;
gap> y := f.2;;
gap> s := f / [[x * y, y * x]];
<fp monoid with 2 generators and 1 relation of length 6>
gap> a := ElementOfFpMonoid(s, x * y);
x*y
gap> b := ElementOfFpMonoid(s, y * x);
y*x
gap> x * y = y * x;
false
gap> a = b;
true

# NiceMonomorphism
gap> S := Monoid(Transformation([1, 3, 4, 1, 3]),
>                Transformation([2, 4, 1, 5, 5]),
>                Transformation([2, 5, 3, 5, 3]),
>                Transformation([4, 1, 2, 2, 1]),
>                Transformation([5, 5, 1, 1, 3]));;
gap> map := IsomorphismFpSemigroup(S);
<transformation monoid of degree 5 with 5 generators> -> 
<fp semigroup with 6 generators and 619 relations of length 4987>
gap> T := Range(map);
<fp semigroup with 6 generators and 619 relations of length 4987>
gap> AsList(T);
[ s1, s2, s3, s4, s5, s6, s2^2, s2*s3, s2*s4, s2*s5, s2*s6, s3*s2, s3^2, 
  s3*s4, s3*s5, s3*s6, s4*s2, s4*s3, s4^2, s4*s5, s4*s6, s5*s2, s5*s3, s5*s4, 
  s5^2, s5*s6, s6*s2, s6*s3, s6*s4, s6*s5, s6^2, s2^3, s2^2*s3, s2^2*s5, 
  s2^2*s6, s2*s3*s2, s2*s3^2, s2*s3*s4, s2*s3*s6, s2*s4*s2, s2*s4*s3, 
  s2*s4^2, s2*s4*s5, s2*s4*s6, s2*s5*s2, s2*s5*s3, s2*s5*s4, s2*s5^2, 
  s2*s5*s6, s2*s6*s2, s2*s6*s3, s2*s6*s4, s2*s6*s5, s2*s6^2, s3*s2^2, 
  s3*s2*s3, s3*s2*s4, s3*s2*s5, s3*s2*s6, s3^2*s2, s3^3, s3^2*s4, s3^2*s5, 
  s3^2*s6, s3*s4*s2, s3*s4*s3, s3*s4^2, s3*s4*s5, s3*s4*s6, s3*s5*s2, 
  s3*s5*s3, s3*s5*s4, s3*s5^2, s3*s5*s6, s3*s6*s2, s3*s6*s3, s3*s6*s4, 
  s3*s6*s5, s3*s6^2, s4*s2^2, s4*s2*s3, s4*s2*s4, s4*s2*s5, s4*s3*s2, 
  s4*s3^2, s4*s3*s4, s4*s3*s5, s4*s3*s6, s4^2*s2, s4^2*s3, s4^3, s4^2*s5, 
  s4^2*s6, s4*s5*s2, s4*s5*s3, s4*s5*s4, s4*s6*s2, s4*s6*s3, s4*s6*s4, 
  s4*s6*s5, s4*s6^2, s5*s2^2, s5*s2*s3, s5*s2*s4, s5*s2*s5, s5*s2*s6, 
  s5*s3*s2, s5*s3^2, s5*s3*s4, s5*s3*s5, s5*s3*s6, s5*s4^2, s5^2*s3, s5^2*s4, 
  s5^3, s5*s6*s3, s5*s6*s4, s5*s6*s5, s6*s2^2, s6*s2*s3, s6*s2*s4, s6*s2*s5, 
  s6*s2*s6, s6*s3*s2, s6*s3^2, s6*s3*s4, s6*s3*s5, s6*s3*s6, s6*s4*s2, 
  s6*s4^2, s6*s5*s2, s6*s5*s3, s6*s5*s4, s6*s5^2, s6^2*s2, s6^2*s3, s6^2*s4, 
  s6^2*s5, s6^3, s2^3*s5, s2^2*s3^2, s2^2*s3*s4, s2^2*s5*s2, s2^2*s5^2, 
  s2^2*s5*s6, s2^2*s6*s4, s2*s3*s2^2, s2*s3*s2*s5, s2*s3^2*s6, s2*s3*s4*s2, 
  s2*s3*s4*s3, s2*s3*s4*s5, s2*s3*s4*s6, s2*s3*s6^2, s2*s4*s3*s2, 
  s2*s4*s3*s5, s2*s4^2*s2, s2*s4^2*s5, s2*s4*s6*s2, s2*s4*s6*s3, s2*s4*s6*s5, 
  s2*s5*s2*s4, s2*s5*s6*s3, s2*s5*s6*s5, s2*s6*s2*s5, s2*s6*s4*s2, 
  s3*s2^2*s3, (s3*s2)^2, s3*s2*s4*s2, s3*s2*s4*s3, s3*s2*s4^2, s3*s2*s4*s5, 
  s3*s2*s5*s3, s3*s2*s5*s6, s3*s2*s6*s4, s3*s2*s6^2, s3^2*s2^2, s3^2*s2*s5, 
  s3^2*s4*s2, s3^2*s4*s3, s3^2*s4*s5, s3^2*s4*s6, s3^2*s5*s4, s3^2*s6*s2, 
  s3^2*s6*s3, s3^2*s6*s5, s3^2*s6^2, s3*s4*s2^2, s3*s4*s2*s3, s3*s4*s2*s4, 
  s3*s4*s3*s2, s3*s4*s3^2, (s3*s4)^2, s3*s4*s3*s5, s3*s4*s3*s6, s3*s4^2*s2, 
  s3*s4^2*s3, s3*s4^2*s5, s3*s4*s5*s2, s3*s4*s5*s3, s3*s4*s5*s4, s3*s4*s6*s2, 
  s3*s4*s6*s3, s3*s4*s6*s4, s3*s4*s6*s5, s3*s4*s6^2, s3*s5*s2^2, s3*s5*s2*s3, 
  s3*s5*s2*s4, s3*s5*s2*s5, s3*s5*s2*s6, s3*s5*s3*s2, (s3*s5)^2, s3*s5*s3*s6, 
  s3*s5^2*s3, s3*s5^2*s4, s3*s5^3, s3*s5*s6*s3, s3*s5*s6*s4, s3*s5*s6*s5, 
  s3*s6*s2^2, s3*s6*s2*s3, s3*s6*s2*s4, s3*s6*s2*s5, s3*s6*s2*s6, 
  s3*s6*s3*s2, s3*s6*s3^2, s3*s6*s3*s4, s3*s6*s3*s5, (s3*s6)^2, s3*s6*s4*s2, 
  s3*s6*s4^2, s3*s6*s5*s3, s3*s6*s5*s4, s3*s6*s5^2, s3*s6^2*s2, s3*s6^2*s3, 
  s3*s6^2*s5, s3*s6^3, s4*s2*s3*s4, s4*s2*s3*s6, (s4*s2)^2, s4*s2*s4*s5, 
  s4*s3*s2^2, s4*s3*s2*s3, s4*s3*s2*s4, s4*s3*s2*s5, s4*s3*s2*s6, s4*s3^3, 
  s4*s3*s4*s2, (s4*s3)^2, s4*s3*s4*s5, s4*s3*s5*s3, s4*s3*s5*s4, s4*s3*s5^2, 
  s4*s3*s5*s6, s4*s3*s6*s2, s4*s3*s6*s3, s4*s3*s6*s5, s4*s3*s6^2, s4^2*s3*s4, 
  s4^2*s5*s3, s4*s5*s2^2, s4*s5*s2*s5, s4*s5*s3*s2, s4*s5*s3^2, s4*s6*s2*s3, 
  s4*s6*s2*s4, s4*s6*s3*s2, s4*s6*s3^2, s4*s6*s3*s4, s4*s6*s5*s3, s4*s6*s5^2, 
  s4*s6^2*s2, s4*s6^2*s3, s4*s6^2*s4, s4*s6^2*s5, s4*s6^3, s5*s2^2*s3, 
  s5*s2*s4*s2, s5*s2*s4*s3, s5*s2*s4^2, s5*s2*s5*s3, s5*s3*s4*s2, 
  s5*s3*s6*s2, s5*s3*s6*s3, s5*s3*s6*s5, s5*s3*s6^2, s5*s4^2*s2, s5^3*s3, 
  s5*s6*s3^2, s6*s2^2*s3, s6*s2^2*s5, s6*s2^2*s6, s6*s2*s3*s2, s6*s2*s3^2, 
  s6*s2*s3*s4, s6*s2*s3*s6, s6*s2*s4*s2, s6*s2*s4*s3, s6*s2*s4*s5, 
  s6*s2*s4*s6, s6*s2*s5*s3, s6*s3*s2^2, s6*s3*s2*s3, s6*s3*s2*s5, 
  s6*s3*s2*s6, s6*s3^3, s6*s3^2*s6, s6*s3*s4*s3, s6*s3*s4^2, s6*s3*s5*s3, 
  s6*s3*s5^2, s6*s3*s5*s6, s6*s5*s2*s3, s6*s5*s2*s4, s6*s5*s3^2, s6*s5*s3*s6, 
  s6*s5*s4^2, s6*s5^2*s4, s6^2*s2*s3, s6^2*s2*s4, s6^2*s3*s4, s6^2*s4*s2, 
  s6^2*s5*s3, s6^3*s2, s6^3*s3, s6^3*s4, s2^2*s5*s2*s4, s2*s3^2*s6*s3, 
  s2*s3^2*s6*s5, s2*s3^2*s6^2, s2*s3*s4*s2^2, s2*(s3*s4)^2, s2*s3*s4*s3*s5, 
  s2*s3*s4*s3*s6, s2*s3*s4*s5*s3, s2*s3*s4*s5*s4, s2*s3*s4*s6*s2, 
  s2*s3*s4*s6*s3, s2*s3*s4*s6*s5, s2*s3*s4*s6^2, s2*s4*s3*s2*s4, 
  s2*s4*s3*s2*s5, s2*s4*s3*s5*s3, s2*s4*s6*s2*s3, s2*s4*s6*s2*s4, 
  s2*s4*s6*s3^2, s2*s4*s6*s5*s3, s2*s4*s6*s5^2, s2*s5*s6*s3^2, s3*s2^2*s3^2, 
  (s3*s2)^2*s5, s3*s2*s6*s4*s2, s3^2*s2*s5*s3, s3^2*s4*s2*s3, s3^2*s4*s3^2, 
  s3*(s3*s4)^2, s3^2*s4*s3*s5, s3^2*s4*s5*s2, s3^2*s4*s5*s3, s3^2*s6*s2*s3, 
  s3^2*s6*s2*s4, s3^2*s6*s3^2, s3^2*s6*s3*s4, s3^2*s6*s5*s3, s3^2*s6*s5^2, 
  s3^2*s6^2*s2, s3^2*s6^2*s3, s3^2*s6^2*s5, s3^2*s6^3, s3*s4*s2*s3*s4, 
  s3*s4*s2*s3*s6, s3*(s4*s2)^2, s3*s4*s2*s4*s5, s3*s4*s3*s2*s3, 
  s3*s4*s3*s2*s4, s3*s4*s3^3, (s3*s4)^2*s3, s3*s4*s3*s5*s3, s3*s4*s3*s5^2, 
  s3*s4*s3*s5*s6, s3*s4*s3*s6*s2, s3*s4*s3*s6*s3, s3*s4*s3*s6*s5, 
  s3*s4*s3*s6^2, s3*s4*s5*s2*s5, s3*s4*s5*s3^2, s3*s4*s6*s2*s3, 
  s3*s4*s6*s2*s4, s3*s4*s6*s3^2, s3*s4*s6*s3*s4, s3*s4*s6*s5*s3, 
  s3*s4*s6*s5^2, s3*s4*s6^2*s2, s3*s4*s6^2*s3, s3*s4*s6^2*s5, s3*s4*s6^3, 
  s3*s5*s2^2*s3, s3*s5*s2*s4*s2, s3*s5*s2*s4*s3, s3*s5*s2*s4^2, 
  s3*s5*s2*s5*s3, s3*s5*s3*s6*s2, s3*s5*s3*s6*s3, s3*s5*s3*s6*s5, 
  s3*s5*s3*s6^2, s3*s5^3*s3, s3*s6*s2*s3^2, s3*s6*s2*s3*s4, s3*s6*s2*s4*s2, 
  s3*s6*s2*s4*s3, s3*s6*s2*s4*s5, s3*s6*s2*s4*s6, s3*s6*s2*s5*s3, 
  s3*s6*s3^2*s6, s3*s6*s3*s4*s3, s3*s6*s5*s3^2, s3*s6*s5*s3*s6, 
  s3*s6^2*s2*s3, s3*s6^2*s2*s4, s3*s6^2*s3*s4, s3*s6^2*s5*s3, s3*s6^3*s2, 
  s3*s6^3*s3, s3*s6^3*s4, s4*s3*s2^2*s3, s4*s3*s2*s4*s3, s4*s3*s2*s4^2, 
  s4*s3*s2*s5*s3, s4*s3*s2*s5*s6, s4*s3*s2*s6^2, (s4*s3)^2*s6, 
  s4*s3*s4*s5*s3, s4*s3*s5^2*s3, s4*s3*s6*s2*s3, s4*s3*s6*s3*s4, 
  s4*s3*s6*s5*s3, s4*s3*s6^2*s2, s4*s3*s6^2*s3, s4*s6*s2*s3*s4, 
  s4*s6*s2*s4*s3, s4*s6*s3*s4*s3, s4*s6^2*s4*s2, s4*s6^2*s5*s3, s4*s6^3*s2, 
  s5*s3*s6*s2*s3, s5*s3*s6*s2*s4, s5*s3*s6*s3^2, s5*s3*s6*s3*s4, 
  s5*s3*s6*s5*s3, s5*s3*s6*s5^2, s5*s3*s6^2*s2, s5*s3*s6^2*s3, s5*s3*s6^2*s5, 
  s5*s3*s6^3, s6*s2^2*s3^2, s6*s2^2*s5*s6, s6*s2*s3*s2^2, s6*s2*s3*s2*s5, 
  s6*s2*s3*s4*s2, s6*s2*s3*s4*s3, s6*s2*s4*s6*s2, s6*s2*s4*s6*s3, 
  s6*s2*s4*s6*s5, s6*s3*s2*s5*s3, s6*s3^2*s6*s2, s6*s3^2*s6*s5, 
  s6*s5*s3*s6*s2, s6*s5*s3*s6*s5, s6*s5*s4^2*s2, s6^2*s2*s3^2, s6^2*s2*s4*s3, 
  s6^2*s3*s4*s3, s6^3*s3*s4, s2*s3^2*s6*s3*s4, s2*s3^2*s6*s5*s3, 
  s2*s3^2*s6^2*s2, s2*s3^2*s6^2*s3, s2*(s3*s4)^2*s3, s2*s3*s4*s3*s5*s3, 
  s2*s3*s4*s3*s6*s2, s2*s3*s4*s5*s3^2, s2*s3*s4*s6^2*s2, s2*s4*s6*s2*s3*s4, 
  s2*s4*s6*s2*s4*s3, s3^2*s4*s3^3, s3^2*s4*s3*s5^2, s3^2*s6*s2*s3*s4, 
  s3^2*s6*s2*s4*s3, s3^2*s6*s2*s4*s6, (s3^2*s6)^2, s3^2*s6*s3*s4*s3, 
  s3^2*s6*s5*s3*s6, s3^2*s6^2*s2*s3, s3^2*s6^2*s2*s4, s3^2*s6^2*s3*s4, 
  s3^2*s6^2*s5*s3, s3^2*s6^3*s2, s3^2*s6^3*s3, (s3*s4)^2*s3*s6, 
  s3*s4*s3*s5^2*s3, s3*s4*s3*s6*s2*s3, s3*s4*s3*s6*s3*s4, s3*s4*s3*s6*s5*s3, 
  s3*s4*s3*s6^2*s2, s3*s4*s3*s6^2*s3, s3*s4*s6*s2*s3*s4, s3*s4*s6*s2*s4*s3, 
  s3*s4*s6*s3*s4*s3, s3*s4*s6^2*s5*s3, s3*s4*s6^3*s2, s3*s5*s3*s6*s2*s3, 
  s3*s5*s3*s6*s2*s4, s3*s5*s3*s6*s3^2, s3*s5*s3*s6*s3*s4, s3*s5*s3*s6*s5*s3, 
  s3*s5*s3*s6*s5^2, s3*s5*s3*s6^2*s2, s3*s5*s3*s6^2*s3, s3*s5*s3*s6^2*s5, 
  s3*s5*s3*s6^3, s3*s6*s2*s3*s4*s3, s3*s6*s2*s4*s6*s2, s3*s6*s2*s4*s6*s3, 
  s3*s6*s2*s4*s6*s5, s3*s6*s3^2*s6*s2, s3*s6*s3^2*s6*s5, s3*s6*s5*s3*s6*s2, 
  (s3*s6*s5)^2, s3*s6^2*s2*s3^2, s3*s6^2*s2*s4*s3, s3*s6^2*s3*s4*s3, 
  s3*s6^3*s3*s4, s4*s3*s2^2*s3^2, (s4*s3)^2*s6*s2, s4*s3*s6*s3*s4*s3, 
  s4*s6*s2*s3*s4*s3, s5*s3*s6*s2*s3*s4, s5*s3*s6*s2*s4*s3, s5*s3*s6*s2*s4*s6, 
  s5*s3*s6*s3^2*s6, s5*s3*s6*s3*s4*s3, (s5*s3*s6)^2, s5*s3*s6^2*s2*s3, 
  s5*s3*s6^2*s2*s4, s5*s3*s6^2*s3*s4, s5*s3*s6^2*s5*s3, s5*s3*s6^3*s2, 
  s5*s3*s6^3*s3, s6*s2*s4*s6*s3^2, s6^3*s3*s4*s3, s2*s3^2*s6*s3*s4*s3, 
  s2*s4*s6*s2*s3*s4*s3, s3^2*s6*s2*s3*s4*s3, s3^2*s6*s2*s4*s6*s2, 
  s3^2*s6*s2*s4*s6*s3, s3^2*s6*s2*s4*s6*s5, (s3^2*s6)^2*s2, (s3^2*s6)^2*s5, 
  s3^2*s6*s5*s3*s6*s2, s3*(s3*s6*s5)^2, s3^2*s6^2*s2*s3^2, s3^2*s6^2*s2*s4*s3,
  s3^2*s6^2*s3*s4*s3, s3^2*s6^3*s3*s4, (s3*s4)^2*s3*s6*s2, 
  s3*s4*s3*s6*s3*s4*s3, s3*s4*s6*s2*s3*s4*s3, s3*s5*s3*s6*s2*s3*s4, 
  s3*s5*s3*s6*s2*s4*s3, s3*s5*s3*s6*s2*s4*s6, s3*s5*s3*s6*s3^2*s6, 
  s3*s5*s3*s6*s3*s4*s3, s3*(s5*s3*s6)^2, s3*s5*s3*s6^2*s2*s3, 
  s3*s5*s3*s6^2*s2*s4, s3*s5*s3*s6^2*s3*s4, s3*s5*s3*s6^2*s5*s3, 
  s3*s5*s3*s6^3*s2, s3*s5*s3*s6^3*s3, s3*s6*s2*s4*s6*s3^2, s3*s6^3*s3*s4*s3, 
  s5*s3*s6*s2*s3*s4*s3, s5*s3*s6*s2*s4*s6*s2, s5*s3*s6*s2*s4*s6*s3, 
  s5*s3*s6*s3^2*s6*s2, (s5*s3*s6)^2*s2, s5*s3*s6^2*s2*s4*s3, 
  s5*s3*s6^2*s3*s4*s3, s5*s3*s6^3*s3*s4, s3^2*s6*s2*s4*s6*s3^2, 
  s3^2*s6^3*s3*s4*s3, s3*s5*s3*s6*s2*s3*s4*s3, s3*s5*s3*s6*s2*s4*s6*s2, 
  s3*s5*s3*s6*s2*s4*s6*s3, s3*s5*s3*s6*s3^2*s6*s2, s3*(s5*s3*s6)^2*s2, 
  s3*s5*s3*s6^2*s2*s4*s3, s3*s5*s3*s6^2*s3*s4*s3, s3*s5*s3*s6^3*s3*s4, 
  s5*s3*s6^3*s3*s4*s3, s3*s5*s3*s6^3*s3*s4*s3 ]
gap> S := Monoid(Transformation([1, 3, 4, 1, 3]),
>                Transformation([4, 1, 2, 2, 1]),
>                Transformation([5, 5, 1, 1, 3]));;
gap> map := IsomorphismFpSemigroup(S);
<transformation monoid of degree 5 with 3 generators> -> 
<fp semigroup with 4 generators and 68 relations of length 491>
gap> T := Range(map);
<fp semigroup with 4 generators and 68 relations of length 491>
gap> TestEnumerator(Enumerator(T));
true
gap> AsSSortedList(T);
[ s1, s2, s3, s4, s2^2, s2*s3, s2*s4, s3*s2, s3^2, s3*s4, s4*s2, s4*s3, s4^2, 
  s2^3, s2^2*s3, s2^2*s4, s2*s3*s2, s2*s3^2, s2*s3*s4, s2*s4*s2, s2*s4*s3, 
  s2*s4^2, s3*s2^2, s3*s2*s3, s3*s2*s4, s3^2*s2, s3^3, s3*s4*s2, s3*s4*s3, 
  s3*s4^2, s4*s2^2, s4*s2*s3, s4*s2*s4, s4*s3*s2, s4*s3^2, s4^2*s2, s4^2*s3, 
  s4^3, s2^3*s3, s2^3*s4, s2^2*s3*s2, s2^2*s3^2, s2^2*s3*s4, s2^2*s4*s2, 
  s2^2*s4^2, s2*s3*s4*s3, s2*s3*s4^2, s2*s4*s2*s3, s2*s4^2*s2, s3*s2*s3^2, 
  s3*s2*s4*s2, s3*s2*s4^2, s3^2*s2*s3, s3*s4*s2^2, s3*s4*s2*s3, s3*s4*s2*s4, 
  s3*s4^2*s2, s3*s4^2*s3, s4*s2^2*s3, s4*s2^2*s4, s4*s2*s3^2, s4*s2*s4*s3, 
  s4*s2*s4^2, s4*s3*s2^2, s4*s3*s2*s3, s4*s3*s2*s4, s4^2*s2*s3, s4^2*s2*s4, 
  s4^2*s3*s2, s4^3*s2, s2^3*s3^2, s2^3*s4*s2, s2^2*s3*s4*s3, s2^2*s3*s4^2, 
  s2^2*s4*s2*s3, s2^2*s4^2*s2, s2*s3*s4^2*s2, s2*s3*s4^2*s3, s3*s2*s4^2*s2, 
  s3*s4*s2*s3^2, s3*s4*s2*s4^2, s4*s2^2*s3^2, s4*s2^2*s3*s4, s4*s2^2*s4*s2, 
  s4*s2^2*s4^2, s4*s2*s4^2*s2, s4*s3*s2*s3^2, s4*s3*s2*s4*s2, s4*s3*s2*s4^2, 
  s4^2*s2*s3^2, s4^2*s2*s4*s3, s4^2*s2*s4^2, s4^3*s2*s3, s2^2*s3*s4^2*s2, 
  s2^2*s3*s4^2*s3, s3*s4*s2*s4^2*s2, s4*s2^2*s3*s4*s3, s4*s2^2*s3*s4^2, 
  s4*s2^2*s4^2*s2, s4*s3*s2*s4^2*s2, (s4^2*s2)^2, s4^3*s2*s3^2, 
  s4*s2^2*s3*s4^2*s2 ]

# \<
gap> T.1 < T.2 * T.3;
true
gap> F := FreeSemigroup(2);; AssignGeneratorVariables(F);;
gap> rels := [[s1 ^ 2, s1], [s1 * s2, s2], [s2 ^ 2, s2 * s1]];;
gap> S := F / rels;
<fp semigroup with 2 generators and 3 relations of length 12>
gap> S.1 < S.2 * S.1;
true
gap> F := FreeMonoid(2);; AssignGeneratorVariables(F);;
gap> rels := [[m1 ^ 2, m1], [m2 ^ 2, m2], [m1 * m2 * m1, m1 * m2],
> [m2 * m1 * m2, m1 * m2]];;
gap> S := F / rels;
<fp monoid with 2 generators and 4 relations of length 18>
gap> S.1 < S.2 * S.1;
true

# IsomorphismFpSemigroup
gap> F := FreeGroup(1);
<free group on the generators [ f1 ]>
gap> R := [F.1 ^ 2];;
gap> IsomorphismFpSemigroup(F / R);
<fp group on the generators [ f1 ]> -> 
<fp semigroup with 3 generators and 8 relations of length 27>
gap> IsomorphismFpMonoid(F / R);
<fp group on the generators [ f1 ]> -> 
<fp monoid with 2 generators and 3 relations of length 8>

# String for IsElementOfFpMonoid 
gap> String(One(FreeMonoid(0)));
"<identity ...>"

# String + Print for FreeMonoid/Semigroup
gap> String(FreeMonoid("a", "b"));
"FreeMonoidAndAssignGeneratorVars([ \"a\", \"b\" ])"
gap> Print(FreeMonoid("a", "b"), "\n");
FreeMonoidAndAssignGeneratorVars([ "a", "b" ])
gap> String(FreeSemigroup("a", "b"));
"FreeSemigroupAndAssignGeneratorVars([ \"a\", \"b\" ])"
gap> Print(FreeSemigroup("a", "b"), "\n");
FreeSemigroupAndAssignGeneratorVars([ "a", "b" ])

# String + Print for FpMonoid/Semigroup
gap> S := RightZeroSemigroup(2);
<transformation semigroup of degree 2 with 2 generators>
gap> S := Range(IsomorphismFpSemigroup(S));
<fp semigroup with 2 generators and 4 relations of length 14>
gap> String(S);
"FreeSemigroupAndAssignGeneratorVars([ \"s1\", \"s2\" ])\> / \<\>[ [ s1^2, s1 \
], [ s1*s2, s2 ], [ s2*s1, s1 ], [ s2^2, s2 ] ]\<"
gap> Print(S, "\n");
FreeSemigroupAndAssignGeneratorVars([ "s1", "s2" ]) / 
[ [ s1^2, s1 ], [ s1*s2, s2 ], [ s2*s1, s1 ], [ s2^2, s2 ] ]
gap> FreeSemigroupAndAssignGeneratorVars(["s1", "s2"]) / 
> [[s1 ^ 2, s1], [s1 * s2, s2], [s2 * s1, s1], [s2 ^ 2, s2]];
<fp semigroup with 2 generators and 4 relations of length 14>
gap> S := Semigroup(RightZeroSemigroup(2), IdentityTransformation);
<transformation monoid of degree 2 with 2 generators>
gap> S := Range(IsomorphismFpMonoid(S));
<fp monoid with 2 generators and 4 relations of length 14>
gap> String(S);
"FreeMonoidAndAssignGeneratorVars([ \"m1\", \"m2\" ])\> / \<\>[ [ m1^2, m1 ], \
[ m1*m2, m2 ], [ m2*m1, m1 ], [ m2^2, m2 ] ]\<"
gap> Print(S, "\n");
FreeMonoidAndAssignGeneratorVars([ "m1", "m2" ]) / 
[ [ m1^2, m1 ], [ m1*m2, m2 ], [ m2*m1, m1 ], [ m2^2, m2 ] ]
gap> FreeMonoidAndAssignGeneratorVars(["m1", "m2"]) / 
> [[m1 ^ 2, m1], [m1 * m2, m2], [m2 * m1, m1], [m2 ^ 2, m2]];
<fp monoid with 2 generators and 4 relations of length 14>
gap> Print(AsMonoid(IsFpMonoid, SymmetricInverseMonoid(2)), "\n");
FreeMonoidAndAssignGeneratorVars([ "m1", "m2" ]) / 
[ [ m1^2, One(m1) ], [ m2^2, m2 ], [ (m1*m2)^2, m2*m1*m2 ], [ (m2*m1)^2, m2*m1\
*m2 ] ]
gap> FreeMonoidAndAssignGeneratorVars(["m1", "m2"]) /
> [[m1 ^ 2, One(m1)], [m2 ^ 2, m2], [(m1 * m2) ^ 2, m2 * m1 * m2], 
> [(m2 * m1) ^ 2, m2 * m1 * m2]];
<fp monoid with 2 generators and 4 relations of length 21>

# IsomorphismFpSemigroup for a free semigroup/monoid
gap> IsomorphismFpSemigroup(FreeSemigroup(2)); 
<free semigroup on the generators [ s1, s2 ]> -> 
<fp semigroup with 2 generators and 0 relations of length 2>
gap> IsomorphismFpMonoid(FreeMonoid(2)); 
<free monoid on the generators [ m1, m2 ]> -> 
<fp monoid with 2 generators and 0 relations of length 2>
gap> IsomorphismFpSemigroup(FreeMonoid(2)); 
CompositionMapping( <fp monoid with 2 generators and 0 relations of length 2> 
-> <fp semigroup with 3 generators and 5 relations of length 18>, 
<free monoid on the generators [ m1, m2 ]> -> 
<fp monoid with 2 generators and 0 relations of length 2> )

# Factorization for an fp monoid
gap> F := FreeMonoid(2);
<free monoid on the generators [ m1, m2 ]>
gap> F := Range(IsomorphismFpMonoid(F));
<fp monoid with 2 generators and 0 relations of length 2>
gap> w := Factorization(F, F.1 * F.2 * One(F));
[ 2, 3 ]
gap> EvaluateWord(GeneratorsOfSemigroup(F), w) = F.1 * F.2 * One(F);
true
gap> w := Factorization(F, One(F));
[ 1 ]
gap> EvaluateWord(GeneratorsOfSemigroup(F), w) = One(F);
true

# Reversed for elements of a fp semigroup/monoid
gap> F := FreeSemigroup("a", "b");
<free semigroup on the generators [ a, b ]>
gap> AssignGeneratorVariables(F);
gap> R := [[a ^ 3, a], [b ^ 2, b], [(a * b) ^ 2, a]];
[ [ a^3, a ], [ b^2, b ], [ (a*b)^2, a ] ]
gap> S := F / R;
<fp semigroup with 2 generators and 3 relations of length 14>
gap> Reversed(a * b * b);
b^2*a
gap> Reversed(a * b * b * a);
a*b^2*a
gap> Reversed(a * b * b) = b * b * a;
true
gap> Reversed(a * b * b * a) = a * b * b * a;
true
gap> F := FreeMonoid("a", "b");
<free monoid on the generators [ a, b ]>
gap> AssignGeneratorVariables(F);
gap> R := ParseRelations([a, b], "a ^ 3=a, b ^ 2= b, (ab) ^ 2= 1");
[ [ a^3, a ], [ b^2, b ], [ (a*b)^2, <identity ...> ] ]
gap> S := F / R;
<fp monoid with 2 generators and 3 relations of length 13>
gap> Reversed(a * b * b) = b * b * a;
true
gap> Reversed(a * b * b * a) = a * b * b * a;
true
gap> Reversed(a * b * b);
b^2*a
gap> Reversed(a * b * b * a);
a*b^2*a
gap> Reversed(One(S));
<identity ...>

# AntiIsomorphismDualFpMonoid/Semigroup
gap> F := FreeSemigroup("a", "b");
<free semigroup on the generators [ a, b ]>
gap> AssignGeneratorVariables(F);
gap> R := [[a ^ 3, a], [b ^ 2, b], [(a * b) ^ 2, a]];
[ [ a^3, a ], [ b^2, b ], [ (a*b)^2, a ] ]
gap> S := F / R;
<fp semigroup with 2 generators and 3 relations of length 14>
gap> map := AntiIsomorphismDualFpSemigroup(S);
MappingByFunction( <fp semigroup with 2 generators and 
  3 relations of length 14>, <fp semigroup with 2 generators and 
  3 relations of length 14>, function( x ) ... end, function( x ) ... end )
gap> RelationsOfFpSemigroup(Range(map));
[ [ a^3, a ], [ b^2, b ], [ (b*a)^2, a ] ]
gap> F := FreeMonoid("a", "b");
<free monoid on the generators [ a, b ]>
gap> AssignGeneratorVariables(F);
gap> R := [[a ^ 3, One(F)], [b ^ 2, One(F)], [(a * b) ^ 2, One(F)]];
[ [ a^3, <identity ...> ], [ b^2, <identity ...> ], 
  [ (a*b)^2, <identity ...> ] ]
gap> S := F / R;
<fp monoid with 2 generators and 3 relations of length 11>
gap> map := AntiIsomorphismDualFpMonoid(S);
MappingByFunction( <fp monoid with 2 generators and 3 relations of length 11>
 , <fp monoid with 2 generators and 3 relations of length 11>
 , function( x ) ... end, function( x ) ... end )
gap> RelationsOfFpMonoid(Range(map));
[ [ a^3, <identity ...> ], [ b^2, <identity ...> ], 
  [ (b*a)^2, <identity ...> ] ]

# EmbeddingFpMonoid
gap> F := FreeSemigroup("a", "b");
<free semigroup on the generators [ a, b ]>
gap> AssignGeneratorVariables(F);
gap> R := [[a ^ 3, a], [b ^ 2, b], [(a * b) ^ 2, a]];
[ [ a^3, a ], [ b^2, b ], [ (a*b)^2, a ] ]
gap> S := F / R;
<fp semigroup with 2 generators and 3 relations of length 14>
gap> Size(S);
3
gap> IsMonoidAsSemigroup(S);
false
gap> map := EmbeddingFpMonoid(S);
<fp semigroup with 2 generators and 3 relations of length 14> -> 
<fp monoid with 2 generators and 3 relations of length 14>
gap> Range(map) = Image(map);
false
gap> Size(Image(map));
3
gap> Size(Range(map));
4

# EmbeddingFpMonoid
gap> F := FreeMonoid("a", "b");
<free monoid on the generators [ a, b ]>
gap> AssignGeneratorVariables(F);
gap> R := [[a ^ 3, a], [b ^ 2, b], [(a * b) ^ 2, a]];
[ [ a^3, a ], [ b^2, b ], [ (a*b)^2, a ] ]
gap> S := F / R;
<fp monoid with 2 generators and 3 relations of length 14>
gap> S := Range(IsomorphismFpSemigroup(S));
<fp semigroup with 3 generators and 8 relations of length 30>
gap> Size(S);
4
gap> IsMonoidAsSemigroup(S);
true
gap> map := EmbeddingFpMonoid(S);
<fp semigroup with 3 generators and 8 relations of length 30> -> 
<fp monoid with 2 generators and 3 relations of length 11>
gap> Range(map) = Image(map);
true
gap> Size(Images(map));
4
gap> Size(Range(map));
4

# IsSubsemigroupOfFpMonoid
gap> F := FreeSemigroup("a", "b");
<free semigroup on the generators [ a, b ]>
gap> AssignGeneratorVariables(F);
gap> R := [[a ^ 3, a], [b ^ 2, b], [(a * b) ^ 2, a]];
[ [ a^3, a ], [ b^2, b ], [ (a*b)^2, a ] ]
gap> S := F / R;
<fp semigroup with 2 generators and 3 relations of length 14>
gap> IsSubsemigroupOfFpMonoid(S);
false
gap> map := EmbeddingFpMonoid(S);
<fp semigroup with 2 generators and 3 relations of length 14> -> 
<fp monoid with 2 generators and 3 relations of length 14>
gap> IsSubsemigroupOfFpMonoid(Image(map));
true
gap> IsSubsemigroupOfFpMonoid(Range(map));
true

# Unbind local variables, auto-generated by etc/tst-unbind-local-vars.py
gap> Unbind(BruteForceInverseCheck);
gap> Unbind(BruteForceIsoCheck);
gap> Unbind(F);
gap> Unbind(G);
gap> Unbind(LoopIterator);
gap> Unbind(Noop);
gap> Unbind(R);
gap> Unbind(S);
gap> Unbind(T);
gap> Unbind(TestEnumerator);
gap> Unbind(TestIterator);
gap> Unbind(a);
gap> Unbind(b);
gap> Unbind(f);
gap> Unbind(factorizable);
gap> Unbind(inv);
gap> Unbind(iso);
gap> Unbind(len);
gap> Unbind(map);
gap> Unbind(rels);
gap> Unbind(s);
gap> Unbind(tst);
gap> Unbind(valid);
gap> Unbind(w);
gap> Unbind(x);
gap> Unbind(y);

#
gap> STOP_TEST("Semigroups package: standard/semigroups/semifp.tst");
