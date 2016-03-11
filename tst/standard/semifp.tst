#############################################################################
##
##  standard/semifp.tst
#Y  Copyright (C) 2015                                   Wilfred A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semifp.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

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

#T# AsFpSemigroup 1: trivial semigroup
gap> S := TrivialSemigroup();
<trivial transformation group of degree 0 with 1 generator>
gap> S := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1 ]>
gap> RelationsOfFpSemigroup(S);
[ [ s1^2, s1 ] ]

#T# AsFpSemigroup 2: 2 element semilattice
gap> S := Semigroup(PartialPerm([1]), PartialPerm([], []));
<partial perm monoid of rank 1 with 2 generators>
gap> S := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1, s2 ]>
gap> RelationsOfFpSemigroup(S);
[ [ s1^2, s1 ], [ s1*s2, s2 ], [ s2*s1, s2 ], [ s2^2, s2 ] ]

#T# AsFpMonoid 1: trivial semigroup
gap> S := TrivialSemigroup();
<trivial transformation group of degree 0 with 1 generator>
gap> S := AsMonoid(IsFpMonoid, S);
<fp group on the generators [  ]>

#T# AsFpMonoid 2: 2 element semilattice
gap> S := Semigroup(PartialPerm([1]), PartialPerm([], []));
<partial perm monoid of rank 1 with 2 generators>
gap> S := AsMonoid(IsFpMonoid, S);
<fp monoid on the generators [ m1 ]>
gap> RelationsOfFpMonoid(S);
[ [ m1^2, m1 ] ]

#T# AsSemigroup: 
#   convert from IsPBRSemigroup to IsFpSemigroup
gap> S := Semigroup( [ PBR([ [ -1 ], [ -3 ], [ -3 ] ], [ [ 1 ], [ ], [ 2, 3 ] ]), PBR([ [ -2 ], [ -3 ], [ -3 ] ], [ [ ], [ 1 ], [ 2, 3 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsFpSemigroup to IsFpSemigroup
gap> F := FreeSemigroup(2);; AssignGeneratorVariables(F);;
gap> rels := [ [ s1^2, s1 ], [ s1*s2, s2 ], [ s2^2, s2*s1 ] ];;
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

#T# AsSemigroup: 
#   convert from IsBipartitionSemigroup to IsFpSemigroup
gap> S := Semigroup( [ Bipartition([ [ 1, -1 ], [ 2 ], [ -2 ] ]), Bipartition([ [ 1, -2 ], [ 2 ], [ -1 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsTransformationSemigroup to IsFpSemigroup
gap> S := Semigroup( [ Transformation( [ 1, 3, 3 ] ), Transformation( [ 2, 3, 3 ] ) ] );
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

#T# AsSemigroup: 
#   convert from IsBooleanMatSemigroup to IsFpSemigroup
gap> S := Semigroup( [ Matrix(IsBooleanMat, [ [ true, false, false ], [ false, false, true ], [ false, false, true ] ]), Matrix(IsBooleanMat, [ [ false, true, false ], [ false, false, true ], [ false, false, true ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsMaxPlusMatrixSemigroup to IsFpSemigroup
gap> S := Semigroup( [ Matrix(IsMaxPlusMatrix, [ [ 0, -infinity, -infinity ], [ -infinity, -infinity, 0 ], [ -infinity, -infinity, 0 ] ]), Matrix(IsMaxPlusMatrix, [ [ -infinity, 0, -infinity ], [ -infinity, -infinity, 0 ], [ -infinity, -infinity, 0 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsMinPlusMatrixSemigroup to IsFpSemigroup
gap> S := Semigroup( [ Matrix(IsMinPlusMatrix, [ [ 0, infinity, infinity ], [ infinity, infinity, 0 ], [ infinity, infinity, 0 ] ]), Matrix(IsMinPlusMatrix, [ [ infinity, 0, infinity ], [ infinity, infinity, 0 ], [ infinity, infinity, 0 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsFpSemigroup
gap> S := Semigroup( [ Matrix(IsProjectiveMaxPlusMatrix, [ [ 0, -infinity, -infinity ], [ -infinity, -infinity, 0 ], [ -infinity, -infinity, 0 ] ]), Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, 0, -infinity ], [ -infinity, -infinity, 0 ], [ -infinity, -infinity, 0 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsIntegerMatrixSemigroup to IsFpSemigroup
gap> S := Semigroup( [ Matrix(IsIntegerMatrix, [ [ 1, 0, 0 ], [ 0, 0, 1 ], [ 0, 0, 1 ] ]), Matrix(IsIntegerMatrix, [ [ 0, 1, 0 ], [ 0, 0, 1 ], [ 0, 0, 1 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsFpSemigroup
gap> S := Semigroup( [ Matrix(IsTropicalMaxPlusMatrix, [ [ 0, -infinity, -infinity ], [ -infinity, -infinity, 0 ], [ -infinity, -infinity, 0 ] ], 3), Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, 0, -infinity ], [ -infinity, -infinity, 0 ], [ -infinity, -infinity, 0 ] ], 3) ] );
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

#T# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsFpSemigroup
gap> S := Semigroup( [ Matrix(IsTropicalMinPlusMatrix, [ [ 0, infinity, infinity ], [ infinity, infinity, 0 ], [ infinity, infinity, 0 ] ], 3), Matrix(IsTropicalMinPlusMatrix, [ [ infinity, 0, infinity ], [ infinity, infinity, 0 ], [ infinity, infinity, 0 ] ], 3) ] );
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

#T# AsSemigroup: 
#   convert from IsNTPMatrixSemigroup to IsFpSemigroup
gap> S := Semigroup( [ Matrix(IsNTPMatrix, [ [ 1, 0, 0 ], [ 0, 0, 1 ], [ 0, 0, 1 ] ], 3, 4), Matrix(IsNTPMatrix, [ [ 0, 1, 0 ], [ 0, 0, 1 ], [ 0, 0, 1 ] ], 3, 4) ] );
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

#T# AsSemigroup: 
#   convert from IsPBRMonoid to IsFpSemigroup
gap> S := Monoid( [ PBR([ [ -2 ], [ -2 ], [ -3 ] ], [ [ ], [ 1, 2 ], [ 3 ] ]), PBR([ [ -1 ], [ -3 ], [ -3 ] ], [ [ 1 ], [ ], [ 2, 3 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsFpMonoid to IsFpSemigroup
gap> F := FreeMonoid(2);; AssignGeneratorVariables(F);;
gap> rels := [ [ m1^2, m1 ], [ m2^2, m2 ], [ m1*m2*m1, m1*m2 ], [ m2*m1*m2, m1*m2 ] ];;
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

#T# AsSemigroup: 
#   convert from IsBipartitionMonoid to IsFpSemigroup
gap> S := Monoid( [ Bipartition([ [ 1, 2, -2 ], [ 3, -3 ], [ -1 ] ]), Bipartition([ [ 1, -1 ], [ 2, 3, -3 ], [ -2 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsTransformationMonoid to IsFpSemigroup
gap> S := Monoid( [ Transformation( [ 2, 2 ] ), Transformation( [ 1, 3, 3 ] ) ] );
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

#T# AsSemigroup: 
#   convert from IsBooleanMatMonoid to IsFpSemigroup
gap> S := Monoid( [ Matrix(IsBooleanMat, [ [ false, true, false ], [ false, true, false ], [ false, false, true ] ]), Matrix(IsBooleanMat, [ [ true, false, false ], [ false, false, true ], [ false, false, true ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsMaxPlusMatrixMonoid to IsFpSemigroup
gap> S := Monoid( [ Matrix(IsMaxPlusMatrix, [ [ -infinity, 0, -infinity ], [ -infinity, 0, -infinity ], [ -infinity, -infinity, 0 ] ]), Matrix(IsMaxPlusMatrix, [ [ 0, -infinity, -infinity ], [ -infinity, -infinity, 0 ], [ -infinity, -infinity, 0 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsMinPlusMatrixMonoid to IsFpSemigroup
gap> S := Monoid( [ Matrix(IsMinPlusMatrix, [ [ infinity, 0, infinity ], [ infinity, 0, infinity ], [ infinity, infinity, 0 ] ]), Matrix(IsMinPlusMatrix, [ [ 0, infinity, infinity ], [ infinity, infinity, 0 ], [ infinity, infinity, 0 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixMonoid to IsFpSemigroup
gap> S := Monoid( [ Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, 0, -infinity ], [ -infinity, 0, -infinity ], [ -infinity, -infinity, 0 ] ]), Matrix(IsProjectiveMaxPlusMatrix, [ [ 0, -infinity, -infinity ], [ -infinity, -infinity, 0 ], [ -infinity, -infinity, 0 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsIntegerMatrixMonoid to IsFpSemigroup
gap> S := Monoid( [ Matrix(IsIntegerMatrix, [ [ 0, 1, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ]), Matrix(IsIntegerMatrix, [ [ 1, 0, 0 ], [ 0, 0, 1 ], [ 0, 0, 1 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixMonoid to IsFpSemigroup
gap> S := Monoid( [ Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, 0, -infinity ], [ -infinity, 0, -infinity ], [ -infinity, -infinity, 0 ] ], 4), Matrix(IsTropicalMaxPlusMatrix, [ [ 0, -infinity, -infinity ], [ -infinity, -infinity, 0 ], [ -infinity, -infinity, 0 ] ], 4) ] );
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

#T# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixMonoid to IsFpSemigroup
gap> S := Monoid( [ Matrix(IsTropicalMinPlusMatrix, [ [ infinity, 0, infinity ], [ infinity, 0, infinity ], [ infinity, infinity, 0 ] ], 2), Matrix(IsTropicalMinPlusMatrix, [ [ 0, infinity, infinity ], [ infinity, infinity, 0 ], [ infinity, infinity, 0 ] ], 2) ] );
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

#T# AsSemigroup: 
#   convert from IsNTPMatrixMonoid to IsFpSemigroup
gap> S := Monoid( [ Matrix(IsNTPMatrix, [ [ 0, 1, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ], 1, 1), Matrix(IsNTPMatrix, [ [ 1, 0, 0 ], [ 0, 0, 1 ], [ 0, 0, 1 ] ], 1, 1) ] );
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

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);

#E#
gap> STOP_TEST("Semigroups package: standard/semifp.tst");
