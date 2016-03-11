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

#T# attributes: IsomorphismFpMonoid, 
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

#T# attributes: IsomorphismFpMonoid, infinite
gap> IsomorphismFpMonoid(FreeMonoid(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `IsomorphismFpMonoid' on 1 arguments

#T# attributes: IsomorphismFpSemigroup, infinite
gap> IsomorphismFpSemigroup(FreeInverseSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `IsomorphismFpSemigroup' on 1 arguments

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

#T# AsMonoid: 
#   convert from IsPBRSemigroup to IsFpMonoid
gap> S := Semigroup( [ PBR([ [ -1 ], [ -2 ], [ -2 ], [ -2 ], [ -2 ] ], [ [ 1 ], [ 2, 3, 4, 5 ], [ ], [ ], [ ] ]), PBR([ [ -2 ], [ -1 ], [ -1 ], [ -1 ], [ -1 ] ], [ [ 2, 3, 4, 5 ], [ 1 ], [ ], [ ], [ ] ]) ] );
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

#T# AsMonoid: 
#   convert from IsFpSemigroup to IsFpMonoid
gap> F := FreeSemigroup(2);; AssignGeneratorVariables(F);;
gap> rels := [ [ s1^2, s1 ], [ s1*s2, s2 ], [ s2*s1, s2 ], [ s2^2, s1 ] ];;
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

#T# AsMonoid: 
#   convert from IsBipartitionSemigroup to IsFpMonoid
gap> S := Semigroup( [ Bipartition([ [ 1, -1 ], [ 2, 3, 4, 5, -2 ], [ -3 ], [ -4 ], [ -5 ] ]), Bipartition([ [ 1, -2 ], [ 2, 3, 4, 5, -1 ], [ -3 ], [ -4 ], [ -5 ] ]) ] );
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

#T# AsMonoid: 
#   convert from IsTransformationSemigroup to IsFpMonoid
gap> S := Semigroup( [ Transformation( [ 1, 2, 2, 2, 2 ] ), Transformation( [ 2, 1, 1, 1, 1 ] ) ] );
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

#T# AsMonoid: 
#   convert from IsBooleanMatSemigroup to IsFpMonoid
gap> S := Semigroup( [ Matrix(IsBooleanMat, [ [ true, false, false, false, false ], [ false, true, false, false, false ], [ false, true, false, false, false ], [ false, true, false, false, false ], [ false, true, false, false, false ] ]), Matrix(IsBooleanMat, [ [ false, true, false, false, false ], [ true, false, false, false, false ], [ true, false, false, false, false ], [ true, false, false, false, false ], [ true, false, false, false, false ] ]) ] );
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

#T# AsMonoid: 
#   convert from IsMaxPlusMatrixSemigroup to IsFpMonoid
gap> S := Semigroup( [ Matrix(IsMaxPlusMatrix, [ [ 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ] ]), Matrix(IsMaxPlusMatrix, [ [ -infinity, 0, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ] ]) ] );
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

#T# AsMonoid: 
#   convert from IsMinPlusMatrixSemigroup to IsFpMonoid
gap> S := Semigroup( [ Matrix(IsMinPlusMatrix, [ [ 0, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity ] ]), Matrix(IsMinPlusMatrix, [ [ infinity, 0, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity ] ]) ] );
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

#T# AsMonoid: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsFpMonoid
gap> S := Semigroup( [ Matrix(IsProjectiveMaxPlusMatrix, [ [ 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ] ]), Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, 0, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ] ]) ] );
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

#T# AsMonoid: 
#   convert from IsIntegerMatrixSemigroup to IsFpMonoid
gap> S := Semigroup( [ Matrix(IsIntegerMatrix, [ [ 1, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ] ]), Matrix(IsIntegerMatrix, [ [ 0, 1, 0, 0, 0 ], [ 1, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0 ] ]) ] );
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

#T# AsMonoid: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsFpMonoid
gap> S := Semigroup( [ Matrix(IsTropicalMaxPlusMatrix, [ [ 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ] ], 3), Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, 0, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ] ], 3) ] );
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

#T# AsMonoid: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsFpMonoid
gap> S := Semigroup( [ Matrix(IsTropicalMinPlusMatrix, [ [ 0, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity ] ], 5), Matrix(IsTropicalMinPlusMatrix, [ [ infinity, 0, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity ] ], 5) ] );
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

#T# AsMonoid: 
#   convert from IsNTPMatrixSemigroup to IsFpMonoid
gap> S := Semigroup( [ Matrix(IsNTPMatrix, [ [ 1, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ] ], 5, 1), Matrix(IsNTPMatrix, [ [ 0, 1, 0, 0, 0 ], [ 1, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0 ] ], 5, 1) ] );
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

#T# AsMonoid: 
#   convert from IsPBRMonoid to IsFpMonoid
gap> S := Monoid( [ PBR([ [ -3 ], [ -2 ], [ -3 ] ], [ [ ], [ 2 ], [ 1, 3 ] ]), PBR([ [ -2 ], [ -2 ], [ -1 ] ], [ [ 3 ], [ 1, 2 ], [ ] ]) ] );
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

#T# AsMonoid: 
#   convert from IsFpMonoid to IsFpMonoid
gap> F := FreeMonoid(2);; AssignGeneratorVariables(F);;
gap> rels := [ [ m1^2, m1 ], [ m1*m2*m1, m1 ], [ m1*m2^2, m2^2 ], [ m2*m1*m2, m2 ], [ m2^2*m1, m2^2 ], [ m2^3, m2^2 ] ];;
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

#T# AsMonoid: 
#   convert from IsBipartitionMonoid to IsFpMonoid
gap> S := Monoid( [ Bipartition([ [ 1, 3, -3 ], [ 2, -2 ], [ -1 ] ]), Bipartition([ [ 1, 2, -2 ], [ 3, -1 ], [ -3 ] ]) ] );
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

#T# AsMonoid: 
#   convert from IsTransformationMonoid to IsFpMonoid
gap> S := Monoid( [ Transformation( [ 3, 2, 3 ] ), Transformation( [ 2, 2, 1 ] ) ] );
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

#T# AsMonoid: 
#   convert from IsBooleanMatMonoid to IsFpMonoid
gap> S := Monoid( [ Matrix(IsBooleanMat, [ [ false, false, true ], [ false, true, false ], [ false, false, true ] ]), Matrix(IsBooleanMat, [ [ false, true, false ], [ false, true, false ], [ true, false, false ] ]) ] );
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

#T# AsMonoid: 
#   convert from IsMaxPlusMatrixMonoid to IsFpMonoid
gap> S := Monoid( [ Matrix(IsMaxPlusMatrix, [ [ -infinity, -infinity, 0 ], [ -infinity, 0, -infinity ], [ -infinity, -infinity, 0 ] ]), Matrix(IsMaxPlusMatrix, [ [ -infinity, 0, -infinity ], [ -infinity, 0, -infinity ], [ 0, -infinity, -infinity ] ]) ] );
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

#T# AsMonoid: 
#   convert from IsMinPlusMatrixMonoid to IsFpMonoid
gap> S := Monoid( [ Matrix(IsMinPlusMatrix, [ [ infinity, infinity, 0 ], [ infinity, 0, infinity ], [ infinity, infinity, 0 ] ]), Matrix(IsMinPlusMatrix, [ [ infinity, 0, infinity ], [ infinity, 0, infinity ], [ 0, infinity, infinity ] ]) ] );
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

#T# AsMonoid: 
#   convert from IsProjectiveMaxPlusMatrixMonoid to IsFpMonoid
gap> S := Monoid( [ Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, -infinity, 0 ], [ -infinity, 0, -infinity ], [ -infinity, -infinity, 0 ] ]), Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, 0, -infinity ], [ -infinity, 0, -infinity ], [ 0, -infinity, -infinity ] ]) ] );
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

#T# AsMonoid: 
#   convert from IsIntegerMatrixMonoid to IsFpMonoid
gap> S := Monoid( [ Matrix(IsIntegerMatrix, [ [ 0, 0, 1 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ]), Matrix(IsIntegerMatrix, [ [ 0, 1, 0 ], [ 0, 1, 0 ], [ 1, 0, 0 ] ]) ] );
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

#T# AsMonoid: 
#   convert from IsTropicalMaxPlusMatrixMonoid to IsFpMonoid
gap> S := Monoid( [ Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, -infinity, 0 ], [ -infinity, 0, -infinity ], [ -infinity, -infinity, 0 ] ], 2), Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, 0, -infinity ], [ -infinity, 0, -infinity ], [ 0, -infinity, -infinity ] ], 2) ] );
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

#T# AsMonoid: 
#   convert from IsTropicalMinPlusMatrixMonoid to IsFpMonoid
gap> S := Monoid( [ Matrix(IsTropicalMinPlusMatrix, [ [ infinity, infinity, 0 ], [ infinity, 0, infinity ], [ infinity, infinity, 0 ] ], 2), Matrix(IsTropicalMinPlusMatrix, [ [ infinity, 0, infinity ], [ infinity, 0, infinity ], [ 0, infinity, infinity ] ], 2) ] );
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

#T# AsMonoid: 
#   convert from IsNTPMatrixMonoid to IsFpMonoid
gap> S := Monoid( [ Matrix(IsNTPMatrix, [ [ 0, 0, 1 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ], 1, 4), Matrix(IsNTPMatrix, [ [ 0, 1, 0 ], [ 0, 1, 0 ], [ 1, 0, 0 ] ], 1, 4) ] );
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

#T# AsSemigroup: 
#   convert from IsReesMatrixSemigroup to IsFpSemigroup
gap> R := ReesMatrixSemigroup(Group([(1, 2)]), [[(1, 2), (1, 2)], [(), ()]]);
<Rees matrix semigroup 2x2 over Group([ (1,2) ])>
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

#T# AsMonoid
#   convert from IsReesMatrixSemigroup to IsFpMonoid
gap> R := ReesMatrixSemigroup(Group([(1, 2)]), [[(1, 2)]]);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
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

#T# AsSemigroup: 
#   convert from IsReesZeroMatrixSemigroup to IsFpSemigroup
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2)]), 
>                                 [[(1, 2), (1, 2)], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
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

#T# AsMonoid
#   convert from IsReesZeroMatrixSemigroup to IsFpMonoid
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[(1, 2)]]);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
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

#T# AsSemigroup: 
#   convert from graph inverse to IsFpSemigroup
gap> S := GraphInverseSemigroup(Digraph([[2],[]]));
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

#T# AsSemigroup: 
#   convert from perm group to IsFpSemigroup
gap> S := DihedralGroup(IsPermGroup, 6);
Group([ (1,2,3), (2,3) ])
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
#   convert from perm group to IsFpMonoid
gap> S := DihedralGroup(IsPermGroup, 6);
Group([ (1,2,3), (2,3) ])
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

#T# AsSemigroup: 
#   convert from non-perm group to IsFpSemigroup
gap> S := DihedralGroup(6);
<pc group of size 6 with 2 generators>
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
#   convert from non-perm group to IsFpMonoid
gap> S := DihedralGroup(6);
<pc group of size 6 with 2 generators>
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

#T# AsSemigroup: 
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

#T# AsSemigroup: 
#   convert from IsBlockBijectionMonoid to IsFpMonoid
gap> S := InverseMonoid([Bipartition([[1, -1, -3], [2, 3, -2]])]);;
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

#T# AsSemigroup: 
#   convert from IsBlockBijectionMonoid to IsFpSemigroup
gap> S := InverseMonoid([Bipartition([[1, -1, -3], [2, 3, -2]])]);;
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

#T# AsSemigroup: 
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

#T# AsSemigroup: 
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

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);

#E#
gap> STOP_TEST("Semigroups package: standard/semifp.tst");
