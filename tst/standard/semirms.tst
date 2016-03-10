############################################################################
##
#W  standard/semirms.tst
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semirms.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();;

#T# helper functions
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

#T# AsSemigroup: 
#   convert from IsPBRSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup( [ PBR([ [ -1 ], [ -5 ], [ -1 ], [ -1 ], [ -5 ], [ -5 ], [ -1 ] ], [ [ 1, 3, 4, 7 ], [ ], [ ], [ ], [ 2, 5, 6 ], [ ], [ ] ]), PBR([ [ -4 ], [ -2 ], [ -4 ], [ -4 ], [ -2 ], [ -2 ], [ -2 ] ], [ [ ], [ 2, 5, 6, 7 ], [ ], [ 1, 3, 4 ], [ ], [ ], [ ] ]), PBR([ [ -3 ], [ -6 ], [ -3 ], [ -3 ], [ -6 ], [ -6 ], [ -3 ] ], [ [ ], [ ], [ 1, 3, 4, 7 ], [ ], [ ], [ 2, 5, 6 ], [ ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsFpSemigroup to IsReesMatrixSemigroup
gap> F := FreeSemigroup(3);; AssignGeneratorVariables(F);;
gap> rels := [ [ s1^2, s1 ], [ s1*s3, s3 ], [ s2^2, s2 ], [ s3*s1, s1 ], [ s3*s2, s1*s2 ], [ s3^2, s3 ], [ s1*s2*s1, s1 ], [ s1*s2*s3, s3 ], [ s2*s1*s2, s2 ] ];;
gap> S := F / rels;
<fp semigroup on the generators [ s1, s2, s3 ]>
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

#T# AsSemigroup: 
#   convert from IsBipartitionSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup( [ Bipartition([ [ 1, 3, 4, 7, -1 ], [ 2, 5, 6, -5 ], [ -2 ], [ -3 ], [ -4 ], [ -6 ], [ -7 ] ]), Bipartition([ [ 1, 3, 4, -4 ], [ 2, 5, 6, 7, -2 ], [ -1 ], [ -3 ], [ -5 ], [ -6 ], [ -7 ] ]), Bipartition([ [ 1, 3, 4, 7, -3 ], [ 2, 5, 6, -6 ], [ -1 ], [ -2 ], [ -4 ], [ -5 ], [ -7 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsTransformationSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup( [ Transformation( [ 1, 5, 1, 1, 5, 5, 1 ] ), Transformation( [ 4, 2, 4, 4, 2, 2, 2 ] ), Transformation( [ 3, 6, 3, 3, 6, 6, 3 ] ) ] );
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

#T# AsSemigroup: 
#   convert from IsBooleanMatSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsBooleanMat, [ [ true, false, false, false, false, false, false ], [ false, false, false, false, true, false, false ], [ true, false, false, false, false, false, false ], [ true, false, false, false, false, false, false ], [ false, false, false, false, true, false, false ], [ false, false, false, false, true, false, false ], [ true, false, false, false, false, false, false ] ]), Matrix(IsBooleanMat, [ [ false, false, false, true, false, false, false ], [ false, true, false, false, false, false, false ], [ false, false, false, true, false, false, false ], [ false, false, false, true, false, false, false ], [ false, true, false, false, false, false, false ], [ false, true, false, false, false, false, false ], [ false, true, false, false, false, false, false ] ]), Matrix(IsBooleanMat, [ [ false, false, true, false, false, false, false ], [ false, false, false, false, false, true, false ], [ false, false, true, false, false, false, false ], [ false, false, true, false, false, false, false ], [ false, false, false, false, false, true, false ], [ false, false, false, false, false, true, false ], [ false, false, true, false, false, false, false ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsMaxPlusMatrixSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsMaxPlusMatrix, [ [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ]), Matrix(IsMaxPlusMatrix, [ [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ] ]), Matrix(IsMaxPlusMatrix, [ [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsMinPlusMatrixSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsMinPlusMatrix, [ [ 0, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity, infinity, infinity ] ]), Matrix(IsMinPlusMatrix, [ [ infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity, infinity ] ]), Matrix(IsMinPlusMatrix, [ [ infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity, infinity ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsProjectiveMaxPlusMatrix, [ [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ]), Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ] ]), Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsIntegerMatrixSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsIntegerMatrix, [ [ 1, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0 ], [ 1, 0, 0, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0 ], [ 1, 0, 0, 0, 0, 0, 0 ] ]), Matrix(IsIntegerMatrix, [ [ 0, 0, 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0, 0 ] ]), Matrix(IsIntegerMatrix, [ [ 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0 ], [ 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0 ], [ 0, 0, 0, 0, 0, 1, 0 ], [ 0, 0, 1, 0, 0, 0, 0 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsTropicalMaxPlusMatrix, [ [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ], 1), Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ] ], 1), Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ] ], 1) ] );
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

#T# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsTropicalMinPlusMatrix, [ [ 0, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity, infinity, infinity ] ], 3), Matrix(IsTropicalMinPlusMatrix, [ [ infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity, infinity ] ], 3), Matrix(IsTropicalMinPlusMatrix, [ [ infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity, infinity ] ], 3) ] );
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

#T# AsSemigroup: 
#   convert from IsNTPMatrixSemigroup to IsReesMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsNTPMatrix, [ [ 1, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0 ], [ 1, 0, 0, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0 ], [ 1, 0, 0, 0, 0, 0, 0 ] ], 4, 1), Matrix(IsNTPMatrix, [ [ 0, 0, 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0, 0 ] ], 4, 1), Matrix(IsNTPMatrix, [ [ 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0 ], [ 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0 ], [ 0, 0, 0, 0, 0, 1, 0 ], [ 0, 0, 1, 0, 0, 0, 0 ] ], 4, 1) ] );
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

#T# AsSemigroup: 
#   convert from IsPBRSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup( [ PBR([ [ -4 ], [ -5 ], [ -3 ], [ -1 ], [ -2 ], [ -1 ] ], [ [ 4, 6 ], [ 5 ], [ 3 ], [ 1 ], [ 2 ], [ ] ]), PBR([ [ -1 ], [ -2 ], [ -3 ], [ -4 ], [ -5 ], [ -2 ] ], [ [ 1 ], [ 2, 6 ], [ 3 ], [ 4 ], [ 5 ], [ ] ]), PBR([ [ -3 ], [ -3 ], [ -3 ], [ -3 ], [ -3 ], [ -3 ] ], [ [ ], [ ], [ 1, 2, 3, 4, 5, 6 ], [ ], [ ], [ ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsFpSemigroup to IsReesZeroMatrixSemigroup
gap> F := FreeSemigroup(3);; AssignGeneratorVariables(F);;
gap> rels := [ [ s1*s2, s1 ], [ s1*s3, s3 ], [ s2^2, s2 ], [ s2*s3, s3 ], [ s3*s1, s3 ], [ s3*s2, s3 ], [ s3^2, s3 ], [ s1^3, s1 ], [ s2*s1^2, s2 ] ];;
gap> S := F / rels;
<fp semigroup on the generators [ s1, s2, s3 ]>
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

#T# AsSemigroup: 
#   convert from IsBipartitionSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup( [ Bipartition([ [ 1, -4 ], [ 2, -5 ], [ 3, -3 ], [ 4, 6, -1 ], [ 5, -2 ], [ -6 ] ]), Bipartition([ [ 1, -1 ], [ 2, 6, -2 ], [ 3, -3 ], [ 4, -4 ], [ 5, -5 ], [ -6 ] ]), Bipartition([ [ 1, 2, 3, 4, 5, 6, -3 ], [ -1 ], [ -2 ], [ -4 ], [ -5 ], [ -6 ] ]) ] );
<bipartition semigroup of degree 6 with 3 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);;
gap> (IsActingSemigroup(S) and UnderlyingSemigroup(T) = Group((1,4)(2,5))) 
> or (not IsActingSemigroup(S) and  UnderlyingSemigroup(T) = Group((1,2)));
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

#T# AsSemigroup: 
#   convert from IsTransformationSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup( [ Transformation( [ 4, 5, 3, 1, 2, 1 ] ), Transformation( [ 1, 2, 3, 4, 5, 2 ] ), Transformation( [ 3, 3, 3, 3, 3, 3 ] ) ] );
<transformation semigroup of degree 6 with 3 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);; 
gap> (IsActingSemigroup(S) and UnderlyingSemigroup(T) = Group((1,4)(2,5))) 
> or (not IsActingSemigroup(S) and  UnderlyingSemigroup(T) = Group((1,2)));
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

#T# AsSemigroup: 
#   convert from IsBooleanMatSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsBooleanMat, [ [ false, false, false, true, false, false ], [ false, false, false, false, true, false ], [ false, false, true, false, false, false ], [ true, false, false, false, false, false ], [ false, true, false, false, false, false ], [ true, false, false, false, false, false ] ]), Matrix(IsBooleanMat, [ [ true, false, false, false, false, false ], [ false, true, false, false, false, false ], [ false, false, true, false, false, false ], [ false, false, false, true, false, false ], [ false, false, false, false, true, false ], [ false, true, false, false, false, false ] ]), Matrix(IsBooleanMat, [ [ false, false, true, false, false, false ], [ false, false, true, false, false, false ], [ false, false, true, false, false, false ], [ false, false, true, false, false, false ], [ false, false, true, false, false, false ], [ false, false, true, false, false, false ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsMaxPlusMatrixSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsMaxPlusMatrix, [ [ -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity ] ]), Matrix(IsMaxPlusMatrix, [ [ 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity ] ]), Matrix(IsMaxPlusMatrix, [ [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsMinPlusMatrixSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsMinPlusMatrix, [ [ infinity, infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity, infinity ] ]), Matrix(IsMinPlusMatrix, [ [ 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity ] ]), Matrix(IsMinPlusMatrix, [ [ infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity ] ]), Matrix(IsProjectiveMaxPlusMatrix, [ [ 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity ] ]), Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsIntegerMatrixSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsIntegerMatrix, [ [ 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 1, 0 ], [ 0, 0, 1, 0, 0, 0 ], [ 1, 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0, 0 ] ]), Matrix(IsIntegerMatrix, [ [ 1, 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 1, 0 ], [ 0, 1, 0, 0, 0, 0 ] ]), Matrix(IsIntegerMatrix, [ [ 0, 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity ] ], 1), Matrix(IsTropicalMaxPlusMatrix, [ [ 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity ] ], 1), Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity ] ], 1) ] );
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

#T# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsTropicalMinPlusMatrix, [ [ infinity, infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity, infinity ] ], 3), Matrix(IsTropicalMinPlusMatrix, [ [ 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity ] ], 3), Matrix(IsTropicalMinPlusMatrix, [ [ infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity ] ], 3) ] );
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

#T# AsSemigroup: 
#   convert from IsNTPMatrixSemigroup to IsReesZeroMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsNTPMatrix, [ [ 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 1, 0 ], [ 0, 0, 1, 0, 0, 0 ], [ 1, 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0, 0 ] ], 4, 1), Matrix(IsNTPMatrix, [ [ 1, 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 1, 0 ], [ 0, 1, 0, 0, 0, 0 ] ], 4, 1), Matrix(IsNTPMatrix, [ [ 0, 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ] ], 4, 1) ] );
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

#T# AsSemigroup: 
#   convert from IsPBRMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid( [ PBR([ [ -3 ], [ -2 ], [ -1 ] ], [ [ 3 ], [ 2 ], [ 1 ] ]), PBR([ [ -2 ], [ -2 ], [ -2 ] ], [ [ ], [ 1, 2, 3 ], [ ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsFpMonoid to IsReesZeroMatrixSemigroup
gap> F := FreeMonoid(2);; AssignGeneratorVariables(F);;
gap> rels := [ [ m1^2, One(F) ], [ m1*m2, m2 ], [ m2*m1, m2 ], [ m2^2, m2 ] ];;
gap> S := F / rels;
<fp monoid on the generators [ m1, m2 ]>
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

#T# AsSemigroup: 
#   convert from IsBipartitionMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid( [ Bipartition([ [ 1, -3 ], [ 2, -2 ], [ 3, -1 ] ]), Bipartition([ [ 1, 2, 3, -2 ], [ -1 ], [ -3 ] ]) ] );
<bipartition monoid of degree 3 with 2 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group([ (1,3) ])>
gap> Size(S) = Size(T);
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

#T# AsSemigroup: 
#   convert from IsTransformationMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid( [ Transformation( [ 3, 2, 1 ] ), Transformation( [ 2, 2, 2 ] ) ] );
<transformation monoid of degree 3 with 2 generators>
gap> T := AsSemigroup(IsReesZeroMatrixSemigroup, S);
<Rees 0-matrix semigroup 1x1 over Group([ (1,3) ])>
gap> Size(S) = Size(T);
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

#T# AsSemigroup: 
#   convert from IsBooleanMatMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid( [ Matrix(IsBooleanMat, [ [ false, false, true ], [ false, true, false ], [ true, false, false ] ]), Matrix(IsBooleanMat, [ [ false, true, false ], [ false, true, false ], [ false, true, false ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsMaxPlusMatrixMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid( [ Matrix(IsMaxPlusMatrix, [ [ -infinity, -infinity, 0 ], [ -infinity, 0, -infinity ], [ 0, -infinity, -infinity ] ]), Matrix(IsMaxPlusMatrix, [ [ -infinity, 0, -infinity ], [ -infinity, 0, -infinity ], [ -infinity, 0, -infinity ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsMinPlusMatrixMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid( [ Matrix(IsMinPlusMatrix, [ [ infinity, infinity, 0 ], [ infinity, 0, infinity ], [ 0, infinity, infinity ] ]), Matrix(IsMinPlusMatrix, [ [ infinity, 0, infinity ], [ infinity, 0, infinity ], [ infinity, 0, infinity ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid( [ Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, -infinity, 0 ], [ -infinity, 0, -infinity ], [ 0, -infinity, -infinity ] ]), Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, 0, -infinity ], [ -infinity, 0, -infinity ], [ -infinity, 0, -infinity ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsIntegerMatrixMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid( [ Matrix(IsIntegerMatrix, [ [ 0, 0, 1 ], [ 0, 1, 0 ], [ 1, 0, 0 ] ]), Matrix(IsIntegerMatrix, [ [ 0, 1, 0 ], [ 0, 1, 0 ], [ 0, 1, 0 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid( [ Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, -infinity, 0 ], [ -infinity, 0, -infinity ], [ 0, -infinity, -infinity ] ], 1), Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, 0, -infinity ], [ -infinity, 0, -infinity ], [ -infinity, 0, -infinity ] ], 1) ] );
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

#T# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid( [ Matrix(IsTropicalMinPlusMatrix, [ [ infinity, infinity, 0 ], [ infinity, 0, infinity ], [ 0, infinity, infinity ] ], 3), Matrix(IsTropicalMinPlusMatrix, [ [ infinity, 0, infinity ], [ infinity, 0, infinity ], [ infinity, 0, infinity ] ], 3) ] );
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

#T# AsSemigroup: 
#   convert from IsNTPMatrixMonoid to IsReesZeroMatrixSemigroup
gap> S := Monoid( [ Matrix(IsNTPMatrix, [ [ 0, 0, 1 ], [ 0, 1, 0 ], [ 1, 0, 0 ] ], 4, 1), Matrix(IsNTPMatrix, [ [ 0, 1, 0 ], [ 0, 1, 0 ], [ 0, 1, 0 ] ], 4, 1) ] );
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

#T# AsSemigroup: 
#   convert from IsFpMonoid to IsReesZeroMatrixSemigroup
gap> F := FreeMonoid(1);; AssignGeneratorVariables(F);;
gap> rels := [ [ m1^2, m1 ] ];;
gap> S := F / rels;
<fp monoid on the generators [ m1 ]>
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

#T# AsSemigroup: 
#   convert from IsPBRMonoid to IsReesMatrixSemigroup
gap> S := Monoid( [ PBR([ [ -2 ], [ -1 ] ], [ [ 2 ], [ 1 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsFpMonoid to IsReesMatrixSemigroup
gap> F := FreeMonoid(1);; AssignGeneratorVariables(F);;
gap> rels := [ [ m1^2, One(F) ] ];;
gap> S := F / rels;
<fp monoid on the generators [ m1 ]>
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

#T# AsSemigroup: 
#   convert from IsBipartitionMonoid to IsReesMatrixSemigroup
gap> S := InverseMonoid( [ Bipartition([ [ 1, -1 ], [ 2, -2 ] ]), Bipartition([ [ 1, -2 ], [ 2, -1 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsTransformationMonoid to IsReesMatrixSemigroup
gap> S := Monoid( [ Transformation( [ 2, 1 ] ) ] );
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

#T# AsSemigroup: 
#   convert from IsBooleanMatMonoid to IsReesMatrixSemigroup
gap> S := Monoid( [ Matrix(IsBooleanMat, [ [ false, true ], [ true, false ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsMaxPlusMatrixMonoid to IsReesMatrixSemigroup
gap> S := Monoid( [ Matrix(IsMaxPlusMatrix, [ [ -infinity, 0 ], [ 0, -infinity ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsMinPlusMatrixMonoid to IsReesMatrixSemigroup
gap> S := Monoid( [ Matrix(IsMinPlusMatrix, [ [ infinity, 0 ], [ 0, infinity ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixMonoid to IsReesMatrixSemigroup
gap> S := Monoid( [ Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, 0 ], [ 0, -infinity ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsIntegerMatrixMonoid to IsReesMatrixSemigroup
gap> S := Monoid( [ Matrix(IsIntegerMatrix, [ [ 0, 1 ], [ 1, 0 ] ]) ] );
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

#T# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixMonoid to IsReesMatrixSemigroup
gap> S := Monoid( [ Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, 0 ], [ 0, -infinity ] ], 1) ] );
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

#T# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixMonoid to IsReesMatrixSemigroup
gap> S := Monoid( [ Matrix(IsTropicalMinPlusMatrix, [ [ infinity, 0 ], [ 0, infinity ] ], 3) ] );
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

#T# AsSemigroup: 
#   convert from IsNTPMatrixMonoid to IsReesMatrixSemigroup
gap> S := Monoid( [ Matrix(IsNTPMatrix, [ [ 0, 1 ], [ 1, 0 ] ], 4, 1) ] );
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

#T# AsSemigroup: 
#   convert from IsFpMonoid to IsReesMatrixSemigroup
gap> F := FreeMonoid(0);; AssignGeneratorVariables(F);;
gap> rels := [ ];;
gap> S := F / rels;
<fp group on the generators [  ]>
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

#E#
gap> STOP_TEST("Semigroups package: standard/semirms.tst");
