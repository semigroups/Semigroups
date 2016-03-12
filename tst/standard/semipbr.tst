#############################################################################
##
#W  standard/semipbr.tst
#Y  Copyright (C) 2015                                  James D. Mitchell
##                                                       
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semipbr.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#T# AsSemigroup: 
#   convert from IsPBRSemigroup to IsPBRSemigroup
gap> S := Semigroup(PBR([[-2, 1, 2], [-2, -1, 1]], [[-2, -1, 1], [-2, -1, 1, 2]]),
>                   PBR([[], []], [[], []]));
<pbr semigroup of degree 2 with 2 generators>
gap> T := AsSemigroup(IsPBRSemigroup, S);
<pbr semigroup of size 8, degree 9 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPBRSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsFpSemigroup to IsPBRSemigroup
gap> F := FreeSemigroup(2);; AssignGeneratorVariables(F);;
gap> rels := [ [ s2^2, s2 ], [ s1^3, s1^2 ], [ s2*s1^2, s2*s1 ], [ s2*s1*s2, s2 ] ];;
gap> S := F / rels;
<fp semigroup on the generators [ s1, s2 ]>
gap> T := AsSemigroup(IsPBRSemigroup, S);
<pbr semigroup of degree 9 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPBRSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsBipartitionSemigroup to IsPBRSemigroup
gap> S := Semigroup( [ Bipartition([ [ 1, 3, -3 ], [ 2, 5, -5 ], [ 4, 7, -7 ], [ 6, 8, -8 ], [ 9, -1 ], [ -2 ], [ -4 ], [ -6 ], [ -9 ] ]), Bipartition([ [ 1, 4, 7, -4 ], [ 2, 5, 9, -2 ], [ 3, 6, 8, -6 ], [ -1 ], [ -3 ], [ -5 ], [ -7 ], [ -8 ], [ -9 ] ]) ] );
<bipartition semigroup of degree 9 with 2 generators>
gap> T := AsSemigroup(IsPBRSemigroup, S);
<pbr semigroup of degree 9 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPBRSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsTransformationSemigroup to IsPBRSemigroup
gap> S := Semigroup( [ Transformation( [ 3, 5, 3, 7, 5, 8, 7, 8, 1 ] ), Transformation( [ 4, 2, 6, 4, 2, 6, 4, 6, 2 ] ) ] );
<transformation semigroup of degree 9 with 2 generators>
gap> T := AsSemigroup(IsPBRSemigroup, S);
<pbr semigroup of degree 9 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPBRSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsBooleanMatSemigroup to IsPBRSemigroup
gap> S := Semigroup( [ Matrix(IsBooleanMat, [ [ false, false, true, false, false, false, false, false, false ], [ false, false, false, false, true, false, false, false, false ], [ false, false, true, false, false, false, false, false, false ], [ false, false, false, false, false, false, true, false, false ], [ false, false, false, false, true, false, false, false, false ], [ false, false, false, false, false, false, false, true, false ], [ false, false, false, false, false, false, true, false, false ], [ false, false, false, false, false, false, false, true, false ], [ true, false, false, false, false, false, false, false, false ] ]), Matrix(IsBooleanMat, [ [ false, false, false, true, false, false, false, false, false ], [ false, true, false, false, false, false, false, false, false ], [ false, false, false, false, false, true, false, false, false ], [ false, false, false, true, false, false, false, false, false ], [ false, true, false, false, false, false, false, false, false ], [ false, false, false, false, false, true, false, false, false ], [ false, false, false, true, false, false, false, false, false ], [ false, false, false, false, false, true, false, false, false ], [ false, true, false, false, false, false, false, false, false ] ]) ] );
<semigroup of 9x9 boolean matrices with 2 generators>
gap> T := AsSemigroup(IsPBRSemigroup, S);
<pbr semigroup of degree 9 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPBRSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsMaxPlusMatrixSemigroup to IsPBRSemigroup
gap> S := Semigroup( [ Matrix(IsMaxPlusMatrix, [ [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ]), Matrix(IsMaxPlusMatrix, [ [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ]) ] );
<semigroup of 9x9 max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsPBRSemigroup, S);
<pbr semigroup of size 8, degree 9 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPBRSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsMinPlusMatrixSemigroup to IsPBRSemigroup
gap> S := Semigroup( [ Matrix(IsMinPlusMatrix, [ [ infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity, infinity ] ]), Matrix(IsMinPlusMatrix, [ [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity ] ]) ] );
<semigroup of 9x9 min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsPBRSemigroup, S);
<pbr semigroup of size 8, degree 9 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPBRSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsPBRSemigroup
gap> S := Semigroup( [ Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ]), Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ]) ] );
<semigroup of 9x9 projective max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsPBRSemigroup, S);
<pbr semigroup of size 8, degree 9 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPBRSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsIntegerMatrixSemigroup to IsPBRSemigroup
gap> S := Semigroup( [ Matrix(IsIntegerMatrix, [ [ 0, 0, 1, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 1, 0 ], [ 0, 0, 0, 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 1, 0 ], [ 1, 0, 0, 0, 0, 0, 0, 0, 0 ] ]), Matrix(IsIntegerMatrix, [ [ 0, 0, 0, 1, 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0, 0, 0, 0 ] ]) ] );
<semigroup of 9x9 integer matrices with 2 generators>
gap> T := AsSemigroup(IsPBRSemigroup, S);
<pbr semigroup of size 8, degree 9 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPBRSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsPBRSemigroup
gap> S := Semigroup( [ Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ], 1), Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ], 1) ] );
<semigroup of 9x9 tropical max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsPBRSemigroup, S);
<pbr semigroup of size 8, degree 9 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPBRSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsPBRSemigroup
gap> S := Semigroup( [ Matrix(IsTropicalMinPlusMatrix, [ [ infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity, infinity ] ], 5), Matrix(IsTropicalMinPlusMatrix, [ [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity ] ], 5) ] );
<semigroup of 9x9 tropical min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsPBRSemigroup, S);
<pbr semigroup of size 8, degree 9 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPBRSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsNTPMatrixSemigroup to IsPBRSemigroup
gap> S := Semigroup( [ Matrix(IsNTPMatrix, [ [ 0, 0, 1, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 1, 0 ], [ 0, 0, 0, 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 1, 0 ], [ 1, 0, 0, 0, 0, 0, 0, 0, 0 ] ], 5, 2), Matrix(IsNTPMatrix, [ [ 0, 0, 0, 1, 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0, 0, 0, 0 ] ], 5, 2) ] );
<semigroup of 9x9 ntp matrices with 2 generators>
gap> T := AsSemigroup(IsPBRSemigroup, S);
<pbr semigroup of size 8, degree 9 with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsPBRSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#E# 
gap> STOP_TEST("Semigroups package: standard/semipbr.tst");
