#############################################################################
##
#W  standard/semiboolmat.tst
#Y  Copyright (C) 2015                                  James D. Mitchell
##                                                       
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semiboolmat.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#T# BruteForceIsoCheck helper functions
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

#T# AsSemigroup: 
#   convert from IsPBRMonoid to IsBooleanMatSemigroup
gap> S := Monoid( [ PBR([ [ -2 ], [ -3 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ], [ -8 ], [ -9 ], [ -10 ], [ -5 ] ], [ [ ], [ 1 ], [ 2 ], [ 3 ], [ 4, 10 ], [ 5 ], [ 6 ], [ 7 ], [ 8 ], [ 9 ] ]) ] );
<commutative pbr monoid of degree 10 with 1 generator>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<commutative monoid of size 10, 10x10 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsFpMonoid to IsBooleanMatSemigroup
gap> F := FreeMonoid(2);; AssignGeneratorVariables(F);;
gap> rels := [ [ m1^2, m1 ], [ m1*m2, m2 ], [ m2*m1, m2 ], [ m2^10, m2^4 ] ];;
gap> S := F / rels;
<fp monoid on the generators [ m1, m2 ]>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<monoid of size 11, 11x11 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsBipartitionMonoid to IsBooleanMatSemigroup
gap> S := Monoid( [ Bipartition([ [ 1, -2 ], [ 2, -3 ], [ 3, -4 ], [ 4, 10, -5 ], [ 5, -6 ], [ 6, -7 ], [ 7, -8 ], [ 8, -9 ], [ 9, -10 ], [ -1 ] ]) ] );
<commutative bipartition monoid of degree 10 with 1 generator>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<commutative monoid of 10x10 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsTransformationMonoid to IsBooleanMatSemigroup
gap> S := Monoid( [ Transformation( [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 5 ] ) ] );
<commutative transformation monoid of degree 10 with 1 generator>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<commutative monoid of 10x10 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsBooleanMatMonoid to IsBooleanMatSemigroup
gap> S := Monoid( [ Matrix(IsBooleanMat, [ [ false, true, false, false, false, false, false, false, false, false ], [ false, false, true, false, false, false, false, false, false, false ], [ false, false, false, true, false, false, false, false, false, false ], [ false, false, false, false, true, false, false, false, false, false ], [ false, false, false, false, false, true, false, false, false, false ], [ false, false, false, false, false, false, true, false, false, false ], [ false, false, false, false, false, false, false, true, false, false ], [ false, false, false, false, false, false, false, false, true, false ], [ false, false, false, false, false, false, false, false, false, true ], [ false, false, false, false, true, false, false, false, false, false ] ]) ] );
<commutative monoid of 10x10 boolean matrices with 1 generator>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<commutative monoid of 10x10 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsMaxPlusMatrixMonoid to IsBooleanMatSemigroup
gap> S := Monoid( [ Matrix(IsMaxPlusMatrix, [ [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0 ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ] ]) ] );
<commutative monoid of 10x10 max-plus matrices with 1 generator>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<commutative monoid of size 10, 10x10 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsMinPlusMatrixMonoid to IsBooleanMatSemigroup
gap> S := Monoid( [ Matrix(IsMinPlusMatrix, [ [ infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0 ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity ] ]) ] );
<commutative monoid of 10x10 min-plus matrices with 1 generator>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<commutative monoid of size 10, 10x10 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixMonoid to IsBooleanMatSemigroup
gap> S := Monoid( [ Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0 ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ] ]) ] );
<commutative monoid of 10x10 projective max-plus matrices with 1 generator>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<commutative monoid of size 10, 10x10 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsIntegerMatrixMonoid to IsBooleanMatSemigroup
gap> S := Monoid( [ Matrix(IsIntegerMatrix, [ [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 0, 1, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 ], [ 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 ] ]) ] );
<commutative monoid of 10x10 integer matrices with 1 generator>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<commutative monoid of size 10, 10x10 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixMonoid to IsBooleanMatSemigroup
gap> S := Monoid( [ Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0 ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ] ], 1) ] );
<commutative monoid of 10x10 tropical max-plus matrices with 1 generator>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<commutative monoid of size 10, 10x10 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixMonoid to IsBooleanMatSemigroup
gap> S := Monoid( [ Matrix(IsTropicalMinPlusMatrix, [ [ infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0 ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity ] ], 5) ] );
<commutative monoid of 10x10 tropical min-plus matrices with 1 generator>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<commutative monoid of size 10, 10x10 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsNTPMatrixMonoid to IsBooleanMatSemigroup
gap> S := Monoid( [ Matrix(IsNTPMatrix, [ [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 0, 1, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 ], [ 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 ] ], 5, 2) ] );
<commutative monoid of 10x10 ntp matrices with 1 generator>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<commutative monoid of size 10, 10x10 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsPBRSemigroup to IsBooleanMatSemigroup
gap> S := Semigroup( [ PBR([ [ -3 ], [ -5 ], [ -4 ], [ -4 ], [ -4 ], [ -3 ], [ -4 ], [ -1 ] ], [ [ 8 ], [ ], [ 1, 6 ], [ 3, 4, 5, 7 ], [ 2 ], [ ], [ ], [ ] ]), PBR([ [ -4 ], [ -6 ], [ -4 ], [ -4 ], [ -4 ], [ -7 ], [ -4 ], [ -2 ] ], [ [ ], [ 8 ], [ ], [ 1, 3, 4, 5, 7 ], [ ], [ 2 ], [ 6 ], [ ] ]) ] );
<pbr semigroup of degree 8 with 2 generators>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of size 7, 8x8 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsFpSemigroup to IsBooleanMatSemigroup
gap> F := FreeSemigroup(2);; AssignGeneratorVariables(F);;
gap> rels := [ [ s1^3, s1*s2 ], [ s1^2*s2, s1*s2 ], [ s1*s2*s1, s1*s2 ], [ s1*s2^2, s1*s2 ], [ s2*s1^2, s1*s2 ], [ s2*s1*s2, s1*s2 ], [ s2^2*s1, s1^2 ], [ s2^4, s1*s2 ] ];;
gap> S := F / rels;
<fp semigroup on the generators [ s1, s2 ]>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of 8x8 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsBipartitionSemigroup to IsBooleanMatSemigroup
gap> S := Semigroup( [ Bipartition([ [ 1, 6, -3 ], [ 2, -5 ], [ 3, 4, 5, 7, -4 ], [ 8, -1 ], [ -2 ], [ -6 ], [ -7 ], [ -8 ] ]), Bipartition([ [ 1, 3, 4, 5, 7, -4 ], [ 2, -6 ], [ 6, -7 ], [ 8, -2 ], [ -1 ], [ -3 ], [ -5 ], [ -8 ] ]) ] );
<bipartition semigroup of degree 8 with 2 generators>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of 8x8 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsTransformationSemigroup to IsBooleanMatSemigroup
gap> S := Semigroup( [ Transformation( [ 3, 5, 4, 4, 4, 3, 4, 1 ] ), Transformation( [ 4, 6, 4, 4, 4, 7, 4, 2 ] ) ] );
<transformation semigroup of degree 8 with 2 generators>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of 8x8 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsBooleanMatSemigroup to IsBooleanMatSemigroup
gap> S := Semigroup( [ Matrix(IsBooleanMat, [ [ false, false, true, false, false, false, false, false ], [ false, false, false, false, true, false, false, false ], [ false, false, false, true, false, false, false, false ], [ false, false, false, true, false, false, false, false ], [ false, false, false, true, false, false, false, false ], [ false, false, true, false, false, false, false, false ], [ false, false, false, true, false, false, false, false ], [ true, false, false, false, false, false, false, false ] ]), Matrix(IsBooleanMat, [ [ false, false, false, true, false, false, false, false ], [ false, false, false, false, false, true, false, false ], [ false, false, false, true, false, false, false, false ], [ false, false, false, true, false, false, false, false ], [ false, false, false, true, false, false, false, false ], [ false, false, false, false, false, false, true, false ], [ false, false, false, true, false, false, false, false ], [ false, true, false, false, false, false, false, false ] ]) ] );
<semigroup of 8x8 boolean matrices with 2 generators>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of 8x8 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsMaxPlusMatrixSemigroup to IsBooleanMatSemigroup
gap> S := Semigroup( [ Matrix(IsMaxPlusMatrix, [ [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ]), Matrix(IsMaxPlusMatrix, [ [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ]) ] );
<semigroup of 8x8 max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of size 7, 8x8 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsMinPlusMatrixSemigroup to IsBooleanMatSemigroup
gap> S := Semigroup( [ Matrix(IsMinPlusMatrix, [ [ infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity ] ]), Matrix(IsMinPlusMatrix, [ [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity ] ]) ] );
<semigroup of 8x8 min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of size 7, 8x8 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsBooleanMatSemigroup
gap> S := Semigroup( [ Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ]), Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ]) ] );
<semigroup of 8x8 projective max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of size 7, 8x8 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsIntegerMatrixSemigroup to IsBooleanMatSemigroup
gap> S := Semigroup( [ Matrix(IsIntegerMatrix, [ [ 0, 0, 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0, 0, 0, 0 ] ]), Matrix(IsIntegerMatrix, [ [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 1, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0, 0, 0 ] ]) ] );
<semigroup of 8x8 integer matrices with 2 generators>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of size 7, 8x8 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsBooleanMatSemigroup
gap> S := Semigroup( [ Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ], 4), Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ], 4) ] );
<semigroup of 8x8 tropical max-plus matrices with 2 generators>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of size 7, 8x8 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsBooleanMatSemigroup
gap> S := Semigroup( [ Matrix(IsTropicalMinPlusMatrix, [ [ infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity ] ], 1), Matrix(IsTropicalMinPlusMatrix, [ [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity ] ], 1) ] );
<semigroup of 8x8 tropical min-plus matrices with 2 generators>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of size 7, 8x8 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsNTPMatrixSemigroup to IsBooleanMatSemigroup
gap> S := Semigroup( [ Matrix(IsNTPMatrix, [ [ 0, 0, 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0, 0, 0, 0 ] ], 5, 5), Matrix(IsNTPMatrix, [ [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 1, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0, 0, 0, 0 ] ], 5, 5) ] );
<semigroup of 8x8 ntp matrices with 2 generators>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of size 7, 8x8 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsPBRSemigroup to IsBooleanMatMonoid
gap> S := Semigroup( [ PBR([ [ -1 ], [ -2 ], [ -2 ], [ -2 ], [ -2 ] ], [ [ 1 ], [ 2, 3, 4, 5 ], [ ], [ ], [ ] ]), PBR([ [ -2 ], [ -1 ], [ -1 ], [ -1 ], [ -1 ] ], [ [ 2, 3, 4, 5 ], [ 1 ], [ ], [ ], [ ] ]) ] );
<pbr semigroup of degree 5 with 2 generators>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 2, 2x2 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsFpSemigroup to IsBooleanMatMonoid
gap> F := FreeSemigroup(2);; AssignGeneratorVariables(F);;
gap> rels := [ [ s1^2, s1 ], [ s1*s2, s2 ], [ s2*s1, s2 ], [ s2^2, s1 ] ];;
gap> S := F / rels;
<fp semigroup on the generators [ s1, s2 ]>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 2, 2x2 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsBipartitionSemigroup to IsBooleanMatMonoid
gap> S := Semigroup( [ Bipartition([ [ 1, -1 ], [ 2, 3, 4, 5, -2 ], [ -3 ], [ -4 ], [ -5 ] ]), Bipartition([ [ 1, -2 ], [ 2, 3, 4, 5, -1 ], [ -3 ], [ -4 ], [ -5 ] ]) ] );
<bipartition semigroup of degree 5 with 2 generators>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of 2x2 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsTransformationSemigroup to IsBooleanMatMonoid
gap> S := Semigroup( [ Transformation( [ 1, 2, 2, 2, 2 ] ), Transformation( [ 2, 1, 1, 1, 1 ] ) ] );
<transformation semigroup of degree 5 with 2 generators>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of 2x2 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsBooleanMatSemigroup to IsBooleanMatMonoid
gap> S := Semigroup( [ Matrix(IsBooleanMat, [ [ true, false, false, false, false ], [ false, true, false, false, false ], [ false, true, false, false, false ], [ false, true, false, false, false ], [ false, true, false, false, false ] ]), Matrix(IsBooleanMat, [ [ false, true, false, false, false ], [ true, false, false, false, false ], [ true, false, false, false, false ], [ true, false, false, false, false ], [ true, false, false, false, false ] ]) ] );
<semigroup of 5x5 boolean matrices with 2 generators>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of 2x2 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsMaxPlusMatrixSemigroup to IsBooleanMatMonoid
gap> S := Semigroup( [ Matrix(IsMaxPlusMatrix, [ [ 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ] ]), Matrix(IsMaxPlusMatrix, [ [ -infinity, 0, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ] ]) ] );
<semigroup of 5x5 max-plus matrices with 2 generators>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 2, 2x2 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsMinPlusMatrixSemigroup to IsBooleanMatMonoid
gap> S := Semigroup( [ Matrix(IsMinPlusMatrix, [ [ 0, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity ] ]), Matrix(IsMinPlusMatrix, [ [ infinity, 0, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity ] ]) ] );
<semigroup of 5x5 min-plus matrices with 2 generators>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 2, 2x2 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsBooleanMatMonoid
gap> S := Semigroup( [ Matrix(IsProjectiveMaxPlusMatrix, [ [ 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ] ]), Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, 0, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ] ]) ] );
<semigroup of 5x5 projective max-plus matrices with 2 generators>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 2, 2x2 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsIntegerMatrixSemigroup to IsBooleanMatMonoid
gap> S := Semigroup( [ Matrix(IsIntegerMatrix, [ [ 1, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ] ]), Matrix(IsIntegerMatrix, [ [ 0, 1, 0, 0, 0 ], [ 1, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0 ] ]) ] );
<semigroup of 5x5 integer matrices with 2 generators>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 2, 2x2 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsBooleanMatMonoid
gap> S := Semigroup( [ Matrix(IsTropicalMaxPlusMatrix, [ [ 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, 0, -infinity, -infinity, -infinity ] ], 1), Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, 0, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity ] ], 1) ] );
<semigroup of 5x5 tropical max-plus matrices with 2 generators>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 2, 2x2 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsBooleanMatMonoid
gap> S := Semigroup( [ Matrix(IsTropicalMinPlusMatrix, [ [ 0, infinity, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity ], [ infinity, 0, infinity, infinity, infinity ] ], 3), Matrix(IsTropicalMinPlusMatrix, [ [ infinity, 0, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity ], [ 0, infinity, infinity, infinity, infinity ] ], 3) ] );
<semigroup of 5x5 tropical min-plus matrices with 2 generators>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 2, 2x2 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsNTPMatrixSemigroup to IsBooleanMatMonoid
gap> S := Semigroup( [ Matrix(IsNTPMatrix, [ [ 1, 0, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 1, 0, 0, 0 ] ], 4, 1), Matrix(IsNTPMatrix, [ [ 0, 1, 0, 0, 0 ], [ 1, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0 ], [ 1, 0, 0, 0, 0 ] ], 4, 1) ] );
<semigroup of 5x5 ntp matrices with 2 generators>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 2, 2x2 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsPBRMonoid to IsBooleanMatMonoid
gap> S := Monoid( [ PBR([ [ -2 ], [ -3 ], [ -4 ], [ -5 ], [ -5 ] ], [ [ ], [ 1 ], [ 2 ], [ 3 ], [ 4, 5 ] ]) ] );
<commutative pbr monoid of degree 5 with 1 generator>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 5, 5x5 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsFpMonoid to IsBooleanMatMonoid
gap> F := FreeMonoid(1);; AssignGeneratorVariables(F);;
gap> rels := [ [ m1^5, m1^4 ] ];;
gap> S := F / rels;
<fp monoid on the generators [ m1 ]>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 5, 5x5 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsBipartitionMonoid to IsBooleanMatMonoid
gap> S := Monoid( [ Bipartition([ [ 1, -2 ], [ 2, -3 ], [ 3, -4 ], [ 4, 5, -5 ], [ -1 ] ]) ] );
<commutative bipartition monoid of degree 5 with 1 generator>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of 5x5 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsTransformationMonoid to IsBooleanMatMonoid
gap> S := Monoid( [ Transformation( [ 2, 3, 4, 5, 5 ] ) ] );
<commutative transformation monoid of degree 5 with 1 generator>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of 5x5 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsBooleanMatMonoid to IsBooleanMatMonoid
gap> S := Monoid( [ Matrix(IsBooleanMat, [ [ false, true, false, false, false ], [ false, false, true, false, false ], [ false, false, false, true, false ], [ false, false, false, false, true ], [ false, false, false, false, true ] ]) ] );
<commutative monoid of 5x5 boolean matrices with 1 generator>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of 5x5 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsMaxPlusMatrixMonoid to IsBooleanMatMonoid
gap> S := Monoid( [ Matrix(IsMaxPlusMatrix, [ [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0 ], [ -infinity, -infinity, -infinity, -infinity, 0 ] ]) ] );
<commutative monoid of 5x5 max-plus matrices with 1 generator>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 5, 5x5 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsMinPlusMatrixMonoid to IsBooleanMatMonoid
gap> S := Monoid( [ Matrix(IsMinPlusMatrix, [ [ infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, infinity, infinity, 0 ], [ infinity, infinity, infinity, infinity, 0 ] ]) ] );
<commutative monoid of 5x5 min-plus matrices with 1 generator>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 5, 5x5 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsProjectiveMaxPlusMatrixMonoid to IsBooleanMatMonoid
gap> S := Monoid( [ Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0 ], [ -infinity, -infinity, -infinity, -infinity, 0 ] ]) ] );
<commutative monoid of 5x5 projective max-plus matrices with 1 generator>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 5, 5x5 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsIntegerMatrixMonoid to IsBooleanMatMonoid
gap> S := Monoid( [ Matrix(IsIntegerMatrix, [ [ 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 0, 0, 0, 0, 1 ], [ 0, 0, 0, 0, 1 ] ]) ] );
<commutative monoid of 5x5 integer matrices with 1 generator>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 5, 5x5 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsTropicalMaxPlusMatrixMonoid to IsBooleanMatMonoid
gap> S := Monoid( [ Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0 ], [ -infinity, -infinity, -infinity, -infinity, 0 ] ], 1) ] );
<commutative monoid of 5x5 tropical max-plus matrices with 1 generator>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 5, 5x5 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsTropicalMinPlusMatrixMonoid to IsBooleanMatMonoid
gap> S := Monoid( [ Matrix(IsTropicalMinPlusMatrix, [ [ infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, infinity, infinity, 0 ], [ infinity, infinity, infinity, infinity, 0 ] ], 3) ] );
<commutative monoid of 5x5 tropical min-plus matrices with 1 generator>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 5, 5x5 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid: 
#   convert from IsNTPMatrixMonoid to IsBooleanMatMonoid
gap> S := Monoid( [ Matrix(IsNTPMatrix, [ [ 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0 ], [ 0, 0, 0, 1, 0 ], [ 0, 0, 0, 0, 1 ], [ 0, 0, 0, 0, 1 ] ], 4, 1) ] );
<commutative monoid of 5x5 ntp matrices with 1 generator>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<commutative monoid of size 5, 5x5 boolean matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsReesMatrixSemigroup to IsBooleanMatSemigroup
gap> R := ReesMatrixSemigroup(Group([(1, 2)]), [[(1, 2), (1, 2)], [(), ()]]);
<Rees matrix semigroup 2x2 over Group([ (1,2) ])>
gap> T := AsSemigroup(IsBooleanMatSemigroup, R);
<semigroup of size 8, 9x9 boolean matrices with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid
#   convert from IsReesMatrixSemigroup to IsBooleanMatMonoid
gap> R := ReesMatrixSemigroup(Group([(1, 2)]), [[(1, 2)]]);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
gap> T := AsMonoid(IsBooleanMatMonoid, R);
<commutative monoid of size 2, 2x2 boolean matrices with 1 generator>
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
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsReesZeroMatrixSemigroup to IsBooleanMatSemigroup
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2)]), 
>                                 [[(1, 2), (1, 2)], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
gap> T := AsSemigroup(IsBooleanMatSemigroup, R);
<semigroup of size 9, 10x10 boolean matrices with 3 generators>
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
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsMonoid
#   convert from IsReesZeroMatrixSemigroup to IsBooleanMatMonoid
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[(1, 2)]]);
<Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>
gap> T := AsMonoid(IsBooleanMatMonoid, R);
<monoid of size 3, 3x3 boolean matrices with 2 generators>
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
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from graph inverse to IsBooleanMatSemigroup
gap> S := GraphInverseSemigroup(Digraph([[2],[]]));
<finite graph inverse semigroup with 2 vertices, 1 edge>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of size 6, 7x7 boolean matrices with 4 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from perm group to IsBooleanMatSemigroup
gap> S := DihedralGroup(IsPermGroup, 6);
Group([ (1,2,3), (2,3) ])
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of 3x3 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from perm group to IsBooleanMatMonoid
gap> S := DihedralGroup(IsPermGroup, 6);
Group([ (1,2,3), (2,3) ])
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<semigroup of 3x3 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from non-perm group to IsBooleanMatSemigroup
gap> S := DihedralGroup(6);
<pc group of size 6 with 2 generators>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of size 6, 6x6 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from non-perm group to IsBooleanMatMonoid
gap> S := DihedralGroup(6);
<pc group of size 6 with 2 generators>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<semigroup of size 6, 6x6 boolean matrices with 2 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsBlockBijectionSemigroup to IsBooleanMatSemigroup
gap> S := InverseSemigroup(Bipartition([[1, -1, -3], [2, 3, -2]]));;
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of size 5, 6x6 boolean matrices with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsBlockBijectionMonoid to IsBooleanMatMonoid
gap> S := InverseMonoid([Bipartition([[1, -1, -3], [2, 3, -2]])]);;
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<monoid of size 6, 6x6 boolean matrices with 2 generators>
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
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsBlockBijectionMonoid to IsBooleanMatSemigroup
gap> S := InverseMonoid([Bipartition([[1, -1, -3], [2, 3, -2]])]);;
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<monoid of size 6, 6x6 boolean matrices with 2 generators>
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
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsPartialPermSemigroup to IsBooleanMatSemigroup
gap> S := InverseSemigroup(PartialPerm([1, 2], [2, 1]),
>                          PartialPerm([1, 2], [3, 1]));
<inverse partial perm semigroup of rank 3 with 2 generators>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of 4x4 boolean matrices with 3 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsPartialPermMonoid to IsBooleanMatMonoid
gap> S := InverseMonoid(PartialPerm([1, 2], [2, 1]),
>                       PartialPerm([1, 2], [3, 1]));
<inverse partial perm monoid of rank 3 with 2 generators>
gap> T := AsMonoid(IsBooleanMatMonoid, S);
<monoid of 4x4 boolean matrices with 3 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismMonoid(IsBooleanMatMonoid, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsPartialPermMonoid to IsBooleanMatSemigroup
gap> S := InverseMonoid(PartialPerm([1, 2], [2, 1]),
>                       PartialPerm([1, 2], [3, 1]));
<inverse partial perm monoid of rank 3 with 2 generators>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<monoid of 4x4 boolean matrices with 3 generators>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(BruteForceInverseCheck);
gap> Unbind(BruteForceIsoCheck);
gap> Unbind(F);
gap> Unbind(R);
gap> Unbind(S);
gap> Unbind(T);
gap> Unbind(map);
gap> Unbind(rels);
gap> Unbind(x);
gap> Unbind(y);

#E# 
gap> STOP_TEST("Semigroups package: standard/semiboolmat.tst");
