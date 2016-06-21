#############################################################################
##
#W  standard/semimaxplus.tst
#Y  Copyright (C) 2015                                  James D. Mitchell
##                                                       
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semimaxplus.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# semimaxplus: C++ code working, for max-plus matrix semigroup
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[0, -4], [-4, -1]]),
>                   Matrix(IsMaxPlusMatrix, [[0, -3], [-3, -1]]));
<semigroup of 2x2 max-plus matrices with 2 generators>
gap> Size(S);
26
gap> NrDClasses(S);
23
gap> NrRClasses(S);
24
gap> NrLClasses(S);
24
gap> NrHClasses(S);
26
gap> NrIdempotents(S);
4
gap> MultiplicativeZero(S);
fail

# semimaxplus: C++ code working, for natural matrix semigroup
gap> S := Monoid(Matrix(IsNTPMatrix, [[0, 1, 0], [1, 1, 0], [0, 1, 0]], 1, 2),
>                Matrix(IsNTPMatrix, [[1, 0, 0], [1, 0, 1], [1, 0, 0]], 1, 2));
<monoid of 3x3 ntp matrices with 2 generators>
gap> Size(S);
37
gap> Length(RelationsOfFpMonoid(Range(IsomorphismFpMonoid(S))));
12
gap> GenericSemigroupData(S);
<closed semigroup data with 37 elements, 17 relations, max word length 7>
gap> NrDClasses(S);
8
gap> NrRClasses(S);
14
gap> NrLClasses(S);
17
gap> NrHClasses(S);
35
gap> NrIdempotents(S);
20
gap> MultiplicativeZero(S);
fail

#T# helper functions
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
#   convert from IsPBRSemigroup to IsMaxPlusMatrixSemigroup
gap> S := Semigroup( [ PBR([ [ -2 ], [ -3 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ], [ -8 ], [ -8 ], [ -1 ] ], [ [ 9 ], [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 6 ], [ 7, 8 ], [ ] ]) ] );
<commutative pbr semigroup of degree 9 with 1 generator>
gap> T := AsSemigroup(IsMaxPlusMatrixSemigroup, S);
<commutative semigroup of size 8, 9x9 max-plus matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsMaxPlusMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsFpSemigroup to IsMaxPlusMatrixSemigroup
gap> F := FreeSemigroup(1);; AssignGeneratorVariables(F);;
gap> rels := [ [ s1^9, s1^8 ] ];;
gap> S := F / rels;
<fp semigroup on the generators [ s1 ]>
gap> T := AsSemigroup(IsMaxPlusMatrixSemigroup, S);
<commutative semigroup of 9x9 max-plus matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsMaxPlusMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsBipartitionSemigroup to IsMaxPlusMatrixSemigroup
gap> S := Semigroup( [ Bipartition([ [ 1, -2 ], [ 2, -3 ], [ 3, -4 ], [ 4, -5 ], [ 5, -6 ], [ 6, -7 ], [ 7, 8, -8 ], [ 9, -1 ], [ -9 ] ]) ] );
<commutative bipartition semigroup of degree 9 with 1 generator>
gap> T := AsSemigroup(IsMaxPlusMatrixSemigroup, S);
<commutative semigroup of 9x9 max-plus matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsMaxPlusMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsTransformationSemigroup to IsMaxPlusMatrixSemigroup
gap> S := Semigroup( [ Transformation( [ 2, 3, 4, 5, 6, 7, 8, 8, 1 ] ) ] );
<commutative transformation semigroup of degree 9 with 1 generator>
gap> T := AsSemigroup(IsMaxPlusMatrixSemigroup, S);
<commutative semigroup of 9x9 max-plus matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsMaxPlusMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsBooleanMatSemigroup to IsMaxPlusMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsBooleanMat, [ [ false, true, false, false, false, false, false, false, false ], [ false, false, true, false, false, false, false, false, false ], [ false, false, false, true, false, false, false, false, false ], [ false, false, false, false, true, false, false, false, false ], [ false, false, false, false, false, true, false, false, false ], [ false, false, false, false, false, false, true, false, false ], [ false, false, false, false, false, false, false, true, false ], [ false, false, false, false, false, false, false, true, false ], [ true, false, false, false, false, false, false, false, false ] ]) ] );
<commutative semigroup of 9x9 boolean matrices with 1 generator>
gap> T := AsSemigroup(IsMaxPlusMatrixSemigroup, S);
<commutative semigroup of 9x9 max-plus matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsMaxPlusMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsMaxPlusMatrixSemigroup to IsMaxPlusMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsMaxPlusMatrix, [ [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ]) ] );
<commutative semigroup of 9x9 max-plus matrices with 1 generator>
gap> T := AsSemigroup(IsMaxPlusMatrixSemigroup, S);
<commutative semigroup of 9x9 max-plus matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsMaxPlusMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsMinPlusMatrixSemigroup to IsMaxPlusMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsMinPlusMatrix, [ [ infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity, infinity ] ]) ] );
<commutative semigroup of 9x9 min-plus matrices with 1 generator>
gap> T := AsSemigroup(IsMaxPlusMatrixSemigroup, S);
<commutative semigroup of size 8, 9x9 max-plus matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsMaxPlusMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsProjectiveMaxPlusMatrixSemigroup to IsMaxPlusMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsProjectiveMaxPlusMatrix, [ [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ]) ] );
<commutative semigroup of 9x9 projective max-plus matrices with 1 generator>
gap> T := AsSemigroup(IsMaxPlusMatrixSemigroup, S);
<commutative semigroup of size 8, 9x9 max-plus matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsMaxPlusMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsIntegerMatrixSemigroup to IsMaxPlusMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsIntegerMatrix, [ [ 0, 1, 0, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 1, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 1, 0 ], [ 1, 0, 0, 0, 0, 0, 0, 0, 0 ] ]) ] );
<commutative semigroup of 9x9 integer matrices with 1 generator>
gap> T := AsSemigroup(IsMaxPlusMatrixSemigroup, S);
<commutative semigroup of size 8, 9x9 max-plus matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsMaxPlusMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsTropicalMaxPlusMatrixSemigroup to IsMaxPlusMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsTropicalMaxPlusMatrix, [ [ -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, 0, -infinity ], [ 0, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity, -infinity ] ], 2) ] );
<commutative semigroup of 9x9 tropical max-plus matrices with 1 generator>
gap> T := AsSemigroup(IsMaxPlusMatrixSemigroup, S);
<commutative semigroup of size 8, 9x9 max-plus matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsMaxPlusMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsTropicalMinPlusMatrixSemigroup to IsMaxPlusMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsTropicalMinPlusMatrix, [ [ infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ infinity, infinity, infinity, infinity, infinity, infinity, infinity, 0, infinity ], [ 0, infinity, infinity, infinity, infinity, infinity, infinity, infinity, infinity ] ], 4) ] );
<commutative semigroup of 9x9 tropical min-plus matrices with 1 generator>
gap> T := AsSemigroup(IsMaxPlusMatrixSemigroup, S);
<commutative semigroup of size 8, 9x9 max-plus matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsMaxPlusMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# AsSemigroup: 
#   convert from IsNTPMatrixSemigroup to IsMaxPlusMatrixSemigroup
gap> S := Semigroup( [ Matrix(IsNTPMatrix, [ [ 0, 1, 0, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 1, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 1, 0 ], [ 1, 0, 0, 0, 0, 0, 0, 0, 0 ] ], 3, 4) ] );
<commutative semigroup of 9x9 ntp matrices with 1 generator>
gap> T := AsSemigroup(IsMaxPlusMatrixSemigroup, S);
<commutative semigroup of size 8, 9x9 max-plus matrices with 1 generator>
gap> Size(S) = Size(T);
true
gap> NrDClasses(S) = NrDClasses(T);
true
gap> NrRClasses(S) = NrRClasses(T);
true
gap> NrLClasses(S) = NrLClasses(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true
gap> map := IsomorphismSemigroup(IsMaxPlusMatrixSemigroup, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);

#E# 
gap> STOP_TEST("Semigroups package: standard/semimaxplus.tst");
