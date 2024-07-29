#############################################################################
##
#W  standard/attributes/isomorph.tst
#Y  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local A, BruteForceInverseCheck, BruteForceIsoCheck, F, G, S, T, U, V, inv
#@local map, x, y, M, N, R, L
gap> START_TEST("Semigroups package: standard/attributes/isomorph.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# helper functions
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

# isomorph: SmallestMultiplicationTable, 1/2
gap> S := DualSymmetricInverseMonoid(2);
<inverse block bijection monoid of degree 2 with 2 generators>
gap> Size(S);
3
gap> SmallestMultiplicationTable(S);
[ [ 1, 2, 3 ], [ 2, 1, 3 ], [ 3, 3, 3 ] ]

# isomorph: SmallestMultiplicationTable, 2/2
gap> S := Semigroup(
> [PBR([[-4, 1, 2, 3], [-4, 1, 2, 4], [-2, -1, 1], [-4, -1, 1, 2, 4]],
>      [[-4, -3, 1, 4], [-3, -1, 3], [-4, -1, 1, 2, 4], [-4, -3, -2, 3, 4]]),
>  PBR([[-3, -2, -1, 1, 3, 4], [-4, -3, -2, -1, 2, 3], [-3, -2, -1, 1],
>       [-1, 1, 2, 3]],
>      [[-3, -2, -1, 2, 3, 4], [-3, 1, 4],
>       [-3, 1, 2], [-4, -3, 1, 2, 3, 4]]),
>  PBR([[-3, -1, 1, 3], [-1, 1, 2], [-2, -1, 1], [-4, -3, -1, 1, 2, 3, 4]],
>      [[-4, -3, -2], [], [-4, -1, 1, 2], [-4, -3, -2, -1, 2, 3, 4]]),
>  PBR([[-3, -2, -1, 2, 3], [-2, -1, 2, 4], [-3, -2, 1, 3],
>       [-4, -3, -2, -1, 1, 2, 3, 4]],
>      [[-4, -2, -1, 4], [-4, -3, -2, 2, 3], [-3, -2, -1, 1, 3, 4], [-3, 2]]),
>  PBR([[-4, -3, -2, -1, 2, 3], [-4, -1, 1, 2, 3, 4], [-3, 1, 2, 3],
>       [-4, -3, -2, -1, 1, 4]],
>      [[-4, -3, -1, 2, 4], [-3, -2, 2, 3, 4], [-4, -2, -1, 1],
>       [-4, -2, 1, 4]]),
>  PBR([[-4, -2, -1, 2, 3], [-4, -3, -1, 1, 3], [-4, 2, 4], [-3, -1, 1]],
>      [[-4, -3, -1, 1, 3], [-4, -3, 2, 3], [-4, -3, -2, -1, 2, 4],
>       [-4, -1, 1, 3, 4]])]);
<pbr semigroup of degree 4 with 6 generators>
gap> Size(S);
11
gap> SmallestMultiplicationTable(S);
[ [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], 
  [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], 
  [ 1, 1, 1, 1, 1, 6, 6, 7, 9, 9, 11 ], [ 1, 1, 1, 11, 1, 6, 6, 6, 9, 9, 11 ],
  [ 1, 1, 1, 11, 1, 6, 6, 6, 9, 9, 11 ], [ 1, 1, 1, 11, 1, 6, 6, 6, 9, 9, 11 ]
    , [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], 
  [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ] ]
gap> CanonicalMultiplicationTable(S);
[ [ 11, 11, 11, 10, 9, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 11, 10, 9, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 11, 10, 9, 11, 10, 8, 9, 10, 11 ], 
  [ 8, 11, 11, 10, 9, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 11, 10, 9, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 11, 7, 9, 11, 10, 8, 9, 10, 11 ], 
  [ 8, 11, 11, 10, 9, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 11, 10, 9, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 11, 10, 9, 11, 10, 8, 9, 10, 11 ], 
  [ 8, 11, 11, 10, 9, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 11, 10, 9, 11, 10, 8, 9, 10, 11 ] ]

# isomorph: IsIsomorphicSemigroup, 1/2
gap> S := DualSymmetricInverseMonoid(2);;
gap> T := Semigroup([Transformation([2, 1, 2, 2]),
>                    Transformation([1, 2, 3, 3])]);;
gap> IsIsomorphicSemigroup(S, T);
false

# isomorph: IsIsomorphicSemigroup, 2/2
gap> S := Semigroup([
>  Matrix(IsNTPMatrix, [[0, 1, 2], [4, 3, 0], [0, 2, 0]], 9, 4),
>  Matrix(IsNTPMatrix, [[1, 1, 0], [4, 1, 1], [0, 0, 0]], 9, 4)]);
<semigroup of 3x3 ntp matrices with 2 generators>
gap> IsIsomorphicSemigroup(S, S);
true
gap> T := AsSemigroup(IsTransformationSemigroup, S);
<transformation semigroup of size 46, degree 47 with 2 generators>
gap> IsIsomorphicSemigroup(S, T);
true
gap> S := Semigroup(IdentityTransformation);
<trivial transformation group of degree 0 with 1 generator>
gap> T := Semigroup(PartialPerm([]));
<trivial partial perm group of rank 0 with 1 generator>
gap> IsIsomorphicSemigroup(S, T);
true
gap> T := JonesMonoid(4);
<regular bipartition *-monoid of degree 4 with 3 generators>
gap> IsIsomorphicSemigroup(S, T);
false

# isomorph: IsIsomorphicSemigroup, for finite simple semigroups
gap> M := [[(1, 2, 3), ()], [(), ()], [(), ()]];
[ [ (1,2,3), () ], [ (), () ], [ (), () ] ]
gap> N := [[(1, 3, 2), ()], [(), (1, 2, 3)], [(1, 3, 2), (1, 3, 2)]];
[ [ (1,3,2), () ], [ (), (1,2,3) ], [ (1,3,2), (1,3,2) ] ]
gap> R := ReesMatrixSemigroup(AlternatingGroup([1 .. 3]), M);
<Rees matrix semigroup 2x3 over Alt( [ 1 .. 3 ] )>
gap> S := ReesMatrixSemigroup(Group([(1, 2, 3)]), N);
<Rees matrix semigroup 2x3 over Group([ (1,2,3) ])>
gap> IsIsomorphicSemigroup(R, S);
true
gap> R := SemigroupByMultiplicationTable(MultiplicationTable(R));;
gap> S := SemigroupByMultiplicationTable(MultiplicationTable(S));;
gap> IsIsomorphicSemigroup(R, S);
true
gap> L := [[(), ()], [(), ()], [(1, 2, 3), ()]];
[ [ (), () ], [ (), () ], [ (1,2,3), () ] ]
gap> T := ReesMatrixSemigroup(SymmetricGroup([1 .. 3]), L);
<Rees matrix semigroup 2x3 over Sym( [ 1 .. 3 ] )>
gap> IsIsomorphicSemigroup(S, T);
false
gap> IsIsomorphicSemigroup(T, T);
true
gap> M := [[(1, 2, 3), ()], [(), ()], [(), ()]];
[ [ (1,2,3), () ], [ (), () ], [ (), () ] ]
gap> N := [[(1, 3, 2), ()], [(), (1, 2, 3)]];
[ [ (1,3,2), () ], [ (), (1,2,3) ] ]
gap> R := ReesMatrixSemigroup(AlternatingGroup([1 .. 3]), M);;
gap> S := ReesMatrixSemigroup(AlternatingGroup([1 .. 3]), N);;
gap> IsIsomorphicSemigroup(R, S);
false
gap> R := ReesMatrixSemigroup(AlternatingGroup([1 .. 5]),
> [[(), ()], [(), (1, 3, 2, 4, 5)]]);;
gap> S := ReesMatrixSemigroup(AlternatingGroup([1 .. 5]),
> [[(1, 5, 4, 3, 2), ()], [(1, 4, 5), (1, 4)(3, 5)]]);;
gap> IsIsomorphicSemigroup(R, S);
false

# isomorph: IsomorphismSemigroups, for infinite semigroup(s)
gap> S := FreeSemigroup(1);;
gap> T := TrivialSemigroup();;
gap> IsomorphismSemigroups(S, T);
fail
gap> IsomorphismSemigroups(S, FreeSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `IsomorphismSemigroups' on 2 arguments

# isomorph: IsomorphismSemigroups, for trivial semigroups
gap> S := TrivialSemigroup(IsTransformationSemigroup);
<trivial transformation group of degree 0 with 1 generator>
gap> T := TrivialSemigroup(IsBipartitionSemigroup);
<trivial block bijection group of degree 1 with 1 generator>
gap> map := IsomorphismSemigroups(S, T);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# isomorph: IsomorphismSemigroups, for monogenic semigroups
gap> S := MonogenicSemigroup(IsTransformationSemigroup, 3, 2);
<commutative non-regular transformation semigroup of size 4, degree 5 with 1 
 generator>
gap> T := MonogenicSemigroup(IsBipartitionSemigroup, 3, 2);
<commutative non-regular block bijection semigroup of size 4, degree 6 with 1 
 generator>
gap> map := IsomorphismSemigroups(S, T);
<commutative non-regular transformation semigroup of size 4, degree 5 with 1 
  generator> -> <commutative non-regular block bijection semigroup of size 4, 
  degree 6 with 1 generator>
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# isomorph: IsomorphismSemigroups, for simple semigroups
gap> S := ReesMatrixSemigroup(SymmetricGroup(3), [[(), (1, 3, 2)],
>                                                 [(2, 3), (1, 2)],
>                                                 [(), (2, 3, 1)]]);
<Rees matrix semigroup 2x3 over Sym( [ 1 .. 3 ] )>
gap> T := ReesMatrixSemigroup(SymmetricGroup(3), [[(), ()],
>                                                 [(), ()],
>                                                 [(), ()]]);
<Rees matrix semigroup 2x3 over Sym( [ 1 .. 3 ] )>
gap> U := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of size 36, degree 37 with 4 generators>
gap> V := AsSemigroup(IsTransformationSemigroup, S);
<transformation semigroup of size 36, degree 37 with 4 generators>
gap> map := IsomorphismSemigroups(S, U);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroups(U, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroups(U, V);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> IsomorphismSemigroups(U, T);
fail

# isomorph: IsomorphismSemigroups, for 0-simple semigroups
gap> S := ReesZeroMatrixSemigroup(SymmetricGroup(3), [[(), (1, 3, 2)],
>                                                     [0, (1, 2)],
>                                                     [(), (2, 3, 1)]]);
<Rees 0-matrix semigroup 2x3 over Sym( [ 1 .. 3 ] )>
gap> T := ReesZeroMatrixSemigroup(SymmetricGroup(3), [[(), ()],
>                                                     [(), ()],
>                                                     [(), 0]]);
<Rees 0-matrix semigroup 2x3 over Sym( [ 1 .. 3 ] )>
gap> U := AsSemigroup(IsBipartitionSemigroup, S);
<bipartition semigroup of size 37, degree 38 with 5 generators>
gap> V := AsSemigroup(IsTransformationSemigroup, S);
<transformation semigroup of size 37, degree 38 with 5 generators>
gap> map := IsomorphismSemigroups(S, U);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroups(U, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroups(U, V);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> IsomorphismSemigroups(U, T);
fail
gap> F := FreeSemigroup(1);;
gap> F := F / [[F.1 ^ 4, F.1]];;
gap> S := ReesZeroMatrixSemigroup(F, [[F.1]]);;
gap> T := ReesZeroMatrixSemigroup(F, [[F.1 ^ 2]]);;
gap> map := IsomorphismSemigroups(S, T);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# isomorph: IsomorphismSemigroups, non-isomorphic partial order of D-classes
gap> S := ZeroSemigroup(3);
<commutative non-regular transformation semigroup of size 3, degree 4 with 2 
 generators>
gap> T := MonogenicSemigroup(3, 1);
<commutative non-regular transformation semigroup of size 3, degree 4 with 1 
 generator>
gap> IsomorphismSemigroups(S, T);
fail

# for monogenic semigroups
gap> S := MonogenicSemigroup(4, 5);;
gap> T := MonogenicSemigroup(20, 1);;
gap> IsomorphismSemigroups(S, T);
fail
gap> S := MonogenicSemigroup(1, 4);;
gap> T := MonogenicSemigroup(2, 3);;
gap> IsomorphismSemigroups(S, T);
fail
gap> S := MonogenicSemigroup(1, 4);;
gap> T := Semigroup(Generators(S) ^ (1, 2));;
gap> IsomorphismSemigroups(S, T) <> fail;
true

# for larger semigroups, Sean Clark's PLU monoid
gap> A := [DigraphFromDiSparse6String(".[{?`abcdefghijklmnopqrstuvwxyz"),
> DigraphFromDiSparse6String(
> ".[_bAdCfEhGjIlKnMpOrQtSvU{_bAdCfEhGjIlKnMpOrQtSvUwxyz"),
> DigraphFromDiSparse6String(".[h?jAlCnE`gbidkfm{h?jAlCnE`gbidkfmwxyz"),
> DigraphFromSparse6String(":[w?BGJORaDILQTcFKNSV`EHMPU")];;
gap> S := Semigroup(List(A, AsBooleanMat));
<monoid of 28x28 boolean matrices with 3 generators>
gap> Size(S);
40
gap> CanonicalMultiplicationTable(S);
[ [ 35, 34, 39, 40, 34, 35, 39, 40, 35, 34, 39, 40, 2, 2, 1, 1, 1, 1, 3, 3, 
      3, 3, 3, 3, 3, 1, 1, 34, 35, 39, 40, 4, 4, 2, 1, 1, 1, 3, 3, 4 ], 
  [ 34, 35, 40, 39, 35, 34, 40, 39, 34, 35, 40, 39, 1, 1, 2, 2, 2, 2, 4, 4, 
      4, 4, 4, 4, 4, 2, 2, 35, 34, 40, 39, 3, 3, 1, 2, 2, 2, 4, 4, 3 ], 
  [ 40, 39, 34, 35, 39, 40, 34, 35, 40, 39, 34, 35, 4, 4, 3, 3, 3, 3, 1, 1, 
      1, 1, 1, 1, 1, 3, 3, 39, 40, 34, 35, 2, 2, 4, 3, 3, 3, 1, 1, 2 ], 
  [ 39, 40, 35, 34, 40, 39, 35, 34, 39, 40, 35, 34, 3, 3, 4, 4, 4, 4, 2, 2, 
      2, 2, 2, 2, 2, 4, 4, 40, 39, 35, 34, 1, 1, 3, 4, 4, 4, 2, 2, 1 ], 
  [ 34, 35, 40, 39, 35, 34, 40, 39, 34, 36, 40, 38, 1, 6, 5, 2, 5, 2, 4, 8, 
      4, 8, 8, 8, 8, 5, 5, 36, 34, 40, 38, 7, 3, 1, 2, 2, 5, 4, 4, 3 ], 
  [ 35, 34, 39, 40, 34, 35, 39, 40, 36, 34, 38, 40, 5, 2, 1, 6, 1, 6, 7, 3, 
      7, 3, 7, 7, 7, 6, 6, 34, 36, 38, 40, 4, 8, 2, 1, 1, 6, 3, 3, 4 ], 
  [ 40, 39, 34, 35, 39, 40, 34, 35, 40, 38, 34, 36, 4, 8, 7, 3, 7, 3, 1, 6, 
      1, 6, 6, 6, 6, 7, 7, 38, 40, 34, 36, 5, 2, 4, 3, 3, 7, 1, 1, 2 ], 
  [ 39, 40, 35, 34, 40, 39, 35, 34, 38, 40, 36, 34, 7, 3, 4, 8, 4, 8, 5, 2, 
      5, 2, 5, 5, 5, 8, 8, 40, 38, 36, 34, 1, 6, 3, 4, 4, 8, 2, 2, 1 ], 
  [ 35, 34, 39, 40, 13, 16, 21, 33, 35, 34, 39, 40, 2, 2, 1, 1, 9, 9, 11, 11, 
      3, 3, 11, 11, 11, 9, 9, 13, 16, 21, 33, 4, 4, 2, 1, 9, 9, 11, 3, 4 ], 
  [ 34, 35, 40, 39, 15, 14, 32, 22, 34, 35, 40, 39, 1, 1, 2, 2, 10, 10, 12, 
      12, 4, 4, 12, 12, 12, 10, 10, 15, 14, 32, 22, 3, 3, 1, 2, 10, 10, 12, 
      4, 3 ], 
  [ 40, 39, 34, 35, 21, 33, 13, 16, 40, 39, 34, 35, 4, 4, 3, 3, 11, 11, 9, 9, 
      1, 1, 9, 9, 9, 11, 11, 21, 33, 13, 16, 2, 2, 4, 3, 11, 11, 9, 1, 2 ], 
  [ 39, 40, 35, 34, 32, 22, 15, 14, 39, 40, 35, 34, 3, 3, 4, 4, 12, 12, 10, 
      10, 2, 2, 10, 10, 10, 12, 12, 32, 22, 15, 14, 1, 1, 3, 4, 12, 12, 10, 
      2, 1 ], 
  [ 2, 1, 4, 3, 1, 2, 4, 3, 2, 9, 4, 11, 35, 16, 13, 34, 13, 34, 40, 33, 40, 
      33, 33, 33, 33, 13, 13, 9, 2, 4, 11, 21, 39, 35, 34, 34, 13, 40, 40, 39 
     ], [ 2, 1, 4, 3, 1, 2, 4, 3, 10, 1, 12, 3, 15, 35, 34, 14, 34, 14, 32, 
      40, 32, 40, 32, 32, 32, 14, 14, 1, 10, 12, 3, 39, 22, 35, 34, 34, 14, 
      40, 40, 39 ], 
  [ 1, 2, 3, 4, 2, 1, 3, 4, 1, 10, 3, 12, 34, 14, 15, 35, 15, 35, 39, 22, 39, 
      22, 22, 22, 22, 15, 15, 10, 1, 3, 12, 32, 40, 34, 35, 35, 15, 39, 39, 
      40 ], 
  [ 1, 2, 3, 4, 2, 1, 3, 4, 9, 2, 11, 4, 13, 34, 35, 16, 35, 16, 21, 39, 21, 
      39, 21, 21, 21, 16, 16, 2, 9, 11, 4, 40, 33, 34, 35, 35, 16, 39, 39, 40 
     ], [ 1, 2, 3, 4, 5, 6, 7, 8, 1, 10, 3, 12, 34, 14, 15, 35, 17, 36, 38, 
      20, 39, 22, 20, 20, 20, 17, 17, 28, 6, 7, 31, 32, 40, 34, 35, 36, 17, 
      38, 39, 40 ], 
  [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 2, 11, 4, 13, 34, 35, 16, 36, 18, 19, 38, 21, 
      39, 19, 19, 19, 18, 18, 5, 29, 30, 8, 40, 33, 34, 35, 36, 18, 38, 39, 
      40 ], 
  [ 4, 3, 2, 1, 7, 8, 5, 6, 4, 11, 2, 9, 40, 33, 21, 39, 19, 38, 36, 18, 35, 
      16, 18, 18, 18, 19, 19, 30, 8, 5, 29, 13, 34, 40, 39, 38, 19, 36, 35, 
      34 ], [ 4, 3, 2, 1, 7, 8, 5, 6, 12, 3, 10, 1, 32, 40, 39, 22, 38, 20, 
      17, 36, 15, 35, 17, 17, 17, 20, 20, 7, 31, 28, 6, 34, 14, 40, 39, 38, 
      20, 36, 35, 34 ], 
  [ 4, 3, 2, 1, 3, 4, 2, 1, 4, 11, 2, 9, 40, 33, 21, 39, 21, 39, 35, 16, 35, 
      16, 16, 16, 16, 21, 21, 11, 4, 2, 9, 13, 34, 40, 39, 39, 21, 35, 35, 34 
     ], [ 4, 3, 2, 1, 3, 4, 2, 1, 12, 3, 10, 1, 32, 40, 39, 22, 39, 22, 15, 
      35, 15, 35, 15, 15, 15, 22, 22, 3, 12, 10, 1, 34, 14, 40, 39, 39, 22, 
      35, 35, 34 ], 
  [ 4, 3, 2, 1, 7, 8, 5, 6, 12, 11, 10, 9, 32, 33, 21, 22, 19, 20, 17, 18, 
      15, 16, 37, 26, 27, 24, 25, 30, 31, 28, 29, 13, 14, 40, 39, 38, 23, 36, 
      35, 34 ], 
  [ 4, 3, 2, 1, 7, 8, 5, 6, 12, 11, 10, 9, 32, 33, 21, 22, 19, 20, 17, 18, 
      15, 16, 27, 37, 26, 25, 23, 30, 31, 28, 29, 13, 14, 40, 39, 38, 24, 36, 
      35, 34 ], 
  [ 4, 3, 2, 1, 7, 8, 5, 6, 12, 11, 10, 9, 32, 33, 21, 22, 19, 20, 17, 18, 
      15, 16, 26, 27, 37, 23, 24, 30, 31, 28, 29, 13, 14, 40, 39, 38, 25, 36, 
      35, 34 ], 
  [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 
      21, 22, 25, 23, 24, 27, 37, 28, 29, 30, 31, 32, 33, 34, 35, 36, 26, 38, 
      39, 40 ], 
  [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 
      21, 22, 24, 25, 23, 37, 26, 28, 29, 30, 31, 32, 33, 34, 35, 36, 27, 38, 
      39, 40 ], 
  [ 34, 35, 40, 39, 15, 14, 32, 22, 34, 36, 40, 38, 1, 6, 5, 2, 28, 10, 12, 
      31, 4, 8, 31, 31, 31, 28, 28, 17, 14, 32, 20, 7, 3, 1, 2, 10, 28, 12, 
      4, 3 ], 
  [ 35, 34, 39, 40, 13, 16, 21, 33, 36, 34, 38, 40, 5, 2, 1, 6, 9, 29, 30, 
      11, 7, 3, 30, 30, 30, 29, 29, 13, 18, 19, 33, 4, 8, 2, 1, 9, 29, 11, 3, 
      4 ], [ 40, 39, 34, 35, 21, 33, 13, 16, 40, 38, 34, 36, 4, 8, 7, 3, 30, 
      11, 9, 29, 1, 6, 29, 29, 29, 30, 30, 19, 33, 13, 18, 5, 2, 4, 3, 11, 
      30, 9, 1, 2 ], 
  [ 39, 40, 35, 34, 32, 22, 15, 14, 38, 40, 36, 34, 7, 3, 4, 8, 12, 31, 28, 
      10, 5, 2, 28, 28, 28, 31, 31, 32, 20, 17, 14, 1, 6, 3, 4, 12, 31, 10, 
      2, 1 ], [ 3, 4, 1, 2, 4, 3, 1, 2, 3, 12, 1, 10, 39, 22, 32, 40, 32, 40, 
      34, 14, 34, 14, 14, 14, 14, 32, 32, 12, 3, 1, 10, 15, 35, 39, 40, 40, 
      32, 34, 34, 35 ], 
  [ 3, 4, 1, 2, 4, 3, 1, 2, 11, 4, 9, 2, 21, 39, 40, 33, 40, 33, 13, 34, 13, 
      34, 13, 13, 13, 33, 33, 4, 11, 9, 2, 35, 16, 39, 40, 40, 33, 34, 34, 35 
     ], [ 2, 1, 4, 3, 1, 2, 4, 3, 2, 1, 4, 3, 35, 35, 34, 34, 34, 34, 40, 40, 
      40, 40, 40, 40, 40, 34, 34, 1, 2, 4, 3, 39, 39, 35, 34, 34, 34, 40, 40, 
      39 ], [ 1, 2, 3, 4, 2, 1, 3, 4, 1, 2, 3, 4, 34, 34, 35, 35, 35, 35, 39, 
      39, 39, 39, 39, 39, 39, 35, 35, 2, 1, 3, 4, 40, 40, 34, 35, 35, 35, 39, 
      39, 40 ], 
  [ 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 34, 34, 35, 35, 36, 36, 38, 38, 39, 
      39, 38, 38, 38, 36, 36, 5, 6, 7, 8, 40, 40, 34, 35, 36, 36, 38, 39, 40 ]
    , [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 
      20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 
      38, 39, 40 ], 
  [ 4, 3, 2, 1, 7, 8, 5, 6, 4, 3, 2, 1, 40, 40, 39, 39, 38, 38, 36, 36, 35, 
      35, 36, 36, 36, 38, 38, 7, 8, 5, 6, 34, 34, 40, 39, 38, 38, 36, 35, 34 ]
    , [ 4, 3, 2, 1, 3, 4, 2, 1, 4, 3, 2, 1, 40, 40, 39, 39, 39, 39, 35, 35, 
      35, 35, 35, 35, 35, 39, 39, 3, 4, 2, 1, 34, 34, 40, 39, 39, 39, 35, 35, 
      34 ], [ 3, 4, 1, 2, 4, 3, 1, 2, 3, 4, 1, 2, 39, 39, 40, 40, 40, 40, 34, 
      34, 34, 34, 34, 34, 34, 40, 40, 4, 3, 1, 2, 35, 35, 39, 40, 40, 40, 34, 
      34, 35 ] ]

# IsomorphismSemigroups
gap> S := FullTransformationMonoid(3);
<full transformation monoid of degree 3>
gap> T := AsMonoid(IsPBRMonoid, S);
<pbr monoid of size 27, degree 3 with 3 generators>
gap> map := IsomorphismSemigroups(S, T);
<full transformation monoid of degree 3> -> <pbr monoid of size 27, degree 3 
  with 3 generators>
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# AutomorphismGroup
gap> S := JonesMonoid(5);
<regular bipartition *-monoid of degree 5 with 4 generators>
gap> G := AutomorphismGroup(S);;
gap> StructureDescription(G);
"C2"
gap> S := FullTransformationMonoid(3);
<full transformation monoid of degree 3>
gap> G := AutomorphismGroup(S);
<group with 2 generators>
gap> StructureDescription(G);
"S3"
gap> S := Semigroup(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2], [-2, 4, -infinity], [1, 0, 3]]));
<commutative semigroup of 3x3 max-plus matrices with 1 generator>
gap> AutomorphismGroup(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `AutomorphismGroup' on 1 arguments

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/attributes/isomorph.tst");
