#############################################################################
##
#W  standard/isomorph.tst
#Y  Copyright (C) 2015-17                               James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/isomorph.tst");
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
[ [ 11, 11, 11, 9, 10, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 11, 9, 10, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 11, 9, 10, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 11, 9, 10, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 8, 9, 10, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 11, 9, 7, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 8, 9, 10, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 11, 9, 10, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 11, 9, 10, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 8, 9, 10, 11, 10, 8, 9, 10, 11 ], 
  [ 11, 11, 11, 9, 10, 11, 10, 8, 9, 10, 11 ] ]

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
MappingByFunction( <commutative non-regular transformation semigroup 
 of size 4, degree 5 with 1 generator>, <commutative non-regular 
 block bijection semigroup of size 4, degree 6 with 1 generator>
 , function( x ) ... end, function( x ) ... end )
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
> DigraphFromSparse6String(":[w?BGJORaDILQTcFKNSV`EHMPU")];
[ <immutable digraph with 28 vertices, 28 edges>, 
  <immutable digraph with 28 vertices, 28 edges>, 
  <immutable digraph with 28 vertices, 28 edges>, 
  <immutable digraph with 28 vertices, 48 edges> ]
gap> S := Semigroup(List(A, AsBooleanMat));
<monoid of 28x28 boolean matrices with 3 generators>
gap> Size(S);
40
gap> CanonicalMultiplicationTable(S);
[ [ 39, 5, 6, 40, 1, 4, 1, 4, 5, 39, 40, 6, 39, 5, 40, 6, 3, 3, 2, 2, 1, 1, 
      4, 4, 39, 5, 6, 40, 4, 4, 1, 1, 4, 4, 4, 1, 1, 1, 2, 3 ], 
  [ 5, 39, 40, 6, 2, 3, 2, 3, 39, 5, 6, 40, 5, 39, 6, 40, 4, 4, 1, 1, 2, 2, 
      3, 3, 5, 39, 40, 6, 3, 3, 2, 2, 3, 3, 3, 2, 2, 2, 1, 4 ], 
  [ 40, 6, 5, 39, 3, 2, 3, 2, 6, 40, 39, 5, 40, 6, 39, 5, 1, 1, 4, 4, 3, 3, 
      2, 2, 40, 6, 5, 39, 2, 2, 3, 3, 2, 2, 2, 3, 3, 3, 4, 1 ], 
  [ 6, 40, 39, 5, 4, 1, 4, 1, 40, 6, 5, 39, 6, 40, 5, 39, 2, 2, 3, 3, 4, 4, 
      1, 1, 6, 40, 39, 5, 1, 1, 4, 4, 1, 1, 1, 4, 4, 4, 3, 2 ], 
  [ 1, 2, 3, 4, 5, 6, 5, 6, 2, 1, 4, 3, 1, 2, 4, 3, 40, 40, 39, 39, 5, 5, 6, 
      6, 1, 2, 3, 4, 6, 6, 5, 5, 6, 6, 6, 5, 5, 5, 39, 40 ], 
  [ 3, 4, 1, 2, 6, 5, 6, 5, 4, 3, 2, 1, 3, 4, 2, 1, 39, 39, 40, 40, 6, 6, 5, 
      5, 3, 4, 1, 2, 5, 5, 6, 6, 5, 5, 5, 6, 6, 6, 40, 39 ], 
  [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 4, 3, 40, 40, 39, 39, 5, 5, 
      6, 6, 10, 9, 12, 11, 8, 8, 7, 7, 8, 8, 8, 7, 7, 7, 39, 40 ], 
  [ 3, 4, 1, 2, 6, 5, 8, 7, 11, 12, 9, 10, 3, 4, 2, 1, 39, 39, 40, 40, 6, 6, 
      5, 5, 12, 11, 10, 9, 7, 7, 8, 8, 7, 7, 7, 8, 8, 8, 40, 39 ], 
  [ 5, 39, 40, 6, 2, 3, 2, 3, 39, 5, 6, 40, 7, 39, 8, 40, 4, 11, 10, 1, 2, 9, 
      12, 3, 7, 39, 40, 8, 3, 12, 2, 9, 12, 12, 12, 9, 9, 9, 1, 4 ], 
  [ 39, 5, 6, 40, 1, 4, 1, 4, 5, 39, 40, 6, 39, 7, 40, 8, 12, 3, 2, 9, 10, 1, 
      4, 11, 39, 7, 8, 40, 11, 4, 10, 1, 11, 11, 11, 10, 10, 10, 2, 3 ], 
  [ 6, 40, 39, 5, 4, 1, 4, 1, 40, 6, 5, 39, 8, 40, 7, 39, 2, 9, 12, 3, 4, 11, 
      10, 1, 8, 40, 39, 7, 1, 10, 4, 11, 10, 10, 10, 11, 11, 11, 3, 2 ], 
  [ 40, 6, 5, 39, 3, 2, 3, 2, 6, 40, 39, 5, 40, 8, 39, 7, 10, 1, 4, 11, 12, 
      3, 2, 9, 40, 8, 7, 39, 9, 2, 12, 3, 9, 9, 9, 12, 12, 12, 4, 1 ], 
  [ 39, 5, 6, 40, 1, 4, 13, 15, 22, 19, 18, 23, 39, 5, 40, 6, 3, 3, 2, 2, 1, 
      1, 4, 4, 19, 22, 23, 18, 15, 15, 13, 13, 15, 15, 15, 13, 13, 13, 2, 3 ],
  [ 5, 39, 40, 6, 2, 3, 14, 16, 20, 21, 24, 17, 5, 39, 6, 40, 4, 4, 1, 1, 2, 
      2, 3, 3, 21, 20, 17, 24, 16, 16, 14, 14, 16, 16, 16, 14, 14, 14, 1, 4 ],
  [ 6, 40, 39, 5, 4, 1, 15, 13, 18, 23, 22, 19, 6, 40, 5, 39, 2, 2, 3, 3, 4, 
      4, 1, 1, 23, 18, 19, 22, 13, 13, 15, 15, 13, 13, 13, 15, 15, 15, 3, 2 ],
  [ 40, 6, 5, 39, 3, 2, 16, 14, 24, 17, 20, 21, 40, 6, 39, 5, 1, 1, 4, 4, 3, 
      3, 2, 2, 17, 24, 21, 20, 14, 14, 16, 16, 14, 14, 14, 16, 16, 16, 4, 1 ],
  [ 4, 3, 2, 1, 40, 39, 40, 39, 3, 4, 1, 2, 4, 16, 1, 14, 21, 5, 6, 24, 17, 
      40, 39, 20, 4, 16, 14, 1, 20, 39, 17, 40, 20, 20, 20, 17, 17, 17, 6, 5 ]
    , [ 4, 3, 2, 1, 40, 39, 40, 39, 3, 4, 1, 2, 15, 3, 13, 2, 5, 22, 23, 6, 
      40, 18, 19, 39, 15, 3, 2, 13, 39, 19, 40, 18, 19, 19, 19, 18, 18, 18, 
      6, 5 ], 
  [ 2, 1, 4, 3, 39, 40, 39, 40, 1, 2, 3, 4, 2, 13, 3, 15, 23, 6, 5, 22, 19, 
      39, 40, 18, 2, 13, 15, 3, 18, 40, 19, 39, 18, 18, 18, 19, 19, 19, 5, 6 ]
    , [ 2, 1, 4, 3, 39, 40, 39, 40, 1, 2, 3, 4, 14, 1, 16, 4, 6, 24, 21, 5, 
      39, 20, 17, 40, 14, 1, 4, 16, 40, 17, 39, 20, 17, 17, 17, 20, 20, 20, 
      5, 6 ], [ 1, 2, 3, 4, 5, 6, 5, 6, 2, 1, 4, 3, 1, 14, 4, 16, 17, 40, 39, 
      20, 21, 5, 6, 24, 1, 14, 16, 4, 24, 6, 21, 5, 24, 24, 24, 21, 21, 21, 
      39, 40 ], 
  [ 1, 2, 3, 4, 5, 6, 5, 6, 2, 1, 4, 3, 13, 2, 15, 3, 40, 18, 19, 39, 5, 22, 
      23, 6, 13, 2, 3, 15, 6, 23, 5, 22, 23, 23, 23, 22, 22, 22, 39, 40 ], 
  [ 3, 4, 1, 2, 6, 5, 6, 5, 4, 3, 2, 1, 3, 15, 2, 13, 19, 39, 40, 18, 23, 6, 
      5, 22, 3, 15, 13, 2, 22, 5, 23, 6, 22, 22, 22, 23, 23, 23, 40, 39 ], 
  [ 3, 4, 1, 2, 6, 5, 6, 5, 4, 3, 2, 1, 16, 4, 14, 1, 39, 20, 17, 40, 6, 24, 
      21, 5, 16, 4, 1, 14, 5, 21, 6, 24, 21, 21, 21, 24, 24, 24, 40, 39 ], 
  [ 39, 5, 6, 40, 1, 4, 13, 15, 22, 19, 18, 23, 39, 7, 40, 8, 12, 3, 2, 9, 
      10, 1, 4, 11, 19, 32, 30, 18, 28, 15, 25, 13, 28, 28, 28, 25, 25, 25, 
      2, 3 ], 
  [ 5, 39, 40, 6, 2, 3, 14, 16, 20, 21, 24, 17, 7, 39, 8, 40, 4, 11, 10, 1, 
      2, 9, 12, 3, 31, 20, 17, 29, 16, 27, 14, 26, 27, 27, 27, 26, 26, 26, 1, 
      4 ], [ 40, 6, 5, 39, 3, 2, 16, 14, 24, 17, 20, 21, 40, 8, 39, 7, 10, 1, 
      4, 11, 12, 3, 2, 9, 17, 29, 31, 20, 26, 14, 27, 16, 26, 26, 26, 27, 27, 
      27, 4, 1 ], 
  [ 6, 40, 39, 5, 4, 1, 15, 13, 18, 23, 22, 19, 8, 40, 7, 39, 2, 9, 12, 3, 4, 
      11, 10, 1, 30, 18, 19, 32, 13, 25, 15, 28, 25, 25, 25, 28, 28, 28, 3, 2 
     ], [ 3, 4, 1, 2, 6, 5, 8, 7, 11, 12, 9, 10, 16, 4, 14, 1, 39, 20, 17, 
      40, 6, 24, 21, 5, 27, 11, 10, 26, 7, 31, 8, 29, 31, 31, 31, 29, 29, 29, 
      40, 39 ], 
  [ 3, 4, 1, 2, 6, 5, 8, 7, 11, 12, 9, 10, 3, 15, 2, 13, 19, 39, 40, 18, 23, 
      6, 5, 22, 12, 28, 25, 9, 32, 7, 30, 8, 32, 32, 32, 30, 30, 30, 40, 39 ],
  [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 14, 4, 16, 17, 40, 39, 20, 21, 
      5, 6, 24, 10, 26, 27, 11, 29, 8, 31, 7, 29, 29, 29, 31, 31, 31, 39, 40 ]
    , [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 2, 15, 3, 40, 18, 19, 39, 
      5, 22, 23, 6, 25, 9, 12, 28, 8, 30, 7, 32, 30, 30, 30, 32, 32, 32, 39, 
      40 ], [ 3, 4, 1, 2, 6, 5, 8, 7, 11, 12, 9, 10, 16, 15, 14, 13, 19, 20, 
      17, 18, 23, 24, 21, 22, 27, 28, 25, 26, 32, 31, 30, 29, 38, 36, 37, 34, 
      35, 33, 40, 39 ], 
  [ 3, 4, 1, 2, 6, 5, 8, 7, 11, 12, 9, 10, 16, 15, 14, 13, 19, 20, 17, 18, 
      23, 24, 21, 22, 27, 28, 25, 26, 32, 31, 30, 29, 37, 38, 36, 35, 33, 34, 
      40, 39 ], 
  [ 3, 4, 1, 2, 6, 5, 8, 7, 11, 12, 9, 10, 16, 15, 14, 13, 19, 20, 17, 18, 
      23, 24, 21, 22, 27, 28, 25, 26, 32, 31, 30, 29, 36, 37, 38, 33, 34, 35, 
      40, 39 ], 
  [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 
      21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 35, 33, 34, 37, 38, 36, 
      39, 40 ], 
  [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 
      21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 34, 35, 33, 38, 36, 37, 
      39, 40 ], 
  [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 
      21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 
      39, 40 ], 
  [ 2, 1, 4, 3, 39, 40, 39, 40, 1, 2, 3, 4, 2, 1, 3, 4, 6, 6, 5, 5, 39, 39, 
      40, 40, 2, 1, 4, 3, 40, 40, 39, 39, 40, 40, 40, 39, 39, 39, 5, 6 ], 
  [ 4, 3, 2, 1, 40, 39, 40, 39, 3, 4, 1, 2, 4, 3, 1, 2, 5, 5, 6, 6, 40, 40, 
      39, 39, 4, 3, 2, 1, 39, 39, 40, 40, 39, 39, 39, 40, 40, 40, 6, 5 ] ]

# IsomorphismSemigroups
gap> S := FullTransformationMonoid(3);
<full transformation monoid of degree 3>
gap> T := AsMonoid(IsPBRMonoid, S);
<pbr monoid of size 27, degree 3 with 3 generators>
gap> IsomorphismSemigroups(S, T);
MappingByFunction( <full transformation monoid of degree 3>, 
<pbr monoid of size 27, degree 3 with 3 generators>
 , function( x ) ... end, function( x ) ... end )

# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(T);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/isomorph.tst");
