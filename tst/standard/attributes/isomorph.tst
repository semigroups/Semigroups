#############################################################################
##
#W  standard/attributes/isomorph.tst
#Y  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local A, BruteForceInverseCheck, BruteForceIsoCheck, D, F, G, S, T, U, V, inv
#@local map, x, y, M, N, R, L, OldOnMultiplicationTable, p, out
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
gap> D := SEMIGROUPS.CanonicalDigraph(S);;
gap> [DigraphNrVertices(D[1]), DigraphNrEdges(D[1])];
[ 12, 22 ]

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
gap> M := CanonicalMultiplicationTable(S);;
gap> Size(M) = Size(S) and ForAll(M, row -> Length(row) = Size(S));
true
gap> T := SemigroupByMultiplicationTable(OnMultiplicationTable(MultiplicationTable(S), (1, 2, 3)));;
gap> CanonicalMultiplicationTable(T) = M;
true

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
gap> M := CanonicalMultiplicationTable(S);;
gap> Size(M) = Size(S) and ForAll(M, row -> Length(row) = Size(S));
true
gap> T := SemigroupByMultiplicationTable(OnMultiplicationTable(MultiplicationTable(S), (1, 2, 3)));;
gap> CanonicalMultiplicationTable(T) = M;
true

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

# OnMultiplicationTable/PermuteMultiplicationTable
gap> OldOnMultiplicationTable := function(table, p)
>  local out;
>  out := Permuted(table, p);
>  Apply(out, x -> OnTuples(x, p));
>  Apply(out, x -> Permuted(x, p));
>  return out;
> end;;
gap> S := MultiplicationTable(DualSymmetricInverseMonoid(2));;
gap> p := (1, 2);;
gap> OldOnMultiplicationTable(S, p) = OnMultiplicationTable(S, p);
true
gap> p := (1, 3);;
gap> OldOnMultiplicationTable(S, p) = OnMultiplicationTable(S, p);
true
gap> p := (2, 3);;
gap> OldOnMultiplicationTable(S, p) = OnMultiplicationTable(S, p);
true
gap> p := (1, 2, 3);;
gap> OldOnMultiplicationTable(S, p) = OnMultiplicationTable(S, p);
true
gap> p := (1, 2, 3, 4);;
gap> OnMultiplicationTable(S, p);
Error, List Element: <list>[4] must have an assigned value
gap> PermuteMultiplicationTable(List([1 .. 3], x -> [1 .. 3]), S, 1);
Error, the argument <p> must be a permutation but found type integer
gap> PermuteMultiplicationTable(List([1 .. 4], x -> [1 .. 4]), S, (1, 2));
Error, the arguments <output> and <table> must have the same dimensions but fo\
und lengths 4 and 3, respectively
gap> M := [[1 .. 4], [1 .. 4]];;
gap> PermuteMultiplicationTable(M, S, (1, 2));
Error, the arguments <output> and <table> must have the same dimensions but fo\
und lengths 2 and 3, respectively
gap> S := M;;
gap> PermuteMultiplicationTable(M, S, (1, 2));
Error, the first argument <output> must be a square table, (expected 2 element\
s per row, but found a row of length 4)
gap> S := MultiplicationTable(SymmetricInverseMonoid(2));;
gap> M := List([1 .. 7], x -> [1 .. 7]);;
gap> p := (1, 4, 2, 5, 7);;
gap> OnMultiplicationTable(S, p) = M;
false
gap> PermuteMultiplicationTable(M, S, p);
gap> OnMultiplicationTable(S, p) = M;
true
gap> OnMultiplicationTable([[1, 3], [1, 2]], (1, 2));
Error, all entries in the second argument <table> must be positive integers fr\
om 1 to Size(<table>) = 2

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/attributes/isomorph.tst");
