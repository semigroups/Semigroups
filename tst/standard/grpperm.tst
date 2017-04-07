#############################################################################
##
#W  standard/grpperm.tst
#Y  Copyright (C) 2015                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/grpperm.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Load a function for isomorphism checking
gap> BruteForceIsoCheck := function(iso)
>   local x, y;
>   if not IsInjective(iso) or not IsSurjective(iso) then
>     return false;
>   fi;
>   #homomorphism
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

#T# IsomorphismPermGroup: for a transformation semigroup
gap> S := Semigroup([Transformation([3, 2, 4, 1]), Transformation([2, 1])]);
<transformation semigroup of degree 4 with 2 generators>
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (1,3,4), (1,2) ])
gap> S := Semigroup([Transformation([2, 1, 2]), Transformation([3, 3, 1])]);
<transformation semigroup of degree 3 with 2 generators>
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument <S> must satisfy IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Rees Matrix Semigroup
gap> R := ReesMatrixSemigroup(Group((1, 3, 5), (2, 4)), [[()]]);;
gap> iso := IsomorphismPermGroup(R);;
gap> G := Range(iso);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> R := ReesMatrixSemigroup(Group((1, 3, 5), (2, 4)), [[(1, 5, 3), ()]]);
<Rees matrix semigroup 2x1 over Group([ (1,3,5), (2,4) ])>
gap> iso := IsomorphismPermGroup(R);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a FP Semigroup
gap> F := FreeSemigroup(2);
<free semigroup on the generators [ s1, s2 ]>
gap> S := F /
> [[F.1 ^ 3, F.1],
> [F.1 ^ 2 * F.2, F.1 * F.2],
> [F.1 * F.2 * F.1, F.2 * F.1],
> [F.2 * F.1 ^ 2, F.1 * F.2 ^ 2],
> [F.2 * F.1 * F.2, F.1 * F.2],
> [F.2 ^ 2 * F.1, F.2 * F.1],
> [F.2 ^ 3, F.2]];
<fp semigroup on the generators [ s1, s2 ]>
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,
gap> S := F /
> [[F.1 ^ 3, F.2 ^ 2],
> [F.1 * F.2 ^ 2, F.1],
> [F.2 ^ 2 * F.1, F.1],
> [F.2 ^ 3, F.2],
> [(F.1 * F.2) ^ 2 * F.1, F.2 * F.1 ^ 2 * F.2],
> [(F.2 * F.1) ^ 2 * F.2, F.1 ^ 2 * F.2 * F.1 ^ 2],
> [(F.1 ^ 2 * F.2) ^ 2, (F.2 * F.1) ^ 2],
> [(F.2 * F.1 ^ 2) ^ 2, (F.1 * F.2) ^ 2],
> [F.2 * (F.1 * F.2 * F.1) ^ 2, (F.1 * F.2 * F.1) ^ 2 * F.2]];
<fp semigroup on the generators [ s1, s2 ]>
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (1,3,6)(2,5,9)(4,8,12)(7,11,16)(10,15,20)(13,14,19)(17,18,21)
(22,23,24), (1,4)(2,6)(3,7)(5,10)(8,13)(9,14)(11,17)(12,18)(15,16)(19,22)
(20,23)(21,24) ])

#T# IsomorphismPermGroup: for a PBR Semigroup
gap> S := Semigroup([PBR([[-1], [-4], [-2], [-3]], [[1], [3], [4], [2]]),
>                    PBR([[-2], [-1], [-3], [-4]], [[2], [1], [3], [4]])]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (1,3,6)(2,5,9)(4,8,12)(7,11,16)(10,15,20)(13,14,19)(17,18,21)
(22,23,24), (1,4)(2,6)(3,7)(5,10)(8,13)(9,14)(11,17)(12,18)(15,16)(19,22)
(20,23)(21,24) ])
gap> S := Semigroup([PBR([[2], [2]], [[], []]),
>                    PBR([[], [-2]], [[], [-2, 2]])]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Bipartition Semigroup
gap> S := Semigroup([Bipartition([[1, 2, -2, -3], [3], [-1]]),
>                    Bipartition([[1, 2, -2], [3, -3], [-1]])]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,
gap> S := Semigroup([PBR([[-2], [-3], [-1]], [[3], [1], [2]])]);
<commutative pbr semigroup of degree 3 with 1 generator>
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (1,2,3) ])

#T# IsomorphismPermGroup: for a Transformation Semigroup
gap> S := Semigroup([Transformation([3, 4, 1, 2, 6, 5]),
>                    Transformation([4, 5, 2, 6, 3, 1])]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (1,3)(2,4)(5,6), (1,4,6)(2,5,3) ])
gap> S := Semigroup([Transformation([2, 1, 2]), Transformation([3, 3, 1])]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument <S> must satisfy IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Partial Perm Semigroup
gap> S := Semigroup([PartialPerm([1, 2, 3, 5], [2, 1, 3, 5]),
>                    PartialPerm([1, 2, 3, 5], [1, 2, 5, 3])]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (1,2), (3,5) ])
gap> S := Monoid(PartialPerm([1, 2, 3, 5], [2, 1, 3, 5]),
>                PartialPerm([1, 2, 3, 5], [1, 2, 5, 3]));;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (), (1,2), (3,5) ])
gap> S := Semigroup([PartialPerm([1, 2], [2, 1]),
>                    PartialPerm([1, 3], [3, 1])]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument <S> must be a partial perm semigroup satisfying IsGroupAsSemigrou\
p,

#T# IsomorphismPermGroup: for a Boolean Mat Semigroup
gap> S := Semigroup([Matrix(IsBooleanMat, [[0, 1, 0], [0, 0, 1], [1, 0, 0]])]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (1,2,3) ])
gap> S := Semigroup([Matrix(IsBooleanMat, [[1, 0, 1], [0, 1, 1], [0, 0, 0]]),
>                    Matrix(IsBooleanMat, [[1, 1, 1], [1, 0, 0], [0, 0, 0]])]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Max Plus Matrix Semigroup
gap> S := Semigroup([Matrix(IsMaxPlusMatrix, [[-infinity, 0, -infinity],
>                                             [-infinity, -infinity, 0],
>                                             [0, -infinity, -infinity]])]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (1,2,3) ])
gap> S := Monoid([Matrix(IsMaxPlusMatrix,
> [[0, -infinity, -infinity, -infinity],
>  [-infinity, -infinity, 0, -infinity],
>  [-infinity, -infinity, -infinity, 0],
>  [-infinity, -infinity, -infinity, 0]])]);
<commutative monoid of 4x4 max-plus matrices with 1 generator>
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Min Plus Matrix Semigroup
gap> S := Semigroup([Matrix(IsMinPlusMatrix,
>                             [[infinity, 0, infinity, infinity],
>                              [infinity, infinity, 0, infinity],
>                              [infinity, infinity, infinity, 0],
>                              [0, infinity, infinity, infinity]]),
>                      Matrix(IsMinPlusMatrix,
>                             [[infinity, infinity, 0, infinity],
>                              [infinity, infinity, infinity, 0],
>                              [0, infinity, infinity, infinity],
>                              [infinity, 0, infinity, infinity]])]);
<semigroup of 4x4 min-plus matrices with 2 generators>
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (1,2,3,4), (1,3)(2,4) ])
gap> S := Semigroup(
> [Matrix(IsMinPlusMatrix, [[infinity, 0, infinity], [0, infinity, infinity],
>     [infinity, 0, infinity]]),
> Matrix(IsMinPlusMatrix, [[infinity, infinity, 0], [infinity, infinity, 0],
>     [0, infinity, infinity]])]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Tropical Max Plus Matrix Semigroup
gap> S := Semigroup(
> [Matrix(IsTropicalMaxPlusMatrix, [[-infinity, -infinity, 0, -infinity],
>     [-infinity, -infinity, -infinity, 0],
>     [0, -infinity, -infinity, -infinity],
>     [-infinity, 0, -infinity, -infinity]], 2),
> Matrix(IsTropicalMaxPlusMatrix, [[-infinity, -infinity, -infinity, 0],
>     [-infinity, -infinity, 0, -infinity],
>     [-infinity, 0, -infinity, -infinity],
>     [0, -infinity, -infinity, -infinity]], 2)]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (1,3)(2,4), (1,4)(2,3) ])
gap> S := Semigroup([Matrix(IsTropicalMaxPlusMatrix, [[3, 0], [2, 1]], 3),
>  Matrix(IsTropicalMaxPlusMatrix, [[1, 1], [0, -infinity]], 3),
>  Matrix(IsTropicalMaxPlusMatrix, [[1, -infinity], [1, 0]], 3)]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Tropical Min Plus Matrix Semigroup
gap> S := Semigroup([
> Matrix(IsTropicalMinPlusMatrix,
>    [[infinity, 0, infinity, infinity, infinity],
>     [infinity, infinity, 0, infinity, infinity],
>     [infinity, infinity, infinity, 0, infinity],
>     [infinity, infinity, infinity, infinity, 0],
>     [0, infinity, infinity, infinity, infinity]], 2)]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (1,2,3,4,5) ])
gap> S := Monoid(
> [Matrix(IsTropicalMinPlusMatrix, [[infinity, 0], [infinity, 0]], 2)]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Projective Max Plus Matrix Semigroup
gap> S := Semigroup([
> Matrix(IsProjectiveMaxPlusMatrix,
>    [[-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>     [-infinity, -infinity, -infinity, 0, -infinity, -infinity],
>     [0, -infinity, -infinity, -infinity, -infinity, -infinity],
>     [-infinity, 0, -infinity, -infinity, -infinity, -infinity],
>     [-infinity, -infinity, -infinity, -infinity, -infinity, 0],
>     [-infinity, -infinity, -infinity, -infinity, 0, -infinity]]),
> Matrix(IsProjectiveMaxPlusMatrix,
>    [[-infinity, -infinity, -infinity, 0, -infinity, -infinity],
>     [-infinity, -infinity, -infinity, -infinity, 0, -infinity],
>     [-infinity, 0, -infinity, -infinity, -infinity, -infinity],
>     [-infinity, -infinity, -infinity, -infinity, -infinity, 0],
>     [-infinity, -infinity, 0, -infinity, -infinity, -infinity],
>     [0, -infinity, -infinity, -infinity, -infinity, -infinity]])]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (1,3)(2,4)(5,6), (1,4,6)(2,5,3) ])
gap> S := Semigroup([
> Matrix(IsProjectiveMaxPlusMatrix, [[-infinity, 0, -infinity],
>     [0, -infinity, -infinity], [-infinity, 0, -infinity]]),
> Matrix(IsProjectiveMaxPlusMatrix, [[-infinity, -infinity, 0],
>     [-infinity, -infinity, 0], [0, -infinity, -infinity]])]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a NTP Matrix Semigroup
gap> S := Semigroup([
> Matrix(IsNTPMatrix, [[0, 0, 1, 0, 0, 0], [0, 0, 0, 1, 0, 0],
>     [1, 0, 0, 0, 0, 0], [0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1],
>     [0, 0, 0, 0, 1, 0]], 2, 3),
> Matrix(IsNTPMatrix, [[0, 0, 0, 1, 0, 0], [0, 0, 0, 0, 1, 0],
>     [0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1], [0, 0, 1, 0, 0, 0],
>     [1, 0, 0, 0, 0, 0]], 2, 3)]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (1,3)(2,4)(5,6), (1,4,6)(2,5,3) ])
gap> S := Semigroup([
> Matrix(IsNTPMatrix, [[0, 0, 1, 0], [1, 0, 0, 2], [0, 4, 0, 2],
>     [1, 3, 0, 5]], 88, 6)]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Block Bijection Semigroup
gap> S := InverseSemigroup(
> [Bipartition([[1, -2], [2, -1], [3, -3], [4, -4], [5, -5],
>    [6, -6]]), Bipartition([[1, -1], [2, -2], [3, -4],
>    [4, -5], [5, -3], [6, -6]])]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (1,2), (3,4,5) ])
gap> S :=
> Semigroup([Bipartition([[1, 4, -4], [2, -3], [3, -1, -2]]),
>  Bipartition([[1, 2, -2, -3], [3, -1], [4, -4]])]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a block bijection monoid
gap> S := InverseMonoid(
> [Bipartition([[1, -2], [2, -1], [3, -3], [4, -4], [5, -5],
>    [6, -6]]), Bipartition([[1, -1], [2, -2], [3, -4],
>    [4, -5], [5, -3], [6, -6]])]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (), (1,2), (3,4,5) ])

#T# IsomorphismPermGroup: for a Integer Matrix Semigroup
gap> S := Semigroup(
> [Matrix(IsIntegerMatrix, [[0, 0, 1, 0, 0, 0], [0, 0, 0, 1, 0, 0],
>     [1, 0, 0, 0, 0, 0], [0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1],
>     [0, 0, 0, 0, 1, 0]]),
> Matrix(IsIntegerMatrix, [[0, 0, 0, 1, 0, 0], [0, 0, 0, 0, 1, 0],
>     [0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1], [0, 0, 1, 0, 0, 0],
>     [1, 0, 0, 0, 0, 0]])]);;
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true
gap> G := Range(iso);
Group([ (1,3)(2,4)(5,6), (1,4,6)(2,5,3) ])
gap> S := Semigroup(
> [Matrix(IsIntegerMatrix, [[0, 0, 0, 1, 0, 0, 0], [1, 0, 0, 0, 0, 0, 0],
>     [0, 0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 1], [0, 0, 0, 0, 1, 0, 0],
>     [0, 0, 0, 0, 1, 0, 0], [0, 0, 0, 0, 0, 0, 1]])]);;
gap> iso := IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,

#T# IsomorphismPermGroup: for a Integer Matrix Semigroup
gap> S := GraphInverseSemigroup(Digraph([[]]));
<finite graph inverse semigroup with 1 vertex, 0 edges>
gap> iso := IsomorphismPermGroup(S);;
gap> BruteForceIsoCheck(iso); BruteForceInverseCheck(iso);
true
true

#T# IsomorphismPermGroup
gap> S := RegularBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> IsomorphismPermGroup(S);
Error, Semigroups: IsomorphismPermGroup: usage,
the argument must be a semigroup satisfying IsGroupAsSemigroup,
gap> S := Semigroup([BooleanMat([[0, 1, 0], [1, 0, 0], [0, 0, 1]]),
> BooleanMat([[0, 1, 0], [0, 0, 1], [1, 0, 0]])]);;
gap> IsomorphismPermGroup(S);
MappingByFunction( <group of size 6, 3x3 boolean matrices with 2 generators>, 
<group of size 6, with 2 generators>
, function( x ) ... end, function( x ) ... end )

#T# IsomorphismPermGroup, infinite 1 / 1
gap> IsomorphismPermGroup(FreeMonoid(3));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `IsomorphismPermGroup' on 1 arguments

#T# IsomorphismPermGroup, for a block bijection semigroup
gap> S := Semigroup(Bipartition([[1, 2, -3, -4], [3, 4, -1, -2]]));;
gap> IsomorphismPermGroup(S);
MappingByFunction( <block bijection group of degree 4 with 1 generator>
, Group([ (1,2) ]), function( x ) ... end, function( x ) ... end )
gap> BruteForceIsoCheck(last);
true
gap> BruteForceInverseCheck(last2);
true

#T# IteratorSortedConjugateStabChain
gap> G := AlternatingGroup(5);;
gap> S := StabChainImmutable(G);;
gap> iter := SEMIGROUPS.IteratorSortedConjugateStabChain(S, (5, 10));
<iterator>
gap> ListIterator(iter) = AsSet(G ^ (5, 10));
true

#T# SEMIGROUPS.LargestElementConjugateStabChain, 1
gap> conj := (3, 9);;
gap> G1 := AlternatingGroup(5);;
gap> base1 := [1 .. 3];;
gap> G2 := G1 ^ conj;;
gap> base2 := OnTuples(base1, conj);;
gap> S1 := StabChainOp(G1, rec(base := base1));
<stabilizer chain record, Base [ 1, 2, 3 ], Orbit length 5, Size: 60>
gap> p1 := SEMIGROUPS.LargestElementConjugateStabChain(S1, (), conj);
(1,9,4,2,5)
gap> q1 := SEMIGROUPS.SmallestElementConjugateStabChain(S1, (), conj);
(4,5,9)
gap> S2 := StabChainOp(G2, rec(base := base2));
<stabilizer chain record, Base [ 1, 2, 9 ], Orbit length 5, Size: 60>
gap> p2 := LargestElementStabChain(S2, ());
(1,9,4,2,5)
gap> p1 = p2;
true
gap> OnTuples(base1, conj * p1);
[ 9, 5, 4 ]
gap> OnTuples(base2, p1);
[ 9, 5, 4 ]
gap> OnTuples(base2, p2);
[ 9, 5, 4 ]
gap> OnTuples(base2, q1);
[ 1, 2, 4 ]
gap> Minimum(List(G2, x -> OnTuples(base2, x)));
[ 1, 2, 4 ]

#T# SEMIGROUPS.LargestElementConjugateStabChain, 2
gap> conj := (1, 10, 7, 4, 3, 6, 2, 9, 8, 5);;
gap> G1 := AlternatingGroup(5);;
gap> base1 := [1 .. 3];;
gap> G2 := G1 ^ conj;;
gap> base2 := OnTuples(base1, conj);;
gap> S1 := StabChainOp(G1, rec(base := base1));
<stabilizer chain record, Base [ 1, 2, 3 ], Orbit length 5, Size: 60>
gap> p1 := SEMIGROUPS.LargestElementConjugateStabChain(S1, (), conj);
()
gap> q1 := SEMIGROUPS.SmallestElementConjugateStabChain(S1, (), conj);
(1,10)(3,9)
gap> S2 := StabChainOp(G2, rec(base := base2));
<stabilizer chain record, Base [ 10, 9, 6 ], Orbit length 5, Size: 60>
gap> p2 := LargestElementStabChain(S2, ());
()
gap> p1 = p2;
true
gap> OnTuples(base1, conj * p1);
[ 10, 9, 6 ]
gap> OnTuples(base2, p1);
[ 10, 9, 6 ]
gap> OnTuples(base2, p2);
[ 10, 9, 6 ]
gap> OnTuples(base2, q1);
[ 1, 3, 6 ]
gap> Minimum(List(G2, x -> OnTuples(base2, x)));
[ 1, 3, 6 ]

#T# SEMIGROUPS.LargestElementConjugateStabChain, 3
gap> conj := (1, 10, 7, 4, 3, 6, 2, 9, 8, 5);;
gap> G1 := AlternatingGroup(10);;
gap> base1 := [1, 2, 3, 4, 5, 6, 7, 8];;
gap> G2 := G1 ^ conj;;
gap> base2 := OnTuples(base1, conj);;
gap> S1 := StabChainOp(G1, rec(base := base1));
<stabilizer chain record, Base [ 1, 2, 3, 4, 5, 6, 7, 8 ], Orbit length 
10, Size: 1814400>
gap> p1 := SEMIGROUPS.LargestElementConjugateStabChain(S1, (), conj);
(1,6,8,2,5,3,7)
gap> q1 := SEMIGROUPS.SmallestElementConjugateStabChain(S1, (), conj);
(1,5,8,10)(2,6,3,4,7,9)
gap> S2 := StabChainOp(G2, rec(base := base2));
<stabilizer chain record, Base [ 10, 9, 6, 3, 1, 2, 4, 5 ], Orbit length 
10, Size: 1814400>
gap> p2 := LargestElementStabChain(S2, ());
(1,6,8,2,5,3,7)
gap> p1 = p2;
true
gap> OnTuples(base1, conj * p1);
[ 10, 9, 8, 7, 6, 5, 4, 3 ]
gap> OnTuples(base2, p1);
[ 10, 9, 8, 7, 6, 5, 4, 3 ]
gap> OnTuples(base2, p2);
[ 10, 9, 8, 7, 6, 5, 4, 3 ]
gap> OnTuples(base2, q1);
[ 1, 2, 3, 4, 5, 6, 7, 8 ]

#gap> Minimum(List(G2, x -> OnTuples(base2, x)));
#[ 1, 2, 3, 4, 5, 6, 7, 8 ]

#T# SEMIGROUPS.LargestElementConjugateStabChain, 4
gap> conj := (1, 13, 6, 8, 4, 12, 7, 5, 2, 3, 11)(9, 10);;
gap> G1 := Group((2, 5)(3, 4)(7, 9, 10, 8), (7, 10)(8, 9), (1, 2, 3, 4, 5), 
>               (6, 7, 8, 9, 10));;
gap> base1 := [1, 7, 6];;
gap> G2 := G1 ^ conj;;
gap> base2 := OnTuples(base1, conj);;
gap> S1 := StabChainOp(G1, rec(base := base1));
<stabilizer chain record, Base [ 1, 7, 6 ], Orbit length 5, Size: 100>
gap> p1 := SEMIGROUPS.LargestElementConjugateStabChain(S1, (), conj);
(5,10)(8,9)
gap> q1 := SEMIGROUPS.SmallestElementConjugateStabChain(S1, (), conj);
(2,12,11,3,13)(4,10,9,8,5)
gap> S2 := StabChainOp(G2, rec(base := base2));
<stabilizer chain record, Base [ 13, 5, 8 ], Orbit length 5, Size: 100>
gap> p2 := LargestElementStabChain(S2, ());
(5,10)(8,9)
gap> p1 = p2;
true
gap> OnTuples(base1, conj * p1);
[ 13, 10, 9 ]
gap> OnTuples(base2, p1);
[ 13, 10, 9 ]
gap> OnTuples(base2, p2);
[ 13, 10, 9 ]
gap> OnTuples(base2, q1);
[ 2, 4, 5 ]
gap> Minimum(List(G2, x -> OnTuples(base2, x)));
[ 2, 4, 5 ]

#T# SEMIGROUPS.LargestElementConjugateStabChain, 5
gap> conj := (1, 10, 4, 2, 9, 11, 5, 8, 3, 6)(7, 12);;
gap> G1 := Group((2, 5)(3, 4)(7, 9, 10, 8), (7, 10)(8, 9), (1, 2, 3, 4, 5), 
>               (6, 7, 8, 9, 10));;
gap> base1 := [1, 6, 7];;
gap> G2 := G1 ^ conj;;
gap> base2 := OnTuples(base1, conj);;
gap> S1 := StabChainOp(G1, rec(base := base1));
<stabilizer chain record, Base [ 1, 6, 7 ], Orbit length 5, Size: 100>
gap> p1 := SEMIGROUPS.LargestElementConjugateStabChain(S1, (), conj);
(1,12,11,3)(2,6)(8,9)
gap> q1 := SEMIGROUPS.SmallestElementConjugateStabChain(S1, (), conj);
(2,10)(3,4,11,12)(6,9)
gap> S2 := StabChainOp(G2, rec(base := base2));
<stabilizer chain record, Base [ 10, 1, 12 ], Orbit length 5, Size: 100>
gap> p2 := LargestElementStabChain(S2, ());
(1,12,11,3)(2,6)(8,9)
gap> p1 = p2;
true
gap> OnTuples(base1, conj * p1);
[ 10, 12, 11 ]
gap> OnTuples(base2, p1);
[ 10, 12, 11 ]
gap> OnTuples(base2, p2);
[ 10, 12, 11 ]
gap> OnTuples(base2, q1);
[ 2, 1, 3 ]
gap> Minimum(List(G2, x -> OnTuples(base2, x)));
[ 2, 1, 3 ]

#T# SEMIGROUPS.LargestElementConjugateStabChain, 6
gap> conj := (1, 10, 4, 2, 9, 11, 5, 8, 3, 6)(7, 12);;
gap> G1 := Group((2, 5)(3, 4)(7, 9, 10, 8), (7, 10)(8, 9), (1, 2, 3, 4, 5), 
>               (6, 7, 8, 9, 10));;
gap> base1 := [1, 2, 6, 7];;
gap> G2 := G1 ^ conj;;
gap> base2 := OnTuples(base1, conj);;
gap> S1 := StabChainOp(G1, rec(base := base1));
<stabilizer chain record, Base [ 1, 2, 6, 7 ], Orbit length 5, Size: 100>
gap> p1 := SEMIGROUPS.LargestElementConjugateStabChain(S1, (), conj);
(1,12,3,11,4)
gap> q1 := SEMIGROUPS.SmallestElementConjugateStabChain(S1, (), conj);
(2,10)(3,4,11,12)(6,9)
gap> S2 := StabChainOp(G2, rec(base := base2));
<stabilizer chain record, Base [ 10, 9, 1, 12 ], Orbit length 5, Size: 100>
gap> p2 := LargestElementStabChain(S2, ());
(1,12,3,11,4)
gap> p1 = p2;
true
gap> OnTuples(base1, conj * p1);
[ 10, 9, 12, 3 ]
gap> OnTuples(base2, p1);
[ 10, 9, 12, 3 ]
gap> OnTuples(base2, p2);
[ 10, 9, 12, 3 ]
gap> OnTuples(base2, q1);
[ 2, 6, 1, 3 ]
gap> Minimum(List(G2, x -> OnTuples(base2, x)));
[ 2, 6, 1, 3 ]

#T# SEMIGROUPS.LargestElementConjugateStabChain, 7
gap> conj := (1, 10, 4, 2, 9, 11, 5, 8, 3, 6)(7, 12);;
gap> G1 := Group((2, 5)(3, 4)(7, 9, 10, 8), (7, 10)(8, 9), (1, 2, 3, 4, 5), 
>               (6, 7, 8, 9, 10));;
gap> base1 := [4, 5, 9, 10];;
gap> G2 := G1 ^ conj;;
gap> base2 := OnTuples(base1, conj);;
gap> S1 := StabChainOp(G1, rec(base := base1));
<stabilizer chain record, Base [ 4, 5, 9, 10 ], Orbit length 5, Size: 100>
gap> p1 := SEMIGROUPS.LargestElementConjugateStabChain(S1, (), conj);
(1,11,12,4,3)(2,10,6,8,9)
gap> q1 := SEMIGROUPS.SmallestElementConjugateStabChain(S1, (), conj);
(1,4,3,11)(6,8)(9,10)
gap> S2 := StabChainOp(G2, rec(base := base2));
<stabilizer chain record, Base [ 2, 8, 11, 4 ], Orbit length 5, Size: 100>
gap> p2 := LargestElementStabChain(S2, ());
(1,11,12,4,3)(2,10,6,8,9)
gap> p1 = p2;
true
gap> OnTuples(base1, conj * p1);
[ 10, 9, 12, 3 ]
gap> OnTuples(base2, p1);
[ 10, 9, 12, 3 ]
gap> OnTuples(base2, p2);
[ 10, 9, 12, 3 ]
gap> OnTuples(base2, q1);
[ 2, 6, 1, 3 ]
gap> Minimum(List(G2, x -> OnTuples(base2, x)));
[ 2, 6, 1, 3 ]

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(BruteForceInverseCheck);
gap> Unbind(BruteForceIsoCheck);
gap> Unbind(F);
gap> Unbind(G);
gap> Unbind(R);
gap> Unbind(S);
gap> Unbind(iso);
gap> Unbind(x);
gap> Unbind(y);

#E#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/grpperm.tst");
