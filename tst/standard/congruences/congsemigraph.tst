############################################################################
##
##  congsemigraph.tst
##  Copyright (C) 2022                     Marina Anagnostopoulou-Merkouri
##                                                          James Mitchell
##
##  Licensing information can be found in the README file of this package.
##
############################################################################
#

#@local C, D, L, S, cong, cong1, cong2, e_1, e_3, i, j, join, meet, pos, val
gap> START_TEST("Semigroups package: standard/congruences/congsemigraph.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# IsCongruenceByWangPair
gap> D := Digraph([[3, 4], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 5 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 5 edges>
gap> cong := CongruenceByWangPair(S, [3, 4], []);
<graph inverse semigroup congruence with H = [ 3, 4 ] and W = [  ]>
gap> IsCongruenceByWangPair(cong);
true
gap> cong := CongruenceByWangPair(S, [4], [2]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 2 ]>
gap> IsCongruenceByWangPair(cong);
true
gap> e_1 := S.1;
e_1
gap> e_3 := S.3;
e_3
gap> cong := SemigroupCongruence(S, [[e_1, e_3]]);
<2-sided semigroup congruence over <finite graph inverse semigroup with 
4 vertices, 5 edges> with 1 generating pairs>
gap> IsCongruenceByWangPair(cong);
false

# CongruenceByWangPair
gap> D := Digraph([[3, 4], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 5 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 5 edges>
gap> cong := CongruenceByWangPair(S, [3, 4], []);
<graph inverse semigroup congruence with H = [ 3, 4 ] and W = [  ]>
gap> cong := CongruenceByWangPair(S, [4], [2]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 2 ]>
gap> cong := CongruenceByWangPair(S, [3, 4], []);
<graph inverse semigroup congruence with H = [ 3, 4 ] and W = [  ]>
gap> cong := CongruenceByWangPair(S, [4], [1]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 1 ]>
gap> D := Digraph([[2], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> cong := CongruenceByWangPair(S, [4], [2]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 2 ]>
gap> cong := CongruenceByWangPair(S, [], [1, 3]);
<graph inverse semigroup congruence with H = [  ] and W = [ 1, 3 ]>
gap> cong := CongruenceByWangPair(S, [4], [2]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 2 ]>
gap> cong := CongruenceByWangPair(S, [4], [1]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 1 ]>
gap> cong := CongruenceByWangPair(S, [3, 4], []);
<graph inverse semigroup congruence with H = [ 3, 4 ] and W = [  ]>
gap> cong := CongruenceByWangPair(S, [], [1, 3]);
<graph inverse semigroup congruence with H = [  ] and W = [ 1, 3 ]>
gap> cong := CongruenceByWangPair(S, [3], []);
Error, the 2nd argument (a list) is not a valid hereditary set
gap> cong := CongruenceByWangPair(S, [], [4]);
Error, the 3rd argument (a list) is not a valid W-set
gap> D := Digraph([[2, 3], [3], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> CongruenceByWangPair(S, [4], []);
<graph inverse semigroup congruence with H = [ 4 ] and W = [  ]>
gap> CongruenceByWangPair(S, [3, 4], [1]);
<graph inverse semigroup congruence with H = [ 3, 4 ] and W = [ 1 ]>
gap> CongruenceByWangPair(S, [4], [2]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 2 ]>
gap> CongruenceByWangPair(S, [], []);
<graph inverse semigroup congruence with H = [  ] and W = [  ]>
gap> CongruenceByWangPair(S, [3], [4]);
Error, the 2nd argument (a list) is not a valid hereditary set

# AsSemigroupCongruenceByGeneratingPairs
gap> D := Digraph([[2, 3], [3], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> C := CongruenceByWangPair(S, [4], [2]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 2 ]>
gap> AsSemigroupCongruenceByGeneratingPairs(C);
<2-sided semigroup congruence over <finite graph inverse semigroup with 
4 vertices, 4 edges> with 2 generating pairs>
gap> EquivalenceRelationPartition(C);
[ [ e_1, e_1e_3e_3^-1 ], 
  [ e_4, v_4, e_4^-1, 0, e_2e_4, e_3e_4, e_4e_4^-1, e_4^-1e_2^-1, 
      e_4^-1e_3^-1, e_1e_3e_4, e_2e_4e_4^-1, e_3e_4e_4^-1, e_4e_4^-1e_2^-1, 
      e_4e_4^-1e_3^-1, e_4^-1e_3^-1e_1^-1, e_1e_3e_4e_4^-1, 
      e_2e_4e_4^-1e_2^-1, e_2e_4e_4^-1e_3^-1, e_3e_4e_4^-1e_2^-1, 
      e_3e_4e_4^-1e_3^-1, e_4e_4^-1e_3^-1e_1^-1, e_1e_3e_4e_4^-1e_2^-1, 
      e_1e_3e_4e_4^-1e_3^-1, e_2e_4e_4^-1e_3^-1e_1^-1, 
      e_3e_4e_4^-1e_3^-1e_1^-1, e_1e_3e_4e_4^-1e_3^-1e_1^-1 ], 
  [ v_2, e_3e_3^-1 ], [ e_1^-1, e_3e_3^-1e_1^-1 ], 
  [ e_1e_1^-1, e_1e_3e_3^-1e_1^-1 ] ]
gap> D := ChainDigraph(4);
<immutable chain digraph with 4 vertices>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 3 edges>
gap> CongruenceByWangPair(S, [4], [1, 2]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 1, 2 ]>
gap> cong := AsSemigroupCongruenceByGeneratingPairs(last);
<2-sided semigroup congruence over <finite graph inverse semigroup with 
4 vertices, 3 edges> with 3 generating pairs>
gap> EquivalenceRelationPartition(cong);
[ [ e_1, e_1e_2e_2^-1 ], 
  [ e_3, v_4, e_3^-1, 0, e_2e_3, e_3e_3^-1, e_3^-1e_2^-1, e_1e_2e_3, 
      e_2e_3e_3^-1, e_3e_3^-1e_2^-1, e_3^-1e_2^-1e_1^-1, e_1e_2e_3e_3^-1, 
      e_2e_3e_3^-1e_2^-1, e_3e_3^-1e_2^-1e_1^-1, e_1e_2e_3e_3^-1e_2^-1, 
      e_2e_3e_3^-1e_2^-1e_1^-1, e_1e_2e_3e_3^-1e_2^-1e_1^-1 ], 
  [ v_1, e_1e_1^-1, e_1e_2e_2^-1e_1^-1 ], [ v_2, e_2e_2^-1 ], 
  [ e_1^-1, e_2e_2^-1e_1^-1 ] ]
gap> CongruenceByWangPair(S, [2, 3, 4], []);
<graph inverse semigroup congruence with H = [ 2, 3, 4 ] and W = [  ]>
gap> cong := AsSemigroupCongruenceByGeneratingPairs(last);
<2-sided semigroup congruence over <finite graph inverse semigroup with 
4 vertices, 3 edges> with 3 generating pairs>
gap> EquivalenceRelationPartition(cong);
[ [ e_1, e_2, e_3, v_2, v_3, v_4, e_1^-1, e_2^-1, e_3^-1, 0, e_1e_2, 
      e_1e_1^-1, e_2e_3, e_2e_2^-1, e_3e_3^-1, e_2^-1e_1^-1, e_3^-1e_2^-1, 
      e_1e_2e_3, e_1e_2e_2^-1, e_2e_3e_3^-1, e_2e_2^-1e_1^-1, e_3e_3^-1e_2^-1,
      e_3^-1e_2^-1e_1^-1, e_1e_2e_3e_3^-1, e_1e_2e_2^-1e_1^-1, 
      e_2e_3e_3^-1e_2^-1, e_3e_3^-1e_2^-1e_1^-1, e_1e_2e_3e_3^-1e_2^-1, 
      e_2e_3e_3^-1e_2^-1e_1^-1, e_1e_2e_3e_3^-1e_2^-1e_1^-1 ] ]
gap> CongruenceByWangPair(S, [], [1]);
<graph inverse semigroup congruence with H = [  ] and W = [ 1 ]>
gap> cong := AsSemigroupCongruenceByGeneratingPairs(last);
<2-sided semigroup congruence over <finite graph inverse semigroup with 
4 vertices, 3 edges> with 1 generating pairs>
gap> EquivalenceRelationPartition(cong);
[ [ v_1, e_1e_1^-1 ] ]

# GeneratingCongruencesOfSemigroup
gap> D := Digraph([[2], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> GeneratingCongruencesOfLattice(S)[1];
<graph inverse semigroup congruence with H = [  ] and W = [ 1 ]>
gap> GeneratingCongruencesOfLattice(S)[2];
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 2 ]>
gap> GeneratingCongruencesOfLattice(S)[3];
<graph inverse semigroup congruence with H = [  ] and W = [ 3 ]>
gap> GeneratingCongruencesOfLattice(S)[4];
<graph inverse semigroup congruence with H = [ 4 ] and W = [  ]>
gap> D := Digraph([[2, 3, 4], [4], [4], []]);
<immutable digraph with 4 vertices, 5 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 5 edges>
gap> GeneratingCongruencesOfLattice(S)[1];
<graph inverse semigroup congruence with H = [ 2, 4 ] and W = [ 1 ]>
gap> GeneratingCongruencesOfLattice(S)[2];
<graph inverse semigroup congruence with H = [ 3, 4 ] and W = [ 1 ]>
gap> GeneratingCongruencesOfLattice(S)[3];
<graph inverse semigroup congruence with H = [  ] and W = [ 2 ]>
gap> GeneratingCongruencesOfLattice(S)[4];
<graph inverse semigroup congruence with H = [  ] and W = [ 3 ]>
gap> D := Digraph([[3], [3, 4], [], []]);
<immutable digraph with 4 vertices, 3 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 3 edges>
gap> GeneratingCongruencesOfLattice(S)[1];
<graph inverse semigroup congruence with H = [  ] and W = [ 1 ]>
gap> GeneratingCongruencesOfLattice(S)[2];
<graph inverse semigroup congruence with H = [ 3 ] and W = [ 2 ]>
gap> GeneratingCongruencesOfLattice(S)[3];
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 2 ]>
gap> GeneratingCongruencesOfLattice(S)[4];
<graph inverse semigroup congruence with H = [ 3 ] and W = [  ]>
gap> D := Digraph([[2, 3, 4, 5], [3], [], [], []]);
<immutable digraph with 5 vertices, 5 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 5 vertices, 5 edges>
gap> GeneratingCongruencesOfLattice(S)[1];
<graph inverse semigroup congruence with H = [ 2, 3, 4 ] and W = [ 1 ]>
gap> GeneratingCongruencesOfLattice(S)[2];
<graph inverse semigroup congruence with H = [ 2, 3, 5 ] and W = [ 1 ]>
gap> GeneratingCongruencesOfLattice(S)[3];
<graph inverse semigroup congruence with H = [ 3, 4, 5 ] and W = [ 1 ]>
gap> GeneratingCongruencesOfLattice(S)[4];
<graph inverse semigroup congruence with H = [  ] and W = [ 2 ]>
gap> GeneratingCongruencesOfLattice(S)[5];
<graph inverse semigroup congruence with H = [ 3 ] and W = [  ]>

#AsCongruenceByWangPair
gap> D := Digraph([[2, 3], [3], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> CongruenceByWangPair(S, [4], [2]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 2 ]>
gap> cong := AsSemigroupCongruenceByGeneratingPairs(last);
<2-sided semigroup congruence over <finite graph inverse semigroup with 
4 vertices, 4 edges> with 2 generating pairs>
gap> AsCongruenceByWangPair(cong);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 2 ]>
gap> CongruenceByWangPair(S, [3, 4], [1]);
<graph inverse semigroup congruence with H = [ 3, 4 ] and W = [ 1 ]>
gap> cong := AsSemigroupCongruenceByGeneratingPairs(last);
<2-sided semigroup congruence over <finite graph inverse semigroup with 
4 vertices, 4 edges> with 3 generating pairs>
gap> AsCongruenceByWangPair(cong);
<graph inverse semigroup congruence with H = [ 3, 4 ] and W = [ 1 ]>
gap> D := Digraph([[2], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> CongruenceByWangPair(S, [4], [1, 2]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 1, 2 ]>
gap> cong := AsSemigroupCongruenceByGeneratingPairs(last);
<2-sided semigroup congruence over <finite graph inverse semigroup with 
4 vertices, 4 edges> with 3 generating pairs>
gap> AsCongruenceByWangPair(cong);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 1, 2 ]>

#JoinSemigroupCongruences
gap> D := Digraph([[2], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> cong1 := CongruenceByWangPair(S, [4], [2]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 2 ]>
gap> cong2 := CongruenceByWangPair(S, [], [1, 3]);
<graph inverse semigroup congruence with H = [  ] and W = [ 1, 3 ]>
gap> JoinSemigroupCongruences(cong1, cong2);
<graph inverse semigroup congruence with H = [ 1 .. 4 ] and W = [  ]>
gap> cong1 := CongruenceByWangPair(S, [4], [2]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 2 ]>
gap> cong2 := CongruenceByWangPair(S, [4], [1]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 1 ]>
gap> JoinSemigroupCongruences(cong1, cong2);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 1, 2 ]>
gap> cong1 := CongruenceByWangPair(S, [3, 4], []);
<graph inverse semigroup congruence with H = [ 3, 4 ] and W = [  ]>
gap> cong2 := CongruenceByWangPair(S, [], [1, 3]);
<graph inverse semigroup congruence with H = [  ] and W = [ 1, 3 ]>
gap> JoinSemigroupCongruences(cong1, cong2);
<graph inverse semigroup congruence with H = [ 3, 4 ] and W = [ 1 ]>
gap> D := Digraph([[3, 4], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 5 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 5 edges>
gap> cong1 := CongruenceByWangPair(S, [3, 4], []);
<graph inverse semigroup congruence with H = [ 3, 4 ] and W = [  ]>
gap> cong2 := CongruenceByWangPair(S, [4], [2]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 2 ]>
gap> JoinSemigroupCongruences(cong1, cong2);
<graph inverse semigroup congruence with H = [ 2 .. 4 ] and W = [  ]>
gap> cong1 := CongruenceByWangPair(S, [3, 4], []);
<graph inverse semigroup congruence with H = [ 3, 4 ] and W = [  ]>
gap> cong2 := CongruenceByWangPair(S, [4], [1]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 1 ]>
gap> JoinSemigroupCongruences(cong1, cong2);
<graph inverse semigroup congruence with H = [ 1, 3, 4 ] and W = [  ]>

# IsSubRelation and IsSuperrelation
gap> D := Digraph([[3, 4], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 5 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 5 edges>
gap> cong1 := CongruenceByWangPair(S, [3, 4], []);
<graph inverse semigroup congruence with H = [ 3, 4 ] and W = [  ]>
gap> cong2 := CongruenceByWangPair(S, [4], [1]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 1 ]>
gap> IsSubrelation(cong1, cong2);
false
gap> cong2 := CongruenceByWangPair(S, [4], []);
<graph inverse semigroup congruence with H = [ 4 ] and W = [  ]>
gap> IsSubrelation(cong1, cong2);
true
gap> IsSuperrelation(cong1, cong2);
false
gap> IsSuperrelation(cong2, cong1);
true
gap> cong1 := CongruenceByWangPair(S, [2, 3, 4], []);
<graph inverse semigroup congruence with H = [ 2, 3, 4 ] and W = [  ]>
gap> cong2 := CongruenceByWangPair(S, [4], [1]);
<graph inverse semigroup congruence with H = [ 4 ] and W = [ 1 ]>
gap> IsSubrelation(cong1, cong2);
false
gap> IsSuperrelation(cong2, cong1);
false

# LatticeOfCongruences
gap> D := ChainDigraph(4);
<immutable chain digraph with 4 vertices>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 3 edges>
gap> LatticeOfCongruences(S);
<lattice of 16 two-sided congruences over 
 <finite graph inverse semigroup with 4 vertices, 3 edges>>
gap> D := Digraph([[2, 3], [3], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> LatticeOfCongruences(S);
<lattice of 10 two-sided congruences over 
 <finite graph inverse semigroup with 4 vertices, 4 edges>>
gap> D := Digraph([[2], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> LatticeOfCongruences(S);
<lattice of 12 two-sided congruences over 
 <finite graph inverse semigroup with 4 vertices, 4 edges>>

# Meet and join
gap> D := Digraph([[3, 4], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 5 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 5 edges>
gap> L := LatticeOfCongruences(S);
<lattice of 10 two-sided congruences over 
 <finite graph inverse semigroup with 4 vertices, 5 edges>>
gap> C := CongruencesOfSemigroup(S);; Set(C);
[ <graph inverse semigroup congruence with H = [  ] and W = [  ]>, 
  <graph inverse semigroup congruence with H = [  ] and W = [ 3 ]>, 
  <graph inverse semigroup congruence with H = [ 1 .. 4 ] and W = [  ]>, 
  <graph inverse semigroup congruence with H = [ 1, 3, 4 ] and W = [  ]>, 
  <graph inverse semigroup congruence with H = [ 2 .. 4 ] and W = [  ]>, 
  <graph inverse semigroup congruence with H = [ 3, 4 ] and W = [  ]>, 
  <graph inverse semigroup congruence with H = [ 4 ] and W = [  ]>, 
  <graph inverse semigroup congruence with H = [ 4 ] and W = [ 2 ]>, 
  <graph inverse semigroup congruence with H = [ 4 ] and W = [ 1 ]>, 
  <graph inverse semigroup congruence with H = [ 4 ] and W = [ 1, 2 ]> ]
gap> val := true;;
> for i in [1 .. Length(C)] do
> for j in [1 .. Length(C)] do
> pos := Position(C, JoinSemigroupCongruences(C[i], C[j]));
> join := PartialOrderDigraphJoinOfVertices(L, i, j);
> if pos <> join then
> Error(StringFormatted("the join of congruences {} and {} is {} but should be {}", 
>                       i, j, pos, join));
> val := false;
> fi;
> od;
> od;
> val;
true
gap> val := true;;
> for i in [1 .. Length(C)] do
> for j in [1 .. Length(C)] do
> pos := Position(C, MeetSemigroupCongruences(C[i], C[j]));
> meet := PartialOrderDigraphMeetOfVertices(L, i, j);
> if pos <> meet then
> Error(StringFormatted("the meet of congruences {} and {} is {} but should be {}", 
>                       i, j, pos, meet));
> val := false;
> fi;
> od;
> od;
> val;
true

# More meet and join
gap> D := Digraph([[2], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> L := LatticeOfCongruences(S);
<lattice of 12 two-sided congruences over 
 <finite graph inverse semigroup with 4 vertices, 4 edges>>
gap> C := CongruencesOfSemigroup(S);; Set(C);
[ <graph inverse semigroup congruence with H = [  ] and W = [  ]>, 
  <graph inverse semigroup congruence with H = [  ] and W = [ 1 ]>, 
  <graph inverse semigroup congruence with H = [  ] and W = [ 3 ]>, 
  <graph inverse semigroup congruence with H = [  ] and W = [ 1, 3 ]>, 
  <graph inverse semigroup congruence with H = [ 1 .. 4 ] and W = [  ]>, 
  <graph inverse semigroup congruence with H = [ 2 .. 4 ] and W = [  ]>, 
  <graph inverse semigroup congruence with H = [ 3, 4 ] and W = [  ]>, 
  <graph inverse semigroup congruence with H = [ 3, 4 ] and W = [ 1 ]>, 
  <graph inverse semigroup congruence with H = [ 4 ] and W = [  ]>, 
  <graph inverse semigroup congruence with H = [ 4 ] and W = [ 1 ]>, 
  <graph inverse semigroup congruence with H = [ 4 ] and W = [ 2 ]>, 
  <graph inverse semigroup congruence with H = [ 4 ] and W = [ 1, 2 ]> ]
gap> val := true;;
> for i in [1 .. Length(C)] do
> for j in [1 .. Length(C)] do
> pos := Position(C, JoinSemigroupCongruences(C[i], C[j]));
> join := PartialOrderDigraphJoinOfVertices(L, i, j);
> if pos <> join then
> Error(StringFormatted("the join of congruences {} and {} is {} but should be {}", 
>                       i, j, pos, join));
> val := false;
> fi;
> od;
> od;
> val;
true
gap> val := true;;
> for i in [1 .. Length(C)] do
> for j in [1 .. Length(C)] do
> pos := Position(C, MeetSemigroupCongruences(C[i], C[j]));
> meet := PartialOrderDigraphMeetOfVertices(L, i, j);
> if pos <> meet then
> Error(StringFormatted("the meet of congruences {} and {} is {} but should be {}", 
>                       i, j, pos, meet));
> val := false;
> fi;
> od;
> od;
> val;
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/congruences/cong.tst");
