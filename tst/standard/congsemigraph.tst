############################################################################
##
##  congsemigraph.tst
##  Copyright (C) 2021                       Marina Anagnostopoulou-Merkouri
##                                                            James Mitchell 
##
##  Licensing information can be found in the README file of this package.
##
############################################################################

#IsCongruenceByWangPair

gap> D := Digraph([[3, 4], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 5 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 5 edges>
gap> cong := CongruenceByWangPair(S, [3, 4], []);
<gis-congruence with H=[ 3, 4 ], W=[  ]>
gap> IsCongruenceByWangPair(cong);
true
gap> cong := CongruenceByWangPair(S, [4], [2]);
<gis-congruence with H=[ 4 ], W=[ 2 ]>
gap> IsCongruenceByWangPair(cong);
true
gap> e_1 := S.1;
e_1
gap> e_3 := S.3;
e_3
gap> cong := SemigroupCongruence(S, [[e_1, e_3]]);
<semigroup congruence over <finite graph inverse semigroup with 4 vertices, 5 
edges> with 1 generating pairs>
gap> IsCongruenceByWangPair(cong);
false

#CongruenceByWangPair
gap> D := Digraph([[3, 4], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 5 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 5 edges>
gap> cong := CongruenceByWangPair(S, [3, 4], []);
<gis-congruence with H=[ 3, 4 ], W=[  ]>
gap> cong := CongruenceByWangPair(S, [4], [2]);
<gis-congruence with H=[ 4 ], W=[ 2 ]>
gap> cong := CongruenceByWangPair(S, [3, 4], []);
<gis-congruence with H=[ 3, 4 ], W=[  ]>
gap> cong := CongruenceByWangPair(S, [4], [1]);
<gis-congruence with H=[ 4 ], W=[ 1 ]>
gap> D := Digraph([[2], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> cong := CongruenceByWangPair(S, [4], [2]);
<gis-congruence with H=[ 4 ], W=[ 2 ]>
gap> cong := CongruenceByWangPair(S, [], [1, 3]);
<gis-congruence with H=[  ], W=[ 1, 3 ]>
gap> cong := CongruenceByWangPair(S, [4], [2]);
<gis-congruence with H=[ 4 ], W=[ 2 ]>
gap> cong := CongruenceByWangPair(S, [4], [1]);
<gis-congruence with H=[ 4 ], W=[ 1 ]>
gap> cong := CongruenceByWangPair(S, [3, 4], []);
<gis-congruence with H=[ 3, 4 ], W=[  ]>
gap> cong := CongruenceByWangPair(S, [], [1, 3]);
<gis-congruence with H=[  ], W=[ 1, 3 ]>
gap> cong := CongruenceByWangPair(S, [3], []);
Error, [ 3 ] is not a valid hereditary subset
gap> cong := CongruenceByWangPair(S, [], [4]);
Error, [ 4 ] is not a valid W-set
gap> D := Digraph([[2, 3], [3], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> CongruenceByWangPair(S, [4], []);
<gis-congruence with H=[ 4 ], W=[  ]>
gap> CongruenceByWangPair(S, [3, 4], [1]);
<gis-congruence with H=[ 3, 4 ], W=[ 1 ]>
gap> CongruenceByWangPair(S, [4], [2]);
<gis-congruence with H=[ 4 ], W=[ 2 ]>
gap> CongruenceByWangPair(S, [], []);
<gis-congruence with H=[  ], W=[  ]>
gap> CongruenceByWangPair(S, [3], [4]);
Error, [ 3 ] is not a valid hereditary subset

# AsSemigroupCongruenceByGeneratingPairs
gap> D := Digraph([[2, 3], [3], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> CongruenceByWangPair(S, [4], [2]);
<gis-congruence with H=[ 4 ], W=[ 2 ]>
gap> AsSemigroupCongruenceByGeneratingPairs(last);
<semigroup congruence over <finite graph inverse semigroup with 4 vertices, 4 
edges> with 2 generating pairs>
gap> EquivalenceRelationPartition(last);
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
<gis-congruence with H=[ 4 ], W=[ 1, 2 ]>
gap> cong := AsSemigroupCongruenceByGeneratingPairs(last);
<semigroup congruence over <finite graph inverse semigroup with 4 vertices, 3 
edges> with 3 generating pairs>
gap> EquivalenceRelationPartition(cong);
[ [ e_1, e_1e_2e_2^-1 ], 
  [ e_3, v_4, e_3^-1, 0, e_2e_3, e_3e_3^-1, e_3^-1e_2^-1, e_1e_2e_3, 
      e_2e_3e_3^-1, e_3e_3^-1e_2^-1, e_3^-1e_2^-1e_1^-1, e_1e_2e_3e_3^-1, 
      e_2e_3e_3^-1e_2^-1, e_3e_3^-1e_2^-1e_1^-1, e_1e_2e_3e_3^-1e_2^-1, 
      e_2e_3e_3^-1e_2^-1e_1^-1, e_1e_2e_3e_3^-1e_2^-1e_1^-1 ], 
  [ v_1, e_1e_1^-1, e_1e_2e_2^-1e_1^-1 ], [ v_2, e_2e_2^-1 ], 
  [ e_1^-1, e_2e_2^-1e_1^-1 ] ]
gap> CongruenceByWangPair(S, [2, 3, 4], []);
<gis-congruence with H=[ 2, 3, 4 ], W=[  ]>
gap> cong := AsSemigroupCongruenceByGeneratingPairs(last);
<semigroup congruence over <finite graph inverse semigroup with 4 vertices, 3 
edges> with 3 generating pairs>
gap> EquivalenceRelationPartition(cong);
[ [ e_1, e_2, e_3, v_2, v_3, v_4, e_1^-1, e_2^-1, e_3^-1, 0, e_1e_2, 
      e_1e_1^-1, e_2e_3, e_2e_2^-1, e_3e_3^-1, e_2^-1e_1^-1, e_3^-1e_2^-1, 
      e_1e_2e_3, e_1e_2e_2^-1, e_2e_3e_3^-1, e_2e_2^-1e_1^-1, e_3e_3^-1e_2^-1,
      e_3^-1e_2^-1e_1^-1, e_1e_2e_3e_3^-1, e_1e_2e_2^-1e_1^-1, 
      e_2e_3e_3^-1e_2^-1, e_3e_3^-1e_2^-1e_1^-1, e_1e_2e_3e_3^-1e_2^-1, 
      e_2e_3e_3^-1e_2^-1e_1^-1, e_1e_2e_3e_3^-1e_2^-1e_1^-1 ] ]
gap> CongruenceByWangPair(S, [], [1]);
<gis-congruence with H=[  ], W=[ 1 ]>
gap> cong := AsSemigroupCongruenceByGeneratingPairs(last);
<semigroup congruence over <finite graph inverse semigroup with 4 vertices, 3 
edges> with 1 generating pairs>
gap> EquivalenceRelationPartition(cong);
[ [ v_1, e_1e_1^-1 ] ]

# GeneratingCongruencesOfSemigroup
gap> D := Digraph([[2], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> GeneratingCongruencesOfLattice(S)[1];
<gis-congruence with H=[  ], W=[ 1 ]>
gap> GeneratingCongruencesOfLattice(S)[2];
<gis-congruence with H=[ 4 ], W=[ 2 ]>
gap> GeneratingCongruencesOfLattice(S)[3];
<gis-congruence with H=[  ], W=[ 3 ]>
gap> GeneratingCongruencesOfLattice(S)[4];
<gis-congruence with H=[ 4 ], W=[  ]>
gap> D := Digraph([[2, 3, 4], [4], [4], []]);
<immutable digraph with 4 vertices, 5 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 5 edges>
gap> GeneratingCongruencesOfLattice(S)[1];
<gis-congruence with H=[ 2, 4 ], W=[ 1 ]>
gap> GeneratingCongruencesOfLattice(S)[2];
<gis-congruence with H=[ 3, 4 ], W=[ 1 ]>
gap> GeneratingCongruencesOfLattice(S)[3];
<gis-congruence with H=[  ], W=[ 2 ]>
gap> GeneratingCongruencesOfLattice(S)[4];
<gis-congruence with H=[  ], W=[ 3 ]>
gap> D := Digraph([[3], [3, 4], [], []]);
<immutable digraph with 4 vertices, 3 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 3 edges>
gap> GeneratingCongruencesOfLattice(S)[1];
<gis-congruence with H=[  ], W=[ 1 ]>
gap> GeneratingCongruencesOfLattice(S)[2];
<gis-congruence with H=[ 3 ], W=[ 2 ]>
gap> GeneratingCongruencesOfLattice(S)[3];
<gis-congruence with H=[ 4 ], W=[ 2 ]>
gap> GeneratingCongruencesOfLattice(S)[4];
<gis-congruence with H=[ 3 ], W=[  ]>
gap> D := Digraph([[2, 3, 4, 5], [3], [], [], []]);
<immutable digraph with 5 vertices, 5 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 5 vertices, 5 edges>
gap> GeneratingCongruencesOfLattice(S)[1];
<gis-congruence with H=[ 2, 3, 4 ], W=[ 1 ]>
gap> GeneratingCongruencesOfLattice(S)[2];
<gis-congruence with H=[ 2, 3, 5 ], W=[ 1 ]>
gap> GeneratingCongruencesOfLattice(S)[3];
<gis-congruence with H=[ 3, 4, 5 ], W=[ 1 ]>
gap> GeneratingCongruencesOfLattice(S)[4];
<gis-congruence with H=[  ], W=[ 2 ]>
gap> GeneratingCongruencesOfLattice(S)[5];
<gis-congruence with H=[ 3 ], W=[  ]>

#AsCongruenceByWangPair
gap> D := Digraph([[2, 3], [3], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> CongruenceByWangPair(S, [4], [2]);
<gis-congruence with H=[ 4 ], W=[ 2 ]>
gap> cong := AsSemigroupCongruenceByGeneratingPairs(last);
<semigroup congruence over <finite graph inverse semigroup with 4 vertices, 4 
edges> with 2 generating pairs>
gap> AsCongruenceByWangPair(cong);
<gis-congruence with H=[ 4 ], W=[ 2 ]>
gap> CongruenceByWangPair(S, [3, 4], [1]);
<gis-congruence with H=[ 3, 4 ], W=[ 1 ]>
gap> cong := AsSemigroupCongruenceByGeneratingPairs(last);
<semigroup congruence over <finite graph inverse semigroup with 4 vertices, 4 
edges> with 3 generating pairs>
gap> AsCongruenceByWangPair(cong);
<gis-congruence with H=[ 3, 4 ], W=[ 1 ]>
gap> D := Digraph([[2], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> CongruenceByWangPair(S, [4], [1, 2]);
<gis-congruence with H=[ 4 ], W=[ 1, 2 ]>
gap> cong := AsSemigroupCongruenceByGeneratingPairs(last);
<semigroup congruence over <finite graph inverse semigroup with 4 vertices, 4 
edges> with 3 generating pairs>
gap> AsCongruenceByWangPair(cong);
<gis-congruence with H=[ 4 ], W=[ 1, 2 ]>

#JoinSemigroupCongruences
gap> D := Digraph([[2], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> cong1 := CongruenceByWangPair(S, [4], [2]);
<gis-congruence with H=[ 4 ], W=[ 2 ]>
gap> cong2 := CongruenceByWangPair(S, [], [1, 3]);
<gis-congruence with H=[  ], W=[ 1, 3 ]>
gap> JoinSemigroupCongruences(cong1, cong2);
<gis-congruence with H=[ 1, 2, 3, 4 ], W=[  ]>
gap> cong1 := CongruenceByWangPair(S, [4], [2]);
<gis-congruence with H=[ 4 ], W=[ 2 ]>
gap> cong2 := CongruenceByWangPair(S, [4], [1]);
<gis-congruence with H=[ 4 ], W=[ 1 ]>
gap> JoinSemigroupCongruences(cong1, cong2);
<gis-congruence with H=[ 4 ], W=[ 1, 2 ]>
gap> cong1 := CongruenceByWangPair(S, [3, 4], []);
<gis-congruence with H=[ 3, 4 ], W=[  ]>
gap> cong2 := CongruenceByWangPair(S, [], [1, 3]);
<gis-congruence with H=[  ], W=[ 1, 3 ]>
gap> JoinSemigroupCongruences(cong1, cong2);
<gis-congruence with H=[ 3, 4 ], W=[ 1 ]>
gap> D := Digraph([[3, 4], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 5 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 5 edges>
gap> cong1 := CongruenceByWangPair(S, [3, 4], []);
<gis-congruence with H=[ 3, 4 ], W=[  ]>
gap> cong2 := CongruenceByWangPair(S, [4], [2]);
<gis-congruence with H=[ 4 ], W=[ 2 ]>
gap> JoinSemigroupCongruences(cong1, cong2);
<gis-congruence with H=[ 2, 3, 4 ], W=[  ]>
gap> cong1 := CongruenceByWangPair(S, [3, 4], []);
<gis-congruence with H=[ 3, 4 ], W=[  ]>
gap> cong2 := CongruenceByWangPair(S, [4], [1]);
<gis-congruence with H=[ 4 ], W=[ 1 ]>
gap> JoinSemigroupCongruences(cong1, cong2);
<gis-congruence with H=[ 1, 3, 4 ], W=[  ]>

#IsSubRelation and IsSuperrelation
gap> D := Digraph([[3, 4], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 5 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 5 edges>
gap> cong1 := CongruenceByWangPair(S, [3, 4], []);
<gis-congruence with H=[ 3, 4 ], W=[  ]>
gap> cong2 := CongruenceByWangPair(S, [4], [1]);
<gis-congruence with H=[ 4 ], W=[ 1 ]>
gap> IsSubrelation(cong1, cong2);
false
gap> cong2 := CongruenceByWangPair(S, [4], []);
<gis-congruence with H=[ 4 ], W=[  ]>
gap> IsSubrelation(cong1, cong2);
true
gap> IsSuperrelation(cong1, cong2);
false
gap> IsSuperrelation(cong2, cong1);
true
gap> cong1 := CongruenceByWangPair(S, [2, 3, 4], []);
<gis-congruence with H=[ 2, 3, 4 ], W=[  ]>
gap> cong2 := CongruenceByWangPair(S, [4], [1]);
<gis-congruence with H=[ 4 ], W=[ 1 ]>
gap> IsSubrelation(cong1, cong2);
false
gap> IsSuperrelation(cong2, cong1);
false

#LatticeOfCongruences
gap> D := ChainDigraph(4);
<immutable chain digraph with 4 vertices>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 3 edges>
gap> LatticeOfCongruences(S);
<poset of 16 congruences over <finite graph inverse semigroup with 4 
 vertices, 3 edges>>
gap> D := Digraph([[2, 3], [3], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> LatticeOfCongruences(S);
<poset of 10 congruences over <finite graph inverse semigroup with 4 
 vertices, 4 edges>>
gap> D := Digraph([[2], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 4 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 4 edges>
gap> LatticeOfCongruences(S);
<poset of 12 congruences over <finite graph inverse semigroup with 4 
 vertices, 4 edges>>
