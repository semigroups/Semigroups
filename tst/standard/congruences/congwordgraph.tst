#############################################################################
##
#W  standard/libsemigroups/congwordgraph.tst
#Y  Copyright (C) 2022                                     James Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local C, C1, C2, F, R, S, it
gap> START_TEST("Semigroups package: standard/libsemigroups/congwordgraph.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

#
gap> F := FreeMonoidAndAssignGeneratorVars("a", "b");
<free monoid on the generators [ a, b ]>
gap> R := [[a ^ 3, a], [b ^ 2, b], [(a * b) ^ 2, a]];
[ [ a^3, a ], [ b^2, b ], [ (a*b)^2, a ] ]
gap> S := F / R;
<fp monoid with 2 generators and 3 relations of length 14>
gap> it := IteratorOfRightCongruences(S);
<iterator>
gap> C := NextIterator(it);
<right congruence by word graph over <fp monoid with 2 generators and 
  3 relations of length 14>>
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ <identity ...>, a, b, b*a ] ]
gap> EquivalenceRelationPartition(C);
[ [ <identity ...>, a, b, b*a ] ]
gap> GeneratingPairsOfRightMagmaCongruence(C);
[ [ <identity ...>, a ] ]
gap> RightSemigroupCongruence(S, last) = C;
true
gap> ForAll(last2, x -> x in C);
true
gap> [S.1 ^ 3, S.1] in C;
true
gap> ImagesElm(C, S.1);
[ <identity ...>, a, b, b*a ]
gap> C := NextIterator(it);
<right congruence by word graph over <fp monoid with 2 generators and 
  3 relations of length 14>>
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ <identity ...>, b ], [ a, b*a ] ]
gap> EquivalenceRelationPartition(C);
[ [ <identity ...>, b ], [ a, b*a ] ]
gap> GeneratingPairsOfRightMagmaCongruence(C);
[ [ <identity ...>, b ] ]
gap> RightSemigroupCongruence(S, last) = C;
true
gap> ForAll(last2, x -> x in C);
true
gap> [S.1 ^ 3, S.1] in C;
true
gap> C := NextIterator(it);
<right congruence by word graph over <fp monoid with 2 generators and 
  3 relations of length 14>>
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ <identity ...> ], [ a, b, b*a ] ]
gap> EquivalenceRelationPartition(C);
[ [ a, b, b*a ] ]
gap> GeneratingPairsOfRightMagmaCongruence(C);
[ [ a, b ] ]
gap> RightSemigroupCongruence(S, last) = C;
true
gap> ForAll(last2, x -> x in C);
true
gap> [S.1 ^ 3, S.1] in C;
true
gap> C := NextIterator(it);
<right congruence by word graph over <fp monoid with 2 generators and 
  3 relations of length 14>>
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ <identity ...> ], [ a, b*a ], [ b ] ]
gap> EquivalenceRelationPartition(C);
[ [ a, b*a ] ]
gap> GeneratingPairsOfRightMagmaCongruence(C);
[ [ a, b*a ] ]
gap> RightSemigroupCongruence(S, last) = C;
true
gap> ForAll(last2, x -> x in C);
true
gap> [S.1 ^ 3, S.1] in C;
true
gap> C := NextIterator(it);
<right congruence by word graph over <fp monoid with 2 generators and 
  3 relations of length 14>>
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ <identity ...> ], [ a ], [ b, b*a ] ]
gap> EquivalenceRelationPartition(C);
[ [ b, b*a ] ]
gap> GeneratingPairsOfRightMagmaCongruence(C);
[ [ b, b*a ] ]
gap> RightSemigroupCongruence(S, last) = C;
true
gap> ForAll(last2, x -> x in C);
true
gap> [S.1 ^ 3, S.1] in C;
true
gap> C := NextIterator(it);
<right congruence by word graph over <fp monoid with 2 generators and 
  3 relations of length 14>>
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ <identity ...> ], [ a ], [ b ], [ b*a ] ]
gap> EquivalenceRelationPartition(C);
[  ]
gap> GeneratingPairsOfRightMagmaCongruence(C);
[  ]
gap> RightSemigroupCongruence(S, last) = C;
true
gap> ForAll(last2, x -> x in C);
true
gap> [S.1 ^ 3, S.1] in C;
true
gap> C := NextIterator(it);
fail

#
gap> F := FreeMonoidAndAssignGeneratorVars("a", "b");
<free monoid on the generators [ a, b ]>
gap> R := [[a ^ 3, a], [b ^ 2, b], [(a * b) ^ 2, a]];
[ [ a^3, a ], [ b^2, b ], [ (a*b)^2, a ] ]
gap> S := F / R;
<fp monoid with 2 generators and 3 relations of length 14>
gap> it := IteratorOfLeftCongruences(S);
<iterator>
gap> C := NextIterator(it);
<left congruence by word graph over <fp monoid with 2 generators and 
  3 relations of length 14>>
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ <identity ...>, a, b, b*a ] ]
gap> EquivalenceRelationPartition(C);
[ [ <identity ...>, a, b, b*a ] ]
gap> GeneratingPairsOfLeftMagmaCongruence(C);
[ [ <identity ...>, a ], [ <identity ...>, b ] ]
gap> LeftSemigroupCongruence(S, last) = C;
true
gap> ForAll(last2, x -> x in C);
true
gap> [S.1 ^ 3, S.1] in C;
true
gap> C := NextIterator(it);
<left congruence by word graph over <fp monoid with 2 generators and 
  3 relations of length 14>>
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ <identity ...>, a ], [ b, b*a ] ]
gap> EquivalenceRelationPartition(C);
[ [ <identity ...>, a ], [ b, b*a ] ]
gap> GeneratingPairsOfLeftMagmaCongruence(C);
[ [ <identity ...>, a ] ]
gap> LeftSemigroupCongruence(S, last) = C;
true
gap> ForAll(last2, x -> x in C);
true
gap> [S.1 ^ 3, S.1] in C;
true
gap> C := NextIterator(it);
<left congruence by word graph over <fp monoid with 2 generators and 
  3 relations of length 14>>
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ <identity ...>, b, b*a ], [ a ] ]
gap> EquivalenceRelationPartition(C);
[ [ <identity ...>, b, b*a ] ]
gap> GeneratingPairsOfLeftMagmaCongruence(C);
[ [ <identity ...>, b ], [ <identity ...>, b*a ] ]
gap> LeftSemigroupCongruence(S, last) = C;
true
gap> ForAll(last2, x -> x in C);
true
gap> [S.1 ^ 3, S.1] in C;
true
gap> C := NextIterator(it);
<left congruence by word graph over <fp monoid with 2 generators and 
  3 relations of length 14>>
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ <identity ...>, b ], [ a, b*a ] ]
gap> EquivalenceRelationPartition(C);
[ [ <identity ...>, b ], [ a, b*a ] ]
gap> GeneratingPairsOfLeftMagmaCongruence(C);
[ [ <identity ...>, b ], [ a, b*a ] ]
gap> LeftSemigroupCongruence(S, last) = C;
true
gap> ForAll(last2, x -> x in C);
true
gap> [S.1 ^ 3, S.1] in C;
true
gap> C := NextIterator(it);
<left congruence by word graph over <fp monoid with 2 generators and 
  3 relations of length 14>>
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ <identity ...>, b ], [ a ], [ b*a ] ]
gap> EquivalenceRelationPartition(C);
[ [ <identity ...>, b ] ]
gap> GeneratingPairsOfLeftMagmaCongruence(C);
[ [ <identity ...>, b ] ]
gap> LeftSemigroupCongruence(S, last) = C;
true
gap> ForAll(last2, x -> x in C);
true
gap> [S.1 ^ 3, S.1] in C;
true
gap> C := NextIterator(it);
<left congruence by word graph over <fp monoid with 2 generators and 
  3 relations of length 14>>
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ <identity ...> ], [ a, b, b*a ] ]
gap> EquivalenceRelationPartition(C);
[ [ a, b, b*a ] ]
gap> GeneratingPairsOfLeftMagmaCongruence(C);
[ [ a, b ] ]
gap> LeftSemigroupCongruence(S, last) = C;
true
gap> ForAll(last2, x -> x in C);
true
gap> [S.1 ^ 3, S.1] in C;
true
gap> C := NextIterator(it);
<left congruence by word graph over <fp monoid with 2 generators and 
  3 relations of length 14>>
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ <identity ...> ], [ a, b*a ], [ b ] ]
gap> EquivalenceRelationPartition(C);
[ [ a, b*a ] ]
gap> GeneratingPairsOfLeftMagmaCongruence(C);
[ [ a, b*a ] ]
gap> LeftSemigroupCongruence(S, last) = C;
true
gap> ForAll(last2, x -> x in C);
true
gap> [S.1 ^ 3, S.1] in C;
true
gap> C := NextIterator(it);
<left congruence by word graph over <fp monoid with 2 generators and 
  3 relations of length 14>>
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ <identity ...> ], [ a ], [ b, b*a ] ]
gap> ImagesElm(C, S.1);
[ a ]
gap> ImagesElm(C, S.2 * S.1);
[ b, b*a ]
gap> EquivalenceRelationPartition(C);
[ [ b, b*a ] ]
gap> GeneratingPairsOfLeftMagmaCongruence(C);
[ [ b, b*a ] ]
gap> LeftSemigroupCongruence(S, last) = C;
true
gap> ForAll(last2, x -> x in C);
true
gap> [S.1 ^ 3, S.1] in C;
true
gap> C := NextIterator(it);
<left congruence by word graph over <fp monoid with 2 generators and 
  3 relations of length 14>>

#
gap> F := FreeSemigroupAndAssignGeneratorVars("a", "b");
<free semigroup on the generators [ a, b ]>
gap> R := [[a ^ 3, a], [b ^ 2, b], [(a * b) ^ 2, a]];
[ [ a^3, a ], [ b^2, b ], [ (a*b)^2, a ] ]
gap> S := F / R;
<fp semigroup with 2 generators and 3 relations of length 14>
gap> it := IteratorOfRightCongruences(S);
<iterator>
gap> C := NextIterator(it);
<right congruence by word graph over <fp semigroup with 2 generators and 
  3 relations of length 14>>
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ a, b, b*a ] ]
gap> EquivalenceRelationPartition(C);
[ [ a, b, b*a ] ]
gap> GeneratingPairsOfRightMagmaCongruence(C);
[ [ a, b ] ]
gap> RightSemigroupCongruence(S, last) = C;
true
gap> ForAll(last2, x -> x in C);
true
gap> [S.1 ^ 3, S.1] in C;
true
gap> ImagesElm(C, S.1);
[ a, b, b*a ]
gap> it := IteratorOfLeftCongruences(S);
<iterator>
gap> C := NextIterator(it);
<left congruence by word graph over <fp semigroup with 2 generators and 
  3 relations of length 14>>
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ a, b, b*a ] ]
gap> EquivalenceRelationPartition(C);
[ [ a, b, b*a ] ]
gap> GeneratingPairsOfLeftMagmaCongruence(C);
[ [ a, b ] ]
gap> LeftSemigroupCongruence(S, last) = C;
true
gap> ForAll(last2, x -> x in C);
true
gap> [S.1 ^ 3, S.1] in C;
true
gap> ImagesElm(C, S.1);
[ a, b, b*a ]

# TODO(later) These should move to Digraphs
gap> DigraphFollowPath(NullDigraph(3), 4, [1, 2, 3]);
Error, the 2nd argument (a pos. int.) must be in the range [1, 3]
gap> DigraphFollowPath(NullDigraph(3), 3, [1, 2, 3]);
fail

# Corner case non-fp monoid
gap> S := GossipMonoid(3);
<monoid of 3x3 boolean matrices with 3 generators>
gap> it := IteratorOfLeftCongruences(S);
<iterator>
gap> NextIterator(it);
<left congruence by word graph over <monoid of size 11, 3x3 boolean matrices 
 with 3 generators>>
gap> C1 := NextIterator(it);
<left congruence by word graph over <monoid of size 11, 3x3 boolean matrices 
 with 3 generators>>
gap> C2 := NextIterator(it);
<left congruence by word graph over <monoid of size 11, 3x3 boolean matrices 
 with 3 generators>>
gap> C1 <> C2;
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/libsemigroups/congwordgraph.tst");
