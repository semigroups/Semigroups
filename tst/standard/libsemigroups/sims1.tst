#############################################################################
##
#W  standard/libsemigroups/sims1.tst
#Y  Copyright (C) 2022                                     James Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local F, R, S, e, it
gap> START_TEST("Semigroups package: standard/libsemigroups/sims1.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# NumberOfRightCongruences
gap> F := FreeMonoidAndAssignGeneratorVars("a", "b");
<free monoid on the generators [ a, b ]>
gap> R := [[a ^ 3, a], [b ^ 2, b], [(a * b) ^ 2, a]];
[ [ a^3, a ], [ b^2, b ], [ (a*b)^2, a ] ]
gap> S := F / R;
<fp monoid with 2 generators and 3 relations of length 14>
gap> NumberOfRightCongruences(S);
6
gap> NumberOfRightCongruences(S, 1);
1
gap> it := IteratorOfRightCongruences(S, 1);
<iterator>
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 1, 1 ] ]
gap> it := IteratorOfRightCongruences(S);
<iterator>
gap> NumberOfRightCongruences(S, 0);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `NumberOfRightCongruences' on 2 argument\
s
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 1, 1 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 1 ], [ 2, 2 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 2 ], [ 2, 2 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 3 ], [ 2, 2 ], [ 2, 3 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 3 ], [ 2, 2 ], [ 3, 3 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 3 ], [ 2, 2 ], [ 4, 3 ], [ 4, 4 ] ]
gap> NextIterator(it);
fail
gap> it := ShallowCopy(it);
<iterator>
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 1, 1 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 1 ], [ 2, 2 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 2 ], [ 2, 2 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 3 ], [ 2, 2 ], [ 2, 3 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 3 ], [ 2, 2 ], [ 3, 3 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 3 ], [ 2, 2 ], [ 4, 3 ], [ 4, 4 ] ]
gap> NextIterator(it);
fail
gap> it := IteratorOfRightCongruences(S, 2);
<iterator>
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 1, 1 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 1 ], [ 2, 2 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 2 ], [ 2, 2 ] ]
gap> NextIterator(it);
fail
gap> NumberOfLeftCongruences(S);
9
gap> it := IteratorOfLeftCongruences(S);
<iterator>
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 1, 1 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 1, 2 ], [ 1, 2 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 1 ], [ 2, 1 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 1 ], [ 2, 2 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 1 ], [ 2, 3 ], [ 2, 3 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 2 ], [ 2, 2 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 3 ], [ 2, 2 ], [ 2, 3 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 3 ], [ 2, 3 ], [ 2, 3 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 3 ], [ 2, 4 ], [ 2, 3 ], [ 2, 4 ] ]
gap> NextIterator(it);
fail
gap> it := ShallowCopy(it);
<iterator>
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 1, 1 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 1, 2 ], [ 1, 2 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 1 ], [ 2, 1 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 1 ], [ 2, 2 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 1 ], [ 2, 3 ], [ 2, 3 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 2 ], [ 2, 2 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 3 ], [ 2, 2 ], [ 2, 3 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 3 ], [ 2, 3 ], [ 2, 3 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 3 ], [ 2, 4 ], [ 2, 3 ], [ 2, 4 ] ]
gap> NextIterator(it);
fail
gap> NumberOfLeftCongruences(S, 2);
5
gap> it := IteratorOfLeftCongruences(S, 2);
<iterator>
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 1, 1 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 1, 2 ], [ 1, 2 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 1 ], [ 2, 1 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 1 ], [ 2, 2 ] ]
gap> OutNeighbours(WordGraph(NextIterator(it)));
[ [ 2, 2 ], [ 2, 2 ] ]
gap> NextIterator(it);
fail
gap> F := FreeSemigroupAndAssignGeneratorVars("a", "b");
<free semigroup on the generators [ a, b ]>
gap> R := [[a ^ 3, a], [b ^ 2, b], [(a * b) ^ 2, a]];
[ [ a^3, a ], [ b^2, b ], [ (a*b)^2, a ] ]
gap> S := F / R;
<fp semigroup with 2 generators and 3 relations of length 14>
gap> NumberOfRightCongruences(S);
4

#
gap> F := FreeMonoidAndAssignGeneratorVars("a", "A", "b", "B", "c", "C");
<free monoid on the generators [ a, A, b, B, c, C ]>
gap> e := One(F);
<identity ...>
gap> R := [[a * A, A * a], [A * a, e], 
>          [b * B, B * b], [B * b, e],
>          [c * C, C * c], [C * c, e],
>          [a ^ 2 * C * a * c, e],
>          [a * c * b ^ 2 * A * C * b, e],
>          [A * B * a * b * c ^ 3, e]];
[ [ a*A, A*a ], [ A*a, <identity ...> ], [ b*B, B*b ], 
  [ B*b, <identity ...> ], [ c*C, C*c ], [ C*c, <identity ...> ], 
  [ a^2*C*a*c, <identity ...> ], [ a*c*b^2*A*C*b, <identity ...> ], 
  [ A*B*a*b*c^3, <identity ...> ] ]
gap> S := F / R;
<fp monoid with 6 generators and 9 relations of length 43>
gap> NumberOfRightCongruences(S, 1);
1
gap> NumberOfRightCongruences(S, 3);
14
gap> NumberOfRightCongruences(S, 4);
14
gap> NumberOfRightCongruences(S, 5);
14
gap> NumberOfLeftCongruences(S, 5);
14

# 
gap> S := PartitionMonoid(3);
<regular bipartition *-monoid of size 203, degree 3 with 4 generators>
gap> NumberOfRightCongruences(S, 10);
135
gap> NumberOfLeftCongruences(S, 10);
135
gap> NumberOfRightCongruences(S, Size(S), [[S.1, S.2], [S.3, S.4]]);
26

#
gap> NumberOfRightCongruences(FreeSemigroup(2), 3);
830
gap> NumberOfRightCongruences(FreeMonoid(2), 3);
229
gap> S := FreeSemigroup(2);
<free semigroup on the generators [ s1, s2 ]>
gap> NumberOfRightCongruences(S, 3, [[S.1 * S.2, S.2 * S.1]]);
229

# 
gap> S := Semigroup(SEMIGROUPS.UniversalFakeOne);;
gap> CanUseFroidurePin(S);
false
gap> NumberOfRightCongruences(S, 3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `NumberOfRightCongruences' on 3 argument\
s
gap> NumberOfLeftCongruences(S, 3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `NumberOfLeftCongruences' on 3 arguments
gap> IteratorOfLeftCongruences(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `IteratorOfLeftCongruences' on 3 argumen\
ts
gap> IteratorOfRightCongruences(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `IteratorOfRightCongruences' on 3 argume\
nts

# NumberOfRightCongruences when RightCongruencesOfSemigroup is known
gap> F := FreeMonoidAndAssignGeneratorVars("a", "b");
<free monoid on the generators [ a, b ]>
gap> R := [[a ^ 3, a], [b ^ 2, b], [(a * b) ^ 2, a]];
[ [ a^3, a ], [ b^2, b ], [ (a*b)^2, a ] ]
gap> S := F / R;
<fp monoid with 2 generators and 3 relations of length 14>
gap> RightCongruencesOfSemigroup(S);;
gap> NumberOfRightCongruences(S);
6
gap> NumberOfRightCongruences(S, 2);
3
gap> NumberOfRightCongruences(S, 2, [[S.1, S.1 * S.2]]);
3
gap> S := F / R;
<fp monoid with 2 generators and 3 relations of length 14>
gap> NumberOfRightCongruences(S);
6
gap> NumberOfRightCongruences(S, 2);
3
gap> NumberOfRightCongruences(S, 2, [[S.1, S.1 * S.2]]);
3
gap> S := F / R;
<fp monoid with 2 generators and 3 relations of length 14>
gap> NumberOfLeftCongruences(S);
9
gap> NumberOfLeftCongruences(S, 2);
5
gap> NumberOfLeftCongruences(S, 2, [[S.1, S.1 * S.2]]);
5
gap> S := F / R;
<fp monoid with 2 generators and 3 relations of length 14>
gap> LeftCongruencesOfSemigroup(S);;
gap> NumberOfLeftCongruences(S);
9
gap> NumberOfLeftCongruences(S, 2);
5
gap> NumberOfLeftCongruences(S, 2, [[S.1, S.1 * S.2]]);
5

# SmallerDegreeTransformationRepresentation
gap> S := PartitionMonoid(3);
<regular bipartition *-monoid of size 203, degree 3 with 4 generators>

# The output of the next is non-deterministic, so suppressed!
gap> SmallerDegreeTransformationRepresentation(S);; 
gap> S := FullTransformationMonoid(2);
<full transformation monoid of degree 2>
gap> SmallerDegreeTransformationRepresentation(S);
CompositionMapping( 
<fp semigroup with 3 generators and 8 relations of length 27> -> 
<full transformation monoid of degree 2>, 
<full transformation monoid of degree 2> -> 
<fp semigroup with 3 generators and 8 relations of length 27> )

#
gap> S := FullTransformationMonoid(2);;
gap> S := AsSemigroup(IsFpSemigroup, S);
<fp semigroup with 3 generators and 8 relations of length 27>
gap> SmallerDegreeTransformationRepresentation(S);
<fp semigroup with 3 generators and 8 relations of length 27> -> 
<full transformation monoid of degree 2>

#
gap> SmallerDegreeTransformationRepresentation(
> AsSemigroup(IsFpSemigroup, FreeSemigroup(2)));
Error, the argument (an fp semigroup) must be finite
gap> SmallerDegreeTransformationRepresentation(FreeSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `SmallerDegreeTransformationRepresentati\
on' on 1 arguments

# 
gap> F := FreeMonoidAndAssignGeneratorVars("a", "b");
<free monoid on the generators [ a, b ]>
gap> R := [[a ^ 3, a], [b ^ 2, b], [(a * b) ^ 2, a]];
[ [ a^3, a ], [ b^2, b ], [ (a*b)^2, a ] ]
gap> S := F / R;
<fp monoid with 2 generators and 3 relations of length 14>
gap> LeftCongruencesOfSemigroup(S);;
gap> it := IteratorOfLeftCongruences(S, 2);
<iterator>
gap> NrEquivalenceClasses(NextIterator(it));
2
gap> NrEquivalenceClasses(NextIterator(it));
2
gap> NrEquivalenceClasses(NextIterator(it));
2
gap> NrEquivalenceClasses(NextIterator(it));
1
gap> NrEquivalenceClasses(NextIterator(it));
2
gap> NextIterator(it);
Error, <iter> is exhausted
gap> RightCongruencesOfSemigroup(S);;
gap> it := IteratorOfRightCongruences(S, 2);
<iterator>
gap> NrEquivalenceClasses(NextIterator(it));
1
gap> NrEquivalenceClasses(NextIterator(it));
2
gap> NrEquivalenceClasses(NextIterator(it));
2
gap> NrEquivalenceClasses(NextIterator(it));
Error, <iter> is exhausted
gap> NrEquivalenceClasses(NextIterator(it));
Error, <iter> is exhausted
gap> NextIterator(it);
Error, <iter> is exhausted

# 
gap> S := PartitionMonoid(2);
<regular bipartition *-monoid of size 15, degree 2 with 3 generators>
gap> NumberOfRightCongruences(S, 10);
86
gap> NumberOfLeftCongruences(S, 10);
86
gap> NumberOfRightCongruences(S, Size(S), [[S.1, S.2], [S.1, S.3]]);
1
gap> NumberOfLeftCongruences(S, Size(S), [[S.1, S.2], [S.1, S.3]]);
1

#
gap> S := PartitionMonoid(2);
<regular bipartition *-monoid of size 15, degree 2 with 3 generators>
gap> RightCongruencesOfSemigroup(S);; LeftCongruencesOfSemigroup(S);;
gap> NumberOfRightCongruences(S, 10);
86
gap> NumberOfLeftCongruences(S, 10);
86
gap> NumberOfRightCongruences(S, Size(S), [[S.1, S.2], [S.1, S.3]]);
1
gap> NumberOfLeftCongruences(S, Size(S), [[S.1, S.2], [S.1, S.3]]);
1

# Check extra
gap> S := PartitionMonoid(2);
<regular bipartition *-monoid of size 15, degree 2 with 3 generators>
gap> NumberOfRightCongruences(S, Size(S), [[S.1], [S.1, S.3]]);
Error, the 3rd argument (a list) must consist of lists of length 2
gap> NumberOfRightCongruences(S, Size(S), [[S.1, fail], [S.1, S.3]]);
Error, the 3rd argument (a list of length 2) must consist of pairs of elements\
 of the 1st argument (a semigroup)
gap> NumberOfLeftCongruences(S, Size(S), [[S.1], [S.1, S.3]]);
Error, the 3rd argument (a list) must consist of lists of length 2
gap> NumberOfLeftCongruences(S, Size(S), [[S.1, fail], [S.1, S.3]]);
Error, the 3rd argument (a list of length 2) must consist of pairs of elements\
 of the 1st argument (a semigroup)
gap> IteratorOfLeftCongruences(S, Size(S), [[S.1], [S.1, S.3]]);
Error, the 3rd argument (a list) must consist of lists of length 2
gap> IteratorOfRightCongruences(S, Size(S), [[S.1, fail], [S.1, S.3]]);
Error, the 3rd argument (a list of length 2) must consist of pairs of elements\
 of the 1st argument (a semigroup)
gap> IteratorOfLeftCongruences(S, Size(S), [[S.1], [S.1, S.3]]);
Error, the 3rd argument (a list) must consist of lists of length 2
gap> IteratorOfLeftCongruences(S, Size(S), [[S.1, fail], [S.1, S.3]]);
Error, the 3rd argument (a list of length 2) must consist of pairs of elements\
 of the 1st argument (a semigroup)

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/libsemigroups/sims1.tst");
