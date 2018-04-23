#############################################################################
##
#W  standard/congfpmon.tst
#Y  Copyright (C) 2017                                      Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/congfpmon.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# Bad input
gap> F := FreeMonoid(2);;
gap> M := F / [[F.2 ^ 2, F.2], [F.1 ^ 3, F.1 ^ 2]];;
gap> cong := SemigroupCongruenceByGeneratingPairs(M, [[M.2]]);
Error, Semigroups: SemigroupCongruenceByGeneratingPairs: usage,
<pairs> must all be lists of length 2,
gap> cong := SemigroupCongruenceByGeneratingPairs(M, [[M.1, F.1]]);
Error, Semigroups: SemigroupCongruenceByGeneratingPairs: usage,
<pairs> must all be lists of elements of <M>,
gap> cong := LeftSemigroupCongruenceByGeneratingPairs(M, [[M.2]]);
Error, Semigroups: LeftSemigroupCongruenceByGeneratingPairs: usage,
<pairs> must all be lists of length 2,
gap> cong := LeftSemigroupCongruenceByGeneratingPairs(M, [[M.1, F.1]]);
Error, Semigroups: LeftSemigroupCongruenceByGeneratingPairs: usage,
<pairs> must all be lists of elements of <M>,
gap> cong := RightSemigroupCongruenceByGeneratingPairs(M, [[M.2]]);
Error, Semigroups: RightSemigroupCongruenceByGeneratingPairs: usage,
<pairs> must all be lists of length 2,
gap> cong := RightSemigroupCongruenceByGeneratingPairs(M, [[M.1, F.1]]);
Error, Semigroups: RightSemigroupCongruenceByGeneratingPairs: usage,
<pairs> must all be lists of elements of <M>,
gap> cong := SemigroupCongruenceByGeneratingPairs(M, [[M.1, M.2]]);;
gap> [M.1, M.2, M.2 ^ 2] in cong;
Error, Semigroups: \in (for a relation): usage,
the first arg <pair> must be a list of length 2,
gap> [F.1, F.2] in cong;
Error, Semigroups: \in (for a relation): usage,
elements of the first arg <pair> must be
in the range of the second arg <cong>,
gap> EquivalenceClassOfElement(cong, Transformation([1, 2, 1]));
Error, Semigroups: EquivalenceClassOfElement: usage,
<elm> must be an element of the range of <cong>,

# A 2-sided example
gap> F := FreeMonoid(2);;
gap> M := F / [[F.2 ^ 2, F.2],
>              [F.1 ^ 3, F.1 ^ 2],
>              [F.2 * F.1 ^ 2, F.1 ^ 2],
>              [F.1 * (F.1 * F.2) ^ 2, F.1 ^ 2 * F.2 * F.1],
>              [(F.2 * F.1) ^ 2 * F.2, F.2]];;
gap> Size(M);
13
gap> M.1 ^ 2 = M.2 * M.1;
false
gap> (M.2 * M.1) ^ 2 * M.2 * M.1 ^ 2 = M.1 ^ 3;
true
gap> pair := [M.1 ^ 2 * M.2 * M.1, M.1 * M.2 * M.1];;
gap> cong := SemigroupCongruence(M, pair);
<semigroup congruence over <fp monoid on the generators [ m1, m2 ]> with 
1 generating pairs>
gap> IsFpMonoidCongruence(cong);
true
gap> NrEquivalenceClasses(cong);
3
gap> [M.2, M.2 * M.1] in cong;
true
gap> part := EquivalenceRelationPartition(cong);;
gap> Length(part) = 1;
true
gap> Length(part[1]) = 11;
true
gap> Size(EquivalenceRelationCanonicalPartition(cong));
1
gap> Size(EquivalenceRelationCanonicalPartition(cong)[1]);
11
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 ]
gap> ImagesElm(cong, M.1);
[ m1 ]
gap> ImagesElm(cong, One(M));
[ <identity ...> ]
gap> classes := EquivalenceClasses(cong);;
gap> SortedList(List(classes, Size));
[ 1, 1, 11 ]
gap> class1 := EquivalenceClassOfElement(cong, M.2 * M.1);;
gap> class2 := EquivalenceClassOfElement(cong, M.1);;
gap> M.1 ^ 2 in class1;
true
gap> M.1 in class1;
false
gap> class1 = class2;
false
gap> enum := Enumerator(class1);;
gap> enum[3];
(m2*m1)^2
gap> enum[11];
m2*m1*m2
gap> Position(enum, M.2 * M.1 * M.2 * M.1);
3
gap> Size(enum);
11
gap> class1 * class2 = EquivalenceClassOfElement(cong, M.2 ^ 20 * M.1 ^ 42);
true
gap> class1 * class2 = EquivalenceClassOfElement(cong, One(M));
false

# A left congruence example
gap> F := FreeMonoid(2);;
gap> M := F / [[F.1 * F.2 ^ 2, F.2 ^ 2],
>              [F.2 ^ 3, F.2 ^ 2],
>              [F.1 ^ 4, F.1],
>              [F.2 * F.1 ^ 2 * F.2, F.2 ^ 2],
>              [F.2 * F.1 ^ 3 * F.2, F.2],
>              [(F.2 * F.1) ^ 2 * F.2, F.2],
>              [F.2 ^ 2 * F.1 ^ 3, F.2 ^ 2],
>              [F.2 * (F.2 * F.1) ^ 2, F.2 ^ 2 * F.1 ^ 2]];;
gap> Size(M);
40
gap> cong := LeftSemigroupCongruence(M, [M.1, M.2 ^ 3]);
<left semigroup congruence over <fp monoid on the generators [ m1, m2 ]> with 
1 generating pairs>
gap> IsFpMonoidCongruence(cong);
true
gap> IsLeftSemigroupCongruence(cong);
true
gap> HasIsSemigroupCongruence(cong);
false
gap> NrEquivalenceClasses(cong);
11
gap> [M.1 ^ 9, M.2 * M.1 ^ 3 * M.2 * M.1] in cong;
true
gap> part := EquivalenceRelationPartition(cong);;
gap> Length(part) = 1;
true
gap> Length(part[1]) = 30;
true
gap> part := EquivalenceRelationCanonicalPartition(cong);;
gap> Size(part);
1
gap> Size(part[1]);
30
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 2, 3, 2, 4, 2, 2, 2, 5, 2, 2, 6, 2, 7, 2, 2, 8, 2, 2, 2, 9, 2, 2, 10, 2, 
  2, 2, 2, 11, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ]
gap> Set(ImagesElm(cong, M.1)) = part[1];
true
gap> ImagesElm(cong, One(M));
[ <identity ...> ]
gap> classes := EquivalenceClasses(cong);;
gap> SortedList(List(classes, Size));
[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 30 ]
gap> class1 := EquivalenceClassOfElement(cong, M.2 * M.1 ^ 2);;
gap> class2 := EquivalenceClassOfElement(cong, M.2);;
gap> M.1 in class1;
true
gap> M.2 in class1;
false
gap> class1 = class2;
false
gap> enum := Enumerator(class1);;
gap> enum[3];
m1^2
gap> enum[11];
m2^2*m1^2
gap> Position(enum, M.2 * M.1 * M.2 * M.1);
13
gap> Size(enum);
30

# A right congruence example
gap> F := FreeMonoid(2);;
gap> M := F / [[F.1 * F.2 ^ 2, F.2 ^ 2],
>              [F.2 ^ 3, F.2 ^ 2],
>              [F.1 ^ 4, F.1],
>              [F.2 * F.1 ^ 2 * F.2, F.2 ^ 2],
>              [F.2 * F.1 ^ 3 * F.2, F.2],
>              [(F.2 * F.1) ^ 2 * F.2, F.2],
>              [F.2 ^ 2 * F.1 ^ 3, F.2 ^ 2],
>              [F.2 * (F.2 * F.1) ^ 2, F.2 ^ 2 * F.1 ^ 2]];;
gap> Size(M);
40
gap> cong := RightSemigroupCongruence(M, [M.1, M.2 ^ 3]);
<right semigroup congruence over <fp monoid on the generators 
[ m1, m2 ]> with 1 generating pairs>
gap> IsFpMonoidCongruence(cong);
true
gap> IsRightSemigroupCongruence(cong);
true
gap> HasIsSemigroupCongruence(cong);
false
gap> NrEquivalenceClasses(cong);
13
gap> [M.1 ^ 9, M.2 * M.1 ^ 3 * M.2 * M.1] in cong;
false
gap> [M.2 * M.1 * M.2 ^ 2, M.1 ^ 4] in cong;
true
gap> part := EquivalenceRelationCanonicalPartition(cong);;
gap> Length(part) = 4;
true
gap> Set(part, Length) = [4, 8, 11];
true
gap> part := EquivalenceRelationCanonicalPartition(cong);;
gap> Length(part);
4
gap> SortedList(List(part, Length));
[ 4, 8, 8, 11 ]
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 2, 3, 4, 2, 5, 2, 6, 7, 4, 8, 9, 4, 2, 6, 6, 7, 10, 11, 6, 7, 4, 2, 2, 
  2, 6, 12, 6, 7, 4, 4, 2, 13, 2, 6, 6, 4, 2, 2, 4 ]
gap> part1 := First(part, l -> M.1 in l);;
gap> Set(ImagesElm(cong, M.1)) = part1;
true
gap> NrEquivalenceClasses(cong);
13
gap> ImagesElm(cong, One(M));
[ <identity ...> ]
gap> classes := EquivalenceClasses(cong);;
gap> SortedList(List(classes, Size));
[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 8, 8, 11 ]
gap> class1 := EquivalenceClassOfElement(cong, M.1 * (M.2 * M.1) ^ 2 * M.2);;
gap> class2 := EquivalenceClassOfElement(cong, M.2 ^ 2 * M.1);;
gap> M.1 in class1;
true
gap> M.2 in class1;
false
gap> class1 = class2;
false
gap> enum := Enumerator(class1);;
gap> enum[3];
m1*m2
gap> enum[11];
m1*(m1*m2)^2*m1^3
gap> Position(enum, M.1 * (M.1 * M.2) ^ 2 * M.1 ^ 3);
11
gap> Size(enum);
11

# Joining two congs together
gap> F := FreeMonoid(2);;
gap> M := F / [[F.1 ^ 4, F.1 ^ 3],
>              [F.1 ^ 3 * F.2, F.1 ^ 3],
>              [F.1 * F.2 ^ 2 * F.1, F.1 ^ 2],
>              [F.1 * F.2 ^ 3, F.1],
>              [F.2 * F.1 ^ 3, F.1 ^ 3],
>              [F.2 ^ 3 * F.1, F.1],
>              [F.2 ^ 4, F.2],
>              [F.1 ^ 2 * F.2 * F.1 ^ 2, F.1 ^ 2],
>              [F.1 * (F.1 * F.2) ^ 2, F.1 ^ 2 * F.2 * F.1],
>              [(F.1 * F.2) ^ 2 * F.1, F.1],
>              [(F.2 * F.1) ^ 2 * F.1, F.1 * F.2 * F.1 ^ 2]];;
gap> Size(M);
39
gap> cong1 := SemigroupCongruence(M, [M.1, M.2]);;
gap> cong2 := SemigroupCongruence(M, [M.1, M.1 ^ 2]);;
gap> cong1 = cong2;
false
gap> IsSubrelation(cong1, cong2);
true
gap> JoinSemigroupCongruences(cong1, cong2) = cong1;
true
gap> M := F / [[F.1, F.2]];;
gap> cong3 := SemigroupCongruence(M, [M.1, M.2 ^ 10]);;
gap> JoinSemigroupCongruences(cong1, cong3);
Error, Semigroups: JoinSemigroupCongruences: usage,
<cong1> and <cong2> must be over the same semigroup,

# Joining two left congs together
gap> F := FreeMonoid(2);;
gap> M := F / [[F.1 ^ 4, F.1 ^ 3],
>              [F.1 ^ 3 * F.2, F.1 ^ 3],
>              [F.1 * F.2 ^ 2 * F.1, F.1 ^ 2],
>              [F.1 * F.2 ^ 3, F.1],
>              [F.2 * F.1 ^ 3, F.1 ^ 3],
>              [F.2 ^ 3 * F.1, F.1],
>              [F.2 ^ 4, F.2],
>              [F.1 ^ 2 * F.2 * F.1 ^ 2, F.1 ^ 2],
>              [F.1 * (F.1 * F.2) ^ 2, F.1 ^ 2 * F.2 * F.1],
>              [(F.1 * F.2) ^ 2 * F.1, F.1],
>              [(F.2 * F.1) ^ 2 * F.1, F.1 * F.2 * F.1 ^ 2]];;
gap> Size(M);
39
gap> cong1 := LeftSemigroupCongruence(M, [M.1, M.2]);;
gap> cong2 := LeftSemigroupCongruence(M, [M.1, M.1 ^ 2]);;
gap> cong1 = cong2;
false
gap> IsSubrelation(cong1, cong2);
true
gap> JoinLeftSemigroupCongruences(cong1, cong2) = cong1;
true
gap> M := F / [[F.1, F.2]];;
gap> cong3 := SemigroupCongruence(M, [M.1, M.2 ^ 10]);;
gap> JoinLeftSemigroupCongruences(cong1, cong3);
Error, Semigroups: JoinLeftSemigroupCongruences: usage,
<cong1> and <cong2> must be over the same semigroup,

# Joining two right congs together
gap> F := FreeMonoid(2);;
gap> M := F / [[F.1 ^ 4, F.1 ^ 3],
>              [F.1 ^ 3 * F.2, F.1 ^ 3],
>              [F.1 * F.2 ^ 2 * F.1, F.1 ^ 2],
>              [F.1 * F.2 ^ 3, F.1],
>              [F.2 * F.1 ^ 3, F.1 ^ 3],
>              [F.2 ^ 3 * F.1, F.1],
>              [F.2 ^ 4, F.2],
>              [F.1 ^ 2 * F.2 * F.1 ^ 2, F.1 ^ 2],
>              [F.1 * (F.1 * F.2) ^ 2, F.1 ^ 2 * F.2 * F.1],
>              [(F.1 * F.2) ^ 2 * F.1, F.1],
>              [(F.2 * F.1) ^ 2 * F.1, F.1 * F.2 * F.1 ^ 2]];;
gap> Size(M);
39
gap> cong1 := RightSemigroupCongruence(M, [M.1, M.2]);;
gap> cong2 := RightSemigroupCongruence(M, [M.1, M.1 ^ 2]);;
gap> cong1 = cong2;
false
gap> IsSubrelation(cong1, cong2);
true
gap> JoinRightSemigroupCongruences(cong1, cong2) = cong1;
true
gap> M := F / [[F.1, F.2]];;
gap> cong3 := SemigroupCongruence(M, [M.1, M.2 ^ 10]);;
gap> JoinRightSemigroupCongruences(cong1, cong3);
Error, Semigroups: JoinRightSemigroupCongruences: usage,
<cong1> and <cong2> must be over the same semigroup,

# SEMIGROUPS_UnbindVariables
gap> Unbind(F);
gap> Unbind(M);
gap> Unbind(class1);
gap> Unbind(class2);
gap> Unbind(classes);
gap> Unbind(cong);
gap> Unbind(cong1);
gap> Unbind(cong2);
gap> Unbind(cong3);
gap> Unbind(enum);
gap> Unbind(pair);
gap> Unbind(part);
gap> Unbind(part1);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/congfpmon.tst");
