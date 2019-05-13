###########################################################################
##
#W  standard/congrms.tst
#Y  Copyright (C) 2014-15                                   Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/congrms.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# All ReesMatrixSemigroup functions tested with a small example

# ReesMatCongTest1: Create a Rees matrix semigroup
gap> g := SymmetricGroup(4);;
gap> StructureDescription(g);
"S4"
gap> mat := [[(1, 3), (1, 2)(3, 4)],
>            [(1, 4, 3, 2), ()],
>            [(1, 3)(2, 4), (1, 3, 4, 2)]];;
gap> S := ReesMatrixSemigroup(g, mat);;

# ReesMatCongTest2: Find all its congruences
gap> congs := CongruencesOfSemigroup(S);;
gap> Size(congs);
23

# ReesMatCongTest3: Construct a congruence manually
gap> n := Group([(1, 4)(2, 3), (1, 3)(2, 4)]);;
gap> colBlocks := [[1], [2]];;
gap> rowBlocks := [[1, 2], [3]];;
gap> cong := RMSCongruenceByLinkedTriple(S, n, colBlocks, rowBlocks);
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (2^2,2,2)>
gap> congs12 := SemigroupCongruence(S, [[RMSElement(S, 1, (1, 2, 3, 4), 2),
>                                        RMSElement(S, 2, (), 2)],
>                                       [RMSElement(S, 1, (1, 3), 1),
>                                        RMSElement(S, 1, (1, 2, 3, 4), 2)],
>                                       [RMSElement(S, 1, (1, 3), 1),
>                                        RMSElement(S, 1, (1, 3)(2, 4), 3)]]);
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (A4,1,1)>
gap> cong = congs12;
false
gap> congs3 := SemigroupCongruence(S, [RMSElement(S, 1, (1, 3), 1),
>                                      RMSElement(S, 1, (1, 2, 3, 4), 2)]);
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (2^2,2,2)>
gap> cong = congs3;
true

# IsSubrelation: RMS case
gap> colBlocks := [[1, 2]];;
gap> rowBlocks := [[1, 2, 3]];;
gap> cong2 := RMSCongruenceByLinkedTriple(S, g, colBlocks, rowBlocks);
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (S4,1,1)>
gap> IsSubrelation(cong, cong2);
false
gap> IsSubrelation(cong2, cong);
true

# RMSCongruenceByLinkedTriple: Bad input
gap> RMSCongruenceByLinkedTriple(S, SymmetricGroup(3), colBlocks, rowBlocks);
Error, Semigroups: RMSCongruenceByLinkedTriple: usage,
the second arg <n> must be a normal subgroup,
gap> RMSCongruenceByLinkedTriple(S, n, [1, [2]], rowBlocks);
Error, Semigroups: RMSCongruenceByLinkedTriple: usage,
the third arg <colBlocks> must be a list of lists,
gap> RMSCongruenceByLinkedTriple(S, n, colBlocks, [[1, 2], 3]);
Error, Semigroups: RMSCongruenceByLinkedTriple: usage,
the fourth arg <rowBlocks> must be a list of lists,
gap> RMSCongruenceByLinkedTriple(S, n, [[1], [2, 3]], rowBlocks);
Error, Semigroups: RMSCongruenceByLinkedTriple: usage,
the third arg <colBlocks> must partition the columns of the matrix of <S>,
gap> RMSCongruenceByLinkedTriple(S, n, colBlocks, [[1], [2]]);
Error, Semigroups: RMSCongruenceByLinkedTriple: usage,
the fourth arg <rowBlocks> must partition the rows of the matrix of <S>,
gap> RMSCongruenceByLinkedTriple(S, n, colBlocks, [[1], [2, 3]]);
Error, Semigroups: RMSCongruenceByLinkedTriple:
invalid triple,

# IsSubrelation: bad input (no zero)
gap> g := SymmetricGroup(4);;
gap> mat := [[(), (1, 2)(3, 4)],
>            [(), ()],
>            [(2, 4), (1, 3, 4, 2)]];;
gap> T := ReesMatrixSemigroup(g, mat);;
gap> n := Group([(2, 4, 3), (1, 4)(2, 3), (1, 3)(2, 4)]);;
gap> colBlocks := [[1, 2]];;
gap> rowBlocks := [[1], [2, 3]];;
gap> cong2 := RMSCongruenceByLinkedTriple(T, n, colBlocks, rowBlocks);;
gap> EquivalenceRelationCanonicalLookup(cong2);
[ 1, 1, 2, 3, 4, 4, 3, 2, 4, 4, 3, 2, 1, 1, 3, 4, 4, 3, 2, 1, 1, 2, 3, 1, 2, 
  3, 1, 1, 2, 3, 1, 2, 3, 2, 4, 2, 1, 1, 2, 3, 4, 4, 3, 2, 4, 4, 3, 2, 4, 4, 
  3, 2, 1, 2, 3, 3, 4, 2, 2, 3, 2, 4, 3, 2, 4, 4, 3, 2, 4, 3, 2, 3, 2, 3, 2, 
  3, 3, 3, 1, 3, 2, 3, 2, 1, 1, 2, 3, 1, 2, 3, 1, 1, 2, 3, 1, 2, 3, 2, 4, 2, 
  1, 1, 2, 3, 1, 3, 3, 4, 2, 2, 3, 3, 2, 2, 2, 3, 2, 4, 3, 2, 4, 4, 3, 2, 4, 
  3, 2, 3, 2, 3, 2, 3, 3, 3, 1, 3, 2, 3, 2, 3, 3, 2, 2, 2 ]
gap> IsSubrelation(cong2, cong);
Error, Semigroups: IsSubrelation: usage,
congruences must be defined over the same semigroup,

# ReesMatCongTest4: Testing membership
gap> x := ReesMatrixSemigroupElement(S, 1, (2, 3), 2);;
gap> y := ReesMatrixSemigroupElement(S, 1, (1, 4), 1);;
gap> z := ReesMatrixSemigroupElement(S, 1, (2, 3, 4), 3);;
gap> t := ReesMatrixSemigroupElement(T, 1, (1, 2)(3, 4), 2);;
gap> [x, y] in cong;
true
gap> [x, z] in cong;
false
gap> [y, z] in cong;
false
gap> [x] in cong;
Error, Semigroups: \in (for a relation): usage,
the first arg <pair> must be a list of length 2,
gap> [x, y, z] in cong;
Error, Semigroups: \in (for a relation): usage,
the first arg <pair> must be a list of length 2,
gap> [t, t] in cong;
Error, Semigroups: \in (for a relation): usage,
elements of the first arg <pair> must be
in the range of the second arg <cong>,
gap> ims := ImagesElm(cong, t);
Error, Semigroups: ImagesElm: usage,
the args <cong> and <elm> must refer to the same semigroup,

# ReesMatCongTest5: Equivalence classes
gap> classes := CongruenceClasses(cong);;
gap> Size(classes) = NrCongruenceClasses(cong);
true
gap> class1 := EquivalenceClassOfElement(cong, x);;
gap> class2 := CongruenceClassOfElement(cong, y);;
gap> class3 := EquivalenceClassOfElement(cong, z);
<congruence class of (1,(2,3,4),3)>
gap> EquivalenceClassOfElement(cong, t);
Error, Semigroups: EquivalenceClassOfElement: usage,
the second arg <elm> must be in the semigroup of first arg <cong>,
gap> congs12 := SemigroupCongruence(S, [[RMSElement(S, 1, (1, 2, 3, 4), 2),
>                                        RMSElement(S, 2, (), 2)],
>                                       [RMSElement(S, 1, (1, 3), 1),
>                                        RMSElement(S, 1, (1, 2, 3, 4), 2)],
>                                       [RMSElement(S, 1, (1, 3), 1),
>                                        RMSElement(S, 1, (1, 3)(2, 4), 3)]]);
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (A4,1,1)>
gap> classother := EquivalenceClassOfElement(congs12, x);;
gap> class1 = class2;
true
gap> class1 = class3;
false
gap> y in class1;
true
gap> x in class3;
false
gap> class1 = classes[3];
true
gap> congs3 := SemigroupCongruence(S, [RMSElement(S, 1, (1, 3), 1),
>                                      RMSElement(S, 1, (1, 2, 3, 4), 2)]);
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (2^2,2,2)>
gap> nCoset := RightCoset(congs3!.n, (1, 3));;
gap> class := RMSCongruenceClassByLinkedTriple(congs3, nCoset, 1, 2);;
gap> class = classes[7];
true
gap> classes[11] * classes[19] = classes[12];
true
gap> classes[12] * classes[10] = classes[8];
true
gap> Position(classes, classes[20] * classes[10]);
21
gap> [classes[12], classes[20]] * classes[10] = [classes[8], classes[21]];
true
gap> classes[24] * classes[2] = classes[15];  # actually 16
false
gap> class1 * classother;
Error, Semigroups: \*: usage,
the args <c1> and <c2> must be classes of the same congruence,
gap> Size(classes[13]);
8
gap> Size(classes[24]);
4
gap> Size(classes[1]);
8
gap> Enumerator(classes[24]);
[ (2,(2,4),3), (2,(1,2,3,4),3), (2,(1,3),3), (2,(1,4,3,2),3) ]

# RMSCongruenceClassByLinkedTriple: bad input
gap> congs3 := SemigroupCongruence(S, [RMSElement(S, 1, (1, 3), 1),
>                                      RMSElement(S, 1, (1, 2, 3, 4), 2)]);
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (2^2,2,2)>
gap> badCoset := RightCoset(SymmetricGroup(6), (1, 5, 4));;
gap> class := RMSCongruenceClassByLinkedTriple(congs3, badCoset, 1, 2);;
Error, Semigroups: RMSCongruenceClassByLinkedTriple: usage,
the second arg <nCoset> must be a right coset of <cong>'s
normal subgroup <n> inside the semigroup's underlying group <g>,
gap> class := RMSCongruenceClassByLinkedTriple(congs3, nCoset, 7, 2);;
Error, Semigroups: RMSCongruenceClassByLinkedTriple: usage,
the third arg <colClass> is out of range,
gap> class := RMSCongruenceClassByLinkedTriple(congs3, nCoset, 1, 42);;
Error, Semigroups: RMSCongruenceClassByLinkedTriple: usage,
the fourth arg <rowClass> is out of range,

# ReesMatCongTest6: Join and meet congruences
gap> congs3 := SemigroupCongruence(S, [RMSElement(S, 1, (1, 3), 1),
>                                      RMSElement(S, 1, (1, 2, 3, 4), 2)]);
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (2^2,2,2)>
gap> congs3;
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (2^2,2,2)>
gap> MeetSemigroupCongruences(congs3, congs3) = congs3;
true
gap> congs9 := SemigroupCongruence(S, [RMSElement(S, 1, (1, 2, 3, 4), 2),
>                                      RMSElement(S, 2, (), 2)]);
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (A4,1,3)>
gap> cong1 := JoinSemigroupCongruences(congs3, congs9);
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (A4,1,2)>
gap> JoinSemigroupCongruences(congs9, congs3) = cong1;
true
gap> MeetSemigroupCongruences(cong1, congs9);
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (A4,1,3)>
gap> cong1 := MeetSemigroupCongruences(congs3, congs9);
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (2^2,2,3)>
gap> MeetSemigroupCongruences(congs9, congs3) = cong1;
true
gap> cong1 := SemigroupCongruence(T, []);;
gap> MeetSemigroupCongruences(congs3, cong1);
Error, Semigroups: MeetSemigroupCongruences: usage,
congruences must be defined over the same semigroup,
gap> JoinSemigroupCongruences(congs3, cong1);
Error, Semigroups: JoinSemigroupCongruences: usage,
congruences must be defined over the same semigroup,

# ReesMatCongTest7: Quotients
gap> congs13 := SemigroupCongruence(S, [[RMSElement(S, 1, (1, 2, 3, 4), 2),
>                                        RMSElement(S, 2, (), 2)],
>                                       [RMSElement(S, 1, (1, 3), 1),
>                                        RMSElement(S, 1, (1, 3)(2, 4), 3)]]);
<semigroup congruence over <Rees matrix semigroup 2x3 over S4>
  with linked triple (A4,1,2)>
gap> q := S / congs13;;
gap> Size(q);
4

# ReesMatCongTest8
# Convert to and from semigroup congruence by generating pairs
gap> ForAll(congs, x -> x = AsRMSCongruenceByLinkedTriple(
>  AsSemigroupCongruenceByGeneratingPairs(x)));
true
gap> cong := AsSemigroupCongruenceByGeneratingPairs(congs[2]);;
gap> AsRZMSCongruenceByLinkedTriple(cong);
Error, Semigroups: AsRZMSCongruenceByLinkedTriple: usage,
the argument must be a congruence over a Rees 0-matrix
semigroup,

# ReesMatCongTest9: Universal semigroup congruences
gap> uni := UniversalSemigroupCongruence(S);
<universal semigroup congruence over <Rees matrix semigroup 2x3 over S4>>
gap> [x, z] in uni;
true
gap> Length(CongruenceClasses(uni)) = 1;
true
gap> eq := CongruenceClassOfElement(uni, y);;
gap> z in eq;
true
gap> cong := AsSemigroupCongruenceByGeneratingPairs(uni);;
gap> cong := AsRMSCongruenceByLinkedTriple(cong);;
gap> cong = uni;
true
gap> Size(S / uni);
1

# AsRMSCongruenceByLinkedTriple
gap> g := SymmetricGroup(4);;
gap> mat := [[(1, 3), (1, 2)(3, 4)],
>            [(1, 4, 3, 2), ()],
>            [(1, 3)(2, 4), (1, 3, 4, 2)]];;
gap> S := ReesMatrixSemigroup(g, mat);;
gap> x := RMSElement(S, 2, (1, 3)(2, 4), 3);;
gap> y := RMSElement(S, 1, (), 3);;
gap> cong := SemigroupCongruenceByGeneratingPairs(S, [[x, y]]);;
gap> AsRMSCongruenceByLinkedTriple(cong);
<semigroup congruence over <Rees matrix semigroup 2x3 over Sym( [ 1 .. 4 ] )>
  with linked triple (S4,1,3)>

# Similar tests, but with zero
# ReesZeroMatCongTest1: Create a Rees 0-matrix semigroup
gap> g := Group([(1, 4, 5), (1, 5, 3, 4)]);;
gap> mat := [[0, 0, (1, 4, 5), 0, 0, (1, 4, 3, 5)],
> [0, (), 0, 0, (3, 5), 0],
> [(), 0, 0, (3, 5), 0, 0]];;
gap> S := ReesZeroMatrixSemigroup(g, mat);;

# ReesZeroMatCongTest2: Find all its congruences
gap> congs := CongruencesOfSemigroup(S);;
gap> Size(congs);
33

# CongruencesOfSemigroup: a different RZMS
gap> g := SymmetricGroup(3);;
gap> mat := [[0, 0, (1, 3)], [(1, 2, 3), (), (2, 3)], [0, 0, ()]];;
gap> T := ReesZeroMatrixSemigroup(g, mat);;
gap> congs1 := CongruencesOfSemigroup(T);;
gap> Size(congs1);
13
gap> EquivalenceRelationCanonicalLookup(congs1[8]);
[ 1, 2, 3, 4, 1, 5, 1, 2, 3, 4, 2, 1, 6, 7, 8, 1, 2, 3, 4, 2, 5, 9, 10, 11, 
  9, 3, 4, 6, 7, 6, 7, 1, 2, 3, 4, 6, 7, 6, 7, 5, 9, 10, 11, 12, 13, 12, 13, 
  3, 4, 6, 7, 10, 11, 12, 13 ]
gap> EquivalenceRelationCanonicalLookup(
> AsSemigroupCongruenceByGeneratingPairs(congs1[8]));
[ 1, 2, 3, 4, 1, 5, 1, 2, 3, 4, 2, 1, 6, 7, 8, 1, 2, 3, 4, 2, 5, 9, 10, 11, 
  9, 3, 4, 6, 7, 6, 7, 1, 2, 3, 4, 6, 7, 6, 7, 5, 9, 10, 11, 12, 13, 12, 13, 
  3, 4, 6, 7, 10, 11, 12, 13 ]

# ReesZeroMatCongTest3: Construct a congruence manually
gap> n := Group([(1, 4)(3, 5), (1, 5)(3, 4)]);;
gap> colBlocks := [[1], [2, 4, 5], [3, 6]];;
gap> rowBlocks := [[1], [2], [3]];;
gap> IsLinkedTriple(S, n, colBlocks, rowBlocks);
false
gap> colBlocks := [[1], [4], [2, 5], [3, 6]];;
gap> IsLinkedTriple(S, n, colBlocks, rowBlocks);
true
gap> IsLinkedTriple(S, Group([()]), [[1 .. 6]], rowBlocks);
false
gap> cong := RZMSCongruenceByLinkedTriple(S, n, colBlocks, rowBlocks);;
gap> congs12 := SemigroupCongruence(S, [[RMSElement(S, 2, (), 2),
>                                        RMSElement(S, 5, (3, 5), 2)],
>                                       [RMSElement(S, 3, (1, 3)(4, 5), 1),
>                                        RMSElement(S, 3, (), 1)]]);;
gap> cong = congs12;
false
gap> congs13 := SemigroupCongruence(S, [[RMSElement(S, 2, (), 2),
>                                        RMSElement(S, 5, (3, 5), 2)],
>                                       [RMSElement(S, 3, (1, 3)(4, 5), 1),
>                                        RMSElement(S, 3, (), 1)],
>                                       [RMSElement(S, 3, (1, 5, 4), 1),
>                                        RMSElement(S, 6, (1, 5, 3, 4), 1)]]);;
gap> cong = congs13;
true

# IsSubrelation: with zero
gap> trivialcong := SemigroupCongruence(S, []);;
gap> ForAll(congs, x -> IsSubrelation(x, trivialcong));
true
gap> ForAll(congs, x -> IsSubrelation(x, x));
true
gap> Number(congs, x -> IsSubrelation(UniversalSemigroupCongruence(S), x));
33
gap> Number(congs, x -> IsSubrelation(x, UniversalSemigroupCongruence(S)));
1
gap> congs19 := SemigroupCongruence(S, [[RMSElement(S, 3, (3, 4, 5), 1),
>                                        RMSElement(S, 3, (), 1)],
>                                       [RMSElement(S, 3, (1, 5, 4), 1),
>                                        RMSElement(S, 6, (1, 5, 3, 4), 1)]]);;
gap> congs24 := SemigroupCongruence(S, [[RMSElement(S, 1, (), 3),
>                                        RMSElement(S, 4, (3, 5), 3)],
>                                       [RMSElement(S, 2, (), 2),
>                                        RMSElement(S, 5, (3, 5), 2)],
>                                       [RMSElement(S, 3, (3, 4, 5), 1),
>                                        RMSElement(S, 3, (), 1)]]);;
gap> IsSubrelation(congs19, congs24);
false

# RZMSCongruenceByLinkedTriple: Bad input
gap> T := ReesZeroMatrixSemigroup(FullTransformationMonoid(3), [[0]]);;
gap> RZMSCongruenceByLinkedTriple(T, n, colBlocks, rowBlocks);
Error, Semigroups: RZMSCongruenceByLinkedTriple: usage,
the first arg <S> must be a Rees 0-matrix semigroup over a group,
gap> RZMSCongruenceByLinkedTriple(S, SymmetricGroup(3), colBlocks, rowBlocks);
Error, Semigroups: RZMSCongruenceByLinkedTriple: usage,
the second arg <n> must be a normal subgroup,
gap> RZMSCongruenceByLinkedTriple(S, n, [1, [2]], rowBlocks);
Error, Semigroups: RZMSCongruenceByLinkedTriple: usage,
the third arg <colBlocks> must be a list of lists,
gap> RZMSCongruenceByLinkedTriple(S, n, colBlocks, [[1, 2], 3]);
Error, Semigroups: RZMSCongruenceByLinkedTriple: usage,
the fourth arg <rowBlocks> must be a list of lists,
gap> RZMSCongruenceByLinkedTriple(S, n, [[1], [2, 3]], rowBlocks);
Error, Semigroups: RZMSCongruenceByLinkedTriple: usage,
the third arg <colBlocks> must partition the columns of the matrix of <S>,
gap> RZMSCongruenceByLinkedTriple(S, n, colBlocks, [[1], [2]]);
Error, Semigroups: RZMSCongruenceByLinkedTriple: usage,
the fourth arg <rowBlocks> must partition the rows of the matrix of <S>,
gap> RZMSCongruenceByLinkedTriple(S, n, colBlocks, [[1], [2, 3]]);
Error, Semigroups: RZMSCongruenceByLinkedTriple:
invalid triple,

# ReesZeroMatCongTest4: Testing membership
gap> x := ReesZeroMatrixSemigroupElement(S, 3, (4, 5), 1);;
gap> y := ReesZeroMatrixSemigroupElement(S, 3, (1, 5, 3, 4), 1);;
gap> z := ReesZeroMatrixSemigroupElement(S, 1, (1, 3, 5), 2);;
gap> t := ReesZeroMatrixSemigroupElement(T, 1, Transformation([2, 1]), 1);;
gap> zero := MultiplicativeZero(S);
0
gap> [x, y] in cong;
true
gap> [x, z] in cong;
false
gap> y := ReesZeroMatrixSemigroupElement(S, 6, (1, 3, 5), 1);;
gap> [x, y] in cong;
true
gap> [x] in cong;
Error, Semigroups: \in (for a relation): usage,
the first arg <pair> must be a list of length 2,
gap> [x, y, z] in cong;
Error, Semigroups: \in (for a relation): usage,
the first arg <pair> must be a list of length 2,
gap> [t, t] in cong;
Error, Semigroups: \in (for a relation): usage,
elements of the first arg <pair> must be
in the range of the second arg <cong>,
gap> [x, x] in cong;
true
gap> [zero, zero] in cong;
true
gap> [x, zero] in cong;
false
gap> ims := ImagesElm(cong, t);
Error, Semigroups: ImagesElm: usage,
the args <cong> and <elm> must refer to the same semigroup,
gap> ims := ImagesElm(cong, zero);
[ 0 ]

# ReesZeroMatCongTest5: Equivalence classes
gap> class0 := CongruenceClassOfElement(cong, zero);
<congruence class of 0>
gap> class0!.nCoset;
0
gap> HasSize(class0);
false
gap> Size(class0);
1
gap> CanonicalRepresentative(class0);
0
gap> classes := CongruenceClasses(cong);;
gap> Size(classes) = NrCongruenceClasses(cong);
true
gap> class1 := EquivalenceClassOfElement(cong, x);;
gap> class2 := CongruenceClassOfElement(cong, y);;
gap> class3 := EquivalenceClassOfElement(cong, z);
<congruence class of (1,(1,3,5),2)>
gap> EquivalenceClassOfElement(cong, t);
Error, Semigroups: EquivalenceClassOfElement: usage,
the second arg <elm> must be in the semigroup of first arg <cong>,
gap> congs12 := SemigroupCongruence(S, [[RMSElement(S, 2, (), 2),
>                                        RMSElement(S, 5, (3, 5), 2)],
>                                       [RMSElement(S, 3, (1, 3)(4, 5), 1),
>                                        RMSElement(S, 3, (), 1)]]);;
gap> classother := EquivalenceClassOfElement(congs12, x);;
gap> class1 = class2;
true
gap> class1 = class3;
false
gap> y in class1;
true
gap> x in class3;
false
gap> class1 = classes[38];
true
gap> congs13 := SemigroupCongruence(S, [[RMSElement(S, 2, (), 2),
>                                        RMSElement(S, 5, (3, 5), 2)],
>                                       [RMSElement(S, 3, (1, 3)(4, 5), 1),
>                                        RMSElement(S, 3, (), 1)],
>                                       [RMSElement(S, 3, (1, 5, 4), 1),
>                                        RMSElement(S, 6, (1, 5, 3, 4), 1)]]);;
gap> nCoset := RightCoset(congs13!.n, (1, 5));;
gap> class := RZMSCongruenceClassByLinkedTriple(congs13, nCoset, 3, 2);;
gap> class = classes[44];
true
gap> classes[45] * classes[4] = classes[73];  # 0 class
true
gap> classes[28] * classes[32] = classes[36];  # non-0 class
true
gap> classes[28] * classes[32] = classes[15];
false
gap> classes[28] * [classes[32], classes[73]] = [classes[36], class0];
true
gap> class1 * classother;
Error, Semigroups: \*: usage,
the args <c1> and <c2> must be classes of the same congruence,
gap> Size(classes[13]);
4
gap> Size(classes[72]);
4
gap> Size(classes[73]);
1
gap> Size(class0);
1
gap> MultiplicativeZero(S) in class0;
true
gap> x in class0;
false
gap> Enumerator(classes[13]);
[ (1,(),3), (1,(1,4)(3,5),3), (1,(1,5)(3,4),3), (1,(1,3)(4,5),3) ]

# RZMSCongruenceClassByLinkedTriple: bad input
gap> badCoset := RightCoset(SymmetricGroup(6), (1, 5, 4));;
gap> class := RZMSCongruenceClassByLinkedTriple(congs13, badCoset, 3, 2);;
Error, Semigroups: RZMSCongruenceClassByLinkedTriple: usage,
the second arg <nCoset> must be a right coset of <cong>'s
normal subgroup <n> inside the semigroup's underlying group <g>,
gap> class := RZMSCongruenceClassByLinkedTriple(congs13, nCoset, 7, 2);;
Error, Semigroups: RZMSCongruenceClassByLinkedTriple: usage,
the third arg <colClass> is out of range,
gap> class := RZMSCongruenceClassByLinkedTriple(congs13, nCoset, 3, 42);;
Error, Semigroups: RZMSCongruenceClassByLinkedTriple: usage,
the fourth arg <rowClass> is out of range,

# ReesZeroMatCongTest6: Join and meet congruences
gap> congs12 := SemigroupCongruence(S, [[RMSElement(S, 2, (), 2),
>                                        RMSElement(S, 5, (3, 5), 2)],
>                                       [RMSElement(S, 3, (1, 3)(4, 5), 1),
>                                        RMSElement(S, 3, (), 1)]]);;
gap> congs31 := SemigroupCongruence(S,
> [[RMSElement(S, 1, (), 3), RMSElement(S, 4, (3, 5), 3)],
>  [RMSElement(S, 3, (4, 5), 1), RMSElement(S, 3, (), 1)],
>  [RMSElement(S, 3, (1, 5, 4), 1), RMSElement(S, 6, (1, 5, 3, 4), 1)]]);;
gap> cong1 := JoinSemigroupCongruences(congs12, congs31);;
gap> cong1 = JoinSemigroupCongruences(congs31, congs12);
true
gap> cong1 := MeetSemigroupCongruences(congs12, congs31);;
gap> cong1 = MeetSemigroupCongruences(congs31, congs12);
true
gap> JoinSemigroupCongruences(congs[3], congs1[2]);
Error, Semigroups: JoinSemigroupCongruences: usage,
congruences must be defined over the same semigroup,
gap> MeetSemigroupCongruences(congs[3], congs1[2]);
Error, Semigroups: MeetSemigroupCongruences: usage,
congruences must be defined over the same semigroup,

# ReesZeroMatCongTest7: Quotients
gap> congs13 := SemigroupCongruence(S,
> [[RMSElement(S, 2, (), 2), RMSElement(S, 5, (3, 5), 2)],
>  [RMSElement(S, 3, (1, 3)(4, 5), 1), RMSElement(S, 3, (), 1)],
>  [RMSElement(S, 3, (1, 5, 4), 1), RMSElement(S, 6, (1, 5, 3, 4), 1)]]);;
gap> q := S / congs13;;
gap> Size(q);
73

# ReesZeroMatCongTest8
# Convert to and from semigroup congruence by generating pairs
gap> ForAll(congs, x -> x = AsRZMSCongruenceByLinkedTriple(
>  AsSemigroupCongruenceByGeneratingPairs(x)));
true

# IsSubrelation: bad input (with zero)
gap> g := SymmetricGroup(4);;
gap> mat := [[(), (1, 2)(3, 4)],
>            [(), ()],
>            [(2, 4), (1, 3, 4, 2)]];;
gap> T := ReesZeroMatrixSemigroup(g, mat);;
gap> n := Group([(2, 4, 3), (1, 4)(2, 3), (1, 3)(2, 4)]);;
gap> colBlocks := [[1, 2]];;
gap> rowBlocks := [[1], [2, 3]];;
gap> cong2 := RZMSCongruenceByLinkedTriple(T, n, colBlocks, rowBlocks);;
gap> IsSubrelation(cong2, congs[3]);
Error, Semigroups: IsSubrelation: usage,
congruences must be defined over the same semigroup,

# ReesZeroMatCongTest9: Universal semigroup congruences
gap> uni := UniversalSemigroupCongruence(S);;
gap> [x, z] in uni;
true
gap> Length(CongruenceClasses(uni)) = 1 and
>   (Representative(CongruenceClasses(uni)[1]) = RMSElement(S, 1, (1, 4, 5), 3)
> or Representative(CongruenceClasses(uni)[1]) =
>   RMSElement(S, 1, (), 1));  # the first is after 4.7.7 the latter before
true
gap> eq := CongruenceClassOfElement(uni, y);
<congruence class of (6,(1,3,5),1)>
gap> eq := EquivalenceClassOfElement(uni, y);;
gap> z in eq;
true
gap> cong := AsSemigroupCongruenceByGeneratingPairs(uni);;
gap> cong := AsRZMSCongruenceByLinkedTriple(cong);;
gap> cong = uni;
true
gap> Size(S / uni);
1

# CongruencesOfSemigroup: another example
gap> g := Group([(1, 4, 5), (1, 5, 3, 4)]);;
gap> StructureDescription(g);
"S4"
gap> mat := [[0, (4, 5), (3, 4), (1, 4, 3), 0],
>            [0, (1, 3, 5, 4), (1, 5, 3), (), 0],
>            [(), 0, (1, 5), (), (1, 4, 3)],
>            [0, (1, 4, 3), (), (4, 5), 0]];;
gap> S := ReesZeroMatrixSemigroup(g, mat);;
gap> congs := CongruencesOfSemigroup(S);;
gap> Size(congs);
29
gap> congs7 := SemigroupCongruence(S,
> [RMSElement(S, 2, (4, 5), 1), RMSElement(S, 2, (1, 3, 4), 4)]);
<semigroup congruence over <Rees 0-matrix semigroup 5x4 over S4>
  with linked triple (A4,5,3)>
gap> congs25 := SemigroupCongruence(S,
> [[RMSElement(S, 1, (), 3), RMSElement(S, 5, (1, 3, 4), 3)],
>  [RMSElement(S, 3, (), 4), RMSElement(S, 4, (4, 5), 4)]]);
<semigroup congruence over <Rees 0-matrix semigroup 5x4 over S4>
  with linked triple (S4,3,4)>
gap> cong1 := JoinSemigroupCongruences(congs7, congs25);
<semigroup congruence over <Rees 0-matrix semigroup 5x4 over S4>
  with linked triple (S4,3,3)>
gap> cong1 = JoinSemigroupCongruences(congs25, congs7);
true
gap> cong1 := MeetSemigroupCongruences(congs7, congs25);
<semigroup congruence over <Rees 0-matrix semigroup 5x4 over S4>
  with linked triple (A4,5,4)>
gap> cong1 = MeetSemigroupCongruences(congs25, congs7);
true
gap> congs9 := SemigroupCongruence(S,
> [[RMSElement(S, 1, (), 3), RMSElement(S, 5, (1, 3, 4), 3)],
>  [RMSElement(S, 2, (4, 5), 1), RMSElement(S, 2, (1, 3, 4), 4)]]);
<semigroup congruence over <Rees 0-matrix semigroup 5x4 over S4>
  with linked triple (A4,4,3)>
gap> congs28 := RZMSCongruenceByLinkedTriple(S, g, [[1, 5], [2], [3, 4]],
> [[1, 2, 4], [3]]);
<semigroup congruence over <Rees 0-matrix semigroup 5x4 over S4>
  with linked triple (S4,3,2)>
gap> congs28 = SemigroupCongruence(S,
> GeneratingPairsOfMagmaCongruence(congs28));
true
gap> MeetSemigroupCongruences(congs9, congs28);
<semigroup congruence over <Rees 0-matrix semigroup 5x4 over S4>
  with linked triple (A4,4,3)>
gap> cong := AsSemigroupCongruenceByGeneratingPairs(congs9);;
gap> AsRZMSCongruenceByLinkedTriple(cong) = congs9;
true
gap> cong = congs9;
true

# AsRZMSCongruenceByLinkedTriple
gap> g := Group([(1, 4, 5), (1, 5, 3, 4)]);;
gap> StructureDescription(g);
"S4"
gap> mat := [[0, (4, 5), (3, 4), (1, 4, 3), 0],
>            [0, (1, 3, 5, 4), (1, 5, 3), (), 0],
>            [(), 0, (1, 5), (), (1, 4, 3)],
>            [0, (1, 4, 3), (), (4, 5), 0]];;
gap> S := ReesZeroMatrixSemigroup(g, mat);;
gap> x := RMSElement(S, 5, (3, 4), 2);;
gap> y := RMSElement(S, 1, (), 4);;
gap> cong := SemigroupCongruenceByGeneratingPairs(S, [[x, y]]);;
gap> AsRZMSCongruenceByLinkedTriple(cong);
<semigroup congruence over <Rees 0-matrix semigroup 5x4 over S4>
  with linked triple (S4,4,3)>

# IsLinkedTriple: bad input
gap> g := Group([(1, 4, 5), (1, 5, 3, 4)]);;
gap> mat := [[0, 0, (1, 4, 5), 0, 0, (1, 4, 3, 5)],
> [0, (), 0, 0, (3, 5), 0],
> [0, 0, 0, (3, 5), 0, 0]];;
gap> S := ReesZeroMatrixSemigroup(g, mat);;
gap> IsLinkedTriple(S, SymmetricGroup(4), [], [[1]]);
Error, Semigroups: IsLinkedTriple: usage,
the first arg <S> must be a finite 0-simple Rees 0-matrix semigroup,
gap> g := Semigroup([Transformation([1, 3, 2]),
>                    Transformation([2, 2, 1])]);;
gap> mat := [[Transformation([1, 3, 2]), Transformation([2, 2, 2])],
>            [Transformation([1, 3, 2]), Transformation([3, 1, 3])]];;
gap> S := ReesMatrixSemigroup(g, mat);;
gap> IsLinkedTriple(S, SymmetricGroup(2), [], [[1]]);
Error, Semigroups: IsLinkedTriple: usage,
the first arg <S> must be a finite simple Rees matrix semigroup,

# ReesCongTest6: Enumerator
gap> s := Semigroup([Transformation([1, 1, 3, 1, 3]),
>                      Transformation([2, 1, 2, 2, 2]),
>                      Transformation([3, 1, 3, 2, 4])]);;
gap> i := SemigroupIdeal(s, Transformation([1, 1, 1, 3, 1]));;
gap> cong := ReesCongruenceOfSemigroupIdeal(i);;
gap> class := CongruenceClassOfElement(cong, Transformation([3, 3, 3, 3, 3]));;
gap> enum := Enumerator(class);;
gap> Size(enum);
5
gap> class := CongruenceClassOfElement(cong, Transformation([2, 2, 2, 1, 2]));;
gap> enum := Enumerator(class);
[ Transformation( [ 2, 2, 2, 1, 2 ] ) ]
gap> Size(enum);
1

# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(T);
gap> Unbind(badCoset);
gap> Unbind(ccong);
gap> Unbind(class);
gap> Unbind(class0);
gap> Unbind(class1);
gap> Unbind(class2);
gap> Unbind(class3);
gap> Unbind(classes);
gap> Unbind(classother);
gap> Unbind(colBlocks);
gap> Unbind(cong);
gap> Unbind(cong1);
gap> Unbind(cong2);
gap> Unbind(congs);
gap> Unbind(congs1);
gap> Unbind(eq);
gap> Unbind(g);
gap> Unbind(ims);
gap> Unbind(mat);
gap> Unbind(n);
gap> Unbind(nCoset);
gap> Unbind(q);
gap> Unbind(rowBlocks);
gap> Unbind(t);
gap> Unbind(uni);
gap> Unbind(x);
gap> Unbind(y);
gap> Unbind(z);
gap> Unbind(zero);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/congrms.tst");
