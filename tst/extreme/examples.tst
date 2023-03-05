#############################################################################
##
#W  extreme/examples.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

#@local S, gens
gap> START_TEST("Semigroups package: extreme/examples.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# ExamplesTest1
gap> gens := [Transformation([2, 8, 3, 7, 1, 5, 2, 6]),
> Transformation([3, 5, 7, 2, 5, 6, 3, 8]),
> Transformation([4, 1, 8, 3, 5, 7, 3, 5]),
> Transformation([4, 3, 4, 5, 6, 4, 1, 2]),
> Transformation([5, 4, 8, 8, 5, 6, 1, 5]),
> Transformation([6, 7, 4, 1, 4, 1, 6, 2]),
> Transformation([7, 1, 2, 2, 2, 7, 4, 5]),
> Transformation([8, 8, 5, 1, 7, 5, 2, 8])];;
gap> S := Semigroup(gens);;
gap> Size(S);
597369
gap> NrRClasses(S);
10139
gap> NrDClasses(S);
257
gap> NrLClasses(S);
3065
gap> NrHClasses(S);
50989
gap> NrIdempotents(S);
8194
gap> NrRegularDClasses(S);
6
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
8
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest2
gap> gens := [Transformation([2, 8, 3, 7, 1, 5, 2, 6]),
> Transformation([3, 5, 7, 2, 5, 6, 3, 8]),
> Transformation([6, 7, 4, 1, 4, 1, 6, 2]),
> Transformation([8, 8, 5, 1, 7, 5, 2, 8])];;
gap> S := Semigroup(gens);;
gap> Size(S);
95540
gap> NrRClasses(S);
6343
gap> NrDClasses(S);
944
gap> NrLClasses(S);
9904
gap> NrHClasses(S);
23659
gap> NrIdempotents(S);
2595
gap> NrRegularDClasses(S);
6
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
8
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest3 
gap> gens := [Transformation([2, 6, 7, 2, 6, 1, 1, 5]),
> Transformation([3, 8, 1, 4, 5, 6, 7, 1]),
> Transformation([4, 3, 2, 7, 7, 6, 6, 5]),
> Transformation([7, 1, 7, 4, 2, 5, 6, 3])];;
gap> S := Semigroup(gens);;
gap> Size(S);
233605
gap> NrRClasses(S);
4396
gap> NrDClasses(S);
661
gap> NrLClasses(S);
16914
gap> NrHClasses(S);
40882
gap> NrIdempotents(S);
4891
gap> NrRegularDClasses(S);
7
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
8
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest4 
gap> gens := [Transformation([1, 5, 6, 2, 5, 2, 1]),
> Transformation([1, 7, 5, 4, 3, 5, 7]),
> Transformation([2, 7, 7, 2, 4, 1, 1]),
> Transformation([3, 2, 2, 4, 1, 7, 6]),
> Transformation([3, 3, 5, 1, 7, 1, 6]),
> Transformation([3, 3, 6, 1, 7, 5, 2]),
> Transformation([3, 4, 6, 5, 4, 4, 7]),
> Transformation([5, 2, 4, 5, 1, 4, 5]),
> Transformation([5, 5, 2, 2, 6, 7, 2]),
> Transformation([7, 7, 5, 4, 5, 3, 2])];;
gap> S := Semigroup(gens);;
gap> Size(S);
97310
gap> NrRClasses(S);
879
gap> NrDClasses(S);
401
gap> NrLClasses(S);
1207
gap> NrHClasses(S);
10664
gap> NrIdempotents(S);
2434
gap> NrRegularDClasses(S);
6
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
7
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest5 
gap> gens := [Transformation([3, 4, 1, 2, 1]),
> Transformation([4, 2, 1, 5, 5]),
> Transformation([4, 2, 2, 2, 4])];;
gap> S := Semigroup(gens);;
gap> Size(S);
731
gap> NrRClasses(S);
26
gap> NrDClasses(S);
4
gap> NrLClasses(S);
23
gap> NrHClasses(S);
194
gap> NrIdempotents(S);
100
gap> NrRegularDClasses(S);
4
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
5
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest6 
gap> gens := [Transformation([1, 3, 4, 1]),
> Transformation([2, 4, 1, 2]),
> Transformation([3, 1, 1, 3]),
> Transformation([3, 3, 4, 1])];;
gap> S := Semigroup(gens);;
gap> Size(S);
61
gap> NrRClasses(S);
9
gap> NrDClasses(S);
5
gap> NrLClasses(S);
14
gap> NrHClasses(S);
34
gap> NrIdempotents(S);
19
gap> NrRegularDClasses(S);
3
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
4
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest7 
gap> gens := [Transformation([1, 3, 2, 3]),
> Transformation([1, 4, 1, 2]),
> Transformation([2, 4, 1, 1]),
> Transformation([3, 4, 2, 2])];;
gap> S := Semigroup(gens);;
gap> Size(S);
114
gap> NrRClasses(S);
11
gap> NrDClasses(S);
5
gap> NrLClasses(S);
19
gap> NrHClasses(S);
51
gap> NrIdempotents(S);
28
gap> NrRegularDClasses(S);
4
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
4
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest8 
gap> gens := [Transformation([1, 3, 2, 3]),
> Transformation([1, 4, 1, 2]),
> Transformation([3, 4, 2, 2]),
> Transformation([4, 1, 2, 1])];;
gap> S := Semigroup(gens);;
gap> Size(S);
68
gap> NrRClasses(S);
16
gap> NrDClasses(S);
8
gap> NrLClasses(S);
20
gap> NrHClasses(S);
40
gap> NrIdempotents(S);
21
gap> NrRegularDClasses(S);
5
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
4
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest9
gap> gens := [Transformation([1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10]),
> Transformation([2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7])];;
gap> S := Semigroup(gens);;
gap> Size(S);
20167
gap> NrRClasses(S);
9
gap> NrDClasses(S);
2
gap> NrLClasses(S);
2
gap> NrHClasses(S);
9
gap> NrIdempotents(S);
9
gap> NrRegularDClasses(S);
2
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
20160
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
true
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
true
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
false
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest10
gap> gens := [Transformation([2, 1, 4, 5, 3, 7, 8, 9, 10, 6]),
> Transformation([1, 2, 4, 3, 5, 6, 7, 8, 9, 10]),
> Transformation([1, 2, 3, 4, 5, 6, 10, 9, 8, 7]),
> Transformation([9, 1, 4, 3, 6, 9, 3, 4, 3, 9])];;
gap> S := Semigroup(gens);;
gap> Size(S);
491558
gap> NrRClasses(S);
2072
gap> NrDClasses(S);
12
gap> NrLClasses(S);
425
gap> NrHClasses(S);
86036
gap> NrIdempotents(S);
13655
gap> NrRegularDClasses(S);
9
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
IdentityTransformation
gap> One(S);
IdentityTransformation
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
8
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
true
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest11
gap> gens := [Transformation([13, 10, 9, 5, 1, 5, 13, 13, 8, 2, 7, 2, 6]),
> Transformation([6, 11, 12, 10, 4, 10, 13, 5, 8, 5, 11, 6, 9])];;
gap> S := Semigroup(gens);;
gap> Size(S);
208650
gap> NrRClasses(S);
31336
gap> NrDClasses(S);
3807
gap> NrLClasses(S);
18856
gap> NrHClasses(S);
70693
gap> NrIdempotents(S);
5857
gap> NrRegularDClasses(S);
8
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
11
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest12
gap> gens := [Transformation([12, 10, 8, 5, 1, 5, 12, 12, 8, 2, 6, 2]),
> Transformation([5, 6, 10, 11, 10, 4, 10, 12, 5, 7, 4, 10]),
> Transformation([6, 8, 12, 5, 4, 8, 10, 7, 4, 1, 10, 11])];;
gap> S := Semigroup(gens);;
gap> Size(S);
945560
gap> NrRClasses(S);
19658
gap> NrDClasses(S);
4092
gap> NrLClasses(S);
132176
gap> NrHClasses(S);
215008
gap> NrIdempotents(S);
15053
gap> NrRegularDClasses(S);
6
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
10
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest13
gap> gens := [Transformation([2, 3, 4, 5, 1, 8, 7, 6, 2, 7]),
> Transformation([5, 4, 1, 2, 3, 7, 6, 5, 4, 1]),
> Transformation([2, 1, 4, 3, 2, 1, 4, 4, 3, 3])];;
gap> S := Semigroup(gens);;
gap> Size(S);
188315
gap> NrRClasses(S);
2105
gap> NrDClasses(S);
8
gap> NrLClasses(S);
37
gap> NrHClasses(S);
15018
gap> NrIdempotents(S);
5964
gap> NrRegularDClasses(S);
8
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
5
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest14
gap> gens := [Transformation([8, 7, 5, 3, 1, 3, 8, 8]),
> Transformation([5, 1, 4, 1, 4, 4, 7, 8])];;
gap> S := Semigroup(gens);;
gap> Size(S);
56
gap> NrRClasses(S);
16
gap> NrDClasses(S);
7
gap> NrLClasses(S);
18
gap> NrHClasses(S);
54
gap> NrIdempotents(S);
16
gap> NrRegularDClasses(S);
4
gap> MultiplicativeZero(S);
Transformation( [ 8, 8, 8, 8, 8, 8, 8, 8 ] )
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
1
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest15 
gap> gens := [Transformation([5, 4, 4, 2, 1]),
> Transformation([2, 5, 5, 4, 1])];;
gap> S := Semigroup(gens);;
gap> Size(S);
12
gap> NrRClasses(S);
1
gap> NrDClasses(S);
1
gap> NrLClasses(S);
1
gap> NrHClasses(S);
1
gap> NrIdempotents(S);
1
gap> NrRegularDClasses(S);
1
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
Transformation( [ 1, 2, 2 ] )
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
12
gap> IsBlockGroup(S);
true
gap> IsSemigroupWithCommutingIdempotents(S);
true
gap> IsCliffordSemigroup(S);
true
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
true
gap> IsCompletelySimpleSemigroup(S);
true
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
true
gap> IsInverseSemigroup(S);
true
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
true
gap> IsOrthodoxSemigroup(S);
true
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
true
gap> IsSynchronizingSemigroup(S);
false
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest16 
gap> gens := [Transformation([1, 2, 1, 3, 3]),
> Transformation([2, 2, 3, 5, 5])];;
gap> S := Semigroup(gens);;
gap> Size(S);
8
gap> NrRClasses(S);
8
gap> NrDClasses(S);
8
gap> NrLClasses(S);
8
gap> NrHClasses(S);
8
gap> NrIdempotents(S);
3
gap> NrRegularDClasses(S);
3
gap> MultiplicativeZero(S);
Transformation( [ 2, 2, 2, 2, 2 ] )
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
1
gap> IsBlockGroup(S);
true
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
true
gap> IsLTrivial(S);
true
gap> IsRTrivial(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest17 
gap> gens := [Transformation([3, 1, 2, 3, 2, 3, 2, 3]),
> Transformation([2, 5, 8, 5, 2, 5, 7, 8])];;
gap> S := Semigroup(gens);;
gap> Size(S);
38
gap> NrRClasses(S);
4
gap> NrDClasses(S);
2
gap> NrLClasses(S);
3
gap> NrHClasses(S);
7
gap> NrIdempotents(S);
7
gap> NrRegularDClasses(S);
2
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
36
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
true
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
false
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest18
gap> gens := [Transformation([3, 3, 2, 6, 2, 4, 4, 6]),
> Transformation([5, 1, 7, 8, 7, 5, 8, 1])];;
gap> S := Semigroup(gens);;
gap> Size(S);
96
gap> NrRClasses(S);
2
gap> NrDClasses(S);
1
gap> NrLClasses(S);
2
gap> NrHClasses(S);
4
gap> NrIdempotents(S);
4
gap> NrRegularDClasses(S);
1
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
96
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
true
gap> IsCompletelySimpleSemigroup(S);
true
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
true
gap> IsSynchronizingSemigroup(S);
false
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest19 
gap> gens := [Transformation([10, 8, 7, 4, 1, 4, 10, 10, 7, 2]),
> Transformation([5, 2, 5, 5, 9, 10, 8, 3, 8, 10])];;
gap> S := Semigroup(gens);;
gap> Size(S);
30176
gap> NrRClasses(S);
152
gap> NrDClasses(S);
11
gap> NrLClasses(S);
456
gap> NrHClasses(S);
4234
gap> NrIdempotents(S);
1105
gap> NrRegularDClasses(S);
7
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
8
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest20
gap> gens := [Transformation([2, 3, 4, 5, 1, 8, 7, 6, 2, 7]),
> Transformation([2, 3, 4, 5, 6, 8, 7, 1, 2, 2])];;
gap> S := Semigroup(gens);;
gap> Size(S);
10080
gap> NrRClasses(S);
2
gap> NrDClasses(S);
1
gap> NrLClasses(S);
1
gap> NrHClasses(S);
2
gap> NrIdempotents(S);
2
gap> NrRegularDClasses(S);
1
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
10080
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
true
gap> IsCompletelySimpleSemigroup(S);
true
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
true
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
true
gap> IsSynchronizingSemigroup(S);
false
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest21
gap> gens := [Transformation([2, 3, 4, 5, 1, 8, 7, 6, 2, 7]),
> Transformation([3, 8, 7, 4, 1, 4, 3, 3, 7, 2])];;
gap> S := Semigroup(gens);;
gap> Size(S);
121804
gap> NrRClasses(S);
462
gap> NrDClasses(S);
33
gap> NrLClasses(S);
8320
gap> NrHClasses(S);
24159
gap> NrIdempotents(S);
4161
gap> NrRegularDClasses(S);
6
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
8
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest22
gap> gens := [Transformation([1, 4, 6, 2, 5, 3, 7, 8]),
> Transformation([6, 3, 2, 7, 5, 1, 8, 8])];;
gap> S := Semigroup(gens);;
gap> Size(S);
131
gap> NrRClasses(S);
41
gap> NrDClasses(S);
11
gap> NrLClasses(S);
25
gap> NrHClasses(S);
101
gap> NrIdempotents(S);
16
gap> NrRegularDClasses(S);
6
gap> MultiplicativeZero(S);
Transformation( [ 8, 8, 8, 8, 5, 8, 8, 8 ] )
gap> MultiplicativeNeutralElement(S);
IdentityTransformation
gap> One(S);
IdentityTransformation
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
1
gap> IsBlockGroup(S);
true
gap> IsSemigroupWithCommutingIdempotents(S);
true
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
true
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
false
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest23
gap> gens := [Transformation([5, 6, 7, 3, 1, 4, 2, 8]),
> Transformation([3, 6, 8, 5, 7, 4, 2, 8])];;
gap> S := Semigroup(gens);;
gap> Size(S);
52300
gap> NrRClasses(S);
130
gap> NrDClasses(S);
14
gap> NrLClasses(S);
2014
gap> NrHClasses(S);
11646
gap> NrIdempotents(S);
94
gap> NrRegularDClasses(S);
7
gap> MultiplicativeZero(S);
Transformation( [ 8, 8, 8, 8, 8, 8, 8, 8 ] )
gap> MultiplicativeNeutralElement(S);
IdentityTransformation
gap> One(S);
IdentityTransformation
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
1
gap> IsBlockGroup(S);
true
gap> IsSemigroupWithCommutingIdempotents(S);
true
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
true
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest24 
gap> gens := [Transformation([1, 2, 4, 5, 6, 3, 7, 8]),
> Transformation([3, 3, 4, 5, 6, 2, 7, 8]),
> Transformation([1, 2, 5, 3, 6, 8, 4, 4])];;
gap> S := Semigroup(gens);;
gap> Size(S);
864
gap> NrRClasses(S);
4
gap> NrDClasses(S);
4
gap> NrLClasses(S);
4
gap> NrHClasses(S);
4
gap> NrIdempotents(S);
4
gap> NrRegularDClasses(S);
4
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
IdentityTransformation
gap> One(S);
IdentityTransformation
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
720
gap> IsBlockGroup(S);
true
gap> IsSemigroupWithCommutingIdempotents(S);
true
gap> IsCliffordSemigroup(S);
true
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
true
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
true
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
true
gap> IsOrthodoxSemigroup(S);
true
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
false
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest25
gap> gens := [Transformation([1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
> 4, 4, 4, 4, 4]),
> Transformation([1, 2, 3, 4, 5, 6, 7, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
> 4]),
> Transformation([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 4, 4, 4, 4, 4, 4, 4, 4,
> 4, 4]),
> Transformation([1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 12, 13, 14, 15, 16, 17,
> 18, 19, 20, 21]),
> Transformation([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
> 18, 19, 20, 21])];;
gap> S := Semigroup(gens);;
gap> Size(S);
5
gap> NrRClasses(S);
5
gap> NrDClasses(S);
5
gap> NrLClasses(S);
5
gap> NrHClasses(S);
5
gap> NrIdempotents(S);
5
gap> NrRegularDClasses(S);
5
gap> MultiplicativeZero(S);
Transformation( [ 1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  4 ] )
gap> MultiplicativeNeutralElement(S);
IdentityTransformation
gap> One(S);
IdentityTransformation
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
1
gap> IsBlockGroup(S);
true
gap> IsSemigroupWithCommutingIdempotents(S);
true
gap> IsCliffordSemigroup(S);
true
gap> IsCommutative(S);
true
gap> IsCompletelyRegularSemigroup(S);
true
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
true
gap> IsLTrivial(S);
true
gap> IsRTrivial(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
true
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
true
gap> IsMonoid(S);
true
gap> IsOrthodoxSemigroup(S);
true
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
true
gap> IsSemilattice(S);
true
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
false
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest26
gap> gens := [Transformation([2, 1, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
> 4, 4, 4, 4, 4]),
> Transformation([2, 3, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
> 1]),
> Transformation([1, 2, 3, 4, 6, 5, 7, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
> 4]),
> Transformation([1, 2, 3, 4, 6, 7, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
> 4]),
> Transformation([1, 2, 3, 4, 5, 6, 7, 9, 8, 10, 11, 4, 4, 4, 4, 4, 4, 4, 4,
> 4, 4]),
> Transformation([1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 10, 4, 4, 4, 4, 4, 4, 4, 4,
> 4, 4]),
> Transformation([1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 13, 12, 14, 15, 16, 17,
> 18, 19, 20, 21]),
> Transformation([1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 13, 14, 15, 16, 12, 17,
> 18, 19, 20, 21]),
> Transformation([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18,
> 19, 20, 21, 17])];;
gap> S := Semigroup(gens);;
gap> Size(S);
639
gap> NrRClasses(S);
5
gap> NrDClasses(S);
5
gap> NrLClasses(S);
5
gap> NrHClasses(S);
5
gap> NrIdempotents(S);
5
gap> NrRegularDClasses(S);
5
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
IdentityTransformation
gap> One(S);
IdentityTransformation
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
24
gap> IsBlockGroup(S);
true
gap> IsSemigroupWithCommutingIdempotents(S);
true
gap> IsCliffordSemigroup(S);
true
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
true
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
true
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
true
gap> IsOrthodoxSemigroup(S);
true
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
false
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest27
gap> gens := [Transformation([2, 1, 1, 2, 1]),
> Transformation([3, 4, 3, 4, 4]),
> Transformation([3, 4, 3, 4, 3]),
> Transformation([4, 3, 3, 4, 4])];;
gap> S := Semigroup(gens);;
gap> Size(S);
16
gap> NrRClasses(S);
4
gap> NrDClasses(S);
1
gap> NrLClasses(S);
2
gap> NrHClasses(S);
8
gap> NrIdempotents(S);
8
gap> NrRegularDClasses(S);
1
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
16
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
true
gap> IsCompletelySimpleSemigroup(S);
true
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
true
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
true
gap> IsSynchronizingSemigroup(S);
false
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest28
gap> gens := [Transformation([4, 4, 4, 1, 1, 6, 7, 8, 9, 10, 11, 1]),
> Transformation([6, 6, 6, 7, 7, 1, 4, 8, 9, 10, 11, 7]),
> Transformation([8, 8, 8, 9, 9, 10, 11, 1, 4, 6, 7, 9]),
> Transformation([2, 2, 2, 4, 4, 6, 7, 8, 9, 10, 11, 4]),
> Transformation([1, 1, 1, 5, 5, 6, 7, 8, 9, 10, 11, 5]),
> Transformation([1, 1, 4, 4, 4, 6, 7, 8, 9, 10, 11, 1]),
> Transformation([1, 1, 7, 4, 4, 6, 7, 8, 9, 10, 11, 6])];;
gap> S := Semigroup(gens);;
gap> Size(S);
1152
gap> NrRClasses(S);
3
gap> NrDClasses(S);
1
gap> NrLClasses(S);
3
gap> NrHClasses(S);
9
gap> NrIdempotents(S);
9
gap> NrRegularDClasses(S);
1
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
1152
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
true
gap> IsCompletelySimpleSemigroup(S);
true
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
true
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
true
gap> IsSynchronizingSemigroup(S);
false
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest29
gap> gens := [Transformation([1, 2, 2, 1, 2]),
> Transformation([3, 4, 3, 4, 4]),
> Transformation([3, 4, 3, 4, 3]),
> Transformation([4, 3, 3, 4, 4])];;
gap> S := Semigroup(gens);;
gap> Size(S);
16
gap> NrRClasses(S);
4
gap> NrDClasses(S);
1
gap> NrLClasses(S);
2
gap> NrHClasses(S);
8
gap> NrIdempotents(S);
8
gap> NrRegularDClasses(S);
1
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
fail
gap> One(S);
fail
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
16
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
true
gap> IsCompletelySimpleSemigroup(S);
true
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
false
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
true
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
true
gap> IsSynchronizingSemigroup(S);
false
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest30
gap> gens := [Transformation([2, 6, 1, 7, 5, 3, 4]),
> Transformation([5, 3, 7, 2, 1, 6, 4]),
> Transformation([2, 5, 5, 3, 4, 2, 3]),
> Transformation([1, 5, 1, 6, 1, 5, 6]),
> Transformation([6, 2, 2, 2, 5, 1, 2]),
> Transformation([7, 5, 4, 4, 4, 5, 5]),
> Transformation([5, 1, 6, 1, 1, 5, 1]),
> Transformation([3, 5, 2, 3, 2, 2, 3])];;
gap> S := Semigroup(gens);;
gap> Size(S);
21343
gap> NrRClasses(S);
401
gap> NrDClasses(S);
7
gap> NrLClasses(S);
99
gap> NrHClasses(S);
4418
gap> NrIdempotents(S);
1471
gap> NrRegularDClasses(S);
6
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
IdentityTransformation
gap> One(S);
IdentityTransformation
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
7
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
true
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# ExamplesTest31
gap> gens := [Transformation([3, 6, 9, 1, 4, 7, 2, 5, 8]),
> Transformation([3, 6, 9, 7, 1, 4, 5, 8, 2]),
> Transformation([8, 2, 5, 5, 4, 5, 5, 2, 8]),
> Transformation([4, 4, 8, 4, 4, 2, 4, 4, 5])];;
gap> S := Semigroup(gens);;
gap> Size(S);
82953
gap> NrRClasses(S);
503
gap> NrDClasses(S);
7
gap> NrLClasses(S);
214
gap> NrHClasses(S);
16426
gap> NrIdempotents(S);
3718
gap> NrRegularDClasses(S);
6
gap> MultiplicativeZero(S);
fail
gap> MultiplicativeNeutralElement(S);
IdentityTransformation
gap> One(S);
IdentityTransformation
gap> if GroupOfUnits(S) <> fail then
>   StructureDescription(GroupOfUnits(S));
> fi;;
gap> Size(MinimalIdeal(S));
9
gap> IsBlockGroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false
gap> IsCliffordSemigroup(S);
false
gap> IsCommutative(S);
false
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCompletelySimpleSemigroup(S);
false
gap> IsHTrivial(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsInverseSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsMonoidAsSemigroup(S);
true
gap> IsOrthodoxSemigroup(S);
false
gap> IsRectangularBand(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsSemiband(S);
false
gap> IsSemilattice(S);
false
gap> IsSimpleSemigroup(S);
false
gap> IsSynchronizingSemigroup(S);
true
gap> IsZeroGroup(S);
false
gap> IsZeroSemigroup(S);
false

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/examples.tst");
