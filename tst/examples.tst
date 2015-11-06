#############################################################################
##
#W  examples.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
gap> START_TEST("Semigroups package: examples.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#T# ExamplesTest1
gap> gens:=[Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
> Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
> Transformation( [ 4, 1, 8, 3, 5, 7, 3, 5 ] ),
> Transformation( [ 4, 3, 4, 5, 6, 4, 1, 2 ] ),
> Transformation( [ 5, 4, 8, 8, 5, 6, 1, 5 ] ),
> Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
> Transformation( [ 7, 1, 2, 2, 2, 7, 4, 5 ] ),
> Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
597369
gap> NrRClasses(s);
10139
gap> NrDClasses(s);
257
gap> NrLClasses(s);
3065
gap> NrHClasses(s);
50989
gap> NrIdempotents(s);
8194
gap> NrRegularDClasses(s);
6
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
8
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest2
gap> gens:=[Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
> Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
> Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
> Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
95540
gap> NrRClasses(s);
6343
gap> NrDClasses(s);
944
gap> NrLClasses(s);
9904
gap> NrHClasses(s);
23659
gap> NrIdempotents(s);
2595
gap> NrRegularDClasses(s);
6
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
8
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest3 
gap> gens:=[Transformation( [ 2, 6, 7, 2, 6, 1, 1, 5 ] ),
> Transformation( [ 3, 8, 1, 4, 5, 6, 7, 1 ] ),
> Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] ),
> Transformation( [ 7, 1, 7, 4, 2, 5, 6, 3 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
233605
gap> NrRClasses(s);
4396
gap> NrDClasses(s);
661
gap> NrLClasses(s);
16914
gap> NrHClasses(s);
40882
gap> NrIdempotents(s);
4891
gap> NrRegularDClasses(s);
7
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
8
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest4 
gap> gens:=[Transformation( [ 1, 5, 6, 2, 5, 2, 1 ] ),
> Transformation( [ 1, 7, 5, 4, 3, 5, 7 ] ),
> Transformation( [ 2, 7, 7, 2, 4, 1, 1 ] ),
> Transformation( [ 3, 2, 2, 4, 1, 7, 6 ] ),
> Transformation( [ 3, 3, 5, 1, 7, 1, 6 ] ),
> Transformation( [ 3, 3, 6, 1, 7, 5, 2 ] ),
> Transformation( [ 3, 4, 6, 5, 4, 4, 7 ] ),
> Transformation( [ 5, 2, 4, 5, 1, 4, 5 ] ),
> Transformation( [ 5, 5, 2, 2, 6, 7, 2 ] ),
> Transformation( [ 7, 7, 5, 4, 5, 3, 2 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
97310
gap> NrRClasses(s);
879
gap> NrDClasses(s);
401
gap> NrLClasses(s);
1207
gap> NrHClasses(s);
10664
gap> NrIdempotents(s);
2434
gap> NrRegularDClasses(s);
6
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
7
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest5 
gap> gens:=[Transformation( [ 3, 4, 1, 2, 1 ] ),
> Transformation( [ 4, 2, 1, 5, 5 ] ),
> Transformation( [ 4, 2, 2, 2, 4 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
731
gap> NrRClasses(s);
26
gap> NrDClasses(s);
4
gap> NrLClasses(s);
23
gap> NrHClasses(s);
194
gap> NrIdempotents(s);
100
gap> NrRegularDClasses(s);
4
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
5
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest6 
gap> gens:=[Transformation( [ 1, 3, 4, 1 ] ),
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ),
> Transformation( [ 3, 3, 4, 1 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
61
gap> NrRClasses(s);
9
gap> NrDClasses(s);
5
gap> NrLClasses(s);
14
gap> NrHClasses(s);
34
gap> NrIdempotents(s);
19
gap> NrRegularDClasses(s);
3
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
4
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest7 
gap> gens:=[Transformation( [ 1, 3, 2, 3 ] ),
> Transformation( [ 1, 4, 1, 2 ] ),
> Transformation( [ 2, 4, 1, 1 ] ),
> Transformation( [ 3, 4, 2, 2 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
114
gap> NrRClasses(s);
11
gap> NrDClasses(s);
5
gap> NrLClasses(s);
19
gap> NrHClasses(s);
51
gap> NrIdempotents(s);
28
gap> NrRegularDClasses(s);
4
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
4
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest8 
gap> gens:=[Transformation( [ 1, 3, 2, 3 ] ),
> Transformation( [ 1, 4, 1, 2 ] ),
> Transformation( [ 3, 4, 2, 2 ] ),
> Transformation( [ 4, 1, 2, 1 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
68
gap> NrRClasses(s);
16
gap> NrDClasses(s);
8
gap> NrLClasses(s);
20
gap> NrHClasses(s);
40
gap> NrIdempotents(s);
21
gap> NrRegularDClasses(s);
5
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
4
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest9
gap> gens:=[Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] ),
> Transformation( [ 2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
20167
gap> NrRClasses(s);
9
gap> NrDClasses(s);
2
gap> NrLClasses(s);
2
gap> NrHClasses(s);
9
gap> NrIdempotents(s);
9
gap> NrRegularDClasses(s);
2
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
20160
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
true
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
true
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest10
gap> gens:=[Transformation( [ 2, 1, 4, 5, 3, 7, 8, 9, 10, 6 ] ),
> Transformation( [ 1, 2, 4, 3, 5, 6, 7, 8, 9, 10 ] ),
> Transformation( [ 1, 2, 3, 4, 5, 6, 10, 9, 8, 7 ] ),
> Transformation( [ 9, 1, 4, 3, 6, 9, 3, 4, 3, 9 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
491558
gap> NrRClasses(s);
2072
gap> NrDClasses(s);
12
gap> NrLClasses(s);
425
gap> NrHClasses(s);
86036
gap> NrIdempotents(s);
13655
gap> NrRegularDClasses(s);
9
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
IdentityTransformation
gap> One(s);
IdentityTransformation
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
8
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
true
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest11
gap> gens:=[Transformation( [ 13, 10, 9, 5, 1, 5, 13, 13, 8, 2, 7, 2, 6 ] ),
> Transformation( [ 6, 11, 12, 10, 4, 10, 13, 5, 8, 5, 11, 6, 9 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
208650
gap> NrRClasses(s);
31336
gap> NrDClasses(s);
3807
gap> NrLClasses(s);
18856
gap> NrHClasses(s);
70693
gap> NrIdempotents(s);
5857
gap> NrRegularDClasses(s);
8
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
11
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest12
gap> gens:=[Transformation( [ 12, 10, 8, 5, 1, 5, 12, 12, 8, 2, 6, 2 ] ),
> Transformation( [ 5, 6, 10, 11, 10, 4, 10, 12, 5, 7, 4, 10 ] ),
> Transformation( [ 6, 8, 12, 5, 4, 8, 10, 7, 4, 1, 10, 11 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
945560
gap> NrRClasses(s);
19658
gap> NrDClasses(s);
4092
gap> NrLClasses(s);
132176
gap> NrHClasses(s);
215008
gap> NrIdempotents(s);
15053
gap> NrRegularDClasses(s);
6
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
10
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest13
gap> gens:=[Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] ),
> Transformation( [ 5, 4, 1, 2, 3, 7, 6, 5, 4, 1 ] ),
> Transformation( [ 2, 1, 4, 3, 2, 1, 4, 4, 3, 3 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
188315
gap> NrRClasses(s);
2105
gap> NrDClasses(s);
8
gap> NrLClasses(s);
37
gap> NrHClasses(s);
15018
gap> NrIdempotents(s);
5964
gap> NrRegularDClasses(s);
8
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
5
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest14
gap> gens:=[Transformation( [ 8, 7, 5, 3, 1, 3, 8, 8 ] ),
> Transformation( [ 5, 1, 4, 1, 4, 4, 7, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
56
gap> NrRClasses(s);
16
gap> NrDClasses(s);
7
gap> NrLClasses(s);
18
gap> NrHClasses(s);
54
gap> NrIdempotents(s);
16
gap> NrRegularDClasses(s);
4
gap> MultiplicativeZero(s);
Transformation( [ 8, 8, 8, 8, 8, 8, 8, 8 ] )
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
1
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest15 
gap> gens:=[Transformation( [ 5, 4, 4, 2, 1 ] ),
> Transformation( [ 2, 5, 5, 4, 1 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
12
gap> NrRClasses(s);
1
gap> NrDClasses(s);
1
gap> NrLClasses(s);
1
gap> NrHClasses(s);
1
gap> NrIdempotents(s);
1
gap> NrRegularDClasses(s);
1
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
Transformation( [ 1, 2, 2 ] )
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
12
gap> IsBlockGroup(s);
true
gap> IsCliffordSemigroup(s);
true
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
true
gap> IsCompletelySimpleSemigroup(s);
true
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
true
gap> IsInverseSemigroup(s);
true
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
true
gap> IsOrthodoxSemigroup(s);
true
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
true
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest16 
gap> gens:=[Transformation( [ 1, 2, 1, 3, 3 ] ),
> Transformation( [ 2, 2, 3, 5, 5 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
8
gap> NrRClasses(s);
8
gap> NrDClasses(s);
8
gap> NrLClasses(s);
8
gap> NrHClasses(s);
8
gap> NrIdempotents(s);
3
gap> NrRegularDClasses(s);
3
gap> MultiplicativeZero(s);
Transformation( [ 2, 2, 2, 2, 2 ] )
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
1
gap> IsBlockGroup(s);
true
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
true
gap> IsLTrivial(s);
true
gap> IsRTrivial(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest17 
gap> gens:=[Transformation( [ 3, 1, 2, 3, 2, 3, 2, 3 ] ),
> Transformation( [ 2, 5, 8, 5, 2, 5, 7, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
38
gap> NrRClasses(s);
4
gap> NrDClasses(s);
2
gap> NrLClasses(s);
3
gap> NrHClasses(s);
7
gap> NrIdempotents(s);
7
gap> NrRegularDClasses(s);
2
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
36
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
true
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest18
gap> gens:=[Transformation( [ 3, 3, 2, 6, 2, 4, 4, 6 ] ),
> Transformation( [ 5, 1, 7, 8, 7, 5, 8, 1 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
96
gap> NrRClasses(s);
2
gap> NrDClasses(s);
1
gap> NrLClasses(s);
2
gap> NrHClasses(s);
4
gap> NrIdempotents(s);
4
gap> NrRegularDClasses(s);
1
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
96
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
true
gap> IsCompletelySimpleSemigroup(s);
true
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
true
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest19 
gap> gens:=[Transformation( [ 10, 8, 7, 4, 1, 4, 10, 10, 7, 2 ] ),
> Transformation( [ 5, 2, 5, 5, 9, 10, 8, 3, 8, 10 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
30176
gap> NrRClasses(s);
152
gap> NrDClasses(s);
11
gap> NrLClasses(s);
456
gap> NrHClasses(s);
4234
gap> NrIdempotents(s);
1105
gap> NrRegularDClasses(s);
7
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
8
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest20
gap> gens:=[Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] ),
> Transformation( [ 2, 3, 4, 5, 6, 8, 7, 1, 2, 2 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
10080
gap> NrRClasses(s);
2
gap> NrDClasses(s);
1
gap> NrLClasses(s);
1
gap> NrHClasses(s);
2
gap> NrIdempotents(s);
2
gap> NrRegularDClasses(s);
1
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
10080
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
true
gap> IsCompletelySimpleSemigroup(s);
true
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
true
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
true
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest21
gap> gens:=[Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] ),
> Transformation( [ 3, 8, 7, 4, 1, 4, 3, 3, 7, 2 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
121804
gap> NrRClasses(s);
462
gap> NrDClasses(s);
33
gap> NrLClasses(s);
8320
gap> NrHClasses(s);
24159
gap> NrIdempotents(s);
4161
gap> NrRegularDClasses(s);
6
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
8
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest22
gap> gens:=[Transformation( [ 1, 4, 6, 2, 5, 3, 7, 8 ] ),
> Transformation( [ 6, 3, 2, 7, 5, 1, 8, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
131
gap> NrRClasses(s);
41
gap> NrDClasses(s);
11
gap> NrLClasses(s);
25
gap> NrHClasses(s);
101
gap> NrIdempotents(s);
16
gap> NrRegularDClasses(s);
6
gap> MultiplicativeZero(s);
Transformation( [ 8, 8, 8, 8, 5, 8, 8, 8 ] )
gap> MultiplicativeNeutralElement(s);
IdentityTransformation
gap> One(s);
IdentityTransformation
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
1
gap> IsBlockGroup(s);
true
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
true
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest23
gap> gens:=[Transformation( [ 5, 6, 7, 3, 1, 4, 2, 8 ] ),
> Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
52300
gap> NrRClasses(s);
130
gap> NrDClasses(s);
14
gap> NrLClasses(s);
2014
gap> NrHClasses(s);
11646
gap> NrIdempotents(s);
94
gap> NrRegularDClasses(s);
7
gap> MultiplicativeZero(s);
Transformation( [ 8, 8, 8, 8, 8, 8, 8, 8 ] )
gap> MultiplicativeNeutralElement(s);
IdentityTransformation
gap> One(s);
IdentityTransformation
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
1
gap> IsBlockGroup(s);
true
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
true
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest24 
gap> gens:=[Transformation( [ 1, 2, 4, 5, 6, 3, 7, 8 ] ),
> Transformation( [ 3, 3, 4, 5, 6, 2, 7, 8 ] ),
> Transformation( [ 1, 2, 5, 3, 6, 8, 4, 4 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
864
gap> NrRClasses(s);
4
gap> NrDClasses(s);
4
gap> NrLClasses(s);
4
gap> NrHClasses(s);
4
gap> NrIdempotents(s);
4
gap> NrRegularDClasses(s);
4
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
IdentityTransformation
gap> One(s);
IdentityTransformation
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
720
gap> IsBlockGroup(s);
true
gap> IsCliffordSemigroup(s);
true
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
true
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
true
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
true
gap> IsOrthodoxSemigroup(s);
true
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest25
gap> gens:=[Transformation( [ 1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
> 4, 4, 4, 4, 4 ] ),
> Transformation( [ 1, 2, 3, 4, 5, 6, 7, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
> 4 ] ),
> Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 4, 4, 4, 4, 4, 4, 4, 4, 
> 4, 4 ] ),
> Transformation( [ 1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 12, 13, 14, 15, 16, 17, 
> 18, 19, 20, 21 ] ),
> Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
> 18, 19, 20, 21 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
5
gap> NrRClasses(s);
5
gap> NrDClasses(s);
5
gap> NrLClasses(s);
5
gap> NrHClasses(s);
5
gap> NrIdempotents(s);
5
gap> NrRegularDClasses(s);
5
gap> MultiplicativeZero(s);
Transformation( [ 1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  4 ] )
gap> MultiplicativeNeutralElement(s);
IdentityTransformation
gap> One(s);
IdentityTransformation
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
1
gap> IsBlockGroup(s);
true
gap> IsCliffordSemigroup(s);
true
gap> IsCommutative(s);
true
gap> IsCompletelyRegularSemigroup(s);
true
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
true
gap> IsLTrivial(s);
true
gap> IsRTrivial(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
true
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
true
gap> IsMonoid(s);
true
gap> IsOrthodoxSemigroup(s);
true
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
true
gap> IsSemilattice(s);
true
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest26
gap> gens:=[Transformation( [ 2, 1, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
> 4, 4, 4, 4, 4 ] ),
> Transformation( [ 2, 3, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
> 1 ] ),
> Transformation( [ 1, 2, 3, 4, 6, 5, 7, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
> 4 ] ),
> Transformation( [ 1, 2, 3, 4, 6, 7, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
> 4 ] ),
> Transformation( [ 1, 2, 3, 4, 5, 6, 7, 9, 8, 10, 11, 4, 4, 4, 4, 4, 4, 4, 4, 
> 4, 4 ] ),
> Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 10, 4, 4, 4, 4, 4, 4, 4, 4, 
> 4, 4 ] ),
> Transformation( [ 1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 13, 12, 14, 15, 16, 17, 
> 18, 19, 20, 21 ] ),
> Transformation( [ 1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 13, 14, 15, 16, 12, 17, 
> 18, 19, 20, 21 ] ),
> Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18,
> 19, 20, 21, 17 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
639
gap> NrRClasses(s);
5
gap> NrDClasses(s);
5
gap> NrLClasses(s);
5
gap> NrHClasses(s);
5
gap> NrIdempotents(s);
5
gap> NrRegularDClasses(s);
5
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
IdentityTransformation
gap> One(s);
IdentityTransformation
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
24
gap> IsBlockGroup(s);
true
gap> IsCliffordSemigroup(s);
true
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
true
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
true
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
true
gap> IsOrthodoxSemigroup(s);
true
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest27
gap> gens:=[Transformation( [ 2, 1, 1, 2, 1 ] ),
> Transformation( [ 3, 4, 3, 4, 4 ] ),
> Transformation( [ 3, 4, 3, 4, 3 ] ),
> Transformation( [ 4, 3, 3, 4, 4 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
16
gap> NrRClasses(s);
4
gap> NrDClasses(s);
1
gap> NrLClasses(s);
2
gap> NrHClasses(s);
8
gap> NrIdempotents(s);
8
gap> NrRegularDClasses(s);
1
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
16
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
true
gap> IsCompletelySimpleSemigroup(s);
true
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
true
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
true
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest28
gap> gens:=[Transformation( [ 4, 4, 4, 1, 1, 6, 7, 8, 9, 10, 11, 1 ] ),
> Transformation( [ 6, 6, 6, 7, 7, 1, 4, 8, 9, 10, 11, 7 ] ),
> Transformation( [ 8, 8, 8, 9, 9, 10, 11, 1, 4, 6, 7, 9 ] ),
> Transformation( [ 2, 2, 2, 4, 4, 6, 7, 8, 9, 10, 11, 4 ] ),
> Transformation( [ 1, 1, 1, 5, 5, 6, 7, 8, 9, 10, 11, 5 ] ),
> Transformation( [ 1, 1, 4, 4, 4, 6, 7, 8, 9, 10, 11, 1 ] ),
> Transformation( [ 1, 1, 7, 4, 4, 6, 7, 8, 9, 10, 11, 6 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
1152
gap> NrRClasses(s);
3
gap> NrDClasses(s);
1
gap> NrLClasses(s);
3
gap> NrHClasses(s);
9
gap> NrIdempotents(s);
9
gap> NrRegularDClasses(s);
1
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
1152
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
true
gap> IsCompletelySimpleSemigroup(s);
true
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
true
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
true
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest29
gap> gens:=[Transformation( [ 1, 2, 2, 1, 2 ] ),
> Transformation( [ 3, 4, 3, 4, 4 ] ),
> Transformation( [ 3, 4, 3, 4, 3 ] ),
> Transformation( [ 4, 3, 3, 4, 4 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
16
gap> NrRClasses(s);
4
gap> NrDClasses(s);
1
gap> NrLClasses(s);
2
gap> NrHClasses(s);
8
gap> NrIdempotents(s);
8
gap> NrRegularDClasses(s);
1
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
fail
gap> One(s);
fail
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
16
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
true
gap> IsCompletelySimpleSemigroup(s);
true
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
false
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
true
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
true
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest30
gap> gens:=[Transformation( [ 2, 6, 1, 7, 5, 3, 4 ] ),
> Transformation( [ 5, 3, 7, 2, 1, 6, 4 ] ),
> Transformation( [ 2, 5, 5, 3, 4, 2, 3 ] ),
> Transformation( [ 1, 5, 1, 6, 1, 5, 6 ] ),
> Transformation( [ 6, 2, 2, 2, 5, 1, 2 ] ),
> Transformation( [ 7, 5, 4, 4, 4, 5, 5 ] ),
> Transformation( [ 5, 1, 6, 1, 1, 5, 1 ] ),
> Transformation( [ 3, 5, 2, 3, 2, 2, 3 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
21343
gap> NrRClasses(s);
401
gap> NrDClasses(s);
7
gap> NrLClasses(s);
99
gap> NrHClasses(s);
4418
gap> NrIdempotents(s);
1471
gap> NrRegularDClasses(s);
6
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
IdentityTransformation
gap> One(s);
IdentityTransformation
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
7
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
true
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest31
gap> gens:=[Transformation( [ 3, 6, 9, 1, 4, 7, 2, 5, 8 ] ),
> Transformation( [ 3, 6, 9, 7, 1, 4, 5, 8, 2 ] ),
> Transformation( [ 8, 2, 5, 5, 4, 5, 5, 2, 8 ] ),
> Transformation( [ 4, 4, 8, 4, 4, 2, 4, 4, 5 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
82953
gap> NrRClasses(s);
503
gap> NrDClasses(s);
7
gap> NrLClasses(s);
214
gap> NrHClasses(s);
16426
gap> NrIdempotents(s);
3718
gap> NrRegularDClasses(s);
6
gap> MultiplicativeZero(s);
fail
gap> MultiplicativeNeutralElement(s);
IdentityTransformation
gap> One(s);
IdentityTransformation
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
9
gap> IsBlockGroup(s);
false
gap> IsCliffordSemigroup(s);
false
gap> IsCommutative(s);
false
gap> IsCompletelyRegularSemigroup(s);
false
gap> IsCompletelySimpleSemigroup(s);
false
gap> IsHTrivial(s);
false
gap> IsLTrivial(s);
false
gap> IsRTrivial(s);
false
gap> IsGroupAsSemigroup(s);
false
gap> IsInverseSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsMonoidAsSemigroup(s);
true
gap> IsOrthodoxSemigroup(s);
false
gap> IsRectangularBand(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> IsSemiband(s);
false
gap> IsSemilattice(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#T# ExamplesTest32: ZeroSemigroup
gap> s := ZeroSemigroup(0);
Error, Semigroups: ZeroSemigroup: usage,
the argument <n> must be a positive integer,
gap> s := ZeroSemigroup(IsPartialPermSemigroup, 0);
Error, Semigroups: ZeroSemigroup: usage,
the argument <n> must be a positive integer,
gap> s := ZeroSemigroup(0, 1);
Error, Semigroups: ZeroSemigroup: usage,
the optional first argument <filter> must be a filter,
gap> s := ZeroSemigroup(0, 0);
Error, Semigroups: ZeroSemigroup: usage,
the optional first argument <filter> must be a filter,
gap> s := ZeroSemigroup(IsPermGroup, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `ZeroSemigroupCons' on 2 arguments
gap> s := ZeroSemigroup(IsPartialPermSemigroup, 2, true);
Error, Semigroups: ZeroSemigroup: usage,
this function takes at most two arguments,

# IsTransformationSemigroup
gap> s := Semigroup(ZeroSemigroup(IsTransformationSemigroup, 1));
<trivial transformation group of degree 0 with 1 generator>
gap> IsZeroSemigroup(s);
true
gap> Size(s);
1
gap> Elements(s);
[ IdentityTransformation ]
gap> s := Semigroup(ZeroSemigroup(IsTransformationSemigroup, 2));
<commutative transformation semigroup of degree 3 with 1 generator>
gap> IsZeroSemigroup(s);
true
gap> Size(s);
2
gap> Elements(s);
[ Transformation( [ 1, 1, 1 ] ), Transformation( [ 1, 3, 1 ] ) ]
gap> s := Semigroup(ZeroSemigroup(IsTransformationSemigroup, 10));
<transformation semigroup of degree 19 with 9 generators>
gap> IsZeroSemigroup(s);
true
gap> Size(s);
10
gap> Elements(s);
[ Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1 ] ), Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     1, 19, 1 ] ), Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     1, 17, 1, 1, 1 ] ), Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     1, 15, 1, 1, 1, 1, 1 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 13, 1, 1, 1, 1, 1, 1,
      1 ] ), Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 11, 1, 1, 1, 1, 1,
      1, 1, 1, 1 ] ), Transformation( [ 1, 1, 1, 1, 1, 1, 1, 9, 1, 1, 1, 1, 1,
     1, 1, 1, 1, 1, 1 ] ), Transformation( [ 1, 1, 1, 1, 1, 7, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1 ] ), 
  Transformation( [ 1, 1, 1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1 ] ), Transformation( [ 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     1, 1, 1 ] ) ]

# IsReesZeroMatrixSemigroup
gap> s := ZeroSemigroup(IsReesZeroMatrixSemigroup, 1);
Error, Semigroups: ZeroSemigroup: usage,
there is no Rees 0-matrix semigroup with only 1 element,
gap> s := ZeroSemigroupCons(IsReesZeroMatrixSemigroup, 1);
Error, Semigroups: ZeroSemigroupCons: usage,
there is no Rees 0-matrix semigroup with only 1 element,
gap> s := Semigroup(ZeroSemigroupCons(IsReesZeroMatrixSemigroup, 2));;
gap> IsReesZeroMatrixSemigroup(s);
true
gap> s;
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> IsZeroSemigroup(s);
true
gap> Size(s);
2
gap> s := Semigroup(ZeroSemigroupCons(IsReesZeroMatrixSemigroup, 20));;
gap> IsReesZeroMatrixSemigroup(s);
true
gap> s;
<Rees 0-matrix semigroup 19x1 over Group(())>
gap> IsZeroSemigroup(s);
true
gap> Size(s);
20

# IsBipartitionSemigroup and IsBlockBijectionSemigroup
gap> s := ZeroSemigroup(IsBipartitionSemigroup, 1);
<trivial bipartition group of degree 1 with 1 generator>
gap> s := ZeroSemigroup(IsBlockBijectionSemigroup, 1);
<trivial bipartition group of degree 1 with 1 generator>
gap> last = last2;
true
gap> s := Semigroup(ZeroSemigroupCons(IsBipartitionSemigroup, 2));
<commutative bipartition semigroup of degree 2 with 1 generator>
gap> IsBlockBijectionSemigroup(s);
false
gap> IsZeroSemigroup(s);
true
gap> Size(s);
2
gap> s := Semigroup(ZeroSemigroupCons(IsBlockBijectionSemigroup, 2));
<commutative bipartition semigroup of degree 3 with 1 generator>
gap> IsBlockBijectionSemigroup(s);
true
gap> IsZeroSemigroup(s);
true
gap> Size(s);
2
gap> s := Semigroup(ZeroSemigroupCons(IsBipartitionSemigroup, 20));
<bipartition semigroup of degree 38 with 19 generators>
gap> IsBlockBijectionSemigroup(s);
true
gap> IsZeroSemigroup(s);
true
gap> Size(s);
20
gap> s := Semigroup(ZeroSemigroupCons(IsBlockBijectionSemigroup, 20));
<bipartition semigroup of degree 38 with 19 generators>
gap> IsBlockBijectionSemigroup(s);
true
gap> IsZeroSemigroup(s);
true
gap> Size(s);
20

# Zero semigroup of order 1
gap> s := ZeroSemigroup(1);
<trivial partial perm group of rank 0 with 1 generator>
gap> GeneratorsOfSemigroup(s);
[ <empty partial perm> ]
gap> HasAsList(s);
true
gap> AsList(s) = GeneratorsOfSemigroup(s);
true
gap> HasMultiplicativeZero(s);
true
gap> MultiplicativeZero(s);
<empty partial perm>
gap> HasIsMonogenicSemigroup(s);
true
gap> IsMonogenicSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
true
gap> IsRegularSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
true
gap> IsGroup(s);
true

# Zero semigroup of order 2
gap> s := ZeroSemigroup(2);
<commutative non-regular partial perm semigroup of size 2, rank 1 with 1 
 generator>
gap> HasIsZeroSemigroup(s);
true
gap> IsZeroSemigroup(s);
true
gap> HasIsLeftZeroSemigroup(s);
false
gap> HasIsRightZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> HasAsList(s);
true
gap> AsList(s);
[ [1,2], <empty partial perm> ]
gap> Elements(s);
[ <empty partial perm>, [1,2] ]
gap> HasMultiplicativeZero(s);
true
gap> MultiplicativeZero(s);
<empty partial perm>
gap> HasIsMonogenicSemigroup(s);
true
gap> IsMonogenicSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
true
gap> IsRegularSemigroup(s);
false
gap> HasIsGroupAsSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false

# Not created by ZeroSemigroup
gap> s := Semigroup(s);;
gap> HasIsZeroSemigroup(s);
false
gap> IsZeroSemigroup(s);
true
gap> HasIsLeftZeroSemigroup(s);
false
gap> HasIsRightZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> HasAsList(s);
false
gap> AsList(s);
[ [1,2], <empty partial perm> ]
gap> Elements(s);
[ <empty partial perm>, [1,2] ]
gap> s := Semigroup(s);;
gap> HasMultiplicativeZero(s);
false
gap> MultiplicativeZero(s);
<empty partial perm>
gap> HasIsMonogenicSemigroup(s);
true
gap> IsMonogenicSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
false
gap> IsRegularSemigroup(s);
false
gap> HasIsGroupAsSemigroup(s);
false
gap> IsGroupAsSemigroup(s);
false

# Zero semigroup of order 50
gap> s := ZeroSemigroup(50);
<non-regular partial perm semigroup of size 50, rank 49 with 49 generators>
gap> HasIsZeroSemigroup(s);
true
gap> IsZeroSemigroup(s);
true
gap> HasIsLeftZeroSemigroup(s);
false
gap> HasIsRightZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> HasAsList(s);
true
gap> IsSubset(AsList(s), Elements(s)) and IsSubset(Elements(s), AsList(s));
true
gap> HasMultiplicativeZero(s);
true
gap> MultiplicativeZero(s);
<empty partial perm>
gap> HasIsMonogenicSemigroup(s);
true
gap> IsMonogenicSemigroup(s);
false
gap> HasIsRegularSemigroup(s);
true
gap> IsRegularSemigroup(s);
false
gap> HasIsGroupAsSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false

# Not created by ZeroSemigroup
gap> s := Semigroup(s);;
gap> HasIsZeroSemigroup(s);
false
gap> IsZeroSemigroup(s);
true
gap> HasIsLeftZeroSemigroup(s);
false
gap> HasIsRightZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> HasAsList(s);
false
gap> IsSubset(AsList(s), Elements(s)) and IsSubset(Elements(s), AsList(s));
true
gap> s := Semigroup(s);;
gap> HasMultiplicativeZero(s);
false
gap> MultiplicativeZero(s);
<empty partial perm>
gap> HasIsMonogenicSemigroup(s);
false
gap> IsMonogenicSemigroup(s);
false
gap> HasIsRegularSemigroup(s);
false
gap> IsRegularSemigroup(s);
false
gap> HasIsGroupAsSemigroup(s);
false
gap> IsGroupAsSemigroup(s);
false

#T# ExamplesTest33: MonogenicSemigroup
gap> s := MonogenicSemigroup(0, 1);
Error, Semigroups: MonogenicSemigroup: usage,
the arguments <m> and <r> must be positive integers,
gap> s := MonogenicSemigroup(1, 0);
Error, Semigroups: MonogenicSemigroup: usage,
the arguments <m> and <r> must be positive integers,
gap> s := MonogenicSemigroup(1, 1, 1);
Error, Semigroups: MonogenicSemigroup: usage,
the optional first argument <filter> must be a filter,
gap> s := MonogenicSemigroup(IsPermGroup, 1, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `MonogenicSemigroupCons' on 3 arguments
gap> s := MonogenicSemigroup(IsTransformationSemigroup, 1, 1);
<trivial transformation group of degree 0 with 1 generator>
gap> s := MonogenicSemigroup(1, 1, 1, 1);
Error, Semigroups: MonogenicSemigroup: usage,
this function takes either two or three arguments,
gap> s := MonogenicSemigroup(1);
Error, Semigroups: MonogenicSemigroup: usage,
this function takes either two or three arguments,

# Trivial monogenic semigroup
gap> s := MonogenicSemigroup(1, 1);
<trivial transformation group of degree 0 with 1 generator>
gap> HasSize(s);
true
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> Size(s);
1
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
true
gap> IsGroup(s);
true
gap> IsRegularSemigroup(s);
true
gap> IsZeroSemigroup(s);
true
gap> GeneratorsOfSemigroup(s);
[ IdentityTransformation ]

# Not created by MonogenicSemigroup
gap> s := Semigroup(s);;
gap> HasSize(s);
false
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> Size(s);
1
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
true
gap> IsGroup(s);
true
gap> IsRegularSemigroup(s);
true
gap> IsZeroSemigroup(s);
true
gap> GeneratorsOfSemigroup(s);
[ IdentityTransformation ]
gap> IndexPeriodOfTransformation(last[1]);
[ 1, 1 ]

# MonogenicSemigroup(2, 1)
gap> s := MonogenicSemigroup(2, 1);
<commutative non-regular transformation semigroup of size 2, degree 3 with 1 
 generator>
gap> HasSize(s);
true
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> Size(s);
2
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsZeroSemigroup(s);
true
gap> GeneratorsOfSemigroup(s);
[ Transformation( [ 1, 1, 2 ] ) ]

# Not created by MonogenicSemigroup
gap> s := Semigroup(s);;
gap> HasSize(s);
false
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
false
gap> HasIsRegularSemigroup(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> Size(s);
2
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsZeroSemigroup(s);
true
gap> GeneratorsOfSemigroup(s);
[ Transformation( [ 1, 1, 2 ] ) ]
gap> IndexPeriodOfTransformation(last[1]);
[ 2, 1 ]

# MonogenicSemigroup(3, 1)
gap> s := MonogenicSemigroup(3, 1);
<commutative non-regular transformation semigroup of size 3, degree 4 with 1 
 generator>
gap> HasSize(s);
true
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> Size(s);
3
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsZeroSemigroup(s);
false
gap> GeneratorsOfSemigroup(s);
[ Transformation( [ 1, 1, 2, 3 ] ) ]

# Not created by MonogenicSemigroup
gap> s := Semigroup(s);;
gap> HasSize(s);
false
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
false
gap> HasIsRegularSemigroup(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> Size(s);
3
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsZeroSemigroup(s);
false
gap> GeneratorsOfSemigroup(s);
[ Transformation( [ 1, 1, 2, 3 ] ) ]
gap> IndexPeriodOfTransformation(last[1]);
[ 3, 1 ]

# MonogenicSemigroup(1, 2)
gap> s := MonogenicSemigroup(1, 2);
<transformation group of size 2, degree 2 with 1 generator>
gap> HasSize(s);
true
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> Size(s);
2
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
true
gap> IsRegularSemigroup(s);
true
gap> IsZeroSemigroup(s);
false
gap> GeneratorsOfSemigroup(s);
[ Transformation( [ 2, 1 ] ) ]

# Not created by MonogenicSemigroup
gap> s := Semigroup(s);
<commutative transformation semigroup of degree 2 with 1 generator>
gap> HasSize(s);
false
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
false
gap> HasIsRegularSemigroup(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> Size(s);
2
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
true
gap> IsRegularSemigroup(s);
true
gap> IsZeroSemigroup(s);
false
gap> GeneratorsOfSemigroup(s);
[ Transformation( [ 2, 1 ] ) ]
gap> IndexPeriodOfTransformation(last[1]);
[ 1, 2 ]

# MonogenicSemigroup(5, 10)
gap> s := MonogenicSemigroup(5, 10);
<commutative non-regular transformation semigroup of size 14, degree 15 with 
 1 generator>
gap> HasSize(s);
true
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> Size(s);
14
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsZeroSemigroup(s);
false
gap> GeneratorsOfSemigroup(s);
[ Transformation( [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 10, 11, 12, 13, 14 ] ) ]

# Not created by MonogenicSemigroup
gap> s := Semigroup(s);
<commutative transformation semigroup of degree 15 with 1 generator>
gap> HasSize(s);
false
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
false
gap> HasIsRegularSemigroup(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> Size(s);
14
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsZeroSemigroup(s);
false
gap> GeneratorsOfSemigroup(s);
[ Transformation( [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 10, 11, 12, 13, 14 ] ) ]
gap> IndexPeriodOfTransformation(last[1]);
[ 5, 10 ]

# Other categories of semigroup

# partial perm
gap> s := MonogenicSemigroup(IsPartialPermSemigroup, 1, 1);
<trivial partial perm group of rank 0 with 1 generator>
gap> IndexPeriodOfTransformation(GeneratorsOfSemigroup(
> AsTransformationSemigroup(s))[1]);
[ 1, 1 ]
gap> s := MonogenicSemigroup(IsPartialPermSemigroup, 2, 1);
<commutative non-regular partial perm semigroup of size 2, rank 1 with 1 
 generator>
gap> IndexPeriodOfTransformation(GeneratorsOfSemigroup(
> AsTransformationSemigroup(s))[1]);
[ 2, 1 ]
gap> s := MonogenicSemigroup(IsPartialPermSemigroup, 2, 2);
<commutative non-regular partial perm semigroup of size 3, rank 3 with 1 
 generator>
gap> IndexPeriodOfTransformation(GeneratorsOfSemigroup(
> AsTransformationSemigroup(s))[1]);
[ 2, 2 ]

# bipartition and block bijection
gap> s := MonogenicSemigroup(IsBipartitionSemigroup, 1, 1);
<trivial bipartition group of degree 1 with 1 generator>
gap> IndexPeriodOfTransformation(GeneratorsOfSemigroup(
> AsTransformationSemigroup(s))[1]);
[ 1, 1 ]
gap> s := MonogenicSemigroup(IsBlockBijectionSemigroup, 2, 1);
<commutative non-regular bipartition semigroup of size 2, degree 3 with 1 
 generator>
gap> IndexPeriodOfTransformation(GeneratorsOfSemigroup(
> AsTransformationSemigroup(s))[1]);
[ 2, 1 ]
gap> s := MonogenicSemigroup(IsBlockBijectionSemigroup, 2, 2);
<commutative non-regular bipartition semigroup of size 3, degree 5 with 1 
 generator>
gap> IndexPeriodOfTransformation(GeneratorsOfSemigroup(
> AsTransformationSemigroup(s))[1]);
[ 2, 2 ]
gap> s := MonogenicSemigroup(IsBlockBijectionSemigroup, 5, 6);
<commutative non-regular bipartition semigroup of size 10, degree 12 with 1 
 generator>
gap> IndexPeriodOfTransformation(GeneratorsOfSemigroup(
> AsTransformationSemigroup(s))[1]);
[ 5, 6 ]

#T# ExamplesTest34: RectangularBand
gap> s := RectangularBand(0, 1);
Error, Semigroups: RectangularBand: usage,
the arguments <m> and <n> must be positive integers,
gap> s := RectangularBand(1, 0);
Error, Semigroups: RectangularBand: usage,
the arguments <m> and <n> must be positive integers,
gap> s := RectangularBand(1);
Error, Semigroups: RectangularBand: usage,
this function takes either two or three arguments,
gap> s := RectangularBand(1, 1, 1, 1);
Error, Semigroups: RectangularBand: usage,
this function takes either two or three arguments,
gap> s := RectangularBand(1, 1, 1);
Error, Semigroups: RectangularBand: usage,
the optional first argument <filter> must be a filter,
gap> s := RectangularBand(IsPermGroup, 1, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBandCons' on 3 arguments
gap> s := RectangularBand(IsReesMatrixSemigroup, 1, 1);;

# Trivial rectangular band
gap> s := RectangularBand(1, 1);
<Rees matrix semigroup 1x1 over Group(())>
gap> HasIsRectangularBand(s);
true
gap> HasIsBand(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> HasSize(s);
true
gap> HasIsTrivial(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> HasIsLeftZeroSemigroup(s);
true
gap> HasIsRightZeroSemigroup(s);
true
gap> IsRectangularBand(s);
true
gap> IsBand(s);
true
gap> IsZeroSemigroup(s);
true
gap> Size(s);
1
gap> IsTrivial(s);
true
gap> IsZeroSemigroup(s);
true
gap> IsLeftZeroSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
true

# Not created by RectangularBand
gap> s := AsTransformationSemigroup(s);
<commutative transformation semigroup of degree 2 with 1 generator>
gap> HasIsRectangularBand(s);
false
gap> HasIsBand(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> HasSize(s);
false
gap> HasIsTrivial(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> HasIsLeftZeroSemigroup(s);
false
gap> HasIsRightZeroSemigroup(s);
false
gap> IsRectangularBand(s);
true
gap> IsBand(s);
true
gap> IsZeroSemigroup(s);
true
gap> Size(s);
1
gap> IsTrivial(s);
true
gap> IsZeroSemigroup(s);
true
gap> IsLeftZeroSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
true

# 10 x 1 rectangular band
gap> s := RectangularBand(10, 1);
<Rees matrix semigroup 10x1 over Group(())>
gap> HasIsRectangularBand(s);
true
gap> HasIsBand(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> HasSize(s);
true
gap> HasIsTrivial(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> HasIsLeftZeroSemigroup(s);
true
gap> HasIsRightZeroSemigroup(s);
true
gap> IsRectangularBand(s);
true
gap> IsBand(s);
true
gap> IsZeroSemigroup(s);
false
gap> Size(s);
10
gap> IsTrivial(s);
false
gap> IsZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false

# Not created by RectangularBand
gap> s := AsTransformationSemigroup(s);
<transformation semigroup of degree 11 with 10 generators>
gap> HasIsRectangularBand(s);
false
gap> HasIsBand(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> HasSize(s);
false
gap> HasIsTrivial(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> HasIsLeftZeroSemigroup(s);
false
gap> HasIsRightZeroSemigroup(s);
false
gap> IsRectangularBand(s);
true
gap> IsBand(s);
true
gap> IsZeroSemigroup(s);
false
gap> Size(s);
10
gap> IsTrivial(s);
false
gap> IsZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false

# 1 x 8 rectangular band
gap> s := RectangularBand(1, 8);
<Rees matrix semigroup 1x8 over Group(())>
gap> HasIsRectangularBand(s);
true
gap> HasIsBand(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> HasSize(s);
true
gap> HasIsTrivial(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> HasIsLeftZeroSemigroup(s);
true
gap> HasIsRightZeroSemigroup(s);
true
gap> IsRectangularBand(s);
true
gap> IsBand(s);
true
gap> IsZeroSemigroup(s);
false
gap> Size(s);
8
gap> IsTrivial(s);
false
gap> IsZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
true

# Not created by RectangularBand
gap> s := AsTransformationSemigroup(s);
<transformation semigroup of degree 9 with 8 generators>
gap> HasIsRectangularBand(s);
false
gap> HasIsBand(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> HasSize(s);
false
gap> HasIsTrivial(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> HasIsLeftZeroSemigroup(s);
false
gap> HasIsRightZeroSemigroup(s);
false
gap> IsRectangularBand(s);
true
gap> IsBand(s);
true
gap> IsZeroSemigroup(s);
false
gap> Size(s);
8
gap> IsTrivial(s);
false
gap> IsZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
true

# 12 x 7 rectangular band
gap> s := RectangularBand(12, 7);
<Rees matrix semigroup 12x7 over Group(())>
gap> HasIsRectangularBand(s);
true
gap> HasIsBand(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> HasSize(s);
true
gap> HasIsTrivial(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> HasIsLeftZeroSemigroup(s);
true
gap> HasIsRightZeroSemigroup(s);
true
gap> IsRectangularBand(s);
true
gap> IsBand(s);
true
gap> IsZeroSemigroup(s);
false
gap> Size(s);
84
gap> IsTrivial(s);
false
gap> IsZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false

# Not created by RectangularBand
gap> s := AsTransformationSemigroup(s);
<transformation semigroup of degree 85 with 84 generators>
gap> HasIsRectangularBand(s);
false
gap> HasIsBand(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> HasSize(s);
false
gap> HasIsTrivial(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> HasIsLeftZeroSemigroup(s);
false
gap> HasIsRightZeroSemigroup(s);
false
gap> IsRectangularBand(s);
true
gap> IsBand(s);
true
gap> IsZeroSemigroup(s);
false
gap> Size(s);
84
gap> IsTrivial(s);
false
gap> IsZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false

# Other categories of semigroup

# transformation semigroup
gap> s := RectangularBand(IsTransformationSemigroup, 1, 1);
<trivial transformation group of degree 0 with 1 generator>
gap> IsHTrivial(s) and IsSimpleSemigroup(s);
true
gap> [NrRClasses(s), NrLClasses(s)] = [1, 1];
true
gap> s := RectangularBand(IsTransformationSemigroup, 10, 10);
<transformation semigroup of size 100, degree 101 with 10 generators>
gap> IsHTrivial(s) and IsSimpleSemigroup(s);
true
gap> [NrRClasses(s), NrLClasses(s)] = [10, 10];
true
gap> s := RectangularBand(IsTransformationSemigroup, 1, 12);
<transformation semigroup of size 12, degree 12 with 12 generators>
gap> IsHTrivial(s) and IsSimpleSemigroup(s);
true
gap> [NrRClasses(s), NrLClasses(s)] = [1, 12];
true
gap> s := RectangularBand(IsTransformationSemigroup, 10, 20);
<transformation semigroup of size 200, degree 201 with 20 generators>
gap> IsHTrivial(s) and IsSimpleSemigroup(s);
true
gap> [NrRClasses(s), NrLClasses(s)] = [10, 20];
true
gap> s := RectangularBand(IsTransformationSemigroup, 20, 10);
<transformation semigroup of size 200, degree 201 with 20 generators>
gap> IsHTrivial(s) and IsSimpleSemigroup(s);
true
gap> [NrRClasses(s), NrLClasses(s)] = [20, 10];
true

# bipartition
gap> s := RectangularBand(IsBipartitionSemigroup, 1, 1);
<trivial bipartition group of degree 1 with 1 generator>
gap> IsHTrivial(s) and IsSimpleSemigroup(s);
true
gap> [NrRClasses(s), NrLClasses(s)] = [1, 1];
true
gap> s := RectangularBand(IsBipartitionSemigroup, 10, 10);
<bipartition semigroup of size 100, degree 4 with 10 generators>
gap> IsHTrivial(s) and IsSimpleSemigroup(s);
true
gap> [NrRClasses(s), NrLClasses(s)] = [10, 10];
true
gap> s := RectangularBand(IsBipartitionSemigroup, 1, 12);
<bipartition semigroup of size 12, degree 4 with 12 generators>
gap> IsHTrivial(s) and IsSimpleSemigroup(s);
true
gap> [NrRClasses(s), NrLClasses(s)] = [1, 12];
true
gap> s := RectangularBand(IsBipartitionSemigroup, 10, 20);
<bipartition semigroup of size 200, degree 5 with 20 generators>
gap> IsHTrivial(s) and IsSimpleSemigroup(s);
true
gap> [NrRClasses(s), NrLClasses(s)] = [10, 20];
true
gap> s := RectangularBand(IsBipartitionSemigroup, 20, 10);
<bipartition semigroup of size 200, degree 5 with 20 generators>
gap> IsHTrivial(s) and IsSimpleSemigroup(s);
true
gap> [NrRClasses(s), NrLClasses(s)] = [20, 10];
true

#T# ExamplesTest35: LeftZeroSemigroup
gap> s := LeftZeroSemigroup(0);
Error, Semigroups: LeftZeroSemigroup: usage,
the argument <n> must be positive a integer,
gap> s := LeftZeroSemigroup(1, 1);
Error, Semigroups: LeftZeroSemigroup: usage,
the optional first argument <filter> must be a filter,
gap> s := LeftZeroSemigroup(IsPermGroup, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBandCons' on 3 arguments
gap> s := LeftZeroSemigroup(IsTransformationSemigroup, 1);
<trivial transformation group of degree 0 with 1 generator>
gap> s := LeftZeroSemigroup(1, 1, 1);
Error, Semigroups: LeftZeroSemigroup: usage,
this function takes at most two arguments,

#T# ExamplesTest36: RightZeroSemigroup
gap> s := RightZeroSemigroup(0);
Error, Semigroups: RightZeroSemigroup: usage,
the argument <n> must be positive a integer,
gap> s := RightZeroSemigroup(1, 1);
Error, Semigroups: RightZeroSemigroup: usage,
the optional first argument <filter> must be a filter,
gap> s := RightZeroSemigroup(IsPermGroup, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBandCons' on 3 arguments
gap> s := RightZeroSemigroup(IsTransformationSemigroup, 1);
<trivial transformation group of degree 0 with 1 generator>
gap> s := RightZeroSemigroup(1, 1, 1);
Error, Semigroups: RightZeroSemigroup: usage,
this function takes at most two arguments,

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(s);
gap> Unbind(gens);

#E# 
gap> STOP_TEST("Semigroups package: examples.tst");
