#############################################################################
##
#W  examples.tst
#Y  Copyright (C) 2011-13                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
gap> START_TEST("Semigroups package: examples.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
true
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
true
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
true
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
true
gap> IsSemilatticeAsSemigroup(s);
true
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
true
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
true
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
true
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
false
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> 
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
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsSynchronizingSemigroup(s, DegreeOfTransformationSemigroup(s));
true
gap> IsZeroGroup(s);
false
gap> IsZeroSemigroup(s);
false

#
gap> SemigroupsStopTest();
gap> STOP_TEST("Semigroups package: examples.tst", 0);
