#############################################################################
##
#W  examples.tst
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################


gap> START_TEST("Semigroups package: examples.tst");
gap> LoadPackage("semigroups", false);;

#
gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoSemigroups:=InfoLevel(InfoSemigroups);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoSemigroups, 0);

#
gap> 
Example 1
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
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 2
gap> gens:=[Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
> Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
> Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
> Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 3
gap> gens:=[Transformation( [ 2, 6, 7, 2, 6, 1, 1, 5 ] ),
> Transformation( [ 3, 8, 1, 4, 5, 6, 7, 1 ] ),
> Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] ),
> Transformation( [ 7, 1, 7, 4, 2, 5, 6, 3 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 4
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
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 5
gap> gens:=[Transformation( [ 3, 4, 1, 2, 1 ] ),
> Transformation( [ 4, 2, 1, 5, 5 ] ),
> Transformation( [ 4, 2, 2, 2, 4 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 6
gap> gens:=[Transformation( [ 1, 3, 4, 1 ] ),
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ),
> Transformation( [ 3, 3, 4, 1 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 7
gap> gens:=[Transformation( [ 1, 3, 2, 3 ] ),
> Transformation( [ 1, 4, 1, 2 ] ),
> Transformation( [ 2, 4, 1, 1 ] ),
> Transformation( [ 3, 4, 2, 2 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 8
gap> gens:=[Transformation( [ 1, 3, 2, 3 ] ),
> Transformation( [ 1, 4, 1, 2 ] ),
> Transformation( [ 3, 4, 2, 2 ] ),
> Transformation( [ 4, 1, 2, 1 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 10
gap> gens:=[Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] ),
> Transformation( [ 2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 11
gap> gens:=[Transformation( [ 2, 1, 4, 5, 3, 7, 8, 9, 10, 6 ] ),
> Transformation( [ 1, 2, 4, 3, 5, 6, 7, 8, 9, 10 ] ),
> Transformation( [ 1, 2, 3, 4, 5, 6, 10, 9, 8, 7 ] ),
> Transformation( [ 9, 1, 4, 3, 6, 9, 3, 4, 3, 9 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 12
gap> gens:=[Transformation( [ 13, 10, 9, 5, 1, 5, 13, 13, 8, 2, 7, 2, 6 ] ),
> Transformation( [ 6, 11, 12, 10, 4, 10, 13, 5, 8, 5, 11, 6, 9 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 13
gap> gens:=[Transformation( [ 12, 10, 8, 5, 1, 5, 12, 12, 8, 2, 6, 2 ] ),
> Transformation( [ 5, 6, 10, 11, 10, 4, 10, 12, 5, 7, 4, 10 ] ),
> Transformation( [ 6, 8, 12, 5, 4, 8, 10, 7, 4, 1, 10, 11 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 14
gap> gens:=[Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] ),
> Transformation( [ 5, 4, 1, 2, 3, 7, 6, 5, 4, 1 ] ),
> Transformation( [ 2, 1, 4, 3, 2, 1, 4, 4, 3, 3 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 15
gap> gens:=[Transformation( [ 8, 7, 5, 3, 1, 3, 8, 8 ] ),
> Transformation( [ 5, 1, 4, 1, 4, 4, 7, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 16
gap> gens:=[Transformation( [ 5, 4, 4, 2, 1 ] ),
> Transformation( [ 2, 5, 5, 4, 1 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 17
gap> gens:=[Transformation( [ 1, 2, 1, 3, 3 ] ),
> Transformation( [ 2, 2, 3, 5, 5 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 18
gap> gens:=[Transformation( [ 3, 1, 2, 3, 2, 3, 2, 3 ] ),
> Transformation( [ 2, 5, 8, 5, 2, 5, 7, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 19
gap> gens:=[Transformation( [ 3, 3, 2, 6, 2, 4, 4, 6 ] ),
> Transformation( [ 5, 1, 7, 8, 7, 5, 8, 1 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 20
gap> gens:=[Transformation( [ 10, 8, 7, 4, 1, 4, 10, 10, 7, 2 ] ),
> Transformation( [ 5, 2, 5, 5, 9, 10, 8, 3, 8, 10 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 21
gap> gens:=[Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] ),
> Transformation( [ 2, 3, 4, 5, 6, 8, 7, 1, 2, 2 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 22
gap> gens:=[Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] ),
> Transformation( [ 3, 8, 7, 4, 1, 4, 3, 3, 7, 2 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 23
gap> gens:=[Transformation( [ 1, 4, 6, 2, 5, 3, 7, 8 ] ),
> Transformation( [ 6, 3, 2, 7, 5, 1, 8, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 24
gap> gens:=[Transformation( [ 5, 6, 7, 3, 1, 4, 2, 8 ] ),
> Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 25
gap> gens:=[Transformation( [ 1, 2, 4, 5, 6, 3, 7, 8 ] ),
> Transformation( [ 3, 3, 4, 5, 6, 2, 7, 8 ] ),
> Transformation( [ 1, 2, 5, 3, 6, 8, 4, 4 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 26
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
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 27
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
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 28
gap> gens:=[Transformation( [ 2, 1, 1, 2, 1 ] ),
> Transformation( [ 3, 4, 3, 4, 4 ] ),
> Transformation( [ 3, 4, 3, 4, 3 ] ),
> Transformation( [ 4, 3, 3, 4, 4 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 29
gap> gens:=[Transformation( [ 4, 4, 4, 1, 1, 6, 7, 8, 9, 10, 11, 1 ] ),
> Transformation( [ 6, 6, 6, 7, 7, 1, 4, 8, 9, 10, 11, 7 ] ),
> Transformation( [ 8, 8, 8, 9, 9, 10, 11, 1, 4, 6, 7, 9 ] ),
> Transformation( [ 2, 2, 2, 4, 4, 6, 7, 8, 9, 10, 11, 4 ] ),
> Transformation( [ 1, 1, 1, 5, 5, 6, 7, 8, 9, 10, 11, 5 ] ),
> Transformation( [ 1, 1, 4, 4, 4, 6, 7, 8, 9, 10, 11, 1 ] ),
> Transformation( [ 1, 1, 7, 4, 4, 6, 7, 8, 9, 10, 11, 6 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 30
gap> gens:=[Transformation( [ 1, 2, 2, 1, 2 ] ),
> Transformation( [ 3, 4, 3, 4, 4 ] ),
> Transformation( [ 3, 4, 3, 4, 3 ] ),
> Transformation( [ 4, 3, 3, 4, 4 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap>
Example 32
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
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> 
Example 33
gap> gens:=[Transformation( [ 3, 6, 9, 1, 4, 7, 2, 5, 8 ] ),
> Transformation( [ 3, 6, 9, 7, 1, 4, 5, 8, 2 ] ),
> Transformation( [ 8, 2, 5, 5, 4, 5, 5, 2, 8 ] ),
> Transformation( [ 4, 4, 8, 4, 4, 2, 4, 4, 5 ] )];;
gap> s:=Semigroup(gens);;
gap> Size(s);
gap> NrRClasses(s);
gap> NrDClasses(s);
gap> NrLClasses(s);
gap> NrHClasses(s);
gap> NrIdempotents(s);
gap> NrRegularDClasses(s);
gap> MultiplicativeZero(s);
gap> MultiplicativeNeutralElement(s);
gap> One(s);
gap> if GroupOfUnits(s)<>fail then StructureDescription(GroupOfUnits(s)); fi;;
gap> Size(MinimalIdeal(s));
gap> IsBlockGroup(s);
gap> IsCliffordSemigroup(s);
gap> IsCommutative(s);
gap> IsCompletelyRegularSemigroup(s);
gap> IsCompletelySimpleSemigroup(s);
gap> IsHTrivial(s);
gap> IsLTrivial(s);
gap> IsRTrivial(s);
gap> IsGroupAsSemigroup(s);
gap> IsInverseSemigroup(s);
gap> IsLeftZeroSemigroup(s);
gap> IsMonoidAsSemigroup(s);
gap> IsOrthodoxSemigroup(s);
gap> IsRectangularBand(s);
gap> IsRegularSemigroup(s);
gap> IsRightZeroSemigroup(s);
gap> IsSemiband(s);
gap> IsSemilatticeAsSemigroup(s);
gap> IsSimpleSemigroup(s);
gap> IsSynchronizingSemigroup(s);
gap> IsZeroGroup(s);
gap> IsZeroSemigroup(s);

#
gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoSemigroups, InfoLevelInfoSemigroups);;
gap> Unbind(InfoLevelInfoSemigroups);; Unbind(InfoLevelInfoWarning);;
gap> Unbind(s);; Unbind(gens);;
gap> STOP_TEST("Semigroups package: examples.tst", 0);
