#############################################################################
##
#W  examples.tst
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################


gap> START_TEST("Citrus package: examples.tst");
gap> LoadPackage("citrus", false);;

#
gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoCitrus:=InfoLevel(InfoCitrus);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoCitrus, 0);

#
gap> Print("Example 1");
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
gap> StructureDescription(GroupOfUnits(s));
gap> MinimalIdealSize(s);
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
gap> Print("Example 2");
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
gap> StructureDescription(GroupOfUnits(s));
gap> MinimalIdealSize(s);
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
gap> Print("Example 3");
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
gap> StructureDescription(GroupOfUnits(s));
gap> MinimalIdealSize(s);
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
gap> Print("Example 4");
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
gap> StructureDescription(GroupOfUnits(s));
gap> MinimalIdealSize(s);
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
gap> Print("Example 5");
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
gap> StructureDescription(GroupOfUnits(s));
gap> MinimalIdealSize(s);
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
gap> Print("Example 6");
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
gap> StructureDescription(GroupOfUnits(s));
gap> MinimalIdealSize(s);
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
gap> Print("Example 7");
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
gap> StructureDescription(GroupOfUnits(s));
gap> MinimalIdealSize(s);
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
gap> Print("Example 8");
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
gap> StructureDescription(GroupOfUnits(s));
gap> MinimalIdealSize(s);
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

gap> gens:=[Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] ),
> Transformation( [ 2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	Size:				20167		 time: 3
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	NrRClasses:		9		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	NrDClasses:		2		 time: 5
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	NrLClasses:		2		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	NrHClasses:		9		 time: 2
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	NrIdempotents:			9		 time: 0
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	NrRegularDClasses:		2		 time: 0
gap> t:=Runtime();; out:=MultiplicativeZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tMultiplicativeZero:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	MultiplicativeZero:		fail		 time: 1
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	HasMultIdentity:		false		 time: 1
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	GroupOfUnitsStructure:		fail		 time: 0
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	MinimalIdealSize:		20160		 time: 1816
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsBlockGroup:			false		 time: 1
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsCliffordSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsCompletelyRegularSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsHTrivial:		false		 time: 1
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsInverseSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsOrthodoxSemigroup:		true		 time: 1
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsRectangularBand:		false		 time: 1
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsRegularSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemiband(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsSemiband:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsSemiband:			false		 time: 7
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsSynchronizingSemigroup:	false		 time: 1
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 10, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 10	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 2, 1, 4, 5, 3, 7, 8, 9, 10, 6 ] ),
> Transformation( [ 1, 2, 4, 3, 5, 6, 7, 8, 9, 10 ] ),
> Transformation( [ 1, 2, 3, 4, 5, 6, 10, 9, 8, 7 ] ),
> Transformation( [ 9, 1, 4, 3, 6, 9, 3, 4, 3, 9 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	Size:				491558		 time: 153
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	NrRClasses:		2072		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	NrDClasses:		12		 time: 201
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	NrLClasses:		425		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	NrHClasses:		86036		 time: 3
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	NrIdempotents:			13655		 time: 35
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	NrRegularDClasses:		9		 time: 1
gap> t:=Runtime();; out:=MultiplicativeZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tMultiplicativeZero:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	MultiplicativeZero:		fail		 time: 2
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	HasMultIdentity:		true		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	GroupOfUnitsStructure:		C2 x D10 x S3		 time: 115
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	MinimalIdealSize:		8		 time: 7
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsBlockGroup:			false		 time: 1
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsCliffordSemigroup:		false		 time: 9
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsLTrivial:		false		 time: 1
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsInverseSemigroup:		false		 time: 7
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsMonoidAsSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsOrthodoxSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsRegularSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemiband(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsSemiband:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsSemiband:			false		 time: 2559
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsSynchronizingSemigroup:	true		 time: 1
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsZeroGroup:			false		 time: 1
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 11, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 11	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 13, 10, 9, 5, 1, 5, 13, 13, 8, 2, 7, 2, 6 ] ),
> Transformation( [ 6, 11, 12, 10, 4, 10, 13, 5, 8, 5, 11, 6, 9 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	Size:				208650		 time: 1523
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	NrRClasses:		31336		 time: 1
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	NrDClasses:		3807		 time: 1135
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	NrLClasses:		18856		 time: 6
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	NrHClasses:		70693		 time: 72
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	NrIdempotents:			5857		 time: 88
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	NrRegularDClasses:		8		 time: 11
gap> t:=Runtime();; out:=MultiplicativeZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tMultiplicativeZero:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	MultiplicativeZero:		fail		 time: 2
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	HasMultIdentity:		false		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	GroupOfUnitsStructure:		fail		 time: 1
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	MinimalIdealSize:		11		 time: 8
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsBlockGroup:			false		 time: 1
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsCliffordSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsRTrivial:		false		 time: 1
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsInverseSemigroup:		false		 time: 3
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsOrthodoxSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsRegularSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemiband(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsSemiband:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsSemiband:			false		 time: 425
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsSynchronizingSemigroup:	true		 time: 1
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 12, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 12	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 12, 10, 8, 5, 1, 5, 12, 12, 8, 2, 6, 2 ] ),
> Transformation( [ 5, 6, 10, 11, 10, 4, 10, 12, 5, 7, 4, 10 ] ),
> Transformation( [ 6, 8, 12, 5, 4, 8, 10, 7, 4, 1, 10, 11 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	Size:				945560		 time: 1411
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	NrRClasses:		19658		 time: 1
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	NrDClasses:		4092		 time: 1116
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	NrLClasses:		132176		 time: 7
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	NrHClasses:		215008		 time: 36
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	NrIdempotents:			15053		 time: 95
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	NrRegularDClasses:		6		 time: 20
gap> t:=Runtime();; out:=MultiplicativeZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tMultiplicativeZero:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	MultiplicativeZero:		fail		 time: 2
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	HasMultIdentity:		false		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	GroupOfUnitsStructure:		fail		 time: 0
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	MinimalIdealSize:		10		 time: 3
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsCliffordSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsRTrivial:		false		 time: 1
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsInverseSemigroup:		false		 time: 4
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsOrthodoxSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsRegularSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemiband(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsSemiband:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsSemiband:			false		 time: 617
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsSynchronizingSemigroup:	true		 time: 1
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 13, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 13	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] ),
> Transformation( [ 5, 4, 1, 2, 3, 7, 6, 5, 4, 1 ] ),
> Transformation( [ 2, 1, 4, 3, 2, 1, 4, 4, 3, 3 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	Size:				188315		 time: 153
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	NrRClasses:		2105		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	NrDClasses:		8		 time: 339
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	NrLClasses:		37		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	NrHClasses:		15018		 time: 3
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	NrIdempotents:			5964		 time: 31
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	NrRegularDClasses:		8		 time: 0
gap> t:=Runtime();; out:=MultiplicativeZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tMultiplicativeZero:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	MultiplicativeZero:		fail		 time: 1
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	HasMultIdentity:		false		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	GroupOfUnitsStructure:		fail		 time: 0
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	MinimalIdealSize:		5		 time: 3
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsCliffordSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsInverseSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsOrthodoxSemigroup:		false		 time: 228
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsRectangularBand:		false		 time: 1
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsRegularSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsSynchronizingSemigroup:	true		 time: 1
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 14, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 14	IsZeroSemigroup:		false		 time: 1


gap> gens:=[Transformation( [ 8, 7, 5, 3, 1, 3, 8, 8 ] ),
> Transformation( [ 5, 1, 4, 1, 4, 4, 7, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	Size:				56		 time: 5
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	NrRClasses:		16		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	NrDClasses:		7		 time: 6
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	NrLClasses:		18		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	NrHClasses:		54		 time: 1
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	NrIdempotents:			16		 time: 0
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	NrRegularDClasses:		4		 time: 0
gap> t:=Runtime();; out:=MultiplicativeZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tMultiplicativeZero:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	MultiplicativeZero:		<object>		 time: 1
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	HasMultIdentity:		false		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	GroupOfUnitsStructure:		fail		 time: 1
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	MinimalIdealSize:		1		 time: 1
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsCliffordSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsHTrivial:		false		 time: 1
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsInverseSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsOrthodoxSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsRegularSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsSemilatticeAsSemigroup:	false		 time: 1
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsSynchronizingSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 15, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 15	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 5, 4, 4, 2, 1 ] ),
> Transformation( [ 2, 5, 5, 4, 1 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	Size:				12		 time: 2
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	NrRClasses:		1		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	NrDClasses:		1		 time: 2
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	NrLClasses:		1		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	NrHClasses:		1		 time: 0
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	NrIdempotents:			1		 time: 0
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	NrRegularDClasses:		1		 time: 0
gap> t:=Runtime();; out:=MultiplicativeZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tMultiplicativeZero:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	MultiplicativeZero:		fail		 time: 0
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	HasMultIdentity:		true		 time: 1
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	GroupOfUnitsStructure:		A4		 time: 4
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	MinimalIdealSize:		12		 time: 4
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsBlockGroup:			true		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsCliffordSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsCommutative:			false		 time: 1
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsCompletelyRegularSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsCompletelySimpleSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsGroupAsSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsInverseSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsMonoidAsSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsOrthodoxSemigroup:		true		 time: 1
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsRegularSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsSimpleSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsSynchronizingSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 16, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 16	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 1, 2, 1, 3, 3 ] ),
> Transformation( [ 2, 2, 3, 5, 5 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	Size:				8		 time: 4
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	NrRClasses:		8		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	NrDClasses:		8		 time: 4
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	NrLClasses:		8		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	NrHClasses:		8		 time: 0
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	NrIdempotents:			3		 time: 0
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	NrRegularDClasses:		3		 time: 0
gap> t:=Runtime();; out:=HasMultZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tHasMultZero:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	HasMultZero:			true		 time: 0
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	HasMultIdentity:		false		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	GroupOfUnitsStructure:		fail		 time: 0
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	MinimalIdealSize:		1		 time: 2
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsBlockGroup:			true		 time: 1
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsCliffordSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsHTrivial:		true		 time: 1
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsLTrivial:		true		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsRTrivial:		true		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsInverseSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsLeftZeroSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsOrthodoxSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsRectangularBand:		false		 time: 1
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsRegularSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsSynchronizingSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 17, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 17	IsZeroSemigroup:		false		 time: 1


gap> gens:=[Transformation( [ 3, 1, 2, 3, 2, 3, 2, 3 ] ),
> Transformation( [ 2, 5, 8, 5, 2, 5, 7, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	Size:				38		 time: 9
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	NrRClasses:		4		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	NrDClasses:		2		 time: 3
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	NrLClasses:		3		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	NrHClasses:		7		 time: 1
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	NrIdempotents:			7		 time: 0
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	NrRegularDClasses:		2		 time: 0
gap> t:=Runtime();; out:=HasMultZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tHasMultZero:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	HasMultZero:			false		 time: 0
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	HasMultIdentity:		false		 time: 1
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	GroupOfUnitsStructure:		fail		 time: 0
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	MinimalIdealSize:		36		 time: 5
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsCliffordSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsCompletelyRegularSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsInverseSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsOrthodoxSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsRegularSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsSynchronizingSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 18, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 18	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 3, 3, 2, 6, 2, 4, 4, 6 ] ),
> Transformation( [ 5, 1, 7, 8, 7, 5, 8, 1 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	Size:				96		 time: 16
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	NrRClasses:		2		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	NrDClasses:		1		 time: 2
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	NrLClasses:		2		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	NrHClasses:		4		 time: 1
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	NrIdempotents:			4		 time: 0
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	NrRegularDClasses:		1		 time: 0
gap> t:=Runtime();; out:=HasMultZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tHasMultZero:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	HasMultZero:			false		 time: 1
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	HasMultIdentity:		false		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	GroupOfUnitsStructure:		fail		 time: 0
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	MinimalIdealSize:		96		 time: 8
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsCliffordSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsCompletelyRegularSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsCompletelySimpleSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsInverseSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsOrthodoxSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsRegularSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsSimpleSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsSynchronizingSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 19, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 19	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 10, 8, 7, 4, 1, 4, 10, 10, 7, 2 ] ),
> Transformation( [ 5, 2, 5, 5, 9, 10, 8, 3, 8, 10 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	Size:				30176		 time: 80
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	NrRClasses:		152		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	NrDClasses:		11		 time: 25
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	NrLClasses:		456		 time: 1
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	NrHClasses:		4234		 time: 2
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	NrIdempotents:			1105		 time: 2
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	NrRegularDClasses:		7		 time: 0
gap> t:=Runtime();; out:=HasMultZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tHasMultZero:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	HasMultZero:			false		 time: 1
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	HasMultIdentity:		false		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	GroupOfUnitsStructure:		fail		 time: 1
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	MinimalIdealSize:		8		 time: 2
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsBlockGroup:			false		 time: 1
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsCliffordSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsInverseSemigroup:		false		 time: 2
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsOrthodoxSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsRegularSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsSynchronizingSemigroup:	true		 time: 1
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 20, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 20	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] ),
> Transformation( [ 2, 3, 4, 5, 6, 8, 7, 1, 2, 2 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	Size:				10080		 time: 9
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	NrRClasses:		2		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	NrDClasses:		1		 time: 3
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	NrLClasses:		1		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	NrHClasses:		2		 time: 0
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	NrIdempotents:			2		 time: 0
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	NrRegularDClasses:		1		 time: 0
gap> t:=Runtime();; out:=HasMultZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tHasMultZero:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	HasMultZero:			false		 time: 1
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	HasMultIdentity:		false		 time: 1
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	GroupOfUnitsStructure:		fail		 time: 0
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	MinimalIdealSize:		10080		 time: 863
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsCliffordSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsCompletelyRegularSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsCompletelySimpleSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsInverseSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsLeftZeroSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsOrthodoxSemigroup:		true		 time: 7
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsRegularSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsSemilatticeAsSemigroup:	false		 time: 1
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsSimpleSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsSynchronizingSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 21, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 21	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] ),
> Transformation( [ 3, 8, 7, 4, 1, 4, 3, 3, 7, 2 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	Size:				121804		 time: 102
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	NrRClasses:		462		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	NrDClasses:		33		 time: 79
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	NrLClasses:		8320		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	NrHClasses:		24159		 time: 2
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	NrIdempotents:			4161		 time: 11
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	NrRegularDClasses:		6		 time: 1
gap> t:=Runtime();; out:=HasMultZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tHasMultZero:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	HasMultZero:			false		 time: 1
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	HasMultIdentity:		false		 time: 24
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	GroupOfUnitsStructure:		fail		 time: 0
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	MinimalIdealSize:		8		 time: 3
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsCliffordSemigroup:		false		 time: 2
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsInverseSemigroup:		false		 time: 2
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsOrthodoxSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsRegularSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsSynchronizingSemigroup:	true		 time: 1
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 22, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 22	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 1, 4, 6, 2, 5, 3, 7, 8 ] ),
> Transformation( [ 6, 3, 2, 7, 5, 1, 8, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	Size:				131		 time: 8
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	NrRClasses:		41		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	NrDClasses:		11		 time: 10
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	NrLClasses:		25		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	NrHClasses:		101		 time: 2
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	NrIdempotents:			16		 time: 1
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	NrRegularDClasses:		6		 time: 0
gap> t:=Runtime();; out:=HasMultZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tHasMultZero:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	HasMultZero:			true		 time: 1
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	HasMultIdentity:		true		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	GroupOfUnitsStructure:		C2		 time: 2
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	MinimalIdealSize:		1		 time: 2
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsBlockGroup:			true		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsCliffordSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsInverseSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsMonoidAsSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsOrthodoxSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsRegularSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsSynchronizingSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 23, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 23	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 5, 6, 7, 3, 1, 4, 2, 8 ] ),
> Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	Size:				52300		 time: 17
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	NrRClasses:		130		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	NrDClasses:		14		 time: 43
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	NrLClasses:		2014		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	NrHClasses:		11646		 time: 4
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	NrIdempotents:			94		 time: 2
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	NrRegularDClasses:		7		 time: 0
gap> t:=Runtime();; out:=HasMultZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tHasMultZero:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	HasMultZero:			true		 time: 1
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	HasMultIdentity:		true		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	GroupOfUnitsStructure:		C10		 time: 3
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	MinimalIdealSize:		1		 time: 8
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsBlockGroup:			true		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsCliffordSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsInverseSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsMonoidAsSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsOrthodoxSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsRegularSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsSynchronizingSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 24, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 24	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 1, 2, 4, 5, 6, 3, 7, 8 ] ),
> Transformation( [ 3, 3, 4, 5, 6, 2, 7, 8 ] ),
> Transformation( [ 1, 2, 5, 3, 6, 8, 4, 4 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	Size:				864		 time: 4
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	NrRClasses:		4		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	NrDClasses:		4		 time: 7
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	NrLClasses:		4		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	NrHClasses:		4		 time: 1
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	NrIdempotents:			4		 time: 0
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	NrRegularDClasses:		4		 time: 0
gap> t:=Runtime();; out:=HasMultZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tHasMultZero:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	HasMultZero:			false		 time: 1
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	HasMultIdentity:		true		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	GroupOfUnitsStructure:		C4		 time: 1
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	MinimalIdealSize:		720		 time: 35
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsBlockGroup:			true		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsCliffordSemigroup:		true		 time: 1
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsCompletelyRegularSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsInverseSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsMonoidAsSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsOrthodoxSemigroup:		true		 time: 1
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsRegularSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsSynchronizingSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 25, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 25	IsZeroSemigroup:		false		 time: 0


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
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	Size:				5		 time: 18
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	NrRClasses:		5		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	NrDClasses:		5		 time: 4
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	NrLClasses:		5		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	NrHClasses:		5		 time: 0
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	NrIdempotents:			5		 time: 0
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	NrRegularDClasses:		5		 time: 0
gap> t:=Runtime();; out:=HasMultZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tHasMultZero:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	HasMultZero:			true		 time: 0
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	HasMultIdentity:		true		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	GroupOfUnitsStructure:		1		 time: 1
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	MinimalIdealSize:		1		 time: 9
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsBlockGroup:			true		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsCliffordSemigroup:		true		 time: 1
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsCommutative:			true		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsCompletelyRegularSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsHTrivial:		true		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsLTrivial:		true		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsRTrivial:		true		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsInverseSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsMonoidAsSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsOrthodoxSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsRegularSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsSemilatticeAsSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsSynchronizingSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 26, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 26	IsZeroSemigroup:		false		 time: 0


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
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	Size:				639		 time: 6
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	NrRClasses:		5		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	NrDClasses:		5		 time: 9
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	NrLClasses:		5		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	NrHClasses:		5		 time: 2
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	NrIdempotents:			5		 time: 0
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	NrRegularDClasses:		5		 time: 0
gap> t:=Runtime();; out:=HasMultZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tHasMultZero:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	HasMultZero:			false		 time: 0
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	HasMultIdentity:		true		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	GroupOfUnitsStructure:		C5		 time: 1
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	MinimalIdealSize:		24		 time: 4
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsBlockGroup:			true		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsCliffordSemigroup:		true		 time: 2
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsCompletelyRegularSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsCompletelySimpleSemigroup:	false		 time: 1
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsInverseSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsMonoidAsSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsOrthodoxSemigroup:		true		 time: 1
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsRegularSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsSynchronizingSemigroup:	false		 time: 1
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 27, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 27	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 2, 1, 1, 2, 1 ] ),
> Transformation( [ 3, 4, 3, 4, 4 ] ),
> Transformation( [ 3, 4, 3, 4, 3 ] ),
> Transformation( [ 4, 3, 3, 4, 4 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	Size:				16		 time: 2
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	NrRClasses:		4		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	NrDClasses:		1		 time: 2
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	NrLClasses:		2		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	NrHClasses:		8		 time: 0
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	NrIdempotents:			8		 time: 0
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	NrRegularDClasses:		1		 time: 1
gap> t:=Runtime();; out:=HasMultZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tHasMultZero:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	HasMultZero:			false		 time: 0
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	HasMultIdentity:		false		 time: 1
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	GroupOfUnitsStructure:		fail		 time: 0
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	MinimalIdealSize:		16		 time: 4
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsCliffordSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsCompletelyRegularSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsCompletelySimpleSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsInverseSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsOrthodoxSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsRegularSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsSimpleSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsSynchronizingSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 28, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 28	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 4, 4, 4, 1, 1, 6, 7, 8, 9, 10, 11, 1 ] ),
> Transformation( [ 6, 6, 6, 7, 7, 1, 4, 8, 9, 10, 11, 7 ] ),
> Transformation( [ 8, 8, 8, 9, 9, 10, 11, 1, 4, 6, 7, 9 ] ),
> Transformation( [ 2, 2, 2, 4, 4, 6, 7, 8, 9, 10, 11, 4 ] ),
> Transformation( [ 1, 1, 1, 5, 5, 6, 7, 8, 9, 10, 11, 5 ] ),
> Transformation( [ 1, 1, 4, 4, 4, 6, 7, 8, 9, 10, 11, 1 ] ),
> Transformation( [ 1, 1, 7, 4, 4, 6, 7, 8, 9, 10, 11, 6 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	Size:				1152		 time: 3
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	NrRClasses:		3		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	NrDClasses:		1		 time: 3
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	NrLClasses:		3		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	NrHClasses:		9		 time: 1
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	NrIdempotents:			9		 time: 0
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	NrRegularDClasses:		1		 time: 1
gap> t:=Runtime();; out:=HasMultZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tHasMultZero:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	HasMultZero:			false		 time: 0
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	HasMultIdentity:		false		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	GroupOfUnitsStructure:		fail		 time: 0
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	MinimalIdealSize:		1152		 time: 124
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsCliffordSemigroup:		false		 time: 2
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsCompletelyRegularSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsCompletelySimpleSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsInverseSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsOrthodoxSemigroup:		true		 time: 1
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsRegularSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsSimpleSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsSynchronizingSemigroup:	false		 time: 1
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 29, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 29	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 1, 2, 2, 1, 2 ] ),
> Transformation( [ 3, 4, 3, 4, 4 ] ),
> Transformation( [ 3, 4, 3, 4, 3 ] ),
> Transformation( [ 4, 3, 3, 4, 4 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	Size:				16		 time: 2
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	NrRClasses:		4		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	NrDClasses:		1		 time: 1
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	NrLClasses:		2		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	NrHClasses:		8		 time: 0
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	NrIdempotents:			8		 time: 1
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	NrRegularDClasses:		1		 time: 1
gap> t:=Runtime();; out:=HasMultZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tHasMultZero:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	HasMultZero:			false		 time: 0
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	HasMultIdentity:		false		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	GroupOfUnitsStructure:		fail		 time: 0
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	MinimalIdealSize:		16		 time: 4
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsCliffordSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsCompletelyRegularSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsCompletelySimpleSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsInverseSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsOrthodoxSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsRegularSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsSimpleSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsSynchronizingSemigroup:	false		 time: 1
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 30, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 30	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 2, 6, 1, 7, 5, 3, 4 ] ),
> Transformation( [ 5, 3, 7, 2, 1, 6, 4 ] ),
> Transformation( [ 2, 5, 5, 3, 4, 2, 3 ] ),
> Transformation( [ 1, 5, 1, 6, 1, 5, 6 ] ),
> Transformation( [ 6, 2, 2, 2, 5, 1, 2 ] ),
> Transformation( [ 7, 5, 4, 4, 4, 5, 5 ] ),
> Transformation( [ 5, 1, 6, 1, 1, 5, 1 ] ),
> Transformation( [ 3, 5, 2, 3, 2, 2, 3 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	Size:				21343		 time: 30
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	NrRClasses:		401		 time: 0
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	NrDClasses:		7		 time: 39
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	NrLClasses:		99		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	NrHClasses:		4418		 time: 2
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	NrIdempotents:			1471		 time: 3
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	NrRegularDClasses:		6		 time: 0
gap> t:=Runtime();; out:=HasMultZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tHasMultZero:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	HasMultZero:			false		 time: 1
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	HasMultIdentity:		true		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	GroupOfUnitsStructure:		PSL(3,2)		 time: 18
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	MinimalIdealSize:		7		 time: 2
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsCliffordSemigroup:		false		 time: 4
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsInverseSemigroup:		false		 time: 3
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsMonoidAsSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsOrthodoxSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsRegularSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsSynchronizingSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 32, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 32	IsZeroSemigroup:		false		 time: 0


gap> gens:=[Transformation( [ 3, 6, 9, 1, 4, 7, 2, 5, 8 ] ),
> Transformation( [ 3, 6, 9, 7, 1, 4, 5, 8, 2 ] ),
> Transformation( [ 8, 2, 5, 5, 4, 5, 5, 2, 8 ] ),
> Transformation( [ 4, 4, 8, 4, 4, 2, 4, 4, 5 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=Size(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	Size:				82953		 time: 46
gap> t:=Runtime();; out:=NrRClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tNrRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	NrRClasses:		503		 time: 1
gap> t:=Runtime();; out:=NrDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tNrDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	NrDClasses:		7		 time: 134
gap> t:=Runtime();; out:=NrLClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tNrLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	NrLClasses:		214		 time: 0
gap> t:=Runtime();; out:=NrHClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tNrHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	NrHClasses:		16426		 time: 1
gap> t:=Runtime();; out:=NrIdempotents(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	NrIdempotents:			3718		 time: 7
gap> t:=Runtime();; out:=NrRegularDClasses(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	NrRegularDClasses:		6		 time: 0
gap> t:=Runtime();; out:=HasMultZero(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tHasMultZero:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	HasMultZero:			false		 time: 1
gap> t:=Runtime();; out:=HasMultIdentity(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	HasMultIdentity:		true		 time: 0
gap> t:=Runtime();; out:=GroupOfUnitsStructure(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	GroupOfUnitsStructure:		(((C3 x C3) : Q8) : C3) : C2		 time: 987
gap> t:=Runtime();; out:=MinimalIdealSize(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	MinimalIdealSize:		9		 time: 7
gap> t:=Runtime();; out:=IsBlockGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsCliffordSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsCliffordSemigroup:		false		 time: 5
gap> t:=Runtime();; out:=IsCommutative(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsCommutative:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsCommutative:			false		 time: 0
gap> t:=Runtime();; out:=IsCompletelyRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsCompletelySimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsHTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsHTrivial:		false		 time: 1
gap> t:=Runtime();; out:=IsLTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsRTrivial(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=IsGroupAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsInverseSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsInverseSemigroup:		false		 time: 4
gap> t:=Runtime();; out:=IsLeftZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsMonoidAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsMonoidAsSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=IsOrthodoxSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsOrthodoxSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRectangularBand(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=IsRegularSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsRegularSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsRightZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSemilatticeAsSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=IsSimpleSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=IsSynchronizingSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsSynchronizingSemigroup:	true		 time: 1
gap> t:=Runtime();; out:=IsZeroGroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=IsZeroSemigroup(s);; t:=Runtime()-t;;
gap> Print("Example ", 33, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 33	IsZeroSemigroup:		false		 time: 0

g
#
gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;
gap> Unbind(s);; Unbind(gens);;
gap> STOP_TEST("Citrus package: examples.tst", 0);
