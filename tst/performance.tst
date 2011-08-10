#############################################################################
##
#W  performance.tst
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# ReadTest(Filename(DirectoriesPackageLibrary("citrus","tst"),"performance.tst"));

gap> START_TEST("performance.tst 0.1");
gap> SetGasmanMessageStatus("none");
gap>   CitrusTests:=[Size, NrGreensRClasses, NrGreensDClasses, NrGreensLClasses,
>  NrGreensHClasses, NrIdempotents, NrRegularDClasses, MultiplicativeZero,
>  HasMultIdentity, GroupOfUnitsStructure, MinimalIdealSize, IsBlockGroup,
>  IsCliffordSemigroup, IsCommutativeSemigroup, IsCompletelyRegularSemigroup,
>  IsCompletelySimpleSemigroup, IsGreensHTrivial, IsGreensLTrivial,
>  IsGreensRTrivial, IsGroupAsSemigroup, IsInverseSemigroup, 
> IsLeftZeroSemigroup,
>  IsMonoidAsSemigroup, IsOrthodoxSemigroup, IsRectangularBand,
>  IsRegularSemigroup, IsRightZeroSemigroup, IsSemiband,
>  IsSemilatticeAsSemigroup, IsSimpleSemigroup,
>  IsSynchronizingSemigroup, IsZeroGroup, IsZeroSemigroup];;

gap> gens:=[Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
> Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
> Transformation( [ 4, 1, 8, 3, 5, 7, 3, 5 ] ),
> Transformation( [ 4, 3, 4, 5, 6, 4, 1, 2 ] ),
> Transformation( [ 5, 4, 8, 8, 5, 6, 1, 5 ] ),
> Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
> Transformation( [ 7, 1, 2, 2, 2, 7, 4, 5 ] ),
> Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=CitrusTests[1](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	Size:				597369		 time: 1582
gap> t:=Runtime();; out:=CitrusTests[2](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tNrGreensRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	NrGreensRClasses:		10139		 time: 0
gap> t:=Runtime();; out:=CitrusTests[3](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tNrGreensDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	NrGreensDClasses:		257		 time: 307
gap> t:=Runtime();; out:=CitrusTests[4](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tNrGreensLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	NrGreensLClasses:		3065		 time: 1
gap> t:=Runtime();; out:=CitrusTests[5](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tNrGreensHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	NrGreensHClasses:		50989		 time: 5
gap> t:=Runtime();; out:=CitrusTests[6](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	NrIdempotents:			8194		 time: 59
gap> t:=Runtime();; out:=CitrusTests[7](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	NrRegularDClasses:		6		 time: 1
gap> t:=Runtime();; out:=CitrusTests[8](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tMultiplicativeZero:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	MultiplicativeZero:		fail		 time: 1
gap> t:=Runtime();; out:=CitrusTests[9](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	HasMultIdentity:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[10](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	GroupOfUnitsStructure:		fail		 time: 0
gap> t:=Runtime();; out:=CitrusTests[11](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	MinimalIdealSize:		8		 time: 3
gap> t:=Runtime();; out:=CitrusTests[12](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[13](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsCliffordSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=CitrusTests[14](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsCommutativeSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsCommutativeSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[15](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[16](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[17](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsGreensHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsGreensHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[18](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsGreensLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsGreensLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[19](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsGreensRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsGreensRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[20](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[21](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsInverseSemigroup:		false		 time: 6
gap> t:=Runtime();; out:=CitrusTests[22](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[23](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[24](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsOrthodoxSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[25](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[26](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsRegularSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[27](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[28](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsSemiband:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsSemiband:			false		 time: 213
gap> t:=Runtime();; out:=CitrusTests[29](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsSemilatticeAsSemigroup:	false		 time: 1
gap> t:=Runtime();; out:=CitrusTests[30](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[31](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsSynchronizingSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=CitrusTests[32](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[33](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 1	IsZeroSemigroup:		false		 time: 0

gap> gens:=[Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
> Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
> Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
> Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=CitrusTests[1](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	Size:				95540		 time: 399
gap> t:=Runtime();; out:=CitrusTests[2](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tNrGreensRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	NrGreensRClasses:		6343		 time: 0
gap> t:=Runtime();; out:=CitrusTests[3](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tNrGreensDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	NrGreensDClasses:		944		 time: 320
gap> t:=Runtime();; out:=CitrusTests[4](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tNrGreensLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	NrGreensLClasses:		9904		 time: 2
gap> t:=Runtime();; out:=CitrusTests[5](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tNrGreensHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	NrGreensHClasses:		23659		 time: 10
gap> t:=Runtime();; out:=CitrusTests[6](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	NrIdempotents:			2595		 time: 57
gap> t:=Runtime();; out:=CitrusTests[7](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	NrRegularDClasses:		6		 time: 3
gap> t:=Runtime();; out:=CitrusTests[8](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tMultiplicativeZero:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	MultiplicativeZero:		fail		 time: 1
gap> t:=Runtime();; out:=CitrusTests[9](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	HasMultIdentity:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[10](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	GroupOfUnitsStructure:		fail		 time: 0
gap> t:=Runtime();; out:=CitrusTests[11](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	MinimalIdealSize:		8		 time: 3
gap> t:=Runtime();; out:=CitrusTests[12](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[13](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsCliffordSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[14](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsCommutativeSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsCommutativeSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[15](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[16](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[17](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsGreensHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsGreensHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[18](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsGreensLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsGreensLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[19](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsGreensRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsGreensRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[20](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[21](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsInverseSemigroup:		false		 time: 2
gap> t:=Runtime();; out:=CitrusTests[22](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[23](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[24](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsOrthodoxSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[25](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[26](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsRegularSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[27](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[28](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsSemiband:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsSemiband:			false		 time: 100
gap> t:=Runtime();; out:=CitrusTests[29](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[30](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[31](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsSynchronizingSemigroup:	true		 time: 1
gap> t:=Runtime();; out:=CitrusTests[32](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[33](s);; t:=Runtime()-t;;
gap> Print("Example ", 2, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 2	IsZeroSemigroup:		false		 time: 0

gap> gens:=[Transformation( [ 2, 6, 7, 2, 6, 1, 1, 5 ] ),
> Transformation( [ 3, 8, 1, 4, 5, 6, 7, 1 ] ),
> Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] ),
> Transformation( [ 7, 1, 7, 4, 2, 5, 6, 3 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=CitrusTests[1](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	Size:				233605		 time: 357
gap> t:=Runtime();; out:=CitrusTests[2](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tNrGreensRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	NrGreensRClasses:		4396		 time: 0
gap> t:=Runtime();; out:=CitrusTests[3](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tNrGreensDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	NrGreensDClasses:		661		 time: 397
gap> t:=Runtime();; out:=CitrusTests[4](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tNrGreensLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	NrGreensLClasses:		16914		 time: 1
gap> t:=Runtime();; out:=CitrusTests[5](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tNrGreensHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	NrGreensHClasses:		40882		 time: 13
gap> t:=Runtime();; out:=CitrusTests[6](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	NrIdempotents:			4891		 time: 22
gap> t:=Runtime();; out:=CitrusTests[7](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	NrRegularDClasses:		7		 time: 2
gap> t:=Runtime();; out:=CitrusTests[8](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tMultiplicativeZero:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	MultiplicativeZero:		fail		 time: 2
gap> t:=Runtime();; out:=CitrusTests[9](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	HasMultIdentity:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[10](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	GroupOfUnitsStructure:		fail		 time: 0
gap> t:=Runtime();; out:=CitrusTests[11](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	MinimalIdealSize:		8		 time: 3
gap> t:=Runtime();; out:=CitrusTests[12](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[13](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsCliffordSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[14](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsCommutativeSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsCommutativeSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[15](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[16](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[17](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsGreensHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsGreensHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[18](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsGreensLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsGreensLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[19](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsGreensRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsGreensRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[20](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[21](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsInverseSemigroup:		false		 time: 4
gap> t:=Runtime();; out:=CitrusTests[22](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[23](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[24](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsOrthodoxSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[25](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[26](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsRegularSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[27](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[28](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsSemiband:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsSemiband:			false		 time: 267
gap> t:=Runtime();; out:=CitrusTests[29](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[30](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[31](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsSynchronizingSemigroup:	true		 time: 1
gap> t:=Runtime();; out:=CitrusTests[32](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[33](s);; t:=Runtime()-t;;
gap> Print("Example ", 3, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 3	IsZeroSemigroup:		false		 time: 0
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
gap> t:=Runtime();; out:=CitrusTests[1](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	Size:				97310		 time: 156
gap> t:=Runtime();; out:=CitrusTests[2](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tNrGreensRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	NrGreensRClasses:		879		 time: 0
gap> t:=Runtime();; out:=CitrusTests[3](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tNrGreensDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	NrGreensDClasses:		401		 time: 116
gap> t:=Runtime();; out:=CitrusTests[4](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tNrGreensLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	NrGreensLClasses:		1207		 time: 1
gap> t:=Runtime();; out:=CitrusTests[5](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tNrGreensHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	NrGreensHClasses:		10664		 time: 22
gap> t:=Runtime();; out:=CitrusTests[6](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	NrIdempotents:			2434		 time: 7
gap> t:=Runtime();; out:=CitrusTests[7](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	NrRegularDClasses:		6		 time: 2
gap> t:=Runtime();; out:=CitrusTests[8](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tMultiplicativeZero:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	MultiplicativeZero:		fail		 time: 1
gap> t:=Runtime();; out:=CitrusTests[9](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	HasMultIdentity:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[10](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	GroupOfUnitsStructure:		fail		 time: 0
gap> t:=Runtime();; out:=CitrusTests[11](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	MinimalIdealSize:		7		 time: 19
gap> t:=Runtime();; out:=CitrusTests[12](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[13](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsCliffordSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[14](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsCommutativeSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsCommutativeSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[15](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[16](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[17](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsGreensHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsGreensHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[18](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsGreensLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsGreensLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[19](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsGreensRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsGreensRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[20](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[21](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsInverseSemigroup:		false		 time: 4
gap> t:=Runtime();; out:=CitrusTests[22](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[23](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[24](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsOrthodoxSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[25](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[26](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsRegularSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[27](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[28](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsSemiband:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsSemiband:			false		 time: 668
gap> t:=Runtime();; out:=CitrusTests[29](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[30](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[31](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsSynchronizingSemigroup:	true		 time: 1
gap> t:=Runtime();; out:=CitrusTests[32](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsZeroGroup:			false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[33](s);; t:=Runtime()-t;;
gap> Print("Example ", 4, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 4	IsZeroSemigroup:		false		 time: 0
gap> gens:=[Transformation( [ 3, 4, 1, 2, 1 ] ),
> Transformation( [ 4, 2, 1, 5, 5 ] ),
> Transformation( [ 4, 2, 2, 2, 4 ] )];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=CitrusTests[1](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	Size:				731		 time: 5
gap> t:=Runtime();; out:=CitrusTests[2](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tNrGreensRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	NrGreensRClasses:		26		 time: 0
gap> t:=Runtime();; out:=CitrusTests[3](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tNrGreensDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	NrGreensDClasses:		4		 time: 14
gap> t:=Runtime();; out:=CitrusTests[4](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tNrGreensLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	NrGreensLClasses:		23		 time: 0
gap> t:=Runtime();; out:=CitrusTests[5](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tNrGreensHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	NrGreensHClasses:		194		 time: 1
gap> t:=Runtime();; out:=CitrusTests[6](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	NrIdempotents:			100		 time: 0
gap> t:=Runtime();; out:=CitrusTests[7](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	NrRegularDClasses:		4		 time: 0
gap> t:=Runtime();; out:=CitrusTests[8](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tMultiplicativeZero:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	MultiplicativeZero:		fail		 time: 1
gap> t:=Runtime();; out:=CitrusTests[9](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	HasMultIdentity:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[10](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	GroupOfUnitsStructure:		fail		 time: 0
gap> t:=Runtime();; out:=CitrusTests[11](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	MinimalIdealSize:		5		 time: 2
gap> t:=Runtime();; out:=CitrusTests[12](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsBlockGroup:			false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[13](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsCliffordSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=CitrusTests[14](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsCommutativeSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsCommutativeSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[15](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsCompletelyRegularSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[16](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsCompletelySimpleSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[17](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsGreensHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsGreensHTrivial:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[18](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsGreensLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsGreensLTrivial:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[19](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsGreensRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsGreensRTrivial:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[20](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsGroupAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[21](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsInverseSemigroup:		false		 time: 1
gap> t:=Runtime();; out:=CitrusTests[22](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsLeftZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[23](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsMonoidAsSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[24](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsOrthodoxSemigroup:		false		 time: 6
gap> t:=Runtime();; out:=CitrusTests[25](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsRectangularBand:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[26](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsRegularSemigroup:		true		 time: 0
gap> t:=Runtime();; out:=CitrusTests[27](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsRightZeroSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[28](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsSemiband:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsSemiband:			false		 time: 142
gap> t:=Runtime();; out:=CitrusTests[29](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsSemilatticeAsSemigroup:	false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[30](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsSimpleSemigroup:		false		 time: 0
gap> t:=Runtime();; out:=CitrusTests[31](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsSynchronizingSemigroup:	true		 time: 0
gap> t:=Runtime();; out:=CitrusTests[32](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsZeroGroup:			false		 time: 1
gap> t:=Runtime();; out:=CitrusTests[33](s);; t:=Runtime()-t;;
gap> Print("Example ", 5, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");
Example 5	IsZeroSemigroup:		false		 time: 0


gap> STOP_TEST( "performance.tst 0.1", 0);