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
gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ), 
>  Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ), 
>  Transformation( [ 4, 1, 8, 3, 5, 7, 3, 5 ] ), 
>  Transformation( [ 4, 3, 4, 5, 6, 4, 1, 2 ] ), 
>  Transformation( [ 5, 4, 8, 8, 5, 6, 1, 5 ] ), 
>  Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ), 
>  Transformation( [ 7, 1, 2, 2, 2, 7, 4, 5 ] ), 
>  Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> t:=Runtime();; out:=CitrusTests[1](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tSize:" , "\t\t\t\t",
> out, "\t\t time: ", t, "\n");
Example 1     Size:                           597369           time: 1381
gap> t:=Runtime();; out:=CitrusTests[2](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tNrGreensRClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[3](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tNrGreensDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[4](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tNrGreensLClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[5](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tNrGreensHClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[6](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tNrIdempotents:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[7](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tNrRegularDClasses:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[8](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tMultiplicativeZero:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[9](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tHasMultIdentity:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[10](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tGroupOfUnitsStructure:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[11](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tMinimalIdealSize:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[12](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsBlockGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[13](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsCliffordSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[14](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsCommutativeSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[15](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsCompletelyRegularSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[16](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsCompletelySimpleSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[17](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsGreensHTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[18](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsGreensLTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[19](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsGreensRTrivial:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[20](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsGroupAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[21](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsInverseSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[22](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsLeftZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[23](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsMonoidAsSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[24](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsOrthodoxSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[25](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsRectangularBand:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[26](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsRegularSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[27](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsRightZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[28](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsSemiband:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[29](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsSemilatticeAsSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[30](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsSimpleSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[31](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsSynchronizingSemigroup:" , "\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[32](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsZeroGroup:" , "\t\t\t",
> out, "\t\t time: ", t, "\n");

gap> t:=Runtime();; out:=CitrusTests[33](s);; t:=Runtime()-t;;
gap> Print("Example ", 1, "\tIsZeroSemigroup:" , "\t\t",
> out, "\t\t time: ", t, "\n");

gap> STOP_TEST( "performance.tst 0.1", 0);


