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
gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
>  Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
>  Transformation( [ 4, 1, 8, 3, 5, 7, 3, 5 ] ),
>  Transformation( [ 4, 3, 4, 5, 6, 4, 1, 2 ] ),
>  Transformation( [ 5, 4, 8, 8, 5, 6, 1, 5 ] ),
>  Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
>  Transformation( [ 7, 1, 2, 2, 2, 7, 4, 5 ] ),
>  Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
> s:=Semigroup(gens);;
gap> CitrusTestSuite(s, 1);
Example 1       Size:                           597369           time: 1626
Example 1       NrGreensRClasses:               10139            time: 0
Example 1       NrGreensDClasses:               257              time: 288
Example 1       NrGreensLClasses:               3065             time: 1
Example 1       NrGreensHClasses:               50989            time: 4
Example 1       NrIdempotents:                  8194             time: 39
Example 1       NrRegularDClasses:              6                time: 1
Example 1       MultiplicativeZero:             fail             time: 1
Example 1       MultiplicativeNeutralElement:   fail             time: 0
Example 1       GroupOfUnits:                   fail             time: 1
Example 1       MinimalIdealSize:               8                time: 26
Example 1       IsBlockGroup:                   false            time: 0
Example 1       IsCliffordSemigroup:            false            time: 0
Example 1       IsCommutativeSemigroup:         false            time: 0
Example 1       IsCompletelyRegularSemigroup:   false            time: 0
Example 1       IsCompletelySimpleSemigroup:    false            time: 0
Example 1       IsGreensHTrivial:               false            time: 0
Example 1       IsGreensLTrivial:               false            time: 0
Example 1       IsGreensRTrivial:               false            time: 1
Example 1       IsGroupAsSemigroup:             false            time: 0
Example 1       IsInverseSemigroup:             false            time: 5
Example 1       IsLeftZeroSemigroup:            false            time: 0
Example 1       IsMonoidAsSemigroup:            false            time: 0
Example 1       IsOrthodoxSemigroup:            false            time: 0
Example 1       IsRectangularBand:              false            time: 0
Example 1       IsRegularSemigroup:             false            time: 0
Example 1       IsRightZeroSemigroup:           false            time: 0
Example 1       IsSemiband:                     false            time: 197
Example 1       IsSemilatticeAsSemigroup:       false            time: 0
Example 1       IsSimpleSemigroup:              false            time: 0
Example 1       IsSynchronizingSemigroup:       true             time: 1
Example 1       IsZeroGroup:                    false            time: 0
Example 1       IsZeroSemigroup:                false            time: 0

gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
>   Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
>   Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
>   Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> CitrusTestSuite(s, 2);
Example 2       Size:                           95540            time: 368
Example 2       NrGreensRClasses:               6343             time: 0
Example 2       NrGreensDClasses:               944              time: 240
Example 2       NrGreensLClasses:               9904             time: 2
Example 2       NrGreensHClasses:               23659            time: 12
Example 2       NrIdempotents:                  2595             time: 21
Example 2       NrRegularDClasses:              6                time: 4
Example 2       MultiplicativeZero:             fail             time: 1
Example 2       MultiplicativeNeutralElement:   fail             time: 0
Example 2       GroupOfUnits:                   fail             time: 0
Example 2       MinimalIdealSize:               8                time: 4
Example 2       IsBlockGroup:                   false            time: 0
Example 2       IsCliffordSemigroup:            false            time: 0
Example 2       IsCommutativeSemigroup:         false            time: 0
Example 2       IsCompletelyRegularSemigroup:   false            time: 0
Example 2       IsCompletelySimpleSemigroup:    false            time: 1
Example 2       IsGreensHTrivial:               false            time: 0
Example 2       IsGreensLTrivial:               false            time: 0
Example 2       IsGreensRTrivial:               false            time: 0
Example 2       IsGroupAsSemigroup:             false            time: 0
Example 2       IsInverseSemigroup:             false            time: 3
Example 2       IsLeftZeroSemigroup:            false            time: 0
Example 2       IsMonoidAsSemigroup:            false            time: 0
Example 2       IsOrthodoxSemigroup:            false            time: 0
Example 2       IsRectangularBand:              false            time: 0
Example 2       IsRegularSemigroup:             false            time: 0
Example 2       IsRightZeroSemigroup:           false            time: 0
Example 2       IsSemiband:                     false            time: 270
Example 2       IsSemilatticeAsSemigroup:       false            time: 0
Example 2       IsSimpleSemigroup:              false            time: 0
Example 2       IsSynchronizingSemigroup:       true             time: 1
Example 2       IsZeroGroup:                    false            time: 0
Example 2       IsZeroSemigroup:                false            time: 0

gap> gens:=[ Transformation( [ 2, 6, 7, 2, 6, 1, 1, 5 ] ),
>   Transformation( [ 3, 8, 1, 4, 5, 6, 7, 1 ] ),
>   Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] ),
>   Transformation( [ 7, 1, 7, 4, 2, 5, 6, 3 ] ) ];;
gap> s:=Monoid(gens);;
gap> CitrusTestSuite(s, 3);
Example 3       Size:                           233606           time: 339
Example 3       NrGreensRClasses:               4397             time: 0
Example 3       NrGreensDClasses:               662              time: 292
Example 3       NrGreensLClasses:               16915            time: 2
Example 3       NrGreensHClasses:               40883            time: 10
Example 3       NrIdempotents:                  4892             time: 26
Example 3       NrRegularDClasses:              8                time: 3
Example 3       MultiplicativeZero:             fail             time: 1
Example 3       HasMultIdentity:                true             time: 0
Example 3       GroupOfUnitsStructure:          1                time: 1
Example 3       MinimalIdealSize:               8                time: 5
Example 3       IsBlockGroup:                   false            time: 0
Example 3       IsCliffordSemigroup:            false            time: 104
Example 3       IsCommutativeSemigroup:         false            time: 0
Example 3       IsCompletelyRegularSemigroup:   false            time: 0
Example 3       IsCompletelySimpleSemigroup:    false            time: 0
Example 3       IsGreensHTrivial:               false            time: 1
Example 3       IsGreensLTrivial:               false            time: 0
Example 3       IsGreensRTrivial:               false            time: 0
Example 3       IsGroupAsSemigroup:             false            time: 0
Example 3       IsInverseSemigroup:             false            time: 3
Example 3       IsLeftZeroSemigroup:            false            time: 0
Example 3       IsMonoidAsSemigroup:            true             time: 0
Example 3       IsOrthodoxSemigroup:            false            time: 1
Example 3       IsRectangularBand:              false            time: 0
Example 3       IsRegularSemigroup:             false            time: 0
Example 3       IsRightZeroSemigroup:           false            time: 0
Example 3       IsSemiband:                     false            time: 263
Example 3       IsSemilatticeAsSemigroup:       false            time: 0
Example 3       IsSimpleSemigroup:              false            time: 0
Example 3       IsSynchronizingSemigroup:       true             time: 1
Example 3       IsZeroGroup:                    false            time: 0
Example 3       IsZeroSemigroup:                false            time: 0

gap> gens:=[ Transformation( [ 1, 5, 6, 2, 5, 2, 1 ] ),
>   Transformation( [ 1, 7, 5, 4, 3, 5, 7 ] ),
>   Transformation( [ 2, 7, 7, 2, 4, 1, 1 ] ),
>   Transformation( [ 3, 2, 2, 4, 1, 7, 6 ] ),
>   Transformation( [ 3, 3, 5, 1, 7, 1, 6 ] ),
>   Transformation( [ 3, 3, 6, 1, 7, 5, 2 ] ),
>   Transformation( [ 3, 4, 6, 5, 4, 4, 7 ] ),
>   Transformation( [ 5, 2, 4, 5, 1, 4, 5 ] ),
>   Transformation( [ 5, 5, 2, 2, 6, 7, 2 ] ),
>   Transformation( [ 7, 7, 5, 4, 5, 3, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> CitrusTestSuite(s, 4);
Example 4       Size:                           97310            time: 137
Example 4       NrGreensRClasses:               879              time: 0
Example 4       NrGreensDClasses:               401              time: 122
Example 4       NrGreensLClasses:               1207             time: 1
Example 4       NrGreensHClasses:               10664            time: 22
Example 4       NrIdempotents:                  2434             time: 7
Example 4       NrRegularDClasses:              6                time: 1
Example 4       MultiplicativeZero:             fail             time: 2
Example 4       HasMultIdentity:                false            time: 0
Example 4       GroupOfUnitsStructure:          fail             time: 0
Example 4       MinimalIdealSize:               7                time: 3
Example 4       IsBlockGroup:                   false            time: 0
Example 4       IsCliffordSemigroup:            false            time: 0
Example 4       IsCommutativeSemigroup:         false            time: 0
Example 4       IsCompletelyRegularSemigroup:   false            time: 0
Example 4       IsCompletelySimpleSemigroup:    false            time: 0
Example 4       IsGreensHTrivial:               false            time: 0
Example 4       IsGreensLTrivial:               false            time: 1
Example 4       IsGreensRTrivial:               false            time: 0
Example 4       IsGroupAsSemigroup:             false            time: 0
Example 4       IsInverseSemigroup:             false            time: 4
Example 4       IsLeftZeroSemigroup:            false            time: 0
Example 4       IsMonoidAsSemigroup:            false            time: 0
Example 4       IsOrthodoxSemigroup:            false            time: 0
Example 4       IsRectangularBand:              false            time: 0
Example 4       IsRegularSemigroup:             false            time: 0
Example 4       IsRightZeroSemigroup:           false            time: 0
Example 4       IsSemiband:                     false            time: 647
Example 4       IsSemilatticeAsSemigroup:       false            time: 0
Example 4       IsSimpleSemigroup:              false            time: 0
Example 4       IsSynchronizingSemigroup:       true             time: 1
Example 4       IsZeroGroup:                    false            time: 0
Example 4       IsZeroSemigroup:                false            time: 0

gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
>   Transformation( [ 4, 2, 1, 5, 5 ] ),
>   Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> CitrusTestSuite(s, 5);
Example 5       Size:                           731              time: 9
Example 5       NrGreensRClasses:               26               time: 0
Example 5       NrGreensDClasses:               4                time: 31
Example 5       NrGreensLClasses:               23               time: 0
Example 5       NrGreensHClasses:               194              time: 1
Example 5       NrIdempotents:                  100              time: 1
Example 5       NrRegularDClasses:              4                time: 0
Example 5       MultiplicativeZero:             fail             time: 0
Example 5       HasMultIdentity:                false            time: 0
Example 5       GroupOfUnitsStructure:          fail             time: 0
Example 5       MinimalIdealSize:               5                time: 2
Example 5       IsBlockGroup:                   false            time: 0
Example 5       IsCliffordSemigroup:            false            time: 1
Example 5       IsCommutativeSemigroup:         false            time: 0
Example 5       IsCompletelyRegularSemigroup:   false            time: 0
Example 5       IsCompletelySimpleSemigroup:    false            time: 0
Example 5       IsGreensHTrivial:               false            time: 0
Example 5       IsGreensLTrivial:               false            time: 0
Example 5       IsGreensRTrivial:               false            time: 0
Example 5       IsGroupAsSemigroup:             false            time: 0
Example 5       IsInverseSemigroup:             false            time: 1
Example 5       IsLeftZeroSemigroup:            false            time: 0
Example 5       IsMonoidAsSemigroup:            false            time: 0
Example 5       IsOrthodoxSemigroup:            false            time: 4
Example 5       IsRectangularBand:              false            time: 0
Example 5       IsRegularSemigroup:             true             time: 0
Example 5       IsRightZeroSemigroup:           false            time: 0
Example 5       IsSemiband:                     false            time: 22
Example 5       IsSemilatticeAsSemigroup:       false            time: 1
Example 5       IsSimpleSemigroup:              false            time: 0
Example 5       IsSynchronizingSemigroup:       true             time: 0
Example 5       IsZeroGroup:                    false            time: 0
Example 5       IsZeroSemigroup:                false            time: 0

gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ), 
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ), 
> Transformation( [ 3, 3, 4, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> CitrusTestSuite(s, 6);
Example 6       Size:                           62               time: 11
Example 6       NrGreensRClasses:               10               time: 0
Example 6       NrGreensDClasses:               6                time: 77
Example 6       NrGreensLClasses:               15               time: 0
Example 6       NrGreensHClasses:               35               time: 1
Example 6       NrIdempotents:                  20               time: 0
Example 6       NrRegularDClasses:              4                time: 0
Example 6       MultiplicativeZero:             fail             time: 1
Example 6       HasMultIdentity:                true             time: 0
Example 6       GroupOfUnitsStructure:          1                time: 1
Example 6       MinimalIdealSize:               4                time: 1
Example 6       IsBlockGroup:                   false            time: 1
Example 6       IsCliffordSemigroup:            false            time: 0
Example 6       IsCommutativeSemigroup:         false            time: 0
Example 6       IsCompletelyRegularSemigroup:   false            time: 0
Example 6       IsCompletelySimpleSemigroup:    false            time: 0
Example 6       IsGreensHTrivial:               false            time: 0
Example 6       IsGreensLTrivial:               false            time: 0
Example 6       IsGreensRTrivial:               false            time: 0
Example 6       IsGroupAsSemigroup:             false            time: 0
Example 6       IsInverseSemigroup:             false            time: 1
Example 6       IsLeftZeroSemigroup:            false            time: 0
Example 6       IsMonoidAsSemigroup:            true             time: 0
Example 6       IsOrthodoxSemigroup:            false            time: 0
Example 6       IsRectangularBand:              false            time: 0
Example 6       IsRegularSemigroup:             false            time: 0
Example 6       IsRightZeroSemigroup:           false            time: 0
Example 6       IsSemiband:                     false            time: 4
Example 6       IsSemilatticeAsSemigroup:       false            time: 0
Example 6       IsSimpleSemigroup:              false            time: 0
Example 6       IsSynchronizingSemigroup:       true             time: 0
Example 6       IsZeroGroup:                    false            time: 0
Example 6       IsZeroSemigroup:                false            time: 0

gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
>  Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 2, 4, 1, 1 ] ), 
>  Transformation( [ 3, 4, 2, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> CitrusTestSuite(s, 7);
Example 7       Size:                           114              time: 7
Example 7       NrGreensRClasses:               11               time: 0
Example 7       NrGreensDClasses:               5                time: 11
Example 7       NrGreensLClasses:               19               time: 0
Example 7       NrGreensHClasses:               51               time: 3
Example 7       NrIdempotents:                  28               time: 0
Example 7       NrRegularDClasses:              4                time: 0
Example 7       MultiplicativeZero:             fail             time: 1
Example 7       HasMultIdentity:                false            time: 1
Example 7       GroupOfUnitsStructure:          fail             time: 0
Example 7       MinimalIdealSize:               4                time: 3
Example 7       IsBlockGroup:                   false            time: 0
Example 7       IsCliffordSemigroup:            false            time: 1
Example 7       IsCommutativeSemigroup:         false            time: 0
Example 7       IsCompletelyRegularSemigroup:   false            time: 0
Example 7       IsCompletelySimpleSemigroup:    false            time: 0
Example 7       IsGreensHTrivial:               false            time: 0
Example 7       IsGreensLTrivial:               false            time: 0
Example 7       IsGreensRTrivial:               false            time: 0
Example 7       IsGroupAsSemigroup:             false            time: 0
Example 7       IsInverseSemigroup:             false            time: 1
Example 7       IsLeftZeroSemigroup:            false            time: 1
Example 7       IsMonoidAsSemigroup:            false            time: 0
Example 7       IsOrthodoxSemigroup:            false            time: 0
Example 7       IsRectangularBand:              false            time: 0
Example 7       IsRegularSemigroup:             false            time: 0
Example 7       IsRightZeroSemigroup:           false            time: 0
Example 7       IsSemiband:                     false            time: 6
Example 7       IsSemilatticeAsSemigroup:       false            time: 0
Example 7       IsSimpleSemigroup:              false            time: 0
Example 7       IsSynchronizingSemigroup:       true             time: 0
Example 7       IsZeroGroup:                    false            time: 0
Example 7       IsZeroSemigroup:                false            time: 0

gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
>  Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 3, 4, 2, 2 ] ),
>  Transformation( [ 4, 1, 2, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> CitrusTestSuite(s, 8);
Example 8       Size:                           69               time: 9
Example 8       NrGreensRClasses:               17               time: 0
Example 8       NrGreensDClasses:               9                time: 12
Example 8       NrGreensLClasses:               21               time: 0
Example 8       NrGreensHClasses:               41               time: 2
Example 8       NrIdempotents:                  22               time: 0
Example 8       NrRegularDClasses:              6                time: 0
Example 8       MultiplicativeZero:             fail             time: 1
Example 8       HasMultIdentity:                true             time: 0
Example 8       GroupOfUnitsStructure:          1                time: 1
Example 8       MinimalIdealSize:               4                time: 2
Example 8       IsBlockGroup:                   false            time: 0
Example 8       IsCliffordSemigroup:            false            time: 0
Example 8       IsCommutativeSemigroup:         false            time: 0
Example 8       IsCompletelyRegularSemigroup:   false            time: 0
Example 8       IsCompletelySimpleSemigroup:    false            time: 0
Example 8       IsGreensHTrivial:               false            time: 0
Example 8       IsGreensLTrivial:               false            time: 0
Example 8       IsGreensRTrivial:               false            time: 0
Example 8       IsGroupAsSemigroup:             false            time: 0
Example 8       IsInverseSemigroup:             false            time: 1
Example 8       IsLeftZeroSemigroup:            false            time: 0
Example 8       IsMonoidAsSemigroup:            true             time: 0
Example 8       IsOrthodoxSemigroup:            false            time: 0
Example 8       IsRectangularBand:              false            time: 0
Example 8       IsRegularSemigroup:             false            time: 0
Example 8       IsRightZeroSemigroup:           false            time: 0
Example 8       IsSemiband:                     false            time: 5
Example 8       IsSemilatticeAsSemigroup:       false            time: 0
Example 8       IsSimpleSemigroup:              false            time: 0
Example 8       IsSynchronizingSemigroup:       true             time: 0
Example 8       IsZeroGroup:                    false            time: 0
Example 8       IsZeroSemigroup:                false            time: 0
gap> STOP_TEST( "performance.tst 0.1", 0);


