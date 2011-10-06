#############################################################################
##
#W  testinstall.tst
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# ReadTest(Filename(DirectoriesPackageLibrary("citrus","tst"),"testinstall.tst"));

gap> START_TEST("testinstall.tst 0.2");
gap> LoadPackage( "citrus" );;
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
>  Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 3, 4, 2, 2 ] ),
>  Transformation( [ 4, 1, 2, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> Size(s); NrGreensRClasses(s); NrGreensLClasses(s); NrGreensDClasses(s);
69
17
21
9
gap> NrIdempotents(s);
22
gap> NrIdempotents(s); NrRegularDClasses(s); IsRegularSemigroup(s);
22
6
false
gap> f:=RandomTransformation(4);
Transformation( [ 1, 3, 4, 1 ] )
gap> f in s;
false
gap> f:=Random(s);
Transformation( [ 1, 1, 3, 1 ] )
gap> f in s;
true
gap> t:=Semigroup(gens{[1..3]});
<semigroup with 3 generators>
gap> ForAll(t, x-> x in s);
true
gap> Size(t);
60
gap> f:=
> Transformation( [ 74, 33, 77, 60, 65, 37, 24, 22, 16, 49, 58, 16, 62, 7, 69, 
>  38, 97, 44, 56, 5, 3, 74, 89, 28, 95, 94, 56, 6, 38, 58, 45, 63, 32, 32, 
>  38, 27, 36, 28, 81, 41, 85, 95, 55, 19, 58, 16, 65, 55, 61, 87, 40, 37, 89, 
>  47, 48, 42, 82, 37, 34, 25, 26, 19, 44, 13, 15, 27, 41, 99, 15, 69, 8, 19, 
>  85, 8, 96, 8, 69, 97, 31, 22, 71, 39, 91, 13, 76, 53, 37, 78, 27, 91, 46, 
>  32, 64, 70, 84, 92, 37, 68, 10, 68 ] );;
gap> IndexPeriodOfTransformation(f);
[ 10, 42 ]
gap> f:=
> Transformation( [ 45, 51, 70, 26, 87, 94, 23, 19, 86, 46, 45, 51, 57, 13, 67, 
>  5, 38, 20, 51, 25, 67, 91, 38, 29, 43, 44, 84, 71, 11, 39, 52, 40, 12, 58, 
>  1, 83, 9, 27, 1, 25, 86, 83, 15, 38, 86, 61, 43, 16, 55, 16, 96, 46, 46, 
>  70, 29, 11, 13, 8, 14, 67, 84, 17, 79, 44, 59, 19, 35, 19, 61, 49, 32, 24, 
>  45, 71, 2, 90, 12, 4, 43, 61, 63, 64, 34, 92, 77, 19, 8, 23, 85, 26, 87, 8, 
>  76, 18, 48, 33, 8, 7, 38, 39 ] );;
gap> IndexPeriodOfTransformation(f);
[ 13, 4 ]
gap> f:=
> Transformation( [ 14, 24, 70, 1, 50, 72, 13, 64, 65, 68, 54, 20, 69, 32, 88, 
>  60, 93, 100, 37, 27, 15, 7, 84, 95, 84, 36, 8, 20, 90, 55, 78, 48, 93, 10, 
>  51, 76, 26, 83, 29, 39, 93, 48, 51, 93, 50, 92, 95, 51, 31, 17, 76, 43, 5, 
>  19, 94, 11, 70, 84, 22, 95, 5, 44, 44, 6, 7, 56, 4, 57, 94, 100, 86, 30, 
>  38, 80, 77, 60, 45, 99, 38, 11, 60, 62, 76, 50, 13, 48, 27, 82, 68, 99, 17, 
>  81, 16, 3, 14, 90, 22, 71, 41, 98 ] );;
gap> IndexPeriodOfTransformation(f);
[ 16, 7 ]
gap> s:=Semigroup(Transformation([4,4,4,4]));
<semigroup with 1 generator>
gap> AsList(s);
[ Transformation( [ 4, 4, 4, 4 ] ) ]
gap> f:=Transformation( [ 7, 5, 3, 2, 6, 7, 10, 8, 8, 3 ] );;
gap> AsPermutation(f, [1,2]);
fail
gap> s:=Semigroup(Transformation( [ 3, 5, 5, 5, 3 ] ), 
> Transformation( [ 5, 5, 2, 1, 5 ] ));;
gap> f:=Transformation( [ 3, 3, 5, 3, 3 ] );;
gap> IsRegularTransformation(s, f);
true
gap> f:=Transformation( [ 5, 5, 5, 5, 5 ] );;
gap> IsRegularTransformation(s, f);
true
gap> f:=Transformation( [ 3, 5, 5, 5, 3 ] );;
gap> IsRegularTransformation(s, f);
true
gap> IsRegularSemigroup(s);
false
gap> f:=Transformation( [ 5, 5, 2, 1, 5 ] );;
gap> IsRegularTransformation(s, f);
false
gap> STOP_TEST( "testinstall.tst 0.2", 0);
