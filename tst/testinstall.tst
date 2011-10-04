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
gap> STOP_TEST( "testinstall.tst 0.2", 0);

