#############################################################################
##
#W  testcompiled.tst
#Y  Copyright (C) 2011-12                               James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# ReadTest(Filename(DirectoriesPackageLibrary("citrus","tst"),
# "testcompiled.tst"));

gap> START_TEST("Citrus package: testcompiled.tst");
gap> LoadPackage( "citrus", false );;

#
gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoCitrus:=InfoLevel(InfoCitrus);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoCitrus, 0);

#
gap> file:=Concatenation(CitrusDir(), "/examples/munn.citrus.gz");;
gap>  ReadCitrus(file, 1376);
[ <identity on [ 1 .. 9 ]>, <identity on [ 1, 2, 3, 4, 5, 6, 7, 9 ]>, 
  <identity on [ 1, 2, 3, 4, 5, 6, 9 ]>, <identity on [ 1, 2, 3, 4, 5, 9 ]>, 
  <identity on [ 1, 2, 3, 4, 9 ]>, <identity on [ 1, 2, 3, 9 ]>, 
  <identity on [ 1, 2, 9 ]>, <identity on [ 1, 9 ]> ]

#
gap> s:=InverseSemigroup( 
> [ PartialPermNC( [ 1, 2 ], [ 3, 1 ] ),
>   PartialPermNC( [ 1, 2, 3 ], [ 1, 3, 4 ] ),
>   PartialPermNC( [ 1, 2, 3 ], [ 2, 4, 1 ] ),
>   PartialPermNC( [ 1, 3, 4 ], [ 3, 4, 1 ] ) ]);;
gap> Size(s); NrRClasses(s); NrLClasses(s); NrDClasses(s);
116
14
14
4
gap> NrIdempotents(s); NrRegularDClasses(s); IsRegularSemigroup(s);
14
4
true
gap> ForAll(s, x-> x in s);
true
gap> t:=InverseSemigroup(Generators(s){[1..3]});
<inverse semigroup with 3 generators>
gap> ForAll(t, x-> x in s);
true
gap> Size(t);
98

#
gap> s:=InverseSemigroup(
> [ PartialPermNC( [ 1, 3, 5, 6, 7 ], [ 9, 1, 5, 3, 8 ] ),
> PartialPermNC( [ 1, 2, 3, 5, 6, 7, 9, 10 ], [ 4, 10, 5, 6, 7, 1, 3, 2 ] ) ]);;
gap> f:=PartialPermNC( [ 3, 4, 5, 6 ], [ 1, 3, 6, 5 ] );;
gap> d:=DClass(s, f);;
gap> F:=InjectionPrincipalFactor(d);; G:=InverseGeneralMapping(F);;
gap> (f^F)^G=f;
true
gap> ForAll(d, f-> (f^F)^G=f);
true
gap> s:=InverseSemigroup(
> [ PartialPermNC( [ 1, 3, 5, 6, 7 ], [ 9, 1, 5, 3, 8 ] ),
> PartialPermNC( [ 1, 2, 3, 5, 6, 7, 9, 10 ], [ 4, 10, 5, 6, 7, 1, 3, 2 ] ) ]);;
gap> d:=DClasses(s)[14];
{<identity on [ 2, 10 ]>}
gap> F:=IsomorphismReesMatrixSemigroup(d);;
gap> G:=InverseGeneralMapping(F);;
gap> ForAll(d, f-> (f^F)^G=f);        
true

#
gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;
gap> STOP_TEST( "Citrus package: testcompiled.tst", 10000);
