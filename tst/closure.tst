#############################################################################
##
#W  closure.tst
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# ReadTest(Filename(DirectoriesPackageLibrary("citrus","tst"),"closure.tst"));

gap> START_TEST("Citrus package: closure.tst");
gap> LoadPackage("citrus", false);;

gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoCitrus:=InfoLevel(InfoCitrus);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoCitrus, 0);

gap> gens:=[ Transformation( [ 2, 6, 7, 2, 6, 1, 1, 5 ] ), 
>  Transformation( [ 3, 8, 1, 4, 5, 6, 7, 1 ] ), 
>  Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] ), 
>  Transformation( [ 7, 1, 7, 4, 2, 5, 6, 3 ] ) ];;
gap> s:=Monoid(gens[1]);;
gap> for i in [2..4] do 
> s:=ClosureSemigroup(s, gens[i], rec(schreier:=false)); 
> od;
gap> Size(s);
233606
gap> NrRClasses(s);
4397
gap> NrLClasses(s);
16915
gap> NrDClasses(s);
662
gap> GroupOfUnits(s);
Group(())

gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;
gap> Unbind(s);; Unbind(gens);;

gap> STOP_TEST( "Citrus package: closure.tst", 10000);

