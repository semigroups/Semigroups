#############################################################################
##
#W  closure.tst
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# ReadTest(Filename(DirectoriesPackageLibrary("citrus","tst"),"closure.tst"));

gap> START_TEST("Citrus package: closure.tst");
gap> LoadPackage("citrus", false);;

#
gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoCitrus:=InfoLevel(InfoCitrus);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoCitrus, 0);

#
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
<monoid with 1 generator>

#
gap> file:=Concatenation(CitrusDir(), "/examples/syntactic.citrus.gz");;
gap> gens:=ReadCitrus(file, 299);;
gap> s:=Semigroup(gens[1]);;
gap> for i in [2..Length(gens)] do 
> s:=ClosureSemigroup(s, gens[i]);
> od;
gap> s;
<semigroup with 6 generators>
gap> Size(s);
15853
gap> Size(Semigroup(Generators(s)));
15853
gap> NrRClasses(s);
355
gap> t:=Semigroup(gens);            
<semigroup with 6 generators>
gap> NrRClasses(t);
355
gap> NrLClasses(s);
353
gap> NrLClasses(t);
353

#
gap> file:=Concatenation(CitrusDir(), "/examples/syntactic.citrus.gz");;
gap> gens:=ReadCitrus(file, 299);;
gap> s:=Semigroup(gens[1], rec(schreier:=false));;
gap> for i in [2..Length(gens)] do 
> s:=ClosureSemigroup(s, gens[i]);
> od;
gap> s;
<semigroup with 6 generators>
gap> Size(s);
15853
gap> Size(Semigroup(Generators(s)));
15853
gap> NrRClasses(s);
355
gap> t:=Semigroup(gens);            
<semigroup with 6 generators>
gap> NrRClasses(t);
355
gap> NrLClasses(s);
353
gap> NrLClasses(t);
353

#
gap> file:=Concatenation(CitrusDir(), "/examples/path.citrus.gz");;
gap> gens:=ReadCitrus(file, 10);;
gap> s:=Semigroup(gens[1]);;         
gap> for i in [2..Length(gens)] do
> s:=ClosureSemigroup(s, gens[i]);
> od;
gap> s;
<monoid with 11 generators>
gap> Size(s);
6996
gap> NrRClasses(s); NrLClasses(s); NrDClasses(s); NrIdempotents(s);
512
392
46
423
gap> t:=Semigroup(gens);
<semigroup with 11 generators>
gap> NrRClasses(t); NrLClasses(t); NrDClasses(t); NrIdempotents(t);
512
392
46
423

#
gap> file:=Concatenation(CitrusDir(), "/examples/path.citrus.gz");;
gap> gens:=ReadCitrus(file, 10);;
gap> s:=Semigroup(gens[1]);;        
gap> s:=ClosureSemigroup(s, gens[2], rec(schreier:=false));;
gap> for i in [2..Length(gens)] do
> s:=ClosureSemigroup(s, gens[i]);
> od;
gap> s;
<monoid with 11 generators>
gap> Size(s);
6996
gap> NrRClasses(s); NrLClasses(s); NrDClasses(s); NrIdempotents(s);
512
392
46
423
gap> t:=Semigroup(gens);
<semigroup with 11 generators>
gap> NrRClasses(t); NrLClasses(t); NrDClasses(t); NrIdempotents(t);
512
392
46
423

#
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
>   Transformation( [ 4, 2, 1, 5, 5 ] ),
>   Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Semigroup(gens[1], gens[2]);
<semigroup with 2 generators>
gap> s:=Monoid(gens[1], gens[2]);   
<monoid with 2 generators>
gap> s:=ClosureSemigroup(s, gens[3]);
<monoid with 3 generators>
gap> Size(s);
732
gap> IsRegularSemigroup(s);
true
gap> MultiplicativeZero(s);
fail
gap> GroupOfUnits(s);
<monoid with 1 generator>

#
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
>   Transformation( [ 4, 2, 1, 5, 5 ] ),
>   Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Monoid(gens[1], gens[2], rec(schreier:=false));
<monoid with 2 generators>
gap> s:=ClosureSemigroup(s, gens[3]);
<monoid with 3 generators>
gap> Size(s);
732
gap> IsRegularSemigroup(s);
true
gap> MultiplicativeZero(s);
fail
gap> GroupOfUnits(s);
<monoid with 1 generator>

#
gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ),
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ),
> Transformation( [ 3, 3, 4, 1 ] ) ];;
gap> s:=Monoid(gens[3]);
<monoid with 1 generator>
gap> for i in [1..Length(gens)] do
> s:=ClosureSemigroup(s, gens[i]);
> od;
gap> s;
<monoid with 4 generators>
gap> Size(s);
62
gap> NrRClasses(s); NrLClasses(s); NrDClasses(s); NrIdempotents(s);
10
15
6
20

#
gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ),
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ),
> Transformation( [ 3, 3, 4, 1 ] ) ];;
gap> s:=Monoid(gens[3]);
<monoid with 1 generator>
gap> for i in [1..Length(gens)] do
> s:=ClosureSemigroup(s, gens[i], rec(schreier:=false));
> od;
gap> s;
<monoid with 4 generators>
gap> Size(s);
62
gap> NrRClasses(s); NrLClasses(s); NrDClasses(s); NrIdempotents(s);
10
15
6
20

#
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
>  Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 2, 4, 1, 1 ] ),
>  Transformation( [ 3, 4, 2, 2 ] ) ];;
gap> s:=Monoid(gens[1]);             
<monoid with 1 generator>
gap> s:=Monoid(gens[1]);
<monoid with 1 generator>
gap> for i in [1..Length(gens)] do
> s:=ClosureSemigroup(s, gens[i]);
> od;
gap> Size(s);
115
gap> NrRClasses(s); NrLClasses(s); NrDClasses(s); NrIdempotents(s);
12
20
6
29

#
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
>  Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 3, 4, 2, 2 ] ),
>  Transformation( [ 4, 1, 2, 1 ] ) ];;
gap> s:=Monoid(gens[1]);             
<monoid with 1 generator>
gap> for i in [1..Length(gens)] do
> s:=ClosureSemigroup(s, gens[i]);
> od;
gap> Size(s);
69
gap> NrRClasses(s); NrLClasses(s); NrDClasses(s); NrIdempotents(s);
17
21
9
22

#
gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;
gap> Unbind(s);; Unbind(t);; Unbind(i);; Unbind(gens);;
gap> STOP_TEST( "Citrus package: closure.tst", 10000);
