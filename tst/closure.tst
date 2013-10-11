#############################################################################
##
#W  closure.tst
#Y  Copyright (C) 2011-13                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# ReadTest(Filename(DirectoriesPackageLibrary("semigroups","tst"),"closure.tst"));
gap> START_TEST("Semigroups package: closure.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#
gap> gens:=[ Transformation( [ 2, 6, 7, 2, 6, 1, 1, 5 ] ), 
>  Transformation( [ 3, 8, 1, 4, 5, 6, 7, 1 ] ), 
>  Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] ), 
>  Transformation( [ 7, 1, 7, 4, 2, 5, 6, 3 ] ) ];;
gap> s:=Monoid(gens[1]);; Size(s);
5
gap> for i in [2..4] do 
> s:=ClosureSemigroup(s, gens[i]); Size(s);
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
<trivial transformation group>

#
gap> file:=Concatenation(SemigroupsDir(), "/examples/syntactic.semigroups.gz");;
gap> gens:=ReadGenerators(file, 299);;
gap> s:=Semigroup(gens[1]);; 
gap> for i in [2..Length(gens)] do 
> s:=ClosureSemigroup(s, gens[i]); 
> od;
gap> s;
<transformation semigroup on 126 pts with 6 generators>
gap> Size(s);
15853
gap> Size(Semigroup(Generators(s)));
15853
gap> NrRClasses(s);
355
gap> t:=Semigroup(gens);            
<transformation semigroup on 126 pts with 6 generators>
gap> NrRClasses(t);
355
gap> NrLClasses(s);
353
gap> NrLClasses(t);
353

#
gap> file:=Concatenation(SemigroupsDir(), "/examples/syntactic.semigroups.gz");;
gap> gens:=ReadGenerators(file, 299);;
gap> s:=Semigroup(gens[1]);; Size(s);
30
gap> for i in [2..Length(gens)] do 
> s:=ClosureSemigroup(s, gens[i]); Size(s);
> od;
gap> s;
<transformation semigroup of size 15853, on 126 pts with 6 generators>
gap> Size(s);
15853
gap> Size(Semigroup(Generators(s)));
15853
gap> NrRClasses(s);
355
gap> t:=Semigroup(gens);            
<transformation semigroup on 126 pts with 6 generators>
gap> NrRClasses(t);
355
gap> NrLClasses(s);
353
gap> NrLClasses(t);
353

#
gap> file:=Concatenation(SemigroupsDir(), "/examples/path.semigroups.gz");;
gap> gens:=ReadGenerators(file, 10);;
gap> s:=Semigroup(gens[1]);;         
gap> for i in [2..Length(gens)] do
> s:=ClosureSemigroup(s, gens[i]); Size(s);
> od;
gap> s;
<transformation semigroup of size 6996, on 11 pts with 11 generators>
gap> Size(s);
6996
gap> NrRClasses(s); NrLClasses(s); NrDClasses(s); NrIdempotents(s);
512
392
46
423
gap> t:=Semigroup(gens);
<transformation semigroup on 11 pts with 11 generators>
gap> NrRClasses(t); NrLClasses(t); NrDClasses(t); NrIdempotents(t);
512
392
46
423

#
gap> file:=Concatenation(SemigroupsDir(), "/examples/path.semigroups.gz");;
gap> gens:=ReadGenerators(file, 10);;
gap> s:=Semigroup(gens[1]);;        
gap> s:=ClosureSemigroup(s, gens[2]);;
gap> for i in [2..Length(gens)] do
> s:=ClosureSemigroup(s, gens[i]); Size(s);
> od;
gap> s;
<transformation semigroup of size 6996, on 11 pts with 11 generators>
gap> Size(s);
6996
gap> NrRClasses(s); NrLClasses(s); NrDClasses(s); NrIdempotents(s);
512
392
46
423
gap> t:=Semigroup(gens);
<transformation semigroup on 11 pts with 11 generators>
gap> NrRClasses(t); NrLClasses(t); NrDClasses(t); NrIdempotents(t);
512
392
46
423

#
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
>   Transformation( [ 4, 2, 1, 5, 5 ] ),
>   Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Monoid(gens[1], gens[2]);   
<transformation monoid on 5 pts with 2 generators>
gap> s:=ClosureSemigroup(s, gens[3]);
<transformation monoid on 5 pts with 3 generators>
gap> Size(s);
732
gap> IsRegularSemigroup(s);
true
gap> MultiplicativeZero(s);
fail
gap> GroupOfUnits(s);
<trivial transformation group>

#
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
>   Transformation( [ 4, 2, 1, 5, 5 ] ),
>   Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Monoid(gens[1], gens[2]);
<transformation monoid on 5 pts with 2 generators>
gap> s:=ClosureSemigroup(s, gens[3]);
<transformation monoid on 5 pts with 3 generators>
gap> Size(s);
732
gap> IsRegularSemigroup(s);
true
gap> MultiplicativeZero(s);
fail
gap> GroupOfUnits(s);
<trivial transformation group>

#
gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ),
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ),
> Transformation( [ 3, 3, 4, 1 ] ) ];;
gap> s:=Monoid(gens[3]);
<commutative transformation monoid on 4 pts with 1 generator>
gap> for i in [1..Length(gens)] do
> s:=ClosureSemigroup(s, gens[i]); Size(s);
> od;
gap> s;
<transformation monoid of size 62, on 4 pts with 4 generators>
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
<commutative transformation monoid on 4 pts with 1 generator>
gap> for i in [1..Length(gens)] do
> s:=ClosureSemigroup(s, gens[i]); Size(s);
> od;
gap> s;
<transformation monoid of size 62, on 4 pts with 4 generators>
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
<commutative transformation monoid on 4 pts with 1 generator>
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
<commutative transformation monoid on 4 pts with 1 generator>
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
gap> SemigroupsStopTest(); 
gap> STOP_TEST( "Semigroups package: closure.tst", 10000);
