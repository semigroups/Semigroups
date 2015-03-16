#############################################################################
##
#W  closure.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
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

#T# ClosureTest1
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

#T# ClosureTest2
gap> gens:=
> [ Transformation( [ 1, 3, 9, 3, 12, 1, 15, 1, 19, 3, 1, 9, 1, 9, 22, 15, 3, 1,
>      24, 15, 1, 26, 1, 28, 1, 30, 1, 32, 1, 34, 1, 36, 1, 38, 1, 40, 1, 42, 1,
>      44, 1, 46, 1, 48, 1, 50, 1, 52, 1, 54, 1, 56, 1, 58, 1, 60, 1, 62, 1, 64,
>      1, 66, 1, 68, 1, 70, 1, 72, 1, 74, 1, 76, 1, 78, 1, 80, 1, 82, 1, 84, 1,
>       86, 1, 88, 1, 90, 1, 92, 1, 94, 1, 96, 1, 98, 1, 100, 1, 102, 1, 104, 1,
>      106, 1, 108, 1, 110, 1, 112, 1, 114, 1, 116, 1, 118, 1, 120, 1, 122, 1,
>       124, 1, 125, 1, 1, 1, 1 ] ), 
>   Transformation( [ 1, 4, 1, 10, 10, 4, 1, 17, 1, 1, 10, 1, 4, 1, 1, 1, 16, 4,
>      1, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1,
>       4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1,
>       4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1,
>       4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1,
>       4, 1, 4, 1, 4, 1, 5, 1, 4, 1, 1, 1 ] ), 
>   Transformation( [ 1, 5, 5, 11, 1, 1, 5, 1, 5, 11, 1, 5, 1, 5, 5, 11, 11, 1,
>       5, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5,
>       1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5,
>       1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5,
>       1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5,
>       1, 5, 1, 5, 1, 5, 1, 5, 1, 1, 5, 1 ] ), 
>   Transformation( [ 1, 6, 1, 6, 6, 13, 1, 18, 1, 6, 6, 1, 21, 1, 1, 1, 6, 23,
>       1, 1, 25, 1, 27, 1, 29, 1, 31, 1, 33, 1, 35, 1, 37, 1, 39, 1, 41, 1, 43,
>      1, 45, 1, 47, 1, 49, 1, 51, 1, 53, 1, 55, 1, 57, 1, 59, 1, 61, 1, 63, 1,
>       65, 1, 67, 1, 69, 1, 71, 1, 73, 1, 75, 1, 77, 1, 79, 1, 81, 1, 83, 1,
>       85, 1, 87, 1, 89, 1, 91, 1, 93, 1, 95, 1, 97, 1, 99, 1, 101, 1, 103, 1,
>       105, 1, 107, 1, 109, 1, 111, 1, 113, 1, 115, 1, 117, 1, 119, 1, 121, 1,
>       123, 1, 1, 1, 126, 1, 1, 1 ] ), 
>   Transformation( [ 1, 7, 7, 1, 1, 14, 16, 1, 7, 1, 1, 20, 14, 16, 7, 1, 1,
>       14, 7, 1, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7,
>      14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14,
>      7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7,
>       14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7,
>       14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7, 14, 7,
>       14, 7, 14, 7, 7, 14 ] ), 
>   Transformation( [ 1, 8, 8, 1, 1, 1, 8, 1, 8, 1, 1, 8, 1, 8, 8, 1, 1, 1, 8,
>       8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1,
>       8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1,
>       8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1,
>       8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1, 8, 1,
>       8, 1, 8, 1, 8, 1, 8, 1, 8, 8, 1 ] ) ];;
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

#T# ClosureTest3
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

#T# ClosureTest4
gap> gens:=[ Transformation( [ 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 ] ), 
>  Transformation( [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 10 ] ), 
>  Transformation( [ 2, 3, 4, 5, 6, 7, 8, 7, 8, 9, 10 ] ), 
>  Transformation( [ 4, 3, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] ), 
>  Transformation( [ 2, 3, 4, 5, 6, 7, 6, 7, 8, 9, 10 ] ), 
>  Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 7, 8, 9 ] ), 
>  Transformation( [ 4, 3, 2, 1, 2, 3, 4, 5, 6, 7, 8 ] ), 
>  Transformation( [ 6, 5, 4, 3, 2, 3, 4, 5, 6, 7, 8 ] ), 
>  Transformation( [ 1, 2, 3, 4, 5, 6, 5, 4, 5, 6, 7 ] ), 
>  Transformation( [ 4, 5, 6, 7, 8, 7, 6, 7, 8, 9, 10 ] ), 
>  Transformation( [ 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4 ] ) ];;
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

#T# ClosureTest5
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

#T# ClosureTest6
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

#T# ClosureTest7
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

#T# ClosureTest8
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

#T# ClosureTest9
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

#T# ClosureTest10
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

#T# ClosureTest11
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

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(i);
gap> Unbind(s);
gap> Unbind(gens);
gap> Unbind(t);

#E# 
gap> STOP_TEST( "Semigroups package: closure.tst");
