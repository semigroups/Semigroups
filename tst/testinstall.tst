#############################################################################
##
#W  testinstall.tst
#Y  Copyright (C) 2011-12                               James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# ReadTest(Filename(DirectoriesPackageLibrary("citrus","tst"),"testinstall.tst"));

gap> START_TEST("Citrus package: testinstall.tst");
gap> LoadPackage( "citrus", false );;

# Set info levels
gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoCitrus:=InfoLevel(InfoCitrus);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoCitrus, 0);

# 
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
>  Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 3, 4, 2, 2 ] ),
>  Transformation( [ 4, 1, 2, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> Size(s); NrRClasses(s); NrLClasses(s); NrDClasses(s);
69
17
21
9
gap> NrIdempotents(s); NrRegularDClasses(s); IsRegularSemigroup(s);
22
6
false
gap> f:=Transformation( [ 1, 3, 4, 1 ] );;
gap> f in s;
false
gap> f:=Transformation( [ 1, 1, 3, 1 ] );;
gap> f in s;
true
gap> t:=Semigroup(gens{[1..3]});
<semigroup with 3 generators>
gap> ForAll(t, x-> x in s);
true
gap> Size(t);
60

# Issue 1
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

# Issue 2
gap> s:=Semigroup(Transformation([4,4,4,4]));;
gap> AsList(s);
[ Transformation( [ 4, 4, 4, 4 ] ) ]

# Issue 3
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

# Issue 9
gap> gens:=[ Transformation( [ 1, 2, 3, 9, 5, 11, 7, 8, 9, 10, 11, 12 ] ),
> Transformation( [ 1, 2, 3, 9, 5, 11, 9, 8, 9, 8, 11, 12 ] ), 
> Transformation( [ 1, 2, 5, 7, 8, 11, 9, 12, 9, 12, 11, 10 ] ),
> Transformation( [ 1, 2, 8, 9, 5, 11, 9, 8, 9, 10, 11, 12 ] ), 
> Transformation( [ 1, 2, 8, 11, 12, 7, 11, 8, 11, 10, 9, 12 ] ),
> Transformation( [ 1, 2, 10, 9, 12, 11, 9, 10, 9, 8, 11, 12 ] ),
> Transformation( [ 1, 2, 12, 4, 10, 6, 7, 12, 9, 12, 11, 10 ] ),
> Transformation( [ 1, 5, 3, 11, 5, 9, 11, 8, 11, 10, 9, 12 ] ), 
> Transformation( [ 1, 5, 8, 6, 12, 7, 11, 10, 11, 10, 9, 12 ] ),
> Transformation( [ 1, 5, 8, 11, 12, 7, 11, 8, 11, 10, 9, 12 ] ), 
> Transformation( [ 1, 5, 12, 7, 8, 11, 9, 12, 9, 12, 11, 10 ] ),
> Transformation( [ 1, 8, 3, 9, 5, 11, 9, 8, 9, 10, 11, 12 ] ),
> Transformation( [ 1, 8, 5, 7, 8, 11, 9, 12, 9, 12, 11, 10 ] ), 
> Transformation( [ 1, 12, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] ), 
> Transformation( [ 1, 12, 10, 9, 12, 11, 7, 10, 4, 10, 6, 12 ] ),
> Transformation( [ 2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11 ] ), 
> Transformation( [ 3, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11 ] ),
> Transformation( [ 5, 6, 7, 8, 11, 12, 10, 9, 10, 9, 12, 11 ] ),
> Transformation( [ 5, 6, 11, 8, 7, 12, 10, 11, 10, 11, 12, 9 ] ),
> Transformation( [ 5, 7, 10, 11, 9, 5, 11, 8, 11, 8, 12, 9 ] ),
> Transformation( [ 5, 10, 7, 5, 10, 11, 12, 9, 12, 9, 11, 8 ] ),
> Transformation( [ 7, 3, 11, 9, 8, 5, 9, 11, 9, 11, 5, 3 ] ), 
> Transformation( [ 7, 5, 8, 6, 12, 7, 11, 10, 11, 10, 9, 12 ] ),
> Transformation( [ 7, 12, 11, 10, 5, 9, 10, 11, 8, 11, 9, 12 ] ),
> Transformation( [ 9, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] ), 
> Transformation( [ 9, 11, 8, 5, 11, 9, 12, 3, 5, 8, 9, 11 ] ), 
> Transformation( [ 11, 7, 9, 5, 10, 11, 12, 9, 12, 9, 11, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> NrDClasses(s);
232
gap> Size(s);
11858
gap> NrRClasses(s);
1455
gap> NrLClasses(s);
690
gap> NrHClasses(s);
5356
gap> NrIdempotents(s);
300
gap> Sum(List(GreensDClasses(s), NrRClasses))=NrRClasses(s);
true
gap> ForAll(Concatenation(List(GreensDClasses(s), RClassReps)), 
> x-> x in s);
true
gap> ForAll([1..NrRClasses(s)], i->
> EvaluateWord(Generators(s), TraceRClassRepsTree(s, i))=
> RClassReps(s)[i]);
true

#
gap> gens:=[ Transformation( [ 1, 2, 3, 5, 4, 6, 7, 8 ] ),
>   Transformation( [ 4, 4, 3, 1, 5, 6, 3, 8 ] ),
>   Transformation( [ 3, 6, 1, 7, 3, 4, 8, 3 ] ),
>   Transformation( [ 1, 2, 3, 4, 5, 3, 7, 8 ] ),
>   Transformation( [ 1, 2, 3, 4, 1, 6, 7, 8 ] ),
>   Transformation( [ 8, 8, 3, 4, 5, 7, 6, 1 ] ) ];;
gap> s:=Monoid(gens);
<monoid with 6 generators>
gap> t:=ClosureSemigroup(s, [Transformation( [ 4, 4, 3, 1, 5, 6, 3, 8 ] )]);
<monoid with 6 generators>
gap> Size(t)=Size(Semigroup(Generators(t)));
true

#gap> s:=Semigroup(ReadCitrus("pkg/citrus/examples/graph9c.citrus.gz", 100013));;
gap> s:=Semigroup([ Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] ), 
>  Transformation( [ 1, 2, 3, 4, 5, 6, 7, 9, 8 ] ), 
>  Transformation( [ 7, 2, 8, 4, 5, 6, 1, 9, 8 ] ), 
>  Transformation( [ 5, 5, 3, 4, 1, 6, 7, 8, 9 ] ), 
>  Transformation( [ 5, 7, 3, 4, 1, 6, 7, 8, 9 ] ), 
>  Transformation( [ 1, 2, 8, 6, 5, 4, 7, 9, 8 ] ), 
>  Transformation( [ 1, 8, 6, 2, 7, 8, 8, 9, 5 ] ), 
>  Transformation( [ 1, 2, 3, 8, 8, 7, 7, 9, 5 ] ), 
>  Transformation( [ 1, 2, 3, 1, 8, 7, 7, 5, 9 ] ), 
>  Transformation( [ 7, 7, 2, 7, 8, 8, 9, 5, 1 ] ), 
>  Transformation( [ 7, 2, 5, 2, 8, 8, 1, 9, 5 ] ), 
>  Transformation( [ 7, 2, 8, 1, 8, 7, 1, 9, 5 ] ), 
>  Transformation( [ 1, 1, 4, 8, 9, 9, 8, 5, 7 ] ), 
>  Transformation( [ 1, 1, 1, 2, 5, 5, 7, 8, 9 ] ), 
>  Transformation( [ 1, 2, 1, 1, 8, 7, 7, 5, 9 ] ), 
>  Transformation( [ 1, 2, 8, 8, 8, 2, 7, 9, 5 ] ), 
>  Transformation( [ 7, 2, 7, 1, 8, 8, 1, 5, 9 ] ), 
>  Transformation( [ 8, 8, 2, 8, 5, 5, 9, 7, 1 ] ), 
>  Transformation( [ 1, 2, 1, 1, 5, 5, 7, 8, 9 ] ), 
>  Transformation( [ 5, 5, 4, 5, 8, 8, 9, 7, 1 ] ), 
>  Transformation( [ 1, 2, 8, 8, 8, 1, 7, 9, 5 ] ), 
>  Transformation( [ 7, 2, 7, 2, 5, 5, 1, 8, 9 ] ) ]);;
gap> f:= Transformation( [ 7, 7, 4, 2, 1, 8, 8, 9, 5 ] );;
gap> d:=DClass(s, Transformation( [ 1, 8, 6, 2, 7, 8, 8, 9, 5 ] ));;
gap> l:=LClass(d, f);
{Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] )}
gap> ll:=LClass(s, f);
{Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] )}
gap> l=ll;
true
gap> ll=l;
true
gap> ll<l;
false
gap> l<ll;
false
gap> Elements(l)=Elements(ll);
true
gap> Size(l); Size(ll);
8
8
gap> DClassOfLClass(ll)=DClassOfLClass(l);
true
gap> DClassOfLClass(l)=d;
true
gap> NrHClasses(l); NrHClasses(ll);
4
4
gap> HClassReps(l);
[ Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] ), 
  Transformation( [ 1, 8, 4, 2, 7, 8, 8, 5, 9 ] ), 
  Transformation( [ 7, 7, 4, 2, 1, 8, 8, 9, 5 ] ), 
  Transformation( [ 7, 7, 4, 2, 1, 8, 8, 5, 9 ] ) ]
gap> HClassReps(ll);
[ Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] ), 
  Transformation( [ 1, 8, 4, 2, 7, 8, 8, 5, 9 ] ), 
  Transformation( [ 7, 7, 4, 2, 1, 8, 8, 9, 5 ] ), 
  Transformation( [ 7, 7, 4, 2, 1, 8, 8, 5, 9 ] ) ]
gap> HClassRepsData(l);
[ [ [ 7, 1, 2, 1, 1, 1 ], [ 7, 1, 1, 1, 1, 1 ], (8,9), () ], 
  [ [ 7, 1, 2, 1, 1, 1 ], [ 7, 1, 1, 1, 1, 1 ], (8,9), (5,9) ], 
  [ [ 7, 1, 2, 1, 1, 1 ], [ 7, 1, 2, 1, 1, 1 ], (8,9), () ], 
  [ [ 7, 1, 2, 1, 1, 1 ], [ 7, 1, 2, 1, 1, 1 ], (8,9), (5,9) ] ]
gap> HClassRepsData(ll);
[ [ [ 7, 1, 2, 1, 2, 1 ], [ 7, 1, 1, 1, 1, 1 ], (8,9), () ], 
  [ [ 7, 1, 2, 1, 2, 1 ], [ 7, 1, 1, 1, 1, 1 ], (8,9), (5,9) ], 
  [ [ 7, 1, 2, 1, 2, 1 ], [ 7, 1, 2, 1, 1, 1 ], (8,9), () ], 
  [ [ 7, 1, 2, 1, 2, 1 ], [ 7, 1, 2, 1, 1, 1 ], (8,9), (5,9) ] ]
gap> Idempotents(l);    
[  ]
gap> Idempotents(ll);
[  ]
gap> IsRegularDClass(d);
false
gap> Size(s); 
6982
gap> Set(HClasses(l))=Set(HClasses(ll));
true
gap> LClassRepFromData(s, l!.data);
Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] )
gap> LClassRepFromData(s, ll!.data);
Transformation( [ 1, 8, 4, 2, 7, 8, 8, 9, 5 ] )
gap> SchutzenbergerGroup(l);
Group([ (5,9), (1,7) ])
gap> g:=SchutzenbergerGroup(ll);
Group([ (5,9), (1,7) ])

# IsomorphismTransformationSemigroup/Monoid
gap> IsomorphismTransformationSemigroup(g);
MappingByFunction( Group([ (5,9), (1,7) ]), <semigroup with 
2 generators>, function( x ) ... end )
gap> s:=Range(last);
<semigroup with 2 generators>
gap> IsGroupAsSemigroup(s);
true
gap> Generators(s);
[ Transformation( [ 1, 4, 3, 2 ] ), Transformation( [ 3, 2, 1, 4 ] ) ]
gap> t:=Range(IsomorphismTransformationMonoid(g));
<monoid with 2 generators>
gap> Generators(t);
[ Transformation( [ 1, 4, 3, 2 ] ), Transformation( [ 3, 2, 1, 4 ] ) ]
gap> h:=Range(IsomorphismPermGroup(t));
Group([ (2,4), (1,3) ])
gap> IsomorphismGroups(g, h);
[ (5,9), (1,7) ] -> [ (2,4), (1,3) ]

#
gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;
gap> STOP_TEST( "Citrus package: testinstall.tst", 10000);
