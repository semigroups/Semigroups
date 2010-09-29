

gap> gens:=[ 
>  Transformation( [ 1, 54, 25, 4, 49, 30, 7, 56, 51, 44, 31, 62, 13, 20, 35, 
>      38, 61, 18, 37, 14, 63, 42, 23, 24, 3, 58, 27, 34, 55, 6, 11, 32, 45, 28, 
>      15, 36, 19, 16, 59, 64, 41, 22, 53, 10, 33, 46, 47, 50, 5, 48, 9, 52, 43, 
>      2, 29, 8, 57, 26, 39, 60, 17, 12, 21, 40 ] ), 
>  Transformation( [ 1, 56, 21, 36, 61, 26, 7, 48, 3, 28, 55, 14, 41, 54, 33, 
>      62, 43, 4, 11, 2, 51, 40, 13, 46, 63, 22, 47, 6, 19, 58, 29, 18, 39, 30, 
>      45, 52, 31, 12, 35, 10, 23, 64, 5, 34, 59, 24, 57, 38, 17, 16, 25, 60, 
>      49, 8, 37, 50, 27, 42, 15, 32, 53, 20, 9, 44 ] ), 
>  Transformation( [ 1, 64, 33, 18, 11, 8, 7, 10, 15, 14, 17, 26, 23, 22, 25, 
>      30, 29, 32, 49, 42, 39, 38, 41, 46, 45, 48, 57, 54, 53, 56, 61, 60, 63, 
>      2, 3, 4, 5, 6, 9, 12, 13, 16, 19, 20, 21, 24, 27, 28, 31, 34, 35, 36, 37, 
>      40, 43, 44, 47, 50, 51, 52, 55, 58, 59, 62 ] ), 
>  Transformation( [ 1, 3, 19, 27, 31, 33, 1, 63, 29, 59, 61, 21, 23, 51, 53, 
>      25, 55, 57, 5, 9, 11, 35, 41, 7, 37, 39, 13, 15, 43, 45, 17, 47, 49, 35, 
>      43, 47, 49, 3, 17, 45, 13, 15, 37, 39, 5, 7, 41, 9, 11, 51, 55, 57, 19, 
>      25, 53, 21, 23, 59, 61, 27, 29, 63, 31, 33 ] ), 
>  Transformation( [ 1, 18, 57, 46, 41, 36, 7, 4, 47, 18, 13, 60, 1, 32, 27, 52, 
>      23, 24, 23, 32, 27, 60, 1, 24, 57, 52, 7, 4, 41, 36, 13, 46, 47, 4, 27, 
>      24, 23, 52, 57, 32, 1, 60, 13, 18, 47, 46, 7, 36, 41, 36, 47, 46, 13, 18, 
>      41, 4, 7, 52, 57, 24, 23, 60, 27, 32 ] ) ];;
gap> s:=Semigroup(gens);;
gap> t:=Semigroup(gens);;
gap> DegreeOfTransformationSemigroup(s);
64
gap> f:=Transformation( 
> [ 53, 15, 42, 7, 6, 36, 20, 59, 6, 29, 37, 48, 52, 4, 32, 18, 
> 13, 55, 49, 42, 46, 35, 52, 7, 53, 27, 9, 33, 41, 18, 63, 29, 42, 33, 56, 63,
> 64, 49, 35, 3, 20, 2, 26, 11, 39, 9, 7, 48, 8, 56, 42, 10, 61, 25, 55, 39, 62,
> 21, 34, 57, 44, 14, 14, 53 ] );;
gap> f in s;
false
gap> f in t;
false
gap> GreensRClassOfElement(s, f);
#I  transformation is not an element of the semigroup
fail
gap> f:=Transformation( [ 1, 33, 49, 57, 61, 63, 1, 59, 53, 51, 55, 39, 41, 35,
 37, 45, 43, 47, 11, 15, 17, 3, 13, 7, 5, 9, 23, 25, 19, 21, 29, 27, 31, 3, 19,
 27, 31, 33, 29, 21, 23, 25, 5, 9, 11, 7, 13, 15, 17, 35, 43, 47, 49, 45, 37, 
 39, 41, 51, 55, 57, 53, 59, 61, 63 ] );;
gap> f in t;
true
gap> r1:=GreensRClassOfElement(s, f);;
gap> r2:=GreensRClassOfElement(t, f);;
gap> r1=r2;
true
gap> s:=OrderPreservingSemigroup(12);
<monoid with 22 generators>
gap> f:=Transformation([1,1,2,4,5,5,5,5,6,12,12,12]);;
gap> r:=GreensRClassOfElementNC(s, f);;
#gap> time;
#193
gap> r2:=GreensRClassOfElement(s, f);;
#gap> time;
#735
gap> r=r2;
true
gap> Representative(r) in r2;
true
gap> Representative(r2) in r;
true
gap> gens:=[ Transformation( [ 5, 1, 4, 6, 2, 3 ] ), 
> Transformation( [ 1, 2, 3, 4, 5, 6 ] ), 
> Transformation( [ 4, 6, 3, 4, 2, 5 ] ), 
> Transformation( [ 5, 4, 6, 3, 1, 3 ] ), 
> Transformation( [ 2, 2, 6, 5, 4, 3 ] ), 
> Transformation( [ 3, 5, 5, 1, 2, 4 ] ), 
> Transformation( [ 6, 5, 1, 3, 3, 4 ] ), 
> Transformation( [ 1, 3, 4, 3, 2, 1 ] ) ];;
gap> s:=Semigroup(gens);;
gap> ExpandOrbitsOfImages(s);
true
#gap> time;
#36
gap> Size(s);
43779
gap> NrGreensRClasses(s);
200
gap> iter:=IteratorOfGreensRClasses(s);
<iterator of R-classes>
gap> r:=NextIterator(iter);
{Transformation( [ 1, 2, 3, 4, 5, 6 ] )}
gap> r:=NextIterator(iter);
{Transformation( [ 4, 6, 3, 4, 2, 5 ] )}
gap> r:=NextIterator(iter);
{Transformation( [ 2, 6, 3, 4, 5, 4 ] )}
gap> r:=NextIterator(iter);
{Transformation( [ 2, 2, 6, 5, 4, 3 ] )}
gap> r:=NextIterator(iter);
{Transformation( [ 5, 4, 4, 6, 2, 3 ] )}
gap> r:=NextIterator(iter);
{Transformation( [ 3, 2, 5, 4, 4, 6 ] )}
gap> r:=NextIterator(iter);
{Transformation( [ 1, 3, 4, 3, 2, 1 ] )}
gap> d:=GreensDClass(r);
{Transformation( [ 1, 3, 4, 3, 2, 1 ] )}
#gap> time;
#19
gap> Size(d);
23400
gap> Position(GreensDClasses(s), d);
1
gap> List(GreensRClasses(s), x-> Position(GreensDClasses(s), GreensDClass(x)));
[ 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 4, 3, 1, 1, 1, 3, 1, 4, 3, 1, 1, 1, 4, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 4, 1, 4, 4, 1, 1, 4, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 3, 1, 1, 1, 1, 1, 4, 4, 5, 1, 1, 4, 1, 1, 
  4, 4, 3, 1, 1, 4, 4, 4, 4, 4, 4, 1, 1, 4, 4, 4, 1, 1, 1, 4, 4, 4, 4, 4, 3, 1, 1, 4, 1, 4, 4, 1, 1, 4, 
  4, 1, 4, 1, 4, 4, 4, 4, 1, 4, 4, 4, 4, 1, 4, 4, 4, 4, 1, 4, 4, 4, 5, 1, 4, 4, 4, 4, 4, 4, 1, 4, 4, 5, 
  4, 4, 4, 4, 5, 5, 5, 5, 4, 5, 5, 5, 4, 5, 5, 5, 4, 1, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 1, 4, 1, 1, 
  4, 5, 5, 5, 1, 4, 1, 5, 5, 1, 1, 4, 5, 1, 1, 4, 5, 1, 4, 4, 5, 4, 4, 5, 5, 5, 6, 4, 4, 4 ]
gap> s:=RandomTransformationSemigroup(100,7);
<semigroup with 100 generators>
gap> f:=RandomTransformation(7);
Transformation( [ 1, 5, 3, 2, 7, 6, 7 ] )
gap> f in s;
true
gap> GreensDClassOfElement(s, f);
{Transformation( [ 1, 2, 6, 4, 7, 5, 7 ] )}
gap> time;
19
gap> d:=last2;         
{Transformation( [ 1, 2, 6, 4, 7, 5, 7 ] )}
gap> GreensRClassReps(d);
[ Transformation( [ 1, 2, 6, 4, 7, 5, 7 ] ), Transformation( [ 1, 7, 4, 5, 6, 2, 5 ] ), 
  Transformation( [ 6, 5, 2, 5, 7, 4, 1 ] ), Transformation( [ 7, 5, 6, 2, 1, 4, 5 ] ), 
  Transformation( [ 1, 6, 7, 5, 4, 2, 2 ] ), Transformation( [ 7, 6, 4, 4, 1, 2, 5 ] ), 
  Transformation( [ 6, 4, 2, 5, 4, 7, 1 ] ), Transformation( [ 6, 7, 2, 4, 1, 2, 5 ] ) ]
gap> time;
33
gap> reps:=last2;
[ Transformation( [ 1, 2, 6, 4, 7, 5, 7 ] ), Transformation( [ 1, 7, 4, 5, 6, 2, 5 ] ), 
  Transformation( [ 6, 5, 2, 5, 7, 4, 1 ] ), Transformation( [ 7, 5, 6, 2, 1, 4, 5 ] ), 
  Transformation( [ 1, 6, 7, 5, 4, 2, 2 ] ), Transformation( [ 7, 6, 4, 4, 1, 2, 5 ] ), 
  Transformation( [ 6, 4, 2, 5, 4, 7, 1 ] ), Transformation( [ 6, 7, 2, 4, 1, 2, 5 ] ) ]
gap> List(reps, x-> Position(GreensRClasses(s), GreensRClassOfElement(s, x)));     
[ 1, 11, 33, 63, 6, 22, 24, 15 ]
gap> SmallGeneratingSet(s);
^Cuser interrupt at
MakeImmutable( c );
 called from
iter!.schutz[iter!.at[2]] * iter!.perms[iter!.scc[iter!.at[1]]] ^ -1 called from
iter!.NextIterator( iter ) called from
NextIterator( iter!.r ) called from
iter!.NextIterator( iter ) called from
Length( out ) called from
...
Entering break read-eval-print loop ...
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit
> ;
gap> Size(s);
677391
gap> 7^7;
823543
gap> r:=GreensRClasses(s)[63];
{Transformation( [ 5, 1, 7, 6, 2, 4, 1 ] )}
gap> NrGreensRClasses(s);
2269
gap> Bell(7);
877
gap> Idempotents(r);
[ Transformation( [ 1, 7, 3, 4, 5, 6, 7 ] ), Transformation( [ 1, 2, 3, 4, 5, 6, 2 ] ) ]
gap> last[2] in r;
true
gap> GeneratorsOfSemigroup(s);     
[ Transformation( [ 1, 2, 6, 4, 7, 5, 7 ] ), Transformation( [ 1, 2, 7, 7, 2, 2, 3 ] ), 
  Transformation( [ 1, 3, 6, 7, 4, 6, 6 ] ), Transformation( [ 1, 4, 5, 1, 1, 5, 1 ] ), 
  Transformation( [ 1, 4, 5, 6, 7, 3, 7 ] ), Transformation( [ 1, 5, 2, 7, 3, 6, 6 ] ), 
  Transformation( [ 1, 6, 1, 3, 6, 4, 2 ] ), Transformation( [ 1, 6, 6, 1, 2, 6, 6 ] ), 
  Transformation( [ 1, 6, 6, 3, 3, 7, 1 ] ), Transformation( [ 1, 7, 2, 3, 7, 5, 3 ] ), 
  Transformation( [ 1, 7, 4, 6, 3, 2, 6 ] ), Transformation( [ 1, 7, 6, 4, 1, 2, 7 ] ), 
  Transformation( [ 2, 1, 2, 3, 5, 3, 6 ] ), Transformation( [ 2, 1, 4, 4, 2, 4, 5 ] ), 
  Transformation( [ 2, 1, 6, 4, 5, 6, 7 ] ), Transformation( [ 2, 2, 3, 3, 3, 7, 5 ] ), 
  Transformation( [ 2, 2, 4, 3, 1, 1, 2 ] ), Transformation( [ 2, 3, 1, 7, 2, 3, 4 ] ), 
  Transformation( [ 2, 3, 5, 3, 6, 5, 4 ] ), Transformation( [ 2, 4, 7, 1, 4, 1, 2 ] ), 
  Transformation( [ 2, 4, 7, 1, 4, 4, 1 ] ), Transformation( [ 2, 5, 3, 3, 1, 6, 7 ] ), 
  Transformation( [ 2, 5, 4, 1, 2, 5, 2 ] ), Transformation( [ 2, 5, 7, 4, 5, 3, 1 ] ), 
  Transformation( [ 2, 6, 2, 5, 4, 7, 3 ] ), Transformation( [ 2, 6, 7, 3, 4, 7, 6 ] ), 
  Transformation( [ 2, 7, 6, 4, 5, 4, 3 ] ), Transformation( [ 3, 2, 6, 2, 4, 6, 6 ] ), 
  Transformation( [ 3, 3, 2, 7, 5, 1, 3 ] ), Transformation( [ 3, 3, 7, 6, 6, 1, 3 ] ), 
  Transformation( [ 3, 4, 6, 4, 6, 2, 5 ] ), Transformation( [ 3, 5, 2, 7, 7, 2, 2 ] ), 
  Transformation( [ 3, 5, 3, 7, 2, 7, 6 ] ), Transformation( [ 3, 6, 2, 6, 7, 4, 1 ] ), 
  Transformation( [ 3, 6, 3, 7, 4, 1, 3 ] ), Transformation( [ 3, 7, 6, 7, 4, 2, 5 ] ), 
  Transformation( [ 4, 1, 3, 2, 3, 2, 6 ] ), Transformation( [ 4, 1, 4, 5, 4, 1, 2 ] ), 
  Transformation( [ 4, 1, 6, 1, 2, 5, 6 ] ), Transformation( [ 4, 2, 2, 4, 5, 4, 4 ] ), 
  Transformation( [ 4, 2, 2, 7, 1, 1, 7 ] ), Transformation( [ 4, 3, 3, 6, 7, 5, 3 ] ), 
  Transformation( [ 4, 3, 5, 5, 3, 4, 6 ] ), Transformation( [ 4, 4, 3, 4, 2, 1, 4 ] ), 
  Transformation( [ 4, 4, 4, 7, 4, 5, 6 ] ), Transformation( [ 4, 6, 1, 6, 5, 2, 4 ] ), 
  Transformation( [ 4, 6, 3, 2, 3, 3, 6 ] ), Transformation( [ 4, 7, 1, 2, 2, 1, 5 ] ), 
  Transformation( [ 4, 7, 3, 5, 3, 6, 3 ] ), Transformation( [ 4, 7, 4, 5, 2, 7, 1 ] ), 
  Transformation( [ 4, 7, 6, 2, 6, 5, 7 ] ), Transformation( [ 4, 7, 6, 7, 6, 7, 7 ] ), 
  Transformation( [ 5, 2, 1, 1, 6, 5, 6 ] ), Transformation( [ 5, 2, 2, 5, 7, 6, 1 ] ), 
  Transformation( [ 5, 2, 3, 1, 2, 1, 4 ] ), Transformation( [ 5, 2, 4, 5, 6, 6, 3 ] ), 
  Transformation( [ 5, 2, 4, 5, 7, 3, 4 ] ), Transformation( [ 5, 2, 7, 1, 1, 4, 2 ] ), 
  Transformation( [ 5, 4, 1, 1, 2, 4, 1 ] ), Transformation( [ 5, 4, 3, 1, 4, 3, 6 ] ), 
  Transformation( [ 5, 4, 7, 1, 4, 5, 7 ] ), Transformation( [ 5, 5, 1, 3, 1, 5, 1 ] ), 
  Transformation( [ 5, 5, 3, 3, 4, 6, 1 ] ), Transformation( [ 5, 6, 3, 2, 1, 4, 6 ] ), 
  Transformation( [ 5, 6, 7, 3, 5, 2, 2 ] ), Transformation( [ 6, 1, 2, 2, 4, 6, 6 ] ), 
  Transformation( [ 6, 1, 2, 2, 6, 7, 6 ] ), Transformation( [ 6, 1, 3, 6, 2, 3, 1 ] ), 
  Transformation( [ 6, 1, 6, 6, 5, 5, 3 ] ), Transformation( [ 6, 2, 2, 1, 7, 4, 4 ] ), 
  Transformation( [ 6, 2, 4, 6, 1, 2, 2 ] ), Transformation( [ 6, 2, 5, 1, 4, 5, 6 ] ), 
  Transformation( [ 6, 4, 1, 3, 7, 4, 4 ] ), Transformation( [ 6, 4, 4, 7, 4, 6, 5 ] ), 
  Transformation( [ 6, 4, 6, 5, 7, 1, 4 ] ), Transformation( [ 6, 4, 7, 5, 4, 4, 5 ] ), 
  Transformation( [ 6, 5, 3, 1, 7, 2, 2 ] ), Transformation( [ 6, 5, 6, 6, 2, 2, 3 ] ), 
  Transformation( [ 6, 6, 5, 2, 5, 6, 5 ] ), Transformation( [ 6, 6, 5, 7, 4, 1, 6 ] ), 
  Transformation( [ 6, 7, 3, 5, 4, 5, 6 ] ), Transformation( [ 7, 1, 1, 3, 1, 6, 2 ] ), 
  Transformation( [ 7, 1, 2, 5, 6, 7, 5 ] ), Transformation( [ 7, 2, 5, 3, 5, 5, 3 ] ), 
  Transformation( [ 7, 3, 3, 5, 1, 7, 4 ] ), Transformation( [ 7, 3, 4, 4, 3, 2, 1 ] ), 
  Transformation( [ 7, 3, 5, 4, 1, 5, 7 ] ), Transformation( [ 7, 3, 5, 6, 2, 1, 2 ] ), 
  Transformation( [ 7, 3, 7, 7, 1, 5, 1 ] ), Transformation( [ 7, 4, 3, 1, 7, 4, 3 ] ), 
  Transformation( [ 7, 4, 4, 6, 3, 4, 6 ] ), Transformation( [ 7, 4, 5, 1, 4, 4, 1 ] ), 
  Transformation( [ 7, 5, 1, 2, 2, 5, 3 ] ), Transformation( [ 7, 5, 1, 7, 7, 7, 2 ] ), 
  Transformation( [ 7, 5, 2, 4, 3, 7, 3 ] ), Transformation( [ 7, 5, 5, 4, 6, 6, 5 ] ), 
  Transformation( [ 7, 5, 7, 6, 7, 2, 5 ] ), Transformation( [ 7, 6, 6, 3, 5, 1, 1 ] ), 
  Transformation( [ 7, 6, 7, 3, 6, 6, 4 ] ), Transformation( [ 7, 7, 2, 2, 2, 3, 6 ] ) ]


s:=Semigroup( [Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ), 
  Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ), 
  Transformation( [ 4, 1, 8, 3, 5, 7, 3, 5 ] ), 
  Transformation( [ 4, 3, 4, 5, 6, 4, 1, 2 ] )]);;
ExpandOrbitsOfImages(s);
f:=Transformation([3,3,4,5,5,5,8,8]);;
t:=ClosureSemigroupNC(s, [f]);





