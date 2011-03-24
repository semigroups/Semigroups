











gap> gens:=[
>   Transformation( [ 5, 23, 27, 8, 21, 49, 36, 33, 4, 44, 3, 49, 48, 18, 10,
>       30, 47, 3, 41, 35, 33, 15, 39, 19, 37, 24, 26, 2, 16, 47, 9, 7, 28, 47,
>       25, 21, 50, 23, 18, 42, 26, 40, 40, 4, 43, 27, 45, 35, 40, 14 ] ),
>   Transformation( [ 42, 50, 5, 37, 17, 35, 39, 35, 21, 9, 32, 15, 21, 17, 38,
>       28, 40, 32, 45, 45, 32, 49, 25, 18, 50, 45, 36, 2, 35, 10, 1, 13, 6,
>       20, 5, 4, 45, 45, 24, 45, 43, 4, 28, 21, 5, 31, 13, 49, 28, 20 ] ) ];;
gap> s:=Semigroup(gens);;
gap> f:=Transformation( [ 39, 6, 6, 17, 25, 17, 39, 28, 28, 5, 6, 17, 4, 25, 
> 32, 25, 32, 6, 4, 6, 28, 28, 32, 17, 17, 5, 17, 39, 17, 32, 5, 25, 6, 32, 39, 
> 25, 28, 6, 25, 39, 17, 17, 17, 28, 17, 6, 6, 6, 17, 39 ] );;
gap> f in s;
true
gap> d:=DClass(s, f);;
gap> g:=Transformation( [ 5, 23, 27, 8, 21, 49, 36, 33, 4, 44, 3, 49, 48, 18,
> 10, 30, 47, 3, 41, 35, 33, 15, 39, 19, 37, 24, 26, 2, 16, 47, 9, 7, 28, 47, 
> 25, 21, 50, 23, 18, 42, 26, 40, 40, 4, 43, 27, 45, 35, 40, 14 ] );;
gap> dd:=DClass(s, g);;
gap> dd=d;
false
gap> dd<d;
true
gap> dd>d;
false
gap> g in dd;
true
gap> f in dd;
false
gap> f in d;
true
gap> f in dd;
false
gap> Size(d);
30683520
gap> Size(dd);
1
gap> NrGreensRClasses(d);
1
gap> NrGreensLClasses(d);
30683520
gap> IsGreensLTrivial(d);
true
gap> IsGreensRTrivial(d);
false
gap> r:=GreensRClasses(d)[1];;
gap> Size(r);
30683520
gap> h:=Transformation( [ 25, 17, 17, 32, 6, 32, 25, 5, 5, 28, 17, 32, 39, 6,
> 4, 6, 4, 17, 39, 17, 5, 5, 4, 32, 32, 28, 32, 25, 32, 4, 28, 6, 17, 4, 25, 6, 
> 5, 17, 6, 25, 32, 32, 32, 5, 32, 17, 17, 17, 32, 25 ] );;
gap> h in r;
true
gap> h in d;
true








gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
>   Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
>   Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
>   Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);
gap> f:=Transformation( [ 7, 1, 2, 2, 2, 4, 7, 7 ] );
gap> g:=Transformation( [ 4, 4, 2, 1, 1, 7, 4, 1 ] );
gap> d:=GreensDClassOfElement(s, f);
gap> g in d;




s:=FullTransformationSemigroup(8);;
f:=Transformation([1,1,2,3,4,5,6,7]);;
ExpandOrbitsOfImages(s);
iter:=IteratorOfGreensDClasses(s);
d:=NextIterator(iter);
d:=NextIterator(iter);
r:=GreensRClassOfElement(s, f);
elts:=Elements(r);;
for i in elts do if not i in d then  fi; od;

# approx. 740ms/690ms with 1000m of memory and profiling off 
# 650ms the first time the for loop is done and 600ms thereafter

rr:=GreensRClassOfElement(s, Transformation([1,1,1,2,3,4,5,6]));
elts:=Elements(rr);;
for i in elts do if not i in d then  fi; od;

# approx. 90ms/55ms with 1000m of memory and profiling off 
# 75ms the first time the for loop is done and 50ms thereafter
# (due to storing RankOfTransformation!)


Sum(List(GreensDClasses(s), Size)); 8^8;

l:=[Transformation( [ 1, 2, 3, 4 ] ), Transformation( [ 1, 3, 2, 3 ] ), 
> Transformation( [ 3, 2, 4, 2 ] ), 
>  Transformation( [ 3, 2, 3, 4 ] ), Transformation( [ 2, 3, 4, 3 ] ), 
> Transformation( [ 1, 3, 1, 3 ] ), 
>  Transformation( [ 4, 2, 4, 2 ] ), Transformation( [ 2, 2, 4, 4 ] ), 
> Transformation( [ 2, 4, 3, 4 ] ), 
>  Transformation( [ 1, 1, 1, 1 ] ) ];;

iter:=IteratorOfDClassReps(s);
for i in [1..15] do NextIterator(iter); od;
iter2:=IteratorOfRClassReps(s);
for i in [1..500] do NextIterator(iter2); od;
NextIterator(iter!.r);

s:=Semigroup(gens);
iter:=IteratorOfDClassReps(s);
for i in [1..15] do NextIterator(iter); od;
NextIterator(iter!.r);

s:=Semigroup(gens); 
iter1:=IteratorOfRClassReps(s);
iter2:=IteratorOfRClassReps(s);
NextIterator(iter1); NextIterator(iter2);

s:=OrderPreservingSemigroup(20);
f:=Random(s);
d:=GreensDClassOfElementNC(s, f);
enum:=Enumerator(d);
Length(enum);
#it seems to take a long time to run through the enumerator!

s:=FullTransformationSemigroup(10);
f:=Transformation( [ 8, 10, 8, 5, 6, 10, 7, 2, 9, 9 ] );
d:=GreensDClassOfElementNC(s, f);; #1s with NC check efficiency here! 
																#1.6s without NC
Idempotents(d);; #1.3s




gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ), 
  Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ), 
  Transformation( [ 4, 1, 8, 3, 5, 7, 3, 5 ] ), 
  Transformation( [ 4, 3, 4, 5, 6, 4, 1, 2 ] ), 
  Transformation( [ 5, 4, 8, 8, 5, 6, 1, 5 ] ), 
  Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ), 
  Transformation( [ 7, 1, 2, 2, 2, 7, 4, 5 ] ), 
  Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
s:=Semigroup(gens);
f:=Transformation( [ 5, 2, 7, 2, 7, 2, 5, 8 ] );;
d:=GreensDClassOfElementNC(s, f);;
d:=GreensDClassOfElement(s, f);;

GreensRClassReps(d);;
NrGreensRClasses(d);


s:=EndomorphismSemigpOfCubeGraph(4);
f:=Transformation( [ 8, 14, 13, 14, 13, 16, 16, 8, 16, 6, 16, 13, 13, 13, 13, 10 ] );

d:=GreensDClassOfElementNC(s, f);
# 8s for this!


s:=FullTransformationSemigroup(5);
f:=Transformation( [ 5, 5, 1, 1, 3 ] );
d:=GreensDClassOfElement(s, f);
GreensRClassReps(d);


gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ), 
  Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ), 
  Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ), 
  Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
s:=Semigroup(gens); 
f:=Transformation( [ 1, 1, 7, 6, 6, 7, 2, 1 ] );
d:=GreensDClassOfElement(s, f);
GreensRClassReps(d);


gens:=[ Transformation( [ 5, 6, 7, 3, 1, 4, 2, 8 ] ), 
  Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] ) ];
s:=Semigroup(gens);
f:=Transformation( [ 4, 8, 8, 7, 6, 2, 5, 8 ] );
r:=GreensRClassOfElement(s, f);
d:=DClassOfRClass(r);

gens:=[ Transformation( [ 5, 6, 7, 3, 1, 4, 2, 8 ] ), 
  Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] ) ];
s:=Semigroup(gens);
f:=Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] );
r:=GreensRClassOfElement(s, f);
d:=DClassOfRClass(r);
R:=GreensRClasses(d);
r:=R[10];
GreensHClassRepsData(r);


  <tr><td class="left"><a href="MT4111/Worksheet12.mw">Worksheet 12</a></td>




