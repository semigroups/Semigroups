#############################################################################
##
#W  d.tst
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# ReadTest(Filename(DirectoriesPackageLibrary("citrus","tst"),"d.tst"));
# around 6s.

gap> START_TEST("d.tst 0.1");
gap> LoadPackage("citrus");;
gap> s:=FullTransformationSemigroup(8);;
gap> f:=Transformation([1,1,2,3,4,5,6,7]);;
gap> ExpandOrbitsOfImages(s);
true
gap> iter:=IteratorOfGreensDClasses(s);
<iterator of D-classes>
gap> d:=NextIterator(iter);
{Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8 ] )}
gap> d:=NextIterator(iter);
{Transformation( [ 1, 2, 3, 4, 5, 6, 7, 1 ] )}
gap> r:=GreensRClassOfElement(s, f);
{Transformation( [ 1, 1, 2, 3, 4, 5, 6, 7 ] )}
gap> elts:=Elements(r);;
gap> for i in elts do if not i in d then Print("Error 1");fi; od;
# approx. 740ms/690ms with 1000m of memory and profiling off 
# 650ms the first time the for loop is done and 600ms thereafter
gap> rr:=GreensRClassOfElement(s, Transformation([1,1,1,2,3,4,5,6]));
{Transformation( [ 1, 1, 1, 3, 4, 5, 6, 7 ] )}
gap> elts:=Elements(rr);; d:=DClassOfRClass(rr);;
gap> for i in elts do if not i in d then Print("Error 2");  fi; od;
# approx. 90ms/55ms with 1000m of memory and profiling off 
# 75ms the first time the for loop is done and 50ms thereafter
# (due to storing RankOfTransformation!)
gap> Sum(List(GreensDClasses(s), Size)); 8^8;
16777216
16777216
gap> iter:=IteratorOfDClassReps(s);
<iterator of D-class reps, 4140 candidates, 16777216 elements, 8 D-classes>
gap> for i in [1..15] do NextIterator(iter); od;
gap> iter2:=IteratorOfRClassReps(s);
<iterator of R-class reps, 8163 candidates, 16777216 elements, 4140 R-classes>
gap> for i in [1..500] do NextIterator(iter2); od;
gap> NextIterator(iter!.data!.r);
[ 8, 1, 1, 1, 1, 1 ]
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
> Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 3, 4, 2, 2 ] ),
>   Transformation( [ 4, 1, 2, 1 ] ) ];;
gap> s:=Semigroup(gens);;
gap> iter:=IteratorOfDClassReps(s);
<iterator of D-class reps, 0 candidates, 0 elements, 0 D-classes>
gap> for i in [1..15] do NextIterator(iter); od;
gap> NextIterator(iter!.data!.r);
fail
gap> s:=Semigroup(gens);;
gap> iter1:=IteratorOfRClassReps(s);;
gap> iter2:=IteratorOfRClassReps(s);;
gap> NextIterator(iter1); NextIterator(iter2);
Transformation( [ 1, 3, 2, 3 ] )
Transformation( [ 1, 3, 2, 3 ] )
gap> NextIterator(iter1); NextIterator(iter2);
Transformation( [ 3, 2, 4, 2 ] )
Transformation( [ 3, 2, 4, 2 ] )
#gap> s:=OrderPreservingSemigroup(20);;
#gap> f:=Random(s);
#gap> d:=GreensDClassOfElementNC(s, f);
#gap> enum:=Enumerator(d);
#gap> Length(enum);
#it seems to take a long time to run through the enumerator!
gap> s:=FullTransformationSemigroup(10);;
gap> f:=Transformation( [ 8, 10, 8, 5, 6, 10, 7, 2, 9, 9 ] );;
gap> d:=GreensDClassOfElementNC(s, f);; #1s with NC check efficiency here! 
#1.6s without NC
gap> Idempotents(d);; #1.3s
gap> NrIdempotents(d);
41160
gap> ForAll(Idempotents(d), IsIdempotent);
true
gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ), 
>   Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ), 
>  Transformation( [ 4, 1, 8, 3, 5, 7, 3, 5 ] ), 
>  Transformation( [ 4, 3, 4, 5, 6, 4, 1, 2 ] ), 
>  Transformation( [ 5, 4, 8, 8, 5, 6, 1, 5 ] ), 
>  Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ), 
>  Transformation( [ 7, 1, 2, 2, 2, 7, 4, 5 ] ), 
>  Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> f:=Transformation( [ 5, 2, 7, 2, 7, 2, 5, 8 ] );;
gap> d:=GreensDClassOfElementNC(s, f);;
gap> d:=GreensDClassOfElement(s, f);;
gap> GreensRClassReps(d);;
gap> NrGreensRClasses(d);
260
#s:=EndomorphismSemigpOfCubeGraph(4);
#f:=Transformation( [ 8, 14, 13, 14, 13, 16, 16, 8, 16, 6, 16, 13, 13, 13, 13, 
# 10 ] );
#d:=GreensDClassOfElementNC(s, f);
# 8s for this!
gap> s:=FullTransformationSemigroup(5);;
gap> f:=Transformation( [ 5, 5, 1, 1, 3 ] );;
gap> d:=GreensDClassOfElement(s, f);;
gap> GreensRClassReps(d);
[ Transformation( [ 3, 3, 4, 4, 1 ] ), Transformation( [ 3, 4, 4, 1, 3 ] ), 
  Transformation( [ 4, 4, 1, 3, 3 ] ), Transformation( [ 4, 3, 4, 1, 3 ] ), 
  Transformation( [ 4, 1, 3, 3, 4 ] ), Transformation( [ 4, 4, 1, 3, 4 ] ), 
  Transformation( [ 3, 4, 1, 3, 4 ] ), Transformation( [ 4, 3, 4, 1, 4 ] ), 
  Transformation( [ 1, 3, 3, 4, 4 ] ), Transformation( [ 1, 4, 3, 3, 4 ] ), 
  Transformation( [ 4, 1, 3, 4, 4 ] ), Transformation( [ 4, 1, 3, 4, 3 ] ), 
  Transformation( [ 4, 3, 1, 3, 4 ] ), Transformation( [ 3, 4, 1, 4, 4 ] ), 
  Transformation( [ 3, 4, 4, 1, 4 ] ), Transformation( [ 3, 1, 3, 4, 4 ] ), 
  Transformation( [ 4, 3, 3, 4, 1 ] ), Transformation( [ 1, 3, 4, 4, 4 ] ), 
  Transformation( [ 1, 3, 4, 3, 4 ] ), Transformation( [ 4, 1, 4, 4, 3 ] ), 
  Transformation( [ 4, 4, 1, 4, 3 ] ), Transformation( [ 3, 3, 4, 1, 4 ] ), 
  Transformation( [ 3, 4, 3, 4, 1 ] ), Transformation( [ 3, 4, 4, 4, 1 ] ), 
  Transformation( [ 4, 4, 4, 1, 3 ] ) ]
gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ), 
>  Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ), 
>  Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ), 
>  Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> f:=Transformation( [ 1, 1, 7, 6, 6, 7, 2, 1 ] );;
gap> d:=GreensDClassOfElement(s, f);
{Transformation( [ 1, 1, 7, 6, 6, 7, 2, 1 ] )}
gap> GreensRClassReps(d);;
gap> NrGreensRClasses(d);
1728
gap> gens:=[ Transformation( [ 5, 6, 7, 3, 1, 4, 2, 8 ] ), 
> Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> f:=Transformation( [ 4, 8, 8, 7, 6, 2, 5, 8 ] );;
gap> r:=GreensRClassOfElement(s, f);
{Transformation( [ 6, 8, 8, 7, 4, 2, 5, 8 ] )}
gap> d:=DClassOfRClass(r);
{Transformation( [ 6, 8, 8, 7, 4, 2, 5, 8 ] )}
gap> gens:=[ Transformation( [ 5, 6, 7, 3, 1, 4, 2, 8 ] ), 
>  Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> f:=Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] );;
gap> r:=GreensRClassOfElement(s, f);
{Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] )}
gap> d:=DClassOfRClass(r);
{Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] )}
gap> R:=GreensRClasses(d);
[ {Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] )}, 
  {Transformation( [ 7, 6, 8, 5, 3, 4, 2, 8 ] )}, 
  {Transformation( [ 7, 4, 2, 8, 3, 5, 6, 8 ] )}, 
  {Transformation( [ 3, 4, 2, 8, 7, 5, 6, 8 ] )}, 
  {Transformation( [ 3, 5, 6, 2, 7, 8, 4, 8 ] )}, 
  {Transformation( [ 7, 5, 6, 2, 3, 8, 4, 8 ] )}, 
  {Transformation( [ 7, 8, 4, 6, 3, 2, 5, 8 ] )}, 
  {Transformation( [ 3, 8, 4, 6, 7, 2, 5, 8 ] )}, 
  {Transformation( [ 3, 2, 5, 4, 7, 6, 8, 8 ] )}, 
  {Transformation( [ 7, 2, 5, 4, 3, 6, 8, 8 ] )} ]
gap> r:=R[10];;
gap> GreensHClassRepsData(r);
[ [ [ 7, 1, 1, 1, 5, 2 ], [ 7, 1, 5, 1, 1, 1 ], (), fail ], 
  [ [ 7, 1, 1, 1, 5, 2 ], [ 7, 1, 5, 1, 1, 1 ], (2,3,6,7,4), fail ], 
  [ [ 7, 1, 1, 1, 5, 2 ], [ 7, 1, 5, 1, 1, 1 ], (2,6,4,3,7), fail ], 
  [ [ 7, 1, 1, 1, 5, 2 ], [ 7, 1, 5, 1, 1, 1 ], (2,7,3,4,6), fail ], 
  [ [ 7, 1, 1, 1, 5, 2 ], [ 7, 1, 5, 1, 1, 1 ], (2,4,7,6,3), fail ], 
  [ [ 7, 1, 2, 1, 5, 2 ], [ 7, 1, 5, 1, 1, 1 ], (), fail ], 
  [ [ 7, 1, 2, 1, 5, 2 ], [ 7, 1, 5, 1, 1, 1 ], (2,3,6,7,4), fail ], 
  [ [ 7, 1, 2, 1, 5, 2 ], [ 7, 1, 5, 1, 1, 1 ], (2,6,4,3,7), fail ], 
  [ [ 7, 1, 2, 1, 5, 2 ], [ 7, 1, 5, 1, 1, 1 ], (2,7,3,4,6), fail ], 
  [ [ 7, 1, 2, 1, 5, 2 ], [ 7, 1, 5, 1, 1, 1 ], (2,4,7,6,3), fail ] ]
gap> STOP_TEST( "d.tst 0.1", 10000);

