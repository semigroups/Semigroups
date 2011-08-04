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
# around 2.3s.

gap> START_TEST("d.tst 0.1");
gap> LoadPackage("citrus");;
gap> s:=FullTransformationSemigroup(8);;
gap> f:=Transformation([1,1,2,3,4,5,6,7]);;
gap> ExpandOrbitsOfImages(s);
gap> iter:=IteratorOfGreensDClasses(s);
gap> d:=NextIterator(iter);
gap> d:=NextIterator(iter);
gap> r:=GreensRClassOfElement(s, f);
gap> elts:=Elements(r);;
gap> for i in elts do if not i in d then  fi; od;
# approx. 740ms/690ms with 1000m of memory and profiling off 
# 650ms the first time the for loop is done and 600ms thereafter
gap> rr:=GreensRClassOfElement(s, Transformation([1,1,1,2,3,4,5,6]));
gap> elts:=Elements(rr);;
gap> for i in elts do if not i in d then  fi; od;
# approx. 90ms/55ms with 1000m of memory and profiling off 
# 75ms the first time the for loop is done and 50ms thereafter
# (due to storing RankOfTransformation!)
gap> Sum(List(GreensDClasses(s), Size)); 8^8;
gap> iter:=IteratorOfDClassReps(s);
gap> for i in [1..15] do NextIterator(iter); od;
gap> iter2:=IteratorOfRClassReps(s);
gap> for i in [1..500] do NextIterator(iter2); od;
gap>NextIterator(iter!.r);
gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
> Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 3, 4, 2, 2 ] ),
>   Transformation( [ 4, 1, 2, 1 ] ) ];;
gap> s:=Semigroup(gens);
gap> iter:=IteratorOfDClassReps(s);
gap> for i in [1..15] do NextIterator(iter); od;
gap> NextIterator(iter!.r);
gap> s:=Semigroup(gens); 
gap> iter1:=IteratorOfRClassReps(s);
gap> iter2:=IteratorOfRClassReps(s);
gap> NextIterator(iter1); NextIterator(iter2);
gap> s:=OrderPreservingSemigroup(20);
gap> f:=Random(s);
gap> d:=GreensDClassOfElementNC(s, f);
gap> enum:=Enumerator(d);
gap> Length(enum);
#it seems to take a long time to run through the enumerator!
gap> s:=FullTransformationSemigroup(10);;
gap> f:=Transformation( [ 8, 10, 8, 5, 6, 10, 7, 2, 9, 9 ] );
gap> d:=GreensDClassOfElementNC(s, f);; #1s with NC check efficiency here! 
#1.6s without NC
gap> Idempotents(d);; #1.3s
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
#s:=EndomorphismSemigpOfCubeGraph(4);
#f:=Transformation( [ 8, 14, 13, 14, 13, 16, 16, 8, 16, 6, 16, 13, 13, 13, 13, 
# 10 ] );
#d:=GreensDClassOfElementNC(s, f);
# 8s for this!
gap> s:=FullTransformationSemigroup(5);
gap> f:=Transformation( [ 5, 5, 1, 1, 3 ] );
gap> d:=GreensDClassOfElement(s, f);
gap> GreensRClassReps(d);
gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ), 
>  Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ), 
>  Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ), 
>  Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> f:=Transformation( [ 1, 1, 7, 6, 6, 7, 2, 1 ] );
gap> d:=GreensDClassOfElement(s, f);
gap> GreensRClassReps(d);
gap> gens:=[ Transformation( [ 5, 6, 7, 3, 1, 4, 2, 8 ] ), 
> Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] ) ];
gap> s:=Semigroup(gens);
gap> f:=Transformation( [ 4, 8, 8, 7, 6, 2, 5, 8 ] );
gap> r:=GreensRClassOfElement(s, f);
gap> d:=GreensDClass(r);
gap> gens:=[ Transformation( [ 5, 6, 7, 3, 1, 4, 2, 8 ] ), 
>  Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] ) ];
gap> s:=Semigroup(gens);
gap> f:=Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] );
gap> r:=GreensRClassOfElement(s, f);
gap> d:=GreensDClass(r);
gap> R:=GreensRClasses(d);
gap> r:=R[10];
gap> GreensHClassRepsData(r);
gap> STOP_TEST( "d.tst 0.1", 10000);

