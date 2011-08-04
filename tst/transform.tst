#############################################################################
##
#W  transform.tst
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# ReadTest(Filename(DirectoriesPackageLibrary("citrus","tst"),"transform.tst"));
# under 2s.

gap> START_TEST("transform.tst 0.1");
gap> LoadPackage("citrus");;
gap> gens:=[ Transformation( [ 2, 3, 2, 4, 3 ] ), 
> Transformation( [ 4, 5, 2, 2, 4 ] ), 
> Transformation( [ 4, 3, 2, 1, 4 ] ), Transformation( [ 5, 5, 1, 3, 1 ] ) ];;
gap> S:=Semigroup(gens);;
gap> List(Elements(S), x-> IsRegularTransformation(S, x));
[ true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, false, true, true, true, true, true, true, 
  true, true, false, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, false, false, true, true, true, true, true, false, false, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, false, false, true, true, false, false, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, false, false, true, true, false, false, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, false, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, false, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, false, false, false, false, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, false, false, true, true, false, false, true, true, true, 
  true, true, true, true, true, true, true, true, false, false, true, false, 
  false, true, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true, false, false, true, true, true, true, true, true, 
  true, false, true, true, true, true, true, true, true, true, true, true, 
  true, true, true, true ]
gap> Collected(last);
[ [ true, 357 ], [ false, 31 ] ]
gap> gens:=[ Transformation( [ 3, 4, 4, 2, 5, 4 ] ), 
> Transformation( [ 2, 4, 5, 3, 2, 3 ] ), Transformation( [ 5, 4, 3, 4, 1, 5 ] ),
> Transformation( [ 1, 6, 2, 1, 4, 5 ] ), Transformation( [ 4, 6, 5, 5, 2, 3 ] ), 
> Transformation( [ 1, 3, 6, 2, 1, 5 ] ), Transformation( [ 3, 6, 2, 6, 5, 4 ] ), 
> Transformation( [ 3, 2, 6, 6, 1, 1 ] ) ];;
gap> S:=Semigroup(gens);;
gap> Collected(List(Elements(S), x-> IsRegularTransformation(S, x)));  
[ [ true, 21786 ] ]
#gap> time;
#7986
gap> gens:=[ Transformation( [ 3, 3, 3, 3 ] ), Transformation( [ 2, 4, 2, 4 ] ),
> Transformation( [ 2, 3, 2, 3 ] ), Transformation( [ 4, 1, 4, 3 ] ),
> Transformation( [ 1, 4, 4, 1 ] ), Transformation( [ 2, 2, 3, 1 ] ),
> Transformation( [ 2, 4, 3, 4 ] ), Transformation( [ 2, 2, 1, 2 ] ),
> Transformation( [ 2, 2, 1, 3 ] ), Transformation( [ 1, 2, 2, 3 ] ), 
> Transformation( [ 2, 4, 3, 2 ] ), Transformation( [ 2, 3, 3, 3 ] ) ];;
gap> S:=Monoid(gens);;
gap> Collected(List(Elements(S), x-> IsRegularTransformation(S, x)));
[ [ true, 179 ] ]
#gap> time;
#16
########
gap> IdempotentNC([1,1,2,2,3,3,4,4], [1,3,6,8]);  
Transformation( [ 1, 1, 3, 3, 6, 6, 8, 8 ] )
gap> IdempotentNC([1,2,1,2,1,1,2], [1,7]);
Transformation( [ 1, 7, 1, 7, 1, 1, 7 ] )
########
######## 
gap> Idempotent([1,1,2,2,3,3,4,4], [1,3,6,8]);  
Transformation( [ 1, 1, 3, 3, 6, 6, 8, 8 ] )
gap> Idempotent([1,2,1,2,1,1,2], [1,7]);
Transformation( [ 1, 7, 1, 7, 1, 1, 7 ] )
########
######## 
gap> filt:=Filtered(Elements(FullTransformationSemigroup(5)), x-> 
> RankOfTransformation(x^2)=RankOfTransformation(x));;
gap> Length(filt);
1305
gap> filt:=Filtered(Elements(FullTransformationSemigroup(3)), 
> x-> RankOfTransformation(x^2)=RankOfTransformation(x));
[ Transformation( [ 1, 1, 1 ] ), Transformation( [ 1, 1, 3 ] ),
  Transformation( [ 1, 2, 1 ] ), Transformation( [ 1, 2, 2 ] ),
  Transformation( [ 1, 2, 3 ] ), Transformation( [ 1, 3, 2 ] ),
  Transformation( [ 1, 3, 3 ] ), Transformation( [ 2, 1, 1 ] ),
  Transformation( [ 2, 1, 2 ] ), Transformation( [ 2, 1, 3 ] ),
  Transformation( [ 2, 2, 2 ] ), Transformation( [ 2, 2, 3 ] ),
  Transformation( [ 2, 3, 1 ] ), Transformation( [ 2, 3, 2 ] ),
  Transformation( [ 3, 1, 1 ] ), Transformation( [ 3, 1, 2 ] ),
  Transformation( [ 3, 2, 1 ] ), Transformation( [ 3, 2, 3 ] ),
  Transformation( [ 3, 3, 1 ] ), Transformation( [ 3, 3, 2 ] ),
  Transformation( [ 3, 3, 3 ] ) ]
gap> Length(filt);
21
########
########
gap> perms:=List(filt, AsPermutation);
[ (), (), (), (), (), (2,3), (), (1,2), (1,2), (1,2), (), (), (1,2,3), (2,3), 
  (1,3), (1,3,2), (1,3), (), (1,3), (2,3), () ]
########
########
gap> ker:=List(filt, CanonicalTransSameKernel);
[ [ 1, 1, 1 ], [ 1, 1, 2 ], [ 1, 2, 1 ], [ 1, 2, 2 ], [ 1, 2, 3 ],
  [ 1, 2, 3 ], [ 1, 2, 2 ], [ 1, 2, 2 ], [ 1, 2, 1 ], [ 1, 2, 3 ],
  [ 1, 1, 1 ], [ 1, 1, 2 ], [ 1, 2, 3 ], [ 1, 2, 1 ], [ 1, 2, 2 ],
  [ 1, 2, 3 ], [ 1, 2, 3 ], [ 1, 2, 1 ], [ 1, 1, 2 ], [ 1, 1, 2 ],
  [ 1, 1, 1 ] ]
gap> im:=List(filt, ImageSetOfTransformation);
[ [ 1 ], [ 1, 3 ], [ 1, 2 ], [ 1, 2 ], [ 1, 2, 3 ], [ 1, 2, 3 ], [ 1, 3 ], 
  [ 1, 2 ], [ 1, 2 ], [ 1, 2, 3 ], [ 2 ], [ 2, 3 ], [ 1, 2, 3 ], [ 2, 3 ], 
  [ 1, 3 ], [ 1, 2, 3 ], [ 1, 2, 3 ], [ 2, 3 ], [ 1, 3 ], [ 2, 3 ], [ 3 ] ]
gap> List([1..21], i-> Idempotent(ker[i], im[i])*perms[i]);         
[ Transformation( [ 1, 1, 1 ] ), Transformation( [ 1, 1, 3 ] ), 
  Transformation( [ 1, 2, 1 ] ), Transformation( [ 1, 2, 2 ] ), 
  Transformation( [ 1, 2, 3 ] ), Transformation( [ 1, 3, 2 ] ), 
  Transformation( [ 1, 3, 3 ] ), Transformation( [ 2, 1, 1 ] ), 
  Transformation( [ 2, 1, 2 ] ), Transformation( [ 2, 1, 3 ] ), 
  Transformation( [ 2, 2, 2 ] ), Transformation( [ 2, 2, 3 ] ), 
  Transformation( [ 2, 3, 1 ] ), Transformation( [ 2, 3, 2 ] ),
  Transformation( [ 3, 1, 1 ] ), Transformation( [ 3, 1, 2 ] ),
  Transformation( [ 3, 2, 1 ] ), Transformation( [ 3, 2, 3 ] ),
  Transformation( [ 3, 3, 1 ] ), Transformation( [ 3, 3, 2 ] ),
  Transformation( [ 3, 3, 3 ] ) ]
gap> Set(last)=Set(filt);
true
########
########
gap> g1:=Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] );;
gap> g2:=Transformation( [ 2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7 ] );;
gap> s:=Monoid(g1,g2);;
gap> d:=GreensDClasses(s);;
gap> h:=GroupHClassOfGreensDClass(d[3]);
{Transformation( [ 2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7 ] )}
gap> perm:=IsomorphismPermGroup(h);;
gap> Size(Range(perm))=Size(h);
true
gap> g1:=Transformation([2,1,4,5,6,7,3,2,1]);;
gap> g2:=Transformation([2,1,4,2,1,4,2,1,4]);; 
gap> m18:=Monoid(g1, g2);;
gap> dc:=GreensDClasses(m18)[2];;
gap> RankOfTransformation(Representative(dc));
7
gap> hc:=GroupHClassOfGreensDClass(dc);
{Transformation( [ 2, 1, 4, 5, 6, 7, 3, 2, 1 ] )}
gap> iso:=IsomorphismPermGroup(hc);;
#this uses small generating set so won't return the same group 
#generators every time...
########
########
gap> s:=Semigroup(Elements(hc));;
gap> Size(s);
10
gap> IsGroupAsSemigroup(s);
true
#gap> time;
#3
gap> iso:=IsomorphismPermGroup(s);;
gap> g1:=Transformation([3,3,2,6,2,4,4,6]);;
gap> g2:=Transformation([5,1,7,8,7,5,8,1]);;
gap> m6:=Semigroup(g1,g2);;
gap> dc:=GreensDClasses(m6);;
gap> hc:=GroupHClassOfGreensDClass(dc[1]);
{Transformation( [ 3, 3, 2, 6, 2, 4, 4, 6 ] )}
gap> s:=Semigroup(Elements(hc));;
gap> iso:=IsomorphismPermGroup(s);;
gap> g1:=Transformation([2,2,4,4,5,6]);;
gap> g2:=Transformation([5,3,4,4,6,6]);;
gap> m1:=Monoid(g1,g2);;
gap> IsRegularTransformation(m1, g1);
true
gap> IsRegularTransformation(m1, g2);
false
gap> IsRegularTransformation(FullTransformationSemigroup(6), g2);
true
gap> ker:=CanonicalTransSameKernel(g2*g1);
[ 1, 2, 2, 2, 3, 3 ]
gap> im:=ImageListOfTransformation(g2);
[ 5, 3, 4, 4, 6, 6 ]
gap> IsInjectiveTransOnList(ker, im);
false
gap> Idempotent(ker, [1,2,5]);
Transformation( [ 1, 2, 2, 2, 5, 5 ] )
gap> IdempotentNC([1,1,1,2,2,3,3], [1,5,6]);
Transformation( [ 1, 1, 1, 5, 5, 6, 6 ] )
gap> t:=Transformation([1,2,9,9,9,8,8,8,4]);;
gap> AsPermutation(t);
(4,9)
gap> t*last;
Transformation( [ 1, 2, 4, 4, 4, 8, 8, 8, 9 ] )
gap> AsPermutation(last);
()
gap> x:=Transformation( [ 3, 4, 4, 6, 1, 3, 3, 7, 1 ] );;
gap> IndexPeriodOfTransformation(x);
[ 2, 3 ]
gap> x^2=x^5;
true
gap> t:=Transformation( [ 6, 7, 4, 1, 7, 4, 6, 1, 3, 4 ] );;
gap> SmallestIdempotentPower(t);
3
gap> t:=Transformation( [ 6, 6, 6, 2, 7, 1, 5, 3, 10, 6 ] );;
gap> SmallestIdempotentPower(t);
2
gap> s:=Semigroup([ Transformation( [ 3, 1, 4, 2, 5, 2, 1, 6, 1 ] ), 
> Transformation( [ 5, 7, 8, 8, 7, 5, 9, 1, 9 ] ), 
> Transformation( [ 7, 6, 2, 8, 4, 7, 5, 8, 3 ] ) ]);;
gap> f:=Transformation( [ 3, 1, 4, 2, 5, 2, 1, 6, 1 ] );;
gap> InversesOfTransformationNC(s, f);
[  ]
gap> IsRegularTransformation(s, f);
false
gap> f:=Transformation( [ 1, 9, 7, 5, 5, 1, 9, 5, 1 ] );;
gap> inv:=InversesOfTransformation(s, f);
[ Transformation( [ 1, 5, 1, 2, 5, 1, 3, 2, 2 ] ),
  Transformation( [ 1, 2, 3, 5, 5, 1, 3, 5, 2 ] ),
  Transformation( [ 1, 5, 1, 1, 5, 1, 3, 1, 2 ] ) ]
gap> IsRegularTransformation(s, f);
true
gap> ForAll(inv, g-> f*g*f=f and g*f*g=g);
true
gap> STOP_TEST( "transform.tst 0.1", 10000);
