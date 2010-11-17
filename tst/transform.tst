#############################################################################
##
#W  transform.tst
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

#ReadTest( Filename( DirectoriesPackageLibrary( "monoid", "tst" ), "transform.tst" ) );
# under 2s.

gap> START_TEST("transform.tst 4.0");
gap> LoadPackage("monoid");;
gap> gens:=[ Transformation( [ 2, 3, 2, 4, 3 ] ), Transformation( [ 4, 5, 2, 2, 4 ] ), Transformation( [ 4, 3, 2, 1, 4 ] ), Transformation( [ 5, 5, 1, 3, 1 ] ) ];;
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
gap> gens:=[ Transformation( [ 3, 4, 4, 2, 5, 4 ] ), Transformation( [ 2, 4, 5, 3, 2, 3 ] ), Transformation( [ 5, 4, 3, 4, 1, 5 ] ), Transformation( [ 1, 6, 2, 1, 4, 5 ] ), Transformation( [ 4, 6, 5, 5, 2, 3 ] ), Transformation( [ 1, 3, 6, 2, 1, 5 ] ), Transformation( [ 3, 6, 2, 6, 5, 4 ] ), Transformation( [ 3, 2, 6, 6, 1, 1 ] ) ];;
gap> S:=Semigroup(gens);;
gap> Collected(List(Elements(S), x-> IsRegularTransformation(S, x)));  
[ [ true, 21786 ] ]
#gap> time;
#7986
gap> gens:=[ Transformation( [ 3, 3, 3, 3 ] ), Transformation( [ 2, 4, 2, 4 ] ), Transformation( [ 2, 3, 2, 3 ] ), Transformation( [ 4, 1, 4, 3 ] ),Transformation( [ 1, 4, 4, 1 ] ), Transformation( [ 2, 2, 3, 1 ] ), Transformation( [ 2, 4, 3, 4 ] ), Transformation( [ 2, 2, 1, 2 ] ), Transformation( [ 2, 2, 1, 3 ] ), Transformation( [ 1, 2, 2, 3 ] ), Transformation( [ 2, 4, 3, 2 ] ), Transformation( [ 2, 3, 3, 3 ] ) ];;
gap> S:=Monoid(gens);;
gap> Collected(List(Elements(S), x-> IsRegularTransformation(S, x)));
[ [ true, 179 ] ]
#gap> time;
#16
########
########
gap> IsTransversal([[1,2], [3,4], [5,6], [7,8]], [1,3,7,9]);
false
gap> IsTransversal([[1,2], [3,4], [5,6], [7,8]], [1,3,6,8]); 
true
gap> IsTransversal([[3,4], [5,6], [7,8]], [1,3,6,8]);       
false
gap> IsTransversal([[3,4], [5,6], [7,8]], [3,1,6,8]);
false
gap> IsTransversal([[1,2], [3,4], [5,6], [7,8]], [3,1,6,8]);
true
gap> IsTransversal([ [3,4], [1,2], [5,6], [7,8]], [3,1,6,8]);
true
gap> IsTransversal([ [3,4], [1,2], [5,6], [7,8]], [1,3,6,8]);
true
gap> for i in [1..1000] do 
>  rand:=RandomTransformation(10);
>  IsTransversal(KernelOfTransformation(rand), 
>  ImageSetOfTransformation(rand));
> od;
########
######## 
gap> IdempotentNC([[1,2], [3,4], [5,6], [7,8]], [1,3,6,8]);  
Transformation( [ 1, 1, 3, 3, 6, 6, 8, 8 ] )
gap> IdempotentNC([[1,3,5,6], [2,4,7]], [1,7]);
Transformation( [ 1, 7, 1, 7, 1, 1, 7 ] )
gap> ker:=[ [ 1 ], [ 2 ], [ 3, 7, 20 ], [ 4, 6, 19 ], [ 5 ], [ 8 ], [ 9 ], [ 10, 24 ], [ 11 ], [ 12 ], [ 13, 25 ], [ 14, 15, 18 ], [ 16 ], [ 17 ], [ 21 ], [ 22 ], [ 23 ] ];;
gap> im:=[ 1, 2, 4, 5, 8, 9, 10, 11, 12, 13, 14, 16, 17, 20, 21, 22, 23 ];;
gap> idem:=IdempotentNC(ker, im);;
gap> KernelOfTransformation(idem)=ker;
true
gap> ImageSetOfTransformation(idem)=im; 
true 
########
######## 
gap> Idempotent([[1,2], [3,4], [5,6], [7,8]], [1,3,6,8]);  
Transformation( [ 1, 1, 3, 3, 6, 6, 8, 8 ] )
gap> Idempotent([[1,3,5,6], [2,4,7]], [1,7]);
Transformation( [ 1, 7, 1, 7, 1, 1, 7 ] )
gap> Idempotent(ker,im);
Transformation( [ 1, 2, 20, 4, 5, 4, 20, 8, 9, 10, 11, 12, 13, 14, 14, 16, 
  17, 14, 4, 20, 21, 22, 23, 10, 13 ] )
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
gap> perms:=List(filt, AsPermOfRange);
[ (), (), (), (), (), (2,3), (), (1,2), (1,2), (1,2), (), (), (1,2,3), (2,3), 
  (1,3), (1,3,2), (1,3), (), (1,3), (2,3), () ]
########
########
gap> ker:=List(filt, KernelOfTransformation);
[ [ [ 1, 2, 3 ] ], [ [ 1, 2 ], [ 3 ] ], [ [ 1, 3 ], [ 2 ] ], 
  [ [ 1 ], [ 2, 3 ] ], [ [ 1 ], [ 2 ], [ 3 ] ], [ [ 1 ], [ 2 ], [ 3 ] ], 
  [ [ 1 ], [ 2, 3 ] ], [ [ 1 ], [ 2, 3 ] ], [ [ 1, 3 ], [ 2 ] ], 
  [ [ 1 ], [ 2 ], [ 3 ] ], [ [ 1, 2, 3 ] ], [ [ 1, 2 ], [ 3 ] ], 
  [ [ 1 ], [ 2 ], [ 3 ] ], [ [ 1, 3 ], [ 2 ] ], [ [ 1 ], [ 2, 3 ] ], 
  [ [ 1 ], [ 2 ], [ 3 ] ], [ [ 1 ], [ 2 ], [ 3 ] ], [ [ 1, 3 ], [ 2 ] ], 
  [ [ 1, 2 ], [ 3 ] ], [ [ 1, 2 ], [ 3 ] ], [ [ 1, 2, 3 ] ] ]
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
gap> last=filt;
true
########
########
gap> g1:=Transformation( [ 8, 7, 5, 3, 1, 3, 8, 8 ] );
Transformation( [ 8, 7, 5, 3, 1, 3, 8, 8 ] )
gap> g2:=Transformation( [ 5, 1, 4, 1, 4, 4, 7, 8 ] );
Transformation( [ 5, 1, 4, 1, 4, 4, 7, 8 ] )
gap> m4:=Monoid(g1,g2);
<monoid with 2 generators>
gap> elts:=List(Elements(m4), AsBinaryRelationOnPoints);;
gap> List(elts, AsTransformationNC)=Elements(m4);
true
########
########
gap> a:=Transformation([2,5,1,7,3,7,7]);;
gap> b:=Transformation([3,6,5,7,2,1,7]);;
gap> j1:=Semigroup(a,b);;
gap> elts:=List(Elements(j1), x-> RandomTransformation
> (KernelOfTransformation(x), ImageSetOfTransformation(x)));;
gap> ForAll([1..Size(j1)], i-> KernelOfTransformation(elts[i])
> =KernelOfTransformation(Elements(j1)[i]) and 
> ImageSetOfTransformation(elts[i])=ImageSetOfTransformation(Elements(j1)
> [i]));
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
gap> s:=Semigroup(Elements(hc));
<semigroup with 10 generators>
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
gap> ker:=KernelOfTransformation(g2*g1);
[ [ 1 ], [ 2, 3, 4 ], [ 5, 6 ] ] 
gap> im:=ImageListOfTransformation(g2);
[ 5, 3, 4, 4, 6, 6 ]
gap> IsTransversal(ker, im);
false
gap> IsTransversal([[1,2,3],[4,5],[6,7]], [1,5,6]);
true
gap> Idempotent([[1,2,3],[4,5],[6,7]], [1,5,6]);
Transformation( [ 1, 1, 1, 5, 5, 6, 6 ] )
gap> IdempotentNC([[1,2,3],[4,5],[6,7]], [1,5,6]);
Transformation( [ 1, 1, 1, 5, 5, 6, 6 ] )
gap> t:=Transformation([1,2,9,9,9,8,8,8,4]);;
gap> AsPermOfRange(t);
(4,9)
gap> t*last;
Transformation( [ 1, 2, 4, 4, 4, 8, 8, 8, 9 ] )
gap> AsPermOfRange(last);
()
gap> x:=RandomTransformation([[1,2,3], [4,5], [6,7,8]], [1,2,3]);;
gap> KernelOfTransformation(x)=[[1,2,3], [4,5], [6,7,8]];
true
gap> ImageSetOfTransformation(x)=[1,2,3];
true
gap> x:=Transformation( [ 3, 4, 4, 6, 1, 3, 3, 7, 1 ] );;
gap> IndexPeriodOfTransformation(x);
[ 2, 3 ]
gap> x^2=x^5;
true
gap> mat:=OneMutable(GeneratorsOfGroup(GL(3,3))[1]);;
gap> mat[3][3]:=Z(3)*0;;
#gap> F:=BaseDomain(mat);;
#gap> TransformationActionNC(Elements(F^3), OnRight, mat);
#Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 10, 10, 10, 13, 13, 13, 16, 16, 
#  16, 19, 19, 19, 22, 22, 22, 25, 25, 25 ] )
gap> t:=Transformation( [ 6, 7, 4, 1, 7, 4, 6, 1, 3, 4 ] );;
gap> SmallestIdempotentPower(t);
6
gap> t:=Transformation( [ 6, 6, 6, 2, 7, 1, 5, 3, 10, 6 ] );;
gap> SmallestIdempotentPower(t);
4
gap> ker:=[[1,2,3],[5,6],[8]];;
gap> img:=[1,2,9];;
gap> IsKerImgOfTransformation(ker,img);
false
gap> ker:=[[1,2,3,4],[5,6,7],[8]];;
gap> IsKerImgOfTransformation(ker,img);
false
gap> img:=[1,2,8];;
gap> IsKerImgOfTransformation(ker,img);
true
gap> TransformationByKernelAndImageNC([[1,2,3,4],[5,6,7],[8]],[1,2,8]);
Transformation( [ 1, 1, 1, 1, 2, 2, 2, 8 ] )
gap> TransformationByKernelAndImageNC([[1,6],[2,5],[3,4]], [4,5,6]);
Transformation( [ 4, 5, 6, 6, 5, 4 ] )
gap> AllTransformationsWithKerAndImg([[1,6],[2,5],[3,4]], [4,5,6]);
[ Transformation( [ 4, 5, 6, 6, 5, 4 ] ), 
  Transformation( [ 6, 5, 4, 4, 5, 6 ] ), 
  Transformation( [ 6, 4, 5, 5, 4, 6 ] ), 
  Transformation( [ 4, 6, 5, 5, 6, 4 ] ), 
  Transformation( [ 5, 6, 4, 4, 6, 5 ] ), 
  Transformation( [ 5, 4, 6, 6, 4, 5 ] ) ]
gap> t:=Transformation( [ 4, 2, 2, 1 ] );;
gap> AsBooleanMatrix(t);
[ [ 0, 0, 0, 1 ], [ 0, 1, 0, 0 ], [ 0, 1, 0, 0 ], [ 1, 0, 0, 0 ] ]
gap> t:=(1,4,5);;
gap> AsBooleanMatrix(t);
[ [ 0, 0, 0, 1, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 1 ],
  [ 1, 0, 0, 0, 0 ] ]
gap> AsBooleanMatrix(t,3);
fail
gap> AsBooleanMatrix(t,5);
[ [ 0, 0, 0, 1, 0 ], [ 0, 1, 0, 0, 0 ], [ 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 1 ],
  [ 1, 0, 0, 0, 0 ] ]
gap> AsBooleanMatrix(t,6);
[ [ 0, 0, 0, 1, 0, 0 ], [ 0, 1, 0, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0 ], 
  [ 0, 0, 0, 0, 1, 0 ], [ 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1 ] ]
gap> t:=Transformation( [ 10, 8, 7, 2, 8, 2, 2, 6, 4, 1 ] );;
gap> KerImgOfTransformation(t);
[ [ [ 1 ], [ 2, 5 ], [ 3 ], [ 4, 6, 7 ], [ 8 ], [ 9 ], [ 10 ] ], 
  [ 1, 2, 4, 6, 7, 8, 10 ] ]
gap> RandomTransformation([[1,2,3], [4,5], [6,7,8]], [1,2,3]);;
gap> RandomTransformation([[1,2,3],[5,7],[4,6]]);;
gap> RandomTransformation([[1,2,3],[5,7],[4,6]]);;
gap> RandomTransformationNC([[1,2,3],[5,7],[4,6]]);;
gap> RandomTransformation([1,2,3], 6);;
gap> RandomTransformationNC([1,2,3], 6);;
gap> RandomTransformation([[1,2,3], [4,5], [6,7,8]], [1,2,3]);;
gap> RandomIdempotent([1,2,3],5);;
gap> RandomIdempotent([[1,6], [2,4], [3,5]]);;
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
gap> STOP_TEST( "transform.tst 4.0", 10000);