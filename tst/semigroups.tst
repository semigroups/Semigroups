#############################################################################
##
#W  semigroups.tst
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

#ReadTest( Filename(  DirectoriesPackageLibrary( "monoid", "tst" ), "semigroups.tst" ) );

gap> START_TEST("semigroups.tst 3.1.4");
gap> LoadPackage("monoid");;
gap> if not IsBound(BruteForceZeroIsoCheck) then 
> BruteForceZeroIsoCheck:=function(iso)
> local x, y;
> 
> if not IsInjective(iso) then 
>   return fail;
> fi;
> 
> #homomorphism
> for x in Source(iso) do 
>   for y in Source(iso) do
>     if x*y in Source(iso) then 
>       if not ImageElm(iso, x)*ImageElm(iso, y)=ImageElm(iso, x*y) then 
>         return [x,y];
>       fi;
>     elif not ImageElm(iso, x*y)=MultiplicativeZero(Range(iso)) then 
>         return fail;
>     fi;  
>   od;
> od;
> return true;
> 
> end;;
> fi;
gap> z:=ZeroSemigroup(19);
<semigroup with 18 generators>
gap> Size(z);
19
gap> Elements(z);
[ 0, z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12, z13, z14, z15, z16,
  z17, z18 ]
gap> GeneratorsOfSemigroup(z);
[ z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12, z13, z14, z15, z16, z17,
  z18 ]
gap> ForAll(Elements(z), x-> ForAll(Elements(z), y-> x*y=
> MultiplicativeZero(z)));
true
gap> MultiplicativeZero(z);
0
gap> last=0;
false
gap> ZeroSemigroupElt(3);
z3
gap> last in z;
true
gap> ZeroSemigroupElt(20) in z;
false
gap> ZeroSemigroupElt(19) in z;
false
gap> ZeroSemigroupElt(18) in z;
true
gap> 0 in z;
false
gap> ZeroSemigroupElt(0) in z;
true
gap> ZeroSemigroupElt(0)=MultiplicativeZero(z);
true
gap> ZeroSemigroupElt(19)*Elements(z)[3];
0
gap> MultiplicativeZero(Elements(z)[3]);
0
gap> last=MultiplicativeZero(z);
true
gap> List(Elements(z), IsMultiplicativeZero);
[ true, false, false, false, false, false, false, false, false, false, false,
  false, false, false, false, false, false, false, false ]
gap> z25:=ZeroSemigroup(25);
<semigroup with 24 generators>
gap> Elements(z25)[4] in z;
true
gap> Elements(z25)[4]=Elements(z)[4];
true
gap> z25=z;
false
gap> Elements(z25)[4]<Elements(z)[4];
false
gap> Elements(z25)[5]<Elements(z)[4];
false
gap> Elements(z25)[5]<Elements(z)[14];
true
gap> Representative(z);
z1
gap> last<MultiplicativeZero(z);
false
gap> MultiplicativeZeroOp(Elements(z)[3]);
0
###########
###########
gap> G:=PrimitiveGroup(5,3);;
gap> ZG:=ZeroGroup(G);
<zero group with 3 generators>
gap> Size(G);
20
gap> Size(ZG);
21
gap> Elements(ZG);
[ 0, (), (2,3,4,5), (2,4)(3,5), (2,5,4,3), (1,2)(3,4), (1,2,3,5,4), 
  (1,2,4,5), (1,2,5,3), (1,3,5,2), (1,3)(4,5), (1,3,2,4), (1,3,4,2,5), 
  (1,4,5,3,2), (1,4,3,5), (1,4,2,3), (1,4)(2,5), (1,5,4,2), (1,5,3,4), 
  (1,5)(2,3), (1,5,2,4,3) ]
gap> elt:=last[2];
()
gap> elt in G;
false
gap> elt in ZG;
true
gap> UnderlyingGroupEltOfZGElt(elt) in G;
true
gap> UnderlyingGroupEltOfZGElt(elt) in ZG;
false
gap> elts:=List(Elements(G), ZeroGroupElt);;
gap> ForAll(elts, x-> x in ZG);
true
gap> SmallGeneratingSet(ZG);   
[ (2,3,4,5), (1,2,3,5,4), 0 ]
gap> ForAll(Elements(ZG){[2..21]}, x->ForAll(Elements(ZG){[2..21]}, y-> 
> UnderlyingGroupEltOfZGElt(x*y)=UnderlyingGroupEltOfZGElt(x)
> *UnderlyingGroupEltOfZGElt(y)));
true
gap> ForAll(Elements(ZG), x-> MultiplicativeZero(ZG)<x);                                                                                                           
false
gap> ForAll(Elements(ZG), x-> MultiplicativeZero(ZG)<=x);
true
gap> One(ZG);
()
gap> UnderlyingGroupEltOfZGElt(One(ZG))=One(G);
true
gap> IsOne(One(ZG)); 
true
gap> ForAll(Elements(ZG), x-> x=x*One(ZG) and x=One(ZG)*x);
true
gap> zg:=Elements(ZG)[14];
(1,4,5,3,2)
gap> MultiplicativeZeroOp(zg);
0
gap> MultiplicativeZeroOp(zg)=MultiplicativeZero(ZG);
true
gap> zg^-1*zg=One(ZG);
true
gap> UnderlyingGroupEltOfZGElt(zg^-1)=UnderlyingGroupEltOfZGElt(zg)^-1;
true
gap> List(Elements(ZG), IsMultiplicativeZero);
[ true, false, false, false, false, false, false, false, false, false, false,
  false, false, false, false, false, false, false, false, false, false ]
###########
###########
gap> g1:=Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] );;
gap> g2:=Transformation( [ 2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7 ] );;
gap> m10:=Monoid(g1,g2);;
gap> g1:=Transformation( [ 10, 8, 7, 4, 1, 4, 10, 10, 7, 2 ] );;
gap> g2:=Transformation( [ 5, 2, 5, 5, 9, 10, 8, 3, 8, 10 ] );;
gap> m12:=Monoid(g1,g2);;
gap> g1:=Transformation([2,1,4,5,3,7,8,9,10,6]);;
gap> g2:=Transformation([1,2,4,3,5,6,7,8,9,10]);;
gap> g3:=Transformation([1,2,3,4,5,6,10,9,8,7]);;
gap> g4:=Transformation([9,1,4,3,6,9,3,4,3,9]);;
gap> m13:=Monoid(g1,g2,g3,g4);;
gap> g1:=Transformation( [ 13, 10, 9, 5, 1, 5, 13, 13, 8, 2, 7, 2, 6 ] );;
gap> g2:=Transformation( [ 6, 11, 12, 10, 4, 10, 13, 5, 8, 5, 11, 6, 9 ] );;       
gap> m16:=Semigroup(g1, g2);;
gap> g1:=Transformation( [ 12, 10, 8, 5, 1, 5, 12, 12, 8, 2, 6, 2 ] );;
gap> g2:=Transformation( [ 5, 6, 10, 11, 10, 4, 10, 12, 5, 7, 4, 10 ] );;
gap> g3:=Transformation( [ 6, 8, 12, 5, 4, 8, 10, 7, 4, 1, 10, 11 ] );;
gap> m17:=Monoid(g1,g2,g3);;
gap> g1:=Transformation([2,3,4,5,1,8,7,6,2,7]);;
gap> g2:=Transformation([5,4,1,2,3,7,6,5,4,1]);;
gap> g3:=Transformation([2,1,4,3,2,1,4,4,3,3]);;
gap> m19:=Monoid(g1,g2,g3);;
gap> g1:=Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] );;
gap> g2:=Transformation([2,3,4,5,6,8,7,1,2,2]);;
gap> m20:=Monoid(g1,g2);;
gap> g1:=Transformation([2,3,4,5,1,8,7,6,2,7]);;
gap> g2:=Transformation( [ 3, 8, 7, 4, 1, 4, 3, 3, 7, 2 ] );;
gap> m21:=Monoid(g1,g2);;
gap> BigMonoids:=[m10, m12, m13, m16, m17, m19, m20, m21];;
gap> g1:=Transformation([2,2,4,4,5,6]);;
gap> g2:=Transformation([5,3,4,4,6,6]);;
gap> m1:=Monoid(g1,g2);;
gap> g1:=Transformation([5,4,4,2,1]);;
gap> g2:=Transformation([2,5,5,4,1]);;
gap> m2:=Monoid(g1,g2);;
gap> g1:=Transformation([1,2,1,3,3]);;
gap> g2:=Transformation([2,2,3,5,5]);;
gap> m3:=Monoid(g1,g2);;
gap> g1:=Transformation( [ 8, 7, 5, 3, 1, 3, 8, 8 ] );;
gap> g2:=Transformation( [ 5, 1, 4, 1, 4, 4, 7, 8 ] );;
gap> m4:=Monoid(g1,g2);;
gap> g1:=Transformation([3,1,2,3,2,3,2,3]);;
gap> g2:=Transformation([2,5,8,5,2,5,7,8]);;
gap> m5:=Monoid(g1,g2);;
gap> g1:=Transformation([3,3,2,6,2,4,4,6]);;
gap> g2:=Transformation([5,1,7,8,7,5,8,1]);;
gap> m6:=Semigroup(g1,g2);;
gap> g1:=Transformation([3,3,2,6,2,4,4,6,3,4,6]);;
gap> g2:=Transformation([4,4,6,1,3,3,3,3,11,11,11]);;
gap> m7:=Monoid(g1,g2);; #(this is a good example!)
gap> g1:=Transformation([3,3,2,6,2,4,4,6,3,4,6]);;
gap> g2:=Transformation([4,4,6,1,3,3,3,3,11,11,11]);;
gap> g3:=Transformation([2,2,3,4,4,6,6,6,6,6,11]);;
gap> m8:=Monoid(g1,g2,g3);;
gap> g1:=Transformation([3,3,2,6,2,4,4,6,3,4,6]);;
gap> g2:=Transformation([4,4,6,1,3,3,3,3,11,11,11]);;
gap> g3:=Transformation([2,2,3,4,4,6,6,6,6,6,11]);;
gap> g4:=Transformation([2,2,3,4,4,6,6,6,6,11,11]);;
gap> m9:=Monoid(g1, g2, g3, g4);;
gap> g1:=Transformation( [ 12, 3, 6, 4, 6, 11, 9, 6, 6, 7, 6, 12 ] );;
gap> g2:=Transformation( [ 10, 7, 2, 11, 7, 3, 12, 4, 3, 8, 7, 5 ] );;
gap> m11:=Monoid(g1,g2);;
gap> g1:=Transformation( [ 3, 2, 12, 2, 7, 9, 4, 2, 1, 12, 11, 12 ] );; 
gap> g2:=Transformation( [ 3, 6, 12, 7, 2, 2, 3, 6, 1, 7, 11, 1 ] );;
gap> m14:=Monoid(g1, g2);;
gap> g1:=Transformation([2,2,3,4,5,6]);;
gap> g2:=Transformation([2,3,4,5,6,1]);; 
gap> m15:=Monoid(g1, g2);;
gap> g1:=Transformation([2,1,4,5,6,7,3,2,1]);;
gap> g2:=Transformation([2,1,4,2,1,4,2,1,4]);;  
gap> m18:=Monoid(g1, g2);;
gap> g1:=Transformation( [ 5, 2, 5, 5, 8, 10, 8, 5, 2, 10 ] );;
gap> g2:=Transformation( [ 2,2,5,5,5,8,8,8,8,8]);;          
gap> m22:=Monoid(g1,g2);; 
gap> g1:=Transformation( [ 4, 6, 3, 8, 5, 6, 10, 4, 3, 7 ] );;
gap> g2:=Transformation( [ 5, 6, 6, 3, 8, 6, 3, 7, 8, 4 ] );;
gap> g3:=Transformation( [ 8, 6, 3, 2, 8, 10, 9, 2, 6, 2 ] );;
gap> m23:=Monoid(g1,g2,g3);;
gap> SmallMonoids:=[m1, m2, m3, m4, m5, m6, m7, m8, m9, m11, m14, m15, m18, m22, m23];;
gap> List(SmallMonoids, x-> List(GreensDClasses(x),
> IsomorphismReesMatrixSemigroupOfDClass));;
gap> ForAll(Random(last), x->BruteForceZeroIsoCheck(x)=true);
true
###########
###########
gap> op5:=OrderPreservingSemigroup(5);
<monoid with 8 generators>
gap> ForAll(Elements(op5), x-> ForAll(Combinations([1..5], 2), y-> 
> y[1]^x<=y[2]^x));
true
gap> n:=Random([1..7]);;
gap> op:=OrderPreservingSemigroup(n);;
gap> Size(op)=Binomial(2*n-1, n-1);
true
gap> IsRegularSemigroup(op);
true
gap> IsOrthodoxSemigroup(op);
false
gap> Size(Idempotents(op))=Fibonacci(2*n);
true
###########
###########
gap> S:=SingularSemigroup(6);
<semigroup with 30 generators>
gap> Size(S);
45936
gap> IsRegularSemigroup(S);
true
gap> IsSemiBand(S);
true
gap> S:=ZeroSemigroup(10);
<semigroup with 9 generators>
gap> Size(S);
10
gap> GeneratorsOfSemigroup(S);
[ z1, z2, z3, z4, z5, z6, z7, z8, z9 ]
gap> Idempotents(S);
[ 0 ]
gap> IsZeroSemigroup(S);
true
gap> GreensRClasses(S);
[ {0}, {z1}, {z2}, {z3}, {z4}, {z5}, {z6}, {z7}, {z8}, {z9} ]
gap> ZeroSemigroupElt(0);
0
gap> ZeroSemigroupElt(4);
z4
gap> S:=ZeroGroup(CyclicGroup(10));
<zero group with 3 generators>
gap> IsRegularSemigroup(S);
true
gap> Elements(S);
[ 0, <identity> of ..., f1, f2, f1*f2, f2^2, f1*f2^2, f2^3, f1*f2^3, f2^4, 
  f1*f2^4 ]
gap> GreensRClasses(S);
[ {<adjoined zero>}, {ZeroGroup(<identity> of ...)} ]
gap> G:=DihedralGroup(10);;
gap> S:=ZeroGroup(G);;
gap> UnderlyingGroupOfZG(S)=G;
true
gap> Elements(S);
[ 0, <identity> of ..., f1, f2, f1*f2, f2^2, f1*f2^2, f2^3, f1*f2^3, f2^4, 
  f1*f2^4 ]
gap> x:=last[5];
f1*f2
gap> UnderlyingGroupEltOfZGElt(x);
f1*f2
gap> S:=OrderPreservingSemigroup(5);
<monoid with 8 generators>
gap> IsSemiBand(S);    
true
gap> IsRegularSemigroup(S);
true
gap> Size(S)=Binomial(2*5-1, 5-1);
true
gap> S:=KiselmanSemigroup(3);
<fp monoid on the generators [ m1, m2, m3 ]>
gap> Elements(S);
[ <identity ...>, m1, m2, m3, m1*m2, m1*m3, m2*m1, m2*m3, m3*m1, m3*m2, 
  m1*m2*m3, m1*m3*m2, m2*m1*m3, m2*m3*m1, m3*m1*m2, m3*m2*m1, m2*m1*m3*m2, 
  m2*m3*m1*m2 ]
gap> Idempotents(S);
[ 1, m1, m2*m1, m3*m2*m1, m3*m1, m2, m3*m2, m3 ]
gap> SetInfoLevel(InfoMonoidAutos, 0);
#gap> FullMatrixSemigroup(3,4);
#<3x3 full matrix semigroup over GF(2^2)>
#gap> Size(last);
#262144
#gap> S:=RandomSemigroup(4,4);
#<semigroup with 4 generators>
#gap> IsFullMatrixSemigroup(S);
#false
#gap> S:=GeneralLinearSemigroup(3,3);
#<3x3 full matrix semigroup over GF(3)>
#gap> IsFullMatrixSemigroup(S);
#true
gap> STOP_TEST( "semigroups.tst 3.1.4", 10000);