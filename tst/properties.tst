#############################################################################
##
#W  properties.tst
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

#ReadTest( Filename( DirectoriesPackageLibrary( "monoid", "tst" ), "properties.tst" ) );
# 800ms to run!


gap> START_TEST("properties.tst 4.0");
gap> LoadPackage("monoid");;
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
gap> List(SmallMonoids, IsCompletelyRegularSemigroup);  
[ false, true, false, false, true, true, true, true, true, false, false, 
  false, false, true, false ]
gap> List(BigMonoids, IsCompletelyRegularSemigroup);  
[ true, false, false, false, false, false, true, false ]
gap> g1:=Transformation([3,3,2,6,2,4,4,6]);;
gap> g2:=Transformation([5,1,7,8,7,5,8,1]);;
gap> cs1:=Semigroup(g1,g2);;
gap> IsSimpleSemigroup(cs1);
true
gap> g1:=Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] );;
gap> g2:=Transformation( [ 2, 3, 4, 5, 6, 8, 7, 1, 2, 2 ] );;
gap> cs2:=Semigroup(g1,g2);;
gap> IsSimpleSemigroup(cs2);
true
gap> g1:=Transformation([2,1,1,2,1]);;
gap> g2:=Transformation([3,4,3,4,4]);;
gap> g3:=Transformation([3,4,3,4,3]);;
gap> g4:=Transformation([4,3,3,4,4]);;
gap> cs3:=Semigroup(g1,g2,g3,g4);;
gap> IsCompletelySimpleSemigroup(cs3);
true
gap> g1:=Transformation([4,4,4,1,1,6,7,8,9,10,11,1]);;
gap> g2:=Transformation([6,6,6,7,7,1,4,8,9,10,11,7]);;
gap> g3:=Transformation([8,8,8,9,9,10,11,1,4,6,7,9]);;
gap> g4:=Transformation([2,2,2,4,4,6,7,8,9,10,11,4]);;
gap> g5:=Transformation([1,1,1,5,5,6,7,8,9,10,11,5]);;
gap> g6:=Transformation([1,1,4,4,4,6,7,8,9,10,11,1]);;
gap> g7:=Transformation([1,1,7,4,4,6,7,8,9,10,11,6]);;
gap> cs4:=Semigroup(g1, g2, g3, g4, g5, g6, g7);;
gap> IsCompletelySimpleSemigroup(cs4);
true
gap> g1:=Transformation([1,2,2,1,2]);;
gap> g2:=Transformation([3,4,3,4,4]);;
gap> g3:=Transformation([3,4,3,4,3]);;
gap> g4:=Transformation([4,3,3,4,4]);;
gap> cs5:=Semigroup(g1,g2,g3,g4);;
gap> IsCompletelySimpleSemigroup(cs5);
true
gap> dc:=GreensDClassOfElement(m9, Transformation( [ 3, 3, 2, 6, 2, 4, 4, 6, 3, 4, 6 ] ));;
gap> d:=GreensDClassOfElement(m14, Transformation( [ 12, 2, 1, 3, 6, 6, 12, 2, 3, 3, 11, 3 ] ));;
gap> g:=GroupHClassOfGreensDClass(d);;
gap> s:=Semigroup(AsList(g));;
gap> IsGroupAsSemigroup(s);
true
gap> IsGroupAsSemigroup(Range(IsomorphismTransformationSemigroup(
> PrimitiveGroup(5,2))));
true
gap> IsGroupAsSemigroup(m14);
false
gap> List(SmallMonoids, IsCliffordSemigroup);
[ false, true, false, false, false, false, false, false, false, false, false, 
  false, false, false, false ]
gap> List(BigMonoids, IsCliffordSemigroup);
[ false, false, false, false, false, false, false, false ]
gap> ForAll(GreensDClasses(m2), x-> Length(GreensHClasses(x))=1 and 
> IsRegularDClass(x));
true
gap> IsCliffordSemigroup(m2);
true
gap> ForAll(GreensDClasses(m2), x-> Length(GreensHClasses(x))=1 and 
> IsRegularDClass(x));
true
gap> g1:=Transformation([1,2,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4]);;
gap> g2:=Transformation([1,2,3,4,5,6,7,4,4,4,4,4,4,4,4,4,4,4,4,4,4]);;
gap> g3:=Transformation([1,2,3,4,5,6,7,8,9,10,11,4,4,4,4,4,4,4,4,4,4]);;
gap> g4:=Transformation([1,2,3,4,4,4,4,4,4,4,4,12,13,14,15,16,4,4,4,4,4]);;
gap> g5:=Transformation([1..21]);;
gap> c3:=Semigroup(g1,g2,g3,g4,g5);;
gap> IsCliffordSemigroup(c3);
true
gap> Size(c3);
5
gap> ForAll(GreensDClasses(c3), x-> Length(GreensHClasses(x))=1 and 
> IsRegularDClass(x));
true
gap> g1:=g1*(1,2);;
gap> g2:=Transformation([1,2,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4])*
> (1,2,3,4);;
gap> g3:=Transformation([1,2,3,4,5,6,7,4,4,4,4,4,4,4,4,4,4,4,4,4,4])*
> (5,6);;
gap> g4:=Transformation([1,2,3,4,5,6,7,4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
> 4, 4, 4 ])*(5,6,7);;
gap> g5:=Transformation([1,2,3,4,5,6,7,8,9,10,11, 4, 4, 4, 4, 4, 4, 4, 4, 
> 4, 4 ])*(8,9);;
gap> g6:=Transformation([1,2,3,4,5,6,7,8,9,10,11, 4, 4, 4, 4, 4, 4, 4, 4, 
> 4, 4 ])*(10,11);;
gap> g7:=Transformation( [ 1,2,3,4,4,4,4,4,4,4,4,12,13,14,15,16,17, 
> 18,19,20,21])*(12,13);;
gap> g8:=Transformation
> ([1,2,3,4,4,4,4,4,4,4,4,12,13,14,15,16,17,18,19,20,21])*
> (12,13,14,15,16);;
gap> g9:=Transformation([1..21])*(17,18,19,20,21);;
gap> c4:=Semigroup(g1,g2,g3,g4,g5,g6,g7,g8,g9);;
gap> IsCliffordSemigroup(c4);
true
gap> ForAll(GreensDClasses(c3), x-> Length(GreensHClasses(x))=1 and 
> IsRegularDClass(x));
true
gap> ForAll(GreensDClasses(c4), x-> Length(GreensHClasses(x))=1 and 
> IsRegularDClass(x));
true
gap> List(SmallMonoids, IsRegularSemigroup);
[ false, true, false, false, true, true, true, true, true, false, false,
  true, true, true, false ]
gap> List(BigMonoids, IsRegularSemigroup);
[ true, false, false, false, false, true, true, false ]
gap> List([c3,c4], IsInverseSemigroup);
[ true, true ]
gap> IsBand(c3);
true
gap> IsBand(c4);
false
gap> List(SmallMonoids, IsBand);
[ false, false, false, false, false, false, false, false, false, false,
  false, false, false, false, false ]
gap> List(BigMonoids, IsBand);  
[ false, false, false, false, false, false, false, false ]
gap> g:=Group(());;
gap> mat:=[[(), (), ()], [(), (), ()], [(), (), ()], [(), (), ()]];;
gap> rms:=ReesMatrixSemigroup(g, mat);;
gap> s:=Range(IsomorphismTransformationSemigroup(rms));;
gap> IsRectangularBand(s);
true
gap> IsBand(s); 
true
gap> IsSemiBand(FullTransformationSemigroup(4));
false
gap> Size(Semigroup(Idempotents(FullTransformationSemigroup(4))));
233
gap> 4^4-Factorial(4)+1;
233
gap> ForAll(SmallMonoids, x-> not IsIdempotentGenerated(x));
true
gap> List(SmallMonoids, IsOrthodoxSemigroup);
[ false, true, false, false, false, false, true, true, true, false, false, 
  false, false, true, false ]
gap> s:=SmallMonoids[7];;
gap> Size(s);;
gap> t:=Semigroup(Idempotents(s));;
gap> IsBand(t);    
true
gap> IsRectangularBand(t);
false
gap> g:=Group(());;
gap> mat:=[[(), (), (), (), (), ()]];;                                   
gap> rms:=ReesMatrixSemigroup(g, mat);;
gap> s:=Range(IsomorphismTransformationSemigroup(rms));;
gap> IsLeftZeroSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false
gap> mat:=TransposedMat(mat);;
gap> rms:=ReesMatrixSemigroup(g, mat);;
gap> s:=Range(IsomorphismTransformationSemigroup(rms));;
gap> IsRightZeroSemigroup(s);
true
gap> IsLeftZeroSemigroup(s);
false
gap> List(BigMonoids, IsLeftZeroSemigroup);    
[ false, false, false, false, false, false, false, false ]
gap> List(BigMonoids, IsRightZeroSemigroup);
[ false, false, false, false, false, false, false, false ]
gap> gens:=[ Transformation( [ 2, 2, 4, 4, 6, 6, 8, 8, 10, 10, 12, 12, 2 ] ), 
> Transformation( [ 1, 1, 3, 3, 5, 5, 7, 7, 9, 9, 11, 11, 3 ] ), 
> Transformation( [ 1, 7, 3, 9, 5, 11, 7, 1, 9, 3, 11, 5, 5 ] ), 
> Transformation( [ 7, 7, 9, 9, 11, 11, 1, 1, 3, 3, 5, 5, 7 ] ) ];;
gap> S:=Semigroup(gens);;
gap> IsSimpleSemigroup(S);
true
gap> IsCompletelySimpleSemigroup(S);
true
gap> gens:=[ Transformation( [ 1, 2, 4, 3, 6, 5, 4 ] ), 
> Transformation( [ 1, 2, 5, 6, 3, 4, 5 ] ), 
> Transformation( [ 2, 1, 2, 2, 2, 2, 2 ] ) ];;
gap> S:=Semigroup(gens);;
gap> IsCompletelyRegularSemigroup(S);
true
gap> gens:=[ Transformation( [ 2, 4, 5, 3, 7, 8, 6, 9, 1 ] ), 
> Transformation( [ 3, 5, 6, 7, 8, 1, 9, 2, 4 ] ) ];;
gap> S:=Semigroup(gens);;
gap> IsGroupAsSemigroup(S);
true
gap> IsCommutativeSemigroup(S);
true
gap> gens:=[Transformation([1,2,4,5,6,3,7,8]),
> Transformation([3,3,4,5,6,2,7,8]),
> Transformation([1,2,5,3,6,8,4,4])];;
gap> S:=Semigroup(gens);;
gap> IsCliffordSemigroup(S);
true
gap> gens:=[ Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 1 ] ), 
> Transformation( [ 2, 2, 2, 5, 5, 5, 8, 8, 8, 2 ] ), 
> Transformation( [ 3, 3, 3, 6, 6, 6, 9, 9, 9, 3 ] ), 
> Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 4 ] ), 
> Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 7 ] ) ];;
gap> S:=Semigroup(gens);;
gap> IsBand(S);
true
gap> IsRectangularBand(S);
true
gap> S:=FullTransformationSemigroup(4);;
gap> x:=Transformation( [ 1, 2, 3, 1 ] );;
gap> D:=GreensDClassOfElement(S, x);;
gap> T:=Semigroup(Elements(D));;
gap> IsSemiBand(T);
true
gap> gens:=[ Transformation( [ 1, 1, 1, 4, 5, 4 ] ), 
>  Transformation( [ 1, 2, 3, 1, 1, 2 ] ), 
>  Transformation( [ 1, 2, 3, 1, 1, 3 ] ), 
>  Transformation( [ 5, 5, 5, 5, 5, 5 ] ) ];;
gap> S:=Semigroup(gens);;
gap> IsOrthodoxSemigroup(S);
true
gap> gens:=[ Transformation( [ 2, 1, 4, 3, 5 ] ), 
>  Transformation( [ 3, 2, 3, 1, 1 ] ) ];;
gap> S:=Semigroup(gens);;
gap> IsRightZeroSemigroup(S);
false
gap> gens:=[Transformation( [ 1, 2, 3, 3, 1 ] ), 
>  Transformation( [ 1, 2, 4, 4, 1 ] )];;
gap> S:=Semigroup(gens);;
gap> IsRightZeroSemigroup(S);
true
gap> gens:=[ Transformation( [ 2, 1, 4, 3, 5 ] ), 
>  Transformation( [ 3, 2, 3, 1, 1 ] ) ];;
gap> S:=Semigroup(gens);;
gap> IsRightZeroSemigroup(S);
false
gap> gens:=[Transformation([1,2,3,3,1]), Transformation([1,2,3,3,3])];;
gap> S:=Semigroup(gens);;
gap> IsLeftZeroSemigroup(S);
true
gap> gens:=[ Transformation( [ 4, 7, 6, 3, 1, 5, 3, 6, 5, 9 ] ), 
>  Transformation( [ 5, 3, 5, 1, 9, 3, 8, 7, 4, 3 ] ), 
>  Transformation( [ 5, 10, 10, 1, 7, 6, 6, 8, 7, 7 ] ), 
>  Transformation( [ 7, 4, 3, 3, 2, 2, 3, 2, 9, 3 ] ), 
>  Transformation( [ 8, 1, 3, 4, 9, 6, 3, 7, 1, 6 ] ) ];;
gap> S:=Semigroup(gens);;
gap> IsZeroSemigroup(S);
false
gap> gens:=[ Transformation( [ 1, 4, 2, 6, 6, 5, 2 ] ), 
> Transformation( [ 1, 6, 3, 6, 2, 1, 6 ] ) ];;
gap> S:=Semigroup(gens);;
gap> MultiplicativeZero(S);
Transformation( [ 1, 1, 1, 1, 1, 1, 1 ] )
gap> STOP_TEST( "properties.tst 4.0", 10000);



