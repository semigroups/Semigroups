##
## install_no_grape.tst
## Version 3.1.1
## Mon Jun  9 17:02:20 BST 2008
##


#ReadTest( Filename( DirectoriesPackageLibrary( "monoid", "tst" ), "install_no_grape.tst" ) );

gap> START_TEST("install_no_grape.tst 3.1.1");
gap> LoadPackage("monoid");;
gap> SetInfoLevel(InfoAutos, 0);
gap> gens:=[ Transformation( [1,8,11,2,5,16,13,14,3,6,15,10,7,4,9,12 ] ), 
>   Transformation( [1,16,9,6,5,8,13,12,15,2,3,4,7,10,11,14] ), 
>   Transformation( [1,3,7,9,1,15,5,11,13,11,13,3,5,15,7,9 ] ) ];;
gap> S:=Semigroup(gens);;
gap> InnerAutomorphismsOfSemigroup(S);
<group of size 16 with 3 generators>
gap> x:=Transformation([2,3,4,5,6,7,8,9,1]);;
gap> y:=Transformation([4,2,3,4,5,6,7,8,9]);;
gap> S:=Semigroup(x,y);;
gap> G:=InnerAutomorphismsOfSemigroup(S);
<group of size 54 with 2 generators>
gap> StructureDescription(last);
"(C9 : C3) : C2"
gap> S:=RandomSemigroup(5,5);
<semigroup with 5 generators>
gap> I:=InnerAutomorphismsOfSemigroup(S);;
gap> IsInnerAutomorphismsOfSemigroup(I);
true
gap> S:=ZeroSemigroup(10);
<zero semigroup with 10 elements>
gap> Size(S);
10
gap> Elements(S);
[ 0, z1, z2, z3, z4, z5, z6, z7, z8, z9 ]
gap> G:=Group([ (2,5)(3,4) ]);;
gap> mat:=[ [ (), (), (), (), () ], [ (), (), (2,5)(3,4), (2,5)(3,4), () ], 
>   [ (), (), (), (2,5)(3,4), (2,5)(3,4) ], 
>   [ (), (2,5)(3,4), (), (2,5)(3,4), () ], 
>   [ (), (2,5)(3,4), (), (2,5)(3,4), () ] ];;
gap> rms:=ReesMatrixSemigroup(G, mat);;
gap> A:=AutomorphismGroup(rms);
<group of size 12 with 12 generators>
gap> IsAutomorphismGroupOfRMS(A);
true
gap> gens:= [ Transformation( [ 4, 3, 3, 6, 7, 2, 3 ] ), 
>   Transformation( [ 6, 6, 4, 4, 2, 1, 4 ] ) ];;
gap> S:=Semigroup(gens);;
gap> Length(GreensRClasses(S));
17
gap> GreensData(GreensRClasses(S)[10]);
GreensRClassData( Transformation( [ 3, 3, 3, 3, 3, 2, 3 ] ), 
[ [ 2, 3 ], [ 4, 6 ], [ 2, 6 ], [ 1, 6 ], [ 2, 4 ], [ 3, 6 ], [ 1, 4 ] ], 
[ (), (2,4,3,5,6), (3,4,5,6), (1,3,4,5,6,2), (3,4), (2,4,5,6,3), (1,2)(3,4) 
 ], Group( [ (), (2,3) ] ) )
gap> SchutzenbergerGroup(last);
Group([ (), (2,3) ])
gap> Number(GreensDClasses(S), IsRegularDClass);
3
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
gap> List(SmallMonoids, IsRegularSemigroup);          
[ false, true, false, false, true, true, true, true, true, false, false, 
  true, true, true, false ]
gap> List(SmallMonoids, IsSimpleSemigroup); 
[ false, false, false, false, false, true, false, false, false, false, false, 
  false, false, false, false ]
gap> List(SmallMonoids, IsCompletelySimpleSemigroup);
[ false, false, false, false, false, true, false, false, false, false, false, 
  false, false, false, false ]
gap> List(SmallMonoids, IsInverseSemigroup);         
[ false, true, false, false, false, false, false, false, false, false, false, 
  false, false, false, false ]
gap> List(SmallMonoids, IsCliffordSemigroup);
[ false, true, false, false, false, false, false, false, false, false, false, 
  false, false, false, false ]
gap> List(SmallMonoids, IsGroupAsSemigroup); 
[ false, false, false, false, false, false, false, false, false, false, 
  false, false, false, false, false ]
gap> List(SmallMonoids, IsZeroSemigroup);
[ false, false, false, false, false, false, false, false, false, false, 
  false, false, false, false, false ]
gap> List(SmallMonoids, IsLeftZeroSemigroup);
[ false, false, false, false, false, false, false, false, false, false, 
  false, false, false, false, false ]
gap> List(SmallMonoids, IsRightZeroSemigroup);
[ false, false, false, false, false, false, false, false, false, false, 
  false, false, false, false, false ]
gap> List(SmallMonoids, IsCommutativeSemigroup);
[ false, false, false, false, false, false, false, false, false, false, 
  false, false, false, false, false ]
gap> List(SmallMonoids, IsZeroGroup);           
[ false, false, false, false, false, false, false, false, false, false, 
  false, false, false, false, false ]
gap> op5:=OrderPreservingSemigroup(5);
<monoid with 8 generators>
gap> s:=SingularSemigroup(5);
<semigroup with 20 generators>
gap> RandomSemigroup(5,5);; RandomMonoid(5,5);; 
gap> RandomReesMatrixSemigroup(3,3,7);;
gap> RandomReesZeroMatrixSemigroup(4,4,4);;
gap> gens:=[ Transformation( [ 1, 4, 3, 5, 2 ] ), 
> Transformation( [ 2, 3, 1, 1, 2 ] ) ];;
gap> S:=Semigroup(gens);;
gap> gens:=[ Transformation( [ 1, 5, 1, 2, 1 ] ), 
> Transformation( [ 5, 1, 4, 3, 2 ] ) ];;
gap> T:=Semigroup(gens);;
gap> idem:=Random(Idempotents(T));;
gap> hom:=SemigroupHomomorphismByFunction(S, T, x-> idem);
SemigroupHomomorphism ( <semigroup with 2 generators>-><semigroup with 
2 generators>)
gap> hom:=SemigroupHomomorphismByFunctionNC(S, T, x-> idem);
SemigroupHomomorphism ( <semigroup with 2 generators>-><semigroup with 
2 generators>)
gap> SemigroupHomomorphismByImagesOfGens(S, T, GeneratorsOfSemigroup(T));
fail
gap> SemigroupHomomorphismByImagesOfGens(S, S, GeneratorsOfSemigroup(S));
SemigroupHomomorphismByImagesOfGens ( <trans. semigroup of size 161 with 
2 generators>-><trans. semigroup of size 161 with 2 generators>)
gap> gens:=[ Transformation( [ 2, 3, 4, 2, 4 ] ),
> Transformation( [ 3, 4, 2, 1, 4 ] ) ];;
gap> S:=Semigroup(gens);;
gap> gens:=[ Transformation( [ 2, 4, 4, 1, 2 ] ),
> Transformation( [ 5, 1, 1, 5, 1 ] ) ];;
gap> T:=Semigroup(gens);;
gap> idem:=Transformation( [ 5, 5, 5, 5, 5 ] );;
gap> list:=List([1..Size(S)], x-> idem);;
gap> hom:=SemigroupHomomorphismByImages(S, T, list);
SemigroupHomomorphismByImagesOfGens ( <trans. semigroup of size 164 with 
2 generators>-><trans. semigroup with 2 generators>)
gap> SemigroupHomomorphismByImagesNC(S, T, list);
SemigroupHomomorphismByImages ( <trans. semigroup of size 164 with 
2 generators>-><trans. semigroup with 2 generators>)
gap> STOP_TEST( "install_no_grape.tst 3.1.1", 10000);