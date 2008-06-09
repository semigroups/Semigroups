##
## autos3.tst
## Version 3.1.1
## Mon Jun  9 09:26:11 BST 2008
##

#dirs := DirectoriesPackageLibrary( "monoid", "tst" );
#ReadTest( Filename( dirs, "autos3.tst" ) );

gap> START_TEST("autos3.tst 3.1.1");
gap> LoadPackage("monoid");;
gap> S:=RandomSemigroup(5,5);
<semigroup with 5 generators>
gap> I:=InnerAutomorphismsOfSemigroup(S);;
gap> IsInnerAutomorphismsOfSemigroup(I);
true
gap> zg:=ZeroGroup(CyclicGroup(70));
<zero group with 4 generators>
gap> I:=InnerAutomorphismsAutomorphismGroup(AutomorphismGroup(zg));
<group of size 1 with 1 generators>
gap> IsInnerAutomorphismsOfZeroGroup(I);
true
gap> G:=Group((1,4,3,5,2));;
gap> mat:=[ [ (), (), () ], [ (), (1,4,3,5,2), () ], [ (), (1,3,2,4,5), () ] ];;
gap> rms:=ReesMatrixSemigroup(G, mat);;
gap> l:=(4,6);
(4,6)
gap> g:=GroupHomomorphismByImages(G, G, [(1,4,3,5,2)], [(1,2,5,3,4)]);
[ (1,4,3,5,2) ] -> [ (1,2,5,3,4) ]
gap> map:=[(), (1,5,4,2,3), (), (), (), () ];
[ (), (1,5,4,2,3), (), (), (), () ]
gap> RMSIsoByTriple(rms, rms, [l, g, map]);
[ (4,6), GroupHomomorphismByImages( Group( [ (1,4,3,5,2) ] ), Group( 
    [ (1,4,3,5,2) ] ), [ (1,4,3,5,2) ], [ (1,2,5,3,4) ] ), 
  [ (), (1,5,4,2,3), (), (), (), () ] ]
gap> IsRMSIsoByTripleRep(last);
true
gap> zg:=ZeroGroup(CyclicGroup(70));
<zero group with 4 generators>
gap> IsZeroGroup(zg);
true
gap> A:=AutomorphismGroup(zg);
<group with 3 generators>
gap> IsAutomorphismGroupOfZeroGroup(A);
true
gap> Size(A);
24
gap> StructureDescription(A);
"C12 x C2"
gap> InnerAutomorphismsOfSemigroup(zg);
<group of size 1 with 1 generators>
gap> InnerAutomorphismsAutomorphismGroup(AutomorphismGroup(zg));
<group of size 1 with 1 generators>
gap> zg:=ZeroGroup(AllGroups(124)[3]);
<zero group with 4 generators>
gap> AutomorphismGroup(zg);;
gap> Size(last);
1860
gap> InnerAutomorphismsOfSemigroup(zg);
<group with 2 generators>
gap> Size(last);
62
gap> last2=InnerAutomorphismsAutomorphismGroup(AutomorphismGroup(zg));
true
gap> S:=ZeroSemigroup(10);
<zero semigroup with 10 elements>
gap> Size(S);
10
gap> Elements(S);
[ 0, z1, z2, z3, z4, z5, z6, z7, z8, z9 ]
gap> AutomorphismGroup(S);
<group with 2 generators>
gap> A:=last;
<group with 2 generators>
gap> Size(A);
362880
gap> Factorial(9)=last;
true
gap> GeneratorsOfGroup(A);
[ SemigroupHomomorphismByImages ( Semigroup( 
    [ z1, z2, z3, z4, z5, z6, z7, z8, z9 ] )->Semigroup( 
    [ z1, z2, z3, z4, z5, z6, z7, z8, z9 ] )), 
  SemigroupHomomorphismByImages ( Semigroup( 
    [ z1, z2, z3, z4, z5, z6, z7, z8, z9 ] )->Semigroup( 
    [ z1, z2, z3, z4, z5, z6, z7, z8, z9 ] )) ]
gap> IsAutomorphismGroupOfZeroSemigroup(A);
true
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
gap> StructureDescription(Range(IsomorphismPermGroup(A)));
"D12"
gap> G:=CyclicGroup(64); 
<pc group of size 64 with 6 generators>
gap> ZG:=ZeroGroup(G);
<zero group with 7 generators>
gap> AutomorphismGroup(ZG);;
gap> Size(last);
32
gap> IsAutomorphismGroupOfZeroGroup(last2);
true
gap> G:=DihedralGroup(20);;
gap> A:=AutomorphismGroup(G);;
gap> f:=Random(A);;
gap> ZG:=ZeroGroup(G);;
gap> ZeroGroupAutomorphism(ZG, f);;
gap> IsZeroGroupAutomorphismRep(last);
true
gap> UnderlyingGroupAutoOfZeroGroupAuto(last2)=f;
true
gap> S:=ZeroSemigroup(11);
<zero semigroup with 11 elements>
gap> AutomorphismGroup(S);
<group with 2 generators>
gap> IsAutomorphismGroupOfZeroSemigroup(last);
true
gap> S:=ZeroSemigroup(6);
<zero semigroup with 6 elements>
gap> elts:=Elements(S);
[ 0, z1, z2, z3, z4, z5 ]
gap> Length(RightTransStabAutoGroup(S, [elts[1]], OnSets));
1
gap> #JDM
gap> Length(RightTransStabAutoGroup(S, [elts[1], elts[2]], OnSets));
5
gap> Length(RightTransStabAutoGroup(S, [elts[1], elts[2]], OnTuples));
5
gap> G:=Group([ (1,2) ]);;
gap> mat:=[ [ (), (), () ], [ (), (1,2), () ], [ (), (1,2), (1,2) ], 
>    [ (), (), () ], [ (), (1,2), () ] ];;
gap> rms:=ReesMatrixSemigroup(G, mat);;
gap> Size(rms);
30
gap> GeneratorsOfSemigroup(rms);
[ (1,(),2), (1,(),3), (1,(),4), (1,(),5), (2,(),1), (3,(),1), (1,(1,2),1) ]
gap> Length(RightTransStabAutoGroup(rms, last, OnSets));
4
gap> Length(RightTransStabAutoGroup(rms, GeneratorsOfSemigroup(rms), OnTuples));
8
gap> G:=Group([ (1,2) ]);;
gap> mat:=[ [ (), (), () ], [ (), (1,2), () ], [ (), (1,2), (1,2) ], 
>    [ (), (), () ], [ (), (1,2), () ] ];;
gap> rms:=ReesMatrixSemigroup(G, mat);;
gap> l:=(1,2)(4,5,6);
(1,2)(4,5,6)
gap> gam:=One(AutomorphismGroup(G));
IdentityMapping( Group([ (1,2) ]) )
gap> g:=(1,2);;
gap> RMSInducedFunction(rms, l, gam, g);
[ false, [ (1,2), (), (), (), (), (1,2), (1,2), () ] ]
gap> RMSInducedFunction(rms, (4,7), gam, ());
[ true, [ (), (), (), (), (), (), (), () ] ]
gap> gens:=[ Transformation( [ 4, 4, 8, 8, 8, 8, 4, 8 ] ), 
>    Transformation( [ 8, 2, 8, 2, 5, 5, 8, 8 ] ), 
>    Transformation( [ 8, 8, 3, 7, 8, 3, 7, 8 ] ), 
>    Transformation( [ 8, 6, 6, 8, 6, 8, 8, 8 ] ) ];;
gap> S:=Semigroup(gens);;
gap> D:=GreensDClasses(S);;
gap> rms1:=Range(IsomorphismReesMatrixSemigroupOfDClass(D[1]));
Rees Zero Matrix Semigroup over <zero group with 2 generators>
gap> rms2:=Range(IsomorphismReesMatrixSemigroupOfDClass(D[4]));
Rees Zero Matrix Semigroup over <zero group with 2 generators>
gap> gens:=[ Transformation( [1,8,11,2,5,16,13,14,3,6,15,10,7,4,9,12 ] ), 
>   Transformation( [1,16,9,6,5,8,13,12,15,2,3,4,7,10,11,14] ), 
>   Transformation( [1,3,7,9,1,15,5,11,13,11,13,3,5,15,7,9 ] ) ];;
gap> S:=Semigroup(gens);;
gap> IsomorphismFpSemigroup(last);
SemigroupHomomorphismByImages ( <trans. semigroup of size 16 with 
3 generators>->Semigroup( [ s1, s2, s3 ] ))
gap> RelationsOfFpSemigroup(Range(last));
[ [ s2*s1, s1*s2 ], [ s3*s1, s1*s3 ], [ s3*s2, s2*s3 ], [ s3^2, s2^2 ], 
  [ s1^2*s3, s1^2*s2 ], [ s1*s2*s3, s1 ], [ s1^3*s2, s1^3 ], 
  [ s1^2*s2^2, s1^2 ], [ s1*s2^3, s1*s3 ], [ s1*s2^2*s3, s1*s2 ], 
  [ s1^5, s1^4 ], [ s2^5, s2 ], [ s2^4*s3, s3 ] ]
gap> x:=Transformation([2,3,4,5,6,7,8,9,1]);;
gap> y:=Transformation([4,2,3,4,5,6,7,8,9]);;
gap> S:=Monoid(x,y);;
gap> IsomorphismFpMonoid(last);
SemigroupHomomorphismByImages ( <trans. semigroup of size 40266 with 
3 generators>->Monoid( [ m1, m2 ], ... ))
gap> Length(RelationsOfFpMonoid(Range(last)));
932
gap> ZG1:=ZeroGroup(Group((1,2,3,5,4)));
<zero group with 2 generators>
gap> ZG2:=ZeroGroup(Group((1,2,3,4,5)));
<zero group with 2 generators>
gap> IsomorphismSemigroups(ZG1, ZG2);
SemigroupHomomorphismByImagesOfGens ( <zero group with 
2 generators>-><zero group with 2 generators>)
gap> ZG2:=ZeroGroup(Group((1,2,3,4)));
<zero group with 2 generators>
gap> IsomorphismSemigroups(ZG1, ZG2);
fail
gap> IsomorphismSemigroups(ZeroSemigroup(5),ZeroSemigroup(5));
IdentityMapping( <zero semigroup with 5 elements> )
gap> IsomorphismSemigroups(ZeroSemigroup(5),ZeroSemigroup(6));
fail
gap> g1:=Transformation( [ 4, 6, 3, 8, 5, 6, 10, 4, 3, 7 ] );;
gap> g2:=Transformation( [ 5, 6, 6, 3, 8, 6, 3, 7, 8, 4 ] );;
gap> g3:=Transformation( [ 8, 6, 3, 2, 8, 10, 9, 2, 6, 2 ] );;
gap> m23:=Monoid(g1,g2,g3);;
gap> D:=GreensDClasses(m23)[17];
{Transformation( [ 7, 6, 6, 6, 7, 4, 8, 6, 6, 6 ] )}
gap> IsomorphismReesMatrixSemigroupOfDClass(D);
SemigroupHomomorphism ( {Transformation( [ 7, 6, 6, 6, 7, 4, 8, 6, 6, 6 
 ] )}-><zero semigroup with 3 elements>)
gap> D:=GreensDClasses(m23)[77];
{Transformation( [ 6, 6, 6, 6, 6, 6, 6, 6, 6, 6 ] )}
gap> IsomorphismReesMatrixSemigroupOfDClass(D);
SemigroupHomomorphism ( {Transformation( [ 6, 6, 6, 6, 6, 6, 6, 6, 6, 6 
 ] )}->Rees Matrix Semigroup over Group(()))
gap> D:=GreensDClasses(m23)[1];
{Transformation( [ 1 .. 10 ] )}
gap> IsomorphismReesMatrixSemigroupOfDClass(D);
SemigroupHomomorphism ( {Transformation( [ 1 .. 10 ] )}->Group(()))
gap> D:=GreensDClasses(m23)[23];
{Transformation( [ 6, 7, 3, 6, 6, 6, 6, 6, 7, 6 ] )}
gap> IsomorphismReesMatrixSemigroupOfDClass(D);
SemigroupHomomorphism ( {Transformation( [ 6, 7, 3, 6, 6, 6, 6, 6, 7, 6 
 ] )}->Rees Zero Matrix Semigroup over <zero group with 3 generators>)
gap> g1:=Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] );;
gap> g2:=Transformation( [ 2, 3, 4, 5, 6, 8, 7, 1, 2, 2 ] );;
gap> cs2:=Semigroup(g1,g2);;
gap> IsomorphismReesMatrixSemigroup(cs2);;
gap> gens:=[ Transformation( [ 6, 2, 7, 5, 3, 5, 4 ] ), 
> Transformation( [ 7, 7, 5, 7, 2, 4, 3 ] ) ];;
gap> S:=Monoid(gens);;
gap> InnerAutomorphismOfSemigroup(S, (1,2,3,4,5));  
fail
gap> InnerAutomorphismOfSemigroupNC(S, (1,2,3,4,5));
^(1,2,3,4,5)
gap> InnerAutomorphismOfSemigroup(S, ());
^()
gap> S:=RandomSemigroup(3,8);;
gap> f:=InnerAutomorphismOfSemigroupNC(S, (1,2)(3,4));
^(1,2)(3,4)
gap> ConjugatorOfInnerAutomorphismOfSemigroup(f);
(1,2)(3,4)
gap> STOP_TEST( "autos3.tst 3.1.1", 10000);