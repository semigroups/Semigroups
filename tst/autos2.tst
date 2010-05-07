#############################################################################
##
#W  autos2.tst
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

## don't run this file if grape is not loaded. 

#ReadTest( Filename( DirectoriesPackageLibrary( "monoid", "tst" ), "autos2.tst" ) );

gap> START_TEST("autos2.tst 3.1.3");
gap> LoadPackage("monoid");;
gap> g1:=Transformation([3,3,2,6,2,4,4,6]);;
gap> g2:=Transformation([5,1,7,8,7,5,8,1]);;
gap> cs1:=Semigroup(g1,g2);;
gap> A:=AutomorphismGroup(cs1);;
gap> IsAutomorphismGroupOfSimpleSemigp(A);
true
gap> IsomorphismAutomorphismGroupOfRMS(A);;
gap> IsomorphismPermGroup(A);;
gap> g1:=Transformation([2,1,1,2,1]);;
gap> g2:=Transformation([3,4,3,4,4]);;
gap> g3:=Transformation([3,4,3,4,3]);;
gap> g4:=Transformation([4,3,3,4,4]);;
gap> cs3:=Semigroup(g1,g2,g3,g4);;
gap> A:=AutomorphismGroup(cs3);;
gap> IsomorphismAutomorphismGroupOfRMS(A);;
gap> IsomorphismPermGroup(A);;
gap> Size(cs3);
16
gap> StructureDescription(A);
"C2 x D8"
gap> g1:=Transformation([1,2,2,1,2]);;
gap> g2:=Transformation([3,4,3,4,4]);;
gap> g3:=Transformation([3,4,3,4,3]);;
gap> g4:=Transformation([4,3,3,4,4]);;
gap> cs5:=Semigroup(g1,g2,g3,g4);;
gap> A:=AutomorphismGroup(cs5);;
gap> IsAutomorphismGroupOfSimpleSemigp(A);
true
gap> IsomorphismAutomorphismGroupOfRMS(A);;
gap> IsomorphismPermGroup(A);;
gap> Size(cs5);
16
gap> StructureDescription(A);
"C2 x D8"
gap> id:=SemigroupHomomorphismByImagesOfGensNC(cs5, cs5, [g1,g2,g3,g4]);;
gap> IsOne(id);
true
gap> id;
IdentityMapping( <semigroup with 4 generators> )
gap> G:=Group(id);
<group with 1 generators>
gap> AutomorphismsSemigroupInGroup(cs5, G);
<group of size 1 with 1 generators>
gap> gens:=[Transformation([2,1,1,2,1]), Transformation([3,4,3,4,4]), Transformation([3,4,3,4,3]),
> Transformation([4,3,3,4,4])];;
gap> cs3:=Semigroup(gens);;
gap> id:=SemigroupHomomorphismByImagesOfGensNC(cs3, cs3, gens);;
gap> G:=Group(id);
<group with 1 generators>
gap> AutomorphismsSemigroupInGroup(cs3, G);
<group of size 1 with 1 generators>
gap> g1:=Transformation([3,3,2,6,2,4,4,6]);;
gap> g2:=Transformation([5,1,7,8,7,5,8,1]);;
gap> cs1:=Semigroup(g1,g2);;
gap> id:=SemigroupHomomorphismByImagesOfGensNC(cs1, cs1, [g1, g2]);;
gap> G:=Group(id);
<group with 1 generators>
gap> AutomorphismsSemigroupInGroup(cs1, G);
<group of size 1 with 1 generators>
gap> g1:=Transformation([2,2,4,4,5,6]);;
gap> g2:=Transformation([5,3,4,4,6,6]);;
gap> S:=Monoid(g1,g2);;
gap> A:=AutomorphismsSemigroupInGroup(S, fail, [false, true, false, false, false]);
<group of size 1 with 1 generators>
gap> InnerAutomorphismsAutomorphismGroup(A);
<group of size 1 with 1 generators>
gap> InnerAutomorphismsOfSemigroup(S);
<group of size 1 with 1 generators>
gap> g1:=Transformation([5,4,4,2,1]);;
gap> g2:=Transformation([2,5,5,4,1]);;
gap> m2:=Monoid(g1,g2);;
gap> A:=AutomorphismsSemigroupInGroup(m2, fail, [false, true, true, false, true]);
<group of size 24 with 3 generators>
gap> StructureDescription(A);
"S4"
gap> StructureDescription(InnerAutomorphismsAutomorphismGroup(A));
"S3"
gap> g1:=Transformation([1,2,1,3,3]);;
gap> g2:=Transformation([2,2,3,5,5]);;
gap> m3:=Monoid(g1,g2);;
gap> AutomorphismGroup(m3);
<group of size 1 with 1 generators>
gap> InnerAutomorphismsOfSemigroup(m3);
<group of size 1 with 1 generators>
gap> Elements(last);
[ IdentityMapping( <monoid with 2 generators> ) ]
gap> 
gap> g1:=Transformation( [ 8, 7, 5, 3, 1, 3, 8, 8 ] );;
gap> g2:=Transformation( [ 5, 1, 4, 1, 4, 4, 7, 8 ] );;
gap> m4:=Monoid(g1,g2);;
gap> AutomorphismGroup(m4);
<group of size 1 with 1 generators>
gap> Elements(last);
[ IdentityMapping( <monoid with 2 generators> ) ]
gap> g1:=Transformation([3,1,2,3,2,3,2,3]);;
gap> g2:=Transformation([2,5,8,5,2,5,7,8]);;
gap> m5:=Monoid(g1,g2);;
gap> AutomorphismGroup(m5);
<group of size 1 with 1 generators>
gap> g1:=Transformation([3,3,2,6,2,4,4,6]);;
gap> g2:=Transformation([5,1,7,8,7,5,8,1]);;
gap> m6:=Semigroup(g1,g2);;
gap> A:=AutomorphismGroup(m6);
<group of size 12 with 2 generators>
gap> InnerAutomorphismsAutomorphismGroup(A);
<group of size 12 with 2 generators>
gap> g1:=Transformation([3,3,2,6,2,4,4,6,3,4,6]);;
gap> g2:=Transformation([4,4,6,1,3,3,3,3,11,11,11]);;
gap> m7:=Monoid(g1,g2);;
gap> A:=AutomorphismsSemigroupInGroup(m7, fail, [false, true, false, false, true]);
<group of size 2 with 2 generators>
gap> InnerAutomorphismsOfSemigroup(m7);
<group of size 1 with 1 generators>
gap> g1:=Transformation([3,3,2,6,2,4,4,6,3,4,6]);;
gap> g2:=Transformation([4,4,6,1,3,3,3,3,11,11,11]);;
gap> g3:=Transformation([2,2,3,4,4,6,6,6,6,6,11]);;
gap> m8:=Monoid(g1,g2,g3);;
gap> A:=AutomorphismsSemigroupInGroup(m8, fail, [false, true, false, false, true]);
<group of size 8 with 3 generators>
gap> HasInnerAutomorphismsOfSemigroup(m8);
true
gap> InnerAutomorphismsOfSemigroup(m8);
<group of size 2 with 1 generators>
gap> StructureDescription(A);
"C2 x C2 x C2"
gap> g1:=Transformation([3,3,2,6,2,4,4,6,3,4,6]);;
gap> g2:=Transformation([4,4,6,1,3,3,3,3,11,11,11]);;
gap> g3:=Transformation([2,2,3,4,4,6,6,6,6,6,11]);;
gap> g4:=Transformation([2,2,3,4,4,6,6,6,6,11,11]);;
gap> m9:=Monoid(g1, g2, g3, g4);;
gap> A:=AutomorphismsSemigroupInGroup(m9, fail, [false, true, false, false, true]);
<group of size 24 with 4 generators>
gap> StructureDescription(A);
"C2 x C2 x S3"
gap> StructureDescription(InnerAutomorphismsAutomorphismGroup(A));
"C2"
gap> g1:=Transformation( [ 12, 3, 6, 4, 6, 11, 9, 6, 6, 7, 6, 12 ] );;
gap> g2:=Transformation( [ 10, 7, 2, 11, 7, 3, 12, 4, 3, 8, 7, 5 ] );;
gap> m11:=Monoid(g1,g2);;
gap> A:=AutomorphismGroup(m11);
<group of size 1 with 1 generators>
gap> g1:=Transformation( [ 3, 2, 12, 2, 7, 9, 4, 2, 1, 12, 11, 12 ] );; 
gap> g2:=Transformation( [ 3, 6, 12, 7, 2, 2, 3, 6, 1, 7, 11, 1 ] );;
gap> m14:=Monoid(g1, g2);;
gap> A:=AutomorphismGroup(m14);
<group of size 1 with 1 generators>
gap> g1:=Transformation([2,2,3,4,5,6]);;
gap> g2:=Transformation([2,3,4,5,6,1]);; 
gap> m15:=Monoid(g1, g2);;
gap> A:=AutomorphismsSemigroupInGroup(m15, fail, [true, true, false, false, false]);
<group of size 12 with 2 generators>
gap> StructureDescription(A);
"D12"
gap> IsAutomorphismGroupOfSemigroup(A);
true
gap> InnerAutomorphismsAutomorphismGroup(A);
D12
gap> gens:=[ Transformation( [ 1, 3, 1, 5, 3 ] ), 
>   Transformation( [ 1, 3, 3, 5, 2 ] ), Transformation( [ 2, 2, 2, 4, 2 ] ), 
>   Transformation( [ 2, 3, 1, 4, 5 ] ), Transformation( [ 2, 3, 3, 3, 3 ] ), 
>   Transformation( [ 3, 3, 1, 4, 1 ] ), Transformation( [ 3, 3, 5, 5, 4 ] ), 
>   Transformation( [ 4, 4, 4, 5, 2 ] ), Transformation( [ 5, 1, 4, 5, 3 ] ), 
>   Transformation( [ 5, 4, 4, 3, 2 ] ), Transformation( [ 5, 5, 1, 4, 1 ] )];;
gap> S:=Semigroup(gens);;
gap> A:=AutomorphismsSemigroupInGroup(S, fail, [true, true, false, false, false]);
<group of size 6 with 2 generators>
gap> IsInnerAutomorphismsOfSemigroup(A);
true
gap> StructureDescription(A);
"S3"
gap> gens:=[ Transformation( [ 1, 5, 3, 1, 5 ] ), Transformation( [ 2, 2, 5, 3, 2 ] ), 
>   Transformation( [ 3, 1, 5, 2, 4 ] ), Transformation( [ 3, 5, 4, 3, 2 ] ), 
>   Transformation( [ 4, 2, 2, 4, 1 ] ) ];;
gap>   S:=Semigroup(gens);
<semigroup with 5 generators>
gap> A:=AutomorphismsSemigroupInGroup(S, fail, [false, false, false, false, true]);
<group of size 10 with 3 generators>
gap> #previous example has all autos inner, so this is a good way to check that 
gap> #outer automorphisms computation returns the same answer as the inner.
gap> #same with the next example
gap> gens:=[ Transformation( [ 1, 2, 6, 6, 5, 6 ] ), 
>   Transformation( [ 1, 6, 1, 2, 1, 4 ] ), 
>   Transformation( [ 3, 6, 3, 3, 2, 4 ] ), 
>   Transformation( [ 6, 2, 4, 5, 1, 3 ] ) ];;
gap>   S:=Semigroup(gens);
<semigroup with 4 generators>
gap> A:=AutomorphismsSemigroupInGroup(S, fail, [false, false, false, false, false]);
<group of size 20 with 3 generators>
gap> g1:=Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] );;
gap> g2:=Transformation([2,3,4,5,6,8,7,1,2,2]);;
gap> m20:=Monoid(g1,g2);;
gap> A:=AutomorphismGroup(m20); 
<group of size 10080 with 8 generators>
gap> InnerAutomorphismsAutomorphismGroup(A);;
gap> Size(last);
720
gap> gens:=[ Transformation( [1,8,11,2,5,16,13,14,3,6,15,10,7,4,9,12 ] ), 
>   Transformation( [1,16,9,6,5,8,13,12,15,2,3,4,7,10,11,14] ), 
>   Transformation( [1,3,7,9,1,15,5,11,13,11,13,3,5,15,7,9 ] ) ];;
gap> S:=Semigroup(gens);
<semigroup with 3 generators>
gap> A:=AutomorphismGroup(S);;
gap> StructureDescription(last);
"C2 x D8"
gap> gens:=[Transformation([4,4,8,8,8,8,4,8]), Transformation([8,2,8,2,5,5,8,8]), 
> Transformation([8,8,3,7,8,3,7,8]), Transformation([8,6,6,8,6,8,8,8])];;
gap> S:=Semigroup(gens);
<semigroup with 4 generators>
gap> A:=AutomorphismGroup(S);
<group of size 1 with 1 generators>
gap> x:=Transformation([2,3,4,5,6,7,8,9,1]);
Transformation( [ 2, 3, 4, 5, 6, 7, 8, 9, 1 ] )
gap> y:=Transformation([4,2,3,4,5,6,7,8,9]);
Transformation( [ 4, 2, 3, 4, 5, 6, 7, 8, 9 ] )
gap> S:=Semigroup(x,y);
<semigroup with 2 generators>
gap> Size(S);
40266
gap> A:=AutomorphismGroup(S);
<group of size 54 with 4 generators>
gap> gens:=[ Transformation( [ 1, 1, 4, 3, 5, 6, 7, 8, 9, 10, 11, 12 ] ), 
>   Transformation( [ 1, 1, 4, 5, 6, 7, 3, 8, 9, 10, 11, 12 ] ), 
>   Transformation( [ 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 8 ] ) ];;
gap> S:=Semigroup(gens);
<semigroup with 3 generators>
gap> A:=AutomorphismGroup(S);
<group of size 480 with 7 generators>
gap> imgs:=[ [ Transformation( [ 1, 1, 5, 4, 3, 6, 7, 8, 9, 10, 11, 12 ] ), 
>       Transformation( [ 1, 1, 5, 7, 4, 3, 6, 8, 9, 10, 11, 12 ] ), 
>       Transformation( [ 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 8 ] ) ], 
>   [ Transformation( [ 1, 1, 5, 4, 3, 6, 7, 8, 9, 10, 11, 12 ] ), 
>       Transformation( [ 1, 1, 5, 3, 7, 4, 6, 8, 9, 10, 11, 12 ] ), 
>       Transformation( [ 1, 2, 3, 4, 5, 6, 7, 11, 12, 8, 9, 10 ] ) ] ];;
gap> gens:=List(imgs, x-> SemigroupHomomorphismByImagesOfGensNC(S, S, x));;
gap> G:=Group(gens);
<group with 2 generators>
gap> A:=AutomorphismsSemigroupInGroup(S, G, [false, false, false, true, false]);
<group of size 48 with 4 generators>
gap> Size(G);
48
gap> A:=AutomorphismsSemigroupInGroup(S, G);
<group of size 48 with 4 generators>
gap> gens:=[ Transformation( [ 1, 1, 4, 3, 5, 6, 7, 8, 9, 10, 11, 12 ] ), 
>   Transformation( [ 1, 1, 4, 5, 6, 7, 3, 8, 9, 10, 11, 12 ] ), 
>   Transformation( [ 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 8 ] ) ];;
gap> S:=Semigroup(gens);;
gap> A:=AutomorphismsSemigroupInGroup(S, G);
<group of size 48 with 4 generators>
gap> HasAutomorphismGroup(S);
true
gap> AutomorphismGroup(S);
<group of size 480 with 7 generators>
gap> A:=AutomorphismsSemigroupInGroup(S, G, [false, false, false, true, false]);
<group of size 48 with 4 generators>
gap> IsGroupOfAutomorphisms(A);
true
gap> HasAutomorphismGroup(S);
true
gap> A:=AutomorphismGroup(S);
<group of size 480 with 7 generators>
gap> A:=AutomorphismsSemigroupInGroup(S, G);
<group of size 48 with 4 generators>
gap> a:=IdempotentNC([[1,3,4],[2,5],[6],[7],[8]],[3,5,6,7,8])*(3,5);;
gap> b:=IdempotentNC([[1,3,4],[2,5],[6],[7],[8]],[3,5,6,7,8])*(3,6,7,8);;
gap> S:=Semigroup(a,b);;
gap> IsGroupAsSemigroup(S);
true
gap> IsomorphismPermGroup(S);
SemigroupHomomorphism ( <semigroup with 2 generators>->Group(
[ (3,5), (3,6,7,8) ]))
gap> gens:=[Transformation([3,5,3,3,5,6]), Transformation([6,2,4,2,2,6])];;
gap> S:=Semigroup(gens);;
gap> H:=GroupHClassOfGreensDClass(GreensDClassOfElement(S, Elements(S)[1]));
{Transformation( [ 2, 2, 2, 2, 2, 6 ] )}
gap> IsomorphismPermGroup(H);
SemigroupHomomorphism ( {Transformation( [ 2, 2, 2, 2, 2, 6 ] )}->Group(()))
gap> S:=RandomSemigroup(2,2);
<semigroup with 2 generators>
gap> A:=AutomorphismGroup(S);;
gap> g1:=Transformation([5,4,4,2,1]);;
gap> g2:=Transformation([2,5,5,4,1]);;
gap> m2:=Monoid(g1,g2);;
gap> IsTransformationSemigroup(m2);
true
gap> A:=AutomorphismGroup(m2);;
gap> Size(A);
24
gap> IsAutomorphismGroupOfSemigroup(A);
true
gap> G:=ZeroGroup(Group([ (1,3)(2,5), (1,3,2,5) ]));;
gap> elts:=Elements(G);;
gap> mat:=[ [ elts[7], elts[1], elts[9], elts[1], elts[1] ], 
>   [ elts[1], elts[1], elts[1], elts[9], elts[1] ], 
>   [ elts[9], elts[1], elts[1], elts[4], elts[9] ], 
>   [ elts[1], elts[1], elts[1], elts[1], elts[1] ], 
>   [ elts[1], elts[5], elts[1], elts[1], elts[1] ] ];;
gap> rzms:=ReesZeroMatrixSemigroup(G, mat);;
gap> AutomorphismGroup(rzms);
<group of size 512 with 512 generators>
gap> IsAutomorphismGroupOfRZMS(last);
true
gap> G:=ZeroGroup(Group([ (1,3) ]));;
gap> z:=MultiplicativeZero(G);; x:=Elements(G)[2];;
gap> mat:=[ [ z, z, z ], [ z, z, z ], [ z, z, z ], [ z, z, z ], [ z, x, z ] ];;
gap> rzms:=ReesZeroMatrixSemigroup(G, mat);;
gap> Size(rzms);
31
gap> Size(GeneratorsOfSemigroup(rzms));
6
gap> Length(RightTransStabAutoGroup(rzms, GeneratorsOfSemigroup(rzms), OnSets));
512
gap> A:=AutomorphismGroup(rzms);
<group of size 3072 with 3072 generators>
gap> zg:=ZeroGroup(Group(()));;
gap> z:=Elements(zg)[1];
0
gap> x:=Elements(zg)[2];
()
gap> mat:=[ [ z, z, z ], [ x, z, z ], [ x, x, z ] ];;
gap> rzms:=ReesZeroMatrixSemigroup(zg, mat);;
gap> RZMSInducedFunction(rzms, (), One(AutomorphismGroup(zg)), x, [1,2,5,6]);
[ (), (),,, (), () ]
gap> RZMSInducedFunction(rzms, (), One(AutomorphismGroup(zg)), x, [3]);     
[ ,, () ]
gap> RZMSInducedFunction(rzms, (), One(AutomorphismGroup(zg)), x, [4]);
[ ,,, () ]
gap> zg:=ZeroGroup(Group([ (1,5,2,3), (1,4)(2,3) ]));;
gap> elts:=Elements(zg);;
gap> mat:=[ [ elts[1], elts[1], elts[11], elts[1], elts[1] ], 
>    [ elts[1], elts[13], elts[21], elts[1], elts[1] ], 
>    [ elts[1], elts[16], elts[1], elts[16], elts[3] ], 
>    [ elts[10], elts[17], elts[1], elts[1], elts[1] ], 
>    [ elts[1], elts[1], elts[1], elts[4], elts[1] ] ];;
gap> rzms:=ReesZeroMatrixSemigroup(zg, mat);;                                   
gap> RZMSInducedFunction(rzms, (), Random(AutomorphismGroup(zg)), Random(elts), [1..10])=fail;
false
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
gap> gam:=One(AutomorphismGroup(UnderlyingSemigroupOfReesZeroMatrixSemigroup(rms1)));
IdentityMapping( <zero group with 2 generators> )
gap> g:=One(UnderlyingSemigroupOfReesZeroMatrixSemigroup(rms2));
()
gap> RZMStoRZMSInducedFunction(rms1, rms2, (2,3)(5,6), gam, [g]);
[ (), (), (), (), (), () ]
gap> G:=Group((1,4,3,5,2));;
gap> ZG:=ZeroGroup(G);
<zero group with 2 generators>
gap> mat:=[ [ (), (), () ], [ (), (1,4,3,5,2), () ], [ (), (1,3,2,4,5), () ] ];;
gap> mat:=List(mat, x-> List(x, ZeroGroupElt));
[ [ (), (), () ], [ (), (1,4,3,5,2), () ], [ (), (1,3,2,4,5), () ] ]
gap> rms:=ReesZeroMatrixSemigroup(ZG, mat);
Rees Zero Matrix Semigroup over <zero group with 2 generators>
gap> l:=(4,6);;
gap> g:=GroupHomomorphismByImages(G, G, [(1,4,3,5,2)], [(1,2,5,3,4)]);
[ (1,4,3,5,2) ] -> [ (1,2,5,3,4) ]
gap> g:=ZeroGroupAutomorphism(ZG, g);
<mapping: <zero group with 2 generators> -> <zero group with 2 generators> >
gap>  map:=List([(), (1,5,4,2,3), (), (), (), () ], ZeroGroupElt);;
gap> RZMSIsoByTriple(rms, rms, [l, g, map]);
[ (4,6), <mapping: <zero group with 2 generators> -> <zero group with 
    2 generators> >,
  [ ZeroGroup(()), ZeroGroup((1,5,4,2,3)), ZeroGroup(()), ZeroGroup(()),
      ZeroGroup(()), ZeroGroup(()) ] ]
gap> RZMSIsoByTriple(rms, rms, [l, g, [()]]);
[ (4,6), <mapping: <zero group with 2 generators> -> <zero group with 
    2 generators> >, [ () ] ]
gap> IsRZMSIsoByTripleRep(last);
true
gap> zg:=ZeroGroup(Group(()));;
gap> z:=Elements(zg)[1];
0
gap> x:=Elements(zg)[2];
()
gap> mat:=[ [ z, z, z ], [ x, z, z ], [ x, x, z ] ];;
gap> rzms:=ReesZeroMatrixSemigroup(zg, mat);;
gap> RZMSGraph(rzms);
rec( isGraph := true, order := 6, group := Group(()), 
  schreierVector := [ -1, -2, -3, -4, -5, -6 ], 
  adjacencies := [ [ 5, 6 ], [ 6 ], [  ], [  ], [ 1 ], [ 1, 2 ] ], 
  representatives := [ 1, 2, 3, 4, 5, 6 ], names := [ 1, 2, 3, 4, 5, 6 ] )
gap> UndirectedEdges(last);
[ [ 1, 5 ], [ 1, 6 ], [ 2, 6 ] ]
gap> gens:=[ Transformation( [ 4, 4, 8, 8, 8, 8, 4, 8 ] ), 
>   Transformation( [ 8, 2, 8, 2, 5, 5, 8, 8 ] ), 
>   Transformation( [ 8, 8, 3, 7, 8, 3, 7, 8 ] ), 
>   Transformation( [ 8, 6, 6, 8, 6, 8, 8, 8 ] ) ];;
gap> S:=Semigroup(gens);;
gap> D:=GreensDClasses(S);;
gap> rms1:=Range(IsomorphismReesMatrixSemigroupOfDClass(D[1]));;
gap> rms2:=Range(IsomorphismReesMatrixSemigroupOfDClass(D[4]));;
gap> IsomorphismSemigroups(rms1, rms2);
[ (2,3)(5,6), IdentityMapping( <zero group with 2 generators> ), 
  [ ZeroGroup(()), ZeroGroup(()), ZeroGroup(()), ZeroGroup(()),
      ZeroGroup(()), ZeroGroup(()) ] ]
gap> IsomorphismSemigroups(rms2, rms1);
[ (2,3)(5,6), IdentityMapping( <zero group with 2 generators> ), 
  [ ZeroGroup(()), ZeroGroup(()), ZeroGroup(()), ZeroGroup(()),
      ZeroGroup(()), ZeroGroup(()) ] ]
gap> rms2:=Range(IsomorphismReesMatrixSemigroupOfDClass(D[2]));
Group(())
gap> IsomorphismSemigroups(rms2, rms1);
fail
gap> rms2:=RandomReesZeroMatrixSemigroup(5,5,5);;
gap> IsomorphismSemigroups(rms2, rms1);
fail
gap> rms2:=RandomReesMatrixSemigroup(5,5,5);;
gap> IsomorphismSemigroups(rms2, rms1);
fail
gap> IsomorphismSemigroups(rms1, rms2);
fail
gap> STOP_TEST( "autos2.tst 3.1.3", 10000);