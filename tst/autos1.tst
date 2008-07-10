##
## autos1.tst
## Version 3.1.2
## Thu 10 Jul 2008 20:25:38 BST
##

#dirs := DirectoriesPackageLibrary( "monoid", "tst" );
#ReadTest( Filename( dirs, "autos1.tst" ) );

gap> START_TEST("autos1.tst 3.1.2");
gap> LoadPackage("monoid");;
gap> SetInfoLevel(InfoAutos, 0);
gap> gens:=[ Transformation( [1,8,11,2,5,16,13,14,3,6,15,10,7,4,9,12 ] ), 
>   Transformation( [1,16,9,6,5,8,13,12,15,2,3,4,7,10,11,14] ), 
>   Transformation( [1,3,7,9,1,15,5,11,13,11,13,3,5,15,7,9 ] ) ];;
gap> S:=Semigroup(gens);;
gap> InnerAutomorphismsOfSemigroup(S);
<group of size 16 with 3 generators>
gap> gens:=GeneratorsOfSemigroup(S);;
gap> G:=Group(SemigroupHomomorphismByImagesOfGensNC(S, S, gens));
<group with 1 generators>
gap> InnerAutomorphismsOfSemigroupInGroup(S, G);
<group of size 1 with 1 generators>
gap> InnerAutomorphismsOfSemigroupInGroup(S, G, true);
<group of size 1 with 1 generators>
gap> InnerAutomorphismsOfSemigroupInGroup(S, G, false);
<group of size 1 with 1 generators>
gap> gens:=[Transformation([4,4,8,8,8,8,4,8]), Transformation([8,2,8,2,5,5,8,8]), 
> Transformation([8,8,3,7,8,3,7,8]), Transformation([8,6,6,8,6,8,8,8])];;
gap> S:=Semigroup(gens);;
gap> InnerAutomorphismsOfSemigroup(S);
<group of size 1 with 1 generators>
gap> x:=Transformation([2,3,4,5,6,7,8,9,1]);;
gap> y:=Transformation([4,2,3,4,5,6,7,8,9]);;
gap> S:=Semigroup(x,y);;
gap> G:=InnerAutomorphismsOfSemigroup(S);
<group of size 54 with 2 generators>
gap> StructureDescription(last);
"(C9 : C3) : C2"
gap> gens:=[ (1,7,4)(2,5,8), (1,8,3,4,2,6,7,5,9), (1,9,2,7,6,8,4,3,5), 
> (1,6,5,7,3,2,4,9,8), (1,5,6,4,8,9,7,2,3), (1,4,7)(3,9,6), 
> (1,3,8,7,9,5,4,6,2), (1,2,9,4,5,3,7,8,6), (2,8,5)(3,6,9) ];;
gap> gens:=List(gens, x-> InnerAutomorphismOfSemigroupNC(S, x));;
gap> G:=SubgroupNC(G, gens);
<group with 9 generators>
gap> InnerAutomorphismsOfSemigroupInGroup(S, G); 
<group of size 27 with 2 generators>
gap> InnerAutomorphismsOfSemigroupInGroup(S, G, false);
<group of size 27 with 2 generators>
gap> gens:=[ Transformation( [ 2, 1, 1, 2, 3 ] ), Transformation( [ 2, 4, 1, 2, 3 ] ), 
>   Transformation( [ 2, 4, 5, 5, 5 ] ), Transformation( [ 3, 4, 3, 4, 2 ] ), 
>   Transformation( [ 4, 2, 4, 2, 4 ] ) ];;
gap> S:=Semigroup(gens);;
gap> InnerAutomorphismsOfSemigroup(S);
<group of size 1 with 1 generators>
gap> gens:=[ Transformation( [ 1, 3, 1, 5, 3 ] ), 
>   Transformation( [ 1, 3, 3, 5, 2 ] ), Transformation( [ 2, 2, 2, 4, 2 ] ), 
>   Transformation( [ 2, 3, 1, 4, 5 ] ), Transformation( [ 2, 3, 3, 3, 3 ] ), 
>   Transformation( [ 3, 3, 1, 4, 1 ] ), Transformation( [ 3, 3, 5, 5, 4 ] ), 
>   Transformation( [ 4, 4, 4, 5, 2 ] ), Transformation( [ 5, 1, 4, 5, 3 ] ), 
>   Transformation( [ 5, 4, 4, 3, 2 ] ), Transformation( [ 5, 5, 1, 4, 1 ] )];;
gap> S:=Semigroup(gens);;
gap> InnerAutomorphismsOfSemigroup(S);
<group of size 6 with 2 generators>
gap> gens:=[ Transformation( [ 1, 2, 2, 3, 5 ] ), Transformation( [ 2, 1, 4, 5, 3 ] ), 
>   Transformation( [ 3, 3, 1, 2, 5 ] ), Transformation( [ 3, 4, 1, 4, 4 ] ), 
>   Transformation( [ 3, 5, 4, 2, 3 ] ), Transformation( [ 4, 4, 5, 5, 4 ] ), 
>   Transformation( [ 4, 5, 5, 4, 4 ] ), Transformation( [ 5, 4, 2, 2, 1 ] ) ];;
gap> S:=Semigroup(gens);;
gap> InnerAutomorphismsOfSemigroup(S);
<group of size 12 with 2 generators>
gap> StructureDescription(last);
"D12"
gap> gens:=[ Transformation( [ 1, 5, 3, 1, 5 ] ), Transformation( [ 2, 2, 5, 3, 2 ] ), 
>   Transformation( [ 3, 1, 5, 2, 4 ] ), Transformation( [ 3, 5, 4, 3, 2 ] ), 
>   Transformation( [ 4, 2, 2, 4, 1 ] ) ];;
gap>   S:=Semigroup(gens);;
gap> InnerAutomorphismsOfSemigroup(S);
<group of size 10 with 2 generators>
gap> StructureDescription(last);
"D10"
gap> gens:=[ Transformation( [ 1, 2, 6, 6, 5, 6 ] ), 
>   Transformation( [ 1, 6, 1, 2, 1, 4 ] ), 
>   Transformation( [ 3, 6, 3, 3, 2, 4 ] ), 
>   Transformation( [ 6, 2, 4, 5, 1, 3 ] ) ];;
gap>   S:=Semigroup(gens);;
gap>   InnerAutomorphismsOfSemigroup(S);
<group of size 20 with 2 generators>
gap> StructureDescription(last);
"C5 : C4"
gap> gens:=[ Transformation( [ 2, 2, 6, 4, 3, 4 ] ), 
>   Transformation( [ 3, 4, 1, 5, 2, 6 ] ), 
>   Transformation( [ 3, 4, 3, 5, 2, 1 ] ), 
>   Transformation( [ 5, 2, 6, 5, 5, 3 ] ) ];;
gap> S:=Semigroup(gens);;
gap> InnerAutomorphismsOfSemigroup(S);
<group of size 6 with 1 generators>
gap>   gens:=[ Transformation( [ 1, 2, 3, 4, 3 ] ), Transformation( [ 1, 3, 2, 4, 3 ] ), 
>   Transformation( [ 2, 2, 3, 5, 1 ] ), Transformation( [ 2, 5, 1, 2, 3 ] ), 
>   Transformation( [ 3, 3, 2, 2, 5 ] ), Transformation( [ 5, 5, 2, 3, 2 ] ) ];;
gap>   S:=Semigroup(gens);;
gap> InnerAutomorphismsOfSemigroup(S);
<group of size 1 with 1 generators>
gap> gens:=[ Transformation( [ 1, 3, 1, 5, 3 ] ), Transformation( [ 2, 5, 3, 1, 4 ] ), 
>   Transformation( [ 4, 1, 1, 3, 4 ] ), Transformation( [ 4, 1, 3, 4, 5 ] ), 
>   Transformation( [ 5, 1, 3, 5, 2 ] ), Transformation( [ 5, 3, 2, 3, 3 ] ) ];;
gap> S:=Semigroup(gens);;
gap> InnerAutomorphismsOfSemigroup(S);
<group of size 8 with 2 generators>
gap> StructureDescription(last);
"D8"
gap> gens:=[ Transformation( [ 2, 2, 2, 3, 1 ] ), Transformation( [ 2, 4, 5, 3, 1 ] ), 
>   Transformation( [ 3, 1, 3, 2, 2 ] ), Transformation( [ 3, 1, 4, 5, 2 ] ), 
>   Transformation( [ 4, 1, 2, 1, 2 ] ), Transformation( [ 4, 3, 4, 2, 4 ] ) ];;
gap> S:=Semigroup(gens);
<semigroup with 6 generators>
gap> InnerAutomorphismsOfSemigroup(S);
<group of size 120 with 2 generators>
gap> gens:=[Transformation([3,5,3,3,5,6]), Transformation([6,2,4,2,2,6])];;
gap> S:=Semigroup(gens);;
gap> InnerAutomorphismsOfSemigroup(S);
<group of size 1 with 1 generators>
gap> G:=CyclicGroup(3);;
gap> R:=GF(2);;
gap> GR:=GroupRing(R, G);;
gap> iso:=IsomorphismTransformationSemigroup(GR);;
gap> S:=Range(iso);;
gap> imgs:=[ [ Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1 ] ), 
>       Transformation( [ 1, 8, 5, 4, 7, 2, 3, 6 ] ), 
>       Transformation( [ 1, 7, 3, 1, 5, 5, 7, 3 ] ), 
>       Transformation( [ 1, 5, 7, 1, 3, 3, 5, 7 ] ), 
>       Transformation( [ 1, 3, 5, 1, 7, 7, 3, 5 ] ), 
>       Transformation( [ 1, 6, 7, 4, 3, 8, 5, 2 ] ), 
>       Transformation( [ 1, 4, 1, 4, 1, 4, 1, 4 ] ), 
>       Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8 ] ) ], 
>   [ Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1 ] ), 
>       Transformation( [ 1, 8, 5, 4, 7, 2, 3, 6 ] ), 
>       Transformation( [ 1, 7, 3, 1, 5, 5, 7, 3 ] ), 
>       Transformation( [ 1, 6, 7, 4, 3, 8, 5, 2 ] ), 
>       Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8 ] ), 
>       Transformation( [ 1, 5, 7, 1, 3, 3, 5, 7 ] ), 
>       Transformation( [ 1, 4, 1, 4, 1, 4, 1, 4 ] ), 
>       Transformation( [ 1, 3, 5, 1, 7, 7, 3, 5 ] ) ] ];;
gap> G:=Group(List(imgs, x-> SemigroupHomomorphismByImagesOfGensNC(S, S, x)));;
gap> Size(G);
168
gap> InnerAutomorphismsOfSemigroupInGroup(S, G);
<group of size 1 with 1 generators>
gap> InnerAutomorphismsOfSemigroupInGroup(S, G, true);
<group of size 1 with 1 generators>
gap> InnerAutomorphismsOfSemigroupInGroup(S, G, false);
<group of size 1 with 1 generators>
gap> InnerAutomorphismsOfSemigroup(S);
<group of size 2 with 1 generators>
gap> gens:=[ Transformation( [ 1, 2, 6, 6, 5, 6 ] ), 
>    Transformation( [ 1, 6, 1, 2, 1, 4 ] ), 
>    Transformation( [ 3, 6, 3, 3, 2, 4 ] ), 
>    Transformation( [ 6, 2, 4, 5, 1, 3 ] ) ];;
gap> S:=Semigroup(gens);;
gap> G:=InnerAutomorphismsOfSemigroup(S);
<group of size 20 with 2 generators>
gap> gens:=[ (1,6)(3,5), (1,5)(4,6), (3,4)(5,6), (1,4)(3,6), (1,3)(4,5) ];;
gap> gens:=List(gens, x-> InnerAutomorphismOfSemigroupNC(S, x));;
gap> G:=SubgroupNC(G, gens);
<group with 5 generators>
gap> InnerAutomorphismsOfSemigroupInGroup(S, G);
<group of size 10 with 2 generators>
gap> InnerAutomorphismsOfSemigroupInGroup(S, G, false);
<group of size 10 with 2 generators>
gap> gens:=[ Transformation( [ 2, 2, 2, 3, 1 ] ), Transformation( [ 2, 4, 5, 3, 1 ] ), 
>   Transformation( [ 3, 1, 3, 2, 2 ] ), Transformation( [ 3, 1, 4, 5, 2 ] ), 
>   Transformation( [ 4, 1, 2, 1, 2 ] ), Transformation( [ 4, 3, 4, 2, 4 ] ) ];;
gap> S:=Semigroup(gens);;
gap> G:=InnerAutomorphismsOfSemigroup(S);
<group of size 120 with 2 generators>
gap> G:=Group(AsList(G)[4]);;
gap> InnerAutomorphismsOfSemigroupInGroup(S, G);
<group of size 6 with 1 generators>
gap> InnerAutomorphismsOfSemigroupInGroup(S, G, true);
<group of size 6 with 1 generators>
gap> STOP_TEST( "autos1.tst 3.1.2", 10000);