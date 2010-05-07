#############################################################################
##
#W  semihomo.tst
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

#ReadTest( Filename(DirectoriesPackageLibrary( "monoid", "tst" ), "semihomo.tst" ) );

gap> START_TEST("semihomo.tst 3.1.3");
gap> LoadPackage("monoid");;
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
gap> gens:=[ Transformation( [ 2, 8, 8, 1, 4, 2, 7, 8 ] ), 
> Transformation( [ 3, 4, 2, 4, 3, 8, 8, 1 ] ), 
> Transformation( [ 6, 8, 7, 2, 8, 7, 4, 4 ] ) ];;
gap> S:=Semigroup(gens);;
gap> gens:=[ Transformation( [ 2, 3, 4, 4, 5 ] ),
> Transformation( [ 3, 2, 5, 3, 4 ] ) ];;
gap> T:=Semigroup(gens);;
gap> D:=GreensDClassOfElement(S, Transformation( [ 1, 1, 1, 1, 2, 1, 8, 1 ] ));;
gap> hom1:=SemigroupHomomorphismByFunctionNC(D, T, x-> Random(Elements(T)));
SemigroupHomomorphism ( {Transformation( [ 1, 1, 1, 1, 2, 1, 8, 1 
 ] )}-><semigroup with 2 generators>)
gap> hom2:=SemigroupHomomorphismByFunctionNC(T, D, x-> Random(Elements(D)));
SemigroupHomomorphism ( <semigroup with 2 generators>->{Transformation( 
[ 1, 1, 1, 1, 2, 1, 8, 1 ] )})
gap> x:=Representative(D);
Transformation( [ 1, 1, 1, 1, 2, 1, 8, 1 ] )
gap> y:=ImageElm(hom1, x);;
gap> ImageElm(hom2, y);;
gap> hom:=SemigroupHomomorphismByFunctionNC(D, D, y-> y*x);
SemigroupHomomorphism ( {Transformation( [ 1, 1, 1, 1, 2, 1, 8, 1 
 ] )}->{Transformation( [ 1, 1, 1, 1, 2, 1, 8, 1 ] )})
gap> SetX(Elements(D), x-> ImageElm(hom, x));
[ Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 1, 8, 1 ] ), 
  Transformation( [ 1, 1, 1, 1, 8, 1, 1, 1 ] ), 
  Transformation( [ 8, 8, 8, 8, 1, 8, 1, 8 ] ) ]
gap> H:=GreensHClassOfElement(T, Transformation( [ 3, 4, 3, 3, 3 ] ));;
gap> hom:=SemigroupHomomorphismByFunctionNC(H, S, y-> x);
SemigroupHomomorphism ( {Transformation( [ 3, 4, 3, 3, 3 
 ] )}-><semigroup with 3 generators>)
gap> ForAll(H, y-> ImageElm(hom, y)=x);
true
gap> hom:=SemigroupHomomorphismByFunctionNC(S, H, y-> Representative(H));
SemigroupHomomorphism ( <semigroup with 3 generators>->{Transformation( 
[ 3, 4, 3, 3, 3 ] )})
gap> ForAll(S, y-> ImageElm(hom, y)=Representative(H));
true
gap> hom1*hom2;
SemigroupHomomorphism ( {Transformation( [ 1, 1, 1, 1, 2, 1, 8, 1 
 ] )}->{Transformation( [ 1, 1, 1, 1, 2, 1, 8, 1 ] )})
gap> hom:=last;
SemigroupHomomorphism ( {Transformation( [ 1, 1, 1, 1, 2, 1, 8, 1 
 ] )}->{Transformation( [ 1, 1, 1, 1, 2, 1, 8, 1 ] )})
gap> SetX(D, x-> ImageElm(hom, x));;
gap> SetX(D, x-> ImageElm(hom, x))=Elements(D);
false
gap> STOP_TEST( "semihomo.tst 3.1.3", 10000);