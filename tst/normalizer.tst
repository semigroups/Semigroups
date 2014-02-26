#############################################################################
##
#W  normalizer.tst
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

gap> START_TEST("Semigroups package: normalizer.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#
gap> S:=Semigroup(
> Transformation( [ 1, 8, 11, 2, 5, 16, 13, 14, 3, 6, 15, 10, 7, 4, 9, 12 ] ),
> Transformation( [ 1, 16, 9, 6, 5, 8, 13, 12, 15, 2, 3, 4, 7, 10, 11, 14 ] ),
> Transformation( [ 1, 3, 7, 9, 1, 15, 5, 11, 13, 11, 13, 3, 5, 15, 7, 9 ] ) );;
gap> Size(Normalizer(SymmetricGroup(16), S));
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
16

#
gap> S:=Semigroup( Transformation( [ 4, 4, 8, 8, 8, 8, 4, 8 ] ), 
> Transformation( [ 8, 2, 8, 2, 5, 5, 8, 8 ] ), 
> Transformation( [ 8, 8, 3, 7, 8, 3, 7, 8 ] ), 
> Transformation( [ 8, 6, 6, 8, 6, 8, 8, 8 ] ) );;
gap> Normalizer(SymmetricGroup(8), S); 
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
Group(())
gap> Size(last);
1

#
gap> S:=Semigroup(Transformation( [ 2, 3, 4, 5, 6, 7, 8, 9, 1 ] ), 
> Transformation( [ 4, 2, 3, 4 ] ));;
gap> Size(Normalizer(SymmetricGroup(9), S));
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
54

#
gap> S:=Semigroup( Transformation( [ 1, 3, 1, 5, 3 ] ), 
>   Transformation( [ 1, 3, 3, 5, 2 ] ), Transformation( [ 2, 2, 2, 4, 2 ] ), 
>   Transformation( [ 2, 3, 1, 4, 5 ] ), Transformation( [ 2, 3, 3, 3, 3 ] ), 
>   Transformation( [ 3, 3, 1, 4, 1 ] ), Transformation( [ 3, 3, 5, 5, 4 ] ), 
>   Transformation( [ 4, 4, 4, 5, 2 ] ), Transformation( [ 5, 1, 4, 5, 3 ] ), 
>   Transformation( [ 5, 4, 4, 3, 2 ] ), Transformation( [ 5, 5, 1, 4, 1 ] ) );;
gap> Size(Normalizer(SymmetricGroup(5), S));
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
6

#
gap> S:=Semigroup( Transformation( [ 1, 3, 1, 5, 3 ] ), 
>   Transformation( [ 1, 3, 3, 5, 2 ] ), Transformation( [ 2, 2, 2, 4, 2 ] ), 
>   Transformation( [ 2, 3, 1, 4, 5 ] ), Transformation( [ 2, 3, 3, 3, 3 ] ), 
>   Transformation( [ 3, 3, 1, 4, 1 ] ), Transformation( [ 3, 3, 5, 5, 4 ] ), 
>   Transformation( [ 4, 4, 4, 5, 2 ] ), Transformation( [ 5, 1, 4, 5, 3 ] ), 
>   Transformation( [ 5, 4, 4, 3, 2 ] ), Transformation( [ 5, 5, 1, 4, 1 ] ) );;
gap> Size(Normalizer(SymmetricGroup(5), S));
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
12

#
gap> S:=Semigroup(Transformation( [ 1, 5, 3, 1, 5 ] ), 
> Transformation( [ 2, 2, 5, 3, 2 ] ), 
> Transformation( [ 3, 1, 5, 2, 4 ] ), Transformation( [ 3, 5, 4, 3, 2 ] ), 
> Transformation( [ 4, 2, 2, 4, 1 ] ) );;
gap> Size(Normalizer(SymmetricGroup(5), S));
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
10

#
gap> S:=Semigroup( Transformation( [ 1, 2, 6, 6, 5, 6 ] ), 
>   Transformation( [ 1, 6, 1, 2, 1, 4 ] ), 
>   Transformation( [ 3, 6, 3, 3, 2, 4 ] ), 
>   Transformation( [ 6, 2, 4, 5, 1, 3 ] ) );;
gap> G:=Normalizer(SymmetricGroup(6), S);;
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
gap> Size(G);
20
gap>  StructureDescription(G);
"C5 : C4"

#
gap> S:=Semigroup( Transformation( [ 2, 2, 6, 4, 3, 4 ] ), 
>   Transformation( [ 3, 4, 1, 5, 2, 6 ] ), 
>   Transformation( [ 3, 4, 3, 5, 2, 1 ] ), 
>   Transformation( [ 5, 2, 6, 5, 5, 3 ] ) );;
gap> Size(S);
12402
gap> Size(Normalizer(SymmetricGroup(6), S));
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
6

#
gap> S:=Semigroup(
> Transformation( [ 1, 2, 3, 4, 3 ] ), Transformation( [ 1, 3, 2, 4, 3 ] ), 
> Transformation( [ 2, 2, 3, 5, 1 ] ), Transformation( [ 2, 5, 1, 2, 3 ] ), 
> Transformation( [ 3, 3, 2, 2, 5 ] ), Transformation( [ 5, 5, 2, 3, 2 ] ) );;
gap> Normalizer(SymmetricGroup(5), S); 
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
Group(())

#
gap> S:=Semigroup( Transformation( [ 1, 3, 1, 5, 3 ] ), 
> Transformation( [ 2, 5, 3, 1, 4 ] ), 
> Transformation( [ 4, 1, 1, 3, 4 ] ), Transformation( [ 4, 1, 3, 4, 5 ] ), 
> Transformation( [ 5, 1, 3, 5, 2 ] ), Transformation( [ 5, 3, 2, 3, 3 ] ) );;
gap> StructureDescription(Normalizer(SymmetricGroup(5), S));
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
"D8"

#
gap> S:=Semigroup( Transformation( [ 2, 2, 2, 3, 1 ] ), 
> Transformation( [ 2, 4, 5, 3, 1 ] ), 
> Transformation( [ 3, 1, 3, 2, 2 ] ), Transformation( [ 3, 1, 4, 5, 2 ] ), 
> Transformation( [ 4, 1, 2, 1, 2 ] ), Transformation( [ 4, 3, 4, 2, 4 ] ) );;
gap> Size(Normalizer(SymmetricGroup(5), S));
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
120

#
gap> S:=Semigroup( Transformation( [ 3, 5, 3, 3, 5 ] ), 
> Transformation( [ 6, 2, 4, 2, 2, 6 ] ) );;
gap> Normalizer(SymmetricGroup(6), S);
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
Group(())

#
gap> S:=Semigroup( Transformation( [ 1, 2, 6, 6, 5, 6 ] ), 
>    Transformation( [ 1, 6, 1, 2, 1, 4 ] ), 
>    Transformation( [ 3, 6, 3, 3, 2, 4 ] ), 
>    Transformation( [ 6, 2, 4, 5, 1, 3 ] ) );;
gap> Size(Normalizer(SymmetricGroup(6), S));
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
20

#
gap> S:=Semigroup( 
> Transformation( [ 2, 2, 2, 3, 1 ] ), Transformation( [ 2, 4, 5, 3, 1 ] ), 
> Transformation( [ 3, 1, 3, 2, 2 ] ), Transformation( [ 3, 1, 4, 5, 2 ] ), 
> Transformation( [ 4, 1, 2, 1, 2 ] ), Transformation( [ 4, 3, 4, 2, 4 ] ) );;
gap> Size(Normalizer(SymmetricGroup(5), S));
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
120

#
gap> S:=Semigroup(AsTransformation((1,2,3,4,5,6,7,8,9)), 
> ConstantTransformation(9, 1));;
gap> Size(Normalizer(SymmetricGroup(9), S));
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
#I  Have 40320 points.
54
gap> Size(Normalizer(SymmetricGroup(9), MinimalIdeal(S)));
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
362880

#
gap> SemigroupsStopTest();

#
gap> STOP_TEST( "Semigroups package: normalizer.tst", 0);
