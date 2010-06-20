#############################################################################
##
#W  cong_no_smallsemi.tst
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

#ReadTest( Filename( DirectoriesPackageLibrary( "monoid", "tst" ), "cong_no_smallsemi.tst" ) );

gap> START_TEST("cong_no_smallsemi.tst 3.1.4");
gap> LoadPackage("monoid");;
gap> cong_smallsemi_info_level:=InfoLevel(InfoMonoidCongruences);;
gap> SetInfoLevel(InfoMonoidCongruences, 0);;
gap> gens:=[ Transformation( [ 1, 2, 6, 4, 5, 6 ] ), 
> Transformation( [ 1, 2, 6, 4, 6, 6 ] ), 
> Transformation( [ 1, 2, 6, 6, 6, 6 ] ), 
> Transformation( [ 1, 6, 3, 4, 6, 6 ] ), 
> Transformation( [ 1, 6, 6, 4, 6, 6 ] ), 
> Transformation( [ 1, 6, 6, 6, 6, 6 ] ), 
> Transformation( [ 6, 2, 3, 4, 6, 6 ] ), 
> Transformation( [ 6, 2, 6, 4, 6, 6 ] ), 
> Transformation( [ 6, 2, 6, 6, 6, 6 ] ), 
> Transformation( [ 6, 6, 3, 4, 6, 6 ] ), 
> Transformation( [ 6, 6, 3, 6, 6, 6 ] ), 
> Transformation( [ 6, 6, 6, 4, 6, 6 ] ), 
> Transformation( [ 6, 6, 6, 6, 6, 6 ] ) ];;
gap> s:=Semigroup(gens);;
gap> CongruencesSemilatticeAsPositions(s);;
#gap> time;
#~200
gap> Length(last);
1086
gap> ForAll(CongruencesSemilattice(s), x-> IsCongruenceOfSemigroupNC(s, x));
true
gap> gens:=[ Transformation( [ 1, 2, 3, 4, 5, 10, 10, 8, 10, 10 ] ), 
>  Transformation( [ 1, 10, 3, 10, 5, 10, 10, 10, 9, 10 ] ), 
>  Transformation( [ 1, 2, 3, 4, 10, 6, 10, 10, 10, 10 ] ), 
>  Transformation( [ 1, 10, 10, 4, 5, 6, 10, 8, 10, 10 ] ), 
>  Transformation( [ 1, 10, 10, 4, 5, 10, 10, 8, 10, 10 ] ), 
>  Transformation( [ 1, 2, 3, 10, 5, 10, 10, 8, 9, 10 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
16
gap> CongruencesSemilattice(s);;
#gap> time;
#962
gap> Length(last);
5056
gap> ForAll(last2, x-> IsCongruenceOfSemigroupNC(s, x));
true
gap> gens:=[ Transformation( [ 1, 2, 10, 10, 10, 10, 7, 10, 10, 10 ] ), 
>  Transformation( [ 1, 2, 3, 10, 5, 6, 10, 10, 9, 10 ] ), 
>  Transformation( [ 10, 10, 10, 4, 10, 10, 10, 10, 10, 10 ] ), 
>  Transformation( [ 1, 2, 3, 10, 5, 10, 7, 8, 9, 10 ] ), 
>  Transformation( [ 10, 10, 10, 10, 10, 10, 10, 8, 10, 10 ] ), 
>  Transformation( [ 10, 10, 10, 4, 5, 6, 10, 8, 9, 10 ] ), 
>  Transformation( [ 10, 2, 10, 4, 5, 10, 7, 10, 10, 10 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
19
gap> CongruencesSemilattice(s);;
#gap> time;
#5276
gap> Length(last);
19022
#gap> ForAll(last2, x-> IsCongruenceOfSemilattice(s, x));
#true
#gap> a:=Transformation( [ 2, 5, 1, 7, 3, 7, 7 ] );;
#gap> b:=Transformation( [ 3, 6, 5, 7, 2, 1, 7 ] );;
#gap> j1:=Semigroup(a,b);;
#gap> s:=Semigroup(Idempotents(j1));
#gap> Size(s);
#32
#gap> CongruencesSemilatticeAsPositions(s);;
#gap> StringTime(time);
#" 0:18:57.499"
#gap> Length(last);
#1385552
gap> gens:=[ Transformation( [ 4, 2, 3, 4 ] ),
> Transformation( [ 1, 4, 4, 4 ] ),                                           
> Transformation( [ 4, 2, 3, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsSemilatticeAsSemigroup(s);
true
gap> CongruencesSemilattice(Semigroup(gens))=
> [ [ [ Transformation( [ 1, 4, 4, 4 ] ) ], 
> [ Transformation( [ 4, 2, 3, 4 ] ) ], 
>      [ Transformation( [ 4, 4, 4, 4 ] ) ] ], 
>  [ [ Transformation( [ 1, 4, 4, 4 ] ) ], 
>      [ Transformation( [ 4, 2, 3, 4 ] ), Transformation( [ 4, 4, 4, 4 ] ) ] ], 
>  [ [ Transformation( [ 1, 4, 4, 4 ] ), Transformation( [ 4, 2, 3, 4 ] ), 
>          Transformation( [ 4, 4, 4, 4 ] ) ] ], 
>  [ [ Transformation( [ 1, 4, 4, 4 ] ), Transformation( [ 4, 4, 4, 4 ] ) ], 
>      [ Transformation( [ 4, 2, 3, 4 ] ) ] ] ];
true
gap> CongruencesSemilatticeAsPositions(s);
[ [ [ 1 ], [ 2 ], [ 3 ] ], [ [ 1 ], [ 2, 3 ] ], [ [ 1, 2, 3 ] ], 
  [ [ 1, 3 ], [ 2 ] ] ]
gap> gens:=[ Transformation( [ 1, 2, 6, 4, 6, 6 ] ), 
> Transformation( [ 6, 2, 3, 4, 6, 6 ] ), 
> Transformation( [ 1, 6, 3, 4, 6, 6 ] ), 
> Transformation( [ 6, 6, 6, 6, 6, 6 ] ) ];;
gap> a:=Semigroup(gens);;
gap> Size(a);
8
gap> f:=Elements(a)[1];
Transformation( [ 1, 2, 6, 4, 6, 6 ] )
gap> g:=Elements(a)[8];
Transformation( [ 6, 6, 6, 6, 6, 6 ] )
gap> ConvexSubsemigroup(a, [f,g]);  
[ Transformation( [ 1, 2, 6, 4, 6, 6 ] ),
  Transformation( [ 1, 6, 6, 4, 6, 6 ] ),
  Transformation( [ 6, 2, 6, 4, 6, 6 ] ),
  Transformation( [ 6, 6, 6, 4, 6, 6 ] ),
  Transformation( [ 6, 6, 6, 6, 6, 6 ] ) ]
gap> ConvexSubsemigroupNC(a, [f,g]);
[ Transformation( [ 1, 2, 6, 4, 6, 6 ] ),
  Transformation( [ 1, 6, 6, 4, 6, 6 ] ),
  Transformation( [ 6, 2, 6, 4, 6, 6 ] ),
  Transformation( [ 6, 6, 6, 4, 6, 6 ] ),
  Transformation( [ 6, 6, 6, 6, 6, 6 ] ) ]
gap> a:=Semigroup(Elements(a));; cayley:=CayleyGraphSemigroup(a);;
gap> subset:=[1,8];;
gap> ConvexSubsemigroupAsPositionsNC(cayley, subset);
[ 1, 3, 5, 7, 8 ]
gap> gens:=[ Transformation( [ 1, 2 ] ), Transformation( [ 2, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
2
gap> ConvexSubsemigroups(s);
[ [ Transformation( [ 1, 2 ] ) ], 
  [ Transformation( [ 1, 2 ] ), Transformation( [ 2, 2 ] ) ], 
  [ Transformation( [ 2, 2 ] ) ] ]
gap> gens:=[ Transformation( [ 3, 2, 3 ] ), Transformation( [ 1, 2, 3 ] ), 
> Transformation( [ 1, 3, 3 ] ) ];;
gap> s:=Semigroup(gens);;
gap> Size(s);
4
gap> ConvexSubsemigroupsAsPositions(s);
[ [ 1 ], [ 1, 2 ], [ 1, 2, 3, 4 ], [ 1, 3 ], [ 2 ], [ 2, 3, 4 ], [ 2, 4 ], 
  [ 3 ], [ 3, 4 ], [ 4 ] ]
gap> gens:=[ Transformation( [ 5, 2, 3, 4, 5 ] ), 
> Transformation( [ 1, 5, 3, 4, 5 ] ), 
> Transformation( [ 1, 5, 5, 4, 5 ] ), 
> Transformation( [ 5, 2, 5, 4, 5 ] ) ];;
gap> s:=Semigroup(gens);;
gap> p:=[ [ Transformation( [ 1, 5, 3, 4, 5 ] ) ],
> [ Transformation( [ 1, 5, 5, 4, 5 ] ), 
> Transformation( [ 5, 5, 3, 4, 5 ] ), 
> Transformation( [ 5, 5, 5, 4, 5 ] ) ], 
> [ Transformation( [ 5, 2, 3, 4, 5 ] ) ], 
> [ Transformation( [ 5, 2, 5, 4, 5 ] ) ] ];;
gap> IsCongruenceOfSemigroup(s, p);
true
gap> p:=[ [ Transformation( [ 1, 5, 3, 4, 5 ] ), 
> Transformation( [ 1, 5, 5, 4, 5 ] ), 
> Transformation( [ 5, 5, 3, 4, 5 ] ), 
> Transformation( [ 5, 5, 5, 4, 5 ] ) ], 
> [ Transformation( [ 5, 2, 3, 4, 5 ] ) ], 
> [ Transformation( [ 5, 2, 3, 4, 5 ] ) ] ];;
gap> IsCongruenceOfSemigroupNC(s, p);
false
gap> gens:=[ Transformation( [ 1, 2, 6, 4, 6, 6 ] ), 
> Transformation( [ 6, 2, 3, 4, 6, 6 ] ), 
> Transformation( [ 1, 6, 3, 4, 6, 6 ] ), 
> Transformation( [ 6, 6, 6, 6, 6, 6 ] ) ];;
gap> a:=Semigroup(gens);;
gap> f:=Elements(a)[1];;
gap> g:=Elements(a)[8];;
gap> IsConvexSubsetOfSemilattice(a,[f,g]);
false
gap> IsConvexSubsetOfSemilatticeNC(a,[f,g]);
false
gap> IsConvexSubsetOfSemilatticeNC(a, ConvexSubsemigroup(a, [f,g]));
true
gap> gens:=[ Transformation( [ 1, 2, 4, 4 ] ), 
> Transformation( [ 1, 4, 4, 4 ] ), 
> Transformation( [ 4, 2, 3, 4 ] ), 
> Transformation( [ 4, 2, 4, 4 ] ), 
> Transformation( [ 4, 4, 3, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> OneMinEltSemilattice(s);
Transformation( [ 4, 4, 4, 4 ] )
gap> OneMinEltSemilatticeAsPosition(s);
6
gap> OneMinEltSemilatticeAsPosition(s, [1,2,3]);
2
gap> OneMinEltSemilatticeAsPosition(s, [2,5,6]);
6
gap> SetInfoLevel(InfoMonoidCongruences, cong_smallsemi_info_level);;
gap> Unbind(cong_smallsemi_info_level);;
gap> STOP_TEST( "cong_no_smallsemi.tst 3.1.4", 10000);