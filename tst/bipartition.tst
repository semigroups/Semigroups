#############################################################################
##
#W  bipartition.tst
#Y  Copyright (C) 2014                                    Attila Egri-Nagy
##                                                       James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

gap> START_TEST("Semigroups package: bipartition.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#IsomorphismTransformationMonoid, IsomorphismTransformationSemigroup
gap> S:=DualSymmetricInverseMonoid(4);                                
<inverse bipartition monoid on 4 pts with 3 generators>
gap> IsomorphismTransformationMonoid(S);
MappingByFunction( <inverse bipartition monoid of size 339, 
 on 4 pts with 3 generators>, <transformation monoid 
 on 339 pts with 3 generators>, function( x ) ... end, function( x ) ... end )
gap> S:=Semigroup( Bipartition( [ [ 1, 2, 3, 4, -2, -3 ], [ -1 ], [ -4 ] ] ), 
>  Bipartition( [ [ 1, 2, -1, -3 ], [ 3, 4, -2, -4 ] ] ), 
>  Bipartition( [ [ 1, 3, -1 ], [ 2, 4, -2, -3 ], [ -4 ] ] ), 
>  Bipartition( [ [ 1, -4 ], [ 2 ], [ 3, -2 ], [ 4, -1 ], [ -3 ] ] ) );;
gap> IsomorphismTransformationSemigroup(S);
MappingByFunction( <bipartition semigroup of size 284, 
 on 4 pts with 4 generators>, <transformation semigroup 
 on 285 pts with 4 generators>, function( x ) ... end, function( x ) ... end )
gap> S:=Monoid(Bipartition( [ [ 1, 2, -2 ], [ 3 ], [ 4, -3, -4 ], [ -1 ] ] ), 
>  Bipartition( [ [ 1, 3, -3, -4 ], [ 2, 4, -1, -2 ] ] ), 
>  Bipartition( [ [ 1, -1, -2 ], [ 2, 3, -3, -4 ], [ 4 ] ] ), 
>  Bipartition( [ [ 1, 4, -4 ], [ 2, -1 ], [ 3, -2, -3 ] ] ) );;
gap> IsomorphismTransformationMonoid(S);
MappingByFunction( <bipartition monoid of size 41, on 4 pts with 4 generators>
 , <transformation monoid on 41 pts with 4 generators>
 , function( x ) ... end, function( x ) ... end )

#the number of iterations, change here to get faster test
gap> N := 333;;

# BASICS
gap> classes:=[[1,2,3, -2], [4, -5], [5, -7], [6, -3, -4], [7], [-1], [-6]];;
gap> f:=BipartitionNC(classes);
<bipartition: [ 1, 2, 3, -2 ], [ 4, -5 ], [ 5, -7 ], [ 6, -3, -4 ], [ 7 ], 
[ -1 ], [ -6 ]>
gap> LeftProjection(f);
<bipartition: [ 1, 2, 3, -1, -2, -3 ], [ 4, -4 ], [ 5, -5 ], [ 6, -6 ], 
[ 7 ], [ -7 ]>

# different order of classes
gap> classes2:=[[-6], [1,2,3, -2], [4, -5], [5, -7], [6, -3, -4], [-1], [7]];;
gap> f = Bipartition(classes2);
true
gap> f:=BipartitionNC([[1,2,-3,-5, -6], [3,-2,-4], [4,7], [5, -7, -8, -9], 
> [6], [8,9,-1]]);
<bipartition: [ 1, 2, -3, -5, -6 ], [ 3, -2, -4 ], [ 4, 7 ], [ 5, -7, -8, -9 
 ], [ 6 ], [ 8, 9, -1 ]>
gap> LeftProjection(f);
<bipartition: [ 1, 2, -1, -2 ], [ 3, -3 ], [ 4, 7 ], [ 5, -5 ], [ 6 ], 
[ 8, 9, -8, -9 ], [ -4, -7 ], [ -6 ]>

# ASSOCIATIVITY
gap> l := List([1..3*N], i->RandomBipartition(17));;
gap> triples := List([1..N], i -> [l[i],l[i+1],l[i+2]]);;
gap> ForAll(triples, x-> ((x[1]*x[2])*x[3]) = (x[1]*(x[2]*x[3])));
true

# EMBEDDING into T_n
gap> l := List([1,2,3,4,5,15,35,1999,64999,65000],i->RandomTransformation(i));;
gap> ForAll(l,t->t=AsTransformation(AsBipartition(t)));
true

#checking IsTransBipartitition
gap> l := List([1,2,3,4,5,15,35,1999,30101,54321],i->RandomTransformation(i));;
gap> ForAll(l,t->IsTransBipartition(AsBipartition(t)));
true

# check big size, identity, multiplication
gap> bp := RandomBipartition(70000);;bp*One(bp)=bp;One(bp)*bp=bp;
true
true

#check BlocksIdempotentTester, first a few little examples
gap> l := BlocksByIntRepNC( [ 3, 1, 2, 3, 3, 0, 0, 0 ]);;
gap> r := BlocksByIntRepNC( [ 2, 1, 2, 2, 2, 0, 0 ]) ;;
gap> BlocksIdempotentTester(l,r);
true
gap> e := BlocksIdempotentCreator(l,r);
<bipartition: [ 1 ], [ 2, 3, 4 ], [ -1 ], [ -2 ], [ -3, -4 ]>
gap> IsIdempotent(e);
true

# JDM is this the right behaviour?
gap> RightBlocks(e) = l;
true
gap> LeftBlocks(e) = r;
true

# AsBipartition for a bipartition
gap> f:=Bipartition( [ [ 1, 2, 3 ], [ 4, -1, -3 ], [ 5, 6, -4, -5 ], 
> [ -2 ], [ -6 ] ] );;
gap> AsBipartition(f, 8);
<bipartition: [ 1, 2, 3 ], [ 4, -1, -3 ], [ 5, 6, -4, -5 ], [ 7 ], [ 8 ], 
[ -2 ], [ -6 ], [ -7 ], [ -8 ]>
gap> AsBipartition(f, 6);
<bipartition: [ 1, 2, 3 ], [ 4, -1, -3 ], [ 5, 6, -4, -5 ], [ -2 ], [ -6 ]>
gap> AsBipartition(f, 4);
<bipartition: [ 1, 2, 3 ], [ 4, -1, -3 ], [ -2 ], [ -4 ]>

# AsPartialPerm for a bipartition
gap> S:=DualSymmetricInverseMonoid(4);;
gap> Number(S, IsPartialPermBipartition);
24
gap> S:=PartitionMonoid(4);;
gap> Number(S, IsPartialPermBipartition);
209
gap> Size(SymmetricInverseMonoid(4));
209
gap> S:=SymmetricInverseMonoid(4);;
gap> ForAll(S, x-> AsPartialPerm(AsBipartition(x))=x);
true
gap> elts:=Filtered(PartitionMonoid(4), IsPartialPermBipartition);;
gap> ForAll(elts, x-> AsBipartition(AsPartialPerm(x),4)=x);
true

#
gap> SemigroupsStopTest();

#
gap> STOP_TEST( "Semigroups package: bipartition.tst", 0);
