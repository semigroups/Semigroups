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

# AsPartialPerm for bipartitions
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

# AsPermutation for bipartitions
gap> G:=SymmetricGroup(5);;
gap> ForAll(G, x-> AsPermutation(AsBipartition(x))=x);
true
gap> G:=GroupOfUnits(PartitionMonoid(5));                   
<bipartition group on 5 pts with 2 generators>
gap> ForAll(G, x-> AsBipartition(AsPermutation(x), 5)=x);
true

#IsomorphismBipartitionSemigroup for a generic semigroup
gap> S:=Semigroup( 
> Bipartition( [ [ 1, 2, 3, -3 ], [ 4, -4, -5 ], [ 5, -1 ], [ -2 ] ] ), 
> Bipartition( [ [ 1, 4, -2, -3 ], [ 2, 3, 5, -5 ], [ -1, -4 ] ] ), 
> Bipartition( [ [ 1, 5 ], [ 2, 4, -3, -5 ], [ 3, -1, -2 ], [ -4 ] ] ), 
> Bipartition( [ [ 1 ], [ 2 ], [ 3, 5, -1, -2 ], [ 4, -3 ], [ -4, -5 ] ] ), 
> Bipartition( [ [ 1 ], [ 2 ], [ 3 ], [ 4, -1, -4 ], [ 5 ], [ -2, -3 ], 
>      [ -5 ] ] ));;
gap> D:=DClass(S, Bipartition( [ [ 1 ], [ 2 ], [ 3 ], [ 4, -1, -4 ], 
> [ 5 ], [ -2, -3 ], [ -5 ] ] ));;
gap> IsRegularDClass(D);
true
gap> R:=PrincipalFactor(D);
<Rees 0-matrix semigroup 12x15 over Group(())>
gap> f:=IsomorphismBipartitionSemigroup(R);
MappingByFunction( <Rees 0-matrix semigroup 12x15 over Group(())>, 
<bipartition semigroup on 182 pts with 181 generators>
 , function( x ) ... end, function( x ) ... end )
gap> g:=InverseGeneralMapping(f);;
gap> ForAll(R, x-> (x^f)^g=x);
true
gap> x:=RMSElement(R, 12,(),8);;
gap> ForAll(R, y-> (x^f)*(y^f)=(x*y)^f);               
true

#IsomorphismBipartitionSemigroup for a transformation semigroup
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ), 
>   Transformation( [ 4, 2, 1, 5, 5 ] ), 
>   Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> S:=Range(IsomorphismBipartitionSemigroup(s));
<bipartition semigroup on 5 pts with 3 generators>
gap> f:=IsomorphismBipartitionSemigroup(s);
MappingByFunction( <transformation semigroup on 5 pts with 3 generators>, 
<bipartition semigroup on 5 pts with 3 generators>
 , function( x ) ... end, <Attribute "AsTransformation"> )
gap> g:=InverseGeneralMapping(f);;
gap> ForAll(s, x-> (x^f)^g=x);                   
true
gap> ForAll(S, x-> (x^g)^f=x);
true
gap> Size(s);
731
gap> Size(S);
731
gap> x:=Random(s);
Transformation( [ 3, 1, 3, 3, 3 ] )
gap> ForAll(s, y-> (x^f)*(y^f)=(x*y)^f);
true

#IsomorphismTransformationSemigroup for a bipartition semigroup consisting of 
#IsTransBipartition
gap> S:=Semigroup( Transformation( [ 1, 3, 4, 1, 3 ] ), 
> Transformation( [ 2, 4, 1, 5, 5 ] ), 
> Transformation( [ 2, 5, 3, 5, 3 ] ), 
> Transformation( [ 4, 1, 2, 2, 1 ] ), 
> Transformation( [ 5, 5, 1, 1, 3 ] ) );;
gap> T:=Range(IsomorphismBipartitionSemigroup(S));
<bipartition semigroup on 5 pts with 5 generators>
gap> f:=IsomorphismTransformationSemigroup(T);
MappingByFunction( <bipartition semigroup on 5 pts with 5 generators>, 
<transformation semigroup on 5 pts with 5 generators>
 , <Attribute "AsTransformation">, function( x ) ... end )
gap> g:=InverseGeneralMapping(f);;      
gap> ForAll(T, x-> (x^f)^g=x);
true
gap> ForAll(S, x-> (x^g)^f=x);
true
gap> Size(T);
602
gap> Size(S);
602
gap> Size(Range(f));
602

#IsomorphismBipartitionSemigroup for a partial perm semigroup
gap> S:=Semigroup(
> [ PartialPerm( [ 1, 2, 3 ], [ 1, 3, 4 ] ), 
>  PartialPerm( [ 1, 2, 3 ], [ 2, 5, 3 ] ), 
>  PartialPerm( [ 1, 2, 3 ], [ 4, 1, 2 ] ), 
>  PartialPerm( [ 1, 2, 3, 4 ], [ 2, 4, 1, 5 ] ), 
>  PartialPerm( [ 1, 3, 5 ], [ 5, 1, 3 ] ) ]);;
gap> T:=Range(IsomorphismBipartitionSemigroup(S));
<bipartition semigroup on 5 pts with 5 generators>
gap> Generators(S);
[ [2,3,4](1), [1,2,5](3), [3,2,1,4], [3,1,2,4,5], (1,5,3) ]
gap> Generators(T);
[ <bipartition: [ 1, -1 ], [ 2, -3 ], [ 3, -4 ], [ 4 ], [ 5 ], [ -2 ], [ -5 ]>
    , <bipartition: [ 1, -2 ], [ 2, -5 ], [ 3, -3 ], [ 4 ], [ 5 ], [ -1 ], 
    [ -4 ]>, <bipartition: [ 1, -4 ], [ 2, -1 ], [ 3, -2 ], [ 4 ], [ 5 ], 
    [ -3 ], [ -5 ]>, <bipartition: [ 1, -2 ], [ 2, -4 ], [ 3, -1 ], 
    [ 4, -5 ], [ 5 ], [ -3 ]>, <bipartition: [ 1, -5 ], [ 2 ], [ 3, -1 ], 
    [ 4 ], [ 5, -3 ], [ -2 ], [ -4 ]> ]
gap> Size(S);
156
gap> Size(T);
156
gap> IsInverseSemigroup(S);
false
gap> IsInverseSemigroup(T);
false
gap> f:=IsomorphismBipartitionSemigroup(S);;
gap> g:=InverseGeneralMapping(f);;
gap> ForAll(S, x-> (x^f)^g=x);
true
gap> ForAll(T, x-> (x^g)^f=x);
true
gap> Size(S);
156
gap> ForAll(S, x-> ForAll(S, y-> (x*y)^f=(x^f)*(y^f)));
true

#IsomorphismPartialPermSemigroup for a semigroup of bipartitions consisting of
#IsPartialPermBipartition
gap> f:=IsomorphismPartialPermSemigroup(T);;
gap> g:=InverseGeneralMapping(f);;
gap> ForAll(T, x-> ForAll(T, y-> (x*y)^f=(x^f)*(y^f)));
true
gap> Size(S); Size(T);
156
156
gap> ForAll(T, x-> (x^f)^g=x);                         
true
gap> ForAll(S, x-> (x^g)^f=x);
true

# testing the cases to which the new methods for IsomorphismPartialPermSemigroup
# and IsomorphismTransformationSemigroup don't apply...
gap> S:=Semigroup(
> Bipartition( [ [ 1, 2, 3, 4, -1, -2, -5 ], [ 5 ], [ -3, -4 ] ] ), 
> Bipartition( [ [ 1, 2, 3 ], [ 4, -2, -4 ], [ 5, -1, -5 ], [ -3 ] ] ), 
> Bipartition( [ [ 1, 3, 5 ], [ 2, 4, -1, -2, -5 ], [ -3 ], [ -4 ] ] ), 
> Bipartition( [ [ 1, -5 ], [ 2, 3, 4, 5 ], [ -1 ], [ -2 ], [ -3, -4 ] ] ), 
> Bipartition( [ [ 1, -4 ], [ 2 ], [ 3, -2 ], [ 4, 5, -1 ], [ -3, -5 ] ] ) );;
gap> IsomorphismPartialPermSemigroup(S);
fail
gap> Range(IsomorphismTransformationSemigroup(S));
<transformation semigroup on 208 pts with 5 generators>

# IsomorphismBipartitionSemigroup for a perm group
gap> G:=DihedralGroup(IsPermGroup, 10);;
gap> f:=IsomorphismBipartitionSemigroup(G);;
gap> g:=InverseGeneralMapping(f);;
gap> ForAll(G, x-> (x^f)^g=x);
true
gap> ForAll(G, x-> ForAll(G, y-> (x*y)^f=x^f*y^f));
true
gap> ForAll(Range(f), x-> (x^g)^f=x);                     
true

#IsomorphismPermGroup
gap> G:=GroupOfUnits(PartitionMonoid(5));
<bipartition group on 5 pts with 2 generators>
gap> IsomorphismPermGroup(G);
MappingByFunction( <bipartition group on 5 pts with 2 generators>, Group([ (1,
2,3,4,5), (1,2) ]), <Attribute "AsPermutation">, function( x ) ... end )
gap> f:=last;; g:=InverseGeneralMapping(f);;           
gap> ForAll(G, x-> ForAll(G, y-> (x*y)^f=x^f*y^f));
true
gap> ForAll(G, x-> (x^f)^g=x);                     
true
gap> ForAll(Range(f), x-> (x^g)^f=x);
true
gap> D:=DClass(PartitionMonoid(5), 
> Bipartition( [ [ 1 ], [ 2, -3 ], [ 3, -4 ], [ 4, -5 ], [ 5 ], [ -1 ], [ -2 ] ]));;
gap> G:=GroupHClass(D);
{Bipartition( [ [ 1 ], [ 2, -2 ], [ 3, -3 ], [ 4, -4, -5 ], [ 5 ], [ -1 ] ] )}
gap> IsomorphismPermGroup(G);
MappingByFunction( {Bipartition( 
[ [ 1 ], [ 2, -2 ], [ 3, -3 ], [ 4, -4, -5 ], [ 5 ], [ -1 ] ] )}, Group([ (2,
4,3), (3,4) ]), function( x ) ... end, function( x ) ... end )

# IsomorphismBipartitionSemigroup for an inverse semigroup of partial perms
gap> S:=InverseSemigroup(
> PartialPerm( [ 1, 3, 5, 7, 9 ], [ 7, 6, 5, 10, 1 ] ), 
> PartialPerm( [ 1, 2, 3, 4, 6, 10 ], [ 9, 10, 4, 2, 5, 6 ] ) );;
gap> T:=Range(IsomorphismBipartitionSemigroup(S));
<inverse bipartition semigroup on 10 pts with 2 generators>
gap> Size(S);
281
gap> Size(T);
281
gap> IsomorphismPartialPermSemigroup(T);
MappingByFunction( <inverse bipartition semigroup of size 281, 
 on 10 pts with 2 generators>, <inverse partial perm semigroup on 9 pts
 with 4 generators>, <Operation "AsPartialPerm">, function( x ) ... end )
gap> Size(Range(last));
281
gap> f:=last2;; g:=InverseGeneralMapping(f);;
gap> ForAll(T, x-> (x^f)^g=x);
true

# AsBlockBijection and IsomorphismBlockBijectionSemigroup for an inverse
# semigroup of partial perms
gap> S:=InverseSemigroup(
> PartialPerm( [ 1, 2, 3, 6, 8, 10 ], [ 2, 6, 7, 9, 1, 5 ] ), 
> PartialPerm( [ 1, 2, 3, 4, 6, 7, 8, 10 ], [ 3, 8, 1, 9, 4, 10, 5, 6 ] ) );;
gap> AsBlockBijection(S.1);
<block bijection: [ 1, -2 ], [ 2, -6 ], [ 3, -7 ], 
[ 4, 5, 7, 9, 11, -3, -4, -8, -10, -11 ], [ 6, -9 ], [ 8, -1 ], [ 10, -5 ]>
gap> S.1;
[3,7][8,1,2,6,9][10,5]
gap> T:=Range(IsomorphismBlockBijectionSemigroup(S));
<inverse bipartition semigroup on 11 pts with 2 generators>
gap> f:=IsomorphismBlockBijectionSemigroup(S);;
gap> g:=InverseGeneralMapping(f);;
gap> ForAll(S, x-> (x^f)^g=x);
true
gap> ForAll(T, x-> (x^g)^f=x);
true
gap> Size(S);
2657
gap> Size(T);
2657
gap> x:=PartialPerm( [ 1, 2, 3, 8 ], [ 8, 4, 10, 3 ] );;
gap> ForAll(S, y-> x^f*y^f=(x*y)^f);
true

# Same as last for non-inverse partial perm semigroup
gap> S:=Semigroup(
> PartialPerm( [ 1, 2, 3, 6, 8, 10 ], [ 2, 6, 7, 9, 1, 5 ] ), 
> PartialPerm( [ 1, 2, 3, 4, 6, 7, 8, 10 ], [ 3, 8, 1, 9, 4, 10, 5, 6 ] ) );;
gap> Size(S);
90
gap> IsInverseSemigroup(S);
false
gap> T:=Range(IsomorphismBlockBijectionSemigroup(S));
<bipartition semigroup on 11 pts with 2 generators>
gap> Size(T);
90
gap> IsInverseSemigroup(T);
false
gap> f:=IsomorphismBlockBijectionSemigroup(S);;
gap> g:=InverseGeneralMapping(f);;
gap> ForAll(S, x-> (x^f)^g=x);
true
gap> ForAll(T, x-> (x^g)^f=x);
true
gap> x:=PartialPerm( [ 1, 3 ], [ 3, 1 ] );;
gap> ForAll(S, y-> x^f*y^f=(x*y)^f);
true

#
gap> SemigroupsStopTest();

#
gap> STOP_TEST( "Semigroups package: bipartition.tst", 0);
