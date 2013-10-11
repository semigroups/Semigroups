#############################################################################
##
#W  smallerdegree.tst
##  Test file for St Andrews Pure Maths Summer School 2012
##
#############################################################################
##
gap> START_TEST("Semigroups package: smallerdegree.tst");
gap> LoadPackage("semigroups", false);;

# 
gap> SemigroupsStartTest();

# General Vagner Preston Example
gap> f1:=PartialPermNC([1,2,4,3]);;
gap> f2:=PartialPermNC([1]);;
gap> f3:=PartialPermNC([0,2]);;
gap> f:=InverseSemigroup(f1,f2,f3);
<inverse partial perm semigroup on 4 pts with 3 generators>
gap> NrMovedPoints(f);
2
gap> Size(f);
5
gap> VPR:=VagnerPrestonRepresentation(f);
MappingByFunction( <inverse partial perm semigroup of size 5, on 4 pts
 with 3 generators>, <inverse partial perm semigroup on 5 pts
 with 3 generators>, function( x ) ... end, function( x ) ... end )
gap> inv:=InverseGeneralMapping(VPR);
MappingByFunction( <inverse partial perm semigroup on 5 pts
 with 3 generators>, <inverse partial perm semigroup of size 5, on 4 pts
 with 3 generators>, function( x ) ... end, function( x ) ... end )
gap> ForAll(f,x->(x^VPR)^inv=x);
true

# Standard VagnerPreston example
gap> I5:=SymmetricInverseSemigroup(5);;
gap> NrMovedPoints(I5);
5
gap> Size(I5);
1546
gap> I5:=Range(VagnerPrestonRepresentation(I5));;
gap> NrMovedPoints(I5);
1545
gap> Size(I5);
1546
gap> I5:=SmallerDegreePartialPermRepresentation(I5);;
gap> NrMovedPoints(Image(I5));
5
gap> Size(Image(I5));
1546

# Example of higher returned degree spotted by Rhiannon
gap> f1:=PartialPermNC([2,1,4,5,3]);;
gap> f2:=PartialPermNC([2,1]);;
gap> f:=InverseSemigroup(f1,f2);;
gap> F:=SmallerDegreePartialPermRepresentation(f);;
gap> f=Image(F);
true
gap> NrMovedPoints(f);
5
gap> Size(f);
8

# Example of higher returned degree spotted by Robert
gap> f1:=PartialPermNC([2,1,0,0,4]);;
gap> f2:=PartialPermNC([1,2,3,5]);;
gap> f:=InverseSemigroup(f1,f2);;
gap> F:=SmallerDegreePartialPermRepresentation(f);;
gap> NrMovedPoints(f);
4
gap> NrMovedPoints(Image(F));
4
gap> Size(f);
15
gap> Size(Image(F));
15

# Example where Rhiannon's function returns a better result (confirmed)
gap> f1:=PartialPermNC([2,1,4,5,3,7,6,9,10,8]);;
gap> f2:=PartialPermNC([2,1,0,0,0,7,6]);;
gap> f:=InverseSemigroup(f1,f2);;
gap> F:=SmallerDegreePartialPermRepresentation(f);;
gap> NrMovedPoints(f);
10
gap> NrMovedPoints(Image(F));
5
gap> Size(f);
8
gap> Size(Image(F));
8

# Example of reducing degree but not moved points
gap> f1:=PartialPermNC([ 1, 2, 3, 4, 5, 6, 10, 11, 15, 16, 17, 18 ], [ 7, 5, 11, 8, 4, 2, 20, 14, 12, 17, 9, 3 ]);;
gap> f2:=PartialPermNC([ 1, 2, 3, 6, 8, 10, 12, 15, 16, 17, 18, 19 ], [ 2, 4, 14, 3, 17, 7, 9, 16, 15, 10, 11, 1 ]);;
gap> f:=InverseSemigroup(f1,f2);;
gap> F:=SmallerDegreePartialPermRepresentation(f);;
gap> NrMovedPoints(f);
19
gap> NrMovedPoints(Image(F));
19
gap> ActionDegree(f);
20
gap> ActionDegree(Image(F));
19

# Example made complicated by right regular representation of Sym(5). Genuine minimum degree of V is 7.
gap> S:=SymmetricGroup(5);
Sym( [ 1 .. 5 ] )
gap> rho:=ActionHomomorphism(S,S);
<action homomorphism>
gap> T:=Image(rho);
<permutation group with 2 generators>
gap> 
gap> H1:=[];
[  ]
gap> H2:=[];
[  ]
gap> for x in Elements(T) do
>   L:=[];
>   for y in [1..120] do
>     Add(L,y^x);
>   od;
>   g:=PartialPerm(L);
>   Add(H2,g);
>   Add(L,121);
>   Add(L,122);
>   f:=PartialPerm(L);
>   Add(H1,f);
> 
> od;
gap> 
gap> J:=[1..120];
[ 1 .. 120 ]
gap> Add(J,122);
gap> Add(J,121);
gap> h:=PartialPerm(J);
<partial perm on 122 pts with degree 122, codegree 122>
gap> 
gap> V:=InverseSemigroup(H1,H2,h);
<inverse partial perm semigroup on 122 pts with 241 generators>
gap> SmallerDegreePartialPermRepresentation(V);
MappingByFunction( <inverse partial perm semigroup on 122 pts
 with 241 generators>, <inverse partial perm semigroup on 12 pts
 with 241 generators>, function( x ) ... end, function( x ) ... end )

#
gap> f1:=PartialPermNC([ 1, 3, 4, 5, 7 ], [ 1, 5, 3, 8, 4 ]);;
gap> f2:=PartialPermNC([ 1, 2, 3, 4, 5, 6 ] ,[ 6, 7, 1, 4, 3, 2 ]);;
gap> f3:=PartialPermNC([ 1, 2, 3, 4, 5, 8 ] ,[ 5, 6, 3, 8, 4, 7 ]);;
gap> f4:=PartialPermNC([ 1, 3, 4, 5, 6, 8 ] ,[ 8, 7, 5, 1, 3, 4 ]);;
gap> f5:=PartialPermNC([ 1, 3, 4, 5, 7, 8 ] ,[ 6, 5, 7, 1, 4, 2 ]);;
gap> s:=InverseSemigroup(f1,f2,f3,f4,f5);;
gap> t:=Elements(s)[51624];
[7,1][8,6](4,5)
gap> Minorants(s,t);
[ <empty partial perm>, [4,5], [5,4], (4,5), [7,1], [5,4][7,1], [4,5][7,1], 
  [7,1](4,5), [8,6], [7,1][8,6], [5,4][8,6], [5,4][7,1][8,6], [4,5][8,6], 
  [4,5][7,1][8,6], [8,6](4,5) ]

#
gap> SemigroupsStopTest();

#
gap> STOP_TEST("Semigroups package: smallerdegree.tst", 10000);

