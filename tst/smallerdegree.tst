#############################################################################
#
# Test file for St Andrews Pure Maths Summer School 2012
#
#############################################################################

gap> START_TEST("Smaller: smallerdegree.tst");
gap> LoadPackage("semigroups", false);;
gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoSemigroups:=InfoLevel(InfoSemigroups);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoSemigroups, 0);

# Standard VagnerPreston example
gap> I5:=SymmetricInverseSemigroup(5);;
gap> NrMovedPoints(I5);
5
gap> Size(I5);
1546
gap> I5:=VagnerPrestonRepresentation(I5);;
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

# Example where Rhiannon's function returns a better result
gap> f1:=PartialPermNC([2,1,4,5,3,7,6,9,10,8]);;
gap> f2:=PartialPermNC([2,1,0,0,0,7,6]);;
gap> f:=InverseSemigroup(f1,f2);;
gap> F:=SmallerDegreePartialPermRepresentation(f);;
gap> NrMovedPoints(f);
10
gap> NrMovedPoints(Image(F));
7
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
19
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
<partial perm on 122 pts>
gap> 
gap> V:=InverseSemigroup(H1,H2,h);
<inverse partial perm semigroup of degree 122 with 241 generators>
gap> SmallerDegreePartialPermRepresentation(V);
MappingByFunction( <inverse partial perm semigroup of degree 122 with 
241 generators>, <inverse partial perm semigroup of degree 22 with 
241 generators>, function( x ) ... end, function( x ) ... end )
gap> time;
6627


#
gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoSemigroups, InfoLevelInfoSemigroups);;
gap> Unbind(InfoLevelInfoSemigroups);; Unbind(InfoLevelInfoWarning);;
gap> STOP_TEST("Semigroups package: smallerdegree.tst", 10000);
