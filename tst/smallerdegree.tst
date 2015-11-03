#%T##########################################################################
##
#W  smallerdegree.tst
#Y  Copyright (C) 2012-15                                  Wilfred Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: smallerdegree.tst");
gap> LoadPackage("semigroups", false);;

# 
gap> SemigroupsStartTest();

#T# SmallerDegreeTest1: VagnerPresetonRepresentation: a basic example
gap> f1 := PartialPerm([ 1, 2, 4, 3 ]);;
gap> f2 := PartialPerm([ 1 ]);;
gap> f3 := PartialPerm([ 0, 2 ]);;
gap> f := InverseSemigroup(f1, f2, f3);
<inverse partial perm semigroup of rank 4 with 3 generators>
gap> NrMovedPoints(f);
2
gap> Size(f);
5
gap> VPR := VagnerPrestonRepresentation(f);
MappingByFunction( <inverse partial perm semigroup of size 5, rank 4 with 3 
 generators>, <inverse partial perm semigroup of rank 5 with 3 generators>
 , function( x ) ... end, function( x ) ... end )
gap> inv := InverseGeneralMapping(VPR);
MappingByFunction( <inverse partial perm semigroup of rank 5 with 3 
 generators>, <inverse partial perm semigroup of size 5, rank 4 with 3 
 generators>, function( x ) ... end, function( x ) ... end )
gap> ForAll(f, x -> (x ^ VPR) ^ inv = x);
true

#T# SmallerDegreeTest2: VagnerPrestonRepresentation
# for SymmetricInverseSemigroup(5)
gap> I5 := SymmetricInverseSemigroup(5);;
gap> NrMovedPoints(I5);
5
gap> Size(I5);
1546
gap> I5 := Range(VagnerPrestonRepresentation(I5));;
gap> NrMovedPoints(I5);
1545
gap> Size(I5);
1546
gap> I5 := SmallerDegreePartialPermRepresentation(I5);;
gap> NrMovedPoints(Image(I5));
5
gap> Size(Image(I5));
1546

#T# SmallerDegreeTest3: VagnerPrestonRepresentation
# for a bipartition semigroup
gap> B := Semigroup([
>  Bipartition( [ [ 1, -4 ], [ 2, -2 ], [ 3 ], [ 4 ], [ 5, -5 ], [ 6 ],
>    [ 7 ], [ -1 ], [ -3 ], [ -6 ], [ -7 ] ] ), 
>  Bipartition( [ [ 1, -5 ], [ 2, -6 ], [ 3, -7 ], [ 4, -3 ], [ 5 ],
>    [ 6, -2 ], [ 7 ], [ -1 ], [ -4 ] ] ), 
>  Bipartition( [ [ 1, -4 ], [ 2, -7 ], [ 3 ], [ 4, -5 ], [ 5, -2 ],
>    [ 6 ], [ 7, -1 ], [ -3 ], [ -6 ] ] ), 
>  Bipartition( [ [ 1 ], [ 2, -2 ], [ 3 ], [ 4, -1 ], [ 5, -5 ], [ 6 ],
>    [ 7 ], [ -3 ], [ -4 ], [ -6 ], [ -7 ] ] ), 
>  Bipartition( [ [ 1 ], [ 2, -6 ], [ 3, -4 ], [ 4 ], [ 5, -1 ],
>    [ 6, -2 ], [ 7, -3 ], [ -5 ], [ -7 ] ] ), 
>  Bipartition( [ [ 1, -7 ], [ 2, -5 ], [ 3 ], [ 4, -1 ], [ 5, -4 ],
>    [ 6 ], [ 7, -2 ], [ -3 ], [ -6 ] ] ) ]);;
gap> IsInverseSemigroup(B);
true
gap> V := Range(VagnerPrestonRepresentation(B));
<inverse partial perm semigroup of rank 664 with 6 generators>

#T# SmallerDegreeTest4: SmallerDegreePartialPermRepresentation Issue 1:
# Example where the degree being returned was greater than the original degree
gap> f1 := PartialPerm([ 2, 1, 4, 5, 3 ]);;
gap> f2 := PartialPerm([ 2, 1 ]);;
gap> f := InverseSemigroup(f1, f2);;
gap> F := SmallerDegreePartialPermRepresentation(f);;
gap> f = Image(F); # SmallerDegreePartialPermRepresentation returns the original
true
gap> NrMovedPoints(f);
5
gap> Size(f);
8

#T# SmallerDegreeTest5: SmallerDegreePartialPermRepresentation Issue 2:
# Example where the degree being returned was greater than the original degree
gap> f1 := PartialPerm([ 2, 1, 0, 0, 4 ]);;
gap> f2 := PartialPerm([ 1, 2, 3, 5 ]);;
gap> f := InverseSemigroup(f1, f2);;
gap> F := SmallerDegreePartialPermRepresentation(f);;
gap> NrMovedPoints(f);
4
gap> NrMovedPoints(Image(F));
4
gap> Size(f);
15
gap> Size(Image(F));
15

#T# SmallerDegreeTest6: SmallerDegreePartialPermRepresentation:
# Example where using SupermumIdempotents helps to give a better result 
gap> f1 := PartialPermNC([ 2, 1, 4, 5, 3, 7, 6, 9, 10, 8 ]);;
gap> f2 := PartialPermNC([ 2, 1, 0, 0, 0, 7, 6 ]);;
gap> f := InverseSemigroup(f1, f2);;
gap> F := SmallerDegreePartialPermRepresentation(f);;
gap> NrMovedPoints(f);
10
gap> NrMovedPoints(Image(F));
5
gap> Size(f);
8
gap> Size(Image(F));
8

#T# SmallerDegreeTest7: SmallerDegreePartialPermRepresentation:
# Example where the degree is reduced but not the number of moved points
gap> f1 := PartialPermNC([ 1, 2, 3, 4, 5, 6, 10, 11, 15, 16, 17, 18 ],
> [ 7, 5, 11, 8, 4, 2, 20, 14, 12, 17, 9, 3 ]);;
gap> f2 := PartialPermNC([ 1, 2, 3, 6, 8, 10, 12, 15, 16, 17, 18, 19 ],
> [ 2, 4, 14, 3, 17, 7, 9, 16, 15, 10, 11, 1 ]);;
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

#T# SmallerDegreeTest8: SmallerDegreePartialPermRepresentation:
# Example made complicated by right regular representation of Sym(5).
gap> S := SymmetricGroup(5);
Sym( [ 1 .. 5 ] )
gap> rho := ActionHomomorphism(S, S);
<action homomorphism>
gap> T := Image(rho);
<permutation group with 2 generators>
gap> 
gap> H1 := [  ];
[  ]
gap> H2 := [  ];
[  ]
gap> for x in Elements(T) do
>   L := [  ];
>   for y in [ 1 .. 120 ] do
>     Add(L, y ^ x);
>   od;
>   g := PartialPerm(L);
>   Add(H2, g);
>   Add(L, 121);
>   Add(L, 122);
>   f := PartialPerm(L);
>   Add(H1, f);
> 
> od;
gap> 
gap> J := [ 1 .. 120 ];
[ 1 .. 120 ]
gap> Add(J, 122);
gap> Add(J, 121);
gap> h := PartialPerm(J);
<partial perm on 122 pts with degree 122, codegree 122>
gap> 
gap> V := InverseSemigroup(H1, H2, h);
<inverse partial perm monoid of rank 122 with 240 generators>
gap> iso := SmallerDegreePartialPermRepresentation(V);;
gap> ActionDegree(Range(iso)) <= 12; # Genuine minimum degree of V is 7.
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(f1);
gap> Unbind(f2);
gap> Unbind(f3);
gap> Unbind(inv);
gap> Unbind(VPR);
gap> Unbind(I5);
gap> Unbind(B);
gap> Unbind(F);
gap> Unbind(J);
gap> Unbind(L);
gap> Unbind(S);
gap> Unbind(T);
gap> Unbind(rho);
gap> Unbind(V);
gap> Unbind(g);
gap> Unbind(f);
gap> Unbind(H2);
gap> Unbind(h);
gap> Unbind(H1);
gap> Unbind(iso);
gap> Unbind(y);
gap> Unbind(x);

#E#
gap> STOP_TEST("Semigroups package: smallerdegree.tst");
