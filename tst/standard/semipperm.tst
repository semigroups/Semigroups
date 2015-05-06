#############################################################################
##
#W  semipperm.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: semipperm.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#T# SemiPPermTest1: NumberSubset
gap> sets := Combinations([1..10]);;
gap> Sort(sets, 
> function(x, y)
>    if Length(x) <> Length(y) then 
>      return Length(x) < Length(y);
>    fi;
>    return x < y;
>  end);
gap> List(sets, x -> NumberSubset(x, 10)) = [ 1 .. 2 ^ 10 ];
true

#T# SemiPPermTest2: Enumerator for a symmetric inverse monoid
gap> S := SymmetricInverseMonoid(3);;
gap> enum := Enumerator(S);
<enumerator of symmetric inverse monoid on 3 pts>
gap> ForAll([1..Length(enum)], x -> Position(enum, enum[x]) = x);
true
gap> ForAll(enum, x -> enum[Position(enum, x)] = x);              
true
gap> Length(enum) = Size(S);
true
gap> ForAll(enum, x -> x in S);
true
gap> ForAll(S, x -> x in enum);
true

#T# SemiPPerm3Test: NumberSubsetOfEqualSize
gap> ForAll([1..10], m -> List(Combinations([1 .. 10], m), x ->
> NumberSubsetOfEqualSize(x, 10)) = [1 .. Binomial(10, m)]);
true

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
<inverse partial perm monoid on 122 pts with 240 generators>
gap> iso := SmallerDegreePartialPermRepresentation(V);;
gap> ActionDegree(Range(iso)) <= 12; # Genuine minimum degree of V is 7.
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(enum);
gap> Unbind(sets);
gap> Unbind(f1);
gap> Unbind(f2);
gap> Unbind(f3);
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
gap> STOP_TEST( "Semigroups package: semipperm.tst");
