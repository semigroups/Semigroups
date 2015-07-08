#%T##########################################################################
##
#W  attributes-inverse.tst
#Y  Copyright (C) 2015                                   Wilfred A. Wilson
##
##  Licensing information can be found in the README file of this package.
#
#############################################################################
##
gap> START_TEST("Semigroups package: attributes-inverse.tst");
gap> LoadPackage("semigroups", false);;

# 
gap> SemigroupsStartTest();

#T# attributes-inverse: VagnerPrestonRepresentation, symmetric inv monoid 4 1/1
gap> S := InverseSemigroup([
>   PartialPerm([2, 3, 4, 1]),
>   PartialPerm([2, 1, 3, 4]),
>   PartialPerm([1, 2, 3, 0])
> ]);;
gap> Size(S);
209
gap> Size(S) = Size(SymmetricInverseMonoid(4));
true
gap> iso := VagnerPrestonRepresentation(S);;
gap> DegreeOfPartialPermSemigroup(Range(iso));
209

#T# attributes-inverse: SameMinorantsSubgroup, symmetric inv monoid 5 1/2
gap> S := SymmetricInverseSemigroup(5);;
gap> h := HClass(S, One(S));
<Green's H-class: <identity partial perm on [ 1, 2, 3, 4, 5 ]>>
gap> SameMinorantsSubgroup(h);
[ <identity partial perm on [ 1, 2, 3, 4, 5 ]> ]
gap> h := HClass(S, PartialPerm([1, 2, 0, 0, 0]));
<Green's H-class: <identity partial perm on [ 1, 2 ]>>
gap> SameMinorantsSubgroup(h);
[ <identity partial perm on [ 1, 2 ]> ]
gap> h := HClass(S, MultiplicativeZero(S));
<Green's H-class: <empty partial perm>>
gap> SameMinorantsSubgroup(h);
[ <empty partial perm> ]

#T# attributes-inverse: SameMinorantsSubgroup, error 2/2
gap> S := FullTransformationMonoid(5);;
gap> h := HClass(S, One(S));
<Green's H-class: IdentityTransformation>
gap> SameMinorantsSubgroup(h);
Error, Semigroups: SameMinorantsSubgroup: usage,
the parent semigroup of the group H-class <h> must be inverse,

#T# attributes-inverse: Minorants, error
gap> S := SymmetricInverseMonoid(3);;
gap> f := PartialPerm([1, 2, 3, 4]);;
gap> Minorants(S, f);
Error, Semigroups: Minorants: usage,
the second argument is not an element of the first,
gap> f := PartialPerm([1, 2, 3]);;
gap> Set(Minorants(S, f));
[ <empty partial perm>, <identity partial perm on [ 1 ]>, 
  <identity partial perm on [ 2 ]>, <identity partial perm on [ 1, 2 ]>, 
  <identity partial perm on [ 3 ]>, <identity partial perm on [ 2, 3 ]>, 
  <identity partial perm on [ 1, 3 ]> ]
gap> NaturalPartialOrder(S);;
gap> Minorants(S, f);
[ <empty partial perm>, <identity partial perm on [ 1 ]>, 
  <identity partial perm on [ 2 ]>, <identity partial perm on [ 1, 2 ]>, 
  <identity partial perm on [ 3 ]>, <identity partial perm on [ 2, 3 ]>, 
  <identity partial perm on [ 1, 3 ]> ]
gap> f := PartialPerm([1, 3, 2]);;

#T# attributes-inverse: character tables of inverse acting semigroups
# Some random examples to test consistency of old code with new
gap> gens:=[
> [ PartialPerm( [ 1, 2, 3, 4, 6, 8, 9 ], [ 1, 5, 3, 8, 9, 4, 10 ] ) ],
> [ PartialPerm( [ 1, 2, 3, 4, 5, 6 ], [ 3, 8, 4, 6, 5, 7 ] ), 
>   PartialPerm( [ 1, 2, 3, 4, 5, 7 ], [ 1, 4, 3, 2, 7, 6 ] ), 
>   PartialPerm( [ 1, 2, 3, 5, 6, 8 ], [ 5, 7, 1, 4, 2, 6 ] ) ],
> [ PartialPerm( [ 1, 2, 3, 5 ], [ 2, 1, 7, 3 ] ), 
>   PartialPerm( [ 1, 2, 4, 5, 6 ], [ 7, 3, 1, 4, 2 ] ), 
>   PartialPerm( [ 1, 2, 3, 4, 6 ], [ 7, 6, 5, 1, 2 ] ), 
>   PartialPerm( [ 1, 3, 6, 7 ], [ 6, 3, 1, 4 ] ) ],
> [ PartialPerm( [ 1, 2, 3, 5 ], [ 1, 6, 4, 7 ] ), 
>   PartialPerm( [ 1, 2, 3, 6 ], [ 1, 6, 5, 2 ] ), 
>   PartialPerm( [ 1, 2, 3, 5, 6, 7 ], [ 4, 3, 5, 7, 1, 6 ] ), 
>   PartialPerm( [ 1, 2, 3, 4, 7 ], [ 6, 4, 2, 3, 1 ] ) ],
> [ PartialPerm( [ 1, 2, 3, 5, 6 ], [ 5, 3, 7, 4, 1 ] ), 
>   PartialPerm( [ 1, 2, 3, 4, 5, 7 ], [ 3, 1, 5, 7, 6, 2 ] ) ],
> [ PartialPerm( [ 1, 2, 3, 4, 5, 6, 9 ], [ 1, 5, 9, 2, 6, 10, 7 ] ), 
>   PartialPerm( [ 1, 3, 4, 7, 8, 9 ], [ 9, 4, 1, 6, 2, 8 ] ), 
>   PartialPerm( [ 1, 2, 3, 4, 5, 9 ], [ 9, 3, 8, 2, 10, 7 ] ) ],
> [ PartialPerm( [ 1, 2, 3, 4, 5 ], [ 6, 4, 1, 2, 7 ] ), 
>   PartialPerm( [ 1, 2, 3, 6 ], [ 3, 5, 7, 4 ] ), 
>   PartialPerm( [ 1, 2, 3, 4, 5, 6, 7 ], [ 1, 7, 9, 5, 2, 8, 4 ] ) ],
> [ PartialPerm( [ 1, 2, 4 ], [ 3, 6, 2 ] ), 
>   PartialPerm( [ 1, 2, 3, 4 ], [ 6, 3, 2, 1 ] ), 
>   PartialPerm( [ 1, 2, 3, 6 ], [ 4, 6, 3, 1 ] ), 
>   PartialPerm( [ 1, 2, 3, 5, 6 ], [ 5, 6, 3, 2, 4 ] ) ],
> [ PartialPerm( [ 1, 2, 3, 4 ], [ 3, 5, 1, 2 ] ), 
>   PartialPerm( [ 1, 2, 3, 4 ], [ 5, 4, 2, 1 ] ), 
>   PartialPerm( [ 1, 2, 4, 5 ], [ 3, 5, 1, 2 ] ) ],
> [ PartialPerm( [ 1, 2, 3, 5 ], [ 4, 1, 2, 3 ] ) ]];;
gap> S:=List(gens, x -> InverseSemigroup(x, rec(generic := false)));
[ <inverse partial perm semigroup of rank 9 with 1 generator>, 
<inverse partial perm semigroup of rank 8 with 3 generators>, 
<inverse partial perm semigroup of rank 7 with 4 generators>, 
<inverse partial perm semigroup of rank 7 with 4 generators>, 
<inverse partial perm semigroup of rank 7 with 2 generators>, 
<inverse partial perm semigroup of rank 10 with 3 generators>, 
<inverse partial perm semigroup of rank 9 with 3 generators>, 
<inverse partial perm semigroup of rank 6 with 4 generators>, 
<inverse partial perm semigroup of rank 5 with 3 generators>, 
<inverse partial perm semigroup of rank 5 with 1 generator> ]
gap> CharacterTableOfInverseSemigroup(S[1]);
[ [ [ 1, 0, 0, 0 ], [ 2, 1, 0, 0 ], [ 1, 1, 1, -1 ], [ 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 3, 4, 5, 8, 9, 10 ]>, 
      <identity partial perm on [ 1, 3, 4, 8, 10 ]>, 
      <identity partial perm on [ 1, 3, 4, 8 ]>, (1)(3)(4,8) ] ]
gap> CharacterTableOfInverseSemigroup(S[2]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 4, 4, 2, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 3, 3, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 3, 3, 2, 2, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 3, 3, 2, 2, 1, 0, 0, 1, E(3)^2, E(3), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 
         ], [ 4, 3, 3, 2, 2, 1, 0, 0, 1, E(3), E(3)^2, 0, 0, 0, 0, 0, 0, 0, 
          0, 0, 0 ], 
      [ 1, 1, 3, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 19, 19, 20, 10, 10, 9, 4, 4, 4, 1, 1, 4, 3, -1, 1, -1, 1, 0, 0, 0, 0 ]
        , [ 38, 38, 40, 20, 20, 18, 8, 8, 8, -1, -1, 8, 6, 0, 2, 0, -1, 0, 0, 
          0, 0 ], 
      [ 19, 19, 20, 10, 10, 9, 4, 4, 4, 1, 1, 4, 3, 1, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 15, 15, 15, 10, 10, 10, 6, 6, 6, 0, 0, 6, 6, 0, 3, -1, 0, 1, -1, 0, 0 
         ], [ 15, 15, 15, 10, 10, 10, 6, 6, 6, 0, 0, 6, 6, 2, 3, 1, 0, 1, 1, 
          0, 0 ], 
      [ 6, 6, 6, 5, 5, 5, 4, 4, 4, 1, 1, 4, 4, 2, 3, 1, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 3, 4, 5, 6, 7, 8 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 5, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 5, 6, 8 ]>, 
      <identity partial perm on [ 3, 5, 6, 7, 8 ]>, 
      <identity partial perm on [ 3, 4, 6, 7, 8 ]>, 
      <identity partial perm on [ 1, 2, 5, 7 ]>, 
      <identity partial perm on [ 4, 5, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 6, 7 ]>, (2)(3,6,7), (2)(3,7,6), 
      <identity partial perm on [ 2, 4, 5, 7 ]>, 
      <identity partial perm on [ 1, 2, 3, 4 ]>, (1)(2,4)(3), 
      <identity partial perm on [ 5, 6, 7 ]>, (5)(6,7), (5,6,7), 
      <identity partial perm on [ 2, 4 ]>, (2,4), 
      <identity partial perm on [ 7 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[3]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 6, 6, 4, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 1, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 ], 
      [ 2, 2, 0, 0, 1, 2, 0, 0, 0, 0, 1, 0, 0, 0, 0 ], 
      [ 10, 10, 6, 6, 6, 6, 3, 3, 3, -1, 3, 1, -1, 0, 0 ], 
      [ 10, 10, 6, 6, 6, 6, 3, 3, 3, 1, 3, 1, 1, 0, 0 ], 
      [ 5, 5, 4, 4, 4, 4, 3, 3, 3, 1, 3, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 4, 7 ]>, 
      <identity partial perm on [ 1, 2, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 3, 7 ]>, 
      <identity partial perm on [ 1, 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 5, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 4, 7 ]>, 
      <identity partial perm on [ 1, 2, 7 ]>, 
      <identity partial perm on [ 3, 6, 7 ]>, 
      <identity partial perm on [ 1, 3, 6 ]>, (1,6)(3), 
      <identity partial perm on [ 2, 4, 7 ]>, 
      <identity partial perm on [ 3, 7 ]>, (3,7), 
      <identity partial perm on [ 6 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[4]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 1, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 2, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 20, 9, 10, 4, 4, 0, 4, 3, 4, 1, -1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 20, 9, 10, 4, 4, 0, 4, 3, 4, 1, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, E(3)^2, E(3), 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, E(3), E(3)^2, 0, 0, 0, 0 ], 
      [ 15, 10, 10, 6, 6, -2, 6, 6, 6, 3, -1, 3, 0, 0, 1, -1, 0, 0 ], 
      [ 15, 10, 10, 6, 6, 2, 6, 6, 6, 3, 1, 3, 0, 0, 1, 1, 0, 0 ], 
      [ 6, 5, 5, 4, 4, 0, 4, 4, 4, 3, 1, 3, 0, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 3, 4, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 4, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 4, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 5, 6 ]>, (1,5)(2,6), 
      <identity partial perm on [ 1, 2, 4, 7 ]>, 
      <identity partial perm on [ 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 4, 6 ]>, 
      <identity partial perm on [ 1, 4, 6 ]>, (1,6)(4), 
      <identity partial perm on [ 2, 3, 4 ]>, (2,3,4), (2,4,3), 
      <identity partial perm on [ 1, 2 ]>, (1,2), 
      <identity partial perm on [ 1 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[5]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 7, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 5, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 19, 10, 4, 4, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 19, 10, 4, 4, 1, E(3)^2, E(3), 0, 0, 0, 0 ], 
      [ 19, 10, 4, 4, 1, E(3), E(3)^2, 0, 0, 0, 0 ], 
      [ 15, 10, 6, 6, 3, 0, 0, 1, -1, 0, 0 ], 
      [ 15, 10, 6, 6, 3, 0, 0, 1, 1, 0, 0 ], 
      [ 6, 5, 4, 4, 3, 0, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 3, 4, 5, 7 ]>, 
      <identity partial perm on [ 1, 2, 3, 4 ]>, 
      <identity partial perm on [ 1, 3, 5, 6 ]>, 
      <identity partial perm on [ 4, 5, 7 ]>, (4,5,7), (4,7,5), 
      <identity partial perm on [ 3, 4 ]>, (3,4), 
      <identity partial perm on [ 4 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[6]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 0, 2, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 2, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 4, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 4, 6, 4, 1, 2, 1, 1, 1, 3, 2, 1, 0, 1, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 8, 12, 8, 2, 4, 2, 2, 2, 6, 4, 2, 0, 2, 0, -1, 0, 0, 0, 0, 0, 0, 
          0, 0 ], 
      [ 4, 6, 4, 1, 2, 1, 1, 1, 3, 2, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 5, 2, 3, 2, 2, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ],
      [ 3, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 ],
      [ 3, 1, 2, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 ],
      [ 2, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ],
      [ 21, 15, 15, 10, 10, 6, 6, 6, 6, 6, 6, 3, 3, -1, 0, 3, 3, 3, 3, 1, -1, 
          0, 0 ], 
      [ 21, 15, 15, 10, 10, 6, 6, 6, 6, 6, 6, 3, 3, 1, 0, 3, 3, 3, 3, 1, 1, 
          0, 0 ], 
      [ 7, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 3, 3, 1, 0, 3, 3, 3, 3, 2, 0, 1, 0 ],
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] 
     ], 
  [ <identity partial perm on [ 1, 2, 5, 6, 7, 9, 10 ]>, 
      <identity partial perm on [ 1, 2, 4, 6, 8, 9 ]>, 
      <identity partial perm on [ 2, 3, 7, 8, 9, 10 ]>, 
      <identity partial perm on [ 1, 5, 6, 7, 10 ]>, 
      <identity partial perm on [ 1, 2, 5, 7, 10 ]>, 
      <identity partial perm on [ 3, 7, 9, 10 ]>, 
      <identity partial perm on [ 1, 4, 7, 8 ]>, 
      <identity partial perm on [ 1, 4, 5, 9 ]>, 
      <identity partial perm on [ 1, 2, 8, 9 ]>, 
      <identity partial perm on [ 2, 3, 7, 9 ]>, 
      <identity partial perm on [ 2, 4, 6, 8 ]>, 
      <identity partial perm on [ 6, 8, 9 ]>, 
      <identity partial perm on [ 1, 3, 4 ]>, (1)(3,4), (1,3,4), 
      <identity partial perm on [ 5, 7, 9 ]>, 
      <identity partial perm on [ 1, 5, 10 ]>, 
      <identity partial perm on [ 3, 6, 9 ]>, 
      <identity partial perm on [ 4, 5, 9 ]>, 
      <identity partial perm on [ 6, 9 ]>, (6,9), 
      <identity partial perm on [ 7 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[7]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, -1, -E(4), E(4), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, -1, E(4), -E(4), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 6, 2, 4, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 2, 2, -2, 0, 0, 0, 1, 1, -1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 2, 2, 2, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 10, 5, 4, 0, 0, 0, 1, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0 ], 
      [ 5, 1, 4, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0 ], 
      [ 3, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ], 
      [ 20, 10, 10, -2, 0, 0, 6, 6, 3, -1, 3, 3, 3, 1, -1, 0, 0 ], 
      [ 20, 10, 10, 2, 0, 0, 6, 6, 3, 1, 3, 3, 3, 1, 1, 0, 0 ], 
      [ 7, 5, 5, 1, 1, 1, 4, 4, 3, 1, 3, 3, 3, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 4, 5, 7, 8, 9 ]>, 
      <identity partial perm on [ 1, 2, 4, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 5, 7 ]>, (1)(2,4)(5,7), 
      (1)(2,5,4,7), (1)(2,7,4,5), <identity partial perm on [ 3, 4, 5, 7 ]>, 
      <identity partial perm on [ 1, 2, 5, 7 ]>, 
      <identity partial perm on [ 2, 4, 6 ]>, (2,4)(6), 
      <identity partial perm on [ 3, 4, 5 ]>, 
      <identity partial perm on [ 2, 4, 7 ]>, 
      <identity partial perm on [ 3, 5, 7 ]>, 
      <identity partial perm on [ 3, 6 ]>, (3,6), 
      <identity partial perm on [ 7 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[8]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], [ 7, 4, 3, 1, -1, 1, 0, 0, 0, 0 ], 
      [ 14, 8, 6, 2, 0, -1, 0, 0, 0, 0 ], [ 7, 4, 3, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 10, 6, 6, 3, -1, 0, 1, -1, 0, 0 ], [ 10, 6, 6, 3, 1, 0, 1, 1, 0, 0 ], 
      [ 5, 4, 4, 3, 1, 0, 2, 0, 1, 0 ], [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 2, 3, 4, 5, 6 ]>, 
      <identity partial perm on [ 1, 2, 3, 6 ]>, 
      <identity partial perm on [ 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 2, 3, 6 ]>, (2)(3,6), (2,3,6), 
      <identity partial perm on [ 2, 3 ]>, (2,3), 
      <identity partial perm on [ 6 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[9]);
[ [ [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 1, -1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 0, 1, -1, 0, 0, 0, 0, 0, 0 ], [ 4, 0, 1, 1, 0, 0, 0, 0, 0, 0 ], 
      [ 4, -2, 2, 0, 1, -1, 0, 0, 0, 0 ], [ 4, 2, 2, 0, 1, 1, 0, 0, 0, 0 ], 
      [ 2, 0, 1, -1, 0, 0, 1, -1, 0, 0 ], [ 2, 0, 1, 1, 0, 0, 1, 1, 0, 0 ], 
      [ 4, 0, 3, 1, 2, 0, 2, 0, 1, 0 ], [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 5 ]>, (1,5)(2,3), 
      <identity partial perm on [ 1, 3, 5 ]>, (1)(3,5), 
      <identity partial perm on [ 1, 3 ]>, (1,3), 
      <identity partial perm on [ 3, 5 ]>, (3,5), 
      <identity partial perm on [ 3 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[10]);
[ [ [ 1, 0, 0, 0, 0 ], [ 2, 1, 0, 0, 0 ], [ 3, 2, 1, 0, 0 ], 
      [ 4, 3, 2, 1, 0 ], [ 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 4 ]>, 
      <identity partial perm on [ 1, 2, 4 ]>, 
      <identity partial perm on [ 1, 4 ]>, <identity partial perm on [ 4 ]>, 
      <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[1]);
[ [ [ 1, 0, 0, 0 ], [ 2, 1, 0, 0 ], [ 1, 1, 1, -1 ], [ 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 3, 4, 5, 8, 9, 10 ]>, 
      <identity partial perm on [ 1, 3, 4, 8, 10 ]>, 
      <identity partial perm on [ 1, 3, 4, 8 ]>, (1)(3)(4,8) ] ]
gap> CharacterTableOfInverseSemigroup(S[2]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 4, 4, 2, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 3, 3, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 3, 3, 2, 2, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 3, 3, 2, 2, 1, 0, 0, 1, E(3)^2, E(3), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 
         ], [ 4, 3, 3, 2, 2, 1, 0, 0, 1, E(3), E(3)^2, 0, 0, 0, 0, 0, 0, 0, 
          0, 0, 0 ], 
      [ 1, 1, 3, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 19, 19, 20, 10, 10, 9, 4, 4, 4, 1, 1, 4, 3, -1, 1, -1, 1, 0, 0, 0, 0 ]
        , [ 38, 38, 40, 20, 20, 18, 8, 8, 8, -1, -1, 8, 6, 0, 2, 0, -1, 0, 0, 
          0, 0 ], 
      [ 19, 19, 20, 10, 10, 9, 4, 4, 4, 1, 1, 4, 3, 1, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 15, 15, 15, 10, 10, 10, 6, 6, 6, 0, 0, 6, 6, 0, 3, -1, 0, 1, -1, 0, 0 
         ], [ 15, 15, 15, 10, 10, 10, 6, 6, 6, 0, 0, 6, 6, 2, 3, 1, 0, 1, 1, 
          0, 0 ], 
      [ 6, 6, 6, 5, 5, 5, 4, 4, 4, 1, 1, 4, 4, 2, 3, 1, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 3, 4, 5, 6, 7, 8 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 5, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 5, 6, 8 ]>, 
      <identity partial perm on [ 3, 5, 6, 7, 8 ]>, 
      <identity partial perm on [ 3, 4, 6, 7, 8 ]>, 
      <identity partial perm on [ 1, 2, 5, 7 ]>, 
      <identity partial perm on [ 4, 5, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 6, 7 ]>, (2)(3,6,7), (2)(3,7,6), 
      <identity partial perm on [ 2, 4, 5, 7 ]>, 
      <identity partial perm on [ 1, 2, 3, 4 ]>, (1)(2,4)(3), 
      <identity partial perm on [ 5, 6, 7 ]>, (5)(6,7), (5,6,7), 
      <identity partial perm on [ 2, 4 ]>, (2,4), 
      <identity partial perm on [ 7 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[3]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 6, 6, 4, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 1, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 ], 
      [ 2, 2, 0, 0, 1, 2, 0, 0, 0, 0, 1, 0, 0, 0, 0 ], 
      [ 10, 10, 6, 6, 6, 6, 3, 3, 3, -1, 3, 1, -1, 0, 0 ], 
      [ 10, 10, 6, 6, 6, 6, 3, 3, 3, 1, 3, 1, 1, 0, 0 ], 
      [ 5, 5, 4, 4, 4, 4, 3, 3, 3, 1, 3, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 4, 7 ]>, 
      <identity partial perm on [ 1, 2, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 3, 7 ]>, 
      <identity partial perm on [ 1, 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 5, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 4, 7 ]>, 
      <identity partial perm on [ 1, 2, 7 ]>, 
      <identity partial perm on [ 3, 6, 7 ]>, 
      <identity partial perm on [ 1, 3, 6 ]>, (1,6)(3), 
      <identity partial perm on [ 2, 4, 7 ]>, 
      <identity partial perm on [ 3, 7 ]>, (3,7), 
      <identity partial perm on [ 6 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[4]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 1, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 2, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 20, 9, 10, 4, 4, 0, 4, 3, 4, 1, -1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 20, 9, 10, 4, 4, 0, 4, 3, 4, 1, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, E(3)^2, E(3), 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, E(3), E(3)^2, 0, 0, 0, 0 ], 
      [ 15, 10, 10, 6, 6, -2, 6, 6, 6, 3, -1, 3, 0, 0, 1, -1, 0, 0 ], 
      [ 15, 10, 10, 6, 6, 2, 6, 6, 6, 3, 1, 3, 0, 0, 1, 1, 0, 0 ], 
      [ 6, 5, 5, 4, 4, 0, 4, 4, 4, 3, 1, 3, 0, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 3, 4, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 4, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 4, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 5, 6 ]>, (1,5)(2,6), 
      <identity partial perm on [ 1, 2, 4, 7 ]>, 
      <identity partial perm on [ 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 4, 6 ]>, 
      <identity partial perm on [ 1, 4, 6 ]>, (1,6)(4), 
      <identity partial perm on [ 2, 3, 4 ]>, (2,3,4), (2,4,3), 
      <identity partial perm on [ 1, 2 ]>, (1,2), 
      <identity partial perm on [ 1 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[5]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 7, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 5, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 19, 10, 4, 4, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 19, 10, 4, 4, 1, E(3)^2, E(3), 0, 0, 0, 0 ], 
      [ 19, 10, 4, 4, 1, E(3), E(3)^2, 0, 0, 0, 0 ], 
      [ 15, 10, 6, 6, 3, 0, 0, 1, -1, 0, 0 ], 
      [ 15, 10, 6, 6, 3, 0, 0, 1, 1, 0, 0 ], 
      [ 6, 5, 4, 4, 3, 0, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 3, 4, 5, 7 ]>, 
      <identity partial perm on [ 1, 2, 3, 4 ]>, 
      <identity partial perm on [ 1, 3, 5, 6 ]>, 
      <identity partial perm on [ 4, 5, 7 ]>, (4,5,7), (4,7,5), 
      <identity partial perm on [ 3, 4 ]>, (3,4), 
      <identity partial perm on [ 4 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[6]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 0, 2, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 2, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 4, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 4, 6, 4, 1, 2, 1, 1, 1, 3, 2, 1, 0, 1, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 8, 12, 8, 2, 4, 2, 2, 2, 6, 4, 2, 0, 2, 0, -1, 0, 0, 0, 0, 0, 0, 
          0, 0 ], 
      [ 4, 6, 4, 1, 2, 1, 1, 1, 3, 2, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 5, 2, 3, 2, 2, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ],
      [ 3, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 ],
      [ 3, 1, 2, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 ],
      [ 2, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ],
      [ 21, 15, 15, 10, 10, 6, 6, 6, 6, 6, 6, 3, 3, -1, 0, 3, 3, 3, 3, 1, -1, 
          0, 0 ], 
      [ 21, 15, 15, 10, 10, 6, 6, 6, 6, 6, 6, 3, 3, 1, 0, 3, 3, 3, 3, 1, 1, 
          0, 0 ], 
      [ 7, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 3, 3, 1, 0, 3, 3, 3, 3, 2, 0, 1, 0 ],
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] 
     ], 
  [ <identity partial perm on [ 1, 2, 5, 6, 7, 9, 10 ]>, 
      <identity partial perm on [ 1, 2, 4, 6, 8, 9 ]>, 
      <identity partial perm on [ 2, 3, 7, 8, 9, 10 ]>, 
      <identity partial perm on [ 1, 5, 6, 7, 10 ]>, 
      <identity partial perm on [ 1, 2, 5, 7, 10 ]>, 
      <identity partial perm on [ 3, 7, 9, 10 ]>, 
      <identity partial perm on [ 1, 4, 7, 8 ]>, 
      <identity partial perm on [ 1, 4, 5, 9 ]>, 
      <identity partial perm on [ 1, 2, 8, 9 ]>, 
      <identity partial perm on [ 2, 3, 7, 9 ]>, 
      <identity partial perm on [ 2, 4, 6, 8 ]>, 
      <identity partial perm on [ 6, 8, 9 ]>, 
      <identity partial perm on [ 1, 3, 4 ]>, (1)(3,4), (1,3,4), 
      <identity partial perm on [ 5, 7, 9 ]>, 
      <identity partial perm on [ 1, 5, 10 ]>, 
      <identity partial perm on [ 3, 6, 9 ]>, 
      <identity partial perm on [ 4, 5, 9 ]>, 
      <identity partial perm on [ 6, 9 ]>, (6,9), 
      <identity partial perm on [ 7 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[7]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, -1, -E(4), E(4), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, -1, E(4), -E(4), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 6, 2, 4, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 2, 2, -2, 0, 0, 0, 1, 1, -1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 2, 2, 2, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 10, 5, 4, 0, 0, 0, 1, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0 ], 
      [ 5, 1, 4, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0 ], 
      [ 3, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ], 
      [ 20, 10, 10, -2, 0, 0, 6, 6, 3, -1, 3, 3, 3, 1, -1, 0, 0 ], 
      [ 20, 10, 10, 2, 0, 0, 6, 6, 3, 1, 3, 3, 3, 1, 1, 0, 0 ], 
      [ 7, 5, 5, 1, 1, 1, 4, 4, 3, 1, 3, 3, 3, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 4, 5, 7, 8, 9 ]>, 
      <identity partial perm on [ 1, 2, 4, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 5, 7 ]>, (1)(2,4)(5,7), 
      (1)(2,5,4,7), (1)(2,7,4,5), <identity partial perm on [ 3, 4, 5, 7 ]>, 
      <identity partial perm on [ 1, 2, 5, 7 ]>, 
      <identity partial perm on [ 2, 4, 6 ]>, (2,4)(6), 
      <identity partial perm on [ 3, 4, 5 ]>, 
      <identity partial perm on [ 2, 4, 7 ]>, 
      <identity partial perm on [ 3, 5, 7 ]>, 
      <identity partial perm on [ 3, 6 ]>, (3,6), 
      <identity partial perm on [ 7 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[8]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], [ 7, 4, 3, 1, -1, 1, 0, 0, 0, 0 ], 
      [ 14, 8, 6, 2, 0, -1, 0, 0, 0, 0 ], [ 7, 4, 3, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 10, 6, 6, 3, -1, 0, 1, -1, 0, 0 ], [ 10, 6, 6, 3, 1, 0, 1, 1, 0, 0 ], 
      [ 5, 4, 4, 3, 1, 0, 2, 0, 1, 0 ], [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 2, 3, 4, 5, 6 ]>, 
      <identity partial perm on [ 1, 2, 3, 6 ]>, 
      <identity partial perm on [ 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 2, 3, 6 ]>, (2)(3,6), (2,3,6), 
      <identity partial perm on [ 2, 3 ]>, (2,3), 
      <identity partial perm on [ 6 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[9]);
[ [ [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 1, -1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 0, 1, -1, 0, 0, 0, 0, 0, 0 ], [ 4, 0, 1, 1, 0, 0, 0, 0, 0, 0 ], 
      [ 4, -2, 2, 0, 1, -1, 0, 0, 0, 0 ], [ 4, 2, 2, 0, 1, 1, 0, 0, 0, 0 ], 
      [ 2, 0, 1, -1, 0, 0, 1, -1, 0, 0 ], [ 2, 0, 1, 1, 0, 0, 1, 1, 0, 0 ], 
      [ 4, 0, 3, 1, 2, 0, 2, 0, 1, 0 ], [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 5 ]>, (1,5)(2,3), 
      <identity partial perm on [ 1, 3, 5 ]>, (1)(3,5), 
      <identity partial perm on [ 1, 3 ]>, (1,3), 
      <identity partial perm on [ 3, 5 ]>, (3,5), 
      <identity partial perm on [ 3 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[10]);
[ [ [ 1, 0, 0, 0, 0 ], [ 2, 1, 0, 0, 0 ], [ 3, 2, 1, 0, 0 ], 
      [ 4, 3, 2, 1, 0 ], [ 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 4 ]>, 
      <identity partial perm on [ 1, 2, 4 ]>, 
      <identity partial perm on [ 1, 4 ]>, <identity partial perm on [ 4 ]>, 
      <empty partial perm> ] ]

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(iso);
gap> Unbind(h);
gap> Unbind(f);

#E#
gap> STOP_TEST("Semigroups package: attributes-inverse.tst");
