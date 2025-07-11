#############################################################################
##
#W  standard/attributes/inverse.tst
#Y  Copyright (C) 2015-2022                                 Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
#
#############################################################################
##

#@local D, I, S, T, W, acting, an, elts, es, f, foo, gens, h, iso, jid, n, reps
#@local small, x
gap> START_TEST("Semigroups package: standard/attributes/inverse.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# attrinv: VagnerPrestonRepresentation, symmetric inv monoid 4 1/1
gap> S := InverseSemigroup([
> PartialPerm([2, 3, 4, 1]),
> PartialPerm([2, 1, 3, 4]),
> PartialPerm([1, 2, 3, 0])]);;
gap> Size(S);
209
gap> Size(S) = Size(SymmetricInverseMonoid(4));
true
gap> iso := VagnerPrestonRepresentation(S);;
gap> DegreeOfPartialPermSemigroup(Range(iso));
209

# attrinv: SameMinorantsSubgroup, symmetric inv monoid 5 1/2
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

# attrinv: SameMinorantsSubgroup, error 2/2
gap> S := FullTransformationMonoid(5);;
gap> h := HClass(S, One(S));
<Green's H-class: IdentityTransformation>
gap> SameMinorantsSubgroup(h);
Error, the parent of the argument (a group H-class) must be an inverse semigro\
up

# attrinv: Minorants, error, 1
gap> S := SymmetricInverseMonoid(3);;
gap> f := PartialPerm([1, 2, 3, 4]);;
gap> Minorants(S, f);
Error, the 2nd argument (a mult. elt.) is not an element of the 1st argument (\
an inverse semigroup)
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

# attrinv: Minorants, not idempotent, 2
gap> S := Semigroup([
> PartialPerm([1, 2, 3, 4], [1, 2, 3, 4]),
> PartialPerm([1, 2, 3], [2, 3, 1])]);;
gap> IsInverseSemigroup(S);
true
gap> Minorants(S, GeneratorsOfSemigroup(S)[2]);
[  ]
gap> S := Semigroup(S, rec(acting := false));;
gap> IsInverseSemigroup(S);
true
gap> Minorants(S, GeneratorsOfSemigroup(S)[1]);
[ <identity partial perm on [ 1, 2, 3 ]> ]

# attrinv: character tables of inverse acting semigroups
# Some random examples to test consistency of old code with new
gap> gens := [
> [PartialPerm([1, 2, 3, 4, 6, 8, 9], [1, 5, 3, 8, 9, 4, 10])],
> [PartialPerm([1, 2, 3, 4, 5, 6], [3, 8, 4, 6, 5, 7]),
>  PartialPerm([1, 2, 3, 4, 5, 7], [1, 4, 3, 2, 7, 6]),
>  PartialPerm([1, 2, 3, 5, 6, 8], [5, 7, 1, 4, 2, 6])],
> [PartialPerm([1, 2, 3, 5], [2, 1, 7, 3]),
>  PartialPerm([1, 2, 4, 5, 6], [7, 3, 1, 4, 2]),
>  PartialPerm([1, 2, 3, 4, 6], [7, 6, 5, 1, 2]),
>  PartialPerm([1, 3, 6, 7], [6, 3, 1, 4])],
> [PartialPerm([1, 2, 3, 5], [1, 6, 4, 7]),
>  PartialPerm([1, 2, 3, 6], [1, 6, 5, 2]),
>  PartialPerm([1, 2, 3, 5, 6, 7], [4, 3, 5, 7, 1, 6]),
>  PartialPerm([1, 2, 3, 4, 7], [6, 4, 2, 3, 1])],
> [PartialPerm([1, 2, 3, 5, 6], [5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 3, 4, 5, 7], [3, 1, 5, 7, 6, 2])],
> [PartialPerm([1, 2, 3, 4, 5, 6, 9], [1, 5, 9, 2, 6, 10, 7]),
>  PartialPerm([1, 3, 4, 7, 8, 9], [9, 4, 1, 6, 2, 8]),
>  PartialPerm([1, 2, 3, 4, 5, 9], [9, 3, 8, 2, 10, 7])],
> [PartialPerm([1, 2, 3, 4, 5], [6, 4, 1, 2, 7]),
>  PartialPerm([1, 2, 3, 6], [3, 5, 7, 4]),
>  PartialPerm([1, 2, 3, 4, 5, 6, 7], [1, 7, 9, 5, 2, 8, 4])],
> [PartialPerm([1, 2, 4], [3, 6, 2]),
>  PartialPerm([1, 2, 3, 4], [6, 3, 2, 1]),
>  PartialPerm([1, 2, 3, 6], [4, 6, 3, 1]),
>  PartialPerm([1, 2, 3, 5, 6], [5, 6, 3, 2, 4])],
> [PartialPerm([1, 2, 3, 4], [3, 5, 1, 2]),
>  PartialPerm([1, 2, 3, 4], [5, 4, 2, 1]),
>  PartialPerm([1, 2, 4, 5], [3, 5, 1, 2])],
> [PartialPerm([1, 2, 3, 5], [4, 1, 2, 3])]];;
gap> S := List(gens, x -> InverseSemigroup(x, rec(acting := true)));
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

#@if CompareVersionNumbers(ReplacedString(GAPInfo.Version, "dev", ""), "4.15")
gap> CharacterTableOfInverseSemigroup(S[1]);
[ [ [ 1, 0, 0, 0 ], [ 2, 1, 0, 0 ], [ 1, 1, 1, -1 ], [ 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 3, 4, 5, 8, 9, 10 ]>, 
      <identity partial perm on [ 1, 3, 4, 6, 8 ]>, 
      <identity partial perm on [ 1, 3, 4, 8 ]>, (1)(3)(4,8) ] ]
gap> CharacterTableOfInverseSemigroup(S[2]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 3, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 4, 4, 0, 1, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 3, 3, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 3, 3, 2, 1, 2, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 3, 3, 2, 1, 2, 0, 0, 0, 1, E(3), E(3)^2, 0, 0, 0, 0, 0, 0, 0, 0, 0 
         ], [ 4, 3, 3, 2, 1, 2, 0, 0, 0, 1, E(3)^2, E(3), 0, 0, 0, 0, 0, 0, 
          0, 0, 0 ], 
      [ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 19, 19, 20, 10, 9, 10, 4, 4, 4, 4, 1, 1, 3, -1, 1, -1, 1, 0, 0, 0, 0 ]
        , [ 38, 38, 40, 20, 18, 20, 8, 8, 8, 8, -1, -1, 6, 0, 2, 0, -1, 0, 0, 
          0, 0 ], 
      [ 19, 19, 20, 10, 9, 10, 4, 4, 4, 4, 1, 1, 3, 1, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 15, 15, 15, 10, 10, 10, 6, 6, 6, 6, 0, 0, 6, 0, 3, -1, 0, 1, -1, 0, 0 
         ], [ 15, 15, 15, 10, 10, 10, 6, 6, 6, 6, 0, 0, 6, 2, 3, 1, 0, 1, 1, 
          0, 0 ], 
      [ 6, 6, 6, 5, 5, 5, 4, 4, 4, 4, 1, 1, 4, 2, 3, 1, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 3, 4, 5, 6, 7, 8 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 3, 5, 6, 8 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 5, 7 ]>, 
      <identity partial perm on [ 1, 3, 6, 8 ]>, 
      <identity partial perm on [ 1, 3, 4, 7 ]>, 
      <identity partial perm on [ 2, 3, 5, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 6 ]>, (1)(2,4,6), (1)(2,6,4), 
      <identity partial perm on [ 1, 2, 3, 4 ]>, (1)(2,4)(3), 
      <identity partial perm on [ 2, 5, 7 ]>, (2)(5,7), (2,5,7), 
      <identity partial perm on [ 2, 5 ]>, (2,5), 
      <identity partial perm on [ 6 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[3]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 6, 6, 4, 2, 2, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], 
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
      <identity partial perm on [ 1, 3, 6, 7 ]>, 
      <identity partial perm on [ 1, 5, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 3, 6, 7 ]>, 
      <identity partial perm on [ 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 3, 6 ]>, (1,6)(3), 
      <identity partial perm on [ 1, 5, 6 ]>, 
      <identity partial perm on [ 3, 7 ]>, (3,7), 
      <identity partial perm on [ 1 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[4]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 20, 10, 9, 4, 4, 3, 4, 4, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 20, 10, 9, 4, 4, 3, 4, 4, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, E(3), E(3)^2, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, E(3)^2, E(3), 0, 0, 0, 0 ], 
      [ 15, 10, 10, 6, 6, 6, 6, 6, -2, 3, -1, 3, 0, 0, 1, -1, 0, 0 ], 
      [ 15, 10, 10, 6, 6, 6, 6, 6, 2, 3, 1, 3, 0, 0, 1, 1, 0, 0 ], 
      [ 6, 5, 5, 4, 4, 4, 4, 4, 0, 3, 1, 3, 0, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 3, 4, 5, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 4, 7 ]>, 
      <identity partial perm on [ 1, 4, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 4, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 5, 6 ]>, (1,5)(2,6), 
      <identity partial perm on [ 1, 3, 6 ]>, (1)(3,6), 
      <identity partial perm on [ 2, 3, 4 ]>, (2,3,4), (2,4,3), 
      <identity partial perm on [ 1, 2 ]>, (1,2), 
      <identity partial perm on [ 1 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[5]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 7, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 5, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 19, 10, 4, 4, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 19, 10, 4, 4, 1, E(3), E(3)^2, 0, 0, 0, 0 ], 
      [ 19, 10, 4, 4, 1, E(3)^2, E(3), 0, 0, 0, 0 ], 
      [ 15, 10, 6, 6, 3, 0, 0, 1, -1, 0, 0 ], 
      [ 15, 10, 6, 6, 3, 0, 0, 1, 1, 0, 0 ], 
      [ 6, 5, 4, 4, 3, 0, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 3, 4, 5, 7 ]>, 
      <identity partial perm on [ 1, 2, 3, 6 ]>, 
      <identity partial perm on [ 1, 3, 5, 6 ]>, 
      <identity partial perm on [ 1, 2, 6 ]>, (1,2,6), (1,6,2), 
      <identity partial perm on [ 1, 3 ]>, (1,3), 
      <identity partial perm on [ 4 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[6]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 2, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 3, 2, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 4, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 3, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 5, 3, 2, 2, 2, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ],
      [ 4, 4, 6, 1, 2, 1, 3, 1, 2, 1, 1, 0, 0, 0, 0, 0, 1, -1, 1, 0, 0, 0, 0 ]
        , [ 8, 8, 12, 2, 4, 2, 6, 2, 4, 2, 2, 0, 0, 0, 0, 0, 2, 0, -1, 0, 0, 
          0, 0 ], 
      [ 4, 4, 6, 1, 2, 1, 3, 1, 2, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 ],
      [ 21, 15, 15, 10, 10, 6, 6, 6, 6, 6, 6, 3, 3, 3, 3, 3, 3, -1, 0, 1, -1, 
          0, 0 ], 
      [ 21, 15, 15, 10, 10, 6, 6, 6, 6, 6, 6, 3, 3, 3, 3, 3, 3, 1, 0, 1, 1, 
          0, 0 ], 
      [ 7, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 1, 0, 2, 0, 1, 0 ],
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] 
     ], 
  [ <identity partial perm on [ 1, 2, 5, 6, 7, 9, 10 ]>, 
      <identity partial perm on [ 2, 3, 7, 8, 9, 10 ]>, 
      <identity partial perm on [ 1, 2, 4, 6, 8, 9 ]>, 
      <identity partial perm on [ 1, 5, 6, 7, 10 ]>, 
      <identity partial perm on [ 1, 3, 4, 7, 8 ]>, 
      <identity partial perm on [ 3, 7, 9, 10 ]>, 
      <identity partial perm on [ 1, 4, 8, 9 ]>, 
      <identity partial perm on [ 3, 4, 6, 9 ]>, 
      <identity partial perm on [ 1, 3, 4, 8 ]>, 
      <identity partial perm on [ 1, 4, 7, 8 ]>, 
      <identity partial perm on [ 2, 4, 6, 8 ]>, 
      <identity partial perm on [ 3, 6, 9 ]>, 
      <identity partial perm on [ 6, 8, 9 ]>, 
      <identity partial perm on [ 1, 3, 7 ]>, 
      <identity partial perm on [ 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 4 ]>, 
      <identity partial perm on [ 1, 3, 4 ]>, (1)(3,4), (1,3,4), 
      <identity partial perm on [ 1, 3 ]>, (1,3), 
      <identity partial perm on [ 1 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[7]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, -1, -E(4), E(4), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, -1, E(4), -E(4), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 6, 4, 0, 0, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 5, 4, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 3, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 2, -2, 0, 0, 2, 1, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0 ], 
      [ 4, 2, 2, 0, 0, 2, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 ], 
      [ 10, 4, 0, 0, 0, 5, 2, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0 ], 
      [ 20, 10, -2, 0, 0, 10, 6, 6, 3, 3, 3, -1, 3, 1, -1, 0, 0 ], 
      [ 20, 10, 2, 0, 0, 10, 6, 6, 3, 3, 3, 1, 3, 1, 1, 0, 0 ], 
      [ 7, 5, 1, 1, 1, 5, 4, 4, 3, 3, 3, 1, 3, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 4, 5, 7, 8, 9 ]>, 
      <identity partial perm on [ 1, 2, 4, 5, 7 ]>, (1)(2,4)(5,7), 
      (1)(2,5,4,7), (1)(2,7,4,5), <identity partial perm on [ 1, 2, 4, 6, 7 ]>
        , <identity partial perm on [ 1, 2, 4, 7 ]>, 
      <identity partial perm on [ 3, 4, 5, 7 ]>, 
      <identity partial perm on [ 2, 5, 7 ]>, 
      <identity partial perm on [ 3, 5, 7 ]>, 
      <identity partial perm on [ 2, 4, 6 ]>, (2,4)(6), 
      <identity partial perm on [ 3, 4, 5 ]>, 
      <identity partial perm on [ 2, 4 ]>, (2,4), 
      <identity partial perm on [ 7 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[8]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], [ 7, 4, 3, 1, -1, 1, 0, 0, 0, 0 ], 
      [ 14, 8, 6, 2, 0, -1, 0, 0, 0, 0 ], [ 7, 4, 3, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 10, 6, 6, 3, -1, 0, 1, -1, 0, 0 ], [ 10, 6, 6, 3, 1, 0, 1, 1, 0, 0 ], 
      [ 5, 4, 4, 3, 1, 0, 2, 0, 1, 0 ], [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 2, 3, 4, 5, 6 ]>, 
      <identity partial perm on [ 1, 2, 3, 6 ]>, 
      <identity partial perm on [ 1, 2, 3, 5 ]>, 
      <identity partial perm on [ 2, 3, 6 ]>, (2)(3,6), (2,3,6), 
      <identity partial perm on [ 2, 3 ]>, (2,3), 
      <identity partial perm on [ 6 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[9]);
[ [ [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 1, -1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 0, 1, -1, 0, 0, 0, 0, 0, 0 ], [ 4, 0, 1, 1, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, -1, 1, -1, 0, 0, 0, 0 ], [ 2, 0, 1, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 4, -2, 2, 0, 0, 0, 1, -1, 0, 0 ], [ 4, 2, 2, 0, 0, 0, 1, 1, 0, 0 ], 
      [ 4, 0, 3, 1, 2, 0, 2, 0, 1, 0 ], [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 5 ]>, (1,5)(2,3), 
      <identity partial perm on [ 1, 3, 5 ]>, (1)(3,5), 
      <identity partial perm on [ 3, 4 ]>, (3,4), 
      <identity partial perm on [ 1, 3 ]>, (1,3), 
      <identity partial perm on [ 3 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[10]);
[ [ [ 1, 0, 0, 0, 0 ], [ 2, 1, 0, 0, 0 ], [ 3, 2, 1, 0, 0 ], 
      [ 4, 3, 2, 1, 0 ], [ 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 4 ]>, 
      <identity partial perm on [ 2, 3, 5 ]>, 
      <identity partial perm on [ 1, 4 ]>, <identity partial perm on [ 4 ]>, 
      <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[1]);
[ [ [ 1, 0, 0, 0 ], [ 2, 1, 0, 0 ], [ 1, 1, 1, -1 ], [ 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 3, 4, 5, 8, 9, 10 ]>, 
      <identity partial perm on [ 1, 3, 4, 6, 8 ]>, 
      <identity partial perm on [ 1, 3, 4, 8 ]>, (1)(3)(4,8) ] ]
gap> CharacterTableOfInverseSemigroup(S[2]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 3, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 4, 4, 0, 1, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 3, 3, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 3, 3, 2, 1, 2, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 3, 3, 2, 1, 2, 0, 0, 0, 1, E(3), E(3)^2, 0, 0, 0, 0, 0, 0, 0, 0, 0 
         ], [ 4, 3, 3, 2, 1, 2, 0, 0, 0, 1, E(3)^2, E(3), 0, 0, 0, 0, 0, 0, 
          0, 0, 0 ], 
      [ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 19, 19, 20, 10, 9, 10, 4, 4, 4, 4, 1, 1, 3, -1, 1, -1, 1, 0, 0, 0, 0 ]
        , [ 38, 38, 40, 20, 18, 20, 8, 8, 8, 8, -1, -1, 6, 0, 2, 0, -1, 0, 0, 
          0, 0 ], 
      [ 19, 19, 20, 10, 9, 10, 4, 4, 4, 4, 1, 1, 3, 1, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 15, 15, 15, 10, 10, 10, 6, 6, 6, 6, 0, 0, 6, 0, 3, -1, 0, 1, -1, 0, 0 
         ], [ 15, 15, 15, 10, 10, 10, 6, 6, 6, 6, 0, 0, 6, 2, 3, 1, 0, 1, 1, 
          0, 0 ], 
      [ 6, 6, 6, 5, 5, 5, 4, 4, 4, 4, 1, 1, 4, 2, 3, 1, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 3, 4, 5, 6, 7, 8 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 3, 5, 6, 8 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 5, 7 ]>, 
      <identity partial perm on [ 1, 3, 6, 8 ]>, 
      <identity partial perm on [ 1, 3, 4, 7 ]>, 
      <identity partial perm on [ 2, 3, 5, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 6 ]>, (1)(2,4,6), (1)(2,6,4), 
      <identity partial perm on [ 1, 2, 3, 4 ]>, (1)(2,4)(3), 
      <identity partial perm on [ 2, 5, 7 ]>, (2)(5,7), (2,5,7), 
      <identity partial perm on [ 2, 5 ]>, (2,5), 
      <identity partial perm on [ 6 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[3]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 6, 6, 4, 2, 2, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], 
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
      <identity partial perm on [ 1, 3, 6, 7 ]>, 
      <identity partial perm on [ 1, 5, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 3, 6, 7 ]>, 
      <identity partial perm on [ 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 3, 6 ]>, (1,6)(3), 
      <identity partial perm on [ 1, 5, 6 ]>, 
      <identity partial perm on [ 3, 7 ]>, (3,7), 
      <identity partial perm on [ 1 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[4]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 20, 10, 9, 4, 4, 3, 4, 4, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 20, 10, 9, 4, 4, 3, 4, 4, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, E(3), E(3)^2, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, E(3)^2, E(3), 0, 0, 0, 0 ], 
      [ 15, 10, 10, 6, 6, 6, 6, 6, -2, 3, -1, 3, 0, 0, 1, -1, 0, 0 ], 
      [ 15, 10, 10, 6, 6, 6, 6, 6, 2, 3, 1, 3, 0, 0, 1, 1, 0, 0 ], 
      [ 6, 5, 5, 4, 4, 4, 4, 4, 0, 3, 1, 3, 0, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 3, 4, 5, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 4, 7 ]>, 
      <identity partial perm on [ 1, 4, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 4, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 5, 6 ]>, (1,5)(2,6), 
      <identity partial perm on [ 1, 3, 6 ]>, (1)(3,6), 
      <identity partial perm on [ 2, 3, 4 ]>, (2,3,4), (2,4,3), 
      <identity partial perm on [ 1, 2 ]>, (1,2), 
      <identity partial perm on [ 1 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[5]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 7, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 5, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 19, 10, 4, 4, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 19, 10, 4, 4, 1, E(3), E(3)^2, 0, 0, 0, 0 ], 
      [ 19, 10, 4, 4, 1, E(3)^2, E(3), 0, 0, 0, 0 ], 
      [ 15, 10, 6, 6, 3, 0, 0, 1, -1, 0, 0 ], 
      [ 15, 10, 6, 6, 3, 0, 0, 1, 1, 0, 0 ], 
      [ 6, 5, 4, 4, 3, 0, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 3, 4, 5, 7 ]>, 
      <identity partial perm on [ 1, 2, 3, 6 ]>, 
      <identity partial perm on [ 1, 3, 5, 6 ]>, 
      <identity partial perm on [ 1, 2, 6 ]>, (1,2,6), (1,6,2), 
      <identity partial perm on [ 1, 3 ]>, (1,3), 
      <identity partial perm on [ 4 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[6]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 2, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 3, 2, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 4, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 3, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 5, 3, 2, 2, 2, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ],
      [ 4, 4, 6, 1, 2, 1, 3, 1, 2, 1, 1, 0, 0, 0, 0, 0, 1, -1, 1, 0, 0, 0, 0 ]
        , [ 8, 8, 12, 2, 4, 2, 6, 2, 4, 2, 2, 0, 0, 0, 0, 0, 2, 0, -1, 0, 0, 
          0, 0 ], 
      [ 4, 4, 6, 1, 2, 1, 3, 1, 2, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 ],
      [ 21, 15, 15, 10, 10, 6, 6, 6, 6, 6, 6, 3, 3, 3, 3, 3, 3, -1, 0, 1, -1, 
          0, 0 ], 
      [ 21, 15, 15, 10, 10, 6, 6, 6, 6, 6, 6, 3, 3, 3, 3, 3, 3, 1, 0, 1, 1, 
          0, 0 ], 
      [ 7, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 1, 0, 2, 0, 1, 0 ],
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] 
     ], 
  [ <identity partial perm on [ 1, 2, 5, 6, 7, 9, 10 ]>, 
      <identity partial perm on [ 2, 3, 7, 8, 9, 10 ]>, 
      <identity partial perm on [ 1, 2, 4, 6, 8, 9 ]>, 
      <identity partial perm on [ 1, 5, 6, 7, 10 ]>, 
      <identity partial perm on [ 1, 3, 4, 7, 8 ]>, 
      <identity partial perm on [ 3, 7, 9, 10 ]>, 
      <identity partial perm on [ 1, 4, 8, 9 ]>, 
      <identity partial perm on [ 3, 4, 6, 9 ]>, 
      <identity partial perm on [ 1, 3, 4, 8 ]>, 
      <identity partial perm on [ 1, 4, 7, 8 ]>, 
      <identity partial perm on [ 2, 4, 6, 8 ]>, 
      <identity partial perm on [ 3, 6, 9 ]>, 
      <identity partial perm on [ 6, 8, 9 ]>, 
      <identity partial perm on [ 1, 3, 7 ]>, 
      <identity partial perm on [ 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 4 ]>, 
      <identity partial perm on [ 1, 3, 4 ]>, (1)(3,4), (1,3,4), 
      <identity partial perm on [ 1, 3 ]>, (1,3), 
      <identity partial perm on [ 1 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[7]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, -1, -E(4), E(4), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, -1, E(4), -E(4), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 6, 4, 0, 0, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 5, 4, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 3, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 2, -2, 0, 0, 2, 1, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0 ], 
      [ 4, 2, 2, 0, 0, 2, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 ], 
      [ 10, 4, 0, 0, 0, 5, 2, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0 ], 
      [ 20, 10, -2, 0, 0, 10, 6, 6, 3, 3, 3, -1, 3, 1, -1, 0, 0 ], 
      [ 20, 10, 2, 0, 0, 10, 6, 6, 3, 3, 3, 1, 3, 1, 1, 0, 0 ], 
      [ 7, 5, 1, 1, 1, 5, 4, 4, 3, 3, 3, 1, 3, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 4, 5, 7, 8, 9 ]>, 
      <identity partial perm on [ 1, 2, 4, 5, 7 ]>, (1)(2,4)(5,7), 
      (1)(2,5,4,7), (1)(2,7,4,5), <identity partial perm on [ 1, 2, 4, 6, 7 ]>
        , <identity partial perm on [ 1, 2, 4, 7 ]>, 
      <identity partial perm on [ 3, 4, 5, 7 ]>, 
      <identity partial perm on [ 2, 5, 7 ]>, 
      <identity partial perm on [ 3, 5, 7 ]>, 
      <identity partial perm on [ 2, 4, 6 ]>, (2,4)(6), 
      <identity partial perm on [ 3, 4, 5 ]>, 
      <identity partial perm on [ 2, 4 ]>, (2,4), 
      <identity partial perm on [ 7 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[8]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], [ 7, 4, 3, 1, -1, 1, 0, 0, 0, 0 ], 
      [ 14, 8, 6, 2, 0, -1, 0, 0, 0, 0 ], [ 7, 4, 3, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 10, 6, 6, 3, -1, 0, 1, -1, 0, 0 ], [ 10, 6, 6, 3, 1, 0, 1, 1, 0, 0 ], 
      [ 5, 4, 4, 3, 1, 0, 2, 0, 1, 0 ], [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 2, 3, 4, 5, 6 ]>, 
      <identity partial perm on [ 1, 2, 3, 6 ]>, 
      <identity partial perm on [ 1, 2, 3, 5 ]>, 
      <identity partial perm on [ 2, 3, 6 ]>, (2)(3,6), (2,3,6), 
      <identity partial perm on [ 2, 3 ]>, (2,3), 
      <identity partial perm on [ 6 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[9]);
[ [ [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 1, -1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 0, 1, -1, 0, 0, 0, 0, 0, 0 ], [ 4, 0, 1, 1, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, -1, 1, -1, 0, 0, 0, 0 ], [ 2, 0, 1, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 4, -2, 2, 0, 0, 0, 1, -1, 0, 0 ], [ 4, 2, 2, 0, 0, 0, 1, 1, 0, 0 ], 
      [ 4, 0, 3, 1, 2, 0, 2, 0, 1, 0 ], [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 5 ]>, (1,5)(2,3), 
      <identity partial perm on [ 1, 3, 5 ]>, (1)(3,5), 
      <identity partial perm on [ 3, 4 ]>, (3,4), 
      <identity partial perm on [ 1, 3 ]>, (1,3), 
      <identity partial perm on [ 3 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[10]);
[ [ [ 1, 0, 0, 0, 0 ], [ 2, 1, 0, 0, 0 ], [ 3, 2, 1, 0, 0 ], 
      [ 4, 3, 2, 1, 0 ], [ 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 4 ]>, 
      <identity partial perm on [ 2, 3, 5 ]>, 
      <identity partial perm on [ 1, 4 ]>, <identity partial perm on [ 4 ]>, 
      <empty partial perm> ] ]
#@else
gap> CharacterTableOfInverseSemigroup(S[1]);
[ [ [ 1, 0, 0, 0 ], [ 2, 1, 0, 0 ], [ 1, 1, 1, -1 ], [ 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 3, 4, 5, 8, 9, 10 ]>, 
      <identity partial perm on [ 1, 3, 4, 6, 8 ]>, 
      <identity partial perm on [ 1, 3, 4, 8 ]>, (1)(3)(4,8) ] ]
gap> CharacterTableOfInverseSemigroup(S[2]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 3, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 4, 4, 0, 1, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 3, 3, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 3, 3, 2, 1, 2, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 3, 3, 2, 1, 2, 0, 0, 0, 1, E(3)^2, E(3), 0, 0, 0, 0, 0, 0, 0, 0, 0 
         ], [ 4, 3, 3, 2, 1, 2, 0, 0, 0, 1, E(3), E(3)^2, 0, 0, 0, 0, 0, 0, 
          0, 0, 0 ], 
      [ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 19, 19, 20, 10, 9, 10, 4, 4, 4, 4, 1, 1, 3, -1, 1, -1, 1, 0, 0, 0, 0 ]
        , [ 38, 38, 40, 20, 18, 20, 8, 8, 8, 8, -1, -1, 6, 0, 2, 0, -1, 0, 0, 
          0, 0 ], 
      [ 19, 19, 20, 10, 9, 10, 4, 4, 4, 4, 1, 1, 3, 1, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 15, 15, 15, 10, 10, 10, 6, 6, 6, 6, 0, 0, 6, 0, 3, -1, 0, 1, -1, 0, 0 
         ], [ 15, 15, 15, 10, 10, 10, 6, 6, 6, 6, 0, 0, 6, 2, 3, 1, 0, 1, 1, 
          0, 0 ], 
      [ 6, 6, 6, 5, 5, 5, 4, 4, 4, 4, 1, 1, 4, 2, 3, 1, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 3, 4, 5, 6, 7, 8 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 3, 5, 6, 8 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 5, 7 ]>, 
      <identity partial perm on [ 1, 3, 6, 8 ]>, 
      <identity partial perm on [ 1, 3, 4, 7 ]>, 
      <identity partial perm on [ 2, 3, 5, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 6 ]>, (1)(2,4,6), (1)(2,6,4), 
      <identity partial perm on [ 1, 2, 3, 4 ]>, (1)(2,4)(3), 
      <identity partial perm on [ 2, 5, 7 ]>, (2)(5,7), (2,5,7), 
      <identity partial perm on [ 2, 5 ]>, (2,5), 
      <identity partial perm on [ 6 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[3]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 6, 6, 4, 2, 2, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], 
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
      <identity partial perm on [ 1, 3, 6, 7 ]>, 
      <identity partial perm on [ 1, 5, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 3, 6, 7 ]>, 
      <identity partial perm on [ 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 3, 6 ]>, (1,6)(3), 
      <identity partial perm on [ 1, 5, 6 ]>, 
      <identity partial perm on [ 3, 7 ]>, (3,7), 
      <identity partial perm on [ 1 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[4]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 20, 10, 9, 4, 4, 3, 4, 4, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 20, 10, 9, 4, 4, 3, 4, 4, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, E(3)^2, E(3), 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, E(3), E(3)^2, 0, 0, 0, 0 ], 
      [ 15, 10, 10, 6, 6, 6, 6, 6, -2, 3, -1, 3, 0, 0, 1, -1, 0, 0 ], 
      [ 15, 10, 10, 6, 6, 6, 6, 6, 2, 3, 1, 3, 0, 0, 1, 1, 0, 0 ], 
      [ 6, 5, 5, 4, 4, 4, 4, 4, 0, 3, 1, 3, 0, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 3, 4, 5, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 4, 7 ]>, 
      <identity partial perm on [ 1, 4, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 4, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 5, 6 ]>, (1,5)(2,6), 
      <identity partial perm on [ 1, 3, 6 ]>, (1)(3,6), 
      <identity partial perm on [ 2, 3, 4 ]>, (2,3,4), (2,4,3), 
      <identity partial perm on [ 1, 2 ]>, (1,2), 
      <identity partial perm on [ 1 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[5]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 7, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 5, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 19, 10, 4, 4, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 19, 10, 4, 4, 1, E(3), E(3)^2, 0, 0, 0, 0 ], 
      [ 19, 10, 4, 4, 1, E(3)^2, E(3), 0, 0, 0, 0 ], 
      [ 15, 10, 6, 6, 3, 0, 0, 1, -1, 0, 0 ], 
      [ 15, 10, 6, 6, 3, 0, 0, 1, 1, 0, 0 ], 
      [ 6, 5, 4, 4, 3, 0, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 3, 4, 5, 7 ]>, 
      <identity partial perm on [ 1, 2, 3, 6 ]>, 
      <identity partial perm on [ 1, 3, 5, 6 ]>, 
      <identity partial perm on [ 1, 2, 6 ]>, (1,2,6), (1,6,2), 
      <identity partial perm on [ 1, 3 ]>, (1,3), 
      <identity partial perm on [ 4 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[6]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 2, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 3, 2, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 4, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 3, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 5, 3, 2, 2, 2, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ],
      [ 4, 4, 6, 1, 2, 1, 3, 1, 2, 1, 1, 0, 0, 0, 0, 0, 1, -1, 1, 0, 0, 0, 0 ]
        , [ 8, 8, 12, 2, 4, 2, 6, 2, 4, 2, 2, 0, 0, 0, 0, 0, 2, 0, -1, 0, 0, 
          0, 0 ], 
      [ 4, 4, 6, 1, 2, 1, 3, 1, 2, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 ],
      [ 21, 15, 15, 10, 10, 6, 6, 6, 6, 6, 6, 3, 3, 3, 3, 3, 3, -1, 0, 1, -1, 
          0, 0 ], 
      [ 21, 15, 15, 10, 10, 6, 6, 6, 6, 6, 6, 3, 3, 3, 3, 3, 3, 1, 0, 1, 1, 
          0, 0 ], 
      [ 7, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 1, 0, 2, 0, 1, 0 ],
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] 
     ], 
  [ <identity partial perm on [ 1, 2, 5, 6, 7, 9, 10 ]>, 
      <identity partial perm on [ 2, 3, 7, 8, 9, 10 ]>, 
      <identity partial perm on [ 1, 2, 4, 6, 8, 9 ]>, 
      <identity partial perm on [ 1, 5, 6, 7, 10 ]>, 
      <identity partial perm on [ 1, 3, 4, 7, 8 ]>, 
      <identity partial perm on [ 3, 7, 9, 10 ]>, 
      <identity partial perm on [ 1, 4, 8, 9 ]>, 
      <identity partial perm on [ 3, 4, 6, 9 ]>, 
      <identity partial perm on [ 1, 3, 4, 8 ]>, 
      <identity partial perm on [ 1, 4, 7, 8 ]>, 
      <identity partial perm on [ 2, 4, 6, 8 ]>, 
      <identity partial perm on [ 3, 6, 9 ]>, 
      <identity partial perm on [ 6, 8, 9 ]>, 
      <identity partial perm on [ 1, 3, 7 ]>, 
      <identity partial perm on [ 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 4 ]>, 
      <identity partial perm on [ 1, 3, 4 ]>, (1)(3,4), (1,3,4), 
      <identity partial perm on [ 1, 3 ]>, (1,3), 
      <identity partial perm on [ 1 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[7]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, -1, -E(4), E(4), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, -1, E(4), -E(4), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 6, 4, 0, 0, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 5, 4, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 3, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 2, -2, 0, 0, 2, 1, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0 ], 
      [ 4, 2, 2, 0, 0, 2, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 ], 
      [ 10, 4, 0, 0, 0, 5, 2, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0 ], 
      [ 20, 10, -2, 0, 0, 10, 6, 6, 3, 3, 3, -1, 3, 1, -1, 0, 0 ], 
      [ 20, 10, 2, 0, 0, 10, 6, 6, 3, 3, 3, 1, 3, 1, 1, 0, 0 ], 
      [ 7, 5, 1, 1, 1, 5, 4, 4, 3, 3, 3, 1, 3, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 4, 5, 7, 8, 9 ]>, 
      <identity partial perm on [ 1, 2, 4, 5, 7 ]>, (1)(2,4)(5,7), 
      (1)(2,5,4,7), (1)(2,7,4,5), <identity partial perm on [ 1, 2, 4, 6, 7 ]>
        , <identity partial perm on [ 1, 2, 4, 7 ]>, 
      <identity partial perm on [ 3, 4, 5, 7 ]>, 
      <identity partial perm on [ 2, 5, 7 ]>, 
      <identity partial perm on [ 3, 5, 7 ]>, 
      <identity partial perm on [ 2, 4, 6 ]>, (2,4)(6), 
      <identity partial perm on [ 3, 4, 5 ]>, 
      <identity partial perm on [ 2, 4 ]>, (2,4), 
      <identity partial perm on [ 7 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[8]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], [ 7, 4, 3, 1, -1, 1, 0, 0, 0, 0 ], 
      [ 14, 8, 6, 2, 0, -1, 0, 0, 0, 0 ], [ 7, 4, 3, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 10, 6, 6, 3, -1, 0, 1, -1, 0, 0 ], [ 10, 6, 6, 3, 1, 0, 1, 1, 0, 0 ], 
      [ 5, 4, 4, 3, 1, 0, 2, 0, 1, 0 ], [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 2, 3, 4, 5, 6 ]>, 
      <identity partial perm on [ 1, 2, 3, 6 ]>, 
      <identity partial perm on [ 1, 2, 3, 5 ]>, 
      <identity partial perm on [ 2, 3, 6 ]>, (2)(3,6), (2,3,6), 
      <identity partial perm on [ 2, 3 ]>, (2,3), 
      <identity partial perm on [ 6 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[9]);
[ [ [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 1, -1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 0, 1, -1, 0, 0, 0, 0, 0, 0 ], [ 4, 0, 1, 1, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, -1, 1, -1, 0, 0, 0, 0 ], [ 2, 0, 1, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 4, -2, 2, 0, 0, 0, 1, -1, 0, 0 ], [ 4, 2, 2, 0, 0, 0, 1, 1, 0, 0 ], 
      [ 4, 0, 3, 1, 2, 0, 2, 0, 1, 0 ], [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 5 ]>, (1,5)(2,3), 
      <identity partial perm on [ 1, 3, 5 ]>, (1)(3,5), 
      <identity partial perm on [ 3, 4 ]>, (3,4), 
      <identity partial perm on [ 1, 3 ]>, (1,3), 
      <identity partial perm on [ 3 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[10]);
[ [ [ 1, 0, 0, 0, 0 ], [ 2, 1, 0, 0, 0 ], [ 3, 2, 1, 0, 0 ], 
      [ 4, 3, 2, 1, 0 ], [ 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 4 ]>, 
      <identity partial perm on [ 2, 3, 5 ]>, 
      <identity partial perm on [ 1, 4 ]>, <identity partial perm on [ 4 ]>, 
      <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[1]);
[ [ [ 1, 0, 0, 0 ], [ 2, 1, 0, 0 ], [ 1, 1, 1, -1 ], [ 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 3, 4, 5, 8, 9, 10 ]>, 
      <identity partial perm on [ 1, 3, 4, 6, 8 ]>, 
      <identity partial perm on [ 1, 3, 4, 8 ]>, (1)(3)(4,8) ] ]
gap> CharacterTableOfInverseSemigroup(S[2]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 3, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 4, 4, 0, 1, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 3, 3, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 3, 3, 2, 1, 2, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 3, 3, 2, 1, 2, 0, 0, 0, 1, E(3)^2, E(3), 0, 0, 0, 0, 0, 0, 0, 0, 0 
         ], [ 4, 3, 3, 2, 1, 2, 0, 0, 0, 1, E(3), E(3)^2, 0, 0, 0, 0, 0, 0, 
          0, 0, 0 ], 
      [ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 19, 19, 20, 10, 9, 10, 4, 4, 4, 4, 1, 1, 3, -1, 1, -1, 1, 0, 0, 0, 0 ]
        , [ 38, 38, 40, 20, 18, 20, 8, 8, 8, 8, -1, -1, 6, 0, 2, 0, -1, 0, 0, 
          0, 0 ], 
      [ 19, 19, 20, 10, 9, 10, 4, 4, 4, 4, 1, 1, 3, 1, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 15, 15, 15, 10, 10, 10, 6, 6, 6, 6, 0, 0, 6, 0, 3, -1, 0, 1, -1, 0, 0 
         ], [ 15, 15, 15, 10, 10, 10, 6, 6, 6, 6, 0, 0, 6, 2, 3, 1, 0, 1, 1, 
          0, 0 ], 
      [ 6, 6, 6, 5, 5, 5, 4, 4, 4, 4, 1, 1, 4, 2, 3, 1, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 3, 4, 5, 6, 7, 8 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 3, 5, 6, 8 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 5, 7 ]>, 
      <identity partial perm on [ 1, 3, 6, 8 ]>, 
      <identity partial perm on [ 1, 3, 4, 7 ]>, 
      <identity partial perm on [ 2, 3, 5, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 6 ]>, (1)(2,4,6), (1)(2,6,4), 
      <identity partial perm on [ 1, 2, 3, 4 ]>, (1)(2,4)(3), 
      <identity partial perm on [ 2, 5, 7 ]>, (2)(5,7), (2,5,7), 
      <identity partial perm on [ 2, 5 ]>, (2,5), 
      <identity partial perm on [ 6 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[3]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 6, 6, 4, 2, 2, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], 
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
      <identity partial perm on [ 1, 3, 6, 7 ]>, 
      <identity partial perm on [ 1, 5, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 3, 6, 7 ]>, 
      <identity partial perm on [ 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 3, 6 ]>, (1,6)(3), 
      <identity partial perm on [ 1, 5, 6 ]>, 
      <identity partial perm on [ 3, 7 ]>, (3,7), 
      <identity partial perm on [ 1 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[4]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 20, 10, 9, 4, 4, 3, 4, 4, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 20, 10, 9, 4, 4, 3, 4, 4, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, E(3)^2, E(3), 0, 0, 0, 0 ], 
      [ 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, E(3), E(3)^2, 0, 0, 0, 0 ], 
      [ 15, 10, 10, 6, 6, 6, 6, 6, -2, 3, -1, 3, 0, 0, 1, -1, 0, 0 ], 
      [ 15, 10, 10, 6, 6, 6, 6, 6, 2, 3, 1, 3, 0, 0, 1, 1, 0, 0 ], 
      [ 6, 5, 5, 4, 4, 4, 4, 4, 0, 3, 1, 3, 0, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 3, 4, 5, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 4, 7 ]>, 
      <identity partial perm on [ 1, 4, 6, 7 ]>, 
      <identity partial perm on [ 2, 3, 4, 7 ]>, 
      <identity partial perm on [ 1, 2, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 5, 6 ]>, (1,5)(2,6), 
      <identity partial perm on [ 1, 3, 6 ]>, (1)(3,6), 
      <identity partial perm on [ 2, 3, 4 ]>, (2,3,4), (2,4,3), 
      <identity partial perm on [ 1, 2 ]>, (1,2), 
      <identity partial perm on [ 1 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[5]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 7, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 5, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0 ]
        , [ 19, 10, 4, 4, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 19, 10, 4, 4, 1, E(3), E(3)^2, 0, 0, 0, 0 ], 
      [ 19, 10, 4, 4, 1, E(3)^2, E(3), 0, 0, 0, 0 ], 
      [ 15, 10, 6, 6, 3, 0, 0, 1, -1, 0, 0 ], 
      [ 15, 10, 6, 6, 3, 0, 0, 1, 1, 0, 0 ], 
      [ 6, 5, 4, 4, 3, 0, 0, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 5, 6, 7 ]>, 
      <identity partial perm on [ 1, 3, 4, 5, 7 ]>, 
      <identity partial perm on [ 1, 2, 3, 6 ]>, 
      <identity partial perm on [ 1, 3, 5, 6 ]>, 
      <identity partial perm on [ 1, 2, 6 ]>, (1,2,6), (1,6,2), 
      <identity partial perm on [ 1, 3 ]>, (1,3), 
      <identity partial perm on [ 4 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[6]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 2, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 3, 2, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 1, 1, 4, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 2, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 3, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ],
      [ 5, 3, 2, 2, 2, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ],
      [ 4, 4, 6, 1, 2, 1, 3, 1, 2, 1, 1, 0, 0, 0, 0, 0, 1, -1, 1, 0, 0, 0, 0 ]
        , [ 8, 8, 12, 2, 4, 2, 6, 2, 4, 2, 2, 0, 0, 0, 0, 0, 2, 0, -1, 0, 0, 
          0, 0 ], 
      [ 4, 4, 6, 1, 2, 1, 3, 1, 2, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 ],
      [ 21, 15, 15, 10, 10, 6, 6, 6, 6, 6, 6, 3, 3, 3, 3, 3, 3, -1, 0, 1, -1, 
          0, 0 ], 
      [ 21, 15, 15, 10, 10, 6, 6, 6, 6, 6, 6, 3, 3, 3, 3, 3, 3, 1, 0, 1, 1, 
          0, 0 ], 
      [ 7, 6, 6, 5, 5, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 1, 0, 2, 0, 1, 0 ],
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] 
     ], 
  [ <identity partial perm on [ 1, 2, 5, 6, 7, 9, 10 ]>, 
      <identity partial perm on [ 2, 3, 7, 8, 9, 10 ]>, 
      <identity partial perm on [ 1, 2, 4, 6, 8, 9 ]>, 
      <identity partial perm on [ 1, 5, 6, 7, 10 ]>, 
      <identity partial perm on [ 1, 3, 4, 7, 8 ]>, 
      <identity partial perm on [ 3, 7, 9, 10 ]>, 
      <identity partial perm on [ 1, 4, 8, 9 ]>, 
      <identity partial perm on [ 3, 4, 6, 9 ]>, 
      <identity partial perm on [ 1, 3, 4, 8 ]>, 
      <identity partial perm on [ 1, 4, 7, 8 ]>, 
      <identity partial perm on [ 2, 4, 6, 8 ]>, 
      <identity partial perm on [ 3, 6, 9 ]>, 
      <identity partial perm on [ 6, 8, 9 ]>, 
      <identity partial perm on [ 1, 3, 7 ]>, 
      <identity partial perm on [ 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 4 ]>, 
      <identity partial perm on [ 1, 3, 4 ]>, (1)(3,4), (1,3,4), 
      <identity partial perm on [ 1, 3 ]>, (1,3), 
      <identity partial perm on [ 1 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[7]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, 1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, -1, -E(4), E(4), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 1, 1, -1, E(4), -E(4), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 6, 4, 0, 0, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 5, 4, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 3, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 2, -2, 0, 0, 2, 1, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0 ], 
      [ 4, 2, 2, 0, 0, 2, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 ], 
      [ 10, 4, 0, 0, 0, 5, 2, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0 ], 
      [ 20, 10, -2, 0, 0, 10, 6, 6, 3, 3, 3, -1, 3, 1, -1, 0, 0 ], 
      [ 20, 10, 2, 0, 0, 10, 6, 6, 3, 3, 3, 1, 3, 1, 1, 0, 0 ], 
      [ 7, 5, 1, 1, 1, 5, 4, 4, 3, 3, 3, 1, 3, 2, 0, 1, 0 ], 
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 4, 5, 7, 8, 9 ]>, 
      <identity partial perm on [ 1, 2, 4, 5, 7 ]>, (1)(2,4)(5,7), 
      (1)(2,5,4,7), (1)(2,7,4,5), <identity partial perm on [ 1, 2, 4, 6, 7 ]>
        , <identity partial perm on [ 1, 2, 4, 7 ]>, 
      <identity partial perm on [ 3, 4, 5, 7 ]>, 
      <identity partial perm on [ 2, 5, 7 ]>, 
      <identity partial perm on [ 3, 5, 7 ]>, 
      <identity partial perm on [ 2, 4, 6 ]>, (2,4)(6), 
      <identity partial perm on [ 3, 4, 5 ]>, 
      <identity partial perm on [ 2, 4 ]>, (2,4), 
      <identity partial perm on [ 7 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[8]);
[ [ [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], [ 7, 4, 3, 1, -1, 1, 0, 0, 0, 0 ], 
      [ 14, 8, 6, 2, 0, -1, 0, 0, 0, 0 ], [ 7, 4, 3, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 10, 6, 6, 3, -1, 0, 1, -1, 0, 0 ], [ 10, 6, 6, 3, 1, 0, 1, 1, 0, 0 ], 
      [ 5, 4, 4, 3, 1, 0, 2, 0, 1, 0 ], [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 2, 3, 4, 5, 6 ]>, 
      <identity partial perm on [ 1, 2, 3, 6 ]>, 
      <identity partial perm on [ 1, 2, 3, 5 ]>, 
      <identity partial perm on [ 2, 3, 6 ]>, (2)(3,6), (2,3,6), 
      <identity partial perm on [ 2, 3 ]>, (2,3), 
      <identity partial perm on [ 6 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[9]);
[ [ [ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 1, -1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
      [ 4, 0, 1, -1, 0, 0, 0, 0, 0, 0 ], [ 4, 0, 1, 1, 0, 0, 0, 0, 0, 0 ], 
      [ 2, 0, 1, -1, 1, -1, 0, 0, 0, 0 ], [ 2, 0, 1, 1, 1, 1, 0, 0, 0, 0 ], 
      [ 4, -2, 2, 0, 0, 0, 1, -1, 0, 0 ], [ 4, 2, 2, 0, 0, 0, 1, 1, 0, 0 ], 
      [ 4, 0, 3, 1, 2, 0, 2, 0, 1, 0 ], [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 5 ]>, (1,5)(2,3), 
      <identity partial perm on [ 1, 3, 5 ]>, (1)(3,5), 
      <identity partial perm on [ 3, 4 ]>, (3,4), 
      <identity partial perm on [ 1, 3 ]>, (1,3), 
      <identity partial perm on [ 3 ]>, <empty partial perm> ] ]
gap> CharacterTableOfInverseSemigroup(S[10]);
[ [ [ 1, 0, 0, 0, 0 ], [ 2, 1, 0, 0, 0 ], [ 3, 2, 1, 0, 0 ], 
      [ 4, 3, 2, 1, 0 ], [ 1, 1, 1, 1, 1 ] ], 
  [ <identity partial perm on [ 1, 2, 3, 4 ]>, 
      <identity partial perm on [ 2, 3, 5 ]>, 
      <identity partial perm on [ 1, 4 ]>, <identity partial perm on [ 4 ]>, 
      <empty partial perm> ] ]
#@fi

# attrinv: NaturalPartialOrder (for a semigroup), works, 1/1
gap> S := InverseSemigroup([Bipartition([[1, -3], [2, -1], [3, 4, -2, -4]]),
> Bipartition([[1, -1], [2, -3], [3, -2], [4, -4]])]);
<inverse block bijection semigroup of degree 4 with 2 generators>
gap> S := AsSemigroup(IsTransformationSemigroup, S);
<transformation semigroup of size 20, degree 20 with 3 generators>
gap> n := Size(S);;
gap> elts := Elements(S);;
gap> NaturalPartialOrder(S);
[ [ 2, 8, 9, 15, 16, 19 ], [ 9, 16, 19 ], [ 4, 9, 11 ], [ 9 ], [ 9, 16, 18 ], 
  [ 5, 9, 10, 14, 16, 18 ], [ 9, 13, 20 ], [ 9 ], [  ], [ 9 ], [ 9 ], 
  [ 9, 11, 13 ], [ 9 ], [ 9, 10, 16 ], [ 8, 9, 16 ], [ 9 ], [ 4, 9, 20 ], 
  [ 9 ], [ 9 ], [ 9 ] ]
gap> List([1 .. n],
>         i -> Filtered([1 .. n],
>                       j -> i <> j and ForAny(Idempotents(S),
>                                              e -> e * elts[i] = elts[j])));
[ [ 2, 8, 9, 15, 16, 19 ], [ 9, 16, 19 ], [ 4, 9, 11 ], [ 9 ], [ 9, 16, 18 ], 
  [ 5, 9, 10, 14, 16, 18 ], [ 9, 13, 20 ], [ 9 ], [  ], [ 9 ], [ 9 ], 
  [ 9, 11, 13 ], [ 9 ], [ 9, 10, 16 ], [ 8, 9, 16 ], [ 9 ], [ 4, 9, 20 ], 
  [ 9 ], [ 9 ], [ 9 ] ]
gap> last = last2;
true

# attrinv: NaturalPartialOrder (for a semigroup), works, 2
gap> S := Semigroup(SymmetricInverseMonoid(3), rec(acting := true));;
gap> es := IdempotentGeneratedSubsemigroup(S);;
gap> n := Size(es);;
gap> elts := Elements(es);
[ <empty partial perm>, <identity partial perm on [ 1 ]>, 
  <identity partial perm on [ 2 ]>, <identity partial perm on [ 1, 2 ]>, 
  <identity partial perm on [ 3 ]>, <identity partial perm on [ 2, 3 ]>, 
  <identity partial perm on [ 1, 3 ]>, <identity partial perm on [ 1, 2, 3 ]> 
 ]
gap> NaturalPartialOrder(es);
[ [  ], [ 1 ], [ 1 ], [ 1, 2, 3 ], [ 1 ], [ 1, 3, 5 ], [ 1, 2, 5 ], 
  [ 1, 2, 3, 4, 5, 6, 7 ] ]
gap> List([1 .. n],
>         i -> Filtered([1 .. n], j -> elts[j] = elts[j] * elts[i] and i <> j));
[ [  ], [ 1 ], [ 1 ], [ 1, 2, 3 ], [ 1 ], [ 1, 3, 5 ], [ 1, 2, 5 ], 
  [ 1, 2, 3, 4, 5, 6, 7 ] ]
gap> last = last2;
true

# attrinv: NaturalPartialOrder (for a semigroup), works, 3
gap> S := Semigroup(SymmetricInverseMonoid(3), rec(acting := true));;
gap> es := IdempotentGeneratedSubsemigroup(S);;
gap> es := AsSemigroup(IsBlockBijectionSemigroup, es);;
gap> n := Size(es);;
gap> elts := Elements(es);;
gap> NaturalPartialOrder(es);
[ [  ], [ 1 ], [ 1 ], [ 1 ], [ 1, 2, 3 ], [ 1, 2, 4 ], [ 1, 3, 4 ], 
  [ 1, 2, 3, 4, 5, 6, 7 ] ]
gap> List([1 .. n],
>         i -> Filtered([1 .. n], j -> elts[j] = elts[j] * elts[i] and i <> j));
[ [  ], [ 1 ], [ 1 ], [ 1 ], [ 1, 2, 3 ], [ 1, 2, 4 ], [ 1, 3, 4 ], 
  [ 1, 2, 3, 4, 5, 6, 7 ] ]
gap> last = last2;
true

# attrinv: NaturalPartialOrder (for a semigroup), error, 1/2
gap> S := Semigroup(
> [Matrix(IsTropicalMinPlusMatrix,
>     [[infinity, 0, infinity, 1, 1, infinity, 3, 2, 3],
>      [3, 1, 1, infinity, 1, 1, 1, 1, 1], [0, 3, 0, 1, 1, 3, 0, infinity, 1],
>      [0, 0, 1, infinity, infinity, 3, 3, 2, 1], [1, 1, 0, 3, 0, 3, 0, 0, 3],
>      [0, 2, 3, 1, 0, 0, infinity, 3, infinity],
>      [1, 2, 3, 3, 1, 2, infinity, infinity, 3],
>      [1, 1, infinity, 3, 3, 1, 1, 1, 1], [1, 2, 0, infinity, 0, 0, 1, 1, 2]],
> 3)]);
<commutative semigroup of 9x9 tropical min-plus matrices with 1 generator>
gap> NaturalPartialOrder(S);
Error, the argument (a semigroup) is not an inverse semigroup

# attrinv: NaturalPartialOrder (for a semigroup), error, 2/2
gap> NaturalPartialOrder(FreeInverseSemigroup(2));
Error, the argument (a semigroup) is not finite

# attrinv: NaturalLeqInverseSemigroup (for a semigroup), error, 1/2
gap> S := Semigroup([
> PBR(
>  [[-4, -3, -2, -1, 1, 4, 5, 6], [-6, -5, -4, -3, -2, 2, 6],
>   [-6, -4, -3, -1, 1, 2, 4, 5, 6], [-6, 2, 3, 4], [-4, -2, 3, 6],
>   [-6, -3, 1, 3, 4, 6]],
>  [[-5, -2, -1, 3, 4], [-5, -2, 1, 3, 5],
>   [-6, -4, -2, 2, 3, 4, 6], [-5, -3, -1, 2, 4, 6], [-6, -3, 1, 2, 3, 4, 6],
>   [-6, -3, 2, 6]]),
> PBR(
>  [[-6, -5, -3, -2, -1, 3, 6], [-6, -2, 1, 2, 5], [-6, -5, -4, -1, 1, 6],
>   [-6, -5, -4, -2, -1, 2, 5], [-6, -2, -1, 1, 2, 4, 5], [-6, -5, 3, 4, 6]],
>  [[-2, 1, 2, 3, 5, 6], [-6, -5, -4, -3, -2, 1, 3, 5],
>   [-6, -4, -3, -1, 2, 5, 6], [-5, -2, 3, 4, 5],
>   [-6, -5, -4, -3, -2, 1, 2, 3, 5, 6], [-4, 2, 3, 4, 5, 6]])]);
<pbr semigroup of degree 6 with 2 generators>
gap> NaturalLeqInverseSemigroup(S);
Error, the argument (a semigroup) is not an inverse semigroup

# attrinv: NaturalLeqInverseSemigroup (for a semigroup), error, 2/2
gap> NaturalLeqInverseSemigroup(FreeInverseSemigroup(2));
Error, the argument (a semigroup) is not finite

# attrinv: IsGreensDGreaterThanFunc (for an inverse op acting semigroup), 1/1
gap> S := InverseSemigroup(
> [Bipartition([[1, -3], [2, -1], [3, 4, 5, -2, -4, -5]]),
>  Bipartition([[1, -1], [2, -3], [3, -4], [4, 5, -2, -5]])]);
<inverse block bijection semigroup of degree 5 with 2 generators>
gap> Size(S);
39
gap> foo := IsGreensDGreaterThanFunc(S);;
gap> foo(S.1, S.2);
false
gap> foo(S.2, S.1);
true
gap> foo(S.1, S.1);
false

# attrinv: PrimitiveIdempotents, inverse, 1/2
gap> S := InverseSemigroup([PartialPerm([1, 2], [3, 1]),
> PartialPerm([1, 2, 3], [1, 3, 4])]);;
gap> Set(PrimitiveIdempotents(S));
[ <identity partial perm on [ 1 ]>, <identity partial perm on [ 2 ]>, 
  <identity partial perm on [ 3 ]>, <identity partial perm on [ 4 ]> ]

# attrinv: PrimitiveIdempotents, inverse, 2/2
gap> S := InverseSemigroup(
> [PartialPerm([1, 2, 3, 5, 6, 11, 12], [4, 3, 7, 5, 1, 11, 12]),
>  PartialPerm([1, 3, 4, 5, 6, 7, 11, 12], [6, 7, 5, 3, 1, 4, 11, 12]),
>  PartialPerm([11, 12], [12, 11])]);;
gap> PrimitiveIdempotents(S);
[ <identity partial perm on [ 11, 12 ]> ]

# attrinv: PrimitiveIdempotents, semigroup, error, 1/2
gap> PrimitiveIdempotents(FreeSemigroup(2));
Error, the argument (a semigroup) is not finite

# attrinv: PrimitiveIdempotents, semigroup, error, 2/2
gap> PrimitiveIdempotents(FreeBand(2));
Error, the argument (a semigroup) is not an inverse semigroup

# attrinv: PrimitiveIdempotents, transformation semigroups
gap> S := InverseSemigroup([
>  Bipartition([[1, -1, -2], [2, 3, -3], [4, -4]]),
>  Bipartition([[1, 2, 3, -4], [4, -1, -2, -3]])]);
<inverse block bijection semigroup of degree 4 with 2 generators>
gap> PrimitiveIdempotents(S);
[ <block bijection: [ 1, 2, 3, -1, -2, -3 ], [ 4, -4 ]> ]
gap> S := AsSemigroup(IsTransformationSemigroup, S);
<transformation semigroup of size 6, degree 7 with 3 generators>
gap> PrimitiveIdempotents(S);
[ Transformation( [ 4, 2, 4, 4, 4, 4, 4 ] ) ]

#
gap> S := InverseSemigroup([
>  PartialPerm([1, 2, 3, 5], [2, 5, 4, 1]),
>  PartialPerm([1, 2, 4], [3, 4, 2]),
>  PartialPerm([1, 2, 3, 5], [1, 4, 3, 2])]);
<inverse partial perm semigroup of rank 5 with 3 generators>
gap> x := ShallowCopy(PrimitiveIdempotents(S));;
gap> Sort(x);
gap> x;
[ <identity partial perm on [ 1 ]>, <identity partial perm on [ 2 ]>, 
  <identity partial perm on [ 3 ]>, <identity partial perm on [ 4 ]>, 
  <identity partial perm on [ 5 ]> ]
gap> T := AsSemigroup(IsBlockBijectionSemigroup, S);
<inverse block bijection semigroup of degree 6 with 3 generators>
gap> x := ShallowCopy(PrimitiveIdempotents(T));;
gap> Sort(x);
gap> x;
[ <block bijection: [ 1, 2, 3, 4, 6, -1, -2, -3, -4, -6 ], [ 5, -5 ]>, 
  <block bijection: [ 1, 2, 3, 5, 6, -1, -2, -3, -5, -6 ], [ 4, -4 ]>, 
  <block bijection: [ 1, 2, 4, 5, 6, -1, -2, -4, -5, -6 ], [ 3, -3 ]>, 
  <block bijection: [ 1, 3, 4, 5, 6, -1, -3, -4, -5, -6 ], [ 2, -2 ]>, 
  <block bijection: [ 1, -1 ], [ 2, 3, 4, 5, 6, -2, -3, -4, -5, -6 ]> ]
gap> T := AsSemigroup(IsTransformationSemigroup, S);
<transformation semigroup of degree 6 with 6 generators>
gap> x := ShallowCopy(PrimitiveIdempotents(T));;
gap> Sort(x);
gap> x;
[ Transformation( [ 1, 6, 6, 6, 6, 6 ] ), 
  Transformation( [ 6, 2, 6, 6, 6, 6 ] ), 
  Transformation( [ 6, 6, 3, 6, 6, 6 ] ), 
  Transformation( [ 6, 6, 6, 4, 6, 6 ] ), 
  Transformation( [ 6, 6, 6, 6, 5, 6 ] ) ]

# attrinv: IsJoinIrreducible, 1/4
gap> S := InverseSemigroup([
> PartialPerm([1, 2, 3, 4], [4, 1, 2, 6]),
> PartialPerm([1, 2, 3, 4], [5, 7, 1, 6]),
> PartialPerm([1, 2, 3, 5], [5, 2, 7, 3]),
> PartialPerm([1, 2, 3, 6, 7], [1, 3, 4, 7, 5]),
> PartialPerm([1, 2, 3, 4, 5, 7], [3, 2, 4, 6, 1, 5])]);;
gap> I := SemigroupIdeal(S,
> PartialPerm([1, 3, 4, 5, 7], [1, 3, 4, 5, 7]));;
gap> x := PartialPerm([1, 2, 4, 6], [2, 3, 1, 4]);;
gap> x in S;
true
gap> IsJoinIrreducible(S, x);
false
gap> x in I;
true
gap> IsJoinIrreducible(S, RandomBipartition(1));
Error, the 2nd argument (a mult. elt.) does not belong to the 1st argument (an\
 inverse semigroup)
gap> IsJoinIrreducible(S, MultiplicativeZero(S));
false

# attrinv: IsJoinIrreducible, 2/4
gap> S := InverseSemigroup(
> [PartialPerm([1, 2, 3, 5, 6, 11, 12], [4, 3, 7, 5, 1, 11, 12]),
>  PartialPerm([1, 3, 4, 5, 6, 7, 11, 12], [6, 7, 5, 3, 1, 4, 11, 12]),
>  PartialPerm([11, 12], [12, 11])]);;
gap> IsJoinIrreducible(S, PrimitiveIdempotents(S)[1]);
true

# attrinv: IsJoinIrreducible, 3/4
gap> S := DualSymmetricInverseMonoid(3);
<inverse block bijection monoid of degree 3 with 3 generators>
gap> x := Bipartition([[1, 2, -1, -2], [3, -3]]);;
gap> IsJoinIrreducible(S, x);
true

# attrinv: IsJoinIrreducible, 4/4
gap> S := InverseSemigroup([
> PartialPerm([1, 2, 4, 6], [2, 1, 4, 6]),
> PartialPerm([1, 2, 3, 4], [4, 1, 2, 6]),
> PartialPerm([1, 2, 3, 4], [5, 7, 1, 6]),
> PartialPerm([1, 2, 3, 5], [5, 2, 7, 3]),
> PartialPerm([1, 2, 3, 6, 7], [1, 3, 4, 7, 5]),
> PartialPerm([1, 2, 3, 4, 5, 7], [3, 2, 4, 6, 1, 5])]);;
gap> x := PartialPerm([1, 2, 4, 6], [2, 3, 1, 4]);;
gap> IsJoinIrreducible(S, x);
false

# attrinv: IsMajorantlyClosed, 1/1
gap> S := DualSymmetricInverseMonoid(3);
<inverse block bijection monoid of degree 3 with 3 generators>
gap> Size(S);
25
gap> T := InverseMonoid([Bipartition([[1, -1], [2, 3, -2, -3]]),
> Bipartition([[1, -2], [2, 3, -1, -3]])]);
<inverse block bijection monoid of degree 3 with 2 generators>
gap> IsMajorantlyClosed(S, T);
false
gap> IsMajorantlyClosed(S, S);
true
gap> IsMajorantlyClosed(T, S);
Error, the 2nd argument (an inverse semigroup) is not a subsemigroup of the 1s\
t argument (an inverse semigroup)
gap> IsMajorantlyClosed(S, Elements(T));
false
gap> IsMajorantlyClosed(S, Elements(S));
true
gap> IsMajorantlyClosed(T, Elements(S));
Error, the 2nd argument (a mult. elt. coll) is not a subset of the 1st argumen\
t (an inverse semigroup)
gap> IsMajorantlyClosed(S, [One(S)]);
true

# attrinv: JoinIrreducibleDClasses, partial perms, 1
gap> S := InverseSemigroup([PartialPerm([1, 2, 3, 4], [2, 4, 1, 5]),
> PartialPerm([1, 3, 5], [5, 1, 3])]);;
gap> JoinIrreducibleDClasses(S)[1] = DClass(S, PartialPerm([3], [3]));
true
gap> S := InverseSemigroup(S, rec(acting := false));;
gap> JoinIrreducibleDClasses(S)[1] = DClass(S, PartialPerm([3], [3]));
true

# attrinv: JoinIrreducibleDClasses, partial perms, 2
gap> S := InverseSemigroup([PartialPerm([1, 2, 3, 4], [2, 4, 1, 5]),
> PartialPerm([1, 3, 5], [5, 1, 3])]);;
gap> JoinIrreducibleDClasses(S)[1] = DClass(S, PartialPerm([3], [3]));
true

# attrinv: JoinIrreducibleDClasses, partial perms, 3
gap> S := Semigroup(
> [PartialPerm([1, 2, 3, 4], [1, 2, 3, 4]),
>  PartialPerm([1, 2, 3], [2, 3, 1])]);;
gap> IsInverseSemigroup(S);
true
gap> JoinIrreducibleDClasses(S) = DClasses(S);
true
gap> ForAll(DClassReps(S), x -> IsJoinIrreducible(S, x));
true

# attrinv: JoinIrreducibleDClasses, partial perms, 4
gap> S := Semigroup([
> PartialPerm([1, 2, 3, 4, 5, 7, 6]),
> PartialPerm([2, 1]),
> PartialPerm([4, 5], [5, 4])]);;
gap> D := JoinIrreducibleDClasses(S);;
gap> reps := Filtered(DClassReps(S), x -> x <> MultiplicativeZero(S));;
gap> Length(D) = Length(reps);
true
gap> Length(D);
3
gap> ForAll(reps, x -> ForAny(D, d -> x in d));
true

# attrinv: JoinIrreducibleDClasses, inverse op, 1/?
gap> S := InverseMonoid([Bipartition([[1, 2, 5, -2, -3, -5], [3, 4, -1, -4]]),
> Bipartition([[1, 4, -5], [2, 5, -1, -2, -3], [3, -4]])]);;
gap> jid :=
> [DClass(S, Bipartition([[1, 4, -1, -4], [2, 3, 5, -2, -3, -5]])),
>  DClass(S, Bipartition([[1, 2, 3, -1, -2, -3], [4, -4], [5, -5]]))];;
gap> JoinIrreducibleDClasses(S) = jid or JoinIrreducibleDClasses(S) = Set(jid);
true

# attrinv: MajorantClosure, 1/1
gap> S := DualSymmetricInverseMonoid(3);
<inverse block bijection monoid of degree 3 with 3 generators>
gap> Size(S);
25
gap> T := InverseMonoid([Bipartition([[1, -1], [2, 3, -2, -3]]),
> Bipartition([[1, -2], [2, 3, -1, -3]])]);
<inverse block bijection monoid of degree 3 with 2 generators>
gap> MajorantClosure(S, T);
[ <block bijection: [ 1, 2, 3, -1, -2, -3 ]>, 
  <block bijection: [ 1, 3, -1, -3 ], [ 2, -2 ]>, 
  <block bijection: [ 1, 3, -2, -3 ], [ 2, -1 ]>, 
  <block bijection: [ 1, -1 ], [ 2, 3, -2, -3 ]>, 
  <block bijection: [ 1, -2 ], [ 2, 3, -1, -3 ]>, 
  <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ]>, 
  <block bijection: [ 1, 2, -1, -2 ], [ 3, -3 ]>, 
  <block bijection: [ 1, 2, -1, -3 ], [ 3, -2 ]>, 
  <block bijection: [ 1, 2, -1 ], [ 3, -2, -3 ]>, 
  <block bijection: [ 1, 2, -2, -3 ], [ 3, -1 ]>, 
  <block bijection: [ 1, 2, -2 ], [ 3, -1, -3 ]>, 
  <block bijection: [ 1, 2, -3 ], [ 3, -1, -2 ]>, 
  <block bijection: [ 1, 3, -1, -2 ], [ 2, -3 ]>, 
  <block bijection: [ 1, 3, -1 ], [ 2, -2, -3 ]>, 
  <block bijection: [ 1, 3, -2 ], [ 2, -1, -3 ]>, 
  <block bijection: [ 1, 3, -3 ], [ 2, -1, -2 ]>, 
  <block bijection: [ 1, -1, -2 ], [ 2, 3, -3 ]>, 
  <block bijection: [ 1, -1, -3 ], [ 2, 3, -2 ]>, 
  <block bijection: [ 1, -2, -3 ], [ 2, 3, -1 ]>, 
  <block bijection: [ 1, -3 ], [ 2, 3, -1, -2 ]>, 
  <block bijection: [ 1, -1 ], [ 2, -3 ], [ 3, -2 ]>, 
  <block bijection: [ 1, -2 ], [ 2, -1 ], [ 3, -3 ]>, 
  <block bijection: [ 1, -3 ], [ 2, -1 ], [ 3, -2 ]>, 
  <block bijection: [ 1, -2 ], [ 2, -3 ], [ 3, -1 ]>, 
  <block bijection: [ 1, -3 ], [ 2, -2 ], [ 3, -1 ]> ]
gap> MajorantClosure(T, S);
Error, the 2nd argument (a semigroup) is not a subset of the 1st argument (an \
inverse semigroup)
gap> MajorantClosure(S, Elements(T));
[ <block bijection: [ 1, 2, 3, -1, -2, -3 ]>, 
  <block bijection: [ 1, 3, -1, -3 ], [ 2, -2 ]>, 
  <block bijection: [ 1, 3, -2, -3 ], [ 2, -1 ]>, 
  <block bijection: [ 1, -1 ], [ 2, 3, -2, -3 ]>, 
  <block bijection: [ 1, -2 ], [ 2, 3, -1, -3 ]>, 
  <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ]>, 
  <block bijection: [ 1, 2, -1, -2 ], [ 3, -3 ]>, 
  <block bijection: [ 1, 2, -1, -3 ], [ 3, -2 ]>, 
  <block bijection: [ 1, 2, -1 ], [ 3, -2, -3 ]>, 
  <block bijection: [ 1, 2, -2, -3 ], [ 3, -1 ]>, 
  <block bijection: [ 1, 2, -2 ], [ 3, -1, -3 ]>, 
  <block bijection: [ 1, 2, -3 ], [ 3, -1, -2 ]>, 
  <block bijection: [ 1, 3, -1, -2 ], [ 2, -3 ]>, 
  <block bijection: [ 1, 3, -1 ], [ 2, -2, -3 ]>, 
  <block bijection: [ 1, 3, -2 ], [ 2, -1, -3 ]>, 
  <block bijection: [ 1, 3, -3 ], [ 2, -1, -2 ]>, 
  <block bijection: [ 1, -1, -2 ], [ 2, 3, -3 ]>, 
  <block bijection: [ 1, -1, -3 ], [ 2, 3, -2 ]>, 
  <block bijection: [ 1, -2, -3 ], [ 2, 3, -1 ]>, 
  <block bijection: [ 1, -3 ], [ 2, 3, -1, -2 ]>, 
  <block bijection: [ 1, -1 ], [ 2, -3 ], [ 3, -2 ]>, 
  <block bijection: [ 1, -2 ], [ 2, -1 ], [ 3, -3 ]>, 
  <block bijection: [ 1, -3 ], [ 2, -1 ], [ 3, -2 ]>, 
  <block bijection: [ 1, -2 ], [ 2, -3 ], [ 3, -1 ]>, 
  <block bijection: [ 1, -3 ], [ 2, -2 ], [ 3, -1 ]> ]
gap> MajorantClosure(S, Elements(S)) = Elements(S);
true
gap> MajorantClosure(T, Elements(S));
Error, the 2nd argument (a mult. elt. coll.) is not a subset of the 1st argume\
nt (an inverse semigroup)
gap> MajorantClosure(S, [One(S)]);
[ <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ]> ]

# attrinv: RightCosetsOfInverseSemigroup, 1/2
gap> S := InverseMonoid([PartialPerm([1, 2, 3, 4], [2, 4, 1, 5]),
> PartialPerm([1, 3, 5], [5, 1, 3])]);;
gap> T := InverseSemigroup(
> [PartialPerm([3], [4]), PartialPerm([1, 3, 5], [3, 5, 1])]);;
gap> RightCosetsOfInverseSemigroup(S, T);
Error, the 2nd argument (an inverse semigroup) must be majorantly closed
gap> RightCosetsOfInverseSemigroup(S,
> InverseSemigroup(MajorantClosure(S, T), rec(small := true)));
[ [ <empty partial perm>, <identity partial perm on [ 1 ]>, [1,2], [1,3], 
      [1,4], [1,5], [2,1], <identity partial perm on [ 2 ]>, [2,3], [2,4], 
      [2,5], <identity partial perm on [ 1, 2 ]>, [2,5](1), [1,2,4], [2,1,3], 
      [1,3][2,4], [1,4][2,5], [1,5][2,3], [3,1], [3,2], 
      <identity partial perm on [ 3 ]>, [3,4], [3,5], 
      <identity partial perm on [ 1, 3 ]>, 
      <identity partial perm on [ 1, 2, 3 ]>, [3,1,2], [3,1,2,4], [1,3,5], 
      [1,4][3,2], [1,4](3), [1,4][3,2,5], [3,1,5], [1,5][3,4], [4,1], [4,2], 
      [4,3], <identity partial perm on [ 4 ]>, [4,5], [3,1][4,2], [3,1][4,5], 
      [3,2](4), [4,1](3), <identity partial perm on [ 3, 4 ]>, [3,4,5], 
      [4,3,5], [4,2,1], [2,1][4,5], <identity partial perm on [ 2, 4 ]>, 
      [2,3][4,1], [2,3](4), [2,4,5], [2,5][4,3], 
      <identity partial perm on [ 1, 2, 4 ]>, 
      <identity partial perm on [ 1, 2, 3, 4 ]>, [1,2,4,5], [3,1,2,4,5], 
      [4,2,1,3], [5,1], [5,2], [5,3], [5,4], <identity partial perm on [ 5 ]>,
      [4,1][5,2], [4,1](5), [5,4,2], [4,3][5,1], [5,4,3], 
      <identity partial perm on [ 4, 5 ]>, [4,5,3], [5,3,1], [3,2][5,1], 
      <identity partial perm on [ 3, 5 ]>, [3,4][5,2], [5,3,4], [3,5,1], 
      [3,5,4], [5,4,2,1], <identity partial perm on [ 2, 4, 5 ]>, 
      [4,1][5,2,3], [5,2](1), <identity partial perm on [ 1, 5 ]>, 
      <identity partial perm on [ 1, 3, 5 ]>, 
      <identity partial perm on [ 1, 2, 4, 5 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 5 ]>, [1,2][5,4], [5,1,3], 
      [1,3][5,4], (1,3,5), [5,4,2,1,3], [1,4](5), [1,5,3], (1,5,3) ] ]
gap> T := InverseSemigroup([PartialPerm([1, 2, 4, 6, 8], [2, 10, 3, 5, 7]),
> PartialPerm([1, 3, 4, 5, 6, 7, 8], [4, 7, 6, 9, 10, 1, 3])]);;
gap> RightCosetsOfInverseSemigroup(S, T);
Error, the 2nd argument (an inverse semigroup) must be a subsemigroup of the 1\
st argument (an inverse semigroup)

# attrinv: RightCosetsOfInverseSemigroup, 2/2
gap> S := InverseSemigroup([
> PartialPerm([1, 2, 3, 4], [4, 1, 2, 6]),
> PartialPerm([1, 2, 3, 4], [5, 7, 1, 6]),
> PartialPerm([1, 2, 3, 5], [5, 2, 7, 3]),
> PartialPerm([1, 2, 3, 6, 7], [1, 3, 4, 7, 5]),
> PartialPerm([1, 2, 3, 4, 5, 7], [3, 2, 4, 6, 1, 5])]);;
gap> W := InverseSemigroup(MajorantClosure(S,
> [PartialPerm([1, 2, 3, 4], [1, 2, 3, 4])]));
<inverse partial perm semigroup of rank 7 with 5 generators>
gap> Set(RightCosetsOfInverseSemigroup(S, W));
[ [ <identity partial perm on [ 1, 2, 3, 4 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 5 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 6 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 5, 6 ]>, 
      <identity partial perm on [ 1, 2, 3, 4, 5, 7 ]> ], [ [2,5](1)(3)(4) ], 
  [ [4,3,2,7](1) ], [ [2,1,3,4,6] ], 
  [ [1,3,4,6](2), [5,1,3,4,6](2), [7,5,1,3,4,6](2) ], [ [1,3,5][4,7](2) ], 
  [ [1,3,2,5](4) ], [ [3,2,1,4,6] ], [ [3,1,4,5](2) ], 
  [ [4,3,1,5](2), [4,3,1,5,7](2), [6,4,3,1,5](2), [6,4,3,1,5,7](2) ], 
  [ [1,5][2,4,3,6] ], [ [2,7][4,3,1,5] ], [ [2,7][3,1,5][4,6] ], 
  [ [4,1,6](2)(3) ], [ [3,5][4,1,7](2), [4,1,7][6,3,5](2) ], [ [2,3,4,1,7] ], 
  [ [3,1,7][4,2,6] ] ]

# attrinv: SupremumIdempotents, 1/1
gap> SupremumIdempotentsNC([], PartialPerm([]));
<empty partial perm>
gap> SupremumIdempotentsNC([], Bipartition([[1], [-1]]));
<bipartition: [ 1 ], [ -1 ]>
gap> SupremumIdempotentsNC([], Bipartition([[1, -1]]));
<block bijection: [ 1, -1 ]>
gap> SupremumIdempotentsNC(Idempotents(DualSymmetricInverseMonoid(3)),
> RandomBlockBijection(3));
<block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ]>
gap> SupremumIdempotentsNC(Transformation([1, 1]), 1);
Error, the argument is not a collection of partial perms, block bijections, or\
 partial perm bipartitions

# attrinv: InversesOfSemigroupElementNC, 1/1
gap> S := InverseSemigroup(
>  [Bipartition([[1, 2, 4, -2, -3], [3, -4, -5], [5, -1]]),
>   Bipartition([[1, 2, 3, 4, 5, -1, -2, -3, -4, -5]]),
>   Bipartition([[1, 2, 3, 4, -2, -3], [5, -1, -4, -5]])]);;
gap> x := Bipartition([[1, -3, -5], [2, 3, 4, 5, -1, -2, -4]]);;
gap> InversesOfSemigroupElementNC(S, x);
[ <block bijection: [ 1, 2, 4, -2, -3, -4, -5 ], [ 3, 5, -1 ]> ]
gap> x in last;
false

# attrinv: IdempotentGeneratedSubsemigroup
gap> IdempotentGeneratedSubsemigroup(FreeInverseSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 4th choice method found for `IdempotentGeneratedSubsemigroup' on 1 a\
rguments
gap> S := InverseSemigroup([
>  PartialPerm([1, 2, 3, 4], [6, 2, 4, 3]),
>  PartialPerm([1, 2, 3, 5], [5, 6, 3, 2]),
>  PartialPerm([1, 2, 5], [3, 5, 4])]);;
gap> S := IdempotentGeneratedSubsemigroup(S);;
gap> HasIsIdempotentGenerated(S) and IsIdempotentGenerated(S);
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/attributes/inverse.tst");
