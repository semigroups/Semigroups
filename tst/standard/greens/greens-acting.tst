#############################################################################
##
#W  standard/greens/greens-acting.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/greens/greens-acting.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# greens-acting: DClassOfLClass, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> L := LClass(S, PartialPerm([1, 7], [3, 5]));;
gap> Size(L);
16
gap> D := DClass(L);;
gap> Size(D);
128
gap> DD := DClassOfLClass(L);;
gap> DD = D;
true
gap> DDD := DClass(S, Representative(L));;
gap> DDD = DD;
true

# greens-acting: DClassOfRClass, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])]);;
gap> R := RClass(S, Transformation([4, 4, 5, 4, 4, 4]));;
gap> Size(R);
30
gap> D := DClass(R);;
gap> Size(D);
930
gap> DD := DClassOfRClass(R);;
gap> DD = D;
true
gap> DDD := DClass(S, Representative(R));;
gap> DDD = DD;
true

# greens-acting: DClassOfHClass, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> H := HClass(S, S.4);;
gap> Size(H);
1
gap> D := DClass(H);;
gap> Size(D);
1
gap> DD := DClassOfHClass(H);;
gap> DD = D;
true
gap> DDD := DClass(S, Representative(H));;
gap> DDD = DD;
true

# greens-acting: LClassOfHClass, 1/1
gap> S := Monoid(
> [Bipartition([[1, 2, 3, 4, 5, -1], [6, -5], [-2, -3, -4], [-6]]),
>  Bipartition([[1, 2, 3, 5, -3, -4, -5], [4, 6, -2], [-1, -6]]),
>  Bipartition([[1, 2, -5, -6], [3, 5, 6, -1, -4], [4, -2, -3]]),
>  Bipartition([[1, 3, -3], [2, 5, 6, -2], [4, -1, -4, -5], [-6]]),
>  Bipartition([[1, 3, -1, -6], [2, 6, -2], [4, -3, -5], [5], [-4]]),
>  Bipartition([[1, -3], [2, 3, 4, 5, -1, -4], [6, -2, -6], [-5]]),
>  Bipartition([[1, 5, -5, -6], [2, 3, -1, -2, -4], [4, 6, -3]]),
>  Bipartition([[1, 4, 6, -1, -2, -4], [2, 5, -5, -6], [3], [-3]]),
>  Bipartition([[1, 5, -1, -3], [2, 4, 6], [3, -2, -6], [-4, -5]]),
>  Bipartition([[1, 5, -2], [2, -1, -5], [3, 4, -6], [6, -3], [-4]])]);;
gap> H := HClass(S, S.1 * S.5 * S.8);;
gap> Size(H);
1
gap> L := LClass(H);;
gap> Size(L);
26
gap> LL := LClassOfHClass(H);;
gap> LL = L;
true
gap> L3 := LClass(S, Representative(H));;
gap> L3 = LL;
true

# greens-acting: RClassOfHClass, 1/1
gap> S := ReesZeroMatrixSemigroup(SymmetricGroup(3), [
> [(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, (), 0, (1, 3), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, (), (1, 3), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, (1, 3), (2, 3), (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
>  0],
> [0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, (), (2, 3), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, (1, 3, 2), (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
>  0],
> [0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, (), (1, 3), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, (1, 2), 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (1, 3), (), (), 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), (1, 2), 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), (1, 2, 3), (1, 3, 2),
>  0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), (1, 3), 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ()]]);;
gap> S := Semigroup(S);
<subsemigroup of 23x23 Rees 0-matrix semigroup with 46 generators>
gap> Size(S);
3175
gap> H := HClass(S, S.1);;
gap> Size(H);
6
gap> R := RClass(H);;
gap> Size(R);
138
gap> RR := RClassOfHClass(H);;
gap> RR = R;
true
gap> RRR := RClass(S, Representative(H));;
gap> RRR = RR;
true

# greens-acting: GreensDClassOfElement, fail, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(generic := false));;
gap> GreensDClassOfElement(S, PartialPerm([19]));
Error, Semigroups: GreensDClassOfElement: usage,
the element does not belong to the semigroup,

# greens-acting: GreensDClassOfElementNC, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(generic := false));;
gap> D := GreensDClassOfElementNC(S, PartialPerm([19]));;
gap> Size(D);
5

# greens-acting: GreensL/RClassOfElement, fail, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(generic := false));;
gap> RClass(S, ConstantTransformation(7, 7));
Error, Semigroups: GreensRClassOfElement: usage,
the element does not belong to the semigroup,
gap> LClass(S, ConstantTransformation(7, 7));
Error, Semigroups: GreensLClassOfElement: usage,
the element does not belong to the semigroup,
gap> HClass(S, ConstantTransformation(7, 7));
Error, Semigroups: GreensHClassOfElement: usage,
the element does not belong to the semigroup,

# greens-acting: GreensL/RClassOfElementNC, fail, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(generic := false));;
gap> Size(RClassNC(S, ConstantTransformation(7, 7)));
1
gap> Size(LClassNC(S, ConstantTransformation(7, 7)));
1
gap> Size(HClassNC(S, ConstantTransformation(7, 7)));
1

# greens-acting: GreensL/RClassOfElement, for a D-class, 1/1
gap> S := Monoid(
> [Bipartition([[1, 2, 3, 4, 5, -1], [6, -5], [-2, -3, -4], [-6]]),
>   Bipartition([[1, 2, 3, 5, -3, -4, -5], [4, 6, -2], [-1, -6]]),
>   Bipartition([[1, 2, -5, -6], [3, 5, 6, -1, -4], [4, -2, -3]]),
>   Bipartition([[1, 3, -3], [2, 5, 6, -2], [4, -1, -4, -5], [-6]]),
>   Bipartition([[1, 3, -1, -6], [2, 6, -2], [4, -3, -5], [5], [-4]]),
>   Bipartition([[1, -3], [2, 3, 4, 5, -1, -4], [6, -2, -6], [-5]]),
>   Bipartition([[1, 5, -5, -6], [2, 3, -1, -2, -4], [4, 6, -3]]),
>   Bipartition([[1, 4, 6, -1, -2, -4], [2, 5, -5, -6], [3], [-3]]),
>   Bipartition([[1, 5, -1, -3], [2, 4, 6], [3, -2, -6], [-4, -5]]),
>   Bipartition([[1, 5, -2], [2, -1, -5], [3, 4, -6], [6, -3], [-4]])],
> rec(generic := false));;
gap> D := DClass(S, S.4 * S.5);;
gap> Size(D);
12
gap> x := Bipartition([[1, 3, 4, -2], [2, 5, 6, -1, -6],
> [-3, -5], [-4]]);;
gap> R := RClass(D, x);;
gap> Size(R);
12
gap> L := LClass(D, x);;
gap> Size(L);
1
gap> LClass(D, IdentityBipartition(8));
Error, Semigroups: GreensLClassOfElement: usage,
the element does not belong to the D-class,
gap> RClass(D, IdentityBipartition(8));
Error, Semigroups: GreensRClassOfElement: usage,
the element does not belong to the D-class,
gap> x := Bipartition([[1, 4, -1, -2, -6], [2, 3, 5, -4],
> [6, -3], [-5]]);;
gap> LClassNC(D, x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `IsBound[]' on 2 arguments
gap> RClassNC(D, x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `IsBound[]' on 2 arguments

# greens-acting: GreensClassOfElementNC(D-class, x) inverse-op, 1/1
gap> S := InverseSemigroup([
>  PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(generic := false));;
gap> D := DClass(S, S.3 * S.2);;
gap> Size(LClassNC(D, S.3 * S.2));
3

# greens-acting: GreensHClassOfElement, 1/1
gap> S := ReesZeroMatrixSemigroup(SymmetricGroup(3), [
> [(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, (), 0, (1, 3), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, (), (1, 3), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, (1, 3), (2, 3), (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
>  0],
> [0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, (), (2, 3), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, (1, 3, 2), (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
>  0],
> [0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, (), (1, 3), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, (1, 2), 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (1, 3), (), (), 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), (1, 2), 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), (1, 2, 3), (1, 3, 2),
>  0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), (1, 3), 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ()]]);;
gap> S := Semigroup(S, rec(generic := false));;
gap> D := DClass(S, S.4 * S.5);;
gap> H := HClass(D, MultiplicativeZero(S));
<Green's H-class: 0>
gap> H := HClassNC(D, MultiplicativeZero(S));
<Green's H-class: 0>
gap> H := HClass(D, IdentityTransformation);
Error, Semigroups: GreensHClassOfElement: usage,
the element does not belong to the D-class,

# greens-acting: GreensHClassOfElement(L/R-class, x), 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(generic := false));;
gap> R := RClass(S, S.3 * S.1 * S.8);;
gap> Size(R);
30
gap> Size(HClass(R, S.3 * S.1 * S.8));
2
gap> L := LClass(S, S.3 * S.1 * S.8);;
gap> Size(L);
62
gap> Size(HClass(L, S.3 * S.1 * S.8));
2
gap> Size(HClassNC(L, S.3 * S.1 * S.8));
2

# greens-acting: \in, for D-class, 1/4
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> D := DClass(S, S.1);;
gap> ForAll(D, x -> x in D);
true
gap> Size(D);
1
gap> Number(S, x -> x in D);
1

# greens-acting: \in, for D-class, 2/4
gap> S := OrderEndomorphisms(5);;
gap> x := Transformation([1, 2, 2, 4, 5]);;
gap> D := DClass(S, x);;
gap> x in D;
true
gap> Transformation([1, 2, 1, 4, 5]) in D;
false

# greens-acting: \in, for D-class, 3/4
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2)]),
> [[0, 0, 0, ()],
>  [(), 0, 0, 0],
>  [(), (), 0, 0],
>  [0, (), (), 0],
>  [0, 0, (), ()]]);;
gap> S := Semigroup(S);;
gap> D := DClass(S, S.1);;
gap> Size(S);
41
gap> Size(D) = Size(S) - 1;
true
gap> ForAll(D, x -> x in D);
true

# greens-acting: \in, for D-class, 4/4
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> D := DClass(S, x);;
gap> y in D;
false

# greens-acting: \in, for L-class, 1/5
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> L := LClass(S, S.1);;
gap> ForAll(L, x -> x in L);
true
gap> Size(L);
1
gap> Number(S, x -> x in L);
1

# greens-acting: \in, for L-class, 2/5
gap> S := OrderEndomorphisms(5);;
gap> x := Transformation([1, 2, 2, 4, 5]);;
gap> L := LClass(S, x);;
gap> x in L;
true
gap> Transformation([1, 2, 1, 4, 5]) in L;
false

# greens-acting: \in, for L-class, 3/5
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2)]),
> [[0, 0, 0, ()],
>  [(), 0, 0, 0],
>  [(), (), 0, 0],
>  [0, (), (), 0],
>  [0, 0, (), ()]]);;
gap> S := Semigroup(S);;
gap> L := LClass(S, S.1);;
gap> Size(S);
41
gap> ForAll(L, x -> x in L);
true

# greens-acting: \in, for L-class, 4/5
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> L := LClass(S, x);;
gap> y in L;
false

# greens-acting: \in, for L-class, 5/5
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 4, 3, 5, 5]);;
gap> L := LClass(S, x);;
gap> y in L;
false

# greens-acting: \in, for R-class, 1/6
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> R := LClass(S, S.1);;
gap> ForAll(R, x -> x in R);
true
gap> Size(R);
1
gap> Number(S, x -> x in R);
1

# greens-acting: \in, for R-class, 2/6
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 4, 3, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# greens-acting: \in, for R-class, 3/6
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 3, 3, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# greens-acting: \in, for R-class, 4/6
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 2, 3, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# greens-acting: \in, for R-class, 5/6
gap> S := OrderEndomorphisms(5);;
gap> x := Transformation([1, 2, 2, 4, 5]);;
gap> R := RClass(S, x);;
gap> x in R;
true
gap> Transformation([1, 2, 1, 4, 5]) in R;
false

# greens-acting: \in, for R-class, 6/6
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# greens-acting: \in, for H-class, 1/3
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> H := HClass(S, x);;
gap> y in H;
false

# greens-acting: \in, for H-class, 2/3
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 2, 3, 5, 5]);;
gap> H := HClass(S, x);;
gap> y in H;
false

# greens-acting: \in, for H-class, 3/3
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> H := HClass(S, x);;
gap> ForAll(H, x -> x in H);
true

# greens-acting: \in, for D-class reps/D-classes, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(generic := false));;
gap> DClassReps(S);
[ [4,5,7](1,3)(6), [6,4,7,1,2,5](3), [4,7,2,5,1,3](6), [1,3][2,7,5][4,6], 
  [4,7](1)(3)(6), [2,7,3,1][6,5], [2,7][5,3](1)(6), [4,6](1)(7), 
  [5,1,3,2][6,4], [4,1,5][6,7,2](3), [6,4,1,3][7,5,2], [2,1,3](4), 
  [4,1][5,2](3)(6), [4,2,1,5][6,7,3], [4,2,1][7,5,3](6), [4,6][7,1](2), 
  <identity partial perm on [ 3, 5 ]>, [1,7,3][4,5](6), [4,5,3](7), [2,5], 
  (1,3)(6), [6,7,1](3), [5,1,3](6), [1,3][4,6], [6,5,3](1), [4,3,1,7], 
  [4,3][6,5](1)(7), [2,3][4,5](1), [4,3,1](6), [2,3](1,7), [2,3][5,1](6)(7), 
  [4,6][7,3], [4,7,1](6), [4,5,3], [6,4,1,2](3), [7,3,2,1], [5,3][6,4](1,2), 
  [7,1,2](4), [1,3,5,2][6,7], [4,2][6,1][7,5](3), [1,3][4,2][6,7](5), 
  [1,3][4,7](2), [6,4,2](3)(5), [4,5][6,1][7,3](2), [6,4,5,3](2), [7,2,5](4), 
  [6,4][7,3](1), [7,5,3], <empty partial perm>, [1,3][4,2](6), [6,1](2)(3), 
  [1,3](2)(6), [1,3][4,6][7,2], [6,7](3,5), [4,3][6,2][7,5](1), 
  [4,3][6,7,1](5), [2,3][4,5], [4,3](5)(6), [4,5][6,2,3](1), 
  [2,3][4,5][7,1](6), [2,5][4,6][7,3], [4,1,2](6), [4,3][7,5], [5,3,7](6), 
  [4,3][6,5](7), [4,3][5,7](6), [2,3][4,7][6,5], [2,5][7,3], [5,1,3][6,7], 
  [4,5](3), [4,1,3][6,7], [2,1,3][4,7], [2,1][5,3](6), [4,3,1][6,5], 
  [2,3][6,5,1], [6,3,1](7), [3,1][6,5,7], [4,7,1][6,3], [4,7][6,5,1], 
  [2,7][4,5], [6,5][7,1,3], [4,1,3](7), [4,1][5,7,3], [4,1][5,7](6), 
  [2,1][4,7,3](6), [2,7,1][4,6], [4,3](6), [4,5], [4,3][6,5], [1,3,2][6,4], 
  [6,1][7,2](3), [1,3][5,2][6,4], [1,3](4), [4,3,2](1), [4,3][7,1,2], 
  [6,4,3,2], [7,2,3](1), [5,2,3][6,4][7,1], [7,3](4), [6,4,1][7,2], 
  [1,5][4,2][6,7](3), [7,3,5](2), [1,5,3][6,7](2), [1,5][4,7,2], [6,1,3](5), 
  [4,5][6,2](3), [4,5][6,1,3], [2,5][4,1,3], [4,5][6,7](3), [6,2,5][7,3], 
  [2,5,3][6,7], [4,7,5], [1,2][6,7,3], [1,3][6,4,5], [6,2,5](3), 
  [1,3][2,5][6,4], [1,3][7,5](4), [1,2][4,3][6,5], [4,3][6,1][7,2], [6,4,3], 
  [1,2,3][6,5], [6,4][7,2,3], [1,5][6,4,2], [5,3,1][6,4], [5,1][6,4,3], 
  [4,6], [6,1,3][7,2], [5,3][7,2](6), [4,2][7,3](6), [1,5][4,3][6,7], 
  [1,5][2,3][6,7], [1,5][4,7,3], [3,1][6,2](5), [4,5][6,3][7,1], 
  [4,5,1][6,2], [4,5,1][6,7], [2,5][4,1][6,3], [2,5][4,1][6,7], [2,1][4,7,5], 
  [5,1][6,2][7,3], [4,1,3][6,5], [4,1][7,3](6), [1,5][4,3](6), [1,7][4,3](6), 
  [1,7][2,3](6), [1,7,3][4,6], [4,7][6,5], [4,1][6,7](3), [2,1][5,3][6,7], 
  [6,7,3](1), [2,3][7,1](6), [2,1][6,5][7,3], [5,7][6,3](1), [4,7][6,3](1), 
  [2,7][4,3](1), [4,7][6,5](1), [2,7][6,5](1), [4,5](1)(7), [6,5,1](3), 
  [4,1][6,5,3], [4,7,3][6,1], [2,7][4,3][6,1], [4,3][6,7](1), [4,1,7](6), 
  [1,2][5,3][6,4], [5,2][6,1,3], [4,2][6,1,3], [4,1,3](2), [6,4,2](3), 
  [5,3][6,4](2), [6,3,2][7,1], [4,1][6,3][7,2], [4,2][7,1,3], [4,2][5,1][7,3],
  [5,1][6,4,2], [6,4,1][7,3](2), [7,2,1](4), [1,3,5][6,7], [6,2][7,5](3), 
  [1,3][6,7](5), [1,2][4,3,5], [1,5][4,3][7,2], [4,3,5][6,7], [1,2,3][7,5], 
  [6,7,2,3](5), [4,2][6,7,5], [4,5][6,1](3), [2,5,3][6,1], [6,1,5][7,3], 
  [4,3][6,2][7,5], [2,3][6,7,5], [4,5][6,7], [5,3,2][6,7], [4,3][5,2][6,7], 
  [1,3][6,2][7,5], [6,4][7,5,3], [6,4,5][7,3], [6,5,2][7,3], [6,4,2][7,3], 
  [3,5][6,4], [6,4,3](1), [2,3][6,4](1), [7,3](1)(4), [5,2][6,1](3), 
  [4,2][5,3][6,1], [4,3][5,2](6), [4,2,3](6), [4,6][7,3](2), [3,5][6,7], 
  [6,7,3](5), [4,3][6,7,5], [4,5][6,2](1), [6,2,5](1), [4,2][7,5](1), 
  [4,5][6,7](1), [4,1][5,3][6,2], [4,3][6,2,1], [4,2,3][7,1], [5,7,3](6), 
  [4,3](6)(7), [4,5](7), [2,3][6,7,1], [5,3,1][6,7], [4,3][5,1][6,7], 
  [4,3][6,5][7,1], [4,7][6,3,1], [2,7][5,1][6,3], [6,3](1,7), [6,5,1](7), 
  [4,7,1][6,5], [2,1,3][6,5], [4,5][7,1,3], [5,2,3][6,4], [4,2][6,1](3), 
  [5,3][6,1](2), [6,1,2][7,3], [5,1,2][6,3], [4,1,2][6,3], [4,3](1,2), 
  [4,1][6,2][7,3], [4,3][6,1,2], [6,4,3][7,2], [6,4,2](1), [1,5,3][6,7], 
  [1,3][6,2](5), [1,3][4,5][6,2], [1,3][4,2,5], [6,3,5][7,2], [4,2][6,3][7,5],
  [1,3][4,5][7,2], [4,5,2][7,3], [4,5,2][6,7], [4,2,5][6,7,3], [4,7,5](2), 
  [2,3][6,1][7,5], [6,1](3,5), [4,3][6,1](5), [4,5][6,7,3], [1,2][4,3][6,7], 
  [1,2,3][6,7], [1,2][4,7,3], [4,2][6,7], [6,7](2), [4,7,2], [6,2](3)(5), 
  [4,5,3][6,2], [6,4,3](5), [2,3][6,4,5], [2,5][7,3](4), [4,2][6,5,3], 
  [4,3][6,5](2), [4,5][7,2,3], [6,4,5], [7,5](4), [5,1][6,4][7,3], 
  [6,4,3][7,1], [6,1,3](2), [4,1,3][7,2], [1,2][4,3](6), [4,5,3][6,7], 
  [2,5][4,3][6,7], [2,3][4,7,5], [6,2][7,5,1], [4,5][6,2][7,1], [4,1,3][6,2], 
  [4,7][5,3](6), [2,7][4,3](6), [2,3][4,6](7), [4,1][6,7,3], [2,3][6,7](1), 
  [4,7,3](1), [2,1][6,3](7), [5,1][6,3,7], [4,1][5,7][6,3], [4,1][6,5,7], 
  [2,1][4,7][6,5], [2,7,1][4,5], [6,5,3][7,1], [4,1][6,5][7,3], 
  [6,4][7,3](2), [6,1][7,2,3], [5,3,2][6,1], [4,3][5,2][6,1], [4,1][6,3,2], 
  [5,2,1][6,3], [6,3][7,2](1), [2,3][6,7](5), [6,2,5,3], [1,5][6,2][7,3], 
  [1,5,2][6,3], [1,5][4,2][6,3], [1,5][4,3](2), [4,2][6,5][7,3], 
  [1,5][4,3][6,2], [1,2][4,5][6,7], [4,5][6,1][7,3], [4,3][6,1,5], 
  [2,3][6,1,5], [4,1,5][7,3], [5,2][6,7,3], [4,3][6,7,2], [1,3][6,2,5], 
  [1,3][4,2][7,5], [1,5][6,4,3], [1,3][4,2][6,5], [5,3][6,4,1], [2,1][6,4,3], 
  [2,3][7,1](4), [5,3][6,1][7,2], [4,2][6,1][7,3], [1,3][4,5][6,7], 
  [4,1][6,2](5), [4,5][6,2,1], [4,2,5][7,1], [1,3][4,7](6), [5,1][6,7,3], 
  [4,3][6,7,1], [4,1,7][6,3], [2,1,7][6,3], [4,3](1,7), [4,1,7][6,5], 
  [4,3][6,5,1], [2,3][4,1][6,5], [2,1][4,5][7,3], [6,1,2,3], [4,1,2][7,3], 
  [6,3][7,1](2), [5,2][6,3,1], [4,2][5,1][6,3], [2,5][6,7,3], [6,2,3][7,5], 
  [6,2](3,5), [4,3][6,2](5), [4,2][6,3,5], [6,3](2)(5), [1,2][6,3][7,5], 
  [6,1][7,3](5), [4,3][6,1][7,5], [4,2][5,3][6,7], [4,3][6,7](2), [4,7,2,3], 
  [6,2][7,5,3], [4,5][6,2][7,3], [6,4,1,3], [4,2,3][6,1], [4,1][7,3](2), 
  [4,1,5][6,2], [4,1][5,3][6,7], [2,1][4,3][6,7], [2,3][4,7,1], [5,7,1][6,3], 
  [4,1][6,3](7), [4,3][6,5](1), [5,2][6,1][7,3], [4,2][6,3](1), [6,3](1)(2), 
  [4,3][7,2](1), [1,5][6,2,3], [1,5][4,2][7,3], [6,3][7,2,5], [6,3,2](5), 
  [4,5,2][6,3], [4,5,3][6,1], [2,5][4,3][6,1], [2,3][4,1][7,5], 
  [1,3][4,2][6,7], [4,5][6,2,3], [4,2,5][7,3], [4,7][5,1][6,3], 
  [2,7][4,1][6,3], [2,1][4,3](7), [4,3][6,1](2), [4,1][7,2,3], 
  [5,1][6,3][7,2], [4,2][6,3][7,1], [6,2][7,3](5), [1,2][4,5][6,3], 
  [1,2,5][6,3], [1,2][4,3][7,5], [4,1][5,2][6,3], [4,2,1][6,3], 
  [4,3][7,1](2), [4,3][6,2,5], [4,2,3][7,5], [6,3][7,5,2], [4,5][6,3][7,2], 
  [4,2][6,3](5), [4,5][6,3](2), [4,3][7,2,5] ]
gap> DClasses(S);
[ <Green's D-class: [4,5,7](1,3)(6)>, <Green's D-class: [6,4,7,1,2,5](3)>, 
  <Green's D-class: [4,7,2,5,1,3](6)>, <Green's D-class: [1,3][2,7,5][4,6]>, 
  <Green's D-class: [4,7](1)(3)(6)>, <Green's D-class: [2,7,3,1][6,5]>, 
  <Green's D-class: [2,7][5,3](1)(6)>, <Green's D-class: [4,6](1)(7)>, 
  <Green's D-class: [5,1,3,2][6,4]>, <Green's D-class: [4,1,5][6,7,2](3)>, 
  <Green's D-class: [6,4,1,3][7,5,2]>, <Green's D-class: [2,1,3](4)>, 
  <Green's D-class: [4,1][5,2](3)(6)>, <Green's D-class: [4,2,1,5][6,7,3]>, 
  <Green's D-class: [4,2,1][7,5,3](6)>, <Green's D-class: [4,6][7,1](2)>, 
  <Green's D-class: <identity partial perm on [ 3, 5 ]>>, 
  <Green's D-class: [1,7,3][4,5](6)>, <Green's D-class: [4,5,3](7)>, 
  <Green's D-class: [2,5]>, <Green's D-class: (1,3)(6)>, 
  <Green's D-class: [6,7,1](3)>, <Green's D-class: [5,1,3](6)>, 
  <Green's D-class: [1,3][4,6]>, <Green's D-class: [6,5,3](1)>, 
  <Green's D-class: [4,3,1,7]>, <Green's D-class: [4,3][6,5](1)(7)>, 
  <Green's D-class: [2,3][4,5](1)>, <Green's D-class: [4,3,1](6)>, 
  <Green's D-class: [2,3](1,7)>, <Green's D-class: [2,3][5,1](6)(7)>, 
  <Green's D-class: [4,6][7,3]>, <Green's D-class: [4,7,1](6)>, 
  <Green's D-class: [4,5,3]>, <Green's D-class: [6,4,1,2](3)>, 
  <Green's D-class: [7,3,2,1]>, <Green's D-class: [5,3][6,4](1,2)>, 
  <Green's D-class: [7,1,2](4)>, <Green's D-class: [1,3,5,2][6,7]>, 
  <Green's D-class: [4,2][6,1][7,5](3)>, <Green's D-class: [1,3][4,2][6,7](5)>
    , <Green's D-class: [1,3][4,7](2)>, <Green's D-class: [6,4,2](3)(5)>, 
  <Green's D-class: [4,5][6,1][7,3](2)>, <Green's D-class: [6,4,5,3](2)>, 
  <Green's D-class: [7,2,5](4)>, <Green's D-class: [6,4][7,3](1)>, 
  <Green's D-class: [7,5,3]>, <Green's D-class: <empty partial perm>>, 
  <Green's D-class: [1,3][4,2](6)>, <Green's D-class: [6,1](2)(3)>, 
  <Green's D-class: [1,3](2)(6)>, <Green's D-class: [1,3][4,6][7,2]>, 
  <Green's D-class: [6,7](3,5)>, <Green's D-class: [4,3][6,2][7,5](1)>, 
  <Green's D-class: [4,3][6,7,1](5)>, <Green's D-class: [1,3][4,5]>, 
  <Green's D-class: [4,3](5)(6)>, <Green's D-class: [4,5][6,2,3](1)>, 
  <Green's D-class: [2,3][4,5][7,1](6)>, <Green's D-class: [2,5][4,6][7,3]>, 
  <Green's D-class: [4,1,2](6)>, <Green's D-class: [4,3][7,5]>, 
  <Green's D-class: [5,3,7](6)>, <Green's D-class: [4,3][6,5](7)>, 
  <Green's D-class: [4,3][5,7](6)>, <Green's D-class: [2,3][4,7][6,5]>, 
  <Green's D-class: [2,5][7,3]>, <Green's D-class: [5,1,3][6,7]>, 
  <Green's D-class: [4,5](3)>, <Green's D-class: [4,1,3][6,7]>, 
  <Green's D-class: [2,1,3][4,7]>, <Green's D-class: [2,1][5,3](6)>, 
  <Green's D-class: [4,3,1][6,5]>, <Green's D-class: [2,3][6,5,1]>, 
  <Green's D-class: [6,3,1](7)>, <Green's D-class: [3,1][6,5,7]>, 
  <Green's D-class: [4,7,1][6,3]>, <Green's D-class: [4,7][6,5,1]>, 
  <Green's D-class: [1,7][4,5]>, <Green's D-class: [6,5][7,1,3]>, 
  <Green's D-class: [4,1,3](7)>, <Green's D-class: [4,1][5,7,3]>, 
  <Green's D-class: [4,1][5,7](6)>, <Green's D-class: [2,1][4,7,3](6)>, 
  <Green's D-class: [2,7,1][4,6]>, <Green's D-class: [4,3](6)>, 
  <Green's D-class: [4,5]>, <Green's D-class: [4,3][6,5]>, 
  <Green's D-class: [1,3,2][6,4]>, <Green's D-class: [6,1][7,2](3)>, 
  <Green's D-class: [1,3][5,2][6,4]>, <Green's D-class: [1,3](4)>, 
  <Green's D-class: [4,3,2](1)>, <Green's D-class: [4,3][7,1,2]>, 
  <Green's D-class: [6,4,3,2]>, <Green's D-class: [7,2,3](1)>, 
  <Green's D-class: [5,2,3][6,4][7,1]>, <Green's D-class: [7,3](4)>, 
  <Green's D-class: [6,4,1][7,2]>, <Green's D-class: [1,5][4,2][6,7](3)>, 
  <Green's D-class: [7,3,5](2)>, <Green's D-class: [1,5,3][6,7](2)>, 
  <Green's D-class: [1,5][4,7,2]>, <Green's D-class: [6,1,3](5)>, 
  <Green's D-class: [4,5][6,2](3)>, <Green's D-class: [4,5][6,1,3]>, 
  <Green's D-class: [2,5][4,1,3]>, <Green's D-class: [4,5][6,7](3)>, 
  <Green's D-class: [6,2,5][7,3]>, <Green's D-class: [2,5,3][6,7]>, 
  <Green's D-class: [4,7,5]>, <Green's D-class: [1,2][6,7,3]>, 
  <Green's D-class: [1,3][6,4,5]>, <Green's D-class: [6,2,5](3)>, 
  <Green's D-class: [1,3][2,5][6,4]>, <Green's D-class: [1,3][7,5](4)>, 
  <Green's D-class: [1,2][4,3][6,5]>, <Green's D-class: [4,3][6,1][7,2]>, 
  <Green's D-class: [6,4,3]>, <Green's D-class: [1,2,3][6,5]>, 
  <Green's D-class: [6,4][7,2,3]>, <Green's D-class: [1,5][6,4,2]>, 
  <Green's D-class: [5,3,1][6,4]>, <Green's D-class: [5,1][6,4,3]>, 
  <Green's D-class: [4,6]>, <Green's D-class: [6,1,3][7,2]>, 
  <Green's D-class: [5,3][7,2](6)>, <Green's D-class: [4,2][7,3](6)>, 
  <Green's D-class: [1,5][4,3][6,7]>, <Green's D-class: [1,5][2,3][6,7]>, 
  <Green's D-class: [1,5][4,7,3]>, <Green's D-class: [3,1][6,2](5)>, 
  <Green's D-class: [4,5][6,3][7,1]>, <Green's D-class: [4,5,1][6,2]>, 
  <Green's D-class: [4,5,1][6,7]>, <Green's D-class: [2,5][4,1][6,3]>, 
  <Green's D-class: [2,5][4,1][6,7]>, <Green's D-class: [2,1][4,7,5]>, 
  <Green's D-class: [5,1][6,2][7,3]>, <Green's D-class: [4,1,3][6,5]>, 
  <Green's D-class: [4,1][7,3](6)>, <Green's D-class: [1,5][4,3](6)>, 
  <Green's D-class: [1,7][4,3](6)>, <Green's D-class: [1,7][2,3](6)>, 
  <Green's D-class: [1,7,3][4,6]>, <Green's D-class: [4,7][6,5]>, 
  <Green's D-class: [4,1][6,7](3)>, <Green's D-class: [2,1][5,3][6,7]>, 
  <Green's D-class: [6,7,3](1)>, <Green's D-class: [2,3][7,1](6)>, 
  <Green's D-class: [2,1][6,5][7,3]>, <Green's D-class: [5,7][6,3](1)>, 
  <Green's D-class: [4,7][6,3](1)>, <Green's D-class: [2,7][4,3](1)>, 
  <Green's D-class: [4,7][6,5](1)>, <Green's D-class: [2,7][6,5](1)>, 
  <Green's D-class: [4,5](1)(7)>, <Green's D-class: [6,5,1](3)>, 
  <Green's D-class: [4,1][6,5,3]>, <Green's D-class: [4,7,3][6,1]>, 
  <Green's D-class: [2,7][4,3][6,1]>, <Green's D-class: [4,3][6,7](1)>, 
  <Green's D-class: [4,1,7](6)>, <Green's D-class: [1,2][5,3][6,4]>, 
  <Green's D-class: [5,2][6,1,3]>, <Green's D-class: [4,2][6,1,3]>, 
  <Green's D-class: [4,1,3](2)>, <Green's D-class: [6,4,2](3)>, 
  <Green's D-class: [5,3][6,4](2)>, <Green's D-class: [6,3,2][7,1]>, 
  <Green's D-class: [4,1][6,3][7,2]>, <Green's D-class: [4,2][7,1,3]>, 
  <Green's D-class: [4,2][5,1][7,3]>, <Green's D-class: [5,1][6,4,2]>, 
  <Green's D-class: [6,4,1][7,3](2)>, <Green's D-class: [7,2,1](4)>, 
  <Green's D-class: [1,3,5][6,7]>, <Green's D-class: [6,2][7,5](3)>, 
  <Green's D-class: [1,3][6,7](5)>, <Green's D-class: [1,2][4,3,5]>, 
  <Green's D-class: [1,5][4,3][7,2]>, <Green's D-class: [4,3,5][6,7]>, 
  <Green's D-class: [1,2,3][7,5]>, <Green's D-class: [6,7,2,3](5)>, 
  <Green's D-class: [4,2][6,7,5]>, <Green's D-class: [4,5][6,1](3)>, 
  <Green's D-class: [2,5,3][6,1]>, <Green's D-class: [6,1,5][7,3]>, 
  <Green's D-class: [4,3][6,2][7,5]>, <Green's D-class: [2,3][6,7,5]>, 
  <Green's D-class: [4,5][6,7]>, <Green's D-class: [5,3,2][6,7]>, 
  <Green's D-class: [4,3][5,2][6,7]>, <Green's D-class: [1,3][6,2][7,5]>, 
  <Green's D-class: [6,4][7,5,3]>, <Green's D-class: [6,4,5][7,3]>, 
  <Green's D-class: [6,5,2][7,3]>, <Green's D-class: [6,4,2][7,3]>, 
  <Green's D-class: [1,5](4)>, <Green's D-class: [6,4,3](1)>, 
  <Green's D-class: [2,3][6,4](1)>, <Green's D-class: [7,3](1)(4)>, 
  <Green's D-class: [5,2][6,1](3)>, <Green's D-class: [4,2][5,3][6,1]>, 
  <Green's D-class: [4,3][5,2](6)>, <Green's D-class: [4,2,3](6)>, 
  <Green's D-class: [4,6][7,3](2)>, <Green's D-class: [1,5][4,7]>, 
  <Green's D-class: [6,7,3](5)>, <Green's D-class: [4,3][6,7,5]>, 
  <Green's D-class: [4,5][6,2](1)>, <Green's D-class: [6,2,5](1)>, 
  <Green's D-class: [4,2][7,5](1)>, <Green's D-class: [4,5][6,7](1)>, 
  <Green's D-class: [4,1][5,3][6,2]>, <Green's D-class: [4,3][6,2,1]>, 
  <Green's D-class: [4,2,3][7,1]>, <Green's D-class: [5,7,3](6)>, 
  <Green's D-class: [4,3](6)(7)>, <Green's D-class: [4,5](7)>, 
  <Green's D-class: [2,3][6,7,1]>, <Green's D-class: [5,3,1][6,7]>, 
  <Green's D-class: [4,3][5,1][6,7]>, <Green's D-class: [4,3][6,5][7,1]>, 
  <Green's D-class: [4,7][6,3,1]>, <Green's D-class: [2,7][5,1][6,3]>, 
  <Green's D-class: [6,3](1,7)>, <Green's D-class: [6,5,1](7)>, 
  <Green's D-class: [4,7,1][6,5]>, <Green's D-class: [2,1,3][6,5]>, 
  <Green's D-class: [4,5][7,1,3]>, <Green's D-class: [5,2,3][6,4]>, 
  <Green's D-class: [4,2][6,1](3)>, <Green's D-class: [5,3][6,1](2)>, 
  <Green's D-class: [6,1,2][7,3]>, <Green's D-class: [5,1,2][6,3]>, 
  <Green's D-class: [4,1,2][6,3]>, <Green's D-class: [4,3](1,2)>, 
  <Green's D-class: [4,1][6,2][7,3]>, <Green's D-class: [4,3][6,1,2]>, 
  <Green's D-class: [6,4,3][7,2]>, <Green's D-class: [6,4,2](1)>, 
  <Green's D-class: [1,5,3][6,7]>, <Green's D-class: [1,3][6,2](5)>, 
  <Green's D-class: [1,3][4,5][6,2]>, <Green's D-class: [1,3][4,2,5]>, 
  <Green's D-class: [6,3,5][7,2]>, <Green's D-class: [4,2][6,3][7,5]>, 
  <Green's D-class: [1,3][4,5][7,2]>, <Green's D-class: [4,5,2][7,3]>, 
  <Green's D-class: [4,5,2][6,7]>, <Green's D-class: [4,2,5][6,7,3]>, 
  <Green's D-class: [4,7,5](2)>, <Green's D-class: [2,3][6,1][7,5]>, 
  <Green's D-class: [6,1](3,5)>, <Green's D-class: [4,3][6,1](5)>, 
  <Green's D-class: [4,5][6,7,3]>, <Green's D-class: [1,2][4,3][6,7]>, 
  <Green's D-class: [1,2,3][6,7]>, <Green's D-class: [1,2][4,7,3]>, 
  <Green's D-class: [4,2][6,7]>, <Green's D-class: [1,2][4,7]>, 
  <Green's D-class: [4,7,2]>, <Green's D-class: [6,2](3)(5)>, 
  <Green's D-class: [4,5,3][6,2]>, <Green's D-class: [6,4,3](5)>, 
  <Green's D-class: [2,3][6,4,5]>, <Green's D-class: [2,5][7,3](4)>, 
  <Green's D-class: [4,2][6,5,3]>, <Green's D-class: [4,3][6,5](2)>, 
  <Green's D-class: [4,5][7,2,3]>, <Green's D-class: [6,4,5]>, 
  <Green's D-class: [7,5](4)>, <Green's D-class: [5,1][6,4][7,3]>, 
  <Green's D-class: [6,4,3][7,1]>, <Green's D-class: [6,1,3](2)>, 
  <Green's D-class: [4,1,3][7,2]>, <Green's D-class: [1,2][4,3](6)>, 
  <Green's D-class: [4,5,3][6,7]>, <Green's D-class: [2,5][4,3][6,7]>, 
  <Green's D-class: [2,3][4,7,5]>, <Green's D-class: [6,2][7,5,1]>, 
  <Green's D-class: [4,5][6,2][7,1]>, <Green's D-class: [4,1,3][6,2]>, 
  <Green's D-class: [4,7][5,3](6)>, <Green's D-class: [2,7][4,3](6)>, 
  <Green's D-class: [2,3][4,6](7)>, <Green's D-class: [4,1][6,7,3]>, 
  <Green's D-class: [2,3][6,7](1)>, <Green's D-class: [4,7,3](1)>, 
  <Green's D-class: [2,1][6,3](7)>, <Green's D-class: [5,1][6,3,7]>, 
  <Green's D-class: [4,1][5,7][6,3]>, <Green's D-class: [4,1][6,5,7]>, 
  <Green's D-class: [2,1][4,7][6,5]>, <Green's D-class: [2,7,1][4,5]>, 
  <Green's D-class: [6,5,3][7,1]>, <Green's D-class: [4,1][6,5][7,3]>, 
  <Green's D-class: [6,4][7,3](2)>, <Green's D-class: [6,1][7,2,3]>, 
  <Green's D-class: [5,3,2][6,1]>, <Green's D-class: [4,3][5,2][6,1]>, 
  <Green's D-class: [4,1][6,3,2]>, <Green's D-class: [5,2,1][6,3]>, 
  <Green's D-class: [6,3][7,2](1)>, <Green's D-class: [2,3][6,7](5)>, 
  <Green's D-class: [6,2,5,3]>, <Green's D-class: [1,5][6,2][7,3]>, 
  <Green's D-class: [1,5,2][6,3]>, <Green's D-class: [1,5][4,2][6,3]>, 
  <Green's D-class: [1,5][4,3](2)>, <Green's D-class: [4,2][6,5][7,3]>, 
  <Green's D-class: [1,5][4,3][6,2]>, <Green's D-class: [1,2][4,5][6,7]>, 
  <Green's D-class: [4,5][6,1][7,3]>, <Green's D-class: [4,3][6,1,5]>, 
  <Green's D-class: [2,3][6,1,5]>, <Green's D-class: [4,1,5][7,3]>, 
  <Green's D-class: [5,2][6,7,3]>, <Green's D-class: [4,3][6,7,2]>, 
  <Green's D-class: [1,3][6,2,5]>, <Green's D-class: [1,3][4,2][7,5]>, 
  <Green's D-class: [1,5][6,4,3]>, <Green's D-class: [1,3][4,2][6,5]>, 
  <Green's D-class: [5,3][6,4,1]>, <Green's D-class: [2,1][6,4,3]>, 
  <Green's D-class: [2,3][7,1](4)>, <Green's D-class: [5,3][6,1][7,2]>, 
  <Green's D-class: [4,2][6,1][7,3]>, <Green's D-class: [1,3][4,5][6,7]>, 
  <Green's D-class: [4,1][6,2](5)>, <Green's D-class: [4,5][6,2,1]>, 
  <Green's D-class: [4,2,5][7,1]>, <Green's D-class: [1,3][4,7](6)>, 
  <Green's D-class: [5,1][6,7,3]>, <Green's D-class: [4,3][6,7,1]>, 
  <Green's D-class: [4,1,7][6,3]>, <Green's D-class: [2,1,7][6,3]>, 
  <Green's D-class: [4,3](1,7)>, <Green's D-class: [4,1,7][6,5]>, 
  <Green's D-class: [4,3][6,5,1]>, <Green's D-class: [2,3][4,1][6,5]>, 
  <Green's D-class: [2,1][4,5][7,3]>, <Green's D-class: [6,1,2,3]>, 
  <Green's D-class: [4,1,2][7,3]>, <Green's D-class: [6,3][7,1](2)>, 
  <Green's D-class: [5,2][6,3,1]>, <Green's D-class: [4,2][5,1][6,3]>, 
  <Green's D-class: [2,5][6,7,3]>, <Green's D-class: [6,2,3][7,5]>, 
  <Green's D-class: [6,2](3,5)>, <Green's D-class: [4,3][6,2](5)>, 
  <Green's D-class: [4,2][6,3,5]>, <Green's D-class: [6,3](2)(5)>, 
  <Green's D-class: [1,2][6,3][7,5]>, <Green's D-class: [6,1][7,3](5)>, 
  <Green's D-class: [4,3][6,1][7,5]>, <Green's D-class: [4,2][5,3][6,7]>, 
  <Green's D-class: [4,3][6,7](2)>, <Green's D-class: [4,7,2,3]>, 
  <Green's D-class: [6,2][7,5,3]>, <Green's D-class: [4,5][6,2][7,3]>, 
  <Green's D-class: [6,4,1,3]>, <Green's D-class: [4,2,3][6,1]>, 
  <Green's D-class: [4,1][7,3](2)>, <Green's D-class: [4,1,5][6,2]>, 
  <Green's D-class: [4,1][5,3][6,7]>, <Green's D-class: [2,1][4,3][6,7]>, 
  <Green's D-class: [2,3][4,7,1]>, <Green's D-class: [5,7,1][6,3]>, 
  <Green's D-class: [4,1][6,3](7)>, <Green's D-class: [4,3][6,5](1)>, 
  <Green's D-class: [5,2][6,1][7,3]>, <Green's D-class: [4,2][6,3](1)>, 
  <Green's D-class: [6,3](1)(2)>, <Green's D-class: [4,3][7,2](1)>, 
  <Green's D-class: [1,5][6,2,3]>, <Green's D-class: [1,5][4,2][7,3]>, 
  <Green's D-class: [6,3][7,2,5]>, <Green's D-class: [6,3,2](5)>, 
  <Green's D-class: [4,5,2][6,3]>, <Green's D-class: [4,5,3][6,1]>, 
  <Green's D-class: [2,5][4,3][6,1]>, <Green's D-class: [2,3][4,1][7,5]>, 
  <Green's D-class: [1,3][4,2][6,7]>, <Green's D-class: [4,5][6,2,3]>, 
  <Green's D-class: [4,2,5][7,3]>, <Green's D-class: [4,7][5,1][6,3]>, 
  <Green's D-class: [2,7][4,1][6,3]>, <Green's D-class: [2,1][4,3](7)>, 
  <Green's D-class: [4,3][6,1](2)>, <Green's D-class: [4,1][7,2,3]>, 
  <Green's D-class: [5,1][6,3][7,2]>, <Green's D-class: [4,2][6,3][7,1]>, 
  <Green's D-class: [6,2][7,3](5)>, <Green's D-class: [1,2][4,5][6,3]>, 
  <Green's D-class: [1,2,5][6,3]>, <Green's D-class: [1,2][4,3][7,5]>, 
  <Green's D-class: [4,1][5,2][6,3]>, <Green's D-class: [4,2,1][6,3]>, 
  <Green's D-class: [4,3][7,1](2)>, <Green's D-class: [4,3][6,2,5]>, 
  <Green's D-class: [4,2,3][7,5]>, <Green's D-class: [6,3][7,5,2]>, 
  <Green's D-class: [4,5][6,3][7,2]>, <Green's D-class: [4,2][6,3](5)>, 
  <Green's D-class: [4,5][6,3](2)>, <Green's D-class: [4,3][7,2,5]> ]

# greens-acting: L-classes/reps, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(generic := false));;
gap> GreensLClasses(S);
[ <Green's L-class: Transformation( [ 2, 2, 1, 2, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 6, 6, 4, 6, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 3, 1, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 2, 5, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 3, 2, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 3, 4, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 3, 4, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 6, 2, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 4, 2, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 2, 3, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 4, 5, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 6, 6, 4, 6, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 3, 4, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 5, 1, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 3, 1, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 4, 3, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 3, 2, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 2, 5, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 3, 5, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 1, 2, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 2, 6, 6, 5, 1, 4 ] )>, 
  <Green's L-class: Transformation( [ 5, 3, 3, 4, 2 ] )>, 
  <Green's L-class: Transformation( [ 1, 3, 3 ] )>, 
  <Green's L-class: Transformation( [ 4, 2, 3, 1, 4, 2 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 2, 6, 6, 3 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 1, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 5, 6, 6, 2 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 3, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 3, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 1, 4, 4, 5 ] )>, 
  <Green's L-class: Transformation( [ 6, 6, 2, 5, 5, 1 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 3, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 1, 4, 4, 5 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 1, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 3, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 5, 4, 4, 2 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 2, 2, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 6, 6, 6, 6, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 5, 5, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 4, 4, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 3, 3, 3, 3 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 1, 1, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 4, 6, 6, 6, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 6, 5, 5, 5, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 6, 3, 3, 3, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 2, 4, 4, 4, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 1, 2, 2, 2, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 5, 4, 4, 4, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 6, 1, 1, 1, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 3, 1, 1, 1, 3, 3 ] )>, 
  <Green's L-class: Transformation( [ 6, 2, 2, 2, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 1, 4, 4, 4, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 3, 2, 2, 2, 3, 3 ] )>, 
  <Green's L-class: Transformation( [ 2, 5, 5, 5, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 5, 3, 3, 3, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 1, 5, 5, 5, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 4, 3, 3, 3, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 1, 3, 3, 4 ] )> ]
gap> LClassReps(S);
[ Transformation( [ 2, 2, 1, 2, 4, 4 ] ), 
  Transformation( [ 6, 6, 4, 6, 5, 5 ] ), 
  Transformation( [ 1, 1, 3, 1, 6, 6 ] ), 
  Transformation( [ 5, 5, 2, 5, 6, 6 ] ), 
  Transformation( [ 2, 2, 3, 2, 5, 5 ] ), 
  Transformation( [ 4, 4, 3, 4, 5, 5 ] ), 
  Transformation( [ 4, 4, 3, 4, 6, 6 ] ), 
  Transformation( [ 2, 2, 6, 2, 4, 4 ] ), 
  Transformation( [ 2, 2, 4, 2, 5, 5 ] ), 
  Transformation( [ 3, 3, 2, 3, 1, 1 ] ), 
  Transformation( [ 5, 5, 4, 5, 1, 1 ] ), 
  Transformation( [ 6, 6, 4, 6, 1, 1 ] ), 
  Transformation( [ 4, 4, 3, 4, 2, 2 ] ), 
  Transformation( [ 1, 1, 5, 1, 6, 6 ] ), 
  Transformation( [ 1, 1, 3, 1, 5, 5 ] ), 
  Transformation( [ 3, 3, 4, 3, 1, 1 ] ), 
  Transformation( [ 2, 2, 3, 2, 6, 6 ] ), 
  Transformation( [ 5, 5, 2, 5, 1, 1 ] ), 
  Transformation( [ 5, 5, 3, 5, 6, 6 ] ), 
  Transformation( [ 2, 2, 1, 2, 6, 6 ] ), 
  Transformation( [ 2, 6, 6, 5, 1, 4 ] ), Transformation( [ 5, 3, 3, 4, 2 ] ),
  Transformation( [ 1, 3, 3 ] ), Transformation( [ 4, 2, 3, 1, 4, 2 ] ), 
  Transformation( [ 4, 4, 2, 6, 6, 3 ] ), Transformation( [ 5, 5, 1, 4, 4 ] ),
  Transformation( [ 4, 4, 5, 6, 6, 2 ] ), Transformation( [ 5, 5, 3, 4, 4 ] ),
  Transformation( [ 1, 1, 3, 4, 4 ] ), Transformation( [ 2, 2, 1, 4, 4, 5 ] ),
  Transformation( [ 6, 6, 2, 5, 5, 1 ] ), Transformation( [ 2, 2, 3, 5, 5 ] ),
  Transformation( [ 3, 3, 1, 4, 4, 5 ] ), Transformation( [ 2, 2, 1, 4, 4 ] ),
  Transformation( [ 1, 1, 3, 5, 5 ] ), Transformation( [ 3, 3, 5, 4, 4, 2 ] ),
  Transformation( [ 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 6, 6, 6, 6, 6, 6 ] ), 
  Transformation( [ 5, 5, 5, 5, 5, 5 ] ), 
  Transformation( [ 4, 4, 4, 4, 4, 4 ] ), 
  Transformation( [ 3, 3, 3, 3, 3, 3 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 1 ] ), 
  Transformation( [ 4, 6, 6, 6, 4, 4 ] ), 
  Transformation( [ 6, 5, 5, 5, 6, 6 ] ), 
  Transformation( [ 6, 3, 3, 3, 6, 6 ] ), 
  Transformation( [ 2, 4, 4, 4, 2, 2 ] ), 
  Transformation( [ 1, 2, 2, 2, 1, 1 ] ), 
  Transformation( [ 5, 4, 4, 4, 5, 5 ] ), 
  Transformation( [ 6, 1, 1, 1, 6, 6 ] ), 
  Transformation( [ 3, 1, 1, 1, 3, 3 ] ), 
  Transformation( [ 6, 2, 2, 2, 6, 6 ] ), 
  Transformation( [ 1, 4, 4, 4, 1, 1 ] ), 
  Transformation( [ 3, 2, 2, 2, 3, 3 ] ), 
  Transformation( [ 2, 5, 5, 5, 2, 2 ] ), 
  Transformation( [ 5, 3, 3, 3, 5, 5 ] ), 
  Transformation( [ 1, 5, 5, 5, 1, 1 ] ), 
  Transformation( [ 4, 3, 3, 3, 4, 4 ] ), 
  Transformation( [ 2, 2, 1, 3, 3, 4 ] ) ]

# greens-acting: R-classes/reps, 1/1
gap> S := OrderEndomorphisms(5);;
gap> S := Semigroup(S, rec(generic := false));
<transformation monoid of degree 5 with 5 generators>
gap> RClasses(S);
[ <Green's R-class: IdentityTransformation>, 
  <Green's R-class: Transformation( [ 1, 1, 2, 3, 4 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 2, 3, 4 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 3, 3, 4 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 3, 4, 4 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 1, 2, 3 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 2, 2, 3 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 2, 3, 3 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 2, 3, 3 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 2, 2, 3 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 3, 3, 3 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 1, 1, 2 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 1, 2, 2 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 2, 2, 2 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 2, 2, 2 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 1, 1, 1 ] )> ]
gap> RClassReps(S);
[ IdentityTransformation, Transformation( [ 1, 1, 2, 3, 4 ] ), 
  Transformation( [ 1, 2, 2, 3, 4 ] ), Transformation( [ 1, 2, 3, 3, 4 ] ), 
  Transformation( [ 1, 2, 3, 4, 4 ] ), Transformation( [ 1, 1, 1, 2, 3 ] ), 
  Transformation( [ 1, 1, 2, 2, 3 ] ), Transformation( [ 1, 1, 2, 3, 3 ] ), 
  Transformation( [ 1, 2, 2, 3, 3 ] ), Transformation( [ 1, 2, 2, 2, 3 ] ), 
  Transformation( [ 1, 2, 3, 3, 3 ] ), Transformation( [ 1, 1, 1, 1, 2 ] ), 
  Transformation( [ 1, 1, 1, 2, 2 ] ), Transformation( [ 1, 1, 2, 2, 2 ] ), 
  Transformation( [ 1, 2, 2, 2, 2 ] ), Transformation( [ 1, 1, 1, 1, 1 ] ) ]

# greens-acting: H-classes/reps, 1/3
gap> S := Monoid(
> [Transformation([2, 2, 2, 2, 2, 2, 2, 2, 2, 4]),
>   Transformation([2, 2, 2, 2, 2, 2, 2, 4, 2, 4]),
>   Transformation([2, 2, 2, 2, 2, 2, 2, 4, 4, 2]),
>   Transformation([2, 2, 2, 2, 2, 2, 2, 4, 4, 4]),
>   Transformation([2, 2, 2, 2, 2, 2, 4, 4, 2, 2]),
>   Transformation([2, 2, 2, 2, 2, 2, 4, 4, 4, 2]),
>   Transformation([2, 2, 2, 2, 2, 4, 2, 2, 2, 4]),
>   Transformation([2, 2, 2, 2, 2, 4, 2, 2, 4, 4]),
>   Transformation([2, 2, 2, 2, 2, 4, 4, 2, 4, 2]),
>   Transformation([2, 2, 2, 4, 2, 2, 2, 4, 2, 2]),
>   Transformation([2, 2, 2, 4, 2, 2, 7, 4, 2, 4]),
>   Transformation([2, 2, 3, 4, 2, 4, 7, 2, 9, 4]),
>   Transformation([2, 2, 3, 4, 2, 6, 2, 2, 9, 2]),
>   Transformation([2, 2, 3, 4, 2, 6, 7, 2, 2, 4]),
>   Transformation([2, 2, 3, 4, 2, 6, 7, 2, 9, 4]),
>   Transformation([2, 2, 4, 2, 2, 2, 2, 2, 2, 4]),
>   Transformation([2, 2, 4, 2, 2, 2, 2, 4, 2, 2]),
>   Transformation([2, 2, 4, 2, 2, 2, 2, 4, 2, 4]),
>   Transformation([2, 2, 4, 2, 2, 2, 4, 4, 2, 2]),
>   Transformation([2, 2, 9, 4, 2, 4, 7, 2, 2, 4]),
>   Transformation([3, 2, 2, 2, 2, 2, 2, 9, 4, 2]),
>   Transformation([3, 2, 2, 2, 2, 2, 2, 9, 4, 4]),
>   Transformation([3, 2, 2, 2, 2, 2, 4, 9, 4, 2]),
>   Transformation([4, 2, 2, 2, 2, 2, 2, 3, 2, 2]),
>   Transformation([4, 2, 2, 2, 2, 2, 2, 3, 2, 4]),
>   Transformation([4, 2, 2, 2, 2, 2, 4, 3, 2, 2]),
>   Transformation([4, 2, 4, 2, 2, 2, 2, 3, 2, 2]),
>   Transformation([4, 2, 4, 2, 2, 2, 2, 3, 2, 4]),
>   Transformation([4, 2, 4, 2, 2, 2, 4, 3, 2, 2]),
>   Transformation([5, 5, 5, 5, 5, 5, 5, 5, 5, 5])],
>   rec(generic := false));;
gap> HClassReps(S);
[ IdentityTransformation, Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 4, 2, 4 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 4, 4, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 4, 4, 4 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 4, 4, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 4, 4, 4, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 4, 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 4, 2, 2, 4, 4 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 4, 4, 2, 4, 2 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 2, 2, 4, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 2, 7, 4, 2, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 4, 7, 2, 9, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 6, 2, 2, 9, 2 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 6, 7, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 6, 7, 2, 9, 4 ] ), 
  Transformation( [ 2, 2, 4, 2, 2, 2, 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 4, 2, 2, 2, 2, 4, 2, 2 ] ), 
  Transformation( [ 2, 2, 4, 2, 2, 2, 2, 4, 2, 4 ] ), 
  Transformation( [ 2, 2, 4, 2, 2, 2, 4, 4, 2, 2 ] ), 
  Transformation( [ 2, 2, 9, 4, 2, 4, 7, 2, 2, 4 ] ), 
  Transformation( [ 3, 2, 2, 2, 2, 2, 2, 9, 4, 2 ] ), 
  Transformation( [ 3, 2, 2, 2, 2, 2, 2, 9, 4, 4 ] ), 
  Transformation( [ 3, 2, 2, 2, 2, 2, 4, 9, 4, 2 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 2, 3, 2, 2 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 2, 3, 2, 4 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 4, 3, 2, 2 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 2, 3, 2, 2 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 2, 3, 2, 4 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 4, 3, 2, 2 ] ), 
  Transformation( [ 5, 5, 5, 5, 5, 5, 5, 5, 5, 5 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 4, 2 ] ), 
  Transformation( [ 2, 2, 4, 2, 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 4, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 4, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 4, 2, 4, 2 ] ), 
  Transformation( [ 2, 2, 4, 2, 2, 2, 4, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 4, 2, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 4, 2, 2, 4, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 4, 4, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 2, 2, 4, 2, 4 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 4, 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 2, 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 4, 4 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 2, 2, 2, 4 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 4, 2, 2, 2 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 2, 2, 2, 4 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 4, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 4, 7, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 2, 7, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 9, 2 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 4, 7, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 9, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 6, 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 6, 2, 2, 9, 4 ] ), 
  Transformation( [ 2, 2, 9, 4, 2, 4, 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 6, 2, 2, 2, 2 ] ), 
  Transformation( [ 3, 2, 2, 2, 2, 2, 2, 2, 4, 2 ] ), 
  Transformation( [ 3, 2, 2, 2, 2, 2, 2, 2, 4, 4 ] ), 
  Transformation( [ 3, 2, 2, 2, 2, 2, 4, 2, 4, 2 ] ), 
  Transformation( [ 2, 2, 9, 4, 2, 4, 2, 2, 2, 2 ] ), 
  Transformation( [ 9, 2, 2, 2, 2, 2, 2, 2, 4, 2 ] ), 
  Transformation( [ 9, 2, 2, 2, 2, 2, 2, 2, 4, 4 ] ), 
  Transformation( [ 9, 2, 2, 2, 2, 2, 4, 2, 4, 2 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 2, 9, 2, 2 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 2, 9, 2, 4 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 4, 9, 2, 2 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 2, 9, 2, 2 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 2, 9, 2, 4 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 4, 9, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 4, 2, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 2, 2 ] ) ]
gap> HClasses(S);
[ <Green's H-class: IdentityTransformation>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 2, 4, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 2, 4, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 2, 4, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 4, 4, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 4, 4, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 4, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 4, 2, 2, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 4, 4, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 2, 2, 4, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 2, 2, 4, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 4, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 2, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 2, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 4, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 2, 7, 4, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 4, 7, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 2, 7, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 4, 7, 2, 9, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 6, 2, 2, 9, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 6, 2, 2, 9, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 6, 7, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 6, 7, 2, 9, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 4, 2, 2, 2, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 4, 2, 2, 2, 2, 4, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 4, 2, 2, 2, 2, 4, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 4, 2, 2, 2, 4, 4, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 9, 4, 2, 4, 7, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 2, 2, 2, 2, 2, 9, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 2, 2, 2, 2, 2, 9, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 2, 2, 2, 2, 4, 9, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 2, 3, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 2, 3, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 4, 3, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 2, 3, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 2, 3, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 4, 3, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 5, 5, 5, 5, 5, 5, 5, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 4, 2, 2, 2, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 2, 4, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 4, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 4, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 4, 2, 2, 2, 4, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 4, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 4, 2, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 4, 4, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 4, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 4, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 9, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 9, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 4, 7, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 6, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 6, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 9, 4, 2, 4, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 9, 4, 2, 4, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 2, 2, 2, 2, 2, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 2, 2, 2, 2, 2, 2, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 2, 2, 2, 2, 4, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 9, 2, 2, 2, 2, 2, 2, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 9, 2, 2, 2, 2, 2, 2, 2, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 9, 2, 2, 2, 2, 2, 4, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 2, 9, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 2, 9, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 4, 9, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 2, 9, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 2, 9, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 4, 9, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 2, 2 ] )> ]
gap> D := DClass(S, S.1);;
gap> HClassReps(D);
[ Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 2, 4 ] ) ]
gap> HClasses(D);
[ <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 2, 4 ] )> ]
gap> L := LClass(S, S.1);;
gap> HClassReps(L);
[ Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 2, 4 ] ) ]
gap> HClasses(L);
[ <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 2, 4 ] )> ]

# greens-acting: H-classes/reps, 2/3
gap> S := Semigroup(FullTransformationMonoid(5), rec(generic := false));;
gap> x := Transformation([1, 1, 2, 3, 4]);;
gap> L := LClass(S, x);;
gap> GreensHClasses(L);
[ <Green's H-class: Transformation( [ 1, 1, 2, 3, 4 ] )>, 
  <Green's H-class: Transformation( [ 1, 2, 3, 4, 1 ] )>, 
  <Green's H-class: Transformation( [ 2, 3, 4, 1, 1 ] )>, 
  <Green's H-class: Transformation( [ 2, 1, 3, 4, 1 ] )>, 
  <Green's H-class: Transformation( [ 3, 4, 1, 1, 2 ] )>, 
  <Green's H-class: Transformation( [ 1, 3, 4, 1, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 1, 1, 2, 3 ] )>, 
  <Green's H-class: Transformation( [ 3, 4, 1, 2, 1 ] )>, 
  <Green's H-class: Transformation( [ 3, 1, 4, 1, 2 ] )>, 
  <Green's H-class: Transformation( [ 1, 4, 1, 2, 3 ] )> ]

# greens-acting: NrXClasses, 1/1
gap> S := Semigroup(SymmetricInverseMonoid(5));;
gap> NrRClasses(S);
32
gap> NrDClasses(S);
6
gap> NrLClasses(S);
32
gap> NrHClasses(S);
252

# greens-acting: GroupHClass, IsGroupHClass, IsomorphismPermGroup, 1/1
gap> S := AsTransformationSemigroup(FullBooleanMatMonoid(4));;
gap> S := Semigroup(S, rec(generic := false));;
gap> D := DClass(S, S.2);
<Green's D-class: Transformation( [ 1, 2, 1, 2, 1, 2, 3, 4, 1, 2, 5, 6, 9,
   10 ] )>
gap> IsRegularDClass(D);
false
gap> GroupHClass(D);
fail
gap> D := DClass(S, S.3);
<Green's D-class: Transformation( [ 1, 1, 1, 2, 1, 3, 1, 4, 1, 1, 5, 6, 9, 11,
  13 ] )>
gap> GroupHClass(D);
fail
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 1, 1, 1, 1, 1, 1, 1, 2, 1, 3, 5, 7, 9, 11,
  13 ] )>
gap> GroupHClass(D);
fail
gap> D := DClass(S, One(S));
<Green's D-class: IdentityTransformation>
gap> H := GroupHClass(D);
<Green's H-class: IdentityTransformation>
gap> IsGroupHClass(H);
true
gap> IsomorphismPermGroup(H);
MappingByFunction( <Green's H-class: IdentityTransformation>, Group([ (5,9)
(6,10)(7,11)(8,12), (2,9,5,3)(4,10,13,7)(6,11)
(8,12,14,15) ]), function( x ) ... end, function( x ) ... end )
gap> IsomorphismPermGroup(HClass(S, S.1));
Error, Semigroups: IsomorphismPermGroup: usage,
the H-class is not a group,

# greens-acting: PartialOrderOfDClasses, 1/2
gap> S := AsTransformationSemigroup(FullBooleanMatMonoid(3));;
gap> S := Semigroup(S, rec(generic := false));;
gap> PartialOrderOfDClasses(S);
[ [ 1, 2, 3, 4 ], [ 2, 3, 6, 7, 8, 9, 10 ], [ 3, 5, 8 ], [ 3, 4, 5 ], 
  [ 5, 11 ], [ 3, 6, 8, 10 ], [ 3, 7, 8, 10 ], [ 5, 8 ], [ 3, 8, 9, 10 ], 
  [ 8, 10 ], [ 11 ] ]

# greens-acting: PartialOrderOfDClasses, 2/2
gap> S := Semigroup([Transformation([2, 3, 6, 5, 4, 8, 10, 4, 1, 4]),
>  Transformation([10, 2, 5, 4, 10, 3, 1, 6, 9, 6])],
> rec(generic := false));;
gap> PartialOrderOfDClasses(S);
[ [ 3, 4, 5 ], [ 4, 5, 6 ], [ 7, 8, 11 ], [ 8, 9, 10, 12 ], 
  [ 5, 9, 11, 12, 19, 36, 58 ], [ 5, 6, 10 ], [ 13, 14, 20 ], 
  [ 14, 15, 16, 21 ], [ 15, 17, 18, 22 ], [ 9, 10, 16, 19, 23, 36, 58 ], 
  [ 7, 11, 17, 20, 21, 34, 56 ], [ 12, 18, 22, 23, 35, 57, 85 ], 
  [ 13, 24, 25, 67, 94, 99, 111, 154 ], [ 25, 26, 27, 37 ], 
  [ 15, 26, 28, 29, 48, 69, 101 ], [ 15, 16, 27, 30, 31, 38, 51 ], 
  [ 28, 31, 32, 39 ], [ 29, 33, 40 ], [ 30, 34, 35, 41 ], 
  [ 13, 20, 31, 37, 54, 82 ], [ 14, 15, 21, 32, 38, 55, 83 ], 
  [ 15, 22, 33, 39, 40, 84 ], [ 22, 23, 33, 41, 63, 76, 78, 89 ], 
  [ 24, 42, 43, 129 ], [ 25, 42, 43, 95, 123, 130, 143, 174 ], 
  [ 26, 42, 44, 45, 131 ], [ 26, 27, 43, 46, 59, 66, 94 ], 
  [ 28, 44, 47, 69, 96, 98, 126, 127, 132, 144, 175 ], 
  [ 29, 45, 48, 97, 133 ], [ 30, 46, 48, 49, 50, 98, 134 ], 
  [ 13, 31, 47, 52, 70, 94, 128, 135, 145 ], [ 15, 28, 30, 31, 32, 51, 60 ], 
  [ 33, 48, 53, 77, 78, 81, 109, 110 ], [ 49, 54, 55, 61 ], [ 33, 50, 62 ], 
  [ 51, 56, 57, 63 ], [ 25, 26, 37, 52, 59, 79, 112 ], 
  [ 15, 27, 30, 31, 32, 38, 51, 80, 113 ], [ 28, 31, 39, 53, 60, 81, 114 ], 
  [ 29, 33, 40, 115 ], [ 30, 33, 41, 61, 62, 107 ], 
  [ 42, 90, 147, 148, 157, 178 ], [ 42, 43, 129 ], [ 42, 44, 129 ], 
  [ 42, 45, 158 ], [ 42, 46, 64, 65, 131 ], [ 42, 47, 67, 68, 131 ], 
  [ 42, 48, 70, 103, 104, 127, 128, 137, 144, 145, 153, 156, 163, 164, 170, 
      175, 176, 183, 188, 193, 198 ], 
  [ 49, 64, 71, 72, 124, 153, 159, 170, 188 ], [ 48, 50, 65, 125, 160 ], 
  [ 48, 51, 66, 73, 74, 126, 161 ], [ 26, 52, 68, 75, 123, 155, 162, 171 ], 
  [ 31, 48, 53, 70, 76, 127, 163 ], 
  [ 13, 54, 71, 79, 111, 128, 156, 164, 189 ], [ 15, 72, 80, 86 ], 
  [ 73, 82, 83, 87 ], [ 74, 78, 84, 88 ], [ 7, 31, 85, 89 ], 
  [ 26, 43, 46, 59, 66, 75, 94, 132, 149, 167, 191 ], 
  [ 15, 28, 30, 31, 51, 60, 76, 108, 113 ], [ 49, 54, 61, 77, 82, 86, 109 ], 
  [ 33, 50, 62, 139 ], [ 51, 63, 78, 84, 87, 88, 110 ], 
  [ 42, 64, 90, 91, 129 ], [ 42, 65, 158 ], [ 42, 66, 92, 93, 131 ], 
  [ 42, 67, 95, 129 ], [ 42, 68, 158 ], [ 42, 69, 96, 97, 131 ], 
  [ 42, 70, 75, 94, 178 ], [ 71, 90, 99, 100, 119, 178 ], 
  [ 72, 91, 101, 102, 150, 173, 179, 187, 196 ], 
  [ 48, 73, 92, 104, 105, 151, 180 ], [ 48, 74, 93, 152, 181 ], 
  [ 26, 42, 46, 66, 75, 94, 178 ], [ 15, 30, 31, 48, 51, 75, 76, 127, 182 ], 
  [ 48, 54, 77, 106, 145, 153, 176 ], [ 48, 78, 84, 107, 127, 183 ], 
  [ 26, 59, 79, 100, 143, 155, 167, 177, 197 ], 
  [ 15, 30, 31, 51, 80, 102, 116 ], [ 31, 48, 81, 103, 108, 144, 198 ], 
  [ 67, 82, 104, 111, 112, 124, 145, 156, 184 ], [ 48, 105, 113, 117 ], 
  [ 48, 69, 84, 114, 115, 126, 186 ], [ 15, 52, 76, 118 ], 
  [ 15, 72, 86, 106, 112, 116, 140 ], [ 73, 82, 87, 107, 114, 117, 141 ], 
  [ 74, 78, 84, 88, 115, 142 ], [ 20, 31, 53, 77, 89, 107, 118 ], 
  [ 90, 119, 120, 157 ], [ 42, 91, 121, 129 ], [ 42, 92, 122, 129 ], 
  [ 42, 93, 158 ], [ 24, 42, 94, 123, 131 ], [ 42, 43, 95, 129 ], 
  [ 42, 96, 129 ], [ 42, 97, 158 ], [ 42, 98, 124, 125, 131 ], 
  [ 99, 119, 129, 130, 131 ], [ 100, 120, 131, 132, 146, 178 ], 
  [ 42, 101, 131, 132, 133 ], 
  [ 101, 102, 121, 132, 134, 135, 136, 161, 179, 182, 195 ], 
  [ 42, 103, 135, 136, 147 ], [ 42, 104, 111, 138, 148 ], 
  [ 48, 105, 113, 121, 122, 192 ], [ 15, 48, 76, 106, 171, 173, 190 ], 
  [ 47, 48, 82, 98, 107, 139, 186 ], 
  [ 15, 30, 31, 48, 51, 108, 136, 144, 191 ], 
  [ 48, 54, 104, 109, 140, 170, 176 ], [ 48, 110, 127, 137, 141, 142, 193 ], 
  [ 24, 42, 111, 119, 143 ], [ 26, 59, 95, 112, 138, 149, 150, 171, 177 ], 
  [ 48, 96, 113, 121, 145, 149 ], [ 48, 96, 113, 114, 145, 151, 184 ], 
  [ 48, 97, 115, 152, 194 ], [ 15, 30, 31, 51, 59, 76, 102, 116, 168 ], 
  [ 48, 105, 113, 117, 139, 169 ], [ 15, 52, 76, 106, 118, 139 ], 
  [ 119, 129, 146, 157 ], [ 120, 131, 147, 157 ], [ 42, 121, 129, 148 ], 
  [ 42, 122, 129, 149 ], [ 42, 123, 158 ], [ 90, 119, 124, 129, 148, 150 ], 
  [ 42, 125, 158 ], [ 42, 126, 131, 151, 152 ], 
  [ 42, 47, 69, 98, 126, 127, 178 ], [ 42, 128, 154, 155, 178 ], 
  [ 129, 157 ], [ 129, 130, 146, 157, 158 ], [ 129, 131, 157, 158 ], 
  [ 129, 131, 132, 147, 178 ], [ 42, 133, 158 ], [ 42, 131, 134, 159, 160 ], 
  [ 99, 131, 135, 148, 162, 178 ], [ 42, 101, 134, 135, 136, 147, 161 ], 
  [ 42, 137, 165, 166, 178 ], [ 26, 42, 138, 167, 172 ], 
  [ 48, 68, 125, 139, 194 ], [ 15, 48, 138, 140, 168, 187, 190 ], 
  [ 48, 82, 98, 141, 165, 169, 199 ], [ 48, 78, 84, 127, 142, 166, 200 ], 
  [ 42, 43, 143, 146 ], [ 42, 47, 69, 98, 126, 144, 147 ], 
  [ 42, 111, 145, 148, 171 ], [ 129, 146, 157 ], [ 131, 147, 157 ], 
  [ 119, 148, 157, 172 ], [ 42, 129, 149 ], [ 120, 129, 131, 132, 150, 172 ], 
  [ 42, 121, 129, 148, 151 ], [ 42, 152, 158 ], [ 42, 71, 148, 153, 173 ], 
  [ 24, 42, 131, 154, 174 ], [ 26, 42, 75, 155, 178 ], 
  [ 42, 90, 111, 156, 177 ], [ 157 ], [ 157, 158 ], 
  [ 42, 129, 159, 178, 179 ], [ 42, 158, 160 ], [ 42, 131, 161, 180, 181 ], 
  [ 131, 158, 162, 172, 178 ], [ 42, 135, 163, 178, 182 ], 
  [ 42, 94, 164, 167, 178 ], [ 42, 131, 165, 184, 185 ], 
  [ 42, 166, 178, 183, 186 ], [ 26, 42, 46, 66, 94, 167, 178 ], 
  [ 15, 30, 31, 48, 51, 127, 167, 168, 195 ], [ 48, 125, 169, 185, 201 ], 
  [ 42, 71, 148, 170, 187 ], [ 26, 42, 75, 171, 172 ], [ 131, 157, 172, 178 ],
  [ 42, 101, 172, 173, 182 ], [ 42, 158, 174 ], 
  [ 42, 47, 69, 98, 126, 175, 178 ], [ 42, 148, 176, 189, 190 ], 
  [ 26, 42, 120, 177, 191 ], [ 131, 157, 178 ], [ 42, 129, 178, 179 ], 
  [ 42, 129, 180, 192 ], [ 42, 158, 181 ], 
  [ 42, 101, 134, 135, 161, 178, 182 ], [ 42, 178, 183, 186 ], 
  [ 42, 129, 149, 184 ], [ 42, 158, 185 ], [ 42, 131, 184, 186, 194 ], 
  [ 42, 101, 172, 187, 195 ], [ 42, 71, 90, 188, 196 ], 
  [ 42, 67, 119, 189, 197 ], [ 42, 69, 127, 172, 190 ], 
  [ 26, 42, 46, 66, 94, 147, 191 ], [ 42, 96, 129, 192 ], 
  [ 42, 178, 193, 199, 200 ], [ 42, 158, 194 ], 
  [ 42, 101, 134, 135, 161, 178, 195 ], [ 42, 101, 120, 136, 196 ], 
  [ 42, 146, 149, 197 ], [ 42, 94, 147, 191, 198 ], [ 42, 67, 131, 199, 201 ],
  [ 42, 69, 127, 178, 200 ], [ 42, 158, 201 ] ]

# greens-acting: Idempotents, 1/?
gap> S := AsTransformationSemigroup(FullPBRMonoid(1));;
gap> S := Semigroup(S, rec(generic := false));;
gap> Idempotents(S);
[ Transformation( [ 1, 2, 2, 1, 5, 6, 6, 5, 5, 6, 6, 5, 1, 2, 2, 1 ] ), 
  Transformation( [ 1, 2, 2, 1, 5, 6, 6, 5, 5, 6, 6, 5, 1, 6, 6, 1 ] ), 
  Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 10, 11 ] ), 
  Transformation( [ 1, 1, 1, 1, 5, 5, 5, 5, 9, 9, 9, 9, 13, 13, 13, 13 ] ), 
  Transformation( [ 1, 1, 1, 1, 5, 5, 5, 5, 9, 9, 9, 9, 13, 9, 9, 13 ] ), 
  IdentityTransformation, Transformation( [ 4, 3, 3, 4, 8, 7, 7, 8, 12, 11,
      11, 12, 16, 15, 15, 16 ] ), 
  Transformation( [ 1, 1, 1, 1, 5, 5, 5, 5, 5, 5, 5, 5, 1, 1, 1, 1 ] ), 
  Transformation( [ 1, 1, 1, 1, 5, 5, 5, 5, 5, 5, 5, 5, 1, 5, 5, 1 ] ), 
  Transformation( [ 4, 3, 3, 4, 8, 7, 7, 8, 8, 7, 7, 8, 4, 3, 3, 4 ] ), 
  Transformation( [ 4, 3, 3, 4, 8, 7, 7, 8, 8, 7, 7, 8, 4, 7, 7, 4 ] ), 
  Transformation( [ 4, 3, 3, 4, 8, 7, 7, 8, 12, 11, 11, 12, 16, 11, 11, 16 ] )
    , Transformation( [ 4, 4, 4, 4, 8, 8, 8, 8, 12, 12, 12, 12, 16, 16, 16,
      16 ] ), Transformation( [ 4, 4, 4, 4, 8, 8, 8, 8, 12, 12, 12, 12, 16,
      12, 12, 16 ] ), Transformation( [ 4, 4, 4, 4, 8, 8, 8, 8, 8, 8, 8, 8, 4,
     4, 4, 4 ] ), Transformation( [ 4, 4, 4, 4, 8, 8, 8, 8, 8, 8, 8, 8, 4, 8,
      8, 4 ] ) ]

# greens-acting: Idempotents, 2/2
gap> S := Semigroup(FullTransformationMonoid(3),
> rec(generic := false));;
gap> RClasses(S);;
gap> Idempotents(S);
[ IdentityTransformation, Transformation( [ 1, 2, 1 ] ), 
  Transformation( [ 3, 2, 3 ] ), Transformation( [ 1, 2, 2 ] ), 
  Transformation( [ 1, 3, 3 ] ), Transformation( [ 2, 2 ] ), 
  Transformation( [ 1, 1 ] ), Transformation( [ 1, 1, 1 ] ), 
  Transformation( [ 2, 2, 2 ] ), Transformation( [ 3, 3, 3 ] ) ]

# greens-acting: Idempotents, for given rank, 1/4
gap> S := Semigroup(DualSymmetricInverseMonoid(3), rec(generic := false));;
gap> Idempotents(S, -1);
Error, Semigroups: Idempotents: usage,
the second argument <n> must be a non-negative integer,

# greens-acting: Idempotents, for given rank, 2/4
gap> S := Semigroup(DualSymmetricInverseMonoid(3), rec(generic := false));;
gap> Idempotents(S, 4);
[  ]

# greens-acting: Idempotents, for given rank, 3/4
gap> S := Semigroup(DualSymmetricInverseMonoid(3), rec(generic := false));;
gap> Idempotents(S);;
gap> Idempotents(S, 2);
[ <block bijection: [ 1, 2, -1, -2 ], [ 3, -3 ]>, 
  <block bijection: [ 1, -1 ], [ 2, 3, -2, -3 ]>, 
  <block bijection: [ 1, 3, -1, -3 ], [ 2, -2 ]> ]

# greens-acting: Idempotents, for given rank, 4/4
gap> S := Semigroup(DualSymmetricInverseMonoid(3), rec(generic := false));;
gap> Idempotents(S, 2);
[ <block bijection: [ 1, 2, -1, -2 ], [ 3, -3 ]>, 
  <block bijection: [ 1, -1 ], [ 2, 3, -2, -3 ]>, 
  <block bijection: [ 1, 3, -1, -3 ], [ 2, -2 ]> ]

# greens-acting: Idempotents, for a D-class, 1/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> IsRegularDClass(D);
false
gap> Idempotents(D);
[  ]

# greens-acting: Idempotents, for a D-class, 2/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> Idempotents(D);
[  ]

# greens-acting: Idempotents, for a L-class, 1/3
gap> S := Semigroup(FullTransformationMonoid(5), rec(generic := false));;
gap> x := Transformation([1, 1, 2, 3, 4]);;
gap> L := LClass(S, x);;
gap> Idempotents(L);
[ Transformation( [ 1, 2, 3, 4, 1 ] ), Transformation( [ 1, 2, 3, 4, 4 ] ), 
  Transformation( [ 1, 2, 3, 4, 2 ] ), Transformation( [ 1, 2, 3, 4, 3 ] ) ]

# greens-acting: Idempotents, for a L-class, 2/3
gap> S := AsTransformationSemigroup(FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> L := LClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsRegularClass(L);
false
gap> Idempotents(L);
[  ]

# greens-acting: Idempotents, for a L-class, 3/3
gap> S := PartitionMonoid(3);;
gap> L := LClass(S, One(S));;
gap> Idempotents(L);
[ <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ]> ]

# greens-acting: Idempotents, for a H-class, 1/2
gap> S := SingularTransformationSemigroup(4);;
gap> H := HClass(S, S.1);
<Green's H-class: Transformation( [ 1, 2, 3, 3 ] )>
gap> Idempotents(H);
[ Transformation( [ 1, 2, 3, 3 ] ) ]

# greens-acting: Idempotents, for a H-class, 1/2
gap> S := AsTransformationSemigroup(FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> H := HClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsGroupHClass(H);
false
gap> Idempotents(H);
[  ]

# greens-acting: NrIdempotents, for a semigroup, 1/2
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> NrIdempotents(S);
24

# greens-acting: NrIdempotents, for a semigroup, 2/2
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> Idempotents(S);;
gap> NrIdempotents(S);
24

# greens-acting: NrIdempotents, for a D-class, 1/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> IsRegularDClass(D);
false
gap> NrIdempotents(D);
0

# greens-acting: NrIdempotents, for a D-class, 2/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> NrIdempotents(D);
0

# greens-acting: NrIdempotents, for a L-class, 1/3
gap> S := Semigroup(FullTransformationMonoid(5), rec(generic := false));;
gap> x := Transformation([1, 1, 2, 3, 4]);;
gap> L := LClass(S, x);;
gap> NrIdempotents(L);
4

# greens-acting: NrIdempotents, for a L-class, 2/3
gap> S := AsTransformationSemigroup(FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> L := LClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsRegularClass(L);
false
gap> NrIdempotents(L);
0

# greens-acting: NrIdempotents, for a L-class, 3/3
gap> S := PartitionMonoid(3);;
gap> L := LClass(S, One(S));;
gap> NrIdempotents(L);
1

# greens-acting: NrIdempotents, for a H-class, 1/2
gap> S := SingularTransformationSemigroup(4);;
gap> H := HClass(S, S.1);
<Green's H-class: Transformation( [ 1, 2, 3, 3 ] )>
gap> NrIdempotents(H);
1

# greens-acting: NrIdempotents, for a H-class, 1/2
gap> S := AsTransformationSemigroup(FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> H := HClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsGroupHClass(H);
false
gap> NrIdempotents(H);
0

# greens-acting: NrIdempotents, for an R-class, 1/2
gap> S := Semigroup(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                    Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> R := First(RClasses(S),
> x -> Transformation([9, 10, 4, 9, 10, 4, 4, 3, 3, 6]) in x);;
gap> NrIdempotents(R);
0
gap> IsRegularClass(R);
false

# greens-acting: NrIdempotents, for an R-class, 3/3
gap> S := Semigroup(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                    Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> R := RClass(S, Transformation([6, 9, 9, 6, 9, 1, 1, 2, 2, 6]));;
gap> IsRegularClass(R);
true
gap> NrIdempotents(R);
7

# greens-acting: IsRegularClass, for an R-class, 1/1
gap> S := Semigroup(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                    Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> R := First(RClasses(S),
> x -> Transformation([9, 10, 4, 9, 10, 4, 4, 3, 3, 6]) in x);;
gap> IsRegularClass(R);
false

# greens-acting: IsRegularClass, for an R-class in group of units, 1/1
gap> S := Monoid(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                 Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> S := AsBipartitionSemigroup(S);;
gap> R := RClass(S, IdentityBipartition(10));;
gap> IsRegularClass(R);
true

# greens-acting: NrRegularDClasses, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(generic := false));;
gap> NrRegularDClasses(S);
6

# greens-acting: Enumerator/IteratorOfR/DClasses, 1/1
gap> S := ReesZeroMatrixSemigroup(SymmetricGroup(3), [
> [(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, (), 0, (1, 3), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, (), (1, 3), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, (1, 3), (2, 3), (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
>  0],
> [0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, (), (2, 3), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, (1, 3, 2), (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
>  0],
> [0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, (), (1, 3), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, (1, 2), 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (1, 3), (), (), 0, 0, 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), (1, 2), 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), (1, 2, 3), (1, 3, 2),
>  0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0, 0, 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), (1, 3), 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (), 0],
> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ()]]);;
gap> S := Semigroup(S, rec(generic := false));;
gap> enum := EnumeratorOfRClasses(S);
<enumerator of R-classes of <semigroup with 46 generators>>
gap> ForAll(enum, x -> enum[Position(enum, x)] = x);
true
gap> ForAll([1 .. Length(enum)], i -> Position(enum, enum[i]) = i);
true
gap> ForAll(enum, x -> x in enum);
true
gap> RClasses(S);;
gap> iter := IteratorOfRClasses(S);
<iterator>
gap> iter := IteratorOfDClasses(S);
<iterator>
gap> S := Semigroup(S, rec(generic := false));;
gap> iter := IteratorOfRClasses(S);
<iterator of R-classes>
gap> iter := IteratorOfDClasses(S);
<iterator of D-classes>
gap> for x in iter do od;

# greens-acting: RhoCosets, 1/3
gap> S := Semigroup([Transformation([1, 3, 7, 5, 5, 7, 1]),
>  Transformation([5, 3, 5, 3, 4, 1, 2]),
>  Transformation([6, 1, 6, 5, 5, 2]),
>  Transformation([6, 1, 7, 6, 3, 2, 4]),
>  Transformation([6, 2, 1, 4, 6, 4, 3]),
>  Transformation([7, 3, 3, 6, 4, 5, 5])], rec(generic := false));;
gap> L := LClass(S, Transformation([1, 5, 1, 4, 4, 3, 2]));;
gap> RhoCosets(L);
<enumerator of perm group>

# greens-acting: RhoCosets, 2/3
gap> S := Semigroup([Transformation([1, 4, 2, 5, 2, 1, 6, 1, 7, 6]),
>  Transformation([2, 8, 4, 7, 5, 8, 3, 5, 8, 6])],
> rec(generic := false));;
gap> L := LClass(S, Transformation([4, 6, 6, 1, 6, 4, 6, 4, 1, 6]));
<Green's L-class: Transformation( [ 4, 6, 6, 1, 6, 4, 6, 4, 1, 6 ] )>
gap> RhoCosets(L);
[ (), (4,6) ]

# greens-acting: RhoCosets, 3/3
gap> S := Semigroup([Transformation([1, 4, 2, 5, 2, 1, 6, 1, 7, 6]),
>  Transformation([2, 8, 4, 7, 5, 8, 3, 5, 8, 6])],
> rec(generic := false));;
gap> L := LClass(S, Transformation([7, 8, 8, 2, 8, 7, 8, 7, 2, 8]));;
gap> RhoCosets(L);
RightTransversal(Group([ (2,8,7), (7,8) ]),Group([ (2,7,8) ]))

# greens-acting: SemigroupDataIndex, 1/1
gap> S := Semigroup([Transformation([1, 4, 2, 5, 2, 1, 6, 1, 7, 6]),
>  Transformation([2, 8, 4, 7, 5, 8, 3, 5, 8, 6])],
> rec(generic := false));;
gap> L := LClass(S, Transformation([7, 8, 8, 2, 8, 7, 8, 7, 2, 8]));;
gap> HasSemigroupDataIndex(L);
false
gap> SemigroupDataIndex(L);
118

# greens-acting: SchutzenbergerGroup, for a D-class, 1/1
gap> S := Semigroup([Transformation([3, 1, 4, 2, 5, 2, 1, 6, 1, 7]),
>   Transformation([6, 2, 8, 4, 7, 5, 8, 3, 5, 8])],
> rec(generic := false));;
gap> D := DClass(S, Transformation([2, 1, 4, 2, 1, 6, 4, 4, 6, 4]));;
gap> SchutzenbergerGroup(D);
Group([ (1,4)(2,6), (1,6,4,2) ])

# greens-acting: SchutzenbergerGroup, for a H-class, 1/1
gap> S := Semigroup([Transformation([7, 1, 4, 3, 2, 7, 7, 6, 6, 5]),
>  Transformation([7, 10, 10, 1, 7, 9, 10, 4, 2, 10])],
>  rec(generic := false));;
gap> H := HClass(S,
> Transformation([10, 10, 10, 10, 10, 10, 10, 7, 10, 10]));;
gap> SchutzenbergerGroup(H);
Group(())

#E# 
gap> STOP_TEST("Semigroups package: standard/greens/greens-acting.tst");
