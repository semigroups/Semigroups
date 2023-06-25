#############################################################################
##
#W  standard/greens/acting.tst
#Y  Copyright (C) 2011-2023                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local BruteForceInverseCheck, BruteForceIsoCheck, CheckLeftGreensMultiplier1
#@local CheckLeftGreensMultiplier2, CheckRightGreensMultiplier1
#@local CheckRightGreensMultiplier2, D, DD, DDD, H, L, L3, LL, R, RR, RRR, S, T
#@local a, acting, b, c, d, e, en, h, inv, it, iter, map, mults, nr, r, x, y
gap> START_TEST("Semigroups package: standard/greens/acting.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# DClassOfLClass, 1/1
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
gap> DD < D; 
false

# DClassOfRClass, 1/1
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

# DClassOfHClass, 1/1
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

# LClassOfHClass, 1/1
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

# RClassOfHClass, 1/1
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

# GreensDClassOfElement, fail, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(acting := true));;
gap> GreensDClassOfElement(S, PartialPerm([19]));
Error, the 2nd argument (a mult. elt.) does not belong to the 1st argument (a \
semigroup)

# GreensDClassOfElementNC, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(acting := true));;
gap> D := GreensDClassOfElementNC(S, PartialPerm([19]));;
gap> Size(D);
5

# GreensL/RClassOfElement, fail, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(acting := true));;
gap> RClass(S, ConstantTransformation(7, 7));
Error, the 2nd argument (a mult. elt.) does not belong to the 1st argument (a \
semigroup)
gap> LClass(S, ConstantTransformation(7, 7));
Error, the 2nd argument (a mult. elt.) does not belong to the 1st argument (a \
semigroup)
gap> HClass(S, ConstantTransformation(7, 7));
Error, the element does not belong to the semigroup

# GreensL/RClassOfElementNC, fail, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(acting := true));;
gap> Size(RClassNC(S, ConstantTransformation(7, 7)));
1
gap> Size(LClassNC(S, ConstantTransformation(7, 7)));
1
gap> Size(HClassNC(S, ConstantTransformation(7, 7)));
1

# GreensL/RClassOfElement, for a D-class, 1/1
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
> rec(acting := true));;
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
Error, the 2nd argument (a mult. elt.) does not belong to the 1st argument (a \
Green's D-class)
gap> RClass(D, IdentityBipartition(8));
Error, the 2nd argument (a mult. elt.) does not belong to the 1st argument (a \
Green's D-class)
gap> x := Bipartition([[1, 4, -1, -2, -6], [2, 3, 5, -4],
> [6, -3], [-5]]);;
gap> LClassNC(D, x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `IsBound[]' on 2 arguments
The 2nd argument is 'fail' which might point to an earlier problem

gap> RClassNC(D, x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `IsBound[]' on 2 arguments
The 2nd argument is 'fail' which might point to an earlier problem


# GreensClassOfElementNC(D-class, x) inverse-op, 1/1
gap> S := InverseSemigroup([
>  PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(acting := true));;
gap> D := DClass(S, S.3 * S.2);;
gap> Size(LClassNC(D, S.3 * S.2));
3

# GreensHClassOfElement, 1/1
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
gap> S := Semigroup(S, rec(acting := true));;
gap> D := DClass(S, S.4 * S.5);;
gap> H := HClass(D, MultiplicativeZero(S));
<Green's H-class: 0>
gap> H := HClassNC(D, MultiplicativeZero(S));
<Green's H-class: 0>
gap> H := HClass(D, IdentityTransformation);
Error, the 2nd argument (a mult. elt.) does not belong to the 1st argument (a \
Green's class)

# GreensHClassOfElement(L/R-class, x), 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(acting := true));;
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

# \in, for D-class, 1/4
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

# \in, for D-class, 2/4
gap> S := OrderEndomorphisms(5);;
gap> x := Transformation([1, 2, 2, 4, 5]);;
gap> D := DClass(S, x);;
gap> x in D;
true
gap> Transformation([1, 2, 1, 4, 5]) in D;
false

# \in, for D-class, 3/4
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

# \in, for D-class, 4/4
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> D := DClass(S, x);;
gap> y in D;
false

# \in, for L-class, 1/5
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

# \in, for L-class, 2/5
gap> S := OrderEndomorphisms(5);;
gap> x := Transformation([1, 2, 2, 4, 5]);;
gap> L := LClass(S, x);;
gap> x in L;
true
gap> Transformation([1, 2, 1, 4, 5]) in L;
false

# \in, for L-class, 3/5
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

# \in, for L-class, 4/5
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> L := LClass(S, x);;
gap> y in L;
false

# \in, for L-class, 5/5
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 4, 3, 5, 5]);;
gap> L := LClass(S, x);;
gap> y in L;
false

# \in, for R-class, 1/6
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

# \in, for R-class, 2/6
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 4, 3, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# \in, for R-class, 3/6
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 3, 3, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# \in, for R-class, 4/6
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 2, 3, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# \in, for R-class, 5/6
gap> S := OrderEndomorphisms(5);;
gap> x := Transformation([1, 2, 2, 4, 5]);;
gap> R := RClass(S, x);;
gap> x in R;
true
gap> Transformation([1, 2, 1, 4, 5]) in R;
false

# \in, for R-class, 6/6
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# \in, for H-class, 1/3
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> H := HClass(S, x);;
gap> y in H;
false

# \in, for H-class, 2/3
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 2, 3, 5, 5]);;
gap> H := HClass(S, x);;
gap> y in H;
false

# \in, for H-class, 3/3
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> H := HClass(S, x);;
gap> ForAll(H, x -> x in H);
true

# \in, for D-class reps/D-classes, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(acting := true));;
gap> DClassReps(S);
[ [4,5,7](1,3)(6), [6,4,7,1,2,5](3), [4,7,2,5,1,3](6), [1,3][2,7,5][4,6], 
  [4,7](1)(3)(6), [2,7,3,1][6,5], [2,7][5,3](1)(6), [4,6](1)(7), 
  [5,1,3,2][6,4], [4,1,5][6,7,2](3), [6,4,1,3][7,5,2], [2,1,3](4), 
  [4,1][5,2](3)(6), [4,2,1,5][6,7,3], [4,2,1][7,5,3](6), [4,6][7,1](2), 
  [1,7,3][4,5](6), [4,5,3](7), (1,3)(6), [6,7,1](3), [5,1,3](6), [1,3][4,6], 
  [6,5,3](1), [4,3,1,7], [4,3][6,5](1)(7), [2,3][4,5](1), [4,3,1](6), 
  [2,3](1,7), [2,3][5,1](6)(7), [4,6][7,3], [4,7,1](6), [4,7][5,1], 
  [6,4,1,2](3), [7,3,2,1], [5,3][6,4](1,2), [7,1,2](4), [1,3,5,2][6,7], 
  [4,2][6,1][7,5](3), [1,3][4,2][6,7](5), [1,3][4,7](2), [6,4,2](3)(5), 
  [4,5][6,1][7,3](2), [6,4,5,3](2), [7,2,5](4), [3,5], [6,4][7,3](1), 
  [5,1](7), <empty partial perm>, [1,3][4,2](6), [6,1](2)(3), [1,3](2)(6), 
  [1,3][4,6][7,2], [6,7](3,5), [4,3][6,2][7,5](1), [4,3][6,7,1](5), 
  [4,3](5)(6), [4,5][6,2,3](1), [2,3][4,5][7,1](6), [2,5][4,6][7,3], 
  [4,1,2](6), [4,1](7), [2,7](1), [5,3,7](6), [4,3][6,5](7), [4,3][5,7](6), 
  [2,3][4,7][6,5], [2,7,1], [5,1,3][6,7], [3,1][4,7], [4,1,3][6,7], 
  [2,1,3][4,7], [2,1][5,3](6), [4,3,1][6,5], [2,3][6,5,1], [6,3,1](7), 
  [3,1][6,5,7], [4,7,1][6,3], [4,7][6,5,1], [6,5][7,1,3], [3,7][6,1], 
  [4,1,3](7), [4,1][5,7,3], [4,1][5,7](6), [2,1][4,7,3](6), [2,7,1][4,6], 
  [4,3](6), [4,5], [4,1][6,7], [1,3,2][6,4], [6,1][7,2](3), [1,3][5,2][6,4], 
  [1,3](4), [4,3,2](1), [4,3][7,1,2], [6,4,3,2], [7,2,3](1), 
  [5,2,3][6,4][7,1], [7,3](4), [6,4,1][7,2], [1,5][4,2][6,7](3), [7,3,5](2), 
  [1,5,3][6,7](2), [1,5][4,7,2], [6,1,3](5), [4,5][6,2](3), [4,5][6,1,3], 
  [2,5][4,1,3], [4,5][6,7](3), [6,2,5][7,3], [2,5,3][6,7], [4,7,5], 
  [1,2][6,7,3], [1,3][6,4,5], [6,2,5](3), [1,3][2,5][6,4], [1,3][7,5](4), 
  [1,2][4,3][6,5], [4,3][6,1][7,2], [6,4,3], [1,2,3][6,5], [6,4][7,2,3], 
  [1,5][6,4,2], [5,3,1][6,4], [5,1][6,4,3], <identity partial perm on [ 4 ]>, 
  [6,1,3][7,2], [5,3][7,2](6), [4,2][7,3](6), [1,5][4,3][6,7], 
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
  [6,4,3](1), [2,3][6,4](1), [7,3](1)(4), [5,2][6,1](3), [4,2][5,3][6,1], 
  [4,3][5,2](6), [4,2,3](6), [4,6][7,3](2), [6,7](5), [6,7,3](5), 
  [4,3][6,7,5], [4,5][6,2](1), [6,2,5](1), [4,2][7,5](1), [4,5][6,7](1), 
  [4,1][5,3][6,2], [4,3][6,2,1], [4,2,3][7,1], [5,7,3](6), [4,3](6)(7), 
  [2,7][6,5], [4,5](7), [2,3][6,7,1], [5,3,1][6,7], [4,3][5,1][6,7], 
  [4,3][6,5][7,1], [4,7][6,3,1], [2,7][5,1][6,3], [6,3](1,7), [6,5,1](7), 
  [4,7,1][6,5], [2,1,3][6,5], [4,5][7,1,3], [5,2,3][6,4], [4,2][6,1](3), 
  [5,3][6,1](2), [6,1,2][7,3], [5,1,2][6,3], [4,1,2][6,3], [4,3](1,2), 
  [4,1][6,2][7,3], [4,3][6,1,2], [6,4,3][7,2], [6,4,2](1), [1,5,3][6,7], 
  [1,3][6,2](5), [1,3][4,5][6,2], [1,3][4,2,5], [6,3,5][7,2], [4,2][6,3][7,5],
  [1,3][4,5][7,2], [4,5,2][7,3], [4,5,2][6,7], [4,2,5][6,7,3], [4,7,5](2), 
  [2,3][6,1][7,5], [6,1](3,5), [4,3][6,1](5), [4,5][6,7,3], [1,2][4,3][6,7], 
  [1,2,3][6,7], [1,2][4,7,3], [4,2][6,7], [6,7](2), [4,7,2], [6,2](3)(5), 
  [4,5,3][6,2], [6,4,3](5), [2,3][6,4,5], [2,5][7,3](4), [4,2][6,5,3], 
  [4,3][6,5](2), [4,5][7,2,3], [6,4,5], [2,5][6,4], [7,5](4), [5,1][6,4][7,3],
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
  <Green's D-class: [1,7,3][4,5](6)>, <Green's D-class: [4,5,3](7)>, 
  <Green's D-class: (1,3)(6)>, <Green's D-class: [6,7,1](3)>, 
  <Green's D-class: [5,1,3](6)>, <Green's D-class: [1,3][4,6]>, 
  <Green's D-class: [6,5,3](1)>, <Green's D-class: [4,3,1,7]>, 
  <Green's D-class: [4,3][6,5](1)(7)>, <Green's D-class: [2,3][4,5](1)>, 
  <Green's D-class: [4,3,1](6)>, <Green's D-class: [2,3](1,7)>, 
  <Green's D-class: [2,3][5,1](6)(7)>, <Green's D-class: [4,6][7,3]>, 
  <Green's D-class: [4,7,1](6)>, <Green's D-class: [4,7][5,1]>, 
  <Green's D-class: [6,4,1,2](3)>, <Green's D-class: [7,3,2,1]>, 
  <Green's D-class: [5,3][6,4](1,2)>, <Green's D-class: [7,1,2](4)>, 
  <Green's D-class: [1,3,5,2][6,7]>, <Green's D-class: [4,2][6,1][7,5](3)>, 
  <Green's D-class: [1,3][4,2][6,7](5)>, <Green's D-class: [1,3][4,7](2)>, 
  <Green's D-class: [6,4,2](3)(5)>, <Green's D-class: [4,5][6,1][7,3](2)>, 
  <Green's D-class: [6,4,5,3](2)>, <Green's D-class: [7,2,5](4)>, 
  <Green's D-class: [3,5]>, <Green's D-class: [6,4][7,3](1)>, 
  <Green's D-class: [5,1](7)>, <Green's D-class: <empty partial perm>>, 
  <Green's D-class: [1,3][4,2](6)>, <Green's D-class: [6,1](2)(3)>, 
  <Green's D-class: [1,3](2)(6)>, <Green's D-class: [1,3][4,6][7,2]>, 
  <Green's D-class: [6,7](3,5)>, <Green's D-class: [4,3][6,2][7,5](1)>, 
  <Green's D-class: [4,3][6,7,1](5)>, <Green's D-class: [4,3](5)(6)>, 
  <Green's D-class: [4,5][6,2,3](1)>, <Green's D-class: [2,3][4,5][7,1](6)>, 
  <Green's D-class: [2,5][4,6][7,3]>, <Green's D-class: [4,1,2](6)>, 
  <Green's D-class: [4,1](7)>, <Green's D-class: [2,7](1)>, 
  <Green's D-class: [5,3,7](6)>, <Green's D-class: [4,3][6,5](7)>, 
  <Green's D-class: [4,3][5,7](6)>, <Green's D-class: [2,3][4,7][6,5]>, 
  <Green's D-class: [2,7,1]>, <Green's D-class: [5,1,3][6,7]>, 
  <Green's D-class: [3,1][4,7]>, <Green's D-class: [4,1,3][6,7]>, 
  <Green's D-class: [2,1,3][4,7]>, <Green's D-class: [2,1][5,3](6)>, 
  <Green's D-class: [4,3,1][6,5]>, <Green's D-class: [2,3][6,5,1]>, 
  <Green's D-class: [6,3,1](7)>, <Green's D-class: [3,1][6,5,7]>, 
  <Green's D-class: [4,7,1][6,3]>, <Green's D-class: [4,7][6,5,1]>, 
  <Green's D-class: [6,5][7,1,3]>, <Green's D-class: [3,7][6,1]>, 
  <Green's D-class: [4,1,3](7)>, <Green's D-class: [4,1][5,7,3]>, 
  <Green's D-class: [4,1][5,7](6)>, <Green's D-class: [2,1][4,7,3](6)>, 
  <Green's D-class: [2,7,1][4,6]>, <Green's D-class: [4,3](6)>, 
  <Green's D-class: [4,5]>, <Green's D-class: [4,1][6,7]>, 
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
  <Green's D-class: <identity partial perm on [ 4 ]>>, 
  <Green's D-class: [6,1,3][7,2]>, <Green's D-class: [5,3][7,2](6)>, 
  <Green's D-class: [4,2][7,3](6)>, <Green's D-class: [1,5][4,3][6,7]>, 
  <Green's D-class: [1,5][2,3][6,7]>, <Green's D-class: [1,5][4,7,3]>, 
  <Green's D-class: [3,1][6,2](5)>, <Green's D-class: [4,5][6,3][7,1]>, 
  <Green's D-class: [4,5,1][6,2]>, <Green's D-class: [4,5,1][6,7]>, 
  <Green's D-class: [2,5][4,1][6,3]>, <Green's D-class: [2,5][4,1][6,7]>, 
  <Green's D-class: [2,1][4,7,5]>, <Green's D-class: [5,1][6,2][7,3]>, 
  <Green's D-class: [4,1,3][6,5]>, <Green's D-class: [4,1][7,3](6)>, 
  <Green's D-class: [1,5][4,3](6)>, <Green's D-class: [1,7][4,3](6)>, 
  <Green's D-class: [1,7][2,3](6)>, <Green's D-class: [1,7,3][4,6]>, 
  <Green's D-class: [4,7][6,5]>, <Green's D-class: [4,1][6,7](3)>, 
  <Green's D-class: [2,1][5,3][6,7]>, <Green's D-class: [6,7,3](1)>, 
  <Green's D-class: [2,3][7,1](6)>, <Green's D-class: [2,1][6,5][7,3]>, 
  <Green's D-class: [5,7][6,3](1)>, <Green's D-class: [4,7][6,3](1)>, 
  <Green's D-class: [2,7][4,3](1)>, <Green's D-class: [4,7][6,5](1)>, 
  <Green's D-class: [2,7][6,5](1)>, <Green's D-class: [4,5](1)(7)>, 
  <Green's D-class: [6,5,1](3)>, <Green's D-class: [4,1][6,5,3]>, 
  <Green's D-class: [4,7,3][6,1]>, <Green's D-class: [2,7][4,3][6,1]>, 
  <Green's D-class: [4,3][6,7](1)>, <Green's D-class: [4,1,7](6)>, 
  <Green's D-class: [1,2][5,3][6,4]>, <Green's D-class: [5,2][6,1,3]>, 
  <Green's D-class: [4,2][6,1,3]>, <Green's D-class: [4,1,3](2)>, 
  <Green's D-class: [6,4,2](3)>, <Green's D-class: [5,3][6,4](2)>, 
  <Green's D-class: [6,3,2][7,1]>, <Green's D-class: [4,1][6,3][7,2]>, 
  <Green's D-class: [4,2][7,1,3]>, <Green's D-class: [4,2][5,1][7,3]>, 
  <Green's D-class: [5,1][6,4,2]>, <Green's D-class: [6,4,1][7,3](2)>, 
  <Green's D-class: [7,2,1](4)>, <Green's D-class: [1,3,5][6,7]>, 
  <Green's D-class: [6,2][7,5](3)>, <Green's D-class: [1,3][6,7](5)>, 
  <Green's D-class: [1,2][4,3,5]>, <Green's D-class: [1,5][4,3][7,2]>, 
  <Green's D-class: [4,3,5][6,7]>, <Green's D-class: [1,2,3][7,5]>, 
  <Green's D-class: [6,7,2,3](5)>, <Green's D-class: [4,2][6,7,5]>, 
  <Green's D-class: [4,5][6,1](3)>, <Green's D-class: [2,5,3][6,1]>, 
  <Green's D-class: [6,1,5][7,3]>, <Green's D-class: [4,3][6,2][7,5]>, 
  <Green's D-class: [2,3][6,7,5]>, <Green's D-class: [4,5][6,7]>, 
  <Green's D-class: [5,3,2][6,7]>, <Green's D-class: [4,3][5,2][6,7]>, 
  <Green's D-class: [1,3][6,2][7,5]>, <Green's D-class: [6,4][7,5,3]>, 
  <Green's D-class: [6,4,5][7,3]>, <Green's D-class: [6,5,2][7,3]>, 
  <Green's D-class: [6,4,2][7,3]>, <Green's D-class: [6,4,3](1)>, 
  <Green's D-class: [2,3][6,4](1)>, <Green's D-class: [7,3](1)(4)>, 
  <Green's D-class: [5,2][6,1](3)>, <Green's D-class: [4,2][5,3][6,1]>, 
  <Green's D-class: [4,3][5,2](6)>, <Green's D-class: [4,2,3](6)>, 
  <Green's D-class: [4,6][7,3](2)>, <Green's D-class: [6,7](5)>, 
  <Green's D-class: [6,7,3](5)>, <Green's D-class: [4,3][6,7,5]>, 
  <Green's D-class: [4,5][6,2](1)>, <Green's D-class: [6,2,5](1)>, 
  <Green's D-class: [4,2][7,5](1)>, <Green's D-class: [4,5][6,7](1)>, 
  <Green's D-class: [4,1][5,3][6,2]>, <Green's D-class: [4,3][6,2,1]>, 
  <Green's D-class: [4,2,3][7,1]>, <Green's D-class: [5,7,3](6)>, 
  <Green's D-class: [4,3](6)(7)>, <Green's D-class: [2,7][6,5]>, 
  <Green's D-class: [4,5](7)>, <Green's D-class: [2,3][6,7,1]>, 
  <Green's D-class: [5,3,1][6,7]>, <Green's D-class: [4,3][5,1][6,7]>, 
  <Green's D-class: [4,3][6,5][7,1]>, <Green's D-class: [4,7][6,3,1]>, 
  <Green's D-class: [2,7][5,1][6,3]>, <Green's D-class: [6,3](1,7)>, 
  <Green's D-class: [6,5,1](7)>, <Green's D-class: [4,7,1][6,5]>, 
  <Green's D-class: [2,1,3][6,5]>, <Green's D-class: [4,5][7,1,3]>, 
  <Green's D-class: [5,2,3][6,4]>, <Green's D-class: [4,2][6,1](3)>, 
  <Green's D-class: [5,3][6,1](2)>, <Green's D-class: [6,1,2][7,3]>, 
  <Green's D-class: [5,1,2][6,3]>, <Green's D-class: [4,1,2][6,3]>, 
  <Green's D-class: [4,3](1,2)>, <Green's D-class: [4,1][6,2][7,3]>, 
  <Green's D-class: [4,3][6,1,2]>, <Green's D-class: [6,4,3][7,2]>, 
  <Green's D-class: [6,4,2](1)>, <Green's D-class: [1,5,3][6,7]>, 
  <Green's D-class: [1,3][6,2](5)>, <Green's D-class: [1,3][4,5][6,2]>, 
  <Green's D-class: [1,3][4,2,5]>, <Green's D-class: [6,3,5][7,2]>, 
  <Green's D-class: [4,2][6,3][7,5]>, <Green's D-class: [1,3][4,5][7,2]>, 
  <Green's D-class: [4,5,2][7,3]>, <Green's D-class: [4,5,2][6,7]>, 
  <Green's D-class: [4,2,5][6,7,3]>, <Green's D-class: [4,7,5](2)>, 
  <Green's D-class: [2,3][6,1][7,5]>, <Green's D-class: [6,1](3,5)>, 
  <Green's D-class: [4,3][6,1](5)>, <Green's D-class: [4,5][6,7,3]>, 
  <Green's D-class: [1,2][4,3][6,7]>, <Green's D-class: [1,2,3][6,7]>, 
  <Green's D-class: [1,2][4,7,3]>, <Green's D-class: [4,2][6,7]>, 
  <Green's D-class: [6,7](2)>, <Green's D-class: [4,7,2]>, 
  <Green's D-class: [6,2](3)(5)>, <Green's D-class: [4,5,3][6,2]>, 
  <Green's D-class: [6,4,3](5)>, <Green's D-class: [2,3][6,4,5]>, 
  <Green's D-class: [2,5][7,3](4)>, <Green's D-class: [4,2][6,5,3]>, 
  <Green's D-class: [4,3][6,5](2)>, <Green's D-class: [4,5][7,2,3]>, 
  <Green's D-class: [6,4,5]>, <Green's D-class: [2,5][6,4]>, 
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

# L-classes/reps, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(acting := true));;
gap> GreensLClasses(S);
[ <Green's L-class: Transformation( [ 2, 2, 1, 2, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 2, 5, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 3, 1, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 3, 4, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 5, 1, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 3, 4, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 6, 6, 4, 6, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 4, 5, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 4, 3, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 6, 2, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 4, 2, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 2, 5, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 3, 1, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 3, 5, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 6, 6, 4, 6, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 3, 4, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 2, 3, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 3, 2, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 3, 2, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 1, 2, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 2, 6, 6, 5, 1, 4 ] )>, 
  <Green's L-class: Transformation( [ 5, 3, 3, 4, 2 ] )>, 
  <Green's L-class: Transformation( [ 1, 3, 3 ] )>, 
  <Green's L-class: Transformation( [ 4, 2, 3, 1, 4, 2 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 2, 2, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 6, 6, 6, 6, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 4, 4, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 5, 5, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 1, 1, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 3, 3, 3, 3 ] )>, 
  <Green's L-class: Transformation( [ 4, 2, 6, 6, 3, 4 ] )>, 
  <Green's L-class: Transformation( [ 5, 3, 4, 4, 6, 5 ] )>, 
  <Green's L-class: Transformation( [ 5, 1, 4, 4, 6, 5 ] )>, 
  <Green's L-class: Transformation( [ 1, 3, 4, 4, 6, 1 ] )>, 
  <Green's L-class: Transformation( [ 4, 5, 6, 6, 2, 4 ] )>, 
  <Green's L-class: Transformation( [ 2, 1, 4, 4, 5, 2 ] )>, 
  <Green's L-class: Transformation( [ 6, 2, 5, 5, 1, 6 ] )>, 
  <Green's L-class: Transformation( [ 2, 1, 4, 4, 6, 2 ] )>, 
  <Green's L-class: Transformation( [ 3, 5, 4, 4, 2, 3 ] )>, 
  <Green's L-class: Transformation( [ 3, 1, 4, 4, 5, 3 ] )>, 
  <Green's L-class: Transformation( [ 1, 3, 5, 5, 6, 1 ] )>, 
  <Green's L-class: Transformation( [ 2, 3, 5, 5, 6, 2 ] )>, 
  <Green's L-class: Transformation( [ 2, 1, 4, 4, 3, 2 ] )>, 
  <Green's L-class: Transformation( [ 6, 6, 6, 6, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 4, 4, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 3, 3, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 5, 5, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 2, 2, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 2, 2, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 6, 6, 6, 6, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 3, 3, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 3, 3, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 1, 1, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 5, 5, 3, 3 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 4, 4, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 2, 2, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 1, 1, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 3, 3, 2, 2 ] )> ]
gap> LClassReps(S);
[ Transformation( [ 2, 2, 1, 2, 4, 4 ] ), 
  Transformation( [ 5, 5, 2, 5, 6, 6 ] ), 
  Transformation( [ 1, 1, 3, 1, 6, 6 ] ), 
  Transformation( [ 4, 4, 3, 4, 5, 5 ] ), 
  Transformation( [ 1, 1, 5, 1, 6, 6 ] ), 
  Transformation( [ 4, 4, 3, 4, 6, 6 ] ), 
  Transformation( [ 6, 6, 4, 6, 5, 5 ] ), 
  Transformation( [ 5, 5, 4, 5, 1, 1 ] ), 
  Transformation( [ 3, 3, 4, 3, 1, 1 ] ), 
  Transformation( [ 2, 2, 6, 2, 4, 4 ] ), 
  Transformation( [ 2, 2, 4, 2, 5, 5 ] ), 
  Transformation( [ 5, 5, 2, 5, 1, 1 ] ), 
  Transformation( [ 1, 1, 3, 1, 5, 5 ] ), 
  Transformation( [ 5, 5, 3, 5, 6, 6 ] ), 
  Transformation( [ 6, 6, 4, 6, 1, 1 ] ), 
  Transformation( [ 4, 4, 3, 4, 2, 2 ] ), 
  Transformation( [ 3, 3, 2, 3, 1, 1 ] ), 
  Transformation( [ 2, 2, 3, 2, 5, 5 ] ), 
  Transformation( [ 2, 2, 3, 2, 6, 6 ] ), 
  Transformation( [ 2, 2, 1, 2, 6, 6 ] ), 
  Transformation( [ 2, 6, 6, 5, 1, 4 ] ), Transformation( [ 5, 3, 3, 4, 2 ] ),
  Transformation( [ 1, 3, 3 ] ), Transformation( [ 4, 2, 3, 1, 4, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 6, 6, 6, 6, 6, 6 ] ), 
  Transformation( [ 4, 4, 4, 4, 4, 4 ] ), 
  Transformation( [ 5, 5, 5, 5, 5, 5 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 1 ] ), 
  Transformation( [ 3, 3, 3, 3, 3, 3 ] ), 
  Transformation( [ 4, 2, 6, 6, 3, 4 ] ), 
  Transformation( [ 5, 3, 4, 4, 6, 5 ] ), 
  Transformation( [ 5, 1, 4, 4, 6, 5 ] ), 
  Transformation( [ 1, 3, 4, 4, 6, 1 ] ), 
  Transformation( [ 4, 5, 6, 6, 2, 4 ] ), 
  Transformation( [ 2, 1, 4, 4, 5, 2 ] ), 
  Transformation( [ 6, 2, 5, 5, 1, 6 ] ), 
  Transformation( [ 2, 1, 4, 4, 6, 2 ] ), 
  Transformation( [ 3, 5, 4, 4, 2, 3 ] ), 
  Transformation( [ 3, 1, 4, 4, 5, 3 ] ), 
  Transformation( [ 1, 3, 5, 5, 6, 1 ] ), 
  Transformation( [ 2, 3, 5, 5, 6, 2 ] ), 
  Transformation( [ 2, 1, 4, 4, 3, 2 ] ), 
  Transformation( [ 6, 6, 6, 6, 5, 5 ] ), 
  Transformation( [ 4, 4, 4, 4, 6, 6 ] ), 
  Transformation( [ 3, 3, 3, 3, 6, 6 ] ), 
  Transformation( [ 5, 5, 5, 5, 4, 4 ] ), 
  Transformation( [ 2, 2, 2, 2, 4, 4 ] ), 
  Transformation( [ 2, 2, 2, 2, 1, 1 ] ), 
  Transformation( [ 6, 6, 6, 6, 2, 2 ] ), 
  Transformation( [ 3, 3, 3, 3, 4, 4 ] ), 
  Transformation( [ 3, 3, 3, 3, 1, 1 ] ), 
  Transformation( [ 1, 1, 1, 1, 6, 6 ] ), 
  Transformation( [ 5, 5, 5, 5, 3, 3 ] ), 
  Transformation( [ 4, 4, 4, 4, 1, 1 ] ), 
  Transformation( [ 2, 2, 2, 2, 5, 5 ] ), 
  Transformation( [ 1, 1, 1, 1, 5, 5 ] ), 
  Transformation( [ 3, 3, 3, 3, 2, 2 ] ) ]

# R-classes/reps, 1/1
gap> S := OrderEndomorphisms(5);;
gap> S := Semigroup(S, rec(acting := true));
<transformation monoid of degree 5 with 5 generators>
gap> RClasses(S);
[ <Green's R-class: IdentityTransformation>, 
  <Green's R-class: Transformation( [ 1, 1, 2, 3, 4 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 2, 3, 4 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 3, 3, 4 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 3, 4, 4 ] )>, 
  <Green's R-class: Transformation( [ 3, 3, 3 ] )>, 
  <Green's R-class: Transformation( [ 3, 3, 4, 4 ] )>, 
  <Green's R-class: Transformation( [ 3, 3, 4, 5, 5 ] )>, 
  <Green's R-class: Transformation( [ 3, 4, 4, 5, 5 ] )>, 
  <Green's R-class: Transformation( [ 3, 4, 4, 4 ] )>, 
  <Green's R-class: Transformation( [ 3, 4, 5, 5, 5 ] )>, 
  <Green's R-class: Transformation( [ 2, 2, 2, 2 ] )>, 
  <Green's R-class: Transformation( [ 2, 2, 2, 5, 5 ] )>, 
  <Green's R-class: Transformation( [ 2, 2, 5, 5, 5 ] )>, 
  <Green's R-class: Transformation( [ 2, 5, 5, 5, 5 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 1, 1, 1 ] )> ]
gap> RClassReps(S);
[ IdentityTransformation, Transformation( [ 1, 1, 2, 3, 4 ] ), 
  Transformation( [ 1, 2, 2, 3, 4 ] ), Transformation( [ 1, 2, 3, 3, 4 ] ), 
  Transformation( [ 1, 2, 3, 4, 4 ] ), Transformation( [ 3, 3, 3 ] ), 
  Transformation( [ 3, 3, 4, 4 ] ), Transformation( [ 3, 3, 4, 5, 5 ] ), 
  Transformation( [ 3, 4, 4, 5, 5 ] ), Transformation( [ 3, 4, 4, 4 ] ), 
  Transformation( [ 3, 4, 5, 5, 5 ] ), Transformation( [ 2, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 5, 5 ] ), Transformation( [ 2, 2, 5, 5, 5 ] ), 
  Transformation( [ 2, 5, 5, 5, 5 ] ), Transformation( [ 1, 1, 1, 1, 1 ] ) ]

# H-classes/reps, 1/3
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
>   rec(acting := true));;
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
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 4, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 2, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 2, 2, 2, 2, 4 ] )>, 
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
  <Green's H-class: Transformation( [ 3, 2, 2, 2, 2, 2, 2, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 2, 2, 2, 2, 2, 2, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 2, 2, 2, 2, 4, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 9, 4, 2, 4, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 9, 4, 2, 4, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 9, 2, 2, 2, 2, 2, 2, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 9, 2, 2, 2, 2, 2, 2, 2, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 9, 2, 2, 2, 2, 2, 4, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 2, 9, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 2, 9, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 4, 9, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 2, 9, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 2, 9, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 4, 9, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 2, 4 ] )> ]
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

# H-classes/reps, 2/3
gap> S := Semigroup(FullTransformationMonoid(5), rec(acting := true));;
gap> x := Transformation([1, 1, 2, 3, 4]);;
gap> L := LClass(S, x);;
gap> GreensHClasses(L);
[ <Green's H-class: Transformation( [ 1, 1, 2, 3, 4 ] )>, 
  <Green's H-class: Transformation( [ 1, 2, 3, 4, 1 ] )>, 
  <Green's H-class: Transformation( [ 2, 3, 4, 1, 1 ] )>, 
  <Green's H-class: Transformation( [ 3, 4, 1, 1, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 1, 1, 2, 3 ] )>, 
  <Green's H-class: Transformation( [ 1, 4, 1, 2, 3 ] )>, 
  <Green's H-class: Transformation( [ 2, 1, 3, 4, 1 ] )>, 
  <Green's H-class: Transformation( [ 1, 3, 4, 1, 2 ] )>, 
  <Green's H-class: Transformation( [ 3, 4, 1, 2, 1 ] )>, 
  <Green's H-class: Transformation( [ 3, 1, 4, 1, 2 ] )> ]

# NrXClasses, 1/1
gap> S := Semigroup(SymmetricInverseMonoid(5));;
gap> NrRClasses(S);
32
gap> NrDClasses(S);
6
gap> NrLClasses(S);
32
gap> NrHClasses(S);
252

# GroupHClass, IsGroupHClass, IsomorphismPermGroup, 1/1
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(4));;
gap> S := Semigroup(S, rec(acting := true));;
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
gap> x := IsomorphismPermGroup(H);;
gap> Source(x) = H;
true
gap> GeneratorsOfGroup(Range(x));
[ (5,9)(6,10)(7,11)(8,12), (2,9,5,3)(4,10,13,7)(6,11)(8,12,14,15) ]
gap> IsomorphismPermGroup(HClass(S, S.1));
Error, the argument (a Green's H-class) is not a group

# PartialOrderOfDClasses, 1/2
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(3));;
gap> S := Semigroup(S, rec(acting := true));;
gap> PartialOrderOfDClasses(S);
<immutable digraph with 11 vertices, 25 edges>

# PartialOrderOfDClasses, 2/2
gap> S := Semigroup([Transformation([2, 3, 6, 5, 4, 8, 10, 4, 1, 4]),
>  Transformation([10, 2, 5, 4, 10, 3, 1, 6, 9, 6])],
> rec(acting := true));;
gap> PartialOrderOfDClasses(S);
<immutable digraph with 201 vertices, 918 edges>

# Idempotents, 1/?
gap> S := AsSemigroup(IsTransformationSemigroup, FullPBRMonoid(1));;
gap> S := Semigroup(S, rec(acting := true));;
gap> Idempotents(S);
[ Transformation( [ 1, 8, 6, 1, 1, 6, 1, 8, 13, 8, 6, 6, 13, 8, 13, 13 ] ), 
  Transformation( [ 1, 8, 6, 8, 8, 6, 1, 8, 13, 8, 6, 6, 13, 8, 13, 13 ] ), 
  Transformation( [ 1, 2, 3, 2, 10, 6, 7, 8, 9, 10 ] ), 
  Transformation( [ 6, 9, 3, 3, 3, 6, 6, 13, 9, 9, 3, 6, 13, 13, 9, 13 ] ), 
  Transformation( [ 6, 9, 3, 9, 9, 6, 6, 13, 9, 9, 3, 6, 13, 13, 9, 13 ] ), 
  IdentityTransformation, Transformation( [ 7, 10, 11, 5, 5, 12, 7, 14, 15,
      10, 11, 12, 16, 14, 15, 16 ] ), 
  Transformation( [ 6, 13, 6, 6, 6, 6, 6, 13, 13, 13, 6, 6, 13, 13, 13, 13 ] )
    , Transformation( [ 6, 13, 6, 13, 13, 6, 6, 13, 13, 13, 6, 6, 13, 13, 13,
      13 ] ), Transformation( [ 7, 14, 12, 7, 7, 12, 7, 14, 16, 14, 12, 12,
      16, 14, 16, 16 ] ), Transformation( [ 7, 14, 12, 14, 14, 12, 7, 14, 16,
      14, 12, 12, 16, 14, 16, 16 ] ), 
  Transformation( [ 7, 10, 11, 10, 10, 12, 7, 14, 15, 10, 11, 12, 16, 14, 15,
      16 ] ), Transformation( [ 12, 15, 11, 11, 11, 12, 12, 16, 15, 15, 11,
      12, 16, 16, 15, 16 ] ), 
  Transformation( [ 12, 15, 11, 15, 15, 12, 12, 16, 15, 15, 11, 12, 16, 16,
      15, 16 ] ), Transformation( [ 12, 16, 12, 12, 12, 12, 12, 16, 16, 16,
      12, 12, 16, 16, 16, 16 ] ), 
  Transformation( [ 12, 16, 12, 16, 16, 12, 12, 16, 16, 16, 12, 12, 16, 16,
      16, 16 ] ) ]

# Idempotents, 2/2
gap> S := Semigroup(FullTransformationMonoid(3),
> rec(acting := true));;
gap> RClasses(S);;
gap> Idempotents(S);
[ IdentityTransformation, Transformation( [ 1, 2, 1 ] ), 
  Transformation( [ 3, 2, 3 ] ), Transformation( [ 1, 2, 2 ] ), 
  Transformation( [ 1, 3, 3 ] ), Transformation( [ 2, 2 ] ), 
  Transformation( [ 1, 1 ] ), Transformation( [ 1, 1, 1 ] ), 
  Transformation( [ 2, 2, 2 ] ), Transformation( [ 3, 3, 3 ] ) ]

# Idempotents, for given rank, 1/4
gap> S := Semigroup(DualSymmetricInverseMonoid(3), rec(acting := true));;
gap> Idempotents(S, -1);
Error, the 2nd argument (an integer) is not non-negative

# Idempotents, for given rank, 2/4
gap> S := Semigroup(DualSymmetricInverseMonoid(3), rec(acting := true));;
gap> Idempotents(S, 4);
[  ]

# Idempotents, for given rank, 3/4
gap> S := Semigroup(DualSymmetricInverseMonoid(3), rec(acting := true));;
gap> Idempotents(S);;
gap> Idempotents(S, 2);
[ <block bijection: [ 1, 2, -1, -2 ], [ 3, -3 ]>, 
  <block bijection: [ 1, -1 ], [ 2, 3, -2, -3 ]>, 
  <block bijection: [ 1, 3, -1, -3 ], [ 2, -2 ]> ]

# Idempotents, for given rank, 4/4
gap> S := Semigroup(DualSymmetricInverseMonoid(3), rec(acting := true));;
gap> Idempotents(S, 2);
[ <block bijection: [ 1, 2, -1, -2 ], [ 3, -3 ]>, 
  <block bijection: [ 1, -1 ], [ 2, 3, -2, -3 ]>, 
  <block bijection: [ 1, 3, -1, -3 ], [ 2, -2 ]> ]

# Idempotents, for a D-class, 1/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> IsRegularDClass(D);
false
gap> Idempotents(D);
[  ]

# Idempotents, for a D-class, 2/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> Idempotents(D);
[  ]

# Idempotents, for a L-class, 1/3
gap> S := Semigroup(FullTransformationMonoid(5), rec(acting := true));;
gap> x := Transformation([1, 1, 2, 3, 4]);;
gap> L := LClass(S, x);;
gap> Idempotents(L);
[ Transformation( [ 1, 2, 3, 4, 1 ] ), Transformation( [ 1, 2, 3, 4, 4 ] ), 
  Transformation( [ 1, 2, 3, 4, 2 ] ), Transformation( [ 1, 2, 3, 4, 3 ] ) ]

# Idempotents, for a L-class, 2/3
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> L := LClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsRegularGreensClass(L);
false
gap> Idempotents(L);
[  ]

# Idempotents, for a L-class, 3/3
gap> S := PartitionMonoid(3);;
gap> L := LClass(S, One(S));;
gap> Idempotents(L);
[ <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ]> ]

# Idempotents, for a H-class, 1/2
gap> S := SingularTransformationSemigroup(4);;
gap> H := HClass(S, S.1);
<Green's H-class: Transformation( [ 1, 2, 3, 3 ] )>
gap> Idempotents(H);
[ Transformation( [ 1, 2, 3, 3 ] ) ]

# Idempotents, for a H-class, 1/2
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> H := HClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsGroupHClass(H);
false
gap> Idempotents(H);
[  ]

# NrIdempotents, for a semigroup, 1/2
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> NrIdempotents(S);
24

# NrIdempotents, for a semigroup, 2/2
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> Idempotents(S);;
gap> NrIdempotents(S);
24

# NrIdempotents, for a D-class, 1/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> IsRegularDClass(D);
false
gap> NrIdempotents(D);
0

# NrIdempotents, for a D-class, 2/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> NrIdempotents(D);
0

# NrIdempotents, for a L-class, 1/3
gap> S := Semigroup(FullTransformationMonoid(5), rec(acting := true));;
gap> x := Transformation([1, 1, 2, 3, 4]);;
gap> L := LClass(S, x);;
gap> NrIdempotents(L);
4

# NrIdempotents, for a L-class, 2/3
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> L := LClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsRegularGreensClass(L);
false
gap> NrIdempotents(L);
0

# NrIdempotents, for a L-class, 3/3
gap> S := PartitionMonoid(3);;
gap> L := LClass(S, One(S));;
gap> NrIdempotents(L);
1

# NrIdempotents, for a H-class, 1/2
gap> S := SingularTransformationSemigroup(4);;
gap> H := HClass(S, S.1);
<Green's H-class: Transformation( [ 1, 2, 3, 3 ] )>
gap> NrIdempotents(H);
1

# NrIdempotents, for a H-class, 1/2
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> H := HClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsGroupHClass(H);
false
gap> NrIdempotents(H);
0

# NrIdempotents, for an R-class, 1/2
gap> S := Semigroup(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                    Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> R := First(RClasses(S),
> x -> Transformation([9, 10, 4, 9, 10, 4, 4, 3, 3, 6]) in x);;
gap> NrIdempotents(R);
0
gap> IsRegularGreensClass(R);
false

# NrIdempotents, for an R-class, 3/3
gap> S := Semigroup(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                    Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> R := RClass(S, Transformation([6, 9, 9, 6, 9, 1, 1, 2, 2, 6]));;
gap> IsRegularGreensClass(R);
true
gap> NrIdempotents(R);
7

# IsRegularGreensClass, for an R-class, 1/1
gap> S := Semigroup(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                    Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> R := First(RClasses(S),
> x -> Transformation([9, 10, 4, 9, 10, 4, 4, 3, 3, 6]) in x);;
gap> IsRegularGreensClass(R);
false

# IsRegularGreensClass, for an R-class in group of units, 1/1
gap> S := Monoid(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                 Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> S := AsSemigroup(IsBipartitionSemigroup, S);;
gap> R := RClass(S, IdentityBipartition(10));;
gap> IsRegularGreensClass(R);
true

# NrRegularDClasses, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(acting := true));;
gap> NrRegularDClasses(S);
6

# Enumerator/IteratorOfR/DClasses, 1/1
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
gap> S := Semigroup(S, rec(acting := true));;
gap> iter := IteratorOfRClasses(S);
<iterator>
gap> iter := IteratorOfDClasses(S);
<iterator>
gap> S := Semigroup(S, rec(acting := true));;
gap> iter := IteratorOfRClasses(S);
<iterator>
gap> iter := IteratorOfDClasses(S);
<iterator>
gap> for x in iter do od;
gap> S := Semigroup(S, rec(acting := true));;
gap> RClasses(S);;
gap> iter := IteratorOfRClasses(S);
<iterator>
gap> for x in iter do od;
gap> Size(S);
3175
gap> iter := IteratorOfDClasses(S);
<iterator>
gap> for x in iter do od;

# RhoCosets, 1/3
gap> S := Semigroup([Transformation([1, 3, 7, 5, 5, 7, 1]),
>  Transformation([5, 3, 5, 3, 4, 1, 2]),
>  Transformation([6, 1, 6, 5, 5, 2]),
>  Transformation([6, 1, 7, 6, 3, 2, 4]),
>  Transformation([6, 2, 1, 4, 6, 4, 3]),
>  Transformation([7, 3, 3, 6, 4, 5, 5])], rec(acting := true));;
gap> L := LClass(S, Transformation([1, 5, 1, 4, 4, 3, 2]));;
gap> RhoCosets(L);
<enumerator of perm group>

# RhoCosets, 2/3
gap> S := Semigroup([Transformation([1, 4, 2, 5, 2, 1, 6, 1, 7, 6]),
>  Transformation([2, 8, 4, 7, 5, 8, 3, 5, 8, 6])],
> rec(acting := true));;
gap> L := LClass(S, Transformation([4, 6, 6, 1, 6, 4, 6, 4, 1, 6]));
<Green's L-class: Transformation( [ 4, 6, 6, 1, 6, 4, 6, 4, 1, 6 ] )>
gap> RhoCosets(L);
[ (), (4,6) ]

# RhoCosets, 3/3
gap> S := Semigroup([Transformation([1, 4, 2, 5, 2, 1, 6, 1, 7, 6]),
>  Transformation([2, 8, 4, 7, 5, 8, 3, 5, 8, 6])],
> rec(acting := true));;
gap> L := LClass(S, Transformation([7, 8, 8, 2, 8, 7, 8, 7, 2, 8]));;
gap> RhoCosets(L);
RightTransversal(Group([ (2,8,7), (7,8) ]),Group([ (2,7,8) ]))

# SemigroupDataIndex, 1/1
gap> S := Semigroup([Transformation([1, 4, 2, 5, 2, 1, 6, 1, 7, 6]),
>  Transformation([2, 8, 4, 7, 5, 8, 3, 5, 8, 6])],
> rec(acting := true));;
gap> L := LClass(S, Transformation([7, 8, 8, 2, 8, 7, 8, 7, 2, 8]));;
gap> HasSemigroupDataIndex(L);
false
gap> SemigroupDataIndex(L);
118

# SchutzenbergerGroup, for a D-class, 1/1
gap> S := Semigroup([Transformation([3, 1, 4, 2, 5, 2, 1, 6, 1, 7]),
>   Transformation([6, 2, 8, 4, 7, 5, 8, 3, 5, 8])],
> rec(acting := true));;
gap> D := DClass(S, Transformation([2, 1, 4, 2, 1, 6, 4, 4, 6, 4]));;
gap> SchutzenbergerGroup(D);
Group([ (2,7,8,4) ])

# SchutzenbergerGroup, for a H-class, 1/1
gap> S := Semigroup([Transformation([7, 1, 4, 3, 2, 7, 7, 6, 6, 5]),
>  Transformation([7, 10, 10, 1, 7, 9, 10, 4, 2, 10])],
>  rec(acting := true));;
gap> H := HClass(S,
> Transformation([10, 10, 10, 10, 10, 10, 10, 7, 10, 10]));;
gap> SchutzenbergerGroup(H);
Group(())

# Test < for Green's classes
gap> S := Semigroup(Transformation([2, 4, 3, 4]), 
>                   Transformation([3, 3, 2, 3]));;
gap> IsRegularSemigroup(S);
false
gap> Set([DClass(S, S.2) < DClass(S, S.1), DClass(S, S.1) < DClass(S, S.2)]);
[ true, false ]
gap> DClass(S, S.1) < DClass(S, S.1);
false
gap> Set([RClass(S, S.2) < RClass(S, S.1), RClass(S, S.1) < RClass(S, S.2)]);
[ true, false ]
gap> RClass(S, S.1) < RClass(S, S.1);
false
gap> T := Semigroup(Transformation([2, 4, 3, 4]));;
gap> RClass(S, S.2) < RClass(T, T.1);
false
gap> DClass(S, S.2) < DClass(T, T.1);
false

# BruteForceIsoCheck helper functions
gap> BruteForceIsoCheck := function(iso)
>   local x, y;
>   if not IsInjective(iso) or not IsSurjective(iso) then
>     return false;
>   fi;
>   for x in Generators(Source(iso)) do
>     for y in Generators(Source(iso)) do
>       if x ^ iso * y ^ iso <> (x * y) ^ iso then
>         return false;
>       fi;
>     od;
>   od;
>   return true;
> end;;
gap> BruteForceInverseCheck := function(map)
> local inv;
>   inv := InverseGeneralMapping(map);
>   return ForAll(Source(map), x -> x = (x ^ map) ^ inv)
>     and ForAll(Range(map), x -> x = (x ^ inv) ^ map);
> end;;

# Issue #637
gap> H := HClass(FullTransformationMonoid(4), Transformation([2, 2, 2]));
<Green's H-class: Transformation( [ 2, 2, 2 ] )>
gap> map := IsomorphismPermGroup(H);;
gap> Transformation([2, 2, 2, 2]) ^ map;
Error, the argument does not belong to the domain of the function
gap> map := InverseGeneralMapping(map);;
gap> (1, 3) ^ map;
Error, the argument does not belong to the domain of the function
gap> BruteForceIsoCheck(map);
true

# Second part of Issue #637
gap> H := HClass(FullTransformationMonoid(4), Transformation([3, 1, 2, 3]));;
gap> map := IsomorphismPermGroup(H);;
gap> BruteForceIsoCheck := function(iso)
>     local x, y;
>     if not IsInjective(iso) or not IsSurjective(iso) then
>         return false;
>     fi;
>     for x in Source(iso) do
>         for y in Source(iso) do
>             if
>              not ImageElm(iso, x) * ImageElm(iso, y)
>                    = ImageElm(iso, x * y) then
>                 Print(x, " ", y, "\n");
>                 return false;
>             fi;
>         od;
>     od;
>     return true;
> end;;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# RhoOrbStabChain for an L-class of a matrix semigroup
gap> S := GLM(4, 4); 
<general linear monoid 4x4 over GF(2^2)>
gap> x := Matrix(GF(2 ^ 2), [[0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2)], 
>                            [Z(2 ^ 2) ^ 2, Z(2 ^ 2) ^ 2, Z(2) ^ 0, Z(2) ^ 0],
>                            [0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2 ^ 2)], 
>                            [0 * Z(2), Z(2 ^ 2) ^ 2, Z(2 ^ 2) ^ 2, Z(2 ^ 2) ^ 2]]);
[ [ 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2) ], [ Z(2^2)^2, Z(2^2)^2, Z(2)^0, Z(2)^0 ], 
  [ 0*Z(2), Z(2)^0, 0*Z(2), Z(2^2) ], 
  [ 0*Z(2), Z(2^2)^2, Z(2^2)^2, Z(2^2)^2 ] ]
gap> L := LClass(S, x);
<Green's L-class: <matrix object of dimensions 4x4 over GF(2^2)>>
gap> RhoOrbStabChain(L);
<matrix group of size 2961100800 with 4 generators>

# SchutzenbergerGroup for a D-class of a matrix semigroup
gap> S := GLM(4, 4); 
<general linear monoid 4x4 over GF(2^2)>
gap> x := Matrix(GF(2 ^ 2), [[0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2)], [Z(2 ^
> 2) ^ 2, Z(2 ^ 2) ^ 2, Z(2) ^ 0, Z(2) ^ 0], [0 * Z(2), Z(2) ^ 0, 0 * Z(2),
> Z(2 ^ 2)], [0 * Z(2), Z(2 ^ 2) ^ 2, Z(2 ^ 2) ^ 2, Z(2 ^ 2) ^ 2]]);
[ [ 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2) ], [ Z(2^2)^2, Z(2^2)^2, Z(2)^0, Z(2)^0 ], 
  [ 0*Z(2), Z(2)^0, 0*Z(2), Z(2^2) ], 
  [ 0*Z(2), Z(2^2)^2, Z(2^2)^2, Z(2^2)^2 ] ]
gap> D := DClass(S, x);
<Green's D-class: <matrix object of dimensions 4x4 over GF(2^2)>>
gap> SchutzenbergerGroup(D);
<matrix group of size 2961100800 with 4 generators>
gap> S := Semigroup([Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [Z(3), Z(3) ^ 0]]),
>  Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [0 * Z(3), 0 * Z(3)]])],
> rec(acting := true));
<semigroup of 2x2 matrices over GF(3) with 2 generators>
gap> x := Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [Z(3), Z(3) ^ 0]]);
[ [ Z(3)^0, Z(3)^0 ], [ Z(3), Z(3)^0 ] ]
gap> D := DClass(S, x);
<Green's D-class: <matrix object of dimensions 2x2 over GF(3)>>
gap> SchutzenbergerGroup(D);
Group(
[ 
  [ [ Z(3)^0, 0*Z(3), 0*Z(3) ], [ 0*Z(3), Z(3)^0, 0*Z(3) ], 
      [ 0*Z(3), 0*Z(3), Z(3)^0 ] ], 
  [ [ Z(3)^0, Z(3)^0, 0*Z(3) ], [ Z(3), Z(3)^0, 0*Z(3) ], 
      [ 0*Z(3), 0*Z(3), Z(3)^0 ] ] ])

# IsomorphismPermGroup for a group H-class of a matrix semigroup
gap> S := Semigroup([Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [Z(3), Z(3) ^ 0]]),
>  Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [0 * Z(3), 0 * Z(3)]])]);
<semigroup of 2x2 matrices over GF(3) with 2 generators>
gap> x := Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [Z(3), Z(3) ^ 0]]);
[ [ Z(3)^0, Z(3)^0 ], [ Z(3), Z(3)^0 ] ]
gap> D := DClass(S, x);
<Green's D-class: <matrix object of dimensions 2x2 over GF(3)>>
gap> BruteForceIsoCheck(IsomorphismPermGroup(GroupHClass(D)));
true
gap> BruteForceInverseCheck(IsomorphismPermGroup(GroupHClass(D)));
true

# Enumerator for a D-class of an acting semigroup
gap> S := Semigroup([Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [Z(3), Z(3) ^ 0]]),
>  Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [0 * Z(3), 0 * Z(3)]])], rec(acting :=
> true));;
gap> x := Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [0 * Z(3), 0 * Z(3)]]);;
gap> D := DClass(S, x);;
gap> en := Enumerator(D);
<enumerator of <Green's D-class: <matrix object of dimensions 2x2 over GF(3)>>
 >
gap> NrRClasses(D);
4
gap> ForAll(en, x -> en[Position(en, x)] = x);
true
gap> ForAll([1 .. Length(en)], i -> Position(en, en[i]) = i);
true
gap> Position(en, Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [Z(3), Z(3) ^ 0]])); 
fail
gap> en[10000];
fail

# Iterator for a D-class of an acting semigroup
gap> S := Semigroup([Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [Z(3), Z(3) ^ 0]]),
>  Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [0 * Z(3), 0 * Z(3)]])]);;
gap> x := Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [0 * Z(3), 0 * Z(3)]]);;
gap> D := DClass(S, x);;
gap> it := Iterator(D);
<iterator>
gap> nr := 0;
0
gap> for x in it do nr := nr + 1; od;
gap> nr = Size(D);
true
gap> D := DClass(S, x);;
gap> AsList(D);;
gap> it := Iterator(D);
<iterator>
gap> nr := 0;
0
gap> for x in it do nr := nr + 1; od;
gap> nr = Size(D);
true
gap> D := DClass(S, x);;
gap> AsSSortedList(D);
[ [ [ 0*Z(3), 0*Z(3) ], [ 0*Z(3), Z(3)^0 ] ], 
  [ [ 0*Z(3), 0*Z(3) ], [ 0*Z(3), Z(3) ] ], 
  [ [ 0*Z(3), 0*Z(3) ], [ Z(3)^0, 0*Z(3) ] ], 
  [ [ 0*Z(3), 0*Z(3) ], [ Z(3)^0, Z(3)^0 ] ], 
  [ [ 0*Z(3), 0*Z(3) ], [ Z(3)^0, Z(3) ] ], 
  [ [ 0*Z(3), 0*Z(3) ], [ Z(3), 0*Z(3) ] ], 
  [ [ 0*Z(3), 0*Z(3) ], [ Z(3), Z(3)^0 ] ], 
  [ [ 0*Z(3), 0*Z(3) ], [ Z(3), Z(3) ] ], 
  [ [ 0*Z(3), Z(3)^0 ], [ 0*Z(3), 0*Z(3) ] ], 
  [ [ 0*Z(3), Z(3)^0 ], [ 0*Z(3), Z(3)^0 ] ], 
  [ [ 0*Z(3), Z(3)^0 ], [ 0*Z(3), Z(3) ] ], 
  [ [ 0*Z(3), Z(3) ], [ 0*Z(3), 0*Z(3) ] ], 
  [ [ 0*Z(3), Z(3) ], [ 0*Z(3), Z(3)^0 ] ], 
  [ [ 0*Z(3), Z(3) ], [ 0*Z(3), Z(3) ] ], 
  [ [ Z(3)^0, 0*Z(3) ], [ 0*Z(3), 0*Z(3) ] ], 
  [ [ Z(3)^0, 0*Z(3) ], [ Z(3)^0, 0*Z(3) ] ], 
  [ [ Z(3)^0, 0*Z(3) ], [ Z(3), 0*Z(3) ] ], 
  [ [ Z(3)^0, Z(3)^0 ], [ 0*Z(3), 0*Z(3) ] ], 
  [ [ Z(3)^0, Z(3)^0 ], [ Z(3)^0, Z(3)^0 ] ], 
  [ [ Z(3)^0, Z(3)^0 ], [ Z(3), Z(3) ] ], 
  [ [ Z(3)^0, Z(3) ], [ 0*Z(3), 0*Z(3) ] ], 
  [ [ Z(3)^0, Z(3) ], [ Z(3)^0, Z(3) ] ], 
  [ [ Z(3)^0, Z(3) ], [ Z(3), Z(3)^0 ] ], 
  [ [ Z(3), 0*Z(3) ], [ 0*Z(3), 0*Z(3) ] ], 
  [ [ Z(3), 0*Z(3) ], [ Z(3)^0, 0*Z(3) ] ], 
  [ [ Z(3), 0*Z(3) ], [ Z(3), 0*Z(3) ] ], 
  [ [ Z(3), Z(3)^0 ], [ 0*Z(3), 0*Z(3) ] ], 
  [ [ Z(3), Z(3)^0 ], [ Z(3)^0, Z(3) ] ], 
  [ [ Z(3), Z(3)^0 ], [ Z(3), Z(3)^0 ] ], 
  [ [ Z(3), Z(3) ], [ 0*Z(3), 0*Z(3) ] ], 
  [ [ Z(3), Z(3) ], [ Z(3)^0, Z(3)^0 ] ], [ [ Z(3), Z(3) ], [ Z(3), Z(3) ] ] ]
gap> it := Iterator(D);
<iterator>
gap> nr := 0;
0
gap> for x in it do nr := nr + 1; od;
gap> nr = Size(D);
true

# Enumerator/Iterator for an H-class of an acting semigroup
gap> S := Semigroup([Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [Z(3), Z(3) ^ 0]]),
>  Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [0 * Z(3), 0 * Z(3)]])],
> rec(acting := true));;
gap> x := Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [0 * Z(3), 0 * Z(3)]]);;
gap> H := HClass(S, x);;
gap> en := Enumerator(H);
<enumerator of <Green's H-class: <matrix object of dimensions 2x2 over GF(3)>>
 >
gap> ForAll(en, x -> en[Position(en, x)] = x);
true
gap> ForAll([1 .. Length(en)], i -> Position(en, en[i]) = i);
true
gap> Position(en, Matrix(GF(3), [[0 * Z(3), Z(3)], [0 * Z(3), Z(3) ^ 0]]));
fail
gap> en[10000];
fail
gap> AsSSortedList(H);
[ [ [ Z(3)^0, Z(3)^0 ], [ 0*Z(3), 0*Z(3) ] ], 
  [ [ Z(3), Z(3) ], [ 0*Z(3), 0*Z(3) ] ] ]
gap> nr := 0;
0
gap> for x in Iterator(H) do nr := nr + 1; od;
gap> nr = Size(H);
true

# Enumerator/Iterator for an L-class of an acting semigroup
gap> S := Semigroup([Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [Z(3), Z(3) ^ 0]]),
>  Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [0 * Z(3), 0 * Z(3)]])],
> rec(acting := true));;
gap> x := Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [0 * Z(3), 0 * Z(3)]]);;
gap> L := LClass(S, x);;
gap> en := Enumerator(L);
<enumerator of <Green's L-class: <matrix object of dimensions 2x2 over GF(3)>>
 >
gap> ForAll(en, x -> en[Position(en, x)] = x);
true
gap> ForAll([1 .. Length(en)], i -> Position(en, en[i]) = i);
true
gap> Position(en, Matrix(GF(3), [[0 * Z(3), Z(3)], [0 * Z(3), Z(3) ^ 0]]));
fail
gap> en[10000];
fail
gap> AsSSortedList(L);
[ [ [ 0*Z(3), 0*Z(3) ], [ Z(3)^0, Z(3)^0 ] ], 
  [ [ 0*Z(3), 0*Z(3) ], [ Z(3), Z(3) ] ], 
  [ [ Z(3)^0, Z(3)^0 ], [ 0*Z(3), 0*Z(3) ] ], 
  [ [ Z(3)^0, Z(3)^0 ], [ Z(3)^0, Z(3)^0 ] ], 
  [ [ Z(3)^0, Z(3)^0 ], [ Z(3), Z(3) ] ], 
  [ [ Z(3), Z(3) ], [ 0*Z(3), 0*Z(3) ] ], 
  [ [ Z(3), Z(3) ], [ Z(3)^0, Z(3)^0 ] ], [ [ Z(3), Z(3) ], [ Z(3), Z(3) ] ] ]
gap> nr := 0;
0
gap> for x in Iterator(L) do nr := nr + 1; od;
gap> nr = Size(L);
true
gap> S := Semigroup([Matrix(GF(3), [[0 * Z(3), 0 * Z(3), 0 * Z(3)], [Z(3) ^ 0,
> Z(3) ^ 0, Z(3) ^ 0], [Z(3), Z(3) ^ 0, 0 * Z(3)]]),
>  Matrix(GF(3), [[0 * Z(3), Z(3), 0 * Z(3)], [0 * Z(3), Z(3), Z(3) ^ 0], [0 *
> Z(3), Z(3), 0 * Z(3)]]),
>  Matrix(GF(3), [[Z(3) ^ 0, Z(3), Z(3) ^ 0], [Z(3) ^ 0, 0 * Z(3), Z(3)],
> [Z(3), 0 * Z(3), 0 * Z(3)]])],
> rec(acting := true));
<semigroup of 3x3 matrices over GF(3) with 3 generators>
gap> x := Matrix(GF(3), [[0 * Z(3), 0 * Z(3), 0 * Z(3)], [0 * Z(3), Z(3) ^ 0, Z(3)], 
> [0 * Z(3), Z(3) ^ 0, Z(3)]]);
[ [ 0*Z(3), 0*Z(3), 0*Z(3) ], [ 0*Z(3), Z(3)^0, Z(3) ], 
  [ 0*Z(3), Z(3)^0, Z(3) ] ]
gap> L := LClass(S, x);;
gap> en := Enumerator(L);
<enumerator of <Green's L-class: <matrix object of dimensions 3x3 over GF(3)>>
 >
gap> Position(en, Matrix(GF(3), [[0 * Z(3), Z(3) ^ 0, Z(3)], [0 * Z(3), Z(3),
> Z(3) ^ 0], [0 * Z(3), Z(3), Z(3) ^ 0]]));
fail

# Enumerator/Iterator for an R-Class
gap> S := Semigroup([Matrix(GF(3), [[0 * Z(3), 0 * Z(3), 0 * Z(3)], [Z(3) ^ 0,
> Z(3) ^ 0, Z(3) ^ 0], [Z(3), Z(3) ^ 0, 0 * Z(3)]]),
>  Matrix(GF(3), [[0 * Z(3), Z(3), 0 * Z(3)], [0 * Z(3), Z(3), Z(3) ^ 0], [0 *
> Z(3), Z(3), 0 * Z(3)]]),
>  Matrix(GF(3), [[Z(3) ^ 0, Z(3), Z(3) ^ 0], [Z(3) ^ 0, 0 * Z(3), Z(3)],
> [Z(3),
> 0 * Z(3), 0 * Z(3)]])],
> rec(acting := true));
<semigroup of 3x3 matrices over GF(3) with 3 generators>
gap> x := Matrix(GF(3), [[0 * Z(3), 0 * Z(3), 0 * Z(3)],
>                        [0 * Z(3), Z(3) ^ 0, Z(3)],
>                        [0 * Z(3), Z(3) ^ 0, Z(3)]]);
[ [ 0*Z(3), 0*Z(3), 0*Z(3) ], [ 0*Z(3), Z(3)^0, Z(3) ], 
  [ 0*Z(3), Z(3)^0, Z(3) ] ]
gap> R := RClass(S, x);;
gap> en := Enumerator(R);
<enumerator of <Green's R-class: <matrix object of dimensions 3x3 over GF(3)>>
 >
gap> Position(en, Matrix(GF(3), [[0 * Z(3), 0 * Z(3), 0 * Z(3)], [Z(3), Z(3) ^ 0,
> Z(3) ^ 0], [Z(3), Z(3) ^ 0, Z(3) ^ 0]]));
fail
gap> Position(en, Matrix(GF(3), [[Z(3) ^ 0, 0 * Z(3), 0 * Z(3)], [0 * Z(3),
> Z(3) ^ 0, 0 * Z(3)], [0 * Z(3), 0 * Z(3), Z(3) ^ 0]]));
fail
gap> ForAll(en, x -> x in R);
true
gap> AsSSortedList(R);;
gap> nr := 0;
0
gap> for x in Iterator(R) do nr := nr + 1; od;
gap> nr = Size(R);
true

# Test function for LeftGreensMultiplier/NC
gap> CheckLeftGreensMultiplier1 := function(S)
>  local L, a, b;
>  for L in LClasses(S) do
>    for a in L do
>      for b in L do
>        if LeftGreensMultiplier(S, a, b) * a <> b then
>          return [a, b];
>        fi;
>      od;
>    od;
>  od;
> return true;
> end;;
gap> CheckLeftGreensMultiplier2 := function(S)
>  local L, a, b;
>  for L in LClasses(S) do
>    for a in L do
>      for b in L do
>        if Set(RClass(S, a), x -> LeftGreensMultiplier(S, a, b) * x) <> Set(RClass(S, b)) then
>           return [a, b];
>        fi;
>      od;
>    od;
>  od;
>  return true;
>  end;;
gap> CheckRightGreensMultiplier1 := function(S)
>  local R, a, b;
>  for R in RClasses(S) do
>    for a in R do
>      for b in R do
>        if a * RightGreensMultiplierNC(S, a, b) <> b then
>          return [a, b];
>        fi;
>      od;
>    od;
>  od;
> return true;
> end;;
gap> CheckRightGreensMultiplier2 := function(S)
>  local R, a, b;
>  for R in RClasses(S) do
>    for a in R do
>      for b in R do
>        if Set(LClass(S, a), x -> x * RightGreensMultiplierNC(S, a, b)) 
>          <> Set(LClass(S, b)) then
>           return [a, b];
>        fi;
>      od;
>    od;
>  od;
>  return true;
>  end;;
gap> S := Monoid(Transformation([1, 3, 2, 1]),
> Transformation([3, 3, 2]),
> Transformation([4, 2, 4, 3]));;
gap> CheckLeftGreensMultiplier1(S);
true
gap> CheckRightGreensMultiplier1(S);
true
gap> CheckLeftGreensMultiplier2(S);
true
gap> CheckRightGreensMultiplier2(S);
true
gap> S := Semigroup(Bipartition([[1, 2, 3, 4, 5, -1], [-2, -5], [-3, -4]]),
>                   Bipartition([[1, 2, 5, -5], [3, -1], [4, -2, -3, -4]]),
>                   Bipartition([[1, -4, -5], [2, 3, 4, 5], [-1, -2, -3]]),
>                   Bipartition([[1, 2, 3, -2], [4, -1], [5, -5], [-3, -4]]),
>                   Bipartition([[1, 2, 4, 5, -1, -2, -3, -5], [3], [-4]]));
<bipartition semigroup of degree 5 with 5 generators>
gap> CheckLeftGreensMultiplier1(S);
true
gap> CheckRightGreensMultiplier1(S);
true
gap> CheckLeftGreensMultiplier2(S);
true
gap> CheckRightGreensMultiplier2(S);
true
gap> S := Semigroup(PartialPerm([1, 2, 3], [4, 2, 1]), 
>                   PartialPerm([1, 2, 3], [2, 4, 1]), 
>                   PartialPerm([1, 2], [2, 3]), 
>                   PartialPerm([1, 2], [3, 4]));
<partial perm semigroup of rank 3 with 4 generators>
gap> CheckLeftGreensMultiplier1(S);
true
gap> CheckRightGreensMultiplier1(S);
true
gap> CheckLeftGreensMultiplier2(S);
true
gap> CheckRightGreensMultiplier2(S);
true

# Bug in RightGreensMultiplierNC
gap> S := Monoid(
> Bipartition([[1, 2, -6], [3, 4, -1], [5, -2], [6], [-3], [-4], [-5]]),
> Bipartition([[1, 2, 3, 5, -2, -6], [4, -5], [6, -3, -4], [-1]]),
> Bipartition([[1, 2, 6, -1], [3, -3], [4, 5, -6], [-2], [-4, -5]]),
> Bipartition([[1, 3, -4], [2, 4, 5, -1, -5], [6, -2, -3, -6]]),
> Bipartition([[1, 3, 4, 6, -5], [2, 5, -1, -2, -4], [-3, -6]]));
<bipartition monoid of degree 6 with 5 generators>
gap> d := Bipartition([[1, 3, -2], [2, 4, 5, 6, -1, -6], [-3], [-4], [-5]]);
<bipartition: [ 1, 3, -2 ], [ 2, 4, 5, 6, -1, -6 ], [ -3 ], [ -4 ], [ -5 ]>
gap> D := DClass(S, d);
<Green's D-class: <bipartition: [ 1, 3, -2 ], [ 2, 4, 5, 6, -1, -6 ], [ -3 ], 
  [ -4 ], [ -5 ]>>
gap> x := Bipartition([[1, 3, -1, -6], [2, 4, 5, 6, -2], [-3], [-4], [-5]]);
<bipartition: [ 1, 3, -1, -6 ], [ 2, 4, 5, 6, -2 ], [ -3 ], [ -4 ], [ -5 ]>
gap> H := HClass(S, x);
<Green's H-class: <bipartition: [ 1, 3, -1, -6 ], [ 2, 4, 5, 6, -2 ], [ -3 ], 
  [ -4 ], [ -5 ]>>
gap> h := Representative(H);
<bipartition: [ 1, 3, -1, -6 ], [ 2, 4, 5, 6, -2 ], [ -3 ], [ -4 ], [ -5 ]>
gap> r := RightGreensMultiplierNC(S, d, h);;
gap> d * r in D;
true

# This is a bit slow so commented out
# gap> S := Monoid(
# > Bipartition([[1, 2, -6], [3, 4, -1], [5, -2], [6], [-3], [-4], [-5]]),
# > Bipartition([[1, 2, 3, 5, -2, -6], [4, -5], [6, -3, -4], [-1]]),
# > Bipartition([[1, 2, 6, -1], [3, -3], [4, 5, -6], [-2], [-4, -5]]),
# > Bipartition([[1, 3, -4], [2, 4, 5, -1, -5], [6, -2, -3, -6]]),
# > Bipartition([[1, 3, 4, 6, -5], [2, 5, -1, -2, -4], [-3, -6]]));
# gap> CheckLeftGreensMultiplier1(S);
# true
# gap> CheckRightGreensMultiplier1(S);
# true
# gap> CheckLeftGreensMultiplier2(S);
# true
# gap> CheckRightGreensMultiplier2(S);
# true

# Fix bug in HClassReps(LClass) 
gap> T := Monoid(Transformation([1, 3, 2, 1]),
>                Transformation([3, 3, 2]), 
>                Transformation([4, 2, 4, 3])); 
<transformation monoid of degree 4 with 3 generators>
gap> D := DClasses(T)[4];;
gap> d := Representative(D);;
gap> mults := List(HClassReps(LClass(T, d)), h -> LeftGreensMultiplierNC(T, d, h));;
gap> Length(Set(mults, m -> RClass(T, m * d))) = Length(RClasses(D));
true

# Fix issue #931 (bug in RhoOrbStabChain(DClass) using the wrong representative
gap> S := Monoid(Transformation([3, 2, 3, 3, 5, 5]), 
>                Transformation([5, 4, 4, 5, 1, 4]), 
>                Transformation([1, 1, 5, 3, 3]), 
>                Transformation([4, 5, 6, 4, 1, 4]), 
>                Transformation([5, 1, 2, 1, 6, 5]));
<transformation monoid of degree 6 with 5 generators>
gap> e := Transformation([1, 6, 6, 1, 5, 6]);
Transformation( [ 1, 6, 6, 1, 5, 6 ] )
gap> c := Transformation([6, 1, 1, 6, 5, 1]);
Transformation( [ 6, 1, 1, 6, 5, 1 ] )
gap> c in DClass(S, e);
true
gap> c in HClass(S, e);
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/greens/acting.tst");
