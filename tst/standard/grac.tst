#############################################################################
##
#W  standard/grac.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/grac.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# grac: DClassOfLClass, 1/1
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

# grac: DClassOfRClass, 1/1
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

# grac: DClassOfHClass, 1/1
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

# grac: LClassOfHClass, 1/1
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

# grac: RClassOfHClass, 1/1
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

# grac: GreensDClassOfElement, fail, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(acting := true));;
gap> GreensDClassOfElement(S, PartialPerm([19]));
Error, Semigroups: GreensDClassOfElement: usage,
the element does not belong to the semigroup,

# grac: GreensDClassOfElementNC, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(acting := true));;
gap> D := GreensDClassOfElementNC(S, PartialPerm([19]));;
gap> Size(D);
5

# grac: GreensL/RClassOfElement, fail, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(acting := true));;
gap> RClass(S, ConstantTransformation(7, 7));
Error, Semigroups: GreensRClassOfElement: usage,
the element does not belong to the semigroup,
gap> LClass(S, ConstantTransformation(7, 7));
Error, Semigroups: GreensLClassOfElement: usage,
the element does not belong to the semigroup,
gap> HClass(S, ConstantTransformation(7, 7));
Error, Semigroups: GreensHClassOfElement: usage,
the element does not belong to the semigroup,

# grac: GreensL/RClassOfElementNC, fail, 1/1
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

# grac: GreensL/RClassOfElement, for a D-class, 1/1
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
Error, Semigroups: GreensLClassOfElement: usage,
the element does not belong to the D-class,
gap> RClass(D, IdentityBipartition(8));
Error, Semigroups: GreensRClassOfElement: usage,
the element does not belong to the D-class,
gap> x := Bipartition([[1, 4, -1, -2, -6], [2, 3, 5, -4],
> [6, -3], [-5]]);;
gap> LClassNC(D, x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
The 2nd argument is 'fail' which might point to an earlier problem
Error, no 1st choice method found for `IsBound[]' on 2 arguments
gap> RClassNC(D, x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
The 2nd argument is 'fail' which might point to an earlier problem
Error, no 1st choice method found for `IsBound[]' on 2 arguments

# grac: GreensClassOfElementNC(D-class, x) inverse-op, 1/1
gap> S := InverseSemigroup([
>  PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(acting := true));;
gap> D := DClass(S, S.3 * S.2);;
gap> Size(LClassNC(D, S.3 * S.2));
3

# grac: GreensHClassOfElement, 1/1
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
Error, Semigroups: GreensHClassOfElement: usage,
the element does not belong to the D-class,

# grac: GreensHClassOfElement(L/R-class, x), 1/1
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

# grac: \in, for D-class, 1/4
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

# grac: \in, for D-class, 2/4
gap> S := OrderEndomorphisms(5);;
gap> x := Transformation([1, 2, 2, 4, 5]);;
gap> D := DClass(S, x);;
gap> x in D;
true
gap> Transformation([1, 2, 1, 4, 5]) in D;
false

# grac: \in, for D-class, 3/4
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

# grac: \in, for D-class, 4/4
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> D := DClass(S, x);;
gap> y in D;
false

# grac: \in, for L-class, 1/5
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

# grac: \in, for L-class, 2/5
gap> S := OrderEndomorphisms(5);;
gap> x := Transformation([1, 2, 2, 4, 5]);;
gap> L := LClass(S, x);;
gap> x in L;
true
gap> Transformation([1, 2, 1, 4, 5]) in L;
false

# grac: \in, for L-class, 3/5
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

# grac: \in, for L-class, 4/5
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> L := LClass(S, x);;
gap> y in L;
false

# grac: \in, for L-class, 5/5
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 4, 3, 5, 5]);;
gap> L := LClass(S, x);;
gap> y in L;
false

# grac: \in, for R-class, 1/6
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

# grac: \in, for R-class, 2/6
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 4, 3, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# grac: \in, for R-class, 3/6
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 3, 3, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# grac: \in, for R-class, 4/6
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 2, 3, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# grac: \in, for R-class, 5/6
gap> S := OrderEndomorphisms(5);;
gap> x := Transformation([1, 2, 2, 4, 5]);;
gap> R := RClass(S, x);;
gap> x in R;
true
gap> Transformation([1, 2, 1, 4, 5]) in R;
false

# grac: \in, for R-class, 6/6
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# grac: \in, for H-class, 1/3
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> H := HClass(S, x);;
gap> y in H;
false

# grac: \in, for H-class, 2/3
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 2, 3, 5, 5]);;
gap> H := HClass(S, x);;
gap> y in H;
false

# grac: \in, for H-class, 3/3
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> H := HClass(S, x);;
gap> ForAll(H, x -> x in H);
true

# grac: \in, for D-class reps/D-classes, 1/1
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
  <Green's D-class: [6,5][7,1,3]>, <Green's D-class: [4,1,7]>, 
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
  <Green's D-class: [4,6][7,3](2)>, <Green's D-class: [1,5][4,7]>, 
  <Green's D-class: [6,7,3](5)>, <Green's D-class: [4,3][6,7,5]>, 
  <Green's D-class: [4,5][6,2](1)>, <Green's D-class: [6,2,5](1)>, 
  <Green's D-class: [4,2][7,5](1)>, <Green's D-class: [4,5][6,7](1)>, 
  <Green's D-class: [4,1][5,3][6,2]>, <Green's D-class: [4,3][6,2,1]>, 
  <Green's D-class: [4,2,3][7,1]>, <Green's D-class: [5,7,3](6)>, 
  <Green's D-class: [4,3](6)(7)>, <Green's D-class: [1,7][4,5]>, 
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
  <Green's D-class: [1,2][4,7]>, <Green's D-class: [4,7,2]>, 
  <Green's D-class: [6,2](3)(5)>, <Green's D-class: [4,5,3][6,2]>, 
  <Green's D-class: [6,4,3](5)>, <Green's D-class: [2,3][6,4,5]>, 
  <Green's D-class: [2,5][7,3](4)>, <Green's D-class: [4,2][6,5,3]>, 
  <Green's D-class: [4,3][6,5](2)>, <Green's D-class: [4,5][7,2,3]>, 
  <Green's D-class: [6,4,5]>, <Green's D-class: [1,5](4)>, 
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

# grac: L-classes/reps, 1/1
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

# grac: R-classes/reps, 1/1
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

# grac: H-classes/reps, 1/3
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

# grac: H-classes/reps, 2/3
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

# grac: NrXClasses, 1/1
gap> S := Semigroup(SymmetricInverseMonoid(5));;
gap> NrRClasses(S);
32
gap> NrDClasses(S);
6
gap> NrLClasses(S);
32
gap> NrHClasses(S);
252

# grac: GroupHClass, IsGroupHClass, IsomorphismPermGroup, 1/1
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
gap> IsomorphismPermGroup(H);
MappingByFunction( <Green's H-class: IdentityTransformation>, Group([ (5,9)
(6,10)(7,11)(8,12), (2,9,5,3)(4,10,13,7)(6,11)
(8,12,14,15) ]), function( x ) ... end, function( x ) ... end )
gap> IsomorphismPermGroup(HClass(S, S.1));
Error, Semigroups: IsomorphismPermGroup: usage,
the H-class is not a group,

# grac: PartialOrderOfDClasses, 1/2
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(3));;
gap> S := Semigroup(S, rec(acting := true));;
gap> PartialOrderOfDClasses(S);
[ [ 1, 2, 3, 4 ], [ 2, 3, 6, 7, 8, 9, 10 ], [ 3, 5, 8 ], [ 3, 4, 5 ], 
  [ 5, 11 ], [ 3, 6, 8, 10 ], [ 3, 7, 8, 10 ], [ 5, 8 ], [ 3, 8, 9, 10 ], 
  [ 8, 10 ], [ 11 ] ]

# grac: PartialOrderOfDClasses, 2/2
gap> S := Semigroup([Transformation([2, 3, 6, 5, 4, 8, 10, 4, 1, 4]),
>  Transformation([10, 2, 5, 4, 10, 3, 1, 6, 9, 6])],
> rec(acting := true));;
gap> PartialOrderOfDClasses(S);
[ [ 3, 4, 5 ], [ 4, 5, 6 ], [ 7, 8, 11 ], [ 8, 9, 10, 12 ], 
  [ 5, 9, 11, 12, 19, 36, 58 ], [ 5, 6, 10 ], [ 13, 14, 20 ], 
  [ 14, 15, 16, 21 ], [ 15, 17, 18, 22 ], [ 9, 10, 16, 19, 23, 36, 58 ], 
  [ 7, 11, 17, 20, 21, 34, 56 ], [ 12, 18, 22, 23, 35, 57, 84 ], 
  [ 13, 24, 25, 67, 93, 99, 111, 153 ], [ 25, 26, 27, 37 ], 
  [ 15, 26, 28, 29, 48, 69, 101 ], [ 15, 16, 27, 30, 31, 38, 51 ], 
  [ 28, 31, 32, 39 ], [ 29, 33, 40 ], [ 30, 34, 35, 41 ], 
  [ 13, 20, 31, 37, 54, 81 ], [ 14, 15, 21, 32, 38, 55, 82 ], 
  [ 15, 22, 33, 39, 40, 83 ], [ 22, 23, 33, 41, 63, 75, 77, 88 ], 
  [ 24, 42, 43, 129 ], [ 25, 42, 43, 94, 123, 130, 142, 173 ], 
  [ 26, 42, 44, 45, 131 ], [ 26, 27, 43, 46, 59, 66, 93 ], 
  [ 28, 44, 47, 69, 95, 97, 126, 127, 132, 143, 174 ], 
  [ 29, 45, 48, 96, 133 ], [ 30, 46, 48, 49, 50, 97, 134 ], 
  [ 13, 31, 47, 52, 70, 93, 128, 135, 144 ], [ 15, 28, 30, 31, 32, 51, 60 ], 
  [ 33, 48, 53, 76, 77, 80, 109, 110 ], [ 49, 54, 55, 61 ], [ 33, 50, 62 ], 
  [ 51, 56, 57, 63 ], [ 25, 26, 37, 52, 59, 78, 112 ], 
  [ 15, 27, 30, 31, 32, 38, 51, 79, 113 ], [ 28, 31, 39, 53, 60, 80, 114 ], 
  [ 29, 33, 40, 115 ], [ 30, 33, 41, 61, 62, 107 ], 
  [ 42, 89, 146, 147, 156, 177 ], [ 42, 43, 129 ], [ 42, 44, 129 ], 
  [ 42, 45, 157 ], [ 42, 46, 64, 65, 131 ], [ 42, 47, 67, 68, 131 ], 
  [ 42, 48, 70, 103, 104, 127, 128, 136, 143, 144, 152, 155, 163, 164, 169, 
      174, 175, 182, 188, 193, 198 ], 
  [ 49, 64, 71, 72, 124, 152, 158, 169, 188 ], [ 48, 50, 65, 125, 159 ], 
  [ 48, 51, 66, 73, 74, 126, 160 ], [ 26, 52, 68, 98, 123, 154, 161, 170 ], 
  [ 31, 48, 53, 70, 75, 127, 163 ], 
  [ 13, 54, 71, 78, 111, 128, 155, 164, 189 ], [ 15, 72, 79, 85 ], 
  [ 73, 81, 82, 86 ], [ 74, 77, 83, 87 ], [ 7, 31, 84, 88 ], 
  [ 26, 43, 46, 59, 66, 93, 98, 132, 148, 183, 191 ], 
  [ 15, 28, 30, 31, 51, 60, 75, 108, 113 ], [ 49, 54, 61, 76, 81, 85, 109 ], 
  [ 33, 50, 62, 138 ], [ 51, 63, 77, 83, 86, 87, 110 ], 
  [ 42, 64, 89, 90, 129 ], [ 42, 65, 157 ], [ 42, 66, 91, 92, 131 ], 
  [ 42, 67, 94, 129 ], [ 42, 68, 157 ], [ 42, 69, 95, 96, 131 ], 
  [ 42, 70, 93, 98, 177 ], [ 71, 89, 99, 100, 119, 177 ], 
  [ 72, 90, 101, 102, 149, 172, 178, 187, 196 ], 
  [ 48, 73, 91, 104, 105, 150, 179 ], [ 48, 74, 92, 151, 180 ], 
  [ 15, 30, 31, 48, 51, 75, 98, 127, 181 ], [ 48, 54, 76, 106, 144, 152, 175 ]
    , [ 48, 77, 83, 107, 127, 182 ], 
  [ 26, 59, 78, 100, 142, 154, 176, 183, 197 ], 
  [ 15, 30, 31, 51, 79, 102, 116 ], [ 31, 48, 80, 103, 108, 143, 198 ], 
  [ 67, 81, 104, 111, 112, 124, 144, 155, 184 ], [ 48, 105, 113, 117 ], 
  [ 48, 69, 83, 114, 115, 126, 186 ], [ 15, 52, 75, 118 ], 
  [ 15, 72, 85, 106, 112, 116, 139 ], [ 73, 81, 86, 107, 114, 117, 140 ], 
  [ 74, 77, 83, 87, 115, 141 ], [ 20, 31, 53, 76, 88, 107, 118 ], 
  [ 89, 119, 120, 156 ], [ 42, 90, 121, 129 ], [ 42, 91, 122, 129 ], 
  [ 42, 92, 157 ], [ 24, 42, 93, 123, 131 ], [ 42, 43, 94, 129 ], 
  [ 42, 95, 129 ], [ 42, 96, 157 ], [ 42, 97, 124, 125, 131 ], 
  [ 26, 42, 46, 66, 93, 98, 177 ], [ 99, 119, 129, 130, 131 ], 
  [ 100, 120, 131, 132, 145, 177 ], [ 42, 101, 131, 132, 133 ], 
  [ 101, 102, 121, 132, 134, 135, 160, 162, 178, 181, 195 ], 
  [ 42, 103, 135, 146, 162 ], [ 42, 104, 111, 137, 147 ], 
  [ 48, 105, 113, 121, 122, 192 ], [ 15, 48, 75, 106, 170, 172, 190 ], 
  [ 47, 48, 81, 97, 107, 138, 186 ], 
  [ 15, 30, 31, 48, 51, 108, 143, 162, 191 ], 
  [ 48, 54, 104, 109, 139, 169, 175 ], [ 48, 110, 127, 136, 140, 141, 193 ], 
  [ 24, 42, 111, 119, 142 ], [ 26, 59, 94, 112, 137, 148, 149, 170, 176 ], 
  [ 48, 95, 113, 121, 144, 148 ], [ 48, 95, 113, 114, 144, 150, 184 ], 
  [ 48, 96, 115, 151, 194 ], [ 15, 30, 31, 51, 59, 75, 102, 116, 167 ], 
  [ 48, 105, 113, 117, 138, 168 ], [ 15, 52, 75, 106, 118, 138 ], 
  [ 119, 129, 145, 156 ], [ 120, 131, 146, 156 ], [ 42, 121, 129, 147 ], 
  [ 42, 122, 129, 148 ], [ 42, 123, 157 ], [ 89, 119, 124, 129, 147, 149 ], 
  [ 42, 125, 157 ], [ 42, 126, 131, 150, 151 ], 
  [ 42, 47, 69, 97, 126, 127, 177 ], [ 42, 128, 153, 154, 177 ], 
  [ 129, 156 ], [ 129, 130, 145, 156, 157 ], [ 129, 131, 156, 157 ], 
  [ 129, 131, 132, 146, 177 ], [ 42, 133, 157 ], [ 42, 131, 134, 158, 159 ], 
  [ 99, 131, 135, 147, 161, 177 ], [ 42, 136, 165, 166, 177 ], 
  [ 26, 42, 137, 171, 183 ], [ 48, 68, 125, 138, 194 ], 
  [ 15, 48, 137, 139, 167, 187, 190 ], [ 48, 81, 97, 140, 165, 168, 199 ], 
  [ 48, 77, 83, 127, 141, 166, 200 ], [ 42, 43, 142, 145 ], 
  [ 42, 47, 69, 97, 126, 143, 146 ], [ 42, 111, 144, 147, 170 ], 
  [ 129, 145, 156 ], [ 131, 146, 156 ], [ 119, 147, 156, 171 ], 
  [ 42, 129, 148 ], [ 120, 129, 131, 132, 149, 171 ], 
  [ 42, 121, 129, 147, 150 ], [ 42, 151, 157 ], [ 42, 71, 147, 152, 172 ], 
  [ 24, 42, 131, 153, 173 ], [ 26, 42, 98, 154, 177 ], 
  [ 42, 89, 111, 155, 176 ], [ 156 ], [ 156, 157 ], 
  [ 42, 129, 158, 177, 178 ], [ 42, 157, 159 ], [ 42, 131, 160, 179, 180 ], 
  [ 131, 157, 161, 171, 177 ], [ 42, 101, 134, 135, 146, 160, 162 ], 
  [ 42, 135, 163, 177, 181 ], [ 42, 93, 164, 177, 183 ], 
  [ 42, 131, 165, 184, 185 ], [ 42, 166, 177, 182, 186 ], 
  [ 15, 30, 31, 48, 51, 127, 167, 183, 195 ], [ 48, 125, 168, 185, 201 ], 
  [ 42, 71, 147, 169, 187 ], [ 26, 42, 98, 170, 171 ], [ 131, 156, 171, 177 ],
  [ 42, 101, 171, 172, 181 ], [ 42, 157, 173 ], 
  [ 42, 47, 69, 97, 126, 174, 177 ], [ 42, 147, 175, 189, 190 ], 
  [ 26, 42, 120, 176, 191 ], [ 131, 156, 177 ], [ 42, 129, 177, 178 ], 
  [ 42, 129, 179, 192 ], [ 42, 157, 180 ], 
  [ 42, 101, 134, 135, 160, 177, 181 ], [ 42, 177, 182, 186 ], 
  [ 26, 42, 46, 66, 93, 177, 183 ], [ 42, 129, 148, 184 ], [ 42, 157, 185 ], 
  [ 42, 131, 184, 186, 194 ], [ 42, 101, 171, 187, 195 ], 
  [ 42, 71, 89, 188, 196 ], [ 42, 67, 119, 189, 197 ], 
  [ 42, 69, 127, 171, 190 ], [ 26, 42, 46, 66, 93, 146, 191 ], 
  [ 42, 95, 129, 192 ], [ 42, 177, 193, 199, 200 ], [ 42, 157, 194 ], 
  [ 42, 101, 134, 135, 160, 177, 195 ], [ 42, 101, 120, 162, 196 ], 
  [ 42, 145, 148, 197 ], [ 42, 93, 146, 191, 198 ], [ 42, 67, 131, 199, 201 ],
  [ 42, 69, 127, 177, 200 ], [ 42, 157, 201 ] ]

# grac: Idempotents, 1/?
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

# grac: Idempotents, 2/2
gap> S := Semigroup(FullTransformationMonoid(3),
> rec(acting := true));;
gap> RClasses(S);;
gap> Idempotents(S);
[ IdentityTransformation, Transformation( [ 1, 2, 1 ] ), 
  Transformation( [ 3, 2, 3 ] ), Transformation( [ 1, 2, 2 ] ), 
  Transformation( [ 1, 3, 3 ] ), Transformation( [ 2, 2 ] ), 
  Transformation( [ 1, 1 ] ), Transformation( [ 1, 1, 1 ] ), 
  Transformation( [ 2, 2, 2 ] ), Transformation( [ 3, 3, 3 ] ) ]

# grac: Idempotents, for given rank, 1/4
gap> S := Semigroup(DualSymmetricInverseMonoid(3), rec(acting := true));;
gap> Idempotents(S, -1);
Error, Semigroups: Idempotents: usage,
the second argument <n> must be a non-negative integer,

# grac: Idempotents, for given rank, 2/4
gap> S := Semigroup(DualSymmetricInverseMonoid(3), rec(acting := true));;
gap> Idempotents(S, 4);
[  ]

# grac: Idempotents, for given rank, 3/4
gap> S := Semigroup(DualSymmetricInverseMonoid(3), rec(acting := true));;
gap> Idempotents(S);;
gap> Idempotents(S, 2);
[ <block bijection: [ 1, 2, -1, -2 ], [ 3, -3 ]>, 
  <block bijection: [ 1, -1 ], [ 2, 3, -2, -3 ]>, 
  <block bijection: [ 1, 3, -1, -3 ], [ 2, -2 ]> ]

# grac: Idempotents, for given rank, 4/4
gap> S := Semigroup(DualSymmetricInverseMonoid(3), rec(acting := true));;
gap> Idempotents(S, 2);
[ <block bijection: [ 1, 2, -1, -2 ], [ 3, -3 ]>, 
  <block bijection: [ 1, -1 ], [ 2, 3, -2, -3 ]>, 
  <block bijection: [ 1, 3, -1, -3 ], [ 2, -2 ]> ]

# grac: Idempotents, for a D-class, 1/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> IsRegularDClass(D);
false
gap> Idempotents(D);
[  ]

# grac: Idempotents, for a D-class, 2/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> Idempotents(D);
[  ]

# grac: Idempotents, for a L-class, 1/3
gap> S := Semigroup(FullTransformationMonoid(5), rec(acting := true));;
gap> x := Transformation([1, 1, 2, 3, 4]);;
gap> L := LClass(S, x);;
gap> Idempotents(L);
[ Transformation( [ 1, 2, 3, 4, 1 ] ), Transformation( [ 1, 2, 3, 4, 4 ] ), 
  Transformation( [ 1, 2, 3, 4, 2 ] ), Transformation( [ 1, 2, 3, 4, 3 ] ) ]

# grac: Idempotents, for a L-class, 2/3
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> L := LClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsRegularGreensClass(L);
false
gap> Idempotents(L);
[  ]

# grac: Idempotents, for a L-class, 3/3
gap> S := PartitionMonoid(3);;
gap> L := LClass(S, One(S));;
gap> Idempotents(L);
[ <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ]> ]

# grac: Idempotents, for a H-class, 1/2
gap> S := SingularTransformationSemigroup(4);;
gap> H := HClass(S, S.1);
<Green's H-class: Transformation( [ 1, 2, 3, 3 ] )>
gap> Idempotents(H);
[ Transformation( [ 1, 2, 3, 3 ] ) ]

# grac: Idempotents, for a H-class, 1/2
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> H := HClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsGroupHClass(H);
false
gap> Idempotents(H);
[  ]

# grac: NrIdempotents, for a semigroup, 1/2
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> NrIdempotents(S);
24

# grac: NrIdempotents, for a semigroup, 2/2
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> Idempotents(S);;
gap> NrIdempotents(S);
24

# grac: NrIdempotents, for a D-class, 1/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> IsRegularDClass(D);
false
gap> NrIdempotents(D);
0

# grac: NrIdempotents, for a D-class, 2/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> NrIdempotents(D);
0

# grac: NrIdempotents, for a L-class, 1/3
gap> S := Semigroup(FullTransformationMonoid(5), rec(acting := true));;
gap> x := Transformation([1, 1, 2, 3, 4]);;
gap> L := LClass(S, x);;
gap> NrIdempotents(L);
4

# grac: NrIdempotents, for a L-class, 2/3
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> L := LClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsRegularGreensClass(L);
false
gap> NrIdempotents(L);
0

# grac: NrIdempotents, for a L-class, 3/3
gap> S := PartitionMonoid(3);;
gap> L := LClass(S, One(S));;
gap> NrIdempotents(L);
1

# grac: NrIdempotents, for a H-class, 1/2
gap> S := SingularTransformationSemigroup(4);;
gap> H := HClass(S, S.1);
<Green's H-class: Transformation( [ 1, 2, 3, 3 ] )>
gap> NrIdempotents(H);
1

# grac: NrIdempotents, for a H-class, 1/2
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> H := HClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsGroupHClass(H);
false
gap> NrIdempotents(H);
0

# grac: NrIdempotents, for an R-class, 1/2
gap> S := Semigroup(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                    Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> R := First(RClasses(S),
> x -> Transformation([9, 10, 4, 9, 10, 4, 4, 3, 3, 6]) in x);;
gap> NrIdempotents(R);
0
gap> IsRegularGreensClass(R);
false

# grac: NrIdempotents, for an R-class, 3/3
gap> S := Semigroup(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                    Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> R := RClass(S, Transformation([6, 9, 9, 6, 9, 1, 1, 2, 2, 6]));;
gap> IsRegularGreensClass(R);
true
gap> NrIdempotents(R);
7

# grac: IsRegularGreensClass, for an R-class, 1/1
gap> S := Semigroup(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                    Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> R := First(RClasses(S),
> x -> Transformation([9, 10, 4, 9, 10, 4, 4, 3, 3, 6]) in x);;
gap> IsRegularGreensClass(R);
false

# grac: IsRegularGreensClass, for an R-class in group of units, 1/1
gap> S := Monoid(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                 Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> S := AsSemigroup(IsBipartitionSemigroup, S);;
gap> R := RClass(S, IdentityBipartition(10));;
gap> IsRegularGreensClass(R);
true

# grac: NrRegularDClasses, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(acting := true));;
gap> NrRegularDClasses(S);
6

# grac: Enumerator/IteratorOfR/DClasses, 1/1
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
gap> S := Semigroup(S, rec(acting := true));;
gap> iter := IteratorOfRClasses(S);
<iterator of R-classes>
gap> iter := IteratorOfDClasses(S);
<iterator of D-classes>
gap> for x in iter do od;

# grac: RhoCosets, 1/3
gap> S := Semigroup([Transformation([1, 3, 7, 5, 5, 7, 1]),
>  Transformation([5, 3, 5, 3, 4, 1, 2]),
>  Transformation([6, 1, 6, 5, 5, 2]),
>  Transformation([6, 1, 7, 6, 3, 2, 4]),
>  Transformation([6, 2, 1, 4, 6, 4, 3]),
>  Transformation([7, 3, 3, 6, 4, 5, 5])], rec(acting := true));;
gap> L := LClass(S, Transformation([1, 5, 1, 4, 4, 3, 2]));;
gap> RhoCosets(L);
<enumerator of perm group>

# grac: RhoCosets, 2/3
gap> S := Semigroup([Transformation([1, 4, 2, 5, 2, 1, 6, 1, 7, 6]),
>  Transformation([2, 8, 4, 7, 5, 8, 3, 5, 8, 6])],
> rec(acting := true));;
gap> L := LClass(S, Transformation([4, 6, 6, 1, 6, 4, 6, 4, 1, 6]));
<Green's L-class: Transformation( [ 4, 6, 6, 1, 6, 4, 6, 4, 1, 6 ] )>
gap> RhoCosets(L);
[ (), (4,6) ]

# grac: RhoCosets, 3/3
gap> S := Semigroup([Transformation([1, 4, 2, 5, 2, 1, 6, 1, 7, 6]),
>  Transformation([2, 8, 4, 7, 5, 8, 3, 5, 8, 6])],
> rec(acting := true));;
gap> L := LClass(S, Transformation([7, 8, 8, 2, 8, 7, 8, 7, 2, 8]));;
gap> RhoCosets(L);
RightTransversal(Group([ (2,8,7), (7,8) ]),Group([ (2,7,8) ]))

# grac: SemigroupDataIndex, 1/1
gap> S := Semigroup([Transformation([1, 4, 2, 5, 2, 1, 6, 1, 7, 6]),
>  Transformation([2, 8, 4, 7, 5, 8, 3, 5, 8, 6])],
> rec(acting := true));;
gap> L := LClass(S, Transformation([7, 8, 8, 2, 8, 7, 8, 7, 2, 8]));;
gap> HasSemigroupDataIndex(L);
false
gap> SemigroupDataIndex(L);
118

# grac: SchutzenbergerGroup, for a D-class, 1/1
gap> S := Semigroup([Transformation([3, 1, 4, 2, 5, 2, 1, 6, 1, 7]),
>   Transformation([6, 2, 8, 4, 7, 5, 8, 3, 5, 8])],
> rec(acting := true));;
gap> D := DClass(S, Transformation([2, 1, 4, 2, 1, 6, 4, 4, 6, 4]));;
gap> SchutzenbergerGroup(D);
Group([ (2,7,8,4) ])

# grac: SchutzenbergerGroup, for a H-class, 1/1
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

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(D);
gap> Unbind(DD);
gap> Unbind(DDD);
gap> Unbind(H);
gap> Unbind(L);
gap> Unbind(L3);
gap> Unbind(LL);
gap> Unbind(R);
gap> Unbind(RR);
gap> Unbind(RRR);
gap> Unbind(S);
gap> Unbind(enum);
gap> Unbind(iter);
gap> Unbind(x);
gap> Unbind(y);

#E# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/grac.tst");
