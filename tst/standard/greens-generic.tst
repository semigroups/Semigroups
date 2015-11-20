#############################################################################
##
#W  standard/greens-generic.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/greens-generic.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# greens-generic: AsSSortedList, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> L := LClass(S, PartialPerm([1, 7], [3, 5]));;
gap> AsSet(L);
[ [1,3][2,5], [1,5][2,3], [2,3,5], [2,5](3), [1,3,5], [1,5](3), 
  <identity partial perm on [ 3, 5 ]>, (3,5), [2,3](5), [2,5,3], [1,3](5), 
  [1,5,3], [7,5](3), [7,3,5], [1,3][7,5], [1,5][7,3] ]

# greens-generic: \< and \=, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> L := LClass(S, PartialPerm([1, 7], [3, 5]));;
gap> LL := LClass(S, S.1);;
gap> LL = L;
false
gap> L < L;
false
gap> L < LL;
false
gap> LL < L;
true
gap> D := DClass(L);;
gap> L = D;
false
gap> L < D;
false

# greens-generic: MultiplicativeNeutralElement, One, for an H-class, 1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> H := HClass(S, S.4);;
gap> MultiplicativeNeutralElement(H);
fail
gap> OneImmutable(H);
<identity partial perm on [ 1, 2, 3, 4, 5, 6, 7 ]>
gap> H := HClass(S, PartialPerm([3, 5], [3, 5]));;
gap> MultiplicativeNeutralElement(H);
<identity partial perm on [ 3, 5 ]>
gap> OneImmutable(H);
<identity partial perm on [ 3, 5 ]>

# greens-generic: MultiplicativeNeutralElement, One, for an H-class, 2
gap> S := Semigroup([
> Transformation([1, 1, 3, 2, 4]),
> Transformation([1, 5, 5, 2, 5]),
> Transformation([4, 1, 3, 5, 5])]);;
gap> H := HClass(S, S.1);;
gap> MultiplicativeNeutralElement(H);
fail
gap> OneImmutable(H);
IdentityTransformation
gap> H := HClass(S, Transformation([1, 1]));;
gap> MultiplicativeNeutralElement(H);
Transformation( [ 1, 1 ] )
gap> OneImmutable(H);
Transformation( [ 1, 1 ] )

# greens-generic: StructureDescription, for an H-class, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> H := HClass(S, PartialPerm([3, 5], [3, 5]));;
gap> StructureDescription(H);
"C2"
gap> H := HClass(S, S.4);;
gap> StructureDescription(H);
fail

# greens-generic: DClassOfLClass, 1/1
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

# greens-generic: DClassOfRClass, 1/1
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

# greens-generic: DClassOfHClass, 1/1
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

# greens-generic: LClassOfHClass, 1/1
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
>   Bipartition([[1, 5, -2], [2, -1, -5], [3, 4, -6], [6, -3], [-4]])]);;
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

# greens-generic: RClassOfHClass, 1/1
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

# greens-generic: GreensDClassOfElement, fail, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(generic := true));;
gap> GreensDClassOfElement(S, PartialPerm([19]));
Error, Semigroups: SEMIGROUPS.EquivalenceClassOfElement: usage,
the element in the 2nd argument does not belong to the semigroup,

# greens-generic: GreensDClassOfElementNC, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(generic := true));;
gap> D := GreensDClassOfElementNC(S, PartialPerm([19]));;

# greens-generic: GreensJClassOfElement, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(generic := true));;
gap> J := GreensJClassOfElement(S, S.2);
<Green's D-class: [6,4,7,1,2,5](3)>

# greens-generic: GreensL/RClassOfElement, fail, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(generic := true));;
gap> RClass(S, ConstantTransformation(7, 7));
Error, Semigroups: SEMIGROUPS.EquivalenceClassOfElement: usage,
the element in the 2nd argument does not belong to the semigroup,
gap> LClass(S, ConstantTransformation(7, 7));
Error, Semigroups: SEMIGROUPS.EquivalenceClassOfElement: usage,
the element in the 2nd argument does not belong to the semigroup,
gap> HClass(S, ConstantTransformation(7, 7));
Error, Semigroups: SEMIGROUPS.EquivalenceClassOfElement: usage,
the element in the 2nd argument does not belong to the semigroup,

# greens-generic: GreensL/RClassOfElementNC, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(generic := true));;
gap> R := RClassNC(S, S.1);
<Green's R-class: Transformation( [ 2, 2, 1, 2, 4, 4 ] )>
gap> Size(R);
120
gap> L := LClassNC(S, S.1);
<Green's L-class: Transformation( [ 2, 2, 1, 2, 4, 4 ] )>
gap> Size(L);
396
gap> H := HClassNC(S, S.1);
<Green's H-class: Transformation( [ 2, 2, 1, 2, 4, 4 ] )>
gap> Size(H);
6

# greens-generic: GreensL/RClassOfElement, for a D-class, 1/1
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
> rec(generic := true));;
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
Error, Semigroups: SEMIGROUPS.EquivalenceClassOfElement: usage,
the element in the 2nd argument does not belong to the semigroup,
gap> RClass(D, IdentityBipartition(8));
Error, Semigroups: SEMIGROUPS.EquivalenceClassOfElement: usage,
the element in the 2nd argument does not belong to the semigroup,
gap> x := Bipartition([[1, 4, -1, -2, -6], [2, 3, 5, -4],
> [6, -3], [-5]]);;
gap> LClassNC(D, x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `GreensLClassOfElementNC' on 2 arguments
gap> RClassNC(D, x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `GreensRClassOfElementNC' on 2 arguments

# greens-generic: GreensHClassOfElement, 1/1
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
gap> S := Semigroup(S, rec(generic := true));;
gap> D := DClass(S, S.4 * S.5);;
gap> H := HClass(D, MultiplicativeZero(S));
<Green's H-class: 0>
gap> H := HClassNC(D, MultiplicativeZero(S));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `GreensHClassOfElementNC' on 2 arguments
gap> H := HClass(D, IdentityTransformation);
Error, Semigroups: SEMIGROUPS.EquivalenceClassOfElement: usage,
the element in the 2nd argument does not belong to the semigroup,

# greens-generic: GreensHClassOfElement(L/R-class, x), 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(generic := true));;
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

# greens-generic: \in, for D-class, 1/4
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

# greens-generic: \in, for D-class, 2/4
gap> S := OrderEndomorphisms(5);;
gap> x := Transformation([1, 2, 2, 4, 5]);;
gap> D := DClass(S, x);;
gap> x in D;
true
gap> Transformation([1, 2, 1, 4, 5]) in D;
false

# greens-generic: \in, for D-class, 3/4
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[0, 0, 0, ()], [
> (), 0, 0, 0], [(), (), 0, 0], [0, (), (), 0], [0, 0, (), ()]]);;
gap> S := Semigroup(S);;
gap> D := DClass(S, S.1);;
gap> Size(S);
41
gap> Size(D) = Size(S) - 1;
true
gap> ForAll(D, x -> x in D);
true

# greens-generic: \in, for D-class, 4/4
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> D := DClass(S, x);;
gap> y in D;
false

# greens-generic: \in, for L-class, 1/5
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

# greens-generic: \in, for L-class, 2/5
gap> S := OrderEndomorphisms(5);;
gap> x := Transformation([1, 2, 2, 4, 5]);;
gap> L := LClass(S, x);;
gap> x in L;
true
gap> Transformation([1, 2, 1, 4, 5]) in L;
false

# greens-generic: \in, for L-class, 3/5
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

# greens-generic: \in, for L-class, 4/5
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> L := LClass(S, x);;
gap> y in L;
false

# greens-generic: \in, for L-class, 5/5
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 4, 3, 5, 5]);;
gap> L := LClass(S, x);;
gap> y in L;
false

# greens-generic: \in, for R-class, 1/6
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

# greens-generic: \in, for R-class, 2/6
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 4, 3, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# greens-generic: \in, for R-class, 3/6
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 3, 3, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# greens-generic: \in, for R-class, 4/6
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 2, 3, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# greens-generic: \in, for R-class, 5/6
gap> S := OrderEndomorphisms(5);;
gap> x := Transformation([1, 2, 2, 4, 5]);;
gap> R := RClass(S, x);;
gap> x in R;
true
gap> Transformation([1, 2, 1, 4, 5]) in R;
false

# greens-generic: \in, for R-class, 6/6
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# greens-generic: \in, for H-class, 1/3
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> H := HClass(S, x);;
gap> y in H;
false

# greens-generic: \in, for H-class, 2/3
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 2, 3, 5, 5]);;
gap> H := HClass(S, x);;
gap> y in H;
false

# greens-generic: \in, for H-class, 3/3
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> H := HClass(S, x);;
gap> ForAll(H, x -> x in H);
true

# greens-generic: \in, for D-class reps/D-classes, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(generic := true));;
gap> DClassReps(S);
[ <empty partial perm>, [3,1], [6,1], [6,5](1), [3,7](1), [1,3,5][6,7], 
  [6,7](5), <identity partial perm on [ 6 ]>, [2,7][6,5], [6,7](2), 
  [1,3][6,4], [2,5][6,4], [2,7](6), [1,3,2][6,4], (1,3)(6), [4,3,1], 
  [4,1][6,5], [4,5][6,2](3), [4,2][6,1](3), [4,1][6,7](3), [4,3,1][6,5], 
  [4,2][6,3,5], [4,1][6,3,2], [4,7][6,3,1], [4,5][6,1](3), [1,5][4,3][6,2], 
  [4,3][6,1,2], [4,3][6,7](1), [4,1,3][6,5], [4,5][6,2](1), [4,7][6,5], 
  [1,5][4,2][6,7](3), [1,5][4,3][6,7], [6,4,3], [4,5][6,7], [4,2][6,7], 
  [6,4,5], [4,1](6), [1,2][4,5][6,3], [4,2][6,3](1), [4,1,7][6,3], 
  [4,3][6,1,5], [1,2][4,3][6,7], [6,4,3](1), [1,7][4,3](6), [6,4,1,2](3), 
  [1,3][4,5][6,2], [4,2][6,1,3], [4,7][6,5](1), [1,3][6,4,5], [1,3][4,2](6), 
  [4,7](1)(3)(6), [1,3][6,2](5), [5,2][6,1,3], [5,1,3][6,7], [6,5,3](1), 
  [1,5,2][6,3], [5,1,2][6,3], [5,7][6,3](1), [6,1,3](5), [6,2](3,5), 
  [5,3,2][6,1], [5,3,1][6,7], [6,5,1](3), [3,1][6,2](5), [1,3,5,2][6,7], 
  [6,7](3,5), [6,3,2](5), [5,2][6,3,1], [5,1][6,3,7], [6,1](3,5), 
  [5,3,2][6,7], [5,3,1][6,4], [5,3,7](6), [5,1,3,2][6,4], [4,5][6,7](3), 
  [6,4,2](3), [4,3,5][6,7], [6,4,3,2], [4,3,1](6), [6,2](3)(5), 
  [5,2][6,1](3), [3,1][6,5,7], [4,7][5,3], [4,2][6,5,3], [4,1][5,3][6,2], 
  [4,5,1][6,7], [6,4,2](3)(5), [4,5,3][6,2], [4,2][5,3][6,1], [4,1][5,3][6,7],
  [4,3][6,5,1], [4,1][6,2](5), [4,5,2][6,7], [4,5,3][6,7], [4,2][6,3](5), 
  [4,1][5,2][6,3], [4,7][5,1][6,3], [4,5,3][6,1], [4,2][5,3][6,7], 
  [5,3][6,4,1], [4,7][5,3](6), [5,1][6,4,2], [4,3][6,2](5), [4,3][5,2][6,1], 
  [4,1][6,5,7], [6,4,3](5), [4,3][5,2](6), [4,1][5,7](6), [4,3](5)(6), 
  [4,1][5,2](3)(6), [4,5,7](1,3)(6), [6,2][7,5](3), [6,1][7,2](3), 
  [6,7,1](3), [2,3][7,1], [7,3,5](2), [7,3,2,1], [6,2,5](3), [6,1](2)(3), 
  [2,7,3,1][6,5], [1,2][4,3,5], [4,3,2](1), [4,3,1,7], [6,3,5][7,2], 
  [6,3,2][7,1], [6,3,1](7), [4,3][7,1], [4,2][6,5][7,3], [4,1][6,2][7,3], 
  [4,7,3][6,1], [4,5][6,3][7,1], [4,2][6,1][7,5](3), [1,3][4,5][7,2], 
  [4,2][7,1,3], [4,1,3](7), [1,2][4,3][6,5], [1,5][6,2][7,3], [6,1,2][7,3], 
  [6,7,3](1), [6,5][7,1,3], [4,3][6,2][7,5](1), [4,3][6,5](7), 
  [4,1,5][6,7,2](3), [1,2,3][7,5], [7,2,3](1), [2,3](1,7), [4,2][6,3][7,5], 
  [4,1][6,3][7,2], [4,7,1][6,3], [6,2,5][7,3], [4,3][6,5](2), [4,3][6,2,1], 
  [2,7][4,3][6,1], [2,5][4,1][6,3], [4,5][6,1][7,3](2), [1,3][4,2,5], 
  [4,1,3](2), [2,1,3][4,7], [1,2,3][6,5], [4,5][6,2,3](1), [2,3][4,7][6,5], 
  [4,2,1,5][6,7,3], [4,3][6,2][7,5], [4,3][6,1][7,2], [4,3][6,7,1], 
  [4,1][6,5][7,3], [4,5][6,2][7,1], [4,2][6,7,5], [4,3][6,7,5], 
  [4,5][6,3][7,2], [4,2][6,3][7,1], [4,1][6,3](7), [4,3][6,1][7,5], 
  [4,3][6,7,2], [6,4,3][7,1], [4,3](6)(7), [6,4,1][7,2], [4,5][6,2][7,3], 
  [4,2][6,1][7,3], [4,7,1][6,5], [6,4,5][7,3], [4,2][7,3](6), [4,7,1](6), 
  [1,2][6,3][7,5], [6,3][7,2](1), [6,3](1,7), [6,1,5][7,3], [1,2][6,7,3], 
  [6,4][7,3](1), [1,3][4,2][6,5], [4,1,3][6,2], [4,5][6,7](1), [1,5][6,4,2], 
  [4,1,3][6,7], [4,3][6,5](1), [4,1,5][6,2], [1,2][4,5][6,7], [1,3][4,5][6,7],
  [1,5][4,2][6,3], [4,1,2][6,3], [4,7][6,3](1), [4,5][6,1,3], [1,3][4,2][6,7],
  [6,4,1,3], [1,3][4,7](6), [6,4,2](1), [4,1,7][6,5], [1,5][6,4,3], 
  [1,2][4,3](6), [4,1,7](6), [1,5][4,3](6), [4,1,2](6), [1,7,3][4,5](6), 
  [6,4,7,1,2,5](3), [1,5,3][6,7], [1,2][5,3][6,4], [1,3][6,7](5), 
  [1,3][5,2][6,4], [5,1,3](6), [6,2,5,3], [5,3][6,1](2), [2,1][5,3][6,7], 
  [2,3][6,5,1], [6,3](2)(5), [5,2,1][6,3], [2,7][5,1][6,3], [2,5,3][6,1], 
  [1,5][6,2,3], [6,1,2,3], [2,3][6,7](1), [2,1,3][6,5], [6,2,5](1), 
  [1,5,3][6,7](2), [1,5][2,3][6,7], [1,2,5][6,3], [6,3](1)(2), [2,1,7][6,3], 
  [2,3][6,1,5], [1,2,3][6,7], [2,3][6,4](1), [1,7][2,3](6), [5,3][6,4](1,2), 
  [1,3][6,2,5], [6,1,3](2), [2,7][6,5](1), [1,3][2,5][6,4], [1,3](2)(6), 
  [2,7][5,3](1)(6), [1,5][4,3][7,2], [4,3][7,1,2], [1,3][6,2][7,5], 
  [6,1,3][7,2], [4,3][6,5](1)(7), [4,3][5,1][6,7], [4,1][6,5,3], 
  [4,5,1][6,2], [1,3][4,2][6,7](5), [5,3][7,1], [4,5,2][7,3], [4,2][5,1][7,3],
  [4,1][5,7,3], [6,5,2][7,3], [5,1][6,2][7,3], [4,3][6,7,1](5), [4,5,2][6,3], 
  [4,2][5,1][6,3], [4,1][5,7][6,3], [4,3][6,1](5), [4,3][5,2][6,7], 
  [5,1][6,4,3], [4,3][5,7](6), [6,4,1,3][7,5,2], [2,3][6,7](5), [5,2,3][6,4], 
  [2,5,3][6,7], [5,3][6,4](2), [2,1][5,3](6), [6,2,3][7,5], [6,1][7,2,3], 
  [2,3][6,7,1], [2,1][6,5][7,3], [6,3][7,2,5], [6,3][7,1](2), [2,1][6,3](7), 
  [2,3][6,1][7,5], [6,2][7,3](5), [5,2][6,1][7,3], [5,1][6,7,3], 
  [6,5,3][7,1], [6,2][7,5,1], [6,7,2,3](5), [6,7,3](5), [6,3][7,5,2], 
  [5,1][6,3][7,2], [5,7,1][6,3], [6,1][7,3](5), [5,2][6,7,3], [5,1][6,4][7,3],
  [5,7,3](6), [5,2,3][6,4][7,1], [6,2][7,5,3], [5,3][6,1][7,2], [6,5,1](7), 
  [6,4][7,5,3], [5,3][7,2](6), [2,3][5,1](6)(7), [4,7][6,5,1], 
  [2,5][4,1][6,7], [6,4,5,3](2), [2,5][6,7,3], [6,4][7,3](2), [2,3][6,7,5], 
  [6,4][7,2,3], [2,3][7,1](6), [4,1][6,7,3], [4,3][6,5][7,1], [4,5][6,1][7,3],
  [4,3][6,2,5], [4,3][6,1](2), [2,1][4,3][6,7], [2,3][4,1][6,5], 
  [4,5][6,2,1], [4,2,5][6,7,3], [2,5][4,3][6,7], [4,5][6,3](2), [4,2,1][6,3], 
  [2,7][4,1][6,3], [2,5][4,3][6,1], [4,3][6,7](2), [2,1][6,4,3], 
  [2,7][4,3](6), [6,4,1][7,3](2), [4,5][6,2,3], [4,2,3][6,1], [2,1][4,7][6,5],
  [2,3][6,4,5], [4,2,3](6), [2,1][4,7,3](6), [6,4,3][7,2], [4,5][6,7,3], 
  [6,4,2][7,3], [4,1][7,3](6), [2,3][4,5][7,1](6), [4,2,1][7,5,3](6), 
  [4,5,3](7), [4,7,2,5,1,3](6), [1,5][4,2][7,3], [4,1,2][7,3], [4,7,3](1), 
  [4,5][7,1,3], [4,2][7,5](1), [4,5](7), [1,5][4,7,2], [1,5][4,7,3], 
  [7,3](4), [4,7,5], [4,7,2], [7,5](4), [4,6][7,1], [1,2][4,3][7,5], 
  [4,3][7,2](1), [4,3](1,7), [4,1,5][7,3], [1,2][4,7,3], [7,3](1)(4), 
  [1,7,3][4,6], [7,1,2](4), [1,3][4,2][7,5], [4,1,3][7,2], [4,5](1)(7), 
  [1,3][7,5](4), [1,3][4,6][7,2], [4,6](1)(7), [2,3][4,5](1), [1,5][4,3](2), 
  [4,3](1,2), [2,7][4,3](1), [2,5][4,1,3], [1,3][4,7](2), [2,1,3](4), 
  [4,5][7,2,3], [4,2,3][7,1], [2,1][4,7,5], [7,2,5](4), [4,2,3][7,5], 
  [4,1][7,2,3], [2,3][4,7,1], [2,1][4,5][7,3], [4,2,5][7,1], [4,7,5](2), 
  [2,3][4,7,5], [4,3][7,2,5], [4,3][7,1](2), [2,1][4,3](7), [2,3][4,1][7,5], 
  [4,7,2,3], [2,3][7,1](4), [2,3][4,6](7), [7,2,1](4), [4,2,5][7,3], 
  [4,1][7,3](2), [2,7,1][4,5], [2,5][7,3](4), [4,6][7,3](2), [2,7,1][4,6], 
  [2,5][4,6][7,3], [4,6][7,1](2), [1,3][2,7,5][4,6] ]
gap> DClasses(S);
[ <Green's D-class: <empty partial perm>>, <Green's D-class: [3,1]>, 
  <Green's D-class: [6,1]>, <Green's D-class: [6,5](1)>, 
  <Green's D-class: [3,7](1)>, <Green's D-class: [1,3,5][6,7]>, 
  <Green's D-class: [6,7](5)>, 
  <Green's D-class: <identity partial perm on [ 6 ]>>, 
  <Green's D-class: [2,7][6,5]>, <Green's D-class: [6,7](2)>, 
  <Green's D-class: [1,3][6,4]>, <Green's D-class: [2,5][6,4]>, 
  <Green's D-class: [2,7](6)>, <Green's D-class: [1,3,2][6,4]>, 
  <Green's D-class: (1,3)(6)>, <Green's D-class: [4,3,1]>, 
  <Green's D-class: [4,1][6,5]>, <Green's D-class: [4,5][6,2](3)>, 
  <Green's D-class: [4,2][6,1](3)>, <Green's D-class: [4,1][6,7](3)>, 
  <Green's D-class: [4,3,1][6,5]>, <Green's D-class: [4,2][6,3,5]>, 
  <Green's D-class: [4,1][6,3,2]>, <Green's D-class: [4,7][6,3,1]>, 
  <Green's D-class: [4,5][6,1](3)>, <Green's D-class: [1,5][4,3][6,2]>, 
  <Green's D-class: [4,3][6,1,2]>, <Green's D-class: [4,3][6,7](1)>, 
  <Green's D-class: [4,1,3][6,5]>, <Green's D-class: [4,5][6,2](1)>, 
  <Green's D-class: [4,7][6,5]>, <Green's D-class: [1,5][4,2][6,7](3)>, 
  <Green's D-class: [1,5][4,3][6,7]>, <Green's D-class: [6,4,3]>, 
  <Green's D-class: [4,5][6,7]>, <Green's D-class: [4,2][6,7]>, 
  <Green's D-class: [6,4,5]>, <Green's D-class: [4,1](6)>, 
  <Green's D-class: [1,2][4,5][6,3]>, <Green's D-class: [4,2][6,3](1)>, 
  <Green's D-class: [4,1,7][6,3]>, <Green's D-class: [4,3][6,1,5]>, 
  <Green's D-class: [1,2][4,3][6,7]>, <Green's D-class: [6,4,3](1)>, 
  <Green's D-class: [1,7][4,3](6)>, <Green's D-class: [6,4,1,2](3)>, 
  <Green's D-class: [1,3][4,5][6,2]>, <Green's D-class: [4,2][6,1,3]>, 
  <Green's D-class: [4,7][6,5](1)>, <Green's D-class: [1,3][6,4,5]>, 
  <Green's D-class: [1,3][4,2](6)>, <Green's D-class: [4,7](1)(3)(6)>, 
  <Green's D-class: [1,3][6,2](5)>, <Green's D-class: [5,2][6,1,3]>, 
  <Green's D-class: [5,1,3][6,7]>, <Green's D-class: [6,5,3](1)>, 
  <Green's D-class: [1,5,2][6,3]>, <Green's D-class: [5,1,2][6,3]>, 
  <Green's D-class: [5,7][6,3](1)>, <Green's D-class: [6,1,3](5)>, 
  <Green's D-class: [6,2](3,5)>, <Green's D-class: [5,3,2][6,1]>, 
  <Green's D-class: [5,3,1][6,7]>, <Green's D-class: [6,5,1](3)>, 
  <Green's D-class: [3,1][6,2](5)>, <Green's D-class: [1,3,5,2][6,7]>, 
  <Green's D-class: [6,7](3,5)>, <Green's D-class: [6,3,2](5)>, 
  <Green's D-class: [5,2][6,3,1]>, <Green's D-class: [5,1][6,3,7]>, 
  <Green's D-class: [6,1](3,5)>, <Green's D-class: [5,3,2][6,7]>, 
  <Green's D-class: [5,3,1][6,4]>, <Green's D-class: [5,3,7](6)>, 
  <Green's D-class: [5,1,3,2][6,4]>, <Green's D-class: [4,5][6,7](3)>, 
  <Green's D-class: [6,4,2](3)>, <Green's D-class: [4,3,5][6,7]>, 
  <Green's D-class: [6,4,3,2]>, <Green's D-class: [4,3,1](6)>, 
  <Green's D-class: [6,2](3)(5)>, <Green's D-class: [5,2][6,1](3)>, 
  <Green's D-class: [3,1][6,5,7]>, <Green's D-class: [4,7][5,3]>, 
  <Green's D-class: [4,2][6,5,3]>, <Green's D-class: [4,1][5,3][6,2]>, 
  <Green's D-class: [4,5,1][6,7]>, <Green's D-class: [6,4,2](3)(5)>, 
  <Green's D-class: [4,5,3][6,2]>, <Green's D-class: [4,2][5,3][6,1]>, 
  <Green's D-class: [4,1][5,3][6,7]>, <Green's D-class: [4,3][6,5,1]>, 
  <Green's D-class: [4,1][6,2](5)>, <Green's D-class: [4,5,2][6,7]>, 
  <Green's D-class: [4,5,3][6,7]>, <Green's D-class: [4,2][6,3](5)>, 
  <Green's D-class: [4,1][5,2][6,3]>, <Green's D-class: [4,7][5,1][6,3]>, 
  <Green's D-class: [4,5,3][6,1]>, <Green's D-class: [4,2][5,3][6,7]>, 
  <Green's D-class: [5,3][6,4,1]>, <Green's D-class: [4,7][5,3](6)>, 
  <Green's D-class: [5,1][6,4,2]>, <Green's D-class: [4,3][6,2](5)>, 
  <Green's D-class: [4,3][5,2][6,1]>, <Green's D-class: [4,1][6,5,7]>, 
  <Green's D-class: [6,4,3](5)>, <Green's D-class: [4,3][5,2](6)>, 
  <Green's D-class: [4,1][5,7](6)>, <Green's D-class: [4,3](5)(6)>, 
  <Green's D-class: [4,1][5,2](3)(6)>, <Green's D-class: [4,5,7](1,3)(6)>, 
  <Green's D-class: [6,2][7,5](3)>, <Green's D-class: [6,1][7,2](3)>, 
  <Green's D-class: [6,7,1](3)>, <Green's D-class: [2,3][7,1]>, 
  <Green's D-class: [7,3,5](2)>, <Green's D-class: [7,3,2,1]>, 
  <Green's D-class: [6,2,5](3)>, <Green's D-class: [6,1](2)(3)>, 
  <Green's D-class: [2,7,3,1][6,5]>, <Green's D-class: [1,2][4,3,5]>, 
  <Green's D-class: [4,3,2](1)>, <Green's D-class: [4,3,1,7]>, 
  <Green's D-class: [6,3,5][7,2]>, <Green's D-class: [6,3,2][7,1]>, 
  <Green's D-class: [6,3,1](7)>, <Green's D-class: [4,3][7,1]>, 
  <Green's D-class: [4,2][6,5][7,3]>, <Green's D-class: [4,1][6,2][7,3]>, 
  <Green's D-class: [4,7,3][6,1]>, <Green's D-class: [4,5][6,3][7,1]>, 
  <Green's D-class: [4,2][6,1][7,5](3)>, <Green's D-class: [1,3][4,5][7,2]>, 
  <Green's D-class: [4,2][7,1,3]>, <Green's D-class: [4,1,3](7)>, 
  <Green's D-class: [1,2][4,3][6,5]>, <Green's D-class: [1,5][6,2][7,3]>, 
  <Green's D-class: [6,1,2][7,3]>, <Green's D-class: [6,7,3](1)>, 
  <Green's D-class: [6,5][7,1,3]>, <Green's D-class: [4,3][6,2][7,5](1)>, 
  <Green's D-class: [4,3][6,5](7)>, <Green's D-class: [4,1,5][6,7,2](3)>, 
  <Green's D-class: [1,2,3][7,5]>, <Green's D-class: [7,2,3](1)>, 
  <Green's D-class: [2,3](1,7)>, <Green's D-class: [4,2][6,3][7,5]>, 
  <Green's D-class: [4,1][6,3][7,2]>, <Green's D-class: [4,7,1][6,3]>, 
  <Green's D-class: [6,2,5][7,3]>, <Green's D-class: [4,3][6,5](2)>, 
  <Green's D-class: [4,3][6,2,1]>, <Green's D-class: [2,7][4,3][6,1]>, 
  <Green's D-class: [2,5][4,1][6,3]>, <Green's D-class: [4,5][6,1][7,3](2)>, 
  <Green's D-class: [1,3][4,2,5]>, <Green's D-class: [4,1,3](2)>, 
  <Green's D-class: [2,1,3][4,7]>, <Green's D-class: [1,2,3][6,5]>, 
  <Green's D-class: [4,5][6,2,3](1)>, <Green's D-class: [2,3][4,7][6,5]>, 
  <Green's D-class: [4,2,1,5][6,7,3]>, <Green's D-class: [4,3][6,2][7,5]>, 
  <Green's D-class: [4,3][6,1][7,2]>, <Green's D-class: [4,3][6,7,1]>, 
  <Green's D-class: [4,1][6,5][7,3]>, <Green's D-class: [4,5][6,2][7,1]>, 
  <Green's D-class: [4,2][6,7,5]>, <Green's D-class: [4,3][6,7,5]>, 
  <Green's D-class: [4,5][6,3][7,2]>, <Green's D-class: [4,2][6,3][7,1]>, 
  <Green's D-class: [4,1][6,3](7)>, <Green's D-class: [4,3][6,1][7,5]>, 
  <Green's D-class: [4,3][6,7,2]>, <Green's D-class: [6,4,3][7,1]>, 
  <Green's D-class: [4,3](6)(7)>, <Green's D-class: [6,4,1][7,2]>, 
  <Green's D-class: [4,5][6,2][7,3]>, <Green's D-class: [4,2][6,1][7,3]>, 
  <Green's D-class: [4,7,1][6,5]>, <Green's D-class: [6,4,5][7,3]>, 
  <Green's D-class: [4,2][7,3](6)>, <Green's D-class: [4,7,1](6)>, 
  <Green's D-class: [1,2][6,3][7,5]>, <Green's D-class: [6,3][7,2](1)>, 
  <Green's D-class: [6,3](1,7)>, <Green's D-class: [6,1,5][7,3]>, 
  <Green's D-class: [1,2][6,7,3]>, <Green's D-class: [6,4][7,3](1)>, 
  <Green's D-class: [1,3][4,2][6,5]>, <Green's D-class: [4,1,3][6,2]>, 
  <Green's D-class: [4,5][6,7](1)>, <Green's D-class: [1,5][6,4,2]>, 
  <Green's D-class: [4,1,3][6,7]>, <Green's D-class: [4,3][6,5](1)>, 
  <Green's D-class: [4,1,5][6,2]>, <Green's D-class: [1,2][4,5][6,7]>, 
  <Green's D-class: [1,3][4,5][6,7]>, <Green's D-class: [1,5][4,2][6,3]>, 
  <Green's D-class: [4,1,2][6,3]>, <Green's D-class: [4,7][6,3](1)>, 
  <Green's D-class: [4,5][6,1,3]>, <Green's D-class: [1,3][4,2][6,7]>, 
  <Green's D-class: [6,4,1,3]>, <Green's D-class: [1,3][4,7](6)>, 
  <Green's D-class: [6,4,2](1)>, <Green's D-class: [4,1,7][6,5]>, 
  <Green's D-class: [1,5][6,4,3]>, <Green's D-class: [1,2][4,3](6)>, 
  <Green's D-class: [4,1,7](6)>, <Green's D-class: [1,5][4,3](6)>, 
  <Green's D-class: [4,1,2](6)>, <Green's D-class: [1,7,3][4,5](6)>, 
  <Green's D-class: [6,4,7,1,2,5](3)>, <Green's D-class: [1,5,3][6,7]>, 
  <Green's D-class: [1,2][5,3][6,4]>, <Green's D-class: [1,3][6,7](5)>, 
  <Green's D-class: [1,3][5,2][6,4]>, <Green's D-class: [5,1,3](6)>, 
  <Green's D-class: [6,2,5,3]>, <Green's D-class: [5,3][6,1](2)>, 
  <Green's D-class: [2,1][5,3][6,7]>, <Green's D-class: [2,3][6,5,1]>, 
  <Green's D-class: [6,3](2)(5)>, <Green's D-class: [5,2,1][6,3]>, 
  <Green's D-class: [2,7][5,1][6,3]>, <Green's D-class: [2,5,3][6,1]>, 
  <Green's D-class: [1,5][6,2,3]>, <Green's D-class: [6,1,2,3]>, 
  <Green's D-class: [2,3][6,7](1)>, <Green's D-class: [2,1,3][6,5]>, 
  <Green's D-class: [6,2,5](1)>, <Green's D-class: [1,5,3][6,7](2)>, 
  <Green's D-class: [1,5][2,3][6,7]>, <Green's D-class: [1,2,5][6,3]>, 
  <Green's D-class: [6,3](1)(2)>, <Green's D-class: [2,1,7][6,3]>, 
  <Green's D-class: [2,3][6,1,5]>, <Green's D-class: [1,2,3][6,7]>, 
  <Green's D-class: [2,3][6,4](1)>, <Green's D-class: [1,7][2,3](6)>, 
  <Green's D-class: [5,3][6,4](1,2)>, <Green's D-class: [1,3][6,2,5]>, 
  <Green's D-class: [6,1,3](2)>, <Green's D-class: [2,7][6,5](1)>, 
  <Green's D-class: [1,3][2,5][6,4]>, <Green's D-class: [1,3](2)(6)>, 
  <Green's D-class: [2,7][5,3](1)(6)>, <Green's D-class: [1,5][4,3][7,2]>, 
  <Green's D-class: [4,3][7,1,2]>, <Green's D-class: [1,3][6,2][7,5]>, 
  <Green's D-class: [6,1,3][7,2]>, <Green's D-class: [4,3][6,5](1)(7)>, 
  <Green's D-class: [4,3][5,1][6,7]>, <Green's D-class: [4,1][6,5,3]>, 
  <Green's D-class: [4,5,1][6,2]>, <Green's D-class: [1,3][4,2][6,7](5)>, 
  <Green's D-class: [5,3][7,1]>, <Green's D-class: [4,5,2][7,3]>, 
  <Green's D-class: [4,2][5,1][7,3]>, <Green's D-class: [4,1][5,7,3]>, 
  <Green's D-class: [6,5,2][7,3]>, <Green's D-class: [5,1][6,2][7,3]>, 
  <Green's D-class: [4,3][6,7,1](5)>, <Green's D-class: [4,5,2][6,3]>, 
  <Green's D-class: [4,2][5,1][6,3]>, <Green's D-class: [4,1][5,7][6,3]>, 
  <Green's D-class: [4,3][6,1](5)>, <Green's D-class: [4,3][5,2][6,7]>, 
  <Green's D-class: [5,1][6,4,3]>, <Green's D-class: [4,3][5,7](6)>, 
  <Green's D-class: [6,4,1,3][7,5,2]>, <Green's D-class: [2,3][6,7](5)>, 
  <Green's D-class: [5,2,3][6,4]>, <Green's D-class: [2,5,3][6,7]>, 
  <Green's D-class: [5,3][6,4](2)>, <Green's D-class: [2,1][5,3](6)>, 
  <Green's D-class: [6,2,3][7,5]>, <Green's D-class: [6,1][7,2,3]>, 
  <Green's D-class: [2,3][6,7,1]>, <Green's D-class: [2,1][6,5][7,3]>, 
  <Green's D-class: [6,3][7,2,5]>, <Green's D-class: [6,3][7,1](2)>, 
  <Green's D-class: [2,1][6,3](7)>, <Green's D-class: [2,3][6,1][7,5]>, 
  <Green's D-class: [6,2][7,3](5)>, <Green's D-class: [5,2][6,1][7,3]>, 
  <Green's D-class: [5,1][6,7,3]>, <Green's D-class: [6,5,3][7,1]>, 
  <Green's D-class: [6,2][7,5,1]>, <Green's D-class: [6,7,2,3](5)>, 
  <Green's D-class: [6,7,3](5)>, <Green's D-class: [6,3][7,5,2]>, 
  <Green's D-class: [5,1][6,3][7,2]>, <Green's D-class: [5,7,1][6,3]>, 
  <Green's D-class: [6,1][7,3](5)>, <Green's D-class: [5,2][6,7,3]>, 
  <Green's D-class: [5,1][6,4][7,3]>, <Green's D-class: [5,7,3](6)>, 
  <Green's D-class: [5,2,3][6,4][7,1]>, <Green's D-class: [6,2][7,5,3]>, 
  <Green's D-class: [5,3][6,1][7,2]>, <Green's D-class: [6,5,1](7)>, 
  <Green's D-class: [6,4][7,5,3]>, <Green's D-class: [5,3][7,2](6)>, 
  <Green's D-class: [2,3][5,1](6)(7)>, <Green's D-class: [4,7][6,5,1]>, 
  <Green's D-class: [2,5][4,1][6,7]>, <Green's D-class: [6,4,5,3](2)>, 
  <Green's D-class: [2,5][6,7,3]>, <Green's D-class: [6,4][7,3](2)>, 
  <Green's D-class: [2,3][6,7,5]>, <Green's D-class: [6,4][7,2,3]>, 
  <Green's D-class: [2,3][7,1](6)>, <Green's D-class: [4,1][6,7,3]>, 
  <Green's D-class: [4,3][6,5][7,1]>, <Green's D-class: [4,5][6,1][7,3]>, 
  <Green's D-class: [4,3][6,2,5]>, <Green's D-class: [4,3][6,1](2)>, 
  <Green's D-class: [2,1][4,3][6,7]>, <Green's D-class: [2,3][4,1][6,5]>, 
  <Green's D-class: [4,5][6,2,1]>, <Green's D-class: [4,2,5][6,7,3]>, 
  <Green's D-class: [2,5][4,3][6,7]>, <Green's D-class: [4,5][6,3](2)>, 
  <Green's D-class: [4,2,1][6,3]>, <Green's D-class: [2,7][4,1][6,3]>, 
  <Green's D-class: [2,5][4,3][6,1]>, <Green's D-class: [4,3][6,7](2)>, 
  <Green's D-class: [2,1][6,4,3]>, <Green's D-class: [2,7][4,3](6)>, 
  <Green's D-class: [6,4,1][7,3](2)>, <Green's D-class: [4,5][6,2,3]>, 
  <Green's D-class: [4,2,3][6,1]>, <Green's D-class: [2,1][4,7][6,5]>, 
  <Green's D-class: [2,3][6,4,5]>, <Green's D-class: [4,2,3](6)>, 
  <Green's D-class: [2,1][4,7,3](6)>, <Green's D-class: [6,4,3][7,2]>, 
  <Green's D-class: [4,5][6,7,3]>, <Green's D-class: [6,4,2][7,3]>, 
  <Green's D-class: [4,1][7,3](6)>, <Green's D-class: [2,3][4,5][7,1](6)>, 
  <Green's D-class: [4,2,1][7,5,3](6)>, <Green's D-class: [4,5,3](7)>, 
  <Green's D-class: [4,7,2,5,1,3](6)>, <Green's D-class: [1,5][4,2][7,3]>, 
  <Green's D-class: [4,1,2][7,3]>, <Green's D-class: [4,7,3](1)>, 
  <Green's D-class: [4,5][7,1,3]>, <Green's D-class: [4,2][7,5](1)>, 
  <Green's D-class: [4,5](7)>, <Green's D-class: [1,5][4,7,2]>, 
  <Green's D-class: [1,5][4,7,3]>, <Green's D-class: [7,3](4)>, 
  <Green's D-class: [4,7,5]>, <Green's D-class: [4,7,2]>, 
  <Green's D-class: [7,5](4)>, <Green's D-class: [4,6][7,1]>, 
  <Green's D-class: [1,2][4,3][7,5]>, <Green's D-class: [4,3][7,2](1)>, 
  <Green's D-class: [4,3](1,7)>, <Green's D-class: [4,1,5][7,3]>, 
  <Green's D-class: [1,2][4,7,3]>, <Green's D-class: [7,3](1)(4)>, 
  <Green's D-class: [1,7,3][4,6]>, <Green's D-class: [7,1,2](4)>, 
  <Green's D-class: [1,3][4,2][7,5]>, <Green's D-class: [4,1,3][7,2]>, 
  <Green's D-class: [4,5](1)(7)>, <Green's D-class: [1,3][7,5](4)>, 
  <Green's D-class: [1,3][4,6][7,2]>, <Green's D-class: [4,6](1)(7)>, 
  <Green's D-class: [2,3][4,5](1)>, <Green's D-class: [1,5][4,3](2)>, 
  <Green's D-class: [4,3](1,2)>, <Green's D-class: [2,7][4,3](1)>, 
  <Green's D-class: [2,5][4,1,3]>, <Green's D-class: [1,3][4,7](2)>, 
  <Green's D-class: [2,1,3](4)>, <Green's D-class: [4,5][7,2,3]>, 
  <Green's D-class: [4,2,3][7,1]>, <Green's D-class: [2,1][4,7,5]>, 
  <Green's D-class: [7,2,5](4)>, <Green's D-class: [4,2,3][7,5]>, 
  <Green's D-class: [4,1][7,2,3]>, <Green's D-class: [2,3][4,7,1]>, 
  <Green's D-class: [2,1][4,5][7,3]>, <Green's D-class: [4,2,5][7,1]>, 
  <Green's D-class: [4,7,5](2)>, <Green's D-class: [2,3][4,7,5]>, 
  <Green's D-class: [4,3][7,2,5]>, <Green's D-class: [4,3][7,1](2)>, 
  <Green's D-class: [2,1][4,3](7)>, <Green's D-class: [2,3][4,1][7,5]>, 
  <Green's D-class: [4,7,2,3]>, <Green's D-class: [2,3][7,1](4)>, 
  <Green's D-class: [2,3][4,6](7)>, <Green's D-class: [7,2,1](4)>, 
  <Green's D-class: [4,2,5][7,3]>, <Green's D-class: [4,1][7,3](2)>, 
  <Green's D-class: [2,7,1][4,5]>, <Green's D-class: [2,5][7,3](4)>, 
  <Green's D-class: [4,6][7,3](2)>, <Green's D-class: [2,7,1][4,6]>, 
  <Green's D-class: [2,5][4,6][7,3]>, <Green's D-class: [4,6][7,1](2)>, 
  <Green's D-class: [1,3][2,7,5][4,6]> ]

# greens-generic: L-classes/reps, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(generic := true));;
gap> GreensLClasses(S);
[ <Green's L-class: Transformation( [ 2, 2, 2, 2, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 4, 4, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 2, 4, 4, 4, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 1, 1, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 4, 1, 1, 1, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 1, 2, 2, 2, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 1, 2, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 6, 6, 6, 6, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 5, 5, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 6, 5, 5, 5, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 5, 2, 2, 2, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 2, 6, 6, 6, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 6, 6, 2, 6, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 1, 6, 6, 6, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 6, 4, 4, 4, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 6, 4, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 4, 5, 5, 5, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 4, 5, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 5, 1, 1, 1, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 5, 1, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 2, 4, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 4, 4, 5, 4, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 5, 5, 6, 4 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 4, 1, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 1, 2, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 1, 1, 6, 4 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 2, 2, 6, 4 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 1, 2, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 2, 4, 5, 5, 1, 2 ] )>, 
  <Green's L-class: Transformation( [ 2, 5, 2, 2, 6, 1 ] )>, 
  <Green's L-class: Transformation( [ 2, 6, 6, 5, 1, 4 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 3, 3, 3, 3 ] )>, 
  <Green's L-class: Transformation( [ 5, 3, 3, 3, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 3, 2, 2, 2, 3, 3 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 3, 2, 5, 5 ] )>, 
  <Green's L-class: Transformation( [ 3, 4, 4, 4, 3, 3 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 4, 5, 3, 3 ] )>, 
  <Green's L-class: Transformation( [ 6, 3, 3, 3, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 6, 3, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 5, 3, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 3, 2, 4, 4 ] )>, 
  <Green's L-class: Transformation( [ 6, 6, 3, 3, 4, 5 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 2, 2, 4, 5 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 3, 2, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 2, 5, 6, 6, 3, 2 ] )>, 
  <Green's L-class: Transformation( [ 2, 6, 2, 2, 4, 3 ] )>, 
  <Green's L-class: Transformation( [ 3, 2, 5, 5, 6, 4 ] )>, 
  <Green's L-class: Transformation( [ 1, 3, 3, 3, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 1, 3, 6, 6 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 4, 3, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 3, 3, 6, 4 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 5, 1, 3, 3 ] )>, 
  <Green's L-class: Transformation( [ 5, 5, 1, 1, 3 ] )>, 
  <Green's L-class: Transformation( [ 1, 1, 4, 4, 5, 3 ] )>, 
  <Green's L-class: Transformation( [ 3, 5, 3, 4, 1 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 2, 3, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 3, 2, 4, 4, 2, 1 ] )>, 
  <Green's L-class: Transformation( [ 4, 2, 3, 1, 4, 2 ] )> ]
gap> LClassReps(S);
[ Transformation( [ 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 4, 4, 4, 4, 4, 4 ] ), 
  Transformation( [ 2, 4, 4, 4, 2, 2 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 1 ] ), 
  Transformation( [ 4, 1, 1, 1, 4, 4 ] ), 
  Transformation( [ 1, 2, 2, 2, 1, 1 ] ), 
  Transformation( [ 2, 2, 1, 2, 4, 4 ] ), 
  Transformation( [ 6, 6, 6, 6, 6, 6 ] ), 
  Transformation( [ 5, 5, 5, 5, 5, 5 ] ), 
  Transformation( [ 6, 5, 5, 5, 6, 6 ] ), 
  Transformation( [ 5, 2, 2, 2, 5, 5 ] ), 
  Transformation( [ 2, 6, 6, 6, 2, 2 ] ), 
  Transformation( [ 6, 6, 2, 6, 5, 5 ] ), 
  Transformation( [ 1, 6, 6, 6, 1, 1 ] ), 
  Transformation( [ 6, 4, 4, 4, 6, 6 ] ), 
  Transformation( [ 4, 4, 6, 4, 1, 1 ] ), 
  Transformation( [ 4, 5, 5, 5, 4, 4 ] ), 
  Transformation( [ 5, 5, 4, 5, 2, 2 ] ), 
  Transformation( [ 5, 1, 1, 1, 5, 5 ] ), 
  Transformation( [ 1, 1, 5, 1, 6, 6 ] ), 
  Transformation( [ 4, 4, 2, 4, 6, 6 ] ), 
  Transformation( [ 4, 4, 5, 4, 6, 6 ] ), 
  Transformation( [ 2, 2, 5, 5, 6, 4 ] ), 
  Transformation( [ 1, 1, 4, 1, 5, 5 ] ), 
  Transformation( [ 2, 2, 1, 2, 6, 6 ] ), 
  Transformation( [ 5, 5, 1, 1, 6, 4 ] ), 
  Transformation( [ 1, 1, 2, 2, 6, 4 ] ), 
  Transformation( [ 2, 2, 1, 2, 5, 5 ] ), 
  Transformation( [ 2, 4, 5, 5, 1, 2 ] ), 
  Transformation( [ 2, 5, 2, 2, 6, 1 ] ), 
  Transformation( [ 2, 6, 6, 5, 1, 4 ] ), 
  Transformation( [ 3, 3, 3, 3, 3, 3 ] ), 
  Transformation( [ 5, 3, 3, 3, 5, 5 ] ), 
  Transformation( [ 3, 2, 2, 2, 3, 3 ] ), 
  Transformation( [ 2, 2, 3, 2, 5, 5 ] ), 
  Transformation( [ 3, 4, 4, 4, 3, 3 ] ), 
  Transformation( [ 5, 5, 4, 5, 3, 3 ] ), 
  Transformation( [ 6, 3, 3, 3, 6, 6 ] ), 
  Transformation( [ 3, 3, 6, 3, 4, 4 ] ), 
  Transformation( [ 3, 3, 5, 3, 6, 6 ] ), 
  Transformation( [ 2, 2, 3, 2, 4, 4 ] ), 
  Transformation( [ 6, 6, 3, 3, 4, 5 ] ), 
  Transformation( [ 3, 3, 2, 2, 4, 5 ] ), 
  Transformation( [ 2, 2, 3, 2, 6, 6 ] ), 
  Transformation( [ 2, 5, 6, 6, 3, 2 ] ), 
  Transformation( [ 2, 6, 2, 2, 4, 3 ] ), 
  Transformation( [ 3, 2, 5, 5, 6, 4 ] ), 
  Transformation( [ 1, 3, 3, 3, 1, 1 ] ), 
  Transformation( [ 3, 3, 1, 3, 6, 6 ] ), 
  Transformation( [ 3, 3, 4, 3, 1, 1 ] ), 
  Transformation( [ 1, 1, 3, 3, 6, 4 ] ), 
  Transformation( [ 1, 1, 5, 1, 3, 3 ] ), Transformation( [ 5, 5, 1, 1, 3 ] ),
  Transformation( [ 1, 1, 4, 4, 5, 3 ] ), Transformation( [ 3, 5, 3, 4, 1 ] ),
  Transformation( [ 3, 3, 2, 3, 1, 1 ] ), 
  Transformation( [ 3, 2, 4, 4, 2, 1 ] ), 
  Transformation( [ 4, 2, 3, 1, 4, 2 ] ) ]

# greens-generic: R-classes/reps, 1/1
gap> S := OrderEndomorphisms(5);;
gap> S := Semigroup(S, rec(generic := true));
<transformation monoid of degree 5 with 5 generators>
gap> RClasses(S);
[ <Green's R-class: Transformation( [ 1, 1, 1, 1, 1 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 1, 1, 2 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 1, 3, 3 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 1, 2, 3 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 3, 3, 3 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 3, 3, 4 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 2, 4, 4 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 2, 3, 4 ] )>, 
  <Green's R-class: Transformation( [ 1, 3, 3, 3, 3 ] )>, 
  <Green's R-class: Transformation( [ 1, 3, 3, 3, 4 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 2, 4, 4 ] )>, 
  <Green's R-class: Transformation( [ 1, 3, 3 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 4, 4, 4 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 4, 4 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 3, 5, 5 ] )>, 
  <Green's R-class: IdentityTransformation> ]
gap> RClassReps(S);
[ Transformation( [ 1, 1, 1, 1, 1 ] ), Transformation( [ 1, 1, 1, 1, 2 ] ), 
  Transformation( [ 1, 1, 1, 3, 3 ] ), Transformation( [ 1, 1, 1, 2, 3 ] ), 
  Transformation( [ 1, 1, 3, 3, 3 ] ), Transformation( [ 1, 1, 3, 3, 4 ] ), 
  Transformation( [ 1, 1, 2, 4, 4 ] ), Transformation( [ 1, 1, 2, 3, 4 ] ), 
  Transformation( [ 1, 3, 3, 3, 3 ] ), Transformation( [ 1, 3, 3, 3, 4 ] ), 
  Transformation( [ 1, 2, 2, 4, 4 ] ), Transformation( [ 1, 3, 3 ] ), 
  Transformation( [ 1, 2, 4, 4, 4 ] ), Transformation( [ 1, 2, 4, 4 ] ), 
  Transformation( [ 1, 2, 3, 5, 5 ] ), IdentityTransformation ]

# greens-generic: R-reps, 1/1
gap> S := OrderEndomorphisms(5);;
gap> S := Semigroup(S, rec(generic := true));;
gap> D := DClass(S, S.2 * S.1);
<Green's D-class: Transformation( [ 1, 1, 2, 3, 4 ] )>
gap> RClassReps(D);
[ Transformation( [ 1, 1, 2, 3, 4 ] ), Transformation( [ 1, 2, 2, 3, 4 ] ), 
  Transformation( [ 1, 2, 3, 3, 4 ] ), Transformation( [ 1, 2, 3, 4, 4 ] ) ]
gap> LClassReps(D);
[ Transformation( [ 1, 1, 2, 3, 4 ] ), Transformation( [ 1, 2, 3, 5, 5 ] ), 
  Transformation( [ 1, 2, 4, 4 ] ), Transformation( [ 1, 3, 3 ] ), 
  Transformation( [ 2, 2 ] ) ]

# greens-generic: H-classes/reps, 1/3
gap> S := Monoid(
> [Transformation([2, 2, 2, 2, 2, 2, 2, 2, 2, 4]),
>  Transformation([2, 2, 2, 2, 2, 2, 2, 4, 2, 4]),
>  Transformation([2, 2, 2, 2, 2, 2, 2, 4, 4, 2]),
>  Transformation([2, 2, 2, 2, 2, 2, 2, 4, 4, 4]),
>  Transformation([2, 2, 2, 2, 2, 2, 4, 4, 2, 2]),
>  Transformation([2, 2, 2, 2, 2, 2, 4, 4, 4, 2]),
>  Transformation([2, 2, 2, 2, 2, 4, 2, 2, 2, 4]),
>  Transformation([2, 2, 2, 2, 2, 4, 2, 2, 4, 4]),
>  Transformation([2, 2, 2, 2, 2, 4, 4, 2, 4, 2]),
>  Transformation([2, 2, 2, 4, 2, 2, 2, 4, 2, 2]),
>  Transformation([2, 2, 2, 4, 2, 2, 7, 4, 2, 4]),
>  Transformation([2, 2, 3, 4, 2, 4, 7, 2, 9, 4]),
>  Transformation([2, 2, 3, 4, 2, 6, 2, 2, 9, 2]),
>  Transformation([2, 2, 3, 4, 2, 6, 7, 2, 2, 4]),
>  Transformation([2, 2, 3, 4, 2, 6, 7, 2, 9, 4]),
>  Transformation([2, 2, 4, 2, 2, 2, 2, 2, 2, 4]),
>  Transformation([2, 2, 4, 2, 2, 2, 2, 4, 2, 2]),
>  Transformation([2, 2, 4, 2, 2, 2, 2, 4, 2, 4]),
>  Transformation([2, 2, 4, 2, 2, 2, 4, 4, 2, 2]),
>  Transformation([2, 2, 9, 4, 2, 4, 7, 2, 2, 4]),
>  Transformation([3, 2, 2, 2, 2, 2, 2, 9, 4, 2]),
>  Transformation([3, 2, 2, 2, 2, 2, 2, 9, 4, 4]),
>  Transformation([3, 2, 2, 2, 2, 2, 4, 9, 4, 2]),
>  Transformation([4, 2, 2, 2, 2, 2, 2, 3, 2, 2]),
>  Transformation([4, 2, 2, 2, 2, 2, 2, 3, 2, 4]),
>  Transformation([4, 2, 2, 2, 2, 2, 4, 3, 2, 2]),
>  Transformation([4, 2, 4, 2, 2, 2, 2, 3, 2, 2]),
>  Transformation([4, 2, 4, 2, 2, 2, 2, 3, 2, 4]),
>  Transformation([4, 2, 4, 2, 2, 2, 4, 3, 2, 2]),
>  Transformation([5, 5, 5, 5, 5, 5, 5, 5, 5, 5])],
>  rec(generic := true));;
gap> HClassReps(S);
[ Transformation( [ 5, 5, 5, 5, 5, 5, 5, 5, 5, 5 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 4, 2, 4 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 4, 4, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 4, 4, 4 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 4, 4, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 4, 4, 4, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 4, 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 4, 2, 2, 4, 4 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 4, 4, 2, 4, 2 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 2, 2, 4, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 4, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 2, 2, 4, 2, 4 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 2, 7, 4, 2, 4 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 4, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 4, 2, 4, 2 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 4, 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 4, 7, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 4, 2, 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 9, 4, 2, 4, 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 9, 4 ] ), 
  Transformation( [ 2, 2, 4, 2, 2, 2, 4, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 9, 4, 2, 4, 7, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 4, 7, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 4, 7, 2, 9, 4 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 4, 2, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 4, 2, 2, 4, 2 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 4, 2, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 9, 4, 2, 4, 2, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 9, 2 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 6, 2, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 6, 2, 2, 9, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 4, 4, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 2, 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 2, 4, 2, 2, 7, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 6, 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 6, 7, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 6, 2, 2, 9, 4 ] ), 
  Transformation( [ 2, 2, 3, 4, 2, 6, 7, 2, 9, 4 ] ), 
  Transformation( [ 2, 2, 4, 2, 2, 2, 2, 2, 2, 4 ] ), 
  Transformation( [ 2, 2, 4, 2, 2, 2, 2, 4, 2, 2 ] ), 
  Transformation( [ 2, 2, 4, 2, 2, 2, 2, 4, 2, 4 ] ), 
  Transformation( [ 2, 2, 4, 2, 2, 2, 4, 4, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 4, 2, 2 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 9, 2, 2, 2, 2, 2, 2, 2, 4, 2 ] ), 
  Transformation( [ 3, 2, 2, 2, 2, 2, 2, 2, 4, 2 ] ), 
  Transformation( [ 3, 2, 2, 2, 2, 2, 2, 9, 4, 2 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 4, 4 ] ), 
  Transformation( [ 9, 2, 2, 2, 2, 2, 2, 2, 4, 4 ] ), 
  Transformation( [ 3, 2, 2, 2, 2, 2, 2, 2, 4, 4 ] ), 
  Transformation( [ 3, 2, 2, 2, 2, 2, 2, 9, 4, 4 ] ), 
  Transformation( [ 9, 2, 2, 2, 2, 2, 4, 2, 4, 2 ] ), 
  Transformation( [ 3, 2, 2, 2, 2, 2, 4, 2, 4, 2 ] ), 
  Transformation( [ 3, 2, 2, 2, 2, 2, 4, 9, 4, 2 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 2, 9, 2, 2 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 2, 3, 2, 2 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 2, 2, 2, 4 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 2, 9, 2, 4 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 2, 3, 2, 4 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 4, 2, 2, 2 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 4, 9, 2, 2 ] ), 
  Transformation( [ 4, 2, 2, 2, 2, 2, 4, 3, 2, 2 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 2, 9, 2, 2 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 2, 3, 2, 2 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 2, 2, 2, 4 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 2, 9, 2, 4 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 2, 3, 2, 4 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 4, 2, 2, 2 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 4, 9, 2, 2 ] ), 
  Transformation( [ 4, 2, 4, 2, 2, 2, 4, 3, 2, 2 ] ), IdentityTransformation ]
gap> HClasses(S);
[ <Green's H-class: Transformation( [ 5, 5, 5, 5, 5, 5, 5, 5, 5, 5 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ] )>, 
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
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 4, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 2, 2, 4, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 2, 7, 4, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 4, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 4, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 4, 7, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 4, 2, 2, 2, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 9, 4, 2, 4, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 9, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 4, 2, 2, 2, 4, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 9, 4, 2, 4, 7, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 4, 7, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 4, 7, 2, 9, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 4, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 4, 2, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 2, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 4, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 9, 4, 2, 4, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 4, 2, 2, 9, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 6, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 6, 2, 2, 9, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 4, 4, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 2, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 4, 2, 2, 7, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 6, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 6, 7, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 6, 2, 2, 9, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 3, 4, 2, 6, 7, 2, 9, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 4, 2, 2, 2, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 4, 2, 2, 2, 2, 4, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 4, 2, 2, 2, 2, 4, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 4, 2, 2, 2, 4, 4, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 2, 4, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 9, 2, 2, 2, 2, 2, 2, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 2, 2, 2, 2, 2, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 2, 2, 2, 2, 2, 9, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 9, 2, 2, 2, 2, 2, 2, 2, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 2, 2, 2, 2, 2, 2, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 2, 2, 2, 2, 2, 9, 4, 4 ] )>, 
  <Green's H-class: Transformation( [ 9, 2, 2, 2, 2, 2, 4, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 2, 2, 2, 2, 4, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 2, 2, 2, 2, 4, 9, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 2, 9, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 2, 3, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 2, 9, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 2, 3, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 4, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 4, 9, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 2, 2, 2, 4, 3, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 2, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 2, 9, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 2, 3, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 2, 2, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 2, 9, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 2, 3, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 4, 2, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 4, 9, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 4, 2, 2, 2, 4, 3, 2, 2 ] )>, 
  <Green's H-class: IdentityTransformation> ]
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

# greens-generic: H-classes/reps, 2/3
gap> S := Semigroup(FullTransformationMonoid(5), rec(generic := true));;
gap> x := Transformation([1, 1, 2, 3, 4]);;
gap> L := LClass(S, x);;
gap> GreensHClasses(L);
[ <Green's H-class: Transformation( [ 2, 1, 3, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 1, 3, 4, 2, 2 ] )>, 
  <Green's H-class: Transformation( [ 3, 4, 2, 2, 1 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 2, 1, 3 ] )>, 
  <Green's H-class: Transformation( [ 2, 2, 1, 3, 4 ] )>, 
  <Green's H-class: Transformation( [ 2, 4, 2, 1, 3 ] )>, 
  <Green's H-class: Transformation( [ 4, 2, 1, 3, 2 ] )>, 
  <Green's H-class: Transformation( [ 2, 1, 3, 2, 4 ] )>, 
  <Green's H-class: Transformation( [ 1, 3, 2, 4, 2 ] )>, 
  <Green's H-class: Transformation( [ 3, 2, 4, 2, 1 ] )> ]

# greens-generic: NrXClasses, 1/1
gap> S := Semigroup(SymmetricInverseMonoid(5));;
gap> NrRClasses(S);
32
gap> NrDClasses(S);
6
gap> NrLClasses(S);
32
gap> NrHClasses(S);
252

# greens-generic: NrXClasses for a D-class, 1/1
gap> S := Semigroup(SymmetricInverseMonoid(5));;
gap> D := DClass(S, S.2);;
gap> NrRClasses(D);
1
gap> NrLClasses(D);
1
gap> R := RClass(S, S.2);;
gap> NrHClasses(R);
1
gap> L := LClass(S, S.2);;
gap> NrHClasses(L);
1

# greens-generic: GreensXClasses, for an infinite semigroup, 1/1
gap> S := FreeSemigroup(2);;
gap> GreensLClasses(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `CayleyGraphDualSemigroup' on 1 argument\
s
gap> GreensRClasses(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `CayleyGraphSemigroup' on 1 arguments
gap> GreensHClasses(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `CayleyGraphDualSemigroup' on 1 argument\
s
gap> GreensDClasses(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `CayleyGraphDualSemigroup' on 1 argument\
s

# greens-generic: GreensHClasses, fail, 1/1
gap> S := Semigroup(SymmetricInverseMonoid(3));;
gap> H := HClass(S, S.2);;
gap> GreensHClasses(H);
Error, Semigroups: GreensHClasses (for a generic semigroup Green's class): usa\
ge,
the argument should be a Green's R-, L-, or D-class,

# greens-generic: GroupHClass, IsGroupHClass, IsomorphismPermGroup, 1/1
gap> S := AsTransformationSemigroup(FullBooleanMatMonoid(4));;
gap> S := Semigroup(S, rec(generic := true));;
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
MappingByFunction( <Green's H-class: IdentityTransformation>, Group([ (1,2)
(3,5)(4,7)(6,10)(8,13)(9,14)(11,12)(15,19)(16,20)(17,22)(18,23)
(21,24), (1,3,6,11)(2,4,8,14)(5,9,15,20)(7,12,17,23)(10,16,21,22)
(13,18,24,19) ]), function( a ) ... end )
gap> IsomorphismPermGroup(HClass(S, S.1));
Error, can only create isomorphisms of group H-classes

# greens-generic: PartialOrderOfDClasses, 1/2
gap> S := AsTransformationSemigroup(FullBooleanMatMonoid(3));;
gap> S := Semigroup(S, rec(generic := true));;
gap> PartialOrderOfDClasses(S);
[ [ 1 ], [ 1, 2 ], [ 2, 3 ], [ 3, 4 ], [ 2, 3, 5 ], [ 3, 4, 5, 6 ], 
  [ 3, 4, 5, 7 ], [ 3, 4, 5, 8 ], [ 3, 4, 5, 6, 7, 8, 9 ], [ 2, 5, 10 ], 
  [ 5, 9, 10, 11 ] ]

# greens-generic: Idempotents, 1/?
gap> S := AsTransformationSemigroup(FullPBRMonoid(1));;
gap> S := Semigroup(S, rec(generic := true));;
gap> Idempotents(S);
[ Transformation( [ 1, 2, 2, 1, 5, 6, 6, 5, 5, 6, 6, 5, 1, 2, 2, 1 ] ), 
  Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 10, 11 ] ), 
  Transformation( [ 1, 1, 1, 1, 5, 5, 5, 5, 9, 9, 9, 9, 13, 13, 13, 13 ] ), 
  IdentityTransformation, Transformation( [ 4, 3, 3, 4, 8, 7, 7, 8, 12, 11,
      11, 12, 16, 15, 15, 16 ] ), 
  Transformation( [ 1, 1, 1, 1, 5, 5, 5, 5, 5, 5, 5, 5, 1, 1, 1, 1 ] ), 
  Transformation( [ 4, 3, 3, 4, 8, 7, 7, 8, 8, 7, 7, 8, 4, 3, 3, 4 ] ), 
  Transformation( [ 1, 2, 2, 1, 5, 6, 6, 5, 5, 6, 6, 5, 1, 6, 6, 1 ] ), 
  Transformation( [ 1, 1, 1, 1, 5, 5, 5, 5, 9, 9, 9, 9, 13, 9, 9, 13 ] ), 
  Transformation( [ 4, 3, 3, 4, 8, 7, 7, 8, 12, 11, 11, 12, 16, 11, 11, 16 ] )
    , Transformation( [ 4, 4, 4, 4, 8, 8, 8, 8, 12, 12, 12, 12, 16, 16, 16,
      16 ] ), Transformation( [ 4, 4, 4, 4, 8, 8, 8, 8, 8, 8, 8, 8, 4, 4, 4,
      4 ] ), Transformation( [ 1, 1, 1, 1, 5, 5, 5, 5, 5, 5, 5, 5, 1, 5, 5,
      1 ] ), Transformation( [ 4, 3, 3, 4, 8, 7, 7, 8, 8, 7, 7, 8, 4, 7, 7,
      4 ] ), Transformation( [ 4, 4, 4, 4, 8, 8, 8, 8, 12, 12, 12, 12, 16, 12,
     12, 16 ] ), Transformation( [ 4, 4, 4, 4, 8, 8, 8, 8, 8, 8, 8, 8, 4, 8,
      8, 4 ] ) ]

# greens-generic: Idempotents, 2/2
gap> S := Semigroup(FullTransformationMonoid(3),
> rec(generic := true));;
gap> RClasses(S);;
gap> Idempotents(S);
[ IdentityTransformation, Transformation( [ 1, 2, 1 ] ), 
  Transformation( [ 1, 2, 2 ] ), Transformation( [ 3, 2, 3 ] ), 
  Transformation( [ 2, 2 ] ), Transformation( [ 1, 3, 3 ] ), 
  Transformation( [ 1, 1, 1 ] ), Transformation( [ 1, 1 ] ), 
  Transformation( [ 2, 2, 2 ] ), Transformation( [ 3, 3, 3 ] ) ]

# greens-generic: Idempotents, for a D-class, 1/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> IsRegularDClass(D);
false
gap> Idempotents(D);
[  ]
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> Idempotents(S);;
gap> Idempotents(D);
[  ]

# greens-generic: Idempotents, for a D-class, 2/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> Idempotents(D);
[  ]

# greens-generic: Idempotents, for a L-class, 1/3
gap> S := Semigroup(FullTransformationMonoid(5), rec(generic := true));;
gap> x := Transformation([1, 1, 2, 3, 4]);;
gap> L := LClass(S, x);;
gap> Idempotents(L);
[ Transformation( [ 1, 2, 3, 4, 2 ] ), Transformation( [ 1, 2, 3, 4, 1 ] ), 
  Transformation( [ 1, 2, 3, 4, 4 ] ), Transformation( [ 1, 2, 3, 4, 3 ] ) ]

# greens-generic: Idempotents, for a L-class, 2/3
gap> S := AsTransformationSemigroup(FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> L := LClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsRegularClass(L);
false
gap> Idempotents(L);
[  ]

# greens-generic: Idempotents, for a L-class, 3/3
gap> S := PartitionMonoid(3);;
gap> L := LClass(S, One(S));;
gap> Idempotents(L);
[ <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ]> ]

# greens-generic: Idempotents, for a H-class, 1/2
gap> S := SingularTransformationSemigroup(4);;
gap> H := HClass(S, S.1);
<Green's H-class: Transformation( [ 1, 2, 3, 3 ] )>
gap> Idempotents(H);
[ Transformation( [ 1, 2, 3, 3 ] ) ]

# greens-generic: Idempotents, for a H-class, 1/2
gap> S := AsTransformationSemigroup(FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> H := HClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsGroupHClass(H);
false
gap> Idempotents(H);
[  ]

# greens-generic: NrIdempotents, for a semigroup, 1/2
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> NrIdempotents(S);
24

# greens-generic: NrIdempotents, for a semigroup, 2/2
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> Idempotents(S);;
gap> NrIdempotents(S);
24

# greens-generic: NrIdempotents, for a D-class, 1/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> IsRegularDClass(D);
false
gap> NrIdempotents(D);
0

# greens-generic: NrIdempotents, for a D-class, 2/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> NrIdempotents(D);
0

# greens-generic: NrIdempotents, for a L-class, 1/3
gap> S := Semigroup(FullTransformationMonoid(5), rec(generic := true));;
gap> x := Transformation([1, 1, 2, 3, 4]);;
gap> L := LClass(S, x);;
gap> NrIdempotents(L);
4

# greens-generic: NrIdempotents, for a L-class, 2/3
gap> S := AsTransformationSemigroup(FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> L := LClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsRegularClass(L);
false
gap> NrIdempotents(L);
0

# greens-generic: NrIdempotents, for a L-class, 3/3
gap> S := PartitionMonoid(3);;
gap> L := LClass(S, One(S));;
gap> NrIdempotents(L);
1

# greens-generic: NrIdempotents, for a H-class, 1/2
gap> S := SingularTransformationSemigroup(4);;
gap> H := HClass(S, S.1);
<Green's H-class: Transformation( [ 1, 2, 3, 3 ] )>
gap> NrIdempotents(H);
1

# greens-generic: NrIdempotents, for a H-class, 1/2
gap> S := AsTransformationSemigroup(FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> H := HClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsGroupHClass(H);
false
gap> NrIdempotents(H);
0

# greens-generic: NrIdempotents, for an R-class, 1/2
gap> S := Semigroup(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                    Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> R := First(RClasses(S),
> x -> Transformation([9, 10, 4, 9, 10, 4, 4, 3, 3, 6]) in x);;
gap> NrIdempotents(R);
0
gap> IsRegularClass(R);
false

# greens-generic: NrIdempotents, for an R-class, 3/3
gap> S := Semigroup(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                    Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> R := RClass(S, Transformation([6, 9, 9, 6, 9, 1, 1, 2, 2, 6]));;
gap> IsRegularClass(R);
true
gap> NrIdempotents(R);
7

# greens-generic: IsRegularClass, for an R-class, 1/1
gap> S := Semigroup(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                    Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> R := First(RClasses(S),
> x -> Transformation([9, 10, 4, 9, 10, 4, 4, 3, 3, 6]) in x);;
gap> IsRegularClass(R);
false

# greens-generic: IsRegularClass, for an R-class in group of units, 1/1
gap> S := Monoid(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                 Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> S := AsBipartitionSemigroup(S);;
gap> R := RClass(S, IdentityBipartition(10));;
gap> IsRegularClass(R);
true

# greens-generic: NrRegularDClasses, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(generic := true));;
gap> NrRegularDClasses(S);
6

# greens-generic: ViewString, PrintString, for Green's relations, 1/1
gap> S := FullTransformationMonoid(3);
<full transformation monoid of degree 3>
gap> GreensRRelation(S);
<Green's R-relation of <full transformation monoid of degree 3>>
gap> GreensLRelation(S);
<Green's L-relation of <full transformation monoid of degree 3>>
gap> GreensHRelation(S);
<Green's H-relation of <full transformation monoid of degree 3>>
gap> GreensDRelation(S);
<Green's D-relation of <full transformation monoid of degree 3>>
gap> GreensJRelation(S);
<Green's D-relation of <full transformation monoid of degree 3>>
gap> PrintString((GreensRRelation(S)));
"\>\>\>GreensRRelation\<(\>\nMonoid( \>[ Transformation( [ 2, 3, 1 ] ), Transf\
ormation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ]\<\> )\<\<)\<\<"
gap> PrintString((GreensLRelation(S)));
"\>\>\>GreensLRelation\<(\>\nMonoid( \>[ Transformation( [ 2, 3, 1 ] ), Transf\
ormation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ]\<\> )\<\<)\<\<"
gap> PrintString((GreensHRelation(S)));
"\>\>\>GreensHRelation\<(\>\nMonoid( \>[ Transformation( [ 2, 3, 1 ] ), Transf\
ormation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ]\<\> )\<\<)\<\<"
gap> PrintString((GreensDRelation(S)));
"\>\>\>GreensDRelation\<(\>\nMonoid( \>[ Transformation( [ 2, 3, 1 ] ), Transf\
ormation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ]\<\> )\<\<)\<\<"

# greens-generic: ViewString, PrintString, for Green's classes, 1/1
gap> S := FullBooleanMatMonoid(3);;
gap> PrintString(RClass(S, S.2));
"\>\>\>GreensRClassOfElement\<(\>Monoid( \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\
\>\>[0, 1, 0]\<, \<\>\>[1, 0, 0]\<, \<\>\>[0, 0, 1]\<\<]\<)\<\>\>\>Matrix(\<\>\
IsBooleanMat\<, \>[\>\>[0, 1, 0]\<, \<\>\>[0, 0, 1]\<, \<\>\>[1, 0, 0]\<\<]\<)\
\<\<, \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[1, 0, 0]\<, \<\>\>[0, 1, 0]\<, \
\<\>\>[1, 0, 1]\<\<]\<)\<\<, \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[1, 0, 0]\
\<, \<\>\>[0, 1, 0]\<, \<\>\>[0, 0, 0]\<\<]\<)\<\<, \>\>\>Matrix(\<\>IsBoolean\
Mat\<, \>[\>\>[1, 1, 0]\<, \<\>\>[1, 0, 1]\<, \<\>\>[0, 1, 1]\<\<]\<)\<\<\<\> \
)\<,\< \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[0, 1, 0]\<, \<\>\>[0, 0, 1]\<,\
 \<\>\>[1, 0, 0]\<\<]\<)\<\<)\<\<"
gap> PrintString(LClass(S, S.2));
"\>\>\>GreensLClassOfElement\<(\>Monoid( \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\
\>\>[0, 1, 0]\<, \<\>\>[1, 0, 0]\<, \<\>\>[0, 0, 1]\<\<]\<)\<\>\>\>Matrix(\<\>\
IsBooleanMat\<, \>[\>\>[0, 1, 0]\<, \<\>\>[0, 0, 1]\<, \<\>\>[1, 0, 0]\<\<]\<)\
\<\<, \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[1, 0, 0]\<, \<\>\>[0, 1, 0]\<, \
\<\>\>[1, 0, 1]\<\<]\<)\<\<, \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[1, 0, 0]\
\<, \<\>\>[0, 1, 0]\<, \<\>\>[0, 0, 0]\<\<]\<)\<\<, \>\>\>Matrix(\<\>IsBoolean\
Mat\<, \>[\>\>[1, 1, 0]\<, \<\>\>[1, 0, 1]\<, \<\>\>[0, 1, 1]\<\<]\<)\<\<\<\> \
)\<,\< \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[0, 1, 0]\<, \<\>\>[0, 0, 1]\<,\
 \<\>\>[1, 0, 0]\<\<]\<)\<\<)\<\<"
gap> PrintString(HClass(S, S.2));
"\>\>\>GreensHClassOfElement\<(\>Monoid( \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\
\>\>[0, 1, 0]\<, \<\>\>[1, 0, 0]\<, \<\>\>[0, 0, 1]\<\<]\<)\<\>\>\>Matrix(\<\>\
IsBooleanMat\<, \>[\>\>[0, 1, 0]\<, \<\>\>[0, 0, 1]\<, \<\>\>[1, 0, 0]\<\<]\<)\
\<\<, \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[1, 0, 0]\<, \<\>\>[0, 1, 0]\<, \
\<\>\>[1, 0, 1]\<\<]\<)\<\<, \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[1, 0, 0]\
\<, \<\>\>[0, 1, 0]\<, \<\>\>[0, 0, 0]\<\<]\<)\<\<, \>\>\>Matrix(\<\>IsBoolean\
Mat\<, \>[\>\>[1, 1, 0]\<, \<\>\>[1, 0, 1]\<, \<\>\>[0, 1, 1]\<\<]\<)\<\<\<\> \
)\<,\< \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[0, 1, 0]\<, \<\>\>[0, 0, 1]\<,\
 \<\>\>[1, 0, 0]\<\<]\<)\<\<)\<\<"
gap> PrintString(DClass(S, S.2));
"\>\>\>GreensDClassOfElement\<(\>Monoid( \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\
\>\>[0, 1, 0]\<, \<\>\>[1, 0, 0]\<, \<\>\>[0, 0, 1]\<\<]\<)\<\>\>\>Matrix(\<\>\
IsBooleanMat\<, \>[\>\>[0, 1, 0]\<, \<\>\>[0, 0, 1]\<, \<\>\>[1, 0, 0]\<\<]\<)\
\<\<, \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[1, 0, 0]\<, \<\>\>[0, 1, 0]\<, \
\<\>\>[1, 0, 1]\<\<]\<)\<\<, \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[1, 0, 0]\
\<, \<\>\>[0, 1, 0]\<, \<\>\>[0, 0, 0]\<\<]\<)\<\<, \>\>\>Matrix(\<\>IsBoolean\
Mat\<, \>[\>\>[1, 1, 0]\<, \<\>\>[1, 0, 1]\<, \<\>\>[0, 1, 1]\<\<]\<)\<\<\<\> \
)\<,\< \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[0, 1, 0]\<, \<\>\>[0, 0, 1]\<,\
 \<\>\>[1, 0, 0]\<\<]\<)\<\<)\<\<"

#E# 
gap> STOP_TEST("Semigroups package: standard/greens-generic.tst");
