#############################################################################
##
#W  standard/greens/froidure-pin.tst
#Y  Copyright (C) 2011-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local CheckLeftGreensMultiplier1, CheckLeftGreensMultiplier2
#@local CheckRightGreensMultiplier1, CheckRightGreensMultiplier2, D, DD, DDD, H
#@local J, L, L3, LL, R, RR, RRR, S, a, acting, an, b, gens, map, x, y
#@local c, d, e, F
gap> START_TEST("Semigroups package: standard/greens/froidure-pin.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# AsSSortedList, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> L := LClass(S, PartialPerm([1, 7], [3, 5]));;
gap> AsSet(L);
[ [1,3][2,5], [1,5][2,3], [2,3,5], [2,5](3), [1,3,5], [1,5](3), 
  <identity partial perm on [ 3, 5 ]>, (3,5), [2,3](5), [2,5,3], [1,3](5), 
  [1,5,3], [7,5](3), [7,3,5], [1,3][7,5], [1,5][7,3] ]

# \< and \=, 1/1
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
true
gap> LL < L;
false
gap> D := DClass(L);;
gap> L = D;
false
gap> L < D;
false

# Test \< for H-classes Issue #198
gap> S := FullTropicalMinPlusMonoid(2, 2);
<monoid of 2x2 tropical min-plus matrices with 6 generators>
gap> H := Set(GeneratorsOfSemigroup(S), x -> HClass(S, x));
[ <Green's H-class: Matrix(IsTropicalMinPlusMatrix, [[0, infinity], 
       [infinity, 0]], 2)>, 
  <Green's H-class: Matrix(IsTropicalMinPlusMatrix, [[infinity, 0], [0, 0]], 2
       )>, 
  <Green's H-class: Matrix(IsTropicalMinPlusMatrix, [[infinity, 0], [0, 1]], 2
       )>, 
  <Green's H-class: Matrix(IsTropicalMinPlusMatrix, [[infinity, 0], [0, 2]], 2
       )>, 
  <Green's H-class: Matrix(IsTropicalMinPlusMatrix, [[infinity, 0], 
       [1, infinity]], 2)>, 
  <Green's H-class: Matrix(IsTropicalMinPlusMatrix, [[infinity, 0], 
       [infinity, infinity]], 2)> ]

# Test \< for H-classes Issue #198
gap> gens := [
>  Transformation([1, 3, 4, 1, 5, 5, 5]),
>  Transformation([1, 4, 1, 3, 5, 5, 5]),
>  Transformation([3, 3, 1, 2, 5, 5, 5]),
>  Transformation([4, 4, 2, 3, 5, 5, 5]),
>  Transformation([1, 1, 3, 4, 5, 5, 6]),
>  Transformation([1, 2, 2]),
>  Transformation([1, 4, 3, 4]),
>  Transformation([1, 2, 4, 4])];;
gap> S := Semigroup(gens);
<transformation semigroup of degree 7 with 8 generators>
gap> D := DClass(S, gens[1]);;
gap> ForAll(gens{[1 .. 4]}, x -> x in D);
true
gap>  NrRClasses(D);
6
gap> R := List(gens{[1 .. 3]}, x -> RClass(S, x));;
gap> IsDuplicateFreeList(R);
true
gap> x := Difference(RClasses(D), R);;
gap> Length(x);
3

# MultiplicativeNeutralElement, One, for an H-class, 1
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

# MultiplicativeNeutralElement, One, for an H-class, 2
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

# StructureDescription, for an H-class, 1/1
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
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(acting := false));;
gap> GreensDClassOfElement(S, PartialPerm([19]));
Error, the 2nd argument (a mult. elt.) does not belong to the source of the 1s\
t argument (a Green's relation)

# GreensDClassOfElementNC, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(acting := false));;
gap> D := GreensDClassOfElementNC(S, PartialPerm([19]));;
Error, the 2nd argument (a mult. elt.) does not belong to the source of the 1s\
t argument (a Green's relation)

# GreensJClassOfElement, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(acting := false));;
gap> J := GreensJClassOfElement(S, S.2);
<Green's D-class: [6,4,7,1,2,5](3)>

# GreensL/RClassOfElement, fail, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(acting := false));;
gap> RClass(S, ConstantTransformation(7, 7));
Error, the 2nd argument (a mult. elt.) does not belong to the source of the 1s\
t argument (a Green's relation)
gap> LClass(S, ConstantTransformation(7, 7));
Error, the 2nd argument (a mult. elt.) does not belong to the source of the 1s\
t argument (a Green's relation)
gap> HClass(S, ConstantTransformation(7, 7));
Error, the 2nd argument (a mult. elt.) does not belong to the source of the 1s\
t argument (a Green's relation)

# GreensL/RClassOfElementNC, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(acting := false));;
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
> rec(acting := false));;
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
Error, the 2nd argument (a mult. elt.) does not belong to the source of the 1s\
t argument (a Green's relation)
gap> RClass(D, IdentityBipartition(8));
Error, the 2nd argument (a mult. elt.) does not belong to the source of the 1s\
t argument (a Green's relation)
gap> x := Bipartition([[1, 4, -1, -2, -6], [2, 3, 5, -4],
> [6, -3], [-5]]);;
gap> LClassNC(D, x);
Error, the 2nd argument (a mult. elt.) does not belong to the source of the 1s\
t argument (a Green's relation)
gap> RClassNC(D, x);
Error, the 2nd argument (a mult. elt.) does not belong to the source of the 1s\
t argument (a Green's relation)

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
gap> S := Semigroup(S, rec(acting := false));;
gap> D := DClass(S, S.4 * S.5);;
gap> H := HClass(D, MultiplicativeZero(S));
<Green's H-class: 0>
gap> H := HClassNC(D, MultiplicativeZero(S));
<Green's H-class: 0>
gap> H := HClass(D, IdentityTransformation);
Error, the 2nd argument (a mult. elt.) does not belong to the source of the 1s\
t argument (a Green's relation)

# GreensHClassOfElement(L/R-class, x), 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(acting := false));;
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
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(acting := false));;
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

# L-classes/reps, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(acting := false));;
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

# R-classes/reps, 1/1
gap> S := OrderEndomorphisms(5);;
gap> S := Semigroup(S, rec(acting := false));
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

# R-reps, 1/1
gap> S := OrderEndomorphisms(5);;
gap> S := Semigroup(S, rec(acting := false));;
gap> D := DClass(S, S.2 * S.1);
<Green's D-class: Transformation( [ 1, 1, 2, 3, 4 ] )>
gap> RClassReps(D);
[ Transformation( [ 1, 1, 2, 3, 4 ] ), Transformation( [ 1, 2, 2, 3, 4 ] ), 
  Transformation( [ 1, 2, 3, 3, 4 ] ), Transformation( [ 1, 2, 3, 4, 4 ] ) ]
gap> LClassReps(D);
[ Transformation( [ 1, 1, 2, 3, 4 ] ), Transformation( [ 1, 2, 3, 5, 5 ] ), 
  Transformation( [ 1, 2, 4, 4 ] ), Transformation( [ 1, 3, 3 ] ), 
  Transformation( [ 2, 2 ] ) ]

# H-classes/reps, 1/3
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
>  rec(acting := false));;
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

# H-classes/reps, 2/3
gap> S := Semigroup(FullTransformationMonoid(5), rec(acting := false));;
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

# NrXClasses for a D-class, 1/1
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

# GreensXClasses, for an infinite semigroup, 1/1
gap> S := FreeSemigroup(2);;
gap> GreensLClasses(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `CayleyGraphDualSemigroup' on 1 argument\
s
gap> GreensRClasses(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `CayleyGraphSemigroup' on 1 arguments
gap> GreensHClasses(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `CayleyGraphDualSemigroup' on 1 argument\
s
gap> GreensDClasses(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `CayleyGraphDualSemigroup' on 1 argument\
s

# GreensHClasses, fail, 1/1
gap> S := Semigroup(SymmetricInverseMonoid(3));;
gap> H := HClass(S, S.2);;
gap> GreensHClasses(H);
Error, the argument is not a Green's R-, L-, or D-class

# GroupHClass, IsGroupHClass, IsomorphismPermGroup, 1/1
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(4));;
gap> S := Semigroup(S, rec(acting := false));;
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
gap> Length(GeneratorsOfGroup(Range(x)));
2
gap> GeneratorsOfGroup(Range(x))[1];
(1,2)(3,5)(4,7)(6,10)(8,13)(9,14)(11,12)(15,19)(16,20)(17,22)(18,23)(21,24)
gap> GeneratorsOfGroup(Range(x))[2];
(1,3,6,11)(2,4,8,14)(5,9,15,20)(7,12,17,23)(10,16,21,22)(13,18,24,19)
gap> IsomorphismPermGroup(HClass(S, S.1));
Error, the argument (a Green's H-class) is not a group

# PartialOrderOfDClasses, 1/2
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(3));;
gap> S := Semigroup(S, rec(acting := false));;
gap> PartialOrderOfDClasses(S);
<immutable digraph with 11 vertices, 25 edges>

# Idempotents, 1/?
gap> S := AsSemigroup(IsTransformationSemigroup, FullPBRMonoid(1));;
gap> S := Semigroup(S, rec(acting := false));;
gap> Idempotents(S);
[ Transformation( [ 1, 8, 6, 1, 1, 6, 1, 8, 13, 8, 6, 6, 13, 8, 13, 13 ] ), 
  Transformation( [ 1, 2, 3, 2, 10, 6, 7, 8, 9, 10 ] ), 
  Transformation( [ 6, 9, 3, 3, 3, 6, 6, 13, 9, 9, 3, 6, 13, 13, 9, 13 ] ), 
  IdentityTransformation, Transformation( [ 7, 10, 11, 5, 5, 12, 7, 14, 15,
      10, 11, 12, 16, 14, 15, 16 ] ), 
  Transformation( [ 6, 13, 6, 6, 6, 6, 6, 13, 13, 13, 6, 6, 13, 13, 13, 13 ] )
    , Transformation( [ 7, 14, 12, 7, 7, 12, 7, 14, 16, 14, 12, 12, 16, 14,
      16, 16 ] ), Transformation( [ 1, 8, 6, 8, 8, 6, 1, 8, 13, 8, 6, 6, 13,
      8, 13, 13 ] ), Transformation( [ 6, 9, 3, 9, 9, 6, 6, 13, 9, 9, 3, 6,
      13, 13, 9, 13 ] ), Transformation( [ 7, 10, 11, 10, 10, 12, 7, 14, 15,
      10, 11, 12, 16, 14, 15, 16 ] ), 
  Transformation( [ 12, 15, 11, 11, 11, 12, 12, 16, 15, 15, 11, 12, 16, 16,
      15, 16 ] ), Transformation( [ 12, 16, 12, 12, 12, 12, 12, 16, 16, 16,
      12, 12, 16, 16, 16, 16 ] ), 
  Transformation( [ 6, 13, 6, 13, 13, 6, 6, 13, 13, 13, 6, 6, 13, 13, 13,
      13 ] ), Transformation( [ 7, 14, 12, 14, 14, 12, 7, 14, 16, 14, 12, 12,
      16, 14, 16, 16 ] ), Transformation( [ 12, 15, 11, 15, 15, 12, 12, 16,
      15, 15, 11, 12, 16, 16, 15, 16 ] ), 
  Transformation( [ 12, 16, 12, 16, 16, 12, 12, 16, 16, 16, 12, 12, 16, 16,
      16, 16 ] ) ]

# Idempotents, 2/2
gap> S := Semigroup(FullTransformationMonoid(3),
> rec(acting := false));;
gap> RClasses(S);;
gap> Idempotents(S);
[ IdentityTransformation, Transformation( [ 1, 2, 1 ] ), 
  Transformation( [ 1, 2, 2 ] ), Transformation( [ 3, 2, 3 ] ), 
  Transformation( [ 2, 2 ] ), Transformation( [ 1, 3, 3 ] ), 
  Transformation( [ 1, 1, 1 ] ), Transformation( [ 1, 1 ] ), 
  Transformation( [ 2, 2, 2 ] ), Transformation( [ 3, 3, 3 ] ) ]

# Idempotents, for a D-class, 1/2
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

# Idempotents, for a D-class, 2/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> Idempotents(D);
[  ]

# Idempotents, for a L-class, 1/3
gap> S := Semigroup(FullTransformationMonoid(5), rec(acting := false));;
gap> x := Transformation([1, 1, 2, 3, 4]);;
gap> L := LClass(S, x);;
gap> Idempotents(L);
[ Transformation( [ 1, 2, 3, 4, 2 ] ), Transformation( [ 1, 2, 3, 4, 1 ] ), 
  Transformation( [ 1, 2, 3, 4, 4 ] ), Transformation( [ 1, 2, 3, 4, 3 ] ) ]

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
gap> S := Semigroup(FullTransformationMonoid(5), rec(acting := false));;
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
>  Transformation([6, 3, 1, 3, 1, 6])], rec(acting := false));;
gap> NrRegularDClasses(S);
6

# ViewString, PrintString, for Green's relations, 1/1
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

# ViewString, PrintString, for Green's classes, 1/1
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

# Test NrXClasses for an CanUseFroidurePin semigroup
gap> S := RegularBooleanMatMonoid(3);;
gap> NrLClasses(S);
54
gap> NrRClasses(S);
54
gap> NrHClasses(S);
402
gap> NrDClasses(S);
10

# Test GreensLClasses for a D-class of an CanUseFroidurePin semigroup
gap> S := RegularBooleanMatMonoid(3);;
gap> D := DClass(S, S.3 * S.4);
<Green's D-class: Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [1, 0, 0]])>
gap> GreensLClasses(D);
[ <Green's L-class: Matrix(IsBooleanMat, [[0, 1, 0], [1, 0, 0], [0, 0, 0]])>, 
  <Green's L-class: Matrix(IsBooleanMat, [[0, 1, 0], [0, 0, 1], [0, 0, 0]])>, 
  <Green's L-class: Matrix(IsBooleanMat, [[0, 0, 1], [1, 0, 0], [0, 0, 0]])>, 
  <Green's L-class: Matrix(IsBooleanMat, [[1, 0, 1], [0, 1, 0], [1, 1, 1]])>, 
  <Green's L-class: Matrix(IsBooleanMat, [[0, 1, 1], [1, 0, 0], [1, 1, 1]])>, 
  <Green's L-class: Matrix(IsBooleanMat, [[1, 1, 0], [0, 0, 1], [1, 1, 1]])>, 
  <Green's L-class: Matrix(IsBooleanMat, [[1, 1, 0], [1, 0, 1], [1, 1, 1]])>, 
  <Green's L-class: Matrix(IsBooleanMat, [[1, 1, 0], [0, 1, 1], [1, 1, 1]])>, 
  <Green's L-class: Matrix(IsBooleanMat, [[0, 1, 1], [1, 0, 1], [1, 1, 1]])> ]

# Test GreensXClasses for an infinite CanUseFroidurePin semigroup
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[0, 2], [-1, 0]]));;
gap> GreensLClasses(S);
Error, the argument (a semigroup) is not finite
gap> GreensRClasses(S);
Error, the argument (a semigroup) is not finite
gap> GreensHClasses(S);
Error, the argument (a semigroup) is not finite
gap> GreensDClasses(S);
Error, the argument (a semigroup) is not finite
gap> GreensJClasses(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `GreensJClasses' on 1 arguments

# IsomorphismPermGroup for a group H-class of a matrix semigroup
gap> S := Semigroup([Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [Z(3), Z(3) ^ 0]]),
>  Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [0 * Z(3), 0 * Z(3)]])], rec(acting :=
> false));
<semigroup of 2x2 matrices over GF(3) with 2 generators>
gap> x := Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [Z(3), Z(3) ^ 0]]);
[ [ Z(3)^0, Z(3)^0 ], [ Z(3), Z(3)^0 ] ]
gap> D := DClass(S, x);
<Green's D-class: <matrix object of dimensions 2x2 over GF(3)>>
gap> map := IsomorphismPermGroup(GroupHClass(D));;
gap> Range(map);
Group([ (1,2,3,4,5,6,7,8) ])
gap> Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [0 * Z(3), 0 * Z(3)]]) ^ map;
Error, the argument does not belong to the domain of the function
gap> (1, 3, 2) ^ InverseGeneralMapping(map);
Error, the argument does not belong to the domain of the function
gap> () ^ InverseGeneralMapping(map);
[ [ Z(3)^0, 0*Z(3) ], [ 0*Z(3), Z(3)^0 ] ]

# PartialOrderOfL/RClasses: 1
gap> S := Semigroup([
>  PBR([[-1], []], [[], [-2, -1, 1, 2]]),
>  PBR([[-2, -1, 1, 2], [-2, -1, 2]], [[-2, -1], [-2, 1, 2]]),
>  PBR([[-1], [1]], [[-1], [-2]])]);
<pbr semigroup of degree 2 with 3 generators>
gap> PartialOrderOfLClasses(S);
<immutable digraph with 8 vertices, 8 edges>
gap> PartialOrderOfRClasses(S);
<immutable digraph with 10 vertices, 9 edges>

# PartialOrderOfL/RClasses: 1
gap> S := FullTransformationMonoid(3);
<full transformation monoid of degree 3>
gap> D := PartialOrderOfLClasses(S);
<immutable digraph with 7 vertices, 9 edges>
gap> IsIsomorphicDigraph(D, DigraphFromDigraph6String("+F?OGC@OoK?"));
true
gap> D := PartialOrderOfRClasses(S);
<immutable digraph with 5 vertices, 6 edges>
gap> IsIsomorphicDigraph(D, DigraphFromDigraph6String("+D[CGO?"));
true

# GreensMultipliers for non-acting semigroup
gap> CheckLeftGreensMultiplier1 := function(S)
>  local L, a, b;
>  for L in LClasses(S) do
>    for a in L do
>      for b in L do
>        if LeftGreensMultiplierNC(S, a, b) * a <> b then
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
>        if Set(RClass(S, a), x -> LeftGreensMultiplierNC(S, a, b) * x) <> Set(RClass(S, b)) then
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
gap> F := FreeMonoid("a", "b", "c", "d");;
gap> a := F.1;; b := F.2;; c := F.3;; d := F.4;;
gap> S := F /
> [[a ^ 2, a], [a * b, b], [a * c, c], [a * d, d], [b * a, b], [c * a, c], 
>  [d * a, d], [b ^ 3, b], [b * c ^ 2, b * c * b], [b * c * d, b ^ 2 * d], 
>  [b * d ^ 2, b ^ 2 * c], [c * b * c, b ^ 2 * c], [c ^ 2 * b, c * b ^ 2], 
>  [c ^ 3, c], [c * d ^ 2, c], [d * b * c, b * c], [d * c * b, d * b ^ 2], 
>  [d * c ^ 2, d], [d ^ 3, d], [b * c * b ^ 2, b * c], [c * b ^ 2 * d, b * d], 
>  [c * d * b * d, d * b * d], [(c * d) ^ 2, c ^ 2 * d * c], 
>  [d * b ^ 2 * d, b ^ 2 * d], [d ^ 2 * c * d, (d * c) ^ 2], 
>  [b * (b * d) ^ 2, (b * d) ^ 2], [b * c * b * d * c, b ^ 2 * d * c * d], 
>  [(b * d) ^ 2 * c, (b * d) ^ 2], [b * (d * c) ^ 2, b ^ 2 * c * b * d], 
>  [c * (b * d) ^ 2, (b * d) ^ 2], [c * d * b ^ 2 * c, d * b ^ 2 * c], 
>  [(d * b) ^ 2 * d, (b * d) ^ 2], [b ^ 2 * d * b ^ 2 * c, b * d * b ^ 2 * c], 
>  [b * c * b * d * b ^ 2, b ^ 2 * d * c * d * b], 
>  [(b * d) ^ 2 * b ^ 2, (b * d) ^ 2 * b], 
>  [b * d * c * d * b ^ 2, b ^ 2 * c * b * d * b], 
>  [c * b * d * b ^ 2 * c, b * d * b ^ 2 * c], 
>  [(d * b) ^ 2 * b * c, b * d * b ^ 2 * c], 
>  [b * d * b ^ 2 * c * b * d, b * d * b ^ 2 * c * b], [a, b]];
<fp monoid with 4 generators and 40 relations of length 276>
gap> CheckLeftGreensMultiplier1(S);
true
gap> CheckLeftGreensMultiplier2(S);
true
gap> CheckRightGreensMultiplier1(S);
true
gap> CheckRightGreensMultiplier2(S);
true
gap> S := Semigroup(Bipartition([[1, 2, 3, 4, 5, -1], [-2, -5], [-3, -4]]),
>                   Bipartition([[1, 2, 5, -5], [3, -1], [4, -2, -3, -4]]),
>                   Bipartition([[1, -4, -5], [2, 3, 4, 5], [-1, -2, -3]]),
>                   Bipartition([[1, 2, 3, -2], [4, -1], [5, -5], [-3, -4]]),
>                   Bipartition([[1, 2, 4, 5, -1, -2, -3, -5], [3], [-4]]));
<bipartition semigroup of degree 5 with 5 generators>
gap> S := Image(EmbeddingFpMonoid(AsSemigroup(IsFpSemigroup, S)));;
gap> CheckLeftGreensMultiplier1(S);
true
gap> CheckLeftGreensMultiplier2(S);
true
gap> CheckRightGreensMultiplier1(S);
true
gap> CheckRightGreensMultiplier2(S);
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/greens/froidure-pin.tst");
