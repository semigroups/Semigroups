#############################################################################
##
#W  standard/greens/generic.tst
#Y  Copyright (C) 2016-2022                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local D, DD, DDD, H, J, L, L3, LL, R, RR, RRR, S, S1, T, acting, an, gens, id
#@local iter, m1, m2, x, y
gap> START_TEST("Semigroups package: standard/greens/generic.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Test IsXTrivial (for Green's classes)
gap> S := Semigroup(
> [Matrix(GF(5), [[0 * Z(5), Z(5) ^ 3], [Z(5) ^ 2, Z(5) ^ 0]]),
>  Matrix(GF(5), [[Z(5) ^ 0, Z(5)], [Z(5), Z(5) ^ 3]]),
>  Matrix(GF(5), [[Z(5) ^ 0, Z(5) ^ 3], [0 * Z(5), 0 * Z(5)]]),
>  Matrix(GF(5), [[Z(5), Z(5) ^ 0], [0 * Z(5), Z(5) ^ 3]]),
>  Matrix(GF(5), [[Z(5), Z(5) ^ 0], [Z(5) ^ 0, Z(5)]]),
>  Matrix(GF(5), [[Z(5) ^ 2, 0 * Z(5)], [Z(5), 0 * Z(5)]]),
>  Matrix(GF(5), [[Z(5) ^ 2, Z(5)], [0 * Z(5), 0 * Z(5)]])]);;
gap> D := GreensDClassOfElement(S,
> Matrix(GF(5), [[Z(5) ^ 3, Z(5) ^ 2], [Z(5) ^ 3, Z(5)]]));
<Green's D-class: <matrix object of dimensions 2x2 over GF(5)>>
gap> IsHTrivial(D);
false
gap> IsLTrivial(D);
false
gap> IsRTrivial(D);
false

# Test \= (for Green's relations)
gap> S := FullBooleanMatMonoid(3);;
gap> T := FullBooleanMatMonoid(2);;
gap> GreensRRelation(S) = GreensRRelation(T);
false
gap> GreensRRelation(S) = GreensLRelation(S);
false
gap> GreensLRelation(S) = GreensRRelation(S);
false
gap> GreensHRelation(S) = GreensRRelation(S);
false
gap> GreensDRelation(S) = GreensRRelation(S);
false
gap> GreensJRelation(S) = GreensRRelation(S);
false
gap> S := FreeInverseSemigroup(3);;
gap> GreensJRelation(S) = GreensRRelation(S);
false

# Test AsSSortedList, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> L := LClass(S, PartialPerm([1, 7], [3, 5]));;
gap> AsSet(L);
[ [1,3][2,5], [1,5][2,3], [2,3,5], [2,5](3), [1,3,5], [1,5](3), 
  <identity partial perm on [ 3, 5 ]>, (3,5), [2,3](5), [2,5,3], [1,3](5), 
  [1,5,3], [7,5](3), [7,3,5], [1,3][7,5], [1,5][7,3] ]

# Test \< and \=, 1/1
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

# Test MultiplicativeNeutralElement, One, for an H-class, 1
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

# Test MultiplicativeNeutralElement, One, for an H-class, 2
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

# Test StructureDescription, for an H-class, 1/1
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

# Test DClassOfLClass, 1/1
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

# Test DClassOfRClass, 1/1
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

# Test DClassOfHClass, 1/1
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

# Test LClassOfHClass, 1/1
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

# Test RClassOfHClass, 1/1
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

# Test GreensDClassOfElement, fail, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(acting := false));;
gap> GreensDClassOfElement(S, PartialPerm([19]));
Error, the 2nd argument (a mult. elt.) does not belong to the source of the 1s\
t argument (a Green's relation)

# Test GreensDClassOfElementNC, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(acting := false));;
gap> D := GreensDClassOfElementNC(S, PartialPerm([19]));;
Error, the 2nd argument (a mult. elt.) does not belong to the source of the 1s\
t argument (a Green's relation)

# Test GreensJClassOfElement, 1/1
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])], rec(acting := false));;
gap> J := GreensJClassOfElement(S, S.2);
<Green's D-class: [6,4,7,1,2,5](3)>

# Test GreensL/RClassOfElement, fail, 1/1
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

# Test GreensL/RClassOfElementNC, 1/1
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

# Test GreensL/RClassOfElement, for a D-class, 1/1
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

# Test GreensHClassOfElement, 1/1
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

# Test GreensHClassOfElement(L/R-class, x), 1/1
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

# Test \in, for D-class, 1/4
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

# Test \in, for D-class, 2/4
gap> S := OrderEndomorphisms(5);;
gap> x := Transformation([1, 2, 2, 4, 5]);;
gap> D := DClass(S, x);;
gap> x in D;
true
gap> Transformation([1, 2, 1, 4, 5]) in D;
false

# Test \in, for D-class, 3/4
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

# Test \in, for D-class, 4/4
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> D := DClass(S, x);;
gap> y in D;
false

# Test \in, for L-class, 1/5
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

# Test \in, for L-class, 2/5
gap> S := OrderEndomorphisms(5);;
gap> x := Transformation([1, 2, 2, 4, 5]);;
gap> L := LClass(S, x);;
gap> x in L;
true
gap> Transformation([1, 2, 1, 4, 5]) in L;
false

# Test \in, for L-class, 3/5
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

# Test \in, for L-class, 4/5
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> L := LClass(S, x);;
gap> y in L;
false

# Test \in, for L-class, 5/5
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 4, 3, 5, 5]);;
gap> L := LClass(S, x);;
gap> y in L;
false

# Test \in, for R-class, 1/6
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

# Test \in, for R-class, 2/6
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 4, 3, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# Test \in, for R-class, 3/6
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 3, 3, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# Test \in, for R-class, 4/6
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 2, 3, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# Test \in, for R-class, 5/6
gap> S := OrderEndomorphisms(5);;
gap> x := Transformation([1, 2, 2, 4, 5]);;
gap> R := RClass(S, x);;
gap> x in R;
true
gap> Transformation([1, 2, 1, 4, 5]) in R;
false

# Test \in, for R-class, 6/6
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> R := RClass(S, x);;
gap> y in R;
false

# Test \in, for H-class, 1/3
gap> x := Transformation([2, 3, 4, 1, 5, 5]);;
gap> S := Semigroup(x);
<commutative transformation semigroup of degree 6 with 1 generator>
gap> y := Transformation([2, 1, 3, 4, 5, 5]);;
gap> H := HClass(S, x);;
gap> y in H;
false

# Test \in, for H-class, 2/3
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> y := Transformation([1, 1, 2, 3, 5, 5]);;
gap> H := HClass(S, x);;
gap> y in H;
false

# Test \in, for H-class, 3/3
gap> x := Transformation([1, 1, 3, 4, 5, 5]);;
gap> S := Semigroup(x);;
gap> H := HClass(S, x);;
gap> ForAll(H, x -> x in H);
true

# Test R-classes/reps, 1/1
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

# Test R-reps, 1/1
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

# Test H-classes/reps, 1/3
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

# Test H-classes/reps, 2/3
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

# Test NrXClasses, 1/1
gap> S := Semigroup(SymmetricInverseMonoid(5));;
gap> NrRClasses(S);
32
gap> NrDClasses(S);
6
gap> NrLClasses(S);
32
gap> NrHClasses(S);
252

# Test NrXClasses for a D-class, 1/1
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

# Test GreensXClasses, for an infinite semigroup, 1/1
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

# Test GreensHClasses, fail, 1/1
gap> S := Semigroup(SymmetricInverseMonoid(3));;
gap> H := HClass(S, S.2);;
gap> GreensHClasses(H);
Error, the argument is not a Green's R-, L-, or D-class

# Test PartialOrderOfDClasses, 1/2
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(3));;
gap> S := Semigroup(S, rec(acting := false));;
gap> PartialOrderOfDClasses(S);
<immutable digraph with 11 vertices, 25 edges>

# Test Idempotents, 1/?
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

# Test Idempotents, 2/2
gap> S := Semigroup(FullTransformationMonoid(3),
> rec(acting := false));;
gap> RClasses(S);;
gap> Idempotents(S);
[ IdentityTransformation, Transformation( [ 1, 2, 1 ] ), 
  Transformation( [ 1, 2, 2 ] ), Transformation( [ 3, 2, 3 ] ), 
  Transformation( [ 2, 2 ] ), Transformation( [ 1, 3, 3 ] ), 
  Transformation( [ 1, 1, 1 ] ), Transformation( [ 1, 1 ] ), 
  Transformation( [ 2, 2, 2 ] ), Transformation( [ 3, 3, 3 ] ) ]

# Test Idempotents, for a D-class, 1/2
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

# Test Idempotents, for a D-class, 2/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> Idempotents(D);
[  ]

# Test Idempotents, for a L-class, 1/3
gap> S := Semigroup(FullTransformationMonoid(5), rec(acting := false));;
gap> x := Transformation([1, 1, 2, 3, 4]);;
gap> L := LClass(S, x);;
gap> Idempotents(L);
[ Transformation( [ 1, 2, 3, 4, 2 ] ), Transformation( [ 1, 2, 3, 4, 1 ] ), 
  Transformation( [ 1, 2, 3, 4, 4 ] ), Transformation( [ 1, 2, 3, 4, 3 ] ) ]

# Test Idempotents, for a L-class, 2/3
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> L := LClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsRegularGreensClass(L);
false
gap> Idempotents(L);
[  ]

# Test Idempotents, for a L-class, 3/3
gap> S := PartitionMonoid(3);;
gap> L := LClass(S, One(S));;
gap> Idempotents(L);
[ <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ]> ]

# Test Idempotents, for a H-class, 1/2
gap> S := SingularTransformationSemigroup(4);;
gap> H := HClass(S, S.1);
<Green's H-class: Transformation( [ 1, 2, 3, 3 ] )>
gap> Idempotents(H);
[ Transformation( [ 1, 2, 3, 3 ] ) ]

# Test Idempotents, for a H-class, 1/2
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> H := HClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsGroupHClass(H);
false
gap> Idempotents(H);
[  ]

# Test NrIdempotents, for a semigroup, 1/2
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> NrIdempotents(S);
24

# Test NrIdempotents, for a semigroup, 2/2
gap> S := Semigroup([PartialPerm([1, 3, 4, 5, 6], [3, 1, 5, 7, 6]),
>  PartialPerm([1, 2, 3, 4, 6, 7], [2, 5, 3, 7, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [3, 5, 7, 1, 6, 2]),
>  PartialPerm([1, 2, 4, 7], [3, 7, 6, 5])]);;
gap> Idempotents(S);;
gap> NrIdempotents(S);
24

# Test NrIdempotents, for a D-class, 1/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> IsRegularDClass(D);
false
gap> NrIdempotents(D);
0

# Test NrIdempotents, for a D-class, 2/2
gap> S := Semigroup([Transformation([2, 3, 4, 5, 1, 5, 6, 7, 8])]);;
gap> D := DClass(S, S.1);
<Green's D-class: Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )>
gap> NrIdempotents(D);
0

# Test NrIdempotents, for a L-class, 1/3
gap> S := Semigroup(FullTransformationMonoid(5), rec(acting := false));;
gap> x := Transformation([1, 1, 2, 3, 4]);;
gap> L := LClass(S, x);;
gap> NrIdempotents(L);
4

# Test NrIdempotents, for a L-class, 2/3
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> L := LClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsRegularGreensClass(L);
false
gap> NrIdempotents(L);
0

# Test NrIdempotents, for a L-class, 3/3
gap> S := PartitionMonoid(3);;
gap> L := LClass(S, One(S));;
gap> NrIdempotents(L);
1

# Test NrIdempotents, for a H-class, 1/2
gap> S := SingularTransformationSemigroup(4);;
gap> H := HClass(S, S.1);
<Green's H-class: Transformation( [ 1, 2, 3, 3 ] )>
gap> NrIdempotents(H);
1

# Test NrIdempotents, for a H-class, 1/2
gap> S := AsSemigroup(IsTransformationSemigroup, FullBooleanMatMonoid(3));
<transformation monoid of degree 8 with 5 generators>
gap> H := HClass(S, Transformation([1, 1, 1, 2, 1, 3, 5]));;
gap> IsGroupHClass(H);
false
gap> NrIdempotents(H);
0

# Test NrIdempotents, for an R-class, 1/2
gap> S := Semigroup(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                    Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> R := First(RClasses(S),
> x -> Transformation([9, 10, 4, 9, 10, 4, 4, 3, 3, 6]) in x);;
gap> NrIdempotents(R);
0
gap> IsRegularGreensClass(R);
false

# Test NrIdempotents, for an R-class, 3/3
gap> S := Semigroup(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                    Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> R := RClass(S, Transformation([6, 9, 9, 6, 9, 1, 1, 2, 2, 6]));;
gap> IsRegularGreensClass(R);
true
gap> NrIdempotents(R);
7

# Test IsRegularGreensClass, for an R-class, 1/1
gap> S := Semigroup(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                    Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> R := First(RClasses(S),
> x -> Transformation([9, 10, 4, 9, 10, 4, 4, 3, 3, 6]) in x);;
gap> IsRegularGreensClass(R);
false

# Test IsRegularGreensClass, for an R-class in group of units, 1/1
gap> S := Monoid(Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>                 Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]));;
gap> S := AsSemigroup(IsBipartitionSemigroup, S);;
gap> R := RClass(S, IdentityBipartition(10));;
gap> IsRegularGreensClass(R);
true

# Test NrRegularDClasses, 1/1
gap> S := Semigroup([Transformation([2, 2, 1, 2, 4, 4]),
>  Transformation([2, 6, 6, 5, 1, 4]), Transformation([3, 2, 5, 5, 6, 4]),
>  Transformation([3, 5, 3, 4, 1]), Transformation([4, 2, 3, 1, 4, 2]),
>  Transformation([4, 4, 2, 6, 6, 3]), Transformation([5, 5, 5, 6, 5, 4]),
>  Transformation([6, 3, 1, 3, 1, 6])], rec(acting := false));;
gap> NrRegularDClasses(S);
6

# Test ViewString, PrintString, for Green's relations, 1/1
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
gap> PrintObj(GreensDRelation(S)); "This string allows us to test PrintObj";
GreensDRelation(
   Monoid( 
   [ Transformation( [ 2, 3, 1 ] ), Transformation( [ 2, 1 ] ), Transformation\
( [ 1, 2, 1 ] ) ] ))"This string allows us to test PrintObj"

# Test ViewString, PrintString, for Green's classes, 1/1
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
gap> PrintObj(DClass(S, S.2)); "This string allows us to test PrintObj";
GreensDClassOfElement(
  Monoid( Matrix(IsBooleanMat, [[0, 1, 0], [1, 0, 0], [0, 0, 1]])
    Matrix(IsBooleanMat, [[0, 1, 0], [0, 0, 1], [1, 0, 0]]), 
    Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [1, 0, 1]]), 
    Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 0]]), 
    Matrix(IsBooleanMat, [[1, 1, 0], [1, 0, 1], [0, 1, 1]]) ), 
  Matrix(IsBooleanMat, [[0, 1, 0], [0, 0, 1], [1, 0, 0]]))
 "This string allows us to test PrintObj"

# Test IsomorphismPermGroup (for an H-class)
gap> S := RegularBooleanMatMonoid(3);
<monoid of 3x3 boolean matrices with 4 generators>
gap> S := AsSemigroup(IsIntegerMatrixSemigroup, S);
<semigroup of 8x8 integer matrices with 5 generators>
gap> D := DClass(S, S.2);;
gap> IsRegularDClass(D);
true
gap> H := GroupHClass(D);;
gap> x := IsomorphismPermGroup(H);;
gap> Source(x) = H;
true
gap> Range(x);
Group([ (1,2)(3,5)(4,6), (1,3,6)(2,4,5) ])
gap> Representative(H) ^ x;
()
gap> y := InverseGeneralMapping(x);;
gap> () ^ y;
<immutable 8x8-matrix over Integers>
gap> () ^ y = Representative(H);
true
gap> Matrix(Integers, [[0, 0, -1, -1, 3, 3, 1, -1], 
>                             [1, -1, 0, 2, 7, 5, 0, -1],
>                             [-3, 4, 0, 2, 0, -1, 0, 0], 
>                             [4, 0, 0, 0, 1, 2, 2, 0], 
>                             [-2, 2, 0, -1, 0, 0, 0, 3],
>                             [0, 2, 0, -1, -2, -2, 1, 2], 
>                             [0, 0, 0, -2, -3, 0, 0, -1], 
>                             [0, 0, 2, 0, 0, 0, 1, 2]]) ^ x;
Error, the argument does not belong to the domain of the function
gap> (1, 10) ^ y;
Error, the argument does not belong to the domain of the function
gap> H := HClass(S, Matrix(Integers, 
> [[1, 0, 0, 0, 0, 0, 0, 0], 
>  [1, 0, 0, 0, 0, 0, 0, 0], 
>  [0, 1, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 1, 0, 0], 
>  [1, 0, 0, 0, 0, 0, 0, 0], 
>  [0, 0, 1, 0, 0, 0, 0, 0],
>  [0, 1, 0, 0, 0, 0, 0, 0], 
>  [0, 0, 0, 0, 0, 0, 0, 1]]));;
gap> IsomorphismPermGroup(H);
Error, the argument (a Green's H-class) is not a group

# Test GreensRClassOfElement for infinite semigroup
gap> S := FreeInverseSemigroup(2);;
gap> GreensRClassOfElement(S, S.1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `GreensRClassOfElement' on 2 arguments
gap> GreensLClassOfElement(S, S.1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `GreensLClassOfElement' on 2 arguments
gap> GreensHClassOfElement(S, S.1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `GreensHClassOfElement' on 2 arguments
gap> GreensDClassOfElement(S, S.1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `GreensDClassOfElement' on 2 arguments

# Test NrL/RClasses for a D-class
gap> S := Semigroup(FullTransformationMonoid(4), rec(acting := false));;
gap> D := DClass(S, S.3);;
gap> NrLClasses(D);
4
gap> NrRClasses(D);
6

# Test NrXClasses for a non-CanUseFroidurePin semigroup
gap> S := Semigroup(SEMIGROUPS.UniversalFakeOne);;
gap> CanUseFroidurePin(S);
false
gap> NrDClasses(S);
1
gap> NrLClasses(S);
1
gap> NrRClasses(S);
1
gap> NrHClasses(S);
1

# Test NrXClassReps for a non-CanUseFroidurePin semigroup
gap> S := Semigroup(SEMIGROUPS.UniversalFakeOne);;
gap> CanUseFroidurePin(S);
false
gap> DClassReps(S);
[ <universal fake one> ]
gap> RClassReps(S);
[ <universal fake one> ]
gap> LClassReps(S);
[ <universal fake one> ]
gap> HClassReps(S);
[ <universal fake one> ]

# Test NrXClassReps for a D-class of a non-CanUseFroidurePin semigroup
gap> S := Semigroup(SEMIGROUPS.UniversalFakeOne);;
gap> CanUseFroidurePin(S);
false
gap> D := DClasses(S)[1];;
gap> RClassReps(D);
[ <universal fake one> ]
gap> LClassReps(D);
[ <universal fake one> ]
gap> HClassReps(D);
[ <universal fake one> ]
gap> NrIdempotents(D);
1
gap> Idempotents(D);
[ <universal fake one> ]

# Test IteratorOfXClasses for a non-CanUseFroidurePin semigroup
gap> S := Semigroup(SEMIGROUPS.UniversalFakeOne);;
gap> CanUseFroidurePin(S);
false
gap> IsFinite(S);
true
gap> iter := IteratorOfDClasses(S);
<iterator>
gap> IsDoneIterator(iter);
false
gap> NextIterator(iter);
<Green's D-class: <object>>
gap> IsDoneIterator(iter);
true
gap> iter := IteratorOfRClasses(S);
<iterator>
gap> IsDoneIterator(iter);
false
gap> NextIterator(iter);
<Green's R-class: <object>>
gap> IsDoneIterator(iter);
true

# Test GreensXClassOfElementNC for a non-CanUseFroidurePin semigroup
gap> S := Semigroup(SEMIGROUPS.UniversalFakeOne);;
gap> IsFinite(S);
true
gap> CanUseFroidurePin(S);
false
gap> GreensRClassOfElementNC(S, S.1);
<Green's R-class: <object>>
gap> GreensLClassOfElementNC(S, S.1);
<Green's L-class: <object>>
gap> GreensDClassOfElementNC(S, S.1);
<Green's D-class: <object>>
gap> GreensHClassOfElementNC(S, S.1);
<Green's H-class: <object>>

# Test XClassOfYClass for a non-CanUseFroidurePin semigroup
gap> S := Semigroup(SEMIGROUPS.UniversalFakeOne);;
gap> IsFinite(S);
true
gap> DClassOfRClass(GreensRClassOfElementNC(S, S.1));
<Green's D-class: <object>>
gap> DClassOfLClass(GreensLClassOfElementNC(S, S.1));
<Green's D-class: <object>>
gap> DClassOfHClass(GreensHClassOfElementNC(S, S.1));
<Green's D-class: <object>>
gap> RClassOfHClass(GreensHClassOfElementNC(S, S.1));
<Green's R-class: <object>>
gap> LClassOfHClass(GreensHClassOfElementNC(S, S.1));
<Green's L-class: <object>>

# Test PartialOrderOfDClasses for a finite non-CanUseFroidurePin,
# non-acting semigroup
gap> S := SemigroupByMultiplicationTable([[1, 1, 1, 1, 1], 
>                                         [1, 1, 1, 1, 1], 
>                                         [1, 1, 1, 1, 2], 
>                                         [1, 1, 2, 1, 1], 
>                                         [1, 1, 1, 2, 1]]);;
gap> IsActingSemigroup(S);
false
gap> CanUseFroidurePin(S);
true
gap> PartialOrderOfDClasses(S);
<immutable digraph with 5 vertices, 7 edges>

# Test PartialOrderOfDClasses for a finite non-CanUseFroidurePin,
# non-acting semigroup
gap> D := Digraph([[2, 3], [2], []]);;
gap> S1 := FullTransformationMonoid(2);;
gap> id := IdentityMapping(S1);;
gap> m1 := SemigroupHomomorphismByFunction(S1, S1, x -> Transformation([1, 1]));;
gap> m2 := SemigroupHomomorphismByFunction(S1, S1, x -> Transformation([2, 2]));;
gap> L := [S1, S1, S1];;
gap> H := [[m1, m2], [id], []];;
gap> S := StrongSemilatticeOfSemigroups(D, L, H);
<strong semilattice of 3 semigroups>
gap> IsFinite(S);
true
gap> IsActingSemigroup(S);
false
gap> CanUseFroidurePin(S);
false
gap> PartialOrderOfDClasses(S);
<immutable digraph with 6 vertices, 7 edges>

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/greens/generic.tst");
