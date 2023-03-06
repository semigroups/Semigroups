#############################################################################
##
#W  standard/greens/acting-inverse.tst
#Y  Copyright (C) 2015-2022                                Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local D, L, R, S, acting, an, en, it, x, y
gap> START_TEST("Semigroups package: standard/greens/acting-inverse.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# SchutzenbergerGroup, for an L-class of an inverse op sgp
gap> S := InverseSemigroup([
>  Bipartition([[1, -4], [2, -2], [3, -3], [4, 5, -1, -5]]),
>  Bipartition([[1, -1], [2, -3], [3, 5, -4, -5], [4, -2]]),
>  Bipartition([[1, -4], [2, -1], [3, 5, -3, -5], [4, -2]])],
>  rec(acting := true));;
gap> x := Bipartition([[1, -2], [2, -4], [3, 4, 5, -1, -3, -5]]);;
gap> L := LClass(S, x);
<Green's L-class: <block bijection: [ 1, -2 ], [ 2, -4 ], 
  [ 3, 4, 5, -1, -3, -5 ]>>
gap> x in L;
true
gap> SchutzenbergerGroup(L);
Group([ (2,3) ])
gap> L := GreensLClassOfElementNC(S, x, true);;
gap> x in L;
true
gap> SchutzenbergerGroup(L);
Group([ (2,3) ])

# DClassOfXClass, for an X=R/L/H-class
gap> S := InverseSemigroup([
>  Bipartition([[1, -4], [2, -2], [3, -3], [4, 5, -1, -5]]),
>  Bipartition([[1, -1], [2, -3], [3, 5, -4, -5], [4, -2]]),
>  Bipartition([[1, -4], [2, -1], [3, 5, -3, -5], [4, -2]])],
>  rec(acting := true));;
gap> x := Bipartition([[1, -1], [2, -4], [3, -2], [4, 5, -3, -5]]);;
gap> D := DClassOfLClass(LClass(S, x));
<Green's D-class: <block bijection: [ 1, 5, -3, -5 ], [ 2, -4 ], [ 3, -2 ], 
  [ 4, -1 ]>>
gap> x in D;
true
gap> D = DClass(S, x);
true
gap> DClassOfRClass(RClass(S, x));
<Green's D-class: <block bijection: [ 1, -4 ], [ 2, -2 ], [ 3, -3 ], 
  [ 4, 5, -1, -5 ]>>
gap> x in D;
true
gap> D = DClass(S, x);
true
gap> DClassOfHClass(HClass(S, x));
<Green's D-class: <block bijection: [ 1, -1 ], [ 2, -4 ], [ 3, -2 ], 
  [ 4, 5, -3, -5 ]>>
gap> x in D;
true
gap> D = DClass(S, x);
true

# LClassOfHClass, for an H-class
gap> S := InverseSemigroup([
>  Bipartition([[1, -4], [2, -2], [3, -3], [4, 5, -1, -5]]),
>  Bipartition([[1, -1], [2, -3], [3, 5, -4, -5], [4, -2]]),
>  Bipartition([[1, -4], [2, -1], [3, 5, -3, -5], [4, -2]])],
>  rec(acting := true));;
gap> x := Bipartition([[1, -4], [2, 3, 5, -2, -3, -5], [4, -1]]);;
gap> L := LClassOfHClass(HClass(S, x));
<Green's L-class: <block bijection: [ 1, -4 ], [ 2, 3, 5, -2, -3, -5 ], 
  [ 4, -1 ]>>
gap> L = LClass(S, x);
true

# GreensHClassOfElement, for a Green's class and an element
gap> S := InverseSemigroup([
> PartialPerm([1, 2, 3, 4], [4, 1, 2, 6]),
> PartialPerm([1, 2, 4], [4, 6, 3]),
> PartialPerm([1, 2, 3, 5], [6, 4, 5, 1])],
> rec(acting := true));;
gap> x := PartialPerm([4, 1, 2, 0]);;
gap> GreensHClassOfElement(DClass(S, x), x);
<Green's H-class: [3,2,1,4]>
gap> GreensHClassOfElement(RClass(S, x), x);
<Green's H-class: [3,2,1,4]>
gap> GreensHClassOfElement(LClass(S, x), x);
<Green's H-class: [3,2,1,4]>

# Size, for an inverse op acting D/L-class
gap> S := InverseSemigroup([
> PartialPerm([1, 2, 3, 4], [4, 1, 2, 6]),
> PartialPerm([1, 2, 4], [4, 6, 3]),
> PartialPerm([1, 2, 3, 5], [6, 4, 5, 1])],
> rec(acting := true));;
gap> x := PartialPerm([4, 1, 2, 0]);;
gap> Size(DClass(S, x));
36
gap> Size(LClass(S, x));
6

# \in, for a D-class and an element, 1
gap> S := InverseSemigroup([
>  Bipartition([[1, -4], [2, -2], [3, -3], [4, 5, -1, -5]]),
>  Bipartition([[1, -1], [2, -3], [3, 5, -4, -5], [4, -2]]),
>  Bipartition([[1, -4], [2, -1], [3, 5, -3, -5], [4, -2]])],
>  rec(acting := true));;
gap> x := Bipartition([[1, -4], [2, 3, 5, -2, -3, -5], [4, -1]]);;
gap> D := DClass(S, x);;
gap> PartialPerm([]) in D;
false
gap> Bipartition([[1, -4], [2, 3, 5, -2, -3, -5], [4, -1], [6, -6]]) in D;
false
gap> Bipartition([[1, -4], [2, 3, 5, -2, -3, -5, 4, -1]]) in D;
false
gap> Bipartition([[1, -4], [2, 3, 5, -2], [-3, -5, 4, -1]]) in D;
false
gap> Bipartition([[1], [2, -3], [3, -4], [4, -5], [5], [-1], [-2]]) in D;
false
gap> Bipartition([[1, 2, 5, -1, -2, -3], [3, -4], [4, -5]]) in D;
false

# \in, for a D-class and an element, 2
gap> S := InverseSemigroup([
>   PartialPerm([1, 2, 3, 5, 6, 7], [5, 7, 1, 9, 4, 2]),
>   PartialPerm([1, 2, 3, 6, 8], [2, 6, 7, 9, 1]),
>   PartialPerm([1, 2, 3, 4, 5, 8], [7, 1, 4, 3, 2, 6]),
>   PartialPerm([1, 2, 3, 4, 5, 7, 9], [5, 3, 8, 1, 9, 4, 6])]);;
gap> x := PartialPerm([2, 4, 5, 7], [4, 1, 6, 7]);;
gap> D := DClass(S, x);;
gap> PartialPerm([2, 4, 5, 7], [2, 4, 7, 5]) in D;
false

# \in, for a D-class and an element, 3
gap> S := SymmetricInverseMonoid(5);
<symmetric inverse monoid of degree 5>
gap> x := S.1 * S.2 * S.1;
(1,2,4)(3,5)
gap> x := S.1 * S.2 * S.3 ;
[5,1](2)(3)(4)
gap> D := DClass(S, x);;
gap> x in D;
true

# \in, for an L-class and an element, 1
gap> S := InverseSemigroup([
>  Bipartition([[1, -4], [2, -2], [3, -3], [4, 5, -1, -5]]),
>  Bipartition([[1, -1], [2, -3], [3, 5, -4, -5], [4, -2]]),
>  Bipartition([[1, -4], [2, -1], [3, 5, -3, -5], [4, -2]])],
>  rec(acting := true));;
gap> x := Bipartition([[1, -4], [2, 3, 5, -2, -3, -5], [4, -1]]);;
gap> L := LClass(S, x);;
gap> PartialPerm([]) in L;
false
gap> Bipartition([[1, -4], [2, 3, 5, -2, -3, -5], [4, -1], [6, -6]]) in L;
false
gap> Bipartition([[1, -4], [2, 3, 5, -2, -3, -5, 4, -1]]) in L;
false
gap> Bipartition([[1, -4], [2, 3, 5, -2], [-3, -5, 4, -1]]) in L;
false
gap> Bipartition([[1], [2, -3], [3, -4], [4, -5], [5], [-1], [-2]]) in L;
false
gap> Bipartition([[1, 2, 5, -1, -2, -3], [3, -4], [4, -5]]) in L;
false

# \in, for an L-class and an element, 2
gap> S := InverseSemigroup([PartialPerm([1, 3], [4, 3]),
>   PartialPerm([1, 2, 3], [4, 1, 2]),
>   PartialPerm([1, 2, 4], [4, 5, 1]),
>   PartialPerm([1, 3, 4], [5, 2, 1])],
> rec(acting := true));;
gap> x := PartialPerm([1, 2, 4], [1, 2, 4]);;
gap> L := LClass(S, x);;
gap> PartialPerm([4, 5, 6], [4, 2, 1]) in L;
false
gap> PartialPerm([1, 4, 5], [4, 2, 1]) in L;
false

# \in, for an L-class and an element, 3
gap> S := SymmetricInverseMonoid(5);
<symmetric inverse monoid of degree 5>
gap> x := S.1 * S.2 * S.1;
(1,2,4)(3,5)
gap> x := S.1 * S.2 * S.3 ^ 6;
<empty partial perm>
gap> x := S.1 * S.2 * S.3 ;
[5,1](2)(3)(4)
gap> L := LClass(S, x);;
gap> x in L;
true

# \in, for an L-class and an element, 4
gap> S := InverseSemigroup(PartialPerm([1, 3, 4, 8], [2, 7, 8, 4]),
> PartialPerm([1, 2, 3, 4, 5, 8], [7, 6, 9, 1, 3, 2]),
> PartialPerm([1, 2, 3, 4, 5, 6, 7, 9], [2, 8, 3, 7, 1, 5, 9, 6]),
> PartialPerm([1, 2, 3, 4, 6, 7, 9], [3, 5, 7, 2, 6, 9, 8]),
> PartialPerm([1, 2, 3, 4, 6, 9], [8, 4, 7, 5, 3, 6]));;
gap> x := PartialPerm([1, 2, 3, 4, 6, 7, 9], [3, 5, 7, 2, 6, 9, 8]);;
gap> y := PartialPermNC([2, 3, 5, 6, 7, 8, 9], [2, 3, 5, 6, 7, 8, 9]);;
gap> x in LClass(S, y);
true

# XClassReps: for an inverse op acting semigroup
gap> S := InverseSemigroup([
> PartialPerm([1, 2, 3, 4], [7, 6, 2, 1]),
> PartialPerm([1, 2, 3, 5, 6], [1, 3, 6, 2, 7]),
> PartialPerm([1, 3, 4, 6, 7], [1, 6, 3, 5, 4]),
> PartialPerm([1, 2, 5, 6, 7], [6, 7, 3, 2, 1])],
> rec(acting := true));;
gap> DClassReps(S);
[ <identity partial perm on [ 1, 2, 6, 7 ]>, 
  <identity partial perm on [ 1, 2, 3, 6, 7 ]>, 
  <identity partial perm on [ 1, 3, 4, 5, 6 ]>, 
  <identity partial perm on [ 6, 7 ]>, <identity partial perm on [ 1, 3, 6 ]>,
  <identity partial perm on [ 2, 3, 6, 7 ]>, 
  <identity partial perm on [ 1, 2, 3, 5 ]>, <empty partial perm>, 
  <identity partial perm on [ 7 ]>, <identity partial perm on [ 2, 3, 7 ]> ]
gap> RClassReps(S);
[ <identity partial perm on [ 1, 2, 6, 7 ]>, [3,2,6][4,1,7], [3,6,7][5,2](1), 
  [3,7][4,6,2](1), [3,2][4,7,6](1), 
  <identity partial perm on [ 1, 2, 3, 6, 7 ]>, [5,2,3,6,7](1), 
  [5,3](1,6,2,7), <identity partial perm on [ 1, 3, 4, 5, 6 ]>, 
  [7,4,3,6,5](1), <identity partial perm on [ 6, 7 ]>, [4,7][5,6], 
  [1,7][2,6], [1,7][3,6], [2,6](7), [1,6](7), [1,7](6), [2,6][4,7], [2,7](6), 
  [3,6](7), [2,7][5,6], [2,6][3,7], [3,6,7], [3,6][4,7], [4,6](7), [5,7](6), 
  [1,7][5,6], [4,7](6), [3,6][5,7], [1,7][4,6], [5,6](7), 
  <identity partial perm on [ 1, 3, 6 ]>, [4,3,6](1), [2,6][7,1,3], 
  [2,6,3][7,1], [2,3,6](1), [2,6][5,3](1), [7,6,1](3), [2,3,6,1], [5,6,3](1), 
  [2,6][5,3,1], [4,6,3][5,1], [5,6,3][7,1], [2,3](1,6), [7,1,6](3), 
  [4,1,6,3], [5,3,1,6], [2,1][5,3][7,6], [2,1][4,6](3), [7,6,3](1), 
  [4,6][5,3](1), [2,3][4,1,6], [5,6,1](3), [4,3,1](6), [4,1][7,3,6], 
  [4,6][7,3](1), [5,3][7,1,6], <identity partial perm on [ 2, 3, 6, 7 ]>, 
  [5,2,3,6,7], [1,6,2,7][5,3], [1,6](2,3,7), [5,7,6,3](2), 
  <identity partial perm on [ 1, 2, 3, 5 ]>, [6,3,2,5](1), [6,2][7,3,5](1), 
  [4,3][6,5,2](1), [7,1,2,3](5), [6,5,2][7,1,3], <empty partial perm>, 
  <identity partial perm on [ 7 ]>, [4,7], [3,7], [6,7], [1,7], [5,7], [2,7], 
  <identity partial perm on [ 2, 3, 7 ]>, [5,2,3][6,7] ]

# XClassReps: for an inverse op acting Greens class
gap> S := InverseSemigroup([
> PartialPerm([1, 2, 3, 4], [7, 6, 2, 1]),
> PartialPerm([1, 2, 3, 5, 6], [1, 3, 6, 2, 7]),
> PartialPerm([1, 3, 4, 6, 7], [1, 6, 3, 5, 4]),
> PartialPerm([1, 2, 5, 6, 7], [6, 7, 3, 2, 1])],
> rec(acting := true));;
gap> x := PartialPerm([1, 3, 4, 5, 6], [1, 3, 4, 5, 6]);;
gap> D := DClass(S, x);;
gap> RClassReps(D);
[ <identity partial perm on [ 1, 3, 4, 5, 6 ]>, [7,4,3,6,5](1) ]
gap> HClassReps(D);
[ <identity partial perm on [ 1, 3, 4, 5, 6 ]>, [5,6,3,4,7](1), 
  [7,4,3,6,5](1), <identity partial perm on [ 1, 3, 4, 6, 7 ]> ]
gap> L := LClass(S, x);;
gap> HClassReps(L);
[ <identity partial perm on [ 1, 3, 4, 5, 6 ]>, [7,4,3,6,5](1) ]

# Greens(H/L)Classes, for an inverse op acting D-class
gap> S := InverseSemigroup([
> PartialPerm([1, 2], [3, 2]),
> PartialPerm([1, 4], [1, 3]),
> PartialPerm([1, 3, 5], [4, 3, 1])],
> rec(acting := true));;
gap> GreensLClasses(S);
[ <Green's L-class: <identity partial perm on [ 2, 3 ]>>, 
  <Green's L-class: [3,1](2)>, 
  <Green's L-class: <identity partial perm on [ 1, 3 ]>>, 
  <Green's L-class: [3,4](1)>, <Green's L-class: [3,1,5]>, 
  <Green's L-class: [1,4](3)>, <Green's L-class: [1,5](3)>, 
  <Green's L-class: <identity partial perm on [ 1, 3, 4 ]>>, 
  <Green's L-class: [4,1,5](3)>, 
  <Green's L-class: <identity partial perm on [ 2 ]>>, 
  <Green's L-class: <empty partial perm>>, 
  <Green's L-class: <identity partial perm on [ 4 ]>>, 
  <Green's L-class: [4,3]>, <Green's L-class: [4,1]>, 
  <Green's L-class: [4,5]> ]
gap> x := PartialPerm([1, 4], [5, 1]);;
gap> GreensHClasses(DClass(S, x));
[ <Green's H-class: <identity partial perm on [ 1, 3 ]>>, 
  <Green's H-class: [3,4](1)>, <Green's H-class: [3,1,5]>, 
  <Green's H-class: [1,4](3)>, <Green's H-class: [1,5](3)>, 
  <Green's H-class: [4,3](1)>, 
  <Green's H-class: <identity partial perm on [ 1, 4 ]>>, 
  <Green's H-class: [4,1,5]>, <Green's H-class: [1,4,3]>, 
  <Green's H-class: [1,5][4,3]>, <Green's H-class: [5,1,3]>, 
  <Green's H-class: [5,1,4]>, 
  <Green's H-class: <identity partial perm on [ 1, 5 ]>>, 
  <Green's H-class: [1,3][5,4]>, <Green's H-class: [1,3](5)>, 
  <Green's H-class: [4,1](3)>, <Green's H-class: [3,4,1]>, 
  <Green's H-class: [3,1][4,5]>, 
  <Green's H-class: <identity partial perm on [ 3, 4 ]>>, 
  <Green's H-class: [4,5](3)>, <Green's H-class: [5,1](3)>, 
  <Green's H-class: [3,4][5,1]>, <Green's H-class: [3,1](5)>, 
  <Green's H-class: [5,4](3)>, 
  <Green's H-class: <identity partial perm on [ 3, 5 ]>> ]
gap> GreensHClasses(RClass(S, x));
[ <Green's H-class: [4,3](1)>, 
  <Green's H-class: <identity partial perm on [ 1, 4 ]>>, 
  <Green's H-class: [4,1,5]>, <Green's H-class: [1,4,3]>, 
  <Green's H-class: [1,5][4,3]> ]
gap> GreensHClasses(LClass(S, x));
[ <Green's H-class: [3,1,5]>, <Green's H-class: [4,1,5]>, 
  <Green's H-class: <identity partial perm on [ 1, 5 ]>>, 
  <Green's H-class: [3,1][4,5]>, <Green's H-class: [3,1](5)> ]
gap> GreensHClasses(HClass(S, x));
Error, the argument is not a Green's L-, R-, or D-class

# Nr(H/R/L)Classes, for an inverse op acting semigroup
gap> S := InverseSemigroup([
> PartialPerm([1, 2], [3, 2]),
> PartialPerm([1, 4], [1, 3]),
> PartialPerm([1, 3, 5], [4, 3, 1])],
> rec(acting := true));;
gap> NrRClasses(S);
15
gap> NrLClasses(S);
15
gap> NrHClasses(S);
51

# Nr(R/L)Classes, for an inverse op acting Greens class
gap> S := InverseSemigroup([
> PartialPerm([1, 2], [3, 2]),
> PartialPerm([1, 4], [1, 3]),
> PartialPerm([1, 3, 5], [4, 3, 1])],
> rec(acting := true));;
gap> x := PartialPerm([1, 3, 4], [5, 3, 1]);;
gap> D := DClass(S, x);;
gap> NrRClasses(D);
2
gap> NrLClasses(D);
2
gap> NrHClasses(D);
4
gap> L := LClass(S, x);;
gap> NrHClasses(L);
2
gap> R := LClass(S, x);;
gap> NrHClasses(R);
2

# GroupHClassOfGreensDClass, for an inverse op acting D-class
gap> S := InverseSemigroup([
> PartialPerm([1, 2], [3, 2]),
> PartialPerm([1, 4], [1, 3]),
> PartialPerm([1, 3, 5], [4, 3, 1])],
> rec(acting := true));;
gap> GroupHClassOfGreensDClass(DClass(S, PartialPerm([])));
<Green's H-class: <empty partial perm>>
gap> GroupHClassOfGreensDClass(DClass(S, PartialPerm([3, 5], [4, 1])));
<Green's H-class: <identity partial perm on [ 1, 3 ]>>

# PartialOrderOfDClasses, for an inverse op acting semigroup
gap> S := SymmetricInverseSemigroup(5);;
gap> S := InverseSemigroup(S, rec(acting := true));;
gap> OutNeighbours(DigraphReflexiveTransitiveReduction(
> PartialOrderOfDClasses(S)));
[ [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 6 ], [  ] ]

# (Nr)Idempotents, for an inv op acting R/L/D-class
gap> S := InverseSemigroup([
>   Bipartition([[1, -4], [2, 4, 5, 6, -1, -2, -5, -6], [3, -3]]),
>   Bipartition([[1, -4], [2, -1], [3, -2], [4, 5, 6, -3, -5, -6]]),
>   Bipartition([[1, -4], [2, -5], [3, 5, 6, -2, -3, -6], [4, -1]]),
>   Bipartition([[1, -5], [2, 5, 6, -3, -4, -6], [3, -2], [4, -1]])],
> rec(acting := true));;
gap> x := Bipartition([[1, 2, 5, 6, -2, -4, -5, -6], [3, -3], [4, -1]]);;
gap> NrIdempotents(HClass(S, x));
0
gap> Idempotents(HClass(S, x));
[  ]
gap> NrIdempotents(RClass(S, x));
1
gap> Idempotents(RClass(S, x));
[ <block bijection: [ 1, 2, 5, 6, -1, -2, -5, -6 ], [ 3, -3 ], [ 4, -4 ]> ]
gap> NrIdempotents(LClass(S, x));
1
gap> Idempotents(LClass(S, x));
[ <block bijection: [ 1, -1 ], [ 2, 4, 5, 6, -2, -4, -5, -6 ], [ 3, -3 ]> ]
gap> NrIdempotents(DClass(S, x));
9
gap> Idempotents(DClass(S, x));
[ <block bijection: [ 1, 2, 5, 6, -1, -2, -5, -6 ], [ 3, -3 ], [ 4, -4 ]>, 
  <block bijection: [ 1, -1 ], [ 2, 4, 5, 6, -2, -4, -5, -6 ], [ 3, -3 ]>, 
  <block bijection: [ 1, 3, 5, 6, -1, -3, -5, -6 ], [ 2, -2 ], [ 4, -4 ]>, 
  <block bijection: [ 1, -1 ], [ 2, 3, 4, 6, -2, -3, -4, -6 ], [ 5, -5 ]>, 
  <block bijection: [ 1, -1 ], [ 2, 3, 5, 6, -2, -3, -5, -6 ], [ 4, -4 ]>, 
  <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, 4, 5, 6, -3, -4, -5, -6 ]>, 
  <block bijection: [ 1, 2, 3, 6, -1, -2, -3, -6 ], [ 4, -4 ], [ 5, -5 ]>, 
  <block bijection: [ 1, 4, 5, 6, -1, -4, -5, -6 ], [ 2, -2 ], [ 3, -3 ]>, 
  <block bijection: [ 1, 3, 4, 6, -1, -3, -4, -6 ], [ 2, -2 ], [ 5, -5 ]> ]

# (Nr)Idempotents, for an inverse op acting semigroup
gap> S := InverseSemigroup(SymmetricInverseSemigroup(5),
> rec(acting := true));;
gap> Idempotents(S, 3);
[ <identity partial perm on [ 1, 2, 3 ]>, 
  <identity partial perm on [ 3, 4, 5 ]>, 
  <identity partial perm on [ 2, 3, 4 ]>, 
  <identity partial perm on [ 2, 4, 5 ]>, 
  <identity partial perm on [ 1, 4, 5 ]>, 
  <identity partial perm on [ 1, 3, 4 ]>, 
  <identity partial perm on [ 2, 3, 5 ]>, 
  <identity partial perm on [ 1, 3, 5 ]>, 
  <identity partial perm on [ 1, 2, 5 ]>, 
  <identity partial perm on [ 1, 2, 4 ]> ]
gap> Length(last) = Binomial(5, 3);
true
gap> S := InverseSemigroup([
>  PartialPerm([1, 2, 3], [1, 3, 4]),
>  PartialPerm([1, 2, 3], [2, 5, 3]),
>  PartialPerm([1, 2, 3, 4], [2, 4, 1, 5]),
>  PartialPerm([1, 3, 5], [5, 1, 3])],
> rec(acting := true));;
gap> NrIdempotents(S);
25
gap> Idempotents(S);
[ <identity partial perm on [ 1, 3, 4 ]>, 
  <identity partial perm on [ 2, 3, 5 ]>, 
  <identity partial perm on [ 1, 2, 4, 5 ]>, 
  <identity partial perm on [ 1, 3, 5 ]>, 
  <identity partial perm on [ 1, 2, 3 ]>, 
  <identity partial perm on [ 1, 2, 3, 4 ]>, 
  <identity partial perm on [ 1, 4 ]>, <identity partial perm on [ 2, 3 ]>, 
  <identity partial perm on [ 1, 2, 5 ]>, <identity partial perm on [ 1, 5 ]>,
  <identity partial perm on [ 3 ]>, <identity partial perm on [ 3, 4 ]>, 
  <identity partial perm on [ 3, 5 ]>, <identity partial perm on [ 1, 3 ]>, 
  <identity partial perm on [ 2 ]>, <identity partial perm on [ 2, 5 ]>, 
  <identity partial perm on [ 2, 4, 5 ]>, <identity partial perm on [ 1, 2 ]>,
  <identity partial perm on [ 1, 2, 4 ]>, <identity partial perm on [ 1 ]>, 
  <identity partial perm on [ 5 ]>, <empty partial perm>, 
  <identity partial perm on [ 2, 4 ]>, <identity partial perm on [ 4 ]>, 
  <identity partial perm on [ 4, 5 ]> ]
gap> Length(Idempotents(S)) = NrIdempotents(S);
true
gap> Idempotents(S, 4);
[ <identity partial perm on [ 1, 2, 4, 5 ]>, 
  <identity partial perm on [ 1, 2, 3, 4 ]> ]
gap> Idempotents(S, -1);
Error, the 2nd argument (an int) is not non-negative

# IteratorOfRClassReps
gap> S := InverseSemigroup(SymmetricInverseSemigroup(5),
> rec(acting := true));;
gap> it := IteratorOfRClassReps(S);
<iterator>
gap> NextIterator(it);
<identity partial perm on [ 1, 2, 3, 4, 5 ]>
gap> NextIterator(it);
<identity partial perm on [ 1, 2, 3, 4 ]>
gap> for x in it do od;
gap> IsDoneIterator(it);
true
gap> RClassReps(S);;
gap> it := IteratorOfRClassReps(S);
<iterator>
gap> NextIterator(it);
<identity partial perm on [ 1, 2, 3, 4, 5 ]>
gap> NextIterator(it);
<identity partial perm on [ 1, 2, 3, 4 ]>
gap> for x in it do od;
gap> IsDoneIterator(it);
true

# Enumerator for an L-class
gap> S := InverseSemigroup(SymmetricInverseSemigroup(5),
> rec(acting := true));;
gap> x := PartialPerm([1, 2, 3, 5], [2, 4, 3, 5]);
[1,2,4](3)(5)
gap> L := LClass(S, x);
<Green's L-class: [1,2,4](3)(5)>
gap> en := Enumerator(L);
<enumerator of <Green's L-class: [1,2,4](3)(5)>>
gap> ForAll(en, x -> en[Position(en, x)] = x);
true
gap> ForAll([1 .. Length(en)], i -> Position(en, en[i]) = i);
true
gap> Position(en, PartialPerm([1, 2, 3], [5, 2, 3]));
fail
gap> S := InverseSemigroup(PartialPerm([1, 2, 3], [2, 3, 1]),
> rec(acting := true));
<partial perm group of rank 3 with 1 generator>
gap> x := PartialPerm([1, 2, 3]);
<identity partial perm on [ 1, 2, 3 ]>
gap> y := PartialPerm([1, 2, 4], [1, 2, 3]);
[4,3](1)(2)
gap> en := Enumerator(LClass(S, x));;
gap> Position(en, y);
fail
gap> en[10000];
fail

# Enumerator for a D-class
gap> S := InverseSemigroup(SymmetricInverseSemigroup(5),
> rec(acting := true));;
gap> x := PartialPerm([1, 2, 3, 5], [2, 4, 3, 5]);
[1,2,4](3)(5)
gap> D := DClass(S, x);
<Green's D-class: [1,2,4](3)(5)>
gap> en := Enumerator(D);
<enumerator of <Green's D-class: [1,2,4](3)(5)>>
gap> ForAll(en, x -> en[Position(en, x)] = x);
true
gap> ForAll([1 .. Length(en)], i -> Position(en, en[i]) = i);
true
gap> Position(en, PartialPerm([1, 2, 3], [5, 2, 3]));
fail
gap> S := InverseSemigroup(PartialPerm([1, 2, 3], [2, 3, 1]),
> rec(acting := true));
<partial perm group of rank 3 with 1 generator>
gap> x := PartialPerm([1, 2, 3]);
<identity partial perm on [ 1, 2, 3 ]>
gap> y := PartialPerm([1, 2, 4], [1, 2, 3]);
[4,3](1)(2)
gap> en := Enumerator(DClass(S, x));
<enumerator of <Green's D-class: <identity partial perm on [ 1, 2, 3 ]>>>
gap> Position(en, y ^ -1);
fail
gap> Position(en, y);
fail
gap> en[10000];
fail

# Iterator for an L-class
gap> S := InverseSemigroup(SymmetricInverseSemigroup(5),
> rec(acting := true));;
gap> x := PartialPerm([1, 2, 3, 5], [2, 4, 3, 5]);
[1,2,4](3)(5)
gap> L := LClass(S, x);
<Green's L-class: [1,2,4](3)(5)>
gap> it := Iterator(L);
<iterator>
gap> for x in it do Assert(0, x in L); od;
gap> L := LClass(S, x);
<Green's L-class: [1,5,2,3,4]>
gap> AsSSortedList(L);;
gap> Iterator(L);
<iterator>

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/greens/acting-inverse.tst");
