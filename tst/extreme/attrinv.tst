#############################################################################
##
#W  extreme/attrinv.tst
#Y  Copyright (C) 2012-15                                   Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
#
#############################################################################
##

#@local A, B, C, D, I, I2, I5, J, J2, P, Q, R, S, T, U, V, VPR, W, WW, a, b
#@local cosets, d, f, f1, f2, f3, g, gens, h, h1, h2, inv, iso, m, m1, m2, t
#@local tmp, w, ww, x, xx, y, yy, z, zz
gap> START_TEST("Semigroups package: extreme/attrinv.tst");
gap> LoadPackage("semigroups", false);;

# 
gap> SEMIGROUPS.StartTest();

# AttributesInverseTest1: JoinIrreducibleDClasses
gap> S := InverseSemigroup([
>  PartialPerm([1, 2, 3, 4], [4, 1, 2, 6]),
>  PartialPerm([1, 2, 3, 4], [5, 7, 1, 6]),
>  PartialPerm([1, 2, 3, 5], [5, 2, 7, 3]),
>  PartialPerm([1, 2, 3, 6, 7], [1, 3, 4, 7, 5]),
>  PartialPerm([1, 2, 3, 4, 5, 7], [3, 2, 4, 6, 1, 5])]);
<inverse partial perm semigroup of rank 7 with 5 generators>
gap> iso := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> A := Range(iso);
<inverse bipartition semigroup of degree 7 with 5 generators>
gap> I := SemigroupIdeal(S, PartialPerm([1, 3, 4, 5, 7], [1, 3, 4, 5, 7]));
<inverse partial perm semigroup ideal of rank 7 with 1 generator>
gap> B := InverseSemigroup([
> Bipartition([[1, -6], [2, -4], [3, -3], [4, 5, 6, 7, -1, -2, -5, -7]]),
> Bipartition([[1, -4], [2, -5], [3, 6, 7, -2, -3, -7], [4, -1], [5, -6]]),
> Bipartition([[1, -6], [2, -5], [3, 5, 7, -3, -4, -7], [4, -2], [6, -1]])]);
<inverse block bijection semigroup of degree 7 with 3 generators>
gap> J := SemigroupIdeal(B,
>  Bipartition([[1, -1], [2, 3, 5, 7, -2, -3, -5, -7], [4, -4],
>    [6, -6]]),
>  Bipartition([[1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7], [3, -3]]));
<inverse bipartition semigroup ideal of degree 7 with 2 generators>
gap> D := JoinIrreducibleDClasses(S);;
gap> Length(D) = 1;
true
gap> D := D[1];;
gap> PartialPerm([4], [4]) in D and PartialPerm([3], [6]) in D;
true
gap> Size(D);
49
gap> D := JoinIrreducibleDClasses(I);;
gap> Length(D) = 1;
true
gap> D := D[1];;
gap> PartialPerm([4], [4]) in D and PartialPerm([1], [4]) in D;
true
gap> Size(D);
49
gap> D := JoinIrreducibleDClasses(A);;
gap> Length(D) = 1;
true
gap> D[1] = DClass(A, Bipartition([[1], [2], [3], [4, -4], [5], [6], [7], [-1],
> [-2], [-3], [-5], [-6], [-7]]));
true
gap> Size(D);
1
gap> Set(JoinIrreducibleDClasses(B)) =
> Set([DClass(B, 
>             Bipartition([[1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7], 
>                          [3, -3]])), 
>      DClass(B, 
>             Bipartition([[1, -1], 
>                          [2, 3, 4, 5, 6, 7, -2, -3, -4, -5, -6, -7]]))]);
true
gap> Set(JoinIrreducibleDClasses(J)) =
> Set([DClass(J, 
>             Bipartition([[1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7], 
>                          [3, -3]])), 
>      DClass(J, 
>             Bipartition([[1, 2, 3, 4, 5, 7, -1, -2, -3, -4, -5, -7], 
>                          [6, -6]]))]);
true

# AttributesInverseTest2: IsJoinIrreducible
gap> S := InverseSemigroup([
>  PartialPerm([1, 2, 3, 4], [4, 1, 2, 6]),
>  PartialPerm([1, 2, 3, 4], [5, 7, 1, 6]),
>  PartialPerm([1, 2, 3, 5], [5, 2, 7, 3]),
>  PartialPerm([1, 2, 3, 6, 7], [1, 3, 4, 7, 5]),
>  PartialPerm([1, 2, 3, 4, 5, 7], [3, 2, 4, 6, 1, 5])]);;
gap> iso := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> A := Range(iso);;
gap> I := SemigroupIdeal(S,
> PartialPerm([1, 3, 4, 5, 7], [1, 3, 4, 5, 7]));;
gap> B := InverseSemigroup([
> Bipartition([[1, -6], [2, -4], [3, -3], [4, 5, 6, 7, -1, -2, -5, -7]]),
> Bipartition([[1, -4], [2, -5], [3, 6, 7, -2, -3, -7], [4, -1], [5, -6]]),
> Bipartition([[1, -6], [2, -5], [3, 5, 7, -3, -4, -7], [4, -2], [6, -1]])]);;
gap> J := SemigroupIdeal(B,
>  Bipartition([[1, -1], [2, 3, 5, 7, -2, -3, -5, -7], [4, -4],
>    [6, -6]]),
>  Bipartition([[1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7], [3, -3]]));;
gap> x := PartialPerm([1, 2, 4, 6], [2, 3, 1, 4]);;   xx := x ^ iso;;
gap> x in S;
true
gap> IsJoinIrreducible(S, x);
false
gap> x in I;
true
gap> IsJoinIrreducible(I, x);
false
gap> xx in A;
true
gap> IsJoinIrreducible(A, xx);
false
gap> y := PartialPerm([5], [3]);;   yy := y ^ iso;;
gap> IsJoinIrreducible(S, y);
true
gap> IsJoinIrreducible(I, y);
true
gap> IsJoinIrreducible(A, yy);
true
gap> P := Bipartition([[1, 3, 5, 6, 7, -3, -4, -5, -6, -7], [2, -2],
> [4, -1]]);;
gap> IsJoinIrreducible(B, P);
false
gap> IsJoinIrreducible(J, P);
false
gap> Q := Bipartition([[1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7],
> [3, -3]]);;
gap> IsJoinIrreducible(B, Q);
true
gap> IsJoinIrreducible(J, Q);
true
gap> R := Bipartition([[1, -4],
> [2, 3, 4, 5, 6, 7, -1, -2, -3, -5, -6, -7]]);;
gap> IsJoinIrreducible(B, R);
true
gap> IsJoinIrreducible(J, R);
true

# AttributesInverseTest3: Minorants 1
gap> S := InverseSemigroup([
>  PartialPerm([1, 2, 3, 4], [4, 1, 2, 6]),
>  PartialPerm([1, 2, 3, 4], [5, 7, 1, 6]),
>  PartialPerm([1, 2, 3, 5], [5, 2, 7, 3]),
>  PartialPerm([1, 2, 3, 6, 7], [1, 3, 4, 7, 5]),
>  PartialPerm([1, 2, 3, 4, 5, 7], [3, 2, 4, 6, 1, 5])]);;
gap> iso := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> A := Range(iso);;
gap> I := SemigroupIdeal(S,
> PartialPerm([1, 3, 4, 5, 7], [1, 3, 4, 5, 7]));;
gap> B := InverseSemigroup([
> Bipartition([[1, -6], [2, -4], [3, -3], [4, 5, 6, 7, -1, -2, -5, -7]]),
> Bipartition([[1, -4], [2, -5], [3, 6, 7, -2, -3, -7], [4, -1], [5, -6]]),
> Bipartition([[1, -6], [2, -5], [3, 5, 7, -3, -4, -7], [4, -2], [6, -1]])]);;
gap> J := SemigroupIdeal(B,
>  Bipartition([[1, -1], [2, 3, 5, 7, -2, -3, -5, -7],
>    [4, -4], [6, -6]]),
>  Bipartition([[1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7], [3, -3]]));;
gap> x := PartialPerm([1, 2, 4, 6], [2, 3, 1, 4]);;
gap> Minorants(S, x);
[ <empty partial perm>, [2,3], [1,2], [6,4], [4,1], [2,3][4,1], [1,2][6,4], 
  [2,3][6,4], [6,4,1], [4,1,2], [1,2,3], [4,1,2,3], [2,3][6,4,1], [6,4,1,2], 
  [1,2,3][6,4] ]
gap> Minorants(I, x);
[ <empty partial perm>, [2,3], [1,2], [4,1], [6,4], [1,2,3], [2,3][4,1], 
  [1,2][6,4], [2,3][6,4], [6,4,1], [4,1,2], [4,1,2,3], [2,3][6,4,1], 
  [6,4,1,2], [1,2,3][6,4] ]
gap> Minorants(A, x ^ iso);
[ <bipartition: [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 6 ], [ 7 ], [ -1 ], 
     [ -2 ], [ -3 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 6, -4 ], [ 7 ], [ -1 ], 
     [ -2 ], [ -3 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2 ], [ 3 ], [ 4, -1 ], [ 5 ], [ 6 ], [ 7 ], [ -2 ], 
     [ -3 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1, -2 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 6 ], [ 7 ], [ -1 ], 
     [ -3 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2, -3 ], [ 3 ], [ 4 ], [ 5 ], [ 6 ], [ 7 ], [ -1 ], 
     [ -2 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2, -3 ], [ 3 ], [ 4 ], [ 5 ], [ 6, -4 ], [ 7 ], 
     [ -1 ], [ -2 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1, -2 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ], [ 6, -4 ], [ 7 ], 
     [ -1 ], [ -3 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1, -2 ], [ 2, -3 ], [ 3 ], [ 4 ], [ 5 ], [ 6 ], [ 7 ], 
     [ -1 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2 ], [ 3 ], [ 4, -1 ], [ 5 ], [ 6, -4 ], [ 7 ], 
     [ -2 ], [ -3 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2, -3 ], [ 3 ], [ 4, -1 ], [ 5 ], [ 6 ], [ 7 ], 
     [ -2 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1, -2 ], [ 2 ], [ 3 ], [ 4, -1 ], [ 5 ], [ 6 ], [ 7 ], 
     [ -3 ], [ -4 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1, -2 ], [ 2, -3 ], [ 3 ], [ 4, -1 ], [ 5 ], [ 6 ], [ 7 ], 
     [ -4 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1, -2 ], [ 2 ], [ 3 ], [ 4, -1 ], [ 5 ], [ 6, -4 ], [ 7 ], 
     [ -3 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2, -3 ], [ 3 ], [ 4, -1 ], [ 5 ], [ 6, -4 ], [ 7 ], 
     [ -2 ], [ -5 ], [ -6 ], [ -7 ]>, 
  <bipartition: [ 1, -2 ], [ 2, -3 ], [ 3 ], [ 4 ], [ 5 ], [ 6, -4 ], [ 7 ], 
     [ -1 ], [ -5 ], [ -6 ], [ -7 ]> ]
gap> z := PartialPerm([], []);
<empty partial perm>
gap> zz := z ^ iso;;
gap> z in S;
true
gap> z in I;
true
gap> zz in A;
true
gap> Minorants(S, z);
[  ]
gap> Minorants(I, z);
[  ]
gap> Minorants(A, zz);
[  ]
gap> P := Bipartition([[1, 3, 5, 6, 7, -3, -4, -5, -6, -7], [2, -2],
> [4, -1]]);;
gap> Q := Bipartition([[1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7],
> [3, -3]]);;
gap> R := Bipartition([[1, -4],
> [2, 3, 4, 5, 6, 7, -1, -2, -3, -5, -6, -7]]);;
gap> m := Minorants(B, Q);
[ <block bijection: [ 1, 2, 3, 4, 5, 6, 7, -1, -2, -3, -4, -5, -6, -7 ]> ]
gap> Minorants(B, R) = m;
true
gap> Minorants(J, Q) = m;
true
gap> Minorants(J, R) = m;
true
gap> m[1] = MultiplicativeZero(B);
true
gap> m := Minorants(B, Bipartition(
> [[1, -6], [2, -4], [3, 4, 7, -3, -5, -7], [5, -2], [6, -1]]));;
gap> Size(m);
15
gap> m1 := Minorants(J, Bipartition(
> [[1, -6], [2, -5], [3, 5, 6, 7, -1, -3, -4, -7], [4, -2]]));;
gap> Size(m1);
7

# AttributesInverseTest4: Minorants 2
gap> U := InverseSemigroup(
> PartialPerm([1, 3, 4, 5, 7], [1, 5, 3, 8, 4]),
> PartialPerm([1, 2, 3, 4, 5, 6], [6, 7, 1, 4, 3, 2]),
> PartialPerm([1, 2, 3, 4, 5, 8], [5, 6, 3, 8, 4, 7]),
> PartialPerm([1, 3, 4, 5, 6, 8], [8, 7, 5, 1, 3, 4]),
> PartialPerm([1, 3, 4, 5, 7, 8], [6, 5, 7, 1, 4, 2]));;
gap> t := PartialPerm([4, 5, 7, 8], [5, 4, 1, 6]);;
gap> t in U;
true
gap> Minorants(U, t);
[ <empty partial perm>, [7,1], [4,5], [8,6], [5,4], [5,4][7,1], [7,1][8,6], 
  [5,4][8,6], [4,5][7,1], [4,5][8,6], (4,5), [8,6](4,5), [5,4][7,1][8,6], 
  [4,5][7,1][8,6], [7,1](4,5) ]

# AttributesInverseTest5: MajorantClosure and IsMajorantlyClosed
gap> S := InverseSemigroup([
>  PartialPerm([1, 2, 3, 4], [4, 1, 2, 6]),
>  PartialPerm([1, 2, 3, 4], [5, 7, 1, 6]),
>  PartialPerm([1, 2, 3, 5], [5, 2, 7, 3]),
>  PartialPerm([1, 2, 3, 6, 7], [1, 3, 4, 7, 5]),
>  PartialPerm([1, 2, 3, 4, 5, 7], [3, 2, 4, 6, 1, 5])]);;
gap> iso := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> A := Range(iso);;
gap> I := SemigroupIdeal(S,
> PartialPerm([1, 3, 4, 5, 7], [1, 3, 4, 5, 7]));;
gap> B := InverseSemigroup([
> Bipartition([[1, -6], [2, -4], [3, -3], [4, 5, 6, 7, -1, -2, -5, -7]]),
> Bipartition([[1, -4], [2, -5], [3, 6, 7, -2, -3, -7], [4, -1], [5, -6]]),
> Bipartition([[1, -6], [2, -5], [3, 5, 7, -3, -4, -7], [4, -2], [6, -1]])]);;
gap> J := SemigroupIdeal(B,
>  Bipartition([[1, -1], [2, 3, 5, 7, -2, -3, -5, -7], [4, -4],
>    [6, -6]]),
>  Bipartition([[1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7], [3, -3]]));;
gap> x := PartialPerm([1, 2, 4, 6], [2, 3, 1, 4]);;   xx := x ^ iso;;
gap> y := PartialPerm([5], [3]);;   yy := y ^ iso;;
gap> P := Bipartition([[1, 3, 5, 6, 7, -3, -4, -5, -6, -7], [2, -2],
> [4, -1]]);;
gap> Q := Bipartition([[1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7],
> [3, -3]]);;
gap> R := Bipartition([[1, -4],
> [2, 3, 4, 5, 6, 7, -1, -2, -3, -5, -6, -7]]);;
gap> IsMajorantlyClosed(S, [x]);
true
gap> MajorantClosure(S, [x]) = [x];
true
gap> IsMajorantlyClosed(I, [x]);
true
gap> MajorantClosure(I, [x]) = [x];
true
gap> IsMajorantlyClosed(A, [xx]);
true
gap> MajorantClosure(A, [xx]) = [xx];
true
gap> IsMajorantlyClosed(S, [y]);
false
gap> IsMajorantlyClosed(I, [y]);
false
gap> IsMajorantlyClosed(A, [yy]);
false
gap> m := MajorantClosure(S, [y]);;
gap> Size(m);
486
gap> m1 := MajorantClosure(I, [y]);;
gap> Size(m1);
485
gap> m2 := MajorantClosure(A, [yy]);;
gap> Size(m2);
486
gap> ForAll(m1, x -> x in m);
true
gap> IsMajorantlyClosed(B, [P]);
true
gap> IsMajorantlyClosed(B, [Q]);
false
gap> IsMajorantlyClosed(B, [R]);
false
gap> IsMajorantlyClosed(J, [P]);
true
gap> IsMajorantlyClosed(J, [Q]);
true
gap> IsMajorantlyClosed(J, [R]);
false
gap> MajorantClosure(B, [Q]);
[ <block bijection: [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ], [ 3, -3 ]>, 
  <block bijection: [ 1, 2, 5, 7, -1, -2, -5, -7 ], [ 3, -3 ], [ 4, -4 ], 
     [ 6, -6 ]>, <block bijection: [ 1, 2, 5, 7, -4, -5, -6, -7 ], [ 3, -3 ], 
     [ 4, -2 ], [ 6, -1 ]>, 
  <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], 
     [ 4, 5, 6, 7, -4, -5, -6, -7 ]>, 
  <block bijection: [ 1, -6 ], [ 2, -4 ], [ 3, -3 ], 
     [ 4, 5, 6, 7, -1, -2, -5, -7 ]> ]
gap> m := MajorantClosure(B, [R]);;
gap> IsMajorantlyClosed(B, m);
true
gap> Size(MajorantClosure(B, [Q, R]));
33
gap> m := MajorantClosure(J, [R]);;
gap> IsMajorantlyClosed(J, m);
true
gap> Size(m);
25

# AttributesInverseTest6: RightCosetsOfInverseSemigroup
gap> S := InverseSemigroup([
>  PartialPerm([1, 2, 3, 4], [4, 1, 2, 6]),
>  PartialPerm([1, 2, 3, 4], [5, 7, 1, 6]),
>  PartialPerm([1, 2, 3, 5], [5, 2, 7, 3]),
>  PartialPerm([1, 2, 3, 6, 7], [1, 3, 4, 7, 5]),
>  PartialPerm([1, 2, 3, 4, 5, 7], [3, 2, 4, 6, 1, 5])]);;
gap> iso := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> A := Range(iso);;
gap> B := InverseSemigroup([
> Bipartition([[1, -6], [2, -4], [3, -3], [4, 5, 6, 7, -1, -2, -5, -7]]),
> Bipartition([[1, -4], [2, -5], [3, 6, 7, -2, -3, -7], [4, -1], [5, -6]]),
> Bipartition([[1, -6], [2, -5], [3, 5, 7, -3, -4, -7], [4, -2], [6, -1]])]);;
gap> w := PartialPerm([1, 2, 3, 4], [1, 2, 3, 4]);;
gap> m := MajorantClosure(S, [w]);;
gap> W := InverseSemigroup(m);
<inverse partial perm semigroup of rank 7 with 5 generators>
gap> IsMajorantlyClosed(S, W);
true
gap> cosets := RightCosetsOfInverseSemigroup(S, W);;
gap> Sort(cosets);
gap> cosets;
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
gap> ww := w ^ iso;;
gap> m := MajorantClosure(A, [ww]);;
gap> WW := InverseSemigroup(m);
<inverse bipartition semigroup of degree 7 with 5 generators>
gap> IsMajorantlyClosed(A, WW);
true
gap> cosets := RightCosetsOfInverseSemigroup(A, WW);;
gap> Sort(cosets);
gap> cosets;
[ [ <bipartition: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], [ 4, -4 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -5 ], [ -6 ], [ -7 ]>, 
      <bipartition: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], [ 4, -4 ], [ 5, -5 ], 
         [ 6, -6 ], [ 7 ], [ -7 ]>, 
      <bipartition: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], [ 4, -4 ], [ 5, -5 ], 
         [ 6 ], [ 7, -7 ], [ -6 ]>, 
      <bipartition: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], [ 4, -4 ], [ 5, -5 ], 
         [ 6 ], [ 7 ], [ -6 ], [ -7 ]>, 
      <bipartition: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], [ 4, -4 ], [ 5 ], 
         [ 6, -6 ], [ 7 ], [ -5 ], [ -7 ]> ], 
  [ <bipartition: [ 1, -1 ], [ 2, -7 ], [ 3, -2 ], [ 4, -3 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -4 ], [ -5 ], [ -6 ]> ], 
  [ <bipartition: [ 1, -1 ], [ 2, -5 ], [ 3, -3 ], [ 4, -4 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -2 ], [ -6 ], [ -7 ]> ], 
  [ <bipartition: [ 1, -4 ], [ 2, -1 ], [ 3, -2 ], [ 4, -6 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -3 ], [ -5 ], [ -7 ]> ], 
  [ <bipartition: [ 1, -3 ], [ 2, -1 ], [ 3, -4 ], [ 4, -6 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -2 ], [ -5 ], [ -7 ]> ], 
  [ <bipartition: [ 1, -5 ], [ 2, -2 ], [ 3, -1 ], [ 4, -3 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -4 ], [ -6 ], [ -7 ]>, 
      <bipartition: [ 1, -5 ], [ 2, -2 ], [ 3, -1 ], [ 4, -3 ], [ 5, -7 ], 
         [ 6, -4 ], [ 7 ], [ -6 ]>, 
      <bipartition: [ 1, -5 ], [ 2, -2 ], [ 3, -1 ], [ 4, -3 ], [ 5 ], 
         [ 6, -4 ], [ 7 ], [ -6 ], [ -7 ]>, 
      <bipartition: [ 1, -5 ], [ 2, -2 ], [ 3, -1 ], [ 4, -3 ], [ 5, -7 ], 
         [ 6 ], [ 7 ], [ -4 ], [ -6 ]> ], 
  [ <bipartition: [ 1, -4 ], [ 2, -2 ], [ 3, -1 ], [ 4, -5 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -3 ], [ -6 ], [ -7 ]> ], 
  [ <bipartition: [ 1, -7 ], [ 2, -6 ], [ 3, -1 ], [ 4, -2 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -3 ], [ -4 ], [ -5 ]> ], 
  [ <bipartition: [ 1, -5 ], [ 2, -7 ], [ 3, -1 ], [ 4, -3 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -2 ], [ -4 ], [ -6 ]> ], 
  [ <bipartition: [ 1, -5 ], [ 2, -7 ], [ 3, -1 ], [ 4, -6 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -2 ], [ -3 ], [ -4 ]> ], 
  [ <bipartition: [ 1, -6 ], [ 2, -2 ], [ 3, -3 ], [ 4, -1 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -4 ], [ -5 ], [ -7 ]> ], 
  [ <bipartition: [ 1, -7 ], [ 2, -2 ], [ 3, -5 ], [ 4, -1 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -3 ], [ -4 ], [ -6 ]>, 
      <bipartition: [ 1, -7 ], [ 2, -2 ], [ 3, -5 ], [ 4, -1 ], [ 5 ], 
         [ 6, -3 ], [ 7 ], [ -4 ], [ -6 ]> ], 
  [ <bipartition: [ 1, -7 ], [ 2, -3 ], [ 3, -4 ], [ 4, -1 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -2 ], [ -5 ], [ -6 ]> ], 
  [ <bipartition: [ 1, -3 ], [ 2, -2 ], [ 3, -4 ], [ 4, -6 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -1 ], [ -5 ], [ -7 ]>, 
      <bipartition: [ 1, -3 ], [ 2, -2 ], [ 3, -4 ], [ 4, -6 ], [ 5, -1 ], 
         [ 6 ], [ 7, -5 ], [ -7 ]>, 
      <bipartition: [ 1, -3 ], [ 2, -2 ], [ 3, -4 ], [ 4, -6 ], [ 5, -1 ], 
         [ 6 ], [ 7 ], [ -5 ], [ -7 ]> ], 
  [ <bipartition: [ 1, -3 ], [ 2, -2 ], [ 3, -5 ], [ 4, -7 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -1 ], [ -4 ], [ -6 ]> ], 
  [ <bipartition: [ 1, -3 ], [ 2, -5 ], [ 3, -2 ], [ 4, -4 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -1 ], [ -6 ], [ -7 ]> ], 
  [ <bipartition: [ 1, -5 ], [ 2, -4 ], [ 3, -6 ], [ 4, -3 ], [ 5 ], [ 6 ], 
         [ 7 ], [ -1 ], [ -2 ], [ -7 ]> ] ]
gap> I2 := SemigroupIdeal(S, PartialPerm([1, 2, 3, 4, 5, 6]));
<inverse partial perm semigroup ideal of rank 7 with 1 generator>
gap> cosets := RightCosetsOfInverseSemigroup(I2, W);;
gap> Sort(cosets);
gap> cosets;
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
gap> C := Bipartition(
> [[1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7], [3, -3]]);;
gap> m := MajorantClosure(B, [C]);;
gap> V := InverseSemigroup(m);
<inverse block bijection semigroup of degree 7 with 5 generators>
gap> IsMajorantlyClosed(B, V);
true
gap> RightCosetsOfInverseSemigroup(B, V);
[ [ <block bijection: [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ], [ 3, -3 ]>
        , <block bijection: [ 1, 2, 5, 7, -1, -2, -5, -7 ], [ 3, -3 ], 
         [ 4, -4 ], [ 6, -6 ]>, 
      <block bijection: [ 1, 2, 5, 7, -4, -5, -6, -7 ], [ 3, -3 ], [ 4, -2 ], 
         [ 6, -1 ]>, <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], 
         [ 4, 5, 6, 7, -4, -5, -6, -7 ]>, 
      <block bijection: [ 1, -6 ], [ 2, -4 ], [ 3, -3 ], 
         [ 4, 5, 6, 7, -1, -2, -5, -7 ]> ] ]
gap> gens := [
> Bipartition(
>  [[1, 2, 5, 7, -1, -2, -5, -7], [3, -3], [4, -4], [6, -6]]),
> Bipartition(
>  [[1, -1], [2, 3, 7, -2, -3, -7], [4, -4], [5, -5], [6, -6]]),
> Bipartition(
>  [[1, -1], [2, -2], [3, 4, 7, -3, -4, -7], [5, -5], [6, -6]])];;
gap> J2 := SemigroupIdeal(B, gens);
<inverse bipartition semigroup ideal of degree 7 with 3 generators>
gap> RightCosetsOfInverseSemigroup(J2, V);
[ [ <block bijection: [ 1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ], [ 3, -3 ]>
        , <block bijection: [ 1, 2, 5, 7, -1, -2, -5, -7 ], [ 3, -3 ], 
         [ 4, -4 ], [ 6, -6 ]>, 
      <block bijection: [ 1, 2, 5, 7, -4, -5, -6, -7 ], [ 3, -3 ], [ 4, -2 ], 
         [ 6, -1 ]>, <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], 
         [ 4, 5, 6, 7, -4, -5, -6, -7 ]>, 
      <block bijection: [ 1, -6 ], [ 2, -4 ], [ 3, -3 ], 
         [ 4, 5, 6, 7, -1, -2, -5, -7 ]> ] ]

# AttributesInverseTest7: SameMinorantsSubgroup
# (trivial examples)
gap> S := InverseSemigroup([
>  PartialPerm([1, 2, 3, 4], [4, 1, 2, 6]),
>  PartialPerm([1, 2, 3, 4], [5, 7, 1, 6]),
>  PartialPerm([1, 2, 3, 5], [5, 2, 7, 3]),
>  PartialPerm([1, 2, 3, 6, 7], [1, 3, 4, 7, 5]),
>  PartialPerm([1, 2, 3, 4, 5, 7], [3, 2, 4, 6, 1, 5])]);;
gap> iso := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> A := Range(iso);;
gap> B := InverseSemigroup([
> Bipartition([[1, -6], [2, -4], [3, -3], [4, 5, 6, 7, -1, -2, -5, -7]]),
> Bipartition([[1, -4], [2, -5], [3, 6, 7, -2, -3, -7], [4, -1], [5, -6]]),
> Bipartition([[1, -6], [2, -5], [3, 5, 7, -3, -4, -7], [4, -2], [6, -1]])]);;
gap> J := SemigroupIdeal(B,
>  Bipartition([[1, -1], [2, 3, 5, 7, -2, -3, -5, -7], [4, -4],
>    [6, -6]]),
>  Bipartition([[1, 2, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7],
>    [3, -3]]));;
gap> h := GreensHClassOfElement(S, PartialPerm([1, 4, 6], [1, 4, 6]));
<Green's H-class: <identity partial perm on [ 1, 4, 6 ]>>
gap> SameMinorantsSubgroup(h);
[ <identity partial perm on [ 1, 4, 6 ]> ]
gap> x := Bipartition([[1, -1], [2], [3], [4, -4], [5], [6, -6], [7], [-2],
> [-3], [-5], [-7]]);;
gap> h := GreensHClassOfElement(A, x);
<Green's H-class: <bipartition: [ 1, -1 ], [ 2 ], [ 3 ], [ 4, -4 ], [ 5 ], 
  [ 6, -6 ], [ 7 ], [ -2 ], [ -3 ], [ -5 ], [ -7 ]>>
gap> SameMinorantsSubgroup(h);
[ <bipartition: [ 1, -1 ], [ 2 ], [ 3 ], [ 4, -4 ], [ 5 ], [ 6, -6 ], [ 7 ], 
     [ -2 ], [ -3 ], [ -5 ], [ -7 ]> ]
gap> x := Bipartition([[1, -1], [2, -2], [3, 4, 5, 6, 7, -3, -4, -5, -6, -7]]);;
gap> h := GreensHClassOfElement(B, x);
<Green's H-class: <block bijection: [ 1, -1 ], [ 2, -2 ], 
  [ 3, 4, 5, 6, 7, -3, -4, -5, -6, -7 ]>>
gap> SameMinorantsSubgroup(h);
[ <block bijection: [ 1, -1 ], [ 2, -2 ], 
     [ 3, 4, 5, 6, 7, -3, -4, -5, -6, -7 ]> ]
gap> x := Bipartition([[1, -1], [2, 3, 5, 6, 7, -2, -3, -5, -6, -7], [4, -4]]);;
gap> h := GreensHClassOfElement(J, x);;
gap> SameMinorantsSubgroup(h);
[ <block bijection: [ 1, -1 ], [ 2, 3, 5, 6, 7, -2, -3, -5, -6, -7 ], 
     [ 4, -4 ]> ]

# AttributesInverseTest8: SameMinorantsSubgroup 
# (non-trivial examples)
gap> f := PartialPermNC([2, 1, 4, 5, 3, 7, 6, 9, 10, 8]);;
gap> g := PartialPermNC([2, 1, 0, 0, 0, 7, 6]);;
gap> S := InverseSemigroup(f, g);;
gap> T := SemigroupIdeal(S, PartialPerm([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]));;
gap> d := DClass(S, PartialPerm([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]));;
gap> h1 := GroupHClass(d);
<Green's H-class: <identity partial perm on [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
 >>
gap> m1 := ShallowCopy(SameMinorantsSubgroup(h1));;
gap> Sort(m1);
gap> m1;
[ <identity partial perm on [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]>, 
  (1)(2)(3,4,5)(6)(7)(8,9,10), (1)(2)(3,5,4)(6)(7)(8,10,9) ]
gap> d := DClass(S, PartialPerm([1, 2, 6, 7], [1, 2, 6, 7]));;
gap> h2 := GroupHClass(d);
<Green's H-class: <identity partial perm on [ 1, 2, 6, 7 ]>>
gap> m2 := ShallowCopy(SameMinorantsSubgroup(h2));;
gap> Sort(m2);
gap> m2;
[ <identity partial perm on [ 1, 2, 6, 7 ]>, (1,2)(6,7) ]
gap> d := DClass(T, PartialPerm([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]));;
gap> h1 := GroupHClass(d);;
gap> tmp := ShallowCopy(SameMinorantsSubgroup(h1));;
gap> Sort(tmp);
gap> tmp = m1;
true
gap> d := DClass(T, PartialPerm([1, 2, 6, 7], [1, 2, 6, 7]));;
gap> h2 := GroupHClass(d);;
gap> tmp := ShallowCopy(SameMinorantsSubgroup(h2));;
gap> Sort(tmp);
gap> tmp = m2;
true

# AttributesInverseTest9: NaturalLeqInverseSemigroup
# for partial perms
gap> a := PartialPerm([1], [6]);;
gap> b := PartialPerm([1, 2, 5], [6, 3, 1]);;
gap> S := InverseSemigroup(a, b);;
gap> NaturalLeqInverseSemigroup(S)(a, b);
true
gap> NaturalLeqInverseSemigroup(S)(b, a);
false
gap> NaturalLeqInverseSemigroup(S)(a, a);
true

# AttributesInverseTest10: NaturalLeqInverseSemigroup
# for block bijections
gap> A := Bipartition([[1, 2, 3, 4, 6, 7, 8, -1, -2, -4, -5, -6, -7, -8],
> [5, -3]]);;
gap> B := Bipartition([[1, 2, 3, 4, 8, -4, -5, -6, -7, -8], [5, -3],
> [6, -2], [7, -1]]);;
gap> S := InverseSemigroup(A, B);;
gap> NaturalLeqInverseSemigroup(S)(A, B);
true
gap> NaturalLeqInverseSemigroup(S)(B, A);
false
gap> NaturalLeqInverseSemigroup(S)(B, B);
true

# AttributesInverseTest11: NaturalLeqInverseSemigroup
# for partial perm bipartitions
gap> f := Bipartition([[1, -2], [2], [-1]]);;
gap> f2 := Bipartition([[1, -2], [2], [3], [4], [5], [6], [-1],
> [-3], [-4], [-5], [-6]]);;
gap> g := Bipartition([[1, -2], [2], [3, -5], [4], [5], [-1],
> [-3], [-4]]);;
gap> S := InverseSemigroup(f);
<inverse bipartition semigroup of degree 2 with 1 generator>
gap> NaturalLeqInverseSemigroup(S)(f, f);
true
gap> NaturalLeqInverseSemigroup(S)(f, g);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `NaturalLeqPartialPermBipartition' on 2 \
arguments
gap> NaturalLeqInverseSemigroup(S)(f2, g);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `NaturalLeqPartialPermBipartition' on 2 \
arguments
gap> NaturalLeqInverseSemigroup(S)(f, f2);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `NaturalLeqPartialPermBipartition' on 2 \
arguments
gap> NaturalLeqInverseSemigroup(AsSemigroup(IsPartialPermSemigroup, S))
> (AsPartialPerm(f), AsPartialPerm(f2));
true
gap> AsPartialPerm(f) = AsPartialPerm(f2);
true
gap> f = f2;
false
gap> f2 := Bipartition([[1, -2], [2], [3], [4], [5], [-1],
> [-3], [-4], [-5]]);;
gap> g := Bipartition([[1, -2], [2], [3, -5], [4], [5], [-1],
> [-3], [-4]]);;
gap> S := InverseSemigroup(f2, g);;
gap> NaturalLeqInverseSemigroup(S)(f, f);
true
gap> NaturalLeqInverseSemigroup(S)(f2, f2);
true
gap> NaturalLeqInverseSemigroup(S)(g, g);
true
gap> NaturalLeqInverseSemigroup(S)(g, f2);
false
gap> NaturalLeqInverseSemigroup(S)(f2, g);
true

# SmallerDegreeTest1: VagnerPresetonRepresentation: a basic example
gap> f1 := PartialPerm([1, 2, 4, 3]);;
gap> f2 := PartialPerm([1]);;
gap> f3 := PartialPerm([0, 2]);;
gap> f := InverseSemigroup(f1, f2, f3);
<inverse partial perm semigroup of rank 4 with 3 generators>
gap> NrMovedPoints(f);
2
gap> Size(f);
5
gap> VPR := VagnerPrestonRepresentation(f);
<inverse partial perm semigroup of size 5, rank 4 with 3 generators> -> 
<inverse partial perm semigroup of rank 5 with 3 generators>
gap> inv := InverseGeneralMapping(VPR);
<inverse partial perm semigroup of rank 5 with 3 generators> -> 
<inverse partial perm semigroup of size 5, rank 4 with 3 generators>
gap> ForAll(f, x -> (x ^ VPR) ^ inv = x);
true

# SmallerDegreeTest2: VagnerPrestonRepresentation
# for SymmetricInverseSemigroup(5)
gap> I5 := SymmetricInverseSemigroup(5);;
gap> NrMovedPoints(I5);
5
gap> Size(I5);
1546
gap> I5 := Range(VagnerPrestonRepresentation(I5));;
gap> NrMovedPoints(I5);
1545
gap> Size(I5);
1546
gap> I5 := SmallerDegreePartialPermRepresentation(I5);;
gap> NrMovedPoints(Image(I5));
5
gap> Size(Image(I5));
1546

# SmallerDegreeTest3: VagnerPrestonRepresentation
# for a bipartition semigroup
gap> B := Semigroup([
>  Bipartition([[1, -4], [2, -2], [3], [4], [5, -5], [6],
>    [7], [-1], [-3], [-6], [-7]]),
>  Bipartition([[1, -5], [2, -6], [3, -7], [4, -3], [5],
>    [6, -2], [7], [-1], [-4]]),
>  Bipartition([[1, -4], [2, -7], [3], [4, -5], [5, -2],
>    [6], [7, -1], [-3], [-6]]),
>  Bipartition([[1], [2, -2], [3], [4, -1], [5, -5], [6],
>    [7], [-3], [-4], [-6], [-7]]),
>  Bipartition([[1], [2, -6], [3, -4], [4], [5, -1],
>    [6, -2], [7, -3], [-5], [-7]]),
>  Bipartition([[1, -7], [2, -5], [3], [4, -1], [5, -4],
>    [6], [7, -2], [-3], [-6]])]);;
gap> IsInverseSemigroup(B);
true
gap> V := Range(VagnerPrestonRepresentation(B));
<inverse partial perm semigroup of rank 664 with 6 generators>

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/attrinv.tst");
