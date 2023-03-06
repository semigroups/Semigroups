#############################################################################
##
#W  extreme/semibipart.tst
#Y  Copyright (C) 2014-15                                 Attila Egri-Nagy
##                                                       James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local D, DD, G, H, HH, L, LL, R, S, T, acting, an, f, g, gens, inv, iso, s, x
gap> START_TEST("Semigroups package: extreme/semibipart.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();
gap> SEMIGROUPS.DefaultOptionsRec.acting := true;;

# BipartitionTest1: IsomorphismTransformationMonoid, IsomorphismTransformationSemigroup
gap> S := DualSymmetricInverseMonoid(4);
<inverse block bijection monoid of degree 4 with 3 generators>
gap> IsomorphismTransformationMonoid(S);
<inverse block bijection monoid of size 339, degree 4 with 3 generators> -> 
<transformation monoid of size 339, degree 339 with 3 generators>
gap> S := Semigroup(Bipartition([[1, 2, 3, 4, -2, -3], [-1], [-4]]),
>  Bipartition([[1, 2, -1, -3], [3, 4, -2, -4]]),
>  Bipartition([[1, 3, -1], [2, 4, -2, -3], [-4]]),
>  Bipartition([[1, -4], [2], [3, -2], [4, -1], [-3]]));;
gap> IsomorphismTransformationSemigroup(S);
<bipartition semigroup of size 284, degree 4 with 4 generators> -> 
<transformation semigroup of size 284, degree 285 with 4 generators>
gap> S := Monoid(Bipartition([[1, 2, -2], [3], [4, -3, -4], [-1]]),
>  Bipartition([[1, 3, -3, -4], [2, 4, -1, -2]]),
>  Bipartition([[1, -1, -2], [2, 3, -3, -4], [4]]),
>  Bipartition([[1, 4, -4], [2, -1], [3, -2, -3]]));;
gap> IsomorphismTransformationMonoid(S);
<bipartition monoid of size 41, degree 4 with 4 generators> -> 
<transformation monoid of size 41, degree 41 with 4 generators>

# IsomorphismBipartitionSemigroup for a CanUseFroidurePin semigroup
gap> S := Semigroup(
> Bipartition([[1, 2, 3, -3], [4, -4, -5], [5, -1], [-2]]),
> Bipartition([[1, 4, -2, -3], [2, 3, 5, -5], [-1, -4]]),
> Bipartition([[1, 5], [2, 4, -3, -5], [3, -1, -2], [-4]]),
> Bipartition([[1], [2], [3, 5, -1, -2], [4, -3], [-4, -5]]),
> Bipartition([[1], [2], [3], [4, -1, -4], [5], [-2, -3],
>      [-5]]));;
gap> D := DClass(S, Bipartition([[1], [2], [3], [4, -1, -4],
> [5], [-2, -3], [-5]]));;
gap> IsRegularDClass(D);
true
gap> R := PrincipalFactor(D);
<Rees 0-matrix semigroup 12x15 over Group(())>
gap> f := IsomorphismSemigroup(IsBipartitionSemigroup, R);
<Rees 0-matrix semigroup 12x15 over Group(())> -> 
<bipartition semigroup of size 181, degree 182 with 26 generators>
gap> g := InverseGeneralMapping(f);;
gap> ForAll(R, x -> (x ^ f) ^ g = x);
true
gap> x := RMSElement(R, 12, (), 8);;
gap> ForAll(R, y -> (x ^ f) * (y ^ f) = (x * y) ^ f);
true

# BipartitionTest14: IsomorphismBipartitionSemigroup
# for a transformation semigroup
gap> gens := [Transformation([3, 4, 1, 2, 1]),
>   Transformation([4, 2, 1, 5, 5]),
>   Transformation([4, 2, 2, 2, 4])];;
gap> s := Semigroup(gens);;
gap> S := Range(IsomorphismSemigroup(IsBipartitionSemigroup, s));
<bipartition semigroup of degree 5 with 3 generators>
gap> f := IsomorphismSemigroup(IsBipartitionSemigroup, s);
<transformation semigroup of degree 5 with 3 generators> -> 
<bipartition semigroup of degree 5 with 3 generators>
gap> g := InverseGeneralMapping(f);;
gap> ForAll(s, x -> (x ^ f) ^ g = x);
true
gap> ForAll(S, x -> (x ^ g) ^ f = x);
true
gap> Size(s);
731
gap> Size(S);
731
gap> x := Transformation([3, 1, 3, 3, 3]);;
gap> ForAll(s, y -> (x ^ f) * (y ^ f) = (x * y) ^ f);
true

# BipartitionTest15: IsomorphismTransformationSemigroup for a bipartition
# semigroup consisting of IsTransBipartition
gap> S := Semigroup(Transformation([1, 3, 4, 1, 3]),
> Transformation([2, 4, 1, 5, 5]),
> Transformation([2, 5, 3, 5, 3]),
> Transformation([4, 1, 2, 2, 1]),
> Transformation([5, 5, 1, 1, 3]));;
gap> T := Range(IsomorphismSemigroup(IsBipartitionSemigroup, S));
<bipartition semigroup of degree 5 with 5 generators>
gap> f := IsomorphismTransformationSemigroup(T);
<bipartition semigroup of degree 5 with 5 generators> -> 
<transformation semigroup of degree 5 with 5 generators>
gap> g := InverseGeneralMapping(f);;
gap> ForAll(T, x -> (x ^ f) ^ g = x);
true
gap> ForAll(S, x -> (x ^ g) ^ f = x);
true
gap> Size(T);
602
gap> Size(S);
602
gap> Size(Range(f));
602

# BipartitionTest16: IsomorphismBipartitionSemigroup
# for a partial perm semigroup
gap> S := Semigroup(
> [PartialPerm([1, 2, 3], [1, 3, 4]),
>  PartialPerm([1, 2, 3], [2, 5, 3]),
>  PartialPerm([1, 2, 3], [4, 1, 2]),
>  PartialPerm([1, 2, 3, 4], [2, 4, 1, 5]),
>  PartialPerm([1, 3, 5], [5, 1, 3])]);;
gap> T := Range(IsomorphismSemigroup(IsBipartitionSemigroup, S));
<bipartition semigroup of degree 5 with 5 generators>
gap> Generators(S);
[ [2,3,4](1), [1,2,5](3), [3,2,1,4], [3,1,2,4,5], (1,5,3) ]
gap> Generators(T);
[ <bipartition: [ 1, -1 ], [ 2, -3 ], [ 3, -4 ], [ 4 ], [ 5 ], [ -2 ], [ -5 ]>
    , <bipartition: [ 1, -2 ], [ 2, -5 ], [ 3, -3 ], [ 4 ], [ 5 ], [ -1 ], 
     [ -4 ]>, <bipartition: [ 1, -4 ], [ 2, -1 ], [ 3, -2 ], [ 4 ], [ 5 ], 
     [ -3 ], [ -5 ]>, 
  <bipartition: [ 1, -2 ], [ 2, -4 ], [ 3, -1 ], [ 4, -5 ], [ 5 ], [ -3 ]>, 
  <bipartition: [ 1, -5 ], [ 2 ], [ 3, -1 ], [ 4 ], [ 5, -3 ], [ -2 ], [ -4 ]>
 ]
gap> Size(S);
156
gap> Size(T);
156
gap> IsInverseSemigroup(S);
false
gap> IsInverseSemigroup(T);
false
gap> f := IsomorphismSemigroup(IsBipartitionSemigroup, S);;
gap> g := InverseGeneralMapping(f);;
gap> ForAll(S, x -> (x ^ f) ^ g = x);
true
gap> ForAll(T, x -> (x ^ g) ^ f = x);
true
gap> Size(S);
156
gap> ForAll(S, x -> ForAll(S, y -> (x * y) ^ f = (x ^ f) * (y ^ f)));
true

# BipartitionTest17: IsomorphismPartialPermSemigroup
# for a semigroup of bipartitions consisting of IsPartialPermBipartition
gap> f := IsomorphismPartialPermSemigroup(T);;
gap> g := InverseGeneralMapping(f);;
gap> ForAll(T, x -> ForAll(T, y -> (x * y) ^ f = (x ^ f) * (y ^ f)));
true
gap> Size(S); Size(T);
156
156
gap> ForAll(T, x -> (x ^ f) ^ g = x);
true
gap> ForAll(S, x -> (x ^ g) ^ f = x);
true

# BipartitionTest18
# Testing the cases to which the new methods for 
# IsomorphismPartialPermSemigroup and IsomorphismTransformationSemigroup
# don't apply
gap> S := Semigroup(
> Bipartition([[1, 2, 3, 4, -1, -2, -5], [5], [-3, -4]]),
> Bipartition([[1, 2, 3], [4, -2, -4], [5, -1, -5], [-3]]),
> Bipartition([[1, 3, 5], [2, 4, -1, -2, -5], [-3], [-4]]),
> Bipartition([[1, -5], [2, 3, 4, 5], [-1], [-2], [-3, -4]]),
> Bipartition([[1, -4], [2], [3, -2], [4, 5, -1], [-3, -5]]));;
gap> IsomorphismPartialPermSemigroup(S);
Error, the argument must be an inverse semigroup
gap> Range(IsomorphismTransformationSemigroup(S));
<transformation semigroup of size 207, degree 208 with 5 generators>

# BipartitionTest19: IsomorphismBipartitionSemigroup for a perm group
gap> G := DihedralGroup(IsPermGroup, 10);;
gap> f := IsomorphismSemigroup(IsBipartitionSemigroup, G);;
gap> g := InverseGeneralMapping(f);;
gap> ForAll(G, x -> (x ^ f) ^ g = x);
true
gap> ForAll(G, x -> ForAll(G, y -> (x * y) ^ f = x ^ f * y ^ f));
true
gap> ForAll(Range(f), x -> (x ^ g) ^ f = x);
true

# BipartitionTest20: IsomorphismPermGroup
gap> G := GroupOfUnits(PartitionMonoid(5));
<block bijection group of degree 5 with 2 generators>
gap> IsomorphismPermGroup(G);;
gap> f := last;; g := InverseGeneralMapping(f);;
gap> ForAll(G, x -> ForAll(G, y -> (x * y) ^ f = x ^ f * y ^ f));
true
gap> ForAll(G, x -> (x ^ f) ^ g = x);
true
gap> ForAll(Range(f), x -> (x ^ g) ^ f = x);
true
gap> S := PartitionMonoid(5);;
gap> D := DClass(S,
> Bipartition([[1], [2, -3], [3, -4], [4, -5], [5], [-1], [-2]]));;
gap> G := GroupHClass(D);;
gap> G = HClass(S, Bipartition([[1], [2, -1, -2], [3, -3], 
> [4, -4, -5], [5]]));
true
gap> IsomorphismPermGroup(G);;

# BipartitionTest21: IsomorphismBipartitionSemigroup
# for an inverse semigroup of partial perms
gap> S := InverseSemigroup(
> PartialPerm([1, 3, 5, 7, 9], [7, 6, 5, 10, 1]),
> PartialPerm([1, 2, 3, 4, 6, 10], [9, 10, 4, 2, 5, 6]));;
gap> T := Range(IsomorphismSemigroup(IsBipartitionSemigroup, S));
<inverse bipartition semigroup of degree 10 with 2 generators>
gap> Size(S);
281
gap> Size(T);
281
gap> IsomorphismPartialPermSemigroup(T);
<inverse bipartition semigroup of size 281, degree 10 with 2 generators> -> 
<inverse partial perm semigroup of size 281, rank 9 with 2 generators>
gap> Size(Range(last));
281
gap> f := last2;; g := InverseGeneralMapping(f);;
gap> ForAll(T, x -> (x ^ f) ^ g = x);
true

# BipartitionTest22: AsBlockBijection and
# IsomorphismSemigroup(IsBlockBijectionSemigroup for an inverse semigroup of
# partial perms
gap> S := InverseSemigroup(
> PartialPerm([1, 2, 3, 6, 8, 10], [2, 6, 7, 9, 1, 5]),
> PartialPerm([1, 2, 3, 4, 6, 7, 8, 10], [3, 8, 1, 9, 4, 10, 5, 6]));;
gap> T := Range(IsomorphismSemigroup(IsBlockBijectionSemigroup, S));
<inverse block bijection semigroup of degree 11 with 2 generators>
gap> f := IsomorphismSemigroup(IsBlockBijectionSemigroup, S);;
gap> g := InverseGeneralMapping(f);;
gap> ForAll(S, x -> (x ^ f) ^ g = x);
true
gap> ForAll(T, x -> (x ^ g) ^ f = x);
true
gap> Size(S);
2657
gap> Size(T);
2657
gap> x := PartialPerm([1, 2, 3, 8], [8, 4, 10, 3]);;
gap> ForAll(S, y -> x ^ f * y ^ f = (x * y) ^ f);
true

# BipartitionTest23: Same as last for non-inverse partial perm semigroup
gap> S := Semigroup(
> PartialPerm([1, 2, 3, 6, 8, 10], [2, 6, 7, 9, 1, 5]),
> PartialPerm([1, 2, 3, 4, 6, 7, 8, 10], [3, 8, 1, 9, 4, 10, 5, 6]));;
gap> Size(S);
90
gap> IsInverseSemigroup(S);
false
gap> T := Range(IsomorphismSemigroup(IsBlockBijectionSemigroup, S));
<block bijection semigroup of size 90, degree 11 with 2 generators>
gap> Size(T);
90
gap> IsInverseSemigroup(T);
false
gap> f := IsomorphismSemigroup(IsBlockBijectionSemigroup, S);;
gap> g := InverseGeneralMapping(f);;
gap> ForAll(S, x -> (x ^ f) ^ g = x);
true
gap> ForAll(T, x -> (x ^ g) ^ f = x);
true
gap> x := PartialPerm([1, 3], [3, 1]);;
gap> ForAll(S, y -> x ^ f * y ^ f = (x * y) ^ f);
true

# BipartitionTest26:
# Tests of things in greens-generic.xml in the order they appear in that file. 
gap> S := Semigroup(
> Bipartition([[1, -1], [2, -2], [3, -3], [4, -4], [5, -8],
>      [6, -9], [7, -10], [8, -11], [9, -12], [10, -13], [11, -5],
>      [12, -6], [13, -7]]),
>  Bipartition([[1, -2], [2, -5], [3, -8], [4, -11], [5, -1],
>      [6, -4], [7, -3], [8, -7], [9, -10], [10, -13], [11, -6],
>      [12, -12], [13, -9]]),
>  Bipartition([[1, 7, -10, -12], [2, 3, 4, 6, 10, 13, -13],
>      [5, 12, -1], [8, 9, 11], [-2, -9], [-3, -7, -8], [-4],
>      [-5], [-6, -11]]), rec(acting := true));
<bipartition semigroup of degree 13 with 3 generators>
gap> f := Bipartition([[1, 2, 3, 4, 7, 8, 11, 13], [5, 9], [6, 10, 12],
> [-1, -2, -6], [-3], [-4, -8], [-5, -11], [-7, -10, -13], [-9],
>  [-12]]);;
gap> H := HClassNC(S, f);
<Green's H-class: <bipartition: [ 1, 2, 3, 4, 7, 8, 11, 13 ], [ 5, 9 ], 
  [ 6, 10, 12 ], [ -1, -2, -6 ], [ -3 ], [ -4, -8 ], [ -5, -11 ], 
  [ -7, -10, -13 ], [ -9 ], [ -12 ]>>
gap> IsGreensClassNC(H);
true
gap> MultiplicativeNeutralElement(H);
<bipartition: [ 1, 2, 3, 4, 7, 8, 11, 13 ], [ 5, 9 ], [ 6, 10, 12 ], 
 [ -1, -2, -6 ], [ -3 ], [ -4, -8 ], [ -5, -11 ], [ -7, -10, -13 ], [ -9 ], 
 [ -12 ]>
gap> StructureDescription(H);
"1"
gap> H := HClassNC(S, f);
<Green's H-class: <bipartition: [ 1, 2, 3, 4, 7, 8, 11, 13 ], [ 5, 9 ], 
  [ 6, 10, 12 ], [ -1, -2, -6 ], [ -3 ], [ -4, -8 ], [ -5, -11 ], 
  [ -7, -10, -13 ], [ -9 ], [ -12 ]>>
gap> f := Bipartition([[1, 2, 5, 6, 7, 8, 9, 10, 11, 12, -1, -10, -12, -13],
> [3, 4, 13], [-2, -9], [-3, -7, -8], [-4], [-5], [-6, -11]]);;
gap> HH := HClassNC(S, f);
<Green's H-class: 
 <bipartition: [ 1, 2, 5, 6, 7, 8, 9, 10, 11, 12, -1, -10, -12, -13 ], 
  [ 3, 4, 13 ], [ -2, -9 ], [ -3, -7, -8 ], [ -4 ], [ -5 ], [ -6, -11 ]>>
gap> HH < H;
false
gap> H < HH;
true
gap> H = HH;
false
gap> D := DClass(H);
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 7, 8, 11, 13 ], [ 5, 9 ], 
  [ 6, 10, 12 ], [ -1, -2, -6 ], [ -3 ], [ -4, -8 ], [ -5, -11 ], 
  [ -7, -10, -13 ], [ -9 ], [ -12 ]>>
gap> DD := DClass(HH);
<Green's D-class: 
 <bipartition: [ 1, 2, 5, 6, 7, 8, 9, 10, 11, 12, -1, -10, -12, -13 ], 
  [ 3, 4, 13 ], [ -2, -9 ], [ -3, -7, -8 ], [ -4 ], [ -5 ], [ -6, -11 ]>>
gap> DD < D;
true
gap> D < DD;
false
gap> D = DD;
false
gap> S := Semigroup(
> [Bipartition([[1, 2, 3, 4, 5], [-1, -2, -4, -5], [-3]]),
>  Bipartition([[1, 2, 3, 4, -2, -3, -4], [5], [-1, -5]]),
>  Bipartition([[1, 2, 3, -3, -5], [4, -1], [5, -2, -4]]),
>  Bipartition([[1, 5, -1, -3], [2, 3], [4, -2], [-4, -5]]),
>  Bipartition([[1, 4, -3], [2], [3], [5, -1, -2, -5], [-4]])]);;
gap> IsGreensLessThanOrEqual(DClass(S, S.1), DClass(S, S.2));
true
gap> IsGreensLessThanOrEqual(DClass(S, S.2), DClass(S, S.1));
false
gap> f := S.1 * S.2 * S.3;
<bipartition: [ 1, 2, 3, 4, 5 ], [ -1, -2, -3, -4, -5 ]>
gap> f := S.1 * S.2;
<bipartition: [ 1, 2, 3, 4, 5 ], [ -1, -5 ], [ -2, -3, -4 ]>
gap> H := HClass(S, f);
<Green's H-class: <bipartition: [ 1, 2, 3, 4, 5 ], [ -1, -5 ], [ -2, -3, -4 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5], [-1, -5], [-2, -3, -4]]) in last;
true
gap> LClass(H);
<Green's L-class: <bipartition: [ 1, 2, 3, 4, 5 ], [ -1, -5 ], [ -2, -3, -4 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5], [-1, -5], [-2, -3, -4]]) in last;
true
gap> RClass(H);
<Green's R-class: <bipartition: [ 1, 2, 3, 4, 5 ], [ -1, -5 ], [ -2, -3, -4 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5], [-1, -2, -4, -5], [-3]]) in last;
true
gap> DClass(RClass(H));
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5 ], [ -1, -2, -4, -5 ], [ -3 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5], [-1, -2, -4, -5], [-3]]) in last;
true
gap> DClass(LClass(H));
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5 ], [ -1, -5 ], [ -2, -3, -4 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5], [-1, -2, -4, -5], [-3]]) in last;
true
gap> DClass(H);
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5 ], [ -1, -5 ], [ -2, -3, -4 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5], [-1, -2, -4, -5], [-3]]) in last;
true
gap> f := Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]);
<bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
gap> H := HClassNC(S, f);
<Green's H-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> LClass(H);
<Green's L-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> RClass(H);
<Green's R-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> DClass(RClass(H));
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> DClass(LClass(H));
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> DClass(H);
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> DClasses(S);
[ <Green's D-class: <bipartition: [ 1, 2, 3, 4, 5 ], [ -1, -2, -4, -5 ], 
      [ -3 ]>>, 
  <Green's D-class: <bipartition: [ 1, 2, 3, 4, -2, -3, -4 ], [ 5 ], 
      [ -1, -5 ]>>, 
  <Green's D-class: <block bijection: [ 1, 2, 3, -3, -5 ], [ 4, -1 ], 
      [ 5, -2, -4 ]>>, 
  <Green's D-class: <bipartition: [ 1, 5, -1, -3 ], [ 2, 3 ], [ 4, -2 ], 
      [ -4, -5 ]>>, 
  <Green's D-class: <bipartition: [ 1, 4, -3 ], [ 2 ], [ 3 ], 
      [ 5, -1, -2, -5 ], [ -4 ]>>, 
  <Green's D-class: <bipartition: [ 1, 2, 3, -1, -2, -5 ], [ 4, 5, -3 ], 
      [ -4 ]>> ]
gap> H := HClassNC(S, f);
<Green's H-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> RClasses(DClass(H));
[ <Green's R-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], 
      [ -4, -5 ]>>, 
  <Green's R-class: <bipartition: [ 1, 2, 3, 4, -2 ], [ 5 ], [ -1, -3 ], 
      [ -4, -5 ]>>, 
  <Green's R-class: <bipartition: [ 1, 4, 5, -2 ], [ 2, 3 ], [ -1, -3 ], 
      [ -4, -5 ]>>, 
  <Green's R-class: <bipartition: [ 1, 4, 5, -2 ], [ 2 ], [ 3 ], [ -1, -3 ], 
      [ -4, -5 ]>>, 
  <Green's R-class: <bipartition: [ 1, 5, -2 ], [ 2, 3 ], [ 4 ], [ -1, -3 ], 
      [ -4, -5 ]>>, 
  <Green's R-class: <bipartition: [ 1, 4 ], [ 2 ], [ 3 ], [ 5, -2 ], 
      [ -1, -3 ], [ -4, -5 ]>>, 
  <Green's R-class: <bipartition: [ 1, 2, 3, -2 ], [ 4, 5 ], [ -1, -3 ], 
      [ -4, -5 ]>> ]
gap> LClasses(DClass(H));
[ <Green's L-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], 
      [ -4, -5 ]>>, 
  <Green's L-class: <bipartition: [ 1, 2, 3, 4, 5, -2, -3, -4 ], [ -1, -5 ]>>,
  <Green's L-class: <block bijection: [ 1, 2, 3, 4, 5, -1, -2, -3, -4, -5 ]>>,
  <Green's L-class: <bipartition: [ 1, 2, 3, 4, 5, -1, -2, -3 ], [ -4, -5 ]>>,
  <Green's L-class: <bipartition: [ 1, 2, 3, 4, 5, -3, -5 ], [ -1, -2, -4 ]>>,
  <Green's L-class: <bipartition: [ 1, 2, 3, 4, 5, -1, -2, -5 ], [ -3 ], 
      [ -4 ]>>, 
  <Green's L-class: <bipartition: [ 1, 2, 3, 4, 5, -1, -2, -3, -5 ], [ -4 ]>>,
  <Green's L-class: <bipartition: [ 1, 2, 3, 4, 5, -2, -3, -4, -5 ], [ -1 ]>>,
  <Green's L-class: <bipartition: [ 1, 2, 3, 4, 5, -1, -3 ], [ -2 ], 
      [ -4, -5 ]>> ]
gap> HClasses(LClass(H));
[ <Green's H-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], 
      [ -4, -5 ]>>, 
  <Green's H-class: <bipartition: [ 1, 2, 3, 4, -2 ], [ 5 ], [ -1, -3 ], 
      [ -4, -5 ]>>, 
  <Green's H-class: <bipartition: [ 1, 4, 5, -2 ], [ 2, 3 ], [ -1, -3 ], 
      [ -4, -5 ]>>, 
  <Green's H-class: <bipartition: [ 1, 4, 5, -2 ], [ 2 ], [ 3 ], [ -1, -3 ], 
      [ -4, -5 ]>>, 
  <Green's H-class: <bipartition: [ 1, 5, -2 ], [ 2, 3 ], [ 4 ], [ -1, -3 ], 
      [ -4, -5 ]>>, 
  <Green's H-class: <bipartition: [ 1, 4 ], [ 2 ], [ 3 ], [ 5, -2 ], 
      [ -1, -3 ], [ -4, -5 ]>>, 
  <Green's H-class: <bipartition: [ 1, 2, 3, -2 ], [ 4, 5 ], [ -1, -3 ], 
      [ -4, -5 ]>> ]
gap> HClasses(RClass(H));
[ <Green's H-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], 
      [ -4, -5 ]>>, 
  <Green's H-class: <bipartition: [ 1, 2, 3, 4, 5, -2, -3, -4 ], [ -1, -5 ]>>,
  <Green's H-class: <block bijection: [ 1, 2, 3, 4, 5, -1, -2, -3, -4, -5 ]>>,
  <Green's H-class: <bipartition: [ 1, 2, 3, 4, 5, -1, -2, -3 ], [ -4, -5 ]>>,
  <Green's H-class: <bipartition: [ 1, 2, 3, 4, 5, -3, -5 ], [ -1, -2, -4 ]>>,
  <Green's H-class: <bipartition: [ 1, 2, 3, 4, 5, -1, -2, -5 ], [ -3 ], 
      [ -4 ]>>, 
  <Green's H-class: <bipartition: [ 1, 2, 3, 4, 5, -1, -2, -3, -5 ], [ -4 ]>>,
  <Green's H-class: <bipartition: [ 1, 2, 3, 4, 5, -2, -3, -4, -5 ], [ -1 ]>>,
  <Green's H-class: <bipartition: [ 1, 2, 3, 4, 5, -1, -3 ], [ -2 ], 
      [ -4, -5 ]>> ]
gap> JClasses(S);
[ <Green's D-class: <bipartition: [ 1, 2, 3, 4, 5 ], [ -1, -2, -4, -5 ], 
      [ -3 ]>>, 
  <Green's D-class: <bipartition: [ 1, 2, 3, 4, -2, -3, -4 ], [ 5 ], 
      [ -1, -5 ]>>, 
  <Green's D-class: <block bijection: [ 1, 2, 3, -3, -5 ], [ 4, -1 ], 
      [ 5, -2, -4 ]>>, 
  <Green's D-class: <bipartition: [ 1, 5, -1, -3 ], [ 2, 3 ], [ 4, -2 ], 
      [ -4, -5 ]>>, 
  <Green's D-class: <bipartition: [ 1, 4, -3 ], [ 2 ], [ 3 ], 
      [ 5, -1, -2, -5 ], [ -4 ]>>, 
  <Green's D-class: <bipartition: [ 1, 2, 3, -1, -2, -5 ], [ 4, 5, -3 ], 
      [ -4 ]>> ]
gap> S := Semigroup(S);
<bipartition semigroup of degree 5 with 5 generators>
gap> D := DClassNC(S, f);
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> D := [D];
[ <Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], 
      [ -4, -5 ]>> ]
gap> D[2] := DClass(S, f);
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2, -3, -4], [-1, -5]]) in last; 
true
gap> D[3] := DClass(RClass(S, f));
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2, -3, -4 ], [ -1, -5 ]>>
gap> Bipartition([[1, 2, 3, 4, 5, -2, -3, -4], [-1, -5]]) in last; 
true
gap> D[4] := DClass(RClass(S, f));
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2, -3, -4 ], [ -1, -5 ]>>
gap> Bipartition([[1, 2, 3, 4, 5, -2, -3, -4], [-1, -5]]) in last; 
true
gap> D[5] := DClass(LClass(S, f));
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2, -3, -4], [-1, -5]]) in last; 
true
gap> D[6] := DClass(HClass(S, f));
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2, -3, -4], [-1, -5]]) in last; 
true
gap> D[7] := DClass(LClass(HClass(S, f)));
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2, -3, -4], [-1, -5]]) in last; 
true
gap> D[8] := DClass(RClass(HClass(S, f)));
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2, -3, -4 ], [ -1, -5 ]>>
gap> Bipartition([[1, 2, 3, 4, 5, -2, -3, -4], [-1, -5]]) in last; 
true
gap> ForAll(Combinations([1 .. 8], 2), x -> D[x[1]] = D[x[2]]);
true
gap> List(D, IsGreensClassNC);
[ true, false, false, false, false, false, false, false ]
gap> D[7] := DClass(LClass(HClassNC(S, f)));
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> D[6] := DClass(RClass(HClassNC(S, f)));
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> D[5] := DClass(HClassNC(S, f));
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> D[4] := DClass(LClassNC(S, f));
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> ForAll(Combinations([1 .. 8], 2), x -> D[x[1]] = D[x[2]]);
true
gap> List(D, IsGreensClassNC);
[ true, false, false, true, true, true, true, false ]
gap> S := Semigroup(S);
<bipartition semigroup of degree 5 with 5 generators>
gap> D := DClassNC(S, f);
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> L := LClassNC(D, f);
<Green's L-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> Size(L);
7
gap> Size(LClass(S, f));
7
gap> LClass(S, f) = LClassNC(D, f);
true
gap> LClass(D, f) = LClassNC(S, f);
true
gap> LClassNC(D, f) = LClassNC(S, f);
true
gap> LClassNC(D, f) = LClass(S, f);
true
gap> S := Semigroup(S);
<bipartition semigroup of degree 5 with 5 generators>
gap> D := DClass(S, f);
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2, -3, -4], [-1, -5]]) in last; 
true
gap> L := LClassNC(D, f);
<Green's L-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> Size(L);
7
gap> Size(LClass(S, f));
7
gap> LClass(S, f) = LClassNC(D, f);
true
gap> LClass(D, f) = LClassNC(S, f);
true
gap> LClassNC(D, f) = LClassNC(S, f);
true
gap> LClassNC(D, f) = LClass(S, f);
true
gap> S := Semigroup(S);
<bipartition semigroup of degree 5 with 5 generators>
gap> D := DClass(S, f);
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2, -3, -4], [-1, -5]]) in last; 
true
gap> R := RClassNC(D, f);
<Green's R-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2, -3, -4], [-1, -5]]) in last; 
true
gap> Size(R);
9
gap> Size(RClass(S, f));
9
gap> RClass(S, f) = RClassNC(D, f);
true
gap> RClass(D, f) = RClassNC(S, f);
true
gap> RClassNC(D, f) = RClassNC(S, f);
true
gap> RClassNC(D, f) = RClass(S, f);
true
gap> S := Semigroup(S);
<bipartition semigroup of degree 5 with 5 generators>
gap> D := DClassNC(S, f);
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> R := RClassNC(D, f);
<Green's R-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> Size(R);
9
gap> Size(RClass(S, f));
9
gap> RClass(S, f) = RClassNC(D, f);
true
gap> RClass(D, f) = RClassNC(S, f);
true
gap> RClassNC(D, f) = RClassNC(S, f);
true
gap> RClassNC(D, f) = RClass(S, f);
true
gap> S := Semigroup(S);
<bipartition semigroup of degree 5 with 5 generators>
gap> D := DClass(S, f);
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2, -3, -4], [-1, -5]]) in last; 
true
gap> H := HClassNC(D, f);
<Green's H-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> Size(H);
1
gap> Size(HClass(S, f));
1
gap> HClass(S, f) = HClassNC(D, f);
true
gap> HClass(D, f) = HClassNC(S, f);
true
gap> HClassNC(D, f) = HClassNC(S, f);
true
gap> HClassNC(D, f) = HClass(S, f);
true
gap> S := Semigroup(S);
<bipartition semigroup of degree 5 with 5 generators>
gap> D := DClassNC(S, f);
<Green's D-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> H := HClassNC(D, f);
<Green's H-class: <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>
  >
gap> Bipartition([[1, 2, 3, 4, 5, -2], [-1, -3], [-4, -5]]) in last;
true
gap> Size(H);
1
gap> Size(HClass(S, f));
1
gap> HClass(S, f) = HClassNC(D, f);
true
gap> HClass(D, f) = HClassNC(S, f);
true
gap> HClassNC(D, f) = HClassNC(S, f);
true
gap> HClassNC(D, f) = HClass(S, f);
true
gap> S := Semigroup([
>  Bipartition([[1, 2, 3, 4, 5, -2, -4], [6, 7], [8, -1, -6],
>      [-3, -5, -7], [-8]]),
>  Bipartition([[1, 2, 3, 4, -1, -2], [5, 6, -5], [7, 8, -4, -6],
>      [-3, -7], [-8]]),
>  Bipartition([[1, 2, 3, 7, -7], [4, 5, 6, 8], [-1, -2],
>      [-3, -6, -8], [-4], [-5]]),
>  Bipartition([[1, 2, 4, 7, -1, -2, -4], [3, -7], [5, -5], [6, 8],
>      [-3], [-6, -8]]),
>  Bipartition([[1, 2, 8, -2], [3, 4, 5, -5], [6, 7, -4], [-1, -7],
>      [-3, -6, -8]]),
>  Bipartition([[1, 2, 5, 6, 7, -4], [3, 8, -5], [4],
>      [-1, -2, -3, -6], [-7], [-8]]),
>  Bipartition([[1, 3, 4, 5, 6, 8, -1, -5], [2, -4], [7, -3, -8],
>      [-2, -6, -7]]),
>  Bipartition([[1, 3, 4, 5, -1, -7], [2, -6], [6], [7, -3],
>      [8, -4], [-2, -5, -8]]),
>  Bipartition([[1, 3, 4, 6, 7, -1, -7, -8], [2, 5, 8, -6], [-2, -4],
>      [-3, -5]]),
>  Bipartition([[1, 3, 4, -8], [2, 6, 8, -1], [5, 7, -2, -3, -4, -7],
>      [-5], [-6]]),
>  Bipartition([[1, 4, 8, -4, -6, -8], [2, 3, 6, -3, -5], [5, -1, -7],
>      [7], [-2]]),
>  Bipartition([[1, 5, -1, -2], [2, 3, 4, 6, 7], [8, -4], [-3, -5],
>      [-6], [-7], [-8]]),
>  Bipartition([[1, -6], [2, 3, 4, -2, -8], [5, 6, 7, -1, -3], [8],
>      [-4, -7], [-5]]),
>  Bipartition([[1, 7, 8, -1, -3, -4, -6], [2, 3, 4], [5, -2, -5],
>      [6], [-7, -8]]),
>  Bipartition([[1, 8, -3, -5, -6], [2, 3, 4, -1], [5, -2], [6, 7],
>      [-4, -7], [-8]]),
>  Bipartition([[1, 7, 8, -5], [2, 3, 5, -6], [4], [6, -1, -3],
>      [-2], [-4, -7, -8]]),
>  Bipartition([[1, 4, -1, -3, -4], [2, 7, 8, -2, -6], [3, 5, 6, -8],
>      [-5, -7]]),
>  Bipartition([[1, 5, 8], [2, 4, 7, -2], [3, 6], [-1, -3],
>      [-4, -5], [-6], [-7], [-8]]),
>  Bipartition([[1], [2, 4], [3, 6, -5], [5, 7, -3, -4, -6],
>      [8, -2], [-1, -7], [-8]]),
>  Bipartition([[1, 5, -8], [2, -4], [3, 6, 8, -1, -6],
>      [4, 7, -2, -3, -5], [-7]])]);;
gap> DClassReps(S);
[ <bipartition: [ 1, 2, 3, 4, 5, -2, -4 ], [ 6, 7 ], [ 8, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -2 ], [ 5, 6, -5 ], [ 7, 8, -4, -6 ], 
     [ -3, -7 ], [ -8 ]>, <bipartition: [ 1, 2, 3, 7, -7 ], [ 4, 5, 6, 8 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 4, 7, -1, -2, -4 ], [ 3, -7 ], [ 5, -5 ], [ 6, 8 ], 
     [ -3 ], [ -6, -8 ]>, <bipartition: [ 1, 2, 8, -2 ], [ 3, 4, 5, -5 ], 
     [ 6, 7, -4 ], [ -1, -7 ], [ -3, -6, -8 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -4 ], [ 3, 8, -5 ], [ 4 ], [ -1, -2, -3, -6 ]
      , [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -5 ], [ 2, -4 ], [ 7, -3, -8 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 3, 4, 5, -1, -7 ], [ 2, -6 ], [ 6 ], 
     [ 7, -3 ], [ 8, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, -8 ], [ 2, 6, 8, -1 ], [ 5, 7, -2, -3, -4, -7 ], 
     [ -5 ], [ -6 ]>, 
  <bipartition: [ 1, 4, 8, -4, -6, -8 ], [ 2, 3, 6, -3, -5 ], [ 5, -1, -7 ], 
     [ 7 ], [ -2 ]>, 
  <bipartition: [ 1, -6 ], [ 2, 3, 4, -2, -8 ], [ 5, 6, 7, -1, -3 ], [ 8 ], 
     [ -4, -7 ], [ -5 ]>, 
  <bipartition: [ 1, 7, 8, -1, -3, -4, -6 ], [ 2, 3, 4 ], [ 5, -2, -5 ], 
     [ 6 ], [ -7, -8 ]>, <bipartition: [ 1, 8, -3, -5, -6 ], [ 2, 3, 4, -1 ], 
     [ 5, -2 ], [ 6, 7 ], [ -4, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 7, 8, -5 ], [ 2, 3, 5, -6 ], [ 4 ], [ 6, -1, -3 ], 
     [ -2 ], [ -4, -7, -8 ]>, 
  <bipartition: [ 1, 4, -1, -3, -4 ], [ 2, 7, 8, -2, -6 ], [ 3, 5, 6, -8 ], 
     [ -5, -7 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -5 ], [ 5, 7, -3, -4, -6 ], 
     [ 8, -2 ], [ -1, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5, -8 ], [ 2, -4 ], [ 3, 6, 8, -1, -6 ], 
     [ 4, 7, -2, -3, -5 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -2, -4 ], [ 3, -1, -6 ], [ 6, 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, -1, -6 ], [ 2, 5, 6, 7, 8, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 4, 7, 8, -2, -4 ], [ 3, 5, 6, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 4, 7, -1, -2, -4, -7 ], [ 3, 5, -5 ], [ 6, 8 ], 
     [ -3 ], [ -6, -8 ]>, <bipartition: [ 1, 2, 5, 6, 7, -1, -2, -4, -7 ], 
     [ 3, 8, -5 ], [ 4 ], [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8, -1, -6, -7 ], [ 6 ], [ 7, -3 ], 
     [ -2, -5, -8 ], [ -4 ]>, 
  <bipartition: [ 1, 4, 5, 8, -5 ], [ 2, 3, 6, -1, -2, -4, -7 ], [ 7 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4 ], [ 2 ], [ 7, -5 ], [ -1, -2, -3, -6 ]
      , [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -1, -4, -5 ], [ 3, -3, -8 ], [ 6, 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 8, -4 ], [ 3, 4, 5, 6, 7, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4, -4 ], [ 2, 5, 6, 7, 8, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 4, -1, -3, -7 ], [ 2, 7, 8, -6 ], 
     [ 3, 5, 6, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 5, -4 ], [ 2, 3, 4, 6, 7, 8, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 5, 6, 7, -5 ], [ 2, 3, 4, -1, -2, -4, -7 ], [ 8 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 4, -8 ], [ 2, 3, 5, 6, 7, 8, -1 ], [ -2, -3, -4, -7 ], 
     [ -5 ], [ -6 ]>, <bipartition: [ 1, 2, 3, 4, 7, 8, -3, -4, -5, -6, -8 ], 
     [ 5, 6, -1, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 4, 7, -3, -4, -5, -6, -8 ], [ 3 ], [ 5, -1, -7 ], 
     [ 6, 8 ], [ -2 ]>, <bipartition: [ 1, 2, 5, 6, 7, -3, -4, -5, -6, -8 ], 
     [ 3, 8, -1, -7 ], [ 4 ], [ -2 ]>, 
  <bipartition: [ 1, 5, 8, -1, -3, -5, -7 ], [ 2, 3, 4, -4, -6, -8 ], 
     [ 6, 7 ], [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -2, -4, -7 ], 
     [ 2, 7, -5 ], [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, 8, -1, -3, -4, -6 ], [ 5, 6, -2, -5 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 2, 5, 6, 7, -1, -3, -4, -6 ], 
     [ 3, 8, -2, -5 ], [ 4 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 7, 8, -2, -5 ], [ 2, 3, 5 ], [ 4 ], [ 6, -1, -3, -4, -6 ]
      , [ -7, -8 ]>, <bipartition: [ 1, 3, 4, 5, 8, -1, -2, -4, -7 ], 
     [ 2, 7, -5 ], [ 6 ], [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 5, 8 ], [ 2, 3, 4, 6, 7 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, -4, -8 ], [ 7, 8, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, <bipartition: [ 1, 4, -1, -6 ], [ 2, 3, 5, 6, 7, 8, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -6 ], [ 2, 7, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -2, -4, -7 ], [ 2 ], [ 7, -5 ], 
     [ -3 ], [ -6, -8 ]>, <bipartition: [ 1, 2, 4, 5, 7, -4 ], [ 3, -5 ], 
     [ 6, 8 ], [ -1, -2, -3, -6 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -1, -3, -5, -8 ], [ 3, 8, -4 ], [ 4 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 4, -4 ], [ 2, 3, 5, 6, 7, 8, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 3, 4, 6, 7, -1, -3, -6, -7 ], 
     [ 2, 5, 8, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4 ], [ 2, 7, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 4, 5, 8, -3, -4, -5, -6, -8 ], [ 2, 3, 6, -1, -7 ], 
     [ 7 ], [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -3, -4, -5, -6, -8 ], 
     [ 2 ], [ 7, -1, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -1, -7 ], [ 3, 8, -3, -4, -5, -6, -8 ], 
     [ 4 ], [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 8, -3, -4, -5, -6, -8 ], 
     [ 2, 7, -1, -7 ], [ 6 ], [ -2 ]>, 
  <bipartition: [ 1, 5, 6, 7, -1, -7 ], [ 2, 3, 4, -3, -4, -5, -6, -8 ], 
     [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 4, -5 ], [ 2, 3, 5, 6, 7, 8, -1, -2, -4, -7 ], [ -3 ], 
     [ -6, -8 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -3, -4, -6 ], [ 2 ], 
     [ 7, -2, -5 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 4, -5 ], [ 2, 7, 8, -1, -2, -4, -7 ], [ 3, 5, 6 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 5, 6, 7, -2, -4 ], [ 2, 3, 4, -1, -6 ], [ 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 4, -2, -4 ], [ 2, 3, 5, 6, 7, 8, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -1, -2, -4, -7 ], [ 3, -5 ], [ 6, 8 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4 ], [ 2, 7, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -3, -5, -8 ], 
     [ 2 ], [ 7, -4 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -4 ], [ 3, 8, -1, -3, -5, -8 ], [ 4 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 3, 4, 5, 8, -1, -3, -5, -8 ], 
     [ 2, 7, -4 ], [ 6 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 5, 6, 7, -4 ], [ 2, 3, 4, -1, -3, -5, -8 ], [ 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 5, 6, 7, -1, -3, -6, -7 ], [ 2, 3, 4, -4 ], [ 8 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 4, -1, -6, -7 ], [ 2, 3, 5, 6, 7, 8, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -3, -4, -5, -6, -8 ], [ 2, 7, -1, -7 ], 
     [ -2 ]>, <bipartition: [ 1, 2, 4, 5, 7, -3, -4, -5, -6, -8 ], 
     [ 3, -1, -7 ], [ 6, 8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -1, -3, -5, -7 ], [ 3, 8, -4, -6, -8 ], 
     [ 4 ], [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -7 ], [ 2 ], 
     [ 7, -3, -4, -5, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -1, -7 ], [ 2, 7, -3, -4, -5, -6, -8 ], 
     [ 6 ], [ -2 ]>, <bipartition: [ 1, 5, 6, 7, -3, -4, -5, -6, -8 ], 
     [ 2, 3, 4, -1, -7 ], [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -2, -5 ], [ 3, 8, -1, -3, -4, -6 ], [ 4 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 3, 4, 5, 8, -1, -3, -4, -6 ], 
     [ 2, 7, -2, -5 ], [ 6 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 5, 6, 7, -2, -5 ], [ 2, 3, 4, -1, -3, -4, -6 ], [ 8 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -3, -4, -6 ], 
     [ 2, 7, -2, -5 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -1, -3, -4, -6 ], [ 3, -2, -5 ], [ 6, 8 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 4, -2, -5 ], [ 2, 7, 8, -1, -3, -4, -6 ], [ 3, 5, 6 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 4, 5, 8, -2, -5 ], [ 2, 3, 6, -1, -3, -4, -6 ], [ 7 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -2, -3, -5, -6 ], 
     [ 2, 7, -4, -8 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -1, -6 ], [ 3, 8, -2, -4 ], [ 4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 4, 5, 8, -4 ], [ 2, 3, 6, -1, -3, -5, -8 ], [ 7 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 4, -1, -3, -5, -8 ], [ 2, 3, 5, 6, 7, 8, -4 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -3, -5, -8 ], 
     [ 2, 7, -4 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -1, -3, -5, -8 ], [ 3, -4 ], [ 6, 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4 ], [ 2 ], [ 7, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -4 ], [ 2, 7, -1, -3, -5, -8 ], [ 6 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 5, 6, 7, -1, -3, -5, -8 ], [ 2, 3, 4, -4 ], [ 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -4 ], [ 3, 8, -1, -3, -6, -7 ], [ 4 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 4, -1, -7 ], [ 2, 7, 8, -3, -4, -5, -6, -8 ], 
     [ 3, 5, 6 ], [ -2 ]>, 
  <bipartition: [ 1, 4, 5, 8, -1, -7 ], [ 2, 3, 6, -3, -4, -5, -6, -8 ], 
     [ 7 ], [ -2 ]>, 
  <bipartition: [ 1, 4, -1, -7 ], [ 2, 3, 5, 6, 7, 8, -3, -4, -5, -6, -8 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -7 ], [ 2, 7, -3, -4, -5, -6, -8 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 4, 5, 8, -1, -3, -5, -7 ], [ 2, 3, 6, -4, -6, -8 ], 
     [ 7 ], [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -3, -5, -7 ], 
     [ 2 ], [ 7, -4, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -1, -7 ], [ 3, -3, -4, -5, -6, -8 ], 
     [ 6, 8 ], [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -2, -5 ], [ 2 ], 
     [ 7, -1, -3, -4, -6 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -2, -5 ], [ 2, 7, -1, -3, -4, -6 ], [ 6 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 5, 6, 7, -1, -3, -4, -6 ], [ 2, 3, 4, -2, -5 ], [ 8 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 4, -2, -5 ], [ 2, 3, 5, 6, 7, 8, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -2, -5 ], [ 2, 7, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 4, -4, -8 ], [ 2, 3, 5, 6, 7, 8, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -6 ], [ 2 ], 
     [ 7, -2, -4 ], [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -2, -4 ], [ 3, 8, -1, -6 ], [ 4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -1, -6 ], [ 2, 7, -2, -4 ], [ 6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 4, 5, 8, -1, -6 ], [ 2, 3, 6, -2, -4 ], [ 7 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 4, -4 ], [ 2, 3, 5, 6, 7, 8, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 4, 5, 8, -1, -3, -5, -8 ], [ 2, 3, 6, -4 ], [ 7 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 4, -4 ], [ 2, 7, 8, -1, -3, -5, -8 ], [ 3, 5, 6 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -4 ], [ 3, -1, -3, -5, -8 ], [ 6, 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4 ], [ 2 ], [ 7, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 2, 5, 6, 7, -1, -3, -6, -7 ], 
     [ 3, 8, -4 ], [ 4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -4 ], [ 2, 7, -1, -3, -6, -7 ], [ 6 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 4, 5, 8, -4 ], [ 2, 3, 6, -1, -3, -6, -7 ], [ 7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 4, 5, 8, -4 ], [ 2, 3, 6, -1, -6, -7 ], [ 7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 4, -3, -4, -5, -6, -8 ], [ 2, 3, 5, 6, 7, 8, -1, -7 ], 
     [ -2 ]>, <bipartition: [ 1, 2, 4, 5, 7, -1, -3, -5, -7 ], 
     [ 3, -4, -6, -8 ], [ 6, 8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -2, -5 ], [ 3, -1, -3, -4, -6 ], [ 6, 8 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 4, -1, -3, -4, -6 ], [ 2, 3, 5, 6, 7, 8, -2, -5 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 4, 5, 8, -1, -3, -4, -6 ], [ 2, 3, 6, -2, -5 ], [ 7 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 2, 4, 5, 7, -1, -6 ], [ 3, -2, -4 ], 
     [ 6, 8 ], [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -2, -4 ], [ 2 ], [ 7, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -2, -4 ], [ 2, 7, -1, -6 ], [ 6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5, 6, 7, -1, -6 ], [ 2, 3, 4, -2, -4 ], [ 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 4, 5, 8, -2, -4 ], [ 2, 3, 6, -1, -6 ], [ 7 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -4 ], [ 3, -1, -3, -6, -7 ], [ 6, 8 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -3, -6, -7 ], 
     [ 2 ], [ 7, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -1, -3, -6, -7 ], [ 2, 7, -4 ], [ 6 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 5, 6, 7, -4 ], [ 2, 3, 4, -1, -3, -6, -7 ], [ 8 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 4, 5, 8, -1, -3, -6, -7 ], [ 2, 3, 6, -4 ], [ 7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -4 ], [ 3, 8, -1, -6, -7 ], [ 4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -4, -6, -8 ], [ 3, 8, -1, -3, -5, -7 ], 
     [ 4 ], [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 8, -1, -3, -5, -7 ], 
     [ 2, 7, -4, -6, -8 ], [ 6 ], [ -2 ]>, 
  <bipartition: [ 1, 5, 6, 7, -4, -6, -8 ], [ 2, 3, 4, -1, -3, -5, -7 ], 
     [ 8 ], [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -3, -5, -7 ], 
     [ 2, 7, -4, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 4, 5, 8, -1, -2, -3, -5, -6 ], [ 2, 3, 6, -4, -8 ], 
     [ 7 ], [ -7 ]>, <bipartition: [ 1, 2, 4, 5, 7, -1, -3, -6, -7 ], 
     [ 3, -4 ], [ 6, 8 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4 ], [ 2, 7, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4 ], [ 2 ], [ 7, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -1, -6, -7 ], [ 3, 8, -4 ], [ 4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -4 ], [ 2, 7, -1, -6, -7 ], [ 6 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 5, 6, 7, -1, -6, -7 ], [ 2, 3, 4, -4 ], [ 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -4, -6, -8 ], 
     [ 2 ], [ 7, -1, -3, -5, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -4, -6, -8 ], [ 2, 7, -1, -3, -5, -7 ], 
     [ 6 ], [ -2 ]>, 
  <bipartition: [ 1, 5, 6, 7, -1, -3, -5, -7 ], [ 2, 3, 4, -4, -6, -8 ], 
     [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 4, -4, -6, -8 ], [ 2, 3, 5, 6, 7, 8, -1, -3, -5, -7 ], 
     [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -4, -6, -8 ], 
     [ 2, 7, -1, -3, -5, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 4, 5, 8, -4, -6, -8 ], [ 2, 3, 6, -1, -3, -5, -7 ], 
     [ 7 ], [ -2 ]>, 
  <bipartition: [ 1, 4, -1, -3, -4, -6 ], [ 2, 7, 8, -2, -5 ], [ 3, 5, 6 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 2, 5, 6, 7, -1, -2, -3, -5, -6 ], 
     [ 3, 8, -4, -8 ], [ 4 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -2, -4 ], [ 2, 7, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 4, -2, -4 ], [ 2, 7, 8, -1, -6 ], [ 3, 5, 6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 4, -1, -3, -5, -8 ], [ 2, 7, 8, -4 ], [ 3, 5, 6 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 4, -1, -3, -6, -7 ], [ 2, 3, 5, 6, 7, 8, -4 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -3, -6, -7 ], 
     [ 2, 7, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 4, -1, -3, -6, -7 ], [ 2, 7, 8, -4 ], [ 3, 5, 6 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 2, 4, 5, 7, -4 ], [ 3, -1, -6, -7 ], 
     [ 6, 8 ], [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -6, -7 ], [ 2 ], [ 7, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -1, -6, -7 ], [ 2, 7, -4 ], [ 6 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 5, 6, 7, -4 ], [ 2, 3, 4, -1, -6, -7 ], [ 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -6, -7 ], 
     [ 2, 7, -4 ], [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 4, 5, 8, -1, -6, -7 ], [ 2, 3, 6, -4 ], [ 7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 4, -3, -4, -5, -6, -8 ], [ 2, 7, 8, -1, -7 ], 
     [ 3, 5, 6 ], [ -2 ]>, 
  <bipartition: [ 1, 4, -4, -6, -8 ], [ 2, 7, 8, -1, -3, -5, -7 ], 
     [ 3, 5, 6 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -4, -6, -8 ], [ 3, -1, -3, -5, -7 ], 
     [ 6, 8 ], [ -2 ]>, 
  <bipartition: [ 1, 4, -1, -3, -5, -7 ], [ 2, 3, 5, 6, 7, 8, -4, -6, -8 ], 
     [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -2, -3, -5, -6 ], [ 2 ], 
     [ 7, -4, -8 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -4, -8 ], [ 3, 8, -1, -2, -3, -5, -6 ], 
     [ 4 ], [ -7 ]>, <bipartition: [ 1, 3, 4, 5, 8, -1, -2, -3, -5, -6 ], 
     [ 2, 7, -4, -8 ], [ 6 ], [ -7 ]>, 
  <bipartition: [ 1, 5, 6, 7, -4, -8 ], [ 2, 3, 4, -1, -2, -3, -5, -6 ], 
     [ 8 ], [ -7 ]>, <bipartition: [ 1, 4, -1, -6, -7 ], [ 2, 7, 8, -4 ], 
     [ 3, 5, 6 ], [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -1, -6, -7 ], [ 3, -4 ], [ 6, 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 4, -4 ], [ 2, 3, 5, 6, 7, 8, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -1, -2, -3, -5, -6 ], [ 3, -4, -8 ], 
     [ 6, 8 ], [ -7 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -4, -8 ], [ 2 ], 
     [ 7, -1, -2, -3, -5, -6 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -4, -8 ], [ 2, 7, -1, -2, -3, -5, -6 ], 
     [ 6 ], [ -7 ]>, <bipartition: [ 1, 5, 6, 7, -1, -2, -3, -5, -6 ], 
     [ 2, 3, 4, -4, -8 ], [ 8 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4, -8 ], [ 2, 7, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, 
  <bipartition: [ 1, 4, 5, 8, -4, -8 ], [ 2, 3, 6, -1, -2, -3, -5, -6 ], 
     [ 7 ], [ -7 ]>, <bipartition: [ 1, 4, -1, -6 ], [ 2, 7, 8, -2, -4 ], 
     [ 3, 5, 6 ], [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 4, -4 ], [ 2, 7, 8, -1, -3, -6, -7 ], [ 3, 5, 6 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 4, -1, -3, -5, -7 ], [ 2, 7, 8, -4, -6, -8 ], 
     [ 3, 5, 6 ], [ -2 ]>, 
  <bipartition: [ 1, 4, -4, -8 ], [ 2, 7, 8, -1, -2, -3, -5, -6 ], 
     [ 3, 5, 6 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -4, -8 ], [ 3, -1, -2, -3, -5, -6 ], 
     [ 6, 8 ], [ -7 ]>, 
  <bipartition: [ 1, 4, -1, -2, -3, -5, -6 ], [ 2, 3, 5, 6, 7, 8, -4, -8 ], 
     [ -7 ]>, <bipartition: [ 1, 4, -4 ], [ 2, 7, 8, -1, -6, -7 ], 
     [ 3, 5, 6 ], [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 4, -1, -2, -3, -5, -6 ], [ 2, 7, 8, -4, -8 ], 
     [ 3, 5, 6 ], [ -7 ]> ]
gap> RClassReps(S);
[ <bipartition: [ 1, 2, 3, 4, 5, -2, -4 ], [ 6, 7 ], [ 8, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -2 ], [ 5, 6, -5 ], [ 7, 8, -4, -6 ], 
     [ -3, -7 ], [ -8 ]>, <bipartition: [ 1, 2, 3, 7, -7 ], [ 4, 5, 6, 8 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 4, 7, -1, -2, -4 ], [ 3, -7 ], [ 5, -5 ], [ 6, 8 ], 
     [ -3 ], [ -6, -8 ]>, <bipartition: [ 1, 2, 8, -2 ], [ 3, 4, 5, -5 ], 
     [ 6, 7, -4 ], [ -1, -7 ], [ -3, -6, -8 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -4 ], [ 3, 8, -5 ], [ 4 ], [ -1, -2, -3, -6 ]
      , [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -5 ], [ 2, -4 ], [ 7, -3, -8 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 3, 4, 5, -1, -7 ], [ 2, -6 ], [ 6 ], 
     [ 7, -3 ], [ 8, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7, -5 ], [ 2, 5, 8, -1, -2, -4, -7 ], [ -3 ], 
     [ -6, -8 ]>, <bipartition: [ 1, 3, 4, -8 ], [ 2, 6, 8, -1 ], 
     [ 5, 7, -2, -3, -4, -7 ], [ -5 ], [ -6 ]>, 
  <bipartition: [ 1, 4, 8, -4, -6, -8 ], [ 2, 3, 6, -3, -5 ], [ 5, -1, -7 ], 
     [ 7 ], [ -2 ]>, 
  <bipartition: [ 1, 5, -1, -2, -4, -7 ], [ 2, 3, 4, 6, 7 ], [ 8, -5 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, -6 ], [ 2, 3, 4, -2, -8 ], [ 5, 6, 7, -1, -3 ], [ 8 ], 
     [ -4, -7 ], [ -5 ]>, 
  <bipartition: [ 1, 7, 8, -1, -3, -4, -6 ], [ 2, 3, 4 ], [ 5, -2, -5 ], 
     [ 6 ], [ -7, -8 ]>, <bipartition: [ 1, 8, -3, -5, -6 ], [ 2, 3, 4, -1 ], 
     [ 5, -2 ], [ 6, 7 ], [ -4, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 7, 8, -5 ], [ 2, 3, 5, -6 ], [ 4 ], [ 6, -1, -3 ], 
     [ -2 ], [ -4, -7, -8 ]>, 
  <bipartition: [ 1, 4, -1, -3, -4 ], [ 2, 7, 8, -2, -6 ], [ 3, 5, 6, -8 ], 
     [ -5, -7 ]>, <bipartition: [ 1, 5, 8 ], [ 2, 4, 7, -7 ], [ 3, 6 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -5 ], [ 5, 7, -3, -4, -6 ], 
     [ 8, -2 ], [ -1, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5, -8 ], [ 2, -4 ], [ 3, 6, 8, -1, -6 ], 
     [ 4, 7, -2, -3, -5 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8, -7 ], [ 6, 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, 7, 8, -7 ], [ -1, -2 ], [ -3, -6, -8 ], 
     [ -4 ], [ -5 ]>, <bipartition: [ 1, 2, 4, 5, 7, -2, -4 ], [ 3, -1, -6 ], 
     [ 6, 8 ], [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 5, 6, 7, 8, -7 ], [ 4 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 7, 8, -7 ], [ 6 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, -1, -6 ], [ 2, 5, 6, 7, 8, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, 8, -7 ], [ 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 5, 8, -7 ], [ 2, 3, 4, 6, 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, 7, -7 ], [ 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 5, 7, 8, -7 ], [ 2, 3, 4 ], [ 6 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 4, 7, 8, -2, -4 ], [ 3, 5, 6, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, 8, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 5, -1, -6 ], [ 2, 3, 4, 6, 7, 8, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 4, 7, -1, -2, -4, -7 ], [ 3, 5, -5 ], [ 6, 8 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, -5 ], [ 2, 3, 4, 5, 6, 7, -1, -2, -4, -7 ], [ 8 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 2, 3, 5, 7, 8, -5 ], [ 4 ], [ 6, -1, -2, -4, -7 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 5, -1, -2, -4, -7 ], [ 2, 3, 4, 6, 7, 8, -5 ], [ -3 ], 
     [ -6, -8 ]>, <bipartition: [ 1, 2, 3, 4, -7 ], [ 5, 6, 7, 8 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 7, -7 ], [ 6, 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, 8, -1, -2, -4, -7 ], [ 5, 6, -5 ], [ -3 ], 
     [ -6, -8 ]>, <bipartition: [ 1, 2, 3, 4, 7, -5 ], [ 5, -1, -2, -4, -7 ], 
     [ 6, 8 ], [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8, -5 ], [ 3, 4, 5, -1, -2, -4, -7 ], [ -3 ], 
     [ -6, -8 ]>, <bipartition: [ 1, 2, 5, 6, 7, -1, -2, -4, -7 ], 
     [ 3, 8, -5 ], [ 4 ], [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8, -1, -6, -7 ], [ 6 ], [ 7, -3 ], 
     [ -2, -5, -8 ], [ -4 ]>, 
  <bipartition: [ 1, 3, 4 ], [ 2, 5, 6, 7, 8, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 4, 5, 8, -5 ], [ 2, 3, 6, -1, -2, -4, -7 ], [ 7 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 8, -1, -2, -4, -7 ], [ 2, 3, 4, 5, -5 ], [ 6, 7 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 7, 8, -5 ], [ 2, 3, 5, 6, -1, -2, -4, -7 ], [ 4 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -5 ], [ 5, 7, 8, -1, -2, -4, -7 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -2, -4, -7 ], [ 5, 6, 7, 8, -5 ], [ -3 ], 
     [ -6, -8 ]>, <bipartition: [ 1, 2, 5, 6, 7 ], [ 3, 8, -7 ], [ 4 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4 ], [ 2 ], [ 7, -5 ], [ -1, -2, -3, -6 ]
      , [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 7, -7 ], [ 6 ], [ 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 5, -7 ], [ 2, 3, 4, 6, 7 ], [ 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -7 ], [ 2 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -1, -4, -5 ], [ 3, -3, -8 ], [ 6, 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 8, -4 ], [ 3, 4, 5, 6, 7, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 2, 3, 4, 8, -1, -3, -5, -8 ], 
     [ 5, -4 ], [ 6, 7 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -1, -3, -5, -8 ], [ 8, -4 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 2, 4, 5, 7, -1, -6, -7 ], [ 3, -3 ], 
     [ 6, 8 ], [ -2, -5, -8 ], [ -4 ]>, 
  <bipartition: [ 1, 2, 8, -1, -2, -4, -7 ], [ 3, 4, 5, 6, 7, -5 ], [ -3 ], 
     [ -6, -8 ]>, <bipartition: [ 1, 3, 4, 5, 7, 8, -7 ], [ 2 ], [ 6 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7, -7 ], [ 2, 5, 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, -4 ], [ 2, 5, 6, 7, 8, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1 ], [ 2, 3, 4, -1, -2, -4, -7 ], 
     [ 5, 6, 7, -5 ], [ 8 ], [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8, -5 ], [ 5, -1, -2, -4, -7 ], [ 6, 7 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 6, 7, 8, -7 ], [ 2, 3, 5 ], [ 4 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 4, -1, -3, -7 ], [ 2, 7, 8, -6 ], [ 3, 5, 6, -4 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -5 ], 
     [ 8, -1, -2, -4, -7 ], [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 5, -4 ], [ 2, 3, 4, 6, 7, 8, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 5, 6, 7, -5 ], [ 2, 3, 4, -1, -2, -4, -7 ], [ 8 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -1, -2, -4, -7 ], [ 5, 7, -5 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -5 ], [ 5, 6, 7, -1, -2, -4, -7 ], [ 8 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 4, -8 ], [ 2, 3, 5, 6, 7, 8, -1 ], [ -2, -3, -4, -7 ], 
     [ -5 ], [ -6 ]>, <bipartition: [ 1, 2, 3, 4, 7, 8, -3, -4, -5, -6, -8 ], 
     [ 5, 6, -1, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 7 ], [ 4, 5, 6, 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 4, 7, -3, -4, -5, -6, -8 ], [ 3 ], [ 5, -1, -7 ], 
     [ 6, 8 ], [ -2 ]>, <bipartition: [ 1, 2, 6, 7, 8, -3, -4, -5, -6, -8 ], 
     [ 3, 4, 5, -1, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -3, -4, -5, -6, -8 ], [ 3, 8, -1, -7 ], 
     [ 4 ], [ -2 ]>, 
  <bipartition: [ 1, 5, 8, -1, -3, -5, -7 ], [ 2, 3, 4, -4, -6, -8 ], 
     [ 6, 7 ], [ -2 ]>, 
  <bipartition: [ 1, 7, 8, -1, -7 ], [ 2, 3, 5, 6, -3, -4, -5, -6, -8 ], 
     [ 4 ], [ -2 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -1, -7 ], 
     [ 5, 7, 8, -3, -4, -5, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -2, -4, -7 ], [ 2, 7, -5 ], [ -3 ], 
     [ -6, -8 ]>, <bipartition: [ 1, 3, 4, -5 ], [ 2, 6, 8, -1, -2, -4, -7 ], 
     [ 5, 7 ], [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 2, 4, 7, 8, -1, -2, -4, -7 ], [ 3, 5, 6, -5 ], [ -3 ], 
     [ -6, -8 ]>, 
  <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -5 ], [ 5, 7, -1, -2, -4, -7 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 5, 8, -1, -2, -4, -7 ], [ 2, 3, 4, -5 ], [ 6, 7 ], 
     [ -3 ], [ -6, -8 ]>, <bipartition: [ 1, 2, 4, 7, 8, -7 ], [ 3, 5, 6 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 5 ], [ 2, 3, 4, 6, 7, 8, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, 8, -1, -3, -4, -6 ], [ 5, 6, -2, -5 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 2, 3, 4, 7, -1, -3, -4, -6 ], 
     [ 5, -2, -5 ], [ 6, 8 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8, -1, -3, -4, -6 ], [ 3, 4, 5, -2, -5 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 2, 5, 6, 7, -1, -3, -4, -6 ], 
     [ 3, 8, -2, -5 ], [ 4 ], [ -7, -8 ]>, 
  <bipartition: [ 1 ], [ 2, 3, 4, 5, 6, 7, -7 ], [ 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 7, 8, -2, -5 ], [ 2, 3, 5 ], [ 4 ], [ 6, -1, -3, -4, -6 ]
      , [ -7, -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -7 ], [ 5, 7, 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -1, -3, -4, -6 ], [ 2, 4, 7, -2, -5 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 2, 3, 4, 6, 8, -1, -2, -4, -7 ], 
     [ 5, 7, -5 ], [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -1, -2, -4, -7 ], [ 2, 4, 7, -5 ], [ -3 ], 
     [ -6, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, -5 ], [ 7, 8, -1, -2, -4, -7 ], [ -3 ], 
     [ -6, -8 ]>, <bipartition: [ 1, 3, 4, 5, 7, -5 ], [ 2, -1, -2, -4, -7 ], 
     [ 6 ], [ 8 ], [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 6, 7, 8, -5 ], [ 2, 3, 5, -1, -2, -4, -7 ], [ 4 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -5 ], [ 2 ], [ 4, 7, -1, -2, -4, -7 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -7 ], [ 2, 7 ], [ 6 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 6, 8 ], [ 5, 7, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8 ], [ 5, -7 ], [ 6, 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5 ], [ 6, 7 ], [ 8, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 4, 7 ], [ 3, 5, -7 ], [ 6, 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8 ], [ 3, 4, 5, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -1, -2, -4, -7 ], [ 2, 7, -5 ], [ 6 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 3, 4, -1, -2, -4, -7 ], [ 2, 6, 8 ], [ 5, 7, -5 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 5, 8 ], [ 2, 3, 4, 6, 7 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5, 8, -7 ], [ 2, 3, 4 ], [ 6, 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, -4, -8 ], [ 7, 8, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, <bipartition: [ 1, 3, 4, 5, 7, -5 ], [ 2, 6, 8, -1, -2, -4, -7 ]
      , [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -5 ], [ 2, -1, -2, -4, -7 ], [ -3 ], 
     [ -6, -8 ]>, <bipartition: [ 1, 2, 3, 4, 5, 8, -2, -4 ], [ 6 ], 
     [ 7, -1, -6 ], [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 4, -1, -6 ], [ 2, 3, 5, 6, 7, 8, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -7 ], [ 3 ], [ 6, 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 6, 8, -7 ], [ 5, 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7, -2, -4 ], [ 2, 5, 8, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 8, -1, -6 ], [ 2, 3, 4, 5, -2, -4 ], [ 6, 7 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -7 ], [ 2, 6, 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -6 ], [ 2, 7, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -2, -4 ], [ 2, 6, 8, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 4, 7, -7 ], [ 3, 5 ], [ 6, 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -2, -4, -7 ], [ 2 ], [ 7, -5 ], 
     [ -3 ], [ -6, -8 ]>, <bipartition: [ 1, 4 ], [ 2, 3, 5, 6, 7, 8, -7 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8 ], [ 2 ], [ 7, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -4 ], [ 3, -5 ], [ 6, 8 ], [ -1, -2, -3, -6 ]
      , [ -7 ], [ -8 ]>, <bipartition: [ 1, 2, 8 ], [ 3, 4, 5, 6, 7, -7 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8, -7 ], [ 5 ], [ 6, 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 5, 8 ], [ 2, 4, 7 ], [ 3, 6 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -7 ], [ 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -7 ], [ 2, 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -7 ], [ 5, 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8, -1, -4, -5 ], [ 6 ], [ 7, -3, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, -4 ], [ 5, 6, 7, 8, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 5, -4 ], [ 2, 3, 4, 6, 7 ], [ 8, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 8, -1, -3, -5, -8 ], [ 2, 3, 4, 5, -4 ], [ 6, 7 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 2, 3, 4, 7, 8, -1, -3, -5, -8 ], 
     [ 5, 6, -4 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, -1, -3, -5, -8 ], [ 5, -4 ], [ 6, 8 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 2, 6, 7, 8, -1, -3, -5, -8 ], 
     [ 3, 4, 5, -4 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -1, -3, -5, -8 ], [ 3, 8, -4 ], [ 4 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 7, 8, -4 ], [ 2, 3, 5, 6, -1, -3, -5, -8 ], [ 4 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -4 ], [ 5, 7, 8, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4, -4 ], [ 2, 6, 8 ], [ 5, 7, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 2, 4, 7, 8, -1, -3, -5, -8 ], 
     [ 3, 5, 6, -4 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 5, -4 ], [ 2, 3, 4, 6, 7, 8, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 3, 4, 5, 6, -7 ], [ 2, 7, 8 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 5, 6, 7, -7 ], [ 2, 3, 4 ], [ 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8 ], [ 5, 7, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 4, -4 ], [ 2, 3, 5, 6, 7, 8, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 3, 4 ], [ 2, 6, 8 ], [ 5, 7, -7 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 4, 5, 8, -7 ], [ 2, 3, 6 ], [ 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -7 ], [ 2 ], [ 4, 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7, -1, -3, -6, -7 ], [ 2, 5, 8, -4 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 8, -4 ], [ 2, 3, 4, 5, -1, -3, -6, -7 ], [ 6, 7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4 ], [ 2, 7, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -1, -6, -7 ], [ 2, 6, 8, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, -1, -7 ], [ 2, 3, 4, 5, 6, 7, -3, -4, -5, -6, -8 ], 
     [ 8 ], [ -2 ]>, <bipartition: [ 1, 2, 3, 5, 7, 8, -1, -7 ], [ 4 ], 
     [ 6, -3, -4, -5, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8 ], [ 6, 7 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4 ], [ 5, 6, 7, 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 7 ], [ 6, 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, 7, 8 ], [ -1, -2, -3, -6 ], [ -4, -5 ], 
     [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 5, 6, 7, 8 ], [ 4 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 7, 8 ], [ 6 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, 8 ], [ 7 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, 7 ], [ 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5, 7, 8 ], [ 2, 3, 4 ], [ 6 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, -3, -4, -5, -6, -8 ], [ 5, -1, -7 ], 
     [ 6, 8 ], [ -2 ]>, <bipartition: [ 1, 2, 3, 4, 5, 8, -7 ], [ 6 ], [ 7 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 4, 5, 8, -3, -4, -5, -6, -8 ], [ 2, 3, 6, -1, -7 ], 
     [ 7 ], [ -2 ]>, 
  <bipartition: [ 1, 8, -1, -7 ], [ 2, 3, 4, 5, -3, -4, -5, -6, -8 ], 
     [ 6, 7 ], [ -2 ]>, 
  <bipartition: [ 1, 5, -3, -4, -5, -6, -8 ], [ 2, 3, 4, 6, 7 ], 
     [ 8, -1, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -3, -4, -5, -6, -8 ], [ 2 ], 
     [ 7, -1, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, 6, 8, -1, -3, -5, -7 ], [ 5, 7, -4, -6, -8 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7, -1, -7 ], [ 2, 5, 8, -3, -4, -5, -6, -8 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 5, 8, -3, -4, -5, -6, -8 ], [ 2, 3, 4, -1, -7 ], 
     [ 6, 7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -1, -7 ], [ 3, 8, -3, -4, -5, -6, -8 ], 
     [ 4 ], [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 8, -3, -4, -5, -6, -8 ], 
     [ 2, 7, -1, -7 ], [ 6 ], [ -2 ]>, 
  <bipartition: [ 1, 5, 6, 7, -1, -7 ], [ 2, 3, 4, -3, -4, -5, -6, -8 ], 
     [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 7, 8, -3, -4, -5, -6, -8 ], [ 2, 3, 5, 6, -1, -7 ], 
     [ 4 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 4, 7, 8, -1, -7 ], [ 3, 5, 6, -3, -4, -5, -6, -8 ], 
     [ -2 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -3, -4, -5, -6, -8 ], 
     [ 5, 7, 8, -1, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -7 ], [ 3, 8 ], [ 4 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 4, -5 ], [ 2, 3, 5, 6, 7, 8, -1, -2, -4, -7 ], [ -3 ], 
     [ -6, -8 ]>, <bipartition: [ 1, 8 ], [ 2, 3, 4, 5, -7 ], [ 6, 7 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8 ], [ 2, 7, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, -2, -5 ], [ 2, 3, 4, 5, 6, 7, -1, -3, -4, -6 ], [ 8 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 2, 3, 5, 7, 8, -2, -5 ], [ 4 ], 
     [ 6, -1, -3, -4, -6 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 7, 8, -2, -5 ], [ 2, 3, 5, 6, -1, -3, -4, -6 ], [ 4 ], 
     [ -7, -8 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -2, -5 ], 
     [ 5, 7, 8, -1, -3, -4, -6 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 5, -1, -3, -4, -6 ], [ 2, 3, 4, 6, 7 ], [ 8, -2, -5 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -3, -4, -6 ], [ 2 ], 
     [ 7, -2, -5 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, -2, -5 ], [ 7, 8, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -2, -5 ], [ 2, -1, -3, -4, -6 ], [ 6 ], 
     [ 8 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7, -2, -5 ], [ 2, 5, 8, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, -1, -3, -4, -6 ], [ 2, 3, 4, 5, 6, 7, -2, -5 ], [ 8 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 5, 8, -1, -3, -4, -6 ], [ 2, 3, 4, -2, -5 ], [ 6, 7 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 6, 7, 8, -2, -5 ], [ 2, 3, 5, -1, -3, -4, -6 ], [ 4 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 3, 4, 5, 8 ], [ 2, 7, -7 ], [ 6 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 7, 8 ], [ 2, 3, 5, 6, -7 ], [ 4 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6 ], [ 5, 7, 8, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8, -1, -3, -4, -6 ], [ 5, -2, -5 ], [ 6, 7 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 4, -5 ], [ 2, 7, 8, -1, -2, -4, -7 ], 
     [ 3, 5, 6 ], [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, 8 ], [ 5, 6, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7 ], [ 5, -7 ], [ 6, 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 7, 8, -7 ], [ 2, 3, 5, 6 ], [ 4 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7 ], [ 3, -7 ], [ 6, 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, -7 ], [ 2, 5, 6, 7, 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 4, 7, 8 ], [ 3, 5, 6, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 5, -7 ], [ 2, 3, 4, 6, 7, 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 4, 5, 8 ], [ 2, 3, 6, -7 ], [ 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 8, -7 ], [ 2, 3, 4, 5 ], [ 6, 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 5 ], [ 2, 3, 4, 6, 7 ], [ 8, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4 ], [ 5, 6, 7, -7 ], [ 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 4, -7 ], [ 2, 3, 5, 6, 7, 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 6, 8 ], [ 5, 7 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5, -1, -2, -3, -5, -6 ], [ 2, 3, 4, 6, 7, 8, -4, -8 ], 
     [ -7 ]>, <bipartition: [ 1, 5, 6, 7, -2, -4 ], [ 2, 3, 4, -1, -6 ], 
     [ 8 ], [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -1, -6 ], [ 5, 7, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 6, 8, -1, -6 ], [ 5, 7, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5, 8, -2, -4 ], [ 2, 3, 4, -1, -6 ], [ 6, 7 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -1, -6 ], [ 2, 4, 7, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -6 ], [ 5, 6, 7, -2, -4 ], [ 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 4, -2, -4 ], [ 2, 3, 5, 6, 7, 8, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, -7 ], [ 5 ], [ 6, 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8, -7 ], [ 3, 4, 5 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -1, -2, -4, -7 ], [ 3, -5 ], [ 6, 8 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8, -4 ], [ 6 ], [ 7, -5 ], [ -1, -2, -3, -6 ]
      , [ -7 ], [ -8 ]>, <bipartition: [ 1, 2, 3, 4 ], [ 5, 6, 7, 8, -7 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, 8, -7 ], [ 5, 6 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8 ], [ 2, 7 ], [ 6 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8 ], [ 5 ], [ 6, 7 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -7 ], [ 5, 6, 7 ], [ 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 4, 7, -4 ], [ 3, 5, -1, -3, -5, -8 ], [ 6, 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4 ], [ 2, 7, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4, -1, -3, -5, -8 ], [ 2, 6, 8, -4 ], [ 5, 7 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 4, 7, 8, -4 ], [ 3, 5, 6, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 5, -1, -3, -5, -8 ], [ 2, 3, 4, 6, 7, 8, -4 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 2, 3, 4, 6, 8, -1, -3, -5, -8 ], 
     [ 5, 7, -4 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 5, 8, -4 ], [ 2, 3, 4, -1, -3, -5, -8 ], [ 6, 7 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 3, 5, 6, 8, -1, -3, -5, -8 ], 
     [ 2, 4, 7, -4 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, -4 ], [ 2, 3, 4, 5, 6, 7, -1, -3, -5, -8 ], [ 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 3, 5, 7, 8, -4 ], [ 4 ], [ 6, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 5, -1, -3, -5, -8 ], [ 2, 3, 4, 6, 7 ], [ 8, -4 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -3, -5, -8 ], 
     [ 2 ], [ 7, -4 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7, -4 ], [ 2, 5, 8, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 5, 8, -1, -3, -5, -8 ], [ 2, 3, 4, -4 ], [ 6, 7 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -4 ], [ 3, 8, -1, -3, -5, -8 ], [ 4 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 3, 4, 5, 8, -1, -3, -5, -8 ], 
     [ 2, 7, -4 ], [ 6 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 5, 6, 7, -4 ], [ 2, 3, 4, -1, -3, -5, -8 ], [ 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 7, 8, -1, -3, -5, -8 ], [ 2, 3, 5, 6, -4 ], [ 4 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -1, -3, -5, -8 ], [ 5, 7, 8, -4 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 3, 4, 6, 7, -1, -3, -5, -8 ], 
     [ 2, 5, 8, -4 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 8, -4 ], [ 2, 3, 4, 5, -1, -3, -5, -8 ], [ 6, 7 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 3, 4, 5, 7, -1, -3, -5, -8 ], 
     [ 2, 6, 8, -4 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 4 ], [ 2, 3, 5, 6, 7, 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5, 6, 7, -1, -3, -6, -7 ], [ 2, 3, 4, -4 ], [ 8 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -4 ], 
     [ 5, 7, -1, -3, -6, -7 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 6, 8, -4 ], [ 5, 7, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 5, 8, -1, -3, -6, -7 ], [ 2, 3, 4, -4 ], [ 6, 7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -4 ], [ 2, 4, 7, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -4 ], [ 5, 6, 7, -1, -6, -7 ], [ 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 4, -1, -6, -7 ], [ 2, 3, 5, 6, 7, 8, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -1, -7 ], 
     [ 5, 7, -3, -4, -5, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, -1, -7 ], [ 7, 8, -3, -4, -5, -6, -8 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -1, -7 ], [ 2, -3, -4, -5, -6, -8 ], [ 6 ], 
     [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, -3, -4, -5, -6, -8 ], [ 2, 3, 4, 5, 6, 7, -1, -7 ], 
     [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 6, 7, 8, -1, -7 ], [ 2, 3, 5, -3, -4, -5, -6, -8 ], 
     [ 4 ], [ -2 ]>, <bipartition: [ 1, 2, 4, 7 ], [ 3, 5 ], [ 6, 8 ], 
     [ -1, -2, -3, -6 ], [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4 ], [ 2, 5, 6, 7, 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7 ], [ 3, 8 ], [ 4 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8 ], [ 2 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 7 ], [ 6 ], [ 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5 ], [ 2, 3, 4, 6, 7 ], [ 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, 8 ], [ 2 ], [ 6 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7 ], [ 2, 5, 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1 ], [ 2, 3, 4, 5, 6, 7 ], [ 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 6, 7, 8 ], [ 2, 3, 5 ], [ 4 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7 ], [ 3 ], [ 6, 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 4, 7, 8 ], [ 3, 5, 6 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5 ], [ 2, 3, 4, 6, 7, 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6 ], [ 5, 7, 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5 ], [ 6, 7 ], [ 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8 ], [ 3, 4, 5 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7 ], [ 2, 6, 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5, 8 ], [ 2, 3, 4 ], [ 6, 7 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 6, 8, -1, -7 ], [ 5, 7, -3, -4, -5, -6, -8 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -1, -7 ], [ 2, 4, 7, -3, -4, -5, -6, -8 ], 
     [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -3, -4, -5, -6, -8 ], 
     [ 2, 7, -1, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, -1, -7 ], [ 2, 6, 8, -3, -4, -5, -6, -8 ], 
     [ 5, 7 ], [ -2 ]>, <bipartition: [ 1, 2, 4, 7, 8, -3, -4, -5, -6, -8 ], 
     [ 3, 5, 6, -1, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 5, -1, -7 ], [ 2, 3, 4, 6, 7, 8, -3, -4, -5, -6, -8 ], 
     [ -2 ]>, <bipartition: [ 1, 2, 4, 5, 7, -3, -4, -5, -6, -8 ], 
     [ 3, -1, -7 ], [ 6, 8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 4, 7, -1, -3, -5, -7 ], [ 3, 5, -4, -6, -8 ], 
     [ 6, 8 ], [ -2 ]>, <bipartition: [ 1, 2, 5, 6, 7, -1, -3, -5, -7 ], 
     [ 3, 8, -4, -6, -8 ], [ 4 ], [ -2 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -3, -4, -5, -6, -8 ], 
     [ 5, 7, -1, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, 6, 8, -3, -4, -5, -6, -8 ], [ 5, 7, -1, -7 ], 
     [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -7 ], [ 2 ], 
     [ 7, -3, -4, -5, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7, -3, -4, -5, -6, -8 ], [ 2, 5, 8, -1, -7 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 5, 8, -1, -7 ], [ 2, 3, 4, -3, -4, -5, -6, -8 ], 
     [ 6, 7 ], [ -2 ]>, 
  <bipartition: [ 1, 8, -3, -4, -5, -6, -8 ], [ 2, 3, 4, 5, -1, -7 ], 
     [ 6, 7 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -1, -7 ], [ 2, 7, -3, -4, -5, -6, -8 ], 
     [ 6 ], [ -2 ]>, <bipartition: [ 1, 5, 6, 7, -3, -4, -5, -6, -8 ], 
     [ 2, 3, 4, -1, -7 ], [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -7 ], [ 2 ], [ 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8 ], [ 2, 4, 7, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -2, -5 ], [ 5, 7, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -2, -5 ], [ 3, 8, -1, -3, -4, -6 ], [ 4 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 3, 4, 5, 8, -1, -3, -4, -6 ], 
     [ 2, 7, -2, -5 ], [ 6 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 5, 6, 7, -2, -5 ], [ 2, 3, 4, -1, -3, -4, -6 ], [ 8 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 7, 8, -1, -3, -4, -6 ], [ 2, 3, 5, 6, -2, -5 ], [ 4 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 2, 4, 7, 8, -2, -5 ], [ 3, 5, 6, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -1, -3, -4, -6 ], 
     [ 5, 7, 8, -2, -5 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -3, -4, -6 ], [ 2, 7, -2, -5 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 4, -2, -5 ], [ 2, 6, 8, -1, -3, -4, -6 ], [ 5, 7 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 2, 4, 7, 8, -1, -3, -4, -6 ], 
     [ 3, 5, 6, -2, -5 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 5, -2, -5 ], [ 2, 3, 4, 6, 7, 8, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 2, 4, 5, 7, -1, -3, -4, -6 ], 
     [ 3, -2, -5 ], [ 6, 8 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 5, -1, -3, -4, -6 ], [ 2, 3, 4, 6, 7, 8, -2, -5 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 2, 8, -1, -3, -4, -6 ], [ 3, 4, 5, 6, 7, -2, -5 ], 
     [ -7, -8 ]>, <bipartition: [ 1 ], [ 2, 3, 4, -1, -3, -4, -6 ], 
     [ 5, 6, 7, -2, -5 ], [ 8 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8, -2, -5 ], [ 5, -1, -3, -4, -6 ], [ 6, 7 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 4, -2, -5 ], [ 2, 7, 8, -1, -3, -4, -6 ], [ 3, 5, 6 ], 
     [ -7, -8 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -2, -5 ], 
     [ 8, -1, -3, -4, -6 ], [ -7, -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -1, -3, -4, -6 ], [ 5, 7, -2, -5 ]
      , [ -7, -8 ]>, <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -1, -3, -4, -6 ], 
     [ 5, 7, -2, -5 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 5, 8, -2, -5 ], [ 2, 3, 4, -1, -3, -4, -6 ], [ 6, 7 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 2, 3, 4, 6, 8, -1, -3, -4, -6 ], 
     [ 5, 7, -2, -5 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 4, 5, 8, -2, -5 ], [ 2, 3, 6, -1, -3, -4, -6 ], [ 7 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 3, 5, 6, 8, -2, -5 ], [ 2 ], 
     [ 4, 7, -1, -3, -4, -6 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7 ], [ 2, 5, 8, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 5, 6, 7 ], [ 2, 3, 4, -7 ], [ 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, -7 ], [ 2, 3, 4, 5, 6, 7 ], [ 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 5, 7, 8, -7 ], [ 4 ], [ 6 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 5, 8 ], [ 2, 3, 4, -7 ], [ 6, 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8 ], [ 6 ], [ 7, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7 ], [ 2, 6, 8, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -7 ], [ 2, 4, 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, -7 ], [ 2, 6, 8 ], [ 5, 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -2, -3, -5, -6 ], [ 2, 7, -4, -8 ], 
     [ -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -4, -8 ], [ 2, 6, 8, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, <bipartition: [ 1, 2, 5, 6, 7, -1, -6 ], [ 3, 8, -2, -4 ], 
     [ 4 ], [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -2, -4 ], [ 5, 7, 8, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 4, 7, -1, -6 ], [ 3, 5, -2, -4 ], [ 6, 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 6, 8, -2, -4 ], [ 5, 7, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8, -1, -6 ], [ 5, -2, -4 ], [ 6, 7 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8, -1, -2, -4, -7 ], [ 6 ], [ 7, -5 ], 
     [ -3 ], [ -6, -8 ]>, 
  <bipartition: [ 1, 2, 3, 5, 7, 8 ], [ 4 ], [ 6, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, 8 ], [ 5, 6 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7 ], [ 5 ], [ 6, 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 7, 8 ], [ 2, 3, 5, 6 ], [ 4 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, -4 ], [ 5, -1, -3, -5, -8 ], [ 6, 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8, -4 ], [ 3, 4, 5, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 4, 5, 8, -4 ], [ 2, 3, 6, -1, -3, -5, -8 ], [ 7 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, -4 ], [ 5, 6, 7, -1, -3, -5, -8 ], [ 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 4, -1, -3, -5, -8 ], [ 2, 3, 5, 6, 7, 8, -4 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -3, -5, -8 ], 
     [ 2, 7, -4 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -4 ], [ 2, 6, 8, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 4, 7, -1, -3, -5, -8 ], [ 3, 5, -4 ], [ 6, 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 6, 8, -4 ], [ 5, 7, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -4 ], [ 5, 7, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, -4 ], [ 7, 8, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -4 ], [ 2, -1, -3, -5, -8 ], [ 6 ], [ 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, -1, -3, -5, -8 ], [ 2, 3, 4, 5, 6, 7, -4 ], [ 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 6, 7, 8, -4 ], [ 2, 3, 5, -1, -3, -5, -8 ], [ 4 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4, -4 ], [ 2, 6, 8, -1, -3, -5, -8 ], [ 5, 7 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 2, 4, 5, 7, -1, -3, -5, -8 ], 
     [ 3, -4 ], [ 6, 8 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -1, -3, -5, -8 ], [ 5, 7, -4 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4 ], [ 2 ], [ 7, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -4 ], [ 2, 7, -1, -3, -5, -8 ], [ 6 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 5, 6, 7, -1, -3, -5, -8 ], [ 2, 3, 4, -4 ], [ 8 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -4 ], 
     [ 5, 7, -1, -3, -5, -8 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -4 ], [ 2, 4, 7, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -4 ], [ 3, 8, -1, -3, -6, -7 ], [ 4 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -1, -3, -6, -7 ], [ 5, 7, 8, -4 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 4, 7, -4 ], [ 3, 5, -1, -3, -6, -7 ], [ 6, 8 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 2, 3, 4, 6, 8, -1, -3, -6, -7 ], 
     [ 5, 7, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8, -4 ], [ 5, -1, -3, -6, -7 ], [ 6, 7 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 2, 4, 7, -4 ], [ 3, 5, -1, -6, -7 ], 
     [ 6, 8 ], [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 4, 7, -1, -7 ], [ 3, 5, -3, -4, -5, -6, -8 ], 
     [ 6, 8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -7 ], [ 5, 6, 7, -3, -4, -5, -6, -8 ], 
     [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 5, -3, -4, -5, -6, -8 ], [ 2, 3, 4, 6, 7, 8, -1, -7 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 2, 8, -3, -4, -5, -6, -8 ], [ 3, 4, 5, 6, 7, -1, -7 ], 
     [ -2 ]>, <bipartition: [ 1 ], [ 2, 3, 4, -3, -4, -5, -6, -8 ], 
     [ 5, 6, 7, -1, -7 ], [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8, -1, -7 ], [ 5, -3, -4, -5, -6, -8 ], 
     [ 6, 7 ], [ -2 ]>, 
  <bipartition: [ 1, 4, -1, -7 ], [ 2, 7, 8, -3, -4, -5, -6, -8 ], 
     [ 3, 5, 6 ], [ -2 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -1, -7 ], 
     [ 8, -3, -4, -5, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -3, -4, -5, -6, -8 ], [ 5, 7, -1, -7 ]
      , [ -2 ]>, 
  <bipartition: [ 1, 4, 5, 8, -1, -7 ], [ 2, 3, 6, -3, -4, -5, -6, -8 ], 
     [ 7 ], [ -2 ]>, <bipartition: [ 1, 3, 5, 6, 8, -1, -7 ], [ 2 ], 
     [ 4, 7, -3, -4, -5, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 4, 5, 8 ], [ 2, 3, 6 ], [ 7 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 8 ], [ 2, 3, 4, 5 ], [ 6, 7 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8 ], [ 2 ], [ 7 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 8 ], [ 3, 4, 5, 6, 7 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7 ], [ 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8 ], [ 2, 7 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4 ], [ 2, 6, 8 ], [ 5, 7 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6 ], [ 2, 7, 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5, 6, 7 ], [ 2, 3, 4 ], [ 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8 ], [ 5, 7 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8 ], [ 2 ], [ 4, 7 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8 ], [ 6 ], [ 7 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4 ], [ 5, 6, 7 ], [ 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -3, -4, -5, -6, -8 ], [ 5, 6, 7, -1, -7 ], 
     [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 4, -1, -7 ], [ 2, 3, 5, 6, 7, 8, -3, -4, -5, -6, -8 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -7 ], [ 2, 7, -3, -4, -5, -6, -8 ], 
     [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 7, -3, -4, -5, -6, -8 ], 
     [ 2, 6, 8, -1, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8, -3, -4, -5, -6, -8 ], [ 6 ], 
     [ 7, -1, -7 ], [ -2 ]>, <bipartition: [ 1, 2, 3, 4, 7, -1, -3, -5, -7 ], 
     [ 5, -4, -6, -8 ], [ 6, 8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8, -1, -3, -5, -7 ], [ 3, 4, 5, -4, -6, -8 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 4, 5, 8, -1, -3, -5, -7 ], [ 2, 3, 6, -4, -6, -8 ], 
     [ 7 ], [ -2 ]>, 
  <bipartition: [ 1, 8, -4, -6, -8 ], [ 2, 3, 4, 5, -1, -3, -5, -7 ], 
     [ 6, 7 ], [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -3, -5, -7 ], 
     [ 2 ], [ 7, -4, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 4, 7, -3, -4, -5, -6, -8 ], [ 3, 5, -1, -7 ], 
     [ 6, 8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -1, -7 ], [ 3, -3, -4, -5, -6, -8 ], 
     [ 6, 8 ], [ -2 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -1, -7 ], 
     [ 5, 7, -3, -4, -5, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -3, -4, -5, -6, -8 ], [ 2, 4, 7, -1, -7 ], 
     [ -2 ]>, <bipartition: [ 1, 2, 4, 7, -2, -5 ], [ 3, 5, -1, -3, -4, -6 ], 
     [ 6, 8 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -2, -5 ], [ 5, 6, 7, -1, -3, -4, -6 ], [ 8 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -2, -5 ], [ 2 ], 
     [ 7, -1, -3, -4, -6 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7, -1, -3, -4, -6 ], [ 2, 5, 8, -2, -5 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 8, -1, -3, -4, -6 ], [ 2, 3, 4, 5, -2, -5 ], [ 6, 7 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -2, -5 ], [ 2, 7, -1, -3, -4, -6 ], [ 6 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 5, 6, 7, -1, -3, -4, -6 ], [ 2, 3, 4, -2, -5 ], [ 8 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -3, -4, -6 ], [ 5, 6, 7, -2, -5 ], [ 8 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 4, -2, -5 ], [ 2, 3, 5, 6, 7, 8, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 8, -2, -5 ], [ 2, 3, 4, 5, -1, -3, -4, -6 ], [ 6, 7 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -2, -5 ], [ 2, 7, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 3, 4, 5, 7, -1, -3, -4, -6 ], 
     [ 2, 6, 8, -2, -5 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8, -1, -3, -4, -6 ], [ 6 ], [ 7, -2, -5 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -2, -5 ], [ 2, 6, 8, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -3, -4, -6 ], [ 5, 6, 7, 8, -2, -5 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 2, 4, 7, -1, -3, -4, -6 ], [ 3, 5, -2, -5 ], [ 6, 8 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, 8, -2, -5 ], [ 5, 6, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, -2, -5 ], [ 5, -1, -3, -4, -6 ], [ 6, 8 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8, -2, -5 ], [ 3, 4, 5, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 3, 4, -1, -3, -4, -6 ], [ 2, 6, 8 ], 
     [ 5, 7, -2, -5 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 6, 8, -2, -5 ], [ 5, 7, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 5, -2, -5 ], [ 2, 3, 4, 6, 7 ], 
     [ 8, -1, -3, -4, -6 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -2, -5 ], [ 2, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -7 ], [ 5, 7 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, -7 ], [ 7, 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -7 ], [ 2 ], [ 6 ], [ 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -2, -3, -5, -6 ], [ 5, 6, 7, -4, -8 ], 
     [ 8 ], [ -7 ]>, 
  <bipartition: [ 1, 4, -4, -8 ], [ 2, 3, 5, 6, 7, 8, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -6 ], [ 2 ], 
     [ 7, -2, -4 ], [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -2, -4 ], [ 3, 8, -1, -6 ], [ 4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -1, -6 ], [ 2, 7, -2, -4 ], [ 6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 7, 8, -1, -6 ], [ 2, 3, 5, 6, -2, -4 ], [ 4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -1, -6 ], [ 5, 7, 8, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, -1, -6 ], [ 5, -2, -4 ], [ 6, 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8, -1, -6 ], [ 3, 4, 5, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 4, 5, 8, -1, -6 ], [ 2, 3, 6, -2, -4 ], [ 7 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 8, -2, -4 ], [ 2, 3, 4, 5, -1, -6 ], [ 6, 7 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 4, 7, -2, -4 ], [ 3, 5, -1, -6 ], [ 6, 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, 8, -1, -6 ], [ 5, 6, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 7, 8, -2, -4 ], [ 2, 3, 5, 6, -1, -6 ], [ 4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6 ], [ 7, 8, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7 ], [ 2, -7 ], [ 6 ], [ 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 6, 7, 8 ], [ 2, 3, 5, -7 ], [ 4 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 5, 7, 8 ], [ 4 ], [ 6 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, 8, -4 ], [ 5, 6, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -3, -5, -8 ], [ 5, 6, 7, -4 ], [ 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 4, -4 ], [ 2, 3, 5, 6, 7, 8, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 4, 5, 8, -1, -3, -5, -8 ], [ 2, 3, 6, -4 ], [ 7 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 8, -1, -3, -5, -8 ], [ 3, 4, 5, 6, 7, -4 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1 ], [ 2, 3, 4, -1, -3, -5, -8 ], 
     [ 5, 6, 7, -4 ], [ 8 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8, -4 ], [ 5, -1, -3, -5, -8 ], [ 6, 7 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 4, -4 ], [ 2, 7, 8, -1, -3, -5, -8 ], [ 3, 5, 6 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -4 ], 
     [ 8, -1, -3, -5, -8 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -1, -3, -5, -8 ], [ 5, 7, -4 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -4 ], [ 2 ], [ 4, 7, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 2, 3, 4, 5, 8, -1, -3, -5, -8 ], 
     [ 6 ], [ 7, -4 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -4 ], [ 3, -1, -3, -5, -8 ], [ 6, 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4 ], [ 2 ], [ 7, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 2, 5, 6, 7, -1, -3, -6, -7 ], 
     [ 3, 8, -4 ], [ 4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -4 ], [ 2, 7, -1, -3, -6, -7 ], [ 6 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 7, 8, -4 ], [ 2, 3, 5, 6, -1, -3, -6, -7 ], [ 4 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 2, 4, 7, 8, -1, -3, -6, -7 ], 
     [ 3, 5, 6, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -4 ], [ 5, 7, 8, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, -4 ], [ 5, -1, -3, -6, -7 ], [ 6, 8 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8, -4 ], [ 3, 4, 5, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 4, 5, 8, -4 ], [ 2, 3, 6, -1, -3, -6, -7 ], [ 7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 8, -1, -3, -6, -7 ], [ 2, 3, 4, 5, -4 ], [ 6, 7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 4, 7, -1, -3, -6, -7 ], [ 3, 5, -4 ], [ 6, 8 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, 8, -4 ], [ 5, 6, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 7, 8, -1, -3, -6, -7 ], [ 2, 3, 5, 6, -4 ], [ 4 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 2, 3, 4, 7, -4 ], [ 5, -1, -6, -7 ], 
     [ 6, 8 ], [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8, -4 ], [ 3, 4, 5, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 4, 5, 8, -4 ], [ 2, 3, 6, -1, -6, -7 ], [ 7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 8, -1, -6, -7 ], [ 2, 3, 4, 5, -4 ], [ 6, 7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, -1, -7 ], [ 5, -3, -4, -5, -6, -8 ], 
     [ 6, 8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8, -1, -7 ], [ 3, 4, 5, -3, -4, -5, -6, -8 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -1, -7 ], [ 2, 6, 8, -3, -4, -5, -6, -8 ], 
     [ -2 ]>, <bipartition: [ 1, 2, 3, 4, -3, -4, -5, -6, -8 ], 
     [ 5, 6, 7, 8, -1, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, 8, -1, -7 ], [ 5, 6, -3, -4, -5, -6, -8 ], 
     [ -2 ]>, <bipartition: [ 1, 3, 4, -3, -4, -5, -6, -8 ], [ 2, 6, 8 ], 
     [ 5, 7, -1, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 5, -1, -7 ], [ 2, 3, 4, 6, 7 ], [ 8, -3, -4, -5, -6, -8 ]
      , [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -1, -7 ], [ 2, -3, -4, -5, -6, -8 ], 
     [ -2 ]>, <bipartition: [ 1, 3, 5, 6, 8 ], [ 2, 4, 7 ], [ -1, -2, -3, -6 ]
      , [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 4, -3, -4, -5, -6, -8 ], [ 2, 3, 5, 6, 7, 8, -1, -7 ], 
     [ -2 ]>, <bipartition: [ 1, 2, 3, 4, 7, 8, -1, -3, -5, -7 ], 
     [ 5, 6, -4, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 7, 8, -4, -6, -8 ], [ 2, 3, 5, 6, -1, -3, -5, -7 ], 
     [ 4 ], [ -2 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -4, -6, -8 ], 
     [ 5, 7, 8, -1, -3, -5, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 5, -1, -3, -5, -7 ], [ 2, 3, 4, 6, 7 ], [ 8, -4, -6, -8 ]
      , [ -2 ]>, <bipartition: [ 1, 2, 3, 4, 6, 8, -4, -6, -8 ], 
     [ 5, 7, -1, -3, -5, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -4, -6, -8 ], [ 2, 4, 7, -1, -3, -5, -7 ], 
     [ -2 ]>, <bipartition: [ 1, 2, 4, 5, 7, -1, -3, -5, -7 ], 
     [ 3, -4, -6, -8 ], [ 6, 8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8, -1, -7 ], [ 6 ], [ 7, -3, -4, -5, -6, -8 ]
      , [ -2 ]>, <bipartition: [ 1, 2, 3, 4, 8, -3, -4, -5, -6, -8 ], 
     [ 5, -1, -7 ], [ 6, 7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -2, -5 ], [ 3, -1, -3, -4, -6 ], [ 6, 8 ], 
     [ -7, -8 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -2, -5 ], 
     [ 5, 7, -1, -3, -4, -6 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -2, -5 ], [ 2, 4, 7, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 4, -1, -3, -4, -6 ], [ 2, 3, 5, 6, 7, 8, -2, -5 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, 4, 5, 8, -1, -3, -4, -6 ], [ 2, 3, 6, -2, -5 ], [ 7 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 2, 3, 5, 7, 8, -1, -3, -4, -6 ], [ 4 ], 
     [ 6, -2, -5 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 4, -1, -3, -4, -6 ], [ 2, 6, 8, -2, -5 ], [ 5, 7 ], 
     [ -7, -8 ]>, <bipartition: [ 1 ], [ 2, 3, 4 ], [ 5, 6, 7, -7 ], [ 8 ], 
     [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 4, -7 ], [ 2, 7, 8 ], [ 3, 5, 6 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 4, 7, -1, -2, -3, -5, -6 ], [ 3, 5, -4, -8 ], 
     [ 6, 8 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -1, -6 ], [ 3, -2, -4 ], [ 6, 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -2, -4 ], [ 2 ], [ 7, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7, -1, -6 ], [ 2, 5, 8, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -2, -4 ], [ 2, 7, -1, -6 ], [ 6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5, 6, 7, -1, -6 ], [ 2, 3, 4, -2, -4 ], [ 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 4, 7, 8, -1, -6 ], [ 3, 5, 6, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5, -1, -6 ], [ 2, 3, 4, 6, 7 ], [ 8, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5, 8, -1, -6 ], [ 2, 3, 4, -2, -4 ], [ 6, 7 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -2, -4 ], [ 2, 4, 7, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, -2, -4 ], [ 5, -1, -6 ], [ 6, 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8, -2, -4 ], [ 3, 4, 5, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 4, 5, 8, -2, -4 ], [ 2, 3, 6, -1, -6 ], [ 7 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, -2, -4 ], [ 2, 3, 4, 5, 6, 7, -1, -6 ], [ 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 5, 7, 8, -2, -4 ], [ 4 ], [ 6, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 8, -7 ], [ 3, 4, 5, 6, 7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1 ], [ 2, 3, 4, -7 ], [ 5, 6, 7 ], [ 8 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 4 ], [ 2, 7, 8, -7 ], [ 3, 5, 6 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7 ], [ 8, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8 ], [ 2 ], [ 4, 7, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6 ], [ 7, 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7 ], [ 2 ], [ 6 ], [ 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 5, 7, 8, -1, -3, -5, -8 ], [ 4 ], [ 6, -4 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -3, -5, -8 ], [ 5, 6, 7, 8, -4 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 3, 4, -1, -3, -5, -8 ], [ 2, 6, 8 ], 
     [ 5, 7, -4 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -4 ], [ 2, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8, -4 ], [ 6 ], [ 7, -1, -3, -5, -8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -4 ], [ 3, -1, -3, -6, -7 ], [ 6, 8 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -3, -6, -7 ], 
     [ 2 ], [ 7, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7, -4 ], [ 2, 5, 8, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 3, 4, 5, 8, -1, -3, -6, -7 ], 
     [ 2, 7, -4 ], [ 6 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 5, 6, 7, -4 ], [ 2, 3, 4, -1, -3, -6, -7 ], [ 8 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 4, 7, 8, -4 ], [ 3, 5, 6, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 5, -4 ], [ 2, 3, 4, 6, 7 ], [ 8, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 5, 8, -4 ], [ 2, 3, 4, -1, -3, -6, -7 ], [ 6, 7 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 3, 5, 6, 8, -1, -3, -6, -7 ], 
     [ 2, 4, 7, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, -1, -3, -6, -7 ], [ 5, -4 ], [ 6, 8 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 2, 6, 7, 8, -1, -3, -6, -7 ], 
     [ 3, 4, 5, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 4, 5, 8, -1, -3, -6, -7 ], [ 2, 3, 6, -4 ], [ 7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, -1, -3, -6, -7 ], [ 2, 3, 4, 5, 6, 7, -4 ], [ 8 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 2, 3, 5, 7, 8, -1, -3, -6, -7 ], 
     [ 4 ], [ 6, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, 8, -4 ], [ 5, 6, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -4 ], [ 3, 8, -1, -6, -7 ], [ 4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 7, 8, -1, -6, -7 ], [ 2, 3, 5, 6, -4 ], [ 4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -1, -6, -7 ], [ 5, 7, 8, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 5, -4 ], [ 2, 3, 4, 6, 7 ], [ 8, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, <bipartition: [ 1, 2, 3, 4, 6, 8, -1, -6, -7 ], 
     [ 5, 7, -4 ], [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 5, 8, -4 ], [ 2, 3, 4, -1, -6, -7 ], [ 6, 7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -1, -6, -7 ], [ 2, 4, 7, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 3, 5, 7, 8, -3, -4, -5, -6, -8 ], [ 4 ], 
     [ 6, -1, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, -3, -4, -5, -6, -8 ], [ 2, 6, 8, -1, -7 ], 
     [ 5, 7 ], [ -2 ]>, 
  <bipartition: [ 1, -4, -6, -8 ], [ 2, 3, 4, 5, 6, 7, -1, -3, -5, -7 ], 
     [ 8 ], [ -2 ]>, <bipartition: [ 1, 2, 3, 5, 7, 8, -4, -6, -8 ], [ 4 ], 
     [ 6, -1, -3, -5, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7, -4, -6, -8 ], [ 2, 5, 8, -1, -3, -5, -7 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -4, -6, -8 ], [ 3, 8, -1, -3, -5, -7 ], 
     [ 4 ], [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 8, -1, -3, -5, -7 ], 
     [ 2, 7, -4, -6, -8 ], [ 6 ], [ -2 ]>, 
  <bipartition: [ 1, 5, 6, 7, -4, -6, -8 ], [ 2, 3, 4, -1, -3, -5, -7 ], 
     [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 7, 8, -1, -3, -5, -7 ], [ 2, 3, 5, 6, -4, -6, -8 ], 
     [ 4 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 4, 7, 8, -4, -6, -8 ], [ 3, 5, 6, -1, -3, -5, -7 ], 
     [ -2 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -1, -3, -5, -7 ], 
     [ 5, 7, 8, -4, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -3, -5, -7 ], [ 2, 7, -4, -6, -8 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 3, 4, -4, -6, -8 ], [ 2, 6, 8, -1, -3, -5, -7 ], 
     [ 5, 7 ], [ -2 ]>, <bipartition: [ 1, 2, 4, 7, 8, -1, -3, -5, -7 ], 
     [ 3, 5, 6, -4, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 5, -4, -6, -8 ], [ 2, 3, 4, 6, 7, 8, -1, -3, -5, -7 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 2, 4, 7, -4, -6, -8 ], [ 3, 5, -1, -3, -5, -7 ], 
     [ 6, 8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8, -4, -6, -8 ], [ 5, -1, -3, -5, -7 ], 
     [ 6, 7 ], [ -2 ]>, <bipartition: [ 1, 2, 3, 4, 5, 8, -1, -3, -5, -7 ], 
     [ 6 ], [ 7, -4, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8, -2, -5 ], [ 6 ], [ 7, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 2, 3, 4, 5, 6, -1, -3, -4, -6 ], 
     [ 7, 8, -2, -5 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -1, -3, -4, -6 ], [ 2, -2, -5 ], [ 6 ], 
     [ 8 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 6, 7, 8, -1, -3, -4, -6 ], [ 2, 3, 5, -2, -5 ], [ 4 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 2, 3, 4, 7, -1, -2, -3, -5, -6 ], 
     [ 5, -4, -8 ], [ 6, 8 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8, -1, -2, -3, -5, -6 ], [ 3, 4, 5, -4, -8 ], 
     [ -7 ]>, <bipartition: [ 1, 4, 5, 8, -1, -2, -3, -5, -6 ], 
     [ 2, 3, 6, -4, -8 ], [ 7 ], [ -7 ]>, 
  <bipartition: [ 1, 8, -4, -8 ], [ 2, 3, 4, 5, -1, -2, -3, -5, -6 ], 
     [ 6, 7 ], [ -7 ]>, <bipartition: [ 1, 2, 3, 4, 5, 8, -1, -6 ], [ 6 ], 
     [ 7, -2, -4 ], [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -2, -4 ], [ 5, 7, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, -2, -4 ], [ 2, 6, 8, -1, -6 ], [ 5, 7 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5, -2, -4 ], [ 2, 3, 4, 6, 7, 8, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8, -2, -4 ], [ 5, -1, -6 ], [ 6, 7 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, 8, -2, -4 ], [ 5, 6, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 5, -2, -4 ], [ 2, 3, 4, 6, 7 ], [ 8, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -2, -4 ], 
     [ 5, 7, -1, -6 ], [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, -2, -4 ], [ 7, 8, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -2, -4 ], [ 2, -1, -6 ], [ 6 ], [ 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, -1, -6 ], [ 2, 3, 4, 5, 6, 7, -2, -4 ], [ 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 6, 7, 8, -2, -4 ], [ 2, 3, 5, -1, -6 ], [ 4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8 ], [ 2, -7 ], [ -1, -2 ], 
     [ -3, -6, -8 ], [ -4 ], [ -5 ]>, 
  <bipartition: [ 1 ], [ 2, 3, 4 ], [ 5, 6, 7 ], [ 8 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 4 ], [ 2, 7, 8 ], [ 3, 5, 6 ], [ -1, -2, -3, -6 ], 
     [ -4, -5 ], [ -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, -1, -3, -5, -8 ], [ 7, 8, -4 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 3, 4, 5, 7, -1, -3, -5, -8 ], 
     [ 2, -4 ], [ 6 ], [ 8 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 6, 7, 8, -1, -3, -5, -8 ], [ 2, 3, 5, -4 ], [ 4 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8, -4 ], [ 6 ], [ 7, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 2, 4, 5, 7, -1, -3, -6, -7 ], 
     [ 3, -4 ], [ 6, 8 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -1, -3, -6, -7 ], [ 5, 7, -4 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4 ], [ 2, 7, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, -1, -3, -6, -7 ], [ 2, 6, 8, -4 ], [ 5, 7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 5, -1, -3, -6, -7 ], [ 2, 3, 4, 6, 7, 8, -4 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 2, 3, 4, 8, -1, -3, -6, -7 ], 
     [ 5, -4 ], [ 6, 7 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, 8, -1, -3, -6, -7 ], [ 5, 6, -4 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 5, -1, -3, -6, -7 ], [ 2, 3, 4, 6, 7 ], [ 8, -4 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -1, -3, -6, -7 ], 
     [ 5, 7, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, -1, -3, -6, -7 ], [ 7, 8, -4 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 3, 4, 5, 7, -1, -3, -6, -7 ], 
     [ 2, -4 ], [ 6 ], [ 8 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, -4 ], [ 2, 3, 4, 5, 6, 7, -1, -3, -6, -7 ], [ 8 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 6, 7, 8, -1, -3, -6, -7 ], [ 2, 3, 5, -4 ], [ 4 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, -1, -6, -7 ], [ 2, 3, 4, 5, 6, 7, -4 ], [ 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, <bipartition: [ 1, 2, 3, 5, 7, 8, -1, -6, -7 ], 
     [ 4 ], [ 6, -4 ], [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4 ], [ 2 ], [ 7, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7, -1, -6, -7 ], [ 2, 5, 8, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -1, -6, -7 ], [ 3, 8, -4 ], [ 4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -4 ], [ 2, 7, -1, -6, -7 ], [ 6 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 5, 6, 7, -1, -6, -7 ], [ 2, 3, 4, -4 ], [ 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 7, 8, -4 ], [ 2, 3, 5, 6, -1, -6, -7 ], [ 4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 4, 7, 8, -1, -6, -7 ], [ 3, 5, 6, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -4 ], [ 5, 7, 8, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, -1, -6, -7 ], [ 2, 6, 8, -4 ], [ 5, 7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 4, 7, 8, -4 ], [ 3, 5, 6, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 5, -1, -6, -7 ], [ 2, 3, 4, 6, 7, 8, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 4, 7, -1, -6, -7 ], [ 3, 5, -4 ], [ 6, 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 3, 4, 6, 8, -4 ], [ 5, 7, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8, -1, -6, -7 ], [ 5, -4 ], [ 6, 7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, -3, -4, -5, -6, -8 ], [ 7, 8, -1, -7 ], 
     [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 7, -3, -4, -5, -6, -8 ], 
     [ 2, -1, -7 ], [ 6 ], [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 6, 7, 8, -3, -4, -5, -6, -8 ], [ 2, 3, 5, -1, -7 ], 
     [ 4 ], [ -2 ]>, <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -4, -6, -8 ], 
     [ 5, 7, -1, -3, -5, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, -4, -6, -8 ], [ 7, 8, -1, -3, -5, -7 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -4, -6, -8 ], [ 2, -1, -3, -5, -7 ], [ 6 ], 
     [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, -1, -3, -5, -7 ], [ 2, 3, 4, 5, 6, 7, -4, -6, -8 ], 
     [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 6, 7, 8, -4, -6, -8 ], [ 2, 3, 5, -1, -3, -5, -7 ], 
     [ 4 ], [ -2 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -1, -3, -5, -7 ], 
     [ 5, 7, -4, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4, -6, -8 ], [ 2 ], [ 7, -1, -3, -5, -7 ]
      , [ -2 ]>, <bipartition: [ 1, 3, 4, 6, 7, -1, -3, -5, -7 ], 
     [ 2, 5, 8, -4, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 5, 8, -4, -6, -8 ], [ 2, 3, 4, -1, -3, -5, -7 ], 
     [ 6, 7 ], [ -2 ]>, 
  <bipartition: [ 1, 8, -1, -3, -5, -7 ], [ 2, 3, 4, 5, -4, -6, -8 ], 
     [ 6, 7 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -4, -6, -8 ], [ 2, 7, -1, -3, -5, -7 ], 
     [ 6 ], [ -2 ]>, 
  <bipartition: [ 1, 5, 6, 7, -1, -3, -5, -7 ], [ 2, 3, 4, -4, -6, -8 ], 
     [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -3, -5, -7 ], [ 5, 6, 7, -4, -6, -8 ], 
     [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 4, -4, -6, -8 ], [ 2, 3, 5, 6, 7, 8, -1, -3, -5, -7 ], 
     [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -4, -6, -8 ], 
     [ 2, 7, -1, -3, -5, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -1, -3, -5, -7 ], [ 2, 6, 8, -4, -6, -8 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, -4, -6, -8 ], [ 5, -1, -3, -5, -7 ], 
     [ 6, 8 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8, -4, -6, -8 ], [ 3, 4, 5, -1, -3, -5, -7 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 4, 5, 8, -4, -6, -8 ], [ 2, 3, 6, -1, -3, -5, -7 ], 
     [ 7 ], [ -2 ]>, <bipartition: [ 1, 2, 3, 4, 7, 8, -4, -6, -8 ], 
     [ 5, 6, -1, -3, -5, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 8, -2, -5 ], [ 3, 4, 5, 6, 7, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1 ], [ 2, 3, 4, -2, -5 ], [ 5, 6, 7, -1, -3, -4, -6 ], 
     [ 8 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 4, -1, -3, -4, -6 ], [ 2, 7, 8, -2, -5 ], [ 3, 5, 6 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -1, -3, -4, -6 ], 
     [ 8, -2, -5 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -1, -3, -4, -6 ], [ 2 ], [ 4, 7, -2, -5 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 2, 3, 4, 7, 8, -1, -2, -3, -5, -6 ], 
     [ 5, 6, -4, -8 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -1, -2, -3, -5, -6 ], [ 3, 8, -4, -8 ], 
     [ 4 ], [ -7 ]>, 
  <bipartition: [ 1, 7, 8, -4, -8 ], [ 2, 3, 5, 6, -1, -2, -3, -5, -6 ], 
     [ 4 ], [ -7 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -4, -8 ], 
     [ 5, 7, 8, -1, -2, -3, -5, -6 ], [ -7 ]>, 
  <bipartition: [ 1, 5, -1, -2, -3, -5, -6 ], [ 2, 3, 4, 6, 7 ], 
     [ 8, -4, -8 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 6, 8, -4, -8 ], [ 5, 7, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, 
  <bipartition: [ 1, 5, 8, -1, -2, -3, -5, -6 ], [ 2, 3, 4, -4, -8 ], 
     [ 6, 7 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -4, -8 ], [ 2, 4, 7, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -2, -4 ], [ 2, 7, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -1, -6 ], [ 2, 6, 8, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 5, 7, 8, -1, -6 ], [ 4 ], [ 6, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, -1, -6 ], [ 2, 6, 8, -2, -4 ], [ 5, 7 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -2, -4 ], [ 5, 6, 7, -1, -6 ], [ 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 8, -1, -6 ], [ 3, 4, 5, 6, 7, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1 ], [ 2, 3, 4, -1, -6 ], [ 5, 6, 7, -2, -4 ], [ 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 4, -2, -4 ], [ 2, 7, 8, -1, -6 ], [ 3, 5, 6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -2, -4 ], [ 8, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -1, -6 ], 
     [ 5, 7, -2, -4 ], [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -2, -4 ], [ 2 ], [ 4, 7, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1 ], [ 2, 3, 4, -4 ], [ 5, 6, 7, -1, -3, -5, -8 ], [ 8 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 4, -1, -3, -5, -8 ], [ 2, 7, 8, -4 ], [ 3, 5, 6 ], 
     [ -2, -6, -7 ]>, <bipartition: [ 1, 3, 5, 6, 8, -1, -3, -5, -8 ], [ 2 ], 
     [ 4, 7, -4 ], [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8, -1, -3, -6, -7 ], [ 6 ], [ 7, -4 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -4 ], [ 5, 6, 7, -1, -3, -6, -7 ], [ 8 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 4, -1, -3, -6, -7 ], [ 2, 3, 5, 6, 7, 8, -4 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -3, -6, -7 ], 
     [ 2, 7, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -4 ], [ 2, 6, 8, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 3, 5, 7, 8, -4 ], [ 4 ], [ 6, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, -4 ], [ 2, 6, 8, -1, -3, -6, -7 ], [ 5, 7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 5, -4 ], [ 2, 3, 4, 6, 7, 8, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -3, -6, -7 ], [ 5, 6, 7, -4 ], [ 8 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 8, -4 ], [ 3, 4, 5, 6, 7, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1 ], [ 2, 3, 4, -4 ], [ 5, 6, 7, -1, -3, -6, -7 ], [ 8 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 4, -1, -3, -6, -7 ], [ 2, 7, 8, -4 ], [ 3, 5, 6 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -1, -3, -6, -7 ], [ 8, -4 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -4 ], [ 5, 7, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 3, 5, 6, 8, -1, -3, -6, -7 ], [ 2 ], 
     [ 4, 7, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -1, -6, -7 ], [ 5, 7, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, <bipartition: [ 1, 2, 3, 4, 5, 6, -1, -6, -7 ], 
     [ 7, 8, -4 ], [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -1, -6, -7 ], [ 2, -4 ], [ 6 ], [ 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, -4 ], [ 2, 3, 4, 5, 6, 7, -1, -6, -7 ], [ 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 6, 7, 8, -1, -6, -7 ], [ 2, 3, 5, -4 ], [ 4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -4 ], [ 3, -1, -6, -7 ], [ 6, 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -4 ], 
     [ 5, 7, -1, -6, -7 ], [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -6, -7 ], [ 2 ], [ 7, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7, -4 ], [ 2, 5, 8, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 5, 8, -1, -6, -7 ], [ 2, 3, 4, -4 ], [ 6, 7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 8, -4 ], [ 2, 3, 4, 5, -1, -6, -7 ], [ 6, 7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -1, -6, -7 ], [ 2, 7, -4 ], [ 6 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 5, 6, 7, -4 ], [ 2, 3, 4, -1, -6, -7 ], [ 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -6, -7 ], 
     [ 2, 7, -4 ], [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -4 ], [ 2, 6, 8, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, -1, -6, -7 ], [ 5, -4 ], [ 6, 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8, -1, -6, -7 ], [ 3, 4, 5, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 4, 5, 8, -1, -6, -7 ], [ 2, 3, 6, -4 ], [ 7 ], 
     [ -2, -5, -8 ], [ -3 ]>, <bipartition: [ 1, 2, 3, 4, 7, 8, -1, -6, -7 ], 
     [ 5, 6, -4 ], [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 8, -1, -7 ], [ 3, 4, 5, 6, 7, -3, -4, -5, -6, -8 ], 
     [ -2 ]>, 
  <bipartition: [ 1 ], [ 2, 3, 4, -1, -7 ], [ 5, 6, 7, -3, -4, -5, -6, -8 ], 
     [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 4, -3, -4, -5, -6, -8 ], [ 2, 7, 8, -1, -7 ], 
     [ 3, 5, 6 ], [ -2 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -3, -4, -5, -6, -8 ], 
     [ 8, -1, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -3, -4, -5, -6, -8 ], [ 2 ], [ 4, 7, -1, -7 ]
      , [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, -4, -6, -8 ], [ 5, 6, 7, -1, -3, -5, -7 ], 
     [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 5, -1, -3, -5, -7 ], [ 2, 3, 4, 6, 7, 8, -4, -6, -8 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 2, 8, -1, -3, -5, -7 ], [ 3, 4, 5, 6, 7, -4, -6, -8 ], 
     [ -2 ]>, <bipartition: [ 1 ], [ 2, 3, 4, -1, -3, -5, -7 ], 
     [ 5, 6, 7, -4, -6, -8 ], [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 4, -4, -6, -8 ], [ 2, 7, 8, -1, -3, -5, -7 ], 
     [ 3, 5, 6 ], [ -2 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -4, -6, -8 ], 
     [ 8, -1, -3, -5, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -1, -3, -5, -7 ], [ 5, 7, -4, -6, -8 ]
      , [ -2 ]>, <bipartition: [ 1, 3, 5, 6, 8, -4, -6, -8 ], [ 2 ], 
     [ 4, 7, -1, -3, -5, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -4, -6, -8 ], [ 3, -1, -3, -5, -7 ], 
     [ 6, 8 ], [ -2 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -4, -6, -8 ], 
     [ 5, 7, -1, -3, -5, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -1, -3, -5, -7 ], [ 2, 4, 7, -4, -6, -8 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 4, -1, -3, -5, -7 ], [ 2, 3, 5, 6, 7, 8, -4, -6, -8 ], 
     [ -2 ]>, <bipartition: [ 1, 5, -4, -6, -8 ], [ 2, 3, 4, 6, 7 ], 
     [ 8, -1, -3, -5, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 5, 7, 8, -1, -3, -5, -7 ], [ 4 ], [ 6, -4, -6, -8 ]
      , [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, -2, -5 ], [ 5, 6, 7, 8, -1, -3, -4, -6 ], 
     [ -7, -8 ]>, <bipartition: [ 1, 3, 4, -2, -5 ], [ 2, 6, 8 ], 
     [ 5, 7, -1, -3, -4, -6 ], [ -7, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -1, -3, -4, -6 ], [ 2, -2, -5 ], 
     [ -7, -8 ]>, 
  <bipartition: [ 1, -4, -8 ], [ 2, 3, 4, 5, 6, 7, -1, -2, -3, -5, -6 ], 
     [ 8 ], [ -7 ]>, <bipartition: [ 1, 2, 3, 5, 7, 8, -4, -8 ], [ 4 ], 
     [ 6, -1, -2, -3, -5, -6 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -1, -2, -3, -5, -6 ], [ 2 ], 
     [ 7, -4, -8 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 4, 6, 7, -4, -8 ], [ 2, 5, 8, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, 
  <bipartition: [ 1, 2, 5, 6, 7, -4, -8 ], [ 3, 8, -1, -2, -3, -5, -6 ], 
     [ 4 ], [ -7 ]>, <bipartition: [ 1, 3, 4, 5, 8, -1, -2, -3, -5, -6 ], 
     [ 2, 7, -4, -8 ], [ 6 ], [ -7 ]>, 
  <bipartition: [ 1, 5, 6, 7, -4, -8 ], [ 2, 3, 4, -1, -2, -3, -5, -6 ], 
     [ 8 ], [ -7 ]>, 
  <bipartition: [ 1, 7, 8, -1, -2, -3, -5, -6 ], [ 2, 3, 5, 6, -4, -8 ], 
     [ 4 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 4, 7, 8, -4, -8 ], [ 3, 5, 6, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -1, -2, -3, -5, -6 ], 
     [ 5, 7, 8, -4, -8 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 4, -4, -8 ], [ 2, 6, 8, -1, -2, -3, -5, -6 ], 
     [ 5, 7 ], [ -7 ]>, <bipartition: [ 1, 2, 4, 7, 8, -1, -2, -3, -5, -6 ], 
     [ 3, 5, 6, -4, -8 ], [ -7 ]>, 
  <bipartition: [ 1, 5, -4, -8 ], [ 2, 3, 4, 6, 7, 8, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, 
  <bipartition: [ 1, 2, 4, 7, -4, -8 ], [ 3, 5, -1, -2, -3, -5, -6 ], 
     [ 6, 8 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 6, 8, -1, -2, -3, -5, -6 ], [ 5, 7, -4, -8 ], 
     [ -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8, -4, -8 ], [ 5, -1, -2, -3, -5, -6 ], 
     [ 6, 7 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, -1, -6 ], [ 7, 8, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -1, -6 ], [ 2, -2, -4 ], [ 6 ], [ 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 6, 7, 8, -1, -6 ], [ 2, 3, 5, -2, -4 ], [ 4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -6 ], [ 5, 6, 7, 8, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, <bipartition: [ 1, 3, 4, -1, -6 ], [ 2, 6, 8 ], 
     [ 5, 7, -2, -4 ], [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -2, -4 ], [ 2, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -1, -3, -5, -8 ], [ 2, -4 ], 
     [ -2, -6, -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, -4 ], [ 7, 8, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -4 ], [ 2, -1, -3, -6, -7 ], [ 6 ], [ 8 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 6, 7, 8, -4 ], [ 2, 3, 5, -1, -3, -6, -7 ], [ 4 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 3, 4, 5, 7, -1, -3, -6, -7 ], 
     [ 2, 6, 8, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -4 ], [ 5, 6, 7, 8, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, -4 ], [ 2, 6, 8 ], [ 5, 7, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -1, -3, -6, -7 ], 
     [ 2, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -6, -7 ], [ 5, 6, 7, -4 ], [ 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 8, -4 ], [ 3, 4, 5, 6, 7, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1 ], [ 2, 3, 4, -4 ], [ 5, 6, 7, -1, -6, -7 ], [ 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 4, -1, -6, -7 ], [ 2, 7, 8, -4 ], [ 3, 5, 6 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -1, -6, -7 ], [ 8, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -4 ], [ 5, 7, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -1, -6, -7 ], [ 2 ], [ 4, 7, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8, -4 ], [ 6 ], [ 7, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -1, -6, -7 ], [ 3, -4 ], [ 6, 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -1, -6, -7 ], [ 5, 7, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -4 ], [ 2, 4, 7, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 4, -4 ], [ 2, 3, 5, 6, 7, 8, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 5, -1, -6, -7 ], [ 2, 3, 4, 6, 7 ], [ 8, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 3, 5, 7, 8, -4 ], [ 4 ], [ 6, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -7 ], [ 5, 6, 7, 8, -3, -4, -5, -6, -8 ], 
     [ -2 ]>, <bipartition: [ 1, 3, 4, -1, -7 ], [ 2, 6, 8 ], 
     [ 5, 7, -3, -4, -5, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -3, -4, -5, -6, -8 ], [ 2, -1, -7 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -4, -6, -8 ], [ 2, 6, 8, -1, -3, -5, -7 ], 
     [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -3, -5, -7 ], [ 5, 6, 7, 8, -4, -6, -8 ], 
     [ -2 ]>, <bipartition: [ 1, 3, 4, -1, -3, -5, -7 ], [ 2, 6, 8 ], 
     [ 5, 7, -4, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -4, -6, -8 ], [ 2, -1, -3, -5, -7 ], 
     [ -2 ]>, <bipartition: [ 1, 2, 3, 4, 5, 8, -4, -6, -8 ], [ 6 ], 
     [ 7, -1, -3, -5, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8, -1, -3, -5, -7 ], [ 5, -4, -6, -8 ], 
     [ 6, 7 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, -1, -3, -5, -7 ], [ 2, 6, 8, -4, -6, -8 ], 
     [ 5, 7 ], [ -2 ]>, <bipartition: [ 1, 2, 3, 4, 5, 6, -1, -3, -5, -7 ], 
     [ 7, 8, -4, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -1, -3, -5, -7 ], [ 2, -4, -6, -8 ], [ 6 ], 
     [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 6, 7, 8, -1, -3, -5, -7 ], [ 2, 3, 5, -4, -6, -8 ], 
     [ 4 ], [ -2 ]>, <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -4, -8 ], 
     [ 5, 7, -1, -2, -3, -5, -6 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -4, -8 ], [ 2, -1, -2, -3, -5, -6 ], [ 6 ], 
     [ 8 ], [ -7 ]>, 
  <bipartition: [ 1, -1, -2, -3, -5, -6 ], [ 2, 3, 4, 5, 6, 7, -4, -8 ], 
     [ 8 ], [ -7 ]>, 
  <bipartition: [ 1, 6, 7, 8, -4, -8 ], [ 2, 3, 5, -1, -2, -3, -5, -6 ], 
     [ 4 ], [ -7 ]>, <bipartition: [ 1, 2, 4, 5, 7, -1, -2, -3, -5, -6 ], 
     [ 3, -4, -8 ], [ 6, 8 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -1, -2, -3, -5, -6 ], 
     [ 5, 7, -4, -8 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4, -8 ], [ 2 ], [ 7, -1, -2, -3, -5, -6 ]
      , [ -7 ]>, <bipartition: [ 1, 3, 4, 6, 7, -1, -2, -3, -5, -6 ], 
     [ 2, 5, 8, -4, -8 ], [ -7 ]>, 
  <bipartition: [ 1, 5, 8, -4, -8 ], [ 2, 3, 4, -1, -2, -3, -5, -6 ], 
     [ 6, 7 ], [ -7 ]>, 
  <bipartition: [ 1, 8, -1, -2, -3, -5, -6 ], [ 2, 3, 4, 5, -4, -8 ], 
     [ 6, 7 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 8, -4, -8 ], [ 2, 7, -1, -2, -3, -5, -6 ], 
     [ 6 ], [ -7 ]>, <bipartition: [ 1, 5, 6, 7, -1, -2, -3, -5, -6 ], 
     [ 2, 3, 4, -4, -8 ], [ 8 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 8, -4, -8 ], [ 2, 7, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, <bipartition: [ 1, 3, 4, 5, 7, -1, -2, -3, -5, -6 ], 
     [ 2, 6, 8, -4, -8 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, -4, -8 ], [ 5, -1, -2, -3, -5, -6 ], 
     [ 6, 8 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 6, 7, 8, -4, -8 ], [ 3, 4, 5, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, 
  <bipartition: [ 1, 4, 5, 8, -4, -8 ], [ 2, 3, 6, -1, -2, -3, -5, -6 ], 
     [ 7 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 7, 8, -4, -8 ], [ 5, 6, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, <bipartition: [ 1, 2, 8, -2, -4 ], [ 3, 4, 5, 6, 7, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1 ], [ 2, 3, 4, -2, -4 ], [ 5, 6, 7, -1, -6 ], [ 8 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 4, -1, -6 ], [ 2, 7, 8, -2, -4 ], [ 3, 5, 6 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -1, -6 ], [ 8, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, <bipartition: [ 1, 3, 5, 6, 8, -1, -6 ], [ 2 ], 
     [ 4, 7, -2, -4 ], [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 8, -1, -3, -6, -7 ], [ 3, 4, 5, 6, 7, -4 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1 ], [ 2, 3, 4, -1, -3, -6, -7 ], 
     [ 5, 6, 7, -4 ], [ 8 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 4, -4 ], [ 2, 7, 8, -1, -3, -6, -7 ], [ 3, 5, 6 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -4 ], 
     [ 8, -1, -3, -6, -7 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -4 ], [ 2 ], [ 4, 7, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -4 ], [ 5, 6, 7, 8, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, -4 ], [ 2, 6, 8 ], [ 5, 7, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -1, -6, -7 ], [ 2, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, <bipartition: [ 1, 2, 3, 4, 5, 8, -1, -6, -7 ], 
     [ 6 ], [ 7, -4 ], [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8, -4 ], [ 5, -1, -6, -7 ], [ 6, 7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, -4 ], [ 2, 6, 8, -1, -6, -7 ], [ 5, 7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, -4 ], [ 7, 8, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 5, 7, -4 ], [ 2, -1, -6, -7 ], [ 6 ], [ 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 6, 7, 8, -4 ], [ 2, 3, 5, -1, -6, -7 ], [ 4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 8, -4, -6, -8 ], [ 3, 4, 5, 6, 7, -1, -3, -5, -7 ], 
     [ -2 ]>, 
  <bipartition: [ 1 ], [ 2, 3, 4, -4, -6, -8 ], [ 5, 6, 7, -1, -3, -5, -7 ], 
     [ 8 ], [ -2 ]>, 
  <bipartition: [ 1, 4, -1, -3, -5, -7 ], [ 2, 7, 8, -4, -6, -8 ], 
     [ 3, 5, 6 ], [ -2 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -1, -3, -5, -7 ], 
     [ 8, -4, -6, -8 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -1, -3, -5, -7 ], [ 2 ], [ 4, 7, -4, -6, -8 ]
      , [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, -4, -8 ], [ 5, 6, 7, -1, -2, -3, -5, -6 ], 
     [ 8 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 8, -1, -2, -3, -5, -6 ], [ 3, 4, 5, 6, 7, -4, -8 ], 
     [ -7 ]>, <bipartition: [ 1 ], [ 2, 3, 4, -1, -2, -3, -5, -6 ], 
     [ 5, 6, 7, -4, -8 ], [ 8 ], [ -7 ]>, 
  <bipartition: [ 1, 4, -4, -8 ], [ 2, 7, 8, -1, -2, -3, -5, -6 ], 
     [ 3, 5, 6 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -4, -8 ], 
     [ 8, -1, -2, -3, -5, -6 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 4 ], [ 2, 6, 8, -1, -2, -3, -5, -6 ], [ 5, 7, -4, -8 ]
      , [ -7 ]>, <bipartition: [ 1, 3, 5, 6, 8, -4, -8 ], [ 2 ], 
     [ 4, 7, -1, -2, -3, -5, -6 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 8, -1, -2, -3, -5, -6 ], [ 6 ], 
     [ 7, -4, -8 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 4, 5, 7, -4, -8 ], [ 3, -1, -2, -3, -5, -6 ], 
     [ 6, 8 ], [ -7 ]>, <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -4, -8 ], 
     [ 5, 7, -1, -2, -3, -5, -6 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -1, -2, -3, -5, -6 ], [ 2, 4, 7, -4, -8 ], 
     [ -7 ]>, 
  <bipartition: [ 1, 4, -1, -2, -3, -5, -6 ], [ 2, 3, 5, 6, 7, 8, -4, -8 ], 
     [ -7 ]>, <bipartition: [ 1, 5, -4, -8 ], [ 2, 3, 4, 6, 7 ], 
     [ 8, -1, -2, -3, -5, -6 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 3, 5, 7, 8, -1, -2, -3, -5, -6 ], [ 4 ], 
     [ 6, -4, -8 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, -2, -4 ], [ 5, 6, 7, 8, -1, -6 ], 
     [ -3, -5, -7 ], [ -8 ]>, <bipartition: [ 1, 3, 4, -2, -4 ], [ 2, 6, 8 ], 
     [ 5, 7, -1, -6 ], [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -1, -6 ], [ 2, -2, -4 ], 
     [ -3, -5, -7 ], [ -8 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -3, -6, -7 ], [ 5, 6, 7, 8, -4 ], 
     [ -2, -5, -8 ]>, <bipartition: [ 1, 3, 4, -1, -3, -6, -7 ], [ 2, 6, 8 ], 
     [ 5, 7, -4 ], [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -4 ], [ 2, -1, -3, -6, -7 ], 
     [ -2, -5, -8 ]>, 
  <bipartition: [ 1, 2, 8, -1, -6, -7 ], [ 3, 4, 5, 6, 7, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1 ], [ 2, 3, 4, -1, -6, -7 ], [ 5, 6, 7, -4 ], [ 8 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 4, -4 ], [ 2, 7, 8, -1, -6, -7 ], [ 3, 5, 6 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -4 ], [ 8, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -4 ], [ 2 ], [ 4, 7, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 3, 4, -4, -6, -8 ], [ 5, 6, 7, 8, -1, -3, -5, -7 ], 
     [ -2 ]>, <bipartition: [ 1, 3, 4, -4, -6, -8 ], [ 2, 6, 8 ], 
     [ 5, 7, -1, -3, -5, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -1, -3, -5, -7 ], [ 2, -4, -6, -8 ], 
     [ -2 ]>, <bipartition: [ 1, 2, 3, 4, -1, -2, -3, -5, -6 ], 
     [ 5, 6, 7, 8, -4, -8 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 4, -1, -2, -3, -5, -6 ], [ 2, 6, 8 ], [ 5, 7, -4, -8 ]
      , [ -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -4, -8 ], [ 2, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, <bipartition: [ 1, 2, 3, 4, 5, 8, -4, -8 ], [ 6 ], 
     [ 7, -1, -2, -3, -5, -6 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 8, -1, -2, -3, -5, -6 ], [ 5, -4, -8 ], 
     [ 6, 7 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 4, -1, -2, -3, -5, -6 ], [ 2, 6, 8, -4, -8 ], 
     [ 5, 7 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, -1, -2, -3, -5, -6 ], [ 7, 8, -4, -8 ], 
     [ -7 ]>, <bipartition: [ 1, 3, 4, 5, 7, -1, -2, -3, -5, -6 ], 
     [ 2, -4, -8 ], [ 6 ], [ 8 ], [ -7 ]>, 
  <bipartition: [ 1, 6, 7, 8, -1, -2, -3, -5, -6 ], [ 2, 3, 5, -4, -8 ], 
     [ 4 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, -1, -6, -7 ], [ 5, 6, 7, 8, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, -1, -6, -7 ], [ 2, 6, 8 ], [ 5, 7, -4 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -4 ], [ 2, -1, -6, -7 ], 
     [ -2, -5, -8 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 8, -4, -8 ], [ 3, 4, 5, 6, 7, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, 
  <bipartition: [ 1 ], [ 2, 3, 4, -4, -8 ], [ 5, 6, 7, -1, -2, -3, -5, -6 ], 
     [ 8 ], [ -7 ]>, 
  <bipartition: [ 1, 4, -1, -2, -3, -5, -6 ], [ 2, 7, 8, -4, -8 ], 
     [ 3, 5, 6 ], [ -7 ]>, 
  <bipartition: [ 1 ], [ 2, 4 ], [ 3, 5, 6, 7, -1, -2, -3, -5, -6 ], 
     [ 8, -4, -8 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 5, 6, 8, -1, -2, -3, -5, -6 ], [ 2 ], [ 4, 7, -4, -8 ]
      , [ -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, -4, -8 ], [ 5, 6, 7, 8, -1, -2, -3, -5, -6 ], 
     [ -7 ]>, <bipartition: [ 1, 3, 4, -4, -8 ], [ 2, 6, 8 ], 
     [ 5, 7, -1, -2, -3, -5, -6 ], [ -7 ]>, 
  <bipartition: [ 1, 3, 4, 5, 6, 7, 8, -1, -2, -3, -5, -6 ], [ 2, -4, -8 ], 
     [ -7 ]> ]
gap> LClassReps(D);
[ <bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1, -3 ], [ -4, -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, -2, -3, -4 ], [ -1, -5 ]>, 
  <block bijection: [ 1, 2, 3, 4, 5, -1, -2, -3, -4, -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, -1, -2, -3 ], [ -4, -5 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, -3, -5 ], [ -1, -2, -4 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, -1, -2, -5 ], [ -3 ], [ -4 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, -1, -2, -3, -5 ], [ -4 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, -2, -3, -4, -5 ], [ -1 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, -1, -3 ], [ -2 ], [ -4, -5 ]> ]
gap> x := Bipartition([[1, 3, 4, 6, 7, -3, -4, -5, -6, -8],
> [2, 5, 8, -1, -7], [-2]]);;
gap> D := DClass(S, x);
<Green's D-class: <bipartition: [ 1, 3, 4, 6, 7, -3, -4, -5, -6, -8 ], 
  [ 2, 5, 8, -1, -7 ], [ -2 ]>>
gap> Bipartition([[1, 2, 4, 7, 8, -3, -4, -5, -6, -8], 
>  [3, 5, 6, -1, -7], [-2]]) in last;
true
gap> LClassReps(D);
[ <bipartition: [ 1, 2, 4, 7, 8, -3, -4, -5, -6, -8 ], [ 3, 5, 6, -1, -7 ], 
     [ -2 ]> ]
gap> L := LClass(S, Bipartition([[1], [2, 4], [3, 6, -3, -4, -5, -6, -8],
> [5, 7, 8, -1, -7], [-2]]));
<Green's L-class: <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, -3, -4, -5, -6, -8 ]
   , [ 5, 7, 8, -1, -7 ], [ -2 ]>>
gap> Bipartition([[1, 2, 4, 7, 8, -3, -4, -5, -6, -8], 
>  [3, 5, 6, -1, -7], [-2]]) in last;
true
gap> LL := LClassNC(S, Bipartition([[1, 3, 4, 6, 7, -3, -4, -5, -6, -8], [2,
> 5, 8, -1, -7], [-2]]));
<Green's L-class: <bipartition: [ 1, 3, 4, 6, 7, -3, -4, -5, -6, -8 ], 
  [ 2, 5, 8, -1, -7 ], [ -2 ]>>
gap> Bipartition([[1, 3, 4, 6, 7, -3, -4, -5, -6, -8], 
>  [2, 5, 8, -1, -7], [-2]]) in last;
true
gap> LL = L;
true
gap> L = LL;
true
gap> Size(L);
64
gap> Size(LL);
64
gap> x := Bipartition([[1], [2, 4], [3, 6, 8, -1, -3, -5, -7],
>  [5, 7, -4, -6, -8], [-2]]);;
gap> D := DClass(RClassNC(S, x));
<Green's D-class: <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -1, -3, -5, -7 ], 
  [ 5, 7, -4, -6, -8 ], [ -2 ]>>
gap> Bipartition([[1], [2, 4], [3, 6, 8, -1, -3, -5, -7], 
>  [5, 7, -4, -6, -8], [-2]]) in last;
true
gap> GroupHClass(D);
fail
gap> IsRegularDClass(D);
false
gap> D := DClass(S, x);
<Green's D-class: <bipartition: [ 1 ], [ 2, 4 ], [ 3, 6, 8, -1, -3, -5, -7 ], 
  [ 5, 7, -4, -6, -8 ], [ -2 ]>>
gap> Bipartition([[1, 2, 4, 7, 8, -4, -6, -8], 
>  [3, 5, 6, -1, -3, -5, -7], [-2]]) in last;
true
gap> IsRegularDClass(D);
false
gap> x := Bipartition([[1, 7, 8, -2, -5], [2, 3, 5, 6, -1, -3, -4, -6],
> [4], [-7, -8]]);;
gap> IsRegularDClass(DClass(S, x));
false
gap> NrRegularDClasses(S);
4
gap> D := First(DClasses(S), IsRegularDClass);
<Green's D-class: <bipartition: [ 1, 2, 3, 7, -7 ], [ 4, 5, 6, 8 ], 
  [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>>
gap> Bipartition([[1, 2, 3, 7, -7], [4, 5, 6, 8], 
>  [-1, -2], [-3, -6, -8], [-4], [-5]]) in last;
true
gap> Size(D);
12078
gap> H := GroupHClass(D);
<Green's H-class: <bipartition: [ 1, 2, 3, 7, -7 ], [ 4, 5, 6, 8 ], 
  [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>>
gap> Bipartition([[1, 2, 3, 7, -7], [4, 5, 6, 8], 
>  [-1, -2], [-3, -6, -8], [-4], [-5]]) in last;
true
gap> StructureDescription(H);
"1"
gap> D := First(DClasses(S), IsRegularDClass);
<Green's D-class: <bipartition: [ 1, 2, 3, 7, -7 ], [ 4, 5, 6, 8 ], 
  [ -1, -2 ], [ -3, -6, -8 ], [ -4 ], [ -5 ]>>
gap> Bipartition([[1, 2, 3, 7, -7], [4, 5, 6, 8], 
>  [-1, -2], [-3, -6, -8], [-4], [-5]]) in last;
true
gap> NrRClasses(D);
99
gap> NrLClasses(D);
122
gap> R := PrincipalFactor(D);
<Rees 0-matrix semigroup 99x122 over 1>
gap> Length(Idempotents(S, 1));
11209
gap> Length(Idempotents(S, 0));
4218
gap> NrIdempotents(S);
15529
gap> last2 + last3;
15427
gap> Length(Idempotents(S, 2));
102
gap> NrRClasses(D);
99
gap> NrDClasses(S);
190
gap> PartialOrderOfDClasses(S);
<immutable digraph with 190 vertices, 642 edges>
gap> StructureDescriptionMaximalSubgroups(S);
[ "1", "C2" ]
gap> StructureDescriptionSchutzenbergerGroups(S);
[ "1", "C2" ]

# BipartitionTest27: IsomorphismPermGroup for a block bijection group
gap> S := Semigroup(
>  Bipartition([[1, 2, -3], [3, -4], [4, -8], [5, -1, -2],
>      [6, -5], [7, -6], [8, -7]]),
>  Bipartition([[1, 2, -7], [3, -1, -2], [4, -8], [5, -4],
>     [6, -5], [7, -3], [8, -6]]), rec(acting := true));;
gap> iso := IsomorphismPermGroup(S);;
gap> inv := InverseGeneralMapping(iso);;
gap> ForAll(S, x -> x ^ iso in Range(iso));
true
gap> ForAll(S, x -> (x ^ iso) ^ inv = x);
true

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/semibipart.tst");
