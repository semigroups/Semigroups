#############################################################################
##
#W  standard/attributes/acting.tst
#Y  Copyright (C) 2015-2022                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local D, I, L, R, S, Y, acting, an, foo, inj, inv, map, x, y, z
gap> START_TEST("Semigroups package: standard/attributes/acting.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();
gap> SEMIGROUPS.DefaultOptionsRec.acting := true;;

# IsMultiplicativeZero
gap> S := InverseSemigroup([PartialPerm([1, 2, 5], [2, 1, 5]),
> PartialPerm([1, 2, 4, 5], [4, 2, 1, 3])]);;
gap> IsMultiplicativeZero(S, PartialPerm([]));
true

# IsGreensDGreaterThanFunc
gap> S := Semigroup([PartialPerm([1, 2, 3], [4, 5, 1]),
>                    PartialPerm([1, 2, 4], [1, 5, 4])]);;
gap> x := PartialPerm([1, 3], [4, 1]);;
gap> y := PartialPerm([1, 4], [1, 4]);;
gap> foo := IsGreensDGreaterThanFunc(S);;
gap> foo(x, x);
false
gap> foo(x, y);
false
gap> foo(y, x);
true
gap> S := InverseSemigroup(S);;
gap> foo := IsGreensDGreaterThanFunc(S);
function( x, y ) ... end
gap> foo(y, x);
false
gap> foo(x, y);
false
gap> z := RepresentativeOfMinimalIdeal(S);
<empty partial perm>
gap> foo(x, x);
false
gap> foo(x, z);
true
gap> foo(z, x);
false
gap> foo(z, y);
false
gap> foo(y, z);
true

# MaximalDClasses for non-regular semigroup
gap> S := Monoid([Bipartition([[1, -2], [2, -1], [3, -3]]),
> Bipartition([[1], [2], [3], [-1], [-2, -3]])]);;
gap> MaximalDClasses(S);
[ <Green's D-class: <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ]>> ]

# MaximalDClasses for regular semigroup
gap> S := FullTransformationMonoid(3);
<full transformation monoid of degree 3>
gap> MaximalDClasses(S);
[ <Green's D-class: IdentityTransformation> ]

# StructureDescriptionMaximalSubgroups
gap> S := Semigroup([Transformation([1, 3, 4, 1, 3]),
> Transformation([5, 5, 1, 1, 3])]);;
gap> StructureDescriptionSchutzenbergerGroups(S);
[ "1", "C2", "C3" ]

# IdempotentGeneratedSubsemigroup, for a semigroup
gap> S := Semigroup([PartialPerm([1, 2, 3], [2, 5, 3]),
> PartialPerm([1, 2, 3, 4], [2, 4, 1, 5])]);;
gap> IdempotentGeneratedSubsemigroup(S);
<inverse partial perm monoid of rank 1 with 2 generators>

# IdempotentGeneratedSubsemigroup, for an inverse semigroup
gap> S := InverseSemigroup([PartialPerm([1, 2], [4, 3]),
> PartialPerm([1, 2, 5], [1, 2, 4])]);;
gap> IdempotentGeneratedSubsemigroup(S);
<inverse partial perm semigroup of rank 5 with 5 generators>

# InjectionPrincipalFactor 1/6
gap> D := GreensDClassOfElement(
>  Monoid([Bipartition([[1, 2, -2], [3, -3], [-1]]),
>    Bipartition([[1, 2], [3], [-1, -3], [-2]]),
>    Bipartition([[1, -1, -2], [2, 3], [-3]])]),
> Bipartition([[1, 2], [3], [-1, -3], [-2]]));;
gap> map := InjectionPrincipalFactor(D);
MappingByFunction( <Green's D-class: <bipartition: [ 1, 2 ], [ 3 ], 
  [ -1, -3 ], [ -2 ]>>, <Rees matrix semigroup 3x4 over Group(())>
 , function( x ) ... end, function( x ) ... end )
gap> inv := InverseGeneralMapping(map);;
gap> ForAll(D, x -> (x ^ map) ^ inv = x);
true
gap> Bipartition([[1, 2, -2], [3, -3], [-1]]) ^ map;
fail
gap> Star(Bipartition([[1, 2, -2], [3, -3], [-1]])) ^ map;
fail

# InjectionPrincipalFactor 2/6
gap> R := PrincipalFactor(DClasses(FullTransformationMonoid(5))[2]);
<Rees 0-matrix semigroup 10x5 over Group([ (1,2,3,4), (1,2) ])>
gap> x := RMSElement(R, 9, (1, 3, 2, 4), 2);;
gap> y := RMSElement(R, 6, (1, 3, 4, 2), 5);;
gap> S := Semigroup(x, y);
<subsemigroup of 10x5 Rees 0-matrix semigroup with 2 generators>
gap> D := DClass(S, RMSElement(R, 6, (1, 3, 4, 2), 5));;
gap> InjectionPrincipalFactor(D);
Error, the argument (a Green's D-class) is not regular

# InjectionPrincipalFactor 3/6
gap> R := PrincipalFactor(DClasses(FullTransformationMonoid(5))[2]);
<Rees 0-matrix semigroup 10x5 over Group([ (1,2,3,4), (1,2) ])>
gap> x := RMSElement(R, 1, (1, 2, 3, 4), 1);;
gap> y := RMSElement(R, 6, (1, 3, 4, 2), 5);;
gap> S := Semigroup(x, y);;
gap> D := DClass(S, x);;
gap> inj := InjectionPrincipalFactor(D);;
gap> Source(inj) = D;
true
gap> Range(inj);
<Rees matrix semigroup 1x1 over Group([ (1,2,3,4) ])>

# InjectionPrincipalFactor 4/6
gap> D := GreensDClassOfElement(
> Semigroup([
>   Transformation([1, 3, 4, 1, 3]),
>   Transformation([5, 5, 1, 1, 3])]),
> Transformation([5, 5, 1, 1, 3]));;
gap> inj := InjectionPrincipalFactor(D);;
gap> Source(inj) = D;
true
gap> Range(inj);
<Rees matrix semigroup 1x1 over Group([ (1,5,3) ])>
gap> Transformation([5, 1, 1, 1, 3]) ^ inj;
fail

# InjectionPrincipalFactor 5/6
gap> D := GreensDClassOfElement(
> Semigroup([
>   Transformation([1, 3, 4, 1, 3]),
>   Transformation([5, 5, 1, 1, 3])]),
> Transformation([1, 4, 1, 1, 4]));;
gap> map := InverseGeneralMapping(InjectionPrincipalFactor(D));;
gap> MultiplicativeZero(Source(map)) ^ map;
fail

# InjectionPrincipalFactor 6/6
gap> S := ReesZeroMatrixSemigroup(Group(()), [[(), 0], [0, ()]]);;
gap> S := Semigroup(RMSElement(S, 2, (), 2),
>                   RMSElement(S, 1, (), 2));;
gap> MaximalSubsemigroups(S);;

# InversesOfSemigroupElement, none, 1/2
# This test gives the wrong result in Semigroups 2.7.1!!!
gap> S := Semigroup([Bipartition([[1, 2, -2], [3, -3], [-1]]),
> Bipartition([[1, -1, -2], [2, 3], [-3]])]);;
gap> x := Bipartition([[1, 2, 3], [-1, -2], [-3]]);;
gap> Y := InversesOfSemigroupElement(S, x);
[ <bipartition: [ 1, 2, 3 ], [ -1, -2 ], [ -3 ]>, 
  <bipartition: [ 1 ], [ 2, 3 ], [ -1, -2 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 3 ], [ -1 ], [ -2 ], [ -3 ]>, 
  <bipartition: [ 1 ], [ 2, 3 ], [ -1 ], [ -2 ], [ -3 ]> ]
gap> ForAll(Y, y -> y in S);
true
gap> ForAll(Y, y -> x * y * x = x and y * x * y = y);
true
gap> Set(Y) = Set(Filtered(AsList(S), y -> x * y * x = x and y * x * y = y));
true

# InversesOfSemigroupElement, fail, 2/2
gap> S := Semigroup([PartialPerm([1, 2, 3, 4], [1, 2, 5, 3]),
> PartialPerm([1, 2, 3, 4], [2, 4, 1, 5]),
> PartialPerm([1, 2, 4, 5], [2, 3, 1, 5]),
> PartialPerm([1, 2, 3, 5], [4, 1, 3, 5]),
> PartialPerm([1, 2, 3, 5], [4, 3, 5, 1])]);;
gap> x := PartialPerm([1, 2, 3, 5], [5, 2, 6, 4]);;
gap> InversesOfSemigroupElement(S, x);
Error, the 2nd argument (a mult. element) must belong to the 1st argument (a s\
emigroup)

# InversesOfSemigroupElementNC, closed rho orb
gap> S := Semigroup([
> Transformation([2, 2, 13, 14, 3, 4, 15, 19, 22, 17, 22, 22, 11, 12, 18, 22,
>                 16, 16, 21, 22, 20, 22, 10]),
> Transformation([2, 2, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
>                 22, 22, 22, 22, 22, 22, 2]),
> Transformation([1, 1, 5, 6, 9, 9, 9, 9, 9, 5, 5, 6, 9, 9, 7, 7, 8, 9, 8, 8,
>                 9, 9, 6])]);;
gap> Size(S);;
gap> x := Transformation([1, 1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
>                         9, 9, 9, 9, 9, 9]);;
gap> InversesOfSemigroupElementNC(S, x);
[ Transformation( [ 2, 2, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
      22, 22, 22, 22, 22, 22, 22, 2 ] ), 
  Transformation( [ 2, 2, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
      22, 22, 22, 22, 22, 22, 22, 22 ] ), 
  Transformation( [ 1, 1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 1 ] ), Transformation( [ 1, 1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
     9, 9, 9, 9, 9, 9, 9, 9, 9, 9 ] ) ]

# InversesOfSemigroupElementNC, non-closed rho orb
gap> S := Semigroup([
> Transformation([2, 2, 13, 14, 3, 4, 15, 19, 22, 17, 22, 22, 11, 12, 18, 22,
>                 16, 16, 21, 22, 20, 22, 10]),
> Transformation([2, 2, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
>                 22, 22, 22, 22, 22, 22, 2]),
>  Transformation([1, 1, 5, 6, 9, 9, 9, 9, 9, 5, 5, 6, 9, 9, 7, 7, 8, 9, 8, 8,
>                  9, 9, 6])]);;
gap> x := Transformation([1, 1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
>                         9, 9, 9, 9, 9, 9]);;
gap> InversesOfSemigroupElementNC(S, x);
[ Transformation( [ 2, 2, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
      22, 22, 22, 22, 22, 22, 22, 2 ] ), 
  Transformation( [ 2, 2, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
      22, 22, 22, 22, 22, 22, 22, 22 ] ), 
  Transformation( [ 1, 1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 1 ] ), Transformation( [ 1, 1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
     9, 9, 9, 9, 9, 9, 9, 9, 9, 9 ] ) ]

# InversesOfSemigroupElementNC non-regular element
gap> S := Semigroup(Transformation([2, 4, 3, 4]),
>                   Transformation([3, 3, 2, 3, 3]),
>                   Transformation([5, 5, 5, 4, 4]),
>                   Transformation([5, 1, 4, 1, 1]),
>                   Transformation([5, 3, 3, 4, 5]));;
gap> IsRegularSemigroup(S);
false
gap> x := Transformation([5, 1, 4, 1, 1]);
Transformation( [ 5, 1, 4, 1, 1 ] )
gap> IsRegularSemigroupElement(S, x);
false
gap> InversesOfSemigroupElementNC(S, x);
[  ]

# MultiplicativeNeutralElement, 1/4
gap> S := Semigroup(Transformation([2, 3, 1]));
<commutative transformation semigroup of degree 3 with 1 generator>
gap> MultiplicativeNeutralElement(S);
IdentityTransformation

# MultiplicativeNeutralElement, 2/4
gap> S := Semigroup(Transformation([1, 2, 1]), Transformation([2, 2, 3]));;
gap> MultiplicativeNeutralElement(S);
fail

# MultiplicativeNeutralElement, 3/4
gap> S := Semigroup(Transformation([1, 4, 6, 2, 5, 3, 7, 8, 9, 9]),
> Transformation([6, 3, 2, 7, 5, 1, 8, 8, 9, 9]));;
gap> MultiplicativeNeutralElement(S);
Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 9 ] )

# MultiplicativeNeutralElement, 4/4
gap> S := Semigroup(Transformation([1, 1, 3]), Transformation([2, 2, 3]));
<transformation semigroup of degree 2 with 2 generators>
gap> MultiplicativeNeutralElement(S);
fail

# MultiplicativeNeutralElement, 5
gap> S := SingularFactorisableDualSymmetricInverseMonoid(3);
<inverse bipartition semigroup ideal of degree 3 with 1 generator>
gap> IsMonoidAsSemigroup(S);
false

# MultiplicativeNeutralElement, 6
gap> S := Semigroup([Transformation([3, 2, 3]),
>                    Transformation([3, 4, 2, 5, 5])]);
<transformation semigroup of degree 5 with 2 generators>
gap> MultiplicativeNeutralElement(S);
fail
gap> S := SemigroupIdeal(S, S.1);
<non-regular transformation semigroup ideal of degree 5 with 1 generator>
gap> MultiplicativeNeutralElement(S);
fail

# MultiplicativeNeutralElement, 7
gap> S := FullTransformationMonoid(3);
<full transformation monoid of degree 3>
gap> S := SemigroupIdeal(S, IdentityTransformation);
<regular transformation semigroup ideal of degree 3 with 1 generator>
gap> MultiplicativeNeutralElement(S);
IdentityTransformation

# MultiplicativeNeutralElement, 8
gap> S := Semigroup(
>  Transformation([2, 3, 1, 4, 4]),
>  Transformation([2, 1, 3, 4, 4]),
>  Transformation([1, 2, 1, 4, 4]));
<transformation semigroup of degree 5 with 3 generators>
gap> MultiplicativeNeutralElement(S);
Transformation( [ 1, 2, 3, 4, 4 ] )
gap> S := SemigroupIdeal(S, S.1);
<regular transformation semigroup ideal of degree 5 with 1 generator>
gap> MultiplicativeNeutralElement(S);
Transformation( [ 1, 2, 3, 4, 4 ] )

# RepresentativeOfMinimalIdeal, 1/3
gap> S := Semigroup(Transformation([1, 2, 1]), Transformation([2, 2, 3]));;
gap> RepresentativeOfMinimalIdeal(S);
Transformation( [ 2, 2, 2 ] )

# RepresentativeOfMinimalIdeal, 2/3
gap> S := Semigroup(
> Bipartition([[1, -2], [2, -1], [3, -3], [4, -4], [5, -5]]),
> Bipartition([[1, -1], [2, -2], [3, -3], [4, -5], [5, -4]]),
> Bipartition([[1, 2, -1], [3, -3], [4, -4], [5, -2], [-5]]),
> Bipartition([[1, -1], [2, 4, -2], [3, -3], [5, -4], [-5]]),
> Bipartition([[1, 2, -3], [3, -1], [4, -4], [5, -2], [-5]]),
> Bipartition([[1, -1], [2, -2], [3, -3], [4, 5, -4], [-5]]),
> Bipartition([[1, -1], [2, -2], [3, 5, -3], [4, -4], [-5]]),
> Bipartition([[1, 2, -3], [3, -1], [4, -4], [5, -5], [-2]]));;
gap> RepresentativeOfMinimalIdeal(S);
<bipartition: [ 1, 2, 4, 5, -1 ], [ 3, -3 ], [ -2 ], [ -4 ], [ -5 ]>

# RepresentativeOfMinimalIdeal, 3/3
gap> S := Semigroup(
> Bipartition([[1, -2], [2, -1], [3, -3], [4, -4], [5, -5]]),
> Bipartition([[1, -1], [2, -2], [3, -3], [4, -5], [5, -4]]),
> Bipartition([[1, 2, -1], [3, -3], [4, -4], [5, -2], [-5]]),
> Bipartition([[1, -1], [2, 4, -2], [3, -3], [5, -4], [-5]]),
> Bipartition([[1, 2, -3], [3, -1], [4, -4], [5, -2], [-5]]),
> Bipartition([[1, -1], [2, -2], [3, -3], [4, 5, -4], [-5]]),
> Bipartition([[1, -1], [2, -2], [3, 5, -3], [4, -4], [-5]]),
> Bipartition([[1, 2, -3], [3, -1], [4, -4], [5, -5], [-2]]));;
gap> I := SemigroupIdeal(S, RepresentativeOfMinimalIdeal(S));;
gap> RepresentativeOfMinimalIdeal(I);
<bipartition: [ 1, 2, 4, 5, -1 ], [ 3, -3 ], [ -2 ], [ -4 ], [ -5 ]>

# Left/RightIdentity
gap> S := Semigroup(Transformation([2, 4, 3, 4]),
>                   Transformation([3, 3, 2, 3, 3]),
>                   Transformation([5, 5, 5, 4, 4]),
>                   Transformation([5, 1, 4, 1, 1]),
>                   Transformation([5, 3, 3, 4, 5]));;
gap> ForAll(S, x -> RightIdentity(S, x) = fail or x * RightIdentity(S, x) = x);
true
gap> ForAll(S, x -> RightIdentity(S, x) = fail or RightIdentity(S, x) in S);
true
gap> ForAll(S, x -> LeftIdentity(S, x) = fail or LeftIdentity(S, x) * x = x);
true
gap> ForAll(S, x -> LeftIdentity(S, x) = fail or LeftIdentity(S, x) in S);
true
gap> L := Filtered(S, x -> LeftIdentity(S, x) = fail);
[ Transformation( [ 2, 4, 3, 4 ] ), Transformation( [ 5, 5, 5, 4, 4 ] ), 
  Transformation( [ 5, 1, 4, 1, 1 ] ), Transformation( [ 5, 2, 4, 2, 2 ] ), 
  Transformation( [ 5, 4, 4, 4, 4 ] ), Transformation( [ 5, 3, 4, 3, 3 ] ) ]
gap> Length(L) = 6;
true
gap> ForAll(L, y -> ForAll(S, x -> x * y <> y));
true
gap> ForAll(L, y -> ForAll(S, x -> x * y <> y));
true
gap> R := Filtered(S, x -> RightIdentity(S, x) = fail);
[ Transformation( [ 2, 4, 3, 4 ] ), Transformation( [ 5, 1, 4, 1, 1 ] ), 
  Transformation( [ 5, 2, 4, 2, 2 ] ) ]
gap> Length(R) = 3;
true
gap> ForAll(R, y -> ForAll(S, x -> y * x <> y));
true
gap> RightIdentity(S, Transformation([7, 6, 8, 10, 5, 5, 9, 2, 7, 8]));
Error, the 2nd argument (a mult. elt.) does not belong to the 1st argument (a \
semigroup)
gap> LeftIdentity(S, Transformation([7, 6, 8, 10, 5, 5, 9, 2, 7, 8]));
Error, the 2nd argument (a mult. elt.) does not belong to the 1st argument (a \
semigroup)
gap> S := Semigroup(Transformation([1, 2, 3, 3]), Transformation([2, 3, 1, 1]));
<transformation semigroup of degree 4 with 2 generators>
gap> IsMonoidAsSemigroup(S);
true
gap> RightIdentity(S, Transformation([3, 1, 2, 2])) = MultiplicativeNeutralElement(S);
true
gap> LeftIdentity(S, Transformation([3, 1, 2, 2])) = MultiplicativeNeutralElement(S);
true
gap> S := Monoid(Transformation([1, 2, 3, 3]), Transformation([2, 3, 1, 1]));
<transformation monoid of degree 4 with 2 generators>
gap> RightIdentity(S, Transformation([3, 1, 2, 2])) = One(S);
true
gap> LeftIdentity(S, Transformation([3, 1, 2, 2])) = One(S);
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/attributes/acting.tst");
