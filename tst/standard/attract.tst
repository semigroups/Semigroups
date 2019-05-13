#############################################################################
##
#W  standard/attract.tst
#Y  Copyright (C) 2015                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST(Concatenation("Semigroups package: ",
>                             "standard/attract.tst"));
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();
gap> SEMIGROUPS.DefaultOptionsRec.acting := true;;

# attract: IsMultiplicativeZero
gap> S := InverseSemigroup([PartialPerm([1, 2, 5], [2, 1, 5]),
> PartialPerm([1, 2, 4, 5], [4, 2, 1, 3])]);;
gap> IsMultiplicativeZero(S, PartialPerm([]));
true

# attract: IsGreensDGreaterThanFunc
gap> S := Semigroup([PartialPerm([1, 2, 3], [4, 5, 1]),
>                    PartialPerm([1, 2, 4], [1, 5, 4])]);;
gap> x := PartialPerm([1, 3], [4, 1]);;
gap> y := PartialPerm([1, 4], [1, 4]);;
gap> foo := IsGreensDGreaterThanFunc(S);;
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
gap> foo(x, z);
true
gap> foo(z, x);
false
gap> foo(z, y);
false
gap> foo(y, z);
true

# attract: MaximalDClasses for non-regular semigroup
gap> S := Monoid([Bipartition([[1, -2], [2, -1], [3, -3]]),
> Bipartition([[1], [2], [3], [-1], [-2, -3]])]);;
gap> MaximalDClasses(S);
[ <Green's D-class: <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ]>> ]

# attract: MaximalDClasses for regular semigroup
gap> S := FullTransformationMonoid(3);
<full transformation monoid of degree 3>
gap> MaximalDClasses(S);
[ <Green's D-class: IdentityTransformation> ]

# attract: StructureDescriptionMaximalSubgroups
gap> S := Semigroup([Transformation([1, 3, 4, 1, 3]),
> Transformation([5, 5, 1, 1, 3])]);;
gap> StructureDescriptionSchutzenbergerGroups(S);
[ "1", "C2", "C3" ]

# attract: IdempotentGeneratedSubsemigroup, for a semigroup
gap> S := Semigroup([PartialPerm([1, 2, 3], [2, 5, 3]),
> PartialPerm([1, 2, 3, 4], [2, 4, 1, 5])]);;
gap> IdempotentGeneratedSubsemigroup(S);
<inverse partial perm monoid of rank 1 with 2 generators>

# attract: IdempotentGeneratedSubsemigroup, for an inverse semigroup
gap> S := InverseSemigroup([PartialPerm([1, 2], [4, 3]),
> PartialPerm([1, 2, 5], [1, 2, 4])]);;
gap> IdempotentGeneratedSubsemigroup(S);
<inverse partial perm semigroup of rank 5 with 5 generators>

# attract: InjectionPrincipalFactor 1/6
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

# attract: InjectionPrincipalFactor 2/6
gap> R := PrincipalFactor(DClasses(FullTransformationMonoid(5))[2]);
<Rees 0-matrix semigroup 10x5 over Group([ (1,2,3,4), (1,2) ])>
gap> x := RMSElement(R, 9, (1, 3, 2, 4), 2);;
gap> y := RMSElement(R, 6, (1, 3, 4, 2), 5);;
gap> S := Semigroup(x, y);
<subsemigroup of 10x5 Rees 0-matrix semigroup with 2 generators>
gap> D := DClass(S, RMSElement(R, 6, (1, 3, 4, 2), 5));;
gap> InjectionPrincipalFactor(D);
Error, Semigroups: InjectionPrincipalFactor: usage,
the argument <D> must be a regular D-class,

# attract: InjectionPrincipalFactor 3/6
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

# attract: InjectionPrincipalFactor 4/6
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

# attract: InjectionPrincipalFactor 5/6
gap> D := GreensDClassOfElement(
> Semigroup([
>   Transformation([1, 3, 4, 1, 3]),
>   Transformation([5, 5, 1, 1, 3])]),
> Transformation([1, 4, 1, 1, 4]));;
gap> map := InverseGeneralMapping(InjectionPrincipalFactor(D));;
gap> MultiplicativeZero(Source(map)) ^ map;
fail

# attract: InjectionPrincipalFactor 6/6
gap> S := ReesZeroMatrixSemigroup(Group(()), [[(), 0], [0, ()]]);;
gap> S := Semigroup(RMSElement(S, 2, (), 2),
>                   RMSElement(S, 1, (), 2));;
gap> MaximalSubsemigroups(S);;

# attract: InversesOfSemigroupElement, none, 1/2
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

# attract: InversesOfSemigroupElement, fail, 2/2
gap> S := Semigroup([PartialPerm([1, 2, 3, 4], [1, 2, 5, 3]),
> PartialPerm([1, 2, 3, 4], [2, 4, 1, 5]),
> PartialPerm([1, 2, 4, 5], [2, 3, 1, 5]),
> PartialPerm([1, 2, 3, 5], [4, 1, 3, 5]),
> PartialPerm([1, 2, 3, 5], [4, 3, 5, 1])]);;
gap> x := PartialPerm([1, 2, 3, 5], [5, 2, 6, 4]);;
gap> InversesOfSemigroupElement(S, x);
Error, Semigroups: InversesOfSemigroupElement: usage,
the second arg (a mult. element) must belong to the first arg (a semigroup),

# attract: InversesOfSemigroupElementNC, closed rho orb
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

# attract: InversesOfSemigroupElementNC, non-closed rho orb
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

# attract: MultiplicativeNeutralElement, 1/4
gap> S := Semigroup(Transformation([2, 3, 1]));
<commutative transformation semigroup of degree 3 with 1 generator>
gap> MultiplicativeNeutralElement(S);
IdentityTransformation

# attract: MultiplicativeNeutralElement, 2/4
gap> S := Semigroup(Transformation([1, 2, 1]), Transformation([2, 2, 3]));;
gap> MultiplicativeNeutralElement(S);
fail

# attract: MultiplicativeNeutralElement, 3/4
gap> S := Semigroup(Transformation([1, 4, 6, 2, 5, 3, 7, 8, 9, 9]),
> Transformation([6, 3, 2, 7, 5, 1, 8, 8, 9, 9]));;
gap> MultiplicativeNeutralElement(S);
Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 9 ] )

# attract: MultiplicativeNeutralElement, 4/4
gap> S := Semigroup(Transformation([1, 1, 3]), Transformation([2, 2, 3]));
<transformation semigroup of degree 2 with 2 generators>
gap> MultiplicativeNeutralElement(S);
fail

# attract: MultiplicativeNeutralElement, 5
gap> S := SingularFactorisableDualSymmetricInverseMonoid(3);
<inverse bipartition semigroup ideal of degree 3 with 1 generator>
gap> IsMonoidAsSemigroup(S);
false

# attract: MultiplicativeNeutralElement, 6
gap> S := Semigroup([Transformation([3, 2, 3]),
>                    Transformation([3, 4, 2, 5, 5])]);
<transformation semigroup of degree 5 with 2 generators>
gap> MultiplicativeNeutralElement(S);
fail
gap> S := SemigroupIdeal(S, S.1);
<non-regular transformation semigroup ideal of degree 5 with 1 generator>
gap> MultiplicativeNeutralElement(S);
fail

# attract: MultiplicativeNeutralElement, 7
gap> S := FullTransformationMonoid(3);
<full transformation monoid of degree 3>
gap> S := SemigroupIdeal(S, IdentityTransformation);
<regular transformation semigroup ideal of degree 3 with 1 generator>
gap> MultiplicativeNeutralElement(S);
IdentityTransformation

# attract: MultiplicativeNeutralElement, 8
gap> S := Semigroup([
>  Transformation([2, 3, 1, 4, 4]),
>  Transformation([2, 1, 3, 4, 4]),
>  Transformation([1, 2, 1, 4, 4])]);
<transformation semigroup of degree 5 with 3 generators>
gap> MultiplicativeNeutralElement(S);
Transformation( [ 1, 2, 3, 4, 4 ] )
gap> S := SemigroupIdeal(S, S.1);
<regular transformation semigroup ideal of degree 5 with 1 generator>
gap> MultiplicativeNeutralElement(S);
Transformation( [ 1, 2, 3, 4, 4 ] )

# attract: RepresentativeOfMinimalIdeal, 1/3
gap> S := Semigroup(Transformation([1, 2, 1]), Transformation([2, 2, 3]));;
gap> RepresentativeOfMinimalIdeal(S);
Transformation( [ 2, 2, 2 ] )

# attract: RepresentativeOfMinimalIdeal, 2/3
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

# attract: RepresentativeOfMinimalIdeal, 3/3
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

# SEMIGROUPS_UnbindVariables
gap> Unbind(D);
gap> Unbind(I);
gap> Unbind(R);
gap> Unbind(S);
gap> Unbind(foo);
gap> Unbind(inj);
gap> Unbind(inv);
gap> Unbind(map);
gap> Unbind(x);
gap> Unbind(y);
gap> Unbind(z);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/attract.tst");
