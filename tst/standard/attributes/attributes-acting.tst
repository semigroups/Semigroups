#%T##########################################################################
##
#W  standard/attributes/attributes-acting.tst
#Y  Copyright (C) 2015                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/attributes/attributes-acting.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS_StartTest();
gap> SEMIGROUPS_DefaultOptionsRec.generic := false;;

# attributes-acting: IsMultiplicativeZero
gap> S := InverseSemigroup( [ PartialPerm( [ 1, 2, 5 ], [ 2, 1, 5 ] ),
>  PartialPerm( [ 1, 2, 4, 5 ], [ 4, 2, 1, 3 ] ) ] );;
gap> IsMultiplicativeZero(S, PartialPerm([]));
true

#T# attributes-acting: IsGreensDLeq
gap> S := Semigroup( [ PartialPerm( [ 1, 2, 3 ], [ 4, 5, 1 ] ),
>                      PartialPerm( [ 1, 2, 4 ], [ 1, 5, 4 ] ) ] );;
gap> x := PartialPerm( [ 1, 3 ], [ 4, 1 ] );;
gap> y := PartialPerm( [ 1, 4 ], [ 1, 4 ] );;
gap> foo := IsGreensDLeq(S);;
gap> foo(x, y);
false
gap> foo(y, x);
true
gap> S := InverseSemigroup(S);;
gap> foo := IsGreensDLeq(S);
function( x, y ) ... end
gap> foo(y, x);
true
gap> foo(x, y);
true
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

#T# attributes-acting: MaximalDClasses for non-regular semigroup
gap> S := Monoid( [ Bipartition( [ [ 1, -2 ], [ 2, -1 ], [ 3, -3 ] ] ),
>  Bipartition( [ [ 1 ], [ 2 ], [ 3 ], [ -1 ], [ -2, -3 ] ] ) ] );;
gap> MaximalDClasses(S);
[ <Green's D-class: <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ]>> ]

#T# attributes-acting: MaximalDClasses for regular semigroup
gap> S := FullTransformationMonoid(3);
<regular transformation monoid of size 27, degree 3 with 3 generators>
gap> MaximalDClasses(S);
[ <Green's D-class: IdentityTransformation> ]

#T# attributes-acting: StructureDescriptionMaximalSubgroups
gap> S := Semigroup( [ Transformation( [ 1, 3, 4, 1, 3 ] ),
> Transformation( [ 5, 5, 1, 1, 3 ] ) ] );;
gap> StructureDescriptionSchutzenbergerGroups(S);
[ "1", "C2", "C3" ]

#T# attributes-acting: IdempotentGeneratedSubsemigroup, for a semigroup
gap> S := Semigroup( [ PartialPerm( [ 1, 2, 3 ], [ 2, 5, 3 ] ),
>  PartialPerm( [ 1, 2, 3, 4 ], [ 2, 4, 1, 5 ] ) ] );;
gap> IdempotentGeneratedSubsemigroup(S);
<commutative partial perm monoid of rank 1 with 1 generator>

#T# attributes-acting: IdempotentGeneratedSubsemigroup, for an inverse semigroup
gap> S := InverseSemigroup( [ PartialPerm( [ 1, 2 ], [ 4, 3 ] ),
> PartialPerm( [ 1, 2, 5 ], [ 1, 2, 4 ] ) ] );;
gap> IdempotentGeneratedSubsemigroup(S);
<inverse partial perm semigroup of rank 5 with 5 generators>

#T# attributes-acting: InjectionPrincipalFactor 1/6
gap> D := GreensDClassOfElement(
>  Monoid( [ Bipartition( [ [ 1, 2, -2 ], [ 3, -3 ], [ -1 ] ] ),
>      Bipartition( [ [ 1, 2 ], [ 3 ], [ -1, -3 ], [ -2 ] ] ),
>      Bipartition( [ [ 1, -1, -2 ], [ 2, 3 ], [ -3 ] ] ) ]
>      ), Bipartition( [ [ 1, 2 ], [ 3 ], [ -1, -3 ], [ -2 ] ] ));;
gap> map := InjectionPrincipalFactor(D);
MappingByFunction( <Green's D-class: <bipartition: [ 1, 2 ], [ 3 ], 
  [ -1, -3 ], [ -2 ]>>, <Rees matrix semigroup 3x4 over Group(())>
 , function( x ) ... end, function( x ) ... end )
gap> inv := InverseGeneralMapping(map);;
gap> ForAll(D, x -> (x ^ map) ^ inv = x);
true
gap> Bipartition( [ [ 1, 2, -2 ], [ 3, -3 ], [ -1 ] ] ) ^ map;
fail
gap> Star(Bipartition( [ [ 1, 2, -2 ], [ 3, -3 ], [ -1 ] ] )) ^ map;
fail

#T# attributes-acting: InjectionPrincipalFactor 2/6
gap> R := PrincipalFactor(DClasses(FullTransformationMonoid(5))[2]);
<Rees 0-matrix semigroup 10x5 over Group([ (1,2,3,4), (1,2) ])>
gap> x := RMSElement(R, 9,(1,3,2,4),2);;
gap> y := RMSElement(R, 6,(1,3,4,2),5);;
gap> S := Semigroup(x, y);
<subsemigroup of 10x5 Rees 0-matrix semigroup with 2 generators>
gap> D := DClass(S, RMSElement(R, 6,(1,3,4,2),5));;
gap> InjectionPrincipalFactor(D);
Error, Semigroups: InjectionPrincipalFactor: usage,
the argument <d> must be a regular D-class,

#T# attributes-acting: InjectionPrincipalFactor 3/6
gap> R := PrincipalFactor(DClasses(FullTransformationMonoid(5))[2]);
<Rees 0-matrix semigroup 10x5 over Group([ (1,2,3,4), (1,2) ])>
gap> x := RMSElement(R, 9,(1,3,2,4),2);;
gap> y := RMSElement(R, 6,(1,3,4,2),5);;
gap> S := Semigroup(x, y);;
gap> D := DClass(S, RMSElement(R, 9,(1,3,2,4),2));;
gap> InjectionPrincipalFactor(D);
MappingByFunction( <Green's D-class: (9,(1,3,2,4),2)>, 
<Rees matrix semigroup 1x1 over Group([ (2,3,
4) ])>, function( x ) ... end, function( x ) ... end )

#T# attributes-acting: InjectionPrincipalFactor 4/6
gap> D := GreensDClassOfElement(
>  Semigroup(
>     [ Transformation( [ 1, 3, 4, 1, 3 ] ), Transformation( [ 5, 5, 1, 1, 3 ] ) ]
>     ), Transformation( [ 5, 5, 1, 1, 3 ] ));;
gap> map := InjectionPrincipalFactor(D);
MappingByFunction( <Green's D-class: Transformation( [ 5, 5, 1, 1, 3 ] )>, 
<Rees matrix semigroup 1x1 over Group([ (1,5,
3) ])>, function( x ) ... end, function( x ) ... end )
gap> Transformation( [ 5, 1, 1, 1, 3 ] ) ^ map;
fail

#T# attributes-acting: InjectionPrincipalFactor 5/6
gap> D := GreensDClassOfElement(
>   Semigroup(
>      [ Transformation( [ 1, 3, 4, 1, 3 ] ), Transformation( [ 5, 5, 1, 1, 3 ] ) ]
>     ), Transformation( [ 1, 4, 1, 1, 4 ] ));;
gap> map := InverseGeneralMapping(InjectionPrincipalFactor(D));;
gap> MultiplicativeZero(Source(map)) ^ map;
fail

#T# attributes-acting: InjectionPrincipalFactor 6/6
gap> S := ReesZeroMatrixSemigroup(Group(()), [[(), 0], [0, ()]]);;
gap> S := Semigroup(RMSElement(S, 2, (), 2),
>                   RMSElement(S, 1, (), 2));;
gap> MaximalSubsemigroups(S);;

#T# attributes-acting: InversesOfSemigroupElement, none, 1/2
gap> S := Semigroup( [ Bipartition( [ [ 1, 2, -2 ], [ 3, -3 ], [ -1 ] ] ),
>  Bipartition( [ [ 1, -1, -2 ], [ 2, 3 ], [ -3 ] ] ) ] );;
gap> x := Bipartition( [ [ 1, 2, 3 ], [ -1, -2 ], [ -3 ] ] );;
gap> InversesOfSemigroupElement(S, x);
[  ]

#T# attributes-acting: InversesOfSemigroupElement, fail, 2/2
gap> S := Semigroup( [ PartialPerm( [ 1, 2, 3, 4 ], [ 1, 2, 5, 3 ] ),
>  PartialPerm( [ 1, 2, 3, 4 ], [ 2, 4, 1, 5 ] ),
>  PartialPerm( [ 1, 2, 4, 5 ], [ 2, 3, 1, 5 ] ),
>  PartialPerm( [ 1, 2, 3, 5 ], [ 4, 1, 3, 5 ] ),
>  PartialPerm( [ 1, 2, 3, 5 ], [ 4, 3, 5, 1 ] ) ] );;
gap> x := PartialPerm( [ 1, 2, 3, 5 ], [ 5, 2, 6, 4 ] );;
gap> InversesOfSemigroupElement(S, x);
fail

#T# attributes-acting: InversesOfSemigroupElementNC, closed rho orb
gap> S := Semigroup(
> [ Transformation( [ 2, 2, 13, 14, 3, 4, 15, 19, 22, 17, 22, 22, 11, 12, 18, 22,
>      16, 16, 21, 22, 20, 22, 10 ] ),
>  Transformation( [ 2, 2, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
>     22, 22, 22, 22, 22, 22, 2 ] ),
>  Transformation( [ 1, 1, 5, 6, 9, 9, 9, 9, 9, 5, 5, 6, 9, 9, 7, 7, 8, 9, 8, 8,
>      9, 9, 6 ] ) ] );;
gap> Size(S);;
gap> x := Transformation( [ 1, 1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
> 9, 9, 9, 9, 9, 9 ] );;
gap> InversesOfSemigroupElementNC(S, x);
[ Transformation( [ 2, 2, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
      22, 22, 22, 22, 22, 22, 22, 2 ] ), 
  Transformation( [ 2, 2, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
      22, 22, 22, 22, 22, 22, 22, 22 ] ), 
  Transformation( [ 1, 1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 1 ] ), Transformation( [ 1, 1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
     9, 9, 9, 9, 9, 9, 9, 9, 9, 9 ] ) ]

#T# attributes-acting: InversesOfSemigroupElementNC, non-closed rho orb
gap> S := Semigroup(
> [ Transformation( [ 2, 2, 13, 14, 3, 4, 15, 19, 22, 17, 22, 22, 11, 12, 18, 22,
>      16, 16, 21, 22, 20, 22, 10 ] ),
>  Transformation( [ 2, 2, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
>     22, 22, 22, 22, 22, 22, 2 ] ),
>  Transformation( [ 1, 1, 5, 6, 9, 9, 9, 9, 9, 5, 5, 6, 9, 9, 7, 7, 8, 9, 8, 8,
>      9, 9, 6 ] ) ] );;
gap> x := Transformation( [ 1, 1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
> 9, 9, 9, 9, 9, 9 ] );;
gap> InversesOfSemigroupElementNC(S, x);
[ Transformation( [ 2, 2, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
      22, 22, 22, 22, 22, 22, 22, 2 ] ), 
  Transformation( [ 2, 2, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
      22, 22, 22, 22, 22, 22, 22, 22 ] ), 
  Transformation( [ 1, 1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 1 ] ), Transformation( [ 1, 1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
     9, 9, 9, 9, 9, 9, 9, 9, 9, 9 ] ) ]

#T# attributes-acting: MultiplicativeNeutralElement, 1/4
gap> S := Semigroup(Transformation([2, 3, 1]));
<commutative transformation semigroup of degree 3 with 1 generator>
gap> MultiplicativeNeutralElement(S);
IdentityTransformation

#T# attributes-acting: MultiplicativeNeutralElement, 2/4
gap> S := Semigroup(Transformation([1, 2, 1]), Transformation([2, 2, 3]));;
gap> MultiplicativeNeutralElement(S);
fail

#T# attributes-acting: MultiplicativeNeutralElement, 3/4
gap> S:=Semigroup( Transformation( [ 1, 4, 6, 2, 5, 3, 7, 8, 9, 9 ] ),
> Transformation( [ 6, 3, 2, 7, 5, 1, 8, 8, 9, 9 ] ) );;
gap> MultiplicativeNeutralElement(S);
Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 9 ] )

#T# attributes-acting: MultiplicativeNeutralElement, 4/4
gap> S := Semigroup(Transformation([1, 1, 3]), Transformation([2, 2, 3]));
<transformation semigroup of degree 2 with 2 generators>
gap> MultiplicativeNeutralElement(S);
fail

#T# attributes-acting: RepresentativeOfMinimalIdeal, 1/3
gap> S := Semigroup(Transformation([1, 2, 1]), Transformation([2, 2, 3]));;
gap> RepresentativeOfMinimalIdeal(S);
Transformation( [ 2, 2, 2 ] )

#T# attributes-acting: RepresentativeOfMinimalIdeal, 2/3
gap> S := Semigroup(
> Bipartition( [ [ 1, -2 ], [ 2, -1 ], [ 3, -3 ], [ 4, -4 ], [ 5, -5 ] ] ),
> Bipartition( [ [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], [ 4, -5 ], [ 5, -4 ] ] ),
> Bipartition( [ [ 1, 2, -1 ], [ 3, -3 ], [ 4, -4 ], [ 5, -2 ], [ -5 ] ] ),
> Bipartition( [ [ 1, -1 ], [ 2, 4, -2 ], [ 3, -3 ], [ 5, -4 ], [ -5 ] ] ),
> Bipartition( [ [ 1, 2, -3 ], [ 3, -1 ], [ 4, -4 ], [ 5, -2 ], [ -5 ] ] ),
> Bipartition( [ [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], [ 4, 5, -4 ], [ -5 ] ] ),
> Bipartition( [ [ 1, -1 ], [ 2, -2 ], [ 3, 5, -3 ], [ 4, -4 ], [ -5 ] ] ),
> Bipartition( [ [ 1, 2, -3 ], [ 3, -1 ], [ 4, -4 ], [ 5, -5 ], [ -2 ] ] ) );;
gap> RepresentativeOfMinimalIdeal(S);
<bipartition: [ 1, 2, 4, 5, -1 ], [ 3, -3 ], [ -2 ], [ -4 ], [ -5 ]>

#T# attributes-acting: RepresentativeOfMinimalIdeal, 3/3
gap> S := Semigroup(
> Bipartition( [ [ 1, -2 ], [ 2, -1 ], [ 3, -3 ], [ 4, -4 ], [ 5, -5 ] ] ),
> Bipartition( [ [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], [ 4, -5 ], [ 5, -4 ] ] ),
> Bipartition( [ [ 1, 2, -1 ], [ 3, -3 ], [ 4, -4 ], [ 5, -2 ], [ -5 ] ] ),
> Bipartition( [ [ 1, -1 ], [ 2, 4, -2 ], [ 3, -3 ], [ 5, -4 ], [ -5 ] ] ),
> Bipartition( [ [ 1, 2, -3 ], [ 3, -1 ], [ 4, -4 ], [ 5, -2 ], [ -5 ] ] ),
> Bipartition( [ [ 1, -1 ], [ 2, -2 ], [ 3, -3 ], [ 4, 5, -4 ], [ -5 ] ] ),
> Bipartition( [ [ 1, -1 ], [ 2, -2 ], [ 3, 5, -3 ], [ 4, -4 ], [ -5 ] ] ),
> Bipartition( [ [ 1, 2, -3 ], [ 3, -1 ], [ 4, -4 ], [ 5, -5 ], [ -2 ] ] ) );;
gap> I := SemigroupIdeal(S, RepresentativeOfMinimalIdeal(S));;
gap> RepresentativeOfMinimalIdeal(I);
<bipartition: [ 1, 2, 4, 5, -1 ], [ 3, -3 ], [ -2 ], [ -4 ], [ -5 ]>

#E#
gap> STOP_TEST("Semigroups package: standard/attributes/attributes-acting.tst");
