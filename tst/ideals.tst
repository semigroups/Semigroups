#############################################################################
##
#W  ideals.tst
#Y  Copyright (C) 2013-14                                James D. Mitchell
##                                                       Julius Jonusas 
##                                                       Wilf Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: ideals.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#
gap> gens:=[ Transformation( [ 2, 6, 1, 7, 5, 3, 4 ] ), 
>   Transformation( [ 5, 3, 7, 2, 1, 6, 4 ] ), 
>   Transformation( [ 2, 5, 5, 3, 4, 2, 3 ] ), 
>   Transformation( [ 1, 5, 1, 6, 1, 5, 6 ] ), 
>   Transformation( [ 6, 2, 2, 2, 5, 1, 2 ] ), 
>   Transformation( [ 7, 5, 4, 4, 4, 5, 5 ] ), 
>   Transformation( [ 5, 1, 6, 1, 1, 5, 1 ] ), 
>   Transformation( [ 3, 5, 2, 3, 2, 2, 3 ] ) ];;
gap> S:=Semigroup(gens);;
gap> I:=SemigroupIdeal(S, S.8);;
gap> x:=S.8*S.1*S.4*S.3;;
gap> R:=RClass(I, x);;
gap> x in R;
true
gap> I:=MinimalIdeal(I);
<simple transformation semigroup ideal on 7 pts with 1 generator>
gap> IsRegularSemigroup(I);
true
gap> Idempotents(I, 0);
[  ]
gap> Idempotents(I);
[ Transformation( [ 6, 6, 6, 6, 6, 6, 6 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 7, 7, 7, 7, 7, 7, 7 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 1, 1 ] ), 
  Transformation( [ 4, 4, 4, 4, 4, 4, 4 ] ), 
  Transformation( [ 5, 5, 5, 5, 5, 5, 5 ] ), 
  Transformation( [ 3, 3, 3, 3, 3, 3, 3 ] ) ]
gap> Idempotents(I, 2);
[  ]
gap> x in R;
true
gap> I:=MinimalIdeal(I);
<simple transformation semigroup ideal on 7 pts with 1 generator>
gap> IsRegularSemigroup(I);;
gap> Idempotents(I, 0);
[  ]
gap> Idempotents(I);
[ Transformation( [ 6, 6, 6, 6, 6, 6, 6 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 7, 7, 7, 7, 7, 7, 7 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 1, 1 ] ), 
  Transformation( [ 4, 4, 4, 4, 4, 4, 4 ] ), 
  Transformation( [ 5, 5, 5, 5, 5, 5, 5 ] ), 
  Transformation( [ 3, 3, 3, 3, 3, 3, 3 ] ) ]
gap> Idempotents(I, 2);
[  ]
gap> S:=Semigroup([ Transformation( [ 1, 3, 4, 1, 3, 5 ] ), 
>   Transformation( [ 1, 5, 3, 5, 5, 5 ] ), 
>   Transformation( [ 2, 4, 6, 1, 6, 5 ] ), 
>   Transformation( [ 3, 2, 4, 2, 3, 3 ] ), 
>   Transformation( [ 4, 1, 2, 6, 2, 1 ] ), 
>   Transformation( [ 4, 6, 4, 3, 3, 3 ] ), 
>   Transformation( [ 4, 6, 5, 5, 2, 6 ] ), 
>   Transformation( [ 5, 1, 6, 1, 6, 3 ] ), 
>   Transformation( [ 5, 2, 5, 3, 5, 3 ] ), 
>   Transformation( [ 6, 4, 5, 5, 1, 6 ] ) ]);
<transformation semigroup on 6 pts with 10 generators>
gap> I:=SemigroupIdeal(S, Representative(DClasses(S)[3]));
<non-regular transformation semigroup ideal on 6 pts with 1 generator>
gap> S:=JonesMonoid(6);
<regular bipartition monoid on 6 pts with 5 generators>
gap> I:=SemigroupIdeal(S, Random(S));
<regular bipartition semigroup ideal on 6 pts with 1 generator>
gap> InversesOfSemigroupElement(I, Random(I));
[ <bipartition: [ 1, 2 ], [ 3, 4 ], [ 5, 6 ], [ -1, -2 ], [ -3, -4 ], 
     [ -5, -6 ]>, <bipartition: [ 1, 4 ], [ 2, 3 ], [ 5, 6 ], [ -1, -2 ], 
     [ -3, -4 ], [ -5, -6 ]>, 
  <bipartition: [ 1, 2 ], [ 3, 6 ], [ 4, 5 ], [ -1, -2 ], [ -3, -4 ], 
     [ -5, -6 ]>, <bipartition: [ 1, 6 ], [ 2, 3 ], [ 4, 5 ], [ -1, -2 ], 
     [ -3, -4 ], [ -5, -6 ]>, 
  <bipartition: [ 1, 6 ], [ 2, 5 ], [ 3, 4 ], [ -1, -2 ], [ -3, -4 ], 
     [ -5, -6 ]>, <bipartition: [ 1, 2 ], [ 3, 4 ], [ 5, 6 ], [ -1, -4 ], 
     [ -2, -3 ], [ -5, -6 ]>, 
  <bipartition: [ 1, 4 ], [ 2, 3 ], [ 5, 6 ], [ -1, -4 ], [ -2, -3 ], 
     [ -5, -6 ]>, <bipartition: [ 1, 2 ], [ 3, 6 ], [ 4, 5 ], [ -1, -4 ], 
     [ -2, -3 ], [ -5, -6 ]>, 
  <bipartition: [ 1, 6 ], [ 2, 3 ], [ 4, 5 ], [ -1, -4 ], [ -2, -3 ], 
     [ -5, -6 ]>, <bipartition: [ 1, 6 ], [ 2, 5 ], [ 3, 4 ], [ -1, -4 ], 
     [ -2, -3 ], [ -5, -6 ]>, 
  <bipartition: [ 1, 2 ], [ 3, 4 ], [ 5, 6 ], [ -1, -2 ], [ -3, -6 ], 
     [ -4, -5 ]>, <bipartition: [ 1, 4 ], [ 2, 3 ], [ 5, 6 ], [ -1, -2 ], 
     [ -3, -6 ], [ -4, -5 ]>, 
  <bipartition: [ 1, 2 ], [ 3, 6 ], [ 4, 5 ], [ -1, -2 ], [ -3, -6 ], 
     [ -4, -5 ]>, <bipartition: [ 1, 6 ], [ 2, 3 ], [ 4, 5 ], [ -1, -2 ], 
     [ -3, -6 ], [ -4, -5 ]>, 
  <bipartition: [ 1, 6 ], [ 2, 5 ], [ 3, 4 ], [ -1, -2 ], [ -3, -6 ], 
     [ -4, -5 ]>, <bipartition: [ 1, 2 ], [ 3, 4 ], [ 5, 6 ], [ -1, -6 ], 
     [ -2, -3 ], [ -4, -5 ]>, 
  <bipartition: [ 1, 4 ], [ 2, 3 ], [ 5, 6 ], [ -1, -6 ], [ -2, -3 ], 
     [ -4, -5 ]>, <bipartition: [ 1, 2 ], [ 3, 6 ], [ 4, 5 ], [ -1, -6 ], 
     [ -2, -3 ], [ -4, -5 ]>, 
  <bipartition: [ 1, 6 ], [ 2, 3 ], [ 4, 5 ], [ -1, -6 ], [ -2, -3 ], 
     [ -4, -5 ]>, <bipartition: [ 1, 6 ], [ 2, 5 ], [ 3, 4 ], [ -1, -6 ], 
     [ -2, -3 ], [ -4, -5 ]>, 
  <bipartition: [ 1, 2 ], [ 3, 4 ], [ 5, 6 ], [ -1, -6 ], [ -2, -5 ], 
     [ -3, -4 ]>, <bipartition: [ 1, 4 ], [ 2, 3 ], [ 5, 6 ], [ -1, -6 ], 
     [ -2, -5 ], [ -3, -4 ]>, 
  <bipartition: [ 1, 2 ], [ 3, 6 ], [ 4, 5 ], [ -1, -6 ], [ -2, -5 ], 
     [ -3, -4 ]>, <bipartition: [ 1, 6 ], [ 2, 3 ], [ 4, 5 ], [ -1, -6 ], 
     [ -2, -5 ], [ -3, -4 ]>, 
  <bipartition: [ 1, 6 ], [ 2, 5 ], [ 3, 4 ], [ -1, -6 ], [ -2, -5 ], 
     [ -3, -4 ]> ]
gap> InversesOfSemigroupElement(I, Random(I));
[ <bipartition: [ 1, 2 ], [ 3, 4 ], [ 5, -5 ], [ 6, -6 ], [ -1, -2 ], 
     [ -3, -4 ]>, <bipartition: [ 1, 2 ], [ 3, -5 ], [ 4, 5 ], [ 6, -6 ], 
     [ -1, -2 ], [ -3, -4 ]>, 
  <bipartition: [ 1, 2 ], [ 3, -5 ], [ 4, -6 ], [ 5, 6 ], [ -1, -2 ], 
     [ -3, -4 ]>, <bipartition: [ 1, -5 ], [ 2, 3 ], [ 4, 5 ], [ 6, -6 ], 
     [ -1, -2 ], [ -3, -4 ]>, 
  <bipartition: [ 1, -5 ], [ 2, 3 ], [ 4, -6 ], [ 5, 6 ], [ -1, -2 ], 
     [ -3, -4 ]>, <bipartition: [ 1, -5 ], [ 2, -6 ], [ 3, 4 ], [ 5, 6 ], 
     [ -1, -2 ], [ -3, -4 ]>, 
  <bipartition: [ 1, 4 ], [ 2, 3 ], [ 5, -5 ], [ 6, -6 ], [ -1, -2 ], 
     [ -3, -4 ]>, <bipartition: [ 1, -5 ], [ 2, 5 ], [ 3, 4 ], [ 6, -6 ], 
     [ -1, -2 ], [ -3, -4 ]>, 
  <bipartition: [ 1, -5 ], [ 2, -6 ], [ 3, 6 ], [ 4, 5 ], [ -1, -2 ], 
     [ -3, -4 ]>, <bipartition: [ 1, 2 ], [ 3, 4 ], [ 5, -3 ], [ 6, -6 ], 
     [ -1, -2 ], [ -4, -5 ]>, 
  <bipartition: [ 1, 2 ], [ 3, -3 ], [ 4, 5 ], [ 6, -6 ], [ -1, -2 ], 
     [ -4, -5 ]>, <bipartition: [ 1, 2 ], [ 3, -3 ], [ 4, -6 ], [ 5, 6 ], 
     [ -1, -2 ], [ -4, -5 ]>, 
  <bipartition: [ 1, -3 ], [ 2, 3 ], [ 4, 5 ], [ 6, -6 ], [ -1, -2 ], 
     [ -4, -5 ]>, <bipartition: [ 1, -3 ], [ 2, 3 ], [ 4, -6 ], [ 5, 6 ], 
     [ -1, -2 ], [ -4, -5 ]>, 
  <bipartition: [ 1, -3 ], [ 2, -6 ], [ 3, 4 ], [ 5, 6 ], [ -1, -2 ], 
     [ -4, -5 ]>, <bipartition: [ 1, 4 ], [ 2, 3 ], [ 5, -3 ], [ 6, -6 ], 
     [ -1, -2 ], [ -4, -5 ]>, 
  <bipartition: [ 1, -3 ], [ 2, 5 ], [ 3, 4 ], [ 6, -6 ], [ -1, -2 ], 
     [ -4, -5 ]>, <bipartition: [ 1, -3 ], [ 2, -6 ], [ 3, 6 ], [ 4, 5 ], 
     [ -1, -2 ], [ -4, -5 ]>, 
  <bipartition: [ 1, 2 ], [ 3, 4 ], [ 5, -1 ], [ 6, -6 ], [ -2, -3 ], 
     [ -4, -5 ]>, <bipartition: [ 1, 2 ], [ 3, -1 ], [ 4, 5 ], [ 6, -6 ], 
     [ -2, -3 ], [ -4, -5 ]>, 
  <bipartition: [ 1, 2 ], [ 3, -1 ], [ 4, -6 ], [ 5, 6 ], [ -2, -3 ], 
     [ -4, -5 ]>, <bipartition: [ 1, -1 ], [ 2, 3 ], [ 4, 5 ], [ 6, -6 ], 
     [ -2, -3 ], [ -4, -5 ]>, 
  <bipartition: [ 1, -1 ], [ 2, 3 ], [ 4, -6 ], [ 5, 6 ], [ -2, -3 ], 
     [ -4, -5 ]>, <bipartition: [ 1, -1 ], [ 2, -6 ], [ 3, 4 ], [ 5, 6 ], 
     [ -2, -3 ], [ -4, -5 ]>, 
  <bipartition: [ 1, 4 ], [ 2, 3 ], [ 5, -1 ], [ 6, -6 ], [ -2, -3 ], 
     [ -4, -5 ]>, <bipartition: [ 1, -1 ], [ 2, 5 ], [ 3, 4 ], [ 6, -6 ], 
     [ -2, -3 ], [ -4, -5 ]>, 
  <bipartition: [ 1, -1 ], [ 2, -6 ], [ 3, 6 ], [ 4, 5 ], [ -2, -3 ], 
     [ -4, -5 ]>, <bipartition: [ 1, 2 ], [ 3, 4 ], [ 5, -5 ], [ 6, -6 ], 
     [ -1, -4 ], [ -2, -3 ]>, 
  <bipartition: [ 1, 2 ], [ 3, -5 ], [ 4, 5 ], [ 6, -6 ], [ -1, -4 ], 
     [ -2, -3 ]>, <bipartition: [ 1, 2 ], [ 3, -5 ], [ 4, -6 ], [ 5, 6 ], 
     [ -1, -4 ], [ -2, -3 ]>, 
  <bipartition: [ 1, -5 ], [ 2, 3 ], [ 4, 5 ], [ 6, -6 ], [ -1, -4 ], 
     [ -2, -3 ]>, <bipartition: [ 1, -5 ], [ 2, 3 ], [ 4, -6 ], [ 5, 6 ], 
     [ -1, -4 ], [ -2, -3 ]>, 
  <bipartition: [ 1, -5 ], [ 2, -6 ], [ 3, 4 ], [ 5, 6 ], [ -1, -4 ], 
     [ -2, -3 ]>, <bipartition: [ 1, 4 ], [ 2, 3 ], [ 5, -5 ], [ 6, -6 ], 
     [ -1, -4 ], [ -2, -3 ]>, 
  <bipartition: [ 1, -5 ], [ 2, 5 ], [ 3, 4 ], [ 6, -6 ], [ -1, -4 ], 
     [ -2, -3 ]>, <bipartition: [ 1, -5 ], [ 2, -6 ], [ 3, 6 ], [ 4, 5 ], 
     [ -1, -4 ], [ -2, -3 ]>, 
  <bipartition: [ 1, 2 ], [ 3, 4 ], [ 5, -1 ], [ 6, -6 ], [ -2, -5 ], 
     [ -3, -4 ]>, <bipartition: [ 1, 2 ], [ 3, -1 ], [ 4, 5 ], [ 6, -6 ], 
     [ -2, -5 ], [ -3, -4 ]>, 
  <bipartition: [ 1, 2 ], [ 3, -1 ], [ 4, -6 ], [ 5, 6 ], [ -2, -5 ], 
     [ -3, -4 ]>, <bipartition: [ 1, -1 ], [ 2, 3 ], [ 4, 5 ], [ 6, -6 ], 
     [ -2, -5 ], [ -3, -4 ]>, 
  <bipartition: [ 1, -1 ], [ 2, 3 ], [ 4, -6 ], [ 5, 6 ], [ -2, -5 ], 
     [ -3, -4 ]>, <bipartition: [ 1, -1 ], [ 2, -6 ], [ 3, 4 ], [ 5, 6 ], 
     [ -2, -5 ], [ -3, -4 ]>, 
  <bipartition: [ 1, 4 ], [ 2, 3 ], [ 5, -1 ], [ 6, -6 ], [ -2, -5 ], 
     [ -3, -4 ]>, <bipartition: [ 1, -1 ], [ 2, 5 ], [ 3, 4 ], [ 6, -6 ], 
     [ -2, -5 ], [ -3, -4 ]>, 
  <bipartition: [ 1, -1 ], [ 2, -6 ], [ 3, 6 ], [ 4, 5 ], [ -2, -5 ], 
     [ -3, -4 ]> ]
gap> S:=SymmetricInverseMonoid(8);;
gap> x:=PartialPerm([1]);;
gap> I:=SemigroupIdeal(S, x);
<inverse partial perm semigroup ideal on 8 pts with 1 generator>
gap> IsZeroSimpleSemigroup(I);
true

#
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ), 
>   Transformation( [ 4, 2, 1, 5, 5 ] ), 
>   Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> I := SemigroupIdeal(s, gens);
<regular transformation semigroup ideal on 5 pts with 3 generators>
gap> data := SemigroupData(I);
<closed semigroup ideal data with 26 reps, 23 lambda-values, 26 rho-values>
gap> Size(I);
731
gap> NrDClasses(I);
4
gap> GreensDClasses(I);
[ {Transformation( [ 3, 4, 1, 2, 1 ] )}, {Transformation( [ 4, 2, 2, 2, 4 ] )}
    , {Transformation( [ 4, 5, 2, 4, 4 ] )}, 
  {Transformation( [ 2, 2, 2, 2, 2 ] )} ]

#
gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ), 
> Transformation( [ 2, 4, 1, 2 ] ), 
> Transformation( [ 3, 1, 1, 3 ] ), 
> Transformation( [ 3, 3, 4, 1 ] ) ];;
gap> s:=Semigroup(gens);;
gap> I := SemigroupIdeal(s, gens{[1,2]});
<non-regular transformation semigroup ideal on 4 pts with 2 generators>
gap> o := LambdaOrb(I);
<closed ideal lambda orbit with 11 points in 2 components>

#
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ), 
>  Transformation( [ 1, 4, 1, 2 ] ), 
>  Transformation( [ 2, 4, 1, 1 ] ), 
>  Transformation( [ 3, 4, 2, 2 ] ) ];; 
gap> s:=Semigroup(gens);;
gap> I := SemigroupIdeal(s, [gens[2]* gens[1], gens[3]^3]);
<non-regular transformation semigroup ideal on 4 pts with 2 generators>
gap> o := RhoOrb(I);
<closed ideal rho orbit with 10 points in 2 components>

#
gap> gens := [                                                              
> PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 10 ], [ 4, 6, 7, 3, 8, 2, 9, 5 ] ),
> PartialPerm( [ 1, 2, 7, 9 ], [ 5, 6, 4, 3 ] ),
> PartialPerm( [ 1, 2, 6, 7, 8 ], [ 5, 1, 6, 2, 3 ] )];;
gap> s := Semigroup(gens);
<partial perm semigroup on 10 pts with 3 generators>
gap> I := SemigroupIdeal(s, [gens[1]^2, gens[2]]);
<non-regular partial perm semigroup ideal on 10 pts with 2 generators>
gap> R := GreensRClassOfElement(I, gens[1]^2);
{PartialPerm( [ 1, 2, 3, 4, 6, 10 ], [ 3, 2, 9, 7, 6, 8 ] )}
gap> DClassOfRClass(R);
{PartialPerm( [ 1, 2, 3, 4, 6, 10 ], [ 3, 2, 9, 7, 6, 8 ] )}
gap> L := GreensLClassOfElement(I, gens[1]^2);
{PartialPerm( [ 1, 2, 3, 4, 6, 10 ], [ 3, 2, 9, 7, 6, 8 ] )}
gap> DClassOfLClass(L);
{PartialPerm( [ 1, 2, 3, 4, 6, 10 ], [ 3, 2, 9, 7, 6, 8 ] )}

# \in for an inverse op semigroup ideal
gap> S:=InverseSemigroup(
> PartialPerm( [ 1, 2, 3, 5, 6, 7, 8 ], [ 5, 9, 10, 6, 3, 8, 4 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5, 6, 8 ], [ 6, 2, 8, 4, 7, 5, 3 ] ), 
>  PartialPerm( [ 1, 2, 4, 6, 8, 9 ], [ 7, 10, 1, 9, 4, 2 ] ), 
>  PartialPerm( [ 1, 2, 4, 7, 8, 9 ], [ 10, 7, 8, 5, 9, 1 ] ), 
>  PartialPerm( [ 1, 2, 3, 6, 8, 10 ], [ 2, 6, 7, 9, 1, 5 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5, 8, 10 ], [ 3, 1, 4, 2, 5, 6, 7 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 6, 7, 8, 10 ], [ 3, 8, 1, 9, 4, 10, 5, 6 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 10 ], [ 4, 7, 6, 9, 10, 1, 3, 2 ] ), 
>  PartialPerm( [ 1, 2, 4, 5, 8, 10 ], [ 6, 2, 7, 8, 10, 4 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5, 8, 10 ], [ 7, 1, 4, 3, 2, 6, 5 ] ) );;
gap> I:=SemigroupIdeal(S, 
> [ PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 10 ], [ 4, 7, 6, 9, 10, 1, 3, 2 ] ), 
>   PartialPerm( [ 4 ], [ 3 ] ), PartialPerm( [ 6, 7 ], [ 5, 8 ] ) ]);;
gap> Size(I);
4626941

# attributes.gi
gap> S:=Semigroup([ Transformation( [ 1, 3, 4, 1, 3, 5 ] ), 
>  Transformation( [ 1, 5, 3, 5, 5, 5 ] ), 
>  Transformation( [ 2, 4, 6, 1, 6, 5 ] ), 
>  Transformation( [ 3, 2, 4, 2, 3, 3 ] ), 
>  Transformation( [ 4, 1, 2, 6, 2, 1 ] ), 
>  Transformation( [ 4, 6, 4, 3, 3, 3 ] ), 
>  Transformation( [ 4, 6, 5, 5, 2, 6 ] ), 
>  Transformation( [ 5, 1, 6, 1, 6, 3 ] ), 
>  Transformation( [ 5, 2, 5, 3, 5, 3 ] ), 
>  Transformation( [ 6, 4, 5, 5, 1, 6 ] ) ]);;
gap> I:=SemigroupIdeal(S, Transformation( [ 1, 3, 4, 1, 3, 5 ] ));;
gap> J:=SemigroupIdeal(S, Transformation( [ 1, 5, 3, 5, 5, 5 ] ));;
gap> IsGreensDLeq(I); IsGreensDLeq(J);
function( x, y ) ... end
function( x, y ) ... end
gap> Length(MaximalDClasses(I)); 
265
gap> MaximalDClasses(J);
[ {Transformation( [ 1, 5, 3, 5, 5, 5 ] )} ]
gap> StructureDescriptionSchutzenbergerGroups(I);
[ "1", "C2", "S3" ]
gap> StructureDescriptionSchutzenbergerGroups(J);
[ "1", "C2", "S3" ]
gap> StructureDescriptionMaximalSubgroups(J);
[ "1", "C2", "S3" ]
gap> StructureDescriptionMaximalSubgroups(I);
[ "1", "C2", "S3" ]
gap> GroupOfUnits(I);
fail
gap> GroupOfUnits(J);
fail
gap> IdempotentGeneratedSubsemigroup(I);;
gap> IdempotentGeneratedSubsemigroup(J);;
gap> last=last2;
true
gap> x:=Transformation( [ 5, 5, 1, 4, 1, 1 ] );;
gap> x in I; 
true
gap> x in J;
true
gap> Length(InversesOfSemigroupElement(I, x))=84;
true
gap> InversesOfSemigroupElement(J, x)=InversesOfSemigroupElement(I, x);
true
gap> MultiplicativeNeutralElement(I);
fail
gap> MultiplicativeNeutralElement(J);
fail
gap> MultiplicativeNeutralElement(I);
fail
gap> MultiplicativeNeutralElement(J);
fail
gap> MultiplicativeZero(I);
fail
gap> MultiplicativeZero(J);
fail
gap> MinimalIdeal(I);
<simple transformation semigroup ideal on 6 pts with 1 generator>
gap> MinimalIdeal(J);
<simple transformation semigroup ideal on 6 pts with 1 generator>
gap> last=last2;
true
gap> MinimalDClass(I);
{Transformation( [ 1, 1, 1, 1, 1, 1 ] )}
gap> MinimalDClass(J);
{Transformation( [ 5, 5, 5, 5, 5, 5 ] )}
gap> 

# attributes
gap> S:=InverseSemigroup(
>  PartialPerm( [ 1, 2, 3, 5, 6, 7, 8 ], [ 5, 9, 10, 6, 3, 8, 4 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5, 6, 8 ], [ 6, 2, 8, 4, 7, 5, 3 ] ), 
>  PartialPerm( [ 1, 2, 4, 6, 8, 9 ], [ 7, 10, 1, 9, 4, 2 ] ), 
>  PartialPerm( [ 1, 2, 4, 7, 8, 9 ], [ 10, 7, 8, 5, 9, 1 ] ), 
>  PartialPerm( [ 1, 2, 3, 6, 8, 10 ], [ 2, 6, 7, 9, 1, 5 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5, 8, 10 ], [ 3, 1, 4, 2, 5, 6, 7 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 6, 7, 8, 10 ], [ 3, 8, 1, 9, 4, 10, 5, 6 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5, 6, 7, 10 ], [ 4, 7, 6, 9, 10, 1, 3, 2 ] ), 
>  PartialPerm( [ 1, 2, 4, 5, 8, 10 ], [ 6, 2, 7, 8, 10, 4 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 5, 8, 10 ], [ 7, 1, 4, 3, 2, 6, 5 ] ));;
gap> K:=SemigroupIdeal(S, S.1*S.2^2*S.8);
<inverse partial perm semigroup ideal on 10 pts with 1 generator>
gap> MaximalDClasses(K);
[ {PartialPerm( [ 3, 6, 9 ], [ 3, 6, 9 ] )} ]
gap> StructureDescriptionMaximalSubgroups(K);
[ "1", "C2", "S3" ]
gap> StructureDescriptionSchutzenbergerGroups(K);
[ "1", "C2", "S3" ]
gap> GroupOfUnits(K);
fail
gap> Size(IdempotentGeneratedSubsemigroup(K));
176
gap> x:=PartialPerm( [ 2, 4, 6, 8, 9 ], [ 10, 1, 9, 4, 2 ] );;
gap> x in K;
false
gap> x:=PartialPerm([9],[9]);;
gap> x in K;
true
gap> InversesOfSemigroupElement(K, x);
[ <identity partial perm on [ 9 ]> ]
gap> MultiplicativeZero(K);
<empty partial perm>
gap> MultiplicativeNeutralElement(K);
fail
gap> MinimalIdeal(K);
<partial perm group on 10 pts with 1 generator>
gap> MinimalDClass(K);
{PartialPerm( [  ], [  ] )}
gap> I:=MinimalIdeal(K);
<partial perm group on 10 pts with 1 generator>
gap> IsomorphismPermGroup(I);
MappingByFunction( <trivial partial perm group on 10 pts with 1 generator>
 , Group(()), <Attribute "AsPermutation">, function( x ) ... end )

# attributes.gi
gap> S:=Monoid( Transformation( [ 2, 6, 7, 2, 6, 1, 1, 5 ] ), 
>   Transformation( [ 3, 8, 1, 4, 5, 6, 7, 1 ] ), 
>   Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] ), 
>   Transformation( [ 7, 1, 7, 4, 2, 5, 6, 3 ] ) );;
gap> L:=SemigroupIdeal(S, GeneratorsOfSemigroup(S));
<non-regular transformation semigroup ideal on 8 pts with 5 generators>
gap> Length(MaximalDClasses(L));
1
gap> L=S;
true
gap> MaximalDClasses(L);
[ {IdentityTransformation} ]
gap> StructureDescriptionSchutzenbergerGroups(L);
[ "1", "C2", "C4", "C5", "S3", "S4" ]
gap> StructureDescriptionMaximalSubgroups(L);
[ "1", "C2", "C4", "C5", "S3", "S4" ]
gap> GroupOfUnits(L);
<trivial transformation group>
gap> IdempotentGeneratedSubsemigroup(L);;
gap> x:=Transformation( [ 1, 4, 4, 5, 5, 3, 3, 1 ] );;
gap> InversesOfSemigroupElement(L, x);
[  ]
gap> InversesOfSemigroupElement(S, x);
[  ]
gap> MultiplicativeNeutralElement(L);
IdentityTransformation
gap> MultiplicativeZero(L);
fail
gap> MinimalIdeal(L);
<simple transformation semigroup ideal on 8 pts with 1 generator>
gap> L:=SemigroupIdeal(S, GeneratorsOfSemigroup(S));
<non-regular transformation semigroup ideal on 8 pts with 5 generators>
gap> MinimalIdeal(J);
<simple transformation semigroup ideal on 6 pts with 1 generator>
gap> MinimalIdeal(L);
<simple transformation semigroup ideal on 8 pts with 1 generator>
gap> MinimalDClass(L);
{Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1 ] )}
gap> MinimalDClass(S);
{Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1 ] )}

# 
gap> R:=Semigroup( [ Bipartition( [ [ 1, 2, 3, 5, -3 ], [ 4, 6, 7, -5 ], [ -1 ],
> [ -2, -4, -7 ], [ -6 ] ] ), Bipartition( [ [ 1, 2, 6, -5 ], [ 3 ], [ 4, 5, -2 ],
> [ 7, -1, -3, -4, -7 ], [ -6 ] ] ), Bipartition( [ [ 1, 3, 4, 5, -2, -3, -5 ], [
> 2, 6, -1, -6, -7 ], [ 7 ], [ -4 ] ] ), Bipartition( [ [ 1, 3, 4, 6, -7 ], [ 2,
> 5, -1, -5 ], [ 7, -2, -3, -4 ], [ -6 ] ] ), Bipartition( [ [ 1, 3 ], [ 2, 5, 6,
> -1, -2, -3 ], [ 4, 7, -4, -7 ], [ -5 ], [ -6 ] ] ), Bipartition( [ [ 1, 4, 5,
> -1, -2, -4, -6 ], [ 2, 3, 7, -3, -5, -7 ], [ 6 ] ] ), Bipartition( [ [ 1, -1, -4
> ], [ 2, 3, 4, 5, 6, 7, -2, -6 ], [ -3, -5, -7 ] ] ), Bipartition( [ [ 1, 7, -6
> ], [ 2, 3, 4, 5, -1, -2, -4 ], [ 6, -3, -5 ], [ -7 ] ] ), Bipartition( [ [ 1, 5,
> -2, -7 ], [ 2, 3, 6, -4 ], [ 4, -1, -5, -6 ], [ 7 ], [ -3 ] ] ), Bipartition( [
> [ 1, -3, -4 ], [ 2 ], [ 3, 7, -1, -7 ], [ 4, 5, -6 ], [ 6, -5 ], [ -2 ] ] ) ]
> );; 
gap> gens:= [ Bipartition( [ [ 1, 2, 3, 5, -3 ], [ 4, 6, 7, -5 ], [ -1 ], [
> -2, -4, -7 ], [ -6 ] ] ), Bipartition( [ [ 1, 2, 6, -5 ], [ 3 ], [ 4, 5, -2 ], [
> 7, -1, -3, -4, -7 ], [ -6 ] ] ), Bipartition( [ [ 1, 3 ], [ 2, 5, 6, -1, -2, -3
> ], [ 4, 7, -4, -7 ], [ -5 ], [ -6 ] ] ), Bipartition( [ [ 1, -3, -4 ], [ 2 ], [
> 3, 7, -1, -7 ], [ 4, 5, -6 ], [ 6, -5 ], [ -2 ] ] ) ];;
gap> M:=SemigroupIdeal(R, gens);
<non-regular bipartition semigroup ideal on 7 pts with 4 generators>
gap> Length(MaximalDClasses(M));
10
gap> Length(GeneratorsOfSemigroup(M));
95
gap> StructureDescriptionSchutzenbergerGroups(M);
[ "1" ]
gap> StructureDescriptionMaximalSubgroups(M);
[ "1" ]
gap> Size(IdempotentGeneratedSubsemigroup(M));
1441
gap> GroupOfUnits(M);
fail
gap> x:=Bipartition( [ [ 1, 2, 3, 4, 5, 6, 7, -3, -5 ], [ -1 ], 
> [ -2, -4, -7 ], [ -6 ] ] );;
gap> x in M;
true
gap> Length(InversesOfSemigroupElement(M, x));
875
gap> ForAll(InversesOfSemigroupElement(M, x), y-> y in M);
true
gap> MultiplicativeNeutralElement(M);
fail
gap> MultiplicativeZero(M);
fail
gap> MinimalIdeal(M);
<simple bipartition semigroup ideal on 7 pts with 1 generator>
gap> MinimalDClass(M);
{Bipartition( [ [ 1, 2, 3, 4, 5, 6, 7 ], [ -1 ], [ -2, -4, -7 ], [ -3, -5 ], 
 [ -6 ] ] )}
gap> MinimalDClass(R);
{Bipartition( [ [ 1, 2, 3, 4, 5, 6, 7 ], [ -1, -2, -3, -4, -5, -7 ], 
 [ -6 ] ] )}

# greens.gi for ideals
#
gap> S:=Semigroup([ Transformation( [ 1, 2, 6, 6, 5, 5 ] ), 
>   Transformation( [ 2, 3, 1, 3, 6, 5 ] ), 
>   Transformation( [ 3, 4, 6, 3, 2, 1 ] ), 
>   Transformation( [ 3, 6, 6, 4, 5, 6 ] ), 
>   Transformation( [ 4, 5, 2, 1, 6, 5 ] ), 
>   Transformation( [ 4, 6, 4, 2, 5, 2 ] ), 
>   Transformation( [ 5, 3, 2, 1, 2, 1 ] ), 
>   Transformation( [ 6, 5, 2, 4, 4, 1 ] ) ]);
<transformation semigroup on 6 pts with 8 generators>
gap> gensI:=[ Transformation( [ 4, 5, 2, 1, 6, 5 ] ), 
>   Transformation( [ 5, 2, 6, 2, 1, 4 ] ), 
>   Transformation( [ 3, 4, 6, 3, 2, 1 ] ) ];;
gap> gensJ:=[ Transformation( [ 1, 2, 6, 6, 5, 5 ] ) ];;
gap> I:=SemigroupIdeal(S, gensI);
<non-regular transformation semigroup ideal on 6 pts with 3 generators>
gap> J:=SemigroupIdeal(S, gensJ);
<regular transformation semigroup ideal on 6 pts with 1 generator>
gap> T:=Semigroup([ PartialPerm( [ 1, 2, 3, 4, 5 ], [ 1, 2, 6, 4, 5 ] ), 
>   PartialPerm( [ 2, 3, 4, 5, 6 ], [ 2, 1, 6, 4, 5 ] ), 
>   PartialPerm( [ 1, 2, 3, 4, 5 ], [ 3, 5, 4, 6, 2 ] ), 
>   PartialPerm( [ 1, 2, 4, 5, 6 ], [ 6, 2, 3, 1, 5 ] ), 
>   PartialPerm( [ 1, 3, 4, 5, 6 ], [ 2, 1, 4, 3, 5 ] ), 
>   PartialPerm( [ 1, 2, 5, 6 ], [ 4, 5, 2, 1 ] ), 
>   PartialPerm( [ 2, 4, 5, 6 ], [ 4, 6, 3, 2 ] ), 
>   PartialPerm( [ 2, 3, 4, 5 ], [ 6, 3, 4, 5 ] ), 
>   PartialPerm( [ 2, 3, 4, 6 ], [ 5, 3, 2, 4 ] ) ]);
<partial perm semigroup on 6 pts with 9 generators>
gap> gensK:=[
>   PartialPerm( [ 2, 5, 6 ], [ 6, 4, 5 ] ), 
>   PartialPerm( [ 1, 2, 4, 6 ], [ 2, 3, 6, 1 ] ), 
>   PartialPerm( [ 1, 2, 3, 5 ], [ 4, 5, 1, 2 ] ), 
>   PartialPerm( [ 1, 2, 5, 6 ], [ 5, 2, 6, 1 ] ) ];;
gap> K:=SemigroupIdeal(T, gensK);
<non-regular partial perm semigroup ideal on 6 pts with 4 generators>
gap> R:=Semigroup( [ Bipartition( [ [ 1, 2, 3, 5, -3 ], [ 4, 6, 7, -5 ], [ -1 ],
> [ -2, -4, -7 ], [ -6 ] ] ), 
>  Bipartition( [ [ 1, 2, 6, -5 ], [ 3 ], [ 4, 5, -2 ], [ 7, -1, -3, -4, -7 ], 
>  [ -6 ] ] ), 
>  Bipartition( [ [ 1, 3, 4, 5, -2, -3, -5 ], [ 2, 6, -1, -6, -7 ], [ 7 ], 
>  [ -4 ] ] ), 
>  Bipartition( [ [ 1, 3, 4, 6, -7 ], [ 2, 5, -1, -5 ], [ 7, -2, -3, -4 ], 
>  [ -6 ] ] ), 
>  Bipartition( [ [ 1, 3 ], [ 2, 5, 6, -1, -2, -3 ], [ 4, 7, -4, -7 ], [ -5 ],
>   [ -6 ] ] ),  Bipartition( [ [ 1, 4, 5, -1, -2, -4, -6 ], 
>  [ 2, 3, 7, -3, -5, -7 ], [ 6 ] ] ), 
> Bipartition( [ [ 1, -1, -4 ], [ 2, 3, 4, 5, 6, 7, -2, -6 ], 
> [ -3, -5, -7 ] ] ), 
>  Bipartition( [ [ 1, 7, -6 ], [ 2, 3, 4, 5, -1, -2, -4 ], [ 6, -3, -5 ], 
>  [ -7 ] ] ), 
>  Bipartition( [ [ 1, 5, -2, -7 ], [ 2, 3, 6, -4 ], [ 4, -1, -5, -6 ], [ 7 ], 
>  [ -3 ] ] ), 
> Bipartition( [ [ 1, -3, -4 ], [ 2 ], [ 3, 7, -1, -7 ], [ 4, 5, -6 ], [ 6, -5 ]
>  , [ -2 ] ] ) ] );
<bipartition semigroup on 7 pts with 10 generators>
gap> gensL:=
> [ Bipartition( [ [ 1, 2, 3, 5, -3 ], [ 4, 6, 7, -5 ], [ -1 ], [ -2, -4, -7 ],\
>  [ -6 ] ] ), 
>   Bipartition( [ [ 1, 2, 6, -5 ], [ 3 ], [ 4, 5, -2 ], [ 7, -1, -3, -4, -7 ],\
>  [ -6 ] ] ), 
>   Bipartition( [ [ 1, 3 ], [ 2, 5, 6, -1, -2, -3 ], [ 4, 7, -4, -7 ], [ -5 ],\
>  [ -6 ] ] ), 
>   Bipartition( [ [ 1, -3, -4 ], [ 2 ], [ 3, 7, -1, -7 ], [ 4, 5, -6 ], [ 6, -\
> 5 ], [ -2 ] ] ) ];;
gap> L:=SemigroupIdeal(R, gensL);
<non-regular bipartition semigroup ideal on 7 pts with 4 generators>
gap> U:=Semigroup( [ Transformation( [ 1, 3, 2, 2, 4, 5 ] ), 
>   Transformation( [ 1, 5, 6, 6, 2, 5 ] ), 
>   Transformation( [ 1, 6, 4, 6, 3, 1 ] ), 
>   Transformation( [ 2, 1, 3, 3, 5, 2 ] ), 
>   Transformation( [ 2, 1, 5, 1, 2, 3 ] ), 
>   Transformation( [ 3, 3, 1, 5, 1, 1 ] ), 
>   Transformation( [ 4, 4, 4, 3, 3, 5 ] ), 
>   Transformation( [ 4, 4, 6, 3, 5, 6 ] ), 
>   Transformation( [ 5, 3, 3, 6, 2, 4 ] ), 
>   Transformation( [ 6, 4, 4, 6, 3, 2 ] ) ] );
<transformation semigroup on 6 pts with 10 generators>
gap> A:=SemigroupIdeal(U, Transformation( [ 5, 3, 3, 6, 2, 4 ] ));
<non-regular transformation semigroup ideal on 6 pts with 1 generator>
gap> B:=SemigroupIdeal(U, Transformation( [ 6, 4, 4, 6, 3, 2 ] ));
<non-regular transformation semigroup ideal on 6 pts with 1 generator>
gap> C:=SemigroupIdeal(U, Transformation( [ 3, 3, 1, 5, 1, 1 ] ));
<regular transformation semigroup ideal on 6 pts with 1 generator>
gap> ideals:=[A,B,C,I,J,K,L];;

# GreensXClasses
gap> GreensDClasses(I);
[ {Transformation( [ 4, 5, 2, 1, 6, 5 ] )}, 
  {Transformation( [ 5, 2, 6, 2, 1, 4 ] )}, 
  {Transformation( [ 3, 4, 6, 3, 2, 1 ] )}, 
  {Transformation( [ 1, 6, 5, 4, 5, 6 ] )}, 
  {Transformation( [ 2, 2, 3, 2, 2, 1 ] )}, 
  {Transformation( [ 2, 2, 2, 2, 6, 2 ] )}, 
  {Transformation( [ 5, 5, 5, 5, 5, 5 ] )}, 
  {Transformation( [ 2, 6, 5, 6, 4, 1 ] )}, 
  {Transformation( [ 6, 5, 2, 5, 1, 4 ] )}, 
  {Transformation( [ 5, 2, 6, 2, 4, 1 ] )}, 
  {Transformation( [ 2, 6, 5, 6, 1, 4 ] )}, 
  {Transformation( [ 6, 5, 2, 5, 4, 1 ] )}, 
  {Transformation( [ 4, 6, 3, 6, 1, 2 ] )}, 
  {Transformation( [ 6, 3, 4, 3, 2, 1 ] )}, 
  {Transformation( [ 3, 4, 6, 4, 1, 2 ] )}, 
  {Transformation( [ 4, 6, 3, 6, 2, 1 ] )}, 
  {Transformation( [ 6, 3, 4, 3, 1, 2 ] )}, 
  {Transformation( [ 3, 4, 6, 4, 2, 1 ] )}, {Transformation( [ 2, 4, 1, 2 ] )}
    , {Transformation( [ 4, 1, 2, 1, 6, 5 ] )}, 
  {Transformation( [ 1, 2, 4, 2 ] )}, {Transformation( [ 2, 4, 1, 4, 6, 5 ] )}
    , {Transformation( [ 4, 1, 2, 1 ] )}, 
  {Transformation( [ 1, 2, 4, 2, 6, 5 ] )}, 
  {Transformation( [ 2, 4, 1, 4 ] )} ]
gap> Number(GreensDClasses(I), IsRegularClass);
4
gap> I:=SemigroupIdeal(S, gensI);;
gap> NrRegularDClasses(I);
4
gap> GreensDClasses(J);
[ {Transformation( [ 1, 2, 6, 6, 5, 5 ] )}, 
  {Transformation( [ 1, 2, 5, 5, 5, 5 ] )}, 
  {Transformation( [ 6, 5, 5, 6, 5, 5 ] )}, 
  {Transformation( [ 6, 6, 6, 6, 6, 6 ] )} ]
gap> Number(GreensDClasses(J), IsRegularClass);
4
gap> J:=SemigroupIdeal(S, gensJ);;
gap> NrRegularDClasses(J);
4
gap> Size(GreensDClasses(K));
735
gap> Number(GreensDClasses(K), IsRegularClass);
5
gap> K:=SemigroupIdeal(T, gensK);;
gap> NrRegularDClasses(K);
5
gap> Size(GreensHClasses(I));
2347
gap> Size(GreensLClasses(I));
75
gap> GreensLClasses(I){[51..60]};
[ {Transformation( [ 3, 3, 3, 3, 5, 3 ] )}, 
  {Transformation( [ 5, 5, 5, 5, 5, 5 ] )}, 
  {Transformation( [ 6, 6, 6, 6, 6, 6 ] )}, 
  {Transformation( [ 1, 1, 1, 1, 1, 1 ] )}, 
  {Transformation( [ 2, 2, 2, 2, 2, 2 ] )}, 
  {Transformation( [ 3, 3, 3, 3, 3, 3 ] )}, 
  {Transformation( [ 4, 4, 4, 4, 4, 4 ] )}, 
  {Transformation( [ 2, 6, 5, 6, 4, 1 ] )}, 
  {Transformation( [ 6, 5, 2, 5, 1, 4 ] )}, 
  {Transformation( [ 5, 2, 6, 2, 4, 1 ] )} ]
gap> Size(GreensRClasses(I));
156
gap> Size(GreensHClasses(J));
2326
gap> Size(GreensLClasses(J));
54
gap> Size(GreensRClasses(J));
135
gap> GreensRClasses(J){[97..103]};
[ {Transformation( [ 1, 5, 1, 5, 5, 2 ] )}, 
  {Transformation( [ 5, 1, 5, 1, 5, 2 ] )}, 
  {Transformation( [ 5, 2, 1, 1, 1, 2 ] )}, 
  {Transformation( [ 1, 2, 1, 5, 5, 1 ] )}, 
  {Transformation( [ 1, 5, 2, 5, 5, 1 ] )}, 
  {Transformation( [ 5, 2, 5, 5, 2, 1 ] )}, 
  {Transformation( [ 5, 2, 1, 2, 1, 5 ] )} ]
gap> Size(GreensHClasses(K));
1555
gap> GreensHClasses(K){[1337..1342]};
[ {PartialPerm( [ 1, 2, 3, 4 ], [ 3, 6, 5, 2 ] )}, 
  {PartialPerm( [ 1, 2, 3, 4 ], [ 5, 6, 2, 3 ] )}, 
  {PartialPerm( [ 1, 2, 3, 4 ], [ 2, 6, 3, 5 ] )}, 
  {PartialPerm( [ 1, 2, 3, 4 ], [ 5, 6, 3, 2 ] )}, 
  {PartialPerm( [ 1, 2, 3, 4 ], [ 2, 6, 5, 3 ] )}, 
  {PartialPerm( [ 1, 2, 3, 4 ], [ 3, 6, 2, 5 ] )} ]
gap> Size(GreensLClasses(K));
917
gap> Size(GreensRClasses(K));
791

# XClassReps
gap> DClassReps(J);
[ Transformation( [ 1, 2, 6, 6, 5, 5 ] ), 
  Transformation( [ 1, 2, 5, 5, 5, 5 ] ), 
  Transformation( [ 6, 5, 5, 6, 5, 5 ] ), 
  Transformation( [ 6, 6, 6, 6, 6, 6 ] ) ]
gap> LClassReps(L){[10..20]};
[ <bipartition: [ 1, 2, 3, 4, 5, 6, 7, -1, -3, -4, -5, -7 ], [ -2 ], [ -6 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, 7, -1, -3, -4, -5, -6, -7 ], [ -2 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, 7, -1, -2, -3, -5, -6, -7 ], [ -4 ]>, 
  <block bijection: [ 1, 2, 3, 4, 5, 6, 7, -1, -2, -3, -4, -5, -6, -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, 7, -2, -6 ], [ -1, -4 ], [ -3, -5, -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, 7, -1, -2, -4, -6 ], [ -3, -5 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, 7, -1, -2, -4, -5, -6, -7 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, 7, -1, -2, -4, -6 ], [ -3, -5, -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, 7, -1, -2, -3, -4, -5, -6 ], [ -7 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, 7, -2, -4, -7 ], [ -1, -5, -6 ], [ -3 ]>, 
  <bipartition: [ 1, 2, 3, 4, 5, 6, 7, -1, -6, -7 ], [ -2, -3, -5 ], [ -4 ]> ]

# GreensXClassOfElement
gap> GreensDClassOfElement(I, Transformation( [ 2, 2, 6, 2, 5, 5 ] ));
{Transformation( [ 3, 3, 2, 3, 3, 1 ] )}
gap> GreensDClassOfElement(J, Transformation( [ 6, 6, 4, 2, 4, 2 ] ));
{Transformation( [ 2, 1, 5, 5, 5, 5 ] )}
gap> GreensDClassOfElement(K, PartialPerm( [ 1, 4, 5, 6 ], [ 6, 2, 1, 3 ] ));
{PartialPerm( [ 1, 4, 5, 6 ], [ 6, 2, 1, 3 ] )}
gap> GreensHClassOfElement(L, Bipartition( [ [ 1, 3, 4, 6, -3 ], 
>   [ 2, 5, 7 ], [ -1 ], [ -2, -4, -7 ], [ -5 ], [ -6 ] ] ));
{Bipartition( [ [ 1, 3, 4, 6, -3 ], [ 2, 5, 7 ], [ -1 ], [ -2, -4, -7 ], 
 [ -5 ], [ -6 ] ] )}
gap> GreensLClassOfElement(A, Transformation( [ 6, 3, 4, 4, 3, 2 ] ));
{Transformation( [ 6, 3, 4, 4, 3, 2 ] )}
gap> GreensRClassOfElement(B, Transformation( [ 3, 4, 6, 6, 4, 2 ] ));
{Transformation( [ 3, 4, 6, 6, 4, 2 ] )}
gap> GreensHClassOfElement(C, Transformation( [ 2, 2, 5, 2, 2, 1 ] ));
{Transformation( [ 2, 2, 5, 2, 2, 1 ] )}

# NrXClasses (Recreate objects)
gap> I:=SemigroupIdeal(S, gensI);;
gap> J:=SemigroupIdeal(S, gensJ);;
gap> K:=SemigroupIdeal(T, gensK);;
gap> L:=SemigroupIdeal(R, gensL);;
gap> A:=SemigroupIdeal(U, Transformation( [ 5, 3, 3, 6, 2, 4 ] ));;
gap> B:=SemigroupIdeal(U, Transformation( [ 6, 4, 4, 6, 3, 2 ] ));;
gap> C:=SemigroupIdeal(U, Transformation( [ 3, 3, 1, 5, 1, 1 ] ));;
gap> ideals:=[A,B,C,I,J,K,L];;
gap> List(ideals, NrDClasses);       
[ 58, 269, 3, 25, 4, 735, 63 ]
gap> List(ideals, NrRClasses);
[ 623, 347, 81, 156, 135, 791, 120 ]
gap> List(ideals, NrLClasses);
[ 269, 307, 41, 75, 54, 917, 165 ]
gap> List(ideals, NrHClasses);
[ 2225, 1722, 1456, 2347, 2326, 1555, 1568 ]

# NrRegularDClasses
gap> List(ideals, NrRegularDClasses);
[ 4, 3, 3, 4, 4, 5, 4 ]

# Idempotents, (and with integer)
gap> L:=SemigroupIdeal(R, gensL);;
gap> Idempotents(A){[444..450]};
[ Transformation( [ 2, 2, 6, 2, 2, 6 ] ), 
  Transformation( [ 2, 2, 3, 2, 2, 3 ] ), 
  Transformation( [ 1, 1, 3, 1, 1, 3 ] ), 
  Transformation( [ 5, 5, 3, 5, 5, 3 ] ), 
  Transformation( [ 5, 5, 6, 5, 5, 6 ] ), 
  Transformation( [ 4, 4, 3, 4, 4, 3 ] ), 
  Transformation( [ 4, 4, 6, 4, 4, 6 ] ) ]
gap> Idempotents(B){[444..450]}; 
[ Transformation( [ 6, 3, 3, 6, 6, 6 ] ), 
  Transformation( [ 1, 3, 3, 1, 1, 1 ] ), 
  Transformation( [ 4, 2, 2, 4, 4, 4 ] ), 
  Transformation( [ 6, 2, 2, 6, 6, 6 ] ), 
  Transformation( [ 5, 3, 3, 5, 5, 5 ] ), 
  Transformation( [ 4, 4, 4, 4, 4, 4 ] ), 
  Transformation( [ 6, 6, 6, 6, 6, 6 ] ) ]
gap> Idempotents(C){[444..450]}; 
[ Transformation( [ 1, 6, 3, 3, 6, 6 ] ), Transformation( [ 1, 3, 3, 3, 3 ] ),
  Transformation( [ 1, 3, 3, 3, 1 ] ), Transformation( [ 1, 3, 3, 6, 6, 6 ] ),
  Transformation( [ 1, 1, 3, 1, 1 ] ), Transformation( [ 1, 3, 3, 3, 6, 6 ] ),
  Transformation( [ 1, 1, 3, 3, 3 ] ) ]
gap> Idempotents(C,1);
[ Transformation( [ 4, 4, 4, 4, 4, 4 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2 ] ), 
  Transformation( [ 6, 6, 6, 6, 6, 6 ] ), 
  Transformation( [ 3, 3, 3, 3, 3, 3 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 1 ] ), 
  Transformation( [ 5, 5, 5, 5, 5, 5 ] ) ]
gap> Idempotents(L, 2);
[ <bipartition: [ 1, 3 ], [ 2, 5, 6, -1, -2, -3 ], [ 4, 7, -4, -7 ], [ -5 ], 
     [ -6 ]>, 
  <bipartition: [ 1, 7, -1, -7 ], [ 2, 3, 4, 5, 6, -3, -4, -5, -6 ], [ -2 ]>, 
  <bipartition: [ 1, 5, -1, -5 ], [ 2, 3, 4, 6, -2, -3, -4, -7 ], [ 7 ], 
     [ -6 ]>, <bipartition: [ 1, 4, 5, 6, -1, -4, -5, -6 ], [ 2 ], 
     [ 3, 7, -2, -7 ], [ -3 ]>, 
  <bipartition: [ 1, 3, 4, 6 ], [ 2, 5, -5 ], [ 7, -1, -3, -4, -6, -7 ], 
     [ -2 ]>, <bipartition: [ 1, 3, 4, 5, 7, -1, -2, -3, -4, -5 ], [ 2 ], 
     [ 6, -6 ], [ -7 ]> ]

# NrIdempotents
gap> List(ideals, NrIdempotents);
[ 547, 528, 528, 774, 774, 43, 1406 ]
gap> List(ideals, i->Size(Idempotents(i)));
[ 547, 528, 528, 774, 774, 43, 1406 ]

# PartialOrderOfDClasses
gap> PartialOrderOfDClasses(K){[100..111]};
[ [ 1, 5, 6, 7 ], [ 1, 5, 6, 7 ], [ 1, 5, 6, 7 ], [ 1, 5, 6, 7 ], 
  [ 1, 5, 6, 7 ], [ 1, 5, 6, 7 ], [ 1, 5, 6, 7 ], [ 1, 5, 6, 7 ], 
  [ 1, 5, 6, 7 ], [ 1, 5, 6, 7 ], [ 1, 5, 6, 7 ], [ 1, 5, 6, 7 ] ]
gap> PartialOrderOfDClasses(I);
[ [ 4, 5, 6 ], [ 4, 5, 6 ], [ 4, 5, 6 ], [ 4, 5, 6, 7 ], [ 5, 6, 7 ], 
  [ 6, 7 ], [ 7 ], [ 4, 5, 6 ], [ 4, 5, 6 ], [ 4, 5, 6 ], [ 4, 5, 6 ], 
  [ 4, 5, 6 ], [ 4, 5, 6 ], [ 4, 5, 6 ], [ 4, 5, 6 ], [ 4, 5, 6 ], 
  [ 4, 5, 6 ], [ 4, 5, 6 ], [ 4, 5, 6 ], [ 4, 5, 6 ], [ 4, 5, 6 ], 
  [ 4, 5, 6 ], [ 4, 5, 6 ], [ 4, 5, 6 ], [ 4, 5, 6 ] ]
gap> PartialOrderOfDClasses(J);
[ [ 1, 2, 3 ], [ 2, 3, 4 ], [ 3, 4 ], [ 4 ] ]
gap> PartialOrderOfDClasses(A);
[ [ 2, 3, 4, 5, 6, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 
      24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34 ], [ 2, 4, 5, 6, 7, 9, 34 ], 
  [ 4, 5, 6, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 
      26, 27, 28, 29, 30, 31, 32, 33 ], [ 4, 6, 7 ], 
  [ 4, 5, 6, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 
      26, 27, 28, 29, 30, 31, 32, 33 ], [ 6, 7 ], [ 7 ], 
  [ 4, 5, 6, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 
      26, 27, 28, 29, 30, 31, 32, 33 ], 
  [ 4, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 
      25, 26, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39, 40, 41, 42, 43, 
      44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58 ], [ 4, 6 ], 
  [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], 
  [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], 
  [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], 
  [ 4, 6 ], [ 4, 6 ], 
  [ 4, 6, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 
      51, 52, 53, 54, 55, 56, 57, 58 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], 
  [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], 
  [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], 
  [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ], [ 4, 6 ] ]
gap> PartialOrderOfDClasses(B){[10..15]};
[ [ 2, 3, 4 ], [ 2, 3, 4 ], [ 2, 3, 4 ], [ 2, 3, 4 ], [ 2, 3, 4 ], 
  [ 2, 3, 4 ] ]
gap> PartialOrderOfDClasses(C);          
[ [ 1, 2, 3 ], [ 2, 3 ], [ 3 ] ]

# Check that sizes were correct
gap> List(ideals, Size);
[ 9285, 7172, 6906, 19167, 19146, 3782, 1568 ]

# this example caused a seg fault before changeset 34d25659aa72
gap> S:=InverseSemigroup([
>  PartialPerm( [ 1, 2, 3, 4 ], [ 4, 1, 2, 6 ] ),
>  PartialPerm( [ 1, 2, 3, 4 ], [ 5, 7, 1, 6 ] ),
>  PartialPerm( [ 1, 2, 3, 5 ], [ 5, 2, 7, 3 ] ),
>  PartialPerm( [ 1, 2, 3, 6, 7 ], [ 1, 3, 4, 7, 5 ] ),
>  PartialPerm( [ 1, 2, 3, 4, 5, 7 ], [ 3, 2, 4, 6, 1, 5 ] ) ]);
<inverse partial perm semigroup on 7 pts with 5 generators>
gap> I:=SemigroupIdeal(S, PartialPerm( [ 1, 3, 4, 5, 7 ], [ 1, 3, 4, 5, 7 ] ));
<inverse partial perm semigroup ideal on 7 pts with 1 generator>
gap> GeneratorsOfSemigroup(I);;

# test for \in method from ideals-acting.gi (only applies to ideals that know
# apriori that they are regular) which partially enumerates the semigroup ideal
# data
gap> S:=FullTransformationMonoid(6);;
gap> x:=Transformation( [ 6, 5, 1, 5, 1, 2 ] );;
gap> I:=SemigroupIdeal(S, x);
<regular transformation semigroup ideal on 6 pts with 1 generator>
gap> x:=Transformation( [ 2, 2, 4, 4, 2, 2 ] );;
gap> x in I;
true
gap> IsClosedData(SemigroupData(I));
false
gap> Size(I);
35136
gap> Size(SemigroupIdeal(S, I));
35136

# IsomorphismPermGroup for an ideal which happens to be a group...
gap> S:=FullTransformationSemigroup(6);;
gap> S:=Semigroup(GroupOfUnits(S));;
gap> I:=SemigroupIdeal(S, S);;
gap> IsomorphismPermGroup(I);
MappingByFunction( <transformation group of size 720, 
 on 6 pts with 3 generators>, Group([ (), (1,2,3,4,5,6), (1,
2) ]), <Attribute "PermutationOfImage">, function( x ) ... end )

#
gap> SemigroupsStopTest(); 
gap> STOP_TEST( "Semigroups package: ideals.tst", 10000);
