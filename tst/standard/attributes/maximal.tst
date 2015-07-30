############################################################################
##
#W  standard/attributes/maximal.tst
#Y  Copyright (C) 2015                                   James D. Mitchell
##                                                       Wilfred A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## TODO: improve code coverage!!
gap> START_TEST("Semigroups package: standard/attributes/maximal.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS_StartTest();;

#T# MaximalTest1: IsMaximalSubsemigroup
gap> S := Semigroup([
>   Transformation([1, 2, 4, 4]),
>   Transformation([4, 4, 1, 4]),
>   Transformation([2, 1, 4, 2])]);
<transformation semigroup of degree 4 with 3 generators>
gap> T := Semigroup([
> Transformation([2, 1, 4, 2]),
> Transformation([2, 1, 2, 2]),
> Transformation([4, 4, 1, 4])]);
<transformation semigroup of degree 4 with 3 generators>
gap> IsMaximalSubsemigroup(S, T); # maximal
true
gap> U := Semigroup([
>   Transformation([2, 2, 1, 1]),
>   Transformation([2, 2, 3, 4]),
>   Transformation([3, 4, 2, 4])]);
<transformation semigroup of degree 4 with 3 generators>
gap> IsSubsemigroup(S, U);
false
gap> IsMaximalSubsemigroup(S, U); # not a subsemigroup
false
gap> IsSubsemigroup(U, S);
false
gap> IsMaximalSubsemigroup(U, S); # not a subsemigroup
false
gap> IsSubsemigroup(S, S);
true
gap> S <> S;
false
gap> IsMaximalSubsemigroup(S, S); # equal semigroups
false
gap> IsMaximalSubsemigroup(S,
> Semigroup(RepresentativeOfMinimalIdeal(S))); # non-maximal
false

#T# maximal: MaximalSubsemigroups, for RMS and group, error 1/4
gap> R := ReesMatrixSemigroup(SymmetricGroup(3), [[(), (1, 2)]]);;
gap> U := Semigroup(RMSElement(R, 1, (), 1), RMSElement(R, 2, (1, 2), 1));
<subsemigroup of 2x1 Rees matrix semigroup with 2 generators>
gap> MaximalSubsemigroups(U, Group(()));
Error, Semigroups: MaximalSubsemigroups: usage,
the first argument <R> must be a Rees matrix semigroup,
gap> MaximalSubsemigroups(U);
[ <subsemigroup of 2x1 Rees matrix semigroup with 1 generator>, 
  <subsemigroup of 2x1 Rees matrix semigroup with 1 generator> ]

#T# maximal: MaximalSubsemigroups, for RMS and group, error 2/4
gap> U := BrauerMonoid(2);
<regular bipartition monoid of degree 2 with 2 generators>
gap> R := ReesMatrixSemigroup(U, [[One(U)]]);
<Rees matrix semigroup 1x1 over <regular bipartition monoid of degree 2 with
  2 generators>>
gap> MaximalSubsemigroups(R, Group(()));
Error, Semigroups: MaximalSubsemigroups: usage,
the first argument <R> must be a Rees matrix semigroup whose underlying
semigroup is a group,

#T# maximal: MaximalSubsemigroups, for RMS and group, error, 3/4
gap> R := RectangularBand(2, 2);
<Rees matrix semigroup 2x2 over Group(())>
gap> MaximalSubsemigroups(R, Group((1, 2)));
Error, Semigroups: MaximalSubsemigroups: usage,
the second argument <H> must be a subgroup of the underlying
group of the Rees matrix semigroup in the first argument, <R>,

#T# maximal: MaximalSubsemigroups, for RMS and group, error, 4/4
gap> R := ReesMatrixSemigroup(SymmetricGroup(3), [[()]]);
<Rees matrix semigroup 1x1 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R, Group(()));
Error, Semigroups: MaximalSubsemigroups: usage,
the second argument <H> must be a maximal subgroup of the underlying
group of the Rees matrix semigroup in the first argument, <R>,

#T# maximal: MaximalSubsemigroups, for RMS and group, 1/3
gap> R := ReesMatrixSemigroup(SymmetricGroup(3), [[(), ()], [(), ()]]);
<Rees matrix semigroup 2x2 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R, Group((1, 2, 3)));
[ <subsemigroup of 2x2 Rees matrix semigroup with 3 generators> ]

#T# maximal: MaximalSubsemigroups, for RMS and group, 2/3
gap> R := ReesMatrixSemigroup(SymmetricGroup(3), [[(), ()]]);
<Rees matrix semigroup 2x1 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R, Group((1, 2, 3)));
[ <subsemigroup of 2x1 Rees matrix semigroup with 3 generators> ]
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 2x1 Rees matrix semigroup with 3 generators>, 
  <subsemigroup of 2x1 Rees matrix semigroup with 3 generators>, 
  <subsemigroup of 2x1 Rees matrix semigroup with 3 generators>, 
  <subsemigroup of 2x1 Rees matrix semigroup with 3 generators>, 
  <Rees matrix semigroup 1x1 over Sym( [ 1 .. 3 ] )>, 
  <Rees matrix semigroup 1x1 over Sym( [ 1 .. 3 ] )> ]

#T# maximal: MaximalSubsemigroups, for RMS and group, 3/3
gap> R := ReesMatrixSemigroup(SymmetricGroup(3), [[()], [()], [()]]);
<Rees matrix semigroup 1x3 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R, Group((1, 3, 2)));
[ <subsemigroup of 1x3 Rees matrix semigroup with 4 generators> ]
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 1x3 Rees matrix semigroup with 4 generators>, 
  <subsemigroup of 1x3 Rees matrix semigroup with 4 generators>, 
  <subsemigroup of 1x3 Rees matrix semigroup with 4 generators>, 
  <subsemigroup of 1x3 Rees matrix semigroup with 4 generators>, 
  <Rees matrix semigroup 1x2 over Sym( [ 1 .. 3 ] )>, 
  <Rees matrix semigroup 1x2 over Sym( [ 1 .. 3 ] )>, 
  <Rees matrix semigroup 1x2 over Sym( [ 1 .. 3 ] )> ]

#T# maximal: MaximalSubsemigroups, for RMS and group, no result, 1/1
gap> G := SymmetricGroup(3);;
gap> R := ReesMatrixSemigroup(G, [[(1, 2, 3), ()], [(), ()]]);
<Rees matrix semigroup 2x2 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R, Group((1, 2)));
[  ]

#T# maximal: MaximalSubsemigroups, for RZMS and group, error 1/5
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(2), [[0, 0]]);
<Rees 0-matrix semigroup 2x1 over Sym( [ 1 .. 2 ] )>
gap> U := Semigroup([
> RMSElement(R, 1, (), 1),
> RMSElement(R, 1, (1, 2), 1),
> RMSElement(R, 2, (), 1)]);
<subsemigroup of 2x1 Rees 0-matrix semigroup with 3 generators>
gap> MaximalSubsemigroups(U, Group(()));
Error, Semigroups: MaximalSubsemigroups: usage,
the first argument <R> must be a Rees 0-matrix semigroup,
gap> MaximalSubsemigroups(U);
[ <subsemigroup of 2x1 Rees 0-matrix semigroup with 2 generators>, 
  <subsemigroup of 2x1 Rees 0-matrix semigroup with 2 generators>, 
  <subsemigroup of 2x1 Rees 0-matrix semigroup with 2 generators> ]

#T# maximal: MaximalSubsemigroups, for RZMS and group, error 2/5
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(2), [[0]]);
<Rees 0-matrix semigroup 1x1 over Sym( [ 1 .. 2 ] )>
gap> MaximalSubsemigroups(R, Group(()));
Error, Semigroups: MaximalSubsemigroups:
the first argument <R> must be a regular Rees 0-matrix semigroup,

#T# maximal: MaximalSubsemigroups, for RZMS and group, error 3/5
gap> R := ReesZeroMatrixSemigroup(FullTransformationMonoid(2),
> [[Transformation([1, 2])]]);
<Rees 0-matrix semigroup 1x1 over <regular transformation monoid 
  of size 4, degree 2 with 2 generators>>
gap> MaximalSubsemigroups(R, Group(()));
Error, Semigroups: MaximalSubsemigroups: usage,
the first argument <R> must be a Rees 0-matrix semigroup whose
underlying semigroup is a group,
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 1x1 Rees 0-matrix semigroup with 4 generators>, 
  <subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators>, 
  <subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators> ]

#T# maximal: MaximalSubsemigroups, for RZMS and group, error 4/5
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(2), [[()]]);
<Rees 0-matrix semigroup 1x1 over Sym( [ 1 .. 2 ] )>
gap> MaximalSubsemigroups(R, Group((1, 2, 3)));
Error, Semigroups: MaximalSubsemigroups: usage,
the second argument <H> must be a subgroup of the underlying
group of the Rees 0-matrix semigroup in the first argument, <R>,

#T# maximal: MaximalSubsemigroups, for RZMS and group, error 5/5
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(2), [[()]]);
<Rees 0-matrix semigroup 1x1 over Sym( [ 1 .. 2 ] )>
gap> MaximalSubsemigroups(R, Group((1, 2)));
Error, Semigroups: MaximalSubsemigroups: usage,
the second argument <H> must be a maximal subgroup of the underlying
group of the Rees 0-matrix semigroup in the first argument, <R>,

#T# maximal: MaximalSubsemigroups, for RZMS and group, 1/3
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(2), [[()]]);
<Rees 0-matrix semigroup 1x1 over Sym( [ 1 .. 2 ] )>
gap> MaximalSubsemigroups(R, Group(()));
[ <subsemigroup of 1x1 Rees 0-matrix semigroup with 3 generators> ]

#T# maximal: MaximalSubsemigroups, for RZMS and group, 2/3
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(2), [[(), 0], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Sym( [ 1 .. 2 ] )>
gap> MaximalSubsemigroups(R, Group(()));
[ <subsemigroup of 2x2 Rees 0-matrix semigroup with 6 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 6 generators> ]

#T# maximal: MaximalSubsemigroups, for RZMS and group, 3/3
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(2),
> [[(), 0], [0, ()], [0, ()]]);
<Rees 0-matrix semigroup 2x3 over Sym( [ 1 .. 2 ] )>
gap> MaximalSubsemigroups(R, Group(()));
[ <subsemigroup of 2x3 Rees 0-matrix semigroup with 7 generators>, 
  <subsemigroup of 2x3 Rees 0-matrix semigroup with 7 generators> ]

#T# maximal: MaximalSubsemigroups, for RZMS, 1/6
gap> R := ReesZeroMatrixSemigroup(FullTransformationMonoid(2),
> [[Transformation([1, 2])]]);
<Rees 0-matrix semigroup 1x1 over <regular transformation monoid 
  of size 4, degree 2 with 2 generators>>
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 1x1 Rees 0-matrix semigroup with 4 generators>, 
  <subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators>, 
  <subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators> ]

#T# maximal: MaximalSubsemigroups, for RZMS, 2/6
gap> R := ReesZeroMatrixSemigroup(Group(()), [[()]]);
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator>, 
  <subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator> ]

#T# maximal: MaximalSubsemigroups, for RZMS, 3/6
gap> R := ReesZeroMatrixSemigroup(Group((1, 2)), [[(), ()]]);
<Rees 0-matrix semigroup 2x1 over Group([ (1,2) ])>
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 2x1 Rees 0-matrix semigroup with 2 generators>, 
  <subsemigroup of 2x1 Rees 0-matrix semigroup with 4 generators>, 
  <Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>, 
  <Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])> ]

#T# maximal: MaximalSubsemigroups, for RZMS, 4/6
gap> R := ReesZeroMatrixSemigroup(Group(()), [[()], [()]]);
<Rees 0-matrix semigroup 1x2 over Group(())>
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 1x2 Rees 0-matrix semigroup with 2 generators>, 
  <Rees 0-matrix semigroup 1x1 over Group(())>, 
  <Rees 0-matrix semigroup 1x1 over Group(())> ]

#T# maximal: MaximalSubsemigroups, for RZMS, 5/6
gap> R := ReesZeroMatrixSemigroup(Group((1, 2)),
> [[(), 0], [0, (1, 2)]]);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 2x2 Rees 0-matrix semigroup with 6 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 6 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 7 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 7 generators> ]

#T# maximal: MaximalSubsemigroups, for RZMS, 6/6
gap> R := ReesZeroMatrixSemigroup(Group([()]),
> [[(), (), 0, 0], [(), (), (), 0],
>  [0, (), (), ()]]);
<Rees 0-matrix semigroup 4x3 over Group(())>
gap> MaximalSubsemigroups(R);
[ <Rees 0-matrix semigroup 4x2 over Group(())>, 
  <Rees 0-matrix semigroup 4x2 over Group(())>, 
  <Rees 0-matrix semigroup 3x3 over Group(())>, 
  <Rees 0-matrix semigroup 3x3 over Group(())>, 
  <Rees 0-matrix semigroup 3x3 over Group(())>, 
  <Rees 0-matrix semigroup 3x3 over Group(())>, 
  <subsemigroup of 4x3 Rees 0-matrix semigroup with 10 generators>, 
  <subsemigroup of 4x3 Rees 0-matrix semigroup with 13 generators>, 
  <subsemigroup of 4x3 Rees 0-matrix semigroup with 14 generators> ]

#T# maximal: MaximalSubsemigroups, for trivial semigroup 1/1
gap> MaximalSubsemigroups(TrivialSemigroup());
[  ]

#T# maximal: MaximalSubsemigroups, for a group as semigroup 1/1
gap> MaximalSubsemigroups(Semigroup(Transformation([2, 3, 1])))
> = [TrivialSemigroup()];
true

#T# maximal; MaximalSubsemigroups, for a semigroup 1/?
gap> S := Monoid([
> Transformation([1, 1, 2, 1, 4, 4]),
> Transformation([1, 3, 1, 5, 1, 3])]);
<transformation monoid of degree 6 with 2 generators>
gap> Length(MaximalSubsemigroups(S)) = 3;
true

#T# maximal; MaximalSubsemigroups, for a semigroup 2/?
gap> S := Semigroup([
> Transformation([1, 1, 2, 1, 4, 4]),
> Transformation([1, 3, 1, 5, 1, 3])]);
<transformation semigroup of degree 6 with 2 generators>
gap> Length(MaximalSubsemigroups(S)) = 2;
true

#T# maximal; MaximalSubsemigroups, for a semigroup 3/?
gap> S := Monoid(Transformation([1, 1, 2]));
<commutative transformation monoid of degree 3 with 1 generator>
gap> Length(MaximalSubsemigroups(S)) = 2;
true

#T# maximal: MaximalSubsemigroups, for a semigroup 4/?
gap> S := Monoid([
> Transformation([1, 1, 1, 1, 1, 1]),
> Transformation([1, 2, 3, 4, 5, 4]),
> Transformation([1, 3, 2, 5, 4, 3])]);
<transformation monoid of degree 6 with 3 generators>
gap> Length(MaximalSubsemigroups(S)) = 5;
true

#T# maximal: MaximalSubsemigroups, for a semigroup 5/?
gap> S := Monoid([
> Transformation([1, 1, 2, 1, 4, 1, 6, 1, 8, 6]),
> Transformation([1, 3, 1, 5, 1, 7, 1, 9, 1, 3]),
> Transformation([1, 4, 1, 2, 1, 8, 1, 6, 1, 4])]);
<transformation monoid of degree 10 with 3 generators>
gap> Length(MaximalSubsemigroups(S)) = 5;
true

#T# maximal: MaximalSubsemigroups, for a semigroup 6/?
gap> S := Semigroup([
> Transformation([2, 2, 2, 1]), Transformation([2, 3, 4, 3])]);
<transformation semigroup of degree 4 with 2 generators>
gap> Length(MaximalSubsemigroups(S));
3

#T# SEMIGROUPS_UnbindVariables

#E#
gap> STOP_TEST("Semigroups package: standard/attributes/maximal.tst");
