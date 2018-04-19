#############################################################################
##
#W  standard/gracreg.tst
#Y  Copyright (C) 2015                                      Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/gracreg.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# gracreg: RhoCosets, for a regular Greens class of an acting semigroup, 1
gap> S := RegularSemigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])], rec(acting := true));;
gap> L := LClass(S, S.1);;
gap> HasIsRegularGreensClass(L) and IsRegularGreensClass(L);
true
gap> RhoCosets(L);
[ () ]

# gracreg: LambdaCosets, for a regular Greens class of an acting semigroup, 1
gap> S := RegularSemigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])], rec(acting := true));;
gap> L := LClass(S, S.1);;
gap> HasIsRegularGreensClass(L) and IsRegularGreensClass(L);
true
gap> LambdaCosets(L);
[ () ]

# gracreg: SchutzenbergerGroup, for a regular acting D-class, 1
gap> S := RegularSemigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])], rec(acting := true));;
gap> D := DClass(S, S.1);;
gap> SchutzenbergerGroup(D);
Group([ (1,2), (1,3,2) ])

# gracreg: SchutzenbergerGroup, for an H-class of a regular acting semigroup, 1
gap> S := RegularSemigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])], rec(acting := true));;
gap> H := HClass(S, S.1);;
gap> SchutzenbergerGroup(H);
Group([ (1,2), (1,3,2) ])

# gracreg: Size, for a regular D-class of an acting semigroup, 1
gap> S := RegularSemigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])]);;
gap> D := DClass(S, S.1);;
gap> Size(D);
504

# gracreg: (D/R)ClassReps, for a regular D-class of an acting semigroup, 1
gap> S := RegularSemigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])], rec(acting := true));;
gap> DClassReps(S);
[ Transformation( [ 1, 2, 3, 3, 1 ] ), Transformation( [ 2, 4, 3, 5, 5 ] ), 
  Transformation( [ 1, 1, 2, 2, 1 ] ), Transformation( [ 5, 5, 5, 5, 5 ] ) ]
gap> RClassReps(S);
[ Transformation( [ 1, 2, 3, 3, 1 ] ), Transformation( [ 2, 3, 3, 1, 1 ] ), 
  Transformation( [ 1, 2, 3, 1, 3 ] ), Transformation( [ 2, 1, 3, 3, 3 ] ), 
  Transformation( [ 3, 1, 2, 3, 3 ] ), Transformation( [ 1, 1, 2, 1, 3 ] ), 
  Transformation( [ 1, 1, 2, 3, 3 ] ), Transformation( [ 1, 3, 2, 3, 3 ] ), 
  Transformation( [ 3, 1, 3, 3, 2 ] ), Transformation( [ 2, 1, 3, 2, 1 ] ), 
  Transformation( [ 3, 1, 1, 3, 2 ] ), Transformation( [ 1, 3, 1, 2, 2 ] ), 
  Transformation( [ 2, 4, 3, 5, 5 ] ), Transformation( [ 5, 2, 4, 5, 3 ] ), 
  Transformation( [ 1, 2, 2, 2, 1 ] ), Transformation( [ 2, 2, 2, 1, 1 ] ), 
  Transformation( [ 2, 1, 2, 1, 1 ] ), Transformation( [ 1, 1, 2, 1, 1 ] ), 
  Transformation( [ 1, 1, 2, 2, 1 ] ), Transformation( [ 2, 1, 1, 2, 2 ] ), 
  Transformation( [ 1, 1, 2, 1, 2 ] ), Transformation( [ 1, 1, 2, 2, 2 ] ), 
  Transformation( [ 1, 2, 2, 2, 2 ] ), Transformation( [ 2, 1, 2, 2, 2 ] ), 
  Transformation( [ 1, 1, 1, 1, 2 ] ), Transformation( [ 1, 2, 2, 1, 2 ] ), 
  Transformation( [ 1, 2, 1, 1, 2 ] ), Transformation( [ 5, 5, 5, 5, 5 ] ) ]

# gracreg: Greens(D/R)Classes, for a regular D-class of an acting semigroup, 1
gap> S := RegularSemigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])], rec(acting := true));;
gap> GreensDClasses(S);
[ <Green's D-class: Transformation( [ 1, 2, 3, 3, 1 ] )>, 
  <Green's D-class: Transformation( [ 2, 4, 3, 5, 5 ] )>, 
  <Green's D-class: Transformation( [ 1, 2, 2, 2, 1 ] )>, 
  <Green's D-class: Transformation( [ 5, 5, 5, 5, 5 ] )> ]
gap> GreensRClasses(S);
[ <Green's R-class: Transformation( [ 1, 2, 3, 3, 1 ] )>, 
  <Green's R-class: Transformation( [ 2, 3, 3, 1, 1 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 3, 1, 3 ] )>, 
  <Green's R-class: Transformation( [ 2, 1, 3, 3, 3 ] )>, 
  <Green's R-class: Transformation( [ 3, 1, 2, 3, 3 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 2, 1, 3 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 2, 3, 3 ] )>, 
  <Green's R-class: Transformation( [ 1, 3, 2, 3, 3 ] )>, 
  <Green's R-class: Transformation( [ 3, 1, 3, 3, 2 ] )>, 
  <Green's R-class: Transformation( [ 2, 1, 3, 2, 1 ] )>, 
  <Green's R-class: Transformation( [ 3, 1, 1, 3, 2 ] )>, 
  <Green's R-class: Transformation( [ 1, 3, 1, 2, 2 ] )>, 
  <Green's R-class: Transformation( [ 2, 4, 3, 5, 5 ] )>, 
  <Green's R-class: Transformation( [ 5, 2, 4, 5, 3 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 2, 2, 1 ] )>, 
  <Green's R-class: Transformation( [ 2, 2, 2, 1, 1 ] )>, 
  <Green's R-class: Transformation( [ 2, 1, 2, 1, 1 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 2, 1, 1 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 2, 2, 1 ] )>, 
  <Green's R-class: Transformation( [ 2, 1, 1, 2, 2 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 2, 1, 2 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 2, 2, 2 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 2, 2, 2 ] )>, 
  <Green's R-class: Transformation( [ 2, 1, 2, 2, 2 ] )>, 
  <Green's R-class: Transformation( [ 1, 1, 1, 1, 2 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 2, 1, 2 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 1, 1, 2 ] )>, 
  <Green's R-class: Transformation( [ 5, 5, 5, 5, 5 ] )> ]

# gracreg: Nr(D/R/L/H)Classes: for a regular acting semigroup with gens
gap> S := RegularSemigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])], rec(acting := true));;
gap> IsActingSemigroup(S);
true
gap> HasGeneratorsOfSemigroup(S);
true
gap> NrDClasses(S);
4
gap> NrRClasses(S);
28
gap> NrLClasses(S);
23
gap> NrHClasses(S);
210

# gracreg: Nr(R/L)Classes: for a regular D-class of an acting semigroup
gap> S := RegularSemigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])], rec(acting := true));;
gap> D := DClass(S, S.3);;
gap> NrRClasses(D);
2
gap> NrLClasses(D);
2
gap> NrHClasses(D) = last * last2;
true

# gracreg: NrHClasses: for a regular D/R/L class of an acting semigroup
gap> S := RegularSemigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])], rec(acting := true));;
gap> D := DClass(S, S.2);;
gap> NrHClasses(D);
4
gap> R := RClass(S, S.2);;
gap> NrHClasses(R);
2
gap> L := LClass(S, S.2);;
gap> NrHClasses(L);
2

# gracreg: PartialOrderOfDClasses for a regular acting semigroup w gens 1
gap> S := FullTransformationMonoid(4);;
gap> S := RegularSemigroup(S, rec(acting := true));;
gap> OutNeighbours(DigraphReflexiveTransitiveReduction(Digraph(
> PartialOrderOfDClasses(S))));
[ [ 2 ], [ 3 ], [ 4 ], [  ] ]

# gracreg: NrIdempotents, for a regular acting semigroup, 1
gap> S := FullTransformationSemigroup(4);;
gap> NrIdempotents(S);
41
gap> S := RegularSemigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])], rec(acting := true));;
gap> NrIdempotents(S);
108

# gracreg: NrRegularDClasses, for a regular acting semigroup, 1
gap> S := RegularSemigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])], rec(acting := true));;
gap> NrRegularDClasses(S) = NrDClasses(S);
true
gap> NrRegularDClasses(S);
4

# gracreg: IteratorOf(D/L)Classes, for a regular acting semigroup, 1
gap> S := RegularSemigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])], rec(acting := true));;
gap> IteratorOfDClasses(S);
<iterator of D-classes>
gap> GreensDClasses(S);;
gap> IteratorOfDClasses(S);
<iterator>
gap> IteratorOfLClasses(S);
<iterator of L-classes>
gap> GreensLClasses(S);;
gap> IteratorOfLClasses(S);
<iterator>

# gracreg: EnumeratorOfRClasses, for a regular acting semigroup, 1
gap> S := RegularSemigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])], rec(acting := true));;
gap> e := EnumeratorOfRClasses(S);
<enumerator of R-classes of <regular transformation semigroup of degree 5 
 with 3 generators>>
gap> Length(e) = NrRClasses(S);
true
gap> e[1];
<Green's R-class: Transformation( [ 1, 2, 3, 3, 1 ] )>
gap> e[Length(e)];
<Green's R-class: Transformation( [ 2, 3, 3, 3, 3 ] )>
gap> e[Length(e) + 1];
fail
gap> Position(e, e[1]);
1
gap> Position(e, e[28]);
28
gap> Position(e,
> RClass(FullTransformationMonoid(5),
>        Transformation([1, 2, 3, 3, 1])));
1
gap> Position(e,
> RClass(FullTransformationMonoid(6),
>        Transformation([3, 1, 2, 6, 4, 5])));
fail
gap> RClass(FullTransformationMonoid(6),
> Transformation([3, 1, 2, 6, 4, 5])) in e;
false
gap> e[1] in e;
true

#Test \< for Green's classes of a regular semigroup
gap> S := SymmetricInverseMonoid(5);;
gap> Set([DClass(S, S.2) < DClass(S, S.3), DClass(S, S.3) < DClass(S, S.2)]);
[ true, false ]
gap> DClass(S, S.3) < DClass(S, S.3);
false
gap> Set([RClass(S, S.2) < RClass(S, S.3), RClass(S, S.3) < RClass(S, S.2)]);
[ true, false ]
gap> RClass(S, S.3) < RClass(S, S.3);
false
gap> Set([LClass(S, S.2) < LClass(S, S.3), LClass(S, S.3) < LClass(S, S.2)]);
[ true, false ]
gap> LClass(S, S.3) < LClass(S, S.3);
false
gap> T := Semigroup(Transformation([2, 4, 3, 4]));;
gap> RClass(S, S.2) < RClass(T, T.1);
false
gap> DClass(S, S.2) < DClass(T, T.1);
false

#Test \< for regular Green's classes of a non-regular semigroup
gap> S := Semigroup(Transformation([6, 5, 3, 3, 3, 1]), 
>                   Transformation([5, 2, 5, 5, 1, 4]));;
gap> R1 := GreensRClassOfElement(S, Transformation([1, 3, 3, 3, 3]));;
gap> R2 := GreensRClassOfElement(S, Transformation([1, 2, 1, 1, 5, 5]));;
gap> IsRegularGreensClass(R1); IsRegularGreensClass(R2);
true
true
gap> Set([R1 < R2, R2 < R1]);
[ true, false ]
gap> L1 := GreensLClassOfElement(S, Transformation([1, 3, 3, 3, 3]));;
gap> L2 := GreensLClassOfElement(S, Transformation([1, 2, 1, 1, 5, 5]));;
gap> IsRegularGreensClass(L1); IsRegularGreensClass(L2);
true
true
gap> Set([L1 < L2, L2 < L1]);
[ true, false ]
gap> D1 := GreensDClassOfElement(S, Transformation([1, 3, 3, 3, 3]));;
gap> D2 := GreensDClassOfElement(S, Transformation([1, 2, 1, 1, 5, 5]));;
gap> IsRegularGreensClass(D1); IsRegularGreensClass(D2);
true
true
gap> Set([D1 < D2, D2 < D1]);
[ true, false ]

# Test NrIdempotents for a regular star bipartition semigroup
# This test takes too long!
# gap> S := JonesMonoid(15);
# <regular bipartition *-monoid of degree 15 with 14 generators>
# gap> NrIdempotents(S);
# 3923351 
gap> S := JonesMonoid(10);
<regular bipartition *-monoid of degree 10 with 9 generators>
gap> NrIdempotents(S);
8944
gap> S := Semigroup(JonesMonoid(0), rec(acting := true));;
gap> NrIdempotents(S);
1
gap> NrIdempotentsByRank(S);
[ 1 ]
gap> S := Semigroup(JonesMonoid(3), rec(acting := true));
<bipartition monoid of degree 3 with 2 generators>
gap> IsRegularSemigroup(S) and IsStarSemigroup(S);
true
gap> NrIdempotents(S);
5
gap> NrIdempotentsByRank(S);
[ 0, 4, 0, 1 ]

# Test Size for a regular D-class
gap> S := PartitionMonoid(4);;
gap> D := DClass(S, S.4);;
gap> IsRegularDClass(D);
true
gap> Size(D);
600

# Test NrLClasses for a regular D-class
gap> S := PartitionMonoid(3);;
gap> D := DClass(S, S.1);;
gap> IsRegularDClass(D);
true
gap> NrLClasses(D);
1

# SEMIGROUPS_UnbindVariables
gap> Unbind(D);
gap> Unbind(H);
gap> Unbind(L);
gap> Unbind(R);
gap> Unbind(S);
gap> Unbind(e);

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/gracreg.tst");
