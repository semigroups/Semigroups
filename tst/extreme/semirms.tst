############################################################################
##
#W  extreme/semirms.tst
#Y  Copyright (C) 2016                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local A, D, G, H, M, R, S, T, U, UU, V, a, acting, an, comps, d, e, eV, f, f1
#@local f2, f3, f4, f5, f6, f7, f8, first_occurrence, g, gens, h, i, id, idems
#@local inj, inv, iso, l, map, mat, reps, t1, t2, x, y, z, zero
gap> START_TEST("Semigroups package: extreme/semirms.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();
gap> SEMIGROUPS.DefaultOptionsRec.acting := true;;

# ReesMatTest2
# Some semigroups to which the methods in Semigroups should not apply
gap> R := ReesZeroMatrixSemigroup(POI(5), [[0, 0, 0], [0, 0, 0]]);
<Rees 0-matrix semigroup 3x2 over <inverse partial perm monoid of size 252, 
  rank 5 with 5 generators>>
gap> R := Semigroup(Generators(R));
<subsemigroup of 3x2 Rees 0-matrix semigroup with 1512 generators>
gap> R := ReesZeroMatrixSemigroup(Group(()), [[0, 0, 0], [0, 0, 0]]);
<Rees 0-matrix semigroup 3x2 over Group(())>
gap> R := ReesZeroMatrixSemigroup(Group(()), [[0, 0, 0], [0, 0, 0]]);
<Rees 0-matrix semigroup 3x2 over Group(())>
gap> R := Semigroup(Generators(R));
<subsemigroup of 3x2 Rees 0-matrix semigroup with 6 generators>
gap> R := ReesZeroMatrixSemigroup(POI(5), [[PartialPerm([], []), 0],
> [0, PartialPerm([], [])]]);
<Rees 0-matrix semigroup 2x2 over <inverse partial perm monoid of size 252, 
  rank 5 with 5 generators>>
gap> R := Semigroup(Generators(R));
<subsemigroup of 2x2 Rees 0-matrix semigroup with 1008 generators>

# ReesMatTest3
# Find a source of interesting subsemigroups of Rees 0-matrix semigroups
gap> S := Semigroup(
>  Transformation([1, 1, 2, 7, 9, 8, 5, 9, 6]),
>  Transformation([1, 1, 7, 2, 8, 9, 9, 5, 6]),
>  Transformation([1, 2, 2, 3, 6, 5, 5, 7]),
>  Transformation([1, 2, 2, 3, 6, 9, 9, 7, 5]),
>  Transformation([1, 2, 2, 3, 7, 5, 5, 7]),
>  Transformation([1, 2, 2, 3, 7, 9, 9, 7, 5]),
>  Transformation([1, 2, 3, 1]), Transformation([1, 2, 3, 3]),
>  Transformation([1, 2, 3, 6, 5, 6]),
>  Transformation([1, 2, 3, 9, 5, 6, 7, 8, 9]),
>  Transformation([1, 2, 7, 8, 9, 5, 5, 9, 6]),
>  Transformation([1, 2, 8, 2, 7, 9, 9, 6, 5]),
>  Transformation([1, 2, 8, 2, 9, 7, 6, 9, 5]),
>  Transformation([2, 1, 1, 3, 6, 5, 5, 7]),
>  Transformation([2, 1, 1, 3, 6, 9, 9, 7, 5]),
>  Transformation([2, 1, 1, 3, 7, 5, 5, 7]),
>  Transformation([2, 1, 1, 3, 7, 9, 9, 7, 5]),
>  Transformation([2, 1, 1, 6, 9, 7, 6, 9, 5]),
>  Transformation([2, 1, 1, 8, 9, 6, 7, 9, 5]),
>  Transformation([2, 1, 1, 8, 9, 7, 6, 9, 5]),
>  Transformation([2, 1, 8, 1, 7, 9, 9, 6, 5]),
>  Transformation([2, 1, 8, 6, 9, 7, 6, 9, 5]),
>  Transformation([5, 5, 8, 1, 7, 2, 1, 6]),
>  Transformation([5, 8, 2, 3, 6, 9, 9, 7, 1]),
>  Transformation([5, 8, 7, 2, 9, 1, 1, 9, 6]),
>  Transformation([6, 6, 9, 4, 8, 1, 1, 8, 9]),
>  Transformation([6, 7, 1, 8, 9, 5, 5, 9, 2]),
>  Transformation([6, 7, 6, 4, 5, 1, 2]),
>  Transformation([6, 7, 7, 8, 9, 1, 2, 9, 5]),
>  Transformation([6, 7, 7, 8, 9, 2, 1, 9, 5]),
>  Transformation([6, 7, 8, 1, 9, 2, 1, 9, 5]),
>  Transformation([6, 7, 9, 4, 5, 1, 2, 8, 9]),
>  Transformation([7, 6, 8, 2, 9, 1, 1, 9, 5]),
>  Transformation([7, 6, 8, 6, 9, 2, 1, 9, 5]),
>  Transformation([8, 5, 2, 7, 9, 6, 6, 9, 1]),
>  Transformation([9, 9, 2, 7, 8, 6, 6, 5, 1]),
>  Transformation([9, 9, 3, 2, 6, 5, 8, 7, 1]),
>  Transformation([9, 9, 3, 4, 5, 6, 7, 8, 1]),
>  Transformation([9, 9, 8, 7, 2, 6, 6, 1, 5]));;
gap> R := PrincipalFactor(DClasses(S)[40]);
<Rees 0-matrix semigroup 26x5 over Group([ (5,8)(6,9), (1,6,9), (1,6) ])>
gap> U := MaximalSubsemigroups(R){[31 .. 36]};
[ <subsemigroup of 26x5 Rees 0-matrix semigroup with 47 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 51 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 39 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 53 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 41 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 45 generators> ]
gap> V := Semigroup(MultiplicativeZero(R),
> RMSElement(R, 13, (1, 6)(5, 8), 3),
> RMSElement(R, 1, (1, 6), 3),
> RMSElement(R, 7, (1, 6)(5, 8), 3),
> RMSElement(R, 23, (5, 8), 2),
> RMSElement(R, 22, (1, 6), 1),
> RMSElement(R, 11, (1, 9), 5),
> RMSElement(R, 2, (1, 6), 5),
> RMSElement(R, 24, (1, 6)(5, 8), 4),
> RMSElement(R, 6, (1, 9)(5, 8), 1),
> RMSElement(R, 15, (1, 9)(5, 8), 2),
> RMSElement(R, 22, (1, 9), 1));;

# ReesMatTest4: from attributes.xml...

# ReesMatTest5: StuctureDescriptionMaximalSubgroups
gap> StructureDescriptionMaximalSubgroups(U[1]);
[ "1", "D12" ]
gap> StructureDescriptionMaximalSubgroups(V);
[ "1", "D12" ]

# ReesMatTest6: StructureDescriptionSchutzenbergerGroups 
gap> StructureDescriptionSchutzenbergerGroups(U[5]);
[ "1", "D12" ]

# ReesMatTest7: MinimalDClass
gap> List(U, MinimalDClass);
[ <Green's D-class: 0>, <Green's D-class: 0>, <Green's D-class: 0>, 
  <Green's D-class: 0>, <Green's D-class: 0>, <Green's D-class: 0> ]
gap> MinimalDClass(V);
<Green's D-class: 0>

# ReesMatTest8: MaximalDClasses
gap> MaximalDClasses(V);
[ <Green's D-class: (13,(1,6)(5,8),3)>, <Green's D-class: (22,(1,6),1)> ]
gap> MaximalDClasses(U[4]);
[ <Green's D-class: (2,(5,8)(6,9),4)>, <Green's D-class: (1,(5,8)(6,9),1)> ]
gap> V := Semigroup(MultiplicativeZero(R),
> RMSElement(R, 13, (1, 6)(5, 8), 3),
> RMSElement(R, 1, (1, 6), 3),
> RMSElement(R, 7, (1, 6)(5, 8), 3),
> RMSElement(R, 23, (5, 8), 2),
> RMSElement(R, 22, (1, 6), 1),
> RMSElement(R, 11, (1, 9), 5),
> RMSElement(R, 2, (1, 6), 5),
> RMSElement(R, 24, (1, 6)(5, 8), 4),
> RMSElement(R, 6, (1, 9)(5, 8), 1),
> RMSElement(R, 15, (1, 9)(5, 8), 2),
> RMSElement(R, 22, (1, 9), 1));;

# ReesMatTest9: PrincipalFactor
gap> D := Filtered(DClasses(V), IsRegularGreensClass)[2];
<Green's D-class: (13,(1,6)(5,8),3)>
gap> inj := InjectionPrincipalFactor(D);; inv := InverseGeneralMapping(inj);;
gap> ForAll(D, x -> (x ^ inj) ^ inv = x);
true
gap> ForAll(D, x ->
>      ForAll(D, y ->
>        (not x * y in D) or (x * y) ^ inj = x ^ inj * y ^ inj));
true

# ReesMatTest10: SmallGeneratingSet
gap> Length(SmallGeneratingSet(V)) <= Length(Generators(V));
true
gap> Apply(U, x -> Semigroup(SmallGeneratingSet(x)));

# ReesMatTest11: MinimalIdeal
gap> MinimalIdeal(V);
<simple Rees 0-matrix semigroup ideal with 1 generator>
gap> List(U, MinimalIdeal);
[ <simple Rees 0-matrix semigroup ideal with 1 generator>, 
<simple Rees 0-matrix semigroup ideal with 1 generator>, 
<simple Rees 0-matrix semigroup ideal with 1 generator>, 
<simple Rees 0-matrix semigroup ideal with 1 generator>, 
<simple Rees 0-matrix semigroup ideal with 1 generator>, 
<simple Rees 0-matrix semigroup ideal with 1 generator> ]

# ReesMatTest12: IsomorphismPermGroup
gap> R := ReesZeroMatrixSemigroup(QuaternionGroup(IsPermGroup, 8), [[()]]);;
gap> T := Semigroup(Filtered(Generators(R), x -> x![1] <> 0));;
gap> iso := IsomorphismPermGroup(T);;
gap> Source(iso) = T;
true
gap> Range(iso);
Group([ (1,5,3,7)(2,8,4,6), (1,2,3,4)(5,6,7,8) ])
gap> inv := InverseGeneralMapping(iso);;
gap> Source(inv);
Group([ (1,5,3,7)(2,8,4,6), (1,2,3,4)(5,6,7,8) ])
gap> Range(inv) = T;
true
gap> ForAll(T, x -> (x ^ iso) ^ inv = x);
true
gap> ForAll(T, x -> ForAll(T, y -> (x * y) ^ iso = x ^ iso * y ^ iso));
true
gap> iso := IsomorphismPermGroup(MinimalIdeal(V));;
gap> IsTrivial(Range(iso));
true
gap> inv := InverseGeneralMapping(iso);;
gap> ForAll(MinimalIdeal(V), x -> (x ^ iso) ^ inv = x);
true
gap> ForAll(MinimalIdeal(V), x ->
>      ForAll(MinimalIdeal(V), y ->
>        (x * y) ^ iso = x ^ iso * y ^ iso));
true

# ReesMatTest13: GroupOfUnits
gap> R := Semigroup(Generators(R));
<subsemigroup of 1x1 Rees 0-matrix semigroup with 3 generators>
gap> GroupOfUnits(R);
<subsemigroup of 1x1 Rees 0-matrix semigroup with 8 generators>
gap> GroupOfUnits(V);
fail
gap> GroupOfUnits(U[5]);
fail

# ReesMatTest14: IdempotentGeneratedSubsemigroup
gap> eV := IdempotentGeneratedSubsemigroup(V);;
gap> Size(eV);
16
gap> Size(V);
421
gap> List(U, IdempotentGeneratedSubsemigroup);;
gap> List(last, Size);
[ 47, 47, 47, 47, 47, 47 ]
gap> last2[1] = last2[2];
true

# ReesMatTest15: IrredundantGeneratingSubset
gap> a := RMSElement(ParentAttr(V), 1, (1, 6), 3);
(1,(1,6),3)
gap> a in IrredundantGeneratingSubset(V);
true
gap> Length(IrredundantGeneratingSubset(V));
10
gap> U[1] = Semigroup(IrredundantGeneratingSubset(U[1]));
true

# ReesMatTest17: MultiplicativeNeutralElement (for an H-class)
gap> H := First(HClasses(V), IsRegularGreensClass);
<Green's H-class: 0>
gap> MultiplicativeNeutralElement(H);
0
gap> H := First(HClasses(V), x -> not IsRegularGreensClass(x));
<Green's H-class: (1,(1,6),3)>
gap> MultiplicativeNeutralElement(H);
fail
gap> h := RMSElement(ParentAttr(U[5]), 17, (1, 9)(5, 8), 5);
(17,(1,9)(5,8),5)
gap> H := GreensHClassOfElement(U[5], h);
<Green's H-class: (17,(1,9)(5,8),5)>
gap> IsRegularGreensClass(H);
true
gap> e := MultiplicativeNeutralElement(H);
(17,(1,6),5)
gap> e ^ 2;
(17,(1,6),5)
gap> e = h;
false
gap> ForAll(H, x -> x * e = x and e * x = x);
true
gap> h := RMSElement(ParentAttr(U[5]), 21, (1, 9, 6)(5, 8), 5);
(21,(1,9,6)(5,8),5)
gap> H := GreensHClassOfElement(U[5], h);
Error, the element does not belong to the semigroup
gap> IsRegularGreensClass(H);
true
gap> MultiplicativeNeutralElement(H);
(17,(1,6),5)

# ReesMatTest18: StructureDescription (for an H-class) 
gap> H := First(HClasses(U[5]), IsRegularGreensClass);;
gap> StructureDescription(H);
"D12"

# ReesMatTest19: Random
gap> Random(V);;
gap> List(U, Random);;  # FIXME(later) no this is not expected

# ReesMatTest20: DClassOf.Class etc
gap> H := First(HClasses(V), x -> not IsRegularGreensClass(x));
<Green's H-class: (1,(1,6),3)>
gap> DClass(H);
<Green's D-class: (1,(1,6),3)>
gap> RClass(H);
<Green's R-class: (1,(1,6),3)>
gap> LClass(H);
<Green's L-class: (1,(1,6),3)>
gap> LClass(H) < LClass(V, Representative(V));
false

# ReesMatTest21: DClasses etc...
gap> RClasses(V);
[ <Green's R-class: 0>, <Green's R-class: (13,(1,6)(5,8),3)>, 
  <Green's R-class: (1,(1,6),3)>, <Green's R-class: (7,(1,6)(5,8),3)>, 
  <Green's R-class: (23,(5,8)(6,9),3)>, <Green's R-class: (22,(1,6),1)>, 
  <Green's R-class: (11,(1,9,6),1)>, <Green's R-class: (2,(),1)>, 
  <Green's R-class: (24,(5,8),1)>, <Green's R-class: (6,(1,9)(5,8),1)>, 
  <Green's R-class: (15,(1,6,9)(5,8),3)>, <Green's R-class: (22,(),3)>, 
  <Green's R-class: (6,(1,9,6)(5,8),3)>, <Green's R-class: (11,(1,9),3)>, 
  <Green's R-class: (2,(1,6),3)>, <Green's R-class: (24,(1,6)(5,8),3)> ]
gap> LClasses(V);
[ <Green's L-class: 0>, <Green's L-class: (13,(1,6)(5,8),3)>, 
  <Green's L-class: (13,(1,9,6)(5,8),2)>, <Green's L-class: (1,(1,6),3)>, 
  <Green's L-class: (1,(1,9,6),2)>, <Green's L-class: (1,(1,6,9),3)>, 
  <Green's L-class: (1,(1,9),2)>, <Green's L-class: (1,(5,8),3)>, 
  <Green's L-class: (1,(5,8)(6,9),2)>, <Green's L-class: (1,(1,6)(5,8),3)>, 
  <Green's L-class: (1,(1,9,6)(5,8),2)>, <Green's L-class: (1,(1,6,9)(5,8),3)>
    , <Green's L-class: (1,(1,9)(5,8),2)>, <Green's L-class: (1,(),3)>, 
  <Green's L-class: (1,(6,9),2)>, <Green's L-class: (1,(1,9,6),3)>, 
  <Green's L-class: (1,(1,6),2)>, <Green's L-class: (1,(6,9),3)>, 
  <Green's L-class: (1,(),2)>, <Green's L-class: (1,(1,9)(5,8),3)>, 
  <Green's L-class: (1,(1,6,9)(5,8),2)>, <Green's L-class: (1,(1,9,6)(5,8),3)>
    , <Green's L-class: (1,(1,6)(5,8),2)>, <Green's L-class: (1,(5,8)(6,9),3)>
    , <Green's L-class: (1,(5,8),2)>, <Green's L-class: (1,(1,9),3)>, 
  <Green's L-class: (1,(1,6,9),2)>, <Green's L-class: (22,(1,6),1)>, 
  <Green's L-class: (22,(),5)>, <Green's L-class: (22,(),4)>, 
  <Green's L-class: (22,(),3)>, <Green's L-class: (22,(6,9),2)> ]
gap> DClasses(V);
[ <Green's D-class: 0>, <Green's D-class: (13,(1,6)(5,8),3)>, 
  <Green's D-class: (1,(1,6),3)>, <Green's D-class: (22,(1,6),1)>, 
  <Green's D-class: (22,(),3)> ]
gap> NrHClasses(V);
58
gap> NrLClasses(V);
32
gap> reps := ShallowCopy(RClassReps(U[2]));;
gap> Sort(reps);
gap> List(reps, RowOfReesZeroMatrixSemigroupElement) = [0 .. 26];
false
gap> LClassReps(R);
[ (1,(1,5,3,7)(2,8,4,6),1), 0 ]
gap> d := DClassReps(U[4]);;
gap> Length(d);
4
gap> IsDuplicateFreeList(d);
true
gap> ForAll(d, x -> x in U[4]);
true
gap> MultiplicativeZero(U[4]) in d;
true

# ReesMatTest22: MultiplicativeZero
gap> List(U, MultiplicativeZero);
[ 0, 0, 0, 0, 0, 0 ]
gap> ForAll(last, IsIdempotent);
true

# ReesMatTest23: GroupHClass
gap> D := Filtered(DClasses(U[1]), IsRegularGreensClass)[2];
<Green's D-class: (7,(5,8)(6,9),2)>
gap> GroupHClass(D);
<Green's H-class: (7,(),2)>
gap> StructureDescription(last);
"D12"
gap> D := First(DClasses(V), IsRegularGreensClass);
<Green's D-class: 0>
gap> GroupHClass(D);
<Green's H-class: 0>
gap> StructureDescription(last);
"1"

# ReesMatTest24: Idempotents
gap> Idempotents(V);
[ 0, (13,(1,9,6)(5,8),3), (13,(1,9,6)(5,8),2), (7,(),3), (7,(),2), (23,(),3), 
  (23,(),2), (22,(),5), (22,(),4), (11,(5,8)(6,9),1), (2,(),5), (2,(),4), 
  (24,(),1), (6,(1,6),1), (15,(1,6),3), (15,(1,6),2) ]
gap> ForAll(last, IsIdempotent);
true
gap> reps := ShallowCopy(Idempotents(U[2]));;
gap> Sort(reps);
gap> reps;
[ 0, (1,(),1), (2,(),4), (2,(),5), (3,(5,8)(6,9),4), (3,(5,8)(6,9),5), 
  (4,(5,8)(6,9),4), (4,(5,8)(6,9),5), (5,(5,8)(6,9),4), (5,(5,8)(6,9),5), 
  (6,(1,6),1), (7,(),2), (7,(),3), (8,(1,9)(5,8),2), (8,(1,9)(5,8),3), 
  (9,(1,9),2), (9,(1,9),3), (10,(),1), (11,(5,8)(6,9),1), (12,(1,9),1), 
  (13,(1,9,6)(5,8),2), (13,(1,9,6)(5,8),3), (14,(1,9,6),2), (14,(1,9,6),3), 
  (15,(1,6),2), (15,(1,6),3), (16,(1,6)(5,8),2), (16,(1,6)(5,8),3), 
  (17,(1,6),4), (17,(1,6),5), (18,(1,9)(5,8),4), (18,(1,9)(5,8),5), 
  (19,(1,9,6)(5,8),2), (19,(1,9,6)(5,8),3), (20,(1,9,6)(5,8),2), 
  (20,(1,9,6)(5,8),3), (21,(1,9,6)(5,8),2), (21,(1,9,6)(5,8),3), (22,(),4), 
  (22,(),5), (23,(),2), (23,(),3), (24,(),1), (25,(),4), (25,(),5), 
  (26,(1,9),2), (26,(1,9),3) ]
gap> ForAll(last, IsIdempotent);
true

# ReesMatTest25: IsRegularGreensClass
gap> Number(RClasses(V), IsRegularGreensClass);
10
gap> Number(DClasses(V), IsRegularGreensClass);
3
gap> NrRegularDClasses(V);
3
gap> Number(DClasses(U[4]), IsRegularGreensClass);
3
gap> NrRegularDClasses(U[4]);
3
gap> Number(LClasses(U[4]), IsRegularGreensClass);
6

# ReesMatTest26: NrIdempotents
gap> NrIdempotents(V) = Length(Idempotents(V));
true
gap> ForAll(U, x -> NrIdempotents(x) = Length(Idempotents(x)));
true
gap> List(DClasses(V), NrIdempotents);
[ 1, 8, 0, 7, 0 ]
gap> List(RClasses(V), NrIdempotents);
[ 1, 2, 0, 2, 2, 2, 1, 2, 1, 1, 2, 0, 0, 0, 0, 0 ]
gap> List(LClasses(R), NrIdempotents);
[ 1, 1 ]

# ReesMatTest27: PartialOrderOfDClasses
gap> PartialOrderOfDClasses(V);
<immutable digraph with 5 vertices, 7 edges>
gap> PartialOrderOfDClasses(U[1]);
<immutable digraph with 4 vertices, 5 edges>
gap> PartialOrderOfDClasses(U[2]);
<immutable digraph with 4 vertices, 5 edges>

# ReesMatTest28: from properties.xml...
gap> IsBand(V);
false
gap> List(U, IsBand);
[ false, false, false, false, false, false ]
gap> IsBlockGroup(V);
false
gap> List(U, IsBlockGroup);
[ false, false, false, false, false, false ]
gap> IsBrandtSemigroup(V);
false
gap> List(U, IsBrandtSemigroup);
[ false, false, false, false, false, false ]
gap> IsCliffordSemigroup(V);
false
gap> List(U, IsCliffordSemigroup);
[ false, false, false, false, false, false ]
gap> IsCommutativeSemigroup(V);
false
gap> List(U, IsCommutativeSemigroup);
[ false, false, false, false, false, false ]
gap> IsCompletelyRegularSemigroup(V);
false
gap> List(U, IsCompletelyRegularSemigroup);
[ false, false, false, false, false, false ]
gap> IsDTrivial(V);
false
gap> IsRTrivial(V);
false
gap> IsLTrivial(V);
false
gap> IsHTrivial(V);
false
gap> List(U, IsGroupAsSemigroup);
[ false, false, false, false, false, false ]
gap> List(U, IsIdempotentGenerated);
[ false, false, false, false, false, false ]
gap> List(U, IsInverseSemigroup);
[ false, false, false, false, false, false ]
gap> R := ReesZeroMatrixSemigroup(QuaternionGroup(IsPermGroup, 8),
> [[(), (), ()]]);;
gap> R := Semigroup(Difference(Generators(R), [MultiplicativeZero(R)]));
<subsemigroup of 3x1 Rees 0-matrix semigroup with 4 generators>
gap> IsRightSimple(R);
false
gap> IsLeftSimple(R);
true
gap> IsCompletelyRegularSemigroup(R);
true
gap> R := ReesZeroMatrixSemigroup(Group(()), [[(), (), (), (), ()]]);;
gap> R := Semigroup(Difference(Generators(R), [MultiplicativeZero(R)]));
<subsemigroup of 5x1 Rees 0-matrix semigroup with 5 generators>
gap> IsLeftZeroSemigroup(R);
true
gap> IsRightZeroSemigroup(R);
false
gap> IsMonogenicSemigroup(R);
false
gap> List(U, IsMonogenicSemigroup);
[ false, false, false, false, false, false ]
gap> R := ReesZeroMatrixSemigroup(Group(()), [[()]]);
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> R := Semigroup(Generators(R));
<subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators>
gap> IsMonoidAsSemigroup(R);
true
gap> List(U, IsMonoidAsSemigroup);
[ false, false, false, false, false, false ]
gap> IsomorphismTransformationSemigroup(R);
<subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators> -> 
<commutative transformation monoid of size 2, degree 2 with 1 generator>
gap> IsOrthodoxSemigroup(R);
true
gap> IsOrthodoxSemigroup(V);
false
gap> List(U, IsOrthodoxSemigroup);
[ false, false, false, false, false, false ]
gap> R := ReesZeroMatrixSemigroup(Group(()), [[(), (), ()], [(), (), ()]]);
<Rees 0-matrix semigroup 3x2 over Group(())>
gap> R := Semigroup(Difference(Generators(R), [MultiplicativeZero(R)]));
<subsemigroup of 3x2 Rees 0-matrix semigroup with 4 generators>
gap> IsRectangularBand(R);
true
gap> IsRectangularBand(V);
false
gap> List(U, IsRectangularBand);
[ false, false, false, false, false, false ]
gap> List(U, IsRegularSemigroup);
[ false, false, false, false, false, false ]
gap> IsRegularSemigroup(V);
false
gap> UU := IdempotentGeneratedSubsemigroup(R);;
gap> IsSemilattice(UU);
false
gap> IsSimpleSemigroup(V);
false
gap> List(U, IsSimpleSemigroup);
[ false, false, false, false, false, false ]
gap> R;
<subsemigroup of 3x2 Rees 0-matrix semigroup with 4 generators>
gap> IsSimpleSemigroup(V);
false
gap> IsSimpleSemigroup(R);
true
gap> f := IsomorphismReesMatrixSemigroup(R); g := InverseGeneralMapping(f);;
<subsemigroup of 3x2 Rees 0-matrix semigroup with 4 generators> -> 
<Rees matrix semigroup 3x2 over Group(())>
gap> ForAll(R, x -> (x ^ f) ^ g = x);
true
gap> ForAll(R, x -> ForAll(R, y -> (x * y) ^ f = x ^ f * y ^ f));
true
gap> R := ReesZeroMatrixSemigroup(Group(()), [[(), (), ()], [(), (), ()]]);
<Rees 0-matrix semigroup 3x2 over Group(())>
gap> R := Semigroup(Generators(R));
<subsemigroup of 3x2 Rees 0-matrix semigroup with 5 generators>
gap> IsZeroGroup(R);
false
gap> IsZeroRectangularBand(R);
true
gap> IsZeroSimpleSemigroup(R);
true
gap> R := ReesZeroMatrixSemigroup(Group(()), [[(), (), ()],
>                                             [(), 0, 0],
>                                             [(), 0, 0]]);
<Rees 0-matrix semigroup 3x3 over Group(())>
gap> R := ReesZeroMatrixSubsemigroup(R, [2, 3], Group(()), [2, 3]);
<Rees 0-matrix semigroup 2x2 over Group(())>
gap> R := Semigroup(Generators(R));
<subsemigroup of 3x3 Rees 0-matrix semigroup with 4 generators>
gap> Size(R);
5
gap> IsZeroSemigroup(R);
true
gap> IsZeroSemigroup(V);
false

# ReesMatTest29: from semigroups.xml ...
gap> gens := Generators(V);;
gap> V := Semigroup(gens[1]);
<subsemigroup of 26x5 Rees 0-matrix semigroup with 1 generator>
gap> for i in [2 .. 12] do
> V := ClosureSemigroup(V, gens[i]);
> od;
gap> V;
<subsemigroup of 26x5 Rees 0-matrix semigroup with 11 generators>
gap> Size(V);
421

# ReesMatTest30: from slp.xml...
gap> x := RMSElement(V, 22, (1, 6, 9), 1);;
gap> Factorization(V, x);
[ 6, 7, 6, 9, 6 ]
gap> EvaluateWord(Generators(V), last);
(22,(1,6,9),1)
gap> x := MultiplicativeZero(R);;
gap> Factorization(R, x);
[ 1, 1 ]
gap> EvaluateWord(Generators(R), last);
0
gap> x := RMSElement(U[4], 26, (6, 9), 5);;
gap> Factorization(U[4], x);;  # = [ 7, 24, 8, 5]
gap> EvaluateWord(Generators(U[4]), last);
(26,(6,9),5)

# ReesMatTest31: Issue 108:
# IsRegularSemigroup for a RZMS returned false negative
gap> t1 := Transformation([4, 3, 1, 3]);;
gap> t2 := Transformation([3, 3, 2, 2]);;
gap> T := Semigroup([t1, t2]);;
gap> IsRegularSemigroup(T);
true
gap> IsGroup(T);
false
gap> mat := [[t2, t1], [t1, t2]];;
gap> R := ReesZeroMatrixSemigroup(T, mat);;
gap> IsRegularSemigroup(R);
true

# AutomorphismGroup of a ReesMatrixSemigroup
gap> G := Group(());;
gap> mat := List([1 .. 5], x -> List([1 .. 5], y -> ()));;
gap> M := ReesMatrixSemigroup(G, mat);
<Rees matrix semigroup 5x5 over Group(())>
gap> AutomorphismGroup(M);
<automorphism group of <Rees matrix semigroup 5x5 over Group(())> with 
5 generators>
gap> Size(last);
14400
gap> G := Group((1, 2, 3, 4, 5));;
gap> mat := [[(), (), (), (), ()],
> [(), (1, 4, 2, 5, 3), (1, 3, 5, 2, 4), (), (1, 4, 2, 5, 3)],
> [(), (1, 2, 3, 4, 5), (1, 2, 3, 4, 5), (), ()],
> [(), (1, 4, 2, 5, 3), (1, 5, 4, 3, 2), (1, 3, 5, 2, 4), ()],
> [(), (1, 2, 3, 4, 5), (1, 2, 3, 4, 5), (1, 5, 4, 3, 2), (1, 2, 3, 4, 5)]];;
gap> M := ReesMatrixSemigroup(G, mat);
<Rees matrix semigroup 5x5 over Group([ (1,2,3,4,5) ])>
gap> AutomorphismGroup(M);
<automorphism group of <Rees matrix semigroup 5x5 over Group([ (1,2,3,4,5) ])>
 with 1 generator>
gap> Size(last);
1
gap> M := Semigroup(Transformation([3, 3, 2, 6, 2, 4, 4, 6]),
> Transformation([5, 1, 7, 8, 7, 5, 8, 1]));;
gap> R := Range(IsomorphismReesMatrixSemigroup(M));;
gap> AutomorphismGroup(R);
<automorphism group of <Rees matrix semigroup 2x2 over Group(
 [ (2,3)(4,6), (2,3,4,6), (2,4,6,3) ])> with 5 generators>
gap> Size(last);
12
gap> G := SmallGroup(256, 4);;
gap> f1 := G.1;; f2 := G.2;; f3 := G.3;; f4 := G.4;;
gap> f5 := G.5;; f6 := G.6;; f7 := G.7;; f8 := G.8;;
gap> y := f2 * f3 * f4 * f5 * f6 * f7;;
gap> iso := IsomorphismPermGroup(G);;
gap> G := Range(iso);;
gap> y := y ^ iso;;
gap> mat := List([1 .. 3], x -> [One(G), y, One(G)]);;
gap> M := ReesMatrixSemigroup(G, mat);;
gap> AutomorphismGroup(M);;
gap> IsomorphismPermGroup(last);;
gap> Size(last2);
294912
gap> G := SymmetricGroup(7);; e := One(G);; mat := [[e], [e]];;
gap> R := ReesMatrixSemigroup(G, mat);
<Rees matrix semigroup 1x2 over Sym( [ 1 .. 7 ] )>
gap> AutomorphismGroup(R);
<automorphism group of <Rees matrix semigroup 1x2 over Sym( [ 1 .. 7 ] )>
  with 5041 generators>
gap> G := Group((1, 4, 3, 5, 2));;
gap> mat := [[(), (), ()], [(), (1, 4, 3, 5, 2), ()], [(), (1, 3, 2, 4, 5), ()]];;
gap> R := ReesMatrixSemigroup(G, mat);;
gap> l := (4, 6);
(4,6)
gap> g := GroupHomomorphismByImages(G, G, [(1, 4, 3, 5, 2)], [(1, 2, 5, 3, 4)]);
[ (1,4,3,5,2) ] -> [ (1,2,5,3,4) ]
gap> map := [(), (1, 5, 4, 2, 3), (), (), (), ()];
[ (), (1,5,4,2,3), (), (), (), () ]
gap> RMSIsoByTriple(R, R, [l, g, map]);
((4,6), GroupHomomorphismByImages( Group( [ (1,4,3,5,2) ] ), Group( 
[ (1,4,3,5,2) ] ), [ (1,4,3,5,2) ], [ (1,2,5,3,4) ] ), 
[ (), (1,5,4,2,3), (), (), (), () ])
gap> G := Group([(2, 5)(3, 4)]);;
gap> mat := [[(), (), (), (), ()], [(), (), (2, 5)(3, 4), (2, 5)(3, 4), ()], 
>   [(), (), (), (2, 5)(3, 4), (2, 5)(3, 4)], 
>   [(), (2, 5)(3, 4), (), (2, 5)(3, 4), ()], 
>   [(), (2, 5)(3, 4), (), (2, 5)(3, 4), ()]];;
gap> R := ReesMatrixSemigroup(G, mat);;
gap> A := AutomorphismGroup(R);;
gap> Size(A);
12

# ReesMatTest100: IsInverseSemigroup (easy true examples)
gap> R := ReesZeroMatrixSemigroup(Group(()), [[()]]);
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> IsInverseSemigroup(R);
true
gap> IsInverseSemigroup(AsSemigroup(IsTransformationSemigroup, R));
true

#
gap> T := Semigroup(Transformation([2, 1]));
<commutative transformation semigroup of degree 2 with 1 generator>
gap> IsGroupAsSemigroup(T);
true
gap> R := ReesZeroMatrixSemigroup(T, [[Transformation([2, 1])]]);
<Rees 0-matrix semigroup 1x1 over <transformation group of size 2, 
  degree 2 with 1 generator>>
gap> IsInverseSemigroup(R);
true
gap> IsInverseSemigroup(AsSemigroup(IsTransformationSemigroup, R));
true

# ReesMatTest101: IsInverseSemigroup (false because of underlying semigroup)
gap> x := Transformation([1, 1, 2]);;
gap> T := Semigroup(x);;
gap> IsInverseSemigroup(T);
false
gap> R := ReesZeroMatrixSemigroup(T, [[0, x], [0, x ^ 2]]);
<Rees 0-matrix semigroup 2x2 over <commutative non-regular transformation 
  semigroup of size 2, degree 3 with 1 generator>>
gap> IsInverseSemigroup(R);
false
gap> IsInverseSemigroup(AsSemigroup(IsTransformationSemigroup, R));
false

# T is known not to be regular
gap> T := Semigroup(x);;
gap> IsRegularSemigroup(T);
false
gap> R := ReesZeroMatrixSemigroup(T, [[0, x], [0, x ^ 2]]);;
gap> IsInverseSemigroup(R);
false
gap> IsInverseSemigroup(AsSemigroup(IsTransformationSemigroup, R));
false

# T is known not to be a monoid
gap> T := Semigroup(x);;
gap> IsMonoidAsSemigroup(T);
false
gap> R := ReesZeroMatrixSemigroup(T, [[0, x], [0, x ^ 2]]);;
gap> IsInverseSemigroup(R);
false
gap> IsInverseSemigroup(AsSemigroup(IsTransformationSemigroup, R));
false

# T is known not to have group of units
gap> T := Semigroup(x);;
gap> GroupOfUnits(T);
fail
gap> R := ReesZeroMatrixSemigroup(T, [[0, x], [0, x ^ 2]]);;
gap> IsInverseSemigroup(R);
false
gap> IsInverseSemigroup(AsSemigroup(IsTransformationSemigroup, R));
false

# T does not have a group of units
gap> T := Semigroup(x);;
gap> R := ReesZeroMatrixSemigroup(T, [[x, 0], [0, x ^ 2]]);;
gap> IsInverseSemigroup(R);
false
gap> IsInverseSemigroup(AsSemigroup(IsTransformationSemigroup, R));
false

# ReesMatTest102: IsInverseSemigroup (false because of matrix)
gap> S := Semigroup(SymmetricInverseMonoid(5));
<partial perm monoid of rank 5 with 4 generators>
gap> id := Identity(S);
<identity partial perm on [ 1, 2, 3, 4, 5 ]>
gap> zero := MultiplicativeZero(S);
<empty partial perm>

# Non-square matrix
gap> R := ReesZeroMatrixSemigroup(S, [[zero, id]]);
<Rees 0-matrix semigroup 2x1 over <partial perm monoid of size 1546, rank 5 
  with 4 generators>>
gap> IsInverseSemigroup(R);
false

# Non-diagonal matrix: Rows or columns without precisely one non-zero entry
gap> R := ReesZeroMatrixSemigroup(S, [[0, id, 0], [id, 0, 0], [0, 0, 0]]);;
gap> IsInverseSemigroup(R);
false
gap> R := ReesZeroMatrixSemigroup(S, [[0, 0, 0], [id, 0, 0], [0, id, 0]]);;
gap> IsInverseSemigroup(R);
false
gap> R := ReesZeroMatrixSemigroup(S, [[0, 0, id], [id, id, 0], [0, id, 0]]);;
gap> IsInverseSemigroup(R);
false
gap> R := ReesZeroMatrixSemigroup(S, [[0, id, 0], [0, id, 0], [0, id, 0]]);;
gap> IsInverseSemigroup(R);
false
gap> R := ReesZeroMatrixSemigroup(S, [[id, 0, 0], [id, id, 0], [0, id, 0]]);;
gap> IsInverseSemigroup(R);
false

# Matrix entries not in the group of units
gap> R := ReesZeroMatrixSemigroup(S, [[id, 0, 0], [0, 0, id], [0, zero, 0]]);;
gap> IsInverseSemigroup(R);
false
gap> y := PartialPerm([1, 2, 3, 4, 0]);
<identity partial perm on [ 1, 2, 3, 4 ]>
gap> R := ReesZeroMatrixSemigroup(S, [[id, 0, 0], [0, 0, id], [0, y, 0]]);;
gap> IsInverseSemigroup(R);
false

# Semigroup is not an inverse monoid
gap> T := FullTransformationMonoid(5);;
gap> R := ReesZeroMatrixSemigroup(T, [[Identity(T)]]);;
gap> IsInverseSemigroup(R);
false

# Example which returns true
gap> y := PartialPerm([4, 3, 5, 1, 2]);;
gap> R := ReesZeroMatrixSemigroup(S, [[id, 0, 0], [0, id, 0], [0, 0, y]]);;
gap> IsInverseSemigroup(R);
true

# ReesMatTest103: NrIdempotents and Idempotents for an inverse RZMS
gap> S := SymmetricInverseMonoid(4);
<symmetric inverse monoid of degree 4>
gap> x := PartialPerm([2, 1, 4, 3]);;
gap> y := PartialPerm([2, 4, 3, 1]);;
gap> R := ReesZeroMatrixSemigroup(S, [[0, x], [y, 0]]);
<Rees 0-matrix semigroup 2x2 over <symmetric inverse monoid of degree 4>>
gap> IsInverseSemigroup(R);
true
gap> NrIdempotents(R);
33
gap> NrIdempotents(R) = NrIdempotents(S) * Length(Rows(R)) + 1;
true
gap> idems := Idempotents(R);;
gap> IsDuplicateFreeList(idems);
true
gap> Length(idems) = NrIdempotents(R);
true
gap> ForAll(R, x -> x in R or not IsIdempotent(x));
true

# ReesMatTest104: NrIdempotents and Idempotents (for a RZMS over a group)
gap> R := ReesZeroMatrixSemigroup(Group(()), [[()]]);
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> NrIdempotents(R);
2
gap> Set(Idempotents(R));
[ 0, (1,(),1) ]
gap> Set(Idempotents(R)) = Elements(R);
true
gap> IsBand(R);
true

#
gap> x := Transformation([2, 1]);;
gap> T := Semigroup(x);
<commutative transformation semigroup of degree 2 with 1 generator>
gap> R := ReesZeroMatrixSemigroup(T, [[x, 0], [x, x ^ 2]]);
<Rees 0-matrix semigroup 2x2 over <transformation group of size 2, 
  degree 2 with 1 generator>>
gap> NrIdempotents(R);
4
gap> Set(Idempotents(R));
[ 0, (1,Transformation( [ 2, 1 ] ),1), (1,Transformation( [ 2, 1 ] ),2), 
  (2,IdentityTransformation,2) ]
gap> ForAll(Idempotents(R), x -> x * x = x);
true
gap> ForAll(R, x -> x in Idempotents(R) or not IsIdempotent(x));
true

#
gap> x := Transformation([1, 1, 2]);;
gap> T := Semigroup(x);
<commutative transformation semigroup of degree 3 with 1 generator>
gap> R := ReesZeroMatrixSemigroup(T, [[x, 0], [0, x ^ 2]]);
<Rees 0-matrix semigroup 2x2 over <commutative transformation semigroup 
  of size 2, degree 3 with 1 generator>>
gap> NrIdempotents(R);
3
gap> Idempotents(R);
[ (1,Transformation( [ 1, 1, 1 ] ),1), (2,Transformation( [ 1, 1, 1 ] ),2), 0 
 ]
gap> ForAll(Idempotents(R), x -> x * x = x);
true
gap> ForAll(R, x -> x in Idempotents(R) or not IsIdempotent(x));
true

# ReesMatTest105: IsInverseSemigroup and Idempotents using sub-RZMS
gap> S := SymmetricInverseMonoid(4);;
gap> x := PartialPerm([2, 1, 4, 3]);;
gap> y := PartialPerm([2, 4, 3, 1]);;
gap> z := PartialPerm([0, 0, 0, 0]);;
gap> R := ReesZeroMatrixSemigroup(S, [[x, x, 0], [y, 0, 0], [0, 0, x]]);
<Rees 0-matrix semigroup 3x3 over <symmetric inverse monoid of degree 4>>
gap> IsInverseSemigroup(R);
false

#
gap> T := Semigroup(RMSElement(R, 1, x, 1));
<subsemigroup of 3x3 Rees 0-matrix semigroup with 1 generator>
gap> IsInverseSemigroup(T);
true
gap> IsReesZeroMatrixSemigroup(T);
false
gap> NrIdempotents(T);
1
gap> Idempotents(T);
[ (1,(1,2)(3,4),1) ]
gap> T := Semigroup(RMSElement(R, 1, x, 1));
<subsemigroup of 3x3 Rees 0-matrix semigroup with 1 generator>
gap> IsReesZeroMatrixSemigroup(T);
false
gap> NrIdempotents(T);
1
gap> Idempotents(T);
[ (1,(1,2)(3,4),1) ]
gap> T := Semigroup(RMSElement(R, 1, y ^ -1, 2));
<subsemigroup of 3x3 Rees 0-matrix semigroup with 1 generator>
gap> IsInverseSemigroup(T);
true
gap> NrIdempotents(T);
1
gap> T := Semigroup(RMSElement(R, 1, y ^ -1, 2));;
gap> IsInverseSemigroup(T);
true
gap> Idempotents(T);
[ (1,(1,4,2)(3),2) ]
gap> T := Semigroup(RMSElement(R, 1, y ^ -1, 2));;
gap> NrIdempotents(T);
1
gap> T := Semigroup(RMSElement(R, 1, y ^ -1, 2));;
gap> Idempotents(T);
[ (1,(1,4,2)(3),2) ]
gap> T := Semigroup(RMSElement(R, 1, y ^ -1, 2));;
gap> SetIsInverseSemigroup(T, true);
gap> Idempotents(T);
[ (1,(1,4,2)(3),2) ]

#
gap> T := ReesZeroMatrixSubsemigroup(R, [2, 3], S, [1, 2, 3]);
<Rees 0-matrix semigroup 2x3 over <symmetric inverse monoid of degree 4>>
gap> IsInverseSemigroup(T);
false
gap> T := ReesZeroMatrixSubsemigroup(R, [2, 3], S, [1, 2]);
<Rees 0-matrix semigroup 2x2 over <symmetric inverse monoid of degree 4>>
gap> IsInverseSemigroup(T);
false
gap> T := ReesZeroMatrixSubsemigroup(R, [1, 2], S, [2, 3]);
<Rees 0-matrix semigroup 2x2 over <symmetric inverse monoid of degree 4>>
gap> IsInverseSemigroup(T);
false
gap> T := ReesZeroMatrixSubsemigroup(R, [2, 3], S, [1, 3]);
<Rees 0-matrix semigroup 2x2 over <symmetric inverse monoid of degree 4>>
gap> IsInverseSemigroup(T);
true
gap> NrIdempotents(T);
33
gap> idems := Idempotents(T);;
gap> ForAll(T, x -> x in idems or not IsIdempotent(x));
true
gap> R := ReesZeroMatrixSemigroup(S, [[z, x, 0], [0, 0, y]]);;
gap> IsInverseSemigroup(R);
false
gap> T := ReesZeroMatrixSubsemigroup(R, [2, 3], S, [1, 2]);;
gap> NrIdempotents(T);
33
gap> T := ReesZeroMatrixSubsemigroup(R, [2, 3], S, [1, 2]);;
gap> idems := Idempotents(T);;
gap> ForAll(T, x -> IsIdempotent(x) and x in idems or not IsIdempotent(x));
true
gap> G := GroupOfUnits(S);;
gap> T := ReesZeroMatrixSubsemigroup(R, [2, 3], G, [1, 2]);
<subsemigroup of 3x2 Rees 0-matrix semigroup with 96 generators>
gap> SetUnderlyingSemigroup(T, G);
gap> IsInverseSemigroup(T);
true
gap> NrIdempotents(T);
3
gap> Idempotents(T);
[ 0, (2,(1,2)(3,4),1), (3,(1,4,2)(3),2) ]
gap> T := ReesZeroMatrixSubsemigroup(R, [2, 3], G, [1, 2]);;
gap> SetUnderlyingSemigroup(T, G);
gap> SetIsInverseSemigroup(T, true);
gap> NrIdempotents(T);
3
gap> Idempotents(T);
[ 0, (2,(1,2)(3,4),1), (3,(1,4,2)(3),2) ]
gap> T := ReesZeroMatrixSubsemigroup(R, [2, 3], G, [1, 2]);;
gap> SetUnderlyingSemigroup(T, G);
gap> NrIdempotents(T);
3
gap> Set(Idempotents(T));
[ 0, (2,(1,2)(3,4),1), (3,(1,4,2)(3),2) ]

# ReesMatTest106: Test for Issue #128
gap> S := SymmetricInverseMonoid(5);;
gap> G := GroupOfUnits(S);;
gap> id := Identity(S);;
gap> R := ReesZeroMatrixSemigroup(S, [[id, 0], [0, id]]);;
gap> T := ReesZeroMatrixSubsemigroup(R, [1 .. 2], G, [1 .. 2]);
<subsemigroup of 2x2 Rees 0-matrix semigroup with 480 generators>
gap> IsReesZeroMatrixSemigroup(T);
true
gap> UnderlyingSemigroup(T);
<partial perm group of size 120, rank 5 with 73 generators>

# ReesMatTest107: RZMSNormalization, errors
gap> T := FullTransformationMonoid(4);
<full transformation monoid of degree 4>
gap> G := GroupOfUnits(T);
<transformation group of degree 4 with 2 generators>
gap> id := Identity(G);
IdentityTransformation
gap> R := ReesZeroMatrixSemigroup(T, [[id]]);
<Rees 0-matrix semigroup 1x1 over <full transformation monoid of degree 4>>
gap> RZMSNormalization(R);
<Rees 0-matrix semigroup 1x1 over <full transformation monoid of degree 4>> 
-> <Rees 0-matrix semigroup 1x1 over <full transformation monoid of degree 4>>
gap> R := ReesZeroMatrixSemigroup(G, [[id]]);
<Rees 0-matrix semigroup 1x1 over <transformation group of size 24, 
  degree 4 with 2 generators>>
gap> iso := RZMSNormalization(R);;
gap> Range(iso);
<Rees 0-matrix semigroup 1x1 over <transformation group of size 24, 
  degree 4 with 2 generators>>
gap> Matrix(Range(iso));
[ [ IdentityTransformation ] ]
gap> inv := InverseGeneralMapping(iso);;

# ReesMatTest108: RZMSNormalization, example 1
gap> G := SymmetricGroup(5);;
gap> R := ReesZeroMatrixSemigroup(G,
> [[0, (1, 4)(2, 5, 3), 0], [0, 0, (4, 2, 3)], [(1, 5)(2, 4, 3), 0, 0]]);
<Rees 0-matrix semigroup 3x3 over Sym( [ 1 .. 5 ] )>
gap> IsInverseSemigroup(R);
true
gap> iso := RZMSNormalization(R);
<Rees 0-matrix semigroup 3x3 over Sym( [ 1 .. 5 ] )> -> 
<Rees 0-matrix semigroup 3x3 over Sym( [ 1 .. 5 ] )>
gap> S := Range(iso);
<Rees 0-matrix semigroup 3x3 over Sym( [ 1 .. 5 ] )>
gap> Matrix(S);
[ [ (), 0, 0 ], [ 0, (), 0 ], [ 0, 0, () ] ]
gap> inv := InverseGeneralMapping(iso);;
gap> x := MultiplicativeZero(R) ^ iso;
0
gap> x ^ inv = MultiplicativeZero(R);
true
gap> x := RMSElement(R, 1, (), 1);
(1,(),1)
gap> x ^ iso;
(1,(1,5)(2,4,3),2)
gap> (x ^ iso) ^ inv = x;
true

# ReesMatTest109: RZMSNormalization, example 2
gap> G := SymmetricGroup(4);;
gap> mat := [
> [0, 0, (1, 3, 2), 0, (), 0, 0, (1, 2, 3)],
> [(), 0, 0, 0, 0, (1, 3, 4, 2), 0, (2, 4)],
> [0, 0, 0, (1, 2, 3), 0, 0, (1, 3, 2), 0],
> [0, 0, 0, 0, 0, 0, (1, 4, 2, 3), 0],
> [(), (1, 2, 3), (1, 2), 0, 0, 0, 0, 0],
> [0, (), 0, 0, 0, (1, 2), 0, 0]];;
gap> R := ReesZeroMatrixSemigroup(G, mat);
<Rees 0-matrix semigroup 8x6 over Sym( [ 1 .. 4 ] )>
gap> iso := RZMSNormalization(R);
<Rees 0-matrix semigroup 8x6 over Sym( [ 1 .. 4 ] )> -> 
<Rees 0-matrix semigroup 8x6 over Sym( [ 1 .. 4 ] )>
gap> S := Range(iso);
<Rees 0-matrix semigroup 8x6 over Sym( [ 1 .. 4 ] )>

# check that mat is in the 'normal' form
gap> mat := Matrix(S);
[ [ (), (), (), 0, 0, 0, 0, 0 ], [ (), 0, 0, (), (), 0, 0, 0 ], 
  [ 0, 0, (), (1,4,2), 0, (), 0, 0 ], [ 0, 0, 0, 0, (), (2,3,4), 0, 0 ], 
  [ 0, 0, 0, 0, 0, 0, (), () ], [ 0, 0, 0, 0, 0, 0, 0, () ] ]
gap> first_occurrence := l -> First([1 .. Length(l)], i -> l[i] <> 0);;
gap> x := Length(mat);;
gap> ForAll([1 .. x - 1],
> i -> first_occurrence(mat[i]) <= first_occurrence(mat[i + 1]));
true
gap> ForAll([1 .. Length(mat[1]) - 1], i ->
> first_occurrence(mat{[1 .. x]}[i]) <= first_occurrence(mat{[1 .. x]}[i + 1]));
true

# check that the connected components are grouped together
gap> comps := RZMSConnectedComponents(S);
[ [ [ 1, 2, 3, 4, 5, 6 ], [ 1, 2, 3, 4 ] ], [ [ 7, 8 ], [ 5, 6 ] ] ]
gap> Concatenation(List(comps, x -> x[1])) = Rows(R);
true
gap> Concatenation(List(comps, x -> x[2])) = Columns(R);
true

# MatrixEntries: Test for Issue #164
gap> mat := [
>  [Bipartition([[1, 2, 3, 4, -2, -3], [-1], [-4]]), 0, 0, 0],
>  [0, Bipartition([[1, 3, -1], [2, 4, -2, -3], [-4]]), 0,
>   Bipartition([[1, 4, -1], [2, 3], [-2], [-3, -4]])],
>  [0, 0, Bipartition([[1, 2, 3, -3], [4, -1, -4], [-2]]), 0]];;
gap> R := ReesZeroMatrixSemigroup(PartitionMonoid(4), mat);;
gap> MatrixEntries(R);
[ 0, <bipartition: [ 1, 2, 3, 4, -2, -3 ], [ -1 ], [ -4 ]>, 
  <bipartition: [ 1, 2, 3, -3 ], [ 4, -1, -4 ], [ -2 ]>, 
  <bipartition: [ 1, 3, -1 ], [ 2, 4, -2, -3 ], [ -4 ]>, 
  <bipartition: [ 1, 4, -1 ], [ 2, 3 ], [ -2 ], [ -3, -4 ]> ]
gap> mat := [
>  [Bipartition([[1, 2, 4], [3, -1, -2], [-3], [-4]]),
>   Bipartition([[1, -2, -4], [2, 3, 4, -3], [-1]])],
>  [Bipartition([[1, 2, 4, -1, -4], [3], [-2, -3]]),
>   Bipartition([[1, 3, -1], [2, 4, -2, -3], [-4]])],
>  [Bipartition([[1, 2, -2, -3], [3, 4, -1], [-4]]),
>   Bipartition([[1, -1, -2], [2, 3, -3, -4], [4]])]];;
gap> R := ReesZeroMatrixSemigroup(PartitionMonoid(4), mat);;
gap> MatrixEntries(R);
[ <bipartition: [ 1, 2, 4, -1, -4 ], [ 3 ], [ -2, -3 ]>, 
  <bipartition: [ 1, 2, 4 ], [ 3, -1, -2 ], [ -3 ], [ -4 ]>, 
  <bipartition: [ 1, 2, -2, -3 ], [ 3, 4, -1 ], [ -4 ]>, 
  <bipartition: [ 1, 3, -1 ], [ 2, 4, -2, -3 ], [ -4 ]>, 
  <bipartition: [ 1, -2, -4 ], [ 2, 3, 4, -3 ], [ -1 ]>, 
  <bipartition: [ 1, -1, -2 ], [ 2, 3, -3, -4 ], [ 4 ]> ]

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/semirms.tst");
