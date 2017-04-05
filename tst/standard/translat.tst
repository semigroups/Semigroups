#############################################################################
##
#W  standard/translat.tst
#Y  Copyright (C) 2016-17                                          Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/translat.tst");
gap> LoadPackage("semigroups", false);;
gap> SEMIGROUPS.StartTest();

#T# Creation of translations semigroups
gap> S := RectangularBand(3,4);;
gap> L := LeftTranslationsSemigroup(S);
<the semigroup of left translations of <regular transformation semigroup 
 of size 12, degree 8 with 4 generators>>
gap> R := RightTranslationsSemigroup(S);
<the semigroup of right translations of <regular transformation semigroup 
 of size 12, degree 8 with 4 generators>>
gap> TranslationalHullSemigroup(S);
<translational hull over <regular transformation semigroup of size 12, 
 degree 8 with 4 generators>>
gap> Size(L);
27
gap> Size(R);
256

#T# Creation of translations semigroups it can't calclate
gap> S := SingularTransformationSemigroup(10);;
gap> LeftTranslationsSemigroup(S);
<the semigroup of left translations of <regular transformation semigroup 
 ideal of degree 10 with 1 generator>>
gap> RightTranslationsSemigroup(S);
<the semigroup of right translations of <regular transformation semigroup 
 ideal of degree 10 with 1 generator>>
gap> TranslationalHullSemigroup(S);
<translational hull over <regular transformation semigroup ideal of 
 degree 10 with 1 generator>>

#T# with calculation - rectangular bands
gap> S := RectangularBand(3,3);
<regular transformation semigroup of size 9, degree 7 with 3 generators>
gap> L := LeftTranslations(S);
<the semigroup of left translations of <simple transformation semigroup 
 of size 9, degree 7 with 3 generators>>
gap> R := RightTranslations(S);
<the semigroup of right translations of <simple transformation semigroup 
 of size 9, degree 7 with 3 generators>>
gap> H := TranslationalHull(S);
<translational hull over <simple transformation semigroup of size 9, degree 7 
 with 3 generators>>
gap> Size(L);
27
gap> GeneratorsOfSemigroup(L);
[ <left translation on <simple transformation semigroup of size 9, degree 7 
     with 3 generators>>, 
  <left translation on <simple transformation semigroup of size 9, degree 7 
     with 3 generators>>, 
  <left translation on <simple transformation semigroup of size 9, degree 7 
     with 3 generators>> ]
gap> GeneratorsOfSemigroup(R);
[ <right translation on <simple transformation semigroup of size 9, degree 7 
     with 3 generators>>, 
  <right translation on <simple transformation semigroup of size 9, degree 7 
     with 3 generators>>, 
  <right translation on <simple transformation semigroup of size 9, degree 7 
     with 3 generators>> ]
gap> GeneratorsOfSemigroup(H);
[ <linked pair of translations on <simple transformation semigroup of size 9, 
     degree 7 with 3 generators>>, <linked pair of translations on 
    <simple transformation semigroup of size 9, degree 7 with 3 generators>>, 
  <linked pair of translations on <simple transformation semigroup of size 9, 
     degree 7 with 3 generators>>, <linked pair of translations on 
    <simple transformation semigroup of size 9, degree 7 with 3 generators>>, 
  <linked pair of translations on <simple transformation semigroup of size 9, 
     degree 7 with 3 generators>>, <linked pair of translations on 
    <simple transformation semigroup of size 9, degree 7 with 3 generators>>, 
  <linked pair of translations on <simple transformation semigroup of size 9, 
     degree 7 with 3 generators>>, <linked pair of translations on 
    <simple transformation semigroup of size 9, degree 7 with 3 generators>>, 
  <linked pair of translations on <simple transformation semigroup of size 9, 
     degree 7 with 3 generators>> ]

#T# small RZMS
gap> G := SmallGroup(4,2);;
gap> H := AsList(G);;
gap> mat := [ [H[1], 0],
> [H[2], H[2]] ];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> L := LeftTranslations(S);
<the semigroup of left translations of <0-simple regular semigroup 
 of size 17, with 4 generators>>
gap> R := RightTranslations(S);
<the semigroup of right translations of <0-simple regular semigroup 
 of size 17, with 4 generators>>
gap> H := TranslationalHull(S);
<translational hull over <0-simple regular semigroup of size 17, with 4 
 generators>>
gap> Size(H);
21
gap> Size(L);
81
gap> Size(R);
81
gap> GeneratorsOfSemigroup(L);
[ <left translation on <0-simple regular semigroup of size 17, with 4 
     generators>>, <left translation on <0-simple regular semigroup 
     of size 17, with 4 generators>>, 
  <left translation on <0-simple regular semigroup of size 17, with 4 
     generators>>, <left translation on <0-simple regular semigroup 
     of size 17, with 4 generators>>, 
  <left translation on <0-simple regular semigroup of size 17, with 4 
     generators>> ]
gap> GeneratorsOfSemigroup(R);
[ <right translation on <0-simple regular semigroup of size 17, with 4 
     generators>>, <right translation on <0-simple regular semigroup 
     of size 17, with 4 generators>>, 
  <right translation on <0-simple regular semigroup of size 17, with 4 
     generators>>, <right translation on <0-simple regular semigroup 
     of size 17, with 4 generators>>, 
  <right translation on <0-simple regular semigroup of size 17, with 4 
     generators>> ]

#T# Test translations generation by digraph endomorphisms
gap> S := ZeroSemigroup(4);;
gap> L := SEMIGROUPS.LeftTranslationsSemigroupElementsByGenerators(
> LeftTranslations(S));
<semigroup of left translations of <commutative non-regular transformation 
 semigroup of size 4, degree 4 with 3 generators> with 17 generatorss>
gap> Size(L);
64
gap> R := SEMIGROUPS.RightTranslationsSemigroupElementsByGenerators(
> RightTranslations(S));
<semigroup of right translations of <commutative non-regular transformation 
 semigroup of size 4, degree 4 with 3 generators> with 17 generatorss>

#T# Further test translations generation by digraph endomorphisms
gap> S := Semigroup([Transformation([2,4,4,1]), Transformation([2,3,2,1]), 
> Transformation([3,3,3])]);;
gap> L := LeftTranslations(S);
<the semigroup of left translations of <transformation semigroup of size 49, 
 degree 4 with 3 generators>>
gap> Size(L);
123
gap> R := RightTranslations(S);
<the semigroup of right translations of <transformation semigroup of size 49, 
 degree 4 with 3 generators>>
gap> Size(R);
55

#T# Translations and translational hulls of monoids that couldn't be calculated
gap> S := BrauerMonoid(5);;
gap> L := LeftTranslations(S);
Monoid( 
[ <left translation on <regular bipartition *-monoid of size 945, degree 5 
     with 3 generators>>, <left translation on <regular bipartition *-monoid 
     of size 945, degree 5 with 3 generators>>, 
  <left translation on <regular bipartition *-monoid of size 945, degree 5 
     with 3 generators>>, <left translation on <regular bipartition *-monoid 
     of size 945, degree 5 with 3 generators>> ] )
gap> Size(L);
945
gap> R := RightTranslations(S);
Monoid( 
[ <right translation on <regular bipartition *-monoid of size 945, degree 5 
     with 3 generators>>, <right translation on <regular bipartition *-monoid 
     of size 945, degree 5 with 3 generators>>, 
  <right translation on <regular bipartition *-monoid of size 945, degree 5 
     with 3 generators>>, <right translation on <regular bipartition *-monoid 
     of size 945, degree 5 with 3 generators>> ] )
gap> Size(R);
945
gap> H := TranslationalHull(S);
Monoid( 
[ <linked pair of translations on <regular bipartition *-monoid of size 945, 
     degree 5 with 3 generators>>, <linked pair of translations on 
    <regular bipartition *-monoid of size 945, degree 5 with 3 generators>>, 
  <linked pair of translations on <regular bipartition *-monoid of size 945, 
     degree 5 with 3 generators>>, <linked pair of translations on 
    <regular bipartition *-monoid of size 945, degree 5 with 3 generators>> 
 ] )
gap> Size(H);
945
gap> S := FullTransformationMonoid(5);;
gap> L := LeftTranslations(S);
Monoid( [ <left translation on <full transformation monoid of degree 5>>, 
  <left translation on <full transformation monoid of degree 5>>, 
  <left translation on <full transformation monoid of degree 5>>, 
  <left translation on <full transformation monoid of degree 5>> ] )
gap> Size(L);
3125
gap> R := RightTranslations(S);
Monoid( [ <right translation on <full transformation monoid of degree 5>>, 
  <right translation on <full transformation monoid of degree 5>>, 
  <right translation on <full transformation monoid of degree 5>>, 
  <right translation on <full transformation monoid of degree 5>> ] )
gap> Size(R);
3125
gap> H := TranslationalHull(S);
Monoid( 
[ <linked pair of translations on <full transformation monoid of degree 5>>, 
  <linked pair of translations on <full transformation monoid of degree 5>>, 
  <linked pair of translations on <full transformation monoid of degree 5>>, 
  <linked pair of translations on <full transformation monoid of degree 5>> 
 ] )
gap> Size(H);
3125

#T# A tiny bit of brute force checking   
gap> SEMIGROUPS.bruteforcetranshull := function(S)
>   local a, d, L, R, H, linkedpairs, dclasses, rclasses, lclasses, reps, i, j, 
>         l, r, flag;
>   L := LeftTranslations(S);
>   R := RightTranslations(S);
>   H := TranslationalHullSemigroup(S);
>   linkedpairs := [];
>   dclasses := DClasses(S);
>   reps := [];
>   
>   for d in dclasses do
>     lclasses := ShallowCopy(LClasses(d));
>     rclasses := ShallowCopy(RClasses(d));
>     for i in [1 .. Minimum(Size(lclasses), Size(rclasses)) - 1] do
>       r := Representative(Intersection(lclasses[1], rclasses[1]));
>       Add(reps, r);
>       Remove(lclasses, 1);
>       Remove(rclasses, 1);
>     od;
>     if Size(lclasses) > Size(rclasses) then
>       #Size(rclasses) = 1
>       for j in [1 .. Size(lclasses)] do
>         r := Representative(Intersection(lclasses[1], rclasses[1]));
>         Add(reps, r);
>         Remove(lclasses, 1);
>       od;
>     else
>       #Size(lclasses) = 1
>       for j in [1 .. Size(rclasses)] do
>         r := Representative(Intersection(lclasses[1], rclasses[1]));
>         Add(reps, r);
>         Remove(rclasses, 1);
>       od;
>     fi;
>   od;
>   
>   for l in L do
>     for r in R do
>       flag := true;
>         for a in Cartesian(reps, reps) do
>           if not a[1] * (a[2] ^ l) = (a[1] ^ r) * a[2] then
>             flag := false;
>             break;
>           fi;
>         od;
>       if flag then 
>         Add(linkedpairs, TranslationalHullElement(H, l, r));
>       fi;
>     od;
>   od;
>   return linkedpairs;
> end;;
gap> G := SmallGroup(4,1);;
gap> H := ShallowCopy(AsList(G));;
gap> mat := [[0, H[4]], [H[4], 0]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> Size(TranslationalHull(S)) = Size(SEMIGROUPS.bruteforcetranshull(S));
true
gap> S := RectangularBand(2,3);;
gap> Size(Semigroup(GeneratorsOfSemigroup(TranslationalHull(S))))
> = Size(SEMIGROUPS.bruteforcetranshull(S));
true

#T# Test translational hull method for arbitrary semigroups
gap> S := ZeroSemigroup(4);;
gap> Size(TranslationalHull(S));
4096
gap> S := Semigroup([Transformation([1,1,2]), Transformation([3,1,3])]);;
gap> H := TranslationalHull(S);
<translational hull over <regular transformation semigroup of size 11, 
 degree 3 with 2 generators>>
gap> H = Semigroup(H);
true
gap> Size(H);
13

#T# OneOp for translations semigroups elements and translational hull elements
gap> G := SmallGroup(6, 1);;
gap> a := G.1;; b := G.2;;
gap> mat := [[a, 0],
> [b, a]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> IsSemigroup(S) and IsFinite(S) and IsZeroSimpleSemigroup(S);;
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
gap> H := TranslationalHull(S);;
gap> OneOp(L.1) = LeftTranslation(L, MappingByFunction(S, S, x -> x));
true
gap> OneOp(R.1) = RightTranslation(R, MappingByFunction(S, S, x -> x));
true
gap> OneOp(Representative(H)) = TranslationalHullElement(
> H, OneOp(L.1), OneOp(R.1));
true

#T# Make sure the generic method and special methods agree for hulls
gap> Semigroup(SEMIGROUPS.TranslationalHullElementsByGenerators(H)) = H;
true

#T# TranslationalHull for semigroups which are not IsEnumerable
gap> S := Semigroup([Transformation([1,4,3,3]), Transformation([3,4,1,1])]);;
gap> S := AsSemigroup(IsFpSemigroup, S);;
gap> IsEnumerableSemigroupRep(S);
false
gap> H := TranslationalHull(S);;
gap> for h in TranslationalHull(S) do
> l := h![1];
> r := h![2];
> for s in S do
> for t in S do
> if not s * (t^l) = (s ^ r) * t then
> Print(s, t, h);
> fi;
> od;
> od;
> od;

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(a);
gap> Unbind(b);
gap> Unbind(G);
gap> Unbind(L);
gap> Unbind(H);
gap> Unbind(mat);
gap> Unbind(R);
gap> Unbind(S);
gap> Unbind(SEMIGROUPS.bruteforcetranshull);

#E#
gap> STOP_TEST("Semigroups package: standard/translat.tst");
