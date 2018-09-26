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
gap> S := RectangularBand(3, 4);;
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

#T# Creation of translations without having generators of the semigroup
gap> S := SymmetricGroup(4);;
gap> L := LeftTranslationsSemigroup(S);;
gap> l := LeftTranslation(L, IdentityTransformation);
<left translation on Sym( [ 1 .. 4 ] )>
gap> R := RightTranslationsSemigroup(S);;
gap> r := RightTranslation(R, IdentityTransformation);
<right translation on Sym( [ 1 .. 4 ] )>

#T# With calculation - rectangular bands
gap> S := RectangularBand(3, 3);
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
gap> Size(R);
27
gap> Size(H);
729
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
gap> SEMIGROUPS.Bitranslations(H);
<semigroups of translational hull elements over <simple transformation 
 semigroup of size 9, degree 7 with 3 generators>>

#T# small RZMS
gap> G := SmallGroup(4, 2);;
gap> H := AsList(G);;
gap> mat := [[H[1], 0],
> [H[2], H[2]]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> L := LeftTranslationsSemigroup(S);
<the semigroup of left translations of <regular semigroup with 4 generators>>
gap> Size(L);
81
gap> R := RightTranslationsSemigroup(S);
<the semigroup of right translations of <0-simple regular semigroup with 4 
 generators>>
gap> Size(R);
81
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> L := LeftTranslations(S);
<the semigroup of left translations of <0-simple regular semigroup with 4 
 generators>>
gap> R := RightTranslations(S);
<the semigroup of right translations of <0-simple regular semigroup with 4 
 generators>>
gap> H := TranslationalHull(S);
<translational hull over <0-simple regular semigroup with 4 generators>>
gap> Size(H);
21
gap> Size(L);
81
gap> Size(R);
81
gap> GeneratorsOfSemigroup(L);
[ <left translation on <0-simple regular semigroup with 4 generators>>, 
  <left translation on <0-simple regular semigroup with 4 generators>>, 
  <left translation on <0-simple regular semigroup with 4 generators>>, 
  <left translation on <0-simple regular semigroup with 4 generators>>, 
  <left translation on <0-simple regular semigroup with 4 generators>> ]
gap> GeneratorsOfSemigroup(R);
[ <right translation on <0-simple regular semigroup with 4 generators>>, 
  <right translation on <0-simple regular semigroup with 4 generators>>, 
  <right translation on <0-simple regular semigroup with 4 generators>>, 
  <right translation on <0-simple regular semigroup with 4 generators>>, 
  <right translation on <0-simple regular semigroup with 4 generators>> ]

#T# Test translations generation by digraph endomorphisms
gap> S := ZeroSemigroup(4);;
gap> L := SEMIGROUPS.LeftTranslationsSemigroupElementsByGenerators(
> LeftTranslations(S));
<semigroup of left translations of <commutative non-regular transformation 
 semigroup of size 4, degree 4 with 3 generators> with 17 generators>
gap> Size(L);
64
gap> R := SEMIGROUPS.RightTranslationsSemigroupElementsByGenerators(
> RightTranslations(S));
<semigroup of right translations of <commutative non-regular transformation 
 semigroup of size 4, degree 4 with 3 generators> with 17 generators>

#T# Further test translations generation
gap> S := Semigroup([Transformation([2, 4, 4, 1]), Transformation([2, 3, 2, 1]), 
> Transformation([3, 3, 3])]);;
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

#T# Test translations backtracking
gap> for i in [1 .. 4] do
> L := LeftZeroSemigroup(i);
> R := RightZeroSemigroup(i);
> Ll := LeftTranslationsSemigroup(L);;
> Lr := LeftTranslationsSemigroup(R);;
> Rl := RightTranslationsSemigroup(L);;
> Rr := RightTranslationsSemigroup(R);;
> if not (Size(Ll) = i ^ i and Size(Rr) = i ^ i
> and Size(Lr) = 1 and Size(Rl) = 1) then
> Error();
> fi;
> od;

#T# Translations and translational hulls of monoids that couldn't be calculated
gap> S := BrauerMonoid(5);;
gap> L := LeftTranslations(S);
<the semigroup of left translations of <regular bipartition *-monoid of 
 degree 5 with 3 generators>>
gap> Size(L);
945
gap> R := RightTranslations(S);
<the semigroup of right translations of <regular bipartition *-monoid 
 of size 945, degree 5 with 3 generators>>
gap> Size(R);
945
gap> H := TranslationalHull(S);
<translational hull over <regular bipartition *-monoid of size 945, degree 5 
 with 3 generators>>
gap> Size(H);
945
gap> S := FullTransformationMonoid(5);;
gap> L := LeftTranslations(S);
<the semigroup of left translations of <full transformation monoid of degree 5\
>>
gap> Size(L);
3125
gap> R := RightTranslations(S);
<the semigroup of right translations of <full transformation monoid of degree \
5>>
gap> Size(R);
3125
gap> H := TranslationalHull(S);
<translational hull over <full transformation monoid of degree 5>>
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
>       # Size(rclasses) = 1
>       for j in [1 .. Size(lclasses)] do
>         r := Representative(Intersection(lclasses[1], rclasses[1]));
>         Add(reps, r);
>         Remove(lclasses, 1);
>       od;
>     else
>       # Size(lclasses) = 1
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
>         Add(linkedpairs, Bitranslation(H, l, r));
>       fi;
>     od;
>   od;
>   return linkedpairs;
> end;;
gap> G := SmallGroup(4, 1);;
gap> H := ShallowCopy(AsList(G));;
gap> mat := [[0, H[4]], [H[4], 0]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> Size(TranslationalHull(S)) = Size(SEMIGROUPS.bruteforcetranshull(S));
true
gap> Size(Semigroup(GeneratorsOfSemigroup(TranslationalHull(S)))) = 
> Size(TranslationalHull(S));
true
gap> S := RectangularBand(2, 3);;
gap> Size(Semigroup(GeneratorsOfSemigroup(TranslationalHull(S))))
> = Size(SEMIGROUPS.bruteforcetranshull(S));
true

#T# Test translational hull method for arbitrary semigroups
gap> S := ZeroSemigroup(4);;
gap> Size(TranslationalHull(S));
4096
gap> S := Semigroup([Transformation([1, 1, 2]), Transformation([3, 1, 3])]);;
gap> H := TranslationalHull(S);;
gap> H = Semigroup(H);
true
gap> Size(H);
13
gap> I := InnerTranslationalHull(S);;
gap> IsWholeFamily(I);
false

#T# Test inner translations
gap> G := SmallGroup(6, 1);;
gap> a := G.1;; b := G.2;;
gap> mat := [[a, 0],
> [b, a]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> Size(InnerLeftTranslations(S)) = Size(S);
true
gap> Size(InnerRightTranslations(S)) = Size(S);
true
gap> Size(InnerTranslationalHull(S)) = Size(S);
true

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
gap> OneOp(Representative(H)) = Bitranslation(
> H, OneOp(L.1), OneOp(R.1));
true

#T# Make sure the generic method and special methods agree for hulls
gap> Semigroup(SEMIGROUPS.BitranslationsByGenerators(H)) = H;
true

#T# TranslationalHull for semigroups which are not IsEnumerableSemigroupRep
gap> S := Semigroup([Transformation([1, 4, 3, 3]), Transformation([3, 4, 1, 1])]);;
gap> S := AsSemigroup(IsFpSemigroup, S);;
gap> IsEnumerableSemigroupRep(S);
false
gap> H := TranslationalHull(S);;
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `TranslationalHull' on 1 arguments
gap> AsList(S);;
gap> for h in TranslationalHull(S) do
> l := h![1];
> r := h![2];
> for s in GeneratorsOfSemigroup(S) do
> for t in GeneratorsOfSemigroup(S) do
> if not s * (t ^ l) = (s ^ r) * t then
> Print(s, t, h);
> fi;
> od;
> od;
> od;

#T# Methods for non-normalised RMS
gap> G := SmallGroup(8, 2);;
gap> mat := [[G.1, G.2], [G.2, G.2 * G.2]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> L := LeftTranslationsSemigroup(S);;
gap> R := RightTranslationsSemigroup(S);;
gap> Size(L) = Size(Semigroup(GeneratorsOfSemigroup(L)));
true
gap> Size(R) = Size(Semigroup(GeneratorsOfSemigroup(R)));
true

#T# Special methods for normalised RMS 
gap> G := SmallGroup(12, 1);;
gap> mat := [[G.1, G.2], [G.1, G.1], [G.2, G.3], [G.1 * G.2, G.1 * G.3]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> T := Range(RMSNormalization(S));;
gap> LS := LeftTranslationsSemigroup(S);;
gap> RS := RightTranslationsSemigroup(S);;
gap> HS := TranslationalHullSemigroup(S);;
gap> L := LeftTranslations(T);;
gap> R := RightTranslations(T);;
gap> H := TranslationalHull(T);;
gap> Size(LS) = Size(L);
true
gap> Size(RS) = Size(R);
true
gap> Size(H);
444
gap> GeneratorsOfSemigroup(L);
[ <left translation on <simple semigroup of size 96, with 6 generators>>, 
  <left translation on <simple semigroup of size 96, with 6 generators>>, 
  <left translation on <simple semigroup of size 96, with 6 generators>>, 
  <left translation on <simple semigroup of size 96, with 6 generators>>, 
  <left translation on <simple semigroup of size 96, with 6 generators>> ]
gap> GeneratorsOfSemigroup(R);
[ <right translation on <simple semigroup of size 96, with 6 generators>>, 
  <right translation on <simple semigroup of size 96, with 6 generators>>, 
  <right translation on <simple semigroup of size 96, with 6 generators>>, 
  <right translation on <simple semigroup of size 96, with 6 generators>>, 
  <right translation on <simple semigroup of size 96, with 6 generators>>, 
  <right translation on <simple semigroup of size 96, with 6 generators>> ]
gap> Size(L) = Size(Semigroup(GeneratorsOfSemigroup(L)));
true
gap> Representative(H);
<linked pair of translations on <simple semigroup of size 96, with 6 
 generators>>
gap> mat := TransposedMat([[G.1, G.2], [G.1, G.1], [G.2, G.3],
> [G.1 * G.2, G.1 * G.3]]);;
gap> T := Range(RMSNormalization(ReesMatrixSemigroup(G, mat)));;
gap> R := RightTranslations(T);;
gap> Size(R) = Size(Semigroup(GeneratorsOfSemigroup(R)));
true
gap> Size(TranslationalHull(T));
100
gap> G := SmallGroup(16, 2);;
gap> mat := [[G.1, G.2, G.2 * G.1], [G.2 * G.2, G.2, G.2 * G.2],
> [G.2 * G.1 * G.2, G.3, G.1 * G.3]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> T := Range(RMSNormalization(S));;
gap> Size(TranslationalHull(T));
160
gap> for h in TranslationalHull(T) do
> l := h![1];
> r := h![2];
> for s in GeneratorsOfSemigroup(T) do
> for t in GeneratorsOfSemigroup(T) do
> if not s * (t ^ l) = (s ^ r) * t then
> Print(s, t, h);
> fi;
> od;
> od;
> od;
gap> G := SmallGroup(6, 1);;
gap> mat := [[G.1, G.2], [G.1 * G.2, G.1], [G.2, G.2]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> T := Range(RMSNormalization(S));;
gap> Size(TranslationalHull(T));
40
gap> Size(Semigroup(TranslationalHull(T)));
40

#T# IsWholeFamily for translations semigroups
gap> S := Semigroup([Transformation([1, 1, 2, 4]), Transformation([3, 1, 3])]);;
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
gap> Ll := ShallowCopy(AsList(L));;
gap> Rl := ShallowCopy(AsList(R));;
gap> Sort(Ll);
gap> Sort(Rl);
gap> l := Ll[1];;
gap> r := Rl[1];;
gap> IsWholeFamily(Semigroup(l));
false
gap> IsWholeFamily(Semigroup(r));
false

#T# Error Testing - Left Translations
gap> S := Semigroup([Transformation([1, 4, 3, 3]),
> Transformation([3, 4, 1, 1])]);;
gap> T := Semigroup([Transformation([5, 2, 3, 2, 1]),
> Transformation([2, 3, 1, 1, 2])]);;
gap> L := LeftTranslationsSemigroup(S);;
gap> R := RightTranslationsSemigroup(S);;
gap> f := MappingByFunction(S, S, x -> S.1);;
gap> g := MappingByFunction(S, T, x -> T.1);;
gap> LeftTranslation(L, f);
Error, Semigroups: LeftTranslation: 
the mapping given must define a left translation,
gap> LeftTranslation(R, f);
Error, Semigroups: LeftTranslation: 
the first argument must be a semigroup of left translations,
gap> LeftTranslation(L, (1, 4)(2, 3));
Error, Semigroups: LeftTranslation: 
the first argument should be a left translations semigroup, and the second arg\
ument should be a mapping on the underlying semigroup of the first argument, o\
r a transformation on the indices of its elements,
gap> LeftTranslation(L, g);
Error, Semigroups: LeftTranslation (from Mapping): 
the domain and range of the second argument must be the underlying semigroup o\
f the first,
gap> x := [2 .. Size(S) + 1];;
gap> Add(x, 1);;
gap> LeftTranslation(L, Transformation(x));
Error, Semigroups: LeftTranslation (from transformation): 
the second argument must act on the indices of the underlying semigroup of the\
 first argument,
gap> S := RectangularBand(2, 3);;
gap> L := LeftTranslationsSemigroup(S);;
gap> l := LeftTranslation(L, Transformation([1, 1, 1, 1, 1, 1]));
Error, Semigroups: LeftTranslation: 
the transformation given must define a left translation,

#T# Error Testing - Right Translations
gap> S := Semigroup([Transformation([1, 4, 3, 3]),
> Transformation([3, 4, 1, 1])]);;
gap> T := Semigroup([Transformation([5, 2, 3, 2, 1]),
> Transformation([2, 3, 1, 1, 2])]);;
gap> L := LeftTranslationsSemigroup(S);;
gap> R := RightTranslationsSemigroup(S);;
gap> f := MappingByFunction(S, S, x -> S.1);;
gap> g := MappingByFunction(S, T, x -> T.1);;
gap> RightTranslation(R, f);
Error, Semigroups: RightTranslation: 
the mapping given must define a right translation,
gap> RightTranslation(L, f);
Error, Semigroups: RightTranslation: 
the first argument must be a semigroup of right translations,
gap> RightTranslation(R, (1, 4)(2, 3));
Error, Semigroups: RightTranslation: 
the first argument should be a right translations semigroup, and the second ar\
gument should be a mapping on the underlying semigroup of the first argument, \
or a transformation on the indices of its elements,
gap> RightTranslation(R, g);
Error, Semigroups: RightTranslation (from Mapping): 
the domain and range of the second argument must be the underlying semigroup o\
f the first,
gap> x := [2 .. Size(S) + 1];;
gap> Add(x, 1);;
gap> RightTranslation(R, Transformation(x));
Error, Semigroups: RightTranslation (from transformation): 
the second argument must act on the indices of the underlying semigroup of the\
 first argument,
gap> S := RectangularBand(2, 3);;
gap> R := RightTranslationsSemigroup(S);;
gap> r := RightTranslation(R, Transformation([1, 1, 1, 1, 1, 1]));
Error, Semigroups: RightTranslation: 
the transformation given must define a right translation,

#T# Error Testing - Left Translations Without Generators
gap> S := SymmetricGroup(4);;
gap> L := LeftTranslationsSemigroup(S);;
gap> LeftTranslation(L, Transformation([1, 1, 1, 1, 1, 1]));
Error, Semigroups: LeftTranslation: 
the transformation given must define a left translation,

#T# Error Testing - Right Translations Without Generators
gap> S := SymmetricGroup(4);;
gap> R := RightTranslationsSemigroup(S);;
gap> RightTranslation(R, Transformation([1, 1, 1, 1, 1, 1]));
Error, Semigroups: RightTranslation: 
the transformation given must define a right translation,

#T# Error Testing - Translational Hull Elements
gap> S := Semigroup([Transformation([1, 4, 3, 3]), Transformation([3, 4, 1, 1])]);;
gap> T := RectangularBand(3, 4);;
gap> L := LeftTranslationsSemigroup(S);;
gap> R := RightTranslationsSemigroup(S);;
gap> RT := RightTranslationsSemigroup(T);;
gap> H := TranslationalHull(S);;
gap> l := Representative(L);;
gap> r := Representative(R);;
gap> Bitranslation(L, l, r);
Error, Semigroups: Bitranslation: 
the first argument must be a translational hull,
gap> Bitranslation(H, r, l);
Error, Semigroups: Bitranslation: 
the second argument must be a left translation and the third argument must be \
a right translation,
gap> l := LeftTranslation(L, MappingByFunction(S, S, x -> S.1 * x));;
gap> r := RightTranslation(R, MappingByFunction(S, S, x -> x * S.2));;
gap> Bitranslation(H, l, r);
Error, Semigroups: Bitranslation: 
the translations given must form a linked pair,
gap> r := Representative(RT);;
gap> Bitranslation(H, l, r);
Error, Semigroups: Bitranslation: 
each argument must have the same underlying semigroup,

#T# Error Testing - Translational Hull Elements Without Generators
gap> S := SymmetricGroup(3);;
gap> L := LeftTranslationsSemigroup(S);;
gap> R := RightTranslationsSemigroup(S);;
gap> H := TranslationalHullSemigroup(S);;
gap> l := LeftTranslationNC(L, IdentityTransformation);;
gap> r := RightTranslationNC(R, Transformation([2, 1, 4, 3, 6, 5]));;
gap> Bitranslation(H, l, r);
Error, Semigroups: Bitranslation: 
the translations given must form a linked pair,

#T# Error Testing - left translations over normalised RMS
gap> G := SmallGroup(12, 1);;
gap> mat := [[G.1, G.2], [G.1, G.1], [G.2, G.3], [G.1 * G.2, G.1 * G.3]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> T := Range(RMSNormalization(S));;
gap> L := LeftTranslationsSemigroup(T);;
gap> LS := LeftTranslationsSemigroup(S);;
gap> lgpfunc := [G.1 * G.3 * G.3, G.2];;
gap> LeftTranslationOfNormalRMS(LS, lgpfunc, IdentityTransformation);
Error, Semigroups: LeftTranslationOfNormalRMS: 
the first argument must be a semigroup of left translations over a normalised \
Rees matrix semigroup over a group,
gap> LeftTranslationOfNormalRMS(L, [G.1, G.1, G.2], IdentityTransformation);
Error, Semigroups: LeftTranslationOfNormalRMS: 
the second argument must be a list of group elements of length equal to the nu\
mber of rows of the underlying semigroup of the first argument,
gap> LeftTranslationOfNormalRMS(L, lgpfunc, Transformation([1, 3, 2]));
Error, Semigroups: LeftTranslationOfNormalRMS: 
the third argument must be a transformation on the number of rows of the under\
lying semigroup of the first argument,

#T# Error Testing - right translations over normalised RMS
gap> G := SmallGroup(12, 1);;
gap> mat := [[G.1, G.2], [G.1, G.1], [G.2, G.3], [G.1 * G.2, G.1 * G.3]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> T := Range(RMSNormalization(S));;
gap> R := RightTranslationsSemigroup(T);;
gap> RS := RightTranslationsSemigroup(S);;
gap> rgpfunc := [G.1 * G.3 * G.3, G.3 * G.3, G.2, G.1];;
gap> RightTranslationOfNormalRMS(RS, rgpfunc, IdentityTransformation);
Error, Semigroups: RightTranslationOfNormalRMS: 
the first argument must be a semigroup of right translations of a normalised R\
ees matrix semigroup over a group,
gap> RightTranslationOfNormalRMS(R, [G.1, G.2], IdentityTransformation);
Error, Semigroups: RightTranslationOfNormalRMS: 
the second argument must be a list of group elements of length equal to the nu\
mber of columns of the underlying semigroup of the first argument,
gap> RightTranslationOfNormalRMS(R, rgpfunc, Transformation([1, 3, 2, 5, 4]));
Error, Semigroups: RightTranslationOfNormalRMS: 
the third argument must be a transformation on the number of columns of the un\
derlying semigroup of the first argument,

#T# Error testing - bitranslations over normalised RMS
gap> G := SmallGroup(12, 1);;
gap> mat := [[G.1, G.2], [G.1, G.1], [G.2, G.3]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> T := Range(RMSNormalization(S));;
gap> G := UnderlyingSemigroup(T);
<pc group of size 12 with 3 generators>
gap> L := LeftTranslations(T);;
gap> R := RightTranslations(T);;
gap> H := TranslationalHull(T);;
gap> HS := TranslationalHullSemigroup(S);;
gap> lgpfunc := [G.1 * G.3 * G.3, G.2];;
gap> rgpfunc := [G.1 * G.3 * G.3, G.3 * G.3, G.2];;
gap> lt := Transformation([2, 2]);;
gap> rt := Transformation([3, 3, 3]);;
gap> l := LeftTranslationOfNormalRMS(L, lgpfunc, lt);;
gap> r := RightTranslationOfNormalRMS(R, rgpfunc, rt);;
gap> h := BitranslationOfNormalRMS(H, l, r);
<linked pair of translations on <simple semigroup of size 72, with 5 
 generators>>
gap> h := BitranslationOfNormalRMS(HS, l, r);
Error, Semigroups: BitranslationOfNormalRMS: 
the first argument must be a semigroup of bitranslations over a normalised RMS\
,
gap> l := LeftTranslationOfNormalRMS(L, lgpfunc, IdentityTransformation);;
gap> h := BitranslationOfNormalRMS(H, l, r);
Error, Semigroups: BitranslationOfNormalRMS: 
the second and third arguments must be a linked left and right translation, re\
spectively,

#T# Hashing translations
gap> S := Semigroup([Transformation([1, 4, 3, 3]), Transformation([3, 4, 1, 1])]);; 
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
gap> l := Representative(L);;
gap> r := Representative(R);;
gap> Lht := HTCreate(l);;
gap> Rht := HTCreate(r);;
gap> for l in L do
> HTAdd(Lht, l, true);
> od;
gap> for r in R do
> HTAdd(Rht, r, true);
> od;

#T# Hashing translational hull elements
gap> S := Semigroup([Transformation([1, 4, 3, 3]), Transformation([3, 4, 1, 1])]);; 
gap> H := TranslationalHull(S);;
gap> ht := HTCreate(Representative(H));;
gap> for h in H do
> HTAdd(ht, h, true);
> od;

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(a);
gap> Unbind(b);
gap> Unbind(f);
gap> Unbind(G);
gap> Unbind(g);
gap> Unbind(L);
gap> Unbind(Ll);
gap> Unbind(Lr);
gap> Unbind(LS);
gap> Unbind(l);
gap> Unbind(lgpfunc);
gap> Unbind(lt);
gap> Unbind(H);
gap> Unbind(HS);
gap> Unbind(I);
gap> Unbind(h);
gap> Unbind(mat);
gap> Unbind(R);
gap> Unbind(RS);
gap> Unbind(RT);
gap> Unbind(Rl);
gap> Unbind(r);
gap> Unbind(rgpfunc);
gap> Unbind(rt);
gap> Unbind(S);
gap> Unbind(T);
gap> Unbind(SEMIGROUPS.bruteforcetranshull);
gap> Unbind(x);

#E#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/translat.tst");
