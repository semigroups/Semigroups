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
gap> L := LeftTranslations(S);
<the semigroup of left translations of <regular transformation semigroup 
 of size 12, degree 8 with 4 generators>>
gap> R := RightTranslations(S);
<the semigroup of right translations of <regular transformation semigroup 
 of size 12, degree 8 with 4 generators>>
gap> TranslationalHull(S);
<translational hull over <regular transformation semigroup of size 12, 
 degree 8 with 4 generators>>
gap> Size(L);
27
gap> Size(R);
256

#T# Creation of translations semigroups we can't calculate
gap> S := InverseSemigroup([
> PartialPerm([1, 2, 3, 4, 5, 6, 7, 10, 11, 13], [4, 6, 7, 3, 8, 2, 9, 5, 1, 12]),
> PartialPerm([1, 2, 4, 8, 11, 12], [3, 5, 1, 10, 11, 7]),
> PartialPerm([1, 2, 7, 9, 12], [5, 6, 4, 3, 10])]);;
gap> T := IdempotentGeneratedSubsemigroup(S);;
gap> M := MunnSemigroup(T);;
gap> LeftTranslations(M);;
gap> RightTranslations(M);;
gap> TranslationalHull(M);;

#T# With calculation - rectangular bands
gap> S := RectangularBand(3, 3);
<regular transformation semigroup of size 9, degree 7 with 3 generators>
gap> L := LeftTranslations(S);
<the semigroup of left translations of <regular transformation semigroup 
 of size 9, degree 7 with 3 generators>>
gap> R := RightTranslations(S);
<the semigroup of right translations of <regular transformation semigroup 
 of size 9, degree 7 with 3 generators>>
gap> H := TranslationalHull(S);
<translational hull over <regular transformation semigroup of size 9, 
 degree 7 with 3 generators>>
gap> Size(L);
27
gap> Size(R);
27
gap> Size(H);
729
gap> Size(GeneratorsOfSemigroup(L));
3
gap> Size(GeneratorsOfSemigroup(R));
3
gap> Size(GeneratorsOfSemigroup(H));
9

#T# small RZMS
gap> G := Range(IsomorphismPermGroup(SmallGroup(4, 2)));;
gap> H := AsList(G);;
gap> mat := [[H[1], 0],
> [H[2], H[2]]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> L := LeftTranslations(S);
<the semigroup of left translations of <regular semigroup with 4 generators>>
gap> Size(L);
81
gap> R := RightTranslations(S);
<the semigroup of right translations of <0-simple regular semigroup with 4 
 generators>>
gap> Size(R);
81
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> L := LeftTranslations(S);
<the semigroup of left translations of <regular semigroup with 4 generators>>
gap> R := RightTranslations(S);
<the semigroup of right translations of <regular semigroup with 4 generators>>
gap> H := TranslationalHull(S);
<translational hull over <regular semigroup with 4 generators>>
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

#T# Further test translations generation
gap> S := Semigroup([Transformation([2, 4, 4, 1]), Transformation([2, 3, 2, 1]), 
> Transformation([3, 3, 3])]);;
gap> L := LeftTranslations(S);
<the semigroup of left translations of <transformation semigroup of degree 4 
 with 3 generators>>
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
> Ll := LeftTranslations(L);;
> Lr := LeftTranslations(R);;
> Rl := RightTranslations(L);;
> Rr := RightTranslations(R);;
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
>   H := TranslationalHull(S);
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
gap> G := Range(IsomorphismPermGroup(SmallGroup(4, 1)));;
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
gap> for h in H do
>      if not h = Bitranslation(H,
>                               LeftPartOfBitranslation(h),
>                               RightPartOfBitranslation(h)) then
>        Print("failure!");
>      fi;
>    od;

#T# Test inner translations
gap> G := Range(IsomorphismPermGroup(SmallGroup(6, 1)));;
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
gap> C := List(InnerLeftTranslations(S), l -> List(S, x -> x ^ l));;
gap> D := List(S, s -> List(S, x -> s * x));;
gap> SortedList(C) = SortedList(D);
true
gap> C := List(InnerRightTranslations(S), r -> List(S, x -> x ^ r));;
gap> D := List(S, s -> List(S, x -> x * s));;
gap> SortedList(C) = SortedList(D);
true

#T# One for translations semigroups elements and translational hull elements
gap> G := Range(IsomorphismPermGroup(SmallGroup(6, 1)));;
gap> a := G.1;; b := G.2;;
gap> mat := [[a, 0],
> [b, a]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> IsSemigroup(S) and IsFinite(S) and IsZeroSimpleSemigroup(S);;
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
gap> H := TranslationalHull(S);;
gap> One(L) = LeftTranslation(L, MappingByFunction(S, S, x -> x));
true
gap> One(R) = RightTranslation(R, MappingByFunction(S, S, x -> x));
true
gap> OneOp(Representative(H)) = Bitranslation(H, One(L), One(R));
true
gap> Semigroup(SEMIGROUPS.BitranslationsBacktrack(H)) = H;
true

#T# TranslationalHull for semigroups which are not IsEnumerableSemigroupRep
gap> S := Semigroup([Transformation([1, 4, 3, 3]), Transformation([3, 4, 1, 1])]);;
gap> S := AsSemigroup(IsFpSemigroup, S);;
gap> IsEnumerableSemigroupRep(S);
false
gap> H := TranslationalHull(S);;
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `TranslationalHull' on 1 arguments

#T# Methods for non-normalised RMS
gap> G := Range(IsomorphismPermGroup(SmallGroup(8, 2)));;
gap> mat := [[G.1, G.2], [G.2, G.2 * G.2]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
gap> Size(L) = Size(Semigroup(GeneratorsOfSemigroup(L)));
true
gap> Size(R) = Size(Semigroup(GeneratorsOfSemigroup(R)));
true

#T# Special methods for normalised RMS 
gap> G := Range(IsomorphismPermGroup(SmallGroup(12, 1)));;
gap> mat := [[G.1, G.2], [G.1, G.1], [G.2, G.3], [G.1 * G.2, G.1 * G.3]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> T := Range(RMSNormalization(S));;
gap> LS := LeftTranslations(S);;
gap> RS := RightTranslations(S);;
gap> HS := TranslationalHull(S);;
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
gap> x := Representative(H);
<linked pair of translations on <simple semigroup of size 96, with 6 
 generators>>
gap> l := LeftTranslation(L, MappingByFunction(T, T, x -> x));;
gap> r := RightTranslation(R, MappingByFunction(T, T, x -> x));;
gap> Bitranslation(H, l, r) = One(x);
true
gap> for x in GeneratorsOfSemigroup(T) do
>      l := LeftTranslation(L, MappingByFunction(T, T, y -> x * y));;
>      r := RightTranslation(R, MappingByFunction(T, T, y -> y * x));;
>      if not Bitranslation(H, l, r) in H then
>        Print("failure!");
>      fi;
>    od;
gap> mat := TransposedMat([[G.1, G.2], [G.1, G.1], [G.2, G.3],
> [G.1 * G.2, G.1 * G.3]]);;
gap> T := Range(RMSNormalization(ReesMatrixSemigroup(G, mat)));;
gap> R := RightTranslations(T);;
gap> Size(R) = Size(Semigroup(GeneratorsOfSemigroup(R)));
true
gap> Size(TranslationalHull(T));
100
gap> G := SmallGroup(16, 2);;
gap> iso := IsomorphismPermGroup(G);;
gap> mat := [[G.1 ^ iso, G.2 ^ iso, (G.2 * G.1) ^ iso],
> [(G.2 * G.2) ^ iso, G.2 ^ iso, (G.2 * G.2) ^ iso],
> [(G.2 * G.1 * G.2) ^ iso, G.3 ^ iso, (G.1 * G.3) ^ iso]];;
gap> S := ReesMatrixSemigroup(Range(iso), mat);;
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
gap> G := Range(IsomorphismPermGroup(SmallGroup(6, 1)));;
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
gap> S := Semigroup([Transformation([1, 4, 3, 3, 6, 5]),
> Transformation([3, 4, 1, 1, 4, 2])]);;
gap> T := Semigroup([Transformation([5, 2, 3, 2, 1]),
> Transformation([2, 3, 1, 1, 2])]);;
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
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
r a list of indices of values of the generators under the translation,
gap> LeftTranslation(L, g);
Error, Semigroups: LeftTranslation (from Mapping): 
the domain and range of the second argument must be the underlying semigroup o\
f the first,
gap> x := [1, Size(S) + 1];;
gap> LeftTranslation(L, x);
Error, Semigroups: LeftTranslation: 
the second argument must map indices of generators to indices of elements of t\
he semigroup of the first argument,
gap> l := LeftTranslation(L, [4, 6]);
Error, Semigroups: LeftTranslation: 
the transformation given must define a left translation,

#T# Error Testing - Right Translations
gap> S := Semigroup([Transformation([1, 4, 3, 3, 6, 5]),
> Transformation([3, 4, 1, 1, 4, 2])]);;
gap> T := Semigroup([Transformation([5, 2, 3, 2, 1]),
> Transformation([2, 3, 1, 1, 2])]);;
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
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
or a list of indices of values of the generators under the translation,
gap> RightTranslation(R, g);
Error, Semigroups: RightTranslation (from Mapping): 
the domain and range of the second argument must be the underlying semigroup o\
f the first,
gap> x := [1, Size(S) + 1];;
gap> RightTranslation(R, x);
Error, Semigroups: RightTranslation: 
the second argument must map indices of generators to indices of elements of t\
he semigroup of the first argument,
gap> r := RightTranslation(R, [6, 3]);
Error, Semigroups: RightTranslation: 
the transformation given must define a right translation,

#T# Error Testing - Translational Hull Elements
gap> S := Semigroup([Transformation([1, 4, 3, 3]), Transformation([3, 4, 1, 1])]);;
gap> T := RectangularBand(3, 4);;
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
gap> RT := RightTranslations(T);;
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

#T# Error Testing - left translations over normalised RMS
gap> G := Range(IsomorphismPermGroup(SmallGroup(12, 1)));;
gap> mat := [[G.1, G.2], [G.1, G.1], [G.2, G.3], [G.1 * G.2, G.1 * G.3]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> T := Range(RMSNormalization(S));;
gap> L := LeftTranslations(T);;
gap> LS := LeftTranslations(S);;
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
gap> G := Range(IsomorphismPermGroup(SmallGroup(12, 1)));;
gap> mat := [[G.1, G.2], [G.1, G.1], [G.2, G.3], [G.1 * G.2, G.1 * G.3]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> T := Range(RMSNormalization(S));;
gap> R := RightTranslations(T);;
gap> RS := RightTranslations(S);;
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
gap> G := Range(IsomorphismPermGroup(SmallGroup(12, 1)));;
gap> mat := [[G.1, G.2], [G.1, G.1], [G.2, G.3]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> T := Range(RMSNormalization(S));;
gap> G := UnderlyingSemigroup(T);;
gap> L := LeftTranslations(T);;
gap> R := RightTranslations(T);;
gap> H := TranslationalHull(T);;
gap> HS := TranslationalHull(S);;
gap> lgpfunc := [G.1 * G.3 * G.3, G.2];;
gap> rgpfunc := [G.1 * G.3 * G.3, G.3 * G.3, G.2];;
gap> lt := Transformation([2, 2]);;
gap> rt := Transformation([3, 3, 3]);;
gap> l := LeftTranslationOfNormalRMS(L, lgpfunc, lt);;
gap> r := RightTranslationOfNormalRMS(R, rgpfunc, rt);;
gap> h := BitranslationOfNormalRMS(H, l, r);
<linked pair of translations on <simple semigroup with 5 generators>>
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
