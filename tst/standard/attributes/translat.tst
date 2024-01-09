#############################################################################
##
#W  standard/translat.tst
#Y  Copyright (C) 2016-17                                          Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local C, D, G, H, HS, I, L, LS, Lht, Ll, Lr, M, R, RS, RT, Rht, Rl, Rr, S, T
#@local a, b, bruteforcetranshull, d, dclasses, f, flag, g, h, ht, i, iso, j, l
#@local l2, lclasses, lgpfunc, linkedpairs, lt, mat, r, r2, rclasses, reps
#@local rgpfunc, rt, s, t, x
gap> START_TEST("Semigroups package: standard/translat.tst");
gap> LoadPackage("semigroups", false);;
gap> SEMIGROUPS.StartTest();

# Creation of translations semigroups
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

# Creation of translations semigroups we can't calculate
gap> S := InverseSemigroup([
> PartialPerm([1, 2, 3, 4, 5, 6, 7, 10, 11, 13], [4, 6, 7, 3, 8, 2, 9, 5, 1, 12]),
> PartialPerm([1, 2, 4, 8, 11, 12], [3, 5, 1, 10, 11, 7]),
> PartialPerm([1, 2, 7, 9, 12], [5, 6, 4, 3, 10])]);;
gap> T := IdempotentGeneratedSubsemigroup(S);;
gap> M := MunnSemigroup(T);;
gap> LeftTranslations(M);;
gap> RightTranslations(M);;
gap> TranslationalHull(M);;

# With calculation - rectangular bands
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

# Monogenic semigroups
gap> S := MonogenicSemigroup(6, 3);
<commutative non-regular transformation semigroup of size 8, degree 9 with 1 
 generator>
gap> L := LeftTranslations(S);
<the semigroup of left translations of <commutative non-regular 
 transformation semigroup of size 8, degree 9 with 1 generator>>
gap> Size(L);
8
gap> GeneratorsOfSemigroup(L);
[ <left translation on <commutative non-regular transformation semigroup 
     of size 8, degree 9 with 1 generator>>, 
  <left translation on <commutative non-regular transformation semigroup 
     of size 8, degree 9 with 1 generator>> ]
gap> R := RightTranslations(S);;
gap> GeneratorsOfSemigroup(R);
[ <right translation on <commutative non-regular transformation semigroup 
     of size 8, degree 9 with 1 generator>>, 
  <right translation on <commutative non-regular transformation semigroup 
     of size 8, degree 9 with 1 generator>> ]
gap> Size(R);
8
gap> H := TranslationalHull(S);;
gap> Size(H);
8
gap> GeneratorsOfSemigroup(H);
[ <bitranslation on <commutative non-regular transformation semigroup 
     of size 8, degree 9 with 1 generator>>, 
  <bitranslation on <commutative non-regular transformation semigroup 
     of size 8, degree 9 with 1 generator>> ]
gap> AsList(TranslationalHull(S));
[ <bitranslation on <commutative non-regular transformation semigroup 
     of size 8, degree 9 with 1 generator>>, 
  <bitranslation on <commutative non-regular transformation semigroup 
     of size 8, degree 9 with 1 generator>>, 
  <bitranslation on <commutative non-regular transformation semigroup 
     of size 8, degree 9 with 1 generator>>, 
  <bitranslation on <commutative non-regular transformation semigroup 
     of size 8, degree 9 with 1 generator>>, 
  <bitranslation on <commutative non-regular transformation semigroup 
     of size 8, degree 9 with 1 generator>>, 
  <bitranslation on <commutative non-regular transformation semigroup 
     of size 8, degree 9 with 1 generator>>, 
  <bitranslation on <commutative non-regular transformation semigroup 
     of size 8, degree 9 with 1 generator>>, 
  <bitranslation on <commutative non-regular transformation semigroup 
     of size 8, degree 9 with 1 generator>> ]

# small RZMS
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
  <left translation on <0-simple regular semigroup with 4 generators>>, 
  <left translation on <0-simple regular semigroup with 4 generators>> ]
gap> GeneratorsOfSemigroup(R);
[ <right translation on <0-simple regular semigroup with 4 generators>>, 
  <right translation on <0-simple regular semigroup with 4 generators>>, 
  <right translation on <0-simple regular semigroup with 4 generators>>, 
  <right translation on <0-simple regular semigroup with 4 generators>>, 
  <right translation on <0-simple regular semigroup with 4 generators>>, 
  <right translation on <0-simple regular semigroup with 4 generators>> ]

# Further test translations generation
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

# Zero semigroups
gap> for i in [1 .. 4] do
> L := LeftZeroSemigroup(i);
> R := RightZeroSemigroup(i);
> Ll := LeftTranslations(L);;
> Lr := LeftTranslations(R);;
> Rl := RightTranslations(L);;
> Rr := RightTranslations(R);;
> if Size(Ll) <> i ^ i or Size(Rr) <> i ^ i
> or Size(Lr) <> 1 or Size(Rl) <> 1 then
> Error();
> fi;
> od;

# Small monoid
gap> S := FullTransformationMonoid(3);;
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
gap> H := TranslationalHull(S);;
gap> Size(L) = Size(R) and Size(R) = Size(H) and Size(H) = Size(S);
true
gap> Size(Semigroup(GeneratorsOfSemigroup(L))) = Size(L);
true
gap> Size(Semigroup(GeneratorsOfSemigroup(R))) = Size(R);
true
gap> Size(Semigroup(GeneratorsOfSemigroup(H))) = Size(H);
true

# Translations and translational hulls of monoids that couldn't be calculated
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
gap> GeneratorsOfSemigroup(H);
[ <bitranslation on <full transformation monoid of degree 5>>, 
  <bitranslation on <full transformation monoid of degree 5>>, 
  <bitranslation on <full transformation monoid of degree 5>>, 
  <bitranslation on <full transformation monoid of degree 5>> ]
gap> GeneratorsOfSemigroup(L);
[ <left translation on <full transformation monoid of degree 5>>, 
  <left translation on <full transformation monoid of degree 5>>, 
  <left translation on <full transformation monoid of degree 5>>, 
  <left translation on <full transformation monoid of degree 5>> ]
gap> GeneratorsOfSemigroup(R);
[ <right translation on <full transformation monoid of degree 5>>, 
  <right translation on <full transformation monoid of degree 5>>, 
  <right translation on <full transformation monoid of degree 5>>, 
  <right translation on <full transformation monoid of degree 5>> ]

# A tiny bit of brute force checking
gap> SEMIGROUPS.bruteforcetranshull := function(S)
>   local a, d, L, R, H, linkedpairs, dclasses, rclasses, lclasses, reps, i, j,
>         l, r, flag;
>   L := LeftTranslations(S);
>   R := RightTranslations(S);
>   H := TranslationalHull(S);
>   linkedpairs := [];
>   dclasses := DClasses(S);
>   reps := [];
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
gap> H := TranslationalHull(S);;
gap> Size(H) = Size(SEMIGROUPS.bruteforcetranshull(S));
true
gap> Size(Semigroup(GeneratorsOfSemigroup(H))) = Size(TranslationalHull(S));
true
gap> S := RectangularBand(2, 3);;
gap> Size(Semigroup(GeneratorsOfSemigroup(TranslationalHull(S))))
> = Size(SEMIGROUPS.bruteforcetranshull(S));
true

# Test translational hull method for arbitrary semigroups
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
>      if h <> Bitranslation(H,
>                            LeftPartOfBitranslation(h),
>                            RightPartOfBitranslation(h)) then
>        Print("failure!");
>      fi;
>    od;

# Test inner translations
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

# One for translations semigroups elements and translational hull elements
gap> G := Range(IsomorphismPermGroup(SmallGroup(6, 1)));;
gap> a := G.1;; b := G.2;;
gap> mat := [[a, 0],
> [b, a]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> IsSemigroup(S) and IsFinite(S) and IsZeroSimpleSemigroup(S);
true
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
gap> H := TranslationalHull(S);;
gap> One(L) = LeftTranslation(L, MappingByFunction(S, S, x -> x));
true
gap> One(R) = RightTranslation(R, MappingByFunction(S, S, x -> x));
true
gap> OneOp(Representative(H)) = Bitranslation(H, One(L), One(R));
true
gap> Semigroup(AsList(H)) = H;
true

# Methods for non-normalised RMS
gap> G := Range(IsomorphismPermGroup(SmallGroup(8, 2)));;
gap> mat := [[G.1, G.2], [G.2, G.2 * G.2]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
gap> Size(L) = Size(Semigroup(GeneratorsOfSemigroup(L)));
true
gap> Size(R) = Size(Semigroup(GeneratorsOfSemigroup(R)));
true

# Special methods for normalised RMS
gap> G := Range(RegularActionHomomorphism(SmallGroup(12, 1)));;
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
gap> Size(HS) = Size(H);
true
gap> Size(H);
444
gap> AsSSortedList(SEMIGROUPS.BitranslationsBacktrack(H)) = AsSSortedList(H);
true
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
<bitranslation on <simple semigroup of size 96, with 6 generators>>
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
gap> l := LeftTranslation(L,
> [(1, 6, 3, 9)(2, 7, 5, 4)(8, 10, 11, 12),
> (1, 6, 3, 9)(2, 7, 5, 4)(8, 10, 11, 12)],
> Transformation([2, 2]));
<left translation on <simple semigroup of size 96, with 6 generators>>
gap> r := RightTranslation(R,
> [(1, 2, 3, 5)(4, 10, 7, 12)(6, 11, 9, 8), (), (), ()],
> Transformation([2, 3, 4, 1]));
<right translation on <simple semigroup of size 96, with 6 generators>>
gap> AsListCanonical(T);;
gap> l2 := LeftTranslation(L, [50, 50]);;
gap> r2 := RightTranslation(R, [9, 5, 6, 12]);;
gap> l = l2 and r = r2;
true

# More normal RMS testing
gap> G := Range(IsomorphismPermGroup(SmallGroup(12, 1)));;
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
gap> Bitranslation(TranslationalHull(T), (1, 4, 3, 2)(5, 7)(6, 8),
> Transformation([1, 1, 1]), Transformation([1, 1, 1]));
<bitranslation on <simple semigroup of size 144, with 4 generators>>
gap> G := Range(IsomorphismPermGroup(SmallGroup(6, 1)));;
gap> mat := [[G.1, G.2], [G.1 * G.2, G.1], [G.2, G.2]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> T := Range(RMSNormalization(S));;
gap> Size(TranslationalHull(T));
40
gap> Size(Semigroup(TranslationalHull(T)));
40
gap> G := Range(IsomorphismPermGroup(SmallGroup(24, 3)));;
gap> mat := [[G.1, G.2, G.2 * G.3]];;
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
gap> Size(HS) = Size(H);
true

# IsWholeFamily for translations semigroups
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

# Subsemigroups of translations
gap> S := Semigroup([Transformation([2, 4, 4, 1]), Transformation([2, 3, 2, 1]),
> Transformation([3, 3, 3])]);;
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
gap> T := Semigroup(Generators(L){[1 .. 20]});
<left translations semigroup over <transformation semigroup of size 49, 
 degree 4 with 3 generators>>
gap> Size(T);
89
gap> ImageSetOfTranslation(AsList(T)[4]);
[ Transformation( [ 1, 1, 1, 1 ] ), Transformation( [ 1, 1, 1, 2 ] ), 
  Transformation( [ 1, 1, 1, 3 ] ), Transformation( [ 1, 1, 1 ] ), 
  Transformation( [ 1, 2, 2, 1 ] ), Transformation( [ 1, 2, 2, 2 ] ), 
  Transformation( [ 1, 2, 2, 3 ] ), Transformation( [ 1, 2, 2 ] ), 
  Transformation( [ 1, 3, 3, 1 ] ), Transformation( [ 1, 3, 3, 3 ] ), 
  Transformation( [ 1, 4, 4, 1 ] ), Transformation( [ 1, 4, 4, 4 ] ), 
  Transformation( [ 2, 1, 1, 1 ] ), Transformation( [ 2, 1, 1, 2 ] ), 
  Transformation( [ 2, 2, 2, 1 ] ), Transformation( [ 2, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 2, 3 ] ), Transformation( [ 2, 2, 2 ] ), 
  Transformation( [ 2, 3, 3, 1 ] ), Transformation( [ 2, 3, 3, 2 ] ), 
  Transformation( [ 2, 3, 3, 3 ] ), Transformation( [ 2, 4, 4, 1 ] ), 
  Transformation( [ 2, 4, 4, 2 ] ), Transformation( [ 2, 4, 4, 4 ] ), 
  Transformation( [ 3, 1, 1, 1 ] ), Transformation( [ 3, 1, 1, 2 ] ), 
  Transformation( [ 3, 1, 1, 3 ] ), Transformation( [ 3, 2, 2, 2 ] ), 
  Transformation( [ 3, 2, 2, 3 ] ), Transformation( [ 3, 3, 3, 1 ] ), 
  Transformation( [ 3, 3, 3, 2 ] ), Transformation( [ 3, 3, 3, 3 ] ), 
  Transformation( [ 3, 3, 3 ] ), Transformation( [ 3, 4, 4, 3 ] ), 
  Transformation( [ 3, 4, 4, 4 ] ), Transformation( [ 4, 1, 1, 1 ] ), 
  Transformation( [ 4, 1, 1, 2 ] ), Transformation( [ 4, 1, 1, 4 ] ), 
  Transformation( [ 4, 2, 2, 2 ] ), Transformation( [ 4, 2, 2, 4 ] ), 
  Transformation( [ 4, 3, 3, 3 ] ), Transformation( [ 4, 3, 3, 4 ] ), 
  Transformation( [ 4, 4, 4, 1 ] ), Transformation( [ 4, 4, 4, 2 ] ), 
  Transformation( [ 4, 4, 4, 3 ] ), Transformation( [ 4, 4, 4, 4 ] ) ]
gap> T := Semigroup(Generators(R){[1 .. 20]});
<right translations semigroup over <transformation semigroup of size 49, 
 degree 4 with 3 generators>>
gap> Size(T);
51
gap> ImageSetOfTranslation(AsList(T)[4]);
[ Transformation( [ 1, 1, 1, 1 ] ), Transformation( [ 1, 1, 1, 2 ] ), 
  Transformation( [ 1, 1, 1 ] ), Transformation( [ 1, 2, 2, 1 ] ), 
  Transformation( [ 1, 2, 2, 2 ] ), Transformation( [ 1, 2, 2 ] ), 
  Transformation( [ 1, 4, 4, 1 ] ), Transformation( [ 1, 4, 4, 4 ] ), 
  Transformation( [ 2, 1, 1, 1 ] ), Transformation( [ 2, 1, 1, 2 ] ), 
  Transformation( [ 2, 2, 2, 1 ] ), Transformation( [ 2, 2, 2, 2 ] ), 
  Transformation( [ 2, 2, 2 ] ), Transformation( [ 2, 4, 4, 1 ] ), 
  Transformation( [ 2, 4, 4, 2 ] ), Transformation( [ 2, 4, 4, 4 ] ), 
  Transformation( [ 4, 1, 1, 1 ] ), Transformation( [ 4, 1, 1, 2 ] ), 
  Transformation( [ 4, 1, 1, 4 ] ), Transformation( [ 4, 2, 2, 2 ] ), 
  Transformation( [ 4, 2, 2, 4 ] ), Transformation( [ 4, 4, 4, 1 ] ), 
  Transformation( [ 4, 4, 4, 2 ] ), Transformation( [ 4, 4, 4, 4 ] ) ]

# Subsemigroups of bitranslations
gap> S := Semigroup([Transformation([2, 4, 4, 1]), Transformation([2, 3, 2, 1]),
> Transformation([3, 3, 3])]);;
gap> H := TranslationalHull(S);;
gap> T := Semigroup(Generators(H){[1 .. 5]});
<semigroup of bitranslations over <transformation semigroup of size 49, 
 degree 4 with 3 generators>>
gap> Size(T);
44

# NrTranslations
gap> S := Semigroup([Transformation([1, 4, 3, 3, 5, 2]),
> Transformation([3, 4, 1, 1, 4, 2])]);;
gap> NrLeftTranslations(S);
208
gap> NrRightTranslations(S);
128
gap> NrBitranslations(S);
78
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
gap> H := TranslationalHull(S);;
gap> HasSize(L) and HasSize(R) and HasSize(H);
true
gap> not (HasAsList(L) or HasAsList(R) or HasAsList(H));
true
gap> G := Range(IsomorphismPermGroup(SmallGroup(12, 1)));;
gap> mat := [[G.1, G.2], [G.1, G.1], [G.2, G.3]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> T := Range(RMSNormalization(S));;
gap> NrLeftTranslations(S) = NrLeftTranslations(T);
true
gap> NrRightTranslations(S) = NrRightTranslations(T);
true
gap> NrBitranslations(S) = NrBitranslations(T);
true

# Error Testing - Left Translations
gap> S := Semigroup([Transformation([1, 4, 3, 3, 6, 5]),
> Transformation([3, 4, 1, 1, 4, 2])]);;
gap> T := Semigroup([Transformation([5, 2, 3, 2, 1]),
> Transformation([2, 3, 1, 1, 2])]);;
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
gap> f := MappingByFunction(S, S, x -> S.1);;
gap> g := MappingByFunction(S, T, x -> T.1);;
gap> LeftTranslation(L, f);
Error, the mapping given must define a left translation
gap> LeftTranslation(L, g);
Error, the domain and range of the second argument must be the underlying semi\
group of the first
gap> x := [1, Size(S) + 1];;
gap> LeftTranslation(L, x);
Error, the second argument must map indices of representatives to indices of e\
lements of the semigroup of the first argument
gap> l := LeftTranslation(L, [4, 6]);
Error, the transformation given must define a left translation
gap> l := LeftTranslation(L, [4]);
Error, the second argument must map indices of representatives to indices of e\
lements of the semigroup of the first argument
gap> l := LeftTranslation(L, MappingByFunction(S, S, x -> S.1 * x));;
gap> T.1 ^ l;
Error, the first argument must be an element of the domain of the second

# Error Testing - Right Translations
gap> S := Semigroup([Transformation([1, 4, 3, 3, 6, 5]),
> Transformation([3, 4, 1, 1, 4, 2])]);;
gap> T := Semigroup([Transformation([5, 2, 3, 2, 1]),
> Transformation([2, 3, 1, 1, 2])]);;
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
gap> f := MappingByFunction(S, S, x -> S.1);;
gap> g := MappingByFunction(S, T, x -> T.1);;
gap> RightTranslation(R, f);
Error, the mapping given must define a right translation
gap> RightTranslation(R, g);
Error, the domain and range of the second argument must be the underlying semi\
group of the first
gap> x := [1, Size(S) + 1];;
gap> RightTranslation(R, x);
Error, the second argument must map indices of representatives to indices of e\
lements of the semigroup of the first argument
gap> r := RightTranslation(R, [6, 3, 2]);
Error, the second argument must map indices of representatives to indices of e\
lements of the semigroup of the first argument
gap> r := RightTranslation(R, [6, 3]);
Error, the transformation given must define a right translation
gap> r := RightTranslation(R, MappingByFunction(S, S, x -> x * S.1));;
gap> T.1 ^ r;
Error, the first argument must be an element of the domain of the second

# Error Testing - Translational Hull Elements
gap> S := Semigroup([Transformation([1, 4, 3, 3]), Transformation([3, 4, 1, 1])]);;
gap> T := RectangularBand(3, 4);;
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
gap> RT := RightTranslations(T);;
gap> H := TranslationalHull(S);;
gap> l := Representative(L);;
gap> r := Representative(R);;
gap> l := LeftTranslation(L, MappingByFunction(S, S, x -> S.1 * x));;
gap> r := RightTranslation(R, MappingByFunction(S, S, x -> x * S.2));;
gap> Bitranslation(H, l, r);
Error, the translations given must satisfy the linking condition
gap> r := Representative(RT);;
gap> Bitranslation(H, l, r);
Error, each argument must have the same underlying semigroup

# Error Testing - left translations over normalised RMS
gap> G := Range(IsomorphismPermGroup(SmallGroup(12, 1)));;
gap> mat := [[G.1, G.2], [G.1, G.1], [G.2, G.3], [G.1 * G.2, G.1 * G.3]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> T := Range(RMSNormalization(S));;
gap> L := LeftTranslations(T);;
gap> LS := LeftTranslations(S);;
gap> lgpfunc := [G.1 * G.3 * G.3, G.2];;
gap> LeftTranslation(L, [G.1, G.1, G.2], IdentityTransformation);
Error, the second argument must be a list of group elements of length equal to\
 the number of rows of the underlying semigroup of the first argument
gap> LeftTranslation(L, lgpfunc, Transformation([1, 3, 2]));
Error, the third argument must be a transformation on the number of rows of th\
e underlying semigroup of the first argument
gap> LeftTranslation(L, [G.1, G.1, G.2], IdentityTransformation);
Error, the second argument must be a list of group elements of length equal to\
 the number of rows of the underlying semigroup of the first argument
gap> LeftTranslation(L, lgpfunc, Transformation([1, 3, 2]));
Error, the third argument must be a transformation on the number of rows of th\
e underlying semigroup of the first argument

# Error Testing - right translations over normalised RMS
gap> G := Range(IsomorphismPermGroup(SmallGroup(12, 1)));;
gap> mat := [[G.1, G.2], [G.1, G.1], [G.2, G.3], [G.1 * G.2, G.1 * G.3]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> T := Range(RMSNormalization(S));;
gap> R := RightTranslations(T);;
gap> RS := RightTranslations(S);;
gap> rgpfunc := [G.1 * G.3 * G.3, G.3 * G.3, G.2, G.1];;
gap> RightTranslation(R, [G.1, G.2], IdentityTransformation);
Error, the second argument must be a list of group elements of length equal to\
 the number of rows of the underlying semigroup of the first argument
gap> RightTranslation(R, rgpfunc, Transformation([1, 3, 2, 5, 4]));
Error, the third argument must be a transformation on the number of columns of\
 the underlying semigroup of the first argument
gap> RightTranslation(R, [G.1, G.2], IdentityTransformation);
Error, the second argument must be a list of group elements of length equal to\
 the number of rows of the underlying semigroup of the first argument
gap> RightTranslation(R, rgpfunc, Transformation([1, 3, 2, 5, 4]));
Error, the third argument must be a transformation on the number of columns of\
 the underlying semigroup of the first argument

# Error testing - bitranslations over normalised RMS
gap> G := Range(RegularActionHomomorphism(SmallGroup(12, 1)));;
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
gap> l := LeftTranslation(L, lgpfunc, lt);;
gap> r := RightTranslation(R, rgpfunc, rt);;
gap> h := Bitranslation(H, l, r);
<bitranslation on <semigroup>>
gap> h := Bitranslation(HS, l, r);
Error, each argument must have the same underlying semigroup
gap> l := LeftTranslation(L, lgpfunc, IdentityTransformation);;
gap> h := Bitranslation(H, l, r);
Error, the translations given must satisfy the linking condition
gap> Bitranslation(H, (1, 2, 3, 5)(4, 10, 7, 12)(6, 11, 9, 8),
> Transformation([2, 2]), Transformation([2, 3, 2]));
Error, the arguments given do not define a bitranslation

# Hashing translations
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

# Hashing translational hull elements
gap> S := Semigroup([Transformation([1, 4, 3, 3]), Transformation([3, 4, 1, 1])]);;
gap> H := TranslationalHull(S);;
gap> ht := HTCreate(Representative(H));;
gap> for h in H do
> HTAdd(ht, h, true);
> od;

# Translational Hull of RZMS which is not completely 0-simple
gap> G := Range(IsomorphismPermGroup(SmallGroup(16, 2)));;
gap> mat := [[G.1, 0, 0], [G.1 * G.2, 0, G.2]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> Size(TranslationalHull(S));
14977

# Translational Hull of RZMS which is not completely 0-simple
gap> G := Range(IsomorphismPermGroup(SmallGroup(16, 2)));;
gap> mat := [[G.1, 0, 0], [G.1 * G.2, 0, G.2]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> NrBitranslations(S);
14977

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/translat.tst");
