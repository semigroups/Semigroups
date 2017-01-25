#############################################################################
##
#W  standard/attributes.tst
#Y  Copyright (C) 2016                                            Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/translat.tst");
gap> LoadPackage("semigroups", false);;
gap> SEMIGROUPS.StartTest();

#T# Creation of translations semigroups
# creation of left/right translations semigroups (without calculation)
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
gap> S := SingularTransformationSemigroup(10);
<regular transformation semigroup ideal of degree 10 with 1 generator>
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
gap> S := RectangularBand(3,4);
<regular transformation semigroup of size 12, degree 8 with 4 generators>
gap> L := LeftTranslations(S);
<the semigroup of left translations of <simple transformation semigroup 
 of size 12, degree 8 with 4 generators>>
gap> R := RightTranslations(S);
<the semigroup of right translations of <simple transformation semigroup 
 of size 12, degree 8 with 4 generators>>
gap> H := TranslationalHull(S);
<translational hull over <simple transformation semigroup of size 12, 
 degree 8 with 4 generators>>
gap> Size(L);
27
gap> GeneratorsOfSemigroup(L);
[ <left translation on <simple transformation semigroup of size 12, degree 8 
     with 4 generators>>, 
  <left translation on <simple transformation semigroup of size 12, degree 8 
     with 4 generators>>, 
  <left translation on <simple transformation semigroup of size 12, degree 8 
     with 4 generators>> ]
gap> GeneratorsOfSemigroup(R);
[ <right translation on <simple transformation semigroup of size 12, degree 8 
     with 4 generators>>, 
  <right translation on <simple transformation semigroup of size 12, degree 8 
     with 4 generators>>, 
  <right translation on <simple transformation semigroup of size 12, degree 8 
     with 4 generators>> ]
gap> GeneratorsOfSemigroup(H);
[ <linked pair of translations on <simple transformation semigroup 
     of size 12, degree 8 with 4 generators>>, 
  <linked pair of translations on <simple transformation semigroup 
     of size 12, degree 8 with 4 generators>>, 
  <linked pair of translations on <simple transformation semigroup 
     of size 12, degree 8 with 4 generators>>, 
  <linked pair of translations on <simple transformation semigroup 
     of size 12, degree 8 with 4 generators>>, 
  <linked pair of translations on <simple transformation semigroup 
     of size 12, degree 8 with 4 generators>>, 
  <linked pair of translations on <simple transformation semigroup 
     of size 12, degree 8 with 4 generators>>, 
  <linked pair of translations on <simple transformation semigroup 
     of size 12, degree 8 with 4 generators>>, 
  <linked pair of translations on <simple transformation semigroup 
     of size 12, degree 8 with 4 generators>>, 
  <linked pair of translations on <simple transformation semigroup 
     of size 12, degree 8 with 4 generators>> ]

#T# small RZMS
gap> G := SmallGroup(4,2);;
gap> H := AsList(G);;
gap> mat := [ [H[1], 0, H[1]],
> [H[2], H[2], H[4]],
> [0, H[3], 0]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> L := LeftTranslations(S);
<the semigroup of left translations of <0-simple regular semigroup 
 of size 37, with 6 generators>>
gap> R := RightTranslations(S);
<the semigroup of right translations of <0-simple regular semigroup 
 of size 37, with 6 generators>>
gap> H := TranslationalHull(S);
<translational hull over <0-simple regular semigroup of size 37, with 6 
 generators>>
gap> Size(H);
45
gap> Size(L);
2197
gap> Size(R);
2197
gap> GeneratorsOfSemigroup(L);
[ <left translation on <0-simple regular semigroup of size 37, with 6 
     generators>>, <left translation on <0-simple regular semigroup 
     of size 37, with 6 generators>>, 
  <left translation on <0-simple regular semigroup of size 37, with 6 
     generators>>, <left translation on <0-simple regular semigroup 
     of size 37, with 6 generators>>, 
  <left translation on <0-simple regular semigroup of size 37, with 6 
     generators>>, <left translation on <0-simple regular semigroup 
     of size 37, with 6 generators>> ]
gap> GeneratorsOfSemigroup(R);
[ <right translation on <0-simple regular semigroup of size 37, with 6 
     generators>>, <right translation on <0-simple regular semigroup 
     of size 37, with 6 generators>>, 
  <right translation on <0-simple regular semigroup of size 37, with 6 
     generators>>, <right translation on <0-simple regular semigroup 
     of size 37, with 6 generators>>, 
  <right translation on <0-simple regular semigroup of size 37, with 6 
     generators>>, <right translation on <0-simple regular semigroup 
     of size 37, with 6 generators>> ]

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

#T# OneOp for translations semigroups elements and translational hull elements
gap> G := SmallGroup(6, 1);;
gap> a := G.1;; b := G.2;;
gap> mat := [[a, 0, b],
> [b, a, 0],
> [0, a, b]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> IsSemigroup(S) and IsFinite(S) and IsZeroSimpleSemigroup(S);;
gap> L := LeftTranslations(S);;
gap> R := RightTranslations(S);;
gap> H := TranslationalHull(S);;
gap> OneOp(L.1) = LeftTranslation(L, MappingByFunction(S, S, x -> x));
true
gap> OneOp(R.1) = RightTranslation(R, MappingByFunction(S, S, x -> x));
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(G);
gap> Unbind(L);
gap> Unbind(H);
gap> Unbind(R);
gap> Unbind(S);
gap> Unbind(SEMIGROUPS.bruteforcetranshull);

#E#
gap> STOP_TEST("Semigroups package: standard/translat.tst");
