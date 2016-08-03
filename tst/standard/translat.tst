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

#
gap> SEMIGROUPS.StartTest();

#T# TranslatTest1
# creation of left/right translations semigroups (without calculation)
gap> S := RectangularBand(3,4);;
gap> LeftTranslationsSemigroup(S);
<the semigroup of left translations of <regular transformation semigroup 
 of size 12, degree 8 with 4 generators>>
gap> RightTranslationsSemigroup(S);
<the semigroup of right translations of <regular transformation semigroup 
 of size 12, degree 8 with 4 generators>>
gap> TranslationalHullSemigroup(S);
<translational hull over <regular transformation semigroup of size 12, 
 degree 8 with 4 generators>>

# for semigroups it can't calculate translations semigroups of
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

# with calculation - rectangular bands
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

# small RZMS
gap> G := SmallGroup(4,2);;
gap> G_list := AsList(G);;
gap> mat := [ [G_list[1], 0, G_list[1]],
> [G_list[2], G_list[2], G_list[4]],
> [0, G_list[3], 0]];;
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
          
          
gap> Unbind(G);
gap> Unbind(G_list);
gap> Unbind(L);
gap> Unbind(H);
gap> Unbind(R);
gap> Unbind(S);

#E#
gap> STOP_TEST("Semigroups package: standard/translat.tst");
