#############################################################################
##
#W  standard/attributes/cartan.tst
#Y  Copyright (C) 2024                                   Balthazar Charles
##                                                             Joseph Ruiz
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local S, ct, D, H, cm, irr, known, pims, mat, M, m, ccm
gap> START_TEST("Semigroups package: standard/attributes/cartan.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#  Creation of a lazy monoid character table - 1
gap> S := FullTransformationMonoid(3);;
gap> MonoidCharacterTable(S);
MonoidCharacterTable( Monoid( [ Transformation( [ 2, 3, 1 ] ), Transformation(\
 [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) )

#  Basic GeneralizedConjugacyClass information is strored correctly
gap> M := FullTransformationMonoid(3);;
gap> m := Random(M);;
gap> ccm := GeneralizedConjugacyClass(M, m);;
gap> Representative(ccm) = m;
true
gap> ParentAttr(ccm) = M;
true

#  Simple check of a monoid character table  - 1
gap> S := FullTransformationMonoid(3);;
gap> ct := MonoidCharacterTable(S);;
gap> irr := Irr(ct);;
gap> mat := List(irr, ValuesOfMonoidClassFunction);;
gap> known := [[1, -1, 1, 0, 0, 0],
> [2, 0, -1, 0, 0, 0],
> [1, 1, 1, 0, 0, 0],
> [2, 0, -1, 1, -1, 0],
> [3, 1, 0, 1, 1, 0],
> [1, 1, 1, 1, 1, 1]];;
gap> TransformingPermutations(mat, known) <> fail;
true

#  Creation of a lazy monoid cartan matrix - 1
gap> S := FullTransformationMonoid(3);;
gap> MonoidCartanMatrix(S);
MonoidCartanMatrix( Monoid( [ Transformation( [ 2, 3, 1 ] ), Transformation( [\
 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) )

#  Simple check of a monoid cartan matrix  - 1
gap> S := FullTransformationMonoid(3);;
gap> cm := MonoidCartanMatrix(S);;
gap> pims := Pims(cm);;
gap> mat := List(pims, ValuesOfCompositionFactorsFunction);;
gap> known := [[1, 0, 0, 0, 0, 0],
> [0, 1, 0, 0, 0, 0],
> [1, 0, 1, 1, 0, 0],
> [1, 0, 0, 1, 0, 0],
> [0, 0, 0, 0, 1, 0],
> [0, 0, 0, 1, 0, 1]];;
gap> TransformingPermutations(mat, known) <> fail;
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/attributes/cartan.tst");
