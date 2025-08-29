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

#@local S, ct, D, H, cm, irr, known, pims, mat, M, m, ccm, matl, perm, i
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

#  ViewString GeneralizedConjugacyClass test
gap> M := FullTransformationMonoid(3);;
gap> m := Transformation([3, 2, 2]);;
gap> GeneralizedConjugacyClass(M, m);
<generalized conjugacy class in Monoid( [ Transformation( [ 2, 3, 1 ] ), Trans\
formation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) for representative Tr\
ansformation( [ 3, 2, 2 ] )>

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

#  Simple check of a monoid DClassBicharacter - 1
gap> S := FullTransformationMonoid(3);;
gap> D := DClasses(S);;
gap> matl := List(D, DClassBicharacter);;
gap> known := [[[6, 0, 0, 0, 0, 0],
> [0, 2, 0, 0, 0, 0],
> [0, 0, 3, 0, 0, 0],
> [0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0]],
> [[18, 0, 0, 6, 0, 0],
> [6, 0, 0, 2, 0, 0],
> [0, 0, 0, 0, 0, 0],
> [6, 0, 0, 2, 0, 0],
> [0, 2, 0, 0, 2, 0],
> [0, 0, 0, 0, 0, 0]],
> [[3, 1, 0, 2, 0, 1],
> [3, 1, 0, 2, 0, 1],
> [3, 1, 0, 2, 0, 1],
> [3, 1, 0, 2, 0, 1],
> [3, 1, 0, 2, 0, 1],
> [3, 1, 0, 2, 0, 1]]];;
gap> for perm in PermutationsList(matl) do
> if ForAll([1 .. Length(known)], i -> (TransformingPermutations(perm[i], known[i]) <> fail)) then
> Display(ForAll([1 .. Length(known)], i -> (TransformingPermutations(perm[i], known[i]) <> fail)));
> fi;
> od;
true

#  Simple check of a monoid DClassBicharacter - 2
gap> S := FullBooleanMatMonoid(2);;
gap> D := DClasses(S);;
gap> matl := List(D, DClassBicharacter);;
gap> known := [[[1, 1, 1, 1, 1], 
> [1, 1, 1, 1, 1], 
> [1, 1, 1, 1, 1], 
> [1, 1, 1, 1, 1], 
> [1, 1, 1, 1, 1]], 
> [[0, 0, 0, 0, 0], 
> [0, 1, 2, 3, 1],
> [0, 2, 4, 6, 2], 
> [0, 3, 6, 9, 3], 
> [0, 1, 2, 3, 1]], 
> [[0, 0, 0, 0, 0], 
> [0, 0, 0, 0, 0], 
> [0, 0, 1, 2, 0], 
> [0, 0, 2, 4, 0], 
> [0, 0, 0, 0, 0]],
> [[0, 0, 0, 0, 0], 
> [0, 0, 0, 0, 0], 
> [0, 0, 0, 0, 0], 
> [0, 0, 0, 2, 0], 
> [0, 0, 0, 0, 2]]];;
gap> for perm in PermutationsList(matl) do
> if ForAll([1 .. Length(known)], i -> (TransformingPermutations(perm[i], known[i]) <> fail)) then
> Display(ForAll([1 .. Length(known)], i -> (TransformingPermutations(perm[i], known[i]) <> fail)));
> fi;
> od;
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/attributes/cartan.tst");
