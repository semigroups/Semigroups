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

#@local S, ct, D, H, cm
gap> START_TEST("Semigroups package: standard/attributes/cartan.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#  Creation of a set of idempotents - 1
gap> S := FullTransformationMonoid(5);;
gap> TransversalIdempotents(S);
[ IdentityTransformation, Transformation( [ 1, 2, 3, 4, 1 ] ), 
  Transformation( [ 2, 2, 3, 4, 2 ] ), Transformation( [ 1, 4, 1, 4, 1 ] ), 
  Transformation( [ 1, 1, 1, 1, 1 ] ) ]

#  Creation of a set of generalized conjugacy class representatives - 1
gap> S := FullTransformationMonoid(6);;
gap> GeneralisedConjugacyClassesRepresentatives(S);
[ IdentityTransformation, Transformation( [ 1, 2, 3, 4, 6, 5 ] ), 
  Transformation( [ 1, 2, 3, 5, 6, 4 ] ), 
  Transformation( [ 1, 2, 4, 3, 6, 5 ] ), 
  Transformation( [ 1, 2, 4, 5, 6, 3 ] ), 
  Transformation( [ 1, 3, 2, 5, 6, 4 ] ), 
  Transformation( [ 1, 3, 4, 5, 6, 2 ] ), 
  Transformation( [ 2, 1, 4, 3, 6, 5 ] ), 
  Transformation( [ 2, 1, 4, 5, 6, 3 ] ), 
  Transformation( [ 2, 3, 1, 5, 6, 4 ] ), 
  Transformation( [ 2, 3, 4, 5, 6, 1 ] ), 
  Transformation( [ 1, 2, 3, 4, 5, 1 ] ), 
  Transformation( [ 1, 2, 3, 5, 4, 1 ] ), 
  Transformation( [ 1, 2, 4, 5, 3, 1 ] ), 
  Transformation( [ 1, 3, 2, 5, 4, 1 ] ), 
  Transformation( [ 1, 3, 4, 5, 2, 1 ] ), 
  Transformation( [ 2, 1, 4, 5, 3, 2 ] ), 
  Transformation( [ 2, 3, 4, 5, 1, 2 ] ), 
  Transformation( [ 2, 2, 3, 4, 5, 2 ] ), 
  Transformation( [ 2, 2, 3, 5, 4, 2 ] ), 
  Transformation( [ 2, 2, 4, 5, 3, 2 ] ), 
  Transformation( [ 3, 3, 2, 5, 4, 3 ] ), 
  Transformation( [ 3, 3, 4, 5, 2, 3 ] ), 
  Transformation( [ 1, 1, 3, 4, 3, 1 ] ), 
  Transformation( [ 1, 1, 4, 3, 4, 1 ] ), 
  Transformation( [ 3, 3, 4, 1, 4, 3 ] ), 
  Transformation( [ 1, 2, 2, 1, 2, 1 ] ), 
  Transformation( [ 2, 1, 1, 2, 1, 2 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 1 ] ) ]

#  Creation of a set of generalized conjugacy classes - 1
gap> S := FullTransformationMonoid(3);;
gap> GeneralisedConjugacyClasses(S);
[ <Generalised Conjugacy Class in Monoid( [ Transformation( [ 2, 3, 1 ] ), Tra\
nsformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) for representative \
IdentityTransformation>, 
  <Generalised Conjugacy Class in Monoid( [ Transformation( [ 2, 3, 1 ] ), Tra\
nsformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) for representative \
Transformation( [ 1, 3, 2 ] )>, 
  <Generalised Conjugacy Class in Monoid( [ Transformation( [ 2, 3, 1 ] ), Tra\
nsformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) for representative \
Transformation( [ 2, 3, 1 ] )>, 
  <Generalised Conjugacy Class in Monoid( [ Transformation( [ 2, 3, 1 ] ), Tra\
nsformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) for representative \
Transformation( [ 1, 2, 1 ] )>, 
  <Generalised Conjugacy Class in Monoid( [ Transformation( [ 2, 3, 1 ] ), Tra\
nsformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) for representative \
Transformation( [ 2, 1, 2 ] )>, 
  <Generalised Conjugacy Class in Monoid( [ Transformation( [ 2, 3, 1 ] ), Tra\
nsformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) for representative \
Transformation( [ 1, 1, 1 ] )> ]

#  Creation of a set of representatives for the generalized conjugacy classes - 
#  1
gap> S := FullTransformationMonoid(3);;
gap> GeneralisedConjugacyClassesRepresentatives(S);
[ IdentityTransformation, Transformation( [ 1, 3, 2 ] ), 
  Transformation( [ 2, 3, 1 ] ), Transformation( [ 1, 2, 1 ] ), 
  Transformation( [ 2, 1, 2 ] ), Transformation( [ 1, 1, 1 ] ) ]

#  Creation of a lazy monoid character table - 1
gap> S := FullTransformationMonoid(3);;
gap> MonoidCharacterTable(S);
MonoidCharacterTable( Monoid( [ Transformation( [ 2, 3, 1 ] ), Transformation(\
 [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) )

#  Creation of the irreducable characters in the ordinary case  - 1
gap> S := FullTransformationMonoid(3);;
gap> ct := MonoidCharacterTable(S);;
gap> Irr(ct);
[ MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [ 2, 3, 1 ]\
 ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) , [ 1, -1,\
 1, 0, 0, 0 ] ), 
  MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [ 2, 3, 1 ]\
 ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) , [ 2, 0, \
-1, 0, 0, 0 ] ), 
  MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [ 2, 3, 1 ]\
 ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) , [ 1, 1, \
1, 0, 0, 0 ] ), 
  MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [ 2, 3, 1 ]\
 ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) , [ 2, 0, \
-1, 1, -1, 0 ] ), 
  MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [ 2, 3, 1 ]\
 ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) , [ 3, 1, \
0, 1, 1, 0 ] ), 
  MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [ 2, 3, 1 ]\
 ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) , [ 1, 1, \
1, 1, 1, 1 ] ) ]

#  Creation of a display monoid character table  - 1
gap> S := FullTransformationMonoid(3);;
gap> ct := MonoidCharacterTable(S);;
gap> Irr(ct);;
gap> Display(ct);
    c.1 c.2 c.3 c.4 c.5 c.6
                           
X.1   1  -1   1   .   .   .
X.2   2   .  -1   .   .   .
X.3   1   1   1   .   .   .
X.4   2   .  -1   1  -1   .
X.5   3   1   .   1   1   .
X.6   1   1   1   1   1   1


#  Computing the bicharater of a D-class - 1
gap> S := FullTransformationMonoid(3);;
gap> D := DClasses(S)[1];;
gap> DClassBicharacter(D);
[ [ 6, 0, 0, 0, 0, 0 ], [ 0, 2, 0, 0, 0, 0 ], [ 0, 0, 3, 0, 0, 0 ], 
  [ 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0 ] ]

#  Computing the bicharater of the regular representation - 1
gap> S := FullTransformationMonoid(3);;
gap> RegularRepresentationBicharacter(S);
[ [ 27, 1, 0, 8, 0, 1 ], [ 9, 3, 0, 4, 0, 1 ], [ 3, 1, 3, 2, 0, 1 ], 
  [ 9, 1, 0, 4, 0, 1 ], [ 3, 3, 0, 2, 2, 1 ], [ 3, 1, 0, 2, 0, 1 ] ]

#  Computing the bicharater of a R-class which contained a group H-class - 1
gap> S := FullTransformationMonoid(3);;
gap> D := RegularDClasses(S)[1];;
gap> H := GroupHClass(D);;
gap> RClassBicharacterOfGroupHClass(H);
[ [ 6, 0, 0, 0, 0, 0 ], [ 0, 2, 0, 0, 0, 0 ], [ 0, 0, 3, 0, 0, 0 ] ]

#  Computing the block diagonal matrix with character tables along the 
#  diagonals - 1
gap> S := FullTransformationMonoid(3);;
gap> DiagonalOfCharacterTables(S);
[ [ 1, -1, 1, 0, 0, 0 ], [ 2, 0, -1, 0, 0, 0 ], [ 1, 1, 1, 0, 0, 0 ], 
  [ 0, 0, 0, 1, -1, 0 ], [ 0, 0, 0, 1, 1, 0 ], [ 0, 0, 0, 0, 0, 1 ] ]

#  Creation of a lazy monoid cartan matrix - 1
gap> S := FullTransformationMonoid(3);;
gap> MonoidCartanMatrix(S);
MonoidCartanMatrix( Monoid( [ Transformation( [ 2, 3, 1 ] ), Transformation( [\
 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) )

#  Creation of the projective indecomposable characters in the ordinary 
#  case  - 1
gap> S := FullTransformationMonoid(3);;
gap> cm := MonoidCartanMatrix(S);;
gap> Pims(cm);
[ MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [ 2, 3, 1 ]\
 ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) , Projecti\
ve Cover Of MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [\
 2, 3, 1 ] ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) \
, [ 1, -1, 1, 0, 0, 0 ] ) ), 
  MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [ 2, 3, 1 ]\
 ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) , Projecti\
ve Cover Of MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [\
 2, 3, 1 ] ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) \
, [ 2, 0, -1, 0, 0, 0 ] ) ), 
  MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [ 2, 3, 1 ]\
 ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) , Projecti\
ve Cover Of MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [\
 2, 3, 1 ] ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) \
, [ 1, 1, 1, 0, 0, 0 ] ) ), 
  MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [ 2, 3, 1 ]\
 ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) , Projecti\
ve Cover Of MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [\
 2, 3, 1 ] ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) \
, [ 2, 0, -1, 1, -1, 0 ] ) ), 
  MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [ 2, 3, 1 ]\
 ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) , Projecti\
ve Cover Of MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [\
 2, 3, 1 ] ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) \
, [ 3, 1, 0, 1, 1, 0 ] ) ), 
  MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [ 2, 3, 1 ]\
 ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) , Projecti\
ve Cover Of MonoidCharacter( MonoidCharacterTable( Monoid( [ Transformation( [\
 2, 3, 1 ] ), Transformation( [ 2, 1 ] ), Transformation( [ 1, 2, 1 ] ) ] ) ) \
, [ 1, 1, 1, 1, 1, 1 ] ) ) ]

#  Creation of a display monoid character table  - 1
gap> S := FullTransformationMonoid(3);;
gap> cm := MonoidCartanMatrix(S);;
gap> Pims(cm);;
gap> Display(cm);
    X.1 X.2 X.3 X.4 X.5 X.6
                           
P.1   1   .   .   .   .   .
P.2   .   1   .   .   .   .
P.3   1   .   1   1   .   .
P.4   1   .   .   1   .   .
P.5   .   .   .   .   1   .
P.6   .   .   .   1   .   1

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/attributes/cartan.tst");
