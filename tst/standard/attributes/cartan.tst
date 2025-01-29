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

#@local S, ct
gap> START_TEST("Semigroups package: standard/attributes/cartan.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#  Creation of a set of idempotents - 1
gap> S := FullTransformationMonoid(10);;
gap> TransversalIdempotents(S);
[ IdentityTransformation, Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 1 ] ), 
  Transformation( [ 2, 2, 3, 4, 5, 6, 7, 8, 9, 2 ] ), 
  Transformation( [ 2, 2, 3, 6, 5, 6, 7, 8, 9, 2 ] ), 
  Transformation( [ 2, 2, 5, 5, 5, 6, 7, 8, 9, 2 ] ), 
  Transformation( [ 1, 1, 3, 4, 5, 5, 8, 8, 8, 1 ] ), 
  Transformation( [ 2, 2, 2, 4, 6, 6, 9, 9, 9, 2 ] ), 
  Transformation( [ 1, 1, 6, 6, 6, 6, 6, 8, 8, 1 ] ), 
  Transformation( [ 2, 2, 2, 2, 2, 2, 2, 2, 9, 2 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ) ]

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

#  Creation of a irreducable characters in the ordinary case  - 1
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


#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/attributes/cartan.tst");
