###########################################################################
##
#W  rees-cong.tst
#Y  Copyright (C) 2015                                      Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: rees-cong.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels and user preferences
gap> SemigroupsStartTest();

#T# ReesCongTest1
# Create a congruence
gap> s := Semigroup( [ Transformation( [ 2, 4, 3, 5, 5 ] ),
>                 Transformation( [ 3, 1, 1, 4, 4 ] ),
>                 Transformation( [ 3, 1, 4, 2, 4 ] ), 
>                 Transformation( [ 3, 4, 2, 3, 4 ] ),
>                 Transformation( [ 4, 1, 5, 1, 2 ] ) ] );
<transformation semigroup on 5 pts with 5 generators>
gap> i := SemigroupIdeal(s, [ Transformation( [ 3, 1, 1, 4, 4 ] ),
>                             Transformation( [ 1, 4, 1, 4, 1 ] ) ] );
<regular transformation semigroup ideal on 5 pts with 2 generators>
gap> cong := ReesCongruenceOfSemigroupIdeal(i);
<Rees congruence of <regular transformation semigroup ideal 
 on 5 pts with 2 generators> over <transformation semigroup 
 on 5 pts with 5 generators>>

#T# ReesCongTest2
# Calculate its congruence classes
gap> NrCongruenceClasses(cong);
19
gap> cc := CongruenceClasses(cong);;
gap> Size(cc);
19

#T# ReesCongTest3
# Try some operations on the congruence classes
gap> List(cc, Size);
[ 1095, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
gap> cc[1] * cc[1];
{Transformation( [ 3, 1, 1, 4, 4 ] )}
gap> cc[2] * cc[1];
{Transformation( [ 3, 1, 1, 4, 4 ] )}
gap> cc[4] * cc[6];
{Transformation( [ 2, 4, 1, 3, 1 ] )}
gap> cc[3] * cc[2] = cc[8];
true

#T# ReesCongTest4
# Convert the congruence to generating pairs
gap> ccong := AsSemigroupCongruenceByGeneratingPairs(cong);
<semigroup congruence over <transformation semigroup of size 1113, 
 on 5 pts with 5 generators> with 1 generating pairs>
gap> NrCongruenceClasses(ccong);
19

#T# ReesCongTest5
# Test the \in function
gap> x := Transformation( [ 3, 4, 2, 4 ] );;      # not in i
gap> y := Transformation( [ 1, 5, 5, 5, 4 ] );;   # in i
gap> [x,y] in cong;
false
gap> [x,x] in cong;
true
gap> x := Transformation( [ 1, 3, 4, 1, 4 ] );;   # in i
gap> [x,y] in cong;
true

#T# ReesCongTest6
gap> im := ImagesElm(cong, x);;
gap> Size(im) = Size(i);
true
gap> ForAll(im, x-> x in i);
true

#E#
gap> STOP_TEST( "Semigroups package: rees-cong.tst");
