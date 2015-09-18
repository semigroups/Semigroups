#############################################################################
##
#W  congruences/univ.tst
#Y  Copyright (C) 2015                                      Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: congruences/univ.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels and user preferences
gap> SEMIGROUPS_StartTest();

#T# CongUnivTest1: No zero, non-simple
gap> s := Semigroup( [ Transformation( [ 1, 3, 4, 1, 3, 7, 5 ] ),
>                      Transformation( [ 5, 7, 1, 6, 1, 7, 6 ] ) ] );;
gap> uni := UniversalSemigroupCongruence(s);
<universal semigroup congruence over <transformation semigroup 
 on 7 pts with 2 generators>>
gap> pairs := GeneratingPairsOfSemigroupCongruence(uni);;
gap> cong := SemigroupCongruence(s, pairs);;
gap> NrCongruenceClasses(cong);
1

#T# CongUnivTest2: Has zero, not 0-simple
gap> s := Semigroup( [ Transformation( [ 2, 4, 3, 5, 5, 7, 1 ] ),
>                      Transformation( [ 6, 2, 3, 3, 1, 5 ] ) ] );;
gap> uni := UniversalSemigroupCongruence(s);;
gap> pairs := GeneratingPairsOfSemigroupCongruence(uni);;
gap> cong := SemigroupCongruence(s, pairs);;
gap> NrCongruenceClasses(cong);
1

#T# CongUnivTest3: Has zero, is 0-simple
gap> r := ReesZeroMatrixSemigroup( Group( [ (5,6) ] ), 
> [ [ 0, (), 0, 0, 0, 0, 0, 0, 0, (5,6), 0, 0, (5,6), (5,6) ], 
>   [ (), 0, (), 0, (), (5,6), 0, (5,6), 0, 0, (5,6), (5,6), (5,6), () ], 
>   [ 0, 0, (), (5,6), 0, 0, 0, (), 0, (5,6), 0, 0, 0, (5,6) ], 
>   [ 0, 0, 0, (5,6), 0, (), (5,6), (), 0, (5,6), 0, (), 0, (5,6) ], 
>   [ 0, (), (5,6), 0, 0, 0, (5,6), (5,6), (), 0, (5,6), (), (5,6), 0 ], 
>   [ 0, (), 0, (5,6), 0, 0, (5,6), 0, (), (5,6), (5,6), (), (5,6), (5,6) ], 
>   [ 0, (5,6), 0, (5,6), 0, (), (5,6), (), 0, 0, 0, (), (), 0 ], 
>   [ (), 0, (), (5,6), (), 0, (5,6), 0, 0, (5,6), (5,6), 0, (5,6), 0 ], 
>   [ 0, (), 0, 0, 0, (5,6), 0, (5,6), (), 0, (5,6), 0, (5,6), 0 ], 
>   [ 0, 0, (5,6), 0, 0, (), (5,6), 0, 0, 0, 0, (), 0, 0 ], 
>   [ 0, (5,6), (), (5,6), 0, 0, 0, (), 0, 0, 0, 0, (), 0 ] ] );;
gap> uni := UniversalSemigroupCongruence(r);;
gap> pairs := GeneratingPairsOfSemigroupCongruence(uni);;
gap> cong := SemigroupCongruence(r, pairs);;
gap> NrCongruenceClasses(cong);
1

#T# CongUnivTest4: No zero, is simple
gap> s := Semigroup(
> [ Transformation( [ 1, 1, 1, 1, 5, 1, 1 ] ), 
>   Transformation( [ 1, 5, 1, 1, 5, 1, 1 ] ), 
>   Transformation( [ 3, 3, 3, 3, 5, 3, 3 ] ), 
>   Transformation( [ 3, 5, 3, 3, 5, 3, 3 ] ), 
>   Transformation( [ 4, 4, 4, 4, 5, 4, 4 ] ), 
>   Transformation( [ 4, 5, 4, 4, 5, 4, 4 ] ), 
>   Transformation( [ 6, 5, 6, 6, 5, 6, 6 ] ), 
>   Transformation( [ 6, 6, 6, 6, 5, 6, 6 ] ), 
>   Transformation( [ 7, 5, 7, 7, 5, 7, 7 ] ), 
>   Transformation( [ 7, 7, 7, 7, 5, 7, 7 ] ) ] );;
gap> uni := UniversalSemigroupCongruence(r);;
gap> pairs := GeneratingPairsOfSemigroupCongruence(uni);;
gap> cong := SemigroupCongruence(r, pairs);;
gap> NrCongruenceClasses(cong);
1

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(s);
gap> Unbind(r);
gap> Unbind(uni);
gap> Unbind(pairs);
gap> Unbind(cong);

#E#
gap> STOP_TEST( "Semigroups package: congruences/simple.tst");
