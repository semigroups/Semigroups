#############################################################################
##
#W  pairs-cong.tst
#Y  Copyright (C) 2014-15                                   Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: pairs-cong.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels and user preferences
gap> SemigroupsStartTest();

#T# PairsCongTest1
gap> s := Semigroup( [ Transformation( [ 1, 3, 4, 1, 3, 5 ] ),
>    Transformation( [ 2, 4, 6, 1, 6, 5 ] ),
>    Transformation( [ 4, 1, 2, 6, 2, 1 ] ), 
>    Transformation( [ 4, 6, 4, 3, 3, 3 ] ),
>    Transformation( [ 5, 1, 6, 1, 6, 3 ] ),
>    Transformation( [ 5, 2, 5, 3, 5, 3 ] ) ] );;
gap> gens := [
>  [ Transformation( [ 5, 5, 2, 4, 2, 4 ] ),
>    Transformation( [ 1, 5, 4, 5, 4, 5 ] ) ],
>  [ Transformation( [ 3, 3, 3, 6, 3, 3 ] ),
>    Transformation( [ 1, 6, 6, 6, 6, 1 ] ) ] ];;
gap> cong := SemigroupCongruence( s, gens );
<semigroup congruence over <transformation semigroup 
 on 6 pts with 6 generators> with 2 generating pairs>
gap> gens[2] in cong;
true
gap> x := Transformation( [ 6, 5, 4, 4, 4, 6 ] );;
gap> y := Transformation( [ 2, 2, 2, 6, 2, 4 ] );;
gap> z := Transformation( [ 2, 4, 6, 1, 6, 5 ] );;
gap> [x,y] in cong; [x,z] in cong; [y,z] in cong;
true
false
false
gap> classes := CongruenceClasses(cong);;
gap> Size(classes) = NrCongruenceClasses(cong);
true
gap> classx := CongruenceClassOfElement(cong, x);;
gap> classy := CongruenceClassOfElement(cong, y);;
gap> classz := CongruenceClassOfElement(cong, z);
{Transformation( [ 2, 4, 6, 1, 6, 5 ] )}
gap> classx = classy;
true
gap> classz = classx;
false
gap> x in classx;
true
gap> y in classx;
true
gap> x in classz;
false
gap> classx = classes[1];
true
gap> classz = classes[133];
true
gap> z * y in classz * classy;
true
gap> y * z in classx * classz;
true
gap> Size(classx);
3084
gap> q := s / cong;;
gap> P := [ [ (), 0, (1,3), (1,3), 0, (), 0 ],
>   [ (), (1,3), 0, 0, (1,3), (), 0 ], [ (), (1,3), 0, (), 0, 0, () ],          
>   [ 0, (), (1,3), (1,3), (), 0, 0 ], [ 0, 0, 0, (), (), (1,3), () ],
>   [ (), 0, (1,3), 0, (), 0, () ] ];;                                
gap> R := ReesZeroMatrixSemigroup(Group([(1,3)]), P);;
gap> x := ReesZeroMatrixSemigroupElement(R, 1, (1,3), 1);;
gap> y := ReesZeroMatrixSemigroupElement(R, 1, (), 1);;
gap> cong := SemigroupCongruenceByGeneratingPairs(R, [[x,y]]);;
gap> c := EquivalenceClasses(cong);;
gap> Size(c) = 43;
true
gap> cong := SemigroupCongruenceByGeneratingPairs(R, [ ]);;
gap> c := EquivalenceClasses(cong);;
gap> Size(c) = 85;
true

#E#
gap> STOP_TEST( "Semigroups package: simple-cong.tst");
