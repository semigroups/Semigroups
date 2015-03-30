#############################################################################
##
#W  simple-cong.tst
#Y  Copyright (C) 2014-15                                   Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: simple-cong.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels and user preferences
gap> SemigroupsStartTest();

#T# SimpleCongTest1: Create a simple semigroup
gap> s := Semigroup( [Transformation( [ 2, 1, 1, 2, 1 ] ), 
>                     Transformation( [ 3, 4, 3, 4, 4 ] ),
>                     Transformation( [ 3, 4, 3, 4, 3 ] ), 
>                     Transformation( [ 4, 3, 3, 4, 4 ] ) ] );;

#T# SimpleCongTest2: Find all congruences of a simple semigroup
gap> congs := CongruencesOfSemigroup(s);;
gap> Size(congs);
34

#T# SimpleCongTest3: Construct a congruence by generating pairs
gap> cong := SemigroupCongruence(s,
> [ [ Transformation( [ 1, 2, 1, 2, 2 ] ),
>     Transformation( [ 2, 1, 2, 1, 2 ] ) ], 
>   [ Transformation( [ 2, 1, 1, 2, 2 ] ),
>     Transformation( [ 1, 2, 2, 1, 2 ] ) ] ] );
<semigroup congruence over <simple transformation semigroup 
 on 5 pts with 4 generators> with linked triple (2,2,2)>
gap> cong = congs[33];
true
gap> cong = congs[11];
false

#T# SimpleCongTest4: Testing membership in a congruence
gap> x := Transformation( [ 1, 2, 2, 1, 1 ] );;
gap> y := Transformation( [ 1, 2, 2, 1, 2 ] );;
gap> z := Transformation( [ 2, 1, 2, 1, 1 ] );;
gap> [x,y] in cong;
true
gap> [x,z] in cong;
false

#T# SimpleCongTest5: Congruence classes
gap> classes := CongruenceClasses(cong);;
gap> Size(classes) = NrCongruenceClasses(cong);
true
gap> classx := CongruenceClassOfElement(cong, x);;
gap> classy := CongruenceClassOfElement(cong, y);;
gap> classz := CongruenceClassOfElement(cong, z);
{Transformation( [ 2, 1, 2, 1, 1 ] )}
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
gap> z * y in classz * classy;
true
gap> y * z in classx * classz;
true
gap> x * z in classz * classx;
false
gap> Size(classx);
4

#T# SimpleCongTest6: Join and meet congruences
gap> JoinSemigroupCongruences(congs[6],congs[11]) = congs[12];
true
gap> JoinSemigroupCongruences(congs[18],congs[11]) = congs[22];
true
gap> MeetSemigroupCongruences(congs[15],congs[1]) = congs[1];
true
gap> MeetSemigroupCongruences(congs[34],congs[34]) = congs[34];
true

#T# SimpleCongTest7: Quotients
gap> q := s / congs[13];;

#T# SimpleCongTest8
# Convert to and from semigroup congruence by generating pairs
gap> pairs := GeneratingPairsOfSemigroupCongruence(congs[4]);;
gap> cong := SemigroupCongruence(s, pairs);
<semigroup congruence over <simple transformation semigroup 
 on 5 pts with 4 generators> with linked triple (1,2,2)>
gap> congs[4] = cong;
true
gap> ForAll(congs, cong ->
> cong = SemigroupCongruence(s, GeneratingPairsOfSemigroupCongruence(cong)));
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(pairs);
gap> Unbind(classy);
gap> Unbind(congs);
gap> Unbind(q);
gap> Unbind(s);
gap> Unbind(classes);
gap> Unbind(classz);
gap> Unbind(classx);
gap> Unbind(cong);
gap> Unbind(y);
gap> Unbind(x);
gap> Unbind(z);

#E#
gap> STOP_TEST( "Semigroups package: simple-cong.tst");
