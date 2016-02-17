#############################################################################
##
#W  standard/congsimple.tst
#Y  Copyright (C) 2014-15                                   Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/congsimple.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

#T# SimpleCongTest1: Create a simple semigroup
gap> S := Semigroup([Transformation([2, 1, 1, 2, 1]),
>                    Transformation([3, 4, 3, 4, 4]),
>                    Transformation([3, 4, 3, 4, 3]),
>                    Transformation([4, 3, 3, 4, 4])]);;

#T# SimpleCongTest2: Find all congruences of a simple semigroup
gap> congs := CongruencesOfSemigroup(S);;
gap> congs[5];
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (2,4,2)>
gap> Size(congs);
34
gap> IsSubrelation(congs[3], congs[4]);
false

#T# SimpleCongTest3: Construct a congruence by generating pairs
gap> cong := SemigroupCongruence(S,
> [[Transformation([1, 2, 1, 2, 2]),
>   Transformation([2, 1, 2, 1, 2])],
>  [Transformation([2, 1, 1, 2, 2]),
>   Transformation([1, 2, 2, 1, 2])]]);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (2,2,2)>
gap> cong = congs[33];
true
gap> cong = congs[11];
false
gap> AsLookupTable(cong);
[ 1, 2, 2, 3, 1, 3, 3, 4, 4, 1, 4, 2, 4, 2, 1, 3 ]

#T# SimpleCongTest4: Testing membership in a congruence
gap> x := Transformation([1, 2, 2, 1, 1]);;
gap> y := Transformation([1, 2, 2, 1, 2]);;
gap> z := Transformation([2, 1, 2, 1, 1]);;
gap> [x, y] in cong;
true
gap> [x, z] in cong;
false
gap> [x, y, z] in cong;
Error, Semigroups: \in (for a congruence): usage,
the first arg <pair> must be a list of length 2,
gap> [Transformation([2, 1, 1, 2, 1]), Transformation([5, 2, 1, 2, 2])] in cong;
Error, Semigroups: \in (for a congruence): usage,
elements of the first arg <pair> must be
in the range of the second arg <cong>,

#T# SimpleCongTest5: Congruence classes
gap> classes := CongruenceClasses(cong);;
gap> Size(classes) = NrCongruenceClasses(cong);
true
gap> EquivalenceClassOfElement(cong, PartialPerm([2], [3]));
Error, Semigroups: EquivalenceClassOfElement: usage,
<elm> must be an element of the range of <cong>,
gap> classx := CongruenceClassOfElement(cong, x);;
gap> classy := CongruenceClassOfElement(cong, y);;
gap> classz := CongruenceClassOfElement(cong, z);
<congruence class of Transformation( [ 2, 1, 2, 1, 1 ] )>
gap> elms := ImagesElm(cong, x);
[ Transformation( [ 1, 2, 2, 1, 2 ] ), Transformation( [ 2, 1, 1, 2, 1 ] ), 
  Transformation( [ 1, 2, 2, 1, 1 ] ), Transformation( [ 2, 1, 1, 2, 2 ] ) ]
gap> ForAll(elms, elm -> elm in classx);
true
gap> Enumerator(classx);
[ Transformation( [ 1, 2, 2, 1, 2 ] ), Transformation( [ 2, 1, 1, 2, 1 ] ), 
  Transformation( [ 1, 2, 2, 1, 1 ] ), Transformation( [ 2, 1, 1, 2, 2 ] ) ]
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
gap> CanonicalRepresentative(classx);
Transformation( [ 1, 2, 2, 1, 2 ] )

#T# SimpleCongTest6: Join and meet congruences
gap> JoinSemigroupCongruences(congs[6], congs[11]) = congs[12];
true
gap> JoinSemigroupCongruences(congs[18], congs[11]) = congs[22];
true
gap> MeetSemigroupCongruences(congs[15], congs[1]) = congs[1];
true
gap> MeetSemigroupCongruences(congs[34], congs[34]) = congs[34];
true

#T# SimpleCongTest7: Quotients
gap> q := S / congs[13];;

#T# SimpleCongTest8
# Convert to and from semigroup congruence by generating pairs
gap> pairs := GeneratingPairsOfSemigroupCongruence(congs[4]);;
gap> cong := SemigroupCongruence(S, pairs);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (1,2,2)>
gap> congs[4] = cong;
true
gap> ForAll(congs, cong ->
> cong = SemigroupCongruence(S, GeneratingPairsOfSemigroupCongruence(cong)));
true

#T# SimpleCongTest9: The universal congruence
gap> S := InverseSemigroup(PartialPerm([1], [2]), PartialPerm([2], [1]));
<inverse partial perm semigroup of rank 2 with 2 generators>
gap> SemigroupCongruence(S, [S.1, S.1 * S.2]);
<universal semigroup congruence over <0-simple inverse partial perm semigroup 
 of rank 2 with 2 generators>>

#T# Test with a 0-simple semigroup
gap> S := Semigroup([Transformation([3, 3, 3]), Transformation([4, 1, 1, 4])]);;
gap> IsRegularSemigroup(S);
true
gap> congs := CongruencesOfSemigroup(S);
[ <universal semigroup congruence over <0-simple regular transformation 
     semigroup of degree 4 with 2 generators>>, 
  <semigroup congruence over <0-simple regular transformation semigroup of 
     degree 4 with 2 generators> with linked triple (1,2,2)> ]
gap> Size(congs);
2

#T# Robustness against infinite semigroups
gap> S := FreeSemigroup(2);;
gap> congs := CongruencesOfSemigroup(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 4th choice method found for `CongruencesOfSemigroup' on 1 arguments

#T# Join/Meet: bad input
gap> S := Semigroup([Transformation([3, 3, 3]), Transformation([4, 1, 1, 4])]);;
gap> IsRegularSemigroup(S);
true
gap> T := Semigroup([Transformation([2, 1, 1, 2, 1]),
>                    Transformation([3, 4, 3, 4, 4]),
>                    Transformation([3, 4, 3, 4, 3]),
>                    Transformation([4, 3, 3, 4, 4])]);;
gap> cong1 := SemigroupCongruence(T,
> [[Transformation([1, 2, 1, 2, 2]),
>   Transformation([2, 1, 2, 1, 2])],
>  [Transformation([2, 1, 1, 2, 2]),
>   Transformation([1, 2, 2, 1, 2])]]);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (2,2,2)>
gap> cong2 := SemigroupCongruence(S, []);
<semigroup congruence over <0-simple regular transformation semigroup of 
 degree 4 with 2 generators> with linked triple (1,2,2)>
gap> MeetSemigroupCongruences(cong1, cong2);
Error, Semigroups: MeetSemigroupCongruences: usage,
<cong1> and <cong2> must be over the same semigroup,
gap> JoinSemigroupCongruences(cong1, cong2);
Error, Semigroups: JoinSemigroupCongruences: usage,
<cong1> and <cong2> must be over the same semigroup,

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(T);
gap> Unbind(classes);
gap> Unbind(classx);
gap> Unbind(classy);
gap> Unbind(classz);
gap> Unbind(cong);
gap> Unbind(cong1);
gap> Unbind(cong2);
gap> Unbind(congs);
gap> Unbind(elms);
gap> Unbind(pairs);
gap> Unbind(q);
gap> Unbind(x);
gap> Unbind(y);
gap> Unbind(z);

#E#
gap> STOP_TEST("Semigroups package: standard/congsimple.tst");
