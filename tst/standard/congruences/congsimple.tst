#############################################################################
##
#W  standard/congruences/congsimple.tst
#Y  Copyright (C) 2014-2022                                 Michael Young
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local C, S, T, acting, classes, classx, classy, classz, cong, cong1, cong2
#@local congs, elms, i, j, map, x, y, z
gap> START_TEST("Semigroups package: standard/congruences/congsimple.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# SimpleCongTest2: Find all congruences of a simple semigroup
gap> S := Semigroup([Transformation([2, 1, 1, 2, 1]),
>                    Transformation([3, 4, 3, 4, 4]),
>                    Transformation([3, 4, 3, 4, 3]),
>                    Transformation([4, 3, 3, 4, 4])]);;
gap> congs := CongruencesOfSemigroup(S);;
gap> cong := SemigroupCongruence(S, [Transformation([2, 1, 1, 2, 1]),
>                                    Transformation([1, 2, 2, 1, 2])]);;
gap> i := Positions(congs, cong);;
gap> Length(i) = 1;
true
gap> IsSimpleSemigroupCongruence(congs[i[1]]);
true
gap> Size(congs);
34
gap> cong1 := SemigroupCongruence(S, [[Transformation([1, 2, 2, 1, 2]),
>                                      Transformation([1, 2, 2, 1, 1])],
>                                     [Transformation([4, 3, 3, 4, 3]),
>                                      Transformation([4, 3, 3, 4, 4])]]);;
gap> cong2 := SemigroupCongruence(S, [[Transformation([1, 2, 1, 2, 2]),
>                                      Transformation([1, 2, 1, 2, 1])],
>                                     [Transformation([1, 2, 2, 1, 2]),
>                                      Transformation([1, 2, 2, 1, 1])]]);;
gap> i := Positions(congs, cong1);;
gap> j := Positions(congs, cong2);;
gap> Length(i) = 1 and Length(j) = 1;
true
gap> i := i[1];;
gap> j := j[1];;
gap> IsSubrelation(congs[i], congs[j]);
false

# SimpleCongTest3: Construct a congruence by generating pairs
gap> S := Semigroup([Transformation([2, 1, 1, 2, 1]),
>                    Transformation([3, 4, 3, 4, 4]),
>                    Transformation([3, 4, 3, 4, 3]),
>                    Transformation([4, 3, 3, 4, 4])]);;
gap> congs := CongruencesOfSemigroup(S);;
gap> cong1 := SemigroupCongruence(S,
> [[Transformation([1, 2, 1, 2, 2]),
>   Transformation([2, 1, 2, 1, 2])],
>  [Transformation([2, 1, 1, 2, 2]),
>   Transformation([1, 2, 2, 1, 2])]]);;
gap> i := Positions(congs, cong1);;
gap> Length(i) = 1;
true
gap> cong2 := SemigroupCongruence(S, [[Transformation([1, 2, 1, 2, 2]),
>                                      Transformation([1, 2, 1, 2, 1])],
>                                     [Transformation([1, 2, 1, 2, 2]),
>                                      Transformation([1, 2, 2, 1, 1])]]);;
gap> j := Positions(congs, cong2);;
gap> Length(j) = 1;
true
gap> i = j;
false
gap> congs[i[1]] = congs[j[1]];
false
gap> cong1 = cong2;
false
gap> EquivalenceRelationCanonicalLookup(cong1);
[ 1, 2, 2, 3, 1, 3, 3, 4, 4, 1, 4, 2, 4, 2, 1, 3 ]

# SimpleCongTest4: Testing membership in a congruence
gap> S := Semigroup([Transformation([2, 1, 1, 2, 1]),
>                    Transformation([3, 4, 3, 4, 4]),
>                    Transformation([3, 4, 3, 4, 3]),
>                    Transformation([4, 3, 3, 4, 4])]);;
gap> IsSimpleSemigroup(S);
true
gap> cong := SemigroupCongruence(S,
> [[Transformation([1, 2, 1, 2, 2]),
>   Transformation([2, 1, 2, 1, 2])],
>  [Transformation([2, 1, 1, 2, 2]),
>   Transformation([1, 2, 2, 1, 2])]]);;
gap> x := Transformation([1, 2, 2, 1, 1]);;
gap> y := Transformation([1, 2, 2, 1, 2]);;
gap> z := Transformation([2, 1, 2, 1, 1]);;
gap> [x, y] in cong;
true
gap> [x, z] in cong;
false
gap> [x, y, z] in cong;
Error, the 1st argument (a list) does not have length 2
gap> [Transformation([2, 1, 1, 2, 1]), Transformation([5, 2, 1, 2, 2])] in cong;
Error, the items in the 1st argument (a list) do not all belong to the range o\
f the 2nd argument (a 2-sided semigroup congruence)

# SimpleCongTest5: Congruence classes
gap> S := Semigroup([Transformation([2, 1, 1, 2, 1]),
>                    Transformation([3, 4, 3, 4, 4]),
>                    Transformation([3, 4, 3, 4, 3]),
>                    Transformation([4, 3, 3, 4, 4])]);;
gap> IsSimpleSemigroup(S);
true
gap> cong := SemigroupCongruence(S,
> [[Transformation([1, 2, 1, 2, 2]),
>   Transformation([2, 1, 2, 1, 2])],
>  [Transformation([2, 1, 1, 2, 2]),
>   Transformation([1, 2, 2, 1, 2])]]);;
gap> classes := EquivalenceClasses(cong);;
gap> Size(classes[1]);
4
gap> Representative(classes[1]);
Transformation( [ 1, 2, 2, 1, 2 ] )
gap> Size(classes) = NrEquivalenceClasses(cong);
true
gap> EquivalenceClassOfElement(cong, PartialPerm([2], [3]));
Error, the 2nd argument (a mult. elt.) does not belong to the range of the 1st\
 argument (a 2-sided congruence)
gap> classx := EquivalenceClassOfElement(cong, x);;
gap> classy := EquivalenceClassOfElement(cong, y);;
gap> classz := EquivalenceClassOfElement(cong, z);
<2-sided congruence class of Transformation( [ 2, 1, 2, 1, 1 ] )>
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
gap> Representative(classx);
Transformation( [ 1, 2, 2, 1, 1 ] )

# SimpleCongTest6: Join and meet congruences
gap> S := Semigroup([Transformation([2, 1, 1, 2, 1]),
>                    Transformation([3, 4, 3, 4, 4]),
>                    Transformation([3, 4, 3, 4, 3]),
>                    Transformation([4, 3, 3, 4, 4])]);;
gap> IsSimpleSemigroup(S);
true
gap> congs := [];;
gap> congs[1] := SemigroupCongruence(S, []);;
gap> congs[2] := SemigroupCongruence(S, [Transformation([1, 2, 1, 2, 1]),
>                                        Transformation([3, 4, 3, 4, 3])]);;
gap> congs[3] := SemigroupCongruence(S, [[Transformation([1, 2, 1, 2, 2]),
>                                         Transformation([1, 2, 1, 2, 1])],
>                                        [Transformation([1, 2, 1, 2, 2]),
>                                         Transformation([1, 2, 2, 1, 1])]]);;
gap> congs[4] := SemigroupCongruence(S, [[Transformation([1, 2, 1, 2, 1]),
>                                         Transformation([3, 4, 3, 4, 3])],
>                                        [Transformation([1, 2, 1, 2, 2]),
>                                         Transformation([1, 2, 1, 2, 1])],
>                                        [Transformation([1, 2, 1, 2, 2]),
>                                         Transformation([1, 2, 2, 1, 1])]]);;
gap> congs[5] := SemigroupCongruence(S, [Transformation([1, 2, 2, 1, 2]),
>                                        Transformation([1, 2, 1, 2, 2])]);;
gap> congs[6] := SemigroupCongruence(S, [[Transformation([1, 2, 1, 2, 1]),
>                                         Transformation([1, 2, 2, 1, 1])],
>                                        [Transformation([1, 2, 1, 2, 1]),
>                                         Transformation([3, 4, 3, 4, 3])],
>                                        [Transformation([1, 2, 2, 1, 2]),
>                                         Transformation([1, 2, 1, 2, 2])]]);;
gap> congs[7] := UniversalSemigroupCongruence(S);;
gap> congs[8] := SemigroupCongruence(S, [[Transformation([1, 2, 1, 2, 1]),
>                                         Transformation([3, 4, 3, 4, 3])],
>                                        [Transformation([1, 2, 1, 2, 2]),
>                                         Transformation([1, 2, 1, 2, 1])],
>                                        [Transformation([1, 2, 2, 1, 2]),
>                                         Transformation([1, 2, 2, 1, 1])]]);;
gap> JoinSemigroupCongruences(congs[2], congs[3]) = congs[4];
true
gap> JoinSemigroupCongruences(congs[6], congs[3]) = congs[7];
true
gap> JoinSemigroupCongruences(congs[6], congs[4]) = congs[7];
true
gap> MeetSemigroupCongruences(congs[5], congs[1]) = congs[1];
true
gap> IsSubrelation(congs[5], congs[1]);
true
gap> MeetSemigroupCongruences(congs[8], congs[8]) = congs[8];
true
gap> JoinSemigroupCongruences(congs[8], congs[8]) = congs[8];
true

# SimpleCongTest7: Quotients
gap> S := Semigroup([Transformation([2, 1, 1, 2, 1]),
>                    Transformation([3, 4, 3, 4, 4]),
>                    Transformation([3, 4, 3, 4, 3]),
>                    Transformation([4, 3, 3, 4, 4])]);;
gap> IsSimpleSemigroup(S);
true
gap> cong := SemigroupCongruence(S, [Transformation([1, 2, 1, 2, 2]),
>                                    Transformation([1, 2, 2, 1, 1])]);;
gap> Size(S / cong);
6

# SimpleCongTest8
# Convert to and from semigroup congruence by generating pairs
gap> S := Semigroup([Transformation([2, 1, 1, 2, 1]),
>                    Transformation([3, 4, 3, 4, 4]),
>                    Transformation([3, 4, 3, 4, 3]),
>                    Transformation([4, 3, 3, 4, 4])]);;
gap> congs := CongruencesOfSemigroup(S);;
gap> ForAll(congs, cong ->
> cong = SemigroupCongruence(S, GeneratingPairsOfSemigroupCongruence(cong)));
true

# SimpleCongTest9: The universal congruence
gap> S := InverseSemigroup(PartialPerm([1], [2]), PartialPerm([2], [1]));
<inverse partial perm semigroup of rank 2 with 2 generators>
gap> IsZeroSimpleSemigroup(S);
true
gap> C := SemigroupCongruence(S, [S.1, S.1 * S.2]);;
gap> NrEquivalenceClasses(C);
1

# Test with a 0-simple semigroup
gap> S := Semigroup([Transformation([3, 3, 3]), Transformation([4, 1, 1, 4])]);;
gap> IsRegularSemigroup(S);
true
gap> Size(CongruencesOfSemigroup(S));
2

# Join/Meet: bad input
gap> S := Semigroup([Transformation([3, 3, 3]), Transformation([4, 1, 1, 4])]);;
gap> IsZeroSimpleSemigroup(S);
true
gap> IsRegularSemigroup(S);
true
gap> T := Semigroup([Transformation([2, 1, 1, 2, 1]),
>                    Transformation([3, 4, 3, 4, 4]),
>                    Transformation([3, 4, 3, 4, 3]),
>                    Transformation([4, 3, 3, 4, 4])]);;
gap> IsSimpleSemigroup(T);
true
gap> cong1 := SemigroupCongruence(T,
> [[Transformation([1, 2, 1, 2, 2]),
>   Transformation([2, 1, 2, 1, 2])],
>  [Transformation([2, 1, 1, 2, 2]),
>   Transformation([1, 2, 2, 1, 2])]]);;
gap> cong2 := SemigroupCongruence(S, []);;
gap> MeetSemigroupCongruences(cong1, cong2);
Error, cannot form the meet of congruences over different semigroups
gap> JoinSemigroupCongruences(cong1, cong2);
Error, cannot form the join of congruences over different semigroups

# not simple or 0-simple: try next method
gap> S := OrderEndomorphisms(2);;
gap> IsSimpleSemigroup(S) or IsZeroSimpleSemigroup(S);
false
gap> congs := CongruencesOfSemigroup(S);;
gap> Size(congs);
3

# ViewObj for a 0-simple semigroup congruence
gap> S := Semigroup([Transformation([3, 3, 3]), Transformation([4, 1, 1, 4])], 
> rec(acting := true));;
gap> IsZeroSimpleSemigroup(S);
true
gap> C := SemigroupCongruence(S, [S.1, S.1 ^ 2]);
<semigroup congruence over <0-simple transformation semigroup of degree 4 
 with 2 generators> with linked triple (1,2,2)>

# SemigroupCongruenceByGeneratingPairs for free group
gap> SemigroupCongruenceByGeneratingPairs(FreeGroup(1), []);
<2-sided semigroup congruence over <free group on the generators [ f1 ]> with 
0 generating pairs>

# CongruenceByIsomorphism, error
gap> S := Semigroup([Transformation([3, 3, 3]), Transformation([4, 1, 1, 4])],
> rec(acting := true));;
gap> map := IsomorphismSemigroup(IsBooleanMatSemigroup, S);;
gap> C := SemigroupCongruence(S, [S.1, S.2]);
<universal semigroup congruence over <0-simple transformation semigroup of 
 degree 4 with 2 generators>>
gap> CongruenceByIsomorphism(map, C);
Error, the range of the 1st argument (a general mapping) is not equal to the r\
ange of the 2nd argument (a congruence)

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/congruences/congsimple.tst");
