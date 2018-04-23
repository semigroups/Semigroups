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
gap> congs[i[1]];
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (2,4,2)>
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
gap> congs[i];
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (1,3,2)>
gap> congs[j];
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (1,2,2)>
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
>   Transformation([1, 2, 2, 1, 2])]]);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (2,2,2)>
gap> i := Positions(congs, cong1);;
gap> Length(i) = 1;
true
gap> cong2 := SemigroupCongruence(S, [[Transformation([1, 2, 1, 2, 2]),
>                                      Transformation([1, 2, 1, 2, 1])],
>                                     [Transformation([1, 2, 1, 2, 2]),
>                                      Transformation([1, 2, 2, 1, 1])]]);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (2,2,2)>
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
>   Transformation([1, 2, 2, 1, 2])]]);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (2,2,2)>
gap> x := Transformation([1, 2, 2, 1, 1]);;
gap> y := Transformation([1, 2, 2, 1, 2]);;
gap> z := Transformation([2, 1, 2, 1, 1]);;
gap> [x, y] in cong;
true
gap> [x, z] in cong;
false
gap> [x, y, z] in cong;
Error, Semigroups: \in (for a relation): usage,
the first arg <pair> must be a list of length 2,
gap> [Transformation([2, 1, 1, 2, 1]), Transformation([5, 2, 1, 2, 2])] in cong;
Error, Semigroups: \in (for a relation): usage,
elements of the first arg <pair> must be
in the range of the second arg <cong>,

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
>   Transformation([1, 2, 2, 1, 2])]]);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (2,2,2)>
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

# SimpleCongTest6: Join and meet congruences
gap> S := Semigroup([Transformation([2, 1, 1, 2, 1]),
>                    Transformation([3, 4, 3, 4, 4]),
>                    Transformation([3, 4, 3, 4, 3]),
>                    Transformation([4, 3, 3, 4, 4])]);;
gap> IsSimpleSemigroup(S);
true
gap> congs := [];;
gap> congs[1] := SemigroupCongruence(S, []);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (1,4,2)>
gap> congs[2] := SemigroupCongruence(S, [Transformation([1, 2, 1, 2, 1]),
>                                        Transformation([3, 4, 3, 4, 3])]);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (2,4,1)>
gap> congs[3] := SemigroupCongruence(S, [[Transformation([1, 2, 1, 2, 2]),
>                                         Transformation([1, 2, 1, 2, 1])],
>                                        [Transformation([1, 2, 1, 2, 2]),
>                                         Transformation([1, 2, 2, 1, 1])]]);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (2,2,2)>
gap> congs[4] := SemigroupCongruence(S, [[Transformation([1, 2, 1, 2, 1]),
>                                         Transformation([3, 4, 3, 4, 3])],
>                                        [Transformation([1, 2, 1, 2, 2]),
>                                         Transformation([1, 2, 1, 2, 1])],
>                                        [Transformation([1, 2, 1, 2, 2]),
>                                         Transformation([1, 2, 2, 1, 1])]]);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (2,2,1)>
gap> congs[5] := SemigroupCongruence(S, [Transformation([1, 2, 2, 1, 2]),
>                                        Transformation([1, 2, 1, 2, 2])]);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (2,3,2)>
gap> congs[6] := SemigroupCongruence(S, [[Transformation([1, 2, 1, 2, 1]),
>                                         Transformation([1, 2, 2, 1, 1])],
>                                        [Transformation([1, 2, 1, 2, 1]),
>                                         Transformation([3, 4, 3, 4, 3])],
>                                        [Transformation([1, 2, 2, 1, 2]),
>                                         Transformation([1, 2, 1, 2, 2])]]);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (2,2,1)>
gap> congs[7] := UniversalSemigroupCongruence(S);
<universal semigroup congruence over <simple transformation semigroup of 
 degree 5 with 4 generators>>
gap> congs[8] := SemigroupCongruence(S, [[Transformation([1, 2, 1, 2, 1]),
>                                         Transformation([3, 4, 3, 4, 3])],
>                                        [Transformation([1, 2, 1, 2, 2]),
>                                         Transformation([1, 2, 1, 2, 1])],
>                                        [Transformation([1, 2, 2, 1, 2]),
>                                         Transformation([1, 2, 2, 1, 1])]]);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (2,2,1)>
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
>                                    Transformation([1, 2, 2, 1, 1])]);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 4 generators> with linked triple (2,3,2)>
gap> q := S / cong;;

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
gap> SemigroupCongruence(S, [S.1, S.1 * S.2]);
<universal semigroup congruence over <0-simple inverse partial perm semigroup 
 of rank 2 with 2 generators>>

# Test with a 0-simple semigroup
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

# not simple or 0-simple: try next method
gap> S := OrderEndomorphisms(2);;
gap> IsSimpleSemigroup(S) or IsZeroSimpleSemigroup(S);
false
gap> congs := CongruencesOfSemigroup(S);;
gap> Size(congs);
3

# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(T);
gap> Unbind(classes);
gap> Unbind(classx);
gap> Unbind(classy);
gap> Unbind(classz);
gap> Unbind(cong);
gap> Unbind(cong1);
gap> Unbind(cong2);
gap> Unbind(cong3);
gap> Unbind(cong4);
gap> Unbind(cong5);
gap> Unbind(cong6);
gap> Unbind(cong7);
gap> Unbind(cong8);
gap> Unbind(congs);
gap> Unbind(elms);
gap> Unbind(pairs);
gap> Unbind(q);
gap> Unbind(x);
gap> Unbind(y);
gap> Unbind(z);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/congsimple.tst");
