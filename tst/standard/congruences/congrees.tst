###########################################################################
##
#W  standard/congruences/congrees.tst
#Y  Copyright (C) 2015-2022                                 Michael Young
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local C, I, J, K, S, T, c, c1, c2, c3, c4, cc, ccong, ci, cj, ck, class1
#@local class2, cong, i1, i2, i3, i4, im, pairs, t, tclass, x, xclass, xxclass
#@local y, yclass, z
gap> START_TEST("Semigroups package: standard/congruences/congrees.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# ReesCongTest1
# Test whether a congruence is Rees and find its ideal
gap> S := Semigroup([Transformation([2, 3, 4, 3, 1, 1]),
>                    Transformation([6, 4, 4, 4, 6, 1])]);;
gap> I := SemigroupIdeal(S, Transformation([4, 4, 4, 4, 4, 2]),
>                           Transformation([3, 3, 3, 3, 3, 2]));;
gap> cong := SemigroupCongruence(S,
>  [[Transformation([4, 4, 4, 4, 4, 2]), Transformation([4, 4, 4, 4, 4, 4])],
>   [Transformation([3, 3, 3, 3, 3, 2]), Transformation([4, 4, 4, 4, 4, 4])],
>   [Transformation([4, 3, 3, 3, 4, 3]), Transformation([4, 4, 4, 4, 4, 4])],
>   [Transformation([3, 3, 3, 3, 4, 4]), Transformation([4, 4, 4, 4, 4, 4])],
>   [Transformation([4, 4, 4, 4, 4, 3]), Transformation([3, 3, 3, 3, 3, 3])]]);;
gap> IsReesCongruence(cong);
true
gap> SemigroupIdealOfReesCongruence(cong) = I;
true
gap> cong := SemigroupCongruence(S, []);;
gap> IsReesCongruence(cong);
false
gap> S := Semigroup([PartialPerm([1, 2, 3], [1, 2, 3]),
>                    PartialPerm([1, 2, 3, 4], [2, 4, 3, 5])]);;
gap> cong := SemigroupCongruence(S, []);;
gap> IsReesCongruence(cong);
true
gap> cong := SemigroupCongruence(S, [PartialPerm([1, 2, 3], [2, 4, 3]),
>                                    PartialPerm([1, 2, 3, 4], [2, 4, 3, 5])]);;
gap> IsReesCongruence(cong);
false

# ReesCongTest2
# Create a congruence, calculate its congruence classes and try some operations
gap> S := Semigroup([Transformation([2, 4, 3, 5, 5]),
>                    Transformation([3, 1, 1, 4, 4]),
>                    Transformation([3, 1, 4, 2, 4]),
>                    Transformation([3, 4, 2, 3, 4]),
>                    Transformation([4, 1, 5, 1, 2])]);
<transformation semigroup of degree 5 with 5 generators>
gap> I := SemigroupIdeal(S, [Transformation([3, 1, 1, 4, 4]),
>                            Transformation([1, 4, 1, 4, 1])]);;
gap> cong := ReesCongruenceOfSemigroupIdeal(I);;
gap> NrEquivalenceClasses(cong);
19
gap> cc := Set(EquivalenceClasses(cong));;
gap> Size(cc);
19
gap> List(cc, Size);
[ 1095, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
gap> cc[1] * cc[1];
<2-sided congruence class of Transformation( [ 3, 1, 1, 4, 4 ] )>
gap> cc[7] * cc[1];
<2-sided congruence class of Transformation( [ 3, 1, 1, 4, 4 ] )>
gap> cc[2] * cc[5];
<2-sided congruence class of Transformation( [ 2, 4, 1, 3, 1 ] )>
gap> cc[9] * cc[7] = cc[11];
true
gap> Print(cong, "\n");
ReesCongruenceOfSemigroupIdeal( SemigroupIdeal( Semigroup(
    [ Transformation( [ 2, 4, 3, 5, 5 ] ), Transformation( [ 3, 1, 1, 4, 4 ] )\
, Transformation( [ 3, 1, 4, 2, 4 ] ), Transformation( [ 3, 4, 2, 3, 4 ] ), Tr\
ansformation( [ 4, 1, 5, 1, 2 ] ) ] ), 
 [ Transformation( [ 3, 1, 1, 4, 4 ] ), Transformation( [ 1, 4, 1, 4, 1 ] ) ]
   ) )

# ReesCongTest3
# Convert a congruence to generating pairs
gap> S := Semigroup([Transformation([1, 3, 2, 4, 3]),
>                      Transformation([1, 3, 5, 5, 3]),
>                      Transformation([5, 1, 2, 5, 5])]);;
gap> I := SemigroupIdeal(S, Transformation([5, 2, 1, 5, 2]),
>                            Transformation([5, 2, 1, 5, 2]));;
gap> cong := ReesCongruenceOfSemigroupIdeal(I);;
gap> ccong := AsSemigroupCongruenceByGeneratingPairs(cong);;
gap> NrEquivalenceClasses(ccong);
12
gap> IsReesCongruence(ccong);
true
gap> SemigroupIdealOfReesCongruence(ccong) = I;
true
gap> EquivalenceRelationPartition(cong) = [AsList(I)];
true

# ReesCongTest4
# Test the \in function
gap> S := Semigroup([Transformation([2, 4, 3, 5, 5]),
>                    Transformation([3, 1, 1, 4, 4]),
>                    Transformation([3, 1, 4, 2, 4]),
>                    Transformation([3, 4, 2, 3, 4]),
>                    Transformation([4, 1, 5, 1, 2])]);;
gap> I := SemigroupIdeal(S, [Transformation([3, 1, 1, 4, 4]),
>                            Transformation([1, 4, 1, 4, 1])]);;
gap> cong := ReesCongruenceOfSemigroupIdeal(I);;
gap> x := Transformation([3, 4, 2, 4]);;      # not in I
gap> y := Transformation([1, 5, 5, 5, 4]);;   # in I
gap> z := Transformation([5, 5, 1, 1, 3]);;   # not even in S
gap> [x, y] in cong;
false
gap> [x, x] in cong;
true
gap> [x, y, y] in cong;
Error, the 1st argument (a list) does not have length 2
gap> [x, z] in cong;
Error, the items in the 1st argument (a list) do not all belong to the range o\
f the 2nd argument (a 2-sided semigroup congruence)
gap> t := Transformation([1, 3, 4, 1, 4]);;   # in i
gap> [t, y] in cong;
true
gap> [x, x] in cong;
true
gap> im := ImagesElm(cong, t);;
gap> Size(im) = Size(I);
true
gap> ForAll(im, x -> x in I);
true
gap> im := ImagesElm(cong, x);;
gap> Size(im);
1
gap> ImagesElm(cong, z);
Error, the 2nd argument (a mult. elt.) does not belong to the range of the 1st\
 argument (a Rees congruence)
gap> yclass := EquivalenceClassOfElement(cong, y);;
gap> x in yclass;
false
gap> tclass := EquivalenceClassOfElement(cong, t);;
gap> y in tclass;
true
gap> EquivalenceClassOfElement(cong, z);
Error, the 2nd argument (a mult. elt.) does not belong to the range of the 1st\
 argument (a 2-sided congruence)
gap> xclass := EquivalenceClassOfElement(cong, x);
<2-sided congruence class of Transformation( [ 3, 4, 2, 4 ] )>
gap> x in xclass;
true
gap> xclass * yclass = tclass;
true
gap> yclass * xclass = yclass;
true
gap> xxclass := EquivalenceClassOfElement(cong, x * x);;
gap> xclass * xclass = xxclass;
true

# ReesCongTest5
# Join some congruences together
gap> S := Semigroup([Transformation([1, 1, 3, 1, 3]),
>                      Transformation([2, 1, 2, 2, 2]),
>                      Transformation([3, 1, 3, 2, 4])]);;
gap> I := SemigroupIdeal(S, Transformation([1, 1, 1, 3, 1]));;
gap> J := SemigroupIdeal(S, Transformation([3, 3, 3, 3, 1]));;
gap> ci := ReesCongruenceOfSemigroupIdeal(I);;
gap> cj := ReesCongruenceOfSemigroupIdeal(J);;
gap> class1 := EquivalenceClassOfElement(ci, Transformation([1, 1, 3, 1, 3]));;
gap> class2 := EquivalenceClassOfElement(cj, Transformation([1, 1, 3, 1, 3]));;
gap> class1 * class2;
Error, the arguments (cong. classes) are not classes of the same congruence
gap> cc := JoinSemigroupCongruences(ci, cj);;
gap> NrEquivalenceClasses(ci); NrEquivalenceClasses(cj); NrEquivalenceClasses(cc);
16
17
15
gap> EquivalenceRelationPartition(cc) =
>    [AsList(SemigroupIdeal(S, Transformation([1, 1, 1, 3, 1]),
>                              Transformation([3, 3, 3, 3, 1])))];
true
gap> K := SemigroupIdeal(FullTransformationMonoid(5),
>                         Transformation([3, 2, 5, 4, 2]));;
gap> ck := ReesCongruenceOfSemigroupIdeal(K);
<Rees congruence of <regular transformation semigroup ideal of degree 5 with
  1 generator> over <full transformation monoid of degree 5>>
gap> JoinSemigroupCongruences(ci, ck);
Error, cannot form the join of congruences over different semigroups

# Generating pairs
gap> S := Semigroup([Transformation([1, 1, 3, 1, 3]),
>                      Transformation([2, 1, 2, 2, 2]),
>                      Transformation([3, 1, 3, 2, 4])]);;
gap> I := SemigroupIdeal(S, Transformation([1, 1, 1, 3, 1]));;
gap> J := SemigroupIdeal(S, Transformation([3, 3, 3, 3, 1]));;
gap> ci := ReesCongruenceOfSemigroupIdeal(I);;
gap> c := SemigroupCongruence(S, GeneratingPairsOfSemigroupCongruence(ci));;
gap> HasIsReesCongruence(c);
false
gap> IsReesCongruence(c);
true
gap> c = ci;
true
gap> cj := ReesCongruenceOfSemigroupIdeal(J);;
gap> ci = cj;
false
gap> EquivalenceRelationPartition(ci) = [AsList(I)];
true
gap> EquivalenceRelationPartition(cj) = [AsList(J)];
true

# IsReesCongruence: False
gap> S := Semigroup([Transformation([3, 4, 3, 2]),
>                    Transformation([4, 4, 4, 2])]);;
gap> cong := SemigroupCongruence(S, [Transformation([2, 4, 2, 2]),
>                                    Transformation([4, 2, 4, 4])]);;
gap> IsReesCongruence(cong);
false

# IsReesCongruence: One class, but not an ideal
gap> S := Semigroup([Transformation([2, 4, 3, 2]),
>                    Transformation([3, 3, 1, 3]),
>                    Transformation([4, 1, 2, 4]),
>                    Transformation([4, 2, 2, 4])]);;
gap> pairs := [Transformation([4, 4, 4, 4]), Transformation([2, 2, 4, 2])];;
gap> cong := SemigroupCongruence(S, pairs);;
gap> IsReesCongruence(cong);
false

# IsSubrelation
gap> S := Semigroup([Transformation([2, 4, 1, 2]),
>                    Transformation([3, 3, 4, 1])]);;
gap> i1 := SemigroupIdeal(S, [Transformation([2, 4, 1, 2])]);;
gap> i2 := SemigroupIdeal(S, [Transformation([4, 2, 2, 4])]);;
gap> i3 := SemigroupIdeal(S, [Transformation([3, 3, 4, 1])]);;
gap> c1 := ReesCongruenceOfSemigroupIdeal(i1);;
gap> c2 := ReesCongruenceOfSemigroupIdeal(i2);;
gap> c3 := ReesCongruenceOfSemigroupIdeal(i3);;
gap> IsSubrelation(c3, c2);
true
gap> IsSubrelation(c1, c2);
true
gap> IsSubrelation(c2, c1);
false
gap> IsSubrelation(c3, c3);
true
gap> IsSubrelation(c3, c1);
false
gap> IsSubrelation(c1, c3);
false
gap> T := Semigroup([Transformation([2, 4, 1, 2])]);;
gap> i4 := SemigroupIdeal(T, [Transformation([2, 4, 1, 2])]);;
gap> c4 := ReesCongruenceOfSemigroupIdeal(i4);;
gap> IsSubrelation(c4, c1);
Error, the 1st and 2nd arguments are congruences over different semigroups

# EquivalenceRelation(Canonical)Lookup
gap> S := FullTransformationMonoid(3);;
gap> I := SemigroupIdeal(S, Transformation([1, 1, 2]));;
gap> cong := ReesCongruenceOfSemigroupIdeal(I);
<Rees congruence of <regular transformation semigroup ideal of degree 3 with
  1 generator> over <full transformation monoid of degree 3>>
gap> EquivalenceRelationLookup(cong);
[ 1, 2, 3, 11, 5, 6, 11, 8, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 
  11, 11, 11, 11, 11, 11, 11 ]
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 2, 3, 4, 5, 6, 4, 7, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
  4, 4 ]

# Trivial congruence as Rees
gap> S := SymmetricInverseMonoid(3);;
gap> cong := ReesCongruenceOfSemigroupIdeal(MinimalIdeal(S));;
gap> EquivalenceRelationLookup(cong) = [1 .. 34];
true
gap> EquivalenceRelationCanonicalLookup(cong) = [1 .. 34];
true

# IsReesCongruence for a right but not left congruence
gap> S := InverseSemigroup([PartialPerm([1, 2], [1, 2]),
>                           PartialPerm([1, 2], [2, 3])]);;
gap> pairs := [PartialPerm([], []), PartialPerm([1], [1])];;
gap> C := RightSemigroupCongruence(S, pairs);;
gap> IsReesCongruence(C);
false

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/congruences/congrees.tst");
