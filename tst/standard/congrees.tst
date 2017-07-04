###########################################################################
##
#W  standard/congrees.tst
#Y  Copyright (C) 2015                                      Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/congrees.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

#T# ReesCongTest1
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

#T# ReesCongTest2
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
gap> NrCongruenceClasses(cong);
19
gap> cc := Set(CongruenceClasses(cong));;
gap> Size(cc);
19
gap> List(cc, Size);
[ 1095, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
gap> cc[1] * cc[1];
<congruence class of Transformation( [ 3, 1, 1, 4, 4 ] )>
gap> cc[7] * cc[1];
<congruence class of Transformation( [ 3, 1, 1, 4, 4 ] )>
gap> cc[2] * cc[5];
<congruence class of Transformation( [ 2, 4, 1, 3, 1 ] )>
gap> cc[9] * cc[7] = cc[11];
true
gap> Print(cong, "\n");
ReesCongruenceOfSemigroupIdeal( SemigroupIdeal( Semigroup(
    [ Transformation( [ 2, 4, 3, 5, 5 ] ), Transformation( [ 3, 1, 1, 4, 4 ] )\
, Transformation( [ 3, 1, 4, 2, 4 ] ), Transformation( [ 3, 4, 2, 3, 4 ] ), Tr\
ansformation( [ 4, 1, 5, 1, 2 ] ) ] ), 
 [ Transformation( [ 3, 1, 1, 4, 4 ] ), Transformation( [ 1, 4, 1, 4, 1 ] ) ]
   ) )

#T# ReesCongTest3
# Convert a congruence to generating pairs
gap> S := Semigroup([Transformation([1, 3, 2, 4, 3]),
>                      Transformation([1, 3, 5, 5, 3]),
>                      Transformation([5, 1, 2, 5, 5])]);;
gap> I := SemigroupIdeal(S, Transformation([5, 2, 1, 5, 2]),
>                            Transformation([5, 2, 1, 5, 2]));;
gap> cong := ReesCongruenceOfSemigroupIdeal(I);;
gap> ccong := AsSemigroupCongruenceByGeneratingPairs(cong);
<semigroup congruence over <transformation semigroup of size 61, degree 5 
 with 3 generators> with 1 generating pairs>
gap> NrCongruenceClasses(ccong);
12
gap> IsReesCongruence(ccong);
true
gap> SemigroupIdealOfReesCongruence(ccong) = I;
true
gap> EquivalenceRelationPartition(cong) = [AsList(I)];
true

#T# ReesCongTest4
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
Error, Semigroups: \in (for a relation): usage,
the first arg <pair> must be a list of length 2,
gap> [x, z] in cong;
Error, Semigroups: \in (for a relation): usage,
elements of the first arg <pair> must be
in the range of the second arg <cong>,
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
Error, Semigroups: ImagesElm: usage,
the args <cong> and <elm> must refer to the same semigroup,
gap> yclass := CongruenceClassOfElement(cong, y);;
gap> x in yclass;
false
gap> tclass := CongruenceClassOfElement(cong, t);;
gap> y in tclass;
true
gap> CongruenceClassOfElement(cong, z);
Error, Semigroups: EquivalenceClassOfElement: usage,
the second arg <elm> must be in the semigroup of first arg <cong>,
gap> xclass := CongruenceClassOfElement(cong, x);
<congruence class of Transformation( [ 3, 4, 2, 4 ] )>
gap> x in xclass;
true
gap> xclass * yclass = tclass;
true
gap> yclass * xclass = yclass;
true
gap> xxclass := CongruenceClassOfElement(cong, x * x);;
gap> xclass * xclass = xxclass;
true

#T# ReesCongTest5
# Join some congruences together
gap> S := Semigroup([Transformation([1, 1, 3, 1, 3]),
>                      Transformation([2, 1, 2, 2, 2]),
>                      Transformation([3, 1, 3, 2, 4])]);;
gap> I := SemigroupIdeal(S, Transformation([1, 1, 1, 3, 1]));;
gap> J := SemigroupIdeal(S, Transformation([3, 3, 3, 3, 1]));;
gap> ci := ReesCongruenceOfSemigroupIdeal(I);;
gap> cj := ReesCongruenceOfSemigroupIdeal(J);;
gap> class1 := CongruenceClassOfElement(ci, Transformation([1, 1, 3, 1, 3]));;
gap> class2 := CongruenceClassOfElement(cj, Transformation([1, 1, 3, 1, 3]));;
gap> class1 * class2;
Error, Semigroups: \*: usage,
the args <c1> and <c2> must be classes of the same congruence,
gap> cc := JoinSemigroupCongruences(ci, cj);;
gap> NrCongruenceClasses(ci); NrCongruenceClasses(cj); NrCongruenceClasses(cc);
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
Error, Semigroups: JoinSemigroupCongruences: usage,
the args <c1> and <c2> must be congruences of the same semigroup,

#T# Generating pairs
gap> S := Semigroup([Transformation([1, 1, 3, 1, 3]),
>                      Transformation([2, 1, 2, 2, 2]),
>                      Transformation([3, 1, 3, 2, 4])]);;
gap> I := SemigroupIdeal(S, Transformation([1, 1, 1, 3, 1]));;
gap> J := SemigroupIdeal(S, Transformation([3, 3, 3, 3, 1]));;
gap> ci := ReesCongruenceOfSemigroupIdeal(I);;
gap> c := SemigroupCongruence(S, GeneratingPairsOfSemigroupCongruence(ci));
<semigroup congruence over <transformation semigroup of size 20, degree 5 
 with 3 generators> with 1 generating pairs>
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

#T# IsReesCongruence: False
gap> S := Semigroup([Transformation([3, 4, 3, 2]),
>                    Transformation([4, 4, 4, 2])]);;
gap> cong := SemigroupCongruence(S, [Transformation([2, 4, 2, 2]),
>                                    Transformation([4, 2, 4, 4])]);;
gap> IsReesCongruence(cong);
false

#T# IsReesCongruence: One class, but not an ideal
gap> S := Semigroup([Transformation([2, 4, 3, 2]),
>                    Transformation([3, 3, 1, 3]),
>                    Transformation([4, 1, 2, 4]),
>                    Transformation([4, 2, 2, 4])]);;
gap> pairs := [Transformation([4, 4, 4, 4]), Transformation([2, 2, 4, 2])];;
gap> cong := SemigroupCongruence(S, pairs);;
gap> IsReesCongruence(cong);
false

#T# IsSubrelation
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
Error, Semigroups: IsSubrelation: usage,
congruences must be defined over the same semigroup,

#T# EquivalenceRelation(Canonical)Lookup
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

#T# Trivial congruence as Rees
gap> S := SymmetricInverseMonoid(3);;
gap> cong := ReesCongruenceOfSemigroupIdeal(MinimalIdeal(S));;
gap> EquivalenceRelationLookup(cong) = [1 .. 34];
true
gap> EquivalenceRelationCanonicalLookup(cong) = [1 .. 34];
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(I);
gap> Unbind(J);
gap> Unbind(K);
gap> Unbind(S);
gap> Unbind(T);
gap> Unbind(c);
gap> Unbind(c1);
gap> Unbind(c2);
gap> Unbind(c3);
gap> Unbind(c4);
gap> Unbind(cc);
gap> Unbind(ccong);
gap> Unbind(ci);
gap> Unbind(cj);
gap> Unbind(ck);
gap> Unbind(class1);
gap> Unbind(class2);
gap> Unbind(cong);
gap> Unbind(i1);
gap> Unbind(i2);
gap> Unbind(i3);
gap> Unbind(i4);
gap> Unbind(im);
gap> Unbind(pairs);
gap> Unbind(t);
gap> Unbind(tclass);
gap> Unbind(x);
gap> Unbind(xclass);
gap> Unbind(xxclass);
gap> Unbind(y);
gap> Unbind(yclass);
gap> Unbind(z);

#E#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/congrees.tst");
