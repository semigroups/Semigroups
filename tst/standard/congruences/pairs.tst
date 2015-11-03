#############################################################################
##
#W  standard/congruences/pairs.tst
#Y  Copyright (C) 2014-15                                   Michael Torpey
##                                                          Wilfred Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/congruences/pairs.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

#T# PairsCongTest2: Checking robustness against infinite semigroups
gap> S := FreeSemigroup(1);
<free semigroup on the generators [ s1 ]>
gap> x := GeneratorsOfSemigroup(S)[1];
s1
gap> gens := [x ^ 2, x ^ 4];
[ s1^2, s1^4 ]
gap> cong := SemigroupCongruence(S, gens);
<semigroup congruence over <free semigroup on the generators [ s1 ]> with 
1 generating pairs>
gap> NonTrivialCongruenceClasses(cong);
Error, Semigroups: NonTrivialEquivalenceClasses: usage,
this function currently only works if <cong> is a congruence of a semigroup
which is known to be finite,
gap> gens in cong;
Error, Semigroups: \in (for a congruence): usage,
this function currently only works if <cong> is a congruence of a semigroup
which is known to be finite,
gap> AsLookupTable(cong);
Error, Semigroups: AsLookupTable: usage,
<cong> must be a congruence of a finite semigroup,
gap> EquivalenceClasses(cong);
Error, Semigroups: EquivalenceClasses: usage,
this function currently only works if <cong> is a congruence of a semigroup
which is known to be finite,
gap> NrCongruenceClasses(cong);
Error, Semigroups: NrCongruenceClasses: usage,
this function currently only works if <cong> is a congruence of a semigroup
which is known to be finite,
gap> class := CongruenceClassOfElement(cong, x);;

#T# PairsCongTest3: \= for two semigroup congruences
gap> gens := [Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5])];;
gap> S := Semigroup(Transformation([1]));;
gap> T := Monoid(gens);;
gap> u := UniversalSemigroupCongruence(S);
<universal semigroup congruence over <trivial transformation group of 
 degree 0 with 0 generators>>
gap> v := SemigroupCongruence(T, [gens[1], gens[1]]);
<semigroup congruence over <commutative transformation monoid of degree 10 
 with 1 generator> with 0 generating pairs>
gap> NrCongruenceClasses(v);
6
gap> Size(T);
6
gap> u = v;
false
gap> u := UniversalSemigroupCongruence(T);
<universal semigroup congruence over <commutative transformation monoid 
 of size 6, degree 10 with 1 generator>>
gap> u = v;
false
gap> gens := List(T, x -> [gens[1], x]);;
gap> v := SemigroupCongruence(T, gens);
<semigroup congruence over <commutative transformation monoid of size 6, 
 degree 10 with 1 generator> with 5 generating pairs>
gap> u = v;
true
gap> NrCongruenceClasses(u);
1

#T# PairsCongTest4: \* for two semigroups congruence classes
gap> gens := [Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5])];;
gap> S := Semigroup(gens);;
gap> gens := List(S, x -> [gens[1], x]);;
gap> u := SemigroupCongruence(S, gens); # universal congruence
<semigroup congruence over <commutative transformation semigroup of degree 10 
 with 1 generator> with 4 generating pairs>
gap> u = UniversalSemigroupCongruence(S);
true
gap> v := SemigroupCongruence(S, [gens[1], gens[1]]); # trivial congruence
<semigroup congruence over <commutative transformation semigroup of size 5, 
 degree 10 with 1 generator> with 0 generating pairs>
gap> classes := Set(CongruenceClasses(v));
[ <congruence class of Transformation( [ 1, 2, 2, 1, 2, 6, 6, 9, 9, 1 ] )>, 
  <congruence class of Transformation( [ 2, 6, 6, 2, 6, 9, 9, 1, 1, 2 ] )>, 
  <congruence class of Transformation( [ 2, 6, 7, 2, 6, 9, 9, 1, 1, 5 ] )>, 
  <congruence class of Transformation( [ 6, 9, 9, 6, 9, 1, 1, 2, 2, 6 ] )>, 
  <congruence class of Transformation( [ 9, 1, 1, 9, 1, 2, 2, 6, 6, 9 ] )> ]
gap> ForAny(CongruenceClasses(u), x -> x in classes);
false
gap> classes[1] * CongruenceClasses(u)[1];
Error, Semigroups: *: usage,
the args must be classes of the same congruence,
gap> CongruenceClasses(u)[1] * classes[1];
Error, Semigroups: *: usage,
the args must be classes of the same congruence,
gap> classes[3] * classes[4];
<congruence class of Transformation( [ 9, 1, 1, 9, 1, 2, 2, 6, 6, 9 ] )>
gap> classes[4] * classes[3];
<congruence class of Transformation( [ 9, 1, 1, 9, 1, 2, 2, 6, 6, 9 ] )>
gap> Representative(classes[5] * classes[2]) =
> Representative(classes[5]) * Representative(classes[2]);
true

#T# A semigroup congruence example
gap> S := Semigroup([Transformation([2, 1, 1, 2, 1]),
>                    Transformation([3, 4, 3, 4, 3]),
>                    Transformation([4, 3, 3, 4, 4]),
>                    Transformation([1, 3, 4, 1, 3])]);;
gap> pair1 := [Transformation([3, 4, 3, 4, 3]),
>              Transformation([1, 2, 1, 2, 1])];;
gap> pair2 := [Transformation([4, 3, 4, 3, 4]),
>              Transformation([3, 4, 3, 4, 3])];;
gap> cong := SemigroupCongruence(S, pair1, pair2);
<semigroup congruence over <transformation semigroup of degree 5 with 4 
 generators> with 2 generating pairs>
gap> HasAsLookupTable(cong);
false
gap> [Transformation([4, 4, 3, 4, 4]), Transformation([3, 3, 1, 3, 3])] in cong;
true
gap> [Transformation([4, 4, 3, 4, 4]), Transformation([3, 3, 1, 3, 3])] in cong;
true
gap> HasAsLookupTable(cong);
false
gap> [Transformation([3, 4, 3, 3, 4]), Transformation([1, 3, 4, 1, 3])] in cong;
false
gap> [Transformation([3, 4, 3, 3, 4]), Transformation([1, 3, 4, 1, 3])] in cong;
false
gap> HasAsLookupTable(cong);
true
gap> AsLookupTable(cong);
[ 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
gap> NonTrivialCongruenceClasses(cong);
[ <congruence class of Transformation( [ 2, 1, 1, 2, 1 ] )> ]
gap> SEMIGROUPS_Enumerate(cong, ReturnFalse);
fail

#T# A left semigroup congruence example
gap> S := Semigroup(Transformation([2, 1, 1, 2, 1]),
>                   Transformation([3, 4, 3, 4, 4]),
>                   Transformation([3, 4, 3, 4, 3]),
>                   Transformation([4, 3, 3, 4, 4]));;
gap> pair1 := [Transformation([3, 4, 3, 4, 3]),
>              Transformation([1, 2, 1, 2, 1])];;
gap> pair2 := [Transformation([4, 3, 4, 3, 4]),
>              Transformation([3, 4, 3, 4, 3])];;
gap> cong := LeftSemigroupCongruence(S, pair1, pair2);
<left semigroup congruence over <transformation semigroup of degree 5 with 4 
 generators> with 2 generating pairs>
gap> [Transformation([3, 4, 4, 3, 3]), Transformation([1, 2, 2, 1, 1])] in cong;
true
gap> [Transformation([3, 4, 4, 3, 3]), Transformation([1, 2, 2, 1, 1])] in cong;
true
gap> [Transformation([1, 2, 1, 2, 2]), Transformation([1, 2, 2, 1, 2])] in cong;
false
gap> HasAsLookupTable(cong);
true
gap> AsLookupTable(cong);
[ 1, 2, 3, 4, 1, 1, 1, 2, 3, 4, 2, 2, 3, 3, 4, 4 ]
gap> NonTrivialCongruenceClasses(cong);
[ <congruence class of Transformation( [ 2, 1, 1, 2, 1 ] )>, 
  <congruence class of Transformation( [ 3, 4, 3, 4, 4 ] )>, 
  <congruence class of Transformation( [ 3, 4, 3, 4, 3 ] )>, 
  <congruence class of Transformation( [ 4, 3, 3, 4, 4 ] )> ]
gap> IsRightSemigroupCongruence(cong);
true
gap> SEMIGROUPS_Enumerate(cong, ReturnTrue);
fail

#T# A right semigroup congruence example
gap> S := Semigroup(Transformation([2, 1, 1, 2, 1]),
>                   Transformation([3, 4, 3, 4, 4]),
>                   Transformation([3, 4, 3, 4, 3]),
>                   Transformation([4, 3, 3, 4, 4]));;
gap> pair1 := [Transformation([3, 4, 3, 4, 3]),
>              Transformation([1, 2, 1, 2, 1])];;
gap> pair2 := [Transformation([4, 3, 4, 3, 4]),
>              Transformation([3, 4, 3, 4, 3])];;
gap> cong := RightSemigroupCongruence(S, pair1, pair2);
<right semigroup congruence over <transformation semigroup of degree 5 with 4 
 generators> with 2 generating pairs>
gap> [Transformation([3, 4, 3, 4, 3]), Transformation([1, 2, 1, 2, 1])] in cong;
true
gap> [Transformation([3, 4, 3, 4, 3]), Transformation([1, 2, 1, 2, 1])] in cong;
true
gap> [Transformation([3, 4, 4, 3, 3]), Transformation([1, 2, 2, 1, 1])] in cong;
false
gap> [Transformation([3, 4, 4, 3, 3]), Transformation([1, 2, 2, 1, 1])] in cong;
false
gap> HasAsLookupTable(cong);
true
gap> AsLookupTable(cong);
[ 1, 2, 3, 4, 5, 6, 7, 8, 3, 9, 10, 11, 3, 3, 12, 13 ]
gap> NonTrivialCongruenceClasses(cong);
[ <congruence class of Transformation( [ 3, 4, 3, 4, 3 ] )> ]
gap> IsLeftSemigroupCongruence(cong);
false
gap> SEMIGROUPS_Enumerate(cong, ReturnFail);
fail

#T# \in: Bad input
gap> S := Semigroup(Transformation([2, 1, 1, 2, 1]),
>                   Transformation([2, 4, 2, 3, 5]),
>                   Transformation([3, 4, 3, 4, 3]),
>                   Transformation([4, 3, 3, 4, 4]));;
gap> pair1 := [Transformation([3, 4, 3, 4, 3]),
>              Transformation([1, 2, 1, 2, 1])];;
gap> cong := SemigroupCongruence(S, pair1);;
gap> [Transformation([2,1,1,2,1])] in cong;
Error, Semigroups: \in (for a congruence): usage,
the first arg <pair> must be a list of length 2,
gap> [Transformation([2,1,1,2,1]), Transformation([5,2,1,2,2])] in cong;
Error, Semigroups: \in (for a congruence): usage,
elements of the first arg <pair> must be
in the range of the second arg <cong>,

#T# Classes
gap> S := Semigroup([
> Transformation([1, 5, 4, 2, 1]), Transformation([2, 1, 1, 1, 3])]);;
gap> pair := [Transformation([2, 2, 2, 3, 2]), Transformation([2, 2, 1, 1, 2])];
[ Transformation( [ 2, 2, 2, 3, 2 ] ), Transformation( [ 2, 2, 1, 1, 2 ] ) ]
gap> cong := SemigroupCongruence(S, pair);;
gap> class := CongruenceClassOfElement(cong, Transformation([1, 2, 2, 2, 1]));;
gap> enum := Enumerator(class);;
gap> Transformation([1, 2, 2, 2, 1]) in enum;
true
gap> Transformation([2, 2, 3, 2, 2]) in enum;
true
gap> Transformation([1, 5, 4, 2, 1]) in enum;
false
gap> enum[3];
Transformation( [ 1, 1, 5, 1, 1 ] )
gap> last in class;
true
gap> enum[25];
Transformation( [ 2, 2, 2, 2, 3 ] )
gap> Transformation([1, 5, 4, 2, 1]) in class;
false
gap> Transformation([1, 2, 2, 2, 1]) in class;
true
gap> Transformation([2, 2, 3, 2, 2]) in class;
true
gap> cong := SemigroupCongruence(S, pair);;
gap> class := CongruenceClassOfElement(cong, Transformation([1, 2, 2, 2, 1]));;
gap> Transformation([1, 1, 5, 1, 1]) in class;
true
gap> Transformation( [ 6, 2, 3, 4, 1, 1 ] ) in class;
Error, Semigroups: \in (for a congruence): usage,
elements of the first arg <pair> must be
in the range of the second arg <cong>,
gap> Size(class);
89

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(x);
gap> Unbind(gens);
gap> Unbind(cong);
gap> Unbind(class);
gap> Unbind(T);
gap> Unbind(u);
gap> Unbind(v);
gap> Unbind(classes);
gap> Unbind(pair1);
gap> Unbind(pair2);
gap> Unbind(pair);
gap> Unbind(enum);

#E#
gap> STOP_TEST("Semigroups package: standard/congruences/pairs.tst");
