#############################################################################
##
#W  pairs-cong.tst
#Y  Copyright (C) 2014-15                                   Michael Torpey
##                                                          Wilfred Wilson
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
<semigroup congruence over <transformation semigroup of degree 6 with 6 
 generators> with 2 generating pairs>
gap> gens[2] in cong;
true
gap> x := Transformation( [ 6, 5, 4, 4, 4, 6 ] );;
gap> y := Transformation( [ 2, 2, 2, 6, 2, 4 ] );;
gap> z := Transformation( [ 2, 4, 6, 1, 6, 5 ] );;
gap> [x,y] in cong; [x,z] in cong; [y,z] in cong;
true
false
false
gap> [x,y,z] in cong;
Error, Semigroups: in: usage,
the first arg <pair> must be a list of length 2,
gap> [x, Transformation([1])] in cong;
Error, Semigroups: in: usage,
elements of the first arg <pair> must be in rangeof the second
arg <cong>,
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

#T# PairsCongTest2: Checking robustness against infinite semigroups
gap> s := FreeSemigroup(1);
<free semigroup on the generators [ s1 ]>
gap> x := GeneratorsOfSemigroup(s)[1];
s1
gap> gens := [ x ^ 2, x ^ 4 ];
[ s1^2, s1^4 ]
gap> cong := SemigroupCongruence(s, gens);
<semigroup congruence over <free semigroup on the generators [ s1 ]> with 
1 generating pairs>
gap> gens in cong;
Error, Semigroups: in: usage,
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

#T# PairsCongTest3: \= for two semigroup congruences
gap> gens := [ Transformation( [ 2, 6, 7, 2, 6, 9, 9, 1, 1, 5 ] ) ];;
gap> s := Semigroup(Transformation([ 1 ]));;
gap> t := Monoid(gens);;
gap> u := UniversalSemigroupCongruence(s);
<universal semigroup congruence over <trivial transformation group of 
 degree 0 with 1 generator>>
gap> v := SemigroupCongruence(t, [gens[1], gens[1]]);
<semigroup congruence over <commutative transformation monoid of degree 10 
 with 1 generator> with 1 generating pairs>
gap> NrCongruenceClasses(v);
6
gap> Size(t);
6
gap> u = v;
false
gap> u := UniversalSemigroupCongruence(t);
<universal semigroup congruence over <commutative transformation monoid 
 of size 6, degree 10 with 1 generator>>
gap> u = v;
false
gap> gens := List(t, x -> [ gens[1], x ]);;
gap> v := SemigroupCongruence(t, gens);
<semigroup congruence over <commutative transformation monoid of size 6, 
 degree 10 with 1 generator> with 6 generating pairs>
gap> u = v;
true
gap> NrCongruenceClasses(u);
1

#T# PairsCongTest4: \* for two semigroups congruence classes
gap> gens := [ Transformation( [ 2, 6, 7, 2, 6, 9, 9, 1, 1, 5 ] ) ];;
gap> s := Semigroup(gens);;
gap> gens := List(s, x -> [ gens[1], x ]);;
gap> u := SemigroupCongruence(s, gens); # universal congruence
<semigroup congruence over <commutative transformation semigroup of degree 10 
 with 1 generator> with 5 generating pairs>
gap> u = UniversalSemigroupCongruence(s);
true
gap> v := SemigroupCongruence(s, [gens[1], gens[1]]); # trivial congruence
<semigroup congruence over <commutative transformation semigroup of size 5, 
 degree 10 with 1 generator> with 2 generating pairs>
gap> classes := CongruenceClasses(v);
[ {Transformation( [ 1, 2, 2, 1, 2, 6, 6, 9, 9, 1 ] )}, 
  {Transformation( [ 2, 6, 6, 2, 6, 9, 9, 1, 1, 2 ] )}, 
  {Transformation( [ 2, 6, 7, 2, 6, 9, 9, 1, 1, 5 ] )}, 
  {Transformation( [ 6, 9, 9, 6, 9, 1, 1, 2, 2, 6 ] )}, 
  {Transformation( [ 9, 1, 1, 9, 1, 2, 2, 6, 6, 9 ] )} ]
gap> ForAny(CongruenceClasses(u), x -> x in classes);
false
gap> classes[1] * CongruenceClasses(u)[1];
Error, Semigroups: *: usage,
the args must be classes of the same congruence,
gap> CongruenceClasses(u)[1] * classes[1];
Error, Semigroups: *: usage,
the args must be classes of the same congruence,
gap> classes[3] * classes[4];
{Transformation( [ 9, 1, 1, 9, 1, 2, 2, 6, 6, 9 ] )}
gap> classes[4] * classes[3];
{Transformation( [ 9, 1, 1, 9, 1, 2, 2, 6, 6, 9 ] )}
gap> Representative(classes[5] * classes[2]) =
> Representative(classes[5]) * Representative(classes[2]);
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(c);
gap> Unbind(classy);
gap> Unbind(gens);
gap> Unbind(q);
gap> Unbind(P);
gap> Unbind(s);
gap> Unbind(classes);
gap> Unbind(classz);
gap> Unbind(classx);
gap> Unbind(cong);
gap> Unbind(y);
gap> Unbind(x);
gap> Unbind(R);
gap> Unbind(z);
gap> Unbind(t);
gap> Unbind(u);
gap> Unbind(v);

#E#
gap> STOP_TEST( "Semigroups package: simple-cong.tst");
