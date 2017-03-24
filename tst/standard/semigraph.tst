#############################################################################
##
#W  standard/semigraph.tst
#Y  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semigraph.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#T# semigraph: test 1
gap> gr := Digraph(rec(nrvertices := 5,
> source := [1, 2, 2, 4, 4], range := [2, 3, 5, 3, 5]));
<digraph with 5 vertices, 5 edges>
gap> S := GraphInverseSemigroup(gr);
<finite graph inverse semigroup with 5 vertices, 5 edges>
gap> Size(S);
39
gap> Elements(S);
[ e_5^-1, e_4^-1, e_3^-1, e_3^-1e_1^-1, e_2^-1, e_2^-1e_1^-1, e_1^-1, 0, e_1, 
  e_1e_1^-1, e_1e_2, e_1e_2e_4^-1, e_1e_2e_2^-1, e_1e_2e_2^-1e_1^-1, e_1e_3, 
  e_1e_3e_5^-1, e_1e_3e_3^-1, e_1e_3e_3^-1e_1^-1, e_2, e_2e_4^-1, e_2e_2^-1, 
  e_2e_2^-1e_1^-1, e_3, e_3e_5^-1, e_3e_3^-1, e_3e_3^-1e_1^-1, e_4, 
  e_4e_4^-1, e_4e_2^-1, e_4e_2^-1e_1^-1, e_5, e_5e_5^-1, e_5e_3^-1, 
  e_5e_3^-1e_1^-1, v_1, v_2, v_3, v_4, v_5 ]
gap> IsVertex(last[1]);
false
gap> IsVertex(last2[Length(last2)]);
true
gap> AssignGeneratorVariables(S);
gap> Zero(e_1);
0
gap> MultiplicativeZero(S);
0
gap> IsZero(e_1);
false
gap> Number(Elements(S), IsZero);
1
gap> x := Filtered(Elements(S), x -> not IsZero(x));
[ e_5^-1, e_4^-1, e_3^-1, e_3^-1e_1^-1, e_2^-1, e_2^-1e_1^-1, e_1^-1, e_1, 
  e_1e_1^-1, e_1e_2, e_1e_2e_4^-1, e_1e_2e_2^-1, e_1e_2e_2^-1e_1^-1, e_1e_3, 
  e_1e_3e_5^-1, e_1e_3e_3^-1, e_1e_3e_3^-1e_1^-1, e_2, e_2e_4^-1, e_2e_2^-1, 
  e_2e_2^-1e_1^-1, e_3, e_3e_5^-1, e_3e_3^-1, e_3e_3^-1e_1^-1, e_4, 
  e_4e_4^-1, e_4e_2^-1, e_4e_2^-1e_1^-1, e_5, e_5e_5^-1, e_5e_3^-1, 
  e_5e_3^-1e_1^-1, v_1, v_2, v_3, v_4, v_5 ]
gap> List(x, Source);
[ v_5, v_3, v_5, v_5, v_3, v_3, v_2, v_1, v_1, v_1, v_1, v_1, v_1, v_1, v_1, 
  v_1, v_1, v_2, v_2, v_2, v_2, v_2, v_2, v_2, v_2, v_4, v_4, v_4, v_4, v_4, 
  v_4, v_4, v_4, v_1, v_2, v_3, v_4, v_5 ]
gap> List(x, Range);
[ v_4, v_4, v_2, v_1, v_2, v_1, v_1, v_2, v_1, v_3, v_4, v_2, v_1, v_5, v_4, 
  v_2, v_1, v_3, v_4, v_2, v_1, v_5, v_4, v_2, v_1, v_3, v_4, v_2, v_1, v_5, 
  v_4, v_2, v_1, v_1, v_2, v_3, v_4, v_5 ]
gap> AssignGeneratorVariables(S);
gap> String(gr);
"Digraph( [ [ 2 ], [ 3, 5 ], [ ], [ 3, 5 ], [ ] ] )"
gap> String(S);
"GraphInverseSemigroup( Digraph( [ [ 2 ], [ 3, 5 ], [ ], [ 3, 5 ], [ ] ] ) )"
gap> EvalString(String(S)) = S;
false
gap> gr := Digraph([[1]]);
<digraph with 1 vertex, 1 edge>
gap> S := GraphInverseSemigroup(gr);
<infinite graph inverse semigroup with 1 vertex, 1 edge>
gap> e_1 ^ -1;
e_1^-1
gap> v_1 ^ -1;
v_1
gap> AssignGeneratorVariables(S);
gap> e_1 ^ -1;
e_1^-1
gap> e_1 = e_2;
false
gap> e_1 ^ -1 * e_1 = v_1;
true
gap> e_1 * e_1 ^ -1 = v_1;
false

# Test Source/Range
gap> gr := Digraph(rec(nrvertices := 5,
> source := [1, 2, 2, 4, 4], range := [2, 3, 5, 3, 5]));;
gap> S := GraphInverseSemigroup(gr);
<finite graph inverse semigroup with 5 vertices, 5 edges>
gap> s := MultiplicativeZero(S);
0
gap> Source(s);
Error, Semigroups: Source: usage,
the argument <x> must not be the zero,
gap> Range(s);
Error, Semigroups: Range: usage,
the argument <x> must not be the zero,

# Test \*
gap> gr := Digraph(rec(nrvertices := 5,
> source := [1, 2, 3], range := [2, 3, 4]));;
gap> S := GraphInverseSemigroup(gr);
<finite graph inverse semigroup with 5 vertices, 3 edges>
gap> AssignGeneratorVariables(S);
gap> y := e_1 * e_2 * e_3;
e_1e_2e_3
gap> x := (e_1 * e_2) ^ -1;
e_2^-1e_1^-1
gap> x * y;
e_3

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(gr);
gap> Unbind(x);

#E#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/semigraph.tst");
