#############################################################################
##
#W  standard/semigroups/semigraph.tst
#Y  Copyright (C) 2014-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local D, DigraphNrVertices, DigraphRange, DigraphSource, S, gr, s, x, y
gap> START_TEST("Semigroups package: standard/semigroups/semigraph.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# semigraph: test 1
gap> gr := Digraph(rec(DigraphNrVertices := 5,
> DigraphSource := [1, 2, 2, 4, 4], DigraphRange := [2, 3, 5, 3, 5]));
<immutable digraph with 5 vertices, 5 edges>
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
gap> IsVertex(Last(last2));
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
"DigraphFromDigraph6String(\"&DOS@O?\")"
gap> String(S);
"GraphInverseSemigroup( DigraphFromDigraph6String(\"&DOS@O?\") )"
gap> EvalString(String(S)) = S;
false
gap> gr := Digraph([[1]]);
<immutable digraph with 1 vertex, 1 edge>
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
gap> gr := Digraph(rec(DigraphNrVertices := 5,
> DigraphSource := [1, 2, 2, 4, 4], DigraphRange := [2, 3, 5, 3, 5]));;
gap> S := GraphInverseSemigroup(gr);
<finite graph inverse semigroup with 5 vertices, 5 edges>
gap> s := MultiplicativeZero(S);
0
gap> Source(s);
Error, the argument (a graph inverse semigroup element) must not be the zero e\
lement
gap> Range(s);
Error, the argument (a graph inverse semigroup element) must not be the zero e\
lement

# Test \*
gap> gr := Digraph(rec(DigraphNrVertices := 5,
> DigraphSource := [1, 2, 3], DigraphRange := [2, 3, 4]));;
gap> S := GraphInverseSemigroup(gr);
<finite graph inverse semigroup with 5 vertices, 3 edges>
gap> AssignGeneratorVariables(S);
gap> y := e_1 * e_2 * e_3;
e_1e_2e_3
gap> x := (e_1 * e_2) ^ -1;
e_2^-1e_1^-1
gap> x * y;
e_3

# VerticesOfGraphInverseSemigroup and IndexOfVertexOfGraphInverseSemigroup
gap> D := Digraph([[3, 4], [3, 4], [4], []]);
<immutable digraph with 4 vertices, 5 edges>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 4 vertices, 5 edges>
gap> VerticesOfGraphInverseSemigroup(S);
[ v_1, v_2, v_3, v_4 ]
gap> IndexOfVertexOfGraphInverseSemigroup(v_1);
1
gap> IndexOfVertexOfGraphInverseSemigroup(v_3);
3
gap> D := ChainDigraph(12);
<immutable chain digraph with 12 vertices>
gap> S := GraphInverseSemigroup(D);
<finite graph inverse semigroup with 12 vertices, 11 edges>
gap> VerticesOfGraphInverseSemigroup(S);
[ v_1, v_2, v_3, v_4, v_5, v_6, v_7, v_8, v_9, v_10, v_11, v_12 ]

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/semigroups/semigraph.tst");
