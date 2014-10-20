#############################################################################
##
#W  graph-inverse.tst
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: graph-inverse.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#
gap> gr:=Digraph(rec(vertices:=[1,2,3,4,5],source:=[1,2,2,4,4], range:=[2,3,5,3,5]));
<digraph with 5 vertices, 5 edges>
gap> S:=GraphInverseSemigroup(gr);
<graph inverse semigroup with 5 vertices, 5 edges>
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
gap> x:=Filtered(Elements(S), x-> not IsZero(x));
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
gap> gr:=Digraph(rec(vertices:=[1],source:=[1], range:=[1]));
<digraph with 1 vertex, 1 edge>
gap> S:=GraphInverseSemigroup(gr);
<graph inverse semigroup with 1 vertices, 1 edges>
gap> e_1^-1;
e_1^-1
gap> v_1^-1;
v_1
gap> AssignGeneratorVariables(S);
gap> e_1^-1;
e_1^-1
gap> e_1=e_2;
false
gap> e_1^-1*e_1=v_1;
true
gap> e_1*e_1^-1=v_1;
false
gap> 

#
gap> SemigroupsStopTest();

#
gap> STOP_TEST( "Semigroups package: graph-inverse.tst", 0);
