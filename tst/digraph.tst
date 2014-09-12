#############################################################################
##
#W  digraph.tst
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: digraph.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

# conversion to and from Grape graphs
gap> gr:=DirectedGraph( 
> [ [ 8 ], [ 4, 5, 6, 8, 9 ], [ 2, 4, 5, 7, 10 ], [ 9 ], 
> [ 1, 4, 6, 7, 9 ], [ 2, 3, 6, 7, 10 ], [ 3, 4, 5, 8, 9 ], 
> [ 3, 4, 9, 10 ], [ 1, 2, 3, 5, 6, 9, 10 ], [ 2, 4, 5, 6, 9 ] ] );
<directed graph with 10 vertices, 43 edges>
gap> Graph(gr);
rec( adjacencies := [ [ 8 ], [ 4, 5, 6, 8, 9 ], [ 2, 4, 5, 7, 10 ], [ 9 ], 
      [ 1, 4, 6, 7, 9 ], [ 2, 3, 6, 7, 10 ], [ 3, 4, 5, 8, 9 ], 
      [ 3, 4, 9, 10 ], [ 1, 2, 3, 5, 6, 9, 10 ], [ 2, 4, 5, 6, 9 ] ], 
  group := Group(()), isGraph := true, names := [ 1 .. 10 ], order := 10, 
  representatives := [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], 
  schreierVector := [ -1, -2, -3, -4, -5, -6, -7, -8, -9, -10 ] )
gap> Adjacencies(gr);
[ [ 8 ], [ 4, 5, 6, 8, 9 ], [ 2, 4, 5, 7, 10 ], [ 9 ], [ 1, 4, 6, 7, 9 ], 
  [ 2, 3, 6, 7, 10 ], [ 3, 4, 5, 8, 9 ], [ 3, 4, 9, 10 ], 
  [ 1, 2, 3, 5, 6, 9, 10 ], [ 2, 4, 5, 6, 9 ] ]
gap> DirectedGraph(Graph(gr))=gr;
true
gap> Graph(DirectedGraph(Graph(gr)))=Graph(gr);
true
gap> S:=Semigroup( [ Transformation( [ 1, 5, 5, 5, 4 ] ), 
> Transformation( [ 2, 3, 3 ] ), Transformation( [ 3, 5, 3, 2, 4 ] ), 
> Transformation( [ 4, 4, 2, 5, 3 ] ), Transformation( [ 4, 5, 3, 2, 4 ] ) ] );;
gap> D:=DClass(S, Transformation( [ 2, 2, 2 ] ));;
gap> R:=PrincipalFactor(D);
<Rees 0-matrix semigroup 16x4 over Group([ (2,5,4), (2,4) ])>
gap> RZMSGraph(R);
rec( adjacencies := [ [ 17, 19 ], [ 17, 20 ], [ 17, 18 ], [ 17, 20 ], 
      [ 17, 18 ], [ 18, 19 ], [ 18, 20 ], [ 17, 19 ], [ 19, 20 ], [ 17, 20 ], 
      [ 19, 20 ], [ 18, 19 ], [ 19, 20 ], [ 17, 19 ], [ 18, 20 ], [ 18, 20 ], 
      [ 1, 2, 3, 4, 5, 8, 10, 14 ], [ 3, 5, 6, 7, 12, 15, 16 ], 
      [ 1, 6, 8, 9, 11, 12, 13, 14 ], [ 2, 4, 7, 9, 10, 11, 13, 15, 16 ] ], 
  group := Group(()), isGraph := true, names := [ 1 .. 20 ], order := 20, 
  representatives := [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 
      17, 18, 19, 20 ], 
  schreierVector := [ -1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12, -13, 
      -14, -15, -16, -17, -18, -19, -20 ] )
gap> DirectedGraph(last);
<directed graph with 20 vertices, 64 edges>

#
gap> SemigroupsStopTest();

#
gap> STOP_TEST( "Semigroups package: graph-inverse.tst", 0);
