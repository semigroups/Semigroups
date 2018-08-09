#############################################################################
##
#W  standard/semieunit.tst
#Y  Copyright (C) 2016                                    Christopher Russell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semieunit.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

#T# McAlisterTripleSemigroup
gap> G := SymmetricGroup([2 .. 5]);;
gap> x := Digraph([[1], [1, 2], [1, 3], [1, 4], [1, 5]]);;
gap> y := Digraph([[1], [1, 2], [1, 3], [1, 4]]);;
gap> M := McAlisterTripleSemigroup(G, x, y, OnPoints);
<McAlister triple semigroup over Sym( [ 2 .. 5 ] )>
gap> IsIsomorphicSemigroup(M, McAlisterTripleSemigroup(G, x, [1, 2, 3, 4],
> OnPoints));
true
gap> IsIsomorphicSemigroup(M, McAlisterTripleSemigroup(G, x, y));
true
gap> IsIsomorphicSemigroup(M, McAlisterTripleSemigroup(G, x, [1, 2, 3, 4]));
true
gap> M = McAlisterTripleSemigroup(G, x, [1, 2, 3, 4], OnPoints);
false
gap> M = Semigroup(Elements(M));
true
gap> M1 := Semigroup(GeneratorsOfSemigroup(M));;
gap> M = M1;
true
gap> M = Semigroup(Elements(M)[2]);
false
gap> M1 := M;;
gap> M = M1;
true
gap> String(M);
"McAlisterTripleSemigroup(SymmetricGroup( [ 2 .. 5 ] ), Digraph( [ [ 1 ], [ 1,\
 2 ], [ 1, 3 ], [ 1, 4 ], [ 1, 5 ] ] ), [ 1 .. 4 ])"
gap> Print(M, "\n");
McAlisterTripleSemigroup(SymmetricGroup( [ 2 .. 5 ] ), Digraph( [ [ 1 ], [ 1, \
2 ], [ 1, 3 ], [ 1, 4 ], [ 1, 5 ] ] ), [ 1 .. 4 ])

#T# McAlisterTripleSemigroup with bad inputs
gap> G1 := FreeGroup(1);;
gap> act := function(x, g) return x; end;;
gap> McAlisterTripleSemigroup(G1, Digraph([[1]]), [1], act);
Error, Semigroups: McAlisterTripleSemigroup: usage,
the first argument must be a finite group,
gap> x1 := Digraph([[1], [1], [3], [3], [1, 3, 5]]);;
gap> G1 := AutomorphismGroup(x1);;
gap> McAlisterTripleSemigroup(G1, x1, [1, 2, 5]);;
Error, Semigroups: McAlisterTripleSemigroup: usage,
the second argument must be a partial order digraph,
gap> x1 := Digraph([[1], [1, 2], [3], [3, 4], [1, 3, 5], [1, 3, 5, 6]]);;
gap> McAlisterTripleSemigroup(G, x1, x1);;
Error, Semigroups: McAlisterTripleSemigroup: usage,
the first argument (a group) must act by order automorphisms on the second arg\
ument (a partial order digraph),
gap> y1 := Digraph([[1], [1, 2], [3], [3, 4], [1, 3, 5], [1, 3, 6]]);;
gap> McAlisterTripleSemigroup(Group(()), x1, y1);
Error, Semigroups: McAlisterTripleSemigroup: usage,
the third argument <Y> must be an induced subdigraph of
the second argument <X> with vertex labels corresponding
to the vertices of <X> on which <Y> was induced,
gap> G1 := SymmetricGroup([5 .. 11]);;
gap> McAlisterTripleSemigroup(G1, x, y);;
Error, Action not well-defined. See the manual section
``Action on canonical representatives''.
gap> x2 := Digraph([[1], [2], [1, 2, 3], [1, 2, 4]]);;
gap> G1 := AutomorphismGroup(x2);;
gap> McAlisterTripleSemigroup (G1, x2, x2);;
Error, Semigroups: McAlisterTripleSemigroup: usage,
the third argument must be a join-semilattice digraph,
gap> y2 := Digraph([[1]]);;
gap> SetDigraphVertexLabel(y2, 1, 2);;
gap> McAlisterTripleSemigroup(G, x, y2);;
Error, Semigroups: McAlisterTripleSemigroup: usage,
the out-neighbours of each vertex of <X> which is in <Y> must contain only ver\
tices which are in <Y> - see the documentation for more detail,
gap> McAlisterTripleSemigroup(TrivialSubgroup(G), x, y, OnPoints);;
Error, Semigroups: McAlisterTripleSemigroup: usage,
every vertex of <X> must be in the orbit of some vertex of <X> which is in <Y>\
 - see the documentation for more detail,
gap> y3 := Digraph([[1], [1, 2], [1, 3]]);;
gap> McAlisterTripleSemigroup(Group([(2, 3), (4, 5)]), x, y3, OnPoints);
Error, Semigroups: McAlisterTripleSemigroup: usage,
every vertex of <X> must be in the orbit of some vertex of <X> which is in <Y>\
 - see the documentation for more detail,
gap> x3 := Digraph([[1], [2]]);;
gap> McAlisterTripleSemigroup(AutomorphismGroup(x3), x3, y2);;
Error, Semigroups: McAlisterTripleSemigroup: 
<act> must fix the vertex of <X> which is the minimal vertex of <Y> - see the \
documentation for more detail,

#T# IsomorphismSemigroup, AsSemigroup
gap> ps := InverseSemigroup([PartialPerm([2, 3, 4, 5], [1, 3, 5, 4]),
> PartialPerm([2, 3, 4, 5], [1, 4, 5, 3])]);;
gap> Mps := IsomorphismSemigroup(IsMcAlisterTripleSemigroup, ps);;
gap> Image(Mps);
[ (1, ()), (1, (1,2)(3,6)(4,5)), (1, (1,4)(2,6)(3,5)), (1, (1,6,5)(2,4,3)), 
  (3, ()), (3, (1,3)(2,5)(4,6)), (3, (1,4)(2,6)(3,5)), (3, (1,5,6)(2,3,4)), 
  (4, ()), (4, (1,2)(3,6)(4,5)), (4, (1,3)(2,5)(4,6)), (4, (1,4)(2,6)(3,5)), 
  (4, (1,5,6)(2,3,4)), (4, (1,6,5)(2,4,3)) ]
gap> AsSemigroup(IsMcAlisterTripleSemigroup, ps);
<McAlister triple semigroup over Group([ (1,5,6)(2,3,4), (1,4)(2,6)(3,5) ])>
gap> ps := InverseSemigroup([PartialPerm([1, 4, 6, 7], [1, 4, 6, 7]),
>   PartialPerm([2, 3, 6, 7], [2, 3, 6, 7]), PartialPerm([6, 7], [6, 7]),
>   PartialPerm([2, 3, 5, 6, 7], [2, 3, 5, 6, 7]),
>   PartialPerm([1, 4, 6, 7], [2, 3, 7, 6]),
>   PartialPerm([2, 3, 6, 7], [1, 4, 7, 6]), PartialPerm([6, 7], [7, 6])]);;
gap> Mps := IsomorphismSemigroup(IsMcAlisterTripleSemigroup, ps);;
gap> Image(Mps);
[ (1, ()), (1, (1,2)), (2, ()), (2, (1,2)), (3, ()), (3, (1,2)), (4, ()) ]
gap> Elements(Range(Mps));
[ (1, ()), (1, (1,2)), (2, ()), (2, (1,2)), (3, ()), (3, (1,2)), (4, ()) ]
gap> IsWholeFamily(Range(Mps));
true
gap> AsSemigroup(IsMcAlisterTripleSemigroup, ps);
<McAlister triple semigroup over Group([ (1,2) ])>

#T# McAlister triple subsemigroup methods
gap> S := Semigroup(Image(Mps){[1 .. 3]});
<McAlister triple subsemigroup over Group([ (1,2) ])>
gap> attr := [MTSSemilattice, MTSGroup, MTSPartialOrder, MTSAction,
> MTSActionHomomorphism, MTSUnderlyingAction, MTSComponents,
> MTSQuotientDigraph, MTSSemilatticeVertexLabelInverseMap];;
gap> M := Range(Mps);;
gap> ForAll(attr, A -> A(S) = A(M));
true
gap> Print(S, "\n");
Semigroup([ MTSE(McAlisterTripleSemigroup(Group( [ (1,2) ] ), Digraph( [ [ 1, \
3 ], [ 2, 3 ], [ 3 ], [ 2, 3, 4 ], [ 1, 3, 5 ] ] ), [ 3, 2, 4, 1 ]), 1, ()), M\
TSE(McAlisterTripleSemigroup(Group( [ (1,2) ] ), Digraph( [ [ 1, 3 ], [ 2, 3 ]\
, [ 3 ], [ 2, 3, 4 ], [ 1, 3, 5 ] ] ), [ 3, 2, 4, 1 ]), 1, (1,2)), MTSE(McAlis\
terTripleSemigroup(Group( [ (1,2) ] ), Digraph( [ [ 1, 3 ], [ 2, 3 ], [ 3 ], [\
 2, 3, 4 ], [ 1, 3, 5 ] ] ), [ 3, 2, 4, 1 ]), 2, ()) ]

#T# AsSemigroup with bad input
gap> T := Semigroup([PartialPerm([1], [3]),
> PartialPerm([1, 2, 3], [2, 3, 1]), PartialPerm([1, 3], [2, 3])]);;
gap> AsSemigroup(IsMcAlisterTripleSemigroup, T);
Error, Semigroups: IsomorphismSemigroup: usage,
the semigroup is not E-unitary,

#T# Other McAlisterTripleSemigroup tests
gap> G := SymmetricGroup([2 .. 5]);;
gap> x := Digraph([[1], [1, 2], [1, 3], [1, 4], [1, 5]]);
<digraph with 5 vertices, 9 edges>
gap> y := Digraph([[1], [1, 2], [1, 3], [1, 4]]);
<digraph with 4 vertices, 7 edges>
gap> M := McAlisterTripleSemigroup(G, x, y, OnPoints);
<McAlister triple semigroup over Sym( [ 2 .. 5 ] )>
gap> IsIsomorphicSemigroup(M, McAlisterTripleSemigroup(G, x, x));
false
gap> IsInverseSemigroup(Semigroup(GeneratorsOfSemigroup(M)));
true
gap> elms := Enumerator(M);;
gap> String(elms[1]);
"MTSE(McAlisterTripleSemigroup(SymmetricGroup( [ 2 .. 5 ] ), Digraph( [ [ 1 ],\
 [ 1, 2 ], [ 1, 3 ], [ 1, 4 ], [ 1, 5 ] ] ), [ 1 .. 4 ]), 2, (3,4,5))"
gap> OneImmutable(M);
fail
gap> M1 := McAlisterTripleSemigroup(G, x, [1, 2]);;
gap> OneImmutable(M1);
(2, ())

#T# McAlister triple semigroup elements
gap> MTSE(M, 4, (2, 4)(3, 5)) * MTSE(M, 4, (2, 5, 3, 4));
(1, (4,5))
gap> MTSE(M, 1, (2, 3, 4, 5)) ^ -2;
(1, (2,4)(3,5))
gap> M = MTSEParent(MTSE(M, 1, (4, 5)));
true
gap> M = McAlisterTripleSemigroupElementParent(MTSE(M, 1, (4, 5)));
true
gap> LeftOne(MTSE(M, 4, (2, 4)(3, 5))) = MTSE(M, 4, ());
true
gap> RightOne(MTSE(M, 4, (2, 4)(3, 5))) = MTSE(M, 2, ());
true
gap> MTSE(M, 10, (2, 3, 4, 5));
Error, Semigroups: McAlisterTripleSemigroupElement: usage,
second argument should be a vertex label of the join-semilattice of the McAlis\
ter triple,
gap> MTSE(M, 1, (5, 6));
Error, Semigroups: McAlisterTripleSemigroupElement: usage,
third argument must an element of the group of the McAlister triple,
gap> MTSE(M, 3, (2, 4)(3, 5));
Error, Semigroups: McAlisterTripleSemigroupElement: usage,
the arguments do not specify an element of the McAlister triple semigroup,
gap> MTSE(M, 1, ())[3];
Error, Semigroups: ELM_LIST (for a McAlisterTripleSemigroupElement): usage,
the index must be at most 2,
gap> M1 := McAlisterTripleSemigroup(G, x, x);;
gap> MTSE(M1, 4, (2, 4)(3, 5)) * MTSE(M, 4, (2, 5, 3, 4));
Error, Semigroups: * (for an McAlisterTripleSemigroupElement): usage,
the elements must be from the same McAlister triple semigroup,

#T# IsomorphismSemigroups
gap> x1 := Digraph([[1], [1, 2], [1, 3], [1, 2, 3, 4], [1, 2, 3, 5]]);;
gap> G1 := Group([(4, 5)]);;
gap> x2 := Digraph([[1, 3, 4, 5], [2, 3, 4, 5], [3, 5], [4, 5], [5]]);;
gap> G2 := Group([(1, 2)]);;
gap> M1 := McAlisterTripleSemigroup(G1, x1, [1, 2, 3, 4]);;
gap> M2 := McAlisterTripleSemigroup(G2, x2, [1, 3, 4, 5]);;
gap> IsomorphismSemigroups(M1, M2);
MappingByFunction( <McAlister triple semigroup over Group([ (4,
5) ])>, <McAlister triple semigroup over Group([ (1,
2) ])>, function( s ) ... end )
gap> x3 := Digraph([[1], [1, 2], [1, 3], [1, 2, 4], [1, 2, 5], [1, 3, 6],
> [1, 3, 7]]);;
gap> y3 := Digraph([[1], [1, 2], [1, 2, 3], [1, 2, 4]]);;
gap> y4 := Digraph([[1], [1, 2], [1, 2, 3], [1, 2, 4]]);;
gap> SetDigraphVertexLabels(y3, [1, 3, 7, 6]);;
gap> SetDigraphVertexLabels(y4, [1, 2, 4, 5]);;
gap> G3 := Group([(6, 7), (4, 5), (2, 3)(4, 6)(5, 7)]);;
gap> M3 := McAlisterTripleSemigroup(G3, x3, y3);;
gap> M4 := McAlisterTripleSemigroup(G3, x3, y4);;
gap> IsomorphismSemigroups(M3, M4);
MappingByFunction( <McAlister triple semigroup over Group([ (6,7), (4,5), (2,
3)(4,6)(5,7) ])>, <McAlister triple semigroup over Group([ (6,7), (4,5), (2,3)
(4,6)(5,7) ])>, function( s ) ... end )
gap> IsomorphismSemigroups(M1, M3);
fail
gap> M5 := McAlisterTripleSemigroup(G3, x3, [1, 2, 3, 4, 5]);;
gap> IsomorphismSemigroups(M5, M3);
fail
gap> M6 := McAlisterTripleSemigroup(Group((4, 5)), x1, [1, 2, 3, 4]);;
gap> x4 := Digraph([[1], [1, 2], [1, 3], [1, 2, 3, 4]]);;
gap> M7 := McAlisterTripleSemigroup(Group((5, 6)), x4, x4);;
gap> IsomorphismSemigroups(M6, M7);
fail

#T# IsomorphicSemigroups with bad input
gap> x1 := Digraph([[1], [1, 2], [1, 3]]);;
gap> G := Group((2, 3));;
gap> M1 := McAlisterTripleSemigroup(G, x1, x1);;
gap> M2 := McAlisterTripleSemigroup(G, x1, [1, 2]);;
gap> IsomorphismSemigroups(M1, M2);
fail
gap> x1 := Digraph([[1], [1, 2], [1, 3]]);;
gap> x2 := Digraph([[1], [1, 2], [1, 3], [1, 4]]);;
gap> M3 := McAlisterTripleSemigroup(Group((2, 3), (2, 3, 4)), x2, x1);;
gap> IsomorphismSemigroups(M1, M3);
fail
gap> M4 := McAlisterTripleSemigroup(Group(()), x1, x1);;
gap> IsomorphismSemigroups(M1, M4);
fail
gap> act := function(x, g)
> return x;
> end;;
gap> M5 := McAlisterTripleSemigroup(G, x1, x1, act);;
gap> IsomorphismSemigroups(M1, M5);
fail

#T# IsomorphismSemigroups, where RepresentativeAction fails
gap> gr := DigraphFromDigraph6String("+H_A?GC_Q@G~wA?G");
<digraph with 9 vertices, 20 edges>
gap> G := Group((1, 2, 3)(4, 5, 6), (8, 9));
Group([ (1,2,3)(4,5,6), (8,9) ])
gap> S1 := McAlisterTripleSemigroup(G, gr, [1, 4, 5, 7, 8]);
<McAlister triple semigroup over Group([ (1,2,3)(4,5,6), (8,9) ])>
gap> S2 := McAlisterTripleSemigroup(G, gr, [3, 6, 7, 8, 9]);
<McAlister triple semigroup over Group([ (1,2,3)(4,5,6), (8,9) ])>
gap> IsomorphismSemigroups(S1, S2);
fail

#T# IsIsomorphicSemigroup
gap> IsIsomorphicSemigroup(M1, M1);
true
gap> IsIsomorphicSemigroup(M1, M5);
false

#T# IsFInverseMonoid, IsFInverseSemigroup
gap> S := Semigroup([Transformation([2, 2]), Transformation([2, 1, 2]),
> Transformation([3, 3, 2])]);;
gap> IsFInverseMonoid(S);
false
gap> IsFInverseSemigroup(S);
false
gap> S := InverseMonoid([PartialPermNC([1, 3], [1, 3]),
> PartialPermNC([1, 2], [3, 1]), PartialPermNC([1, 2], [3, 2])]);;
gap> IsFInverseMonoid(S);
false
gap> S := InverseMonoid([PartialPermNC([1], [1]),
> PartialPermNC([1, 2], [1, 2]), PartialPermNC([1, 2, 3], [1, 2, 3])]);;
gap> IsFInverseMonoid(S);
true
gap> IsFInverseSemigroup(S);
true
gap> G := Group((2, 3));;
gap> x := Digraph([[1], [1, 2], [1, 3]]);;
gap> M := McAlisterTripleSemigroup(G, x, x);;
gap> IsFInverseSemigroup(M);
true
gap> IsFInverseMonoid(M);
false
gap> M := McAlisterTripleSemigroup(Group(()), Digraph([[1], [1, 2]]), [1, 2]);;
gap> IsFInverseMonoid(M);
true
gap> IsFInverseSemigroup(M);
true
gap> S := McAlisterTripleSemigroup(Group((4, 5)),
> Digraph([[1], [1, 2], [1, 3], [1, 2, 3, 4], [1, 2, 3, 5]]), [1 .. 4]);
<McAlister triple semigroup over Group([ (4,5) ])>
gap> IsFInverseSemigroup(S);
false

#T# EUnitaryInverseCover 
#TODO: Add checks that these covers are idempotent separating homomorphisms
gap> S := InverseMonoid([PartialPermNC([1, 3], [1, 3]),
> PartialPermNC([1, 2], [3, 1]), PartialPermNC([1, 2], [3, 2])]);;
gap> cov := EUnitaryInverseCover(S);;
gap> IsEUnitaryInverseSemigroup(Source(cov));
true
gap> S = Range(cov);
true
gap> S := Monoid([Transformation([1, 4, 4, 4]),
> Transformation([1, 2, 4, 4])]);;
gap> cov := EUnitaryInverseCover(S);;
gap> IsEUnitaryInverseSemigroup(Source(cov));
true
gap> S = Range(cov);
true
gap> S := Semigroup([Bipartition([[1, 3, -1, -2, -3], [2]]),
> Bipartition([[1, 3, -1, -2], [2, -3]]),
> Bipartition([[1, 3], [2], [-1, -2, -3]]),
> Bipartition([[1, -2, -3], [2, 3, -1]])]);;
gap> EUnitaryInverseCover(S);
Error, Semigroups: EUnitaryInverseCover: usage,
the argument must be an inverse semigroup,
gap> S := InverseSemigroup([PartialPerm([1, 2, 4], [4, 3, 2]),
> PartialPerm([1, 3], [3, 4])]);;
gap> cov := EUnitaryInverseCover(S);;
gap> IsEUnitaryInverseSemigroup(Source(cov));
true
gap> S = Range(cov);
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(A);
gap> Unbind(act);
gap> Unbind(cov);
gap> Unbind(elms);
gap> Unbind(G);
gap> Unbind(M);
gap> Unbind(Mps);
gap> Unbind(ps);
gap> Unbind(S);
gap> Unbind(x);
gap> Unbind(x1);
gap> Unbind(x2);
gap> Unbind(x3);
gap> Unbind(y);
gap> Unbind(y1);
gap> Unbind(y2);
gap> Unbind(y3);

#E#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/semieunit.tst");
