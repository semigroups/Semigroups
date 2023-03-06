#############################################################################
##
#W  standard/semigroups/semieunit.tst
#Y  Copyright (C) 2016-2022                            Christopher Russell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local G, G1, G2, G3, M, M1, M2, M3, M4, M5, M6, M7, Mps, S, S1, S2, T, act
#@local attr, cov, elms, gr, iso, ps, s, str, x, x1, x2, x3, x4, y, y1, y2, y3
#@local y4
gap> START_TEST("Semigroups package: standard/semigroups/semieunit.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

#  McAlisterTripleSemigroup
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
gap>  String(M) = "McAlisterTripleSemigroup(SymmetricGroup( [ 2 .. 5 ] ), \
> DigraphFromDigraph6String(\"&D`acg_\"), [ 1 .. 4 ])";
true
gap> s := "";;
gap> str := OutputTextString(s, false);;
gap> PrintTo(str, M);
gap> CloseStream(str);
gap> String(M) = "McAlisterTripleSemigroup(SymmetricGroup( [ 2 .. 5 ] ), \
> DigraphFromDigraph6String(\"&D`acg_\"), [ 1 .. 4 ])";
true

#  McAlisterTripleSemigroup with bad inputs
gap> G1 := FreeGroup(1);;
gap> act := function(x, g) return x; end;;
gap> McAlisterTripleSemigroup(G1, Digraph([[1]]), [1], act);
Error, the 1st argument (a group) is not finite
gap> x1 := Digraph([[1], [1], [3], [3], [1, 3, 5]]);;
gap> G1 := AutomorphismGroup(x1);;
gap> McAlisterTripleSemigroup(G1, x1, [1, 2, 5]);;
Error, the 2nd argument (a digraph) must be a partial order digraph
gap> x1 := Digraph([[1], [1, 2], [3], [3, 4], [1, 3, 5], [1, 3, 5, 6]]);;
gap> McAlisterTripleSemigroup(G, x1, x1);;
Error, the 1st argument (a group) must act by order automorphisms on the 2nd a\
rgument (a partial order digraph)
gap> y1 := Digraph([[1], [1, 2], [3], [3, 4], [1, 3, 5], [1, 3, 6]]);;
gap> McAlisterTripleSemigroup(Group(()), x1, y1);
Error, the 3rd argument <X> (a digraph) must be an induced subdigraph of the 2\
nd argument <Y> (a digraph) with vertex labels corresponding to the vertices o\
f <X> on which <Y> was induced
gap> G1 := SymmetricGroup([5 .. 11]);;
gap> McAlisterTripleSemigroup(G1, x, y);;
Error, Action not well-defined. See the manual section
``Action on canonical representatives''.
gap> x2 := Digraph([[1], [2], [1, 2, 3], [1, 2, 4]]);;
gap> G1 := AutomorphismGroup(x2);;
gap> McAlisterTripleSemigroup (G1, x2, x2);;
Error, the 3rd argument (a digraph) must be a join-semilattice digraph
gap> y2 := Digraph([[1]]);;
gap> SetDigraphVertexLabel(y2, 1, 2);;
gap> McAlisterTripleSemigroup(G, x, y2);;
Error, the out-neighbours of each vertex of the 2nd argument (a digraph) which\
 is in the 3rd argument <Y> (a digraph) must contain only vertices which are i\
n <Y> - see the documentation for more details
gap> McAlisterTripleSemigroup(TrivialSubgroup(G), x, y, OnPoints);;
Error, every vertex of <X> must be in the orbit of some vertex of <X> which is\
 in <Y> - see the documentation for more detail
gap> y3 := Digraph([[1], [1, 2], [1, 3]]);;
gap> McAlisterTripleSemigroup(Group([(2, 3), (4, 5)]), x, y3, OnPoints);
Error, every vertex of <X> must be in the orbit of some vertex of <X> which is\
 in <Y> - see the documentation for more detail
gap> x3 := Digraph([[1], [2]]);;
gap> McAlisterTripleSemigroup(AutomorphismGroup(x3), x3, y2);;
Error, <act> must fix the vertex of <X> which is the minimal vertex of <Y> - s\
ee the documentation for more detail

#  IsomorphismSemigroup, AsSemigroup
gap> ps := InverseSemigroup([PartialPerm([2, 3, 4, 5], [1, 3, 5, 4]),
> PartialPerm([2, 3, 4, 5], [1, 4, 5, 3])]);;
gap> Mps := IsomorphismSemigroup(IsMcAlisterTripleSemigroup, ps);;
gap> Range(Mps);
<McAlister triple semigroup over Group([ (1,5,6)(2,3,4), (1,4)(2,6)(3,5) ])>
gap> AsSemigroup(IsMcAlisterTripleSemigroup, ps);
<McAlister triple semigroup over Group([ (1,5,6)(2,3,4), (1,4)(2,6)(3,5) ])>
gap> ps := InverseSemigroup([PartialPerm([1, 4, 6, 7], [1, 4, 6, 7]),
>   PartialPerm([2, 3, 6, 7], [2, 3, 6, 7]), PartialPerm([6, 7], [6, 7]),
>   PartialPerm([2, 3, 5, 6, 7], [2, 3, 5, 6, 7]),
>   PartialPerm([1, 4, 6, 7], [2, 3, 7, 6]),
>   PartialPerm([2, 3, 6, 7], [1, 4, 7, 6]), PartialPerm([6, 7], [7, 6])]);;
gap> Mps := IsomorphismSemigroup(IsMcAlisterTripleSemigroup, ps);;
gap> Range(Mps);
<McAlister triple semigroup over Group([ (1,2) ])>
gap> Elements(Range(Mps));;
gap> IsWholeFamily(Range(Mps));
true
gap> AsSemigroup(IsMcAlisterTripleSemigroup, ps);
<McAlister triple semigroup over Group([ (1,2) ])>
gap> G := Semigroup(PartialPerm([1, 2, 3], [2, 3, 1]));;
gap> iso := IsomorphismSemigroup(IsMcAlisterTripleSemigroup, G);;
gap> PartialPerm([1, 2, 3], [2, 3, 1]) ^ iso;;

#  McAlister triple subsemigroup methods
gap> S := Semigroup(Elements(Range(Mps)){[1, 2, 3]});
<McAlister triple subsemigroup over Group([ (1,2) ])>
gap> attr := [MTSSemilattice, MTSGroup, MTSPartialOrder, MTSAction,
> MTSActionHomomorphism, MTSUnderlyingAction, MTSComponents,
> MTSQuotientDigraph, MTSSemilatticeVertexLabelInverseMap];;
gap> M := Range(Mps);;
gap> ForAll(attr, A -> A(S) = A(M));
true
gap> s := "";;
gap> str := OutputTextString(s, false);;
gap> PrintTo(str, Semigroup(Elements(M1){[1, 2, 3]}));
gap> CloseStream(str);
gap>  s = 
> "Semigroup([ MTSE(McAlisterTripleSemigroup(SymmetricGroup( [ 2 .. 5 ] ), Digra\
> p\\\nhFromDigraph6String(\"&D`acg_\"), [ 1 .. 4 ]), 1, ()), MTSE(McAlisterTrip\
> leSemig\\\nroup(SymmetricGroup( [ 2 .. 5 ] ), DigraphFromDigraph6String(\"&D`a\
> cg_\"), [ 1 .\\\n. 4 ]), 1, (4,5)), MTSE(McAlisterTripleSemigroup(SymmetricGro\
> up( [ 2 .. 5 ] ),\\\n DigraphFromDigraph6String(\"&D`acg_\"), [ 1 .. 4 ]), 1, \
> (3,4)) ])";
true

#  AsSemigroup with bad input
gap> T := Semigroup([PartialPerm([1], [3]),
> PartialPerm([1, 2, 3], [2, 3, 1]), PartialPerm([1, 3], [2, 3])]);;
gap> AsSemigroup(IsMcAlisterTripleSemigroup, T);
Error, the 2nd argument (a semigroup) is not E-unitary

#  Other McAlisterTripleSemigroup tests
gap> G := SymmetricGroup([2 .. 5]);;
gap> x := Digraph([[1], [1, 2], [1, 3], [1, 4], [1, 5]]);;
gap> y := Digraph([[1], [1, 2], [1, 3], [1, 4]]);;
gap> M := McAlisterTripleSemigroup(G, x, y, OnPoints);
<McAlister triple semigroup over Sym( [ 2 .. 5 ] )>
gap> IsIsomorphicSemigroup(M, McAlisterTripleSemigroup(G, x, x));
false
gap> IsInverseSemigroup(Semigroup(GeneratorsOfSemigroup(M)));
true
gap> elms := Enumerator(M);;
gap> String(elms[1]){[1 .. 40]};
"MTSE(McAlisterTripleSemigroup(SymmetricG"
gap> OneImmutable(M);
fail
gap> M1 := McAlisterTripleSemigroup(G, x, [1, 2]);;
gap> OneImmutable(M1);
(2, ())
gap> x1 := DigraphFromDiSparse6String(".P__@_@_@__D_D_D__H_H_H_@DH_@DHL_@DHL_@DHLp?`abcdefghijklmno");;
gap> y1 := InducedSubdigraph(x1, [1, 2, 3, 4, 6, 10, 11, 14, 15]);;
gap> McAlisterTripleSemigroup(AutomorphismGroup(x1), x1, y1);;

#  McAlister triple semigroup elements
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
Error, the 2nd argument should be a vertex label of the join-semilattice of th\
e McAlister triple
gap> MTSE(M, 1, (5, 6));
Error, the 3rd argument must an element of the group of the McAlister triple
gap> MTSE(M, 3, (2, 4)(3, 5));
Error, the arguments do not specify an element of the McAlister triple semigro\
up
gap> MTSE(M, 1, ())[3];
Error, the 2nd argument (a pos. int.) must be at most 2
gap> M1 := McAlisterTripleSemigroup(G, x, x);;
gap> MTSE(M1, 4, (2, 4)(3, 5)) * MTSE(M, 4, (2, 5, 3, 4));
Error, the arguments (McAlister triple elements) do not belong to the same McA\
lister triple semigroup

#  IsomorphismSemigroups
gap> x1 := Digraph([[1], [1, 2], [1, 3], [1, 2, 3, 4], [1, 2, 3, 5]]);;
gap> G1 := Group([(4, 5)]);;
gap> x2 := Digraph([[1, 3, 4, 5], [2, 3, 4, 5], [3, 5], [4, 5], [5]]);;
gap> G2 := Group([(1, 2)]);;
gap> M1 := McAlisterTripleSemigroup(G1, x1, [1, 2, 3, 4]);;
gap> M2 := McAlisterTripleSemigroup(G2, x2, [1, 3, 4, 5]);;
gap> IsomorphismSemigroups(M1, M2);;
gap> x3 := Digraph([[1], [1, 2], [1, 3], [1, 2, 4], [1, 2, 5], [1, 3, 6],
> [1, 3, 7]]);;
gap> y3 := Digraph([[1], [1, 2], [1, 2, 3], [1, 2, 4]]);;
gap> y4 := Digraph([[1], [1, 2], [1, 2, 3], [1, 2, 4]]);;
gap> SetDigraphVertexLabels(y3, [1, 3, 7, 6]);;
gap> SetDigraphVertexLabels(y4, [1, 2, 4, 5]);;
gap> G3 := Group([(6, 7), (4, 5), (2, 3)(4, 6)(5, 7)]);;
gap> M3 := McAlisterTripleSemigroup(G3, x3, y3);;
gap> M4 := McAlisterTripleSemigroup(G3, x3, y4);;
gap> IsomorphismSemigroups(M3, M4);;
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

#  IsomorphismSemigroups with bad input
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

#  IsomorphismSemigroups, where RepresentativeAction fails
gap> gr := DigraphFromDigraph6String("+H_A?GC_Q@G~wA?G");
<immutable digraph with 9 vertices, 20 edges>
gap> G := Group((1, 2, 3)(4, 5, 6), (8, 9));;
gap> S1 := McAlisterTripleSemigroup(G, gr, [1, 4, 5, 7, 8]);
<McAlister triple semigroup over Group([ (1,2,3)(4,5,6), (8,9) ])>
gap> S2 := McAlisterTripleSemigroup(G, gr, [3, 6, 7, 8, 9]);
<McAlister triple semigroup over Group([ (1,2,3)(4,5,6), (8,9) ])>
gap> IsomorphismSemigroups(S1, S2);
fail

#  IsIsomorphicSemigroup
gap> IsIsomorphicSemigroup(M1, M1);
true
gap> IsIsomorphicSemigroup(M1, M5);
false

#  IsFInverseMonoid, IsFInverseSemigroup
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

# EUnitaryInverseCover 
# TODO(later) Add checks that these covers are idempotent separating
# homomorphisms
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
Error, the argument must be an inverse semigroup
gap> S := InverseSemigroup([PartialPerm([1, 2, 4], [4, 3, 2]),
> PartialPerm([1, 3], [3, 4])]);;
gap> cov := EUnitaryInverseCover(S);;
gap> IsEUnitaryInverseSemigroup(Source(cov));
true
gap> S = Range(cov);
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/semigroups/semieunit.tst");
