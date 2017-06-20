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
gap> LoadPackage("Digraphs", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

#T# McAlisterTripleSemigroup
gap> G := SymmetricGroup([2 .. 5]);;
gap> x := Digraph([[1], [1, 2], [1, 3], [1, 4], [1, 5]]);;
gap> y := Digraph([[1], [1, 2], [1, 3], [1, 4]]);;
gap> M := McAlisterTripleSemigroup(G, OnPoints, x, y);
<McAlister triple semigroup over Sym( [ 2 .. 5 ] )>
gap> M = McAlisterTripleSemigroup(G, OnPoints, x, [1, 2, 3, 4]);
true
gap> M = McAlisterTripleSemigroup(G, x, y);
true
gap> M = McAlisterTripleSemigroup(G, x, [1, 2, 3, 4]);
true
gap> String(M);
"McAlisterTripleSemigroup(SymmetricGroup( [ 2 .. 5 ] ), Digraph( [ [ 1 ], [ 1,\
 2 ], [ 1, 3 ], [ 1, 4 ], [ 1, 5 ] ] ), Digraph( [ [ 1 ], [ 1, 2 ], [ 1, 3 ], \
[ 1, 4 ] ] ))"

#T# McAlisterTripleSemigroup with bad inputs
gap> G1 := FreeGroup(1);;
gap> act := function(x, g) return x; end;;
gap> McAlisterTripleSemigroup(G1, act, Digraph([[1]]), [1]);
Error, Semigroups: McAlisterTripleSemigroup: usage,
the first argument must be a finite group,
gap> y1 := Digraph([[1], [1, 2], [1, 3], [1, 4], [1, 5], [1, 6]]);;
gap> McAlisterTripleSemigroup(G, x, y1);;
Error, Semigroups: McAlisterTripleSemigroup: usage,
the fourth argument must be a subdigraph of the third argument,
gap> x1 := Digraph([[1], [1], [3], [3, 4], [1, 3, 5]]);;
gap> McAlisterTripleSemigroup(G, x1, x1);;
Error, Semigroups: McAlisterTripleSemigroup: usage,
the first argument (a group) must act by order automorphisms on the third argu\
ment (a partial order digraph),
gap> G1 := SymmetricGroup([5 .. 11]);;
gap> McAlisterTripleSemigroup(G1, x, y);;
Error, Action not well-defined. See the manual section
``Action on canonical representatives''.
gap> G1 := AutomorphismGroup(x1);;
gap> McAlisterTripleSemigroup(G1, x1, x1);;
Error, Semigroups: McAlisterTripleSemigroup: usage,
the third argument must be a partial order digraph,
gap> x2 := Digraph([[1], [2], [1, 2, 3], [1, 2, 4]]);;
gap> G1 := AutomorphismGroup(x2);;
gap> McAlisterTripleSemigroup (G1, x2, x2);;
Error, Semigroups: McAlisterTripleSemigroup: usage,
the fourth argument must be a join semilattice digraph,
gap> y2 := Digraph([[1]]);;
gap> SetDigraphVertexLabel(y2, 1, 2);;
gap> McAlisterTripleSemigroup(G, x, y2);;
Error, Semigroups: McAlisterTripleSemigroup: usage,
condition M2 is not satisfied - see the documentation for details,
gap> McAlisterTripleSemigroup(TrivialSubgroup(G), OnPoints, x, y);;
Error, Semigroups: McAlisterTripleSemigroup: 
Condition M3 is not satisfied - see the documentation for details,
gap> y3 := Digraph([[1], [1, 2], [1, 3]]);;
gap> McAlisterTripleSemigroup(Group([(2, 3), (4, 5)]), OnPoints, x, y3);
Error, Semigroups: McAlisterTripleSemigroup: usage,
condition M3 is not satisfied - see the documentation for details,
gap> x3 := Digraph([[1], [2]]);;
gap> McAlisterTripleSemigroup(AutomorphismGroup(x3), x3, y2);;
Error, Semigroups: McAlisterTriple: usage,
condition M4 is not satisfied - see the documentation for details,

#T# IsomorphismSemigroup, AsMcAlisterTripleSemigroup
gap> ps := InverseSemigroup([PartialPerm([2, 3, 4, 5], [1, 3, 5, 4]),
> PartialPerm([2, 3, 4, 5], [1, 4, 5, 3])]);;
gap> Mps := IsomorphismMcAlisterTripleSemigroup(ps);;
gap> Image(Mps);
[ (1, ()), (1, (2,3)), (1, (1,2)), (1, (1,2,3)), (1, (1,3,2)), (1, (1,3)), 
  (2, ()), (2, (2,3)), (2, (1,2,3)), (2, (1,3)), (3, ()), (3, (2,3)), 
  (3, (1,2)), (3, (1,3,2)) ]
gap> AsMcAlisterTripleSemigroup(ps);
<McAlister triple semigroup over Group([ (2,3), (1,2,3), (2,3), (1,3,2) ])>
gap> ps := InverseSemigroup([PartialPerm([1, 4, 6, 7], [1, 4, 6, 7]),
>   PartialPerm([2, 3, 6, 7], [2, 3, 6, 7]), PartialPerm([6, 7], [6, 7]),
>   PartialPerm([2, 3, 5, 6, 7], [2, 3, 5, 6, 7]),
>   PartialPerm([1, 4, 6, 7], [2, 3, 7, 6]),
>   PartialPerm([2, 3, 6, 7], [1, 4, 7, 6]), PartialPerm([6, 7], [7, 6])]);;
gap> Mps := IsomorphismMcAlisterTripleSemigroup(ps);;
gap> Image(Mps);
[ (1, ()), (1, (1,2)), (2, ()), (3, ()), (3, (1,2)), (5, ()), (5, (1,2)) ]
gap> Elements(Range(Mps));
[ (1, ()), (1, (1,2)), (2, ()), (3, ()), (3, (1,2)), (5, ()), (5, (1,2)) ]
gap> IsWholeFamily(Image(Mps));
true
gap> AsMcAlisterTripleSemigroup(ps);
<McAlister triple semigroup over Group([ (), (), (), (1,2), (1,2), (1,2) ])>

#T# AsMcAlisterTripleSemigroup with bad input
gap> T := Semigroup([PartialPerm([1], [3]),
> PartialPerm([1, 2, 3], [2, 3, 1]), PartialPerm([1, 3], [2, 3])]);;
gap> AsMcAlisterTripleSemigroup(T);
Error, Semigroups: IsomorphismMcAlisterTripleSemigroup: usage,
the semigroup is not E-unitary,

#T# Other McAlisterTripleSemigroup tests
gap> G := SymmetricGroup([2 .. 5]);;
gap> x := Digraph([[1], [1, 2], [1, 3], [1, 4], [1, 5]]);
<digraph with 5 vertices, 9 edges>
gap> y := Digraph([[1], [1, 2], [1, 3], [1, 4]]);
<digraph with 4 vertices, 7 edges>
gap> M := McAlisterTripleSemigroup(G, OnPoints, x, y);
<McAlister triple semigroup over Sym( [ 2 .. 5 ] )>
gap> M = McAlisterTripleSemigroup(AutomorphismGroup(x), x, y);
true
gap> M = McAlisterTripleSemigroup(G, x, x);
false
gap> IsInverseSemigroup(Semigroup(GeneratorsOfSemigroup(M)));
true
gap> elms := Enumerator(M);;
gap> String(elms[1]);
"MTE(McAlisterTripleSemigroup(SymmetricGroup( [ 2 .. 5 ] ), Digraph( [ [ 1 ], \
[ 1, 2 ], [ 1, 3 ], [ 1, 4 ], [ 1, 5 ] ] ), Digraph( [ [ 1 ], [ 1, 2 ], [ 1, 3\
 ], [ 1, 4 ] ] )), 1, ())"

#T# McAlister triple semigroup elements
gap> MTE(M, 4, (2, 4)(3, 5)) * MTE(M, 4, (2, 5, 3, 4));
(1, (4,5))
gap> MTE(M, 1, (2, 3, 4, 5)) ^ -2;
(1, (2,4)(3,5))
gap> M = MTEParent(MTE(M, 1, (4, 5)));
true
gap> M = McAlisterTripleSemigroupElementParent(MTE(M, 1, (4, 5)));
true
gap> MTE(M, 10, (2, 3, 4, 5));
Error, Semigroups: McAlisterTripleSemigroupElement: usage,
second input should be a vertex of the join semilattice of the McAlister tripl\
e,
gap> MTE(M, 1, (5, 6));
Error, Semigroups: McAlisterTripleSemigroupElement: usage,
third input must an element of the group of the McAlister triple,
gap> MTE(M, 3, (2, 4)(3, 5));
Error, Semigroups: McAlisterTripleSemigroupElement: usage,
the arguments do not specify an element of the McAlister triple semigroup,
gap> MTE(M, 1, ())[3];
Error, Semigroups: ELM_LIST (for a McAlisterTripleSemigroupElement): usage,
the index must be at most 2,
gap> M1 := McAlisterTripleSemigroup(G, x, x);;
gap> MTE(M1, 4, (2, 4)(3, 5)) * MTE(M, 4, (2, 5, 3, 4));
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
gap> M5 := McAlisterTripleSemigroup(G, act, x1, x1);;
gap> IsomorphismSemigroups(M1, M5);
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

#T# EUnitaryInverseCover
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
gap> STOP_TEST("Semigroups package: standard/eunittest.tst");
