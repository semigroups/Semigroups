#############################################################################
##
#W  standard/isorms.tst
#Y  Copyright (C) 2015-17                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/isorms.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# helper functions
gap> BruteForceIsoCheck := function(iso)
>   local x, y;
>   if not IsInjective(iso) or not IsSurjective(iso) then
>     return false;
>   fi;
>   for x in Generators(Source(iso)) do
>     for y in Generators(Source(iso)) do
>       if x ^ iso * y ^ iso <> (x * y) ^ iso then
>         return false;
>       fi;
>     od;
>   od;
>   return true;
> end;;
gap> BruteForceInverseCheck := function(map)
> local inv;
>   inv := InverseGeneralMapping(map);
>   return ForAll(Source(map), x -> x = (x ^ map) ^ inv)
>     and ForAll(Range(map), x -> x = (x ^ inv) ^ map);
> end;;

#T#
gap> R := ReesZeroMatrixSemigroup(Group([(2, 8), (2, 8, 6)]),
> [[0, (2, 8), 0, 0, 0, (2, 8, 6)],
>  [(), 0, (2, 8, 6), (2, 6), (2, 6, 8), 0],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0],
>  [0, (2, 8, 6), 0, 0, 0, (2, 8)],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0]]);;
gap> A := AutomorphismGroup(R);
<automorphism group of <Rees 0-matrix semigroup 6x6 over Group([ (2,8), (2,8,
6) ])> with 24 generators>
gap> Size(Range(IsomorphismPermGroup(A)));
82944

#T#
gap> SetInfoLevel(InfoSemigroups, 2);
gap> R := ReesZeroMatrixSemigroup(Group([(2, 8), (2, 8, 6)]),
> [[0, (2, 8), 0, 0, 0, (2, 8, 6)],
>  [(), 0, (2, 8, 6), (2, 6), (2, 6, 8), 0],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0],
>  [0, (2, 8, 6), 0, 0, 0, (2, 8)],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0]]);;
gap> A := AutomorphismGroup(R);
#I  finding automorphisms of the graph . . . 2304 found
#I  finding the stabilizer of matrix . . . 12
#I  finding the automorphism group of the group . . . found 6
#I  finding the stabilizer of matrix entries . . . 1
#I  the graph has 2 connected components
#I  backtracking in the direct product of size 2304 . . . 
<automorphism group of <Rees 0-matrix semigroup 6x6 over Group([ (2,8), (2,8,
6) ])> with 24 generators>
gap> Size(Range(IsomorphismPermGroup(A)));
82944
gap> SetInfoLevel(InfoSemigroups, 0);

#T#
gap> I := MinimalIdeal(PartitionMonoid(4));;
gap> R := Range(IsomorphismReesMatrixSemigroup(I));;
gap> G := AutomorphismGroup(R);
<automorphism group of <Rees matrix semigroup 15x15 over Group(())> with 
5 generators>
gap> G := Range(IsomorphismPermGroup(AutomorphismGroup(R)));;
gap> Size(G);
1710012252724199424000000

#T# RZMSInducedFunction with lots of connected component
gap> I := SemigroupIdeal(
>  InverseMonoid([
>    PartialPermNC([1, 2, 3, 4, 5], [2, 3, 4, 5, 1]),
>    PartialPermNC([1, 2, 3, 4, 5], [2, 1, 3, 4, 5]),
>    PartialPermNC([2, 3, 4, 5], [1, 2, 3, 4])]),
>  [PartialPermNC([1], [1])]);;
gap> R := Range(IsomorphismReesZeroMatrixSemigroup(I));
<Rees 0-matrix semigroup 5x5 over Group(())>
gap> A := AutomorphismGroup(R);
<automorphism group of <Rees 0-matrix semigroup 5x5 over Group(())> with 
5 generators>
gap> Size(A);
120
gap> ForAll(A, BruteForceIsoCheck);
true

#T# RMSInducedFunction
gap> AutomorphismGroup(RectangularBand(IsReesMatrixSemigroup, 4, 3));
<automorphism group of <Rees matrix semigroup 4x3 over Group(())> with 
5 generators>

#T# RZMSInducedFunction with one connected component
gap> R := ReesZeroMatrixSemigroup(Group([()]),
> [[(), (), (), ()], [(), (), (), ()], [(), (), (), ()]]);
<Rees 0-matrix semigroup 4x3 over Group(())>
gap> AutomorphismGroup(R);
<automorphism group of <Rees 0-matrix semigroup 4x3 over Group(())> with 
6 generators>
gap> Size(last);
144
gap> ForAll(A, BruteForceIsoCheck);
true

#T# AutomorphismGroup: for a RZMS with trivial automorphism group of graph
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2, 3), (1, 2)]),
>                                 [[(1, 3), (1, 2)], [0, (2, 3)]]);;
gap> AutomorphismGroup(R);
<automorphism group of <Rees 0-matrix semigroup 2x2 over Group([ (1,2,3), (1,
2) ])> with 4 generators>
gap> Size(last);
6

#T# AutomorphismGroup: for a RZMS over not a group
gap> S := ReesZeroMatrixSemigroup(FullTransformationMonoid(2),
>                                 [[IdentityTransformation]]);
<Rees 0-matrix semigroup 1x1 over <full transformation monoid of degree 2>>
gap> AutomorphismGroup(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `AutomorphismGroup' on 1 arguments

#T# AutomorphismGroup: for a RZMSDigraph with 0 generators
gap> func := function(n, i)
>   local out;
>   out := ListWithIdenticalEntries(n, 0);
>   out[i] := ();
>   return out;
> end;
function( n, i ) ... end
gap> mat := List([1 .. 33], i -> func(33, i));;
gap> R := ReesZeroMatrixSemigroup(Group(()), mat);
<Rees 0-matrix semigroup 33x33 over Group(())>
gap> AutomorphismGroup(R);
<automorphism group of <Rees 0-matrix semigroup 33x33 over Group(())> with 
33 generators>

#T# AutomorphismGroup: for a RMS over not a group
gap> R := ReesMatrixSemigroup(SymmetricInverseMonoid(2), [[PartialPerm([1])]]);
<Rees matrix semigroup 1x1 over <symmetric inverse monoid of degree 2>>
gap> AutomorphismGroup(R);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `AutomorphismGroup' on 1 arguments

#T# AutomorphismGroup: 1x1 RMS
gap> R := ReesMatrixSemigroup(Group(()), [[()]]);
<Rees matrix semigroup 1x1 over Group(())>
gap> A := AutomorphismGroup(R);
<automorphism group of <Rees matrix semigroup 1x1 over Group(())> with 
1 generator>
gap> ForAll(A, BruteForceIsoCheck);
true

#T# AutomorphismGroup: 1x2 and 2x1 RMS
gap> A := AutomorphismGroup(RectangularBand(IsReesMatrixSemigroup, 2, 1));
<automorphism group of <Rees matrix semigroup 2x1 over Group(())> with 
2 generators>
gap> ForAll(A, BruteForceIsoCheck);
true
gap> A := AutomorphismGroup(RectangularBand(IsReesMatrixSemigroup, 1, 2));
<automorphism group of <Rees matrix semigroup 1x2 over Group(())> with 
2 generators>
gap> ForAll(A, BruteForceIsoCheck);
true

#T# AutomorphismGroup: 1x3 and 3x1 RMS
gap> A := AutomorphismGroup(RectangularBand(IsReesMatrixSemigroup, 3, 1));
<automorphism group of <Rees matrix semigroup 3x1 over Group(())> with 
3 generators>
gap> ForAll(A, BruteForceIsoCheck);
true
gap> A := AutomorphismGroup(RectangularBand(IsReesMatrixSemigroup, 1, 3));
<automorphism group of <Rees matrix semigroup 1x3 over Group(())> with 
3 generators>
gap> ForAll(A, BruteForceIsoCheck);
true

#T# AutomorphismGroup: 33x33 RMS
gap> AutomorphismGroup(RectangularBand(IsReesMatrixSemigroup, 33, 33));
<automorphism group of <Rees matrix semigroup 33x33 over Group(())> with 
65 generators>

#T# IdentityMapping: for an RMS
gap> R := ReesMatrixSemigroup(Group([()]), [[()]]);
<Rees matrix semigroup 1x1 over Group(())>
gap> map := IdentityMapping(R);
((), IdentityMapping( Group( [ () ] ) ), [ (), () ])
gap> BruteForceIsoCheck(last);
true
gap> ForAll(R, x -> x = x ^ map);
true

#T# IsomorphismSemigroups: RMS fail
gap> R := RectangularBand(IsReesMatrixSemigroup, 2, 2);
<Rees matrix semigroup 2x2 over Group(())>
gap> S := RectangularBand(IsReesMatrixSemigroup, 2, 3);
<Rees matrix semigroup 2x3 over Group(())>
gap> IsomorphismSemigroups(R, S);
fail

#T# IsomorphismSemigroups: from RMS to itself
gap> R := RectangularBand(IsReesMatrixSemigroup, 2, 2);
<Rees matrix semigroup 2x2 over Group(())>
gap> map := IsomorphismSemigroups(R, R);
((), IdentityMapping( Group( [ () ] ) ), [ (), (), (), () ])
gap> BruteForceIsoCheck(map);
true
gap> ForAll(R, x -> x = x ^ map);
true

#T# IsomorphismSemigroups: from RMS to RMS
gap> S := ReesMatrixSemigroup(Group([(1, 2)]), [[(), ()], [(), (1, 2)]]);;
gap> R := ReesMatrixSemigroup(Group([(1, 2)]), [[(), (1, 2)], [(), ()]]);;
gap> map := IsomorphismSemigroups(R, S);
((), GroupHomomorphismByImages( Group( [ (1,2) ] ), Group( [ (1,2) ] ), 
[ (1,2) ], [ (1,2) ] ), [ (), (1,2), (), () ])
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := InverseGeneralMapping(map);
((), GroupHomomorphismByImages( Group( [ (1,2) ] ), Group( [ (1,2) ] ), 
[ (1,2) ], [ (1,2) ] ), [ (), (1,2), (), () ])
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# IsomorphismSemigroups: from RZMS to RZMS
gap> S := ReesZeroMatrixSemigroup(Group([()]), [[(), ()], [(), 0]]);;
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[(), 0], [(), ()]]);;
gap> map := IsomorphismSemigroups(R, S);
((3,4), GroupHomomorphismByImages( Group( [ () ] ), Group( [ () ] ), [  ], 
[  ] ), [ (), (), (), () ])
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := InverseGeneralMapping(map);
((3,4), GroupHomomorphismByImages( Group( () ), Group( [ () ] ), [  ], 
[  ] ), [ (), (), (), () ])
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# IsomorphismSemigroups: fail (non-regular RZMS)
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[0, 0], [(), ()]]);
<Rees 0-matrix semigroup 2x2 over Group(())>
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[(), ()], [(), (1, 2)]]);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
gap> IsomorphismSemigroups(R, S);
Error, Semigroups: IsomorphismSemigroups: usage,
the arguments must be regular Rees 0-matrix semigroups over groups,

#T# IsomorphismSemigroups: fail (different dimensions)
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[(), ()]]);;
gap> S := ReesZeroMatrixSemigroup(Group([()]), [[()], [()]]);;
gap> IsomorphismSemigroups(R, S);
fail

#T# IsomorphismSemigroups: from RZMS to itself
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[(), ()]]);;
gap> map := IsomorphismSemigroups(R, R);
((), IdentityMapping( Group( [ () ] ) ), [ (), (), () ])
gap> BruteForceIsoCheck(map);
true
gap> ForAll(R, x -> x = x ^ map);
true
gap> BruteForceInverseCheck(map);
true

#T# IsomorphismSemigroups: fail (non-isomorphic groups)
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2, 3, 4)]), [[(), ()]]);;
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2), (3, 4)]), [[(), ()]]);;
gap> IsomorphismSemigroups(R, S);
fail

#T# IsomorphismSemigroups: fail (non-isomorphic graphs)
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[(), ()], [(), ()]]);;
gap> S := ReesZeroMatrixSemigroup(Group([()]), [[(), 0], [(), ()]]);;
gap> IsomorphismSemigroups(R, S);
fail

#T# IsomorphismSemigroups: fail (no map found)
gap> mat1 := [[(), ()], [(), ()]];;
gap> mat2 := [[(), ()], [(), (1, 2)]];;
gap> R1 := ReesZeroMatrixSemigroup(Group((1, 2)), mat1);;
gap> R2 := ReesZeroMatrixSemigroup(Group((1, 2)), mat2);;
gap> IsomorphismSemigroups(R1, R2);
fail

#T# IsomorphismSemigroups: non-trivial isomorphism 1/2
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[(), 0], [0, ()]]);;
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[0, ()], [(1, 2), 0]]);;
gap> map := IsomorphismSemigroups(R, S);
((3,4), GroupHomomorphismByImages( Group( [ (1,2) ] ), Group( [ (1,2) ] ), 
[ (1,2) ], [ (1,2) ] ), [ (), (), (1,2), () ])
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := InverseGeneralMapping(map);
((3,4), GroupHomomorphismByImages( Group( [ (1,2) ] ), Group( [ (1,2) ] ), 
[ (1,2) ], [ (1,2) ] ), [ (), (), (), (1,2) ])
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# IsomorphismSemigroups: non-trivial isomorphism 2/2
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2, 3), (1, 2)]),
>                                 [[0, (1, 2, 3)], [(1, 3, 2), ()]]);;
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2, 3), (1, 2)]),
>                                 [[(1, 3), (1, 2)], [0, (2, 3)]]);;
gap> map := IsomorphismSemigroups(R, S);
((3,4), GroupHomomorphismByImages( Group( [ (1,2,3), (1,2) ] ), Group( 
[ (1,2,3), (1,2) ] ), [ (1,2,3), (1,2) ], [ (1,2,3), (1,2) ] ), 
[ (), (1,2,3), (2,3), (1,2) ])
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := InverseGeneralMapping(map);
((3,4), GroupHomomorphismByImages( Group( [ (1,2,3), (1,2) ] ), Group( 
[ (1,2,3), (1,2) ] ), [ (1,2,3), (1,2) ], [ (1,2,3), (1,2) ] ), 
[ (), (1,3,2), (1,2), (2,3) ])
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

#T# SEMIGROUPS.RZMStoRZMSInducedFunction: error, 1/1
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[(), 0], [0, ()]]);;
gap> SEMIGROUPS.RZMStoRZMSInducedFunction(R, R, fail, fail, [1]);
Error, Semigroups: SEMIGROUPS.RZMStoRZMSInducedFunction: usage,
the 5th argument must be a list of length 2,

#T# \=: RMS and RMS elements
gap> R := RectangularBand(IsReesMatrixSemigroup, 2, 2);
<Rees matrix semigroup 2x2 over Group(())>
gap> G := AutomorphismGroup(R);
<automorphism group of <Rees matrix semigroup 2x2 over Group(())> with 
3 generators>
gap> G.1 = G.1;
true
gap> G.1 = G.2;
false
gap> One(G) = One(G.1);
true

#T# \=: RMS and RMS elements
gap> R := RectangularBand(IsReesMatrixSemigroup, 2, 2);
<Rees matrix semigroup 2x2 over Group(())>
gap> S := RectangularBand(IsReesMatrixSemigroup, 2, 2);
<Rees matrix semigroup 2x2 over Group(())>
gap> G := AutomorphismGroup(R); H := AutomorphismGroup(S);
<automorphism group of <Rees matrix semigroup 2x2 over Group(())> with 
3 generators>
<automorphism group of <Rees matrix semigroup 2x2 over Group(())> with 
3 generators>
gap> G.1 = H.1;
false
gap> G.1 * G.2;
((3,4), GroupHomomorphismByImages( Group( [ () ] ), Group( [ () ] ), [ () ], 
[ () ] ), [ (), (), (), () ])
gap> R.1 ^ G.1;
(1,(),1)
gap> ImagesElm(G.2, R.1);
[ (1,(),2) ]
gap> G.1 ^ -1;
((), IdentityMapping( Group( [ () ] ) ), [ (), (), (), () ])
gap> IsOne(G.1);
true
gap> IsOne(One(G.1 * G.2));
true
gap> IsOne(G.1 * G.2);
false
gap> Print(G.2); true;
RMSIsoByTriple ( ReesMatrixSemigroup( Group( [ () ] ), 
[ [ (), () ], [ (), () ] ] ), ReesMatrixSemigroup( Group( [ () ] ), 
[ [ (), () ], [ (), () ] ] ), [(3,4), GroupHomomorphismByImages( Group( 
[ () ] ), Group( [ () ] ), [ () ], [ () ] ), [ (), (), (), () ]])true
gap> PreImagesRepresentative(G.2, R.1);
(1,(),2)
gap> G.1 ^ -1;
IdentityMapping( <Rees matrix semigroup 2x2 over Group(())> )

#T# \=: RZMS and RZMS elements 1/2
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2, 3)]),
> [[(1, 2, 3), 0], [0, (1, 2, 3)]]);;
gap> G := AutomorphismGroup(R);
<automorphism group of <Rees 0-matrix semigroup 2x2 over Group([ (1,2,
3) ])> with 5 generators>
gap> map := RZMSIsoByTriple(R, R,
> [(1, 2)(3, 4), IdentityMapping(Group((1, 2, 3))), [(), (), (), ()]]);
((1,2)(3,4), IdentityMapping( Group( [ (1,2,3) ] ) ), [ (), (), (), () ])
gap> map in G;
true
gap> CompositionMapping2(G.1, G.2);
((), IdentityMapping( Group( [ (1,2,3) ] ) ), [ (), (1,3,2), (), (1,3,2) ])
gap> R.1 ^ G.1;
(1,(),1)
gap> G.1 ^ -1;
((), IdentityMapping( Group( [ (1,2,3) ] ) ), [ (), (), (), () ])
gap> IsOne(G.1);
true
gap> IsOne(One(G.1 * G.2));
true
gap> Print(G.1); true;
IdentityMapping( ReesZeroMatrixSemigroup( Group( [ (1,2,3) ] ), 
[ [ (1,2,3), 0 ], [ 0, (1,2,3) ] ] ) )true

#T# \=: RZMS and RZMS elements 2/2
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2, 3)]),
> [[(1, 2, 3), 0], [0, (1, 2, 3)]]);;
gap> G := AutomorphismGroup(R);
<automorphism group of <Rees 0-matrix semigroup 2x2 over Group([ (1,2,
3) ])> with 5 generators>
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2, 3), (1, 2)]),
> [[0, (1, 2, 3)], [(1, 3, 2), ()]]);;
gap> H := AutomorphismGroup(S);
<automorphism group of <Rees 0-matrix semigroup 2x2 over Group([ (1,2,3), (1,
2) ])> with 4 generators>
gap> G.1 = H.1;
false
gap> CompositionMapping2(G.1, G.2);
((), IdentityMapping( Group( [ (1,2,3) ] ) ), [ (), (1,3,2), (), (1,3,2) ])
gap> R.1 ^ G.1;
(1,(),1)
gap> G.1 ^ -1;
((), IdentityMapping( Group( [ (1,2,3) ] ) ), [ (), (), (), () ])
gap> IsOne(G.1);
true
gap> IsOne(One(G.1 * G.2));
true
gap> Print(G.1); true;
IdentityMapping( ReesZeroMatrixSemigroup( Group( [ (1,2,3) ] ), 
[ [ (1,2,3), 0 ], [ 0, (1,2,3) ] ] ) )true
gap> PreImagesRepresentative(G.2, R.1);
(1,(),1)
gap> ImagesElm(G.2, R.1);
[ (1,(),1) ]

# Issue #167 (part 1), problem with IsomorphismSemigroups for RMS and RZMS when
# one of the arguments was did not satisfy IsWholeFamily
gap> R := ReesMatrixSemigroup(Group(()), [[(), ()], [(), ()]]);;
gap> W := Semigroup(RMSElement(R, 2, (), 2));;
gap> S := ReesMatrixSemigroup(Group(()), [[()]]);;
gap> IsTrivial(S) and IsTrivial(W);
true
gap> IsomorphismSemigroups(S, S);
((), IdentityMapping( Group( [ () ] ) ), [ (), () ])
gap> IsomorphismSemigroups(W, W);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `IsomorphismSemigroups' on 2 arguments
gap> IsomorphismSemigroups(S, W);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `IsomorphismSemigroups' on 2 arguments
gap> IsomorphismSemigroups(W, S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `IsomorphismSemigroups' on 2 arguments
gap> map := IsomorphismReesMatrixSemigroup(W);;
gap> WW := Range(map);
<Rees matrix semigroup 1x1 over Group(())>
gap> IsomorphismSemigroups(S, WW);
((), GroupHomomorphismByImages( Group( [ () ] ), Group( [ () ] ), [  ], 
[  ] ), [ (), () ])
gap> IsomorphismSemigroups(WW, S);
((), GroupHomomorphismByImages( Group( [ () ] ), Group( [ () ] ), [  ], 
[  ] ), [ (), () ])

# Issue #167 (part 2)
gap> G1 := SymmetricGroup(IsPermGroup, 2);;
gap> R1 := ReesMatrixSemigroup(G1, [[Identity(G1)]]);;
gap> G2 := SymmetricGroup(IsPcGroup, 2);;
gap> R2 := ReesMatrixSemigroup(G2, [[Identity(G2)]]);;
gap> map := IsomorphismSemigroups(R1, R2);
((), GroupHomomorphismByImages( SymmetricGroup( [ 1 .. 2 ] ), Group( 
[ f1 ] ), [ (1,2) ], [ f1 ] ), [ <identity> of ..., <identity> of ... ])
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := InverseGeneralMapping(map);
((), Pcgs([ f1 ]) -> [ (1,2) ], [ (), () ])
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# Issue #167 (part 3)
gap> R := ReesMatrixSemigroup(Group(()), [[()]]);
<Rees matrix semigroup 1x1 over Group(())>
gap> iso := IsomorphismSemigroups(R, R);
((), IdentityMapping( Group( [ () ] ) ), [ (), () ])
gap> inv := InverseGeneralMapping(iso);
((), IdentityMapping( Group( [ () ] ) ), [ (), () ])
gap> Representative(R) ^ inv;
(1,(),1)

# Issue #167 (part 4)
gap> G1 := Group([(2, 4), (1, 2)]);;
gap> mat1 := [[(1, 2), (2, 4)]];;
gap> R1 := ReesMatrixSemigroup(G1, mat1);;
gap> G2 := Group([(1, 2)(3, 6)(4, 5), (1, 3, 5)(2, 4, 6)]);;
gap> mat2 := [[(1, 2)(3, 6)(4, 5), (1, 5, 3)(2, 6, 4)]];;
gap> R2 := ReesMatrixSemigroup(G2, mat2);;
gap> map := IsomorphismSemigroups(R1, R2);
((), GroupHomomorphismByImages( Group( [ (2,4), (1,2) ] ), Group( 
[ (1,2)(3,6)(4,5), (1,3,5)(2,4,6) ] ), [ (2,4), (1,2) ], 
[ (1,2)(3,6)(4,5), (1,4)(2,3)(5,6) ] ), [ (), (1,4)(2,3)(5,6), (1,3,5)(2,4,6) 
 ])
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := InverseGeneralMapping(map);
((), GroupHomomorphismByImages( Group( [ (1,2)(3,6)(4,5), (1,4)(2,3)(5,6) 
 ] ), Group( [ (2,4), (1,2) ] ), [ (1,2)(3,6)(4,5), (1,4)(2,3)(5,6) ], 
[ (2,4), (1,2) ] ), [ (), (1,2), (1,4,2) ])
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# Issue #167 (part 5)
gap> R := ReesMatrixSemigroup(Group(()), [[(), ()], [(), ()]]);;
gap> W := Semigroup(RMSElement(R, 2, (), 2));;
gap> S := ReesMatrixSemigroup(Group(()), [[()]]);;
gap> norm := InverseGeneralMapping(IsomorphismReesMatrixSemigroup(W));;
gap> iso := IsomorphismSemigroups(S, Source(norm));;
gap> comp := CompositionMapping(norm, iso);;
gap> inv := InverseGeneralMapping(comp);;
gap> Representative(S) ^ comp = Representative(W);
true
gap> Representative(W) ^ inv;
(1,(),1)

# Check inverses work
gap> R := ReesZeroMatrixSemigroup(Group([()]),
> [[(), 0, (), 0], [0, (), 0, ()]]);;
gap> A := AutomorphismGroup(R);
<automorphism group of <Rees 0-matrix semigroup 4x2 over Group(())> with 
3 generators>
gap> Size(A);
8
gap> ForAll(A, x -> x * x ^ -1 = One(A));
true
gap> ForAll(A, x -> x ^ -1 * x = One(A));
true
gap> ForAll(A, BruteForceIsoCheck);
true
gap> ForAll(A, BruteForceInverseCheck);
true

#T# Errors in checked version of RMSIsoByTriple
gap> g := SymmetricGroup(4);;
gap> mat := [[(1, 3), (1, 2)(3, 4)],
>            [(1, 4, 3, 2), ()],
>            [(1, 3)(2, 4), (1, 3, 4, 2)]];;
gap> R := ReesMatrixSemigroup(g, mat);;
gap> S := ReesMatrixSemigroup(Group((1, 2)), [[()]]);;
gap> auto := IdentityMapping(g);;
gap> g_elms_list := [(), (1, 3), (), (), ()];;
gap> RMSIsoByTriple(R, S, [(), auto, g_elms_list]);
Error, Semigroups: RMSIsoByTriple:
<R1> and <R2> are not isomorphic,
gap> RMSIsoByTriple(R, R, [42, auto, g_elms_list]);
Error, Semigroups: RMSIsoByTriple: usage,
<triple>[1] should be a permutation,
gap> RMSIsoByTriple(R, R, [(1, 7), auto, g_elms_list]);
Error, Semigroups: RMSIsoByTriple: usage,
<triple>[1] should be a permutation on [1 .. 5],
gap> RMSIsoByTriple(R, R, [(1, 4), auto, g_elms_list]);
Error, Semigroups: RMSIsoByTriple: usage,
<triple>[1] should not map columns to rows,
gap> RMSIsoByTriple(R, R, [(), fail, g_elms_list]);
Error, Semigroups: RMSIsoByTriple: usage,
<triple>[2] should be an isomorphism from
the underlying group of <R1> to that of <R2>,
gap> RMSIsoByTriple(R, R, [(), auto, [(), (), ()]]);
Error, Semigroups: RMSIsoByTriple: usage,
<triple>[3] should have length equal to
the number of rows and columns of <R1>,
gap> RMSIsoByTriple(R, R, [(), auto, [42, 43, 44, 45, 46]]);
Error, Semigroups: RMSIsoByTriple: usage,
<triple>[3] should only contain elements from the underlying group of <R2>,
gap> RMSIsoByTriple(R, R, [(), auto, g_elms_list]);
Error, Semigroups: RMSIsoByTriple: usage,
<triple>[3] does not define an isomorphism,
gap> iso := RMSIsoByTripleNC(R, R, [(), auto, g_elms_list]);;
gap> BruteForceIsoCheck(iso);
false
gap> g_elms_list := [(), (), (), (), ()];;
gap> iso := RMSIsoByTriple(R, R, [(), auto, g_elms_list]);
((), IdentityMapping( SymmetricGroup( [ 1 .. 4 ] ) ), [ (), (), (), (), () ])
gap> BruteForceIsoCheck(iso);
true

#T# Errors in checked version of RZMSIsoByTriple
gap> g := SymmetricGroup(3);;
gap> mat := [[0, 0, (1, 3)], [(1, 2, 3), (), (2, 3)], [0, 0, ()]];;
gap> R := ReesZeroMatrixSemigroup(g, mat);;
gap> S := ReesZeroMatrixSemigroup(Group((1, 2)), [[()]]);;
gap> auto := IdentityMapping(g);;
gap> g_elms_list := [(), (1, 3), (), (), (), ()];;
gap> RZMSIsoByTriple(R, S, [(), auto, g_elms_list]);
Error, Semigroups: RZMSIsoByTriple:
<R1> and <R2> are not isomorphic,
gap> RZMSIsoByTriple(R, R, [42, auto, g_elms_list]);
Error, Semigroups: RZMSIsoByTriple: usage,
<triple>[1] should be a permutation,
gap> RZMSIsoByTriple(R, R, [(1, 3), auto, g_elms_list]);
Error, Semigroups: RZMSIsoByTriple: usage,
<triple>[1] should act as an isomorphism from
the graph of <R1> to the graph of <R2>,
gap> RZMSIsoByTriple(R, R, [(), fail, g_elms_list]);
Error, Semigroups: RZMSIsoByTriple: usage,
<triple>[2] should be an isomorphism from
the underlying group of <R1> to that of <R2>,
gap> RZMSIsoByTriple(R, R, [(), auto, [(), (), ()]]);
Error, Semigroups: RZMSIsoByTriple: usage,
<triple>[3] should have length equal to
the number of rows and columns of <R1>,
gap> RZMSIsoByTriple(R, R, [(), auto, [41, 42, 43, 44, 45, 46]]);
Error, Semigroups: RZMSIsoByTriple: usage,
<triple>[3] should only contain elements from the underlying group of <R2>,
gap> RZMSIsoByTriple(R, R, [(), auto, g_elms_list]);
Error, Semigroups: RZMSIsoByTriple: usage,
<triple>[3] does not define an isomorphism,
gap> iso := RZMSIsoByTripleNC(R, R, [(), auto, g_elms_list]);;
gap> BruteForceIsoCheck(iso);
false
gap> g_elms_list := [(), (), (), (), (), ()];;
gap> iso := RZMSIsoByTriple(R, R, [(), auto, g_elms_list]);
((), IdentityMapping( SymmetricGroup( [ 1 .. 3 ] ) ), 
[ (), (), (), (), (), () ])
gap> BruteForceIsoCheck(iso);
true

#T# IsomorphismRees(Zero)MatrixSemigroupOverPermGroup
gap> S := FullTransformationMonoid(3);;
gap> IsomorphismReesMatrixSemigroupOverPermGroup(S);
Error, Semigroups: IsomorphismReesMatrixSemigroupOverPermGroup: usage,
the argument must be a finite simple semigroup,
gap> IsomorphismReesZeroMatrixSemigroupOverPermGroup(S);
Error, Semigroups: IsomorphismReesZeroMatrixSemigroupOverPermGroup: usage,
the argument must be a finite 0-simple semigroup,
gap> G := SymmetricGroup(2);;
gap> R := ReesMatrixSemigroup(G, [[G.1, G.1]]);
<Rees matrix semigroup 2x1 over Sym( [ 1 .. 2 ] )>
gap> iso := IsomorphismReesMatrixSemigroupOverPermGroup(R);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> S := Semigroup(Representative(R));;
gap> iso := IsomorphismReesMatrixSemigroupOverPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> G := AllSmallGroups(8)[3];;
gap> R := ReesMatrixSemigroup(G, [[G.1, G.1]]);
<Rees matrix semigroup 2x1 over <pc group of size 8 with 3 generators>>
gap> iso := IsomorphismReesMatrixSemigroupOverPermGroup(R);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> S := Semigroup(Representative(R));;
gap> iso := IsomorphismReesMatrixSemigroupOverPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> G := SymmetricGroup(2);;
gap> R := ReesZeroMatrixSemigroup(G, [[(), 0, 0], [0, (), 0], [0, 0, ()]]);
<Rees 0-matrix semigroup 3x3 over Sym( [ 1 .. 2 ] )>
gap> iso := IsomorphismReesZeroMatrixSemigroupOverPermGroup(R);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> S := Semigroup(RMSElement(R, 2, (), 1), RMSElement(R, 1, (), 2));;
gap> iso := IsomorphismReesZeroMatrixSemigroupOverPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> G := AllSmallGroups(8)[3];;
gap> x := One(G);;
gap> R := ReesZeroMatrixSemigroup(G, [[x, 0, 0], [0, x, 0], [0, 0, x]]);
<Rees 0-matrix semigroup 3x3 over <pc group of size 8 with 3 generators>>
gap> iso := IsomorphismReesZeroMatrixSemigroupOverPermGroup(R);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> S := Semigroup(RMSElement(R, 1, x, 2), RMSElement(R, 2, x, 1));;
gap> iso := IsomorphismReesZeroMatrixSemigroupOverPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> R := ReesZeroMatrixSemigroup(G, [[G.1, 0, 0], [0, G.1, 0], [0, 0, G.1]]);
<Rees 0-matrix semigroup 3x3 over <pc group of size 8 with 3 generators>>
gap> S := Semigroup(RMSElement(R, 1, x, 2), RMSElement(R, 2, x, 1));;
gap> iso := IsomorphismReesZeroMatrixSemigroupOverPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(A);
gap> Unbind(BruteForceInverseCheck);
gap> Unbind(BruteForceIsoCheck);
gap> Unbind(G);
gap> Unbind(G1);
gap> Unbind(G2);
gap> Unbind(H);
gap> Unbind(I);
gap> Unbind(R);
gap> Unbind(R1);
gap> Unbind(R2);
gap> Unbind(S);
gap> Unbind(W);
gap> Unbind(WW);
gap> Unbind(auto);
gap> Unbind(comp);
gap> Unbind(func);
gap> Unbind(g);
gap> Unbind(inv);
gap> Unbind(iso);
gap> Unbind(list);
gap> Unbind(map);
gap> Unbind(mat);
gap> Unbind(mat1);
gap> Unbind(mat2);
gap> Unbind(norm);
gap> Unbind(x);
gap> Unbind(y);
gap> Unbind(BruteForceInverseCheck);
gap> Unbind(BruteForceIsoCheck);

#E#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/isorms.tst");
