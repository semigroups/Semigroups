#############################################################################
##
#W  standard/attributes/isorms.tst
#Y  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local A, B, BruteForceInverseCheck, BruteForceIsoCheck, G, G1, G2, H, I, R
#@local R1, R2, S, T, U, UU, V, W, WW, auto, comp, func, g, g_elms_list, id
#@local inv, iso, map, mat, mat1, mat2, norm, out, x, y
gap> START_TEST("Semigroups package: standard/attributes/isorms.tst");
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

#
gap> R := ReesZeroMatrixSemigroup(Group([(2, 8), (2, 8, 6)]),
> [[0, (2, 8), 0, 0, 0, (2, 8, 6)],
>  [(), 0, (2, 8, 6), (2, 6), (2, 6, 8), 0],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0],
>  [0, (2, 8, 6), 0, 0, 0, (2, 8)],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0]]);;
gap> A := AutomorphismGroup(R);;
gap> Print(A.1); "string to test printing";
RZMSIsoByTriple ( ReesZeroMatrixSemigroup( Group( [ (2,8), (2,8,6) ] ), 
[ [ 0, (2,8), 0, 0, 0, (2,8,6) ], [ (), 0, (2,8,6), (2,6), (2,6,8), 0 ], 
  [ (2,8,6), 0, (2,6,8), (2,8), (), 0 ], [ (2,8,6), 0, (2,6,8), (2,8), (), 0 ]
    , [ 0, (2,8,6), 0, 0, 0, (2,8) ], [ (2,8,6), 0, (2,6,8), (2,8), (), 0 ] 
 ] ), ReesZeroMatrixSemigroup( Group( [ (2,8), (2,8,6) ] ), 
[ [ 0, (2,8), 0, 0, 0, (2,8,6) ], [ (), 0, (2,8,6), (2,6), (2,6,8), 0 ], 
  [ (2,8,6), 0, (2,6,8), (2,8), (), 0 ], [ (2,8,6), 0, (2,6,8), (2,8), (), 0 ]
    , [ 0, (2,8,6), 0, 0, 0, (2,8) ], [ (2,8,6), 0, (2,6,8), (2,8), (), 0 ] 
 ] ), (), IdentityMapping( Group( [ (2,8), (2,8,6) ] ) ), 
[ (2,6), (6,8), (2,8), (2,6), (6,8), (2,8), (2,6), (2,6), (6,8), (6,8), 
  (2,8), (6,8) ] )"string to test printing"
gap> Size(Range(IsomorphismPermGroup(A)));
82944

#
gap> SetInfoLevel(InfoSemigroups, 2);
gap> R := ReesZeroMatrixSemigroup(Group([(2, 8), (2, 8, 6)]),
> [[0, (2, 8), 0, 0, 0, (2, 8, 6)],
>  [(), 0, (2, 8, 6), (2, 6), (2, 6, 8), 0],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0],
>  [0, (2, 8, 6), 0, 0, 0, (2, 8)],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0]]);;
gap> A := AutomorphismGroup(R);;
#I  the graph has 2304 automorphisms
#I  the size of stabilizer of the matrix is 12
#I  the underlying group has 6 automorphisms
#I  the size of the stabilizer of the matrix entries is 1
#I  the graph has 2 connected components
#I  backtracking in the direct product of size 2304 . . . 
#I  found subgroup of size 2304
gap> Size(Range(IsomorphismPermGroup(A)));
82944
gap> SetInfoLevel(InfoSemigroups, 0);

#
gap> I := MinimalIdeal(PartitionMonoid(4));;
gap> R := Range(IsomorphismReesMatrixSemigroup(I));;
gap> G := AutomorphismGroup(R);
<automorphism group of <Rees matrix semigroup 15x15 over Group(())> with 
5 generators>
gap> G := Range(IsomorphismPermGroup(AutomorphismGroup(R)));;
gap> Size(G);
1710012252724199424000000

# RZMSInducedFunction with lots of connected component
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

# RMSInducedFunction
gap> AutomorphismGroup(RectangularBand(IsReesMatrixSemigroup, 4, 3));
<automorphism group of <Rees matrix semigroup 4x3 over Group(())> with 
5 generators>

# RZMSInducedFunction with one connected component
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

# AutomorphismGroup: for a RZMS with trivial automorphism group of graph
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2, 3), (1, 2)]),
>                                 [[(1, 3), (1, 2)], [0, (2, 3)]]);;
gap> A := AutomorphismGroup(R);;
gap> Size(A);
6
gap> IsAbelian(A);
false

# AutomorphismGroup: for a RZMS over not a group
gap> S := ReesZeroMatrixSemigroup(FullTransformationMonoid(2),
>                                 [[IdentityTransformation]]);
<Rees 0-matrix semigroup 1x1 over <full transformation monoid of degree 2>>
gap> G := AutomorphismGroup(S);;
gap> IsCyclic(G);
true

# AutomorphismGroup: for a RZMSDigraph with 0 generators
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

# AutomorphismGroup: for a RMS over not a group
gap> R := ReesMatrixSemigroup(SymmetricInverseMonoid(2), [[PartialPerm([1])]]);
<Rees matrix semigroup 1x1 over <symmetric inverse monoid of degree 2>>
gap> GeneratorsOfSemigroup(R);;
gap> G := AutomorphismGroup(R);;
gap> IsCyclic(G);
true

# AutomorphismGroup: 1x1 RMS
gap> R := ReesMatrixSemigroup(Group(()), [[()]]);
<Rees matrix semigroup 1x1 over Group(())>
gap> A := AutomorphismGroup(R);
<automorphism group of <Rees matrix semigroup 1x1 over Group(())> with 
1 generator>
gap> ForAll(A, BruteForceIsoCheck);
true

# AutomorphismGroup: 1x2 and 2x1 RMS
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

# AutomorphismGroup: 1x3 and 3x1 RMS
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

# AutomorphismGroup: 33x33 RMS
gap> A := AutomorphismGroup(RectangularBand(IsReesMatrixSemigroup, 33, 33));
<automorphism group of <Rees matrix semigroup 33x33 over Group(())> with 
65 generators>

# AutomorphismGroup: RMS over non-trivial group
gap> B := AutomorphismGroup(ReesMatrixSemigroup(SymmetricGroup(3), [[(), ()]]));
<automorphism group of <Rees matrix semigroup 2x1 over Sym( [ 1 .. 3 ] )>
  with 7 generators>

# \< for triples over different RMS
gap> A.1 < B.2;
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `<' on 2 arguments

# Codecoverage
gap> S := 
> ReesMatrixSemigroup(Group([(1, 2)(3, 118)(4, 117)(5, 116)(6, 115)(7, 114)(8, 113)(9, 112)
>     (10, 111)(11, 110)(12, 109)(13, 108)(14, 107)(15, 106)(16, 105)(17, 104)(18, 103)(19, 102)(20, 101)
>     (21, 100)(22, 99)(23, 98)(24, 97)(25, 96)(26, 95)(27, 94)(28, 93)(29, 92)(30, 91)(31, 90)
>     (32, 89)(33, 88)(34, 87)(35, 86)(36, 85)(37, 84)(38, 83)(39, 82)(40, 81)(41, 80)(42, 79)
>     (43, 78)(44, 77)(45, 76)(46, 75)(47, 74)(48, 73)(49, 72)(50, 71)(51, 70)(52, 69)(53, 68)
>     (54, 67)(55, 66)(56, 65)(57, 64)(58, 63)(59, 62)(60, 61),
>   (1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49,
>       51, 53, 55, 57, 59, 61, 63, 65, 67, 69, 71, 73, 75, 77, 79, 81, 83, 85, 87, 89, 91, 93, 95, 97, 99,
>      101, 103, 105, 107, 109, 111, 113, 115, 117)(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32,
>      34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76, 78, 80, 82,
>       84, 86, 88, 90, 92, 94, 96, 98, 100, 102, 104, 106, 108, 110, 112, 114, 116, 118)]),
> [[(), ()],
>   [(), (1, 27, 53, 79, 105, 13, 39, 65, 91, 117, 25, 51, 77, 103, 11, 37, 63, 89, 115, 23, 49, 75, 101, 9,
>           35, 61, 87, 113, 21, 47, 73, 99, 7, 33, 59, 85, 111, 19, 45, 71, 97, 5, 31, 57, 83, 109, 17, 43,
>           69, 95, 3, 29, 55, 81, 107, 15, 41, 67, 93)(2, 28, 54, 80, 106, 14, 40, 66, 92, 118, 26, 52, 78,
>          104, 12, 38, 64, 90, 116, 24, 50, 76, 102, 10, 36, 62, 88, 114, 22, 48, 74, 100, 8, 34, 60, 86, 112,
>           20, 46, 72, 98, 6, 32, 58, 84, 110, 18, 44, 70, 96, 4, 30, 56, 82, 108, 16, 42, 68, 94)],
>   [(), (1, 82)(2, 81)(3, 80)(4, 79)(5, 78)(6, 77)(7, 76)(8, 75)(9, 74)(10, 73)
>         (11, 72)(12, 71)(13, 70)(14, 69)(15, 68)(16, 67)(17, 66)(18, 65)(19, 64)(20, 63)
>         (21, 62)(22, 61)(23, 60)(24, 59)(25, 58)(26, 57)(27, 56)(28, 55)(29, 54)(30, 53)
>         (31, 52)(32, 51)(33, 50)(34, 49)(35, 48)(36, 47)(37, 46)(38, 45)(39, 44)(40, 43)
>         (41, 42)(83, 118)(84, 117)(85, 116)(86, 115)(87, 114)(88, 113)(89, 112)(90, 111)(91, 110)
>         (92, 109)(93, 108)(94, 107)(95, 106)(96, 105)(97, 104)(98, 103)(99, 102)(100, 101)]]);
<Rees matrix semigroup 2x3 over <permutation group with 2 generators>>
gap> AutomorphismGroup(S);
<automorphism group of <Rees matrix semigroup 2x3 over 
  <permutation group of size 118 with 2 generators>> with 3 generators>

# IdentityMapping: for an RMS
gap> R := ReesMatrixSemigroup(Group([()]), [[()]]);
<Rees matrix semigroup 1x1 over Group(())>
gap> map := IdentityMapping(R);
((), IdentityMapping( Group( [ () ] ) ), [ (), () ])
gap> BruteForceIsoCheck(last);
true
gap> ForAll(R, x -> x = x ^ map);
true

# IsomorphismSemigroups: RMS fail
gap> R := RectangularBand(IsReesMatrixSemigroup, 2, 2);
<Rees matrix semigroup 2x2 over Group(())>
gap> S := RectangularBand(IsReesMatrixSemigroup, 2, 3);
<Rees matrix semigroup 2x3 over Group(())>
gap> IsomorphismSemigroups(R, S);
fail

# IsomorphismSemigroups: from RMS to itself
gap> R := RectangularBand(IsReesMatrixSemigroup, 2, 2);
<Rees matrix semigroup 2x2 over Group(())>
gap> map := IsomorphismSemigroups(R, R);
((), IdentityMapping( Group( [ () ] ) ), [ (), (), (), () ])
gap> BruteForceIsoCheck(map);
true
gap> ForAll(R, x -> x = x ^ map);
true

# IsomorphismSemigroups: from RMS to RMS
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

# IsomorphismSemigroups: from RZMS to RZMS
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
((3,4), GroupHomomorphismByImages( Group( [ () ] ), Group( [ () ] ), [  ], 
[  ] ), [ (), (), (), () ])
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# IsomorphismSemigroups: fail (non-regular RZMS)
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[0, 0], [(), ()]]);
<Rees 0-matrix semigroup 2x2 over Group(())>
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[(), ()], [(), (1, 2)]]);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
gap> IsomorphismSemigroups(R, S);
fail

# IsomorphismSemigroups: fail (different dimensions)
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[(), ()]]);;
gap> S := ReesZeroMatrixSemigroup(Group([()]), [[()], [()]]);;
gap> IsomorphismSemigroups(R, S);
fail

# IsomorphismSemigroups: from RZMS to itself
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[(), ()]]);;
gap> map := IsomorphismSemigroups(R, R);
((), IdentityMapping( Group( [ () ] ) ), [ (), (), () ])
gap> BruteForceIsoCheck(map);
true
gap> ForAll(R, x -> x = x ^ map);
true
gap> BruteForceInverseCheck(map);
true

# IsomorphismSemigroups: fail (non-isomorphic groups)
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2, 3, 4)]), [[(), ()]]);;
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2), (3, 4)]), [[(), ()]]);;
gap> IsomorphismSemigroups(R, S);
fail

# IsomorphismSemigroups: fail (non-isomorphic graphs)
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[(), ()], [(), ()]]);;
gap> S := ReesZeroMatrixSemigroup(Group([()]), [[(), 0], [(), ()]]);;
gap> IsomorphismSemigroups(R, S);
fail

# IsomorphismSemigroups: fail (no map found)
gap> mat1 := [[(), ()], [(), ()]];;
gap> mat2 := [[(), ()], [(), (1, 2)]];;
gap> R1 := ReesZeroMatrixSemigroup(Group((1, 2)), mat1);;
gap> R2 := ReesZeroMatrixSemigroup(Group((1, 2)), mat2);;
gap> IsomorphismSemigroups(R1, R2);
fail

# IsomorphismSemigroups: non-trivial isomorphism 1/2
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

# IsomorphismSemigroups: non-trivial isomorphism 2/2
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

# SEMIGROUPS.RZMStoRZMSInducedFunction: error, 1/1
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[(), 0], [0, ()]]);;
gap> SEMIGROUPS.RZMStoRZMSInducedFunction(R, R, fail, fail, [1]);
Error, the 5th argument (a list) must have length 2, but found 1

# \=: RMS and RMS elements
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

# \=: RMS and RMS elements
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

# \=: RZMS and RZMS elements 1/2
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2, 3)]),
> [[(1, 2, 3), 0], [0, (1, 2, 3)]]);;
gap> G := AutomorphismGroup(R);;
gap> Length(GeneratorsOfSemigroup(G));
5
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

# \=: RZMS and RZMS elements 2/2
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2, 3)]),
> [[(1, 2, 3), 0], [0, (1, 2, 3)]]);;
gap> G := AutomorphismGroup(R);;
gap> Length(GeneratorsOfSemigroup(G));
5
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2, 3), (1, 2)]),
> [[0, (1, 2, 3)], [(1, 3, 2), ()]]);;
gap> H := AutomorphismGroup(S);;
gap> Length(GeneratorsOfSemigroup(H));
4
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

# Issue #167 (part 1), problem with IsomorphismSemigroups for RMS and RZMS
# when one of the arguments was did not satisfy IsWholeFamily
gap> R := ReesMatrixSemigroup(Group(()), [[(), ()], [(), ()]]);;
gap> W := Semigroup(RMSElement(R, 2, (), 2));;
gap> S := ReesMatrixSemigroup(Group(()), [[()]]);;
gap> IsTrivial(S) and IsTrivial(W);
true
gap> map := IsomorphismSemigroups(S, S);
((), IdentityMapping( Group( [ () ] ) ), [ (), () ])
gap> map := IsomorphismSemigroups(W, W);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroups(W, S);
CompositionMapping( ((), GroupHomomorphismByImages( Group( [ () ] ), Group( 
[ () ] ), [  ], [  ] ), [ (), () ]), 
<Rees matrix semigroup 1x1 over Group(())> -> 
<Rees matrix semigroup 1x1 over Group(())> )
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismReesMatrixSemigroup(W);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> WW := Range(map);
<Rees matrix semigroup 1x1 over Group(())>
gap> map := IsomorphismSemigroups(S, WW);
((), GroupHomomorphismByImages( Group( [ () ] ), Group( [ () ] ), [  ], 
[  ] ), [ (), () ])
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroups(WW, S);
((), GroupHomomorphismByImages( Group( [ () ] ), Group( [ () ] ), [  ], 
[  ] ), [ (), () ])
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> IsReesMatrixSemigroup(W);
true
gap> map := IsomorphismSemigroups(W, W);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroups(S, W);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroups(W, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# IsomorphismSemigroups, for RMS where an argument is not WholeFamily
gap> R := ReesMatrixSemigroup(SymmetricGroup(4),
>                             [[(1, 2), (1, 4), (1, 4, 3)],
>                              [(1, 2), (), (2, 4)],
>                              [(1, 2), (1, 4, 2), (1, 3, 2)]]);;
gap> U := Semigroup([RMSElement(R, 1, (1, 2, 4), 2),
>                    RMSElement(R, 3, (1, 4), 2)]);;
gap> UU := Semigroup([RMSElement(R, 2, (), 2)]);;
gap> G := SymmetricGroup(IsPcGroup, 3);;
gap> V := ReesMatrixSemigroup(G, [[G.1, G.2 ^ 2]]);;
gap> G := Group([[[0, 1, 0],
>                 [1, 0, 0],
>                 [0, 0, 1]],
>                [[0, 1, 0],
>                 [0, 0, 1],
>                 [1, 0, 0]]]);;
gap> id := Identity(G);;
gap> S := ReesMatrixSemigroup(G, [[id, id, id, id],
>                                 [id, id, id, id],
>                                 [id, id, id, id]]);;
gap> W := ReesMatrixSubsemigroup(S, [3, 4], G, [2]);;
gap> ForAll([U, UU, V, W, R, S], IsReesMatrixSemigroup);
true
gap> ForAll([U, UU, V, W, R, S], IsCompletelySimpleSemigroup);
true

# IsomorphismClasses: [U, V, W], [R], [UU], [S]
gap> IsomorphismSemigroups(U, UU);
fail
gap> IsomorphismSemigroups(UU, U);
fail
gap> iso := IsomorphismSemigroups(U, U);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> iso := IsomorphismSemigroups(U, V);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> iso := IsomorphismSemigroups(U, W);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> IsomorphismSemigroups(U, R);
fail
gap> IsomorphismSemigroups(U, S);
fail
gap> iso := IsomorphismSemigroups(V, U);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> iso := IsomorphismSemigroups(V, V);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> iso := IsomorphismSemigroups(V, W);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> IsomorphismSemigroups(V, R);
fail
gap> IsomorphismSemigroups(V, S);
fail
gap> iso := IsomorphismSemigroups(W, U);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> iso := IsomorphismSemigroups(W, V);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> iso := IsomorphismSemigroups(W, W);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> IsomorphismSemigroups(W, R);
fail
gap> IsomorphismSemigroups(W, S);
fail
gap> IsomorphismSemigroups(R, U);
fail
gap> IsomorphismSemigroups(R, V);
fail
gap> IsomorphismSemigroups(R, W);
fail
gap> iso := IsomorphismSemigroups(R, R);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> IsomorphismSemigroups(R, S);
fail
gap> IsomorphismSemigroups(S, U);
fail
gap> IsomorphismSemigroups(S, V);
fail
gap> IsomorphismSemigroups(S, W);
fail
gap> IsomorphismSemigroups(S, R);
fail
gap> iso := IsomorphismSemigroups(S, S);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true

# IsomorphismSemigroups, for RZMS where an argument is not WholeFamily
gap> true;;

# Issue #167 (part 2)
gap> G1 := SymmetricGroup(IsPermGroup, 2);;
gap> R1 := ReesMatrixSemigroup(G1, [[Identity(G1)]]);;
gap> G2 := SymmetricGroup(IsPcGroup, 2);;
gap> R2 := ReesMatrixSemigroup(G2, [[Identity(G2)]]);;
gap> map := IsomorphismSemigroups(R1, R2);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := InverseGeneralMapping(map);;
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
((), GroupHomomorphismByImages( Group( [ (1,2)(3,6)(4,5), (1,3,5)(2,4,6) 
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

# Errors in checked version of RMSIsoByTriple
gap> g := SymmetricGroup(4);;
gap> mat := [[(1, 3), (1, 2)(3, 4)],
>            [(1, 4, 3, 2), ()],
>            [(1, 3)(2, 4), (1, 3, 4, 2)]];;
gap> R := ReesMatrixSemigroup(g, mat);;
gap> S := ReesMatrixSemigroup(Group((1, 2)), [[()]]);;
gap> auto := IdentityMapping(g);;
gap> g_elms_list := [(), (1, 3), (), (), ()];;
gap> RMSIsoByTriple(R, S, [(), auto, g_elms_list]);
Error, the 1st and 2nd arguments (Rees matrix semigroups) have different numbe\
rs of rows and columns
gap> RMSIsoByTriple(R, R, [42, auto, g_elms_list]);
Error, the 1st entry in the 3rd argument (a triple) is not a permutation
gap> RMSIsoByTriple(R, R, [(1, 7), auto, g_elms_list]);
Error, the 1st entry (a permutation) in the 3rd argument (a triple) is not a p\
ermutation on [1 .. 5]
gap> RMSIsoByTriple(R, R, [(1, 4), auto, g_elms_list]);
Error, the 1st entry (a permutation) in the 3rd argument (a triple) maps rows \
to columns
gap> RMSIsoByTriple(R, R, [(), fail, g_elms_list]);
Error, the 2nd entry in the 3rd argument (a triple) is not an isomorphism betw\
een the underlying groups of the 1st and 2nd arguments (Rees matrix semigroups\
)
gap> RMSIsoByTriple(R, R, [(), auto, [(), (), ()]]);
Error, the 3rd entry (a list) in the 3rd argument (a triple)does not have leng\
th equal to the number of rows and columns of the 1st argument (a Rees matrix \
semigroup)
gap> RMSIsoByTriple(R, R, [(), auto, [42, 43, 44, 45, 46]]);
Error, the 3rd entry (a list) in the 3rd argument (a triple) does not consist \
of elements of the underlying group of the 2nd argument (a Rees matrix semigro\
up)
gap> RMSIsoByTriple(R, R, [(), auto, g_elms_list]);
Error, the 3rd entry (a list) in the 3rd argument (a triple) does not define a\
n isomorphism
gap> iso := RMSIsoByTripleNC(R, R, [(), auto, g_elms_list]);;
gap> BruteForceIsoCheck(iso);
false
gap> g_elms_list := [(), (), (), (), ()];;
gap> iso := RMSIsoByTriple(R, R, [(), auto, g_elms_list]);
((), IdentityMapping( SymmetricGroup( [ 1 .. 4 ] ) ), [ (), (), (), (), () ])
gap> BruteForceIsoCheck(iso);
true

# Errors in checked version of RZMSIsoByTriple
gap> g := SymmetricGroup(3);;
gap> mat := [[0, 0, (1, 3)], [(1, 2, 3), (), (2, 3)], [0, 0, ()]];;
gap> R := ReesZeroMatrixSemigroup(g, mat);;
gap> S := ReesZeroMatrixSemigroup(Group((1, 2)), [[()]]);;
gap> auto := IdentityMapping(g);;
gap> g_elms_list := [(), (1, 3), (), (), (), ()];;
gap> RZMSIsoByTriple(R, S, [(), auto, g_elms_list]);
Error, the 1st and 2nd arguments (Rees 0-matrix semigroups) have different num\
bers of rows and columns
gap> RZMSIsoByTriple(R, R, [42, auto, g_elms_list]);
Error, the 1st entry in the 3rd argument (a triple) is not a permutation
gap> RZMSIsoByTriple(R, R, [(1, 3), auto, g_elms_list]);
Error, the 1st entry in the 3rd argument (a triple) is not an isomorphism from\
 the graph of the 1st argument (a Rees 0-matrix semigroup) and the graph of th\
e 2nd argument (a Rees 0-matrix semigroup)
gap> RZMSIsoByTriple(R, R, [(), fail, g_elms_list]);
Error, the 2nd entry in the 3rd argument (a triple) is not an isomorphism betw\
een the underlying groups of the 1st and 2nd arguments (Rees 0-matrix semigrou\
ps)
gap> RZMSIsoByTriple(R, R, [(), auto, [(), (), ()]]);
Error, the 3rd entry (a list) in the 3rd argument (a triple)does not have leng\
th equal to the number of rows and columns of the 1st argument (a Rees 0-matri\
x semigroup)
gap> RZMSIsoByTriple(R, R, [(), auto, [41, 42, 43, 44, 45, 46]]);
Error, the 3rd entry (a list) in the 3rd argument (a triple) does not consist \
of elements of the underlying group of the 2nd argument (a Rees 0-matrix semig\
roup)
gap> RZMSIsoByTriple(R, R, [(), auto, g_elms_list]);
Error, the 3rd entry (a list) in the 3rd argument (a triple) does not define a\
n isomorphism
gap> iso := RZMSIsoByTripleNC(R, R, [(), auto, g_elms_list]);;
gap> BruteForceIsoCheck(iso);
false
gap> g_elms_list := [(), (), (), (), (), ()];;
gap> iso := RZMSIsoByTriple(R, R, [(), auto, g_elms_list]);
((), IdentityMapping( SymmetricGroup( [ 1 .. 3 ] ) ), 
[ (), (), (), (), (), () ])
gap> BruteForceIsoCheck(iso);
true

# IsomorphismSemigroups, for RMS
gap> G := CyclicGroup(6);;
gap> R := ReesMatrixSemigroup(G, [[One(G)]]);;
gap> S := ReesMatrixSemigroup(SymmetricGroup(3), [[()]]);;
gap> IsomorphismSemigroups(R, S);
fail
gap> R := ReesMatrixSemigroup(FullTransformationSemigroup(2),
>                             [[IdentityTransformation]]);;
gap> S := ReesMatrixSemigroup(FullTransformationSemigroup(3),
>                             [[IdentityTransformation]]);;
gap> IsomorphismSemigroups(R, S);
fail
gap> R := ReesMatrixSemigroup(Group((1, 2)), [[(), ()], [(), ()]]);;
gap> S := ReesMatrixSemigroup(Group((1, 2)), [[(), ()], [(), (1, 2)]]);;
gap> IsomorphismSemigroups(R, S);
fail
gap> G := AllSmallGroups(6)[1];;
gap> H := AllSmallGroups(6)[2];;
gap> R := ReesMatrixSemigroup(G, [[One(G)]]);;
gap> S := ReesMatrixSemigroup(H, [[One(H)]]);;
gap> T := ReesMatrixSemigroup(SymmetricGroup(3), [[()]]);;
gap> IsomorphismSemigroups(R, S);
fail
gap> IsomorphismSemigroups(T, S);
fail
gap> map := IsomorphismSemigroups(T, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroups(R, T);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# IsomorphismSemigroups, for RZMS
gap> G := CyclicGroup(6);;
gap> R := ReesZeroMatrixSemigroup(G, [[One(G)]]);;
gap> S := ReesZeroMatrixSemigroup(SymmetricGroup(3), [[()]]);;
gap> IsomorphismSemigroups(R, S);
fail
gap> R := ReesZeroMatrixSemigroup(FullTransformationSemigroup(2),
>                                 [[IdentityTransformation]]);;
gap> S := ReesZeroMatrixSemigroup(FullTransformationSemigroup(3),
>                                 [[IdentityTransformation]]);;
gap> IsomorphismSemigroups(R, S);
fail
gap> R := ReesZeroMatrixSemigroup(Group((1, 2)), [[(), ()], [(), ()]]);;
gap> S := ReesZeroMatrixSemigroup(Group((1, 2)), [[(), ()], [(), (1, 2)]]);;
gap> IsomorphismSemigroups(R, S);
fail
gap> G := AllSmallGroups(6)[1];;
gap> H := AllSmallGroups(6)[2];;
gap> R := ReesZeroMatrixSemigroup(G, [[One(G)]]);;
gap> S := ReesZeroMatrixSemigroup(H, [[One(H)]]);;
gap> T := ReesZeroMatrixSemigroup(SymmetricGroup(3), [[()]]);;
gap> IsomorphismSemigroups(R, S);
fail
gap> IsomorphismSemigroups(T, S);
fail
gap> map := IsomorphismSemigroups(T, R);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> map := IsomorphismSemigroups(R, T);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true
gap> G := CyclicGroup(IsPcGroup, 5);;
gap> R := ReesZeroMatrixSemigroup(G, [[One(G), 0], [One(G), One(G)]]);;
gap> S := ReesZeroMatrixSemigroup(G, [[One(G), One(G)], [0, One(G)]]);;
gap> map := IsomorphismSemigroups(R, S);;
gap> BruteForceIsoCheck(map);
true
gap> BruteForceInverseCheck(map);
true

# IsomorphismRees(Zero)MatrixSemigroupOverPermGroup
gap> S := FullTransformationMonoid(3);;
gap> IsomorphismReesMatrixSemigroupOverPermGroup(S);
Error, the argument is not a finite simple semigroup
gap> IsomorphismReesZeroMatrixSemigroupOverPermGroup(S);
Error, the argument is not a finite 0-simple semigroup
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
gap> G := AsSemigroup(IsTransformationSemigroup, G);
<transformation monoid of size 8, degree 8 with 7 generators>
gap> x := IdentityTransformation;;
gap> y := Transformation([4, 6, 7, 1, 8, 2, 3, 5]);;
gap> R := ReesZeroMatrixSemigroup(G, [[x, 0, 0], [0, x, 0], [0, 0, x]]);
<Rees 0-matrix semigroup 3x3 over <transformation monoid of size 8, degree 8 
  with 7 generators>>
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
gap> R := ReesZeroMatrixSemigroup(G, [[y, 0, 0], [0, y, 0], [0, 0, y]]);
<Rees 0-matrix semigroup 3x3 over <transformation group of size 8, 
  degree 8 with 7 generators>>
gap> S := Semigroup(RMSElement(R, 1, x, 2), RMSElement(R, 2, x, 1));;
gap> iso := IsomorphismReesZeroMatrixSemigroupOverPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true
gap> S := Semigroup(MinimalIdeal(FullTransformationMonoid(5)));;
gap> IsomorphismReesMatrixSemigroupOverPermGroup(S);;
gap> BruteForceIsoCheck(iso);
true
gap> BruteForceInverseCheck(iso);
true

# CanonicalReesZeroMatrixSemigroup
gap> S := ReesZeroMatrixSemigroup(SymmetricGroup([1 .. 4]),
> [[(), (2, 3), (2, 3, 4)], [(1, 2)(3, 4), (), (1, 2, 4, 3)],
> [(1, 4, 2), (1, 3)(2, 4), ()]]);;
gap> T := ReesZeroMatrixSemigroup(SymmetricGroup([1 .. 4]), 
> [[(1, 2, 4, 3), (2, 4, 3), (1, 4)(2, 3)], [(1, 4), (), (1, 3)],
> [(), (1, 3)(2, 4), (1, 3, 4, 2)]]);;
gap> mat := [[(), (), ()], [(1, 4), (), (2, 4)],
> [(), (1, 3), (1, 4, 3, 2)]];;
gap> Matrix(CanonicalReesZeroMatrixSemigroup(S)) = mat;
true
gap> Matrix(CanonicalReesZeroMatrixSemigroup(T)) = mat;
true
gap> S := ReesZeroMatrixSemigroup(Group(
> [(1, 2, 3), (4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
> 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
> 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
> 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78,
> 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97,
> 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
> 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
> 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142)]), [[(),
> (1, 2, 3)(4, 69, 134, 60, 125, 51, 116, 42, 107, 33, 98, 24, 89, 15, 80, 6,
> 71, 136, 62, 127, 53, 118, 44, 109, 35, 100, 26, 91, 17, 82, 8, 73, 138, 64,
> 129, 55, 120, 46, 111, 37, 102, 28, 93, 19, 84, 10, 75, 140, 66, 131, 57, 122,
> 48, 113, 39, 104, 30, 95, 21, 86, 12, 77, 142, 68, 133, 59, 124, 50, 115, 41,
> 106, 32, 97, 23, 88, 14, 79, 5, 70, 135, 61, 126, 52, 117, 43, 108, 34, 99,
> 25, 90, 16, 81, 7, 72, 137, 63, 128, 54, 119, 45, 110, 36, 101, 27, 92, 18,
> 83, 9, 74, 139, 65, 130, 56, 121, 47, 112, 38, 103, 29, 94, 20, 85, 11, 76,
> 141, 67, 132, 58, 123, 49, 114, 40, 105, 31, 96, 22, 87, 13, 78), (), ()],
> [(1, 3, 2)(4, 106, 69, 32, 134, 97, 60, 23, 125, 88, 51, 14, 116, 79, 42, 5,
> 107, 70, 33, 135, 98, 61, 24, 126, 89, 52, 15, 117, 80, 43, 6, 108, 71, 34,
> 136, 99, 62, 25, 127, 90, 53, 16, 118, 81, 44, 7, 109, 72, 35, 137, 100, 63,
> 26, 128, 91, 54, 17, 119, 82, 45, 8, 110, 73, 36, 138, 101, 64, 27, 129, 92,
> 55, 18, 120, 83, 46, 9, 111, 74, 37, 139, 102, 65, 28, 130, 93, 56, 19, 121,
> 84, 47, 10, 112, 75, 38, 140, 103, 66, 29, 131, 94, 57, 20, 122, 85, 48, 11,
> 113, 76, 39, 141, 104, 67, 30, 132, 95, 58, 21, 123, 86, 49, 12, 114, 77, 40,
> 142, 105, 68, 31, 133, 96, 59, 22, 124, 87, 50, 13, 115, 78, 41), (), (4, 64,
> 124, 45, 105, 26, 86, 7, 67, 127, 48, 108, 29, 89, 10, 70, 130, 51, 111, 32,
> 92, 13, 73, 133, 54, 114, 35, 95, 16, 76, 136, 57, 117, 38, 98, 19, 79, 139,
> 60, 120, 41, 101, 22, 82, 142, 63, 123, 44, 104, 25, 85, 6, 66, 126, 47, 107,
> 28, 88, 9, 69, 129, 50, 110, 31, 91, 12, 72, 132, 53, 113, 34, 94, 15, 75,
> 135, 56, 116, 37, 97, 18, 78, 138, 59, 119, 40, 100, 21, 81, 141, 62, 122, 43,
> 103, 24, 84, 5, 65, 125, 46, 106, 27, 87, 8, 68, 128, 49, 109, 30, 90, 11, 71,
> 131, 52, 112, 33, 93, 14, 74, 134, 55, 115, 36, 96, 17, 77, 137, 58, 118, 39,
> 99, 20, 80, 140, 61, 121, 42, 102, 23, 83), 0]]);;
gap> mat := [[0, (), (), ()],
> [(),  (),  (4, 96, 49, 141, 94, 47, 139, 92, 45, 137, 90, 43, 135, 88, 41,
> 133, 86, 39, 131, 84, 37, 129, 82, 35, 127, 80, 33, 125, 78, 31, 123, 76, 29,
> 121, 74, 27, 119, 72, 25, 117, 70, 23, 115, 68, 21, 113, 66, 19, 111, 64, 17,
> 109, 62, 15, 107, 60, 13, 105, 58, 11, 103, 56, 9, 101, 54, 7, 99, 52, 5, 97,
> 50, 142, 95, 48, 140, 93, 46, 138, 91, 44, 136, 89, 42, 134, 87, 40, 132, 85,
> 38, 130, 83, 36, 128, 81, 34, 126, 79, 32, 124, 77, 30, 122, 75, 28, 120, 73,
> 26, 118, 71, 24, 116, 69, 22, 114, 67, 20, 112, 65, 18, 110, 63, 16, 108, 61,
> 14, 106, 59, 12, 104, 57, 10, 102, 55, 8, 100, 53, 6, 98, 51), (1, 2, 3)(4,
> 142, 141, 140, 139, 138, 137, 136, 135, 134, 133, 132, 131, 130, 129, 128,
> 127, 126, 125, 124, 123, 122, 121, 120, 119, 118, 117, 116, 115, 114, 113,
> 112, 111, 110, 109, 108, 107, 106, 105, 104, 103, 102, 101, 100, 99, 98, 97,
> 96, 95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79, 78,
> 77, 76, 75, 74, 73, 72, 71, 70, 69, 68, 67, 66, 65, 64, 63, 62, 61, 60, 59,
> 58, 57, 56, 55, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42, 41, 40,
> 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21,
> 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5)]];;
gap> Matrix(CanonicalReesZeroMatrixSemigroup(S)) = mat;
true
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2, 3, 4)]), [[(), (1, 2, 3, 4), 
> (1, 2, 3, 4), (1, 3)(2, 4), 0], [0, (), (1, 2, 3, 4), (), ()], [0, (), (), (1,
> 2, 3, 4), ()], [(1, 4, 3, 2), (1, 4, 3, 2), 0, (), (1, 3)(2, 4)], [0, 0, (1,
> 4, 3, 2), (1, 3)(2, 4), ()]]);;
gap> mat := [[(), 0, 0, (), ()], [0, (), (), (1, 4, 3, 2), (1, 2, 3, 4)],
> [(), (), (1, 2, 3, 4), (), 0], [(1, 3)(2, 4), 0, (), (), ()],
> [(), 0, (1, 3)(2, 4), (1, 2, 3, 4), (1, 4, 3, 2)]];;
gap> Matrix(CanonicalReesZeroMatrixSemigroup(S)) = mat;
true
gap> T := CanonicalReesZeroMatrixSemigroup(S);;
gap> mat = Matrix(T);
true
gap> UnderlyingSemigroup(S) = UnderlyingSemigroup(T);
true
gap> S := ReesZeroMatrixSemigroup(AlternatingGroup([1 .. 5]),
> [[(), 0], [(1, 3, 4), (2, 4, 5)], [(1, 5, 2), (1, 5, 2, 4, 3)]]);;
gap> mat := [[0, ()], [(), ()], [(), (1, 5, 4)]];;
gap> Matrix(CanonicalReesZeroMatrixSemigroup(S)) = mat;
true
gap> T := CanonicalReesZeroMatrixSemigroup(S);;
gap> mat = Matrix(T);
true
gap> UnderlyingSemigroup(S) = UnderlyingSemigroup(T);
true

# CanonicalReesMatrixSemigroup
gap> S := ReesMatrixSemigroup(Group([(1, 2), (3, 4)]), 
> [[(), (), (3, 4), (), ()], [(), (3, 4), (), (3, 4), (1, 2)], [(), (1, 2), (3,
> 4), (), ()], [(1, 2)(3, 4), (3, 4), (), (), ()], [(), (1, 2), (1, 2)(3, 4),
> (), ()]]);;
gap> mat := [[(), (), (), (), ()], [(), (), (), (), (1, 2)],
> [(), (), (), (1, 2), ()], [(), (3, 4), (1, 2)(3, 4), (), (1, 2)],
> [(), (), (3, 4), (1, 2)(3, 4), (3, 4)]];;
gap> mat = Matrix(CanonicalReesMatrixSemigroup(S));
true
gap> mat = Matrix(CanonicalReesMatrixSemigroup(S));
true
gap> S := ReesMatrixSemigroup(AlternatingGroup([1 .. 5]),
> [[(), (), (1, 5, 4, 2, 3)], [(1, 5, 4), (1, 3, 2, 5, 4), ()], [(), (), (1, 2,
> 3, 4, 5)], [(), (), ()]]);;
gap> mat :=
> [[(), (), ()], [(), (), (1, 4)(2, 5)], [(), (), (1, 3, 5, 4, 2)],
> [(), (1, 3, 4), (1, 3, 5)]];;
gap> mat = Matrix(CanonicalReesMatrixSemigroup(S));
true
gap> mat = Matrix(CanonicalReesMatrixSemigroup(S));
true

# CanonicalX error messages
gap> G := Semigroup([Transformation([2, 1])]);;
gap> mat := [[IdentityTransformation, IdentityTransformation,
> IdentityTransformation], [IdentityTransformation, IdentityTransformation,
> Transformation([2, 1])]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> CanonicalReesZeroMatrixSemigroup(S);
Error, the underlying semigroup of the argument (a Rees 0-matrix semigroup) is\
 not a group
gap> S := ReesMatrixSemigroup(G, mat);;
gap> CanonicalReesMatrixSemigroup(S);
Error, the underlying semigroup of the argument (a Rees 0-matrix semigroup) is\
 not a group

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/attributes/isorms.tst");
