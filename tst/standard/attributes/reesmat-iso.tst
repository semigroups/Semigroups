#############################################################################
##
#W  standard/attributes/reesmat-iso.tst
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/attributes/reesmat-iso.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS_StartTest();

#T#
gap> R := ReesZeroMatrixSemigroup(Group([(2, 8), (2, 8, 6)]),
> [[0, (2, 8), 0, 0, 0, (2, 8, 6)],
>  [(), 0, (2, 8, 6), (2, 6), (2, 6, 8), 0],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0],
>  [0, (2, 8, 6), 0, 0, 0, (2, 8)],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0]]);;
gap> AutomorphismGroup(R);
<automorphism group of <Rees 0-matrix semigroup 6x6 over Group([ (2,8), (2,8,
6) ])> with 37 generators>
gap> Size(last);
72

#T#
gap> SetInfoLevel(InfoSemigroups, 2);
gap> R := ReesZeroMatrixSemigroup(Group([(2, 8), (2, 8, 6)]),
> [[0, (2, 8), 0, 0, 0, (2, 8, 6)],
>  [(), 0, (2, 8, 6), (2, 6), (2, 6, 8), 0],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0],
>  [0, (2, 8, 6), 0, 0, 0, (2, 8)],
>  [(2, 8, 6), 0, (2, 6, 8), (2, 8), (), 0]]);;
gap> AutomorphismGroup(R);
#I  finding automorphisms of the graph . . . 2304 found
#I  finding the stabilizer of matrix . . . 12
#I  finding the automorphism group of the group . . . found 6
#I  finding the stabilizer of matrix entries . . . 1
#I  the graph has 2 connected components
#I  backtracking in the direct product of size 2304 . . . 
<automorphism group of <Rees 0-matrix semigroup 6x6 over Group([ (2,8), (2,8,
6) ])> with 37 generators>
gap> Size(last);
72
gap> SetInfoLevel(InfoSemigroups, 0);

#T#
gap> I := MinimalIdeal(PartitionMonoid(4));
<simple bipartition semigroup ideal of degree 4 with 1 generator>
gap> R := Range(IsomorphismReesMatrixSemigroup(I));
<Rees matrix semigroup 15x15 over Group(())>
gap> G := AutomorphismGroup(R);
<automorphism group of <Rees matrix semigroup 15x15 over Group(())> with 
5 generators>
gap> G := Range(IsomorphismPermGroup(AutomorphismGroup(R)));
<permutation group with 5 generators>
gap> Size(G);
1710012252724199424000000

#T# RZMSInducedFunction with lots of connected component
gap> I := SemigroupIdeal(
> InverseMonoid(
>  [ PartialPermNC( [ 1, 2, 3, 4, 5 ], [ 2, 3, 4, 5, 1 ] ), PartialPermNC( [ 1,\
> 2, 3, 4, 5 ], [ 2, 1, 3, 4, 5 ] ), PartialPermNC( [ 2, 3, 4, 5 ], [ 1, 2, 3, \
> 4 ] ) ] ), [ PartialPermNC( [ 1 ], [ 1 ] ) ] );;
gap> R := Range(IsomorphismReesZeroMatrixSemigroup(I));
<Rees 0-matrix semigroup 5x5 over Group(())>
gap> AutomorphismGroup(R);
<automorphism group of <Rees 0-matrix semigroup 5x5 over Group(())> with 
5 generators>
gap> Size(last);
120

#T# RMSInducedFunction
gap> AutomorphismGroup(RectangularBand(4, 3));
<automorphism group of <Rees matrix semigroup 4x3 over Group(())> with 
5 generators>

#T# RZMSInducedFunction with one connected component
gap> R := ReesZeroMatrixSemigroup( Group( [ () ] ),
> [ [ (), (), (), () ], [ (), (), (), () ], [ (), (), (), () ] ] );
<Rees 0-matrix semigroup 4x3 over Group(())>
gap> AutomorphismGroup(R);
<automorphism group of <Rees 0-matrix semigroup 4x3 over Group(())> with 
6 generators>
gap> Size(last);
144

#T# AutomorphismGroup: for a RZMS with trivial automorphism group of graph
gap> R := ReesZeroMatrixSemigroup( Group( [ (1,2,3), (1,2) ] ), [ [ (1,3), (1,2) ], [ 0, (2,3)] ] );;
gap> AutomorphismGroup(R);
<automorphism group of <Rees 0-matrix semigroup 2x2 over Group([ (1,2,3), (1,
2) ])> with 6 generators>
gap> Size(last);
6

#T# AutomorphismGroup: for a RZMS over not a group
gap> S := ReesZeroMatrixSemigroup(FullTransformationMonoid(2), [[IdentityTransformation]]);
<Rees 0-matrix semigroup 1x1 over <regular transformation monoid 
  of size 4, degree 2 with 2 generators>>
gap> AutomorphismGroup(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `AutomorphismGroup' on 1 arguments

#T# AutomorphismGroup: for a RZMSGraph with 0 generators
gap> func := function(n, i)
>   local out;
>   out := ListWithIdenticalEntries(n, 0);
>   out[i] := ();
>   return out;
> end;
function( n, i ) ... end
gap> mat := List([1..33], i -> func(33, i));;
gap> R := ReesZeroMatrixSemigroup(Group(()), mat);
<Rees 0-matrix semigroup 33x33 over Group(())>
gap> AutomorphismGroup(R);
<automorphism group of <Rees 0-matrix semigroup 33x33 over Group(())> with 
33 generators>

#T# AutomorphismGroup: for a RMS over not a group
gap> R := ReesMatrixSemigroup(SymmetricInverseMonoid(2), [[PartialPerm([1])]]);
<Rees matrix semigroup 1x1 over <inverse partial perm monoid of rank 2 with
  2 generators>>
gap> AutomorphismGroup(R);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `AutomorphismGroup' on 1 arguments

#T# AutomorphismGroup: 1x1 RMS
gap> R := ReesMatrixSemigroup(Group(()), [[()]]);
<Rees matrix semigroup 1x1 over Group(())>
gap> AutomorphismGroup(R);
<automorphism group of <Rees matrix semigroup 1x1 over Group(())> with 
1 generator>

#T# AutomorphismGroup: 1x2 and 2x1 RMS
gap> AutomorphismGroup(RectangularBand(2, 1));
<automorphism group of <Rees matrix semigroup 2x1 over Group(())> with 
2 generators>
gap> AutomorphismGroup(RectangularBand(1, 2));
<automorphism group of <Rees matrix semigroup 1x2 over Group(())> with 
2 generators>

#T# AutomorphismGroup: 1x3 and 3x1 RMS
gap> AutomorphismGroup(RectangularBand(3, 1));
<automorphism group of <Rees matrix semigroup 3x1 over Group(())> with 
3 generators>
gap> AutomorphismGroup(RectangularBand(1, 3));
<automorphism group of <Rees matrix semigroup 1x3 over Group(())> with 
3 generators>

#T# AutomorphismGroup: 33x33 RMS
gap> AutomorphismGroup(RectangularBand(33, 33));
<automorphism group of <Rees matrix semigroup 33x33 over Group(())> with 
65 generators>

#T# IdentityMapping: for an RMS
gap> R := ReesMatrixSemigroup(Group( [ () ] ), [ [ () ] ]);
<Rees matrix semigroup 1x1 over Group(())>
gap> IdentityMapping(R);
((), IdentityMapping( Group( [ () ] ) ), [ (), () ])

#T# IsomorphismSemigroups: RMS fail
gap> R := RectangularBand(2, 2);
<Rees matrix semigroup 2x2 over Group(())>
gap> S := RectangularBand(2, 3);
<Rees matrix semigroup 2x3 over Group(())>
gap> IsomorphismSemigroups(R, S);
fail

#T# IsomorphismSemigroups: from RMS to itself
gap> R := RectangularBand(2, 2);
<Rees matrix semigroup 2x2 over Group(())>
gap> IsomorphismSemigroups(R, R);
((), IdentityMapping( Group( [ () ] ) ), [ (), (), (), () ])

#T# IsomorphismSemigroups: from RMS to RMS
gap> S := ReesMatrixSemigroup( Group( [ (1,2) ] ), [ [ (), () ], [ (), (1,2) ] ] );
<Rees matrix semigroup 2x2 over Group([ (1,2) ])>
gap> R := ReesMatrixSemigroup( Group( [ (1,2) ] ), [ [ (), (1,2) ], [ (), () ] ] );
<Rees matrix semigroup 2x2 over Group([ (1,2) ])>
gap> IsomorphismSemigroups(R, S);
((), GroupHomomorphismByImages( Group( [ (1,2) ] ), Group( [ (1,2) ] ), 
[ (1,2) ], [ (1,2) ] ), [ (), (), (), () ])

#T# IsomorphismSemigroups: from RZMS to RZMS
gap> S := ReesZeroMatrixSemigroup( Group( [ () ] ), [ [ (), () ], [ (), 0 ] ] );
<Rees 0-matrix semigroup 2x2 over Group(())>
gap> R := ReesZeroMatrixSemigroup( Group( [ () ] ), [ [ (), 0 ], [ (), () ] ] );
<Rees 0-matrix semigroup 2x2 over Group(())>
gap> IsomorphismSemigroups(R, S);
((3,4), GroupHomomorphismByImages( Group( [ () ] ), Group( [ () ] ), [  ], 
[  ] ), [ (), (), (), () ])

#T# IsomorphismSemigroups: fail (non-regular RZMS)
gap> R := ReesZeroMatrixSemigroup( Group( [ () ] ), [ [ 0, 0 ], [ (), () ] ] );
<Rees 0-matrix semigroup 2x2 over Group(())>
gap> S := ReesZeroMatrixSemigroup( Group( [ (1,2) ] ), [ [ (), () ], [ (), (1,2) ] ] );
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
gap> IsomorphismSemigroups(R, S);
Error, Semigroups: IsomorphismSemigroups: usage,
the arguments must be regular Rees 0-matrix semigroups over groups,

#T# IsomorphismSemigroups: fail (different dimensions)
gap> R := ReesZeroMatrixSemigroup( Group( [ () ] ), [ [ (), () ] ] );;
gap> S := ReesZeroMatrixSemigroup( Group( [ () ] ), [ [ () ], [ () ] ] );;
gap> IsomorphismSemigroups(R, S);
fail

#T# IsomorphismSemigroups: from RZMS to itself
gap> R := ReesZeroMatrixSemigroup( Group( [ () ] ), [ [ (), () ] ] );;
gap> IsomorphismSemigroups(R, R);
((), IdentityMapping( Group( [ () ] ) ), [ (), (), () ])

#T# IsomorphismSemigroups: fail (non-isomorphic groups)
gap> R := ReesZeroMatrixSemigroup( Group( [ (1,2,3,4) ] ), [ [ (), () ] ] );;
gap> S := ReesZeroMatrixSemigroup( Group( [ (1,2), (3,4) ] ), [ [ (), () ] ] );;
gap> IsomorphismSemigroups(R, S);
fail

#T# IsomorphismSemigroups: fail (non-isomorphic graphs)
gap> R := ReesZeroMatrixSemigroup( Group( [ () ] ), [ [ (), () ], [(), ()] ] );;
gap> S := ReesZeroMatrixSemigroup( Group( [ () ] ), [ [ (), 0 ], [(), ()] ] );;
gap> IsomorphismSemigroups(R, S);
fail

#T# IsomorphismSemigroups: non-trivial isomorphism 1/2
gap> R := ReesZeroMatrixSemigroup( Group( [ (1,2) ] ), [ [ (), 0 ], [ 0, ()] ] );;
gap> S := ReesZeroMatrixSemigroup( Group( [ (1,2) ] ), [ [ 0, () ], [(1,2), 0] ] );;
gap> IsomorphismSemigroups(R, S);
((3,4), GroupHomomorphismByImages( Group( [ (1,2) ] ), Group( [ (1,2) ] ), 
[ (1,2) ], [ (1,2) ] ), [ (), (), (1,2), () ])

#T# IsomorphismSemigroups: non-trivial isomorphism 2/2
gap> S := ReesZeroMatrixSemigroup( Group( [ (1,2,3), (1,2) ] ), [ [ 0, (1,2,3) ], [(1,3,2), ()] ] );;
gap> R := ReesZeroMatrixSemigroup( Group( [ (1,2,3), (1,2) ] ), [ [ (1,3), (1,2) ], [ 0, (2,3)] ] );;
gap> IsomorphismSemigroups(R, S);
((3,4), GroupHomomorphismByImages( Group( [ (1,2,3), (1,2) ] ), Group( 
[ (1,2,3), (1,2) ] ), [ (1,2,3), (1,2) ], [ (1,2,3), (1,2) ] ), 
[ (), (1,2,3), (2,3), (1,2) ])

#T# SEMIGROUPS_RZMStoRZMSInducedFunction: error, 1/1
gap> R := ReesZeroMatrixSemigroup( Group( [ (1,2) ] ), [ [ (), 0 ], [ 0, ()] ] );;
gap> SEMIGROUPS_RZMStoRZMSInducedFunction(R, R, fail, fail, [1]);
Error, Semigroups: SEMIGROUPS_RZMStoRZMSInducedFunction: usage,
the 5th argument must be a list of length 2,

#T# \=: RMS and RMS elements
gap> R := RectangularBand(2, 2);
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
gap> R := RectangularBand(2, 2);
<Rees matrix semigroup 2x2 over Group(())>
gap> S := RectangularBand(2, 2);
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
gap> R := ReesZeroMatrixSemigroup( Group( [ (1,2,3) ] ), [ [ (1,2,3), 0
> ], [ 0, (1,2,3)] ] );;
gap> G := AutomorphismGroup(R);
<automorphism group of <Rees 0-matrix semigroup 2x2 over Group([ (1,2,
3) ])> with 3 generators>
gap> G.1 = G.2;
false
gap> CompositionMapping2(G.1, G.2);
((), IdentityMapping( Group( [ (1,2,3) ] ) ), [ (), (), (), () ])
gap> R.1 ^ G.1;
(1,(),1)
gap> G.1 ^ -1;
((), IdentityMapping( Group( [ (1,2,3) ] ) ), [ (), (1,3,2), (), (1,3,2) ])
gap> IsOne(G.1);
false
gap> IsOne(One(G.1 * G.2));
true
gap> Print(G.1); true
RZMSIsoByTriple ( ReesZeroMatrixSemigroup( Group( [ (1,2,3) ] ), 
[ [ (1,2,3), 0 ], [ 0, (1,2,3) ] ] ), ReesZeroMatrixSemigroup( Group( 
[ (1,2,3) ] ), [ [ (1,2,3), 0 ], [ 0, (1,2,3) ] 
 ] ), (), IdentityMapping( Group( [ (1,2,3) ] ) ), [ (), (1,2,3), (), (1,2,3) 
 ] )Syntax error: ; expected in stream line 2
^

#T# \=: RZMS and RZMS elements 2/2
gap> R := ReesZeroMatrixSemigroup( Group( [ (1,2,3) ] ), [ [ (1,2,3), 0
> ], [ 0, (1,2,3)] ] );;
gap> G := AutomorphismGroup(R);
<automorphism group of <Rees 0-matrix semigroup 2x2 over Group([ (1,2,
3) ])> with 3 generators>
gap> S := ReesZeroMatrixSemigroup( Group( [ (1,2,3), (1,2) ] ), [ [ 0, (1,2,3)
> ], [(1,3,2), ()] ] );;
gap> H := AutomorphismGroup(S);
<automorphism group of <Rees 0-matrix semigroup 2x2 over Group([ (1,2,3), (1,
2) ])> with 6 generators>
gap> G.1 = H.1;
false
gap> CompositionMapping2(G.1, G.2);
((), IdentityMapping( Group( [ (1,2,3) ] ) ), [ (), (), (), () ])
gap> R.1 ^ G.1;
(1,(),1)
gap> G.1 ^ -1;
((), IdentityMapping( Group( [ (1,2,3) ] ) ), [ (), (1,3,2), (), (1,3,2) ])
gap> IsOne(G.1);
false
gap> IsOne(One(G.1 * G.2));
true
gap> Print(G.1); true;
RZMSIsoByTriple ( ReesZeroMatrixSemigroup( Group( [ (1,2,3) ] ), 
[ [ (1,2,3), 0 ], [ 0, (1,2,3) ] ] ), ReesZeroMatrixSemigroup( Group( 
[ (1,2,3) ] ), [ [ (1,2,3), 0 ], [ 0, (1,2,3) ] 
 ] ), (), IdentityMapping( Group( [ (1,2,3) ] ) ), [ (), (1,2,3), (), (1,2,3) 
 ] )true
gap> PreImagesRepresentative(G.2, R.1);
(1,(),1)
gap> ImagesElm(G.2, R.1);
[ (1,(),1) ]

#E#
# SEMIGROUPS_UnbindVariables
gap> STOP_TEST("Semigroups package: standard/attributes/reesmat-iso.tst");
