#############################################################################
##
#W  extreme/translat.tst
#Y  Copyright (C) 2016-17                                          Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local G, S, h, l, mat, r, s, t
gap> START_TEST("Semigroups package: extreme/translat.tst");
gap> LoadPackage("semigroups", false);;
gap> SEMIGROUPS.StartTest();

# RZMS Translational Hull
gap> G := Range(IsomorphismPermGroup(SmallGroup(12, 1)));;
gap> mat := [[0, G.2], [G.1, G.1], [G.2, 0]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> Size(TranslationalHull(S));
97
gap> for h in TranslationalHull(S) do
> l := h![1];
> r := h![2];
> for s in S do
> for t in S do
> if not s * (t ^ l) = (s ^ r) * t then
> Print(s, t, h);
> fi;
> od;
> od;
> od;

# RMS Translational Hull
gap> G := Range(IsomorphismPermGroup(SmallGroup(12, 1)));;
gap> mat := TransposedMat([[G.1, G.2, G.3 * G.2, G.1 * G.3],
> [G.2 * G.2, G.3, G.1, G.3 * G.3]]);;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> Size(TranslationalHull(S));
444

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/translat.tst");
