#############################################################################
##
#W  congruence_smallsemi.tst
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id: install_with_grape.tst 33 2010-05-08 14:54:13Z jamesm $
##

#ReadTest( Filename( DirectoriesPackageLibrary( "monoid", "tst" ), "congruence_smallsemi.tst" ) );

gap> START_TEST("install_with_grape.tst 3.1.4");

gap> a:=SmallSemigroup(6, 15040);
<small semigroup of size 6>
gap> CongruencesSemilatticeByCayleyGraph(a)=CongruencesOfSmallSemigroup(a);
true
gap> enum:=EnumeratorOfSmallSemigroups(5, IsSemilatticeAsSemigroup, true);
<enumerator of semigroups of size 5>
gap> ForAll(enum, x-> CongruencesSemilatticeByCayleyGraph(x)=
> CongruencesOfSmallSemigroup(x));
true
gap> enum:=EnumeratorOfSmallSemigroups(6, IsSemilatticeAsSemigroup, true);
<enumerator of semigroups of size 6>
gap> ForAll(enum, x-> CongruencesSemilatticeByCayleyGraph(x)=
> CongruencesOfSmallSemigroup(x));
true
