#############################################################################
##
#W  cong_smallsemi.tst
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

#ReadTest( Filename( DirectoriesPackageLibrary( "monoid", "tst" ), "cong_smallsemi.tst" ) );

gap> START_TEST("cong_smallsemi.tst 3.1.4");
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
gap> STOP_TEST( "cong_smallsemi.tst 3.1.4", 10000);
