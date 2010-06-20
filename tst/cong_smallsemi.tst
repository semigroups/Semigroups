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
gap> LoadPackage("monoid");;
gap> if IsBound(GAPInfo.PackagesInfo.smallsemi) and 
> CompareVersionNumbers("0.6", "smallsemi") then 
> LoadPackage("smallsemi", "0.6", false);;
> fi;
gap> cong_smallsemi_info_level:=InfoLevel(InfoMonoidCongruences);;
gap> smallsemi_info_level:=InfoLevel(InfoSmallsemi);;
gap> SetInfoLevel(InfoMonoidCongruences, 0);;
gap> SetInfoLevel(InfoSmallsemi, 0);;
gap> a:=SmallSemigroup(6, 15040);
<small semigroup of size 6>
gap> CongruencesSemilatticeAsPositions(a)=CongruencesOfSemigroup(a);
true
gap> enum:=EnumeratorOfSmallSemigroups(5, IsSemilatticeAsSemigroup, true);
<enumerator of semigroups of size 5>
gap> ForAll(enum, x-> CongruencesSemilatticeAsPositions(x)=
> CongruencesOfSemigroup(x));
true
gap> enum:=EnumeratorOfSmallSemigroups(6, IsSemilatticeAsSemigroup, true);
<enumerator of semigroups of size 6>
gap> ForAll(enum, x-> CongruencesSemilatticeAsPositions(x)=
> CongruencesOfSemigroup(x));
true
gap> SetInfoLevel(InfoMonoidCongruences, cong_smallsemi_info_level);;
gap> SetInfoLevel(InfoSmallsemi, smallsemi_info_level);;
gap> Unbind(cong_smallsemi_info_level);;
gap> Unbind(smallsemi_info_level);;
gap> STOP_TEST( "cong_smallsemi.tst 3.1.4", 10000);
