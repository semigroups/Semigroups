#############################################################################
##
#W  cong_no_smallsemi.tst
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

#ReadTest( Filename( DirectoriesPackageLibrary( "monoid", "tst" ), "cong_no_smallsemi.tst" ) );

gap> START_TEST("cong_no_smallsemi.tst 3.1.4");
gap> gens:=[ Transformation( [ 1, 2, 6, 4, 5, 6 ] ), 
> Transformation( [ 1, 2, 6, 4, 6, 6 ] ), 
> Transformation( [ 1, 2, 6, 6, 6, 6 ] ), 
> Transformation( [ 1, 6, 3, 4, 6, 6 ] ), 
> Transformation( [ 1, 6, 6, 4, 6, 6 ] ), 
> Transformation( [ 1, 6, 6, 6, 6, 6 ] ), 
> Transformation( [ 6, 2, 3, 4, 6, 6 ] ), 
> Transformation( [ 6, 2, 6, 4, 6, 6 ] ), 
> Transformation( [ 6, 2, 6, 6, 6, 6 ] ), 
> Transformation( [ 6, 6, 3, 4, 6, 6 ] ), 
> Transformation( [ 6, 6, 3, 6, 6, 6 ] ), 
> Transformation( [ 6, 6, 6, 4, 6, 6 ] ), 
> Transformation( [ 6, 6, 6, 6, 6, 6 ] ) ];;
gap> s:=Semigroup(gens);;
gap> CongruencesSemilatticeByCayleyGraph(s);;
#gap> time;
#913
gap> Length(last2);
1086
gap> ForAll(CongruencesSemilattice(s), x-> IsCongruenceOfSemilattice(s, x));
true
gap> STOP_TEST( "cong_no_smallsemi.tst 3.1.4", 10000);