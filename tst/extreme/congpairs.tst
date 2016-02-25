############################################################################
##
#W  extreme/congpairs.tst
#Y  Copyright (C) 2016                                    Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: extreme/congpairs.tst");
gap> LoadPackage("semigroups", false);;

#  
gap> SEMIGROUPS.StartTest();;

# LatticeOfXCongruences with 'transrep' flag
gap> S := Semigroup( [ PartialPerm( [ 1, 2 ], [ 2, 1 ] ), 
>                      PartialPerm( [ 1, 2 ], [ 3, 1 ] ) ] );;
gap> l := [ [  ], [ 1, 3 ], [ 1 ], [ 1, 5 ], [ 1 ], [ 1, 2, 3, 10, 21 ], 
> [ 1, 4, 5, 16, 25 ], [ 1, 12 ], [ 1, 4, 5, 10, 13, 23, 24, 30 ], [ 1 ], 
> [ 1, 8, 12, 16, 35 ], [ 1 ], [ 1, 4, 5 ], [ 1, 8, 10, 12, 29 ], 
> [ 1, 2, 3, 16, 17, 20, 22, 37 ], [ 1 ], [ 1, 2, 3 ], [ 1, 8, 12, 19 ], 
> [ 1, 8, 12 ], [ 1, 2, 3, 16, 22 ], [ 1, 3, 10 ], [ 1, 3, 16 ], 
> [ 1, 4, 5, 10, 24 ], [ 1, 5, 10 ], [ 1, 5, 16 ], 
> [ 1, 2, 3, 6, 10, 16, 20, 21, 22, 31, 33 ], 
> [ 1, 4, 5, 7, 10, 16, 23, 24, 25, 31, 34 ], 
> [ 1, 8, 10, 11, 12, 14, 16, 19, 29, 31, 35, 36 ], [ 1, 10, 12 ], 
> [ 1, 4, 5, 10, 13, 23, 24 ], [ 1, 10, 16 ], 
> [ 1, 8, 10, 11, 12, 14, 16, 18, 19, 28, 29, 31, 35, 36 ], 
> [ 1, 3, 10, 16, 21, 22, 31 ], [ 1, 5, 10, 16, 24, 25, 31 ], [ 1, 12, 16 ], 
> [ 1, 10, 12, 16, 29, 31, 35 ], [ 1, 2, 3, 16, 17, 20, 22 ] ];;
gap> l = SEMIGROUPS.LatticeOfXCongruences(S, "Right", rec(transrep:=true))![1];
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(l);

#E#
gap> STOP_TEST("Semigroups package: extreme/congpairs.tst");
