#############################################################################
##
#W  semipperm.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#
gap> START_TEST("Semigroups package: semipperm.tst");
gap> LoadPackage("semigroups", false);;

#T# SemiPPerm1: NumberSubset
gap> sets := Combinations([1..10]);;
gap> Sort(sets, 
> function(x, y)
>    if Length(x) <> Length(y) then 
>      return Length(x) < Length(y);
>    fi;
>    return x < y;
>  end);
gap> List(sets, x-> NumberSubset(x, 10)) = [ 1 .. 2 ^ 10 ];
true

#
gap> SemigroupsStartTest();

#E#
gap> STOP_TEST( "Semigroups package: semipperm.tst");

