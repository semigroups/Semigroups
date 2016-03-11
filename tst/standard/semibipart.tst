#############################################################################
##
#W  standard/semibipart.tst
#Y  Copyright (C) 2015                                  James D. Mitchell
##                                                       
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semibipart.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#T# BruteForceIsoCheck helper functions
gap> BruteForceIsoCheck := function(iso)
>   local x, y;
>   if not IsInjective(iso) or not IsSurjective(iso) then
>     return false;
>   fi;
>   for x in Source(iso) do
>     for y in Source(iso) do
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

# IsomorphismSemigroup for IsBlockBijection checks if the semigroup is inverse
# inside the method and not in the filter to enter the method.
gap> S := ReesMatrixSemigroup(Group([(1,2)]), [[()]]);
<Rees matrix semigroup 1x1 over Group([ (1,2) ])>
gap> S := AsSemigroup(IsBlockBijectionSemigroup, S);
<block bijection group of degree 3 with 1 generator>

#E# 
gap> STOP_TEST("Semigroups package: standard/semibipart.tst");
