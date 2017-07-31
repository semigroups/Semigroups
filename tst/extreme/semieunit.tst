#############################################################################
##
#W  extreme/semieunit.tst
#Y  Copyright (C) 2016                                    Christopher Russell 
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semieunit.tst");
gap> LoadPackage("semigroups", false);;
gap> LoadPackage("Digraphs", false);;
gap> LoadPackage("smallsemi", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

#T# McAlisterTriple
gap> inv_semigroups := AllSmallSemigroups([2 .. 7], IsInverseSemigroup, true);;
#I  Smallsemi: loading data for semigroup properties. Please be patient.
#I  Smallsemi: loading data for semigroups of size 7.
gap> e_unitary_semigroups := Filtered(inv_semigroups, IsEUnitaryInverseSemigroup);;
gap> triples := ShallowCopy(e_unitary_semigroups);;
gap> Apply(triples, a -> AsSemigroup(IsPartialPermSemigroup, a));;
gap> Apply(triples, a -> IsomorphismMcAlisterTriple(a));;
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
gap> Apply(triples, a -> BruteForceIsoCheck(a));;
gap> false in triples;
false

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(e_unitary_semigroups);
gap> Unbind(inv_semigroups);
gap> Unbind(triples);

#E#
gap> STOP_TEST("Semigroups package: extreme/eunittest.tst");