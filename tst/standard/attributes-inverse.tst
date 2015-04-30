#%T##########################################################################
##
#W  attributes-inverse.tst
#Y  Copyright (C) 2015                                   Wilfred A. Wilson
##
##  Licensing information can be found in the README file of this package.
#
#############################################################################
##
gap> START_TEST("Semigroups package: attributes-inverse.tst");
gap> LoadPackage("semigroups", false);;

# 
gap> SemigroupsStartTest();

#T#
gap> S := InverseSemigroup([
>   PartialPerm([2, 3, 4, 1]),
>   PartialPerm([2, 1, 3, 4]),
>   PartialPerm([1, 2, 3, 0])
> ]);;
gap> Size(S);
209
gap> Size(S) = Size(SymmetricInverseMonoid(4));
true
gap> iso := VagnerPrestonRepresentation(S);;
gap> DegreeOfPartialPermSemigroup(Range(iso));
209

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(iso);

#E#
gap> STOP_TEST("Semigroups package: attributes-inverse.tst");
