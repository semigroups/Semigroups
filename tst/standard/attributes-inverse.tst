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

#T# attributes-inverse: VagnerPrestonRepresentation, symmetric inv monoid 4 1/1
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

#T# attributes-inverse: SameMinorantsSubgroup, symmetric inv monoid 5 1/3
gap> S := SymmetricInverseSemigroup(5);;
gap> h := HClass(S, One(S));
<Green's H-class: <identity partial perm on [ 1, 2, 3, 4, 5 ]>>
gap> SameMinorantsSubgroup(h);
[ <identity partial perm on [ 1, 2, 3, 4, 5 ]> ]
gap> h := HClass(S, PartialPerm([1, 2, 0, 0, 0]));
<Green's H-class: <identity partial perm on [ 1, 2 ]>>
gap> SameMinorantsSubgroup(h);
[ <identity partial perm on [ 1, 2 ]> ]
gap> h := HClass(S, MultiplicativeZero(S));
<Green's H-class: <empty partial perm>>
gap> SameMinorantsSubgroup(h);
[ <empty partial perm> ]

#T# attributes-inverse: SameMinorantsSubgroup, partial perm semigroup, 2/3
gap> S;;

#T# attributes-inverse: SameMinorantsSubgroup, try next method 3/3
gap> S := AsTransformationSemigroup(SymmetricInverseSemigroup(5));;
gap> h := HClass(S, One(S));
<Green's H-class: IdentityTransformation>
gap> SameMinorantsSubgroup(h);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `NaturalLeqInverseSemigroup' on 2 argume\
nts

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(iso);
gap> Unbind(h);

#E#
gap> STOP_TEST("Semigroups package: attributes-inverse.tst");
