#############################################################################
##
#W  standard/greens/greens-regular.tst
#Y  Copyright (C) 2015                                   Wilfred A. Wilson
##                                                       
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/greens/greens-regular.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# greens-regular: RhoCosets, for a regular Greens class of an acting semigroup, 1
gap> S := Semigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])], rec(generic := false));;
gap> IsRegularSemigroup(S);
true
gap> L := LClass(S, S.1);;
gap> HasIsRegularClass(L) and IsRegularClass(L);
true
gap> RhoCosets(L);
[ () ]

# greens-regular: LambdaCosets, for a regular Greens class of an acting semigroup, 1
gap> S := Semigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])], rec(generic := false));;
gap> IsRegularSemigroup(S);
true
gap> L := LClass(S, S.1);;
gap> HasIsRegularClass(L) and IsRegularClass(L);
true
gap> LambdaCosets(L);
[ () ]

# greens-regular: SchutzenbergerGroup, for a regular acting D-class, 1
gap> S := Semigroup([
> Transformation([1, 2, 3, 3, 1]),
> Transformation([2, 4, 3, 5, 5]),
> Transformation([5, 1, 2, 5, 3])], rec(generic := false));;
gap> IsRegularSemigroup(S);
true
gap> D := DClass(S, S.1);;
gap> SchutzenbergerGroup(D);
Group([ (1,2), (1,3,2) ])

# greens-regular: SchutzenbergerGroup, for an H-class of a regular acting semigroup, 1

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(D);
gap> Unbind(L);

#E# 
gap> STOP_TEST("Semigroups package: standard/greens/greens-regular.tst");
