#############################################################################
##
#W  standard/acting.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/acting.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();
gap> SEMIGROUPS.DefaultOptionsRec.acting := true;;

# CosetMonoid
gap> CosetMonoid(SymmetricGroup(2));      
<transformation monoid of degree 3 with 2 generators>
gap> CosetMonoid(SymmetricGroup(3));      
<transformation monoid of degree 18 with 6 generators>
gap> CosetMonoid(SymmetricGroup(4));
<transformation monoid of degree 234 with 18 generators>

# UnbindVariables
gap> Unbind(S);
gap> Unbind(f);
gap> Unbind(gens);
gap> Unbind(iter);
gap> Unbind(r);
gap> Unbind(s);
gap> Unbind(x);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/acting.tst");
