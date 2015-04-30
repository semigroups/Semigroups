#%T##########################################################################
##
#W  smallerdegree.tst
#Y  Copyright (C) 2012-15                                  Wilfred Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: smallerdegree.tst");
gap> LoadPackage("semigroups", false);;

# 
gap> SemigroupsStartTest();

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(f1);
gap> Unbind(f2);
gap> Unbind(f3);
gap> Unbind(inv);
gap> Unbind(VPR);
gap> Unbind(I5);
gap> Unbind(B);
gap> Unbind(F);
gap> Unbind(J);
gap> Unbind(L);
gap> Unbind(S);
gap> Unbind(T);
gap> Unbind(rho);
gap> Unbind(V);
gap> Unbind(g);
gap> Unbind(f);
gap> Unbind(H2);
gap> Unbind(h);
gap> Unbind(H1);
gap> Unbind(iso);
gap> Unbind(y);
gap> Unbind(x);

#E#
gap> STOP_TEST("Semigroups package: smallerdegree.tst");
