#############################################################################
##
#W  standard/uf.tst
#Y  Copyright (C) 2018                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/uf.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# StructuralCopy and IsMutable 
gap> uf := UF_NEW(10);
<wrapper for instance of C++ UF class>
gap> StructuralCopy(uf);
<wrapper for instance of C++ UF class>
gap> IsIdenticalObj(uf, last);
false
gap> IsMutable(uf);
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/uf.tst");
