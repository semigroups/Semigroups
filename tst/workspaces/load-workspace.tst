#############################################################################
##
#W  workspaces/load-workspace.tst
#Y  Copyright (C) 2016                                      Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
##  This file, together with save-workspace.tst, tests the ability to save
##  and load workspaces which include Semigroups objects.  Objects should be
##  created and stored in save-workspace.tst, and then loaded and tested in
##  load-workspace.tst to ensure that they work correctly after being saved.
##
#############################################################################

# Set up testing environment
gap> START_TEST("Semigroups package: workspaces/load-workspace.tst");
gap> SEMIGROUPS.StartTest();

#############################################################################
##  Test objects that were created in save-workspace.tst, using the same
##  variable names.  Give the test blocks the same headers as in the previous
##  file.
#############################################################################

#T# Union-find structures
gap> UF_TABLE(uftable1);
[ 1, 2, 3, 2, 5, 6, 7, 8, 2, 10 ]
gap> UF_TABLE(uftable2);
[ 1 ]
gap> UF_FIND(uftable3, 17);
17
gap> UF_FIND(uftable3, 20222);
234
gap> UF_FIND(uftable3, 234);
234

# Test congruences
gap> NrEquivalenceClasses(cong);
4
gap> [Matrix(IsBooleanMat, [[0, 1], [1, 0]]), 
>     Matrix(IsBooleanMat, [[0, 1], [1, 0]])] in cong;
true
gap> [Matrix(IsBooleanMat, [[0, 1], [1, 1]]), 
>     Matrix(IsBooleanMat, [[0, 1], [1, 0]])] in cong;
true
gap> [Matrix(IsBooleanMat, [[0, 0], [0, 0]]), 
>     Matrix(IsBooleanMat, [[0, 1], [1, 0]])] in cong;
false
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 1, 2, 1, 1, 3, 1, 1, 2, 1, 3, 1, 1, 2, 4, 3 ]

#############################################################################
##  Tests end here
#############################################################################

# SEMIGROUPS_UnbindVariables
gap> Unbind(uftable1);
gap> Unbind(uftable2);
gap> Unbind(uftable3);

#E#
gap> STOP_TEST("Semigroups package: workspaces/load-workspace.tst");
