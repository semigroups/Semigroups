#############################################################################
##
#W  workspaces/save-workspace.tst
#Y  Copyright (C) 2016                                      Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
##  This file, together with load-workspace.tst, tests the ability to save
##  and load workspaces which include Semigroups objects.  Objects should be
##  created and stored in save-workspace.tst, and then loaded and tested in
##  load-workspace.tst to ensure that they work correctly after being saved.
##
#############################################################################

# Set up testing environment
gap> START_TEST("Semigroups package: workspaces/save-workspace.tst");
gap> SetInfoLevel(InfoDebug, 0);
gap> LoadPackage("semigroups", false);;
gap> SEMIGROUPS.StartTest();

#############################################################################
##  Create objects below here, and add tests to load-workspace.tst to ensure
##  that they are saved to disk correctly.  Do not reuse variable names.
#############################################################################

#T# Union-find structures
gap> uftable1 := UF_NEW(10);;
gap> UF_UNION(uftable1, [4, 9]);;
gap> UF_UNION(uftable1, [4, 2]);;
gap> uftable2 := UF_NEW(1);;
gap> uftable3 := UF_NEW(30000);;
gap> UF_UNION(uftable3, [20222, 234]);;

# Test congruences
gap> S := Semigroup(Matrix(IsBooleanMat, [[0, 1], [1, 0]]), 
>                   Matrix(IsBooleanMat, [[1, 0], [1, 1]]), 
>                   Matrix(IsBooleanMat, [[1, 0], [0, 0]]), 
>                   Matrix(IsBooleanMat, [[1, 0], [0, 0]]));;
gap> cong := LeftSemigroupCongruence(S, [S.1, S.2]);;
gap> NrEquivalenceClasses(cong);
4
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 1, 2, 1, 1, 3, 1, 1, 2, 1, 3, 1, 1, 2, 4, 3 ]

#############################################################################
##  Tests end here
#############################################################################

# Save the workspace
gap> SaveWorkspace(Concatenation(SEMIGROUPS.PackageDir,
>                                "/tst/workspaces/test-output.w"));
true

# SEMIGROUPS_UnbindVariables
gap> Unbind(uftable1);
gap> Unbind(uftable2);
gap> Unbind(uftable3);

#E#
gap> STOP_TEST("Semigroups package: workspaces/save-workspace.tst");
