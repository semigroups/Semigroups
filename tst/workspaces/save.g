#############################################################################
##
#W  save.g
#Y  Copyright (C) 2023                                    James D Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

SetInfoLevel(InfoPackageLoading, 4);
LoadPackage("semigroups", false);
SetInfoLevel(InfoPackageLoading, 0);
QuitGap(SemigroupsTestInstall() and Test("tst/workspaces/save-workspace.tst"));
