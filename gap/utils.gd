#############################################################################
##
#W  utils.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareGlobalFunction("SemigroupsDir");
DeclareGlobalFunction("SemigroupsStartTest");
DeclareGlobalFunction("SemigroupsStopTest");
DeclareGlobalFunction("SemigroupsMakeDoc");
DeclareGlobalFunction("SemigroupsTestAll");
DeclareGlobalFunction("SemigroupsTestInstall");
DeclareGlobalFunction("SemigroupsTestManualExamples");
DeclareGlobalFunction("SemigroupsManualExamples");

BindGlobal("SemigroupsOmitFromTestManualExamples", []);
