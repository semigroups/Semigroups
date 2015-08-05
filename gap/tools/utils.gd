#############################################################################
##
#W  utils.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareGlobalFunction("SemigroupsMakeDoc");
DeclareGlobalFunction("SemigroupsTestInstall");
DeclareGlobalFunction("SemigroupsTestStandard");

DeclareGlobalFunction("SEMIGROUPS_StartTest");
DeclareGlobalFunction("SEMIGROUPS_StopTest");
DeclareGlobalFunction("SEMIGROUPS_TestAll");
DeclareGlobalFunction("SEMIGROUPS_TestManualExamples");
DeclareGlobalFunction("SEMIGROUPS_ManualExamples");
DeclareGlobalFunction("SEMIGROUPS_Test");

BindGlobal("SEMIGROUPS_OmitFromTests", []);
