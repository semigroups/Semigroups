#############################################################################
##
#W  utils.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareGlobalFunction("SemigroupsDir");
DeclareGlobalFunction("SemigroupsStartTest");
DeclareGlobalFunction("SemigroupsStopTest");
DeclareGlobalFunction("SemigroupsMakeDoc");
DeclareGlobalFunction("SemigroupsMathJaxLocal");
DeclareGlobalFunction("SemigroupsMathJaxDefault");
DeclareGlobalFunction("SemigroupsTestAll");
DeclareGlobalFunction("SemigroupsTestInstall");
DeclareGlobalFunction("SemigroupsTestManualExamples");
DeclareGlobalFunction("SemigroupsManualExamples");

DeclareGlobalFunction("GeneratorsReadFile");
DeclareGlobalFunction("ReadGeneratorsLine");
DeclareGlobalFunction("ReadGenerators");
DeclareGlobalFunction("GeneratorsWriteFile");
DeclareGlobalFunction("WriteGeneratorsLine");
DeclareGlobalFunction("WriteGenerators");
DeclareGlobalFunction("IteratorFromGeneratorsFile");

DeclareOperation("ShortStringRep", [IsTransformation]);

BindGlobal("SemigroupsOmitFromTestManualExamples", []);

