#############################################################################
##
#W  utils.gd
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareGlobalFunction("SemigroupsDir");
DeclareGlobalFunction("SemigroupsMakeDoc");
DeclareGlobalFunction("SemigroupsMathJaxLocal");
DeclareGlobalFunction("SemigroupsMathJaxDefault");
DeclareGlobalFunction("SemigroupsTestAll");
DeclareGlobalFunction("SemigroupsTestInstall");
DeclareGlobalFunction("SemigroupsTestManualExamples");
DeclareGlobalFunction("SemigroupsReadTestManualExamples");

if not IsBound(Generators) then 
  DeclareAttribute("Generators", IsSemigroup);
fi;

DeclareGlobalFunction("ReadSemigroups");
DeclareGlobalFunction("ReadSemigroupsLine");
DeclareGlobalFunction("ReadSemigroupsLinePP");
DeclareGlobalFunction("WriteSemigroups");

