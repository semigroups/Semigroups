#############################################################################
##
#W  utils.gd
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareGlobalFunction("CitrusMakeDoc");
DeclareGlobalFunction("CitrusMathJaxLocal");
DeclareGlobalFunction("CitrusMathJaxDefault");
DeclareGlobalFunction("CitrusTestAll");
DeclareGlobalFunction("CitrusTestInstall");
DeclareGlobalFunction("CitrusTestManualExamples");
DeclareGlobalFunction("CitrusReadTestManualExamples");
DeclareGlobalFunction("DClass");
DeclareGlobalFunction("DClassNC");

if not IsBound(Generators) then 
  DeclareAttribute("Generators", IsSemigroup);
fi;

DeclareGlobalFunction("HClass");
DeclareGlobalFunction("HClassNC");
DeclareGlobalFunction("LClass");
DeclareGlobalFunction("LClassNC");
DeclareGlobalFunction("RandomTransformationSemigroup");
DeclareGlobalFunction("RandomTransformationMonoid");
DeclareGlobalFunction("RClass");
DeclareGlobalFunction("RClassNC");

DeclareGlobalFunction("ReadCitrus");
DeclareGlobalFunction("WriteCitrus");
