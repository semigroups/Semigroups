#############################################################################
##
#W  convenience.gd
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareGlobalFunction("CitrusDefaultMem");
DeclareGlobalFunction("CitrusLoMem");
DeclareGlobalFunction("CitrusHiMem");
DeclareGlobalFunction("CitrusVeryLoMem");
DeclareGlobalFunction("CitrusMakeDoc");
DeclareGlobalFunction("CitrusTestAll");
DeclareGlobalFunction("CitrusTestInstall");
DeclareGlobalFunction("CitrusTestManualExamples");
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

