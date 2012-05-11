#############################################################################
##
#W  utils.gd
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareGlobalFunction("CitrusDir");
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
DeclareGlobalFunction("IteratorByIterator");
DeclareGlobalFunction("LClass");
DeclareGlobalFunction("LClassNC");
DeclareGlobalFunction("ListByIterator");
DeclareGlobalFunction("RClass");
DeclareGlobalFunction("RClassNC");

DeclareGlobalFunction("ReadCitrus");
DeclareGlobalFunction("ReadCitrusLine");
DeclareGlobalFunction("ReadCitrusLinePP");
DeclareGlobalFunction("WriteCitrus");

DeclareGlobalFunction("CitrusIsNotCompiled");

DeclareAttribute("LambdaPt", IsObject);
DeclareAttribute("RhoPt", IsObject);
DeclareAttribute("LambdaOrb", IsSemigroup);
DeclareAttribute("RhoOrb", IsSemigroup);
