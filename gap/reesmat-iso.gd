############################################################################
##
#W  reesmat-iso.gd
#Y  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareGlobalFunction("RMSInducedFunction");
DeclareGlobalFunction("RZMSInducedFunction");
DeclareGlobalFunction("RZMStoRZMSInducedFunction");

DeclareCategory("IsRMSIsoByTriple",  IsGeneralMapping and IsSPGeneralMapping and
   IsTotal and IsSingleValued and IsInjective and IsSurjective and
   IsAttributeStoringRep);
DeclareCategory("IsRZMSIsoByTriple", IsGeneralMapping and IsSPGeneralMapping and
   IsTotal and IsSingleValued and IsInjective and IsSurjective and
   IsAttributeStoringRep);

DeclareGlobalFunction("RMSIsoByTriple");
DeclareGlobalFunction("RZMSIsoByTriple");

DeclareOperation("ELM_LIST", [IsRMSIsoByTriple, IsPosInt]);
DeclareOperation("ELM_LIST", [IsRZMSIsoByTriple, IsPosInt]);

DeclareProperty("IsAutomorphismGroupOfRMS", IsAutomorphismGroup);
DeclareProperty("IsAutomorphismGroupOfRZMS", IsAutomorphismGroup);

DeclareGlobalFunction("HashFunctionMatrixOfRMS");
