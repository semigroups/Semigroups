############################################################################
##
#W  reesmat-iso.gd
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareGlobalFunction("RMSInducedFunction");
DeclareGlobalFunction("RZMSInducedFunction");
DeclareGlobalFunction("RZMStoRZMSInducedFunction");

DeclareCategory("IsRMSIsoByTriple",  IsGeneralMapping and IsSPGeneralMapping and 
   IsTotal and IsSingleValued and IsInjective and IsSurjective);
DeclareCategory("IsRZMSIsoByTriple", IsGeneralMapping and IsSPGeneralMapping and 
   IsTotal and IsSingleValued and IsInjective and IsSurjective );

DeclareGlobalFunction("RMSIsoByTriple");
DeclareGlobalFunction("RZMSIsoByTriple");

