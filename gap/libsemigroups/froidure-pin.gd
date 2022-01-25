###########################################################################
##
##  libsemigroups/froidure-pin.gd
##  Copyright (C) 2022                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains declarations for the interface with
# libsemigroups::FroidurePin

DeclareGlobalFunction("CppFroidurePin");

DeclareProperty("CanComputeCppFroidurePin", IsSemigroup);
DeclareOperation("HasCppFroidurePin", [IsSemigroup]);
DeclareProperty("IsEnumerated", IsSemigroup);
