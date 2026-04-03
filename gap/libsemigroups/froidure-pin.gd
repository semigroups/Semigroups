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

DeclareGlobalFunction("LibsemigroupsFroidurePin");

# We increment the rank below so that CanUseLibsemigroupsFroidurePin is used in
# preference to CanUseGapFroidurePin, because o/w they have equal rank.
DeclareProperty("CanUseLibsemigroupsFroidurePin", IsSemigroup, 2);
DeclareOperation("HasLibsemigroupsFroidurePin", [IsSemigroup]);

DeclareOperation("FroidurePinMemFnRec", [IsSemigroup]);
DeclareOperation("FroidurePinMemFnRec",
                 [IsSemigroup, IsListOrCollection]);
