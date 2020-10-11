#############################################################################
##
##  libsemigroups/fpsemi.gd
##  Copyright (C) 2022                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file exists to construct a libsemigroups FpSemigroup object for an fp
# semigroup or monoid. The resulting libsemigroups::FpSemigroup object is only
# used to initialize a libsemigroups::Congruence object where most questions
# asked about FpSemigroup/FpMonoids will be answered.

DeclareGlobalFunction("LibsemigroupsFpSemigroup");
