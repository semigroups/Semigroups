#############################################################################
##
##  libsemigroups/presentation.gd
##  Copyright (C) 2025                                      Joseph Edwards
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file exists to construct a libsemigroups Presentation object for an fp
# semigroup or monoid. The resulting libsemigroups::Presentation object is only
# used to initialize a libsemigroups::Congruence object where most questions
# asked about Presentation/FpMonoids will be answered.

DeclareGlobalFunction("LibsemigroupsPresentation");
