###########################################################################
##
##  cong.gd
##  Copyright (C) 2021                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# TODO(now) move the entire content of this file into the cong.gi file, since
# we don't use any of these things outside that file they should be there

# A congruence belongs to this category if it can use libsemigroups to compute
# things about itself.
DeclareCategory("CanComputeCppCongruence", IsAnyCongruenceCategory);

# A semigroup satisfies this property if its congruences should belong to
# CanComputeCppCongruence.
DeclareProperty("CanComputeCppCongruences", IsSemigroup);

DeclareAttribute("AnyCongruenceKindString", CanComputeCppCongruence);

DeclareGlobalFunction("CppCongruence");

DeclareOperation("CongruenceWordToClassIndex",
                 [CanComputeCppCongruence, IsHomogeneousList]);
DeclareOperation("CongruenceWordToClassIndex",
                 [CanComputeCppCongruence, IsMultiplicativeElement]);

DeclareOperation("CongruenceLessNC",
                 [CanComputeCppCongruence,
                  IsMultiplicativeElement,
                  IsMultiplicativeElement]);
