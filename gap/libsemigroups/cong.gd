############################################################################
##
##  libsemigroups/cong.gd
##  Copyright (C) 2022                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
############################################################################

# A congruence belongs to this category if it can use libsemigroups to compute
# things about itself.
DeclareCategory("CanComputeCppCongruence", IsAnyCongruenceCategory);

# The next operation is the only one supplied by libsemigroups/cong.gd/i that
# is exported.

DeclareOperation("CongruenceLessNC",
                 [CanComputeCppCongruence,
                  IsMultiplicativeElement,
                  IsMultiplicativeElement]);
