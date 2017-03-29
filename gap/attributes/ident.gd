#############################################################################
##
#W  ident.gd
#Y  Copyright (C) 2017                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains operations related to identities that may hold for all
# elements of a semigroup.

DeclareOperation("VerifyIdentity",
                 [IsMultiplicativeElementCollection, IsHomogeneousList]);
DeclareOperation("RandomAssociativityTest",
                 [IsTable]);
