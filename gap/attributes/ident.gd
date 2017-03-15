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
DeclareOperation("NrLettersIdentity",
                 [IsHomogeneousList]);
DeclareOperation("ReverseIdentity",
                 [IsHomogeneousList]);
DeclareOperation("RandomTable",
                 [IsPosInt]);
DeclareOperation("RandomTuple",
                 [IsPosInt]);
DeclareOperation("GroupAlgebraProduct",
                 [IsTable, IsHomogeneousList, IsHomogeneousList]);
DeclareOperation("RandomAssociativityTest",
                 [IsTable]);
