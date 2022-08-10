###########################################################################
##
##  libsemigroups/sims1.gd
##  Copyright (C) 2022                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains declarations for the interface with
# libsemigroups::Sims1 and related.

DeclareCategory("IsWordGraph", IsImmutableDigraph);

DeclareOperation("NumberOfRightCongruences",
                 [IsSemigroup, IsPosInt, IsListOrCollection]);
DeclareOperation("NumberOfLeftCongruences",
                 [IsSemigroup, IsPosInt, IsListOrCollection]);
DeclareOperation("NumberOfLeftCongruences", [IsSemigroup, IsPosInt]);
DeclareOperation("NumberOfRightCongruences", [IsSemigroup, IsPosInt]);
DeclareAttribute("NumberOfRightCongruences", IsSemigroup);
DeclareAttribute("NumberOfLeftCongruences", IsSemigroup);

DeclareOperation("IteratorOfRightCongruences",
                 [IsSemigroup, IsPosInt, IsListOrCollection]);
DeclareOperation("IteratorOfLeftCongruences",
                 [IsSemigroup, IsPosInt, IsListOrCollection]);

DeclareOperation("IteratorOfRightCongruences",
                 [IsSemigroup, IsPosInt]);
DeclareOperation("IteratorOfLeftCongruences",
                 [IsSemigroup, IsPosInt]);

DeclareOperation("IteratorOfRightCongruences",
                 [IsSemigroup]);
DeclareOperation("IteratorOfLeftCongruences",
                 [IsSemigroup]);
