############################################################################
##
##  semigroups/reesmat.gd
##  Copyright (C) 2014-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareGlobalFunction("RMSElementNC");
DeclareOperation("GreensHClassOfElement",
                 [IsReesZeroMatrixSemigroup, IsPosInt, IsPosInt]);
DeclareAttribute("MatrixEntries", IsReesZeroMatrixSemigroup);
DeclareAttribute("MatrixEntries", IsReesMatrixSemigroup);
DeclareAttribute("RZMSDigraph", IsReesZeroMatrixSemigroup);
DeclareAttribute("RZMSConnectedComponents", IsReesZeroMatrixSemigroup);
DeclareAttribute("RZMSNormalization", IsReesZeroMatrixSemigroup);
DeclareAttribute("RMSNormalization", IsReesMatrixSemigroup);
