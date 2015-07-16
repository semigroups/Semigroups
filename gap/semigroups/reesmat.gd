############################################################################
##
#W  reesmat.gd
#Y  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareGlobalFunction("RMSElementNC");
DeclareAttribute("RZMSGraph", IsReesZeroMatrixSemigroup, "mutable");
DeclareOperation("GreensHClassOfElement",
                 [IsReesZeroMatrixSemigroup, IsPosInt, IsPosInt]);
DeclareAttribute("MatrixEntries", IsReesZeroMatrixSemigroup);
DeclareAttribute("MatrixEntries", IsReesMatrixSemigroup);
