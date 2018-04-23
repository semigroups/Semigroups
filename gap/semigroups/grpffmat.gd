############################################################################
##
##  grpffmat.gd
##  Copyright (C) 2013-15                                James D. Mitchell
##                                                       Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains everything required to create and manipulate groups of
# matrices over finite fields. These essentially just delegate to the
# library methods via an isomorphism.

DeclareSynonym("IsMatrixOverFiniteFieldGroup",
               IsGroup and IsMatrixOverFiniteFieldSemigroup);

DeclareAttribute("IsomorphismMatrixGroup",
                 IsMatrixOverFiniteFieldSemigroup);
DeclareAttribute("AsMatrixGroup",
                 IsMatrixOverFiniteFieldSemigroup);

DeclareOperation("\^", [IsMatrixOverFiniteFieldGroup, IsMatrixOverFiniteField]);
