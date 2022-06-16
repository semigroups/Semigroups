############################################################################
##
##  semigroups/semiffmat.gd
##  Copyright (C) 2013-2022                              James D. Mitchell
##                                                         Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsMatrixOverFiniteFieldSemigroup", IsSemigroup);
DeclareSynonym("IsMatrixOverFiniteFieldMonoid",
               IsMonoid and IsMatrixOverFiniteFieldSemigroup);

InstallTrueMethod(CanComputeSize, IsMatrixOverFiniteFieldSemigroup);
InstallTrueMethod(IsFinite, IsMatrixOverFiniteFieldSemigroup);

DeclareAttribute("BaseDomain", IsMatrixOverFiniteFieldSemigroup);
DeclareAttribute("BaseDomain", IsMatrixOverFiniteFieldMonoid);

# (mp) This is defined for groups, and already difficult there, so I
# guess close to impossible to do in matrix semigroups
DeclareProperty("IsFullMatrixMonoid", IsSemigroup);
DeclareSynonymAttr("IsGeneralLinearMonoid", IsFullMatrixMonoid);

# Right action of a matrix over a field on a row space
DeclareGlobalFunction("MatrixOverFiniteFieldRowSpaceRightAction");
# Given two H-related matrices x and y, computes the element
# of the schutzenberger group of the matrices' H-class
# that maps im x to im y. Returns an invertible matrix
DeclareGlobalFunction("MatrixOverFiniteFieldSchutzGrpElement");
DeclareGlobalFunction("MatrixOverFiniteFieldStabilizerAction");
DeclareGlobalFunction("MatrixOverFiniteFieldLambdaConjugator");
DeclareGlobalFunction("MatrixOverFiniteFieldIdempotentTester");
DeclareGlobalFunction("MatrixOverFiniteFieldIdempotentCreator");
DeclareGlobalFunction("MatrixOverFiniteFieldLocalRightInverse");
