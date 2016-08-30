############################################################################
##
#W  semiffmat.gd
#Y  Copyright (C) 2013                                   James D. Mitchell
##                                                         Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##


DeclareOperation("RandomMatrixSemigroup", [IsRing, IsPosInt, IsPosInt]);
DeclareOperation("RandomMatrixMonoid", [IsRing, IsPosInt, IsPosInt]);
DeclareOperation("RandomMatrixSemigroup", [IsRing, IsPosInt, IsPosInt, IsList]);
DeclareOperation("RandomMatrixMonoid", [IsRing, IsPosInt, IsPosInt, IsList]);

DeclareSynonym("IsMatrixOverFiniteFieldSemigroup",
               IsMatrixOverFiniteFieldCollection and IsSemigroup);
DeclareSynonym("IsMatrixOverFiniteFieldMonoid",
               IsMatrixOverFiniteFieldCollection and IsMonoid);
DeclareAttribute("DegreeOfMatrixSemigroup", IsMatrixOverFiniteFieldSemigroup);
DeclareProperty("IsMatrixOverFiniteFieldSemigroupGreensClass", IsGreensClass);

InstallTrueMethod(CanComputeSize, IsMatrixOverFiniteFieldSemigroup);
InstallTrueMethod(IsFinite, IsMatrixOverFiniteFieldSemigroup);

# (mp) This is defined for groups, and already difficult there, so I
# guess close to impossible to do in matrix semigroups
DeclareProperty("IsFullMatrixSemigroup", IsSemigroup);
DeclareSynonymAttr("IsGeneralLinearSemigroup", IsFullMatrixSemigroup);

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

# Constructor for matrix semigroups, we want the flexibility
# to pass a list of generators, a field and a list of generators
# a field, a row/column dimension, and a list of generators.
DeclareGlobalFunction("MatrixSemigroup");

# This has to involve a field, otherwise we'll have to choose a default
# field (rationals?):
DeclareAttribute("IsomorphismMatrixSemigroup", IsSemigroup);
DeclareAttribute("AsMatrixSemigroup", IsSemigroup);
DeclareOperation("IsomorphismMatrixSemigroup", [IsSemigroup, IsRing]);
#T One would want to choose a representation as well, but this doesn't work
#DeclareOperation("IsomorphismMatrixSemigroup", [IsSemigroup, IsOperation]);
DeclareOperation("AsMatrixSemigroup", [IsSemigroup, IsRing]);
