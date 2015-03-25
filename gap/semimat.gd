############################################################################
##
#W  semimat.gd
#Y  Copyright (C) 2013                                   James D. Mitchell
##                                                         Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## Some of this code is based on code taken from grpmat.gd in the GAP
## library

DeclareSynonym("IsMatrixSemigroup", IsSMatrixCollection and IsSemigroup);
DeclareAttribute("DegreeOfMatrixSemigroup", IsMatrixSemigroup);
DeclareAttribute("BaseDomain", IsMatrixSemigroup);
DeclareProperty("IsMatrixSemigroupGreensClass", IsGreensClass);

InstallTrueMethod(CanComputeSize, IsMatrixSemigroup and IsFinite);

# (mp) This is defined for groups, and already difficult there, so I
# guess close to impossible to do in matrix semigroups
DeclareProperty("IsFullMatrixSemigroup", IsSemigroup);
DeclareSynonymAttr("IsGeneralLinearSemigroup", IsFullMatrixSemigroup);

#DeclareGlobalFunction("ComputeRowSpaceAndTransformation");
#DeclareOperation("CanonicalRowSpace", [IsMatrixObj and IsFFECollColl]);
#DeclareOperation("RowSpaceTransformation", [IsMatrixObj and IsFFECollColl]);
#DeclareOperation("RowSpaceTransformationInv",
#[IsMatrixObj and IsFFECollColl]);

# Right action of a matrix over a field on a row space
DeclareGlobalFunction("SMatrixRowSpaceRightAction");
# Given a row space rsp and a matrix mat, computes
# a matrix mat' such that rsp.(mat * mat') = id_rsp
DeclareGlobalFunction("SMatrixLocalRightInverse");
# Given two H-related matrices x and y, computes the element
# of the schutzenberger group of the matrices' H-class
# that maps im x to im y. Returns an invertible matrix
DeclareGlobalFunction("SMatrixSchutzGrpElement");
DeclareGlobalFunction("SMatrixStabilizerAction");
DeclareGlobalFunction("SMatrixLambdaConjugator");
DeclareGlobalFunction("SMatrixIdempotentTester");
DeclareGlobalFunction("SMatrixIdempotentCreator");

# Constructor for matrix semigroups, we want the flexibility
# to pass a list of generators, a field and a list of generators
# a field, a row/column dimension, and a list of generators.
DeclareGlobalFunction("MatrixSemigroup");

# This has to involve a field, otherwise we'll have to choose a default
# field (rationals?):
DeclareAttribute("IsomorphismMatrixSemigroup", IsSemigroup);
DeclareAttribute("AsMatrixSemigroup", IsSemigroup);
DeclareOperation("IsomorphismMatrixSemigroup", [IsSemigroup, IsRing]);
DeclareOperation("AsMatrixSemigroup", [IsSemigroup, IsRing]);
