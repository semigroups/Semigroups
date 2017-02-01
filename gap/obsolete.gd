#############################################################################
##
#W  obsolete.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("RandomTransformationSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("RandomTransformationMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("RandomPartialPermSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("RandomPartialPermMonoid", [IsPosInt, IsPosInt]);

DeclareOperation("DotDClasses", [IsSemigroup]);
DeclareOperation("DotDClasses", [IsSemigroup, IsRecord]);

DeclareOperation("PartialTransformationSemigroup", [IsPosInt]);

DeclareOperation("AsPartialPermSemigroup", [IsSemigroup]);
DeclareOperation("AsTransformationSemigroup", [IsSemigroup]);
DeclareOperation("AsBipartitionSemigroup", [IsSemigroup]);
DeclareOperation("AsBlockBijectionSemigroup", [IsSemigroup]);

DeclareOperation("IsomorphismBipartitionSemigroup", [IsSemigroup]);
DeclareOperation("IsomorphismBlockBijectionSemigroup", [IsSemigroup]);

DeclareOperation("FactorisableDualSymmetricInverseSemigroup", [IsPosInt]);
DeclareOperation("SingularFactorisableDualSymmetricInverseSemigroup",
                 [IsPosInt]);

DeclareOperation("IsSynchronizingTransformationCollection",
                 [IsTransformationCollection, IsPosInt]);
