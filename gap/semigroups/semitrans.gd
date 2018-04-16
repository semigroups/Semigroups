#############################################################################
##
##  semitrans.gd
##  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("\^", [IsTransformationCollection, IsPerm]);
DeclareAttribute("FixedPointsOfTransformationSemigroup",
                 IsTransformationSemigroup);
DeclareAttribute("DigraphOfActionOnPoints", IsTransformationSemigroup);
DeclareOperation("DigraphOfActionOnPoints",
                 [IsTransformationSemigroup, IsInt]);
DeclareAttribute("DigraphOfActionOnPairs", IsTransformationSemigroup);
DeclareOperation("DigraphOfActionOnPairs",
                 [IsTransformationSemigroup, IsInt]);

DeclareAttribute("ComponentRepsOfTransformationSemigroup",
                 IsTransformationSemigroup);
DeclareAttribute("ComponentsOfTransformationSemigroup",
                 IsTransformationSemigroup);
DeclareProperty("IsConnectedTransformationSemigroup",
                 IsTransformationSemigroup);
DeclareAttribute("CyclesOfTransformationSemigroup",
                 IsTransformationSemigroup);

DeclareProperty("IsSynchronizingSemigroup", IsTransformationSemigroup);
DeclareOperation("IsSynchronizingSemigroup",
                 [IsTransformationSemigroup, IsPosInt]);

DeclareProperty("IsTransitive", IsTransformationSemigroup);
DeclareOperation("IsTransitive", [IsTransformationCollection, IsPosInt]);
DeclareOperation("IsTransitive", [IsTransformationCollection, IsList]);

DeclareAttribute("EndomorphismMonoid", IsDigraph);
DeclareOperation("EndomorphismMonoid", [IsDigraph, IsHomogeneousList]);

DeclareOperation("DirectProductOp", [IsList, IsSemigroup]);
