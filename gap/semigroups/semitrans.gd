#############################################################################
##
#W  semitrans.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("\^", [IsTransformationCollection, IsPerm]);
DeclareAttribute("FixedPoints", IsTransformationSemigroup);
DeclareAttribute("DigraphOfActionOnPoints", IsTransformationSemigroup);
DeclareOperation("DigraphOfActionOnPoints", [IsTransformationSemigroup, IsPosInt]);
DeclareProperty("IsTransformationSemigroupGreensClass", IsGreensClass);
DeclareOperation("AsTransformationSemigroup", [IsSemigroup]);

DeclareAttribute("ComponentRepsOfTransformationSemigroup",
                 IsTransformationSemigroup);
DeclareAttribute("ComponentsOfTransformationSemigroup",
                 IsTransformationSemigroup);
DeclareAttribute("CyclesOfTransformationSemigroup",
                 IsTransformationSemigroup);

DeclareOperation("IsSynchronizingSemigroup", [IsTransformationSemigroup]);
DeclareOperation("IsSynchronizingSemigroup",
                 [IsTransformationSemigroup, IsPosInt]);
DeclareOperation("IsSynchronizingTransformationCollection",
                 [IsTransformationCollection, IsPosInt]);

DeclareProperty("IsTransitive", IsTransformationSemigroup);
DeclareOperation("IsTransitive", [IsTransformationCollection, IsPosInt]);
DeclareOperation("IsTransitive", [IsTransformationCollection, IsList]);

DeclareAttribute("SmallestElementSemigroup", IsSemigroup);
DeclareAttribute("LargestElementSemigroup", IsSemigroup);

DeclareAttribute("EndomorphismMonoid", IsDigraph);
DeclareOperation("EndomorphismMonoid", [IsDigraph, IsHomogeneousList]);

