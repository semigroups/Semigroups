#############################################################################
##
##  semigroups/semitrans.gd
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("\^", [IsTransformationCollection, IsPerm]);
DeclareAttribute("FixedPointsOfTransformationSemigroup",
                 IsTransformationSemigroup);

DeclareOperation("DigraphOfAction",
                 [IsTransformationCollection, IsList, IsFunction]);
DeclareAttribute("DigraphOfActionOnPoints",
                 IsTransformationCollection);
DeclareOperation("DigraphOfActionOnPoints",
                 [IsTransformationCollection, IsInt]);

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

DeclareAttribute("DigraphCore", IsDigraph);

DeclareOperation("WreathProduct",
                 [IsMultiplicativeElementCollection,
                  IsMultiplicativeElementCollection]);

DeclareAttribute("SmallestElementRClass", IsGreensRClass);
DeclareAttribute("LargestElementRClass", IsGreensRClass);
