#############################################################################
##
##  homomorph.gd
##  Copyright (C) 2022                               Artemis Konstantinidi
##                                                         Chinmaya Nagpal
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsSemigroupHomomorphismByImagesOrFunction",
                IsSemigroupGeneralMapping
                and IsSingleValued
                and IsTotal
                and IsAttributeStoringRep,
                []);

DeclareRepresentation("IsSemigroupHomomorphismByImages",
                      IsSPGeneralMapping
                      and IsSemigroupHomomorphismByImagesOrFunction,
                      []);

DeclareRepresentation("IsSemigroupHomomorphismByFunction",
                      IsSPMappingByFunctionRep
                      and IsSemigroupHomomorphismByImagesOrFunction,
                      []);

DeclareRepresentation("IsSemigroupIsomorphismByFunction",
                      IsSemigroupHomomorphismByFunction
                      and IsBijective
                      and IsSPMappingByFunctionWithInverseRep,
                      []);

# This operation is not called SemigroupHomomorphismByImagesNC because that
# name is used in the main GAP library

DeclareOperation("SemigroupHomomorphismByImages_NC",
                 [IsSemigroup, IsSemigroup, IsList, IsList]);
DeclareOperation("SemigroupHomomorphismByImages",
                 [IsSemigroup, IsSemigroup, IsList, IsList]);
DeclareOperation("SemigroupHomomorphismByImages",
                 [IsSemigroup, IsSemigroup, IsList]);
DeclareOperation("SemigroupHomomorphismByImages",
                 [IsSemigroup, IsSemigroup]);
DeclareOperation("SemigroupHomomorphismByImages",
                 [IsSemigroup, IsList, IsList]);

DeclareOperation("SemigroupIsomorphismByImagesNC",
                 [IsSemigroup, IsSemigroup, IsList, IsList]);
DeclareOperation("SemigroupIsomorphismByImages",
                 [IsSemigroup, IsSemigroup, IsList, IsList]);
DeclareOperation("SemigroupIsomorphismByImages",
                 [IsSemigroup, IsSemigroup, IsList]);
DeclareOperation("SemigroupIsomorphismByImages",
                 [IsSemigroup, IsSemigroup]);
DeclareOperation("SemigroupIsomorphismByImages",
                 [IsSemigroup, IsList, IsList]);

DeclareOperation("SemigroupIsomorphismByFunctionNC",
                 [IsSemigroup, IsSemigroup, IsFunction, IsFunction]);
DeclareOperation("SemigroupIsomorphismByFunction",
                 [IsSemigroup, IsSemigroup, IsFunction, IsFunction]);

DeclareOperation("SemigroupHomomorphismByFunctionNC",
                 [IsSemigroup, IsSemigroup, IsFunction]);
DeclareOperation("SemigroupHomomorphismByFunction",
                 [IsSemigroup, IsSemigroup, IsFunction]);

DeclareAttribute("KernelOfSemigroupHomomorphism", IsSemigroupHomomorphism);

DeclareOperation("AsSemigroupHomomorphismByFunction",
                 [IsSemigroupHomomorphismByImages]);
DeclareOperation("AsSemigroupHomomorphismByFunction",
                 [IsSemigroupHomomorphismByFunction]);
DeclareOperation("AsSemigroupHomomorphismByImages",
                 [IsSemigroupHomomorphismByFunction]);
DeclareOperation("AsSemigroupHomomorphismByImages",
                 [IsSemigroupHomomorphismByImages]);

DeclareOperation("AsSemigroupIsomorphismByFunction",
                 [IsSemigroupHomomorphismByImages]);
DeclareOperation("AsSemigroupIsomorphismByFunction",
                 [IsSemigroupIsomorphismByFunction]);
