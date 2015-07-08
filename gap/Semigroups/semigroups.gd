#############################################################################
##
#W  semigroups.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains operations for creating semigroups, subsemigroups, and
# closures. 

# These two operations are used to produce the view strings for a semigroup. 
# TODO example

DeclareOperation("SEMIGROUPS_ViewStringPrefix", [IsSemigroup]);
DeclareOperation("SEMIGROUPS_ViewStringSuffix", [IsSemigroup]);

# The following true method is required since IsInverseSemigroup is a property
# of semigroups and so objectifying something using IsInverseSemigroup does not
# result in a semigroup.

InstallTrueMethod(IsSemigroup, IsInverseSemigroup);

DeclareCategory("IsSemigroupWithInverseOp",
                IsInverseSemigroup);
DeclareOperation("SemigroupByGenerators",
                 [IsAssociativeElementCollection, IsRecord]);
DeclareOperation("MonoidByGenerators",
                 [IsAssociativeElementCollection, IsRecord]);
DeclareOperation("InverseMonoidByGenerators",
                 [IsAssociativeElementCollection and
                  IsMultiplicativeElementWithOneCollection, IsRecord]);
DeclareOperation("InverseSemigroupByGenerators",
                 [IsAssociativeElementCollection, IsRecord]);

DeclareOperation("ClosureInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsAssociativeElementCollection]);
DeclareOperation("ClosureInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsAssociativeElementCollection,
                  IsRecord]);
DeclareOperation("ClosureInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsAssociativeElement]);
DeclareOperation("ClosureInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsAssociativeElement, IsRecord]);
DeclareGlobalFunction("ClosureInverseSemigroupNC");

DeclareOperation("ClosureSemigroup",
                 [IsSemigroup, IsAssociativeElementCollection, IsRecord]);
DeclareOperation("ClosureSemigroup",
                 [IsSemigroup, IsAssociativeElementCollection]);
DeclareOperation("ClosureSemigroup",
                 [IsSemigroup, IsAssociativeElement, IsRecord]);
DeclareOperation("ClosureSemigroup",
                 [IsSemigroup, IsAssociativeElement]);
DeclareOperation("ClosureSemigroupNC", 
                 [IsSemigroup, IsListOrCollection, IsRecord]);
DeclareGlobalFunction("SEMIGROUPS_AddGenerators");

#TODO move this to attributes!
DeclareAttribute("Generators", IsSemigroup);

#TODO move this to examples!
DeclareGlobalFunction("RandomSemigroup");
DeclareGlobalFunction("RandomMonoid");

DeclareOperation("SubsemigroupByProperty", [IsSemigroup, IsFunction]);
DeclareOperation("SubsemigroupByProperty",
                 [IsSemigroup, IsFunction, IsPosInt]);

DeclareOperation("InverseSubsemigroupByProperty",
                 [IsSemigroup, IsFunction]);
DeclareOperation("InverseSubsemigroupByProperty",
                 [IsSemigroupWithInverseOp, IsFunction, IsPosInt]);

# undocumented
DeclareGlobalFunction("RegularSemigroup");
