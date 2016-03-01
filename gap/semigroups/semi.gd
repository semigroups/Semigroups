#############################################################################
##
#W  semi.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains operations for creating semigroups, subsemigroups, and
# closures.

# The following true method is required since IsInverseSemigroup is a property
# of semigroups and so objectifying something using IsInverseSemigroup does not
# result in a semigroup.

InstallTrueMethod(IsSemigroup, IsInverseSemigroup);

DeclareCategory("IsSemigroupWithInverseOp",
                IsInverseSemigroup);
DeclareOperation("SemigroupByGenerators",
                 [IsMultiplicativeElementCollection, IsRecord]);
DeclareOperation("MonoidByGenerators",
                 [IsMultiplicativeElementCollection, IsRecord]);
DeclareOperation("InverseMonoidByGenerators",
                 [IsMultiplicativeElementWithOneCollection, IsRecord]);
DeclareOperation("InverseSemigroupByGenerators",
                 [IsMultiplicativeElementCollection, IsRecord]);

DeclareOperation("ClosureInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsMultiplicativeElementCollection]);
DeclareOperation("ClosureInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsMultiplicativeElementCollection,
                  IsRecord]);
DeclareOperation("ClosureInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsMultiplicativeElement]);
DeclareOperation("ClosureInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsMultiplicativeElement, IsRecord]);
DeclareGlobalFunction("ClosureInverseSemigroupNC");

DeclareOperation("ClosureSemigroup",
                 [IsSemigroup, IsMultiplicativeElementCollection, IsRecord]);
DeclareOperation("ClosureSemigroup",
                 [IsSemigroup, IsMultiplicativeElementCollection]);
DeclareOperation("ClosureSemigroup",
                 [IsSemigroup, IsMultiplicativeElement, IsRecord]);
DeclareOperation("ClosureSemigroup",
                 [IsSemigroup, IsMultiplicativeElement]);
DeclareOperation("ClosureSemigroupNC",
                 [IsSemigroup, IsListOrCollection, IsRecord]);

DeclareOperation("AsSemigroup", [IsFunction, IsSemigroup]);
DeclareConstructor("IsomorphismSemigroup", [IsSemigroup, IsSemigroup]);

DeclareOperation("AsMonoid", [IsFunction, IsSemigroup]);
DeclareConstructor("IsomorphismMonoid", [IsSemigroup, IsSemigroup]);

#TODO move this to attributes!
DeclareAttribute("Generators", IsSemigroup);

#TODO move this to examples!
DeclareGlobalFunction("RandomSemigroup");
DeclareConstructor("RandomSemigroupCons",
                   [IsSemigroup, IsPosInt, IsInt, IsInt, IsInt]);
DeclareGlobalFunction("RandomMonoid");
DeclareConstructor("RandomMonoidCons",
                   [IsSemigroup, IsPosInt, IsInt, IsInt, IsInt]);
DeclareGlobalFunction("RandomInverseSemigroup");
DeclareConstructor("RandomInverseSemigroupCons",
                   [IsSemigroup, IsPosInt, IsInt, IsInt, IsInt]);
DeclareGlobalFunction("RandomInverseMonoid");
DeclareConstructor("RandomInverseMonoidCons",
                   [IsSemigroup, IsPosInt, IsInt, IsInt, IsInt]);

DeclareOperation("SubsemigroupByProperty", [IsSemigroup, IsFunction]);
DeclareOperation("SubsemigroupByProperty",
                 [IsSemigroup, IsFunction, IsPosInt]);

DeclareOperation("InverseSubsemigroupByProperty",
                 [IsSemigroup, IsFunction]);
DeclareOperation("InverseSubsemigroupByProperty",
                 [IsSemigroupWithInverseOp, IsFunction, IsPosInt]);

# undocumented
DeclareGlobalFunction("RegularSemigroup");
