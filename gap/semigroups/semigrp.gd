#############################################################################
##
#W  semigrp.gd
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

InstallTrueMethod(IsStarSemigroup,
                  IsAssociativeElementWithStarCollection and
                  IsSemigroupWithInverseOp);

DeclareOperation("ClosureInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsMultiplicativeElementCollection]);
DeclareOperation("ClosureInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsMultiplicativeElementCollection,
                  IsRecord]);
DeclareOperation("ClosureInverseSemigroup",
                 [IsSemigroupWithInverseOp, IsListOrCollection and IsEmpty,
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
DeclareOperation("AsSemigroup", [IsFunction, IsRing, IsSemigroup]);
DeclareOperation("AsSemigroup", [IsFunction, IsPosInt, IsSemigroup]);
DeclareOperation("AsSemigroup", [IsFunction, IsPosInt, IsPosInt, IsSemigroup]);
DeclareConstructor("IsomorphismSemigroup",
                   [IsSemigroup, IsSemigroup]);
DeclareConstructor("IsomorphismSemigroup",
                   [IsSemigroup, IsRing, IsSemigroup]);
DeclareConstructor("IsomorphismSemigroup",
                   [IsSemigroup, IsPosInt, IsSemigroup]);
DeclareConstructor("IsomorphismSemigroup",
                   [IsSemigroup, IsPosInt, IsPosInt, IsSemigroup]);

DeclareOperation("AsMonoid", [IsFunction, IsSemigroup]);
DeclareOperation("AsMonoid", [IsFunction, IsRing, IsSemigroup]);
DeclareOperation("AsMonoid", [IsFunction, IsPosInt, IsSemigroup]);
DeclareOperation("AsMonoid", [IsFunction, IsPosInt, IsPosInt, IsSemigroup]);
DeclareConstructor("IsomorphismMonoid",
                   [IsSemigroup, IsSemigroup]);
DeclareConstructor("IsomorphismMonoid",
                   [IsSemigroup, IsRing, IsSemigroup]);
DeclareConstructor("IsomorphismMonoid",
                   [IsSemigroup, IsPosInt, IsSemigroup]);
DeclareConstructor("IsomorphismMonoid",
                   [IsSemigroup, IsPosInt, IsPosInt, IsSemigroup]);

#TODO move this to attributes!
DeclareAttribute("Generators", IsSemigroup);

#TODO move this to examples!
DeclareGlobalFunction("RandomSemigroup");
DeclareConstructor("RandomSemigroupCons", [IsSemigroup, IsList]);
DeclareGlobalFunction("RandomMonoid");
DeclareConstructor("RandomMonoidCons", [IsMonoid, IsList]);
DeclareGlobalFunction("RandomInverseSemigroup");
DeclareConstructor("RandomInverseSemigroupCons", [IsSemigroup, IsList]);
DeclareGlobalFunction("RandomInverseMonoid");
DeclareConstructor("RandomInverseMonoidCons", [IsMonoid, IsList]);

DeclareConstructor("SEMIGROUPS_ProcessRandomArgsCons", 
                   [IsSemigroup, IsList]);

DeclareOperation("SubsemigroupByProperty", [IsSemigroup, IsFunction]);
DeclareOperation("SubsemigroupByProperty",
                 [IsSemigroup, IsFunction, IsPosInt]);

DeclareOperation("InverseSubsemigroupByProperty",
                 [IsSemigroup, IsFunction]);
DeclareOperation("InverseSubsemigroupByProperty",
                 [IsSemigroupWithInverseOp, IsFunction, IsPosInt]);

# undocumented
DeclareGlobalFunction("RegularSemigroup");
