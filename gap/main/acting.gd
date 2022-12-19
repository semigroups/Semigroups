############################################################################
##
##  main/acting.gd
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsSemigroupData", IsList and IsComponentObjectRep);
DeclareFilter("IsClosedData", IsSemigroupData);

DeclareAttribute("SemigroupData", IsActingSemigroup, "mutable");
DeclareOperation("SemigroupData", [IsActingSemigroup, IsLambdaOrb]);

DeclareOperation("Enumerate", [IsSemigroupData]);
DeclareOperation("Enumerate", [IsSemigroupData, IsCyclotomic]);
DeclareOperation("Enumerate", [IsSemigroupData, IsCyclotomic, IsFunction]);
DeclareOperation("OrbitGraphAsSets", [IsSemigroupData]);
DeclareOperation("OrbitGraph", [IsSemigroupData]);
DeclareOperation("PositionOfFound", [IsSemigroupData]);

# these must be here since SEMIGROUPS.UniversalFakeOne is used in lots of other
# places

DeclareCategory("SEMIGROUPS_IsUniversalFakeOne", IsAssociativeElement);

SEMIGROUPS.UniversalFakeOne :=
           Objectify(NewType(NewFamily("SEMIGROUPS.UniversalFakeOneFamily",
                                       SEMIGROUPS_IsUniversalFakeOne,
                                       CanEasilyCompareElements,
                                       CanEasilyCompareElements),
                             SEMIGROUPS_IsUniversalFakeOne
                             and IsComponentObjectRep),
                     rec());
