############################################################################
##
#W  acting.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("SemigroupData", IsActingSemigroup, "mutable");
DeclareCategory("IsSemigroupData", IsList);
DeclareFilter("IsClosedData", IsSemigroupData);

DeclareOperation("Enumerate", [IsSemigroupData]);
DeclareOperation("Enumerate", [IsSemigroupData, IsCyclotomic]);
DeclareOperation("Enumerate", [IsSemigroupData, IsCyclotomic, IsFunction]);
DeclareOperation("OrbitGraphAsSets", [IsSemigroupData]);
DeclareOperation("OrbitGraph", [IsSemigroupData]);
DeclareOperation("PositionOfFound", [IsSemigroupData]);

# these must be here since UniversalFakeOne is used in lots of other places

DeclareCategory("IsUniversalFakeOne", IsAssociativeElement);

BindGlobal("UniversalFakeOneFamily",
  NewFamily("UniversalFakeOneFamily", IsUniversalFakeOne,
   CanEasilyCompareElements, CanEasilyCompareElements));

BindGlobal("UniversalFakeOne",
Objectify(NewType(UniversalFakeOneFamily, IsUniversalFakeOne), rec()));

#EOF
