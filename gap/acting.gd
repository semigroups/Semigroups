############################################################################
##
#W  acting.gd
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("SemigroupData", IsActingSemigroup, "mutable");
DeclareGlobalFunction("SizeOfSemigroupData");
DeclareCategory("IsSemigroupData", IsList);

DeclareOperation("Enumerate", [IsSemigroupData]);
DeclareOperation("Enumerate", [IsSemigroupData, IsCyclotomic]);
DeclareOperation("Enumerate", [IsSemigroupData, IsCyclotomic, IsFunction]);
DeclareOperation("OrbitGraphAsSets", [IsSemigroupData]);
DeclareOperation("PositionOfFound", [IsSemigroupData]);

DeclareCategory("IsUniversalFakeOne", IsAssociativeElement);
BindGlobal("UniversalFakeOneFamily", 
  NewFamily("UniversalFakeOneFamily", IsUniversalFakeOne,
   CanEasilyCompareElements, CanEasilyCompareElements));
                                                
BindGlobal("UniversalFakeOne", 
Objectify(NewType(UniversalFakeOneFamily, IsUniversalFakeOne), rec()));

#EOF
