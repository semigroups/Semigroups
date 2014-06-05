############################################################################
##
#W  non-exhaustive.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("NonExhaustiveData", IsNonExhaustiveSemigroup, "mutable");
DeclareCategory("IsNonExhaustiveData", IsList);
DeclareFilter("IsClosedData", IsNonExhaustiveData);
DeclareAttribute("SizeOfNonExhaustiveData", IsNonExhaustiveData);

DeclareOperation("Enumerate", [IsNonExhaustiveData]);
DeclareOperation("Enumerate", [IsNonExhaustiveData, IsCyclotomic]);
DeclareOperation("Enumerate", [IsNonExhaustiveData, IsCyclotomic, IsFunction]);
DeclareOperation("OrbitGraphAsSets", [IsNonExhaustiveData]);
DeclareOperation("OrbitGraph", [IsNonExhaustiveData]);
DeclareOperation("PositionOfFound", [IsNonExhaustiveData]);

DeclareCategory("IsUniversalFakeOne", IsAssociativeElement);
BindGlobal("UniversalFakeOneFamily", 
  NewFamily("UniversalFakeOneFamily", IsUniversalFakeOne,
   CanEasilyCompareElements, CanEasilyCompareElements));
                                                
BindGlobal("UniversalFakeOne", 
Objectify(NewType(UniversalFakeOneFamily, IsUniversalFakeOne), rec()));

#EOF
