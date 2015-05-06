#############################################################################
##
#W  greens-generic.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

## This file contains methods for Green's classes/relations for generic
## semigroups.

DeclareAttribute("DClassType", IsSemigroup);
DeclareAttribute("HClassType", IsSemigroup);
DeclareAttribute("LClassType", IsSemigroup);
DeclareAttribute("RClassType", IsSemigroup);

DeclareProperty("IsGreensClassNC", IsGreensClass);
DeclareProperty("IsRegularClass", IsGreensClass);
DeclareCategory("IsHClassOfRegularSemigroup", IsGreensClass);

DeclareAttribute("SchutzenbergerGroup", IsGreensClass);

DeclareOperation("GreensDClassOfElementNC", 
                 [IsSemigroup, IsAssociativeElement]);
DeclareOperation("GreensJClassOfElementNC", 
                 [IsSemigroup, IsAssociativeElement]); 

DeclareAttribute("RegularDClasses", IsSemigroup);
DeclareAttribute("NrRegularDClasses", IsSemigroup);
DeclareAttribute("PartialOrderOfDClasses", IsSemigroup);

DeclareOperation("GreensLClassOfElement",
                 [IsGreensClass, IsAssociativeElement]);
DeclareOperation("GreensLClassOfElementNC",
                 [IsCollection, IsAssociativeElement]);

DeclareOperation("GreensRClassOfElement",
                 [IsGreensClass, IsAssociativeElement]);
DeclareOperation("GreensRClassOfElementNC",
                 [IsCollection, IsAssociativeElement]);
DeclareOperation("EnumeratorOfRClasses", [IsSemigroup]);

DeclareOperation("GreensHClassOfElement",
                 [IsGreensClass, IsAssociativeElement]);
DeclareOperation("GreensHClassOfElementNC",
                 [IsCollection, IsAssociativeElement]);

DeclareSynonymAttr("GroupHClass", GroupHClassOfGreensDClass);
DeclareAttribute("StructureDescription", IsGreensHClass);
DeclareAttribute("MultiplicativeNeutralElement", IsGreensHClass);

DeclareAttribute("DClassReps", IsSemigroup);
DeclareAttribute("HClassReps", IsCollection);
DeclareAttribute("LClassReps", IsCollection);
DeclareAttribute("RClassReps", IsCollection);

DeclareAttribute("NrDClasses", IsSemigroup);
DeclareAttribute("NrHClasses", IsCollection);
DeclareAttribute("NrLClasses", IsCollection);
DeclareAttribute("NrRClasses", IsCollection);

DeclareAttribute("Idempotents", IsGreensClass);
DeclareOperation("Idempotents", [IsSemigroup, IsInt]);
DeclareAttribute("NrIdempotents", IsCollection);

DeclareSynonym("DClass", GreensDClassOfElement);
DeclareSynonym("LClass", GreensLClassOfElement);
DeclareSynonym("RClass", GreensRClassOfElement);
DeclareSynonym("HClass", GreensHClassOfElement);

DeclareSynonym("DClassNC", GreensDClassOfElementNC);
DeclareSynonym("LClassNC", GreensLClassOfElementNC);
DeclareSynonym("RClassNC", GreensRClassOfElementNC);
DeclareSynonym("HClassNC", GreensHClassOfElementNC);

DeclareOperation("DClass", [IsGreensClass]);
DeclareOperation("LClass", [IsGreensHClass]);
DeclareOperation("RClass", [IsGreensHClass]);

InstallMethod(DClass, "for an R-class", [IsGreensRClass], DClassOfRClass);
InstallMethod(DClass, "for an L-class", [IsGreensLClass], DClassOfLClass);
InstallMethod(DClass, "for an H-class", [IsGreensHClass], DClassOfHClass);
InstallMethod(LClass, "for an H-class", [IsGreensHClass], LClassOfHClass);
InstallMethod(RClass, "for an H-class", [IsGreensHClass], RClassOfHClass);

DeclareOperation("DClassNC", [IsGreensClass]);

DeclareSynonymAttr("DClasses", GreensDClasses);
DeclareSynonymAttr("HClasses", GreensHClasses);
DeclareSynonymAttr("JClasses", GreensJClasses);
DeclareSynonymAttr("LClasses", GreensLClasses);
DeclareSynonymAttr("RClasses", GreensRClasses);

