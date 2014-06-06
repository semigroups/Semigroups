#############################################################################
##
#W  greens.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

# this file contains methods for Green's relations and classes of semigroups,
# which do not depend on the representation as IsNonExhaustiveSemigroup.

DeclareAttribute("MultiplicativeNeutralElement", IsGreensHClass);

DeclareOperation("GreensDClassOfElementNC", [IsSemigroup,
IsAssociativeElement]); 

DeclareOperation("GreensHClassOfElementNC", [IsSemigroup,
IsAssociativeElement]);
DeclareOperation("GreensHClassOfElementNC", [IsGreensClass,
IsAssociativeElement]);
DeclareOperation("GreensHClassOfElement", [IsGreensClass,
IsAssociativeElement]);

DeclareOperation("GreensLClassOfElement", [IsGreensDClass,
IsAssociativeElement]);
DeclareOperation("GreensLClassOfElementNC", [IsSemigroup,
IsAssociativeElement]); 
DeclareOperation("GreensLClassOfElementNC", [IsGreensDClass,
IsAssociativeElement]);

DeclareOperation("GreensRClassOfElement", [IsGreensDClass,
IsAssociativeElement]);
DeclareOperation("GreensRClassOfElementNC", [IsSemigroup,
IsAssociativeElement]); 
DeclareOperation("GreensRClassOfElementNC", [IsGreensDClass,
IsAssociativeElement]);

DeclareAttribute("DClassReps", IsSemigroup);
DeclareAttribute("HClassReps", IsSemigroup);
DeclareAttribute("HClassReps", IsGreensClass);
DeclareAttribute("LClassReps", IsSemigroup); 
DeclareAttribute("LClassReps", IsGreensDClass); 
DeclareAttribute("RClassReps", IsSemigroup);
DeclareAttribute("RClassReps", IsGreensDClass);

DeclareAttribute("Idempotents", IsGreensClass);
DeclareOperation("Idempotents", [IsSemigroup, IsInt]);
DeclareSynonymAttr("GroupHClass", GroupHClassOfGreensDClass);

DeclareAttribute("StructureDescription", IsGreensHClass);

DeclareProperty("IsRegularClass", IsGreensClass);
InstallTrueMethod(IsRegularClass, IsRegularDClass);
DeclareCategory("IsHClassOfRegularSemigroup", IsGreensClass);
DeclareCategory("IsInverseOpClass", IsGreensClass); 
InstallTrueMethod(IsRegularClass, IsInverseOpClass and IsGreensDClass);
InstallTrueMethod(IsRegularClass, IsInverseOpClass and IsGreensRClass);
InstallTrueMethod(IsRegularClass, IsInverseOpClass and IsGreensLClass);

DeclareAttribute("PartialOrderOfDClasses", IsSemigroup);
DeclareAttribute("SchutzenbergerGroup", IsGreensClass);

# abbreviations...

DeclareSynonym("DClass", GreensDClassOfElement);
DeclareSynonym("DClassNC", GreensDClassOfElementNC);
DeclareOperation("DClass", [IsGreensClass]);
DeclareOperation("DClassNC", [IsGreensClass]);

DeclareSynonym("HClass", GreensHClassOfElement);
DeclareSynonym("HClassNC", GreensHClassOfElementNC);

DeclareSynonym("LClass", GreensLClassOfElement);
DeclareSynonym("LClassNC", GreensLClassOfElementNC);
DeclareOperation("LClass", [IsGreensHClass]);

DeclareSynonym("RClass", GreensRClassOfElement);
DeclareSynonym("RClassNC", GreensRClassOfElementNC);
DeclareOperation("RClass", [IsGreensHClass]);

DeclareSynonymAttr("DClasses", GreensDClasses);
DeclareSynonymAttr("HClasses", GreensHClasses);
DeclareSynonymAttr("JClasses", GreensJClasses);
DeclareSynonymAttr("LClasses", GreensLClasses);
DeclareSynonymAttr("RClasses", GreensRClasses);

DeclareAttribute("NrDClasses", IsSemigroup); 
DeclareAttribute("NrHClasses", IsSemigroup);
DeclareAttribute("NrHClasses", IsGreensClass);
DeclareAttribute("NrLClasses", IsSemigroup);
DeclareAttribute("NrLClasses", IsGreensDClass);
DeclareAttribute("NrRClasses", IsSemigroup);
DeclareAttribute("NrRClasses", IsGreensDClass);

DeclareAttribute("NrRegularDClasses", IsSemigroup); 
DeclareAttribute("NrIdempotents", IsSemigroup);
DeclareAttribute("NrIdempotents", IsGreensClass);


DeclareAttribute("DClassType", IsSemigroup);
DeclareAttribute("HClassType", IsSemigroup);
DeclareAttribute("LClassType", IsSemigroup);
DeclareAttribute("RClassType", IsSemigroup);

#EOF
