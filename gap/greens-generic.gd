#############################################################################
##
#W  greens-generic.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##

## This file contains methods for Green's classes/relations for generic
## semigroups.

# different types of class

DeclareProperty("IsGreensClassNC", IsGreensClass);
DeclareProperty("IsRegularClass", IsGreensClass);

DeclareAttribute("DClassType", IsSemigroup);
DeclareAttribute("HClassType", IsSemigroup);
DeclareAttribute("LClassType", IsSemigroup);
DeclareAttribute("RClassType", IsSemigroup);

# Schutzenberger group

DeclareAttribute("SchutzenbergerGroup", IsGreensClass);

# D-classes

DeclareOperation("GreensDClassOfElementNC",
                 [IsSemigroup, IsAssociativeElement]);
DeclareAttribute("RegularDClasses", IsSemigroup);
DeclareAttribute("NrRegularDClasses", IsSemigroup);
DeclareAttribute("PartialOrderOfDClasses", IsSemigroup);

# H-classes

DeclareOperation("GreensHClassOfElement",
                 [IsGreensClass, IsAssociativeElement]);
DeclareOperation("GreensHClassOfElementNC",
                 [IsCollection, IsAssociativeElement]);
DeclareSynonymAttr("GroupHClass", GroupHClassOfGreensDClass);
DeclareCategory("IsHClassOfRegularSemigroup", IsGreensClass);
DeclareAttribute("StructureDescription", IsGreensHClass);
DeclareAttribute("MultiplicativeNeutralElement", IsGreensHClass);

# L-classes

DeclareOperation("GreensLClassOfElement",
                 [IsGreensClass, IsAssociativeElement]);
DeclareOperation("GreensLClassOfElementNC",
                 [IsCollection, IsAssociativeElement]);

# R-classes

DeclareOperation("GreensRClassOfElement",
                 [IsGreensClass, IsAssociativeElement]);
DeclareOperation("GreensRClassOfElementNC",
                 [IsCollection, IsAssociativeElement]);
DeclareOperation("EnumeratorOfRClasses", [IsSemigroup]);

# representatives

DeclareAttribute("DClassReps", IsSemigroup);
DeclareAttribute("HClassReps", IsCollection);
DeclareAttribute("LClassReps", IsCollection);
DeclareAttribute("RClassReps", IsCollection);

# number of classes

DeclareAttribute("NrDClasses", IsSemigroup);
DeclareAttribute("NrHClasses", IsCollection);
DeclareAttribute("NrLClasses", IsCollection);
DeclareAttribute("NrRClasses", IsCollection);

# idempotents

DeclareAttribute("Idempotents", IsGreensClass);
DeclareOperation("Idempotents", [IsSemigroup, IsInt]);
DeclareAttribute("NrIdempotents", IsCollection);

# abbreviations and synonyms

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
