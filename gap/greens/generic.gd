#############################################################################
##
##  greens/generic.gd
##  Copyright (C) 2016-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for Green's relations and classes of semigroups
# where the particular representation of the Green's classes is not important.

DeclareAttribute("DClassType", IsSemigroup);
DeclareAttribute("HClassType", IsSemigroup);
DeclareAttribute("LClassType", IsSemigroup);
DeclareAttribute("RClassType", IsSemigroup);

DeclareProperty("IsGreensClassNC", IsGreensClass);
DeclareProperty("IsRegularGreensClass", IsGreensClass);

InstallTrueMethod(IsRegularGreensClass, IsRegularDClass);
InstallTrueMethod(IsRegularDClass, IsRegularGreensClass and IsGreensDClass);

DeclareAttribute("SchutzenbergerGroup", IsGreensClass);

DeclareOperation("GreensDClassOfElementNC",
                 [IsSemigroup, IsMultiplicativeElement]);
DeclareOperation("GreensJClassOfElementNC",
                 [IsSemigroup, IsMultiplicativeElement]);

DeclareAttribute("RegularDClasses", IsSemigroup);
DeclareAttribute("NrRegularDClasses", IsSemigroup);
DeclareAttribute("PartialOrderOfDClasses", IsSemigroup);
DeclareAttribute("PartialOrderOfLClasses", IsSemigroup);
DeclareAttribute("PartialOrderOfRClasses", IsSemigroup);

DeclareOperation("GreensLClassOfElement",
                 [IsGreensClass, IsMultiplicativeElement]);
DeclareOperation("GreensLClassOfElementNC",
                 [IsCollection, IsMultiplicativeElement]);

DeclareOperation("GreensRClassOfElement",
                 [IsGreensClass, IsMultiplicativeElement]);
DeclareOperation("GreensRClassOfElementNC",
                 [IsCollection, IsMultiplicativeElement]);

DeclareOperation("GreensHClassOfElement",
                 [IsGreensClass, IsMultiplicativeElement]);
DeclareOperation("GreensHClassOfElementNC",
                 [IsCollection, IsMultiplicativeElement]);

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
DeclareAttribute("NrIdempotentsByRank", IsCollection);

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

DeclareSynonymAttr("DClasses", GreensDClasses);
DeclareSynonymAttr("HClasses", GreensHClasses);
DeclareSynonymAttr("JClasses", GreensJClasses);
DeclareSynonymAttr("LClasses", GreensLClasses);
DeclareSynonymAttr("RClasses", GreensRClasses);

DeclareAttribute("OneImmutable", IsGreensHClass);

DeclareProperty("IsHTrivial", IsGreensClass);
DeclareProperty("IsLTrivial", IsGreensDClass);
DeclareProperty("IsRTrivial", IsGreensDClass);

DeclareOperation("LeftGreensMultiplier",
                 [IsGreensRClass, IsGreensRClass]);
DeclareOperation("RightGreensMultiplier",
                 [IsGreensLClass, IsGreensLClass]);
DeclareOperation("LeftGreensMultiplier",
                 [IsSemigroup,
                  IsMultiplicativeElement,
                  IsMultiplicativeElement]);
DeclareOperation("RightGreensMultiplier",
                 [IsSemigroup,
                  IsMultiplicativeElement,
                  IsMultiplicativeElement]);
DeclareOperation("LeftGreensMultiplierNC",
                 [IsSemigroup,
                  IsMultiplicativeElement,
                  IsMultiplicativeElement]);
DeclareOperation("RightGreensMultiplierNC",
                 [IsSemigroup,
                  IsMultiplicativeElement,
                  IsMultiplicativeElement]);
