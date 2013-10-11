#############################################################################
##
#W  greens.gd
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
##  This file contains algorithms for computing Green's relations
##  and related notions for transformation semigroups and monoids. 
##  The theory behind these algorithms is developed in 
##  
##  [LPRR1] S. A.   Linton, G.  Pfeiffer, E.  F.  Robertson, and N.   Ruskuc,
##  Groups  and actions in  transformation semigroups, to appear in Math Z.
##  (1998).
##
##  Early versions of the algorithms themselves are described in
##
##  [LPRR2] S. A.   Linton, G.  Pfeiffer, E.  F.  Robertson, and N.   Ruskuc,
##  Computing transformation semigroups, (1998), in preparation.
##  
##  Another reference is
##
##  [LM]  G.  Lallement and R. McFadden, On the   determination of Green's
##  relations in finite transformation semigroups, J. Symbolic Computation 10
##  (1990), 481--489.
##
#############################################################################
#############################################################################

#new

# LambdaRhoLookup(d)[i]=j if orbit[j][4] in reps[i] (orbit[j][4] is one of the
# R-reps of the D-class d) and LambdaRhoLookup(d) is only bound for those
# indices i where there is an R-rep in the scc of the D-class in reps[i] 
DeclareAttribute("LambdaRhoLookup", IsGreensDClass and
IsActingSemigroupGreensClass);

DeclareAttribute("MultiplicativeNeutralElement", IsGreensHClass);

DeclareProperty("IsPartialPermSemigroupGreensClass", IsGreensClass);
DeclareProperty("IsTransformationSemigroupGreensClass", IsGreensClass);
DeclareProperty("IsGreensClassNC", IsActingSemigroupGreensClass);

DeclareAttribute("LambdaOrbSCC", IsActingSemigroupGreensClass);
DeclareAttribute("LambdaOrbSCCIndex", IsActingSemigroupGreensClass);

DeclareAttribute("RhoOrbSCC", IsActingSemigroupGreensClass);
DeclareAttribute("RhoOrbSCCIndex", IsActingSemigroupGreensClass);

DeclareAttribute("LambdaCosets", IsActingSemigroupGreensClass);
DeclareAttribute("RhoCosets", IsActingSemigroupGreensClass);

DeclareAttribute("SemigroupDataSCC", IsActingSemigroupGreensClass);
DeclareAttribute("SemigroupDataSCCIndex", IsActingSemigroupGreensClass);
DeclareAttribute("SemigroupDataIndex", IsActingSemigroupGreensClass);

#old

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

# other...
DeclareGlobalFunction("CreateDClass");
DeclareGlobalFunction("CreateDClassNC");
DeclareGlobalFunction("CreateHClass");
DeclareGlobalFunction("CreateHClassNC");
DeclareGlobalFunction("CreateLClass");
DeclareGlobalFunction("CreateLClassNC");
DeclareGlobalFunction("CreateInverseOpLClassNC");
DeclareGlobalFunction("CreateInverseOpLClass");
DeclareGlobalFunction("CreateRClass");
DeclareGlobalFunction("CreateRClassNC");

DeclareAttribute("DClassType", IsSemigroup);
DeclareAttribute("HClassType", IsSemigroup);
DeclareAttribute("LClassType", IsSemigroup);
DeclareAttribute("RClassType", IsSemigroup);

#technical...
DeclareGlobalFunction("IsRegularClass@");
DeclareGlobalFunction("Idempotents@");
DeclareGlobalFunction("NrIdempotents@");

#EOF
