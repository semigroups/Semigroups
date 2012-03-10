#############################################################################
##
#W  greens.gd
#Y  Copyright (C) 2011-12                                James D. Mitchell
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

DeclareGlobalFunction("CreateDClass");
DeclareGlobalFunction("CreateHClass");
DeclareGlobalFunction("CreateLClass");
DeclareGlobalFunction("CreateRClass");

DeclareAttribute("DClassType", IsSemigroup);
DeclareAttribute("HClassType", IsSemigroup);
DeclareAttribute("LClassType", IsSemigroup);
DeclareAttribute("RClassType", IsSemigroup);

DeclareAttribute("DClassReps", IsSemigroup);
DeclareAttribute("HClassReps", IsSemigroup);
DeclareAttribute("LClassReps", IsSemigroup); 
DeclareAttribute("RClassReps", IsSemigroup);

DeclareOperation("EnumeratorOfRClasses", [IsSemigroup]);

DeclareOperation("GreensDClassOfElementNC", [IsSemigroup]); 
DeclareOperation("GreensHClassOfElementNC", [IsSemigroup]); 
DeclareOperation("GreensLClassOfElementNC", [IsSemigroup]); 
DeclareOperation("GreensRClassOfElementNC", [IsSemigroup]);

DeclareProperty("IsGreensClassOfTransSemigp", IsGreensClass);

DeclareProperty("IsRegularLClass", IsGreensClass); 
DeclareProperty("IsRegularRClass", IsGreensClass);

DeclareOperation("IteratorOfDClasses", [IsSemigroup]);
DeclareOperation("IteratorOfHClasses", [IsSemigroup]);
DeclareOperation("IteratorOfLClasses", [IsSemigroup]); 
DeclareOperation("IteratorOfRClasses", [IsSemigroup]);

DeclareOperation("IteratorOfDClassReps", [IsSemigroup]); 
DeclareOperation("IteratorOfLClassReps", [IsSemigroup]); 
DeclareOperation("IteratorOfHClassReps", [IsSemigroup]); 
DeclareOperation("IteratorOfRClassReps", [IsSemigroup]);

DeclareOperation("IteratorOfDClassRepsData", [IsSemigroup]);
DeclareOperation("IteratorOfHClassRepsData", [IsSemigroup]);
DeclareOperation("IteratorOfLClassRepsData", [IsSemigroup]);
DeclareOperation("IteratorOfRClassRepsData", [IsSemigroup]);

DeclareAttribute("NrIdempotents", IsSemigroup);

DeclareAttribute("NrDClasses", IsSemigroup); 
DeclareAttribute("NrHClasses", IsSemigroup);
DeclareAttribute("NrLClasses", IsSemigroup);
DeclareAttribute("NrRClasses", IsSemigroup);
DeclareAttribute("NrRegularDClasses", IsSemigroup); 

DeclareAttribute("PartialOrderOfDClasses", IsSemigroup);

DeclareAttribute("SchutzenbergerGroup", IsGreensClass);

DeclareSynonymAttr("DClasses", GreensDClasses);
DeclareSynonymAttr("HClasses", GreensHClasses);
DeclareSynonymAttr("JClasses", GreensJClasses);
DeclareSynonymAttr("LClasses", GreensLClasses);
DeclareSynonymAttr("RClasses", GreensRClasses);

# the following functions in r.gi are currently undocumented

DeclareGlobalFunction("AddToOrbitsOfImages");
DeclareGlobalFunction("CreateImageOrbitSCCPerms");
DeclareGlobalFunction("CreateImageOrbitSchutzGp");
DeclareGlobalFunction("DisplayOrbitsOfImages");
DeclareGlobalFunction("ExpandOrbitsOfImages");
DeclareGlobalFunction("ForwardOrbitOfImage");
DeclareAttribute("HClassRepsData", IsTransformationSemigroup);
DeclareAttribute("ImageOrbit", IsGreensRClass and 
 IsGreensClassOfTransSemigp, "mutable"); # mutable essential!
DeclareGlobalFunction("ImageOrbitFromData");
DeclareAttribute("ImageOrbitKersHT", IsGreensRClass and 
 IsGreensClassOfTransSemigp);
DeclareGlobalFunction("ImageOrbitKersHTFromData");
DeclareAttribute("ImageOrbitPerms", IsGreensRClass and 
 IsGreensClassOfTransSemigp);
DeclareGlobalFunction("ImageOrbitPermsFromData"); 
DeclareGlobalFunction("ImageOrbitSCCFromData");
DeclareAttribute("ImageOrbitSCC", IsGreensRClass and 
 IsGreensClassOfTransSemigp);
DeclareAttribute("ImageOrbitSchutzGp", IsGreensRClass and
IsGreensClassOfTransSemigp);
DeclareGlobalFunction("ImageOrbitSchutzGpFromData");
DeclareGlobalFunction("ImageOrbitStabChainFromData");
DeclareAttribute("ImageOrbitStabChain", IsGreensRClass and 
 IsGreensClassOfTransSemigp);
DeclareGlobalFunction("InOrbitsOfImages");
DeclareProperty("IsIteratorOfRClassRepsData", IsIterator);
DeclareGlobalFunction("IsRegularRClassData");
DeclareProperty("IsRClassNC", IsGreensClassOfTransSemigp);
DeclareGlobalFunction("IteratorOfNewRClassRepsData");

DeclareGlobalFunction("HClassRepsDataFromData");
DeclareGlobalFunction("NrIdempotentsRClassFromData");
DeclareAttribute("OrbitsOfImages", IsTransformationSemigroup, "mutable");
DeclareGlobalFunction("PreInOrbitsOfImages");
DeclareGlobalFunction("RClassIndexFromData");
DeclareGlobalFunction("HClassRepFromData");
DeclareGlobalFunction("RClassRepFromData");
DeclareAttribute("RClassRepsData", IsTransformationSemigroup);
DeclareGlobalFunction("TraceRClassRepsTree");

# the documentation for the functions below can be found in 
# /monoid/doc/d.xml


# the following functions in d.gi are currently undocumented

DeclareGlobalFunction("AddToOrbitsOfKernels");
DeclareGlobalFunction("CreateKernelOrbitSchutzGp");
DeclareGlobalFunction("CreateKernelOrbitSCCRels");
DeclareGlobalFunction("CreateSchutzGpOfDClass");
DeclareGlobalFunction("DClassRClassRepsDataFromData");
DeclareGlobalFunction("DClassRepFromData");
DeclareAttribute("DClassRepsData", IsTransformationSemigroup);
DeclareGlobalFunction("DClassSchutzGpFromData");
DeclareGlobalFunction("DisplayOrbitsOfKernels");
DeclareGlobalFunction("ExpandOrbitsOfKernels");
DeclareGlobalFunction("ForwardOrbitOfKernel");
DeclareAttribute("GeneratorsAsListOfImages", IsTransformationSemigroup);
DeclareAttribute("GroupHClass", IsGreensDClass and
 IsGreensClassOfTransSemigp);
DeclareAttribute("LClassRepsData", IsTransformationSemigroup);
DeclareGlobalFunction("LClassRepsDataFromData");
DeclareGlobalFunction("ImageOrbitCosetsFromData"); #input kernel orbit data!
DeclareAttribute("ImageOrbitCosets", IsGreensDClass and
 IsGreensClassOfTransSemigp);
DeclareGlobalFunction("InOrbitsOfKernels");
DeclareProperty("IsIteratorOfDClassRepsData", IsIterator);
DeclareGlobalFunction("IteratorOfNewDClassReps");
DeclareAttribute("KernelOrbit", IsGreensDClass and
 IsGreensClassOfTransSemigp, "mutable");
DeclareGlobalFunction("KernelOrbitFromData"); 
DeclareAttribute("KernelOrbitCosets", IsGreensDClass and
 IsGreensClassOfTransSemigp);
DeclareGlobalFunction("KernelOrbitCosetsFromData");
DeclareAttribute("KernelOrbitRels", IsGreensDClass and
 IsGreensClassOfTransSemigp);
DeclareGlobalFunction("KernelOrbitRelsFromData");
DeclareAttribute("KernelOrbitSCC", IsGreensDClass and
 IsGreensClassOfTransSemigp);
DeclareGlobalFunction("KernelOrbitSCCFromData");
DeclareAttribute("KernelOrbitSchutzGp", IsGreensDClass and
 IsGreensClassOfTransSemigp);
DeclareGlobalFunction("KernelOrbitSchutzGpFromData");
DeclareAttribute("KernelOrbitStabChain", IsGreensDClass and
 IsGreensClassOfTransSemigp);
DeclareGlobalFunction("KernelOrbitStabChainFromData");
DeclareAttribute("KerRightToImgLeft", IsGreensDClass and 
IsGreensClassOfTransSemigp);
DeclareGlobalFunction("KerRightToImgLeftFromData");
DeclareAttribute("OrbitsOfKernels", IsTransformationSemigroup, "mutable");
DeclareGlobalFunction("PreInOrbitsOfKernels");
DeclareGlobalFunction("SizeDClassFromData");
DeclareGlobalFunction("SizeOrbitsOfKernels");

# the following functions in l.gi are currently undocumented

DeclareGlobalFunction("LClassRepFromData");

# the following functions in h.gi are currently undocumented

DeclareProperty("IsIteratorOfRClassReps", IsIterator);
DeclareProperty("IsIteratorOfLClassReps", IsIterator);
DeclareProperty("IsIteratorOfDClassReps", IsIterator);
DeclareProperty("IsIteratorOfHClassReps", IsIterator);

DeclareProperty("IsIteratorOfRClasses", IsIterator);
DeclareProperty("IsIteratorOfLClasses", IsIterator);
DeclareProperty("IsIteratorOfDClasses", IsIterator);
DeclareProperty("IsIteratorOfHClasses", IsIterator);

DeclareProperty("IsIteratorOfSemigroup", IsIterator);
DeclareProperty("IsIteratorOfRClassElements", IsIterator);
DeclareProperty("IsIteratorOfLClassElements", IsIterator);
DeclareProperty("IsIteratorOfDClassElements", IsIterator);
DeclareProperty("IsIteratorOfHClassElements", IsIterator);

DeclareGlobalFunction("UnderlyingSemigroupOfIterator");
DeclareProperty("IsCitrusPkgIterator", IsIterator);

DeclareProperty("IsOrbitsOfImages", IsObject);
DeclareProperty("IsOrbitsOfKernels", IsObject);
DeclareProperty("IsCitrusPkgImgKerOrbit", IsOrbit);
