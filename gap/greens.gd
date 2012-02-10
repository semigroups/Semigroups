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

# in greens.xml

DeclareAttribute("DClassReps", IsTransformationSemigroup);
DeclareOperation("EnumeratorOfRClasses", [IsTransformationSemigroup]);
DeclareOperation("GreensDClassOfElementNC", [IsTransformationSemigroup]); 
DeclareOperation("GreensRClassOfElementNC", [IsTransformationSemigroup]);
DeclareAttribute("HClassReps", IsTransformationSemigroup);
DeclareProperty("IsGreensClassOfTransSemigp", IsGreensClass);
DeclareProperty("IsRegularRClass", IsGreensClassOfTransSemigp);
DeclareGlobalFunction("IteratorOfDClasses");
DeclareGlobalFunction("IteratorOfDClassReps"); 
DeclareGlobalFunction("IteratorOfRClasses");
DeclareGlobalFunction("IteratorOfRClassReps");
DeclareAttribute("NrIdempotents", IsTransformationSemigroup);
DeclareAttribute("NrDClasses", IsTransformationSemigroup); 
DeclareAttribute("NrHClasses", IsTransformationSemigroup);
DeclareAttribute("NrRClasses", IsTransformationSemigroup);
DeclareAttribute("RClassReps", IsTransformationSemigroup);
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
DeclareGlobalFunction("CreateRClass");
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
DeclareGlobalFunction("IteratorOfRClassRepsData");
DeclareGlobalFunction("HClassRepsDataFromData");
DeclareGlobalFunction("NrIdempotentsRClassFromData");
DeclareAttribute("OrbitsOfImages", IsTransformationSemigroup, "mutable");
DeclareGlobalFunction("PreInOrbitsOfImages");
DeclareGlobalFunction("RClassIndexFromData");
DeclareGlobalFunction("RClassRepFromData");
DeclareAttribute("RClassRepsData", IsTransformationSemigroup);
DeclareAttribute("RClassType", IsTransformationSemigroup);
DeclareGlobalFunction("TraceRClassRepsTree");

# the documentation for the functions below can be found in 
# /monoid/doc/d.xml

DeclareAttribute("PartialOrderOfDClasses", IsTransformationSemigroup);
DeclareAttribute("NrRegularDClasses", IsTransformationSemigroup); 

# the following functions in d.gi are currently undocumented

DeclareGlobalFunction("AddToOrbitsOfKernels");
DeclareGlobalFunction("CreateDClass");
DeclareGlobalFunction("CreateKernelOrbitSchutzGp");
DeclareGlobalFunction("CreateKernelOrbitSCCRels");
DeclareGlobalFunction("CreateSchutzGpOfDClass");
DeclareGlobalFunction("DClassRClassRepsDataFromData");
DeclareGlobalFunction("DClassRepFromData");
DeclareAttribute("DClassRepsData", IsTransformationSemigroup);
DeclareGlobalFunction("DClassSchutzGpFromData");
DeclareAttribute("DClassType", IsTransformationSemigroup);
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
DeclareGlobalFunction("IteratorOfDClassRepsData");
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

DeclareGlobalFunction("CreateLClass");
DeclareOperation("GreensLClassOfElementNC", [IsTransformationSemigroup]); 
DeclareGlobalFunction("LClassRepFromData");
DeclareAttribute("LClassReps", IsTransformationSemigroup); 
DeclareAttribute("LClassType", IsTransformationSemigroup);
DeclareProperty("IsRegularLClass", IsGreensClassOfTransSemigp); 
DeclareGlobalFunction("IteratorOfLClasses"); 
DeclareGlobalFunction("IteratorOfLClassRepsData");
DeclareGlobalFunction("IteratorOfLClassReps"); 
DeclareAttribute("NrLClasses", IsTransformationSemigroup);

# the following functions in h.gi are currently undocumented

DeclareGlobalFunction("CreateHClass");
DeclareOperation("GreensHClassOfElementNC", [IsTransformationSemigroup]); 
DeclareGlobalFunction("HClassRepFromData");
DeclareAttribute("HClassType", IsTransformationSemigroup);
DeclareGlobalFunction("IteratorOfHClasses");
DeclareGlobalFunction("IteratorOfHClassReps"); 
DeclareGlobalFunction("IteratorOfHClassRepsData");

DeclareProperty("IsIteratorOfRClassReps", IsIterator);
DeclareProperty("IsIteratorOfLClassReps", IsIterator);
DeclareProperty("IsIteratorOfDClassReps", IsIterator);
DeclareProperty("IsIteratorOfHClassReps", IsIterator);

# IteratorOfRClasses and IteratorOfLClasses
# should be operations so that they can be applied to 
# D-classes as well as transformation semigroups!
# and IteratorOfDClass should be an operation so that I can make an
# iterator that of D-classes satisfying some properties a la smallsemi..

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
