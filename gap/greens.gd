#############################################################################
##
#W  greens.gd
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
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


# in greens.gi

DeclareOperation("EnumeratorOfRClasses", [IsTransformationSemigroup]);

# the documentation for the functions below can be found in 
# /monoid/doc/r.xml

DeclareAttribute("HClassReps", IsTransformationSemigroup);
DeclareOperation("GreensDClassOfElementNC", [IsTransformationSemigroup]); 
DeclareOperation("GreensRClassOfElementNC", [IsTransformationSemigroup]);
DeclareAttribute("GreensDClassReps", IsTransformationSemigroup);
DeclareAttribute("GreensRClassReps", IsTransformationSemigroup);
DeclareProperty("IsGreensClassOfTransSemigp", IsGreensClass);
DeclareProperty("IsRegularRClass", IsGreensClassOfTransSemigp);
DeclareGlobalFunction("IteratorOfGreensDClasses");
DeclareGlobalFunction("IteratorOfGreensRClasses");
DeclareGlobalFunction("IteratorOfDClassReps"); 
DeclareGlobalFunction("IteratorOfRClassReps");
DeclareAttribute("NrIdempotents", IsTransformationSemigroup);
DeclareAttribute("SchutzenbergerGroup", IsGreensClass);
DeclareAttribute("NrGreensDClasses", IsTransformationSemigroup); DeclareAttribute("NrGreensHClasses", IsTransformationSemigroup);
DeclareAttribute("NrGreensRClasses", IsTransformationSemigroup);

# the following functions in r.gi are currently undocumented

DeclareGlobalFunction("AddToOrbitsOfImages");
DeclareGlobalFunction("CreateImageOrbitSCCPerms");
DeclareGlobalFunction("CreateImageOrbitSchutzGp");
DeclareGlobalFunction("CreateRClass");
DeclareGlobalFunction("DisplayOrbitsOfImages");
DeclareGlobalFunction("ExpandOrbitsOfImages");
DeclareGlobalFunction("ForwardOrbitOfImage");
DeclareAttribute("GreensHClassRepsData", IsTransformationSemigroup);
DeclareAttribute("GreensRClassRepsData", IsTransformationSemigroup);
DeclareAttribute("ImageOrbit", IsGreensRClass and 
 IsGreensClassOfTransSemigp, "mutable"); # mutable essential!
DeclareGlobalFunction("ImageOrbitFromData");
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
DeclareGlobalFunction("GreensHClassRepsDataFromData");
DeclareGlobalFunction("NrIdempotentsRClassFromData");
DeclareGlobalFunction("NrRClassesOrbitsOfImages");
DeclareAttribute("OrbitsOfImages", IsTransformationSemigroup, "mutable");
DeclareGlobalFunction("PreInOrbitsOfImages");
DeclareGlobalFunction("RClassIndexFromData");
DeclareGlobalFunction("RClassRepFromData");
DeclareAttribute("RClassType", IsTransformationSemigroup);
DeclareGlobalFunction("SizeOrbitsOfImages");
DeclareGlobalFunction("TraceRClassRepsTree");

# the documentation for the functions below can be found in 
# /monoid/doc/d.xml

DeclareAttribute("PartialOrderOfDClasses", IsSemigroup, "mutable");
DeclareAttribute("NrRegularDClasses", IsTransformationSemigroup); 

# the following functions in d.gi are currently undocumented

DeclareGlobalFunction("AddToOrbitsOfKernels");
DeclareGlobalFunction("CreateDClass");
DeclareGlobalFunction("CreateKernelOrbitSchutzGp");
DeclareGlobalFunction("CreateKernelOrbitSCCRels");
DeclareGlobalFunction("CreateSchutzGpOfDClass");
DeclareGlobalFunction("DClassRClassRepsDataFromData");
 DeclareGlobalFunction("DClassRepFromData");
DeclareGlobalFunction("DClassSchutzGpFromData");
DeclareAttribute("DClassType", IsTransformationSemigroup);
DeclareGlobalFunction("DisplayOrbitsOfKernels");
DeclareGlobalFunction("ExpandOrbitsOfKernels");
DeclareGlobalFunction("ForwardOrbitOfKernel");
DeclareAttribute("GeneratorsAsListOfImages", IsTransformationSemigroup);
DeclareAttribute("GreensDClassRepsData", IsTransformationSemigroup);
DeclareAttribute("GreensLClassRepsData", IsTransformationSemigroup);
DeclareGlobalFunction("GreensLClassRepsDataFromData");
DeclareGlobalFunction("ImageOrbitCosetsFromData"); #input kernel orbit data!
DeclareAttribute("GroupHClass", IsGreensDClass and
 IsGreensClassOfTransSemigp);#M
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
DeclareOperation("GreensLClassOfElementNC", [IsTransformationSemigroup]); #M
DeclareAttribute("GreensLClassReps", IsTransformationSemigroup); #M
DeclareProperty("IsRegularLClass", IsGreensClassOfTransSemigp); #M
DeclareGlobalFunction("IteratorOfGreensLClasses"); #M
DeclareGlobalFunction("IteratorOfLClassRepsData");
DeclareGlobalFunction("IteratorOfLClassReps"); #M
DeclareGlobalFunction("LClassRepFromData");
DeclareAttribute("LClassType", IsTransformationSemigroup);
DeclareAttribute("NrGreensLClasses", IsTransformationSemigroup);

# the following functions in h.gi are currently undocumented

DeclareGlobalFunction("CreateHClass");
DeclareOperation("GreensHClassOfElementNC", [IsTransformationSemigroup]); #M
DeclareGlobalFunction("HClassRepFromData");
DeclareAttribute("HClassType", IsTransformationSemigroup);
DeclareGlobalFunction("IteratorOfGreensHClasses");#M
DeclareGlobalFunction("IteratorOfHClassReps"); #M
DeclareGlobalFunction("IteratorOfHClassRepsData");

DeclareProperty("IsIteratorOfRClassReps", IsIterator);
DeclareProperty("IsIteratorOfLClassReps", IsIterator);
DeclareProperty("IsIteratorOfDClassReps", IsIterator);
DeclareProperty("IsIteratorOfHClassReps", IsIterator);

# IteratorOfGreensRClasses and IteratorOfGreensLClasses
# should be operations so that they can be applied to 
# D-classes as well as transformation semigroups!
# and IteratorOfGreensDClass should be an operation so that I can make an
# iterator that of D-classes satisfying some properties a la smallsemi..

DeclareProperty("IsIteratorOfGreensRClasses", IsIterator);
DeclareProperty("IsIteratorOfGreensLClasses", IsIterator);
DeclareProperty("IsIteratorOfGreensDClasses", IsIterator);
DeclareProperty("IsIteratorOfGreensHClasses", IsIterator);

DeclareProperty("IsIteratorOfSemigroup", IsIterator);
DeclareProperty("IsIteratorOfRClassElements", IsIterator);
DeclareProperty("IsIteratorOfLClassElements", IsIterator);
DeclareProperty("IsIteratorOfDClassElements", IsIterator);

DeclareGlobalFunction("UnderlyingSemigroupOfIterator");
DeclareProperty("IsCitrusPkgIterator", IsIterator);

DeclareProperty("IsOrbitsOfImages", IsObject);
DeclareProperty("IsOrbitsOfKernels", IsObject);
DeclareProperty("IsCitrusPkgImgKerOrbit", IsOrbit);
