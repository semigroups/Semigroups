#############################################################################
##
#W  greens.gd
#Y  Copyright (C) 2006-2011                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##
##  This file contains fast algorithms for computing Green's relations
##  and related notions for transformation semigroups and monoid. 
##  The theory behind these algorithms is developed in 
##  
##  [LPRR1] S. A.   Linton, G.  Pfeiffer, E.  F.  Robertson, and N.   Ruskuc,
##  Groups  and actions in  transformation semigroups, to appear in Math Z.
##  (1998).
##
##  The algorithms themselves are described in
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

# the documentation for the functions below can be found in 
# /monoid/doc/r.xml

DeclareAttribute("GreensHClassReps", IsTransformationSemigroup);
DeclareOperation("GreensRClassOfElementNC", [IsTransformationSemigroup]);
DeclareAttribute("GreensRClassReps", IsTransformationSemigroup);
DeclareProperty("IsGreensClassOfTransSemigp", IsGreensClass);
DeclareAttribute("IsRegularRClass", IsGreensClassOfTransSemigp);
DeclareGlobalFunction("IteratorOfGreensRClasses");
DeclareGlobalFunction("IteratorOfRClassReps");
DeclareAttribute("NrIdempotents", IsTransformationSemigroup);
DeclareAttribute("SchutzenbergerGroup", IsGreensClass);
DeclareAttribute("NrGreensHClasses", IsTransformationSemigroup);
DeclareAttribute("NrGreensRClasses", IsTransformationSemigroup);

# the following functions in r.gi are currently undocumented

DeclareGlobalFunction("AddToOrbitsOfImages");
DeclareGlobalFunction("CreateImageOrbitSchutzGp");
DeclareGlobalFunction("CreateRClass");
DeclareGlobalFunction("CreateSchreierTreeOfSCC");
DeclareGlobalFunction("CreateReverseSchreierTreeOfSCC");
DeclareGlobalFunction("DisplayOrbitsOfImages");
DeclareGlobalFunction("ExpandOrbitsOfImages");
DeclareGlobalFunction("ForwardOrbitOfImage");
DeclareAttribute("GreensHClassRepsData", IsTransformationSemigroup);
DeclareAttribute("GreensRClassRepsData", IsTransformationSemigroup);
DeclareAttribute("ImageOrbit", IsGreensRClass and 
 IsGreensClassOfTransSemigp, "mutable"); # mutable essential!
DeclareGlobalFunction("ImageOrbitFromData");
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
DeclareGlobalFunction("IsRegularRClassData");
DeclareGlobalFunction("IteratorOfNewRClassRepsData");
DeclareGlobalFunction("IteratorOfRClassRepsData");
DeclareGlobalFunction("MultipliersOfSCCOfImageOrbit");# CreateImageOrbitPerms
DeclareGlobalFunction("NrIdempotentsRClassFromData"); #?
DeclareGlobalFunction("NrRClassesOrbitsOfImages");
DeclareAttribute("OrbitsOfImages", IsTransformationSemigroup, "mutable");
DeclareGlobalFunction("PreInOrbitsOfImages");
DeclareAttribute("RClassType", IsTransformationSemigroup);
DeclareGlobalFunction("SizeOrbitsOfImages");
DeclareGlobalFunction("TraceSchreierTreeOfSCCForward");
DeclareGlobalFunction("TraceSchreierTreeOfSCCBack");

# the documentation for the functions below can be found in 
# /monoid/doc/d.xml

DeclareAttribute("PartialOrderOfDClasses", IsSemigroup, "mutable");

# the following functions in d.gi are currently undocumented

DeclareGlobalFunction("AddToOrbitsOfKernels");
DeclareGlobalFunction("CreateDClass");
DeclareGlobalFunction("CreateKernelOrbitSchutzGp");
DeclareGlobalFunction("DClassRClassRepsDataFromData");
 DeclareGlobalFunction("DClassRepFromData");
DeclareGlobalFunction("DClassSchutzGpFromData");
DeclareAttribute("DClassType", IsTransformationSemigroup);
DeclareGlobalFunction("DisplayOrbitsOfKernels");
DeclareGlobalFunction("ExpandOrbitsOfKernels");
DeclareGlobalFunction("ForwardOrbitOfKernel");
DeclareAttribute("GeneratorsAsListOfImages", IsTransformationSemigroup);
DeclareOperation("GreensDClassOfElementNC", [IsTransformationSemigroup]);
DeclareAttribute("GreensDClassReps", IsTransformationSemigroup);
DeclareAttribute("GreensLClassRepsData", IsTransformationSemigroup);

DeclareGlobalFunction("ImageOrbitCosetsFromData"); #input kernel orbit data!
DeclareAttribute("ImageOrbitCosets", IsGreensDClass and
 IsGreensClassOfTransSemigp);
DeclareAttribute("KernelOrbit", IsGreensDClass and
 IsGreensClassOfTransSemigp, "mutable");
DeclareGlobalFunction("KernelOrbitFromData"); 
DeclareAttribute("KernelOrbitCosets", IsGreensDClass and
 IsGreensClassOfTransSemigp);
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

##
# the following functions have not yet been processed for release!


GT:=function(x,y) return x>y; end;


DeclareGlobalFunction("SizeOrbitsOfKernels");
DeclareGlobalFunction("SizeDClassRepsData");
DeclareGlobalFunction("NrLClassesOrbitsOfKernels");
DeclareGlobalFunction("InDClassRepsData");

DeclareGlobalFunction("DClassLSchutzGpFromData");
DeclareGlobalFunction("DClassLStabChainFromData");
DeclareGlobalFunction("InOrbitsOfKernels");
DeclareGlobalFunction("PreInOrbitsOfKernels");

# the following functions should be removed!

DeclareGlobalFunction("RClassRepsDataFromOrbits");
DeclareAttribute("GreensDClass", IsGreensRClass);

# newly introduced 2011

DeclareGlobalFunction("DClassImageOrbitCosetsFromData");

DeclareGlobalFunction("CreateSchutzGpOfDClass");

##

DeclareInfoClass("InfoMonoidGreens");

DeclareAttribute("OrbitsOfKernels", IsTransformationSemigroup, "mutable");

DeclareGlobalFunction("MultipliersOfSCCOfKernelOrbit");

DeclareGlobalFunction("RightSchutzGpOfKerOrbit");
DeclareGlobalFunction("LeftSchutzGpOfKerOrbit");
DeclareGlobalFunction("SchutzGpOfDClass");

DeclareGlobalFunction("RClassRepFromData");
# RClassRep = Representative
DeclareAttribute("RClassImageOrbitSCC", IsGreensRClass and 
 IsGreensClassOfTransSemigp);
DeclareOperation("GreensHClassOfElementNC", [IsTransformationSemigroup]);
DeclareAttribute("LClassSchutzGp", IsGreensLClass and 
 IsGreensClassOfTransSemigp);
DeclareAttribute("LClassSCC", IsGreensLClass and 
 IsGreensClassOfTransSemigp);
DeclareAttribute("LClassRels", IsGreensLClass and 
 IsGreensClassOfTransSemigp);

DeclareGlobalFunction("HClassRepFromData");
DeclareGlobalFunction("LClassRepFromData");
DeclareAttribute("LClassKernelOrbit", IsGreensLClass and 
IsGreensClassOfTransSemigp);
DeclareGlobalFunction("LClassSchutzGpFromData");
DeclareGlobalFunction("LClassRelsFromData");
DeclareGlobalFunction("LClassSCCFromData");
DeclareOperation("GreensLClassOfElementNC", [IsTransformationSemigroup]);

DeclareGlobalFunction("DClassOrbitsFromData");
DeclareGlobalFunction("DClassKerSchutzGpFromData");
DeclareGlobalFunction("DClassImgSchutzGpFromData");
DeclareGlobalFunction("DClassRelsFromData");
DeclareGlobalFunction("DClassImgSCCFromData");
DeclareAttribute("GreensRClass", IsGreensHClass);
DeclareAttribute("GreensLClass", IsGreensHClass);
DeclareGlobalFunction("CreateLClass");
DeclareGlobalFunction("CreateHClass");

DeclareAttribute("LClassType", IsTransformationSemigroup);
DeclareAttribute("HClassType", IsTransformationSemigroup);

DeclareGlobalFunction("KerRightToImgLeftFromData");
DeclareAttribute("KerRightToImgLeft", IsGreensDClass and 
IsGreensClassOfTransSemigp);
DeclareAttribute("GreensLClassReps", IsTransformationSemigroup);

DeclareProperty("IsEnumeratorOfRClassElements", IsEnumeratorByFunctions);
DeclareProperty("IsEnumeratorOfLClassElements", IsEnumeratorByFunctions);
DeclareProperty("IsEnumeratorOfDClassElements", IsEnumeratorByFunctions);

DeclareGlobalFunction("IteratorOfLClassRepsData");
DeclareGlobalFunction("IteratorOfHClassRepsData");
DeclareGlobalFunction("IteratorOfLClassReps");
DeclareGlobalFunction("IteratorOfDClassReps");
DeclareGlobalFunction("IteratorOfHClassReps");
DeclareGlobalFunction("IteratorOfNewDClassReps");

#DeclareGlobalFunction("EnumeratorOfRClassReps");

DeclareProperty("IsIteratorOfRClassRepsData", IsIterator);
DeclareProperty("IsIteratorOfHClassRepsData", IsIterator);
DeclareProperty("IsIteratorOfRClassReps", IsIterator);
DeclareProperty("IsIteratorOfLClassReps", IsIterator);
DeclareProperty("IsIteratorOfDClassReps", IsIterator);
DeclareProperty("IsIteratorOfHClassReps", IsIterator);

# IteratorOfGreensRClasses and IteratorOfGreensLClasses
# should be operations so that they can be applied to 
# D-classes as well as transformation semigroups!

DeclareGlobalFunction("IteratorOfGreensLClasses");
DeclareGlobalFunction("IteratorOfGreensDClasses");
# and IteratorOfGreensDClass should be an operation so that I can make an
# iterator that of D-classes satisfying some properties a la smallsemi..
DeclareOperation("IteratorOfGreensHClasses", [IsTransformationSemigroup]);

DeclareProperty("IsIteratorOfGreensRClasses", IsIterator);
DeclareProperty("IsIteratorOfGreensLClasses", IsIterator);
DeclareProperty("IsIteratorOfGreensDClasses", IsIterator);
DeclareProperty("IsIteratorOfGreensHClasses", IsIterator);

DeclareOperation("IteratorOfIdempotents", [IsTransformationSemigroup]);

DeclareProperty("IsIteratorOfSemigroup", IsIterator);
DeclareProperty("IsIteratorOfRClassElements", IsIterator);
DeclareProperty("IsIteratorOfLClassElements", IsIterator);
DeclareProperty("IsIteratorOfDClassElements", IsIterator);

DeclareAttribute("UnderlyingSemigroupOfIterator", 
IsIterator);

DeclareAttribute("NrGreensLClasses", IsTransformationSemigroup);
DeclareAttribute("NrGreensDClasses", IsTransformationSemigroup);

DeclareAttribute("IsRegularLClass", IsGreensClassOfTransSemigp);

DeclareAttribute("DClassLCosets", IsGreensDClass and 
 IsGreensClassOfTransSemigp);
DeclareAttribute("DClassRCosets", IsGreensDClass and 
 IsGreensClassOfTransSemigp);

DeclareGlobalFunction("DClassRCosetsFromData");
DeclareGlobalFunction("DClassLCosetsFromData");
DeclareProperty("IsOrbitsOfImages", IsObject);
DeclareProperty("IsOrbitsOfKernels", IsObject);
DeclareProperty("IsMonoidPkgImgKerOrbit", IsOrbit);
DeclareAttribute("GroupHClass", IsGreensDClass and IsGreensClassOfTransSemigp);




