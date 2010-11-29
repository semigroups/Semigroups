#############################################################################
##
#W  greens.gd
#Y  Copyright (C) 2006-2010                             James D. Mitchell
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
DeclareGlobalFunction("CreateRClass");
DeclareGlobalFunction("CreateSchreierTreeOfSCC");
DeclareGlobalFunction("CreateReverseSchreierTreeOfSCC");
DeclareGlobalFunction("DisplayOrbitsOfImages");
DeclareGlobalFunction("ExpandOrbitsOfImages");
DeclareGlobalFunction("ForwardOrbitOfImage");
DeclareAttribute("GreensHClassRepsData", IsTransformationSemigroup);
DeclareGlobalFunction("InOrbitsOfImages");
DeclareGlobalFunction("IsRegularRClassData");
DeclareGlobalFunction("IteratorOfNewRClassRepsData");
DeclareGlobalFunction("IteratorOfRClassRepsData");
DeclareGlobalFunction("MultipliersOfSCCOfImageOrbit");
DeclareGlobalFunction("NrRClassesOrbitsOfImages");
DeclareAttribute("OrbitsOfImages", IsTransformationSemigroup, "mutable");
DeclareGlobalFunction("PreInOrbitsOfImages");
DeclareAttribute("RClassImageOrbit", IsGreensRClass and 
 IsGreensClassOfTransSemigp);
DeclareGlobalFunction("RClassImageOrbitFromData");
DeclareGlobalFunction("RClassPermsFromData");
DeclareAttribute("RClassPerms", IsGreensRClass and 
 IsGreensClassOfTransSemigp);
DeclareGlobalFunction("RClassSCCFromData");
DeclareAttribute("RClassSCC", IsGreensRClass and 
 IsGreensClassOfTransSemigp);
DeclareGlobalFunction("RClassSchutzGpFromData");
DeclareGlobalFunction("RClassStabChainFromData");
DeclareAttribute("RClassStabChain", IsGreensRClass and 
 IsGreensClassOfTransSemigp);
DeclareAttribute("RClassType", IsTransformationSemigroup);
DeclareGlobalFunction("SchutzGpOfImageOrbit");
DeclareGlobalFunction("SizeOrbitsOfImages");
DeclareGlobalFunction("TraceSchreierTreeOfSCCForward");
DeclareGlobalFunction("TraceSchreierTreeOfSCCBack");

# the documentation for the functions below can be found in 
# /monoid/doc/d.xml

DeclareAttribute("PartialOrderOfDClasses", IsSemigroup, "mutable");


# the following functions should be removed!

DeclareGlobalFunction("RClassRepsDataFromOrbits");





# the following functions have not yet been processed for release!

DeclareInfoClass("InfoMonoidGreens");

DeclareGlobalFunction("AddToOrbitsOfKernels");
DeclareAttribute("OrbitsOfKernels", IsTransformationSemigroup, "mutable");

DeclareGlobalFunction("DisplayOrbitsOfKernels");


DeclareGlobalFunction("ExpandOrbitsOfKernels");


DeclareGlobalFunction("SizeOrbitsOfKernels");
DeclareGlobalFunction("SizeDClassRepsData");

DeclareGlobalFunction("NrLClassesOrbitsOfKernels");

DeclareGlobalFunction("InOrbitsOfKernels");
DeclareGlobalFunction("InDClassRepsData");




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
DeclareGlobalFunction("LClassKernelOrbitFromData");
DeclareAttribute("LClassKernelOrbit", IsGreensLClass and 
IsGreensClassOfTransSemigp);
DeclareGlobalFunction("LClassSchutzGpFromData");
DeclareGlobalFunction("LClassStabChainFromData");
DeclareGlobalFunction("LClassRelsFromData");
DeclareGlobalFunction("LClassSCCFromData");
DeclareOperation("GreensLClassOfElementNC", [IsTransformationSemigroup]);

DeclareGlobalFunction("DClassRepFromData");
DeclareGlobalFunction("DClassKernelOrbitFromData");
DeclareGlobalFunction("DClassKernelOrbit");
DeclareGlobalFunction("DClassImageOrbitFromData");
DeclareGlobalFunction("DClassImageOrbit");
DeclareGlobalFunction("DClassOrbitsFromData");
DeclareGlobalFunction("DClassKerSchutzGpFromData");
DeclareGlobalFunction("DClassImgSchutzGpFromData");
DeclareGlobalFunction("DClassSchutzGpFromData");
DeclareGlobalFunction("DClassRelsFromData");
DeclareGlobalFunction("DClassPermsFromData");
DeclareGlobalFunction("DClassKerSCCFromData");
DeclareGlobalFunction("DClassKernelSCC");
DeclareGlobalFunction("DClassImgSCCFromData");

DeclareGlobalFunction("DClassRClassRepsDataFromData");

DeclareAttribute("GreensDClass", IsGreensRClass);
DeclareAttribute("GreensRClass", IsGreensHClass);
DeclareAttribute("GreensLClass", IsGreensHClass);


DeclareGlobalFunction("CreateDClass");
DeclareGlobalFunction("CreateLClass");
DeclareGlobalFunction("CreateHClass");

DeclareAttribute("DClassType", IsTransformationSemigroup);
DeclareAttribute("LClassType", IsTransformationSemigroup);
DeclareAttribute("HClassType", IsTransformationSemigroup);

DeclareGlobalFunction("KerRightToImgLeftFromData");
DeclareAttribute("KerRightToImgLeft", IsGreensDClass and 
IsGreensClassOfTransSemigp);

DeclareOperation("GreensDClassOfElementNC", [IsTransformationSemigroup]);

DeclareAttribute("GreensLClassReps", IsTransformationSemigroup);
DeclareAttribute("GreensDClassReps", IsTransformationSemigroup);

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
DeclareGlobalFunction("DClassStabChainFromData");


DeclareGlobalFunction("ForwardOrbitOfKernel");

DeclareAttribute("GreensRClassRepsData", IsTransformationSemigroup);
#DeclareGlobalFunction("RClassRepsData");
DeclareAttribute("GreensLClassRepsData", IsTransformationSemigroup);


DeclareProperty("IsOrbitsOfImages", IsObject);
DeclareProperty("IsOrbitsOfKernels", IsObject);

DeclareProperty("IsMonoidPkgImgKerOrbit", IsOrbit);

DeclareAttribute("GroupHClass", IsGreensDClass and IsGreensClassOfTransSemigp);

# probably get rid of the following...


#DeclareGlobalFunction("DisplayOrbitOfImage");
#DeclareAttribute("IsGlobalDClass", IsGreensDClass);
#DeclareAttribute("IsGlobalRClass", IsGreensRClass);
#DeclareGlobalFunction("ExpandDClassRepsData");
#DeclareAttribute("DClassRepsData", IsTransformationSemigroup, "mutable");
#DeclareGlobalFunction("DClassCosetsFromData");
#DeclareGlobalFunction("DClassStabChainFromData");
#DeclareAttribute("RClassDataFromRep", IsTransformation);
#DeclareGlobalFunction("GreensRClassRepsNC");
#DeclareGlobalFunction("IsInSCCOfOrbitNC");
#DeclareGlobalFunction("IsInSCCOfOrbit");
#DeclareGlobalFunction("IsInOrbit");
#DeclareProperty("RClassRepsData", IsTransformationSemigroup);
# maybe remove the following and add them to AddToOrbitsOfImages...
#DeclareGlobalFunction("ForwardOrbitOfImageNC");
#DeclareGlobalFunction("ForwardOrbitOfKernelNC");

