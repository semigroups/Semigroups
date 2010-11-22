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

# the documentation for the functions in this file can be found in 
# /monoid/doc/r.xml

DeclareGlobalFunction("AddToOrbitsOfImages");
DeclareGlobalFunction("CreateRClass");
DeclareGlobalFunction("CreateSchreierTreeOfSCC");
DeclareGlobalFunction("CreateReverseSchreierTreeOfSCC");
DeclareGlobalFunction("DisplayOrbitsOfImages");
DeclareGlobalFunction("ExpandOrbitsOfImages");
DeclareGlobalFunction("ForwardOrbitOfImage");
DeclareOperation("GreensRClassOfElementNC", [IsTransformationSemigroup]);
DeclareAttribute("GreensRClassReps", IsTransformationSemigroup);

###########################################################################

###########################################################################
##
##	<#GAPDoc Label="IsGreensClassOfTransSemigp">
##	<ManSection>
##	<Prop Name="IsGreensClassOfTransSemigp" Arg="C"/>
##	<Description>
##	returns <C>true</C> if <C>C</C> is a Green's class of a transformation 
##	semigroup and returns <C>false</C> otherwise.<P/>
##
##	This attribute is required so that a Green's class knowns that it belongs to 
##	a transformation semigroup, so that the methods defined in the
##	<Package>MONOID</Package> are used in preference to those in the library.
##	<Example>
##  gap> a:=Transformation( [ 2, 1, 4, 5, 6, 3 ] );;
##  gap> b:=Transformation( [ 2, 3, 1, 5, 4, 1 ] );;
##  gap> M:=Semigroup(a,b);;
##  gap> GreensLClassOfElement(M,a);
##  {Transformation( [ 2, 1, 4, 5, 6, 3 ] )}
##  gap> IsGreensClassOfTransSemigp(last);
##  true
##  gap> f:=FreeSemigroup(3);;
##  gap> a:=f.1;; b:=f.2;; c:=f.3;; 
##  gap> s:=f/[[a^2, a], [b^2,b], [c^2,c], [a*b,a], [b*a,b], [a*c,a], 
##  > [c*a,c], [b*c,b],[c*b,c]];
##  &lt;fp semigroup on the generators [ s1, s2, s3 ]>
##  gap> Size(s);
##  3
##  gap> GreensLClassOfElement(s,a);
##  {s1}
##  gap> IsGreensClassOfTransSemigp(last);
##  false
##	</Example> <!-- greens.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

DeclareProperty("IsGreensClassOfTransSemigp", IsGreensClass);




#############################################################################
##
##	<#GAPDoc Label="SchutzenbergerGroup">
##	<ManSection>
##	<Attr Name="SchutzenbergerGroup" Arg="D"/>
##	<Description>
##	if <C>D</C> satisfies <Ref Attr="IsGreensRClassData"/>, 
##	<Ref Attr="IsGreensLClassData"/>, <Ref Attr="IsGreensHClassData"/>, or 
##	<Ref Attr="IsGreensDClassData"/>, then <C>SchutzenbergerGroup</C> returns the 
##	<C>schutz</C> component of <C>D</C>. <P/>
##	
##	If <C>D</C> satisfies <Ref Attr="IsGreensRClass" BookName="ref"/>, 
##	<Ref Attr="IsGreensLClass" BookName="ref"/>, 
##	<Ref Attr="IsGreensHClass" BookName="ref"/>, 
##	<Ref Attr="IsGreensDClass" BookName="ref"/>, then <C>SchutzenbergerGroup</C> 
##	returns the <C>schutz</C> component of <C>GreensData</C> with argument
##	<C>D</C>.
##	<Example>
##  gap> gens:=[ Transformation( [ 4, 4, 3, 5, 3 ] ), 
##  > Transformation( [ 5, 1, 1, 4, 1 ] ), 
##  > Transformation( [ 5, 5, 4, 4, 5 ] ) ];;
##  gap> f:=Transformation( [ 4, 5, 5, 5, 5 ] );;
##  gap> SchutzenbergerGroup(GreensDClassOfElement(S, f));
##  Group([ (), (4,5) ])
##  gap> SchutzenbergerGroup(GreensRClassOfElement(S, f));
##  Group([ (), (4,5) ])
##  gap> SchutzenbergerGroup(GreensLClassOfElement(S, f));
##  Group([ (), (4,5) ])
##  gap> SchutzenbergerGroup(GreensHClassOfElement(S, f));
##  Group([ (), (4,5) ])
##	</Example> <!-- greens.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("SchutzenbergerGroup", IsGreensClass);

#############################################################################
##
##	<#GAPDoc Label="PartialOrderOfDClasses">
##	<ManSection>
##	<Attr Name="PartialOrderOfDClasses" Arg="S"/>
##	<Description>
##	returns the partial order of the <C>D</C>-classes of <C>S</C> as a directed 
##	graph in <Package>GRAPE</Package>, if it is installed, using the command 
##	<Log>
##	Graph(Group(()), [1..Length(GreensDClasses(S))], OnPoints, function(x,y)
##	return y in poset[x]; end, true); ;
##	</Log>
##	where <C>y</C> in <C>poset[x]</C> if and only if 
##	<C>S^1yS^1</C> is a subset of  <C>S^1 xS^1</C>. <P/>
##	
##	If <Package>GRAPE</Package> is not loaded, 
##	then the list <C>poset</C> is returned instead.
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("PartialOrderOfDClasses", IsSemigroup, "mutable");



#############################################################################
#############################################################################
# new for 4.0!

DeclareInfoClass("InfoMonoidGreens");


DeclareAttribute("OrbitsOfImages", IsTransformationSemigroup, "mutable");
DeclareAttribute("OrbitsOfKernels", IsTransformationSemigroup, "mutable");


DeclareGlobalFunction("AddToOrbitsOfKernels");



DeclareGlobalFunction("DisplayOrbitsOfKernels");


DeclareGlobalFunction("ExpandOrbitsOfKernels");


DeclareGlobalFunction("SizeOrbitsOfImages");
DeclareGlobalFunction("SizeOrbitsOfKernels");
DeclareGlobalFunction("SizeDClassRepsData");

DeclareGlobalFunction("NrRClassesOrbitsOfImages");
DeclareGlobalFunction("NrLClassesOrbitsOfKernels");

DeclareGlobalFunction("PreInOrbitsOfImages");
DeclareGlobalFunction("InOrbitsOfImages");
DeclareGlobalFunction("InOrbitsOfKernels");
DeclareGlobalFunction("InDClassRepsData");



DeclareGlobalFunction("TraceSchreierTreeOfSCCForward");
DeclareGlobalFunction("TraceSchreierTreeOfSCCBack");

DeclareGlobalFunction("MultipliersOfSCCOfImageOrbit");
DeclareGlobalFunction("MultipliersOfSCCOfKernelOrbit");

DeclareGlobalFunction("SchutzGpOfImageOrbit");
DeclareGlobalFunction("RightSchutzGpOfKerOrbit");
DeclareGlobalFunction("LeftSchutzGpOfKerOrbit");
DeclareGlobalFunction("SchutzGpOfDClass");

DeclareGlobalFunction("RClassRepFromData");
# RClassRep = Representative
DeclareGlobalFunction("RClassImageOrbitFromData");
DeclareAttribute("RClassImageOrbit", IsGreensRClass and 
 IsGreensClassOfTransSemigp);
DeclareAttribute("RClassImageOrbitSCC", IsGreensRClass and 
 IsGreensClassOfTransSemigp);
DeclareGlobalFunction("RClassSchutzGpFromData");
# RclassSchutzGp = SchutzenbergerGroup!
DeclareGlobalFunction("RClassStabChainFromData");
DeclareAttribute("RClassStabChain", IsGreensRClass and 
 IsGreensClassOfTransSemigp);
DeclareGlobalFunction("RClassPermsFromData");
DeclareAttribute("RClassPerms", IsGreensRClass and 
 IsGreensClassOfTransSemigp);
DeclareGlobalFunction("RClassSCCFromData");
DeclareAttribute("RClassSCC", IsGreensRClass and 
 IsGreensClassOfTransSemigp);


DeclareOperation("GreensHClassOfElementNC", [IsTransformationSemigroup]);

#JDM remove the following later!
DeclareGlobalFunction("RClassRepsDataFromOrbits");

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

DeclareAttribute("RClassType", IsTransformationSemigroup);
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

DeclareGlobalFunction("IteratorOfRClassRepsData");
DeclareGlobalFunction("IteratorOfLClassRepsData");
DeclareGlobalFunction("IteratorOfHClassRepsData");
DeclareGlobalFunction("IteratorOfNewRClassRepsData");
DeclareGlobalFunction("IteratorOfRClassReps");
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

DeclareGlobalFunction("IteratorOfGreensRClasses");
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

DeclareAttribute("NrGreensRClasses", IsTransformationSemigroup);
DeclareAttribute("NrGreensLClasses", IsTransformationSemigroup);
DeclareAttribute("NrGreensDClasses", IsTransformationSemigroup);
DeclareAttribute("NrGreensHClasses", IsTransformationSemigroup);
DeclareAttribute("NrIdempotents", IsTransformationSemigroup);

DeclareAttribute("IsRegularRClass", IsGreensClassOfTransSemigp);
DeclareAttribute("IsRegularLClass", IsGreensClassOfTransSemigp);
DeclareGlobalFunction("IsRegularRClassData");



DeclareAttribute("DClassLCosets", IsGreensDClass and 
 IsGreensClassOfTransSemigp);
DeclareAttribute("DClassRCosets", IsGreensDClass and 
 IsGreensClassOfTransSemigp);

DeclareGlobalFunction("DClassRCosetsFromData");
DeclareGlobalFunction("DClassLCosetsFromData");
DeclareGlobalFunction("DClassStabChainFromData");


DeclareGlobalFunction("ForwardOrbitOfKernel");

DeclareAttribute("GreensRClassRepsData", IsTransformationSemigroup);
DeclareGlobalFunction("RClassRepsData");
DeclareAttribute("GreensLClassRepsData", IsTransformationSemigroup);

DeclareAttribute("GreensHClassReps", IsTransformationSemigroup);
DeclareAttribute("GreensHClassRepsData", IsTransformationSemigroup);

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

