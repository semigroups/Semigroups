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

###########################################################################
##	<#GAPDoc Label="AddToOrbitsOfImages">
##	<ManSection>
##	<Func Name="AddToOrbitsOfImages" Arg="s, f, o, d"/>
##	<Description>
##	The arguments should be: <A>s</A> a semigroup or D-class, <A>f</A> a 
##	transformation, <A>o</A> the attribute <Ref Attr="OrbitsOfImages"/>
##	of <A>s</A>, and <A>d=[j, k, l, m, val, n, g]</A> where:
##	<List>
##	<Item> 
##	<A>j</A> - is the size of the image set of <A>f</A>;
##	</Item>
##	<Item> 
##		<A>k</A> - is the index of the orbit containing the image of <A>f</A>, 
##		that is,
##		<C>ImageSetOfTransformation(f)</C> is in 
##		<C>OrbitsOfImages(s)!.orbits[j][k]
##		</C>;
##	</Item>
##	<Item>
##		<A>l</A> - is the position of the image of <A>f</A> in 
##		<C>OrbitsOfImages(s)!.orbits[j][k]</C>;
##	</Item>
##	<Item> 
##		<A>m</A> - is the index of the strongly connected component of 
##		<C>OrbitsOfImages(s)!.orbits[j][k]</C> containing the image of <A>f</A>;
##	</Item>
##	<Item>
##		<A>val</A> - is the return value of <C>HTValue(
##		OrbitsOfImages(s)!.orbits[j][k]!.kernels_ht[m],
##		KernelOfTransformation(f))</C>, 
##		that is, the index of the list of 
##		representatives of R-classes of <A>s</A> with the same 
##		Schutzenberger group, 
##		strong orbit of images, and kernel as <A>f</A>; 
##	</Item>
##	<Item>
##		<A>n</A> - the length of the list
##		<C>OrbitsOfImages(s)!.orbits[j][k]!.reps[m][val]</C> 
##		of representatives of R-classes of <A>s</A> with the same Schutzenberger 
##		group, strong orbit of images, and kernel as <A>f</A>;
##	</Item>
##	<Item>
##		<A>g</A> - the transformation 
##		<C>f*OrbitsOfImages(s)!.orbits[j][k]!.perms[l]</C>, which is R-related to
##		<A>f</A> and has the property that the image <C>X</C> of <A>g</A>
##		satisfies <Display>Position(OrbitsOfImages(s)!.orbits[j][k], X)=
##		OrbitsOfImages(s)!.orbits[j][k]!.scc[m][1].</Display>
##	</Item>
##</List>
##	The result of this function is to add all the information relating to 
##  the R-class of <A>f</A> in <A>s</A> to the attribute 
##  <Ref Attr="OrbitsOfImages"/> of <A>s</A>. 
##	</Description>
##  </ManSection>
##	<#/GAPDoc><!-- non-user -->

DeclareGlobalFunction("AddToOrbitsOfImages");



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
##	<#GAPDoc Label="GreensRClassReps">
##	<ManSection>
##	<Attr Name="GreensRClassReps" Arg="S"/>
##	<Description>
##	returns the representative of all the R-classes of the transformation 
##	semigroup <C>S</C> as a list of lists ordered by the rank of the 
##	representatives and the order that the representatives are produced by the 
##	function <Ref Attr="GreensRClasses" BookName="ref"/>.  Also 
##	<C>GreensRClassReps[i][j]</C> is the representative of 
##	<Ref Attr="GradedRClasses"/><C>[i][j]</C>.
##	<Example>
##  gap> gens:=[ Transformation( [ 1, 2, 1, 2, 1 ] ), 
##  > Transformation( [ 3, 4, 2, 1, 4 ] ) ];;
##  gap> S:=Semigroup(gens);; 
##  gap> GreensRClassReps(S);
##  [ [ Transformation( [ 1, 2, 1, 2, 1 ] ), 
##    Transformation( [ 1, 2, 2, 1, 2 ] ), 
##        Transformation( [ 2, 1, 2, 1, 1 ] ) ], 
##    [ Transformation( [ 3, 4, 2, 1, 4 ] ) ] ]
##	</Example><!-- greens.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareAttribute("GreensRClassReps", IsTransformationSemigroup);

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

###########################################################################
##
##	<#GAPDoc Label="Idempotents">
##	<ManSection>
##	<Func Name="Idempotents" Arg="X[,n]"/>
##	<Description>
##	returns a list of the idempotents in the transformation semigroup or Green's 
##	class <C>X</C>. <P/>
##
##	If the optional second argument <C>n</C> is present, then a list of the 
##	idempotents in <C>S</C> of rank <C>n</C> is returned. If you are only 
##	interested in the 
##	idempotents of a given rank, then the second version of the function will 
##	likely be faster.
##	<Example>
##  gap> S:=Semigroup([ Transformation( [ 2, 3, 4, 1 ] ), 
##  > Transformation( [ 3, 3, 1, 1 ] ) ]);;
##  gap> Idempotents(S, 1);
##  [  ]
##  gap> Idempotents(S, 2);                        
##  [ Transformation( [ 1, 1, 3, 3 ] ), Transformation( [ 1, 3, 3, 1 ] ), 
##    Transformation( [ 2, 2, 4, 4 ] ), Transformation( [ 4, 2, 2, 4 ] ) ]
##  gap> Idempotents(S, 3);                        
##  [  ]
##  gap> Idempotents(S, 4);                        
##  [ Transformation( [ 1, 2, 3, 4 ] ) ]
##  gap> Idempotents(S);
##  [ Transformation( [ 1, 1, 3, 3 ] ), Transformation( [ 1, 2, 3, 4 ] ), 
##    Transformation( [ 1, 3, 3, 1 ] ), Transformation( [ 2, 2, 4, 4 ] ), 
##    Transformation( [ 4, 2, 2, 4 ] ) ]
##	</Example> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

#############################################################################
#############################################################################
# new for 4.0!

DeclareInfoClass("InfoMonoidGreens");


DeclareAttribute("OrbitsOfImages", IsTransformationSemigroup, "mutable");
DeclareAttribute("OrbitsOfKernels", IsTransformationSemigroup, "mutable");


DeclareGlobalFunction("AddToOrbitsOfKernels");


DeclareGlobalFunction("DisplayOrbitsOfImages");
DeclareGlobalFunction("DisplayOrbitsOfKernels");

DeclareGlobalFunction("ExpandOrbitsOfImages");
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

DeclareGlobalFunction("CreateSchreierTreeOfSCC");
DeclareGlobalFunction("CreateReverseSchreierTreeOfSCC");

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

DeclareOperation("GreensRClassOfElementNC", [IsTransformationSemigroup]);
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

DeclareGlobalFunction("CreateRClass");
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

DeclareGlobalFunction("ForwardOrbitOfImage");
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

