##
## properties.gd
## Version 3.1.2
## Fri 11 Jul 2008 13:36:12 BST
##

##  The functions in this file are used to test whether a given transformation
##  semigroup has a given property. The algorithms can be found in:
##  
##  R. Gray and J. D. Mitchell, Largest subsemigroups of the full transformation 
##  monoid, to appear in Discrete Math.
##

###########################################################################
##
##	<#GAPDoc Label="IsCompletelyRegularSemigroup">
##	<ManSection>
##	<Prop Name="IsCompletelyRegularSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <C>S</C> is completely 
##	regular and <C>false</C> otherwise.<P/>
##	A semigroup is <E>completely regular</E> 
##	if every element is contained in a subgroup.
##
##	<Example>	
##  gap&gt; gens:=[ Transformation( [ 1, 2, 4, 3, 6, 5, 4 ] ), 
##  &gt;  Transformation( [ 1, 2, 5, 6, 3, 4, 5 ] ), 
##  &gt;  Transformation( [ 2, 1, 2, 2, 2, 2, 2 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsCompletelyRegularSemigroup(S);
##  true
##  gap&gt; S:=RandomSemigroup(5,5);;
##  gap&gt; IsSimpleSemigroup(S);
##  false
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareProperty("IsCompletelyRegularSemigroup", IsTransformationSemigroup);

###########################################################################
##
##	<#GAPDoc Label="IsCompletelySimpleSemigroup">
##	<ManSection><Heading>IsSimpleSemigroup</Heading>
##	<Prop Name="IsSimpleSemigroup" Arg="S"/>
##	<Prop Name="IsCompletelySimpleSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <C>S</C> is simple and 
##	<C>false</C> otherwise.<P/>
##
##	A semigroup is <E>simple</E> if it has no proper
##	2-sided ideals. A semigroup is <E>completely simple</E> if it is simple and 
##	possesses minimal left and right ideals. A finite semigroup is simple if and 
##	only if it is completely simple. 
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 2, 2, 4, 4, 6, 6, 8, 8, 10, 10, 12, 12, 2 ] ), 
##  &gt;  Transformation( [ 1, 1, 3, 3, 5, 5, 7, 7, 9, 9, 11, 11, 3 ] ), 
##  &gt;  Transformation( [ 1, 7, 3, 9, 5, 11, 7, 1, 9, 3, 11, 5, 5 ] ), 
##  &gt;  Transformation( [ 7, 7, 9, 9, 11, 11, 1, 1, 3, 3, 5, 5, 7 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsSimpleSemigroup(S);
##  true
##  gap&gt; IsCompletelySimpleSemigroup(S);
##  true
##  gap&gt; S:=RandomSemigroup(5,5);;
##  gap&gt; IsSimpleSemigroup(S);
##  false
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

if not IsBound(IsCompletelySimpleSemigroup) then 
	DeclareSynonymAttr("IsCompletelySimpleSemigroup", IsFinite and 
		IsSimpleSemigroup);
fi;

#DeclareProperty("IsCompletelySimpleSemigroup", IsTransformationSemigroup);

###########################################################################
##
##	<#GAPDoc Label="IsGroupAsSemigroup">
##	<ManSection>
##	<Prop Name="IsGroupAsSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <C>S</C> is a group and 
##	<C>false</C> otherwise.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 2, 4, 5, 3, 7, 8, 6, 9, 1 ] ), 
##  &gt;  Transformation( [ 3, 5, 6, 7, 8, 1, 9, 2, 4 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsGroupAsSemigroup(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareProperty("IsGroupAsSemigroup", IsTransformationSemigroup);

###########################################################################
##
##	<#GAPDoc Label="IsCommutativeSemigroup">
##	<ManSection>
##	<Prop Name="IsCommutativeSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <C>S</C> is commutative 
##	and <C>false</C> otherwise. The function 
##	<Ref Prop="IsCommutative" BookName="ref"/> can also be used to test if a 
##	semigroup is commutative.  <P/>
##
##	A semigroup <C>S</C> is <E>commutative</E> if 
##	<C>xy=yx</C> for all <C>x,y</C> in <C>S</C>.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 2, 4, 5, 3, 7, 8, 6, 9, 1 ] ), 
##  &gt;  Transformation( [ 3, 5, 6, 7, 8, 1, 9, 2, 4 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsCommutativeSemigroup(S);
##  true
##  gap&gt; IsCommutative(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareProperty("IsCommutativeSemigroup", IsTransformationSemigroup);

###########################################################################
##
##	<#GAPDoc Label="IsRegularSemigroup">
##	<ManSection>
##	<Prop Name="IsRegularSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <C>S</C> is a regular
##	semigroup and <C>false</C> otherwise. The algorithm used here is essentially 
##	the same algorithm as that used for 
##	<Ref Attr="GreensRClasses" BookName="ref"/> in <Package>MONOID</Package>. If 
##	<C>S</C> is regular, then <C>S</C> will have the attribute 
##	<C>GreensRClasses</C> after <C>IsRegularSemigroup</C> is invoked. <P/>
##
##	A semigroup <C>S</C> is <E>regular</E> if for all <C>x</C> in <C>S</C> there 
##	exists <C>y</C> in <C>S</C> such that <C>xyx=x</C>.
##
##	<Example>
##  gap&gt; IsRegularSemigroup(FullTransformationSemigroup(5));
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

###########################################################################
##
##	<#GAPDoc Label="IsInverseSemigroup">
##	<ManSection>
##	<Prop Name="IsInverseSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <C>S</C> is an inverse
##	semigroup and <C>false</C> otherwise.<P/>
##
##	A semigroup <C>S</C> is an <E>inverse semigroup</E> if every element 
##	<C>x</C> in <C>S</C> has a unique semigroup inverse, that is, a unique 
##	element <C>y</C> such that <C>xyx=x</C> and <C>yxy=y</C>.
##
##	<Example>
##  gap&gt; gens:=[Transformation([1,2,4,5,6,3,7,8]),
##  &gt; Transformation([3,3,4,5,6,2,7,8]),
##  &gt;Transformation([1,2,5,3,6,8,4,4])];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsInverseSemigroup(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

###########################################################################
##
##	<#GAPDoc Label="IsCliffordSemigroup">
##	<ManSection>
##	<Prop Name="IsCliffordSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <C>S</C> is a Clifford 
##	semigroup and <C>false</C> otherwise.<P/>
##
##  A semigroup <C>S</C> is a <E>Clifford 
##	semigroup</E> if it is a regular semigroup whose idempotents are central,
##	 that is, for all <C>e</C> in <C>S</C> with <C>e^2=e</C> and <C>x</C> in 
##	<C>S</C> we have that <C>ex=xe</C>.
##
##	<Example>
##  gap&gt; gens:=[Transformation([1,2,4,5,6,3,7,8]),
##  &gt; Transformation([3,3,4,5,6,2,7,8]),
##  &gt;Transformation([1,2,5,3,6,8,4,4])];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsCliffordSemigroup(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareProperty("IsCliffordSemigroup", IsTransformationSemigroup);

###########################################################################
##
##	<#GAPDoc Label="IsBand">
##	<ManSection>
##	<Prop Name="IsBand" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <C>S</C> is a band and 
##	<C>false</C> otherwise.<P/>
##
##	A semigroup <C>S</C> is a <E>band</E> if every element is an idempotent, 
##	that is, <C>x^2=x</C> for all <C>x</C> in <C>S</C>.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 1 ] ), 
##  &gt; Transformation( [ 2, 2, 2, 5, 5, 5, 8, 8, 8, 2 ] ), 
##  &gt; Transformation( [ 3, 3, 3, 6, 6, 6, 9, 9, 9, 3 ] ), 
##  &gt; Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 4 ] ), 
##  &gt; Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 7 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsBand(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareProperty("IsBand", IsTransformationSemigroup);

###########################################################################
##
##	<#GAPDoc Label="IsRectangularBand">
##	<ManSection>
##	<Prop Name="IsRectangularBand" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <C>S</C> is a 
##	rectangular band and <C>false</C> otherwise.<P/>
##
##	A semigroup <C>S</C> is a <E>rectangular band</E> if for all <C>x,y,z</C> in 
##	<C>S</C> we have that <C>x^2=x</C> and <C>xyz=xz</C>.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 1 ] ), 
##  &gt; Transformation( [ 2, 2, 2, 5, 5, 5, 8, 8, 8, 2 ] ), 
##  &gt; Transformation( [ 3, 3, 3, 6, 6, 6, 9, 9, 9, 3 ] ), 
##  &gt; Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 4 ] ), 
##  &gt; Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 7 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsRectangularBand(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareProperty("IsRectangularBand", IsTransformationSemigroup);

###########################################################################
##
##	<#GAPDoc Label="IsSemiBand">
##	<ManSection>
##	<Prop Name="IsSemiBand" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <C>S</C> is a 
##	semiband and <C>false</C> otherwise.<P/>
##
##	A semigroup <C>S</C> is a <E>semiband</E> if it is generated by its 
##	idempotent elements, that is, elements satisfying <C>x^2=x</C>.
##
##	<Example>
##  gap&gt; S:=FullTransformationSemigroup(4);;
##  gap&gt; x:=Transformation( [ 1, 2, 3, 1 ] );;
##  gap&gt; D:=GreensDClassOfElement(S, x);;
##  gap&gt; T:=Semigroup(Elements(D));;
##  gap&gt; IsSemiBand(T);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareProperty("IsSemiBand", IsTransformationSemigroup);

###########################################################################
##
##	<#GAPDoc Label="IsOrthodoxSemigroup">
##	<ManSection>
##	<Prop Name="IsOrthodoxSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <C>S</C> is 
##	orthodox and <C>false</C> otherwise.<P/>
##	
##	A semigroup is an <E>orthodox semigroup</E> if its idempotent elements 
##	form a subsemigroup.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 1, 1, 1, 4, 5, 4 ] ), 
##  &gt;  Transformation( [ 1, 2, 3, 1, 1, 2 ] ), 
##  &gt;  Transformation( [ 1, 2, 3, 1, 1, 3 ] ), 
##  &gt;  Transformation( [ 5, 5, 5, 5, 5, 5 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsOrthodoxSemigroup(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareProperty("IsOrthodoxSemigroup", IsTransformationSemigroup);

###########################################################################
##
##	<#GAPDoc Label="IsRightZeroSemigroup">
##	<ManSection>
##	<Prop Name="IsRightZeroSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <C>S</C> is 
##	a right zero semigroup and <C>false</C> otherwise.<P/>
##
##	A semigroup <C>S</C> is a <E>right zero semigroup</E> if <C>xy=y</C> for all 
##	<C>x,y</C> in <C>S</C>.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 2, 1, 4, 3, 5 ] ), 
##  &gt;  Transformation( [ 3, 2, 3, 1, 1 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsRightZeroSemigroup(S);
##  false
##  gap&gt; gens:=[Transformation( [ 1, 2, 3, 3, 1 ] ), 
##  &gt;  Transformation( [ 1, 2, 4, 4, 1 ] )];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsRightZeroSemigroup(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>


DeclareProperty("IsRightZeroSemigroup", IsTransformationSemigroup);

###########################################################################
##
##	<#GAPDoc Label="IsLeftZeroSemigroup">
##	<ManSection>
##	<Prop Name="IsLeftZeroSemigroup" Arg="S"/>
##	<Description>
##	returns <C>true</C> if the transformation semigroup <C>S</C> is 
##	a left zero semigroup and <C>false</C> otherwise.<P/>
##
##	A semigroup <C>S</C> is a <E>left zero semigroup</E> if <C>xy=x</C> for all 
##	<C>x,y</C> in <C>S</C>.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 2, 1, 4, 3, 5 ] ), 
##  &gt;  Transformation( [ 3, 2, 3, 1, 1 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsRightZeroSemigroup(S);
##  false
##  gap&gt; gens:=[Transformation( [ 1, 2, 3, 3, 1 ] ), 
##  &gt; Transformation( [ 1, 2, 3, 3, 3 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsLeftZeroSemigroup(S);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareProperty("IsLeftZeroSemigroup", IsTransformationSemigroup);

###########################################################################
##
##	<#GAPDoc Label="MultiplicativeZero">
##	<ManSection>
##	<Prop Name="MultiplicativeZero" Arg="S"/>
##	<Description> 
##	returns the multiplicative zero of the transformation semigroup <C>S</C> if 
##	it has one and returns <C>fail</C> otherwise. 
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 1, 4, 2, 6, 6, 5, 2 ] ), 
##  &gt; Transformation( [ 1, 6, 3, 6, 2, 1, 6 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; MultiplicativeZero(S);
##  Transformation( [ 1, 1, 1, 1, 1, 1, 1 ] )
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

###########################################################################
##
##	<#GAPDoc Label="IsZeroSemigroup">
##	<ManSection>
##	<Prop Name="IsZeroSemigroup" Arg="S"/>
##	<Description> 
##	returns <C>true</C> if the transformation semigroup <C>S</C> is 
##	a zero semigroup or if <C>S</C> was created using the 
##	<Ref Oper="ZeroSemigroup"/> command. Otherwise <C>false</C> is returned.<P/>
##
##	A semigroup <C>S</C> is a <E>zero semigroup</E> if there exists an element 
##	<C>0</C> in <C>S</C> such that <C>xy=0</C> for all <C>x,y</C> in <C>S</C>.
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 4, 7, 6, 3, 1, 5, 3, 6, 5, 9 ] ), 
##  &gt; Transformation( [ 5, 3, 5, 1, 9, 3, 8, 7, 4, 3 ] ), 
##  &gt; Transformation( [ 5, 10, 10, 1, 7, 6, 6, 8, 7, 7 ] ), 
##  &gt; Transformation( [ 7, 4, 3, 3, 2, 2, 3, 2, 9, 3 ] ), 
##  &gt; Transformation( [ 8, 1, 3, 4, 9, 6, 3, 7, 1, 6 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; IsZeroSemigroup(S);
##  false
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

DeclareProperty("IsZeroSemigroup", IsTransformationSemigroup);

###########################################################################
##
##	<#GAPDoc Label="IsZeroGroup">
##	<ManSection>
##	<Prop Name="IsZeroGroup" Arg="S"/>
##	<Description> 
##	returns <C>true</C> if the transformation semigroup <C>S</C> is 
##	a zero group or if <C>S</C> was created using the <Ref Oper="ZeroGroup"/> 
##	command. Otherwise <C>false</C> is returned.<P/>
##
##	A semigroup <C>S</C> <C>S</C> is a <E>zero group</E> if there exists an 
##	element <C>0</C> in <C>S</C> such that <C>S</C> without <C>0</C> is a group 
##	and for all <C>x</C> in <C>S</C> we have that <C>x0=0x=0</C>.
##	<Example>
##  gap&gt; S:=ZeroGroup(DihedralGroup(10));;
##  gap&gt; iso:=IsomorphismTransformationSemigroup(S);;
##  gap&gt; T:=Range(iso);;
##  gap&gt; IsZeroGroup(T);
##  true
##	</Example> <!-- properties.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

#DeclareAttribute("IsZeroGroup", IsSemigroup);