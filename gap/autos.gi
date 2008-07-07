##
## autos.gi
## Version 3.1.1
## Mon Jun  9 17:02:20 BST 2008
##

##  <#GAPDoc Label="autostop">
##  This file contains functions for computing automorphisms of transformation
##  semigroups.
##	<#/GAPDoc>
##  
##

#-if there was a way to make a presentation from R-classes then we could use an
# R-class version of the algorithm! (perhaps try this with FroidurePinSimpleAlg)

###########################################################################
##
##  <#GAPDoc Label="InnerAutomorphismsOfSemigroup">
##  <ManSection>
##  <Attr Name="InnerAutomorphismsOfSemigroup" Arg="S"/>
##	<Description>
##
##	<C>InnerAutomorphismsOfSemigroup</C> returns the group of 
##	inner automorphisms of the transformation semigroup <C>S</C>. <P/>
##	
##	The same result can be obtained 
##	by applying <Ref Attr="InnerAutomorphismsAutomorphismGroup"/> to the result 
##	of <Ref Attr="AutomorphismGroup"/> of <C>S</C>. 
##
##	It is possible that the inner 
##	automorphism of <C>S</C> have been calculated at the same time as the entire 
##	automorphism group of <C>S</C> but it might not be.  
##	If the degree of <C>S</C>
##	is high, then this function may take a long time to return a value. <P/>
##
##	The notion of inner automorphisms of semigroups differs from the 
##	notion of the same name for groups. Indeed, if <C>S</C> is a semigroup of 
##	transformations of degree <M>n</M>, then <M>g \in S_n</M> induces an inner 
##	automorphism of <C>S</C> if the mapping <M>s\mapsto g^{-1}sg</M> for all 
##	<M>s \in</M> <C>S</C> is an automorphism of <C>S</C>.
##	<Example>
##  gap&gt; x:=Transformation([2,3,4,5,6,7,8,9,1]);;
##  gap&gt; y:=Transformation([4,2,3,4,5,6,7,8,9]);;
##  gap&gt; S:=Semigroup(x,y);;
##  gap&gt; G:=InnerAutomorphismsOfSemigroup(S);
##  &lt;group of size 54 with 2 generators&gt;
##	</Example> <!-- autos1.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

InstallMethod(InnerAutomorphismsOfSemigroup, "for a transformation semigroup", true, [IsTransformationSemigroup], 0, 
function(S)
local gens, id, G;

return InnerAutomorphismsOfSemigroupInGroup(S, fail, false);

end);

###########################################################################

InstallOtherMethod(InnerAutomorphismsOfSemigroup, "for a zero group", true, [IsZeroGroup], 0, 
function(ZG)
return InnerAutomorphismsAutomorphismGroup(AutomorphismGroup(ZG));
end);

###########################################################################
##
##  <#GAPDoc Label="InnerAutomorphismsAutomorphismGroup">
##  <ManSection> 
##  <Attr Name="InnerAutomorphismsAutomorphismGroup" Arg="autgroup"/>
##	<Description>
##
##	If <C>autgroup</C> satisfies <Ref Prop="IsAutomorphismGroupOfSemigroup"/>, 
##	then, this attribute 
##	stores the subgroup of inner automorphisms of the original semigroup. <P/>
##
##	It is possible that the inner 
##	automorphism of <C>autgroup</C> have been calculated at the same time as 
##	<C>autgroup</C> was calculated but it might not be.  
##	If the degree of underlying semigroup 
##	is high, then this function may take a long time to return a value. <P/>
##
##	The notion of inner automorphisms of semigroups differs from the 
##	notion of the same name for groups. Indeed, if <C>S</C> is a semigroup of 
##	transformations of degree <M>n</M>, then <M>g \in S_n</M> induces an inner 
##	automorphism of <C>S</C> if the mapping <M>s\mapsto g^{-1}sg</M> for all 
##	<M>s \in</M> <C>S</C> is an automorphism of <C>S</C>.<P/>
##
##	If <C>autgroup</C> satisfies <Ref Prop="IsAutomorphismGroupOfZeroGroup"/>, 
##	then <C>InnerAutomorphismsAutomorphismGroup</C> returns the subgroup of 
##	inner automorphisms inside the automorphism 
##	group of the zero group by computing the inner automorphisms of the 
##	underlying group. Note that in this case the notion of inner automorphisms 
##	corresponds to that of the group theoretic notion.
##	<Example>
##  gap&gt; g1:=Transformation([3,3,2,6,2,4,4,6]);;
##  gap&gt; g2:=Transformation([5,1,7,8,7,5,8,1]);;
##  gap&gt; m6:=Semigroup(g1,g2);;
##  gap&gt; A:=AutomorphismGroup(m6);
##  &lt;group of size 12 with 2 generators&gt;
##  gap&gt; InnerAutomorphismsAutomorphismGroup(A);
##  &lt;group of size 12 with 2 generators&gt; 
##  gap&gt; last=InnerAutomorphismsOfSemigroup(m6); 
##	</Example>	<!-- autos2.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

InstallMethod(InnerAutomorphismsAutomorphismGroup, "for the automorphism group of a semigroup", true, [IsAutomorphismGroupOfSemigroup], 31, 
function(G)

return InnerAutomorphismsOfSemigroup(Range(One(G)));
end);

###########################################################################

InstallMethod(InnerAutomorphismsAutomorphismGroup, "for automorphism group of zero group", [IsAutomorphismGroupOfZeroGroup], 30, 
function(autg)
local zg, g, gens, inner;

zg:=Source(Identity(autg));
g:=UnderlyingGroupOfZG(zg);
inner:=InnerAutomorphismsAutomorphismGroup(AutomorphismGroup(g));

if not IsTrivial(inner) then 
	gens:=List(GeneratorsOfGroup(inner), x-> ZeroGroupAutomorphism(zg, x));
else 
	gens:=One(autg);
fi;

g:=Group(gens);
SetNiceMonomorphism(g,  GroupHomomorphismByImagesNC(g, inner, GeneratorsOfGroup(g), List(GeneratorsOfGroup(g), 
function(x) 
if IsOne(x) then 
	return One(inner);
else 
	return x!.grpauto;
fi;
end )));

SetIsHandledByNiceMonomorphism(g, true);
SetNiceObject(g, inner);
UseIsomorphismRelation(inner, g);
SetIsInnerAutomorphismsOfZeroGroup(g, true);

return g;

end);

###########################################################################
##
##  <#GAPDoc Label="InnerAutomorphismsOfSemigroupInGroup">
##  <ManSection>
##  <Oper Name="InnerAutomorphismsOfSemigroupInGroup" Arg="S, G[, bval]"/>
##	<Description>
##	<C>InnerAutomorphismsOfSemigroup</C> returns the group of 
##	inner automorphisms of the transformation semigroup <C>S</C> that also 
##	belong to the group <C>G</C>. The default setting is that the inner 
##	automorphisms of <C>S</C> are calculated first, then filtered to see which 
##	elements also belong to <C>G</C>.<P/>
##
##	If the optional argument <C>bval</C> is present and true, then the filtering 
##	is done online. Otherwise, then this is equivalent to doing 
##	<C>InnerAutomorphismsOfSemigroupInGroup(S, G)</C>. <P/>
##
##	If <Ref InfoClass="InfoAutos"/> is set to level <C>4</C>, then a prompt will 
##	appear during the procedure to let you decide when the filtering should be 
##	done. In this case the value of <C>bval</C> is irrelevant.<P/>
##	<Example>
##  gap&gt; gens:=[ Transformation( [1,8,11,2,5,16,13,14,3,6,15,10,7,4,9,12 ] ), 
##  &gt;   Transformation( [1,16,9,6,5,8,13,12,15,2,3,4,7,10,11,14] ), 
##  &gt;   Transformation( [1,3,7,9,1,15,5,11,13,11,13,3,5,15,7,9 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; InnerAutomorphismsOfSemigroup(S);
##  &lt;group of size 16 with 3 generators&gt;
##  gap&gt; G:=Group(SemigroupHomomorphismByImagesOfGensNC(S, S, gens));
##  &lt;group with 1 generators&gt;
##  gap&gt; InnerAutomorphismsOfSemigroupInGroup(S, G);
##  &lt;group of size 1 with 1 generators&gt;
##  gap&gt; InnerAutomorphismsOfSemigroupInGroup(S, G, true);
##  &lt;group of size 1 with 1 generators&gt;
##  gap&gt; InnerAutomorphismsOfSemigroupInGroup(S, G, false);
##  &lt;group of size 1 with 1 generators&gt;
##	</Example> <!-- autos1.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallOtherMethod(InnerAutomorphismsOfSemigroupInGroup, "for a transformation semigroup and group", true, [IsTransformationSemigroup, IsGroup], 0, 
function(S, G)
return InnerAutomorphismsOfSemigroupInGroup(S, G, false);
end);

###########################################################################

InstallOtherMethod(InnerAutomorphismsOfSemigroupInGroup, "for a semigroup", true, [IsTransformationSemigroup, IsObject, IsBool], 0, 
function(S, superlist, bval)
local constants, m, n, inner, stop, OnSets3, useryn, gens, id, G, H, doit, perms, orb, l, x, image, hom, new, bad, autos;

if not (superlist=fail or IsGroup(superlist)) then 
	Error("<superlist> should be a group or fail");
	return fail;
fi;

#########

	useryn:=function(helptext)
	local keyed, out;
	
	repeat 
	
		keyed:=CHAR_INT(ReadByte(InputTextUser()));

		if keyed = 'y' or keyed = 'Y' then 
			out:=true;
		elif keyed ='n' or keyed = 'N' then 
			out:=false;
		elif keyed ='h' or keyed = 'H' then 
			Info(InfoAutos, 4, helptext);
			out:=fail;
		else
			Info(InfoAutos, 4, "Error: input must be y, n, or h (for help)!");
			out:=fail;
		fi;
	until not out=fail;
	return out;
	end; 
	
#########

constants:=Set(Flat(GradedImagesTransformationMonoid(S)[1]));
n:=DegreeOfTransformationSemigroup(S);
Info(InfoAutos, 2, "InnerAutomorphisms: computing stabiliser of images");

m:=0;

if not Length(constants)=0 and not Length(constants)=n then 
	inner:=Stabilizer(SymmetricGroup(n), AsSet([constants, Difference([1..n], constants)]), OnSetsSets);
else
	inner:=SymmetricGroup(n);
fi;

stop:=false;

repeat 
	m:=m+1;
	if not IsEmpty(GradedImagesTransformationMonoid(S)[m]) then 
		inner:=Stabilizer(inner, Set(GradedImagesTransformationMonoid(S)[m]), OnSetsSets);
		if IsTrivial(inner) then 
			stop:=true;		
		fi;
	fi;
until m=n-1 or stop;

if not IsTrivial(inner) then 
	Info(InfoAutos, 3, "InnerAutomorphisms: stabilizer has size ", Size(inner));
	OnSets3:=function(x, g)
		return Set(List( x, i-> OnSetsSets(i, g)));
	end;
	
	Info(InfoAutos, 2, "InnerAutomorphisms: computing stabiliser of kernels");

	m:=1;
	stop:=false;

	repeat 
		m:=m+1;
		if not IsEmpty(GradedKernelsTransformationMonoid(S)[m]) then 
			inner:=Stabilizer(inner, Set(GradedKernelsTransformationMonoid(S)[m]), OnSets3);
			if IsTrivial(inner) then 
				stop:=true;		
			fi;
		fi;
	until m=n-1 or stop;

	Info(InfoAutos, 3, "InnerAutomorphisms: stabilizer has size ", Size(inner));
fi; #stabilizer of kernels

if IsMonoid(S) then 
		gens:=GeneratorsOfMonoid(S);
else
		gens:=GeneratorsOfSemigroup(S); 
fi;
	
if IsTrivial(inner) then 
	id:=SemigroupHomomorphismByImagesOfGensNC(S, S, gens);
	SetIsOne(id, true);
	inner:=Group(id);
	SetAsSSortedList(inner, [id]);
	SetIsInnerAutomorphismsOfSemigroup(inner, true);
	SetInnerAutomorphismsOfSemigroup(S, inner);
	return inner; 
fi;

Info(InfoAutos, 2, "InnerAutomorphisms: computing the stabilizer of generators under \n#I  the action of the stabilizers of images and kernels");
G:=OrbitStabilizer(inner, AsSet(gens), OnSets); #setwise stabilizer
H:=OrbitStabilizer(G.stabilizer, gens, OnTuples); #pointwise stabilizer

Info(InfoAutos, 2, "InnerAutomorphisms: the inner automorphism search space contains \n#I  ", Length(G.orbit), " elements");

perms:=AsList(RightTransversal(inner, G.stabilizer));
orb:=AsList(RightTransversal(G.stabilizer, H.stabilizer));

if InfoLevel(InfoAutos)=4 and not superlist=fail then 
	Info(InfoAutos, 4, "InnerAutomorphisms: the size of the super group is ", Size(superlist));
	Info(InfoAutos, 4, "######################################################################");
	Info(InfoAutos, 4, "InnerAutomorphisms: filter elements in the super group online? (y/n/h) #");
	Info(InfoAutos, 4, "######################################################################");
	bval:=useryn("Filtering the elements online is only advisable if the size of \n#I  the super group is comparable or less than the size of the inner search \n#I  space.");
fi;

Info(InfoAutos, 1, "InnerAutomorphisms: computing inner automorphisms");

l:=0;

if not bval then 
	inner:=Group(orb);
	SetAsSSortedList(inner, AsSet(orb));
else 
	bad:=Filtered(orb, x-> SemigroupHomomorphismByImagesOfGensNC(S, S, OnTuples(gens, x)) in superlist);
	inner:=Group(bad);
	SetAsSSortedList(inner, AsSet(bad));
fi;

bad:=[];

repeat
	l:=l+1; 
	x:=perms[l];
	image:=OnTuples(gens, x);
	#JDM consider moving this or changing it to InnerAutomorphismOfSemigroup())
	if not image=gens then
		if not x in inner and not x in bad then
			if ForAll(image, x-> x in S) then 
				new:=Semigroup(image);
				if ForAll(gens, g-> g in new) then 
					if (bval and SemigroupHomomorphismByImagesOfGensNC(S, S, image) in superlist) or not bval then
						inner:=ClosureGroupDefault(inner, x);
					fi;
				else
					Info(InfoAutos, 4, "InnerAutomorphisms: images don't generate");
					bad:=Union(bad, Orbit(inner, image, OnTuples)); 
				fi;
			else
				Info(InfoAutos, 4, "InnerAutomorphisms: images not all in semigroup");
			fi;
		fi;
	fi;
	Info(InfoAutos, 4, "InnerAutomorphisms: automorphisms ", Size(inner), " nonautomorphisms ", Length(bad), " counter ", l);
until 2*Size(inner)+Length(bad)>Length(perms) or l=Length(perms);

if not bval and not superlist=fail then 
	Info(InfoAutos, 2, "######################################################################");
	Info(InfoAutos, 2, "AutomorphismGroup: finding the intersection of the superlist and the \n#I  automorphism group"); 
	new:=Group(());
	SetAsSSortedList(new, [()]);
	
	for x in inner do
		if not x in new and InnerAutomorphismOfSemigroupNC(S, x) in superlist then 
			new:=ClosureGroupDefault(new, x);
		fi;
	od;
	inner:=new;
fi;

autos:=Group(List(SmallGeneratingSet(inner), x-> InnerAutomorphismOfSemigroupNC(S, x)));
SetAsSSortedList(autos, SetX(inner, x-> InnerAutomorphismOfSemigroupNC(S, x)));
SetIsInnerAutomorphismsOfSemigroup(autos, true);
#SetInnerAutomorphismsOfSemigroup(S, autos);

return autos;

end);

###########################################################################
##
##  <#GAPDoc Label="AutomorphismGroup">
##  <ManSection>
##  <Attr Name="AutomorphismGroup" Arg="S"/>
##	<Description>
##	<C>AutomorphismGroup</C> returns the group of 
##	automorphisms of the transformation semigroup, zero group, zero semigroup, 
##	Rees matrix semigroup, or Rees 0-matrix semigroup <C>S</C>; that is, 
##	semigroups satisfying the properties <Ref Prop="IsTransformationSemigroup" 
##	BookName="ref"/>, <Ref Prop="IsZeroGroup"/>, <Ref Prop="IsZeroSemigroup"/>,  
##	<Ref Prop="IsReesMatrixSemigroup" BookName="ref"/>, or 
##	<Ref Prop="IsReesZeroMatrixSemigroup" BookName="ref"/>. <P/>
##
##	If <C>S</C> is a transformation semigroup, then <C>AutomorphismGroup</C> 
##	computes the automorphism group of <C>S</C> using the algorithm 
##	described in <Cite Key="computing"/>.  If <C>S</C> is a (completely) simple 
##	transformation semigroup, then the automorphism group is computed by passing 
##	to an isomorphic Rees matrix semigroup. If <C>S</C> is a
##	transformation group, then the automorphism group is computed by passing to 
##	an isomorphic permutation group. If <C>S</C> has order 
##	<M>&lt;10</M>  and knows its Cayley table 
##	(<Ref Prop="MultiplicationTable" BookName="ref"/>), then the automorphism 
##	group is calculated by finding the setwise stabilizer of the Cayley table in
##	the symmetric group of degree <C>|S|</C> under the action ??? <P/>
##
##	If <C>S</C> is a zero group, then <C>AutomorphismGroup</C> computes the 
##	automorphism group of the underlying group. Obviously, every automorphism of 
##	a zero group is the extension of an automorphism of the underlying group 
##	that fixes the zero element. <P/>
##	
##	If <C>S</C> is a zero semigroup, then every permutation of the elements of 
##	<C>S</C> that fixes the zero element is an 
##	automorphism. Thus the automorphism group of a zero semigroup of order 
##	<M>n</M> is isomorphic to the symmetric group on <M>n-1</M> elements. <P/>
##
##	If <C>S</C> is a Rees matrix semigroup or a Rees 0-matrix semigroup, then 
##	the automorphism group of <C>S</C> is calculated using the algorithm 
##	described in <Cite Key="computing" Where="Section 2"/>. In this case, the 
##	returned group has as many generators as elements. This may be changed in 
##	the future.<P/>
##
##	If <Ref InfoClass="InfoAutos"/> is set to level <C>4</C>, then prompts may 
##	appear during the procedure to let you choose how the computation proceeds. 
##	<P/>
##	<Example>
##  gap&gt; g1:=Transformation([5,4,4,2,1]);;
##  gap&gt; g2:=Transformation([2,5,5,4,1]);;
##  gap&gt; m2:=Monoid(g1,g2);;
##  gap&gt; IsTransformationSemigroup(m2);
##  true
##  gap&gt; AutomorphismGroup(m2);
##  &lt;group of size 24 with 5 generators&gt;
##  gap&gt; IsAutomorphismGroupOfSemigroup(last);
##  true
##  gap&gt; zg:=ZeroGroup(CyclicGroup(70));
##  &lt;zero group with 4 generators&gt;
##  gap&gt; IsZeroGroup(zg);
##  true
##  gap&gt; AutomorphismGroup(zg);
##  &lt;group with 3 generators&gt;
##  gap&gt; IsAutomorphismGroupOfZeroGroup(last);
##  true
##  gap&gt; InnerAutomorphismsOfSemigroup(zg);
##  &lt;group of size 1 with 1 generators&gt;
##  gap&gt; InnerAutomorphismsAutomorphismGroup(AutomorphismGroup(zg));
##  &lt;group of size 1 with 1 generators&gt;
##  gap&gt; last2=InnerAutomorphismsAutomorphismGroup(AutomorphismGroup(zg));
##  true
##  gap&gt; S:=ZeroSemigroup(10);
##  &lt;zero semigroup with 10 elements&gt;
##  gap&gt; Size(S);
##  10
##  gap&gt; Elements(S);
##  [ 0, z1, z2, z3, z4, z5, z6, z7, z8, z9 ]
##  gap&gt; A:=AutomorphismGroup(S);
##  &lt;group with 2 generators&gt;
##  gap&gt; IsAutomorphismGroupOfZeroSemigroup(A);
##  true
##  gap&gt; Factorial(9)=Size(A);
##  true
##  gap&gt; G:=Group([ (2,5)(3,4) ]);;
##  gap&gt; mat:=[ [ (), (), (), (), () ], 
##  &gt;   [ (), (), (2,5)(3,4), (2,5)(3,4), () ], 
##  &gt;   [ (), (), (), (2,5)(3,4), (2,5)(3,4) ], 
##  &gt;   [ (), (2,5)(3,4), (), (2,5)(3,4), () ], 
##  &gt;   [ (), (2,5)(3,4), (), (2,5)(3,4), () ] ];;
##  gap&gt; rms:=ReesMatrixSemigroup(G, mat);
##  Rees Matrix Semigroup over Group([ (2,5)(3,4) ])
##  gap&gt; A:=AutomorphismGroup(rms);
##  &lt;group of size 12 with 12 generators&gt;
##  gap&gt; IsAutomorphismGroupOfRMS(A);
##  true
##  gap&gt; G:=ZeroGroup(Group([ (1,3)(2,5), (1,3,2,5) ]));;
##  gap&gt; elts:=Elements(G);;
##  gap&gt; mat:=[ [ elts[7], elts[1], elts[9], elts[1], elts[1] ], 
##  &gt;   [ elts[1], elts[1], elts[1], elts[9], elts[1] ], 
##  &gt;   [ elts[9], elts[1], elts[1], elts[4], elts[9] ], 
##  &gt;   [ elts[1], elts[1], elts[1], elts[1], elts[1] ], 
##  &gt;   [ elts[1], elts[5], elts[1], elts[1], elts[1] ] ];;
##  gap&gt; rzms:=ReesZeroMatrixSemigroup(G, mat);;
##  gap&gt; AutomorphismGroup(rzms);
##  gap&gt; IsAutomorphismGroupOfRZMS(A);
##  true
##  &lt;group of size 512 with 512 generators&gt;
##	</Example> <!-- autos3.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

###########################################################################

InstallOtherMethod(AutomorphismGroup, "for a semigroup", true, [IsTransformationSemigroup], 0, 
function(S)
return AutomorphismsSemigroupInGroup(S, fail, List([1..5], ReturnFalse)); 
end);

###########################################################################

InstallMethod(AutomorphismGroup, "for a zero group", [IsZeroGroup], 
function(zg)
local g, gens, autsg, auts;

g:=UnderlyingGroupOfZG(zg);
autsg:=AutomorphismGroup(g);

if IsTrivial(autsg) then 
	return Group(List(Elements(autsg), x-> ZeroGroupAutomorphism(zg, x)));
else 

	gens:=List(GeneratorsOfGroup(autsg), x-> ZeroGroupAutomorphism(zg, x));

	g:=Group(gens);

	SetIsAutomorphismGroup(g, true);
	SetIsAutomorphismGroupOfSemigroup(g, true);
	SetIsAutomorphismGroupOfZeroGroup(g, true);
	SetIsGroupOfAutomorphisms(g, true);
	SetIsFinite(g, true);
	SetNiceMonomorphism(g,  GroupHomomorphismByImagesNC(g, autsg, GeneratorsOfGroup(g), List(GeneratorsOfGroup(g), x-> x!.grpauto)));
	SetIsHandledByNiceMonomorphism(g, true);
	SetNiceObject(g, autsg);
	UseIsomorphismRelation(autsg, g);

return g;
fi;
end);

###########################################################################

InstallMethod(AutomorphismGroup, "for a RZMS", true, [IsReesZeroMatrixSemigroup], 0,
function(rms)

Print("#I It appears that the `grape' package is not fully installed. As a\n#I consequence this function is not currently available.\n");
return fail;
end);

###########################################################################

InstallOtherMethod(AutomorphismGroup, "for a zero semigroup", true, [IsZeroSemigroup], 0,
function(ZS)
local n, g1, g2, elms, G, hom, sym, iso;

Info(InfoAutos, 2, "AutomorphismGroup: computing the automorphism group of a zero semigroup");

n:=Size(ZS);
if n=2 then 
	hom:=SemigroupHomomorphismByImagesNC(ZS, ZS, Elements(ZS));
	SetIsOne(hom, true);
	G:=Group(SemigroupHomomorphismByImagesNC(ZS, ZS, Elements(ZS)));
	SetAsSSortedList(G, [hom]);
	SetIsAutomorphismGroupOfZeroSemigroup(G, true);

  return G;
else 
  elms:=Elements(ZS);

  g1:=SemigroupHomomorphismByImagesNC(ZS, ZS, Concatenation([elms[1]], elms{[3..n]}, [elms[2]]));
  g2:=SemigroupHomomorphismByImagesNC(ZS, ZS, Concatenation(elms{[1,3,2]}, elms{[4..n]}));

  SetIsInjective(g1, true); SetIsInjective(g2, true);
  SetIsSurjective(g2, true); SetIsSurjective(g1, true);
  SetIsTotal(g1, true); SetIsTotal(g2, true);
	SetInverseGeneralMapping(g1, SemigroupHomomorphismByImagesNC(ZS, ZS, Concatenation([elms[1]], Reversed(elms{[3..n]}), [elms[2]])));
	SetInverseGeneralMapping(g2, g2);
	G:=Group(g1, g2);
	sym:=GroupWithGenerators([PermList(Concatenation([1],[3..n], [2])),(2,3)]);
	
	#iso:= GroupHomomorphismByImagesNC(G, sym, [g1, g2], GeneratorsOfGroup(sym));
	#SetInverseGeneralMapping(iso, GroupHomomorphismByImagesNC(sym, G, GeneratorsOfGroup(sym), [g1, g2]));
	iso:=MappingByFunction(G, sym, function(x)
	#local str;
	#str:=List(x!.imgslist{[2..Length(x!.imgslist)]}, y-> String(y));
	#str:=List(str, y-> Int(y{[2..Length(y)]})); 
	#return PermList(str);
	return PermList(List(x!.imgslist{[2..Length(x!.imgslist)]}, y-> y![1]));
	end);
	SetInverseGeneralMapping(iso, MappingByFunction(sym, G, function(x)
	local perm;
	
	perm:=List([1..LargestMovedPoint(sym)], y->y^x-1);
	perm:=List(perm, ZeroSemigroupElt);
	
	return SemigroupHomomorphismByImagesNC(ZS, ZS, perm); end));
	
	SetIsomorphismPermGroup(G, iso); 
	SetNiceMonomorphism(G, IsomorphismPermGroup(G));
	SetIsHandledByNiceMonomorphism(G, true);
	SetNiceObject(G, Range(NiceMonomorphism(G)));
	UseIsomorphismRelation(G, Range(NiceMonomorphism(G)));
	SetIsAutomorphismGroupOfZeroSemigroup(G, true);

	return G;
fi;

end);

###########################################################################

InstallOtherMethod(AutomorphismGroup, "for a RMS", true, [IsReesMatrixSemigroup], 0,
function(rms)
local G, mat, m, n, autograph, autogroup, A, inner, transversal1, transversal2, g, h, l, t, tup, new, elts, id, hom;

G:=UnderlyingSemigroupOfReesMatrixSemigroup(rms);

mat:=SandwichMatrixOfReesMatrixSemigroup(rms);
m:=Length(mat[1]); n:=Length(mat);

if n=1 and m=1 then

	#rms is just a group in disguise!
	Info(InfoAutos, 2, "AutomorphismGroup: ReesMatrixSemigroup is a group");
	
	Info(InfoAutos, 2, "AutomorphismGroup: computing automorphism group of underlying group");

	autogroup:=AutomorphismGroup(G);
	A:=Group(List(GeneratorsOfGroup(autogroup), x-> RMSIsoByTriple(rms, rms, [(), x, [One(G), One(G)]])));

	SetIsAutomorphismGroup(A, true);
	SetIsGroupOfAutomorphisms(A, true);
	SetIsFinite(A, true);
	hom:=GroupHomomorphismByImagesNC(A, autogroup, GeneratorsOfGroup(A), GeneratorsOfGroup(autogroup));
	SetInverseGeneralMapping(hom, GroupHomomorphismByImagesNC(autogroup, A, GeneratorsOfGroup(autogroup), GeneratorsOfGroup(A)));
	SetNiceMonomorphism(A, hom);
	SetIsHandledByNiceMonomorphism(A, true);
	SetNiceObject(A, Range(NiceMonomorphism(A)));
	UseIsomorphismRelation(A, autogroup);
	
	return A;

elif n=2 and m=1 then 
	autograph:=Group((2,3));
elif n>2 and m=1 then 
	autograph:=Group((2,3), PermList(Concatenation([1],[3..n+m],[2]))); 
else 
	autograph:=DirectProduct(SymmetricGroup(m), SymmetricGroup(n));
fi;

Info(InfoAutos, 2, "AutomorphismGroup: the automorphism group of the graph has size ", Size(autograph));

Info(InfoAutos, 2, "AutomorphismGroup: computing automorphism group of underlying group");
autogroup:=AutomorphismGroup(G);
Info(InfoAutos, 2, "AutomorphismGroup: the automorphism group of underlying group has size ", Size(autogroup));

Info(InfoAutos, 3, "AutomorphismGroup: computing inner automorphisms of underlying group");
if not IsTrivial(autogroup) then 
	inner:=InnerAutomorphismsAutomorphismGroup(autogroup);
	transversal1:=RightTransversal(autogroup, inner);
else
	transversal1:=Elements(autogroup);
fi;

Info(InfoAutos, 2, "AutomorphismGroup: Aut G/Inn G has size ", Length(transversal1));

transversal2:=RightTransversal(G, Centre(G));
Info(InfoAutos, 2, "AutomorphismGroup: G/Z(G) has size ", Length(transversal2));

Info(InfoAutos, 2, "AutomorphismGroup: search space has ", Size(autograph), "x", Length(transversal1), "x", Length(transversal2), "=", Size(autograph)*Length(transversal1)*Length(transversal2), " elements");

#JDM with the commented out lines rather than the current version the algorithm
#JDM runs about half the speed. 
#id:=RMSIsoByTriple(rms, rms, [One(autograph), One(autogroup), List([1..m+n], x-> One(G))]);
#A:=Group(id);
#SetAsSSortedList(A, [id]);
#elts:=[];
A:=[];

for l in autograph do 

	for t in transversal1 do 

		for tup in transversal2 do
				#if Size(A)<Size(autograph)*Length(transversal1)*Length(transversal2) then 
					new:=RMSInducedFunction(rms, l, t, tup); 	
					if new[1]=true then
						new:=RMSIsoByTriple(rms, rms, [l, t, new[2]]);
						#if not new in A then 
							#AddSet(A, new); 
							#A:=ClosureGroupDefault(A, new);
							Add(A, new);
						#fi;
					fi;
				#fi;
		od; 
	od;
od;
			
A:=Group(A);
SetAsList(A, GeneratorsOfGroup(A));
SetIsAutomorphismGroup(A, true);
SetIsGroupOfAutomorphisms(A, true);
SetIsAutomorphismGroupOfRMS(A, true);
SetIsFinite(A, true);

#JDM maybe there should be a RectangularBand automorphism group command...
if IsTrivial(UnderlyingSemigroupOfReesMatrixSemigroup(rms)) then 
	SetIsomorphismPermGroup(A, GroupHomomorphismByImagesNC(A, autograph, GeneratorsOfGroup(A), List(GeneratorsOfGroup(A), x-> x!.triple[1])));
	SetInverseGeneralMapping(IsomorphismPermGroup(A), GroupHomomorphismByImagesNC(autograph, A, GeneratorsOfGroup(Range(IsomorphismPermGroup(A))), List(GeneratorsOfGroup(Range(IsomorphismPermGroup(A))), x-> RMSIsoByTriple(rms, rms, [x, One(autogroup), List([1..m+n], x-> One(G))]))));
	
	SetNiceMonomorphism(A, IsomorphismPermGroup(A));
	SetIsHandledByNiceMonomorphism(A, true);
	UseIsomorphismRelation(A, Range(NiceMonomorphism(A)));
fi;

return A;

end);

###########################################################################
##
##  <#GAPDoc Label="AutomorphismsSemigroupInGroup">
##  <ManSection>
##  <Oper Name="AutomorphismsSemigroupInGroup" Arg="S, G[, bvals]"/>
##	<Description>
##	<C>AutomorphismsSemigroupInGroup</C> returns the group of 
##	automorphisms of the transformation semigroup <C>S</C> that also 
##	belong to the group <C>G</C>. If <C>G=fail</C>, then 
##	<C>AutomorphismsSemigroupInGroup</C> returns the same value as
##	<Ref Attr="AutomorphismGroup"/>. The default setting is that the  
##	automorphisms of <C>S</C> are calculated first, then filtered to see which 
##	elements also belong to <C>G</C>.<P/>
##
##	The optional argument  <C>bvals</C> is a list of 
##	<M>5</M> Boolean variables that correspond to the following options:
##	<List>
##		<Item>if <C>bvals[1]</C> is <C>true</C>, then &GAP; will run a cheap check 
##		to see if all the automorphisms are inner. Note that this can return 
##		<C>false</C> when all the automorphisms are inner, that is the condition 
##		is sufficient but not necessary. The default setting is <C>false</C>.
##		</Item>
##		<Item>if <C>bvals[2]</C> is <C>true</C>, then &GAP; will try to compute 
##		the inner automorphisms of <C>S</C> before computing the entire
##		automorphism group. For semigroups of large degree this may not be 
##		sensible. The default setting is <C>false</C>.</Item>
##		<Item>if <C>bvals[3]</C> is <C>true</C>, then &GAP; will test elements in
##		the inner automorphism search space to see if they are in <C>G</C> online.  
##		The default setting is <C>false</C>.</Item>
##		<Item>if <C>bvals[4]</C> is <C>true</C>, then &GAP; will test elements in
##		the outer (i.e. not inner) automorphism search space to see if they are in 
##		<C>G</C> online.  The default setting is <C>false</C>.</Item>
##		<Item>if <C>bvals[5]</C> is <C>true</C>, then &GAP; will keep track of 
##		non-automorphisms in the search for outer automorphisms.  The default 
##		setting is <C>false</C>.</Item>
##	</List>
##
##	<Example>
##  gap&gt; g1:=Transformation([5,4,4,2,1]);;
##  gap&gt; g2:=Transformation([2,5,5,4,1]);;
##  gap&gt; m2:=Monoid(g1,g2);;
##  gap&gt; A:=AutomorphismsSemigroupInGroup(m2, fail, [false, true, true, false, true]);
##  &lt;group of size 24 with 3 generators&gt;
##  gap&gt; g1:=Transformation([3,3,2,6,2,4,4,6,3,4,6]);;
##  gap&gt; g2:=Transformation([4,4,6,1,3,3,3,3,11,11,11]);;
##  gap&gt; m7:=Monoid(g1,g2);;
##  gap&gt; A:=AutomorphismsSemigroupInGroup(m7, fail, [false, true, false, false, true]);
##  &lt;group of size 2 with 2 generators&gt;
##  gap&gt; imgs:=[ [ Transformation( [ 1, 1, 5, 4, 3, 6, 7, 8, 9, 10, 11, 12 ] ), 
##  &gt;       Transformation( [ 1, 1, 5, 7, 4, 3, 6, 8, 9, 10, 11, 12 ] ), 
##  &gt;       Transformation( [ 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 8 ] ) ], 
##  &gt;   [ Transformation( [ 1, 1, 5, 4, 3, 6, 7, 8, 9, 10, 11, 12 ] ), 
##  &gt;       Transformation( [ 1, 1, 5, 3, 7, 4, 6, 8, 9, 10, 11, 12 ] ), 
##  &gt;       Transformation( [ 1, 2, 3, 4, 5, 6, 7, 11, 12, 8, 9, 10 ] ) ] ];;
##  gap&gt; gens:=List(imgs, x-&gt; SemigroupHomomorphismByImagesOfGensNC(S, S, x));;
##  gap&gt; G:=Group(gens);
##  &lt;group with 2 generators&gt;
##  gap&gt; A:=AutomorphismsSemigroupInGroup(S, G, [false, false, false, true, false]);
##  &lt;group of size 48 with 4 generators&gt;
##  gap&gt; Size(G);
##  48
##  gap&gt; A:=AutomorphismsSemigroupInGroup(S, G);
##  &lt;group of size 48 with 4 generators&gt;
##  gap&gt; gens:=[ Transformation( [ 1, 1, 4, 3, 5, 6, 7, 8, 9, 10, 11, 12 ] ), 
##  &gt;   Transformation( [ 1, 1, 4, 5, 6, 7, 3, 8, 9, 10, 11, 12 ] ), 
##  &gt;   Transformation( [ 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 8 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; A:=AutomorphismsSemigroupInGroup(S, G);
##  &lt;group of size 48 with 4 generators&gt;
##  gap&gt; HasAutomorphismGroup(S);
##  true
##  gap&gt; AutomorphismGroup(S);
##  &lt;group of size 480 with 7 generators&gt;
##	</Example> <!-- autos2.tst -->
##	</Description>
##  </ManSection>
##	<#/GAPDoc>

InstallOtherMethod(AutomorphismsSemigroupInGroup, "for a semigroup", true, [IsTransformationSemigroup, IsGroup], 0, 
function(S, G)
return AutomorphismsSemigroupInGroup(S, G, [false, false, false, false, true]); 
end);


###########################################################################

InstallMethod(AutomorphismsSemigroupInGroup, "for a semigroup", true, [IsTransformationSemigroup, IsObject, IsList], 0,
function(S, superlist, bvals)

Print("#I It appears that the `grape' package is not fully installed. As a\n#I consequence this function is not currently available.\n");
return fail;

end);

#############################################################################
##
##  <#GAPDoc Label="RightTransStabAutoGroup">
##  <ManSection>
##	<Oper Name="RightTransStabAutoGroup" Arg="S, elts, func"/> 
##	<Description>
##
##	returns a right transversal of the stabilizer w.r.t the action <C>func</C> 
##	of the elements 
##	<C>elts</C> in the automorphism group of the zero semigroup, Rees matrix
##	semigroup, or Rees 0-matrix semigroup <C>S</C>. That is, <C>S</C> 
##	satisfying <Ref Prop="IsZeroSemigroup"/>,  
##	<Ref Prop="IsReesMatrixSemigroup" BookName="ref"/>, or 
##	<Ref Prop="IsReesZeroMatrixSemigroup" BookName="ref"/>.
##	<Example>
##  gap&gt; S:=ZeroSemigroup(6);
##  &lt;zero semigroup with 6 elements&gt;
##  gap&gt; elts:=Elements(S);
##  [ 0, z1, z2, z3, z4, z5 ]
##  gap&gt; Length(RightTransStabAutoGroup(S, [elts[1]], OnSets));
##  1
##  gap&gt; Length(RightTransStabAutoGroup(S, [elts[1], elts[2]], OnSets));
##  5
##  gap&gt; Length(RightTransStabAutoGroup(S, [elts[1], elts[2]], OnTuples));
##  5
##  gap&gt; G:=Group([ (1,2) ]);;
##  gap&gt; mat:=[ [ (), (), () ], [ (), (1,2), () ], [ (), (1,2), (1,2) ], 
##  &gt;    [ (), (), () ], [ (), (1,2), () ] ];;
##  gap&gt; rms:=ReesMatrixSemigroup(G, mat);;
##  gap&gt; Size(rms);
##  30
##  gap&gt; GeneratorsOfSemigroup(rms);
##  [ (1,(),2), (1,(),3), (1,(),4), (1,(),5), (2,(),1), (3,(),1), (1,(1,2),1) ]
##  gap&gt; Length(RightTransStabAutoGroup(rms, last, OnSets));
##  4
##  gap&gt; Length(RightTransStabAutoGroup(rms, GeneratorsOfSemigroup(rms), OnTuples));
##  8
##  gap&gt; G:=ZeroGroup(Group([ (1,3) ]));;
##  gap&gt; z:=MultiplicativeZero(G);; x:=Elements(G)[2];;
##  gap&gt; mat:=[ [ z, z, z ], [ z, z, z ], [ z, z, z ], [ z, z, z ], [ z, x, z ] ];;
##  gap&gt; rzms:=ReesZeroMatrixSemigroup(G, mat);
##  gap&gt; Size(rzms);
##  31
##  gap&gt; Size(GeneratorsOfSemigroup(rzms));
##  6
##  gap&gt; Length(RightTransStabAutoGroup(rzms, GeneratorsOfSemigroup(rzms), OnSets));
##  512
##  gap&gt; A:=AutomorphismGroup(rzms);
##  &lt;group of size 3072 with 3072 generators&gt;
##  </Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallOtherMethod(RightTransStabAutoGroup, "for a auto. gp. zero semigroup", true, [IsZeroSemigroup, IsMultiplicativeElementCollection, IsFunction], 0, 
function(S, elts, func)
local imgs, iso, stab;
#JDM some error handling here.

#imgs:=List(elts, x-> String(x));
#imgs:=List(imgs, x-> Int(x{[2..Length(x)]})+1);
imgs:=List(elts, x->x![1]+1);

iso:=IsomorphismPermGroup(AutomorphismGroup(S));
stab:=Stabilizer(Range(iso), imgs, func);

#JDM could use enumerator here instead of AsList.
return OnTuples(AsList(RightTransversal(Range(iso), stab)), InverseGeneralMapping(iso));

end);

###########################################################################

InstallOtherMethod(RightTransStabAutoGroup, "for a Rees matrix semigroup", true, [IsReesMatrixSemigroup, IsReesMatrixSemigroupElementCollection, IsFunction], 0,
function(rms, elts, func)

local G, mat, m, n, autograph, autogroup, inner, transversal1, transversal2, g, h, l, t, new, rmsgens, tup, trans, tuples, A, indices;

if not ForAll(elts, x-> x in rms) then 
	Error("<elts> must all lie in <rms>");
	return fail;
fi; #JDM make NC version? to omit this?

G:=UnderlyingSemigroupOfReesMatrixSemigroup(rms);

mat:=SandwichMatrixOfReesMatrixSemigroup(rms);
m:=Length(mat[1]); n:=Length(mat);

if n=1 and m=1 then
	
	#rms is just a group in disguise!
	Info(InfoAutos, 2, "AutomorphismGroupRMS: ReesMatrixSemigroup is a group");
	
	Info(InfoAutos, 2, "AutomorphismGroupRMS: computing automorphism group of underlying group");
	G:=IsomorphismPermGroup(rms);
	autogroup:=RightTransversal(AutomorphismGroup(Range(G)), Stabilizer(AutomorphismGroup(Range(G)), OnTuples(elts, G), func));
	
	A:=List(autogroup, x-> RMSIsoByTriple(rms, rms, [(), x, [One(G), One(G)]]));

	return A;

elif n=2 and m=1 then 
	autograph:=Group((2,3));
elif n>2 and m=1 then 
	autograph:=Group((2,3), PermList(Concatenation([1],[3..n+m],[2]))); 
else 
	autograph:=DirectProduct(SymmetricGroup(m), SymmetricGroup(n));
fi;

Info(InfoAutos, 2, "AutomorphismGroupRMS: the automorphism group of the graph has size ", Size(autograph));

Info(InfoAutos, 2, "AutomorphismGroupRMS: computing automorphism group of underlying group");
autogroup:=AutomorphismGroup(G);
Info(InfoAutos, 2, "AutomorphismGroupRMS: the automorphism group of underlying group has size ", Size(autogroup));

Info(InfoAutos, 3, "AutomorphismGroupRMS: computing inner automorphisms of underlying group");
if not IsTrivial(autogroup) then 
	inner:=InnerAutomorphismsAutomorphismGroup(autogroup);
	transversal1:=RightTransversal(autogroup, inner);
else
	transversal1:=Elements(autogroup);
fi;

Info(InfoAutos, 2, "AutomorphismGroupRMS: Aut G/Inn G has size ", Length(transversal1));

transversal2:=RightTransversal(G, Centre(G));
Info(InfoAutos, 2, "AutomorphismGroupRMS: G/Z(G) has size ", Length(transversal2));

Info(InfoAutos, 2, "AutomorphismGroupRMS: search space has ", Size(autograph), "x", Length(transversal1), "x", Length(transversal2), "=", Size(autograph)*Length(transversal1)*Length(transversal2), " elements");

trans:=[];
tuples:=[];
#rmsgens:=OnTuples(gens, iso);
#indices:=List(gens, IndexPeriodOfTransformation);#JDM does this help much?

for l in autograph do 

	for t in transversal1 do 

		for tup in transversal2 do
				new:=RMSInducedFunction(rms, l, t, tup); 	
				if new[1]=true then
					new:=RMSIsoByTriple(rms, rms, [l, t, new[2]]);
					tup:=func(elts, new);
					if not tup in tuples then
						AddSet(tuples, tup); 
						Add(trans, new);
					fi;
			fi;
		od;
	od;
od;

return trans;
#JDM could also return <tuples> if desirable

end);

#############################################################################
##	JDM for the example given in the manual example this appears to run more 
##	slowly than just running automorphism group (although the overall 
##	computation is longer)

InstallOtherMethod(RightTransStabAutoGroup, "for a RZMS", true, [IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroupElementCollection, IsFunction], 0,
function(rms, elts, func)

Print("#I It appears that the `grape' package is not fully installed. As a\n#I consequence this function is not currently available.\n");
return fail;

end);

#############################################################################
##
##  <#GAPDoc Label="RMSIsoByTriple">
##  <ManSection>
##  <Func Name="RMSIsoByTriple" Arg="rms1, rms2, triple"/>
##	<Description>
##	function to create an isomorphism between the Rees matrix semigroups 
##	<C>rms1</C> 
##	and <C>rms2</C> defined by <C>triple</C>. The first component of 
##	<C>triple</C> should be an isomorphism from the underlying group of 
##	<C>rms1</C> to the underlying group of <C>rms2</C>, the second component 
##	should be an isomorphism from the graph associated to the matrix of 
##	<C>rms1</C> to the graph associated with the matrix of <C>rms2</C>, and the 
##	third component should be a function (given as a list of image elements) 
##	from the index sets of <C>rms1</C> to the underlying group of <C>rms2</C>; 
##	see <Cite Key="computing" Where="Section 2"/> for further details. <P/>
##
##	Note that this function only creates an object with representation 
##	<Ref Filt="IsRMSIsoByTripleRep"/> and does not check that <C>triple</C> 
##	actually 
##	defines an isomorphism from <C>rms1</C> to <C>rms2</C> or that the arguments 
##	even make sense. To create an 
##	isomorphism from <C>rms1</C> to <C>rms2</C> use 
##	<Ref Oper="IsomorphismSemigroups"/>.  
##  <Example>
##  gap&gt; G:=Group((1,4,3,5,2));;
##  gap&gt; mat:=[ [ (), (), () ], [ (), (1,4,3,5,2), () ], [ (), (1,3,2,4,5), () ] ];;
##  gap&gt; rms:=ReesMatrixSemigroup(G, mat);;
##  gap&gt; l:=(4,6);;
##  gap&gt; g:=GroupHomomorphismByImages(G, G, [(1,4,3,5,2)], [(1,2,5,3,4)]);
##  [ (1,4,3,5,2) ] -> [ (1,2,5,3,4) ]
##  gap&gt; map:=[(), (1,5,4,2,3), (), (), (), () ];;
##  gap&gt; RMSIsoByTriple(rms, rms, [l, g, map]);
##  [ (4,6), GroupHomomorphismByImages( Group( [ (1,4,3,5,2) ] ), Group( 
##    [ (1,4,3,5,2) ] ), [ (1,4,3,5,2) ], [ (1,2,5,3,4) ] ), 
##    [ (), (1,5,4,2,3), (), (), (), () ] ]
##  gap&gt; IsRMSIsoByTripleRep(last);
##  true
##  gap&gt; #the previous actually defines an automorphism of rms
##  gap&gt; #on the other hand, the next example is nonsense but no error
##  gap&gt; #is given
##  gap&gt; RMSIsoByTriple(rms, rms, [l, g, [()]]);
##  [ (4,6), GroupHomomorphismByImages( Group( [ (1,4,3,5,2) ] ), Group( 
##    [ (1,4,3,5,2) ] ), [ (1,4,3,5,2) ], [ (1,2,5,3,4) ] ), [ () ] ]
##  </Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallGlobalFunction(RMSIsoByTriple, 
function(rms1, rms2, triple)
  local fam1, fam2, mapfam, map;

  fam1 :=  ElementsFamily(FamilyObj(rms1));
  fam2 :=  ElementsFamily(FamilyObj(rms2));
  mapfam := GeneralMappingsFamily(fam1,fam2);	
  map := rec( triple := triple);
  Objectify(NewType(mapfam, IsGeneralMapping and 
IsSPGeneralMapping and IsTotal and IsSingleValued and IsInjective and IsSurjective and RespectsMultiplication and IsRMSIsoByTripleRep), map);
  SetSource(map, rms1);
  SetRange(map, rms2);
  IsOne(map);		
  
  return map;
end);

###########################################################################
##
##	The functions below are the usual functions for dealing with homomorphisms
##	in IsRMSIsoByTripleRep
##

InstallMethod( ViewObj, "for object in `IsRMSIsoByTriple'",
   [IsGeneralMapping and IsRMSIsoByTripleRep], 
   function( obj )
      Print( obj!.triple );
end );

#############

InstallMethod( PrintObj, "for object in `IsRMSIsoByTriple'",
   [IsGeneralMapping and IsRMSIsoByTripleRep], 
   function( obj )
      Print( "RMSIsoByTriple ( ",Source(obj),",", obj!.triple, " )" );
end );

#############

InstallMethod(ImagesRepresentative, "for an RMS element under a mapping by a triple",
      FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
	IsGeneralMapping and IsRMSIsoByTripleRep, IsReesMatrixSemigroupElement ],
      function( triple, x)
	local g, i, j, lambda, gamma, f, m;

     m:=RowsOfReesMatrixSemigroup(Source(triple));

     g := UnderlyingElementOfReesMatrixSemigroupElement(x);
     i := RowIndexOfReesMatrixSemigroupElement(x);
     j := ColumnIndexOfReesMatrixSemigroupElement(x)+m;
     lambda := triple!.triple[1];
     gamma := triple!.triple[2]; 
     f := triple!.triple[3];

    return ReesMatrixSemigroupElementNC(Range(triple), i^lambda,
 	f[i]*ImageElm(gamma,g)/f[j], j^lambda-m);
end);

#############

InstallMethod(PreImagesRepresentative,  "for an RMS element under a mapping by a triple", 
FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
	IsGeneralMapping and IsRMSIsoByTripleRep, IsReesMatrixSemigroupElement ],
function(triple, x)

return ImagesRepresentative(triple^-1, x);

end);

#############

InstallMethod(ImagesElm, "for an RMS element under a mapping by a triple",
      FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
	IsGeneralMapping and IsRMSIsoByTripleRep, IsReesMatrixSemigroupElement ],
      function( triple, x)
	
      return [ImagesRepresentative(triple, x)];
end);

#############

InstallMethod(CompositionMapping2, "for objects in `IsRMSIsoByTriple'", IsIdenticalObj, [IsGeneralMapping and IsRMSIsoByTripleRep, IsGeneralMapping and IsRMSIsoByTripleRep], 0,
function(a1, a2)
local n, l1, l2, g1, g2, f1, f2;

n:=RowsOfReesMatrixSemigroup(Source(a1))
   +ColumnsOfReesMatrixSemigroup(Source(a1));

l1:=a1!.triple[1]; l2:=a2!.triple[1];
g1:=a1!.triple[2]; g2:=a2!.triple[2];
f1:=a1!.triple[3]; f2:=a2!.triple[3];

return RMSIsoByTriple(Source(a1), Range(a2), [l1*l2, g1*g2, List([1..n], x->f2[x^l1]*f1[x]^g2)]);
end);

#############

InstallMethod(InverseGeneralMapping, "for objects in `IsRMSIsoByTriple'", true, [IsEndoGeneralMapping and IsRMSIsoByTripleRep],  0,
function(a)
local l, g, f, n; 

n:=RowsOfReesMatrixSemigroup(Source(a))
   +ColumnsOfReesMatrixSemigroup(Source(a));

l:=a!.triple[1]; g:=a!.triple[2]; f:=a!.triple[3];

return RMSIsoByTriple(Range(a), Source(a), [l^-1, g^-1, List([1..n],x-> 
(f[x^l]^(g^-1))^-1)]);
end);

#############

InstallMethod(InverseGeneralMapping, "for objects in `IsRMSIsoByTriple'", true, [IsEndoGeneralMapping and IsRMSIsoByTripleRep and IsOne],  0, x-> x);

#############

InstallMethod(\=, "for objects in `IsRMSIsoByTriple'", IsIdenticalObj, [IsEndoGeneralMapping and IsRMSIsoByTripleRep, IsEndoGeneralMapping and IsRMSIsoByTripleRep],  0,
function(triple1, triple2)

if triple1!.triple[1]=triple2!.triple[1] and triple1!.triple[2]=triple2!.triple[2] and triple1!.triple[3]=triple2!.triple[3] then 

	return true;

else 

	return OnTuples(GeneratorsOfSemigroup(Source(triple1)), triple1)=OnTuples(GeneratorsOfSemigroup(Source(triple1)), triple2);
fi; 

end);

#############

InstallMethod(\<, "for objects in `IsRMSIsoByTriple'", IsIdenticalObj,
[IsGeneralMapping and IsRMSIsoByTripleRep, IsGeneralMapping and 
 IsRMSIsoByTripleRep],  0,
function(triple1, triple2)

return (triple1!.triple[1]<triple2!.triple[1]) or
	(triple1!.triple[1]=triple2!.triple[1] and triple1!.triple[2]<triple2!.triple[2]) or (triple1!.triple[1]=triple2!.triple[1] and triple1!.triple[2]=triple2!.triple[2] and triple1!.triple[3]<triple2!.triple[3]);

end);

#############

InstallMethod(IsOne, "for objects in `IsRMSIsoByTriple'", true, [IsEndoGeneralMapping and IsRMSIsoByTripleRep], 0, 
function(triple)

return IsOne(triple!.triple[1]) and IsOne(triple!.triple[2]) and ForAll(triple!.triple[3], IsOne);
end);


#############################################################################
##
##  <#GAPDoc Label="RZMSIsoByTriple">
##  <ManSection>
##  <Func Name="RZMSIsoByTriple" Arg="rzms1, rzms2, triple"/>
##	<Description>
##	function to create an isomorphism between the Rees 0-matrix semigroups 
##	<C>rzms1</C> 
##	and <C>rzms2</C> defined by <C>triple</C>. The first component of 
##	<C>triple</C> should be an isomorphism from the underlying zero group of 
##	<C>rzms1</C> to the underlying zero group of <C>rzms2</C>, the second 
##	component 
##	should be an isomorphism from the graph associated to the matrix of 
##	<C>rzms1</C> to the graph associated with the matrix of <C>rzms2</C>, and 
##	the third component should be a function (given as a list of image elements) 
##	from the index sets of <C>rzms1</C> to the underlying zero group of 
##	<C>rzms2</C>; see <Cite Key="computing" Where="Section 2"/> for further 
##	details. <P/>
##
##	Note that this function only creates an object with representation 
##	<Ref Filt="IsRZMSIsoByTripleRep"/> and does not check that <C>triple</C> 
##	actually 
##	defines an isomorphism from <C>rzms1</C> to <C>rzms2</C> or that the 
##	arguments even make sense. To create an 
##	isomorphism from <C>rzms1</C> to <C>rzms2</C> use 
##	<Ref Oper="IsomorphismSemigroups"/>.  
##  <Example>
##  gap&gt; G:=Group((1,4,3,5,2));;
##  gap&gt; ZG:=ZeroGroup(G);
##  &lt;zero group with 2 generators&gt;
##  gap&gt; mat:=[ [ (), (), () ], [ (), (1,4,3,5,2), () ], [ (), (1,3,2,4,5), () ] ];;
##  gap&gt; mat:=List(mat, x-&gt; List(x, ZeroGroupElt));
##  [ [ (), (), () ], [ (), (1,4,3,5,2), () ], [ (), (1,3,2,4,5), () ] ]
##  gap&gt; rms:=ReesZeroMatrixSemigroup(ZG, mat);
##  Rees Zero Matrix Semigroup over &lt;zero group with 2 generators&gt;
##  gap&gt; l:=(4,6);;
##  gap&gt; g:=GroupHomomorphismByImages(G, G, [(1,4,3,5,2)], [(1,2,5,3,4)]);
##  [ (1,4,3,5,2) ] -&gt; [ (1,2,5,3,4) ]
##  gap&gt; g:=ZeroGroupAutomorphism(ZG, g);
##  &lt;mapping: &lt;zero group with 2 generators&gt; -&gt; &lt;zero group with 2 generators&gt; &gt;
##  gap&gt;  map:=List([(), (1,5,4,2,3), (), (), (), () ], ZeroGroupElt);;
##  gap&gt; RZMSIsoByTriple(rms, rms, [l, g, map]);
##  [ (4,6), &lt;mapping: &lt;zero group with 2 generators&gt; -&gt; &lt;zero group with 
##    2 generators&gt; &gt;, 
##  [ ZeroGroup(()), ZeroGroup((1,5,4,2,3)), ZeroGroup(()), ZeroGroup(()), 
##      ZeroGroup(()), ZeroGroup(()) ] ]
##  gap&gt; RZMSIsoByTriple(rms, rms, [l, g, [()]]);
##  [ (4,6), &lt;mapping: &lt;zero group with 2 generators&gt; -&gt; &lt;zero group with 
##    2 generators&gt; &gt;, [ () ] ]
##  gap&gt; IsRZMSIsoByTripleRep(last);
##  true    
##  </Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>


InstallGlobalFunction(RZMSIsoByTriple, 
function(rms1, rms2, triple)
  local fam1, fam2, mapfam, map;

  fam1 :=  ElementsFamily(FamilyObj(rms1));
  fam2 :=  ElementsFamily(FamilyObj(rms2));
  mapfam := GeneralMappingsFamily(fam1,fam2);	
  map := rec( triple := triple);
  Objectify(NewType(mapfam, IsGeneralMapping and 
IsSPGeneralMapping and IsTotal and IsSingleValued and IsInjective and IsSurjective and RespectsMultiplication and IsRZMSIsoByTripleRep), map);
  SetSource(map, rms1);
  SetRange(map, rms2);
  
  return map;
end);

###########################################################################
##
##	The functions below are the usual functions for dealing with homomorphisms
##	in IsRZMSIsoByTripleRep
##

InstallMethod( ViewObj, "for object in `IsRZMSIsoByTriple'",
   [IsGeneralMapping and IsRZMSIsoByTripleRep],   
   function( obj )
      Print( obj!.triple );
end );

#############

InstallMethod( PrintObj, "for object in `IsRZMSIsoByTriple'",
   [IsGeneralMapping and IsRZMSIsoByTripleRep],
   function( obj )
      Print( "RZMSIsoByTriple ( ",Source(obj),",", obj!.triple, " )" );
end );

#############

InstallMethod(ImagesRepresentative, "for an RZMS element under a mapping by a triple",
      FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
	IsGeneralMapping and IsRZMSIsoByTripleRep, IsReesZeroMatrixSemigroupElement ],
      function( triple, x)
	local g, i, j, lambda, gamma, f, m;

if not x=MultiplicativeZero(Source(triple)) then
     m:=RowsOfReesZeroMatrixSemigroup(Source(triple));

     g := UnderlyingElementOfReesZeroMatrixSemigroupElement(x);
     i := RowIndexOfReesZeroMatrixSemigroupElement(x);
     j := ColumnIndexOfReesZeroMatrixSemigroupElement(x)+m;
     lambda := triple!.triple[1];
     gamma := triple!.triple[2]; 
     f := triple!.triple[3];

    return ReesZeroMatrixSemigroupElementNC(Range(triple), i^lambda,
 	f[i]*ImageElm(gamma,g)/f[j], j^lambda-m);

    else 

      return x;
fi;

end);

#############

InstallMethod(PreImagesRepresentative,  "for an RZMS element under a mapping by a triple", 
FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
	IsGeneralMapping and IsRZMSIsoByTripleRep, IsReesZeroMatrixSemigroupElement ],
function(triple, x)

return ImagesRepresentative(triple^-1, x);

end);

#############

InstallMethod(ImagesElm, "for an RZMS element under a mapping by a triple",
      FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
	IsGeneralMapping and IsRZMSIsoByTripleRep, IsReesZeroMatrixSemigroupElement ],
      function( triple, x)
	
      return [ImagesRepresentative(triple, x)];
end);

#############

InstallMethod(CompositionMapping2, "for objects in `IsRZMSIsoByTriple'", IsIdenticalObj, [IsGeneralMapping and IsRZMSIsoByTripleRep, IsGeneralMapping and IsRZMSIsoByTripleRep], 0,
function(a1, a2)
local n, l1, l2, g1, g2, f1, f2;

n:=RowsOfReesZeroMatrixSemigroup(Source(a1))
   +ColumnsOfReesZeroMatrixSemigroup(Source(a1));

l1:=a1!.triple[1]; l2:=a2!.triple[1];
g1:=a1!.triple[2]; g2:=a2!.triple[2];
f1:=a1!.triple[3]; f2:=a2!.triple[3];

return RZMSIsoByTriple(Source(a1), Range(a2), [l1*l2, g1*g2, 
List([1..n], x->f2[x^l1]*f1[x]^g2)]);
end);

#############

InstallMethod(InverseGeneralMapping, "for objects in `IsRZMSIsoByTriple'", true, [IsGeneralMapping and IsRZMSIsoByTripleRep],  0,
function(a)
local l, g, f, n; 

n:=RowsOfReesZeroMatrixSemigroup(Source(a))
   +ColumnsOfReesZeroMatrixSemigroup(Source(a));

l:=a!.triple[1]; g:=a!.triple[2]; f:=a!.triple[3];

return RZMSIsoByTriple(Range(a), Source(a), [l^-1, g^-1, List([1..n],x-> 
(f[x^l]^(g^-1))^-1)]);
end);

#############

InstallMethod(IsOne, "for objects in `IsRZMSIsoByTriple'", true, [IsEndoGeneralMapping and IsRZMSIsoByTripleRep], 0, 
function(triple)

return IsOne(triple!.triple[1]) and IsOne(triple!.triple[2]) and ForAll(triple!.triple[3], IsOne);
end);

#############

InstallMethod(\=, "for objects in `IsRZMSIsoByTriple'", IsIdenticalObj, [IsEndoGeneralMapping and IsRZMSIsoByTripleRep, IsEndoGeneralMapping and IsRZMSIsoByTripleRep],  0,
function(triple1, triple2)

if triple1!.triple[1]=triple2!.triple[1] and triple1!.triple[2]=triple2!.triple[2] and triple1!.triple[3]=triple2!.triple[3] then 

	return true;

else 

	return OnTuples(GeneratorsOfSemigroup(Source(triple1)), triple1)=OnTuples(GeneratorsOfSemigroup(Source(triple1)), triple2);
fi; 

end);

#############

InstallMethod(\<, "for objects in `IsRZMSIsoByTriple'", IsIdenticalObj,
[IsGeneralMapping and IsRZMSIsoByTripleRep, IsGeneralMapping and 
 IsRZMSIsoByTripleRep],  0,
function(triple1, triple2)

return (triple1!.triple[1]<triple2!.triple[1]) or
	(triple1!.triple[1]=triple2!.triple[1] and triple1!.triple[2]<triple2!.triple[2]) or (triple1!.triple[1]=triple2!.triple[1] and triple1!.triple[2]=triple2!.triple[2] and triple1!.triple[3]<triple2!.triple[3]);

	
end);

#############################################################################
##
##  <#GAPDoc Label="RMSInducedFunction">
##  <ManSection>
##	<Oper Name="RMSInducedFunction" Arg="RMS, lambda, gamma, g"/> 
##	<Description>
##	<C>lambda</C> is an automorphism of the graph associated to the Rees matrix 
##	semigroup <C>RMS</C>, <C>gamma</C> an automorphism of the underlying group 
##	of <C>RMS</C>, and <C>g</C> an element of the underlying group of 
##	<C>RMS</C>.  The function <C>RMSInducedFunction</C> attempts to find the 
##	function determined by <C>lambda</C> and <C>gamma</C> from the union of the 
##	index sets <M>I</M> and <M>J</M> 
##	to the group <M>G</M> of <C>RMS</C><M>=M[G; I, J; P]</M> where the first 
##	element is given by the element <C>g</C>.  If a conflict is found, then 
##	false is returned together with the induced map. See <Cite Key="computing" 
##	Where="Section 2"/> for further details.
##	<Example>
##  gap&gt; G:=Group([ (1,2) ]);;
##  gap&gt; mat:=[ [ (), (), () ], [ (), (1,2), () ], [ (), (1,2), (1,2) ], 
##  &gt;    [ (), (), () ], [ (), (1,2), () ] ];;
##  gap&gt; rms:=ReesMatrixSemigroup(G, mat);;
##  gap&gt; l:=(1,2)(4,5,6);
##  (1,2)(4,5,6)
##  gap&gt; gam:=One(AutomorphismGroup(G));
##  IdentityMapping( Group([ (1,2) ]) )
##  gap&gt; g:=(1,2);
##  gap&gt; RMSInducedFunction(rms, l, gam, g);
##  [ false, [ (1,2), (), (), (), (), (1,2), (1,2), () ] ]
##  gap&gt; RMSInducedFunction(rms, (4,7), gam, ());
##  [ true, [ (), (), (), (), (), (), (), () ] ]
##  </Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(RMSInducedFunction, "for a RMS", true, [IsReesMatrixSemigroup, IS_PERM, IsGeneralMapping, IsMultiplicativeElement], 0,

function(rms, l, g, groupelt)
local mat, m, n, imagelist, i, j;

imagelist:=[];
mat:=SandwichMatrixOfReesMatrixSemigroup(rms);
m:=Length(mat[1]); n:=Length(mat);

imagelist[1]:=groupelt;

imagelist{[m+1..n+m]}:=List([m+1..n+m], v-> mat[v^l-m][1^l]*imagelist[1]*(mat[v-m][1]^g)^-1);
imagelist{[2..m]}:=List([2..m], v-> (mat[(m+1)^l-m][v^l])^-1*imagelist[m+1]*(mat[(m+1)-m][v]^g));

for i in [2..m] do 
  for j in [m+2..n+m] do 
    if not mat[j^l-m][i^l]=imagelist[j]*mat[j-m][i]^g*imagelist[i]^-1 then 
      return [false, imagelist];
    fi;
  od;
od;

return [true, imagelist];

end);

#############################################################################
##
##  <#GAPDoc Label="RZMSInducedFunction">
##  <ManSection>
##	<Oper Name="RZMSInducedFunction" Arg="RZMS, lambda, gamma, g, comp"/> 
##	<Description>
##	<C>lambda</C> is an automorphism of the graph associated to the Rees 0-
##	matrix semigroup <C>RZMS</C>, <C>gamma</C> an automorphism of the underlying 
##	zero group of <C>RZMS</C>, <C>comp</C> is a connected component of the graph 
##	associated to <C>RZMS</C>, and <C>g</C> is an element of the underlying zero 
##	group of <C>RZMS</C>.  The function <C>RZMSInducedFunction</C> attempts to 
##	find the partial function determined by <C>lambda</C> and <C>gamma</C> from <C>comp</C> to the zero group <M>G^0</M> 
##	of <C>RZMS</C><M>=M^0[G^0; I, J; P]</M> where the image of the first element 
##	in <C>comp</C> is given by the element <C>g</C>.  If a conflict is found, 
##	then fail is returned. See <Cite Key="computing" Where="Section 2"/> for 
##	further details.
##	<Example>
##  gap&gt; zg:=ZeroGroup(Group(()));;
##  gap&gt; z:=Elements(zg)[1];
##  0
##  gap&gt; x:=Elements(zg)[2];
##  ()
##  gap&gt; mat:=[ [ z, z, z ], [ x, z, z ], [ x, x, z ] ];;
##  gap&gt; rzms:=ReesZeroMatrixSemigroup(zg, mat);;
##  gap&gt; RZMSInducedFunction(rzms, (), One(AutomorphismGroup(zg)), x, [1,2,5,6])
##  [ (), (),,, (), () ]
##  gap&gt; RZMSInducedFunction(rzms, (), One(AutomorphismGroup(zg)), x, [3]);     
##  [ ,, () ]
##  gap&gt; RZMSInducedFunction(rzms, (), One(AutomorphismGroup(zg)), x, [4]);
##  [ ,,, () ]
##  gap&gt; zg:=ZeroGroup(Group([ (1,5,2,3), (1,4)(2,3) ]));;
##  gap&gt; elts:=Elements(zg);;
##  gap&gt; mat:=[ [ elts[1], elts[1], elts[11], elts[1], elts[1] ], 
##  &gt;    [ elts[1], elts[13], elts[21], elts[1], elts[1] ], 
##  &gt;    [ elts[1], elts[16], elts[1], elts[16], elts[3] ], 
##  &gt;    [ elts[10], elts[17], elts[1], elts[1], elts[1] ], 
##  &gt;    [ elts[1], elts[1], elts[1], elts[4], elts[1] ] ];
##  gap&gt; rzms:=ReesZeroMatrixSemigroup(zg, mat);                                   
##  gap&gt; RZMSInducedFunction(rzms, (), Random(AutomorphismGroup(zg)), Random(elts), [1..10])=fail;
##  false
##	</Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(RZMSInducedFunction, "for a RZMS", true, [IsReesZeroMatrixSemigroup, IS_PERM, IsGeneralMapping, IsMultiplicativeElement, IsList], 0,

function(rms, l, g, groupelt, component)

Print("#I It appears that the `grape' package is not fully installed. As a\n#I consequence this function is not currently available.\n");
return fail;

end);

#############################################################################
##
##  <#GAPDoc Label="RZMStoRZMSInducedFunction">
##  <ManSection>
##	<Oper Name="RZMStoRZMSInducedFunction" Arg="RZMS1, RZMS2, lambda, gamma, elts"/>
##	<Description>
##	<C>lambda</C> is an automorphism of the graph associated to the Rees 0-
##	matrix semigroup <C>RZMS1</C> composed with isomorphism from that graph to the graph of <C>RZMS2</C>, <C>gamma</C> an automorphism of the 
##	underlying zero group of <C>RZMS1</C>,  and <C>elts</C> is a list of 
##	elements of the underlying zero group of <C>RZMS2</C>.  The function 
##	<C>RZMStoRZMSInducedFunction</C> attempts to find the function determined by
##	<C>lambda</C> and <C>gamma</C> from the union of the index sets <M>I</M> and
##	<M>J</M> of <C>RZMS1</C> to the zero group <M>G^0</M> of 
##	<C>RZMS2</C><M>=M^0[G^0; I, J; P]
##	</M> where the image of the first element in the <M>i</M>th connected
##	component of the associated graph of <C>RZMS1</C> is given by 
##	<C>elts[i]</C>.  If a conflict is found, then false is returned. See 
##	<Cite Key="computing" Where="Section 2"/> for further details.
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 4, 4, 8, 8, 8, 8, 4, 8 ] ), 
##    Transformation( [ 8, 2, 8, 2, 5, 5, 8, 8 ] ), 
##    Transformation( [ 8, 8, 3, 7, 8, 3, 7, 8 ] ), 
##    Transformation( [ 8, 6, 6, 8, 6, 8, 8, 8 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; D:=GreensDClasses(S);;
##  gap&gt; rms1:=Range(IsomorphismReesMatrixSemigroupOfDClass(D[1]));
##  Rees Zero Matrix Semigroup over &lt;zero group with 2 generators&gt;
##  gap&gt; rms2:=Range(IsomorphismReesMatrixSemigroupOfDClass(D[4]));
##  Rees Zero Matrix Semigroup over &lt;zero group with 2 generators&gt;
##  gap&gt; gam:=One(AutomorphismGroup(UnderlyingSemigroupOfReesZeroMatrixSemigroup(Group(rms1))));
##  IdentityMapping( &lt;zero group with 2 generators&gt; )
##  gap&gt; g:=One(UnderlyingSemigroupOfReesZeroMatrixSemigroup(rms2));
##  ()
##  gap&gt; RZMStoRZMSInducedFunction(rms1, rms2, (2,3)(5,6), gam, [g]);
##  [ (), (), (), (), (), () ]
##	</Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(RZMStoRZMSInducedFunction, "for a RZMS", true, [IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroup, IS_PERM, IsGeneralMapping, IsList], 0,

function(rms1, rms2, l, g, groupelts)

Print("#I It appears that the `grape' package is not fully installed. As a\n#I consequence this function is not currently available.\n");
return fail;

end);

#############################################################################
##
##  <#GAPDoc Label="RZMSGraph">
##  <ManSection>
##  <Func Name="RZMSGraph" Arg="rzms"/>
##	<Description>
##  if <C>rzms</C> is the Rees 0-matrix semigroup M[G;I,J;P], then 
##	<C>RZMSGraph</C> returns the 
##  undirected bipartite graph with |I|+|J| vertices and edge (i,j) if and only 
##  if <M>i&lt;|I|+1</M>, <M>j&gt;|I|</M> and <M>p_{j-|I|, i}</M> is not zero.  
##	The returned object is a simple undirected graph created in 
##	<Package>GRAPE</Package> using the command
##	<Display>
##	Graph(Group(()), [1..n+m], OnPoints, adj, true);
##	</Display>
##	where <C>adj</C> is <C>true</C> if and only 
##  if <M>i&lt;|I|+1</M>, <M>j&gt;|I|</M> and <M>p_{j-|I|, i}</M> is not zero.
##	<Example>
##  gap&gt; zg:=ZeroGroup(Group(()));;
##  gap&gt; z:=Elements(zg)[1];
##  0
##  gap&gt; x:=Elements(zg)[2];
##  ()
##  gap&gt; mat:=[ [ 0, 0, 0 ], [ (), 0, 0 ], [ (), (), 0 ] ];;
##  gap&gt; rzms:=ReesZeroMatrixSemigroup(zg, mat);;
##  gap&gt; RZMSGraph(rzms);
##  rec( isGraph := true, order := 6, group := Group(()), 
##    schreierVector := [ -1, -2, -3, -4, -5, -6 ], 
##    adjacencies := [ [ 5, 6 ], [ 6 ], [  ], [  ], [ 1 ], [ 1, 2 ] ], 
##    representatives := [ 1, 2, 3, 4, 5, 6 ], names := [ 1, 2, 3, 4, 5, 6 ] )
##  gap&gt; UndirectedEdges(last);
##  [ [ 1, 5 ], [ 1, 6 ], [ 2, 6 ] ]
##  </Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(RZMSGraph, "for a RZMS", true, [IsReesZeroMatrixSemigroup], 0,
function(rms)

Print("#I It appears that the `grape' package is not fully installed. As a\n#I consequence this function is not currently available.\n");
return fail;

end);

###########################################################################

###################
## Isomorphisms	  #
###################

###########################################################################

###########################################################################
##
##  <#GAPDoc Label="IsomorphismAutomorphismGroupOfRMS">
##  <ManSection>
##  <Attr Name="IsomorphismAutomorphismGroupOfRMS" Arg="G"/>
##
##	<Description>
##	if <C>G</C> is the automorphism group of a simple transformation semigroup, 
##	then <C>IsomorphismAutomorphismGroupOfRMS</C> returns a 
##	<Ref Func="GroupHomomorphismByImages" BookName="ref"/> from the automorphism 
##	group of <C>G</C> to the automorphism group of an isomorphic Rees matrix 
##	semigroup, obtained by using <Ref Func="IsomorphismReesMatrixSemigroup"/>.
##	<Example>
##  gap&gt; g1:=Transformation([1,2,2,1,2]);;
##  gap&gt; g2:=Transformation([3,4,3,4,4]);;
##  gap&gt; g3:=Transformation([3,4,3,4,3]);;
##  gap&gt; g4:=Transformation([4,3,3,4,4]);;
##  gap&gt; cs5:=Semigroup(g1,g2,g3,g4);;
##  gap&gt; AutomorphismGroup(cs5);
##  &lt;group of size 16 with 3 generators&gt;
##  gap&gt; IsomorphismAutomorphismGroupOfRMS(last);
##  [ SemigroupHomomorphism ( &lt;semigroup with 4 generators&gt;-&gt;&lt;semigroup with 
##      4 generators&gt;), SemigroupHomomorphism ( &lt;semigroup with 
##      4 generators&gt;-&gt;&lt;semigroup with 4 generators&gt;), 
##    SemigroupHomomorphism ( &lt;semigroup with 4 generators&gt;-&gt;&lt;semigroup with 
##      4 generators&gt;) ] -&gt; 
##  [ [ (1,4)(2,3)(5,6), IdentityMapping( Group( [ (1,2) ] ) ), 
##        [ (), (1,2), (1,2), (), (), () ] ], 
##    [ (1,3,4,2), IdentityMapping( Group( [ (1,2) ] ) ), 
##        [ (), (), (), (), (), (1,2) ] ], 
##    [ (1,3)(2,4), IdentityMapping( Group( [ (1,2) ] ) ), 
##        [ (), (), (), (), (), (1,2) ] ] ] 
##	</Example> <!-- auto2.tst --> 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(IsomorphismAutomorphismGroupOfRMS, "for the automorphism group of a simple semigroup", true, [IsAutomorphismGroupOfSimpleSemigp], 0, 
function(G)
local S, iso, H, hom;

S:=Source(One(G));
iso:=IsomorphismReesMatrixSemigroup(S);
H:=AutomorphismGroup(Range(iso));

return GroupHomomorphismByImagesNC(G, H, GeneratorsOfGroup(G), SmallGeneratingSet(H));
end);

###########################################################################
##
##  <#GAPDoc Label="IsomorphismFpSemigroup">
##  <ManSection>
##  <Attr Name="IsomorphismFpSemigroup" Arg="S"/>
##	<Description>
##	returns an isomorphism to a finitely presented semigroup from the 
##	transformation semigroup <C>S</C>. Currently works by running the function
##	<Ref Func="FroidurePinExtendedAlg" BookName="ref"/> in the library. 
##	If <C>S</C> satisfies <Ref Prop="IsMonoid" BookName="ref"/>, use the command 
##	<Ref Attr="IsomorphismFpMonoid"/> instead.
##	<Example> 
##  gap&gt; gens:=[ Transformation( [1,8,11,2,5,16,13,14,3,6,15,10,7,4,9,12 ] ), 
##  &gt;   Transformation( [1,16,9,6,5,8,13,12,15,2,3,4,7,10,11,14] ), 
##  &gt;   Transformation( [1,3,7,9,1,15,5,11,13,11,13,3,5,15,7,9 ] ) ];
##  gap&gt; S:=Semigroup(gens);
##  &lt;semigroup with 3 generators&gt;
##  gap&gt; IsomorphismFpSemigroup(last);
##  SemigroupHomomorphismByImages ( &lt;trans. semigroup of size 16 with 
##  3 generators&gt;-&gt;Semigroup( [ s1, s2, s3 ] ))
##	</Example> <!--autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(IsomorphismFpSemigroup, "for a transformation semigroup", true, [IsTransformationSemigroup], 0, 
function(S)

if IsMonoid(S) then 
  Error("<S> must NOT be a monoid");
else
  FroidurePinExtendedAlg(S);
  return IsomorphismFpSemigroup(S);
fi;
end);

###########################################################################
##
##  <#GAPDoc Label="IsomorphismFpMonoid">
##  <ManSection>
##  <Attr Name="IsomorphismFpMonoid" Arg="S"/>
##	<Description>
##	returns an isomorphism to a finitely presented monoid from the 
##	transformation monoid <C>S</C>. Currently works by running the function
##	<Ref Func="FroidurePinExtendedAlg" BookName="ref"/> in the library. 
##	If <C>S</C> satisfies <Ref Prop="IsSemigroup" BookName="ref"/>, use the 
##	command <Ref Attr="IsomorphismFpSemigroup"/> instead.
##	<Example> 
##  gap&gt; x:=Transformation([2,3,4,5,6,7,8,9,1]);;
##  gap&gt; y:=Transformation([4,2,3,4,5,6,7,8,9]);;
##  gap&gt; S:=Monoid(x,y);;
##  gap&gt; IsomorphismFpMonoid(last);
##  SemigroupHomomorphismByImages ( &lt;trans. semigroup of size 40266 with 
##  3 generators&gt;-&gt;Monoid( [ m1, m2 ], ... ))
##  gap&gt; Length(RelationsOfFpMonoid(Range(last)));
##  932
##	</Example>  <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallOtherMethod(IsomorphismFpMonoid, "for a transformation monoid", true, [IsTransformationMonoid], 0, 
function(S)

FroidurePinExtendedAlg(S);
return IsomorphismFpMonoid(S);
end);

#############################################################################
##
##  <#GAPDoc Label="IsomorphismSemigroups">
##  <ManSection>
##	<Oper Name="IsomorphismSemigroups" Arg="S, T"/>
##	<Description>
##	this operation returns an isomorphism from the semigroup <C>S</C> to 
##	the semigroup <C>T</C> if one exists and returns fail otherwise. <P/>
##
## So far, there are only methods available for zero groups, zero semigroups, 
##	Rees matrix semigroups, and Rees 0-matrix semigroups.  
##	<Example>
##  gap&gt; ZG1:=ZeroGroup(Group((1,2,3,5,4)));
##  &lt;zero group with 2 generators&gt;
##  gap&gt; ZG2:=ZeroGroup(Group((1,2,3,4,5)));
##  &lt;zero group with 2 generators&gt;
##  gap&gt; IsomorphismSemigroups(ZG1, ZG2);
##  SemigroupHomomorphismByImagesOfGens ( &lt;zero group with 
##  2 generators&gt;-&gt;&lt;zero group with 2 generators&gt;)
##  gap&gt; ZG2:=ZeroGroup(Group((1,2,3,4)));
##  &lt;zero group with 2 generators&gt;
##  gap&gt; IsomorphismSemigroups(ZG1, ZG2);
##  fail
##  gap&gt; IsomorphismSemigroups(ZeroSemigroup(5),ZeroSemigroup(5));
##  IdentityMapping( &lt;zero semigroup with 5 elements&gt; )
##  gap&gt; IsomorphismSemigroups(ZeroSemigroup(5),ZeroSemigroup(6));
##  fail
##  gap&gt; gens:=[ Transformation( [ 4, 4, 8, 8, 8, 8, 4, 8 ] ), 
##  &gt;   Transformation( [ 8, 2, 8, 2, 5, 5, 8, 8 ] ), 
##  &gt;   Transformation( [ 8, 8, 3, 7, 8, 3, 7, 8 ] ), 
##  &gt;   Transformation( [ 8, 6, 6, 8, 6, 8, 8, 8 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; D:=GreensDClasses(S);;
##  gap&gt; rms1:=Range(IsomorphismReesMatrixSemigroupOfDClass(D[1]));;
##  gap&gt; rms2:=Range(IsomorphismReesMatrixSemigroupOfDClass(D[4]));;
##  gap&gt; IsomorphismSemigroups(rms1, rms2);
##  [ (2,3)(5,6), IdentityMapping( &lt;zero group with 2 generators&gt; ), 
##    [ ZeroGroup(()), ZeroGroup(()), ZeroGroup(()), ZeroGroup(()), ZeroGroup(()), 
##        ZeroGroup(()) ] ]
##  gap&gt; IsomorphismSemigroups(rms2, rms1);
##  [ (2,3)(5,6), IdentityMapping( &lt;zero group with 2 generators&gt; ), 
##    [ ZeroGroup(()), ZeroGroup(()), ZeroGroup(()), ZeroGroup(()), ZeroGroup(()), 
##        ZeroGroup(()) ] ]
##  gap&gt; rms2:=Range(IsomorphismReesMatrixSemigroupOfDClass(D[2]));
##  Group(())
##  gap&gt; IsomorphismSemigroups(rms2, rms1);
##  fail
##  gap&gt; rms2:=RandomReesZeroMatrixSemigroup(5,5,5);
##  Rees Zero Matrix Semigroup over &lt;zero group with 2 generators&gt;
##  gap&gt; IsomorphismSemigroups(rms2, rms1);
##  fail
##  gap&gt; rms2:=RandomReesMatrixSemigroup(5,5,5);
##  Rees Matrix Semigroup over Group([ (1,2)(3,4,5), (2,4,3), (1,4,5,3), 
##    (1,4,5,2) ])
##  gap&gt; IsomorphismSemigroups(rms2, rms1);
##  fail
##  gap&gt; IsomorphismSemigroups(rms1, rms2);
##  fail
##	</Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallOtherMethod(IsomorphismSemigroups,  "for a ZG and ZG", true, [IsZeroGroup, IsZeroGroup], 0,
function(ZG1, ZG2)
local G1, G2, iso, imgs;

if ZG1=ZG2 then 
  return IdentityMapping(ZG1);
else

  G1:=UnderlyingGroupOfZG(ZG1);
  G2:=UnderlyingGroupOfZG(ZG2);
  iso:=IsomorphismGroups(G1, G2);

  if not iso=fail then #JDM could be some ordering problem here
    imgs:=OnTuples(List(GeneratorsOfMonoid(ZG1), 
           UnderlyingGroupEltOfZGElt){[1..Length(GeneratorsOfMonoid(ZG1))-1]}, iso);

  return SemigroupHomomorphismByImagesOfGensNC(ZG1, ZG2, Concatenation(
          List(imgs, ZeroGroupElt), [MultiplicativeZero(ZG2)]));
  fi;
fi;

return fail;

end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a RMS and RZMS", true, [IsReesMatrixSemigroup, IsReesZeroMatrixSemigroup], 0,
function(S1, S2)

return fail;

end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a RZMS and RMS", true, [IsReesZeroMatrixSemigroup, IsReesMatrixSemigroup], 0,
function(S1, S2)

return fail;

end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a RMS and ZS", true, [IsReesMatrixSemigroup, IsZeroSemigroup], 0,
function(S1, S2)
return fail;
end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a ZS and RMS", true, [IsZeroSemigroup, IsReesMatrixSemigroup], 0,
function(S1, S2)
return fail;
end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a ZS and ZS", true, [IsZeroSemigroup, IsZeroSemigroup], 0,
function(S1, S2)

if S1=S2 then 
  return IdentityMapping(S1); 
else 
  return fail;
fi;
# if they have the same number of elements, they are the same

end);

#############################################################################

InstallMethod(IsomorphismSemigroups, "for a RMS and RMS", true, [IsReesMatrixSemigroup, IsReesMatrixSemigroup], 0,
function(R1, R2)
local g1, g2, iso, G1, G2, isograph, isogroup, f, l, g, tup, candidate, mat, m, n;

if not (Size(R1)=Size(R2) and ColumnsOfReesMatrixSemigroup(R1)=ColumnsOfReesMatrixSemigroup(R2) and RowsOfReesMatrixSemigroup(R1)=RowsOfReesMatrixSemigroup(R2)) then 
  return fail;
else

	mat:=SandwichMatrixOfReesMatrixSemigroup(R1);
	m:=Length(mat[1]); n:=Length(mat);

	if R1=R2 then 

		g:=UnderlyingSemigroupOfReesMatrixSemigroup(R1); 
		return RMSIsoByTriple(R1, R2, [(), IdentityMapping(g), List([1..m+n], 
                         x-> One(g))]);

	else
	
		g1:=UnderlyingSemigroupOfReesMatrixSemigroup(R1);
		g2:=UnderlyingSemigroupOfReesMatrixSemigroup(R2);
		iso:=IsomorphismGroups(g1, g2);

  #for RMS without 0 the graphs are always isomorphic, being complete bipartite.

		if iso=fail then 
			return fail;
  	else 
			isograph:=DirectProduct(SymmetricGroup(m), SymmetricGroup(n));
			
			#all isomorphisms from g1 to g2
			isogroup:=List(Elements(AutomorphismGroup(g1)), x->x*iso); 
  
			#find an induced function, if there is one
			for l in isograph do
				for g in isogroup do
					for tup in Elements(g2) do 
						candidate:=RMSInducedFunction(R2, l, g, tup); 
						if not candidate[1]=false then 
							return RMSIsoByTriple(R1, R2, [l, g, candidate[2]]);
						fi;
					od;
				od;
			od;

			return fail;
		fi;
	fi;
fi;
end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups, "for a RZMS and RZMS", true, [IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroup], 0,
function(R1, R2)

Print("#I It appears that the `grape' package is not fully installed. As a\n#I consequence this function is not currently available.\n");
return fail;

end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a group and RZMS", true, [IsGroup, IsReesZeroMatrixSemigroup], 0,
function(S1, S2)
return fail;
end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a RZMS and group", true, [IsReesZeroMatrixSemigroup, IsGroup], 0,
function(S1, S2)
return fail;
end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a group and zero semigroup", true, [IsGroup, IsZeroSemigroup], 0,
function(S1, S2)
return fail;
end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a zero semigroup and group", true, [IsZeroSemigroup, IsGroup], 0,
function(S1, S2)
return fail;
end);

###########################################################################
## 
##  <#GAPDoc Label="IsomorphismReesMatrixSemigroupOfDClass">
##  <ManSection>
##	<Oper Name="IsomorphismReesMatrixSemigroupOfDClass" Arg="D"/>
##	<Description>
##	The <E>principal factor</E> of the <M>D</M>-class <C>D</C> is the semigroup 
##	with elements <M>D</M> and <M>0</M> and multiplication <M>x*y</M> defined to 
##	be the product <M>xy</M> in the semigroup containing <C>D</C> if 
##	<M>xy \in D</M> and <M>0</M> otherwise. <P/>
##
##	<C>IsomorphismReesMatrixSemigroupOfDClass</C> returns an 
##	isomorphism from the principal factor of the
##	<M>D</M>-class <C>D</C> to a Rees matrix, Rees 0-matrix or 
##	zero semigroup, as given by the Rees-Suschewitsch Theorem; see 
##	<Cite Key="howie" Where="Theorem 3.2.3"/>. 
##	<Example> 
##  gap&gt; g1:=Transformation( [ 4, 6, 3, 8, 5, 6, 10, 4, 3, 7 ] );;
##  gap&gt; g2:=Transformation( [ 5, 6, 6, 3, 8, 6, 3, 7, 8, 4 ] );;
##  gap&gt; g3:=Transformation( [ 8, 6, 3, 2, 8, 10, 9, 2, 6, 2 ] );;
##  gap&gt; m23:=Monoid(g1,g2,g3);;
##  gap&gt; D:=GreensDClasses(m23)[17];
##  {Transformation( [ 7, 6, 6, 6, 7, 4, 8, 6, 6, 6 ] )}
##  gap&gt; IsomorphismReesMatrixSemigroupOfDClass(D);
##  SemigroupHomomorphism ( {Transformation( [ 7, 6, 6, 6, 7, 4, 8, 6, 6, 6 
##   ] )}-&gt;&lt;zero semigroup with 3 elements&gt;)
##  gap&gt; D:=GreensDClasses(m23)[77];
##  {Transformation( [ 6, 6, 6, 6, 6, 6, 6, 6, 6, 6 ] )}
##  gap&gt; IsomorphismReesMatrixSemigroupOfDClass(D);
##  SemigroupHomomorphism ( {Transformation( [ 6, 6, 6, 6, 6, 6, 6, 6, 6, 6 
##   ] )}-&gt;Rees Matrix Semigroup over Group(()))
##  gap&gt; D:=GreensDClasses(m23)[1];
##  {Transformation( [ 1 .. 10 ] )}
##  gap&gt; IsomorphismReesMatrixSemigroupOfDClass(D);
##  SemigroupHomomorphism ( {Transformation( [ 1 .. 10 ] )}-&gt;Group(()))
##  gap&gt; D:=GreensDClasses(m23)[23];
##  {Transformation( [ 6, 7, 3, 6, 6, 6, 6, 6, 7, 6 ] )}
##  gap&gt; IsomorphismReesMatrixSemigroupOfDClass(D);
##  SemigroupHomomorphism ( {Transformation( [ 6, 7, 3, 6, 6, 6, 6, 6, 7, 6 
##   ] )}-&gt;Rees Zero Matrix Semigroup over &lt;zero group with 3 generators&gt;)
##	</Example> <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(IsomorphismReesMatrixSemigroupOfDClass, "for a D-class", true, [IsGreensDClass and IsAssociatedSemigpTransSemigp], 0, 
function(D)
local g, zg, rep, r, l, rreps, lreps, mat, func, rms, invlreps, invrreps, Q, R, hom, invfunc, RMS;

if  IsRegularDClass(D) then 
	g:= Range(IsomorphismPermGroup(GroupHClassOfGreensDClass(D)));
	zg:=ZeroGroup(g);
	rep:=Representative(GroupHClassOfGreensDClass(D));

	r:=GreensRClassOfElement(ParentAttr(D), rep);
	l:=GreensLClassOfElement(ParentAttr(D), rep);

	rreps:=List(GreensHClasses(l), Representative);
	lreps:=List(GreensHClasses(r), Representative);
	
	if Length(lreps)=1 and Length(rreps)=1 then #it is a group
		rms:=g;
		func:=PermRepTrans;
		invfunc:=x-> IdempotentNC(KernelOfTransformation(rep), ImageSetOfTransformation(rep))*x;
		#hom:=SemigroupHomomorphismByFunctionNC(D, g, PermRepTrans);
		#return SemigroupHomomorphismByFunctionNC(D, g, PermRepTrans);
	else

		RMS:=true;
	
		mat:=List(lreps, x-> List(rreps, function(y)
			if x*y in D then 
				return PermRepTrans(x*y);
			else
				RMS:=false;
				return MultiplicativeZero(zg);
			fi; end));

		if RMS then 
			rms:=ReesMatrixSemigroup(g, mat);
	
			func:=function(d)
				local col, row;
				col:=PositionProperty(lreps, x-> ImageSetOfTransformation(d)=ImageSetOfTransformation(x));
				row:=PositionProperty(rreps, x-> KernelOfTransformation(d)=KernelOfTransformation(x));    
				return ReesMatrixSemigroupElementNC(rms, row,
					PermRepTrans(lreps[col]*rreps[row])^-1*
					PermRepTrans(lreps[col]*d*rreps[row])*
					PermRepTrans(lreps[col]*rreps[row])^-1, col);
			end;
		
			invfunc:=function(rmselt)
			local i,a,lambda;

			i:=RowIndexOfReesMatrixSemigroupElement(rmselt);
			a:=UnderlyingElementOfReesMatrixSemigroupElement(rmselt);
			lambda:=ColumnIndexOfReesMatrixSemigroupElement(rmselt);

			return rreps[i]*a*lreps[lambda];
			end;

		else

		#find inverses for rreps and lreps
	
		#JDM is there an easier way? 
		Q:=List([1..Length(rreps)], x-> PositionProperty( List(mat, y-> y[x]), z-> not z=MultiplicativeZero(zg)));
		invrreps:=List([1..Length(rreps)], x-> mat[Q[x]][x]^-1*lreps[Q[x]]);

		R:=List([1..Length(lreps)], x-> PositionProperty(mat[x], y-> not y=MultiplicativeZero(zg)));
		invlreps:=List([1..Length(lreps)], x-> rreps[R[x]]*mat[x][R[x]]^-1);

		mat:=List(mat, x-> List(x, function(y)
			if not y=MultiplicativeZero(zg) then 
				return ZeroGroupElt(y);
			fi;
			return y; end));

		rms:=ReesZeroMatrixSemigroup(zg, mat);

		func:=function(d)
		local col, row;
	
		col:=PositionProperty(lreps, x-> ImageSetOfTransformation(d)=ImageSetOfTransformation(x));
		if not col=fail then 
			row:=PositionProperty(rreps, x-> KernelOfTransformation(d)=KernelOfTransformation(x));    

			return ReesZeroMatrixSemigroupElementNC(rms, row, ZeroGroupElt(PermRepTrans(invrreps[row]*d*invlreps[col])), col);
		fi;
	
		return MultiplicativeZero(rms);
		end;
		
		invfunc:=function(rmselt)
		local i,a,lambda;
		
		if rmselt=MultiplicativeZero(zg) then
			Error("the multiplicative zero has no preimage");
		fi;
		
		i:=RowIndexOfReesZeroMatrixSemigroupElement(rmselt);
		a:=UnderlyingGroupEltOfZGElt(UnderlyingElementOfReesZeroMatrixSemigroupElement(rmselt));
		lambda:=ColumnIndexOfReesZeroMatrixSemigroupElement(rmselt);

		return rreps[i]*a*lreps[lambda];
		end;
	fi;
fi;
else	#it's not a regular D-class
			#and so it's a zero semigroup
	 
	rms:=ZeroSemigroup(Size(D)+1);

	func:=function(x)
		if x in D then 
			return Elements(rms)[Position(Elements(D), x)+1];
		else
			return MultiplicativeZero(rms);
		fi;
	end;
	
	invfunc:=function(x)
		local str;
		
		if x=MultiplicativeZero(rms) then
			Error("the multiplicative zero has no preimage");
		fi;
		
		#str:=String(x);
		#return Elements(D)[Int(str{[2..Length(str)]})];
		return Elements(D)[x![1]];
	end;

fi;

hom:=SemigroupHomomorphismByFunctionNC(D, rms, func);
SetInverseGeneralMapping(hom, 
       SemigroupHomomorphismByFunctionNC(rms, D, invfunc));
SetIsInjective(hom, true);
SetIsSurjective(hom, true);
SetIsTotal(hom, true);
if not HasIsZeroSemigroup(rms) then 
	SetIsZeroSemigroup(rms, false);
fi;

return hom;

end);


#############################################################################
## 
##  <#GAPDoc Label="IsomorphismReesMatrixSemigroup">
##  <ManSection>
##	<Oper Name="IsomorphismReesMatrixSemigroup" Arg="S"/>
##	<Description>
##	returns an isomorphism from the (completely) simple transformation semigroup 
##	<M>S</M> to a Rees matrix semigroup, as given by the Rees-Suschewitsch 
##	Theorem; see <Cite Key="howie" Where="Theorem 3.2.3"/>.
##	<Example>
##  gap&gt; g1:=Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] );;
##  gap&gt; g2:=Transformation( [ 2, 3, 4, 5, 6, 8, 7, 1, 2, 2 ] );;
##  gap&gt; cs2:=Semigroup(g1,g2);;
##  gap&gt; IsomorphismReesMatrixSemigroup(cs2);
##  SemigroupHomomorphism ( &lt;semigroup with 
##  2 generators&gt;-&gt;Rees Matrix Semigroup over Group(
##  [ (2,5)(3,8)(4,6), (1,6,3)(5,8) ]))
##  </Example>  <!-- autos3.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>
##
##
## JDM it might be possible to improve this after making improvements to the 
## JDM the way in which MONOID handles Green's relations of completely simple
## JDM semigroups.

InstallMethod(IsomorphismReesMatrixSemigroup, "for a transformation semigroup", true, [IsTransformationSemigroup], 0, 
function(S)
local g, rep, r, l, rreps, lreps, mat, func, rms, D, img, ker, hom,
	invfunc;

if IsCompletelySimpleSemigroup(S) #and not IsGroupAsTransSemigroup(S) 
	then 

  D:=GreensDClassOfElement(S, Representative(S));
  g:= Range(IsomorphismPermGroup(GroupHClassOfGreensDClass(D)));
    
  rep:=Representative(GroupHClassOfGreensDClass(D));

  r:=GreensRClassOfElement(S, rep);
  l:=GreensLClassOfElement(S, rep);
  img:=ImageSetOfTransformation(Representative(l));
  ker:=KernelOfTransformation(Representative(r));

  rreps:= List(List(GreensHClasses(l), x-> KernelOfTransformation
           (Representative(x))), y-> IdempotentNC(y, img)); 
  lreps:= List(List(GreensHClasses(r), x-> ImageSetOfTransformation
           (Representative(x))), y-> IdempotentNC(ker, y)); 

  mat:=List(lreps, x-> List(rreps, y-> PermRepTrans(x*y)));

  rms:=ReesMatrixSemigroup(g, mat);
  
  #JDM this could be speeded up :)
  func:=function(d)
    local col, row;
    col:=PositionProperty(lreps, x-> ImageSetOfTransformation(d)
		=ImageSetOfTransformation(x));
    row:=PositionProperty(rreps, x-> KernelOfTransformation(d)
		=KernelOfTransformation(x));    

    return ReesMatrixSemigroupElementNC(rms, row, 
	    PermRepTrans(lreps[col]*rreps[row])^-1*
            PermRepTrans(lreps[col]*d*rreps[row])*
            PermRepTrans(lreps[col]*rreps[row])^-1, col);
    end;
   
    invfunc:=function(rmselt)
      local i,a,lambda;

      i:=RowIndexOfReesMatrixSemigroupElement(rmselt);
      a:=UnderlyingElementOfReesMatrixSemigroupElement(rmselt);
      lambda:=ColumnIndexOfReesMatrixSemigroupElement(rmselt);

      return rreps[i]*a*lreps[lambda];
    end;

    hom:=SemigroupHomomorphismByFunctionNC(S, rms, func);
   
    SetInverseGeneralMapping(hom, 
       SemigroupHomomorphismByFunctionNC(rms, S, invfunc));
    SetIsInjective(hom, true);
    SetIsSurjective(hom, true);
    SetIsTotal(hom, true);
    
    return hom;
  #elif IsGroupAsTransSemigroup(S) then
	#	#JDM should there be split cases here for semigroups and monoids
	#	
  #	G:=Group(List(GeneratorsOfSemigroup(S), x-> PermRepTrans(x)));
	#	hom:=SemigroupHomomorphismByFunctionNC(S, G, PermRepTrans);
	#	SetInverseGeneralMapping(hom, SemigroupHomomorphismByFunctionNC(G, S, x->Idempotents(S)[1]*x));
	#	SetIsBijective(hom, true); SetIsTotal(hom, true);
	#	return hom;
		
  else
    Error("<S> must be a completely simple semigroup");
  fi;
end);

###########################################################################
##
##  <#GAPDoc Label="IsomorphismPermGroup">
##  <ManSection>
##  <Attr Name="IsomorphismPermGroup" Arg="G"/>
##	<Description>
##	if <C>G</C> satisfies <Ref Prop="IsAutomorphismGroupOfSimpleSemigp"/>, then 
##	<C>IsomorphismPermGroup</C> returns an isomorphism from <C>G</C> to a 
##	permutation group by composing the result 
##	<C>f</C> of <Ref Attr="IsomorphismAutomorphismGroupOfRMS"/> on <C>G</C> with
##	the result of <C>IsomorphismPermGroup</C> on <C>Range(f)</C>.<P/>
##	
##	if <C>G</C> satisfies <Ref Prop="IsAutomorphismGroupOfRMS"/> or 
##	<Ref Prop="IsAutomorphismGroupOfRZMS"/>, then <C>IsomorphismPermGroup</C> 
##	returns an isomorphism from <C>G</C> to a permutation group acting  
##	either on the elements of <C>S</C> or on 
##	itself, whichever gives a permutation group of lower degree.<P/>
##
##	if <C>G</C> is a transformation semigroup that satisfies 
##	<Ref Prop="IsGroupAsSemigroup"/>, then <C>IsomorphismPermGroup</C> 
##	returns an isomorphism from <C>G</C> to the permutation group obtained by 
##	applying <Ref Func="PermRepTrans"/> to any element of <C>G</C>.<P/>
##
##	if <C>G</C> is a group <M>H</M>-class of a transformation semigroup, then 
##	<C>IsomorphismPermGroup</C> returns an isomorphism from <C>G</C> to the 
##	permutation group obtained by applying <Ref Func="PermRepTrans"/> to any
##	element of <C>G</C>.
##	<Example>
##  gap&gt; g1:=Transformation([3,3,2,6,2,4,4,6]);;
##  gap&gt; g2:=Transformation([5,1,7,8,7,5,8,1]);;
##  gap&gt; cs1:=Semigroup(g1,g2);;
##  gap&gt; AutomorphismGroup(cs1);
##  &lt;group of size 12 with 2 generators&gt;
##  gap&gt; IsomorphismPermGroup(last);
##  [ SemigroupHomomorphism ( &lt;semigroup with 2 generators&gt;-&gt;&lt;semigroup with 
##      2 generators&gt;), SemigroupHomomorphism ( &lt;semigroup with 
##      2 generators&gt;-&gt;&lt;semigroup with 2 generators&gt;) ] -&gt; 
##  [ (1,11,2,12,3,10)(4,8,5,9,6,7), (1,6)(2,5)(3,4)(7,10)(8,12)(9,11) ]
##  gap&gt; Size(cs1);
##  96
##  gap&gt; a:=IdempotentNC([[1,3,4],[2,5],[6],[7],[8]],[3,5,6,7,8])*(3,5);;
##  gap&gt; b:=IdempotentNC([[1,3,4],[2,5],[6],[7],[8]],[3,5,6,7,8])*(3,6,7,8);;
##  gap&gt; S:=Semigroup(a,b);;
##  gap&gt; IsGroupAsSemigroup(S);
##  true
##  gap&gt; IsomorphismPermGroup(S);
##  SemigroupHomomorphism ( &lt;semigroup with 2 generators&gt;-&gt;Group(
##  [ (3,5), (3,6,7,8) ]))
##  gap&gt; gens:=[Transformation([3,5,3,3,5,6]), Transformation([6,2,4,2,2,6])];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; H:=GroupHClassOfGreensDClass(GreensDClassOfElement(S, Elements(S)[1]));
##  {Transformation( [ 2, 2, 2, 2, 2, 6 ] )}
##  gap&gt; IsomorphismPermGroup(H);
##  SemigroupHomomorphism ( {Transformation( [ 2, 2, 2, 2, 2, 6 ] )}-&gt;Group(()))
##	</Example> <!-- autos2.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallOtherMethod(IsomorphismPermGroup, "for an auto. gp. of a simple semigp", true, [IsAutomorphismGroupOfSimpleSemigp], 0, 
function(A)
local iso;

iso:=IsomorphismAutomorphismGroupOfRMS(A);
return iso*IsomorphismPermGroup(Range(iso));

end);

###########################################################################
##JDM this could certainly be improved...

InstallOtherMethod(IsomorphismPermGroup, "for an automorphism group of a RMS", true, [IsAutomorphismGroupOfRMS], 0, 
function(A)
local RMS, permgp, a, perm, hom, i;

RMS:=Source(One(A));
permgp:=Group(());
SetAsSSortedList(permgp, [()]);
	
if Size(RMS)< Size(A) then 
	i:=0;
	repeat
		i:=i+1;
	#for a in [1..Size(A)/2+1] do
		a:= AsSSortedList(A)[i];
		perm:=PermListList([1..Size(RMS)], List([1..Size(RMS)], x-> Position(AsSSortedList(RMS), ImageElm(a, AsSSortedList(RMS)[x]))));
		if not perm in permgp then 
			permgp:=ClosureGroupDefault(permgp, perm); 
		fi;
	#od;
	until Size(permgp)=Size(A);

	#JDM better not to take SmallGeneratingSet of permgp here since
	#JDM the original generators of permgp will be easier to find in A
	
	hom:=GroupHomomorphismByFunction(A, permgp, x-> PermListList([1..Size(RMS)], List([1..Size(RMS)], y-> Position(AsSSortedList(RMS), ImageElm(x, AsSSortedList(RMS)[y])))));
	#JDM need the inverse of hom
	
	#hom:=GroupHomomorphismByImagesNC(A, permgp, GeneratorsOfGroup(A), List(GeneratorsOfGroup(A), x-> PermListList([1..Size(RMS)], List([1..Size(RMS)], y-> Position(AsSSortedList(RMS), ImageElm(x, AsSSortedList(RMS)[y])))))); 
	
	#SetInverseGeneralMapping(hom, GroupHomomorphismByImagesNC(permgp, A, GeneratorsOfGroup(permgp), List(GeneratorsOfGroup(permgp), x-> First(A, y-> ImageElm(hom, y)=x))));

	#JDM need the inverse of hom
	
	SetIsBijective(hom, true);

	return hom;

else 
	hom := ActionHomomorphism( A, A, OnRight, "surjective" );
	SetIsBijective( hom, true );
	return hom;
fi;

end);

###########################################################################

InstallOtherMethod(IsomorphismPermGroup, "for an automorphism group of a RZMS", true, [IsAutomorphismGroupOfRZMS], 0, 
function(A)
local RZMS, permgp, a, perm, hom, i;

RZMS:=Source(One(A));
permgp:=Group(());
SetAsSSortedList(permgp, [()]);
	
if Size(RZMS)<Size(A) then 
	i:=0;
	repeat
		i:=i+1;
		a:= AsSSortedList(A)[i];
		perm:=PermListList([1..Size(RZMS)], List([1..Size(RZMS)], x-> Position(AsSSortedList(RZMS), ImageElm(a, AsSSortedList(RZMS)[x]))));
		if not perm in permgp then 
			permgp:=ClosureGroupDefault(permgp, perm); 
		fi;
	until Size(permgp)=Size(A);
	
	hom:=GroupHomomorphismByFunction(A, permgp, x-> PermListList([1..Size(RZMS)], List([1..Size(RZMS)], y-> Position(AsSSortedList(RZMS), ImageElm(x, AsSSortedList(RZMS)[y])))));
	
	#JDM need the inverse of hom, see comments in IsomorphismPermGroup of RMS
	
	SetIsBijective(hom, true);

	return hom;

else 
	hom := ActionHomomorphism( A, A, OnRight, "surjective" );
	SetIsBijective( hom, true );
	return hom;
fi;

end);

###########################################################################

InstallOtherMethod(IsomorphismPermGroup, "for an H-class of a transformation semigroup", true, 
[IsGreensHClass and IsAssociatedSemigpTransSemigp], 0,
function(hc)
local elms, gp, mapfun;

if not IsGroupHClass( hc )  then
   Error( "can only create isomorphism of group H-classes" );
fi;

elms:=AsSSortedList( hc ); 
gp:=Group(List(elms, PermRepTrans));
mapfun := x-> PermRepTrans(x); 

return 	SemigroupHomomorphismByFunctionNC(hc, Group(SmallGeneratingSet(gp)), mapfun );

end);

###########################################################################

InstallOtherMethod(IsomorphismPermGroup, "for a transformation semigroup", true, 
[IsTransformationSemigroup], 0,
function(S)
local elms, gens, mapfun;

if not IsGroupAsSemigroup(S)  then
   Error( "can only create isomorphism groups for semigroups that are groups" );
fi;

gens:=List(GeneratorsOfSemigroup(S), PermRepTrans);
mapfun := x-> PermRepTrans(x); 

return 	SemigroupHomomorphismByFunctionNC(S, Group(gens), mapfun );

end);


###########################################################################
##
##  <#GAPDoc Label="RightTransversal">
##  <ManSection>
##  <Attr Name="RightTransversal" Arg="autos, inner"/>
##	<Description>
##	returns a transversal of right cosets of the inner automorphisms
##	<C>inner</C> in the group of automorphism <C>autos</C> of a zero group by
##	finding the corresponding right transversal for the underlying group and 
##	applying <Ref Func="ZeroGroupAutomorphism"/>.
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(RightTransversal, [IsAutomorphismGroupOfZeroGroup, IsInnerAutomorphismsOfZeroGroup], 
function(G, H)
local zg;
zg:=Source(Identity(G));
return List(RightTransversal(NiceObject(G), NiceObject(H)), x->  ZeroGroupAutomorphism(zg, x));
end);

###########################################################################
##
##  <#GAPDoc Label="ZeroGroupAutomorphism">
##  <ManSection>
##  <Func Name="ZeroGroupAutomorphism" Arg="ZG, f"/>
##	<Description>
##	converts the group automorphism <C>f</C> of the underlying group of 
##	the zero group <C>ZG</C> into an automorphism of the zero group <C>ZG</C>.
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallGlobalFunction(ZeroGroupAutomorphism,
function( zg, auto)

  local fam, mapfam, map;

  fam :=  ElementsFamily(FamilyObj(zg));
  mapfam := GeneralMappingsFamily(fam,fam);	
  
  auto:=rec(grpauto:=auto);

  Objectify(NewType(mapfam, IsEndoGeneralMapping and 
IsSPGeneralMapping and IsTotal and IsSingleValued and IsInjective and IsSurjective and RespectsMultiplication and RespectsOne and RespectsZero and IsZeroGroupAutomorphismRep), auto);

  SetSource(auto, zg);
  SetRange(auto, zg);
  SetIsInjective(auto, true);
  SetIsSurjective(auto, true);
  SetIsTotal(auto, true);
	
  return auto;
end);

###########################################################################
##
##  <#GAPDoc Label="UnderlyingGroupAutoOfZeroGroupAuto">
##  <ManSection>
##  <Attr Name="UnderlyingGroupAutoOfZeroGroupAuto" Arg="f"/>
##	<Description>
##	returns the underlying group automorphism of the zero group automorphism 
##	<C>f</C>. That is, the restriction of <C>f</C> to its source zero group 
##	without the zero. 
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(UnderlyingGroupAutoOfZeroGroupAuto, "for a zero group automorphism", true, [IsEndoGeneralMapping and IsZeroGroupAutomorphismRep], 0, x-> x!.grpauto);

###########################################################################
##
##	The functions below are the usual functions for dealing with zero group
##	automorphisms.
##

InstallMethod(ImagesRepresentative, "for a zero group automorphism",FamSourceEqFamElm, 
[ IsTotal and IsSingleValued and IsEndoGeneralMapping and IsZeroGroupAutomorphismRep, IsZeroGroupElt ],
function( aut, x)

if IsMultiplicativeZero(x) then 
	return x;
fi;

return ZeroGroupElt(ImagesRepresentative(aut!.grpauto, UnderlyingGroupEltOfZGElt(x)));

end);

#############

InstallMethod(PreImagesRepresentative,   "for a zero group automorphism", 
FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
	IsEndoGeneralMapping and IsZeroGroupAutomorphismRep, IsZeroGroupElt ],
function(aut, x)

if IsMultiplicativeZero(x) then 
          return x;
fi;

return ImagesRepresentative(aut!.grpauto^-1, UnderlyingGroupEltOfZGElt(x));

end);

#############

InstallMethod(ImagesElm, "for a zero group automorphism",
      FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
	IsEndoGeneralMapping and IsZeroGroupAutomorphismRep, IsZeroGroupElt ],
      function( triple, x)
	
      return [ImagesRepresentative(triple, x)];
end);

#############

InstallMethod(CompositionMapping2, "for objects in IsZeroGroupAutomorphismRep", IsIdenticalObj, [IsEndoGeneralMapping and IsZeroGroupAutomorphismRep, IsEndoGeneralMapping and IsZeroGroupAutomorphismRep], 0,
function(a1, a2)

return ZeroGroupAutomorphism(Source(a1), a1!.grpauto*a2!.grpauto);

end);

#############

InstallMethod(InverseGeneralMapping, "for objects in IsZeroGroupAutomorphismRep", true, [IsEndoGeneralMapping and IsZeroGroupAutomorphismRep],  0,
function(a)

return ZeroGroupAutomorphism(Source(a), a!.grpauto^-1);

end);

#############

InstallMethod(\=, "for objects in IsZeroGroupAutomorphismRep", IsIdenticalObj, [IsZeroGroupAutomorphismRep, IsZeroGroupAutomorphismRep],  0,
function(a1, a2)

return ForAll(GeneratorsOfMonoid(Source(a1)), 
   x -> ImageElm(a1,x) = ImageElm(a2,x));

end);

#############

InstallMethod(\<, "for objects in IsZeroGroupAutomorphismRep", IsIdenticalObj, [IsZeroGroupAutomorphismRep, IsZeroGroupAutomorphismRep],  0,
function(a1, a2)

return a1!.grpauto<a2!.grpauto;

end);


#############


#############################################################################
##
#M  InnerAutomorphismNC( <G>, <g> ) . . . . . . . . . . .  inner automorphism
##

InstallMethod( InnerAutomorphismOfSemigroupNC, "for trans. semigroup and permutation", true, [ IsTransformationSemigroup, IsPerm ], 0,
function( S, g )
local fam, hom;
fam:= ElementsFamily( FamilyObj( S ) );
hom:= Objectify( NewType( GeneralMappingsFamily( fam, fam ), IsInnerAutomorphismOfSemigroup and IsSPGeneralMapping and IsSingleValued and IsTotal and IsInjective and IsSurjective and RespectsMultiplication and IsAttributeStoringRep ), rec() );

SetConjugatorOfInnerAutomorphismOfSemigroup( hom, g );
SetSource( hom, S );
SetRange(  hom, S );

return hom;
end );

#############

InstallMethod( InnerAutomorphismOfSemigroup, "for trans. semigroup and permutation", true, [ IsTransformationSemigroup, IsPerm ], 0,
function( S, g )
local hom, gens, imgs, new;

hom:=InnerAutomorphismOfSemigroupNC(S, g);

if g=() then 
	return hom;
fi;

gens:=GeneratorsOfSemigroup(S);
imgs:=OnTuples(gens, hom);

if ForAll(imgs, x-> x in S) then 
	new:=Semigroup(imgs);
	if ForAll(GeneratorsOfSemigroup(S), x-> x in new) then 
		return hom;
	fi;
fi;

return fail;
end);

InstallMethod( InverseGeneralMapping,
    "for inner automorphism of semigroup",
    true,
    [ IsInnerAutomorphismOfSemigroup ], 0,
    inn -> InnerAutomorphismOfSemigroupNC( Source( inn ), 
                     Inverse( ConjugatorOfInnerAutomorphismOfSemigroup( inn ) ) ) );

#############################################################################
##
#M  ImagesRepresentative( <hom>, <g> )  . . . . .  for conjugator isomorphism
##

InstallMethod( ImagesRepresentative,
    "for inner automorphism of semigroup",
    FamSourceEqFamElm,
    [ IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup, IsTransformation ], 0,
    function( hom, g )
    return g ^ ConjugatorOfInnerAutomorphismOfSemigroup( hom );
    end );


InstallMethod(ImagesElm, "for semigroup homomorphism by function",
      FamSourceEqFamElm, [IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup, IsTransformation],
      function( hom, x)
	
      return [ImagesRepresentative(hom, x)];
end);

#############################################################################
##
#M  PreImagesRepresentative( <hom>, <g> ) . . . .  for conjugator isomorphism
##

InstallMethod( PreImagesRepresentative,
    "for inner automorphism of semigroup",
    FamRangeEqFamElm,
    [ IsInnerAutomorphismOfSemigroup, IsTransformation ], 0,
    function( hom, g )
    return g ^ ( ConjugatorOfInnerAutomorphismOfSemigroup( hom ) ^ -1 );
    end );

#####################

InstallMethod( CompositionMapping2,
    "for inner automorphisms of semigroups and semigroup homomorphism",
    IsIdenticalObj,
    [ IsInnerAutomorphismOfSemigroup, IsSemigroupHomomorphism ], 0,
    function( inn, hom )
    if not IsIdenticalObj( Source( inn ), Source( hom ) )  then
      return fail;
    elif IsOne(hom) then 
    	return inn;
    elif IsOne(inn) then 
    	return hom;
    fi;

    if IsMonoid(Source(hom)) then 
    	return  SemigroupHomomorphismByImagesOfGensNC( Source( hom ),
    	Range(inn), OnTuples(OnTuples(GeneratorsOfMonoid(Source(hom)), hom ), inn));
		else
			return  SemigroupHomomorphismByImagesOfGensNC( Source( hom ),
    	Range(inn), OnTuples(OnTuples(GeneratorsOfSemigroup(Source(hom)), hom ) , inn));
		fi;

    end );
    
#####################

    InstallMethod( CompositionMapping2,
    "for  semigroup homomorphism and inner automorphisms of semigroups",
    IsIdenticalObj,
    [ IsSemigroupHomomorphism , IsInnerAutomorphismOfSemigroup], 0,
    function( hom, inn )
    if not IsIdenticalObj( Source( inn ), Source( hom ) )  then
      return fail;
    elif IsOne(hom) then 
    	return inn;
    elif IsOne(inn) then 
    	return hom;
    fi;
   
    if IsMonoid(Source(inn)) then 	   
	    return SemigroupHomomorphismByImagesOfGensNC( Source( inn ),
    	Range(hom), OnTuples(OnTuples(GeneratorsOfMonoid(Source(inn)), inn), hom));
		else 
			 return SemigroupHomomorphismByImagesOfGensNC( Source( inn ),
    	Range(hom), OnTuples(OnTuples(GeneratorsOfSemigroup(Source(inn)), inn), hom));
   	fi;
    end );
   

#####################

    InstallMethod( CompositionMapping2,
    "for two inner automorphisms of semigroups",
    IsIdenticalObj,
    [ IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup , IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup ], 0,
    function( inn1, inn2 )
    if not IsIdenticalObj( Source( inn1 ), Source( inn2 ) )  then
      return fail;
    fi;
    return InnerAutomorphismOfSemigroupNC( Source( inn1 ),
                 ConjugatorOfInnerAutomorphismOfSemigroup( inn2 )
               * ConjugatorOfInnerAutomorphismOfSemigroup( inn1 ) );
    end );

#####################

InstallMethod(\<, "for semigroup homomorphism by images of gens. and inner automorphism", IsIdenticalObj, [IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep, IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup],  0,
function(hom, inn)

if IsMonoid(Source(inn)) then 
	return OnTuples(GeneratorsOfMonoid(Source(inn)), inn)>hom!.imgsgens;
else
	return OnTuples(GeneratorsOfSemigroup(Source(inn)), inn)>hom!.imgsgens;
fi;

end);

#####################

InstallMethod(\<, "inner automorphism and for semigroup homomorphism by images of gens. ", IsIdenticalObj, [IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup, IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep], 0,
function(inn, hom)

if IsMonoid(Source(inn)) then 
	return OnTuples(GeneratorsOfMonoid(Source(inn)), inn)<hom!.imgsgens;
else
	return OnTuples(GeneratorsOfSemigroup(Source(inn)), inn)<hom!.imgsgens;
fi;

end);

#####################

InstallMethod(\<, "inner automorphism and inner automorphism", IsIdenticalObj, [IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup, IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup], 0,
function(inn1, inn2)

return OnTuples(GeneratorsOfSemigroup(Source(inn1)), inn1)<OnTuples(GeneratorsOfSemigroup(Source(inn2)), inn2);

end);

#####################

InstallMethod(\=, "inner automorphism and inner automorphism", IsIdenticalObj, [IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup, IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup], 0,
function(inn1, inn2)

return OnTuples(GeneratorsOfSemigroup(Source(inn1)), inn1)=OnTuples(GeneratorsOfSemigroup(Source(inn2)), inn2);

end);

#####################

InstallMethod(\=, "inner automorphism and for semigroup homomorphism by images of gens. ", IsIdenticalObj, [IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup, IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep], 0,
function(inn, hom)

if IsMonoid(Source(inn)) then 
	return OnTuples(GeneratorsOfMonoid(Source(inn)), inn)=hom!.imgsgens;
else
	return OnTuples(GeneratorsOfSemigroup(Source(inn)), inn)=hom!.imgsgens;
fi;

end);

#####################

InstallMethod(\=, "for semigroup homomorphism by images of gens. and inner automorphism", IsIdenticalObj, [IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep, IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup], 0,
function(hom, inn)

if IsMonoid(Source(inn)) then 
	return OnTuples(GeneratorsOfMonoid(Source(inn)), inn)=hom!.imgsgens;
else
	return OnTuples(GeneratorsOfSemigroup(Source(inn)), inn)=hom!.imgsgens;
fi;
end);

#####################

InstallMethod(One, "for inner automorphisms of semigroup", true, 
[IsInnerAutomorphismOfSemigroup], 0, x -> InnerAutomorphismOfSemigroupNC(Source(x), ()));

#####################

InstallMethod(IsOne,  "for inner automorphism of semigroup", true, [IsInnerAutomorphismOfSemigroup], 0, 
		function(inn)
		
		return IsOne(ConjugatorOfInnerAutomorphismOfSemigroup(inn));
		end);
		

#####################

InstallMethod( PrintObj,
    "for inner automorphism of semigroup",
    true,
    [ IsInnerAutomorphismOfSemigroup ], 0,
    function( inn )
    Print( "InnerAutomorphism( ", Source( inn ), ", ",
           ConjugatorOfInnerAutomorphismOfSemigroup( inn ), " )" );
    end );

#####################

InstallMethod( ViewObj, "for inner auto. of trans. semigroup",
   [IsInnerAutomorphismOfSemigroup],   
   function( hom )
      Print( "^", ConjugatorOfInnerAutomorphismOfSemigroup(hom) );
end );

###########################################################################
##
#M	IsomorphismSemigroups( <RZMS>, <ZS> );
#M	IsomorphismSemigroups( <ZS>, <RZMS> );
##	
##	JDM not yet implemented. also add method for RMS and group

#InstallOtherMethod(IsomorphismSemigroups,  "for a RZMS and ZS", true, 
#[IsReesZeroMatrixSemigroup, IsZeroSemigroup], 0,
#function(S1, S2)
#
#if ForAll(SandwichMatrixOfReesZeroMatrixSemigroup(S1), 
#          row -> ForAll(row, x-> x=MultiplicativeZero(
#              UnderlyingSemigroupOfReesMatrixSemigroup(S1)))) 
#   and Size(S1)=Size(S2) then
#
#   return SemigroupHomomorphismByImagesOfGensNC(S1, S2, List([1..Length
# (GeneratorsOfSemigroup(S1))], x-> AsList(S2)[x]);
#
#end);

#EOF