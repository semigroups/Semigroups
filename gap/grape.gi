##
## grape.gi
## Version 3.1.3
## Fri  7 Nov 2008 17:45:12 GMT
##

## The functions in this file require the grape package to be loaded and are not 
## read by MONOID if grape is not available.

## there are methods for PartialOrderOfDClasses, AutomorphismGroup (of a RZMS), 
## the main function AutomorphismsSemigroupInGroup, RightTransStabAutoGroup (for 
## a RZMS), RZMSInducedFunction, RZMStoRZMSInducedFunction, RZMSGraph, 
## IsomorphismSemigroups (for RZMS and RZMS).

#################
# from greens.gi#
#################

#############################################################################
##
##	<#GAPDoc Label="PartialOrderOfDClasses">
##	<ManSection>
##	<Attr Name="PartialOrderOfDClasses" Arg="S"/>
##	<Description>
##	returns the partial order of the <C>D</C>-classes of <C>S</C> as a directed 
##	graph in <Package>GRAPE</Package> using the command 
##	<Display>
##	Graph(Group(()), [1..Length(GreensDClasses(S))], OnPoints, function(x,y)
##	return y in poset[x]; end, true); ;
##	</Display>
##	where <C>y</C> in <C>poset[x]</C> if and only if 
##	<M>S^1yS^1\subseteq S^1 xS^1</M>.
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(PartialOrderOfDClasses, "for a generic semigroup", true, [IsSemigroup], 
function(M)
local class, poset, a, i, j, c;

class:= GreensDClasses(M);  
poset:= List([1..Length(class)], x->[]);

for i in [1..Length(class)] do
	AddSet(poset[i], i);
	for a in GeneratorsOfSemigroup(M) do
		for c in GreensRClasses(class[i]) do
			AddSet(poset[i], PositionProperty(class, x-> a * Representative(c) in x));
		od;
		for c in GreensLClasses(class[i]) do
			AddSet(poset[i], PositionProperty(class, x-> Representative(c) * a in x));
		od;
	od;
od;

#transitive closure

for i in [1..Length(class)] do
	for j in [1..Length(class)] do
		if j in poset[i] then 
			poset[i]:=Union(poset[j], poset[i]);
		fi;
	od;
od;


#return poset;
return Graph(Group(()), [1..Length(class)], OnPoints, function(x,y) return y in poset[x]; end, true); ;

end);

################
# from autos.gi#
################

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

InstallMethod(AutomorphismGroup, "for a RZMS", true, [IsReesZeroMatrixSemigroup], 0,
function(rms)
local mat, m, n, autograph, r, autogroup, inner, transversal1, ZG, G, transversal2, A, B, l, t, tup, new, components, i, g, transversal3;

mat:=SandwichMatrixOfReesZeroMatrixSemigroup(rms);
m:=Length(mat[1]); n:=Length(mat);

Info(InfoAutos, 2, "computing automorphism group of graph");
autograph:=AutGroupGraph(RZMSGraph(rms), [[1..m],[m+1..n+m]]);
Info(InfoAutos, 2, "the automorphism group of the graph has size ", Size(autograph));
components:=ConnectedComponents(RZMSGraph(rms));
r:=Length(components);
Info(InfoAutos, 2, "the graph has ", r, " connected components");

ZG:=UnderlyingSemigroupOfReesZeroMatrixSemigroup(rms);

Info(InfoAutos, 2, "computing automorphism group of underlying zero group");
autogroup:=AutomorphismGroup(ZG);
Info(InfoAutos, 2, "the automorphism group of underlying zero group has size ", Size(autogroup));

Info(InfoAutos, 3, "computing inner automorphisms of underlying zero group");
if not IsTrivial(autogroup) then 
	inner:=InnerAutomorphismsAutomorphismGroup(autogroup);
	transversal1:=RightTransversal(autogroup, inner);
else
	transversal1:=Elements(autogroup);
fi;
Info(InfoAutos, 2, "Aut G/Inn G has size ", Length(transversal1));

G:=UnderlyingGroupOfZG(ZG);

transversal2:=List(RightTransversal(G, Centre(G)), ZeroGroupElt);
G:=List(G, ZeroGroupElt);

Info(InfoAutos, 2, "|G/Z(G)|+", r-1, "|G| equals ", Length(transversal2)+(r-1)*Size(G));

Info(InfoAutos, 2, "search space has ", Size(autograph), "x", Length(transversal1), "x", Length(transversal2)+(r-1)*Size(G), "=", Size(autograph)*Length(transversal1)*(Length(transversal2)+(r-1)*Size(G)), " elements");
A:=[];

for l in autograph do 

	for t in transversal1 do 
	
		B:=List([1..r], x->[]);
		for i in [1..r] do 
			if not i=1 then 
				transversal3:=G;
			else 
				transversal3:=transversal2;#List(RightTransversal(G, Centre(G)), ZeroGroupElt);
			fi;
			for g in transversal3 do 
				new:=RZMSInducedFunction(rms, l, t, g, components[i]); 	
				if not new=fail then
					Add(B[i], new);
				fi;
			od;
		od;
		Append(A, List(Cartesian(B), x-> RZMSIsoByTriple(rms, rms, [l, t, Sum(x)])));
	od;
od;

A:=Group(A);
SetAsList(A, GeneratorsOfGroup(A));
SetIsAutomorphismGroup(A, true);
SetIsGroupOfAutomorphisms(A, true);
SetIsAutomorphismGroupOfRZMS(A, true);
SetIsFinite(A, true);

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

InstallMethod(AutomorphismsSemigroupInGroup, "for a semigroup", true, [IsTransformationSemigroup, IsObject, IsList], 0,
function(S, superlist, bvals)

local iso, autos1, autos2, gens, dclasses, pos, info, size, regu, left, righ, idem, colours, i, j, k, l, poset, c, RMS, isom, orbits, orb, part, new, rms, good, bad, mappings, cart, map, hom, image, autos, OnSets3, n, m, stop, inner, action, constants, allinner, x, id, pos_list, subgens, A, r, grp, R, g, y, G, H, homs, userint, useryn, doit, perms, stab, counter;

if not ForAll(bvals, IsBool) then
	Error("3rd argument must be a list of 5 boolean variables"); 
	return fail;
fi;
if not (IsGroup(superlist) or superlist=fail) then 
	Error("<superlist> should be a group or fail");
	return fail;
fi;

#########

if IsMonoid(S) then 
	gens:=GeneratorsOfMonoid(S);
else
	gens:=GeneratorsOfSemigroup(S); 
fi;

if not superlist=fail and HasAutomorphismGroup(S) then 
	Info(InfoAutos, 2, "##################################################################");
	Info(InfoAutos, 2, "AutomorphismGroup: finding the intersection of the super group and the \n#I  automorphism group"); 
	id:=SemigroupHomomorphismByImagesOfGensNC(S, S, gens);
	SetIsOne(id, true);
	new:=Group(id);
	SetAsSSortedList(new, [id]);
	
	for x in AutomorphismGroup(S) do
		if x in superlist and not x in new then 
			new:=ClosureGroupDefault(new, x);
		fi;
	od;
	
	SetIsGroupOfAutomorphisms(new, true);
	SetIsFinite(new, true);
	return new;
fi;
	

if Size(S)<10 and HasMultiplicationTable(S) then 
	Info(InfoAutos, 1, "AutomorphismGroup: semigroup is small");
	action:=function(table, perm)
		return List(Permuted(List(table, x-> OnTuples(x, perm)), perm), x->		Permuted(x, perm));
	end;
	
	autos1:=Stabilizer(SymmetricGroup(Size(S)), MultiplicationTable(S), action);
	
	if not superlist=fail then 

		Info(InfoAutos, 2, "######################################################################");
		Info(InfoAutos, 2, "AutomorphismGroup: finding the intersection of the super group and the \n#I  automorphism group"); 
		new:=Group(());
		SetAsSSortedList(new, [()]);
	
		for x in autos1 do
			if not x in new and SemigroupHomomorphismByImagesNC(S, S, Permuted(Elements(S), x)) in superlist then 
				new:=ClosureGroupDefault(new, x);
			fi;
		od;
	fi;

	if not IsTrivial(autos1) then  
		autos2:=Group(List(GeneratorsOfGroup(autos1), x-> SemigroupHomomorphismByImagesNC(S, S, Permuted(Elements(S), x))));
		hom:=GroupHomomorphismByImagesNC(autos2, autos1, GeneratorsOfGroup(autos2), GeneratorsOfGroup(autos1));
		SetInverseGeneralMapping(hom, GroupHomomorphismByImagesNC(autos1, autos2, GeneratorsOfGroup(autos1), GeneratorsOfGroup(autos2)));
		SetNiceMonomorphism(autos2, hom);
		SetIsHandledByNiceMonomorphism(autos2, true);
		SetNiceObject(autos2, Range(NiceMonomorphism(autos2)));
		UseIsomorphismRelation(autos2, autos1);
	else
		autos2:=Group(IdentityMapping(S));
		SetSize(autos2, 1);
		SetIsTrivial(autos2, true);
	fi;
	
	SetIsAutomorphismGroup(autos2, true);
	SetIsGroupOfAutomorphisms(autos2, true);
	SetAutomorphismGroup(S, autos2);
	SetIsAutomorphismGroupOfSemigroup(autos2, true);
	SetIsFinite(autos2, true);
	
	if IsBound(new) and not IsTrivial(autos2) then 
		
		autos2:=Group(List(GeneratorsOfGroup(new), x-> SemigroupHomomorphismByImagesNC(S, S, Permuted(Elements(S), x))));
		hom:=GroupHomomorphismByImagesNC(autos2, new, GeneratorsOfGroup(autos2), GeneratorsOfGroup(new));
		SetInverseGeneralMapping(hom, GroupHomomorphismByImagesNC(new, autos2, GeneratorsOfGroup(new), GeneratorsOfGroup(autos2)));
		SetNiceMonomorphism(autos2, hom);
		SetIsHandledByNiceMonomorphism(autos2, true);
		SetNiceObject(autos2, Range(NiceMonomorphism(autos2)));
		UseIsomorphismRelation(autos2, new);
		
		SetIsFinite(autos2, true);
		SetIsGroupOfAutomorphisms(autos2, true);
	fi;
	
	return autos2;

#########

elif IsCompletelySimpleSemigroup(S) then 

	Info(InfoAutos, 1, "AutomorphismGroup: semigroup is completely simple");

	Info(InfoAutos, 2, "AutomorphismGroup: computing an isomorphic Rees Matrix semigroup");
	iso:=IsomorphismReesMatrixSemigroup(S);

	Info(InfoAutos, 2, "AutomorphismGroup: computing the automorphism group of \n#I  the Rees Matrix semigroup");
	autos1:=AutomorphismGroup(Range(iso));
	
	if not superlist=fail then 
		Info(InfoAutos, 2, "######################################################################");
		Info(InfoAutos, 2, "AutomorphismGroup: finding the intersection of the super group and the \n#I  automorphism group"); 
		autos1:=Group(Filtered(autos1, x-> iso*x*InverseGeneralMapping(iso) in superlist));
	fi;

	Info(InfoAutos, 3, "AutomorphismGroup: group with size ", Size(autos1), " and ", Length(GeneratorsOfGroup(autos1)), " generators");

	Info(InfoAutos, 2, "AutomorphismGroup: conjugating the generators of the automorphism group");
	
	gens:=List(SmallGeneratingSet(autos1), function(x)
	local new;
		new:=iso*x*InverseGeneralMapping(iso);
		SetIsBijective(new, true);
		SetInverseGeneralMapping(new, iso*x^-1*InverseGeneralMapping(iso));
		return new; end);

	#JDM would it be better to use ConjugateGroup here?? Can't just now since
	#JDM IsCollsElms seems to fail; whatever it means!
	
	autos2:=Group(gens);
	UseIsomorphismRelation(autos2, autos1);
	if superlist=fail then 
		SetIsAutomorphismGroup(autos2, true);
		SetIsAutomorphismGroupOfSimpleSemigp(autos2, true);
		SetIsAutomorphismGroupOfSemigroup(autos2, true);
	fi;
	SetIsGroupOfAutomorphisms(autos2, true);
	SetSize(autos2, Size(autos1));
	SetIsFinite(autos2, true);

	return autos2;

#########

else
	
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

	Info(InfoAutos, 1, "AutomorphismGroup: semigroup is not completely simple");
	
	n:=DegreeOfTransformationSemigroup(S);
	constants:=Set(Flat(GradedImagesOfTransSemigroup(S)[1]));
	Info(InfoAutos, 2, "AutomorphismGroup: ", Length(constants), " constants");
	allinner:=false;
	bad:=[];
	
	id:=SemigroupHomomorphismByImagesOfGensNC(S, S, gens);
	SetIsOne(id, true);
	autos:=Group(id);
	SetAsSSortedList(autos, [id]);
	
	if InfoLevel(InfoAutos)=4 and not Length(constants)=0 then
		Info(InfoAutos, 4, "##################################################################");
		Info(InfoAutos, 4, "AutomorphismGroup: Check if all automorphisms are inner? (y/n/h) #"); 
		Info(InfoAutos, 4, "##################################################################");
		bvals[1]:=useryn("It is advisable to answer yes if the size of the semigroup is not \n#I  too large.");
	elif Length(constants)=0 then
		allinner:=false;
		bvals[1]:=false;
	#else
	#	doit:=false;
	fi;
	
	if bvals[1] then 
		
		allinner:=true; 
	
		Info(InfoAutos, 2, "AutomorphismGroup: checking if all automorphisms are inner");
		#JDM should really be an enumerator in the code, then 
		#JDM this would all get simpler.
		if not Length(constants)=n and not HasAsSSortedList(S) then 
			action:=[];
			i:=0;
			repeat
				i:=i+1;
				R:=GreensData(GreensRClasses(S)[i]);
				grp:= R!.schutz;
				x:= R!.rep;

				j:=0;
				repeat
					j:=j+1;
					m:=R!.perms[j];
					k:=0;
					repeat
						k:=k+1;
						g:=AsList(grp)[k];
						new:=x*(g*m^-1);
						new:=new![1]{constants};
						if not new in action then 
							AddSet(action, new);
						else
							allinner:=false;
						fi;
					until k=Size(grp) or allinner=false;
				until j=Length(R!.perms) or allinner=false;
			until i=Length(GreensRClasses(S)) or allinner=false;
		elif not Length(constants)=n and HasAsSSortedList(S) then 
			action:=[];
			i:=0;
			repeat
				i:=i+1;
				new:=AsSSortedList(S)[i]![1]{constants};
				if not new in action then 
					AddSet(action, new);
				else
					allinner:=false;
				fi;
			until allinner=false or i=Size(S);
		fi;
		#JDM 
		
		Info(InfoAutos, 2, "AutomorphismGroup: ", allinner);
		
	fi; #check if all autos are inner
	
	if allinner and HasInnerAutomorphismsOfSemigroup(S) then 
		inner:=InnerAutomorphismsOfSemigroup(S);
		SetIsAutomorphismGroupOfSemigroup(inner, true);
		SetIsGroupOfAutomorphisms(inner, true);
		SetIsAutomorphismGroup(inner, true);
		
		return inner;
	elif not allinner then 

		Info(InfoAutos, 2, "AutomorphismGroup: computing D-classes");
		dclasses:=GreensDClasses(S);
		Info(InfoAutos, 2, "AutomorphismGroup: semigroup has size ", Size(S));
		Info(InfoAutos, 2, "AutomorphismGroup: ", Length(dclasses), " D-classes in total");

		#positions of D-classes containing generators

		pos:=SetX(gens, x-> PositionProperty(dclasses, y-> x in y));

		Info(InfoAutos, 2, "AutomorphismGroup: ", Length(pos), " D-classes of generators");

		info:=List(dclasses{pos}, c-> [Size(c), IsRegularDClass(c), 
		Length(GreensLClasses(c)), Length(GreensRClasses(c)), 
		Length(Idempotents(c))]);

		size:=SetX(dclasses{pos}, Size);
		regu:=SetX(dclasses{pos}, IsRegularDClass);
		left:=SetX(dclasses{pos}, c-> Length(GreensLClasses(c)));
		righ:=SetX(dclasses{pos}, c-> Length(GreensRClasses(c)));
		idem:=SetX(dclasses{pos}, c-> Length(Idempotents(c)));

		colours:=List([1..Length(pos)], x-> []);
		Info(InfoAutos, 2, "AutomorphismGroup: partitioning D-classes");

		for i in [1..Length(dclasses)] do 
			c:=dclasses[i];

			if Size(c) in size and IsRegularDClass(c) in regu and 
				Length(Idempotents(c)) in idem and 
				Length(GreensLClasses(c)) in left and 
				Length(GreensRClasses(c)) in righ then 

				j:=Position(info, [Size(c), IsRegularDClass(c), 
				Length(GreensLClasses(c)), Length(GreensRClasses(c)),
				Length(Idempotents(c))]);
			
				if not j=fail then 
					Add(colours[j], i);
				fi;
			fi;
		od;

		colours:=Filtered(colours, x-> not x=[]);

		Info(InfoAutos, 2, "AutomorphismGroup: computing isomorphisms to Rees Matrix Semigroups \n#I  for D-classes of generators");

		RMS:=List([1..Length(dclasses)], function(x)
				if x in pos then 
					return IsomorphismReesMatrixSemigroupOfDClass(dclasses[x]);
				else
					return fail; 
				fi; end);

		isom:=List([1..Length(dclasses)], x-> List([1..Length(dclasses)],    
			function(y)
			if x in pos and y=x then 
				return IdentityMapping(Range(RMS[x]));
			else
				return 0;
			fi; end));

		if ForAll(colours, x-> Length(x)=1) then
			poset:=Group(());
		else
			Info(InfoAutos, 2, "AutomorphismGroup: computing automorphism group of poset of D-classes"); 
			poset:=AutGroupGraph(PartialOrderOfDClasses(S), colours);

		#partition the orbit of the D-class containing each generator w.r.t.
		#isomorphism of D-classes as principal factors  

			if not IsTrivial(poset) then 

				orbits:=Orbits(poset, pos);

				if not ForAll(orbits, x-> Length(x)=1) then 

					Info(InfoAutos, 2, "AutomorphismGroup: partitioning orbits by isomorphism of D-classes"); 
					part:=[];

					for orb in orbits do
						j:=Intersection(orb, pos);
						if not j=[] then 
						
							if Length(orb)=1 then 
								Add(part, orb); orb:=[];
							else
								repeat
									j:=j[1];
									k:=[j];
									
									for i in orb do 

										#rms:=Range(IsomorphismReesMatrixSemigroupOfDClass(dclasses[i]));
									
										if IsGroup(Range(RMS[j])) and IsGroup(Range(RMS[i])) then
											iso:=IsomorphismGroups(Range(RMS[j]), Range(RMS[i]));
										else
											iso:=IsomorphismSemigroups(Range(RMS[j]), Range(RMS[i]));
										fi;
										
										if not iso=fail then       

											AddSet(k, i);
											if isom[j][i]=0 and not i=j then
												isom[j][i]:=iso;
												isom[i][j]:=InverseGeneralMapping(iso);
											fi;
											
										fi;
									od;
									for l in Combinations(Difference(k,[j]), 2) do #JDM better?
										
										if isom[l[1]][l[2]]=0 and not i=j then 
											isom[l[1]][l[2]]:=isom[l[1]][j]*isom[j][l[2]];
											isom[l[2]][l[1]]:=isom[l[2]][j]*isom[j][l[1]];
										fi;
									od;
									Add(part, k);
									orb:=Difference(orb, k);
								until orb=[];
							fi;
						fi;  
					od;
					Info(InfoAutos, 2, "AutomorphismGroup: computing the intersection of stabilizers \n#I  of newly partitioned orbits of D-classes");
					if not part=orbits then  
						poset:=Intersection(List(part, x-> Stabilizer(poset, x, OnSets)));
					fi;
					new:=Concatenation(GeneratorsOfGroup(poset), [()]);
					poset:=Group(List(new, x-> RestrictedPerm(x, Union(part))));
				else 
					poset:=Group(());
				fi;
			fi;
		fi;

		Info(InfoAutos, 2, "AutomorphismGroup: automorphism group of poset of D-classes has size ", Size(poset), "\n#I  and moves ", NrMovedPoints(poset), " points"); 

		pos_list:=List(gens, x-> PositionProperty(dclasses, y-> x in y));
		#do not change the pos_list line! It is necessary.

		mappings:=List([1..Maximum(pos)], x-> [0]);

		for j in pos do 
			subgens:=AsSSortedList(gens{Filtered([1..Length(gens)], x-> pos_list[x]=j)});
			Info(InfoAutos, 3, "AutomorphismGroup: finding automorphism group of D-class ", j);

			if IsGroup(Range(RMS[j])) then 
				Info(InfoAutos, 3, "AutomorphismGroup: D-class is a group with size ", Size(Range(RMS[j])), " of degree ", LargestMovedPoint(Range(RMS[j])));
				A:=AutomorphismGroup(Range(RMS[j]));
				
				if not IsTrivial(A) and IsHandledByNiceMonomorphism(A) then 
					SetEnumerator(A, EnumeratorByFunctions(A, rec(ElementNumber:=function(enum, pos) local B; B:=UnderlyingCollection(enum); return PreImagesRepresentative(NiceMonomorphism(B), Enumerator(NiceObject(B))[pos]); end, 
				NumberElement:=function(enum, elm)
				local B; B:=UnderlyingCollection(enum); 
				return Position(Enumerator(NiceObject(B)), ImageElm(NiceMonomorphism(B), elm)); end )));
				fi;
					
				subgens:=OnTuples(subgens, RMS[j]);
				stab:=Stabilizer(A, subgens, OnTuples); #pointwise stabilizer
				
				if not Size(stab)=1 then
					A:=RightTransversal(A, Stabilizer(A, subgens, OnTuples)); 
				fi;
			else
				Info(InfoAutos, 3, "AutomorphismGroup: D-class is a Rees matrix semigroup with size ", Size(Range(RMS[j])));
				A:=RightTransStabAutoGroup(Range(RMS[j]), OnTuples(subgens, RMS[j]), OnTuples);
				#RightTransversal(AutomorphismGroup(Range(RMS[j])), Stabilizer(AutomorphismGroup(Range(RMS[j])), subgens));
			fi;
			Info(InfoAutos, 3, "AutomorphismGroup: automorphism group of D-class has size ", Size(A));
			mappings[j]:=Enumerator(A);
				
		od;
		
		mappings:=EnumeratorOfCartesian(mappings);
	fi;
	
	if InfoLevel(InfoAutos)=4 and not HasInnerAutomorphismsOfSemigroup(S) then #and not allinner then 
		Info(InfoAutos, 4, "################################################################");
		Info(InfoAutos, 4, "AutomorphismGroup: Try to compute inner automorphisms? (y/n/h) #");
		Info(InfoAutos, 4, "################################################################");

		bvals[2]:=useryn("It might help to find an result more quickly if yes is the answer. \n#I  However, if the degree of the semigroup is large (>20 approx.), \n#I  then the answer should probably be no.");;
	fi;
	
	if HasInnerAutomorphismsOfSemigroup(S) then 
		autos:=InnerAutomorphismsOfSemigroup(S);
	elif bvals[2] then #compute inner autos
		Info(InfoAutos, 3, "AutomorphismGroup: computing stabiliser of images");
		m:=0;

		if not Length(constants)=0 and not Length(constants)=n then 
			inner:=Stabilizer(SymmetricGroup(n), AsSet([constants, Difference([1..n], constants)]), OnSetsSets);
		else
			inner:=SymmetricGroup(n);
		fi;

		stop:=false;

		repeat 
			m:=m+1;
			if not IsEmpty(GradedImagesOfTransSemigroup(S)[m]) then 
				inner:=Stabilizer(inner, Set(GradedImagesOfTransSemigroup(S)[m]), OnSetsSets);
				if IsTrivial(inner) then 
					stop:=true;		
				fi;
			fi;
		until m=n-1 or stop;

		Info(InfoAutos, 4, "AutomorphismGroup: stabilizer has size ", Size(inner));

		if not IsTrivial(inner) then 
			OnSets3:=function(x, g)
				return Set(List( x, i-> OnSetsSets(i, g)));
			end;
	
			Info(InfoAutos, 3, "AutomorphismGroup: computing stabiliser of kernels");

			m:=1;
			stop:=false;
		
			repeat 
				m:=m+1;
				if not IsEmpty(GradedKernelsOfTransSemigroup(S)[m]) then 
					inner:=Stabilizer(inner, Set(GradedKernelsOfTransSemigroup(S)[m]), OnSets3);
					if IsTrivial(inner) then 
						stop:=true;		
					fi;
				fi;
			until m=n-1 or stop;
	
			Info(InfoAutos, 4, "AutomorphismGroup: stabilizer has size ", Size(inner));
		fi; #stabilizer of kernels

		if IsTrivial(inner) then
			Info(InfoAutos, 2, "AutomorphismGroup: inner automorphisms are trivial");
			SetIsInnerAutomorphismsOfSemigroup(autos, true);
			SetInnerAutomorphismsOfSemigroup(S, autos);
			if allinner then
				SetIsAutomorphismGroup(autos, true);
				SetIsGroupOfAutomorphisms(autos, true);
				SetIsAutomorphismGroupOfSemigroup(autos, true);
				return autos; 
			fi;
		else 
			Info(InfoAutos, 3, "AutomorphismGroup: computing the stabilizer of generators under \n#I  the action of the stabilizers of images and kernels");
			G:=OrbitStabilizer(inner, AsSet(gens), OnSets); #setwise stabilizer
			H:=OrbitStabilizer(G.stabilizer, gens, OnTuples); #pointwise stabilizer

			if not allinner then 
				Info(InfoAutos, 2, "AutomorphismGroup: the outer automorphism search space contains \n#I  ", Length(mappings)*Size(poset), " elements");
			fi;
			Info(InfoAutos, 2, "AutomorphismGroup: the inner automorphism search space contains \n#I  ", Length(G.orbit), " elements");

			if InfoLevel(InfoAutos)=4 then 

				Info(InfoAutos, 4, "####################################################################");
				Info(InfoAutos, 4, "AutomorphismGroup: proceed with inner automorphism search? (y/n/h) #"); 
				Info(InfoAutos, 4, "####################################################################");
				doit:=useryn("If the size of the inner search space is comparable or smaller in size \n#I  to that of the outer search space, then it is probably best to answer \n#I  yes. If the size of the inner space is much bigger than the outer, then  \n#I  it will probably be quicker to answer no.");;
			elif allinner or Length(mappings)*Size(poset)<=2*Length(G.orbit) or Length(G.orbit)<=Length(mappings)*Size(poset) then 
				doit:=true;
			else
				doit:=false;
			fi;

			if doit then 
				
				perms:=AsList(RightTransversal(inner, G.stabilizer));
				orb:=RightTransversal(G.stabilizer, H.stabilizer);
				
				if InfoLevel(InfoAutos)=4 and not superlist=fail then 
					Info(InfoAutos, 4, "AutomorphismGroup: the size of the super group is ", Size(superlist));
					Info(InfoAutos, 4, "######################################################################");
					Info(InfoAutos, 4, "AutomorphismGroup: Filter elements in the super group online? (y/n/h) #");
					Info(InfoAutos, 4, "######################################################################");
					bvals[3]:=useryn("Filtering the elements online is only advisable if the length of \n#I  the super group is comparable or less than the size of the inner search \n#I  space. You will prompted to ask if you want to filter after the \n#I  search for inner automorphisms.");
				elif superlist=fail then 
					bvals[3]:=false;
				fi; 

				Info(InfoAutos, 1, "AutomorphismGroup: computing inner automorphisms");

				l:=0;
				inner:=Group(orb);
				SetAsSSortedList(inner, AsSet(orb));

				repeat
					l:=l+1; 
					x:=perms[l];
					image:=OnTuples(gens, x);
					#JDM consider moving this or changing it to InnerAutomorphismOfSemigroup())
					if not image=gens then
						if ForAll(image, x-> x in S) then 
							hom:=SemigroupHomomorphismByImagesOfGensNC(S, S, image);
							if not x in inner and not x in bad then
								new:=Semigroup(image);
								if ForAll(gens, g-> g in new) then 
									if (bvals[3] and hom in superlist) or not bvals[3] then
										inner:=ClosureGroupDefault(inner, x);
										if not bad=[] then 
											bad:=Union(List(bad, x-> Orbit(inner, x, OnTuples))); 
										fi;
									fi;
								else
									Info(InfoAutos, 4, "AutomorphismGroup: images don't generate");
									bad:=Union(bad, Orbit(inner, image, OnTuples)); 
								fi;
							fi;
						else
							Info(InfoAutos, 4, "AutomorphismGroup: images not all in semigroup");
						fi;
					fi;
					
					Info(InfoAutos, 4, "AutomorphismGroup: automorphisms ", Size(inner), " nonautomorphisms ", Length(bad), " counter ", l);
				until 2*Size(inner)+Length(bad)>Length(perms) or l=Length(perms);
				
				autos:=Group(List(SmallGeneratingSet(inner), x-> InnerAutomorphismOfSemigroupNC(S, x)));
				SetAsSSortedList(autos, SetX(inner, x-> InnerAutomorphismOfSemigroupNC(S, x)));
				SetIsInnerAutomorphismsOfSemigroup(autos, true);
				SetInnerAutomorphismsOfSemigroup(S, autos);
				
				if allinner then 
					SetIsAutomorphismGroup(autos, true);
					SetIsGroupOfAutomorphisms(autos, true);
					SetIsAutomorphismGroupOfSemigroup(autos, true);
					return autos;
				elif IsTrivial(autos) then 
					Info(InfoAutos, 2, "AutomorphismGroup: inner automorphisms are trivial");
				else 
					Info(InfoAutos, 4,	"AutomorphismGroup: inner automorphism group has ", Length(GeneratorsOfGroup(autos)), " generators and size ", Size(autos)); 
				fi;
			fi; 
		fi; 
	fi;  
	
	l:=0;
	Info(InfoAutos, 2, "AutomorphismGroup: the outer automorphism search space contains \n#I  ", Length(mappings)*Size(poset), " elements");
	if InfoLevel(InfoAutos)=4 and not superlist=fail then 
		Info(InfoAutos, 4, "AutomorphismGroup: the length of the super group is ", Size(superlist));
		Info(InfoAutos, 4, "#################################################################");
		Info(InfoAutos, 4, "AutomorphismGroup: In the search for outer automorphisms, filter \n#I  elements in the super group online? (y/n/h) ");
		Info(InfoAutos, 4, "#################################################################");
		bvals[4]:=useryn("Filtering the elements online is probably not advisable unless the \n#I  search space is very big or unless the final automorphism group is \n#I  very large.");
		
	elif superlist=fail then  
		bvals[4]:=false;
	fi; 
	
	if bvals[4] then 
			autos:=Group(Filtered(GeneratorsOfGroup(autos), x-> x in superlist));
	fi;
	
	if InfoLevel(InfoAutos)=4 then
		if superlist=fail then 
			Info(InfoAutos, 4, "#################################################################");
		fi;

		Info(InfoAutos, 4, "AutomorphismGroup: In the search for outer automorphisms, keep \n#I  track of nonautomorphisms? (y/n/h) ");

		Info(InfoAutos, 4, "#################################################################");

		bvals[5]:=useryn("keeping track of nonautomorphisms may make the computation faster \n#I  it may also use more memory. So, if the space is very large, better \n#I  answer no. ");
	fi;

	Info(InfoAutos, 1, "AutomorphismGroup: computing outer automorphisms");
	counter:=0;
	stop:=false;
	poset:=Enumerator(poset);

	repeat

		l:=l+1;
		k:=0;

		while k<Length(poset) and not stop do
			map:=mappings[l];
			counter:=counter+1;
			Info(InfoAutos, 4, "AutomorphismGroup: automorphisms ", Size(autos), " nonautomorphisms ", Length(bad), " counter ", counter);
			if 2*Size(autos)+Length(bad)>Length(mappings)*Size(poset) then 
				stop:=true;
			else 
				k:=k+1;
				i:=poset[k];
				Info(InfoAutos, 4, "AutomorphismGroup: automorphism of poset is: ", i);
				
				for j in pos do
					iso:=isom[j][j^i];
					#iso:=IsomorphismSemigroups(Range(RMS[j]), Range(RMS[j^i]));
					if not IsOne(map[j]) or not IsOne(iso) or not IsOne(i) then 
						map[j]:=RMS[j]*map[j]*iso*InverseGeneralMapping(RMS[j^i]);
					else 
						map[j]:=IdentityMapping(Source(RMS[j]));
					fi;
				od;
				
				image:=List([1..Length(gens)], x-> gens[x]^map[pos_list[x]]);
				map:=SemigroupHomomorphismByImagesOfGensNC(S, S, image);
				#danger this may not be a semigroup homomorphism!!
				
				if not IsOne(map) then 
				if (bvals[4] and map in superlist) or not bvals[4] then
					if not map in autos then 
						hom:=SemigroupHomomorphismByImagesOfGens(S, S, image);
						if not hom=fail then
							if AsSet(image)=gens then 
								new:=gens;
							else
								new:=Semigroup(image);
							fi;
							if ForAll(gens, g-> g in new) then 
								if not image in bad then 
									SetIsBijective(hom, true);
									autos:=ClosureGroupDefault(autos, hom);
								else
									Info(InfoAutos, 4, "AutomorphismGroup: mapping already known to be a nonautomorphism");
								fi;
							else
								Info(InfoAutos, 4, "AutomorphismGroup: images don't generate");
								if bvals[5] then  
									bad:=Union(bad, Orbit(autos, image, OnTuples));
								fi;
							fi;
						else
							Info(InfoAutos, 4, "AutomorphismGroup: relations don't hold");
							if bvals[5] then 
								bad:=Union(bad, Orbit(autos, image, OnTuples));
							fi;
						fi;
					else 
						Info(InfoAutos, 4, "AutomorphismGroup: mapping already known to be an automorphism");
					fi;
				else 
					Info(InfoAutos, 4, "AutomorphismGroup: not in super group");
			fi;
				fi;
			fi;
		od;
	until l=Length(mappings) or stop;
fi;

SetIsAutomorphismGroup(autos, true);
SetIsAutomorphismGroupOfSemigroup(autos, true);

Info(InfoAutos, 4, "AutomorphismGroup: automorphisms ", Size(autos), " nonautomorphisms ", Length(bad), " counter ", counter);

if not bvals[4] and not superlist=fail then 
	SetAutomorphismGroup(S, autos); 
	
	Info(InfoAutos, 2, "##################################################################");
	Info(InfoAutos, 2, "AutomorphismGroup: finding the intersection of the superlist and the \n#I  automorphism group"); 
	new:=Group(id);
	SetAsSSortedList(new, [id]);
	
	for x in autos do
		if x in superlist and not x in new then 
			new:=ClosureGroupDefault(new, x);
		fi;
	od;
	autos:=new;
fi;

SetIsFinite(autos, true);
SetIsGroupOfAutomorphisms(autos, true);
return autos;

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
#############################################################################
##	JDM for the example given in the manual example this appears to run more 
##	slowly than just running automorphism group (although the overall 
##	computation is longer)

InstallOtherMethod(RightTransStabAutoGroup, "for a RZMS", true, [IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroupElementCollection, IsFunction], 0,
function(rms, elts, func)
local mat, m, n, autograph, r, autogroup, inner, transversal1, ZG, G, transversal2, A, l, t, tup, new, trans, tuples, rmsgens, indices, components, transversal3, x, i, g, B;

if not ForAll(elts, x-> x in rms) then 
	Error("<elts> must all lie in <rms>");
	return fail;
fi; #JDM make NC version? to omit this?

mat:=SandwichMatrixOfReesZeroMatrixSemigroup(rms);
m:=Length(mat[1]); n:=Length(mat);

Info(InfoAutos, 2, "computing automorphism group of graph");
autograph:=AutGroupGraph(RZMSGraph(rms), [[1..m],[m+1..n+m]]);
Info(InfoAutos, 2, "the automorphism group of the graph has size ", Size(autograph));

components:=ConnectedComponents(RZMSGraph(rms));
r:=Length(components);
Info(InfoAutos, 2, "the graph has ", r, " connected components");

ZG:=UnderlyingSemigroupOfReesZeroMatrixSemigroup(rms);

Info(InfoAutos, 2, "computing automorphism group of underlying zero group");
autogroup:=AutomorphismGroup(ZG);
Info(InfoAutos, 4, "the automorphism group of underlying zero group has size ", Size(autogroup));

Info(InfoAutos, 3, "computing inner automorphisms of underlying zero group");
if not IsTrivial(autogroup) then 
	inner:=InnerAutomorphismsAutomorphismGroup(autogroup);
	transversal1:=RightTransversal(autogroup, inner);
else
	transversal1:=Elements(autogroup);
fi;
Info(InfoAutos, 2, "Aut G/Inn G has size ", Length(transversal1));

G:=UnderlyingGroupOfZG(ZG);
transversal2:=List(RightTransversal(G, Centre(G)), ZeroGroupElt);
G:=List(G, ZeroGroupElt);

Info(InfoAutos, 2, "|G/Z(G)|+", r-1, "|G| equals ", Length(transversal2)+(r-1)*Size(G));

Info(InfoAutos, 2, "search space has ", Size(autograph), "x", Length(transversal1), "x", Length(transversal2)+(r-1)*Size(G), "=", Size(autograph)*Length(transversal1)*(Length(transversal2)+(r-1)*Size(G)), " elements");

trans:=[];
tuples:=[];

m:=0;
for l in autograph do 

	for t in transversal1 do 
	
		B:=List([1..r], x->[]);
		for i in [1..r] do 
			if not i=1 then 
				transversal3:=G;
			else 
				transversal3:=transversal2;
			fi;
			for g in transversal3 do 
				new:=RZMSInducedFunction(rms, l, t, g, components[i]); 	
				if not new=fail then
					Add(B[i], new);
				fi;
			od;
		od;

		for x in Cartesian(B) do
			new:=RZMSIsoByTriple(rms, rms, [l, t, Sum(x)]);
			tup:=func(elts, new);
			if not tup in tuples then
				AddSet(tuples, tup); 
				Add(trans, new);
			fi;
		od;
		
	od;
od;

return trans;
#JDM return tuples too?
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
local mat, m, n, graph, rep, imagelist, i, j, k, sub, bicomps, edges, defined, orb, v, new, involved, verts, Last, perm;

mat:=SandwichMatrixOfReesZeroMatrixSemigroup(rms);
m:=Length(mat[1]); n:=Length(mat);
graph:=RZMSGraph(rms);

rep:=Minimum(component);
imagelist:=[];
imagelist[rep]:=groupelt;

if Length(component)=Length(Vertices(graph)) then 
	edges:=DirectedEdges(graph);
	bicomps:=Bicomponents(graph);
else
	sub:=InducedSubgraph(graph, component);
	perm:=MappingPermListList(VertexNames(sub), Vertices(sub));
	edges:=OnTuplesTuples(DirectedEdges(sub), perm^-1);
	bicomps:=OnTuplesTuples(Bicomponents(sub), perm^-1);
fi;

defined:=[];
orb:=[rep];  
j:=0;

repeat 

  j:=j+1;
  Last:=orb[j];
 
  involved:=Filtered(edges, x-> x[1]=Last and not x in defined);
  
  if not involved=[] then

    verts:=List(involved, x-> x[2]);
    Append(orb, Filtered(verts, x-> not x in orb));

    for k in [1..Length(verts)] do
      v:=verts[k];
      
      if Last in bicomps[1] then
        new:=mat[v^l-m][Last^l]*imagelist[Last]*(mat[v-m][Last]^g)^-1;
      else
        new:=(mat[Last^l-m][v^l])^-1*imagelist[Last]*(mat[Last-m][v]^g);
      fi; 
      
      if not IsBound(imagelist[v]) then 
        imagelist[v]:=new;
      elif not imagelist[v]=new then
        return fail;
      fi;
      defined:=Union(defined, [involved[k], Reversed(involved[k])]);   
    
    od;
  fi;

until defined=edges;

return imagelist;

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
local mat1, mat2, m, n, rmsgraph, components, reps, imagelist, i, j, k, sub, bicomps, edges, defined, orb, v, new, involved, verts, Last, perm;

mat1:=SandwichMatrixOfReesZeroMatrixSemigroup(rms1);
mat2:=SandwichMatrixOfReesZeroMatrixSemigroup(rms2);
m:=Length(mat1[1]); n:=Length(mat1);
rmsgraph:=RZMSGraph(rms1);
components:=ConnectedComponents(rmsgraph);

if not Length(groupelts)=Length(components) then 
  Error("Must be as many elements as components");
fi;

reps:=List(components, Minimum);
imagelist:=[];
imagelist{reps}:=groupelts;

for i in [1..Length(components)] do 

	if Length(components)=1 then 
  		edges:=DirectedEdges(rmsgraph);
  		bicomps:=Bicomponents(rmsgraph);
  	else
  		sub:=InducedSubgraph(rmsgraph, components[i]);
  		perm:=MappingPermListList(VertexNames(sub), Vertices(sub));
  		edges:=OnTuplesTuples(DirectedEdges(sub), perm^-1);
  		bicomps:=OnTuplesTuples(Bicomponents(sub), perm^-1);
  	fi;

  defined:=[];
  orb:=[reps[i]];  
  j:=0;

  repeat 

  j:=j+1;
  Last:=orb[j];
 
  involved:=Filtered(edges, x-> x[1]=Last and not x in defined);
  
  if not involved=[] then

    verts:=List(involved, x-> x[2]);
    Append(orb, Filtered(verts, x-> not x in orb));

    for k in [1..Length(verts)] do
      v:=verts[k];
      
      if Last in bicomps[1] then
        new:=mat2[v^l-m][Last^l]*imagelist[Last]*(mat1[v-m][Last]^g)^-1;
      else
        new:=(mat2[Last^l-m][v^l])^-1*imagelist[Last]*(mat1[Last-m][v]^g);
      fi; 
      
      if not IsBound(imagelist[v]) then 
        imagelist[v]:=new;
      elif not imagelist[v]=new then
        return fail;
      fi;
      defined:=Union(defined, [involved[k], Reversed(involved[k])]);   
    
    od;
  fi;

  until defined=edges;

od;

return imagelist;

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
local mat, n, m, adj, graph, zero;

mat:=SandwichMatrixOfReesZeroMatrixSemigroup(rms);
n:=Length(mat);
m:=Length(mat[1]);
zero:=MultiplicativeZero(UnderlyingSemigroupOfReesZeroMatrixSemigroup(rms));

adj:=function(x,y)
if x<=m and y>m then 
  return not mat[y-m][x]=zero;
elif x>m and y<=m then 
  return not mat[x-m][y]=zero;
else 
  return false;
fi;
end;

return Graph(Group(()), [1..n+m], OnPoints, adj, true);
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

InstallOtherMethod(IsomorphismSemigroups, "for a RZMS and RZMS", true, [IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroup], 0,
function(R1, R2)
local g1, g2, iso, G1, G2, isograph, isogroup, f, l, g, tup, candidate, mat, m, n;

if not (Size(R1)=Size(R2) and ColumnsOfReesZeroMatrixSemigroup(R1)=ColumnsOfReesZeroMatrixSemigroup(R2) and RowsOfReesZeroMatrixSemigroup(R1)=RowsOfReesZeroMatrixSemigroup(R2)) then 
  return fail;
else

	mat:=SandwichMatrixOfReesZeroMatrixSemigroup(R1);
	m:=Length(mat[1]); n:=Length(mat);

	if R1=R2 then 
		return IdentityMapping(R1);
	else

		g1:=UnderlyingSemigroupOfReesZeroMatrixSemigroup(R1);
		g2:=UnderlyingSemigroupOfReesZeroMatrixSemigroup(R2);
		iso:=IsomorphismSemigroups(g1, g2);

		if iso=fail then 
			return fail;
		else 
			G1:=RZMSGraph(R1); G2:=RZMSGraph(R2);
			isograph:=AutGroupGraph(G1, [[1..m],[m+1..n+m]]);

			if not UndirectedEdges(G1)=UndirectedEdges(G2) then 
				f:=GraphIsomorphism(G1, G2);
				if f=fail then 
					return fail;
				fi;
				isograph:=List(AutGroupGraph(G1), x-> x*f);
			fi;

			#all isomorphisms from g1 to g2
			isogroup:=List(AutomorphismGroup(g1), x->x*iso); 

			#find an induced function, if there is one
			for l in isograph do
				for g in isogroup do
					for tup in Elements(g2) do 
						if not IsMultiplicativeZero(tup) then 
							candidate:=RZMStoRZMSInducedFunction(R1, R2, l, g, [tup]);
							if not candidate=false then 
								return RZMSIsoByTriple(R1, R2, [l, g, candidate]);
							fi;
						fi;
					od;
				od;
			od;
			return fail;
		fi;
	fi;
fi;
end);