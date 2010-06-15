#############################################################################
##
#W  grape.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
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

InstallMethod(PartialOrderOfDClasses, "for a  semigroup", 
[IsSemigroup], 
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

InstallMethod(AutomorphismGroup, "for a RZMS", true, [IsReesZeroMatrixSemigroup], 0,
function(rms)
local mat, m, n, autograph, r, autogroup, inner, transversal1, ZG, G, transversal2, A, B, l, t, tup, new, components, i, g, transversal3;

mat:=SandwichMatrixOfReesZeroMatrixSemigroup(rms);
m:=Length(mat[1]); n:=Length(mat);

Info(InfoMonoidAutos, 2, "computing automorphism group of graph");
autograph:=AutGroupGraph(RZMSGraph(rms), [[1..m],[m+1..n+m]]);
Info(InfoMonoidAutos, 2, "the automorphism group of the graph has size ", Size(autograph));
components:=ConnectedComponents(RZMSGraph(rms));
r:=Length(components);
Info(InfoMonoidAutos, 2, "the graph has ", r, " connected components");

ZG:=UnderlyingSemigroupOfReesZeroMatrixSemigroup(rms);

Info(InfoMonoidAutos, 2, "computing automorphism group of underlying zero group");
autogroup:=AutomorphismGroup(ZG);
Info(InfoMonoidAutos, 2, "the automorphism group of underlying zero group has size ", Size(autogroup));

Info(InfoMonoidAutos, 3, "computing inner automorphisms of underlying zero group");
if not IsTrivial(autogroup) then 
	inner:=InnerAutomorphismsAutomorphismGroup(autogroup);
	transversal1:=RightTransversal(autogroup, inner);
else
	transversal1:=Elements(autogroup);
fi;
Info(InfoMonoidAutos, 2, "Aut G/Inn G has size ", Length(transversal1));

G:=UnderlyingGroupOfZG(ZG);

transversal2:=List(RightTransversal(G, Centre(G)), ZeroGroupElt);
G:=List(G, ZeroGroupElt);

Info(InfoMonoidAutos, 2, "|G/Z(G)|+", r-1, "|G| equals ", Length(transversal2)+(r-1)*Size(G));

Info(InfoMonoidAutos, 2, "search space has ", Size(autograph), "x", Length(transversal1), "x", Length(transversal2)+(r-1)*Size(G), "=", Size(autograph)*Length(transversal1)*(Length(transversal2)+(r-1)*Size(G)), " elements");
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
	Info(InfoMonoidAutos, 2, "##################################################################");
	Info(InfoMonoidAutos, 2, "AutomorphismGroup: finding the intersection of the super group and the \n#I  automorphism group"); 
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
	Info(InfoMonoidAutos, 1, "AutomorphismGroup: semigroup is small");
	action:=function(table, perm)
		return List(Permuted(List(table, x-> OnTuples(x, perm)), perm), x->		Permuted(x, perm));
	end;
	
	autos1:=Stabilizer(SymmetricGroup(Size(S)), MultiplicationTable(S), action);
	
	if not superlist=fail then 

		Info(InfoMonoidAutos, 2, "######################################################################");
		Info(InfoMonoidAutos, 2, "AutomorphismGroup: finding the intersection of the super group and the \n#I  automorphism group"); 
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

	Info(InfoMonoidAutos, 1, "AutomorphismGroup: semigroup is completely simple");

	Info(InfoMonoidAutos, 2, "AutomorphismGroup: computing an isomorphic Rees Matrix semigroup");
	iso:=IsomorphismReesMatrixSemigroup(S);

	Info(InfoMonoidAutos, 2, "AutomorphismGroup: computing the automorphism group of \n#I  the Rees Matrix semigroup");
	autos1:=AutomorphismGroup(Range(iso));
	
	if not superlist=fail then 
		Info(InfoMonoidAutos, 2, "######################################################################");
		Info(InfoMonoidAutos, 2, "AutomorphismGroup: finding the intersection of the super group and the \n#I  automorphism group"); 
		autos1:=Group(Filtered(autos1, x-> iso*x*InverseGeneralMapping(iso) in superlist));
	fi;

	Info(InfoMonoidAutos, 3, "AutomorphismGroup: group with size ", Size(autos1), " and ", Length(GeneratorsOfGroup(autos1)), " generators");

	Info(InfoMonoidAutos, 2, "AutomorphismGroup: conjugating the generators of the automorphism group");
	
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
			Info(InfoMonoidAutos, 4, helptext);
			out:=fail;
		else
			Info(InfoMonoidAutos, 4, "Error: input must be y, n, or h (for help)!");
			out:=fail;
		fi;
	until not out=fail;
	return out;
	end; 
	
	#########

	Info(InfoMonoidAutos, 1, "AutomorphismGroup: semigroup is not completely simple");
	
	n:=DegreeOfTransformationSemigroup(S);
	constants:=Set(Flat(GradedImagesOfTransSemigroup(S)[1]));
	Info(InfoMonoidAutos, 2, "AutomorphismGroup: ", Length(constants), " constants");
	allinner:=false;
	bad:=[];
	
	id:=SemigroupHomomorphismByImagesOfGensNC(S, S, gens);
	SetIsOne(id, true);
	autos:=Group(id);
	SetAsSSortedList(autos, [id]);
	
	if InfoLevel(InfoMonoidAutos)=4 and not Length(constants)=0 then
		Info(InfoMonoidAutos, 4, "##################################################################");
		Info(InfoMonoidAutos, 4, "AutomorphismGroup: Check if all automorphisms are inner? (y/n/h) #"); 
		Info(InfoMonoidAutos, 4, "##################################################################");
		bvals[1]:=useryn("It is advisable to answer yes if the size of the semigroup is not \n#I  too large.");
	elif Length(constants)=0 then
		allinner:=false;
		bvals[1]:=false;
	#else
	#	doit:=false;
	fi;
	
	if bvals[1] then 
		
		allinner:=true; 
	
		Info(InfoMonoidAutos, 2, "AutomorphismGroup: checking if all automorphisms are inner");
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
		
		Info(InfoMonoidAutos, 2, "AutomorphismGroup: ", allinner);
		
	fi; #check if all autos are inner
	
	if allinner and HasInnerAutomorphismsOfSemigroup(S) then 
		inner:=InnerAutomorphismsOfSemigroup(S);
		SetIsAutomorphismGroupOfSemigroup(inner, true);
		SetIsGroupOfAutomorphisms(inner, true);
		SetIsAutomorphismGroup(inner, true);
		
		return inner;
	elif not allinner then 

		Info(InfoMonoidAutos, 2, "AutomorphismGroup: computing D-classes");
		dclasses:=GreensDClasses(S);
		Info(InfoMonoidAutos, 2, "AutomorphismGroup: semigroup has size ", Size(S));
		Info(InfoMonoidAutos, 2, "AutomorphismGroup: ", Length(dclasses), " D-classes in total");

		#positions of D-classes containing generators

		pos:=SetX(gens, x-> PositionProperty(dclasses, y-> x in y));

		Info(InfoMonoidAutos, 2, "AutomorphismGroup: ", Length(pos), " D-classes of generators");

		info:=List(dclasses{pos}, c-> [Size(c), IsRegularDClass(c), 
		Length(GreensLClasses(c)), Length(GreensRClasses(c)), 
		Length(Idempotents(c))]);

		size:=SetX(dclasses{pos}, Size);
		regu:=SetX(dclasses{pos}, IsRegularDClass);
		left:=SetX(dclasses{pos}, c-> Length(GreensLClasses(c)));
		righ:=SetX(dclasses{pos}, c-> Length(GreensRClasses(c)));
		idem:=SetX(dclasses{pos}, c-> Length(Idempotents(c)));

		colours:=List([1..Length(pos)], x-> []);
		Info(InfoMonoidAutos, 2, "AutomorphismGroup: partitioning D-classes");

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

		Info(InfoMonoidAutos, 2, "AutomorphismGroup: computing isomorphisms to Rees Matrix Semigroups \n#I  for D-classes of generators");

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
			Info(InfoMonoidAutos, 2, "AutomorphismGroup: computing automorphism group of poset of D-classes"); 
			poset:=AutGroupGraph(PartialOrderOfDClasses(S), colours);

		#partition the orbit of the D-class containing each generator w.r.t.
		#isomorphism of D-classes as principal factors  

			if not IsTrivial(poset) then 

				orbits:=Orbits(poset, pos);

				if not ForAll(orbits, x-> Length(x)=1) then 

					Info(InfoMonoidAutos, 2, "AutomorphismGroup: partitioning orbits by isomorphism of D-classes"); 
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
					Info(InfoMonoidAutos, 2, "AutomorphismGroup: computing the intersection of stabilizers \n#I  of newly partitioned orbits of D-classes");
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

		Info(InfoMonoidAutos, 2, "AutomorphismGroup: automorphism group of poset of D-classes has size ", Size(poset), "\n#I  and moves ", NrMovedPoints(poset), " points"); 

		pos_list:=List(gens, x-> PositionProperty(dclasses, y-> x in y));
		#do not change the pos_list line! It is necessary.

		mappings:=List([1..Maximum(pos)], x-> [0]);

		for j in pos do 
			subgens:=AsSSortedList(gens{Filtered([1..Length(gens)], x-> pos_list[x]=j)});
			Info(InfoMonoidAutos, 3, "AutomorphismGroup: finding automorphism group of D-class ", j);

			if IsGroup(Range(RMS[j])) then 
				Info(InfoMonoidAutos, 3, "AutomorphismGroup: D-class is a group with size ", Size(Range(RMS[j])), " of degree ", LargestMovedPoint(Range(RMS[j])));
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
				Info(InfoMonoidAutos, 3, "AutomorphismGroup: D-class is a Rees matrix semigroup with size ", Size(Range(RMS[j])));
				A:=RightTransStabAutoGroup(Range(RMS[j]), OnTuples(subgens, RMS[j]), OnTuples);
				#RightTransversal(AutomorphismGroup(Range(RMS[j])), Stabilizer(AutomorphismGroup(Range(RMS[j])), subgens));
			fi;
			Info(InfoMonoidAutos, 3, "AutomorphismGroup: automorphism group of D-class has size ", Size(A));
			mappings[j]:=Enumerator(A);
				
		od;
		
		mappings:=EnumeratorOfCartesian(mappings);
	fi;
	
	if InfoLevel(InfoMonoidAutos)=4 and not HasInnerAutomorphismsOfSemigroup(S) then #and not allinner then 
		Info(InfoMonoidAutos, 4, "################################################################");
		Info(InfoMonoidAutos, 4, "AutomorphismGroup: Try to compute inner automorphisms? (y/n/h) #");
		Info(InfoMonoidAutos, 4, "################################################################");

		bvals[2]:=useryn("It might help to find an result more quickly if yes is the answer. \n#I  However, if the degree of the semigroup is large (>20 approx.), \n#I  then the answer should probably be no.");;
	fi;
	
	if HasInnerAutomorphismsOfSemigroup(S) then 
		autos:=InnerAutomorphismsOfSemigroup(S);
	elif bvals[2] then #compute inner autos
		Info(InfoMonoidAutos, 3, "AutomorphismGroup: computing stabiliser of images");
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

		Info(InfoMonoidAutos, 4, "AutomorphismGroup: stabilizer has size ", Size(inner));

		if not IsTrivial(inner) then 
			OnSets3:=function(x, g)
				return Set(List( x, i-> OnSetsSets(i, g)));
			end;
	
			Info(InfoMonoidAutos, 3, "AutomorphismGroup: computing stabiliser of kernels");

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
	
			Info(InfoMonoidAutos, 4, "AutomorphismGroup: stabilizer has size ", Size(inner));
		fi; #stabilizer of kernels

		if IsTrivial(inner) then
			Info(InfoMonoidAutos, 2, "AutomorphismGroup: inner automorphisms are trivial");
			SetIsInnerAutomorphismsOfSemigroup(autos, true);
			SetInnerAutomorphismsOfSemigroup(S, autos);
			if allinner then
				SetIsAutomorphismGroup(autos, true);
				SetIsGroupOfAutomorphisms(autos, true);
				SetIsAutomorphismGroupOfSemigroup(autos, true);
				return autos; 
			fi;
		else 
			Info(InfoMonoidAutos, 3, "AutomorphismGroup: computing the stabilizer of generators under \n#I  the action of the stabilizers of images and kernels");
			G:=OrbitStabilizer(inner, AsSet(gens), OnSets); #setwise stabilizer
			H:=OrbitStabilizer(G.stabilizer, gens, OnTuples); #pointwise stabilizer

			if not allinner then 
				Info(InfoMonoidAutos, 2, "AutomorphismGroup: the outer automorphism search space contains \n#I  ", Length(mappings)*Size(poset), " elements");
			fi;
			Info(InfoMonoidAutos, 2, "AutomorphismGroup: the inner automorphism search space contains \n#I  ", Length(G.orbit), " elements");

			if InfoLevel(InfoMonoidAutos)=4 then 

				Info(InfoMonoidAutos, 4, "####################################################################");
				Info(InfoMonoidAutos, 4, "AutomorphismGroup: proceed with inner automorphism search? (y/n/h) #"); 
				Info(InfoMonoidAutos, 4, "####################################################################");
				doit:=useryn("If the size of the inner search space is comparable or smaller in size \n#I  to that of the outer search space, then it is probably best to answer \n#I  yes. If the size of the inner space is much bigger than the outer, then  \n#I  it will probably be quicker to answer no.");;
			elif allinner or Length(mappings)*Size(poset)<=2*Length(G.orbit) or Length(G.orbit)<=Length(mappings)*Size(poset) then 
				doit:=true;
			else
				doit:=false;
			fi;

			if doit then 
				
				perms:=AsList(RightTransversal(inner, G.stabilizer));
				orb:=RightTransversal(G.stabilizer, H.stabilizer);
				
				if InfoLevel(InfoMonoidAutos)=4 and not superlist=fail then 
					Info(InfoMonoidAutos, 4, "AutomorphismGroup: the size of the super group is ", Size(superlist));
					Info(InfoMonoidAutos, 4, "######################################################################");
					Info(InfoMonoidAutos, 4, "AutomorphismGroup: Filter elements in the super group online? (y/n/h) #");
					Info(InfoMonoidAutos, 4, "######################################################################");
					bvals[3]:=useryn("Filtering the elements online is only advisable if the length of \n#I  the super group is comparable or less than the size of the inner search \n#I  space. You will prompted to ask if you want to filter after the \n#I  search for inner automorphisms.");
				elif superlist=fail then 
					bvals[3]:=false;
				fi; 

				Info(InfoMonoidAutos, 1, "AutomorphismGroup: computing inner automorphisms");

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
									Info(InfoMonoidAutos, 4, "AutomorphismGroup: images don't generate");
									bad:=Union(bad, Orbit(inner, image, OnTuples)); 
								fi;
							fi;
						else
							Info(InfoMonoidAutos, 4, "AutomorphismGroup: images not all in semigroup");
						fi;
					fi;
					
					Info(InfoMonoidAutos, 4, "AutomorphismGroup: automorphisms ", Size(inner), " nonautomorphisms ", Length(bad), " counter ", l);
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
					Info(InfoMonoidAutos, 2, "AutomorphismGroup: inner automorphisms are trivial");
				else 
					Info(InfoMonoidAutos, 4,	"AutomorphismGroup: inner automorphism group has ", Length(GeneratorsOfGroup(autos)), " generators and size ", Size(autos)); 
				fi;
			fi; 
		fi; 
	fi;  
	
	l:=0;
	Info(InfoMonoidAutos, 2, "AutomorphismGroup: the outer automorphism search space contains \n#I  ", Length(mappings)*Size(poset), " elements");
	if InfoLevel(InfoMonoidAutos)=4 and not superlist=fail then 
		Info(InfoMonoidAutos, 4, "AutomorphismGroup: the length of the super group is ", Size(superlist));
		Info(InfoMonoidAutos, 4, "#################################################################");
		Info(InfoMonoidAutos, 4, "AutomorphismGroup: In the search for outer automorphisms, filter \n#I  elements in the super group online? (y/n/h) ");
		Info(InfoMonoidAutos, 4, "#################################################################");
		bvals[4]:=useryn("Filtering the elements online is probably not advisable unless the \n#I  search space is very big or unless the final automorphism group is \n#I  very large.");
		
	elif superlist=fail then  
		bvals[4]:=false;
	fi; 
	
	if bvals[4] then 
			autos:=Group(Filtered(GeneratorsOfGroup(autos), x-> x in superlist));
	fi;
	
	if InfoLevel(InfoMonoidAutos)=4 then
		if superlist=fail then 
			Info(InfoMonoidAutos, 4, "#################################################################");
		fi;

		Info(InfoMonoidAutos, 4, "AutomorphismGroup: In the search for outer automorphisms, keep \n#I  track of nonautomorphisms? (y/n/h) ");

		Info(InfoMonoidAutos, 4, "#################################################################");

		bvals[5]:=useryn("keeping track of nonautomorphisms may make the computation faster \n#I  it may also use more memory. So, if the space is very large, better \n#I  answer no. ");
	fi;

	Info(InfoMonoidAutos, 1, "AutomorphismGroup: computing outer automorphisms");
	counter:=0;
	stop:=false;
	poset:=Enumerator(poset);

	repeat

		l:=l+1;
		k:=0;

		while k<Length(poset) and not stop do
			map:=mappings[l];
			counter:=counter+1;
			Info(InfoMonoidAutos, 4, "AutomorphismGroup: automorphisms ", Size(autos), " nonautomorphisms ", Length(bad), " counter ", counter);
			if 2*Size(autos)+Length(bad)>Length(mappings)*Size(poset) then 
				stop:=true;
			else 
				k:=k+1;
				i:=poset[k];
				Info(InfoMonoidAutos, 4, "AutomorphismGroup: automorphism of poset is: ", i);
				
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
									Info(InfoMonoidAutos, 4, "AutomorphismGroup: mapping already known to be a nonautomorphism");
								fi;
							else
								Info(InfoMonoidAutos, 4, "AutomorphismGroup: images don't generate");
								if bvals[5] then  
									bad:=Union(bad, Orbit(autos, image, OnTuples));
								fi;
							fi;
						else
							Info(InfoMonoidAutos, 4, "AutomorphismGroup: relations don't hold");
							if bvals[5] then 
								bad:=Union(bad, Orbit(autos, image, OnTuples));
							fi;
						fi;
					else 
						Info(InfoMonoidAutos, 4, "AutomorphismGroup: mapping already known to be an automorphism");
					fi;
				else 
					Info(InfoMonoidAutos, 4, "AutomorphismGroup: not in super group");
			fi;
				fi;
			fi;
		od;
	until l=Length(mappings) or stop;
fi;

SetIsAutomorphismGroup(autos, true);
SetIsAutomorphismGroupOfSemigroup(autos, true);

Info(InfoMonoidAutos, 4, "AutomorphismGroup: automorphisms ", Size(autos), " nonautomorphisms ", Length(bad), " counter ", counter);

if not bvals[4] and not superlist=fail then 
	SetAutomorphismGroup(S, autos); 
	
	Info(InfoMonoidAutos, 2, "##################################################################");
	Info(InfoMonoidAutos, 2, "AutomorphismGroup: finding the intersection of the superlist and the \n#I  automorphism group"); 
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

Info(InfoMonoidAutos, 2, "computing automorphisms of graph...");
autograph:=AutGroupGraph(RZMSGraph(rms), [[1..m],[m+1..n+m]]);
Info(InfoMonoidAutos, 2, Size(autograph), "graph automorphisms");

components:=ConnectedComponents(RZMSGraph(rms));
r:=Length(components);
Info(InfoMonoidAutos, 2, "graph has ", r, " connected components");

ZG:=UnderlyingSemigroupOfReesZeroMatrixSemigroup(rms);

Info(InfoMonoidAutos, 2, "computing automorphism group of underlying zero group");
autogroup:=AutomorphismGroup(ZG);
Info(InfoMonoidAutos, 4, "the automorphism group of underlying zero group has size ", Size(autogroup));

Info(InfoMonoidAutos, 3, "computing inner automorphisms of underlying zero group");
if not IsTrivial(autogroup) then 
	inner:=InnerAutomorphismsAutomorphismGroup(autogroup);
	transversal1:=RightTransversal(autogroup, inner);
else
	transversal1:=Elements(autogroup);
fi;
Info(InfoMonoidAutos, 2, "Aut G/Inn G has size ", Length(transversal1));

G:=UnderlyingGroupOfZG(ZG);
transversal2:=List(RightTransversal(G, Centre(G)), ZeroGroupElt);
G:=List(G, ZeroGroupElt);

Info(InfoMonoidAutos, 2, "|G/Z(G)|+", r-1, "|G| equals ", Length(transversal2)+(r-1)*Size(G));

Info(InfoMonoidAutos, 2, "search space has ", Size(autograph), "x", Length(transversal1), "x", Length(transversal2)+(r-1)*Size(G), "=", Size(autograph)*Length(transversal1)*(Length(transversal2)+(r-1)*Size(G)), " elements");

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
#return [trans,tuples];
end);


####################

InstallOtherMethod(RightTransStabAutoGroupNC, "for a RZMS", true, [IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroupElementCollection, IsFunction], 0,
function(rms, elts, func)
local mat, m, n, autograph, r, autogroup, inner, transversal1, ZG, G, transversal2, A, l, t, tup, new, trans, tuples, rmsgens, indices, components, transversal3, x, i, g, B, stab;

mat:=SandwichMatrixOfReesZeroMatrixSemigroup(rms);
m:=Length(mat[1]); n:=Length(mat);

Info(InfoMonoidAutos, 3, "computing automorphisms of graph...");
autograph:=AutGroupGraph(RZMSGraph(rms), [[1..m],[m+1..n+m]]);
Info(InfoMonoidAutos, 3, Size(autograph), "graph automorphisms");

components:=ConnectedComponents(RZMSGraph(rms));
r:=Length(components);
Info(InfoMonoidAutos, 3, "graph has ", r, " connected components");

ZG:=UnderlyingSemigroupOfReesZeroMatrixSemigroup(rms);

Info(InfoMonoidAutos, 3, "computing automorphism group of underlying zero group");
autogroup:=AutomorphismGroup(ZG);
Info(InfoMonoidAutos, 3, Size(autogroup), "automorphisms of underlying zero group");

Info(InfoMonoidAutos, 3, "computing inner automorphisms of underlying zero group...");
if not IsTrivial(autogroup) then 
	inner:=InnerAutomorphismsAutomorphismGroup(autogroup);
	transversal1:=RightTransversal(autogroup, inner);
else
	transversal1:=Elements(autogroup);
fi;
Info(InfoMonoidAutos, 3, "Aut G/Inn G has size ", Length(transversal1));

G:=UnderlyingGroupOfZG(ZG);
transversal2:=List(RightTransversal(G, Centre(G)), ZeroGroupElt);
G:=List(G, ZeroGroupElt);

Info(InfoMonoidAutos, 3, "|G/Z(G)|+", r-1, "|G| equals ", Length(transversal2)+(r-1)*Size(G));

Info(InfoMonoidAutos, 3, "search space has ", Size(autograph), "x", Length(transversal1), "x", Length(transversal2)+(r-1)*Size(G), "=", Size(autograph)*Length(transversal1)*(Length(transversal2)+(r-1)*Size(G)), " elements");

trans:=[];
tuples:=[];
stab:=[];

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
			if tup=elts then 
				Add(stab, new);
			elif not tup in tuples then
				AddSet(tuples, tup); 
				Add(trans, new);
			fi;
		od;
		
	od;
od;

return trans;
#JDM return tuples too?
#return [trans,tuples,stab];
end);



#############################################################################

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
#JDM this requires some changes see bug 100525bA

InstallOtherMethod(IsomorphismSemigroups, "for a RZMS and RZMS", 
[IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroup],
function(R1, R2)
local g1, g2, iso, G1, G2, isograph, isogroup, f, l, g, tup, candidate, mat, 
m, n;

if not (Size(R1)=Size(R2) and 
 ColumnsOfReesZeroMatrixSemigroup(R1)=ColumnsOfReesZeroMatrixSemigroup(R2) and 
  RowsOfReesZeroMatrixSemigroup(R1)=RowsOfReesZeroMatrixSemigroup(R2)) then 
   return fail;
else

	mat:=SandwichMatrixOfReesZeroMatrixSemigroup(R1);
	m:=Length(mat[1]); n:=Length(mat);

	if R1=R2 then 
		#return IdentityMapping(R1);
		g:=UnderlyingSemigroupOfReesZeroMatrixSemigroup(R2); 
		return RZMSIsoByTriple(R1, R2, [(), IdentityMapping(g), List([1..m+n], 
     x-> One(g))]);
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
						  #JDM bug 100525bA
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