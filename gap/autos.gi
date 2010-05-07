#############################################################################
##
#W  autos.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

#-if there was a way to make a presentation from R-classes then we could use an
# R-class version of the algorithm! (perhaps try this with FroidurePinSimpleAlg)

###########################################################################

InstallMethod(InnerAutomorphismsOfSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(S)
local gens, id, G;
return InnerAutomorphismsOfSemigroupInGroup(S, fail, false);
end);

###########################################################################

InstallOtherMethod(InnerAutomorphismsOfSemigroup, "for a zero group", 
[IsZeroGroup],
function(ZG)
return InnerAutomorphismsAutomorphismGroup(AutomorphismGroup(ZG));
end);

###########################################################################

#JDM why the higher rank filter?

InstallMethod(InnerAutomorphismsAutomorphismGroup, 
"for the automorphism group of a semigroup", 
[IsAutomorphismGroupOfSemigroup], 31,
function(G)

return InnerAutomorphismsOfSemigroup(Range(One(G)));
end);

###########################################################################

#JDM why the higher rank filter?

InstallMethod(InnerAutomorphismsAutomorphismGroup, 
"for automorphism group of zero group", [IsAutomorphismGroupOfZeroGroup], 30, 
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

InstallOtherMethod(InnerAutomorphismsOfSemigroupInGroup, 
"for a transformation semigroup and group", [IsTransformationSemigroup, IsGroup],
function(S, G)
return InnerAutomorphismsOfSemigroupInGroup(S, G, false);
end);

###########################################################################

InstallOtherMethod(InnerAutomorphismsOfSemigroupInGroup, 
"for a trans. semigroup, object, and boolean", 
[IsTransformationSemigroup, IsObject, IsBool],
function(S, superlist, bval)
local constants, m, n, inner, stop, OnSets3, useryn, gens, id, G, H, doit, 
perms, orb, l, x, image, hom, new, bad, autos;

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

constants:=Set(Flat(GradedImagesOfTransSemigroup(S)[1]));
n:=DegreeOfTransformationSemigroup(S);
Info(InfoMonoidAutos, 2, "InnerAutomorphisms: computing stabiliser of images");

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

if not IsTrivial(inner) then 
	Info(InfoMonoidAutos, 3, "InnerAutomorphisms: stabilizer has size ", Size(inner));
	OnSets3:=function(x, g)
		return Set(List( x, i-> OnSetsSets(i, g)));
	end;
	
	Info(InfoMonoidAutos, 2, "InnerAutomorphisms: computing stabiliser of kernels");

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

	Info(InfoMonoidAutos, 3, "InnerAutomorphisms: stabilizer has size ", Size(inner));
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

Info(InfoMonoidAutos, 2, "InnerAutomorphisms: computing the stabilizer of generators under \n#I  the action of the stabilizers of images and kernels");

G:=OrbitStabilizer(inner, AsSet(gens), OnSets); #setwise stabilizer
H:=OrbitStabilizer(G.stabilizer, gens, OnTuples); #pointwise stabilizer

Info(InfoMonoidAutos, 2, "InnerAutomorphisms: the inner automorphism search space contains \n#I  ", Length(G.orbit), " elements");

perms:=AsList(RightTransversal(inner, G.stabilizer));
orb:=AsList(RightTransversal(G.stabilizer, H.stabilizer));

if InfoLevel(InfoMonoidAutos)=4 and not superlist=fail then 
	Info(InfoMonoidAutos, 4, "InnerAutomorphisms: the size of the super group is ", Size(superlist));
	Info(InfoMonoidAutos, 4, "######################################################################");
	Info(InfoMonoidAutos, 4, "InnerAutomorphisms: filter elements in the super group online? (y/n/h) #");
	Info(InfoMonoidAutos, 4, "######################################################################");
	bval:=useryn("Filtering the elements online is only advisable if the size of \n#I  the super group is comparable or less than the size of the inner search \n#I  space.");
fi;

Info(InfoMonoidAutos, 1, "InnerAutomorphisms: computing inner automorphisms");

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
	if not image=gens then
		if not x in inner and not x in bad then
			if ForAll(image, x-> x in S) then 
				new:=Semigroup(image);
				if ForAll(gens, g-> g in new) then 
					if (bval and SemigroupHomomorphismByImagesOfGensNC(S, S, image) in superlist) or not bval then
						inner:=ClosureGroupDefault(inner, x);
					fi;
				else
					#Info(InfoMonoidAutos, 4, "InnerAutomorphisms: images don't generate");
					bad:=Union(bad, Orbit(inner, image, OnTuples)); 
				fi;
			else
				#Info(InfoMonoidAutos, 4, "InnerAutomorphisms: images not all in semigroup");
			fi;
		fi;
	fi;
	#Print("InnerAutomorphisms: automorphisms ", Size(inner), " nonautomorphisms ", Length(bad), " counter ", l, "\r");
until 2*Size(inner)+Length(bad)>Length(perms) or l=Length(perms);

if not bval and not superlist=fail then 
	Info(InfoMonoidAutos, 2, "######################################################################");
	Info(InfoMonoidAutos, 2, "AutomorphismGroup: finding the intersection of the superlist and the \n#I  automorphism group"); 
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
return autos;

end);

###########################################################################

InstallOtherMethod(AutomorphismGroup, "for a semigroup", 
[IsTransformationSemigroup], 
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

InstallMethod(AutomorphismGroup, "for a RZMS", [IsReesZeroMatrixSemigroup],
function(rms)

Print("#I It appears that the `grape' package is not fully installed. As a\n#I consequence this function is not available.\n");
return fail;
end);

###########################################################################

InstallOtherMethod(AutomorphismGroup, "for a zero semigroup", [IsZeroSemigroup], 
function(ZS)
local n, g1, g2, elms, G, hom, sym, iso;

Info(InfoMonoidAutos, 2, "AutomorphismGroup: computing the automorphism group of a zero semigroup");

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

InstallOtherMethod(AutomorphismGroup, "for a Rees matrix semigroup", [IsReesMatrixSemigroup],
function(rms)
local G, mat, m, n, autograph, autogroup, A, inner, transversal1, transversal2, g, h, l, t, tup, new, elts, id, hom;

G:=UnderlyingSemigroupOfReesMatrixSemigroup(rms);

mat:=SandwichMatrixOfReesMatrixSemigroup(rms);
m:=Length(mat[1]); n:=Length(mat);

if n=1 and m=1 then

	#rms is just a group in disguise!
	Info(InfoMonoidAutos, 2, "AutomorphismGroup: ReesMatrixSemigroup is a group");
	
	Info(InfoMonoidAutos, 2, "AutomorphismGroup: computing automorphism group of underlying group");

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

Info(InfoMonoidAutos, 2, "AutomorphismGroup: the automorphism group of the graph has size ", Size(autograph));

Info(InfoMonoidAutos, 2, "AutomorphismGroup: computing automorphism group of underlying group");
autogroup:=AutomorphismGroup(G);
Info(InfoMonoidAutos, 2, "AutomorphismGroup: the automorphism group of underlying group has size ", Size(autogroup));

Info(InfoMonoidAutos, 3, "AutomorphismGroup: computing inner automorphisms of underlying group");
if not IsTrivial(autogroup) then 
	inner:=InnerAutomorphismsAutomorphismGroup(autogroup);
	transversal1:=RightTransversal(autogroup, inner);
else
	transversal1:=Elements(autogroup);
fi;

Info(InfoMonoidAutos, 2, "AutomorphismGroup: Aut G/Inn G has size ", Length(transversal1));

transversal2:=RightTransversal(G, Centre(G));
Info(InfoMonoidAutos, 2, "AutomorphismGroup: G/Z(G) has size ", Length(transversal2));

Info(InfoMonoidAutos, 2, "AutomorphismGroup: search space has ", Size(autograph), "x", Length(transversal1), "x", Length(transversal2), "=", Size(autograph)*Length(transversal1)*Length(transversal2), " elements");

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

InstallOtherMethod(AutomorphismsSemigroupInGroup, "for a trans. semigroup and a group", [IsTransformationSemigroup, IsGroup],
function(S, G)
return AutomorphismsSemigroupInGroup(S, G, [false, false, false, false, true]); 
end);

###########################################################################

InstallMethod(AutomorphismsSemigroupInGroup, "for a trans. semigroup, object, and list", 
[IsTransformationSemigroup, IsObject, IsList],
function(S, superlist, bvals)

Print("#I It appears that the `grape' package is not fully installed.", 
" As a\n#I consequence this function is not available.\n");
return fail;
end);

#############################################################################

InstallOtherMethod(RightTransStabAutoGroup, 
"for a zero semigroup, mult. elt. collection, and function",
[IsZeroSemigroup, IsMultiplicativeElementCollection, IsFunction], 
function(S, elts, func)
local imgs, iso, stab;
#JDM some error handling here.

#imgs:=List(elts, x-> String(x));
#imgs:=List(imgs, x-> Int(x{[2..Length(x)]})+1);
imgs:=List(elts, x->x![1]+1);

iso:=IsomorphismPermGroup(AutomorphismGroup(S));
stab:=OrbitStabilizer(Range(iso), imgs, func);

#return [OnTuples(AsList(RightTransversal(Range(iso), stab.stabilizer)), 
#InverseGeneralMapping(iso)), List(stab.orbit, x-> List(x, y-> ZeroSemigroupElt(y-1)))]; 

#JDM could use enumerator here instead of AsList?
return OnTuples(AsList(RightTransversal(Range(iso), stab.stabilizer)), 
InverseGeneralMapping(iso));
end);

###########################################################################

InstallOtherMethod(RightTransStabAutoGroup, "for a Rees matrix semigroup",  
[IsReesMatrixSemigroup, IsReesMatrixSemigroupElementCollection, IsFunction],
function(rms, elts, func)

local G, mat, m, n, autograph, autogroup, inner, transversal1, transversal2, 
g, h, l, t, new, rmsgens, tup, trans, tuples, A, indices;

if not ForAll(elts, x-> x in rms) then 
	Error("<elts> must all lie in <rms>");
	return fail;
fi; #JDM make NC version? to omit this?

G:=UnderlyingSemigroupOfReesMatrixSemigroup(rms);

mat:=SandwichMatrixOfReesMatrixSemigroup(rms);
m:=Length(mat[1]); n:=Length(mat);

if n=1 and m=1 then
	
	#rms is just a group in disguise!
	Info(InfoMonoidAutos, 2, "AutomorphismGroupRMS: ReesMatrixSemigroup is a group");
	
	Info(InfoMonoidAutos, 2, "AutomorphismGroupRMS: computing automorphism group of", 
	" underlying group");
	G:=IsomorphismPermGroup(rms);
	autogroup:=RightTransversal(AutomorphismGroup(Range(G)), 
	 Stabilizer(AutomorphismGroup(Range(G)), OnTuples(elts, G), func));
	
	A:=List(autogroup, x-> RMSIsoByTriple(rms, rms, [(), x, [One(G), One(G)]]));

	return A;

elif n=2 and m=1 then 
	autograph:=Group((2,3));
elif n>2 and m=1 then 
	autograph:=Group((2,3), PermList(Concatenation([1],[3..n+m],[2]))); 
else 
	autograph:=DirectProduct(SymmetricGroup(m), SymmetricGroup(n));
fi;

Info(InfoMonoidAutos, 2, "AutomorphismGroupRMS: the automorphism group of the graph", 
 " has size ", Size(autograph));

Info(InfoMonoidAutos, 2, "AutomorphismGroupRMS: computing automorphism group of ", 
 " underlying group");
autogroup:=AutomorphismGroup(G);
Info(InfoMonoidAutos, 2, "AutomorphismGroupRMS: the automorphism group of underlying group has size ", Size(autogroup));

Info(InfoMonoidAutos, 3, "AutomorphismGroupRMS: computing inner automorphisms of underlying group");
if not IsTrivial(autogroup) then 
	inner:=InnerAutomorphismsAutomorphismGroup(autogroup);
	transversal1:=RightTransversal(autogroup, inner);
else
	transversal1:=Elements(autogroup);
fi;

Info(InfoMonoidAutos, 2, "AutomorphismGroupRMS: Aut G/Inn G has size ", Length(transversal1));

transversal2:=RightTransversal(G, Centre(G));
Info(InfoMonoidAutos, 2, "AutomorphismGroupRMS: G/Z(G) has size ", Length(transversal2));

Info(InfoMonoidAutos, 2, "AutomorphismGroupRMS: search space has ", Size(autograph), "x", 
Length(transversal1), "x", Length(transversal2), "=", 
Size(autograph)*Length(transversal1)*Length(transversal2), " elements");

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

#return [trans, tuples]; #JDM changes
#JDM could also return <tuples> if desirable
return trans;

end);

#############################################################################
##	JDM for the example given in the manual example this appears to run more 
##	slowly than just running automorphism group (although the overall 
##	computation is longer)

InstallOtherMethod(RightTransStabAutoGroup, "for a RZMS",
[IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroupElementCollection, IsFunction],
function(rms, elts, func)

Print("#I It appears that the `grape' package is not fully installed.", 
 " As a\n#I consequence this function is not available.\n");
return fail;

#JDM see grape.gi!!

end);

#############################################################################
#JDM JDM JDM the following is only commented out to allow for a new method in 
#the dev version. If doing a new release before moving things over from dev, 
#then uncomment this function.

InstallGlobalFunction(RMSIsoByTriple, 
function(rms1, rms2, triple)
  local fam1, fam2, mapfam, map;

  fam1 :=  ElementsFamily(FamilyObj(rms1));
  fam2 :=  ElementsFamily(FamilyObj(rms2));
  mapfam := GeneralMappingsFamily(fam1,fam2);	
  map := rec( triple := triple);
  Objectify(NewType(mapfam, IsGeneralMapping and IsSPGeneralMapping and 
   IsTotal and IsSingleValued and IsInjective and IsSurjective and 
    RespectsMultiplication and IsRMSIsoByTripleRep), map);
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

#############################################################################

InstallMethod( PrintObj, "for object in `IsRMSIsoByTriple'",
   [IsGeneralMapping and IsRMSIsoByTripleRep], 
   function( obj )
      Print( "RMSIsoByTriple ( ",Source(obj),",", obj!.triple, " )" );
end );

#############################################################################

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

#############################################################################

InstallMethod(PreImagesRepresentative,  "for an RMS element under a mapping by a triple", 
FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
	IsGeneralMapping and IsRMSIsoByTripleRep, IsReesMatrixSemigroupElement ],
function(triple, x)

return ImagesRepresentative(triple^-1, x);

end);

#############################################################################

InstallMethod(ImagesElm, "for an RMS element under a mapping by a triple",
      FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
	IsGeneralMapping and IsRMSIsoByTripleRep, IsReesMatrixSemigroupElement ],
      function( triple, x)
	
      return [ImagesRepresentative(triple, x)];
end);

#############################################################################

InstallMethod(CompositionMapping2, "for objects in `IsRMSIsoByTriple'", 
IsIdenticalObj, 
[IsGeneralMapping and IsRMSIsoByTripleRep, IsGeneralMapping and IsRMSIsoByTripleRep],
function(a1, a2)
local n, l1, l2, g1, g2, f1, f2;

n:=RowsOfReesMatrixSemigroup(Source(a1))
   +ColumnsOfReesMatrixSemigroup(Source(a1));

l1:=a1!.triple[1]; l2:=a2!.triple[1];
g1:=a1!.triple[2]; g2:=a2!.triple[2];
f1:=a1!.triple[3]; f2:=a2!.triple[3];

return RMSIsoByTriple(Source(a1), Range(a2), [l1*l2, g1*g2, List([1..n], 
 x->f2[x^l1]*f1[x]^g2)]);
end);

#############################################################################

InstallMethod(InverseGeneralMapping, "for objects in `IsRMSIsoByTriple'", 
[IsEndoGeneralMapping and IsRMSIsoByTripleRep],
function(a)
local l, g, f, n; 

n:=RowsOfReesMatrixSemigroup(Source(a))
   +ColumnsOfReesMatrixSemigroup(Source(a));

l:=a!.triple[1]; g:=a!.triple[2]; f:=a!.triple[3];

return RMSIsoByTriple(Range(a), Source(a), [l^-1, g^-1, List([1..n],x-> 
(f[x^l]^(g^-1))^-1)]);
end);

#############################################################################

InstallMethod(InverseGeneralMapping, "for objects in `IsRMSIsoByTriple'", [IsEndoGeneralMapping and IsRMSIsoByTripleRep and IsOne], x-> x);

#############################################################################

InstallMethod(\=, "for objects in `IsRMSIsoByTriple'", IsIdenticalObj, 
[IsEndoGeneralMapping and IsRMSIsoByTripleRep, IsEndoGeneralMapping and 
 IsRMSIsoByTripleRep],
function(triple1, triple2)

if triple1!.triple[1]=triple2!.triple[1] and triple1!.triple[2]=triple2!.triple[2] and triple1!.triple[3]=triple2!.triple[3] then 

	return true;

else 

	return OnTuples(GeneratorsOfSemigroup(Source(triple1)), triple1)=OnTuples(GeneratorsOfSemigroup(Source(triple1)), triple2);
fi; 

end);

#############################################################################

InstallMethod(\<, "for objects in `IsRMSIsoByTriple'", IsIdenticalObj,
[IsGeneralMapping and IsRMSIsoByTripleRep, IsGeneralMapping and 
 IsRMSIsoByTripleRep],  
function(triple1, triple2)

return (triple1!.triple[1]<triple2!.triple[1]) or
	(triple1!.triple[1]=triple2!.triple[1] and triple1!.triple[2]<triple2!.triple[2]) or (triple1!.triple[1]=triple2!.triple[1] and triple1!.triple[2]=triple2!.triple[2] and triple1!.triple[3]<triple2!.triple[3]);

end);

#############################################################################

InstallMethod(IsOne, "for objects in `IsRMSIsoByTriple'", true, [IsGeneralMapping and IsRMSIsoByTripleRep], 99, 
function(triple)

return IsOne(triple!.triple[1]) and IsOne(triple!.triple[2]) and ForAll(triple!.triple[3], IsOne);
end);


#############################################################################
#JDM JDM JDM the following is only commented out to allow for a new method in 
#the dev version. If doing a new release before moving things over from dev, 
#then uncomment this function.

InstallGlobalFunction(RZMSIsoByTriple, 
function(rms1, rms2, triple)
  local fam1, fam2, mapfam, map;

  fam1 :=  ElementsFamily(FamilyObj(rms1));
  fam2 :=  ElementsFamily(FamilyObj(rms2));
  mapfam := GeneralMappingsFamily(fam1,fam2);	
  map := rec( triple := triple);
  Objectify(NewType(mapfam, IsGeneralMapping and 
IsSPGeneralMapping and IsTotal and IsSingleValued and IsInjective and 
IsSurjective and RespectsMultiplication and IsRZMSIsoByTripleRep), map);
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

#############################################################################

InstallMethod( PrintObj, "for object in `IsRZMSIsoByTriple'",
   [IsGeneralMapping and IsRZMSIsoByTripleRep],
   function( obj )
      Print( "RZMSIsoByTriple ( ",Source(obj),",", obj!.triple, " )" );
end );

#############################################################################

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

#############################################################################

InstallMethod(PreImagesRepresentative,  "for an RZMS element under a mapping by a triple", 
FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
	IsGeneralMapping and IsRZMSIsoByTripleRep, IsReesZeroMatrixSemigroupElement ],
function(triple, x)

return ImagesRepresentative(triple^-1, x);

end);

#############################################################################

InstallMethod(ImagesElm, "for an RZMS element under a mapping by a triple",
      FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
	IsGeneralMapping and IsRZMSIsoByTripleRep, IsReesZeroMatrixSemigroupElement ],
      function( triple, x)
	
      return [ImagesRepresentative(triple, x)];
end);

#############################################################################

InstallMethod(CompositionMapping2, "for objects in `IsRZMSIsoByTriple'", IsIdenticalObj, [IsGeneralMapping and IsRZMSIsoByTripleRep, IsGeneralMapping and IsRZMSIsoByTripleRep],
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

#############################################################################

InstallMethod(InverseGeneralMapping, "for objects in `IsRZMSIsoByTriple'",
[IsGeneralMapping and IsRZMSIsoByTripleRep],
function(a)
local l, g, f, n; 

n:=RowsOfReesZeroMatrixSemigroup(Source(a))
   +ColumnsOfReesZeroMatrixSemigroup(Source(a));

l:=a!.triple[1]; g:=a!.triple[2]; f:=a!.triple[3];

return RZMSIsoByTriple(Range(a), Source(a), [l^-1, g^-1, List([1..n],x-> 
(f[x^l]^(g^-1))^-1)]);
end);

#############################################################################

InstallMethod(IsOne, "for objects in `IsRZMSIsoByTriple'",
[IsEndoGeneralMapping and IsRZMSIsoByTripleRep],
function(triple)

return IsOne(triple!.triple[1]) and IsOne(triple!.triple[2]) and 
 ForAll(triple!.triple[3], IsOne);
end);

#############################################################################

InstallMethod(\=, "for objects in `IsRZMSIsoByTriple'", IsIdenticalObj, 
[IsEndoGeneralMapping and IsRZMSIsoByTripleRep, IsEndoGeneralMapping and 
IsRZMSIsoByTripleRep], 
function(triple1, triple2)

if triple1!.triple[1]=triple2!.triple[1] and triple1!.triple[2]=triple2!.triple[2] and triple1!.triple[3]=triple2!.triple[3] then 

	return true;

else 

	return OnTuples(GeneratorsOfSemigroup(Source(triple1)), 
	 triple1)=OnTuples(GeneratorsOfSemigroup(Source(triple1)), triple2);
fi; 

end);

#############################################################################

InstallMethod(\<, "for objects in `IsRZMSIsoByTriple'", IsIdenticalObj,
[IsGeneralMapping and IsRZMSIsoByTripleRep, IsGeneralMapping and 
 IsRZMSIsoByTripleRep],
function(triple1, triple2)

return (triple1!.triple[1]<triple2!.triple[1]) or
 (triple1!.triple[1]=triple2!.triple[1] and triple1!.triple[2]<triple2!.triple[2]) or 
	(triple1!.triple[1]=triple2!.triple[1] and triple1!.triple[2]=triple2!.triple[2] and 
	 triple1!.triple[3]<triple2!.triple[3]);
end);

#############################################################################

InstallMethod(RMSInducedFunction, "for a RMS", 
[IsReesMatrixSemigroup, IS_PERM, IsGeneralMapping, IsMultiplicativeElement], 

function(rms, l, g, groupelt)
local mat, m, n, imagelist, i, j;

imagelist:=[];
mat:=SandwichMatrixOfReesMatrixSemigroup(rms);
m:=Length(mat[1]); n:=Length(mat);

imagelist[1]:=groupelt;

imagelist{[m+1..n+m]}:=List([m+1..n+m], v-> mat[v^l-m][1^l]*imagelist[1]*
 (mat[v-m][1]^g)^-1);
imagelist{[2..m]}:=List([2..m], v-> (mat[(m+1)^l-m][v^l])^-1*imagelist[m+1]*
 (mat[(m+1)-m][v]^g));

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

InstallMethod(RZMSInducedFunction, "for a RZMS",
[IsReesZeroMatrixSemigroup, IS_PERM, IsGeneralMapping, IsMultiplicativeElement, IsList],
function(rms, l, g, groupelt, component)

Print("#I It appears that the `grape' package is not fully installed. As a\n#I consequence this function is not available.\n");
return fail;

end);

#############################################################################

InstallMethod(RZMStoRZMSInducedFunction, "for a RZMS", 
[IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroup, IS_PERM, IsGeneralMapping, IsList],

function(rms1, rms2, l, g, groupelts)

Print("#I It appears that the `grape' package is not fully installed.", 
"As a\n#I consequence this function is not available.\n");
return fail;

end);

#############################################################################

InstallMethod(RZMSGraph, "for a RZMS", true, [IsReesZeroMatrixSemigroup], 0,
function(rms)

Print("#I It appears that the `grape' package is not fully installed.", 
"As a\n#I consequence this function is not available.\n");
return fail;

end);

###########################################################################
## Isomorphisms
###########################################################################

InstallMethod(IsomorphismAutomorphismGroupOfRMS, "for the automorphism group of a simple semigroup", true, [IsAutomorphismGroupOfSimpleSemigp], 0, 
function(G)
local S, iso, H, hom;

S:=Source(One(G));
iso:=IsomorphismReesMatrixSemigroup(S);
H:=AutomorphismGroup(Range(iso));

return GroupHomomorphismByImagesNC(G, H, GeneratorsOfGroup(G), SmallGeneratingSet(H));
end);

###########################################################################

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

InstallOtherMethod(IsomorphismFpMonoid, "for a transformation monoid", 
[IsTransformationMonoid],
function(S)
FroidurePinExtendedAlg(S);
return IsomorphismFpMonoid(S);
end);

#############################################################################

InstallOtherMethod(IsomorphismSemigroups,  "for a zero group and zero group", 
[IsZeroGroup, IsZeroGroup], 
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

Print("#I It appears that the `grape' package is not fully installed. As a\n#I consequence this function is not available.\n");
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

InstallMethod(IsomorphismReesMatrixSemigroupOfDClass, 
"for a Green's D-class", [IsGreensDClass and IsAssociatedSemigpTransSemigp],
function(D)
local g, zg, rep, r, l, rreps, lreps, mat, func, rms, invlreps, invrreps, Q, R, 
hom, invfunc, RMS;

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
		func:=AsPermOfRange;
		invfunc:=x-> IdempotentNC(KernelOfTransformation(rep), ImageSetOfTransformation(rep))*x;
		#hom:=SemigroupHomomorphismByFunctionNC(D, g, AsPermOfRange);
		#return SemigroupHomomorphismByFunctionNC(D, g, AsPermOfRange);
	else

		RMS:=true;
	
		mat:=List(lreps, x-> List(rreps, function(y)
			if x*y in D then 
				return AsPermOfRange(x*y);
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
					AsPermOfRange(lreps[col]*rreps[row])^-1*
					AsPermOfRange(lreps[col]*d*rreps[row])*
					AsPermOfRange(lreps[col]*rreps[row])^-1, col);
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

			return ReesZeroMatrixSemigroupElementNC(rms, row, ZeroGroupElt(AsPermOfRange(invrreps[row]*d*invlreps[col])), col);
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

  mat:=List(lreps, x-> List(rreps, y-> AsPermOfRange(x*y)));

  rms:=ReesMatrixSemigroup(g, mat);
  
  #JDM this could be speeded up :)
  func:=function(d)
    local col, row;
    col:=PositionProperty(lreps, x-> ImageSetOfTransformation(d)
		=ImageSetOfTransformation(x));
    row:=PositionProperty(rreps, x-> KernelOfTransformation(d)
		=KernelOfTransformation(x));    

    return ReesMatrixSemigroupElementNC(rms, row, 
	    AsPermOfRange(lreps[col]*rreps[row])^-1*
            AsPermOfRange(lreps[col]*d*rreps[row])*
            AsPermOfRange(lreps[col]*rreps[row])^-1, col);
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
  #	G:=Group(List(GeneratorsOfSemigroup(S), x-> AsPermOfRange(x)));
	#	hom:=SemigroupHomomorphismByFunctionNC(S, G, AsPermOfRange);
	#	SetInverseGeneralMapping(hom, SemigroupHomomorphismByFunctionNC(G, S, x->Idempotents(S)[1]*x));
	#	SetIsBijective(hom, true); SetIsTotal(hom, true);
	#	return hom;
		
  else
    Error("<S> must be a completely simple semigroup");
  fi;
end);

###########################################################################

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
gp:=Group(List(elms, AsPermOfRange));
mapfun := x-> AsPermOfRange(x); 

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

gens:=List(GeneratorsOfSemigroup(S), AsPermOfRange);
mapfun := x-> AsPermOfRange(x); 

return 	SemigroupHomomorphismByFunctionNC(S, Group(gens), mapfun );

end);


###########################################################################

InstallMethod(RightTransversal, 
[IsAutomorphismGroupOfZeroGroup, IsInnerAutomorphismsOfZeroGroup], 
function(G, H)
local zg;
zg:=Source(Identity(G));
return List(RightTransversal(NiceObject(G), NiceObject(H)), x-> 
 ZeroGroupAutomorphism(zg, x));
end);

###########################################################################

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

InstallMethod(UnderlyingGroupAutoOfZeroGroupAuto, "for a zero group automorphism", 
[IsEndoGeneralMapping and IsZeroGroupAutomorphismRep], x-> x!.grpauto);

###########################################################################
##
##	The functions below are the usual functions for dealing with zero group
##	automorphisms.
##

InstallMethod(ImagesRepresentative, "for a zero group automorphism", 
true, [ IsTotal and IsSingleValued and IsEndoGeneralMapping and 
IsZeroGroupAutomorphismRep, IsZeroGroupElt ],
function( aut, x)

if IsMultiplicativeZero(x) then 
	return x;
fi;

return ZeroGroupElt(ImagesRepresentative(aut!.grpauto, UnderlyingGroupEltOfZGElt(x)));

end);

#JDM Add method for OnPoints and OnTuples currently inside Automorphism group in 
#dev/gap/grape.gi

###########################################################################

InstallMethod(PreImagesRepresentative,   "for a zero group automorphism", 
FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
	IsEndoGeneralMapping and IsZeroGroupAutomorphismRep, IsZeroGroupElt ],
function(aut, x)

if IsMultiplicativeZero(x) then 
          return x;
fi;

return ImagesRepresentative(aut!.grpauto^-1, UnderlyingGroupEltOfZGElt(x));

end);

###########################################################################

InstallMethod(ImagesElm, "for a zero group automorphism",
      FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
	IsEndoGeneralMapping and IsZeroGroupAutomorphismRep, IsZeroGroupElt ],
      function( triple, x)
	
      return [ImagesRepresentative(triple, x)];
end);

###########################################################################

InstallMethod(CompositionMapping2, "for objects in IsZeroGroupAutomorphismRep", IsIdenticalObj, [IsEndoGeneralMapping and IsZeroGroupAutomorphismRep, IsEndoGeneralMapping and IsZeroGroupAutomorphismRep], 0,
function(a1, a2)

return ZeroGroupAutomorphism(Source(a1), a1!.grpauto*a2!.grpauto);

end);

###########################################################################

InstallMethod(InverseGeneralMapping, "for objects in IsZeroGroupAutomorphismRep", true, [IsEndoGeneralMapping and IsZeroGroupAutomorphismRep],  0,
function(a)

return ZeroGroupAutomorphism(Source(a), a!.grpauto^-1);

end);

###########################################################################

InstallMethod(\=, "for objects in IsZeroGroupAutomorphismRep", IsIdenticalObj, [IsZeroGroupAutomorphismRep, IsZeroGroupAutomorphismRep],  0,
function(a1, a2)

return ForAll(GeneratorsOfMonoid(Source(a1)), 
   x -> ImageElm(a1,x) = ImageElm(a2,x));

end);

###########################################################################

InstallMethod(\<, "for objects in IsZeroGroupAutomorphismRep", IsIdenticalObj, [IsZeroGroupAutomorphismRep, IsZeroGroupAutomorphismRep],  0,
function(a1, a2)

return a1!.grpauto<a2!.grpauto;

end);

###########################################################################

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

###########################################################################

InstallMethod( InnerAutomorphismOfSemigroup, "for trans. semigroup and permutation", true, 
[ IsTransformationSemigroup, IsPerm ], 0,
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
    "for semigroup homomorphism and inner automorphisms of semigroups",
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