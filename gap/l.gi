#############################################################################
##
#W  greens_l_orb.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$

#############################################################################
## Notes

# - this file is alphabetized, keep it that way!

# - this file should only contain functions relating to kernels/l-classes!

# - what makes IteratorOfLClassReps slow is the repeated calls to 
#   SchutzenbergerGroupOfSCCOfKernelOrbit. Should rethink this. Maybe
#   iterate thru RClassReps and find the L-classes intersecting it, using
#   Green's lemma to say what the schutz group should be?

# - must work on OrbitsOfKernels/LClasses!!!

##
#############################################################################

# maybe the following should be used in IteratorOfLClassReps JDM

InstallGlobalFunction(AddToOrbitsOfKernels,
function(s, f, data)
local j, k, l, m, val, o, O, one, gens, reps, schutz; 

j:=data[1]; 	#ker length
k:=data[2]; 	#index of orbit containing ker
l:=data[3]; 	#position of ker in O[j][k]
m:=data[4]; 	#scc of O[j][k] containing ker
val:=data[5]; #position of img in O[j][k]!images_ht[m]

o:=OrbitsOfKernels(s);
O := o!.orbits;
one:=o!.one;
gens:=o!.gens;

if k = fail then #ker has not been seen before
	if IsTransformationMonoid( s ) or not f = one then
		ForwardOrbitOfKernelNC(s, f); #maybe ForwardOrbitOfKernelNC should be removed JDM?
	fi;
else #ker has been seen before (and so k,l,m not= fail)
	if IsTransformationMonoid( s ) or not f = one then
		reps:=O[j][k]!.reps[m]; 
		schutz:=O[j][k]!.schutz[m];
		
		if not IsBound(O[j][k]!.rels[l]) then 
			#we never considered this scc before! (and so val must be fail)
			O[j][k]!.trees[m]:=CreateSchreierTreeOfSCC(O[j][k], m);
			O[j][k]!.reverse[m]:=CreateReverseSchreierTreeOfSCC(O[j][k], m);
			MultipliersOfSCCOfKernelOrbit(gens, O[j][k], m);
			f:= O[j][k]!.rels[l][2]*f; #ker(x)=O[j][k][scc[m][1]]
			schutz[Length(schutz)+1]:=[SchutzenbergerGroupOfSCCOfKernelOrbit(gens, 
			 O[j][k], f, m)];;
			O[j][k]!.images_ht[m]:=HashTableForImage(ImageSetOfTransformation(f));
			reps[Length(reps)+1]:=[f];
			o!.data[Length(o!.data)+1]:=[j, k, l, m, 1, 1];
		else #we have considered scc before
			f:= O[j][k]!.rels[l][2]*f;

			if not val=fail then #image seen before
				reps[val][Length(reps[val])+1]:=f;
				o!.data[Length(o!.data)+1]:=[j, k, l, m, val, Length(reps[val])];
				schutz[val][Length(schutz[val])+1]:=
				 SchutzenbergerGroupOfSCCOfKernelOrbit(gens, O[j][k], f, m);
			else #new image
				schutz[Length(schutz)+1]:=[SchutzenbergerGroupOfSCCOfKernelOrbit(gens, 
				 O[j][k], f, m)];
				reps[Length(reps)+1]:=[f];
				o!.data[Length(o!.data)+1]:=[j, k, l, m, Length(reps), 1];
				HTAdd(O[j][k]!.images_ht[m], ImageSetOfTransformation( f ), 
				 Length(reps));
			fi;
		fi;
	fi;
fi;

return o!.data[Length(o!.data)];
end);

# new for 3.2!
#############################################################################
# Algorithm D.

InstallOtherMethod(AsList, "for an L-class of trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
local f, s, d, h, elts, p;

Info(InfoMonoidGreens, 4, "AsList: for an L-class");

f:=l!.rep; #rep should have its image at the first place in the scc
s:=l!.parent;
d:=l!.data;

h:=List(RClassSchutzGpFromData(s, d)[2], x-> f*x);
# <h> contains the H-class of the representative of <r> as a subgroup. 

elts:=[];

for p in RClassPermsFromData(s, d){RClassSCCFromData(s, d)} do 
	elts:=Concatenation(elts, h*p^-1);
od;
return elts;
end);

#new for 3.2!
#############################################################################

InstallGlobalFunction(DisplayOrbitsOfKernels, 
function(s)
local o;

o:=OrbitsOfKernels(s);

Print("finished: ", o!.finished, "\n");
Print("orbits: "); View(o!.orbits); Print("\n");
Print("at: ", o!.at, "\n");
Print("ht: "); View(o!.ht); Print("\n");
Print("size: ", SizeOrbitsOfKernels(s), "\n");
Print("L-classes: ", NrLClassesOrbitsOfKernels(s), "\n");
return true;
end);

#new for 3.2!
#############################################################################

InstallGlobalFunction(ForwardOrbitOfKernel, 
function(s, f)
local ker, i, o;

#Info(InfoMonoidGreens, 4, "ForwardOrbitOfKernel");

ker:=KernelOfTransformation(f);
o:=OrbitsOfKernels(s)!.orbits;

if IsBound(o[Length(ker)]) then 
  i:=Position(o[Length(ker)], x-> ker in x);
else
  i:=fail;
fi;

if not i=fail then 
  return o[i];
fi;

return ForwardOrbitOfKernelNC(s, f);
end);

#new for 3.2!
#############################################################################

InstallGlobalFunction(ForwardOrbitOfKernelNC,
function(s, f)
local gens, ker, o, scc, t, img, ht, O;

#Info(InfoWarning, 2, "Warning: calling this function more than once with the ",
#" same arguments will repeatedly add the returned value to OrbitsOfKernels. ",
#"Use ForwardOrbitOfImage instead.");

#Info(InfoMonoidGreens, 4, "ForwardOrbitOfKernelNC");

if IsMonoid(s) then 
	gens:=GeneratorsOfMonoid(s);
else
	gens:=GeneratorsOfSemigroup(s);
fi;

ker:=KernelOfTransformation(f);
o:=Orb(s, ker, OnKernelsAntiAction, rec(
        treehashsize:=NextPrimeInt(Minimum(100000, 
         3*Binomial(DegreeOfTransformationSemigroup(s), Length(ker)))), 
        schreier:=true,
        gradingfunc := function(o,x) return Length(x); end, 
        orbitgraph := true, 
        onlygrades:=[Length(ker)], 
        storenumbers:=true));
Enumerate(o);

#strongly connected components
scc:=Set(List(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(List(OrbitGraph(o), Set)), 
 Set));;
o!.scc:=scc;

#boolean list corresponding to membership in scc[i]
t:=List([1..Length(scc)], i-> BlistList([1..Length(o)], scc[i]));
o!.truth:=t;

#Schreier trees for strongly connected components
o!.trees:=EmptyPlist(Length(scc));
o!.reverse:=EmptyPlist(Length(scc));
o!.trees[1]:=CreateSchreierTreeOfSCC(o,1); 
o!.reverse[1]:=CreateReverseSchreierTreeOfSCC(o,1);

#representatives of L-classes with image belonging in scc[i] partitioned 
#according to their kernels
o!.reps:=List([1..Length(scc)], x-> []);
Add(o!.reps[1], [f]);

#images of representatives of L-classes with kernel belonging in scc[i]
o!.images_ht:=[];
img:=ImageSetOfTransformation(f);
ht := HTCreate(img, rec( hfd := 100003, treehashsize := 100003 ));
HTAdd(ht, img, 1);
Add(o!.images_ht, ht);

#multipliers of scc containing the kernel of f
o!.rels:=EmptyPlist(Length(o));
MultipliersOfSCCOfKernelOrbit(gens, o, 1);

#schutzenberger group corresponding to scc[1] and f
#this differs from the method for R-classes in that here we require one group 
#per rep! 
o!.schutz:=List([1..Length(scc)], x-> []);
Add(o!.schutz[1], [SchutzenbergerGroupOfSCCOfKernelOrbit(gens, o, f, 1)]);

#OrbitsOfKernels!.orbits is partitioned according to image size of the first element in 
# each component!

O:=OrbitsOfKernels(s);

if IsBound(O!.orbits[Length(ker)]) then 
  Add(O!.orbits[Length(ker)], o);
else
  O!.orbits[Length(ker)]:=[o];
fi;

O!.data[Length(O!.data)+1]:=[Length(ker), Length(O!.orbits[Length(ker)]), 
 1, 1, 1, 1];

return o;
end);

# new for 3.2!
#############################################################################

InstallMethod(GreensLClassReps, "for a trans. semigroup", 
[IsTransformationSemigroup], 
function(s)
local iter, i, o;
Info(InfoMonoidGreens, 4, "GreensLClassReps");

iter:=IteratorOfLClassReps(s); 
for i in iter do 
od;

return List(OrbitsOfKernels(s)!.data, x-> LClassRepFromData(s, x));
end);

# new for 3.2!
#############################################################################

InstallGlobalFunction(HashTableForImage, 
function(img)
local ht;
ht := HTCreate(img, rec( hfd := 100003, treehashsize := 100003 ));
HTAdd(ht, img, 1);

return ht;
end);

# new for 3.2!
#############################################################################
# JDM change this to accept s as arg[1] instead of what's there?

InstallGlobalFunction(InOrbitsOfKernels, 
function(arg)
local O, f, gens, j, k, l, m, val, n, g, schutz, x, ker, o;

#Info(InfoMonoidGreens, 4, "InOrbitsOfKernels");

o:=OrbitsOfKernels(arg[1]);
O:=o!.orbits; f:=arg[2]; 
gens:=o!.gens;
ker:=KernelOfTransformation(f);

if Length(arg)=3 then 
	j:=arg[3][1]; k:=arg[3][2]; l:=arg[3][3];
	m:=arg[3][4]; val:=arg[3][5]; n:=arg[3][6];
	g:=arg[3][7];
else
	j:=Length(ker);
	k:=fail; l:=fail; m:=fail; val:=fail; n:=1; g:=fail;
fi;

if not IsBound(O[j]) then
	return [false, [j, fail, fail, fail, fail, 1, fail]];
fi;

if k=fail then
	k:=0;

	repeat
		k:=k+1;
		l:=Position(O[j][k], ker);
	until not l=fail or k=Length(O[j]);

	if l = fail then 
		return [false, [j, fail, fail, fail, fail, 1, fail]];
	fi;
	m:=PositionProperty(O[j][k]!.truth, x-> x[l]);
	if not IsBound(O[j][k]!.rels[l]) then #we never considered this scc before! 
		return [false, [j,k,l,m,fail, 1, fail]];
	fi;
fi;

if g=fail and not l=fail and IsBound(O[j][k]!.rels[l]) then 
	g:=O[j][k]!.rels[l][2]*f; #make the kernel of f the first elt in the scc
fi; 

if val=fail then 
	if not IsBound(O[j][k]!.images_ht[m]) then 
		return [false, [j, k, l, m, fail, 1, g]];
	fi;
	val:=HTValue(O[j][k]!.images_ht[m], ImageSetOfTransformation(f));
	if val=fail then 
  	return [false, [j, k, l, m, fail, 1, g]];
	fi;
fi;

schutz:=O[j][k]!.schutz[m][val][n][1];

if schutz=true then 
	return [true, [j,k,l,m,val,1,g]]; #JDM should the 1 be an n?
fi;

#JDM note that we always look at the first Schutz. gp! when n=1
# even when n is greater than 1 below!!!

while n<=Length(O[j][k]!.reps[m][val]) do 
	schutz:=O[j][k]!.schutz[m][val][n][1];
	x:=O[j][k]!.reps[m][val][n];
	if SiftedPermutation(schutz, PermLeftQuoTransformationNC(x, g))=() then 
		return [true ,[j,k,l,m,val,n,g]];
	fi;
	n:=n+1;

od;

return [false, [j,k,l,m,val,n,g]];
end);


# new for 3.2!
#############################################################################
# add some more info statements?

InstallGlobalFunction(IteratorOfLClassReps, 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfLClassReps");

iter:=IteratorByFunctions( rec(
			
			IsDoneIterator := iter-> iter!.chooser(iter, IsDoneIterator)=fail,
			
			NextIterator := iter-> iter!.chooser(iter, NextIterator),
			
			ShallowCopy := iter -> rec( i:=0, s:=iter!.s, 
			last_called := NextIterator, last_value := 0, 
			chooser:=iter!.chooser, next:=iter!.next),
			
			i:=0, # in case one iterator is started, then 
			      # another iterator is started. 
			
			s:= s,
			
			last_called := NextIterator,
				
			last_value := 0,
			
			######################################################################
			
			chooser := function( iter, called_by )
			local o;
			
			if iter!.last_called = IsDoneIterator then 
				iter!.last_called := called_by;
				return iter!.last_value; 
			fi;

			if iter!.last_called = NextIterator then
				iter!.last_called := called_by;
				if iter!.last_value=fail then 
					return fail;
				fi;
				
				o:=OrbitsOfKernels(iter!.s);
				
				if iter!.i < Length(o!.data) then 
					# we already know this rep
					iter!.i:=iter!.i+1;
					iter!.last_value:=LClassRepFromData(iter!.s, 
					 o!.data[iter!.i]);
				elif o!.finished then  
					iter!.last_value:=fail;
				else
					# must find a new rep if it exists
					iter!.i:=o!.at;
					repeat 
						iter!.last_value:=iter!.next(iter);
					until not iter!.last_value=false or iter!.last_value=fail;
				fi;
				return iter!.last_value;
			fi;
			
			end,
			
			######################################################################

			next:=function(iter) 
			local s, oo, gens, n, one, ht, o, O, data, i, ker, j, new, k, l, m, x, 
			 reps, img, ht2, images_ht, val, schutz, y, z;
			
			s := iter!.s;
			oo:=OrbitsOfKernels(s);
			gens := oo!.gens;
			n := oo!.deg;
			one := oo!.one;
			ht:= oo!.ht;
			o := ht!.o;
			O := oo!.orbits;
			data := oo!.data;
			
			if iter!.i=Length(o) then 
				oo!.finished:=true;
				return fail;
			fi;
			
			iter!.i:=iter!.i+1;
			oo!.at:=iter!.i;
			i := iter!.i;

			if iter!.i=1 then #move this to OrbitsOfKernels! JDM
				HTAdd(ht, one, true);
			fi;

			ker:=KernelOfTransformation(o[i]);
			j:=Length(ker);
			new:=false;
			
			#check if ker has been seen before
			if IsBound(O[j]) then
				k:=0;
				repeat
					k:=k+1;
					l:=Position(O[j][k], ker);
				until not l=fail or k=Length(O[j]);
				
				if l=fail then k:=fail; fi;
			else 
				k:=fail;
			fi;
			
			if k = fail then #ker has not been seen before
				new:=true; x:=o[i]; 
				
				if IsTransformationMonoid( s ) or not o[i] = one then
					ForwardOrbitOfKernelNC(s, o[i]);
				fi;
				
			else #ker has been seen before
				if IsTransformationMonoid( s ) or not o[i] = one then
					
					m:=PositionProperty(O[j][k]!.truth, x-> x[l]); #the scc containing ker 
					reps:=O[j][k]!.reps[m]; #reps of L-classes corresponding to scc[m] 
																	#partitioned by image.
					schutz:=O[j][k]!.schutz[m];
					
					if not IsBound(O[j][k]!.rels[l]) then 
						#we never considered this scc before!
						O[j][k]!.trees[m]:=CreateSchreierTreeOfSCC(O[j][k], m);
						O[j][k]!.reverse[m]:=CreateReverseSchreierTreeOfSCC(O[j][k], m);
						MultipliersOfSCCOfKernelOrbit(gens, O[j][k], m);
						x:= O[j][k]!.rels[l][2]*o[i]; #ker(x)=O[j][k][scc[m][1]]
						schutz[Length(schutz)+1]:=[SchutzenbergerGroupOfSCCOfKernelOrbit(gens, 
						 O[j][k], x, m)];;
						img:=ImageSetOfTransformation(x);
						ht2 := HTCreate(img, 
						 rec( hfd := 100003, treehashsize := 100003 ));
						HTAdd(ht2, img, 1);
						O[j][k]!.images_ht[m]:=ht2;
						reps[Length(reps)+1]:=[x];
						data[Length(data)+1]:=[j, k, l, m, 1, 1];
						new:=true; 
		
					else #we have considered scc before
						images_ht:=O[j][k]!.images_ht[m];
						#if Length(scc[m])=1 then do nothing fi;
						x:= O[j][k]!.rels[l][2]*o[i]; #ker(x)=O[j][k][scc[m][1]]
						val:=HTValue(images_ht, ImageSetOfTransformation(x));
						
						if not val=fail then #image seen before
							#schutz:=O[j][k]!.schutz[m][val][1];
							if not ForAny([1..Length(reps[val])], y-> schutz[val][y][1]=true or 
							 SiftedPermutation(schutz[val][y][1], 
							 PermLeftQuoTransformationNC(reps[val][y], x))=()) then 
								reps[val][Length(reps[val])+1]:=x;
								data[Length(data)+1]:=[j, k, l, m, val, Length(reps[val])];
								schutz[val][Length(schutz[val])+1]:=
								 SchutzenbergerGroupOfSCCOfKernelOrbit(gens, O[j][k], x, m);
								new:=true;
							fi;
						else #new image
							schutz[Length(schutz)+1]:=[SchutzenbergerGroupOfSCCOfKernelOrbit(gens, 
							 O[j][k], x, m)];
							reps[Length(reps)+1]:=[x];
							data[Length(data)+1]:=[j, k, l, m, Length(reps), 1];
							HTAdd(images_ht, ImageSetOfTransformation( x ), 
							 Length(reps));
							new:=true; 
						fi;
					fi;
				fi;
			fi;
			
			if new then #install new pts in the orbit
				
				for y in [1..Length(gens)] do
					z:=x*gens[y]; 
					if HTValue(ht, z)=fail then  
						HTAdd(ht, z, true);
						o[Length(o)+1]:=z;
					fi;
				od;
				
				if IsTransformationMonoid( s ) or not o[i] = one then 
					return x;
				fi;
			fi;
			
			return false;
			end
			######################################################################
));

SetIsIteratorOfLClassReps(iter, true);
SetUnderlyingSemigroupOfIterator(iter, s);

return iter;
end);

# new for 3.2!
############################################################################

InstallGlobalFunction(LClassKernelOrbitFromData,
function(s, d)
#Info(InfoMonoidGreens, 4, "LClassKernelOrbitFromData");
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
end);

# new for 3.2!
############################################################################

InstallGlobalFunction(LClassRepFromData,
function(s, d)
#Info(InfoMonoidGreens, 4, "LClassRepFromData");
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]]!.reps[d[4]][d[5]][d[6]];
end);

# new for 3.2!
############################################################################

InstallGlobalFunction(LClassSchutzGpFromData, 
function(s, d)
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]]!.schutz[d[4]][d[5]][d[6]][2];
end);

# new for 3.2!
############################################################################

InstallGlobalFunction(LClassStabChainFromData, 
function(s, d)
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]]!.schutz[d[4]][d[5]][d[6]][1];
end);

# new for 3.2!
############################################################################

InstallGlobalFunction(LClassSCCFromData,
function(s,d)
#Info(InfoMonoidGreens, 4, "LClassSCCFromData");
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]]!.scc[d[4]];
end);


#############################################################################
# j is the index of the scc we are computing the multipliers for!

#JDM we could just store the words here and use EvaluateWord whenever the 
# actual element is required?

InstallGlobalFunction(MultipliersOfSCCOfKernelOrbit,
function(gens, o, j)
local rels, scc, i, f, g, k, tup, h;

#Info(InfoMonoidGreens, 4, "MultipliersOfSCCOfKernelOrbit");

rels:=o!.rels;
scc:=o!.scc[j];

for i in scc do
	f:=EvaluateWord(gens, Reversed(TraceSchreierTreeOfSCCForward(o, j, i)));
	# OnKernelAntiAction(o[scc[1]], f)=o[i]
	g:=EvaluateWord(gens, Reversed(TraceSchreierTreeOfSCCBack(o, j, i)));
	# OnKernelsAntiAction(o[i], g)=o[scc[1]] 
	#reversed as we have a left action not right as in R-classes!

	#JDM check the efficiency of the following, alternatives: 
	# - could multiply g by some permutation!?
	# - take the inverse of f as a binary relation..
	# - instead of doing this just find the order of the permutation on o[scc[1]]
	#   corresponding to f*g
	# - use a while loop...

	tup:=OnTuplesOfSetsAntiAction(OnTuplesOfSetsAntiAction(o[scc[1]], f), g);
	if not tup=o[scc[1]] then 
		g:=g*(f*g)^(Order(PermListList(tup, o[scc[1]]))-1);
	fi;
	
	#tup:=OnTuplesOfSetsAntiAction(OnTuplesOfSetsAntiAction(o[i], g), f);
	#if not tup=o[i] then 
	#	f:=f*(g*f)^(Order(PermListList(tup, o[i]))-1);
	#fi;
	
	rels[i]:=[f,g];
od;

return rels;
end);

#############################################################################
#

InstallGlobalFunction(NrLClassesOrbitsOfKernels,
function(s)
local i, j, k, l, m, c;

#Info(InfoMonoidGreens, 4, "NrLClassesOrbitsOfKernel");

c:=OrbitsOfKernels(s);
m:=[];

c:=c!.orbits;

for i in c do
  for j in i do 
    for k in j!.reps do 
      for l in k do 
        Add(m, Length(l));
      od;
    od;
  od;
od;

m:=Sum(m);

if OrbitsOfKernels(s)!.finished then 
	SetNrGreensLClasses(s, m);
fi;
return m;
end);

#############################################################################
# would it be better to have this stored in the (empty) record used to 
# create <s> in SemigroupByGenerators, to avoid method selection? Since
# this is called so so many times it might be worth it... JDM for MN

InstallMethod(OrbitsOfKernels, "for a trans. semigroup",
[IsTransformationSemigroup], 
function(s)
local gens, n, one, ht;

#Info(InfoMonoidGreens, 4, "OrbitsOfKernels");

if IsTransformationMonoid( s ) then
	gens := GeneratorsOfMonoid( s );
else
	gens := GeneratorsOfSemigroup( s );
fi;

n := DegreeOfTransformationSemigroup( s );
one := TransformationNC( [ 1 .. n ] );
ht := HTCreate(one);
ht!.o := [one];

return rec(
  finished:=false,
  orbits:=EmptyPlist(DegreeOfTransformationSemigroup(s)), 
  at:=0, 
  gens:=gens,
  s:=s,
	deg := n,
	one := one,
	ht:=ht,
	data:=[]
);
end);

# new for 3.2!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfLClassReps], 
function(iter)
local O, s;

s:=UnderlyingSemigroupOfIterator(iter);
O:=OrbitsOfKernels(s);

Print( "<iterator of L-class reps, ", Length(O!.ht!.o), " candidates, ", 
 SizeOrbitsOfKernels(s), " elements, ", NrLClassesOrbitsOfKernels(s), 
 " L-classes>");
return;
end);

# new for 3.2!
#############################################################################
#

#gens are the generators of the semigroup
#o is orbit
#f is a representative of scc with index k
#k is the index of scc containing index of image of f

# JDMJDMJDM we require one schutz group per sublist in 
# o!.reps[m] as otherwise in the iterator we are performing the wrong comparison
# when we check SiftedPermutation

InstallGlobalFunction(SchutzenbergerGroupOfSCCOfKernelOrbit,
function(gens, o, f, k) 
local scc, bound, g, rels, t, graph, is_sym, i, j;

#Info(InfoMonoidGreens, 4, "SchutzenbergerGroupOfSCCOfKernelOrbit");

scc:=o!.scc[k];

if Length(o[scc[1]])<1000 then 
	bound:=Factorial(Length(o[scc[1]]));
else
	bound:=infinity;
fi;

g:=Group(());
rels:=o!.rels;
t:=o!.truth;
graph:=OrbitGraph(o);
is_sym:=false;

for i in scc do 
	for j in [1..Length(gens)] do 
		if IsBound(graph[i][j]) and t[k][graph[i][j]] then
  		g:=ClosureGroup(g,  PermLeftQuoTransformationNC(f, rels[graph[i][j]][2] * 
  		 (gens[j] * (rels[i][1] * f))));
		fi; 
		if Size(g)>=bound then 
		  is_sym:=true;
			break;
		fi;
	od;
	if Size(g)>=bound then 
		break;
	fi;
od;

if not is_sym then 
	return [StabChainImmutable(g), g];
else
	return [is_sym, g];
fi;
end);

# new for 3.2!
#############################################################################
# returns the size of the semigroup so far

InstallGlobalFunction(SizeOrbitsOfKernels, 
function(s)
local i, o, j, c, k, l;
i:=0;

#Info(InfoMonoidGreens, 4, "SizeOrbitsOfKernels");

c:=OrbitsOfKernels(s)!.orbits;

for o in Concatenation(Compacted(c)) do 
  for j in [1..Length(o!.scc)] do
    if IsBound(o!.schutz[j]) then 
      for k in [1..Length(o!.schutz[j])] do 
      	for l in o!.schutz[j][k] do 
      		#Error("");
      		i:=i+Size(l[2])*Length(o!.scc[j]);
      	od;
      od;
    fi;
  od;
od;

#if OrbitsOfKernels(s)!.finished and not HasSize(s) then 
#	SetSize(s, i);
#fi; JDM

return i;
end);