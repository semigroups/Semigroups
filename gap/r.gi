#############################################################################
##
#W  greens_r_orb.gi
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

# - this file should only contain functions relating to images/r-classes!

# - consider storing the scc of an R-class in the R-class, it seems that 
# a third of the time taken in Idempotents is spent in line JDM1

# - check that wherever we have called d:=InOrbitsOfImages we do not perform
# f*perms[l] afterwards but instead use d[7]!


#############################################################################
## To do 

# - install IteratorOfRClassRepsData etc for s and IsPosInt!

# - make test files

# - remove all functions supporting the old methods etc for GreensRClasses/
#  images... 

# - check other functions in other files to see if they still work (or not!)

# - write documentation

##
#############################################################################

InstallMethod( \=, "for R-class and R-class of trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp, IsGreensRClass and 
IsGreensClassOfTransSemigp],
function(r1, r2)

return r1!.parent=r2!.parent and r1!.rep in r2;
end);

##
#############################################################################

InstallMethod( \=, "for trans. semigp. and trans. semigp.",
[IsTransformationSemigroup, IsTransformationSemigroup],
function(s1, s2)
return ForAll(Generators(s1), x-> x in s2) and 
 ForAll(Generators(s2), x-> x in s1);
end);

## new for 4.0!
#############################################################################
##  Algorithm E. 

InstallMethod( \in, "for trans. and R-class of trans. semigp.", 
[IsTransformation, IsGreensRClass and IsGreensClassOfTransSemigp],
function(f, r)
local rep, d, o, i, schutz, s;

rep:= r!.rep; 

if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or
 RankOfTransformation(f) <> RankOfTransformation(rep) or
 KernelOfTransformation(f) <> KernelOfTransformation(rep) then
	return false;
fi;

if f=rep then 
	return true;
fi;

d:=r!.data;
s:=r!.parent;
o:=r!.o!.orbits[d[1]][d[2]];

i:= Position(o, ImageSetOfTransformation(f));

if i = fail or not o!.truth[d[4]][i] then #check they are in the same scc
	return false;
fi;

schutz:= RClassStabChain(r);

return schutz=true or SiftedPermutation(schutz, 
 PermLeftQuoTransformationNC(rep, f*o!.perms[i]))=();
end);


# new for 4.0!
#############################################################################
# not algorithm X.

InstallMethod(\in, "for a transformation semigroup", 
[IsTransformation, IsTransformationSemigroup],
function(f, s)
local gens, g, o, iter, orbits;

#Info(InfoMonoidGreens, 4, "\in: for a trans. semigroup");

if HasAsSSortedList(s) then 
	return f in AsSSortedList(s);
fi;

gens:=Generators(s);

if not DegreeOfTransformation(f) = DegreeOfTransformation(gens[1]) then 
	return false;
fi;

o:=OrbitsOfImages(s);
orbits:=o!.orbits;
g:=InOrbitsOfImages(s, f, orbits, []);

if g[1] then 
	return true;
elif o!.finished then 
	return false;
fi;

# check what's already known...
iter:=IteratorOfNewRClassRepsData(s);

repeat
	NextIterator(iter);
	g:=InOrbitsOfImages(s, f, orbits, g[2]);

	if g[1] then 
		return true;
	fi;
until IsDoneIterator(iter);

#JDM could also put something in here that returns false if everything,
#from OrbitsOfImages(s)!.at to the end of OrbitsOfImages(s)!.ht!.o 
#has rank less than f. Might be a good idea when degree is very high!

return false;
end);

#############################################################################
# s <- old semigroup, t <- new semigroup, new <- new generators,
# j, k from O[j][k]

# JDM clean the following up! It's an extra mess due to the fact you can't make
# structural copies of hash tables!

InstallGlobalFunction(AddGeneratorsToOrbitsOfImages, 
function(s, t, new, j, k)
local O, o, filt, scc, r, m, old_scc, old_reps, ht, data, n, d, i, f, y, z, oo,
 o_t, val, l, reps, gens, g, h, kernels_ht, max;

O:=RClassImageOrbitFromData(s, [j,k]);
o:=StructuralCopy(O);
o!.ht:=HashTableForImage(o[1]);
for i in [1..Length(O)] do 
	HTAdd(o!.ht, O[i], i);
od;

AddGeneratorsToOrbit(o, new);

gens:=o!.gens;

Unbind(o!.truth); Unbind(o!.trees);
Unbind(o!.reps); 
o!.kernels_ht:=EmptyPlist(Length(O!.scc));
o!.perms:=EmptyPlist(Length(o));

filt:=function(o, scc) return not ForAny(OrbitsOfImages(t)!.orbits[j],
 x-> o[scc[1]] in x); end;

if Length(O!.scc)>1 or Length(o)>Length(O) then 
	scc:=Set(List(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(
	 OrbitGraphAsSets(o)), Set));;
	scc:=Filtered(scc, x-> filt(o,x));
else
	scc:=StructuralCopy(O!.scc);
fi;

r:=Length(scc);
o!.schutz:=List([1..r], x-> []);
o!.reps:=List([1..r], x-> []);
o!.scc:=scc;

o!.truth:=List([1..r], i-> BlistList([1..Length(o)], scc[i]));
o!.trees:=List([1..r], x-> CreateSchreierTreeOfSCC(o, x));

OrbitsOfImages(t)!.orbits[j][k]:=o;
ht:=OrbitsOfImages(t)!.ht;
data:=OrbitsOfImages(t)!.data;

for m in [1..r] do 
  old_scc:=Filtered([1..Length(O!.scc)], i-> O!.scc[i][1] in scc[m]);

  if not old_scc=[] then #scc contains an old scc
		old_reps:=StructuralCopy(O!.reps{old_scc});
		max:=List(old_scc, m-> Length(O!.reps[m]));
		max:=PositionProperty(old_scc, m-> Length(O!.reps[m])=Maximum(max));
		
		#kernels_ht:=O!.kernels_ht[old_scc[max]];
		kernels_ht:=HashTableForKernels(KernelOfTransformation(
		 old_reps[max][1][1]));
		
		for i in [1..Length(old_reps[max])] do 
			HTAdd(kernels_ht, KernelOfTransformation(old_reps[max][i][1]), i);
		od;

		o!.kernels_ht[m]:=kernels_ht;
		data:=Concatenation(data, List([1..Length(old_reps[max])], val-> 
		 [j, k, scc[m][1], m, val, 1]));
		
		o!.perms:=o!.perms+MultipliersOfSCCOfImageOrbit(gens, o, m);
		#JDM use old perms as far as possible!
		for i in [1..Length(old_scc)] do 
			old_reps[i]:=List(old_reps[i], x-> x*o!.perms[O!.scc[old_scc[i]][1]]);
		od;
		
		#old_reps[max]:=List(old_reps[max], x-> x*o!.perms[O!.scc[old_scc[max]][1]]);
		o!.schutz[m]:=SchutzGpOfImageOrbit(gens, o, old_reps[max][1][1], m);
		#reuse old schutz gp! JDM prev.line was o!.reps[m][1][1]
		
		if Size(o!.schutz[m][2])=Size(O!.schutz[old_scc[max]][2]) then 
			o!.reps[m]:=old_reps[max];
			old_reps:=old_reps{Concatenation([1..max-1], [max+1..Length(old_reps)])};
		else
			for i in [1..Length(old_reps[max])] do 
				o!.reps[m][Length(o!.reps[m])+1]:=[old_reps[max][i][1]];
				old_reps[1][i]:=old_reps[max][i]{[2..Length(old_reps[max][i])]};
			od;
		fi;
		
		for f in Concatenation(o!.reps[m]) do 
			for g in new do  
				h:=g*f;
				val:=HTValue(ht, h);
				if val=fail then 
					HTAdd(ht, h, true);
					ht!.o[Length(ht!.o)+1]:=h;
				fi;
			od;
		od;
		
		if not old_reps=[] and not o!.schutz[m]=true then 
		
			o_t:=OrbitsOfImages(t)!.orbits;
			old_reps:=List(old_reps, Concatenation); 
			
			for i in [1..Length(old_reps)] do
				for n in [1..Length(old_reps[i])] do 
					f:=old_reps[i][n];
					d:=InOrbitsOfImages(t, f, o_t, [j, k, scc[m][1], m, fail, 0]); 
					if not d[1] then #AddToOrbitsOfImages
						val:=d[2][5]; n:=d[2][6]; reps:=o!.reps[m];
						if not val=fail then #old kernel
							reps[val][n+1]:=f;
							data[Length(data)+1]:=[j, k, scc[m][1], m, val, n+1];
						else #new kernel
							val:=Length(reps)+1;
							reps[val]:=[f];
							data[Length(data)+1]:=[j, k, scc[m][1], m, val, 1];
							HTAdd(kernels_ht, KernelOfTransformation( f ), val);
						fi;
						
						for g in new do  
							h:=g*f;
							val:=HTValue(ht, h);
							if val=fail then 
								HTAdd(ht, h, true);
								ht!.o[Length(ht!.o)+1]:=h;
							fi;
						od;
					fi;
				od;
			od;
		fi;
	else #new points in the orbit!
		f:=o!.reps[1][1][1]*EvaluateWord(gens, 
		 TraceSchreierTreeForward(o, scc[m][1]));
		o!.reps[m]:=[[f]];
		o!.kernels_ht[m]:=HashTableForKernels(
		 KernelOfTransformation(f));
		o!.perms:=o!.perms+MultipliersOfSCCOfImageOrbit(gens, o, m);
		o!.schutz[m]:=SchutzGpOfImageOrbit(gens, o, f, m);

		#install descendants of f in OrbitsOfImages(t)!.ht
		ht:=OrbitsOfImages(t)!.ht; oo:=ht!.o;
		for y in [1..Length(gens)] do
			z:=gens[y]*f;
			if HTValue(ht, z)=fail then 
				HTAdd(ht, z, true);
				oo[Length(oo)+1]:=z;
				#schreier words here JDM
			fi;
		od;

		data[Length(data)+1]:=[j,k,scc[m][1],m,1,1];
	fi;
od;

return true;
end);


#############################################################################
# s <- semigroup or d-class; f <- transformation; o <- OrbitsOfImages(s); 
# data <- img data

# if s is a d-class, then data should have j, k, l, m and g set!

InstallGlobalFunction(AddToOrbitsOfImages,
function(s, f, o, data)
local j, k, l, m, val, n, g, O, one, gens, d, reps, schutz, img, scc, i, 
 oo, r, ht, y, z, out, deg, bound, treehashsize, images;

j:=data[1]; 	# img size
k:=data[2]; 	# index of orbit containing img
l:=data[3]; 	# position of img in O[j][k]
m:=data[4]; 	# scc of O[j][k] containing img
val:=data[5]; # position of ker in O[j][k]!.kernels_ht[m]
n:=data[6]; 	# the length of O[j][k]!.reps[m][val]
g:=data[7];		# f*O[j][k]!.perms[l];

O := o!.orbits;  gens:=o!.gens; d:=o!.data;

if IsBound(o!.ht) then #o = OrbitsOfImages(s)
	one:=o!.one;
	images:=o!.images; #JDM new!
	ht:=o!.ht; o:=ht!.o;
fi;

if k = fail then #new img and l, m, val, n, g=fail
								 #don't call this function with a d-class and k=fail!

################################################################################

	if IsBound(O[j]) then 
		oo:=ForwardOrbitOfImage(s, f, images[j], gens);
		Add(O[j], oo[1]);
	else
		images[j]:=HTCreate(ImageSetOfTransformation(f));
		oo:=ForwardOrbitOfImage(s, f, images[j], gens);
		O[j]:=[oo[1]];
	fi;
	
	#JDM new!
	for i in oo[1] do 
		HTAdd(images[j], i, true);
	od;
	#JDM new ends

	reps:=oo[2];
	out:=[j, Length(O[j]), 1, 1, 1, 1];
	
	for m in [1..Length(oo[3])] do 
		d[Length(d)+1]:=[j, Length(O[j]), oo[3][m][1], m, 1, 1];
	od;

##############################################################################

else #old img
	reps:=O[j][k]!.reps[m];
	
	if not val=fail then #old kernel
		reps[val][n+1]:=g;
		out:=[j, k, l, m, val, n+1];
		d[Length(d)+1]:=out;
	else #new kernel
		val:=Length(reps)+1;
		reps[val]:=[g];
		out:=[j, k, l, m, val, 1];
		d[Length(d)+1]:=out;
		HTAdd(O[j][k]!.kernels_ht[m], KernelOfTransformation( g ), val);
	fi;
	reps:=[g]; #JDM g or f?
fi;

##############################################################################

#install new pts in the orbit
if IsBound(ht) then 
	for f in reps do 
		for y in [1..Length(gens)] do
			z:=gens[y]*f;
			if HTValue(ht, z)=fail then  
				HTAdd(ht, z, true);
				o[Length(o)+1]:=z;
				#schreier words here
			fi;
		od;
	od;
fi;

return out;
end);

# new for 4.0!
#############################################################################
# Algorithm D.
# JDM check for efficiency!

InstallOtherMethod(AsList, "for an R-class of trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
local f, s, d, h, elts, p, o;

Info(InfoMonoidGreens, 4, "AsList: for an R-class");

f:=r!.rep; #rep should have its image at the first place in the scc
s:=r!.parent;
d:=r!.data;
o:=r!.o[d[1]][d[2]];

h:=List(SchutzenbergerGroup(r), x-> f*x);

elts:=[];

for p in RClassPermsFromData(s, d, o){RClassSCC(r)} do 
	elts:=Concatenation(elts, h*p^-1);
od;
return elts;
end);

# new for 4.0!
#############################################################################
# this should be removed after the library method for AsSSortedList 
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)!

InstallOtherMethod(AsSSortedList, "for R-class of trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
Info(InfoMonoidGreens, 4, "AsSSortedList: for an R-class");
return ConstantTimeAccessList(EnumeratorSorted(r));
end);

# new for 4.0!
#############################################################################

InstallGlobalFunction(ClosureSemigroup,
function(s, new)
return ClosureSemigroupNC(s, Filtered(new, x-> not x in s));
end);

# new for 4.0!
#############################################################################
# this should be properly installed and tested!! JDM
# JDM clean this up!

InstallGlobalFunction(ClosureSemigroupNC,
function(s, new)
local t, o_s, o_t, j, n, orbits, gens, i, k, type, data, ht, val, g, h, f, l, 
 o, d;

if new=[] then 
	return s;
fi;

if IsTransformationMonoid(s) then 
	t:=Monoid(Concatenation(Generators(s), new));
else
	t:=Semigroup(Concatenation(Generators(s), new));
fi;

# initialize the R-class reps orbit!
###############################################################################

o_s:=OrbitsOfImages(s);
ht:= HTCreate(new[1]);;
HTAdd(ht, o_s!.one, true);

for f in new do 
	HTAdd(ht, f, true);
od;

ht!.o:= Concatenation([o_s!.one], new); 

for i in [o_s!.at+1..Length(o_s!.ht!.o)] do 
	g:=o_s!.ht!.o[i];
	val:=HTValue(ht, g);
	if val=fail then 
		HTAdd(ht, g, true);
		ht!.o[Length(ht!.o)+1]:=g;
	fi;
od;

###############################################################################

n:=o_s!.deg;

o_t:= Objectify(NewType(FamilyObj(t), IsOrbitsOfImages), 
rec( finished:=false,
     orbits:=EmptyPlist(n), 
     images:=EmptyPlist(n),
     at:=0, 
     gens:=Generators(t),
     s:=t,
     deg := n, 
     one := o_s!.one,
     ht:=ht,
     data:=EmptyPlist(Length(o_s!.data)), 
));

SetOrbitsOfImages(t, o_t);

###############################################################################

j:=Maximum(List(new, Rank));
orbits:=o_t!.orbits;
data:=o_t!.data;

for i in [n,n-1..1] do 
	if IsBound(o_s!.orbits[i]) then 
		if i>j then 
			orbits[i]:=StructuralCopy(o_s!.orbits[i]);
			Append(data, RClassRepsDataFromOrbits(orbits[i], i));
			#JDM could avoid using RClassRepsDataFromOrbits
			# if the data was stored in the orbits when created. 
		else
			orbits[i]:=[];
			for k in [1..Length(o_s!.orbits[i])] do 
				AddGeneratorsToOrbitsOfImages(s, t, new, i, k);
			od;
		fi;
	fi;
od;

return t;

end);

# new for 4.0!
#############################################################################
#

InstallGlobalFunction(CreateRClass, 
function(s, data, orbit, rep)
local r;

data:=data{[1..6]};

r:=Objectify(RClassType(s), rec(parent:=s, data:=data, 
o:=orbit, rep:=rep));

SetRepresentative(r, rep);
SetEquivalenceClassRelation(r, GreensRRelation(s));
return r;
end);

# new for 4.0!
#############################################################################
# o is the orbit
# i is the index of the scc of o we are trying to create the Schreier tree for!

# this should be moved to greens.gi

InstallGlobalFunction(CreateSchreierTreeOfSCC,
function(o, i)
local gen, pos, seen, oo, graph, j, k, l, scc, t;

#Info(InfoMonoidGreens, 4, "CreateSchreierTreeOfSCC");

if i=1 then 
  return [o!.schreiergen, o!.schreierpos];
fi;

scc:=o!.scc[i];

gen:=List([1..Length(o)], x-> fail);
pos:=List([1..Length(o)], x-> fail);
seen:=BlistList([1..Length(o)], [scc[1]]);
t:=o!.truth[i];
oo:=[scc[1]];
graph:=OrbitGraph(o);
j:=0;

while Length(oo)<Length(scc) do 
  j:=j+1;
	k:=oo[j];
	l:=0;
	while l<Length(graph[k]) and Length(oo)<Length(scc) do  
		l:=l+1;
		if IsBound(graph[k][l]) and not seen[graph[k][l]] and t[graph[k][l]] then 
			Add(oo, graph[k][l]); seen[graph[k][l]]:=true; 
			gen[graph[k][l]]:=l; pos[graph[k][l]]:=k;
		fi;
	od;
od;

return [gen, pos];
end);

# new for 4.0!
#############################################################################
#

# this should be moved to greens.gi

InstallGlobalFunction(CreateReverseSchreierTreeOfSCC,
function(o, i)
local rev, j, k, l, m, graph, scc, gen, pos, seen, t, oo; 

#Info(InfoMonoidGreens, 4, "CreateReverseSchreierTreeOfSCC");

graph:=OrbitGraph(o);
rev:=List([1..Length(graph)], x-> List([1..Length(o!.gens)], x-> []));

for j in [1..Length(graph)] do
  for k in [1..Length(graph[j])] do 
    if IsBound(graph[j][k]) then 
      Add(rev[graph[j][k]][k], j);
      #starting at position j and applying gens[k] we obtain graph[j][k];
    fi;
  od;
od;

scc:=o!.scc[i];

gen:=List([1..Length(o)], x-> fail);
pos:=List([1..Length(o)], x-> fail);
seen:=BlistList([1..Length(o)], [scc[1]]);
t:=o!.truth[i];
oo:=[scc[1]];

j:=0;

while Length(oo)<Length(scc) do 
  j:=j+1;
  k:=oo[j];
  l:=0;
  while l< Length(rev[k]) and Length(oo)<Length(scc) do 
    l:=l+1;
    m:=0;
    while m< Length(rev[k][l]) and Length(oo)<Length(scc) do 
      m:=m+1;
      if not seen[rev[k][l][m]] and t[rev[k][l][m]] then 
        Add(oo, rev[k][l][m]); seen[rev[k][l][m]]:=true;
        gen[rev[k][l][m]]:=l; pos[rev[k][l][m]]:=k;
      fi;
    od;
  od;
od;


return [gen, pos];
end);

#new for 4.0!
#############################################################################

InstallGlobalFunction(DisplayOrbitsOfImages, 
function(s)
local o;

o:=OrbitsOfImages(s);

Print("finished: ", o!.finished, "\n");
Print("orbits: "); View(o!.orbits); Print("\n");
Print("at: ", o!.at, "\n");
Print("ht: "); View(o!.ht); Print("\n");
Print("size: ", SizeOrbitsOfImages(s), "\n");
Print("R-classes: ", NrRClassesOrbitsOfImages(s), "\n");
return true;
end);

#new for 4.0!
#############################################################################
#JDM remove this...

InstallGlobalFunction(DisplayOrbitOfImage, 
function(o)

View(o); Print("\n");
Print(o!.scc, "\n");
Print(o!.schutz, "\n");
Print(o!.reps, "\n");

return true;
end);

# new for 4.0!
#############################################################################
#

InstallOtherMethod(Enumerator, "for R-class of trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
local enum, h;

Info(InfoMonoidGreens, 4, "Enumerator: for an R-class");

h:=List(Elements(SchutzenbergerGroup(r)), x-> r!.rep*x);

enum:=EnumeratorByFunctions(r, rec(
	
	rep:=r!.rep,
	
	h:=h, 
	
	len:=Length(h),
	
	p:=RClassPermsFromData(r!.parent, r!.data, r!.o),
	
	scc:=RClassSCC(r),
	
	###########################################################################
	
	ElementNumber:=function(enum, pos)
	local q, n, m;
		if pos>Length(enum) then 
			return fail;
		fi;
		
		if pos<=enum!.len then 
			return enum!.h[pos];
		fi;
		
		n:=pos-1;
		m:=enum!.len;
		
    q := QuoInt(n, m);
    pos:= [ q, n - q * m ]+1;

		return enum!.h[pos[2]]*enum!.p[enum!.scc[pos[1]]]^-1;
	end, 
	
	###########################################################################
	
	NumberElement:=function(enum, f)
		local rep, d, s, o, i, j;
		rep:=enum!.rep;
	
		if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or
		 RankOfTransformation(f) <> RankOfTransformation(rep) or
		 KernelOfTransformation(f) <> KernelOfTransformation(rep) then
			return fail;
		fi;
		
		if f=rep then 
			return 1;
		fi;
		
		d:=r!.data;
		s:=r!.parent;
		
		# check image is in the same weak orbit
		o:= RClassImageOrbit(r);
		i:= Position(o, ImageSetOfTransformation(f));
		
		if i = fail or not o!.truth[d[4]][i] then #check they are in the same scc
			return fail;
		fi;
		
		j:= Position(Elements(SchutzenbergerGroup(r)),
		 PermLeftQuoTransformationNC(rep, f*o!.perms[i]));
		
		if j = fail then 
			return fail;
		fi;
		
		return Length(enum!.h)*(Position(enum!.scc, i)-1)+j;

	end, 

	###########################################################################
	
	Membership:=function(elm, enum) 
	return elm in UnderlyingCollection(enum); #the R-class itself!
	end,
	
	Length:=enum -> Size(r),

	PrintObj:=function(enum)
	Print( "<enumerator of R-class>");
	return;
	end));

return enum;
end);

#new for 4.0!
#############################################################################

InstallOtherMethod(Enumerator, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local out, iter, i;

Info(InfoMonoidGreens, 4, "Enumerator: for a trans. semigroup");

out:=[];
iter:=Iterator(s);

for i in iter do 
	out[Length(out)+1]:=i;
od;

return Immutable(out);
end);

#new for 4.0!
#############################################################################
# finds all orbits of images!!

InstallGlobalFunction(ExpandOrbitsOfImages, 
function(s)
local o, iter, i;

Info(InfoMonoidGreens, 4, "ExpandOrbitsOfImages");

o:=OrbitsOfImages(s);

if not o!.finished then 
	iter:=IteratorOfNewRClassRepsData(s);
	for i in iter do od;
fi;

return true;
end);

#new for 4.0!
#############################################################################

InstallGlobalFunction(ForwardOrbitOfImage, 
function(arg)
local s, f, images, img, deg, j, bound, treehashsize, o, scc, r, reps, gens, i;

s:=arg[1]; f:=arg[2];

if Length(arg)>=3 then 
	images:=arg[3];
else
	images:=fail;
fi;

if Length(arg)=4 then 
	gens:=arg[4];
else
	if IsTransformationMonoid(s) then 
		gens:=GeneratorsOfMonoid(s);
	else
		gens:=GeneratorsOfSemigroup(s);
	fi;
fi;

img:=ImageSetOfTransformation(f);
deg:=DegreeOfTransformationSemigroup(s);
j:=Length(img);

if deg<1000 then 
	bound:=Binomial(DegreeOfTransformationSemigroup(s), j);
	treehashsize:=3*bound;
else
	bound:=infinity;
	treehashsize:=100000;
fi;
	
o:=Orb(s, img, OnSets, rec(
				treehashsize:=NextPrimeInt(Minimum(100000, treehashsize)), 
				schreier:=true,
				gradingfunc := function(o,x) return [Length(x), x]; end, 
				orbitgraph := true, 
				onlygrades:=function(x, y) 
				return x[1]=j and (y=fail or HTValue(y, x[2])=fail); end,
				onlygradesdata:=images,
				storenumbers:=true, 
				log:=true));

SetIsMonoidPkgImgKerOrbit(o, true);
Enumerate(o, bound);
	
#strongly connected components
scc:=Set(List(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(OrbitGraphAsSets(o)), Set));;

#if not filt=fail then 
	#JDM put in the grading function of the orbit possible! 
	# when running IteratorOfRClassRepsData we could benefit
	# from adding the filt to the grading in the orbit.
	#scc:=Filtered(scc, x-> filt(o,x));
#fi;

r:=Length(scc);
o!.scc:=scc;

#boolean list corresponding to membership in scc[i]
o!.truth:=List([1..r], i-> BlistList([1..Length(o)], scc[i]));
o!.trees:=List([1..r], x-> CreateSchreierTreeOfSCC(o,x));

#representatives of R-classes with image belonging in scc[i] partitioned 
#according to their kernels
reps:=List([1..r], m-> 
 f*EvaluateWord(gens, TraceSchreierTreeForward(o, scc[m][1])));
o!.reps:=List(reps, x->[[x]]); 

#kernels of representatives of R-classes with image belonging in scc[i]
o!.kernels_ht:=List([1..r], m-> 
 HashTableForKernels(KernelOfTransformation(reps[m])));

#calculate the multipliers for all scc's 
o!.perms:=EmptyPlist(Length(o));
for i in [1..r] do 
	o!.perms:=o!.perms+MultipliersOfSCCOfImageOrbit(gens, o, i);
od;

#schutzenberger groups
o!.schutz:=List([1..r], m-> SchutzGpOfImageOrbit(gens, o, reps[m], m));

return [o, reps, scc];
end);

# new method in 4.0!
#############################################################################
#

InstallMethod(GreensRClassData, "for a R-class of a trans. semigroup",
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
local rep, d, s, scc, l, o, p, g;

Info(InfoWarning, 1, "this is a legacy from Monoid 3.*");

rep:=r!.rep;
d:=r!.data;
s:=r!.parent;

scc:=RClassSCC(r);
l:=Position(scc, d[3]);
o:=RClassImageOrbit(r){scc};
p:=RClassPerms(r){scc};
g:=SchutzenbergerGroup(r);

#d[3] is the index of the scc containing rep!
if not l=1 then 
	o:=Concatenation(o{[l..Length(o)]}, o{[1..l-1]});
	p:=List(Concatenation(p{[l..Length(p)]}, 
	p{[1..l-1]}), x-> x*p[l]^-1);
	g:=g^(p[1]^-1);
fi;

return RClassData(rec( rep:=rep, strongorb:=o, 
perms:=p, schutz:=g));;
end);

# new for 4.0!
#############################################################################
# JDM test the efficiency of this function!

InstallMethod(GreensRClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local iter, out, i, f;

Info(InfoMonoidGreens, 4, "GreensRClasses");

iter:=IteratorOfGreensRClasses(s);
out:=EmptyPlist(NrGreensRClasses(s));
i:=0;

for f in iter do 
	i:=i+1;
	out[i]:=f;
od;

return out;
end);

# new for 4.0!
#############################################################################

InstallOtherMethod(GreensRClassOfElement, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
local d, rep, type, c;

Info(InfoMonoidGreens, 4, "GreensRClassOfElement");

if not f in s then 
	Info(InfoWarning, 1, "transformation is not an element of the semigroup");
	return fail;
fi;

d:=InOrbitsOfImages(s, f)[2];
d[3]:=RClassSCCFromData(s, d)[1];
#d[3]:=1;
#rep:=d[7]; not nec. the rep!

return CreateRClass(s, d, OrbitsOfImages(s), 
 RClassRepFromData(s, d));
end);

# new for 4.0!
#############################################################################

InstallOtherMethod(GreensRClassOfElementNC, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
local d, j, o, r, n, gens;

Info(InfoMonoidGreens, 4, "GreensRClassOfElementNC");

d:=InOrbitsOfImages(s, f);

if d[1] then # f in s!
	Info(InfoMonoidGreens, 2, "transformation is an element of semigroup");
	d:=d[2]; d[3]:=1;
	r:=CreateRClass(s, d, OrbitsOfImages(s), RClassRepFromData(s, d));
	return r;
elif OrbitsOfImages(s)!.finished then #f not in s!
	Info(InfoMonoidGreens, 2, "transformation is not an element of semigroup");
	return fail;
fi;

Info(InfoMonoidGreens, 2, "transformation may not be an element of semigroup");

n := DegreeOfTransformationSemigroup( s );
j:=Length(ImageSetOfTransformation(f));
o:=EmptyPlist(n);
o[j]:=[ForwardOrbitOfImage(s, f, fail)[1]];
# function(o, scc) return scc[1]=1; end

o:=rec( finished:=false, orbits:=o, gens:=Generators(s), s:=s, 
 deg := n, data:=[]);
#local orbits of images!

r:=CreateRClass(s, [j,1,1,1,1,1], o, f);
return r;
end);

# new for 4.0!
#############################################################################

InstallMethod(GreensRClassReps, "for a trans. semigroup", 
[IsTransformationSemigroup], 
function(s)
local iter, i, o;
Info(InfoMonoidGreens, 4, "GreensRClassReps");

ExpandOrbitsOfImages(s);
return List(OrbitsOfImages(s)!.data, x-> 
 RClassRepFromData(s, x));
end);

# new for 4.0!
#############################################################################
# I don't see the need for iterator and enumerator of idempotents, as there
# are just not that many idempotents in general. Or if there are, then 
# we cannot compute the R-class even.... 

InstallOtherMethod( Idempotents, "for a R-class of a trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
local out, ker, rep, n, o, i, img, j;

if HasIsRegularRClass(r) and not IsRegularRClass(r) then 
	return [];
fi;

out:= [];
ker:= KernelOfTransformation(r!.rep);
rep:=r!.rep![1];
n:=Length(Set(rep));
o:=RClassImageOrbit(r){RClassSCC(r)}; #JDM1

for i in o do
	img:=EmptyPlist(n);
	j:=1;
	while j<=n and POS_LIST_DEFAULT(img, rep[i[j]], 0)=fail do
		img[j]:=rep[i[j]];
		j:=j+1; 
	od;
	
	if j=n+1 then 
		out[Length(out)+1]:=IdempotentNC(ker, i); #JDM can't we use transformationNC
	fi;
od;

return out;
end);

#############################################################################
# Usage: s, f, OrbitsOfImages(s)!.orbits, d_img or s, f

InstallGlobalFunction(InOrbitsOfImages, 
function(arg)
local img, j, k, l, m, val, n, g, schutz, t, reps, s, O, f, x;

s:=arg[1]; f:=arg[2]; j:=fail;
k:=fail; l:=fail; m:=fail; val:=fail; n:=0; g:=fail;

if Length(arg)=4 then 
	O:=arg[3];
	if not arg[4]=[] then 
		j:=arg[4][1]; k:=arg[4][2]; l:=arg[4][3];
		m:=arg[4][4]; val:=arg[4][5]; n:=arg[4][6];
		
		if Length(arg[4])=7 then 
			g:=arg[4][7];
		fi;
	fi;

	if k=fail then 
		img:=ImageSetOfTransformation(f);
	fi;
	if j=fail then 
		j:=Length(img);
	fi;
else
	O:=OrbitsOfImages(s)!.orbits;
	img:=ImageSetOfTransformation(f);
	j:=Length(img);
fi;

if not IsBound(O[j]) then
	return [false, [j, fail, fail, fail, fail, 0, fail]];
fi;

if k=fail then #l=fail, m=fail, g=fail
	k:=0;

	repeat
		k:=k+1;
		l:=Position(O[j][k], img);
	until not l=fail or k=Length(O[j]);
	
	if l = fail then 
		return [false, [j, fail, fail, fail, fail, 0, fail]];
	fi;
	m:=PositionProperty(O[j][k]!.truth, x-> x[l]);
	g:=f*O[j][k]!.perms[l];
fi;

if g=fail then #this can happen if coming from GreensRClassReps for example.
	g:=f*O[j][k]!.perms[l];
fi;

if val=fail then 
	val:=HTValue(O[j][k]!.kernels_ht[m], KernelOfTransformation(f));
fi;

if val=fail then 
	return [false, [j, k, l, m, fail, 0, g]];
fi;

schutz:=O[j][k]!.schutz[m][1];

if schutz=true then 
	return [true, [j,k,l,m,val,1,g]];
fi;

reps:=O[j][k]!.reps[m][val];
t:=Length(reps);

while n<t do 
	n:=n+1;
	x:=reps[n];
	if SiftedPermutation(schutz, PermLeftQuoTransformationNC(x, g))=() then 
		return [true ,[j,k,l,m,val,n,g]];
	fi;
od;

return [false, [j,k,l,m,val,n,g]];
end);

# new for 4.0!
#############################################################################
# test further for efficiency in comparison to IsRegularTransformation! JDM

InstallMethod(IsRegularRClass, "for an R-class of trans. semigroup",
[IsGreensClassOfTransSemigp], 
function(r)
local f, img, o, i, m;

if not IsGreensRClass(r) then 
	return false;
fi;

if HasIdempotents(r) then 
	return Length(Idempotents(r))>0; 
fi;

return IsRegularRClassData(r!.parent, r!.data, r!.rep, r!.o);
end);

#############################################################################
# the following exists to avoid creating an R-class before check that it is 
# regular!

InstallGlobalFunction(IsRegularRClassData, 
function(arg)
local f, img, m, i, s, d, o;

s:=arg[1]; d:=arg[2]; f:=arg[3];

if HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then 
  return true;
fi;

if Length(arg)=4 then 
	o:=arg[4];
else
	o:=OrbitsOfImages(s);
fi;

img:= ImageListOfTransformation(f);
m:=Length(ImageSetOfTransformation(f));
o:=RClassImageOrbitFromData(s, d, o){RClassSCCFromData(s, d, o)};

for i in o do
	if Length(Set(img{i})) = m then
		return true;
	fi;
od;

return false;
end);

# new for 4.0!
#############################################################################
# JDM move to greens.gi when that file is updated!

InstallMethod(Iterator, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "Iterator: for a trans. semigroup");

iter:= IteratorByFunctions( rec( 
	
	R:=IteratorOfGreensRClasses(s), 
	
	r:=fail,
	
	NextIterator:=function(iter)
	
	if IsDoneIterator(iter) then 
		return fail;
	fi;
	
	if iter!.r=fail or IsDoneIterator(iter!.r) then 
		iter!.r:=Iterator(NextIterator(iter!.R));
	fi;
	
	return NextIterator(iter!.r);
	end,
	
	IsDoneIterator:= iter -> IsDoneIterator(iter!.R) and IsDoneIterator(iter!.r),
	
	ShallowCopy:= iter -> rec(R:=IteratorOfGreensRClasses(s), r:=fail)
));

SetIsIteratorOfSemigroup(iter, true);

return iter;
end);

# new for 4.0!
#############################################################################
# JDM it is not really clear that this is needed...

InstallMethod(Iterator, "for a R-class of a trans. semigroup",
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
local iter;

Info(InfoMonoidGreens, 4, "Iterator: for an R-class");

if HasAsSSortedList(r) then 
	iter:=IteratorList(AsSSortedList(r));
else
	iter:=IteratorByFunctions(rec( 
	
	schutz:=List(SchutzenbergerGroup(r), x-> r!.rep*x),
	
	perms:=RClassPermsFromData(r!.parent, r!.data, r!.o),
	
	scc:=RClassSCC(r), #JDM better if this was just Length(scc)!
	
	at:=[1,0],
	
	IsDoneIterator:=iter-> iter!.at[1]=Length(iter!.scc) and 
		iter!.at[2]=Length(iter!.schutz),
	
	NextIterator:=function(iter)
	
	if IsDoneIterator(iter) then 
		return fail;
	fi;

	if iter!.at[2]<Length(iter!.schutz) then #JDM better if Length(schutz) stored!
		iter!.at[2]:=iter!.at[2]+1;
	else
		iter!.at[1]:=iter!.at[1]+1; 
		iter!.at[2]:=1;
	fi;
	return iter!.schutz[iter!.at[2]]*iter!.perms[iter!.scc[iter!.at[1]]]^-1;
	end,
	
	ShallowCopy:=iter-> rec(schutz:=iter!.schutz, perms:=iter!.perms, 
	 scc:=iter!.scc, at:=[1,0])
	));
fi;

SetIsIteratorOfRClassElements(iter, true);

return iter;
end);

#############################################################################

InstallOtherMethod(IteratorOfGreensHClasses, "for an R-class", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
Error("not yet implemented!");
end);


# new for 4.0!
#############################################################################

InstallGlobalFunction(IteratorOfGreensRClasses, 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfGreensRClasses");

iter:=IteratorByFunctions( rec(
	
	i:=0,
	
	s:=s, 
	
	reps:=IteratorOfRClassReps(s),
	
	IsDoneIterator := iter -> IsDoneIterator(iter!.reps), 
	
	NextIterator:= function(iter)
	local c, rep, d;
	
	rep:=NextIterator(iter!.reps);
	
	if rep=fail then 
		return fail;
	fi;
	
	iter!.i:=iter!.i+1;
	d:=OrbitsOfImages(s)!.data[iter!.i];
	return CreateRClass(s, d, OrbitsOfImages(s), rep);
	end,

	ShallowCopy:=iter-> rec(i:=0, s:=iter!.s, reps:=IteratorOfRClassReps(s))
));

SetIsIteratorOfGreensRClasses(iter, true);
SetUnderlyingSemigroupOfIterator(iter, s);
return iter;
end);

# new for 4.0!
#############################################################################
# not a user function!

InstallGlobalFunction(IteratorOfNewRClassRepsData, 
function(s)
local iter, o;

o:=OrbitsOfImages(s);
iter:=IteratorOfRClassRepsData(s);
iter!.i:=Length(o!.data); 
return iter;
end);

# new for 4.0!
#############################################################################
# not a user function!

InstallGlobalFunction(IteratorOfRClassRepsData, 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfRClassRepsData");

iter:=IteratorByFunctions( rec(
	
	ShallowCopy := iter -> rec( i:=0, s:=iter!.s, 
	last_called := NextIterator, last_value := 0, 
	chooser:=iter!.chooser, next:=iter!.next),
	
	i:=0, # representative index i.e. which representative we are at
	
	s:= s,
	
	next_value := fail,
	
	last_called_by_is_done:=false,
	
	######################################################################
	
	IsDoneIterator:=function(iter)
	local o, ht, gens, i, x, d, y, z, one, O, orbits;
	 
	if iter!.last_called_by_is_done then 
		return iter!.next_value=fail;
	fi;
	
	iter!.last_called_by_is_done:=true;
	
	O:=OrbitsOfImages(s);
	
	iter!.next_value:=fail;
	
	if iter!.i < Length(O!.data) then 
	# we already know this rep
		iter!.i:=iter!.i+1;
		iter!.next_value:=O!.data[iter!.i];
		return false;
	elif O!.finished then  
		return true;
	fi;
	
	ht:=O!.ht;
	o:=ht!.o;
	i:=O!.at;
	
	if i=Length(o) then
	#at the end of the orbit!
		O!.finished:=true;
		return true;
	fi;
	
	gens:=O!.gens;
	orbits:=O!.orbits;
	
	while i<Length(o) do 
		O!.at:=O!.at+1;
		i :=i+1;
		x:=o[i];
		
		d:=InOrbitsOfImages(s, x, orbits, []);

		if not d[1] then #new rep!
			if IsTransformationMonoid(s) or not i = 1 then 
				d:=AddToOrbitsOfImages(s, x, O, d[2]);
				d[3]:=RClassSCCFromData(s, d, O)[1];
				iter!.i:=iter!.i+1;
				iter!.next_value:=d;
				return false;
			fi;
		fi;
	od;
	
	O!.finished:=true;
	return true;
	end,

	######################################################################

	NextIterator:=function(iter) 
	
	if not iter!.last_called_by_is_done then 
		IsDoneIterator(iter);
	fi;

	iter!.last_called_by_is_done:=false;
	return iter!.next_value;
	end
	
	######################################################################
));

SetIsIteratorOfRClassRepsData(iter, true);

return iter;
end);

#############################################################################
#

#InstallMethod(IteratorOfRClassReps, "for a transformation semigroup", 
#[IsTransformationSemigroup], 

InstallGlobalFunction(IteratorOfRClassReps,
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfRClassReps");

iter:=IteratorByFunctions( rec(

	s:=s,
	
	data:=IteratorOfRClassRepsData(s),
	
	IsDoneIterator := iter-> IsDoneIterator(iter!.data),
	
	NextIterator := function(iter)
	if not IsDoneIterator(iter!.data) then 
		return RClassRepFromData(iter!.s, NextIterator(iter!.data));
	fi;
	return fail; end,
	
	ShallowCopy := iter -> rec( data:=IteratorOfRClassRepsData(
	iter!.s))
));

SetIsIteratorOfRClassReps(iter, true);
SetUnderlyingSemigroupOfIterator(iter, s);

return iter;
end);

# new for 4.0!
#############################################################################
# <j> is the index of the scc we are computing the multipliers for!

InstallGlobalFunction(MultipliersOfSCCOfImageOrbit,
function(gens, o, j)
local i, p, f, scc, schreier;

#p:=o!.perms;
p:=EmptyPlist(Length(o));
scc:=o!.scc[j];

#for i in scc do
#	f:=EvaluateWord(gens, TraceSchreierTreeOfSCCBack(o, j, i));
#	p[i]:=PermList(MappingPermListList(o[i], OnTuples(o[i], f)));
#od; JDM this works also!

for i in scc do
	f:=EvaluateWord(gens, TraceSchreierTreeOfSCCForward(o, j, i)); 
	p[i]:=MappingPermListList(OnTuples(o[scc[1]], f), o[scc[1]]);
od;

return p;
end);

# new for 4.0!
#############################################################################

InstallOtherMethod(NrGreensHClasses, "for an R-class of a trans. semigroup", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
r-> NrGreensLClasses(GreensDClass(r)));

# new for 4.0!
#############################################################################

InstallMethod(NrGreensRClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)

Info(InfoMonoidGreens, 4, "NrGreensRClasses");

ExpandOrbitsOfImages(s);
return NrRClassesOrbitsOfImages(s);
end);

# new for 4.0!
#############################################################################

InstallOtherMethod(NrIdempotents, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
local out, ker, rep, n, o, i, img, j;

if HasIdempotents(r) then 
	return Length(Idempotents(r));
fi;

if HasIsRegularRClass(r) and not IsRegularRClass(r) then 
	return 0;
fi;

out:= 0;
ker:= KernelOfTransformation(r!.rep);
rep:=r!.rep![1];
n:=Length(Set(rep));
o:=r!.o{RClassSCC(r)}; #JDM1

for i in o do
	img:=EmptyPlist(n);
	j:=1;
	while j<=n and POS_LIST_DEFAULT(img, rep[i[j]], 0)=fail do
		img[Length(img)+1]:=rep[i[j]];
		j:=j+1; 
	od;
	
	if j=n+1 then 
		out:=out+1;
	fi;
od;

return out;
end);

# new for 4.0!
#############################################################################

InstallGlobalFunction(NrRClassesOrbitsOfImages,
function(s)
local i, j, k, l, m, c;

#Info(InfoMonoidGreens, 4, "NrRClassesOrbitsOfImages");

c:=OrbitsOfImages(s);
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

if OrbitsOfImages(s)!.finished then 
	SetNrGreensRClasses(s, m);
fi;
return m;
end);

# new for 4.0!
#############################################################################

InstallMethod(OrbitsOfImages, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(s)
local gens, n, one, ht, i, type;

gens:=Generators(s);
n := DegreeOfTransformationSemigroup( s );
one := TransformationNC( [ 1 .. n ] );

# JDM should ht and ht!.o really be an Orb objects?
ht := HTCreate(one);
HTAdd(ht, one, true);
for i in gens do 
	HTAdd(ht, i, true);
od;
ht!.o := Concatenation([one], gens); 
# JDM 

type:=NewType(FamilyObj(s), IsOrbitsOfImages);

return Objectify(type, rec(
  finished:=false, 
  orbits:=EmptyPlist(n), 
  images:=EmptyPlist(n), 
  at:=0, 
  gens:=gens,
  s:=s,
	deg := n,
	one := one,
	ht:=ht,
	data:=[]
));
end);

# new for 4.0!
############################################################################

InstallMethod(ParentAttr, "for a R-class of a trans. semigroup", 
[IsGreensRClass and IsGreensClassOfTransSemigp], x-> x!.parent);

# new for 4.0!
############################################################################


InstallMethod(PrintObj, [IsOrbitsOfImages], 
function(o)
Print("<orbits of images; at ", o!.at, " of ", Length(o!.ht!.o), "; ", 
SizeOrbitsOfImages(o!.s), " elements; ", NrRClassesOrbitsOfImages(o!.s), 
" R-classes>");
end);


# new for 4.0!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassRepsData], 
function(iter)
local O, s;

s:=iter!.s;
O:=OrbitsOfImages(s);

Print( "<iterator of R-class reps data, ", Length(O!.ht!.o), " candidates, ", 
 SizeOrbitsOfImages(s), " elements, ", NrRClassesOrbitsOfImages(s), 
 " R-classes>");
return;
end);

# new for 4.0!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassReps], 
function(iter)
local O, s;

s:=UnderlyingSemigroupOfIterator(iter);
O:=OrbitsOfImages(s);

Print( "<iterator of R-class reps, ", Length(O!.ht!.o), " candidates, ", 
 SizeOrbitsOfImages(s), " elements, ", NrRClassesOrbitsOfImages(s), 
 " R-classes>");
return;
end);

#############################################################################
# keep here

InstallMethod( PrintObj, "for R-class data",
[ IsGreensRClassData and IsGreensRClassDataRep],
function( obj )
Print( "GreensRClassData( ", obj!.rep,  " )" );
end );

# new for 4.0!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfGreensRClasses], 
function(iter)
Print( "<iterator of R-classes>");
return;
end);

# new for 4.0!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfSemigroup], 
function(iter)
Print("<iterator of transformation semigroup>");
return;
end);

# new for 4.0!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassElements], 
function(iter)
Print("<iterator of R-class>");
return;
end);

###########################################################################
#JDM do not recreate the family and type every time here?

InstallGlobalFunction(RClassData, function(list)
return Objectify(NewType(NewFamily("Green's R-class data", IsGreensRClassData), 
IsGreensRClassData and IsGreensRClassDataRep), list);
end);

# new for 4.0!
############################################################################
#JDM could be an attribute?

InstallMethod(RClassImageOrbit, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
return r!.o!.orbits[r!.data[1]][r!.data[2]];
end);

############################################################################
#

InstallOtherMethod(RClassImageOrbit, "for a D-class of a trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
return d!.o[1]!.orbits[d!.data[1]][d!.data[2]];
end);

############################################################################
#

InstallGlobalFunction(RClassImageOrbitFromData,
function(arg)
local s, d;

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	return arg[3]!.orbits[d[1]][d[2]];
else
	return OrbitsOfImages(s)!.orbits[d[1]][d[2]];
fi;

end);

# new for 4.0!
############################################################################

InstallGlobalFunction(RClassRepFromData,
function(arg)
local s, o, d;

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfImages(s)!.orbits[d[1]][d[2]];
fi;

return o!.reps[d[4]][d[5]][d[6]];
end);

# RClassRep = Representative!

############################################################################
#JDM remove the following later!

InstallGlobalFunction(RClassRepsDataFromOrbits,
function(O, j)
local data, k, m, val, n;

data:=[];

for k in [1..Length(O)] do 
	for m in [1..Length(O[k]!.scc)] do 
		for val in [1..Length(O[k]!.reps[m])] do 
			for n in [1..Length(O[k]!.reps[m][val])] do 
				data[Length(data)+1]:=[j,k,1,m, val,n];
			od;
		od;
	od;
od;

return data;

end);


# new for 4.0!
############################################################################
# take care using the following, it returns all of the perms for the weak orbit

InstallGlobalFunction(RClassPermsFromData, 
function(arg)
local s, o, d;

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfImages(s)!.orbits[d[1]][d[2]];
fi;

return o!.perms;
end);

############################################################################

InstallMethod(RClassPerms, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
local d;
d:=r!.data;

return r!.o!.orbits[d[1]][d[2]]!.perms;
end);

############################################################################

InstallOtherMethod(RClassPerms, "for a D-class of a trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(r)
local d;
d:=r!.data[1];

return r!.o[1]!.orbits[d[1]][d[2]]!.perms;
end);

############################################################################

InstallGlobalFunction(RClassRepsData, 
s-> OrbitsOfImages(s)!.data);

# new for 4.0!
############################################################################

InstallMethod(RClassSCC, "for an R-class of a trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
local d;

d:=r!.data;
return r!.o!.orbits[d[1]][d[2]]!.scc[d[4]];
end);

# new for 4.0!
############################################################################

InstallOtherMethod(RClassSCC, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(r)
local d;

d:=r!.data[1];
return r!.o[1]!.orbits[d[1]][d[2]]!.scc[d[4]];
end);


# new for 4.0!
############################################################################
#JDM maybe delete?

InstallGlobalFunction(RClassSCCFromData,
function(arg)
local s, o, d;
s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfImages(s)!.orbits[d[1]][d[2]];
fi;

return o!.scc[d[4]];
end);

# new for 4.0!
############################################################################

#InstallGlobalFunction(RClassSchutzGp, 
#function(r)
#local d;

#d:=r!.data;
#return r!.o[d[1]][d[2]]!.schutz[d[4]][2];
#end);

# new for 4.0!
############################################################################

InstallGlobalFunction(RClassSchutzGpFromData, 
function(arg)
local s, o, d;

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfImages(s)!.orbits[d[1]][d[2]];
fi;

return o!.schutz[d[4]][2];
end);

# new for 4.0!
############################################################################

InstallMethod(RClassStabChain, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
local d;

d:=r!.data;
return r!.o!.orbits[d[1]][d[2]]!.schutz[d[4]][1];
end);

# new for 4.0!
############################################################################

InstallOtherMethod(RClassStabChain, "for a D-class of a trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(r)
local d;

d:=r!.data[1];
return r!.o[1]!.orbits[d[1]][d[2]]!.schutz[d[4]][1];
end);

# new for 4.0!
############################################################################

InstallGlobalFunction(RClassStabChainFromData, 
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfImages(s)!.orbits[d[1]][d[2]];
fi;

return o!.schutz[d[4]][1];
end);

############################################################################

InstallMethod(RClassType, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s);

return NewType( FamilyObj( s ), IsEquivalenceClass and 
	 IsEquivalenceClassDefaultRep and IsGreensRClass and 
	 IsGreensClassOfTransSemigp);
end);

# new for 4.0!
############################################################################

InstallOtherMethod(SchutzenbergerGroup, "for a R-class of a trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
local d;
d:=r!.data;
return r!.o!.orbits[d[1]][d[2]]!.schutz[d[4]][2];
end);

#return SchutzenbergerGroup(r);#^(o!.perms[d[3]]^-1);
#
# replace the above with function calls? No!
#
# an R-class rep always has its image in the first pos. of the scc containing
# it hence there is no need to conjugate by the perm above!? 

# new for 4.0!
#############################################################################
# gens are the generators of the semigroup
# o is orbit
# f is a representative of scc with index k
# k is the index of scc containing index of image of f

InstallGlobalFunction(SchutzGpOfImageOrbit,
function(gens, o, f, k) 
local p, t, g, bound, graph, i, j, scc, is_sym;

scc:=o!.scc[k];

if Length(o[scc[1]])<1000 then 
	bound:=Factorial(Length(o[scc[1]]));
else
	bound:=infinity;
fi;

g:=Group(());
p:=o!.perms;
t:=o!.truth;
graph:=OrbitGraph(o);
is_sym:=false;

for i in scc do 
	for j in [1..Length(gens)] do 
		if IsBound(graph[i][j]) and t[k][graph[i][j]] then
  		g:=ClosureGroup(g, PermLeftQuoTransformationNC(f, f/p[i] *
  		 (gens[j]*p[graph[i][j]])));
		fi; #keep track of schreier gens here!
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

# new for 4.0!
#############################################################################
##  Algorithm C. 

InstallOtherMethod(Size, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)

Info(InfoMonoidGreens, 4, "Size: for an R-class");

return Size(SchutzenbergerGroup(r))*Length(RClassSCC(r));
end);

#JDM really require a RClassSizeFromData command that is used here?

#############################################################################
##  Algorithm V.

InstallMethod(Size, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local iter, o, i;

Info(InfoMonoidGreens, 4, "Size: for a trans. semigroup");

ExpandOrbitsOfImages(s);
return SizeOrbitsOfImages(s);
end);

# new for 4.0!
#############################################################################
# returns the size of the semigroup so far

InstallGlobalFunction(SizeOrbitsOfImages, 
function(s)
local i, o, j, c;
i:=0;

c:=OrbitsOfImages(s)!.orbits;

for o in Concatenation(Compacted(c)) do 
  for j in [1..Length(o!.scc)] do
    if IsBound(o!.schutz[j]) and IsBound(o!.reps[j]) and IsBound(o!.scc[j]) then 
      i:=i+Size(o!.schutz[j][2])*
      Sum(List(o!.reps[j]), Length)*Length(o!.scc[j]);
    fi;
  od;
od;

if OrbitsOfImages(s)!.finished then 
	SetSize(s, i);
fi;

return i;
end);

# new for 4.0!
#############################################################################
# returns a word in generators that takes o!.scc[i][1] to o[j] assuming that
# j in scc[i]

InstallGlobalFunction(TraceSchreierTreeOfSCCForward, 
function( o, i, j)
local word;

word := [];
while j > o!.scc[i][1] do
	Add(word, o!.trees[i][1][j]);
	j := o!.trees[i][2][j];
od;
return Reversed(word);
end );

# new for 4.0!
#############################################################################

InstallGlobalFunction(TraceSchreierTreeOfSCCBack,
function( o, i, j)
local word;

word := [];
while j > o!.scc[i][1] do
	Add(word, o!.reverse[i][1][j]);
	j := o!.reverse[i][2][j];
od;
return word;
end);

#############################################################################
# keep here

InstallMethod( ViewObj, "for Green's R-class data",
[IsGreensRClassData and IsGreensRClassDataRep],
function( obj )
Print( "GreensRClassData( ", obj!.rep, ", ", obj!.strongorb,", ", obj!.perms,
", ", obj!.schutz, " )" );
end );

#############################################################################
# 

InstallMethod( ViewObj, "for a monoid pkg img ker orbit",
[IsMonoidPkgImgKerOrbit],
function( o )

Print("<");
if IsClosed(o) then 
	Print("closed "); 
else 
	Print("open "); 
fi;

if IsPosInt(o[1][1]) then 
	Print("image "); 
else 
	Print("kernel ");
fi;

Print("orbit ", Length(o!.orbit), " points"); 
if IsBound(o!.reps) and IsPosInt(o[1][1]) then 
	Print(", ");
	Print(Length(o!.scc), " scc, ");
	Print(Sum(List(o!.reps, Length)), " kernels, ");
	Print(Sum(Concatenation(List(o!.reps, x-> List(x, Length)))), " reps>");
elif IsBound(o!.reps) then 
	Print(", ");
	Print(Length(o!.scc), " scc, ");
	Print(Sum(List(o!.reps, Length)), " images, ");
	Print(Sum(Concatenation(List(o!.reps, x-> List(x, Length)))), " reps>");
else
	Print(">");
fi;

end );


#############################################################################
# DELETE!

CheckSchreierWords:=function(o)
local scc, words, reps, i, j, k, f, gens;
gens:=o!.gens;
scc:=o!.scc;
words:=o!.words;
reps:=o!.reps;

for i in [1..Length(scc)] do 
  for j in [1..Length(words[i])] do #words related to scc[i]
    for k in [1..Length(words[i][j])] do 
    	if not EvaluateWord(gens, words[i][j][k])=reps[i][j][k] then 
				return [i,j,k];
			fi;
    od;
  od;
od;

return true;
end;

#############################################################################

ConvertToOldStyle:=function(c)
local out, i, j, k;

out:=[[],[],[],[]];

for i in c do 
  for j in i do 
    for k in [1..Length(j!.scc)] do 
      if IsBound(j!.reps[k][1]) then
				Add(out[1], j{j!.scc[k]});
				Add(out[2], j!.perms{j!.scc[k]});
				Add(out[3], j!.schutz[k][2]);
				Add(out[4], j!.scc[k]);
			fi;
    od;
  od;
od;

return out;
end;
