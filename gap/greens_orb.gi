#############################################################################
##
#W  greens_orb.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id: greens.gd 54 2010-07-06 11:09:37Z jamesm $

#############################################################################
##


# it can be that when you are doing iterator of orbits of images that 
# you create an orbit that is later contained in another orbit
# this will lead to the later orbit not have all of its reps, etc defined. 

# JDM what should be done about the above comment?

HashTableForKernels:=function(ker)
local hf, ht;
hf:=function ( l, hashlen )
    local  v, i;
    v := 0;
    for i  in [ 1 .. Length( l ) ]  do
        v := (v * 101 + ORB_HashFunctionForPlainFlatList( l[i], hashlen )) mod hashlen;
    od;
    return v + 1;
end;

ht := HTCreate(ker, rec( hf := hf, hfd := 1001, treehashsize := 1001 ));
HTAdd(ht, ker, 1);

return ht;
end;


#############################################################################

InstallMethod(OrbitsOfImages, "for a trans. semigroup",
[IsTransformationSemigroup], x-> 
 EmptyPlist(DegreeOfTransformationSemigroup(x)));

#############################################################################
# o is the orbit
# i is the index of the scc of o we are trying to create the Schreier tree for!

InstallGlobalFunction(CreateSchreierTreeOfSCC,
function(o, i)
local gen, pos, seen, oo, graph, j, k, l, scc, t;

if IsBound(o!.trees[i]) then 
  Error("Schreier tree already created for this scc");
fi;

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

while Length(oo)<Length(o!.scc[i]) do 
  j:=j+1;
	k:=oo[j];
	l:=0;
	while l<Length(graph[k]) do  
		l:=l+1;
		if IsBound(graph[k][l]) and not seen[graph[k][l]] and t[graph[k][l]] then 
			Add(oo, graph[k][l]); seen[graph[k][l]]:=true; 
			gen[graph[k][l]]:=l; pos[graph[k][l]]:=k;
		fi;
	od;
od;

return [gen, pos];
end);

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


#############################################################################
# <j> is the index of the scc we are computing the multipliers for!

InstallGlobalFunction(MultipliersOfSCCOfOrbit,
function(gens, o, j)
local i, p, f, scc, q;

p:=o!.perms;
scc:=o!.scc[j];

for i in scc do
  f:=EvaluateWord(gens, TraceSchreierTreeOfSCCForward(o, j, i)); 
  p[i]:=PermList(MappingPermListListNC_C(OnTuples(o[scc[1]], f), o[scc[1]]));
od;

return p;
end);

#############################################################################
#

#gens are the generators of the semigroup
#o is orbit
#f is a representative of scc with index k
#k is the index of scc containing index of image of f

InstallGlobalFunction(SchutzenbergerGroupOfSCCOfOrbit,
function(gens, o, f, k) 
local p, t, g, bound, graph, i, j, scc;

scc:=o!.scc[k];

if Length(o[scc[1]])<1000 then 
	bound:=Factorial(Length(o[scc[1]]));
else
	bound:=infinity;
fi;

g:=Group(());
#g:=Orb([()], (), OnRight, rec(log:=true));
#Enumerate(g);
p:=o!.perms;
t:=o!.truth;
graph:=OrbitGraph(o);

for i in scc do 
	for j in [1..Length(gens)] do 
		if IsBound(graph[i][j]) and t[k][graph[i][j]] then
			g:=ClosureGroup(g, PermList(PermLeftQuoTransformationNC_C(f, f/p[i] *
			 (gens[j]*p[graph[i][j]]))));
			#new:=PermList(PermLeftQuoTransformationNC_C(f, f/p[i] *
			# (gens[j]*p[graph[i][j]])));
			#if not new in g then 
			#  AddGeneratorsToOrbit(g, [new]);
			#fi;
		fi;
		if Size(g)>=bound then 
			break;
		fi;
	od;
	if Size(g)>=bound then 
		break;
	fi;
od;

#ht:=HTCreate(());
#for i in g do 
#  HTAdd(ht, i, true);
#od;

#return ht;
return g;
end);

#############################################################################

InstallGlobalFunction(ForwardOrbitOfImageNC, 
function(s, f)
local img, o, scc, t, i, gens, g, bound, graph, j, schutz;

Info(InfoWarning, 2, "Warning: calling this function more than once with the ",
" same arguments will repeatedly add the returned value to OrbitsOfImages. ",
"Use ForwardOrbitOfImage instead.");

if IsMonoid(s) then 
	gens:=GeneratorsOfMonoid(s);
else
	gens:=GeneratorsOfSemigroup(s);
fi;

img:=Set(f![1]);
o:=Orb(s, img, OnSets, rec(
        treehashsize:=NextPrimeInt(Minimum(100000, 
         3*Binomial(DegreeOfTransformationSemigroup(s), Length(img)))), 
        schreier:=true,
        gradingfunc := function(o,x) return Length(x); end, 
        orbitgraph := true, 
        onlygrades:=[Length(img)], 
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
o!.trees[1]:=CreateSchreierTreeOfSCC(o,1);

#representatives of R-classes with image belonging in scc[i]
#o!.reps:=List([1..Length(scc)], x-> HTCreate(fail));
#HTAdd(o!.reps[1], f, true);
o!.reps:=List([1..Length(scc)], x-> []);
Add(o!.reps[1], f);

#kernels of representatives of R-classes with image belonging in scc[i]
#o!.kernels:=List([1..Length(scc)], x-> []); #probably this is not necessary!
#Add(o!.kernels[1], KernelOfTransformationNC(f)); 

o!.kernels_ht:=[];
Add(o!.kernels_ht, HashTableForKernels(KernelOfTransformationNC(f)));

#calculate the multipliers and schutzenberger groups for the scc containing
#img. 
scc:=scc[1];

#multipliers of scc containing the image of f
o!.perms:=EmptyPlist(Length(o));
o!.perms:=MultipliersOfSCCOfOrbit(gens, o, 1);

#schutzenberger group corresponding to scc[1]
o!.schutz:=EmptyPlist(Length(scc));
o!.schutz[1]:=SchutzenbergerGroupOfSCCOfOrbit(gens, o, f, 1);

#OrbitsOfImages is partitioned according to image size of the first element!
if IsBound(OrbitsOfImages(s)[Length(img)]) then 
  Add(OrbitsOfImages(s)[Length(img)], o);
else
  OrbitsOfImages(s)[Length(img)]:=[o];
fi;

#SetIsOrbitOfImage(o, true); #JDM used for anything?

return o;
end);

#############################################################################

InstallGlobalFunction(ForwardOrbitOfImage, 
function(s, f)
local img, i;

img:=Set(f![1]);

if IsBound(OrbitsOfImages(s)[Length(img)]) then 
  i:=Position(OrbitsOfImages(s)[Length(img)], x-> img in x);
else
  i:=fail;
fi;

if not i=fail then 
  return OrbitsOfImages(s)[i];
fi;

return ForwardOrbitOfImageNC(s, f);
end);

#############################################################################
# assumes that the image of f is the first element in the strong orbit 
# containing it, and it assumes that the image of f belongs to o!.scc[i]. 

InstallGlobalFunction(IsInSCCOfOrbitNC,
function(f, o, i)
local val, schutz, reps;

val:=HTValue(o!.kernels_ht[i], KernelOfTransformationNC(f));

if val=fail then 
  return false;
fi;

schutz:=o!.schutz[i];
reps:=o!.reps[i];

return PermList(PermLeftQuoTransformationNC_C(reps[val], f))
     in schutz;
end);

#############################################################################

InstallGlobalFunction(IsInSCCOfOrbit,
function(f, o, i)
local j;

j:=Position(o, Set(f![1]));

if not j=fail and j in o!.scc[i] then 
  return IsInSCCOfOrbitNC(f*o!.perms[j], o, i);
fi;

return false;
end);

#############################################################################

InstallGlobalFunction(IsInOrbit,
function(f, o)
local i, j;
i:=Position(o, Set(f![1]));

if not i=fail then 
  j:=PositionProperty(o!.scc, x-> i in x);
  return IsInSCCOfOrbitNC(f*o!.perms[i], o, j);
fi;

return false;
end);

#############################################################################
#IteratorOfGreensRClasses:=

# actually turn this into an iterator!!

InstallGlobalFunction(IteratorOfRClassReps,
function(s)
local n, one, gens, O, o, i, j, img, k, l, m, x, ht, y;

n := DegreeOfTransformationSemigroup( s );
one := TransformationNC( [ 1 .. n ] );

if IsTransformationMonoid( s ) then
	gens := GeneratorsOfMonoid( s );
else
	gens := GeneratorsOfSemigroup( s );
fi;

O:=OrbitsOfImages(s);

ht:=HTCreate(one);
HTAdd(ht, one, true);
o:=[one];

i:=0;

while i<Length(o) do 
  i:=i+1;
  img := Set(o[i]![1]);
  
  j:=Length(img);
  if IsBound(O[j]) then
    k:=0;
    
    repeat
      k:=k+1;
      l:=Position(O[j][k], img);
    until not l=fail or k=Length(O[j]);
    
    if l=fail then k:=fail; fi;
    #k:=PositionProperty(O[j], x-> img in x); #(1)
     
  else 
    k:=fail;
  fi;

	if k = fail then
		if IsTransformationMonoid( s ) or not o[i] = one then
			ForwardOrbitOfImageNC(s, o[i]);
			# this calculates multipliers and schutz of its first scc!
		fi;
		
		for y in gens do
		  y:=y*o[i]; 
		  if HTValue(ht, y)=fail then  
		    HTAdd(ht, y, true);
		    Add(o, y);
		  fi;
		od;
		
	else
		if IsTransformationMonoid( s ) or not o[i] = one then
			
			m:=PositionProperty(O[j][k]!.scc, x-> l in x);
			
			#now calculate multipliers and schutz!
			if not IsBound(O[j][k]!.perms[l]) then #we never consider this scc before!
			  O[j][k]!.trees[m]:=CreateSchreierTreeOfSCC(O[j][k], m);
			  O[j][k]!.perms:=MultipliersOfSCCOfOrbit(gens, O[j][k], m);
			  O[j][k]!.schutz[m]:=SchutzenbergerGroupOfSCCOfOrbit(gens, O[j][k], 
			   o[i] * O[j][k]!.perms[l], m);;
			  
			fi;
		
			x := o[i] * O[j][k]!.perms[l];
			if O[j][k]!.reps[m]=[] or not IsInSCCOfOrbitNC(x, O[j][k], m) then
				Add(O[j][k]!.reps[m], x);
				if IsBound(O[j][k]!.kernels_ht[m]) then 
			    HTAdd(O[j][k]!.kernels_ht[m], KernelOfTransformationNC( x ), Length(O[j][k]!.reps[m]));
			  else
			    O[j][k]!.kernels_ht[m]:=HashTableForKernels(KernelOfTransformationNC( x ));
			  fi;
			  for y in gens do
		      y:=y*x; #keep track of what generators are applied here!
		      if HTValue(ht, y)=fail then  
		        HTAdd(ht, y, true);
		        Add(o, y);
		      fi;
		    od;
			fi;
		fi;
	fi;
	
od;

return [i, O];
end);

#############################################################################
# DELETE!

NrNewRClasses:=function(c)
local i, j, k, l;
l:=[];

for i in c do
  for j in i do 
    for k in j!.reps do 
      Add(l, Length(k));
    od;
  od;
od;

return l;
end;

ConvertToOldStyle:=function(c)
local out, i, j, k;

out:=[[],[],[],[]];

for i in c do 
  for j in i do 
    for k in [1..Length(j!.scc)] do 
      if IsBound(j!.reps[k][1]) then
				Add(out[1], j{j!.scc[k]});
				Add(out[2], j!.perms{j!.scc[k]});
				Add(out[3], j!.schutz[k]);
				Add(out[4], j!.scc[k]);
			fi;
    od;
  od;
od;

return out;
end;

NewSize:=function(c)
local i, o, j;
i:=0;

for o in Concatenation(c) do 
  for j in [1..Length(o!.scc)] do
    if IsBound(o!.schutz[j]) and IsBound(o!.reps[j]) and IsBound(o!.scc[j]) then 
      i:=i+Size(o!.schutz[j])*Length(o!.reps[j])*Length(o!.scc[j]);
      #i:=i+o!.schutz[j]!.nr*Length(o!.reps[j])*Length(o!.scc[j]);
    fi;
  od;
od;

return i;
end;