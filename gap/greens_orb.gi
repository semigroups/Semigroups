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

#############################################################################

NewSize:=function(c)
local i, o, j;
i:=0;

for o in Concatenation(Compacted(c)) do 
  for j in [1..Length(o!.scc)] do
    if IsBound(o!.schutz[j]) and IsBound(o!.reps[j]) and IsBound(o!.scc[j]) then 
      i:=i+Size(o!.schutz[j][2])*Sum(List(o!.reps[j]), Length)*Length(o!.scc[j]);
    fi;
  od;
od;

return i;
end;

#############################################################################

NrNewRClasses:=function(c)
local i, j, k, l, m;
m:=[];

for i in c do
  for j in i do 
    for k in j!.reps do 
      for l in k do 
        Add(m, Length(l));
      od;
    od;
  od;
od;

return Sum(m);
end;

#############################################################################
# the following should move to orbits_orb.gi..

HashTableForKernels:=function(ker)
local hf, ht;
hf:=function ( l, hashlen )
local  v, i;
v := 0;
for i  in [ 1 .. Length( l ) ]  do
	v := (v * 101 + ORB_HashFunctionForPlainFlatList( l[i], hashlen )) 
	 mod hashlen;
od;
return v + 1;
end;

ht := HTCreate(ker, rec( hf := hf, hfd := 100003, treehashsize := 100003 ));
HTAdd(ht, ker, 1);

return ht;
end;

#############################################################################
# first component of OrbitsOfImages should be false when it is not complete
# and true when it is!

InstallMethod(OrbitsOfImages, "for a trans. semigroup",
[IsTransformationSemigroup], x-> 
# Concatenation([false], EmptyPlist(DegreeOfTransformationSemigroup(x))));
EmptyPlist(DegreeOfTransformationSemigroup(x)));

#############################################################################
# o is the orbit
# i is the index of the scc of o we are trying to create the Schreier tree for!

InstallGlobalFunction(CreateSchreierTreeOfSCC,
function(o, i)
local gen, pos, seen, oo, graph, j, k, l, scc, t;

#if IsBound(o!.trees[i]) then 
#  Error("Schreier tree already created for this scc");
#fi;

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

#############################################################################
#

CreateReverseSchreierTreeOfSCC:=function(o, i)
local rev, j, k, l, m, graph, scc, gen, pos, seen, t, oo; 

graph:=OrbitGraph(o);
rev:=List([1..Length(graph)], x-> List([1..Length(o!.gens)], x-> []));

for j in [1..Length(graph)] do
  for k in [1..Length(graph[j])] do 
    if IsBound(graph[j][k]) then 
      Add(rev[graph[j][k]][k], j);
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
end;


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

TraceSchreierTreeOfSCCBack:=
function( o, i, j)
local word;
word := [];
while j > o!.scc[i][1] do
	Add(word, o!.reverse[i][1][j]);
	j := o!.reverse[i][2][j];
od;
return word;
end;


#############################################################################
# <j> is the index of the scc we are computing the multipliers for!

InstallGlobalFunction(MultipliersOfSCCOfOrbit,
#function(arg)
function(gens, o, j)
local i, p, f, scc, schreier;

#o:=arg[1]; j:=arg[2]; 
#gens:=o!.gens;

#if Length(arg)=3 then 
#  schreier:=arg[3].schreier;
#else
#  schreier:=false;
#fi;

p:=o!.perms;
scc:=o!.scc[j];

#if schreier then 
#	for i in scc do
#		f:=EvaluateWord(gens, TraceSchreierTreeOfSCCBack(o, j, i));
#		p[i]:=PermList(MappingPermListList(o[i], OnTuples(o[i], f)));
#	od;
#else
	for i in scc do
		f:=EvaluateWord(gens, TraceSchreierTreeOfSCCForward(o, j, i)); 
		p[i]:=MappingPermListList(OnTuples(o[scc[1]], f), o[scc[1]]);
	od;
#fi;

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
local p, t, g, bound, graph, i, j, scc, is_sym;

#gens:=o!.gens;
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

#############################################################################

InstallGlobalFunction(ForwardOrbitOfImageNC, 
#function(arg)
function(s, f)
local img, o, scc, t, i, gens, g, bound, graph, j, schutz, schreier;
Info(InfoWarning, 2, "Warning: calling this function more than once with the ",
" same arguments will repeatedly add the returned value to OrbitsOfImages. ",
"Use ForwardOrbitOfImage instead.");

#s:=arg[1]; f:=arg[2];

#if Length(arg)=3 then 
#  schreier:=arg[3].schreier;
#else 
#  schreier:=false;
#fi;

if IsMonoid(s) then 
	gens:=GeneratorsOfMonoid(s);
else
	gens:=GeneratorsOfSemigroup(s);
fi;

#img:=Set(f![1]);
img:=ImageSetOfTransformation(f);
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

#representatives of R-classes with image belonging in scc[i] partitioned 
#according to their kernels
o!.reps:=List([1..Length(scc)], x-> []);
Add(o!.reps[1], [f]);

#if schreier then 
#	o!.words:=List([1..Length(scc)], x-> []);
#	o!.reverse:=EmptyPlist(Length(scc));
#	o!.reverse[1]:=CreateReverseSchreierTreeOfSCC(o,1);
#fi;

#kernels of representatives of R-classes with image belonging in scc[i]
o!.kernels_ht:=[];
Add(o!.kernels_ht, HashTableForKernels(KernelOfTransformation(f)));

#calculate the multipliers and schutzenberger groups for the scc containing
#img. 
scc:=scc[1];

#multipliers of scc containing the image of f
o!.perms:=EmptyPlist(Length(o));
#o!.perms:=MultipliersOfSCCOfOrbit(o, 1, rec(schreier:=schreier));
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

#img:=Set(f![1]);
img:=ImageSetOfTransformation(f);

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
#IteratorOfGreensRClasses:=

# if partial info is known... IteratorOfGreensRClasses
# if full info is known...    EnumeratorOfGreensRClasses
# if full info is known...    GreensRClasses

# actually turn this into an iterator!! add some more info statements?
InstallGlobalFunction(IteratorOfRClassReps, 
function(arg)
local s, gens, iter, n, one;
s:=arg[1];

if IsTransformationMonoid( s ) then
	gens := GeneratorsOfMonoid( s );
else
	gens := GeneratorsOfSemigroup( s );
fi;

n := DegreeOfTransformationSemigroup( s );
one := TransformationNC( [ 1 .. n ] );

iter:=IteratorByFunctions( rec(
			
			IsDoneIterator := iter-> iter!.next(iter, false)=fail,
			
			NextIterator := iter-> iter!.next(iter, true),
			
			ShallowCopy := ReturnFail,
			
			gens:=gens,
			
			i := 0,
			
			s:=s,
			
			n := n,
			
			one := one,
			
			ht:=HTCreate(one),
			
			o:=[one],

############################################################################

			next:=function(iter, advance) 
			local O, o, i, j, img, k, l, m, x, ht, y, val, reps, schutz, new, kernels_ht, 
			z, schreier, s, report, last_report, one;
			
			if iter!.i=Length(iter!.o) then 
				return fail;
			fi;
			
			if advance then
			  one:=iter!.one;
				o:=iter!.o;
				gens:=iter!.gens;
				ht:=iter!.ht;
				s:=iter!.s;
				
				if iter!.i=0 then 
			    HTAdd(ht, one, true);
				fi;
				
				iter!.i:=iter!.i+1;
				i:=iter!.i;
				O:=OrbitsOfImages(s);
				
				img:=ImageSetOfTransformation(o[i]);
				j:=Length(img);
				new:=false;
				
				#check if img has been seen before
				if IsBound(O[j]) then
					k:=0;
					repeat
						k:=k+1;
						l:=Position(O[j][k], img);
					until not l=fail or k=Length(O[j]);
					
					if l=fail then k:=fail; fi;
				else 
					k:=fail;
				fi;
			
				if k = fail then #img has not been seen before
					new:=true; x:=o[i]; 
					
					if IsTransformationMonoid( s ) or not o[i] = one then
						ForwardOrbitOfImageNC(s, o[i]);
						#scheier words here!
					fi;
					
				else #img has been seen before
					if IsTransformationMonoid( s ) or not o[i] = one then
						
						m:=PositionProperty(O[j][k]!.truth, x-> x[l]); #the scc containing img
						reps:=O[j][k]!.reps[m];
						
						#scheier words here!
						
						#calculate multipliers and schutz!
						if not IsBound(O[j][k]!.perms[l]) then #we never considered this scc before!
							O[j][k]!.trees[m]:=CreateSchreierTreeOfSCC(O[j][k], m);
							
							#schreier words here
							
							O[j][k]!.perms:=MultipliersOfSCCOfOrbit(gens, O[j][k], m);
							x:= o[i] * O[j][k]!.perms[l];
							O[j][k]!.schutz[m]:=SchutzenbergerGroupOfSCCOfOrbit(gens, O[j][k], x, m);;
							O[j][k]!.kernels_ht[m]:=HashTableForKernels(KernelOfTransformation( x ));
							reps[Length(reps)+1]:=[x]; 
							new:=true; 
			
						else
							kernels_ht:=O[j][k]!.kernels_ht[m];
			
							x := o[i] * O[j][k]!.perms[l];
							val:=HTValue(kernels_ht, KernelOfTransformation(x));
							
							if not val=fail then #kernel seen before
								schutz:=O[j][k]!.schutz[m][1];
								if not ForAny(reps[val], y-> schutz=true or 
								 SiftedPermutation(schutz, PermLeftQuoTransformationNC(y, x))=()) then 
									reps[val][Length(reps[val])+1]:=x; 
									new:=true;
									#schreier words here
								fi;
							else #new kernel
								reps[Length(reps)+1]:=[x]; 
								#schreier words here
								HTAdd(kernels_ht, KernelOfTransformation( x ), 
								 Length(reps));
								new:=true; 
							fi;
						fi;
					fi;
				fi;
			
				if new then #install new pts in the orbit
					for y in [1..Length(gens)] do
						z:=gens[y]*x; 
						if HTValue(ht, z)=fail then  
							HTAdd(ht, z, true);
							o[Length(o)+1]:=z;
							#schreier words here
						fi;
					od;
				fi;
			
				return x;
			fi;
			return true;
			end));

SetIsIteratorOfRClassReps(iter, true);

return iter;
end);

############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassReps], 
function(iter)

Print( "<iterator of R-class representatives, ", Length(iter!.o), " candidates, ", 
 NewSize(OrbitsOfImages(iter!.s)), " elements, ", NrNewRClasses(OrbitsOfImages(iter!.s)), " R-classes>");
return;
end);

############################################################################

InstallGlobalFunction(GreensRClassRepsNC,
function(arg)
#function(s)
local n, one, gens, O, o, i, j, img, k, l, m, x, ht, y, val, reps, 
 schutz, new, kernels_ht, z, schreier, o_words, x_word, words, s, report, last_report;

s:=arg[1];

if Length(arg)=2 then
  if "schreier" in RecNames(arg[2]) then 
	  schreier:=arg[2].schreier;
	elif "report" in RecNames(arg[2]) then 
	  report:=arg[2].report; last_report:=0;
	fi;
else 
	schreier:=false; report:=false;
fi;

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
#o_words:=[fail];
#x_word:=[];

i:=0;

while i<Length(o) do 
  i:=i+1;
  if not report=false and i>last_report+report then 
    Info(InfoWarning, 1, Length(o), " candidates, ", NewSize(O), " elements, ",
    NrNewRClasses(O), " R-classes");
    last_report:=i;
  fi;
  
  #img := Set(o[i]![1]);
  img:=ImageSetOfTransformation(o[i]);
  j:=Length(img);
  new:=false;

  #check if img has been seen before
  if IsBound(O[j]) then
    k:=0;
    repeat
      k:=k+1;
      l:=Position(O[j][k], img);
    until not l=fail or k=Length(O[j]);
    
    if l=fail then k:=fail; fi;
  else 
    k:=fail;
  fi;

	if k = fail then #img has not been seen before
		new:=true; x:=o[i]; 
		
		if IsTransformationMonoid( s ) or not o[i] = one then
			ForwardOrbitOfImageNC(s, o[i]);
			#ForwardOrbitOfImageNC(s, o[i], rec(schreier:=schreier));
			#if schreier then 
			#  x_word:=o_words[i];
			#  Add(O[j][Length(O[j])]!.words[1], [x_word]);
			#fi;
		fi;
		
	else #img has been seen before
		if IsTransformationMonoid( s ) or not o[i] = one then
			
			m:=PositionProperty(O[j][k]!.truth, x-> x[l]); #the scc containing img
			reps:=O[j][k]!.reps[m];
			
			#if schreier then 
			#  words:=O[j][k]!.words[m];
			#fi;
			
			#calculate multipliers and schutz!
			if not IsBound(O[j][k]!.perms[l]) then #we never considered this scc before!
			  O[j][k]!.trees[m]:=CreateSchreierTreeOfSCC(O[j][k], m);
			  #if schreier then 
				#  O[j][k]!.reverse[m]:=CreateReverseSchreierTreeOfSCC(O[j][k], m);
				#  x_word:=Concatenation(o_words[i], TraceSchreierTreeOfSCCBack(O[j][k], m, l));
				#  words[Length(reps)+1]:=[x_word];
				#fi;
			  #O[j][k]!.perms:=MultipliersOfSCCOfOrbit(O[j][k], m, rec(schreier:=schreier));
			  O[j][k]!.perms:=MultipliersOfSCCOfOrbit(gens, O[j][k], m);
			  x:= o[i] * O[j][k]!.perms[l];
			  O[j][k]!.schutz[m]:=SchutzenbergerGroupOfSCCOfOrbit(gens, O[j][k], x, m);;
			  O[j][k]!.kernels_ht[m]:=HashTableForKernels(KernelOfTransformation( x ));
				reps[Length(reps)+1]:=[x]; 
				new:=true; 

			else
				kernels_ht:=O[j][k]!.kernels_ht[m];

				x := o[i] * O[j][k]!.perms[l];
				val:=HTValue(kernels_ht, KernelOfTransformation(x));
				
				if not val=fail then #kernel seen before
					schutz:=O[j][k]!.schutz[m][1];
					if not ForAny(reps[val], y-> schutz=true or 
					 SiftedPermutation(schutz, PermLeftQuoTransformationNC(y, x))=()) then 
						reps[val][Length(reps[val])+1]:=x; 
						new:=true;
						#if schreier then 
						#  x_word:=Concatenation(o_words[i], 
				  	#	 TraceSchreierTreeOfSCCBack(O[j][k], m, l));
				  	#	words[val][Length(reps[val])]:=x_word;
						#fi;
						
					fi;
				else #new kernel
					reps[Length(reps)+1]:=[x]; 
					#if schreier then 
					#  x_word:=Concatenation(o_words[i], 
					#   TraceSchreierTreeOfSCCBack(O[j][k], m, l));
					#  words[Length(reps)]:=[x_word];
					#fi;
					HTAdd(kernels_ht, KernelOfTransformation( x ), 
					 Length(reps));
					new:=true; 
				fi;
			fi;
		fi;
	fi;

	if new then #install new pts in the orbit
		for y in [1..Length(gens)] do
		  z:=gens[y]*x; 
		  if HTValue(ht, z)=fail then  
		    HTAdd(ht, z, true);
		    o[Length(o)+1]:=z;
		    #if schreier then
		    #  if not x_word=fail then  
		    #   o_words[Length(o)]:=Concatenation([y], x_word);
		    #  else 
		    #    o_words[Length(o)]:=[y];
		    #  fi;
		    #fi;
		  fi;
		od;
	fi;
od;

return [i, O];
end);


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
    for k in [1..Length(words[i][j])] do #words of reps of scc[i] with a given kernel
			#f:=EvaluateWord(gens, words[i][j][k]);
			#if not KernelOfTransformationNC(f)=KernelOfTransformationNC(reps[i][j][k]) 
			# or not Set(f![1])=Set(reps[i][j][k]![1]) then 
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

#############################################################################

NewSize:=function(c)
local i, o, j;
i:=0;

for o in Concatenation(Compacted(c)) do 
  for j in [1..Length(o!.scc)] do
    if IsBound(o!.schutz[j]) and IsBound(o!.reps[j]) and IsBound(o!.scc[j]) then 
      i:=i+Size(o!.schutz[j][2])*Sum(List(o!.reps[j]), Length)*Length(o!.scc[j]);
    fi;
  od;
od;

return i;
end;

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

schutz:=o!.schutz[i][1];
reps:=o!.reps[i][val];

return ForAny(reps, x-> schutz=true or PermLeftQuoTransformationNC(x, f)
     in schutz);
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
  j:=PositionProperty(o!.truth, x-> x[i]);
  return IsInSCCOfOrbitNC(f*o!.perms[i], o, j);
fi;

return false;
end);