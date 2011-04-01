#############################################################################
##
#W  orbits.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

# - this file is alphabetized, keep it that way!

# - this file should contains functions relating to orbit calculations!

#############################################################################
# Notes

# new method for 4.0!
#############################################################################

InstallMethod(ChooseHashFunction, "for transformations and pos. int.",
[IsTransformation, IsInt],
function(p, hashlen)
return rec(func := HashFunctionForTransformation, data := [101, 
hashlen]);
end);

# new for 4.0! - GradedImagesOfTransSemigroup - "for a trans. semigroup"
###########################################################################

InstallMethod(GradedImagesOfTransSemigroup, "for a trans. semigroup",
[IsTransformationSemigroup],
function(s)
  local gens, n, ht, o, m, out, len, new, k, i, j;

  if IsSemigroup(s) then  
    gens:=Generators(s);
    n:=Degree(s);
  else
    gens:=s;
    n:=Degree(s[1]);
  fi;

  ht:=HTCreate([1..n], rec(hashlen:=1009));
  HTAdd(ht, [1..n], true);
  o:=[[1..n]]; m:=1; 

  if n<15 then 
    out:=List([1..n], x->EmptyPlist(Binomial(n, x)));
  else
    out:=List([1..n], x->[]);
  fi;

  len:=List([1..n], x-> 0);

  if IsMonoid(s) or IsMonoidAsSemigroup(s) then 
    out[n][1]:=[1..n]; 
    len[n]:=1;
  fi;

  for i in o do
    for j in gens do
      new:=OnSets(i, j);
      if HTValue(ht, new)=fail then 
	m:=m+1; o[m]:=new;
	HTAdd(ht, new, true);
	k:=Length(new);
	len[k]:=len[k]+1;
	out[k][len[k]]:=new;
      fi;
    od;
  od;

  return out;
end);

# new for 4.0! - GradedKernelsOfTransSemigroup - "for a trans. semigroup"
#############################################################################

InstallMethod(GradedKernelsOfTransSemigroup, "for a trans. semigroup",
[IsTransformationSemigroup],
function(s)
  local gens, n, ht, o, m, out, len, new, k, i, j;

  if IsSemigroup(s) then  
    gens:=GeneratorsAsListOfImages(s);
    n:=Degree(s);
  else
    gens:=s;
    n:=Degree(s[1]);
  fi;
 
  ht:=HTCreate([1..n], rec(hashlen:=10007)); HTAdd(ht, [1..n], true);
  o:=[[1..n]]; m:=1;

  if n<11 then 
    out:=List([1..n], x->EmptyPlist(Stirling2(n, x)));
  else
    out:=List([1..n], x->[]);
  fi;

  len:=List([1..n], x-> 0);

  if IsMonoid(s) or IsMonoidAsSemigroup(s) then 
    out[n][1]:=[1..n]; 
    len[n]:=1;
  fi;

  for i in o do
    for j in gens do
      new:=CanonicalTransSameKernel(i{j});
      if HTValue(ht, new)=fail then 
	m:=m+1; o[m]:=new;
	HTAdd(ht, new, true);
        k:=MaximumList(new);
        len[k]:=len[k]+1;
        out[k][len[k]]:=new;
      fi;
    od;
  od;

  return out;
end);

# new for 4.0!
#############################################################################

InstallGlobalFunction(HashTableForImagesFixedSize, 
function(img)
  local s, n, p, ht;

  s:=Set(img);
  n:=Length(img);
  
  if n<500 then 
    p:=Minimum(NextPrimeInt(Binomial(n, Length(s))), 100003);
  else
    p:=100003;
  fi;

  ht := HTCreate(s, rec( hfd := p, treehashsize := p ));
  HTAdd(ht, s, 1);

  return ht;
end);

# new for 4.0!
#############################################################################

InstallGlobalFunction(HashTableForImages, 
function(img)
  local s, p, ht;
  s:=Set(img);
  p:=Minimum(NextPrimeInt(2^Length(img)), 100003);

  ht := HTCreate(s, rec( hfd := p, treehashsize := p ));
  HTAdd(ht, s, 1);

  return ht;
end);

# new for 4.0!
#############################################################################

InstallGlobalFunction(HashTableForKernels, 
function(ker, n)
local p, ht;

if n<11 then 
  p:=Minimum(NextPrimeInt(Stirling2(n, Maximum(ker))), 100003);
else
  p:=100003;
fi;

ht := HTCreate(ker, rec( hfd := p, treehashsize := p ));
HTAdd(ht, ker, 1);

return ht;
end);

# new for 4.0!
#############################################################################

InstallGlobalFunction(HashFunctionForTransformation,
function(v,data) 
   return ORB_HashFunctionForIntList(v![1], data); 
end);

# new method and output for 4.0!
###########################################################################
#

InstallMethod(ImagesOfTransSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup],
s-> Orb(Generators(s), [1..Degree(s)], OnSets, rec(storenumbers:=true, 
schreier:=true)));

# new method and output for 4.0!
###########################################################################
# 

InstallOtherMethod(ImagesOfTransSemigroup, "for trans. semigp. and pos. int.", 
[IsTransformationSemigroup, IsPosInt],
function(s, m)
local n;
n:=Degree(s);

return Orb(Generators(s), [1..n], OnSets, rec(storenumbers:=true, 
gradingfunc:=function(o,x) return Length(x); end, schreier:=true,
onlygrades:=[m..n]));
end);

# new method and output for 4.0!
########################################################################### 
# JDM would it be useful here to make use of any kernels already known
# from the D-class computation?

# MN it would be useful to have a version of Orb which allowed us to change 
# onlygrades and then update the orbit!

InstallOtherMethod(KernelsOfTransSemigroup, "for a trans. semigroup", 
[IsTransformationSemigroup],  
function(s)
local gens, n, max, bound;

gens:=Generators(s);
n:=Degree(s); max:=Maximum(List(gens, Degree));

if max=n and n<1000 then 
	bound:=Bell(n);
elif n<1000 then 
	bound:=Sum([1..max], x-> Stirling2(n, x));
else
	bound:=100000;
fi;

return Orb(gens, List([1..n], x-> [x]), OnKernelsAntiAction, 
 rec(storenumbers:=true, 
 treehashsize:=NextPrimeInt(Minimum(100000, 3*bound)), schreier:=true));
end);

# new method and output for 4.0!
########################################################################### 

InstallOtherMethod(KernelsOfTransSemigroup, "for a trans. semigroup", 
[IsTransformationSemigroup, IsPosInt], 
function(s, m)
local n, max, bound, gens;

gens:=Generators(s);
n:=Degree(s); max:=Maximum(List(gens, Degree));

if max=n and n<1000 then 
	bound:=Bell(n);
elif n<1000 then 
	bound:=Sum([1..max], x-> Stirling2(n, x));
else
	bound:=100000;
fi;

return Orb(gens, List([1..n], x-> [x]), OnKernelsAntiAction, 
 rec(storenumbers:=true, 
 treehashsize:=NextPrimeInt(Minimum(100000, 3*bound)),
 gradingfunc:=function(o,x) return Length(x); end,
 onlygrades:=[m..max], schreier:=true));
end);

# new method for 4.0!
###########################################################################

InstallGlobalFunction(OnKernelsAntiAction, [IsList, IsTransformation],
function(ker, f)
local n, g, i;

n:=f![1];

if IsBound(TABLE_OF_TRANS_KERNEL) then 
  g:=TABLE_OF_TRANS_KERNEL(ker,Length(n));
else
  g:= EmptyPlist(Length(n)); 
  for i in [1..Length(ker)] do
    g{ker[i]}:= ListWithIdenticalEntries(Length(ker[i]), i);
  od;
fi;

g:= TransformationNC(g{n});
return ImageAndKernelOfTransformation(g)[2];
end);

###########################################################################

InstallGlobalFunction(OnTuplesOfSetsAntiAction, [IsObject, IsTransformation], 
function(tup, s)
local res, ker, set, perm, k;

ker:=ImageAndKernelOfTransformation(s)[2];
res:=[];

for set in tup do
	Unbind(perm);
	for k in ker do
		if k[1]^s in set then
			if IsBound(perm) then
				perm:=Union(perm, k);
			else 
				perm:=ShallowCopy(k);
			fi;
		fi;
	od;
	if IsBound(perm) then  
		Add(res, perm);
	fi;
od;
return res;
end);

# new method and output for 4.0!
#############################################################################
#

InstallGlobalFunction(StrongOrbitsInForwardOrbit, 
function(o)
local graph;

if not (IsOrbit(o) and IsBound(o!.orbitgraph)) then 
	Error("Usage: the argument should be an orbit with orbit graph ", 
	 "created by the orb package");
fi;

graph:=OrbitGraphAsSets(o);
graph:=STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(graph);

return List(graph, x-> o{x});
end);
