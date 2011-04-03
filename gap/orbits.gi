#############################################################################
##
#W  orbits.gi
#Y  Copyright (C) 2006-2011                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

# - this file is alphabetized, keep it that way!

# new for 4.0! - ChooseHashFunction - "for transformations and pos. int."
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

  if IsMonoid(s) or TransformationNC([1..n]) in s then 
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

  if IsMonoid(s) or TransformationNC([1..n]) in s then 
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

# new for 4.0! - HashTableForImages - not a user function!
#############################################################################

InstallGlobalFunction(HashTableForImages, 
function(img)
  local x, n, p, ht;

  x:=Set(img); n:=Length(img);
  
  if n<15 then 
    p:=Minimum(NextPrimeInt(Binomial(n, Length(x))), 1009);
  else
    p:=1009;
  fi;

  ht := HTCreate(x, rec( hfd := 1009, treehashsize := 1009 ));
  HTAdd(ht, x, 1);

  return ht;
end);

# new for 4.0! - HashTableForKernels - not a user function!
#############################################################################

InstallGlobalFunction(HashTableForKernels, 
function(ker, n)
  local p, ht;

  if n<11 then 
    p:=Minimum(NextPrimeInt(Stirling2(n, Maximum(ker))), 10007);
  else
    p:=10007;
  fi;

  ht := HTCreate(ker, rec( hfd := p, treehashsize := p ));
  HTAdd(ht, ker, 1);

  return ht;
end);

# new for 4.0! - HashFunctionForTransformation - not a user function!
#############################################################################

InstallGlobalFunction(HashFunctionForTransformation,
function(v,data) 
   return ORB_HashFunctionForIntList(v![1], data); 
end);

# new for 4.0! - ImagesOfTransSemigroup - "for a transformation semigroup"
###########################################################################

InstallMethod(ImagesOfTransSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup],
  s-> Orb(Generators(s), [1..Degree(s)], OnSets, rec(storenumbers:=true, 
   schreier:=true)));

# new for 4.0! - ImagesOfTransSemigroup - "for trans semigp and pos int"
###########################################################################

InstallOtherMethod(ImagesOfTransSemigroup, "for trans semigp and pos int", 
[IsTransformationSemigroup, IsPosInt],
function(s, m)
local n;
  n:=DegreeOfTransformationSemigroup(s);

  return Orb(Generators(s), [1..n], OnSets, rec(storenumbers:=true, 
   gradingfunc:=function(o,x) return Length(x); end, schreier:=true,
  onlygrades:=[m..n]));
end);

# new for 4.0! - KernelsOfTransSemigroup - "for a trans. semigroup"
########################################################################### 

InstallOtherMethod(KernelsOfTransSemigroup, "for a trans. semigroup", 
[IsTransformationSemigroup],  
function(s)
  local n, bound;

  n:=DegreeOfTransformationSemigroup(s); 
  
  if n<8 then
    bound:=Minimum(NextPrimeInt(Bell(n)), 10007);
  else
    bound:=10007;
  fi;

  return Orb(GeneratorsAsListOfImages(s), [1..n], function(f,g) return
   CanonicalTransSameKernel(f{g}); end, rec(storenumbers:=true, 
   treehashsize:=bound, schreier:=true));
end);

# new for 4.0! - KernelsOfTransSemigroup - "for trans semigp pos int"
########################################################################### 

InstallOtherMethod(KernelsOfTransSemigroup, "for trans semigp pos int", 
[IsTransformationSemigroup, IsPosInt], 
function(s, m)
  local n, max, bound, gens;

  n:=DegreeOfTransformationSemigroup(s); 
  max:=MaximumList(Generators(s), RankOfTransformation);
  
  if n<11 then
    bound:=Minimum(NextPrimeInt(Stirling2(n,m)), 10007);
  else
    bound:=10007;
  fi;

  return Orb(GeneratorsAsListOfImages(s), [1..n], function(f,g) return
   CanonicalTransSameKernel(f{g}); end, rec(storenumbers:=true, 
   treehashsize:=bound, gradingfunc:=function(o,x) return Length(x); end,
   onlygrades:=[m..max], schreier:=true));
end);

# new for 4.0! - OnKernelsAntiAction - for a trans img list and same 
###########################################################################

InstallGlobalFunction(OnKernelsAntiAction, 
[IsTransformation, IsTransformation],
function(f,g)
  return CanonicalTransSameKernel(f{g});  
end);

# new for 4.0! - StrongOrbitsInForwardOrbit - for IsOrbit
#############################################################################

InstallGlobalFunction(StrongOrbitsInForwardOrbit, [IsOrbit], 
function(o)
  local graph;

  if not IsBound(o!.orbitgraph) then 
    Error("Usage: the argument should be an orbit with orbit graph ", 
     "created by the orb package");
  fi;

  graph:=STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(OrbitGraphAsSets(o));

  return List(graph, x-> o{x});
end);
