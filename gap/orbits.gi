#############################################################################
##
#W  orbits.gi
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# - this file is alphabetized, keep it that way!

# new for 0.1! - ChooseHashFunction - "for transformations and pos. int."
#############################################################################

InstallMethod(ChooseHashFunction, "for transformations and pos. int.",
[IsTransformation, IsInt],
function(p, hashlen)
  return rec(func := HashFunctionForTransformation, data := [101, 
   hashlen]);
end);

# new for 0.1! - GradedImagesOfTransSemigroup - "for a trans. semigroup"
###########################################################################

InstallMethod(GradedImagesOfTransSemigroup, "for a trans. semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
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

# new for 0.1! - GradedKernelsOfTransSemigroup - "for a trans. semigroup"
#############################################################################

InstallMethod(GradedKernelsOfTransSemigroup, "for a trans. semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, n, ht, o, m, out, len, new, k, i, j;

  if IsSemigroup(s) then  
    gens:=GeneratorsAsListOfImages(s);
    n:=Degree(s);
  else
    gens:=s;
    n:=Degree(s[1]);
  fi;
 
  ht:=HTCreate([1..n], rec(hashlen:=1009)); HTAdd(ht, [1..n], true);
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

# new for 0.1! - HashTableForImages - not a user function!
#############################################################################

InstallGlobalFunction(HashTableForImages, 
function(img)
  local img_set, ht;

  img_set:=Set(img); 
  ht := HTCreate(img_set, rec( hfd := 1009, treehashsize := 1009 ));
  HTAdd(ht, img_set, 1);

  return ht;
end);

# new for 0.1! - HashTableForKernels - not a user function!
#############################################################################

InstallGlobalFunction(HashTableForKernels, 
function(ker, n)
  local ht;

  ht := HTCreate(ker, rec( hfd := 1009, treehashsize := 1009 ));
  HTAdd(ht, ker, 1);

  return ht;
end);

# new for 0.1! - HashFunctionForTransformation - not a user function!
#############################################################################

InstallGlobalFunction(HashFunctionForTransformation,
function(v,data) 
   return ORB_HashFunctionForIntList(v![1], data); 
end);

# new for 0.1! - ImagesOfTransSemigroup - "for a transformation semigroup"
###########################################################################
# Notes: this orbit always contains [1..Degree(s)] even if this is not the
# image of any element in s. 

InstallMethod(ImagesOfTransSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
  s-> Orb(Generators(s), [1..Degree(s)], OnSets, rec(storenumbers:=true, 
   schreier:=true)));

# new for 0.1! - ImagesOfTransSemigroup - "for trans semigp and pos int"
###########################################################################

InstallOtherMethod(ImagesOfTransSemigroup, "for trans semigp and pos int", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsPosInt],
function(s, m)
local n;
  n:=DegreeOfTransformationSemigroup(s);

  return Orb(Generators(s), [1..n], OnSets, rec(storenumbers:=true, 
              schreier:=true, 
              gradingfunc:=function(o,x) return Length(x); end, 
              onlygrades:=function(x,y) return x in y; end, 
              onlygradesdata:=[m..n]));
end);

# new for 0.1! - KernelsOfTransSemigroup - "for a trans. semigroup"
########################################################################### 

InstallOtherMethod(KernelsOfTransSemigroup, "for a trans. semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],  
function(s)
  local n, bound;

  n:=DegreeOfTransformationSemigroup(s); 
  
  return Orb(GeneratorsAsListOfImages(s), [1..n], function(f,g) return
   CanonicalTransSameKernel(f{g}); end, rec(storenumbers:=true, 
   treehashsize:=1009, schreier:=true));
end);

# new for 0.1! - KernelsOfTransSemigroup - "for trans. semi. and  pos. int."
########################################################################### 

InstallOtherMethod(KernelsOfTransSemigroup, "for trans. semi. and pos. int.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsPosInt], 
function(s, m)
  local n;

  n:=DegreeOfTransformationSemigroup(s); 

  return Orb(GeneratorsAsListOfImages(s), [1..n], 
              function(f,g) return CanonicalTransSameKernel(f{g}); end, 
              rec(storenumbers:=true, treehashsize:=1009, 
              gradingfunc:=function(o,x) return Maximum(x); end,
              onlygrades:=function(x, y) return x in y; end, 
              onlygradesdata:=[m..n], schreier:=true));
end);

# new for 0.1! - OnKernelsAntiAction - for a trans img list and same 
###########################################################################

InstallGlobalFunction(OnKernelsAntiAction, 
[IsList, IsTransformation],
function(ker, f)
  return CanonicalTransSameKernel(ker{f![1]});  
end);

# new for 0.1! - StrongOrbitsInForwardOrbit - for IsOrbit
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

#EOF

