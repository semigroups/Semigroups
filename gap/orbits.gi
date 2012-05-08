#############################################################################
##
#W  orbits.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# - this file is alphabetized, keep it that way!

#CCC

# new for 0.2! - ChooseHashFunction - "for blist and pos. int."
#############################################################################

InstallMethod(ChooseHashFunction, "for blist and pos. int.",
[IsBlistRep, IsPosInt],
function(p, hashlen)
  return rec(func := HashFunctionForBlist, data := [101, hashlen]);
end);

# new for 0.4! - CitrusEvalWord - "for trans. imgs. and  pos. ints".
#############################################################################

InstallGlobalFunction(CitrusEvalWord,
function(gens, w)
  local  i, res;
        
  if Length( w ) = 0  then
    return [1..Length(gens[1])];
  fi;
  res := gens[w[1]];
  for i  in [ 2 .. Length( w ) ]  do
    res := gens[w[i]]{res};
  od;
  return res;
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

  ht:=HTCreate([1..n], rec(hashlen:=s!.opts!.hashlen!.S));
  HTAdd(ht, [1..n], true);
  o:=[[1..n]]; m:=1; 

  if n<15 then 
    out:=List([1..n], x->EmptyPlist(Binomial(n, x)));
  else
    out:=List([1..n], x->[]);
  fi;

  len:=List([1..n], x-> 0);

  if IsMonoid(s) or TransformationNC([1..n]*1) in s then 
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
 
  ht:=HTCreate([1..n], rec(hashlen:=s!.opts!.hashlen!.S)); 
  HTAdd(ht, [1..n], true);
  o:=[[1..n]]; m:=1;

  if n<11 then 
    out:=List([1..n], x->EmptyPlist(Stirling2(n, x)));
  else
    out:=List([1..n], x->[]);
  fi;

  len:=List([1..n], x-> 0);

  if IsMonoid(s) or TransformationNC([1..n]*1) in s then 
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

# new for 0.2! - HashFunctionForBlist - "for a blist"
#############################################################################

InstallGlobalFunction(HashFunctionForBlist, 
function(v, data)
  return ORB_HashFunctionForIntList(ListBlist([1..Length(v)], v), data);
end);

# new for 0.1! - HashFunctionForTransformation - not a user function!
#############################################################################

#InstallGlobalFunction(HashFunctionForTransformation,
#function(v,data) 
#   return ORB_HashFunctionForIntList(v![1], data); 
#end);

#III

# new for 0.1! - ImagesOfTransSemigroup - "for a transformation semigroup"
###########################################################################
# Notes: this orbit always contains [1..Degree(s)] even if this is not the
# image of any element in s. 

InstallMethod(ImagesOfTransSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
  s-> Orb(Generators(s), [1..Degree(s)], function(set, f) return
  Set(f![1]{set}); end, rec(storenumbers:=true, 
   schreier:=true)));

# new for 0.2! - ImagesOfTransSemigroupAsBlists - "for a trans. semigroup"
###########################################################################
# Notes: this orbit always contains [1..Degree(s)] even if this is not the
# image of any element in s. 

InstallMethod(ImagesOfTransSemigroupAsBlists, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local n, seed;

  n:=Degree(s); seed:=BlistList([1..n], [1..n]); 
  return Orb(Generators(s), seed, OnBlist, 
   rec(storenumbers:=true, schreier:=true));
end);

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

#KKK

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

#OOO

# new for 0.2! - OnBlist - for blist and transformation 
###########################################################################
# Notes: this is BlistList([1..Length(blist)], 
# OnSets(ListBlist([1..Length(blist)], blist), f));

InstallGlobalFunction(OnBlist, 
[IsBlist, IsTransformation], 
function(blist, f)
  local n, img;
  n:=Length(blist); img:=f![1]; 
  return BlistList([1..n], img{ListBlist([1..n], blist)}); 
end);

# new for 0.1! - OnKernelsAntiAction - for a trans img list and same 
###########################################################################

InstallGlobalFunction(OnKernelsAntiAction, 
[IsList, IsTransformation],
function(ker, f)
  return CanonicalTransSameKernel(ker{f![1]});  
end);

# new for 0.4! - OrbSCC - "for an orbit"
#############################################################################

InstallGlobalFunction(OrbSCC,
function(o)
  local scc, r, i;

  if IsBound(o!.scc) then 
    return o!.scc;
  fi;

  if not IsClosed(o) then #JDM good idea?
    Enumerate(o);
  fi;

  scc:=Set(List(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(OrbitGraphAsSets(o)),
     Set));;
  r:=Length(scc);

  o!.scc:=scc;
  o!.scc_lookup:=ListWithIdenticalEntries(Length(o), 1);

  if Length(scc)>1 then
    for i in [2..r] do
      o!.scc_lookup{scc[i]}:=ListWithIdenticalEntries(Length(scc[i]), i);
    od;
  fi;

  o!.truth:=List([1..r], i-> BlistList([1..Length(o)], scc[i]));
  
  return scc;
end); 

# new for 0.4! - OrbSCCLookup - "for an orbit"
#############################################################################

InstallGlobalFunction(OrbSCCLookup, 
function(o)

  if IsBound(o!.scc_lookup) then 
    return o!.scc_lookup;
  fi;

  OrbSCC(o);
  return o!.scc_lookup;
end);

# new for 0.4! - OrbSCCTruthTable - "for an orbit"
#############################################################################

InstallGlobalFunction(OrbSCCTruthTable, 
function(o)

  if IsBound(o!.truth) then
    return o!.truth;
  fi;

  OrbSCC(o);
  return o!.truth;
end);

#RRR

# new for 0.4! - ReverseSchreierTreeOfSCC - "for an orbit and pos. int."
###########################################################################

InstallGlobalFunction(ReverseSchreierTreeOfSCC,
function(o, i)
  local r, graph, rev, scc, gen, pos, seen, t, oo, j, k, l, m;

  r:=Length(OrbSCC(o));

  if i>r then 
    Error("the orbit only has ", r, " strongly connected components,");
    return;
  fi;

  if not IsBound(o!.reverse) then
    o!.reverse:=EmptyPlist(r);
  fi;
 
  if IsBound(o!.reverse[i]) then
    return o!.reverse[i];
  fi;

  if not IsBound(o!.rev) then
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

    o!.rev:=rev;
  fi;

  scc:=o!.scc[i]; rev:=o!.rev;

  gen:=ListWithIdenticalEntries(Length(o), fail);
  pos:=ListWithIdenticalEntries(Length(o), fail);
  seen:=BlistList([1..Length(o)], [scc[1]]);
  t:=o!.truth[i]; oo:=EmptyPlist(Length(scc));
  oo[1]:=scc[1]; j:=0;

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
  
  o!.reverse[i]:=[gen, pos];
  return [gen, pos];
end);

#TTT

# new for 0.7! - TransformationActionNC - "for mult elt, list, function"
###############################################################################

InstallMethod(TransformationActionNC, "for mult elt, list, function",
[IsObject, IsList, IsFunction],
function(f, dom, act)
  local n, out, i;
  
  n:=Size(dom);
  out:=EmptyPlist(n);
  
  for i in [1..n] do 
    out[i]:=Position(dom, act(dom[i], f));
  od;

  return Transformation(out);
end);

# new for 0.7! - TransformationActionNC - "for semigroup, list, function"
###############################################################################

InstallOtherMethod(TransformationActionNC, "for a semigroup, list, function",
[IsSemigroup, IsList, IsFunction],
function(s, dom, act)
  return List(Generators(s), f-> TransformationActionNC(f, dom, act));
end);

#SSS

# new for 0.4! - SchreierTreeOfSCC - "for an orbit and pos. int."
###########################################################################

InstallGlobalFunction(SchreierTreeOfSCC,
function(o, i)
  local r, scc, len, gen, pos, seen, t, oo, m, graph, j, k, l, len_k;

  r:=Length(OrbSCC(o));

  if i>r then 
    Error("the orbit only has ", r, " strongly connected components,");
    return;
  fi;
  
  if not IsBound(o!.trees) then
    o!.trees:=EmptyPlist(r);
  fi;

  if IsBound(o!.trees[i]) then 
    return o!.trees[i];
  fi;

  if i=1 then
    o!.trees[i]:=[o!.schreiergen, o!.schreierpos];
    return o!.trees[i];
  fi;

  scc:=o!.scc[i]; len:=Length(o);
  gen:=ListWithIdenticalEntries(len, fail);
  pos:=ListWithIdenticalEntries(len, fail);
  seen:=BlistList([1..len], [scc[1]]);
  t:=o!.truth[i];
  oo:=[scc[1]]; m:=1;
  graph:=OrbitGraph(o);
  j:=0;
  len:=Length(scc);

  while m<len do
    j:=j+1; k:=oo[j]; l:=0; len_k:=Length(graph[k]);
    while l<len_k and m<len do
      l:=l+1;
      if IsBound(graph[k][l]) and not seen[graph[k][l]] and t[graph[k][l]] then
        m:=m+1;
        oo[m]:=graph[k][l]; seen[graph[k][l]]:=true;
        gen[graph[k][l]]:=l; pos[graph[k][l]]:=k;
      fi;
    od;
  od;
  o!.trees[i]:=[gen, pos];

  return [gen, pos];
end);

# new for 0.4! - SchutzGps - "for a trans. semigroup"
#############################################################################

InstallMethod(SchutzGps, "for a trans. semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
s-> List(CitrusSkeleton(s)!.schutz, x-> x[2]));

# new for 0.4! - CitrusSkeleton - "for a trans. semigroup"
#############################################################################

InstallMethod(CitrusSkeleton, "for a trans. semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local o, scc, r, gens, perms, schutz, i;

  o:=ImagesOfTransSemigroup(s); Enumerate(o); 
  scc:=OrbSCC(o); r:=Length(scc); gens:=GeneratorsAsListOfImages(s);
  o!.perms:=EmptyPlist(Length(o)); schutz:=EmptyPlist(r);

  for i in [1..r] do 
    ReverseSchreierTreeOfSCC(o, i);
    o!.perms:=o!.perms+CreateImageOrbitSCCPerms(gens, o, i);
    SchreierTreeOfSCC(o, i);
    schutz[i]:=CreateImageOrbitSchutzGp(gens, o,
     CitrusEvalWord(gens, TraceSchreierTreeForward(o, scc[i][1])), i);
  od;
  
  o!.schutz:=schutz;
  
  return o;
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

# mod for 0.4! - TraceSchreierTreeOfSCCBack - "for an orb, pos int, pos int"
#############################################################################
# Usage: o = orbit of images; i = index of scc; j = element of scc[i].

# Notes: returns a word in the generators that takes o[j] to o!.scc[i][1]  
# assuming that j in scc[i]

InstallGlobalFunction(TraceSchreierTreeOfSCCBack,
function(o, i, j)
  local tree, scc, word;

  tree:=ReverseSchreierTreeOfSCC(o, i);
  scc:=OrbSCC(o)[i];

  word := [];
  while j > scc[1] do
    Add(word, tree[1][j]);
    j := tree[2][j];
  od;
  return word;
end);

# mod for 0.4! - TraceSchreierTreeOfSCCForward - "for an orb, pos int, pos int"
#############################################################################
# Usage: o = orbit of images; i = index of scc; j = element of scc[i].

# Notes: returns a word in the generators that takes o!.scc[i][1] to o[j] 
# assuming that j in scc[i]

InstallGlobalFunction(TraceSchreierTreeOfSCCForward,
function(o, i, j)
  local tree, scc, word;

  tree:=SchreierTreeOfSCC(o, i);
  scc:=OrbSCC(o)[i];

  word := [];
  while j > scc[1] do
    Add(word, tree[1][j]);
    j :=tree[2][j];
  od;
  return Reversed(word);
end);

#EOF
