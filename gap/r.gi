#############################################################################
##
#W  r.gi
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ), Transformation( [ 2, 4, 1, 2 ]$
#>   Transformation( [ 3, 1, 1, 3 ] ), Transformation( [ 3, 3, 4, 1 ] ) ]
#> ;;


# Factorization(s, f);

# 1) calculate the tree that says how to get from one R-class rep to another by
# applying which generator (using rep_to_o and an as yet unwritten inverse of
# OrbitsOfImages(s)!.graph). DONE!

# 2) do something like d1:=PreInOrbitsOfImages(s, f, false) (unrectified image)
# where f is the element giving rise to the R-class rep. 

# 3) find the word from the tree in 1) that takes me to the element g of the
# potential R-class reps orbit that yields the R-class rep of the R-class of f
# call this w_1

# 4) do PreInOrbitsOfImages(s, g, false) and TraceSchreierTreeOfSCCBack to find
# which generators to apply to rectify the image of g call these w_2

# 5) Find p:=PermLeftQuoTransformationNC(R-class rep, g with rect image) 

# 6) Give p as a word in the generators of SchutzGp(R-class)

# 7) Find p as a word in generators of s using words from
# CreateSchutzGpOfImgOrb (or whatever) [i, j, k]->
# Concatenation(TraceSchrBack(o, ?, i), Generators(s)[j], TraceSchrForward(k))
# call this w_3

# 8) Find the word using TraceSchreierTreeForward(o, d[1][4], d[1][3]) that
# unrectifies the image of f and call this w_4;

# 9) The word you are looking for is w_1, w_2, w_3, w_4 (this should anyway
# yield something with the same kernel and image as f if it is not f, then
# maybe separate the computation for r-class reps and other elements.)

#############################################################################
# Conventions:

# - do not use the functions from convenience.gi! (double-check!)

# - use ImageOrbitFromData instead of ImageOrbit!

# - don't use underlyingcollection in enumerators!

# - check that wherever we have called d:=InOrbitsOfImages we do not perform
# f*perms[l] afterwards but instead use d[7]!

# - ensure EmptyPlist is properly used!

#############################################################################
## Notes

# - should make more use of OrbitsOfImages(s)!.images.

# - this file is alphabetized, keep it that way!

# - this file should only contain functions relating to images/r-classes!

#############################################################################
## To do 

# - what about IsGreensLessThanOrEqual?

# - install methods for Generators etc of IsRightSemigroupIdeal, IsLeftSemigroupIdeal
#   In particular, so that IsGreensLessThanOrEqual works.

# - install method for Position(GreensRClasses(s), blah) using InOrbitsOfImages...
# requires the list GreensRClasses to have some property to allow method
# selection. Wait for future version. 

# - install IteratorOfRClassRepsData etc for s and IsPosInt! or more generally
# create an iterator of R-classes satisfying whatever properties we want. 

#############################################################################
# other equalities of Green's classes handled by generic method in greens.gi!

# new for 0.1! - \= - "for R-class and R-class of trans. semigp."
#############################################################################

InstallMethod(\=, "for R-class and R-class of trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp, IsGreensRClass and 
IsGreensClassOfTransSemigp],
function(r1, r2)
  return r1!.parent=r2!.parent and r1!.rep in r2;
end);

# new for 0.1! - \< - "for R-class and R-class of trans. semigp."
#############################################################################

InstallMethod(\<, "for R-class and R-class of trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp, IsGreensRClass and 
IsGreensClassOfTransSemigp],
function(r1, r2)
  return r1!.parent=r2!.parent and r1!.rep < r2!.rep;
end);

# new for 0.1! - \in - "for trans. and R-class of trans. semigp."
#############################################################################
# Algorithm E. 

InstallMethod(\in, "for trans. and R-class of trans. semigp.", 
[IsTransformation, IsGreensRClass and IsGreensClassOfTransSemigp],
function(f, r)
  local rep, s, d, o, i, g, schutz;

  rep:= r!.rep; 

  if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or
   RankOfTransformation(f) <> RankOfTransformation(rep) or
   CanonicalTransSameKernel(f) <> CanonicalTransSameKernel(rep) then 
    return false;
  fi;

  s:=r!.parent;
  d:=r!.data;
  o:=r!.o!.orbits[d[1]][d[2]];

  i:= Position(o, ImageSetOfTransformation(f));

  if i = fail or not o!.truth[d[4]][i] then #check they are in the same scc
    return false;
  fi;

  schutz:=ImageOrbitStabChain(r);
  
  if schutz=true then 
    return true;
  fi;

  g:=f*o!.perms[i];

  if g=rep then
    return true;
  elif schutz=false then 
    return false;
  fi;

  return SiftedPermutation(schutz, PermLeftQuoTransformationNC(rep, g))=();
end);

#AAA

# new for 0.1! - AddToOrbitsOfImages - not a user function! 
#############################################################################
# Usage: s = semigroup or d-class; f = transformation; data = image data; 
# o = OrbitsOfImages(s).

# Notes: if s is a d-class, then data should have j, k, l, m and g not = fail!

InstallGlobalFunction(AddToOrbitsOfImages,
function(s, f, data, o)
  local j, k, l, m, val, n, g, O, gens, d, lens, data_ht, t, one, images, ht, graph, gen, pos, oo, reps, out, i, reps_pos, z, y;
  
  j:=data[1]; 	# img size
  k:=data[2]; 	# index of orbit containing img
  l:=data[3]; 	# position of img in O[j][k]
  m:=data[4]; 	# scc of O[j][k] containing img
  val:=data[5]; # position of ker in O[j][k]!.kernels_ht[m]
  n:=data[6]; 	# the length of O[j][k]!.reps[m][val]
  g:=data[7];	# f*O[j][k]!.perms[l];

  O := o!.orbits;  gens:=o!.gens; d:=o!.data; lens:=o!.lens;
  data_ht:=o!.data_ht; t:=Length(gens);

  if IsBound(o!.ht) then # o = OrbitsOfImages(s)
    one:=o!.one; images:=o!.images; 
    ht:=o!.ht; graph:=o!.graph; gen:=o!.gen; pos:=o!.pos; o:=ht!.o; 
  fi;

 # if r=6 then Error(""); fi;

  if k = fail then  #new img and l, m, val, n, g=fail
                    #don't call this function with a d-class and k=fail!
  ################################################################################
          
    lens[j]:=lens[j]+1;
    oo:=ForwardOrbitOfImage(s, f, images, gens);
          
    if IsBound(O[j]) then 
      O[j][lens[j]]:=oo[1];
    else
      O[j]:=[oo[1]];
    fi;
          
    for i in oo[1] do 
      HTAdd(images, i, lens[j]);
    od;
          
    reps:=oo[2]; #reps. corresponding to all scc of oo[1]
    out:=[j, lens[j], 1, 1, 1, 1];
    i:=Length(d); reps_pos:=EmptyPlist(Length(oo[3]));
    
    for m in [1..Length(oo[3])] do  # oo[3] is a list of the first index in
                                    # every scc of oo[1]
      i:=i+1;
      d[i]:=[j, lens[j], oo[3][m], m, 1, 1]; 
      HTAdd(data_ht, d[i], i);

      reps_pos[m]:=i;
    od;

  ##############################################################################

  else #old img
    reps:=O[j][k]!.reps[m];
      
    if not val=fail then #old kernel
      reps[val][n+1]:=g;
      out:=[j, k, l, m, val, n+1];
      i:=Length(d)+1;
      d[i]:=out; 
      reps_pos:=[i];
      HTAdd(data_ht, out, i);
    else #new kernel
      val:=Length(reps)+1;
      reps[val]:=[g];
      out:=[j, k, l, m, val, 1];
      i:=Length(d)+1;
      d[i]:=out;
      reps_pos:=[i];
      HTAdd(data_ht, out, i);
      HTAdd(O[j][k]!.kernels_ht[m], CanonicalTransSameKernel( g ), val);
    fi;
    reps:=[g]; 
  fi;

  ##############################################################################
  #install new pts in the orbit

  if IsBound(ht) then 
    i:=Length(o); 
    for j in [1..Length(reps)] do
      f:=reps[j];
      for y in [1..Length(gens)] do
        z:=gens[y]*f;
        if HTValue(ht, z)=fail then  
          i:=i+1;
          HTAdd(ht, z, i);
          o[i]:=z;
          pos[i]:=reps_pos[j]; gen[i]:=y;
        
         #schreier words here
          HTAdd(ht, z, true);
          i:=i+1; o[i]:=z;
        fi;
      od;
    od;
  fi;

  return out;
end);

# new for 0.1! - AsList - "for an R-class of trans. semigp."
#############################################################################
# Algorithm D.

InstallOtherMethod(AsList, "for an R-class of trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local f, g, elts, perms, scc, o, p, i;
  
  Info(InfoCitrusGreens, 4, "AsList: for an R-class");

  f:=r!.rep; #rep should have its image at the first place in the scc
  g:=List(SchutzenbergerGroup(r), x-> f*x);
  elts:=EmptyPlist(Size(r));

  perms:=ImageOrbitPerms(r);
  scc:=ImageOrbitSCC(r);
  o:=ImageOrbit(r); #this doesn't slow things down here!

  for i in ImageOrbitSCC(r) do 
    p:=perms[i];
    elts:=Concatenation(elts, g*p^-1);
  od;
  return elts;
end);

# new for 0.1! - AsSSortedList - "for R-class of trans. semigp."
#############################################################################
# this should be removed after the library method for AsSSortedList 
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)!

InstallOtherMethod(AsSSortedList, "for R-class of trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  Info(InfoCitrusGreens, 4, "AsSSortedList: for an R-class");
  return ConstantTimeAccessList(EnumeratorSorted(r));
end);

#CCC

# new for 0.1! - CreateImageOrbitSCCPerms - not a user function!
#############################################################################
# Usage: gens = Generators(s); o = image orbit; j = index of scc.

InstallGlobalFunction(CreateImageOrbitSCCPerms,
function(gens, o, j)
  local p, scc, f, i;

  p:=EmptyPlist(Length(o));
  scc:=o!.scc[j];

  for i in scc do
  	f:=EvaluateWord(gens, TraceSchreierTreeOfSCCBack(o, j, i));
  	p[i]:=MappingPermListList(o[i], OnTuples(o[i], f));
  od; #JDM this works also!

  #for i in scc do
  #  f:=EvaluateWord(gens, TraceSchreierTreeOfSCCForward(o, j, i)); 
  #  p[i]:=MappingPermListList(OnTuples(o[scc[1]], f), o[scc[1]]);
  #od;

  return p;
end);

# new for 0.1! - CreateImageOrbitSchutzGp - not a user function!
#############################################################################
# Usage: gens = generators of the semigroup; o = image orbit;
# f = representative of scc with index k;
# k = index of scc containing position of image of f in o.

InstallGlobalFunction(CreateImageOrbitSchutzGp,
function(gens, o, f, k) 
  local scc, bound, g, p, t, graph, is_sym, l, words, h, i, j;

  scc:=o!.scc[k];

  if Length(o[scc[1]])<1000 then 
    bound:=Factorial(Length(o[scc[1]]));
  else
    bound:=infinity;
  fi;

  g:=Group(()); p:=o!.perms; t:=o!.truth;
  graph:=OrbitGraph(o); is_sym:=false; l:=0; words:=[];

  for i in scc do 
    for j in [1..Length(gens)] do 
    
      if IsBound(graph[i][j]) and t[k][graph[i][j]] then
        #JDM potentially dangerous change here!
        #h:=PermLeftQuoTransformationNC(f, f/p[i] * (gens[j]*p[graph[i][j]]));
        h:=PermLeftQuoTransformationNC(f,
        f*EvaluateWord(gens, TraceSchreierTreeOfSCCForward(o, k,
        i)) * (gens[j]*p[graph[i][j]]));
        if not h=() then 
          g:=ClosureGroup(g, h);
          l:=l+1;
          words[l]:=[i, j, graph[i][j]];
        fi;

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

  if is_sym then
    return [true, g, words];
  elif Size(g)=1 then 
    return [false, g, words];
  fi;
    
  return [StabChainImmutable(g), g, words];
end);

# new for 0.1! - CreateRClass - not a user function!
#############################################################################
# Usage: s = semigroup; data = image data (any lengths); 
# orbit = OrbitsOfImages(s) or local variant; rep = representative.

# Notes: data[3]=l should be for the representative which has rectified image,
# rep should be with rectified image only!

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

# new for 0.1! - CreateSchreierTreeOfSCC - not a user function!
#############################################################################
# Usage: o is the image/kernel orbit, i is the index of the scc!

InstallGlobalFunction(CreateSchreierTreeOfSCC,
function(o, i)
  local scc, gen, pos, seen, t, oo, graph, j, k, l;

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

# new for 0.1! - CreateReverseSchreierTreeOfSCC - not a user function!
#############################################################################
# Usage: o is the image/kernel orbit, i is the index of the scc!

InstallGlobalFunction(CreateReverseSchreierTreeOfSCC,
function(o, i)
  local graph, rev, scc, gen, pos, seen, t, oo, j, k, l, m;

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
  t:=o!.truth[i]; oo:=[scc[1]]; j:=0;

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

#DDD

# new for 0.1! - DisplayOrbitsOfImages - not a user function!
#############################################################################

InstallGlobalFunction(DisplayOrbitsOfImages, 
function(s)
  local o, k, i, j;

  o:=OrbitsOfImages(s);

  Print("finished: \t", o!.finished, "\n");
  Print("orbits: \t"); 

  if ForAny([1..Degree(s)], j-> IsBound(o!.orbits[j])) then 
    k:=0;
    for i in o!.orbits do 
      for j in i do 
        if k=1 then 
          Print("\t\t"); 
        else
          k:=1;
        fi;
        View(j); Print("\n");
      od;
    od;
  else 
    Print("\n");
  fi;

  Print("at: \t\t", o!.at, "\n");
  Print("ht: \t\t"); View(o!.ht); Print("\n");
  Print("size: \t\t", SizeOrbitsOfImages(s), "\n");
  Print("R-classes: \t", NrRClassesOrbitsOfImages(s), "\n");
  Print("data ht: \t"); View(o!.data_ht); Print("\n");
  Print("images: \t"); View(o!.images); Print("\n");
  return true;
end);

#EEE

# new for 0.1! - Enumerator - "for R-class of trans. semigp."
#############################################################################

InstallOtherMethod(Enumerator, "for R-class of trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local enum;

  Info(InfoCitrusGreens, 4, "Enumerator: for an R-class");

  enum:=EnumeratorByFunctions(r, rec(
          
    rep:=r!.rep, len:=Size(SchutzenbergerGroup(r)),
    
    schutz:=Enumerator(SchutzenbergerGroup(r)), 
    
    perms:=ImageOrbitPerms(r), scc:=ImageOrbitSCC(r),
    
    ########################################################################
  
    ElementNumber:=function(enum, pos)
      local q, n, m;

      if pos>Length(enum) then 
        return fail;
      fi;
          
      if pos<=enum!.len then 
        return enum!.rep*enum!.schutz[pos];
      fi;
          
      n:=pos-1; m:=enum!.len;
      q:=QuoInt(n, m); pos:=[ q, n - q * m ]+1;

      return enum[pos[2]]*ImageOrbitPerms(r)[ImageOrbitSCC(r)[pos[1]]]^-1;    
    end, 
    
    ########################################################################
    
    NumberElement:=function(enum, f)
      local rep, d, s, o, i, j;
      rep:=enum!.rep;
    
      if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or
       RankOfTransformation(f) <> RankOfTransformation(rep) or
        CanonicalTransSameKernel(f) <> CanonicalTransSameKernel(rep) then
          return fail;
      fi;
            
      if f=rep then 
        return 1;
      fi;
        
      o:= ImageOrbit(r); d:=r!.data;
      i:= Position(o, ImageSetOfTransformation(f));
            
      if i = fail or not o!.truth[d[4]][i] then 
        return fail;
      fi;
            
      j:= Position(enum!.schutz,
       PermLeftQuoTransformationNC(rep, f*o!.perms[i]));
            
      if j = fail then 
        return fail;
      fi;
            
      return enum!.len*(Position(ImageOrbitSCC(r), i)-1)+j;
    end, 

    #########################################################################
    
    Membership:=function(elm, enum) 
      return elm in r; 
    end,
    
    Length:=enum -> Size(r),

    PrintObj:=function(enum)
      Print( "<enumerator of R-class>");
      return;
    end));

  return enum;
end);

# new for 0.1! - Enumerator - "for a transformation semigroup"
#############################################################################
# JDM this could be an actual enumerator!?

InstallOtherMethod(Enumerator, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local out, iter, j, i;

  Info(InfoCitrusGreens, 4, "Enumerator: for a trans. semigroup");

  out:=EmptyPlist(Size(s)); #it is very slightly faster than taking []

  iter:=Iterator(s);
  j:=0;

  for i in iter do 
    j:=j+1;
    out[j]:=i;
  od;

  return Immutable(out);
end);

# new for 0.1! - ExpandOrbitsOfImages - not a user function!
#############################################################################
# JDM could put a more highly optimized version of this function in, not using
# an iterator, and passing all record components !. to InOrbitsOfImages and
# AddToOrbitsOfImages. 

InstallGlobalFunction(ExpandOrbitsOfImages, 
function(s)
  local o, iter, i;

  Info(InfoCitrusGreens, 4, "ExpandOrbitsOfImages");

  o:=OrbitsOfImages(s);

  if not o!.finished then 
    iter:=IteratorOfNewRClassRepsData(s);
    for i in iter do od;
  fi;
  return true;
end);

#FFF

# new for 0.2! - Factorization - "for a transformation semigroup"
#############################################################################

InstallOtherMethod(Factorization, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsTransformation], 
function(s, f)
  local d, l, o;
  
  if not f in s then 
    return fail;
  fi;#hack

  d:=PreInOrbitsOfImages(s, f, false);
  l:=d[2][3]; o:=OrbitsOfImages(s)!.orbits[d[2][1]][d[2][2]];
  d[2][3]:=o!.scc[d[2][4]][1]; #hack!

#  r:=RClass(s, f); 
#  g:=SchutzenbergerGroup(r);

  return Concatenation(TraceRClassRepsTree(s, HTValue(OrbitsOfImages(s)!.
   data_ht, d[2]{[1..6]})), TraceSchreierTreeOfSCCForward(o, d[2][4], l));
end);

# new for 0.1! - ForwardOrbitOfImage - not a user function!
#############################################################################
# Usage: s = semigroup; f = transformation; 
# images = OrbitsOfImages(s)!.images (optional);
# gens = Generators(s) (optional).

InstallGlobalFunction(ForwardOrbitOfImage, 
function(arg)
  local s, f, images, img, deg, j, bound, treehashsize, o, scc, r, reps, 
   gens, i;

  s:=arg[1]; f:=arg[2];

  if Length(arg)>=3 then 
    images:=arg[3];
  else
    images:=fail;
  fi;

  if Length(arg)=4 then 
    gens:=arg[4];
  else
    gens:=Generators(s);
  fi;

  img:=ImageSetOfTransformation(f);
  deg:=DegreeOfTransformationSemigroup(s);
  j:=Length(img);

  if deg<15 then 
    bound:=Binomial(DegreeOfTransformationSemigroup(s), j);
    treehashsize:=3*bound;
  else
    bound:=infinity;
    treehashsize:=1000;
  fi;
  
  o:=Orb(s, img, OnSets, rec(
          treehashsize:=NextPrimeInt(Minimum(1000, treehashsize)), 
          schreier:=true,
          gradingfunc := function(o,x) return [Length(x), x]; end, 
          orbitgraph := true, 
          onlygrades:=function(x, y) 
            return x[1]=j and (y=fail or HTValue(y, x[2])=fail); end,
          onlygradesdata:=images,
          storenumbers:=true, 
          log:=true));

  SetIsCitrusPkgImgKerOrbit(o, true);
  o!.img:=true; #for ViewObj method
  Enumerate(o, bound);
          
  #strongly connected components
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
   HashTableForKernels(CanonicalTransSameKernel(reps[m]), deg));

  #calculate the multipliers for all scc's 
  o!.perms:=EmptyPlist(Length(o));
  
  for i in [1..r] do 
    o!.perms:=o!.perms+CreateImageOrbitSCCPerms(gens, o, i);
  od;

  #schutzenberger groups
  o!.schutz:=List([1..r], m-> CreateImageOrbitSchutzGp(gens, o, reps[m], m));

  #nr idempotents
  
  o!.nr_idempotents:=List([1..r], m-> []);

  return [o, reps, List([1..r], m-> scc[m][1])];
end);

#GGG

# new for 0.1! - GreensHClasses - "for an R-class of a trans. semigp."
###########################################################################

InstallOtherMethod(GreensHClasses, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
  local s, d, o, m, data, out, f, h, i;

  s:=r!.parent; d:=DClassOfRClass(r); o:=d!.o; 
  m:=NrGreensHClasses(r); data:=GreensHClassRepsData(r); 
  
  out:=EmptyPlist(m); 

  for i in [1..m] do 

    if HasGreensHClassReps(r) then 
      f:=GreensHClassReps(r)[i];
    else
      f:=HClassRepFromData(s, data[i], o);
    fi;

    h:=CreateHClass(s, data[i], o, f);
    SetRClassOfHClass(h, r); SetDClassOfHClass(h, d);
    out[i]:=h;
  od;

  return out;
end);

# new for 0.1! - GreensHClassReps - "for an R-class of a trans. semigp."
#############################################################################
# JDM should we SetGreensLClassReps of d? 

InstallOtherMethod(GreensHClassReps, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local f, cosets, perms, scc, out, k, i, j;

  if HasGreensHClasses(r) then 
    return List(GreensHClasses(r), Representative);
  fi;

  f:= r!.rep; cosets:=ImageOrbitCosets(DClassOfRClass(r));
  perms:=ImageOrbitPerms(r); scc:=ImageOrbitSCC(r);

  out:=EmptyPlist(Length(scc)*Length(cosets));

  if not HasNrGreensHClasses(r) then 
    SetNrGreensHClasses(r, Length(scc)*Length(cosets));
  fi;

  k:=0;

  for i in scc do 
    i:=perms[i];
    for j in cosets do 
      k:=k+1;
      out[k]:=f*(j/i);
    od;
  od;

  return out;
end);

# new for 0.1! - GreensHClassRepsData - "for an R-class of a trans. semigp."
#############################################################################
# JDM1 should we SetGreensLClassReps of d? 

# JDM0 this and other like it should be iterators as illustrated by the 
# Coxeter semigroup example...

InstallOtherMethod(GreensHClassRepsData, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local f, scc, d, cosets, out, k, data, i, j;

  f:= r!.rep; scc:=ImageOrbitSCC(r);
  d:=DClassOfRClass(r); cosets:=ImageOrbitCosets(d);

  out:=EmptyPlist(Length(scc)*Length(cosets));
  
  if not HasNrGreensHClasses(r) then 
    SetNrGreensHClasses(r, Length(scc)*Length(cosets));
  fi;

  k:=0; data:=[r!.data, ShallowCopy(d!.data[2])];
  data[2][3]:=Position(KernelOrbit(d), CanonicalTransSameKernel(f));

  for i in scc do 
    for j in cosets do 
      k:=k+1;
      out[k]:=StructuralCopy(data);
      out[k][1][3]:=i; out[k][3]:=j; out[k][4]:=fail;
    od;
  od;

  return out;
end);

# new for 0.1! - GreensHClassRepsDataFromData - not a user function
#############################################################################
# Usage: s = semigroup; data = image data; o = OrbitsOfImages.

InstallGlobalFunction(GreensHClassRepsDataFromData, 
function(s, data, o)
  local f, scc, d, cosets, out, k, i, j;

  f:=RClassRepFromData(s, data, o); scc:=ImageOrbitSCCFromData(s, data, o);
  d:=GreensDClassOfElementNC(s, f); cosets:=ImageOrbitCosets(d);
  out:=EmptyPlist(Length(scc)*Length(cosets));
  
  k:=0; data:=[ShallowCopy(data), ShallowCopy(d!.data[2])];
  data[2][3]:=Position(KernelOrbit(d), CanonicalTransSameKernel(f));

  for i in scc do 
    for j in cosets do 
      k:=k+1;
      out[k]:=StructuralCopy(data);
      out[k][1][3]:=i; out[k][3]:=j; out[k][4]:=fail;
    od;
  od;

  return out;
end);

# new for 0.1! - GreensRClasses - "for a transformation semigroup"
#############################################################################

InstallMethod(GreensRClasses, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local iter, out, i, r;

  Info(InfoCitrusGreens, 4, "GreensRClasses");

  iter:=IteratorOfGreensRClasses(s);
  out:=EmptyPlist(NrGreensRClasses(s));
  i:=0;

  for r in iter do 
    i:=i+1;
    out[i]:=r;
  od;

  # JDM need to objectify GreensRClasses here if we want to have a method for 
  # Position. How to do this without losing info about out!?

  return out;
end);

# new for 0.1! - GreensRClassOfElement - "for a trans. semigp and trans."
#############################################################################

InstallOtherMethod(GreensRClassOfElement, "for a trans. semigp and trans.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsTransformation],
function(s, f)
  local d;

  Info(InfoCitrusGreens, 4, "GreensRClassOfElement");

  if not f in s then 
    Info(InfoWarning, 1, "transformation is not an element of the semigroup");
    return fail;
  fi;

  d:=PreInOrbitsOfImages(s, f, true)[2];

  return CreateRClass(s, d, OrbitsOfImages(s), RClassRepFromData(s, d));
end);

# new for 0.1! - GreensRClassOfElementNC - "for a trans. semigp and trans."
#############################################################################
# JDM double check everything is ok here!

InstallOtherMethod(GreensRClassOfElementNC, "for a trans. semigp and trans.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsTransformation],
function(s, f)
  local d, j, r, o, n;

  Info(InfoCitrusGreens, 4, "GreensRClassOfElementNC");

  d:=PreInOrbitsOfImages(s, f, true);

  if d[1] then # f in s!
    Info(InfoCitrusGreens, 2, "transformation is an element of semigroup");
    return CreateRClass(s, d[2], OrbitsOfImages(s), RClassRepFromData(s, d[2]));
  elif OrbitsOfImages(s)!.finished then #f not in s!
    Info(InfoCitrusGreens, 2, "transformation is not an element of semigroup");
    return fail;
  fi;

  Info(InfoCitrusGreens, 2, "transformation may not be an element of semigroup");

  j:=Length(ImageSetOfTransformation(f));
  n:=DegreeOfTransformationSemigroup(s);
  o:=[]; o[j]:=[ForwardOrbitOfImage(s, f)[1]]; 
  #JDM ForwardOrbit here calculates the schutz gps., perms and so on 
  #   of all the scc's of the orbit. We only need those for the first one...
  #   add optional fifth arg that filters the scc's.

  #JDM also PreInOrbitsOfImages might have some non-fail values, in which case
  # we should use them. If everything is known except val or n, then we don't
  # really want to do what we are doing here...

  o:=rec( finished:=false, orbits:=o, gens:=Generators(s), s:=s, 
   deg := n, data:=[], images:=fail, lens:=List([1..n], function(x) if x=j then
   return 1; else return 0; fi; end), data_ht:=HTCreate([1,1,1,1,1,1],
   rec(hashlen:=CitrusHashLen!.imgs)));
  #local orbits of images! 
  #JDM shouldn't data contain [j,1,1,1,1,1]??

  r:=CreateRClass(s, [j,1,1,1,1,1], o, f);

  return r;
end);

# new for 0.1! - GreensRClassReps - "for a transformation semigroup"
#############################################################################

InstallMethod(GreensRClassReps, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  Info(InfoCitrusGreens, 4, "GreensRClassReps");

  ExpandOrbitsOfImages(s);

  return List(OrbitsOfImages(s)!.data, x-> RClassRepFromData(s, x));
end);

# new for 0.1! - GreensRClassRepsData - "for a transformation semigroup"
#############################################################################

InstallMethod(GreensRClassRepsData, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  Info(InfoCitrusGreens, 4, "GreensRClassRepsData");
  ExpandOrbitsOfImages(s);
  return OrbitsOfImages(s)!.data;
end);

#III

# new for 0.1! - Idempotents - "for a R-class of a trans. semigp."
#############################################################################
# I don't see the need for iterator and enumerator of idempotents, as there
# are just not that many idempotents in general. Or if there are, then 
# we cannot compute the R-class even.... 

InstallOtherMethod(Idempotents, "for a R-class of a trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local out, f, ker, o, scc, j, i;

  if HasIsRegularRClass(r) and not IsRegularRClass(r) then 
    return [];
  fi;

  if NrIdempotentsRClassFromData(r!.parent, r!.data, r!.o)=0 then
    return [];
  fi;

  if RankOfTransformation(r!.rep)=DegreeOfTransformation(r!.rep) then
    return [TransformationNC([1..DegreeOfTransformation(r!.rep)])];
  fi;

  out:=[]; ker:=CanonicalTransSameKernel(r!.rep);
  o:=ImageOrbit(r); scc:=ImageOrbitSCC(r); j:=0;

  for i in scc do
    i:=o[i];
    if IsInjectiveTransOnList(ker, i) then  
      j:=j+1;
      out[j]:=IdempotentNC(ker, i);
    fi;
  od;

  if not HasNrIdempotents(r) then 
    SetNrIdempotents(r, j);
  fi;

  return out;
end);

# new for 0.1! - ImageOrbit - "for an R-class of a trans. semigp."
############################################################################

InstallMethod(ImageOrbit, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
  local data;

  data:=r!.data;
  return r!.o!.orbits[data[1]][data[2]];
end);

# new for 0.1! - ImageOrbitFromData - not a user function!
############################################################################
# Usage: s = semigroup; d = image data; o = OrbitOfImages(s) (optional). 

InstallGlobalFunction(ImageOrbitFromData,
function(arg)
  local s, d;

  s:=arg[1]; d:=arg[2];

  if Length(arg)=3 then 
    return arg[3]!.orbits[d[1]][d[2]];
  fi;
  
  return OrbitsOfImages(s)!.orbits[d[1]][d[2]];
end);

# new for 0.1! - ImageOrbitPermsFromData - not a user function!
############################################################################
# Usage: s = semigroup; d = image data; o = OrbitsOfImages (optional)

InstallGlobalFunction(ImageOrbitPermsFromData, 
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

# new for 0.1! - ImageOrbitPerms - "for an R-class of a trans. semigp."
############################################################################

InstallMethod(ImageOrbitPerms, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local d;
  d:=r!.data;

  return r!.o!.orbits[d[1]][d[2]]!.perms;
end);

# new for 0.1! - ImageOrbitSCC - "for an R-class of a trans. semigp."
############################################################################

InstallMethod(ImageOrbitSCC, "for an R-class of a trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
  local d;

  d:=r!.data;
  return r!.o!.orbits[d[1]][d[2]]!.scc[d[4]];
end);

# new for 0.1! - ImageOrbitSCC - "for a D-class of a trans. semigp." 
############################################################################
# JDM move to d.gi

InstallOtherMethod(ImageOrbitSCC, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(r)
  local d;

  d:=r!.data[1];
  return r!.o[1]!.orbits[d[1]][d[2]]!.scc[d[4]];
end);

# new for 0.1! - ImageOrbitSCCFromData - not a user function!
############################################################################
# Usage: s = semigroup; d = image data; o = orbits of images (optional)

InstallGlobalFunction(ImageOrbitSCCFromData,
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

# new for 0.1! - ImageOrbitSchutzGp - "for an R-class of a trans. semigp."
############################################################################

InstallMethod(ImageOrbitSchutzGp, "for an R-class of a trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)

Info(InfoWarning, 1, "please use SchutzenbergerGroup instead");
return SchutzenbergerGroup(r);
end);

# new for 0.2! - ImageOrbitSchutzGpGensAsWords - "for R-class of trans. semi
############################################################################

InstallMethod(ImageOrbitSchutzGpGensAsWords, "for R-class of trans. semi",
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local d;
  d:=r!.data;
  return r!.o!.orbits[d[1]][d[2]]!.schutz[d[4]][3];
end);

# new for 0.1! - ImageOrbitSchutzGpFromData - not a user function!
############################################################################
# Usage: s = semigroup; d = image data (any format); 
# o = OrbitsOfImages(s)  (optional).

InstallGlobalFunction(ImageOrbitSchutzGpFromData, 
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

# new for 0.1! - ImageOrbitStabChain - "for an R-class of a trans. semigp."
###########################

InstallMethod(ImageOrbitStabChain, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
  local d;

  d:=r!.data;
  return r!.o!.orbits[d[1]][d[2]]!.schutz[d[4]][1];
end);

# new for 0.1! - ImageOrbitStabChain - "for a D-class of a trans. semigp."
############################################################################
# JDM move to d.gi

InstallOtherMethod(ImageOrbitStabChain, "for a D-class of a trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(r)
  local d;

  d:=r!.data[1];
  return r!.o[1]!.orbits[d[1]][d[2]]!.schutz[d[4]][1];
end);

# new for 0.1! - ImageOrbitStabChainFromData - not a user function!
############################################################################
# Usage: s = semigroup; d = image data (any format); 
# o = OrbitsOfImages(s)  (optional).

InstallGlobalFunction(ImageOrbitStabChainFromData, 
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

# new for 0.1! - InOrbitsOfImages - not a user function!
#############################################################################
# Usage: f = transformation; 
# rectify = should l correspond to f (false) or be o[scc[1]] (true);
# data = image data; o = OrbitsOfImages(s)!.orbits;
# images = OrbitsOfImages(s)!.images (ht of images of elements found so far).

# Notes: previous default was that rectify = false

# JDM should [img, ker] be included as data[8]? 
# JDM perhaps things could be speed up by taking everything with a ! in it
# and making it an argument?!

InstallGlobalFunction(InOrbitsOfImages, 
function(f, rectify, data, o, images)
  local j, k, l, m, val, n, g, img, schutz, reps, i;

  j:=data[1]; k:=data[2]; l:=data[3];
  m:=data[4]; val:=data[5]; n:=data[6]; 
  g:=data[7]; 

  if k=fail then 
    img:=ImageSetOfTransformation(f);
    if j=fail then 
      j:=Length(img);
    fi;
  fi;

  if not IsBound(o[j]) then
    return [false, [j, fail, fail, fail, fail, 0, fail]];
  fi;

  if k=fail then #l=fail, m=fail, g=fail
          
    k:=HTValue(images, img);
          
    if k=fail then 
      return [false, [j, fail, fail, fail, fail, 0, fail]];
    fi;
          
    l:=Position(o[j][k], img);
          
    m:=o[j][k]!.scc_lookup[l];
    g:=f*o[j][k]!.perms[l];
  fi;

  if g=fail then #this can happen if coming from GreensRClassReps.
    g:=f*o[j][k]!.perms[l];
  fi;

  if rectify then 
    l:=o[j][k]!.scc[m][1];
  fi;

  if val=fail then 
    val:=HTValue(o[j][k]!.kernels_ht[m], CanonicalTransSameKernel(f));
    if val=fail then 
      return [false, [j, k, l, m, fail, 0, g]];
    fi;
  fi;

  schutz:=o[j][k]!.schutz[m][1];

  if schutz=true then 
    return [true, [j,k,l,m,val,1,g]];
  fi;

  reps:=o[j][k]!.reps[m][val];
  i:=Length(reps);

  while n<i do 
    n:=n+1;
    if schutz=false then
      if reps[n]=g then # JDM not sure this doesn't slow things down...
        return [true, [j,k,l,m,val,n,g]];
      fi;
    elif SiftedPermutation(schutz, PermLeftQuoTransformationNC(reps[n], g))=() then 
      return [true, [j,k,l,m,val,n,g]];
    fi;
  od;

  return [false, [j,k,l,m,val,n,g]];
end);

# new for 0.1! - IsRegularRClass - "for a Green's class of trans. semigroup"
#############################################################################

InstallMethod(IsRegularRClass, "for a Green's class of trans. semigroup",
[IsGreensClassOfTransSemigp], 
function(r)

  if not IsGreensRClass(r) then 
    return false;
  fi;

  if HasNrIdempotents(r) then 
    return NrIdempotents(r)>0;
  fi;

  if HasIdempotents(r) then 
    return Length(Idempotents(r))>0; 
  fi;

  return IsRegularRClassData(r!.parent, r!.data, r!.o, r!.rep);
end);

# new for 0.1! - IsRegularRClassData - not a user function
#############################################################################
# Usage: s = semigroup; d = image data; o = OrbitsOfImages (optional)
# f = R-class rep (optional)

# JDM this could be improved to use NrIdempotents if known!

InstallGlobalFunction(IsRegularRClassData, 
function(arg)
  local s, d, o, f, scc, i;

  s:=arg[1]; d:=arg[2]; 

  if HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then 
    return true;
  fi;

  if Length(arg)>=3 then 
    o:=arg[3];
  else
    o:=OrbitsOfImages(s);
  fi;

  if Length(arg)=4 then 
    f:=arg[4];
  else
    f:=RClassRepFromData(s, d, o);
  fi;

  f:=f![1];
  scc:=ImageOrbitSCCFromData(s, d, o);
  o:=ImageOrbitFromData(s, d, o);

  for i in scc do
    if IsInjectiveTransOnList(f, o[i]) then
      return true;
    fi;
  od;

  return false;
end);

# new for 0.1! - Iterator - "for a transformation semigroup"
#############################################################################
# JDM move to greens.gi

InstallMethod(Iterator, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local iter;

  Info(InfoCitrusGreens, 4, "Iterator: for a trans. semigroup");

  iter:= IteratorByFunctions( rec( 
    
    R:=IteratorOfGreensRClasses(s), 
    
    r:=fail, s:=s, 
    
    NextIterator:=function(iter)
      
      if IsDoneIterator(iter) then 
        return fail;
      fi;
      
      if iter!.r=fail or IsDoneIterator(iter!.r) then 
        iter!.r:=Iterator(NextIterator(iter!.R));
      fi;
      
      return NextIterator(iter!.r);
    end,
    
    IsDoneIterator:= iter -> IsDoneIterator(iter!.R) and 
     IsDoneIterator(iter!.r),
    
    ShallowCopy:= iter -> rec(R:=IteratorOfGreensRClasses(s), r:=fail)));

  SetIsIteratorOfSemigroup(iter, true);
  SetIsCitrusPkgIterator(iter, true);

  return iter;
end);

# new for 0.1! - Iterator - "for a R-class of a trans. semigroup"
#############################################################################
# JDM it is not really clear that this is needed...

InstallMethod(Iterator, "for a R-class of a trans. semigroup",
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
  local iter;

  Info(InfoCitrusGreens, 4, "Iterator: for an R-class");

  if HasAsSSortedList(r) then 
    iter:=IteratorList(AsSSortedList(r));
  else
    iter:=IteratorByFunctions(rec( 
          
      schutz:=List(SchutzenbergerGroup(r), x-> r!.rep*x),
      #JDM this is bad idea replace as in enumerator...

      m:=Size(SchutzenbergerGroup(r)), s:=r!.parent, 
          
      perms:=ImageOrbitPermsFromData(r!.parent, r!.data, r!.o),
          
      scc:=ImageOrbitSCC(r), 
          
      n:=Length(ImageOrbitSCC(r)),
          
      at:=[1,0],
          
      IsDoneIterator:=iter-> iter!.at[1]=iter!.n and iter!.at[2]=iter!.m,
          
      NextIterator:=function(iter)
          
        if IsDoneIterator(iter) then 
          return fail;
        fi;

        if iter!.at[2]<iter!.m then 
          iter!.at[2]:=iter!.at[2]+1;
        else
          iter!.at[1]:=iter!.at[1]+1; 
          iter!.at[2]:=1;
        fi;
        
        return iter!.schutz[iter!.at[2]]*iter!.perms[iter!.scc[iter!.at[1]]]^-1;
      
      end,
          
      ShallowCopy:=iter-> rec( schutz:=List(SchutzenbergerGroup(r), x-> 
       r!.rep*x),
      m:=Size(SchutzenbergerGroup(r)), 
      perms:=ImageOrbitPermsFromData(r!.parent, r!.data, r!.o), 
      scc:=ImageOrbitSCC(r), n:=Length(ImageOrbitSCC(r)), at:=[1,0])));
  fi;

  SetIsIteratorOfRClassElements(iter, true);
  SetIsCitrusPkgIterator(iter, true); 
  return iter;
end);

# new for 0.1! - IteratorOfGreensRClasses - not a user function!
#############################################################################
# JDM why not use IteratorOfRClassRepsData below, it should be more
# straightforward, see IteratorOfGreensLClasses.

InstallGlobalFunction(IteratorOfGreensRClasses, 
function(s)
  local iter;

  if not IsTransformationSemigroup(s) then 
    Info(InfoWarning, 1, "Usage: arg. should be a transformation semigroup.");
    return fail;
  fi;

  Info(InfoCitrusGreens, 4, "IteratorOfGreensRClasses");

  iter:=IteratorByFunctions( rec(
          
    i:=0, s:=s, reps:=IteratorOfRClassReps(s),
    
    IsDoneIterator := iter -> IsDoneIterator(iter!.reps), 
          
    NextIterator:= function(iter)
      local rep, d;
          
      rep:=NextIterator(iter!.reps);
          
      if rep=fail then 
        return fail;
      fi;
          
      iter!.i:=iter!.i+1;
      d:=OrbitsOfImages(s)!.data[iter!.i];
      return CreateRClass(s, d, OrbitsOfImages(s), rep);
    end,

    ShallowCopy:=iter-> rec(i:=0, s:=iter!.s, reps:=IteratorOfRClassReps(s))));

  SetIsIteratorOfGreensRClasses(iter, true);
  SetIsCitrusPkgIterator(iter, true); 
  return iter;
end);

# new for 0.1! - IteratorOfNewRClassRepsData - not a user function!
#############################################################################

InstallGlobalFunction(IteratorOfNewRClassRepsData, 
function(s)
  local iter, o;

  o:=OrbitsOfImages(s);
  iter:=IteratorOfRClassRepsData(s);
  iter!.i:=Length(o!.data); 
  return iter;
end);

# new for 0.1! - IteratorOfRClassRepsData - not a user function!
#############################################################################

InstallGlobalFunction(IteratorOfRClassRepsData, 
function(s)
  local iter;
  
  Info(InfoCitrusGreens, 4, "IteratorOfRClassRepsData");

  iter:=IteratorByFunctions( rec(
          
  ShallowCopy := iter -> rec( i:=0, s:=iter!.s, 
  next_value:=fail, last_called_by_is_done:=false),

  i:=0, # representative index i.e. which representative we are at

  s:=s,

  next_value := fail,

  last_called_by_is_done:=false,

  ######################################################################

  IsDoneIterator:=function(iter)
    local O, ht, o, i, gens, orbits, images, x, d;
   
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

    ht:=O!.ht; o:=ht!.o; i:=O!.at;

    if i=Length(o) then
    #at the end of the orbit!
      O!.finished:=true;
      Unbind(O!.ht); Unbind(O!.lens); 
      return true;
    fi;

    gens:=O!.gens; orbits:=O!.orbits; images:=O!.images;

    while i<Length(o) do 
      O!.at:=O!.at+1;
      i:=i+1;
      x:=o[i];
      d:=InOrbitsOfImages(x, false, [fail, fail, fail, fail, fail, 0, fail], 
       orbits, images); #JDM should return both scc[1] and l

      if not d[1] then #new rep!
        if IsTransformationMonoid(s) or not i = 1 then 
          Add(O!.rep_to_o, i); 
          Add(O!.mult_ind, d[2]{[1..4]}); 
          if not d[2][3]=fail then #old img
            d[2][3]:=O!.orbits![d[2][1]][d[2][2]]!.scc[d[2][4]][1]; #rectify
          #JDM much much better if InOrbitsOfImages return both the rectified
          #and unrectified positions. 
          fi;

          d:=AddToOrbitsOfImages(s, x, d[2], O);
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
  end));

  ######################################################################

  SetIsIteratorOfRClassRepsData(iter, true);
  SetIsCitrusPkgIterator(iter, true);
  return iter;
end);

# new for 0.1! - IteratorOfRClassReps - not a user function!
#############################################################################

InstallGlobalFunction(IteratorOfRClassReps,
function(s)
  local iter;

  Info(InfoCitrusGreens, 4, "IteratorOfRClassReps");

  if not IsTransformationSemigroup(s) then
    Info(InfoWarning, 1, "Usage: argument should be a transformation",
    " semigroup");
    return fail;
  fi;

  iter:=IteratorByFunctions( rec(

    s:=s, data:=IteratorOfRClassRepsData(s),
                
    IsDoneIterator := iter-> IsDoneIterator(iter!.data),
        
    NextIterator := function(iter)
      if not IsDoneIterator(iter!.data) then 
        return RClassRepFromData(iter!.s, NextIterator(iter!.data));
      fi;
      return fail; 
    end,
            
    ShallowCopy := iter -> rec( data:=IteratorOfRClassRepsData(
    iter!.s))));

  SetIsIteratorOfRClassReps(iter, true);
  SetIsCitrusPkgIterator(iter, true);

  return iter;
end);

#NNN

# new for 0.1! - NrGreensHClasses - "for an R-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(NrGreensHClasses, "for an R-class of a trans. semigroup", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
r-> NrGreensLClasses(DClassOfRClass(r)));

# new for 0.1! - NrGreensRClasses - "for a transformation semigroup"
#############################################################################

InstallMethod(NrGreensRClasses, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)

  Info(InfoCitrusGreens, 4, "NrGreensRClasses");

  ExpandOrbitsOfImages(s);
  return NrRClassesOrbitsOfImages(s);
end);

# new for 0.1! - NrIdempotents - "for an R-class of a trans. semigp."
#############################################################################

InstallOtherMethod(NrIdempotents, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)

  if HasIdempotents(r) then 
    return Length(Idempotents(r));
  fi;

  if HasIsRegularRClass(r) and not IsRegularRClass(r) then 
    return 0;
  fi;

  if Rank(r!.rep)=Degree(r!.parent) then 
    return 1;
  fi;
  
  return NrIdempotentsRClassFromData(r!.parent, r!.data, r!.o);
end);

# new for 0.1! - NrIdempotentsRClassFromData - not a user function!
#############################################################################
# Usage: s = semigroup; d = image data; o = OrbitsOfImages(s/r) (optional)

InstallGlobalFunction(NrIdempotentsRClassFromData, 
function(arg)
  local s, d, O, o, out, rep, scc, i;

  s:=arg[1]; d:=arg[2];

  if Length(arg)=3 then 
    O:=arg[3];
  else
    O:=OrbitsOfImages(s);
  fi;
  
  o:=O!.orbits[d[1]][d[2]];
  
  if IsBound(o!.nr_idempotents[d[4]][d[5]]) then 
    return o!.nr_idempotents[d[4]][d[5]];
  fi;

  if d[1]=DegreeOfTransformationSemigroup(s) then
    o!.nr_idempotents[d[4]][d[5]]:=1;
    return 1;
  fi;

  out:=0; 
  # must have third argument for local R-classes!
  rep:=RClassRepFromData(s, d, O)![1];
  scc:=ImageOrbitSCCFromData(s, d, O);

  for i in scc do
    if IsInjectiveTransOnList(rep, o[i]) then
      out:=out+1;
    fi;
  od;

  o!.nr_idempotents[d[4]][d[5]]:=out;
  return out;
end);

# new for 0.1! - NrRClassesOrbitsOfImages - not a user function! 
#############################################################################

InstallGlobalFunction(NrRClassesOrbitsOfImages,
function(s)
  local c, m, i, j, k, l;

  c:=OrbitsOfImages(s);
  m:=0; c:=c!.orbits;

  for i in c do
    for j in i do 
      for k in j!.reps do 
        for l in k do 
          m:=m+Length(l);
        od;
      od;
    od;
  od;

  if OrbitsOfImages(s)!.finished then 
    SetNrGreensRClasses(s, m);
  fi;
  return m;
end);

#OOO

# new for 0.1! - OrbitsOfImages - "for a transformation semigroup"
#############################################################################

InstallMethod(OrbitsOfImages, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local gens, n, one, ht, i, type;

  gens:=Generators(s);
  n := DegreeOfTransformationSemigroup( s );
  one := TransformationNC( [ 1 .. n ] );

  ht := HTCreate(one, rec(hashlen:=CitrusHashLen!.rclassreps_orb)); 
  HTAdd(ht, one, true); 
  
  for i in [1..Length(gens)] do 
    HTAdd(ht, gens[i], i);
  od;
  
  ht!.o := Concatenation([one], gens); 

  type:=NewType(FamilyObj(s), IsOrbitsOfImages);

  return Objectify(type, rec(
    finished:=false, 
    orbits:=EmptyPlist(n),
    lens:=[1..n]*0, #lens[j]=Length(orbits[j])
    images:=HTCreate(ImageSetOfTransformation(gens[1]),
     rec(hashlen:=CitrusHashLen!.imgs)), 
    at:=0, 
    gens:=gens,
    s:=s,
    deg := n,
    one := one,
    ht:=ht,
    data_ht:=HTCreate([1,1,1,1,1,1], rec(hashlen:=CitrusHashLen!.rclass_data)), 
    data:=[],
    graph:=[], 
    rep_to_o:=[], mult_ind:=[],
    gen:=ListWithIdenticalEntries(Length(gens)+1, fail), 
    pos:=ListWithIdenticalEntries(Length(gens)+1, fail)
  ));
end);

# JDM MN talk to MN re: hashing doesn't work very well when we use smaller hash
# lengths but memory is out of hand when we use longer ones. What to do?

#PPP

# new for 0.1! - ParentAttr - "for a R-class of a trans. semigroup"
############################################################################

InstallMethod(ParentAttr, "for a R-class of a trans. semigroup", 
[IsGreensClass and IsGreensClassOfTransSemigp], x-> x!.parent);

# new for 0.1! - PreInOrbitsOfImages - not a user function!
#############################################################################
# Usage: s = semigroup; f = element; 
# rectify = l for f (false) or for R-class rep (true);
# o = OrbitsOfImages(s)!.orbits (optional);  d = image data (optional).

InstallGlobalFunction(PreInOrbitsOfImages, 
function(arg)
  local s, f, images, data, o;

  s:=arg[1]; f:=arg[2]; 
  images:=OrbitsOfImages(s)!.images; 

  if Length(arg)>=4 then 
    o:=arg[4];
  else
    o:=OrbitsOfImages(s)!.orbits;
  fi;
  
  if Length(arg)>=5 then 
    data:=arg[4];
  else
    data:=[fail, fail, fail, fail, fail, 0, fail];
  fi;

  return InOrbitsOfImages(f, arg[3], data, o, images);
end);

# new for 0.1! - PrintObj - for IsOrbitsOfImages
############################################################################

InstallMethod(PrintObj, [IsOrbitsOfImages], 
function(o)
  Print("<orbits of images; at ", o!.at, " of ", Length(o!.ht!.o), "; ", 
  SizeOrbitsOfImages(o!.s), " elements; ", NrRClassesOrbitsOfImages(o!.s), 
  " R-classes>");
end);

# new for 0.1! - PrintObj - for IsIteratorOfRClassRepsData
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

# new for 0.1! - PrintObj - IsIteratorOfRClassReps
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassReps], 
function(iter)
  local O, s;

  s:=iter!.s; O:=OrbitsOfImages(s);

  Print( "<iterator of R-class reps, ", Length(O!.ht!.o), " candidates, ", 
   SizeOrbitsOfImages(s), " elements, ", NrRClassesOrbitsOfImages(s), 
   " R-classes>");
  return;
end);

# new for 0.1! - PrintObj - for IsIteratorOfGreensRClasses
############################################################################

InstallMethod(PrintObj, [IsIteratorOfGreensRClasses], 
function(iter)
  Print( "<iterator of R-classes>");
  return;
end);

# new for 0.1! - PrintObj - for IsIteratorOfSemigroup
############################################################################

InstallMethod(PrintObj, [IsIteratorOfSemigroup], 
function(iter)
  Print("<iterator of transformation semigroup>");
  return;
end);

# new for 0.1! - PrintObj - for IsIteratorOfRClassElements
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassElements], 
function(iter)
  Print("<iterator of R-class>");
  return;
end);

#RRR

# new for 0.1! - Random - "for an R-class of a trans. semigp."
############################################################################

InstallOtherMethod(Random, "for an R-class of a trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
  local f, g, i;

  f:=r!.rep;
  g:=Random(SchutzenbergerGroup(r));
  i:=Random(ImageOrbitSCC(r));
  
  return f*g*ImageOrbitPerms(r)[i]^-1; 
end);

# new for 0.1! - RClassRepFromData - not a user function!
############################################################################
# Usage: s = semigroup; d = image data; o = OrbitOfImages(s) (optional)

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

# new for 0.1! - RClassRepsDataFromOrbits - not a user function!
############################################################################
# Usage: O = orbits of image; j = image size.

#JDM remove the following later!

InstallGlobalFunction(RClassRepsDataFromOrbits,
function(O, j)
  local i, data, k, m, val, n;

  data:=[]; i:=0;

  for k in [1..Length(O)] do 
    for m in [1..Length(O[k]!.scc)] do 
      for val in [1..Length(O[k]!.reps[m])] do 
        for n in [1..Length(O[k]!.reps[m][val])] do 
          i:=i+1; data[i]:=[j,k,1,m, val,n];
        od;
      od;
    od;
  od;

  return data;
end);

# new for 0.1! - RClassType - "for a transformation semigroup"
############################################################################

InstallMethod(RClassType, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s);

  return NewType( FamilyObj( s ), IsEquivalenceClass and 
	 IsEquivalenceClassDefaultRep and IsGreensRClass and 
	 IsGreensClassOfTransSemigp);
end);

#SSS

# new for 0.1! - SchutzenbergerGroup - "for a R-class of a trans. semigp."
############################################################################

InstallOtherMethod(SchutzenbergerGroup, "for a R-class of a trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local d;
  d:=r!.data;
  return r!.o!.orbits[d[1]][d[2]]!.schutz[d[4]][2];
end);

# new for 0.1! - Size - "for an R-class of a trans. semigp."
#############################################################################
# Algorithm C. 

InstallOtherMethod(Size, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)

  Info(InfoCitrusGreens, 4, "Size: for an R-class");

  return Size(SchutzenbergerGroup(r))*Length(ImageOrbitSCC(r));
end);

# new for 0.1! - Size - "for a transformation semigroup"
#############################################################################
# Algorithm V.

InstallMethod(Size, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)

  Info(InfoCitrusGreens, 4, "Size: for a trans. semigroup");

  ExpandOrbitsOfImages(s);
  return SizeOrbitsOfImages(s);
end);

# new for 0.1! - Size - "for a simple transformation semigroup"
#############################################################################
# JDM check this is actually superior to the above method for Size

InstallOtherMethod(Size, "for a simple transformation semigroup",
[IsSimpleSemigroup and IsTransformationSemigroup],
function(s)
  local gens, ims, kers, H;

  gens:=Generators(s);

  ims:=Size(Set(List(gens, ImageSetOfTransformation)));
  kers:=Size(Set(List(gens, CanonicalTransSameKernel)));
  H:=GreensHClassOfElement(s, gens[1]);

  return Size(H)*ims*kers;
end);

# new for 0.1! - SizeOrbitsOfImages - not a user function!
#############################################################################
# Usage: s = semigroup.

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

#TTT

# new for 0.2! - TraceRClassRepsTree - not a user function!
#############################################################################
# Usage: s = trans. semigroup; i = index of R-class rep 

# Returns: a word in the generators of s equal to GreensRClassReps(s)[i]. 

InstallGlobalFunction(TraceRClassRepsTree, 
function(s, i)
  local gen, pos, t, u, o, word_1, word_2, j;

  o:=OrbitsOfImages(s);
  gen:=o!.gen; pos:=o!.pos; t:=o!.rep_to_o; u:=o!.mult_ind; o:=o!.orbits;

  word_1:=[]; word_2:=[]; j:=i;

  while not gen[t[j]]=fail do
    Add(word_1, gen[t[j]]);
    word_2:= Concatenation(word_2, 
     TraceSchreierTreeOfSCCBack(o[u[j][1]][u[j][2]], u[j][4], u[j][3]));
    j:=pos[t[j]];
  od;
  
  Add(word_1, t[j]-1);
  return Concatenation(word_1, Reversed(word_2));
end);

# new for 0.1! - TraceSchreierTreeOfSCCForward - not a user function!
#############################################################################
# Usage: o = orbit of images; i = index of scc; j = element of scc[i].

# Notes: returns a word in the generators that takes o!.scc[i][1] to o[j] 
# assuming that j in scc[i]

InstallGlobalFunction(TraceSchreierTreeOfSCCForward, 
function(o, i, j)
  local word;

  word := [];
  while j > o!.scc[i][1] do
    Add(word, o!.trees[i][1][j]);
    j := o!.trees[i][2][j];
  od;
  return Reversed(word);
end);

# new for 0.1! - TraceSchreierTreeOfSCCBack - not a user function!
#############################################################################
# Usage: o = orbit of images; i = index of scc; j = element of scc[i].

# Notes: returns a word in the generators that takes o[j] to o!.scc[i][1]  
# assuming that j in scc[i]

InstallGlobalFunction(TraceSchreierTreeOfSCCBack,
function(o, i, j)
  local word;

  if not IsBound(o!.reverse) then 
    o!.reverse:=EmptyPlist(Length(o!.scc));
  fi; 

  if not IsBound(o!.reverse[i]) then 
    o!.reverse[i]:=CreateReverseSchreierTreeOfSCC(o, i);
  fi; 

  word := [];
  while j > o!.scc[i][1] do
    Add(word, o!.reverse[i][1][j]);
    j := o!.reverse[i][2][j];
  od;
  return word;
end);

#VVV

# new for 0.1! - ViewObj - "for a citrus pkg img ker orbit"
#############################################################################

InstallMethod(ViewObj, "for a citrus pkg img ker orbit",
[IsCitrusPkgImgKerOrbit],
function( o )

  Print("<");
  if IsClosed(o) then 
    Print("closed "); 
  else 
    Print("open "); 
  fi;

  Print("orbit, ", Length(o!.orbit));

  if o!.img then 
    Print(" images with size ", Length(o[1])); 
  else 
    Print(" kernels with ", Maximum(o[1]), " classes");
  fi;

  if IsBound(o!.reps) and o!.img then 
    Print(", ");
    Print(Length(o!.scc), " components, ");
    Print(Sum(List(o!.reps, Length)), " kernels, ");
      Print(Sum(Concatenation(List(o!.reps, x-> List(x, Length)))), " reps>");
  elif IsBound(o!.reps) then 
    Print(", ");
    Print(Length(o!.scc), " components, ");
    Print(Sum(List(o!.reps, Length)), " images, ");
    Print(Sum(Concatenation(List(o!.reps, x-> List(x, Length)))), " reps>");
  else
    Print(">");
  fi;
  return;
end );

#EOF
