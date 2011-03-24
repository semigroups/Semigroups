#############################################################################
##
#W  r.gi
#Y  Copyright (C) 2006-2011                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$


#############################################################################
# Conventions:

# - do not use the functions from convenience.gi! (double-check!)

# - use ImageOrbitFromData instead of ImageOrbit!

# - do not use ImageAndKernelOfTransformation!

# - don't use underlyingcollection in enumerators!

# - check that wherever we have called d:=InOrbitsOfImages we do not perform
# f*perms[l] afterwards but instead use d[7]!

# - ensure EmptyPlist is properly used!

#############################################################################
## Notes

# - this file is alphabetized, keep it that way!

# - this file should only contain functions relating to images/r-classes!

#############################################################################
## To do 

# - change the initial line of every function to say new for 4.0! function name,
# and non-user function (where appropriate).

# - make sure that RClassSchutzGp=false is properly implemented. 

# - RClassSCCLookUpFromData and RClassSCCLookUp?

# - RClassSCCTruthTableFromData and RClassSCCTruthTable?

# - make sure that when doing Intersection of two symmetric groups we take 
# SymmetricGroup(Intersection(MovedPoints(a), MovedPoints(b))) and not 
# Intersection(a,b)

# - what about IsGreensLessThanOrEqual?

# - install methods for Generators etc of IsRightSemigroupIdeal, IsLeftSemigroupIdeal
#   In particular, so that IsGreensLessThanOrEqual works.

# - install method for Position(GreensRClasses(s), blah) using InOrbitsOfImages...
# requires the list GreensRClasses to have some property to allow method
# selection. Wait for future version. 

# - install IteratorOfRClassRepsData etc for s and IsPosInt! or more generally
# create and iterator of R-classes satisfying whatever properties we want. 

# new for 4.0! \=, "for R-class and R-class of trans. semigp."
#############################################################################

InstallMethod( \=, "for R-class and R-class of trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp, IsGreensRClass and 
IsGreensClassOfTransSemigp],
function(r1, r2)
  return r1!.parent=r2!.parent and r1!.rep in r2;
end);

# new for 4.0! \=, "for Green's class and Green's class of trans. semigp."
#############################################################################

InstallMethod( \=, "for Green's class and Green's class of trans. semigp.",
[IsGreensClassOfTransSemigp, IsGreensClassOfTransSemigp],
function(x, y)
  return x!.parent=y!.parent and x!.rep in y and Size(x)=Size(y);
end);

# new for 4.0! \<, "for R-class and R-class of trans. semigp."
#############################################################################

InstallMethod( \<, "for R-class and R-class of trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp, IsGreensRClass and 
IsGreensClassOfTransSemigp],
function(r1, r2)
  return r1!.parent=r2!.parent and r1!.rep < r2!.rep;
end);

## new for 4.0! \in, "for trans. and R-class of trans. semigp."
#############################################################################
## Algorithm E. 

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

  g:=f*o!.perms[i];

  if g=rep then
    return true;
  fi;

  schutz:= ImageOrbitStabChain(r);

  if schutz=false then 
    return false;
  fi;

  return schutz=true or SiftedPermutation(schutz, 
   PermLeftQuoTransformationNC(rep, g))=();
end);

# new for 4.0! - \in - "for a transformation semigroup"
#############################################################################
# Notes: not algorithm X.

InstallMethod(\in, "for a transformation semigroup", 
[IsTransformation, IsTransformationSemigroup],
function(f, s)
  local gens, g, o, iter, orbits, images;

  if HasAsSSortedList(s) then 
    return f in AsSSortedList(s);
  fi;

  gens:=Generators(s);

  if not DegreeOfTransformation(f) = DegreeOfTransformation(gens[1]) then 
    return false;
  fi;

  o:=OrbitsOfImages(s);
  g:=PreInOrbitsOfImages(s, f, false);

  if g[1] then 
    return true;
  elif o!.finished then 
    return false;
  fi;

  # check what's already known...
  iter:=IteratorOfNewRClassRepsData(s);
  orbits:=o!.orbits; images:=o!.images;

  repeat
    NextIterator(iter);
    g:=InOrbitsOfImages(f, false, g[2], orbits, images);

    if g[1] then 
      return true;
    fi;
  until IsDoneIterator(iter);

  #JDM could also put something in here that returns false if everything,
  #from OrbitsOfImages(s)!.at to the end of OrbitsOfImages(s)!.ht!.o 
  #has rank less than f. Might be a good idea when degree is very high!

  return false;
end);

# new for 4.0! - AddToOrbitsOfImages - not a user function! 
#############################################################################
# Usage: s = semigroup or d-class; f = transformation; data = image data; 
# o = OrbitsOfImages(s).

# Notes: if s is a d-class, then data should have j, k, l, m and g not = fail!

InstallGlobalFunction(AddToOrbitsOfImages,
function(s, f, data, o)
  local j, k, l, m, val, n, g, O, gens, d, lens, data_ht, one, images, ht, oo, reps, 
   out, i, z, y;

  j:=data[1]; 	# img size
  k:=data[2]; 	# index of orbit containing img
  l:=data[3]; 	# position of img in O[j][k]
  m:=data[4]; 	# scc of O[j][k] containing img
  val:=data[5]; # position of ker in O[j][k]!.kernels_ht[m]
  n:=data[6]; 	# the length of O[j][k]!.reps[m][val]
  g:=data[7];	# f*O[j][k]!.perms[l];

  #JDM not sure it will improve things but maybe everything with a ! here
  #should be an argument to AddToOrbitsOfImages. 
  O := o!.orbits;  gens:=o!.gens; d:=o!.data; lens:=o!.lens;
  data_ht:=o!.data_ht;

  if IsBound(o!.ht) then # o = OrbitsOfImages(s)
    one:=o!.one;
    images:=o!.images; 
    ht:=o!.ht; o:=ht!.o;
  fi;

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
    i:=Length(d);
          
    for m in [1..Length(oo[3])] do  # oo[3] is a list of the first index in
                                    # every scc of oo[1]
      i:=i+1;
      d[i]:=[j, lens[j], oo[3][m], m, 1, 1]; 
      HTAdd(data_ht, d[i], i);
    od;

  ##############################################################################

  else #old img
    reps:=O[j][k]!.reps[m];
      
    if not val=fail then #old kernel
      reps[val][n+1]:=g;
      out:=[j, k, l, m, val, n+1];
      i:=Length(d)+1;
      d[i]:=out;
      HTAdd(data_ht, out, i);
    else #new kernel
      val:=Length(reps)+1;
      reps[val]:=[g];
      out:=[j, k, l, m, val, 1];
      i:=Length(d)+1;
      d[i]:=out;
      HTAdd(data_ht, out, i);
      HTAdd(O[j][k]!.kernels_ht[m], CanonicalTransSameKernel( g ), val);
    fi;
    reps:=[g]; 
  fi;

  ##############################################################################
  #install new pts in the orbit

  if IsBound(ht) then 
    i:=Length(o);
    for f in reps do 
      for y in [1..Length(gens)] do
        z:=gens[y]*f;
        if HTValue(ht, z)=fail then  
          HTAdd(ht, z, true);
          i:=i+1;
          o[i]:=z;
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

InstallOtherMethod(AsList, "for an R-class of trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local f, g, elts, perms, scc, o, p, i;
  
  Info(InfoMonoidGreens, 4, "AsList: for an R-class");

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
# not a user function!

# j is the index of the scc we are computing the multipliers for!

InstallGlobalFunction(CreateImageOrbitSCCPerms,
function(gens, o, j)
  local p, scc, f, i;

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
# not a user function!

# Usage: gens = generators of the semigroup
# o = image orbit
# f = representative of scc with index k
# k = index of scc containing position of image of f in o

InstallGlobalFunction(CreateImageOrbitSchutzGp,
function(gens, o, f, k) 
  local scc, bound, g, p, t, graph, is_sym, i, j;

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

  if is_sym then
    return [true, g];
  elif Size(g)=1 then 
    return [false, g];
  fi;
    
  return [StabChainImmutable(g), g];
end);


# new for 4.0!
#############################################################################
# not a user function!

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
# not a user function!

# o is the orbit, i is the index of the scc!

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

# new for 4.0!
#############################################################################
# not a user function!

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
# not a user function!

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

# new for 4.0!
#############################################################################

InstallOtherMethod(Enumerator, "for R-class of trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local enum, h;

  Info(InfoMonoidGreens, 4, "Enumerator: for an R-class");

  h:=List(Elements(SchutzenbergerGroup(r)), x-> r!.rep*x);
  # maybe store the above line? JDM
  
  enum:=EnumeratorByFunctions(r, rec(
          
          rep:=r!.rep,
          
          h:=h, 
          
          len:=Length(h),
          
          p:=ImageOrbitPerms(r), 
          
          scc:=ImageOrbitSCC(r),
          
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
               CanonicalTransSameKernel(f) <> CanonicalTransSameKernel(rep) then
              return fail;
            fi;
                  
            if f=rep then 
              return 1;
            fi;
                  
            d:=r!.data;
            s:=r!.parent;
              
            # check image is in the same weak orbit
            o:= ImageOrbitFromData(s, d, r!.o);
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
            return elm in r; 
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
# JDM this could be an actual enumerator!?

InstallOtherMethod(Enumerator, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
  local out, iter, j, i;

  Info(InfoMonoidGreens, 4, "Enumerator: for a trans. semigroup");

  out:=EmptyPlist(Size(s)); #it is very slightly faster than taking []

  iter:=Iterator(s);
  j:=0;

  for i in iter do 
    j:=j+1;
    out[j]:=i;
  od;

  return Immutable(out);
end);

# new for 4.0!
#############################################################################
# not a user function!

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

# new for 4.0!
#############################################################################
# not a user function!

# Usage: s = semigroup; f = transformation; images = OrbitsOfImages(s)!.images;
# gens = Generators(s).

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
    treehashsize:=10000;
  fi;
          
  o:=Orb(s, img, OnSets, rec(
          treehashsize:=NextPrimeInt(Minimum(10000, treehashsize)), 
          schreier:=true,
          gradingfunc := function(o,x) return [Length(x), x]; end, 
          orbitgraph := true, 
          onlygrades:=function(x, y) 
            return x[1]=j and (y=fail or HTValue(y, x[2])=fail); end,
          onlygradesdata:=images,
          storenumbers:=true, 
          log:=true));

  SetIsMonoidPkgImgKerOrbit(o, true);
  o!.img:=true; #for ViewObj method
  Enumerate(o, bound);
          
  #strongly connected components
  scc:=Set(List(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(OrbitGraphAsSets(o)), Set));;

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

# new for 4.0!
###########################################################################

InstallOtherMethod(GreensHClasses, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
  local s, d, o, m, D, out, f, h, i;

  s:=r!.parent; d:=DClassOfRClass(r); o:=d!.o; 
  m:=NrGreensHClasses(r); D:=GreensHClassRepsData(r); 
  
  out:=EmptyPlist(m); 

  for i in [1..m] do 

    if HasGreensHClassReps(r) then 
      f:=GreensHClassReps(r)[i];
    else
      f:=HClassRepFromData(s, D[i], o);
    fi;

    h:=CreateHClass(s, D[i], o, f);
    SetRClassOfHClass(h, r); SetDClassOfHClass(h, d);
    out[i]:=h;
  od;

  return out;
end);

# new for 4.0!
#############################################################################
# JDM0 should we SetGreensLClassReps of d? 

InstallOtherMethod(GreensHClassReps, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local f, cosets, perms, scc, out, k, i, j;

  if HasGreensHClasses(r) then 
    return List(GreensHClasses(r), Representative);
  fi;

  f:= r!.rep;
  cosets:=ImageOrbitCosets(DClassOfRClass(r));
  perms:=ImageOrbitPerms(r);
  scc:=ImageOrbitSCC(r);

  out:=EmptyPlist(Length(scc)*Length(cosets));
  SetNrGreensHClasses(r, Length(scc)*Length(cosets));
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

# new for 4.0!
#############################################################################
# JDM1 should we SetGreensLClassReps of d? 

# JDM0 this and other like it should be iterators as illustrated by the 
# Coxeter semigroup example...

InstallOtherMethod(GreensHClassRepsData, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local f, scc, d, m, out, k, data, i, j;

  f:= r!.rep;
  scc:=ImageOrbitSCC(r);
  d:=DClassOfRClass(r);
  m:=Length(ImageOrbitCosets(d));

  out:=EmptyPlist(Length(scc)*m);
  SetNrGreensHClasses(r, Length(scc)*m);

  k:=0;
  data:=[r!.data, d!.data[2]];

  for i in scc do 
    for j in [1..m] do 
      k:=k+1;
      out[k]:=ShallowCopy(data);
      out[k][3]:=[i,j];
    od;
  od;

  return out;
end);

# new for 4.0!
#############################################################################

InstallMethod(GreensRClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
  local iter, out, i, r;

  Info(InfoMonoidGreens, 4, "GreensRClasses");

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

# new for 4.0!
#############################################################################

InstallOtherMethod(GreensRClassOfElement, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
  local d;

  Info(InfoMonoidGreens, 4, "GreensRClassOfElement");

  if not f in s then 
    Info(InfoWarning, 1, "transformation is not an element of the semigroup");
    return fail;
  fi;

  d:=PreInOrbitsOfImages(s, f, true)[2];

  return CreateRClass(s, d, OrbitsOfImages(s), RClassRepFromData(s, d));
end);

# new for 4.0! - GreensRClassOfElementNC - "for a trans. semigp and trans."
#############################################################################
# JDM double check everything is ok here!

InstallOtherMethod(GreensRClassOfElementNC, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
  local d, j, r, o, n;

  Info(InfoMonoidGreens, 4, "GreensRClassOfElementNC");

  d:=PreInOrbitsOfImages(s, f, true);

  if d[1] then # f in s!
    Info(InfoMonoidGreens, 2, "transformation is an element of semigroup");
    return CreateRClass(s, d[2], OrbitsOfImages(s), RClassRepFromData(s, d[2]));
  elif OrbitsOfImages(s)!.finished then #f not in s!
    Info(InfoMonoidGreens, 2, "transformation is not an element of semigroup");
    return fail;
  fi;

  Info(InfoMonoidGreens, 2, "transformation may not be an element of semigroup");

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
   return 1; else return 0; fi; end), data_ht:=HTCreate([1,1,1,1,1,1]));
  #local orbits of images! 
  #JDM shouldn't data contain [j,1,1,1,1,1]??

  r:=CreateRClass(s, [j,1,1,1,1,1], o, f);

  return r;
end);

# new for 4.0!
#############################################################################

InstallMethod(GreensRClassReps, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
  Info(InfoMonoidGreens, 4, "GreensRClassReps");

  ExpandOrbitsOfImages(s);

  return List(OrbitsOfImages(s)!.data, x-> RClassRepFromData(s, x));
end);

# new for 4.0! - GreensRClassRepsData - "for a transformation semigroup"
#############################################################################

InstallMethod(GreensRClassRepsData, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
  Info(InfoMonoidGreens, 4, "GreensRClassRepsData");
  ExpandOrbitsOfImages(s);
  return OrbitsOfImages(s)!.data;
end);

# new for 4.0!
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

  out:=[]; 
  f:=r!.rep;
  ker:=KernelOfTransformation(f);
  f:=f![1];
  o:=ImageOrbitFromData(r!.parent, r!.data, r!.o);
  scc:=ImageOrbitSCC(r); 
  j:=0;

  for i in scc do
    i:=o[i];
    if IsInjectiveTransOnList(f, i) then  
      j:=j+1;
      out[j]:=IdempotentNC(ker, i);
    fi;
  od;

  return out;
end);

# new for 4.0! InOrbitsOfImages - not a user function!
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
      if reps[n]=g then # not sure this doesn't slow things down...
        return [true, [j,k,l,m,val,n,g]];
      fi;
    elif SiftedPermutation(schutz, PermLeftQuoTransformationNC(reps[n], g))=() then 
      return [true, [j,k,l,m,val,n,g]];
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

# new for 4.0!
#############################################################################
# not a user function!

# s, d, o, f

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

# new for 4.0!
#############################################################################

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
          
          ShallowCopy:= iter -> rec(R:=IteratorOfGreensRClasses(s), r:=fail) ));

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
          
      m:=Size(SchutzenbergerGroup(r)),
          
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
          
      ShallowCopy:=iter-> rec( schutz:=List(SchutzenbergerGroup(r), x-> r!.rep*x),
       m:=Size(SchutzenbergerGroup(r)), perms:=ImageOrbitPermsFromData(r!.parent,
       r!.data, r!.o), scc:=ImageOrbitSCC(r), n:=Length(ImageOrbitSCC(r)), at:=[1,0])));
  fi;

  SetIsIteratorOfRClassElements(iter, true);

  return iter;
end);

# new for 4.0!
#############################################################################

InstallGlobalFunction(IteratorOfGreensRClasses, 
function(s)
  local iter;

  if not IsTransformationSemigroup(s) then 
    Info(InfoWarning, 1, "Usage: arg. should be a transformation semigroup.");
    return fail;
  fi;

  Info(InfoMonoidGreens, 4, "IteratorOfGreensRClasses");

  iter:=IteratorByFunctions( rec(
          
    i:=0,
    s:=s, 
    reps:=IteratorOfRClassReps(s),
    
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

# new for 4.0! - IteratorOfRClassRepsData - not a user function!
#############################################################################

InstallGlobalFunction(IteratorOfRClassRepsData, 
function(s)
  local iter;
  
  Info(InfoMonoidGreens, 4, "IteratorOfRClassRepsData");

  iter:=IteratorByFunctions( rec(
          
  ShallowCopy := iter -> rec( i:=0, s:=iter!.s, 
  next_value:=fail, last_called_by_is_done:=false),

  i:=0, # representative index i.e. which representative we are at

  s:= s,

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
    images:=O!.images;

    while i<Length(o) do 
      O!.at:=O!.at+1;
      i :=i+1;
      x:=o[i];
      d:=InOrbitsOfImages(x, true, [fail, fail, fail, fail, fail, 0, fail], 
       orbits, images);

      if not d[1] then #new rep!
        if IsTransformationMonoid(s) or not i = 1 then 
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

  return iter;

end);

# new for 4.0!
#############################################################################

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
    return fail; 
  end,
          
  ShallowCopy := iter -> rec( data:=IteratorOfRClassRepsData(
  iter!.s))));

  SetIsIteratorOfRClassReps(iter, true);
  SetUnderlyingSemigroupOfIterator(iter, s);

  return iter;
end);


# new for 4.0!
#############################################################################

InstallOtherMethod(NrGreensHClasses, "for an R-class of a trans. semigroup", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
r-> NrGreensLClasses(DClassOfRClass(r)));

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

# new for 4.0! NrIdempotentsRClassFromData - not a user function!
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
  # must add third argument for local R-classes!
  rep:=RClassRepFromData(s, d, O)![1];
  scc:=ImageOrbitSCCFromData(s, d, O);

  for i in scc do
    i:=o[i];
    if IsInjectiveTransOnList(rep, i) then
      out:=out+1;
    fi;
  od;

  o!.nr_idempotents[d[4]][d[5]]:=out;
  return out;
end);

# new for 4.0!
#############################################################################
# not a user function!

InstallGlobalFunction(NrRClassesOrbitsOfImages,
function(s)
  local c, m, i, j, k, l;

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
# not a user function!

InstallMethod(OrbitsOfImages, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(s)
  local gens, n, one, ht, i, type;

  gens:=Generators(s);
  n := DegreeOfTransformationSemigroup( s );
  one := TransformationNC( [ 1 .. n ] );

  ht := HTCreate(one);
  HTAdd(ht, one, true);
  
  for i in gens do 
    HTAdd(ht, i, true);
  od;
  
  ht!.o := Concatenation([one], gens); 

  type:=NewType(FamilyObj(s), IsOrbitsOfImages);

  return Objectify(type, rec(
    finished:=false, 
    orbits:=EmptyPlist(n),
    lens:=[1..n]*0, #lens[j]=Length(orbits[j])
    images:=HTCreate(ImageSetOfTransformation(gens[1])),
    at:=0, 
    gens:=gens,
    s:=s,
    deg := n,
    one := one,
    ht:=ht,
    data_ht:=HTCreate([1,1,1,1,1,1]),
    data:=[]
  ));
end);

# new for 4.0!
############################################################################

InstallMethod(ParentAttr, "for a R-class of a trans. semigroup", 
[IsGreensClass and IsGreensClassOfTransSemigp], x-> x!.parent);

# new for 4.0! - PreInOrbitsOfImages - not a user function!
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

# new for 4.0!
#############################################################################

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

# new for 4.0!
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

# new for 4.0!
############################################################################

InstallMethod(ImageOrbit, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
  local data;

  data:=r!.data;
  return r!.o!.orbits[data[1]][data[2]];
end);

# new for 4.0!
############################################################################

InstallGlobalFunction(ImageOrbitFromData,
function(arg)
  local s, d;

  s:=arg[1]; d:=arg[2];

  if Length(arg)=3 then 
    return arg[3]!.orbits[d[1]][d[2]];
  fi;
  
  return OrbitsOfImages(s)!.orbits[d[1]][d[2]];
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

# new for 4.0!
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

# new for 4.0!
############################################################################

InstallMethod(ImageOrbitPerms, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local d;
  d:=r!.data;

  return r!.o!.orbits[d[1]][d[2]]!.perms;
end);

# new for 4.0!
############################################################################

InstallOtherMethod(ImageOrbitPerms, "for a D-class of a trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(r)
  local d;
  d:=r!.data[1];

  return r!.o[1]!.orbits[d[1]][d[2]]!.perms;
end);

# new for 4.0!
############################################################################
# is this function required? JDM

#InstallGlobalFunction(RClassRepsData, 
# s-> OrbitsOfImages(s)!.data);

# new for 4.0!
############################################################################

InstallMethod(ImageOrbitSCC, "for an R-class of a trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
  local d;

  d:=r!.data;
  return r!.o!.orbits[d[1]][d[2]]!.scc[d[4]];
end);

# new for 4.0!
############################################################################

InstallOtherMethod(ImageOrbitSCC, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(r)
  local d;

  d:=r!.data[1];
  return r!.o[1]!.orbits[d[1]][d[2]]!.scc[d[4]];
end);

# new for 4.0!
############################################################################

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

# new for 4.0! ImageOrbitSchutzGp - "for an R-class of a trans. semigp."
############################################################################

InstallMethod(ImageOrbitSchutzGp, "for an R-class of a trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)

Info(InfoWarning, 1, "please use SchutzenbergerGroup instead");
return SchutzenbergerGroup(r);
end);


# new for 4.0! ImageOrbitSchutzGpFromData - not a user function!
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

# new for 4.0!
############################################################################

InstallMethod(ImageOrbitStabChain, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
  local d;

  d:=r!.data;
  return r!.o!.orbits[d[1]][d[2]]!.schutz[d[4]][1];
end);

# new for 4.0!
############################################################################

InstallOtherMethod(ImageOrbitStabChain, "for a D-class of a trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(r)
  local d;

  d:=r!.data[1];
  return r!.o[1]!.orbits[d[1]][d[2]]!.schutz[d[4]][1];
end);

# new for 4.0!
############################################################################

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

# new for 4.0!
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

# new for 4.0!
#############################################################################
##  Algorithm C. 

InstallOtherMethod(Size, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)

  Info(InfoMonoidGreens, 4, "Size: for an R-class");

  return Size(SchutzenbergerGroup(r))*Length(ImageOrbitSCC(r));
end);

# new for 4.0!
#############################################################################
##  Algorithm V.

InstallMethod(Size, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)

  Info(InfoMonoidGreens, 4, "Size: for a trans. semigroup");

  ExpandOrbitsOfImages(s);
  return SizeOrbitsOfImages(s);
end);

#############################################################################
# JDM check this is actually superior to the above method for Size

InstallOtherMethod(Size, "for a simple transformation semigroup",
[IsSimpleSemigroup and IsTransformationSemigroup],
function(s)
  local gens, ims, kers, H;

  gens:=Generators(s);

  ims:=Size(Set(List(gens, ImageSetOfTransformation)));
  kers:=Size(Set(List(gens, KernelOfTransformation)));
  H:=GreensHClassOfElement(s, gens[1]);

  return Size(H)*ims*kers;
end);

# new for 4.0!
#############################################################################
# not a user function!

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
# not a user function!

# returns a word in generators that takes o!.scc[i][1] to o[j] assuming that
# j in scc[i]

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

# new for 4.0!
#############################################################################
# not a user function!

InstallGlobalFunction(TraceSchreierTreeOfSCCBack,
function(o, i, j)
  local word;

  word := [];
  while j > o!.scc[i][1] do
    Add(word, o!.reverse[i][1][j]);
    j := o!.reverse[i][2][j];
  od;
  return word;
end);

#############################################################################

InstallMethod( ViewObj, "for Green's R-class data",
[IsGreensRClassData and IsGreensRClassDataRep],
function( obj )
  Print( "GreensRClassData( ", obj!.rep, ", ", obj!.strongorb,", ", obj!.perms,
  ", ", obj!.schutz, " )" );
end );

# new for 4.0!
#############################################################################

InstallMethod( ViewObj, "for a monoid pkg img ker orbit",
[IsMonoidPkgImgKerOrbit],
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
      if not EvaluateWord(gens, words[i][j][k])=reps[i][j][k] 
       then 			
        return [i,j,k];
      fi;
    od;
  od;
od;

return true;
end;

#############################################################################
