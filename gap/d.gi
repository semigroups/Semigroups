###########################################################################
##
#W  d.gi
#Y  Copyright (C) 2006-2011                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$

#############################################################################
## Notes

# - install a method for GreensHClassReps of a D-class!

# - install iterator method for D-classes? maybe not.

# - use ImageOrbitFromData instead of ImageOrbit (does this make any
# difference?)

# - check that wherever we have called d:=InOrbitsOfImages we do not perform
# f*perms[l] afterwards but instead use d[7]!

#############################################################################
# other equalities of Green's classes handled by generic method in r.gi!

# new for 3.0! - \= - "for D-class and D-class of trans. semigp." 
############################################################################

InstallMethod( \=, "for D-class and D-class of trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp, IsGreensDClass and 
IsGreensClassOfTransSemigp],
function(d1, d2)
  return d1!.parent=d2!.parent and d1!.rep in d2;
end);

# new for 3.0! - \< - "for D-class and D-class of trans. semigp." 
############################################################################

InstallMethod( \<, "for D-class and D-class of trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp, IsGreensDClass and 
IsGreensClassOfTransSemigp],
function(d1, d2)
  return d1!.parent=d2!.parent and d1!.rep < d2!.rep;
end);

# new for 4.0! - \in - "for trans. and D-class of trans. semigp."
#############################################################################

InstallOtherMethod(\in, "for trans. and D-class of trans. semigp.",
[IsTransformation, IsGreensDClass and IsGreensClassOfTransSemigp],
function(f, d)
  local rep, img, o, i, data, ker, schutz, p, cosets, g, sift;

  rep:=d!.rep; 

  if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) 
   or RankOfTransformation(f) <> RankOfTransformation(rep) then
    return false;
  fi;
  
  o:=ImageOrbit(d);
  data:=d!.data;
  
  i:= Position(o, ImageSetOfTransformation(f));

  if i = fail or not o!.truth[data[1][4]][i] then 
    return false;
  fi;

  g:=f*o!.perms[i]; #adjust image of f so that it is equal o[scc[1]]
  
  o:=KernelOrbit(d);
  ker:=CanonicalTransSameKernel(g);

  i:=Position(o, ker);

  if i = fail or not o!.truth[d!.data[2][4]][i] then 
    return false;
  fi;

  schutz:=KernelOrbitStabChain(d); 

  if schutz=true then
    return true;
  fi;

  g:=o!.rels[i][2]*g; #adjust kernel of f so that it is equal o[scc[1]]

  if g=rep then 
    return true;
  fi;

  p:=KerRightToImgLeftFromData(d!.parent, d!.data[2], d!.o[2])^-1;
  cosets:=ImageOrbitCosets(d);
  g:= PermLeftQuoTransformationNC(rep, g);
  sift:= not schutz=false;

  for i in cosets do
    if sift and SiftedPermutation(schutz, (g/i)^p)=() then 
      return true;
    elif g/i=() then 
      return true;  
    fi;
  od;

  return false;
end);

#AAA

# new for 4.0! - AddToOrbitsOfKernels - not a user function!
#############################################################################
# Usage: s = the semigroup; f = transformation (R-class rep with rectified
# image); data = [image data, kernel data] (without true/false, where kernel data
# is the the data corresponding to f with rectified image and kernel and not the 
# original f, so that data[1][3]=o[j][k]!.scc[m][1]!, only 6 components); 
# o = [OrbitsOfImages(s), OrbitsOfKernels(s)] (optional)

# JDM perhaps things could be speed up by taking everything with a ! in it
# and making it an argument?!

# JDM consider allowing data with true/false if this makes things simpler

InstallGlobalFunction(AddToOrbitsOfKernels,
function(arg)
  local s, f, data, o, j, k, l, m, val, n, g, O, gens, imgs_gens, d, 
   lens, kernels, oo, reps, convert, d_schutz, r_reps, i, data_ht;
  
  s:=arg[1]; f:=arg[2]; data:=arg[3]; 

  if Length(arg)=4 then 
    o:=arg[4];
  else
    o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];
  fi;

  data[1]:=data[1]{[1..6]};
  
  j:=data[2][1];    # ker length
  k:=data[2][2];    # index of orbit containing ker
  l:=data[2][3];    # position of ker in O[j][k]
  m:=data[2][4];    # scc of O[j][k] containing ker
  val:=data[2][5];  # position of img in O[j][k]!images_ht[m]
  n:=data[2][6];    # the length of O[j][k]!.reps[m][val]
  g:=data[2][7];    # O[j][k]!.rels[l][2]*f
  #r:=data[2][8]    # the index of the coset rep. 

  O := o[2]!.orbits; gens:=o[1]!.gens; imgs_gens:=o[2]!.imgs_gens; d:=o[2]!.data; 
  lens:=o[2]!.lens; kernels:=o[2]!.kernels; data_ht:=o[2]!.data_ht;

  if k = fail then #new ker and l, m, val, n, g, i=fail

  ################################################################################

    lens[j]:=lens[j]+1;
    oo:=ForwardOrbitOfKernel(s, f, kernels, imgs_gens);
          
    if IsBound(O[j]) then 
      O[j][lens[j]]:=oo;
    else
      O[j]:=[oo];
    fi;
          
    for i in oo do 
      HTAdd(kernels, i, lens[j]);
    od;
    
    data[2]:=[j, lens[j], 1, 1, 1, 1]; #JDM return i here?

    Add(oo!.r_reps[1][1][1], data[1]);
    Add(oo!.d_schutz[1], [CreateSchutzGpOfDClass(s, data, o)]);
    
    i:=Length(d)+1;
    d[i]:=data;
    HTAdd(data_ht, data[2], i);

  ################################################################################
          
  else #old ker
    reps:=O[j][k]!.reps[m]; 
    convert:=O[j][k]!.convert[m];
    d_schutz:=O[j][k]!.d_schutz[m];
    r_reps:=O[j][k]!.r_reps[m];

    if not Length(reps)=0 then #considered this scc before
      if not val=fail then #old image
        reps[val][n+1]:=g;
        data[2]:=[j, k, l, m, val, n+1]; #JDM return i here?
        convert[val][n+1]:=AsPermOfKerImg(f); 
        d_schutz[val][n+1]:=CreateSchutzGpOfDClass(s, data, o);
        r_reps[val][n+1]:=[data[1]];
        i:=Length(d)+1;
        d[i]:=data;
        HTAdd(data_ht, data[2], i);
      else #new image
        val:=Length(reps)+1;
        reps[val]:=[g];
        data[2]:=[j,k,l,m,val,1];
        convert[val]:=[AsPermOfKerImg(g)];
        d_schutz[val]:=[CreateSchutzGpOfDClass(s, data, o)];
        r_reps[val]:=[[data[1]]];
        i:=Length(d)+1;
        d[i]:=data;
        HTAdd(data_ht, data[2], i);
        HTAdd(O[j][k]!.images_ht[m], ImageSetOfTransformation( f ), val);
      fi;
    else #we never considered this scc before!
      
    ##############################################################################
      
      O[j][k]!.trees[m]:=CreateSchreierTreeOfSCC(O[j][k], m);
      O[j][k]!.reverse[m]:=CreateReverseSchreierTreeOfSCC(O[j][k], m);
      r_reps[1]:=[[data[1]]];
      O[j][k]!.images_ht[m]:=HashTableForImages(f![1]);
      O[j][k]!.rels:=O[j][k]!.rels+CreateKernelOrbitSCCRels(gens, O[j][k], m);
      g:=O[j][k]!.rels[l][2]*f;
      reps[1]:=[g];
      convert[1]:=[AsPermOfKerImg(g)];
      O[j][k]!.schutz[m]:=CreateKernelOrbitSchutzGp(gens, O[j][k], g, m);
      l:=O[j][k]!.scc[m][1]; #rectify the kernel!
      data[2]:=[j, k, l, m, 1, 1];
      d_schutz[1]:=[CreateSchutzGpOfDClass(s, data, o)];
      i:=Length(d)+1;
      HTAdd(data_ht, data[2], i);
      d[i]:=data;
    fi;
  fi;

  return data;
end);

# new for 4.0! - AsSSortedList - "for a D-class of trans. semigp."
#############################################################################
# this should be removed after the library method for AsSSortedList 
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)!

InstallOtherMethod(AsSSortedList, "for a D-class of trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  Info(InfoMonoidGreens, 4, "AsSSortedList: for a D-class");
  return ConstantTimeAccessList(EnumeratorSorted(d));
end);

#CCC

# new for 4.0! - CreateDClass - not a user function!
#############################################################################
# Usage: s = semigroup; data = [image data, kernel data] (any lengths);
# orbit = [OrbitsOfImages, OrbitsOfKernels] or local variants;
# rep = representative.

# Notes: data[1][3] & data[2][3] should be rectified, rep should have rectified
# image and kernel.

InstallGlobalFunction(CreateDClass, 
function(s, data, orbit, rep)
  local d;

  data:=[data[1]{[1..6]}, data[2]{[1..6]}];

  d:=Objectify(DClassType(s), rec(parent:=s, data:=data, 
   o:=orbit, rep:=rep));
  
  SetRepresentative(d, rep);
  SetEquivalenceClassRelation(d, GreensDRelation(s));
  return d;
end);

# new for 4.0! - CreateKernelOrbitSCCRels - not a user function
#############################################################################
# Usage: gens = generators of a semigroup or monoid;
# o = a kernel orbit; j = the index of a strongly connect component of o.

InstallGlobalFunction(CreateKernelOrbitSCCRels,
function(gens, o, j)
  local rels, scc, m, n, f, g, h, lookup, i, k, l, t;

  rels:=EmptyPlist(Length(o));
  scc:=o!.scc[j]; m:=Length(Set(o[1])); n:=Length(o[1]);

  for i in scc do
    #reversed used below as we have a left action not right as in R-classes!
    f:=EvaluateWord(gens, Reversed(TraceSchreierTreeOfSCCForward(o, j, i)));
    # OnKernelAntiAction(o[scc[1]], f)=o[i]
    # g:=EvaluateWord(gens, Reversed(TraceSchreierTreeOfSCCBack(o, j, i)));
    # OnKernelsAntiAction(o[i], g)=o[scc[1]] 
 
    # JDM we could remove the create of the reverse schreier trees above
    
    h:=o[scc[1]]{f![1]}; lookup:=EmptyPlist(n); k:=0;
    for l in [1..n] do 
      if not IsBound(lookup[h[l]]) then 
        lookup[h[l]]:=l;
        k:=k+1;
      fi;
      if k=m then 
        break;
      fi;
    od;

    rels[i]:=[f, TransformationNC(List([1..n], x-> lookup[o[scc[1]][x]]))];
  od;

  return rels;
end);

# new for 4.0! - CreateKernelOrbitSchutzGp - not a user function!
#############################################################################
# Usage: gens = generators of semigroup; o = kernel orbit; 
# f = transformation; k = index of the scc of o containing the image of f

# Notes: creates the Schutzenberger group of the kernel orbit 
# (which is KerRight)

InstallGlobalFunction(CreateKernelOrbitSchutzGp,
function(gens, o, f, k) 
  local scc, max, bound, g, rels, t, graph, is_sym, i, j;

  scc:=o!.scc[k];
  max:=MaximumList(o[scc[1]]);

  if max<1000 then 
    bound:=Factorial(max);
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
  
  g:=g^(f![4]^-1);# convert to KerRight!

  if is_sym then 
    return [true, g];
  elif Size(g)=1 then 
    return [false, g];
  fi;
  
  return [StabChainImmutable(g), g];
end);

# new for 4.0! - CreateSchutzGpOfDClass - not a user function!
#############################################################################
# Usage: s = semigroup; d = [image data, kernel data]; 
# o = [OrbitsOfImages(s), OrbitsOfKernels(s)] (optional).  

# Notes: returns a stabilizer chain for the schutz gp (ImgLeft) which is the
# intersection of the schutz gps of the defining L-class and R-class, the group
# itself, and a transversal of cosets of the schutz gp in the schutz gp of the
# R-class. 

InstallGlobalFunction(CreateSchutzGpOfDClass, 
function(arg)
  local s, d, o, g, h, p, pts;

  s:=arg[1]; d:=arg[2];

  if Length(arg)=3 then 
    o:=arg[3];
  else
    o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];
  fi;

  g:=ImageOrbitSchutzGpFromData(s, d[1], o[1]);
  
  if Size(g)=1 then 
    h:=g;
  else 
    p:=KerRightToImgLeftFromData(s, d[2], o[2]);
    if KernelOrbitStabChainFromData(s, d[2], o[2])=true and
     ImageOrbitStabChainFromData(s, d[1], o[1])=true then 
      pts:=OnSets(MovedPoints(KernelOrbitSchutzGpFromData(s, d[2], o[2])), p);
      h:=SymmetricGroup(Intersection(pts, MovedPoints(g)));
      # store KernelOrbitSchutzGpFromData(s, d[2],o[2])^p?
    else
      h:=Intersection(g, KernelOrbitSchutzGpFromData(s, d[2], o[2])^p);
    fi;
  fi;

  return [StabChainImmutable(h), h, RightTransversal(g, h)];
end);

#DDD

# new for 4.0! - DClassRClassRepsDataFromData - not a user function!
############################################################################
# Usage: s = semigroup; d = [image data, kernel data]; o = [OrbitsOfImages(s),
# OrbitsOfKernels(s)] (optional)

# Notes: returns the R-class reps data of the D-class with the given data if it
# is bound, this is the R-class reps data found during the iteration of all 
# D-classes.

InstallGlobalFunction(DClassRClassRepsDataFromData,
function(arg)
  local s, d, o;

  s:=arg[1]; d:=arg[2][2];

  if Length(arg)=3 then 
    o:=arg[3][2]!.orbits[d[1]][d[2]];
  else
    o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
  fi;

  if IsBound(o!.r_reps[d[4]][d[5]][d[6]]) then 
    return o!.r_reps[d[4]][d[5]][d[6]];
  fi;
  return fail;
end);

# new for 4.0! - DClassRepFromData - not a user function!
#############################################################################
# Usage: s = semigroup; d = [image data, kernel data];
# o = [OrbitsOfImages, OrbitsOfKernels].

# Notes: returns a trans. with kernel and img in the first positions of their
# scc's. 

InstallGlobalFunction(DClassRepFromData, 
function(arg)
  local s, d, o;

  s:=arg[1]; d:=arg[2][2];

  if Length(arg)=3 then 
    o:=arg[3][2]!.orbits[d[1]][d[2]];
  else
    o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
  fi;

  return o!.reps[d[4]][d[5]][d[6]];
end);

# new for 4.0! - DClassSchutzGpFromData - not a user function!
#############################################################################
# Usage: s - semigroup; d = kernel data; o = orbits of kernels (optional).

# Notes: returns the schutz. gp. of the D-class (which is ImgLeft) and is a 
# subgroup of ImageOrbitSchutzGpFromData(s, d[1], o[1]) (which is ImgLeft). 
# This is the intersection of ImageOrbitSchutzGp and
# KernelOrbitSchutzGp^KerRightToImgLeft.

InstallGlobalFunction(DClassSchutzGpFromData, 
function(arg)
local s, d, o;

  s:=arg[1]; d:=arg[2];

  if Length(arg)=3 then 
    o:=arg[3]!.orbits[d[1]][d[2]];
  else
    o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
  fi;

  return o!.d_schutz[d[4]][d[5]][d[6]][2];
end);

# new for 4.0! - DClassOfRClass - "for an R-class of a trans. semigroup"
#############################################################################
# Notes: this does not check if the representative of r is an element of s.

# JDM double check that ker_o doesn't require more components...

InstallOtherMethod(DClassOfRClass, "for an R-class of a trans. semigroup", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local s, f, d, data, j, img_o, ker_o;

  s:=r!.parent; f:=r!.rep;
  d:=PreInOrbitsOfKernels(s, f, true);

  if d[1][1] then # f in s! not d[2][1] if not d[1][1]?  
    Info(InfoMonoidGreens, 2, "transformation is an element of the semigroup");
    return GreensDClassOfElement(s, f);
  elif OrbitsOfImages(s)!.finished then #f not in s!
    Info(InfoMonoidGreens, 2, "transformation is not an element of the ",
    "semigroup");
    return fail;
  fi;

  #JDM see the comments in GreensRClassOfElementNC

  Info(InfoMonoidGreens, 2, "transformation may not be an element of the ",
   "semigroup");

  j:=Length(ImageSetOfTransformation(f));
  img_o:=r!.o;

  Info(InfoMonoidGreens, 2, "finding orbit of kernel...");
  ker_o:=[]; ker_o[j]:=[ForwardOrbitOfKernel(s, f)];
  ker_o:=rec(gens:=Generators(s), orbits:=ker_o, data:=[]);
  #JDM should ker_o!.data:=[[j,1,1,1,1,1],[j,1,1,1,1,1]]?

  Info(InfoMonoidGreens, 2, "finding the kernel orbit Schutz. gp. ...");
  Add(ker_o!.orbits[j][1]!.d_schutz[1], 
   [CreateSchutzGpOfDClass(s, [r!.data, [j,1,1,1,1,1]], [img_o, ker_o])]);
  
  return CreateDClass(s, [r!.data, [j,1,1,1,1,1]], [img_o, ker_o], f);
end);

# new for 4.0! - DClassType - "for a transformation semigroup"
############################################################################# 

InstallMethod(DClassType, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s);
  return NewType( FamilyObj( s ), IsEquivalenceClass and 
	  IsEquivalenceClassDefaultRep and IsGreensDClass and 
	  IsGreensClassOfTransSemigp);
end);

# new for 4.0! - DisplayOrbitsOfKernels - not a user function!
#############################################################################

InstallGlobalFunction(DisplayOrbitsOfKernels,
function(s)
  local o, k, i, j;

  o:=OrbitsOfKernels(s);

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

  Print("size: \t\t", SizeOrbitsOfKernels(s), "\n");
  Print("D-classes: \t", Length(o!.data), "\n");
  #Print("data ht: "); View(o!.data_ht); Print("\n");
  Print("kernels: \t"); View(o!.kernels); Print("\n");
  return true;
end);

#EEE

# new for 4.0! - Enumerator - "for a D-class of trans. semigp."
#############################################################################

InstallOtherMethod(Enumerator, "for a D-class of trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  local enum;

  Info(InfoMonoidGreens, 4, "Enumerator: for a D-class");

  enum:=EnumeratorByFunctions(d, rec(
          
    m:=Length(ImageOrbitSCCFromData(d!.parent, d!.data[1], d!.o[1]))*
    Size(ImageOrbitSchutzGpFromData(d!.parent, d!.data[1], d!.o[1])),
    # size of any R-class in d.

    #######################################################################
      
    ElementNumber:=function(enum, pos)
    local q, n, m, R;
      
      if pos>Length(enum) then 
        return fail;
      fi;
      
      R:=GreensRClasses(d); #JDM better if EnumeratorOfRClasses
      n:=pos-1;
      m:=enum!.m;
      
      q := QuoInt(n, m);
      pos:= [ q, n - q * m ]+1;

      return Enumerator(R[pos[1]])[pos[2]];
    end, 
    
    #######################################################################
    
    NumberElement:=function(enum, f)
      local R, i, j;
        
      R:=GreensRClasses(d); #JDM better if EnumeratorOfRClasses
      for i in [1..Length(R)] do 
        j:=Position(Enumerator(R[i]), f);
        if not j=fail then 
          return enum!.m*(i-1)+j;
        fi;
      od;
      return fail;
    end, 

    #######################################################################
    
    Membership:=function(elm, enum) 
      return elm in d; #the D-class itself!
    end,
    
    Length:=enum -> Size(d),
    
    PrintObj:=function(enum)
      Print( "<enumerator of D-class>");
    return;
  end));

  return enum;
end);

# new for 4.0! - ExpandOrbitsOfKernels - not a user function!
#############################################################################

InstallGlobalFunction(ExpandOrbitsOfKernels, 
function(s)
  local o, iter, i;

  Info(InfoMonoidGreens, 4, "ExpandOrbitsOfKernels");
  
  o:=OrbitsOfKernels(s);
  
  if not o!.finished then 
    iter:=IteratorOfNewDClassReps(s);
    for i in iter do od;
  fi;
  return true;
end);

#FFF

# new for 4.0! - ForwardOrbitOfKernel - not a user function!
#############################################################################
# Usage: s = semigroup; f = transformation; 
# kernels = OrbitsOfKernels(s)!.kernels (optional); 
# gens = GeneratorsAsListOfImages(s) (optional).

# JDM maybe this should take the image data as input also! and then d_schutz should 
# in this function

InstallGlobalFunction(ForwardOrbitOfKernel, 
function(arg)
  local s, f, kernels, gens, ker, deg, j, bound, treehashsize, o, scc, r, i,
  gens_imgs;
  
  s:=arg[1]; f:=arg[2];

  if Length(arg)>=3 then 
    kernels:=arg[3];
  else
    kernels:=fail;
  fi;

  if Length(arg)=4 then 
    gens_imgs:=arg[4];
  else
    gens_imgs:=GeneratorsAsListOfImages(s);
  fi;

  gens:=Generators(s);

  ker:=CanonicalTransSameKernel(f![1]);
  deg:=DegreeOfTransformationSemigroup(s);
  j:=MaximumList(ker); # rank!
          
  if deg<11 then 
    bound:=Stirling2(DegreeOfTransformationSemigroup(s), j);
    treehashsize:=3*bound;
  else
    bound:=infinity;
    treehashsize:=10000;
  fi;
          
  o:=Orb(gens_imgs, ker, function(f,g) return CanonicalTransSameKernel(f{g}); end,
         rec( treehashsize:=NextPrimeInt(Minimum(10000, treehashsize)),  
          schreier:=true,
          gradingfunc := function(o,x) return [MaximumList(x), x]; end, 
          orbitgraph := true,
          onlygrades:=function(x, y)
           return x[1]=j and (y=fail or HTValue(y, x[2])=fail); end,
          onlygrades:=kernels, 
          storenumbers:=true,
          log:=true));

  SetIsMonoidPkgImgKerOrbit(o, true);
  o!.img:=false; #for ViewObj method
  Enumerate(o, bound);

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
  o!.trees:=EmptyPlist(r);
  o!.reverse:=EmptyPlist(r);
  o!.trees[1]:=CreateSchreierTreeOfSCC(o,1); 
  o!.reverse[1]:=CreateReverseSchreierTreeOfSCC(o,1);

  #representatives of D-classes with kernel belonging in scc[i] partitioned 
  #according to their kernels
  o!.reps:=List([1..r], x-> []);
  Add(o!.reps[1], [f]);

  #R-class reps corresponding to D-class with rep in o!.reps
  o!.r_reps:=List([1..r], x-> []);
  Add(o!.r_reps[1], [[]]); 

  #JDM modify the following if you decide to get rid of KerRight!
  o!.convert:=List([1..r], x-> []);
  Add(o!.convert[1], [AsPermOfKerImg(f)]);

  #images of representatives of D-classes with kernel belonging in scc[i]
  o!.images_ht:=[HashTableForImages(f![1])];

  #multipliers of scc containing the kernel of f
  o!.rels:=EmptyPlist(Length(o));
  o!.rels:=o!.rels+CreateKernelOrbitSCCRels(gens, o, 1);
          
  #schutzenberger group of kernel orbit KerRight
  o!.schutz:=EmptyPlist(r);
  o!.schutz[1]:=CreateKernelOrbitSchutzGp(gens, o, f, 1);

  #intersection of image orbit and kernel orbit schutz. gps ImgLeft
  o!.d_schutz:=List([1..r], x-> []);
  return o;
end);

#GGG

# new for 4.0! - GeneratorsAsListOfImages - "for a trans. semigp."
#############################################################################

InstallMethod(GeneratorsAsListOfImages, "for a trans. semigp.", 
[IsTransformationSemigroup],
s-> List(Generators(s), f-> f![1]));

# new for 4.0! - GreensDClasses - "for a transformation semigroup"
#############################################################################

InstallMethod(GreensDClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
  local iter, out, i, d;

  iter:=IteratorOfGreensDClasses(s);
  out:=EmptyPlist(NrGreensDClasses(s));
  i:=0;

  for d in iter do
    i:=i+1;
    out[i]:=d;
  od;

  return out;
end);

# new for 4.0! - GreensDClassOfElement - "for a trans. semigp and trans."
#############################################################################

InstallOtherMethod(GreensDClassOfElement, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
  local d;

  Info(InfoMonoidGreens, 4, "GreensDClassOfElement");

  if not f in s then 
    Info(InfoWarning, 1, "transformation is not an element of the semigroup");
    return fail;
  fi;

  d:=PreInOrbitsOfKernels(s, f, true);

  if not d[2][1] then #orbit of kernel not previously calculated!
    d:=AddToOrbitsOfKernels(s, d[1][2][7], [d[1][2],d[2][2]]); 
    #d[1][2][7] is f with rectified image!
  else
    d:=[d[1][2],d[2][2]];
  fi;

  d:=CreateDClass(s, d, [OrbitsOfImages(s), OrbitsOfKernels(s)], 
   DClassRepFromData(s, d));

  return d;
end);

# new for 4.0! - GreensDClassOfElementNC - "for a trans. semigp and trans."
############################################################################
# Notes: this should be similar to DClassOfRClass. 

InstallOtherMethod(GreensDClassOfElementNC, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
  local d, j, n, img_o, ker_o;

  Info(InfoMonoidGreens, 4, "GreensDClassOfElementNC");

  n:=DegreeOfTransformationSemigroup(s);

  if not DegreeOfTransformation(f)=n then 
    Info(InfoWarning, 1, "Usage: trans. semigroup and trans. of equal degree");
    return fail;
  fi;

  d:=PreInOrbitsOfKernels(s, f, true);

  if d[1][1] then #f in s!
    #JDM inefficient as we run PreInOrbitsOfKernels twice!
    return GreensDClassOfElement(s, f);
  elif OrbitsOfImages(s)!.finished then #f not in s!
    Info(InfoMonoidGreens, 2, "transformation is not an element of the ",
    "semigroup");
    return fail;
  fi;

  Info(InfoMonoidGreens, 2, "transformation may not be an element of the ",
  "semigroup");

  j:=Length(ImageSetOfTransformation(f));

  Info(InfoMonoidGreens, 2, "finding image orbit...");
  img_o:=[]; img_o[j]:=[ForwardOrbitOfImage(s, f)[1]];
  #JDM see comments in GreensRClassOfElementNC...
  img_o:=rec( finished:=false, orbits:=img_o, gens:=Generators(s), s:=s, 
   deg := n, data:=[[j,1,1,1,1,1]], images:=fail, lens:=List([1..n], 
   function(x) if x=j then return 1; else return 0; fi; end), 
   data_ht:=HTCreate([1,1,1,1,1,1]));
  #JDM images should not be fail in this...
  
  Info(InfoMonoidGreens, 2, "finding kernel orbit...");
  ker_o:=[]; ker_o[j]:=[ForwardOrbitOfKernel(s, f)];
  ker_o:=rec( orbits:=ker_o, gens:=Generators(s), data:=[[j,1,1,1,1,1],
   [j,1,1,1,1,1]], kernels:=fail);
  #JDM is it nec. to specify ker_o!.data? 

  Info(InfoMonoidGreens, 2, "finding the kernel orbit schutz. gp. ...");
  Add(ker_o!.orbits[j][1]!.d_schutz[1], [CreateSchutzGpOfDClass(s,
   [[j,1,1,1,1,1], [j,1,1,1,1,1]], [img_o, ker_o])]);

  return CreateDClass(s, [[j,1,1,1,1,1], [j,1,1,1,1,1]], [img_o, ker_o], f);
end);

# new for 4.0! - GreensDClassReps - "for a trans. semigroup"
#############################################################################
# move to greens.gi JDM
InstallMethod(GreensDClassReps, "for a trans. semigroup", 
[IsTransformationSemigroup], 
  function(s)

  ExpandOrbitsOfKernels(s);
  return List(OrbitsOfKernels(s)!.data, x-> DClassRepFromData(s, x));
end);

# new for 4.0! - GreensDClassRepsData - "for a trans. semigroup"
#############################################################################
# move to greens.gi JDM 
 
InstallMethod(GreensDClassRepsData, "for a trans. semigroup", 
[IsTransformationSemigroup], 
  function(s)
  ExpandOrbitsOfKernels(s);
  return OrbitsOfKernels(s)!.data;
end);

# new for 4.0! - GreensHClasses - "for a D-class of a trans. semigroup"
#############################################################################
# JDM could this be better/more efficient?! Could avoid using GreensRClasses. 

InstallOtherMethod(GreensHClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  return Concatenation(List(GreensRClasses(d), GreensHClasses));
end);

# new for 4.0! - GreensLClassRepsData - "for a D-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(GreensLClassRepsData, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
  d-> GreensLClassRepsDataFromData(d!.parent, d!.data, d!.o));

# new for 4.0! - GreensLClassRepsDataFromData - not a user function!
#############################################################################
# Usage: s = semigroup; data = [image data, kernel data];
# o = [OrbitsOfKernels, OrbitsOfImages].

InstallGlobalFunction(GreensLClassRepsDataFromData,
function(s, data, o)
  local scc, img_co, out, k, i, j;
  
  scc:=ImageOrbitSCCFromData(s, data[1], o[1]);
  img_co:=ImageOrbitCosetsFromData(s, data[2], o[2]);
  out:=EmptyPlist(Length(scc)*Length(img_co)); k:=0; 
  
  for i in scc do
    for j in img_co do
      k:=k+1;
      out[k]:=StructuralCopy(data);
      out[k][1][3]:=i; out[k][3]:=j;
    od;
  od;
  return out;
end);

# new for 4.0! - GreensLClassReps - "for a D-class of a trans. semigroup"
#############################################################################
# maybe write iterator/enumerator later! JDM

InstallOtherMethod(GreensLClassReps, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  local perms, scc, cosets, f, out, k, i, j;

  perms:=ImageOrbitPerms(d); scc:=ImageOrbitSCC(d); 
  cosets:=ImageOrbitCosets(d); f:=d!.rep;

  out:=EmptyPlist(Length(scc)*Length(cosets));
  SetNrGreensLClasses(d, Length(scc)*Length(cosets));
  
  k:=0;

  for i in scc do 
    for j in cosets do 
      k:=k+1;
      out[k]:=f*(j/perms[i]);
    od;
  od;

  return out;
end);

# new for 4.0! - GreensRClassRepsData - "for a D-class of a trans. semigroup"
#############################################################################
# Notes: maybe write iterator/enumerator later! This is relatively slow in 
# comparison to GreensRClassReps, as here we have to search for the data.

InstallOtherMethod(GreensRClassRepsData, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  local out, f, rels, scc, cosets, j, k, l, m, val, orbits, images, t, r, c, 
   g, data, i, x;

  out:=DClassRClassRepsDataFromData(d!.parent, d!.data, d!.o);
  # read the R-classes of d found during the enumeration of D-classes.

  if Length(out)=NrGreensRClasses(d) then 
    return out;
  fi;

  f:=Representative(d); rels:=KernelOrbitRels(d);
  scc:=KernelOrbitSCC(d); cosets:=KernelOrbitCosets(d);

  j:=d!.data[1][1]; k:=d!.data[1][2]; l:=d!.data[1][3]; m:=d!.data[1][4];

  scc:=List(scc, i-> rels[i][1]*f);
  val:=List(scc, g-> HTValue(ImageOrbit(d)!.kernels_ht[m], 
   CanonicalTransSameKernel(g)));

  l:=List(cosets, x-> Position(ImageOrbit(d),
   ImageSetOfTransformation(f*x^-1)));

  out:=EmptyPlist(NrGreensRClasses(d));
  orbits:=d!.o[1]!.orbits; images:=d!.o[1]!.images;
  t:=0; r:=Length(scc); c:=Length(cosets);

  for i in [1..r] do 
    for x in [1..c] do 
      g:=scc[i]*cosets[x]^-1;
      data:=InOrbitsOfImages(g, true, [j, k, l[x], m, val[i], 0, fail], 
       orbits, images);
      #could do SiftedPermutation directly here, maybe speed things up?
      if not data[1] then 
	data:=AddToOrbitsOfImages(d, g, data[2], d!.o[1]);
      else 
        data:=data[2];
      fi;
      t:=t+1;
      out[t]:=data;
    od;
  od;
  return out;
end);

# new for 4.0! - GreensRClassReps - "for a D-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(GreensRClassReps, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  local rels, scc, cosets, f, out, k, g, i, j;

  if HasGreensRClassRepsData(d) then 
    return List(GreensRClassRepsData(d), x-> 
     RClassRepFromData(d!.parent, x, d!.o[1]));
  fi;

  rels:=KernelOrbitRels(d); scc:=KernelOrbitSCC(d);
  cosets:=KernelOrbitCosets(d); f:=d!.rep;

  out:=EmptyPlist(Length(scc)*Length(cosets));
  SetNrGreensRClasses(d, Length(scc)*Length(cosets));

  k:=0;
  
  for i in scc do
    g:=rels[i][1]*f;
    for j in cosets do
      k:=k+1;
      out[k]:=g*j^-1;
    od;
  od;

  return out;
end);

# new for 4.0! - GreensLClasses - "for a D-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(GreensLClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  local s, o, m, out, i, f, l, data;

  s:=d!.parent; o:=d!.o; m:=NrGreensLClasses(d); out:=EmptyPlist(m); 

  for i in [1..m] do 
    data:=GreensLClassRepsData(d)[i]; 
    if HasGreensLClassReps(d) then 
      f:=GreensLClassReps(d)[i];
    else
      f:=LClassRepFromData(s, data, o);
    fi;
    
    l:=CreateLClass(s, data, o, f);
    SetDClassOfLClass(l, d);
    out[i]:=l;
  od;

  return out;
end);

# new for 4.0! - GreensRClasses - "for a D-class of a trans. semigroup"
#############################################################################
# Notes: it would be helpful to have a IteratorOfGreensRClasses for use in 
# the  enumerator of a D-class. This is relatively slow in comparison to 
# GreensRClassReps, as finding GreensRClassRepsData involves a search. 
# This could be improved by using GreensRClassReps to give the reps of 
# R-classes and then just setting the attributes of the R-class by finding 
# the value from the d!.data[1]. Alternatively, R-classes of a D-class could be
# found in the same way that L-classes of a D-class are found.

InstallOtherMethod(GreensRClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  local s, o, out, i, reps, f, r, data;

  s:=d!.parent; o:=d!.o[1]; 
  out:=EmptyPlist(NrGreensRClasses(d)); 
  i:=0; reps:=GreensRClassRepsData(d);

  for data in reps do 
    f:=RClassRepFromData(s, data, o);
    r:=CreateRClass(s, data, o, f);
    SetDClassOfRClass(r, d);
    i:=i+1;
    out[i]:=r;
  od;

  return out;
end);

# new for 4.0! - GreensRClassOfElement - "for D-class of trans. semigroup"
#############################################################################
# Notes: maybe think this through a bit more...

InstallOtherMethod(GreensRClassOfElement, "for D-class of trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp, IsTransformation], 
function(d, f)
  local o, e, data, g, r;
   
  if not f in d then 
    Info(InfoWarning, 1, "transformation is not an element of the D-class");
    return fail;
  fi;
  
  o:=d!.o[1]; e:=d!.data[1];
  data:=InOrbitsOfImages(f, true, [e[1], e[2], Position(o!.orbits[e[1]][e[2]],
   ImageSetOfTransformation(f)), e[4], fail, 0, fail], o!.orbits, o!.images);
  
  # the position call in the previous line is a waste as this is already found
  # when `f in d' is called. 

  if not data[1] then 
    data:=AddToOrbitsOfImages(d, f, data[2], o);
  else 
    data:=data[2];
  fi;

  g:=RClassRepFromData(d!.parent, data, o);
  r:=CreateRClass(d!.parent, data, o, g);
  SetDClassOfRClass(r, d);
  return r;
end);

#III

# new for 4.0! - Idempotents - "for a D-class of a trans. semigp."
#############################################################################
# JDM compare what is here with the method for Idempotents of an R-class!

InstallOtherMethod(Idempotents, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  local ker_o, ker_scc, n, out, k, img_o, img_scc, m, i, j, l, lookup;

  if HasIsRegularDClass(d) and not IsRegularDClass(d) then 
    return [];
  fi;

  if NrIdempotentsRClassFromData(d!.parent, d!.data[1], d!.o[1])=0 then 
    return [];
  fi;

  if RankOfTransformation(d!.rep)=DegreeOfTransformation(d!.rep) then
    return [TransformationNC([1..DegreeOfTransformation(d!.rep)])];
  fi;

  ker_o:=KernelOrbit(d); ker_scc:=KernelOrbitSCC(d); 
  img_o:=ImageOrbit(d); img_scc:=ImageOrbitSCC(d);
  
  n:=Length(d!.rep![1]); #degree
  m:=Length(img_o[1]); #rank
  
  if HasNrIdempotents(d) then 
    out:=EmptyPlist(NrIdempotents(d));
  else
    out:=[]; 
  fi;
  k:=0;

  for i in img_scc do 
    for j in ker_scc do 
      if IsInjectiveTransOnList(ker_o[j], img_o[i]) then 
        k:=k+1;
        #do any better? 
        lookup:=EmptyPlist(n);
        for l in [1..m] do 
          lookup[ker_o[j][img_o[i][l]]]:=img_o[i][l];
        od;
        
        out[k]:=TransformationNC(List(ker_o[j], x-> lookup[x])); 
      fi;
    od;
  od;

  if not HasNrIdempotents(d) then 
    SetNrIdempotents(d, k);
  fi;

  return out;
end);

# new for 4.0! - ImageOrbit - "for a D-class of a trans. semigp"
#############################################################################
# equivalently ImageOrbitFromData(d!.parent, d!.data[1], d!.o[1]);

InstallOtherMethod(ImageOrbit, "for a D-class of a trans. semigp", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  local e;
  e:=d!.data[1];
  return d!.o[1]!.orbits[e[1]][e[2]];
end);

# new for 4.0! - ImageOrbitCosets - "for a D-class of trans. semigroup"
###########################################################################
# Notes: returns a transversal of right cosets of SchutzenbergerGroup(d)
# (which is ImgLeft) in ImageOrbitSchutzGp (which is ImgLeft). 

InstallMethod(ImageOrbitCosets, "for a D-class of trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  local D;
  D:=d!.data[2];
  return d!.o[2]!.orbits[D[1]][D[2]]!.d_schutz[D[4]][D[5]][D[6]][3];
end);

# new for 4.0! - ImageOrbitCosetsFromData - not a user function!
###########################################################################
# Usage: s - semigroup; d - kernel orbit data; o - orbits of kernels 
# (optional!)

# Notes: returns a transversal of right cosets of SchutzenbergerGroup(d)
# (which is ImgLeft) in ImageOrbitSchutzGp (which is ImgLeft). Unlike other
# ImageOrbit... functions this requires kernel data!

InstallGlobalFunction(ImageOrbitCosetsFromData,
function(arg)
  local s, d, o;
  s:=arg[1]; d:=arg[2];
  
  if Length(arg)=3 then 
    o:=arg[3];
  else
    o:=OrbitsOfKernels(s);
  fi;

  return o!.orbits[d[1]][d[2]]!.d_schutz[d[4]][d[5]][d[6]][3];
end);

# new for 4.0! - ImageOrbitPerms - for a D-class of a trans. semigp.
############################################################################

InstallOtherMethod(ImageOrbitPerms, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(r)
  local d;  
  d:=r!.data[1];
  return r!.o[1]!.orbits[d[1]][d[2]]!.perms;
end);

# new for 4.0! - ImageOrbitSchutzGp - not a user function!
############################################################################
# Notes: returns the schutz. gp. of the image orbit of the D-class, which is
# ImgLeft. Equivalent, to ImageOrbitSchutzGpFromData(d!.parent, d!.data[1],
# d!.o[1]).

InstallOtherMethod(ImageOrbitSchutzGp, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  local e;
  e:=d!.data[1];
  return d!.o[1]!.orbits[e[1]][e[2]]!.schutz[e[4]][2];
end);

# new for 4.0! - InOrbitsOfKernels - not a user function
#############################################################################
# Usage: f = transformation; 
# rectify = should l correspond to f (false) or be o[scc[1]] (true);
# data = [image data, kernel data] (including true/false), 
# o = [OrbitsOfImages(s)!.orbits, OrbitsOfKernels(s)!.orbits],
# kernels = OrbitsOfKernels(s)!.kernels (ht of kernels found so far)

# Notes:  f is converted to be an R-class representative at the start here. 
# In particular, data[1][2] corresponds to the R-class rep, data[2][2]
# corresponds to the R-class rep with rectified kernel only if g
# has already been found. Otherwise data[2][2] corresponds to the R-class rep
# and not the R-class rep with rectified kernel. 

# Also note that this does not work for D-classes!

# JDM: should [img, ker] be included as data[2][2][9]?

# JDM: would this be speed up if all !. were arguments? Also s is not 
# used anywhere in the function. 

InstallGlobalFunction(InOrbitsOfKernels, 
function(f, rectify, data, o, kernels)
  local j, k, l, m, val, n, g, r, ker, schutz, reps, i, p, cosets, t, h, sift;

  j:=data[2][2][1]; k:=data[2][2][2]; l:=data[2][2][3]; m:=data[2][2][4]; 
  val:=data[2][2][5]; n:=data[2][2][6]; g:=data[2][2][7]; r:=data[2][2][8];
  o:=o[2]; 
  # r is not used here, even if it is known a priori...


  f:=data[1][2][7];

  if k=fail then 
    ker:=CanonicalTransSameKernel(f);
    if j=fail then 
      j:=MaximumList(ker);
    fi;
  fi;
  
  data:=data[1];

  if not IsBound(o[j]) then
    return [data, [false, [j, fail, fail, fail, fail, 0, fail, fail]]];
  fi;

  if k=fail then #l=fail, m=fail, g=fail
    
    k:=HTValue(kernels, ker);

    if k=fail then 
      return [data, [false, [j, fail, fail, fail, fail, 0, fail, fail]]];
    fi;

    l:=Position(o[j][k], ker);
    m:=o[j][k]!.scc_lookup[l];
          
    if not IsBound(o[j][k]!.rels[l]) then
      return [data, [false, [j, k, l, m, fail, 0, fail, fail]]];
    fi;
  
    g:=o[j][k]!.rels[l][2]*f;
  fi;

  if g=fail then 
    g:=o[j][k]!.rels[l][2]*f;
  fi;

  if rectify then
    l:=o[j][k]!.scc[m][1];
  fi;

  if val=fail then 
    val:=HTValue(o[j][k]!.images_ht[m], ImageSetOfTransformation(f));
    if val=fail then 
      return [data, [false, [j, k, l, m, fail, 0, g, fail]]];
    fi;
  fi;
  
  schutz:=o[j][k]!.schutz[m][1];
  
  if schutz=true then 
    return [data, [true, [j, k, l, m, val, 1, g, 1]]]; 
  fi;

  reps:=o[j][k]!.reps[m][val];
  i:=Length(reps);
  sift:=not schutz=false;

  while n<i do
    n:=n+1;
    p:=o[j][k]!.convert[m][val][n]^-1;
    cosets:=o[j][k]!.d_schutz[m][val][n][3]; #ImageOrbitCosets
    t:=Length(cosets);
    h:=PermLeftQuoTransformationNC(reps[n], g);
      
    for r in [1..t] do 
      if sift and SiftedPermutation(schutz, (h/cosets[r])^p)=() then 
        return [data, [true, [j, k, l, m, val, n, g, r]]];
      elif h/cosets[r]=() then
        return [data, [true, [j, k, l, m, val, n, g, r]]];
      fi;
    od;
  od;

  return [data, [false, [j, k, l, m, val, n, g, r]]];
end);

# new for 4.0! - IsRegularDClass - "for a D-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(IsRegularDClass, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)

  if HasNrIdempotents(d) then 
    return NrIdempotents(d)>0;
  elif HasIdempotents(d) then 
    return not Idempotents(d)=[];
  fi;

  return IsRegularRClassData(d!.parent, d!.data[1], d!.o[1], d!.rep);
end);

# new for 4.0! - IteratorOfDClassReps - user function!
#############################################################################
# Usage: s = transformation semigroup.

InstallGlobalFunction(IteratorOfDClassReps, 
function(s)
  local iter;

  Info(InfoMonoidGreens, 4, "IteratorOfDClassReps");
  
  if not IsTransformationSemigroup(s) then 
    Info(InfoWarning, 1, "Usage: argument should be a transformation", 
    " semigroup");
    return fail;
  fi;

  iter:=IteratorByFunctions(rec(
    
    s:=s, data:=IteratorOfDClassRepsData(s), 

    IsDoneIterator := iter -> IsDoneIterator(iter!.data),

    NextIterator := function(iter) 
      if not IsDoneIterator(iter!.data) then 
        return DClassRepFromData(iter!.s, NextIterator(iter!.data));
      fi;
      return fail;
    end,

    ShallowCopy:= iter -> rec( s:=s, data:=IteratorOfDClassRepsData(s))));

  SetIsIteratorOfDClassReps(iter, true);
  SetUnderlyingSemigroupOfIterator(iter, s);
  
  return iter;
end);

# new for 4.0! - IteratorOfDClassRepsData - not a user function!
#############################################################################
# Usage: s = transformation semigroup.

InstallGlobalFunction(IteratorOfDClassRepsData, 
function(s)
  local iter;
  
  Info(InfoMonoidGreens, 4, "IteratorOfDClassRepsData");
  
  iter:=IteratorByFunctions( rec(

    ShallowCopy := iter -> rec(),

    i:=0, # representative index i.e. which representative we are at

    s:= s,

    next_value:=fail,

    last_called_by_is_done:=false,

    r:=IteratorOfRClassRepsData(s),

    ######################################################################

    IsDoneIterator:=function(iter) 
      local O, o, r, ker, f, d, r_reps, e;

      if iter!.last_called_by_is_done then 
        return iter!.next_value=fail;
      fi;

      iter!.last_called_by_is_done:=true;

      O:=OrbitsOfKernels(s);

      iter!.next_value:=fail;

      if iter!.i < Length(O!.data) then 
        iter!.i:=iter!.i+1;
        iter!.next_value:=O!.data[iter!.i];
        return false;
      elif O!.finished then  
        return true;
      fi;
      
      o:=[OrbitsOfImages(s)!.orbits, OrbitsOfKernels(s)!.orbits];
      r:=iter!.r; ker:=OrbitsOfKernels(s)!.kernels;

      for d in r do  
        f:=RClassRepFromData(s, d);
        d:=InOrbitsOfKernels(f, true, [[true, Concatenation(d, [f])], 
         [false, [d[1], fail, fail, fail, fail, 0, fail, fail]]], o,  ker);
        if not d[2][1] then #f not in existing D-class
          d:=AddToOrbitsOfKernels(s, f, [d[1][2], d[2][2]]);
          iter!.i:=iter!.i+1;
          iter!.next_value:=d;
          return false;
        else #store R-class in kernel orbit
          e:=d[2][2];
          r_reps:=o[2][e[1]][e[2]]!.r_reps[e[4]][e[5]][e[6]];
          r_reps[Length(r_reps)+1]:=d[1][2];
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

  SetIsIteratorOfDClassRepsData(iter, true);

  return iter;
end);

# new for 4.0! - IteratorOfGreensDClasses - user function!
#############################################################################
# JDM why not use IteratorOfDClassRepsData below, it should be more
# straightforward, see IteratorOfGreensLClasses.

InstallGlobalFunction(IteratorOfGreensDClasses, 
function(arg)
  local iter, s;

  Info(InfoMonoidGreens, 4, "IteratorOfGreensDClasses");

  if not (Length(arg) mod 3)=1 or not IsTransformationSemigroup(arg[1]) then 
    Info(InfoWarning, 1, "Usage: argument should be a transformation semigroup");
    #" and optionally function, operator, value, function, operator, value, ...");
    return fail;
  fi;

  #JDM do the same as the below for IteratorOfRClasses etc..

  if Length(arg)>1 then 
    return IteratorByFunctions( rec(
            
            arg:=arg,
            
            iter:=IteratorOfGreensDClasses(arg[1]),
           
            next:=fail,
            
            last_called:=false,

            IsDoneIterator :=function(iter)
            local d;        
              
              if iter!.last_called then 
                return iter!.next=fail;
              fi;

              iter!.last_called:=true;

              repeat 
                d:=NextIterator(iter!.iter);
              until d=fail or ForAll([1..(Length(arg)-1)/3], i-> 
               arg[3*i](arg[3*i-1](d), arg[3*i+1]));
              
              iter!.next:=d;

              return d=fail;
            end,

            NextIterator:=function(iter)
            
              if not iter!.last_called then 
                IsDoneIterator(iter);
              fi;
              iter!.last_called:=false;
              return iter!.next;
            end,
    
            ShallowCopy:=iter -> rec(iter:=iter!.iter, arg:=iter!.arg)));
  fi;

  #########################################################################

  s:=arg[1];

  iter:=IteratorByFunctions( rec(
    
    i:=0, s:=s, reps:=IteratorOfDClassReps(s),
    
    IsDoneIterator := iter -> IsDoneIterator(iter!.reps), 
    
    NextIterator:= function(iter)
    local c, rep, d;
    
    rep:=NextIterator(iter!.reps);
    
    if rep=fail then 
            return fail;
    fi;
    
    iter!.i:=iter!.i+1;
    d:=OrbitsOfKernels(iter!.s)!.data[iter!.i];

    return CreateDClass(s, d, [OrbitsOfImages(s), 
     OrbitsOfKernels(s)], rep);
    end,
    
    ShallowCopy:=iter-> rec(i:=0, s:=iter!.s, 
     reps:=IteratorOfRClassReps(s))));

  SetIsIteratorOfGreensDClasses(iter, true);
  SetUnderlyingSemigroupOfIterator(iter, s);
  return iter;
end);

# new for 4.0! - IteratorOfNewDClassReps - not a user function!
###########################################################################

InstallGlobalFunction(IteratorOfNewDClassReps, 
function(s)
  local o, iter;

  o:=OrbitsOfKernels(s);
  iter:=IteratorOfDClassReps(s);
  iter!.i:=Length(o!.data); 
  return iter;
end);

#KKK

# new for 4.0! - KernelOrbit - "for a D-class of a trans. semigp." 
############################################################################

InstallMethod(KernelOrbit, "for a D-class of a trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  local e;
  e:=d!.data[2];
  return d!.o[2]!.orbits[e[1]][e[2]];
end);

# new for 4.0! - KernelOrbitFromData - not a user function!
############################################################################
# Usage: s - semigroup; d - D-class data; o - orbits of images and kernels

InstallGlobalFunction(KernelOrbitFromData,
function(arg)
  local s, d;

  s:=arg[1]; d:=arg[2][2];

  if Length(arg)=3 then
    return arg[3][2]!.orbits[d[1]][d[2]];
  fi;

  return OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
end);

# new for 4.0! - KernelOrbitCosets - not a user function!
###########################################################################
# Notes: returns a transversal of right cosets of SchutzenbergerGroup(d)
# (which is ImgLeft) in KernelOrbitSchutzGp^KerRightToImgLeft(d) (which is
# ImgLeft after conjugating). 

InstallMethod(KernelOrbitCosets, "for a D-class of trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  local schutz;

  schutz:=KernelOrbitSchutzGpFromData(d!.parent, d!.data[2], d!.o[2]); 

  if Size(schutz)=1 then 
   return [()];
  fi;

  return RightTransversal(schutz^KerRightToImgLeft(d), 
   SchutzenbergerGroup(d));
end);

# new for 4.0! - KernelOrbitCosetsFromData - not a user function!
###########################################################################
# Notes: returns a transversal of right cosets of SchutzenbergerGroup(d)
# (which is ImgLeft) in KernelOrbitSchutzGp^KerRightToImgLeft(d) (which is
# ImgLeft after conjugating). 

InstallGlobalFunction(KernelOrbitCosetsFromData, 
function(arg)
  local s, d, o, schutz, cosets;
 
  s:=arg[1]; d:=arg[2];
  
  if Length(arg)=3 then 
    o:=arg[3];
  else
    o:=OrbitsOfKernels(s);
  fi;

  if IsBound(o!.orbits[d[1]][d[2]]!.d_schutz[d[4]][d[5]][d[6]][4]) then 
    return o!.orbits[d[1]][d[2]]!.d_schutz[d[4]][d[5]][d[6]][4];
  fi;
  
  schutz:=KernelOrbitSchutzGpFromData(s, d, o);
  
  if Size(schutz)=1 then
    cosets:=[()];
  else
    cosets:=RightTransversal(schutz^KerRightToImgLeftFromData(s, d, o),
       DClassSchutzGpFromData(s, d, o));
  fi;

  o!.orbits[d[1]][d[2]]!.d_schutz[d[4]][d[5]][d[6]][4]:=cosets;
  return cosets;
end);

# new for 4.0! - KernelOrbitRels - "for a D-class of a trans. semigp"
############################################################################

InstallMethod(KernelOrbitRels,"for a D-class of a trans. semigp",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  local e;
  e:=d!.data[2]; 
  return d!.o[2]!.orbits[e[1]][e[2]]!.rels;
end);

# new for 4.0! - KernelOrbitRelsFromData - not a user function!
############################################################################
# Usage: s = semigroup; d = kernel data; o = OrbitsOfKernels (optional)

InstallGlobalFunction(KernelOrbitRelsFromData,
function(arg)
  local s, d, o;
  s:=arg[1]; d:=arg[2];
  
  if Length(arg)=3 then
    o:=arg[3]!.orbits[d[1]][d[2]];
  else  
    o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
  fi;

  return o!.rels;
end);

# new for 4.0! - KernelOrbitSCC - "for D-class of trans. semigp"
#############################################################################

InstallMethod(KernelOrbitSCC, "for D-class of trans. semigp", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  local e;
  
  e:=d!.data[2];
  return d!.o[2]!.orbits[e[1]][e[2]]!.scc[e[4]];
end);

# new for 4.0! - KernelOrbitSCCFromData - not a user function!
############################################################################
# Usage: s = semigroup; d = kernel data; o = OrbitsOfKernels(s) (optional)

InstallGlobalFunction(KernelOrbitSCCFromData, 
function(arg)
  local s, d, o;

  s:=arg[1]; d:=arg[2];

  if Length(arg)=3 then
    o:=arg[3]!.orbits[d[1]][d[2]];
  else
    o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
  fi;

  return o!.scc[d[4]];
end);

# new for 4.0! - KernelOrbitSchutzGp - "for a D-class of a trans. semigp."
############################################################################
# Notes: returns the schutz. gp. of the kernel orbit of the D-class, which is
# KerRight.

InstallMethod(KernelOrbitSchutzGp, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  local e;
  e:=d!.data[2];
  return d!.o[2]!.orbits[e[1]][e[2]]!.schutz[e[4]][2];
end);

# new for 4.0! - KernelOrbitSchutzGpFromData - not a user function!
############################################################################
# Usage: s = semigroup; d = kernel data; o = OrbitsOfKernels(s) (optional)

# Notes: returns the right Schutzenberger group (acting on kernels on the right) 
# of the kernel orbit with the given data. Use KerRightToImgLeft to switch from 
# the right Schutz. gp. to the left one corresponding to a specific D-class. 

InstallGlobalFunction(KernelOrbitSchutzGpFromData,
function(arg)
  local s, d, o;

  s:=arg[1]; d:=arg[2];

  if Length(arg)=3 then
    o:=arg[3]!.orbits[d[1]][d[2]];
  else
    o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
  fi;

  return o!.schutz[d[4]][2];
end);

# new for 4.0! - KernelOrbitStabChain - not a user function!
############################################################################
# Notes: returns true, false, or the stabilizer chain of the right Schutzenberger 
# group of the specified kernel orbit. True indicates the Schutz. gp.
# is the symmetric group, false indicates it is trivial. Note that the right 
# Schutzenberger group is obtained by considering the right action on kernels
# classes. Use KerRightToImgLeft to switch from the right Schutz. gp. to the
# left one corresponding a specific D-class. 

InstallMethod(KernelOrbitStabChain, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  local e;
  e:=d!.data[2];
  return d!.o[2]!.orbits[e[1]][e[2]]!.schutz[e[4]][1];
end);

# new for 4.0! - KernelOrbitStabChainFromData - not a user function!
############################################################################

# Usage: s - semigroup; d - kernel orbit data (i.e. second comp. of D-class
# data); o - orbits of kernels (optional).

# Notes: returns true, false, or the stabilizer chain of the right Schutzenberger 
# group of the specified kernel orbit. True indicates the Schutz. gp.
# is the symmetric group, false indicates it is trivial. Note that the right 
# Schutzenberger group is obtained by considering the right action on kernels
# classes. Use KerRightToImgLeft to switch from the right Schutz. gp. to the
# left one corresponding a specific D-class. 

InstallGlobalFunction(KernelOrbitStabChainFromData,
function(arg)
  local s, d, o;

  s:=arg[1]; d:=arg[2];

  if Length(arg)=3 then
    o:=arg[3]!.orbits[d[1]][d[2]];
  else
    o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
  fi;

  return o!.schutz[d[4]][1];
end);

# new for 4.0! - KerRightToImgLeftFromData - not a user function.
###########################################################################
# Usage: s = semigroup; d = kernel data; o = orbits of kernels (optional) 

# Notes: permutation converting a perm. of ker. classes to one of img elts

InstallGlobalFunction(KerRightToImgLeftFromData,
function(arg)
  local s, d, o;

  s:=arg[1]; d:=arg[2];

  if Length(arg)=3 then 
    o:=arg[3]!.orbits[d[1]][d[2]];
  else
    o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
  fi;

  return o!.convert[d[4]][d[5]][d[6]];
end);

# new for 4.0! - KerRightToImgLeft - "for a D-class of a trans. semigp"
#############################################################################

InstallMethod(KerRightToImgLeft, "for a D-class of a trans. semigp",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
d-> KerRightToImgLeftFromData(d!.parent, d!.data[2], d!.o[2]));

#NNN

# new for 4.0! - NrGreensDClasses - "for a transformation semigroup"
#############################################################################

InstallMethod(NrGreensDClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
  Info(InfoMonoidGreens, 4, "NrGreensDClasses");
  ExpandOrbitsOfKernels(s);
  return Length(OrbitsOfKernels(s)!.data);
end);

# new for 4.0! - NrGreensHClasses - "for a D-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(NrGreensHClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  return NrGreensRClasses(d)*NrGreensLClasses(d);
end);

# new for 4.0! - NrGreensLClasses - "for a D-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(NrGreensLClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  return Length(ImageOrbitCosets(d))*Length(ImageOrbitSCCFromData(d!.parent, d!.
  data[1], d!.o[1]));
end);

# new for 4.0! - NrGreensRClasses - "for a D-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(NrGreensRClasses, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)

  if HasGreensRClassReps(d) then 
    return Length(GreensRClassReps(d));
  fi;

  return Length(KernelOrbitSCC(d))*Length(KernelOrbitCosets(d));
end);

# new for 4.0! - NrIdempotents - "for a D-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(NrIdempotents, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  local ker_o, ker_scc, img_o, img_scc, k, i, j;
 
  if HasIdempotents(d) then 
    return Length(Idempotents(d));
  fi;

  if HasIsRegularDClass(d) and not IsRegularDClass(d) then 
    return 0;
  fi;
  
  if NrIdempotentsRClassFromData(d!.parent, d!.data[1], d!.o[1])=0 then
    return 0;
  fi;

  if RankOfTransformation(d!.rep)=DegreeOfTransformation(d!.rep) then 
    return 1;
  fi;

  k:=0;

  if HasGreensRClassRepsData(d) then 
    for i in GreensRClassRepsData(d) do 
      k:=k+NrIdempotentsRClassFromData(d!.parent, i, d!.o[1]);
    od;
  else

    ker_o:=KernelOrbit(d); ker_scc:=KernelOrbitSCC(d);
    img_o:=ImageOrbit(d); img_scc:=ImageOrbitSCC(d);
    k:=0;

    for i in img_scc do
      for j in ker_scc do
        if IsInjectiveTransOnList(ker_o[j], img_o[i]) then
          k:=k+1;
        fi;
      od;
    od;
  fi;  

  return k;
end);

# new for 4.0! - NrRegularDClasses - "for a transformation semigroup"
#############################################################################

InstallMethod(NrRegularDClasses, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
  local o;
  ExpandOrbitsOfKernels(s);
  o:=OrbitsOfImages(s);

  return Number(OrbitsOfKernels(s)!.data, x-> IsRegularRClassData(s, x[1], o));
end);

#OOO

# new for 4.0! - OrbitsOfKernels - "for a transformation semigroup"
#############################################################################

InstallMethod(OrbitsOfKernels, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(s)
  local n, gens;

  n:=DegreeOfTransformationSemigroup(s);
  gens:=Generators(s);

  return Objectify(NewType(FamilyObj(s), IsOrbitsOfKernels), rec(
    finished:=false,
    orbits:=EmptyPlist(n), 
    lens:=[1..n]*0, #lens[j]=Length(orbits[j])
    kernels:=HTCreate([1..n]),
    imgs_gens:=List(gens, x-> x![1]),
    gens:=gens,
    s:=s,
    data_ht:=HTCreate([1,1,1,1,1,1,1]),
    data:=[]));
end);

#PPP

# new for 4.0! - ParentAttr - "for a D-class of a trans. semigroup"
#############################################################################

InstallMethod(ParentAttr, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], x-> x!.parent);

# new for 4.0! - PartialOrderOfDClasses - "for a transformation semigroup"
#############################################################################

InstallMethod(PartialOrderOfDClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
  local d, n, out, gens, data_ht, o, j, data, i, x, f, orbits;

  d:=GreensDClasses(s); n:=NrGreensDClasses(s); 
  out:= List([1..n], x-> EmptyPlist(n));
  gens:=Generators(s);  data_ht:=OrbitsOfKernels(s)!.data_ht;
  o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];
  orbits:=[o[1]!.orbits, o[2]!.orbits];

  for i in [1..n] do
    for x in gens do
      
      for f in GreensRClassReps(d[i]) do
        data:=PreInOrbitsOfKernels(s, x * f, true, orbits)[2][2]{[1..6]};
        data[3]:=KernelOrbitSCCFromData(s, data, o[2])[1];
        AddSet(out[i], HTValue(data_ht, data));
      od;
      
      for f in GreensLClassReps(d[i]) do
        data:=PreInOrbitsOfKernels(s, f * x, true, orbits)[2][2]{[1..6]};
        data[3]:=KernelOrbitSCCFromData(s, data, o[2])[1];
        AddSet(out[i], HTValue(data_ht, data));
      od;
    od;
  od;

  return out;
end);

# new for 4.0! - PreInOrbitsOfKernels - not a user function!
#############################################################################
# Usage: s = semigroup (not a D-class), f = transformation,
# rectify = should l in kernel data correspond to f (false) or be o[scc[1]] (true),
# o = [OrbitsOfImages(s)!.orbits, OrbitsOfImages(s)!.orbits] (optional), 
# d = [image data, kernel data] (including true/false) (optional). 

# Notes: returns the data of the D-class containing f where l is for the R-class
# rep in image data and l is for the D-class rep in kernel data.  This should
# method should be replaced in due course by a method for \in for OrbitsOfKernels.

InstallGlobalFunction(PreInOrbitsOfKernels, 
function(arg)
  local s, f, kernels, o, data;
  
  s:=arg[1]; f:=arg[2];

  if not IsTransformationSemigroup(s) then #this can probably be remove
                                           #at some future point
    Info(InfoWarning, 1, 
    "Usage: s = semigroup (not a D-class), f = transformation,\n", 
    "#I  o = [OrbitsOfImages(s)!.orbits, OrbitsOfImages(s)!.orbits] (optional),\n", 
    "#I  d = [image data, kernel data] (including true/false) (optional).");
    return fail;
  fi;

  kernels:=OrbitsOfKernels(s)!.kernels;

  if Length(arg)>=4 then 
    o:=arg[4];
  else
    o:=[OrbitsOfImages(s)!.orbits, OrbitsOfKernels(s)!.orbits];
  fi;
  
  if Length(arg)>=5 then 
    data:=arg[5];
  else
    data:=[PreInOrbitsOfImages(s, f, arg[3], o[1]), [false, 
     [fail, fail, fail, fail, fail , 0, fail, fail]]]; 
  fi;
  
  if not data[1][1] then #f not in OrbitsOfImages(s)
    return data;
  fi;
  
  return InOrbitsOfKernels(f, arg[3], data, o, kernels);
end);

# new for 4.0! - PrintObj - "for orbits of kernels"
#############################################################################

InstallMethod(PrintObj, [IsOrbitsOfKernels], 
function(o)
  Print("<orbits of kernels; ", SizeOrbitsOfKernels(o!.s), " elements; ", 
   Length(o!.data), " D-classes>");
end);

# new for 4.0! - PrintObj - "for iterator of D-class reps"
#############################################################################

InstallMethod(PrintObj, [IsIteratorOfDClassReps], 
function(iter)
  local s;

  s:=iter!.s;

  Print( "<iterator of D-class reps, ", Length(OrbitsOfImages(s)!.data), 
    " candidates, ", SizeOrbitsOfKernels(s), " elements, ", 
    Length(OrbitsOfKernels(s)!.data), " D-classes>");
  return;
end);

# new for 4.0! - PrintObj - "for iterator of D-classes"
############################################################################

InstallMethod(PrintObj, [IsIteratorOfGreensDClasses], 
function(iter)
  Print( "<iterator of D-classes>");
return;
end);

#SSS

# new for 4.0! - SchutzenbergerGroup - "for a D-class of a trans. semigp."
############################################################################

InstallOtherMethod(SchutzenbergerGroup, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  local e;
  e:=d!.data[2];
  return KernelOrbitFromData(d!.parent, d!.data, d!.o)!.
   d_schutz[e[4]][e[5]][e[6]][2];
end);

# new for 4.0! - Size - "for a D-class of a trans. semigp."
#############################################################################

InstallOtherMethod(Size, "for a D-class of a trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  local r, l;

  r:=ImageOrbitSchutzGp(d); l:=KernelOrbitSchutzGp(d);

  return Size(r)*Length(ImageOrbitSCC(d))*Length(KernelOrbitSCC(d))*Size(l)/
   Size(SchutzenbergerGroup(d));
end);

# new for 4.0! - SizeOrbitsOfKernels - not a user function.
#############################################################################
# Usage: s = transformation semigroup.

# Notes: this should be a method for Size for the OrbitsOfKernels object.

InstallGlobalFunction(SizeOrbitsOfKernels, 
function(s)
  local data, i, r, l, d;

  data:=OrbitsOfKernels(s)!.data; i:=0;

  for d in data do
    r:=ImageOrbitSchutzGpFromData(s, d[1]);
    l:=KernelOrbitSchutzGpFromData(s, d[2]);
    i:=i+(Size(r)*Length(ImageOrbitSCCFromData(s, d[1]))
     *Length(KernelOrbitSCCFromData(s, d[2]))*Size(l)/
     Size(DClassSchutzGpFromData(s,  d[2])));
  od;

  return i;
end);

#EOF


