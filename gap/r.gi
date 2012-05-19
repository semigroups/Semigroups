#############################################################################
##
#W  r.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#############################################################################
# Notes

# - this file is alphabetized, keep it that way!

# - this file should only contain functions relating to images/R-classes!

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
  local rep, d, o, i, schutz, g;

  rep:=Representative(r);

  if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or
   RankOfTransformation(f) <> RankOfTransformation(rep) or
   CanonicalTransSameKernel(f) <> CanonicalTransSameKernel(rep) then 
    Info(InfoCitrus, 1, "degree, rank, or kernel not equal to those of", 
    " any of the R-class elements,"); 
    return false;
  fi;

  d:=r!.data; o:=r!.o!.orbits[d[1]][d[2]];

  i:= Position(o, ImageSetOfTransformation(f));

  if i = fail or not o!.truth[d[4]][i] then #check they are in the same scc
    Info(InfoCitrus, 1, "image not equal to that of any R-class element,");
    return false;
  fi;

  schutz:=ImageOrbitStabChain(r);
  
  if schutz=true then
    Info(InfoCitrus, 3, "Schutz. group of R-class is symmetric group");
    return true;
  fi;

  g:=f*o!.perms[i];

  if g=rep then
    Info(InfoCitrus, 3, "transformation with rectified image equals ", 
    "R-class representative");
    return true;
  elif schutz=false then
    Info(InfoCitrus, 3, "Schutz. group of R-class is trivial");
    return false;
  fi;

  return SiftedPermutation(schutz, PermLeftQuoTransformationNC(rep, g))=();
end);

#AAA

# mod for 0.4! - AddToOrbitsOfImages - not a user function! 
#############################################################################
# Usage: s = semigroup or d-class; f = trans. img. list; data = image data; 
# o = OrbitsOfImages(s).
# Notes: if s is a d-class, then data should have j, k, l, m and g not = fail!

InstallGlobalFunction(AddToOrbitsOfImages,
function(s, f, data, o, install)
  local j, k, l, m, val, n, g, O, gens, d, lens, data_ht, images, ht, gen1, 
  pos1, f_o, out, reps, ker, i, z, y;

  j:=data[1]; 	# img size
  k:=data[2]; 	# index of orbit containing img
  l:=data[3]; 	# position of img in O[j][k]
  m:=data[4]; 	# scc of O[j][k] containing img
  val:=data[5]; # position of ker in O[j][k]!.kernels_ht[m]
  n:=data[6]; 	# the length of O[j][k]!.reps[m][val]
  g:=data[7];	# f*O[j][k]!.perms[l];

  O:=o!.orbits; gens:=o!.gens; d:=o!.data; lens:=o!.lens;
  data_ht:=o!.data_ht; 

  if install then # o = OrbitsOfImages(s)
    images:=o!.images; ht:=o!.ht; 
    if s!.opts!.schreier then 
      gen1:=o!.gen1; pos1:=o!.pos1; 
    fi;
    o:=ht!.o; 
  fi;

  if k = fail then  #new img and l, m, val, n, g=fail
                    #don't call this function with a d-class and k=fail!

  #########################################################################
          
    lens[j]:=lens[j]+1;
    f_o:=ForwardOrbitOfImage(s, f, images, gens);
          
    if IsBound(O[j]) then 
      O[j][lens[j]]:=f_o;
    else
      O[j]:=[f_o];
    fi;
          
    for i in f_o do 
      HTAdd(images, i, lens[j]);
    od;
          
    out:=[j, lens[j], 1, 1, 1, 1];
    g:=f;
   
  ###########################################################################

  else #old img
    reps:=O[j][k]!.reps[m];

    if not val=fail then #old kernel
      reps[val][n+1]:=g;
      out:=[j, k, l, m, val, n+1];
    else #new kernel
      val:=Length(reps)+1;
      if reps=[] then #scc not previously considered 
        O[j][k]!.perms:=O[j][k]!.perms+CreateImageOrbitSCCPerms(gens, 
         O[j][k], m);
        if g=fail then 
          g:=OnTuples(f, O[j][k]!.perms[l]);
        fi;
        ker:=CanonicalTransSameKernel(g); 
        O[j][k]!.kernels_ht[m]:=HTCreate(ker, 
         rec(forflatplainlists:=true, hashlen:=s!.opts!.hashlen!.S));
        HTAdd(O[j][k]!.kernels_ht[m], ker, 1);
        O[j][k]!.schutz[m]:=CreateImageOrbitSchutzGp(gens, O[j][k], g, m);
      fi;
      
      reps[val]:=[g];
      out:=[j, k, O[j][k]!.scc[m][1], m, val, 1];
      HTAdd(O[j][k]!.kernels_ht[m], CanonicalTransSameKernel( g ), val);
    fi;
  fi;

  i:=Length(d)+1; d[i]:=out; HTAdd(data_ht, out, i);

  #install new pts in the orbit

  if install then
    m:=Length(gens); j:=Length(o);
    if s!.opts!.schreier then 
      for y in [1..m] do
        z:=g{gens[y]};
        if HTValue(ht, z)=fail then  
          j:=j+1; z:=HTAdd(ht, z, j); o[j]:=ht!.els[z]; 
          pos1[j]:=i; gen1[j]:=y;
        fi;
      od;
    else
      for y in [1..m] do
        z:=g{gens[y]};
        if HTValue(ht, z)=fail then
          j:=j+1; z:=HTAdd(ht, z, j); o[j]:=ht!.els[z];
        fi;
      od;
    fi;
  fi;
  return out;
end);

# new for 0.1! - AsList - "for an R-class of trans. semigp."
#############################################################################
# Algorithm D.

InstallOtherMethod(AsList, "for an R-class of trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local f, g, elts, perms, scc, p, i;
  
  Info(InfoCitrus, 4, "AsList: for an R-class");

  f:=r!.rep; g:=List(SchutzenbergerGroup(r), x-> f*x);
  elts:=EmptyPlist(Size(r));

  perms:=ImageOrbitPerms(r); scc:=ImageOrbitSCC(r);

  for i in scc do 
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

# JDM one method for all types of classes!

InstallOtherMethod(AsSSortedList, "for R-class of trans. semigp.",
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  Info(InfoCitrus, 4, "AsSSortedList: for an R-class");
  return ConstantTimeAccessList(EnumeratorSorted(r));
end);

#CCC

# mod for 0.4! - CreateImageOrbitSCCPerms - not a user function!
#############################################################################
# Usage: gens = imgs of generators; o = image orbit; j = index of scc.

InstallGlobalFunction(CreateImageOrbitSCCPerms,
function(gens, o, j)
  local p, scc, f, i;

  p:=EmptyPlist(Length(o)); scc:=o!.scc[j];

  for i in scc do
    f:=CitrusEvalWord(gens, TraceSchreierTreeOfSCCBack(o, j, i));
    p[i]:=MappingPermListList(o[i], f{o[i]});
  od; 
  
  #JDM this worked, but not with Factorization!!
  
  #for i in scc do
  #  f:=EvaluateWord(gens, TraceSchreierTreeOfSCCForward(o, j, i)); 
  #  p[i]:=MappingPermListList(OnTuples(o[scc[1]], f), o[scc[1]]);
  #od;

  return p;
end);

# mod for 0.4! - CreateImageOrbitSchutzGp - not a user function!
#############################################################################
# Usage: gens = generators as list of imgs; o = image orbit;
# f = img. list of rep of scc with index k;
# k = index of scc containing position of image of f in o.

InstallGlobalFunction(CreateImageOrbitSchutzGp,
function(gens, o, f, k) 
  local scc, bound, g, p, t, graph, is_sym, l, words, h, K, i, j;

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
        #h:=PermLeftQuoTransformationNC(f, f/p[i] * (gens[j]*p[graph[i][j]]));
        
        #h:=PermLeftQuoTransformationNC(f,
        # f*EvaluateWord(gens, TraceSchreierTreeOfSCCForward(o, k, i)) * 
        #  (gens[j]*p[graph[i][j]]));
        
        h:=OnTuples(gens[j], p[graph[i][j]]){CitrusEvalWord(gens,
        TraceSchreierTreeOfSCCForward(o, k, i))};
        h:=PermLeftQuoTransformationNC(f, h{f});
        
        if not h=() then 
          K:=ClosureGroup(g, h);
          if Size(K)>Size(g) then 
            g:=K; l:=l+1; words[l]:=[i,j];
          fi;
        fi;

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

  #data:=data{[1..6]};

  r:=Objectify(RClassType(s), rec(parent:=s, data:=data, 
   o:=orbit, rep:=rep));

  SetRepresentative(r, rep);
  SetEquivalenceClassRelation(r, GreensRRelation(s));
  return r;
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
  Print("size: \t\t", Size(OrbitsOfImages(s)), "\n");
  Print("R-classes: \t", NrRClasses(OrbitsOfImages(s)), "\n");
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

  Info(InfoCitrus, 4, "Enumerator: for an R-class");

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

# new for 0.1! - ExpandOrbitsOfImages - not a user function!
#############################################################################

InstallGlobalFunction(ExpandOrbitsOfImages, 
function(s)
  local o, iter, i;

  Info(InfoCitrus, 4, "ExpandOrbitsOfImages");

  o:=OrbitsOfImages(s);

  if not o!.finished then 
    iter:=IteratorOfNewRClassRepsData(s);
    for i in iter do od;
  fi;
  return true;
end);

#FFF

# mod for 0.5! - Factorization - "for a trans. semigp. and trans."
#############################################################################

InstallOtherMethod(Factorization, "for a trans. semigroup and trans.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsTransformation], 
function(s, f)
  local data, l, o, rep, p, w, g, q;
 
  if not f in s then 
    Error("transformation is not an element of the semigroup,");
    return;
  fi;
 
  if not s!.opts!.schreier then 
    Error("it is not possible to factorize elements of this semigroup,");
    return;
  fi;

  data:=PreInOrbitsOfImages(s, f, false)[2];

  l:=data[3]; o:=ImageOrbitFromData(s, data);
  data[3]:=ImageOrbitSCCFromData(s, data)[1]; #JDM hack rectify!
  rep:=RClassRepFromData(s, data); p:=data[8];

  if p=fail then 
    p:=PermLeftQuoTransformationNC(rep![1], data[7]);
  fi;

  if l=data[3] and p=() then # f is an R-class rep!
    Info(InfoCitrus, 2, "transformation is an R-class representative.");
    return TraceRClassRepsTree(s, RClassIndexFromData(s, data));
  fi;
  
  if not l=data[3] then 
    w:=TraceSchreierTreeOfSCCForward(o, data[4], l);
    g:=EvaluateWord(Generators(s), w);
    q:=PermLeftQuoTransformationNC(rep*g*ImageOrbitPermsFromData(s, data)[l],
     rep); # would be good to remove this step!
  else
    w:=[]; q:=();
  fi;

  if p*q=() then 
    return Concatenation(TraceRClassRepsTree(s, RClassIndexFromData(s,
     data)), w);
  fi;
  
  # f= rep*p*q*g. 
  
  return Concatenation(TraceRClassRepsTree(s, RClassIndexFromData(s, data)),
   Factorization(s, data, p*q), w);
end);

# new for 0.4! - Factorization - "for a trans. semi., img data, and perm" 
#############################################################################
# Usage: s = trans. semigroup, data = image data, f = permutation

# Returns: a word in the generators of s that acts on the image of
# the representative of the R-class with data <data> in the same way that f
# acts on this image.

# Notes: this is rather slow! Require some MN assistance with this one. 

InstallOtherMethod(Factorization, "for a trans. semi., img data, and perm",
[IsTransformationSemigroup, IsList, IsPerm],
function(s, data, f)
  local g, w, out, orders, power, gen, o, word, graph, m, u, i;
  
  g:=ImageOrbitSchutzGpFromData(s, data);
  w:=String(Factorization(g, f));
  
  if w="<identity ...>" then
    return [];
  fi;
 
  w:=List(SplitString(w, "*"), x-> SplitString(x, "^"));
  out:=[]; orders:=List(GeneratorsOfGroup(g), Order);
  
  for u in w do 
    if IsBound(u[2]) then 
      power:=Int(u[2]); gen:=Int(u[1]{[2..Length(u[1])]});
      if IsNegInt(power) then 
        power:=power+orders[gen];
      fi;
      for i in [1..power] do 
        Add(out, gen);
      od;
    else
      Add(out, Int(u[1]{[2..Length(u[1])]}));
    fi;
  od;
  o:=ImageOrbitFromData(s, data);
  word:=o!.schutz[data[4]][3];
  graph:=OrbitGraph(o); m:=data[4];

  return Concatenation(List(out, x->
  Concatenation([TraceSchreierTreeOfSCCForward(o, m,
  word[x][1]), [word[x][2]], TraceSchreierTreeOfSCCBack(o, m,
  graph[word[x][1]][word[x][2]])])));
end);

# mod for 0.4! - ForwardOrbitOfImage - not a user function!
#############################################################################
# Usage: s = semigroup; f = img list of trans; 
# images = OrbitsOfImages(s)!.images (optional);
# gens = Generators(s) (optional).

InstallGlobalFunction(ForwardOrbitOfImage, 
function(arg)
  local s, f, images, gens, img, deg, j, bound, treehashsize, o, r, reps, ker;

  s:=arg[1]; f:=arg[2];

  if Length(arg)>=3 then 
    images:=arg[3];
  else
    images:=fail;
  fi;

  if Length(arg)=4 then 
    gens:=arg[4];
  else
    gens:=List(Generators(s), x-> x![1]);
  fi;

  img:=SSortedList(f);
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
  r:=Length(OrbSCC(o));

  #representatives of R-classes with image belonging in scc[i] partitioned 
  #according to their kernels
  reps:=List([1..r], x-> []); reps[1][1]:=[f]; o!.reps:=reps;

  #kernels of representatives of R-classes with image belonging in scc[i]
  ker:=CanonicalTransSameKernel(f);
  o!.kernels_ht:=[HTCreate(ker, rec(forflatplainlists:=true, 
   hashlen:=s!.opts!.hashlen!.S))];
  HTAdd(o!.kernels_ht[1], ker, 1);

  #calculate the multipliers for all scc's 
  o!.perms:=EmptyPlist(Length(o));
  o!.perms:=o!.perms+CreateImageOrbitSCCPerms(gens, o, 1);

  #schutzenberger groups
  o!.schutz:=EmptyPlist(r);
  o!.schutz[1]:=CreateImageOrbitSchutzGp(gens, o, f, 1);

  #nr idempotents
  o!.nr_idempotents:=List([1..r], m-> []);

  return o;
end);

#GGG

# new for 0.1! - GreensHClasses - "for an R-class of a trans. semigp."
###########################################################################

InstallOtherMethod(GreensHClasses, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
  local s, d, o, m, data, out, f, h, i;

  Info(InfoCitrus, 4, "GreensHClasses: for an R-class");
  
  s:=r!.parent; d:=DClassOfRClass(r); o:=d!.o; 
  m:=NrHClasses(r); data:=HClassRepsData(r); 
  
  out:=EmptyPlist(m); 

  for i in [1..m] do 

    if HasHClassReps(r) then 
      f:=HClassReps(r)[i];
    else
      f:=HClassRepFromData(s, data[i], o);
    fi;

    h:=CreateHClass(s, data[i], o, f);
    SetRClassOfHClass(h, r); SetDClassOfHClass(h, d);
    out[i]:=h;
  od;

  return out;
end);

# new for 0.5! - GreensHClassOfElementNC - "for an R-class and trans."
###########################################################################

InstallOtherMethod(GreensHClassOfElementNC, "for an R-class and trans.",
[IsGreensRClass and IsGreensClassOfTransSemigp, IsTransformation], 
function(r, f)
  local d, data, l, schutz, g, cosets, i, p, h;

  d:=DClassOfRClass(r); data:=ShallowCopy(d!.data);
  data[1][3]:=Position(ImageOrbit(r), ImageSetOfTransformation(f));
  
  l:=Position(KernelOrbit(d), CanonicalTransSameKernel(f));
  data[2][3]:=l;

  data[4]:=fail; 

  schutz:=KernelOrbitStabChain(d);

  if schutz=true then
    data[3]:=();
  else
    g:=PermLeftQuoTransformationNC(Representative(d),
     KernelOrbitRels(d)[l][2]*f*ImageOrbitPerms(d)[data[1][3]]);

    cosets:=ImageOrbitCosets(d);
    i:=0;

    if schutz=false then
      repeat
        i:=i+1;
      until g/cosets[i]=();
    else
      p:=KerRightToImgLeft(d)^-1;
      repeat
        i:=i+1;
      until SiftedPermutation(schutz, (g/cosets[i])^p)=();
    fi;
    data[3]:=cosets[i];
  fi;

  h:=CreateHClass(ParentAttr(r), data, d!.o,
    HClassRepFromData(ParentAttr(d), data, d!.o));  
  SetRClassOfHClass(h, r);
  return h;
end);

# new for 0.1! - HClassRepsData - "for an R-class of a trans. semigp."
#############################################################################
# JDM should we SetLClassReps of d? 

# JDM this and other like it should be iterators as illustrated by the 
# Coxeter semigroup example...

InstallOtherMethod(HClassRepsData, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local f, scc, d, cosets, out, k, data, i, j;

  Info(InfoCitrus, 4, "HClassRepsData: for an R-class");

  f:=r!.rep; scc:=ImageOrbitSCC(r);
  d:=DClassOfRClass(r); cosets:=ImageOrbitCosets(d);

  out:=EmptyPlist(Length(scc)*Length(cosets));
  
  if not HasNrHClasses(r) then 
    SetNrHClasses(r, Length(scc)*Length(cosets));
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

# new for 0.1! - HClassRepsDataFromData - not a user function
#############################################################################
# Usage: s = semigroup; data = image data; o = OrbitsOfImages.

InstallGlobalFunction(HClassRepsDataFromData, 
function(s, data, o)
  local f, scc, d, cosets, out, k, i, j;

  Info(InfoCitrus, 4, "HClassRepsDataFromData: for an R-class");

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

  Info(InfoCitrus, 4, "GreensRClasses: for a trans. semi.");

  iter:=IteratorOfRClasses(s);
  out:=EmptyPlist(NrRClasses(s));
  i:=0;

  for r in iter do 
    i:=i+1;
    out[i]:=r;
  od;

  return out;
end);

# new for 0.1! - GreensRClassOfElement - "for a trans. semigp and trans."
#############################################################################

InstallOtherMethod(GreensRClassOfElement, "for a trans. semigp and trans.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsTransformation],
function(s, f)
  local d;

  Info(InfoCitrus, 4, "GreensRClassOfElement: for trans. semi. and trans.");

  if not f in s then 
    Error("the transformation is not an element of the semigroup,");
    return;
  fi;

  d:=PreInOrbitsOfImages(s, f, true)[2];

  return CreateRClass(s, d{[1..6]}, OrbitsOfImages(s), RClassRepFromData(s, d));
end);

# new for 0.1! - GreensRClassOfElementNC - "for a trans. semigp and trans."
#############################################################################
# JDM double check everything is ok here!

InstallOtherMethod(GreensRClassOfElementNC, "for a trans. semigp and trans.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsTransformation],
function(s, f)
  local d, j, r, o, n;

  Info(InfoCitrus, 4, "GreensRClassOfElementNC: for trans. semi. and trans.");

  d:=PreInOrbitsOfImages(s, f, true);

  if d[1] then # f in s!
    Info(InfoCitrus, 2, "transformation is an element of semigroup");
    return CreateRClass(s, d[2]{[1..6]}, OrbitsOfImages(s),
     RClassRepFromData(s, d[2]));
  elif OrbitsOfImages(s)!.finished then #f not in s!
    Error("transformation is not an element of semigroup,");
    return;
  fi;

  Info(InfoCitrus, 2, "transformation may not be an element of semigroup");

  j:=Length(ImageSetOfTransformation(f));
  n:=DegreeOfTransformationSemigroup(s);
  o:=[]; o[j]:=[ForwardOrbitOfImage(s, f![1])]; 

  #JDM also PreInOrbitsOfImages might have some non-fail values, in which case
  # we should use them. If everything is known except val or n, then we don't
  # really want to do what we are doing here...

  o:=rec( finished:=false, orbits:=o, gens:=List(Generators(s), x-> x![1]), 
   s:=s, 
   deg := n, data:=[], images:=fail, lens:=List([1..n], function(x) if x=j then
   return 1; else return 0; fi; end), data_ht:=HTCreate([1,1,1,1,1,1],
   rec(forflatplainlists:=true, hashlen:=s!.opts!.hashlen!.M)));
  #local orbits of images! 
  #JDM shouldn't data contain [j,1,1,1,1,1]??

  r:=CreateRClass(s, [j,1,1,1,1,1], o, f);
  SetIsRClassNC(r, true);
  return r;
end);

# new for 0.1! - RClassRepsData - "for a transformation semigroup"
#############################################################################
# move to greens.gi

InstallMethod(RClassRepsData, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  Info(InfoCitrus, 4, "RClassRepsData: for a trans. semi.");
  ExpandOrbitsOfImages(s);
  return OrbitsOfImages(s)!.data;
end);

#HHH

# new for 0.1! - HClassReps - "for an R-class of a trans. semigp."
#############################################################################
# JDM should we SetLClassReps of d? 

InstallOtherMethod(HClassReps, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
  local f, cosets, perms, scc, out, k, i, j;

  Info(InfoCitrus, 4, "HClassReps: for an R-class");

  if HasGreensHClasses(r) then 
    return List(GreensHClasses(r), Representative);
  fi;

  f:= r!.rep; cosets:=ImageOrbitCosets(DClassOfRClass(r));
  perms:=ImageOrbitPerms(r); scc:=ImageOrbitSCC(r);

  out:=EmptyPlist(Length(scc)*Length(cosets));

  if not HasNrHClasses(r) then 
    SetNrHClasses(r, Length(scc)*Length(cosets));
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

  Info(InfoCitrus, 4, "Idempotents: for an R-class");

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

# new for 0.4! - ImageOrbitKersHTFromData - not a user function!
############################################################################

InstallGlobalFunction(ImageOrbitKersHTFromData, 
function(arg)
  local s, d, o;
  
  s:=arg[1]; d:=arg[2];

  if Length(arg)=3 then 
    o:=arg[3]!.orbits[d[1]][d[2]];
  else
    o:=OrbitsOfImages(s)!.orbits[d[1]][d[2]];
  fi;

  return o!.kernels_ht[d[4]];
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

InstallGlobalFunction(InOrbitsOfImages, 
function(f, rectify, data, o, images)
  local j, k, l, m, val, n, g, img, schutz, reps, i, p, perms;

  j:=data[1]; k:=data[2]; l:=data[3];
  m:=data[4]; val:=data[5]; n:=data[6]; 
  g:=data[7]; 

  if k=fail then 
    #img:=ImageSetOfTransformation(f);
    img:=SSortedList(f);
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
    perms:=o[j][k]!.perms;
    
    if not IsBound(perms[l]) then 
      return [false, [ j, k, l, m, fail, 0, fail ] ];
    fi;

    #g:=f*perms[l];
    g:=OnTuples(f, perms[l]);
  fi;

  if g=fail then 
  #this can happen if coming from RClassReps.
    if IsBound(o[j][k]!.perms[l]) then 
      #g:=f*o[j][k]!.perms[l];
      g:=OnTuples(f, o[j][k]!.perms[l]);
    else
      return [false, [j,k,l,m,val,n,g]];
    fi;
  fi;

  if rectify then 
    l:=o[j][k]!.scc[m][1];
  fi;

  if val=fail then
    if not IsBound(o[j][k]!.kernels_ht[m]) then 
      return [false, [j, k, l, m, fail, 0, g]];
    fi;

    val:=HTValue(o[j][k]!.kernels_ht[m], CanonicalTransSameKernel(f));
    if val=fail then 
      return [false, [j, k, l, m, fail, 0, g]];
    fi;
  fi;

  schutz:=o[j][k]!.schutz[m][1];

  if schutz=true then 
    return [true, [j,k,l,m,val,1,g, fail]];
  fi;

  reps:=o[j][k]!.reps[m][val];
  i:=Length(reps);

  if schutz=false then 
    while n<i do 
      n:=n+1;
      if reps[n]=g then 
        return [true, [j, k, l, m, val, n, g, ()]];
      fi;
    od;
  else    
    while n<i do 
      n:=n+1;
      p:=PermLeftQuoTransformationNC(reps[n], g); 
      if SiftedPermutation(schutz, p)=() then 
        return [true, [j,k,l,m,val,n,g,p]];
      fi;
    od;
  fi;

  return [false, [j,k,l,m,val,n,g]];
end);

# new for 0.4! - IsRClassNC - "for a Green's class of trans. semigroup"
#############################################################################

InstallMethod(IsRClassNC, "for a Green's class of trans. semigroup", 
[IsGreensClassOfTransSemigp], ReturnFalse);

# new for 0.1! - IsRegularRClass - "for a Green's class of trans. semigroup"
#############################################################################

InstallMethod(IsRegularRClass, "for a Green's class of trans. semigroup",
[IsGreensClassOfTransSemigp], 
function(r)

  Info(InfoCitrus, 4, "IsRegularRClass: for a Green's class");

  if not IsGreensRClass(r) then 
    Info(InfoCitrus, 2, r, " is not an R-class,");
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

# mod for 0.4! - IsRegularRClassData - not a user function
#############################################################################
# Usage: s = semigroup; d = image data; o = OrbitsOfImages (optional)
# f = R-class rep (optional)

InstallGlobalFunction(IsRegularRClassData, 
function(arg)
  local s, d, o, f, scc, i;

  Info(InfoCitrus, 4, "IsRegularRClassData");

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

  if IsBound(o!.nr_idempotents[d[4]][d[5]]) then
    return o!.nr_idempotents[d[4]][d[5]]>0;
  fi;

  for i in scc do
    if IsInjectiveTransOnList(f, o[i]) then
      return true;
    fi;
  od;

  return false;
end);

# new for 0.1! - Iterator - "for a R-class of a trans. semigroup"
#############################################################################
# this is more efficient!

InstallMethod(Iterator, "for a R-class of a trans. semigroup",
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)
  local iter;

  Info(InfoCitrus, 4, "Iterator: for an R-class");

  if HasAsSSortedList(r) then 
    iter:=IteratorList(AsSSortedList(r));
  else
    iter:=IteratorByFunctions(rec( 
          
      schutz:=List(SchutzenbergerGroup(r), x-> r!.rep*x),
      #turns out this is a good idea!

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

# mod for 0.7! - IteratorOfRClasses - "for a trans. semigroup"
#############################################################################

InstallMethod(IteratorOfRClasses, "for a trans. semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local iter;

  Info(InfoCitrus, 4, "IteratorOfRClasses");

  iter:=IteratorByFunctions( rec(
          
    data:=IteratorOfRClassRepsData(s),
    
    IsDoneIterator := iter -> IsDoneIterator(iter!.data), 
          
    NextIterator:= function(iter)
      local d;
          
      d:=NextIterator(iter!.data);
          
      if d=fail then 
        return fail;
      fi;
          
      return CreateRClass(s, d{[1..6]}, OrbitsOfImages(s), RClassRepFromData(s, d));
    end,

    ShallowCopy:=iter-> rec(data:=IteratorOfRClassRepsData(s))));

  SetIsIteratorOfRClasses(iter, true);
  SetIsCitrusPkgIterator(iter, true); 
  return iter;
end);

# new for 0.1! - IteratorOfNewRClassRepsData - not a user function!
#############################################################################

InstallGlobalFunction(IteratorOfNewRClassRepsData, 
function(s)
  local iter, o;

  Info(InfoCitrus, 4, "IteratorOfNewRClassRepsData");
  
  o:=OrbitsOfImages(s);
  iter:=IteratorOfRClassRepsData(s);
  iter!.i:=Length(o!.data); 
  return iter;
end);

# new for 0.1! - IteratorOfRClassRepsData - not a user function!
#############################################################################

InstallMethod(IteratorOfRClassRepsData, "for a trans. semigroup",
[IsTransformationSemigroup],
function(s)
  local iter, is_done_iterator;
  
  Info(InfoCitrus, 4, "IteratorOfRClassRepsData");

  if s!.opts!.schreier then 
    is_done_iterator:=function(iter)
      local O, ht, o, i, orbits, images, x, d;
   
      if iter!.last_called_by_is_done then 
        return iter!.next_value=fail;
      fi;

      iter!.last_called_by_is_done:=true;

      O:=OrbitsOfImages(s);

      iter!.next_value:=fail;

      if iter!.i < Length(O!.data) then # we already know this rep
        iter!.i:=iter!.i+1;
        iter!.next_value:=O!.data[iter!.i];
        return false;
      elif O!.finished then  
        return true;
      fi;

      ht:=O!.ht; o:=ht!.o; i:=O!.at;

      if i=Length(o) then #at the end of the orbit!
        O!.finished:=true;
        return true;
      fi;

      orbits:=O!.orbits; images:=O!.images;

      while i<Length(o) do 
        O!.at:=O!.at+1; i:=i+1; x:=o[i];
        d:=InOrbitsOfImages(x, false, [fail, fail, fail, fail, fail, 0, fail], 
         orbits, images); 

        if not d[1] then #new rep!
          if IsTransformationMonoid(s) or not i = 1 then 
            Add(O!.pos2, i); Add(O!.gen2, d[2]{[1..4]}); 
            d:=AddToOrbitsOfImages(s, x, d[2], O, true);
            iter!.i:=iter!.i+1; iter!.next_value:=d;
            return false;
          fi;
        fi;
      od;

      O!.finished:=true;
      return true;
    end;
  else
    is_done_iterator:=function(iter)
      local O, ht, o, i, orbits, images, x, d;
                    
      if iter!.last_called_by_is_done then
        return iter!.next_value=fail;
      fi;
                                                
      iter!.last_called_by_is_done:=true;
  
      O:=OrbitsOfImages(s);
  
      iter!.next_value:=fail;
  
      if iter!.i < Length(O!.data) then # we already know this rep
        iter!.i:=iter!.i+1;
        iter!.next_value:=O!.data[iter!.i];
        return false;
      elif O!.finished then
        return true;
      fi;
      
      ht:=O!.ht; o:=ht!.o; i:=O!.at;
      
      if i=Length(o) then #at the end of the orbit!
        O!.finished:=true;
        return true;
      fi;
    
      orbits:=O!.orbits; images:=O!.images;
    
      while i<Length(o) do
        O!.at:=O!.at+1; i:=i+1; x:=o[i];
        d:=InOrbitsOfImages(x, false, [fail, fail, fail, fail, fail, 0, fail],
         orbits, images);
       
        if not d[1] then #new rep!
          if IsTransformationMonoid(s) or not i = 1 then
            d:=AddToOrbitsOfImages(s, x, d[2], O, true);
            iter!.i:=iter!.i+1; iter!.next_value:=d;
            return false;
          fi;
        fi;
      od;
    
      O!.finished:=true;
      return true;
    end; 
  fi;

  iter:=IteratorByFunctions( rec(
          
  ShallowCopy := iter -> rec( i:=0, s:=iter!.s, 
  next_value:=fail, last_called_by_is_done:=false),

  i:=0, # representative index i.e. which representative we are at

  s:=s,

  next_value := fail,

  last_called_by_is_done:=false,

  ######################################################################

  IsDoneIterator:=is_done_iterator,

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

# new for 0.1! - IteratorOfRClassReps - "for a trans. semigroup"!
#############################################################################

InstallMethod(IteratorOfRClassReps, "for a trans. semigroup",
[IsTransformationSemigroup],
function(s)
  local iter;

  Info(InfoCitrus, 4, "IteratorOfRClassReps");
  
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

# new for 0.1! - NrHClasses - "for an R-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(NrHClasses, "for an R-class of a trans. semigroup", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
r-> NrLClasses(DClassOfRClass(r)));

# mod for 0.5! - NrRClasses - "for a transformation semigroup"
#############################################################################

InstallMethod(NrRClasses, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)

  Info(InfoCitrus, 4, "NrRClasses");

  ExpandOrbitsOfImages(s);
  #return NrRClasses(OrbitsOfImages(s));
  return Length(OrbitsOfImages(s)!.data);
end);

# new for 0.1! - NrIdempotents - "for an R-class of a trans. semigp."
#############################################################################

InstallOtherMethod(NrIdempotents, "for an R-class of a trans. semigp.", 
[IsGreensRClass and IsGreensClassOfTransSemigp],
function(r)

  Info(InfoCitrus, 4, "NrIdempotents: for an R-class");

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

  Info(InfoCitrus, 4, "NrIdempotentsRClassFromData");

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

# new for 0.5! - NrRClasses - "for orbits of images" 
#############################################################################

InstallOtherMethod(NrRClasses, "for orbits of images",
[IsOrbitsOfImages],
function(data)
  local c, m, i, j, k, l;

  m:=0; c:=data!.orbits;

  for i in c do
    for j in i do 
      for k in j!.reps do 
        for l in k do 
          m:=m+Length(l);
        od;
      od;
    od;
  od;

  return m;
end);

#OOO

# mod for 0.4! - OrbitsOfImages - "for a transformation semigroup"
#############################################################################

InstallMethod(OrbitsOfImages, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local gens, n, one, ht, j, i, type, o, ht_len;

  Info(InfoCitrus, 4, "OrbitsOfImages");

  gens:=List(Generators(s), x-> ShallowCopy(x![1]));
  #JDM remove ShallowCopy in future if Orb is fixed.
  n := Degree(s); 
  ht_len:=s!.opts!.hashlen!.L;
  o:=EmptyPlist(ht_len);

  one := [ 1 .. n ]*1;
  ht := HTCreate(one, rec(forflatplainlists:=true, hashlen:=ht_len));  
  ht!.o:=o; 

  j:=HTAdd(ht, one, 1);
  o[1]:=ht!.els[j];

  for i in [2..Length(gens)+1] do 
    j:=HTAdd(ht, gens[i-1], i);
    o[i]:=ht!.els[j];
  od;
  
  type:=NewType(FamilyObj(s), IsOrbitsOfImages);

  return Objectify(type, rec(
    finished:=false, 
    orbits:=EmptyPlist(n),
    lens:=[1..n]*0, #lens[j]=Length(orbits[j])
    images:=HTCreate(SSortedList(gens[1]), rec(forflatplainlists:=true, 
     hashlen:=s!.opts!.hashlen!.S)),
    at:=0, 
    gens:=gens, 
    ht:=ht,
    data_ht:=HTCreate([1,1,1,1,1,1], rec(forflatplainlists:=true, 
     hashlen:=s!.opts!.hashlen!.M)),
    data:=[], 
    gen1:=ListWithIdenticalEntries(Length(gens)+1, fail), 
    pos1:=ListWithIdenticalEntries(Length(gens)+1, fail),
    gen2:=[], # JDM this can be removed as soon as InOrbitsOfImage returns 
              # both the rectified and unrectified image positions. 
    pos2:=[]
  ));
end);

#PPP

# new for 0.1! - ParentAttr - "for Green's class of a trans. semigroup"
############################################################################
# JDM move to greens.gi, is this a good name?

InstallMethod(ParentAttr, "for a Green's class of a trans. semigroup", 
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
  
  if IsTransformation(f) then 
    f:=f![1];
  fi;

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
  Size(o), " elements; ", NrRClasses(o), " R-classes>");
end);

# new for 0.1! - PrintObj - for IsIteratorOfRClassRepsData
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassRepsData], 
function(iter)
  local O, s;

  s:=iter!.s;
  O:=OrbitsOfImages(s);
  # JDM O!.ht!.o is unbound if the calc is completed. 
  Print( "<iterator of R-class reps data, ", Length(O!.ht!.o), " candidates, ", 
   Size(OrbitsOfImages(s)), " elements, ", NrRClasses(OrbitsOfImages(s)), 
   " R-classes>");
  return;
end);

# new for 0.1! - PrintObj - IsIteratorOfRClassReps
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassReps], 
function(iter)
  local O, s;
  if IsBound(iter!.s) then 
    s:=iter!.s; O:=OrbitsOfImages(s);

    Print( "<iterator of R-class reps, ", Length(O!.ht!.o), " candidates, ", 
     Size(OrbitsOfImages(s)), " elements, ", NrRClasses(OrbitsOfImages(s)), 
     " R-classes>");
    return;
  fi;
  Print("<iterator of R-class reps>");
  return;
end);

# new for 0.1! - PrintObj - for IsIteratorOfRClasses
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClasses], 
function(iter)
  Print( "<iterator of R-classes>");
  return;
end);

# mod for 0.8! - PrintObj - for IsIteratorOfSemigroup
############################################################################

InstallMethod(PrintObj, [IsIteratorOfSemigroup], 
function(iter)
  if IsFullTransformationSemigroup(iter!.s) then 
    Print("<iterator of full trans. semigroup>");
  elif IsTransformationSemigroup(iter!.s) then 
    Print("<iterator of transformation semigroup>");
  elif IsPartialPermSemigroup(iter!.s) then 
    Print("<iterator of semigroup of partial perms>");
  fi;
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

  Info(InfoCitrus, 4, "Random: for an R-class");

  f:=r!.rep;
  g:=Random(SchutzenbergerGroup(r));
  i:=Random(ImageOrbitSCC(r));
  
  return f*g*ImageOrbitPerms(r)[i]^-1; 
end);

# new for 0.4! - RClassIndexFromData - not a user function!
############################################################################
# Usage: s = semigroup; d = image data.

# Returns: the index <i> of the R-class corresponding to the image data <d>, 
# that is, GreensRClasses(s)[i]!.data=d.

InstallGlobalFunction(RClassIndexFromData, 
function(s, d)
  return HTValue(OrbitsOfImages(s)!.data_ht, d{[1..6]});
end);

# mod for 0.4! - RClassRepFromData - not a user function!
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

  return TransformationNC(o!.reps[d[4]][d[5]][d[6]]);
end);

# new for 0.1! - RClassReps - "for a transformation semigroup"
#############################################################################

InstallMethod(RClassReps, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  Info(InfoCitrus, 4, "RClassReps: for a trans. semi.");

  ExpandOrbitsOfImages(s);
  return List(OrbitsOfImages(s)!.data, x-> RClassRepFromData(s, x));
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

  Info(InfoCitrus, 4, "Size: for an R-class");

  return Size(SchutzenbergerGroup(r))*Length(ImageOrbitSCC(r));
end);

# new for 0.5! - Size - "for orbits of images"
#############################################################################
# Usage: s = semigroup.

InstallOtherMethod(Size, "for orbits of images",
[IsOrbitsOfImages],
function(data)
  local i, j, o, orbits;
  
  i:=0; orbits:=data!.orbits;

  for o in Concatenation(Compacted(orbits)) do 
    for j in [1..Length(o!.scc)] do
      if IsBound(o!.schutz[j]) and IsBound(o!.reps[j]) and 
       IsBound(o!.scc[j]) then 
        i:=i+Size(o!.schutz[j][2])*
        Sum(List(o!.reps[j]), Length)*Length(o!.scc[j]);
      fi;
    od;
  od;

  return i;
end);

#TTT

# new for 0.4! - TraceRClassRepsTree - not a user function!
#############################################################################
# Usage: s = trans. semigroup; i = index of R-class rep 

# Returns: a word in the generators of s equal to GreensRClassReps(s)[i]. 

InstallGlobalFunction(TraceRClassRepsTree, 
function(s, i)
  local o, gen1, pos1, gen2, pos2, word_1, word_2, j, orb, m, l;

  Info(InfoCitrus, 4, "TraceRClassRepsTree");

  if not s!.opts!.schreier then 
    Error("it is not possible to factorize elements of this semigroup,");
    return;
  fi;

  o:=OrbitsOfImages(s);
  gen1:=o!.gen1; pos1:=o!.pos1; gen2:=o!.gen2; pos2:=o!.pos2; o:=o!.orbits;

  word_1:=[]; word_2:=[]; j:=i;

  while not gen1[pos2[j]]=fail do
    Add(word_1, gen1[pos2[j]]);
    if not ForAny(gen2[j], x-> x=fail) then 
      orb:=o[gen2[j][1]][gen2[j][2]]; m:=gen2[j][4]; l:=gen2[j][3]; 
      word_2:= Concatenation(word_2, 
       Reversed(TraceSchreierTreeOfSCCBack(orb, m, l)));
    fi;
    j:=pos1[pos2[j]];
  od;
  
  if not pos2[j]=1 then  
    Add(word_1, pos2[j]-1);
  fi;
  
  if not ForAny(gen2[j], x-> x=fail) then 
    orb:=o[gen2[j][1]][gen2[j][2]]; m:=gen2[j][4]; l:=gen2[j][3];
    word_2:=Concatenation(word_2, 
     Reversed(TraceSchreierTreeOfSCCBack(orb, m, l)));
  fi;
  
  return Concatenation(word_1, Reversed(word_2));
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
