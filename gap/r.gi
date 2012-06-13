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

#DDD

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

#NNN

# new for 0.1! - NrHClasses - "for an R-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(NrHClasses, "for an R-class of a trans. semigroup", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
r-> NrLClasses(DClassOfRClass(r)));

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
