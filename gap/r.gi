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

#JDM here

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

#EOF
