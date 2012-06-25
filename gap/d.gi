###########################################################################
##
#W  d.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

#############################################################################
## Notes

# - RegularDClasses

# new for 0.1! - GreensHClasses - "for a D-class of a trans. semigroup"
#############################################################################
# JDM could this be better/more efficient?! Could avoid using GreensRClasses. 

InstallOtherMethod(GreensHClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  return Concatenation(List(GreensRClasses(d), GreensHClasses));
end);

# new for 0.5! - GreensHClassOfElementNC - "for a D-class and trans."
#############################################################################

InstallOtherMethod(GreensHClassOfElementNC, "for a D-class and trans.",
[IsGreensDClass and IsGreensClassOfTransSemigp, IsTransformation], 
function(d, f)
  local l_img, data, schutz, g, r, reps, l_ker, cosets, p, h;
  
  l_img:=Position(ImageOrbit(d), ImageSetOfTransformation(f));
  
  data:=ShallowCopy(d!.data);
  
  # find image data

  data[1][3]:=l_img; 
  data[1][5]:=HTValue(ImageOrbitKersHT(d), CanonicalTransSameKernel(f));
  data[4]:=fail;
  
  schutz:=ImageOrbitStabChain(d);
  g:=f*ImageOrbitPerms(d)[l_img];
  r:=0;
  reps:=ImageOrbit(d)!.reps[data[1][4]][data[1][5]];

  if schutz=true then 
    r:=1;
  elif schutz=false then 
    repeat
      r:=r+1;
    until reps[r]=g![1]; 
  else
    repeat
      r:=r+1;
    until SiftedPermutation(schutz, PermLeftQuoTransformationNC(reps[r],
     g![1]))=();
  fi;

  data[1][6]:=r;

  # find cosets

  l_ker:=Position(KernelOrbit(d), CanonicalTransSameKernel(f));
  data[2][3]:=l_ker;
  
  schutz:=KernelOrbitStabChain(d);
  
  if schutz=true then 
    data[3]:=();
  else
    g:=PermLeftQuoTransformationNC(Representative(d), 
     KernelOrbitRels(d)[l_ker][2]*g);
    
    cosets:=ImageOrbitCosets(d);
    r:=0;
    
    if schutz=false then 
      repeat
        r:=r+1;
      until g/cosets[r]=();
    else
      p:=KerRightToImgLeft(d)^-1;
      repeat
        r:=r+1;
      until SiftedPermutation(schutz, (g/cosets[r])^p)=();
    fi;
    data[3]:=cosets[r];
  fi;

  h:=CreateHClass(ParentAttr(d), data, d!.o, 
    HClassRepFromData(ParentAttr(d), data, d!.o));
  SetDClassOfHClass(h, d);
  return h;
end);

# new for 0.1! - GreensLClasses - "for a D-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(GreensLClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  local s, o, m, out, i, f, l, data;

  s:=d!.parent; o:=d!.o; m:=NrLClasses(d); out:=EmptyPlist(m); 

  for i in [1..m] do 
    data:=List(LClassRepsData(d)[i], ShallowCopy); 
    if HasLClassReps(d) then 
      f:=LClassReps(d)[i];
    else
      f:=LClassRepFromData(s, data, o);
    fi;
    
    l:=CreateLClass(s, data, o, f);
    SetDClassOfLClass(l, d);
    out[i]:=l;
  od;

  return out;
end);

# new for 0.5! - GreensLClassOfElement - "for D-class and transformation"
#############################################################################

InstallOtherMethod(GreensLClassOfElement, "for D-class and transformation",
[IsGreensDClass and IsGreensClassOfTransSemigp, IsTransformation],
function(d, f)

  if not f in d then 
    Error("transformation is not an element of the D-class,");
    return;  
  fi;     

  return GreensLClassOfElementNC(d, f);
end);

# new for 0.5! - GreensLClassOfElementNC - "for D-class and transformation"
#############################################################################

InstallOtherMethod(GreensLClassOfElementNC,  "for D-class and transformation",
[IsGreensDClass and IsGreensClassOfTransSemigp, IsTransformation],
function(d, f)
  local l, schutz, data, g, cosets, r, p;

  l:=Position(ImageOrbit(d), ImageSetOfTransformation(f));
  schutz:=KernelOrbitStabChain(d);

  data:=ShallowCopy(d!.data);
  data[1][3]:=l;
  
  if schutz=true then 
    data[3]:=();
  else
    g:=f*ImageOrbitPerms(d)[l];
    l:=Position(KernelOrbit(d), CanonicalTransSameKernel(f));
    g:=PermLeftQuoTransformationNC(Representative(d),
     KernelOrbitRels(d)[l][2]*g);
    
    cosets:=ImageOrbitCosets(d);
    r:=0;
    
    if schutz=false then 
      repeat
        r:=r+1;
      until g/cosets[r]=();
    else
      p:=KerRightToImgLeft(d)^-1;
      repeat
        r:=r+1;
      until SiftedPermutation(schutz, (g/cosets[r])^p)=();
    fi;
    data[3]:=cosets[r];
  fi;

  l:=CreateLClass(ParentAttr(d), data, d!.o,
   Representative(d)*(data[3]/ImageOrbitPerms(d)[data[1][3]]));;
  SetDClassOfLClass(l, d);
  return l;
end);

#HHH

# new for 0.1! - HClassReps - "for a D-class"
############################################################################
# JDM surely this can be improved too...

InstallOtherMethod(HClassReps, "for a D-class",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  local s, img, ker, out;

  s:=d!.parent;
  img:=OrbitsOfImages(s); ker:=OrbitsOfKernels(s);
  out:=List(RClassRepsData(d), x->
   HClassRepsDataFromData(s, x, img));

  return List(Concatenation(out), x-> HClassRepFromData(s, x, [img, ker]));
end);

#III

# new for 0.1! - Idempotents - "for a D-class of a trans. semigp."
#############################################################################
# JDM compare what is here with the method for Idempotents of an R-class!

InstallOtherMethod(Idempotents, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  local ker_o, ker_scc, n, out, k, img_o, img_scc, m, i, j, l, lookup;

  if HasIsRegularDClass(d) and not IsRegularDClass(d) then 
    Info(InfoCitrus, 2, "the D-class is not regular.");
    return [];
  fi;

  if NrIdempotentsRClassFromData(d!.parent, d!.data[1], d!.o[1])=0 then 
    Info(InfoCitrus, 2, "the D-class is not regular.");
    return [];
  fi;

  if RankOfTransformation(d!.rep)=DegreeOfTransformation(d!.rep) then
    Info(InfoCitrus, 2, "the D-class is the group of units.");
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

# new for 0.1! - IsRegularDClass - "for a D-class of a trans. semigroup"
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

# new for 0.1! - IteratorOfDClassReps - user function!
#############################################################################
# Usage: s = transformation semigroup.

InstallMethod(IteratorOfDClassReps, "for a trans. semigroup", 
[IsTransformationSemigroup],
function(s)
  local iter;

  Info(InfoCitrus, 4, "IteratorOfDClassReps");
  
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

  return iter;
end);

# new for 0.1! - IteratorOfDClassRepsData - not a user function!
#############################################################################
# Usage: s = transformation semigroup.

InstallMethod(IteratorOfDClassRepsData, "for a trans. semigroup",
[IsTransformationSemigroup], 
function(s)
  local iter;
  
  Info(InfoCitrus, 4, "IteratorOfDClassRepsData");
  
  iter:=IteratorByFunctions( rec(

    ShallowCopy := iter -> rec(),

    i:=0, s:=s, next_value:=fail,

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
        d:=InOrbitsOfKernels(f, true, [[true, Concatenation(d, [f![1]])], 
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

#NNN

# new for 0.1! - NrHClasses - "for a D-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(NrHClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  return NrRClasses(d)*NrLClasses(d);
end);

# new for 0.1! - NrLClasses - "for a D-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(NrLClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  return Length(ImageOrbitCosets(d))*Length(ImageOrbitSCCFromData(d!.parent, d!.
  data[1], d!.o[1]));
end);

# new for 0.1! - NrRClasses - "for a D-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(NrRClasses, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)

  if HasRClassReps(d) then 
    return Length(RClassReps(d));
  fi;

  return Length(KernelOrbitSCC(d))*Length(KernelOrbitCosets(d));
end);

# new for 0.1! - NrIdempotents - "for a D-class of a trans. semigroup"
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

  if HasRClassRepsData(d) then 
    for i in RClassRepsData(d) do 
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

# new for 0.1! - NrRegularDClasses - "for a transformation semigroup"
#############################################################################

InstallMethod(NrRegularDClasses, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local o;
  ExpandOrbitsOfKernels(s);
  o:=OrbitsOfImages(s);

  return Number(OrbitsOfKernels(s)!.data, x-> IsRegularRClassData(s, x[1], o));
end);

#OOO

# new for 0.1! - OrbitsOfKernels - "for a transformation semigroup"
#############################################################################

InstallMethod(OrbitsOfKernels, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local n, gens;

  n:=DegreeOfTransformationSemigroup(s);
  gens:=Generators(s);

  return Objectify(NewType(FamilyObj(s), IsOrbitsOfKernels), rec(
    finished:=false,
    orbits:=EmptyPlist(n), 
    lens:=[1..n]*0, #lens[j]=Length(orbits[j])
    kernels:=HTCreate([1..n]*1, rec(forflatplainlists:=true,
     hashlen:=s!.opts!.hashlen!.S)),
    imgs_gens:=List(gens, x-> x![1]),
    gens:=gens,
    s:=s,
    data_ht:=HTCreate([1,1,1,1,1,1,1], rec(forflatplainlists:=true,
     hashlen:=s!.opts!.hashlen!.S)),
    data:=[]));
end);

#PPP

# new for 0.1! - ParentAttr - "for a D-class of a trans. semigroup"
#############################################################################

InstallMethod(ParentAttr, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], x-> x!.parent);

# new for 0.1! - PartialOrderOfDClasses - "for a transformation semigroup"
#############################################################################

InstallMethod(PartialOrderOfDClasses, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local d, n, out, gens, data_ht, o, j, data, i, x, f, orbits;

  d:=GreensDClasses(s); n:=NrDClasses(s); 
  out:= List([1..n], x-> EmptyPlist(n));
  gens:=Generators(s);  data_ht:=OrbitsOfKernels(s)!.data_ht;
  o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];
  orbits:=[o[1]!.orbits, o[2]!.orbits];

  for i in [1..n] do
    for x in gens do
      
      for f in RClassReps(d[i]) do
        data:=PreInOrbitsOfKernels(s, x * f, true, orbits)[2][2]{[1..6]};
        data[3]:=KernelOrbitSCCFromData(s, data, o[2])[1];
        AddSet(out[i], HTValue(data_ht, data));
      od;
      
      for f in LClassReps(d[i]) do
        data:=PreInOrbitsOfKernels(s, f * x, true, orbits)[2][2]{[1..6]};
        data[3]:=KernelOrbitSCCFromData(s, data, o[2])[1];
        AddSet(out[i], HTValue(data_ht, data));
      od;
    od;
  od;

  return out;
end);

# new for 0.1! - PreInOrbitsOfKernels - not a user function!
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

# new for 0.1! - PrintObj - "for orbits of kernels"
#############################################################################

InstallMethod(PrintObj, [IsOrbitsOfKernels], 
function(o)
  Print("<orbits of kernels; ", SizeOrbitsOfKernels(o!.s), " elements; ", 
   Length(o!.data), " D-classes>");
end);

# new for 0.1! - PrintObj - "for iterator of D-class reps"
#############################################################################

InstallMethod(PrintObj, [IsIteratorOfDClassReps], 
function(iter)
  local s;
  if IsBound(iter!.s) then 
    s:=iter!.s;

    Print( "<iterator of D-class reps, ", Length(OrbitsOfImages(s)!.data), 
    " candidates, ", SizeOrbitsOfKernels(s), " elements, ", 
      Length(OrbitsOfKernels(s)!.data), " D-classes>");
    return;
  fi;
  Print("<iterator of D-class reps>");
  return;
end);

# new for 0.1! - PrintObj - "for iterator of D-classes"
############################################################################

InstallMethod(PrintObj, [IsIteratorOfDClasses], 
function(iter)
  Print( "<iterator of D-classes>");
return;
end);

# new for 0.7! - PrintObj - "for an iterator of a D-class"
############################################################################

InstallMethod(PrintObj, [IsIteratorOfDClassElements],
function(iter)
  Print( "<iterator of D-class>");
return;
end);

#SSS

# new for 0.1! - SchutzenbergerGroup - "for a D-class of a trans. semigp."
############################################################################

InstallOtherMethod(SchutzenbergerGroup, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  local e;
  e:=d!.data[2];
  return KernelOrbitFromData(d!.parent, d!.data, d!.o)!.
   d_schutz[e[4]][e[5]][e[6]][2];
end);

# new for 0.1! - Size - "for a D-class of a trans. semigp."
#############################################################################

InstallOtherMethod(Size, "for a D-class of a trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  local r, l;

  r:=ImageOrbitSchutzGp(d); l:=KernelOrbitSchutzGp(d);

  return Size(r)*Length(ImageOrbitSCC(d))*Length(KernelOrbitSCC(d))*Size(l)/
   Size(SchutzenbergerGroup(d));
end);

# new for 0.2! - SizeDClassFromData
#############################################################################
# Usage: s = transformation semigroup; d = D-class data. 

InstallGlobalFunction(SizeDClassFromData, 
function(s, d)
  local r, l;

  r:=ImageOrbitSchutzGpFromData(s, d[1]);
  l:=KernelOrbitSchutzGpFromData(s, d[2]);
  return Size(r)*Length(ImageOrbitSCCFromData(s, d[1]))
    *Length(KernelOrbitSCCFromData(s, d[2]))*Size(l)/
    Size(DClassSchutzGpFromData(s,  d[2]));
end);

# new for 0.1! - SizeOrbitsOfKernels - not a user function.
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
