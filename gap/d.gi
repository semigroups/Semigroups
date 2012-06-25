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

#NNN

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

#PPP

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

#EOF
