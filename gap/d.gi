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


#EOF
