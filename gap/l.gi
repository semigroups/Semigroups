#############################################################################
##
#W  l.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#############################################################################
# Notes

#GGG

# new for 0.1! - GreensHClasses - "for an L-class of a trans. semigp."
#############################################################################

InstallOtherMethod(GreensHClasses, "for an L-class of a trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(l)
  local s, d, o, m, data, out, f, h, i;

  s:=l!.parent; d:=DClassOfLClass(l); o:=l!.o; 
  m:=NrHClasses(l); data:=HClassRepsData(l); 
  
  out:=EmptyPlist(m);

  for i in [1..m] do 
    
    if HasHClassReps(l) then 
      f:=HClassReps(l)[i];
    else
      f:=HClassRepFromData(s, data[i], o);
    fi;

    h:=CreateHClass(s, data[i], o, f);
    SetLClassOfHClass(h, l); SetDClassOfHClass(h, d);
    out[i]:=h;
  od;

  return out;
end);

# new for 0.5! - GreensHClassOfElementNC - "for an L-class and trans."
#############################################################################
# JDM is there a better way? If not clean it up!

InstallOtherMethod(GreensHClassOfElementNC, "for an L-class and trans.", 
[IsGreensLClass and IsGreensClassOfTransSemigp, IsTransformation],
function(l, f)
  local data, l_ker, g, rep, p, cosets, h;

  data:=ShallowCopy(l!.data);
  l_ker:=Position(KernelOrbit(l), CanonicalTransSameKernel(f));
  data[2][3]:=l_ker;
  
  g:=KernelOrbitRels(l)[l_ker][2]*f*(l!.data[3]/
   ImageOrbitPerms(l)[data[1][3]])^-1;
  
  rep:=DClassRepFromData(ParentAttr(l), l!.data, l!.o);
  p:=KerRightToImgLeft(l)^-1;
  cosets:=KernelOrbitCosets(l);
  
  data[4]:=cosets[PositionCanonical(cosets, PermLeftQuoTransformationNC(rep,
   g)^p)];

  h:=CreateHClass(ParentAttr(l), data, l!.o,
  HClassRepFromData(ParentAttr(l), data, l!.o));
  SetLClassOfHClass(h, l);
  return h;
end);

#HHH

# new for 0.1! - HClassReps - "for an L-class of a trans. semigp."
#############################################################################

InstallOtherMethod(HClassReps, "for an L-class of a trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
  local f, cosets, rels, scc, out, k, i, j;

  if HasGreensHClasses(l) then 
    return List(GreensHClasses(l), Representative);
  fi;

  f:=l!.rep; cosets:=KernelOrbitCosets(l); rels:=KernelOrbitRels(l);
  scc:=KernelOrbitSCC(l);

  out:=EmptyPlist(Length(scc)*Length(cosets));
  
  if not HasNrHClasses(l) then 
    SetNrHClasses(l, Length(scc)*Length(cosets));
  fi;

  k:=0;

  for i in scc do 
    i:=rels[i][1]*f;
    for j in cosets do 
      k:=k+1;
      out[k]:=i*j;
    od;
  od;

  return out;
end);

# new for 0.1! - HClassRepsData - "for L-class of trans. semigp."
#############################################################################

InstallOtherMethod(HClassRepsData, "for L-class of trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
  local f, scc, cosets, out, k, data, i, j;

  f:= l!.rep; scc:=KernelOrbitSCC(l);
  cosets:=KernelOrbitCosets(l);

  out:=EmptyPlist(Length(scc)*Length(cosets));

  if not HasNrHClasses(l) then 
    SetNrHClasses(l, Length(scc)*Length(cosets));
  fi;

  k:=0; 
  data:=[StructuralCopy(l!.data[1]), StructuralCopy(l!.data[2]),
  StructuralCopy(l!.data[3])]; 
  for i in scc do 
    for j in cosets do 
      k:=k+1;
      out[k]:=StructuralCopy(data);
      out[k][2][3]:=i; out[k][3]:=data[3]; out[k][4]:=j; 
    od;
  od;

  return out;
end);

#NNN

# new for 0.1! - NrHClasses - "for an L-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(NrHClasses, "for an L-class of a trans. semigroup", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
  l-> NrRClasses(DClassOfLClass(l)));

#EOF
