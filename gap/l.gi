#############################################################################
##
#W  l.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

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
