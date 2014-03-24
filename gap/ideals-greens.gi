############################################################################# 
## 
#W  ideals-greens.gi
#Y  Copyright (C) 2013-14                                 James D. Mitchell
##                                                           Julius Jonusas
## 
##  Licensing information can be found in the README file of this package. 
## 
############################################################################# 
##

InstallMethod(NrDClasses, "for an acting semigroup ideal",
[IsActingSemigroupWithInverseOp and IsSemigroupIdeal],
function(I)
  return Length(OrbSCC(LambdaOrb(I)))-1;
end);

# 

InstallMethod(NrDClasses, "for an acting semigroup ideal",
[IsActingSemigroup and IsSemigroupIdeal],
function(I)
  Enumerate(SemigroupData(I));
  return Length(SemigroupData(I)!.dorbit);
end);

#

InstallMethod(GreensDClasses, "for an acting semigroup ideal",
[IsActingSemigroup and IsSemigroupIdeal],
function(I)
  Enumerate(SemigroupData(I));
  return SemigroupData(I)!.dorbit;
end);

#

InstallMethod(PartialOrderOfDClasses, "for an acting semigroup ideal", 
[IsActingSemigroup and IsSemigroupIdeal],
function(I)
  local data;

  data:=SemigroupData(I);
  Enumerate(data);
  return data!.poset;
end);

#

InstallMethod(DClassReps, "for an acting semigroup ideal", 
[IsActingSemigroup and IsSemigroupIdeal],
function(I)
  local data;

  data:=SemigroupData(I);
  Enumerate(data);
  return List(data!.dorbit, Representative);
end);

#

InstallMethod(NrRegularDClasses, "for an acting semigroup ideal",
[IsActingSemigroup and IsSemigroupIdeal],
function(I)
  return Number(GreensDClasses(I), IsRegularDClass);
end);

