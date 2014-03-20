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
[IsActingSemigroup and IsSemigroupIdeal],
function(I)
  Enumerate(SemigroupData(I));
  return Length(SemigroupData(I)!.dorbit);
end);

