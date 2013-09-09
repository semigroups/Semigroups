############################################################################
##
#W  acting.gd
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# hash table of all valid lambda-rho values found so far, HTValue of
# LambdaRhoHT points to where the existing R-class reps with same lambda-rho
# value are in SemigroupData(s).reps. 
DeclareAttribute("LambdaRhoHT", IsActingSemigroup, "mutable");

# LambdaRhoLookup(d)[i]=j if orbit[j][4] in reps[i] (orbit[j][4] is one of the 
# R-reps of the D-class d)
# and LambdaRhoLookup(d) is only bound for those indices i where 
# there is an R-rep in the scc of the D-class in reps[i] 
DeclareAttribute("LambdaRhoLookup", IsGreensDClass and
 IsActingSemigroupGreensClass);

DeclareAttribute("SemigroupData", IsActingSemigroup, "mutable");
DeclareGlobalFunction("SizeOfSemigroupData");
DeclareCategory("IsSemigroupData", IsList);

DeclareOperation("Enumerate", [IsSemigroupData]);
DeclareOperation("Enumerate", [IsSemigroupData, IsCyclotomic]);
DeclareOperation("Enumerate", [IsSemigroupData, IsCyclotomic, IsFunction]);
DeclareOperation("OrbitGraphAsSets", [IsSemigroupData]);
DeclareOperation("PositionOfFound", [IsSemigroupData]);

#EOF
