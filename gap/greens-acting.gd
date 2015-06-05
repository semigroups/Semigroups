#############################################################################
##
#W  greens-acting.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

# Green's relations, classes, etc for acting semigroups.

DeclareAttribute("LambdaOrb", IsActingSemigroupGreensClass, "mutable");
DeclareAttribute("RhoOrb", IsActingSemigroupGreensClass, "mutable");

DeclareAttribute("LambdaOrbSCC", IsActingSemigroupGreensClass);
DeclareAttribute("LambdaOrbSCCIndex", IsActingSemigroupGreensClass);

DeclareAttribute("RhoOrbSCC", IsActingSemigroupGreensClass);
DeclareAttribute("RhoOrbSCCIndex", IsActingSemigroupGreensClass);

DeclareAttribute("LambdaCosets", IsActingSemigroupGreensClass);
DeclareAttribute("RhoCosets", IsActingSemigroupGreensClass);

DeclareAttribute("SemigroupDataIndex", IsActingSemigroupGreensClass);

# This category should not be a property.  The reason is that if a Green's
# D-class is not created as an InverseOpClass, then its rep is rectified wrt
# Lambda and Rho, but if it is subsequently considered as an InverseOpClass,
# then the Rho value of the rep will be wrong (i.e. not nec. in the first
# position of the scc of the LambdaOrb containing its Rho-value, but in the
# first position of the scc of the RhoOrb containing it). Lots of methods in
# this file use the fact that the rep of an inverse-op D-class has the
# representative with Lambda and Rho values in the first position of the scc of
# the *LambdaOrb* containing them.  The same comments apply to inverse-op
# L-classes, their reps must have their Rho value in the first pos of the scc
# of the *LambdaOrb* containing the Rho value. So, an L-class created as a
# non-inverse-op class, which later becomes an inverse-op class will have the
# wrong representative. Therefore we don't allow classes to change from
# non-inverse-op to inverse-op.  representative

DeclareCategory("IsInverseOpClass", IsGreensClass);

DeclareOperation("GreensDClassOfElementNC",
                 [IsActingSemigroup, IsAssociativeElement, IsBool]);
DeclareOperation("GreensLClassOfElementNC",
                 [IsActingSemigroup, IsAssociativeElement, IsBool]);
DeclareOperation("GreensRClassOfElementNC",
                 [IsActingSemigroup, IsAssociativeElement, IsBool]);
DeclareOperation("GreensHClassOfElementNC",
                 [IsActingSemigroup, IsAssociativeElement, IsBool]);

DeclareOperation("GreensDClassOfElementNC",
                 [IsGreensClass, IsAssociativeElement, IsBool]);
DeclareOperation("GreensLClassOfElementNC",
                 [IsGreensClass, IsAssociativeElement, IsBool]);
DeclareOperation("GreensRClassOfElementNC",
                 [IsGreensClass, IsAssociativeElement, IsBool]);
DeclareOperation("GreensHClassOfElementNC",
                 [IsGreensClass, IsAssociativeElement, IsBool]);

# the following is required in green-acting and greens-inverse.
DeclareGlobalFunction("SEMIGROUPS_InverseRectifyRho");
