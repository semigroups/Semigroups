###########################################################################
##
##  cong.gd
##  Copyright (C) 2021                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("CanComputeCppCongruence", IsAnyCongruenceCategory);

DeclareGlobalFunction("CppCongruence");

DeclareOperation("CongruenceWordToClassIndex",
                 [CanComputeCppCongruence, IsHomogeneousList]);
DeclareOperation("CongruenceWordToClassIndex",
                 [CanComputeCppCongruence, IsMultiplicativeElement]);

DeclareOperation("CongruenceLessNC",
                 [CanComputeCppCongruence,
                  IsMultiplicativeElement,
                  IsMultiplicativeElement]);
