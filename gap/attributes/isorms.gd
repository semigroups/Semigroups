############################################################################
##
##  isorms.gd
##  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsRMSIsoByTriple", IsGeneralMapping and IsSPGeneralMapping
                                    and IsTotal and IsSingleValued and
                                    IsInjective and IsSurjective and
                                    IsAttributeStoringRep);
DeclareCategory("IsRZMSIsoByTriple", IsGeneralMapping and IsSPGeneralMapping
                                     and IsTotal and IsSingleValued and
                                     IsInjective and IsSurjective and
                                     IsAttributeStoringRep);

DeclareOperation("RMSIsoByTriple", [IsReesMatrixSemigroup,
                                    IsReesMatrixSemigroup,
                                    IsDenseList]);
DeclareOperation("RZMSIsoByTriple", [IsReesZeroMatrixSemigroup,
                                     IsReesZeroMatrixSemigroup,
                                     IsDenseList]);

DeclareOperation("RMSIsoByTripleNC", [IsReesMatrixSemigroup,
                                      IsReesMatrixSemigroup,
                                      IsDenseList]);
DeclareOperation("RZMSIsoByTripleNC", [IsReesZeroMatrixSemigroup,
                                       IsReesZeroMatrixSemigroup,
                                       IsDenseList]);

DeclareOperation("ELM_LIST", [IsRMSIsoByTriple, IsPosInt]);
DeclareOperation("ELM_LIST", [IsRZMSIsoByTriple, IsPosInt]);

DeclareProperty("IsAutomorphismGroupOfRMSOrRZMS", IsGroup and IsFinite);

DeclareAttribute("IsomorphismReesMatrixSemigroupOverPermGroup", IsSemigroup);
DeclareAttribute("IsomorphismReesZeroMatrixSemigroupOverPermGroup",
                 IsSemigroup);
