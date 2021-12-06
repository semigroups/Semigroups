############################################################################
##
##  cong.gd
##  Copyright (C) 2015                                   Michael C. Young
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains declarations for functions, operations and attributes of
## semigroup congruences.  Methods for most of these are implemented for
## specific types of congruence in the following files:
##
##       conginv.gi     - Inverse semigroups
##       congpairs.gi   - Congruences with generating pairs
##       congrees.gi    - Rees congruences
##       congrms.gi     - (0-)simple Rees matrix semigroups
##       congsimple.gi  - (0-)simple semigroups
##       conguniv.gi    - Universal congruences
##
## Some general functions are also implemented in cong.gi
##

# Possibly missing:
# - ImagesElm (implemented in conginv.gi)

########################################################################
# Congruences
########################################################################

# These are equivalence relations
InstallTrueMethod(IsEquivalenceRelation, IsLeftSemigroupCongruence);
InstallTrueMethod(IsEquivalenceRelation, IsRightSemigroupCongruence);

# Flexible functions for creating congruences
DeclareGlobalFunction("SemigroupCongruence");
DeclareGlobalFunction("LeftSemigroupCongruence");
DeclareGlobalFunction("RightSemigroupCongruence");

# Properties of congruences
DeclareProperty("IsRightSemigroupCongruence", IsLeftSemigroupCongruence);
DeclareProperty("IsLeftSemigroupCongruence", IsRightSemigroupCongruence);
DeclareProperty("IsSemigroupCongruence", IsLeftSemigroupCongruence);
DeclareProperty("IsSemigroupCongruence", IsRightSemigroupCongruence);

# Attributes of congruences
# EquivalenceRelationPartition is implemented in libsemigroups/cong.gi
DeclareAttribute("NonTrivialEquivalenceClasses", IsEquivalenceRelation);
DeclareAttribute("EquivalenceRelationLookup", IsEquivalenceRelation);
DeclareAttribute("EquivalenceRelationCanonicalLookup", IsEquivalenceRelation);
DeclareAttribute("NrEquivalenceClasses", IsEquivalenceRelation);
DeclareAttribute("EquivalenceRelationCanonicalPartition",
                 IsEquivalenceRelation);

# TODO(now): shouldn't the below be in congpairs.gd

DeclareSynonym("GeneratingPairsOfLeftSemigroupCongruence",
               GeneratingPairsOfLeftMagmaCongruence);
DeclareSynonym("GeneratingPairsOfRightSemigroupCongruence",
               GeneratingPairsOfRightMagmaCongruence);

# No-checks version of the "\in" operation
DeclareOperation("CongruenceTestMembershipNC", [IsEquivalenceRelation,
                                                IsMultiplicativeElement,
                                                IsMultiplicativeElement]);

# Algebraic operators
DeclareOperation("JoinLeftSemigroupCongruences",
                 [IsLeftSemigroupCongruence, IsLeftSemigroupCongruence]);
DeclareOperation("JoinRightSemigroupCongruences",
                 [IsRightSemigroupCongruence, IsRightSemigroupCongruence]);

# Comparison operators
DeclareOperation("IsSubrelation",
                 [IsEquivalenceRelation, IsEquivalenceRelation]);
DeclareOperation("IsSuperrelation",
                 [IsEquivalenceRelation, IsEquivalenceRelation]);

########################################################################
# Congruence classes
########################################################################

# IsCongruenceClass is declared in gap/lib/mgmcong.gd:140
DeclareCategory("IsLeftCongruenceClass",
                IsEquivalenceClass and IsAttributeStoringRep);
DeclareCategory("IsRightCongruenceClass",
                IsEquivalenceClass and IsAttributeStoringRep);

# Actions
DeclareOperation("OnLeftCongruenceClasses",
                 [IsLeftCongruenceClass, IsMultiplicativeElement]);
DeclareOperation("OnRightCongruenceClasses",
                 [IsRightCongruenceClass, IsMultiplicativeElement]);

########################################################################
# Congruence lattices and related
########################################################################

DeclareAttribute("CongruencesOfSemigroup", IsSemigroup);
DeclareAttribute("LeftCongruencesOfSemigroup", IsSemigroup);
DeclareAttribute("RightCongruencesOfSemigroup", IsSemigroup);

DeclareAttribute("MinimalCongruencesOfSemigroup", IsSemigroup);
DeclareAttribute("MinimalLeftCongruencesOfSemigroup", IsSemigroup);
DeclareAttribute("MinimalRightCongruencesOfSemigroup", IsSemigroup);

DeclareOperation("MinimalCongruencesOfSemigroup",
                 [IsSemigroup, IsMultiplicativeElementCollection]);
DeclareOperation("MinimalLeftCongruencesOfSemigroup",
                 [IsSemigroup, IsMultiplicativeElementCollection]);
DeclareOperation("MinimalRightCongruencesOfSemigroup",
                 [IsSemigroup, IsMultiplicativeElementCollection]);

DeclareAttribute("PrincipalCongruencesOfSemigroup", IsSemigroup);
DeclareAttribute("PrincipalLeftCongruencesOfSemigroup", IsSemigroup);
DeclareAttribute("PrincipalRightCongruencesOfSemigroup", IsSemigroup);

DeclareOperation("PrincipalCongruencesOfSemigroup",
                 [IsSemigroup, IsMultiplicativeElementCollection]);
DeclareOperation("PrincipalLeftCongruencesOfSemigroup",
                 [IsSemigroup, IsMultiplicativeElementCollection]);
DeclareOperation("PrincipalRightCongruencesOfSemigroup",
                 [IsSemigroup, IsMultiplicativeElementCollection]);
