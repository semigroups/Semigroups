#############################################################################
##
##  semigroups/semieunit.gd
##  Copyright (C) 2016-2022                            Christopher Russell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsMcAlisterTripleSemigroupElement",
                IsAssociativeElement and IsMultiplicativeElementWithInverse);

# This is a representation for McAlister triple semigroup elements, which are
# created via the function McAlisterTripleSemigroupElement.
#
# If x belongs to the representation IsMcAlisterTripleElementRep, then the
# components are:
#
#   x![1]:   The McAlister triple semigroup which this element belongs to
#
#   x![2]:   A vertex of the McAlisterTripleSemigroupSemilattice of x![1]
#
#   x![3]:   An element of the McAlisterTripleSemigroupGroup of x![1]

DeclareRepresentation("IsMcAlisterTripleSemigroupElementRep",
                      IsMcAlisterTripleSemigroupElement
                      and IsPositionalObjectRep, 3);

DeclareCategoryCollections("IsMcAlisterTripleSemigroupElement");
DeclareSynonymAttr("IsMTSE", IsMcAlisterTripleSemigroupElement);
DeclareSynonymAttr("IsMcAlisterTripleSemigroup",
                   IsInverseSemigroup and IsGeneratorsOfInverseSemigroup
                   and IsMcAlisterTripleSemigroupElementCollection
                   and IsWholeFamily and IsActingSemigroup);
DeclareSynonymAttr("IsMTS", IsMcAlisterTripleSemigroup);
DeclareSynonym("IsMcAlisterTripleSubsemigroup",
IsMcAlisterTripleSemigroupElementCollection and IsSemigroup);

InstallTrueMethod(IsFinite, IsMcAlisterTripleSubsemigroup);

# This is a representation for McAlister triple semigroup, which are
# created via the function McAlisterTripleSemigroup.
#
# The attributes stored upon creation are:
#
#   McAlisterTripleSemigroupGroup
#   McAlisterTripleSemigroupPartialOrder
#   McAlisterTripleSemigroupSemilattice
#   McAlisterTripleSemigroupAction
#   McAlisterTripleSemigroupUnderlyingAction
#   McAlisterTripleSemigroupActionHomomorphism
#   GeneratorsOfSemigroup
#
# their purpose is described in the section of the user manual on McAlister
# triple semigroups.

DeclareRepresentation("IsMcAlisterTripleSemigroupDefaultRep",
                      IsMcAlisterTripleSemigroup and IsAttributeStoringRep,
                      []);

InstallTrueMethod(IsGeneratorsOfInverseSemigroup,
                  IsMcAlisterTripleSemigroupElementCollection);

# Operations for creating McAlister triple semigroups
DeclareOperation("McAlisterTripleSemigroup",
                 [IsGroup, IsDigraph, IsDigraph, IsFunction]);
DeclareOperation("McAlisterTripleSemigroup",
                 [IsGroup, IsDigraph, IsHomogeneousList, IsFunction]);
DeclareOperation("McAlisterTripleSemigroup",
                 [IsPermGroup, IsDigraph, IsDigraph]);
DeclareOperation("McAlisterTripleSemigroup",
                 [IsPermGroup, IsDigraph, IsHomogeneousList]);

# Attributes for McAlister triple subsemigroups
DeclareAttribute("McAlisterTripleSemigroupGroup",
                 IsMcAlisterTripleSubsemigroup);
DeclareSynonymAttr("MTSGroup", McAlisterTripleSemigroupGroup);
DeclareAttribute("McAlisterTripleSemigroupAction",
                 IsMcAlisterTripleSubsemigroup);
DeclareSynonymAttr("MTSAction", McAlisterTripleSemigroupAction);
DeclareAttribute("McAlisterTripleSemigroupPartialOrder",
                 IsMcAlisterTripleSubsemigroup);
DeclareSynonymAttr("MTSPartialOrder", McAlisterTripleSemigroupPartialOrder);
DeclareAttribute("McAlisterTripleSemigroupSemilattice",
                 IsMcAlisterTripleSubsemigroup);
DeclareSynonymAttr("MTSSemilattice", McAlisterTripleSemigroupSemilattice);
DeclareAttribute("McAlisterTripleSemigroupActionHomomorphism",
                 IsMcAlisterTripleSubsemigroup);
DeclareSynonymAttr("MTSActionHomomorphism",
                 McAlisterTripleSemigroupActionHomomorphism);
DeclareAttribute("McAlisterTripleSemigroupUnderlyingAction",
                 IsMcAlisterTripleSubsemigroup);
DeclareSynonymAttr("MTSUnderlyingAction",
                   McAlisterTripleSemigroupUnderlyingAction);
DeclareAttribute("McAlisterTripleSemigroupSemilatticeVertexLabelInverseMap",
                 IsMcAlisterTripleSubsemigroup);
DeclareSynonymAttr("MTSSemilatticeVertexLabelInverseMap",
                   McAlisterTripleSemigroupSemilatticeVertexLabelInverseMap);
DeclareAttribute("OneImmutable",
                 IsMcAlisterTripleSemigroupElementCollection);
DeclareAttribute("McAlisterTripleSemigroupComponents",
                 IsMcAlisterTripleSubsemigroup);
DeclareSynonymAttr("MTSComponents",
                   McAlisterTripleSemigroupComponents);
DeclareAttribute("McAlisterTripleSemigroupQuotientDigraph",
                 IsMcAlisterTripleSubsemigroup);
DeclareSynonymAttr("MTSQuotientDigraph",
                   McAlisterTripleSemigroupQuotientDigraph);

# Operations for creating McAlister triple semigroup elements
DeclareOperation("McAlisterTripleSemigroupElement",
                 [IsMcAlisterTripleSemigroup,
                 IsPosInt, IsMultiplicativeElementWithInverse]);
DeclareSynonym("MTSE", McAlisterTripleSemigroupElement);

# Operations for McAlister triple semigroup elements
DeclareAttribute("McAlisterTripleSemigroupElementParent",
                 IsMcAlisterTripleSemigroupElementRep);
DeclareSynonymAttr("MTSEParent", McAlisterTripleSemigroupElementParent);
DeclareOperation("ELM_LIST", [IsMcAlisterTripleSemigroupElementRep, IsPosInt]);

# Inverse semigroup methods
DeclareAttribute("EUnitaryInverseCover", IsSemigroup);
DeclareProperty("IsFInverseSemigroup", IsSemigroup);
DeclareProperty("IsFInverseMonoid", IsSemigroup);
