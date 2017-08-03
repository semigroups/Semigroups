#############################################################################
##
#W  semieunit.gd
#Y  Copyright (C) 2016                                    Christopher Russell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsMcAlisterTripleSemigroupElement",
                IsAssociativeElement and IsMultiplicativeElementWithInverse);

DeclareRepresentation("IsMcAlisterTripleSemigroupElementRep",
                      IsMcAlisterTripleSemigroupElement
                      and IsPositionalObjectRep, 2);

DeclareCategoryCollections("IsMcAlisterTripleSemigroupElement");
DeclareSynonym("IsMTSE", IsMcAlisterTripleSemigroupElement);
DeclareSynonymAttr("IsMcAlisterTripleSemigroup",
                   IsInverseSemigroup and IsGeneratorsOfInverseSemigroup
                   and IsMcAlisterTripleSemigroupElementCollection
                   and IsWholeFamily and IsEnumerableSemigroupRep);
# TODO: Add description. See congpairs.gd
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

# Attributes for McAlister triple semigroups
DeclareAttribute("McAlisterTripleSemigroupGroup",
                 IsMcAlisterTripleSemigroup and IsWholeFamily);
DeclareAttribute("McAlisterTripleSemigroupAction",
                 IsMcAlisterTripleSemigroup and IsWholeFamily);
DeclareAttribute("McAlisterTripleSemigroupPartialOrder",
                 IsMcAlisterTripleSemigroup and IsWholeFamily);
DeclareAttribute("McAlisterTripleSemigroupSemilattice",
                 IsMcAlisterTripleSemigroup and IsWholeFamily);
DeclareAttribute("McAlisterTripleSemigroupElmList",
                 IsMcAlisterTripleSemigroup and IsWholeFamily);
DeclareAttribute("OneImmutable",
                 IsMcAlisterTripleSemigroup and IsWholeFamily and IsMonoid);

# Operations for relating to McAlister triple semigroups
DeclareAttribute("IsomorphismMcAlisterTripleSemigroup",
                IsSemigroup);

# Operations for creating McAlister triple semigroup elements
DeclareOperation("McAlisterTripleSemigroupElement",
                 [IsMcAlisterTripleSemigroup,
                 IsPosInt, IsMultiplicativeElementWithInverse]);
DeclareSynonym("MTSE", McAlisterTripleSemigroupElement);

# Operations for McAlister triple semigroup elements
DeclareAttribute("McAlisterTripleSemigroupElementParent",
                 IsMcAlisterTripleSemigroupElementRep);
DeclareSynonym("MTSEParent", McAlisterTripleSemigroupElementParent);
DeclareOperation("ELM_LIST", [IsMcAlisterTripleSemigroupElementRep, IsPosInt]);

# Inverse semigroup methods
DeclareAttribute("EUnitaryInverseCover", IsSemigroup);
DeclareProperty("IsFInverseSemigroup", IsSemigroup);
DeclareProperty("IsFInverseMonoid", IsSemigroup);
