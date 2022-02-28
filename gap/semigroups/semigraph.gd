#############################################################################
##
##  semigroups/semigraph.gd
##  Copyright (C) 2014-2022               Zak Mesyan and James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("GraphInverseSemigroup", [IsDigraph]);

DeclareCategory("IsGraphInverseSemigroupElement", IsAssociativeElement);
DeclareCategoryCollections("IsGraphInverseSemigroupElement");

DeclareSynonymAttr("IsGraphInverseSubsemigroup",
                   IsSemigroup and
                   IsGraphInverseSemigroupElementCollection);

DeclareSynonymAttr("IsGraphInverseSemigroup",
                   IsGraphInverseSubsemigroup and IsWholeFamily);

DeclareAttribute("GraphOfGraphInverseSemigroup", IsGraphInverseSemigroup);
DeclareAttribute("Range", IsGraphInverseSemigroupElement);
DeclareAttribute("Source", IsGraphInverseSemigroupElement);

DeclareOperation("IsVertex", [IsGraphInverseSemigroupElement]);

InstallTrueMethod(IsGeneratorsOfInverseSemigroup,
                  IsGraphInverseSemigroupElementCollection);

# The following are required because we use Zero in an unintended way (it's
# supposed to be an additive zero).
DeclareOperation("ZeroOp", [IsGraphInverseSemigroupElement]);
DeclareProperty("IsZero", IsGraphInverseSemigroupElement);

DeclareOperation("IndexOfVertexOfGraphInverseSemigroup",
                 [IsGraphInverseSemigroupElement]);
DeclareAttribute("VerticesOfGraphInverseSemigroup",
                 IsGraphInverseSemigroup);
