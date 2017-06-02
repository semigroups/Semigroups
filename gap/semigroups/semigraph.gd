#############################################################################
##
#W  semigraph.gd
#Y  Copyright (C) 2014-15                 Zak Mesyan and James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("GraphInverseSemigroup", [IsDigraph]);

DeclareCategory("IsGraphInverseSemigroupElement", IsAssociativeElement);
DeclareCategoryCollections("IsGraphInverseSemigroupElement");

DeclareSynonymAttr("IsGraphInverseSubsemigroup",
                   IsInverseSemigroup and IsSemigroup and
                   IsGraphInverseSemigroupElementCollection);

DeclareSynonymAttr("IsGraphInverseSemigroup",
                   IsGraphInverseSubsemigroup and IsWholeFamily);

DeclareAttribute("GraphOfGraphInverseSemigroup", IsGraphInverseSemigroup);
DeclareAttribute("Range", IsGraphInverseSemigroupElement);
DeclareAttribute("Source", IsGraphInverseSemigroupElement);

DeclareOperation("IsVertex", [IsGraphInverseSemigroupElement]);

InstallTrueMethod(IsGeneratorsOfInverseSemigroup,
                  IsGraphInverseSemigroupElementCollection);

# JDM: why are these required?
DeclareAttribute("MultiplicativeZero", IsGraphInverseSemigroup);
DeclareOperation("ZeroOp", [IsGraphInverseSemigroupElement]);
DeclareProperty("IsZero", IsGraphInverseSemigroupElement);
