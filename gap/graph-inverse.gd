#############################################################################
##
#W  graph-inverse.gd
#Y  Copyright (C) 2014                    Zak Mesyan and James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("GraphInverseSemigroup", [IsDigraph]);

DeclareCategory( "IsGraphInverseSemigroupElement", IsAssociativeElement);
DeclareCategoryCollections( "IsGraphInverseSemigroupElement");

DeclareSynonymAttr("IsGraphInverseSubsemigroup",
  IsInverseSemigroup and IsSemigroup and IsGraphInverseSemigroupElementCollection);

DeclareSynonymAttr("IsGraphInverseSemigroup", IsGraphInverseSubsemigroup and
IsWholeFamily); 

DeclareAttribute("GraphOfGraphInverseSemigroup", IsGraphInverseSemigroup);
DeclareAttribute("Range", IsGraphInverseSemigroupElement);
DeclareAttribute("Source", IsGraphInverseSemigroupElement);

DeclareOperation("IsVertex", [IsGraphInverseSemigroupElement]);

# JDM: why are these required?
DeclareAttribute("MultiplicativeZero", IsGraphInverseSemigroup);
DeclareOperation("ZeroOp", [IsGraphInverseSemigroupElement]);
DeclareProperty("IsZero", IsGraphInverseSemigroupElement);
