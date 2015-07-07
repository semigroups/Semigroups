#############################################################################
##
#W  star.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsAssociativeElementWithStar", IsAssociativeElement);
DeclareCategoryCollections("IsAssociativeElementWithStar");
DeclareOperation("StarOp", [IsAssociativeElementWithStar]);
DeclareAttribute("Star", IsAssociativeElementWithStar);

DeclareSynonym("IsStarSemigroup",
               IsSemigroup and IsAssociativeElementWithStarCollection);
DeclareSynonym("IsRegularStarSemigroup",
               IsRegularSemigroup and IsAssociativeElementWithStarCollection);

DeclareOperation("InverseOp", [IsAssociativeElementWithStar]);
