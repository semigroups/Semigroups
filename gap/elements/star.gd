#############################################################################
##
##  elements/star.gd
##  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsAssociativeElementWithStar", IsAssociativeElement);
DeclareCategoryCollections("IsAssociativeElementWithStar");
DeclareOperation("StarOp", [IsAssociativeElementWithStar]);
DeclareAttribute("Star", IsAssociativeElementWithStar);

DeclareProperty("IsStarSemigroup",
                IsSemigroup and IsAssociativeElementWithStarCollection);
DeclareSynonym("IsRegularStarSemigroup",
               IsRegularSemigroup and IsStarSemigroup);

DeclareOperation("InverseOp", [IsAssociativeElementWithStar]);
