###############################################################################
##
#W  freeband.gd
#Y  Copyright (C) 2013-15                                  Julius Jonusas
##
##  Licensing information can be found in the README file of this package.
##
###############################################################################

DeclareCategory("IsFreeBandElement", IsAssociativeElement);
DeclareCategoryCollections("IsFreeBandElement");
DeclareCategory("IsFreeBandCategory", IsSemigroup);
DeclareProperty("IsFreeBand", IsSemigroup);
DeclareSynonym("IsFreeBandSubsemigroup",
               IsSemigroup and IsFreeBandElementCollection);

DeclareGlobalFunction("FreeBand");

DeclareGlobalFunction("SEMIGROUPS_HashFunctionForFreeBandElements");
DeclareGlobalFunction("SEMIGROUPS_FreeBandElmToWord");
