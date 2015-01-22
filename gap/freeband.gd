################################################################################
##
#W  freeband.gd
#Y  Copyright (C) 2013-15                                  Julius Jonusas
##
##  Licensing information can be foundin the README file of this package.
##
################################################################################

DeclareCategory("IsFreeBandElement", IsAssociativeElement);
DeclareCategoryCollections("IsFreeBandElement");
DeclareCategory("IsFreeBandCategory", IsSemigroup);
DeclareProperty("IsFreeBand", IsSemigroup);
DeclareSynonym("IsFreeBandSubsemigroup", IsSemigroup and
IsFreeBandElementCollection);
InstallTrueMethod(IsFinite, IsFreeBandSubsemigroup);

DeclareGlobalFunction("FreeBand");

DeclareGlobalFunction("SEMIGROUPS_HashFunctionForFreeBandElements");
