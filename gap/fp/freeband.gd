###############################################################################
##
##  fp/freeband.gd
##  Copyright (C) 2013-2022                                 Julius Jonusas
##
##  Licensing information can be found in the README file of this package.
##
###############################################################################

DeclareCategory("IsFreeBandElement", IsAssociativeElement);
DeclareCategoryCollections("IsFreeBandElement");
DeclareCategory("IsFreeBandCategory", IsSemigroup and IsBand);
DeclareProperty("IsFreeBand", IsSemigroup);
DeclareSynonym("IsFreeBandSubsemigroup",
               IsSemigroup and IsFreeBandElementCollection);

DeclareGlobalFunction("FreeBand");
DeclareAttribute("ContentOfFreeBandElement", IsFreeBandElement);
DeclareAttribute("ContentOfFreeBandElementCollection",
                 IsFreeBandElementCollection);

DeclareOperation("EqualInFreeBand", [IsList, IsList]);
