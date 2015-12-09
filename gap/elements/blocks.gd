############################################################################
##
#W  blocks.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsBlocks", IsList);
DeclareCategoryCollections("IsBlocks");

BindGlobal("BlocksFamily",
           NewFamily("BlocksFamily", IsBlocks, CanEasilySortElements,
                     CanEasilySortElements));

BindGlobal("BlocksType",
           NewType(BlocksFamily, IsBlocks and IsComponentObjectRep and
                   IsAttributeStoringRep));

DeclareGlobalFunction("BlocksNC");

DeclareAttribute("ProjectionFromBlocks", IsBlocks);
DeclareAttribute("NrBlocks", IsBlocks);
DeclareAttribute("RankOfBlocks", IsBlocks);
DeclareAttribute("NrTransverseBlocks", IsBlocks);
DeclareAttribute("DegreeOfBlocks", IsBlocks);
DeclareAttribute("ExtRepOfBlocks", IsBlocks);
