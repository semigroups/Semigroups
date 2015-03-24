############################################################################
##
#W  grpmatobj.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##                                                       Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Unfortunately, matrix obj groups are also matrix groups, since IsMatrixGroup
# is just a synonym for IsRingElementCollCollColl and IsGroup. Also it is not
# possible to create these objects using the function Group, use Semigroup
# instead. 

#FIXME move this
DeclareOperation("AsMatrix", [IsMatrixObj]);

DeclareSynonym("SEMIGROUPS_IsMatrixObjGroup",
               IsGroup and IsMatrixSemigroup);

DeclareAttribute("BaseDomain", SEMIGROUPS_IsMatrixObjGroup);
DeclareAttribute("IsomorphismMatrixGroup", SEMIGROUPS_IsMatrixObjGroup);
DeclareAttribute("IsomorphismMatrixObjGroup", IsMatrixGroup);
DeclareAttribute("AsMatrixGroup", SEMIGROUPS_IsMatrixObjGroup);

DeclareOperation("ClosureGroupNC", 
                 [SEMIGROUPS_IsMatrixObjGroup, IsCollection]);

DeclareOperation("\^", [SEMIGROUPS_IsMatrixObjGroup, IsMatrixObj]);

BindGlobal("SEMIGROUPS_MatrixObjGroupRankIncrement" , 400);

#InstallTrueMethod(IsMagmaWithInverses, CategoryCollections(IsMatrixObj));

