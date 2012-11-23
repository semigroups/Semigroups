



# Why does the following not work?
#DeclareCategoryCollections("IsBinaryRelation");

DeclareProperty("IsBinaryRelationCollection", IsCollection);
DeclareSynonymAttr("IsBinaryRelationSemigroup", IsSemigroup and
IsBinaryRelationCollection);
