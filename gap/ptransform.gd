


# PartialPerms

DeclareGlobalFunction("AsPermOfDomRan");

DeclareRepresentation( "IsPartialPerm", 
IsPositionalObjectRep and IsMultiplicativeElementWithOne and
IsAttributeStoringRep and IsAssociativeElement, [1]);

DeclareAttribute("Dom", IsPartialPerm);
DeclareAttribute("Ran", IsPartialPerm);

DeclareCategoryCollections("IsPartialPerm");

BindGlobal("PartialPermFamily", NewFamily("PartialPermFamily", 
IsPartialPerm));

BindGlobal("PartialPermType", NewType(PartialPermFamily, 
IsPartialPerm));

DeclareGlobalFunction("PartialPermNC");
DeclareGlobalFunction("RandomPartialPerm");

# Partial transformations

DeclareRepresentation( "IsPartialTransformation",
IsPositionalObjectRep and IsMultiplicativeElementWithOne and 
IsAttributeStoringRep and IsAssociativeElement, [2] );

DeclareCategoryCollections("IsPartialTransformation");

DeclareSynonymAttr("IsPartialTransformationSemigroup", IsSemigroup and IsPartialTransformationCollection);

DeclareGlobalFunction("PartialTransformation");
DeclareGlobalFunction("PartialTransformationNC");

BindGlobal("PartialTransformationFamily", NewFamily("PartialTransformationFamily", 
IsPartialTransformation));

BindGlobal("PartialTransformationType", NewType(PartialTransformationFamily, 
IsPartialTransformation));

BindGlobal("PartialTransformationSemigroupsType", 
NewType(CollectionsFamily(PartialTransformationFamily), 
IsPartialTransformationSemigroup and IsAttributeStoringRep));

##################


DeclareProperty("IsPartialTransformationMonoid", 
IsPartialTransformationSemigroup);

DeclareAttribute("DegreeOfPartialTransformationSemigroup",
	IsPartialTransformationSemigroup);
	
DeclareOperation("AsPartialTransformation", [IsTransformation]);
DeclareOperation("AsPartialTransformationNC", [IsTransformation]);

DeclareAttribute("DomainOfPartialTransformation", IsPartialTransformation);
