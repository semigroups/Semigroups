############################################################################
##
#W  translat.gd
#Y  Copyright (C) 2015                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory( "IsTranslationsSemigroupElement", IsAssociativeElement);
DeclareCategory( "IsLeftTranslationsSemigroupElement", IsTranslationsSemigroupElement);
DeclareCategory( "IsRightTranslationsSemigroupElement", IsTranslationsSemigroupElement);
DeclareCategory( "IsTranslationalHullElement", IsAssociativeElement);


DeclareCategoryCollections( "IsTranslationsSemigroupElement");
DeclareCategoryCollections( "IsLeftTranslationsSemigroupElement");
DeclareCategoryCollections( "IsRightTranslationsSemigroupElement");
DeclareCategoryCollections( "IsTranslationalHullElement" );

DeclareGlobalFunction("LeftTranslation");
DeclareGlobalFunction("RightTranslation");
DeclareGlobalFunction("TranslationalHullElement");


DeclareSynonymAttr("IsLeftTranslationsSemigroup", IsSemigroup and
	IsLeftTranslationsSemigroupElementCollection);
DeclareSynonymAttr("IsRightTranslationsSemigroup", IsSemigroup and
	IsRightTranslationsSemigroupElementCollection);
DeclareSynonymAttr("IsTranslationsSemigroup", IsSemigroup and
	IsTranslationsSemigroupElementCollection);
DeclareSynonymAttr("IsTranslationalHull", IsSemigroup and 
	IsTranslationalHullElementCollection);
	

DeclareOperation("LeftTranslationsSemigroup", [IsSemigroup]);
DeclareOperation("RightTranslationsSemigroup", [IsSemigroup]);


DeclareAttribute("UnderlyingSemigroup", IsTranslationsSemigroup);
DeclareAttribute("UnderlyingSemigroup", IsTranslationalHull);

DeclareAttribute("LeftTranslationsSemigroupOfFamily", IsFamily);
DeclareAttribute("RightTranslationsSemigroupOfFamily", IsFamily);
DeclareAttribute("TranslationalHullOfFamily", IsFamily);


DeclareAttribute("TypeLeftTranslationsSemigroupElements",
	IsLeftTranslationsSemigroup);
DeclareAttribute("TypeRightTranslationsSemigroupElements", 
	IsRightTranslationsSemigroup);
DeclareAttribute("TypeTranslationalHullElements",
	IsTranslationalHull);



DeclareAttribute("LeftTranslations", IsSemigroup);
DeclareAttribute("RightTranslations", IsSemigroup);
DeclareAttribute("TranslationalHull", IsSemigroup);
DeclareAttribute("TranslationalHull2", IsRectangularBand);
