############################################################################
##
#W  translat.gd
#Y  Copyright (C) 2015-17                      James D. Mitchell, Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsTranslationsSemigroupElement", IsAssociativeElement and
                  IsMultiplicativeElementWithOne);
DeclareCategory("IsLeftTranslationsSemigroupElement",
                IsTranslationsSemigroupElement);
DeclareCategory("IsRightTranslationsSemigroupElement",
                IsTranslationsSemigroupElement);
DeclareCategory("IsTranslationalHullElement", IsAssociativeElement and
                  IsMultiplicativeElementWithOne);

DeclareCategoryCollections("IsTranslationsSemigroupElement");
DeclareCategoryCollections("IsLeftTranslationsSemigroupElement");
DeclareCategoryCollections("IsRightTranslationsSemigroupElement");
DeclareCategoryCollections("IsTranslationalHullElement" );

DeclareGlobalFunction("LeftTranslation");
DeclareGlobalFunction("LeftTranslationNC");
DeclareGlobalFunction("RightTranslation");
DeclareGlobalFunction("RightTranslationNC");
DeclareGlobalFunction("TranslationalHullElement");
DeclareGlobalFunction("TranslationalHullElementNC");
DeclareGlobalFunction("LeftTranslationsSemigroup");
DeclareGlobalFunction("RightTranslationsSemigroup");
DeclareGlobalFunction("TranslationalHullSemigroup");

DeclareSynonym("IsLeftTranslationsSemigroup", IsSemigroup and
                  IsLeftTranslationsSemigroupElementCollection);
DeclareSynonym("IsRightTranslationsSemigroup", IsSemigroup and
                  IsRightTranslationsSemigroupElementCollection);
DeclareSynonym("IsTranslationsSemigroup", IsSemigroup and
                  IsTranslationsSemigroupElementCollection);
DeclareSynonym("IsTranslationalHull", IsSemigroup and 
                  IsTranslationalHullElementCollection);

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


DeclareAttribute("LeftTranslations", IsSemigroup and IsFinite);
DeclareAttribute("InnerLeftTranslations", IsSemigroup and IsFinite);
DeclareAttribute("RightTranslations", IsSemigroup and IsFinite);
DeclareAttribute("InnerRightTranslations", IsSemigroup and IsFinite);
DeclareAttribute("TranslationalHull", IsSemigroup and IsFinite);
DeclareAttribute("InnerTranslationalHull", IsSemigroup and IsFinite);
DeclareAttribute("TranslationalElements", IsTranslationsSemigroup and IsWholeFamily);
DeclareAttribute("TranslationalElements", IsTranslationalHull and IsWholeFamily);
