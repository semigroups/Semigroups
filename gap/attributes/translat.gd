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
DeclareCategory("IsBitranslation", IsAssociativeElement and
                  IsMultiplicativeElementWithOne);

DeclareCategoryCollections("IsTranslationsSemigroupElement");
DeclareCategoryCollections("IsLeftTranslationsSemigroupElement");
DeclareCategoryCollections("IsRightTranslationsSemigroupElement");
DeclareCategoryCollections("IsBitranslation");

DeclareGlobalFunction("LeftTranslation");
DeclareGlobalFunction("LeftTranslationNC");
DeclareGlobalFunction("RightTranslation");
DeclareGlobalFunction("RightTranslationNC");
DeclareGlobalFunction("Bitranslation");
DeclareGlobalFunction("BitranslationNC");
DeclareGlobalFunction("LeftTranslationsSemigroup");
DeclareGlobalFunction("RightTranslationsSemigroup");
DeclareGlobalFunction("TranslationalHullSemigroup");
DeclareGlobalFunction("LeftPartOfBitranslation");
DeclareGlobalFunction("RightPartOfBitranslation");

DeclareSynonym("IsLeftTranslationsSemigroup", IsSemigroup and
                  IsLeftTranslationsSemigroupElementCollection);
DeclareSynonym("IsRightTranslationsSemigroup", IsSemigroup and
                  IsRightTranslationsSemigroupElementCollection);
DeclareSynonym("IsTranslationsSemigroup", IsSemigroup and
                  IsTranslationsSemigroupElementCollection);
DeclareSynonym("IsTranslationalHull", IsSemigroup and
                  IsBitranslationCollection);

DeclareAttribute("UnderlyingSemigroup", IsTranslationsSemigroup);
DeclareAttribute("UnderlyingSemigroup", IsTranslationalHull);

DeclareAttribute("LeftTranslationsSemigroupOfFamily", IsFamily);
DeclareAttribute("RightTranslationsSemigroupOfFamily", IsFamily);
DeclareAttribute("TranslationalHullOfFamily", IsFamily);

DeclareAttribute("TypeLeftTranslationsSemigroupElements",
                 IsLeftTranslationsSemigroup);
DeclareAttribute("TypeRightTranslationsSemigroupElements",
                 IsRightTranslationsSemigroup);
DeclareAttribute("TypeBitranslations", IsTranslationalHull);

DeclareAttribute("LeftTranslations",
                  IsEnumerableSemigroupRep and IsFinite);
DeclareAttribute("InnerLeftTranslations",
                  IsEnumerableSemigroupRep and IsFinite);
DeclareAttribute("RightTranslations",
                  IsEnumerableSemigroupRep and IsFinite);
DeclareAttribute("InnerRightTranslations",
                  IsEnumerableSemigroupRep and IsFinite);
DeclareAttribute("TranslationalHull",
                  IsEnumerableSemigroupRep and IsFinite);
DeclareAttribute("InnerTranslationalHull",
                  IsEnumerableSemigroupRep and IsFinite);
DeclareAttribute("TranslationalElements",
                  IsTranslationsSemigroup and IsWholeFamily);
DeclareAttribute("TranslationalElements",
                  IsTranslationalHull and IsWholeFamily);
