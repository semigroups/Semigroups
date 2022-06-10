############################################################################
##
# W  translat.gd
# Y  Copyright (C) 2015-22                      James D. Mitchell, Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsTranslationsSemigroupElement",
                IsAssociativeElement and IsMultiplicativeElementWithOne);
DeclareCategory("IsLeftTranslationsSemigroupElement",
                IsTranslationsSemigroupElement);
DeclareCategory("IsRightTranslationsSemigroupElement",
                IsTranslationsSemigroupElement);
DeclareCategory("IsBitranslation",
                IsAssociativeElement and IsMultiplicativeElementWithOne);

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

DeclareSynonym("IsLeftTranslationsSemigroup",
               IsSemigroup and IsLeftTranslationsSemigroupElementCollection);
DeclareSynonym("IsRightTranslationsSemigroup",
               IsSemigroup and IsRightTranslationsSemigroupElementCollection);
DeclareSynonym("IsTranslationsSemigroup",
               IsSemigroup and IsTranslationsSemigroupElementCollection);
DeclareSynonym("IsBitranslationsSemigroup",
               IsSemigroup and IsBitranslationCollection);

DeclareAttribute("UnderlyingSemigroup", IsTranslationsSemigroup);
DeclareAttribute("UnderlyingSemigroup", IsBitranslationsSemigroup);

DeclareAttribute("LeftTranslationsSemigroupOfFamily", IsFamily);
DeclareAttribute("RightTranslationsSemigroupOfFamily", IsFamily);
DeclareAttribute("TranslationalHullOfFamily", IsFamily);

DeclareAttribute("TypeLeftTranslationsSemigroupElements",
                 IsLeftTranslationsSemigroup);
DeclareAttribute("TypeRightTranslationsSemigroupElements",
                 IsRightTranslationsSemigroup);
DeclareAttribute("TypeBitranslations", IsBitranslationsSemigroup);

DeclareAttribute("LeftTranslations",
                 IsSemigroup and CanUseFroidurePin and IsFinite);
DeclareAttribute("InnerLeftTranslations",
                 IsSemigroup and CanUseFroidurePin and IsFinite);
DeclareAttribute("RightTranslations",
                 IsSemigroup and CanUseFroidurePin and IsFinite);
DeclareAttribute("InnerRightTranslations",
                 IsSemigroup and CanUseFroidurePin and IsFinite);
DeclareAttribute("TranslationalHull",
                 IsSemigroup and CanUseFroidurePin and IsFinite);
DeclareAttribute("InnerTranslationalHull",
                 IsSemigroup and CanUseFroidurePin and IsFinite);
DeclareAttribute("TranslationalElements",
                  IsTranslationsSemigroup and IsWholeFamily);
DeclareAttribute("TranslationalElements",
                  IsBitranslationsSemigroup and IsWholeFamily);

InstallTrueMethod(IsFinite, IsLeftTranslationsSemigroup);
InstallTrueMethod(IsFinite, IsRightTranslationsSemigroup);
InstallTrueMethod(IsFinite, IsBitranslationsSemigroup);

DeclareAttribute("UnderlyingGenerators", IsTranslationsSemigroup);
DeclareAttribute("UnderlyingGenerators", IsBitranslationsSemigroup);

DeclareOperation("ImageOfTranslation", [IsTranslationsSemigroupElement]);
