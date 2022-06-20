############################################################################
##
# W  translat.gd
# Y  Copyright (C) 2015-22                      James D. Mitchell, Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsSemigroupTranslation",
                IsAssociativeElement and IsMultiplicativeElementWithOne);
DeclareCategory("IsLeftTranslation",
                IsSemigroupTranslation);
DeclareCategory("IsRightTranslation",
                IsSemigroupTranslation);
DeclareCategory("IsBitranslation",
                IsAssociativeElement and IsMultiplicativeElementWithOne);

DeclareCategoryCollections("IsSemigroupTranslation");
DeclareCategoryCollections("IsLeftTranslation");
DeclareCategoryCollections("IsRightTranslation");
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
               IsSemigroup and IsLeftTranslationCollection);
DeclareSynonym("IsRightTranslationsSemigroup",
               IsSemigroup and IsRightTranslationCollection);
DeclareSynonym("IsTranslationsSemigroup",
               IsSemigroup and IsSemigroupTranslationCollection);
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

InstallTrueMethod(CanUseGapFroidurePin, IsLeftTranslationsSemigroup);
InstallTrueMethod(CanUseGapFroidurePin, IsRightTranslationsSemigroup);
InstallTrueMethod(CanUseGapFroidurePin, IsBitranslationsSemigroup);

DeclareAttribute("UnderlyingRepresentatives", IsTranslationsSemigroup);

DeclareOperation("ImageOfTranslation", [IsSemigroupTranslation]);
