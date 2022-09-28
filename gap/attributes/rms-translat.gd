############################################################################
##
# W  rms-translat.gd
# Y  Copyright (C) 2015-22                                      Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# The definitions in this file are internal, and hence not included in the
# user documentation.
DeclareCategory("_IsTranslationOfNormalRMS", IsSemigroupTranslation);
DeclareCategory("_IsLeftTranslationOfNormalRMS",
                _IsTranslationOfNormalRMS and IsLeftTranslation);
DeclareCategory("_IsRightTranslationOfNormalRMS",
                _IsTranslationOfNormalRMS and IsRightTranslation);
DeclareCategory("_IsBitranslationOfNormalRMS", IsBitranslation);

DeclareCategoryCollections("_IsTranslationOfNormalRMS");
DeclareCategoryCollections("_IsLeftTranslationOfNormalRMS");
DeclareCategoryCollections("_IsRightTranslationOfNormalRMS");
DeclareCategoryCollections("_IsBitranslationOfNormalRMS");

DeclareSynonym("_IsTranslationOfNormalRMSSemigroup", IsSemigroup and
                _IsTranslationOfNormalRMSCollection);
DeclareSynonym("_IsLeftTranslationOfNormalRMSSemigroup", IsSemigroup and
                _IsLeftTranslationOfNormalRMSCollection);
DeclareSynonym("_IsRightTranslationOfNormalRMSSemigroup", IsSemigroup and
                _IsRightTranslationOfNormalRMSCollection);
DeclareSynonym("_IsBitranslationOfNormalRMSSemigroup", IsSemigroup and
                _IsBitranslationOfNormalRMSCollection);

DeclareGlobalFunction("_LeftTranslationOfNormalRMS");
DeclareGlobalFunction("_LeftTranslationOfNormalRMSNC");
DeclareGlobalFunction("_RightTranslationOfNormalRMS");
DeclareGlobalFunction("_RightTranslationOfNormalRMSNC");

DeclareGlobalFunction("_BitranslationOfNormalRMSByTripleNC");
