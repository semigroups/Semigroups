############################################################################
##
# W  rms-translat.gd
# Y  Copyright (C) 2015-22                                       Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# The declarations in this file are purposefully undocumented.
DeclareCategory("IsTranslationOfNormalRMS", IsSemigroupTranslation);
DeclareCategory("IsLeftTranslationOfNormalRMS",
                IsTranslationOfNormalRMS and IsLeftTranslation);
DeclareCategory("IsRightTranslationOfNormalRMS",
                IsTranslationOfNormalRMS and IsRightTranslation);
DeclareCategory("IsBitranslationOfNormalRMS", IsBitranslation);

DeclareCategoryCollections("IsTranslationOfNormalRMS");
DeclareCategoryCollections("IsLeftTranslationOfNormalRMS");
DeclareCategoryCollections("IsRightTranslationOfNormalRMS");
DeclareCategoryCollections("IsBitranslationOfNormalRMS");

DeclareSynonym("IsTranslationOfNormalRMSSemigroup", IsSemigroup and
                IsTranslationOfNormalRMSCollection);
DeclareSynonym("IsLeftTranslationOfNormalRMSSemigroup", IsSemigroup and
                IsLeftTranslationOfNormalRMSCollection);
DeclareSynonym("IsRightTranslationOfNormalRMSSemigroup", IsSemigroup and
                IsRightTranslationOfNormalRMSCollection);
DeclareSynonym("IsBitranslationOfNormalRMSSemigroup", IsSemigroup and
                IsBitranslationOfNormalRMSCollection);

DeclareGlobalFunction("LeftTranslationOfNormalRMS");
DeclareGlobalFunction("LeftTranslationOfNormalRMSNC");
DeclareGlobalFunction("RightTranslationOfNormalRMS");
DeclareGlobalFunction("RightTranslationOfNormalRMSNC");

DeclareGlobalFunction("BitranslationOfNormalRMSByTripleNC");
