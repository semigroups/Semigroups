#############################################################################
##
##  cartan.gd
##  Copyright (C) 2024                                   Balthazar Charles
##                                                             Joseph Ruiz
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsGeneralizedConjugacyClass", IsObject);
DeclareAttribute("Representative", IsGeneralizedConjugacyClass);
DeclareAttribute("ParentAttr", IsGeneralizedConjugacyClass);
DeclareOperation("GeneralizedConjugacyClass",
                 [IsSemigroup, IsMultiplicativeElement]);
DeclareAttribute("GeneralizedConjugacyClassesRepresentatives", IsSemigroup);
DeclareAttribute("GeneralizedConjugacyClasses", IsSemigroup);
DeclareCategory("IsMonoidCharacterTable", IsObject);
DeclareAttribute("ParentAttr", IsMonoidCharacterTable);
DeclareAttribute("MonoidCharacterTable", IsSemigroup);

DeclareCategory("IsMonoidCharacter", IsObject);
DeclareOperation("MonoidCharacter", [IsMonoidCharacterTable, IsList]);
DeclareOperation("PimMonoidCharacter",
                 [IsMonoidCharacterTable, IsDenseList, IsMonoidCharacter]);
DeclareAttribute("ParentAttr", IsMonoidCharacter);
DeclareAttribute("ValuesOfMonoidClassFunction", IsMonoidCharacter);
DeclareAttribute("ProjectiveCoverOf", IsMonoidCharacter);
DeclareAttribute("ValuesOfCompositionFactorsFunction", IsMonoidCharacter);
DeclareAttribute("DClassBicharacter", IsGreensDClass);
DeclareAttribute("RegularRepresentationBicharacter", IsSemigroup);
DeclareAttribute("RClassBicharacterOfGroupHClass", IsGroupHClass);
DeclareAttribute("RClassRadicalOfGroupHClass", IsGroupHClass);
DeclareAttribute("RClassRadicalBicharacterOfGroupHClass", IsGroupHClass);
DeclareAttribute("BlockDiagonalMatrixOfCharacterTables", IsSemigroup);
DeclareAttribute("Irr", IsMonoidCharacterTable);

DeclareCategory("IsMonoidCartanMatrix", IsObject);
DeclareAttribute("ParentAttr", IsMonoidCartanMatrix);
DeclareAttribute("MonoidCartanMatrix", IsSemigroup);

DeclareAttribute("Pims", IsMonoidCartanMatrix);