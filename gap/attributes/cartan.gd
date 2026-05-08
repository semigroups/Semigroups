#############################################################################
##
##  cartan.gd
##  Copyright (C) 2024-2026                              Balthazar Charles
##                                                             Joseph Ruiz
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsGeneralizedConjugacyClass", IsCollection);
DeclareAttribute("Representative", IsGeneralizedConjugacyClass);
DeclareAttribute("ParentAttr", IsGeneralizedConjugacyClass);
DeclareAttribute("MapToGroupHClass", IsGeneralizedConjugacyClass);
DeclareOperation("GeneralizedConjugacyClass",
                 [IsSemigroup, IsMultiplicativeElement]);
DeclareOperation("GeneralizedConjugacyClass",
                 [IsSemigroup, IsMultiplicativeElement, IsGeneralMapping]);
DeclareAttribute("GeneralizedConjugacyClasses", IsSemigroup);
DeclareAttribute("GeneralizedConjugacyClassesRepresentatives", IsSemigroup);
DeclareCategory("IsMonoidCharacterTable", IsObject);
DeclareAttribute("ParentAttr", IsMonoidCharacterTable);
DeclareAttribute("CartanMatrix", IsMonoidCharacterTable);
DeclareAttribute("OrdinaryCharacterTable", IsSemigroup);
DeclareOperation("CharacterTable", [IsSemigroup]);

DeclareCategory("IsMonoidCharacter", IsObject);
DeclareOperation("Character", [IsMonoidCharacterTable, IsList]);
DeclareOperation("Character",
                 [IsMonoidCharacterTable, IsDenseList, IsMonoidCharacter]);
DeclareOperation("\^", [IsMultiplicativeElement, IsMonoidCharacter]);
DeclareAttribute("ParentAttr", IsMonoidCharacter);
DeclareAttribute("ValuesOfClassFunction", IsMonoidCharacter);
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
DeclareAttribute("CartanMatrix", IsSemigroup);

DeclareAttribute("Pims", IsMonoidCartanMatrix);