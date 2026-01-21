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
DeclareAttribute("Irr", IsMonoidCharacterTable);
DeclareAttribute("ComputedMonoidBrauerTables", IsMonoidCharacterTable);  # todo
DeclareAttribute("MonoidCharacterTable", IsSemigroup);

# todo block
DeclareCategory("IsMonoidBrauerTable", IsObject);
DeclareAttribute("MonoidCharacterTable", IsMonoidBrauerTable);
DeclareAttribute("Irr", IsMonoidBrauerTable);
DeclareAttribute("UnderlyingCharacteristic", IsMonoidBrauerTable);
DeclareAttribute("UnderlyingField", IsMonoidBrauerTable);
# todo end block

DeclareCategory("IsMonoidCharacter", IsObject);
DeclareOperation("MonoidCharacter", [IsMonoidCharacterTable, IsList]);
DeclareOperation("PimMonoidCharacter",
                 [IsMonoidCharacterTable, IsDenseList, IsMonoidCharacter]);
DeclareAttribute("ParentAttr", IsMonoidCharacter);
DeclareAttribute("ValuesOfMonoidClassFunction", IsMonoidCharacter);
DeclareAttribute("ProjectiveCoverOf", IsMonoidCharacter);
DeclareAttribute("ValuesOfCompositionFactorsFunction", IsMonoidCharacter);

# todo block
DeclareCategory("IsMonoidModularCharacter", IsObject);
DeclareOperation("MonoidModularCharacter", [IsMonoidBrauerTable, IsList]);
DeclareOperation("PimMonoidModularCharacter",
                 [IsMonoidBrauerTable, IsDenseList, IsMonoidModularCharacter]);
DeclareAttribute("ParentAttr", IsMonoidModularCharacter);
DeclareAttribute("ValuesOfMonoidPClassFunction", IsMonoidCharacter);
DeclareAttribute("ProjectiveCoverOf", IsMonoidModularCharacter);
DeclareAttribute("ValuesOfCompositionFactorsFunction",
                 IsMonoidModularCharacter);
# todo end block

DeclareAttribute("DClassBicharacter", IsGreensDClass);
DeclareAttribute("RegularRepresentationBicharacter", IsSemigroup);
DeclareAttribute("RClassBicharacterOfGroupHClass", IsGroupHClass);
DeclareAttribute("RClassRadicalOfGroupHClass", IsGroupHClass);
DeclareAttribute("RClassRadicalBicharacterOfGroupHClass", IsGroupHClass);
DeclareAttribute("BlockDiagonalMatrixOfCharacterTables", IsSemigroup);

DeclareCategory("IsMonoidCartanMatrix", IsObject);
DeclareAttribute("ParentAttr", IsMonoidCartanMatrix);
DeclareAttribute("MonoidCartanMatrix", IsSemigroup);

DeclareAttribute("Pims", IsMonoidCartanMatrix);

DeclareOperation("PrepareTableDisplay", [IsList, IsString, IsString]);