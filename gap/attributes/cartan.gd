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

DeclareAttribute("TransversalIdempotents", IsSemigroup);
DeclareCategory("IsGeneralisedConjugacyClass", IsObject);
DeclareAttribute("Representative", IsGeneralisedConjugacyClass);
DeclareAttribute("ParentAttr", IsGeneralisedConjugacyClass);
DeclareOperation("GeneralisedConjugacyClass", [IsSemigroup, IsObject]);
DeclareAttribute("GeneralisedConjugacyClassesRepresentatives", IsSemigroup);
DeclareAttribute("GeneralisedConjugacyClasses", IsSemigroup);
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
DeclareAttribute("DiagonalOfCharacterTables", IsSemigroup);
DeclareAttribute("Irr", IsMonoidCharacterTable);

DeclareCategory("IsMonoidCartanMatrix", IsObject);
DeclareAttribute("ParentAttr", IsMonoidCartanMatrix);
DeclareAttribute("MonoidCartanMatrix", IsSemigroup);

DeclareAttribute("Pims", IsMonoidCartanMatrix);
