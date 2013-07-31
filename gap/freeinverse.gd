

#Header required...


DeclareCategory("IsFreeInverseSemigroupElement",
  IsAssociativeElementWithUniqueSemigroupInverse);
DeclareCategoryCollections("IsFreeInverseSemigroupElement");
DeclareProperty("IsFreeInverseSemigroup", IsSemigroup);
DeclareGlobalFunction("FreeInverseSemigroup");
DeclareAttribute("CanonicalForm", IsFreeInverseSemigroupElement);
