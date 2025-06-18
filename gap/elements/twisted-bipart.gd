
DeclareCategory("IsTwistedBipartition", IsMultiplicativeElementWithOne
and IsMultiplicativeElementWithZero and
IsAssociativeElement);

DeclareOperation("TwistedBipartition", [IsInt, IsBipartition, IsInt]);
DeclareOperation("ZeroTwistedBipartition", [IsInt, IsInt]);
DeclareAttribute("DegreeOfTwistedBipartition",
IsTwistedBipartition);
DeclareAttribute("MaxFloatingBlocks",
IsTwistedBipartition);
DeclareAttribute("NrFloatingBlocks",
IsTwistedBipartition);
DeclareAttribute("UnderlyingBipartition",
IsTwistedBipartition);

DeclareOperation("RandomNonZeroTwistedBipartition", [IsInt, IsInt]);
DeclareProperty("IsZero", IsTwistedBipartition);
DeclareAttribute("ZeroImmutable", IsTwistedBipartition);
