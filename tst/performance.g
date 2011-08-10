
# Read("pkg/citrus/tst/performance.g");

MinimalIdealSize:=s-> Size(MinimalIdeal(s));

HasMultIdentity:=s-> not MultiplicativeNeutralElement(s)=fail;

GroupOfUnitsStructure:=function(s)
  local g;
  g:=GroupOfUnits(s);
  if not g=fail then 
    return StructureDescription(g);
  fi;
  return g;
end;

  CitrusTests:=[Size, NrGreensRClasses, NrGreensDClasses, NrGreensLClasses,
  NrGreensHClasses, NrIdempotents, NrRegularDClasses, MultiplicativeZero,
  HasMultIdentity, GroupOfUnitsStructure, MinimalIdealSize, IsBlockGroup,
  IsCliffordSemigroup, IsCommutativeSemigroup, IsCompletelyRegularSemigroup,
  IsCompletelySimpleSemigroup, IsGreensHTrivial, IsGreensLTrivial,
  IsGreensRTrivial, IsGroupAsSemigroup, IsInverseSemigroup, IsLeftZeroSemigroup,
  IsMonoidAsSemigroup, IsOrthodoxSemigroup, IsRectangularBand,
  IsRegularSemigroup, IsRightZeroSemigroup, IsSemiband,
  IsSemilatticeAsSemigroup, IsSimpleSemigroup,
  IsSynchronizingSemigroup, IsZeroGroup, IsZeroSemigroup];

CitrusTestSuite:=function(s, j)
  local tabs, t, out, i;

  tabs:=Concatenation([4,2,2,2,2,3,2,2,2,2,2,3,2,1,1,1],
  ListWithIdenticalEntries(11, 2), [3,1,2,1,3,2]);

  for i in [1..Length(CitrusTests)] do 
    Print("Example ", j, "\t");
    Print(NAME_FUNC(CitrusTests[i]), ": ", 
    Concatenation(ListWithIdenticalEntries(tabs[i],
    "\t")));
    t:=Runtime(); out:=CitrusTests[i](s); t:=Runtime()-t;
    Print(out, "\t\t time: ", t, "\n");
  od;
end;

CitrusTestWriter:=function(gens, j)
  local tests;
  
  tabs:=Concatenation([4,2,2,2,2,3,2,2,2,2,2,3,2,2,1,1],
  ListWithIdenticalEntries(11, 2), [3,1,2,1,3,2]);
  Print("gap> gens:=", gens, ";;\n");
  Print("gap> s:=Semigroup(gens);;\n");
  for i in [1..Length(CitrusTests)] do 
   Print("gap> t:=Runtime\(\);; out:=CitrusTests\[", i, "\]\(s\);;", 
   " t:=Runtime\(\)-t;;\n");
   Print("gap> Print\(\"Example \", ",j,", \"\\t",
   NAME_FUNC(CitrusTests[i]), ":\" , ", "\"", 
   Concatenation(ListWithIdenticalEntries(tabs[i], "\\t")),"\",\n");
   Print("> out, \"\\t\\t time: \", t, \"\\n\"\);\n\n");
  od;
end;

#Print("\nGap should be started with at least 1g of memory (gap -m 1000m)\n\n");
#Print("Note that the times are typically higher than those obtained by",
#" running,\nsay, Size(s); time;\n\n");

#ReadTest("pkg/citrus/tst/performance.tst");

