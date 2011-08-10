
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

CreateCitrusPerformanceTest:=function(file, gens, j)
  local str, tabs, s, t, out, i;
  
  str:=StringFile(file);
  str:=SplitString(str, "\n");  
  str:=str{[1..Length(str)-1]}; 
  str:=JoinStringsWithSeparator(str, "\n");
  str:=Concatenation(str, "\n\n");

  tabs:=Concatenation([4,2,2,2,2,3,2,2,2,2,2,3,2,2,1,1],
  ListWithIdenticalEntries(11, 2), [3,1,2,1,3,2]);
 
  str:=Concatenation(str, "gap> gens:=[", StringPrint(gens[1]), ",\n");
  
  for i in [2..Length(gens)] do 
    str:=Concatenation(str, "> ", StringPrint(gens[i]));
    if i<Length(gens) then 
      str:=Concatenation(str, ",\n");
    else
      str:=Concatenation(str, "];;\n");
    fi;
  od;
  str:=Concatenation(str, "gap> s:=Semigroup(gens);;\n");
  s:=Semigroup(gens); 
  for i in [1..Length(CitrusTests)] do 
    str:=Concatenation(str, "gap> t:=Runtime\(\);; out:=CitrusTests\[", 
    String(i), "\]\(s\);;", " t:=Runtime\(\)-t;;\n");
    str:=Concatenation(str, "gap> Print\(\"Example \", ",String(j),", \"\\t",
    NAME_FUNC(CitrusTests[i]), ":\" , ", "\"", 
    Concatenation(ListWithIdenticalEntries(tabs[i], "\\t")),"\",\n", 
    "> out, \"\\t\\t time: \", t, \"\\n\"\);\n");
    t:=Runtime(); out:=CitrusTests[i](s); t:=Runtime()-t;
    str:=Concatenation(str, "Example ", String(j), "\t",
    NAME_FUNC(CitrusTests[i]), ":",
    Concatenation(ListWithIdenticalEntries(tabs[i], "\t")), String(out), 
    "\t\t time: ", String(t), "\n");
  od;
  str:=Concatenation(str, "\n\ngap> STOP_TEST\( \"performance.tst 0.1\", 0\);");
  FileString(file, str);
  return true;
end;

#Print("\nGap should be started with at least 1g of memory (gap -m 1000m)\n\n");
#Print("Note that the times are typically higher than those obtained by",
#" running,\nsay, Size(s); time;\n\n");

#ReadTest("pkg/citrus/tst/performance.tst");

