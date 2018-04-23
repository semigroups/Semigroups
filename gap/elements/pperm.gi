############################################################################
##
##  pperm.gi
##  Copyright (C) 2016                                      Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

InstallMethod(IndexPeriodOfSemigroupElement, "for a partial perm",
[IsPartialPerm], IndexPeriodOfPartialPerm);

InstallMethod(CyclesOfPartialPerm, "for a partial perm", [IsPartialPerm],
function(f)
  local n, seen, out, i, j, cycle;

  n := Maximum(DegreeOfPartialPerm(f), CoDegreeOfPartialPerm(f));
  seen := BlistList([1 .. n], ImageSetOfPartialPerm(f));
  out := [];

  # find chains
  for i in DomainOfPartialPerm(f) do
    if not seen[i] then
      i := i ^ f;
      while i <> 0 do
        seen[i] := false;
        i := i ^ f;
      od;
    fi;
  od;

  # find cycles
  for i in DomainOfPartialPerm(f) do
    if seen[i] then
      j := i ^ f;
      cycle := [j];
      while j <> i do
        seen[j] := false;
        j := j ^ f;
        Add(cycle, j);
      od;
      Add(out, cycle);
    fi;
  od;
  return out;
end);
