############################################################################
##
##  elements/pperm.gi
##  Copyright (C) 2016-2022                                 Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

if ApplicableMethod(AsDigraph, [PartialPerm([])]) = fail then
  # The method below (or an equivalent) will only be introduced in Digraphs >
  # 1.5.0, this should be removed when Semigroups requires Digraphs > 1.5.0

  DeclareOperation("AsDigraph", [IsPartialPerm]);

  InstallMethod(AsDigraph, "for a partial perm", [IsPartialPerm],
  function(f)
    local n, list, x, i;
    n := Maximum(DegreeOfPartialPerm(f), CodegreeOfPartialPerm(f));
    list := EmptyPlist(n);
    for i in [1 .. n] do
      x := i ^ f;
      if x > n then
        return fail;
      elif x <> 0 then
        list[i] := [x];
      else
        list[i] := [];
      fi;
    od;
    return DigraphNC(IsImmutableDigraph, list);
  end);
fi;

InstallMethod(IndexPeriodOfSemigroupElement, "for a partial perm",
[IsPartialPerm], IndexPeriodOfPartialPerm);

InstallMethod(CyclesOfPartialPerm, "for a partial perm", [IsPartialPerm],
function(f)
  return DigraphAllSimpleCircuits(AsDigraph(f));
end);
