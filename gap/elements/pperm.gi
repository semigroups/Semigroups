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
  return DigraphAllSimpleCircuits(AsDigraph(f));
end);
