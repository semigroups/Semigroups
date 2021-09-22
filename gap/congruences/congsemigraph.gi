############################################################################
##
##  congsemigraph.gd
##  Copyright (C) 2021                       Marina Anagnostopoulou-Merkouri
##                                                            James Mitchell 
##
##  Licensing information can be found in the README file of this package.
##
############################################################################

InstallMethod(CongruenceByWangPair, 
"for a graph inverse semigroup, s.s. list, and s.s. list",
[IsGraphInverseSemigroup, IsSSortedList, IsSSortedList],
function(S, H, W)
  local fam, cong;

  # TODO check the arguments make sense.
  # Construct the object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));
  cong := Objectify(NewType(fam, IsCongruenceByWangPair),
                    rec(H := H, W := W));
  SetSource(cong, S);
  SetRange(cong, S);
  return cong;
end);

InstallMethod(ViewObj, "for a congruence by Wang pair",
[IsCongruenceByWangPair], 
function(C)
  Print(ViewString(C));
end);

InstallMethod(ViewString, "for a congruence by Wang pair",
[IsCongruenceByWangPair], 
function(C)
  return Concatenation("<gis-congruence with H=", 
                       ViewString(C!.H),
                       ", W=", 
                       ViewString(C!.W), 
                       ">");
end);

InstallMethod(AsCongruenceByWangPair, "for a semigroup congruence",
[IsSemigroupCongruence],
function(C)
  local H, W, eq, j;
  if not IsGraphInverseSemigroup(Source(C)) then
    ErrorNoReturn("TODO");
  fi;
  
  H := [];
  W := [];
  eq := EquivalenceRelationPartition(C);
  eq := Filtered(eq, x -> ForAny(x, IsVertex));
  for j in eq do
    if MultiplicativeZero(Source(C)) in j then
      Append(H, Filtered(j, IsVertex));
    else
      Append(W, Filtered(j, IsVertex));
    fi;
  od;
  Sort(H);
  Sort(W);
  return CongruenceByWangPair(Source(C), H, W);
end);

