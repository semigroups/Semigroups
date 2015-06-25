############################################################################
##
#W  cong-semilattice.gi
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for congruences on semilattices.
##

InstallMethod(BlockCoincidenceTable,
"for a semilattice congruence",
[IsSemilatticeCongruence],
function(cong)
  local pairs, nr, tab, meets, i, j, critpoints, pt, next;
  pairs := GeneratingPairsOfSemigroupCongruence(cong);
  nr := Length(pairs);
  tab := UF_NEW(nr);
  
  # Find the meet of each pair
  meets := List(pairs, pair-> pair[1] * pair[2]);
  cong!.meets := meets;
  
  # Search for collisions
  for i in [1..nr] do
    for j in [i+1..nr] do
      critpoints := [pairs[i][1] * pairs[j][1],
                     pairs[i][1] * pairs[j][2],
                     pairs[i][2] * pairs[j][1],
                     pairs[i][1] * pairs[j][2]];
      for pt in critpoints do
        if (meets[i]*pt = meets[i]) and
           (meets[j]*pt = meets[j]) then
          UF_UNION(tab, [i,j]);
          break;
        fi;
      od;
    od;
  od;

  # "Normalise" the table (using successive numbers)
  UF_FLATTEN(tab);
  tab := UF_TABLE(tab);
  next := 1;
  for i in [1..Length(tab)] do
    if tab[i] = i then
      tab[i] := next;
      next := next + 1;
    else
      tab[i] := tab[tab[i]];
    fi;
  od;
  return tab;
end);

#

InstallMethod(\in,
"for an associative element collection and a semilattice congruence",
[IsAssociativeElementCollection, IsSemilatticeCongruence],
1,
function(pair, cong)
  local s, genpairs, blocktable, meets, nrblocks, hitblocks, pairno, blockno, i;
  # Input checks
  if not Size(pair) = 2 then
    Error("Semigroups: \in: usage,\n",
          "the first arg <pair> must be a list of length 2,");
    return;
  fi;
  s := Range(cong);
  if not (pair[1] in s and pair[2] in s) then
    Error("Semigroups: \in: usage,\n",
          "elements of the first arg <pair> must be in range",
          "of the second\narg <cong>,");
    return;
  fi;
  
  # Is the pair reflexive?
  if pair[1] = pair[2] then
    return true;
  fi;
  
  # Find which blocks hit each element
  genpairs := GeneratingPairsOfSemigroupCongruence(cong);
  blocktable := BlockCoincidenceTable(cong);
  meets := cong!.meets;
  nrblocks := Maximum(blocktable);
  hitblocks := [ BlistList([1..nrblocks], []), BlistList([1..nrblocks], []) ];
  for pairno in [1..Length(genpairs)] do
    blockno := blocktable[pairno];
    for i in [1,2] do
      # Do we already know that pair[i] hits this block?
      if hitblocks[i][blockno] then
        continue;
      else
        # Is pair[i] "hit" by this genpair?
        if pair[i]*meets[pairno] = meets[pairno] and
           (pair[i]*genpairs[pairno][1] = pair[i] or
            pair[i]*genpairs[pairno][2] = pair[i]) then
          hitblocks[i][blockno] := true;
        fi;
      fi;
    od;
    # Are pair[1] and pair[2] both in this block?
    if hitblocks[1][blockno] and hitblocks[2][blockno] then
      return true;
    fi;
  od;
  # If we haven't found a shared block yet, then the elements do not meet
  return false;
end);
