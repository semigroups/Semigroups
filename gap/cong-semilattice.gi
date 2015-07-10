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
  meets := MeetsOfGeneratingPairs(cong);
  
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

InstallMethod(MeetsOfGeneratingPairs,
"for a semilattice congruence",
[IsSemilatticeCongruence],
cong -> List(GeneratingPairsOfSemigroupCongruence(cong),
             pair -> pair[1] * pair[2]));

#

InstallGlobalFunction(SEMIGROUPS_SemilatticeCongClassNoOfElm,
function(cong, elm)
  # Returns an int describing which congruence class elm is in
  local genpairs, meets, pairno;
  genpairs := GeneratingPairsOfSemigroupCongruence(cong);
  meets := MeetsOfGeneratingPairs(cong);
  for pairno in [1..Length(genpairs)] do
    # Is elm "hit" by this genpair?
    if elm*meets[pairno] = meets[pairno] and
       (elm*genpairs[pairno][1] = elm or
        elm*genpairs[pairno][2] = elm) then
      return BlockCoincidenceTable(cong)[pairno];
    fi;
  od;
  # elm is in a singleton class
  return 0;
end);

#

InstallMethod(\in,
"for an associative element collection and a semilattice congruence",
[IsAssociativeElementCollection, IsSemilatticeCongruence],
1,
function(pair, cong)
  local s, c1;
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

  # Try reflexivity
  if pair[1] = pair[2] then
    return true;
  else
    # Use the class numbers
    c1 := SEMIGROUPS_SemilatticeCongClassNoOfElm(cong, pair[1]);
    if c1 = 0 then
      return false; # singleton
    else
      return c1 = SEMIGROUPS_SemilatticeCongClassNoOfElm(cong, pair[2]);
    fi;
  fi;
end);

#

InstallMethod(EquivalenceClassOfElement,
"for a semilattice congruence and an associative element",
[IsSemilatticeCongruence, IsAssociativeElement],
1,
function(cong, elm)
  # Check the arguments
  if not elm in Range(cong) then
    Error("Semigroups: EquivalenceClassOfElement: usage,\n",
          "the second arg <elm> must be ",
          "in the semigroup of first arg <cong>");
    return;
  fi;
  return EquivalenceClassOfElementNC(cong, elm);
end);

#

InstallMethod(EquivalenceClassOfElementNC,
"for a semilattice congruence and an associative element",
[IsSemilatticeCongruence, IsAssociativeElement],
1,
function(cong, elm)
  local fam, class;
  fam := CollectionsFamily(FamilyObj(elm));
  class := Objectify(NewType(fam, IsSemilatticeCongruenceClass),
                   rec(classNo := SEMIGROUPS_SemilatticeCongClassNoOfElm(cong, elm)));
  SetParentAttr(class, cong);
  SetEquivalenceClassRelation(class, cong);
  SetRepresentative(class, elm);
  return class;
end);

#

InstallMethod(\in,
"for an associative element and a semilattice congruence class",
[IsAssociativeElement, IsSemilatticeCongruenceClass],
function(elm, class)
  local s, cong;
  cong := ParentAttr(class);
  s := Range(cong);
  return(elm in s and
         SEMIGROUPS_SemilatticeCongClassNoOfElm(cong, elm) = class!.classNo);
end);

#
