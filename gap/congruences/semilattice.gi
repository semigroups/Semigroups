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
  local fam, classNo, class;
  fam := CollectionsFamily(FamilyObj(elm));
  classNo := SEMIGROUPS_SemilatticeCongClassNoOfElm(cong, elm);
  class := Objectify(NewType(fam, IsSemilatticeCongruenceClass),
                   rec(classNo := classNo));
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
  cong := EquivalenceClassRelation(class);
  s := Range(cong);
  return(elm in s and
         SEMIGROUPS_SemilatticeCongClassNoOfElm(cong, elm) = class!.classNo);
end);

#

InstallMethod(\=,
"for two semilattice congruence classes",
[IsSemilatticeCongruenceClass, IsSemilatticeCongruenceClass],
function(c1, c2)
  if EquivalenceClassRelation(c1) <> EquivalenceClassRelation(c2) then
    return false;
  fi;
  if c1!.classNo = 0 then
    return Representative(c1) = Representative(c2);
  else
    return c1!.classNo = c2!.classNo;
  fi;
end);

#

InstallMethod(\*,
"for two semilattice congruence classes",
[IsSemilatticeCongruenceClass, IsSemilatticeCongruenceClass],
function(c1, c2)
  if EquivalenceClassRelation(c1) <> EquivalenceClassRelation(c2) then
    Error("Semigroups: \*: usage,\n",
          "the args <c1> and <c2> must be classes of the same congruence,");
  elif c1 = c2 then
    return c1;
  else
    return EquivalenceClassOfElementNC(EquivalenceClassRelation(c1),
                   Representative(c1) * Representative(c2));
  fi;
end);

#

InstallMethod(NonTrivialEquivalenceClasses,
"for a semilattice congruence",
[IsSemilatticeCongruence],
function(cong)
  local s, nrclasses, fam, list, i;
  s := Range(cong);
  nrclasses := Maximum(BlockCoincidenceTable(cong));
  fam := CollectionsFamily(FamilyObj(GeneratorsOfSemigroup(s)[1]));
  list := EmptyPlist(nrclasses);
  for i in [1..nrclasses] do
    list[i] := Objectify(NewType(fam, IsSemilatticeCongruenceClass),
                       rec(classNo := i));
    SetParentAttr(list[i], cong);
    SetEquivalenceClassRelation(list[i], cong);
  od;
  return list;
end);

#

InstallMethod(Representative,
"for a semilattice congruence class",
[IsSemilatticeCongruenceClass],
function(class)
  local cong, pairs;
  cong := EquivalenceClassRelation(class);
  pairs := GeneratingPairsOfSemigroupCongruence(cong);
  return pairs[Position(BlockCoincidenceTable(cong), class!.classNo)][1];
end);

#

InstallGlobalFunction(SemilatticeElementsBetween,
function(s, bottom, top)
  if not (bottom in s and top in s) then
    Error("Semigroups: SemilatticeElementsBetween: usage,\n",
          "<bottom> and <top> must be elements of <s>,");
    return;
  elif not bottom * top = bottom then
    Error("Semigroups: SemilatticeElementsBetween: usage,\n",
          "<bottom> * <top> must be equal to <bottom>,");
    return;
  fi;
  return SemilatticeElementsBetweenNC(s, bottom, top);
end);

#

InstallGlobalFunction(SemilatticeElementsBetweenNC,
function(s, bottom, top)
  local gens, list, g, x, i, j;
  # Start by getting all the elements top*gen which lie in the range
  gens := GeneratorsOfSemigroup(s);
  list := Set([]);
  for g in gens do
    x := top * g;
    if x * bottom = bottom then
      AddSet(list, x);
    fi;
  od;

  # Now find all combinations of these class generators
  for i in [1..Length(list)] do # for each generator
    for j in [i+1..Length(list)] do # for each elm discovered so far
      x := list[i] * list[j];
      if not x in list then
        Add(list, x);
      fi;
    od;
  od;

  return list;
end);

#

InstallMethod(AsList,
"for a semilattice congruence class",
[IsSemilatticeCongruenceClass],
function(class)
  local cong, s, tab, blocks, pairs, meets, list, i;
  if class!.classNo = 0 then
    return [Representative(class)];
  fi;
  cong := EquivalenceClassRelation(class);
  s := Range(cong);
  tab := BlockCoincidenceTable(cong);
  blocks := Positions(tab, class!.classNo);
  pairs := GeneratingPairsOfSemigroupCongruence(cong);
  meets := MeetsOfGeneratingPairs(cong);
  list := [];
  for i in blocks do
    UniteSet(list, SemilatticeElementsBetweenNC(s, meets[i], pairs[i][1]));
    UniteSet(list, SemilatticeElementsBetweenNC(s, meets[i], pairs[i][2]));
  od;
  return list;
end);

#

InstallMethod(Enumerator,
"for a semilattice congruence class",
[IsSemilatticeCongruenceClass],
function(class)
  # PLACEHOLDER FUNCTION
  return AsList(class);
end);
