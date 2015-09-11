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
  local cong, s, tab, blocks, pairs, meets, list, i, enum;
  if class!.classNo = 0 then
    return [Representative(class)];
  fi;
  if HasEnumerator(class) then
    enum := Enumerator(class);
    enum!.ElementNumber(enum, infinity);
    return enum!.list;
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
  local s, record, enum, blocks, pairs, meets, i, b, cong;
  # If we already have the elements, use them
  if HasAsList(class) or class!.classNo = 0 then
    return AsList(class);
  fi;

  # cong has not yet been enumerated: make functions
  cong := EquivalenceClassRelation(class);
  s := Range(cong);
  record := rec();

  record.ElementNumber := function(enum, pos)
    local range, newelms, x;
    if pos <= enum!.len then
      return enum!.list[pos];
    fi;
    while (pos > enum!.len) do
      range := enum!.ranges[enum!.nextRange];
      newelms := SemilatticeElementsBetweenNC(s, range[1], range[2]);
      for x in newelms do
        if HTValue(enum!.ht, x) = fail then
          enum!.len := enum!.len + 1;
          enum!.list[enum!.len] := x;
          HTAdd(enum!.ht, x, enum!.len);
        fi;
      od;
      enum!.nextRange := enum!.nextRange + 1;
      if enum!.nextRange > Length(enum!.ranges) then
        SetAsList(class, enum!.list);
        SetLength(class, enum!.len);
        SetEnumerator(class, enum!.list);
        break;
      fi;
    od;
    # If the list is long enough, return the element.
    if pos <= enum!.len then
      return enum!.list[pos];
    fi;
    return fail;
  end;

  record.NumberElement := function(enum, elm)
    local pos;
    if elm in class then
      # elm is in the class
      pos := HTValue(enum!.ht, elm);
      if pos <> fail then
        # elm already has a position
        return pos;
      else
        # put elm in the next available position
        enum!.len := enum!.len + 1;
        enum!.list[enum!.len] := elm;
        HTAdd(enum!.ht, elm, enum!.len);
        return enum!.len;
      fi;
    else
      # elm is not in the class
      return fail;
    fi;
  end;

  record.Length := function(enum)
    # Finish enumerating
    enum!.ElementNumber(enum, infinity);
    return enum!.len;
  end;

  enum := EnumeratorByFunctions(class, record);
  enum!.cong := EquivalenceClassRelation(UnderlyingCollection(enum));
  enum!.rep := Representative(class);
  enum!.list := [enum!.rep];
  enum!.len := 1;
  enum!.ht := HTCreate(enum!.rep);
  HTAdd(enum!.ht, enum!.rep, 1);
  
  # Store all the ranges which might contain elements in this class
  blocks := Positions(BlockCoincidenceTable(enum!.cong), class!.classNo);
  pairs := GeneratingPairsOfSemigroupCongruence(enum!.cong);
  meets := MeetsOfGeneratingPairs(cong);
  enum!.ranges := EmptyPlist(Length(blocks)*2);
  i := 0;
  for b in blocks do
    enum!.ranges[i+1] := [meets[b], pairs[b][1]];
    enum!.ranges[i+2] := [meets[b], pairs[b][2]];
    i := i + 2;
  od;
  enum!.nextRange := 1;

  return enum;
end);

#

InstallMethod(NrCongruenceClasses,
"for a semilattice congruence",
[IsSemilatticeCongruence],
function(cong)
  local classes;
  classes := NonTrivialEquivalenceClasses(cong);
  return Size(Range(cong)) - Sum(classes, Size) + Size(classes);
end);

#

InstallMethod(EquivalenceClasses,
"for a semilattice congruence",
[IsSemilatticeCongruence],
function(cong)
  local s, classes, fam, i, x;
  s := Range(cong);
  classes := ShallowCopy(NonTrivialEquivalenceClasses(cong));
  fam := CollectionsFamily(FamilyObj(Representative(s)));
  i := Length(classes);
  for x in s do
    if SEMIGROUPS_SemilatticeCongClassNoOfElm(cong, x) = 0 then
      i := i + 1;
      classes[i] := Objectify(NewType(fam, IsSemilatticeCongruenceClass),
                              rec(classNo := 0));
      SetParentAttr(classes[i], cong);
      SetEquivalenceClassRelation(classes[i], cong);
      SetRepresentative(classes[i], x);
    fi;
  od;
  return classes;
end);

#

InstallMethod(ImagesElm,
"for a semilattice congruence and an associative element",
[IsSemilatticeCongruence, IsAssociativeElement],
function(cong, elm)
  # Check the arguments
  if not elm in Range(cong) then
    Error("Semigroups: ImagesElm: usage,\n",
          "the second arg <elm> must be in",
          "the semigroup of the first arg <cong>");
    return;
  fi;
  return AsList(EquivalenceClassOfElementNC(cong, elm));
end);

#

InstallMethod(MeetSemigroupCongruences,
"for two semilattice congruences",
[IsSemilatticeCongruence, IsSemilatticeCongruence],
function(cong1, cong2)
  local s, outpairs, gens, pairs1, pairs2, meets1, meets2, i1, i2, max, p, min, 
        gen, newmin;
  s := Range(cong1);
  if s <> Range(cong2) then
    Error("Semigroups: MeetOfSemigroupCongruences: usage,\n",
          "args <cong1> and <cong2> must be over the same semigroup");
    return;
  fi;
  outpairs := [];
  gens := GeneratorsOfSemigroup(s);
  pairs1 := GeneratingPairsOfSemigroupCongruence(cong1);
  pairs2 := GeneratingPairsOfSemigroupCongruence(cong2);
  meets1 := MeetsOfGeneratingPairs(cong1);
  meets2 := MeetsOfGeneratingPairs(cong2);
  for i1 in [1 .. Length(pairs1)] do
    for i2 in [1 .. Length(pairs2)] do
      # Maxima xs, xt, ys, yt
      max := [pairs1[i1][1] * pairs2[i2][1],
              pairs1[i1][1] * pairs2[i2][2],
              pairs1[i1][2] * pairs2[i2][1],
              pairs1[i1][2] * pairs2[i2][2]];
      # Calculate the minimum of each range
      for p in max do
        min := p;
        for gen in gens do
          newmin := min * gen;
          if meets1[i1] * newmin = meets1[i1]
             and meets2[i2] * newmin = meets2[i2] then
            min := newmin;
          fi;
        od;
        Add(outpairs, [min, p]);
      od;
    od;
  od;
  return SemigroupCongruence(s, outpairs);
end);
