############################################################################
##
#W  congruences/pairs.gi
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains functions for any finite semigroup congruence with
## generating pairs, using a union-find method.  See Howie 1.5 and see MT's
## MSc thesis "Computing with Semigroup Congruences", chapter 2
##

InstallGlobalFunction(SEMIGROUPS_SetupCongData,
function(cong)
  local S, elms, pairs, hashlen, ht, data, genpairs, right_compat, left_compat;

  S := Range(cong);
  elms := SEMIGROUP_ELEMENTS(GenericSemigroupData(S), infinity);

  # Is this a left, right, or 2-sided congruence?
  if HasGeneratingPairsOfMagmaCongruence(cong) then
    genpairs := GeneratingPairsOfSemigroupCongruence(cong);
    left_compat := true;
    right_compat := true;
  elif HasGeneratingPairsOfLeftMagmaCongruence(cong) then
    genpairs := GeneratingPairsOfLeftSemigroupCongruence(cong);
    left_compat := true;
    right_compat := false;
  elif HasGeneratingPairsOfRightMagmaCongruence(cong) then
    genpairs := GeneratingPairsOfRightSemigroupCongruence(cong);
    left_compat := false;
    right_compat := true;
  fi;

  pairs := List(genpairs, x -> [Position(elms, x[1]), Position(elms, x[2])]);
  hashlen := SEMIGROUPS_OptionsRec(S).hashlen.L;

  ht := HTCreate([elms[1], elms[1]], rec(forflatplainlists := true,
                                         treehashsize := hashlen));
  data := rec(cong := cong,
              left_compat := left_compat,
              right_compat := right_compat,
              pairstoapply := pairs,
              pos := 0,
              ht := ht,
              elms := elms,
              found := false,
              ufdata := UF_NEW(Size(S)));
  cong!.data := Objectify(NewType(FamilyObj(cong),
                                  SEMIGROUPS_IsSemigroupCongruenceData),
                          data);
end);

#

# Install the following methods for three different filters
install_pairs_methods_with_filter@ := function(cong_type)
  InstallImmediateMethod(IsFinite,
  Concatenation("for a ", cong_type.string),
  cong_type.filter and HasRange,
  0,
  function(cong)
    if HasIsFinite(Range(cong)) and IsFinite(Range(cong)) then
      return true;
    fi;
    TryNextMethod();
  end);

  #

  InstallMethod(\in,
  Concatenation("for dense list and ", cong_type.string),
  [IsDenseList, cong_type.filter and cong_type.haspairs],
  function(pair, cong)
    local S, elms, p1, p2, table, lookfunc;

    # Input checks
    if Size(pair) <> 2 then
      ErrorMayQuit("Semigroups: \\in: usage,\n",
                   "the first arg <pair> must be a list of length 2,");
    fi;
    S := Range(cong);
    if not (pair[1] in S and pair[2] in S) then
      ErrorMayQuit("Semigroups: \\in: usage,\n",
                   "elements of the first arg <pair> must be\n",
                   "in the range of the second arg <cong>,");
    fi;
    if not (HasIsFinite(S) and IsFinite(S)) then
      ErrorMayQuit("Semigroups: \\in: usage,\n",
                   "this function currently only works if <cong> is a ",
                   "congruence of a semigroup\nwhich is known to be finite,");
    fi;

    elms := SEMIGROUP_ELEMENTS(GenericSemigroupData(S), infinity);
    p1 := Position(elms, pair[1]);
    p2 := Position(elms, pair[2]);

    # Use lookup table if available
    if HasAsLookupTable(cong) then
      table := AsLookupTable(cong);
      return table[p1] = table[p2];
    else
      # Otherwise, begin calculating the lookup table and look for this pair
      lookfunc := function(data, lastpair)
        return UF_FIND(data!.ufdata, p1)
               = UF_FIND(data!.ufdata, p2);
      end;
      return SEMIGROUPS_Enumerate(cong, lookfunc)!.found;
    fi;
  end);

  #

  InstallMethod(AsLookupTable,
  Concatenation("for a ", cong_type.string),
  [cong_type.filter],
  function(cong)
    if not (HasIsFinite(Range(cong)) and IsFinite(Range(cong))) then
      ErrorMayQuit("Semigroups: AsLookupTable: usage,\n",
                   "<cong> must be a congruence of a finite semigroup,");
    fi;
    SEMIGROUPS_Enumerate(cong, ReturnFalse);
    return AsLookupTable(cong);
  end);

  #

  InstallMethod(SEMIGROUPS_Enumerate,
  Concatenation("for a ", cong_type.string, " and a function"),
  [cong_type.filter, IsFunction],
  function(cong, lookfunc)
    if HasAsLookupTable(cong) then
      return fail;
    fi;
    if not IsBound(cong!.data) then
      SEMIGROUPS_SetupCongData(cong);
    fi;
    return SEMIGROUPS_Enumerate(cong!.data, lookfunc);
  end);

  #

  InstallMethod(SEMIGROUPS_Enumerate,
  "for semigroup congruence data and a function",
  [SEMIGROUPS_IsSemigroupCongruenceData, IsFunction],
  function(data, lookfunc)
    local cong, S, ufdata, pairstoapply, ht, right, left, genstoapply, i, nr,
          found, x, j, y, next, newtable, ii, left_compat, right_compat;

    cong := data!.cong;
    S := Range(cong);

    ufdata := data!.ufdata;
    pairstoapply := data!.pairstoapply;
    ht := data!.ht;
    left_compat := data!.left_compat;
    right_compat := data!.right_compat;

    if left_compat then
      left := LeftCayleyGraphSemigroup(S);
    fi;
    if right_compat then
      right := RightCayleyGraphSemigroup(S);
    fi;

    genstoapply := [1 .. Size(GeneratorsOfSemigroup(S))];
    i := data!.pos;
    nr := Size(pairstoapply);
    found := false;

    if i = 0 then
      # Add the generating pairs themselves
      for x in pairstoapply do
        if x[1] <> x[2] and HTValue(ht, x) = fail then
          HTAdd(ht, x, true);
          UF_UNION(ufdata, x);
          # Have we found what we were looking for?
          if lookfunc(data, x) then
            data!.found := true;
            return data;
          fi;
        fi;
      od;
    fi;

    while i < nr do
      i := i + 1;
      x := pairstoapply[i];
      # Add the pair's left-multiples
      if left_compat then
        for j in genstoapply do
          y := [left[x[1]][j], left[x[2]][j]];
          if y[1] <> y[2] and HTValue(ht, y) = fail then
            HTAdd(ht, y, true);
            nr := nr + 1;
            pairstoapply[nr] := y;
            UF_UNION(ufdata, y);
            if lookfunc(data, y) then
              found := true;
            fi;
          fi;
        od;
      fi;

      if right_compat then
        # Add the pair's right-multiples
        for j in genstoapply do
          y := [right[x[1]][j], right[x[2]][j]];
          if y[1] <> y[2] and HTValue(ht, y) = fail then
            HTAdd(ht, y, true);
            nr := nr + 1;
            pairstoapply[nr] := y;
            UF_UNION(ufdata, y);
            if lookfunc(data, y) then
              found := true;
            fi;
          fi;
        od;
      fi;

      if found then
        # Save our place
        data!.pos := i;
        data!.found := found;
        return data;
      fi;
    od;

    # "Normalise" the table for clean lookup
    next := 1;
    newtable := [];
    for i in [1 .. UF_SIZE(ufdata)] do
      ii := UF_FIND(ufdata, i);
      if ii = i then
        newtable[i] := next;
        next := next + 1;
      else
        newtable[i] := newtable[ii];
      fi;
    od;

    SetAsLookupTable(cong, newtable);
    Unbind(cong!.data);
    data!.found := lookfunc(data, fail);
    return data;
  end);

  #

  InstallMethod(EquivalenceClasses,
  Concatenation("for a ", cong_type.string),
  [cong_type.filter],
  function(cong)
    local classes, next, tab, elms, i;

    if not (HasIsFinite(Range(cong)) and IsFinite(Range(cong))) then
      ErrorMayQuit("Semigroups: EquivalenceClasses: usage,\n",
                   "this function currently only works if <cong> is a ",
                   "congruence of a semigroup\nwhich is known to be finite,");
    fi;
    classes := [];
    next := 1;
    tab := AsLookupTable(cong);
    elms := SEMIGROUP_ELEMENTS(GenericSemigroupData(Range(cong)), infinity);
    for i in [1 .. Size(tab)] do
      if tab[i] = next then
        classes[next] := EquivalenceClassOfElementNC(cong, elms[i]);
        next := next + 1;
      fi;
    od;
    return classes;
  end);

  #

  InstallMethod(NonTrivialEquivalenceClasses,
  Concatenation("for a ", cong_type.string),
  [cong_type.filter],
  function(cong)
    local classes;

    if not (HasIsFinite(Range(cong)) and IsFinite(Range(cong))) then
      ErrorMayQuit("Semigroups: NonTrivialEquivalenceClasses: usage,\n",
                   "this function currently only works if <cong> is a ",
                   "congruence of a semigroup\nwhich is known to be finite,");
    fi;
    classes := EquivalenceClasses(cong);
    return Filtered(classes, c -> Size(c) > 1);
  end);

  #

  InstallMethod(\in,
  "for an associative element and a finite congruence class",
  [IsAssociativeElement, IsCongruenceClass and IsFinite],
  function(elm, class)
    return [elm, Representative(class)] in EquivalenceClassRelation(class);
  end);

  #

  InstallMethod(Size,
  "for a finite congruence class",
  [IsCongruenceClass and IsFinite],
  function(class)
    local elms, p, tab;
    elms := SEMIGROUP_ELEMENTS(GenericSemigroupData(Parent(class)), infinity);
    p := Position(elms, Representative(class));
    tab := AsLookupTable(EquivalenceClassRelation(class));
    return Number(tab, n -> n = tab[p]);
  end);

  #

  InstallMethod(\=,
  "for two congruence classes",
  [IsCongruenceClass, IsCongruenceClass],
  function(class1, class2)
    return EquivalenceClassRelation(class1) = EquivalenceClassRelation(class2)
      and [Representative(class1), Representative(class2)]
          in EquivalenceClassRelation(class1);
  end);

  #

  InstallMethod(\=,
  Concatenation("for two finite ", cong_type.string, "s"),
  [cong_type.filter and cong_type.haspairs and IsFinite,
   cong_type.filter and cong_type.haspairs and IsFinite],
  function(cong1, cong2)
    return Range(cong1) = Range(cong2)
           and ForAll(cong_type.pairs(cong1), pair -> pair in cong2)
           and ForAll(cong_type.pairs(cong2), pair -> pair in cong1);
  end);

  #

  InstallMethod(\*,
  "for two congruence classes",
  [IsCongruenceClass, IsCongruenceClass],
  function(class1, class2)
    if EquivalenceClassRelation(class1) <> EquivalenceClassRelation(class2) then
      ErrorMayQuit("Semigroups: \*: usage,\n",
                   "the args must be classes of the same congruence,");
    fi;
    return CongruenceClassOfElement(EquivalenceClassRelation(class1),
                                    Representative(class1) *
                                    Representative(class2));
  end);

  #

  InstallMethod(ImagesElm,
  Concatenation("for a ", cong_type.string, " and an associative element"),
  [cong_type.filter, IsAssociativeElement],
  function(cong, elm)
    local elms, lookup, classNo;
    elms := SEMIGROUP_ELEMENTS(GenericSemigroupData(Range(cong)), infinity);
    lookup := AsLookupTable(cong);
    classNo := lookup[Position(elms, elm)];
    return elms{Positions(lookup, classNo)};
  end);

  #

  InstallMethod(AsList,
  "for a congruence class",
  [IsCongruenceClass],
  function(class)
    return ImagesElm(EquivalenceClassRelation(class), Representative(class));
  end);

  #

  InstallMethod(NrCongruenceClasses,
  Concatenation("for a ", cong_type.string, " with generating pairs"),
  [cong_type.filter and cong_type.haspairs],
  function(cong)
    local S;
    S := Range(cong);
    if not (HasIsFinite(S) and IsFinite(S)) then
      ErrorMayQuit("Semigroups: NrCongruenceClasses: usage,\n",
                   "this function currently only works if <cong> is a ",
                   "congruence of a semigroup\nwhich is known to be finite,");
    fi;
    return Maximum(AsLookupTable(cong));
  end);

  #

  InstallMethod(Enumerator,
  "for a congruence class",
  [IsCongruenceClass],
  function(class)
    local cong, S, record, enum;

    cong := EquivalenceClassRelation(class);
    S := Range(cong);

    if not (HasIsFinite(S) and IsFinite(S)) then
      TryNextMethod();
    fi;

    # cong has been enumerated: return a list
    if HasAsLookupTable(cong) then
      return Enumerator(AsList(class));
    fi;

    # cong has not yet been enumerated: make functions
    record := rec();

    record.ElementNumber := function(enum, pos)
      local lookfunc, result, table, classno, i;
      if pos <= enum!.len then
        return enum!.elms[enum!.list[pos]];
      fi;
      lookfunc := function(data, lastpair)
        local classno, i;
        classno := UF_FIND(data!.ufdata, enum!.rep);
        if classno = UF_FIND(data!.ufdata, lastpair[1]) then
          for i in [1 .. UF_SIZE(data!.ufdata)] do
            if (not enum!.found[i]) and UF_FIND(data!.ufdata, i) = classno then
              enum!.found[i] := true;
              enum!.len := enum!.len + 1;
              enum!.list[enum!.len] := i;
            fi;
          od;
        fi;
        return enum!.len >= pos;
      end;
      result := SEMIGROUPS_Enumerate(enum!.cong, lookfunc);
      if result = fail then
        # cong has AsLookupTable
        table := AsLookupTable(enum!.cong);
        classno := table[enum!.rep];
        for i in [1 .. Size(Range(enum!.cong))] do
          if table[i] = classno and not enum!.found[i] then
            enum!.found[i] := true;
            enum!.len := enum!.len + 1;
            enum!.list[enum!.len] := i;
          fi;
        od;
        SetSize(class, enum!.len);
        SetAsList(class, enum!.list);
      fi;
      if pos <= enum!.len then
        return enum!.elms[enum!.list[pos]];
      else
        return fail;
      fi;
    end;

    record.NumberElement := function(enum, elm)
      local x, lookfunc, result, table, classno, i;
      x := Position(enum!.elms, elm);
      lookfunc := function(data, lastpair)
        return UF_FIND(data!.ufdata, x)
               = UF_FIND(data!.ufdata, enum!.rep);
      end;
      result := SEMIGROUPS_Enumerate(enum!.cong, lookfunc);
      if result = fail then
        # cong has AsLookupTable
        result := fail;
        table := AsLookupTable(enum!.cong);
        classno := table[enum!.rep];
        for i in [1 .. Size(Range(enum!.cong))] do
          if table[i] = classno and not enum!.found[i] then
            enum!.found[i] := true;
            enum!.len := enum!.len + 1;
            enum!.list[enum!.len] := i;
            if i = x then
              result := enum!.len;
            fi;
          fi;
        od;
        SetSize(class, enum!.len);
        SetAsList(class, enum!.list);
        return result;
      elif result!.found then
        # elm is in the class
        if enum!.found[x] then
          # elm already has a position
          return Position(enum!.list, x);
        else
          # put elm in the next available position
          enum!.found[x] := true;
          enum!.len := enum!.len + 1;
          enum!.list[enum!.len] := x;
          return enum!.len;
        fi;
      else
        # elm is not in the class
        return fail;
      fi;
    end;

    enum := EnumeratorByFunctions(class, record);
    enum!.cong := EquivalenceClassRelation(UnderlyingCollection(enum));
    enum!.elms := SEMIGROUP_ELEMENTS(GenericSemigroupData(Range(enum!.cong)),
                                     infinity);
    enum!.rep := Position(enum!.elms,
                          Representative(UnderlyingCollection(enum)));
    enum!.list := [enum!.rep];
    enum!.found := BlistList([1 .. Size(enum!.elms)], [enum!.rep]);
    enum!.len := 1;

    return enum;
  end);

  #

  InstallMethod(ViewObj,
  Concatenation("for a ", cong_type.string),
  [cong_type.filter and cong_type.haspairs],
  function(cong)
    Print("<");
    Print(cong_type.string);
    Print(" over ");
    ViewObj(Range(cong));
    Print(" with ", Size(cong_type.pairs(cong)),
          " generating pairs");
    Print(">");
  end);

  #

  InstallMethod(PrintObj,
  Concatenation("for a ", cong_type.string),
  [cong_type.filter and cong_type.haspairs],
  function(cong)
    if cong_type.filter = IsLeftSemigroupCongruence then
      Print("Left");
    elif cong_type.filter = IsRightSemigroupCongruence then
      Print("Right");
    fi;
    Print("SemigroupCongruence( ");
    PrintObj(Range(cong));
    Print(", ");
    Print(cong_type.pairs(cong));
    Print(" )");
  end);

end;

for cong_type@ in [rec(filter := IsSemigroupCongruence,
                       string := "semigroup congruence",
                       pairs := GeneratingPairsOfSemigroupCongruence,
                       haspairs := HasGeneratingPairsOfMagmaCongruence),
                   rec(filter := IsLeftSemigroupCongruence,
                       string := "left semigroup congruence",
                       pairs := GeneratingPairsOfLeftSemigroupCongruence,
                       haspairs := HasGeneratingPairsOfLeftMagmaCongruence),
                   rec(filter := IsRightSemigroupCongruence,
                       string := "right semigroup congruence",
                       pairs := GeneratingPairsOfRightSemigroupCongruence,
                       haspairs := HasGeneratingPairsOfRightMagmaCongruence)] do
  install_pairs_methods_with_filter@(cong_type@);
od;

# Finished installing methods for all three filters
Unbind(cong_type@);
Unbind(install_pairs_methods_with_filter@);

#

InstallMethod(CongruencesOfSemigroup,
"for a semigroup",
[IsSemigroup],
function(S)
  local elms, pairs, congs1, pair, newcong, congs, length, found, start, i, j;
  elms := SEMIGROUP_ELEMENTS(GenericSemigroupData(S), infinity);

  # Get all non-reflexive pairs in SxS
  pairs := Combinations(elms, 2);

  # Get all the unique 1-generated congruences
  Info(InfoSemigroups, 1, "Getting all 1-generated congruences...");
  #congs1 := Set(pairs, pair -> SemigroupCongruence(S, pair));
  congs1 := [];
  for pair in pairs do
    newcong := SemigroupCongruence(S, pair);
    if not ForAny(congs1, cong -> newcong = cong) then
      Add(congs1, newcong);
    fi;
  od;
  congs := ShallowCopy(congs1);

  # Take all their joins
  Info(InfoSemigroups, 1, "Taking joins...");
  length := 0;
  found := true;
  while found do
    start := length + 1;
    found := false;
    length := Length(congs);
    for i in [start .. length] do
      for j in [1 .. Length(congs1)] do
        newcong := JoinSemigroupCongruences(congs[i], congs1[j]);
        if not newcong in congs then
          Add(congs, newcong);
          found := true;
        fi;
      od;
    od;
  od;

  Add(congs, SemigroupCongruence(S, []));
  return congs;
end);

#

InstallMethod(IsSubcongruence,
"for two semigroup congruences",
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence,
 IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
function(cong1, cong2)
  # Tests whether cong2 is a subcongruence of cong1
  if Range(cong1) <> Range(cong2) then
    ErrorMayQuit("Semigroups: IsSubcongruence: usage,\n",
                 "congruences must be defined over the same semigroup,");
  fi;
  return ForAll(GeneratingPairsOfSemigroupCongruence(cong2),
                pair -> pair in cong1);
end);

#

InstallMethod(LatticeOfCongruences,
"for a semigroup",
[IsSemigroup],
function(S)
  local elms, pairs, congs1, nrcongs, children, parents, pair, badcong,
        newchildren, newparents, newcong, i, congs, length, found, start, j, k;
  elms := SEMIGROUP_ELEMENTS(GenericSemigroupData(S), infinity);

  # Get all non-reflexive pairs in SxS
  pairs := Combinations(elms, 2);

  # Get all the unique 1-generated congruences
  Info(InfoSemigroups, 1, "Getting all 1-generated congruences...");
  congs1 := [];     # List of all congruences found so far
  nrcongs := 0;     # Number of congruences found so far
  children := [];   # List of lists of children
  parents := [];    # List of lists of parents
  for pair in pairs do
    badcong := false;
    newchildren := []; # Children of newcong
    newparents := [];  # Parents of newcong
    newcong := SemigroupCongruence(S, pair);
    for i in [1 .. Length(congs1)] do
      if IsSubcongruence(congs1[i], newcong) then
        if IsSubcongruence(newcong, congs1[i]) then
          # This is not a new congruence - drop it!
          badcong := true;
          break;
        else
          Add(newparents, i);
        fi;
      elif IsSubcongruence(newcong, congs1[i]) then
        Add(newchildren, i);
      fi;
    od;
    if not badcong then
      nrcongs := nrcongs + 1;
      congs1[nrcongs] := newcong;
      children[nrcongs] := newchildren;
      parents[nrcongs] := newparents;
      for i in newchildren do
        Add(parents[i], nrcongs);
      od;
      for i in newparents do
        Add(children[i], nrcongs);
      od;
    fi;
  od;
  congs := ShallowCopy(congs1);

  # Take all their joins
  Info(InfoSemigroups, 1, "Taking joins...");
  length := 0;
  found := true;
  while found do
    # There are new congruences to try joining
    start := length + 1; # New congruences start here
    found := false;      # Have we found any more congruence on this sweep?
    for i in [start .. Length(congs)] do # for each new congruence
      for j in [1 .. Length(congs1)] do  # for each 1-generated congruence
        newcong := JoinSemigroupCongruences(congs[i], congs1[j]);
        badcong := false;  # Is newcong the same as another congruence?
        newchildren := []; # Children of newcong
        newparents := [];  # Parents of newcong
        for k in [1 .. Length(congs)] do
          if IsSubcongruence(congs[k], newcong) then
            if IsSubcongruence(newcong, congs[k]) then
              # This is the same as an old congruence - discard it!
              badcong := true;
              break;
            else
              Add(newparents, k);
            fi;
          elif IsSubcongruence(newcong, congs[k]) then
            Add(newchildren, k);
          fi;
        od;
        if not badcong then
          nrcongs := nrcongs + 1;
          congs[nrcongs] := newcong;
          children[nrcongs] := newchildren;
          parents[nrcongs] := newparents;
          for i in newchildren do
            Add(parents[i], nrcongs);
          od;
          for i in newparents do
            Add(children[i], nrcongs);
          od;
          found := true;
        fi;
      od;
    od;
  od;

  # Add the trivial congruence at the start
  children := Concatenation([[]], children + 1);
  for i in [2 .. nrcongs] do
    Add(children[i], 1, 1);
  od;
  Add(congs, SemigroupCongruence(S, []), 1);

  SetCongruencesOfSemigroup(S, congs);
  return children;
end);

#

InstallMethod(JoinSemigroupCongruences,
"for two semigroup congruences",
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence,
 IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
function(c1, c2)
  local pairs;
  # TODO: combine lookup tables
  if Range(c1) <> Range(c2) then
    ErrorMayQuit("Semigroups: JoinSemigroupCongruences: usage,\n",
                 "congruences must be defined over the same semigroup,");
  fi;
  pairs := Concatenation(ShallowCopy(GeneratingPairsOfSemigroupCongruence(c1)),
                         ShallowCopy(GeneratingPairsOfSemigroupCongruence(c2)));
  return SemigroupCongruence(Range(c1), pairs);
end);

#
