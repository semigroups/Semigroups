############################################################################
##
#W  pairs-cong.gi
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains functions for any finite semigroup congruence with
## generating pairs, using a union-find method.
##

InstallGlobalFunction(SEMIGROUPS_UF_Find,
function(table, i)
  while table[i] <> i do
    i := table[i];
  od;
  return i;
end);

#

InstallGlobalFunction(SEMIGROUPS_UF_Union,
function(table, pair)
  local ii, jj;

  ii := SEMIGROUPS_UF_Find(table, pair[1]);
  jj := SEMIGROUPS_UF_Find(table, pair[2]);

  if ii < jj then
    table[jj] := ii;
  elif jj < ii then
    table[ii] := jj;
  fi;
end);

#

InstallGlobalFunction(SEMIGROUPS_SetupCongData,
function(cong)
  local s, elms, pairs, hashlen, ht, data;

  s := Range(cong);
  elms := AsSSortedList(s);
  pairs := List(GeneratingPairsOfSemigroupCongruence(cong),
                x -> [Position(elms, x[1]), Position(elms, x[2])]);

  hashlen := SEMIGROUPS_OptionsRec(s).hashlen.L;

  ht := HTCreate([elms[1], elms[1]], rec(forflatplainlists := true,
                                         treehashsize := hashlen));
  data := rec(cong := cong,
              lookup := [1 .. Size(s)],
              pairstoapply := pairs,
              pos := 0,
              ht := ht,
              elms := elms,
              found := false);
  cong!.data := Objectify(NewType(FamilyObj(cong), IsSemigroupCongruenceData),
                          data);
  return;
end);

# This is a temporary fix to make congruences know that they are finite
# This allows MT's congruence methods for finite congruences to be selected

# TODO Make finite semigroup congruences be set as such at creation
# TODO Make IsFinite method for IsSemigroupCongruence
InstallImmediateMethod(IsFinite,
"for a semigroup congruence",
IsSemigroupCongruence and HasRange,
0,
function(cong)
  if HasIsFinite(Range(cong)) and IsFinite(Range(cong)) then
    return true;
  fi;
  TryNextMethod();
end);

#

InstallMethod(\in,
"for dense list and semigroup congruence",
[IsDenseList, IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
function(pair, cong)
  local s, elms, p1, p2, table, lookfunc;
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
  if not (HasIsFinite(s) and IsFinite(s)) then
    Error("Semigroups: \in: usage,\n",
          "this function currently only works if <cong> is a congruence of a ",
          "semigroup\nwhich is known to be finite,");
    return;
  fi;

  elms := Elements(s);
  p1 := Position(elms, pair[1]);
  p2 := Position(elms, pair[2]);

  # Use lookup table if available
  if HasAsLookupTable(cong) then
    table := AsLookupTable(cong);
    return table[p1] = table[p2];
  else
    # Otherwise, begin calculating the lookup table and look for this pair
    lookfunc := function(data, lastpair)
      return SEMIGROUPS_UF_Find(data!.lookup, p1)
             = SEMIGROUPS_UF_Find(data!.lookup, p2);
    end;
    return Enumerate(cong, lookfunc)!.found;
  fi;
end);

#

InstallMethod(AsLookupTable,
"for semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  if not (HasIsFinite(Range(cong)) and IsFinite(Range(cong))) then
    Error("Semigroups: AsLookupTable: usage,\n",
          "<cong> must be a congruence of a finite semigroup,");
    return;
  fi;
  Enumerate(cong, ReturnFalse);
  return AsLookupTable(cong);
end);

#

InstallMethod(Enumerate,
"for a semigroup congruence and a function",
[IsSemigroupCongruence, IsFunction],
function(cong, lookfunc)
  if HasAsLookupTable(cong) then
    return fail;
  fi;
  if not IsBound(cong!.data) then
    SEMIGROUPS_SetupCongData(cong);
  fi;
  return Enumerate(cong!.data, lookfunc);
end);

#

InstallMethod(Enumerate,
"for semigroup congruence data and a function",
[IsSemigroupCongruenceData, IsFunction],
function(data, lookfunc)
  local cong, s, table, pairstoapply, ht, right, left, genstoapply, i, nr,
        found, x, j, y, next, newtable, ii;

  cong := data!.cong;
  s := Range(cong);

  table := data!.lookup;
  pairstoapply := data!.pairstoapply;
  ht := data!.ht;

  right := RightCayleyGraphSemigroup(s);
  left := LeftCayleyGraphSemigroup(s);

  genstoapply := [1 .. Size(right[1])];
  i := data!.pos;
  nr := Size(pairstoapply);
  found := false;

  if i = 0 then
    # Add the generating pairs themselves
    for x in pairstoapply do
      if x[1] <> x[2] and HTValue(ht, x) = fail then
        HTAdd(ht, x, true);
        SEMIGROUPS_UF_Union(table, x);
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
    for j in genstoapply do
      # Add the pair's left-multiples
      y := [right[x[1]][j], right[x[2]][j]];
      if y[1] <> y[2] and HTValue(ht, y) = fail then
        HTAdd(ht, y, true);
        nr := nr + 1;
        pairstoapply[nr] := y;
        SEMIGROUPS_UF_Union(table, y);
        if lookfunc(data, y) then
          found := true;
        fi;
      fi;

      # Add the pair's right-multiples
      y := [left[x[1]][j], left[x[2]][j]];
      if y[1] <> y[2] and HTValue(ht, y) = fail then
        HTAdd(ht, y, true);
        nr := nr + 1;
        pairstoapply[nr] := y;
        SEMIGROUPS_UF_Union(table, y);
        if lookfunc(data, y) then
          found := true;
        fi;
      fi;
    od;

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
  for i in [1 .. Size(table)] do
    ii := SEMIGROUPS_UF_Find(table, i);
    if ii = i then
      newtable[i] := next;
      next := next + 1;
    else
      newtable[i] := newtable[ii];
    fi;
  od;

  SetAsLookupTable(cong, newtable);
  Unbind(cong!.data);
  data!.found := found;
  return data;
end);

#

InstallMethod(EquivalenceClasses,
"for a semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  local classes, next, tab, elms, i;
  if not (HasIsFinite(Range(cong)) and IsFinite(Range(cong))) then
    Error("Semigroups: EquivalenceClasses: usage,\n",
          "this function currently only works if <cong> is a congruence of a ",
          "semigroup\nwhich is known to be finite,");
    return;
  fi;
  classes := [];
  next := 1;
  tab := AsLookupTable(cong);
  elms := Elements(Range(cong));
  for i in [1 .. Size(tab)] do
    if tab[i] = next then
      classes[next] := EquivalenceClassOfElementNC(cong, elms[i]);
      next := next + 1;
    fi;
  od;
  return classes;
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
  local p, tab;
  p := Position(Elements(Parent(class)), Representative(class));
  tab := AsLookupTable(EquivalenceClassRelation(class));
  return Number(tab, n -> n = tab[p]);
end);

#

InstallMethod(\=,
"for two congruence classes",
[IsCongruenceClass and IsFinite, IsCongruenceClass and IsFinite],
function(class1, class2)
  return EquivalenceClassRelation(class1) = EquivalenceClassRelation(class2)
    and [Representative(class1), Representative(class2)]
        in EquivalenceClassRelation(class1);
end);

#

InstallMethod(\=,
"for two finite semigroup congruences",
[IsSemigroupCongruence and IsFinite, IsSemigroupCongruence and IsFinite],
function(cong1, cong2)
  return Range(cong1) = Range(cong2) and
         AsLookupTable(cong1) = AsLookupTable(cong2);
end);

#

InstallMethod(\*,
"for two semigroup congruence classes",
[IsCongruenceClass, IsCongruenceClass],
function(class1, class2)
  if EquivalenceClassRelation(class1) <> EquivalenceClassRelation(class2) then
    Error("Semigroups: \*: usage,\n",
          "the args must be classes of the same congruence,");
    return;
  fi;
  return CongruenceClassOfElement(EquivalenceClassRelation(class1),
                                  Representative(class1) *
                                  Representative(class2));
end);

#

InstallMethod(ImagesElm,
"for a semigroup congruence and an associative element",
[IsSemigroupCongruence, IsAssociativeElement],
function(cong, elm)
  local elms, lookup, classNo;
  elms := AsSSortedList(Range(cong));
  lookup := AsLookupTable(cong);
  classNo := lookup[Position(elms, elm)];
  return elms{Positions(lookup, classNo)};
end);

#

InstallMethod(AsList,
"for a semigroup congruence class",
[IsCongruenceClass],
function(class)
  return ImagesElm(EquivalenceClassRelation(class), Representative(class));
end);

#

InstallMethod(NrCongruenceClasses,
"for a semigroup congruence with generating pairs",
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
function(cong)
  local s;
  s := Range(cong);
  if not (HasIsFinite(s) and IsFinite(s)) then
    Error("Semigroups: NrCongruenceClasses: usage,\n",
          "this function currently only works if <cong> is a congruence of a ",
          "semigroup\nwhich is known to be finite,");
    return;
  fi;
  return Maximum(AsLookupTable(cong));
end);

#

InstallMethod(Enumerator,
"for a semigroup congruence class",
[IsCongruenceClass],
function(class)
  local cong, s, record, enum;

  cong := EquivalenceClassRelation(class);
  s := Range(cong);

  if not (HasIsFinite(s) and IsFinite(s)) then
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
      classno := SEMIGROUPS_UF_Find(data!.lookup, enum!.rep);
      if classno = SEMIGROUPS_UF_Find(data!.lookup, lastpair[1]) then
        for i in [1 .. Size(data!.lookup)] do
          if (not enum!.found[i])
              and SEMIGROUPS_UF_Find(data!.lookup, i) = classno then
            enum!.found[i] := true;
            enum!.len := enum!.len + 1;
            enum!.list[enum!.len] := i;
          fi;
        od;
      fi;
      return enum!.len >= pos;
    end;
    result := Enumerate(enum!.cong, lookfunc);
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
    local x;
    x := Position(enum!.elms, elm);
    if [x, enum!.rep] in enum!.cong then # this does the calculations
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
  enum!.elms := AsSSortedList(Range(enum!.cong));
  enum!.rep := Position(enum!.elms,
                        Representative(UnderlyingCollection(enum)));
  enum!.list := [enum!.rep];
  enum!.found := BlistList([1 .. Size(enum!.elms)], [enum!.rep]);
  enum!.len := 1;

  return enum;
end);

#

InstallMethod(ViewObj,
"for a semigroup congruence",
[IsSemigroupCongruence],
1,
function(cong)
  Print("<semigroup congruence over ");
  ViewObj(Range(cong));
  if HasGeneratingPairsOfMagmaCongruence(cong) then
    Print(" with ", Size(GeneratingPairsOfSemigroupCongruence(cong)),
          " generating pairs");
  fi;
  Print(">");
end);

#

InstallMethod(PrintObj,
"for a semigroup congruence",
[IsSemigroupCongruence],
1,
function(cong)
  Print("SemigroupCongruence( ");
  PrintObj(Range(cong));
  Print(", ");
  if HasGeneratingPairsOfMagmaCongruence(cong) then
    Print(GeneratingPairsOfSemigroupCongruence(cong));
  fi;
  Print(")");
end);
