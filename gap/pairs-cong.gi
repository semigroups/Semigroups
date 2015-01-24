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

InstallGlobalFunction(SEMIGROUPS_SetupCongData,
function(cong)
  local s, elms, pairs, hashlen, ht, data, pairstoapply, pos, found;
  s := Range(cong);
  elms := Elements(s);
  pairs := List( GeneratingPairsOfSemigroupCongruence(cong),
                 x -> [Position(elms, x[1]), Position(elms, x[2])] );

  if IsBound(s!.opts) then
    hashlen := s!.opts.hashlen.L;
  else
    hashlen := SemigroupsOptionsRec.hashlen.L;
  fi;
  ht := HTCreate( [elms[1],elms[1]], rec(forflatplainlists := true,
              treehashsize := hashlen ) );
  data := rec( cong := cong,
               lookup := [1 .. Size(s)],
               pairstoapply := pairs,
               pos := 0,
               ht := ht,
               elms := elms,
               found := false );
  cong!.data := Objectify(
                 NewType(FamilyObj(cong), IsSemigroupCongruenceData), data);
  return;
end);

#

InstallMethod(\in,
"for dense list and semigroup congruence",
[IsDenseList, IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
function(pair, cong)
  local s, elms, p1, p2, table, find, lookfunc;
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
    TryNextMethod();
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
    if not IsBound(cong!.data) then
      SEMIGROUPS_SetupCongData(cong);
    fi;
    find := function(table,i)
      while table[i] <> i do
        i := table[i];
      od;
      return i;
    end;
    lookfunc := function(data, lastpair)
      return find(data!.lookup, p1) = find(data!.lookup, p2);
    end;
    return Enumerate(cong!.data, lookfunc)!.found;
  fi;
end);

#

InstallMethod(AsLookupTable,
"for semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  if not (HasIsFinite(Range(cong)) and IsFinite(Range(cong))) then
    Error("Semigroups: AsLookupTable: usage,\n",
          "<cong> must be a finite semigroup");
    return;
  fi;
  if not IsBound(cong!.data) then
    SEMIGROUPS_SetupCongData(cong);
  fi;
  Enumerate(cong!.data, ReturnFalse);
  return AsLookupTable(cong);
end);

#

InstallMethod(Enumerate,
"for semigroup congruence data and a function",
[IsSemigroupCongruenceData, IsFunction],
function(data, lookfunc)
  local cong, s, table, pairstoapply, ht, right, left, find, union, genstoapply,
        i, nr, found, x, j, y, next, newtable, ii, result;
  cong := data!.cong;
  s := Range(cong);

  table := data!.lookup;
  pairstoapply := data!.pairstoapply;
  ht := data!.ht;

  right := RightCayleyGraphSemigroup(s);
  left := LeftCayleyGraphSemigroup(s);

  find := function(i)
    while table[i] <> i do
      i := table[i];
    od;
    return i;
  end;

  union := function(pair)
    local ii, jj;

    ii := find(pair[1]);
    jj := find(pair[2]);

    if ii < jj then
      table[jj] := ii;
    elif jj < ii then
      table[ii] := jj;
    fi;
  end;

  genstoapply := [1 .. Size(right[1])];
  i := data!.pos;
  nr := Size(pairstoapply);
  found := false;

  if i = 0 then
    # Add the generating pairs themselves
    for x in pairstoapply do
      if x[1] <> x[2] and HTValue(ht, x) = fail then
        HTAdd(ht, x, true);
        union(x);
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
        union(y);
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
        union(y);
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
    ii := find(i);
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
    TryNextMethod();
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
  return EquivalenceClassRelation(class1) = EquivalenceClassRelation(class2) and
         [Representative(class1), Representative(class2)]
         in EquivalenceClassRelation(class1);
end);

#

InstallMethod(\=,
"for two semigroup congruences",
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
                 Representative(class1) * Representative(class2));
end);

#

InstallMethod(NrCongruenceClasses,
"for a semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  local s;
  s := Range(cong);
  if not (HasIsFinite(s) and IsFinite(s)) then
    TryNextMethod();
  fi;
  return Maximum(AsLookupTable(cong));
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
