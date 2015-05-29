############################################################################
##
#W  cong-pairs.gi
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

DeclareCategory("IsSemigroupCongruenceData", IsRecord);
DeclareOperation("Enumerate", [IsSemigroupCongruenceData, IsFunction]);

#

InstallGlobalFunction(SEMIGROUPS_SetupCongData,
function(cong)
  local s, elms, pairs, hashlen, ht, treehashsize, data, pairstoapply, pos, 
        found, has_changed;

  s := Range(cong);
  elms := ELEMENTS_SEMIGROUP(GenericSemigroupData(s), infinity);
  pairs := List(GeneratingPairsOfSemigroupCongruence(cong),
                x -> [Position(elms, x[1]), Position(elms, x[2])]);

  hashlen := SEMIGROUPS_OptionsRec(s).hashlen.L;
  ht := HTCreate([elms[1], elms[1]], rec(forflatplainlists := true,
                                         treehashsize := hashlen));
  data := rec(cong := cong,
              pairstoapply := pairs,
              pos := 0,
              ht := ht,
              elms := elms,
              found := false,  
              ufdata := UF_NEW(Size(s)));
  cong!.data := Objectify(
                 NewType(FamilyObj(cong), IsSemigroupCongruenceData), data);
  return;
end);

# This is a temporary fix to make congruences know that they are finite
# This allows MT's congruence methods for finite congruences to be selected

# TODO Make finite semigroup congruences be set as such at creation
# TODO Make IsFinite method for IsSemigroupCongruence

for Filter in [IsSemigroupCongruence,
               IsLeftSemigroupCongruence,
               IsRightSemigroupCongruence] do

#
  
DeclareOperation("Enumerate", [Filter, IsFunction]);

#

InstallImmediateMethod(IsFinite,
"for a semigroup congruence",
Filter and HasRange,
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
[IsDenseList, Filter and HasGeneratingPairsOfMagmaCongruence],
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
    Error("Semigroups: \in: usage,\n",
          "this function currently only works if <cong> is a congruence of a ",
          "semigroup\nwhich is known to be finite,");
    return;
  fi;

  elms := ELEMENTS_SEMIGROUP(GenericSemigroupData(s), infinity);
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
    return Enumerate(cong, lookfunc)!.found;
  fi;
end);

#

InstallMethod(AsLookupTable,
"for semigroup congruence",
[Filter],
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
[Filter, IsFunction],
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
  local cong, s, ufdata, pairstoapply, ht, right, left, genstoapply, i, nr, 
        found, x, j, y, next, newtable, ii;

  cong := data!.cong;
  s := Range(cong);
  
  ufdata := data!.ufdata;
  pairstoapply := data!.pairstoapply;
  ht := data!.ht;

  if IsRightSemigroupCongruence(cong) then
    right := RightCayleyGraphSemigroup(s);
  elif IsLeftSemigroupCongruence(cong) then
    left := LeftCayleyGraphSemigroup(s);
  fi;

  genstoapply := [1 .. Size(right[1])];
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
    if IsLeftSemigroupCongruence(cong) then
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

    if IsRightSemigroupCongruence(cong) then
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
  data!.found := found;
  return data;
end);

#

InstallMethod(EquivalenceClasses,
"for a semigroup congruence",
[Filter],
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
  elms := ELEMENTS_SEMIGROUP(GenericSemigroupData(Range(cong)), infinity);
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
  local elms, p, tab;
  elms := ELEMENTS_SEMIGROUP(GenericSemigroupData(Parent(class)), infinity);
  p := Position(elms, Representative(class));
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
[Filter and IsFinite, Filter and IsFinite],
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

InstallMethod(ImagesElm,
"for a semigroup congruence and an associative element",
[Filter, IsAssociativeElement],
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
[Filter and HasGeneratingPairsOfMagmaCongruence],
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
      classno := UF_FIND(data!.ufdata, enum!.rep);
      if classno = UF_FIND(data!.ufdata, lastpair[1]) then
        for i in [1..UF_SIZE(data!.ufdata)] do
          if (not enum!.found[i]) and UF_FIND(data!.ufdata, i) = classno then
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
      for i in [1..Size(Range(enum!.cong))] do
        if table[i] = classno and not enum!.found[i] then
          enum!.found[i] := true;
          enum!.len := enum!.len + 1;
          enum!.list[enum!.len] := i;
        fi;
      od;
      #TODO: erase these 3 lines
      if enum!.len <> Size(class) then
        Error("This should never happen!"); return;
      fi;
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
  enum!.rep := Position(enum!.elms, Representative(UnderlyingCollection(enum)));
  enum!.list := [enum!.rep];
  enum!.found := BlistList([1 .. Size(enum!.elms)], [enum!.rep]);
  enum!.len := 1;
  
  return enum;
end);

#

InstallMethod(ViewObj,
"for a semigroup congruence",
[Filter],
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
[Filter],
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

#

od;
