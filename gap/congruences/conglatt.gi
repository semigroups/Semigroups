############################################################################
##
#W  conglatt.gi
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

## This file contains functions for a lattice of congruences.
##
## When the congruences of a semigroup are computed, they form a lattice with
## respect to containment.  The information about how the congruences lie in
## this lattice is stored in an IsCongruenceLattice object (a positional object
## based on a list) and can be retrieved from this object with the following
## methods
##
## Most of these methods simply redirect to the first entry of the object, the
## actual list of lists of integers which defines the containments in the
## lattice.  Component 2 in the list is the list of congruences, which is hidden
## when this object is used.

InstallMethod(\[\],
"for a congruence lattice",
[IsCongruenceLattice, IsObject],
function(latt, x)
  return latt![1][x];
end);

InstallMethod(ViewObj,
"for a congruence lattice",
[IsCongruenceLattice],
function(latt)
  ViewObj(latt![1]);
end);

InstallMethod(PrintObj,
"for a congruence lattice",
[IsCongruenceLattice],
function(latt)
  PrintObj(latt![1]);
end);

InstallMethod(Length,
"for a congruence lattice",
[IsCongruenceLattice],
function(latt)
  return Length(latt![1]);
end);

InstallMethod(IsBound\[\],
"for a congruence lattice and a positive integer",
[IsCongruenceLattice, IsPosInt],
function(latt, x)
  return IsBound(latt![1][x]);
end);

###############################################################################
# LatticeOfXCongruences function
###############################################################################
#
# This abstract function takes a semigroup 'S', a string 'type_string' and a
# record 'record'.
# type_string should be in ["Left", "Right", ""] and describes the sort of
# relations we want to find (respectively: left congruences, right congruences,
# two-sided congruences), referred to as 'x congs' below.
# record may contain any of the following components, which should be set to
# 'true' to have the stated effect:
#   * minimal - Return only minimal x-congs
#   * 1gen - Return only x-congs with a single generating pair
#   * transrep - Return only x-congs which contain no 2-sided congruences
#
###############################################################################

# FIXME Monolithic functions = BAD!!! JDM

SEMIGROUPS.LatticeOfXCongruences := function(S, type_string, record)
  local transrep, _XSemigroupCongruence, elms, pairs, congs1, nrcongs, children,
        parents, pair, badcong, newchildren, newparents, newcong, i, c, p,
        congs, 2congs, image, next, set_func, lattice, join_func, length, found,
        ignore, start, j, k, report;

  if not IsFinite(S) then
    ErrorNoReturn("Semigroups: SEMIGROUPS.LatticeOfXCongruences: usage,\n",
                  "first argument <S> must be a finite semigroup,");
  fi;

  report :=  SEMIGROUPS.OptionsRec(S).report;
  SEMIGROUPS.OptionsRec(S).report := false;

  transrep := IsBound(record.transrep) and record.transrep;
  _XSemigroupCongruence := EvalString(Concatenation(type_string,
                                                    "SemigroupCongruence"));
  elms := SEMIGROUP_AS_LIST(GenericSemigroupData(S));

  # Get all non-reflexive pairs in SxS
  pairs := Combinations(elms, 2);

  # Get all the unique 1-generated congs
  Info(InfoSemigroups, 1, "Getting all 1-generated congs...");
  congs1 := [];     # List of all congs found so far
  nrcongs := 0;     # Number of congs found so far
  children := [];   # List of lists of children
  parents := [];    # List of lists of parents
  for pair in pairs do
    badcong := false;
    newchildren := []; # Children of newcong
    newparents := [];  # Parents of newcong
    newcong := _XSemigroupCongruence(S, pair);
    for i in [1 .. Length(congs1)] do
      if IsSubrelation(congs1[i], newcong) then
        if IsSubrelation(newcong, congs1[i]) then
          # This is not a new cong - drop it!
          badcong := true;
          break;
        else
          Add(newparents, i);
        fi;
      elif IsSubrelation(newcong, congs1[i]) then
        Add(newchildren, i);
      fi;
    od;
    if not badcong then
      nrcongs := nrcongs + 1;
      congs1[nrcongs] := newcong;
      children[nrcongs] := newchildren;
      parents[nrcongs] := newparents;
      for c in newchildren do
        Add(parents[c], nrcongs);
      od;
      for p in newparents do
        Add(children[p], nrcongs);
      od;
    fi;
  od;

  congs := ShallowCopy(congs1);
  if transrep then
    # Find and remove any 2-sided congruences, and discard their parents
    2congs := Set([]);
    for i in [1 .. Length(congs)] do
      if not IsBound(congs[i]) then
        continue;
      fi;
      if IsSemigroupCongruence(congs[i]) then
        # Remove it from the list
        Unbind(congs[i]);
        # Remove all its parents
        for p in parents[i] do
          Unbind(congs[p]);
          if p in 2congs then
            RemoveSet(2congs, p);
          fi;
        od;
        # Store it unless it has 2-sided children
        if ForAll(children[i], c -> not c in 2congs) then
          AddSet(2congs, i);
        fi;
      fi;
    od;

    # Remove holes from congs and change children and parents appropriately
    image := ListWithIdenticalEntries(nrcongs, fail);
    next := 1;
    for i in [1 .. nrcongs] do
      if IsBound(congs[i]) then
        image[i] := next;
        next := next + 1;
      else
        Unbind(parents[i]);
        Unbind(children[i]);
        nrcongs := nrcongs - 1;
      fi;
    od;
    congs := Compacted(congs);
    parents := Compacted(parents);
    children := Compacted(children);
    parents := List(parents, l -> Filtered(List(l, i -> image[i]),
                                           i -> i <> fail));
    children := List(children, l -> Filtered(List(l, i -> image[i]),
                                             i -> i <> fail));
    2congs := List(2congs, i -> congs1[i]);
    congs1 := congs;
  fi;

  # We now have all 1-generated congs, which must include all the minimal
  # congs.  We can return if necessary.
  if IsBound(record.minimal) and record.minimal = true then
    # Find all the minimal congs (those with no children)
    congs := congs{Positions(children, [])};
    # Note: we don't include the trivial cong
    # Set the MinimalXCongruencesOfSemigroup attribute
    set_func := EvalString(Concatenation("SetMinimal",
                                         type_string,
                                         "CongruencesOfSemigroup"));
    set_func(S, congs);
    # Minimal congs cannot contain each other
    children := ListWithIdenticalEntries(Length(congs), []);
    lattice := Objectify(NewType(FamilyObj(children),
                                 IsCongruenceLattice),
                         [children, congs]);
    SEMIGROUPS.OptionsRec(S).report := report;
    return lattice;
  elif IsBound(record.1gen) and record.1gen = true then
    # Add the trivial cong at the start
    children := Concatenation([[]], children + 1);
    for i in [2 .. nrcongs + 1] do
      Add(children[i], 1, 1);
    od;
    Add(congs, _XSemigroupCongruence(S, []), 1);
    # Return the lattice, but don't set any attributes
    lattice := Objectify(NewType(FamilyObj(children),
                                 IsCongruenceLattice),
                         [children, congs]);
    SEMIGROUPS.OptionsRec(S).report := report;
    return lattice;
  fi;

  # Take all their joins
  Info(InfoSemigroups, 1, "Taking joins...");
  join_func := EvalString(Concatenation("Join",
                                        type_string,
                                        "SemigroupCongruences"));
  length := 0;
  found := true;
  # 'ignore' is a list of congs that we don't try joining
  ignore := BlistList(congs, []);
  while found do
    # There are new congs to try joining
    start := length + 1;     # New congs start here
    found := false;          # Have we found any more congs on this sweep?
    length := Length(congs); # Remember starting position for next sweep
    for i in [start .. Length(congs)] do # for each new cong
      for j in [1 .. Length(congs1)] do  # for each 1-generated cong
        newcong := join_func(congs[i], congs1[j]);
        badcong := false;  # Is newcong the same as another cong?
        newchildren := []; # Children of newcong
        newparents := [];  # Parents of newcong
        for k in [1 .. Length(congs)] do
          if IsSubrelation(congs[k], newcong) then
            if IsSubrelation(newcong, congs[k]) then
              # This is the same as an old cong - discard it!
              badcong := true;
              break;
            else
              Add(newparents, k);
            fi;
          elif IsSubrelation(newcong, congs[k]) then
            Add(newchildren, k);
          fi;
        od;
        # Check for 2-sided congs if 'transrep' is set
        if transrep then
          if IsSemigroupCongruence(newcong) then
            badcong := true;
            Add(2congs, newcong);
            for p in newparents do
              ignore[p] := true;
            od;
          elif ForAny(2congs, c2 -> IsSubrelation(newcong, c2)) then
            badcong := true;
          fi;
        fi;
        if not badcong then
          nrcongs := nrcongs + 1;
          congs[nrcongs] := newcong;
          children[nrcongs] := newchildren;
          parents[nrcongs] := newparents;
          ignore[nrcongs] := false;
          for c in newchildren do
            Add(parents[c], nrcongs);
          od;
          for p in newparents do
            Add(children[p], nrcongs);
          od;
          found := true;
        fi;
      od;
    od;
  od;

  if transrep and (true in ignore) then
    # Remove any congs in 'ignore'
    for i in [1 .. Length(congs)] do
      if ignore[i] then
        Unbind(congs[i]);
      fi;
    od;

    # Remove holes from congs and change children and parents appropriately
    image := ListWithIdenticalEntries(nrcongs, fail);
    next := 1;
    for i in [1 .. nrcongs] do
      if not ignore[i] then
        image[i] := next;
        next := next + 1;
      else
        Unbind(parents[i]);
        Unbind(children[i]);
        nrcongs := nrcongs - 1;
      fi;
    od;
    congs := Compacted(congs);
    parents := Compacted(parents);
    children := Compacted(children);
    parents := List(parents, l -> Filtered(List(l, i -> image[i]),
                                           i -> i <> fail));
    children := List(children, l -> Filtered(List(l, i -> image[i]),
                                             i -> i <> fail));
  fi;

  # Add the trivial cong at the start
  children := Concatenation([[]], children + 1);
  for i in [2 .. nrcongs + 1] do
    Add(children[i], 1, 1);
  od;
  Add(congs, _XSemigroupCongruence(S, []), 1);

  # We have a list of all the congs
  set_func := EvalString(Concatenation("Set",
                                       type_string,
                                       "CongruencesOfSemigroup"));
  set_func(S, congs);

  SEMIGROUPS.OptionsRec(S).report := report;
  # Objectify the result
  lattice := Objectify(NewType(FamilyObj(children),
                               IsCongruenceLattice),
                       [children, congs]);
  return lattice;
end;

InstallMethod(LatticeOfLeftCongruences,
"for a semigroup", [IsSemigroup],
S -> SEMIGROUPS.LatticeOfXCongruences(S, "Left", rec()));

InstallMethod(LatticeOfRightCongruences,
"for a semigroup", [IsSemigroup],
S -> SEMIGROUPS.LatticeOfXCongruences(S, "Right", rec()));

InstallMethod(LatticeOfCongruences,
"for a semigroup", [IsSemigroup],
S -> SEMIGROUPS.LatticeOfXCongruences(S, "", rec()));

InstallMethod(LeftCongruencesOfSemigroup,
"for a semigroup", [IsSemigroup],
S -> LatticeOfLeftCongruences(S)![2]);

InstallMethod(RightCongruencesOfSemigroup,
"for a semigroup", [IsSemigroup],
S -> LatticeOfRightCongruences(S)![2]);

InstallMethod(CongruencesOfSemigroup,
"for a semigroup", [IsSemigroup],
S -> LatticeOfCongruences(S)![2]);

InstallMethod(MinimalLeftCongruencesOfSemigroup,
"for a semigroup", [IsSemigroup],
S -> SEMIGROUPS.LatticeOfXCongruences(S, "Left", rec(minimal := true))![2]);

InstallMethod(MinimalRightCongruencesOfSemigroup,
"for a semigroup", [IsSemigroup],
S -> SEMIGROUPS.LatticeOfXCongruences(S, "Right", rec(minimal := true))![2]);

InstallMethod(MinimalCongruencesOfSemigroup,
"for a semigroup", [IsSemigroup],
S -> SEMIGROUPS.LatticeOfXCongruences(S, "", rec(minimal := true))![2]);

InstallMethod(DotString,
"for a congruence lattice",
[IsCongruenceLattice],
function(latt)
  # Call the below function, with info turned off
  return DotString(latt, rec(info := false));
end);

InstallMethod(DotString,
"for a congruence lattice and a record",
[IsCongruenceLattice, IsRecord],
function(latt, opts)
  local congs, S, symbols, i, nr, rel, str, j, k;
  # If the user wants info, then change the node labels
  if opts.info = true then
    # The congruences are stored inside the lattice object
    congs := latt![2];
    S := Range(congs[1]);
    symbols := EmptyPlist(Length(latt));
    for i in [1 .. Length(latt)] do
      nr := NrEquivalenceClasses(congs[i]);
      if nr = 1 then
        symbols[i] := "U";
      elif nr = Size(S) then
        symbols[i] := "T";
      elif IsReesCongruence(congs[i]) then
        symbols[i] := Concatenation("R", String(i));
      else
        symbols[i] := String(i);
      fi;
    od;
  else
    symbols := List([1 .. Length(latt)], String);
  fi;

  rel := List([1 .. Length(latt)], x -> Filtered(latt[x], y -> x <> y));
  str := "";

  if Length(rel) < 40 then
    Append(str, "//dot\ngraph graphname {\n     node [shape=circle]\n");
  else
    Append(str, "//dot\ngraph graphname {\n     node [shape=point]\n");
  fi;

  for i in [1 .. Length(rel)] do
    j := Difference(rel[i], Union(rel{rel[i]}));
    i := symbols[i];
    for k in j do
      k := symbols[k];
      Append(str, Concatenation(i, " -- ", k, "\n"));
    od;
  od;

  Append(str, " }");

  return str;
end);
